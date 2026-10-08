/*  ar.usp_CustomerEnquiry - everything the debtors enquiry screen shows for one account: the
    balance and credit position, the aged profile, invoiced turnover, days sales outstanding,
    receipt effectiveness, the open items and the collection history.  Turnover is invoiced value,
    not order value, and DSO is closing VAT-inclusive receivables over VAT-inclusive credit sales
    for the stated period; with no credit sales the ratio is undefined rather than zero (AR-022).
    Enquiry only - nothing here posts.
    REQ: AR-015, AR-017, AR-022, COM-002 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE ar.usp_CustomerEnquiry @CompanyCode varchar(10), @CustomerCode varchar(20),
    @UserCode varchar(20) = NULL, @TurnoverMonths smallint = 12
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    IF @UserCode IS NOT NULL AND core.fn_HasPermission(@UserCode, 'AR.ENQUIRY', @CompanyCode, NULL) = 0
        THROW 50301, 'STERP-E-0301: You are not authorised to enquire on debtor accounts in this company.', 1;
    IF NOT EXISTS (SELECT 1 FROM ar.Customer WHERE CompanyCode = @CompanyCode AND CustomerCode = @CustomerCode)
        THROW 50302, 'STERP-E-0302: That customer account does not exist in this company.', 1;
    DECLARE @AsOf date = CAST(GETDATE() AS date), @From date;
    SET @From = DATEADD(month, -ISNULL(NULLIF(@TurnoverMonths, 0), 12), @AsOf);
    /*  1 - balance, exposure and credit control position. */
    SELECT * FROM ar.vw_CustomerBalance WHERE CompanyCode = @CompanyCode AND CustomerCode = @CustomerCode;
    /*  2 - aged profile; every bucket is returned even when empty so the screen grid is stable. */
    SELECT b.BucketNo, b.BucketName, ISNULL(COUNT(a.OpenItemId), 0) AS ItemCount,
           ISNULL(SUM(a.ResidualAmountGBP), 0) AS AmountGBP, ISNULL(SUM(a.DisputedAmountGBP), 0) AS DisputedGBP
      FROM (VALUES (0, 'Credits and unapplied'), (1, 'Not yet due'), (2, '1-30 days overdue'), (3, '31-60 days overdue'),
                   (4, '61-90 days overdue'), (5, 'Over 90 days overdue')) AS b (BucketNo, BucketName)
      LEFT JOIN ar.vw_OpenItemAged a ON a.BucketNo = b.BucketNo AND a.CompanyCode = @CompanyCode AND a.CustomerCode = @CustomerCode
     GROUP BY b.BucketNo, b.BucketName ORDER BY b.BucketNo;
    /*  3 - turnover, DSO and receipt effectiveness over the chosen window. */
    SELECT @From AS PeriodFrom, @AsOf AS PeriodTo, DATEDIFF(day, @From, @AsOf) AS PeriodDays, 'GBP' AS CurrencyCode,
           'VAT inclusive' AS VatBasis, t.InvoiceCount, t.NetTurnoverGBP, t.GrossCreditSalesGBP, t.CreditNoteGBP, bal.BalanceGBP,
           CASE WHEN t.GrossCreditSalesGBP > 0 THEN ROUND(bal.BalanceGBP / t.GrossCreditSalesGBP * DATEDIFF(day, @From, @AsOf), 1) END AS DsoDays,
           r.ReceiptCount, r.ReceivedGBP, r.DishonouredGBP,
           CASE WHEN t.GrossCreditSalesGBP > 0 THEN ROUND(r.ReceivedGBP * 100.0 / t.GrossCreditSalesGBP, 2) END AS ReceiptEffectivenessPct,
           al.AverageDaysToPay, al.SettledOnTimePct, c.FirstTradedDate, c.LastTransactionDate
      FROM ar.Customer c
     CROSS APPLY (SELECT ISNULL(SUM(bb.BalanceGBP), 0) FROM ar.vw_CustomerBalance bb
                   WHERE bb.CompanyCode = c.CompanyCode AND bb.CustomerCode = c.CustomerCode) AS bal (BalanceGBP)
     CROSS APPLY (SELECT ISNULL(COUNT_BIG(CASE WHEN i.DocumentType = 'INVOICE' THEN 1 END), 0),
            ISNULL(SUM(CASE WHEN i.DocumentType = 'CREDITNOTE' THEN -i.NetAmountGBP ELSE i.NetAmountGBP END), 0),
            ISNULL(SUM(CASE WHEN i.DocumentType = 'CREDITNOTE' THEN -i.GrossAmountGBP ELSE i.GrossAmountGBP END), 0),
            ISNULL(SUM(CASE WHEN i.DocumentType = 'CREDITNOTE' THEN i.GrossAmountGBP ELSE 0 END), 0)
            FROM ar.SalesInvoice i WHERE i.CompanyCode = c.CompanyCode AND i.CustomerCode = c.CustomerCode
             AND i.Status = 'POSTED' AND i.DocumentDate BETWEEN @From AND @AsOf)
          AS t (InvoiceCount, NetTurnoverGBP, GrossCreditSalesGBP, CreditNoteGBP)
     CROSS APPLY (SELECT ISNULL(COUNT_BIG(CASE WHEN p.ConfirmationState = 'CONFIRMED' THEN 1 END), 0),
            ISNULL(SUM(CASE WHEN p.ConfirmationState = 'CONFIRMED' THEN p.GrossReceivedGBP ELSE 0 END), 0),
            ISNULL(SUM(CASE WHEN p.ConfirmationState = 'DISHONOURED' THEN p.GrossReceivedGBP ELSE 0 END), 0)
            FROM ar.Receipt p WHERE p.CompanyCode = c.CompanyCode AND p.CustomerCode = c.CustomerCode
             AND p.ReceiptDate BETWEEN @From AND @AsOf) AS r (ReceiptCount, ReceivedGBP, DishonouredGBP)
     CROSS APPLY (SELECT ROUND(AVG(CAST(DATEDIFF(day, o.DueDate, x.EffectiveDate) AS decimal(9,2))), 1),
            ROUND(100.0 * SUM(CASE WHEN x.EffectiveDate <= o.DueDate THEN 1.0 ELSE 0 END) / NULLIF(COUNT_BIG(*), 0), 2)
            FROM ar.Allocation x JOIN ar.OpenItem o ON o.OpenItemId = x.TargetOpenItemId
           WHERE x.CompanyCode = c.CompanyCode AND x.CustomerCode = c.CustomerCode AND x.Status = 'ACTIVE'
             AND x.EffectiveDate BETWEEN @From AND @AsOf AND o.ItemType = 'INVOICE') AS al (AverageDaysToPay, SettledOnTimePct)
     WHERE c.CompanyCode = @CompanyCode AND c.CustomerCode = @CustomerCode;
    /*  4 - the open items themselves, oldest first. */
    SELECT TOP (500) OpenItemId, ItemType, DocumentNumber, DocumentDate, DueDate, CurrencyCode, OriginalAmount, ResidualAmount,
           ResidualAmountGBP, SettlementStatus, BalanceSide, DaysOverdue, BucketNo, BucketName, IsDisputed, DisputedAmountGBP,
           CollectionStatus, LastReminderStep, LastReminderDate
      FROM ar.vw_OpenItemAged WHERE CompanyCode = @CompanyCode AND CustomerCode = @CustomerCode ORDER BY DueDate, DocumentNumber;
    /*  5 - collection cases and the contact history behind them. */
    SELECT k.CaseId, k.CaseRef, k.CaseType, k.DocumentNumber, k.ReasonCode, k.Reason, k.DisputedAmountGBP, k.OwnerUser,
           k.CollectionStep, k.NextActionDate, k.NextAction, k.Status, a.ActionSeq, a.ActionType, a.ActionDate, a.PerformedBy,
           a.Narrative, a.PromiseAmountGBP, a.PromiseDate, a.PromiseStatus
      FROM ar.CollectionCase k LEFT JOIN ar.CollectionAction a ON a.CaseId = k.CaseId
     WHERE k.CompanyCode = @CompanyCode AND k.CustomerCode = @CustomerCode ORDER BY k.CaseId DESC, a.ActionSeq;
END
GO
