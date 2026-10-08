/*  ar.usp_AgedDebtSummary - aged receivables as at a chosen date.  Ageing is rebuilt from
    ar.fn_OpenItemAsOf, so a report re-run for an earlier date is unaffected by receipts and
    allocations made since, and the bucket totals always add back to the receivables balance on
    that same date (AR-017).  Credits and unapplied cash are reported in their own bucket rather
    than netted into overdue debt, and the gross overdue debit total is exposed alongside.
    REQ: AR-013, AR-017, AR-021 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE ar.usp_AgedDebtSummary @CompanyCode varchar(10), @AsOf date = NULL,
    @UserCode varchar(20) = NULL, @CustomerCode varchar(20) = NULL, @IncludeZeroAccounts bit = 0
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    IF @UserCode IS NOT NULL AND core.fn_HasPermission(@UserCode, 'AR.ENQUIRY', @CompanyCode, NULL) = 0
        THROW 50301, 'STERP-E-0301: You are not authorised to enquire on debtor accounts in this company.', 1;
    IF NOT EXISTS (SELECT 1 FROM core.Company WHERE CompanyCode = @CompanyCode)
        THROW 50303, 'STERP-E-0303: That company does not exist.', 1;
    SET @AsOf = ISNULL(@AsOf, CAST(GETDATE() AS date));
    /*  1 - bucket summary for the company as a whole. */
    SELECT @CompanyCode AS CompanyCode, @AsOf AS AsOf, b.BucketNo, b.BucketName, ISNULL(COUNT(a.OpenItemId), 0) AS ItemCount,
           ISNULL(COUNT(DISTINCT a.CustomerCode), 0) AS CustomerCount, ISNULL(SUM(a.ResidualAsOfGBP), 0) AS AmountGBP,
           ISNULL(SUM(a.DebitGBP), 0) AS DebitGBP, ISNULL(SUM(a.CreditGBP), 0) AS CreditGBP,
           ISNULL(SUM(CASE WHEN a.IsDisputed = 1 THEN a.DisputedAmountGBP ELSE 0 END), 0) AS DisputedGBP
      FROM (VALUES (0, 'Credits and unapplied'), (1, 'Not yet due'), (2, '1-30 days overdue'), (3, '31-60 days overdue'),
                   (4, '61-90 days overdue'), (5, 'Over 90 days overdue')) AS b (BucketNo, BucketName)
      LEFT JOIN ar.fn_OpenItemAsOf(@CompanyCode, @AsOf) a
             ON a.BucketNo = b.BucketNo AND (@CustomerCode IS NULL OR a.CustomerCode = @CustomerCode)
     GROUP BY b.BucketNo, b.BucketName ORDER BY b.BucketNo;
    /*  2 - the same figures per account, as the printed report lays them out. */
    SELECT c.CompanyCode, c.CustomerCode, c.LegalName AS CustomerName, c.CurrencyCode, c.TermsCode, c.Status AS CustomerStatus,
           ISNULL(cr.CreditLimitGBP, 0) AS CreditLimitGBP, ISNULL(cr.HoldFlag, 0) AS HoldFlag, cr.ControllerUser, s.BalanceGBP,
           s.CreditsGBP, s.NotYetDueGBP, s.Due1To30GBP, s.Due31To60GBP, s.Due61To90GBP, s.DueOver90GBP, s.GrossOverdueGBP,
           s.DisputedGBP, s.OldestDueDate, s.MaxDaysOverdue, s.ItemCount
      FROM ar.Customer c
      LEFT JOIN ar.CustomerCredit cr ON cr.CompanyCode = c.CompanyCode AND cr.CustomerCode = c.CustomerCode
     CROSS APPLY (SELECT ISNULL(SUM(a.ResidualAsOfGBP), 0), ISNULL(SUM(a.CreditGBP), 0),
            ISNULL(SUM(CASE WHEN a.BucketNo = 1 THEN a.ResidualAsOfGBP ELSE 0 END), 0),
            ISNULL(SUM(CASE WHEN a.BucketNo = 2 THEN a.ResidualAsOfGBP ELSE 0 END), 0),
            ISNULL(SUM(CASE WHEN a.BucketNo = 3 THEN a.ResidualAsOfGBP ELSE 0 END), 0),
            ISNULL(SUM(CASE WHEN a.BucketNo = 4 THEN a.ResidualAsOfGBP ELSE 0 END), 0),
            ISNULL(SUM(CASE WHEN a.BucketNo = 5 THEN a.ResidualAsOfGBP ELSE 0 END), 0),
            ISNULL(SUM(CASE WHEN a.BucketNo >= 2 THEN a.DebitGBP ELSE 0 END), 0),
            ISNULL(SUM(CASE WHEN a.IsDisputed = 1 THEN a.DisputedAmountGBP ELSE 0 END), 0),
            MIN(CASE WHEN a.ResidualAsOfGBP > 0 THEN a.DueDate END), ISNULL(MAX(a.DaysOverdue), 0), ISNULL(COUNT_BIG(*), 0)
            FROM ar.fn_OpenItemAsOf(@CompanyCode, @AsOf) a WHERE a.CustomerCode = c.CustomerCode)
          AS s (BalanceGBP, CreditsGBP, NotYetDueGBP, Due1To30GBP, Due31To60GBP, Due61To90GBP, DueOver90GBP, GrossOverdueGBP,
                DisputedGBP, OldestDueDate, MaxDaysOverdue, ItemCount)
     WHERE c.CompanyCode = @CompanyCode AND (@CustomerCode IS NULL OR c.CustomerCode = @CustomerCode)
       AND (@IncludeZeroAccounts = 1 OR s.ItemCount > 0)
     ORDER BY s.GrossOverdueGBP DESC, c.CustomerCode;
END
GO
