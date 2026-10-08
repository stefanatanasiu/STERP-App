/*  Debtors reporting views.  The three objects are held in one file because the worklist reads the
    ageing view and the balance view reads it too, so they must be created in this order.
    REQ: AR-013, AR-015, AR-016, AR-017, AR-019, AR-021, AR-022, D-22, D-28 */

/*  ar.vw_OpenItemAged - every live sales-ledger open item with its age, its due-date bucket as at
    today and the dispute and collection state.  On the due date an invoice is not yet overdue, so
    bucket 1 is "not yet due" and bucket 2 starts at one day past due.  Credit balances (unapplied
    receipts, credit notes and deposits) are carried in bucket 0 and are never aged as overdue debt.
    REQ: AR-013, AR-015, AR-017, AR-019, AR-021 */
CREATE OR ALTER VIEW ar.vw_OpenItemAged
AS
SELECT o.CompanyCode, o.CustomerCode, c.LegalName AS CustomerName, c.Status AS CustomerStatus, c.CollectionMethod,
       c.ContactRestriction, c.AccountOwner, o.OpenItemId, o.ItemType, o.DocumentNumber, o.DocumentDate, o.DueDate,
       o.PostingDate, o.FiscalYear, o.PeriodNo, o.CurrencyCode, o.FxRate, o.OriginalAmount, o.OriginalAmountGBP,
       o.ResidualAmount, o.ResidualAmountGBP, o.SettlementStatus, o.TermsCode, o.DiscountDueDate, o.DiscountAmount,
       o.IsDisputed, o.DisputedAmountGBP, o.CollectionStatus, o.LastReminderStep, o.LastReminderDate, o.WrittenOff,
       o.BadDebtReliefClaimed, o.SourceInvoiceId, o.SourceReceiptId, d.AsOf, v.AgeDays, v.DaysOverdue, k.BucketNo, n.BucketName,
       CASE WHEN o.ResidualAmountGBP < 0 THEN 'CREDIT' ELSE 'DEBIT' END AS BalanceSide,
       CASE WHEN o.ResidualAmountGBP < 0 THEN -o.ResidualAmountGBP ELSE 0 END AS UnappliedGBP,
       CASE WHEN o.ResidualAmountGBP > 0 THEN o.ResidualAmountGBP ELSE 0 END AS DebitGBP,
       CASE WHEN k.BucketNo >= 2 THEN o.ResidualAmountGBP ELSE 0 END AS OverdueGBP,
       CASE WHEN k.BucketNo >= 2 THEN o.ResidualAmountGBP - o.DisputedAmountGBP ELSE 0 END AS UndisputedOverdueGBP
  FROM ar.OpenItem o
  JOIN ar.Customer c ON c.CompanyCode = o.CompanyCode AND c.CustomerCode = o.CustomerCode
 CROSS APPLY (SELECT CAST(GETDATE() AS date)) AS d (AsOf)
 CROSS APPLY (SELECT DATEDIFF(day, o.DocumentDate, d.AsOf),
                     CASE WHEN d.AsOf > o.DueDate THEN DATEDIFF(day, o.DueDate, d.AsOf) ELSE 0 END) AS v (AgeDays, DaysOverdue)
 CROSS APPLY (SELECT CASE WHEN o.ResidualAmountGBP < 0 THEN 0 WHEN v.DaysOverdue = 0 THEN 1 WHEN v.DaysOverdue <= 30 THEN 2
                          WHEN v.DaysOverdue <= 60 THEN 3 WHEN v.DaysOverdue <= 90 THEN 4 ELSE 5 END) AS k (BucketNo)
 CROSS APPLY (SELECT CASE k.BucketNo WHEN 0 THEN 'Credits and unapplied' WHEN 1 THEN 'Not yet due' WHEN 2 THEN '1-30 days overdue'
                          WHEN 3 THEN '31-60 days overdue' WHEN 4 THEN '61-90 days overdue' ELSE 'Over 90 days overdue' END) AS n (BucketName)
 WHERE o.SettlementStatus IN ('OPEN', 'PARTIAL') AND o.ResidualAmountGBP <> 0;
GO
/*  ar.vw_CustomerBalance - one row for every sales-ledger account: the open-item balance split into
    gross debt, unapplied cash, credit notes and deposits, the aged profile, and the credit position
    with any live scoped override added to the limit.  A dispute does not reduce exposure (AR-015).
    REQ: AR-007, AR-008, AR-015, AR-017, AR-021, AR-022, D-06 */
CREATE OR ALTER VIEW ar.vw_CustomerBalance
AS
SELECT c.CompanyCode, c.CustomerCode, c.LegalName, c.TradingName, c.Status AS CustomerStatus, c.CurrencyCode, c.TermsCode,
       c.CollectionMethod, c.StatementFrequency, c.AccountOwner, c.SalespersonCode, c.Classification, c.InsolvencyType,
       c.ContactRestriction, ISNULL(cr.CreditLimitGBP, 0) AS CreditLimitGBP, ISNULL(cr.OverdueThresholdGBP, 0) AS OverdueThresholdGBP,
       ISNULL(cr.OverdueDaysTrigger, 30) AS OverdueDaysTrigger, cr.RiskClass, cr.CreditScore, cr.ReviewDate, cr.ControllerUser,
       ISNULL(cr.HoldFlag, 0) AS HoldFlag, cr.HoldReason, ISNULL(cr.RequiresPrepayment, 0) AS RequiresPrepayment,
       ISNULL(cr.ProFormaOnly, 0) AS ProFormaOnly, cr.CreditInsuranceCoverGBP, b.OpenItemCount, b.BalanceGBP, b.GrossDebtGBP,
       b.UnappliedCashGBP, b.CreditNoteGBP, b.DepositGBP, b.OverdueGBP, b.UndisputedOverdueGBP, b.DisputedGBP, b.NotYetDueGBP,
       b.Due1To30GBP, b.Due31To60GBP, b.Due61To90GBP, b.DueOver90GBP, b.OldestDueDate, b.MaxDaysOverdue,
       ISNULL(ov.OverrideGBP, 0) AS OverrideGBP, b.BalanceGBP AS ExposureGBP,
       ISNULL(cr.CreditLimitGBP, 0) + ISNULL(ov.OverrideGBP, 0) - b.BalanceGBP AS CreditAvailableGBP,
       CASE WHEN ISNULL(cr.CreditLimitGBP, 0) > 0 THEN ROUND(b.BalanceGBP * 100.0 / cr.CreditLimitGBP, 2) END AS UtilisationPct,
       CASE WHEN ISNULL(cr.HoldFlag, 0) = 1 THEN 'HOLD'
            WHEN ISNULL(cr.CreditLimitGBP, 0) > 0 AND b.BalanceGBP > cr.CreditLimitGBP + ISNULL(ov.OverrideGBP, 0) THEN 'OVERLIMIT'
            WHEN b.OverdueGBP > ISNULL(cr.OverdueThresholdGBP, 0) AND b.MaxDaysOverdue >= ISNULL(cr.OverdueDaysTrigger, 30) THEN 'OVERDUE'
            ELSE 'OK' END AS CreditState
  FROM ar.Customer c
  LEFT JOIN ar.CustomerCredit cr ON cr.CompanyCode = c.CompanyCode AND cr.CustomerCode = c.CustomerCode
 CROSS APPLY (SELECT ISNULL(COUNT_BIG(a.OpenItemId), 0), ISNULL(SUM(a.ResidualAmountGBP), 0), ISNULL(SUM(a.DebitGBP), 0),
        ISNULL(SUM(CASE WHEN a.ItemType = 'RECEIPT' THEN a.UnappliedGBP ELSE 0 END), 0),
        ISNULL(SUM(CASE WHEN a.ItemType = 'CREDIT' THEN a.UnappliedGBP ELSE 0 END), 0),
        ISNULL(SUM(CASE WHEN a.ItemType = 'DEPOSIT' THEN a.UnappliedGBP ELSE 0 END), 0), ISNULL(SUM(a.OverdueGBP), 0),
        ISNULL(SUM(a.UndisputedOverdueGBP), 0), ISNULL(SUM(a.DisputedAmountGBP), 0),
        ISNULL(SUM(CASE WHEN a.BucketNo = 1 THEN a.ResidualAmountGBP ELSE 0 END), 0),
        ISNULL(SUM(CASE WHEN a.BucketNo = 2 THEN a.ResidualAmountGBP ELSE 0 END), 0),
        ISNULL(SUM(CASE WHEN a.BucketNo = 3 THEN a.ResidualAmountGBP ELSE 0 END), 0),
        ISNULL(SUM(CASE WHEN a.BucketNo = 4 THEN a.ResidualAmountGBP ELSE 0 END), 0),
        ISNULL(SUM(CASE WHEN a.BucketNo = 5 THEN a.ResidualAmountGBP ELSE 0 END), 0),
        MIN(CASE WHEN a.BalanceSide = 'DEBIT' THEN a.DueDate END), ISNULL(MAX(a.DaysOverdue), 0)
        FROM ar.vw_OpenItemAged a WHERE a.CompanyCode = c.CompanyCode AND a.CustomerCode = c.CustomerCode)
      AS b (OpenItemCount, BalanceGBP, GrossDebtGBP, UnappliedCashGBP, CreditNoteGBP, DepositGBP, OverdueGBP, UndisputedOverdueGBP,
            DisputedGBP, NotYetDueGBP, Due1To30GBP, Due31To60GBP, Due61To90GBP, DueOver90GBP, OldestDueDate, MaxDaysOverdue)
 OUTER APPLY (SELECT SUM(x.OverrideAmountGBP) FROM ar.CreditOverride x WHERE x.CompanyCode = c.CompanyCode
        AND x.CustomerCode = c.CustomerCode AND x.Status = 'ACTIVE' AND CAST(GETDATE() AS date) BETWEEN x.ValidFrom AND x.ExpiresOn) AS ov (OverrideGBP);
GO
/*  ar.vw_CollectionWorklist - the overdue items a credit controller must act on, each carrying the
    baseline dunning stage for its age (reminder at 7 days, firm reminder at 21, account stop at 30,
    letter before action at 45), the owning case, the latest promise to pay and the owner.  A step is
    suppressed for a fully disputed item, for a customer with a recorded contact restriction and
    while an open promise has not yet fallen due; the debt itself is never erased (AR-015, AR-016).
    REQ: AR-015, AR-016, AR-024, D-06, D-28 */
CREATE OR ALTER VIEW ar.vw_CollectionWorklist
AS
SELECT a.CompanyCode, a.CustomerCode, a.CustomerName, a.CustomerStatus, a.OpenItemId, a.ItemType, a.DocumentNumber,
       a.DocumentDate, a.DueDate, a.CurrencyCode, a.ResidualAmount, a.ResidualAmountGBP, a.DisputedAmountGBP,
       a.ResidualAmountGBP - a.DisputedAmountGBP AS UndisputedGBP, a.AsOf, a.DaysOverdue, a.BucketNo, a.BucketName,
       a.IsDisputed, a.CollectionStatus, a.LastReminderStep, a.LastReminderDate, a.CollectionMethod, a.ContactRestriction,
       g.StageNo, s.StageCode, s.StageAction, k.CaseId, k.CaseRef, k.CaseType, k.ReasonCode, k.Status AS CaseStatus,
       k.NextActionDate, k.NextAction, k.EscalatedTo, p.PromiseAmountGBP, p.PromiseDate, p.PromiseStatus,
       COALESCE(k.OwnerUser, cc.ControllerUser, a.AccountOwner, 'CREDMGR') AS OwnerUser,
       ISNULL(cc.HoldFlag, 0) AS HoldFlag, ISNULL(cc.CreditLimitGBP, 0) AS CreditLimitGBP,
       CASE WHEN a.IsDisputed = 1 AND a.ResidualAmountGBP - a.DisputedAmountGBP <= 0 THEN 1
            WHEN a.ContactRestriction IS NOT NULL THEN 1
            WHEN p.PromiseStatus = 'OPEN' AND p.PromiseDate >= a.AsOf THEN 1 ELSE 0 END AS IsSuppressed,
       CASE WHEN a.IsDisputed = 1 AND a.ResidualAmountGBP - a.DisputedAmountGBP <= 0 THEN 'Amount is wholly in dispute'
            WHEN a.ContactRestriction IS NOT NULL THEN 'Contact restriction recorded on the account'
            WHEN p.PromiseStatus = 'OPEN' AND p.PromiseDate >= a.AsOf THEN 'Promise to pay not yet due' END AS SuppressReason,
       g.StageNo * 100 + CASE WHEN a.ResidualAmountGBP > 10000 THEN 3
            WHEN a.ResidualAmountGBP > 1000 THEN 2 ELSE 1 END AS Priority
  FROM ar.vw_OpenItemAged a
  LEFT JOIN ar.CustomerCredit cc ON cc.CompanyCode = a.CompanyCode AND cc.CustomerCode = a.CustomerCode
 CROSS APPLY (SELECT CASE WHEN a.DaysOverdue >= 45 THEN 4 WHEN a.DaysOverdue >= 30 THEN 3 WHEN a.DaysOverdue >= 21 THEN 2
                          WHEN a.DaysOverdue >= 7 THEN 1 ELSE 0 END) AS g (StageNo)
 CROSS APPLY (SELECT CASE g.StageNo WHEN 0 THEN 'STATEMENT' WHEN 1 THEN 'REMINDER1' WHEN 2 THEN 'REMINDER2'
                          WHEN 3 THEN 'ACCOUNTSTOP' ELSE 'FINALDEMAND' END,
                     CASE g.StageNo WHEN 0 THEN 'Open item statement' WHEN 1 THEN 'Courteous reminder with residual and payment details'
                          WHEN 2 THEN 'Firm reminder citing statutory interest and compensation'
                          WHEN 3 THEN 'Credit hold on new release, owner notified'
                          ELSE 'Letter before action under the Pre-Action Protocol for Debt Claims' END) AS s (StageCode, StageAction)
 OUTER APPLY (SELECT TOP (1) x.CaseId, x.CaseRef, x.CaseType, x.ReasonCode, x.Status, x.NextActionDate, x.NextAction, x.EscalatedTo, x.OwnerUser
                FROM ar.CollectionCase x WHERE x.CompanyCode = a.CompanyCode AND x.CustomerCode = a.CustomerCode
                 AND x.Status IN ('OPEN', 'ESCALATED') AND (x.OpenItemId = a.OpenItemId OR x.OpenItemId IS NULL)
               ORDER BY CASE WHEN x.OpenItemId = a.OpenItemId THEN 0 ELSE 1 END, x.CaseId DESC) AS k
 OUTER APPLY (SELECT TOP (1) y.PromiseAmountGBP, y.PromiseDate, y.PromiseStatus FROM ar.CollectionAction y
               WHERE y.CaseId = k.CaseId AND y.ActionType = 'PROMISE' ORDER BY y.ActionDate DESC, y.ActionSeq DESC) AS p
 WHERE a.BucketNo >= 2 AND a.WrittenOff = 0;
GO
