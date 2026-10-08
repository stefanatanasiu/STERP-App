/*  ar.fn_OpenItemAsOf - the sales ledger rebuilt as it stood at the close of a chosen date.  The
    stored residual on ar.OpenItem is today's figure, so an ageing or a statement for an earlier
    date reverses every allocation, settlement discount and write-off that took effect after that
    date.  A report run twice for the same date therefore gives the same answer however much cash
    has arrived since (AR-017, AR-021).  Credit balances keep their negative sign and are carried
    in bucket 0 so that they are never presented as overdue debt.
    REQ: AR-017, AR-018, AR-021, AR-013 */
CREATE OR ALTER FUNCTION ar.fn_OpenItemAsOf (@CompanyCode varchar(10), @AsOf date)
RETURNS TABLE
AS
RETURN
SELECT o.CompanyCode, o.CustomerCode, o.OpenItemId, o.ItemType, o.DocumentNumber, o.DocumentDate, o.DueDate,
       o.PostingDate, o.CurrencyCode, o.FxRate, o.OriginalAmount, o.OriginalAmountGBP, o.TermsCode, o.IsDisputed,
       o.DisputedAmountGBP, o.CollectionStatus, o.WrittenOff, r.ResidualAsOfGBP, v.DaysOverdue, k.BucketNo, n.BucketName,
       CASE WHEN r.ResidualAsOfGBP < 0 THEN -r.ResidualAsOfGBP ELSE 0 END AS CreditGBP,
       CASE WHEN r.ResidualAsOfGBP > 0 THEN r.ResidualAsOfGBP ELSE 0 END AS DebitGBP
  FROM ar.OpenItem o
 CROSS APPLY (SELECT o.OriginalAmountGBP
        - ISNULL((SELECT SUM(t.AppliedAmountGBP + t.DiscountAmountGBP + t.WriteOffAmountGBP) FROM ar.Allocation t
                   WHERE t.TargetOpenItemId = o.OpenItemId AND t.Status = 'ACTIVE' AND t.EffectiveDate <= @AsOf), 0)
        + ISNULL((SELECT SUM(s.AppliedAmountGBP + s.RealisedFxGBP) FROM ar.Allocation s
                   WHERE s.SourceOpenItemId = o.OpenItemId AND s.Status = 'ACTIVE' AND s.EffectiveDate <= @AsOf), 0)) AS r (ResidualAsOfGBP)
 CROSS APPLY (SELECT CASE WHEN @AsOf > o.DueDate THEN DATEDIFF(day, o.DueDate, @AsOf) ELSE 0 END) AS v (DaysOverdue)
 CROSS APPLY (SELECT CASE WHEN r.ResidualAsOfGBP < 0 THEN 0 WHEN v.DaysOverdue = 0 THEN 1 WHEN v.DaysOverdue <= 30 THEN 2
                          WHEN v.DaysOverdue <= 60 THEN 3 WHEN v.DaysOverdue <= 90 THEN 4 ELSE 5 END) AS k (BucketNo)
 CROSS APPLY (SELECT CASE k.BucketNo WHEN 0 THEN 'Credits and unapplied' WHEN 1 THEN 'Not yet due' WHEN 2 THEN '1-30 days overdue'
                          WHEN 3 THEN '31-60 days overdue' WHEN 4 THEN '61-90 days overdue' ELSE 'Over 90 days overdue' END) AS n (BucketName)
 WHERE o.CompanyCode = @CompanyCode AND o.PostingDate <= @AsOf AND o.SettlementStatus <> 'REVERSED' AND r.ResidualAsOfGBP <> 0;
GO
