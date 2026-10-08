/*  rpt.vw_ExceptionAgeing - every control exception with its age counted from first detection and
    the same four buckets the EXCPAGE batch prints: 0-7 days, 8-30 days, 31-60 days and 61 days
    plus.  Age runs from RaisedAt, never from the last reassignment, so moving an exception to a
    new owner does not make it look younger (XMD-006).  An exception with neither an owning user
    nor an owning role is reported as (UNOWNED) rather than quietly dropped.
    A view takes no parameter: AsOf is today, and a report for an earlier cutoff filters
    RaisedDate <= cutoff and recomputes the age from RaisedDate.
    REQ: XMD-006, XMD-009, XMD-012, COM-022, RPT-016 */
SET NOCOUNT ON;
GO
IF SCHEMA_ID(N'rpt') IS NULL EXEC (N'CREATE SCHEMA rpt');
GO
CREATE OR ALTER VIEW rpt.vw_ExceptionAgeing
AS
SELECT e.ExceptionId,
       e.CompanyCode,
       e.SiteCode,
       e.Module,
       e.Severity,
       e.Category,
       ISNULL(e.DocumentType, '')   AS DocumentType,
       ISNULL(e.DocumentRef, '')    AS DocumentRef,
       ISNULL(e.Message, '')        AS Message,
       ISNULL(e.AmountGBP, 0)       AS AmountGBP,
       e.Quantity,
       e.OwnerUser,
       e.OwnerRole,
       o.Owner,
       e.Priority,
       e.RaisedAt,
       CAST(e.RaisedAt AS date)     AS RaisedDate,
       e.RaisedBy,
       e.DueAt,
       e.EscalatedAt,
       e.Status,
       e.ResolvedAt,
       d.AsOf,
       v.AgeDays,
       b.BucketNo,
       n.BucketName,
       CASE WHEN o.Owner = '(UNOWNED)' THEN 1 ELSE 0 END AS IsUnowned,
       CASE WHEN e.Severity = 'HOLD' THEN 1 ELSE 0 END   AS IsHold,
       CASE WHEN e.DueAt IS NOT NULL AND e.DueAt < CAST(d.AsOf AS datetime2(3)) THEN 1 ELSE 0 END AS IsPastDue,
       CASE WHEN e.EscalatedAt IS NOT NULL THEN 1 ELSE 0 END AS IsEscalated,
       CASE WHEN b.BucketNo = 1 THEN 1 ELSE 0 END AS Count0007,
       CASE WHEN b.BucketNo = 2 THEN 1 ELSE 0 END AS Count0830,
       CASE WHEN b.BucketNo = 3 THEN 1 ELSE 0 END AS Count3160,
       CASE WHEN b.BucketNo = 4 THEN 1 ELSE 0 END AS Count61Up,
       CASE WHEN b.BucketNo = 1 THEN ISNULL(e.AmountGBP, 0) ELSE 0 END AS Value0007GBP,
       CASE WHEN b.BucketNo = 2 THEN ISNULL(e.AmountGBP, 0) ELSE 0 END AS Value0830GBP,
       CASE WHEN b.BucketNo = 3 THEN ISNULL(e.AmountGBP, 0) ELSE 0 END AS Value3160GBP,
       CASE WHEN b.BucketNo = 4 THEN ISNULL(e.AmountGBP, 0) ELSE 0 END AS Value61UpGBP
  FROM core.ExceptionItem e
 CROSS APPLY (SELECT CAST(GETDATE() AS date)) AS d (AsOf)
 CROSS APPLY (SELECT ISNULL(NULLIF(e.OwnerUser, ''), ISNULL(NULLIF(e.OwnerRole, ''), '(UNOWNED)'))) AS o (Owner)
 CROSS APPLY (SELECT DATEDIFF(day, e.RaisedAt, d.AsOf)) AS v (AgeDays)
 CROSS APPLY (SELECT CASE WHEN v.AgeDays <= 7 THEN 1 WHEN v.AgeDays <= 30 THEN 2
                          WHEN v.AgeDays <= 60 THEN 3 ELSE 4 END) AS b (BucketNo)
 CROSS APPLY (SELECT CASE b.BucketNo WHEN 1 THEN '0-7 days' WHEN 2 THEN '8-30 days'
                          WHEN 3 THEN '31-60 days' ELSE '61 days plus' END) AS n (BucketName);
GO
