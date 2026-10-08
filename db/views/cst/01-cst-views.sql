/*  cst views - the stock reporting layer.  Views, not procedures, because the client and the
    report writer both read them and neither needs parameters.
    REQ: CST-002, CST-007, CST-021, CST-022, RPT-011 */
SET NOCOUNT ON;
GO
/*  cst.vw_ItemValuation - one line per valuation pool with its allowance taken off, which is
    the number that belongs in the accounts (FRS 102 lower of cost and NRV). */
CREATE OR ALTER VIEW cst.vw_ItemValuation
AS
SELECT ic.CompanyCode,
       ic.ItemCode,
       ic.Revision,
       ic.SiteCode,
       ic.CostMethod,
       ic.QtyOnHand,
       ic.AverageUnitCost,
       ic.CarryingValueGBP,
       ic.AllowanceGBP,
       NetValueGBP = ic.CarryingValueGBP - ic.AllowanceGBP,
       NetUnitCost = CASE WHEN ic.QtyOnHand = 0 THEN NULL
                          ELSE CAST((ic.CarryingValueGBP - ic.AllowanceGBP)
                                    / ic.QtyOnHand AS decimal(19, 6)) END,
       ic.NrvUnitValue,
       ValuationState = CASE WHEN ic.QtyOnHand < 0 THEN 'NEGATIVE'
                             WHEN ic.AllowanceGBP > 0 THEN 'IMPAIRED'
                             WHEN ic.QtyOnHand = 0 AND ic.CarryingValueGBP <> 0 THEN 'RESIDUAL'
                             ELSE 'NORMAL' END
  FROM cst.ItemCost ic;
GO
/*  cst.vw_JobSummary - budget against actual for every job, with the sub-job level shown so
    the reader can see which rows roll into which (D-13). */
CREATE OR ALTER VIEW cst.vw_JobSummary
AS
SELECT j.CompanyCode,
       j.JobRef,
       j.ParentJobRef,
       j.JobLevel,
       j.Description,
       j.CustomerCode,
       j.OperationalStatus,
       j.FinancialStatus,
       BudgetGBP    = CAST(ISNULL(b.TotalBudgetGBP, 0) AS decimal(18, 2)),
       ActualGBP    = CAST(ISNULL(c.ActualGBP, 0) AS decimal(18, 2)),
       CommittedGBP = CAST(ISNULL(c.CommittedGBP, 0) AS decimal(18, 2)),
       VarianceGBP  = CAST(ISNULL(b.TotalBudgetGBP, 0)
                           - ISNULL(c.ActualGBP, 0) - ISNULL(c.CommittedGBP, 0) AS decimal(18, 2))
  FROM cst.Job j
  OUTER APPLY (SELECT TOP (1) jb.TotalBudgetGBP FROM cst.JobBudget jb
                WHERE jb.CompanyCode = j.CompanyCode AND jb.JobRef = j.JobRef
                  AND jb.Status = 'APPROVED'
                ORDER BY jb.Version DESC) b
  OUTER APPLY (SELECT ActualGBP    = SUM(CASE WHEN t.CostNature = 'ACTUAL' THEN t.AmountGBP ELSE 0 END),
                      CommittedGBP = SUM(CASE WHEN t.CostNature = 'COMMITTED' THEN t.AmountGBP ELSE 0 END)
                 FROM cst.JobCostTransaction t
                WHERE t.CompanyCode = j.CompanyCode AND t.JobRef = j.JobRef
                  AND t.ElementCode <> 'REVENUE') c;
GO
/*  cst.vw_CostSetCompare - the live set against the set it was compared with when it was
    approved.  An item priced in only one of the two sets is shown, not dropped (CST-012). */
CREATE OR ALTER VIEW cst.vw_CostSetCompare
AS
SELECT s.CompanyCode,
       SetCode       = s.CostSetCode,
       ComparedTo    = s.ComparedToSetCode,
       ItemCode      = COALESCE(a.ItemCode, p.ItemCode),
       Revision      = COALESCE(a.Revision, p.Revision),
       NewUnitCost   = a.TotalUnitCost,
       PriorUnitCost = p.TotalUnitCost,
       DifferenceGBP = CAST(ISNULL(a.TotalUnitCost, 0) - ISNULL(p.TotalUnitCost, 0) AS decimal(19, 6)),
       DifferencePct = CASE WHEN ISNULL(p.TotalUnitCost, 0) = 0 THEN NULL
                            ELSE CAST((a.TotalUnitCost - p.TotalUnitCost) * 100.0
                                      / p.TotalUnitCost AS decimal(9, 4)) END,
       Presence      = CASE WHEN a.ItemCode IS NULL THEN 'REMOVED'
                            WHEN p.ItemCode IS NULL THEN 'ADDED'
                            ELSE 'BOTH' END
  FROM cst.CostSet s
  LEFT JOIN cst.StandardCost a
         ON a.CompanyCode = s.CompanyCode AND a.CostSetCode = s.CostSetCode
  FULL JOIN cst.StandardCost p
         ON p.CompanyCode = s.CompanyCode AND p.CostSetCode = s.ComparedToSetCode
        AND p.ItemCode = a.ItemCode AND p.Revision = a.Revision
 WHERE s.ComparedToSetCode IS NOT NULL;
GO
