/* ---------------------------------------------------------------------------
   94-cst-demo - demonstration costing data for STC so that the screens, the
   enquiries and the four batch reports have something real to show.

   What it builds
     * CST27000  a customer contract with two sub-jobs (D-13), an original budget
                 and an approved revision, actual, committed and accrued cost and
                 the revenue that makes the margin meaningful (CST-014..CST-017)
     * CST27100  an internal rectification job with no revenue at all, so the job
                 profitability report has to print "n/a" and not "-100%" (CST-020)
     * WIP balances for both contracts (CST-018)
     * variances of every family the engine raises, one of them deliberately left
       with no owner and no disposal so that CSTVARRP has something to flag
     * FY2027STD, a cost set still in REVIEW, priced against the live FY2026STD so
       that the cost set comparison and the revaluation impact have two sides
       (CST-012); it is NOT activated - activation is a decision, not a seed

   Re-runnable: every insert is guarded, and the whole script is keyed on STC.
   REQ: CST-012, CST-014, CST-015, CST-016, CST-017, CST-018, CST-019, CST-020
   --------------------------------------------------------------------------- */
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'cst.Job') IS NULL OR NOT EXISTS (SELECT 1 FROM core.Company WHERE CompanyCode = 'STC')
BEGIN
    PRINT 'Costing demo skipped - cst tables or company STC not present';
    RETURN;
END
GO
DECLARE @co varchar(10) = 'STC';

/* -- An earlier cut of this seed used illustrative item codes that do not exist in
      inv.Item.  They are removed here so that a database seeded with that cut comes
      back to the same state as a fresh one.                                       */
DELETE FROM cst.StandardCost
 WHERE CompanyCode = @co
   AND ItemCode IN ('CONV-SECT-3M','DRIVE-UNIT-5K','BELT-1200','PANEL-ENC-IP65','PANEL-GEAR-SET');
DELETE FROM cst.Variance
 WHERE CompanyCode = @co
   AND SourceDocRef IN ('GRN270518','API270601','MI270551','TS270419','TS270605','PER202603','LC270620');
DELETE FROM cst.JobCostTransaction
 WHERE CompanyCode = @co AND CreatedBy = 'DEMO'
   AND SourceDocRef IN ('MI270551','MI270604','PO270712','MI270688');

/* -- Jobs and sub-jobs ------------------------------------------------------ */
MERGE cst.Job AS t
USING (VALUES
  ('CST27000', NULL,      1, 'Ridgeway conveyor replacement, phase 2', 'RDG', 'C0010', 'SO270114', 'CUSTOMER', 'JBROWN',  'PROD', '2026-04-06', '2026-09-25', 'OPEN'),
  ('CST27001', 'CST27000',2, 'Ridgeway conveyor - mechanical build',   'RDG', 'C0010', 'SO270114', 'CUSTOMER', 'JBROWN',  'PROD', '2026-04-06', '2026-07-31', 'OPEN'),
  ('CST27002', 'CST27000',2, 'Ridgeway conveyor - control panel',      'RDG', 'C0010', 'SO270114', 'CUSTOMER', 'APATEL',  'PROD', '2026-05-05', '2026-09-04', 'OPEN'),
  ('CST27100', NULL,      1, 'Rework of batch 2026-114, internal',     'RDG', NULL,    NULL,       'RECTIFY',  'MSHAH',   'QUAL', '2026-06-01', '2026-06-30', 'OPEN')
) AS s (JobRef, ParentJobRef, JobLevel, Description, SiteCode, CustomerCode, SalesOrderNumber, Purpose, OwnerUser, CostCentre, StartDate, TargetEndDate, OperationalStatus)
   ON t.CompanyCode = @co AND t.JobRef = s.JobRef
WHEN NOT MATCHED BY TARGET THEN
    INSERT (CompanyCode, JobRef, ParentJobRef, JobLevel, Description, SiteCode, CustomerCode, SalesOrderNumber,
            Purpose, OwnerUser, CostCentre, StartDate, TargetEndDate, OperationalStatus, FinancialStatus, CreatedBy)
    VALUES (@co, s.JobRef, s.ParentJobRef, s.JobLevel, s.Description, s.SiteCode, s.CustomerCode, s.SalesOrderNumber,
            s.Purpose, s.OwnerUser, s.CostCentre, s.StartDate, s.TargetEndDate, s.OperationalStatus, 'OPEN', 'DEMO');

/* -- Budgets.  Version 1 is the original; version 2 is the approved revision
      after the customer added the second drive unit.  Only the approved version
      is the budget - the original is kept so that the change is visible (CST-015). */
MERGE cst.JobBudget AS t
USING (VALUES
  ('CST27000', 1, 1, 48000.00, 'Original quotation 2026-114',                          'SUPERSEDED'),
  ('CST27000', 2, 0, 56500.00, 'Variation 1 - second drive unit agreed 2026-05-18',    'APPROVED'),
  ('CST27001', 1, 1, 31000.00, 'Original mechanical build allowance',                  'APPROVED'),
  ('CST27002', 1, 1, 19500.00, 'Original control panel allowance',                     'APPROVED'),
  ('CST27100', 1, 1,  2400.00, 'Rework allowance agreed by quality',                   'APPROVED')
) AS s (JobRef, Version, IsOriginal, TotalBudgetGBP, ChangeReason, Status)
   ON t.CompanyCode = @co AND t.JobRef = s.JobRef AND t.Version = s.Version
WHEN NOT MATCHED BY TARGET THEN
    INSERT (CompanyCode, JobRef, Version, IsOriginal, TotalBudgetGBP, ChangeReason, Status, ApprovedBy, ApprovedAt, CreatedBy)
    VALUES (@co, s.JobRef, s.Version, s.IsOriginal, s.TotalBudgetGBP, s.ChangeReason, s.Status,
            CASE WHEN s.Status = 'APPROVED' THEN 'DFIELD' ELSE NULL END,
            CASE WHEN s.Status = 'APPROVED' THEN CAST('2026-05-18T09:30:00' AS datetime2(3)) ELSE NULL END, 'DEMO');

INSERT cst.JobBudgetLine (JobBudgetId, LineNum, ElementCode, FiscalYear, BudgetQty, BudgetAmountGBP, Narrative)
SELECT b.JobBudgetId, v.LineNum, v.ElementCode, 2026, v.BudgetQty, v.BudgetAmountGBP, v.Narrative
  FROM (VALUES
  ('CST27000', 2, 1, 'MATERIAL', CAST(NULL AS decimal(19,6)), 31500.00, 'Conveyor sections, drives and belting'),
  ('CST27000', 2, 2, 'LABOUR',   CAST(640 AS decimal(19,6)),  16000.00, 'Fitters, 640 hours at 25.00'),
  ('CST27000', 2, 3, 'OVHDVAR',  CAST(640 AS decimal(19,6)),   5760.00, 'Production overhead at 9.00 per labour hour'),
  ('CST27000', 2, 4, 'SUBCON',   CAST(NULL AS decimal(19,6)),  3240.00, 'Electrical commissioning'),
  ('CST27100', 1, 1, 'LABOUR',   CAST(72 AS decimal(19,6)),    1800.00, 'Rework, 72 hours at 25.00'),
  ('CST27100', 1, 2, 'OVHDVAR',  CAST(72 AS decimal(19,6)),     600.00, 'Overhead on rework hours')
  ) AS v (JobRef, Version, LineNum, ElementCode, BudgetQty, BudgetAmountGBP, Narrative)
  JOIN cst.JobBudget b ON b.CompanyCode = @co AND b.JobRef = v.JobRef AND b.Version = v.Version
 WHERE EXISTS (SELECT 1 FROM cst.CostElement e WHERE e.ElementCode = v.ElementCode)
   AND NOT EXISTS (SELECT 1 FROM cst.JobBudgetLine l WHERE l.JobBudgetId = b.JobBudgetId AND l.LineNum = v.LineNum);

/* -- Cost transactions.  Cost sits on the sub-job that incurred it; the parent
      gets it exactly once through the roll-up, never by re-posting (D-13).
      REVENUE is held as a negative cost so that one table carries both sides.  */
INSERT cst.JobCostTransaction (CompanyCode, JobRef, ElementCode, SourceModule, SourceDocType, SourceDocRef, ItemCode,
                               WorkCentreCode, Quantity, Hours, Rate, Amount, AmountGBP, CostNature, RecognitionDate,
                               FiscalYear, PeriodNo, Narrative, CreatedBy)
SELECT @co, v.JobRef, v.ElementCode, v.SourceModule, v.SourceDocType, v.SourceDocRef, v.ItemCode,
       v.WorkCentreCode, v.Quantity, v.Hours, v.Rate, v.AmountGBP, v.AmountGBP, v.CostNature, v.RecognitionDate,
       2026, v.PeriodNo, v.Narrative, 'DEMO'
  FROM (VALUES
  ('CST27001','MATERIAL','INV','ISSUE',  'MI270551','BAR-AL6082-25MM', NULL,       CAST(510 AS decimal(19,6)), CAST(NULL AS decimal(19,6)), CAST(NULL AS decimal(19,6)),  9435.00,'ACTUAL',   CAST('2026-04-21' AS date),1,'Aluminium bar issued to the build'),
  ('CST27001','MATERIAL','INV','ISSUE',  'MI270604','BATTERY-36V-14AH',NULL,       CAST(50 AS decimal(19,6)),  CAST(NULL AS decimal(19,6)), CAST(NULL AS decimal(19,6)),  7100.00,'ACTUAL',   CAST('2026-05-19' AS date),2,'Battery packs, bought after the variation'),
  ('CST27001','LABOUR',  'CST','TIME',   'TS270419',NULL,           'WC-FIT',   CAST(NULL AS decimal(19,6)),CAST(318 AS decimal(19,6)), CAST(25.00 AS decimal(19,6)), 7950.00,'ACTUAL',   CAST('2026-05-29' AS date),2,'Fitting hours to date'),
  ('CST27001','OVHDVAR', 'CST','ABSORB', 'OV270529',NULL,           'WC-FIT',   CAST(NULL AS decimal(19,6)),CAST(318 AS decimal(19,6)), CAST(9.00 AS decimal(19,6)),  2862.00,'ACTUAL',   CAST('2026-05-29' AS date),2,'Overhead absorbed on fitting hours'),
  ('CST27001','MATERIAL','PO', 'ORDER',  'PO270712','BAR-TI6AL4V-16MM',NULL,     CAST(60 AS decimal(19,6)), CAST(NULL AS decimal(19,6)), CAST(NULL AS decimal(19,6)),  4380.00,'COMMITTED',CAST('2026-06-08' AS date),3,'Titanium bar ordered, not yet received'),
  ('CST27002','MATERIAL','INV','ISSUE',  'MI270688','BEARING-6802-2RS',NULL,     CAST(600 AS decimal(19,6)),  CAST(NULL AS decimal(19,6)), CAST(NULL AS decimal(19,6)),  2310.00,'ACTUAL',   CAST('2026-05-26' AS date),2,'Bearings for the drive assembly'),
  ('CST27002','LABOUR',  'CST','TIME',   'TS270605',NULL,           'WC-ELEC',  CAST(NULL AS decimal(19,6)),CAST(96 AS decimal(19,6)),  CAST(28.00 AS decimal(19,6)), 2688.00,'ACTUAL',   CAST('2026-06-05' AS date),3,'Panel wiring'),
  ('CST27002','SUBCON',  'AP', 'ACCRUAL','AC270630',NULL,           NULL,       CAST(NULL AS decimal(19,6)),CAST(NULL AS decimal(19,6)),CAST(NULL AS decimal(19,6)),  1640.00,'ACCRUED',  CAST('2026-06-30' AS date),3,'Commissioning engineer, work done not invoiced'),
  ('CST27000','REVENUE', 'AR', 'INVOICE','SI270331',NULL,           NULL,       CAST(NULL AS decimal(19,6)),CAST(NULL AS decimal(19,6)),CAST(NULL AS decimal(19,6)),-38000.00,'ACTUAL',  CAST('2026-06-20' AS date),3,'Stage payment 1 invoiced'),
  ('CST27100','LABOUR',  'CST','TIME',   'TS270615',NULL,           'WC-FIT',   CAST(NULL AS decimal(19,6)),CAST(84 AS decimal(19,6)),  CAST(25.00 AS decimal(19,6)), 2100.00,'ACTUAL',   CAST('2026-06-15' AS date),3,'Rework hours - over the allowance'),
  ('CST27100','OVHDVAR', 'CST','ABSORB', 'OV270615',NULL,           'WC-FIT',   CAST(NULL AS decimal(19,6)),CAST(84 AS decimal(19,6)),  CAST(9.00 AS decimal(19,6)),   756.00,'ACTUAL',   CAST('2026-06-15' AS date),3,'Overhead on rework hours')
  ) AS v (JobRef, ElementCode, SourceModule, SourceDocType, SourceDocRef, ItemCode, WorkCentreCode, Quantity, Hours, Rate,
          AmountGBP, CostNature, RecognitionDate, PeriodNo, Narrative)
 WHERE EXISTS (SELECT 1 FROM cst.CostElement e WHERE e.ElementCode = v.ElementCode)
   AND NOT EXISTS (SELECT 1 FROM cst.JobCostTransaction t
                    WHERE t.CompanyCode = @co AND t.JobRef = v.JobRef AND t.SourceDocRef = v.SourceDocRef
                      AND t.ElementCode = v.ElementCode);

/* -- WIP.  Closing value is the arithmetic of the columns to its left, which is
      how WIPCALC proves itself at period end (CST-018).                        */
MERGE cst.WipBalance AS t
USING (VALUES
  ('CST27000', 2026, 3, 0.00,    18845.00, 0.00,  10638.00, 2862.00, 1640.00, 0.00,    0.00, 0.00),
  ('CST27100', 2026, 3, 0.00,        0.00, 0.00,   2100.00,  756.00,    0.00, 0.00,    0.00, 0.00)
) AS s (JobRef, FiscalYear, PeriodNo, OpeningValueGBP, MaterialIssuesGBP, MaterialReturnsGBP, LabourGBP,
        OverheadGBP, SubcontractGBP, CompletionTransfersGBP, ScrapGBP, WriteOffGBP)
   ON t.CompanyCode = @co AND t.JobRef = s.JobRef AND t.FiscalYear = s.FiscalYear AND t.PeriodNo = s.PeriodNo
      AND t.KitRef IS NULL
WHEN NOT MATCHED BY TARGET THEN
    INSERT (CompanyCode, JobRef, FiscalYear, PeriodNo, OpeningValueGBP, MaterialIssuesGBP, MaterialReturnsGBP,
            LabourGBP, OverheadGBP, SubcontractGBP, CompletionTransfersGBP, ScrapGBP, WriteOffGBP,
            ValueAdjustmentGBP, ClosingValueGBP, LastUpdatedAt)
    VALUES (@co, s.JobRef, s.FiscalYear, s.PeriodNo, s.OpeningValueGBP, s.MaterialIssuesGBP, s.MaterialReturnsGBP,
            s.LabourGBP, s.OverheadGBP, s.SubcontractGBP, s.CompletionTransfersGBP, s.ScrapGBP, s.WriteOffGBP, 0.00,
            s.OpeningValueGBP + s.MaterialIssuesGBP - s.MaterialReturnsGBP + s.LabourGBP + s.OverheadGBP
              + s.SubcontractGBP - s.CompletionTransfersGBP - s.ScrapGBP - s.WriteOffGBP, SYSDATETIME());

/* -- Variances.  The last one has no owner and no disposal on purpose: CSTVARRP
      exists to make exactly that kind of row impossible to ignore (CST-019).   */
INSERT cst.Variance (CompanyCode, VarianceType, ItemCode, JobRef, SourceDocType, SourceDocRef, BaselineQty, ActualQty,
                     BaselineRate, ActualRate, QuantityVarianceGBP, RateVarianceGBP, TotalVarianceGBP, IsAdverse,
                     FiscalYear, PeriodNo, ReasonCode, Reason, OwnerUser, Disposal)
SELECT @co, v.VarianceType, v.ItemCode, v.JobRef, v.SourceDocType, v.SourceDocRef, v.BaselineQty, v.ActualQty,
       v.BaselineRate, v.ActualRate, v.QuantityVarianceGBP, v.RateVarianceGBP,
       v.QuantityVarianceGBP + v.RateVarianceGBP,
       CASE WHEN v.QuantityVarianceGBP + v.RateVarianceGBP >= 0 THEN 1 ELSE 0 END,
       2026, v.PeriodNo, v.ReasonCode, v.Reason, v.OwnerUser, v.Disposal
  FROM (VALUES
  ('PPV',         'BATTERY-36V-14AH', CAST(NULL AS varchar(20)), 'GRN','GRN270518', CAST(50 AS decimal(19,6)), CAST(50 AS decimal(19,6)), CAST(142.00 AS decimal(19,6)), CAST(148.00 AS decimal(19,6)), CAST(0.00 AS decimal(18,2)), CAST(300.00 AS decimal(18,2)), 2,'SUPPRICE','Supplier increase not yet in the standard','KWATSON','ACCEPTED'),
  ('INVOICEPRICE','BATTERY-36V-14AH', CAST(NULL AS varchar(20)), 'API','API270601', CAST(50 AS decimal(19,6)), CAST(50 AS decimal(19,6)), CAST(148.00 AS decimal(19,6)), CAST(149.20 AS decimal(19,6)), CAST(0.00 AS decimal(18,2)), CAST(60.00 AS decimal(18,2)),  3,'SUPPRICE','Invoice above the receipt price - increment only','KWATSON','ACCEPTED'),
  ('MATUSAGE',    'BAR-AL6082-25MM',  'CST27001',                'ISS','MI270551',  CAST(453 AS decimal(19,6)),CAST(510 AS decimal(19,6)),CAST(18.50 AS decimal(19,6)),  CAST(18.50 AS decimal(19,6)),  CAST(1054.50 AS decimal(18,2)),CAST(0.00 AS decimal(18,2)),  1,'SITECOND','Extra bar used, site levels out of tolerance','JBROWN','INVESTIGATE'),
  ('LABEFF',      CAST(NULL AS varchar(40)),'CST27001',       'TIM','TS270419',  CAST(280 AS decimal(19,6)),CAST(318 AS decimal(19,6)),CAST(25.00 AS decimal(19,6)),   CAST(25.00 AS decimal(19,6)),   CAST(950.00 AS decimal(18,2)), CAST(0.00 AS decimal(18,2)),  2,'ACCESS','Access restricted while the line was running','JBROWN','ACCEPTED'),
  ('LABRATE',     CAST(NULL AS varchar(40)),'CST27002',       'TIM','TS270605',  CAST(96 AS decimal(19,6)), CAST(96 AS decimal(19,6)), CAST(26.00 AS decimal(19,6)),   CAST(28.00 AS decimal(19,6)),   CAST(0.00 AS decimal(18,2)),   CAST(192.00 AS decimal(18,2)),3,'GRADE','Senior electrician used on a standard grade task','APATEL','ACCEPTED'),
  ('OVHD',        CAST(NULL AS varchar(40)),CAST(NULL AS varchar(20)),'PER','PER202603',CAST(1800 AS decimal(19,6)),CAST(1642 AS decimal(19,6)),CAST(9.00 AS decimal(19,6)),CAST(9.00 AS decimal(19,6)),CAST(1422.00 AS decimal(18,2)),CAST(0.00 AS decimal(18,2)),3,'CAPACITY','Under absorbed - hours below normal capacity (D-05)','DFIELD','PANDL'),
  ('LANDED',      'BAR-TI6AL4V-16MM', CAST(NULL AS varchar(20)), 'LND','LC270620',  CAST(NULL AS decimal(19,6)),CAST(NULL AS decimal(19,6)),CAST(NULL AS decimal(19,6)),  CAST(NULL AS decimal(19,6)),    CAST(0.00 AS decimal(18,2)),   CAST(-118.00 AS decimal(18,2)),3,'FREIGHT','Freight settled below the estimate',CAST(NULL AS varchar(20)),CAST(NULL AS varchar(16)))
  ) AS v (VarianceType, ItemCode, JobRef, SourceDocType, SourceDocRef, BaselineQty, ActualQty, BaselineRate, ActualRate,
          QuantityVarianceGBP, RateVarianceGBP, PeriodNo, ReasonCode, Reason, OwnerUser, Disposal)
 WHERE NOT EXISTS (SELECT 1 FROM cst.Variance x WHERE x.CompanyCode = @co AND x.SourceDocRef = v.SourceDocRef
                     AND x.VarianceType = v.VarianceType);

/* -- A proposed cost set, still in REVIEW.  It is priced against FY2026STD, so
      CSTSETCM has a previous and a proposed side and can show what activation
      would post.  Nothing here activates it (CST-012, CST-023).               */
IF NOT EXISTS (SELECT 1 FROM cst.CostSet WHERE CompanyCode = @co AND CostSetCode = 'FY2027STD')
    INSERT cst.CostSet (CompanyCode, CostSetCode, Name, Purpose, EffectiveFrom, CurrencyCode, FiscalYear, Status,
                        ComparedToSetCode, SourceEvidence, PreparedBy, ReviewedBy, CreatedBy)
    VALUES (@co, 'FY2027STD', 'Standard cost set, financial year 2027', 'STANDARD', '2027-01-01', 'GBP', 2027, 'REVIEW',
            'FY2026STD', 'Supplier price list 2027 and the agreed rate review', 'KWATSON', 'DFIELD', 'DEMO');

/* -- The two sides of the comparison.  Two items move more than five per cent, one
      barely moves, one is new to the proposal and one has been dropped from it,
      which gives the report one line in each of its bands.                      */
INSERT cst.StandardCost (CompanyCode, CostSetCode, ItemCode, Revision, CostingLotSize, TotalUnitCost,
                         PreviousUnitCost, VariancePct, Completeness)
SELECT @co, v.CostSetCode, v.ItemCode, '', 1, v.TotalUnitCost, v.PreviousUnitCost,
       CASE WHEN ISNULL(v.PreviousUnitCost, 0) = 0 THEN NULL
            ELSE CAST((v.TotalUnitCost - v.PreviousUnitCost) * 100.0 / v.PreviousUnitCost AS decimal(9,4)) END,
       'COMPLETE'
  FROM (VALUES
  ('FY2026STD','BAR-AL6082-25MM',  CAST(18.500000 AS decimal(19,6)), CAST(NULL AS decimal(19,6))),
  ('FY2026STD','BAR-TI6AL4V-16MM', CAST(73.000000 AS decimal(19,6)), CAST(NULL AS decimal(19,6))),
  ('FY2026STD','BEARING-6802-2RS', CAST(3.850000 AS decimal(19,6)),  CAST(NULL AS decimal(19,6))),
  ('FY2026STD','BB-SHELL-BSA68',   CAST(11.200000 AS decimal(19,6)), CAST(NULL AS decimal(19,6))),
  ('FY2027STD','BAR-AL6082-25MM',  CAST(19.980000 AS decimal(19,6)), CAST(18.500000 AS decimal(19,6))),
  ('FY2027STD','BAR-TI6AL4V-16MM', CAST(73.730000 AS decimal(19,6)), CAST(73.000000 AS decimal(19,6))),
  ('FY2027STD','BEARING-6802-2RS', CAST(4.210000 AS decimal(19,6)),  CAST(3.850000 AS decimal(19,6))),
  ('FY2027STD','BATTERY-36V-14AH', CAST(148.000000 AS decimal(19,6)),CAST(NULL AS decimal(19,6)))
  ) AS v (CostSetCode, ItemCode, TotalUnitCost, PreviousUnitCost)
 WHERE EXISTS (SELECT 1 FROM cst.CostSet c WHERE c.CompanyCode = @co AND c.CostSetCode = v.CostSetCode)
   AND NOT EXISTS (SELECT 1 FROM cst.StandardCost s WHERE s.CompanyCode = @co AND s.CostSetCode = v.CostSetCode
                     AND s.ItemCode = v.ItemCode AND s.Revision = '');

/* -- Costed pools so that the valuation report and the revaluation impact have a
      quantity to work on.  BAR-AL6082-25MM is written down to net realisable value
      after the redesign, which is what the NRV lines of CSTINVAL are for.       */
MERGE cst.ItemCost AS t
USING (VALUES
  ('BAR-AL6082-25MM',  '*', 'MWA',  CAST(260 AS decimal(19,6)), CAST(4810.00 AS decimal(18,2)),  CAST(18.500000 AS decimal(19,6)), CAST(18.500000 AS decimal(19,6)), CAST(16.000000 AS decimal(19,6)), CAST(650.00 AS decimal(18,2))),
  ('BEARING-6802-2RS', '*', 'MWA',  CAST(240 AS decimal(19,6)), CAST(924.00 AS decimal(18,2)),   CAST(3.850000 AS decimal(19,6)),  CAST(3.850000 AS decimal(19,6)),  CAST(NULL AS decimal(19,6)),      CAST(0.00 AS decimal(18,2))),
  ('BAR-TI6AL4V-16MM', '*', 'FIFO', CAST(140 AS decimal(19,6)), CAST(10290.00 AS decimal(18,2)), CAST(73.500000 AS decimal(19,6)), CAST(NULL AS decimal(19,6)),      CAST(NULL AS decimal(19,6)),      CAST(0.00 AS decimal(18,2))),
  ('BB-SHELL-BSA68',   '*', 'MWA',  CAST(60 AS decimal(19,6)),  CAST(672.00 AS decimal(18,2)),   CAST(11.200000 AS decimal(19,6)), CAST(11.200000 AS decimal(19,6)), CAST(NULL AS decimal(19,6)),      CAST(0.00 AS decimal(18,2)))
) AS s (ItemCode, SiteCode, CostMethod, QtyOnHand, CarryingValueGBP, AverageUnitCost, StandardUnitCost, NrvUnitValue, AllowanceGBP)
   ON t.CompanyCode = @co AND t.ItemCode = s.ItemCode AND t.Revision = '' AND t.SiteCode = s.SiteCode
WHEN NOT MATCHED BY TARGET THEN
    INSERT (CompanyCode, ItemCode, Revision, SiteCode, CostMethod, QtyOnHand, CarryingValueGBP, AverageUnitCost,
            StandardUnitCost, NrvUnitValue, AllowanceGBP, LastUpdatedAt, LastUpdatedBy)
    VALUES (@co, s.ItemCode, '', s.SiteCode, s.CostMethod, s.QtyOnHand, s.CarryingValueGBP, s.AverageUnitCost,
            s.StandardUnitCost, s.NrvUnitValue, s.AllowanceGBP, SYSDATETIME(), 'DEMO');

/* -- FIFO layers for the one FIFO pool, oldest first, adding back to the pool
      value so that the layer line of the reconciliation is zero.               */
INSERT cst.CostLayer (CompanyCode, ItemCode, Revision, LayerSeq, ReceiptDate, SourceDocType, SourceDocRef,
                      QtyOriginal, QtyRemaining, UnitCost, ValueRemainingGBP, IsClosed)
SELECT @co, 'BAR-TI6AL4V-16MM', '', v.LayerSeq, v.ReceiptDate, 'GRN', v.SourceDocRef, v.QtyOriginal, v.QtyRemaining,
       v.UnitCost, CAST(v.QtyRemaining * v.UnitCost AS decimal(18,2)), 0
  FROM (VALUES
  (CAST(1 AS bigint), CAST('2026-03-18' AS date), 'GRN270318', CAST(80 AS decimal(19,6)), CAST(40 AS decimal(19,6)),  CAST(72.000000 AS decimal(19,6))),
  (CAST(2 AS bigint), CAST('2026-05-06' AS date), 'GRN270506', CAST(60 AS decimal(19,6)), CAST(60 AS decimal(19,6)),  CAST(73.500000 AS decimal(19,6))),
  (CAST(3 AS bigint), CAST('2026-06-11' AS date), 'GRN270611', CAST(40 AS decimal(19,6)), CAST(40 AS decimal(19,6)),  CAST(75.000000 AS decimal(19,6)))
  ) AS v (LayerSeq, ReceiptDate, SourceDocRef, QtyOriginal, QtyRemaining, UnitCost)
 WHERE NOT EXISTS (SELECT 1 FROM cst.CostLayer l WHERE l.CompanyCode = @co AND l.ItemCode = 'BAR-TI6AL4V-16MM'
                     AND l.Revision = '' AND l.LayerSeq = v.LayerSeq);

PRINT 'Seeded costing demo data (jobs, budgets, cost, WIP, variances, FY2027STD)';
GO
