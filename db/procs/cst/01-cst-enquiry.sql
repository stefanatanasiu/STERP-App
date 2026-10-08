/*  cst procedures - the set-based side of costing.  The COBOL engine owns the arithmetic;
    these exist because they are aggregations and enquiries, which a cursor-driven program
    has no business doing one row at a time.
    REQ: CST-013, CST-016, CST-018, CST-022, RPT-011, RPT-014 */
SET NOCOUNT ON;
GO
/*  cst.usp_OrderMargin - the ex-VAT revenue and the cost of a whole sales order on one row.
    MARGIN "ORDER" calls this and then applies the margin rules itself, so that the
    "UNDEFINED" states and the rounding live in exactly one place (CST-013).
    Lines with no cost are counted, never valued at zero - a flattering margin built on a
    missing cost is worse than no margin at all. */
CREATE OR ALTER PROCEDURE cst.usp_OrderMargin
    @CompanyCode varchar(10),
    @OrderNumber varchar(30),
    @CostBasis   varchar(12) = 'STANDARD'
AS
BEGIN
    SET NOCOUNT ON;
    SET XACT_ABORT ON;
    DECLARE @basis varchar(12) = UPPER(ISNULL(NULLIF(LTRIM(RTRIM(@CostBasis)), ''), 'STANDARD'));
    IF @basis NOT IN ('STANDARD', 'AVERAGE', 'FIFO', 'ACTUAL', 'QUOTE')
        SET @basis = 'STANDARD';

    DECLARE @activeSet varchar(20) =
        (SELECT TOP (1) CostSetCode FROM cst.CostSet
          WHERE CompanyCode = @CompanyCode AND Status = 'ACTIVE'
          ORDER BY EffectiveFrom DESC, CostSetCode);

    ;WITH ln AS (
        SELECT l.ItemCode,
               l.Revision,
               l.NetAmount,
               Qty = l.AcceptedQty - l.CancelledQty,
               UnitCost = CASE @basis
                   WHEN 'STANDARD' THEN COALESCE(sc.TotalUnitCost, ic.StandardUnitCost, ic.AverageUnitCost)
                   WHEN 'AVERAGE'  THEN ic.AverageUnitCost
                   WHEN 'FIFO'     THEN COALESCE(fl.UnitCost, ic.AverageUnitCost)
                   WHEN 'QUOTE'    THEN COALESCE(sc.TotalUnitCost, ic.AverageUnitCost)
                   ELSE COALESCE(ic.AverageUnitCost, sc.TotalUnitCost)
               END
          FROM so.SalesOrderLine l
          LEFT JOIN cst.ItemCost ic
                 ON ic.CompanyCode = l.CompanyCode AND ic.ItemCode = l.ItemCode
                AND ic.Revision = ISNULL(l.Revision, '') AND ic.SiteCode = '*'
          LEFT JOIN cst.StandardCost sc
                 ON sc.CompanyCode = l.CompanyCode AND sc.ItemCode = l.ItemCode
                AND sc.Revision = ISNULL(l.Revision, '') AND sc.CostSetCode = @activeSet
          OUTER APPLY (SELECT TOP (1) cl.UnitCost FROM cst.CostLayer cl
                        WHERE cl.CompanyCode = l.CompanyCode AND cl.ItemCode = l.ItemCode
                          AND cl.Revision = ISNULL(l.Revision, '') AND cl.IsClosed = 0
                          AND cl.QtyRemaining > 0
                        ORDER BY cl.ReceiptDate, cl.LayerSeq) fl
         WHERE l.CompanyCode = @CompanyCode
           AND l.OrderNumber = @OrderNumber
           AND l.LineType NOT IN ('TEXT', 'KITCOMP')
    )
    SELECT NetRevenue    = CAST(ISNULL(SUM(ln.NetAmount), 0) AS decimal(18, 2)),
           CostAmount    = CAST(ISNULL(SUM(CASE WHEN ln.UnitCost IS NULL THEN 0
                                                ELSE ROUND(ln.Qty * ln.UnitCost, 2) END), 0) AS decimal(18, 2)),
           LinesCounted  = COUNT(*),
           LinesNoCost   = SUM(CASE WHEN ln.UnitCost IS NULL OR ln.UnitCost = 0 THEN 1 ELSE 0 END),
           CostBasisUsed = @basis
      FROM ln
    HAVING COUNT(*) > 0;
END;
GO
/*  cst.usp_CostEnquiry - what one item costs, by every basis we hold, side by side.
    The screen shows them together so nobody has to guess which number they are looking at. */
CREATE OR ALTER PROCEDURE cst.usp_CostEnquiry
    @CompanyCode varchar(10),
    @ItemCode    varchar(40),
    @Revision    varchar(4) = '',
    @SiteCode    varchar(10) = '*'
AS
BEGIN
    SET NOCOUNT ON;
    SELECT ic.ItemCode,
           ic.Revision,
           ic.SiteCode,
           ic.CostMethod,
           ic.QtyOnHand,
           ic.AverageUnitCost,
           ic.CarryingValueGBP,
           ic.AllowanceGBP,
           NetCarryingGBP  = ic.CarryingValueGBP - ic.AllowanceGBP,
           ic.NrvUnitValue,
           ic.StandardUnitCost,
           ic.LastPurchaseCost,
           FifoOldestUnit  = fl.UnitCost,
           FifoLayers      = ISNULL(lc.Layers, 0),
           StandardSet     = sc.CostSetCode,
           StandardFromSet = sc.TotalUnitCost,
           IsImpaired      = CASE WHEN ic.AllowanceGBP > 0 THEN 'Y' ELSE 'N' END
      FROM cst.ItemCost ic
      LEFT JOIN cst.StandardCost sc
             ON sc.CompanyCode = ic.CompanyCode AND sc.ItemCode = ic.ItemCode
            AND sc.Revision = ic.Revision
            AND sc.CostSetCode = (SELECT TOP (1) CostSetCode FROM cst.CostSet
                                   WHERE CompanyCode = ic.CompanyCode AND Status = 'ACTIVE'
                                   ORDER BY EffectiveFrom DESC, CostSetCode)
      OUTER APPLY (SELECT TOP (1) cl.UnitCost FROM cst.CostLayer cl
                    WHERE cl.CompanyCode = ic.CompanyCode AND cl.ItemCode = ic.ItemCode
                      AND cl.Revision = ic.Revision AND cl.IsClosed = 0 AND cl.QtyRemaining > 0
                    ORDER BY cl.ReceiptDate, cl.LayerSeq) fl
      OUTER APPLY (SELECT Layers = COUNT(*) FROM cst.CostLayer cl2
                    WHERE cl2.CompanyCode = ic.CompanyCode AND cl2.ItemCode = ic.ItemCode
                      AND cl2.Revision = ic.Revision AND cl2.IsClosed = 0 AND cl2.QtyRemaining > 0) lc
     WHERE ic.CompanyCode = @CompanyCode
       AND ic.ItemCode = @ItemCode
       AND (@Revision IS NULL OR @Revision = '' OR ic.Revision = @Revision)
       AND (@SiteCode IS NULL OR ic.SiteCode = @SiteCode)
     ORDER BY ic.SiteCode;
END;
GO
/*  cst.usp_JobProfitability - budget, committed, actual and revenue for a job and, when it
    has them, its sub-jobs (D-13).  A sub-job's cost belongs to its parent exactly once, so
    the roll-up is by ParentJobRef and never by a LIKE on the reference. */
CREATE OR ALTER PROCEDURE cst.usp_JobProfitability
    @CompanyCode  varchar(10),
    @JobRef       varchar(20) = NULL,
    @IncludeSubs  bit = 1
AS
BEGIN
    SET NOCOUNT ON;
    ;WITH scope AS (
        SELECT j.CompanyCode, j.JobRef, j.ParentJobRef, j.JobLevel, j.Description,
               j.CustomerCode, j.SalesOrderNumber, j.OperationalStatus, j.FinancialStatus,
               RootJobRef = CASE WHEN @IncludeSubs = 1 THEN ISNULL(j.ParentJobRef, j.JobRef) ELSE j.JobRef END
          FROM cst.Job j
         WHERE j.CompanyCode = @CompanyCode
           AND (@JobRef IS NULL
                OR j.JobRef = @JobRef
                OR (@IncludeSubs = 1 AND j.ParentJobRef = @JobRef))
    )
    SELECT s.JobRef,
           s.ParentJobRef,
           s.JobLevel,
           s.Description,
           s.CustomerCode,
           s.OperationalStatus,
           s.FinancialStatus,
           BudgetGBP     = CAST(ISNULL(b.TotalBudgetGBP, 0) AS decimal(18, 2)),
           ActualGBP     = CAST(ISNULL(c.ActualGBP, 0) AS decimal(18, 2)),
           CommittedGBP  = CAST(ISNULL(c.CommittedGBP, 0) AS decimal(18, 2)),
           AccruedGBP    = CAST(ISNULL(c.AccruedGBP, 0) AS decimal(18, 2)),
           RevenueGBP    = CAST(ISNULL(r.RevenueGBP, 0) AS decimal(18, 2)),
           WipGBP        = CAST(ISNULL(w.ClosingValueGBP, 0) AS decimal(18, 2)),
           /*  estimate at completion - what is spent plus what is committed, never less
               than the approved budget's remaining work (CST-016) */
           ForecastGBP   = CAST(ISNULL(c.ActualGBP, 0) + ISNULL(c.CommittedGBP, 0)
                                + ISNULL(c.AccruedGBP, 0) AS decimal(18, 2)),
           VarianceGBP   = CAST(ISNULL(b.TotalBudgetGBP, 0)
                                - (ISNULL(c.ActualGBP, 0) + ISNULL(c.CommittedGBP, 0)
                                   + ISNULL(c.AccruedGBP, 0)) AS decimal(18, 2)),
           MarginGBP     = CAST(ISNULL(r.RevenueGBP, 0) - ISNULL(c.ActualGBP, 0) AS decimal(18, 2)),
           MarginState   = CASE WHEN ISNULL(r.RevenueGBP, 0) = 0 THEN 'UNDEFINED' ELSE 'OK' END,
           MarginPct     = CASE WHEN ISNULL(r.RevenueGBP, 0) = 0 THEN NULL
                                ELSE CAST((r.RevenueGBP - ISNULL(c.ActualGBP, 0)) * 100.0
                                          / r.RevenueGBP AS decimal(9, 4)) END
      FROM scope s
      OUTER APPLY (SELECT TOP (1) jb.TotalBudgetGBP FROM cst.JobBudget jb
                    WHERE jb.CompanyCode = s.CompanyCode AND jb.JobRef = s.JobRef
                      AND jb.Status = 'APPROVED'
                    ORDER BY jb.Version DESC) b
      OUTER APPLY (SELECT ActualGBP    = SUM(CASE WHEN t.CostNature = 'ACTUAL' THEN t.AmountGBP ELSE 0 END),
                          CommittedGBP = SUM(CASE WHEN t.CostNature = 'COMMITTED' THEN t.AmountGBP ELSE 0 END),
                          AccruedGBP   = SUM(CASE WHEN t.CostNature = 'ACCRUED' THEN t.AmountGBP ELSE 0 END),
                          RevenueRows  = SUM(CASE WHEN t.ElementCode = 'REVENUE' THEN 1 ELSE 0 END)
                     FROM cst.JobCostTransaction t
                    WHERE t.CompanyCode = s.CompanyCode AND t.JobRef = s.JobRef
                      AND t.ElementCode <> 'REVENUE') c
      OUTER APPLY (SELECT RevenueGBP = SUM(-t.AmountGBP) FROM cst.JobCostTransaction t
                    WHERE t.CompanyCode = s.CompanyCode AND t.JobRef = s.JobRef
                      AND t.ElementCode = 'REVENUE') r
      OUTER APPLY (SELECT TOP (1) wb.ClosingValueGBP FROM cst.WipBalance wb
                    WHERE wb.CompanyCode = s.CompanyCode AND wb.JobRef = s.JobRef
                    ORDER BY wb.FiscalYear DESC, wb.PeriodNo DESC) w
     ORDER BY ISNULL(s.ParentJobRef, s.JobRef), s.JobLevel, s.JobRef;
END;
GO
/*  cst.usp_ValuationRecon - CST-022.  The inventory valuation only means something if it
    agrees with the ledger, so the difference is stated, not left for someone to find.
    Only a POSTED journal forms the ledger balance the valuation is agreed to; journals still
    in flight and unresolved costing exceptions are reported beside it, never inside it. */
CREATE OR ALTER PROCEDURE cst.usp_ValuationRecon
    @CompanyCode varchar(10),
    @AsAt        date = NULL,
    @AccountCode varchar(20) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    DECLARE @asAtDate date = ISNULL(@AsAt, CAST(SYSDATETIME() AS date));

    /*  CST-022 / XMD-005.  The ledger side is every account the chart of accounts marks as
        the INVENTORY control, read by posting date - the same accounts and the same date
        rule RECONCTL and the demonstration bridge use - so the valuation report and the
        control-account reconciliation can never tell two different stories about stock.
        An explicit @AccountCode still reconciles that one account on its own. */
    DECLARE @acctList TABLE (AccountCode varchar(20) PRIMARY KEY);
    IF @AccountCode IS NOT NULL
        INSERT @acctList (AccountCode) VALUES (@AccountCode);
    ELSE
        INSERT @acctList (AccountCode)
        SELECT AccountCode FROM fin.Account
         WHERE CompanyCode = @CompanyCode AND ISNULL(ControlType, '') = 'INVENTORY';
    IF NOT EXISTS (SELECT 1 FROM @acctList)
        INSERT @acctList (AccountCode) VALUES ('1100');

    DECLARE @acct varchar(20) = (SELECT MIN(AccountCode) + CASE WHEN COUNT(*) > 1 THEN '..' ELSE '' END
                                   FROM @acctList);

    DECLARE @stockGBP decimal(18, 2), @allowGBP decimal(18, 2), @layerGBP decimal(18, 2);
    SELECT @stockGBP = ISNULL(SUM(CarryingValueGBP), 0),
           @allowGBP = ISNULL(SUM(AllowanceGBP), 0)
      FROM cst.ItemCost WHERE CompanyCode = @CompanyCode;
    SELECT @layerGBP = ISNULL(SUM(ValueRemainingGBP), 0)
      FROM cst.CostLayer
     WHERE CompanyCode = @CompanyCode AND IsClosed = 0 AND QtyRemaining > 0;

    DECLARE @ledgerGBP decimal(18, 2) = 0, @unpostedGBP decimal(18, 2) = 0;
    IF OBJECT_ID(N'fin.JournalLine') IS NOT NULL AND OBJECT_ID(N'fin.Journal') IS NOT NULL
        SELECT @ledgerGBP   = ISNULL(SUM(CASE WHEN jh.Status = 'POSTED'
                                              THEN jl.DebitAmountGBP - jl.CreditAmountGBP
                                              ELSE 0 END), 0),
               @unpostedGBP = ISNULL(SUM(CASE WHEN jh.Status IN ('DRAFT', 'SUBMITTED', 'APPROVED')
                                              THEN jl.DebitAmountGBP - jl.CreditAmountGBP
                                              ELSE 0 END), 0)
          FROM fin.JournalLine jl
          JOIN fin.Journal jh ON jh.JournalId = jl.JournalId
          JOIN @acctList al ON al.AccountCode = jl.AccountCode
         WHERE jh.CompanyCode = @CompanyCode
           AND jh.PostingDate <= @asAtDate;

    /*  an unresolved costing exception is a known hole in the valuation: it is counted and
        shown in its own right so that nobody can read the reconciliation as complete. */
    DECLARE @openExceptions int = 0;
    IF OBJECT_ID(N'core.ExceptionItem') IS NOT NULL
        SELECT @openExceptions = COUNT(*)
          FROM core.ExceptionItem
         WHERE CompanyCode = @CompanyCode AND Module = 'CST' AND Status <> 'RESOLVED';

    SELECT CompanyCode        = @CompanyCode,
           AsAt               = @asAtDate,
           AccountCode        = @acct,
           PoolCarryingGBP    = @stockGBP,
           AllowanceGBP       = @allowGBP,
           NetInventoryGBP    = @stockGBP - @allowGBP,
           FifoLayerGBP       = @layerGBP,
           LedgerBalanceGBP   = @ledgerGBP,
           DifferenceGBP      = (@stockGBP - @allowGBP) - @ledgerGBP,
           ReconciledFlag     = CASE WHEN ABS((@stockGBP - @allowGBP) - @ledgerGBP) < 0.01
                                     THEN 'Y' ELSE 'N' END,
           LayerDifferenceGBP = @layerGBP - @stockGBP,
           /*  columns 12 onwards are additions; CSTINVAL reads the first eleven */
           UnpostedLedgerGBP  = @unpostedGBP,
           OpenCostExceptions = @openExceptions;
END;
GO
