/*  fin.usp_BudgetCompare - actuals against a named budget version.  AC-012: an approved budget
    of 1,000.00, a revised forecast of 1,100.00 and actual expense of 1,050.00 give a variance
    of 50.00 adverse to budget and 50.00 favourable to forecast.  Revising a forecast creates a
    new version; it never overwrites the approved baseline (FIN-009).
    REQ: FIN-009, FIN-023 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE fin.usp_BudgetCompare @CompanyCode varchar(10), @FiscalYear smallint, @BudgetCode varchar(20), @Version smallint = NULL,
    @PeriodFrom tinyint = 1, @PeriodTo tinyint = 12, @DimCostCentre varchar(20) = NULL, @CompareCode varchar(20) = NULL, @CompareVersion smallint = NULL
AS
BEGIN
    SET NOCOUNT ON;
    IF @Version IS NULL SELECT @Version = MAX(Version) FROM fin.Budget WHERE CompanyCode = @CompanyCode AND BudgetCode = @BudgetCode AND FiscalYear = @FiscalYear;
    IF @CompareCode IS NOT NULL AND @CompareVersion IS NULL
        SELECT @CompareVersion = MAX(Version) FROM fin.Budget WHERE CompanyCode = @CompanyCode AND BudgetCode = @CompareCode AND FiscalYear = @FiscalYear;
    ;WITH bud AS (
        SELECT b.AccountCode, b.DimCostCentre, SUM(b.Amount) AS BudgetGBP
          FROM fin.BudgetLine b
         WHERE b.CompanyCode = @CompanyCode AND b.BudgetCode = @BudgetCode AND b.FiscalYear = @FiscalYear AND b.Version = @Version
           AND b.PeriodNo BETWEEN @PeriodFrom AND @PeriodTo AND (@DimCostCentre IS NULL OR b.DimCostCentre = @DimCostCentre)
         GROUP BY b.AccountCode, b.DimCostCentre),
    cmp AS (
        SELECT b.AccountCode, b.DimCostCentre, SUM(b.Amount) AS CompareGBP
          FROM fin.BudgetLine b
         WHERE @CompareCode IS NOT NULL AND b.CompanyCode = @CompanyCode AND b.BudgetCode = @CompareCode AND b.FiscalYear = @FiscalYear
           AND b.Version = @CompareVersion AND b.PeriodNo BETWEEN @PeriodFrom AND @PeriodTo AND (@DimCostCentre IS NULL OR b.DimCostCentre = @DimCostCentre)
         GROUP BY b.AccountCode, b.DimCostCentre),
    act AS (
        SELECT l.AccountCode, l.DimCostCentre, SUM(l.DebitAmountGBP) - SUM(l.CreditAmountGBP) AS ActualGBP
          FROM fin.JournalLine l JOIN fin.Journal j ON j.JournalId = l.JournalId AND j.Status = 'POSTED'
         WHERE l.CompanyCode = @CompanyCode AND j.FiscalYear = @FiscalYear AND j.PeriodNo BETWEEN @PeriodFrom AND @PeriodTo
           AND (@DimCostCentre IS NULL OR l.DimCostCentre = @DimCostCentre)
         GROUP BY l.AccountCode, l.DimCostCentre),
    k AS (SELECT AccountCode, DimCostCentre FROM bud UNION SELECT AccountCode, DimCostCentre FROM act UNION SELECT AccountCode, DimCostCentre FROM cmp)
    SELECT k.AccountCode, a.Name AS AccountName, c.Statement, k.DimCostCentre, @BudgetCode AS BudgetCode, @Version AS BudgetVersion,
           ISNULL(bud.BudgetGBP, 0) AS BudgetGBP, ISNULL(act.ActualGBP, 0) AS ActualGBP,
           ISNULL(act.ActualGBP, 0) - ISNULL(bud.BudgetGBP, 0) AS VarianceGBP,
           CASE WHEN c.Statement = 'PL' AND c.ClassType = 'EXPENSE'
                THEN CASE WHEN ISNULL(act.ActualGBP, 0) > ISNULL(bud.BudgetGBP, 0) THEN 'ADVERSE' WHEN ISNULL(act.ActualGBP, 0) < ISNULL(bud.BudgetGBP, 0) THEN 'FAVOURABLE' ELSE 'ON BUDGET' END
                ELSE CASE WHEN ISNULL(act.ActualGBP, 0) < ISNULL(bud.BudgetGBP, 0) THEN 'ADVERSE' WHEN ISNULL(act.ActualGBP, 0) > ISNULL(bud.BudgetGBP, 0) THEN 'FAVOURABLE' ELSE 'ON BUDGET' END END AS VarianceSense,
           @CompareCode AS CompareCode, @CompareVersion AS CompareVersion, ISNULL(cmp.CompareGBP, 0) AS CompareGBP,
           ISNULL(act.ActualGBP, 0) - ISNULL(cmp.CompareGBP, 0) AS CompareVarianceGBP
      FROM k
      JOIN fin.Account a ON a.CompanyCode = @CompanyCode AND a.AccountCode = k.AccountCode
      JOIN fin.AccountClass c ON c.ClassCode = a.ClassCode
      LEFT JOIN bud ON bud.AccountCode = k.AccountCode AND ISNULL(bud.DimCostCentre, '') = ISNULL(k.DimCostCentre, '')
      LEFT JOIN cmp ON cmp.AccountCode = k.AccountCode AND ISNULL(cmp.DimCostCentre, '') = ISNULL(k.DimCostCentre, '')
      LEFT JOIN act ON act.AccountCode = k.AccountCode AND ISNULL(act.DimCostCentre, '') = ISNULL(k.DimCostCentre, '')
     ORDER BY k.AccountCode, k.DimCostCentre;
END
GO
/*  fin.usp_ControlRecon - control account balance against its supporting subledger detail as at
    a common cutoff.  A difference is reported with its owner and contributing references; it is
    never squared off with an automatic balancing entry (FIN-021, AC-017).
    REQ: FIN-021 */
CREATE OR ALTER PROCEDURE fin.usp_ControlRecon @CompanyCode varchar(10), @AsOfDate date, @ControlType varchar(10) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    SELECT a.AccountCode, a.Name AS AccountName, a.ControlType, a.ControlSubledger,
           ISNULL(ctl.BalanceGBP, 0) AS ControlBalanceGBP,
           ISNULL(det.DetailGBP, 0) AS DetailBalanceGBP,
           ISNULL(ctl.BalanceGBP, 0) - ISNULL(det.DetailGBP, 0) AS DifferenceGBP,
           ISNULL(ctl.LineCount, 0) AS ControlLineCount, ISNULL(det.RefCount, 0) AS DetailRefCount,
           CASE WHEN ABS(ISNULL(ctl.BalanceGBP, 0) - ISNULL(det.DetailGBP, 0)) <= 0.005 THEN 'RECONCILED' ELSE 'DIFFERENCE' END AS ReconState,
           r.Status AS LastRunStatus, r.OwnerUser, r.Note AS LastNote, r.RunAt AS LastRunAt
      FROM fin.Account a
      OUTER APPLY (SELECT SUM(l.DebitAmountGBP) - SUM(l.CreditAmountGBP) AS BalanceGBP, COUNT_BIG(*) AS LineCount
                     FROM fin.JournalLine l JOIN fin.Journal j ON j.JournalId = l.JournalId AND j.Status = 'POSTED'
                    WHERE l.CompanyCode = a.CompanyCode AND l.AccountCode = a.AccountCode AND j.PostingDate <= @AsOfDate) ctl
      OUTER APPLY (SELECT SUM(l.DebitAmountGBP) - SUM(l.CreditAmountGBP) AS DetailGBP, COUNT(DISTINCT l.SubledgerRef) AS RefCount
                     FROM fin.JournalLine l JOIN fin.Journal j ON j.JournalId = l.JournalId AND j.Status = 'POSTED'
                    WHERE l.CompanyCode = a.CompanyCode AND l.AccountCode = a.AccountCode AND j.PostingDate <= @AsOfDate
                      AND l.SubledgerRef IS NOT NULL) det
      OUTER APPLY (SELECT TOP (1) x.Status, x.OwnerUser, x.Note, x.RunAt FROM rpt.ReconciliationRun x
                    WHERE x.CompanyCode = a.CompanyCode AND x.AccountCode = a.AccountCode ORDER BY x.RunAt DESC) r
     WHERE a.CompanyCode = @CompanyCode AND a.ControlType IS NOT NULL AND (@ControlType IS NULL OR a.ControlType = @ControlType)
     ORDER BY a.ControlType, a.AccountCode;
END
GO
