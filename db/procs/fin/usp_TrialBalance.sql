/*  fin.usp_TrialBalance - brought forward, period movement and carried forward per posting
    account for a company and a range of periods.  Totals reconcile to the posted journal lines
    because both sides come from fin.vw_AccountBalance.  Profit and loss accounts have no
    brought forward balance in the first period of a year: the prior year is closed to retained
    earnings by the year end run (FIN-012).
    REQ: FIN-021, FIN-023, FIN-030 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE fin.usp_TrialBalance @CompanyCode varchar(10), @FiscalYear smallint, @PeriodFrom tinyint = 1, @PeriodTo tinyint = 12,
    @IncludeZero bit = 0, @StatementFilter char(2) = NULL, @ManagementGroup varchar(30) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    SELECT a.CompanyCode, a.AccountCode, a.Name AS AccountName, a.ClassCode, c.ClassType, c.NormalBalance, c.Statement,
           a.StatutoryLineCode, s.Caption AS StatutoryCaption, a.ManagementGroup, a.ControlType, a.TaxAnalysisFlag,
           ISNULL(bf.MovementGBP, 0) AS BroughtForwardGBP,
           ISNULL(mv.DebitGBP, 0) AS PeriodDebitGBP, ISNULL(mv.CreditGBP, 0) AS PeriodCreditGBP,
           ISNULL(bf.MovementGBP, 0) + ISNULL(mv.MovementGBP, 0) AS ClosingGBP,
           CASE WHEN ISNULL(bf.MovementGBP, 0) + ISNULL(mv.MovementGBP, 0) > 0 THEN ISNULL(bf.MovementGBP, 0) + ISNULL(mv.MovementGBP, 0) ELSE 0 END AS ClosingDebitGBP,
           CASE WHEN ISNULL(bf.MovementGBP, 0) + ISNULL(mv.MovementGBP, 0) < 0 THEN -(ISNULL(bf.MovementGBP, 0) + ISNULL(mv.MovementGBP, 0)) ELSE 0 END AS ClosingCreditGBP,
           ISNULL(mv.LineCount, 0) AS LineCount, c.SortOrder AS ClassSort
      FROM fin.Account a
      JOIN fin.AccountClass c ON c.ClassCode = a.ClassCode
      LEFT JOIN fin.StatutoryLine s ON s.StatutoryLineCode = a.StatutoryLineCode
           AND s.Framework = (SELECT AccountingFramework FROM core.Company WHERE CompanyCode = @CompanyCode)
      OUTER APPLY (SELECT SUM(v.MovementGBP) AS MovementGBP FROM fin.vw_AccountBalance v
                    WHERE v.CompanyCode = a.CompanyCode AND v.AccountCode = a.AccountCode
                      AND (v.FiscalYear < @FiscalYear OR (v.FiscalYear = @FiscalYear AND v.PeriodNo < @PeriodFrom))
                      AND (c.Statement = 'BS' OR v.FiscalYear = @FiscalYear)) bf
      OUTER APPLY (SELECT SUM(v.DebitGBP) AS DebitGBP, SUM(v.CreditGBP) AS CreditGBP, SUM(v.MovementGBP) AS MovementGBP, SUM(v.LineCount) AS LineCount
                     FROM fin.vw_AccountBalance v
                    WHERE v.CompanyCode = a.CompanyCode AND v.AccountCode = a.AccountCode
                      AND v.FiscalYear = @FiscalYear AND v.PeriodNo BETWEEN @PeriodFrom AND @PeriodTo) mv
     WHERE a.CompanyCode = @CompanyCode AND a.AccountKind = 'POSTING'
       AND (@StatementFilter IS NULL OR c.Statement = @StatementFilter)
       AND (@ManagementGroup IS NULL OR a.ManagementGroup = @ManagementGroup)
       AND (@IncludeZero = 1 OR ISNULL(bf.MovementGBP, 0) <> 0 OR ISNULL(mv.MovementGBP, 0) <> 0 OR ISNULL(mv.DebitGBP, 0) <> 0)
     ORDER BY c.SortOrder, a.AccountCode;
END
GO
/*  fin.usp_AccountActivity - drill-through from a statement line to the posted journal lines
    behind it, newest first.  REQ: FIN-023, FIN-002 */
CREATE OR ALTER PROCEDURE fin.usp_AccountActivity @CompanyCode varchar(10), @AccountCode varchar(12) = NULL, @FiscalYear smallint = NULL,
    @PeriodFrom tinyint = NULL, @PeriodTo tinyint = NULL, @DimCostCentre varchar(20) = NULL, @DimProject varchar(20) = NULL, @MaxRows int = 500
AS
BEGIN
    SET NOCOUNT ON;
    SELECT TOP (@MaxRows) j.JournalNumber, j.JournalType, j.SourceModule, j.SourceRef, j.EventCode, j.PostingDate, j.DocumentDate,
           j.FiscalYear, j.PeriodNo, l.LineNum, l.AccountCode, a.Name AS AccountName, l.DebitAmountGBP, l.CreditAmountGBP,
           l.CurrencyCode, l.DebitAmount, l.CreditAmount, l.DimCostCentre, l.DimProject, l.Narrative, l.VatCode,
           l.SubledgerType, l.SubledgerRef, j.Description, j.PostedBy, j.PostedAt, j.EvidenceRef, j.IsReversal, j.ReversalOfJournalId
      FROM fin.JournalLine l
      JOIN fin.Journal j ON j.JournalId = l.JournalId AND j.Status = 'POSTED'
      JOIN fin.Account a ON a.CompanyCode = l.CompanyCode AND a.AccountCode = l.AccountCode
     WHERE l.CompanyCode = @CompanyCode
       AND (@AccountCode IS NULL OR l.AccountCode = @AccountCode)
       AND (@FiscalYear IS NULL OR j.FiscalYear = @FiscalYear)
       AND (@PeriodFrom IS NULL OR j.PeriodNo >= @PeriodFrom)
       AND (@PeriodTo IS NULL OR j.PeriodNo <= @PeriodTo)
       AND (@DimCostCentre IS NULL OR l.DimCostCentre = @DimCostCentre)
       AND (@DimProject IS NULL OR l.DimProject = @DimProject)
     ORDER BY j.PostingDate DESC, j.JournalNumber DESC, l.LineNum;
END
GO
