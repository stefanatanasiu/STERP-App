/*  Financial reporting and the statutory profile.  The trial balance has to agree with the
    journals it came from, name the company and the period it was taken for, carry the statutory
    format line every posting account sits on, and drill through to the entries that make up a
    balance.  The statutory profile has to hold the framework, the accounting reference date and
    a period of account that is never longer than eighteen months.
    Every assertion runs inside a transaction that is rolled back.
    REQ: FIN-023, FIN-030   AC: AC-119 */
SET NOCOUNT ON;
GO
PRINT '# fin-gaps';
DECLARE @co varchar(10) = 'STC', @fy smallint, @msg varchar(400), @n int, @m int;
DECLARE @dr decimal(19,2), @cr decimal(19,2), @tb decimal(19,2), @jl decimal(19,2);

SELECT TOP (1) @fy = FiscalYear FROM fin.Period
 WHERE CompanyCode = @co AND Status IN ('OPEN', 'SOFT', 'HARD') ORDER BY FiscalYear DESC;

/*  1 - the trial balance balances: what it says was debited was also credited. */
BEGIN TRY
    BEGIN TRANSACTION;
    CREATE TABLE #tb (CompanyCode varchar(10), AccountCode varchar(12), AccountName varchar(80),
        ClassCode varchar(10), ClassType varchar(10), NormalBalance char(2), Statement char(2),
        StatutoryLineCode varchar(20), StatutoryCaption varchar(100), ManagementGroup varchar(30),
        ControlType varchar(10), TaxAnalysisFlag varchar(20), BroughtForwardGBP decimal(19,2),
        PeriodDebitGBP decimal(19,2), PeriodCreditGBP decimal(19,2), ClosingGBP decimal(19,2),
        ClosingDebitGBP decimal(19,2), ClosingCreditGBP decimal(19,2), LineCount int, ClassSort int);
    INSERT #tb EXEC fin.usp_TrialBalance @CompanyCode = @co, @FiscalYear = @fy,
        @PeriodFrom = 1, @PeriodTo = 12, @IncludeZero = 1;
    SELECT @dr = ISNULL(SUM(ClosingDebitGBP), 0), @cr = ISNULL(SUM(ClosingCreditGBP), 0) FROM #tb;
    IF @dr = @cr PRINT 'ok 1 - the trial balance balances at ' + CAST(@dr AS varchar(30))
        + ' -- REQ: FIN-023';
    ELSE PRINT 'not ok 1 - debits ' + CAST(@dr AS varchar(30)) + ' do not equal credits '
        + CAST(@cr AS varchar(30));

    /*  2 - and it reconciles to the posted entries rather than to a parallel total. */
    SELECT @tb = ISNULL(SUM(PeriodDebitGBP), 0) FROM #tb;
    SELECT @jl = ISNULL(SUM(l.DebitAmountGBP), 0)
      FROM fin.JournalLine l JOIN fin.Journal j ON j.JournalId = l.JournalId
     WHERE j.CompanyCode = @co AND j.FiscalYear = @fy AND j.Status = 'POSTED';
    IF @tb = @jl PRINT 'ok 2 - the period debits reconcile to the posted journal lines ('
        + CAST(@tb AS varchar(30)) + ') -- REQ: FIN-023';
    ELSE PRINT 'not ok 2 - the trial balance reports ' + CAST(@tb AS varchar(30))
        + ' of debits but the journals hold ' + CAST(@jl AS varchar(30));

    /*  3 - every line identifies the company, the account, its statement and its statutory line. */
    SELECT @n = COUNT(*) FROM #tb
     WHERE CompanyCode IS NULL OR AccountCode IS NULL OR Statement IS NULL
        OR StatutoryLineCode IS NULL OR StatutoryCaption IS NULL;
    IF @n = 0 AND EXISTS (SELECT 1 FROM #tb)
        PRINT 'ok 3 - every trial balance line names its company, statement and statutory caption'
            + ' -- REQ: FIN-023, FIN-030';
    ELSE PRINT 'not ok 3 - ' + CAST(ISNULL(@n, -1) AS varchar(10))
        + ' trial balance lines are not fully identified';
    DROP TABLE #tb;
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 1 - unexpected error: ' + @msg;
    PRINT 'not ok 2 - trial balance not produced';
    PRINT 'not ok 3 - trial balance not produced';
END CATCH

/*  4 - every posting account maps to exactly one statutory format line of its own framework.
    Without the catalogue behind it the statutory statement is a list of unmapped accounts. */
SELECT @n = COUNT(*)
  FROM fin.Account a
  JOIN core.Company c ON c.CompanyCode = a.CompanyCode
 WHERE a.AccountKind = 'POSTING' AND a.Status = 'ACTIVE'
   AND (SELECT COUNT(*) FROM fin.StatutoryLine s
         WHERE s.Framework = c.AccountingFramework
           AND s.StatutoryLineCode = a.StatutoryLineCode) <> 1;
IF @n = 0 PRINT 'ok 4 - every posting account maps to exactly one statutory format line'
    + ' -- REQ: FIN-023, FIN-030';
ELSE PRINT 'not ok 4 - ' + CAST(@n AS varchar(10))
    + ' posting accounts map to no statutory line, or to more than one';

/*  5 - the statutory line catalogue itself is unambiguous: one caption per framework and code. */
SELECT @n = COUNT(*) FROM (SELECT Framework, StatutoryLineCode FROM fin.StatutoryLine
     GROUP BY Framework, StatutoryLineCode HAVING COUNT(*) > 1) d;
IF @n = 0 AND EXISTS (SELECT 1 FROM fin.StatutoryLine)
    PRINT 'ok 5 - the statutory format is held once per framework and line -- REQ: FIN-023';
ELSE PRINT 'not ok 5 - the statutory format catalogue is empty or holds duplicates';

/*  6 - a balance drills through to the entries that made it. */
BEGIN TRY
    BEGIN TRANSACTION;
    DECLARE @acct varchar(12);
    SELECT TOP (1) @acct = l.AccountCode, @jl = SUM(l.DebitAmountGBP - l.CreditAmountGBP)
      FROM fin.JournalLine l JOIN fin.Journal j ON j.JournalId = l.JournalId
     WHERE j.CompanyCode = @co AND j.FiscalYear = @fy AND j.Status = 'POSTED'
     GROUP BY l.AccountCode HAVING SUM(l.DebitAmountGBP - l.CreditAmountGBP) <> 0
     ORDER BY l.AccountCode;
    IF @acct IS NULL PRINT 'ok 6 - no posted movement to drill into # SKIP';
    ELSE
    BEGIN
        CREATE TABLE #act (JournalNumber varchar(30), JournalType varchar(12), SourceModule varchar(6),
            SourceRef varchar(40), EventCode varchar(30), PostingDate date, DocumentDate date,
            FiscalYear smallint, PeriodNo tinyint, LineNum int, AccountCode varchar(12),
            AccountName varchar(80), DebitAmountGBP decimal(19,2), CreditAmountGBP decimal(19,2),
            CurrencyCode char(3), DebitAmount decimal(19,2), CreditAmount decimal(19,2),
            DimCostCentre varchar(20), DimProject varchar(20), Narrative varchar(200),
            VatCode varchar(4), SubledgerType varchar(6), SubledgerRef varchar(40),
            Description varchar(200), PostedBy varchar(20), PostedAt datetime2(3),
            EvidenceRef varchar(200), IsReversal bit, ReversalOfJournalId bigint);
        INSERT #act EXEC fin.usp_AccountActivity @CompanyCode = @co, @AccountCode = @acct,
            @FiscalYear = @fy, @MaxRows = 5000;
        SELECT @tb = ISNULL(SUM(DebitAmountGBP - CreditAmountGBP), 0) FROM #act;
        IF @tb = @jl PRINT 'ok 6 - account ' + @acct + ' drills through to the entries behind it ('
            + CAST(@jl AS varchar(30)) + ') -- REQ: FIN-023';
        ELSE PRINT 'not ok 6 - the drill-through totals ' + CAST(@tb AS varchar(30))
            + ' but the ledger holds ' + CAST(@jl AS varchar(30));
        DROP TABLE #act;
    END
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 6 - unexpected error: ' + @msg;
END CATCH

/*  7 - every company records its accounting framework and its accounting reference date. */
SELECT @n = COUNT(*) FROM core.Company
 WHERE AccountingFramework IS NULL OR AccountingRefDate IS NULL OR AccountingRefDate = '';
IF @n = 0 PRINT 'ok 7 - every company holds a framework and an accounting reference date'
    + ' -- REQ: FIN-030';
ELSE PRINT 'not ok 7 - ' + CAST(@n AS varchar(10))
    + ' companies have no framework or no accounting reference date';

/*  8 - a period of account may be shortened or lengthened but never beyond eighteen months. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT fin.PeriodOfAccount (CompanyCode, StartDate, EndDate, LengthMonths, Framework,
        IsArdChange, Reason, Status)
    VALUES (@co, '2028-04-01', '2029-03-31', 12, 'FRS102', 1, 'Aligned with the new parent', 'OPEN');
    SELECT @n = COUNT(*) FROM fin.PeriodOfAccount
     WHERE CompanyCode = @co AND EndDate = '2029-03-31' AND LengthMonths = 12 AND IsArdChange = 1;
    SELECT @m = COUNT(*) FROM fin.PeriodOfAccount WHERE LengthMonths > 18;
    IF @n = 1 AND @m = 0
        PRINT 'ok 8 - a reference date change records an approved period of up to 18 months'
            + ' -- REQ: FIN-030, AC-119';
    ELSE PRINT 'not ok 8 - the period of account was not recorded, or ' + CAST(@m AS varchar(10))
        + ' periods exceed eighteen months';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 8 - unexpected error: ' + @msg;
END CATCH

/*  9 - the tax analysis flags used for the corporation tax computation are all accounts that
    appear in the trial balance, so the computation reconciles back to it. */
SELECT @n = COUNT(*) FROM fin.Account a
 WHERE a.TaxAnalysisFlag IS NOT NULL
   AND (a.AccountKind <> 'POSTING' OR a.StatutoryLineCode IS NULL);
IF @n = 0 PRINT 'ok 9 - every tax analysis flag sits on a mapped posting account -- REQ: FIN-030';
ELSE PRINT 'not ok 9 - ' + CAST(@n AS varchar(10))
    + ' tax analysis flags cannot be reconciled to the trial balance';

/*  10 - budget comparison states the variance as actual less budget, with a sense. */
BEGIN TRY
    BEGIN TRANSACTION;
    DECLARE @bc varchar(20);
    SELECT TOP (1) @bc = BudgetCode FROM fin.Budget WHERE CompanyCode = @co AND FiscalYear = @fy;
    IF @bc IS NULL PRINT 'ok 10 - no budget is seeded # SKIP';
    ELSE
    BEGIN
        CREATE TABLE #bc (AccountCode varchar(12), AccountName varchar(80), Statement char(2),
            DimCostCentre varchar(20), BudgetCode varchar(20), BudgetVersion smallint,
            BudgetGBP decimal(19,2), ActualGBP decimal(19,2), VarianceGBP decimal(19,2),
            VarianceSense varchar(12), CompareCode varchar(20), CompareVersion smallint,
            CompareGBP decimal(19,2), CompareVarianceGBP decimal(19,2));
        INSERT #bc EXEC fin.usp_BudgetCompare @CompanyCode = @co, @FiscalYear = @fy,
            @BudgetCode = @bc, @PeriodFrom = 1, @PeriodTo = 12;
        SELECT @n = COUNT(*) FROM #bc WHERE VarianceGBP <> ActualGBP - BudgetGBP;
        SELECT @m = COUNT(*) FROM #bc WHERE VarianceSense IS NULL;
        IF @n = 0 AND @m = 0
            PRINT 'ok 10 - every budget variance is actual less budget and carries its sense'
                + ' -- REQ: FIN-023';
        ELSE PRINT 'not ok 10 - ' + CAST(@n AS varchar(10)) + ' variances do not agree and '
            + CAST(@m AS varchar(10)) + ' carry no sense';
        DROP TABLE #bc;
    END
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 10 - unexpected error: ' + @msg;
END CATCH
GO
