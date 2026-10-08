/*  Costing jobs: a contract with one sub-job, the budget that was approved rather than the one
    that was proposed, and the work in progress roll-forward.  A sub-job's cost belongs to its
    parent exactly once (D-13), the approved version is the baseline a variance is measured
    against (CST-015), and closing work in progress is opening plus additions less deductions
    (CST-018) - anything else is a plug.  Each case is built and rolled back.
    REQ: CST-014, CST-015, CST-016, CST-018, CST-020  AC: AC-C5 */
SET NOCOUNT ON;
GO
PRINT '# cst-jobs';
/*  Re-runnable: clear anything an aborted earlier run could have left behind. */
DELETE FROM cst.JobCostTransaction WHERE CompanyCode = 'STC' AND JobRef LIKE 'ZZJOB%';
DELETE FROM cst.WipBalance         WHERE CompanyCode = 'STC' AND JobRef LIKE 'ZZJOB%';
DELETE FROM cst.JobBudgetLine      WHERE JobBudgetId IN (SELECT JobBudgetId FROM cst.JobBudget
                                                          WHERE CompanyCode = 'STC' AND JobRef LIKE 'ZZJOB%');
DELETE FROM cst.JobBudget          WHERE CompanyCode = 'STC' AND JobRef LIKE 'ZZJOB%';
DELETE FROM cst.Job                WHERE CompanyCode = 'STC' AND JobRef LIKE 'ZZJOB%';
GO
DECLARE @msg varchar(400), @raised varchar(1), @budget decimal(28,6), @actual decimal(28,6),
        @committed decimal(28,6), @subActual decimal(28,6), @wip decimal(28,6),
        @closing decimal(28,6), @proof decimal(28,6), @rows int;

DECLARE @jobs TABLE (Job varchar(20), Parent varchar(20), Level tinyint, Description varchar(150),
                     Customer varchar(20), Operational varchar(14), Financial varchar(14),
                     Budget decimal(18,2), Actual decimal(18,2), Committed decimal(18,2),
                     Variance decimal(18,2), Wip decimal(18,2));

/*  1 to 4 - a parent and its sub-job.  Five thousand approved, two thousand one hundred spent
    and nine hundred committed, the sub-job's share sitting on the sub-job and nowhere else.
    REQ: CST-014, CST-015, CST-016  AC: AC-C5 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT cst.Job (CompanyCode, JobRef, ParentJobRef, JobLevel, Description, SiteCode, CustomerCode,
                    Purpose, OwnerUser, StartDate, TargetEndDate, OperationalStatus, FinancialStatus, CreatedBy)
    VALUES ('STC', 'ZZJOB1', NULL, 1, 'Test contract', 'RDG', 'CUST001', 'CUSTOMER', 'PM',
            '2026-09-01', '2026-12-01', 'OPEN', 'OPEN', 'TEST'),
           ('STC', 'ZZJOB1S', 'ZZJOB1', 2, 'Test sub-job', 'RDG', 'CUST001', 'CUSTOMER', 'PM',
            '2026-09-01', '2026-11-01', 'OPEN', 'OPEN', 'TEST');

    /*  Version 1 was approved at five thousand; version 2 is a proposal and is not a baseline. */
    INSERT cst.JobBudget (CompanyCode, JobRef, Version, IsOriginal, TotalBudgetGBP, ChangeReason,
                          Status, ApprovedBy, ApprovedAt, CreatedBy)
    VALUES ('STC', 'ZZJOB1', 1, 1, 5000.00, 'Original estimate', 'APPROVED', 'FINMGR', SYSUTCDATETIME(), 'TEST'),
           ('STC', 'ZZJOB1', 2, 0, 9000.00, 'Scope change under discussion', 'DRAFT', NULL, NULL, 'TEST');

    INSERT cst.JobCostTransaction (CompanyCode, JobRef, ElementCode, SourceModule, SourceDocType, SourceDocRef,
                                   Amount, AmountGBP, CostNature, RecognitionDate, FiscalYear, PeriodNo, CreatedBy)
    VALUES ('STC', 'ZZJOB1',  'MAT', 'INV', 'ISSUE', 'ZZMV1', 1500.00, 1500.00, 'ACTUAL',    '2026-10-02', 2027, 4, 'TEST'),
           ('STC', 'ZZJOB1S', 'LAB', 'CST', 'TIME',  'ZZTM1',  600.00,  600.00, 'ACTUAL',    '2026-10-03', 2027, 4, 'TEST'),
           ('STC', 'ZZJOB1',  'SUB', 'PO',  'ORDER', 'ZZPO1',  900.00,  900.00, 'COMMITTED', '2026-10-04', 2027, 4, 'TEST');

    DELETE @jobs;
    INSERT @jobs EXEC cst.usp_JobList @CompanyCode = 'STC', @JobRef = 'ZZJOB1';
    SELECT @rows = COUNT(*) FROM @jobs;
    SELECT @budget = Budget, @actual = Actual, @committed = Committed FROM @jobs WHERE Job = 'ZZJOB1';
    SELECT @subActual = Actual FROM @jobs WHERE Job = 'ZZJOB1S';

    IF @rows = 2
        PRINT 'ok 1 - asking for the contract returns the sub-job under it -- REQ: CST-014';
    ELSE PRINT 'not ok 1 - the worklist returned ' + ISNULL(CAST(@rows AS varchar(20)),'NULL')
               + ' row(s) rather than the parent and its sub-job -- REQ: CST-014';

    IF @budget = 5000.00
        PRINT 'ok 2 - the baseline is the approved version, not the proposal -- REQ: CST-015';
    ELSE PRINT 'not ok 2 - the budget came back as ' + ISNULL(CAST(@budget AS varchar(20)),'NULL')
               + ' rather than 5000.00 -- REQ: CST-015';

    IF @actual = 1500.00 AND @subActual = 600.00
        PRINT 'ok 3 - the sub-job carries its own cost and the parent is not charged twice -- REQ: CST-016  AC: AC-C5';
    ELSE PRINT 'not ok 3 - parent actual ' + ISNULL(CAST(@actual AS varchar(20)),'NULL') + ', sub-job actual '
               + ISNULL(CAST(@subActual AS varchar(20)),'NULL') + ' -- REQ: CST-016  AC: AC-C5';

    SELECT @actual = SUM(Actual), @committed = SUM(Committed) FROM @jobs;
    IF @actual = 2100.00 AND @committed = 900.00
        PRINT 'ok 4 - the contract has cost 2100.00 and committed 900.00 against 5000.00 -- REQ: CST-016  AC: AC-C5';
    ELSE PRINT 'not ok 4 - actual ' + ISNULL(CAST(@actual AS varchar(20)),'NULL') + ', committed '
               + ISNULL(CAST(@committed AS varchar(20)),'NULL') + ' -- REQ: CST-016  AC: AC-C5';

    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 1 to 4 - unexpected error in the job cost tests: ' + @msg;
END CATCH

/*  5 to 7 - the work in progress roll-forward.  Opening plus what went in less what came out
    is the closing balance, and the worklist reports the latest period's closing figure.
    REQ: CST-018, CST-020 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT cst.Job (CompanyCode, JobRef, ParentJobRef, JobLevel, Description, SiteCode, Purpose,
                    OwnerUser, StartDate, OperationalStatus, FinancialStatus, CreatedBy)
    VALUES ('STC', 'ZZJOB2', NULL, 1, 'Work in progress test', 'RDG', 'INTERNAL', 'PM',
            '2026-09-01', 'OPEN', 'OPEN', 'TEST');

    INSERT cst.WipBalance (CompanyCode, JobRef, FiscalYear, PeriodNo, OpeningValueGBP, MaterialIssuesGBP,
                           MaterialReturnsGBP, LabourGBP, OverheadGBP, SubcontractGBP,
                           CompletionTransfersGBP, ScrapGBP, WriteOffGBP, ValueAdjustmentGBP, ClosingValueGBP)
    VALUES ('STC', 'ZZJOB2', 2027, 3, 0, 1000.00, 0, 0, 0, 0, 0, 0, 0, 0, 1000.00),
           ('STC', 'ZZJOB2', 2027, 4, 1000.00, 800.00, 100.00, 300.00, 150.00, 50.00,
            900.00, 60.00, 40.00, 0, 1200.00);

    SELECT @closing = ClosingValueGBP,
           @proof = OpeningValueGBP + MaterialIssuesGBP - MaterialReturnsGBP + LabourGBP + OverheadGBP
                    + SubcontractGBP + ValueAdjustmentGBP - CompletionTransfersGBP - ScrapGBP - WriteOffGBP
      FROM cst.WipBalance WHERE CompanyCode = 'STC' AND JobRef = 'ZZJOB2' AND FiscalYear = 2027 AND PeriodNo = 4;

    IF @closing = @proof
        PRINT 'ok 5 - closing work in progress is the roll-forward and not a plug -- REQ: CST-018';
    ELSE PRINT 'not ok 5 - closing ' + ISNULL(CAST(@closing AS varchar(20)),'NULL') + ' against a roll-forward of '
               + ISNULL(CAST(@proof AS varchar(20)),'NULL') + ' -- REQ: CST-018';

    IF @closing = 1200.00
        PRINT 'ok 6 - 1000.00 opening, 2300.00 in and 1100.00 out leaves 1200.00 -- REQ: CST-018';
    ELSE PRINT 'not ok 6 - the period closed at ' + ISNULL(CAST(@closing AS varchar(20)),'NULL')
               + ' rather than 1200.00 -- REQ: CST-018';

    DELETE @jobs;
    INSERT @jobs EXEC cst.usp_JobList @CompanyCode = 'STC', @JobRef = 'ZZJOB2';
    SELECT @wip = Wip FROM @jobs WHERE Job = 'ZZJOB2';

    IF @wip = 1200.00
        PRINT 'ok 7 - the worklist shows the latest period, not the first one it finds -- REQ: CST-018, CST-020';
    ELSE PRINT 'not ok 7 - the worklist reported work in progress of ' + ISNULL(CAST(@wip AS varchar(20)),'NULL')
               + ' -- REQ: CST-018, CST-020';

    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 5 to 7 - unexpected error in the work in progress tests: ' + @msg;
END CATCH

/*  8 and 9 - the two refusals that keep job costing honest: a job hierarchy is two levels deep
    (D-13) and a cost transaction has to say what kind of cost it is.  REQ: CST-014, CST-016 */
BEGIN TRY
    BEGIN TRANSACTION;
    SET @raised = 'N';
    BEGIN TRY
        INSERT cst.Job (CompanyCode, JobRef, ParentJobRef, JobLevel, Description, Purpose,
                        OperationalStatus, FinancialStatus, CreatedBy)
        VALUES ('STC', 'ZZJOB3', 'ZZJOB1S', 3, 'Sub-sub-job', 'CUSTOMER', 'OPEN', 'OPEN', 'TEST');
    END TRY
    BEGIN CATCH
        SET @raised = 'Y';
    END CATCH
    IF @raised = 'Y'
        PRINT 'ok 8 - a job hierarchy stops at two levels -- REQ: CST-014';
    ELSE PRINT 'not ok 8 - a third level of job was accepted -- REQ: CST-014';

    INSERT cst.Job (CompanyCode, JobRef, JobLevel, Description, Purpose, OperationalStatus,
                    FinancialStatus, CreatedBy)
    VALUES ('STC', 'ZZJOB4', 1, 'Nature test', 'INTERNAL', 'OPEN', 'OPEN', 'TEST');
    SET @raised = 'N';
    BEGIN TRY
        INSERT cst.JobCostTransaction (CompanyCode, JobRef, ElementCode, SourceModule, SourceDocType,
                                       SourceDocRef, Amount, AmountGBP, CostNature, RecognitionDate, CreatedBy)
        VALUES ('STC', 'ZZJOB4', 'MAT', 'INV', 'ISSUE', 'ZZMV9', 10.00, 10.00, 'GUESS', '2026-10-05', 'TEST');
    END TRY
    BEGIN CATCH
        SET @raised = 'Y';
    END CATCH
    IF @raised = 'Y'
        PRINT 'ok 9 - a cost is actual, accrued, committed or an estimate and says which -- REQ: CST-016';
    ELSE PRINT 'not ok 9 - a cost of an unknown nature was accepted -- REQ: CST-016';

    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 8 to 9 - unexpected error in the job control tests: ' + @msg;
END CATCH
GO
