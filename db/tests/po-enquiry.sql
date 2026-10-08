/*  Purchasing enquiry contract and arithmetic tests.  The commitment balance is the number the
    budget service, the buyer's enquiry and the month-end accrual all read, so its sign rules are
    pinned here: COMMITTED adds, RELEASED and CANCELLED take away, and ACCRUED / INVOICED are the
    same money already in the nominal ledger and are reported separately.  Every assertion runs
    inside a transaction that is rolled back, so the script is repeatable on a live database and
    needs no seed data of its own.
    REQ: PO-008, PO-017, PO-022, PO-024, D-08, D-09
    AC: an invoiced commitment is not counted twice */
SET NOCOUNT ON;
GO
PRINT '# po-enquiry';
DECLARE @msg varchar(400), @dec decimal(18,2), @n int, @co varchar(10) = 'STC',
        @ord varchar(30) = 'ZZ-TEST-CMT-1', @bud varchar(20) = 'ZZ-TEST-BUDGET',
        @d date = CAST(GETDATE() AS date);

/*  1 - the commitment enquiry refuses a grouping the specification does not define. */
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC po.usp_CommitmentEnquiry @CompanyCode = @co, @GroupBy = 'BUYER';
    PRINT 'not ok 1 - an unknown grouping was accepted -- REQ: PO-017';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0801%' PRINT 'ok 1 - the commitment enquiry only groups by ORDER, BUDGET or SUPPLIER -- REQ: PO-017';
    ELSE PRINT 'not ok 1 - unexpected error: ' + @msg;
END CATCH

/*  2 - COMMITTED adds, RELEASED and CANCELLED take away: 1,050.00 - 400.00 - 100.00 = 550.00. */
BEGIN TRANSACTION;
INSERT po.Commitment (CompanyCode, OrderNumber, LineNum, BudgetCode, AccountCode, FiscalYear,
                      Stage, MovementDate, Quantity, AmountGBP, NonRecoverableVatGBP, ChargeGBP, CreatedBy)
VALUES (@co, @ord, 1, @bud, '7100', 2026, 'COMMITTED', @d, 10, 1000.00, 0.00, 50.00, 'TEST'),
       (@co, @ord, 1, @bud, '7100', 2026, 'RELEASED',  @d,  4,  400.00, 0.00,  0.00, 'TEST'),
       (@co, @ord, 1, @bud, '7100', 2026, 'CANCELLED', @d,  1,  100.00, 0.00,  0.00, 'TEST'),
       (@co, @ord, 1, @bud, '7100', 2026, 'INVOICED',  @d,  4,  400.00, 0.00,  0.00, 'TEST'),
       (@co, @ord, 1, @bud, '7100', 2026, 'ACCRUED',   @d,  2,  200.00, 0.00,  0.00, 'TEST');
SELECT @dec = OpenCommitmentGBP FROM po.vw_CommitmentBalance
 WHERE CompanyCode = @co AND OrderNumber = @ord;
IF @dec = 550.00 PRINT 'ok 2 - the open commitment is 550.00 after release and cancellation -- REQ: PO-017, D-08';
ELSE PRINT 'not ok 2 - the open commitment was ' + ISNULL(CONVERT(varchar(20), @dec), 'NULL') + ', expected 550.00 -- REQ: PO-017';

/*  3 - invoiced and accrued money is reported beside the commitment, never inside it. */
SELECT @dec = InvoicedGBP, @n = CASE WHEN AccruedGBP = 200.00 THEN 1 ELSE 0 END
  FROM po.vw_CommitmentBalance WHERE CompanyCode = @co AND OrderNumber = @ord;
IF @dec = 400.00 AND @n = 1 PRINT 'ok 3 - invoiced 400.00 and accrued 200.00 are reported separately -- REQ: PO-017, PO-014';
ELSE PRINT 'not ok 3 - invoiced was ' + ISNULL(CONVERT(varchar(20), @dec), 'NULL') + ' -- REQ: PO-017';

/*  4 - the same movements grouped by budget reach the buyer's enquiry. */
CREATE TABLE #c (GroupKey varchar(60), BudgetCode varchar(20), CostCentre varchar(20),
                 AccountCode varchar(12), SupplierCode varchar(20), FiscalYear smallint,
                 OpenCommitmentGBP decimal(38,2), AccruedGBP decimal(38,2), InvoicedGBP decimal(38,2),
                 CommittedQty decimal(38,6), OrderCount int, LastMovementDate date);
BEGIN TRY
    INSERT #c EXEC po.usp_CommitmentEnquiry @CompanyCode = @co, @BudgetCode = @bud, @GroupBy = 'BUDGET';
    SELECT @dec = OpenCommitmentGBP FROM #c WHERE GroupKey = @bud;
    IF @dec = 550.00 PRINT 'ok 4 - the budget grouping reports the same 550.00 -- REQ: PO-017, PO-024';
    ELSE PRINT 'not ok 4 - the budget grouping reported ' + ISNULL(CONVERT(varchar(20), @dec), 'NULL') + ' -- REQ: PO-017';
END TRY
BEGIN CATCH
    PRINT 'not ok 4 - the commitment enquiry failed: ' + ERROR_MESSAGE() + ' -- REQ: PO-017';
END CATCH
DROP TABLE #c;
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  5 - the open order enquiry answers with the columns the buyer's screen and the invoice match
    depend on; a missing column is a broken contract even when there is no data today. */
SELECT @n = COUNT(*) FROM sys.dm_exec_describe_first_result_set(
        N'EXEC po.usp_OpenOrders @CompanyCode = ''STC''', NULL, 0)
 WHERE name IN ('OrderNumber', 'Status', 'OutstandingQty', 'OutstandingGBP',
                'ReceivedNotInvoicedGBP', 'CommitmentGBP', 'EarliestDueDate', 'IsCommitment');
IF @n = 8 PRINT 'ok 5 - the open order enquiry carries outstanding, GRNI and commitment -- REQ: PO-024, PO-014';
ELSE PRINT 'not ok 5 - only ' + CONVERT(varchar(10), @n) + ' of the 8 contract columns were found -- REQ: PO-024';

/*  6 - lateness is reported on both bases, so a re-promised date cannot hide a late delivery. */
SELECT @n = COUNT(*) FROM sys.dm_exec_describe_first_result_set(
        N'EXEC po.usp_OverdueSchedules @CompanyCode = ''STC''', NULL, 0)
 WHERE name IN ('DaysLateOriginalBasis', 'DaysLateCurrentBasis', 'PromisedDateOriginal',
                'PromisedDateCurrent', 'ChangeCount', 'ValueAtRiskGBP');
IF @n = 6 PRINT 'ok 6 - overdue schedules report both the original and the current promise -- REQ: PO-022, D-09';
ELSE PRINT 'not ok 6 - only ' + CONVERT(varchar(10), @n) + ' of the 6 lateness columns were found -- REQ: PO-022';

/*  7 - an accepted over delivery leaves no negative outstanding quantity behind. */
CREATE TABLE #o (OrderNumber varchar(30), OrderType varchar(12), SiteCode varchar(10),
                 SupplierCode varchar(20), BuyerUser varchar(20), OrderDate date, CurrencyCode char(3),
                 Status varchar(14), DeliveryStatus varchar(14), MatchStatus varchar(14),
                 TermsCode varchar(12), TermsSnapshot varchar(100), IncotermsRule varchar(3),
                 NetAmount decimal(18,2), VatAmount decimal(18,2), GrossAmount decimal(18,2),
                 CommitmentGBP decimal(18,2), ApprovedBy varchar(20), ApprovedAt datetime2(3),
                 IssuedAt datetime2(3), AcknowledgedAt datetime2(3), BlanketOrderNumber varchar(30),
                 BudgetCode varchar(20), LineCount int, OutstandingQty decimal(38,6),
                 OutstandingGBP decimal(38,2), ReceivedNotInvoicedGBP decimal(38,2),
                 EarliestDueDate date, LatestDueDate date, OverdueLines int, IsCommitment bit);
BEGIN TRY
    INSERT #o EXEC po.usp_OpenOrders @CompanyCode = @co, @IncludeClosed = 1;
    SELECT @n = COUNT(*) FROM #o WHERE OutstandingQty < 0;
    IF @n = 0 PRINT 'ok 7 - no open order reports a negative outstanding quantity -- REQ: PO-008, PO-011';
    ELSE PRINT 'not ok 7 - ' + CONVERT(varchar(10), @n) + ' order(s) report a negative outstanding quantity -- REQ: PO-008';
END TRY
BEGIN CATCH
    PRINT 'not ok 7 - the open order enquiry failed: ' + ERROR_MESSAGE() + ' -- REQ: PO-024';
END CATCH
DROP TABLE #o;
GO
