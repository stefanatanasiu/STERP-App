/*  Payment proposals, payment practices reporting and the withdrawal of payment approval when a
    payee bank account changes.  A residual that one proposal has reserved is not offered to the
    next one and is never reserved twice however often the selection is repeated (AP-011), a
    released or cancelled run cannot be reselected at all, the half yearly payment practices
    figures are filed as a version that later restatements do not overwrite (AP-028), and a change
    of beneficiary withdraws the payment approval that relied on the old destination (AP-002).
    Test data uses the ZZ prefix in company STC; every case is built inside a transaction and
    rolled back, so a second run behaves exactly like the first.
    REQ: AP-002, AP-011, AP-012, AP-013, AP-014, AP-016, AP-028, COM-005
    AC: AC-028, AC-033, AC-128 */
SET NOCOUNT ON;
GO
PRINT '# ap-payments';
/*  Re-runnable: clear anything an aborted earlier run could have left behind. */
DELETE FROM ap.PaymentPracticesDetail WHERE CompanyCode = 'STC' AND SupplierCode LIKE 'ZZAP%';
DELETE FROM ap.PaymentPracticesReport WHERE CompanyCode = 'STC' AND ReportingPeriodKey = '201907-31';
DELETE FROM ap.PaymentProposalLine    WHERE ProposalId IN (SELECT ProposalId FROM ap.PaymentProposal
                                                            WHERE CompanyCode = 'STC' AND ProposalRef LIKE 'ZZPP%');
DELETE FROM ap.PaymentProposal        WHERE CompanyCode = 'STC' AND ProposalRef LIKE 'ZZPP%';
DELETE FROM core.Approval             WHERE CompanyCode = 'STC' AND DocumentRef LIKE 'ZZPP%';
DELETE FROM ap.Allocation             WHERE CompanyCode = 'STC' AND SupplierCode LIKE 'ZZAP%';
DELETE FROM ap.PaymentInstruction     WHERE CompanyCode = 'STC' AND SupplierCode LIKE 'ZZAP%';
DELETE FROM ap.OpenItem               WHERE CompanyCode = 'STC' AND SupplierCode LIKE 'ZZAP%';
DELETE FROM ap.SupplierBank           WHERE CompanyCode = 'STC' AND SupplierCode LIKE 'ZZAP%';
DELETE FROM ap.Supplier               WHERE CompanyCode = 'STC' AND SupplierCode LIKE 'ZZAP%';
GO
DECLARE @msg varchar(400), @p1 int, @p2 int, @p3 int, @p4 int, @rid int, @ver smallint, @pkey varchar(12),
        @gross decimal(18,2), @cr decimal(18,2), @cash decimal(18,2), @held decimal(18,2),
        @r1 decimal(18,2), @r2 decimal(18,2), @net decimal(18,2), @else bit, @st varchar(14), @n int, @avg decimal(9,2);

DECLARE @res TABLE (SelectedCount varchar(24), ExcludedCount varchar(24), CashPrincipal varchar(24),
                    CreditsApplied varchar(24), Discounts varchar(24), Reserved varchar(24), Outflow varchar(24));
DECLARE @ppr TABLE (ReportId int, Version smallint, ReportingPeriodKey varchar(12));
DECLARE @wd  TABLE (ApprovalsInvalidated varchar(12), InstructionsCancelled varchar(12));

/*  1 to 8 - the proposal of AC-033: invoices of 900.00 and 400.00, a credit of 150.00 and 500.00
    of the larger invoice already reserved by an active payment instruction.  The cutoff sits in
    early 2020 so that only this test's own items are due for selection.
    REQ: AP-011, AP-012, AP-014, COM-005  AC: AC-033 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT ap.Supplier (CompanyCode, SupplierCode, LegalName, TermsCode, CurrencyCode, PaymentMethod,
                        Status, CreatedBy)
    VALUES ('STC', 'ZZAPS1', 'Proposal Reservation Test Ltd', 'N30', 'GBP', 'BACS', 'ACTIVE', 'TEST');
    INSERT ap.SupplierBank (CompanyCode, SupplierCode, BankRef, BeneficiaryName, SortCode, AccountNumber,
                            Status, ApprovedBy, ApprovedAt, CreatedBy)
    VALUES ('STC', 'ZZAPS1', 'MAIN', 'Proposal Reservation Test Ltd', '201234', '41234567',
            'APPROVED', 'APMGR', SYSDATETIME(), 'TEST');

    /*  ZZAPI01 carries 500.00 of reservation already held by an active payment instruction. */
    INSERT ap.OpenItem (CompanyCode, SupplierCode, ItemType, DocumentNumber, DocumentDate, DueDate, PostingDate,
                        DateReceived, CurrencyCode, FxRate, OriginalAmount, OriginalAmountGBP, ResidualAmount,
                        ResidualAmountGBP, ReservedAmountGBP, TermsCode, SettlementStatus, CreatedBy)
    VALUES ('STC','ZZAPS1','INVOICE','ZZAPI01','2019-12-16','2020-01-15','2019-12-16','2019-12-16','GBP',1.0,
             900.00,  900.00,  900.00,  900.00, 500.00, 'N30', 'OPEN', 'TEST'),
           ('STC','ZZAPS1','INVOICE','ZZAPI02','2019-12-21','2020-01-20','2019-12-21','2019-12-21','GBP',1.0,
             400.00,  400.00,  400.00,  400.00,   0.00, 'N30', 'OPEN', 'TEST'),
           ('STC','ZZAPS1','CREDIT' ,'ZZAPC01','2019-12-22','2019-12-22','2019-12-22','2019-12-22','GBP',1.0,
            -150.00, -150.00, -150.00, -150.00,   0.00, 'N30', 'OPEN', 'TEST');

    INSERT ap.PaymentProposal (CompanyCode, ProposalRef, SelectionCutoffDate, PayDate, PaymentMethod,
                               BankAccountCode, CurrencyCode, PreparedBy, CreatedBy)
    VALUES ('STC', 'ZZPP1', '2020-01-31', '2020-02-05', 'BACS', 'GBP1', 'GBP', 'APCLERK', 'TEST'),
           ('STC', 'ZZPP2', '2020-01-31', '2020-02-05', 'BACS', 'GBP1', 'GBP', 'APCLERK', 'TEST');
    SELECT @p1 = ProposalId FROM ap.PaymentProposal WHERE CompanyCode = 'STC' AND ProposalRef = 'ZZPP1';
    SELECT @p2 = ProposalId FROM ap.PaymentProposal WHERE CompanyCode = 'STC' AND ProposalRef = 'ZZPP2';

    DELETE @res;
    INSERT @res EXEC ap.usp_ReserveProposal @CompanyCode = 'STC', @ProposalId = @p1, @UserCode = 'APCLERK';

    SELECT @gross = ISNULL(SUM(CASE WHEN IsExcluded = 0 AND GrossAmountGBP > 0 THEN GrossAmountGBP ELSE 0 END), 0),
           @cr    = ISNULL(SUM(CASE WHEN IsExcluded = 0 THEN CreditAppliedGBP ELSE 0 END), 0),
           @cash  = ISNULL(SUM(CASE WHEN IsExcluded = 0 AND NetPayGBP > 0 THEN NetPayGBP ELSE 0 END), 0),
           @held  = ISNULL(SUM(ReservedAmountGBP), 0)
      FROM ap.PaymentProposalLine WHERE ProposalId = @p1 AND SupplierCode = 'ZZAPS1';

    IF @gross = 1300.00 AND @cr = 150.00
        PRINT 'ok 1 - the two due invoices less the 150.00 credit leave 1150.00 of cash required -- REQ: AP-011  AC: AC-033';
    ELSE PRINT 'not ok 1 - selected gross was ' + ISNULL(CAST(@gross AS varchar(20)), 'NULL') + ' and credits '
               + ISNULL(CAST(@cr AS varchar(20)), 'NULL') + ' rather than 1300.00 and 150.00 -- REQ: AP-011  AC: AC-033';

    IF @cash - @cr = 650.00
        PRINT 'ok 2 - only the 650.00 that is not already reserved elsewhere is eligible for this run -- REQ: AP-011, AP-014  AC: AC-033';
    ELSE PRINT 'not ok 2 - the run asked for ' + ISNULL(CAST(@cash - @cr AS varchar(20)), 'NULL')
               + ' rather than 650.00 of additional cash -- REQ: AP-011, AP-014  AC: AC-033';

    SELECT @else = IsReservedElsewhere, @net = NetPayGBP FROM ap.PaymentProposalLine
     WHERE ProposalId = @p1 AND DocumentNumber = 'ZZAPI01';
    IF @else = 1 AND @net = 400.00
        PRINT 'ok 3 - the invoice held by another instruction is flagged and only its free 400.00 is offered -- REQ: AP-011  AC: AC-033';
    ELSE PRINT 'not ok 3 - the 900.00 invoice came back with reserved-elsewhere '
               + ISNULL(CAST(@else AS varchar(4)), 'NULL') + ' and net pay '
               + ISNULL(CAST(@net AS varchar(20)), 'NULL') + ' -- REQ: AP-011  AC: AC-033';

    SELECT @r1 = ReservedAmountGBP FROM ap.OpenItem WHERE CompanyCode = 'STC' AND DocumentNumber = 'ZZAPI01' AND ItemType = 'INVOICE';
    SELECT @r2 = ReservedAmountGBP FROM ap.OpenItem WHERE CompanyCode = 'STC' AND DocumentNumber = 'ZZAPI02' AND ItemType = 'INVOICE';
    IF @held = 800.00 AND @r1 = 900.00 AND @r2 = 400.00
        PRINT 'ok 4 - selecting reserves 800.00 against the ledger, leaving the invoices fully spoken for -- REQ: AP-011, AP-014';
    ELSE PRINT 'not ok 4 - the proposal reserved ' + ISNULL(CAST(@held AS varchar(20)), 'NULL')
               + ' and the ledger now holds ' + ISNULL(CAST(@r1 AS varchar(20)), 'NULL') + ' and '
               + ISNULL(CAST(@r2 AS varchar(20)), 'NULL') + ' -- REQ: AP-011, AP-014';

    /*  Reselecting the same proposal must release what it held before it takes it again. */
    DELETE @res;
    INSERT @res EXEC ap.usp_ReserveProposal @CompanyCode = 'STC', @ProposalId = @p1, @UserCode = 'APCLERK';
    SELECT @cash = ISNULL(SUM(CASE WHEN IsExcluded = 0 AND NetPayGBP > 0 THEN NetPayGBP ELSE 0 END), 0),
           @cr   = ISNULL(SUM(CASE WHEN IsExcluded = 0 THEN CreditAppliedGBP ELSE 0 END), 0),
           @held = ISNULL(SUM(ReservedAmountGBP), 0)
      FROM ap.PaymentProposalLine WHERE ProposalId = @p1 AND SupplierCode = 'ZZAPS1';
    SELECT @r1 = ReservedAmountGBP FROM ap.OpenItem WHERE CompanyCode = 'STC' AND DocumentNumber = 'ZZAPI01' AND ItemType = 'INVOICE';
    SELECT @r2 = ReservedAmountGBP FROM ap.OpenItem WHERE CompanyCode = 'STC' AND DocumentNumber = 'ZZAPI02' AND ItemType = 'INVOICE';
    IF @cash - @cr = 650.00 AND @held = 800.00 AND @r1 = 900.00 AND @r2 = 400.00
        PRINT 'ok 5 - running the selection again reserves the same residual once, not twice -- REQ: AP-011, AP-014  AC: AC-033';
    ELSE PRINT 'not ok 5 - after reselecting, cash required ' + ISNULL(CAST(@cash - @cr AS varchar(20)), 'NULL')
               + ' and the ledger holds ' + ISNULL(CAST(@r1 AS varchar(20)), 'NULL') + ' and '
               + ISNULL(CAST(@r2 AS varchar(20)), 'NULL') + ' -- REQ: AP-011, AP-014  AC: AC-033';

    SELECT @st = Status, @n = CASE WHEN ContentHash IS NULL AND ApprovalId IS NULL THEN 1 ELSE 0 END
      FROM ap.PaymentProposal WHERE ProposalId = @p1;
    IF @st = 'PROPOSED' AND @n = 1
        PRINT 'ok 6 - a reselected proposal is proposed again and carries no earlier approval forward -- REQ: AP-011, AP-012';
    ELSE PRINT 'not ok 6 - the proposal was left at ' + ISNULL(@st, 'NULL')
               + ' with a stale approval flag of ' + ISNULL(CAST(@n AS varchar(4)), 'NULL') + ' -- REQ: AP-011, AP-012';

    /*  A second proposal prepared over the same ledger finds nothing left to claim. */
    DELETE @res;
    INSERT @res EXEC ap.usp_ReserveProposal @CompanyCode = 'STC', @ProposalId = @p2, @UserCode = 'APCLERK';
    SELECT @n = COUNT(*) FROM ap.PaymentProposalLine
     WHERE ProposalId = @p2 AND SupplierCode = 'ZZAPS1' AND IsExcluded = 1
       AND ExclusionReason = 'Already reserved by another active payment instruction';
    SELECT @cash = ISNULL(SUM(CASE WHEN IsExcluded = 0 AND NetPayGBP > 0 THEN NetPayGBP ELSE 0 END), 0)
      FROM ap.PaymentProposalLine WHERE ProposalId = @p2 AND SupplierCode = 'ZZAPS1';

    IF @n = 2
        PRINT 'ok 7 - both invoices are listed on the second proposal as reserved by another instruction -- REQ: AP-011, COM-005  AC: AC-033';
    ELSE PRINT 'not ok 7 - ' + ISNULL(CAST(@n AS varchar(4)), 'NULL')
               + ' line(s) carried the already-reserved exclusion rather than 2 -- REQ: AP-011, COM-005  AC: AC-033';

    IF @cash = 0.00
        PRINT 'ok 8 - the second proposal asks for no cash because the residual is consumed once only -- REQ: AP-011, AP-014  AC: AC-033';
    ELSE PRINT 'not ok 8 - the second proposal still asked for ' + ISNULL(CAST(@cash AS varchar(20)), 'NULL')
               + ' -- REQ: AP-011, AP-014  AC: AC-033';

    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 1 to 8 - unexpected error in the proposal reservation tests: ' + @msg;
END CATCH

/*  9 - a released run has already been sent to the bank, so it cannot be reselected.
    REQ: AP-011, AP-013 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT ap.PaymentProposal (CompanyCode, ProposalRef, SelectionCutoffDate, PayDate, PaymentMethod,
                               BankAccountCode, CurrencyCode, Status, PreparedBy, CreatedBy)
    VALUES ('STC', 'ZZPP5', '2020-01-31', '2020-02-05', 'BACS', 'GBP1', 'GBP', 'RELEASED', 'APCLERK', 'TEST');
    SELECT @p3 = ProposalId FROM ap.PaymentProposal WHERE CompanyCode = 'STC' AND ProposalRef = 'ZZPP5';
    EXEC ap.usp_ReserveProposal @CompanyCode = 'STC', @ProposalId = @p3, @UserCode = 'APCLERK';
    PRINT 'not ok 9 - a released payment run was reselected -- REQ: AP-011, AP-013';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0031%'
        PRINT 'ok 9 - a released payment run cannot be reselected -- REQ: AP-011, AP-013';
    ELSE PRINT 'not ok 9 - unexpected error: ' + @msg;
END CATCH

/*  10 - nor can a cancelled one, which must be replaced rather than reopened.
    REQ: AP-011, AP-016 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT ap.PaymentProposal (CompanyCode, ProposalRef, SelectionCutoffDate, PayDate, PaymentMethod,
                               BankAccountCode, CurrencyCode, Status, PreparedBy, CreatedBy)
    VALUES ('STC', 'ZZPP6', '2020-01-31', '2020-02-05', 'BACS', 'GBP1', 'GBP', 'CANCELLED', 'APCLERK', 'TEST');
    SELECT @p4 = ProposalId FROM ap.PaymentProposal WHERE CompanyCode = 'STC' AND ProposalRef = 'ZZPP6';
    EXEC ap.usp_ReserveProposal @CompanyCode = 'STC', @ProposalId = @p4, @UserCode = 'APCLERK';
    PRINT 'not ok 10 - a cancelled payment run was reselected -- REQ: AP-011, AP-016';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0031%'
        PRINT 'ok 10 - a cancelled payment run cannot be reselected -- REQ: AP-011, AP-016';
    ELSE PRINT 'not ok 10 - unexpected error: ' + @msg;
END CATCH

/*  11 to 16 - the payment practices half year of AC-128: four invoices on thirty day terms paid
    10, 35, 70 and 45 days after they were received, one of the late ones disputed.  Average days
    to pay 40, bands 25 / 50 / 25 and 75 per cent not paid within terms.
    REQ: AP-028  AC: AC-128 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT ap.Supplier (CompanyCode, SupplierCode, LegalName, TermsCode, CurrencyCode, PaymentMethod,
                        Status, CreatedBy)
    VALUES ('STC', 'ZZAPS2', 'Payment Practices Test Ltd', 'N30', 'GBP', 'BACS', 'ACTIVE', 'TEST');

    INSERT ap.OpenItem (CompanyCode, SupplierCode, ItemType, DocumentNumber, DocumentDate, DueDate, PostingDate,
                        DateReceived, CurrencyCode, FxRate, OriginalAmount, OriginalAmountGBP, ResidualAmount,
                        ResidualAmountGBP, TermsCode, SettlementStatus, IsDisputed, CreatedBy)
    VALUES ('STC','ZZAPS2','INVOICE','ZZAPV01','2019-08-01','2019-08-31','2019-08-01','2019-08-01','GBP',1.0,
             1000.00,  1000.00, 0.00, 0.00, 'N30', 'SETTLED', 0, 'TEST'),
           ('STC','ZZAPS2','INVOICE','ZZAPV02','2019-08-01','2019-08-31','2019-08-01','2019-08-01','GBP',1.0,
             1000.00,  1000.00, 0.00, 0.00, 'N30', 'SETTLED', 0, 'TEST'),
           ('STC','ZZAPS2','INVOICE','ZZAPV03','2019-08-01','2019-08-31','2019-08-01','2019-08-01','GBP',1.0,
             1000.00,  1000.00, 0.00, 0.00, 'N30', 'SETTLED', 1, 'TEST'),
           ('STC','ZZAPS2','INVOICE','ZZAPV04','2019-08-01','2019-08-31','2019-08-01','2019-08-01','GBP',1.0,
             1000.00,  1000.00, 0.00, 0.00, 'N30', 'SETTLED', 0, 'TEST'),
           ('STC','ZZAPS2','PAYMENT','ZZAPY01','2019-08-11','2019-08-11','2019-08-11', NULL,      'GBP',1.0,
            -1000.00, -1000.00, 0.00, 0.00, NULL,  'SETTLED', 0, 'TEST'),
           ('STC','ZZAPS2','PAYMENT','ZZAPY02','2019-09-05','2019-09-05','2019-09-05', NULL,      'GBP',1.0,
            -1000.00, -1000.00, 0.00, 0.00, NULL,  'SETTLED', 0, 'TEST'),
           ('STC','ZZAPS2','PAYMENT','ZZAPY03','2019-10-10','2019-10-10','2019-10-10', NULL,      'GBP',1.0,
            -1000.00, -1000.00, 0.00, 0.00, NULL,  'SETTLED', 0, 'TEST'),
           ('STC','ZZAPS2','PAYMENT','ZZAPY04','2019-09-15','2019-09-15','2019-09-15', NULL,      'GBP',1.0,
            -1000.00, -1000.00, 0.00, 0.00, NULL,  'SETTLED', 0, 'TEST');

    INSERT ap.PaymentInstruction (CompanyCode, InstructionRef, SupplierCode, PaymentMethod, BankAccountCode,
                                  CurrencyCode, Amount, AmountGBP, BeneficiaryName, ValueDate, Status,
                                  SettledAt, SettledAmountGBP, CreatedBy)
    VALUES ('STC','ZZAPX01','ZZAPS2','BACS','GBP1','GBP',1000.00,1000.00,'Payment Practices Test Ltd',
            '2019-08-11','SETTLED','2019-08-11T11:00:00',1000.00,'TEST'),
           ('STC','ZZAPX02','ZZAPS2','BACS','GBP1','GBP',1000.00,1000.00,'Payment Practices Test Ltd',
            '2019-09-05','SETTLED','2019-09-05T11:00:00',1000.00,'TEST'),
           ('STC','ZZAPX03','ZZAPS2','BACS','GBP1','GBP',1000.00,1000.00,'Payment Practices Test Ltd',
            '2019-10-10','SETTLED','2019-10-10T11:00:00',1000.00,'TEST'),
           ('STC','ZZAPX04','ZZAPS2','BACS','GBP1','GBP',1000.00,1000.00,'Payment Practices Test Ltd',
            '2019-09-15','SETTLED','2019-09-15T11:00:00',1000.00,'TEST');

    INSERT ap.Allocation (CompanyCode, SupplierCode, SourceType, SourceOpenItemId, TargetOpenItemId,
                          PaymentInstructionId, EffectiveDate, CurrencyCode, AppliedAmount, AppliedAmountGBP,
                          Status, CreatedBy)
    SELECT 'STC', 'ZZAPS2', 'PAYMENT', p.OpenItemId, i.OpenItemId, x.PaymentInstructionId, x.ValueDate,
           'GBP', 1000.00, 1000.00, 'ACTIVE', 'TEST'
      FROM (VALUES ('ZZAPV01','ZZAPY01','ZZAPX01'), ('ZZAPV02','ZZAPY02','ZZAPX02'),
                   ('ZZAPV03','ZZAPY03','ZZAPX03'), ('ZZAPV04','ZZAPY04','ZZAPX04')) v (Inv, Pay, Instr)
      JOIN ap.OpenItem i ON i.CompanyCode = 'STC' AND i.ItemType = 'INVOICE' AND i.DocumentNumber = v.Inv
      JOIN ap.OpenItem p ON p.CompanyCode = 'STC' AND p.ItemType = 'PAYMENT' AND p.DocumentNumber = v.Pay
      JOIN ap.PaymentInstruction x ON x.CompanyCode = 'STC' AND x.InstructionRef = v.Instr;

    DELETE @ppr;
    INSERT @ppr EXEC ap.usp_PaymentPracticesData @CompanyCode = 'STC', @PeriodRef = 'PPR-20190701-20191231',
        @AverageDays = 40.00, @PctWithin30 = 25.00, @Pct31To60 = 50.00, @PctOver60 = 25.00,
        @PctNotInTerms = 75.00, @InvoiceCount = 4, @TotalValue = 4000.00, @CreatedBy = 'APCLERK';
    SELECT @rid = ReportId, @ver = Version, @pkey = ReportingPeriodKey FROM @ppr;

    SELECT @n = InvoiceCount, @avg = AverageDaysToPay FROM ap.PaymentPracticesReport WHERE ReportId = @rid;
    IF @ver = 1 AND @pkey = '201907-31' AND @n = 4
        PRINT 'ok 11 - the half year is filed as version 1 of period 201907-31 over four invoices -- REQ: AP-028  AC: AC-128';
    ELSE PRINT 'not ok 11 - the report was filed as version ' + ISNULL(CAST(@ver AS varchar(6)), 'NULL')
               + ' of period ' + ISNULL(@pkey, 'NULL') + ' over '
               + ISNULL(CAST(@n AS varchar(6)), 'NULL') + ' invoice(s) -- REQ: AP-028  AC: AC-128';

    IF @avg = 40.00
        PRINT 'ok 12 - average days to pay over the four invoices is 40 days -- REQ: AP-028  AC: AC-128';
    ELSE PRINT 'not ok 12 - average days to pay came back as ' + ISNULL(CAST(@avg AS varchar(20)), 'NULL')
               + ' rather than 40.00 -- REQ: AP-028  AC: AC-128';

    SELECT @gross = PctPaidWithin30, @cr = PctPaid31To60, @cash = PctPaid61Plus
      FROM ap.PaymentPracticesReport WHERE ReportId = @rid;
    IF @gross = 25.00 AND @cr = 50.00 AND @cash = 25.00 AND @gross + @cr + @cash = 100.00
        PRINT 'ok 13 - the 30 day, 31 to 60 day and 61 day or more bands are 25, 50 and 25 and add to 100 per cent -- REQ: AP-028  AC: AC-128';
    ELSE PRINT 'not ok 13 - the bands were ' + ISNULL(CAST(@gross AS varchar(20)), 'NULL') + ', '
               + ISNULL(CAST(@cr AS varchar(20)), 'NULL') + ' and ' + ISNULL(CAST(@cash AS varchar(20)), 'NULL')
               + ' -- REQ: AP-028  AC: AC-128';

    SELECT @cash = PctNotPaidWithinTerms FROM ap.PaymentPracticesReport WHERE ReportId = @rid;
    IF @cash = 75.00
        PRINT 'ok 14 - three of the four invoices missed the agreed 30 day terms, so 75 per cent were late -- REQ: AP-028  AC: AC-128';
    ELSE PRINT 'not ok 14 - the proportion not paid within terms was '
               + ISNULL(CAST(@cash AS varchar(20)), 'NULL') + ' rather than 75.00 -- REQ: AP-028  AC: AC-128';

    SELECT @n = COUNT(*), @gross = ISNULL(SUM(GrossAmountGBP), 0) FROM ap.PaymentPracticesDetail
     WHERE ReportId = @rid AND SupplierCode = 'ZZAPS2';
    SELECT @cr = COUNT(*) FROM ap.PaymentPracticesDetail
     WHERE ReportId = @rid AND SupplierCode = 'ZZAPS2' AND DaysToPay IN (10, 35, 70, 45);
    IF @n = 4 AND @cr = 4 AND @gross = 4000.00
        PRINT 'ok 15 - one detail row per invoice at 10, 35, 70 and 45 days and 4000.00 of value backs the headline figures -- REQ: AP-028  AC: AC-128';
    ELSE PRINT 'not ok 15 - ' + ISNULL(CAST(@n AS varchar(6)), 'NULL') + ' detail row(s) worth '
               + ISNULL(CAST(@gross AS varchar(20)), 'NULL') + ' were kept, of which '
               + ISNULL(CAST(@cr AS varchar(6)), 'NULL') + ' had the expected days to pay -- REQ: AP-028  AC: AC-128';

    /*  A restatement is a new version; the figures already filed are not rewritten. */
    DELETE @ppr;
    INSERT @ppr EXEC ap.usp_PaymentPracticesData @CompanyCode = 'STC', @PeriodRef = 'PPR-20190701-20191231',
        @AverageDays = 41.00, @PctWithin30 = 25.00, @Pct31To60 = 50.00, @PctOver60 = 25.00,
        @PctNotInTerms = 75.00, @InvoiceCount = 4, @TotalValue = 4000.00, @CreatedBy = 'APCLERK';
    SELECT @ver = Version FROM @ppr;
    SELECT @avg = AverageDaysToPay FROM ap.PaymentPracticesReport WHERE ReportId = @rid;
    IF @ver = 2 AND @avg = 40.00
        PRINT 'ok 16 - restating the period files version 2 and leaves version 1 exactly as reported -- REQ: AP-028  AC: AC-128';
    ELSE PRINT 'not ok 16 - the restatement was filed as version ' + ISNULL(CAST(@ver AS varchar(6)), 'NULL')
               + ' and version 1 now reads ' + ISNULL(CAST(@avg AS varchar(20)), 'NULL') + ' -- REQ: AP-028  AC: AC-128';

    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 11 to 16 - unexpected error in the payment practices tests: ' + @msg;
END CATCH

/*  17 - a reporting period that is not a proper half year reference is refused outright.
    REQ: AP-028 */
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC ap.usp_PaymentPracticesData @CompanyCode = 'STC', @PeriodRef = 'PPR-2019', @AverageDays = 40.00,
        @PctWithin30 = 25.00, @Pct31To60 = 50.00, @PctOver60 = 25.00, @PctNotInTerms = 75.00,
        @InvoiceCount = 4, @TotalValue = 4000.00, @CreatedBy = 'APCLERK';
    PRINT 'not ok 17 - a malformed reporting period was accepted -- REQ: AP-028';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-AP028%'
        PRINT 'ok 17 - a malformed reporting period reference is rejected -- REQ: AP-028';
    ELSE PRINT 'not ok 17 - unexpected error: ' + @msg;
END CATCH

/*  18 to 20 - the bank change of AC-028.  A proposal awaiting approval and an approved one both
    pay this supplier; the beneficiary details change, so the approvals that relied on the old
    destination are withdrawn, the instruction that has not yet been sent is cancelled and the
    reservations it held are given back.  REQ: AP-002, AP-012, AP-013, AP-016  AC: AC-028 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT ap.Supplier (CompanyCode, SupplierCode, LegalName, TermsCode, CurrencyCode, PaymentMethod,
                        Status, CreatedBy)
    VALUES ('STC', 'ZZAPS3', 'Bank Change Test Ltd', 'N30', 'GBP', 'BACS', 'ACTIVE', 'TEST');
    INSERT ap.SupplierBank (CompanyCode, SupplierCode, BankRef, BeneficiaryName, SortCode, AccountNumber,
                            Status, ApprovedBy, ApprovedAt, CreatedBy)
    VALUES ('STC', 'ZZAPS3', 'MAIN', 'Bank Change Test Ltd', '309876', '87654321',
            'APPROVED', 'APMGR', SYSDATETIME(), 'TEST');

    INSERT ap.OpenItem (CompanyCode, SupplierCode, ItemType, DocumentNumber, DocumentDate, DueDate, PostingDate,
                        DateReceived, CurrencyCode, FxRate, OriginalAmount, OriginalAmountGBP, ResidualAmount,
                        ResidualAmountGBP, ReservedAmountGBP, TermsCode, SettlementStatus, CreatedBy)
    VALUES ('STC','ZZAPS3','INVOICE','ZZAPI11','2019-12-16','2020-01-15','2019-12-16','2019-12-16','GBP',1.0,
            1000.00, 1000.00, 1000.00, 1000.00, 400.00, 'N30', 'OPEN', 'TEST'),
           ('STC','ZZAPS3','INVOICE','ZZAPI12','2019-12-18','2020-01-17','2019-12-18','2019-12-18','GBP',1.0,
             900.00,  900.00,  900.00,  900.00, 600.00, 'N30', 'OPEN', 'TEST');

    INSERT ap.PaymentProposal (CompanyCode, ProposalRef, SelectionCutoffDate, PayDate, PaymentMethod,
                               BankAccountCode, CurrencyCode, Status, PreparedBy, CreatedBy)
    VALUES ('STC', 'ZZPP3', '2020-01-31', '2020-02-05', 'BACS', 'GBP1', 'GBP', 'PROPOSED', 'APCLERK', 'TEST'),
           ('STC', 'ZZPP4', '2020-01-31', '2020-02-05', 'BACS', 'GBP1', 'GBP', 'APPROVED', 'APCLERK', 'TEST');
    SELECT @p3 = ProposalId FROM ap.PaymentProposal WHERE CompanyCode = 'STC' AND ProposalRef = 'ZZPP3';
    SELECT @p4 = ProposalId FROM ap.PaymentProposal WHERE CompanyCode = 'STC' AND ProposalRef = 'ZZPP4';

    INSERT ap.PaymentProposalLine (ProposalId, LineNum, SupplierCode, OpenItemId, DocumentNumber, CurrencyCode,
                                   GrossAmount, GrossAmountGBP, DiscountGBP, CreditAppliedGBP, NetPayGBP,
                                   IsExcluded, IsReservedElsewhere, ReservedAmountGBP)
    SELECT @p3, 1, 'ZZAPS3', o.OpenItemId, 'ZZAPI11', 'GBP', 1000.00, 1000.00, 0.00, 0.00, 400.00, 0, 0, 400.00
      FROM ap.OpenItem o WHERE o.CompanyCode = 'STC' AND o.ItemType = 'INVOICE' AND o.DocumentNumber = 'ZZAPI11'
    UNION ALL
    SELECT @p4, 1, 'ZZAPS3', o.OpenItemId, 'ZZAPI12', 'GBP', 900.00, 900.00, 0.00, 0.00, 600.00, 0, 0, 600.00
      FROM ap.OpenItem o WHERE o.CompanyCode = 'STC' AND o.ItemType = 'INVOICE' AND o.DocumentNumber = 'ZZAPI12';

    INSERT core.Approval (CompanyCode, DocumentType, DocumentRef, ApprovalType, Amount, CurrencyCode, AmountGBP,
                          ContentHash, RequiredPermission, RequestedBy, Status)
    VALUES ('STC', 'PAYMENTPROPOSAL', 'ZZPP3', 'PAYMENTRUN', 400.00, 'GBP', 400.00, 'ZZHASH-P3',
            'AP.PAYMENT.APPROVE', 'APCLERK', 'PENDING'),
           ('STC', 'PAYMENTPROPOSAL', 'ZZPP4', 'PAYMENTRUN', 600.00, 'GBP', 600.00, 'ZZHASH-P4',
            'AP.PAYMENT.APPROVE', 'APCLERK', 'APPROVED');

    INSERT ap.PaymentInstruction (CompanyCode, InstructionRef, SupplierCode, PaymentMethod, BankAccountCode,
                                  CurrencyCode, Amount, AmountGBP, BeneficiaryName, ValueDate, Status, CreatedBy)
    VALUES ('STC','ZZAPX11','ZZAPS3','BACS','GBP1','GBP',600.00,600.00,'Bank Change Test Ltd',
            '2020-02-05','APPROVED','TEST'),
           ('STC','ZZAPX12','ZZAPS3','BACS','GBP1','GBP',400.00,400.00,'Bank Change Test Ltd',
            '2020-02-04','SUBMITTED','TEST');

    DELETE @wd;
    INSERT @wd EXEC ap.usp_InvalidatePaymentApprovals @CompanyCode = 'STC', @SupplierCode = 'ZZAPS3',
        @UserCode = 'APMGR', @Reason = 'Payee bank details changed';

    SELECT @st = Status FROM core.Approval WHERE CompanyCode = 'STC' AND DocumentRef = 'ZZPP3';
    IF @st = 'INVALIDATED'
        PRINT 'ok 18 - a changed supplier bank invalidates the pending payment approval -- REQ: AP-002, AP-012  AC: AC-028';
    ELSE PRINT 'not ok 18 - the pending payment approval was left at ' + ISNULL(@st, 'NULL')
               + ' -- REQ: AP-002, AP-012  AC: AC-028';

    SELECT @n = COUNT(*) FROM ap.PaymentInstruction
     WHERE CompanyCode = 'STC' AND InstructionRef = 'ZZAPX11' AND Status = 'CANCELLED';
    SELECT @cr = COUNT(*) FROM ap.PaymentInstruction
     WHERE CompanyCode = 'STC' AND InstructionRef = 'ZZAPX12' AND Status = 'SUBMITTED';
    IF @n = 1 AND @cr = 1
        PRINT 'ok 19 - the approved but unsent instruction is cancelled while the submitted one is left alone -- REQ: AP-013, AP-016  AC: AC-028';
    ELSE PRINT 'not ok 19 - the unsent instruction cancelled count was ' + ISNULL(CAST(@n AS varchar(6)), 'NULL')
               + ' and the submitted one is still submitted ' + ISNULL(CAST(@cr AS varchar(6)), 'NULL')
               + ' -- REQ: AP-013, AP-016  AC: AC-028';

    SELECT @st = Status FROM ap.PaymentProposal WHERE ProposalId = @p4;
    SELECT @held = ISNULL(SUM(ReservedAmountGBP), 0) FROM ap.OpenItem
     WHERE CompanyCode = 'STC' AND SupplierCode = 'ZZAPS3';
    IF @st = 'DRAFT' AND @held = 0.00
        PRINT 'ok 20 - the approved proposal falls back to draft and the reservations it held are released -- REQ: AP-002, AP-011  AC: AC-028';
    ELSE PRINT 'not ok 20 - the approved proposal is at ' + ISNULL(@st, 'NULL') + ' with '
               + ISNULL(CAST(@held AS varchar(20)), 'NULL') + ' still reserved -- REQ: AP-002, AP-011  AC: AC-028';

    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 18 to 20 - unexpected error in the bank change tests: ' + @msg;
END CATCH
GO
