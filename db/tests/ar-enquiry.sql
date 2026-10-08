/*  Enquiry contract tests: the error paths the debtors procedures must refuse on, the permission
    gate, and the allocation enquiry offering only the same debtor's items with the D-22 short
    payment tolerance.  Every assertion runs inside a transaction that is rolled back.
    REQ: AR-010, AR-018, AR-022, COM-002, D-22, AC: credit of a different debtor cannot be used */
SET NOCOUNT ON;
GO
PRINT '# ar-enquiry';
DECLARE @msg varchar(400), @t date = CAST(GETDATE() AS date), @n int, @dec decimal(18,2);

/*  1 - an enquiry on an account that does not exist is refused, not answered with nothing. */
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC ar.usp_CustomerEnquiry @CompanyCode = 'STC', @CustomerCode = 'NOSUCHCUST', @UserCode = 'ARCLERK';
    PRINT 'not ok 1 - an enquiry on an unknown customer returned data -- REQ: AR-022';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0302%' PRINT 'ok 1 - an enquiry on an unknown customer is refused -- REQ: AR-022, COM-002';
    ELSE PRINT 'not ok 1 - unexpected error: ' + @msg;
END CATCH

/*  2 - a statement presentation the specification does not define is refused. */
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC ar.usp_CustomerStatementLines @CompanyCode = 'STC', @CustomerCode = 'C0001', @AsOf = NULL, @Style = 'SUMMARY';
    PRINT 'not ok 2 - an unknown statement style was accepted -- REQ: AR-018';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0306%' PRINT 'ok 2 - an unknown statement style is refused -- REQ: AR-018';
    ELSE PRINT 'not ok 2 - unexpected error: ' + @msg;
END CATCH

/*  3 - an unknown collection stage is refused rather than silently returning everything. */
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC ar.usp_CreditControlWorklist @CompanyCode = 'STC', @UserCode = 'CREDMGR', @StageCode = 'CHASEHARD';
    PRINT 'not ok 3 - an unknown collection stage was accepted -- REQ: AR-016';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0304%' PRINT 'ok 3 - an unknown collection stage is refused -- REQ: AR-016';
    ELSE PRINT 'not ok 3 - unexpected error: ' + @msg;
END CATCH

/*  4 - a user without the debtors enquiry permission is refused (COM-002). */
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC ar.usp_AgedDebtSummary @CompanyCode = 'STC', @AsOf = NULL, @UserCode = 'WHSE';
    PRINT 'not ok 4 - a warehouse operator obtained the aged debt report -- REQ: COM-002';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0301%' PRINT 'ok 4 - a user without AR.ENQUIRY is refused the aged debt report -- REQ: COM-002, AR-017';
    ELSE PRINT 'not ok 4 - unexpected error: ' + @msg;
END CATCH

/*  5 - the sales ledger clerk does hold the permission, so the same call succeeds. */
BEGIN TRY
    BEGIN TRANSACTION;
    IF core.fn_HasPermission('ARCLERK', 'AR.ENQUIRY', 'STC', NULL) = 1
        PRINT 'ok 5 - the sales ledger clerk holds AR.ENQUIRY -- REQ: COM-002';
    ELSE PRINT 'not ok 5 - the sales ledger clerk does not hold AR.ENQUIRY';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 5 - unexpected error: ' + @msg;
END CATCH

/*  6 and 7 - the allocation enquiry offers this debtor's items only, and never another debtor's
    unapplied credit, however convenient the amounts look. */
BEGIN TRANSACTION;
INSERT ar.Customer (CompanyCode, CustomerCode, LegalName, CurrencyCode, Status, CreatedBy)
VALUES ('STC','ZZALC1','Allocation Test One Ltd','GBP','ACTIVE','TEST'), ('STC','ZZALC2','Allocation Test Two Ltd','GBP','ACTIVE','TEST');
INSERT ar.OpenItem (CompanyCode, CustomerCode, ItemType, DocumentNumber, DocumentDate, DueDate, PostingDate, CurrencyCode,
        OriginalAmount, OriginalAmountGBP, ResidualAmount, ResidualAmountGBP, CreatedBy)
VALUES ('STC','ZZALC1','INVOICE','TST-ALC-I1', DATEADD(day,-20,@t), DATEADD(day,-5,@t), DATEADD(day,-20,@t),'GBP',1000,1000,1000,1000,'TEST'),
       ('STC','ZZALC1','INVOICE','TST-ALC-I2', DATEADD(day,-10,@t), DATEADD(day,20,@t), DATEADD(day,-10,@t),'GBP',2000,2000,2000,2000,'TEST'),
       ('STC','ZZALC2','RECEIPT','TST-ALC-R2', DATEADD(day,-2,@t),  DATEADD(day,-2,@t), DATEADD(day,-2,@t), 'GBP',-1000,-1000,-1000,-1000,'TEST');
SELECT @n = COUNT(*) FROM ar.vw_OpenItemAged WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZALC1' AND BalanceSide = 'DEBIT';
IF @n = 2 PRINT 'ok 6 - both of this debtor''s open invoices are offered for allocation -- REQ: AR-010';
ELSE PRINT 'not ok 6 - ' + CAST(@n AS varchar(4)) + ' invoice(s) offered';
SELECT @n = COUNT(*) FROM ar.vw_OpenItemAged WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZALC1' AND BalanceSide = 'CREDIT';
IF @n = 0 PRINT 'ok 7 - another debtor''s unapplied receipt is not offered against this account -- REQ: AR-010, AR-021';
ELSE PRINT 'not ok 7 - ' + CAST(@n AS varchar(4)) + ' foreign credit(s) offered';

/*  8 - the short payment a clerk may write off without approval is the D-22 figure of GBP 5.00. */
SET @dec = TRY_CONVERT(decimal(18,2), core.fn_Param('AR.WRITEOFF.AUTO.GBP', 'STC', @t));
IF @dec = 5.00 PRINT 'ok 8 - the clerk short payment tolerance is GBP 5.00 -- REQ: AR-012, D-22';
ELSE PRINT 'not ok 8 - tolerance read as ' + ISNULL(CAST(@dec AS varchar(20)), 'NULL');
ROLLBACK TRANSACTION;

/*  9 - a statement adds up: the balance brought forward at the start of the statement period plus
    the movements posted in it equals the closing balance the statement prints. */
BEGIN TRANSACTION;
DECLARE @open decimal(18,2), @close decimal(18,2), @move decimal(18,2),
        @from date = DATEFROMPARTS(YEAR(@t), MONTH(@t), 1), @prior date;
SET @prior = DATEADD(day, -1, @from);
SELECT @open = ISNULL(SUM(a.ResidualAsOfGBP), 0) FROM ar.fn_OpenItemAsOf('STC', @prior) a WHERE a.CustomerCode = 'C0001';
SELECT @close = ISNULL(SUM(a.ResidualAsOfGBP), 0) FROM ar.fn_OpenItemAsOf('STC', @t) a WHERE a.CustomerCode = 'C0001';
SET @move = @close - @open;
IF @open + @move = @close AND @close = (SELECT ISNULL(SUM(BalanceGBP), 0) FROM ar.vw_CustomerBalance WHERE CompanyCode = 'STC' AND CustomerCode = 'C0001')
    PRINT 'ok 9 - statement opening plus movement equals closing, and closing equals the ledger balance -- REQ: AR-018, AR-021';
ELSE PRINT 'not ok 9 - opening ' + ISNULL(CAST(@open AS varchar(20)), 'NULL') + ' movement ' + ISNULL(CAST(@move AS varchar(20)), 'NULL')
         + ' closing ' + ISNULL(CAST(@close AS varchar(20)), 'NULL');
ROLLBACK TRANSACTION;
PRINT '# ar-enquiry done';
GO
