/*  Ageing tests: the due-date buckets, the rule that an invoice is not overdue on its due date,
    credits kept out of the overdue buckets, the bucket totals reconciling to the balance on the
    same date, and the as-at-date rebuild that makes an old aged debt report reproducible.
    Every assertion runs inside a transaction that is rolled back.
    REQ: AR-013, AR-017, AR-021, AC: aged buckets reconcile to the same date receivables balance */
SET NOCOUNT ON;
GO
PRINT '# ar-ageing';
DECLARE @t date = CAST(GETDATE() AS date), @msg varchar(400), @n int, @bal decimal(18,2), @sum decimal(18,2),
        @r decimal(18,2), @b int;

/*  1 to 6 - the default buckets: not yet due, 1-30, 31-60, 61-90 and over 90 days overdue. */
BEGIN TRANSACTION;
INSERT ar.Customer (CompanyCode, CustomerCode, LegalName, CurrencyCode, TermsCode, Status, CreatedBy)
VALUES ('STC', 'ZZAGE1', 'Ageing Test Account Ltd', 'GBP', 'N30', 'ACTIVE', 'TEST');
INSERT ar.CustomerCredit (CompanyCode, CustomerCode, CreditLimitGBP, OverdueThresholdGBP, OverdueDaysTrigger, CreatedBy)
VALUES ('STC', 'ZZAGE1', 50000.00, 0, 30, 'TEST');
INSERT ar.OpenItem (CompanyCode, CustomerCode, ItemType, DocumentNumber, DocumentDate, DueDate, PostingDate, CurrencyCode,
        OriginalAmount, OriginalAmountGBP, ResidualAmount, ResidualAmountGBP, CreatedBy)
VALUES ('STC','ZZAGE1','INVOICE','TST-AGE-A', DATEADD(day,-30,@t), @t,                 DATEADD(day,-30,@t),'GBP',1000,1000,1000,1000,'TEST'),
       ('STC','ZZAGE1','INVOICE','TST-AGE-B', DATEADD(day,-31,@t), DATEADD(day, -1,@t),DATEADD(day,-31,@t),'GBP',2000,2000,2000,2000,'TEST'),
       ('STC','ZZAGE1','INVOICE','TST-AGE-C', DATEADD(day,-60,@t), DATEADD(day,-30,@t),DATEADD(day,-60,@t),'GBP',3000,3000,3000,3000,'TEST'),
       ('STC','ZZAGE1','INVOICE','TST-AGE-D', DATEADD(day,-61,@t), DATEADD(day,-31,@t),DATEADD(day,-61,@t),'GBP',4000,4000,4000,4000,'TEST'),
       ('STC','ZZAGE1','INVOICE','TST-AGE-E', DATEADD(day,-91,@t), DATEADD(day,-61,@t),DATEADD(day,-91,@t),'GBP',5000,5000,5000,5000,'TEST'),
       ('STC','ZZAGE1','INVOICE','TST-AGE-F',DATEADD(day,-121,@t), DATEADD(day,-91,@t),DATEADD(day,-121,@t),'GBP',6000,6000,6000,6000,'TEST'),
       ('STC','ZZAGE1','CREDIT', 'TST-AGE-G', DATEADD(day,-10,@t), DATEADD(day,-10,@t),DATEADD(day,-10,@t),'GBP',-500,-500,-500,-500,'TEST');

SELECT @b = BucketNo FROM ar.vw_OpenItemAged WHERE CompanyCode = 'STC' AND DocumentNumber = 'TST-AGE-A';
IF @b = 1 PRINT 'ok 1 - on its due date an invoice is not yet overdue -- REQ: AR-017';
ELSE PRINT 'not ok 1 - due today landed in bucket ' + ISNULL(CAST(@b AS varchar(4)), 'NULL') + ' -- REQ: AR-017';
SELECT @b = BucketNo FROM ar.vw_OpenItemAged WHERE CompanyCode = 'STC' AND DocumentNumber = 'TST-AGE-B';
IF @b = 2 PRINT 'ok 2 - one day past due ages into the 1-30 day bucket -- REQ: AR-017';
ELSE PRINT 'not ok 2 - one day past due landed in bucket ' + ISNULL(CAST(@b AS varchar(4)), 'NULL');
SELECT @b = BucketNo FROM ar.vw_OpenItemAged WHERE CompanyCode = 'STC' AND DocumentNumber = 'TST-AGE-C';
IF @b = 2 PRINT 'ok 3 - exactly thirty days past due stays in the 1-30 day bucket -- REQ: AR-017';
ELSE PRINT 'not ok 3 - thirty days past due landed in bucket ' + ISNULL(CAST(@b AS varchar(4)), 'NULL');
SELECT @b = BucketNo FROM ar.vw_OpenItemAged WHERE CompanyCode = 'STC' AND DocumentNumber = 'TST-AGE-D';
IF @b = 3 PRINT 'ok 4 - thirty one days past due ages into the 31-60 day bucket -- REQ: AR-017';
ELSE PRINT 'not ok 4 - thirty one days past due landed in bucket ' + ISNULL(CAST(@b AS varchar(4)), 'NULL');
SELECT @b = MIN(BucketNo) FROM ar.vw_OpenItemAged WHERE CompanyCode = 'STC' AND DocumentNumber IN ('TST-AGE-E');
IF @b = 4 PRINT 'ok 5 - sixty one days past due ages into the 61-90 day bucket -- REQ: AR-017';
ELSE PRINT 'not ok 5 - sixty one days past due landed in bucket ' + ISNULL(CAST(@b AS varchar(4)), 'NULL');
SELECT @b = BucketNo FROM ar.vw_OpenItemAged WHERE CompanyCode = 'STC' AND DocumentNumber = 'TST-AGE-F';
IF @b = 5 PRINT 'ok 6 - ninety one days past due ages into the over ninety day bucket -- REQ: AR-017';
ELSE PRINT 'not ok 6 - ninety one days past due landed in bucket ' + ISNULL(CAST(@b AS varchar(4)), 'NULL');

/*  7 - a credit balance is reported as a credit, never as overdue debt. */
SELECT @b = BucketNo, @r = OverdueGBP FROM ar.vw_OpenItemAged WHERE CompanyCode = 'STC' AND DocumentNumber = 'TST-AGE-G';
IF @b = 0 AND @r = 0 PRINT 'ok 7 - an unapplied credit sits in the credits bucket and adds nothing to overdue debt -- REQ: AR-013, AR-017';
ELSE PRINT 'not ok 7 - credit landed in bucket ' + ISNULL(CAST(@b AS varchar(4)), 'NULL') + ' with overdue ' + ISNULL(CAST(@r AS varchar(20)), 'NULL');

/*  8 - the bucket totals add back to the receivables balance for the same account and date. */
SELECT @bal = BalanceGBP FROM ar.vw_CustomerBalance WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZAGE1';
SELECT @sum = SUM(ResidualAmountGBP) FROM ar.vw_OpenItemAged WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZAGE1';
IF @bal = @sum AND @bal = 20500.00 PRINT 'ok 8 - bucket totals reconcile to the receivables balance of 20,500.00 -- REQ: AR-017, AR-021';
ELSE PRINT 'not ok 8 - balance ' + ISNULL(CAST(@bal AS varchar(20)), 'NULL') + ' against bucket total ' + ISNULL(CAST(@sum AS varchar(20)), 'NULL');
ROLLBACK TRANSACTION;

/*  9 and 10 - an aged debt report for an earlier date is not changed by a later receipt: the
    as-at rebuild reverses every allocation that took effect after the reporting date. */
BEGIN TRANSACTION;
DECLARE @inv bigint, @rct bigint;
INSERT ar.Customer (CompanyCode, CustomerCode, LegalName, CurrencyCode, TermsCode, Status, CreatedBy)
VALUES ('STC', 'ZZAGE2', 'As At Date Test Ltd', 'GBP', 'N30', 'ACTIVE', 'TEST');
INSERT ar.OpenItem (CompanyCode, CustomerCode, ItemType, DocumentNumber, DocumentDate, DueDate, PostingDate, CurrencyCode,
        OriginalAmount, OriginalAmountGBP, ResidualAmount, ResidualAmountGBP, CreatedBy)
VALUES ('STC','ZZAGE2','INVOICE','TST-ASOF-I', DATEADD(day,-100,@t), DATEADD(day,-70,@t), DATEADD(day,-100,@t),'GBP',1000,1000,600,600,'TEST');
SET @inv = SCOPE_IDENTITY();
INSERT ar.OpenItem (CompanyCode, CustomerCode, ItemType, DocumentNumber, DocumentDate, DueDate, PostingDate, CurrencyCode,
        OriginalAmount, OriginalAmountGBP, ResidualAmount, ResidualAmountGBP, SettlementStatus, CreatedBy)
VALUES ('STC','ZZAGE2','RECEIPT','TST-ASOF-R', DATEADD(day,-10,@t), DATEADD(day,-10,@t), DATEADD(day,-10,@t),'GBP',-400,-400,0,0,'SETTLED','TEST');
SET @rct = SCOPE_IDENTITY();
INSERT ar.Allocation (CompanyCode, CustomerCode, SourceType, SourceOpenItemId, TargetOpenItemId, EffectiveDate, CurrencyCode,
        AppliedAmount, AppliedAmountGBP, Status, CreatedBy)
VALUES ('STC','ZZAGE2','RECEIPT', @rct, @inv, DATEADD(day,-10,@t), 'GBP', 400, 400, 'ACTIVE', 'TEST');

SELECT @r = ResidualAsOfGBP, @b = BucketNo FROM ar.fn_OpenItemAsOf('STC', DATEADD(day,-20,@t)) WHERE DocumentNumber = 'TST-ASOF-I';
IF @r = 1000.00 AND @b = 3 PRINT 'ok 9 - as at twenty days ago the invoice still shows 1,000.00 in the 31-60 day bucket -- REQ: AR-017, AR-021';
ELSE PRINT 'not ok 9 - as at residual ' + ISNULL(CAST(@r AS varchar(20)), 'NULL') + ' bucket ' + ISNULL(CAST(@b AS varchar(4)), 'NULL');
SELECT @r = ResidualAsOfGBP FROM ar.fn_OpenItemAsOf('STC', @t) WHERE DocumentNumber = 'TST-ASOF-I';
IF @r = 600.00 PRINT 'ok 10 - as at today the same invoice shows the 600.00 left after the receipt -- REQ: AR-010, AR-017';
ELSE PRINT 'not ok 10 - residual today ' + ISNULL(CAST(@r AS varchar(20)), 'NULL');

/*  11 - the receipt that paid it is fully consumed and shows nothing as at today. */
SELECT @n = COUNT(*) FROM ar.fn_OpenItemAsOf('STC', @t) WHERE DocumentNumber = 'TST-ASOF-R';
IF @n = 0 PRINT 'ok 11 - a fully applied receipt leaves no residual on the ledger -- REQ: AR-010';
ELSE PRINT 'not ok 11 - a fully applied receipt still reports ' + CAST(@n AS varchar(4)) + ' open row(s)';
ROLLBACK TRANSACTION;
PRINT '# ar-ageing done';
GO
