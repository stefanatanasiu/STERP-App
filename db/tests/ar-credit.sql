/*  Credit exposure and collection worklist tests: utilisation and available credit, the over limit
    and hold states, the rule that recording a dispute does not reduce exposure, the baseline
    dunning stage for an age, and the three reasons a collection step is suppressed.
    Every assertion runs inside a transaction that is rolled back.
    REQ: AR-007, AR-008, AR-015, AR-016, D-06, D-28, AC: a dispute does not erase debt */
SET NOCOUNT ON;
GO
PRINT '# ar-credit';
DECLARE @t date = CAST(GETDATE() AS date), @n int, @dec decimal(18,2), @pct decimal(18,6), @s varchar(20), @sup bit,
        @reason varchar(200), @case int;

BEGIN TRANSACTION;
INSERT ar.Customer (CompanyCode, CustomerCode, LegalName, CurrencyCode, TermsCode, Status, AccountOwner, CreatedBy)
VALUES ('STC', 'ZZCRD1', 'Over Limit Test Ltd', 'GBP', 'N30', 'ACTIVE', 'ARCLERK', 'TEST'),
       ('STC', 'ZZCRD2', 'Restricted Contact Ltd', 'GBP', 'N30', 'ACTIVE', 'ARCLERK', 'TEST');
UPDATE ar.Customer SET ContactRestriction = 'Written contact only, via the appointed adviser'
 WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCRD2';
INSERT ar.CustomerCredit (CompanyCode, CustomerCode, CreditLimitGBP, OverdueThresholdGBP, OverdueDaysTrigger, ControllerUser, CreatedBy)
VALUES ('STC', 'ZZCRD1', 10000.00, 0, 30, 'CREDMGR', 'TEST'), ('STC', 'ZZCRD2', 10000.00, 0, 30, 'CREDMGR', 'TEST');
INSERT ar.OpenItem (CompanyCode, CustomerCode, ItemType, DocumentNumber, DocumentDate, DueDate, PostingDate, CurrencyCode,
        OriginalAmount, OriginalAmountGBP, ResidualAmount, ResidualAmountGBP, CreatedBy)
VALUES ('STC','ZZCRD1','INVOICE','TST-CRD-1', DATEADD(day,-70,@t), DATEADD(day,-40,@t), DATEADD(day,-70,@t),'GBP',12000,12000,12000,12000,'TEST'),
       ('STC','ZZCRD2','INVOICE','TST-CRD-2', DATEADD(day,-40,@t), DATEADD(day,-10,@t), DATEADD(day,-40,@t),'GBP', 3000, 3000, 3000, 3000,'TEST');

/*  1 and 2 - exposure, utilisation and available credit against a 10,000.00 limit. */
SELECT @dec = CreditAvailableGBP, @pct = UtilisationPct, @s = CreditState
  FROM ar.vw_CustomerBalance WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCRD1';
IF @dec = -2000.00 AND @pct = 120.00 PRINT 'ok 1 - a 12,000.00 balance on a 10,000.00 limit is 120 per cent used with 2,000.00 of excess -- REQ: AR-008, AR-022';
ELSE PRINT 'not ok 1 - available ' + ISNULL(CAST(@dec AS varchar(20)), 'NULL') + ' utilisation ' + ISNULL(CAST(@pct AS varchar(20)), 'NULL');
IF @s = 'OVERLIMIT' PRINT 'ok 2 - the account is reported as over its credit limit -- REQ: AR-008';
ELSE PRINT 'not ok 2 - credit state was ' + ISNULL(@s, 'NULL');

/*  3 - a live scoped override raises the limit for as long as it is valid, and no longer (D-06). */
INSERT ar.CreditOverride (CompanyCode, CustomerCode, Scope, OverrideAmountGBP, Reason, RequestedBy, ApprovedBy, ValidFrom, ExpiresOn, Status)
VALUES ('STC','ZZCRD1','ACCOUNT', 5000.00, 'Seasonal peak agreed with the credit manager', 'ARCLERK', 'CREDMGR', DATEADD(day,-1,@t), DATEADD(day,3,@t), 'ACTIVE');
SELECT @dec = CreditAvailableGBP, @s = CreditState FROM ar.vw_CustomerBalance WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCRD1';
IF @dec = 3000.00 AND @s = 'OVERDUE' PRINT 'ok 3 - an active override of 5,000.00 restores 3,000.00 of headroom -- REQ: AR-008, D-06';
ELSE PRINT 'not ok 3 - available after override ' + ISNULL(CAST(@dec AS varchar(20)), 'NULL') + ' state ' + ISNULL(@s, 'NULL');

/*  4 - the worklist puts a forty day old debt at the account stop stage (D-06 baseline). */
SELECT @s = StageCode, @sup = IsSuppressed FROM ar.vw_CollectionWorklist WHERE CompanyCode = 'STC' AND DocumentNumber = 'TST-CRD-1';
IF @s = 'ACCOUNTSTOP' AND @sup = 0 PRINT 'ok 4 - forty days past due reaches the account stop stage and is workable -- REQ: AR-016, D-06';
ELSE PRINT 'not ok 4 - stage ' + ISNULL(@s, 'NULL') + ' suppressed ' + ISNULL(CAST(@sup AS varchar(4)), 'NULL');

/*  5 and 6 - recording a dispute suppresses the chase but does not erase the debt or the exposure. */
UPDATE ar.OpenItem SET IsDisputed = 1, DisputedAmountGBP = 12000.00, CollectionStatus = 'DISPUTED'
 WHERE CompanyCode = 'STC' AND DocumentNumber = 'TST-CRD-1';
SELECT @sup = IsSuppressed, @reason = SuppressReason FROM ar.vw_CollectionWorklist WHERE CompanyCode = 'STC' AND DocumentNumber = 'TST-CRD-1';
IF @sup = 1 AND @reason LIKE '%dispute%' PRINT 'ok 5 - a wholly disputed item is suppressed from the chase with a reason -- REQ: AR-015, AR-016';
ELSE PRINT 'not ok 5 - suppressed ' + ISNULL(CAST(@sup AS varchar(4)), 'NULL') + ' reason ' + ISNULL(@reason, 'NULL');
SELECT @dec = ExposureGBP FROM ar.vw_CustomerBalance WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCRD1';
IF @dec = 12000.00 PRINT 'ok 6 - the dispute does not reduce credit exposure -- REQ: AR-015, AC: a dispute does not erase debt';
ELSE PRINT 'not ok 6 - exposure after the dispute was ' + ISNULL(CAST(@dec AS varchar(20)), 'NULL');

/*  7 - a partial dispute leaves the undisputed amount collectable. */
UPDATE ar.OpenItem SET DisputedAmountGBP = 4000.00 WHERE CompanyCode = 'STC' AND DocumentNumber = 'TST-CRD-1';
SELECT @sup = IsSuppressed, @dec = UndisputedGBP FROM ar.vw_CollectionWorklist WHERE CompanyCode = 'STC' AND DocumentNumber = 'TST-CRD-1';
IF @sup = 0 AND @dec = 8000.00 PRINT 'ok 7 - a partial dispute still allows the undisputed 8,000.00 to be collected -- REQ: AR-015, AR-016';
ELSE PRINT 'not ok 7 - suppressed ' + ISNULL(CAST(@sup AS varchar(4)), 'NULL') + ' undisputed ' + ISNULL(CAST(@dec AS varchar(20)), 'NULL');

/*  8 - a recorded contact restriction suppresses the step for that customer. */
SELECT @sup = IsSuppressed, @reason = SuppressReason FROM ar.vw_CollectionWorklist WHERE CompanyCode = 'STC' AND DocumentNumber = 'TST-CRD-2';
IF @sup = 1 AND @reason LIKE '%restriction%' PRINT 'ok 8 - a contact restriction suppresses the collection step -- REQ: AR-016, AR-028';
ELSE PRINT 'not ok 8 - suppressed ' + ISNULL(CAST(@sup AS varchar(4)), 'NULL') + ' reason ' + ISNULL(@reason, 'NULL');

/*  9 - an open promise to pay that has not yet fallen due holds the chase off. */
INSERT ar.CollectionCase (CompanyCode, CustomerCode, CaseRef, CaseType, DocumentNumber, Reason, OwnerUser, Status, CreatedBy)
VALUES ('STC','ZZCRD1','TST-CASE-1','COLLECTION','TST-CRD-1','Customer agreed a date to clear the undisputed balance','CREDMGR','OPEN','TEST');
SET @case = SCOPE_IDENTITY();
INSERT ar.CollectionAction (CaseId, ActionSeq, ActionType, ActionDate, PerformedBy, Narrative, PromiseAmountGBP, PromiseDate, PromiseStatus)
VALUES (@case, 1, 'PROMISE', @t, 'CREDMGR', 'Promise of 8,000.00 recorded', 8000.00, DATEADD(day, 5, @t), 'OPEN');
SELECT @sup = IsSuppressed, @reason = SuppressReason FROM ar.vw_CollectionWorklist WHERE CompanyCode = 'STC' AND DocumentNumber = 'TST-CRD-1';
IF @sup = 1 AND @reason LIKE '%Promise%' PRINT 'ok 9 - an open promise to pay suppresses the chase until its date -- REQ: AR-015, AR-016';
ELSE PRINT 'not ok 9 - suppressed ' + ISNULL(CAST(@sup AS varchar(4)), 'NULL') + ' reason ' + ISNULL(@reason, 'NULL');

/*  10 - a settled item leaves the worklist altogether, so a reminder is never sent for it. */
UPDATE ar.OpenItem SET ResidualAmount = 0, ResidualAmountGBP = 0, SettlementStatus = 'SETTLED'
 WHERE CompanyCode = 'STC' AND DocumentNumber = 'TST-CRD-2';
SELECT @n = COUNT(*) FROM ar.vw_CollectionWorklist WHERE CompanyCode = 'STC' AND DocumentNumber = 'TST-CRD-2';
IF @n = 0 PRINT 'ok 10 - an item settled before issue is not chased -- REQ: AR-016, AC: reminders are not sent for settled amounts';
ELSE PRINT 'not ok 10 - a settled item still appears ' + CAST(@n AS varchar(4)) + ' time(s) on the worklist';
ROLLBACK TRANSACTION;

/*  11 - an account placed on hold is reported as held whatever its balance. */
BEGIN TRANSACTION;
INSERT ar.Customer (CompanyCode, CustomerCode, LegalName, CurrencyCode, Status, CreatedBy)
VALUES ('STC', 'ZZCRD3', 'Held Account Ltd', 'GBP', 'ONHOLD', 'TEST');
INSERT ar.CustomerCredit (CompanyCode, CustomerCode, CreditLimitGBP, HoldFlag, HoldReason, HoldSetBy, CreatedBy)
VALUES ('STC', 'ZZCRD3', 25000.00, 1, 'Invoice more than thirty days past due', 'CREDMGR', 'TEST');
SELECT @s = CreditState FROM ar.vw_CustomerBalance WHERE CompanyCode = 'STC' AND CustomerCode = 'ZZCRD3';
IF @s = 'HOLD' PRINT 'ok 11 - an account on hold is reported as held -- REQ: AR-008, D-06';
ELSE PRINT 'not ok 11 - credit state was ' + ISNULL(@s, 'NULL');
ROLLBACK TRANSACTION;
PRINT '# ar-credit done';
GO
