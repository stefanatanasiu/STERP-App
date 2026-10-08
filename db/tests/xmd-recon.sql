/*  Reconciliation tests for the cross-module reporting views: rpt.vw_ControlBalance must describe
    the general ledger control side exactly as the RECONCTL batch does in 1000-LOAD-GL, so that the
    client grid and the 132-column report can never disagree about the same cutoff; rpt.vw_Exception
    Ageing must use the same four buckets EXCPAGE prints; and rpt.usp_ChainTrace must return an
    ordered, gap-free chain.  Every assertion that writes runs inside a transaction that is rolled
    back, and no expected value is hard coded against demo data that other modules keep extending:
    the reference figure is recomputed from the RECONCTL definition itself.
    REQ: XMD-001, XMD-004, XMD-005, XMD-006, XMD-010, XMD-012, FIN-021, RPT-015, RPT-016
    AC: the control balance view and RECONCTL report the same control balance at the same cutoff */
SET NOCOUNT ON;
GO
PRINT '# xmd-recon';
DECLARE @co varchar(10) = 'STC', @cut char(8) = '20261031', @msg varchar(400), @n int, @m int,
        @fy smallint, @pn tinyint, @acc varchar(12), @nonctl varchar(12), @jid bigint,
        @base decimal(18,2), @after decimal(18,2), @all0 decimal(18,2), @all1 decimal(18,2),
        @late decimal(18,2), @famGl int, @famVw int;
DECLARE @gl TABLE (ControlType varchar(10) NOT NULL PRIMARY KEY, BalanceGBP decimal(18,2), Accts int);
DECLARE @vw TABLE (ControlType varchar(10) NOT NULL PRIMARY KEY, BalanceGBP decimal(18,2), Accts int);
DECLARE @ct TABLE (StepNo int, Stage varchar(24), StepKind varchar(10), DocumentType varchar(12),
                   Reference varchar(40), StepDate date, Status varchar(12), Quantity decimal(19,6),
                   AmountGBP decimal(18,2), Note varchar(60));
DECLARE @sum TABLE (CompanyCode varchar(10), DocumentType varchar(12), Reference varchar(40), ChainType varchar(12),
                    ChainName varchar(40), Party varchar(20), StepCount int, OperationalSteps int, FinancialSteps int,
                    Reversals int, TotalGBP decimal(18,2), IsComplete bit, IsBroken bit, IsTruncated bit,
                    ReasonCode varchar(10), Message varchar(200));

/*  The RECONCTL 1000-LOAD-GL statement, copied from cobol\batch\xmd\RECONCTL.cob, is the reference. */
INSERT @gl (ControlType, BalanceGBP, Accts)
SELECT a.ControlType, SUM(l.DebitAmountGBP - l.CreditAmountGBP), COUNT(DISTINCT a.AccountCode)
  FROM fin.JournalLine l
  JOIN fin.Journal j ON j.JournalId = l.JournalId
  JOIN fin.Account a ON a.CompanyCode = l.CompanyCode AND a.AccountCode = l.AccountCode
 WHERE l.CompanyCode = @co AND j.Status = 'POSTED' AND CONVERT(char(8), j.PostingDate, 112) <= @cut
   AND a.ControlType IS NOT NULL AND a.ControlType <> ''
 GROUP BY a.ControlType;
INSERT @vw (ControlType, BalanceGBP, Accts)
SELECT v.ControlType, SUM(v.BalanceGBP), COUNT(DISTINCT v.AccountCode)
  FROM rpt.vw_ControlBalance v
 WHERE v.CompanyCode = @co AND CONVERT(char(8), v.PostingDate, 112) <= @cut
 GROUP BY v.ControlType;
SELECT @famGl = COUNT(*) FROM @gl;
SELECT @famVw = COUNT(*) FROM @vw;

/*  1 - every control family RECONCTL reports for STC at 31 October 2026 is in the view with the
    same sterling balance, and the view invents no family of its own. */
SELECT @n = COUNT(*) FROM @gl g FULL OUTER JOIN @vw v ON v.ControlType = g.ControlType
 WHERE g.ControlType IS NULL OR v.ControlType IS NULL OR g.BalanceGBP <> v.BalanceGBP;
IF @n = 0 AND @famGl > 0 AND @famVw = @famGl
    PRINT 'ok 1 - the control balance view reproduces the RECONCTL general ledger balance for all '
        + CAST(@famGl AS varchar(4)) + ' control families at cutoff 20261031 -- REQ: XMD-005, FIN-021';
ELSE PRINT 'not ok 1 - ' + CAST(@n AS varchar(6)) + ' control family differences, RECONCTL families '
        + CAST(@famGl AS varchar(4)) + ' against view families ' + CAST(@famVw AS varchar(4));

/*  2 - the view carries the control accounts themselves, so the report can name them. */
SELECT @n = COUNT(*) FROM @gl g JOIN @vw v ON v.ControlType = g.ControlType WHERE g.Accts <> v.Accts;
IF @n = 0 AND @famGl > 0
    PRINT 'ok 2 - each family holds the same distinct control accounts as RECONCTL counts -- REQ: XMD-005, XMD-012';
ELSE PRINT 'not ok 2 - ' + CAST(@n AS varchar(6)) + ' families disagree on their control account count';

/*  3 - the presentation sign is RECONCTL''s: creditors, goods received not invoiced, VAT and
    accruals are shown the way the ledger carries them, as credits. */
SELECT @n = COUNT(*) FROM rpt.vw_ControlBalance v
 WHERE v.ReportedBalanceGBP <> v.BalanceGBP * CASE WHEN v.ControlType IN ('AP', 'GRNI', 'VAT', 'ACCRUAL') THEN -1 ELSE 1 END;
IF @n = 0 PRINT 'ok 3 - the reported balance follows the RECONCTL control sign convention -- REQ: XMD-005, XMD-012';
ELSE PRINT 'not ok 3 - ' + CAST(@n AS varchar(6)) + ' rows carry a sign RECONCTL would not print';

/*  4 to 8 - what the cutoff and the posted state actually mean, proved by moving one journal. */
SELECT @fy = FiscalYear, @pn = PeriodNo FROM fin.Period WHERE CompanyCode = @co AND CONVERT(date, @cut, 112) BETWEEN StartDate AND EndDate;
SELECT TOP (1) @acc = AccountCode FROM fin.Account WHERE CompanyCode = @co AND ControlType = 'AR' ORDER BY AccountCode;
SELECT TOP (1) @nonctl = AccountCode FROM fin.Account WHERE CompanyCode = @co AND AccountKind = 'POSTING'
   AND (ControlType IS NULL OR ControlType = '') ORDER BY AccountCode;
BEGIN TRY
    BEGIN TRANSACTION;
    SELECT @base = ISNULL(SUM(BalanceGBP), 0) FROM rpt.vw_ControlBalance
     WHERE CompanyCode = @co AND ControlType = 'AR' AND CONVERT(char(8), PostingDate, 112) <= @cut;
    SELECT @all0 = ISNULL(SUM(BalanceGBP), 0) FROM rpt.vw_ControlBalance
     WHERE CompanyCode = @co AND CONVERT(char(8), PostingDate, 112) <= @cut;
    SELECT @late = ISNULL(SUM(BalanceGBP), 0) FROM rpt.vw_ControlBalance
     WHERE CompanyCode = @co AND ControlType = 'AR' AND CONVERT(char(8), PostingDate, 112) <= '20261130';

    INSERT fin.Journal (CompanyCode, JournalNumber, JournalType, Description, SourceModule, SourceRef, EventCode,
                        DocumentDate, PostingDate, FiscalYear, PeriodNo, CurrencyCode, TotalDebit, TotalCredit,
                        TotalDebitGBP, TotalCreditGBP, Status, CreatedBy)
    VALUES (@co, 'ZZXMDRECON1', 'GL', 'Control balance view test', 'XMD', 'ZZ-XMD-RECON-1', 'XMD.TEST',
            CONVERT(date, @cut, 112), CONVERT(date, @cut, 112), @fy, @pn, 'GBP', 1234.56, 1234.56,
            1234.56, 1234.56, 'DRAFT', 'TEST');
    SET @jid = SCOPE_IDENTITY();
    INSERT fin.JournalLine (JournalId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount,
                            DebitAmountGBP, CreditAmountGBP, CurrencyCode)
    VALUES (@jid, 1, @co, @acc, 1234.56, 0, 1234.56, 0, 'GBP');

    SELECT @after = ISNULL(SUM(BalanceGBP), 0) FROM rpt.vw_ControlBalance
     WHERE CompanyCode = @co AND ControlType = 'AR' AND CONVERT(char(8), PostingDate, 112) <= @cut;
    IF @after = @base PRINT 'ok 4 - an unposted journal is not part of the control balance -- REQ: XMD-005, FIN-021';
    ELSE PRINT 'not ok 4 - a draft journal moved the control balance by ' + CAST(@after - @base AS varchar(24));

    UPDATE fin.Journal SET Status = 'POSTED' WHERE JournalId = @jid;
    SELECT @after = ISNULL(SUM(BalanceGBP), 0) FROM rpt.vw_ControlBalance
     WHERE CompanyCode = @co AND ControlType = 'AR' AND CONVERT(char(8), PostingDate, 112) <= @cut;
    IF @after = @base + 1234.56
        PRINT 'ok 5 - posting the journal moves the control balance by exactly its sterling debit -- REQ: XMD-005, XMD-012';
    ELSE PRINT 'not ok 5 - posting moved the balance by ' + CAST(@after - @base AS varchar(24)) + ' not 1234.56';

    UPDATE fin.Journal SET PostingDate = DATEADD(day, 1, CONVERT(date, @cut, 112)) WHERE JournalId = @jid;
    SELECT @after = ISNULL(SUM(BalanceGBP), 0) FROM rpt.vw_ControlBalance
     WHERE CompanyCode = @co AND ControlType = 'AR' AND CONVERT(char(8), PostingDate, 112) <= @cut;
    SELECT @all1 = ISNULL(SUM(BalanceGBP), 0) FROM rpt.vw_ControlBalance
     WHERE CompanyCode = @co AND ControlType = 'AR' AND CONVERT(char(8), PostingDate, 112) <= '20261130';
    IF @after = @base AND @all1 = @late + 1234.56
        PRINT 'ok 6 - a journal posted after the cutoff is outside it and inside the later one -- REQ: XMD-005, XMD-012';
    ELSE PRINT 'not ok 6 - cutoff drift ' + CAST(@after - @base AS varchar(24)) + ' later cutoff drift '
             + CAST(@all1 - @late AS varchar(24));

    UPDATE fin.Journal SET PostingDate = CONVERT(date, @cut, 112) WHERE JournalId = @jid;
    UPDATE fin.JournalLine SET AccountCode = @nonctl WHERE JournalId = @jid;
    SELECT @after = ISNULL(SUM(BalanceGBP), 0) FROM rpt.vw_ControlBalance
     WHERE CompanyCode = @co AND CONVERT(char(8), PostingDate, 112) <= @cut;
    IF @after = @all0 AND @nonctl IS NOT NULL
        PRINT 'ok 7 - a posting to an account with no control type never reaches the control balance -- REQ: XMD-005';
    ELSE PRINT 'not ok 7 - a non-control posting moved the control balance by ' + CAST(@after - @all0 AS varchar(24));

    UPDATE fin.JournalLine SET AccountCode = @acc WHERE JournalId = @jid;
    UPDATE fin.JournalLine SET CompanyCode = 'CAL' WHERE JournalId = @jid;
    SELECT @after = ISNULL(SUM(BalanceGBP), 0) FROM rpt.vw_ControlBalance
     WHERE CompanyCode = @co AND CONVERT(char(8), PostingDate, 112) <= @cut;
    IF @after = @all0 PRINT 'ok 8 - another company''s control posting stays out of this company''s balance -- REQ: XMD-005, COM-001';
    ELSE PRINT 'not ok 8 - a CAL posting moved the STC control balance by ' + CAST(@after - @all0 AS varchar(24));
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 4 - unexpected error while moving a test journal: ' + @msg;
END CATCH

/*  9 to 12 - the exception ageing buckets are the ones EXCPAGE prints, counted from first
    detection: 0-7, 8-30, 31-60 and 61 days plus.  Reassignment does not reset the age (XMD-006). */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT core.ExceptionItem (CompanyCode, Module, Severity, Category, DocumentType, DocumentRef, Message,
                               AmountGBP, OwnerUser, OwnerRole, Status, RaisedAt)
    VALUES (@co, 'XMD', 'WARNING', 'TEST', 'TEST', 'ZZAGE-07', 'Seven days old', 100, 'FINCTRL', NULL, 'OPEN', DATEADD(day, -7, CAST(GETDATE() AS date))),
           (@co, 'XMD', 'WARNING', 'TEST', 'TEST', 'ZZAGE-08', 'Eight days old', 200, 'FINCTRL', NULL, 'OPEN', DATEADD(day, -8, CAST(GETDATE() AS date))),
           (@co, 'XMD', 'WARNING', 'TEST', 'TEST', 'ZZAGE-30', 'Thirty days old', 300, 'FINCTRL', NULL, 'OPEN', DATEADD(day, -30, CAST(GETDATE() AS date))),
           (@co, 'XMD', 'WARNING', 'TEST', 'TEST', 'ZZAGE-31', 'Thirty one days old', 400, 'FINCTRL', NULL, 'OPEN', DATEADD(day, -31, CAST(GETDATE() AS date))),
           (@co, 'XMD', 'WARNING', 'TEST', 'TEST', 'ZZAGE-60', 'Sixty days old', 500, 'FINCTRL', NULL, 'OPEN', DATEADD(day, -60, CAST(GETDATE() AS date))),
           (@co, 'XMD', 'HOLD',    'TEST', 'TEST', 'ZZAGE-61', 'Sixty one days old', 600, NULL, NULL, 'OPEN', DATEADD(day, -61, CAST(GETDATE() AS date)));
    SELECT @n = COUNT(*) FROM rpt.vw_ExceptionAgeing
     WHERE CompanyCode = @co AND DocumentRef LIKE 'ZZAGE-%'
       AND BucketNo = CASE DocumentRef WHEN 'ZZAGE-07' THEN 1 WHEN 'ZZAGE-08' THEN 2 WHEN 'ZZAGE-30' THEN 2
                                       WHEN 'ZZAGE-31' THEN 3 WHEN 'ZZAGE-60' THEN 3 ELSE 4 END;
    IF @n = 6 PRINT 'ok 9 - exceptions age into the EXCPAGE buckets 0-7, 8-30, 31-60 and 61 plus -- REQ: XMD-006, RPT-016';
    ELSE PRINT 'not ok 9 - only ' + CAST(@n AS varchar(4)) + ' of 6 exceptions landed in the expected bucket';

    SELECT @n = COUNT(*) FROM rpt.vw_ExceptionAgeing
     WHERE CompanyCode = @co AND DocumentRef LIKE 'ZZAGE-%'
       AND BucketName = CASE BucketNo WHEN 1 THEN '0-7 days' WHEN 2 THEN '8-30 days' WHEN 3 THEN '31-60 days' ELSE '61 days plus' END
       AND Count0007 + Count0830 + Count3160 + Count61Up = 1
       AND Value0007GBP + Value0830GBP + Value3160GBP + Value61UpGBP = AmountGBP;
    IF @n = 6 PRINT 'ok 10 - every exception is counted once, in one bucket, for its own value -- REQ: XMD-006, XMD-012';
    ELSE PRINT 'not ok 10 - ' + CAST(6 - @n AS varchar(4)) + ' exceptions are double counted or misnamed';

    SELECT @n = AgeDays FROM rpt.vw_ExceptionAgeing WHERE CompanyCode = @co AND DocumentRef = 'ZZAGE-61';
    UPDATE core.ExceptionItem SET OwnerUser = 'SYSADM' WHERE CompanyCode = @co AND DocumentRef = 'ZZAGE-61';
    SELECT @m = AgeDays FROM rpt.vw_ExceptionAgeing WHERE CompanyCode = @co AND DocumentRef = 'ZZAGE-61';
    IF @n = 61 AND @m = 61 PRINT 'ok 11 - reassigning an exception does not reset its age -- REQ: XMD-006';
    ELSE PRINT 'not ok 11 - age before reassignment ' + ISNULL(CAST(@n AS varchar(6)), 'NULL') + ' after ' + ISNULL(CAST(@m AS varchar(6)), 'NULL');

    SELECT @n = COUNT(*) FROM rpt.vw_ExceptionAgeing
     WHERE CompanyCode = @co AND DocumentRef = 'ZZAGE-61' AND Owner = '(UNOWNED)' AND IsUnowned = 1 AND IsHold = 1;
    UPDATE core.ExceptionItem SET OwnerUser = NULL WHERE CompanyCode = @co AND DocumentRef = 'ZZAGE-61';
    SELECT @m = COUNT(*) FROM rpt.vw_ExceptionAgeing
     WHERE CompanyCode = @co AND DocumentRef = 'ZZAGE-61' AND Owner = '(UNOWNED)' AND IsUnowned = 1 AND IsHold = 1;
    IF @n = 0 AND @m = 1 PRINT 'ok 12 - an exception with no owner is reported as unowned, not omitted -- REQ: XMD-006, XMD-009';
    ELSE PRINT 'not ok 12 - the unowned hold was reported ' + CAST(@m AS varchar(4)) + ' time(s)';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 9 - unexpected error while ageing test exceptions: ' + @msg;
END CATCH

/*  13 to 15 - the chain trace proc: an ordered gap-free chain, a stated break, a refused type. */
BEGIN TRY
    DELETE @ct;
    INSERT @ct EXEC rpt.usp_ChainTrace @CompanyCode = @co, @DocType = 'ORDER',
        @Reference = 'ZZ-NO-SUCH-ORDER', @StepsOnly = 1;
    DELETE @sum;
    INSERT @sum EXEC rpt.usp_ChainTrace @CompanyCode = @co, @DocType = 'ORDER',
        @Reference = 'ZZ-NO-SUCH-ORDER', @SummaryOnly = 1;
    SELECT @n = COUNT(*) FROM @ct;
    IF @n = 0
       AND EXISTS (SELECT 1 FROM @sum WHERE IsBroken = 1 AND IsComplete = 0 AND StepCount = 0 AND ReasonCode = 'XMD0016')
        PRINT 'ok 13 - an unknown reference is reported as a broken chain, not as an empty success -- REQ: XMD-012';
    ELSE PRINT 'not ok 13 - the unknown reference returned ' + CAST(@n AS varchar(6)) + ' step(s)';
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    PRINT 'not ok 13 - unexpected error: ' + @msg;
END CATCH

BEGIN TRY
    DELETE @ct;
    SELECT TOP (1) @msg = d.OrderNumber FROM so.DeliverySchedule d WHERE d.CompanyCode = @co ORDER BY d.OrderNumber;
    INSERT @ct EXEC rpt.usp_ChainTrace @CompanyCode = @co, @DocType = 'ORDER', @Reference = @msg, @StepsOnly = 1;
    SELECT @n = COUNT(*), @m = COUNT(DISTINCT StepNo) FROM @ct;
    DECLARE @lo int, @hi int;
    SELECT @lo = MIN(StepNo), @hi = MAX(StepNo) FROM @ct;
    IF @n > 0 AND @n = @m AND @lo = 1 AND @hi = @n
       AND EXISTS (SELECT 1 FROM @ct WHERE StepNo = 1 AND StepKind = 'OPERATION' AND DocumentType = 'ORDER')
        PRINT 'ok 14 - the chain trace numbers its steps from one, without gaps, operations first -- REQ: XMD-001, XMD-003';
    ELSE PRINT 'not ok 14 - ' + CAST(@n AS varchar(6)) + ' steps with ' + CAST(@m AS varchar(6)) + ' distinct numbers for ' + ISNULL(@msg, 'NULL');
    SELECT @n = COUNT(*) FROM @ct WHERE StepKind = 'REVERSAL' AND DocumentType <> 'JOURNAL';
    IF @n = 0 PRINT 'ok 15 - a reversal or credit is listed as its own step, never folded into another -- REQ: XMD-004';
    ELSE PRINT 'not ok 15 - ' + CAST(@n AS varchar(6)) + ' reversals were attached to an operational step';
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    PRINT 'not ok 14 - unexpected error: ' + @msg;
END CATCH

BEGIN TRY
    DELETE @ct;
    INSERT @ct EXEC rpt.usp_ChainTrace @CompanyCode = @co, @DocType = 'INVOICE', @Reference = 'ANY', @StepsOnly = 1;
    PRINT 'not ok 16 - a chain type the system cannot trace was accepted';
END TRY
BEGIN CATCH
    IF ERROR_MESSAGE() LIKE '%STERP-E-0803%'
        PRINT 'ok 16 - a document type that starts no chain is refused with a stated reason -- REQ: XMD-001, COM-020';
    ELSE PRINT 'not ok 16 - the wrong error was raised: ' + ERROR_MESSAGE();
END CATCH
GO

/*  CR2611 - one definition per control account.  The debtors control is proved by three
    programs (RECONCTL, ARRECON, AGEDDEBT) and levelled by fin.usp_DemoBridgeControls; the
    stock control by two (RECONCTL and CSTINVAL through cst.usp_ValuationRecon).  These
    assertions pin the definitions together so that a change to one has to change the others.
    REQ: XMD-002, XMD-005, AR-021, AR-026, CST-022 */
DECLARE @co2 varchar(10) = 'STC', @cut2 char(8) = '20261031', @n2 int,
        @orig decimal(18,2), @outst decimal(18,2), @allow decimal(18,2), @glAr decimal(18,2),
        @invSub decimal(18,2), @invGl decimal(18,2);
DECLARE @vr TABLE (CompanyCode varchar(10), AsAt date, AccountCode varchar(20),
                   PoolCarryingGBP decimal(18,2), AllowanceGBP decimal(18,2),
                   NetInventoryGBP decimal(18,2), FifoLayerGBP decimal(18,2),
                   LedgerBalanceGBP decimal(18,2), DifferenceGBP decimal(18,2),
                   ReconciledFlag char(1), LayerDifferenceGBP decimal(18,2),
                   UnpostedLedgerGBP decimal(18,2), OpenCostExceptions int);

/*  The ARRECON 1000-SUBLEDGER / AGEDDEBT 1000 residual, copied from the COBOL. */
SELECT @outst = ISNULL(SUM(t.Resid), 0) FROM (
    SELECT o.OriginalAmountGBP
         - ISNULL((SELECT SUM(a.AppliedAmountGBP + a.DiscountAmountGBP + a.WriteOffAmountGBP)
                     FROM ar.Allocation a WHERE a.TargetOpenItemId = o.OpenItemId
                      AND a.Status = 'ACTIVE' AND a.EffectiveDate <= @cut2), 0)
         + ISNULL((SELECT SUM(b.AppliedAmountGBP)
                     FROM ar.Allocation b WHERE b.SourceOpenItemId = o.OpenItemId
                      AND b.Status = 'ACTIVE' AND b.EffectiveDate <= @cut2), 0) AS Resid
      FROM ar.OpenItem o
     WHERE o.CompanyCode = @co2 AND CONVERT(char(8), o.PostingDate, 112) <= @cut2
       AND o.SettlementStatus <> 'REVERSED') t;
SELECT @orig = ISNULL(SUM(o.OriginalAmountGBP), 0) FROM ar.OpenItem o
 WHERE o.CompanyCode = @co2 AND CONVERT(char(8), o.PostingDate, 112) <= @cut2
   AND o.SettlementStatus <> 'REVERSED';
SELECT @allow = ISNULL(SUM(a.DiscountAmountGBP + a.WriteOffAmountGBP), 0)
  FROM ar.Allocation a JOIN ar.OpenItem o ON o.OpenItemId = a.TargetOpenItemId
 WHERE o.CompanyCode = @co2 AND a.Status = 'ACTIVE' AND a.EffectiveDate <= @cut2
   AND CONVERT(char(8), o.PostingDate, 112) <= @cut2 AND o.SettlementStatus <> 'REVERSED';
SELECT @glAr = ISNULL(SUM(l.DebitAmountGBP - l.CreditAmountGBP), 0)
  FROM fin.JournalLine l
  JOIN fin.Journal j ON j.JournalId = l.JournalId
  JOIN fin.Account a ON a.CompanyCode = l.CompanyCode AND a.AccountCode = l.AccountCode
 WHERE l.CompanyCode = @co2 AND j.Status = 'POSTED'
   AND CONVERT(char(8), j.PostingDate, 112) <= @cut2 AND a.ControlType = 'AR';

/*  17 - a settlement discount or a write off allowed on an allocation leaves the customer
    owing less, so the sales ledger balance is the outstanding amount and not the sum of the
    documents as raised.  The two definitions differ by exactly the allowances given. */
IF ABS((@orig - @allow) - @outst) < 0.005
    PRINT 'ok 17 - the sales ledger balance is the outstanding amount, below the documents as raised by the '
        + CONVERT(varchar(24), @allow) + ' of discounts and write offs allowed -- REQ: AR-021, XMD-002';
ELSE PRINT 'not ok 17 - outstanding ' + CONVERT(varchar(24), @outst) + ' is not originals '
        + CONVERT(varchar(24), @orig) + ' less allowances ' + CONVERT(varchar(24), @allow);

/*  18 - RECONCTL, ARRECON, AGEDDEBT and the bridge all measure that one figure, so the
    debtors control account carries it.  A dataset levelled by fin.usp_DemoBridgeControls
    agrees; a difference here is the two reconciliations disagreeing with each other. */
IF ABS(@outst - @glAr) < 0.005
    PRINT 'ok 18 - the debtors control account holds the outstanding sales ledger at cutoff 20261031 -- REQ: AR-026, XMD-005';
ELSE PRINT 'not ok 18 - sales ledger ' + CONVERT(varchar(24), @outst) + ' against control account '
        + CONVERT(varchar(24), @glAr);

/*  19 - stock is worth what is on hand at cost.  RECONCTL 2130-SQL-INV and CSTINVAL (through
    cst.usp_ValuationRecon) must read the same costing pool, and a stock valuation is never
    negative - the movement history on its own made it so. */
SELECT @invSub = ISNULL(SUM(ic.CarryingValueGBP - ISNULL(ic.AllowanceGBP, 0)), 0)
  FROM cst.ItemCost ic WHERE ic.CompanyCode = @co2;
INSERT @vr EXEC cst.usp_ValuationRecon @CompanyCode = @co2, @AsAt = '2026-10-31';
SELECT @n2 = COUNT(*) FROM @vr WHERE ABS(NetInventoryGBP - @invSub) >= 0.005;
IF @n2 = 0 AND @invSub >= 0
    PRINT 'ok 19 - RECONCTL and CSTINVAL value stock from the same costing pool, '
        + CONVERT(varchar(24), @invSub) + ', and it is not negative -- REQ: CST-022, XMD-005';
ELSE PRINT 'not ok 19 - the valuation reconciliation reports a different, or negative, stock value than '
        + CONVERT(varchar(24), @invSub);

/*  20 - and against the same accounts: every account the chart of accounts marks INVENTORY,
    by posting date, which is how RECONCTL groups the general ledger side. */
SELECT @invGl = ISNULL(SUM(l.DebitAmountGBP - l.CreditAmountGBP), 0)
  FROM fin.JournalLine l
  JOIN fin.Journal j ON j.JournalId = l.JournalId
  JOIN fin.Account a ON a.CompanyCode = l.CompanyCode AND a.AccountCode = l.AccountCode
 WHERE l.CompanyCode = @co2 AND j.Status = 'POSTED'
   AND CONVERT(char(8), j.PostingDate, 112) <= @cut2 AND a.ControlType = 'INVENTORY';
SELECT @n2 = COUNT(*) FROM @vr WHERE ABS(LedgerBalanceGBP - @invGl) >= 0.005;
IF @n2 = 0
    PRINT 'ok 20 - the valuation reconciliation reads every INVENTORY control account by posting date, '
        + CONVERT(varchar(24), @invGl) + ' -- REQ: CST-022, XMD-012';
ELSE PRINT 'not ok 20 - the valuation ledger side does not match the RECONCTL INVENTORY grouping '
        + CONVERT(varchar(24), @invGl);
GO
