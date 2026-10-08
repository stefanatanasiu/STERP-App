/*  The reconciliation and enquiry rules of costing that the other cst tests do not reach.

    CST-022  The valuation is agreed to the ledger at one cut off, and only POSTED journals
             form the ledger side of that agreement.  Journals still in flight and unresolved
             costing exceptions are reported beside the reconciliation, in their own right,
             never folded into the posted balance - a reconciliation that quietly counts a
             draft journal is a reconciliation that hides a difference.
    CST-001  A cost set states what it is for and what state it is in, so a quote estimate can
             never be read back as an actual inventory value, and cst.usp_CostEnquiry shows
             every basis side by side with the set a standard came from.

    Every fixture is prefixed ZZCGAP / ZZ-CST-GAP- and is built inside a transaction that is
    rolled back, so the seeded demonstration costs are left exactly as they were found.

    REQ: CST-001, CST-022  AC: AC-059, AC-067 */
SET NOCOUNT ON;
GO
PRINT '# cst-gaps';
/*  Re-runnable: clear anything an aborted earlier run could have left behind. */
DELETE FROM fin.JournalLine WHERE JournalId IN
       (SELECT JournalId FROM fin.Journal WHERE CompanyCode = 'STC' AND JournalNumber LIKE 'ZZCGAP%');
DELETE FROM fin.Journal WHERE CompanyCode = 'STC' AND JournalNumber LIKE 'ZZCGAP%';
DELETE FROM core.ExceptionItem WHERE CompanyCode = 'STC' AND DocumentRef LIKE 'ZZCGAP%';
DELETE FROM cst.CostLayer WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-CST-GAP%';
DELETE FROM cst.ItemCost WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-CST-GAP%';
DELETE FROM cst.StandardCost WHERE CompanyCode = 'STC' AND CostSetCode LIKE 'ZZCGAPSET%';
DELETE FROM cst.CostSet WHERE CompanyCode = 'STC' AND CostSetCode LIKE 'ZZCGAPSET%';
DELETE FROM inv.Item WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-CST-GAP-%';
GO
DECLARE @msg varchar(400), @jid bigint, @n int, @s varchar(40), @item varchar(40),
        @ledger0 decimal(18,2), @unposted0 decimal(18,2), @exc0 int, @pool0 decimal(18,2),
        @allow0 decimal(18,2), @pool2 decimal(18,2), @diff1 decimal(18,2),
        @ledger1 decimal(18,2), @unposted1 decimal(18,2), @exc1 int, @pool1 decimal(18,2),
        @allow1 decimal(18,2), @net1 decimal(18,2), @flag varchar(1);

/*  The reconciliation result set.  Columns 12 and 13 are the separately visible ones. */
DECLARE @rec TABLE (CompanyCode varchar(10), AsAt date, AccountCode varchar(20),
                    PoolCarryingGBP decimal(18,2), AllowanceGBP decimal(18,2), NetInventoryGBP decimal(18,2),
                    FifoLayerGBP decimal(18,2), LedgerBalanceGBP decimal(18,2), DifferenceGBP decimal(18,2),
                    ReconciledFlag varchar(1), LayerDifferenceGBP decimal(18,2),
                    UnpostedLedgerGBP decimal(18,2), OpenCostExceptions int);

DECLARE @enq TABLE (ItemCode varchar(40), Revision varchar(4), SiteCode varchar(10), CostMethod varchar(4),
                    QtyOnHand decimal(28,6), AverageUnitCost decimal(28,6), CarryingValueGBP decimal(28,6),
                    AllowanceGBP decimal(28,6), NetCarryingGBP decimal(28,6), NrvUnitValue decimal(28,6),
                    StandardUnitCost decimal(28,6), LastPurchaseCost decimal(28,6),
                    FifoOldestUnit decimal(28,6), FifoLayers int, StandardSet varchar(20),
                    StandardFromSet decimal(28,6), IsImpaired varchar(1));

/*  ---------------------------------------------------------------------------------------
    CST-022 - the ledger side of the reconciliation.
    --------------------------------------------------------------------------------------- */
BEGIN TRY
    BEGIN TRANSACTION;

    DELETE @rec;
    INSERT @rec EXEC cst.usp_ValuationRecon @CompanyCode = 'STC', @AsAt = '2026-12-31', @AccountCode = '1100';
    SELECT @ledger0 = LedgerBalanceGBP, @unposted0 = UnpostedLedgerGBP, @exc0 = OpenCostExceptions,
           @pool0 = PoolCarryingGBP FROM @rec;

    IF @ledger0 IS NOT NULL AND @unposted0 IS NOT NULL AND @exc0 IS NOT NULL
        PRINT 'ok 1 - the reconciliation reports the ledger, the unposted and the exceptions -- REQ: CST-022';
    ELSE PRINT 'not ok 1 - the reconciliation does not report all three figures -- REQ: CST-022';

    /*  a journal that nobody has posted: real intent, no ledger effect */
    INSERT fin.Journal (CompanyCode, JournalNumber, JournalType, Description, SourceModule, DocumentDate,
            PostingDate, FiscalYear, PeriodNo, TotalDebit, TotalCredit, TotalDebitGBP, TotalCreditGBP,
            Status, PreparedBy, CreatedBy)
    VALUES ('STC', 'ZZCGAP-DRAFT', 'GL', 'Gap test unposted stock adjustment', 'CST', '2026-10-15',
            '2026-10-15', 2026, 7, 1234.56, 1234.56, 1234.56, 1234.56, 'DRAFT', 'COSTACC', 'TEST');
    SET @jid = SCOPE_IDENTITY();
    INSERT fin.JournalLine (JournalId, LineNum, CompanyCode, AccountCode, DebitAmount, CreditAmount,
            DebitAmountGBP, CreditAmountGBP, Narrative)
    VALUES (@jid, 1, 'STC', '1100', 1234.56, 0, 1234.56, 0, 'Gap test stock debit'),
           (@jid, 2, 'STC', '5100', 0, 1234.56, 0, 1234.56, 'Gap test contra');

    DELETE @rec;
    INSERT @rec EXEC cst.usp_ValuationRecon @CompanyCode = 'STC', @AsAt = '2026-12-31', @AccountCode = '1100';
    SELECT @ledger1 = LedgerBalanceGBP, @unposted1 = UnpostedLedgerGBP FROM @rec;

    IF @ledger1 = @ledger0
        PRINT 'ok 2 - an unposted journal does not move the posted ledger balance -- REQ: CST-022  AC: AC-067';
    ELSE PRINT 'not ok 2 - the ledger balance moved by '
               + CONVERT(varchar(20), @ledger1 - @ledger0) + ' for an unposted journal -- REQ: CST-022';

    IF @unposted1 - @unposted0 = 1234.56
        PRINT 'ok 3 - the unposted journal is separately visible at 1234.56 -- REQ: CST-022';
    ELSE PRINT 'not ok 3 - the unposted figure moved by '
               + ISNULL(CONVERT(varchar(20), @unposted1 - @unposted0), 'NULL') + ' -- REQ: CST-022';

    /*  the controller posts it: now and only now is it part of the agreement */
    UPDATE fin.Journal SET Status = 'POSTED', PostedBy = 'FINCTRL', PostedAt = SYSDATETIME()
     WHERE JournalId = @jid;

    DELETE @rec;
    INSERT @rec EXEC cst.usp_ValuationRecon @CompanyCode = 'STC', @AsAt = '2026-12-31', @AccountCode = '1100';
    SELECT @ledger1 = LedgerBalanceGBP, @unposted1 = UnpostedLedgerGBP FROM @rec;

    IF @ledger1 - @ledger0 = 1234.56
        PRINT 'ok 4 - posting the journal moves the ledger balance by 1234.56 -- REQ: CST-022';
    ELSE PRINT 'not ok 4 - the posted journal moved the balance by '
               + ISNULL(CONVERT(varchar(20), @ledger1 - @ledger0), 'NULL') + ' -- REQ: CST-022';

    IF @unposted1 = @unposted0
        PRINT 'ok 5 - once posted it is no longer counted as unposted -- REQ: CST-022';
    ELSE PRINT 'not ok 5 - a posted journal is still being reported as unposted -- REQ: CST-022';

    /*  the same cut off on both sides: a journal dated after it is not in the balance */
    DELETE @rec;
    INSERT @rec EXEC cst.usp_ValuationRecon @CompanyCode = 'STC', @AsAt = '2026-09-30', @AccountCode = '1100';
    SELECT @n = CASE WHEN LedgerBalanceGBP = @ledger1 THEN 1 ELSE 0 END FROM @rec;
    IF @n = 0
        PRINT 'ok 6 - a journal dated after the cut off is outside the reconciliation -- REQ: CST-022';
    ELSE PRINT 'not ok 6 - the cut off was ignored on the ledger side -- REQ: CST-022';

    /*  an unresolved costing exception is a hole in the valuation and is counted as one */
    INSERT core.ExceptionItem (CompanyCode, SiteCode, Module, Severity, Category, DocumentType, DocumentRef,
            Message, OwnerUser, RaisedBy, Status)
    VALUES ('STC', 'RDG', 'CST', 'ERROR', 'VALUATION', 'ITEM', 'ZZCGAP-EXC-1',
            'Gap test: no cost is held for a stocked item', 'COSTACC', 'TEST', 'OPEN');

    DELETE @rec;
    INSERT @rec EXEC cst.usp_ValuationRecon @CompanyCode = 'STC', @AsAt = '2026-12-31', @AccountCode = '1100';
    SELECT @exc1 = OpenCostExceptions FROM @rec;
    IF @exc1 - @exc0 = 1
        PRINT 'ok 7 - an unresolved costing exception is separately visible -- REQ: CST-022  AC: AC-067';
    ELSE PRINT 'not ok 7 - the open exception count moved by '
               + ISNULL(CONVERT(varchar(20), @exc1 - @exc0), 'NULL') + ' -- REQ: CST-022';

    /*  resolving it takes it out of the count, so the figure means what it says */
    UPDATE core.ExceptionItem SET Status = 'RESOLVED', ResolvedBy = 'COSTACC', ResolvedAt = SYSDATETIME()
     WHERE CompanyCode = 'STC' AND DocumentRef = 'ZZCGAP-EXC-1';
    DELETE @rec;
    INSERT @rec EXEC cst.usp_ValuationRecon @CompanyCode = 'STC', @AsAt = '2026-12-31', @AccountCode = '1100';
    SELECT @exc1 = OpenCostExceptions FROM @rec;
    IF @exc1 = @exc0
        PRINT 'ok 8 - a resolved exception leaves the open count -- REQ: CST-022';
    ELSE PRINT 'not ok 8 - a resolved exception is still counted as open -- REQ: CST-022';

    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 1 to 8 - unexpected error in the reconciliation tests: ' + @msg + ' -- REQ: CST-022';
END CATCH

/*  9 to 13 - the costing side of the same cut off: the write down is shown beside the gross
    carrying value rather than buried in it, and the difference to the ledger is stated. */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE @rec;
    INSERT @rec EXEC cst.usp_ValuationRecon @CompanyCode = 'STC', @AsAt = '2026-12-31', @AccountCode = '1100';
    SELECT @pool0 = PoolCarryingGBP, @allow0 = AllowanceGBP FROM @rec;

    INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, QuantumQty, IsIndivisible,
            TraceabilityType, MakeBuy, ValuationGroup, CostMethod, LifecycleStatus, IsManufacturable, CreatedBy)
    VALUES ('STC', 'ZZ-CST-GAP-1', 'Gap test written down part', 'BOUGHT', 'EA', 1, 1, 'NONE', 'BUY',
            'BOUGHT', 'MWA', 'RELEASED', 0, 'TEST');
    INSERT cst.ItemCost (CompanyCode, ItemCode, Revision, SiteCode, CostMethod, QtyOnHand,
            CarryingValueGBP, AverageUnitCost, NrvUnitValue, AllowanceGBP, LastUpdatedBy)
    VALUES ('STC', 'ZZ-CST-GAP-1', '', '*', 'MWA', 100, 1000.00, 10.000000, 7.000000, 300.00, 'TEST');

    DELETE @rec;
    INSERT @rec EXEC cst.usp_ValuationRecon @CompanyCode = 'STC', @AsAt = '2026-12-31', @AccountCode = '1100';
    SELECT @pool1 = PoolCarryingGBP, @allow1 = AllowanceGBP, @net1 = NetInventoryGBP,
           @ledger1 = LedgerBalanceGBP, @diff1 = DifferenceGBP, @flag = ReconciledFlag FROM @rec;

    IF @pool1 - @pool0 = 1000.00 AND @allow1 - @allow0 = 300.00
        PRINT 'ok 9 - the write down is shown beside the gross value, not inside it -- REQ: CST-022  AC: AC-067';
    ELSE PRINT 'not ok 9 - gross moved ' + ISNULL(CONVERT(varchar(20), @pool1 - @pool0), 'NULL')
               + ' allowance moved ' + ISNULL(CONVERT(varchar(20), @allow1 - @allow0), 'NULL') + ' -- REQ: CST-022';

    IF @net1 = @pool1 - @allow1
        PRINT 'ok 10 - the net inventory carried is the gross less the allowance -- REQ: CST-022';
    ELSE PRINT 'not ok 10 - the net inventory is not the gross less the allowance -- REQ: CST-022';

    /*  the difference is stated, not left for someone to find */
    IF @diff1 = @net1 - @ledger1
        PRINT 'ok 11 - the reconciliation states the difference to the posted ledger -- REQ: CST-022';
    ELSE PRINT 'not ok 11 - the stated difference does not tie to the two sides -- REQ: CST-022';

    IF (@flag = 'Y' AND ABS(@diff1) < 0.01) OR (@flag = 'N' AND ABS(@diff1) >= 0.01)
        PRINT 'ok 12 - the agreement flag follows the difference at the cut off -- REQ: CST-022  AC: AC-067';
    ELSE PRINT 'not ok 12 - the agreement flag says ' + @flag + ' for a difference of '
               + ISNULL(CONVERT(varchar(20), @diff1), 'NULL') + ' -- REQ: CST-022';

    /*  an estimate written into a draft cost set is not part of the valuation at all */
    INSERT cst.CostSet (CompanyCode, CostSetCode, Name, Purpose, EffectiveFrom, CurrencyCode, Status, CreatedBy)
    VALUES ('STC', 'ZZCGAPSET-E', 'Gap test quotation estimate', 'QUOTE', '2027-12-01', 'GBP', 'DRAFT', 'TEST');
    INSERT cst.StandardCost (CompanyCode, CostSetCode, ItemCode, Revision, CostingLotSize, TotalUnitCost)
    VALUES ('STC', 'ZZCGAPSET-E', 'ZZ-CST-GAP-1', '', 1, 99.000000);
    DELETE @rec;
    INSERT @rec EXEC cst.usp_ValuationRecon @CompanyCode = 'STC', @AsAt = '2026-12-31', @AccountCode = '1100';
    SELECT @pool2 = PoolCarryingGBP FROM @rec;
    IF @pool2 = @pool1
        PRINT 'ok 13 - a quoted estimate does not move the posted valuation -- REQ: CST-022, CST-001';
    ELSE PRINT 'not ok 13 - a quotation changed the valuation by '
               + ISNULL(CONVERT(varchar(20), @pool2 - @pool1), 'NULL') + ' -- REQ: CST-022';

    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 9 to 13 - unexpected error in the valuation tests: ' + @msg + ' -- REQ: CST-022';
END CATCH

/*  ---------------------------------------------------------------------------------------
    CST-001 - a cost always says what it is for and what state it is in.
    --------------------------------------------------------------------------------------- */
BEGIN TRY
    BEGIN TRANSACTION;
    SET @s = 'N';
    BEGIN TRY
        INSERT cst.CostSet (CompanyCode, CostSetCode, Name, Purpose, EffectiveFrom, CurrencyCode, Status, CreatedBy)
        VALUES ('STC', 'ZZCGAPSET-X', 'Gap test set with no stated purpose', 'GUESS', '2026-01-01', 'GBP',
                'DRAFT', 'TEST');
    END TRY
    BEGIN CATCH
        SET @s = 'Y';
    END CATCH
    IF @s = 'Y'
        PRINT 'ok 14 - a cost set must state a governed purpose -- REQ: CST-001  AC: AC-059';
    ELSE PRINT 'not ok 14 - a cost set was written without a governed purpose -- REQ: CST-001';

    SET @s = 'N';
    BEGIN TRY
        INSERT cst.CostSet (CompanyCode, CostSetCode, Name, Purpose, EffectiveFrom, CurrencyCode, Status, CreatedBy)
        VALUES ('STC', 'ZZCGAPSET-Y', 'Gap test set in no stated state', 'QUOTE', '2026-01-01', 'GBP',
                'LIVE', 'TEST');
    END TRY
    BEGIN CATCH
        SET @s = 'Y';
    END CATCH
    IF @s = 'Y'
        PRINT 'ok 15 - a cost set must state a governed approval state -- REQ: CST-001';
    ELSE PRINT 'not ok 15 - a cost set was written with an ungoverned state -- REQ: CST-001';
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 14 to 15 - unexpected error: ' + @msg + ' -- REQ: CST-001';
END CATCH

/*  16 to 19 - a quote estimate is never read back as the inventory value.  A quote set and an
    active standard set both hold a price for the same item; the enquiry reports the standard
    one and names the set it came from, and the quote is nowhere in the carrying value. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, QuantumQty, IsIndivisible,
            TraceabilityType, MakeBuy, ValuationGroup, CostMethod, LifecycleStatus, IsManufacturable, CreatedBy)
    VALUES ('STC', 'ZZ-CST-GAP-2', 'Gap test quoted part', 'BOUGHT', 'EA', 1, 1, 'NONE', 'BUY',
            'BOUGHT', 'MWA', 'RELEASED', 0, 'TEST');
    /*  the approved set is dated latest, so it is the active one the enquiry resolves to
        without disturbing any set the seeded demonstration data relies on */
    INSERT cst.CostSet (CompanyCode, CostSetCode, Name, Purpose, EffectiveFrom, CurrencyCode, Status,
            ApprovedBy, ApprovedAt, CreatedBy)
    VALUES ('STC', 'ZZCGAPSET-S', 'Gap test approved standard set', 'STANDARD', '2027-12-01', 'GBP',
            'ACTIVE', 'FINCTRL', SYSDATETIME(), 'TEST'),
           ('STC', 'ZZCGAPSET-Q', 'Gap test quotation set', 'QUOTE', '2027-12-01', 'GBP',
            'DRAFT', NULL, NULL, 'TEST');
    INSERT cst.StandardCost (CompanyCode, CostSetCode, ItemCode, Revision, CostingLotSize, TotalUnitCost)
    VALUES ('STC', 'ZZCGAPSET-S', 'ZZ-CST-GAP-2', '', 1, 12.000000),
           ('STC', 'ZZCGAPSET-Q', 'ZZ-CST-GAP-2', '', 1, 99.000000);
    INSERT cst.ItemCost (CompanyCode, ItemCode, Revision, SiteCode, CostMethod, QtyOnHand,
            CarryingValueGBP, AverageUnitCost, StandardUnitCost, LastUpdatedBy)
    VALUES ('STC', 'ZZ-CST-GAP-2', '', '*', 'MWA', 10, 115.00, 11.500000, 12.000000, 'TEST');

    DELETE @enq;
    INSERT @enq EXEC cst.usp_CostEnquiry @CompanyCode = 'STC', @ItemCode = 'ZZ-CST-GAP-2',
           @Revision = '', @SiteCode = '*';

    SELECT @s = StandardSet, @n = CASE WHEN StandardFromSet = 12.000000 THEN 1 ELSE 0 END FROM @enq;
    IF @s = 'ZZCGAPSET-S'
        PRINT 'ok 16 - the enquiry names the cost set the standard came from -- REQ: CST-001  AC: AC-059';
    ELSE PRINT 'not ok 16 - the standard was taken from ' + ISNULL(@s, 'nowhere') + ' -- REQ: CST-001';

    IF @n = 1
        PRINT 'ok 17 - the quoted 99.00 is not offered as the approved standard -- REQ: CST-001';
    ELSE PRINT 'not ok 17 - the standard cost shown is not the approved one -- REQ: CST-001';

    SELECT @n = COUNT(*) FROM @enq
     WHERE AverageUnitCost IS NOT NULL AND CarryingValueGBP IS NOT NULL AND CostMethod IS NOT NULL
       AND SiteCode IS NOT NULL AND IsImpaired IS NOT NULL;
    IF @n = 1
        PRINT 'ok 18 - every basis is shown side by side with its unit and method -- REQ: CST-001';
    ELSE PRINT 'not ok 18 - the enquiry does not state the basis of what it shows -- REQ: CST-001';

    SELECT @pool1 = CarryingValueGBP, @net1 = NetCarryingGBP FROM @enq;
    IF @pool1 = 115.00 AND @net1 = 115.00
        PRINT 'ok 19 - the inventory value is the carrying value, not the quote -- REQ: CST-001, CST-022';
    ELSE PRINT 'not ok 19 - the carrying value came to '
               + ISNULL(CONVERT(varchar(20), @pool1), 'NULL') + ' rather than 115.00 -- REQ: CST-001';

    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 16 to 19 - unexpected error: ' + @msg + ' -- REQ: CST-001';
END CATCH
GO
PRINT '# cst-gaps done';
GO
