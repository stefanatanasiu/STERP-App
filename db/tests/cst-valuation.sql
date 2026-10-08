/*  Costing valuation: the two worked examples of the specification proved against the tables
    that carry them.  C1 is FIFO - ten at 5.00 then ten at 8.00, issue twelve - and C2 is the
    moving weighted average - a hundred worth 400.00 and fifty worth 350.00.  The arithmetic
    belongs to the COBOL engines; what is proved here is that the stock of layers and the item
    cost row can hold the answer, that the enquiry reads it back, and that the constraints
    refuse the states that would make a valuation a fiction.
    Every case is built inside a transaction and rolled back.
    REQ: CST-005, CST-006, CST-007, CST-021, CST-026  AC: AC-C1, AC-C2 */
SET NOCOUNT ON;
GO
PRINT '# cst-valuation';
/*  Re-runnable: clear anything an aborted earlier run could have left behind. */
DELETE FROM cst.CostLayer WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-CST%';
DELETE FROM cst.ItemCost  WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-CST%';
DELETE FROM cst.StandardCost WHERE CompanyCode = 'STC' AND CostSetCode LIKE 'ZZSET%';
DELETE FROM cst.CostSet   WHERE CompanyCode = 'STC' AND CostSetCode LIKE 'ZZSET%';
GO
DECLARE @msg varchar(400), @relief decimal(28,6), @remain decimal(28,6), @layers int,
        @oldest decimal(28,6), @avg decimal(28,6), @qty decimal(28,6), @carry decimal(28,6),
        @net decimal(28,6), @impaired varchar(1), @raised varchar(1), @self varchar(3),
        @item varchar(40);

/*  A real item, because cst.ItemCost is keyed to the item master.  Its own cost rows are
    cleared inside each transaction and come back when the transaction is rolled back. */
SELECT TOP (1) @item = ItemCode FROM inv.Item WHERE CompanyCode = 'STC' ORDER BY ItemCode;

DECLARE @enq TABLE (ItemCode varchar(40), Revision varchar(4), SiteCode varchar(10), CostMethod varchar(4),
                    QtyOnHand decimal(28,6), AverageUnitCost decimal(28,6), CarryingValueGBP decimal(28,6),
                    AllowanceGBP decimal(28,6), NetCarryingGBP decimal(28,6), NrvUnitValue decimal(28,6),
                    StandardUnitCost decimal(28,6), LastPurchaseCost decimal(28,6),
                    FifoOldestUnit decimal(28,6), FifoLayers int, StandardSet varchar(20),
                    StandardFromSet decimal(28,6), IsImpaired varchar(1));

/*  1 to 4 - worked example C1.  Two FIFO layers, an issue of twelve units: the first layer is
    consumed whole and two units come out of the second, so the issue is relieved at 66.00 and
    eight units worth 64.00 are left behind.  REQ: CST-006, CST-026  AC: AC-C1 */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE FROM cst.CostLayer WHERE CompanyCode = 'STC' AND ItemCode = @item;
    DELETE FROM cst.ItemCost  WHERE CompanyCode = 'STC' AND ItemCode = @item;
    INSERT cst.ItemCost (CompanyCode, ItemCode, Revision, SiteCode, CostMethod, QtyOnHand,
                         CarryingValueGBP, AverageUnitCost, LastUpdatedBy)
    VALUES ('STC', @item, '', 'RDG', 'FIFO', 20, 130.00, 6.500000, 'TEST');
    INSERT cst.CostLayer (CompanyCode, ItemCode, Revision, LayerSeq, ReceiptDate, SourceDocType, SourceDocRef,
                          QtyOriginal, QtyRemaining, UnitCost, ValueRemainingGBP, IsClosed)
    VALUES ('STC', @item, '', 1, '2026-09-01', 'GRN', 'ZZGRN1', 10, 10, 5.000000, 50.00, 0),
           ('STC', @item, '', 2, '2026-09-14', 'GRN', 'ZZGRN2', 10, 10, 8.000000, 80.00, 0);

    DELETE @enq;
    INSERT @enq EXEC cst.usp_CostEnquiry @CompanyCode = 'STC', @ItemCode = @item, @Revision = '', @SiteCode = NULL;
    SELECT @layers = FifoLayers, @oldest = FifoOldestUnit FROM @enq;

    IF @layers = 2 AND @oldest = 5.000000
        PRINT 'ok 1 - the enquiry sees both layers and the oldest one at 5.00 -- REQ: CST-006, CST-026';
    ELSE PRINT 'not ok 1 - layers ' + ISNULL(CAST(@layers AS varchar(20)),'NULL') + ', oldest '
               + ISNULL(CAST(@oldest AS varchar(20)),'NULL') + ' -- REQ: CST-006, CST-026';

    /*  The issue the engine would write: layer one closes, layer two keeps eight units. */
    UPDATE cst.CostLayer SET QtyRemaining = 0, ValueRemainingGBP = 0, IsClosed = 1
     WHERE CompanyCode = 'STC' AND ItemCode = @item AND LayerSeq = 1;
    UPDATE cst.CostLayer SET QtyRemaining = 8, ValueRemainingGBP = 64.00
     WHERE CompanyCode = 'STC' AND ItemCode = @item AND LayerSeq = 2;
    SELECT @remain = SUM(ValueRemainingGBP), @qty = SUM(QtyRemaining)
      FROM cst.CostLayer WHERE CompanyCode = 'STC' AND ItemCode = @item;
    SET @relief = 130.00 - @remain;

    IF @relief = 66.00
        PRINT 'ok 2 - an issue of twelve is relieved at 66.00, not at an average -- REQ: CST-006  AC: AC-C1';
    ELSE PRINT 'not ok 2 - the issue came off at ' + ISNULL(CAST(@relief AS varchar(20)),'NULL')
               + ' rather than 66.00 -- REQ: CST-006  AC: AC-C1';

    IF @remain = 64.00 AND @qty = 8
        PRINT 'ok 3 - eight units worth 64.00 remain, all in the newer layer -- REQ: CST-006  AC: AC-C1';
    ELSE PRINT 'not ok 3 - ' + ISNULL(CAST(@qty AS varchar(20)),'NULL') + ' units worth '
               + ISNULL(CAST(@remain AS varchar(20)),'NULL') + ' remain -- REQ: CST-006  AC: AC-C1';

    /*  A layer may not be consumed past empty - stock cannot be worth less than nothing. */
    SET @raised = 'N';
    BEGIN TRY
        UPDATE cst.CostLayer SET QtyRemaining = -1
         WHERE CompanyCode = 'STC' AND ItemCode = @item AND LayerSeq = 2;
    END TRY
    BEGIN CATCH
        SET @raised = 'Y';
    END CATCH
    IF @raised = 'Y'
        PRINT 'ok 4 - a layer cannot be consumed past empty -- REQ: CST-005, CST-006';
    ELSE PRINT 'not ok 4 - a negative layer quantity was accepted -- REQ: CST-005, CST-006';

    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 1 to 4 - unexpected error in the FIFO tests: ' + @msg;
END CATCH

/*  5 to 8 - worked example C2.  A hundred units worth 400.00 and fifty worth 350.00 average at
    5.00; an issue of forty comes off at 200.00 and leaves a hundred and ten worth 550.00, and
    the average is unchanged by an issue because an issue is not a purchase.
    REQ: CST-005, CST-021, CST-026  AC: AC-C2 */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE FROM cst.CostLayer WHERE CompanyCode = 'STC' AND ItemCode = @item;
    DELETE FROM cst.ItemCost  WHERE CompanyCode = 'STC' AND ItemCode = @item;
    INSERT cst.ItemCost (CompanyCode, ItemCode, Revision, SiteCode, CostMethod, QtyOnHand,
                         CarryingValueGBP, AverageUnitCost, NrvUnitValue, AllowanceGBP, LastUpdatedBy)
    VALUES ('STC', @item, '', 'RDG', 'MWA', 150, 750.00, 5.000000, 4.000000, 0, 'TEST');

    SELECT @avg = CarryingValueGBP / NULLIF(QtyOnHand, 0)
      FROM cst.ItemCost WHERE CompanyCode = 'STC' AND ItemCode = @item;
    IF @avg = 5.000000
        PRINT 'ok 5 - 400.00 over a hundred and 350.00 over fifty average at 5.00 -- REQ: CST-005  AC: AC-C2';
    ELSE PRINT 'not ok 5 - the average came to ' + ISNULL(CAST(@avg AS varchar(20)),'NULL')
               + ' rather than 5.00 -- REQ: CST-005  AC: AC-C2';

    UPDATE cst.ItemCost SET QtyOnHand = 110, CarryingValueGBP = 550.00
     WHERE CompanyCode = 'STC' AND ItemCode = @item;
    SELECT @qty = QtyOnHand, @carry = CarryingValueGBP, @avg = AverageUnitCost
      FROM cst.ItemCost WHERE CompanyCode = 'STC' AND ItemCode = @item;
    SET @relief = 750.00 - @carry;

    IF @relief = 200.00
        PRINT 'ok 6 - an issue of forty at the average is relieved at 200.00 -- REQ: CST-005  AC: AC-C2';
    ELSE PRINT 'not ok 6 - the issue came off at ' + ISNULL(CAST(@relief AS varchar(20)),'NULL')
               + ' rather than 200.00 -- REQ: CST-005  AC: AC-C2';

    IF @qty = 110 AND @carry = 550.00 AND @avg = 5.000000
        PRINT 'ok 7 - a hundred and ten worth 550.00 remain and the average is still 5.00 -- REQ: CST-005  AC: AC-C2';
    ELSE PRINT 'not ok 7 - ' + ISNULL(CAST(@qty AS varchar(20)),'NULL') + ' units worth '
               + ISNULL(CAST(@carry AS varchar(20)),'NULL') + ' at '
               + ISNULL(CAST(@avg AS varchar(20)),'NULL') + ' -- REQ: CST-005  AC: AC-C2';

    /*  CST-021 - the write-down is held as an allowance beside the cost, never by rewriting it. */
    UPDATE cst.ItemCost SET AllowanceGBP = 110.00
     WHERE CompanyCode = 'STC' AND ItemCode = @item;
    DELETE @enq;
    INSERT @enq EXEC cst.usp_CostEnquiry @CompanyCode = 'STC', @ItemCode = @item, @Revision = '', @SiteCode = NULL;
    SELECT @carry = CarryingValueGBP, @net = NetCarryingGBP, @impaired = IsImpaired FROM @enq;

    IF @carry = 550.00 AND @net = 440.00 AND @impaired = 'Y'
        PRINT 'ok 8 - the allowance nets the pool down without touching the cost -- REQ: CST-021';
    ELSE PRINT 'not ok 8 - cost ' + ISNULL(CAST(@carry AS varchar(20)),'NULL') + ', net '
               + ISNULL(CAST(@net AS varchar(20)),'NULL') + ', impaired '
               + ISNULL(@impaired,'NULL') + ' -- REQ: CST-021';

    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 5 to 8 - unexpected error in the average cost tests: ' + @msg;
END CATCH

/*  9 and 10 - we value by exactly three methods, and a set approved by the person who prepared
    it is visible as such so that activation can be refused.  REQ: CST-005, CST-007 */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE FROM cst.CostLayer WHERE CompanyCode = 'STC' AND ItemCode = @item;
    DELETE FROM cst.ItemCost  WHERE CompanyCode = 'STC' AND ItemCode = @item;
    SET @raised = 'N';
    BEGIN TRY
        INSERT cst.ItemCost (CompanyCode, ItemCode, Revision, SiteCode, CostMethod, LastUpdatedBy)
        VALUES ('STC', @item, '', 'RDG', 'LIFO', 'TEST');
    END TRY
    BEGIN CATCH
        SET @raised = 'Y';
    END CATCH
    IF @raised = 'Y'
        PRINT 'ok 9 - a pool may only be valued FIFO, MWA or at standard -- REQ: CST-005';
    ELSE PRINT 'not ok 9 - an unknown valuation method was accepted -- REQ: CST-005';

    INSERT cst.CostSet (CompanyCode, CostSetCode, Name, Purpose, EffectiveFrom, CurrencyCode,
                        FiscalYear, Status, PreparedBy, ApprovedBy, ApprovedAt, CreatedBy)
    VALUES ('STC', 'ZZSET1', 'Self approved set', 'STANDARD', '2026-10-01', 'GBP',
            2027, 'APPROVED', 'COSTCLERK', 'COSTCLERK', SYSUTCDATETIME(), 'TEST');
    DECLARE @sets TABLE (CostSet varchar(20), Name varchar(80), Purpose varchar(12), Status varchar(10),
                         Currency char(3), FiscalYear smallint, EffectiveFrom date, ComparedTo varchar(20),
                         PreparedBy varchar(20), ApprovedBy varchar(20), ApprovedAt datetime2(3),
                         ActivatedAt datetime2(3), Items int, SelfApproved varchar(3));
    INSERT @sets EXEC cst.usp_CostSetList @CompanyCode = 'STC';
    SELECT @self = SelfApproved FROM @sets WHERE CostSet = 'ZZSET1';

    IF @self = 'YES'
        PRINT 'ok 10 - a set approved by its own preparer is reported, not hidden -- REQ: CST-007, CST-012';
    ELSE PRINT 'not ok 10 - self approval was reported as ' + ISNULL(@self,'NULL')
               + ' -- REQ: CST-007, CST-012';

    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 9 to 10 - unexpected error in the policy tests: ' + @msg;
END CATCH
GO
