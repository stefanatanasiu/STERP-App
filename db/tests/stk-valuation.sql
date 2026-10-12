/*  Stock movement valuation (GUI-028).  inv.usp_PostStockMovement is the one contract through which
    owned stock changes, so the costing pool (cst.ItemCost), its explanation (cst.ItemCostHistory) and
    the FIFO layers (cst.CostLayer) have to move in the same transaction as the physical balance.
    RECONCTL and CSTINVAL read the pool as the inventory subsidiary ledger; before this contract
    carried valuation, a despatch, a receipt and a receipt reversal moved stock and the ledger but
    left the pool alone, so the stock control account and its subsidiary drifted apart.

    Worked numbers: C1 (FIFO, 10 at 5.00 then 10 at 8.00, issue 12 = 66.00) and C2 (moving average,
    100 for 400.00 then 50 for 350.00 = 5.00, issue 40 = 200.00) are the specification examples the
    COBOL engines prove; the GUI-028 case replays the seven stock movements of the failed GUI run
    (net GBP 1,932.10) and demands the pools move by exactly that.

    Every scenario runs inside a transaction that is rolled back, except the replay and failure
    scenarios, which need the procedure to own its transaction: those use their own ZZ-SV items and
    remove everything they wrote.
    REQ: ITM-015, ITM-016, ITM-024, CST-005, CST-006, CST-009, CST-022, CST-024, COM-015, D-22 */
SET NOCOUNT ON;
GO
PRINT '# stk-valuation';
GO
/*  Clear anything an aborted earlier run could have left behind. */
DELETE FROM cst.ItemCostHistory WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-SV%';
DELETE FROM cst.CostLayer       WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-SV%';
DELETE FROM cst.ItemCost        WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-SV%';
DELETE FROM inv.StockMovement   WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-SV%';
DELETE FROM inv.StockBalance    WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-SV%';
DELETE FROM core.IdempotencyKey WHERE CorrelationId LIKE 'ZZSV-%';
DELETE FROM core.ExceptionItem  WHERE CompanyCode = 'STC' AND Module = 'CST' AND DocumentRef LIKE 'ZZ-SV%';
DELETE FROM cst.CostPolicy      WHERE CompanyCode = 'STC' AND ValuationGroup LIKE 'ZZSV%';
DELETE FROM inv.Item            WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-SV%';
GO
IF OBJECT_ID('tempdb..#sv_item') IS NOT NULL DROP PROCEDURE #sv_item;
IF OBJECT_ID('tempdb..#sv_pool') IS NOT NULL DROP PROCEDURE #sv_pool;
IF OBJECT_ID('tempdb..#sv_post') IS NOT NULL DROP PROCEDURE #sv_post;
IF OBJECT_ID('tempdb..#sv_is')   IS NOT NULL DROP PROCEDURE #sv_is;
GO
CREATE PROCEDURE #sv_item @Item varchar(40), @Method varchar(4), @Group varchar(20) = 'ZZSV', @Indivisible bit = 1
AS
    INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, QuantumQty, IsIndivisible,
            TraceabilityType, MakeBuy, ValuationGroup, CostMethod, LifecycleStatus, CreatedBy)
    VALUES ('STC', @Item, 'Valuation test item', 'BOUGHT', 'EA', 1, @Indivisible, 'NONE', 'BUY', @Group, @Method,
            'RELEASED', 'ZZSV');
GO
CREATE PROCEDURE #sv_pool @Item varchar(40), @Site varchar(10), @Rev varchar(4), @Method varchar(4),
    @Qty decimal(19,6), @Value decimal(18,2), @Avg decimal(19,6), @Std decimal(19,6) = NULL
AS
    INSERT cst.ItemCost (CompanyCode, ItemCode, Revision, SiteCode, CostMethod, QtyOnHand, CarryingValueGBP,
            AverageUnitCost, StandardUnitCost, LastUpdatedBy)
    VALUES ('STC', @Item, @Rev, @Site, @Method, @Qty, @Value, @Avg, @Std, 'TEST');
GO
/*  One movement through the shared contract.  The optional evidence parameters are only named when
    the contract has them, so the same scenarios also run against the contract as it stood before
    valuation was carried (the call then states only the unit cost, as the COBOL callers did). */
CREATE PROCEDURE #sv_post @Site varchar(10), @Loc varchar(20), @Item varchar(40), @Rev varchar(4), @Type varchar(14),
    @Qty decimal(19,6), @Date date = '2026-10-15', @Unit decimal(19,6) = NULL, @Value decimal(18,2) = NULL,
    @Corr varchar(64) = NULL, @Owner varchar(10) = 'OWNED', @OwnerCode varchar(20) = '',
    @Cond varchar(12) = 'ACCEPTED', @DocRef varchar(40) = 'ZZ-SV', @DocType varchar(20) = 'TEST',
    @CpSite varchar(10) = NULL, @CpLoc varchar(20) = NULL, @Dir char(1) = NULL, @Whs varchar(20) = 'MAIN',
    @MovId bigint = NULL OUTPUT
AS
BEGIN
    SET NOCOUNT ON;
    DECLARE @sql nvarchar(max) = N'EXEC inv.usp_PostStockMovement @CompanyCode = ''STC'', @SiteCode = @Site, '
        + N'@WarehouseCode = @Whs, @LocationCode = @Loc, @ItemCode = @Item, @MovementType = @Type, '
        + N'@Quantity = @Qty, @MovementDate = @Date, @UserCode = ''ZZSV'', @Revision = @Rev, @OwnerType = @Owner, '
        + N'@OwnerCode = @OwnerCode, @QualityStatus = @Cond, @Direction = @Dir, @UnitCost = @Unit, '
        + N'@SourceDocType = @DocType, @SourceDocRef = @DocRef, @CorrelationId = @Corr, '
        + N'@CounterpartLocationCode = @CpLoc';
    IF @Value IS NOT NULL AND EXISTS (SELECT 1 FROM sys.parameters WHERE object_id = OBJECT_ID(N'inv.usp_PostStockMovement') AND name = N'@ValueGBP')
        SET @sql += N', @ValueGBP = @Value';
    IF @CpSite IS NOT NULL AND EXISTS (SELECT 1 FROM sys.parameters WHERE object_id = OBJECT_ID(N'inv.usp_PostStockMovement') AND name = N'@CounterpartSiteCode')
        SET @sql += N', @CounterpartSiteCode = @CpSite';
    EXEC sp_executesql @sql,
         N'@Site varchar(10), @Whs varchar(20), @Loc varchar(20), @Item varchar(40), @Type varchar(14), @Qty decimal(19,6), @Date date, @Rev varchar(4), @Owner varchar(10), @OwnerCode varchar(20), @Cond varchar(12), @Dir char(1), @Unit decimal(19,6), @DocType varchar(20), @DocRef varchar(40), @Corr varchar(64), @CpLoc varchar(20), @Value decimal(18,2), @CpSite varchar(10)',
         @Site, @Whs, @Loc, @Item, @Type, @Qty, @Date, @Rev, @Owner, @OwnerCode, @Cond, @Dir, @Unit, @DocType, @DocRef, @Corr, @CpLoc, @Value, @CpSite;
    SELECT TOP (1) @MovId = MovementId FROM inv.StockMovement
     WHERE CompanyCode = 'STC' AND ItemCode = @Item AND SourceDocRef = @DocRef AND MovementType = @Type
       AND SiteCode = @Site AND LocationCode = @Loc ORDER BY MovementId DESC;
END
GO
/*  Assertion helper: the pool at (site, revision) holds exactly this quantity, value and average. */
CREATE PROCEDURE #sv_is @No varchar(8), @Name varchar(300), @Item varchar(40), @Site varchar(10), @Rev varchar(4),
    @Qty decimal(19,6), @Value decimal(18,2), @Avg decimal(19,6) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    DECLARE @q decimal(19,6), @v decimal(18,2), @a decimal(19,6), @n int;
    SELECT @n = COUNT(*), @q = MAX(QtyOnHand), @v = MAX(CarryingValueGBP), @a = MAX(AverageUnitCost)
      FROM cst.ItemCost WHERE CompanyCode = 'STC' AND ItemCode = @Item AND SiteCode = @Site AND Revision = @Rev;
    IF @n = 1 AND @q = @Qty AND @v = @Value AND (@Avg IS NULL OR @a = @Avg)
        PRINT 'ok ' + @No + ' - ' + @Name;
    ELSE PRINT 'not ok ' + @No + ' - ' + @Name + ' [pools ' + CAST(@n AS varchar(10)) + ', qty '
        + ISNULL(CAST(@q AS varchar(30)), 'NULL') + ', value ' + ISNULL(CAST(@v AS varchar(30)), 'NULL')
        + ', avg ' + ISNULL(CAST(@a AS varchar(30)), 'NULL') + '; expected qty ' + CAST(@Qty AS varchar(30))
        + ', value ' + CAST(@Value AS varchar(30)) + ISNULL(', avg ' + CAST(@Avg AS varchar(30)), '') + ']';
END
GO
/*  ======================================================================================
    1 to 12 - GUI-028.  The seven stock movements of the failed GUI run, on items built to the
    same positions: a standard costed finished good and a moving average component despatched,
    a moving average component received at the second site, and a bar received at a site that
    has no pool for it, reversed and received again in two parts.  The pools must move by the
    values the ledger was posted with, and every movement must leave one linked history row.
    ====================================================================================== */
DECLARE @msg varchar(400), @m1 bigint, @m2 bigint, @m3 bigint, @m4 bigint, @m5 bigint, @m6 bigint, @m7 bigint,
        @before decimal(18,2), @after decimal(18,2), @hist int, @linked int, @histsum decimal(18,2),
        @movsum decimal(18,2), @method varchar(4), @std decimal(19,6);
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC #sv_item 'ZZ-SV-FRAME', 'STD';
    EXEC #sv_item 'ZZ-SV-TYRE',  'MWA';
    EXEC #sv_item 'ZZ-SV-ALU',   'MWA';
    EXEC #sv_item 'ZZ-SV-BAR',   'MWA';
    EXEC #sv_post 'RDG', 'FG01',   'ZZ-SV-FRAME', 'C', 'OPENING', 24, @DocRef = 'ZZ-SV-OPEN-F';
    EXEC #sv_post 'RDG', 'STKB01', 'ZZ-SV-TYRE',  'A', 'OPENING', 227, @DocRef = 'ZZ-SV-OPEN-T';
    EXEC #sv_post 'BHM', 'STKA01', 'ZZ-SV-ALU',   'A', 'OPENING', 55, @DocRef = 'ZZ-SV-OPEN-A';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-BAR',   'A', 'OPENING', 318, @DocRef = 'ZZ-SV-OPEN-B';
    EXEC #sv_pool 'ZZ-SV-FRAME', 'RDG', 'C', 'STD', 24, 235.92, 9.830000, 10.124900;
    EXEC #sv_pool 'ZZ-SV-TYRE',  'RDG', 'A', 'MWA', 227, 4412.88, 19.440000;
    EXEC #sv_pool 'ZZ-SV-ALU',   'BHM', 'A', 'MWA', 55, 1095.05, 19.910000;
    EXEC #sv_pool 'ZZ-SV-BAR',   'RDG', 'A', 'MWA', 318, 16561.44, 52.080000;
    SELECT @before = SUM(CarryingValueGBP) FROM cst.ItemCost WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-SV-%';

    EXEC #sv_post 'RDG', 'FG01',   'ZZ-SV-FRAME', 'C', 'ISSUE',   6,  @Unit = 10.1249, @Value = 60.75,  @DocRef = 'ZZ-SV-DNS2-1', @DocType = 'DESPATCH', @MovId = @m1 OUTPUT;
    EXEC #sv_post 'RDG', 'STKB01', 'ZZ-SV-TYRE',  'A', 'ISSUE',   16, @Unit = 19.44,   @Value = 311.04, @DocRef = 'ZZ-SV-DNS2-2', @DocType = 'DESPATCH', @MovId = @m2 OUTPUT;
    EXEC #sv_post 'BHM', 'GOODSIN', 'ZZ-SV-ALU',  'A', 'RECEIPT', 40, @Unit = 15.9948, @Value = 639.79, @DocRef = 'ZZ-SV-GRN4', @DocType = 'GOODSRECEIPT', @MovId = @m3 OUTPUT;
    EXEC #sv_post 'BHM', 'GOODSIN', 'ZZ-SV-BAR',  'A', 'RECEIPT', 45, @Unit = 36.98,   @Value = 1664.10, @DocRef = 'ZZ-SV-GRN5', @DocType = 'GOODSRECEIPT', @MovId = @m4 OUTPUT;
    EXEC #sv_post 'BHM', 'GOODSIN', 'ZZ-SV-BAR',  'A', 'ISSUE',   45, @Unit = 36.98,   @Value = 1664.10, @DocRef = 'ZZ-SV-GRN5', @DocType = 'RECEIPTREVERSAL', @MovId = @m5 OUTPUT;
    EXEC #sv_post 'BHM', 'GOODSIN', 'ZZ-SV-BAR',  'A', 'RECEIPT', 20, @Unit = 36.98,   @Value = 739.60,  @DocRef = 'ZZ-SV-GRN6', @DocType = 'GOODSRECEIPT', @MovId = @m6 OUTPUT;
    EXEC #sv_post 'BHM', 'GOODSIN', 'ZZ-SV-BAR',  'A', 'RECEIPT', 25, @Unit = 36.98,   @Value = 924.50,  @DocRef = 'ZZ-SV-GRN7', @DocType = 'GOODSRECEIPT', @MovId = @m7 OUTPUT;

    EXEC #sv_is '1', 'a standard costed despatch of 6 relieves the pool by the posted 60.75 and leaves the standard alone', 'ZZ-SV-FRAME', 'RDG', 'C', 18, 175.17, 9.731667;
    SELECT @std = StandardUnitCost FROM cst.ItemCost WHERE CompanyCode = 'STC' AND ItemCode = 'ZZ-SV-FRAME';
    IF @std = 10.124900 PRINT 'ok 2 - the despatch did not rewrite the approved standard unit cost -- REQ: CST-007';
    ELSE PRINT 'not ok 2 - the standard unit cost became ' + ISNULL(CAST(@std AS varchar(30)), 'NULL');
    EXEC #sv_is '3', 'a moving average despatch of 16 relieves 311.04 at the average and the average is unchanged', 'ZZ-SV-TYRE', 'RDG', 'A', 211, 4101.84, 19.440000;
    EXEC #sv_is '4', 'a receipt of 40 into the existing site pool re-averages it: 1,095.05 + 639.79 over 95', 'ZZ-SV-ALU', 'BHM', 'A', 95, 1734.84, 18.261474;
    EXEC #sv_is '5', 'receipts reversed and received again leave the new site pool at 45 worth 1,664.10', 'ZZ-SV-BAR', 'BHM', 'A', 45, 1664.10, 36.980000;
    EXEC #sv_is '6', 'the pool at the other site is untouched by the receipts at this one', 'ZZ-SV-BAR', 'RDG', 'A', 318, 16561.44, 52.080000;

    SELECT @after = SUM(CarryingValueGBP) FROM cst.ItemCost WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-SV-%';
    IF @after - @before = 1932.10
        PRINT 'ok 7 - the pools moved by exactly GBP 1,932.10, the net of the seven GUI stock movements -- REQ: CST-022, ITM-024';
    ELSE PRINT 'not ok 7 - the pools moved by ' + ISNULL(CAST(@after - @before AS varchar(30)), 'NULL') + ' rather than 1932.10';

    SELECT @movsum = SUM(ValueGBP) FROM inv.StockMovement
     WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-SV-%' AND MovementId IN (@m1, @m2, @m3, @m4, @m5, @m6, @m7);
    IF @movsum = 1932.10 PRINT 'ok 8 - the movement values sum to the same 1,932.10 the pools moved by -- REQ: ITM-024';
    ELSE PRINT 'not ok 8 - the movement values sum to ' + ISNULL(CAST(@movsum AS varchar(30)), 'NULL');

    SELECT @hist = COUNT(*), @histsum = SUM(ValueChangeGBP)
      FROM cst.ItemCostHistory WHERE CompanyCode = 'STC' AND MovementId IN (@m1, @m2, @m3, @m4, @m5, @m6, @m7);
    IF @hist = 7 AND @histsum = 1932.10
        PRINT 'ok 9 - each of the seven movements left one history row linked to it, summing to 1,932.10 -- REQ: CST-024';
    ELSE PRINT 'not ok 9 - ' + CAST(@hist AS varchar(10)) + ' linked history rows summing to ' + ISNULL(CAST(@histsum AS varchar(30)), 'NULL');

    SELECT @linked = COUNT(*) FROM cst.ItemCostHistory h
      JOIN inv.StockMovement m ON m.MovementId = h.MovementId
     WHERE m.MovementId IN (@m1, @m2, @m3, @m4, @m5, @m6, @m7) AND h.ItemCode = m.ItemCode
       AND h.ValueChangeGBP = m.ValueGBP AND h.QtyChange = m.Quantity AND h.QtyAfter - h.QtyBefore = h.QtyChange
       AND h.ValueAfterGBP - h.ValueBeforeGBP = h.ValueChangeGBP;
    IF @linked = 7 PRINT 'ok 10 - every history row restates its movement''s quantity and value and ties before to after -- REQ: CST-024';
    ELSE PRINT 'not ok 10 - only ' + CAST(@linked AS varchar(10)) + ' history rows agree with their movements';

    SELECT @method = CostMethod FROM inv.StockMovement WHERE MovementId = @m1;
    IF @method = 'STD' AND (SELECT CostMethod FROM inv.StockMovement WHERE MovementId = @m2) = 'MWA'
        PRINT 'ok 11 - the movement records the valuation method that priced it -- REQ: CST-004';
    ELSE PRINT 'not ok 11 - the movement method is ' + ISNULL(@method, 'NULL');

    IF (SELECT ValueGBP FROM inv.StockMovement WHERE MovementId = @m5) = -1664.10
       AND EXISTS (SELECT 1 FROM cst.ItemCostHistory WHERE MovementId = @m5 AND ChangeType = 'ISSUE' AND QtyAfter = 0 AND ValueAfterGBP = 0 AND AvgAfter = 0)
        PRINT 'ok 12 - reversing the first receipt at its original value empties the pool to nil quantity and nil value -- REQ: CST-009, D-22';
    ELSE PRINT 'not ok 12 - the reversal did not return the pool to nil';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 1 to 12 - unexpected error in the GUI-028 scenario: ' + @msg;
END CATCH
GO
/*  ======================================================================================
    13 to 18 - worked example C2, moving average, through the movement contract.  A hundred worth
    400.00 and fifty worth 350.00 average at 5.00; an issue of forty with no cost evidence comes off
    at the average, 200.00, leaving 110 worth 550.00 - the figures the MWAVAL engine proves.
    ====================================================================================== */
DECLARE @msg varchar(400), @m1 bigint, @m2 bigint, @m3 bigint, @mv decimal(18,2), @mu decimal(19,6);
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC #sv_item 'ZZ-SV-C2', 'MWA';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-C2', 'A', 'RECEIPT', 100, @Value = 400.00, @DocRef = 'ZZ-SV-C2-1', @DocType = 'GOODSRECEIPT', @MovId = @m1 OUTPUT;
    EXEC #sv_is '13', 'a first receipt creates the pool: 100 worth 400.00 averaging 4.00', 'ZZ-SV-C2', 'RDG', 'A', 100, 400.00, 4.000000;
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-C2', 'A', 'RECEIPT', 50, @Value = 350.00, @DocRef = 'ZZ-SV-C2-2', @DocType = 'GOODSRECEIPT', @MovId = @m2 OUTPUT;
    EXEC #sv_is '14', 'a second receipt re-averages the pool from the carrying value: 750.00 over 150 = 5.00', 'ZZ-SV-C2', 'RDG', 'A', 150, 750.00, 5.000000;
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-C2', 'A', 'ISSUE', 40, @DocRef = 'ZZ-SV-C2-3', @DocType = 'DESPATCH', @MovId = @m3 OUTPUT;
    EXEC #sv_is '15', 'an issue of forty at the average leaves 110 worth 550.00 and the average is unchanged', 'ZZ-SV-C2', 'RDG', 'A', 110, 550.00, 5.000000;
    SELECT @mv = ValueGBP, @mu = UnitCost FROM inv.StockMovement WHERE MovementId = @m3;
    IF @mv = -200.00 AND @mu = 5.000000
        PRINT 'ok 16 - an issue with no cost evidence is valued by the pool at 200.00 and the movement says so -- REQ: CST-006';
    ELSE PRINT 'not ok 16 - the unevidenced issue was recorded at value ' + ISNULL(CAST(@mv AS varchar(30)), 'NULL') + ' and unit cost ' + ISNULL(CAST(@mu AS varchar(30)), 'NULL');
    IF (SELECT COUNT(*) FROM cst.ItemCostHistory WHERE CompanyCode = 'STC' AND ItemCode = 'ZZ-SV-C2') = 3
       AND EXISTS (SELECT 1 FROM cst.ItemCostHistory WHERE MovementId = @m2 AND QtyBefore = 100 AND ValueBeforeGBP = 400.00 AND AvgBefore = 4.000000
                      AND QtyAfter = 150 AND ValueAfterGBP = 750.00 AND AvgAfter = 5.000000 AND ChangeType = 'RECEIPT')
       AND EXISTS (SELECT 1 FROM cst.ItemCostHistory WHERE MovementId = @m3 AND QtyBefore = 150 AND ValueBeforeGBP = 750.00 AND AvgBefore = 5.000000
                      AND QtyAfter = 110 AND ValueAfterGBP = 550.00 AND ChangeType = 'ISSUE' AND QtyChange = -40 AND ValueChangeGBP = -200.00)
        PRINT 'ok 17 - the history explains each recalculation: before, change and after for the receipt and the issue -- REQ: CST-024';
    ELSE PRINT 'not ok 17 - the history rows do not explain the three recalculations';
    IF NOT EXISTS (SELECT 1 FROM cst.CostLayer WHERE CompanyCode = 'STC' AND ItemCode = 'ZZ-SV-C2')
        PRINT 'ok 18 - a moving average pool keeps no FIFO layers -- REQ: CST-005';
    ELSE PRINT 'not ok 18 - FIFO layers were written for a moving average pool';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 13 to 18 - unexpected error in the moving average scenario: ' + @msg;
END CATCH
GO
/*  ======================================================================================
    19 to 26 - worked example C1, FIFO, through the movement contract.  Ten at 5.00 then ten at
    8.00; an issue of twelve consumes the first layer whole and two from the second: 66.00 relieved,
    eight worth 64.00 left, all in the newer layer.  Layers keep the movement that created them.
    ====================================================================================== */
DECLARE @msg varchar(400), @m1 bigint, @m2 bigint, @m3 bigint, @m4 bigint, @l1q decimal(19,6), @l1v decimal(18,2),
        @l1c bit, @l2q decimal(19,6), @l2v decimal(18,2), @l2m bigint, @mv decimal(18,2);
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC #sv_item 'ZZ-SV-FIFO', 'FIFO';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-FIFO', 'A', 'RECEIPT', 10, '2026-10-02', @Value = 50.00, @DocRef = 'ZZ-SV-FIFO-1', @DocType = 'GOODSRECEIPT', @MovId = @m1 OUTPUT;
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-FIFO', 'A', 'RECEIPT', 10, '2026-10-14', @Value = 80.00, @DocRef = 'ZZ-SV-FIFO-2', @DocType = 'GOODSRECEIPT', @MovId = @m2 OUTPUT;
    IF (SELECT COUNT(*) FROM cst.CostLayer WHERE CompanyCode = 'STC' AND ItemCode = 'ZZ-SV-FIFO' AND QtyOriginal = 10 AND IsClosed = 0) = 2
       AND EXISTS (SELECT 1 FROM cst.CostLayer WHERE ItemCode = 'ZZ-SV-FIFO' AND MovementId = @m1 AND UnitCost = 5.000000 AND ValueRemainingGBP = 50.00 AND SourceDocRef = 'ZZ-SV-FIFO-1')
       AND EXISTS (SELECT 1 FROM cst.CostLayer WHERE ItemCode = 'ZZ-SV-FIFO' AND MovementId = @m2 AND UnitCost = 8.000000 AND ValueRemainingGBP = 80.00 AND SourceDocRef = 'ZZ-SV-FIFO-2')
        PRINT 'ok 19 - each FIFO receipt opens a layer at its own cost, linked to the movement that created it -- REQ: CST-005, CST-024';
    ELSE PRINT 'not ok 19 - the receipts did not open two linked layers at 5.00 and 8.00';
    EXEC #sv_is '20', 'the FIFO pool carries both layers: 20 worth 130.00', 'ZZ-SV-FIFO', 'RDG', 'A', 20, 130.00;
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-FIFO', 'A', 'ISSUE', 12, '2026-10-15', @DocRef = 'ZZ-SV-FIFO-3', @DocType = 'DESPATCH', @MovId = @m3 OUTPUT;
    SELECT @l1q = QtyRemaining, @l1v = ValueRemainingGBP, @l1c = IsClosed FROM cst.CostLayer WHERE ItemCode = 'ZZ-SV-FIFO' AND MovementId = @m1;
    SELECT @l2q = QtyRemaining, @l2v = ValueRemainingGBP FROM cst.CostLayer WHERE ItemCode = 'ZZ-SV-FIFO' AND MovementId = @m2;
    SELECT @mv = ValueGBP FROM inv.StockMovement WHERE MovementId = @m3;
    IF @mv = -66.00
        PRINT 'ok 21 - an issue of twelve is relieved at 66.00 from the oldest layers, not at the 6.50 average -- REQ: CST-005  AC: AC-C1';
    ELSE PRINT 'not ok 21 - the issue was valued at ' + ISNULL(CAST(@mv AS varchar(30)), 'NULL');
    IF @l1q = 0 AND @l1v = 0 AND @l1c = 1 AND @l2q = 8 AND @l2v = 64.00
        PRINT 'ok 22 - the first layer is consumed and closed, eight worth 64.00 remain in the second -- REQ: CST-005  AC: AC-C1';
    ELSE PRINT 'not ok 22 - layer one ' + ISNULL(CAST(@l1q AS varchar(30)), 'NULL') + '/' + ISNULL(CAST(@l1v AS varchar(30)), 'NULL')
               + ' closed ' + ISNULL(CAST(@l1c AS varchar(3)), 'NULL') + ', layer two ' + ISNULL(CAST(@l2q AS varchar(30)), 'NULL') + '/' + ISNULL(CAST(@l2v AS varchar(30)), 'NULL');
    EXEC #sv_is '23', 'the pool follows the layers: 8 worth 64.00 at 8.00', 'ZZ-SV-FIFO', 'RDG', 'A', 8, 64.00, 8.000000;
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-FIFO', 'A', 'ISSUE', 8, '2026-10-15', @DocRef = 'ZZ-SV-FIFO-4', @DocType = 'DESPATCH', @MovId = @m4 OUTPUT;
    EXEC #sv_is '24', 'issuing the rest empties the pool with no residual value', 'ZZ-SV-FIFO', 'RDG', 'A', 0, 0.00, 0.000000;
    IF NOT EXISTS (SELECT 1 FROM cst.CostLayer WHERE ItemCode = 'ZZ-SV-FIFO' AND (IsClosed = 0 OR QtyRemaining <> 0 OR ValueRemainingGBP <> 0))
        PRINT 'ok 25 - every layer is closed at nil quantity and nil value -- REQ: CST-005, D-22';
    ELSE PRINT 'not ok 25 - a layer was left open or carrying value';
    IF (SELECT COUNT(*) FROM cst.ItemCostHistory WHERE ItemCode = 'ZZ-SV-FIFO') = 4
       AND (SELECT SUM(ValueChangeGBP) FROM cst.ItemCostHistory WHERE ItemCode = 'ZZ-SV-FIFO') = 0
        PRINT 'ok 26 - four linked history rows and a net value change of nil over the whole life of the pool -- REQ: CST-024';
    ELSE PRINT 'not ok 26 - the FIFO history does not net to nil';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 19 to 26 - unexpected error in the FIFO scenario: ' + @msg;
END CATCH
GO
/*  ======================================================================================
    27 to 31 - standard cost.  An issue with no cost evidence is relieved at the standard, not at the
    average the pool has drifted to; a receipt is taken in at the value the receipt posted.
    ====================================================================================== */
DECLARE @msg varchar(400), @m1 bigint, @m2 bigint, @mv decimal(18,2), @mm varchar(4);
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC #sv_item 'ZZ-SV-STD', 'STD';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-STD', 'A', 'OPENING', 10, @DocRef = 'ZZ-SV-STD-O';
    EXEC #sv_pool 'ZZ-SV-STD', 'RDG', 'A', 'STD', 10, 100.00, 10.000000, 10.500000;
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-STD', 'A', 'ISSUE', 4, @DocRef = 'ZZ-SV-STD-1', @DocType = 'DESPATCH', @MovId = @m1 OUTPUT;
    EXEC #sv_is '27', 'an unevidenced standard issue of 4 is relieved at the standard 10.50: 42.00', 'ZZ-SV-STD', 'RDG', 'A', 6, 58.00, 9.666667;
    SELECT @mv = ValueGBP, @mm = CostMethod FROM inv.StockMovement WHERE MovementId = @m1;
    IF @mv = -42.00 AND @mm = 'STD'
        PRINT 'ok 28 - the movement records the standard relief and its method -- REQ: CST-007';
    ELSE PRINT 'not ok 28 - the standard issue was recorded at ' + ISNULL(CAST(@mv AS varchar(30)), 'NULL') + ' by method ' + ISNULL(@mm, 'NULL');
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-STD', 'A', 'RECEIPT', 5, @Value = 52.50, @DocRef = 'ZZ-SV-STD-2', @DocType = 'GOODSRECEIPT', @MovId = @m2 OUTPUT;
    EXEC #sv_is '29', 'a standard receipt takes the value the receipt posted into the pool: 5 at 52.50', 'ZZ-SV-STD', 'RDG', 'A', 11, 110.50, 10.045455;
    IF (SELECT StandardUnitCost FROM cst.ItemCost WHERE ItemCode = 'ZZ-SV-STD') = 10.500000
        PRINT 'ok 30 - the approved standard is untouched by movements -- REQ: CST-007';
    ELSE PRINT 'not ok 30 - the standard unit cost changed';
    IF NOT EXISTS (SELECT 1 FROM cst.CostLayer WHERE ItemCode = 'ZZ-SV-STD')
        PRINT 'ok 31 - a standard pool keeps no FIFO layers -- REQ: CST-005';
    ELSE PRINT 'not ok 31 - FIFO layers were written for a standard pool';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 27 to 31 - unexpected error in the standard cost scenario: ' + @msg;
END CATCH
GO
/*  ======================================================================================
    32 to 36 - units, currency and quantum.  The movement is stated in stock units; a receipt in a
    purchase unit and a foreign currency reaches the pool as the sterling value the caller posted,
    never as quantity times a price in another unit.  A quantity that breaks the item's increment is
    refused and leaves no trace in the pool or the history.
    ====================================================================================== */
DECLARE @msg varchar(400), @m1 bigint, @hist int, @err varchar(400), @mv decimal(18,2), @mu decimal(19,6);
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC #sv_item 'ZZ-SV-UOM', 'MWA';
    /*  three boxes of twelve = 36 stock units; the supplier price converts to 83.33 sterling, which is not
        36 x 2.314167 (83.31), so the value the caller posted has to win */
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-UOM', 'A', 'RECEIPT', 36, @Unit = 2.314167, @Value = 83.33, @DocRef = 'ZZ-SV-UOM-1', @DocType = 'GOODSRECEIPT', @MovId = @m1 OUTPUT;
    EXEC #sv_is '32', 'the pool takes the sterling value posted for the receipt (83.33), not stock quantity times a unit price', 'ZZ-SV-UOM', 'RDG', 'A', 36, 83.33, 2.314722;
    SELECT @mv = ValueGBP, @mu = UnitCost FROM inv.StockMovement WHERE MovementId = @m1;
    IF @mv = 83.33 AND @mu = 2.314167
        PRINT 'ok 33 - the movement keeps the posted value and the unit cost the caller stated -- REQ: ITM-024';
    ELSE PRINT 'not ok 33 - the movement says value ' + ISNULL(CAST(@mv AS varchar(30)), 'NULL') + ' at unit cost ' + ISNULL(CAST(@mu AS varchar(30)), 'NULL');
    SELECT @hist = COUNT(*) FROM cst.ItemCostHistory WHERE CompanyCode = 'STC' AND ItemCode = 'ZZ-SV-UOM';
    SET @err = '';
    BEGIN TRY
        EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-UOM', 'A', 'RECEIPT', 2.5, @Value = 6.00, @DocRef = 'ZZ-SV-UOM-2', @DocType = 'GOODSRECEIPT';
    END TRY
    BEGIN CATCH
        SET @err = ERROR_MESSAGE();
    END CATCH
    IF @err LIKE '%STERP-E-0028%' PRINT 'ok 34 - a fractional quantity of an indivisible item is refused before it is valued -- REQ: ITM-005, ITM-007';
    ELSE PRINT 'not ok 34 - the quantum breach was answered with: ' + @err;
    EXEC #sv_is '35', 'the refused receipt left the pool as it was', 'ZZ-SV-UOM', 'RDG', 'A', 36, 83.33, 2.314722;
    IF (SELECT COUNT(*) FROM cst.ItemCostHistory WHERE CompanyCode = 'STC' AND ItemCode = 'ZZ-SV-UOM') = @hist
        PRINT 'ok 36 - and wrote no history -- REQ: CST-024';
    ELSE PRINT 'not ok 36 - the refused receipt wrote history';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 32 to 36 - unexpected error in the units scenario: ' + @msg;
END CATCH
GO
/*  ======================================================================================
    37 to 41 - non-owned stock is not our asset.  Customer and supplier owned material, and anything
    in a location flagged as not owned, changes the physical balance and nothing in the costing pool.
    ====================================================================================== */
DECLARE @msg varchar(400), @n int, @bal decimal(19,6);
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC #sv_item 'ZZ-SV-OWN', 'MWA';
    EXEC #sv_post 'RDG', 'CUSTOWN', 'ZZ-SV-OWN', 'A', 'RECEIPT', 10, @Value = 100.00, @Owner = 'CUSTOMER', @OwnerCode = 'C0007', @Whs = 'SUB', @DocRef = 'ZZ-SV-OWN-1';
    EXEC #sv_post 'RDG', 'STKA02', 'ZZ-SV-OWN', 'A', 'RECEIPT', 6, @Value = 60.00, @Owner = 'SUPPLIER', @OwnerCode = 'S0001', @DocRef = 'ZZ-SV-OWN-2';
    EXEC #sv_post 'RDG', 'CUSTOWN', 'ZZ-SV-OWN', 'A', 'RECEIPT', 4, @Value = 40.00, @Whs = 'SUB', @DocRef = 'ZZ-SV-OWN-3';
    SELECT @bal = SUM(QtyOnHand) FROM inv.StockBalance WHERE CompanyCode = 'STC' AND ItemCode = 'ZZ-SV-OWN';
    IF @bal = 20 PRINT 'ok 37 - the physical balances moved: 10 customer owned, 6 supplier owned and 4 in a not-owned location -- REQ: ITM-012';
    ELSE PRINT 'not ok 37 - the physical balance is ' + ISNULL(CAST(@bal AS varchar(30)), 'NULL');
    SELECT @n = COUNT(*) FROM cst.ItemCost WHERE CompanyCode = 'STC' AND ItemCode = 'ZZ-SV-OWN';
    IF @n = 0 PRINT 'ok 38 - no costing pool was created for stock the company does not own -- REQ: ITM-012, CST-022';
    ELSE PRINT 'not ok 38 - ' + CAST(@n AS varchar(10)) + ' pool(s) were created for non-owned stock';
    SELECT @n = COUNT(*) FROM cst.ItemCostHistory WHERE CompanyCode = 'STC' AND ItemCode = 'ZZ-SV-OWN';
    IF @n = 0 PRINT 'ok 39 - and no valuation history -- REQ: CST-024';
    ELSE PRINT 'not ok 39 - non-owned movements wrote ' + CAST(@n AS varchar(10)) + ' history rows';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-OWN', 'A', 'RECEIPT', 5, @Value = 55.00, @DocRef = 'ZZ-SV-OWN-4';
    EXEC #sv_post 'RDG', 'STKA02', 'ZZ-SV-OWN', 'A', 'ISSUE', 6, @Value = 60.00, @Owner = 'SUPPLIER', @OwnerCode = 'S0001', @DocRef = 'ZZ-SV-OWN-5';
    EXEC #sv_is '40', 'owned receipts are valued while the supplier owned issue leaves the pool alone', 'ZZ-SV-OWN', 'RDG', 'A', 5, 55.00, 11.000000;
    SELECT @n = COUNT(*) FROM inv.StockMovement WHERE CompanyCode = 'STC' AND ItemCode = 'ZZ-SV-OWN' AND OwnerType <> 'OWNED' AND CostMethod IS NOT NULL;
    IF @n = 0 PRINT 'ok 41 - non-owned movements carry no valuation method -- REQ: ITM-012';
    ELSE PRINT 'not ok 41 - ' + CAST(@n AS varchar(10)) + ' non-owned movements claim a valuation method';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 37 to 41 - unexpected error in the ownership scenario: ' + @msg;
END CATCH
GO
/*  ======================================================================================
    42 to 50 - condition changes and transfers.  A quality hold changes availability and not value; a
    transfer inside one pool changes nothing in the pool; a transfer between two site pools takes
    the value out of the origin at its average and puts exactly that into the destination, so the
    company's stock is worth the same before and after; when policy pools the item company wide the
    same transfer is neutral.
    ====================================================================================== */
DECLARE @msg varchar(400), @m1 bigint, @m2 bigint, @tot0 decimal(18,2), @tot1 decimal(18,2), @h0 int, @carry decimal(18,2);
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC #sv_item 'ZZ-SV-XFER', 'MWA';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-XFER', 'A', 'OPENING', 100, @DocRef = 'ZZ-SV-XFER-O1';
    EXEC #sv_post 'BHM', 'STKA01', 'ZZ-SV-XFER', 'A', 'OPENING', 20,  @DocRef = 'ZZ-SV-XFER-O2';
    EXEC #sv_pool 'ZZ-SV-XFER', 'RDG', 'A', 'MWA', 100, 1000.00, 10.000000;
    EXEC #sv_pool 'ZZ-SV-XFER', 'BHM', 'A', 'MWA', 20,  400.00,  20.000000;
    SELECT @tot0 = SUM(CarryingValueGBP) FROM cst.ItemCost WHERE ItemCode = 'ZZ-SV-XFER';
    SELECT @h0 = COUNT(*) FROM cst.ItemCostHistory WHERE ItemCode = 'ZZ-SV-XFER';

    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-XFER', 'A', 'QUALITY', 10, @Dir = 'O', @DocRef = 'ZZ-SV-XFER-Q', @DocType = 'HOLD';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-XFER', 'A', 'QUALITY', 10, @Dir = 'I', @Cond = 'QUARANTINE', @DocRef = 'ZZ-SV-XFER-Q', @DocType = 'HOLD';
    EXEC #sv_is '42', 'a quality hold moves the stock between conditions without touching the pool', 'ZZ-SV-XFER', 'RDG', 'A', 100, 1000.00, 10.000000;
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-XFER', 'A', 'TRANSFEROUT', 5, @CpLoc = 'STKA02', @CpSite = 'RDG', @DocRef = 'ZZ-SV-XFER-T1', @DocType = 'TRANSFER';
    EXEC #sv_post 'RDG', 'STKA02', 'ZZ-SV-XFER', 'A', 'TRANSFERIN',  5, @CpLoc = 'STKA01', @CpSite = 'RDG', @DocRef = 'ZZ-SV-XFER-T1', @DocType = 'TRANSFER';
    EXEC #sv_is '43', 'a transfer between two locations of one site leaves the pool as it was', 'ZZ-SV-XFER', 'RDG', 'A', 100, 1000.00, 10.000000;
    IF (SELECT COUNT(*) FROM cst.ItemCostHistory WHERE ItemCode = 'ZZ-SV-XFER') = @h0
        PRINT 'ok 44 - and neither the hold nor the same-pool transfer wrote valuation history -- REQ: CST-024';
    ELSE PRINT 'not ok 44 - value-neutral movements wrote history';

    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-XFER', 'A', 'TRANSFEROUT', 10, @CpLoc = 'STKA01', @CpSite = 'BHM', @DocRef = 'ZZ-SV-XFER-T2', @DocType = 'TRANSFER', @MovId = @m1 OUTPUT;
    SELECT @carry = ABS(ValueGBP) FROM inv.StockMovement WHERE MovementId = @m1;
    EXEC #sv_post 'BHM', 'STKA01', 'ZZ-SV-XFER', 'A', 'TRANSFERIN', 10, @Value = @carry, @CpLoc = 'STKA01', @CpSite = 'RDG', @DocRef = 'ZZ-SV-XFER-T2', @DocType = 'TRANSFER', @MovId = @m2 OUTPUT;
    IF @carry = 100.00 PRINT 'ok 45 - the transfer out of the origin pool is relieved at its average, 100.00 -- REQ: CST-009';
    ELSE PRINT 'not ok 45 - the origin pool was relieved by ' + ISNULL(CAST(@carry AS varchar(30)), 'NULL');
    EXEC #sv_is '46', 'the origin pool gave up 10 units and 100.00', 'ZZ-SV-XFER', 'RDG', 'A', 90, 900.00, 10.000000;
    EXEC #sv_is '47', 'the destination pool received exactly the relieved value: 30 worth 500.00', 'ZZ-SV-XFER', 'BHM', 'A', 30, 500.00, 16.666667;
    SELECT @tot1 = SUM(CarryingValueGBP) FROM cst.ItemCost WHERE ItemCode = 'ZZ-SV-XFER';
    IF @tot1 = @tot0 PRINT 'ok 48 - the company''s stock is worth the same after the inter-site transfer -- REQ: CST-022';
    ELSE PRINT 'not ok 48 - the transfer changed the company valuation from ' + CAST(@tot0 AS varchar(30)) + ' to ' + ISNULL(CAST(@tot1 AS varchar(30)), 'NULL');

    INSERT cst.CostPolicy (CompanyCode, ValuationGroup, EffectiveFrom, CostMethod, PoolBySite, PoolByRevision, ApprovedBy)
    VALUES ('STC', 'ZZSVW', '2026-01-01', 'MWA', 0, 1, 'TEST');
    EXEC #sv_item 'ZZ-SV-WILD', 'MWA', 'ZZSVW';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-WILD', 'A', 'RECEIPT', 10, @Value = 100.00, @DocRef = 'ZZ-SV-WILD-1', @DocType = 'GOODSRECEIPT';
    EXEC #sv_post 'BHM', 'STKA01', 'ZZ-SV-WILD', 'A', 'RECEIPT', 10, @Value = 120.00, @DocRef = 'ZZ-SV-WILD-2', @DocType = 'GOODSRECEIPT';
    EXEC #sv_is '49', 'policy that pools by revision only holds both sites'' receipts in one company pool: 20 worth 220.00', 'ZZ-SV-WILD', '*', 'A', 20, 220.00, 11.000000;
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-WILD', 'A', 'TRANSFEROUT', 5, @CpLoc = 'STKA01', @CpSite = 'BHM', @DocRef = 'ZZ-SV-WILD-T', @DocType = 'TRANSFER';
    EXEC #sv_post 'BHM', 'STKA01', 'ZZ-SV-WILD', 'A', 'TRANSFERIN',  5, @CpLoc = 'STKA01', @CpSite = 'RDG', @DocRef = 'ZZ-SV-WILD-T', @DocType = 'TRANSFER';
    IF (SELECT COUNT(*) FROM cst.ItemCostHistory WHERE ItemCode = 'ZZ-SV-WILD') = 2
       AND (SELECT COUNT(*) FROM cst.ItemCost WHERE ItemCode = 'ZZ-SV-WILD') = 1
       AND EXISTS (SELECT 1 FROM cst.ItemCost WHERE ItemCode = 'ZZ-SV-WILD' AND SiteCode = '*' AND Revision = 'A' AND QtyOnHand = 20 AND CarryingValueGBP = 220.00)
        PRINT 'ok 50 - a transfer between sites that share a company pool is neutral: one pool, two history rows -- REQ: CST-004';
    ELSE PRINT 'not ok 50 - the company wide pool was split or moved by the transfer';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 42 to 50 - unexpected error in the transfer scenario: ' + @msg;
END CATCH
GO
/*  ======================================================================================
    51 to 58 - zero residuals, returns, adjustments, openings and shortfalls.
    ====================================================================================== */
DECLARE @msg varchar(400), @m1 bigint, @m2 bigint, @n int, @mv decimal(18,2), @vs varchar(12);
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC #sv_item 'ZZ-SV-ZERO', 'MWA';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-ZERO', 'A', 'OPENING', 7, @DocRef = 'ZZ-SV-ZERO-O';
    EXEC #sv_pool 'ZZ-SV-ZERO', 'RDG', 'A', 'MWA', 7, 10.01, 1.430000;
    /*  7 x 1.428571 posts 10.00, so 0.01 would be stranded on an empty pool: it is cleared to the issue */
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-ZERO', 'A', 'ISSUE', 7, @Unit = 1.428571, @DocRef = 'ZZ-SV-ZERO-1', @DocType = 'DESPATCH', @MovId = @m1 OUTPUT;
    EXEC #sv_is '51', 'issuing the last units leaves no rounding residual on the empty pool', 'ZZ-SV-ZERO', 'RDG', 'A', 0, 0.00, 0.000000;
    SELECT @mv = ValueGBP FROM inv.StockMovement WHERE MovementId = @m1;
    IF @mv = -10.01
        PRINT 'ok 52 - the residual 0.01 is cleared to the issue, which is recorded at the full 10.01 -- REQ: D-22, CST-009';
    ELSE PRINT 'not ok 52 - the last issue was recorded at ' + ISNULL(CAST(@mv AS varchar(30)), 'NULL');
    SELECT @vs = ValuationState FROM cst.vw_ItemValuation WHERE CompanyCode = 'STC' AND ItemCode = 'ZZ-SV-ZERO';
    IF @vs = 'NORMAL'
        PRINT 'ok 53 - the valuation view shows the emptied pool as normal, not RESIDUAL -- REQ: CST-022';
    ELSE PRINT 'not ok 53 - the emptied pool is shown as ' + ISNULL(@vs, 'NULL');

    EXEC #sv_item 'ZZ-SV-ADJ', 'MWA';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-ADJ', 'A', 'OPENING', 10, @DocRef = 'ZZ-SV-ADJ-O';
    EXEC #sv_pool 'ZZ-SV-ADJ', 'RDG', 'A', 'MWA', 10, 100.00, 10.000000;
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-ADJ', 'A', 'ADJUSTDOWN', 2, @DocRef = 'ZZ-SV-ADJ-1', @DocType = 'COUNT';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-ADJ', 'A', 'ADJUSTUP',   3, @DocRef = 'ZZ-SV-ADJ-2', @DocType = 'COUNT';
    EXEC #sv_is '54', 'count adjustments with no cost evidence are valued at the current average: -20.00 then +30.00', 'ZZ-SV-ADJ', 'RDG', 'A', 11, 110.00, 10.000000;
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-ADJ', 'A', 'RETURNIN', 2, @Unit = 12.00, @DocRef = 'ZZ-SV-ADJ-3', @DocType = 'DESPATCHREV';
    EXEC #sv_is '55', 'a return comes back at the original cost it left with (12.00), not at the pool average', 'ZZ-SV-ADJ', 'RDG', 'A', 13, 134.00, 10.307692;

    EXEC #sv_item 'ZZ-SV-OPEN', 'MWA';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-OPEN', 'A', 'OPENING', 10, @DocRef = 'ZZ-SV-OPEN-1';
    SELECT @n = COUNT(*) FROM cst.ItemCost WHERE ItemCode = 'ZZ-SV-OPEN';
    IF @n = 0 PRINT 'ok 56 - an opening balance with no valuation evidence invents no value and no pool -- REQ: ITM-024, CST-006';
    ELSE PRINT 'not ok 56 - an unevidenced opening created ' + CAST(@n AS varchar(10)) + ' pool(s)';
    EXEC #sv_post 'RDG', 'STKA02', 'ZZ-SV-OPEN', 'A', 'OPENING', 10, @Unit = 2.50, @DocRef = 'ZZ-SV-OPEN-2';
    EXEC #sv_is '57', 'an opening with cost evidence opens the pool at that cost: 10 worth 25.00', 'ZZ-SV-OPEN', 'RDG', 'A', 10, 25.00, 2.500000;
    IF EXISTS (SELECT 1 FROM cst.ItemCostHistory WHERE ItemCode = 'ZZ-SV-OPEN' AND ChangeType = 'OPENING' AND ValueChangeGBP = 25.00)
        PRINT 'ok 58 - and its history row says OPENING -- REQ: CST-024';
    ELSE PRINT 'not ok 58 - no OPENING history row';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 51 to 58 - unexpected error in the residual and adjustment scenario: ' + @msg;
END CATCH
GO
/*  A pool that holds fewer units than the physical balance cannot be driven negative; the physical
    movement is not blocked by a valuation that was never complete, but the gap is raised as a costing
    exception so the reconciliation cannot be read as whole. */
DECLARE @msg varchar(400), @m1 bigint, @exc0 int, @exc1 int, @mv decimal(18,2), @phys decimal(19,6);
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC #sv_item 'ZZ-SV-SHORT', 'MWA';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-SHORT', 'A', 'OPENING', 8, @DocRef = 'ZZ-SV-SHORT-O';
    EXEC #sv_pool 'ZZ-SV-SHORT', 'RDG', 'A', 'MWA', 5, 50.00, 10.000000;
    SELECT @exc0 = COUNT(*) FROM core.ExceptionItem WHERE CompanyCode = 'STC' AND Module = 'CST' AND Status <> 'RESOLVED';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-SHORT', 'A', 'ISSUE', 8, @Unit = 10.00, @DocRef = 'ZZ-SV-SHORT-1', @DocType = 'DESPATCH', @MovId = @m1 OUTPUT;
    EXEC #sv_is '59', 'an issue larger than the pool relieves the pool to nil and never below', 'ZZ-SV-SHORT', 'RDG', 'A', 0, 0.00, 0.000000;
    SELECT @mv = ValueGBP FROM inv.StockMovement WHERE MovementId = @m1;
    SELECT @phys = SUM(QtyOnHand) FROM inv.StockBalance WHERE ItemCode = 'ZZ-SV-SHORT';
    IF @mv = -50.00 AND @phys = 0
        PRINT 'ok 60 - the physical issue still posted and carries only the value the pool held, 50.00 -- REQ: D-09, CST-022';
    ELSE PRINT 'not ok 60 - the shortfall issue was recorded at ' + ISNULL(CAST(@mv AS varchar(30)), 'NULL') + ' with physical stock ' + ISNULL(CAST(@phys AS varchar(30)), 'NULL');
    SELECT @exc1 = COUNT(*) FROM core.ExceptionItem WHERE CompanyCode = 'STC' AND Module = 'CST' AND Status <> 'RESOLVED';
    IF @exc1 = @exc0 + 1 PRINT 'ok 61 - the unvalued 3 units are raised as an open costing exception -- REQ: CST-022, COM-017';
    ELSE PRINT 'not ok 61 - open costing exceptions went from ' + CAST(@exc0 AS varchar(10)) + ' to ' + CAST(@exc1 AS varchar(10));
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 59 to 61 - unexpected error in the shortfall scenario: ' + @msg;
END CATCH
GO
/*  ======================================================================================
    62 to 71 - replay and failure, with the procedure owning its own transaction (no caller
    transaction).  A repeated correlation id must not value the movement twice; a refused or failed
    movement must leave nothing behind - not the balance, the movement, the pool, the history, the
    layers nor the idempotency key.  These scenarios commit, so they use items of their own and
    remove them afterwards.
    ====================================================================================== */
DECLARE @msg varchar(400), @err varchar(400), @mv0 int, @h0 int, @l0 int, @k0 int, @b0 decimal(19,6);
BEGIN TRY
    EXEC #sv_item 'ZZ-SV-RPL', 'MWA';
    EXEC #sv_item 'ZZ-SV-RPF', 'FIFO';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-RPL', 'A', 'RECEIPT', 10, @Value = 100.00, @Corr = 'ZZSV-R1', @DocRef = 'ZZ-SV-RPL-1', @DocType = 'GOODSRECEIPT';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-RPL', 'A', 'RECEIPT', 10, @Value = 100.00, @Corr = 'ZZSV-R1', @DocRef = 'ZZ-SV-RPL-1', @DocType = 'GOODSRECEIPT';
    EXEC #sv_is '62', 'a replayed receipt is valued once: 10 worth 100.00, not 20 worth 200.00', 'ZZ-SV-RPL', 'RDG', 'A', 10, 100.00, 10.000000;
    IF (SELECT COUNT(*) FROM inv.StockMovement WHERE ItemCode = 'ZZ-SV-RPL') = 1
       AND (SELECT COUNT(*) FROM cst.ItemCostHistory WHERE ItemCode = 'ZZ-SV-RPL') = 1
       AND (SELECT SUM(QtyOnHand) FROM inv.StockBalance WHERE ItemCode = 'ZZ-SV-RPL') = 10
        PRINT 'ok 63 - the replay wrote no second movement, balance change or history row -- REQ: COM-015';
    ELSE PRINT 'not ok 63 - the replay left more than one movement or history row';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-RPL', 'A', 'ISSUE', 4, @Corr = 'ZZSV-R2', @DocRef = 'ZZ-SV-RPL-2', @DocType = 'DESPATCH';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-RPL', 'A', 'ISSUE', 4, @Corr = 'ZZSV-R2', @DocRef = 'ZZ-SV-RPL-2', @DocType = 'DESPATCH';
    EXEC #sv_is '64', 'a replayed issue relieves the pool once: 6 worth 60.00', 'ZZ-SV-RPL', 'RDG', 'A', 6, 60.00, 10.000000;

    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-RPF', 'A', 'RECEIPT', 10, @Value = 50.00, @Corr = 'ZZSV-R3', @DocRef = 'ZZ-SV-RPF-1', @DocType = 'GOODSRECEIPT';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-RPF', 'A', 'RECEIPT', 10, @Value = 50.00, @Corr = 'ZZSV-R3', @DocRef = 'ZZ-SV-RPF-1', @DocType = 'GOODSRECEIPT';
    IF (SELECT COUNT(*) FROM cst.CostLayer WHERE ItemCode = 'ZZ-SV-RPF') = 1
       AND (SELECT SUM(QtyRemaining) FROM cst.CostLayer WHERE ItemCode = 'ZZ-SV-RPF') = 10
        PRINT 'ok 65 - a replayed FIFO receipt opens one layer, not two -- REQ: COM-015, CST-005';
    ELSE PRINT 'not ok 65 - the replayed FIFO receipt opened more than one layer';

    /*  failure after the physical balance and movement are written: a negative value is not evidence */
    SELECT @mv0 = COUNT(*), @b0 = ISNULL(SUM(Quantity), 0) FROM inv.StockMovement WHERE ItemCode = 'ZZ-SV-RPL';
    SELECT @h0 = COUNT(*) FROM cst.ItemCostHistory WHERE ItemCode = 'ZZ-SV-RPL';
    SELECT @k0 = COUNT(*) FROM core.IdempotencyKey WHERE CorrelationId LIKE 'ZZSV-%';
    SET @err = '';
    BEGIN TRY
        EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-RPL', 'A', 'RECEIPT', 5, @Value = -1.00, @Corr = 'ZZSV-F1', @DocRef = 'ZZ-SV-RPL-F1', @DocType = 'GOODSRECEIPT';
    END TRY
    BEGIN CATCH
        SET @err = ERROR_MESSAGE();
    END CATCH
    IF @err LIKE '%STERP-E-0031%' PRINT 'ok 66 - a negative supplied value is refused with a stated reason -- REQ: ITM-015, COM-014';
    ELSE PRINT 'not ok 66 - the negative value was answered with: ' + @err;
    IF (SELECT COUNT(*) FROM inv.StockMovement WHERE ItemCode = 'ZZ-SV-RPL') = @mv0
       AND (SELECT SUM(QtyOnHand) FROM inv.StockBalance WHERE ItemCode = 'ZZ-SV-RPL') = 6
       AND (SELECT COUNT(*) FROM cst.ItemCostHistory WHERE ItemCode = 'ZZ-SV-RPL') = @h0
       AND (SELECT COUNT(*) FROM core.IdempotencyKey WHERE CorrelationId LIKE 'ZZSV-%') = @k0
        PRINT 'ok 67 - the failed movement left no movement, balance change, history row or idempotency key -- REQ: COM-015';
    ELSE PRINT 'not ok 67 - the failed movement left partial writes behind';
    EXEC #sv_is '68', 'and the pool is exactly as it was', 'ZZ-SV-RPL', 'RDG', 'A', 6, 60.00, 10.000000;

    SET @err = '';
    BEGIN TRY
        EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-RPL', 'A', 'ISSUE', 9999, @Corr = 'ZZSV-F2', @DocRef = 'ZZ-SV-RPL-F2', @DocType = 'DESPATCH';
    END TRY
    BEGIN CATCH
        SET @err = ERROR_MESSAGE();
    END CATCH
    IF @err LIKE '%STERP-E-0015%' AND (SELECT COUNT(*) FROM core.IdempotencyKey WHERE CorrelationId = 'ZZSV-F2') = 0
        PRINT 'ok 69 - an issue beyond physical stock is refused and the pool is not asked to value it -- REQ: ITM-016, D-09';
    ELSE PRINT 'not ok 69 - the over-issue was answered with: ' + @err;
    EXEC #sv_is '70', 'the pool still holds 6 worth 60.00 after the refusals', 'ZZ-SV-RPL', 'RDG', 'A', 6, 60.00, 10.000000;
    IF EXISTS (SELECT 1 FROM core.IdempotencyKey WHERE CorrelationId = 'ZZSV-R1' AND Status = 'COMPLETED')
        PRINT 'ok 71 - the replayed instruction is recorded once, as completed -- REQ: COM-015';
    ELSE PRINT 'not ok 71 - the idempotency key for the receipt is missing or not completed';
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    PRINT 'not ok 62 to 71 - unexpected error in the replay and failure scenario: ' + @msg;
END CATCH
GO
/*  72 to 86 - pool dimensions, receipt-layer reversal and allowance follow-through.
    These exercise the boundaries not present in the original GUI reproduction. */
DECLARE @msg varchar(400), @first bigint, @newer bigint, @reverse bigint;
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC #sv_item 'ZZ-SV-DIM', 'FIFO';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-DIM', 'A', 'RECEIPT', 10, @Value = 50, @DocRef = 'ZZ-SV-DIM-R1', @MovId = @first OUTPUT;
    EXEC #sv_post 'BHM', 'STKA01', 'ZZ-SV-DIM', 'A', 'RECEIPT', 10, @Value = 80, @DocRef = 'ZZ-SV-DIM-B1';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-DIM', 'B', 'RECEIPT', 10, @Value = 120, @DocRef = 'ZZ-SV-DIM-RB';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-DIM', 'A', 'ISSUE', 2, @DocRef = 'ZZ-SV-DIM-I1';
    EXEC #sv_is '72', 'FIFO relief is confined to the issuing site and revision', 'ZZ-SV-DIM', 'RDG', 'A', 8, 40, 5;
    EXEC #sv_is '73', 'FIFO relief leaves the other site pool alone', 'ZZ-SV-DIM', 'BHM', 'A', 10, 80, 8;
    EXEC #sv_is '74', 'FIFO relief leaves the other revision pool alone', 'ZZ-SV-DIM', 'RDG', 'B', 10, 120, 12;
    IF EXISTS (SELECT 1 FROM cst.CostLayer WHERE MovementId = @first AND SiteCode = 'RDG' AND QtyRemaining = 8 AND ValueRemainingGBP = 40)
        PRINT 'ok 75 - the FIFO layer carries the pool site and the remaining value';
    ELSE PRINT 'not ok 75 - the FIFO layer lost its site or value';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-DIM', 'A', 'RECEIPT', 10, @Value = 90, @DocRef = 'ZZ-SV-DIM-R2', @MovId = @newer OUTPUT;
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-DIM', 'A', 'ISSUE', 10, @Unit = 99, @DocRef = 'ZZ-SV-DIM-R2',
                  @DocType = 'RECEIPTREVERSAL', @MovId = @reverse OUTPUT;
    EXEC #sv_is '76', 'a FIFO receipt reversal removes its original layer, not the oldest layer', 'ZZ-SV-DIM', 'RDG', 'A', 8, 40, 5;
    IF EXISTS (SELECT 1 FROM cst.CostLayer WHERE MovementId = @newer AND QtyRemaining = 0 AND ValueRemainingGBP = 0 AND IsClosed = 1)
       AND EXISTS (SELECT 1 FROM cst.CostLayer WHERE MovementId = @first AND QtyRemaining = 8 AND ValueRemainingGBP = 40)
        PRINT 'ok 77 - the reversed receipt layer is closed and the older layer is unchanged';
    ELSE PRINT 'not ok 77 - the reversal consumed the wrong layer';
    IF EXISTS (SELECT 1 FROM inv.StockMovement WHERE MovementId = @reverse AND ValueGBP = -90)
        PRINT 'ok 78 - a receipt reversal uses the original posted value even when the caller supplies a different price';
    ELSE PRINT 'not ok 78 - the receipt reversal did not retain original valuation';

    INSERT cst.CostPolicy (CompanyCode, ValuationGroup, EffectiveFrom, CostMethod, PoolBySite, PoolByRevision, ApprovedBy)
    VALUES ('STC', 'ZZSVR', '2026-01-01', 'MWA', 1, 0, 'TEST');
    EXEC #sv_item 'ZZ-SV-REV', 'FIFO', 'ZZSVR';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-REV', 'A', 'RECEIPT', 4, @Value = 20;
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-REV', 'B', 'RECEIPT', 3, @Value = 30;
    EXEC #sv_is '79', 'approved policy overrides the item method and pools all revisions', 'ZZ-SV-REV', 'RDG', '', 7, 50, 7.142857;
    IF (SELECT COUNT(*) FROM cst.ItemCost WHERE ItemCode = 'ZZ-SV-REV') = 1
       AND NOT EXISTS (SELECT 1 FROM cst.CostLayer WHERE ItemCode = 'ZZ-SV-REV')
        PRINT 'ok 80 - revisionless moving average policy creates one pool and no FIFO layers';
    ELSE PRINT 'not ok 80 - revisionless policy split the pool or created layers';
    UPDATE cst.ItemCost SET NrvUnitValue = 5, AllowanceGBP = 15 WHERE ItemCode = 'ZZ-SV-REV';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-REV', 'C', 'RECEIPT', 1, @Value = 10;
    IF EXISTS (SELECT 1 FROM cst.ItemCost WHERE ItemCode = 'ZZ-SV-REV' AND AllowanceGBP = 20)
        PRINT 'ok 81 - receipt recalculates the allowance against the retained NRV';
    ELSE PRINT 'not ok 81 - receipt left a stale NRV allowance';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-REV', 'A', 'ISSUE', 2;
    EXEC #sv_is '82', 'revisionless issue uses the shared current average', 'ZZ-SV-REV', 'RDG', '', 6, 45, 7.5;
    IF EXISTS (SELECT 1 FROM cst.ItemCost WHERE ItemCode = 'ZZ-SV-REV' AND AllowanceGBP = 15 AND NrvUnitValue = 5)
        PRINT 'ok 83 - issue recalculates allowance and preserves NRV';
    ELSE PRINT 'not ok 83 - issue lost NRV or left a stale allowance';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-REV', 'A', 'ISSUE', 2;
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-REV', 'B', 'ISSUE', 3;
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-REV', 'C', 'ISSUE', 1;
    IF EXISTS (SELECT 1 FROM cst.ItemCost WHERE ItemCode = 'ZZ-SV-REV' AND QtyOnHand = 0 AND CarryingValueGBP = 0 AND AllowanceGBP = 0)
        PRINT 'ok 84 - emptying the shared pool clears its allowance too';
    ELSE PRINT 'not ok 84 - the empty pool retained an allowance';
    EXEC #sv_item 'ZZ-SV-ALLOW', 'MWA';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-ALLOW', 'A', 'RECEIPT', 10, @Value = 100;
    UPDATE cst.ItemCost SET AllowanceGBP = 20 WHERE ItemCode = 'ZZ-SV-ALLOW';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-ALLOW', 'A', 'ISSUE', 4;
    IF EXISTS (SELECT 1 FROM cst.ItemCost WHERE ItemCode = 'ZZ-SV-ALLOW' AND AllowanceGBP = 12 AND NrvUnitValue IS NULL)
        PRINT 'ok 85 - an allowance without unit NRV is relieved proportionately';
    ELSE PRINT 'not ok 85 - the allowance without unit NRV was not relieved proportionately';
    IF NOT EXISTS (SELECT 1 FROM cst.ItemCost WHERE ItemCode LIKE 'ZZ-SV-%' AND (QtyOnHand < 0 OR CarryingValueGBP < 0 OR AllowanceGBP < 0))
        PRINT 'ok 86 - dimensioned relief and reversals leave no negative pool or allowance';
    ELSE PRINT 'not ok 86 - dimensioned valuation produced a negative balance';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 72 to 86 - unexpected error in dimensions and allowance: ' + @msg;
END CATCH
GO
/*  87 to 89 - legacy wildcard origin and a separately established destination pool.
    Pool identity, not merely a '*' site on either leg, determines transfer neutrality. */
DECLARE @msg varchar(400), @out bigint, @carry decimal(18,2);
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC #sv_item 'ZZ-SV-MIX', 'MWA';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-MIX', 'A', 'OPENING', 20;
    EXEC #sv_post 'BHM', 'STKA01', 'ZZ-SV-MIX', 'A', 'OPENING', 5;
    EXEC #sv_pool 'ZZ-SV-MIX', '*', '', 'MWA', 20, 100, 5;
    EXEC #sv_pool 'ZZ-SV-MIX', 'BHM', 'A', 'MWA', 5, 50, 10;
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-MIX', 'A', 'TRANSFEROUT', 4, @CpSite = 'BHM', @CpLoc = 'STKA01', @MovId = @out OUTPUT;
    SELECT @carry = ABS(ValueGBP) FROM inv.StockMovement WHERE MovementId = @out;
    EXEC #sv_post 'BHM', 'STKA01', 'ZZ-SV-MIX', 'A', 'TRANSFERIN', 4, @Value = @carry, @CpSite = 'RDG', @CpLoc = 'STKA01';
    EXEC #sv_is '87', 'wildcard fallback origin is relieved when the destination resolves to a different pool', 'ZZ-SV-MIX', '*', '', 16, 80, 5;
    EXEC #sv_is '88', 'the exact destination receives the value relieved from the wildcard origin', 'ZZ-SV-MIX', 'BHM', 'A', 9, 70, 7.777778;
    IF (SELECT SUM(CarryingValueGBP) FROM cst.ItemCost WHERE ItemCode = 'ZZ-SV-MIX') = 150
       AND (SELECT COUNT(*) FROM cst.ItemCostHistory WHERE ItemCode = 'ZZ-SV-MIX') = 2
        PRINT 'ok 89 - mixed legacy pool transfer preserves company value and records both pool identities';
    ELSE PRINT 'not ok 89 - mixed legacy pool transfer changed company value or omitted its history';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 87 to 89 - unexpected error in wildcard transfer: ' + @msg;
END CATCH
GO
/*  90 to 94 - GUI DNS00000003 regression. FINISHED policy is company-wide, but
    the established standard pool is site-specific. Policy does not migrate stock.
    An empty wildcard pool, including one left by an older posting, must not mask it. */
DECLARE @msg varchar(400), @movement bigint;
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT cst.CostPolicy (CompanyCode, ValuationGroup, EffectiveFrom, CostMethod, PoolBySite, PoolByRevision, ApprovedBy)
    VALUES ('STC', 'ZZSVSTD', '2026-01-01', 'STD', 0, 1, 'TEST');
    EXEC #sv_item 'ZZ-SV-POLSTD', 'STD', 'ZZSVSTD';
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-POLSTD', 'C', 'OPENING', 31;
    EXEC #sv_pool 'ZZ-SV-POLSTD', 'RDG', 'C', 'STD', 31, 704.63, 22.73, 23.4119;
    EXEC #sv_pool 'ZZ-SV-POLSTD', '*', 'C', 'STD', 0, 0, 0;
    EXEC #sv_post 'RDG', 'STKA01', 'ZZ-SV-POLSTD', 'C', 'ISSUE', 8,
                  @Unit = 23.4119, @Value = 187.30, @MovId = @movement OUTPUT;
    EXEC #sv_is '90', 'company-wide policy cannot shadow an established exact-site standard pool', 'ZZ-SV-POLSTD', 'RDG', 'C', 23, 517.33, 22.492609;
    IF EXISTS (SELECT 1 FROM inv.StockMovement WHERE MovementId = @movement AND Revision = 'C' AND CostMethod = 'STD'
               AND UnitCost = 23.4125 AND ValueGBP = -187.30)
        PRINT 'ok 91 - the standard despatch keeps its revision and relieves the posted 187.30';
    ELSE PRINT 'not ok 91 - standard despatch lost its revision or value';
    IF EXISTS (SELECT 1 FROM cst.ItemCostHistory WHERE MovementId = @movement AND SiteCode = 'RDG'
               AND QtyBefore = 31 AND QtyAfter = 23 AND ValueBeforeGBP = 704.63 AND ValueAfterGBP = 517.33)
        PRINT 'ok 92 - standard despatch history identifies the established pool and its actual before/after';
    ELSE PRINT 'not ok 92 - standard despatch valued an empty policy pool';
    EXEC #sv_is '93', 'an empty legacy wildcard remains unchanged rather than masking the site pool', 'ZZ-SV-POLSTD', '*', 'C', 0, 0, 0;
    IF (SELECT StandardUnitCost FROM cst.ItemCost WHERE ItemCode = 'ZZ-SV-POLSTD' AND SiteCode = 'RDG') = 23.4119
       AND NOT EXISTS (SELECT 1 FROM core.ExceptionItem WHERE Module = 'CST' AND DocumentRef = 'ZZ-SV' AND Quantity = 8)
        PRINT 'ok 94 - the approved standard survives and no false valuation shortfall is raised';
    ELSE PRINT 'not ok 94 - standard despatch changed the standard or raised a false shortfall';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 90 to 94 - unexpected error in established standard pool regression: ' + @msg;
END CATCH
GO
/*  Remove what the committed scenarios wrote. */
DELETE FROM cst.ItemCostHistory WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-SV%';
DELETE FROM cst.CostLayer       WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-SV%';
DELETE FROM cst.ItemCost        WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-SV%';
DELETE FROM inv.StockMovement   WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-SV%';
DELETE FROM inv.StockBalance    WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-SV%';
DELETE FROM core.IdempotencyKey WHERE CorrelationId LIKE 'ZZSV-%';
DELETE FROM core.ExceptionItem  WHERE CompanyCode = 'STC' AND Module = 'CST' AND DocumentRef LIKE 'ZZ-SV%';
DELETE FROM inv.Item            WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-SV%';
GO
