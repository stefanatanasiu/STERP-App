/*  Stock availability, enquiry, movement and reservation.  The availability buckets, the single
    stock contract and the reservation primitive are all exercised against the seeded catalogue.
    Every assertion runs inside its own transaction and rolls back, so no stock is left behind.
    REQ: ITM-011, ITM-012, ITM-013, ITM-014, ITM-015, ITM-016, ITM-017, COM-005, COM-009, D-09 */
SET NOCOUNT ON;
GO
PRINT '# itm-stock';
DECLARE @msg varchar(400), @item varchar(40), @lotitem varchar(40), @loc varchar(20), @whs varchar(20),
        @n int, @before decimal(19,6), @after decimal(19,6), @free decimal(38,6), @took decimal(38,6);

SELECT TOP (1) @item = ItemCode FROM inv.Item
 WHERE CompanyCode = 'STC' AND TraceabilityType = 'NONE' AND LifecycleStatus = 'RELEASED'
   AND IsStocked = 1 AND IsIndivisible = 1 AND QuantumQty = 1 ORDER BY ItemCode;
SELECT TOP (1) @lotitem = ItemCode FROM inv.Item
 WHERE CompanyCode = 'STC' AND TraceabilityType = 'LOT' AND LifecycleStatus = 'RELEASED' ORDER BY ItemCode;
SELECT TOP (1) @whs = WarehouseCode, @loc = LocationCode FROM inv.Location
 WHERE CompanyCode = 'STC' AND SiteCode = 'RDG' AND LocationType = 'STOCK'
   AND IsAvailable = 1 AND IsOwnedStock = 1 AND IsActive = 1 ORDER BY LocationCode;

/*  1 - the availability view never carries an empty bucket. */
BEGIN TRANSACTION;
SELECT @n = COUNT(*) FROM inv.vw_StockAvailability WHERE CompanyCode = 'STC' AND QtyOnHand = 0 AND QtyInTransit = 0;
IF @n = 0 PRINT 'ok 1 - the availability view holds no empty stock buckets -- REQ: ITM-012';
ELSE PRINT 'not ok 1 - ' + CAST(@n AS varchar(10)) + ' empty buckets are reported -- REQ: ITM-012';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  2 - third party owned material in our custody is identified as such, never as our stock. */
BEGIN TRANSACTION;
SELECT @n = COUNT(*) FROM inv.vw_StockAvailability
 WHERE CompanyCode = 'STC' AND OwnerType <> 'OWNED' AND OwnerCode <> '' AND IsOwnedStock = 0;
IF @n > 0 PRINT 'ok 2 - ' + CAST(@n AS varchar(10)) + ' third party owned bucket(s) are reported with their owner -- REQ: ITM-012';
ELSE PRINT 'not ok 2 - no third party owned stock is distinguished -- REQ: ITM-012';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  3 - the enquiry refuses to call third party owned material available. */
BEGIN TRANSACTION;
DECLARE @enq TABLE (CompanyCode varchar(10), SiteCode varchar(10), WarehouseCode varchar(20), LocationCode varchar(20),
                    LocationType varchar(12), ItemCode varchar(40), Revision varchar(4), LotCode varchar(30), SerialNo varchar(40),
                    OwnerType varchar(10), OwnerCode varchar(20), QualityStatus varchar(12), QtyOnHand decimal(19,6),
                    QtyInTransit decimal(19,6), StockUomCode varchar(10), ExpiryDate date, TraceabilityType varchar(8),
                    LifecycleStatus varchar(10), QtyReserved decimal(38,6), IsEligible bit, NotEligibleReason varchar(60));
INSERT @enq EXEC inv.usp_StockEnquiry @CompanyCode = 'STC', @AsOf = '2026-10-15';
IF EXISTS (SELECT 1 FROM @enq WHERE OwnerType <> 'OWNED' AND IsEligible = 0 AND NotEligibleReason LIKE 'Third party owned%')
    PRINT 'ok 3 - customer and supplier owned stock is reported as held, not as available -- REQ: ITM-012, ITM-013';
ELSE PRINT 'not ok 3 - third party owned stock was not excluded from availability -- REQ: ITM-012';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  4 - quarantined and inspection stock is physical but not eligible. */
BEGIN TRANSACTION;
IF EXISTS (SELECT 1 FROM @enq WHERE QualityStatus = 'QUARANTINE' AND IsEligible = 0 AND NotEligibleReason LIKE 'Quality condition%')
   AND EXISTS (SELECT 1 FROM @enq WHERE QualityStatus = 'INSPECTION' AND IsEligible = 0)
    PRINT 'ok 4 - quarantined and inspection stock is on hand but not eligible -- REQ: ITM-013, ITM-017';
ELSE PRINT 'not ok 4 - a quality condition did not remove stock from availability -- REQ: ITM-013';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  5 - the enquiry scope filters, so a condition enquiry returns only that condition. */
BEGIN TRANSACTION;
DELETE @enq;
INSERT @enq EXEC inv.usp_StockEnquiry @CompanyCode = 'STC', @Condition = 'QUARANTINE', @AsOf = '2026-10-15';
IF EXISTS (SELECT 1 FROM @enq) AND NOT EXISTS (SELECT 1 FROM @enq WHERE QualityStatus <> 'QUARANTINE')
    PRINT 'ok 5 - a quality condition enquiry returns only that condition -- REQ: ITM-012, ITM-023';
ELSE PRINT 'not ok 5 - the condition filter did not restrict the enquiry -- REQ: ITM-012';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  6 - a receipt is the only way the balance rises, and it rises by exactly the quantity. */
BEGIN TRANSACTION;
BEGIN TRY
    SELECT @before = ISNULL(SUM(QtyOnHand), 0) FROM inv.StockBalance
     WHERE CompanyCode = 'STC' AND SiteCode = 'RDG' AND WarehouseCode = @whs AND LocationCode = @loc AND ItemCode = @item;
    EXEC inv.usp_PostStockMovement @CompanyCode = 'STC', @SiteCode = 'RDG', @WarehouseCode = @whs, @LocationCode = @loc,
         @ItemCode = @item, @MovementType = 'RECEIPT', @Quantity = 50, @MovementDate = '2026-10-15', @UserCode = 'WHSE',
         @SourceDocType = 'TEST', @SourceDocRef = 'TST-MOVE-1';
    SELECT @after = ISNULL(SUM(QtyOnHand), 0) FROM inv.StockBalance
     WHERE CompanyCode = 'STC' AND SiteCode = 'RDG' AND WarehouseCode = @whs AND LocationCode = @loc AND ItemCode = @item;
    IF @after = @before + 50 AND EXISTS (SELECT 1 FROM inv.StockMovement WHERE CompanyCode = 'STC' AND ItemCode = @item
                                           AND SourceDocRef = 'TST-MOVE-1' AND Quantity = 50 AND MovementType = 'RECEIPT')
        PRINT 'ok 6 - a receipt of 50 raised the balance from ' + CAST(@before AS varchar(30)) + ' to ' + CAST(@after AS varchar(30)) + ' and left a movement -- REQ: ITM-015, ITM-016';
    ELSE PRINT 'not ok 6 - the balance moved from ' + CAST(@before AS varchar(30)) + ' to ' + CAST(@after AS varchar(30)) + ' -- REQ: ITM-015';
END TRY
BEGIN CATCH
    PRINT 'not ok 6 - unexpected error: ' + ERROR_MESSAGE();
END CATCH
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  7 - a movement may never create a negative physical balance (D-09). */
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC inv.usp_PostStockMovement @CompanyCode = 'STC', @SiteCode = 'RDG', @WarehouseCode = @whs, @LocationCode = @loc,
         @ItemCode = @item, @MovementType = 'ISSUE', @Quantity = 9999999, @MovementDate = '2026-10-15', @UserCode = 'WHSE',
         @SourceDocType = 'TEST', @SourceDocRef = 'TST-MOVE-2';
    PRINT 'not ok 7 - an issue drove the balance negative -- REQ: ITM-016, D-09';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0015%' PRINT 'ok 7 - an issue that would create negative stock is refused -- REQ: ITM-016, D-09';
    ELSE PRINT 'not ok 7 - unexpected error: ' + @msg;
END CATCH

/*  8 - the permitted quantity increment is policed on the movement, not only on the screen. */
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC inv.usp_PostStockMovement @CompanyCode = 'STC', @SiteCode = 'RDG', @WarehouseCode = @whs, @LocationCode = @loc,
         @ItemCode = @item, @MovementType = 'RECEIPT', @Quantity = 2.5, @MovementDate = '2026-10-15', @UserCode = 'WHSE',
         @SourceDocType = 'TEST', @SourceDocRef = 'TST-MOVE-3';
    PRINT 'not ok 8 - a fractional quantity of an indivisible item was accepted -- REQ: ITM-005, ITM-007';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0028%' PRINT 'ok 8 - a quantity that breaks the item quantum is refused -- REQ: ITM-005, ITM-007';
    ELSE PRINT 'not ok 8 - unexpected error: ' + @msg;
END CATCH

/*  9 - a lot controlled item cannot move without its lot reference. */
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC inv.usp_PostStockMovement @CompanyCode = 'STC', @SiteCode = 'RDG', @WarehouseCode = @whs, @LocationCode = @loc,
         @ItemCode = @lotitem, @MovementType = 'RECEIPT', @Quantity = 10, @MovementDate = '2026-10-15', @UserCode = 'WHSE',
         @SourceDocType = 'TEST', @SourceDocRef = 'TST-MOVE-4';
    PRINT 'not ok 9 - a lot controlled item moved without a lot -- REQ: ITM-011';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0027%' PRINT 'ok 9 - a lot controlled item without a lot reference is refused -- REQ: ITM-011';
    ELSE PRINT 'not ok 9 - unexpected error: ' + @msg;
END CATCH

/* 10 - stock cannot be posted into a closed accounting period. */
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC inv.usp_PostStockMovement @CompanyCode = 'STC', @SiteCode = 'RDG', @WarehouseCode = @whs, @LocationCode = @loc,
         @ItemCode = @item, @MovementType = 'RECEIPT', @Quantity = 5, @MovementDate = '2026-05-15', @UserCode = 'WHSE',
         @SourceDocType = 'TEST', @SourceDocRef = 'TST-MOVE-5';
    PRINT 'not ok 10 - a stock movement entered a closed period -- REQ: ITM-015, COM-009';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0009%' PRINT 'ok 10 - a movement into a closed period is refused -- REQ: ITM-015, COM-009';
    ELSE PRINT 'not ok 10 - unexpected error: ' + @msg;
END CATCH

/* 11 - an unknown location is refused, so stock cannot be invented in a place that does not exist. */
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC inv.usp_PostStockMovement @CompanyCode = 'STC', @SiteCode = 'RDG', @WarehouseCode = @whs, @LocationCode = 'NOWHERE',
         @ItemCode = @item, @MovementType = 'RECEIPT', @Quantity = 5, @MovementDate = '2026-10-15', @UserCode = 'WHSE',
         @SourceDocType = 'TEST', @SourceDocRef = 'TST-MOVE-6';
    PRINT 'not ok 11 - a movement was posted to an unknown location -- REQ: ITM-012, ITM-015';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0026%' PRINT 'ok 11 - a movement to an unknown location is refused -- REQ: ITM-012, ITM-015';
    ELSE PRINT 'not ok 11 - unexpected error: ' + @msg;
END CATCH

/* 12 - a reservation holds free stock for one demand and reduces the free quantity by that much. */
DECLARE @res TABLE (Action varchar(10), ReservationId bigint, QtyReserved decimal(38,6), QtyShortfall decimal(38,6),
                    QtyFreeBefore decimal(38,6), QtyFreeAfter decimal(38,6), QtyEligible decimal(38,6),
                    QtyReservedTotal decimal(38,6), QtyOwnDemand decimal(38,6), InvalidCount int);
BEGIN TRANSACTION;
BEGIN TRY
    EXEC inv.usp_PostStockMovement @CompanyCode = 'STC', @SiteCode = 'RDG', @WarehouseCode = @whs, @LocationCode = @loc,
         @ItemCode = @item, @MovementType = 'RECEIPT', @Quantity = 50, @MovementDate = '2026-10-15', @UserCode = 'WHSE',
         @SourceDocType = 'TEST', @SourceDocRef = 'TST-RESV-SETUP';
    INSERT @res EXEC inv.usp_ReserveStock @CompanyCode = 'STC', @ItemCode = @item, @Action = 'RESERVE',
         @DemandType = 'SALESORDER', @DemandRef = 'TST-RESV-1', @DemandLine = 1, @UserCode = 'WHSE',
         @SiteCode = 'RDG', @Quantity = 10, @AsOf = '2026-10-15';
    SELECT @free = QtyFreeBefore, @took = QtyReserved FROM @res;
    IF @took = 10 AND EXISTS (SELECT 1 FROM @res WHERE QtyFreeAfter = QtyFreeBefore - 10 AND QtyShortfall = 0)
       AND EXISTS (SELECT 1 FROM inv.StockReservation WHERE CompanyCode = 'STC' AND ItemCode = @item
                     AND DemandRef = 'TST-RESV-1' AND Status = 'ACTIVE' AND QtyReserved = 10)
        PRINT 'ok 12 - 10 of ' + CAST(@free AS varchar(30)) + ' free units are reserved for TST-RESV-1 -- REQ: ITM-014, ITM-013';
    ELSE PRINT 'not ok 12 - the reservation did not reduce the free quantity -- REQ: ITM-014';
END TRY
BEGIN CATCH
    PRINT 'not ok 12 - unexpected error: ' + ERROR_MESSAGE();
END CATCH
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/* 13 - releasing a reservation gives the stock back to everyone else. */
BEGIN TRANSACTION;
BEGIN TRY
    DELETE @res;
    EXEC inv.usp_PostStockMovement @CompanyCode = 'STC', @SiteCode = 'RDG', @WarehouseCode = @whs, @LocationCode = @loc,
         @ItemCode = @item, @MovementType = 'RECEIPT', @Quantity = 50, @MovementDate = '2026-10-15', @UserCode = 'WHSE',
         @SourceDocType = 'TEST', @SourceDocRef = 'TST-RESV-SETUP2';
    INSERT @res EXEC inv.usp_ReserveStock @CompanyCode = 'STC', @ItemCode = @item, @Action = 'RESERVE',
         @DemandType = 'SALESORDER', @DemandRef = 'TST-RESV-3', @DemandLine = 1, @UserCode = 'WHSE',
         @SiteCode = 'RDG', @Quantity = 12, @AsOf = '2026-10-15';
    SELECT @free = QtyFreeAfter FROM @res;
    DELETE @res;
    EXEC inv.usp_ReserveStock @CompanyCode = 'STC', @ItemCode = @item, @Action = 'RELEASE',
         @DemandType = 'SALESORDER', @DemandRef = 'TST-RESV-3', @DemandLine = 1, @UserCode = 'WHSE',
         @SiteCode = 'RDG', @AsOf = '2026-10-15';
    /*  The released quantity is only visible to the next demand that asks, because the proc
        reports the free quantity it measured on entry.                                      */
    INSERT @res EXEC inv.usp_ReserveStock @CompanyCode = 'STC', @ItemCode = @item, @Action = 'RESERVE',
         @DemandType = 'SALESORDER', @DemandRef = 'TST-RESV-6', @DemandLine = 1, @UserCode = 'WHSE',
         @SiteCode = 'RDG', @Quantity = 1, @AsOf = '2026-10-15';
    IF EXISTS (SELECT 1 FROM inv.StockReservation WHERE CompanyCode = 'STC' AND ItemCode = @item
                 AND DemandRef = 'TST-RESV-3' AND Status = 'RELEASED')
       AND EXISTS (SELECT 1 FROM @res WHERE QtyFreeBefore = @free + 12)
        PRINT 'ok 13 - releasing TST-RESV-3 returns 12 units to the free quantity -- REQ: ITM-014';
    ELSE PRINT 'not ok 13 - the released reservation did not return its stock -- REQ: ITM-014';
END TRY
BEGIN CATCH
    PRINT 'not ok 13 - unexpected error: ' + ERROR_MESSAGE();
END CATCH
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/* 14 - the same unit cannot be promised twice: an over reservation is refused outright. */
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC inv.usp_ReserveStock @CompanyCode = 'STC', @ItemCode = @item, @Action = 'RESERVE',
         @DemandType = 'SALESORDER', @DemandRef = 'TST-RESV-2', @DemandLine = 1, @UserCode = 'WHSE',
         @SiteCode = 'RDG', @Quantity = 99999999, @AsOf = '2026-10-15';
    PRINT 'not ok 14 - more stock was reserved than is free -- REQ: ITM-014, COM-005';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0015%' PRINT 'ok 14 - a reservation larger than the free quantity is refused -- REQ: ITM-014, COM-005';
    ELSE PRINT 'not ok 14 - unexpected error: ' + @msg;
END CATCH

/* 15 - a demand cannot consume more than it reserved. */
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC inv.usp_PostStockMovement @CompanyCode = 'STC', @SiteCode = 'RDG', @WarehouseCode = @whs, @LocationCode = @loc,
         @ItemCode = @item, @MovementType = 'RECEIPT', @Quantity = 50, @MovementDate = '2026-10-15', @UserCode = 'WHSE',
         @SourceDocType = 'TEST', @SourceDocRef = 'TST-RESV-SETUP3';
    EXEC inv.usp_ReserveStock @CompanyCode = 'STC', @ItemCode = @item, @Action = 'RESERVE',
         @DemandType = 'SALESORDER', @DemandRef = 'TST-RESV-4', @DemandLine = 1, @UserCode = 'WHSE',
         @SiteCode = 'RDG', @Quantity = 5, @AsOf = '2026-10-15';
    EXEC inv.usp_ReserveStock @CompanyCode = 'STC', @ItemCode = @item, @Action = 'CONSUME',
         @DemandType = 'SALESORDER', @DemandRef = 'TST-RESV-4', @DemandLine = 1, @UserCode = 'WHSE',
         @SiteCode = 'RDG', @Quantity = 9, @AsOf = '2026-10-15';
    PRINT 'not ok 15 - a demand consumed more than it reserved -- REQ: ITM-014';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0015%' PRINT 'ok 15 - issuing more than the demand reserved is refused -- REQ: ITM-014';
    ELSE PRINT 'not ok 15 - unexpected error: ' + @msg;
END CATCH

/* 16 - an unsupported reservation action is refused rather than guessed. */
BEGIN TRY
    BEGIN TRANSACTION;
    EXEC inv.usp_ReserveStock @CompanyCode = 'STC', @ItemCode = @item, @Action = 'PRETEND',
         @DemandType = 'SALESORDER', @DemandRef = 'TST-RESV-5', @UserCode = 'WHSE', @SiteCode = 'RDG',
         @Quantity = 1, @AsOf = '2026-10-15';
    PRINT 'not ok 16 - an unknown reservation action was carried out -- REQ: ITM-014';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-0015%' PRINT 'ok 16 - an unsupported reservation action is refused -- REQ: ITM-014';
    ELSE PRINT 'not ok 16 - unexpected error: ' + @msg;
END CATCH

PRINT '# itm-stock done';
GO
