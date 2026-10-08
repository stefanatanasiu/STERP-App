/*  Item supply rules, the copy rule at the data layer, and product compliance and packaging
    data.  These close the gaps left by db\tests\itm-items.sql and db\tests\itm-stock.sql:
    the site planning parameters and purchase pack rules that procurement rounds against
    (ITM-008), the fact that a copied identity starts with nothing attached to it (ITM-021),
    and the compliance and packaging data the packaging data report RPT-019 reconciles to
    (ITM-027).  Every assertion runs inside its own transaction and rolls back, and every
    fixture key is prefixed ZZITMGAP in company STC.
    REQ: ITM-008, ITM-021, ITM-027, RPT-019
    AC: AC-071 pack rounding is procurement's, not engineering's
    AC: AC-132 dangerous goods on despatch, packaging weights by material for a period */
SET NOCOUNT ON;
GO
PRINT '# itm-gaps';
DECLARE @msg varchar(400), @n int, @dec decimal(19,6), @rdg decimal(19,6), @bhm decimal(19,6),
        @before decimal(19,6), @after decimal(19,6), @kg decimal(19,6), @recy decimal(19,6),
        @co varchar(10) = 'STC', @item varchar(40) = 'LINK-QUICK-11S',
        @pkg varchar(40) = 'HUB-REAR-148-BOOST-XD';

/*  1 - a purchase pack that holds nothing could never round an order up, so inv.ItemUom will
       not record one (CK_ItemUom_F).  Procurement rounding can therefore never divide by zero. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT inv.ItemUom (CompanyCode, ItemCode, UomCode, UsageType, StockUnitsPerUom, EffectiveFrom, CreatedBy)
    VALUES (@co, @item, 'SLV', 'PACK', 0, '2027-01-01', 'TEST');
    PRINT 'not ok 1 - a purchase pack holding nothing was accepted -- REQ: ITM-008';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%CK_ItemUom_F%' PRINT 'ok 1 - a purchase pack must hold a positive quantity -- REQ: ITM-008';
    ELSE PRINT 'not ok 1 - unexpected error: ' + @msg;
END CATCH

/*  2 - the purchase pack is effective dated, so an order rounds against the pack that was in
       force on its own date.  The sleeve of 25 became a sleeve of 20 on 1 October 2026 and the
       earlier date still sees 25.  REQ: ITM-008 */
BEGIN TRANSACTION;
SELECT TOP (1) @before = StockUnitsPerUom FROM inv.ItemUom
 WHERE CompanyCode = @co AND ItemCode = @item AND UsageType = 'PURCHASE'
   AND EffectiveFrom <= '2026-09-15' AND (EffectiveTo IS NULL OR EffectiveTo >= '2026-09-15')
 ORDER BY EffectiveFrom DESC;
SELECT TOP (1) @after = StockUnitsPerUom FROM inv.ItemUom
 WHERE CompanyCode = @co AND ItemCode = @item AND UsageType = 'PURCHASE'
   AND EffectiveFrom <= '2026-10-15' AND (EffectiveTo IS NULL OR EffectiveTo >= '2026-10-15')
 ORDER BY EffectiveFrom DESC;
IF @before = 25 AND @after = 20
    PRINT 'ok 2 - the purchase pack in force moves from 25 to 20 by date, it is not rewritten -- REQ: ITM-008';
ELSE PRINT 'not ok 2 - the pack in force was ' + ISNULL(CONVERT(varchar(20), @before), 'null') + ' then '
         + ISNULL(CONVERT(varchar(20), @after), 'null') + ', expected 25 then 20 -- REQ: ITM-008';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  3 - planning parameters belong to a site.  Birmingham may buy the same part in fives while
       Reading buys it in twenty-fives, and neither site inherits the other's rule.
       REQ: ITM-008   AC: AC-071 */
BEGIN TRANSACTION;
BEGIN TRY
    INSERT inv.ItemSite (CompanyCode, SiteCode, ItemCode, DefaultWarehouseCode, DefaultLocationCode, MakeBuy,
                         LeadTimeDays, MinOrderQty, PurchaseMultipleQty, ReorderPointQty, SafetyStockQty,
                         MaxStockQty, AbcClass, CreatedBy)
    VALUES (@co, 'BHM', @item, 'MAIN', 'STKA01', 'BUY', 3, 5, 5, 20, 10, 200, 'C', 'TEST');
    SELECT @rdg = PurchaseMultipleQty FROM inv.ItemSite WHERE CompanyCode = @co AND SiteCode = 'RDG' AND ItemCode = @item;
    SELECT @bhm = PurchaseMultipleQty FROM inv.ItemSite WHERE CompanyCode = @co AND SiteCode = 'BHM' AND ItemCode = @item;
    IF @rdg = 25 AND @bhm = 5
        PRINT 'ok 3 - the same part has a pack of 25 at Reading and a pack of 5 at Birmingham -- REQ: ITM-008';
    ELSE PRINT 'not ok 3 - the packs were ' + ISNULL(CONVERT(varchar(20), @rdg), 'null') + ' and '
             + ISNULL(CONVERT(varchar(20), @bhm), 'null') + ' -- REQ: ITM-008';

    /*  4 - and the rounding they produce differs by site: 65 pieces of engineering demand buy
           three sleeves (75) at Reading and thirteen packs (65) at Birmingham.  The excess is
           created by procurement rounding; the demand of 65 is untouched either way. */
    IF CEILING(65.0 / @rdg) * @rdg = 75 AND CEILING(65.0 / @rdg) * @rdg - 65 = 10
       AND CEILING(65.0 / @bhm) * @bhm = 65 AND CEILING(65.0 / @bhm) * @bhm - 65 = 0
        PRINT 'ok 4 - 65 of demand rounds to 75 at Reading with 10 of excess supply, and to 65 at Birmingham -- REQ: ITM-008, AC-071';
    ELSE PRINT 'not ok 4 - the site pack rules did not produce 75/10 and 65/0 -- REQ: ITM-008';

    /*  5 - an approved make or buy choice may also be taken at the site: Birmingham can be told
           to make a part the company normally buys, and the item default is not overwritten. */
    UPDATE inv.ItemSite SET MakeBuy = 'MAKE' WHERE CompanyCode = @co AND SiteCode = 'BHM' AND ItemCode = @item;
    SET @n = 0;
    SELECT @n = COUNT(*) FROM inv.Item i
      JOIN inv.ItemSite s ON s.CompanyCode = i.CompanyCode AND s.ItemCode = i.ItemCode
     WHERE i.CompanyCode = @co AND i.ItemCode = @item AND s.SiteCode = 'BHM'
       AND ISNULL(s.MakeBuy, i.MakeBuy) = 'MAKE' AND i.MakeBuy = 'BUY';
    IF @n = 1 PRINT 'ok 5 - a site may hold its own approved make or buy choice without changing the item -- REQ: ITM-008';
    ELSE PRINT 'not ok 5 - the site make or buy choice did not override the item default -- REQ: ITM-008';
END TRY
BEGIN CATCH
    PRINT 'not ok 3 - unexpected error: ' + ERROR_MESSAGE();
END CATCH
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  6 - the seeded planning data is usable: nothing carries a negative lead time, minimum or
       pack multiple, because a planner cannot round against a negative rule.  REQ: ITM-008 */
BEGIN TRANSACTION;
SELECT @n = COUNT(*) FROM inv.ItemSite
 WHERE ISNULL(MinOrderQty, 0) < 0 OR ISNULL(PurchaseMultipleQty, 0) < 0 OR ISNULL(ReorderPointQty, 0) < 0
    OR ISNULL(SafetyStockQty, 0) < 0 OR LeadTimeDays < 0;
IF @n = 0 PRINT 'ok 6 - every site planning record holds usable lead times, minimums and pack multiples -- REQ: ITM-008';
ELSE PRINT 'not ok 6 - ' + CONVERT(varchar(10), @n) + ' site planning records are not usable -- REQ: ITM-008';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  7 - an item identity is never reused, so a copy always needs a code of its own.
       REQ: ITM-021 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, CreatedBy)
    VALUES (@co, @item, 'A second item on an existing code', 'BOUGHT', 'EA', 'TEST');
    PRINT 'not ok 7 - an existing item code was reused -- REQ: ITM-021';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%PK_Item%' OR @msg LIKE '%duplicate key%'
        PRINT 'ok 7 - an item identity is never reused, so a copy must be given its own code -- REQ: ITM-021';
    ELSE PRINT 'not ok 7 - unexpected error: ' + @msg;
END CATCH

/*  8 - a new identity starts with nothing attached: no balance, no movement, no serial and no
       reservation follow a code into existence.  REQ: ITM-021   AC: AC-069 */
BEGIN TRANSACTION;
BEGIN TRY
    INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, LifecycleStatus, CreatedBy)
    VALUES (@co, 'ZZITMGAP-SQL-NEW', 'ZZ a brand new identity', 'BOUGHT', 'EA', 'DRAFT', 'TEST');
    SELECT @n = (SELECT COUNT(*) FROM inv.StockBalance WHERE CompanyCode = @co AND ItemCode = 'ZZITMGAP-SQL-NEW')
               + (SELECT COUNT(*) FROM inv.StockMovement WHERE CompanyCode = @co AND ItemCode = 'ZZITMGAP-SQL-NEW')
               + (SELECT COUNT(*) FROM inv.Serial WHERE CompanyCode = @co AND ItemCode = 'ZZITMGAP-SQL-NEW')
               + (SELECT COUNT(*) FROM inv.StockReservation WHERE CompanyCode = @co AND ItemCode = 'ZZITMGAP-SQL-NEW');
    IF @n = 0 PRINT 'ok 8 - a new item code carries no balance, movement, serial or reservation -- REQ: ITM-021, AC-069';
    ELSE PRINT 'not ok 8 - ' + CONVERT(varchar(10), @n) + ' records attached themselves to a new code -- REQ: ITM-021';
END TRY
BEGIN CATCH
    PRINT 'not ok 8 - unexpected error: ' + ERROR_MESSAGE();
END CATCH
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  9 - dangerous goods data is either complete or absent.  A UN number on its own cannot be
       printed on a despatch note, so no item is allowed to hold one without its class and
       packing group.  REQ: ITM-027   AC: AC-132 */
BEGIN TRANSACTION;
SELECT @n = COUNT(*) FROM inv.Item
 WHERE DangerousGoodsUn IS NOT NULL AND DangerousGoodsUn <> ''
   AND (DangerousGoodsClass IS NULL OR DangerousGoodsClass = ''
        OR DangerousGoodsPackGroup IS NULL OR DangerousGoodsPackGroup = '');
IF @n = 0 PRINT 'ok 9 - every dangerous goods item can be labelled: UN number, class and packing group -- REQ: ITM-027, AC-132';
ELSE PRINT 'not ok 9 - ' + CONVERT(varchar(10), @n) + ' items hold a UN number with no class or packing group -- REQ: ITM-027';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/* 10 - a company that has enabled the UKCA flag must hold the declaration of conformity the
       flag asserts; the mark is not kept without its evidence.  REQ: ITM-027 */
BEGIN TRANSACTION;
SELECT @n = COUNT(*) FROM inv.Item WHERE UkcaMarked = 1 AND (ConformityDocRef IS NULL OR ConformityDocRef = '');
IF @n = 0 PRINT 'ok 10 - every UKCA marked item names its declaration of conformity -- REQ: ITM-027';
ELSE PRINT 'not ok 10 - ' + CONVERT(varchar(10), @n) + ' UKCA marked items hold no conformity reference -- REQ: ITM-027';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/* 11 - AC-132.  1,000 units despatched in the period, each with 0.05 kg of plastic packaging at
       30% recycled content, reconcile to 50 kg of plastic including 15 kg of recycled material.
       This is the same join RPT-019 (PKGRPT) makes: despatched quantity from the movements of
       the period times the packaging composition held on the item.
       REQ: ITM-027, RPT-019   AC: AC-132 */
BEGIN TRANSACTION;
BEGIN TRY
    INSERT inv.StockMovement (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode, MovementType,
                              Quantity, UomCode, MovementDate, PostingDate, SourceModule, SourceDocType,
                              SourceDocRef, CorrelationId, CreatedBy)
    VALUES (@co, 'RDG', 'MAIN', 'STKA01', @pkg, 'DESPATCH', -600, 'EA', '2026-09-10', '2026-09-10', 'so',
            'DESPATCH', 'ZZITMGAP-DN-1', 'ZZITMGAP-PKG-1', 'TEST'),
           (@co, 'RDG', 'MAIN', 'STKA01', @pkg, 'DESPATCH', -400, 'EA', '2026-09-24', '2026-09-24', 'so',
            'DESPATCH', 'ZZITMGAP-DN-2', 'ZZITMGAP-PKG-2', 'TEST'),
           (@co, 'RDG', 'MAIN', 'STKA01', @pkg, 'RECEIPT',   250, 'EA', '2026-09-15', '2026-09-15', 'po',
            'RECEIPT',  'ZZITMGAP-GR-1', 'ZZITMGAP-PKG-3', 'TEST');
    SELECT @kg  = SUM(d.Despatched * p.WeightGrams) / 1000.0,
           @recy = SUM(d.Despatched * p.WeightGrams * ISNULL(p.RecycledContentPct, 0) / 100.0) / 1000.0
      FROM inv.ItemPackaging p
      CROSS APPLY (SELECT Despatched = ISNULL(SUM(CASE WHEN m.Quantity < 0 THEN -m.Quantity ELSE 0 END), 0)
                     FROM inv.StockMovement m
                    WHERE m.CompanyCode = p.CompanyCode AND m.ItemCode = p.ItemCode
                      AND m.PostingDate >= '2026-09-01' AND m.PostingDate <= '2026-09-30'
                      AND m.CorrelationId LIKE 'ZZITMGAP%') d
     WHERE p.CompanyCode = @co AND p.ItemCode = @pkg AND p.PackagingMaterial = 'PLASTIC';
    IF @kg = 50 AND @recy = 15
        PRINT 'ok 11 - 1,000 units despatched in the period are 50 kg of plastic packaging, 15 kg of it recycled -- REQ: ITM-027, RPT-019, AC-132';
    ELSE PRINT 'not ok 11 - the period reconciled to ' + ISNULL(CONVERT(varchar(30), @kg), 'null') + ' kg of plastic and '
             + ISNULL(CONVERT(varchar(30), @recy), 'null') + ' kg recycled, expected 50 and 15 -- REQ: ITM-027, RPT-019';

    /* 12 - a receipt is not a despatch.  Only what left the building is reported, so the 250
           received in the same period add nothing to the packaging placed on the market. */
    SELECT @dec = ISNULL(SUM(CASE WHEN m.Quantity < 0 THEN -m.Quantity ELSE 0 END), 0)
      FROM inv.StockMovement m
     WHERE m.CompanyCode = @co AND m.ItemCode = @pkg AND m.CorrelationId LIKE 'ZZITMGAP%'
       AND m.PostingDate >= '2026-09-01' AND m.PostingDate <= '2026-09-30';
    IF @dec = 1000 PRINT 'ok 12 - only the quantity that left stock is counted, the receipt of 250 is not -- REQ: ITM-027, RPT-019';
    ELSE PRINT 'not ok 12 - the despatched quantity was ' + CONVERT(varchar(30), @dec) + ', expected 1000 -- REQ: ITM-027';

    /* 13 - a despatch outside the period belongs to another return.  REQ: ITM-027, RPT-019 */
    SELECT @dec = ISNULL(SUM(CASE WHEN m.Quantity < 0 THEN -m.Quantity ELSE 0 END), 0)
      FROM inv.StockMovement m
     WHERE m.CompanyCode = @co AND m.ItemCode = @pkg AND m.CorrelationId LIKE 'ZZITMGAP%'
       AND m.PostingDate >= '2026-10-01' AND m.PostingDate <= '2026-10-31';
    IF @dec = 0 PRINT 'ok 13 - the packaging data report is bounded by its period -- REQ: ITM-027, RPT-019';
    ELSE PRINT 'not ok 13 - ' + CONVERT(varchar(30), @dec) + ' units leaked into the next period -- REQ: ITM-027';
END TRY
BEGIN CATCH
    PRINT 'not ok 11 - unexpected error: ' + ERROR_MESSAGE();
END CATCH
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/* 14 - packaging weight is held by material, so paper is never taxed as plastic: the Plastic
       Packaging Tax flag is only ever set on a plastic component.  REQ: ITM-027 */
BEGIN TRANSACTION;
SELECT @n = COUNT(*) FROM inv.ItemPackaging WHERE IsPlasticPackagingTax = 1 AND PackagingMaterial <> 'PLASTIC';
IF @n = 0 PRINT 'ok 14 - only plastic packaging is flagged for Plastic Packaging Tax -- REQ: ITM-027';
ELSE PRINT 'not ok 14 - ' + CONVERT(varchar(10), @n) + ' non-plastic components are flagged for the plastic tax -- REQ: ITM-027';
SELECT @n = COUNT(*) FROM inv.ItemPackaging
 WHERE WeightGrams <= 0 OR RecycledContentPct < 0 OR RecycledContentPct > 100;
IF @n = 0 PRINT 'ok 15 - every packaging component has a positive weight and a recycled content between 0 and 100 -- REQ: ITM-027, RPT-019';
ELSE PRINT 'not ok 15 - ' + CONVERT(varchar(10), @n) + ' packaging components cannot be reported -- REQ: ITM-027';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/* 16 - nothing from this suite survives it. */
SELECT @n = (SELECT COUNT(*) FROM inv.Item WHERE ItemCode LIKE 'ZZITMGAP%')
           + (SELECT COUNT(*) FROM inv.ItemSite WHERE ItemCode LIKE 'ZZITMGAP%')
           + (SELECT COUNT(*) FROM inv.StockMovement WHERE CorrelationId LIKE 'ZZITMGAP%');
IF @n = 0 PRINT 'ok 16 - the suite left no fixtures behind -- REQ: ITM-008, ITM-021, ITM-027';
ELSE PRINT 'not ok 16 - ' + CONVERT(varchar(10), @n) + ' fixtures survived the rollback -- REQ: ITM-021';

PRINT '# itm-gaps done';
GO
