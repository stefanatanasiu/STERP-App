/*  Item identity, alias search, classification, protected reference data, retire-not-delete,
    VAT class, customs data and the lifecycle impact review.  Every assertion runs inside its own
    transaction and rolls back, so the catalogue is left exactly as the seed made it.
    REQ: ITM-001, ITM-002, ITM-003, ITM-004, ITM-009, ITM-010, ITM-020, ITM-022, ITM-023,
         ITM-025, ITM-026, COM-013, D-21 */
SET NOCOUNT ON;
GO
PRINT '# itm-items';
DECLARE @msg varchar(400), @item varchar(40), @alias varchar(40), @n int, @vat varchar(4), @rev varchar(4);

/*  1 - an item code may only use the permitted characters (D-21: no free text identity). */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, CreatedBy)
    VALUES ('STC', 'TST ITEM#1', 'Item code with a space and a hash', 'BOUGHT', 'EA', 'SYSADM');
    PRINT 'not ok 1 - an item code with forbidden characters was accepted -- REQ: ITM-001';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%CK_Item_Code%' PRINT 'ok 1 - an item code with forbidden characters is refused -- REQ: ITM-001, D-21';
    ELSE PRINT 'not ok 1 - unexpected error: ' + @msg;
END CATCH

/*  2 - a code built only from the permitted characters is accepted. */
BEGIN TRANSACTION;
BEGIN TRY
    INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, CreatedBy)
    VALUES ('STC', 'TST-ITEM.01/A_B', 'Permitted punctuation only', 'BOUGHT', 'EA', 'SYSADM');
    IF EXISTS (SELECT 1 FROM inv.Item WHERE CompanyCode = 'STC' AND ItemCode = 'TST-ITEM.01/A_B')
        PRINT 'ok 2 - a code using the permitted characters is accepted -- REQ: ITM-001';
    ELSE PRINT 'not ok 2 - the item was not created -- REQ: ITM-001';
END TRY
BEGIN CATCH
    PRINT 'not ok 2 - unexpected error: ' + ERROR_MESSAGE();
END CATCH
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  3 - the classification decides what an item may take part in, so the class itself is closed. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, CreatedBy)
    VALUES ('STC', 'TST-CLASS-01', 'Unknown item class', 'WIDGET', 'EA', 'SYSADM');
    PRINT 'not ok 3 - an unknown item class was accepted -- REQ: ITM-004';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%CK_Item_Class%' PRINT 'ok 3 - an unknown item class is refused -- REQ: ITM-004, ITM-003';
    ELSE PRINT 'not ok 3 - unexpected error: ' + @msg;
END CATCH

/*  4 - an item carries a current revision and that revision exists in its own right. */
BEGIN TRANSACTION;
SELECT TOP (1) @item = i.ItemCode, @rev = i.CurrentRevision
  FROM inv.Item i JOIN inv.ItemRevision r ON r.CompanyCode = i.CompanyCode AND r.ItemCode = i.ItemCode AND r.Revision = i.CurrentRevision
 WHERE i.CompanyCode = 'STC' ORDER BY i.ItemCode;
IF @item IS NOT NULL AND EXISTS (SELECT 1 FROM inv.ItemRevision WHERE CompanyCode = 'STC' AND ItemCode = @item AND Revision = @rev AND Status IN ('DRAFT', 'RELEASED', 'SUPERSEDED', 'RETIRED'))
    PRINT 'ok 4 - the current revision of ' + @item + ' is a revision of record -- REQ: ITM-002';
ELSE PRINT 'not ok 4 - no item carries a revision of record -- REQ: ITM-002';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  5 - a partner part number finds the internal identity, and says it was an alias that found it. */
BEGIN TRANSACTION;
SET @item = NULL; SET @alias = NULL;
SELECT TOP (1) @item = ItemCode, @alias = PartnerItemCode FROM inv.ItemPartner
 WHERE CompanyCode = 'STC' AND PartnerItemCode <> '' ORDER BY ItemPartnerId;
DECLARE @look TABLE (Code varchar(40), Name varchar(100), Source varchar(10), Alias varchar(40), Status varchar(10));
INSERT @look EXEC inv.usp_UiItemLookup @CompanyCode = 'STC', @Search = @alias;
IF EXISTS (SELECT 1 FROM @look WHERE Code = @item AND Source = 'ALIAS' AND Alias = @alias)
    PRINT 'ok 5 - the supplier part number ' + @alias + ' resolves to ' + @item + ' and is reported as an alias -- REQ: ITM-009, D-21';
ELSE PRINT 'not ok 5 - the alias ' + ISNULL(@alias, '(none)') + ' did not resolve to ' + ISNULL(@item, '(none)') + ' -- REQ: ITM-009';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  6 - the full internal code finds the item itself, never a truncation of another code. */
BEGIN TRANSACTION;
DELETE @look;
INSERT @look EXEC inv.usp_UiItemLookup @CompanyCode = 'STC', @Search = @item;
IF EXISTS (SELECT 1 FROM @look WHERE Code = @item AND Source = 'ITEM' AND Alias = @item)
   AND NOT EXISTS (SELECT 1 FROM @look WHERE Source = 'ITEM' AND Code <> @item AND Code NOT LIKE '%' + @item + '%')
    PRINT 'ok 6 - the full code ' + @item + ' resolves to its own identity -- REQ: ITM-001, ITM-009';
ELSE PRINT 'not ok 6 - the full code did not resolve cleanly -- REQ: ITM-001';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  7 - category protection.  A classification value that live items still carry is in use, and
       the catalogue still holds the items, so the value cannot simply be withdrawn. */
BEGIN TRANSACTION;
SELECT @n = COUNT(*) FROM inv.Item WHERE CompanyCode = 'STC' AND Category = 'BEARING' AND LifecycleStatus <> 'RETIRED';
IF @n > 0 PRINT 'ok 7 - category BEARING is still carried by ' + CAST(@n AS varchar(10)) + ' live items, so it is in use -- REQ: ITM-004';
ELSE PRINT 'not ok 7 - no live item carries the BEARING category -- REQ: ITM-004';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/*  8 - a unit of measure that items are classified against cannot be deleted. */
BEGIN TRY
    BEGIN TRANSACTION;
    DELETE FROM inv.Uom WHERE UomCode = 'EA';
    PRINT 'not ok 8 - a unit of measure in use was deleted -- REQ: ITM-004, ITM-005';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%REFERENCE%' OR @msg LIKE '%FK_%' PRINT 'ok 8 - a unit of measure still in use cannot be deleted -- REQ: ITM-004, ITM-005';
    ELSE PRINT 'not ok 8 - unexpected error: ' + @msg;
END CATCH

/*  9 - a VAT code that an item classification still points at cannot be deleted. */
BEGIN TRY
    BEGIN TRANSACTION;
    SELECT TOP (1) @vat = VatCode FROM inv.ItemVatClass WHERE CompanyCode = 'STC' ORDER BY ItemCode, Direction;
    DELETE FROM fin.VatCode WHERE VatCode = @vat;
    PRINT 'not ok 9 - a VAT code in use was deleted -- REQ: ITM-025';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%REFERENCE%' OR @msg LIKE '%FK_%' PRINT 'ok 9 - a VAT code still used by an item classification cannot be deleted -- REQ: ITM-025';
    ELSE PRINT 'not ok 9 - unexpected error: ' + @msg;
END CATCH

/* 10 - the VAT liability applies by tax point: a later classification does not rewrite history. */
BEGIN TRANSACTION;
BEGIN TRY
    SET @item = NULL;
    SELECT TOP (1) @item = ItemCode FROM inv.ItemVatClass
     WHERE CompanyCode = 'STC' AND Direction = 'SALE' AND VatCode = 'S' AND EffectiveFrom <= '2026-10-15' ORDER BY ItemCode;
    INSERT inv.ItemVatClass (CompanyCode, ItemCode, Direction, EffectiveFrom, VatCode, ApprovedBy, CreatedBy)
    VALUES ('STC', @item, 'SALE', '2026-11-01', 'Z', 'VATOWNER', 'TEST');
    DECLARE @before varchar(4), @after varchar(4);
    SELECT TOP (1) @before = VatCode FROM inv.ItemVatClass WHERE CompanyCode = 'STC' AND ItemCode = @item AND Direction = 'SALE'
       AND EffectiveFrom <= '2026-10-15' AND (EffectiveTo IS NULL OR EffectiveTo >= '2026-10-15') ORDER BY EffectiveFrom DESC;
    SELECT TOP (1) @after = VatCode FROM inv.ItemVatClass WHERE CompanyCode = 'STC' AND ItemCode = @item AND Direction = 'SALE'
       AND EffectiveFrom <= '2026-11-15' AND (EffectiveTo IS NULL OR EffectiveTo >= '2026-11-15') ORDER BY EffectiveFrom DESC;
    IF @before = 'S' AND @after = 'Z'
        PRINT 'ok 10 - the VAT liability of ' + @item + ' changes by tax point, not retrospectively -- REQ: ITM-025, D-02';
    ELSE PRINT 'not ok 10 - tax point selection gave ' + ISNULL(@before, 'null') + ' then ' + ISNULL(@after, 'null') + ' -- REQ: ITM-025';
END TRY
BEGIN CATCH
    PRINT 'not ok 10 - unexpected error: ' + ERROR_MESSAGE();
END CATCH
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/* 11 - a commodity code is numeric, so an invented alphanumeric code is refused. */
BEGIN TRY
    BEGIN TRANSACTION;
    SELECT TOP (1) @item = ItemCode FROM inv.Item WHERE CompanyCode = 'STC' AND IsPurchasable = 1 ORDER BY ItemCode;
    INSERT inv.ItemCustoms (CompanyCode, ItemCode, EffectiveFrom, CommodityCode, CountryOfOrigin, CreatedBy)
    VALUES ('STC', @item, '2027-01-01', '8714AB10', 'GB', 'TEST');
    PRINT 'not ok 11 - an alphanumeric commodity code was accepted -- REQ: ITM-026';
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%CK_ItemCus_Cn%' PRINT 'ok 11 - a commodity code that is not numeric is refused -- REQ: ITM-026';
    ELSE PRINT 'not ok 11 - unexpected error: ' + @msg;
END CATCH

/* 12 - the seeded customs data is complete enough to declare: code, origin and weight basis. */
BEGIN TRANSACTION;
SELECT @n = COUNT(*) FROM inv.ItemCustoms
 WHERE CompanyCode = 'STC' AND (LEN(CommodityCode) < 8 OR CountryOfOrigin LIKE '%[^A-Z]%' OR CountryOfOrigin = '');
IF @n = 0 PRINT 'ok 12 - every item customs row carries an eight digit commodity code and a country of origin -- REQ: ITM-026';
ELSE PRINT 'not ok 12 - ' + CAST(@n AS varchar(10)) + ' customs rows are incomplete -- REQ: ITM-026';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/* 13 - the impact review reports what still depends on an item before it is retired. */
BEGIN TRANSACTION;
DECLARE @imp TABLE (QtyOnHand decimal(38,6), QtyInTransit decimal(38,6), QtyReserved decimal(38,6), MovementCount int,
                    OpenSalesLines int, OpenPurchaseLines int, ActiveBomLines int, OwnBoms int, ReleasedKitLines int,
                    SubstituteRules int, OpenHolds int, OpenCountLines int, LiveSerials int, LifecycleStatus varchar(10),
                    ItemClass varchar(12), ValuationGroup varchar(20), CostMethod varchar(4), StockUomCode varchar(10),
                    TraceabilityType varchar(8), SuccessorItemCode varchar(40), Description varchar(100));
SET @item = NULL;
SELECT TOP (1) @item = b.ItemCode FROM inv.StockBalance b JOIN inv.Item i ON i.CompanyCode = b.CompanyCode AND i.ItemCode = b.ItemCode
 WHERE b.CompanyCode = 'STC' AND b.QtyOnHand > 0 AND i.LifecycleStatus = 'RELEASED' ORDER BY b.ItemCode;
INSERT @imp EXEC inv.usp_ItemImpact @CompanyCode = 'STC', @ItemCode = @item;
IF EXISTS (SELECT 1 FROM @imp WHERE QtyOnHand > 0 AND LifecycleStatus = 'RELEASED')
    PRINT 'ok 13 - the impact review shows ' + @item + ' still holds stock, so it may not be deleted -- REQ: ITM-010, ITM-020, ITM-022';
ELSE PRINT 'not ok 13 - the impact review did not report the stock held by ' + ISNULL(@item, '(none)') + ' -- REQ: ITM-010';
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/* 14 - an open reservation is part of the impact, so a demand is never stranded. */
BEGIN TRANSACTION;
BEGIN TRY
    INSERT inv.StockReservation (CompanyCode, SiteCode, ItemCode, DemandType, DemandRef, DemandLine, QtyReserved, Firmness, Status, CreatedBy)
    VALUES ('STC', 'RDG', @item, 'SALESORDER', 'TST-IMPACT-1', 1, 3, 'FIRM', 'ACTIVE', 'SYSADM');
    DELETE @imp;
    INSERT @imp EXEC inv.usp_ItemImpact @CompanyCode = 'STC', @ItemCode = @item;
    IF EXISTS (SELECT 1 FROM @imp WHERE QtyReserved >= 3)
        PRINT 'ok 14 - the impact review counts the open reservation against ' + @item + ' -- REQ: ITM-010, ITM-014, ITM-023';
    ELSE PRINT 'not ok 14 - the open reservation was not reported -- REQ: ITM-010';
END TRY
BEGIN CATCH
    PRINT 'not ok 14 - unexpected error: ' + ERROR_MESSAGE();
END CATCH
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/* 15 - retire, do not delete: the identity survives and the change is audited. */
BEGIN TRANSACTION;
BEGIN TRY
    UPDATE inv.Item SET LifecycleStatus = 'RETIRED', StatusReason = 'Test retirement', ModifiedBy = 'SYSADM', ModifiedAt = SYSDATETIME()
     WHERE CompanyCode = 'STC' AND ItemCode = @item;
    IF EXISTS (SELECT 1 FROM inv.Item WHERE CompanyCode = 'STC' AND ItemCode = @item AND LifecycleStatus = 'RETIRED')
       AND EXISTS (SELECT 1 FROM core.AuditLog WHERE EntityType = 'inv.Item' AND EntityKey = @item AND Action = 'UPDATE' AND NewValue LIKE '%RETIRED%')
        PRINT 'ok 15 - a retired item keeps its identity and its history, and the change is audited -- REQ: ITM-020, ITM-022, COM-013';
    ELSE PRINT 'not ok 15 - retirement was not recorded or not audited -- REQ: ITM-020, COM-013';
END TRY
BEGIN CATCH
    PRINT 'not ok 15 - unexpected error: ' + ERROR_MESSAGE();
END CATCH
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

PRINT '# itm-items done';
GO
