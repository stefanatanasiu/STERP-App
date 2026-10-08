/*  inv.vw_StockAvailability - one row per physical stock bucket with everything the
    availability rules need: ownership, quality condition, location availability, expiry and the
    item's traceability policy.  The arithmetic that turns these rows into physical / eligible /
    reserved / free quantities lives in the COBOL service AVAILQ, which is the only place the
    definitions in section 6 of the specification are implemented.
    REQ: ITM-012, ITM-013 */
SET NOCOUNT ON;
GO
CREATE OR ALTER VIEW inv.vw_StockAvailability
AS
SELECT b.CompanyCode, b.SiteCode, b.WarehouseCode, b.LocationCode, b.ItemCode, b.Revision,
       b.LotCode, b.SerialNo, b.OwnerType, b.OwnerCode, b.QualityStatus,
       b.QtyOnHand, b.QtyInTransit, b.StockBalanceId,
       l.LocationType, l.IsAvailable, l.IsOwnedStock,
       lt.ExpiryDate, lt.QualityStatus AS LotQualityStatus,
       i.StockUomCode, i.TraceabilityType, i.ShelfLifeDays, i.MinRemainingShelfLifeDays,
       i.LifecycleStatus, i.IsStocked, i.ItemClass
  FROM inv.StockBalance b
  JOIN inv.Location l ON l.CompanyCode = b.CompanyCode AND l.SiteCode = b.SiteCode
                     AND l.WarehouseCode = b.WarehouseCode AND l.LocationCode = b.LocationCode
  JOIN inv.Item i ON i.CompanyCode = b.CompanyCode AND i.ItemCode = b.ItemCode
  LEFT JOIN inv.Lot lt ON lt.CompanyCode = b.CompanyCode AND lt.ItemCode = b.ItemCode
                      AND lt.LotCode = b.LotCode AND b.LotCode <> ''
 WHERE b.QtyOnHand <> 0 OR b.QtyInTransit <> 0;
GO
/*  inv.vw_ItemAlias - the full-code and alias search set.  An internal code, a description and
    every partner part number are searchable, but the internal identity is always reported with
    the alias that found it so nothing is matched by truncation (D-21).
    REQ: ITM-001, ITM-009 */
CREATE OR ALTER VIEW inv.vw_ItemAlias
AS
SELECT i.CompanyCode, i.ItemCode, i.Description, i.LifecycleStatus, 'ITEM' AS MatchSource,
       i.ItemCode AS Alias, '' AS PartnerType, '' AS PartnerCode,
       CAST('2000-01-01' AS date) AS EffectiveFrom, CAST(NULL AS date) AS EffectiveTo
  FROM inv.Item i
UNION ALL
SELECT p.CompanyCode, p.ItemCode, i.Description, i.LifecycleStatus, 'ALIAS',
       p.PartnerItemCode, p.PartnerType, p.PartnerCode, p.EffectiveFrom, p.EffectiveTo
  FROM inv.ItemPartner p
  JOIN inv.Item i ON i.CompanyCode = p.CompanyCode AND i.ItemCode = p.ItemCode;
GO
