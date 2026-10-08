/*  inv.usp_StockEnquiry - the enquiry behind the stock enquiry screen and the stock status
    report.  It distinguishes item/revision, company, owner, site, location, lot/serial and
    quality condition, and it labels each bucket as eligible or not so that third-party-owned,
    in-transit and quarantined quantities can never be read as ordinary available stock.
    REQ: ITM-012, ITM-013, ITM-023 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE inv.usp_StockEnquiry
    @CompanyCode varchar(10), @ItemCode varchar(40) = NULL, @SiteCode varchar(10) = NULL,
    @WarehouseCode varchar(20) = NULL, @LocationCode varchar(20) = NULL, @Revision varchar(4) = NULL,
    @LotCode varchar(30) = NULL, @SerialNo varchar(40) = NULL, @OwnerType varchar(10) = NULL,
    @Condition varchar(12) = NULL, @AsOf date = NULL, @IncludeZero bit = 0
AS
BEGIN
    SET NOCOUNT ON;
    SET @AsOf = ISNULL(@AsOf, CAST(SYSDATETIME() AS date));
    SELECT v.CompanyCode, v.SiteCode, v.WarehouseCode, v.LocationCode, v.LocationType, v.ItemCode,
           v.Revision, v.LotCode, v.SerialNo, v.OwnerType, v.OwnerCode, v.QualityStatus,
           v.QtyOnHand, v.QtyInTransit, v.StockUomCode, v.ExpiryDate, v.TraceabilityType, v.LifecycleStatus,
           ISNULL(r.QtyReserved, 0) AS QtyReserved,
           CASE WHEN v.OwnerType = 'OWNED' AND v.QualityStatus = 'ACCEPTED' AND v.IsAvailable = 1
                     AND v.IsOwnedStock = 1 AND v.LocationType <> 'TRANSIT'
                     AND (v.ExpiryDate IS NULL OR v.ExpiryDate >= DATEADD(day, ISNULL(v.MinRemainingShelfLifeDays, 0), @AsOf))
                     AND v.LifecycleStatus NOT IN ('BLOCKED', 'RETIRED')
                THEN CAST(1 AS bit) ELSE CAST(0 AS bit) END AS IsEligible,
           CASE WHEN v.OwnerType <> 'OWNED' THEN 'Third party owned - in our custody'
                WHEN v.LocationType = 'TRANSIT' THEN 'In transit between locations'
                WHEN v.ExpiryDate IS NOT NULL AND v.ExpiryDate < @AsOf THEN 'Expired'
                WHEN v.ExpiryDate IS NOT NULL AND v.ExpiryDate < DATEADD(day, ISNULL(v.MinRemainingShelfLifeDays, 0), @AsOf)
                     THEN 'Inside minimum remaining shelf life'
                WHEN v.QualityStatus <> 'ACCEPTED' THEN 'Quality condition ' + LOWER(v.QualityStatus)
                WHEN v.IsAvailable = 0 THEN 'Location is not an available stock location'
                WHEN v.LifecycleStatus IN ('BLOCKED', 'RETIRED') THEN 'Item is ' + LOWER(v.LifecycleStatus)
                ELSE '' END AS NotEligibleReason
      FROM inv.vw_StockAvailability v
      OUTER APPLY (SELECT SUM(s.QtyReserved - s.QtyConsumed) AS QtyReserved FROM inv.StockReservation s
                    WHERE s.CompanyCode = v.CompanyCode AND s.ItemCode = v.ItemCode AND s.Status = 'ACTIVE'
                      AND s.SiteCode = v.SiteCode AND ISNULL(s.LocationCode, v.LocationCode) = v.LocationCode
                      AND ISNULL(NULLIF(s.LotCode, ''), v.LotCode) = v.LotCode) r
     WHERE v.CompanyCode = @CompanyCode
       AND (@ItemCode IS NULL OR @ItemCode = '' OR v.ItemCode = @ItemCode)
       AND (@SiteCode IS NULL OR @SiteCode = '' OR v.SiteCode = @SiteCode)
       AND (@WarehouseCode IS NULL OR @WarehouseCode = '' OR v.WarehouseCode = @WarehouseCode)
       AND (@LocationCode IS NULL OR @LocationCode = '' OR v.LocationCode = @LocationCode)
       AND (@Revision IS NULL OR @Revision = '' OR v.Revision = @Revision)
       AND (@LotCode IS NULL OR @LotCode = '' OR v.LotCode = @LotCode)
       AND (@SerialNo IS NULL OR @SerialNo = '' OR v.SerialNo = @SerialNo)
       AND (@OwnerType IS NULL OR @OwnerType = '' OR v.OwnerType = @OwnerType)
       AND (@Condition IS NULL OR @Condition = '' OR v.QualityStatus = @Condition)
       AND (@IncludeZero = 1 OR v.QtyOnHand <> 0 OR v.QtyInTransit <> 0)
     ORDER BY v.ItemCode, v.SiteCode, v.WarehouseCode, v.LocationCode, v.LotCode, v.SerialNo;
END
GO
/*  inv.usp_UiItemLookup - F2 lookup for item codes and partner aliases.  REQ: ITM-001, ITM-009 */
CREATE OR ALTER PROCEDURE inv.usp_UiItemLookup @CompanyCode varchar(10) = NULL, @Search varchar(60) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    SELECT TOP 200 a.ItemCode AS Code, a.Description AS Name, a.MatchSource AS Source,
           a.Alias AS Alias, a.LifecycleStatus AS Status
      FROM inv.vw_ItemAlias a
     WHERE (@CompanyCode IS NULL OR a.CompanyCode = @CompanyCode)
       AND (@Search IS NULL OR @Search = '' OR a.ItemCode LIKE '%' + @Search + '%'
            OR a.Description LIKE '%' + @Search + '%' OR a.Alias LIKE '%' + @Search + '%')
     ORDER BY a.ItemCode, a.MatchSource;
END
GO
