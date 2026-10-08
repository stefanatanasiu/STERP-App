/*  inv.usp_ItemImpact - what still depends on an item.  Nothing is blocked, made obsolete,
    superseded or retired, and no protected financial attribute is changed, until the user has
    seen the outstanding stock, reservations, orders, bills, kits and quality obligations.
    The COBOL service ITMIMPCT turns these counts into the decision and the message.
    REQ: ITM-010, ITM-020, ITM-022, ITM-023 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE inv.usp_ItemImpact
    @CompanyCode varchar(10), @ItemCode varchar(40), @Revision varchar(4) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    SELECT
      (SELECT ISNULL(SUM(QtyOnHand), 0) FROM inv.StockBalance WHERE CompanyCode = @CompanyCode AND ItemCode = @ItemCode
          AND (@Revision IS NULL OR @Revision = '' OR Revision = @Revision)) AS QtyOnHand,
      (SELECT ISNULL(SUM(QtyInTransit), 0) FROM inv.StockBalance WHERE CompanyCode = @CompanyCode AND ItemCode = @ItemCode) AS QtyInTransit,
      (SELECT ISNULL(SUM(QtyReserved - QtyConsumed), 0) FROM inv.StockReservation WHERE CompanyCode = @CompanyCode
          AND ItemCode = @ItemCode AND Status = 'ACTIVE') AS QtyReserved,
      (SELECT COUNT(*) FROM inv.StockMovement WHERE CompanyCode = @CompanyCode AND ItemCode = @ItemCode) AS MovementCount,
      (SELECT COUNT(*) FROM so.SalesOrderLine l JOIN so.SalesOrder h ON h.SalesOrderId = l.SalesOrderId
         WHERE l.CompanyCode = @CompanyCode AND l.ItemCode = @ItemCode
           AND h.CommercialStatus NOT IN ('COMPLETED', 'CANCELLED') AND l.AcceptedQty > l.DespatchedQty + l.CancelledQty) AS OpenSalesLines,
      (SELECT COUNT(*) FROM po.PurchaseOrderLine l JOIN po.PurchaseOrder h ON h.PurchaseOrderId = l.PurchaseOrderId
         WHERE l.CompanyCode = @CompanyCode AND l.ItemCode = @ItemCode
           AND h.Status NOT IN ('CLOSED', 'CANCELLED') AND l.OrderedQty > l.ReceivedQty + l.CancelledQty) AS OpenPurchaseLines,
      (SELECT COUNT(*) FROM bom.BomComponent c JOIN bom.BomHeader h ON h.BomHeaderId = c.BomHeaderId
         WHERE h.CompanyCode = @CompanyCode AND c.ComponentItemCode = @ItemCode AND h.Status = 'RELEASED') AS ActiveBomLines,
      (SELECT COUNT(*) FROM bom.BomHeader WHERE CompanyCode = @CompanyCode AND ParentItemCode = @ItemCode
          AND Status IN ('RELEASED', 'DRAFT', 'REVIEW')) AS OwnBoms,
      (SELECT COUNT(*) FROM kit.ExecutionKitLine l JOIN kit.ExecutionKit k ON k.ExecutionKitId = l.ExecutionKitId
         WHERE k.CompanyCode = @CompanyCode AND l.ComponentItemCode = @ItemCode
           AND k.Status NOT IN ('FINCLOSED', 'CANCELLED')) AS ReleasedKitLines,
      (SELECT COUNT(*) FROM inv.ItemSubstitute WHERE CompanyCode = @CompanyCode
          AND (ItemCode = @ItemCode OR SubstituteItemCode = @ItemCode)) AS SubstituteRules,
      (SELECT COUNT(*) FROM inv.QualityHold WHERE CompanyCode = @CompanyCode AND ItemCode = @ItemCode AND Status = 'OPEN') AS OpenHolds,
      (SELECT COUNT(*) FROM inv.StockCountLine cl JOIN inv.StockCount c ON c.CountId = cl.CountId
         WHERE c.CompanyCode = @CompanyCode AND cl.ItemCode = @ItemCode AND c.Status IN ('OPEN', 'COUNTED', 'RECOUNT')) AS OpenCountLines,
      (SELECT COUNT(*) FROM inv.Serial WHERE CompanyCode = @CompanyCode AND ItemCode = @ItemCode
          AND Status IN ('INSTOCK', 'RESERVED')) AS LiveSerials,
      i.LifecycleStatus, i.ItemClass, i.ValuationGroup, i.CostMethod, i.StockUomCode,
      i.TraceabilityType, ISNULL(i.SuccessorItemCode, '') AS SuccessorItemCode, i.Description
      FROM inv.Item i WHERE i.CompanyCode = @CompanyCode AND i.ItemCode = @ItemCode;
END
GO
/*  inv.usp_ItemWhereUsed - where an item is used and how it has moved.  @Mode BOM, KIT, SUBST,
    USAGE or ALL.  Costs are only joined when @ShowCost = 1, which the caller sets from the
    permission check, so a restricted user sees a labelled gap, never an invented zero.
    REQ: ITM-023, ITM-019 */
CREATE OR ALTER PROCEDURE inv.usp_ItemWhereUsed
    @CompanyCode varchar(10), @ItemCode varchar(40), @Mode varchar(8) = 'ALL',
    @FromDate date = NULL, @ToDate date = NULL, @ShowCost bit = 0
AS
BEGIN
    SET NOCOUNT ON;
    SET @FromDate = ISNULL(@FromDate, '2000-01-01'); SET @ToDate = ISNULL(@ToDate, '2099-12-31');
    SELECT Kind, Code, Descr, Ref, Qty, Uom, Val, WhenOn, Status FROM (
        SELECT 'BOM' AS Kind, h.ParentItemCode AS Code, CAST(h.Description AS varchar(60)) AS Descr,
               CAST(h.BomRef + '/' + h.BomRevision AS varchar(40)) AS Ref, c.UsageQty AS Qty,
               CAST(ISNULL(c.UomCode, '') AS varchar(10)) AS Uom, CAST(0 AS decimal(18,2)) AS Val,
               h.EffectiveFrom AS WhenOn, CAST(h.Status AS varchar(12)) AS Status, 1 AS Ord
          FROM bom.BomComponent c JOIN bom.BomHeader h ON h.BomHeaderId = c.BomHeaderId
         WHERE h.CompanyCode = @CompanyCode AND c.ComponentItemCode = @ItemCode AND @Mode IN ('ALL', 'BOM')
        UNION ALL
        SELECT 'KIT', k.KitItemCode, CAST(k.Description AS varchar(60)), CAST(k.KitVersion AS varchar(40)), l.Quantity,
               CAST(ISNULL(l.UomCode, '') AS varchar(10)), CAST(0 AS decimal(18,2)), k.EffectiveFrom, CAST(k.Status AS varchar(12)), 2
          FROM kit.KitDefinitionLine l JOIN kit.KitDefinition k ON k.KitDefId = l.KitDefId
         WHERE k.CompanyCode = @CompanyCode AND l.ComponentItemCode = @ItemCode AND @Mode IN ('ALL', 'KIT')
        UNION ALL
        SELECT 'SUBST', s.SubstituteItemCode, CAST(i.Description AS varchar(60)), CAST('ratio' AS varchar(40)), s.ConversionRatio,
               CAST('' AS varchar(10)), CAST(0 AS decimal(18,2)), s.ValidFrom,
               CAST(CASE WHEN s.ApprovedBy IS NULL THEN 'UNAPPROVED' ELSE 'APPROVED' END AS varchar(12)), 3
          FROM inv.ItemSubstitute s JOIN inv.Item i ON i.CompanyCode = s.CompanyCode AND i.ItemCode = s.SubstituteItemCode
         WHERE s.CompanyCode = @CompanyCode AND s.ItemCode = @ItemCode AND @Mode IN ('ALL', 'SUBST')
        UNION ALL
        SELECT 'USAGE', CAST(m.MovementType AS varchar(40)), CAST(m.SiteCode + ' ' + m.LocationCode AS varchar(60)),
               CAST(ISNULL(m.SourceDocType, '') + ' ' + ISNULL(m.SourceDocRef, '') AS varchar(40)), SUM(m.Quantity),
               CAST('' AS varchar(10)), ISNULL(SUM(m.ValueGBP), 0) * @ShowCost, MIN(m.MovementDate),
               CAST('POSTED' AS varchar(12)), 4
          FROM inv.StockMovement m
         WHERE m.CompanyCode = @CompanyCode AND m.ItemCode = @ItemCode AND @Mode IN ('ALL', 'USAGE')
           AND m.MovementDate BETWEEN @FromDate AND @ToDate
         GROUP BY m.MovementType, m.SiteCode, m.LocationCode, m.SourceDocType, m.SourceDocRef
    ) u ORDER BY u.Ord, u.Code;
END
GO
