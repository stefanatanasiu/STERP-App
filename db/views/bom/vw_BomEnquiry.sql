/*  bom.vw_BomComponentUsage  - one readable row per component occurrence.
    Notes, services and cost-only resources are flagged here so that no screen, report or planning
    run can mistake them for physically stocked material (BOM-027); the stock unit quantity is the
    converted quantity used for aggregation and costing (BOM-010).
    bom.vw_BomCurrentRevision - the deterministic selection: the one released revision effective on
    a date for a company/site/purpose/variant, highest revision wins a tie (BOM-005, BOM-003).
    bom.vw_BomExplosionSummary - retained calculation runs with their reproducibility evidence,
    completeness and stop information (BOM-032, BOM-029).

    REQ: BOM-002, BOM-003, BOM-005, BOM-010, BOM-027, BOM-029, BOM-032
*/
SET NOCOUNT ON;
GO
CREATE OR ALTER VIEW bom.vw_BomComponentUsage
AS
SELECT h.CompanyCode, h.SiteCode, h.BomRef, h.BomRevision, h.Status, h.Purpose, h.VariantCode,
       h.ParentItemCode, h.NominalBasis, h.YieldFactor, h.ProductionQuantum, h.BatchSize, h.IsPhantom,
       c.BomComponentId, c.LineNum, c.PathRef, c.ComponentItemCode, c.ComponentRevision,
       c.UsageQty, c.UsageBasis, c.UomCode, c.ConversionFactor, c.ScrapPct, c.SupplyBehaviour,
       c.IsOptional, c.AlternateGroup, c.IsSelected, c.NeedOffsetDays, c.OperationRef, c.Notes,
       QtyPerNominalUnit = CAST(c.UsageQty / NULLIF(h.NominalBasis, 0) AS decimal(19,6)),
       StockUnitQty = CAST(c.UsageQty * c.ConversionFactor AS decimal(19,6)),
       InputWithScrap = CAST(c.UsageQty * (1 + c.ScrapPct) AS decimal(19,6)),
       IsStockMaterial = CASE WHEN c.SupplyBehaviour IN ('NOTE', 'SERVICE', 'COSTONLY') THEN CAST(0 AS bit)
                              ELSE CAST(1 AS bit) END,
       LineKind = CASE WHEN c.SupplyBehaviour = 'NOTE' THEN 'Note (no demand, no value)'
                       WHEN c.SupplyBehaviour = 'SERVICE' THEN 'Service (value only)'
                       WHEN c.SupplyBehaviour = 'COSTONLY' THEN 'Cost-only resource'
                       WHEN c.SupplyBehaviour = 'NONSTOCK' THEN 'Non-stock material'
                       WHEN c.SupplyBehaviour = 'PHANTOM' THEN 'Phantom group'
                       WHEN c.SupplyBehaviour = 'MAKE' THEN 'Manufactured subassembly'
                       WHEN c.SupplyBehaviour = 'USESTOCK' THEN 'Use stock, do not expand'
                       ELSE 'Purchased material' END,
       EffectiveFrom = ISNULL(c.EffectiveFrom, h.EffectiveFrom),
       EffectiveTo = COALESCE(c.EffectiveTo, h.EffectiveTo)
  FROM bom.BomComponent c
  JOIN bom.BomHeader h ON h.BomHeaderId = c.BomHeaderId;
GO
CREATE OR ALTER VIEW bom.vw_BomCurrentRevision
AS
SELECT h.CompanyCode, h.SiteCode, h.Purpose, h.VariantCode, h.ParentItemCode, h.BomHeaderId,
       h.BomRef, h.BomRevision, h.Status, h.NominalBasis, h.YieldFactor, h.ProductionQuantum,
       h.BatchSize, h.IsPhantom, h.EffectiveFrom, h.EffectiveTo, h.ReleasedAt, h.ReleasedBy,
       h.JobRef, h.ValidationId,
       ComponentCount = (SELECT COUNT(*) FROM bom.BomComponent c WHERE c.BomHeaderId = h.BomHeaderId),
       SelectionRank = ROW_NUMBER() OVER (
            PARTITION BY h.CompanyCode, ISNULL(h.SiteCode, '*'), h.ParentItemCode, h.Purpose,
                         ISNULL(h.VariantCode, '*'), ISNULL(h.JobRef, '*')
            ORDER BY h.EffectiveFrom DESC, h.BomRevision DESC, h.BomHeaderId DESC)
  FROM bom.BomHeader h
 WHERE h.Status = 'RELEASED';
GO
CREATE OR ALTER VIEW bom.vw_BomExplosionSummary
AS
SELECT x.ExplosionId, x.CompanyCode, x.SiteCode, x.CalcRef, x.CalcType, x.Purpose, x.ParentItemCode,
       x.ParentRevision, x.BomHeaderId, x.RequiredQty, x.EffectiveDate, x.EvaluatedAt, x.GrossStarts,
       x.RunCount, x.BatchCount, x.MaxDepth, x.OccurrenceCount, x.CostSetCode, x.CurrencyCode,
       x.TotalCost, x.UnitCost, x.MaxBuildableQty, x.Completeness, x.WarningCount, x.ErrorMessage,
       x.CyclePath, x.RequestedBy, x.CorrelationId,
       LeafCount = (SELECT COUNT(*) FROM bom.BomExplosionLine l
                     WHERE l.ExplosionId = x.ExplosionId AND l.NodeType = 'LEAF'),
       ShortageCount = (SELECT COUNT(*) FROM bom.BomExplosionLine l
                         WHERE l.ExplosionId = x.ExplosionId AND ISNULL(l.ShortageQty, 0) > 0),
       StopReasons = (SELECT STRING_AGG(s.StopReason, ', ') FROM
                        (SELECT DISTINCT l.StopReason FROM bom.BomExplosionLine l
                          WHERE l.ExplosionId = x.ExplosionId AND l.StopReason IS NOT NULL) s),
       IsReproducible = CASE WHEN x.BomHeaderId IS NOT NULL AND x.EffectiveDate IS NOT NULL
                             THEN CAST(1 AS bit) ELSE CAST(0 AS bit) END
  FROM bom.BomExplosion x;
GO
