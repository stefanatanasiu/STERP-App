/*  bom.usp_BomStructure - the read side of the bill editor.

    One procedure, four shapes, selected by @Mode, because the green screen that frmBomEditor
    replaced had one transaction with a one character "page" field.  Everything here is a pure
    set operation over the structures; nothing calculates - explosion, netting and costing are
    the COBOL engine's job (BOMEXPL / BOMNET / BOMCOST).

      REVISIONS  every revision of a bill (or of a parent item), newest first, with the row count
                 and the rank the deterministic selection would give it (BOM-002, BOM-005)
      HEADER     one revision with its declared boundary and release evidence (BOM-001, BOM-004)
      LINES      the component occurrences of one revision (BOM-006..BOM-011, BOM-018, BOM-027)
      BYPRODUCT  the by-product recoveries of one revision (BOM-026)

    @BomRevision NULL with @Mode HEADER/LINES/BYPRODUCT means "the revision effective on @AsOf",
    chosen by bom.vw_BomCurrentRevision so the screen and the engine agree on what is current.

    REQ: BOM-001, BOM-002, BOM-004, BOM-005, BOM-010, BOM-018, BOM-026, BOM-027
*/
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE bom.usp_BomStructure
    @CompanyCode    varchar(10) = NULL,
    @BomRef         varchar(30) = NULL,
    @BomRevision    varchar(4)  = NULL,
    @ParentItemCode varchar(40) = NULL,
    @Mode           varchar(12) = 'LINES',
    @AsOf           date        = NULL
AS
BEGIN
    SET NOCOUNT ON;

    SELECT @CompanyCode = NULLIF(LTRIM(RTRIM(ISNULL(@CompanyCode, ''))), ''),
           @BomRef      = NULLIF(LTRIM(RTRIM(ISNULL(@BomRef, ''))), ''),
           @BomRevision = NULLIF(LTRIM(RTRIM(ISNULL(@BomRevision, ''))), ''),
           @ParentItemCode = NULLIF(LTRIM(RTRIM(ISNULL(@ParentItemCode, ''))), ''),
           @Mode        = UPPER(ISNULL(NULLIF(LTRIM(RTRIM(@Mode)), ''), 'LINES')),
           @AsOf        = ISNULL(@AsOf, CAST(SYSDATETIME() AS date));

    IF @Mode = 'REVISIONS'
    BEGIN
        SELECT h.BomHeaderId, h.CompanyCode, h.SiteCode, h.BomRef, h.BomRevision, h.ParentItemCode,
               h.Purpose, h.VariantCode, h.JobRef, h.Description, h.Status,
               h.NominalBasis, h.YieldFactor, h.ProductionQuantum, h.BatchSize, h.IsPhantom,
               h.EffectiveFrom, h.EffectiveTo, h.AuthorUser, h.ReviewerUser, h.ReleasedBy, h.ReleasedAt,
               h.ChangeRef,
               ComponentCount = (SELECT COUNT(*) FROM bom.BomComponent c WHERE c.BomHeaderId = h.BomHeaderId),
               IsEffectiveNow = CASE WHEN h.Status = 'RELEASED' AND h.EffectiveFrom <= @AsOf
                                      AND (h.EffectiveTo IS NULL OR h.EffectiveTo > @AsOf)
                                     THEN CAST(1 AS bit) ELSE CAST(0 AS bit) END,
               SelectionRank = cr.SelectionRank
          FROM bom.BomHeader h
          LEFT JOIN bom.vw_BomCurrentRevision cr ON cr.BomHeaderId = h.BomHeaderId
         WHERE (@CompanyCode IS NULL OR h.CompanyCode = @CompanyCode)
           AND (@BomRef IS NULL OR h.BomRef = @BomRef)
           AND (@ParentItemCode IS NULL OR h.ParentItemCode = @ParentItemCode)
           AND (@BomRef IS NOT NULL OR @ParentItemCode IS NOT NULL)
         ORDER BY h.BomRef, h.EffectiveFrom DESC, h.BomRevision DESC;
        RETURN 0;
    END;

    /* Resolve the one revision the other three modes report on. */
    DECLARE @HeaderId int;

    SELECT TOP (1) @HeaderId = h.BomHeaderId
      FROM bom.BomHeader h
     WHERE (@CompanyCode IS NULL OR h.CompanyCode = @CompanyCode)
       AND (@BomRef IS NULL OR h.BomRef = @BomRef)
       AND (@ParentItemCode IS NULL OR h.ParentItemCode = @ParentItemCode)
       AND (@BomRevision IS NULL OR h.BomRevision = @BomRevision)
       AND (@BomRevision IS NOT NULL
            OR (h.EffectiveFrom <= @AsOf AND (h.EffectiveTo IS NULL OR h.EffectiveTo > @AsOf)))
     ORDER BY CASE WHEN h.Status = 'RELEASED' THEN 0 ELSE 1 END,
              h.EffectiveFrom DESC, h.BomRevision DESC, h.BomHeaderId DESC;

    IF @Mode = 'HEADER'
    BEGIN
        SELECT h.BomHeaderId, h.CompanyCode, h.SiteCode, h.BomRef, h.BomRevision, h.ParentItemCode,
               h.ParentRevision, h.Purpose, h.VariantCode, h.JobRef, h.Description,
               h.NominalBasis, h.YieldFactor, h.ProductionQuantum, h.BatchSize, h.CostingLotSize,
               h.IsPhantom, h.EffectiveFrom, h.EffectiveTo, h.Status, h.AuthorUser, h.ReviewerUser,
               h.ReleasedBy, h.ReleasedAt, h.ChangeRef, h.ChangeReason, h.EngineeringNotes,
               ParentDescription = i.Description, StockUom = i.StockUomCode,
               ComponentCount = (SELECT COUNT(*) FROM bom.BomComponent c WHERE c.BomHeaderId = h.BomHeaderId),
               ByProductCount = (SELECT COUNT(*) FROM bom.BomByProduct b WHERE b.BomHeaderId = h.BomHeaderId),
               IsFrozen = CASE WHEN h.Status IN ('RELEASED', 'SUPERSEDED', 'RETIRED')
                               THEN CAST(1 AS bit) ELSE CAST(0 AS bit) END
          FROM bom.BomHeader h
          LEFT JOIN inv.Item i ON i.CompanyCode = h.CompanyCode AND i.ItemCode = h.ParentItemCode
         WHERE h.BomHeaderId = @HeaderId;
        RETURN 0;
    END;

    IF @Mode = 'BYPRODUCT'
    BEGIN
        SELECT b.LineNum, b.ByProductItemCode, Description = i.Description, b.RecoveryQty, b.UomCode,
               b.ConversionFactor, b.ValuationBasis, b.RecoveryValue, b.DispositionRule, b.ApprovedBy
          FROM bom.BomByProduct b
          JOIN bom.BomHeader h ON h.BomHeaderId = b.BomHeaderId
          LEFT JOIN inv.Item i ON i.CompanyCode = h.CompanyCode AND i.ItemCode = b.ByProductItemCode
         WHERE b.BomHeaderId = @HeaderId
         ORDER BY b.LineNum;
        RETURN 0;
    END;

    SELECT u.LineNum, u.PathRef, u.ComponentItemCode, Description = i.Description, u.ComponentRevision,
           u.UsageQty, u.UsageBasis, u.UomCode, u.ConversionFactor, u.ScrapPct, u.SupplyBehaviour,
           u.LineKind, u.IsStockMaterial, u.IsOptional, u.AlternateGroup, u.IsSelected,
           u.QtyPerNominalUnit, u.StockUnitQty, u.InputWithScrap, u.NeedOffsetDays, u.OperationRef,
           u.EffectiveFrom, u.EffectiveTo, u.Notes
      FROM bom.vw_BomComponentUsage u
      LEFT JOIN inv.Item i ON i.CompanyCode = u.CompanyCode AND i.ItemCode = u.ComponentItemCode
     WHERE EXISTS (SELECT 1 FROM bom.BomComponent c
                    WHERE c.BomComponentId = u.BomComponentId AND c.BomHeaderId = @HeaderId)
     ORDER BY u.LineNum;
    RETURN 0;
END;
GO
PRINT 'Created bom.usp_BomStructure';
GO
