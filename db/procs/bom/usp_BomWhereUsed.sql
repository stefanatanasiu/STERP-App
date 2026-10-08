/*  bom.usp_BomWhereUsed - direct and indirect parents of a component (BOM-023).

    The traversal is upward, which is why it is a set operation here rather than a recursive COBOL
    walk: every bill that quotes the item, then every bill that quotes those parents, with the
    effectivity, revision and variant context carried up each step so the engineer can see which
    revision of the parent is affected on the stated date.  @IncludeDraft = 0 restricts the answer to
    released structures, which is what an engineering change impact assessment needs (BOM-031).

    The second result set lists the open demand and released kits that quote an affected parent, so
    the caller (WHEREUSD, ECOIMPCT, frmBomWhereUsed) does not have to re-query them one by one.

    REQ: BOM-023, BOM-028, BOM-031
*/
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE bom.usp_BomWhereUsed
    @CompanyCode varchar(10), @ComponentItemCode varchar(40), @AsOf date = NULL, @MaxLevels smallint = 12,
    @IncludeDraft bit = 0, @VariantCode varchar(20) = NULL, @IncludeDemand bit = 1
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    SET @AsOf = ISNULL(@AsOf, CAST(SYSDATETIME() AS date));
    IF @MaxLevels IS NULL OR @MaxLevels < 1 OR @MaxLevels > 25 SET @MaxLevels = 12;

    ;WITH Used AS (
        SELECT Level = CAST(1 AS smallint), h.BomHeaderId, h.BomRef, h.BomRevision, h.Status,
               h.ParentItemCode, h.VariantCode, h.Purpose, h.EffectiveFrom, h.EffectiveTo,
               c.ComponentItemCode, c.UsageQty, c.UomCode, c.UsageBasis, c.IsOptional, c.AlternateGroup,
               PathText = CAST(c.ComponentItemCode + ' < ' + h.ParentItemCode AS varchar(400)),
               QtyPerTop = CAST(c.UsageQty / NULLIF(h.NominalBasis, 0) AS decimal(28,12))
          FROM bom.BomComponent c
          JOIN bom.BomHeader h ON h.BomHeaderId = c.BomHeaderId
         WHERE h.CompanyCode = @CompanyCode AND c.ComponentItemCode = @ComponentItemCode
           AND (@IncludeDraft = 1 OR h.Status IN ('RELEASED', 'SUPERSEDED'))
           AND (@VariantCode IS NULL OR h.VariantCode IS NULL OR h.VariantCode = @VariantCode)
           AND h.EffectiveFrom <= @AsOf AND (h.EffectiveTo IS NULL OR h.EffectiveTo > @AsOf)
        UNION ALL
        SELECT Level = CAST(u.Level + 1 AS smallint), h.BomHeaderId, h.BomRef, h.BomRevision, h.Status,
               h.ParentItemCode, h.VariantCode, h.Purpose, h.EffectiveFrom, h.EffectiveTo,
               c.ComponentItemCode, c.UsageQty, c.UomCode, c.UsageBasis, c.IsOptional, c.AlternateGroup,
               PathText = CAST(u.PathText + ' < ' + h.ParentItemCode AS varchar(400)),
               QtyPerTop = CAST(u.QtyPerTop * c.UsageQty / NULLIF(h.NominalBasis, 0) AS decimal(28,12))
          FROM Used u
          JOIN bom.BomComponent c ON c.ComponentItemCode = u.ParentItemCode
          JOIN bom.BomHeader h ON h.BomHeaderId = c.BomHeaderId
         WHERE h.CompanyCode = @CompanyCode AND u.Level < @MaxLevels
           AND (@IncludeDraft = 1 OR h.Status IN ('RELEASED', 'SUPERSEDED'))
           AND h.EffectiveFrom <= @AsOf AND (h.EffectiveTo IS NULL OR h.EffectiveTo > @AsOf)
           AND CHARINDEX(h.ParentItemCode + ' <', u.PathText) = 0   /* a cycle must not loop here (BOM-013) */
    )
    SELECT Level, BomHeaderId, BomRef, BomRevision, Status, ParentItemCode, ComponentItemCode,
           VariantCode, Purpose, EffectiveFrom, EffectiveTo, UsageQty, UomCode, UsageBasis,
           IsOptional, AlternateGroup, PathText,
           QtyPerTopUnit = CAST(QtyPerTop AS decimal(19,6)),
           IsDirect = CASE WHEN Level = 1 THEN CAST(1 AS bit) ELSE CAST(0 AS bit) END
      FROM Used
     ORDER BY Level, ParentItemCode, BomRef, BomRevision
    OPTION (MAXRECURSION 25);

    IF @IncludeDemand = 1
        SELECT ImpactType = x.ImpactType, x.DocumentType, x.DocumentRef, x.DocumentStatus, x.ParentItemCode,
               x.NeedDate, x.AffectedQty, x.Decision
          FROM bom.BomChangeImpact x
         WHERE x.CompanyCode = @CompanyCode AND x.ComponentItemCode = @ComponentItemCode
         ORDER BY x.NeedDate, x.DocumentRef;
END
GO
