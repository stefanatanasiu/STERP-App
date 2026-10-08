/*  bom.usp_BomCompareRevisions - what changed between two revisions of the same bill (BOM-025).

    A full outer join of the two component lists on the comparison identity (component item plus
    alternate group) classifies every line as ADDED, REMOVED, CHANGED or UNCHANGED, and names the
    field that moved.  The quantity difference is reported per nominal basis so that two revisions
    with different basis quantities still compare like with like, and the standard cost of the
    difference is reported when a cost set is supplied - that is the "calculated cost impact" the
    requirement asks for.  BOMCOMP uses this for the online comparison; BOMRP030 prints it.

    REQ: BOM-002, BOM-025, BOM-029
*/
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE bom.usp_BomCompareRevisions
    @CompanyCode varchar(10), @BomRef varchar(30), @FromRevision varchar(4), @ToRevision varchar(4),
    @CostSetCode varchar(20) = NULL, @IncludeUnchanged bit = 0
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;

    DECLARE @FromId int, @ToId int, @FromBasis decimal(19,6), @ToBasis decimal(19,6);
    SELECT @FromId = BomHeaderId, @FromBasis = NominalBasis FROM bom.BomHeader
     WHERE CompanyCode = @CompanyCode AND BomRef = @BomRef AND BomRevision = @FromRevision;
    SELECT @ToId = BomHeaderId, @ToBasis = NominalBasis FROM bom.BomHeader
     WHERE CompanyCode = @CompanyCode AND BomRef = @BomRef AND BomRevision = @ToRevision;
    IF @FromId IS NULL OR @ToId IS NULL
        THROW 51421, 'STERP-E-BOM0901: one of the revisions to compare does not exist', 1;
    IF @FromId = @ToId
        THROW 51422, 'STERP-E-BOM0902: a revision cannot be compared with itself', 1;

    ;WITH L AS (
        SELECT Side = 'F', c.ComponentItemCode, c.ComponentRevision, AltGroup = ISNULL(c.AlternateGroup, ''),
               PerUnit = CAST(c.UsageQty / NULLIF(@FromBasis, 0) AS decimal(28,12)), c.UsageQty, c.UomCode,
               c.UsageBasis, c.ScrapPct, c.SupplyBehaviour, c.IsOptional, c.EffectiveFrom, c.EffectiveTo, c.LineNum
          FROM bom.BomComponent c WHERE c.BomHeaderId = @FromId
        UNION ALL
        SELECT Side = 'T', c.ComponentItemCode, c.ComponentRevision, ISNULL(c.AlternateGroup, ''),
               CAST(c.UsageQty / NULLIF(@ToBasis, 0) AS decimal(28,12)), c.UsageQty, c.UomCode,
               c.UsageBasis, c.ScrapPct, c.SupplyBehaviour, c.IsOptional, c.EffectiveFrom, c.EffectiveTo, c.LineNum
          FROM bom.BomComponent c WHERE c.BomHeaderId = @ToId
    ), P AS (
        SELECT ComponentItemCode = ISNULL(f.ComponentItemCode, t.ComponentItemCode),
               AltGroup = ISNULL(f.AltGroup, t.AltGroup),
               FromQty = f.PerUnit, ToQty = t.PerUnit, FromUom = f.UomCode, ToUom = t.UomCode,
               FromBasisType = f.UsageBasis, ToBasisType = t.UsageBasis, FromScrap = f.ScrapPct, ToScrap = t.ScrapPct,
               FromSupply = f.SupplyBehaviour, ToSupply = t.SupplyBehaviour, FromRev = f.ComponentRevision,
               ToRev = t.ComponentRevision, FromOpt = f.IsOptional, ToOpt = t.IsOptional,
               FromEff = f.EffectiveFrom, ToEff = t.EffectiveFrom
          FROM (SELECT * FROM L WHERE Side = 'F') f
          FULL OUTER JOIN (SELECT * FROM L WHERE Side = 'T') t
            ON t.ComponentItemCode = f.ComponentItemCode AND t.AltGroup = f.AltGroup
    )
    SELECT ChangeType = CASE WHEN FromQty IS NULL THEN 'ADDED' WHEN ToQty IS NULL THEN 'REMOVED'
                             WHEN FromQty <> ToQty OR ISNULL(FromUom,'') <> ISNULL(ToUom,'')
                                  OR ISNULL(FromBasisType,'') <> ISNULL(ToBasisType,'')
                                  OR ISNULL(FromScrap,0) <> ISNULL(ToScrap,0)
                                  OR ISNULL(FromSupply,'') <> ISNULL(ToSupply,'')
                                  OR ISNULL(FromRev,'') <> ISNULL(ToRev,'')
                                  OR ISNULL(FromOpt,0) <> ISNULL(ToOpt,0)
                                  OR ISNULL(FromEff,'19000101') <> ISNULL(ToEff,'19000101') THEN 'CHANGED'
                             ELSE 'UNCHANGED' END,
           p.ComponentItemCode, AlternateGroup = NULLIF(p.AltGroup, ''),
           FromQtyPerUnit = CAST(p.FromQty AS decimal(19,6)), ToQtyPerUnit = CAST(p.ToQty AS decimal(19,6)),
           QtyDifference = CAST(ISNULL(p.ToQty, 0) - ISNULL(p.FromQty, 0) AS decimal(19,6)),
           p.FromUom, p.ToUom, p.FromBasisType, p.ToBasisType, p.FromScrap, p.ToScrap,
           p.FromSupply, p.ToSupply, p.FromRev, p.ToRev, p.FromEff, p.ToEff,
           ChangedFields = STUFF(
                CASE WHEN ISNULL(p.FromQty,-1) <> ISNULL(p.ToQty,-1) THEN ',quantity' ELSE '' END +
                CASE WHEN ISNULL(p.FromUom,'') <> ISNULL(p.ToUom,'') THEN ',unit' ELSE '' END +
                CASE WHEN ISNULL(p.FromBasisType,'') <> ISNULL(p.ToBasisType,'') THEN ',basis' ELSE '' END +
                CASE WHEN ISNULL(p.FromScrap,0) <> ISNULL(p.ToScrap,0) THEN ',scrap' ELSE '' END +
                CASE WHEN ISNULL(p.FromSupply,'') <> ISNULL(p.ToSupply,'') THEN ',supply' ELSE '' END +
                CASE WHEN ISNULL(p.FromRev,'') <> ISNULL(p.ToRev,'') THEN ',revision' ELSE '' END +
                CASE WHEN ISNULL(p.FromEff,'19000101') <> ISNULL(p.ToEff,'19000101') THEN ',effectivity' ELSE '' END,
                1, 1, ''),
           UnitCost = c.UnitCost,
           CostImpact = CAST(ISNULL(c.UnitCost, 0) * (ISNULL(p.ToQty, 0) - ISNULL(p.FromQty, 0)) AS decimal(18,2))
      FROM P p
      OUTER APPLY (SELECT TOP (1) UnitCost = sc.TotalUnitCost FROM cst.StandardCost sc
                    WHERE sc.CompanyCode = @CompanyCode AND sc.ItemCode = p.ComponentItemCode
                      AND (@CostSetCode IS NULL OR sc.CostSetCode = @CostSetCode)
                    ORDER BY sc.CostSetCode, sc.StandardCostId DESC) c
     WHERE @IncludeUnchanged = 1
        OR p.FromQty IS NULL OR p.ToQty IS NULL
        OR p.FromQty <> p.ToQty OR ISNULL(p.FromUom,'') <> ISNULL(p.ToUom,'')
        OR ISNULL(p.FromBasisType,'') <> ISNULL(p.ToBasisType,'') OR ISNULL(p.FromScrap,0) <> ISNULL(p.ToScrap,0)
        OR ISNULL(p.FromSupply,'') <> ISNULL(p.ToSupply,'') OR ISNULL(p.FromRev,'') <> ISNULL(p.ToRev,'')
        OR ISNULL(p.FromOpt,0) <> ISNULL(p.ToOpt,0)
        OR ISNULL(p.FromEff,'19000101') <> ISNULL(p.ToEff,'19000101')
     ORDER BY 1, p.ComponentItemCode;
END
GO
