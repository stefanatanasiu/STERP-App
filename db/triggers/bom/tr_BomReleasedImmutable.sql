/*  tr_BomReleasedImmutable - a released revision is frozen (BOM-004).

    The rule is expressed as a trigger rather than as procedure code because the bill is edited from
    several places (the editor, the bulk change run, the job bill copy) and engineering asked in 2016
    for one place that can never be bypassed.  Changing a released structure must raise a new
    revision; only the lifecycle columns of the header (status, supersession, retirement, the audit
    stamps) may move on a released row.

    REQ: BOM-004, BOM-028, BOM-029
*/
SET NOCOUNT ON;
GO
CREATE OR ALTER TRIGGER bom.tr_BomReleasedImmutable ON bom.BomComponent
AFTER UPDATE, DELETE
AS
BEGIN
    SET NOCOUNT ON;
    IF NOT EXISTS (SELECT 1 FROM deleted) RETURN;

    /*  a delete from a released structure is always a change of the released content */
    IF NOT EXISTS (SELECT 1 FROM inserted)
       AND EXISTS (SELECT 1 FROM deleted d JOIN bom.BomHeader h ON h.BomHeaderId = d.BomHeaderId
                    WHERE h.Status IN ('RELEASED', 'SUPERSEDED'))
    BEGIN
        ROLLBACK TRANSACTION;
        THROW 51431, 'STERP-E-BOM0413: a released bill revision is immutable - raise a new revision to change it', 1;
    END

    /*  an update that leaves the engineering content identical is not a change: the nightly
        re-load and the seed scripts rewrite the same values and must stay re-runnable */
    IF EXISTS (SELECT 1 FROM inserted i
               JOIN deleted d ON d.BomComponentId = i.BomComponentId
               JOIN bom.BomHeader h ON h.BomHeaderId = i.BomHeaderId
                WHERE h.Status IN ('RELEASED', 'SUPERSEDED')
                  AND (i.BomHeaderId <> d.BomHeaderId
                    OR i.ComponentItemCode <> d.ComponentItemCode
                    OR i.UsageQty <> d.UsageQty
                    OR i.UsageBasis <> d.UsageBasis
                    OR i.UomCode <> d.UomCode
                    OR i.ConversionFactor <> d.ConversionFactor
                    OR i.ScrapPct <> d.ScrapPct
                    OR i.SupplyBehaviour <> d.SupplyBehaviour
                    OR i.IsOptional <> d.IsOptional
                    OR ISNULL(i.ComponentRevision, '') <> ISNULL(d.ComponentRevision, '')
                    OR ISNULL(i.AlternateGroup, '') <> ISNULL(d.AlternateGroup, '')
                    OR ISNULL(i.EffectiveFrom, '19000101') <> ISNULL(d.EffectiveFrom, '19000101')
                    OR ISNULL(i.EffectiveTo, '29991231') <> ISNULL(d.EffectiveTo, '29991231')))
    BEGIN
        ROLLBACK TRANSACTION;
        THROW 51431, 'STERP-E-BOM0413: a released bill revision is immutable - raise a new revision to change it', 1;
    END
END
GO
CREATE OR ALTER TRIGGER bom.tr_BomHeaderRetireCheck ON bom.BomHeader
AFTER UPDATE
AS
BEGIN
    SET NOCOUNT ON;
    IF NOT UPDATE(Status) RETURN;
    /*  BOM-028: a revision that open demand or a job bill still points at cannot be retired */
    IF EXISTS (SELECT 1 FROM inserted i JOIN deleted d ON d.BomHeaderId = i.BomHeaderId
                WHERE i.Status = 'RETIRED' AND d.Status <> 'RETIRED'
                  AND (EXISTS (SELECT 1 FROM bom.BomLineage l WHERE l.SourceBomHeaderId = i.BomHeaderId
                                 AND EXISTS (SELECT 1 FROM bom.BomHeader t WHERE t.BomHeaderId = l.TargetBomHeaderId
                                              AND t.Status IN ('DRAFT', 'REVIEW', 'RELEASED')))
                    OR EXISTS (SELECT 1 FROM bom.BomChangeImpact x WHERE x.BomHeaderId = i.BomHeaderId
                                 AND x.Decision = 'PENDING')))
    BEGIN
        ROLLBACK TRANSACTION;
        THROW 51432, 'STERP-E-BOM0414: the revision is still referenced by a job bill or open demand and cannot be retired', 1;
    END
END
GO
