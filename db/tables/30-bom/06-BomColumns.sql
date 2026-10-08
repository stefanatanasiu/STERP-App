/*  Columns added to the bom tables after the first cut, once the calculation output contract of
    specification section 12 was implemented in full.  Every complete output has to carry the
    basis, yield, quantum, run/batch counts, the unrounded requirement, the rounding increment,
    the pack/excess evidence and the selected revision at every level - so the stored explosion
    has to carry them too.  All additions are idempotent.
    REQ: BOM-006, BOM-007, BOM-008, BOM-011, BOM-019, BOM-021, BOM-026, BOM-027, BOM-032 */
SET NOCOUNT ON;
GO
/*  header: approved run consolidation (D-10), the scrap-versus-yield intent the bill must state,
    and the conversion economics of the parent process used by the cost roll-up. */
IF COL_LENGTH(N'bom.BomHeader', 'ConsolidateRuns')       IS NULL ALTER TABLE bom.BomHeader ADD ConsolidateRuns bit NOT NULL CONSTRAINT DF_Bom_Cons DEFAULT 0;
IF COL_LENGTH(N'bom.BomHeader', 'LossIntentNote')        IS NULL ALTER TABLE bom.BomHeader ADD LossIntentNote varchar(200) NULL;
IF COL_LENGTH(N'bom.BomHeader', 'SetupCostPerRun')       IS NULL ALTER TABLE bom.BomHeader ADD SetupCostPerRun decimal(19,6) NULL;
IF COL_LENGTH(N'bom.BomHeader', 'ConversionCostPerStart') IS NULL ALTER TABLE bom.BomHeader ADD ConversionCostPerStart decimal(19,6) NULL;
IF COL_LENGTH(N'bom.BomHeader', 'SourceBomHeaderId')     IS NULL ALTER TABLE bom.BomHeader ADD SourceBomHeaderId int NULL;
GO
/*  component: cost-only resources carry their own rate and element; a missing rate must stop a
    falsely complete cost (BOM-027). */
IF COL_LENGTH(N'bom.BomComponent', 'CostOnlyRate')   IS NULL ALTER TABLE bom.BomComponent ADD CostOnlyRate decimal(19,6) NULL;
IF COL_LENGTH(N'bom.BomComponent', 'CostElementCode') IS NULL ALTER TABLE bom.BomComponent ADD CostElementCode varchar(10) NULL;
IF COL_LENGTH(N'bom.BomComponent', 'OptionChoiceRef') IS NULL ALTER TABLE bom.BomComponent ADD OptionChoiceRef varchar(20) NULL;
GO
/*  explosion run: the reproducibility fields of specification section 12. */
IF COL_LENGTH(N'bom.BomExplosion', 'NominalBasis')     IS NULL ALTER TABLE bom.BomExplosion ADD NominalBasis decimal(19,6) NULL;
IF COL_LENGTH(N'bom.BomExplosion', 'YieldFactor')      IS NULL ALTER TABLE bom.BomExplosion ADD YieldFactor decimal(9,6) NULL;
IF COL_LENGTH(N'bom.BomExplosion', 'ProductionQuantum') IS NULL ALTER TABLE bom.BomExplosion ADD ProductionQuantum decimal(19,6) NULL;
IF COL_LENGTH(N'bom.BomExplosion', 'BatchSize')        IS NULL ALTER TABLE bom.BomExplosion ADD BatchSize decimal(19,6) NULL;
IF COL_LENGTH(N'bom.BomExplosion', 'ExcessQty')        IS NULL ALTER TABLE bom.BomExplosion ADD ExcessQty decimal(19,6) NULL;
IF COL_LENGTH(N'bom.BomExplosion', 'ConsolidateRuns')  IS NULL ALTER TABLE bom.BomExplosion ADD ConsolidateRuns bit NOT NULL CONSTRAINT DF_BomX_Cons DEFAULT 0;
IF COL_LENGTH(N'bom.BomExplosion', 'ReasonCode')       IS NULL ALTER TABLE bom.BomExplosion ADD ReasonCode varchar(8) NULL;
IF COL_LENGTH(N'bom.BomExplosion', 'MaterialCost')     IS NULL ALTER TABLE bom.BomExplosion ADD MaterialCost decimal(18,2) NULL;
IF COL_LENGTH(N'bom.BomExplosion', 'ConversionCost')   IS NULL ALTER TABLE bom.BomExplosion ADD ConversionCost decimal(18,2) NULL;
IF COL_LENGTH(N'bom.BomExplosion', 'RecoveryCredit')   IS NULL ALTER TABLE bom.BomExplosion ADD RecoveryCredit decimal(18,2) NULL;
IF COL_LENGTH(N'bom.BomExplosion', 'UnitDenominator')  IS NULL ALTER TABLE bom.BomExplosion ADD UnitDenominator decimal(19,6) NULL;
IF COL_LENGTH(N'bom.BomExplosion', 'ShortageCount')    IS NULL ALTER TABLE bom.BomExplosion ADD ShortageCount int NOT NULL CONSTRAINT DF_BomX_Sc DEFAULT 0;
GO
/*  explosion line: per-occurrence evidence - the good output of the node, its gross starts,
    its own run/batch counts, the selected revision, the pack proposal and the excess. */
IF COL_LENGTH(N'bom.BomExplosionLine', 'GoodQty')       IS NULL ALTER TABLE bom.BomExplosionLine ADD GoodQty decimal(19,6) NULL;
IF COL_LENGTH(N'bom.BomExplosionLine', 'GrossStarts')   IS NULL ALTER TABLE bom.BomExplosionLine ADD GrossStarts decimal(19,6) NULL;
IF COL_LENGTH(N'bom.BomExplosionLine', 'RunCount')      IS NULL ALTER TABLE bom.BomExplosionLine ADD RunCount int NULL;
IF COL_LENGTH(N'bom.BomExplosionLine', 'BatchCount')    IS NULL ALTER TABLE bom.BomExplosionLine ADD BatchCount int NULL;
IF COL_LENGTH(N'bom.BomExplosionLine', 'BomRevision')   IS NULL ALTER TABLE bom.BomExplosionLine ADD BomRevision varchar(4) NULL;
IF COL_LENGTH(N'bom.BomExplosionLine', 'NominalBasis')  IS NULL ALTER TABLE bom.BomExplosionLine ADD NominalBasis decimal(19,6) NULL;
IF COL_LENGTH(N'bom.BomExplosionLine', 'YieldFactor')   IS NULL ALTER TABLE bom.BomExplosionLine ADD YieldFactor decimal(9,6) NULL;
IF COL_LENGTH(N'bom.BomExplosionLine', 'UsageQty')      IS NULL ALTER TABLE bom.BomExplosionLine ADD UsageQty decimal(19,6) NULL;
IF COL_LENGTH(N'bom.BomExplosionLine', 'ScrapPct')      IS NULL ALTER TABLE bom.BomExplosionLine ADD ScrapPct decimal(9,6) NULL;
IF COL_LENGTH(N'bom.BomExplosionLine', 'StockUomCode')  IS NULL ALTER TABLE bom.BomExplosionLine ADD StockUomCode varchar(10) NULL;
IF COL_LENGTH(N'bom.BomExplosionLine', 'PackQty')       IS NULL ALTER TABLE bom.BomExplosionLine ADD PackQty decimal(19,6) NULL;
IF COL_LENGTH(N'bom.BomExplosionLine', 'ExcessQty')     IS NULL ALTER TABLE bom.BomExplosionLine ADD ExcessQty decimal(19,6) NULL;
IF COL_LENGTH(N'bom.BomExplosionLine', 'OccurrenceRef') IS NULL ALTER TABLE bom.BomExplosionLine ADD OccurrenceRef varchar(40) NULL;
IF COL_LENGTH(N'bom.BomExplosionLine', 'AlternateGroup') IS NULL ALTER TABLE bom.BomExplosionLine ADD AlternateGroup varchar(10) NULL;
IF COL_LENGTH(N'bom.BomExplosionLine', 'SupplyBehaviour') IS NULL ALTER TABLE bom.BomExplosionLine ADD SupplyBehaviour varchar(10) NULL;
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_BomComponent_Item')
    CREATE INDEX IX_BomComponent_Item ON bom.BomComponent (ComponentItemCode) INCLUDE (BomHeaderId, LineNum, UsageQty, UsageBasis, SupplyBehaviour);
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_BomHeader_Parent')
    CREATE INDEX IX_BomHeader_Parent ON bom.BomHeader (CompanyCode, ParentItemCode, Status) INCLUDE (BomRef, BomRevision, EffectiveFrom, EffectiveTo, Purpose, SiteCode, VariantCode, JobRef);
GO
