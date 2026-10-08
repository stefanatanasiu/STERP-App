/*  bom.BomExplosion / bom.BomExplosionLine - reproducible calculation results (gross explosion,
    netted plan, cost roll-up and what-if scenarios) with full path evidence.
    REQ: BOM-012..BOM-016, BOM-020, BOM-021, BOM-029, BOM-032, CST-023, RPT-012 */
SET NOCOUNT ON;
IF OBJECT_ID(N'bom.BomExplosion') IS NULL CREATE TABLE bom.BomExplosion (
  ExplosionId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_BomExplosion PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NULL, CalcRef varchar(30) NOT NULL,
  CalcType varchar(10) NOT NULL CONSTRAINT CK_BomX_Type CHECK (CalcType IN ('GROSS','NET','COST','SCENARIO','WHEREUSED')), Purpose varchar(60) NULL, ParentItemCode varchar(40) NOT NULL, ParentRevision varchar(4) NULL, BomHeaderId int NULL,
  RequiredQty decimal(19,6) NOT NULL, EffectiveDate date NOT NULL, EvaluatedAt datetime2(3) NOT NULL CONSTRAINT DF_BomX_EA DEFAULT SYSDATETIME(), GrossStarts decimal(19,6) NULL, RunCount int NOT NULL CONSTRAINT DF_BomX_Rc DEFAULT 1,
  BatchCount int NULL, MaxDepth smallint NULL, OccurrenceCount int NOT NULL CONSTRAINT DF_BomX_Oc DEFAULT 0, CostSetCode varchar(20) NULL, CurrencyCode char(3) NULL, TotalCost decimal(18,2) NULL, UnitCost decimal(19,6) NULL,
  MaxBuildableQty decimal(19,6) NULL, Completeness varchar(12) NOT NULL CONSTRAINT DF_BomX_Comp DEFAULT 'COMPLETE' CONSTRAINT CK_BomX_Comp CHECK (Completeness IN ('COMPLETE','INCOMPLETE','ERROR')),
  WarningCount int NOT NULL CONSTRAINT DF_BomX_Wc DEFAULT 0, ErrorMessage varchar(400) NULL, CyclePath varchar(400) NULL, RequestedBy varchar(20) NULL, CorrelationId varchar(64) NULL, CONSTRAINT UQ_BomExplosion UNIQUE (CompanyCode, CalcRef));
GO
IF OBJECT_ID(N'bom.BomExplosionLine') IS NULL CREATE TABLE bom.BomExplosionLine (
  ExplosionLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_BomExplosionLine PRIMARY KEY, ExplosionId bigint NOT NULL REFERENCES bom.BomExplosion(ExplosionId), LineNum int NOT NULL, Depth smallint NOT NULL, PathText varchar(400) NOT NULL,
  ParentItemCode varchar(40) NOT NULL, ParentLineNum int NULL, ComponentItemCode varchar(40) NOT NULL, ComponentRevision varchar(4) NULL, BomHeaderId int NULL, UsageBasis varchar(12) NOT NULL, UomCode varchar(10) NOT NULL,
  ConversionFactor decimal(19,9) NOT NULL CONSTRAINT DF_BomXL_F DEFAULT 1, UnroundedQty decimal(28,12) NOT NULL, YieldContribution decimal(28,12) NULL, ScrapContribution decimal(28,12) NULL,
  RoundingIncrement decimal(19,6) NOT NULL CONSTRAINT DF_BomXL_Ri DEFAULT 0, RequiredQty decimal(19,6) NOT NULL, NodeType varchar(10) NOT NULL CONSTRAINT CK_BomXL_Nt CHECK (NodeType IN ('LEAF','INTERMED','PHANTOM','BYPRODUCT','NOTE')),
  StopReason varchar(30) NULL, StockUsedQty decimal(19,6) NULL, ReservedQty decimal(19,6) NULL, InboundQty decimal(19,6) NULL, ShortageQty decimal(19,6) NULL, ShortageDate date NULL, UnitCost decimal(19,6) NULL, ExtendedCost decimal(18,2) NULL,
  CostElementCode varchar(10) NULL, IsFixedCost bit NOT NULL CONSTRAINT DF_BomXL_Fx DEFAULT 0, WarningText varchar(300) NULL, CONSTRAINT UQ_BomExplosionLine UNIQUE (ExplosionId, LineNum));
GO
