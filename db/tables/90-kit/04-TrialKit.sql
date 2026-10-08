/*  kit.TrialKit / kit.TrialKitLine - non-committing material feasibility.  A trial changes no
    stock, reservation, supply order or ledger balance.  REQ: KIT-006, KIT-007, SO-010 */
SET NOCOUNT ON;
IF OBJECT_ID(N'kit.TrialKit') IS NULL CREATE TABLE kit.TrialKit (
  TrialId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_TrialKit PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, TrialRef varchar(30) NOT NULL, ParentItemCode varchar(40) NOT NULL, Revision varchar(4) NULL,
  BomHeaderId int NULL, RequestedQty decimal(19,6) NOT NULL, RequiredDate date NULL, SelectionDate date NOT NULL, StockSnapshotAt datetime2(3) NOT NULL CONSTRAINT DF_Trl_Sa DEFAULT SYSDATETIME(),
  IncludeExpectedSupply bit NOT NULL CONSTRAINT DF_Trl_Es DEFAULT 0, StockEligibilityPolicy varchar(40) NULL, PrioritySequence varchar(60) NULL, ScenarioNo smallint NOT NULL CONSTRAINT DF_Trl_Sc DEFAULT 1, MaxBuildableQty decimal(19,6) NULL,
  LimitingItemCode varchar(40) NULL, ShortageCount int NOT NULL CONSTRAINT DF_Trl_Sh DEFAULT 0, Completeness varchar(12) NOT NULL CONSTRAINT DF_Trl_Comp DEFAULT 'COMPLETE', Warnings varchar(400) NULL, RequestedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Trl_CA DEFAULT SYSDATETIME(), CONSTRAINT UQ_TrialKit UNIQUE (CompanyCode, TrialRef, ScenarioNo));
GO
IF OBJECT_ID(N'kit.TrialKitLine') IS NULL CREATE TABLE kit.TrialKitLine (
  TrialLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_TrialKitLine PRIMARY KEY, TrialId bigint NOT NULL REFERENCES kit.TrialKit(TrialId), LineNum int NOT NULL, PathRef varchar(20) NULL, Depth smallint NOT NULL CONSTRAINT DF_TrlL_Dp DEFAULT 1,
  ComponentItemCode varchar(40) NOT NULL, Revision varchar(4) NULL, UomCode varchar(10) NULL, GrossRequirementQty decimal(19,6) NOT NULL, NetRequirementQty decimal(19,6) NOT NULL, FreeStockQty decimal(19,6) NOT NULL CONSTRAINT DF_TrlL_Fq DEFAULT 0,
  OwnReservedQty decimal(19,6) NOT NULL CONSTRAINT DF_TrlL_Oq DEFAULT 0, OtherReservedQty decimal(19,6) NOT NULL CONSTRAINT DF_TrlL_Rq DEFAULT 0, ExpectedInboundQty decimal(19,6) NOT NULL CONSTRAINT DF_TrlL_Eq DEFAULT 0,
  ExpectedInboundDate date NULL, AllocatedQty decimal(19,6) NOT NULL CONSTRAINT DF_TrlL_Aq DEFAULT 0, ShortageQty decimal(19,6) NOT NULL CONSTRAINT DF_TrlL_Sq DEFAULT 0, TimePhasedShortageDate date NULL,
  IsLimiting bit NOT NULL CONSTRAINT DF_TrlL_Lim DEFAULT 0, SuggestedAction varchar(40) NULL, WarningText varchar(300) NULL, CONSTRAINT UQ_TrialKitLine UNIQUE (TrialId, LineNum));
GO
