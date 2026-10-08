/*  kit.ExecutionKit / kit.ExecutionKitLine - the material requirements and actual activity for a
    particular assembly, job or order.  Release fixes the selected BOM and item revisions.
    REQ: KIT-002..KIT-005, KIT-011, KIT-016, KIT-024 */
SET NOCOUNT ON;
IF OBJECT_ID(N'kit.ExecutionKit') IS NULL CREATE TABLE kit.ExecutionKit (
  ExecutionKitId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_ExecutionKit PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, KitRef varchar(30) NOT NULL, ParentItemCode varchar(40) NOT NULL, ParentRevision varchar(4) NULL,
  BomHeaderId int NULL, BomRevision varchar(4) NULL, SourceDemandType varchar(12) NULL CONSTRAINT CK_ExKit_Sd CHECK (SourceDemandType IN ('SALESORDER','JOB','STOCK','REWORK','PARENTKIT')), SourceDemandRef varchar(40) NULL, JobRef varchar(20) NULL,
  ParentKitRef varchar(30) NULL, PlannedGoodQty decimal(19,6) NOT NULL, CompletedGoodQty decimal(19,6) NOT NULL CONSTRAINT DF_ExKit_Cq DEFAULT 0, RejectedQty decimal(19,6) NOT NULL CONSTRAINT DF_ExKit_Rq DEFAULT 0, GrossStarts decimal(19,6) NULL,
  YieldFactor decimal(9,6) NULL, ProductionQuantum decimal(19,6) NULL, BatchSize decimal(19,6) NULL, RunCount int NOT NULL CONSTRAINT DF_ExKit_Rc DEFAULT 1, BatchCount int NULL, PlannedStartDate date NULL, NeedDate date NULL,
  PlannedCompletionDate date NULL, ActualCompletionDate date NULL, SelectionDate date NULL, SupplyChoiceNote varchar(200) NULL, OwnerUser varchar(20) NULL,
  Status varchar(14) NOT NULL CONSTRAINT DF_ExKit_St DEFAULT 'DRAFT' CONSTRAINT CK_ExKit_St CHECK (Status IN ('DRAFT','REVIEWED','RELEASED','PARTISSUED','ISSUED','PARTCOMPLETE','OPCOMPLETE','FINCLOSED','CANCELLED')), ReleasedBy varchar(20) NULL,
  ReleasedAt datetime2(3) NULL, ApprovalId bigint NULL, ReopenedBy varchar(20) NULL, ReopenedAt datetime2(3) NULL, ReopenReason varchar(200) NULL, WipValueGBP decimal(18,2) NOT NULL CONSTRAINT DF_ExKit_Wv DEFAULT 0,
  IsBackflush bit NOT NULL CONSTRAINT DF_ExKit_Bf DEFAULT 0, CorrelationId varchar(64) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ExKit_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL,
  ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_ExecutionKit UNIQUE (CompanyCode, KitRef));
GO
IF OBJECT_ID(N'kit.ExecutionKitLine') IS NULL CREATE TABLE kit.ExecutionKitLine (
  KitLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_ExecutionKitLine PRIMARY KEY, ExecutionKitId bigint NOT NULL REFERENCES kit.ExecutionKit(ExecutionKitId), LineNum int NOT NULL, PathRef varchar(20) NOT NULL,
  ComponentItemCode varchar(40) NOT NULL, ComponentRevision varchar(4) NULL, BomComponentId int NULL, UsageBasis varchar(12) NOT NULL CONSTRAINT DF_ExKitL_Ub DEFAULT 'PROPORTION', OriginalRequirementQty decimal(19,6) NOT NULL,
  CurrentRequirementQty decimal(19,6) NOT NULL, UomCode varchar(10) NOT NULL, ConversionFactor decimal(19,9) NOT NULL CONSTRAINT DF_ExKitL_Cf DEFAULT 1, ScrapPct decimal(9,6) NOT NULL CONSTRAINT DF_ExKitL_Sp DEFAULT 0,
  ReservedQty decimal(19,6) NOT NULL CONSTRAINT DF_ExKitL_Rq DEFAULT 0, PickedQty decimal(19,6) NOT NULL CONSTRAINT DF_ExKitL_Pq DEFAULT 0, GrossIssuedQty decimal(19,6) NOT NULL CONSTRAINT DF_ExKitL_Gq DEFAULT 0,
  ReturnedQty decimal(19,6) NOT NULL CONSTRAINT DF_ExKitL_Xq DEFAULT 0, ScrappedQty decimal(19,6) NOT NULL CONSTRAINT DF_ExKitL_Sq DEFAULT 0, NetIssuedQty decimal(19,6) NOT NULL CONSTRAINT DF_ExKitL_Nq DEFAULT 0,
  RemainingQty decimal(19,6) NOT NULL CONSTRAINT DF_ExKitL_Mq DEFAULT 0, ExcessQty decimal(19,6) NOT NULL CONSTRAINT DF_ExKitL_Eq DEFAULT 0, SubstituteItemCode varchar(40) NULL, SubstituteApprovalId bigint NULL, DeviationReason varchar(300) NULL,
  DeviationApprovedBy varchar(20) NULL, NeedDate date NULL, IssueLocationCode varchar(20) NULL, PlannedUnitCostGBP decimal(19,6) NULL, IssuedValueGBP decimal(18,2) NOT NULL CONSTRAINT DF_ExKitL_Iv DEFAULT 0,
  Status varchar(12) NOT NULL CONSTRAINT DF_ExKitL_St DEFAULT 'OPEN' CONSTRAINT CK_ExKitL_St CHECK (Status IN ('OPEN','PARTISSUED','ISSUED','CLOSED','CANCELLED')), CONSTRAINT UQ_ExecutionKitLine UNIQUE (ExecutionKitId, LineNum));
GO
