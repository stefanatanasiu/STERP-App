/*  bom.BomHeader / bom.BomComponent / bom.BomByProduct - versioned bills of material.
    Effectivity is half-open [start, end).  Yield models lost parent output; scrap is an extra
    component input fraction.  REQ: BOM-001..BOM-011, BOM-017, BOM-018, BOM-026, D-10 */
SET NOCOUNT ON;
IF OBJECT_ID(N'bom.BomHeader') IS NULL CREATE TABLE bom.BomHeader (
  BomHeaderId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_BomHeader PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NULL, BomRef varchar(30) NOT NULL, BomRevision varchar(4) NOT NULL, ParentItemCode varchar(40) NOT NULL,
  ParentRevision varchar(4) NULL, Purpose varchar(10) NOT NULL CONSTRAINT DF_Bom_Pur DEFAULT 'STANDARD' CONSTRAINT CK_Bom_Pur CHECK (Purpose IN ('STANDARD','JOB','ORDER')), JobRef varchar(20) NULL, VariantCode varchar(20) NULL,
  Description varchar(150) NULL, NominalBasis decimal(19,6) NOT NULL CONSTRAINT DF_Bom_B DEFAULT 1 CONSTRAINT CK_Bom_B CHECK (NominalBasis > 0),
  YieldFactor decimal(9,6) NOT NULL CONSTRAINT DF_Bom_Y DEFAULT 1 CONSTRAINT CK_Bom_Y CHECK (YieldFactor > 0 AND YieldFactor <= 1),
  ProductionQuantum decimal(19,6) NOT NULL CONSTRAINT DF_Bom_G DEFAULT 1 CONSTRAINT CK_Bom_G CHECK (ProductionQuantum > 0), BatchSize decimal(19,6) NULL, IsPhantom bit NOT NULL CONSTRAINT DF_Bom_Ph DEFAULT 0, CostingLotSize decimal(19,6) NULL,
  EffectiveFrom date NOT NULL, EffectiveTo date NULL, Status varchar(12) NOT NULL CONSTRAINT DF_Bom_St DEFAULT 'DRAFT' CONSTRAINT CK_Bom_St CHECK (Status IN ('DRAFT','REVIEW','RELEASED','SUPERSEDED','RETIRED')), AuthorUser varchar(20) NULL,
  ReviewerUser varchar(20) NULL, ReleasedBy varchar(20) NULL, ReleasedAt datetime2(3) NULL, ChangeRef varchar(30) NULL, ChangeReason varchar(200) NULL, EngineeringNotes varchar(400) NULL, SupersededByBomHeaderId int NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Bom_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_BomHeader UNIQUE (CompanyCode, BomRef, BomRevision),
  CONSTRAINT CK_Bom_Eff CHECK (EffectiveTo IS NULL OR EffectiveTo > EffectiveFrom));
GO
IF OBJECT_ID(N'bom.BomComponent') IS NULL CREATE TABLE bom.BomComponent (
  BomComponentId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_BomComponent PRIMARY KEY, BomHeaderId int NOT NULL REFERENCES bom.BomHeader(BomHeaderId), LineNum int NOT NULL, PathRef varchar(20) NOT NULL, ComponentItemCode varchar(40) NOT NULL,
  ComponentRevision varchar(4) NULL, UsageQty decimal(19,6) NOT NULL CONSTRAINT CK_BomC_Q CHECK (UsageQty >= 0),
  UsageBasis varchar(12) NOT NULL CONSTRAINT DF_BomC_Ub DEFAULT 'PROPORTION' CONSTRAINT CK_BomC_Ub CHECK (UsageBasis IN ('PROPORTION','FIXEDRUN','FIXEDBATCH')), UomCode varchar(10) NOT NULL,
  ConversionFactor decimal(19,9) NOT NULL CONSTRAINT DF_BomC_F DEFAULT 1 CONSTRAINT CK_BomC_F CHECK (ConversionFactor > 0), ScrapPct decimal(9,6) NOT NULL CONSTRAINT DF_BomC_S DEFAULT 0 CONSTRAINT CK_BomC_S CHECK (ScrapPct >= 0),
  SupplyBehaviour varchar(10) NOT NULL CONSTRAINT DF_BomC_Sb DEFAULT 'PURCHASED' CONSTRAINT CK_BomC_Sb CHECK (SupplyBehaviour IN ('PURCHASED','MAKE','USESTOCK','PHANTOM','NONSTOCK','SERVICE','COSTONLY','NOTE')),
  IsOptional bit NOT NULL CONSTRAINT DF_BomC_Opt DEFAULT 0, AlternateGroup varchar(10) NULL, IsSelected bit NOT NULL CONSTRAINT DF_BomC_Sel DEFAULT 1, EffectiveFrom date NULL, EffectiveTo date NULL,
  NeedOffsetDays smallint NOT NULL CONSTRAINT DF_BomC_Off DEFAULT 0, OperationRef varchar(20) NULL, IssueLocationCode varchar(20) NULL, QualityRestriction varchar(60) NULL, Notes varchar(300) NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_BomC_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_BomComponent UNIQUE (BomHeaderId, LineNum));
GO
IF OBJECT_ID(N'bom.BomByProduct') IS NULL CREATE TABLE bom.BomByProduct (
  BomByProductId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_BomByProduct PRIMARY KEY, BomHeaderId int NOT NULL REFERENCES bom.BomHeader(BomHeaderId), LineNum int NOT NULL, ByProductItemCode varchar(40) NOT NULL,
  RecoveryQty decimal(19,6) NOT NULL CONSTRAINT CK_BomBp_Q CHECK (RecoveryQty > 0), UomCode varchar(10) NOT NULL, ConversionFactor decimal(19,9) NOT NULL CONSTRAINT DF_BomBp_F DEFAULT 1,
  ValuationBasis varchar(12) NOT NULL CONSTRAINT DF_BomBp_Vb DEFAULT 'NRV' CONSTRAINT CK_BomBp_Vb CHECK (ValuationBasis IN ('NRV','STANDARD','FIXED','NONE')), RecoveryValue decimal(19,6) NULL, DispositionRule varchar(60) NULL,
  ApprovedBy varchar(20) NULL, RowVer rowversion, CONSTRAINT UQ_BomByProduct UNIQUE (BomHeaderId, LineNum));
GO
