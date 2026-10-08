/*  cst.CostSet / cst.StandardCost / cst.StandardCostElement - approved standard cost sets with
    their element breakdown and costing lot size.  REQ: CST-007, CST-010..CST-012, D-05 */
SET NOCOUNT ON;
IF OBJECT_ID(N'cst.CostSet') IS NULL CREATE TABLE cst.CostSet (
  CompanyCode varchar(10) NOT NULL, CostSetCode varchar(20) NOT NULL, Name varchar(80) NOT NULL,
  Purpose varchar(12) NOT NULL CONSTRAINT DF_CstSet_Pur DEFAULT 'STANDARD' CONSTRAINT CK_CstSet_Pur CHECK (Purpose IN ('STANDARD','SIMULATION','QUOTE','BUDGET')), EffectiveFrom date NOT NULL,
  CurrencyCode char(3) NOT NULL CONSTRAINT DF_CstSet_Cur DEFAULT 'GBP', FiscalYear smallint NULL,
  Status varchar(10) NOT NULL CONSTRAINT DF_CstSet_St DEFAULT 'DRAFT' CONSTRAINT CK_CstSet_St CHECK (Status IN ('DRAFT','REVIEW','APPROVED','ACTIVE','SUPERSEDED')), ComparedToSetCode varchar(20) NULL, SourceEvidence varchar(200) NULL,
  PreparedBy varchar(20) NULL, ReviewedBy varchar(20) NULL, ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL, ActivatedAt datetime2(3) NULL, RevaluationJournalId bigint NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_CstSet_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_CostSet PRIMARY KEY (CompanyCode, CostSetCode));
GO
IF OBJECT_ID(N'cst.StandardCost') IS NULL CREATE TABLE cst.StandardCost (
  StandardCostId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_StandardCost PRIMARY KEY, CompanyCode varchar(10) NOT NULL, CostSetCode varchar(20) NOT NULL, ItemCode varchar(40) NOT NULL, Revision varchar(4) NOT NULL CONSTRAINT DF_StdC_Rev DEFAULT '',
  CostingLotSize decimal(19,6) NOT NULL CONSTRAINT DF_StdC_Lot DEFAULT 1 CONSTRAINT CK_StdC_Lot CHECK (CostingLotSize > 0), TotalUnitCost decimal(19,6) NOT NULL CONSTRAINT DF_StdC_T DEFAULT 0, PreviousUnitCost decimal(19,6) NULL,
  VariancePct decimal(9,4) NULL, BomHeaderId int NULL, Completeness varchar(12) NOT NULL CONSTRAINT DF_StdC_Comp DEFAULT 'COMPLETE', Warnings varchar(300) NULL, RowVer rowversion,
  CONSTRAINT UQ_StandardCost UNIQUE (CompanyCode, CostSetCode, ItemCode, Revision));
GO
IF OBJECT_ID(N'cst.StandardCostElement') IS NULL CREATE TABLE cst.StandardCostElement (
  StandardCostId int NOT NULL REFERENCES cst.StandardCost(StandardCostId), ElementCode varchar(10) NOT NULL REFERENCES cst.CostElement(ElementCode), UnitCost decimal(19,6) NOT NULL CONSTRAINT DF_StdCE_U DEFAULT 0,
  FixedCostPerLot decimal(19,6) NOT NULL CONSTRAINT DF_StdCE_F DEFAULT 0, Quantity decimal(19,6) NULL, RateUsed decimal(19,6) NULL, SourceNote varchar(200) NULL, CONSTRAINT PK_StandardCostElement PRIMARY KEY (StandardCostId, ElementCode));
GO
