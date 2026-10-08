/*  cst.CostElement / cst.CostPolicy - cost element catalogue and the per-valuation-group
    valuation method (moving weighted average, FIFO or standard; LIFO is prohibited).
    REQ: CST-002, CST-004, CST-026, D-04 */
SET NOCOUNT ON;
IF OBJECT_ID(N'cst.CostElement') IS NULL CREATE TABLE cst.CostElement (
  ElementCode varchar(10) NOT NULL CONSTRAINT PK_CostElement PRIMARY KEY, Name varchar(60) NOT NULL,
  ElementType varchar(12) NOT NULL CONSTRAINT CK_CstEl_Type CHECK (ElementType IN ('MATERIAL','LABOUR','MACHINE','SUBCON','OVHDVAR','OVHDFIX','LANDED','RECOVERY','VARIANCE')), IsInventoriable bit NOT NULL CONSTRAINT DF_CstEl_Inv DEFAULT 1,
  SortOrder int NOT NULL CONSTRAINT DF_CstEl_So DEFAULT 10, RowVer rowversion);
GO
IF OBJECT_ID(N'cst.CostPolicy') IS NULL CREATE TABLE cst.CostPolicy (
  CompanyCode varchar(10) NOT NULL, ValuationGroup varchar(20) NOT NULL, EffectiveFrom date NOT NULL, CostMethod varchar(4) NOT NULL CONSTRAINT CK_CstPol_M CHECK (CostMethod IN ('MWA','FIFO','STD')),
  PoolBySite bit NOT NULL CONSTRAINT DF_CstPol_Ps DEFAULT 0, PoolByRevision bit NOT NULL CONSTRAINT DF_CstPol_Pr DEFAULT 0, IncludeLandedCost bit NOT NULL CONSTRAINT DF_CstPol_Lc DEFAULT 1, EffectiveTo date NULL, ApprovedBy varchar(20) NULL,
  ConversionDate date NULL, Notes varchar(200) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_CstPol_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT PK_CostPolicy PRIMARY KEY (CompanyCode, ValuationGroup, EffectiveFrom));
GO
IF OBJECT_ID(N'cst.WorkCentre') IS NULL CREATE TABLE cst.WorkCentre (
  CompanyCode varchar(10) NOT NULL, WorkCentreCode varchar(20) NOT NULL, Name varchar(60) NOT NULL, SiteCode varchar(10) NULL, EffectiveFrom date NOT NULL CONSTRAINT DF_Wc_VF DEFAULT '2000-01-01',
  SetupRatePerHour decimal(19,6) NOT NULL CONSTRAINT DF_Wc_Sr DEFAULT 0, RunRatePerHour decimal(19,6) NOT NULL CONSTRAINT DF_Wc_Rr DEFAULT 0, MachineRatePerHour decimal(19,6) NOT NULL CONSTRAINT DF_Wc_Mr DEFAULT 0,
  OverheadRatePerHour decimal(19,6) NOT NULL CONSTRAINT DF_Wc_Or DEFAULT 0, OverheadBasis varchar(10) NOT NULL CONSTRAINT DF_Wc_Ob DEFAULT 'LABOURHR' CONSTRAINT CK_Wc_Ob CHECK (OverheadBasis IN ('LABOURHR','MACHINEHR','PCTCOST')),
  NormalCapacityHours decimal(19,6) NULL, CostCentre varchar(20) NULL, ApprovedBy varchar(20) NULL, IsActive bit NOT NULL CONSTRAINT DF_Wc_Act DEFAULT 1, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Wc_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_WorkCentre PRIMARY KEY (CompanyCode, WorkCentreCode, EffectiveFrom));
GO
