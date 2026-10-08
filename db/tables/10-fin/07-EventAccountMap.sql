/*  fin.EventAccountMap - approved account mapping for every in-scope business event.  A missing
    mapping blocks the posting; there is no silent suspense account.  REQ: FIN-003 */
SET NOCOUNT ON;
IF OBJECT_ID(N'fin.EventAccountMap') IS NULL CREATE TABLE fin.EventAccountMap (
  CompanyCode varchar(10) NOT NULL, EventCode varchar(30) NOT NULL, LegCode varchar(20) NOT NULL, Module varchar(6) NOT NULL, EventName varchar(100) NOT NULL, LegDescription varchar(100) NOT NULL,
  DrCr char(2) NOT NULL CONSTRAINT CK_EvMap_DrCr CHECK (DrCr IN ('DR','CR')), AccountCode varchar(12) NOT NULL, DimCostCentre varchar(20) NULL, DimProject varchar(20) NULL, IsConditional bit NOT NULL CONSTRAINT DF_EvMap_Cond DEFAULT 0,
  Condition varchar(60) NULL, SortOrder int NOT NULL CONSTRAINT DF_EvMap_So DEFAULT 10, IsActive bit NOT NULL CONSTRAINT DF_EvMap_Act DEFAULT 1, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_EvMap_CA DEFAULT SYSDATETIME(),
  ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_EventAccountMap PRIMARY KEY (CompanyCode, EventCode, LegCode));
GO
