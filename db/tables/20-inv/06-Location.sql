/*  inv.Warehouse / inv.Location - physical storage, including quarantine, inspection, WIP,
    in-transit, scrap and subcontract locations.  REQ: ITM-012, ITM-017, PO-010, D-09 */
SET NOCOUNT ON;
IF OBJECT_ID(N'inv.Warehouse') IS NULL CREATE TABLE inv.Warehouse (
  CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, WarehouseCode varchar(20) NOT NULL, Name varchar(60) NOT NULL, IsDefault bit NOT NULL CONSTRAINT DF_Whs_Def DEFAULT 0, IsActive bit NOT NULL CONSTRAINT DF_Whs_Act DEFAULT 1,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Whs_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT PK_Warehouse PRIMARY KEY (CompanyCode, SiteCode, WarehouseCode));
GO
IF OBJECT_ID(N'inv.Location') IS NULL CREATE TABLE inv.Location (
  CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, WarehouseCode varchar(20) NOT NULL, LocationCode varchar(20) NOT NULL, Name varchar(60) NOT NULL,
  LocationType varchar(12) NOT NULL CONSTRAINT CK_Loc_Type CHECK (LocationType IN ('STOCK','QUARANTINE','INSPECTION','WIP','TRANSIT','SCRAP','SUBCON','GOODSIN','DESPATCH','RETURNS')), IsAvailable bit NOT NULL CONSTRAINT DF_Loc_Avail DEFAULT 1,
  IsOwnedStock bit NOT NULL CONSTRAINT DF_Loc_Own DEFAULT 1, AllowNegative bit NOT NULL CONSTRAINT DF_Loc_Neg DEFAULT 0, PickSequence int NULL, IsActive bit NOT NULL CONSTRAINT DF_Loc_Act DEFAULT 1, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Loc_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_Location PRIMARY KEY (CompanyCode, SiteCode, WarehouseCode, LocationCode));
GO
