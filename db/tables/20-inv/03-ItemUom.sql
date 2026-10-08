/*  inv.ItemUom / inv.ItemSite - item unit conversions (stock units per entered unit) and
    site-specific planning parameters.  REQ: ITM-005..ITM-008, D-09 */
SET NOCOUNT ON;
IF OBJECT_ID(N'inv.ItemUom') IS NULL CREATE TABLE inv.ItemUom (
  CompanyCode varchar(10) NOT NULL, ItemCode varchar(40) NOT NULL, UomCode varchar(10) NOT NULL REFERENCES inv.Uom(UomCode),
  UsageType varchar(10) NOT NULL CONSTRAINT CK_ItemUom_Use CHECK (UsageType IN ('STOCK','PURCHASE','SALES','ISSUE','PRICE','PACK')), StockUnitsPerUom decimal(19,9) NOT NULL CONSTRAINT CK_ItemUom_F CHECK (StockUnitsPerUom > 0),
  PackDescription varchar(40) NULL, EffectiveFrom date NOT NULL CONSTRAINT DF_ItemUom_VF DEFAULT '2000-01-01', EffectiveTo date NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ItemUom_CA DEFAULT SYSDATETIME(),
  ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_ItemUom PRIMARY KEY (CompanyCode, ItemCode, UomCode, UsageType, EffectiveFrom));
GO
IF OBJECT_ID(N'inv.ItemSite') IS NULL CREATE TABLE inv.ItemSite (
  CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, ItemCode varchar(40) NOT NULL, DefaultLocationCode varchar(20) NULL, DefaultWarehouseCode varchar(20) NULL, MakeBuy varchar(6) NULL,
  LeadTimeDays smallint NOT NULL CONSTRAINT DF_ItemSite_Lt DEFAULT 0, MinOrderQty decimal(19,6) NULL, PurchaseMultipleQty decimal(19,6) NULL, ReorderPointQty decimal(19,6) NULL, SafetyStockQty decimal(19,6) NULL, MaxStockQty decimal(19,6) NULL,
  AbcClass char(1) NULL, IsActive bit NOT NULL CONSTRAINT DF_ItemSite_Act DEFAULT 1, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ItemSite_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL,
  ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_ItemSite PRIMARY KEY (CompanyCode, SiteCode, ItemCode));
GO
