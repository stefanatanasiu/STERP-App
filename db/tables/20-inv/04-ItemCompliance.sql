/*  inv.ItemVatClass / inv.ItemCustoms / inv.ItemPackaging - effective-dated VAT liability,
    customs data and packaging composition.  REQ: ITM-025, ITM-026, ITM-027, RPT-019 */
SET NOCOUNT ON;
IF OBJECT_ID(N'inv.ItemVatClass') IS NULL CREATE TABLE inv.ItemVatClass (
  CompanyCode varchar(10) NOT NULL, ItemCode varchar(40) NOT NULL, Direction varchar(6) NOT NULL CONSTRAINT CK_ItemVat_Dir CHECK (Direction IN ('SALE','PURCH')), EffectiveFrom date NOT NULL,
  VatCode varchar(4) NOT NULL REFERENCES fin.VatCode(VatCode), EffectiveTo date NULL, EvidenceRef varchar(100) NULL, ApprovedBy varchar(20) NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ItemVat_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_ItemVatClass PRIMARY KEY (CompanyCode, ItemCode, Direction, EffectiveFrom));
GO
IF OBJECT_ID(N'inv.ItemCustoms') IS NULL CREATE TABLE inv.ItemCustoms (
  CompanyCode varchar(10) NOT NULL, ItemCode varchar(40) NOT NULL, EffectiveFrom date NOT NULL, CommodityCode varchar(10) NOT NULL, CountryOfOrigin char(2) NOT NULL, PreferentialOrigin bit NOT NULL CONSTRAINT DF_ItemCus_Pref DEFAULT 0,
  PreferenceEvidenceRef varchar(60) NULL, DutyRatePct decimal(9,4) NULL, ExportControlled bit NOT NULL CONSTRAINT DF_ItemCus_Exp DEFAULT 0, ExportLicenceRef varchar(40) NULL, SupplementaryUnit varchar(10) NULL,
  NetWeightKgPerStockUnit decimal(19,6) NULL, EffectiveTo date NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ItemCus_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL,
  RowVer rowversion, CONSTRAINT PK_ItemCustoms PRIMARY KEY (CompanyCode, ItemCode, EffectiveFrom), CONSTRAINT CK_ItemCus_Cn CHECK (CommodityCode NOT LIKE '%[^0-9]%'));
GO
IF OBJECT_ID(N'inv.ItemPackaging') IS NULL CREATE TABLE inv.ItemPackaging (
  CompanyCode varchar(10) NOT NULL, ItemCode varchar(40) NOT NULL, PackagingMaterial varchar(20) NOT NULL, PackagingType varchar(20) NOT NULL CONSTRAINT DF_ItemPkg_Type DEFAULT 'PRIMARY', WeightGrams decimal(19,6) NOT NULL,
  RecycledContentPct decimal(9,4) NULL, NationOfSupply varchar(3) NULL, IsPlasticPackagingTax bit NOT NULL CONSTRAINT DF_ItemPkg_Ppt DEFAULT 0, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ItemPkg_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT PK_ItemPackaging PRIMARY KEY (CompanyCode, ItemCode, PackagingMaterial, PackagingType));
GO
