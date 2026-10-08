/*  inv.Lot / inv.Serial - traceability identities with expiry and certificate evidence.
    REQ: ITM-011, ITM-016, KIT-019, D-09, D-17 */
SET NOCOUNT ON;
IF OBJECT_ID(N'inv.Lot') IS NULL CREATE TABLE inv.Lot (
  CompanyCode varchar(10) NOT NULL, ItemCode varchar(40) NOT NULL, LotCode varchar(30) NOT NULL, Revision varchar(4) NULL, SupplierLotRef varchar(40) NULL, SupplierCode varchar(20) NULL, ManufacturedDate date NULL, ReceivedDate date NULL,
  ExpiryDate date NULL, QualityStatus varchar(12) NOT NULL CONSTRAINT DF_Lot_Qs DEFAULT 'ACCEPTED' CONSTRAINT CK_Lot_Qs CHECK (QualityStatus IN ('ACCEPTED','QUARANTINE','INSPECTION','REJECTED','EXPIRED')), CertificateRef varchar(60) NULL,
  OriginCountry char(2) NULL, RetentionClass varchar(16) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Lot_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT PK_Lot PRIMARY KEY (CompanyCode, ItemCode, LotCode));
GO
IF OBJECT_ID(N'inv.Serial') IS NULL CREATE TABLE inv.Serial (
  CompanyCode varchar(10) NOT NULL, ItemCode varchar(40) NOT NULL, SerialNo varchar(40) NOT NULL, LotCode varchar(30) NULL, Revision varchar(4) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_Ser_St DEFAULT 'INSTOCK' CONSTRAINT CK_Ser_St CHECK (Status IN ('INSTOCK','RESERVED','ISSUED','DESPATCHED','SCRAPPED','RETURNED')), CurrentSiteCode varchar(10) NULL, CurrentLocationCode varchar(20) NULL,
  QualityStatus varchar(12) NOT NULL CONSTRAINT DF_Ser_Qs DEFAULT 'ACCEPTED', BuiltFromKitRef varchar(30) NULL, DespatchRef varchar(30) NULL, CustomerCode varchar(20) NULL, WarrantyEndDate date NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Ser_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_Serial PRIMARY KEY (CompanyCode, ItemCode, SerialNo));
GO
