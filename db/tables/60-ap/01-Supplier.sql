/*  ap.Supplier / ap.SupplierAddress / ap.SupplierDueDiligence - purchase-ledger identity,
    business-size classification for payment reporting, and due-diligence evidence.
    REQ: AP-001, AP-028, AP-030, PO-026, D-29 */
SET NOCOUNT ON;
IF OBJECT_ID(N'ap.Supplier') IS NULL CREATE TABLE ap.Supplier (
  CompanyCode varchar(10) NOT NULL REFERENCES core.Company(CompanyCode), SupplierCode varchar(20) NOT NULL, LegalName varchar(120) NOT NULL, TradingName varchar(120) NULL,
  LegalForm varchar(14) NOT NULL CONSTRAINT DF_Supp_Lf DEFAULT 'LTD' CONSTRAINT CK_Supp_Lf CHECK (LegalForm IN ('LTD','PLC','LLP','PARTNERSHIP','SOLETRADER','PUBLICBODY','OVERSEAS','CHARITY')), CompaniesHouseNo varchar(10) NULL,
  CompaniesHouseStatus varchar(20) NULL, CompaniesHouseCheckedAt date NULL, VatLocation varchar(3) NOT NULL CONSTRAINT DF_Supp_Vl DEFAULT 'GB' CONSTRAINT CK_Supp_Vl CHECK (VatLocation IN ('GB','XI','EU','ROW')), VatRegNo varchar(15) NULL,
  VatValidatedAt date NULL, VatValidationRef varchar(40) NULL, Eori varchar(17) NULL, CountryCode char(2) NOT NULL CONSTRAINT DF_Supp_Cc DEFAULT 'GB',
  BusinessSize varchar(8) NOT NULL CONSTRAINT DF_Supp_Bs DEFAULT 'MEDIUM' CONSTRAINT CK_Supp_Bs CHECK (BusinessSize IN ('SMALL','MEDIUM','LARGE')), CurrencyCode char(3) NOT NULL CONSTRAINT DF_Supp_Cur DEFAULT 'GBP',
  AllowedCurrencies varchar(40) NOT NULL CONSTRAINT DF_Supp_Ac DEFAULT 'GBP', TermsCode varchar(12) NULL, ApControlAccount varchar(12) NULL, DefaultExpenseAccount varchar(12) NULL, DimCostCentre varchar(20) NULL,
  PaymentMethod varchar(10) NOT NULL CONSTRAINT DF_Supp_Pm DEFAULT 'BACS' CONSTRAINT CK_Supp_Pm CHECK (PaymentMethod IN ('BACS','FPS','CHAPS','SWIFT','SEPA','NONE')), MinimumPaymentGBP decimal(18,2) NULL, BuyerUser varchar(20) NULL,
  ApOwnerUser varchar(20) NULL, PurchaseHold bit NOT NULL CONSTRAINT DF_Supp_Ph DEFAULT 0, PaymentHold bit NOT NULL CONSTRAINT DF_Supp_Pay DEFAULT 0, HoldReason varchar(200) NULL,
  SanctionsStatus varchar(12) NOT NULL CONSTRAINT DF_Supp_Ss DEFAULT 'CLEAR' CONSTRAINT CK_Supp_Ss CHECK (SanctionsStatus IN ('CLEAR','POTENTIAL','MATCH','BLOCKED')), SanctionsScreenedAt datetime2(3) NULL,
  IsCisOrConstructionDrc bit NOT NULL CONSTRAINT DF_Supp_Cis DEFAULT 0, EInvoiceEnabled bit NOT NULL CONSTRAINT DF_Supp_Ei DEFAULT 0, IsPersonalData bit NOT NULL CONSTRAINT DF_Supp_Pd DEFAULT 0, RetentionClass varchar(16) NULL,
  Status varchar(10) NOT NULL CONSTRAINT DF_Supp_St DEFAULT 'ACTIVE' CONSTRAINT CK_Supp_St CHECK (Status IN ('PENDING','ACTIVE','ONHOLD','SUSPENDED','RETIRED')), AnnualSpendGBP decimal(18,2) NULL, LastTransactionDate date NULL,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Supp_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_Supplier PRIMARY KEY (CompanyCode, SupplierCode));
GO
IF OBJECT_ID(N'ap.SupplierAddress') IS NULL CREATE TABLE ap.SupplierAddress (
  AddressId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_SupplierAddress PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SupplierCode varchar(20) NOT NULL,
  AddressRole varchar(12) NOT NULL CONSTRAINT CK_SuppAdr_Role CHECK (AddressRole IN ('REGISTERED','ORDER','REMIT','COLLECT')), AddressRef varchar(10) NOT NULL CONSTRAINT DF_SuppAdr_Ref DEFAULT 'MAIN', Name varchar(120) NULL,
  Line1 varchar(60) NOT NULL, Line2 varchar(60) NULL, Line3 varchar(60) NULL, Town varchar(40) NULL, County varchar(40) NULL, Postcode varchar(12) NULL, CountryCode char(2) NOT NULL CONSTRAINT DF_SuppAdr_Cc DEFAULT 'GB', ContactName varchar(80) NULL,
  ContactEmail varchar(128) NULL, ContactPhone varchar(30) NULL, ContactPurpose varchar(40) NULL, IsPersonalData bit NOT NULL CONSTRAINT DF_SuppAdr_Pd DEFAULT 0, IsDefault bit NOT NULL CONSTRAINT DF_SuppAdr_Def DEFAULT 0,
  ValidFrom date NOT NULL CONSTRAINT DF_SuppAdr_VF DEFAULT '2000-01-01', ValidTo date NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_SuppAdr_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL,
  ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_SupplierAddress UNIQUE (CompanyCode, SupplierCode, AddressRole, AddressRef));
GO
IF OBJECT_ID(N'ap.SupplierDueDiligence') IS NULL CREATE TABLE ap.SupplierDueDiligence (
  DueDiligenceId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_SupplierDueDiligence PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SupplierCode varchar(20) NOT NULL,
  EvidenceType varchar(20) NOT NULL CONSTRAINT CK_SuppDd_Type CHECK (EvidenceType IN ('SANCTIONS','MODERNSLAVERY','ANTIBRIBERY','INSURANCE','QUALITYCERT','FINANCIAL','OTHER')), Reference varchar(60) NULL, ObtainedDate date NULL, ExpiryDate date NULL,
  ReviewDate date NULL, Outcome varchar(12) NOT NULL CONSTRAINT DF_SuppDd_Out DEFAULT 'PASS' CONSTRAINT CK_SuppDd_Out CHECK (Outcome IN ('PASS','FAIL','PENDING','EXPIRED','WAIVED')), IsMandatory bit NOT NULL CONSTRAINT DF_SuppDd_M DEFAULT 0,
  Note varchar(300) NULL, RecordedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_SuppDd_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion);
GO
