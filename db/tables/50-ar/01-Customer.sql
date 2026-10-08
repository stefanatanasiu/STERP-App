/*  ar.Customer - sales-ledger identity, VAT and trade profile, and the parties that order,
    are invoiced, pay and are stated.  REQ: AR-001, AR-002, AR-027, AR-028, D-27, D-29 */
SET NOCOUNT ON;
IF OBJECT_ID(N'ar.Customer') IS NULL CREATE TABLE ar.Customer (
  CompanyCode varchar(10) NOT NULL REFERENCES core.Company(CompanyCode), CustomerCode varchar(20) NOT NULL, LegalName varchar(120) NOT NULL, TradingName varchar(120) NULL,
  LegalForm varchar(14) NOT NULL CONSTRAINT DF_Cust_Lf DEFAULT 'LTD' CONSTRAINT CK_Cust_Lf CHECK (LegalForm IN ('LTD','PLC','LLP','PARTNERSHIP','SOLETRADER','PUBLICBODY','OVERSEAS','CHARITY')), CompaniesHouseNo varchar(10) NULL,
  CompaniesHouseStatus varchar(20) NULL, CompaniesHouseCheckedAt date NULL, VatLocation varchar(3) NOT NULL CONSTRAINT DF_Cust_Vl DEFAULT 'GB' CONSTRAINT CK_Cust_Vl CHECK (VatLocation IN ('GB','XI','EU','ROW')), VatRegNo varchar(15) NULL,
  VatValidatedAt date NULL, VatValidationRef varchar(40) NULL, IsVatBusiness bit NOT NULL CONSTRAINT DF_Cust_Vb DEFAULT 1, DrcEligible bit NOT NULL CONSTRAINT DF_Cust_Drc DEFAULT 0, Eori varchar(17) NULL,
  CountryCode char(2) NOT NULL CONSTRAINT DF_Cust_Cc DEFAULT 'GB', CurrencyCode char(3) NOT NULL CONSTRAINT DF_Cust_Cur DEFAULT 'GBP', AllowedCurrencies varchar(40) NOT NULL CONSTRAINT DF_Cust_Ac DEFAULT 'GBP', TermsCode varchar(12) NULL,
  ArControlAccount varchar(12) NULL, CollectionMethod varchar(10) NOT NULL CONSTRAINT DF_Cust_Cm DEFAULT 'BACS' CONSTRAINT CK_Cust_Cm CHECK (CollectionMethod IN ('BACS','FPS','DD','CHAPS','CARD','CHEQUE')),
  StatementFrequency varchar(10) NOT NULL CONSTRAINT DF_Cust_Sf DEFAULT 'MONTHLY', PriceListCode varchar(20) NULL, PriceGroup varchar(20) NULL, SalespersonCode varchar(20) NULL, AccountOwner varchar(20) NULL, Classification varchar(20) NULL,
  InvoiceCustomerCode varchar(20) NULL, PayerCustomerCode varchar(20) NULL, StatementCustomerCode varchar(20) NULL, DeliveryTermsCode varchar(10) NULL, RequiresPurchaseOrderRef bit NOT NULL CONSTRAINT DF_Cust_Po DEFAULT 0,
  EInvoiceEnabled bit NOT NULL CONSTRAINT DF_Cust_Ei DEFAULT 0, SanctionsStatus varchar(12) NOT NULL CONSTRAINT DF_Cust_Ss DEFAULT 'CLEAR', SanctionsScreenedAt datetime2(3) NULL, InsolvencyType varchar(16) NULL, InsolvencyDate date NULL,
  OfficeHolder varchar(120) NULL, ProofOfDebtRef varchar(40) NULL, IsPersonalData bit NOT NULL CONSTRAINT DF_Cust_Pd DEFAULT 0, RetentionClass varchar(16) NULL, ContactRestriction varchar(100) NULL,
  Status varchar(10) NOT NULL CONSTRAINT DF_Cust_St DEFAULT 'ACTIVE' CONSTRAINT CK_Cust_St CHECK (Status IN ('PROSPECT','ACTIVE','ONHOLD','SUSPENDED','RETIRED')), StatusReason varchar(200) NULL, FirstTradedDate date NULL,
  LastTransactionDate date NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Cust_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT PK_Customer PRIMARY KEY (CompanyCode, CustomerCode));
GO
IF OBJECT_ID(N'ar.CustomerAddress') IS NULL CREATE TABLE ar.CustomerAddress (
  AddressId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_CustomerAddress PRIMARY KEY, CompanyCode varchar(10) NOT NULL, CustomerCode varchar(20) NOT NULL,
  AddressRole varchar(12) NOT NULL CONSTRAINT CK_CustAdr_Role CHECK (AddressRole IN ('REGISTERED','BILLING','STATEMENT','DELIVERY')), AddressRef varchar(10) NOT NULL CONSTRAINT DF_CustAdr_Ref DEFAULT 'MAIN', Name varchar(120) NULL,
  Line1 varchar(60) NOT NULL, Line2 varchar(60) NULL, Line3 varchar(60) NULL, Town varchar(40) NULL, County varchar(40) NULL, Postcode varchar(12) NULL, CountryCode char(2) NOT NULL CONSTRAINT DF_CustAdr_Cc DEFAULT 'GB', ContactName varchar(80) NULL,
  ContactEmail varchar(128) NULL, ContactPhone varchar(30) NULL, ContactPurpose varchar(40) NULL, IsPersonalData bit NOT NULL CONSTRAINT DF_CustAdr_Pd DEFAULT 0, IsDefault bit NOT NULL CONSTRAINT DF_CustAdr_Def DEFAULT 0,
  ValidFrom date NOT NULL CONSTRAINT DF_CustAdr_VF DEFAULT '2000-01-01', ValidTo date NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_CustAdr_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL,
  ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_CustomerAddress UNIQUE (CompanyCode, CustomerCode, AddressRole, AddressRef));
GO
