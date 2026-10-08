/*  core.Company / core.CompanyProfile - legal companies and effective-dated statutory profile.
    REQ: COM-001, COM-020, COM-025, FIN-030, D-01 */
SET NOCOUNT ON;
IF OBJECT_ID(N'core.Company') IS NULL CREATE TABLE core.Company (
  CompanyCode varchar(10) NOT NULL CONSTRAINT PK_Company PRIMARY KEY, Name varchar(100) NOT NULL, TradingName varchar(100) NULL, LegalForm varchar(20) NOT NULL CONSTRAINT DF_Company_LF DEFAULT 'LTD', RegisteredNumber varchar(10) NULL,
  Jurisdiction varchar(30) NOT NULL CONSTRAINT DF_Company_Jur DEFAULT 'England and Wales', VatRegNo varchar(15) NULL, XiVatRegNo varchar(15) NULL, Eori varchar(17) NULL, XiEori varchar(17) NULL,
  AccountingFramework varchar(10) NOT NULL CONSTRAINT DF_Company_Fw DEFAULT 'FRS102' CONSTRAINT CK_Company_Fw CHECK (AccountingFramework IN ('FRS102','FRS105')), AccountingRefDate char(5) NOT NULL CONSTRAINT DF_Company_Ard DEFAULT '03-31',
  VatStagger varchar(12) NOT NULL CONSTRAINT DF_Company_Stg DEFAULT 'QUARTER-03', VatScheme varchar(12) NOT NULL CONSTRAINT DF_Company_Sch DEFAULT 'INVOICE' CONSTRAINT CK_Company_Sch CHECK (VatScheme IN ('INVOICE','CASH')),
  BaseCurrency char(3) NOT NULL CONSTRAINT DF_Company_Cur DEFAULT 'GBP', CalendarCode varchar(10) NOT NULL CONSTRAINT DF_Company_Cal DEFAULT 'ENG', RegOfficeLine1 varchar(60) NULL, RegOfficeLine2 varchar(60) NULL, RegOfficeTown varchar(40) NULL,
  RegOfficePostcode varchar(10) NULL, RegOfficeCountry varchar(40) NULL, PaymentPracticesInScope bit NOT NULL CONSTRAINT DF_Company_Ppr DEFAULT 0, PackagingInScope bit NOT NULL CONSTRAINT DF_Company_Pkg DEFAULT 0,
  EInvoicingEnabled bit NOT NULL CONSTRAINT DF_Company_Einv DEFAULT 0, DomesticReverseChargeEnabled bit NOT NULL CONSTRAINT DF_Company_Drc DEFAULT 0, IsActive bit NOT NULL CONSTRAINT DF_Company_Act DEFAULT 1, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Company_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion);
GO
IF OBJECT_ID(N'core.CompanyProfile') IS NULL CREATE TABLE core.CompanyProfile (
  CompanyCode varchar(10) NOT NULL REFERENCES core.Company(CompanyCode), EffectiveFrom date NOT NULL, EffectiveTo date NULL, VatRegNo varchar(15) NULL, XiVatRegNo varchar(15) NULL, Eori varchar(17) NULL, AccountingFramework varchar(10) NOT NULL,
  AccountingRefDate char(5) NOT NULL, VatStagger varchar(12) NOT NULL, VatScheme varchar(12) NOT NULL, VatRegisteredFrom date NULL, VatDeregisteredFrom date NULL, RegOfficeLine1 varchar(60) NULL, RegOfficeTown varchar(40) NULL,
  RegOfficePostcode varchar(10) NULL, ChangeReason varchar(200) NULL, AuthorityRef varchar(40) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_CoProf_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL,
  ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_CompanyProfile PRIMARY KEY (CompanyCode, EffectiveFrom));
GO
