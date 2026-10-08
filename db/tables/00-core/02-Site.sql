/*  core.Site - operating sites of a company, each with its bank-holiday calendar.  REQ: COM-001, COM-026 */
SET NOCOUNT ON;
IF OBJECT_ID(N'core.Site') IS NULL CREATE TABLE core.Site (
  CompanyCode varchar(10) NOT NULL REFERENCES core.Company(CompanyCode), SiteCode varchar(10) NOT NULL, Name varchar(100) NOT NULL, Nation varchar(3) NOT NULL CONSTRAINT CK_Site_Nation CHECK (Nation IN ('ENG','SCT','NIR','WAL')),
  CalendarCode varchar(10) NOT NULL CONSTRAINT DF_Site_Cal DEFAULT 'ENG', AddressLine1 varchar(60) NULL, AddressLine2 varchar(60) NULL, Town varchar(40) NULL, Postcode varchar(10) NULL,
  Country varchar(40) NOT NULL CONSTRAINT DF_Site_Ctry DEFAULT 'United Kingdom', IsManufacturing bit NOT NULL CONSTRAINT DF_Site_Mfg DEFAULT 0, IsActive bit NOT NULL CONSTRAINT DF_Site_Act DEFAULT 1, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Site_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_Site PRIMARY KEY (CompanyCode, SiteCode));
GO
