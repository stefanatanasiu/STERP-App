/*  fin.Account - governed chart of accounts, control-account designation and statutory mapping.
    REQ: FIN-001, FIN-003, FIN-005, FIN-023, FIN-030 */
SET NOCOUNT ON;
IF OBJECT_ID(N'fin.Account') IS NULL CREATE TABLE fin.Account (
  CompanyCode varchar(10) NOT NULL REFERENCES core.Company(CompanyCode), AccountCode varchar(12) NOT NULL, Name varchar(80) NOT NULL, ClassCode varchar(10) NOT NULL REFERENCES fin.AccountClass(ClassCode),
  AccountKind varchar(8) NOT NULL CONSTRAINT DF_Acc_Kind DEFAULT 'POSTING' CONSTRAINT CK_Acc_Kind CHECK (AccountKind IN ('POSTING','SUMMARY')),
  ControlType varchar(10) NULL CONSTRAINT CK_Acc_Ctl CHECK (ControlType IN ('AR','AP','INVENTORY','WIP','VAT','GRNI','ACCRUAL','BANK','DEPOSIT','RETAINED')), ControlSubledger varchar(6) NULL,
  AllowManualJournal bit NOT NULL CONSTRAINT DF_Acc_Man DEFAULT 1, CurrencyCode char(3) NULL, IsVatEligible bit NOT NULL CONSTRAINT DF_Acc_Vat DEFAULT 0, StatutoryLineCode varchar(20) NULL, ManagementGroup varchar(30) NULL,
  TaxAnalysisFlag varchar(20) NULL, ValidFrom date NOT NULL CONSTRAINT DF_Acc_VF DEFAULT '2000-01-01', ValidTo date NULL,
  Status varchar(10) NOT NULL CONSTRAINT DF_Acc_St DEFAULT 'ACTIVE' CONSTRAINT CK_Acc_St CHECK (Status IN ('ACTIVE','BLOCKED','RETIRED')), CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Acc_CA DEFAULT SYSDATETIME(),
  ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_Account PRIMARY KEY (CompanyCode, AccountCode));
GO
