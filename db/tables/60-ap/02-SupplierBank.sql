/*  ap.SupplierBank - payment destinations.  A new or changed destination needs Confirmation of
    Payee plus an independent call-back to a contact already on record, then approval by someone
    other than the preparer.  Restricted data (D-17).  REQ: AP-002, AP-027, COM-003, COM-008 */
SET NOCOUNT ON;
IF OBJECT_ID(N'ap.SupplierBank') IS NULL CREATE TABLE ap.SupplierBank (
  SupplierBankId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_SupplierBank PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SupplierCode varchar(20) NOT NULL, BankRef varchar(10) NOT NULL CONSTRAINT DF_SuppBk_Ref DEFAULT 'MAIN',
  BeneficiaryName varchar(120) NOT NULL, CountryCode char(2) NOT NULL CONSTRAINT DF_SuppBk_Cc DEFAULT 'GB', CurrencyCode char(3) NOT NULL CONSTRAINT DF_SuppBk_Cur DEFAULT 'GBP', SortCode char(6) NULL, AccountNumber char(8) NULL,
  RollNumber varchar(18) NULL, Iban varchar(34) NULL, Bic varchar(11) NULL, BankName varchar(60) NULL, PaymentMethod varchar(10) NOT NULL CONSTRAINT DF_SuppBk_Pm DEFAULT 'BACS',
  CopResult varchar(12) NULL CONSTRAINT CK_SuppBk_Cop CHECK (CopResult IN ('MATCH','CLOSEMATCH','NOMATCH','UNAVAILABLE')), CopCheckedAt datetime2(3) NULL, CopReference varchar(60) NULL, CopDecision varchar(200) NULL,
  CallBackVerifiedAt datetime2(3) NULL, CallBackVerifiedBy varchar(20) NULL, CallBackContactSource varchar(120) NULL, CallBackNote varchar(300) NULL, ValidFrom date NOT NULL CONSTRAINT DF_SuppBk_VF DEFAULT '2000-01-01', ValidTo date NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_SuppBk_St DEFAULT 'PENDING' CONSTRAINT CK_SuppBk_St CHECK (Status IN ('PENDING','VERIFIED','APPROVED','REJECTED','SUPERSEDED')), PreparedBy varchar(20) NULL, ApprovedBy varchar(20) NULL,
  ApprovedAt datetime2(3) NULL, ApprovalId bigint NULL, SupersedesBankId int NULL, IsSensitive bit NOT NULL CONSTRAINT DF_SuppBk_Sens DEFAULT 1, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_SuppBk_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_SupplierBank UNIQUE (CompanyCode, SupplierCode, BankRef, ValidFrom));
GO
