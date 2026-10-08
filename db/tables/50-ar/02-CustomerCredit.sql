/*  ar.CustomerCredit / ar.CreditOverride - credit policy, exposure control and scoped,
    expiring overrides granted by an independent authority.  REQ: AR-007, AR-008, SO-007, D-06 */
SET NOCOUNT ON;
IF OBJECT_ID(N'ar.CustomerCredit') IS NULL CREATE TABLE ar.CustomerCredit (
  CompanyCode varchar(10) NOT NULL, CustomerCode varchar(20) NOT NULL, CreditLimitGBP decimal(18,2) NOT NULL CONSTRAINT DF_CustCr_Lim DEFAULT 0, OverdueThresholdGBP decimal(18,2) NOT NULL CONSTRAINT DF_CustCr_Ovd DEFAULT 0,
  OverdueDaysTrigger smallint NOT NULL CONSTRAINT DF_CustCr_Odt DEFAULT 30, RiskClass varchar(10) NULL, CreditAgency varchar(40) NULL, CreditScore int NULL, CreditScoreDate date NULL, CreditInsuranceCoverGBP decimal(18,2) NULL,
  CreditInsurer varchar(60) NULL, ReviewDate date NULL, ControllerUser varchar(20) NULL, ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL, HoldFlag bit NOT NULL CONSTRAINT DF_CustCr_Hold DEFAULT 0, HoldReason varchar(200) NULL,
  HoldSetBy varchar(20) NULL, HoldSetAt datetime2(3) NULL, RequiresPrepayment bit NOT NULL CONSTRAINT DF_CustCr_Pre DEFAULT 0, ProFormaOnly bit NOT NULL CONSTRAINT DF_CustCr_Pf DEFAULT 0, LastExposureGBP decimal(18,2) NULL,
  LastExposureAt datetime2(3) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_CustCr_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT PK_CustomerCredit PRIMARY KEY (CompanyCode, CustomerCode));
GO
IF OBJECT_ID(N'ar.CreditOverride') IS NULL CREATE TABLE ar.CreditOverride (
  OverrideId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_CreditOverride PRIMARY KEY, CompanyCode varchar(10) NOT NULL, CustomerCode varchar(20) NOT NULL,
  Scope varchar(12) NOT NULL CONSTRAINT CK_CrOv_Scope CHECK (Scope IN ('ORDER','ACCOUNT','DESPATCH')), DocumentRef varchar(40) NULL, OverrideAmountGBP decimal(18,2) NOT NULL, ExcessGBP decimal(18,2) NULL, Reason varchar(300) NOT NULL,
  RequestedBy varchar(20) NOT NULL, ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL, ApprovalId bigint NULL, ValidFrom date NOT NULL, ExpiresOn date NOT NULL,
  Status varchar(10) NOT NULL CONSTRAINT DF_CrOv_St DEFAULT 'PENDING' CONSTRAINT CK_CrOv_St CHECK (Status IN ('PENDING','ACTIVE','EXPIRED','REVOKED','REJECTED')), CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_CrOv_CA DEFAULT SYSDATETIME(),
  RowVer rowversion);
GO
