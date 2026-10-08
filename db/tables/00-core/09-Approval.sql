/*  core.Approval / core.ApprovalRule - independent approval with content hash and amount matrix.
    REQ: COM-003, COM-008, COM-022, D-07 section 2.1 */
SET NOCOUNT ON;
IF OBJECT_ID(N'core.ApprovalRule') IS NULL CREATE TABLE core.ApprovalRule (
  CompanyCode varchar(10) NOT NULL, ApprovalType varchar(30) NOT NULL, SeqNo smallint NOT NULL, MinAmount decimal(18,2) NOT NULL CONSTRAINT DF_ApprRule_Min DEFAULT 0, MaxAmount decimal(18,2) NULL, RequiredPermission varchar(40) NOT NULL,
  RequiredRole varchar(20) NULL, ApproversRequired tinyint NOT NULL CONSTRAINT DF_ApprRule_Cnt DEFAULT 1, DecisionRef varchar(10) NULL, Description varchar(200) NULL, IsActive bit NOT NULL CONSTRAINT DF_ApprRule_Act DEFAULT 1,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ApprRule_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT PK_ApprovalRule PRIMARY KEY (CompanyCode, ApprovalType, SeqNo));
GO
IF OBJECT_ID(N'core.Approval') IS NULL CREATE TABLE core.Approval (
  ApprovalId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_Approval PRIMARY KEY, CompanyCode varchar(10) NOT NULL, DocumentType varchar(20) NOT NULL, DocumentRef varchar(40) NOT NULL, ApprovalType varchar(30) NOT NULL, Amount decimal(18,2) NULL,
  CurrencyCode char(3) NULL, AmountGBP decimal(18,2) NULL, ContentHash varchar(64) NOT NULL, RequiredPermission varchar(40) NULL, RequiredRole varchar(20) NULL, RequestedBy varchar(20) NOT NULL,
  RequestedAt datetime2(3) NOT NULL CONSTRAINT DF_Appr_RA DEFAULT SYSDATETIME(), DueAt datetime2(3) NULL,
  Status varchar(14) NOT NULL CONSTRAINT DF_Appr_St DEFAULT 'PENDING' CONSTRAINT CK_Appr_St CHECK (Status IN ('PENDING','APPROVED','REJECTED','INVALIDATED')), DecidedBy varchar(20) NULL, DecidedAt datetime2(3) NULL, Comment varchar(400) NULL,
  DelegationId int NULL, InvalidatedReason varchar(200) NULL, CorrelationId varchar(64) NULL, RowVer rowversion);
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_Approval_Doc') CREATE INDEX IX_Approval_Doc ON core.Approval(CompanyCode, DocumentType, DocumentRef, Status);
GO
