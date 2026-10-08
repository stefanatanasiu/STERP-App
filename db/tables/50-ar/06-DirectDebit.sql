/*  ar.DirectDebitMandate / ar.DirectDebitCollection - Bacs Direct Debit scheme: lodgement,
    advance notice, collection, ARUDD unpaids and DDICA indemnity claims.  REQ: AR-025, D-16 */
SET NOCOUNT ON;
IF OBJECT_ID(N'ar.DirectDebitMandate') IS NULL CREATE TABLE ar.DirectDebitMandate (
  MandateId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_DirectDebitMandate PRIMARY KEY, CompanyCode varchar(10) NOT NULL, CustomerCode varchar(20) NOT NULL, MandateReference varchar(18) NOT NULL, ServiceUserNumber varchar(6) NOT NULL,
  PayerName varchar(120) NOT NULL, SortCode char(6) NOT NULL, AccountNumber char(8) NOT NULL, RollNumber varchar(18) NULL, SignedDate date NOT NULL, LodgedDate date NULL, FirstCollectionDate date NULL,
  AdvanceNoticeDays smallint NOT NULL CONSTRAINT DF_DDM_Adv DEFAULT 10, AdvanceNoticeAgreedInWriting bit NOT NULL CONSTRAINT DF_DDM_Awr DEFAULT 0,
  Status varchar(12) NOT NULL CONSTRAINT DF_DDM_St DEFAULT 'PENDING' CONSTRAINT CK_DDM_St CHECK (Status IN ('PENDING','LODGED','ACTIVE','CANCELLED','EXPIRED')), CancelledDate date NULL, CancelReason varchar(200) NULL,
  GuaranteeIssuedAt datetime2(3) NULL, IsSensitive bit NOT NULL CONSTRAINT DF_DDM_Sens DEFAULT 1, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_DDM_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL,
  ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_DirectDebitMandate UNIQUE (CompanyCode, MandateReference));
GO
IF OBJECT_ID(N'ar.DirectDebitCollection') IS NULL CREATE TABLE ar.DirectDebitCollection (
  CollectionId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_DirectDebitCollection PRIMARY KEY, MandateId int NOT NULL REFERENCES ar.DirectDebitMandate(MandateId), CompanyCode varchar(10) NOT NULL, CustomerCode varchar(20) NOT NULL,
  CollectionRef varchar(30) NOT NULL, AdvanceNoticeSentAt date NULL, SubmissionDate date NULL, ProcessingDate date NULL, DueDate date NOT NULL, AmountGBP decimal(18,2) NOT NULL, TransactionCode varchar(4) NULL, BacsFileRef varchar(60) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_DDC_St DEFAULT 'PLANNED' CONSTRAINT CK_DDC_St CHECK (Status IN ('PLANNED','SUBMITTED','COLLECTED','UNPAID','INDEMNITY','CANCELLED')), UnpaidReasonCode varchar(4) NULL, UnpaidDate date NULL,
  IndemnityClaimRef varchar(40) NULL, IndemnityDate date NULL, ReceiptId bigint NULL, OpenItemIds varchar(300) NULL, CorrelationId varchar(64) NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_DDC_CA DEFAULT SYSDATETIME(), RowVer rowversion, CONSTRAINT UQ_DirectDebitCollection UNIQUE (CompanyCode, CollectionRef));
GO
