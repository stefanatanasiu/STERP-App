/*  fin.VatTransaction / fin.VatReturn / fin.VatErrorCorrection - the digital VAT records that
    feed the nine-box MTD return, with late-entry and error-correction handling.
    REQ: FIN-017, FIN-018, FIN-025, FIN-026, FIN-027, FIN-029 */
SET NOCOUNT ON;
IF OBJECT_ID(N'fin.VatTransaction') IS NULL CREATE TABLE fin.VatTransaction (
  VatTxnId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_VatTransaction PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SourceModule varchar(6) NOT NULL, DocumentType varchar(20) NOT NULL, DocumentRef varchar(40) NOT NULL, LineNum int NULL,
  PartyType varchar(10) NULL, PartyCode varchar(20) NULL, PartyVatRegNo varchar(15) NULL, TaxPointDate date NOT NULL, PostingDate date NOT NULL, VatPeriodKey varchar(10) NULL, JournalId bigint NULL, VatCode varchar(4) NOT NULL,
  RatePct decimal(9,4) NOT NULL, Mechanism varchar(12) NOT NULL CONSTRAINT DF_VatT_Mech DEFAULT 'NORMAL', Direction varchar(6) NOT NULL CONSTRAINT CK_VatT_Dir CHECK (Direction IN ('OUTPUT','INPUT')),
  CurrencyCode char(3) NOT NULL CONSTRAINT DF_VatT_Cur DEFAULT 'GBP', FxRate decimal(18,8) NOT NULL CONSTRAINT DF_VatT_Fx DEFAULT 1, NetAmount decimal(18,2) NOT NULL, VatAmount decimal(18,2) NOT NULL, NetAmountGBP decimal(18,2) NOT NULL,
  VatAmountGBP decimal(18,2) NOT NULL, RecoverableVatGBP decimal(18,2) NOT NULL CONSTRAINT DF_VatT_Rec DEFAULT 0, VatBox tinyint NULL, NetBox tinyint NULL, PlaceOfSupply varchar(20) NULL, EvidenceRef varchar(60) NULL,
  IsLateEntry bit NOT NULL CONSTRAINT DF_VatT_Late DEFAULT 0, AdjustmentType varchar(16) NULL, ReversalOfVatTxnId bigint NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_VatT_CA DEFAULT SYSDATETIME(), RowVer rowversion);
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_VatTransaction_Period') CREATE INDEX IX_VatTransaction_Period ON fin.VatTransaction(CompanyCode, VatPeriodKey, Direction);
GO
IF OBJECT_ID(N'fin.VatReturn') IS NULL CREATE TABLE fin.VatReturn (
  VatReturnId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_VatReturn PRIMARY KEY, CompanyCode varchar(10) NOT NULL, VatPeriodKey varchar(10) NOT NULL, Version smallint NOT NULL CONSTRAINT DF_VatRet_Ver DEFAULT 1, VatRegNo varchar(15) NULL,
  Box1 decimal(18,2) NOT NULL CONSTRAINT DF_VatRet_B1 DEFAULT 0, Box2 decimal(18,2) NOT NULL CONSTRAINT DF_VatRet_B2 DEFAULT 0, Box3 decimal(18,2) NOT NULL CONSTRAINT DF_VatRet_B3 DEFAULT 0,
  Box4 decimal(18,2) NOT NULL CONSTRAINT DF_VatRet_B4 DEFAULT 0, Box5 decimal(18,2) NOT NULL CONSTRAINT DF_VatRet_B5 DEFAULT 0, Box6 decimal(18,0) NOT NULL CONSTRAINT DF_VatRet_B6 DEFAULT 0,
  Box7 decimal(18,0) NOT NULL CONSTRAINT DF_VatRet_B7 DEFAULT 0, Box8 decimal(18,0) NOT NULL CONSTRAINT DF_VatRet_B8 DEFAULT 0, Box9 decimal(18,0) NOT NULL CONSTRAINT DF_VatRet_B9 DEFAULT 0,
  Status varchar(14) NOT NULL CONSTRAINT DF_VatRet_St DEFAULT 'DRAFT' CONSTRAINT CK_VatRet_St CHECK (Status IN ('DRAFT','FINALISED','SUBMITTED','AMENDED')), PreparedBy varchar(20) NULL, PreparedAt datetime2(3) NULL, ApprovedBy varchar(20) NULL,
  ApprovedAt datetime2(3) NULL, SubmittedAt datetime2(3) NULL, SubmissionRef varchar(60) NULL, HmrcResponse varchar(300) NULL, PaymentJournalId bigint NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_VatRet_CA DEFAULT SYSDATETIME(),
  RowVer rowversion, CONSTRAINT UQ_VatReturn UNIQUE (CompanyCode, VatPeriodKey, Version));
GO
IF OBJECT_ID(N'fin.VatErrorCorrection') IS NULL CREATE TABLE fin.VatErrorCorrection (
  ErrorId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_VatErrorCorrection PRIMARY KEY, CompanyCode varchar(10) NOT NULL, OriginalVatPeriodKey varchar(10) NOT NULL, CorrectionVatPeriodKey varchar(10) NULL, DiscoveredDate date NOT NULL,
  NetErrorAmount decimal(18,2) NOT NULL, Box6Turnover decimal(18,2) NULL, IsDeliberate bit NOT NULL CONSTRAINT DF_VatErr_Del DEFAULT 0, Method varchar(14) NOT NULL CONSTRAINT CK_VatErr_M CHECK (Method IN ('NEXTRETURN','DISCLOSURE')),
  Reason varchar(300) NOT NULL, SourceDocumentRefs varchar(300) NULL, DisclosureRef varchar(60) NULL, PreparedBy varchar(20) NULL, ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_VatErr_St DEFAULT 'OPEN', CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_VatErr_CA DEFAULT SYSDATETIME(), RowVer rowversion);
GO
