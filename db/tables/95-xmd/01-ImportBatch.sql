/*  stg.ImportBatch / stg.ImportRow - business bulk imports with preview, duplicate check and a
    per-document accept or reject outcome.  REQ: COM-018, XMD-007 */
SET NOCOUNT ON;
IF OBJECT_ID(N'stg.ImportBatch') IS NULL CREATE TABLE stg.ImportBatch (
  BatchId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_ImportBatch PRIMARY KEY, CompanyCode varchar(10) NOT NULL, BatchRef varchar(40) NOT NULL, ImportType varchar(30) NOT NULL, SourceFile varchar(260) NULL, SubmittedBy varchar(20) NULL,
  SubmittedAt datetime2(3) NOT NULL CONSTRAINT DF_ImpB_SA DEFAULT SYSDATETIME(), Mode varchar(8) NOT NULL CONSTRAINT DF_ImpB_Md DEFAULT 'PREVIEW' CONSTRAINT CK_ImpB_Md CHECK (Mode IN ('PREVIEW','COMMIT')),
  RowCountTotal int NOT NULL CONSTRAINT DF_ImpB_Rt DEFAULT 0, RowCountAccepted int NOT NULL CONSTRAINT DF_ImpB_Ra DEFAULT 0, RowCountRejected int NOT NULL CONSTRAINT DF_ImpB_Rr DEFAULT 0,
  RowCountDuplicate int NOT NULL CONSTRAINT DF_ImpB_Rd DEFAULT 0, Status varchar(12) NOT NULL CONSTRAINT DF_ImpB_St DEFAULT 'OPEN' CONSTRAINT CK_ImpB_St CHECK (Status IN ('OPEN','VALIDATED','COMMITTED','PARTIAL','FAILED')),
  CorrelationId varchar(64) NULL, CompletedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_ImportBatch UNIQUE (CompanyCode, BatchRef));
GO
IF OBJECT_ID(N'stg.ImportRow') IS NULL CREATE TABLE stg.ImportRow (
  ImportRowId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_ImportRow PRIMARY KEY, BatchId int NOT NULL REFERENCES stg.ImportBatch(BatchId), RowNum int NOT NULL, NaturalKey varchar(120) NULL, RawData varchar(4000) NULL,
  ValidationOutcome varchar(12) NOT NULL CONSTRAINT DF_ImpR_Vo DEFAULT 'PENDING' CONSTRAINT CK_ImpR_Vo CHECK (ValidationOutcome IN ('PENDING','VALID','INVALID','DUPLICATE')), ValidationMessage varchar(400) NULL, CreatedDocumentType varchar(20) NULL,
  CreatedDocumentRef varchar(40) NULL, ProcessedAt datetime2(3) NULL, ProcessedBy varchar(20) NULL, CONSTRAINT UQ_ImportRow UNIQUE (BatchId, RowNum));
GO
