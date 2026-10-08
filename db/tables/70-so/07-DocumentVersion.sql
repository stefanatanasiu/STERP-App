/*  so.DocumentVersion - the versioned, historically reproducible record of what the customer was
    sent and what the customer accepted: quotes, acknowledgements, pick notes, despatch notes and
    invoices.  A reissue reproduces a held version exactly; it changes no business state, so a
    reprint only increments ReprintCount and leaves its own audit trail.  REQ: SO-022 */
SET NOCOUNT ON;
IF OBJECT_ID(N'so.DocumentVersion') IS NULL CREATE TABLE so.DocumentVersion (
  DocVersionId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_DocumentVersion PRIMARY KEY, CompanyCode varchar(10) NOT NULL, DocumentType varchar(20) NOT NULL, DocumentRef varchar(30) NOT NULL, VersionNo smallint NOT NULL,
  IssuedAt datetime2(3) NOT NULL CONSTRAINT DF_DocV_IA DEFAULT SYSDATETIME(), IssuedBy varchar(20) NULL, ContentHash varchar(64) NULL, Snapshot varchar(max) NULL,
  ReprintCount int NOT NULL CONSTRAINT DF_DocV_Rp DEFAULT 0, ChangeSummary varchar(400) NULL,
  CONSTRAINT UQ_DocumentVersion UNIQUE (CompanyCode, DocumentType, DocumentRef, VersionNo));
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_DocumentVersion_Ref') CREATE INDEX IX_DocumentVersion_Ref ON so.DocumentVersion(CompanyCode, DocumentRef, DocumentType, VersionNo);
GO
