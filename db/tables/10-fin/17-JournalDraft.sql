/*  fin.JournalDraft - the header of a manual journal while it is being prepared, submitted,
    approved or rejected.  Its lines live in fin.JournalStage under the same CorrelationId, so
    posting is simply a call to fin.usp_PostJournal with that correlation id and nothing has to
    be copied.  A draft has no ledger effect whatever (FIN-004, COM-006); a material edit after
    submission returns it to DRAFT and invalidates any approval (COM-008).
    REQ: FIN-004, FIN-005 */
SET NOCOUNT ON;
IF OBJECT_ID(N'fin.JournalDraft') IS NULL CREATE TABLE fin.JournalDraft (
  DraftId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_JournalDraft PRIMARY KEY, CompanyCode varchar(10) NOT NULL, CorrelationId varchar(64) NOT NULL, JournalRef varchar(30) NULL,
  JournalType varchar(12) NOT NULL CONSTRAINT DF_JnlD_Type DEFAULT 'GL', Description varchar(200) NOT NULL, SourceModule varchar(6) NOT NULL CONSTRAINT DF_JnlD_Src DEFAULT 'FIN', SourceRef varchar(40) NULL,
  EventCode varchar(30) NULL, DocumentDate date NOT NULL, PostingDate date NOT NULL, CurrencyCode char(3) NOT NULL CONSTRAINT DF_JnlD_Cur DEFAULT 'GBP', FxRate decimal(18,8) NULL,
  TotalDebit decimal(18,2) NOT NULL CONSTRAINT DF_JnlD_TD DEFAULT 0, TotalCredit decimal(18,2) NOT NULL CONSTRAINT DF_JnlD_TC DEFAULT 0, EvidenceRef varchar(120) NULL, LineCount int NOT NULL CONSTRAINT DF_JnlD_LC DEFAULT 0,
  Status varchar(12) NOT NULL CONSTRAINT DF_JnlD_St DEFAULT 'DRAFT' CONSTRAINT CK_JnlD_St CHECK (Status IN ('DRAFT','SUBMITTED','APPROVED','REJECTED','POSTED','VOID')),
  PreparedBy varchar(20) NULL, SubmittedBy varchar(20) NULL, SubmittedAt datetime2(3) NULL, ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL, RejectedBy varchar(20) NULL, RejectReason varchar(200) NULL,
  PostedJournalNo varchar(30) NULL, ContentHash varchar(40) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_JnlD_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL,
  ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_JournalDraft UNIQUE (CorrelationId));
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_JournalDraft_Status') CREATE INDEX IX_JournalDraft_Status ON fin.JournalDraft(CompanyCode, Status) INCLUDE (PostingDate, Description);
GO
