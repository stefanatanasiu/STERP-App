/*  fin.Journal / fin.JournalLine / fin.JournalStage - the nominal ledger.  Posted money is never
    updated: corrections are linked reversals.  COBOL posts through the staging table because it
    cannot pass table-valued parameters.  REQ: FIN-004, FIN-005, FIN-006, COM-006, COM-014 */
SET NOCOUNT ON;
IF OBJECT_ID(N'fin.Journal') IS NULL CREATE TABLE fin.Journal (
  JournalId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_Journal PRIMARY KEY, CompanyCode varchar(10) NOT NULL, JournalNumber varchar(30) NOT NULL, JournalType varchar(12) NOT NULL CONSTRAINT DF_Jnl_Type DEFAULT 'GL',
  Description varchar(200) NOT NULL, SourceModule varchar(6) NOT NULL CONSTRAINT DF_Jnl_Src DEFAULT 'FIN', SourceRef varchar(40) NULL, EventCode varchar(30) NULL, DocumentDate date NOT NULL, PostingDate date NOT NULL, FiscalYear smallint NOT NULL,
  PeriodNo tinyint NOT NULL, CurrencyCode char(3) NOT NULL CONSTRAINT DF_Jnl_Cur DEFAULT 'GBP', FxRate decimal(18,8) NOT NULL CONSTRAINT DF_Jnl_Fx DEFAULT 1, FxRateDate date NULL, FxRateType varchar(6) NULL,
  TotalDebit decimal(18,2) NOT NULL CONSTRAINT DF_Jnl_TD DEFAULT 0, TotalCredit decimal(18,2) NOT NULL CONSTRAINT DF_Jnl_TC DEFAULT 0, TotalDebitGBP decimal(18,2) NOT NULL CONSTRAINT DF_Jnl_TDG DEFAULT 0,
  TotalCreditGBP decimal(18,2) NOT NULL CONSTRAINT DF_Jnl_TCG DEFAULT 0, Status varchar(12) NOT NULL CONSTRAINT DF_Jnl_St DEFAULT 'POSTED' CONSTRAINT CK_Jnl_St CHECK (Status IN ('DRAFT','SUBMITTED','APPROVED','POSTED','REJECTED','VOID','TEMPLATE')),
  ReversalOfJournalId bigint NULL, ReversedPct decimal(9,4) NULL, IsReversal bit NOT NULL CONSTRAINT DF_Jnl_Rev DEFAULT 0, ReversedAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_Jnl_RA DEFAULT 0, RecurringCode varchar(20) NULL, OccurrenceNo int NULL,
  PreparedBy varchar(20) NULL, ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL, PostedBy varchar(20) NULL, PostedAt datetime2(3) NULL, CorrelationId varchar(64) NULL, EvidenceRef varchar(120) NULL, RetentionClass varchar(16) NULL,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Jnl_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_Journal_Number UNIQUE (CompanyCode, JournalNumber));
GO
IF OBJECT_ID(N'fin.JournalLine') IS NULL CREATE TABLE fin.JournalLine (
  JournalLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_JournalLine PRIMARY KEY, JournalId bigint NOT NULL REFERENCES fin.Journal(JournalId), LineNum int NOT NULL, CompanyCode varchar(10) NOT NULL, AccountCode varchar(12) NOT NULL,
  DebitAmount decimal(18,2) NOT NULL CONSTRAINT DF_JnlL_D DEFAULT 0, CreditAmount decimal(18,2) NOT NULL CONSTRAINT DF_JnlL_C DEFAULT 0, DebitAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_JnlL_DG DEFAULT 0,
  CreditAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_JnlL_CG DEFAULT 0, CurrencyCode char(3) NOT NULL CONSTRAINT DF_JnlL_Cur DEFAULT 'GBP', DimCostCentre varchar(20) NULL, DimProject varchar(20) NULL, Narrative varchar(200) NULL,
  VatCode varchar(4) NULL, VatPeriodKey varchar(10) NULL, JobRef varchar(20) NULL, SubledgerType varchar(6) NULL, SubledgerRef varchar(40) NULL, SourceRef varchar(40) NULL, BankAccountCode varchar(20) NULL, ReconId int NULL,
  CONSTRAINT CK_JnlL_Side CHECK ((DebitAmount = 0 AND CreditAmount >= 0) OR (CreditAmount = 0 AND DebitAmount >= 0)), CONSTRAINT UQ_JournalLine UNIQUE (JournalId, LineNum));
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_JournalLine_Account') CREATE INDEX IX_JournalLine_Account ON fin.JournalLine(CompanyCode, AccountCode) INCLUDE (DebitAmountGBP, CreditAmountGBP);
GO
/*  Staging rows are keyed by CorrelationId; fin.usp_PostJournal consumes and deletes them. */
IF OBJECT_ID(N'fin.JournalStage') IS NULL CREATE TABLE fin.JournalStage (
  StageId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_JournalStage PRIMARY KEY, CorrelationId varchar(64) NOT NULL, LineNum int NOT NULL, CompanyCode varchar(10) NOT NULL, AccountCode varchar(12) NOT NULL,
  DebitAmount decimal(18,2) NOT NULL CONSTRAINT DF_JnlS_D DEFAULT 0, CreditAmount decimal(18,2) NOT NULL CONSTRAINT DF_JnlS_C DEFAULT 0, CurrencyCode char(3) NOT NULL CONSTRAINT DF_JnlS_Cur DEFAULT 'GBP', DebitAmountGBP decimal(18,2) NULL,
  CreditAmountGBP decimal(18,2) NULL, DimCostCentre varchar(20) NULL, DimProject varchar(20) NULL, Narrative varchar(200) NULL, VatCode varchar(4) NULL, JobRef varchar(20) NULL, SubledgerType varchar(6) NULL, SubledgerRef varchar(40) NULL,
  SourceRef varchar(40) NULL, BankAccountCode varchar(20) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_JnlS_CA DEFAULT SYSDATETIME(), CONSTRAINT UQ_JournalStage UNIQUE (CorrelationId, LineNum));
GO
