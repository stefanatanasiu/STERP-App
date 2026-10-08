/*  fin.BankReconMatch / fin.BankReconMatchLine - many-to-many bank reconciliation matching.
    One match group ties n statement lines to m book (journal) lines; the group only completes
    when both sides agree.  A member can belong to one open group only, so a transaction cannot
    be reconciled twice (FIN-014, AC-014).  REQ: FIN-013, FIN-014, FIN-015 */
SET NOCOUNT ON;
IF OBJECT_ID(N'fin.BankReconMatch') IS NULL CREATE TABLE fin.BankReconMatch (
  MatchId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_BankReconMatch PRIMARY KEY, CompanyCode varchar(10) NOT NULL, BankAccountCode varchar(20) NOT NULL, ReconId int NULL REFERENCES fin.BankRecon(ReconId),
  MatchRef varchar(30) NOT NULL, MatchType varchar(12) NOT NULL CONSTRAINT DF_BRM_Type DEFAULT 'EXACT' CONSTRAINT CK_BRM_Type CHECK (MatchType IN ('EXACT','GROUPED','PARTIAL','EXPLAINED')),
  StatementTotal decimal(18,2) NOT NULL CONSTRAINT DF_BRM_Stmt DEFAULT 0, BookTotal decimal(18,2) NOT NULL CONSTRAINT DF_BRM_BT DEFAULT 0, DifferenceAmount decimal(18,2) NOT NULL CONSTRAINT DF_BRM_DF DEFAULT 0,
  CurrencyCode char(3) NOT NULL CONSTRAINT DF_BRM_Cur DEFAULT 'GBP', MatchDate date NOT NULL, Explanation varchar(200) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_BRM_St DEFAULT 'MATCHED' CONSTRAINT CK_BRM_St CHECK (Status IN ('MATCHED','REVERSED')), ReversedBy varchar(20) NULL, ReversedAt datetime2(3) NULL, ReverseReason varchar(200) NULL,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_BRM_CA DEFAULT SYSDATETIME(), RowVer rowversion, CONSTRAINT UQ_BankReconMatch UNIQUE (CompanyCode, BankAccountCode, MatchRef));
GO
IF OBJECT_ID(N'fin.BankReconMatchLine') IS NULL CREATE TABLE fin.BankReconMatchLine (
  MatchLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_BankReconMatchLine PRIMARY KEY, MatchId int NOT NULL REFERENCES fin.BankReconMatch(MatchId), CompanyCode varchar(10) NOT NULL,
  SideType varchar(10) NOT NULL CONSTRAINT CK_BRML_Side CHECK (SideType IN ('STATEMENT','BOOK')), StatementLineId bigint NULL, JournalLineId bigint NULL, Amount decimal(18,2) NOT NULL,
  CurrencyCode char(3) NOT NULL CONSTRAINT DF_BRML_Cur DEFAULT 'GBP', EntryDate date NULL, Narrative varchar(200) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_BRML_CA DEFAULT SYSDATETIME());
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_BankReconMatchLine_Stmt') CREATE INDEX IX_BankReconMatchLine_Stmt ON fin.BankReconMatchLine(CompanyCode, StatementLineId);
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_BankReconMatchLine_Book') CREATE INDEX IX_BankReconMatchLine_Book ON fin.BankReconMatchLine(CompanyCode, JournalLineId);
GO
/*  The book side of the cash book needs a reconciliation state of its own: a journal line that
    touches a bank account is outstanding until a completed match group includes it. */
IF COL_LENGTH('fin.JournalLine', 'ReconMatchId') IS NULL ALTER TABLE fin.JournalLine ADD ReconMatchId int NULL;
GO
