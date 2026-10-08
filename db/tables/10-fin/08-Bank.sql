/*  fin.BankAccount / fin.BankStatementLine / fin.BankRecon - cash book and reconciliation.
    Sort code and account number are restricted data (D-17).  REQ: FIN-013, FIN-014, FIN-015 */
SET NOCOUNT ON;
IF OBJECT_ID(N'fin.BankAccount') IS NULL CREATE TABLE fin.BankAccount (
  CompanyCode varchar(10) NOT NULL, BankAccountCode varchar(20) NOT NULL, Name varchar(80) NOT NULL, AccountCode varchar(12) NOT NULL, CurrencyCode char(3) NOT NULL CONSTRAINT DF_Bank_Cur DEFAULT 'GBP', BankName varchar(60) NULL,
  BranchName varchar(60) NULL, SortCode char(6) NULL, AccountNumber char(8) NULL, Iban varchar(34) NULL, Bic varchar(11) NULL, BacsServiceUserNumber varchar(6) NULL, SupportsBacs bit NOT NULL CONSTRAINT DF_Bank_Bacs DEFAULT 1,
  SupportsChaps bit NOT NULL CONSTRAINT DF_Bank_Chaps DEFAULT 1, SupportsFps bit NOT NULL CONSTRAINT DF_Bank_Fps DEFAULT 1, CalendarCode varchar(10) NOT NULL CONSTRAINT DF_Bank_Cal DEFAULT 'BACS', StatementBalance decimal(18,2) NULL,
  StatementDate date NULL, LastReconciledDate date NULL, Status varchar(10) NOT NULL CONSTRAINT DF_Bank_St DEFAULT 'ACTIVE', IsSensitive bit NOT NULL CONSTRAINT DF_Bank_Sens DEFAULT 1, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Bank_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_BankAccount PRIMARY KEY (CompanyCode, BankAccountCode));
GO
IF OBJECT_ID(N'fin.BankRecon') IS NULL CREATE TABLE fin.BankRecon (
  ReconId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_BankRecon PRIMARY KEY, CompanyCode varchar(10) NOT NULL, BankAccountCode varchar(20) NOT NULL, StatementRef varchar(40) NOT NULL, FromDate date NOT NULL, ToDate date NOT NULL,
  CurrencyCode char(3) NOT NULL, OpeningStatementBalance decimal(18,2) NOT NULL, ClosingStatementBalance decimal(18,2) NOT NULL, BookBalance decimal(18,2) NULL, OutstandingReceipts decimal(18,2) NULL, OutstandingPayments decimal(18,2) NULL,
  UnexplainedDifference decimal(18,2) NULL, Status varchar(12) NOT NULL CONSTRAINT DF_Recon_St DEFAULT 'OPEN' CONSTRAINT CK_Recon_St CHECK (Status IN ('OPEN','COMPLETE','REOPENED')), CompletedBy varchar(20) NULL, CompletedAt datetime2(3) NULL,
  ReopenedBy varchar(20) NULL, ReopenReason varchar(200) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Recon_CA DEFAULT SYSDATETIME(), RowVer rowversion);
GO
IF OBJECT_ID(N'fin.BankStatementLine') IS NULL CREATE TABLE fin.BankStatementLine (
  StatementLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_BankStatementLine PRIMARY KEY, CompanyCode varchar(10) NOT NULL, BankAccountCode varchar(20) NOT NULL, StatementRef varchar(40) NOT NULL, LineNum int NOT NULL, EntryDate date NOT NULL,
  ValueDate date NULL, Description varchar(200) NULL, BankReference varchar(60) NULL, Amount decimal(18,2) NOT NULL, CurrencyCode char(3) NOT NULL,
  EntryType varchar(10) NOT NULL CONSTRAINT CK_BSL_Type CHECK (EntryType IN ('RECEIPT','PAYMENT','CHARGE','INTEREST','TRANSFER','OTHER')),
  MatchStatus varchar(12) NOT NULL CONSTRAINT DF_BSL_Ms DEFAULT 'UNMATCHED' CONSTRAINT CK_BSL_Ms CHECK (MatchStatus IN ('UNMATCHED','MATCHED','PARTIAL','EXCLUDED')), MatchedAmount decimal(18,2) NOT NULL CONSTRAINT DF_BSL_Ma DEFAULT 0,
  ReconId int NULL REFERENCES fin.BankRecon(ReconId), MatchedJournalLineId bigint NULL, MatchedAt datetime2(3) NULL, MatchedBy varchar(20) NULL, RowVer rowversion,
  CONSTRAINT UQ_BankStatementLine UNIQUE (CompanyCode, BankAccountCode, StatementRef, LineNum));
GO
