/*  ap.SupplierStatement / ap.SupplierStatementLine / ap.PaymentPracticesReport - statement
    reconciliation and the half-yearly payment practices figures.  REQ: AP-019, AP-028, RPT-018 */
SET NOCOUNT ON;
IF OBJECT_ID(N'ap.SupplierStatement') IS NULL CREATE TABLE ap.SupplierStatement (
  StatementId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_SupplierStatement PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SupplierCode varchar(20) NOT NULL, StatementRef varchar(40) NOT NULL, StatementDate date NOT NULL, CurrencyCode char(3) NOT NULL,
  StatementBalance decimal(18,2) NOT NULL, LedgerBalance decimal(18,2) NULL, DifferenceAmount decimal(18,2) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_SStmt_St DEFAULT 'OPEN' CONSTRAINT CK_SStmt_St CHECK (Status IN ('OPEN','RECONCILED','DISPUTED')), ReconciledBy varchar(20) NULL, ReconciledAt datetime2(3) NULL, Note varchar(300) NULL,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_SStmt_CA DEFAULT SYSDATETIME(), RowVer rowversion, CONSTRAINT UQ_SupplierStatement UNIQUE (CompanyCode, SupplierCode, StatementRef));
GO
IF OBJECT_ID(N'ap.SupplierStatementLine') IS NULL CREATE TABLE ap.SupplierStatementLine (
  StatementLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_SupplierStatementLine PRIMARY KEY, StatementId int NOT NULL REFERENCES ap.SupplierStatement(StatementId), LineNum int NOT NULL, SupplierDocRef varchar(40) NOT NULL, DocumentDate date NULL,
  DueDate date NULL, Amount decimal(18,2) NOT NULL, MatchStatus varchar(14) NOT NULL CONSTRAINT DF_SStmtL_Ms DEFAULT 'UNMATCHED' CONSTRAINT CK_SStmtL_Ms CHECK (MatchStatus IN ('UNMATCHED','MATCHED','MISSINGOURS','MISSINGTHEIRS','TIMING','DISPUTED')),
  MatchedOpenItemId bigint NULL, Note varchar(200) NULL, CONSTRAINT UQ_SupplierStatementLine UNIQUE (StatementId, LineNum));
GO
IF OBJECT_ID(N'ap.PaymentPracticesReport') IS NULL CREATE TABLE ap.PaymentPracticesReport (
  ReportId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_PaymentPracticesReport PRIMARY KEY, CompanyCode varchar(10) NOT NULL, ReportingPeriodKey varchar(12) NOT NULL, PeriodStart date NOT NULL, PeriodEnd date NOT NULL, DueDate date NOT NULL,
  StandardTermsDays smallint NULL, MaximumTermsDays smallint NULL, TermsChangeNarrative varchar(400) NULL, AverageDaysToPay decimal(9,2) NULL, PctPaidWithin30 decimal(9,2) NULL, PctPaid31To60 decimal(9,2) NULL, PctPaid61Plus decimal(9,2) NULL,
  ValuePaidWithin30GBP decimal(18,2) NULL, ValuePaid31To60GBP decimal(18,2) NULL, ValuePaid61PlusGBP decimal(18,2) NULL, PctNotPaidWithinTerms decimal(9,2) NULL, ValueNotPaidWithinTermsGBP decimal(18,2) NULL, PctDisputed decimal(9,2) NULL,
  InvoiceCount int NULL, ExcludedInvoiceCount int NULL, ExclusionNarrative varchar(400) NULL, UsesSupplyChainFinance bit NOT NULL CONSTRAINT DF_Ppr_Scf DEFAULT 0, UsesEInvoicing bit NOT NULL CONSTRAINT DF_Ppr_Ei DEFAULT 0,
  Status varchar(12) NOT NULL CONSTRAINT DF_Ppr_St DEFAULT 'DRAFT' CONSTRAINT CK_Ppr_St CHECK (Status IN ('DRAFT','APPROVED','PUBLISHED')), Version smallint NOT NULL CONSTRAINT DF_Ppr_Ver DEFAULT 1, ApprovedByDirector varchar(20) NULL,
  ApprovedAt datetime2(3) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Ppr_CA DEFAULT SYSDATETIME(), RowVer rowversion, CONSTRAINT UQ_PaymentPracticesReport UNIQUE (CompanyCode, ReportingPeriodKey, Version));
GO
