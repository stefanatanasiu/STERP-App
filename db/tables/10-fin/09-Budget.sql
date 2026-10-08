/*  fin.Budget / fin.BudgetLine / fin.RecurringJournal - approved budget versions, recurring
    journals, accruals and prepayments.  REQ: FIN-007, FIN-008, FIN-009, D-08 */
SET NOCOUNT ON;
IF OBJECT_ID(N'fin.Budget') IS NULL CREATE TABLE fin.Budget (
  CompanyCode varchar(10) NOT NULL, BudgetCode varchar(20) NOT NULL, FiscalYear smallint NOT NULL, Name varchar(80) NOT NULL,
  BudgetType varchar(12) NOT NULL CONSTRAINT DF_Bud_Type DEFAULT 'BUDGET' CONSTRAINT CK_Bud_Type CHECK (BudgetType IN ('BUDGET','FORECAST')), Version smallint NOT NULL CONSTRAINT DF_Bud_Ver DEFAULT 1,
  CurrencyCode char(3) NOT NULL CONSTRAINT DF_Bud_Cur DEFAULT 'GBP', Status varchar(10) NOT NULL CONSTRAINT DF_Bud_St DEFAULT 'DRAFT' CONSTRAINT CK_Bud_St CHECK (Status IN ('DRAFT','APPROVED','SUPERSEDED')),
  IsBaseline bit NOT NULL CONSTRAINT DF_Bud_Base DEFAULT 0, OwnerUser varchar(20) NULL, ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Bud_CA DEFAULT SYSDATETIME(),
  ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_Budget PRIMARY KEY (CompanyCode, BudgetCode, FiscalYear, Version));
GO
IF OBJECT_ID(N'fin.BudgetLine') IS NULL CREATE TABLE fin.BudgetLine (
  BudgetLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_BudgetLine PRIMARY KEY, CompanyCode varchar(10) NOT NULL, BudgetCode varchar(20) NOT NULL, FiscalYear smallint NOT NULL, Version smallint NOT NULL, PeriodNo tinyint NOT NULL,
  AccountCode varchar(12) NOT NULL, DimCostCentre varchar(20) NULL, DimProject varchar(20) NULL, Amount decimal(18,2) NOT NULL CONSTRAINT DF_BudL_Amt DEFAULT 0, Narrative varchar(200) NULL,
  CONSTRAINT UQ_BudgetLine UNIQUE (CompanyCode, BudgetCode, FiscalYear, Version, PeriodNo, AccountCode, DimCostCentre, DimProject));
GO
IF OBJECT_ID(N'fin.RecurringJournal') IS NULL CREATE TABLE fin.RecurringJournal (
  CompanyCode varchar(10) NOT NULL, RecurringCode varchar(20) NOT NULL, Name varchar(80) NOT NULL,
  JournalKind varchar(10) NOT NULL CONSTRAINT DF_Rec_Kind DEFAULT 'RECURRING' CONSTRAINT CK_Rec_Kind CHECK (JournalKind IN ('RECURRING','ACCRUAL','PREPAY')), TemplateJournalId bigint NULL,
  Frequency varchar(10) NOT NULL CONSTRAINT CK_Rec_Fr CHECK (Frequency IN ('MONTHLY','QUARTERLY','ANNUAL')), StartDate date NOT NULL, EndDate date NULL, NextDueDate date NULL, OccurrencesPlanned int NULL,
  OccurrencesPosted int NOT NULL CONSTRAINT DF_Rec_OP DEFAULT 0, SourceAmount decimal(18,2) NULL, ReleasedAmount decimal(18,2) NOT NULL CONSTRAINT DF_Rec_RA DEFAULT 0, RemainingAmount decimal(18,2) NULL,
  RequiresApproval bit NOT NULL CONSTRAINT DF_Rec_Appr DEFAULT 1, ApprovedBy varchar(20) NULL, Status varchar(10) NOT NULL CONSTRAINT DF_Rec_St DEFAULT 'ACTIVE' CONSTRAINT CK_Rec_St CHECK (Status IN ('ACTIVE','HELD','ENDED')),
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Rec_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT PK_RecurringJournal PRIMARY KEY (CompanyCode, RecurringCode));
GO
