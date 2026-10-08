/*  fin.FiscalYear / fin.Period / fin.VatPeriod - financial calendar (April-March) and the
    independently maintained VAT return calendar.  REQ: COM-009, FIN-010, FIN-011, FIN-018, D-15 */
SET NOCOUNT ON;
IF OBJECT_ID(N'fin.FiscalYear') IS NULL CREATE TABLE fin.FiscalYear (
  CompanyCode varchar(10) NOT NULL REFERENCES core.Company(CompanyCode), FiscalYear smallint NOT NULL, StartDate date NOT NULL, EndDate date NOT NULL, PeriodCount tinyint NOT NULL CONSTRAINT DF_FY_Cnt DEFAULT 12,
  Status varchar(8) NOT NULL CONSTRAINT DF_FY_St DEFAULT 'OPEN' CONSTRAINT CK_FY_St CHECK (Status IN ('FUTURE','OPEN','CLOSED')), YearEndJournalNo varchar(30) NULL, ClosedBy varchar(20) NULL, ClosedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT PK_FiscalYear PRIMARY KEY (CompanyCode, FiscalYear));
GO
IF OBJECT_ID(N'fin.Period') IS NULL CREATE TABLE fin.Period (
  CompanyCode varchar(10) NOT NULL, FiscalYear smallint NOT NULL, PeriodNo tinyint NOT NULL, StartDate date NOT NULL, EndDate date NOT NULL, Name varchar(20) NOT NULL,
  Status varchar(8) NOT NULL CONSTRAINT DF_Per_St DEFAULT 'FUTURE' CONSTRAINT CK_Per_St CHECK (Status IN ('FUTURE','OPEN','SOFT','HARD')), SoftCloseAt datetime2(3) NULL, HardCloseAt datetime2(3) NULL, ClosedBy varchar(20) NULL,
  ReopenedBy varchar(20) NULL, ReopenedAt datetime2(3) NULL, ReopenReason varchar(200) NULL, RowVer rowversion, CONSTRAINT PK_Period PRIMARY KEY (CompanyCode, FiscalYear, PeriodNo));
GO
IF OBJECT_ID(N'fin.VatPeriod') IS NULL CREATE TABLE fin.VatPeriod (
  CompanyCode varchar(10) NOT NULL, VatPeriodKey varchar(10) NOT NULL, VatRegNo varchar(15) NULL, StartDate date NOT NULL, EndDate date NOT NULL, FilingDueDate date NOT NULL, PaymentDueDate date NOT NULL,
  Frequency varchar(10) NOT NULL CONSTRAINT DF_VatPer_Fr DEFAULT 'QUARTERLY' CONSTRAINT CK_VatPer_Fr CHECK (Frequency IN ('QUARTERLY','MONTHLY','ANNUAL')),
  Status varchar(14) NOT NULL CONSTRAINT DF_VatPer_St DEFAULT 'OPEN' CONSTRAINT CK_VatPer_St CHECK (Status IN ('OPEN','PREPARING','FINALISED','SUBMITTED','AMENDED')), SubmissionRef varchar(60) NULL, FinalisedBy varchar(20) NULL,
  FinalisedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_VatPeriod PRIMARY KEY (CompanyCode, VatPeriodKey));
GO
