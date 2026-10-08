/*  rpt.MetricDefinition / rpt.ReconciliationRun - one published definition per shared measure,
    and control-account reconciliation results at a common cutoff.
    REQ: XMD-010, XMD-012, FIN-021, D-19 */
SET NOCOUNT ON;
IF OBJECT_ID(N'rpt.MetricDefinition') IS NULL CREATE TABLE rpt.MetricDefinition (
  MetricCode varchar(30) NOT NULL CONSTRAINT PK_MetricDefinition PRIMARY KEY, Name varchar(80) NOT NULL, Definition varchar(600) NOT NULL, NumeratorBasis varchar(200) NULL, DenominatorBasis varchar(200) NULL, CurrencyBasis varchar(40) NULL,
  CutoffBasis varchar(60) NULL, ZeroDenominatorRule varchar(100) NULL, DecisionRef varchar(10) NULL, OwnerRole varchar(20) NULL, IsActive bit NOT NULL CONSTRAINT DF_Met_Act DEFAULT 1, RowVer rowversion);
GO
IF OBJECT_ID(N'rpt.ReconciliationRun') IS NULL CREATE TABLE rpt.ReconciliationRun (
  ReconRunId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_ReconciliationRun PRIMARY KEY, CompanyCode varchar(10) NOT NULL, ControlType varchar(10) NOT NULL, AsOfDate date NOT NULL, FiscalYear smallint NULL, PeriodNo tinyint NULL,
  AccountCode varchar(12) NULL, ControlBalanceGBP decimal(18,2) NOT NULL, DetailBalanceGBP decimal(18,2) NOT NULL, DifferenceGBP decimal(18,2) NOT NULL, ContributingRefs varchar(600) NULL, OwnerUser varchar(20) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_Rec_St DEFAULT 'OPEN' CONSTRAINT CK_RecRun_St CHECK (Status IN ('OPEN','EXPLAINED','ACCEPTED','CORRECTED')), ReviewedBy varchar(20) NULL, ReviewedAt datetime2(3) NULL, Note varchar(400) NULL,
  RunAt datetime2(3) NOT NULL CONSTRAINT DF_RecRun_RA DEFAULT SYSDATETIME(), RunBy varchar(20) NULL, RowVer rowversion);
GO
