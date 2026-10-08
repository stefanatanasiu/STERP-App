/*  cst.Job / cst.JobBudget / cst.JobBudgetLine / cst.JobCostTransaction / cst.WipBalance /
    cst.Variance - two-level job costing (job and sub-job), budgets, actuals, WIP and variances.
    REQ: CST-014..CST-020, D-13 */
SET NOCOUNT ON;
IF OBJECT_ID(N'cst.Job') IS NULL CREATE TABLE cst.Job (
  CompanyCode varchar(10) NOT NULL, JobRef varchar(20) NOT NULL, ParentJobRef varchar(20) NULL, JobLevel tinyint NOT NULL CONSTRAINT DF_Job_Lv DEFAULT 1 CONSTRAINT CK_Job_Lv CHECK (JobLevel IN (1,2)), Description varchar(150) NOT NULL,
  SiteCode varchar(10) NULL, CustomerCode varchar(20) NULL, SalesOrderNumber varchar(30) NULL, Purpose varchar(12) NOT NULL CONSTRAINT DF_Job_Pur DEFAULT 'INTERNAL' CONSTRAINT CK_Job_Pur CHECK (Purpose IN ('CUSTOMER','INTERNAL','RECTIFY','CAPITAL')),
  OwnerUser varchar(20) NULL, CostCentre varchar(20) NULL, ProjectCode varchar(20) NULL, StartDate date NULL, TargetEndDate date NULL, ActualEndDate date NULL,
  OperationalStatus varchar(14) NOT NULL CONSTRAINT DF_Job_Os DEFAULT 'OPEN' CONSTRAINT CK_Job_Os CHECK (OperationalStatus IN ('PLANNED','OPEN','COMPLETE','CANCELLED')),
  FinancialStatus varchar(14) NOT NULL CONSTRAINT DF_Job_Fs DEFAULT 'OPEN' CONSTRAINT CK_Job_Fs CHECK (FinancialStatus IN ('OPEN','REVIEW','CLOSED','REOPENED')), ClosedBy varchar(20) NULL, ClosedAt datetime2(3) NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Job_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_Job PRIMARY KEY (CompanyCode, JobRef));
GO
IF OBJECT_ID(N'cst.JobBudget') IS NULL CREATE TABLE cst.JobBudget (
  JobBudgetId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_JobBudget PRIMARY KEY, CompanyCode varchar(10) NOT NULL, JobRef varchar(20) NOT NULL, Version smallint NOT NULL, IsOriginal bit NOT NULL CONSTRAINT DF_JobB_Orig DEFAULT 0,
  TotalBudgetGBP decimal(18,2) NOT NULL CONSTRAINT DF_JobB_T DEFAULT 0, ChangeReason varchar(300) NULL, Status varchar(10) NOT NULL CONSTRAINT DF_JobB_St DEFAULT 'DRAFT' CONSTRAINT CK_JobB_St CHECK (Status IN ('DRAFT','APPROVED','SUPERSEDED')),
  ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_JobB_CA DEFAULT SYSDATETIME(), RowVer rowversion, CONSTRAINT UQ_JobBudget UNIQUE (CompanyCode, JobRef, Version));
GO
IF OBJECT_ID(N'cst.JobBudgetLine') IS NULL CREATE TABLE cst.JobBudgetLine (
  JobBudgetLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_JobBudgetLine PRIMARY KEY, JobBudgetId int NOT NULL REFERENCES cst.JobBudget(JobBudgetId), LineNum int NOT NULL, ElementCode varchar(10) NOT NULL, PeriodNo tinyint NULL,
  FiscalYear smallint NULL, BudgetQty decimal(19,6) NULL, BudgetAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_JobBL_A DEFAULT 0, Narrative varchar(200) NULL, CONSTRAINT UQ_JobBudgetLine UNIQUE (JobBudgetId, LineNum));
GO
IF OBJECT_ID(N'cst.JobCostTransaction') IS NULL CREATE TABLE cst.JobCostTransaction (
  JobCostId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_JobCostTransaction PRIMARY KEY, CompanyCode varchar(10) NOT NULL, JobRef varchar(20) NOT NULL, KitRef varchar(30) NULL, ElementCode varchar(10) NOT NULL, SourceModule varchar(6) NOT NULL,
  SourceDocType varchar(20) NULL, SourceDocRef varchar(40) NULL, ItemCode varchar(40) NULL, WorkCentreCode varchar(20) NULL, Quantity decimal(19,6) NULL, Hours decimal(19,6) NULL, Rate decimal(19,6) NULL,
  CurrencyCode char(3) NOT NULL CONSTRAINT DF_JobC_Cur DEFAULT 'GBP', Amount decimal(18,2) NOT NULL, AmountGBP decimal(18,2) NOT NULL,
  CostNature varchar(10) NOT NULL CONSTRAINT DF_JobC_Nat DEFAULT 'ACTUAL' CONSTRAINT CK_JobC_Nat CHECK (CostNature IN ('ACTUAL','ACCRUED','COMMITTED','ESTIMATE')), RecognitionDate date NOT NULL, FiscalYear smallint NULL, PeriodNo tinyint NULL,
  IsApproved bit NOT NULL CONSTRAINT DF_JobC_Ap DEFAULT 1, ApprovedBy varchar(20) NULL, JournalId bigint NULL, ReversalOfJobCostId bigint NULL, Narrative varchar(200) NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_JobC_CA DEFAULT SYSDATETIME());
GO
IF OBJECT_ID(N'cst.WipBalance') IS NULL CREATE TABLE cst.WipBalance (
  WipId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_WipBalance PRIMARY KEY, CompanyCode varchar(10) NOT NULL, JobRef varchar(20) NULL, KitRef varchar(30) NULL, FiscalYear smallint NOT NULL, PeriodNo tinyint NOT NULL,
  OpeningValueGBP decimal(18,2) NOT NULL CONSTRAINT DF_Wip_O DEFAULT 0, MaterialIssuesGBP decimal(18,2) NOT NULL CONSTRAINT DF_Wip_Mi DEFAULT 0, MaterialReturnsGBP decimal(18,2) NOT NULL CONSTRAINT DF_Wip_Mr DEFAULT 0,
  LabourGBP decimal(18,2) NOT NULL CONSTRAINT DF_Wip_L DEFAULT 0, OverheadGBP decimal(18,2) NOT NULL CONSTRAINT DF_Wip_Ov DEFAULT 0, SubcontractGBP decimal(18,2) NOT NULL CONSTRAINT DF_Wip_Sc DEFAULT 0,
  CompletionTransfersGBP decimal(18,2) NOT NULL CONSTRAINT DF_Wip_Ct DEFAULT 0, ScrapGBP decimal(18,2) NOT NULL CONSTRAINT DF_Wip_Sp DEFAULT 0, WriteOffGBP decimal(18,2) NOT NULL CONSTRAINT DF_Wip_Wo DEFAULT 0,
  ValueAdjustmentGBP decimal(18,2) NOT NULL CONSTRAINT DF_Wip_Va DEFAULT 0, ClosingValueGBP decimal(18,2) NOT NULL CONSTRAINT DF_Wip_C DEFAULT 0, LastUpdatedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT UQ_WipBalance UNIQUE (CompanyCode, JobRef, KitRef, FiscalYear, PeriodNo));
GO
IF OBJECT_ID(N'cst.Variance') IS NULL CREATE TABLE cst.Variance (
  VarianceId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_Variance PRIMARY KEY, CompanyCode varchar(10) NOT NULL,
  VarianceType varchar(16) NOT NULL CONSTRAINT CK_Var_Type CHECK (VarianceType IN ('PPV','INVOICEPRICE','MATUSAGE','MATPRICE','LABEFF','LABRATE','OVHD','RETURNVAL','LANDED','REVALUE','YIELD')), ItemCode varchar(40) NULL, JobRef varchar(20) NULL,
  KitRef varchar(30) NULL, SourceDocType varchar(20) NULL, SourceDocRef varchar(40) NULL, BaselineQty decimal(19,6) NULL, ActualQty decimal(19,6) NULL, BaselineRate decimal(19,6) NULL, ActualRate decimal(19,6) NULL,
  QuantityVarianceGBP decimal(18,2) NOT NULL CONSTRAINT DF_Var_Qv DEFAULT 0, RateVarianceGBP decimal(18,2) NOT NULL CONSTRAINT DF_Var_Rv DEFAULT 0, TotalVarianceGBP decimal(18,2) NOT NULL CONSTRAINT DF_Var_Tv DEFAULT 0,
  IsAdverse bit NOT NULL CONSTRAINT DF_Var_Adv DEFAULT 1, FiscalYear smallint NULL, PeriodNo tinyint NULL, ReasonCode varchar(20) NULL, Reason varchar(300) NULL, OwnerUser varchar(20) NULL, Disposal varchar(16) NULL, JournalId bigint NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Var_CA DEFAULT SYSDATETIME());
GO
