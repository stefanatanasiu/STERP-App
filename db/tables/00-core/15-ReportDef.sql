/*  core.ReportDef / core.JobRun - batch report catalogue and run register.  REQ: COM-019, RPT-001..RPT-019 */
SET NOCOUNT ON;
IF OBJECT_ID(N'core.ReportDef') IS NULL CREATE TABLE core.ReportDef (
  ReportCode varchar(20) NOT NULL CONSTRAINT PK_ReportDef PRIMARY KEY, Module varchar(6) NOT NULL, Title varchar(100) NOT NULL, ProgramName varchar(8) NOT NULL, ParamSpec varchar(100) NULL, PermissionCode varchar(40) NULL,
  RequirementRefs varchar(200) NULL, Description varchar(300) NULL, IsActive bit NOT NULL CONSTRAINT DF_RepDef_Act DEFAULT 1, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_RepDef_CA DEFAULT SYSDATETIME(),
  ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion);
GO
IF OBJECT_ID(N'core.JobRun') IS NULL CREATE TABLE core.JobRun (
  JobRunId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_JobRun PRIMARY KEY, JobName varchar(40) NOT NULL, CompanyCode varchar(10) NULL, SiteCode varchar(10) NULL, Arguments varchar(300) NULL, SubmittedBy varchar(20) NULL,
  StartedAt datetime2(3) NOT NULL CONSTRAINT DF_JobRun_SA DEFAULT SYSDATETIME(), EndedAt datetime2(3) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_JobRun_St DEFAULT 'RUNNING' CONSTRAINT CK_JobRun_St CHECK (Status IN ('RUNNING','OK','WARNING','FAILED','ABENDED')), ReturnCode int NULL, RowsProcessed int NULL, LogFile varchar(260) NULL,
  SpoolFile varchar(260) NULL, Message varchar(400) NULL, RowVer rowversion);
GO
