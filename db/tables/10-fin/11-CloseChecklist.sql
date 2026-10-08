/*  fin.CloseChecklist - period-close control evidence and sign-off.  REQ: FIN-011, FIN-021, XMD-005 */
SET NOCOUNT ON;
IF OBJECT_ID(N'fin.CloseChecklist') IS NULL CREATE TABLE fin.CloseChecklist (
  ChecklistId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_CloseChecklist PRIMARY KEY, CompanyCode varchar(10) NOT NULL, FiscalYear smallint NOT NULL, PeriodNo tinyint NOT NULL, StepCode varchar(20) NOT NULL, StepName varchar(100) NOT NULL,
  OwnerRole varchar(20) NULL, OwnerUser varchar(20) NULL, EvidenceRef varchar(200) NULL, DifferenceGBP decimal(18,2) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_Close_St DEFAULT 'OPEN' CONSTRAINT CK_Close_St CHECK (Status IN ('OPEN','DONE','ACCEPTED','BLOCKED')), IsBlocking bit NOT NULL CONSTRAINT DF_Close_Blk DEFAULT 1, CompletedBy varchar(20) NULL,
  CompletedAt datetime2(3) NULL, ReviewedBy varchar(20) NULL, ReviewedAt datetime2(3) NULL, Note varchar(300) NULL, RowVer rowversion, CONSTRAINT UQ_CloseChecklist UNIQUE (CompanyCode, FiscalYear, PeriodNo, StepCode));
GO
