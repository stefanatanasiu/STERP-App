/*  core.ExceptionItem - owned, aged, prioritised business exception worklist.  REQ: COM-017, COM-022, XMD-006 */
SET NOCOUNT ON;
IF OBJECT_ID(N'core.ExceptionItem') IS NULL CREATE TABLE core.ExceptionItem (
  ExceptionId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_ExceptionItem PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NULL, Module varchar(6) NOT NULL,
  Severity varchar(12) NOT NULL CONSTRAINT CK_Exc_Sev CHECK (Severity IN ('HOLD','WARNING','ERROR')), Category varchar(40) NOT NULL, DocumentType varchar(20) NULL, DocumentRef varchar(40) NULL, Message varchar(400) NOT NULL,
  AmountGBP decimal(18,2) NULL, Quantity decimal(19,6) NULL, OwnerUser varchar(20) NULL, OwnerRole varchar(20) NULL, Priority tinyint NOT NULL CONSTRAINT DF_Exc_Pri DEFAULT 5, RaisedAt datetime2(3) NOT NULL CONSTRAINT DF_Exc_RA DEFAULT SYSDATETIME(),
  RaisedBy varchar(20) NULL, DueAt datetime2(3) NULL, AgeDays int NULL, EscalatedAt datetime2(3) NULL, Status varchar(14) NOT NULL CONSTRAINT DF_Exc_St DEFAULT 'OPEN' CONSTRAINT CK_Exc_St CHECK (Status IN ('OPEN','ACKNOWLEDGED','RESOLVED')),
  ResolvedBy varchar(20) NULL, ResolvedAt datetime2(3) NULL, ResolutionNote varchar(400) NULL, CorrelationId varchar(64) NULL, RowVer rowversion);
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_ExceptionItem_Open') CREATE INDEX IX_ExceptionItem_Open ON core.ExceptionItem(CompanyCode, Status, Module, RaisedAt);
GO
