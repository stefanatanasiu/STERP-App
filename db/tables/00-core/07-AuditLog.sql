/*  core.AuditLog - attributable before/after history of every protected change.  REQ: COM-013, COM-021, RPT-016 */
SET NOCOUNT ON;
IF OBJECT_ID(N'core.AuditLog') IS NULL CREATE TABLE core.AuditLog (
  AuditId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_AuditLog PRIMARY KEY, EventTime datetime2(3) NOT NULL CONSTRAINT DF_Audit_ET DEFAULT SYSDATETIME(), UserCode varchar(20) NULL, CompanyCode varchar(10) NULL, EntityType varchar(60) NOT NULL,
  EntityKey varchar(120) NOT NULL, Action varchar(20) NOT NULL, FieldName varchar(60) NULL, OldValue varchar(400) NULL, NewValue varchar(400) NULL, Reason varchar(200) NULL, AuthorityRef varchar(60) NULL,
  Source varchar(20) NOT NULL CONSTRAINT DF_Audit_Src DEFAULT 'SQL', CorrelationId varchar(64) NULL, IsSensitive bit NOT NULL CONSTRAINT DF_Audit_Sens DEFAULT 0);
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_AuditLog_Entity') CREATE INDEX IX_AuditLog_Entity ON core.AuditLog(EntityType, EntityKey, EventTime);
GO
