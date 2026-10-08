/*  core.IdempotencyKey - one confirmed business instruction, one effect.  REQ: COM-015, COM-014 */
SET NOCOUNT ON;
IF OBJECT_ID(N'core.IdempotencyKey') IS NULL CREATE TABLE core.IdempotencyKey (
  CorrelationId varchar(64) NOT NULL CONSTRAINT PK_IdempotencyKey PRIMARY KEY, Operation varchar(60) NOT NULL, CompanyCode varchar(10) NULL, UserCode varchar(20) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_Idem_St DEFAULT 'INPROGRESS' CONSTRAINT CK_Idem_St CHECK (Status IN ('INPROGRESS','COMPLETED','FAILED')), ResultRef varchar(60) NULL, ResultMessage varchar(400) NULL,
  StartedAt datetime2(3) NOT NULL CONSTRAINT DF_Idem_SA DEFAULT SYSDATETIME(), EndedAt datetime2(3) NULL, RowVer rowversion);
GO
