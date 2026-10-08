/*  core.NumberSeries / core.DocumentNumber - gap-free per-company document numbering,
    voided numbers retained and never reissued.  REQ: COM-005, D-15, D-21 */
SET NOCOUNT ON;
IF OBJECT_ID(N'core.NumberSeries') IS NULL CREATE TABLE core.NumberSeries (
  CompanyCode varchar(10) NOT NULL, SeriesCode varchar(20) NOT NULL, DocumentType varchar(20) NOT NULL, Description varchar(100) NOT NULL, Prefix varchar(6) NOT NULL CONSTRAINT DF_NumSer_Pfx DEFAULT '',
  NumberLength tinyint NOT NULL CONSTRAINT DF_NumSer_Len DEFAULT 6, NextNumber bigint NOT NULL CONSTRAINT DF_NumSer_Next DEFAULT 1, MinNumber bigint NOT NULL CONSTRAINT DF_NumSer_Min DEFAULT 1,
  MaxNumber bigint NOT NULL CONSTRAINT DF_NumSer_Max DEFAULT 999999, IsLegalSequence bit NOT NULL CONSTRAINT DF_NumSer_Legal DEFAULT 0, IsActive bit NOT NULL CONSTRAINT DF_NumSer_Act DEFAULT 1, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_NumSer_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_NumberSeries PRIMARY KEY (CompanyCode, SeriesCode));
GO
IF OBJECT_ID(N'core.DocumentNumber') IS NULL CREATE TABLE core.DocumentNumber (
  CompanyCode varchar(10) NOT NULL, SeriesCode varchar(20) NOT NULL, DocumentNumber varchar(30) NOT NULL, SequenceNo bigint NOT NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_DocNum_St DEFAULT 'ISSUED' CONSTRAINT CK_DocNum_St CHECK (Status IN ('ISSUED','USED','VOID')), IssuedAt datetime2(3) NOT NULL CONSTRAINT DF_DocNum_IA DEFAULT SYSDATETIME(), IssuedBy varchar(20) NULL,
  VoidedAt datetime2(3) NULL, VoidedBy varchar(20) NULL, VoidReason varchar(200) NULL, CorrelationId varchar(64) NULL, RowVer rowversion, CONSTRAINT PK_DocumentNumber PRIMARY KEY (CompanyCode, SeriesCode, DocumentNumber));
GO
