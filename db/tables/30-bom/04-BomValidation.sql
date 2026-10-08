/*  bom.BomValidation / bom.BomValidationFinding - the evidence a bill was completely validated.
    BOM-004 says release requires complete validation AND authorised engineering review, so the
    release procedure refuses to act without a PASS row here that still matches the bill.
    BOM-029 says every refusal names the affected path and the reason, which is what a finding is.
    REQ: BOM-004, BOM-005, BOM-013, BOM-018, BOM-026, BOM-027, BOM-029 */
SET NOCOUNT ON;
IF OBJECT_ID(N'bom.BomValidation') IS NULL CREATE TABLE bom.BomValidation (
  ValidationId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_BomValidation PRIMARY KEY, CompanyCode varchar(10) NOT NULL, BomHeaderId int NOT NULL REFERENCES bom.BomHeader(BomHeaderId),
  ValidationRef varchar(30) NOT NULL, EffectiveDate date NOT NULL, ValidatedBy varchar(20) NOT NULL, ValidatedAt datetime2(3) NOT NULL CONSTRAINT DF_BomV_VA DEFAULT SYSDATETIME(),
  Outcome varchar(12) NOT NULL CONSTRAINT CK_BomV_Out CHECK (Outcome IN ('PASS','WARNING','FAIL')), ErrorCount int NOT NULL CONSTRAINT DF_BomV_Ec DEFAULT 0, WarningCount int NOT NULL CONSTRAINT DF_BomV_Wc DEFAULT 0,
  ComponentCount int NOT NULL CONSTRAINT DF_BomV_Cc DEFAULT 0, CycleFound bit NOT NULL CONSTRAINT DF_BomV_Cy DEFAULT 0, CyclePath varchar(400) NULL, ContentHash varchar(64) NULL, ReasonCode varchar(8) NULL,
  Message varchar(300) NULL, CorrelationId varchar(64) NULL, CONSTRAINT UQ_BomValidation UNIQUE (CompanyCode, ValidationRef));
GO
IF OBJECT_ID(N'bom.BomValidationFinding') IS NULL CREATE TABLE bom.BomValidationFinding (
  FindingId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_BomValidationFinding PRIMARY KEY, ValidationId int NOT NULL REFERENCES bom.BomValidation(ValidationId), LineNum int NOT NULL,
  Severity varchar(8) NOT NULL CONSTRAINT CK_BomVF_Sev CHECK (Severity IN ('ERROR','WARNING','INFO')), ReasonCode varchar(8) NOT NULL, BomComponentId int NULL, ComponentLineNum int NULL,
  PathText varchar(400) NULL, ItemCode varchar(40) NULL, Message varchar(300) NOT NULL, CONSTRAINT UQ_BomValidationFinding UNIQUE (ValidationId, LineNum));
GO
IF COL_LENGTH(N'bom.BomHeader', 'ValidationId') IS NULL ALTER TABLE bom.BomHeader ADD ValidationId int NULL;
GO
