/*  core.PaymentTerms - reproducible due and discount dates, including the statutory default.
    REQ: COM-012, AR-003, AP-009, D-25 */
SET NOCOUNT ON;
IF OBJECT_ID(N'core.PaymentTerms') IS NULL CREATE TABLE core.PaymentTerms (
  TermsCode varchar(12) NOT NULL CONSTRAINT PK_PaymentTerms PRIMARY KEY, Name varchar(60) NOT NULL, BasisType varchar(16) NOT NULL CONSTRAINT CK_Terms_Basis CHECK (BasisType IN ('INVOICE','EOM','DAYOFNEXTMONTH')),
  Days smallint NOT NULL CONSTRAINT DF_Terms_Days DEFAULT 0, DayOfMonth tinyint NULL, DiscountDays smallint NULL, DiscountPct decimal(9,4) NULL, IsStatutoryDefault bit NOT NULL CONSTRAINT DF_Terms_Stat DEFAULT 0, EffectiveDays smallint NULL,
  Description varchar(200) NULL, IsActive bit NOT NULL CONSTRAINT DF_Terms_Act DEFAULT 1, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Terms_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL,
  ModifiedAt datetime2(3) NULL, RowVer rowversion);
GO
