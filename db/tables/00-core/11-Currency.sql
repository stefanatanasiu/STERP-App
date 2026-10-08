/*  core.Currency / core.FxRate - sterling base, rates quoted as foreign units per GBP 1.
    REQ: COM-010, FIN-019, D-14 */
SET NOCOUNT ON;
IF OBJECT_ID(N'core.Currency') IS NULL CREATE TABLE core.Currency (
  CurrencyCode char(3) NOT NULL CONSTRAINT PK_Currency PRIMARY KEY, Name varchar(40) NOT NULL, Symbol nvarchar(4) NOT NULL, MinorUnits tinyint NOT NULL CONSTRAINT DF_Cur_Mu DEFAULT 2, IsActive bit NOT NULL CONSTRAINT DF_Cur_Act DEFAULT 1,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Cur_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion);
GO
IF OBJECT_ID(N'core.FxRate') IS NULL CREATE TABLE core.FxRate (
  CurrencyCode char(3) NOT NULL REFERENCES core.Currency(CurrencyCode), RateDate date NOT NULL, RateType varchar(6) NOT NULL CONSTRAINT CK_Fx_Type CHECK (RateType IN ('SPOT','HMRC','BOE')),
  UnitsPerGBP decimal(18,8) NOT NULL CONSTRAINT CK_Fx_Pos CHECK (UnitsPerGBP > 0), Source varchar(40) NOT NULL CONSTRAINT DF_Fx_Src DEFAULT 'Bank of England', EnteredQuotation varchar(20) NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Fx_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_FxRate PRIMARY KEY (CurrencyCode, RateDate, RateType));
GO
