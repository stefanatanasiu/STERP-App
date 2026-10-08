/*  core.Parameter - effective-dated company policy values (decisions D-01..D-30).  REQ: COM-025, D-07..D-30 */
SET NOCOUNT ON;
IF OBJECT_ID(N'core.Parameter') IS NULL CREATE TABLE core.Parameter (
  ParamCode varchar(40) NOT NULL, CompanyCode varchar(10) NOT NULL CONSTRAINT DF_Param_Co DEFAULT '*', EffectiveFrom date NOT NULL, Value varchar(200) NOT NULL,
  DataType varchar(10) NOT NULL CONSTRAINT DF_Param_Dt DEFAULT 'DECIMAL' CONSTRAINT CK_Param_Dt CHECK (DataType IN ('DECIMAL','INT','TEXT','DATE','BOOL','PCT','MONEY')), UnitOfValue varchar(20) NULL, DecisionRef varchar(10) NULL,
  Description varchar(200) NULL, OwnerRole varchar(20) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Param_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT PK_Parameter PRIMARY KEY (ParamCode, CompanyCode, EffectiveFrom));
GO
