/*  fin.Dimension / fin.DimensionValue / fin.AccountDimRule - analysis dimensions and the
    permitted account/dimension combinations validated at posting.  REQ: FIN-002, FIN-005 */
SET NOCOUNT ON;
IF OBJECT_ID(N'fin.Dimension') IS NULL CREATE TABLE fin.Dimension (
  DimensionCode varchar(10) NOT NULL CONSTRAINT PK_Dimension PRIMARY KEY, Name varchar(60) NOT NULL, Purpose varchar(200) NULL, SortOrder int NOT NULL CONSTRAINT DF_Dim_So DEFAULT 1, IsActive bit NOT NULL CONSTRAINT DF_Dim_Act DEFAULT 1,
  RowVer rowversion);
GO
IF OBJECT_ID(N'fin.DimensionValue') IS NULL CREATE TABLE fin.DimensionValue (
  CompanyCode varchar(10) NOT NULL, DimensionCode varchar(10) NOT NULL REFERENCES fin.Dimension(DimensionCode), DimValue varchar(20) NOT NULL, Name varchar(80) NOT NULL, OwnerUser varchar(20) NULL, SiteCode varchar(10) NULL,
  ValidFrom date NOT NULL CONSTRAINT DF_DimV_VF DEFAULT '2000-01-01', ValidTo date NULL, IsActive bit NOT NULL CONSTRAINT DF_DimV_Act DEFAULT 1, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_DimV_CA DEFAULT SYSDATETIME(),
  ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_DimensionValue PRIMARY KEY (CompanyCode, DimensionCode, DimValue));
GO
IF OBJECT_ID(N'fin.AccountDimRule') IS NULL CREATE TABLE fin.AccountDimRule (
  CompanyCode varchar(10) NOT NULL, AccountCode varchar(12) NOT NULL, DimensionCode varchar(10) NOT NULL REFERENCES fin.Dimension(DimensionCode),
  Requirement varchar(10) NOT NULL CONSTRAINT CK_AccDim_Req CHECK (Requirement IN ('MANDATORY','OPTIONAL','FORBIDDEN')), AllowedValues varchar(200) NULL, Description varchar(200) NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_AccDim_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_AccountDimRule PRIMARY KEY (CompanyCode, AccountCode, DimensionCode));
GO
