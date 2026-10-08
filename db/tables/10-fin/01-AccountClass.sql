/*  fin.AccountClass / fin.StatutoryLine - account classification and the FRS 102 / FRS 105
    statutory format lines every posting account maps to.  REQ: FIN-001, FIN-023 */
SET NOCOUNT ON;
IF OBJECT_ID(N'fin.AccountClass') IS NULL CREATE TABLE fin.AccountClass (
  ClassCode varchar(10) NOT NULL CONSTRAINT PK_AccountClass PRIMARY KEY, Name varchar(60) NOT NULL, ClassType varchar(10) NOT NULL CONSTRAINT CK_AccCls_Type CHECK (ClassType IN ('ASSET','LIABILITY','EQUITY','REVENUE','EXPENSE')),
  NormalBalance char(2) NOT NULL CONSTRAINT CK_AccCls_Nb CHECK (NormalBalance IN ('DR','CR')), Statement char(2) NOT NULL CONSTRAINT CK_AccCls_St CHECK (Statement IN ('PL','BS')), SortOrder int NOT NULL CONSTRAINT DF_AccCls_So DEFAULT 100,
  RowVer rowversion);
GO
IF OBJECT_ID(N'fin.StatutoryLine') IS NULL CREATE TABLE fin.StatutoryLine (
  Framework varchar(10) NOT NULL, StatutoryLineCode varchar(20) NOT NULL, Statement char(2) NOT NULL CONSTRAINT CK_StatLine_St CHECK (Statement IN ('PL','BS')), Caption varchar(100) NOT NULL, ParentLineCode varchar(20) NULL,
  SortOrder int NOT NULL CONSTRAINT DF_StatLine_So DEFAULT 100, SignConvention smallint NOT NULL CONSTRAINT DF_StatLine_Sgn DEFAULT 1, RowVer rowversion, CONSTRAINT PK_StatutoryLine PRIMARY KEY (Framework, StatutoryLineCode));
GO
