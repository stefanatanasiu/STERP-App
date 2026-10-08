/*  so.PriceList / so.PriceListLine - deterministic effective-dated price precedence
    (manual, contract, customer item, price group, standard list) with quantity breaks.
    REQ: SO-004, SO-006 */
SET NOCOUNT ON;
IF OBJECT_ID(N'so.PriceList') IS NULL CREATE TABLE so.PriceList (
  CompanyCode varchar(10) NOT NULL, PriceListCode varchar(20) NOT NULL, Name varchar(80) NOT NULL, PriceLevel tinyint NOT NULL CONSTRAINT CK_PrL_Lv CHECK (PriceLevel BETWEEN 1 AND 5),
  Scope varchar(12) NOT NULL CONSTRAINT CK_PrL_Sc CHECK (Scope IN ('CONTRACT','CUSTOMER','GROUP','STANDARD')), CurrencyCode char(3) NOT NULL CONSTRAINT DF_PrL_Cur DEFAULT 'GBP', CustomerCode varchar(20) NULL, PriceGroup varchar(20) NULL,
  ContractRef varchar(40) NULL, ValidFrom date NOT NULL, ValidTo date NULL, IsActive bit NOT NULL CONSTRAINT DF_PrL_Act DEFAULT 1, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_PrL_CA DEFAULT SYSDATETIME(),
  ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_PriceList PRIMARY KEY (CompanyCode, PriceListCode));
GO
IF OBJECT_ID(N'so.PriceListLine') IS NULL CREATE TABLE so.PriceListLine (
  PriceLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_PriceListLine PRIMARY KEY, CompanyCode varchar(10) NOT NULL, PriceListCode varchar(20) NOT NULL, ItemCode varchar(40) NOT NULL, UomCode varchar(10) NULL,
  PricePerQty decimal(19,6) NOT NULL CONSTRAINT DF_PrLL_Pp DEFAULT 1, QtyBreak decimal(19,6) NOT NULL CONSTRAINT DF_PrLL_Qb DEFAULT 0, CurrencyCode char(3) NOT NULL CONSTRAINT DF_PrLL_Cur DEFAULT 'GBP', UnitPrice decimal(19,6) NOT NULL,
  DiscountPct decimal(9,4) NOT NULL CONSTRAINT DF_PrLL_D DEFAULT 0, MinimumMarginPct decimal(9,4) NULL, ValidFrom date NOT NULL, ValidTo date NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_PrLL_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT UQ_PriceListLine UNIQUE (CompanyCode, PriceListCode, ItemCode, QtyBreak, CurrencyCode, ValidFrom));
GO
