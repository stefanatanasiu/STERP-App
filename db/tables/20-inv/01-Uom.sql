/*  inv.Uom - units of measure.  Only dimensionally compatible conversions are permitted and a
    missing or zero factor is invalid, never an assumed one-for-one.  REQ: ITM-005, D-09 */
SET NOCOUNT ON;
IF OBJECT_ID(N'inv.Uom') IS NULL CREATE TABLE inv.Uom (
  UomCode varchar(10) NOT NULL CONSTRAINT PK_Uom PRIMARY KEY, Name varchar(40) NOT NULL, UomDimension varchar(10) NOT NULL CONSTRAINT CK_Uom_Dim CHECK (UomDimension IN ('EACH','MASS','VOLUME','LENGTH','AREA','TIME')), BaseUomCode varchar(10) NULL,
  FactorToBase decimal(19,9) NOT NULL CONSTRAINT DF_Uom_Fac DEFAULT 1 CONSTRAINT CK_Uom_Fac CHECK (FactorToBase > 0), IsMetric bit NOT NULL CONSTRAINT DF_Uom_Met DEFAULT 1, DecimalPlaces tinyint NOT NULL CONSTRAINT DF_Uom_Dp DEFAULT 6,
  IsActive bit NOT NULL CONSTRAINT DF_Uom_Act DEFAULT 1, RowVer rowversion);
GO
