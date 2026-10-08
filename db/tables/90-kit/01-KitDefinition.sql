/*  kit.KitDefinition / kit.KitDefinitionLine - versioned commercial sales kits.  The parent is a
    non-stock commercial identity; the components carry demand, cost and traceability.
    REQ: KIT-001, KIT-023, SO-020, D-11 */
SET NOCOUNT ON;
IF OBJECT_ID(N'kit.KitDefinition') IS NULL CREATE TABLE kit.KitDefinition (
  KitDefId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_KitDefinition PRIMARY KEY, CompanyCode varchar(10) NOT NULL, KitItemCode varchar(40) NOT NULL, KitVersion varchar(4) NOT NULL, Name varchar(100) NOT NULL, Description varchar(300) NULL,
  SupplyKind varchar(10) NOT NULL CONSTRAINT DF_KitD_Sk DEFAULT 'SINGLE' CONSTRAINT CK_KitD_Sk CHECK (SupplyKind IN ('SINGLE','MIXED')), VatOwnerDecisionBy varchar(20) NULL, VatOwnerDecisionAt datetime2(3) NULL,
  AllowPartialDespatch bit NOT NULL CONSTRAINT DF_KitD_Pd DEFAULT 0, EffectiveFrom date NOT NULL, EffectiveTo date NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_KitD_St DEFAULT 'DRAFT' CONSTRAINT CK_KitD_St CHECK (Status IN ('DRAFT','RELEASED','SUPERSEDED','RETIRED')), ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_KitD_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_KitDefinition UNIQUE (CompanyCode, KitItemCode, KitVersion));
GO
IF OBJECT_ID(N'kit.KitDefinitionLine') IS NULL CREATE TABLE kit.KitDefinitionLine (
  KitDefLineId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_KitDefinitionLine PRIMARY KEY, KitDefId int NOT NULL REFERENCES kit.KitDefinition(KitDefId), LineNum int NOT NULL, ComponentItemCode varchar(40) NOT NULL, ComponentRevision varchar(4) NULL,
  Quantity decimal(19,6) NOT NULL CONSTRAINT CK_KitDL_Q CHECK (Quantity > 0), UomCode varchar(10) NOT NULL, ConversionFactor decimal(19,9) NOT NULL CONSTRAINT DF_KitDL_Cf DEFAULT 1, StandaloneSellingPrice decimal(19,6) NULL,
  AllocationPct decimal(9,6) NULL, VatCode varchar(4) NULL, IsCustomerSpecified bit NOT NULL CONSTRAINT DF_KitDL_Cs DEFAULT 0, SubstitutionAllowed bit NOT NULL CONSTRAINT DF_KitDL_Sa DEFAULT 1,
  IsOptional bit NOT NULL CONSTRAINT DF_KitDL_Opt DEFAULT 0, Notes varchar(200) NULL, CONSTRAINT UQ_KitDefinitionLine UNIQUE (KitDefId, LineNum));
GO
