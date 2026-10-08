/*  inv.ItemPartner / inv.ItemSubstitute - supplier and customer item references (aliases) with
    supply terms and price breaks, plus approved substitutes.  REQ: ITM-009, ITM-019, PO-002, PO-004 */
SET NOCOUNT ON;
IF OBJECT_ID(N'inv.ItemPartner') IS NULL CREATE TABLE inv.ItemPartner (
  ItemPartnerId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_ItemPartner PRIMARY KEY, CompanyCode varchar(10) NOT NULL, ItemCode varchar(40) NOT NULL,
  PartnerType varchar(10) NOT NULL CONSTRAINT CK_ItemPart_Pt CHECK (PartnerType IN ('SUPPLIER','CUSTOMER','MANUFACT')), PartnerCode varchar(20) NOT NULL, PartnerItemCode varchar(40) NOT NULL, PartnerDescription varchar(100) NULL,
  UomCode varchar(10) NULL, StockUnitsPerUom decimal(19,9) NULL, QtyBreak decimal(19,6) NOT NULL CONSTRAINT DF_ItemPart_Qb DEFAULT 0, CurrencyCode char(3) NULL, UnitPrice decimal(19,6) NULL,
  PricePerQty decimal(19,6) NOT NULL CONSTRAINT DF_ItemPart_Pp DEFAULT 1, LeadTimeDays smallint NULL, MinOrderQty decimal(19,6) NULL, OrderMultipleQty decimal(19,6) NULL, PreferenceRank tinyint NOT NULL CONSTRAINT DF_ItemPart_Rank DEFAULT 1,
  IsApprovedSource bit NOT NULL CONSTRAINT DF_ItemPart_Appr DEFAULT 1, QuoteRef varchar(40) NULL, EffectiveFrom date NOT NULL CONSTRAINT DF_ItemPart_VF DEFAULT '2000-01-01', EffectiveTo date NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ItemPart_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT UQ_ItemPartner UNIQUE (CompanyCode, ItemCode, PartnerType, PartnerCode, QtyBreak, EffectiveFrom));
GO
IF OBJECT_ID(N'inv.ItemSubstitute') IS NULL CREATE TABLE inv.ItemSubstitute (
  CompanyCode varchar(10) NOT NULL, ItemCode varchar(40) NOT NULL, SubstituteItemCode varchar(40) NOT NULL, SubstituteRevision varchar(4) NULL,
  ConversionRatio decimal(19,9) NOT NULL CONSTRAINT DF_ItemSub_R DEFAULT 1 CONSTRAINT CK_ItemSub_R CHECK (ConversionRatio > 0), ContextPurchase bit NOT NULL CONSTRAINT DF_ItemSub_P DEFAULT 0,
  ContextSales bit NOT NULL CONSTRAINT DF_ItemSub_S DEFAULT 0, ContextKit bit NOT NULL CONSTRAINT DF_ItemSub_K DEFAULT 1, ContextBom bit NOT NULL CONSTRAINT DF_ItemSub_B DEFAULT 0,
  RequiresCustomerAgreement bit NOT NULL CONSTRAINT DF_ItemSub_C DEFAULT 0, Restrictions varchar(200) NULL, ValidFrom date NOT NULL CONSTRAINT DF_ItemSub_VF DEFAULT '2000-01-01', ValidTo date NULL, ApprovedBy varchar(20) NULL,
  ApprovedAt datetime2(3) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ItemSub_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT PK_ItemSubstitute PRIMARY KEY (CompanyCode, ItemCode, SubstituteItemCode));
GO
