/*  po.SupplierSource / po.PurchasePrice / po.PurchasePriceBreak / po.SourcingEvent /
    po.SourcingQuote - approved sources, effective dated purchase pricing with quantity breaks
    and price-per units, and the sourcing comparison that justifies the chosen supplier.
    The cheapest price is deliberately NOT the automatic winner (PO-002).
    REQ: PO-002, PO-004, PO-023, D-08 */
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'po.SupplierSource') IS NULL CREATE TABLE po.SupplierSource (
  SupplierSourceId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_SupplierSource PRIMARY KEY,
  CompanyCode varchar(10) NOT NULL, ItemCode varchar(40) NOT NULL, SupplierCode varchar(20) NOT NULL,
  SupplierPartNumber varchar(40) NULL, Priority int NOT NULL CONSTRAINT DF_SupSrc_Pr DEFAULT 1,
  ApprovalStatus varchar(12) NOT NULL CONSTRAINT DF_SupSrc_As DEFAULT 'APPROVED'
    CONSTRAINT CK_SupSrc_As CHECK (ApprovalStatus IN ('APPROVED','CONDITION','UNAPPROVED','SUSPENDED')),
  ApprovalEvidence varchar(200) NULL, ContractRef varchar(40) NULL, QuoteRef varchar(40) NULL,
  LeadTimeDays int NOT NULL CONSTRAINT DF_SupSrc_Lt DEFAULT 0,
  MinOrderQty decimal(19,6) NOT NULL CONSTRAINT DF_SupSrc_Mo DEFAULT 0,
  PackQty decimal(19,6) NOT NULL CONSTRAINT DF_SupSrc_Pk DEFAULT 1,
  OrderUomCode varchar(10) NULL, CurrencyCode char(3) NOT NULL CONSTRAINT DF_SupSrc_Cu DEFAULT 'GBP',
  OverDeliveryTolerancePct decimal(9,4) NULL, CommercialRestriction varchar(200) NULL,
  EffectiveFrom date NOT NULL CONSTRAINT DF_SupSrc_Ef DEFAULT '2000-01-01', EffectiveTo date NULL,
  IsActive bit NOT NULL CONSTRAINT DF_SupSrc_Ia DEFAULT 1,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_SupSrc_CA DEFAULT SYSDATETIME(),
  ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT UQ_SupplierSource UNIQUE (CompanyCode, ItemCode, SupplierCode, EffectiveFrom));
GO
IF OBJECT_ID(N'po.PurchasePrice') IS NULL CREATE TABLE po.PurchasePrice (
  PurchasePriceId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_PurchasePrice PRIMARY KEY,
  CompanyCode varchar(10) NOT NULL, SupplierCode varchar(20) NOT NULL, ItemCode varchar(40) NULL,
  ItemGroupCode varchar(20) NULL,
  PriceBasis varchar(10) NOT NULL CONSTRAINT DF_PPrc_Ba DEFAULT 'LIST'
    CONSTRAINT CK_PPrc_Ba CHECK (PriceBasis IN ('CONTRACT','QUOTE','LIST','HISTORY')),
  AgreementRef varchar(40) NULL, CurrencyCode char(3) NOT NULL CONSTRAINT DF_PPrc_Cu DEFAULT 'GBP',
  UomCode varchar(10) NULL, PricePerQty decimal(19,6) NOT NULL CONSTRAINT DF_PPrc_Pp DEFAULT 1,
  UnitPrice decimal(19,6) NOT NULL, DiscountPct decimal(9,4) NOT NULL CONSTRAINT DF_PPrc_Di DEFAULT 0,
  SettlementDiscountPct decimal(9,4) NOT NULL CONSTRAINT DF_PPrc_Sd DEFAULT 0,
  EffectiveFrom date NOT NULL, EffectiveTo date NULL, Priority int NOT NULL CONSTRAINT DF_PPrc_Pri DEFAULT 10,
  IsActive bit NOT NULL CONSTRAINT DF_PPrc_Ia DEFAULT 1,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_PPrc_CA DEFAULT SYSDATETIME(),
  ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion);
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = N'IX_PurchasePrice_Lookup' AND object_id = OBJECT_ID(N'po.PurchasePrice'))
  CREATE INDEX IX_PurchasePrice_Lookup ON po.PurchasePrice (CompanyCode, SupplierCode, ItemCode, EffectiveFrom);
GO
IF OBJECT_ID(N'po.PurchasePriceBreak') IS NULL CREATE TABLE po.PurchasePriceBreak (
  PriceBreakId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_PurchasePriceBreak PRIMARY KEY,
  PurchasePriceId int NOT NULL REFERENCES po.PurchasePrice(PurchasePriceId),
  FromQty decimal(19,6) NOT NULL, UnitPrice decimal(19,6) NOT NULL,
  DiscountPct decimal(9,4) NOT NULL CONSTRAINT DF_PBrk_Di DEFAULT 0,
  CONSTRAINT UQ_PurchasePriceBreak UNIQUE (PurchasePriceId, FromQty));
GO
IF OBJECT_ID(N'po.SourcingEvent') IS NULL CREATE TABLE po.SourcingEvent (
  SourcingEventId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_SourcingEvent PRIMARY KEY,
  CompanyCode varchar(10) NOT NULL, SourcingRef varchar(30) NOT NULL, RequisitionNumber varchar(30) NULL,
  ItemCode varchar(40) NULL, Description varchar(200) NULL, Quantity decimal(19,6) NOT NULL,
  UomCode varchar(10) NULL, NeededByDate date NULL, BuyerUser varchar(20) NULL, EventDate date NOT NULL,
  SelectedSupplierCode varchar(20) NULL, SelectionReason varchar(300) NULL,
  SelectedOnPrice bit NOT NULL CONSTRAINT DF_SrcEv_Op DEFAULT 0,
  UnapprovedSourceUsed bit NOT NULL CONSTRAINT DF_SrcEv_Uu DEFAULT 0,
  AuthorityApprovalId bigint NULL, AuthorisedBy varchar(20) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_SrcEv_St DEFAULT 'OPEN'
    CONSTRAINT CK_SrcEv_St CHECK (Status IN ('OPEN','SELECTED','AUTHREQD','CANCELLED')),
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_SrcEv_CA DEFAULT SYSDATETIME(),
  ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT UQ_SourcingEvent UNIQUE (CompanyCode, SourcingRef));
GO
IF OBJECT_ID(N'po.SourcingQuote') IS NULL CREATE TABLE po.SourcingQuote (
  SourcingQuoteId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_SourcingQuote PRIMARY KEY,
  SourcingEventId int NOT NULL REFERENCES po.SourcingEvent(SourcingEventId), LineNum int NOT NULL,
  SupplierCode varchar(20) NOT NULL, ApprovalStatus varchar(12) NULL, QuoteRef varchar(40) NULL,
  CurrencyCode char(3) NOT NULL CONSTRAINT DF_SrcQt_Cu DEFAULT 'GBP',
  UnitPrice decimal(19,6) NOT NULL, PricePerQty decimal(19,6) NOT NULL CONSTRAINT DF_SrcQt_Pp DEFAULT 1,
  DiscountPct decimal(9,4) NOT NULL CONSTRAINT DF_SrcQt_Di DEFAULT 0,
  LineValueGBP decimal(18,2) NOT NULL CONSTRAINT DF_SrcQt_Lv DEFAULT 0,
  LeadTimeDays int NOT NULL CONSTRAINT DF_SrcQt_Lt DEFAULT 0, QuoteValidTo date NULL,
  OnTimePct decimal(9,4) NULL, QualityPct decimal(9,4) NULL, Restriction varchar(200) NULL,
  IsSelected bit NOT NULL CONSTRAINT DF_SrcQt_Se DEFAULT 0, RankByPrice int NULL,
  CONSTRAINT UQ_SourcingQuote UNIQUE (SourcingEventId, LineNum));
GO
