/*  so.Quote / so.QuoteLine - versioned quotations with validity and acceptance evidence.
    A quote is not a sales commitment.  REQ: SO-001, COM-006 */
SET NOCOUNT ON;
IF OBJECT_ID(N'so.Quote') IS NULL CREATE TABLE so.Quote (
  QuoteId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_Quote PRIMARY KEY, CompanyCode varchar(10) NOT NULL, QuoteNumber varchar(30) NOT NULL, Version smallint NOT NULL CONSTRAINT DF_Qte_Ver DEFAULT 1, CustomerCode varchar(20) NULL,
  ProspectName varchar(120) NULL, QuoteDate date NOT NULL, ValidUntil date NOT NULL, CurrencyCode char(3) NOT NULL CONSTRAINT DF_Qte_Cur DEFAULT 'GBP', FxRate decimal(18,8) NOT NULL CONSTRAINT DF_Qte_Fx DEFAULT 1, TermsCode varchar(12) NULL,
  TermsVersion varchar(20) NULL, DeliveryEstimate varchar(100) NULL, Assumptions varchar(400) NULL, SalespersonCode varchar(20) NULL, NetAmount decimal(18,2) NOT NULL CONSTRAINT DF_Qte_Net DEFAULT 0,
  VatAmount decimal(18,2) NOT NULL CONSTRAINT DF_Qte_Vat DEFAULT 0, GrossAmount decimal(18,2) NOT NULL CONSTRAINT DF_Qte_Gr DEFAULT 0, CostBasisGBP decimal(18,2) NULL, MarginPct decimal(9,4) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_Qte_St DEFAULT 'DRAFT' CONSTRAINT CK_Qte_St CHECK (Status IN ('DRAFT','ISSUED','REVISED','ACCEPTED','DECLINED','EXPIRED','CONVERTED','PARTCONV')), AcceptedAt datetime2(3) NULL,
  AcceptanceEvidence varchar(200) NULL, ConvertedOrderNumber varchar(30) NULL, CorrelationId varchar(64) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Qte_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL,
  ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_Quote UNIQUE (CompanyCode, QuoteNumber, Version));
GO
IF OBJECT_ID(N'so.QuoteLine') IS NULL CREATE TABLE so.QuoteLine (
  QuoteLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_QuoteLine PRIMARY KEY, QuoteId bigint NOT NULL REFERENCES so.Quote(QuoteId), LineNum int NOT NULL, ItemCode varchar(40) NULL, Revision varchar(4) NULL, Description varchar(200) NOT NULL,
  Quantity decimal(19,6) NOT NULL, UomCode varchar(10) NULL, UnitPrice decimal(19,6) NOT NULL, PricePerQty decimal(19,6) NOT NULL CONSTRAINT DF_QteL_Pp DEFAULT 1, DiscountPct1 decimal(9,4) NOT NULL CONSTRAINT DF_QteL_D1 DEFAULT 0,
  DiscountPct2 decimal(9,4) NOT NULL CONSTRAINT DF_QteL_D2 DEFAULT 0, NetAmount decimal(18,2) NOT NULL, VatCode varchar(4) NULL, VatAmount decimal(18,2) NOT NULL CONSTRAINT DF_QteL_V DEFAULT 0, EstimatedCostGBP decimal(18,2) NULL,
  CostBasis varchar(20) NULL, MarginPct decimal(9,4) NULL, LeadTimeDays smallint NULL, CONSTRAINT UQ_QuoteLine UNIQUE (QuoteId, LineNum));
GO
