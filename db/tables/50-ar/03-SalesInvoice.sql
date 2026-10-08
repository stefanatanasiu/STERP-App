/*  ar.SalesInvoice / ar.SalesInvoiceLine / ar.SalesInvoiceVat - valid UK VAT invoices and credit
    notes with the commercial inputs snapshotted at confirmation.
    REQ: AR-004, AR-006, AR-023, AR-029, COM-007, COM-020, FIN-017 */
SET NOCOUNT ON;
IF OBJECT_ID(N'ar.SalesInvoice') IS NULL CREATE TABLE ar.SalesInvoice (
  InvoiceId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_SalesInvoice PRIMARY KEY, CompanyCode varchar(10) NOT NULL, DocumentType varchar(12) NOT NULL CONSTRAINT CK_SInv_Dt CHECK (DocumentType IN ('INVOICE','CREDITNOTE','PROFORMA')),
  InvoiceNumber varchar(30) NOT NULL, CustomerCode varchar(20) NOT NULL, InvoiceCustomerCode varchar(20) NULL, DocumentDate date NOT NULL, TaxPointDate date NOT NULL, PostingDate date NOT NULL, DueDate date NOT NULL, FiscalYear smallint NULL,
  PeriodNo tinyint NULL, VatPeriodKey varchar(10) NULL, CurrencyCode char(3) NOT NULL CONSTRAINT DF_SInv_Cur DEFAULT 'GBP', FxRate decimal(18,8) NOT NULL CONSTRAINT DF_SInv_Fx DEFAULT 1, FxRateDate date NULL, FxRateType varchar(6) NULL,
  NetAmount decimal(18,2) NOT NULL CONSTRAINT DF_SInv_Net DEFAULT 0, VatAmount decimal(18,2) NOT NULL CONSTRAINT DF_SInv_Vat DEFAULT 0, GrossAmount decimal(18,2) NOT NULL CONSTRAINT DF_SInv_Gr DEFAULT 0,
  NetAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_SInv_NetG DEFAULT 0, VatAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_SInv_VatG DEFAULT 0, GrossAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_SInv_GrG DEFAULT 0,
  SettlementDiscountPct decimal(9,4) NULL, DepositAppliedGBP decimal(18,2) NOT NULL CONSTRAINT DF_SInv_Dep DEFAULT 0, TermsCode varchar(12) NULL, TermsSnapshot varchar(100) NULL, TermsSource varchar(20) NULL, BillToName varchar(120) NULL,
  BillToAddress varchar(300) NULL, DeliveryAddress varchar(300) NULL, CustomerVatRegNo varchar(15) NULL, CompanyVatRegNo varchar(15) NULL, VatLocation varchar(3) NULL, IsSimplifiedInvoice bit NOT NULL CONSTRAINT DF_SInv_Simp DEFAULT 0,
  SpecialWording varchar(300) NULL, CustomerOrderRef varchar(40) NULL, SalesOrderNumber varchar(30) NULL, DespatchRef varchar(30) NULL, SalespersonCode varchar(20) NULL, OriginalInvoiceNumber varchar(30) NULL, CreditReason varchar(200) NULL,
  CreditableRemainingGBP decimal(18,2) NULL, Status varchar(12) NOT NULL CONSTRAINT DF_SInv_St DEFAULT 'DRAFT' CONSTRAINT CK_SInv_St CHECK (Status IN ('DRAFT','APPROVED','POSTED','REVERSED','VOID')), ApprovalId bigint NULL,
  ApprovedBy varchar(20) NULL, PostedBy varchar(20) NULL, PostedAt datetime2(3) NULL, JournalId bigint NULL, IssuedAt datetime2(3) NULL, ReprintCount int NOT NULL CONSTRAINT DF_SInv_Rp DEFAULT 0, EInvoiceRef varchar(60) NULL,
  RetentionClass varchar(16) NULL, CorrelationId varchar(64) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_SInv_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT UQ_SalesInvoice UNIQUE (CompanyCode, DocumentType, InvoiceNumber));
GO
IF OBJECT_ID(N'ar.SalesInvoiceLine') IS NULL CREATE TABLE ar.SalesInvoiceLine (
  InvoiceLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_SalesInvoiceLine PRIMARY KEY, InvoiceId bigint NOT NULL REFERENCES ar.SalesInvoice(InvoiceId), LineNum int NOT NULL, ItemCode varchar(40) NULL, Revision varchar(4) NULL,
  Description varchar(200) NOT NULL, Quantity decimal(19,6) NOT NULL, UomCode varchar(10) NULL, ConversionFactor decimal(19,9) NULL, StockQuantity decimal(19,6) NULL, UnitPrice decimal(19,6) NOT NULL,
  PricePerQty decimal(19,6) NOT NULL CONSTRAINT DF_SInvL_Pp DEFAULT 1, PriceSource varchar(20) NULL, DiscountPct1 decimal(9,4) NOT NULL CONSTRAINT DF_SInvL_D1 DEFAULT 0, DiscountPct2 decimal(9,4) NOT NULL CONSTRAINT DF_SInvL_D2 DEFAULT 0,
  AllowanceAmount decimal(18,2) NOT NULL CONSTRAINT DF_SInvL_Al DEFAULT 0, ChargeAmount decimal(18,2) NOT NULL CONSTRAINT DF_SInvL_Ch DEFAULT 0, NetAmount decimal(18,2) NOT NULL, VatCode varchar(4) NOT NULL, VatRatePct decimal(9,4) NOT NULL,
  VatAmount decimal(18,2) NOT NULL, NetAmountGBP decimal(18,2) NOT NULL, VatAmountGBP decimal(18,2) NOT NULL, VatLiability varchar(12) NULL, VatMechanism varchar(12) NULL, VatEvidenceRef varchar(60) NULL, CostSnapshotGBP decimal(18,2) NULL,
  DimCostCentre varchar(20) NULL, DimProject varchar(20) NULL, JobRef varchar(20) NULL, RevenueAccountCode varchar(12) NULL, SalesOrderLine int NULL, DespatchLine int NULL, KitParentLine int NULL, StandaloneSellingPrice decimal(19,6) NULL,
  AllocatedRevenueGBP decimal(18,2) NULL, CONSTRAINT UQ_SalesInvoiceLine UNIQUE (InvoiceId, LineNum));
GO
IF OBJECT_ID(N'ar.SalesInvoiceVat') IS NULL CREATE TABLE ar.SalesInvoiceVat (
  InvoiceId bigint NOT NULL REFERENCES ar.SalesInvoice(InvoiceId), VatCode varchar(4) NOT NULL, RatePct decimal(9,4) NOT NULL, NetAmount decimal(18,2) NOT NULL, VatAmount decimal(18,2) NOT NULL, NetAmountGBP decimal(18,2) NOT NULL,
  VatAmountGBP decimal(18,2) NOT NULL, VatBox tinyint NULL, NetBox tinyint NULL, CONSTRAINT PK_SalesInvoiceVat PRIMARY KEY (InvoiceId, VatCode));
GO
