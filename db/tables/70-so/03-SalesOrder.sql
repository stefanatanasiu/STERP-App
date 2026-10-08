/*  so.SalesOrder / so.SalesOrderLine / so.DeliverySchedule - commercial demand.  Requested,
    original promised and current promised dates are all retained so service measures cannot be
    improved by editing the current promise.  REQ: SO-002..SO-008, SO-015, SO-021, SO-025..SO-029 */
SET NOCOUNT ON;
IF OBJECT_ID(N'so.SalesOrder') IS NULL CREATE TABLE so.SalesOrder (
  SalesOrderId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_SalesOrder PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, OrderNumber varchar(30) NOT NULL,
  OrderType varchar(12) NOT NULL CONSTRAINT DF_SOrd_Ot DEFAULT 'STANDARD' CONSTRAINT CK_SOrd_Ot CHECK (OrderType IN ('STANDARD','BLANKET','CALLOFF','REPEAT','SCHEDULED','DROPSHIP')), BlanketOrderNumber varchar(30) NULL,
  CustomerCode varchar(20) NOT NULL, InvoiceCustomerCode varchar(20) NULL, CustomerOrderRef varchar(40) NULL, OrderDate date NOT NULL, QuoteNumber varchar(30) NULL, CurrencyCode char(3) NOT NULL CONSTRAINT DF_SOrd_Cur DEFAULT 'GBP',
  FxRate decimal(18,8) NOT NULL CONSTRAINT DF_SOrd_Fx DEFAULT 1, FxRateDate date NULL, TermsCode varchar(12) NULL, TermsSnapshot varchar(100) NULL, SalespersonCode varchar(20) NULL, IncotermsRule varchar(3) NULL, IncotermsNamedPlace varchar(60) NULL,
  DeliveryAddressRef varchar(10) NULL, DeliveryAddressSnapshot varchar(300) NULL, BillToAddressSnapshot varchar(300) NULL, TermsAndConditionsVersion varchar(20) NULL, RetentionOfTitle bit NOT NULL CONSTRAINT DF_SOrd_Rot DEFAULT 1,
  VatTreatmentBasis varchar(20) NULL, CustomerVatRegNo varchar(15) NULL, VatLocation varchar(3) NULL, NetAmount decimal(18,2) NOT NULL CONSTRAINT DF_SOrd_Net DEFAULT 0, VatAmount decimal(18,2) NOT NULL CONSTRAINT DF_SOrd_Vat DEFAULT 0,
  GrossAmount decimal(18,2) NOT NULL CONSTRAINT DF_SOrd_Gr DEFAULT 0, GrossAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_SOrd_GrG DEFAULT 0,
  CommercialStatus varchar(12) NOT NULL CONSTRAINT DF_SOrd_Cs DEFAULT 'DRAFT' CONSTRAINT CK_SOrd_Cs CHECK (CommercialStatus IN ('DRAFT','ACCEPTED','AMENDED','CANCELLED','COMPLETED')),
  ReleaseStatus varchar(14) NOT NULL CONSTRAINT DF_SOrd_Rs DEFAULT 'AWAITAPPROVAL' CONSTRAINT CK_SOrd_Rs CHECK (ReleaseStatus IN ('AWAITAPPROVAL','CREDITHOLD','COMMERCIALHOLD','SUPPLYHOLD','RELEASED')),
  FulfilmentStatus varchar(14) NOT NULL CONSTRAINT DF_SOrd_Fs DEFAULT 'UNALLOCATED' CONSTRAINT CK_SOrd_Fs CHECK (FulfilmentStatus IN ('UNALLOCATED','PARTRESERVED','RESERVED','PARTPICKED','PICKED','PARTDESPATCH','DESPATCHED')),
  BillingStatus varchar(14) NOT NULL CONSTRAINT DF_SOrd_Bs DEFAULT 'NOTELIGIBLE' CONSTRAINT CK_SOrd_Bs CHECK (BillingStatus IN ('NOTELIGIBLE','ELIGIBLE','PARTBILLED','BILLED')), HoldReason varchar(200) NULL, CreditOverrideId int NULL,
  ApprovalId bigint NULL, ContentHash varchar(64) NULL, DepositRequiredGBP decimal(18,2) NULL, DepositReceivedGBP decimal(18,2) NOT NULL CONSTRAINT DF_SOrd_Dep DEFAULT 0, AcknowledgementIssuedAt datetime2(3) NULL, CorrelationId varchar(64) NULL,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_SOrd_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_SalesOrder UNIQUE (CompanyCode, OrderNumber));
GO
IF OBJECT_ID(N'so.SalesOrderLine') IS NULL CREATE TABLE so.SalesOrderLine (
  SalesOrderLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_SalesOrderLine PRIMARY KEY, SalesOrderId bigint NOT NULL REFERENCES so.SalesOrder(SalesOrderId), CompanyCode varchar(10) NOT NULL, OrderNumber varchar(30) NOT NULL, LineNum int NOT NULL,
  LineRef varchar(10) NOT NULL, ItemCode varchar(40) NULL, Revision varchar(4) NULL, Description varchar(200) NOT NULL,
  LineType varchar(10) NOT NULL CONSTRAINT DF_SOrdL_Lt DEFAULT 'STOCK' CONSTRAINT CK_SOrdL_Lt CHECK (LineType IN ('STOCK','NONSTOCK','SERVICE','KIT','KITCOMP','CHARGE','TEXT')), KitParentLineNum int NULL, AcceptedQty decimal(19,6) NOT NULL,
  CancelledQty decimal(19,6) NOT NULL CONSTRAINT DF_SOrdL_Cq DEFAULT 0, DespatchedQty decimal(19,6) NOT NULL CONSTRAINT DF_SOrdL_Dq DEFAULT 0, BilledQty decimal(19,6) NOT NULL CONSTRAINT DF_SOrdL_Bq DEFAULT 0,
  ReturnedQty decimal(19,6) NOT NULL CONSTRAINT DF_SOrdL_Rq DEFAULT 0, UomCode varchar(10) NULL, ConversionFactor decimal(19,9) NOT NULL CONSTRAINT DF_SOrdL_Cf DEFAULT 1, StockQty decimal(19,6) NULL, UnitPrice decimal(19,6) NOT NULL,
  PricePerQty decimal(19,6) NOT NULL CONSTRAINT DF_SOrdL_Pp DEFAULT 1, PriceSource varchar(20) NULL, PriceListCode varchar(20) NULL, DiscountPct1 decimal(9,4) NOT NULL CONSTRAINT DF_SOrdL_D1 DEFAULT 0,
  DiscountPct2 decimal(9,4) NOT NULL CONSTRAINT DF_SOrdL_D2 DEFAULT 0, AllowanceAmount decimal(18,2) NOT NULL CONSTRAINT DF_SOrdL_Al DEFAULT 0, ChargeAmount decimal(18,2) NOT NULL CONSTRAINT DF_SOrdL_Ch DEFAULT 0,
  NetAmount decimal(18,2) NOT NULL CONSTRAINT DF_SOrdL_Net DEFAULT 0, VatCode varchar(4) NULL, VatRatePct decimal(9,4) NULL, VatAmount decimal(18,2) NOT NULL CONSTRAINT DF_SOrdL_Vat DEFAULT 0, VatEvidenceRef varchar(60) NULL,
  VatOverrideReason varchar(200) NULL, CostSnapshotGBP decimal(19,6) NULL, CostBasis varchar(20) NULL, MarginPct decimal(9,4) NULL, MarginApprovalId bigint NULL, StandaloneSellingPrice decimal(19,6) NULL, DimCostCentre varchar(20) NULL,
  DimProject varchar(20) NULL, JobRef varchar(20) NULL, SupplyType varchar(12) NOT NULL CONSTRAINT DF_SOrdL_Sup DEFAULT 'STOCK' CONSTRAINT CK_SOrdL_Sup CHECK (SupplyType IN ('STOCK','BACKTOBACK','DROPSHIP','MAKE')), LinkedPoNumber varchar(30) NULL,
  LinkedPoLine int NULL, LinkedKitRef varchar(30) NULL, Status varchar(10) NOT NULL CONSTRAINT DF_SOrdL_St DEFAULT 'OPEN' CONSTRAINT CK_SOrdL_St CHECK (Status IN ('OPEN','CLOSED','CANCELLED')),
  CONSTRAINT UQ_SalesOrderLine UNIQUE (SalesOrderId, LineNum));
GO
IF OBJECT_ID(N'so.DeliverySchedule') IS NULL CREATE TABLE so.DeliverySchedule (
  ScheduleId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_DeliverySchedule PRIMARY KEY, SalesOrderLineId bigint NOT NULL REFERENCES so.SalesOrderLine(SalesOrderLineId), CompanyCode varchar(10) NOT NULL, OrderNumber varchar(30) NOT NULL,
  LineNum int NOT NULL, ScheduleNo int NOT NULL, Quantity decimal(19,6) NOT NULL, DespatchedQty decimal(19,6) NOT NULL CONSTRAINT DF_SSch_Dq DEFAULT 0, SiteCode varchar(10) NULL, DeliveryAddressRef varchar(10) NULL, RequestedDate date NOT NULL,
  OriginalPromiseDate date NOT NULL, CurrentPromiseDate date NOT NULL, ChangeReason varchar(200) NULL, ChangeCount int NOT NULL CONSTRAINT DF_SSch_Cc DEFAULT 0, FulfilmentMethod varchar(12) NOT NULL CONSTRAINT DF_SSch_Fm DEFAULT 'WAREHOUSE',
  Priority tinyint NOT NULL CONSTRAINT DF_SSch_Pri DEFAULT 5, Status varchar(10) NOT NULL CONSTRAINT DF_SSch_St DEFAULT 'OPEN' CONSTRAINT CK_SSch_St CHECK (Status IN ('OPEN','PARTIAL','COMPLETE','CANCELLED')),
  CONSTRAINT UQ_DeliverySchedule UNIQUE (SalesOrderLineId, ScheduleNo));
GO
