/*  po.PurchaseOrder / po.PurchaseOrderLine / po.PurchaseSchedule - approved supplier commitments
    with acknowledgement evidence and original versus revised promised dates.
    REQ: PO-003..PO-008, PO-017, PO-020, PO-022, PO-027 */
SET NOCOUNT ON;
IF OBJECT_ID(N'po.PurchaseOrder') IS NULL CREATE TABLE po.PurchaseOrder (
  PurchaseOrderId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_PurchaseOrder PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, OrderNumber varchar(30) NOT NULL,
  OrderType varchar(12) NOT NULL CONSTRAINT DF_POrd_Ot DEFAULT 'STANDARD' CONSTRAINT CK_POrd_Ot CHECK (OrderType IN ('STANDARD','BLANKET','CALLOFF','SUBCONTRACT','DROPSHIP','SERVICE')), BlanketOrderNumber varchar(30) NULL,
  SupplierCode varchar(20) NOT NULL, BuyerUser varchar(20) NULL, OrderDate date NOT NULL, CurrencyCode char(3) NOT NULL CONSTRAINT DF_POrd_Cur DEFAULT 'GBP', FxRate decimal(18,8) NOT NULL CONSTRAINT DF_POrd_Fx DEFAULT 1, FxRateDate date NULL,
  TermsCode varchar(12) NULL, TermsSnapshot varchar(100) NULL, TermsExceptionApprovalId bigint NULL, IncotermsRule varchar(3) NULL, IncotermsNamedPlace varchar(60) NULL, OwnershipPassesAt varchar(20) NULL, DeliveryAddressSnapshot varchar(300) NULL,
  TermsAndConditionsVersion varchar(20) NULL, SupplierReference varchar(40) NULL, NetAmount decimal(18,2) NOT NULL CONSTRAINT DF_POrd_Net DEFAULT 0, VatAmount decimal(18,2) NOT NULL CONSTRAINT DF_POrd_Vat DEFAULT 0,
  GrossAmount decimal(18,2) NOT NULL CONSTRAINT DF_POrd_Gr DEFAULT 0, CommitmentGBP decimal(18,2) NOT NULL CONSTRAINT DF_POrd_Cm DEFAULT 0,
  Status varchar(14) NOT NULL CONSTRAINT DF_POrd_St DEFAULT 'DRAFT' CONSTRAINT CK_POrd_St CHECK (Status IN ('DRAFT','AWAITAPPROVAL','APPROVED','ISSUED','ACKNOWLEDGED','AMENDED','CLOSED','CANCELLED')),
  DeliveryStatus varchar(14) NOT NULL CONSTRAINT DF_POrd_Ds DEFAULT 'NOTRECEIVED' CONSTRAINT CK_POrd_Ds CHECK (DeliveryStatus IN ('NOTRECEIVED','PARTRECEIVED','FULLRECEIVED','SHORTCLOSED')),
  MatchStatus varchar(14) NOT NULL CONSTRAINT DF_POrd_Ms DEFAULT 'UNINVOICED' CONSTRAINT CK_POrd_Ms CHECK (MatchStatus IN ('UNINVOICED','PARTINVOICED','FULLINVOICED','DISPUTE')), ApprovalId bigint NULL, ContentHash varchar(64) NULL,
  ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL, IssuedAt datetime2(3) NULL, AcknowledgedAt datetime2(3) NULL, AcknowledgementNote varchar(300) NULL, LinkedSalesOrderNumber varchar(30) NULL, BudgetCode varchar(20) NULL,
  CorrelationId varchar(64) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_POrd_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT UQ_PurchaseOrder UNIQUE (CompanyCode, OrderNumber));
GO
/*  PO-021: a copy keeps a reference to the order it was copied from.  The receipt, approval and
    payment state is deliberately not copied, but the provenance of the new draft is. */
IF COL_LENGTH(N'po.PurchaseOrder', N'CopiedFromOrderNumber') IS NULL
  ALTER TABLE po.PurchaseOrder ADD CopiedFromOrderNumber varchar(30) NULL;
GO
IF OBJECT_ID(N'po.PurchaseOrderLine') IS NULL CREATE TABLE po.PurchaseOrderLine (
  PurchaseOrderLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_PurchaseOrderLine PRIMARY KEY, PurchaseOrderId bigint NOT NULL REFERENCES po.PurchaseOrder(PurchaseOrderId), CompanyCode varchar(10) NOT NULL, OrderNumber varchar(30) NOT NULL,
  LineNum int NOT NULL, ItemCode varchar(40) NULL, Revision varchar(4) NULL, Description varchar(200) NOT NULL,
  LineType varchar(10) NOT NULL CONSTRAINT DF_POrdL_Lt DEFAULT 'STOCK' CONSTRAINT CK_POrdL_Lt CHECK (LineType IN ('STOCK','NONSTOCK','SERVICE','SUBCON','CHARGE','TEXT')), OrderedQty decimal(19,6) NOT NULL,
  CancelledQty decimal(19,6) NOT NULL CONSTRAINT DF_POrdL_Cq DEFAULT 0, ReceivedQty decimal(19,6) NOT NULL CONSTRAINT DF_POrdL_Rq DEFAULT 0, AcceptedQty decimal(19,6) NOT NULL CONSTRAINT DF_POrdL_Aq DEFAULT 0,
  ReturnedQty decimal(19,6) NOT NULL CONSTRAINT DF_POrdL_Xq DEFAULT 0, InvoicedQty decimal(19,6) NOT NULL CONSTRAINT DF_POrdL_Iq DEFAULT 0, UomCode varchar(10) NULL, ConversionFactor decimal(19,9) NOT NULL CONSTRAINT DF_POrdL_Cf DEFAULT 1,
  StockQty decimal(19,6) NULL, UnitPrice decimal(19,6) NOT NULL, PricePerQty decimal(19,6) NOT NULL CONSTRAINT DF_POrdL_Pp DEFAULT 1, PriceSource varchar(20) NULL, DiscountPct decimal(9,4) NOT NULL CONSTRAINT DF_POrdL_D DEFAULT 0,
  ChargeAmount decimal(18,2) NOT NULL CONSTRAINT DF_POrdL_Ch DEFAULT 0, NetAmount decimal(18,2) NOT NULL CONSTRAINT DF_POrdL_Net DEFAULT 0, VatCode varchar(4) NULL, VatRatePct decimal(9,4) NULL,
  ExpectedVatAmount decimal(18,2) NOT NULL CONSTRAINT DF_POrdL_V DEFAULT 0, IsRecoverableVat bit NOT NULL CONSTRAINT DF_POrdL_Rv DEFAULT 1, OverDeliveryTolerancePct decimal(9,4) NULL, AccountCode varchar(12) NULL, DimCostCentre varchar(20) NULL,
  DimProject varchar(20) NULL, JobRef varchar(20) NULL, DestinationSiteCode varchar(10) NULL, DestinationLocationCode varchar(20) NULL, AckUnitPrice decimal(19,6) NULL, AckQty decimal(19,6) NULL, AckNote varchar(200) NULL,
  LinkedSalesOrderNumber varchar(30) NULL, LinkedSalesOrderLine int NULL, RequisitionNumber varchar(30) NULL, ServiceValue decimal(18,2) NULL, AcceptedServiceValue decimal(18,2) NOT NULL CONSTRAINT DF_POrdL_Sv DEFAULT 0,
  Status varchar(12) NOT NULL CONSTRAINT DF_POrdL_St DEFAULT 'OPEN' CONSTRAINT CK_POrdL_St CHECK (Status IN ('OPEN','CLOSED','SHORTCLOSED','CANCELLED')), ShortCloseReason varchar(200) NULL,
  CONSTRAINT UQ_PurchaseOrderLine UNIQUE (PurchaseOrderId, LineNum));
GO
IF OBJECT_ID(N'po.PurchaseSchedule') IS NULL CREATE TABLE po.PurchaseSchedule (
  ScheduleId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_PurchaseSchedule PRIMARY KEY, PurchaseOrderLineId bigint NOT NULL REFERENCES po.PurchaseOrderLine(PurchaseOrderLineId), CompanyCode varchar(10) NOT NULL, OrderNumber varchar(30) NOT NULL,
  LineNum int NOT NULL, ScheduleNo int NOT NULL, Quantity decimal(19,6) NOT NULL, ReceivedQty decimal(19,6) NOT NULL CONSTRAINT DF_PSch_Rq DEFAULT 0, RequestedDate date NOT NULL, OriginalConfirmedDate date NULL, CurrentConfirmedDate date NULL,
  ExpectedArrivalDate date NULL, ChangeReason varchar(200) NULL, ChangeCount int NOT NULL CONSTRAINT DF_PSch_Cc DEFAULT 0, DestinationSiteCode varchar(10) NULL, DestinationLocationCode varchar(20) NULL,
  Status varchar(10) NOT NULL CONSTRAINT DF_PSch_St DEFAULT 'OPEN' CONSTRAINT CK_PSch_St CHECK (Status IN ('OPEN','PARTIAL','COMPLETE','CANCELLED')), CONSTRAINT UQ_PurchaseSchedule UNIQUE (PurchaseOrderLineId, ScheduleNo));
GO
