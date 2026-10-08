/*  po.Receipt / po.ReceiptLine / po.ServiceAcceptance - physical receipt, ownership recognition
    (including stock in transit for imports), inspection routing and evidenced service acceptance.
    REQ: PO-009..PO-012, PO-014, D-03, D-09 */
SET NOCOUNT ON;
IF OBJECT_ID(N'po.Receipt') IS NULL CREATE TABLE po.Receipt (
  ReceiptId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_PoReceipt PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, ReceiptRef varchar(30) NOT NULL, SupplierCode varchar(20) NOT NULL, PurchaseOrderNumber varchar(30) NULL,
  DeliveryNoteRef varchar(40) NULL, PhysicalDate date NOT NULL, PostingDate date NOT NULL, FiscalYear smallint NULL, PeriodNo tinyint NULL, WarehouseCode varchar(20) NULL,
  OwnershipBasis varchar(16) NOT NULL CONSTRAINT DF_PRcp_Ob DEFAULT 'RECEIPT' CONSTRAINT CK_PRcp_Ob CHECK (OwnershipBasis IN ('RECEIPT','INCOTERMS','DROPSHIP')), IsStockInTransit bit NOT NULL CONSTRAINT DF_PRcp_Sit DEFAULT 0,
  ShipmentEvidenceRef varchar(60) NULL, ImportEntryRef varchar(30) NULL, RetentionOfTitle bit NOT NULL CONSTRAINT DF_PRcp_Rot DEFAULT 0, GrniValueGBP decimal(18,2) NOT NULL CONSTRAINT DF_PRcp_Gv DEFAULT 0,
  Status varchar(12) NOT NULL CONSTRAINT DF_PRcp_St DEFAULT 'CONFIRMED' CONSTRAINT CK_PRcp_St CHECK (Status IN ('DRAFT','CONFIRMED','REVERSED')), ReversalOfReceiptId bigint NULL, JournalId bigint NULL, CorrelationId varchar(64) NULL,
  ReceivedBy varchar(20) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_PRcp_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT UQ_PoReceipt UNIQUE (CompanyCode, ReceiptRef));
GO
IF OBJECT_ID(N'po.ReceiptLine') IS NULL CREATE TABLE po.ReceiptLine (
  ReceiptLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_PoReceiptLine PRIMARY KEY, ReceiptId bigint NOT NULL REFERENCES po.Receipt(ReceiptId), LineNum int NOT NULL, PurchaseOrderLineId bigint NULL, PurchaseOrderNumber varchar(30) NULL,
  PurchaseOrderLine int NULL, ScheduleNo int NULL, ItemCode varchar(40) NULL, Revision varchar(4) NULL, Description varchar(200) NULL, ReceivedQty decimal(19,6) NOT NULL, UomCode varchar(10) NULL,
  ConversionFactor decimal(19,9) NOT NULL CONSTRAINT DF_PRcpL_Cf DEFAULT 1, StockQty decimal(19,6) NOT NULL, AcceptedQty decimal(19,6) NOT NULL CONSTRAINT DF_PRcpL_Aq DEFAULT 0, QuarantineQty decimal(19,6) NOT NULL CONSTRAINT DF_PRcpL_Qq DEFAULT 0,
  RejectedQty decimal(19,6) NOT NULL CONSTRAINT DF_PRcpL_Xq DEFAULT 0, ReturnedQty decimal(19,6) NOT NULL CONSTRAINT DF_PRcpL_Rq DEFAULT 0, MatchedQty decimal(19,6) NOT NULL CONSTRAINT DF_PRcpL_Mq DEFAULT 0,
  OverDeliveryQty decimal(19,6) NOT NULL CONSTRAINT DF_PRcpL_Oq DEFAULT 0, LocationCode varchar(20) NULL, SupplierLotRef varchar(40) NULL, LotCode varchar(30) NULL, SerialNo varchar(40) NULL, ExpiryDate date NULL, UnitPrice decimal(19,6) NULL,
  ProvisionalValueGBP decimal(18,2) NOT NULL CONSTRAINT DF_PRcpL_Pv DEFAULT 0, LandedEstimateGBP decimal(18,2) NOT NULL CONSTRAINT DF_PRcpL_Le DEFAULT 0, StandardCostGBP decimal(19,6) NULL,
  PriceVarianceGBP decimal(18,2) NOT NULL CONSTRAINT DF_PRcpL_Pv2 DEFAULT 0, CommodityCode varchar(10) NULL, CountryOfOrigin char(2) NULL, DutyAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_PRcpL_Du DEFAULT 0,
  QualityStatus varchar(12) NOT NULL CONSTRAINT DF_PRcpL_Qs DEFAULT 'ACCEPTED', InspectionRequired bit NOT NULL CONSTRAINT DF_PRcpL_Ir DEFAULT 0, MovementId bigint NULL, CONSTRAINT UQ_PoReceiptLine UNIQUE (ReceiptId, LineNum));
GO
IF OBJECT_ID(N'po.ServiceAcceptance') IS NULL CREATE TABLE po.ServiceAcceptance (
  AcceptanceId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_ServiceAcceptance PRIMARY KEY, CompanyCode varchar(10) NOT NULL, AcceptanceRef varchar(30) NOT NULL, PurchaseOrderNumber varchar(30) NOT NULL, PurchaseOrderLine int NOT NULL,
  SupplierCode varchar(20) NOT NULL, AcceptanceBasis varchar(10) NOT NULL CONSTRAINT CK_SvcAcc_B CHECK (AcceptanceBasis IN ('QUANTITY','MILESTONE','VALUE')), AcceptedQty decimal(19,6) NULL, MilestoneRef varchar(40) NULL,
  AcceptedValue decimal(18,2) NOT NULL, AcceptedValueGBP decimal(18,2) NOT NULL, RemainingCommitmentGBP decimal(18,2) NOT NULL CONSTRAINT DF_SvcAcc_Rc DEFAULT 0, AcceptanceDate date NOT NULL, PostingDate date NULL, Evidence varchar(300) NULL,
  AcceptedBy varchar(20) NOT NULL, MatchedValueGBP decimal(18,2) NOT NULL CONSTRAINT DF_SvcAcc_Mv DEFAULT 0, AccrualJournalId bigint NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_SvcAcc_St DEFAULT 'ACCEPTED' CONSTRAINT CK_SvcAcc_St CHECK (Status IN ('ACCEPTED','REVERSED')), CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_SvcAcc_CA DEFAULT SYSDATETIME(),
  RowVer rowversion, CONSTRAINT UQ_ServiceAcceptance UNIQUE (CompanyCode, AcceptanceRef));
GO
