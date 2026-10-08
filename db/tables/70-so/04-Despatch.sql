/*  so.Despatch / so.DespatchLine - confirmed physical fulfilment, export evidence and the
    point at which control passes and COGS is recognised (D-03).  REQ: SO-012, SO-013, SO-026 */
SET NOCOUNT ON;
IF OBJECT_ID(N'so.Despatch') IS NULL CREATE TABLE so.Despatch (
  DespatchId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_Despatch PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, DespatchRef varchar(30) NOT NULL, CustomerCode varchar(20) NOT NULL, OrderNumber varchar(30) NULL,
  DespatchDate date NOT NULL, PostingDate date NOT NULL, FiscalYear smallint NULL, PeriodNo tinyint NULL, DeliveryAddressSnapshot varchar(300) NULL, IncotermsRule varchar(3) NULL, IncotermsNamedPlace varchar(60) NULL, CarrierCode varchar(20) NULL,
  CarrierReference varchar(60) NULL, PackCount int NULL, GrossWeightKg decimal(19,6) NULL, IsExport bit NOT NULL CONSTRAINT DF_Dsp_Exp DEFAULT 0, DestinationCountry char(2) NULL, CustomerEori varchar(17) NULL, ExportDeclarationRef varchar(40) NULL,
  ProofOfExportType varchar(30) NULL, ProofObtainedDate date NULL, ProofDeadlineDate date NULL,
  ExportEvidenceStatus varchar(12) NOT NULL CONSTRAINT DF_Dsp_Ees DEFAULT 'NA' CONSTRAINT CK_Dsp_Ees CHECK (ExportEvidenceStatus IN ('NA','PENDING','OBTAINED','OVERDUE','FAILED')), DangerousGoodsDeclared bit NOT NULL CONSTRAINT DF_Dsp_Dg DEFAULT 0,
  Status varchar(12) NOT NULL CONSTRAINT DF_Dsp_St DEFAULT 'CONFIRMED' CONSTRAINT CK_Dsp_St CHECK (Status IN ('DRAFT','CONFIRMED','REVERSED')), CogsPostedGBP decimal(18,2) NOT NULL CONSTRAINT DF_Dsp_Cg DEFAULT 0, JournalId bigint NULL,
  IsCutoffReview bit NOT NULL CONSTRAINT DF_Dsp_Co DEFAULT 0, CorrelationId varchar(64) NULL, ConfirmedBy varchar(20) NULL, ConfirmedAt datetime2(3) NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Dsp_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_Despatch UNIQUE (CompanyCode, DespatchRef));
GO
IF OBJECT_ID(N'so.DespatchLine') IS NULL CREATE TABLE so.DespatchLine (
  DespatchLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_DespatchLine PRIMARY KEY, DespatchId bigint NOT NULL REFERENCES so.Despatch(DespatchId), LineNum int NOT NULL, SalesOrderLineId bigint NULL, OrderNumber varchar(30) NULL,
  OrderLineNum int NULL, ScheduleNo int NULL, ItemCode varchar(40) NOT NULL, Revision varchar(4) NULL, Description varchar(200) NULL, PickedQty decimal(19,6) NOT NULL CONSTRAINT DF_DspL_Pq DEFAULT 0, DespatchedQty decimal(19,6) NOT NULL,
  UomCode varchar(10) NULL, ConversionFactor decimal(19,9) NOT NULL CONSTRAINT DF_DspL_Cf DEFAULT 1, StockQty decimal(19,6) NOT NULL, WarehouseCode varchar(20) NULL, LocationCode varchar(20) NULL, LotCode varchar(30) NULL, SerialNo varchar(40) NULL,
  UnitCostGBP decimal(19,6) NULL, CogsGBP decimal(18,2) NOT NULL CONSTRAINT DF_DspL_Cg DEFAULT 0, CostMethod varchar(4) NULL, CommodityCode varchar(10) NULL, CountryOfOrigin char(2) NULL, CustomsValueGBP decimal(18,2) NULL,
  BilledQty decimal(19,6) NOT NULL CONSTRAINT DF_DspL_Bq DEFAULT 0, MovementId bigint NULL, KitRef varchar(30) NULL, CONSTRAINT UQ_DespatchLine UNIQUE (DespatchId, LineNum));
GO
