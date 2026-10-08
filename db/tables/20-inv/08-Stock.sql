/*  inv.StockBalance / inv.StockMovement / inv.StockReservation / inv.QualityHold.
    Physical balances are only ever changed by inv.usp_PostStockMovement; negative physical stock
    is prohibited (D-09).  REQ: ITM-012..ITM-018, COM-016, XMD-012 */
SET NOCOUNT ON;
IF OBJECT_ID(N'inv.StockBalance') IS NULL CREATE TABLE inv.StockBalance (
  StockBalanceId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_StockBalance PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, WarehouseCode varchar(20) NOT NULL, LocationCode varchar(20) NOT NULL, ItemCode varchar(40) NOT NULL,
  Revision varchar(4) NOT NULL CONSTRAINT DF_StkB_Rev DEFAULT '', LotCode varchar(30) NOT NULL CONSTRAINT DF_StkB_Lot DEFAULT '', SerialNo varchar(40) NOT NULL CONSTRAINT DF_StkB_Ser DEFAULT '',
  OwnerType varchar(10) NOT NULL CONSTRAINT DF_StkB_Ot DEFAULT 'OWNED' CONSTRAINT CK_StkB_Ot CHECK (OwnerType IN ('OWNED','CUSTOMER','SUPPLIER')), OwnerCode varchar(20) NOT NULL CONSTRAINT DF_StkB_Oc DEFAULT '',
  QualityStatus varchar(12) NOT NULL CONSTRAINT DF_StkB_Qs DEFAULT 'ACCEPTED' CONSTRAINT CK_StkB_Qs CHECK (QualityStatus IN ('ACCEPTED','QUARANTINE','INSPECTION','REJECTED','EXPIRED')),
  QtyOnHand decimal(19,6) NOT NULL CONSTRAINT DF_StkB_Qoh DEFAULT 0 CONSTRAINT CK_StkB_Qoh CHECK (QtyOnHand >= 0), QtyReserved decimal(19,6) NOT NULL CONSTRAINT DF_StkB_Qr DEFAULT 0,
  QtyInTransit decimal(19,6) NOT NULL CONSTRAINT DF_StkB_Qt DEFAULT 0, LastMovementAt datetime2(3) NULL, LastCountedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT UQ_StockBalance UNIQUE (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode, Revision, LotCode, SerialNo, OwnerType, OwnerCode, QualityStatus));
GO
IF OBJECT_ID(N'inv.StockMovement') IS NULL CREATE TABLE inv.StockMovement (
  MovementId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_StockMovement PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, WarehouseCode varchar(20) NOT NULL, LocationCode varchar(20) NOT NULL, ItemCode varchar(40) NOT NULL,
  Revision varchar(4) NOT NULL CONSTRAINT DF_StkM_Rev DEFAULT '', LotCode varchar(30) NOT NULL CONSTRAINT DF_StkM_Lot DEFAULT '', SerialNo varchar(40) NOT NULL CONSTRAINT DF_StkM_Ser DEFAULT '',
  OwnerType varchar(10) NOT NULL CONSTRAINT DF_StkM_Ot DEFAULT 'OWNED', OwnerCode varchar(20) NOT NULL CONSTRAINT DF_StkM_Oc DEFAULT '', QualityStatus varchar(12) NOT NULL CONSTRAINT DF_StkM_Qs DEFAULT 'ACCEPTED',
  MovementType varchar(14) NOT NULL CONSTRAINT CK_StkM_Mt CHECK (MovementType IN ('RECEIPT','RETURNIN','ISSUE','DESPATCH','TRANSFEROUT','TRANSFERIN','ADJUSTUP','ADJUSTDOWN','COMPLETION','SCRAP','SUPPRETURN','QUALITY','COUNT','OPENING','DISASSEMBLY')),
  Quantity decimal(19,6) NOT NULL, UomCode varchar(10) NULL, EnteredQuantity decimal(19,6) NULL, ConversionFactor decimal(19,9) NULL, UnitCost decimal(19,6) NULL, ValueGBP decimal(18,2) NULL, CostMethod varchar(4) NULL, MovementDate date NOT NULL,
  PostingDate date NOT NULL, FiscalYear smallint NULL, PeriodNo tinyint NULL, SourceModule varchar(6) NOT NULL, SourceDocType varchar(20) NULL, SourceDocRef varchar(40) NULL, SourceDocLine int NULL, CounterpartLocationCode varchar(20) NULL,
  ReasonCode varchar(20) NULL, Reason varchar(200) NULL, ApprovalRef varchar(40) NULL, JournalId bigint NULL, ReversalOfMovementId bigint NULL, CorrelationId varchar(64) NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_StkM_CA DEFAULT SYSDATETIME());
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_StockMovement_Item') CREATE INDEX IX_StockMovement_Item ON inv.StockMovement(CompanyCode, ItemCode, MovementDate) INCLUDE (Quantity, ValueGBP);
GO
IF OBJECT_ID(N'inv.StockReservation') IS NULL CREATE TABLE inv.StockReservation (
  ReservationId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_StockReservation PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, ItemCode varchar(40) NOT NULL, Revision varchar(4) NULL, WarehouseCode varchar(20) NULL,
  LocationCode varchar(20) NULL, LotCode varchar(30) NULL, SerialNo varchar(40) NULL, DemandType varchar(12) NOT NULL CONSTRAINT CK_StkR_Dt CHECK (DemandType IN ('SALESORDER','KIT','TRANSFER','JOB','OTHER')), DemandRef varchar(40) NOT NULL,
  DemandLine int NULL, DemandScheduleNo int NULL, QtyReserved decimal(19,6) NOT NULL CONSTRAINT CK_StkR_Q CHECK (QtyReserved >= 0), QtyPicked decimal(19,6) NOT NULL CONSTRAINT DF_StkR_Qp DEFAULT 0,
  QtyConsumed decimal(19,6) NOT NULL CONSTRAINT DF_StkR_Qc DEFAULT 0, Firmness varchar(10) NOT NULL CONSTRAINT DF_StkR_F DEFAULT 'FIRM' CONSTRAINT CK_StkR_F CHECK (Firmness IN ('FIRM','SOFT','PLANNED')), RequiredDate date NULL,
  Priority tinyint NOT NULL CONSTRAINT DF_StkR_Pri DEFAULT 5, OwnerUser varchar(20) NULL, Status varchar(10) NOT NULL CONSTRAINT DF_StkR_St DEFAULT 'ACTIVE' CONSTRAINT CK_StkR_St CHECK (Status IN ('ACTIVE','CONSUMED','RELEASED','INVALID')),
  InvalidReason varchar(200) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_StkR_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion);
GO
IF OBJECT_ID(N'inv.QualityHold') IS NULL CREATE TABLE inv.QualityHold (
  HoldId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_QualityHold PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, ItemCode varchar(40) NOT NULL, LotCode varchar(30) NULL, SerialNo varchar(40) NULL,
  LocationCode varchar(20) NULL, Quantity decimal(19,6) NOT NULL, HoldType varchar(12) NOT NULL CONSTRAINT CK_QH_Type CHECK (HoldType IN ('INSPECTION','QUARANTINE','REJECT','EXPIRY','RECALL')), ReasonCode varchar(20) NULL,
  Reason varchar(300) NOT NULL, RaisedBy varchar(20) NOT NULL, RaisedAt datetime2(3) NOT NULL CONSTRAINT DF_QH_RA DEFAULT SYSDATETIME(), SourceDocType varchar(20) NULL, SourceDocRef varchar(40) NULL,
  Outcome varchar(12) NULL CONSTRAINT CK_QH_Out CHECK (Outcome IN ('ACCEPT','REJECT','REWORK','RETURN','SCRAP')), OutcomeQty decimal(19,6) NULL, ReleasedBy varchar(20) NULL, ReleasedAt datetime2(3) NULL, AuthorityRef varchar(40) NULL,
  Status varchar(10) NOT NULL CONSTRAINT DF_QH_St DEFAULT 'OPEN' CONSTRAINT CK_QH_St CHECK (Status IN ('OPEN','CLOSED')), RowVer rowversion);
GO
