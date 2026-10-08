/*  cst.ItemCost / cst.ItemCostHistory / cst.CostLayer - carrying value per valuation pool, the
    moving-weighted-average recalculation history and the FIFO receipt layers.
    REQ: CST-005, CST-006, CST-022, ITM-024, D-04 */
SET NOCOUNT ON;
IF OBJECT_ID(N'cst.ItemCost') IS NULL CREATE TABLE cst.ItemCost (
  ItemCostId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_ItemCost PRIMARY KEY, CompanyCode varchar(10) NOT NULL, ItemCode varchar(40) NOT NULL, Revision varchar(4) NOT NULL CONSTRAINT DF_ItmC_Rev DEFAULT '',
  SiteCode varchar(10) NOT NULL CONSTRAINT DF_ItmC_Site DEFAULT '*', CostMethod varchar(4) NOT NULL CONSTRAINT CK_ItmC_M CHECK (CostMethod IN ('MWA','FIFO','STD')), QtyOnHand decimal(19,6) NOT NULL CONSTRAINT DF_ItmC_Q DEFAULT 0,
  CarryingValueGBP decimal(18,2) NOT NULL CONSTRAINT DF_ItmC_V DEFAULT 0, AverageUnitCost decimal(19,6) NOT NULL CONSTRAINT DF_ItmC_A DEFAULT 0, StandardUnitCost decimal(19,6) NULL, LastPurchaseCost decimal(19,6) NULL,
  NrvUnitValue decimal(19,6) NULL, AllowanceGBP decimal(18,2) NOT NULL CONSTRAINT DF_ItmC_Al DEFAULT 0, LastUpdatedAt datetime2(3) NULL, LastUpdatedBy varchar(20) NULL, RowVer rowversion,
  CONSTRAINT UQ_ItemCost UNIQUE (CompanyCode, ItemCode, Revision, SiteCode));
GO
IF OBJECT_ID(N'cst.ItemCostHistory') IS NULL CREATE TABLE cst.ItemCostHistory (
  CostHistoryId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_ItemCostHistory PRIMARY KEY, CompanyCode varchar(10) NOT NULL, ItemCode varchar(40) NOT NULL, Revision varchar(4) NOT NULL CONSTRAINT DF_ItmCH_Rev DEFAULT '',
  EffectiveAt datetime2(3) NOT NULL CONSTRAINT DF_ItmCH_EA DEFAULT SYSDATETIME(), MovementId bigint NULL,
  ChangeType varchar(14) NOT NULL CONSTRAINT CK_ItmCH_Ct CHECK (ChangeType IN ('RECEIPT','ISSUE','REVALUE','LANDEDCOST','PRICEADJUST','OPENING','WRITEDOWN','STDACTIVATE')), QtyBefore decimal(19,6) NOT NULL, ValueBeforeGBP decimal(18,2) NOT NULL,
  AvgBefore decimal(19,6) NOT NULL, QtyChange decimal(19,6) NOT NULL, ValueChangeGBP decimal(18,2) NOT NULL, QtyAfter decimal(19,6) NOT NULL, ValueAfterGBP decimal(18,2) NOT NULL, AvgAfter decimal(19,6) NOT NULL, SourceDocType varchar(20) NULL,
  SourceDocRef varchar(40) NULL, CreatedBy varchar(20) NULL);
GO
IF OBJECT_ID(N'cst.CostLayer') IS NULL CREATE TABLE cst.CostLayer (
  LayerId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_CostLayer PRIMARY KEY, CompanyCode varchar(10) NOT NULL, ItemCode varchar(40) NOT NULL, Revision varchar(4) NOT NULL CONSTRAINT DF_Lay_Rev DEFAULT '', LayerSeq bigint NOT NULL,
  ReceiptDate date NOT NULL, SourceDocType varchar(20) NULL, SourceDocRef varchar(40) NULL, MovementId bigint NULL, LotCode varchar(30) NULL, QtyOriginal decimal(19,6) NOT NULL,
  QtyRemaining decimal(19,6) NOT NULL CONSTRAINT CK_Lay_Qr CHECK (QtyRemaining >= 0), UnitCost decimal(19,6) NOT NULL, ValueRemainingGBP decimal(18,2) NOT NULL CONSTRAINT DF_Lay_Vr DEFAULT 0,
  LandedAdjustmentGBP decimal(18,2) NOT NULL CONSTRAINT DF_Lay_La DEFAULT 0, IsClosed bit NOT NULL CONSTRAINT DF_Lay_Cl DEFAULT 0, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Lay_CA DEFAULT SYSDATETIME(), RowVer rowversion,
  CONSTRAINT UQ_CostLayer UNIQUE (CompanyCode, ItemCode, Revision, LayerSeq));
GO
