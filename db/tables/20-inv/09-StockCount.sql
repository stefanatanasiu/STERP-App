/*  inv.StockCount / inv.StockCountLine - controlled count scope, recount and approved adjustment.
    REQ: ITM-018, FIN-022 */
SET NOCOUNT ON;
IF OBJECT_ID(N'inv.StockCount') IS NULL CREATE TABLE inv.StockCount (
  CountId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_StockCount PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, CountRef varchar(30) NOT NULL,
  CountType varchar(10) NOT NULL CONSTRAINT DF_Cnt_Type DEFAULT 'CYCLE' CONSTRAINT CK_Cnt_Type CHECK (CountType IN ('CYCLE','FULL','SPOT')), ScopeDescription varchar(200) NULL, WarehouseCode varchar(20) NULL, CutoffAt datetime2(3) NULL,
  IsFrozen bit NOT NULL CONSTRAINT DF_Cnt_Fr DEFAULT 0, Status varchar(12) NOT NULL CONSTRAINT DF_Cnt_St DEFAULT 'OPEN' CONSTRAINT CK_Cnt_St CHECK (Status IN ('OPEN','COUNTED','RECOUNT','APPROVED','POSTED','CANCELLED')), CountedBy varchar(20) NULL,
  ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Cnt_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT UQ_StockCount UNIQUE (CompanyCode, CountRef));
GO
IF OBJECT_ID(N'inv.StockCountLine') IS NULL CREATE TABLE inv.StockCountLine (
  CountLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_StockCountLine PRIMARY KEY, CountId int NOT NULL REFERENCES inv.StockCount(CountId), LineNum int NOT NULL, ItemCode varchar(40) NOT NULL, Revision varchar(4) NULL,
  WarehouseCode varchar(20) NOT NULL, LocationCode varchar(20) NOT NULL, LotCode varchar(30) NULL, SerialNo varchar(40) NULL, ExpectedQty decimal(19,6) NOT NULL, CountedQty decimal(19,6) NULL, RecountQty decimal(19,6) NULL,
  InterveningMovementQty decimal(19,6) NOT NULL CONSTRAINT DF_CntL_Im DEFAULT 0, AdjustmentQty decimal(19,6) NULL, AdjustmentValueGBP decimal(18,2) NULL, ReasonCode varchar(20) NULL, MovementId bigint NULL,
  Status varchar(10) NOT NULL CONSTRAINT DF_CntL_St DEFAULT 'OPEN', RowVer rowversion, CONSTRAINT UQ_StockCountLine UNIQUE (CountId, LineNum));
GO
