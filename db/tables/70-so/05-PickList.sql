/*  so.PickList / so.PickListLine - picking instructions, controlled short picks and the lot or
    serial actually selected.  Picking is not despatch.  REQ: SO-012, ITM-011 */
SET NOCOUNT ON;
IF OBJECT_ID(N'so.PickList') IS NULL CREATE TABLE so.PickList (
  PickListId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_PickList PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, PickListRef varchar(30) NOT NULL, OrderNumber varchar(30) NULL, CustomerCode varchar(20) NULL,
  WarehouseCode varchar(20) NULL, RequiredDate date NULL, PickerUser varchar(20) NULL, PrintCount int NOT NULL CONSTRAINT DF_Pick_Pc DEFAULT 0,
  Status varchar(12) NOT NULL CONSTRAINT DF_Pick_St DEFAULT 'OPEN' CONSTRAINT CK_Pick_St CHECK (Status IN ('OPEN','PARTPICKED','PICKED','CANCELLED')), CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Pick_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_PickList UNIQUE (CompanyCode, PickListRef));
GO
IF OBJECT_ID(N'so.PickListLine') IS NULL CREATE TABLE so.PickListLine (
  PickLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_PickListLine PRIMARY KEY, PickListId bigint NOT NULL REFERENCES so.PickList(PickListId), LineNum int NOT NULL, SalesOrderLineId bigint NULL, ScheduleNo int NULL, ItemCode varchar(40) NOT NULL,
  Revision varchar(4) NULL, RequiredQty decimal(19,6) NOT NULL, PickedQty decimal(19,6) NOT NULL CONSTRAINT DF_PickL_Pq DEFAULT 0, ShortQty decimal(19,6) NOT NULL CONSTRAINT DF_PickL_Sq DEFAULT 0, WarehouseCode varchar(20) NULL,
  LocationCode varchar(20) NULL, LotCode varchar(30) NULL, SerialNo varchar(40) NULL, ExpiryDate date NULL, ReservationId bigint NULL, SubstituteItemCode varchar(40) NULL, SubstituteApprovalId bigint NULL, ShortReason varchar(200) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_PickL_St DEFAULT 'OPEN', CONSTRAINT UQ_PickListLine UNIQUE (PickListId, LineNum));
GO
