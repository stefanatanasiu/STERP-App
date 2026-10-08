/*  kit.KitPick / kit.KitSupplyCommit - what the warehouse actually took off the shelf for a kit,
    and the named future supply a kit line is covered by.

    A pick is neither a reservation nor an issue (KIT-009): the box has left the shelf but the
    balance has not moved, so the pick is recorded on its own and carries the lot/serial that was
    physically selected.  KITCOMPL reads it back so that genealogy can name the lot that is really
    inside the unit rather than a proportion of everything issued (KIT-019).

    A supply commitment is the second half of KIT-008: a kit line may be covered by eligible stock
    OR by explicitly identified future supply.  Nothing is reserved in inv for a commitment - the
    inbound order is simply spoken for, and the line's outstanding requirement falls by the
    committed quantity so that stock is not reserved twice for the same need.
    REQ: KIT-008, KIT-009, KIT-019 */
SET NOCOUNT ON;
IF OBJECT_ID(N'kit.KitPick') IS NULL CREATE TABLE kit.KitPick (
  KitPickId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_KitPick PRIMARY KEY, CompanyCode varchar(10) NOT NULL, KitRef varchar(30) NOT NULL, KitLineId bigint NULL, LineNum int NULL,
  ComponentItemCode varchar(40) NOT NULL, Revision varchar(4) NULL,
  PickType varchar(10) NOT NULL CONSTRAINT CK_KitPk_Type CHECK (PickType IN ('PICK','SHORTPICK','UNPICK')), PickedQty decimal(19,6) NOT NULL, UomCode varchar(10) NULL,
  WarehouseCode varchar(20) NULL, LocationCode varchar(20) NULL, LotCode varchar(30) NULL, SerialNo varchar(40) NULL, PickDate date NOT NULL,
  ConsumedQty decimal(19,6) NOT NULL CONSTRAINT DF_KitPk_Cq DEFAULT 0, ReasonCode varchar(20) NULL, Reason varchar(300) NULL, PickedBy varchar(20) NULL, CorrelationId varchar(64) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_KitPk_CA DEFAULT SYSDATETIME());
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_KitPick_Line') CREATE INDEX IX_KitPick_Line ON kit.KitPick(CompanyCode, KitRef, LineNum, KitPickId);
GO
IF OBJECT_ID(N'kit.KitSupplyCommit') IS NULL CREATE TABLE kit.KitSupplyCommit (
  KitSupplyCommitId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_KitSupplyCommit PRIMARY KEY, CompanyCode varchar(10) NOT NULL, KitRef varchar(30) NOT NULL, KitLineId bigint NULL, LineNum int NULL,
  ComponentItemCode varchar(40) NOT NULL, Revision varchar(4) NULL,
  SupplyType varchar(12) NOT NULL CONSTRAINT CK_KitSc_Type CHECK (SupplyType IN ('PURCHASE','WORKSORDER','KIT','TRANSFER')), SupplyRef varchar(40) NOT NULL, SupplyLine int NULL,
  CommittedQty decimal(19,6) NOT NULL, UomCode varchar(10) NULL, DueDate date NULL, RequiredDate date NULL,
  Status varchar(10) NOT NULL CONSTRAINT DF_KitSc_St DEFAULT 'ACTIVE' CONSTRAINT CK_KitSc_St CHECK (Status IN ('ACTIVE','RELEASED','CONSUMED')), IsLate bit NOT NULL CONSTRAINT DF_KitSc_Lt DEFAULT 0,
  ReasonCode varchar(20) NULL, Reason varchar(300) NULL, CommittedBy varchar(20) NULL, CorrelationId varchar(64) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_KitSc_CA DEFAULT SYSDATETIME());
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_KitSupplyCommit_Line') CREATE INDEX IX_KitSupplyCommit_Line ON kit.KitSupplyCommit(CompanyCode, KitRef, LineNum, Status);
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_KitSupplyCommit_Supply') CREATE INDEX IX_KitSupplyCommit_Supply ON kit.KitSupplyCommit(CompanyCode, SupplyType, SupplyRef, SupplyLine);
GO
