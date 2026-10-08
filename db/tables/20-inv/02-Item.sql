/*  inv.Item - the governed catalogue identity.  Balances, costs and demand are derived from
    transactions and are never editable here.  REQ: ITM-001..ITM-004, ITM-010, ITM-020, ITM-027, D-21 */
SET NOCOUNT ON;
IF OBJECT_ID(N'inv.Item') IS NULL CREATE TABLE inv.Item (
  CompanyCode varchar(10) NOT NULL REFERENCES core.Company(CompanyCode), ItemCode varchar(40) NOT NULL, Description varchar(100) NOT NULL, LongDescription varchar(300) NULL,
  ItemClass varchar(12) NOT NULL CONSTRAINT CK_Item_Class CHECK (ItemClass IN ('BOUGHT','MADE','NONSTOCK','SERVICE','PHANTOM','KIT','PACKAGING','COSTONLY')), Category varchar(20) NULL, ProductGroup varchar(20) NULL, StewardUser varchar(20) NULL,
  StockUomCode varchar(10) NOT NULL REFERENCES inv.Uom(UomCode), PurchaseUomCode varchar(10) NULL, SalesUomCode varchar(10) NULL, IssueUomCode varchar(10) NULL,
  QuantumQty decimal(19,6) NOT NULL CONSTRAINT DF_Item_Qm DEFAULT 1 CONSTRAINT CK_Item_Qm CHECK (QuantumQty > 0), IsIndivisible bit NOT NULL CONSTRAINT DF_Item_Ind DEFAULT 1, NetWeightKg decimal(19,6) NULL, VolumeM3 decimal(19,6) NULL,
  DrawingRef varchar(40) NULL, SpecificationRef varchar(40) NULL, CurrentRevision varchar(4) NOT NULL CONSTRAINT DF_Item_Rev DEFAULT 'A',
  TraceabilityType varchar(8) NOT NULL CONSTRAINT DF_Item_Tr DEFAULT 'NONE' CONSTRAINT CK_Item_Tr CHECK (TraceabilityType IN ('NONE','LOT','SERIAL')), ShelfLifeDays int NULL, MinRemainingShelfLifeDays int NULL,
  RequiresInspection bit NOT NULL CONSTRAINT DF_Item_Insp DEFAULT 0, MakeBuy varchar(6) NOT NULL CONSTRAINT DF_Item_Mb DEFAULT 'BUY' CONSTRAINT CK_Item_Mb CHECK (MakeBuy IN ('MAKE','BUY','DUAL')),
  ValuationGroup varchar(20) NOT NULL CONSTRAINT DF_Item_Vg DEFAULT 'BOUGHT', CostMethod varchar(4) NOT NULL CONSTRAINT DF_Item_Cm DEFAULT 'MWA' CONSTRAINT CK_Item_Cm CHECK (CostMethod IN ('MWA','FIFO','STD')), StandardBatchQty decimal(19,6) NULL,
  InventoryAccountGroup varchar(20) NULL, RevenueAccountGroup varchar(20) NULL, IsPurchasable bit NOT NULL CONSTRAINT DF_Item_Pur DEFAULT 1, IsSaleable bit NOT NULL CONSTRAINT DF_Item_Sal DEFAULT 1,
  IsManufacturable bit NOT NULL CONSTRAINT DF_Item_Mfg DEFAULT 0, IsConsumable bit NOT NULL CONSTRAINT DF_Item_Con DEFAULT 1, IsStocked bit NOT NULL CONSTRAINT DF_Item_Stk DEFAULT 1, IsDrcSpecifiedGoods bit NOT NULL CONSTRAINT DF_Item_Drc DEFAULT 0,
  UkcaMarked bit NOT NULL CONSTRAINT DF_Item_Ukca DEFAULT 0, ConformityDocRef varchar(40) NULL, DangerousGoodsUn varchar(8) NULL, DangerousGoodsClass varchar(8) NULL, DangerousGoodsPackGroup varchar(4) NULL, ReachStatus varchar(20) NULL,
  WeeeCategory varchar(20) NULL, BatteryCategory varchar(20) NULL,
  LifecycleStatus varchar(10) NOT NULL CONSTRAINT DF_Item_Ls DEFAULT 'DRAFT' CONSTRAINT CK_Item_Ls CHECK (LifecycleStatus IN ('DRAFT','REVIEW','RELEASED','BLOCKED','OBSOLETE','RETIRED')), SuccessorItemCode varchar(40) NULL,
  PredecessorItemCode varchar(40) NULL, StatusReason varchar(200) NULL, ReleasedAt datetime2(3) NULL, ReleasedBy varchar(20) NULL, RetentionClass varchar(16) NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Item_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_Item PRIMARY KEY (CompanyCode, ItemCode),
  CONSTRAINT CK_Item_Code CHECK (ItemCode NOT LIKE '%[^A-Z0-9./_-]%'));
GO
IF OBJECT_ID(N'inv.ItemRevision') IS NULL CREATE TABLE inv.ItemRevision (
  CompanyCode varchar(10) NOT NULL, ItemCode varchar(40) NOT NULL, Revision varchar(4) NOT NULL, Description varchar(100) NULL, EffectiveFrom date NOT NULL, EffectiveTo date NULL, DrawingRef varchar(40) NULL, ChangeReason varchar(200) NULL,
  Status varchar(10) NOT NULL CONSTRAINT DF_ItemRev_St DEFAULT 'RELEASED' CONSTRAINT CK_ItemRev_St CHECK (Status IN ('DRAFT','RELEASED','SUPERSEDED','RETIRED')), IsInterchangeable bit NOT NULL CONSTRAINT DF_ItemRev_Int DEFAULT 1,
  ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ItemRev_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT PK_ItemRevision PRIMARY KEY (CompanyCode, ItemCode, Revision));
GO
