/*  cst.LandedCost / cst.LandedCostAllocation / cst.Impairment - attributable acquisition charges
    (freight, duty, clearance) allocated once, and FRS 102 lower-of-cost-and-NRV write-downs.
    REQ: CST-008, CST-021, CST-025, PO-015, PO-016 */
SET NOCOUNT ON;
IF OBJECT_ID(N'cst.LandedCost') IS NULL CREATE TABLE cst.LandedCost (
  LandedCostId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_LandedCost PRIMARY KEY, CompanyCode varchar(10) NOT NULL, ChargeRef varchar(30) NOT NULL, ShipmentRef varchar(30) NULL, ShipmentDate date NULL, SupplierCode varchar(20) NULL,
  ImportEntryRef varchar(30) NULL, Category varchar(14) NOT NULL CONSTRAINT CK_Lnd_Cat CHECK (Category IN ('FREIGHT','INSURANCE','DUTY','CLEARANCE','HANDLING','OTHER')), IsEligibleToCapitalise bit NOT NULL CONSTRAINT DF_Lnd_Cap DEFAULT 1,
  AllocationBasis varchar(10) NOT NULL CONSTRAINT CK_Lnd_Bas CHECK (AllocationBasis IN ('VALUE','WEIGHT','VOLUME','QUANTITY','EXPLICIT')), CurrencyCode char(3) NOT NULL CONSTRAINT DF_Lnd_Cur DEFAULT 'GBP',
  FxRate decimal(18,8) NOT NULL CONSTRAINT DF_Lnd_Fx DEFAULT 1, EstimatedAmount decimal(18,2) NOT NULL CONSTRAINT DF_Lnd_Est DEFAULT 0, EstimatedAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_Lnd_EstG DEFAULT 0, ActualAmount decimal(18,2) NULL,
  ActualAmountGBP decimal(18,2) NULL, AllocatedGBP decimal(18,2) NOT NULL CONSTRAINT DF_Lnd_Alloc DEFAULT 0, AllocationDate date NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_Lnd_St DEFAULT 'ESTIMATED' CONSTRAINT CK_Lnd_St CHECK (Status IN ('ESTIMATED','ALLOCATED','ACTUALISED','CANCELLED')), SupplierInvoiceRef varchar(40) NULL, ApprovedBy varchar(20) NULL, JournalId bigint NULL,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Lnd_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_LandedCost UNIQUE (CompanyCode, ChargeRef));
GO
IF OBJECT_ID(N'cst.LandedCostAllocation') IS NULL CREATE TABLE cst.LandedCostAllocation (
  AllocationId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_LandedCostAllocation PRIMARY KEY, LandedCostId int NOT NULL REFERENCES cst.LandedCost(LandedCostId), LineNum int NOT NULL, ReceiptRef varchar(30) NOT NULL, ReceiptLine int NULL,
  ItemCode varchar(40) NOT NULL, LotCode varchar(30) NULL, BasisValue decimal(19,6) NOT NULL, BasisShare decimal(19,9) NOT NULL, EstimatedShareGBP decimal(18,2) NOT NULL CONSTRAINT DF_LndA_E DEFAULT 0, ActualShareGBP decimal(18,2) NULL,
  DeltaGBP decimal(18,2) NOT NULL CONSTRAINT DF_LndA_D DEFAULT 0, QtyRemaining decimal(19,6) NULL, QtyConsumed decimal(19,6) NULL, ToInventoryGBP decimal(18,2) NOT NULL CONSTRAINT DF_LndA_Inv DEFAULT 0,
  ToCogsGBP decimal(18,2) NOT NULL CONSTRAINT DF_LndA_Cog DEFAULT 0, ToWipGBP decimal(18,2) NOT NULL CONSTRAINT DF_LndA_Wip DEFAULT 0, ToVarianceGBP decimal(18,2) NOT NULL CONSTRAINT DF_LndA_Var DEFAULT 0, PostedAt datetime2(3) NULL,
  CONSTRAINT UQ_LandedCostAllocation UNIQUE (LandedCostId, LineNum));
GO
IF OBJECT_ID(N'cst.Impairment') IS NULL CREATE TABLE cst.Impairment (
  ImpairmentId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_Impairment PRIMARY KEY, CompanyCode varchar(10) NOT NULL, ItemCode varchar(40) NOT NULL, Revision varchar(4) NULL, SiteCode varchar(10) NULL, AssessmentDate date NOT NULL,
  Quantity decimal(19,6) NOT NULL, CarryingUnitCost decimal(19,6) NOT NULL, EstimatedSellingPrice decimal(19,6) NULL, CostsToCompleteAndSell decimal(19,6) NULL, NrvUnitValue decimal(19,6) NOT NULL, WriteDownGBP decimal(18,2) NOT NULL,
  ReasonCode varchar(20) NULL, Evidence varchar(300) NULL, IsReversal bit NOT NULL CONSTRAINT DF_Imp_Rev DEFAULT 0, ReversalOfImpairmentId int NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_Imp_St DEFAULT 'PROPOSED' CONSTRAINT CK_Imp_St CHECK (Status IN ('PROPOSED','APPROVED','POSTED','REJECTED')), ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL, JournalId bigint NULL,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Imp_CA DEFAULT SYSDATETIME(), RowVer rowversion);
GO
