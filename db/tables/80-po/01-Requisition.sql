/*  po.Requisition / po.RequisitionLine - justified purchasing need with budget and demand
    ownership.  A requisition is not a supplier commitment.  REQ: PO-001, PO-005, D-08 */
SET NOCOUNT ON;
IF OBJECT_ID(N'po.Requisition') IS NULL CREATE TABLE po.Requisition (
  RequisitionId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_Requisition PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, RequisitionNumber varchar(30) NOT NULL, RequesterUser varchar(20) NOT NULL,
  Purpose varchar(200) NULL, RequisitionDate date NOT NULL, NeededByDate date NULL, DimCostCentre varchar(20) NULL, DimProject varchar(20) NULL, JobRef varchar(20) NULL, SourceDemandType varchar(12) NULL, SourceDemandRef varchar(40) NULL,
  TotalValueGBP decimal(18,2) NOT NULL CONSTRAINT DF_Req_Tv DEFAULT 0, BudgetCode varchar(20) NULL, BudgetUsedPct decimal(9,4) NULL, BudgetOverrideBy varchar(20) NULL, BudgetOverrideReason varchar(200) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_Req_St DEFAULT 'DRAFT' CONSTRAINT CK_Req_St CHECK (Status IN ('DRAFT','SUBMITTED','APPROVED','REJECTED','PARTCONV','CONVERTED','CANCELLED')), ApprovalId bigint NULL, ApprovedBy varchar(20) NULL,
  ApprovedAt datetime2(3) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Req_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT UQ_Requisition UNIQUE (CompanyCode, RequisitionNumber));
GO
IF OBJECT_ID(N'po.RequisitionLine') IS NULL CREATE TABLE po.RequisitionLine (
  RequisitionLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_RequisitionLine PRIMARY KEY, RequisitionId bigint NOT NULL REFERENCES po.Requisition(RequisitionId), LineNum int NOT NULL, ItemCode varchar(40) NULL, Description varchar(200) NOT NULL,
  Quantity decimal(19,6) NULL, UomCode varchar(10) NULL, EstimatedUnitPrice decimal(19,6) NULL, EstimatedValueGBP decimal(18,2) NOT NULL CONSTRAINT DF_ReqL_Ev DEFAULT 0, SuggestedSupplierCode varchar(20) NULL, NeededByDate date NULL,
  ConvertedQty decimal(19,6) NOT NULL CONSTRAINT DF_ReqL_Cq DEFAULT 0, PurchaseOrderNumber varchar(30) NULL, PurchaseOrderLine int NULL, AccountCode varchar(12) NULL, Status varchar(10) NOT NULL CONSTRAINT DF_ReqL_St DEFAULT 'OPEN',
  CONSTRAINT UQ_RequisitionLine UNIQUE (RequisitionId, LineNum));
GO
