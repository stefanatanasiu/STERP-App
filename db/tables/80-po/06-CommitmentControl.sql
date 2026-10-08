/*  po.Commitment / po.BlanketBalance / po.OrderAmendment / po.ToleranceRule /
    po.SupplierDiligence / po.SupplierPerformance - the purchase commitment ledger (one row per
    stage movement so commitment, accrual and payable are never added together for the same
    portion), blanket call-off balances, the original versus revised amendment history, the
    D-08 tolerance policy, PO-026 due diligence evidence and PO-023 performance statistics.
    REQ: PO-007, PO-011, PO-017, PO-020, PO-021, PO-022, PO-023, PO-026, D-08, D-29 */
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'po.Commitment') IS NULL CREATE TABLE po.Commitment (
  CommitmentId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_PoCommitment PRIMARY KEY,
  CompanyCode varchar(10) NOT NULL, OrderNumber varchar(30) NOT NULL, LineNum int NOT NULL,
  ScheduleNo int NULL, BudgetCode varchar(20) NULL, AccountCode varchar(12) NULL,
  DimCostCentre varchar(20) NULL, DimProject varchar(20) NULL, FiscalYear smallint NULL, PeriodNo tinyint NULL,
  Stage varchar(12) NOT NULL CONSTRAINT CK_PoCmt_Stg CHECK (Stage IN ('COMMITTED','RELEASED','ACCRUED','INVOICED','CANCELLED')),
  MovementDate date NOT NULL, Quantity decimal(19,6) NOT NULL CONSTRAINT DF_PoCmt_Qt DEFAULT 0,
  AmountGBP decimal(18,2) NOT NULL, NonRecoverableVatGBP decimal(18,2) NOT NULL CONSTRAINT DF_PoCmt_Nv DEFAULT 0,
  ChargeGBP decimal(18,2) NOT NULL CONSTRAINT DF_PoCmt_Ch DEFAULT 0,
  SourceDocType varchar(20) NULL, SourceDocRef varchar(40) NULL, Narrative varchar(200) NULL,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_PoCmt_CA DEFAULT SYSDATETIME());
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = N'IX_PoCommitment_Order' AND object_id = OBJECT_ID(N'po.Commitment'))
  CREATE INDEX IX_PoCommitment_Order ON po.Commitment (CompanyCode, OrderNumber, LineNum, Stage);
GO
IF OBJECT_ID(N'po.BlanketBalance') IS NULL CREATE TABLE po.BlanketBalance (
  BlanketBalanceId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_BlanketBalance PRIMARY KEY,
  CompanyCode varchar(10) NOT NULL, BlanketOrderNumber varchar(30) NOT NULL, LineNum int NOT NULL,
  ItemCode varchar(40) NULL, Description varchar(200) NULL,
  AgreedQty decimal(19,6) NOT NULL CONSTRAINT DF_BlkB_Aq DEFAULT 0,
  CalledOffQty decimal(19,6) NOT NULL CONSTRAINT DF_BlkB_Cq DEFAULT 0,
  AgreedValue decimal(18,2) NOT NULL CONSTRAINT DF_BlkB_Av DEFAULT 0,
  CalledOffValue decimal(18,2) NOT NULL CONSTRAINT DF_BlkB_Cv DEFAULT 0,
  CurrencyCode char(3) NOT NULL CONSTRAINT DF_BlkB_Cu DEFAULT 'GBP',
  ValidFrom date NULL, ValidTo date NULL,
  Status varchar(10) NOT NULL CONSTRAINT DF_BlkB_St DEFAULT 'OPEN'
    CONSTRAINT CK_BlkB_St CHECK (Status IN ('OPEN','EXHAUSTED','EXPIRED','CLOSED')),
  ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT UQ_BlanketBalance UNIQUE (CompanyCode, BlanketOrderNumber, LineNum));
GO
IF OBJECT_ID(N'po.OrderAmendment') IS NULL CREATE TABLE po.OrderAmendment (
  AmendmentId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_OrderAmendment PRIMARY KEY,
  CompanyCode varchar(10) NOT NULL, OrderNumber varchar(30) NOT NULL, LineNum int NULL, ScheduleNo int NULL,
  AmendmentNo int NOT NULL, Source varchar(12) NOT NULL CONSTRAINT DF_PoAmd_Sr DEFAULT 'BUYER'
    CONSTRAINT CK_PoAmd_Sr CHECK (Source IN ('BUYER','ACK','SUPPLIER','SYSTEM')),
  FieldName varchar(40) NOT NULL, OriginalValue varchar(100) NULL, RevisedValue varchar(100) NULL,
  IsMaterial bit NOT NULL CONSTRAINT DF_PoAmd_Mt DEFAULT 0,
  RequiresReapproval bit NOT NULL CONSTRAINT DF_PoAmd_Rr DEFAULT 0, ApprovalId bigint NULL,
  AffectedSalesOrder varchar(30) NULL, AffectedJobRef varchar(20) NULL, OwnerUser varchar(20) NULL,
  ActionNote varchar(300) NULL, Reason varchar(300) NULL, AmendedAt datetime2(3) NOT NULL CONSTRAINT DF_PoAmd_At DEFAULT SYSDATETIME(),
  AmendedBy varchar(20) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_PoAmd_St DEFAULT 'OPEN'
    CONSTRAINT CK_PoAmd_St CHECK (Status IN ('OPEN','ACCEPTED','REJECTED','ACTIONED')),
  CONSTRAINT UQ_OrderAmendment UNIQUE (CompanyCode, OrderNumber, AmendmentNo, FieldName));
GO
IF OBJECT_ID(N'po.ToleranceRule') IS NULL CREATE TABLE po.ToleranceRule (
  ToleranceRuleId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_ToleranceRule PRIMARY KEY,
  CompanyCode varchar(10) NOT NULL, ScopeType varchar(10) NOT NULL CONSTRAINT DF_PoTol_Sc DEFAULT 'COMPANY'
    CONSTRAINT CK_PoTol_Sc CHECK (ScopeType IN ('COMPANY','SUPPLIER','ITEM')),
  ScopeCode varchar(40) NOT NULL CONSTRAINT DF_PoTol_Sv DEFAULT '*',
  OverDeliveryPct decimal(9,4) NOT NULL CONSTRAINT DF_PoTol_Od DEFAULT 0,
  OverDeliveryQty decimal(19,6) NOT NULL CONSTRAINT DF_PoTol_Oq DEFAULT 0,
  UnderDeliveryPct decimal(9,4) NOT NULL CONSTRAINT DF_PoTol_Ud DEFAULT 0,
  PriceVariancePct decimal(9,4) NOT NULL CONSTRAINT DF_PoTol_Pp DEFAULT 2,
  PriceVarianceAbs decimal(18,2) NOT NULL CONSTRAINT DF_PoTol_Pa DEFAULT 50,
  ChargeVarianceAbs decimal(18,2) NOT NULL CONSTRAINT DF_PoTol_Ca DEFAULT 25,
  EarlyDeliveryDays int NOT NULL CONSTRAINT DF_PoTol_Ed DEFAULT 5,
  LateDeliveryDays int NOT NULL CONSTRAINT DF_PoTol_Ld DEFAULT 0,
  ActionOverDelivery varchar(6) NOT NULL CONSTRAINT DF_PoTol_Ao DEFAULT 'BLOCK'
    CONSTRAINT CK_PoTol_Ao CHECK (ActionOverDelivery IN ('WARN','BLOCK')),
  ApproverRole varchar(20) NULL, IsActive bit NOT NULL CONSTRAINT DF_PoTol_Ia DEFAULT 1,
  ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL,
  CONSTRAINT UQ_ToleranceRule UNIQUE (CompanyCode, ScopeType, ScopeCode));
GO
IF OBJECT_ID(N'po.SupplierDiligence') IS NULL CREATE TABLE po.SupplierDiligence (
  DiligenceId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_SupplierDiligence PRIMARY KEY,
  CompanyCode varchar(10) NOT NULL, SupplierCode varchar(20) NOT NULL,
  EvidenceType varchar(20) NOT NULL CONSTRAINT CK_PoDil_Et CHECK (EvidenceType IN
    ('SANCTIONS','COMPANIESHSE','VATNUMBER','BUSINESSSIZE','MODERNSLAVERY','ANTIBRIBERY','INSURANCE','QUALITY')),
  EvidenceRef varchar(100) NULL, Outcome varchar(12) NOT NULL CONSTRAINT DF_PoDil_Oc DEFAULT 'PASS'
    CONSTRAINT CK_PoDil_Oc CHECK (Outcome IN ('PASS','REVIEW','FAIL','EXPIRED','NOTHELD')),
  IsBlocking bit NOT NULL CONSTRAINT DF_PoDil_Bl DEFAULT 0, IsMandatory bit NOT NULL CONSTRAINT DF_PoDil_Mn DEFAULT 1,
  CheckedAt datetime2(3) NOT NULL CONSTRAINT DF_PoDil_Ca DEFAULT SYSDATETIME(), CheckedBy varchar(20) NULL,
  ValidFrom date NULL, ValidTo date NULL, NextReviewDate date NULL, Note varchar(300) NULL,
  ExceptionApprovalId bigint NULL, ExceptionBy varchar(20) NULL, ExceptionReason varchar(300) NULL,
  RowVer rowversion, CONSTRAINT UQ_SupplierDiligence UNIQUE (CompanyCode, SupplierCode, EvidenceType, CheckedAt));
GO
IF OBJECT_ID(N'po.SupplierPerformance') IS NULL CREATE TABLE po.SupplierPerformance (
  PerformanceId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_SupplierPerformance PRIMARY KEY,
  CompanyCode varchar(10) NOT NULL, SupplierCode varchar(20) NOT NULL, PeriodKey varchar(7) NOT NULL,
  MeasureBasis varchar(10) NOT NULL CONSTRAINT DF_PoPrf_Mb DEFAULT 'ORIGINAL'
    CONSTRAINT CK_PoPrf_Mb CHECK (MeasureBasis IN ('ORIGINAL','CURRENT')),
  ToleranceDays int NOT NULL CONSTRAINT DF_PoPrf_Td DEFAULT 0,
  ReceiptCount int NOT NULL CONSTRAINT DF_PoPrf_Rc DEFAULT 0,
  OnTimeCount int NOT NULL CONSTRAINT DF_PoPrf_Ot DEFAULT 0,
  InFullCount int NOT NULL CONSTRAINT DF_PoPrf_If DEFAULT 0,
  OtifCount int NOT NULL CONSTRAINT DF_PoPrf_Oc DEFAULT 0,
  OnTimePct decimal(9,4) NOT NULL CONSTRAINT DF_PoPrf_Op DEFAULT 0,
  InFullPct decimal(9,4) NOT NULL CONSTRAINT DF_PoPrf_Ip DEFAULT 0,
  OtifPct decimal(9,4) NOT NULL CONSTRAINT DF_PoPrf_Op2 DEFAULT 0,
  AcceptedQty decimal(19,6) NOT NULL CONSTRAINT DF_PoPrf_Aq DEFAULT 0,
  RejectedQty decimal(19,6) NOT NULL CONSTRAINT DF_PoPrf_Rq DEFAULT 0,
  QualityAcceptPct decimal(9,4) NOT NULL CONSTRAINT DF_PoPrf_Qa DEFAULT 0,
  PriceVarianceGBP decimal(18,2) NOT NULL CONSTRAINT DF_PoPrf_Pv DEFAULT 0,
  PurchaseValueGBP decimal(18,2) NOT NULL CONSTRAINT DF_PoPrf_Pu DEFAULT 0,
  AvgLeadTimeDays decimal(9,2) NOT NULL CONSTRAINT DF_PoPrf_Al DEFAULT 0,
  QuotedLeadTimeDays decimal(9,2) NOT NULL CONSTRAINT DF_PoPrf_Ql DEFAULT 0,
  CalculatedAt datetime2(3) NOT NULL CONSTRAINT DF_PoPrf_Ct DEFAULT SYSDATETIME(),
  CONSTRAINT UQ_SupplierPerformance UNIQUE (CompanyCode, SupplierCode, PeriodKey, MeasureBasis));
GO
IF COL_LENGTH(N'po.PurchaseOrderLine', N'LandedEstimateGBP') IS NULL
  ALTER TABLE po.PurchaseOrderLine ADD LandedEstimateGBP decimal(18,2) NOT NULL CONSTRAINT DF_POrdL_Lg DEFAULT 0;
GO
IF COL_LENGTH(N'po.PurchaseOrderLine', N'NonRecoverableVatGBP') IS NULL
  ALTER TABLE po.PurchaseOrderLine ADD NonRecoverableVatGBP decimal(18,2) NOT NULL CONSTRAINT DF_POrdL_Nv DEFAULT 0;
GO
IF COL_LENGTH(N'po.Receipt', N'SubcontractJobRef') IS NULL
  ALTER TABLE po.Receipt ADD SubcontractJobRef varchar(20) NULL;
GO
