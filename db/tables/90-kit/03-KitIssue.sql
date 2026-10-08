/*  kit.KitIssue / kit.KitCompletion / kit.KitGenealogy / kit.Disassembly - actual material
    movement onto a kit, good output, component-to-parent traceability and controlled disassembly.
    REQ: KIT-010, KIT-013..KIT-015, KIT-019, KIT-021 */
SET NOCOUNT ON;
IF OBJECT_ID(N'kit.KitIssue') IS NULL CREATE TABLE kit.KitIssue (
  KitIssueId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_KitIssue PRIMARY KEY, CompanyCode varchar(10) NOT NULL, KitRef varchar(30) NOT NULL, KitLineId bigint NULL, LineNum int NULL, ComponentItemCode varchar(40) NOT NULL, Revision varchar(4) NULL,
  IssueType varchar(12) NOT NULL CONSTRAINT CK_KitI_Type CHECK (IssueType IN ('ISSUE','RETURN','SCRAP','BACKFLUSH','RECOVERY')), Quantity decimal(19,6) NOT NULL, UomCode varchar(10) NULL, StockQty decimal(19,6) NOT NULL,
  WarehouseCode varchar(20) NULL, LocationCode varchar(20) NULL, LotCode varchar(30) NULL, SerialNo varchar(40) NULL, ActualDate date NOT NULL, PostingDate date NOT NULL, OperatorUser varchar(20) NULL, UnitCostGBP decimal(19,6) NULL,
  ValueGBP decimal(18,2) NOT NULL CONSTRAINT DF_KitI_V DEFAULT 0, ReasonCode varchar(20) NULL, Reason varchar(300) NULL, ApprovalId bigint NULL, OriginalKitIssueId bigint NULL, MovementId bigint NULL, JournalId bigint NULL,
  CorrelationId varchar(64) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_KitI_CA DEFAULT SYSDATETIME());
GO
IF OBJECT_ID(N'kit.KitCompletion') IS NULL CREATE TABLE kit.KitCompletion (
  CompletionId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_KitCompletion PRIMARY KEY, CompanyCode varchar(10) NOT NULL, KitRef varchar(30) NOT NULL, CompletionSeq int NOT NULL, ParentItemCode varchar(40) NOT NULL, Revision varchar(4) NULL,
  GoodQty decimal(19,6) NOT NULL, RejectedQty decimal(19,6) NOT NULL CONSTRAINT DF_KitC_Rq DEFAULT 0, ScrappedOutputQty decimal(19,6) NOT NULL CONSTRAINT DF_KitC_Sq DEFAULT 0, LotCode varchar(30) NULL, SerialNo varchar(40) NULL,
  WarehouseCode varchar(20) NULL, LocationCode varchar(20) NULL, CompletionDate date NOT NULL, PostingDate date NOT NULL, CostBasis varchar(4) NULL, UnitCostGBP decimal(19,6) NULL,
  WipTransferGBP decimal(18,2) NOT NULL CONSTRAINT DF_KitC_Wt DEFAULT 0, IsFinalCompletion bit NOT NULL CONSTRAINT DF_KitC_Fc DEFAULT 0, ApprovedBy varchar(20) NULL, MovementId bigint NULL, JournalId bigint NULL, CorrelationId varchar(64) NULL,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_KitC_CA DEFAULT SYSDATETIME(), CONSTRAINT UQ_KitCompletion UNIQUE (CompanyCode, KitRef, CompletionSeq));
GO
IF OBJECT_ID(N'kit.KitGenealogy') IS NULL CREATE TABLE kit.KitGenealogy (
  GenealogyId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_KitGenealogy PRIMARY KEY, CompanyCode varchar(10) NOT NULL, KitRef varchar(30) NOT NULL, CompletionId bigint NULL, ParentItemCode varchar(40) NOT NULL, ParentLotCode varchar(30) NULL,
  ParentSerialNo varchar(40) NULL, ComponentItemCode varchar(40) NOT NULL, ComponentLotCode varchar(30) NULL, ComponentSerialNo varchar(40) NULL, ConsumedQty decimal(19,6) NOT NULL, WasSubstitute bit NOT NULL CONSTRAINT DF_KitG_Sub DEFAULT 0,
  PlannedItemCode varchar(40) NULL, KitIssueId bigint NULL, RecordedAt datetime2(3) NOT NULL CONSTRAINT DF_KitG_RA DEFAULT SYSDATETIME());
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_KitGenealogy_Component') CREATE INDEX IX_KitGenealogy_Component ON kit.KitGenealogy(CompanyCode, ComponentItemCode, ComponentLotCode);
GO
IF OBJECT_ID(N'kit.Disassembly') IS NULL CREATE TABLE kit.Disassembly (
  DisassemblyId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_Disassembly PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, DisassemblyRef varchar(30) NOT NULL, ParentItemCode varchar(40) NOT NULL, Revision varchar(4) NULL,
  LotCode varchar(30) NULL, SerialNo varchar(40) NULL, ConsumedQty decimal(19,6) NOT NULL, DisassemblyDate date NOT NULL, PostingDate date NOT NULL, BomHeaderId int NULL, ParentCarryingValueGBP decimal(18,2) NOT NULL,
  ApprovedCostsGBP decimal(18,2) NOT NULL CONSTRAINT DF_Dis_Ac DEFAULT 0, RecoveredValueGBP decimal(18,2) NOT NULL CONSTRAINT DF_Dis_Rv DEFAULT 0, LossGBP decimal(18,2) NOT NULL CONSTRAINT DF_Dis_Ls DEFAULT 0,
  AllocationBasis varchar(12) NOT NULL CONSTRAINT DF_Dis_Ab DEFAULT 'STANDARD', Reason varchar(300) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_Dis_St DEFAULT 'DRAFT' CONSTRAINT CK_Dis_St CHECK (Status IN ('DRAFT','APPROVED','POSTED','CANCELLED')), ApprovedBy varchar(20) NULL, JournalId bigint NULL, CorrelationId varchar(64) NULL,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Dis_CA DEFAULT SYSDATETIME(), RowVer rowversion, CONSTRAINT UQ_Disassembly UNIQUE (CompanyCode, DisassemblyRef));
GO
IF OBJECT_ID(N'kit.DisassemblyLine') IS NULL CREATE TABLE kit.DisassemblyLine (
  DisassemblyLineId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_DisassemblyLine PRIMARY KEY, DisassemblyId int NOT NULL REFERENCES kit.Disassembly(DisassemblyId), LineNum int NOT NULL, ComponentItemCode varchar(40) NOT NULL, Revision varchar(4) NULL,
  LotCode varchar(30) NULL, SerialNo varchar(40) NULL, PlannedQty decimal(19,6) NULL, RecoveredQty decimal(19,6) NOT NULL, DamagedQty decimal(19,6) NOT NULL CONSTRAINT DF_DisL_Dq DEFAULT 0, UomCode varchar(10) NULL,
  QualityStatus varchar(12) NOT NULL CONSTRAINT DF_DisL_Qs DEFAULT 'INSPECTION', AllocatedValueGBP decimal(18,2) NOT NULL CONSTRAINT DF_DisL_Av DEFAULT 0, LocationCode varchar(20) NULL, MovementId bigint NULL,
  CONSTRAINT UQ_DisassemblyLine UNIQUE (DisassemblyId, LineNum));
GO

/*  KIT-020 / AC-081.  kit.KitIssue is the single ledger of every economic event on a kit, so the
    cost enquiry (KITCOST) also writes and reads conversion and relief rows.  The original CHECK
    predated that and only allowed the material dispositions, which made AbsorbedGBP and
    CompletedGBP structurally zero and left RECOVERY in no bucket.  Widened in place, idempotently. */
IF EXISTS (SELECT 1 FROM sys.check_constraints
           WHERE name = N'CK_KitI_Type' AND parent_object_id = OBJECT_ID(N'kit.KitIssue')
             AND definition NOT LIKE N'%SUBSTISSUE%')
    ALTER TABLE kit.KitIssue DROP CONSTRAINT CK_KitI_Type;
GO
IF OBJECT_ID(N'kit.KitIssue') IS NOT NULL
   AND NOT EXISTS (SELECT 1 FROM sys.check_constraints
                   WHERE name = N'CK_KitI_Type' AND parent_object_id = OBJECT_ID(N'kit.KitIssue'))
    ALTER TABLE kit.KitIssue WITH NOCHECK ADD CONSTRAINT CK_KitI_Type
        CHECK (IssueType IN ('ISSUE','SUBSTISSUE','RETURN','SCRAP','BACKFLUSH','RECOVERY',
                             'LABOUR','OVERHEAD','SUBCON','COMPLETE'));
GO
