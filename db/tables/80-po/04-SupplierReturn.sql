/*  po.SupplierReturn / po.SupplierReturnLine / po.ImportEntry / po.ImportEntryLine - returns to
    supplier with expected credit, and customs declaration evidence for imports.
    REQ: PO-013, PO-021, PO-025, FIN-028, D-02 */
SET NOCOUNT ON;
IF OBJECT_ID(N'po.SupplierReturn') IS NULL CREATE TABLE po.SupplierReturn (
  SupplierReturnId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_SupplierReturn PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, ReturnRef varchar(30) NOT NULL, SupplierCode varchar(20) NOT NULL,
  SupplierRmaRef varchar(40) NULL, OriginalReceiptRef varchar(30) NULL, PurchaseOrderNumber varchar(30) NULL, ReturnDate date NOT NULL, PostingDate date NOT NULL, ReasonCode varchar(20) NOT NULL, Reason varchar(300) NULL,
  DespatchEvidence varchar(100) NULL, ExpectedCreditGBP decimal(18,2) NOT NULL CONSTRAINT DF_SupRet_Ec DEFAULT 0, CreditChaseDueDate date NULL, ReplacementExpected bit NOT NULL CONSTRAINT DF_SupRet_Re DEFAULT 0,
  ReplacementOrderNumber varchar(30) NULL, IsPostInvoice bit NOT NULL CONSTRAINT DF_SupRet_Pi DEFAULT 0, CreditNoteRef varchar(40) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_SupRet_St DEFAULT 'OPEN' CONSTRAINT CK_SupRet_St CHECK (Status IN ('OPEN','DESPATCHED','CREDITED','CLOSED','CANCELLED')), JournalId bigint NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_SupRet_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_SupplierReturn UNIQUE (CompanyCode, ReturnRef));
GO
IF OBJECT_ID(N'po.SupplierReturnLine') IS NULL CREATE TABLE po.SupplierReturnLine (
  SupplierReturnLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_SupplierReturnLine PRIMARY KEY, SupplierReturnId bigint NOT NULL REFERENCES po.SupplierReturn(SupplierReturnId), LineNum int NOT NULL, ReceiptRef varchar(30) NULL,
  ReceiptLine int NULL, ItemCode varchar(40) NOT NULL, Revision varchar(4) NULL, LotCode varchar(30) NULL, SerialNo varchar(40) NULL, ReturnedQty decimal(19,6) NOT NULL, UomCode varchar(10) NULL, StockQty decimal(19,6) NOT NULL,
  WasAccepted bit NOT NULL CONSTRAINT DF_SupRetL_Wa DEFAULT 1, AttributableUnitCostGBP decimal(19,6) NULL, ReturnValueGBP decimal(18,2) NOT NULL CONSTRAINT DF_SupRetL_Rv DEFAULT 0,
  ClearingValueGBP decimal(18,2) NOT NULL CONSTRAINT DF_SupRetL_Cv DEFAULT 0, VarianceGBP decimal(18,2) NOT NULL CONSTRAINT DF_SupRetL_Va DEFAULT 0, CreditedQty decimal(19,6) NOT NULL CONSTRAINT DF_SupRetL_Cq DEFAULT 0, MovementId bigint NULL,
  CONSTRAINT UQ_SupplierReturnLine UNIQUE (SupplierReturnId, LineNum));
GO
IF OBJECT_ID(N'po.ImportEntry') IS NULL CREATE TABLE po.ImportEntry (
  ImportEntryId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_ImportEntry PRIMARY KEY, CompanyCode varchar(10) NOT NULL, ImportEntryRef varchar(30) NOT NULL, MovementReferenceNumber varchar(40) NOT NULL, EntryDate date NOT NULL,
  CustomsAgentCode varchar(20) NULL, SupplierCode varchar(20) NULL, PurchaseOrderNumber varchar(30) NULL, ReceiptRef varchar(30) NULL, CustomsValueGBP decimal(18,2) NOT NULL, DutyAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_Imp_Du DEFAULT 0,
  ImportVatGBP decimal(18,2) NOT NULL CONSTRAINT DF_Imp_Iv DEFAULT 0, ImportVatMethod varchar(10) NOT NULL CONSTRAINT DF_Imp_Vm DEFAULT 'PVA' CONSTRAINT CK_Imp_Vm CHECK (ImportVatMethod IN ('PVA','BORDER','DEFERMENT')),
  DefermentAccount varchar(20) NULL, VatPeriodKey varchar(10) NULL, StatementStatus varchar(12) NOT NULL CONSTRAINT DF_Imp_Ss DEFAULT 'PENDING' CONSTRAINT CK_Imp_Ss CHECK (StatementStatus IN ('PENDING','RECONCILED','DIFFERENCE')),
  StatementRef varchar(40) NULL, C79Ref varchar(40) NULL, JournalId bigint NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Imp_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL,
  RowVer rowversion, CONSTRAINT UQ_ImportEntry UNIQUE (CompanyCode, ImportEntryRef));
GO
/*  PO-021: a customs declaration has exactly one movement reference number, so re-loading an
    accepted import cannot raise a second duty / import VAT obligation for the same entry. */
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = N'UQ_ImportEntry_Mrn' AND object_id = OBJECT_ID(N'po.ImportEntry'))
  ALTER TABLE po.ImportEntry ADD CONSTRAINT UQ_ImportEntry_Mrn UNIQUE (CompanyCode, MovementReferenceNumber);
GO
IF OBJECT_ID(N'po.ImportEntryLine') IS NULL CREATE TABLE po.ImportEntryLine (
  ImportEntryLineId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_ImportEntryLine PRIMARY KEY, ImportEntryId int NOT NULL REFERENCES po.ImportEntry(ImportEntryId), LineNum int NOT NULL, ItemCode varchar(40) NULL, ReceiptRef varchar(30) NULL,
  ReceiptLine int NULL, CommodityCode varchar(10) NOT NULL, CountryOfOrigin char(2) NOT NULL, PreferenceClaimed bit NOT NULL CONSTRAINT DF_ImpL_Pc DEFAULT 0, Quantity decimal(19,6) NULL, NetWeightKg decimal(19,6) NULL,
  CustomsValueGBP decimal(18,2) NOT NULL, DutyRatePct decimal(9,4) NOT NULL CONSTRAINT DF_ImpL_Dr DEFAULT 0, DutyAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_ImpL_Da DEFAULT 0, ImportVatGBP decimal(18,2) NOT NULL CONSTRAINT DF_ImpL_Iv DEFAULT 0,
  LandedCostId int NULL, CONSTRAINT UQ_ImportEntryLine UNIQUE (ImportEntryId, LineNum));
GO
