/*  ap.OpenItem / ap.Allocation - purchase-ledger open items, advances and settlement allocation.
    REQ: AP-014, AP-017, AP-018, AP-020, AP-022, AP-024 */
SET NOCOUNT ON;
IF OBJECT_ID(N'ap.OpenItem') IS NULL CREATE TABLE ap.OpenItem (
  OpenItemId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_ApOpenItem PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SupplierCode varchar(20) NOT NULL,
  ItemType varchar(12) NOT NULL CONSTRAINT CK_ApOI_Type CHECK (ItemType IN ('INVOICE','CREDIT','PAYMENT','ADVANCE','ADJUSTMENT','INTEREST')), DocumentNumber varchar(30) NOT NULL, SupplierDocRef varchar(40) NULL, DocumentDate date NOT NULL,
  DueDate date NOT NULL, PostingDate date NOT NULL, DateReceived date NULL, FiscalYear smallint NULL, PeriodNo tinyint NULL, CurrencyCode char(3) NOT NULL CONSTRAINT DF_ApOI_Cur DEFAULT 'GBP',
  FxRate decimal(18,8) NOT NULL CONSTRAINT DF_ApOI_Fx DEFAULT 1, OriginalAmount decimal(18,2) NOT NULL, OriginalAmountGBP decimal(18,2) NOT NULL, ResidualAmount decimal(18,2) NOT NULL, ResidualAmountGBP decimal(18,2) NOT NULL,
  RevaluationGBP decimal(18,2) NOT NULL CONSTRAINT DF_ApOI_Rv DEFAULT 0, ReservedAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_ApOI_Res DEFAULT 0, TermsCode varchar(12) NULL, DiscountDueDate date NULL, DiscountAmount decimal(18,2) NULL,
  SettlementStatus varchar(12) NOT NULL CONSTRAINT DF_ApOI_Ss DEFAULT 'OPEN' CONSTRAINT CK_ApOI_Ss CHECK (SettlementStatus IN ('OPEN','PARTIAL','SETTLED','REVERSED')), IsBlocked bit NOT NULL CONSTRAINT DF_ApOI_Blk DEFAULT 0,
  BlockReason varchar(200) NULL, IsDisputed bit NOT NULL CONSTRAINT DF_ApOI_Dis DEFAULT 0, PurchaseInvoiceId bigint NULL, PaymentInstructionId bigint NULL, JournalId bigint NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ApOI_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_ApOpenItem UNIQUE (CompanyCode, ItemType, DocumentNumber));
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_ApOpenItem_Supplier') CREATE INDEX IX_ApOpenItem_Supplier ON ap.OpenItem(CompanyCode, SupplierCode, SettlementStatus, DueDate);
GO
IF OBJECT_ID(N'ap.Allocation') IS NULL CREATE TABLE ap.Allocation (
  AllocationId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_ApAllocation PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SupplierCode varchar(20) NOT NULL,
  SourceType varchar(10) NOT NULL CONSTRAINT CK_ApAl_Src CHECK (SourceType IN ('PAYMENT','CREDIT','ADVANCE','ADJUST')), SourceOpenItemId bigint NOT NULL, TargetOpenItemId bigint NOT NULL, PaymentInstructionId bigint NULL,
  RemittanceRef varchar(30) NULL, EffectiveDate date NOT NULL, CurrencyCode char(3) NOT NULL CONSTRAINT DF_ApAl_Cur DEFAULT 'GBP', AppliedAmount decimal(18,2) NOT NULL, AppliedAmountGBP decimal(18,2) NOT NULL,
  DiscountAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_ApAl_Dis DEFAULT 0, DifferenceGBP decimal(18,2) NOT NULL CONSTRAINT DF_ApAl_Df DEFAULT 0, DifferenceReason varchar(200) NULL,
  RealisedFxGBP decimal(18,2) NOT NULL CONSTRAINT DF_ApAl_Fx DEFAULT 0, Status varchar(10) NOT NULL CONSTRAINT DF_ApAl_St DEFAULT 'ACTIVE' CONSTRAINT CK_ApAl_St CHECK (Status IN ('ACTIVE','REVERSED')), ReversalOfAllocationId bigint NULL,
  JournalId bigint NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ApAl_CA DEFAULT SYSDATETIME(), RowVer rowversion);
GO
