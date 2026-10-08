/*  ar.OpenItem / ar.Receipt / ar.Allocation - the sales-ledger open-item account.  Balances are
    derived, never edited.  REQ: AR-009..AR-014, AR-017, AR-019, AR-021, D-22 */
SET NOCOUNT ON;
IF OBJECT_ID(N'ar.OpenItem') IS NULL CREATE TABLE ar.OpenItem (
  OpenItemId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_ArOpenItem PRIMARY KEY, CompanyCode varchar(10) NOT NULL, CustomerCode varchar(20) NOT NULL,
  ItemType varchar(12) NOT NULL CONSTRAINT CK_ArOI_Type CHECK (ItemType IN ('INVOICE','CREDIT','RECEIPT','ADJUSTMENT','DEPOSIT','INTEREST','WRITEOFF')), DocumentNumber varchar(30) NOT NULL, DocumentDate date NOT NULL, DueDate date NOT NULL,
  PostingDate date NOT NULL, FiscalYear smallint NULL, PeriodNo tinyint NULL, CurrencyCode char(3) NOT NULL CONSTRAINT DF_ArOI_Cur DEFAULT 'GBP', FxRate decimal(18,8) NOT NULL CONSTRAINT DF_ArOI_Fx DEFAULT 1, OriginalAmount decimal(18,2) NOT NULL,
  OriginalAmountGBP decimal(18,2) NOT NULL, ResidualAmount decimal(18,2) NOT NULL, ResidualAmountGBP decimal(18,2) NOT NULL, RevaluationGBP decimal(18,2) NOT NULL CONSTRAINT DF_ArOI_Rv DEFAULT 0,
  VatAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_ArOI_Vt DEFAULT 0, TermsCode varchar(12) NULL, DiscountDueDate date NULL, DiscountAmount decimal(18,2) NULL,
  SettlementStatus varchar(12) NOT NULL CONSTRAINT DF_ArOI_Ss DEFAULT 'OPEN' CONSTRAINT CK_ArOI_Ss CHECK (SettlementStatus IN ('OPEN','PARTIAL','SETTLED','REVERSED')), IsDisputed bit NOT NULL CONSTRAINT DF_ArOI_Dis DEFAULT 0,
  DisputedAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_ArOI_Da DEFAULT 0, CollectionStatus varchar(12) NOT NULL CONSTRAINT DF_ArOI_Cs DEFAULT 'NORMAL', LastReminderStep tinyint NULL, LastReminderDate date NULL,
  BadDebtReliefClaimed bit NOT NULL CONSTRAINT DF_ArOI_Bd DEFAULT 0, WrittenOff bit NOT NULL CONSTRAINT DF_ArOI_Wo DEFAULT 0, SourceInvoiceId bigint NULL, SourceReceiptId bigint NULL, JournalId bigint NULL,
  RetentionOfTitle bit NOT NULL CONSTRAINT DF_ArOI_Rot DEFAULT 0, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ArOI_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT UQ_ArOpenItem UNIQUE (CompanyCode, ItemType, DocumentNumber));
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_ArOpenItem_Customer') CREATE INDEX IX_ArOpenItem_Customer ON ar.OpenItem(CompanyCode, CustomerCode, SettlementStatus, DueDate);
GO
IF OBJECT_ID(N'ar.Receipt') IS NULL CREATE TABLE ar.Receipt (
  ReceiptId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_ArReceipt PRIMARY KEY, CompanyCode varchar(10) NOT NULL, ReceiptNumber varchar(30) NOT NULL, CustomerCode varchar(20) NULL, PayerCustomerCode varchar(20) NULL, PayerName varchar(120) NULL,
  PaymentMethod varchar(10) NOT NULL CONSTRAINT CK_ArRc_Pm CHECK (PaymentMethod IN ('BACS','FPS','DD','CHAPS','CARD','CHEQUE','CASH','OTHER')), BankAccountCode varchar(20) NOT NULL, ReceiptDate date NOT NULL, ValueDate date NULL,
  PostingDate date NOT NULL, CurrencyCode char(3) NOT NULL CONSTRAINT DF_ArRc_Cur DEFAULT 'GBP', FxRate decimal(18,8) NOT NULL CONSTRAINT DF_ArRc_Fx DEFAULT 1, GrossReceived decimal(18,2) NOT NULL, GrossReceivedGBP decimal(18,2) NOT NULL,
  BankFee decimal(18,2) NOT NULL CONSTRAINT DF_ArRc_Fee DEFAULT 0, AllocatedAmount decimal(18,2) NOT NULL CONSTRAINT DF_ArRc_Al DEFAULT 0, UnallocatedAmount decimal(18,2) NOT NULL CONSTRAINT DF_ArRc_Un DEFAULT 0, BankReference varchar(60) NULL,
  ExternalReference varchar(60) NULL, DirectDebitCollectionId bigint NULL,
  ConfirmationState varchar(12) NOT NULL CONSTRAINT DF_ArRc_Cf DEFAULT 'CONFIRMED' CONSTRAINT CK_ArRc_Cf CHECK (ConfirmationState IN ('EXPECTED','CONFIRMED','DISHONOURED','REVERSED')), DishonouredDate date NULL, DishonourReason varchar(200) NULL,
  ReversalOfReceiptId bigint NULL, Status varchar(12) NOT NULL CONSTRAINT DF_ArRc_St DEFAULT 'POSTED', JournalId bigint NULL, CorrelationId varchar(64) NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ArRc_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_ArReceipt UNIQUE (CompanyCode, ReceiptNumber));
GO
IF OBJECT_ID(N'ar.Allocation') IS NULL CREATE TABLE ar.Allocation (
  AllocationId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_ArAllocation PRIMARY KEY, CompanyCode varchar(10) NOT NULL, CustomerCode varchar(20) NOT NULL,
  SourceType varchar(10) NOT NULL CONSTRAINT CK_ArAl_Src CHECK (SourceType IN ('RECEIPT','CREDIT','DEPOSIT','ADJUST')), SourceOpenItemId bigint NOT NULL, TargetOpenItemId bigint NOT NULL, EffectiveDate date NOT NULL,
  CurrencyCode char(3) NOT NULL CONSTRAINT DF_ArAl_Cur DEFAULT 'GBP', AppliedAmount decimal(18,2) NOT NULL, AppliedAmountGBP decimal(18,2) NOT NULL, DiscountAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_ArAl_Dis DEFAULT 0,
  WriteOffAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_ArAl_Wo DEFAULT 0, WriteOffReason varchar(200) NULL, WriteOffApprovalId bigint NULL, RealisedFxGBP decimal(18,2) NOT NULL CONSTRAINT DF_ArAl_Fx DEFAULT 0,
  IsCrossCurrency bit NOT NULL CONSTRAINT DF_ArAl_Cc DEFAULT 0, ConversionRate decimal(18,8) NULL, Status varchar(10) NOT NULL CONSTRAINT DF_ArAl_St DEFAULT 'ACTIVE' CONSTRAINT CK_ArAl_St CHECK (Status IN ('ACTIVE','REVERSED')),
  ReversalOfAllocationId bigint NULL, ReversedAt datetime2(3) NULL, ReversedBy varchar(20) NULL, JournalId bigint NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ArAl_CA DEFAULT SYSDATETIME(), RowVer rowversion);
GO
