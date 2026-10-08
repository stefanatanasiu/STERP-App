/*  ap.PurchaseInvoice / ap.PurchaseInvoiceLine / ap.PurchaseInvoiceVat / ap.InvoiceMatch -
    supplier invoice register, coding, three-way matching and VAT evidence.
    REQ: AP-003..AP-010, AP-021, AP-025, AP-029, AP-030, AP-031, D-08 */
SET NOCOUNT ON;
IF OBJECT_ID(N'ap.PurchaseInvoice') IS NULL CREATE TABLE ap.PurchaseInvoice (
  PurchaseInvoiceId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_PurchaseInvoice PRIMARY KEY, CompanyCode varchar(10) NOT NULL, DocumentType varchar(12) NOT NULL CONSTRAINT CK_PInv_Dt CHECK (DocumentType IN ('INVOICE','CREDITNOTE')),
  RegisterNumber varchar(30) NOT NULL, SupplierCode varchar(20) NOT NULL, SupplierDocRef varchar(40) NOT NULL, SupplierDocRefNormalised varchar(40) NOT NULL, DateReceived date NOT NULL, DocumentDate date NOT NULL, TaxPointDate date NOT NULL,
  PostingDate date NULL, DueDate date NULL, FiscalYear smallint NULL, PeriodNo tinyint NULL, VatPeriodKey varchar(10) NULL, CurrencyCode char(3) NOT NULL CONSTRAINT DF_PInv_Cur DEFAULT 'GBP',
  FxRate decimal(18,8) NOT NULL CONSTRAINT DF_PInv_Fx DEFAULT 1, FxRateDate date NULL, StatedNet decimal(18,2) NOT NULL CONSTRAINT DF_PInv_Sn DEFAULT 0, StatedVat decimal(18,2) NOT NULL CONSTRAINT DF_PInv_Sv DEFAULT 0,
  StatedGross decimal(18,2) NOT NULL CONSTRAINT DF_PInv_Sg DEFAULT 0, NetAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_PInv_Ng DEFAULT 0, VatAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_PInv_Vg DEFAULT 0,
  GrossAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_PInv_Gg DEFAULT 0, RecoverableVatGBP decimal(18,2) NOT NULL CONSTRAINT DF_PInv_Rv DEFAULT 0, NonRecoverableVatGBP decimal(18,2) NOT NULL CONSTRAINT DF_PInv_Nv DEFAULT 0,
  VatEvidenceValid bit NOT NULL CONSTRAINT DF_PInv_Ve DEFAULT 1, VatEvidenceNote varchar(200) NULL, TermsCode varchar(12) NULL, TermsSnapshot varchar(100) NULL, TermsOverrideReason varchar(200) NULL,
  RegistrationState varchar(14) NOT NULL CONSTRAINT DF_PInv_Rs DEFAULT 'CAPTURED' CONSTRAINT CK_PInv_Rs CHECK (RegistrationState IN ('CAPTURED','INCOMPLETE','READY','DUPLICATE','REJECTED')),
  MatchState varchar(14) NOT NULL CONSTRAINT DF_PInv_Ms DEFAULT 'UNMATCHED' CONSTRAINT CK_PInv_Ms CHECK (MatchState IN ('NOTREQUIRED','UNMATCHED','PARTIAL','MATCHED','VARIANCE')),
  AccountingState varchar(14) NOT NULL CONSTRAINT DF_PInv_As DEFAULT 'UNPOSTED' CONSTRAINT CK_PInv_As CHECK (AccountingState IN ('UNPOSTED','AWAITAPPROVAL','POSTED','PARTREVERSED','REVERSED')),
  PaymentEligibility varchar(14) NOT NULL CONSTRAINT DF_PInv_Pe DEFAULT 'BLOCKED' CONSTRAINT CK_PInv_Pe CHECK (PaymentEligibility IN ('BLOCKED','APPROVED','PARTRESERVED','FULLRESERVED')),
  SettlementState varchar(12) NOT NULL CONSTRAINT DF_PInv_Ss DEFAULT 'UNPAID' CONSTRAINT CK_PInv_Ss CHECK (SettlementState IN ('UNPAID','PARTPAID','SETTLED','RETURNED')), BlockReason varchar(200) NULL, OwnerUser varchar(20) NULL,
  ApprovalId bigint NULL, ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL, IsNonPo bit NOT NULL CONSTRAINT DF_PInv_Npo DEFAULT 0, NonPoReason varchar(60) NULL, PendingMatchClearingGBP decimal(18,2) NOT NULL CONSTRAINT DF_PInv_Pm DEFAULT 0,
  VatRepaidAt date NULL, VatRepaidVatPeriodKey varchar(10) NULL, VatReclaimedAgainAt date NULL, LateInterestClaimedGBP decimal(18,2) NOT NULL CONSTRAINT DF_PInv_Li DEFAULT 0, LateCompensationGBP decimal(18,2) NOT NULL CONSTRAINT DF_PInv_Lc DEFAULT 0,
  AttachmentRef varchar(200) NULL, EInvoiceRef varchar(60) NULL, JournalId bigint NULL, CorrelationId varchar(64) NULL, RetentionClass varchar(16) NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_PInv_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_PurchaseInvoice UNIQUE (CompanyCode, RegisterNumber),
  CONSTRAINT UQ_PurchaseInvoice_Dup UNIQUE (CompanyCode, SupplierCode, DocumentType, SupplierDocRefNormalised));
GO
IF OBJECT_ID(N'ap.PurchaseInvoiceLine') IS NULL CREATE TABLE ap.PurchaseInvoiceLine (
  PurchaseInvoiceLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_PurchaseInvoiceLine PRIMARY KEY, PurchaseInvoiceId bigint NOT NULL REFERENCES ap.PurchaseInvoice(PurchaseInvoiceId), LineNum int NOT NULL, Description varchar(200) NOT NULL,
  ItemCode varchar(40) NULL, PurchaseOrderNumber varchar(30) NULL, PurchaseOrderLine int NULL, ReceiptRef varchar(30) NULL, ReceiptLine int NULL, ServiceAcceptanceRef varchar(30) NULL, Quantity decimal(19,6) NULL, UomCode varchar(10) NULL,
  ConversionFactor decimal(19,9) NULL, StockQuantity decimal(19,6) NULL, UnitPrice decimal(19,6) NULL, PricePerQty decimal(19,6) NOT NULL CONSTRAINT DF_PInvL_Pp DEFAULT 1, ChargeAmount decimal(18,2) NOT NULL CONSTRAINT DF_PInvL_Ch DEFAULT 0,
  NetAmount decimal(18,2) NOT NULL, VatCode varchar(4) NOT NULL, VatRatePct decimal(9,4) NOT NULL, VatAmount decimal(18,2) NOT NULL, NetAmountGBP decimal(18,2) NOT NULL, VatAmountGBP decimal(18,2) NOT NULL,
  IsRecoverableVat bit NOT NULL CONSTRAINT DF_PInvL_Rv DEFAULT 1, AccountCode varchar(12) NULL, DimCostCentre varchar(20) NULL, DimProject varchar(20) NULL, JobRef varchar(20) NULL,
  CostNature varchar(10) NOT NULL CONSTRAINT DF_PInvL_Cn DEFAULT 'EXPENSE' CONSTRAINT CK_PInvL_Cn CHECK (CostNature IN ('STOCK','EXPENSE','ASSET','WIP','LANDED','DUTY')), LandedCostId int NULL,
  CONSTRAINT UQ_PurchaseInvoiceLine UNIQUE (PurchaseInvoiceId, LineNum));
GO
IF OBJECT_ID(N'ap.PurchaseInvoiceVat') IS NULL CREATE TABLE ap.PurchaseInvoiceVat (
  PurchaseInvoiceId bigint NOT NULL REFERENCES ap.PurchaseInvoice(PurchaseInvoiceId), VatCode varchar(4) NOT NULL, RatePct decimal(9,4) NOT NULL, NetAmount decimal(18,2) NOT NULL, VatAmount decimal(18,2) NOT NULL, NetAmountGBP decimal(18,2) NOT NULL,
  VatAmountGBP decimal(18,2) NOT NULL, RecoverableGBP decimal(18,2) NOT NULL CONSTRAINT DF_PInvV_Rec DEFAULT 0, VatBox tinyint NULL, NetBox tinyint NULL, CONSTRAINT PK_PurchaseInvoiceVat PRIMARY KEY (PurchaseInvoiceId, VatCode));
GO
IF OBJECT_ID(N'ap.InvoiceMatch') IS NULL CREATE TABLE ap.InvoiceMatch (
  MatchId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_InvoiceMatch PRIMARY KEY, CompanyCode varchar(10) NOT NULL, PurchaseInvoiceId bigint NOT NULL, PurchaseInvoiceLineNum int NOT NULL, PurchaseOrderNumber varchar(30) NULL,
  PurchaseOrderLine int NULL, ReceiptRef varchar(30) NULL, ReceiptLine int NULL, ItemCode varchar(40) NULL, OrderedQty decimal(19,6) NULL, ReceivedAcceptedQty decimal(19,6) NULL, ReturnedQty decimal(19,6) NOT NULL CONSTRAINT DF_Mat_Rq DEFAULT 0,
  PreviouslyMatchedQty decimal(19,6) NOT NULL CONSTRAINT DF_Mat_Pq DEFAULT 0, MatchedQty decimal(19,6) NOT NULL, OrderUnitPrice decimal(19,6) NULL, InvoiceUnitPrice decimal(19,6) NULL,
  QtyVariance decimal(19,6) NOT NULL CONSTRAINT DF_Mat_Qv DEFAULT 0, PriceVariancePct decimal(9,4) NULL, PriceVarianceGBP decimal(18,2) NOT NULL CONSTRAINT DF_Mat_Pv DEFAULT 0, ChargeVarianceGBP decimal(18,2) NOT NULL CONSTRAINT DF_Mat_Cv DEFAULT 0,
  VatVarianceGBP decimal(18,2) NOT NULL CONSTRAINT DF_Mat_Vv DEFAULT 0, GrniClearedGBP decimal(18,2) NOT NULL CONSTRAINT DF_Mat_Gc DEFAULT 0, WithinTolerance bit NOT NULL CONSTRAINT DF_Mat_Wt DEFAULT 1, ToleranceNote varchar(200) NULL,
  ApprovalId bigint NULL, ApprovedBy varchar(20) NULL, Status varchar(12) NOT NULL CONSTRAINT DF_Mat_St DEFAULT 'PROPOSED' CONSTRAINT CK_Mat_St CHECK (Status IN ('PROPOSED','MATCHED','DISPUTED','REVERSED')), CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Mat_CA DEFAULT SYSDATETIME(), RowVer rowversion);
GO
