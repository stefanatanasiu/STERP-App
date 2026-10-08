/*  ap.LatePaymentClaim / ap.PaymentPracticesDetail / ap.EInvoiceInbox / ap.VatEvidence -
    the four payables records the baseline tables do not already carry:
      - a supplier's statutory late payment interest and compensation claim, recorded as a
        finance cost linked to the late invoice (AP-030, D-25);
      - the invoice level detail a published payment practices report reproduces from, so that
        changing an invoice after publication cannot alter the report (AP-028, RPT-018);
      - the structured e-invoice inbox, so a Peppol file and a PDF of the same document are
        recognised as one (AP-031, D-26);
      - the input VAT evidence outcome for each invoice, including the six month repayment
        and the later reclaim (AP-025, AP-029).
    REQ: AP-025, AP-028, AP-029, AP-030, AP-031, RPT-018, D-25, D-26 */
SET NOCOUNT ON;
IF OBJECT_ID(N'ap.LatePaymentClaim') IS NULL CREATE TABLE ap.LatePaymentClaim (
  ClaimId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_ApLatePaymentClaim PRIMARY KEY, CompanyCode varchar(10) NOT NULL, ClaimRef varchar(30) NOT NULL, SupplierCode varchar(20) NOT NULL,
  PurchaseInvoiceId bigint NULL, OpenItemId bigint NULL, DocumentNumber varchar(30) NULL, SupplierDocRef varchar(40) NULL, ClaimDate date NOT NULL, DueDate date NOT NULL, PaidDate date NULL,
  DaysLate int NOT NULL CONSTRAINT DF_ApLpc_Dl DEFAULT 0, DebtAmountGBP decimal(18,2) NOT NULL, BaseRatePct decimal(9,4) NOT NULL CONSTRAINT DF_ApLpc_Br DEFAULT 0,
  MarginPct decimal(9,4) NOT NULL CONSTRAINT DF_ApLpc_Mg DEFAULT 8, StatutoryRatePct decimal(9,4) NOT NULL CONSTRAINT DF_ApLpc_Sr DEFAULT 0, InterestGBP decimal(18,2) NOT NULL CONSTRAINT DF_ApLpc_In DEFAULT 0,
  CompensationGBP decimal(18,2) NOT NULL CONSTRAINT DF_ApLpc_Cp DEFAULT 0, ClaimedTotalGBP decimal(18,2) NOT NULL CONSTRAINT DF_ApLpc_Ct DEFAULT 0,
  AgreedTotalGBP decimal(18,2) NULL, StatuteRef varchar(40) NOT NULL CONSTRAINT DF_ApLpc_St DEFAULT 'LPCDIA1998', IsDisputed bit NOT NULL CONSTRAINT DF_ApLpc_Di DEFAULT 0, DisputeReason varchar(300) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_ApLpc_S DEFAULT 'CLAIMED' CONSTRAINT CK_ApLpc_S CHECK (Status IN ('CLAIMED','AGREED','REJECTED','SETTLED','WAIVED')),
  ApprovalId bigint NULL, ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL, FinanceCostAccount varchar(12) NULL, JournalId bigint NULL, PaymentOpenItemId bigint NULL, Note varchar(300) NULL,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ApLpc_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT UQ_ApLatePaymentClaim UNIQUE (CompanyCode, ClaimRef));
GO
IF OBJECT_ID(N'ap.PaymentPracticesDetail') IS NULL CREATE TABLE ap.PaymentPracticesDetail (
  DetailId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_ApPpDetail PRIMARY KEY, ReportId int NOT NULL REFERENCES ap.PaymentPracticesReport(ReportId), CompanyCode varchar(10) NOT NULL,
  SupplierCode varchar(20) NOT NULL, BusinessSize varchar(8) NULL, PurchaseInvoiceId bigint NULL, RegisterNumber varchar(30) NULL, SupplierDocRef varchar(40) NULL, DateReceived date NOT NULL,
  GoodsReceivedDate date NULL, MeasurementStartDate date NOT NULL, AgreedTermsCode varchar(12) NULL, AgreedTermDays smallint NULL, DueDate date NULL, PaymentDate date NULL,
  DaysToPay int NULL, GrossAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_ApPpd_G DEFAULT 0, PaidAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_ApPpd_P DEFAULT 0,
  Band varchar(10) NOT NULL CONSTRAINT DF_ApPpd_B DEFAULT 'UNPAID' CONSTRAINT CK_ApPpd_B CHECK (Band IN ('W30','D31T60','D61PLUS','UNPAID')), PaidWithinTerms bit NOT NULL CONSTRAINT DF_ApPpd_T DEFAULT 1,
  IsDisputed bit NOT NULL CONSTRAINT DF_ApPpd_D DEFAULT 0, DisputeReason varchar(200) NULL, HasRetentionClause bit NOT NULL CONSTRAINT DF_ApPpd_R DEFAULT 0,
  IsExcluded bit NOT NULL CONSTRAINT DF_ApPpd_X DEFAULT 0, ExclusionReason varchar(200) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ApPpd_CA DEFAULT SYSDATETIME(),
  CONSTRAINT UQ_ApPpDetail UNIQUE (ReportId, SupplierCode, RegisterNumber));
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_ApPpDetail_Band') CREATE INDEX IX_ApPpDetail_Band ON ap.PaymentPracticesDetail(ReportId, Band, IsExcluded);
GO
IF OBJECT_ID(N'ap.EInvoiceInbox') IS NULL CREATE TABLE ap.EInvoiceInbox (
  InboxId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_ApEInvoiceInbox PRIMARY KEY, CompanyCode varchar(10) NOT NULL, FileName varchar(200) NOT NULL, FormatCode varchar(20) NOT NULL CONSTRAINT DF_ApEin_F DEFAULT 'PEPPOLBIS3',
  AccessPointRef varchar(60) NULL, ReceivedAt datetime2(3) NOT NULL CONSTRAINT DF_ApEin_RA DEFAULT SYSDATETIME(), SupplierCode varchar(20) NULL, SupplierVatRegNo varchar(15) NULL, SupplierDocRef varchar(40) NULL,
  SupplierDocRefNormalised varchar(40) NULL, DocumentType varchar(12) NULL, DocumentDate date NULL, TaxPointDate date NULL, CurrencyCode char(3) NULL, StatedNet decimal(18,2) NULL, StatedVat decimal(18,2) NULL,
  StatedGross decimal(18,2) NULL, LineCount int NOT NULL CONSTRAINT DF_ApEin_LC DEFAULT 0,
  Status varchar(12) NOT NULL CONSTRAINT DF_ApEin_S DEFAULT 'RECEIVED' CONSTRAINT CK_ApEin_S CHECK (Status IN ('RECEIVED','PARSED','REGISTERED','DUPLICATE','REJECTED')),
  RegisterNumber varchar(30) NULL, PurchaseInvoiceId bigint NULL, RejectReason varchar(300) NULL, ProcessedBy varchar(20) NULL, ProcessedAt datetime2(3) NULL, RowVer rowversion,
  CONSTRAINT UQ_ApEInvoiceInbox UNIQUE (CompanyCode, FileName));
GO
IF OBJECT_ID(N'ap.VatEvidence') IS NULL CREATE TABLE ap.VatEvidence (
  EvidenceId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_ApVatEvidence PRIMARY KEY, CompanyCode varchar(10) NOT NULL, PurchaseInvoiceId bigint NOT NULL, RegisterNumber varchar(30) NULL, SupplierCode varchar(20) NOT NULL,
  EvidenceType varchar(14) NOT NULL CONSTRAINT CK_ApVe_Et CHECK (EvidenceType IN ('VATINVOICE','SIMPLIFIED','C79','PVASTATEMENT','SELFBILL','REVERSECHG','PROFORMA','STATEMENT','NONE')),
  EvidenceRef varchar(60) NULL, SupplierVatRegNo varchar(15) NULL, TaxPointDate date NULL, VatPeriodKey varchar(10) NULL, ClaimedVatGBP decimal(18,2) NOT NULL CONSTRAINT DF_ApVe_Cv DEFAULT 0,
  AllowedVatGBP decimal(18,2) NOT NULL CONSTRAINT DF_ApVe_Av DEFAULT 0, HeldVatGBP decimal(18,2) NOT NULL CONSTRAINT DF_ApVe_Hv DEFAULT 0,
  Outcome varchar(12) NOT NULL CONSTRAINT DF_ApVe_Ou DEFAULT 'VALID' CONSTRAINT CK_ApVe_Ou CHECK (Outcome IN ('VALID','INVALID','HELD','RELEASED','REPAID','RECLAIMED')),
  FailureReason varchar(200) NULL, RepaidVatPeriodKey varchar(10) NULL, RepaidAt date NULL, RepaidVatTxnId bigint NULL, ReclaimedVatPeriodKey varchar(10) NULL, ReclaimedAt date NULL, ReclaimedVatTxnId bigint NULL,
  RecordedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ApVe_CA DEFAULT SYSDATETIME(), RowVer rowversion);
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_ApVatEvidence_Inv') CREATE INDEX IX_ApVatEvidence_Inv ON ap.VatEvidence(CompanyCode, PurchaseInvoiceId, Outcome);
GO
/*  Columns the baseline tables are missing for the reservation, set-off and e-invoice flows. */
IF COL_LENGTH('ap.OpenItem', 'SetOffAgainstCustomer') IS NULL ALTER TABLE ap.OpenItem ADD SetOffAgainstCustomer varchar(20) NULL;
GO
IF COL_LENGTH('ap.OpenItem', 'SetOffApprovalId') IS NULL ALTER TABLE ap.OpenItem ADD SetOffApprovalId bigint NULL;
GO
IF COL_LENGTH('ap.OpenItem', 'AdvanceAppliedGBP') IS NULL ALTER TABLE ap.OpenItem ADD AdvanceAppliedGBP decimal(18,2) NOT NULL CONSTRAINT DF_ApOI_Adv DEFAULT 0;
GO
IF COL_LENGTH('ap.PaymentProposal', 'ReservedTotalGBP') IS NULL ALTER TABLE ap.PaymentProposal ADD ReservedTotalGBP decimal(18,2) NOT NULL CONSTRAINT DF_PProp_Rt DEFAULT 0;
GO
IF COL_LENGTH('ap.PaymentProposalLine', 'ReservedAmountGBP') IS NULL ALTER TABLE ap.PaymentProposalLine ADD ReservedAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_PPropL_Ra DEFAULT 0;
GO
IF COL_LENGTH('ap.PurchaseInvoice', 'GoodsReceivedDate') IS NULL ALTER TABLE ap.PurchaseInvoice ADD GoodsReceivedDate date NULL;
GO
IF COL_LENGTH('ap.PurchaseInvoice', 'AgreedTermDays') IS NULL ALTER TABLE ap.PurchaseInvoice ADD AgreedTermDays smallint NULL;
GO
IF COL_LENGTH('ap.PurchaseInvoice', 'HasRetentionClause') IS NULL ALTER TABLE ap.PurchaseInvoice ADD HasRetentionClause bit NOT NULL CONSTRAINT DF_PInv_Ret DEFAULT 0;
GO
IF COL_LENGTH('ap.PurchaseInvoice', 'DisputeReason') IS NULL ALTER TABLE ap.PurchaseInvoice ADD DisputeReason varchar(200) NULL;
GO
IF COL_LENGTH('ap.SupplierBank', 'ChangeRequestSource') IS NULL ALTER TABLE ap.SupplierBank ADD ChangeRequestSource varchar(30) NULL;
GO
