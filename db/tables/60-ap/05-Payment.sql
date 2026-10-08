/*  ap.PaymentProposal / ap.PaymentProposalLine / ap.PaymentInstruction - payment runs.  A
    submitted instruction keeps its approved beneficiary snapshot; an uncertain outcome keeps its
    reservation.  REQ: AP-011..AP-016, AP-026, D-16 */
SET NOCOUNT ON;
IF OBJECT_ID(N'ap.PaymentProposal') IS NULL CREATE TABLE ap.PaymentProposal (
  ProposalId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_PaymentProposal PRIMARY KEY, CompanyCode varchar(10) NOT NULL, ProposalRef varchar(30) NOT NULL, Version smallint NOT NULL CONSTRAINT DF_PProp_Ver DEFAULT 1, SelectionCutoffDate date NOT NULL,
  PayDate date NOT NULL, PaymentMethod varchar(10) NOT NULL CONSTRAINT CK_PProp_Pm CHECK (PaymentMethod IN ('BACS','FPS','CHAPS','SWIFT','SEPA')), BankAccountCode varchar(20) NOT NULL,
  CurrencyCode char(3) NOT NULL CONSTRAINT DF_PProp_Cur DEFAULT 'GBP', IsBacsProcessingDay bit NOT NULL CONSTRAINT DF_PProp_Bd DEFAULT 1, BacsInputDate date NULL, CashPrincipalGBP decimal(18,2) NOT NULL CONSTRAINT DF_PProp_Cp DEFAULT 0,
  CreditsAppliedGBP decimal(18,2) NOT NULL CONSTRAINT DF_PProp_Cr DEFAULT 0, DiscountsTakenGBP decimal(18,2) NOT NULL CONSTRAINT DF_PProp_Di DEFAULT 0, FeesGBP decimal(18,2) NOT NULL CONSTRAINT DF_PProp_Fe DEFAULT 0,
  TotalOutflowGBP decimal(18,2) NOT NULL CONSTRAINT DF_PProp_To DEFAULT 0, ExcludedCount int NOT NULL CONSTRAINT DF_PProp_Ec DEFAULT 0,
  Status varchar(12) NOT NULL CONSTRAINT DF_PProp_St DEFAULT 'DRAFT' CONSTRAINT CK_PProp_St CHECK (Status IN ('DRAFT','PROPOSED','APPROVED','RELEASED','CANCELLED','SUPERSEDED')), PreparedBy varchar(20) NULL, Approver1 varchar(20) NULL,
  Approver1At datetime2(3) NULL, Approver2 varchar(20) NULL, Approver2At datetime2(3) NULL, ApprovalId bigint NULL, ContentHash varchar(64) NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_PProp_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_PaymentProposal UNIQUE (CompanyCode, ProposalRef, Version));
GO
IF OBJECT_ID(N'ap.PaymentProposalLine') IS NULL CREATE TABLE ap.PaymentProposalLine (
  ProposalLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_PaymentProposalLine PRIMARY KEY, ProposalId int NOT NULL REFERENCES ap.PaymentProposal(ProposalId), LineNum int NOT NULL, SupplierCode varchar(20) NOT NULL, OpenItemId bigint NULL,
  DocumentNumber varchar(30) NULL, SupplierBankId int NULL, CurrencyCode char(3) NOT NULL, GrossAmount decimal(18,2) NOT NULL, GrossAmountGBP decimal(18,2) NOT NULL, DiscountGBP decimal(18,2) NOT NULL CONSTRAINT DF_PPropL_Di DEFAULT 0,
  CreditAppliedGBP decimal(18,2) NOT NULL CONSTRAINT DF_PPropL_Cr DEFAULT 0, NetPayGBP decimal(18,2) NOT NULL, IsExcluded bit NOT NULL CONSTRAINT DF_PPropL_Ex DEFAULT 0, ExclusionReason varchar(200) NULL,
  IsReservedElsewhere bit NOT NULL CONSTRAINT DF_PPropL_Re DEFAULT 0, ReservedByInstructionId bigint NULL, CONSTRAINT UQ_PaymentProposalLine UNIQUE (ProposalId, LineNum));
GO
IF OBJECT_ID(N'ap.PaymentInstruction') IS NULL CREATE TABLE ap.PaymentInstruction (
  PaymentInstructionId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_PaymentInstruction PRIMARY KEY, CompanyCode varchar(10) NOT NULL, InstructionRef varchar(30) NOT NULL, ProposalId int NULL, ProposalVersion smallint NULL,
  SupplierCode varchar(20) NOT NULL, PaymentMethod varchar(10) NOT NULL, BankAccountCode varchar(20) NOT NULL, CurrencyCode char(3) NOT NULL, Amount decimal(18,2) NOT NULL, AmountGBP decimal(18,2) NOT NULL,
  FxRate decimal(18,8) NOT NULL CONSTRAINT DF_PIns_Fx DEFAULT 1, BeneficiaryName varchar(120) NOT NULL, BeneficiarySortCode char(6) NULL, BeneficiaryAccountNumber char(8) NULL, BeneficiaryIban varchar(34) NULL, BeneficiaryBic varchar(11) NULL,
  SupplierBankId int NULL, CopResultSnapshot varchar(12) NULL, ValueDate date NOT NULL, SubmittedAt datetime2(3) NULL, SchemeReference varchar(60) NULL, BacsFileRef varchar(60) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_PIns_St DEFAULT 'APPROVED' CONSTRAINT CK_PIns_St CHECK (Status IN ('APPROVED','SUBMITTED','ACCEPTED','SETTLED','REJECTED','CANCELLED','UNCERTAIN','RETURNED')), SettledAt datetime2(3) NULL,
  SettledAmountGBP decimal(18,2) NULL, OutcomeNote varchar(300) NULL, InvestigationOwner varchar(20) NULL, RemittanceRef varchar(30) NULL, RemittanceIssuedAt datetime2(3) NULL, RemittanceReissueCount int NOT NULL CONSTRAINT DF_PIns_Rr DEFAULT 0,
  Authoriser1 varchar(20) NULL, Authoriser2 varchar(20) NULL, ApprovalId bigint NULL, JournalId bigint NULL, CorrelationId varchar(64) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_PIns_CA DEFAULT SYSDATETIME(),
  ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_PaymentInstruction UNIQUE (CompanyCode, InstructionRef));
GO
