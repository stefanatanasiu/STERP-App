/*  ar.LatePaymentClaim / ar.BadDebtRelief - statutory interest and fixed compensation under the
    Late Payment of Commercial Debts (Interest) Act 1998, and VAT bad debt relief.
    REQ: AR-024, AR-026, D-25, D-28 */
SET NOCOUNT ON;
IF OBJECT_ID(N'ar.LatePaymentClaim') IS NULL CREATE TABLE ar.LatePaymentClaim (
  ClaimId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_LatePaymentClaim PRIMARY KEY, CompanyCode varchar(10) NOT NULL, CustomerCode varchar(20) NOT NULL, ClaimRef varchar(30) NOT NULL, OpenItemId bigint NULL, InvoiceNumber varchar(30) NULL,
  InvoiceDueDate date NOT NULL, PrincipalGBP decimal(18,2) NOT NULL, DaysOverdue int NOT NULL, ReferenceRatePct decimal(9,4) NOT NULL, MarginPct decimal(9,4) NOT NULL CONSTRAINT DF_Lpc_M DEFAULT 8, InterestGBP decimal(18,2) NOT NULL,
  CompensationGBP decimal(18,2) NOT NULL, RecoveryCostsGBP decimal(18,2) NOT NULL CONSTRAINT DF_Lpc_Rc DEFAULT 0, TotalClaimGBP decimal(18,2) NOT NULL, CalculatedAt datetime2(3) NOT NULL CONSTRAINT DF_Lpc_CA DEFAULT SYSDATETIME(),
  CalculatedTo date NOT NULL, Status varchar(10) NOT NULL CONSTRAINT DF_Lpc_St DEFAULT 'CALC' CONSTRAINT CK_Lpc_St CHECK (Status IN ('CALC','APPROVED','ISSUED','WAIVED','SETTLED')), ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL,
  WaivedBy varchar(20) NULL, WaiverReason varchar(300) NULL, ClaimInvoiceNumber varchar(30) NULL, RowVer rowversion, CONSTRAINT UQ_LatePaymentClaim UNIQUE (CompanyCode, ClaimRef));
GO
IF OBJECT_ID(N'ar.BadDebtRelief') IS NULL CREATE TABLE ar.BadDebtRelief (
  ReliefId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_BadDebtRelief PRIMARY KEY, CompanyCode varchar(10) NOT NULL, CustomerCode varchar(20) NOT NULL, OpenItemId bigint NOT NULL, InvoiceNumber varchar(30) NOT NULL, SupplyDate date NOT NULL,
  DueDate date NOT NULL, EligibleFromDate date NOT NULL, WrittenOffDate date NULL, WrittenOffGBP decimal(18,2) NOT NULL, VatAccountedGBP decimal(18,2) NOT NULL, ReliefClaimedGBP decimal(18,2) NOT NULL CONSTRAINT DF_Bdr_Cl DEFAULT 0,
  ClaimVatPeriodKey varchar(10) NULL, RepaidGBP decimal(18,2) NOT NULL CONSTRAINT DF_Bdr_Rp DEFAULT 0, RepaymentVatPeriodKey varchar(10) NULL, IsSoldOrFactored bit NOT NULL CONSTRAINT DF_Bdr_Sf DEFAULT 0,
  Status varchar(12) NOT NULL CONSTRAINT DF_Bdr_St DEFAULT 'ELIGIBLE' CONSTRAINT CK_Bdr_St CHECK (Status IN ('PENDING','ELIGIBLE','CLAIMED','PARTREPAID','REPAID','BLOCKED')), BlockReason varchar(200) NULL, JournalId bigint NULL,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Bdr_CA DEFAULT SYSDATETIME(), RowVer rowversion);
GO
