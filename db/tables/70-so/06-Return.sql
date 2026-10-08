/*  so.ReturnAuthorisation / so.ReturnLine / so.RebateAgreement / so.RebateAccrual - customer
    returns with inspection and disposition, and retrospective volume rebates accrued against
    revenue as variable consideration.  REQ: SO-017, SO-018, SO-027, D-12 */
SET NOCOUNT ON;
IF OBJECT_ID(N'so.ReturnAuthorisation') IS NULL CREATE TABLE so.ReturnAuthorisation (
  ReturnId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_ReturnAuthorisation PRIMARY KEY, CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL, ReturnRef varchar(30) NOT NULL, CustomerCode varchar(20) NOT NULL,
  OriginalOrderNumber varchar(30) NULL, OriginalDespatchRef varchar(30) NULL, OriginalInvoiceNumber varchar(30) NULL, RequestedDate date NOT NULL, AuthorisedDate date NULL, ReceivedDate date NULL, InspectedDate date NULL,
  ReasonCode varchar(20) NOT NULL, Reason varchar(300) NULL, IsFaulty bit NOT NULL CONSTRAINT DF_Ret_Flt DEFAULT 0, WithinReturnWindow bit NOT NULL CONSTRAINT DF_Ret_Win DEFAULT 1, WarrantyClaim bit NOT NULL CONSTRAINT DF_Ret_War DEFAULT 0,
  ReplacementDecision varchar(12) NULL CONSTRAINT CK_Ret_Rep CHECK (ReplacementDecision IN ('NONE','REPLACE','CREDIT','REPAIR')), ReplacementOrderNumber varchar(30) NULL, CreditNoteNumber varchar(30) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_Ret_St DEFAULT 'REQUESTED' CONSTRAINT CK_Ret_St CHECK (Status IN ('REQUESTED','AUTHORISED','RECEIVED','INSPECTED','CLOSED','REJECTED')), AuthorisedBy varchar(20) NULL, ApprovalId bigint NULL,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Ret_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_ReturnAuthorisation UNIQUE (CompanyCode, ReturnRef));
GO
IF OBJECT_ID(N'so.ReturnLine') IS NULL CREATE TABLE so.ReturnLine (
  ReturnLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_ReturnLine PRIMARY KEY, ReturnId bigint NOT NULL REFERENCES so.ReturnAuthorisation(ReturnId), LineNum int NOT NULL, ItemCode varchar(40) NOT NULL, Revision varchar(4) NULL,
  LotCode varchar(30) NULL, SerialNo varchar(40) NULL, RequestedQty decimal(19,6) NOT NULL, AuthorisedQty decimal(19,6) NOT NULL CONSTRAINT DF_RetL_Aq DEFAULT 0, ReceivedQty decimal(19,6) NOT NULL CONSTRAINT DF_RetL_Rq DEFAULT 0,
  AcceptedQty decimal(19,6) NOT NULL CONSTRAINT DF_RetL_Cq DEFAULT 0, RejectedQty decimal(19,6) NOT NULL CONSTRAINT DF_RetL_Xq DEFAULT 0, CreditedQty decimal(19,6) NOT NULL CONSTRAINT DF_RetL_Kq DEFAULT 0, UomCode varchar(10) NULL,
  Disposition varchar(12) NULL CONSTRAINT CK_RetL_Disp CHECK (Disposition IN ('RESTOCK','REWORK','SUPPRETURN','SCRAP','HOLD')), OriginalUnitCostGBP decimal(19,6) NULL, ReturnValueGBP decimal(18,2) NULL,
  VarianceGBP decimal(18,2) NOT NULL CONSTRAINT DF_RetL_Va DEFAULT 0, CreditUnitPrice decimal(19,6) NULL, VatCode varchar(4) NULL, MovementId bigint NULL, QualityHoldId bigint NULL, CONSTRAINT UQ_ReturnLine UNIQUE (ReturnId, LineNum));
GO
IF OBJECT_ID(N'so.RebateAgreement') IS NULL CREATE TABLE so.RebateAgreement (
  RebateId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_RebateAgreement PRIMARY KEY, CompanyCode varchar(10) NOT NULL, RebateRef varchar(30) NOT NULL, CustomerCode varchar(20) NOT NULL, Description varchar(200) NULL, ItemCode varchar(40) NULL,
  ProductGroup varchar(20) NULL, Basis varchar(12) NOT NULL CONSTRAINT CK_Reb_Bas CHECK (Basis IN ('TURNOVER','QUANTITY','TIERED')), ThresholdValue decimal(19,6) NULL, RebatePct decimal(9,4) NULL, RebatePerUnit decimal(19,6) NULL,
  PeriodStart date NOT NULL, PeriodEnd date NOT NULL, CurrencyCode char(3) NOT NULL CONSTRAINT DF_Reb_Cur DEFAULT 'GBP',
  Status varchar(12) NOT NULL CONSTRAINT DF_Reb_St DEFAULT 'ACTIVE' CONSTRAINT CK_Reb_St CHECK (Status IN ('DRAFT','ACTIVE','CLOSED','SETTLED')), ApprovedBy varchar(20) NULL, SettlementCreditNote varchar(30) NULL, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Reb_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_RebateAgreement UNIQUE (CompanyCode, RebateRef));
GO
IF OBJECT_ID(N'so.RebateAccrual') IS NULL CREATE TABLE so.RebateAccrual (
  AccrualId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_RebateAccrual PRIMARY KEY, RebateId int NOT NULL REFERENCES so.RebateAgreement(RebateId), CompanyCode varchar(10) NOT NULL, FiscalYear smallint NOT NULL, PeriodNo tinyint NOT NULL,
  QualifyingRevenueGBP decimal(18,2) NOT NULL CONSTRAINT DF_RebA_Qr DEFAULT 0, QualifyingQty decimal(19,6) NULL, AccruedGBP decimal(18,2) NOT NULL CONSTRAINT DF_RebA_Ac DEFAULT 0, SettledGBP decimal(18,2) NOT NULL CONSTRAINT DF_RebA_Se DEFAULT 0,
  QualifyingInvoiceRefs varchar(400) NULL, JournalId bigint NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_RebA_CA DEFAULT SYSDATETIME(), CONSTRAINT UQ_RebateAccrual UNIQUE (RebateId, FiscalYear, PeriodNo));
GO
