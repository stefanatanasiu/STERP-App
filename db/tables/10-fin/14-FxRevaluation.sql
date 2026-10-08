/*  fin.FxRevaluation / fin.FxRevaluationLine - period-end revaluation of eligible monetary
    balances and the realised settlement position.  A revaluation is reversed at the start of
    the next period, so the lifetime gain on AC-016 is 62.50 and not 85.00: the carrying
    adjustment is recognised exactly once.  REQ: FIN-019, FIN-020 */
SET NOCOUNT ON;
IF OBJECT_ID(N'fin.FxRevaluation') IS NULL CREATE TABLE fin.FxRevaluation (
  RevalId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_FxRevaluation PRIMARY KEY, CompanyCode varchar(10) NOT NULL, FiscalYear smallint NOT NULL, PeriodNo tinyint NOT NULL, AsAtDate date NOT NULL,
  RateType varchar(6) NOT NULL CONSTRAINT DF_FxRev_RT DEFAULT 'SPOT', TotalGainGBP decimal(18,2) NOT NULL CONSTRAINT DF_FxRev_G DEFAULT 0, TotalLossGBP decimal(18,2) NOT NULL CONSTRAINT DF_FxRev_L DEFAULT 0,
  NetAdjustmentGBP decimal(18,2) NOT NULL CONSTRAINT DF_FxRev_N DEFAULT 0, JournalNumber varchar(30) NULL, ReversalJournalNo varchar(30) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_FxRev_St DEFAULT 'PREVIEW' CONSTRAINT CK_FxRev_St CHECK (Status IN ('PREVIEW','POSTED','REVERSED')),
  RunBy varchar(20) NULL, RunAt datetime2(3) NOT NULL CONSTRAINT DF_FxRev_RA DEFAULT SYSDATETIME(), CorrelationId varchar(64) NULL, RowVer rowversion,
  CONSTRAINT UQ_FxRevaluation UNIQUE (CompanyCode, FiscalYear, PeriodNo, AsAtDate));
GO
IF OBJECT_ID(N'fin.FxRevaluationLine') IS NULL CREATE TABLE fin.FxRevaluationLine (
  RevalLineId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_FxRevaluationLine PRIMARY KEY, RevalId int NOT NULL REFERENCES fin.FxRevaluation(RevalId), CompanyCode varchar(10) NOT NULL, AccountCode varchar(12) NOT NULL,
  SubledgerType varchar(6) NULL, SubledgerRef varchar(40) NULL, CurrencyCode char(3) NOT NULL, ForeignBalance decimal(18,2) NOT NULL, CarryingAmountGBP decimal(18,2) NOT NULL,
  RateOriginal decimal(18,8) NOT NULL, RateAtDate decimal(18,8) NOT NULL, RevaluedAmountGBP decimal(18,2) NOT NULL, AdjustmentGBP decimal(18,2) NOT NULL,
  AdjustmentType varchar(12) NOT NULL CONSTRAINT CK_FxRevL_T CHECK (AdjustmentType IN ('UNREALISED','REALISED')), CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_FxRevL_CA DEFAULT SYSDATETIME());
GO
/*  fin.WriteOff - approved write-offs, impairments and other value-only corrections.  The
    threshold decides who may approve; an adjustment never changes a physical quantity
    (FIN-022, AC-017: 100.00 impairment leaves 100 units and a 900.00 carrying value).
    REQ: FIN-022 */
IF OBJECT_ID(N'fin.WriteOff') IS NULL CREATE TABLE fin.WriteOff (
  WriteOffId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_WriteOff PRIMARY KEY, CompanyCode varchar(10) NOT NULL, WriteOffRef varchar(30) NOT NULL,
  WriteOffType varchar(14) NOT NULL CONSTRAINT CK_WOff_Type CHECK (WriteOffType IN ('BADDEBT','STOCKNRV','STOCKLOSS','BALANCE','OTHER')), ReasonCode varchar(20) NOT NULL, Reason varchar(300) NOT NULL,
  AccountCode varchar(12) NOT NULL, ContraAccountCode varchar(12) NOT NULL, SubledgerType varchar(6) NULL, SubledgerRef varchar(40) NULL, CarryingAmountGBP decimal(18,2) NULL, AmountGBP decimal(18,2) NOT NULL,
  QuantityAffected decimal(19,6) NULL, ThresholdGBP decimal(18,2) NULL, ApprovalLevel varchar(12) NULL, EvidenceRef varchar(200) NULL, PostingDate date NOT NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_WOff_St DEFAULT 'DRAFT' CONSTRAINT CK_WOff_St CHECK (Status IN ('DRAFT','PENDING','APPROVED','POSTED','REJECTED')),
  RequestedBy varchar(20) NULL, ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL, JournalNumber varchar(30) NULL, CorrelationId varchar(64) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_WOff_CA DEFAULT SYSDATETIME(), RowVer rowversion, CONSTRAINT UQ_WriteOff UNIQUE (CompanyCode, WriteOffRef));
GO
