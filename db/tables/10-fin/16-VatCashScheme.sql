/*  fin.VatCashLink - the cash accounting scheme (FIN-029).  Under the scheme output VAT is due
    when payment is received and input VAT recoverable when payment is made, so each settlement
    against a VAT transaction is recorded here and the return is built from these rows instead
    of the tax point.  AC-118: a 1,200.00 invoice with 200.00 VAT settled 600.00 contributes
    100.00 of output VAT; postponed import VAT and reverse charges stay on the invoice basis.
    REQ: FIN-029 */
SET NOCOUNT ON;
IF OBJECT_ID(N'fin.VatCashLink') IS NULL CREATE TABLE fin.VatCashLink (
  CashLinkId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_VatCashLink PRIMARY KEY, CompanyCode varchar(10) NOT NULL, VatTxnId bigint NOT NULL, SettlementRef varchar(40) NOT NULL, SettlementDate date NOT NULL,
  VatPeriodKey varchar(10) NULL, SettledGrossGBP decimal(18,2) NOT NULL, SettledNetGBP decimal(18,2) NOT NULL, SettledVatGBP decimal(18,2) NOT NULL, SettledPct decimal(9,4) NOT NULL,
  Direction varchar(6) NOT NULL CONSTRAINT CK_VatCL_Dir CHECK (Direction IN ('OUTPUT','INPUT')), IsTransitional bit NOT NULL CONSTRAINT DF_VatCL_Tr DEFAULT 0, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_VatCL_CA DEFAULT SYSDATETIME(), CONSTRAINT UQ_VatCashLink UNIQUE (CompanyCode, VatTxnId, SettlementRef));
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_VatCashLink_Period') CREATE INDEX IX_VatCashLink_Period ON fin.VatCashLink(CompanyCode, VatPeriodKey, Direction);
GO
/*  fin.VatReturnBox - the frozen box values and the transactions that produced them when a
    return is finalised.  A later edit to a source transaction cannot change these figures
    (FIN-026, AC-115), and the drill-down always reconciles to what was submitted. */
IF OBJECT_ID(N'fin.VatReturnBox') IS NULL CREATE TABLE fin.VatReturnBox (
  ReturnBoxId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_VatReturnBox PRIMARY KEY, VatReturnId int NOT NULL REFERENCES fin.VatReturn(VatReturnId), CompanyCode varchar(10) NOT NULL, BoxNo tinyint NOT NULL,
  BoxValue decimal(18,2) NOT NULL, TransactionCount int NOT NULL CONSTRAINT DF_VatRB_Cnt DEFAULT 0, IsWholePounds bit NOT NULL CONSTRAINT DF_VatRB_WP DEFAULT 0, Caption varchar(100) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_VatRB_CA DEFAULT SYSDATETIME(), CONSTRAINT UQ_VatReturnBox UNIQUE (VatReturnId, BoxNo));
GO
/*  The VAT transaction rows that fed a finalised return, so that a finalised box can be drilled
    into even after the open ledger moves on. */
IF OBJECT_ID(N'fin.VatReturnDetail') IS NULL CREATE TABLE fin.VatReturnDetail (
  ReturnDetailId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_VatReturnDetail PRIMARY KEY, VatReturnId int NOT NULL REFERENCES fin.VatReturn(VatReturnId), VatTxnId bigint NOT NULL, BoxNo tinyint NOT NULL,
  ContributionGBP decimal(18,2) NOT NULL, IsLateEntry bit NOT NULL CONSTRAINT DF_VatRD_Late DEFAULT 0, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_VatRD_CA DEFAULT SYSDATETIME());
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'IX_VatReturnDetail_Box') CREATE INDEX IX_VatReturnDetail_Box ON fin.VatReturnDetail(VatReturnId, BoxNo);
GO
/*  Error corrections need to know which return actually carried the adjustment. */
IF COL_LENGTH('fin.VatErrorCorrection', 'ThresholdUsedGBP') IS NULL ALTER TABLE fin.VatErrorCorrection ADD ThresholdUsedGBP decimal(18,2) NULL;
GO
IF COL_LENGTH('fin.VatErrorCorrection', 'AdjustmentJournalNo') IS NULL ALTER TABLE fin.VatErrorCorrection ADD AdjustmentJournalNo varchar(30) NULL;
GO
/*  The scheme a VAT transaction was captured under, so that leaving the cash accounting scheme
    can identify the outstanding transitional amounts. */
IF COL_LENGTH('fin.VatTransaction', 'VatScheme') IS NULL ALTER TABLE fin.VatTransaction ADD VatScheme varchar(12) NULL;
GO
IF COL_LENGTH('fin.VatTransaction', 'SettledVatGBP') IS NULL ALTER TABLE fin.VatTransaction ADD SettledVatGBP decimal(18,2) NOT NULL CONSTRAINT DF_VatT_SetV DEFAULT 0;
GO
