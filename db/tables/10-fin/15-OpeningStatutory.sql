/*  fin.OpeningBalance - approved opening ledger balances at cut over, with the opening-equity
    treatment identified separately.  Loading opening subledger detail must not duplicate the
    control value in the opening journal: AC-018 opening AR is 500.00, not 1,000.00, so detail
    rows carry SourceType = 'SUBLEDGER' and the control leg is derived from them.
    REQ: FIN-024, COM-018 */
SET NOCOUNT ON;
IF OBJECT_ID(N'fin.OpeningBalance') IS NULL CREATE TABLE fin.OpeningBalance (
  OpeningId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_OpeningBalance PRIMARY KEY, CompanyCode varchar(10) NOT NULL, BatchRef varchar(30) NOT NULL, AsAtDate date NOT NULL, AccountCode varchar(12) NOT NULL,
  SourceType varchar(12) NOT NULL CONSTRAINT DF_OpBal_Src DEFAULT 'LEDGER' CONSTRAINT CK_OpBal_Src CHECK (SourceType IN ('LEDGER','SUBLEDGER','EQUITY')), SubledgerType varchar(6) NULL, SubledgerRef varchar(40) NULL,
  DocumentRef varchar(40) NULL, DocumentDate date NULL, CurrencyCode char(3) NOT NULL CONSTRAINT DF_OpBal_Cur DEFAULT 'GBP', DebitAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_OpBal_D DEFAULT 0,
  CreditAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_OpBal_C DEFAULT 0, DimCostCentre varchar(20) NULL, DimProject varchar(20) NULL, Narrative varchar(200) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_OpBal_St DEFAULT 'LOADED' CONSTRAINT CK_OpBal_St CHECK (Status IN ('LOADED','REJECTED','POSTED')), RejectReason varchar(200) NULL, JournalNumber varchar(30) NULL,
  LoadedBy varchar(20) NULL, LoadedAt datetime2(3) NOT NULL CONSTRAINT DF_OpBal_LA DEFAULT SYSDATETIME(), RowVer rowversion,
  CONSTRAINT UQ_OpeningBalance UNIQUE (CompanyCode, BatchRef, AccountCode, SourceType, SubledgerRef, DocumentRef));
GO
/*  fin.StatutoryObligation - accounting reference date, statutory filing and tax deadlines per
    company and period of account.  AC-119: a 31 March 2027 year end for a private company gives
    accounts filing 31 December 2027, corporation tax payment 1 January 2028 and the CT600
    31 March 2028.  A period of account may run up to 18 months after an approved ARD change.
    REQ: FIN-030, XMD-013 */
IF OBJECT_ID(N'fin.StatutoryObligation') IS NULL CREATE TABLE fin.StatutoryObligation (
  ObligationId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_StatutoryObligation PRIMARY KEY, CompanyCode varchar(10) NOT NULL, PeriodEndDate date NOT NULL,
  ObligationType varchar(16) NOT NULL CONSTRAINT CK_StOb_Type CHECK (ObligationType IN ('ACCOUNTS','CTPAYMENT','CTRETURN','CONFIRMATION','VATRETURN')), Description varchar(120) NOT NULL,
  DueDate date NOT NULL, EscalateFrom date NULL, ResponsibleRole varchar(20) NULL, ResponsibleUser varchar(20) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_StOb_St DEFAULT 'OPEN' CONSTRAINT CK_StOb_St CHECK (Status IN ('OPEN','ESCALATED','COMPLETE','WAIVED')), CompletedBy varchar(20) NULL, CompletedAt datetime2(3) NULL,
  Reference varchar(60) NULL, Note varchar(300) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_StOb_CA DEFAULT SYSDATETIME(), RowVer rowversion,
  CONSTRAINT UQ_StatutoryObligation UNIQUE (CompanyCode, PeriodEndDate, ObligationType));
GO
/*  Period of account, including an approved change of accounting reference date (maximum 18
    months).  The baseline period is the April-March fiscal year. */
IF OBJECT_ID(N'fin.PeriodOfAccount') IS NULL CREATE TABLE fin.PeriodOfAccount (
  PeriodOfAccountId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_PeriodOfAccount PRIMARY KEY, CompanyCode varchar(10) NOT NULL, StartDate date NOT NULL, EndDate date NOT NULL, LengthMonths smallint NOT NULL,
  Framework varchar(10) NOT NULL CONSTRAINT DF_PoA_Fw DEFAULT 'FRS102', IsArdChange bit NOT NULL CONSTRAINT DF_PoA_Ard DEFAULT 0, ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL, Reason varchar(200) NULL,
  Status varchar(10) NOT NULL CONSTRAINT DF_PoA_St DEFAULT 'OPEN' CONSTRAINT CK_PoA_St CHECK (Status IN ('OPEN','CLOSED','FILED')), CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_PoA_CA DEFAULT SYSDATETIME(), RowVer rowversion,
  CONSTRAINT UQ_PeriodOfAccount UNIQUE (CompanyCode, EndDate));
GO
