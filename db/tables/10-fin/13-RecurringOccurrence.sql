/*  fin.RecurringOccurrence - occurrence history for recurring journals, accruals and
    prepayments.  The unique key (CompanyCode, RecurringCode, OccurrenceNo) is what stops the
    same occurrence posting twice when a run is repeated (FIN-007, AC-011: three 100.00
    occurrences total 300.00, not 400.00).  A blocked occurrence is recorded with its reason so
    that it is visible rather than silently postponed or redated.
    REQ: FIN-007, FIN-008 */
SET NOCOUNT ON;
IF OBJECT_ID(N'fin.RecurringOccurrence') IS NULL CREATE TABLE fin.RecurringOccurrence (
  OccurrenceId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_RecurringOccurrence PRIMARY KEY, CompanyCode varchar(10) NOT NULL, RecurringCode varchar(20) NOT NULL, OccurrenceNo int NOT NULL,
  ScheduledDate date NOT NULL, PostingDate date NULL, FiscalYear smallint NULL, PeriodNo tinyint NULL, Amount decimal(18,2) NOT NULL CONSTRAINT DF_RecOcc_Amt DEFAULT 0,
  Status varchar(12) NOT NULL CONSTRAINT DF_RecOcc_St DEFAULT 'DUE' CONSTRAINT CK_RecOcc_St CHECK (Status IN ('DUE','POSTED','BLOCKED','SKIPPED','REVERSED')),
  JournalId bigint NULL, JournalNumber varchar(30) NULL, ReversalJournalNo varchar(30) NULL, BlockReason varchar(200) NULL, ExceptionId int NULL, CorrelationId varchar(64) NULL,
  PostedBy varchar(20) NULL, PostedAt datetime2(3) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_RecOcc_CA DEFAULT SYSDATETIME(), RowVer rowversion,
  CONSTRAINT UQ_RecurringOccurrence UNIQUE (CompanyCode, RecurringCode, OccurrenceNo));
GO
/*  Template lines for a recurring journal, accrual or prepayment.  The release schedule is
    derived from them: a prepayment of 300.00 over three periods releases 100.00 each time and
    stops when the source balance is exhausted (FIN-008, AC-011). */
IF OBJECT_ID(N'fin.RecurringLine') IS NULL CREATE TABLE fin.RecurringLine (
  RecurringLineId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_RecurringLine PRIMARY KEY, CompanyCode varchar(10) NOT NULL, RecurringCode varchar(20) NOT NULL, LineNum int NOT NULL, AccountCode varchar(12) NOT NULL,
  DebitAmount decimal(18,2) NOT NULL CONSTRAINT DF_RecL_D DEFAULT 0, CreditAmount decimal(18,2) NOT NULL CONSTRAINT DF_RecL_C DEFAULT 0, DimCostCentre varchar(20) NULL, DimProject varchar(20) NULL,
  Narrative varchar(200) NULL, VatCode varchar(4) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_RecL_CA DEFAULT SYSDATETIME(), RowVer rowversion,
  CONSTRAINT UQ_RecurringLine UNIQUE (CompanyCode, RecurringCode, LineNum));
GO
/*  Columns the release engine needs that the original fin.RecurringJournal did not carry. */
IF COL_LENGTH('fin.RecurringJournal', 'EventCode') IS NULL ALTER TABLE fin.RecurringJournal ADD EventCode varchar(30) NULL;
GO
IF COL_LENGTH('fin.RecurringJournal', 'ReversesNextPeriod') IS NULL ALTER TABLE fin.RecurringJournal ADD ReversesNextPeriod bit NOT NULL CONSTRAINT DF_Rec_RevNext DEFAULT 0;
GO
IF COL_LENGTH('fin.RecurringJournal', 'LastOccurrenceNo') IS NULL ALTER TABLE fin.RecurringJournal ADD LastOccurrenceNo int NOT NULL CONSTRAINT DF_Rec_LastOcc DEFAULT 0;
GO
