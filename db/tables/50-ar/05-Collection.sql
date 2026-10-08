/*  ar.CollectionCase / ar.CollectionAction - disputes, promises to pay and the baseline
    collection sequence with contact history.  REQ: AR-015, AR-016, AR-027, D-06, D-28 */
SET NOCOUNT ON;
IF OBJECT_ID(N'ar.CollectionCase') IS NULL CREATE TABLE ar.CollectionCase (
  CaseId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_CollectionCase PRIMARY KEY, CompanyCode varchar(10) NOT NULL, CustomerCode varchar(20) NOT NULL, CaseRef varchar(30) NOT NULL,
  CaseType varchar(12) NOT NULL CONSTRAINT CK_ColC_Type CHECK (CaseType IN ('DISPUTE','COLLECTION','INSOLVENCY','LEGAL')), OpenItemId bigint NULL, DocumentNumber varchar(30) NULL, ReasonCode varchar(20) NULL, Reason varchar(300) NOT NULL,
  DisputedAmountGBP decimal(18,2) NOT NULL CONSTRAINT DF_ColC_Da DEFAULT 0, OwnerUser varchar(20) NULL, CollectionStep tinyint NOT NULL CONSTRAINT DF_ColC_Step DEFAULT 0, NextActionDate date NULL, NextAction varchar(200) NULL,
  EscalatedTo varchar(40) NULL, ExternalRecoveryRef varchar(60) NULL, PreActionProtocolSentAt date NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_ColC_St DEFAULT 'OPEN' CONSTRAINT CK_ColC_St CHECK (Status IN ('OPEN','ESCALATED','RESOLVED','CLOSED')), ResolvedAt datetime2(3) NULL, ResolutionNote varchar(400) NULL,
  OpenedAt datetime2(3) NOT NULL CONSTRAINT DF_ColC_OA DEFAULT SYSDATETIME(), CreatedBy varchar(20) NULL, ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT UQ_CollectionCase UNIQUE (CompanyCode, CaseRef));
GO
IF OBJECT_ID(N'ar.CollectionAction') IS NULL CREATE TABLE ar.CollectionAction (
  ActionId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_CollectionAction PRIMARY KEY, CaseId int NOT NULL REFERENCES ar.CollectionCase(CaseId), ActionSeq int NOT NULL,
  ActionType varchar(14) NOT NULL CONSTRAINT CK_ColA_Type CHECK (ActionType IN ('STATEMENT','REMINDER1','REMINDER2','CALL','EMAIL','PROMISE','STOP','DEMAND','HANDOFF','NOTE')), ActionDate date NOT NULL, PerformedBy varchar(20) NULL,
  ContactName varchar(80) NULL, Narrative varchar(400) NULL, PromiseAmountGBP decimal(18,2) NULL, PromiseDate date NULL, PromiseStatus varchar(10) NULL CONSTRAINT CK_ColA_Ps CHECK (PromiseStatus IN ('OPEN','KEPT','BROKEN','CANCELLED')),
  SuppressedReason varchar(200) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_ColA_CA DEFAULT SYSDATETIME(), CONSTRAINT UQ_CollectionAction UNIQUE (CaseId, ActionSeq));
GO
