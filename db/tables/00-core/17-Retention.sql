/*  core.RetentionClass / core.LegalHold / core.StatutoryObligation - retention policy, legal hold
    and the per-company obligations register.  REQ: COM-021, COM-027, COM-028, XMD-013, D-17 */
SET NOCOUNT ON;
IF OBJECT_ID(N'core.RetentionClass') IS NULL CREATE TABLE core.RetentionClass (
  RetentionClass varchar(16) NOT NULL CONSTRAINT PK_RetentionClass PRIMARY KEY, Name varchar(60) NOT NULL, RetentionYears smallint NOT NULL, Basis varchar(60) NOT NULL, ContainsPersonalData bit NOT NULL CONSTRAINT DF_Retn_Pd DEFAULT 0,
  DisposalAction varchar(16) NOT NULL CONSTRAINT DF_Retn_Act DEFAULT 'REVIEW' CONSTRAINT CK_Retn_Act CHECK (DisposalAction IN ('REVIEW','ANONYMISE','DELETE')), DecisionRef varchar(10) NULL, Description varchar(200) NULL, RowVer rowversion);
GO
IF OBJECT_ID(N'core.LegalHold') IS NULL CREATE TABLE core.LegalHold (
  LegalHoldId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_LegalHold PRIMARY KEY, CompanyCode varchar(10) NOT NULL, EntityType varchar(60) NOT NULL, EntityKey varchar(120) NOT NULL, Reason varchar(300) NOT NULL, AuthorityRef varchar(60) NULL,
  AppliedBy varchar(20) NOT NULL, AppliedAt datetime2(3) NOT NULL CONSTRAINT DF_Hold_AA DEFAULT SYSDATETIME(), ReleasedBy varchar(20) NULL, ReleasedAt datetime2(3) NULL, Status varchar(10) NOT NULL CONSTRAINT DF_Hold_St DEFAULT 'ACTIVE',
  RowVer rowversion);
GO
IF OBJECT_ID(N'core.StatutoryObligation') IS NULL CREATE TABLE core.StatutoryObligation (
  ObligationId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_StatutoryObligation PRIMARY KEY, CompanyCode varchar(10) NOT NULL, ObligationCode varchar(20) NOT NULL, PeriodKey varchar(20) NOT NULL, PeriodStart date NULL, PeriodEnd date NULL,
  DueDate date NOT NULL, OwnerUser varchar(20) NULL, OwnerRole varchar(20) NULL, Status varchar(12) NOT NULL CONSTRAINT DF_Oblig_St DEFAULT 'OPEN' CONSTRAINT CK_Oblig_St CHECK (Status IN ('OPEN','INPROGRESS','COMPLETE','LATE')),
  CompletedAt datetime2(3) NULL, CompletedBy varchar(20) NULL, EvidenceRef varchar(60) NULL, EscalatedAt datetime2(3) NULL, Notes varchar(300) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Oblig_CA DEFAULT SYSDATETIME(), RowVer rowversion,
  CONSTRAINT UQ_StatObligation UNIQUE (CompanyCode, ObligationCode, PeriodKey));
GO
