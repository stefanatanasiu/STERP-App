/*  bom.BomChange / bom.BomChangeLine - engineering change proposals with per-bill outcome.
    REQ: BOM-024, BOM-031, XMD-008 */
SET NOCOUNT ON;
IF OBJECT_ID(N'bom.BomChange') IS NULL CREATE TABLE bom.BomChange (
  ChangeId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_BomChange PRIMARY KEY, CompanyCode varchar(10) NOT NULL, ChangeRef varchar(30) NOT NULL, Title varchar(150) NOT NULL, Description varchar(400) NULL,
  ChangeType varchar(12) NOT NULL CONSTRAINT CK_BomCh_Type CHECK (ChangeType IN ('REPLACE','ADD','REMOVE','QTYCHANGE','MIXED')), RequestedBy varchar(20) NOT NULL, RequestedAt datetime2(3) NOT NULL CONSTRAINT DF_BomCh_RA DEFAULT SYSDATETIME(),
  EffectiveDate date NULL, ApprovedBy varchar(20) NULL, ApprovedAt datetime2(3) NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_BomCh_St DEFAULT 'DRAFT' CONSTRAINT CK_BomCh_St CHECK (Status IN ('DRAFT','PREVIEW','APPROVED','APPLIED','REJECTED','PARTIAL')), AffectedBills int NOT NULL CONSTRAINT DF_BomCh_Ab DEFAULT 0,
  AppliedBills int NOT NULL CONSTRAINT DF_BomCh_Pb DEFAULT 0, AffectedKits varchar(400) NULL, ImpactNotes varchar(400) NULL, RowVer rowversion, CONSTRAINT UQ_BomChange UNIQUE (CompanyCode, ChangeRef));
GO
IF OBJECT_ID(N'bom.BomChangeLine') IS NULL CREATE TABLE bom.BomChangeLine (
  ChangeLineId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_BomChangeLine PRIMARY KEY, ChangeId int NOT NULL REFERENCES bom.BomChange(ChangeId), LineNum int NOT NULL, BomHeaderId int NULL, BomRef varchar(30) NULL, BomRevision varchar(4) NULL,
  Action varchar(12) NOT NULL CONSTRAINT CK_BomChL_Act CHECK (Action IN ('REPLACE','ADD','REMOVE','QTYCHANGE')), ComponentItemCode varchar(40) NULL, NewComponentItemCode varchar(40) NULL, OldUsageQty decimal(19,6) NULL,
  NewUsageQty decimal(19,6) NULL, NewBomRevision varchar(4) NULL, Outcome varchar(12) NULL CONSTRAINT CK_BomChL_Out CHECK (Outcome IN ('PENDING','APPLIED','FAILED','SKIPPED')), FailureReason varchar(300) NULL, RowVer rowversion,
  CONSTRAINT UQ_BomChangeLine UNIQUE (ChangeId, LineNum));
GO
