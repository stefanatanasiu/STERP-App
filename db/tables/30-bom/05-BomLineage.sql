/*  bom.BomLineage  - where a job/order bill came from (BOM-022: complete lineage, and the copy
                      excludes execution history, allocations and prior approvals).
    bom.BomChangeImpact - the explicit retain/adopt/replan decision an engineering change needs
                      for every released kit or open demand it touches (BOM-031), plus the
                      reference check that stops a referenced revision being retired (BOM-028).
    REQ: BOM-022, BOM-028, BOM-030, BOM-031 */
SET NOCOUNT ON;
IF OBJECT_ID(N'bom.BomLineage') IS NULL CREATE TABLE bom.BomLineage (
  LineageId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_BomLineage PRIMARY KEY, CompanyCode varchar(10) NOT NULL, TargetBomHeaderId int NOT NULL REFERENCES bom.BomHeader(BomHeaderId), SourceBomHeaderId int NULL,
  SourceBomRef varchar(30) NULL, SourceBomRevision varchar(4) NULL, DerivationType varchar(12) NOT NULL CONSTRAINT CK_BomLin_Dt CHECK (DerivationType IN ('COPY','REVISION','CHANGE','MANUAL')),
  DemandType varchar(12) NULL, DemandRef varchar(40) NULL, LinesCopied int NOT NULL CONSTRAINT DF_BomLin_Lc DEFAULT 0, LinesExcluded int NOT NULL CONSTRAINT DF_BomLin_Lx DEFAULT 0,
  ExclusionNote varchar(300) NULL, DerivedBy varchar(20) NOT NULL, DerivedAt datetime2(3) NOT NULL CONSTRAINT DF_BomLin_DA DEFAULT SYSDATETIME(), CorrelationId varchar(64) NULL, ChangeRef varchar(30) NULL);
GO
IF OBJECT_ID(N'bom.BomChangeImpact') IS NULL CREATE TABLE bom.BomChangeImpact (
  ImpactId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_BomChangeImpact PRIMARY KEY, ChangeId int NULL REFERENCES bom.BomChange(ChangeId), CompanyCode varchar(10) NOT NULL, ChangeRef varchar(30) NOT NULL,
  ImpactType varchar(12) NOT NULL CONSTRAINT CK_BomImp_Ty CHECK (ImpactType IN ('KIT','DEMAND','JOBBOM','PARENTBOM','ITEM')), BomHeaderId int NOT NULL CONSTRAINT DF_BomImp_Bh DEFAULT 0, ParentItemCode varchar(40) NULL,
  ComponentItemCode varchar(40) NULL, DocumentType varchar(20) NULL, DocumentRef varchar(40) NOT NULL CONSTRAINT DF_BomImp_Dr DEFAULT '', DocumentStatus varchar(12) NULL, AffectedQty decimal(19,6) NULL, NeedDate date NULL, PathText varchar(400) NULL,
  Decision varchar(10) NOT NULL CONSTRAINT DF_BomImp_Dec DEFAULT 'PENDING' CONSTRAINT CK_BomImp_Dec CHECK (Decision IN ('PENDING','RETAIN','ADOPT','REPLAN','CANCEL')), DecisionBy varchar(20) NULL, DecisionAt datetime2(3) NULL,
  DecisionNote varchar(300) NULL, RaisedAt datetime2(3) NOT NULL CONSTRAINT DF_BomImp_RA DEFAULT SYSDATETIME(), RowVer rowversion, CONSTRAINT UQ_BomChangeImpact UNIQUE (CompanyCode, ChangeRef, ImpactType, DocumentRef, BomHeaderId));
GO
