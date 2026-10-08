/*  core.SanctionsList / core.SanctionsScreening - OFSI-style screening of parties and payees.
    REQ: COM-029, AR-028, PO-026, D-29 */
SET NOCOUNT ON;
IF OBJECT_ID(N'core.SanctionsList') IS NULL CREATE TABLE core.SanctionsList (
  ListEntryId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_SanctionsList PRIMARY KEY, ListCode varchar(12) NOT NULL CONSTRAINT DF_SancL_List DEFAULT 'OFSI', EntryRef varchar(20) NOT NULL,
  EntityType varchar(12) NOT NULL CONSTRAINT CK_SancL_Type CHECK (EntityType IN ('INDIVIDUAL','ENTITY','VESSEL')), Name varchar(120) NOT NULL, NameNormalised varchar(120) NOT NULL, Aliases varchar(300) NULL, Country varchar(40) NULL,
  DateOfBirth varchar(20) NULL, Regime varchar(60) NULL, ListedOn date NULL, LastUpdated date NULL, IsActive bit NOT NULL CONSTRAINT DF_SancL_Act DEFAULT 1, CONSTRAINT UQ_SanctionsList_Entry UNIQUE (ListCode, EntryRef));
GO
IF OBJECT_ID(N'core.SanctionsScreening') IS NULL CREATE TABLE core.SanctionsScreening (
  ScreeningId bigint IDENTITY(1,1) NOT NULL CONSTRAINT PK_SanctionsScreening PRIMARY KEY, CompanyCode varchar(10) NOT NULL, PartyType varchar(12) NOT NULL CONSTRAINT CK_SancS_PT CHECK (PartyType IN ('CUSTOMER','SUPPLIER','PAYEE','CONTACT')),
  PartyCode varchar(20) NOT NULL, PartyName varchar(120) NOT NULL, Country varchar(40) NULL, ScreenedAt datetime2(3) NOT NULL CONSTRAINT DF_SancS_SA DEFAULT SYSDATETIME(), ScreenedBy varchar(20) NULL, TriggerEvent varchar(20) NULL,
  MatchStatus varchar(12) NOT NULL CONSTRAINT CK_SancS_MS CHECK (MatchStatus IN ('CLEAR','POTENTIAL','MATCH')), MatchedEntryId int NULL, MatchScore decimal(9,4) NULL, MatchedName varchar(120) NULL,
  Decision varchar(12) NULL CONSTRAINT CK_SancS_Dec CHECK (Decision IN ('CLEARED','BLOCKED','PENDING')), DecidedBy varchar(20) NULL, DecidedAt datetime2(3) NULL, Note varchar(400) NULL, RowVer rowversion);
GO
