/*  fin.VatCode / fin.VatRate - effective-dated UK VAT liabilities, special mechanisms and the
    MTD nine-box mapping.  REQ: FIN-016, FIN-017, FIN-025, FIN-028, D-02, D-23 */
SET NOCOUNT ON;
IF OBJECT_ID(N'fin.VatCode') IS NULL CREATE TABLE fin.VatCode (
  VatCode varchar(4) NOT NULL CONSTRAINT PK_VatCode PRIMARY KEY, Name varchar(60) NOT NULL, Liability varchar(12) NOT NULL CONSTRAINT CK_VatC_Liab CHECK (Liability IN ('STANDARD','REDUCED','ZERO','EXEMPT','OUTSIDE')),
  Direction varchar(5) NOT NULL CONSTRAINT DF_VatC_Dir DEFAULT 'BOTH' CONSTRAINT CK_VatC_Dir CHECK (Direction IN ('SALE','PURCH','BOTH')),
  Mechanism varchar(12) NOT NULL CONSTRAINT DF_VatC_Mech DEFAULT 'NORMAL' CONSTRAINT CK_VatC_Mech CHECK (Mechanism IN ('NORMAL','RCSERVICE','DRC','PVA','C79','BADDEBT','EXPORT')), IsRecoverable bit NOT NULL CONSTRAINT DF_VatC_Rec DEFAULT 1,
  SelfAccounts bit NOT NULL CONSTRAINT DF_VatC_Self DEFAULT 0, OutputVatBox tinyint NULL, InputVatBox tinyint NULL, OutputNetBox tinyint NULL, InputNetBox tinyint NULL, InvoiceWording varchar(200) NULL, EvidenceRequired varchar(100) NULL,
  IsActive bit NOT NULL CONSTRAINT DF_VatC_Act DEFAULT 1, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_VatC_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion);
GO
IF OBJECT_ID(N'fin.VatRate') IS NULL CREATE TABLE fin.VatRate (
  VatCode varchar(4) NOT NULL REFERENCES fin.VatCode(VatCode), EffectiveFrom date NOT NULL, RatePct decimal(9,4) NOT NULL, ApprovedBy varchar(20) NULL, StatutoryRef varchar(100) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_VatR_CA DEFAULT SYSDATETIME(), RowVer rowversion, CONSTRAINT PK_VatRate PRIMARY KEY (VatCode, EffectiveFrom));
GO
