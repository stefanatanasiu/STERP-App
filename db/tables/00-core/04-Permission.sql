/*  core.Permission, core.RolePermission, core.SoDRule, core.Delegation - authority model.
    REQ: COM-002, COM-003, D-07 */
SET NOCOUNT ON;
IF OBJECT_ID(N'core.Permission') IS NULL CREATE TABLE core.Permission (
  PermissionCode varchar(40) NOT NULL CONSTRAINT PK_Permission PRIMARY KEY, Module varchar(6) NOT NULL, Name varchar(100) NOT NULL, IsSensitive bit NOT NULL CONSTRAINT DF_Perm_Sens DEFAULT 0, IsApproval bit NOT NULL CONSTRAINT DF_Perm_Appr DEFAULT 0,
  Description varchar(200) NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Perm_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion);
GO
IF OBJECT_ID(N'core.RolePermission') IS NULL CREATE TABLE core.RolePermission (
  RoleCode varchar(20) NOT NULL REFERENCES core.Role(RoleCode), PermissionCode varchar(40) NOT NULL REFERENCES core.Permission(PermissionCode), CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_RolePerm_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_RolePermission PRIMARY KEY (RoleCode, PermissionCode));
GO
/*  A segregation-of-duties rule forbids one user from holding both sides of a protected process. */
IF OBJECT_ID(N'core.SoDRule') IS NULL CREATE TABLE core.SoDRule (
  SoDRuleCode varchar(30) NOT NULL CONSTRAINT PK_SoDRule PRIMARY KEY, Name varchar(100) NOT NULL, PermissionA varchar(40) NOT NULL REFERENCES core.Permission(PermissionCode),
  PermissionB varchar(40) NOT NULL REFERENCES core.Permission(PermissionCode), ApprovalType varchar(30) NULL, Severity varchar(12) NOT NULL CONSTRAINT DF_SoD_Sev DEFAULT 'BLOCK' CONSTRAINT CK_SoD_Sev CHECK (Severity IN ('BLOCK','WARN')),
  Rationale varchar(200) NULL, IsActive bit NOT NULL CONSTRAINT DF_SoD_Act DEFAULT 1, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_SoD_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL,
  RowVer rowversion);
GO
/*  Named, time-bounded approval delegation - maximum 30 days (D-07). */
IF OBJECT_ID(N'core.Delegation') IS NULL CREATE TABLE core.Delegation (
  DelegationId int IDENTITY(1,1) NOT NULL CONSTRAINT PK_Delegation PRIMARY KEY, CompanyCode varchar(10) NOT NULL, FromUser varchar(20) NOT NULL REFERENCES core.AppUser(UserCode), ToUser varchar(20) NOT NULL REFERENCES core.AppUser(UserCode),
  ApprovalType varchar(30) NULL, MaxAmount decimal(18,2) NULL, ValidFrom date NOT NULL, ValidTo date NOT NULL, Reason varchar(200) NOT NULL,
  Status varchar(12) NOT NULL CONSTRAINT DF_Deleg_St DEFAULT 'ACTIVE' CONSTRAINT CK_Deleg_St CHECK (Status IN ('ACTIVE','REVOKED','EXPIRED')), CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Deleg_CA DEFAULT SYSDATETIME(),
  ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT CK_Deleg_Span CHECK (ValidTo >= ValidFrom AND DATEDIFF(day, ValidFrom, ValidTo) <= 30));
GO
