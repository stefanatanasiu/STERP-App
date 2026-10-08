/*  core.AppUser, core.Role, core.UserRole - business users, roles and scoped assignments.
    REQ: COM-002, COM-003, COM-019 */
SET NOCOUNT ON;
IF OBJECT_ID(N'core.AppUser') IS NULL CREATE TABLE core.AppUser (
  UserCode varchar(20) NOT NULL CONSTRAINT PK_AppUser PRIMARY KEY, DisplayName varchar(80) NOT NULL, WindowsLogin varchar(128) NULL, EmailAddress varchar(128) NULL, JobTitle varchar(60) NULL, DefaultCompanyCode varchar(10) NULL,
  DefaultSiteCode varchar(10) NULL, IsReadOnly bit NOT NULL CONSTRAINT DF_AppUser_RO DEFAULT 0, IsActive bit NOT NULL CONSTRAINT DF_AppUser_Act DEFAULT 1, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_AppUser_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion);
GO
IF NOT EXISTS (SELECT 1 FROM sys.indexes WHERE name = 'UQ_AppUser_WindowsLogin') CREATE UNIQUE INDEX UQ_AppUser_WindowsLogin ON core.AppUser(WindowsLogin) WHERE WindowsLogin IS NOT NULL;
GO
IF OBJECT_ID(N'core.Role') IS NULL CREATE TABLE core.Role (
  RoleCode varchar(20) NOT NULL CONSTRAINT PK_Role PRIMARY KEY, Name varchar(80) NOT NULL, Description varchar(200) NULL, IsApprovalRole bit NOT NULL CONSTRAINT DF_Role_Appr DEFAULT 0, IsActive bit NOT NULL CONSTRAINT DF_Role_Act DEFAULT 1,
  CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Role_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion);
GO
IF OBJECT_ID(N'core.UserRole') IS NULL CREATE TABLE core.UserRole (
  UserCode varchar(20) NOT NULL REFERENCES core.AppUser(UserCode), RoleCode varchar(20) NOT NULL REFERENCES core.Role(RoleCode), CompanyCode varchar(10) NOT NULL, SiteCode varchar(10) NOT NULL CONSTRAINT DF_UserRole_Site DEFAULT '*',
  ValidFrom date NOT NULL CONSTRAINT DF_UserRole_VF DEFAULT '2000-01-01', ValidTo date NULL, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_UserRole_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL,
  ModifiedAt datetime2(3) NULL, RowVer rowversion, CONSTRAINT PK_UserRole PRIMARY KEY (UserCode, RoleCode, CompanyCode, SiteCode));
GO
