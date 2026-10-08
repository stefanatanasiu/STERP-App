/*  core.MenuItem - classic MDI menu tree.  The .NET shell reads this; FormClass NULL = sub-menu.
    REQ: COM-002, COM-024 */
SET NOCOUNT ON;
IF OBJECT_ID(N'core.MenuItem') IS NULL CREATE TABLE core.MenuItem (
  MenuCode varchar(40) NOT NULL CONSTRAINT PK_MenuItem PRIMARY KEY, ParentMenuCode varchar(40) NULL, Caption varchar(60) NOT NULL, FormClass varchar(60) NULL, FormArgument varchar(60) NULL, PermissionCode varchar(40) NULL,
  SortOrder int NOT NULL CONSTRAINT DF_Menu_Sort DEFAULT 100, Shortcut varchar(20) NULL, IsActive bit NOT NULL CONSTRAINT DF_Menu_Act DEFAULT 1, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Menu_CA DEFAULT SYSDATETIME(),
  ModifiedBy varchar(20) NULL, ModifiedAt datetime2(3) NULL, RowVer rowversion);
GO
