/* core.MaintDef - registry of the generic maintenance screens driven by the WinForms
   client (docs\NET-FRAMEWORK.md).  One row per simple master file; the .NET client
   never builds SQL of its own, it posts a MaintCode and column values and
   core.usp_UiMaintSave whitelists them against core.MaintColumn.            COM-013 */
IF OBJECT_ID(N'core.MaintDef') IS NULL
CREATE TABLE core.MaintDef (
    MaintCode       varchar(30)   NOT NULL CONSTRAINT PK_MaintDef PRIMARY KEY,
    Title           varchar(80)   NOT NULL,
    ModuleCode      varchar(10)   NOT NULL CONSTRAINT DF_MaintDef_Module DEFAULT 'common',
    SchemaName      sysname       NOT NULL,
    TableName       sysname       NOT NULL,
    CompanyScoped   bit           NOT NULL CONSTRAINT DF_MaintDef_Scoped DEFAULT 1,
    RetireColumn    sysname       NULL,
    PermissionCode  varchar(60)   NULL,
    HelpText        varchar(400)  NULL,
    IsActive        bit           NOT NULL CONSTRAINT DF_MaintDef_Active DEFAULT 1,
    CreatedBy       varchar(20)   NULL,
    CreatedAt       datetime2(3)  NOT NULL CONSTRAINT DF_MaintDef_Created DEFAULT SYSDATETIME(),
    ModifiedBy      varchar(20)   NULL,
    ModifiedAt      datetime2(3)  NULL,
    RowVer          rowversion    NOT NULL
);
GO
