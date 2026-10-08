/* core.UserPreference - per user client settings (window size, last company,
   grid column widths).  Written by the WinForms shell, never by business code. */
IF OBJECT_ID(N'core.UserPreference') IS NULL
CREATE TABLE core.UserPreference (
    UserCode   varchar(20)   NOT NULL,
    PrefKey    varchar(60)   NOT NULL,
    PrefValue  nvarchar(400) NULL,
    ModifiedAt datetime2(3)  NOT NULL CONSTRAINT DF_UserPreference_Mod DEFAULT SYSDATETIME(),
    CONSTRAINT PK_UserPreference PRIMARY KEY (UserCode, PrefKey)
);
GO
