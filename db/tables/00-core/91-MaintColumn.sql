/* core.MaintColumn - the columns of a generic maintenance screen.  Only columns
   registered here may be listed or written by core.usp_UiMaintList / usp_UiMaintSave,
   and only when they really exist on the target table (COL_LENGTH check), so a
   definition can be seeded before the owning module has deployed its table.      */
IF OBJECT_ID(N'core.MaintColumn') IS NULL
CREATE TABLE core.MaintColumn (
    MaintCode    varchar(30)  NOT NULL,
    ColumnName   sysname      NOT NULL,
    Caption      varchar(60)  NOT NULL,
    DataType     varchar(10)  NOT NULL CONSTRAINT DF_MaintColumn_Type DEFAULT 'TEXT',
                 /* TEXT CODE INT DECIMAL MONEY QTY RATE PCT DATE BOOL */
    SortOrder    int          NOT NULL CONSTRAINT DF_MaintColumn_Sort DEFAULT 0,
    Width        int          NOT NULL CONSTRAINT DF_MaintColumn_Width DEFAULT 110,
    MaxLength    int          NULL,
    IsKey        bit          NOT NULL CONSTRAINT DF_MaintColumn_Key DEFAULT 0,
    IsRequired   bit          NOT NULL CONSTRAINT DF_MaintColumn_Req DEFAULT 0,
    IsReadOnly   bit          NOT NULL CONSTRAINT DF_MaintColumn_RO DEFAULT 0,
    IsInList     bit          NOT NULL CONSTRAINT DF_MaintColumn_List DEFAULT 1,
    IsInEdit     bit          NOT NULL CONSTRAINT DF_MaintColumn_Edit DEFAULT 1,
    IsSearchable bit          NOT NULL CONSTRAINT DF_MaintColumn_Search DEFAULT 0,
    LookupProc   varchar(128) NULL,
    LookupArg    varchar(60)  NULL,
    DefaultValue varchar(100) NULL,
    CONSTRAINT PK_MaintColumn PRIMARY KEY (MaintCode, ColumnName)
);
GO
