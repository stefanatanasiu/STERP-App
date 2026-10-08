/* core.usp_UiMaintDef - definition + column list of a generic maintenance screen.
   Columns that are registered but not (yet) present on the target table are hidden,
   so a definition may be seeded before the owning module deploys its table.
   The permission is checked for the signed-on company (the user's default company
   when the caller does not say), the same scope core.usp_UiMaintDefList and
   core.usp_UiMaintList use - a company-scoped role assignment never matches a
   NULL company, so without it the launcher listed screens this refused.         */
CREATE OR ALTER PROCEDURE core.usp_UiMaintDef
    @MaintCode   varchar(30),
    @UserCode    varchar(20) = NULL,
    @CompanyCode varchar(10) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    DECLARE @full nvarchar(300), @perm varchar(60), @allowed bit;
    SELECT @full = QUOTENAME(d.SchemaName) + N'.' + QUOTENAME(d.TableName), @perm = d.PermissionCode
    FROM core.MaintDef d WHERE d.MaintCode = @MaintCode AND d.IsActive = 1;
    IF @full IS NULL THROW 50090, 'STERP-E-0090: Maintenance definition is not registered.', 1;

    IF @CompanyCode IS NULL OR LTRIM(RTRIM(@CompanyCode)) = ''
        SELECT @CompanyCode = u.DefaultCompanyCode FROM core.AppUser u WHERE u.UserCode = @UserCode;
    EXEC core.usp_UiCheckAccess @UserCode, @perm, @CompanyCode, NULL, @allowed OUTPUT;
    IF @allowed = 0 THROW 50091, 'STERP-E-0091: You are not authorised to open this file maintenance.', 1;

    SELECT d.MaintCode, d.Title, d.ModuleCode, d.SchemaName, d.TableName, d.CompanyScoped,
           d.RetireColumn, d.PermissionCode, d.HelpText,
           CONVERT(bit, CASE WHEN OBJECT_ID(@full) IS NULL THEN 0 ELSE 1 END) AS TableExists
    FROM core.MaintDef d WHERE d.MaintCode = @MaintCode;

    SELECT c.ColumnName, c.Caption, c.DataType, c.SortOrder, c.Width, c.MaxLength,
           c.IsKey, c.IsRequired, c.IsReadOnly, c.IsInList, c.IsInEdit, c.IsSearchable,
           c.LookupProc, c.LookupArg, c.DefaultValue
    FROM core.MaintColumn c
    WHERE c.MaintCode = @MaintCode AND COL_LENGTH(@full, c.ColumnName) IS NOT NULL
    ORDER BY c.SortOrder, c.ColumnName;
END
GO
