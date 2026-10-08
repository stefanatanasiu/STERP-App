/* core.usp_UiMaintDefList - the maintenance screens the user may open, used by the
   generic "File maintenance" launcher and by the menu seed.                      */
CREATE OR ALTER PROCEDURE core.usp_UiMaintDefList
    @UserCode    varchar(20) = NULL,
    @CompanyCode varchar(10) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    CREATE TABLE #d (MaintCode varchar(30) NOT NULL PRIMARY KEY, Title varchar(80) NOT NULL,
                     ModuleCode varchar(10) NOT NULL, PermissionCode varchar(60) NULL, Allowed bit NOT NULL);
    INSERT #d (MaintCode, Title, ModuleCode, PermissionCode, Allowed)
        SELECT d.MaintCode, d.Title, d.ModuleCode, d.PermissionCode, 1
        FROM core.MaintDef d
        WHERE d.IsActive = 1
          AND OBJECT_ID(QUOTENAME(d.SchemaName) + N'.' + QUOTENAME(d.TableName)) IS NOT NULL;

    DECLARE @mc varchar(30), @pc varchar(60), @ok bit;
    DECLARE def_cur CURSOR LOCAL FAST_FORWARD FOR SELECT MaintCode, PermissionCode FROM #d WHERE PermissionCode IS NOT NULL;
    OPEN def_cur;
    FETCH NEXT FROM def_cur INTO @mc, @pc;
    WHILE @@FETCH_STATUS = 0
    BEGIN
        EXEC core.usp_UiCheckAccess @UserCode, @pc, @CompanyCode, NULL, @ok OUTPUT;
        UPDATE #d SET Allowed = ISNULL(@ok, 0) WHERE MaintCode = @mc;
        FETCH NEXT FROM def_cur INTO @mc, @pc;
    END
    CLOSE def_cur;
    DEALLOCATE def_cur;

    SELECT MaintCode, Title, ModuleCode FROM #d WHERE Allowed = 1 ORDER BY ModuleCode, Title;
END
GO
