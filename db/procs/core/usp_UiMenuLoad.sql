/* core.usp_UiMenuLoad - the MDI menu bar.  Returns the core.MenuItem rows the signed
   on user is allowed to see; a branch the permission filter has emptied is dropped
   so the client never shows a menu with nothing under it.               COM-002, COM-024 */
CREATE OR ALTER PROCEDURE core.usp_UiMenuLoad
    @UserCode    varchar(20),
    @CompanyCode varchar(10) = NULL,
    @SiteCode    varchar(10) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    IF OBJECT_ID(N'core.MenuItem') IS NULL
    BEGIN
        SELECT CONVERT(varchar(40), NULL) AS MenuCode, CONVERT(varchar(40), NULL) AS ParentMenuCode,
               CONVERT(varchar(60), NULL) AS Caption, CONVERT(varchar(80), NULL) AS FormClass,
               CONVERT(varchar(60), NULL) AS FormArgument, CONVERT(varchar(60), NULL) AS PermissionCode,
               CONVERT(int, NULL) AS SortOrder, CONVERT(varchar(20), NULL) AS Shortcut
        WHERE 1 = 0;
        RETURN;
    END

    CREATE TABLE #perm (PermissionCode varchar(60) NOT NULL PRIMARY KEY, Allowed bit NOT NULL);
    INSERT #perm (PermissionCode, Allowed)
        SELECT DISTINCT PermissionCode, 0 FROM core.MenuItem
        WHERE IsActive = 1 AND PermissionCode IS NOT NULL AND LTRIM(RTRIM(PermissionCode)) <> '';

    DECLARE @pc varchar(60), @ok bit;
    DECLARE perm_cur CURSOR LOCAL FAST_FORWARD FOR SELECT PermissionCode FROM #perm;
    OPEN perm_cur;
    FETCH NEXT FROM perm_cur INTO @pc;
    WHILE @@FETCH_STATUS = 0
    BEGIN
        EXEC core.usp_UiCheckAccess @UserCode, @pc, @CompanyCode, @SiteCode, @ok OUTPUT;
        UPDATE #perm SET Allowed = ISNULL(@ok, 0) WHERE PermissionCode = @pc;
        FETCH NEXT FROM perm_cur INTO @pc;
    END
    CLOSE perm_cur;
    DEALLOCATE perm_cur;

    -- visible = not secured, or the user holds the permission
    CREATE TABLE #vis (MenuCode varchar(40) NOT NULL PRIMARY KEY, ParentMenuCode varchar(40) NULL);
    INSERT #vis (MenuCode, ParentMenuCode)
        SELECT m.MenuCode, m.ParentMenuCode
        FROM core.MenuItem m
        LEFT JOIN #perm p ON p.PermissionCode = m.PermissionCode
        WHERE m.IsActive = 1
          AND (m.PermissionCode IS NULL OR LTRIM(RTRIM(m.PermissionCode)) = '' OR p.Allowed = 1);

    -- a branch the filter has emptied is not published at all (repeat for nested branches)
    DECLARE @dropped int = 1;
    WHILE @dropped > 0
    BEGIN
        DELETE v FROM #vis v
        WHERE EXISTS (SELECT 1 FROM core.MenuItem c WHERE c.ParentMenuCode = v.MenuCode AND c.IsActive = 1)
          AND NOT EXISTS (SELECT 1 FROM #vis c WHERE c.ParentMenuCode = v.MenuCode);
        SET @dropped = @@ROWCOUNT;
    END
    DELETE v FROM #vis v
    WHERE v.ParentMenuCode IS NOT NULL AND NOT EXISTS (SELECT 1 FROM #vis p WHERE p.MenuCode = v.ParentMenuCode);

    SELECT m.MenuCode, m.ParentMenuCode, m.Caption, m.FormClass, m.FormArgument,
           m.PermissionCode, m.SortOrder, m.Shortcut
    FROM core.MenuItem m
    JOIN #vis v ON v.MenuCode = m.MenuCode
    ORDER BY ISNULL(m.ParentMenuCode, ''), m.SortOrder, m.Caption;
END
GO
