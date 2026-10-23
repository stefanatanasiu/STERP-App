/* core.usp_UiMaintSave - add / change / retire a row of a generic maintenance screen.
   @Data is  <r><c n="ColumnName" v="value" /> ... </r>  posted by the WinForms client.
   Column names are whitelisted against core.MaintColumn and wrapped in QUOTENAME;
   the values stay sp_executesql parameters (read out of #v inside the statement), so
   nothing the user types is ever concatenated into SQL text.
   RETIRE never deletes - it clears the retire flag, so referenced rows survive.
                                                            COM-013, COM-016, COM-024 */
CREATE OR ALTER PROCEDURE core.usp_UiMaintSave
    @MaintCode   varchar(30),
    @UserCode    varchar(20),
    @CompanyCode varchar(10) = NULL,
    @Mode        varchar(10),            /* ADD | CHANGE | RETIRE | REINSTATE */
    @Data        xml
AS
BEGIN
    SET NOCOUNT ON;
    SET XACT_ABORT ON;

    DECLARE @full nvarchar(300), @perm varchar(60), @retire sysname, @allowed bit, @title varchar(80);
    SELECT @full = QUOTENAME(d.SchemaName) + N'.' + QUOTENAME(d.TableName),
           @perm = d.PermissionCode, @retire = d.RetireColumn, @title = d.Title
    FROM core.MaintDef d WHERE d.MaintCode = @MaintCode AND d.IsActive = 1;
    IF @full IS NULL THROW 50090, 'STERP-E-0090: Maintenance definition is not registered.', 1;
    IF OBJECT_ID(@full) IS NULL THROW 50092, 'STERP-E-0092: The file this screen maintains is not installed yet.', 1;

    EXEC core.usp_UiCheckAccess @UserCode, @perm, @CompanyCode, NULL, @allowed OUTPUT;
    IF @allowed = 0 THROW 50091, 'STERP-E-0091: You are not authorised to amend this file.', 1;

    SET @Mode = UPPER(LTRIM(RTRIM(@Mode)));
    IF @Mode NOT IN ('ADD', 'CHANGE', 'RETIRE', 'REINSTATE')
        THROW 50094, 'STERP-E-0094: Unknown maintenance action.', 1;

    CREATE TABLE #v (ColumnName sysname COLLATE DATABASE_DEFAULT NOT NULL PRIMARY KEY, Val nvarchar(400) COLLATE DATABASE_DEFAULT NULL, IsNull bit NOT NULL);
    INSERT #v (ColumnName, Val, IsNull)
        SELECT x.value('@n', 'sysname'), x.value('@v', 'nvarchar(400)'),
               ISNULL(x.value('@null', 'bit'), 0)
        FROM @Data.nodes('/r/c') AS t(x)
        WHERE x.value('@n', 'sysname') IS NOT NULL;

    /* drop anything that is not a registered, writable column of this screen */
    DELETE v FROM #v v
    WHERE NOT EXISTS (SELECT 1 FROM core.MaintColumn c
                      WHERE c.MaintCode = @MaintCode AND c.ColumnName = v.ColumnName
                        AND c.IsReadOnly = 0 AND COL_LENGTH(@full, c.ColumnName) IS NOT NULL);

    DECLARE @keyWhere nvarchar(max), @keyText varchar(200), @sql nvarchar(max), @set nvarchar(max), @list nvarchar(max), @vals nvarchar(max);

    SELECT @keyWhere = STRING_AGG(CONVERT(nvarchar(max),
               QUOTENAME(c.ColumnName) + N' = (SELECT v.Val FROM #v v WHERE v.ColumnName = ' +
               QUOTENAME(c.ColumnName, '''') + N')'), N' AND ')
    FROM core.MaintColumn c
    WHERE c.MaintCode = @MaintCode AND c.IsKey = 1 AND COL_LENGTH(@full, c.ColumnName) IS NOT NULL;

    SELECT @keyText = STRING_AGG(CONVERT(varchar(max), ISNULL(v.Val, '')), '/')
                      WITHIN GROUP (ORDER BY c.SortOrder, c.ColumnName)
    FROM #v v JOIN core.MaintColumn c ON c.MaintCode = @MaintCode AND c.ColumnName = v.ColumnName
    WHERE c.IsKey = 1;

    IF @Mode = 'ADD'
    BEGIN
        SELECT @list = STRING_AGG(CONVERT(nvarchar(max), QUOTENAME(v.ColumnName)), N', '),
               @vals = STRING_AGG(CONVERT(nvarchar(max),
                   N'(SELECT CASE WHEN v.IsNull = 1 THEN NULL ELSE v.Val END FROM #v v WHERE v.ColumnName = ' +
                   QUOTENAME(v.ColumnName, '''') + N')'), N', ')
        FROM #v v;
        IF @list IS NULL THROW 50095, 'STERP-E-0095: Nothing to save.', 1;
        IF COL_LENGTH(@full, 'CreatedBy') IS NOT NULL
            SELECT @list = @list + N', [CreatedBy]', @vals = @vals + N', @usr';
        SET @sql = N'INSERT ' + @full + N' (' + @list + N') SELECT ' + @vals + N';';
    END
    ELSE IF @Mode = 'CHANGE'
    BEGIN
        IF @keyWhere IS NULL THROW 50096, 'STERP-E-0096: This screen has no key columns registered.', 1;
        SELECT @set = STRING_AGG(CONVERT(nvarchar(max),
                   QUOTENAME(v.ColumnName) + N' = (SELECT CASE WHEN v2.IsNull = 1 THEN NULL ELSE v2.Val END' +
                   N' FROM #v v2 WHERE v2.ColumnName = ' + QUOTENAME(v.ColumnName, '''') + N')'), N', ')
        FROM #v v JOIN core.MaintColumn c ON c.MaintCode = @MaintCode AND c.ColumnName = v.ColumnName
        WHERE c.IsKey = 0;
        IF @set IS NULL THROW 50095, 'STERP-E-0095: Nothing to save.', 1;
        IF COL_LENGTH(@full, 'ModifiedBy') IS NOT NULL SET @set = @set + N', [ModifiedBy] = @usr';
        IF COL_LENGTH(@full, 'ModifiedAt') IS NOT NULL SET @set = @set + N', [ModifiedAt] = SYSDATETIME()';
        SET @sql = N'UPDATE ' + @full + N' SET ' + @set + N' WHERE ' + @keyWhere + N';';
    END
    ELSE
    BEGIN
        IF @retire IS NULL OR COL_LENGTH(@full, @retire) IS NULL
            THROW 50097, 'STERP-E-0097: This file has no retire flag - rows cannot be withdrawn.', 1;
        IF @keyWhere IS NULL THROW 50096, 'STERP-E-0096: This screen has no key columns registered.', 1;
        SET @sql = N'UPDATE ' + @full + N' SET ' + QUOTENAME(@retire) + N' = ' +
                   CASE WHEN @Mode = 'RETIRE' THEN N'0' ELSE N'1' END;
        IF COL_LENGTH(@full, 'ModifiedBy') IS NOT NULL SET @sql = @sql + N', [ModifiedBy] = @usr';
        IF COL_LENGTH(@full, 'ModifiedAt') IS NOT NULL SET @sql = @sql + N', [ModifiedAt] = SYSDATETIME()';
        SET @sql = @sql + N' WHERE ' + @keyWhere + N';';
    END

    DECLARE @rows int;
    BEGIN TRAN;
        EXEC sp_executesql @sql, N'@usr varchar(20)', @usr = @UserCode;
        SET @rows = @@ROWCOUNT;
        IF @Mode <> 'ADD' AND @rows = 0
        BEGIN
            ROLLBACK TRAN;
            THROW 50098, 'STERP-E-0098: The record was not found - it may have been amended by another user.', 1;
        END

        /* COM-013 audit trail; written through dynamic SQL so this procedure deploys
           before the audit module does. */
        IF OBJECT_ID(N'core.AuditLog') IS NOT NULL
        BEGIN
            BEGIN TRY
                EXEC sp_executesql
                    N'INSERT core.AuditLog (EventTime, UserCode, CompanyCode, EntityType, EntityKey, Action, Reason, Source)
                      VALUES (SYSDATETIME(), @u, @c, @et, @ek, @a, @r, ''STERP.exe'')',
                    N'@u varchar(20), @c varchar(10), @et varchar(60), @ek varchar(100), @a varchar(20), @r varchar(200)',
                    @u = @UserCode, @c = @CompanyCode, @et = @MaintCode, @ek = @keyText, @a = @Mode, @r = @title;
            END TRY
            BEGIN CATCH
            END CATCH
        END
    COMMIT TRAN;

    SELECT @rows AS RowsAffected, @keyText AS EntityKey, @Mode AS Action;
END
GO
