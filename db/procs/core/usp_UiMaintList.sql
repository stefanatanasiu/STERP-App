/* core.usp_UiMaintList - rows for the list grid of a generic maintenance screen.
   The SELECT is built from core.MaintColumn only: every identifier goes through
   QUOTENAME and every user value is a sp_executesql parameter, so no user text ever
   reaches the SQL text.
   @ForLookup = 1 is the read-only F2 pick list (core.usp_UiLookup): choosing a code
   for a field is not opening the maintenance screen, so the maintain permission is
   not asked for - the company scoping still applies.                      COM-002 */
CREATE OR ALTER PROCEDURE core.usp_UiMaintList
    @MaintCode   varchar(30),
    @UserCode    varchar(20)   = NULL,
    @CompanyCode varchar(10)   = NULL,
    @Search      nvarchar(100) = NULL,
    @Top         int           = 500,
    @ForLookup   bit           = 0
AS
BEGIN
    SET NOCOUNT ON;
    DECLARE @full nvarchar(300), @scoped bit, @perm varchar(60), @allowed bit;
    SELECT @full = QUOTENAME(d.SchemaName) + N'.' + QUOTENAME(d.TableName),
           @scoped = d.CompanyScoped, @perm = d.PermissionCode
    FROM core.MaintDef d WHERE d.MaintCode = @MaintCode AND d.IsActive = 1;
    IF @full IS NULL THROW 50090, 'STERP-E-0090: Maintenance definition is not registered.', 1;
    IF OBJECT_ID(@full) IS NULL THROW 50092, 'STERP-E-0092: The file this screen maintains is not installed yet.', 1;

    IF ISNULL(@ForLookup, 0) = 1
        SET @allowed = 1;
    ELSE
        EXEC core.usp_UiCheckAccess @UserCode, @perm, @CompanyCode, NULL, @allowed OUTPUT;
    IF @allowed = 0 THROW 50091, 'STERP-E-0091: You are not authorised to open this file maintenance.', 1;

    DECLARE @cols nvarchar(max), @keys nvarchar(max), @where nvarchar(max) = N'', @sql nvarchar(max);
    SELECT @cols = STRING_AGG(QUOTENAME(c.ColumnName), N', ') WITHIN GROUP (ORDER BY c.SortOrder, c.ColumnName)
    FROM core.MaintColumn c
    WHERE c.MaintCode = @MaintCode AND (c.IsInList = 1 OR c.IsKey = 1)
      AND COL_LENGTH(@full, c.ColumnName) IS NOT NULL;
    IF @cols IS NULL THROW 50093, 'STERP-E-0093: No columns are registered for this maintenance screen.', 1;

    SELECT @keys = STRING_AGG(QUOTENAME(c.ColumnName), N', ') WITHIN GROUP (ORDER BY c.SortOrder, c.ColumnName)
    FROM core.MaintColumn c
    WHERE c.MaintCode = @MaintCode AND c.IsKey = 1 AND COL_LENGTH(@full, c.ColumnName) IS NOT NULL;

    IF @scoped = 1 AND COL_LENGTH(@full, 'CompanyCode') IS NOT NULL AND @CompanyCode IS NOT NULL
        SET @where = @where + N' AND ([CompanyCode] = @co OR [CompanyCode] = ''*'')';

    IF @Search IS NOT NULL AND LTRIM(RTRIM(@Search)) <> ''
    BEGIN
        DECLARE @like nvarchar(max);
        SELECT @like = STRING_AGG(CONVERT(nvarchar(max), QUOTENAME(c.ColumnName) + N' LIKE @sv'), N' OR ')
        FROM core.MaintColumn c
        WHERE c.MaintCode = @MaintCode AND (c.IsSearchable = 1 OR c.IsKey = 1)
          AND c.DataType IN ('TEXT', 'CODE') AND COL_LENGTH(@full, c.ColumnName) IS NOT NULL;
        IF @like IS NOT NULL SET @where = @where + N' AND (' + @like + N')';
    END

    SET @sql = N'SELECT TOP (@top) ' + @cols + N' FROM ' + @full + N' WHERE 1 = 1' + @where +
               N' ORDER BY ' + ISNULL(@keys, @cols);
    DECLARE @pattern nvarchar(120) = N'%' + ISNULL(LTRIM(RTRIM(@Search)), N'') + N'%';
    EXEC sp_executesql @sql,
         N'@co varchar(10), @sv nvarchar(120), @top int',
         @co = @CompanyCode, @sv = @pattern, @top = @Top;
END
GO
