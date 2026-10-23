/* core.usp_UiReportList - reports the signed on user may run (core.ReportDef).  */
CREATE OR ALTER PROCEDURE core.usp_UiReportList
    @UserCode    varchar(20) = NULL,
    @CompanyCode varchar(10) = NULL,
    @Module      varchar(10) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    IF OBJECT_ID(N'core.ReportDef') IS NULL
    BEGIN
        SELECT CONVERT(varchar(20), NULL) AS ReportCode, CONVERT(varchar(10), NULL) AS Module,
               CONVERT(varchar(80), NULL) AS Title, CONVERT(varchar(20), NULL) AS ProgramName,
               CONVERT(varchar(100), NULL) AS ParamSpec, CONVERT(varchar(60), NULL) AS PermissionCode
        WHERE 1 = 0;
        RETURN;
    END

    CREATE TABLE #r (ReportCode varchar(20) COLLATE DATABASE_DEFAULT NOT NULL PRIMARY KEY, Allowed bit NOT NULL);
    INSERT #r (ReportCode, Allowed) SELECT d.ReportCode, 1 FROM core.ReportDef d
        WHERE d.IsActive = 1 AND (@Module IS NULL OR @Module = '' OR d.Module = @Module);

    DECLARE @rc varchar(20), @pc varchar(60), @ok bit;
    DECLARE rpt_cur CURSOR LOCAL FAST_FORWARD FOR
        SELECT d.ReportCode, d.PermissionCode FROM core.ReportDef d
        JOIN #r r ON r.ReportCode = d.ReportCode WHERE d.PermissionCode IS NOT NULL;
    OPEN rpt_cur;
    FETCH NEXT FROM rpt_cur INTO @rc, @pc;
    WHILE @@FETCH_STATUS = 0
    BEGIN
        EXEC core.usp_UiCheckAccess @UserCode, @pc, @CompanyCode, NULL, @ok OUTPUT;
        UPDATE #r SET Allowed = ISNULL(@ok, 0) WHERE ReportCode = @rc;
        FETCH NEXT FROM rpt_cur INTO @rc, @pc;
    END
    CLOSE rpt_cur;
    DEALLOCATE rpt_cur;

    SELECT d.ReportCode, d.Module, d.Title, d.ProgramName, d.ParamSpec, d.PermissionCode
    FROM core.ReportDef d JOIN #r r ON r.ReportCode = d.ReportCode AND r.Allowed = 1
    ORDER BY d.Module, d.Title;
END
GO
