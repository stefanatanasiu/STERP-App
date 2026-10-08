/* core.usp_UiJobList - batch job monitor over core.JobRun.               CONVENTIONS 3.6 */
CREATE OR ALTER PROCEDURE core.usp_UiJobList
    @CompanyCode varchar(10) = NULL,
    @FromDate    date        = NULL,
    @Status      varchar(20) = NULL,
    @Top         int         = 500
AS
BEGIN
    SET NOCOUNT ON;
    IF OBJECT_ID(N'core.JobRun') IS NULL
    BEGIN
        SELECT CONVERT(int, NULL) AS JobRunId, CONVERT(varchar(40), NULL) AS JobName,
               CONVERT(varchar(10), NULL) AS CompanyCode, CONVERT(varchar(20), NULL) AS SubmittedBy,
               CONVERT(datetime2(3), NULL) AS StartedAt, CONVERT(datetime2(3), NULL) AS EndedAt,
               CONVERT(varchar(20), NULL) AS Status, CONVERT(int, NULL) AS ReturnCode,
               CONVERT(varchar(260), NULL) AS LogFile, CONVERT(varchar(260), NULL) AS SpoolFile,
               CONVERT(int, NULL) AS Seconds
        WHERE 1 = 0;
        RETURN;
    END
    SELECT TOP (@Top) j.JobRunId, j.JobName, j.CompanyCode, j.SubmittedBy, j.StartedAt, j.EndedAt,
           j.Status, j.ReturnCode, j.LogFile, j.SpoolFile,
           DATEDIFF(second, j.StartedAt, ISNULL(j.EndedAt, SYSDATETIME())) AS Seconds
    FROM core.JobRun j
    WHERE (@CompanyCode IS NULL OR j.CompanyCode = @CompanyCode OR j.CompanyCode IS NULL)
      AND (@FromDate IS NULL OR j.StartedAt >= CONVERT(datetime2(3), @FromDate))
      AND (@Status IS NULL OR @Status = '' OR @Status = 'ALL' OR j.Status = @Status)
    ORDER BY j.JobRunId DESC;
END
GO
