/* core.usp_UiExceptionList - the exception worklist (holds, warnings, errors).
   Resolution goes through core.usp_ResolveException.                     COM-017 */
CREATE OR ALTER PROCEDURE core.usp_UiExceptionList
    @CompanyCode varchar(10) = NULL,
    @Status      varchar(20) = 'OPEN',
    @Severity    varchar(20) = NULL,
    @OwnerUser   varchar(20) = NULL
AS
BEGIN
    SET NOCOUNT ON;
    IF OBJECT_ID(N'core.ExceptionItem') IS NULL
    BEGIN
        SELECT CONVERT(int, NULL) AS ExceptionId, CONVERT(varchar(10), NULL) AS CompanyCode,
               CONVERT(varchar(10), NULL) AS Module, CONVERT(varchar(10), NULL) AS Severity,
               CONVERT(varchar(30), NULL) AS Category, CONVERT(varchar(20), NULL) AS DocumentType,
               CONVERT(varchar(40), NULL) AS DocumentRef, CONVERT(varchar(400), NULL) AS Message,
               CONVERT(varchar(20), NULL) AS OwnerUser, CONVERT(int, NULL) AS Priority,
               CONVERT(datetime2(3), NULL) AS RaisedAt, CONVERT(varchar(20), NULL) AS Status,
               CONVERT(int, NULL) AS AgeDays
        WHERE 1 = 0;
        RETURN;
    END
    SELECT e.ExceptionId, e.CompanyCode, e.Module, e.Severity, e.Category, e.DocumentType,
           e.DocumentRef, e.Message, e.OwnerUser, e.Priority, e.RaisedAt, e.Status,
           DATEDIFF(day, e.RaisedAt, SYSDATETIME()) AS AgeDays
    FROM core.ExceptionItem e
    WHERE (@CompanyCode IS NULL OR e.CompanyCode = @CompanyCode)
      AND (@Status IS NULL OR @Status = '' OR @Status = 'ALL' OR e.Status = @Status)
      AND (@Severity IS NULL OR @Severity = '' OR @Severity = 'ALL' OR e.Severity = @Severity)
      AND (@OwnerUser IS NULL OR @OwnerUser = '' OR e.OwnerUser = @OwnerUser)
    ORDER BY e.Priority, e.RaisedAt;
END
GO
