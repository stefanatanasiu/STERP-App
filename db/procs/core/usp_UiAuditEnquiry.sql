/* core.usp_UiAuditEnquiry - audit trail enquiry form.                      COM-013 */
CREATE OR ALTER PROCEDURE core.usp_UiAuditEnquiry
    @CompanyCode varchar(10)  = NULL,
    @EntityType  varchar(60)  = NULL,
    @EntityKey   varchar(100) = NULL,
    @FromDate    date         = NULL,
    @ToDate      date         = NULL,
    @UserCode    varchar(20)  = NULL,
    @Top         int          = 1000
AS
BEGIN
    SET NOCOUNT ON;
    IF OBJECT_ID(N'core.AuditLog') IS NULL
    BEGIN
        SELECT CONVERT(datetime2(3), NULL) AS EventTime, CONVERT(varchar(20), NULL) AS UserCode,
               CONVERT(varchar(10), NULL) AS CompanyCode, CONVERT(varchar(60), NULL) AS EntityType,
               CONVERT(varchar(100), NULL) AS EntityKey, CONVERT(varchar(20), NULL) AS Action,
               CONVERT(varchar(60), NULL) AS FieldName, CONVERT(varchar(200), NULL) AS OldValue,
               CONVERT(varchar(200), NULL) AS NewValue, CONVERT(varchar(200), NULL) AS Reason,
               CONVERT(varchar(40), NULL) AS Source
        WHERE 1 = 0;
        RETURN;
    END
    SELECT TOP (@Top) a.EventTime, a.UserCode, a.CompanyCode, a.EntityType, a.EntityKey,
           a.Action, a.FieldName, a.OldValue, a.NewValue, a.Reason, a.Source
    FROM core.AuditLog a
    WHERE (@CompanyCode IS NULL OR a.CompanyCode = @CompanyCode)
      AND (@EntityType IS NULL OR @EntityType = '' OR a.EntityType LIKE '%' + @EntityType + '%')
      AND (@EntityKey IS NULL OR @EntityKey = '' OR a.EntityKey LIKE '%' + @EntityKey + '%')
      AND (@UserCode IS NULL OR @UserCode = '' OR a.UserCode = @UserCode)
      AND (@FromDate IS NULL OR a.EventTime >= CONVERT(datetime2(3), @FromDate))
      AND (@ToDate IS NULL OR a.EventTime < DATEADD(day, 1, CONVERT(datetime2(3), @ToDate)))
    ORDER BY a.EventTime DESC;
END
GO
