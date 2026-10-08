/* core.usp_UiApprovalInbox - pending approvals for the approvals inbox form.
   Decisions are taken by core.usp_DecideApproval (self-approval refused). COM-003 */
CREATE OR ALTER PROCEDURE core.usp_UiApprovalInbox
    @CompanyCode varchar(10) = NULL,
    @UserCode    varchar(20) = NULL,
    @Status      varchar(20) = 'PENDING'
AS
BEGIN
    SET NOCOUNT ON;
    IF OBJECT_ID(N'core.Approval') IS NULL
    BEGIN
        SELECT CONVERT(int, NULL) AS ApprovalId, CONVERT(varchar(10), NULL) AS CompanyCode,
               CONVERT(varchar(20), NULL) AS DocumentType, CONVERT(varchar(40), NULL) AS DocumentRef,
               CONVERT(varchar(30), NULL) AS ApprovalType, CONVERT(decimal(18, 2), NULL) AS Amount,
               CONVERT(varchar(20), NULL) AS RequestedBy, CONVERT(datetime2(3), NULL) AS RequestedAt,
               CONVERT(varchar(20), NULL) AS Status, CONVERT(int, NULL) AS AgeDays
        WHERE 1 = 0;
        RETURN;
    END
    SELECT a.ApprovalId, a.CompanyCode, a.DocumentType, a.DocumentRef, a.ApprovalType, a.Amount,
           a.RequestedBy, a.RequestedAt, a.Status,
           DATEDIFF(day, a.RequestedAt, SYSDATETIME()) AS AgeDays
    FROM core.Approval a
    WHERE (@CompanyCode IS NULL OR a.CompanyCode = @CompanyCode)
      AND (@Status IS NULL OR @Status = '' OR @Status = 'ALL' OR a.Status = @Status)
    ORDER BY a.RequestedAt DESC;
END
GO
