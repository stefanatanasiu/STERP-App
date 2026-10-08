/*  core.usp_WriteAudit - attributable history of a material change.  REQ: COM-013, COM-021 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE core.usp_WriteAudit @UserCode varchar(20), @CompanyCode varchar(10), @EntityType varchar(60), @EntityKey varchar(120), @Action varchar(20),
    @FieldName varchar(60) = NULL, @OldValue varchar(400) = NULL, @NewValue varchar(400) = NULL, @Reason varchar(200) = NULL,
    @AuthorityRef varchar(60) = NULL, @Source varchar(20) = 'SQL', @CorrelationId varchar(64) = NULL, @IsSensitive bit = 0
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    INSERT core.AuditLog (EventTime, UserCode, CompanyCode, EntityType, EntityKey, Action, FieldName, OldValue, NewValue, Reason, AuthorityRef, Source, CorrelationId, IsSensitive)
    VALUES (SYSDATETIME(), @UserCode, @CompanyCode, @EntityType, @EntityKey, @Action, @FieldName, @OldValue, @NewValue, @Reason, @AuthorityRef, @Source, @CorrelationId, @IsSensitive);
    SELECT CAST(SCOPE_IDENTITY() AS bigint) AS AuditId;
END
GO
