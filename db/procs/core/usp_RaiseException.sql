/*  core.usp_RaiseException / core.usp_ResolveException / core.usp_AgeExceptions - the owned,
    aged exception worklist.  A notification is not approval and reassignment does not reset age.
    REQ: COM-017, COM-022, XMD-006 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE core.usp_RaiseException @CompanyCode varchar(10), @Module varchar(6), @Severity varchar(12), @Category varchar(40), @Message varchar(400),
    @DocumentType varchar(20) = NULL, @DocumentRef varchar(40) = NULL, @SiteCode varchar(10) = NULL, @OwnerUser varchar(20) = NULL, @OwnerRole varchar(20) = NULL,
    @Priority tinyint = 5, @DueAt datetime2(3) = NULL, @AmountGBP decimal(18,2) = NULL, @Quantity decimal(19,6) = NULL, @RaisedBy varchar(20) = NULL, @CorrelationId varchar(64) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    IF @Severity NOT IN ('HOLD', 'WARNING', 'ERROR') THROW 50017, 'STERP-E-0017: Severity must be HOLD, WARNING or ERROR so that holds, warnings and hard errors stay distinguishable.', 1;
    /*  COM-022 - an exception is owned by a person.  A caller that knows only the responsible
        role still gets a named owner: the active holder of that role for the company.  */
    IF NULLIF(@OwnerUser, '') IS NULL AND NULLIF(@OwnerRole, '') IS NOT NULL
        SELECT TOP (1) @OwnerUser = u.UserCode
          FROM core.UserRole r JOIN core.AppUser u ON u.UserCode = r.UserCode
         WHERE r.RoleCode = @OwnerRole AND u.IsActive = 1
           AND (r.CompanyCode = @CompanyCode OR r.CompanyCode = '*')
           AND (r.ValidTo IS NULL OR r.ValidTo >= CAST(SYSDATETIME() AS date))
         ORDER BY CASE WHEN r.CompanyCode = @CompanyCode THEN 0 ELSE 1 END, u.UserCode;
    DECLARE @id bigint;
    SELECT TOP (1) @id = ExceptionId FROM core.ExceptionItem
     WHERE CompanyCode = @CompanyCode AND Module = @Module AND Category = @Category AND Status <> 'RESOLVED'
       AND ISNULL(DocumentType, '') = ISNULL(@DocumentType, '') AND ISNULL(DocumentRef, '') = ISNULL(@DocumentRef, '');
    IF @id IS NULL
    BEGIN
        INSERT core.ExceptionItem (CompanyCode, SiteCode, Module, Severity, Category, DocumentType, DocumentRef, Message, AmountGBP, Quantity, OwnerUser, OwnerRole, Priority, RaisedBy, DueAt, CorrelationId)
        VALUES (@CompanyCode, @SiteCode, @Module, @Severity, @Category, @DocumentType, @DocumentRef, @Message, @AmountGBP, @Quantity, @OwnerUser, @OwnerRole, @Priority, @RaisedBy, @DueAt, @CorrelationId);
        SET @id = CAST(SCOPE_IDENTITY() AS bigint);
    END
    ELSE UPDATE core.ExceptionItem SET Message = @Message, Severity = @Severity, Priority = @Priority, OwnerUser = ISNULL(@OwnerUser, OwnerUser) WHERE ExceptionId = @id;
    SELECT ExceptionId, CompanyCode, Module, Severity, Category, DocumentType, DocumentRef, Message, OwnerUser, OwnerRole, Priority, RaisedAt, DueAt, Status
      FROM core.ExceptionItem WHERE ExceptionId = @id;
END
GO
CREATE OR ALTER PROCEDURE core.usp_ResolveException @ExceptionId bigint, @UserCode varchar(20), @ResolutionNote varchar(400), @Status varchar(14) = 'RESOLVED'
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    IF @Status NOT IN ('ACKNOWLEDGED', 'RESOLVED') THROW 50017, 'STERP-E-0017: An exception can only be acknowledged or resolved.', 1;
    IF NOT EXISTS (SELECT 1 FROM core.ExceptionItem WHERE ExceptionId = @ExceptionId) THROW 50017, 'STERP-E-0017: That exception does not exist.', 1;
    IF @Status = 'RESOLVED' AND (@ResolutionNote IS NULL OR LEN(LTRIM(@ResolutionNote)) = 0)
        THROW 50017, 'STERP-E-0017: Resolution evidence is required before an exception can be closed.', 1;
    UPDATE core.ExceptionItem SET Status = @Status, ResolvedBy = CASE WHEN @Status = 'RESOLVED' THEN @UserCode ELSE ResolvedBy END,
           ResolvedAt = CASE WHEN @Status = 'RESOLVED' THEN SYSDATETIME() ELSE ResolvedAt END, ResolutionNote = @ResolutionNote
     WHERE ExceptionId = @ExceptionId;
    INSERT core.AuditLog (UserCode, CompanyCode, EntityType, EntityKey, Action, NewValue, Reason, Source)
    SELECT @UserCode, CompanyCode, 'core.ExceptionItem', CAST(@ExceptionId AS varchar(20)), @Status, @Status, @ResolutionNote, 'SQL' FROM core.ExceptionItem WHERE ExceptionId = @ExceptionId;
    SELECT ExceptionId, Status, ResolvedBy, ResolvedAt, ResolutionNote FROM core.ExceptionItem WHERE ExceptionId = @ExceptionId;
END
GO
/*  Nightly ageing: refresh age, escalate overdue items to the financial controller's worklist. */
CREATE OR ALTER PROCEDURE core.usp_AgeExceptions @AsOf datetime2(3) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    SET @AsOf = ISNULL(@AsOf, SYSDATETIME());
    UPDATE core.ExceptionItem SET AgeDays = DATEDIFF(day, RaisedAt, @AsOf) WHERE Status <> 'RESOLVED';
    UPDATE core.ExceptionItem SET EscalatedAt = @AsOf, Priority = CASE WHEN Priority > 1 THEN Priority - 1 ELSE 1 END
     WHERE Status <> 'RESOLVED' AND EscalatedAt IS NULL AND DueAt IS NOT NULL AND DueAt < @AsOf;
    SELECT CompanyCode, Module, Severity, COUNT(*) AS OpenCount, MAX(AgeDays) AS OldestDays,
           SUM(CASE WHEN EscalatedAt IS NOT NULL THEN 1 ELSE 0 END) AS EscalatedCount
      FROM core.ExceptionItem WHERE Status <> 'RESOLVED' GROUP BY CompanyCode, Module, Severity ORDER BY CompanyCode, Module, Severity;
END
GO
