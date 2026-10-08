/*  core.usp_RequestApproval / core.usp_DecideApproval / core.usp_InvalidateApprovals.
    The preparer can never approve their own item, a segregation-of-duties conflict blocks the
    decision, delegation is named and time bounded (maximum 30 days), and a material change to the
    document content invalidates the approval.  REQ: COM-003, COM-008, COM-022, D-07 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE core.usp_RequestApproval @CompanyCode varchar(10), @DocumentType varchar(20), @DocumentRef varchar(40), @ApprovalType varchar(30),
    @ContentHash varchar(64), @RequestedBy varchar(20), @Amount decimal(18,2) = NULL, @CurrencyCode char(3) = 'GBP', @AmountGBP decimal(18,2) = NULL,
    @DueAt datetime2(3) = NULL, @CorrelationId varchar(64) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    SET @AmountGBP = ISNULL(@AmountGBP, @Amount);
    DECLARE @perm varchar(40), @role varchar(20), @id bigint;
    SELECT TOP (1) @perm = RequiredPermission, @role = RequiredRole FROM core.ApprovalRule
     WHERE ApprovalType = @ApprovalType AND CompanyCode IN (@CompanyCode, '*') AND IsActive = 1
       AND ISNULL(@AmountGBP, 0) >= MinAmount AND (MaxAmount IS NULL OR ISNULL(@AmountGBP, 0) <= MaxAmount)
     ORDER BY CASE WHEN CompanyCode = @CompanyCode THEN 0 ELSE 1 END, SeqNo;
    IF @perm IS NULL THROW 50021, 'STERP-E-0021: No approval rule covers this action and value. Maintain the approval matrix before requesting approval.', 1;
    UPDATE core.Approval SET Status = 'INVALIDATED', InvalidatedReason = 'Material change to the approved content'
     WHERE CompanyCode = @CompanyCode AND DocumentType = @DocumentType AND DocumentRef = @DocumentRef AND Status IN ('PENDING', 'APPROVED') AND ContentHash <> @ContentHash;
    SELECT TOP (1) @id = ApprovalId FROM core.Approval
     WHERE CompanyCode = @CompanyCode AND DocumentType = @DocumentType AND DocumentRef = @DocumentRef AND ApprovalType = @ApprovalType
       AND ContentHash = @ContentHash AND Status = 'PENDING' ORDER BY ApprovalId DESC;
    IF @id IS NULL
    BEGIN
        INSERT core.Approval (CompanyCode, DocumentType, DocumentRef, ApprovalType, Amount, CurrencyCode, AmountGBP, ContentHash, RequiredPermission, RequiredRole, RequestedBy, DueAt, CorrelationId)
        VALUES (@CompanyCode, @DocumentType, @DocumentRef, @ApprovalType, @Amount, @CurrencyCode, @AmountGBP, @ContentHash, @perm, @role, @RequestedBy, @DueAt, @CorrelationId);
        SET @id = CAST(SCOPE_IDENTITY() AS bigint);
    END
    SELECT ApprovalId, CompanyCode, DocumentType, DocumentRef, ApprovalType, Amount, AmountGBP, ContentHash, RequiredPermission, RequiredRole, RequestedBy, RequestedAt, DueAt, Status
      FROM core.Approval WHERE ApprovalId = @id;
END
GO
CREATE OR ALTER PROCEDURE core.usp_DecideApproval @ApprovalId bigint, @UserCode varchar(20), @Decision varchar(10), @Comment varchar(400) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    IF @Decision NOT IN ('APPROVED', 'REJECTED') THROW 50021, 'STERP-E-0021: A decision must be APPROVED or REJECTED.', 1;
    DECLARE @co varchar(10), @req varchar(20), @perm varchar(40), @st varchar(14), @amt decimal(18,2), @atype varchar(30), @del int, @today date = CAST(SYSDATETIME() AS date);
    SELECT @co = CompanyCode, @req = RequestedBy, @perm = RequiredPermission, @st = Status, @amt = ISNULL(AmountGBP, 0), @atype = ApprovalType FROM core.Approval WHERE ApprovalId = @ApprovalId;
    IF @st IS NULL THROW 50021, 'STERP-E-0021: That approval request does not exist.', 1;
    IF @st <> 'PENDING' THROW 50021, 'STERP-E-0021: That approval is no longer pending, so it cannot be decided again.', 1;
    IF @UserCode = @req THROW 50006, 'STERP-E-0006: The preparer cannot approve their own item. An independent approver is required.', 1;
    IF EXISTS (SELECT 1 FROM core.SoDRule s WHERE s.IsActive = 1 AND s.Severity = 'BLOCK' AND (s.ApprovalType IS NULL OR s.ApprovalType = @atype)
                 AND core.fn_HasPermission(@UserCode, s.PermissionA, @co, NULL) = 1 AND core.fn_HasPermission(@UserCode, s.PermissionB, @co, NULL) = 1)
        THROW 50007, 'STERP-E-0007: Segregation of duties prevents this user from approving an action they can also originate.', 1;
    IF core.fn_HasPermission(@UserCode, @perm, @co, NULL) = 0
    BEGIN
        SELECT TOP (1) @del = d.DelegationId FROM core.Delegation d
         WHERE d.ToUser = @UserCode AND d.Status = 'ACTIVE' AND d.CompanyCode IN (@co, '*') AND d.ValidFrom <= @today AND d.ValidTo >= @today
           AND DATEDIFF(day, d.ValidFrom, d.ValidTo) <= 30 AND (d.ApprovalType IS NULL OR d.ApprovalType = @atype) AND (d.MaxAmount IS NULL OR d.MaxAmount >= @amt)
           AND core.fn_HasPermission(d.FromUser, @perm, @co, NULL) = 1 AND d.FromUser <> @req ORDER BY d.DelegationId DESC;
        IF @del IS NULL THROW 50008, 'STERP-E-0008: This user does not hold the required approval authority, and no valid named delegation applies.', 1;
    END
    UPDATE core.Approval SET Status = @Decision, DecidedBy = @UserCode, DecidedAt = SYSDATETIME(), Comment = @Comment, DelegationId = @del WHERE ApprovalId = @ApprovalId;
    INSERT core.AuditLog (UserCode, CompanyCode, EntityType, EntityKey, Action, OldValue, NewValue, Reason, AuthorityRef, Source)
    VALUES (@UserCode, @co, 'core.Approval', CAST(@ApprovalId AS varchar(20)), @Decision, 'PENDING', @Decision, @Comment, CASE WHEN @del IS NULL THEN NULL ELSE 'Delegation ' + CAST(@del AS varchar(10)) END, 'SQL');
    SELECT ApprovalId, CompanyCode, DocumentType, DocumentRef, ApprovalType, AmountGBP, Status, RequestedBy, DecidedBy, DecidedAt, DelegationId, Comment FROM core.Approval WHERE ApprovalId = @ApprovalId;
END
GO
CREATE OR ALTER PROCEDURE core.usp_InvalidateApprovals @CompanyCode varchar(10), @DocumentType varchar(20), @DocumentRef varchar(40), @ContentHash varchar(64),
    @UserCode varchar(20) = NULL, @Reason varchar(200) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    SET @Reason = ISNULL(@Reason, 'Material change to the approved content');
    DECLARE @changed TABLE (ApprovalId bigint, PriorStatus varchar(14));
    UPDATE a SET Status = 'INVALIDATED', InvalidatedReason = @Reason
      OUTPUT inserted.ApprovalId, deleted.Status INTO @changed
      FROM core.Approval a
     WHERE a.CompanyCode = @CompanyCode AND a.DocumentType = @DocumentType AND a.DocumentRef = @DocumentRef
       AND a.Status IN ('PENDING', 'APPROVED') AND a.ContentHash <> @ContentHash;
    INSERT core.AuditLog (UserCode, CompanyCode, EntityType, EntityKey, Action, OldValue, NewValue, Reason, Source)
    SELECT @UserCode, @CompanyCode, 'core.Approval', CAST(ApprovalId AS varchar(20)), 'INVALIDATED', PriorStatus, 'INVALIDATED', @Reason, 'SQL' FROM @changed;
    SELECT (SELECT COUNT(*) FROM @changed) AS InvalidatedCount, @DocumentType AS DocumentType, @DocumentRef AS DocumentRef, @ContentHash AS ContentHash;
END
GO
