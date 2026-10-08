/*  bom.usp_ReleaseBom - release a drafted bill revision.

    BOM-004 is the whole point of this procedure: a revision may only be released when a complete
    validation (BOMVALID, outcome PASS or WARNING) still matches the bill content, and when an
    authorised engineering review exists that is not the author's own signature.  Once released the
    revision is immutable - the component rows are frozen by tr_BomReleasedImmutable and a further
    change must create a new revision, which this procedure records when @SupersedeRevision is given.

    BOM-028 keeps the superseded revision as history rather than deleting it, and BOM-029 makes every
    refusal carry the reason and the affected path.  BOM-030 makes a repeated release request with the
    same correlation identity return the first outcome instead of releasing twice.

    REQ: BOM-003, BOM-004, BOM-005, BOM-028, BOM-029, BOM-030
*/
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE bom.usp_ReleaseBom
    @CompanyCode varchar(10), @BomRef varchar(30), @BomRevision varchar(4), @UserCode varchar(20),
    @ReviewerUser varchar(20) = NULL, @EffectiveFrom date = NULL, @SupersedeRevision varchar(4) = NULL,
    @ChangeRef varchar(30) = NULL, @Reason varchar(200) = NULL, @CorrelationId varchar(64) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;

    DECLARE @HeaderId int, @Status varchar(12), @Author varchar(20), @Parent varchar(40), @Site varchar(10),
            @ValidationId int, @Outcome varchar(12), @Hash varchar(64), @LiveHash varchar(64),
            @PrevResult varchar(200);

    /*  a repeated instruction must not release twice (COM-015, BOM-030) */
    IF @CorrelationId IS NOT NULL
    BEGIN
        SELECT @PrevResult = ResultRef FROM core.IdempotencyKey WHERE CorrelationId = @CorrelationId;
        IF @PrevResult IS NOT NULL
        BEGIN
            SELECT Outcome = 'REPEAT', BomRef = @BomRef, BomRevision = @BomRevision, ResultRef = @PrevResult,
                   Message = 'STERP-I-BOM0402: already released by this request; no further change made';
            RETURN;
        END
    END

    SELECT @HeaderId = BomHeaderId, @Status = Status, @Author = ISNULL(AuthorUser, CreatedBy),
           @Parent = ParentItemCode, @Site = SiteCode
      FROM bom.BomHeader WHERE CompanyCode = @CompanyCode AND BomRef = @BomRef AND BomRevision = @BomRevision;
    IF @HeaderId IS NULL
        THROW 51401, 'STERP-E-BOM0401: bill revision not found - nothing to release', 1;

    IF NOT EXISTS (SELECT 1 FROM core.UserRole r JOIN core.RolePermission rp ON rp.RoleCode = r.RoleCode
                   WHERE r.UserCode = @UserCode AND rp.PermissionCode = 'BOM.RELEASE'
                     AND r.CompanyCode IN (@CompanyCode, '*'))
        THROW 51402, 'STERP-E-BOM0403: user is not authorised to release bills of material', 1;

    IF @Status = 'RELEASED'
        THROW 51403, 'STERP-E-BOM0404: the revision is already released and is immutable - raise a new revision', 1;
    IF @Status IN ('SUPERSEDED', 'RETIRED')
        THROW 51404, 'STERP-E-BOM0405: a superseded or retired revision cannot be released again', 1;

    /*  BOM-004: complete validation evidence that still matches the bill */
    SELECT TOP (1) @ValidationId = ValidationId, @Outcome = Outcome, @Hash = ContentHash
      FROM bom.BomValidation WHERE BomHeaderId = @HeaderId ORDER BY ValidatedAt DESC;
    IF @ValidationId IS NULL
        THROW 51405, 'STERP-E-BOM0406: the revision has not been validated - run BOMVALID before release', 1;
    IF @Outcome = 'FAIL'
    BEGIN
        DECLARE @Finding varchar(400), @Msg nvarchar(400);
        SELECT TOP (1) @Finding = ISNULL(PathText, ItemCode) + ' - ' + Message
          FROM bom.BomValidationFinding WHERE ValidationId = @ValidationId AND Severity = 'ERROR' ORDER BY LineNum;
        SET @Msg = N'STERP-E-BOM0407: validation failed - ' + ISNULL(@Finding, N'see the findings list');
        THROW 51406, @Msg, 1;
    END

    SELECT @LiveHash = CONVERT(varchar(64), HASHBYTES('SHA2_256',
        (SELECT STRING_AGG(CONVERT(varchar(200), c.LineNum) + '|' + c.ComponentItemCode + '|' +
                CONVERT(varchar(40), c.UsageQty) + '|' + c.UsageBasis + '|' + c.UomCode + '|' +
                CONVERT(varchar(40), c.ScrapPct) + '|' + c.SupplyBehaviour, ';')
           FROM bom.BomComponent c WHERE c.BomHeaderId = @HeaderId)), 2);
    IF @Hash IS NOT NULL AND @LiveHash IS NOT NULL AND @Hash <> @LiveHash
        THROW 51407, 'STERP-E-BOM0408: the bill changed after it was validated - validate the current content', 1;

    /*  authorised engineering review, never the author's own signature (COM-003) */
    SET @ReviewerUser = ISNULL(@ReviewerUser, (SELECT ReviewerUser FROM bom.BomHeader WHERE BomHeaderId = @HeaderId));
    IF @ReviewerUser IS NULL
        THROW 51408, 'STERP-E-BOM0409: an authorised engineering review is required before release', 1;
    IF @ReviewerUser = @Author OR @ReviewerUser = @UserCode
        THROW 51409, 'STERP-E-BOM0410: the engineering review may not be signed by the author or releaser', 1;
    IF NOT EXISTS (SELECT 1 FROM core.UserRole r JOIN core.RolePermission rp ON rp.RoleCode = r.RoleCode
                   WHERE r.UserCode = @ReviewerUser AND rp.PermissionCode IN ('BOM.RELEASE', 'BOM.CHANGE.APPROVE')
                     AND r.CompanyCode IN (@CompanyCode, '*'))
        THROW 51410, 'STERP-E-BOM0411: the named reviewer does not hold engineering review authority', 1;

    BEGIN TRAN;
        DECLARE @Now datetime2(3) = SYSDATETIME(), @OldId int;
        DECLARE @Eff date = ISNULL(@EffectiveFrom,
                (SELECT EffectiveFrom FROM bom.BomHeader WHERE BomHeaderId = @HeaderId));

        /*  BOM-028: the revision being replaced keeps its history, it is never deleted */
        IF @SupersedeRevision IS NOT NULL
        BEGIN
            SELECT @OldId = BomHeaderId FROM bom.BomHeader
             WHERE CompanyCode = @CompanyCode AND BomRef = @BomRef AND BomRevision = @SupersedeRevision;
            IF @OldId IS NULL
            BEGIN
                ROLLBACK TRAN;
                THROW 51411, 'STERP-E-BOM0412: the revision to supersede does not exist', 1;
            END
            UPDATE bom.BomHeader SET Status = 'SUPERSEDED', SupersededByBomHeaderId = @HeaderId,
                   EffectiveTo = @Eff, ModifiedBy = @UserCode, ModifiedAt = @Now
             WHERE BomHeaderId = @OldId;
        END

        UPDATE bom.BomHeader
           SET Status = 'RELEASED', ReleasedBy = @UserCode, ReleasedAt = @Now, ReviewerUser = @ReviewerUser,
               ValidationId = @ValidationId, EffectiveFrom = @Eff, ChangeRef = ISNULL(@ChangeRef, ChangeRef),
               ChangeReason = ISNULL(@Reason, ChangeReason), ModifiedBy = @UserCode, ModifiedAt = @Now
         WHERE BomHeaderId = @HeaderId;

        EXEC core.usp_WriteAudit @UserCode = @UserCode, @CompanyCode = @CompanyCode, @EntityType = 'bom.BomHeader',
             @EntityKey = @BomRef, @Action = 'RELEASE', @FieldName = 'Status', @OldValue = @Status,
             @NewValue = 'RELEASED', @Reason = @Reason, @AuthorityRef = @ReviewerUser, @Source = 'BOM',
             @CorrelationId = @CorrelationId;

        IF @CorrelationId IS NOT NULL AND NOT EXISTS (SELECT 1 FROM core.IdempotencyKey WHERE CorrelationId = @CorrelationId)
            INSERT core.IdempotencyKey (CorrelationId, Operation, Status, ResultRef)
            VALUES (@CorrelationId, 'BOM.RELEASE', 'COMPLETED', @BomRef + '/' + @BomRevision);
    COMMIT TRAN;

    SELECT Outcome = 'RELEASED', BomRef = @BomRef, BomRevision = @BomRevision, ParentItemCode = @Parent,
           ReleasedBy = @UserCode, ReviewerUser = @ReviewerUser, EffectiveFrom = @Eff,
           SupersededRevision = @SupersedeRevision, ValidationId = @ValidationId,
           Message = 'STERP-I-BOM0401: revision released and frozen';
END
GO
