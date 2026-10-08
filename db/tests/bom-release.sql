/*  The release gate of a bill of material and the freeze that follows it.

    A revision may only be released when a complete validation still matches the bill content and
    when an engineering review exists that is neither the author's nor the releaser's own signature;
    once released the structure is frozen and a change must raise a new revision.  Every assertion
    builds its own ZZBOM fixture inside a transaction that is rolled back, so the demo bills seeded
    by 31-bom-structures.sql are left exactly as they were found.

    REQ: BOM-003, BOM-004, BOM-005, BOM-028, BOM-029, BOM-030  AC: AC-087, AC-088 */
SET NOCOUNT ON;
GO
PRINT '# bom-release';
/*  Re-runnable: clear anything an aborted earlier run could have left behind.  A released fixture
    has to be put back to draft first or the immutability trigger refuses the clean up itself. */
UPDATE bom.BomHeader SET Status = 'DRAFT' WHERE BomRef LIKE 'ZZBOM%' AND Status <> 'DRAFT';
DELETE f FROM bom.BomValidationFinding f
  JOIN bom.BomValidation v ON v.ValidationId = f.ValidationId
  JOIN bom.BomHeader h ON h.BomHeaderId = v.BomHeaderId WHERE h.BomRef LIKE 'ZZBOM%';
UPDATE bom.BomHeader SET ValidationId = NULL, SupersededByBomHeaderId = NULL WHERE BomRef LIKE 'ZZBOM%';
DELETE v FROM bom.BomValidation v JOIN bom.BomHeader h ON h.BomHeaderId = v.BomHeaderId WHERE h.BomRef LIKE 'ZZBOM%';
DELETE c FROM bom.BomComponent c JOIN bom.BomHeader h ON h.BomHeaderId = c.BomHeaderId WHERE h.BomRef LIKE 'ZZBOM%';
DELETE h FROM bom.BomHeader h WHERE h.BomRef LIKE 'ZZBOM%';
DELETE FROM core.UserRole WHERE UserCode = 'QUALITY' AND RoleCode = 'ENGMGR' AND CreatedBy = 'TEST';
DELETE FROM core.IdempotencyKey WHERE CorrelationId LIKE 'ZZBOM%';
DELETE FROM inv.Item WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-BOM-%';
/*  The parent and the component of the fixture have to be real items: a bill may only be written
    against the item master.  They are dropped again at the foot of this test. */
INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, QuantumQty, IsIndivisible,
        TraceabilityType, MakeBuy, ValuationGroup, CostMethod, LifecycleStatus, IsManufacturable, CreatedBy)
VALUES ('STC', 'ZZ-BOM-TOP', 'Release gate test assembly', 'MADE', 'EA', 1, 1, 'NONE', 'MAKE', 'MADE', 'MWA', 'RELEASED', 1, 'TEST'),
       ('STC', 'ZZ-BOM-CHILD', 'Release gate test component', 'BOUGHT', 'EA', 1, 1, 'NONE', 'BUY', 'BOUGHT', 'MWA', 'RELEASED', 0, 'TEST');
GO
DECLARE @msg varchar(400), @hdr int, @old int, @hash varchar(64), @status varchar(12),
        @reviewer varchar(20), @released varchar(20), @vid int, @n int, @qty decimal(19,6);
DECLARE @rep TABLE (Outcome varchar(12), BomRef varchar(30), BomRevision varchar(4),
                    ResultRef varchar(200), Message varchar(300));

/*  1 - a revision nobody validated may not be released, however senior the releaser.
    REQ: BOM-004 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, Purpose, Description,
            NominalBasis, YieldFactor, ProductionQuantum, EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZBOM-REL', 'A', 'ZZ-BOM-TOP', 'STANDARD', 'Release gate fixture',
            1, 1, 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST');
    SET @hdr = SCOPE_IDENTITY();
    INSERT bom.BomComponent (BomHeaderId, LineNum, PathRef, ComponentItemCode, UsageQty, UsageBasis,
            UomCode, ConversionFactor, ScrapPct, SupplyBehaviour, CreatedBy)
    VALUES (@hdr, 10, '1', 'ZZ-BOM-CHILD', 2, 'PROPORTION', 'EA', 1, 0, 'PURCHASED', 'TEST');

    EXEC bom.usp_ReleaseBom @CompanyCode = 'STC', @BomRef = 'ZZBOM-REL', @BomRevision = 'A',
         @UserCode = 'ENGMGR', @ReviewerUser = 'QUALITY', @EffectiveFrom = '2026-02-01';
    PRINT 'not ok 1 - an unvalidated revision was released -- REQ: BOM-004';
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-BOM0406%' PRINT 'ok 1 - release without a validation is refused -- REQ: BOM-004';
    ELSE PRINT 'not ok 1 - unexpected error: ' + @msg;
END CATCH

/*  2 - a validation that failed names the offending path in the refusal.  REQ: BOM-004, BOM-029 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
            EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZBOM-REL', 'A', 'ZZ-BOM-TOP', 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST');
    SET @hdr = SCOPE_IDENTITY();
    INSERT bom.BomComponent (BomHeaderId, LineNum, PathRef, ComponentItemCode, UsageQty, UsageBasis,
            UomCode, ScrapPct, SupplyBehaviour, CreatedBy)
    VALUES (@hdr, 10, '1', 'ZZ-BOM-CHILD', 2, 'PROPORTION', 'EA', 0, 'MAKE', 'TEST');
    INSERT bom.BomValidation (CompanyCode, BomHeaderId, ValidationRef, EffectiveDate, ValidatedBy,
            Outcome, ErrorCount, WarningCount, ComponentCount, CycleFound)
    VALUES ('STC', @hdr, 'ZZVAL-F', '2026-01-01', 'ENGINEER', 'FAIL', 1, 0, 1, 0);
    SET @vid = SCOPE_IDENTITY();
    INSERT bom.BomValidationFinding (ValidationId, LineNum, Severity, ReasonCode, ComponentLineNum,
            PathText, ItemCode, Message)
    VALUES (@vid, 1, 'ERROR', 'BOM0202', 10, 'ZZ-BOM-TOP > ZZ-BOM-CHILD', 'ZZ-BOM-CHILD',
            'manufactured component has no effective bill');

    EXEC bom.usp_ReleaseBom @CompanyCode = 'STC', @BomRef = 'ZZBOM-REL', @BomRevision = 'A',
         @UserCode = 'ENGMGR', @ReviewerUser = 'QUALITY';
    PRINT 'not ok 2 - a failed validation still allowed the release -- REQ: BOM-004';
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-BOM0407%' AND @msg LIKE '%ZZ-BOM-CHILD%'
        PRINT 'ok 2 - a failed validation blocks release and names the path -- REQ: BOM-004, BOM-029';
    ELSE PRINT 'not ok 2 - unexpected error: ' + @msg;
END CATCH

/*  3 - the evidence must match the bill as it stands now, not as it was.  REQ: BOM-004, BOM-005 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
            EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZBOM-REL', 'A', 'ZZ-BOM-TOP', 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST');
    SET @hdr = SCOPE_IDENTITY();
    INSERT bom.BomComponent (BomHeaderId, LineNum, PathRef, ComponentItemCode, UsageQty, UsageBasis,
            UomCode, ScrapPct, SupplyBehaviour, CreatedBy)
    VALUES (@hdr, 10, '1', 'ZZ-BOM-CHILD', 2, 'PROPORTION', 'EA', 0, 'PURCHASED', 'TEST');
    SELECT @hash = CONVERT(varchar(64), HASHBYTES('SHA2_256',
        (SELECT STRING_AGG(CONVERT(varchar(200), c.LineNum) + '|' + c.ComponentItemCode + '|' +
                CONVERT(varchar(40), c.UsageQty) + '|' + c.UsageBasis + '|' + c.UomCode + '|' +
                CONVERT(varchar(40), c.ScrapPct) + '|' + c.SupplyBehaviour, ';')
           FROM bom.BomComponent c WHERE c.BomHeaderId = @hdr)), 2);
    INSERT bom.BomValidation (CompanyCode, BomHeaderId, ValidationRef, EffectiveDate, ValidatedBy,
            Outcome, ComponentCount, CycleFound, ContentHash)
    VALUES ('STC', @hdr, 'ZZVAL-H', '2026-01-01', 'ENGINEER', 'PASS', 1, 0, @hash);
    /*  the engineer changes the usage after the validation ran */
    UPDATE bom.BomComponent SET UsageQty = 3 WHERE BomHeaderId = @hdr AND LineNum = 10;

    EXEC bom.usp_ReleaseBom @CompanyCode = 'STC', @BomRef = 'ZZBOM-REL', @BomRevision = 'A',
         @UserCode = 'ENGMGR', @ReviewerUser = 'QUALITY';
    PRINT 'not ok 3 - a bill changed after validation was released -- REQ: BOM-004';
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-BOM0408%'
        PRINT 'ok 3 - stale validation evidence blocks the release -- REQ: BOM-004, BOM-005';
    ELSE PRINT 'not ok 3 - unexpected error: ' + @msg;
END CATCH

/*  4 to 6 - who may sign.  The author may not review their own bill, the reviewer must hold the
    authority, and the releaser must hold BOM.RELEASE.  REQ: BOM-003, BOM-004 */
DECLARE @case int = 4, @user varchar(20), @rev varchar(20), @expect varchar(20), @what varchar(120);
DECLARE @sig TABLE (Seq int, UserCode varchar(20), ReviewerUser varchar(20), Expect varchar(20), What varchar(120));
INSERT @sig VALUES
  (4, 'ENGMGR', 'ENGINEER', 'STERP-E-BOM0410', 'the author may not sign the engineering review of their own bill'),
  (5, 'ENGMGR', 'APCLERK',  'STERP-E-BOM0411', 'a reviewer without engineering review authority is refused'),
  (6, 'WHSE',   'QUALITY',  'STERP-E-BOM0403', 'a releaser without BOM.RELEASE is refused');
WHILE EXISTS (SELECT 1 FROM @sig WHERE Seq = @case)
BEGIN
    SELECT @user = UserCode, @rev = ReviewerUser, @expect = Expect, @what = What FROM @sig WHERE Seq = @case;
    BEGIN TRY
        BEGIN TRANSACTION;
        INSERT core.UserRole (UserCode, RoleCode, CompanyCode, SiteCode, ValidFrom, CreatedBy)
        VALUES ('QUALITY', 'ENGMGR', '*', '*', '2024-04-01', 'TEST');
        INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
                EffectiveFrom, Status, AuthorUser, CreatedBy)
        VALUES ('STC', 'RDG', 'ZZBOM-REL', 'A', 'ZZ-BOM-TOP', 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST');
        SET @hdr = SCOPE_IDENTITY();
        INSERT bom.BomComponent (BomHeaderId, LineNum, PathRef, ComponentItemCode, UsageQty, UsageBasis,
                UomCode, ScrapPct, SupplyBehaviour, CreatedBy)
        VALUES (@hdr, 10, '1', 'ZZ-BOM-CHILD', 2, 'PROPORTION', 'EA', 0, 'PURCHASED', 'TEST');
        INSERT bom.BomValidation (CompanyCode, BomHeaderId, ValidationRef, EffectiveDate, ValidatedBy,
                Outcome, ComponentCount, CycleFound)
        VALUES ('STC', @hdr, 'ZZVAL-S', '2026-01-01', 'ENGINEER', 'PASS', 1, 0);

        EXEC bom.usp_ReleaseBom @CompanyCode = 'STC', @BomRef = 'ZZBOM-REL', @BomRevision = 'A',
             @UserCode = @user, @ReviewerUser = @rev;
        PRINT 'not ok ' + CONVERT(varchar(2), @case) + ' - ' + @what + ' was accepted -- REQ: BOM-004';
        IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
    END TRY
    BEGIN CATCH
        SET @msg = ERROR_MESSAGE();
        IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
        IF @msg LIKE '%' + @expect + '%'
            PRINT 'ok ' + CONVERT(varchar(2), @case) + ' - ' + @what + ' -- REQ: BOM-003, BOM-004';
        ELSE PRINT 'not ok ' + CONVERT(varchar(2), @case) + ' - unexpected error: ' + @msg;
    END CATCH
    SET @case = @case + 1;
END

/*  7 and 8 - the clean release, then the refusal to release the same revision twice.
    REQ: BOM-004, BOM-005, BOM-030 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT core.UserRole (UserCode, RoleCode, CompanyCode, SiteCode, ValidFrom, CreatedBy)
    VALUES ('QUALITY', 'ENGMGR', '*', '*', '2024-04-01', 'TEST');
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
            EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZBOM-REL', 'A', 'ZZ-BOM-TOP', 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST');
    SET @hdr = SCOPE_IDENTITY();
    INSERT bom.BomComponent (BomHeaderId, LineNum, PathRef, ComponentItemCode, UsageQty, UsageBasis,
            UomCode, ScrapPct, SupplyBehaviour, CreatedBy)
    VALUES (@hdr, 10, '1', 'ZZ-BOM-CHILD', 2, 'PROPORTION', 'EA', 0, 'PURCHASED', 'TEST');
    INSERT bom.BomValidation (CompanyCode, BomHeaderId, ValidationRef, EffectiveDate, ValidatedBy,
            Outcome, ComponentCount, CycleFound)
    VALUES ('STC', @hdr, 'ZZVAL-P', '2026-01-01', 'ENGINEER', 'PASS', 1, 0);
    SET @vid = SCOPE_IDENTITY();

    EXEC bom.usp_ReleaseBom @CompanyCode = 'STC', @BomRef = 'ZZBOM-REL', @BomRevision = 'A',
         @UserCode = 'ENGMGR', @ReviewerUser = 'QUALITY', @EffectiveFrom = '2026-02-01',
         @Reason = 'Engineering release', @CorrelationId = 'ZZBOM-COR-1';
    SELECT @status = Status, @reviewer = ReviewerUser, @released = ReleasedBy FROM bom.BomHeader WHERE BomHeaderId = @hdr;
    IF @status = 'RELEASED' AND @reviewer = 'QUALITY' AND @released = 'ENGMGR'
       AND EXISTS (SELECT 1 FROM bom.BomHeader WHERE BomHeaderId = @hdr AND ValidationId = @vid AND ReleasedAt IS NOT NULL)
       AND EXISTS (SELECT 1 FROM core.IdempotencyKey WHERE CorrelationId = 'ZZBOM-COR-1' AND ResultRef = 'ZZBOM-REL/A')
        PRINT 'ok 7 - a validated revision with an independent review is released and stamped -- REQ: BOM-004, BOM-005';
    ELSE PRINT 'not ok 7 - the release did not stamp the revision: status ' + ISNULL(@status, 'none') +
               ', reviewer ' + ISNULL(@reviewer, 'none') + ' -- REQ: BOM-004';

    BEGIN TRY
        EXEC bom.usp_ReleaseBom @CompanyCode = 'STC', @BomRef = 'ZZBOM-REL', @BomRevision = 'A',
             @UserCode = 'ENGMGR', @ReviewerUser = 'QUALITY';
        PRINT 'not ok 8 - an already released revision was released again -- REQ: BOM-004';
    END TRY
    BEGIN CATCH
        SET @msg = ERROR_MESSAGE();
        IF @msg LIKE '%STERP-E-BOM0404%' PRINT 'ok 8 - a released revision cannot be released again -- REQ: BOM-004';
        ELSE PRINT 'not ok 8 - unexpected error: ' + @msg;
    END CATCH
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 7 - the release itself failed: ' + @msg;
    PRINT 'not ok 8 - the second release was not reached: ' + @msg;
END CATCH

/*  9 - the same instruction repeated returns the first outcome instead of releasing twice.
    REQ: BOM-030, COM-015 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
            EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZBOM-REL', 'A', 'ZZ-BOM-TOP', 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST');
    SET @hdr = SCOPE_IDENTITY();
    INSERT core.IdempotencyKey (CorrelationId, Operation, Status, ResultRef)
    VALUES ('ZZBOM-COR-2', 'BOM.RELEASE', 'COMPLETED', 'ZZBOM-REL/A');
    DELETE @rep;
    INSERT @rep EXEC bom.usp_ReleaseBom @CompanyCode = 'STC', @BomRef = 'ZZBOM-REL', @BomRevision = 'A',
         @UserCode = 'ENGMGR', @ReviewerUser = 'QUALITY', @CorrelationId = 'ZZBOM-COR-2';
    IF EXISTS (SELECT 1 FROM @rep WHERE Outcome = 'REPEAT' AND ResultRef = 'ZZBOM-REL/A')
       AND (SELECT Status FROM bom.BomHeader WHERE BomHeaderId = @hdr) = 'DRAFT'
        PRINT 'ok 9 - a repeated release instruction returns the first outcome and changes nothing -- REQ: BOM-030';
    ELSE PRINT 'not ok 9 - the repeated instruction was not recognised -- REQ: BOM-030';
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 9 - unexpected error: ' + @msg;
END CATCH

/*  10 to 12 - the freeze.  A released structure may not be edited or deleted, but writing the same
    engineering content back is not a change, so the seed stays re-runnable.
    REQ: BOM-004, BOM-028 */
DECLARE @frz int = 10, @sql nvarchar(400);
DECLARE @fr TABLE (Seq int, Stmt nvarchar(400), Expect varchar(20), What varchar(140));
INSERT @fr VALUES
  (10, N'UPDATE bom.BomComponent SET UsageQty = 9 WHERE BomHeaderId = @h AND LineNum = 10;',
       'STERP-E-BOM0413', 'the usage on a released revision may not be changed'),
  (11, N'DELETE bom.BomComponent WHERE BomHeaderId = @h AND LineNum = 10;',
       'STERP-E-BOM0413', 'a line may not be deleted from a released revision'),
  (12, N'UPDATE bom.BomComponent SET UsageQty = 2, Notes = ''reloaded'' WHERE BomHeaderId = @h AND LineNum = 10;',
       'NONE', 'rewriting the identical engineering content on a released revision is permitted');
WHILE EXISTS (SELECT 1 FROM @fr WHERE Seq = @frz)
BEGIN
    SELECT @sql = Stmt, @expect = Expect, @what = What FROM @fr WHERE Seq = @frz;
    BEGIN TRY
        BEGIN TRANSACTION;
        INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
                EffectiveFrom, Status, AuthorUser, ReviewerUser, ReleasedBy, ReleasedAt, CreatedBy)
        VALUES ('STC', 'RDG', 'ZZBOM-FRZ', 'A', 'ZZ-BOM-TOP', 1, '2026-01-01', 'RELEASED', 'ENGINEER',
                'QUALITY', 'ENGMGR', SYSDATETIME(), 'TEST');
        SET @hdr = SCOPE_IDENTITY();
        INSERT bom.BomComponent (BomHeaderId, LineNum, PathRef, ComponentItemCode, UsageQty, UsageBasis,
                UomCode, ScrapPct, SupplyBehaviour, CreatedBy)
        VALUES (@hdr, 10, '1', 'ZZ-BOM-CHILD', 2, 'PROPORTION', 'EA', 0, 'PURCHASED', 'TEST');

        EXEC sp_executesql @sql, N'@h int', @h = @hdr;
        IF @expect = 'NONE' PRINT 'ok ' + CONVERT(varchar(2), @frz) + ' - ' + @what + ' -- REQ: BOM-004';
        ELSE PRINT 'not ok ' + CONVERT(varchar(2), @frz) + ' - ' + @what + ' was accepted -- REQ: BOM-004';
        IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
    END TRY
    BEGIN CATCH
        SET @msg = ERROR_MESSAGE();
        IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
        IF @expect <> 'NONE' AND @msg LIKE '%' + @expect + '%'
            PRINT 'ok ' + CONVERT(varchar(2), @frz) + ' - ' + @what + ' -- REQ: BOM-004, BOM-028';
        ELSE PRINT 'not ok ' + CONVERT(varchar(2), @frz) + ' - unexpected error: ' + @msg;
    END CATCH
    SET @frz = @frz + 1;
END

/*  13 - superseding keeps the replaced revision as history rather than deleting it.  REQ: BOM-028 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT core.UserRole (UserCode, RoleCode, CompanyCode, SiteCode, ValidFrom, CreatedBy)
    VALUES ('QUALITY', 'ENGMGR', '*', '*', '2024-04-01', 'TEST');
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
            EffectiveFrom, Status, AuthorUser, ReviewerUser, ReleasedBy, ReleasedAt, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZBOM-SUP', 'A', 'ZZ-BOM-TOP', 1, '2026-01-01', 'RELEASED', 'ENGINEER',
            'QUALITY', 'ENGMGR', SYSDATETIME(), 'TEST');
    SET @old = SCOPE_IDENTITY();
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
            EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZBOM-SUP', 'B', 'ZZ-BOM-TOP', 1, '2026-06-01', 'DRAFT', 'ENGINEER', 'TEST');
    SET @hdr = SCOPE_IDENTITY();
    INSERT bom.BomComponent (BomHeaderId, LineNum, PathRef, ComponentItemCode, UsageQty, UsageBasis,
            UomCode, ScrapPct, SupplyBehaviour, CreatedBy)
    VALUES (@hdr, 10, '1', 'ZZ-BOM-CHILD', 3, 'PROPORTION', 'EA', 0, 'PURCHASED', 'TEST');
    INSERT bom.BomValidation (CompanyCode, BomHeaderId, ValidationRef, EffectiveDate, ValidatedBy,
            Outcome, ComponentCount, CycleFound)
    VALUES ('STC', @hdr, 'ZZVAL-B', '2026-06-01', 'ENGINEER', 'WARNING', 1, 0);

    EXEC bom.usp_ReleaseBom @CompanyCode = 'STC', @BomRef = 'ZZBOM-SUP', @BomRevision = 'B',
         @UserCode = 'ENGMGR', @ReviewerUser = 'QUALITY', @EffectiveFrom = '2026-06-01',
         @SupersedeRevision = 'A', @Reason = 'Revision B supersedes A';
    SELECT @n = COUNT(*) FROM bom.BomHeader
     WHERE BomHeaderId = @old AND Status = 'SUPERSEDED' AND SupersededByBomHeaderId = @hdr
       AND EffectiveTo = '2026-06-01';
    IF @n = 1 PRINT 'ok 13 - the superseded revision is kept as history and closed at the new date -- REQ: BOM-028';
    ELSE PRINT 'not ok 13 - the superseded revision was not closed off correctly -- REQ: BOM-028';
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 13 - unexpected error: ' + @msg;
END CATCH
GO
DELETE FROM inv.Item WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-BOM-%';
PRINT '# bom-release done';
GO
