/*  Where used: which bills quote a component, directly and indirectly, and what a cycle does to the
    upward walk.  The assertions read the released demo structures of worked example B1 (a plate used
    on the brace, the brace used on the frame) and B6 (the shared screw and the deliberate loop), so
    the published usage figures are the expected values.  Anything the test creates is a ZZ fixture
    inside a transaction that is rolled back.

    REQ: BOM-013, BOM-023, BOM-028, BOM-031  AC: AC-093, AC-096 */
SET NOCOUNT ON;
GO
PRINT '# bom-whereused';
/*  Re-runnable: remove any fixture an aborted earlier run left behind. */
DELETE c FROM bom.BomComponent c JOIN bom.BomHeader h ON h.BomHeaderId = c.BomHeaderId WHERE h.BomRef LIKE 'ZZWU%';
DELETE h FROM bom.BomHeader h WHERE h.BomRef LIKE 'ZZWU%';
DELETE FROM inv.Item WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-WU-%';
DELETE FROM inv.Item WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-CYC-%';
/*  A bill may only be written against the item master, so the fixtures need their items.  They are
    dropped again at the foot of this test. */
INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, QuantumQty, IsIndivisible,
        TraceabilityType, MakeBuy, ValuationGroup, CostMethod, LifecycleStatus, IsManufacturable, CreatedBy)
VALUES ('STC', 'ZZ-WU-DRAFT-TOP', 'Where used draft parent', 'MADE', 'EA', 1, 1, 'NONE', 'MAKE', 'MADE', 'MWA', 'RELEASED', 1, 'TEST'),
       ('STC', 'ZZ-CYC-A', 'Cycle fixture A', 'MADE', 'EA', 1, 1, 'NONE', 'MAKE', 'MADE', 'MWA', 'RELEASED', 1, 'TEST'),
       ('STC', 'ZZ-CYC-B', 'Cycle fixture B', 'MADE', 'EA', 1, 1, 'NONE', 'MAKE', 'MADE', 'MWA', 'RELEASED', 1, 'TEST'),
       ('STC', 'ZZ-CYC-SELF', 'Cycle fixture quoting itself', 'MADE', 'EA', 1, 1, 'NONE', 'MAKE', 'MADE', 'MWA', 'RELEASED', 1, 'TEST');
GO
DECLARE @msg varchar(400), @n int, @lvl smallint, @qty decimal(19,6), @path varchar(400), @hdr int;
DECLARE @wu TABLE (Level smallint, BomHeaderId int, BomRef varchar(30), BomRevision varchar(4),
                   Status varchar(12), ParentItemCode varchar(40), ComponentItemCode varchar(40),
                   VariantCode varchar(20), Purpose varchar(10), EffectiveFrom date, EffectiveTo date,
                   UsageQty decimal(19,6), UomCode varchar(10), UsageBasis varchar(12), IsOptional bit,
                   AlternateGroup varchar(10), PathText varchar(400), QtyPerTopUnit decimal(19,6), IsDirect bit);

/*  1 to 3 - B1: DEMO-PLATE is quoted 0.4 kg on the brace, and two braces go into the frame, so the
    plate is an indirect component of the frame at 0.8 kg per frame.  REQ: BOM-023 */
BEGIN TRY
    DELETE @wu;
    INSERT @wu EXEC bom.usp_BomWhereUsed @CompanyCode = 'STC', @ComponentItemCode = 'DEMO-PLATE',
         @AsOf = '2026-10-01', @MaxLevels = 10, @IncludeDraft = 0, @IncludeDemand = 0;

    SELECT @n = COUNT(*) FROM @wu WHERE Level = 1 AND ParentItemCode = 'DEMO-BRACE' AND IsDirect = 1;
    IF @n = 1 PRINT 'ok 1 - the direct parent of the plate is the brace -- REQ: BOM-023';
    ELSE PRINT 'not ok 1 - the direct parent was not reported (' + CONVERT(varchar(4), ISNULL(@n, -1)) + ' rows) -- REQ: BOM-023';

    SELECT @path = PathText, @qty = QtyPerTopUnit FROM @wu
     WHERE Level = 2 AND ParentItemCode = 'DEMO-FRAME';
    IF @path = 'DEMO-PLATE < DEMO-BRACE < DEMO-FRAME'
        PRINT 'ok 2 - the frame is reported as an indirect parent with the full path -- REQ: BOM-023, BOM-031';
    ELSE PRINT 'not ok 2 - the indirect path was ' + ISNULL(@path, 'not reported') + ' -- REQ: BOM-023';

    IF @qty = 0.800000 PRINT 'ok 3 - 0.4 kg on each of two braces is 0.8 kg per frame -- REQ: BOM-023, BOM-010';
    ELSE PRINT 'not ok 3 - usage per top unit was ' + ISNULL(CONVERT(varchar(20), @qty), 'null') + ', expected 0.800000 -- REQ: BOM-023';
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    PRINT 'not ok 1 - the where used enquiry failed: ' + @msg;
    PRINT 'not ok 2 - the where used enquiry failed';
    PRINT 'not ok 3 - the where used enquiry failed';
END CATCH

/*  4 and 5 - B6: one screw is shared by two subassemblies, both under the same root, and the root
    is reached once for each distinct path.  REQ: BOM-023 */
BEGIN TRY
    DELETE @wu;
    INSERT @wu EXEC bom.usp_BomWhereUsed @CompanyCode = 'STC', @ComponentItemCode = 'DEMO-B6-SCREW',
         @AsOf = '2026-10-01', @MaxLevels = 10, @IncludeDraft = 0, @IncludeDemand = 0;

    SELECT @n = COUNT(DISTINCT ParentItemCode) FROM @wu WHERE Level = 1;
    IF @n = 2 PRINT 'ok 4 - the shared screw reports both direct parents -- REQ: BOM-023';
    ELSE PRINT 'not ok 4 - ' + CONVERT(varchar(4), ISNULL(@n, -1)) + ' direct parents, expected 2 -- REQ: BOM-023';

    SELECT @n = COUNT(*) FROM @wu WHERE Level = 2 AND ParentItemCode = 'DEMO-B6-ROOT';
    IF @n = 2 PRINT 'ok 5 - the shared root is reported once for each distinct path -- REQ: BOM-023, BOM-031';
    ELSE PRINT 'not ok 5 - the root was reported ' + CONVERT(varchar(4), ISNULL(@n, -1)) + ' times, expected 2 -- REQ: BOM-023';
END TRY
BEGIN CATCH
    PRINT 'not ok 4 - the where used enquiry failed: ' + ERROR_MESSAGE();
    PRINT 'not ok 5 - the where used enquiry failed';
END CATCH

/*  6 and 7 - an engineering change impact assessment looks at released structures only, unless the
    engineer asks for the drafts as well.  REQ: BOM-031 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
            EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZWU-DRAFT', 'A', 'ZZ-WU-DRAFT-TOP', 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST');
    SET @hdr = SCOPE_IDENTITY();
    INSERT bom.BomComponent (BomHeaderId, LineNum, PathRef, ComponentItemCode, UsageQty, UsageBasis,
            UomCode, ScrapPct, SupplyBehaviour, CreatedBy)
    VALUES (@hdr, 10, '1', 'DEMO-PLATE', 5, 'PROPORTION', 'KG', 0, 'PURCHASED', 'TEST');

    DELETE @wu;
    INSERT @wu EXEC bom.usp_BomWhereUsed @CompanyCode = 'STC', @ComponentItemCode = 'DEMO-PLATE',
         @AsOf = '2026-10-01', @MaxLevels = 10, @IncludeDraft = 0, @IncludeDemand = 0;
    IF NOT EXISTS (SELECT 1 FROM @wu WHERE ParentItemCode = 'ZZ-WU-DRAFT-TOP')
        PRINT 'ok 6 - a drafted bill is not part of a released where used answer -- REQ: BOM-031';
    ELSE PRINT 'not ok 6 - a drafted bill appeared in the released answer -- REQ: BOM-031';

    DELETE @wu;
    INSERT @wu EXEC bom.usp_BomWhereUsed @CompanyCode = 'STC', @ComponentItemCode = 'DEMO-PLATE',
         @AsOf = '2026-10-01', @MaxLevels = 10, @IncludeDraft = 1, @IncludeDemand = 0;
    IF EXISTS (SELECT 1 FROM @wu WHERE ParentItemCode = 'ZZ-WU-DRAFT-TOP' AND Status = 'DRAFT')
        PRINT 'ok 7 - the engineer can ask for the drafted bills as well -- REQ: BOM-031';
    ELSE PRINT 'not ok 7 - the drafted bill was not reported when asked for -- REQ: BOM-031';
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 6 - unexpected error: ' + @msg;
    PRINT 'not ok 7 - unexpected error: ' + @msg;
END CATCH

/*  8 and 9 - the seeded B6 loop.  DEMO-B6-CYC quotes DEMO-B6-LOOP and DEMO-B6-LOOP quotes
    DEMO-B6-CYC again; the upward walk must stop at the point where a parent would repeat instead of
    running away to the recursion limit.  REQ: BOM-013, BOM-023 */
BEGIN TRY
    DELETE @wu;
    INSERT @wu EXEC bom.usp_BomWhereUsed @CompanyCode = 'STC', @ComponentItemCode = 'DEMO-B6-LOOP',
         @AsOf = '2026-10-01', @MaxLevels = 20, @IncludeDraft = 1, @IncludeDemand = 0;

    SELECT @lvl = ISNULL(MAX(Level), 0), @n = COUNT(*) FROM @wu;
    IF @lvl <= 2 AND @n <= 4
        PRINT 'ok 8 - the upward walk over a cyclic structure terminates -- REQ: BOM-013';
    ELSE PRINT 'not ok 8 - the cyclic walk reached level ' + CONVERT(varchar(4), @lvl) +
               ' with ' + CONVERT(varchar(6), @n) + ' rows -- REQ: BOM-013';

    IF NOT EXISTS (SELECT 1 FROM @wu WHERE PathText LIKE '%' + ParentItemCode + ' <%' + ParentItemCode + ' <%')
        PRINT 'ok 9 - no reported path visits the same parent twice -- REQ: BOM-013';
    ELSE PRINT 'not ok 9 - a reported path repeats a parent -- REQ: BOM-013';
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    PRINT 'not ok 8 - the cyclic walk failed: ' + @msg;
    PRINT 'not ok 9 - the cyclic walk failed';
END CATCH

/*  10 and 11 - a freshly built two bill loop, and an item that quotes itself, are both refused the
    runaway: the enquiry answers, it does not fall over on the recursion limit.  REQ: BOM-013 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
            EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZWU-CYCA', 'A', 'ZZ-CYC-A', 1, '2026-01-01', 'RELEASED', 'ENGINEER', 'TEST');
    SET @hdr = SCOPE_IDENTITY();
    INSERT bom.BomComponent (BomHeaderId, LineNum, PathRef, ComponentItemCode, UsageQty, UsageBasis,
            UomCode, ScrapPct, SupplyBehaviour, CreatedBy)
    VALUES (@hdr, 10, '1', 'ZZ-CYC-B', 1, 'PROPORTION', 'EA', 0, 'MAKE', 'TEST');
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
            EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZWU-CYCB', 'A', 'ZZ-CYC-B', 1, '2026-01-01', 'RELEASED', 'ENGINEER', 'TEST');
    SET @hdr = SCOPE_IDENTITY();
    INSERT bom.BomComponent (BomHeaderId, LineNum, PathRef, ComponentItemCode, UsageQty, UsageBasis,
            UomCode, ScrapPct, SupplyBehaviour, CreatedBy)
    VALUES (@hdr, 10, '1', 'ZZ-CYC-A', 1, 'PROPORTION', 'EA', 0, 'MAKE', 'TEST');
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
            EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZWU-SELF', 'A', 'ZZ-CYC-SELF', 1, '2026-01-01', 'RELEASED', 'ENGINEER', 'TEST');
    SET @hdr = SCOPE_IDENTITY();
    INSERT bom.BomComponent (BomHeaderId, LineNum, PathRef, ComponentItemCode, UsageQty, UsageBasis,
            UomCode, ScrapPct, SupplyBehaviour, CreatedBy)
    VALUES (@hdr, 10, '1', 'ZZ-CYC-SELF', 1, 'PROPORTION', 'EA', 0, 'MAKE', 'TEST');

    DELETE @wu;
    INSERT @wu EXEC bom.usp_BomWhereUsed @CompanyCode = 'STC', @ComponentItemCode = 'ZZ-CYC-A',
         @AsOf = '2026-10-01', @MaxLevels = 25, @IncludeDraft = 1, @IncludeDemand = 0;
    SELECT @n = COUNT(*), @lvl = ISNULL(MAX(Level), 0) FROM @wu;
    IF @n = 1 AND @lvl = 1
        PRINT 'ok 10 - a two bill loop is walked once and then stopped -- REQ: BOM-013, BOM-023';
    ELSE PRINT 'not ok 10 - the two bill loop produced ' + CONVERT(varchar(6), @n) + ' rows to level ' +
               CONVERT(varchar(4), @lvl) + ', expected 1 row at level 1 -- REQ: BOM-013';

    DELETE @wu;
    INSERT @wu EXEC bom.usp_BomWhereUsed @CompanyCode = 'STC', @ComponentItemCode = 'ZZ-CYC-SELF',
         @AsOf = '2026-10-01', @MaxLevels = 25, @IncludeDraft = 1, @IncludeDemand = 0;
    SELECT @n = COUNT(*) FROM @wu;
    IF @n = 1 PRINT 'ok 11 - an item that quotes itself is reported once, not endlessly -- REQ: BOM-013';
    ELSE PRINT 'not ok 11 - the self reference produced ' + CONVERT(varchar(6), @n) + ' rows, expected 1 -- REQ: BOM-013';
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 10 - unexpected error: ' + @msg;
    PRINT 'not ok 11 - unexpected error: ' + @msg;
END CATCH

/*  12 - the enquiry the forms use: a component nobody quotes answers an empty list rather than an
    error, which is what frmBomWhereUsed shows as "not used".  REQ: BOM-023 */
BEGIN TRY
    DELETE @wu;
    INSERT @wu EXEC bom.usp_BomWhereUsed @CompanyCode = 'STC', @ComponentItemCode = 'ZZ-NO-SUCH-ITEM',
         @AsOf = '2026-10-01', @IncludeDemand = 0;
    SELECT @n = COUNT(*) FROM @wu;
    IF @n = 0 PRINT 'ok 12 - an unused component answers an empty where used list -- REQ: BOM-023';
    ELSE PRINT 'not ok 12 - an unused component reported ' + CONVERT(varchar(6), @n) + ' parents -- REQ: BOM-023';
END TRY
BEGIN CATCH
    PRINT 'not ok 12 - unexpected error: ' + ERROR_MESSAGE();
END CATCH
GO
DELETE FROM inv.Item WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-WU-%';
DELETE FROM inv.Item WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-CYC-%';
PRINT '# bom-whereused done';
GO
