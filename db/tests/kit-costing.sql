/*  Kit costing: the seven values of KIT-020 and the two boundaries that are easy to get wrong.
    Residual is issued less returned plus absorbed less completed; scrapped material is a
    disposition of material that was issued, so it is reported and never deducted (AC-081); and a
    made subassembly is charged to its parent once, at its own value, not again through the
    children that were consumed to make it.  Each case is built and rolled back.
    REQ: KIT-014, KIT-016, KIT-020  AC: AC-081 */
SET NOCOUNT ON;
GO
PRINT '# kit-costing';
/*  Re-runnable: clear anything an aborted earlier run could have left behind. */
DELETE g FROM kit.KitGenealogy g WHERE g.KitRef LIKE 'ZZKIT%';
DELETE i FROM kit.KitIssue i WHERE i.KitRef LIKE 'ZZKIT%';
DELETE c FROM kit.KitCompletion c WHERE c.KitRef LIKE 'ZZKIT%';
DELETE l FROM kit.ExecutionKitLine l JOIN kit.ExecutionKit k ON k.ExecutionKitId = l.ExecutionKitId WHERE k.KitRef LIKE 'ZZKIT%';
DELETE k FROM kit.ExecutionKit k WHERE k.KitRef LIKE 'ZZKIT%';
DELETE b FROM inv.StockBalance b WHERE b.ItemCode LIKE 'ZZ-KIT%';
DELETE i FROM inv.Item i WHERE i.ItemCode LIKE 'ZZ-KIT%';
GO
DECLARE @msg varchar(400), @site varchar(10), @parent varchar(40), @uom varchar(10),
        @sa varchar(40) = 'ZZ-KIT-SA', @other varchar(10), @kid bigint,
        @iss decimal(28,6), @ret decimal(28,6), @scr decimal(28,6), @abs decimal(28,6),
        @cmp decimal(28,6), @res decimal(28,6), @pln decimal(28,6), @var decimal(28,6), @n int;

SELECT TOP (1) @site = SiteCode FROM core.Site WHERE CompanyCode = 'STC' ORDER BY SiteCode;
SELECT TOP (1) @parent = ItemCode FROM inv.Item WHERE CompanyCode = 'STC' AND ItemClass = 'MADE' ORDER BY ItemCode;
SELECT TOP (1) @uom = StockUomCode FROM inv.Item WHERE CompanyCode = 'STC' AND ItemClass = 'BOUGHT' ORDER BY ItemCode;
SET @uom = ISNULL(@uom, 'EA');
SELECT TOP (1) @other = CompanyCode FROM core.Company WHERE CompanyCode <> 'STC' ORDER BY CompanyCode;

DECLARE @cs TABLE (KitRef varchar(30), ParentItemCode varchar(40), Status varchar(14),
                   PlannedGoodQty decimal(19,6), CompletedGoodQty decimal(19,6),
                   PlannedGBP decimal(28,6), IssuedGBP decimal(28,6), ReturnedGBP decimal(28,6),
                   ScrappedGBP decimal(28,6), AbsorbedGBP decimal(28,6), CompletedGBP decimal(28,6),
                   ResidualGBP decimal(28,6), VarianceGBP decimal(28,6), WipValueGBP decimal(18,2));

/*  1 to 5 - one kit with an issue, a return and a scrap, which is the whole of AC-081's material
    side.  REQ: KIT-013, KIT-014, KIT-020  AC: AC-081 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT kit.ExecutionKit (CompanyCode, SiteCode, KitRef, ParentItemCode, SourceDemandType, SourceDemandRef,
            PlannedGoodQty, CompletedGoodQty, NeedDate, OwnerUser, Status, ReleasedBy, WipValueGBP, CreatedBy)
    VALUES ('STC', @site, 'ZZKITC1', @parent, 'STOCK', 'ZZKITC1', 10, 0, '2026-11-15', 'PLANNER', 'PARTISSUED', 'SUPERVISOR', 80.00, 'TEST');
    SELECT @kid = ExecutionKitId FROM kit.ExecutionKit WHERE CompanyCode = 'STC' AND KitRef = 'ZZKITC1';
    INSERT kit.ExecutionKitLine (ExecutionKitId, LineNum, PathRef, ComponentItemCode, UsageBasis,
            OriginalRequirementQty, CurrentRequirementQty, UomCode, GrossIssuedQty, ReturnedQty, ScrappedQty,
            NetIssuedQty, RemainingQty, PlannedUnitCostGBP, IssuedValueGBP, Status)
    VALUES (@kid, 1, '1', 'ZZ-COMP-A', 'PROPORTION', 50, 50, @uom, 50, 10, 5, 40, 10, 2.000000, 80.00, 'PARTISSUED'),
           (@kid, 2, '2', 'ZZ-COMP-B', 'PROPORTION', 10, 10, @uom,  0,  0, 0,  0, 10, 3.000000,  0.00, 'OPEN'),
           (@kid, 3, '3', 'ZZ-COMP-C', 'FIXEDRUN',    1,  1, @uom,  0,  0, 0,  0,  1, 4.000000,  0.00, 'OPEN');
    INSERT kit.KitIssue (CompanyCode, KitRef, LineNum, ComponentItemCode, IssueType, Quantity, UomCode,
            StockQty, ActualDate, PostingDate, OperatorUser, UnitCostGBP, ValueGBP, ReasonCode, CreatedBy)
    VALUES ('STC','ZZKITC1',1,'ZZ-COMP-A','ISSUE',  50, @uom, 50, '2026-10-02','2026-10-02','STORES', 2.000000, 100.00, NULL,     'TEST'),
           ('STC','ZZKITC1',1,'ZZ-COMP-A','RETURN', 10, @uom, 10, '2026-10-03','2026-10-03','STORES', 2.000000,  20.00, 'UNUSED', 'TEST'),
           ('STC','ZZKITC1',1,'ZZ-COMP-A','SCRAP',   5, @uom,  5, '2026-10-04','2026-10-04','STORES', 2.000000,  10.00, 'DAMAGED','TEST');

    DELETE @cs;
    INSERT @cs EXEC kit.usp_KitCostSummary @CompanyCode = 'STC', @KitRef = 'ZZKITC1';
    SELECT @iss = IssuedGBP, @ret = ReturnedGBP, @scr = ScrappedGBP, @abs = AbsorbedGBP,
           @cmp = CompletedGBP, @res = ResidualGBP, @pln = PlannedGBP, @var = VarianceGBP FROM @cs;

    IF @iss = 100.00 AND @ret = 20.00 AND @scr = 10.00
        PRINT 'ok 1 - issued, returned and scrapped are each reported in their own right -- REQ: KIT-020';
    ELSE PRINT 'not ok 1 - issued ' + ISNULL(CAST(@iss AS varchar(20)),'NULL') + ', returned ' + ISNULL(CAST(@ret AS varchar(20)),'NULL')
              + ', scrapped ' + ISNULL(CAST(@scr AS varchar(20)),'NULL') + ' -- REQ: KIT-020';

    IF @res = @iss - @ret + @abs - @cmp
        PRINT 'ok 2 - residual is issued less returned plus absorbed less completed -- REQ: KIT-020';
    ELSE PRINT 'not ok 2 - residual ' + ISNULL(CAST(@res AS varchar(20)),'NULL') + ' does not follow the stated formula -- REQ: KIT-020';

    IF @res = 80.00
        PRINT 'ok 3 - a kit issued 100.00, returned 20.00 and scrapped 10.00 carries 80.00 residual -- REQ: KIT-020  AC: AC-081';
    ELSE PRINT 'not ok 3 - residual came to ' + ISNULL(CAST(@res AS varchar(20)),'NULL') + ' rather than 80.00 -- REQ: KIT-020  AC: AC-081';

    IF @res <> @iss - @ret - @scr
        PRINT 'ok 4 - scrapped material stays inside the issued cost and is not deducted again -- REQ: KIT-014, KIT-020  AC: AC-081';
    ELSE PRINT 'not ok 4 - the scrapped value of ' + ISNULL(CAST(@scr AS varchar(20)),'NULL') + ' was deducted from residual -- REQ: KIT-014  AC: AC-081';

    IF @pln = 134.000000 AND @var = @iss - @ret + @abs - @pln
        PRINT 'ok 5 - planned cost is the requirement at plan, and variance measures actual against it -- REQ: KIT-020';
    ELSE PRINT 'not ok 5 - planned ' + ISNULL(CAST(@pln AS varchar(20)),'NULL') + ' variance ' + ISNULL(CAST(@var AS varchar(20)),'NULL')
              + ' -- REQ: KIT-020';

    /*  6 - a cancelled line is no longer part of the plan. */
    UPDATE kit.ExecutionKitLine SET Status = 'CANCELLED' WHERE ExecutionKitId = @kid AND LineNum = 3;
    DELETE @cs;
    INSERT @cs EXEC kit.usp_KitCostSummary @CompanyCode = 'STC', @KitRef = 'ZZKITC1';
    SELECT @pln = PlannedGBP FROM @cs;
    IF @pln = 130.000000
        PRINT 'ok 6 - a cancelled requirement line drops out of the planned cost -- REQ: KIT-005, KIT-020';
    ELSE PRINT 'not ok 6 - planned cost after cancelling a line came to ' + ISNULL(CAST(@pln AS varchar(20)),'NULL') + ' -- REQ: KIT-020';

    /*  7 - the kit is valued once, not once per material line.  The summary joins the movements to
        the kit, so three open lines must not treble a single issue. */
    IF @iss = 100.00
        PRINT 'ok 7 - a single issue is counted once however many material lines the kit carries -- REQ: KIT-020';
    ELSE PRINT 'not ok 7 - one issue of 100.00 was reported as ' + ISNULL(CAST(@iss AS varchar(20)),'NULL') + ' -- REQ: KIT-020';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 1 to 7 - unexpected error in the material cost tests: ' + @msg;
END CATCH

/*  8 to 10 - AC-081: a made subassembly is charged to its parent once, at the value of the
    subassembly itself.  The children consumed to build it were already charged to the
    subassembly's own kit and must not be charged to the parent a second time.
    REQ: KIT-020  AC: AC-081 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, QuantumQty, IsIndivisible,
            TraceabilityType, MakeBuy, ValuationGroup, CostMethod, LifecycleStatus, IsManufacturable, CreatedBy)
    VALUES ('STC', @sa, 'Subassembly used by the cost tests', 'MADE', @uom, 1, 1, 'NONE', 'MAKE', 'MADE', 'MWA', 'RELEASED', 1, 'TEST');

    /*  The child kit: raw material worth 60.00 was consumed to make one subassembly. */
    INSERT kit.ExecutionKit (CompanyCode, SiteCode, KitRef, ParentItemCode, SourceDemandType, SourceDemandRef,
            PlannedGoodQty, CompletedGoodQty, NeedDate, Status, ReleasedBy, CreatedBy)
    VALUES ('STC', @site, 'ZZKITSUB', @sa, 'PARENTKIT', 'ZZKITTOP', 1, 1, '2026-10-20', 'OPCOMPLETE', 'SUPERVISOR', 'TEST');
    SELECT @kid = ExecutionKitId FROM kit.ExecutionKit WHERE CompanyCode = 'STC' AND KitRef = 'ZZKITSUB';
    INSERT kit.ExecutionKitLine (ExecutionKitId, LineNum, PathRef, ComponentItemCode, UsageBasis,
            OriginalRequirementQty, CurrentRequirementQty, UomCode, GrossIssuedQty, NetIssuedQty,
            RemainingQty, PlannedUnitCostGBP, IssuedValueGBP, Status)
    VALUES (@kid, 1, '1', 'ZZ-COMP-A', 'PROPORTION', 20, 20, @uom, 20, 20, 0, 2.000000, 40.00, 'ISSUED'),
           (@kid, 2, '2', 'ZZ-COMP-B', 'PROPORTION', 10, 10, @uom, 10, 10, 0, 2.000000, 20.00, 'ISSUED');
    INSERT kit.KitIssue (CompanyCode, KitRef, LineNum, ComponentItemCode, IssueType, Quantity, UomCode,
            StockQty, ActualDate, PostingDate, OperatorUser, UnitCostGBP, ValueGBP, CreatedBy)
    VALUES ('STC','ZZKITSUB',1,'ZZ-COMP-A','ISSUE', 20, @uom, 20, '2026-10-05','2026-10-05','STORES', 2.000000, 40.00, 'TEST'),
           ('STC','ZZKITSUB',2,'ZZ-COMP-B','ISSUE', 10, @uom, 10, '2026-10-05','2026-10-05','STORES', 2.000000, 20.00, 'TEST');
    INSERT kit.KitCompletion (CompanyCode, KitRef, CompletionSeq, ParentItemCode, GoodQty, CompletionDate,
            PostingDate, UnitCostGBP, WipTransferGBP, IsFinalCompletion, ApprovedBy, CreatedBy)
    VALUES ('STC','ZZKITSUB',1, @sa, 1, '2026-10-06','2026-10-06', 100.000000, 100.00, 1, 'SUPERVISOR', 'TEST');

    /*  The parent kit consumes the finished subassembly at its own carrying value of 100.00. */
    INSERT kit.ExecutionKit (CompanyCode, SiteCode, KitRef, ParentItemCode, SourceDemandType, SourceDemandRef,
            PlannedGoodQty, NeedDate, Status, ReleasedBy, CreatedBy)
    VALUES ('STC', @site, 'ZZKITTOP', @parent, 'STOCK', 'ZZKITTOP', 1, '2026-10-25', 'PARTISSUED', 'SUPERVISOR', 'TEST');
    SELECT @kid = ExecutionKitId FROM kit.ExecutionKit WHERE CompanyCode = 'STC' AND KitRef = 'ZZKITTOP';
    INSERT kit.ExecutionKitLine (ExecutionKitId, LineNum, PathRef, ComponentItemCode, UsageBasis,
            OriginalRequirementQty, CurrentRequirementQty, UomCode, GrossIssuedQty, NetIssuedQty,
            RemainingQty, PlannedUnitCostGBP, IssuedValueGBP, Status)
    VALUES (@kid, 1, '1', @sa, 'PROPORTION', 1, 1, @uom, 1, 1, 0, 100.000000, 100.00, 'ISSUED');
    INSERT kit.KitIssue (CompanyCode, KitRef, LineNum, ComponentItemCode, IssueType, Quantity, UomCode,
            StockQty, ActualDate, PostingDate, OperatorUser, UnitCostGBP, ValueGBP, CreatedBy)
    VALUES ('STC','ZZKITTOP',1, @sa, 'ISSUE', 1, @uom, 1, '2026-10-07','2026-10-07','STORES', 100.000000, 100.00, 'TEST');

    DELETE @cs;
    INSERT @cs EXEC kit.usp_KitCostSummary @CompanyCode = 'STC', @KitRef = 'ZZKITTOP';
    SELECT @iss = IssuedGBP, @res = ResidualGBP FROM @cs;
    IF @iss = 100.00
        PRINT 'ok 8 - the parent is charged the subassembly value once, not that value plus its children -- REQ: KIT-020  AC: AC-081';
    ELSE PRINT 'not ok 8 - the parent was charged ' + ISNULL(CAST(@iss AS varchar(20)),'NULL') + ' for a subassembly worth 100.00 -- REQ: KIT-020  AC: AC-081';
    IF @res = 100.00
        PRINT 'ok 9 - the parent residual carries the subassembly once -- REQ: KIT-020  AC: AC-081';
    ELSE PRINT 'not ok 9 - the parent residual came to ' + ISNULL(CAST(@res AS varchar(20)),'NULL') + ' -- REQ: KIT-020  AC: AC-081';

    DELETE @cs;
    INSERT @cs EXEC kit.usp_KitCostSummary @CompanyCode = 'STC', @KitRef = 'ZZKITSUB';
    SELECT @iss = IssuedGBP FROM @cs;
    IF @iss = 60.00
        PRINT 'ok 10 - the historical child material stays on the subassembly kit where it was spent -- REQ: KIT-020  AC: AC-081';
    ELSE PRINT 'not ok 10 - the subassembly kit reported ' + ISNULL(CAST(@iss AS varchar(20)),'NULL') + ' rather than 60.00 -- REQ: KIT-020';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 8 to 10 - unexpected error in the subassembly cost tests: ' + @msg;
END CATCH

/*  11 to 14 - the as at date, the empty kit, the company scope and the formula as an invariant
    over everything the company holds.  REQ: KIT-020, KIT-022, COM-001 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT kit.ExecutionKit (CompanyCode, SiteCode, KitRef, ParentItemCode, PlannedGoodQty, NeedDate, Status, CreatedBy)
    VALUES ('STC', @site, 'ZZKITC2', @parent, 4, '2026-11-18', 'RELEASED', 'TEST'),
           ('STC', @site, 'ZZKITC3', @parent, 4, '2026-11-19', 'DRAFT',    'TEST');
    INSERT kit.KitIssue (CompanyCode, KitRef, LineNum, ComponentItemCode, IssueType, Quantity, UomCode,
            StockQty, ActualDate, PostingDate, OperatorUser, UnitCostGBP, ValueGBP, CreatedBy)
    VALUES ('STC','ZZKITC2',1,'ZZ-COMP-A','ISSUE', 10, @uom, 10, '2026-10-02','2026-10-02','STORES', 2.000000, 20.00, 'TEST'),
           ('STC','ZZKITC2',1,'ZZ-COMP-A','ISSUE', 15, @uom, 15, '2026-10-20','2026-10-20','STORES', 2.000000, 30.00, 'TEST');

    DELETE @cs;
    INSERT @cs EXEC kit.usp_KitCostSummary @CompanyCode = 'STC', @KitRef = 'ZZKITC2', @AsAt = '2026-10-10';
    SELECT @iss = IssuedGBP FROM @cs;
    DELETE @cs;
    INSERT @cs EXEC kit.usp_KitCostSummary @CompanyCode = 'STC', @KitRef = 'ZZKITC2';
    SELECT @res = IssuedGBP FROM @cs;
    IF @iss = 20.00 AND @res = 50.00
        PRINT 'ok 11 - a cost summary as at a date ignores what was posted after it -- REQ: KIT-020, KIT-022';
    ELSE PRINT 'not ok 11 - as at 10 October reported ' + ISNULL(CAST(@iss AS varchar(20)),'NULL')
              + ' against ' + ISNULL(CAST(@res AS varchar(20)),'NULL') + ' to date -- REQ: KIT-020';

    DELETE @cs;
    INSERT @cs EXEC kit.usp_KitCostSummary @CompanyCode = 'STC', @KitRef = 'ZZKITC3';
    IF EXISTS (SELECT 1 FROM @cs WHERE PlannedGBP = 0 AND IssuedGBP = 0 AND ReturnedGBP = 0 AND ScrappedGBP = 0
                                   AND AbsorbedGBP = 0 AND CompletedGBP = 0 AND ResidualGBP = 0)
        PRINT 'ok 12 - a kit with no movements reports zero values, not an empty row -- REQ: KIT-020';
    ELSE PRINT 'not ok 12 - a kit with no movements did not report a row of zeroes -- REQ: KIT-020';

    DELETE @cs;
    IF @other IS NOT NULL INSERT @cs EXEC kit.usp_KitCostSummary @CompanyCode = @other;
    IF @other IS NULL OR NOT EXISTS (SELECT 1 FROM @cs WHERE KitRef LIKE 'ZZKIT%')
        PRINT 'ok 13 - a cost summary answers for one company only -- REQ: KIT-020, COM-001';
    ELSE PRINT 'not ok 13 - another company could see the kits of STC -- REQ: COM-001';

    DELETE @cs;
    INSERT @cs EXEC kit.usp_KitCostSummary @CompanyCode = 'STC';
    SET @n = (SELECT COUNT(*) FROM @cs WHERE ResidualGBP <> IssuedGBP - ReturnedGBP + AbsorbedGBP - CompletedGBP);
    IF @n = 0 AND EXISTS (SELECT 1 FROM @cs)
        PRINT 'ok 14 - the residual formula holds for every kit in the company, not only the built cases -- REQ: KIT-020';
    ELSE PRINT 'not ok 14 - ' + ISNULL(CAST(@n AS varchar(10)),'NULL') + ' kit(s) break the residual formula -- REQ: KIT-020';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 11 to 14 - unexpected error: ' + @msg;
END CATCH
GO
