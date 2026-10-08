/*  The kit views and the recall trace.  The views carry no rules of their own: they present what
    the engine wrote, one row per kit for the execution list, one row per short line for the
    storeman, and the genealogy with the planned part beside the part actually fitted.  What is
    tested here is the shape, the company and site scope, and the fact that a short line is
    pickable rather than hidden inside a kit level summary.
    REQ: KIT-012, KIT-019, KIT-022, COM-001  AC: AC-083 */
SET NOCOUNT ON;
GO
PRINT '# kit-views';
/*  Re-runnable: clear anything an aborted earlier run could have left behind. */
DELETE g FROM kit.KitGenealogy g WHERE g.KitRef LIKE 'ZZKIT%';
DELETE i FROM kit.KitIssue i WHERE i.KitRef LIKE 'ZZKIT%';
DELETE c FROM kit.KitCompletion c WHERE c.KitRef LIKE 'ZZKIT%';
DELETE l FROM kit.ExecutionKitLine l JOIN kit.ExecutionKit k ON k.ExecutionKitId = l.ExecutionKitId WHERE k.KitRef LIKE 'ZZKIT%';
DELETE k FROM kit.ExecutionKit k WHERE k.KitRef LIKE 'ZZKIT%';
GO
DECLARE @msg varchar(400), @site varchar(10), @site2 varchar(10), @parent varchar(40), @desc varchar(100),
        @uom varchar(10), @other varchar(10), @kid bigint, @kid2 bigint, @issue bigint, @n int,
        @ready bit, @open bit, @short decimal(28,6), @cols int;

SELECT TOP (1) @site = SiteCode FROM core.Site WHERE CompanyCode = 'STC' ORDER BY SiteCode;
SELECT TOP (1) @site2 = SiteCode FROM core.Site WHERE CompanyCode = 'STC' AND SiteCode <> @site ORDER BY SiteCode;
SELECT TOP (1) @parent = ItemCode, @desc = Description FROM inv.Item
 WHERE CompanyCode = 'STC' AND ItemClass = 'MADE' ORDER BY ItemCode;
SELECT TOP (1) @uom = StockUomCode FROM inv.Item WHERE CompanyCode = 'STC' AND ItemClass = 'BOUGHT' ORDER BY ItemCode;
SET @uom = ISNULL(@uom, 'EA');
SELECT TOP (1) @other = CompanyCode FROM core.Company WHERE CompanyCode <> 'STC' ORDER BY CompanyCode;

/*  1 - the execution view offers the whole execution list shape in one row per kit.
    REQ: KIT-022 */
SELECT @cols = COUNT(*) FROM sys.columns
 WHERE object_id = OBJECT_ID(N'kit.vw_KitExecution')
   AND name IN ('CompanyCode','SiteCode','KitRef','ParentItemCode','ParentDescription','Status',
                'SourceDemandType','SourceDemandRef','PlannedGoodQty','CompletedGoodQty','RejectedQty',
                'NeedDate','PlannedCompletionDate','ActualCompletionDate','OwnerUser','ReleasedBy',
                'WipValueGBP','IsBackflush','LineCount','ShortLines','ExcessLines','IssuedQty',
                'ReturnedQty','ScrappedQty','RemainingQty','IsReady','IsOpen');
IF @cols = 27 PRINT 'ok 1 - the kit execution view presents the full execution list shape -- REQ: KIT-022';
ELSE PRINT 'not ok 1 - the execution view exposes ' + ISNULL(CAST(@cols AS varchar(10)),'NULL') + ' of the 27 expected columns -- REQ: KIT-022';

/*  2 - the shortage view is a working list of lines, with everything needed to go and pick one. */
SELECT @cols = COUNT(*) FROM sys.columns
 WHERE object_id = OBJECT_ID(N'kit.vw_KitShortage')
   AND name IN ('CompanyCode','SiteCode','KitRef','ParentItemCode','KitStatus','LineNum',
                'ComponentItemCode','ComponentDesc','PlannedItemCode','SubstituteItemCode','UomCode',
                'CurrentRequirementQty','ReservedQty','PickedQty','GrossIssuedQty','NetIssuedQty',
                'RemainingQty','ExcessQty','NeedDate','ShortQty','LineStatus');
IF @cols = 21 PRINT 'ok 2 - the shortage view presents one pickable line, not a kit level summary -- REQ: KIT-009, KIT-022';
ELSE PRINT 'not ok 2 - the shortage view exposes ' + ISNULL(CAST(@cols AS varchar(10)),'NULL') + ' of the 21 expected columns -- REQ: KIT-022';

/*  3 - the genealogy view carries the planned part beside the fitted part. */
SELECT @cols = COUNT(*) FROM sys.columns
 WHERE object_id = OBJECT_ID(N'kit.vw_KitGenealogyLine')
   AND name IN ('CompanyCode','GenealogyId','KitRef','CompletionId','ParentItemCode','ParentLotCode',
                'ParentSerialNo','ComponentItemCode','ComponentLotCode','ComponentSerialNo','ConsumedQty',
                'WasSubstitute','PlannedItemCode','IssueType','ActualDate','OperatorUser','UomCode');
IF @cols = 17 PRINT 'ok 3 - the genealogy view presents the fitted part, its lot and the part planned -- REQ: KIT-012, KIT-019';
ELSE PRINT 'not ok 3 - the genealogy view exposes ' + ISNULL(CAST(@cols AS varchar(10)),'NULL') + ' of the 17 expected columns -- REQ: KIT-019';

/*  4 to 9 - the execution and shortage views over kits built for the purpose.
    REQ: KIT-022  AC: AC-083 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT kit.ExecutionKit (CompanyCode, SiteCode, KitRef, ParentItemCode, SourceDemandType, SourceDemandRef,
            PlannedGoodQty, CompletedGoodQty, RejectedQty, NeedDate, OwnerUser, Status, ReleasedBy, WipValueGBP, IsBackflush, CreatedBy)
    VALUES ('STC', @site, 'ZZKITV1', @parent, 'JOB', 'ZZJOB1', 10, 4, 1, '2026-11-05', 'PLANNER', 'PARTISSUED', 'SUPERVISOR', 500.00, 0, 'TEST'),
           ('STC', @site, 'ZZKITV2', @parent, 'JOB', 'ZZJOB2',  5, 5, 0, '2026-09-30', 'PLANNER', 'FINCLOSED',  'SUPERVISOR',   0.00, 1, 'TEST'),
           ('STC', @site, 'ZZKITV3', @parent, 'JOB', 'ZZJOB3',  8, 0, 0, '2026-11-06', 'ENGINEER','DRAFT',      NULL,           0.00, 0, 'TEST');
    SELECT @kid  = ExecutionKitId FROM kit.ExecutionKit WHERE CompanyCode = 'STC' AND KitRef = 'ZZKITV1';
    SELECT @kid2 = ExecutionKitId FROM kit.ExecutionKit WHERE CompanyCode = 'STC' AND KitRef = 'ZZKITV3';
    INSERT kit.ExecutionKitLine (ExecutionKitId, LineNum, PathRef, ComponentItemCode, UsageBasis,
            OriginalRequirementQty, CurrentRequirementQty, UomCode, ReservedQty, PickedQty, GrossIssuedQty,
            ReturnedQty, ScrappedQty, NetIssuedQty, RemainingQty, ExcessQty, NeedDate, PlannedUnitCostGBP, Status)
    VALUES (@kid, 1, '1', 'ZZ-COMP-A', 'PROPORTION', 40, 40, @uom, 10, 10, 20, 2, 1, 18, 22, 0, '2026-11-04', 1.000000, 'PARTISSUED'),
           (@kid, 2, '2', 'ZZ-COMP-B', 'PROPORTION', 20, 20, @uom,  5,  0,  0, 0, 0,  0, 20, 0, NULL,         2.000000, 'OPEN'),
           (@kid, 3, '3', 'ZZ-COMP-C', 'PROPORTION', 10,  6, @uom,  0,  8,  8, 0, 0,  8,  0, 2, NULL,         3.000000, 'ISSUED'),
           (@kid, 4, '4', 'ZZ-COMP-D', 'PROPORTION',  5,  5, @uom,  0,  0,  0, 0, 0,  0,  5, 0, NULL,         4.000000, 'CANCELLED'),
           (@kid2,1, '1', 'ZZ-COMP-A', 'PROPORTION', 12, 12, @uom,  0,  0,  0, 0, 0,  0, 12, 0, NULL,         1.000000, 'OPEN');

    SELECT @ready = IsReady, @open = IsOpen, @n = LineCount FROM kit.vw_KitExecution
     WHERE CompanyCode = 'STC' AND KitRef = 'ZZKITV1';
    IF @n = 3 AND @ready = 0 AND @open = 1
        PRINT 'ok 4 - a cancelled line is left out of the execution list and a short line keeps the kit open and not ready -- REQ: KIT-005, KIT-022';
    ELSE PRINT 'not ok 4 - line count ' + ISNULL(CAST(@n AS varchar(10)),'NULL') + ', ready ' + ISNULL(CAST(@ready AS varchar(4)),'NULL')
              + ', open ' + ISNULL(CAST(@open AS varchar(4)),'NULL') + ' -- REQ: KIT-022';

    IF EXISTS (SELECT 1 FROM kit.vw_KitExecution
                WHERE CompanyCode = 'STC' AND KitRef = 'ZZKITV1'
                  AND ShortLines = 2 AND ExcessLines = 1 AND IssuedQty = 28 AND ReturnedQty = 2
                  AND ScrappedQty = 1 AND RemainingQty = 42 AND ParentDescription = @desc
                  AND SourceDemandRef = 'ZZJOB1' AND CompletedGoodQty = 4 AND RejectedQty = 1)
        PRINT 'ok 5 - the execution row reports the engine''s own issued, returned, scrapped and outstanding quantities -- REQ: KIT-022';
    ELSE PRINT 'not ok 5 - the execution row did not reconcile to the lines the engine wrote -- REQ: KIT-022';

    SELECT @open = IsOpen FROM kit.vw_KitExecution WHERE CompanyCode = 'STC' AND KitRef = 'ZZKITV2';
    IF @open = 0 PRINT 'ok 6 - a financially closed kit is no longer an open kit -- REQ: KIT-017, KIT-024';
    ELSE PRINT 'not ok 6 - a financially closed kit is still reported open -- REQ: KIT-017';

    SET @n = (SELECT COUNT(*) FROM kit.vw_KitShortage WHERE CompanyCode = 'STC' AND KitRef = 'ZZKITV1');
    SELECT @short = ShortQty FROM kit.vw_KitShortage WHERE CompanyCode = 'STC' AND KitRef = 'ZZKITV1' AND LineNum = 1;
    IF @n = 2 AND @short = 12
        PRINT 'ok 7 - every short line is listed on its own with the quantity still to find -- REQ: KIT-009, KIT-022';
    ELSE PRINT 'not ok 7 - ' + ISNULL(CAST(@n AS varchar(10)),'NULL') + ' short line(s) with a first shortage of '
              + ISNULL(CAST(@short AS varchar(20)),'NULL') + ' -- REQ: KIT-022';

    IF NOT EXISTS (SELECT 1 FROM kit.vw_KitShortage WHERE CompanyCode = 'STC' AND KitRef = 'ZZKITV1' AND LineNum IN (3,4))
       AND NOT EXISTS (SELECT 1 FROM kit.vw_KitShortage WHERE CompanyCode = 'STC' AND KitRef = 'ZZKITV3')
        PRINT 'ok 8 - a covered line, a cancelled line and an unreleased kit are not on the picking list -- REQ: KIT-009, KIT-022';
    ELSE PRINT 'not ok 8 - the picking list offered work that is not outstanding -- REQ: KIT-022';

    IF EXISTS (SELECT 1 FROM kit.vw_KitShortage WHERE CompanyCode = 'STC' AND KitRef = 'ZZKITV1' AND LineNum = 1 AND NeedDate = '2026-11-04')
       AND EXISTS (SELECT 1 FROM kit.vw_KitShortage WHERE CompanyCode = 'STC' AND KitRef = 'ZZKITV1' AND LineNum = 2 AND NeedDate = '2026-11-05'
                     AND KitStatus = 'PARTISSUED' AND UomCode = @uom AND LineStatus = 'OPEN')
        PRINT 'ok 9 - a line keeps its own need date and otherwise inherits the kit''s -- REQ: KIT-009, KIT-022';
    ELSE PRINT 'not ok 9 - the need date on the picking list did not fall back to the kit -- REQ: KIT-022';

    /*  10 - the views are scoped by company and site, so one site cannot pick another's work. */
    SET @n = (SELECT COUNT(*) FROM kit.vw_KitExecution WHERE CompanyCode = ISNULL(@other,'ZZNONE') AND KitRef LIKE 'ZZKITV%');
    SET @cols = (SELECT COUNT(*) FROM kit.vw_KitShortage WHERE CompanyCode = 'STC' AND SiteCode = ISNULL(@site2,'ZZNOSITE') AND KitRef = 'ZZKITV1');
    IF @n = 0 AND @cols = 0
       AND EXISTS (SELECT 1 FROM kit.vw_KitExecution WHERE CompanyCode = 'STC' AND SiteCode = @site AND KitRef = 'ZZKITV1')
        PRINT 'ok 10 - the execution and shortage views answer for one company and site only -- REQ: KIT-022, COM-001';
    ELSE PRINT 'not ok 10 - the company or site scope of the kit views leaked -- REQ: COM-001';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 4 to 10 - unexpected error in the execution and shortage view tests: ' + @msg;
END CATCH

/*  11 to 16 - genealogy: the part fitted, the part planned, and the walk back up the levels.
    REQ: KIT-012, KIT-019  AC: AC-083 */
DECLARE @gen TABLE (Depth int, GenealogyId bigint, KitRef varchar(30), ParentItemCode varchar(40),
                    ParentLotCode varchar(30), ParentSerialNo varchar(40), ComponentItemCode varchar(40),
                    ComponentLotCode varchar(30), ComponentSerialNo varchar(40), ConsumedQty decimal(28,6),
                    WasSubstitute bit, PlannedItemCode varchar(40));
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT kit.KitIssue (CompanyCode, KitRef, LineNum, ComponentItemCode, IssueType, Quantity, UomCode,
            StockQty, LotCode, ActualDate, PostingDate, OperatorUser, UnitCostGBP, ValueGBP, CreatedBy)
    VALUES ('STC','ZZKITG1',1,'ZZ-COMP-A','ISSUE', 5, @uom, 5, 'ZZLOT-A', '2026-10-08','2026-10-08','STORES', 2.000000, 10.00, 'TEST');
    SET @issue = SCOPE_IDENTITY();

    /*  Level one: the component went into the subassembly.  Level two: the subassembly went into
        the finished parent, and it was a substitute for the part the bill asked for.            */
    INSERT kit.KitGenealogy (CompanyCode, KitRef, ParentItemCode, ParentLotCode, ComponentItemCode,
            ComponentLotCode, ConsumedQty, WasSubstitute, PlannedItemCode, KitIssueId)
    VALUES ('STC','ZZKITG1','ZZ-KIT-SA','ZZLOT-SA','ZZ-COMP-A','ZZLOT-A', 5, 0, NULL,        @issue),
           ('STC','ZZKITG2', @parent,   'ZZLOT-TOP','ZZ-KIT-SA','ZZLOT-SA',1, 1, 'ZZ-COMP-X', NULL);

    IF EXISTS (SELECT 1 FROM kit.vw_KitGenealogyLine
                WHERE CompanyCode = 'STC' AND KitRef = 'ZZKITG1'
                  AND PlannedItemCode = 'ZZ-COMP-A' AND WasSubstitute = 0)
        PRINT 'ok 11 - where nothing was substituted the planned part is the part fitted -- REQ: KIT-019';
    ELSE PRINT 'not ok 11 - an unsubstituted line did not report the fitted part as the planned part -- REQ: KIT-019';

    IF EXISTS (SELECT 1 FROM kit.vw_KitGenealogyLine
                WHERE CompanyCode = 'STC' AND KitRef = 'ZZKITG2'
                  AND ComponentItemCode = 'ZZ-KIT-SA' AND PlannedItemCode = 'ZZ-COMP-X' AND WasSubstitute = 1)
        PRINT 'ok 12 - a substitution is shown as evidence: the part fitted beside the part planned -- REQ: KIT-012, KIT-019  AC: AC-083';
    ELSE PRINT 'not ok 12 - a substituted line did not distinguish the fitted part from the planned part -- REQ: KIT-012';

    IF EXISTS (SELECT 1 FROM kit.vw_KitGenealogyLine
                WHERE KitRef = 'ZZKITG1' AND IssueType = 'ISSUE' AND ActualDate = '2026-10-08'
                  AND OperatorUser = 'STORES' AND UomCode = @uom)
       AND EXISTS (SELECT 1 FROM kit.vw_KitGenealogyLine WHERE KitRef = 'ZZKITG2' AND IssueType = 'CONSUMED')
        PRINT 'ok 13 - the genealogy line carries the movement that put the part there, or says it was consumed -- REQ: KIT-019';
    ELSE PRINT 'not ok 13 - the genealogy line did not pick up the issue behind it -- REQ: KIT-019';

    DELETE @gen;
    INSERT @gen EXEC kit.usp_KitGenealogy @CompanyCode = 'STC', @ItemCode = 'ZZ-COMP-A', @LotCode = 'ZZLOT-A';
    IF EXISTS (SELECT 1 FROM @gen WHERE Depth = 1 AND ParentItemCode = 'ZZ-KIT-SA' AND ParentLotCode = 'ZZLOT-SA')
       AND EXISTS (SELECT 1 FROM @gen WHERE Depth = 2 AND ParentItemCode = @parent AND ParentLotCode = 'ZZLOT-TOP')
        PRINT 'ok 14 - a recall trace walks a lot up through the subassembly to the finished parent -- REQ: KIT-019  AC: AC-083';
    ELSE PRINT 'not ok 14 - the backward trace did not reach the finished parent -- REQ: KIT-019';

    DELETE @gen;
    INSERT @gen EXEC kit.usp_KitGenealogy @CompanyCode = 'STC', @ItemCode = 'ZZ-COMP-A', @LotCode = 'ZZLOT-A', @MaxDepth = 1;
    IF NOT EXISTS (SELECT 1 FROM @gen WHERE Depth > 1) AND EXISTS (SELECT 1 FROM @gen WHERE Depth = 1)
        PRINT 'ok 15 - the trace stops at the depth it was asked for -- REQ: KIT-019';
    ELSE PRINT 'not ok 15 - the trace ignored the depth it was given -- REQ: KIT-019';

    DELETE @gen;
    INSERT @gen EXEC kit.usp_KitGenealogy @CompanyCode = 'STC', @ItemCode = 'ZZ-COMP-A', @LotCode = 'ZZLOT-NOTHING';
    SET @n = (SELECT COUNT(*) FROM @gen);
    DELETE @gen;
    IF @other IS NOT NULL INSERT @gen EXEC kit.usp_KitGenealogy @CompanyCode = @other, @ItemCode = 'ZZ-COMP-A';
    IF @n = 0 AND NOT EXISTS (SELECT 1 FROM @gen)
        PRINT 'ok 16 - the trace answers for the stated lot and company only -- REQ: KIT-019, COM-001';
    ELSE PRINT 'not ok 16 - the lot or company scope of the recall trace leaked -- REQ: KIT-019, COM-001';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 11 to 16 - unexpected error in the genealogy tests: ' + @msg;
END CATCH
GO
