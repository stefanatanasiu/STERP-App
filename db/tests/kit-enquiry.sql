/*  Kit readiness and multi-kit trial allocation.  A kit is ready only when EVERY material line is
    covered, and the free stock of a shared component is promised ONCE, down an explicit priority
    sequence, so two competing kits can never both be reported complete on the same units.
    Every assertion builds its own kits inside a transaction that is rolled back, so the demo data
    and the stock ledger are left exactly as they were found.
    REQ: KIT-006, KIT-007, KIT-022  AC: AC-078 */
SET NOCOUNT ON;
GO
PRINT '# kit-enquiry';
/*  Re-runnable: clear anything an aborted earlier run could have left behind. */
DELETE g FROM kit.KitGenealogy g WHERE g.KitRef LIKE 'ZZKIT%';
DELETE i FROM kit.KitIssue i WHERE i.KitRef LIKE 'ZZKIT%';
DELETE c FROM kit.KitCompletion c WHERE c.KitRef LIKE 'ZZKIT%';
DELETE l FROM kit.ExecutionKitLine l JOIN kit.ExecutionKit k ON k.ExecutionKitId = l.ExecutionKitId WHERE k.KitRef LIKE 'ZZKIT%';
DELETE k FROM kit.ExecutionKit k WHERE k.KitRef LIKE 'ZZKIT%';
DELETE b FROM inv.StockBalance b WHERE b.ItemCode LIKE 'ZZ-KIT%';
DELETE i FROM inv.Item i WHERE i.ItemCode LIKE 'ZZ-KIT%';
GO
DECLARE @msg varchar(400), @site varchar(10), @whs varchar(20), @loc varchar(20),
        @parent varchar(40), @uom varchar(10), @pin varchar(40) = 'ZZ-KIT-PIN',
        @k1 bigint, @k2 bigint, @k3 bigint, @ready bit, @short int, @covered int,
        @lines int, @pct decimal(5,1), @n int, @free decimal(28,6), @reserved decimal(28,6),
        @a decimal(28,6), @b decimal(28,6), @sa decimal(28,6), @sb decimal(28,6);

SELECT TOP (1) @site = l.SiteCode, @whs = l.WarehouseCode, @loc = l.LocationCode
  FROM inv.Location l
 WHERE l.CompanyCode = 'STC' AND l.LocationType = 'STOCK'
   AND l.IsOwnedStock = 1 AND l.IsAvailable = 1 AND l.IsActive = 1
 ORDER BY l.SiteCode, l.WarehouseCode, l.LocationCode;
SELECT TOP (1) @parent = ItemCode FROM inv.Item
 WHERE CompanyCode = 'STC' AND ItemClass = 'MADE' ORDER BY ItemCode;
SELECT TOP (1) @uom = StockUomCode FROM inv.Item
 WHERE CompanyCode = 'STC' AND ItemClass = 'BOUGHT' ORDER BY ItemCode;
SET @uom = ISNULL(@uom, 'EA');

DECLARE @rd TABLE (KitRef varchar(30), SiteCode varchar(10), ParentItemCode varchar(40), Status varchar(14),
                   PlannedGoodQty decimal(19,6), CompletedGoodQty decimal(19,6), NeedDate date,
                   WipValueGBP decimal(18,2), LineCount int, ShortLines int, CoveredLines int,
                   ReadyPct decimal(5,1), IsReady bit, IssuedQty decimal(28,6),
                   ReturnedQty decimal(28,6), ScrappedQty decimal(28,6));
DECLARE @tr TABLE (Seq int, KitRef varchar(30), LineNum int, ItemCode varchar(40), NeedDate date,
                   RequiredQty decimal(28,6), FreeQty decimal(28,6), ClaimedBefore decimal(28,6),
                   AllocatedQty decimal(28,6), ShortQty decimal(28,6));

/*  1 to 7 - readiness.  ZZKITR1 is covered on every line; ZZKITR2 is short on exactly one.
    REQ: KIT-006, KIT-022 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT kit.ExecutionKit (CompanyCode, SiteCode, KitRef, ParentItemCode, SourceDemandType, SourceDemandRef,
            PlannedGoodQty, CompletedGoodQty, NeedDate, OwnerUser, Status, ReleasedBy, WipValueGBP, CreatedBy)
    VALUES ('STC', @site, 'ZZKITR1', @parent, 'STOCK', 'ZZKITR1', 10, 0, '2026-11-10', 'PLANNER', 'RELEASED', 'SUPERVISOR', 0, 'TEST'),
           ('STC', @site, 'ZZKITR2', @parent, 'STOCK', 'ZZKITR2', 10, 0, '2026-11-11', 'PLANNER', 'RELEASED', 'SUPERVISOR', 0, 'TEST'),
           ('STC', @site, 'ZZKITR3', @parent, 'STOCK', 'ZZKITR3', 10, 0, '2026-11-12', 'PLANNER', 'RELEASED', 'SUPERVISOR', 0, 'TEST');
    SELECT @k1 = ExecutionKitId FROM kit.ExecutionKit WHERE CompanyCode = 'STC' AND KitRef = 'ZZKITR1';
    SELECT @k2 = ExecutionKitId FROM kit.ExecutionKit WHERE CompanyCode = 'STC' AND KitRef = 'ZZKITR2';

    /*  Three covered lines on R1: nothing outstanding beyond what is reserved. */
    INSERT kit.ExecutionKitLine (ExecutionKitId, LineNum, PathRef, ComponentItemCode, UsageBasis,
            OriginalRequirementQty, CurrentRequirementQty, UomCode, ReservedQty, PickedQty,
            GrossIssuedQty, ReturnedQty, ScrappedQty, NetIssuedQty, RemainingQty, ExcessQty,
            PlannedUnitCostGBP, Status)
    VALUES (@k1, 1, '1', 'ZZ-COMP-A', 'PROPORTION', 40, 40, @uom, 40, 0, 0, 0, 0, 0, 40, 0, 1.000000, 'OPEN'),
           (@k1, 2, '2', 'ZZ-COMP-B', 'PROPORTION', 20, 20, @uom, 20, 0, 0, 0, 0, 0, 20, 0, 2.000000, 'OPEN'),
           (@k1, 3, '3', 'ZZ-COMP-C', 'FIXEDRUN',    1,  1, @uom,  0, 0, 6, 2, 1, 4,  0, 0, 5.000000, 'ISSUED'),
           /*  R2 repeats the same three lines but line 2 is reserved for only half of what is left. */
           (@k2, 1, '1', 'ZZ-COMP-A', 'PROPORTION', 40, 40, @uom, 40, 0, 0, 0, 0, 0, 40, 0, 1.000000, 'OPEN'),
           (@k2, 2, '2', 'ZZ-COMP-B', 'PROPORTION', 20, 20, @uom, 10, 0, 0, 0, 0, 0, 20, 0, 2.000000, 'OPEN'),
           (@k2, 3, '3', 'ZZ-COMP-C', 'FIXEDRUN',    1,  1, @uom,  1, 0, 0, 0, 0, 0,  1, 0, 5.000000, 'OPEN');

    DELETE @rd;
    INSERT @rd EXEC kit.usp_KitReadiness @CompanyCode = 'STC', @KitRef = 'ZZKITR1';
    SELECT @ready = IsReady, @short = ShortLines, @covered = CoveredLines, @lines = LineCount, @pct = ReadyPct FROM @rd;
    IF @ready = 1 AND @short = 0 AND @covered = 3 AND @lines = 3 AND @pct = 100.0
        PRINT 'ok 1 - a kit whose every line is covered is reported ready at 100 per cent -- REQ: KIT-006, KIT-022';
    ELSE PRINT 'not ok 1 - ready ' + ISNULL(CAST(@ready AS varchar(4)),'NULL') + ', short ' + ISNULL(CAST(@short AS varchar(8)),'NULL')
              + ', covered ' + ISNULL(CAST(@covered AS varchar(8)),'NULL') + ' of ' + ISNULL(CAST(@lines AS varchar(8)),'NULL')
              + ' at ' + ISNULL(CAST(@pct AS varchar(10)),'NULL') + '% -- REQ: KIT-006';

    DELETE @rd;
    INSERT @rd EXEC kit.usp_KitReadiness @CompanyCode = 'STC', @KitRef = 'ZZKITR2';
    SELECT @ready = IsReady, @short = ShortLines, @covered = CoveredLines, @pct = ReadyPct FROM @rd;
    IF @ready = 0 AND @short = 1 AND @covered = 2
        PRINT 'ok 2 - one short line out of three is enough to make the whole kit not ready -- REQ: KIT-006, KIT-022';
    ELSE PRINT 'not ok 2 - a kit with a short line reported ready ' + ISNULL(CAST(@ready AS varchar(4)),'NULL')
              + ' with ' + ISNULL(CAST(@short AS varchar(8)),'NULL') + ' short line(s) -- REQ: KIT-006';

    IF @pct = 66.7 PRINT 'ok 3 - the readiness percentage is advisory and reports two lines of three as 66.7 -- REQ: KIT-022';
    ELSE PRINT 'not ok 3 - two covered lines of three were reported as ' + ISNULL(CAST(@pct AS varchar(10)),'NULL') + '% -- REQ: KIT-022';

    /*  Covering the one short line is the only thing that changes the answer. */
    UPDATE kit.ExecutionKitLine SET ReservedQty = 20 WHERE ExecutionKitId = @k2 AND LineNum = 2;
    DELETE @rd;
    INSERT @rd EXEC kit.usp_KitReadiness @CompanyCode = 'STC', @KitRef = 'ZZKITR2';
    SELECT @ready = IsReady, @short = ShortLines FROM @rd;
    IF @ready = 1 AND @short = 0
        PRINT 'ok 4 - covering the last short line is what makes the kit ready, nothing else -- REQ: KIT-006';
    ELSE PRINT 'not ok 4 - after covering every line the kit is still reported ready ' + ISNULL(CAST(@ready AS varchar(4)),'NULL') + ' -- REQ: KIT-006';

    /*  A cancelled line is no longer a requirement and must not hold the kit back. */
    UPDATE kit.ExecutionKitLine SET ReservedQty = 0, Status = 'CANCELLED' WHERE ExecutionKitId = @k2 AND LineNum = 2;
    DELETE @rd;
    INSERT @rd EXEC kit.usp_KitReadiness @CompanyCode = 'STC', @KitRef = 'ZZKITR2';
    SELECT @ready = IsReady, @lines = LineCount FROM @rd;
    IF @ready = 1 AND @lines = 2
        PRINT 'ok 5 - a cancelled line leaves the requirement and does not count against readiness -- REQ: KIT-005, KIT-006';
    ELSE PRINT 'not ok 5 - a cancelled line left ' + ISNULL(CAST(@lines AS varchar(8)),'NULL')
              + ' line(s) and readiness ' + ISNULL(CAST(@ready AS varchar(4)),'NULL') + ' -- REQ: KIT-006';

    /*  A kit with no material lines at all is not ready; there is nothing to be ready about. */
    DELETE @rd;
    INSERT @rd EXEC kit.usp_KitReadiness @CompanyCode = 'STC', @KitRef = 'ZZKITR3';
    SELECT @ready = IsReady, @lines = LineCount, @pct = ReadyPct FROM @rd;
    IF @ready = 0 AND @lines = 0 AND @pct = 0
        PRINT 'ok 6 - a kit with no material lines is not reported ready -- REQ: KIT-006, KIT-022';
    ELSE PRINT 'not ok 6 - an empty kit reported ready ' + ISNULL(CAST(@ready AS varchar(4)),'NULL') + ' -- REQ: KIT-006';

    /*  Issued, returned and scrapped are carried on the readiness row from the lines themselves. */
    DELETE @rd;
    INSERT @rd EXEC kit.usp_KitReadiness @CompanyCode = 'STC', @KitRef = 'ZZKITR1';
    IF EXISTS (SELECT 1 FROM @rd WHERE IssuedQty = 6 AND ReturnedQty = 2 AND ScrappedQty = 1)
        PRINT 'ok 7 - issued, returned and scrapped quantities are reported beside readiness -- REQ: KIT-013, KIT-014, KIT-022';
    ELSE PRINT 'not ok 7 - the issued, returned and scrapped totals did not reconcile to the lines -- REQ: KIT-022';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 1 to 7 - unexpected error in the readiness tests: ' + @msg;
END CATCH

/*  8 - the company and site scope of the readiness enquiry.  REQ: KIT-022, COM-001 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT kit.ExecutionKit (CompanyCode, SiteCode, KitRef, ParentItemCode, PlannedGoodQty, NeedDate, Status, CreatedBy)
    VALUES ('STC', @site, 'ZZKITR4', @parent, 5, '2026-11-20', 'RELEASED', 'TEST');
    DELETE @rd;
    INSERT @rd EXEC kit.usp_KitReadiness @CompanyCode = 'STC', @SiteCode = 'ZZNOSITE';
    SET @n = (SELECT COUNT(*) FROM @rd WHERE KitRef = 'ZZKITR4');
    DELETE @rd;
    INSERT @rd EXEC kit.usp_KitReadiness @CompanyCode = 'STC', @SiteCode = @site, @Status = 'RELEASED';
    IF @n = 0 AND EXISTS (SELECT 1 FROM @rd WHERE KitRef = 'ZZKITR4')
       AND NOT EXISTS (SELECT 1 FROM @rd WHERE Status <> 'RELEASED' OR SiteCode <> @site)
        PRINT 'ok 8 - the readiness enquiry answers for one company, site and status only -- REQ: KIT-022, COM-001';
    ELSE PRINT 'not ok 8 - the site or status scope of the readiness enquiry leaked -- REQ: KIT-022';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 8 - unexpected error: ' + @msg;
END CATCH

/*  9 to 14 - AC-078.  Two kits each need four of the same pin and only six are free.  The higher
    priority kit is covered, the other is short two, and the six units are promised once.
    REQ: KIT-006, KIT-007  AC: AC-078 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, QuantumQty, IsIndivisible,
            TraceabilityType, MakeBuy, ValuationGroup, CostMethod, LifecycleStatus, CreatedBy)
    VALUES ('STC', @pin, 'Trial allocation test pin', 'BOUGHT', @uom, 1, 1, 'NONE', 'BUY', 'BOUGHT', 'MWA', 'RELEASED', 'TEST');
    INSERT inv.StockBalance (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode,
            OwnerType, QualityStatus, QtyOnHand, QtyReserved)
    VALUES ('STC', @site, @whs, @loc, @pin, 'OWNED', 'ACCEPTED', 6, 0);

    INSERT kit.ExecutionKit (CompanyCode, SiteCode, KitRef, ParentItemCode, SourceDemandType, SourceDemandRef,
            PlannedGoodQty, NeedDate, OwnerUser, Status, ReleasedBy, CreatedBy)
    VALUES ('STC', @site, 'ZZKITA', @parent, 'SALESORDER', 'ZZKITA', 4, '2026-11-02', 'PLANNER', 'RELEASED', 'SUPERVISOR', 'TEST'),
           ('STC', @site, 'ZZKITB', @parent, 'SALESORDER', 'ZZKITB', 4, '2026-11-09', 'PLANNER', 'RELEASED', 'SUPERVISOR', 'TEST'),
           ('STC', @site, 'ZZKITD', @parent, 'STOCK',      'ZZKITD', 4, '2026-11-01', 'ENGINEER', 'DRAFT',    NULL,         'TEST');
    INSERT kit.ExecutionKitLine (ExecutionKitId, LineNum, PathRef, ComponentItemCode, UsageBasis,
            OriginalRequirementQty, CurrentRequirementQty, UomCode, RemainingQty, NeedDate, PlannedUnitCostGBP, Status)
    SELECT k.ExecutionKitId, 1, '1', @pin, 'PROPORTION', 4, 4, @uom, 4, k.NeedDate, 0.310000, 'OPEN'
      FROM kit.ExecutionKit k
     WHERE k.CompanyCode = 'STC' AND k.KitRef IN ('ZZKITA','ZZKITB','ZZKITD');

    SELECT @free = SUM(QtyOnHand), @reserved = SUM(QtyReserved) FROM inv.StockBalance
     WHERE CompanyCode = 'STC' AND ItemCode = @pin;
    SET @n = (SELECT COUNT(*) FROM inv.StockReservation WHERE CompanyCode = 'STC' AND ItemCode = @pin);

    DELETE @tr;
    INSERT @tr EXEC kit.usp_TrialAllocate @CompanyCode = 'STC', @SiteCode = @site, @ItemCode = @pin;

    SELECT @a = AllocatedQty, @sa = ShortQty FROM @tr WHERE KitRef = 'ZZKITA';
    SELECT @b = AllocatedQty, @sb = ShortQty FROM @tr WHERE KitRef = 'ZZKITB';
    IF @a = 4 AND @sa = 0
        PRINT 'ok 9 - the higher priority kit is covered in full with four of the six free pins -- REQ: KIT-007  AC: AC-078';
    ELSE PRINT 'not ok 9 - the first kit was allocated ' + ISNULL(CAST(@a AS varchar(20)),'NULL')
              + ' short ' + ISNULL(CAST(@sa AS varchar(20)),'NULL') + ' -- REQ: KIT-007  AC: AC-078';
    IF @b = 2 AND @sb = 2
        PRINT 'ok 10 - the second kit gets the remaining two pins and is short two -- REQ: KIT-007  AC: AC-078';
    ELSE PRINT 'not ok 10 - the second kit was allocated ' + ISNULL(CAST(@b AS varchar(20)),'NULL')
              + ' short ' + ISNULL(CAST(@sb AS varchar(20)),'NULL') + ' -- REQ: KIT-007  AC: AC-078';
    IF ISNULL(@a,0) + ISNULL(@b,0) = 6
        PRINT 'ok 11 - the six free pins are promised once across both kits, never twice -- REQ: KIT-007  AC: AC-078';
    ELSE PRINT 'not ok 11 - ' + ISNULL(CAST(ISNULL(@a,0) + ISNULL(@b,0) AS varchar(20)),'NULL')
              + ' pins were allocated out of six free -- REQ: KIT-007  AC: AC-078';
    IF EXISTS (SELECT 1 FROM @tr WHERE KitRef = 'ZZKITB' AND ClaimedBefore = 4 AND FreeQty = 6)
        PRINT 'ok 12 - the later kit is answered against the stock the earlier kit has already claimed -- REQ: KIT-007';
    ELSE PRINT 'not ok 12 - the claim made by the earlier kit was not carried into the later one -- REQ: KIT-007';
    IF NOT EXISTS (SELECT 1 FROM @tr WHERE KitRef = 'ZZKITD')
        PRINT 'ok 13 - an unreleased draft kit makes no claim on stock in a trial -- REQ: KIT-006, KIT-024';
    ELSE PRINT 'not ok 13 - a draft kit was given a share of the free stock -- REQ: KIT-006';

    /*  A trial commits nothing: no balance, no reservation and no kit row moved. */
    IF (SELECT SUM(QtyOnHand) FROM inv.StockBalance WHERE CompanyCode = 'STC' AND ItemCode = @pin) = @free
       AND (SELECT SUM(QtyReserved) FROM inv.StockBalance WHERE CompanyCode = 'STC' AND ItemCode = @pin) = @reserved
       AND (SELECT COUNT(*) FROM inv.StockReservation WHERE CompanyCode = 'STC' AND ItemCode = @pin) = @n
       AND NOT EXISTS (SELECT 1 FROM kit.ExecutionKitLine l JOIN kit.ExecutionKit k ON k.ExecutionKitId = l.ExecutionKitId
                        WHERE k.KitRef IN ('ZZKITA','ZZKITB') AND (l.ReservedQty <> 0 OR l.RemainingQty <> 4))
        PRINT 'ok 14 - the trial changed no stock, no reservation and no requirement -- REQ: KIT-006  AC: AC-078';
    ELSE PRINT 'not ok 14 - the trial committed something: stock, a reservation or a requirement moved -- REQ: KIT-006  AC: AC-078';

    /*  Priority is the stated sequence, not an accident of the data: reverse it and the answer
        reverses with it. */
    UPDATE kit.ExecutionKitLine SET NeedDate = '2026-11-20'
      FROM kit.ExecutionKitLine l JOIN kit.ExecutionKit k ON k.ExecutionKitId = l.ExecutionKitId
     WHERE k.CompanyCode = 'STC' AND k.KitRef = 'ZZKITA';
    DELETE @tr;
    INSERT @tr EXEC kit.usp_TrialAllocate @CompanyCode = 'STC', @SiteCode = @site, @ItemCode = @pin;
    SELECT @a = AllocatedQty FROM @tr WHERE KitRef = 'ZZKITA';
    SELECT @b = AllocatedQty FROM @tr WHERE KitRef = 'ZZKITB';
    IF @b = 4 AND @a = 2
        PRINT 'ok 15 - changing the stated priority changes which kit is covered -- REQ: KIT-007  AC: AC-078';
    ELSE PRINT 'not ok 15 - after reversing the priority the first kit still took ' + ISNULL(CAST(@a AS varchar(20)),'NULL') + ' -- REQ: KIT-007';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 9 to 15 - unexpected error in the trial allocation tests: ' + @msg;
END CATCH

/*  16 - with no free stock at all every competing kit is short its whole requirement, and none of
    them is quietly reported as covered.  REQ: KIT-006, KIT-007 */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, QuantumQty, IsIndivisible,
            TraceabilityType, MakeBuy, ValuationGroup, CostMethod, LifecycleStatus, CreatedBy)
    VALUES ('STC', @pin, 'Trial allocation test pin', 'BOUGHT', @uom, 1, 1, 'NONE', 'BUY', 'BOUGHT', 'MWA', 'RELEASED', 'TEST');
    INSERT kit.ExecutionKit (CompanyCode, SiteCode, KitRef, ParentItemCode, PlannedGoodQty, NeedDate, Status, ReleasedBy, CreatedBy)
    VALUES ('STC', @site, 'ZZKITE', @parent, 4, '2026-11-03', 'RELEASED', 'SUPERVISOR', 'TEST');
    INSERT kit.ExecutionKitLine (ExecutionKitId, LineNum, PathRef, ComponentItemCode, UsageBasis,
            OriginalRequirementQty, CurrentRequirementQty, UomCode, RemainingQty, NeedDate, Status)
    SELECT k.ExecutionKitId, 1, '1', @pin, 'PROPORTION', 4, 4, @uom, 4, k.NeedDate, 'OPEN'
      FROM kit.ExecutionKit k WHERE k.CompanyCode = 'STC' AND k.KitRef = 'ZZKITE';
    DELETE @tr;
    INSERT @tr EXEC kit.usp_TrialAllocate @CompanyCode = 'STC', @SiteCode = @site, @ItemCode = @pin;
    IF EXISTS (SELECT 1 FROM @tr WHERE KitRef = 'ZZKITE' AND FreeQty = 0 AND AllocatedQty = 0 AND ShortQty = 4)
        PRINT 'ok 16 - with nothing free the requirement is reported short in full, not covered -- REQ: KIT-006, KIT-007';
    ELSE PRINT 'not ok 16 - a kit with no free stock was not reported short in full -- REQ: KIT-006';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 16 - unexpected error: ' + @msg;
END CATCH
GO
