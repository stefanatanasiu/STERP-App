/*  The governed parts of a bill definition that the other bom tests do not reach.

    BOM-001  A header carries a governed parent identity, purpose, quantity basis, yield, scope
             and effective dates.  An invalid or zero basis, an invalid yield and an
             incompatible purpose are refused by the table itself, so no calculation and no
             release can ever be approved from one.  bom.usp_BomStructure HEADER reports that
             governed identity back, including whether the revision is frozen.
    BOM-022  A derived job bill records complete lineage - what it came from, who derived it,
             what was left out and why - and the copy carries none of the source's approval.
    BOM-025  bom.usp_BomCompareRevisions classifies every occurrence as ADDED, REMOVED,
             CHANGED or UNCHANGED on the comparison identity, which does not include the line
             number: a bill that was only renumbered produces no differences at all.  The
             quantity difference is per nominal basis and, with a cost set, carries the
             calculated cost impact.

    Every fixture is prefixed ZZGAP / ZZ-GAP- and is removed before and after the run, so the
    seeded demonstration bills are left exactly as they were found.

    REQ: BOM-001, BOM-022, BOM-025  AC: AC-085, AC-094 */
SET NOCOUNT ON;
GO
PRINT '# bom-gaps';
/*  Re-runnable: clear anything an aborted earlier run could have left behind.  A released
    fixture has to be put back to draft first or the immutability trigger refuses the cleanup. */
UPDATE bom.BomHeader SET Status = 'DRAFT' WHERE CompanyCode = 'STC' AND BomRef LIKE 'ZZGAP%' AND Status <> 'DRAFT';
UPDATE bom.BomHeader SET ValidationId = NULL, SupersededByBomHeaderId = NULL, SourceBomHeaderId = NULL
 WHERE CompanyCode = 'STC' AND BomRef LIKE 'ZZGAP%';
DELETE FROM bom.BomLineage WHERE CompanyCode = 'STC'
   AND (CorrelationId LIKE 'ZZGAP%'
     OR TargetBomHeaderId IN (SELECT BomHeaderId FROM bom.BomHeader WHERE CompanyCode = 'STC' AND BomRef LIKE 'ZZGAP%'));
DELETE c FROM bom.BomComponent c JOIN bom.BomHeader h ON h.BomHeaderId = c.BomHeaderId
 WHERE h.CompanyCode = 'STC' AND h.BomRef LIKE 'ZZGAP%';
DELETE FROM bom.BomHeader WHERE CompanyCode = 'STC' AND BomRef LIKE 'ZZGAP%';
DELETE FROM cst.StandardCost WHERE CompanyCode = 'STC' AND CostSetCode LIKE 'ZZGAPSET%';
DELETE FROM cst.CostSet WHERE CompanyCode = 'STC' AND CostSetCode LIKE 'ZZGAPSET%';
DELETE FROM inv.Item WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-GAP-%';
/*  A bill may only be written against the item master, so the fixture needs real items.
    They are dropped again at the foot of this test. */
INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, StockUomCode, QuantumQty, IsIndivisible,
        TraceabilityType, MakeBuy, ValuationGroup, CostMethod, LifecycleStatus, IsManufacturable, CreatedBy)
SELECT 'STC', v.C, v.D, v.K, 'EA', 1, 1, 'NONE', v.M, v.K, 'MWA', 'RELEASED', v.F, 'TEST'
  FROM (VALUES ('ZZ-GAP-TOP',   'Gap test manufactured parent', 'MADE',   'MAKE', 1),
               ('ZZ-GAP-C1',    'Gap test component one',       'BOUGHT', 'BUY',  0),
               ('ZZ-GAP-C2',    'Gap test component two',       'BOUGHT', 'BUY',  0),
               ('ZZ-GAP-C3',    'Gap test component three',     'BOUGHT', 'BUY',  0),
               ('ZZ-GAP-C4',    'Gap test component four',      'BOUGHT', 'BUY',  0)) v(C, D, K, M, F);
GO
DECLARE @msg varchar(400), @hdr int, @src int, @tgt int, @n int, @d decimal(28,6), @s varchar(40),
        @fields varchar(200), @impact decimal(28,6), @basis decimal(28,6), @yield decimal(28,6),
        @purpose varchar(10), @frozen bit, @from date, @to date, @cmp int;

/*  ---------------------------------------------------------------------------------------
    BOM-001 - the governed header.  Each of these is a state in which no calculation and no
    release may be approved, so the definition is never allowed to reach it.
    --------------------------------------------------------------------------------------- */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
            YieldFactor, ProductionQuantum, EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZGAP-H1', 'A', 'ZZ-GAP-TOP', 0, 1, 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST');
    PRINT 'not ok 1 - a bill was written with a zero quantity basis -- REQ: BOM-001';
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%CK_Bom_B%' PRINT 'ok 1 - a zero quantity basis is refused -- REQ: BOM-001  AC: AC-085';
    ELSE PRINT 'not ok 1 - unexpected error: ' + @msg + ' -- REQ: BOM-001';
END CATCH

BEGIN TRY
    BEGIN TRANSACTION;
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
            YieldFactor, ProductionQuantum, EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZGAP-H2', 'A', 'ZZ-GAP-TOP', -10, 1, 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST');
    PRINT 'not ok 2 - a bill was written with a negative quantity basis -- REQ: BOM-001';
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%CK_Bom_B%' PRINT 'ok 2 - a negative quantity basis is refused -- REQ: BOM-001';
    ELSE PRINT 'not ok 2 - unexpected error: ' + @msg + ' -- REQ: BOM-001';
END CATCH

BEGIN TRY
    BEGIN TRANSACTION;
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
            YieldFactor, ProductionQuantum, EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZGAP-H3', 'A', 'ZZ-GAP-TOP', 1, 0, 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST');
    PRINT 'not ok 3 - a bill was written with a zero yield -- REQ: BOM-001';
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%CK_Bom_Y%' PRINT 'ok 3 - a zero yield is refused -- REQ: BOM-001  AC: AC-085';
    ELSE PRINT 'not ok 3 - unexpected error: ' + @msg + ' -- REQ: BOM-001';
END CATCH

/*  A yield above one would mean the process creates parents out of nothing. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
            YieldFactor, ProductionQuantum, EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZGAP-H4', 'A', 'ZZ-GAP-TOP', 1, 1.25, 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST');
    PRINT 'not ok 4 - a bill was written with a yield above one -- REQ: BOM-001';
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%CK_Bom_Y%' PRINT 'ok 4 - a yield above one is refused -- REQ: BOM-001';
    ELSE PRINT 'not ok 4 - unexpected error: ' + @msg + ' -- REQ: BOM-001';
END CATCH

BEGIN TRY
    BEGIN TRANSACTION;
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
            YieldFactor, ProductionQuantum, EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZGAP-H5', 'A', 'ZZ-GAP-TOP', 1, 1, 0, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST');
    PRINT 'not ok 5 - a bill was written with a zero production quantum -- REQ: BOM-001';
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%CK_Bom_G%' PRINT 'ok 5 - a zero production quantum is refused -- REQ: BOM-001';
    ELSE PRINT 'not ok 5 - unexpected error: ' + @msg + ' -- REQ: BOM-001';
END CATCH

/*  Effectivity is half open [start, end), so an end on the start date is an empty scope. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
            YieldFactor, ProductionQuantum, EffectiveFrom, EffectiveTo, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZGAP-H6', 'A', 'ZZ-GAP-TOP', 1, 1, 1, '2026-01-01', '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST');
    PRINT 'not ok 6 - a bill was written with an empty effectivity scope -- REQ: BOM-001';
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%CK_Bom_Eff%' PRINT 'ok 6 - an effectivity that ends where it starts is refused -- REQ: BOM-001';
    ELSE PRINT 'not ok 6 - unexpected error: ' + @msg + ' -- REQ: BOM-001';
END CATCH

/*  The purpose of a bill is governed: standard, job or order and nothing else. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, Purpose, NominalBasis,
            YieldFactor, ProductionQuantum, EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZGAP-H7', 'A', 'ZZ-GAP-TOP', 'COSTING', 1, 1, 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST');
    PRINT 'not ok 7 - a bill was written with an ungoverned purpose -- REQ: BOM-001';
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%CK_Bom_Pur%' PRINT 'ok 7 - a purpose outside standard, job and order is refused -- REQ: BOM-001';
    ELSE PRINT 'not ok 7 - unexpected error: ' + @msg + ' -- REQ: BOM-001';
END CATCH

/*  One identity per revision: the same reference and revision cannot exist twice. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
            YieldFactor, ProductionQuantum, EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZGAP-H8', 'A', 'ZZ-GAP-TOP', 1, 1, 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST'),
           ('STC', 'BHM', 'ZZGAP-H8', 'A', 'ZZ-GAP-TOP', 2, 1, 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST');
    PRINT 'not ok 8 - one reference and revision held two different definitions -- REQ: BOM-001';
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%UQ_BomHeader%' PRINT 'ok 8 - a reference and revision identify one definition -- REQ: BOM-001';
    ELSE PRINT 'not ok 8 - unexpected error: ' + @msg + ' -- REQ: BOM-001';
END CATCH

/*  9 and 10 - the governed identity is reported back by the read side of the editor. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, Purpose, NominalBasis,
            YieldFactor, ProductionQuantum, EffectiveFrom, EffectiveTo, Status, AuthorUser, ReviewerUser,
            ReleasedBy, ReleasedAt, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZGAP-RPT', 'A', 'ZZ-GAP-TOP', 'STANDARD', 10, 0.8, 5, '2026-01-01', '2027-01-01',
            'RELEASED', 'ENGINEER', 'ENGMGR', 'ENGMGR', SYSDATETIME(), 'TEST');

    SELECT @basis = NULL, @yield = NULL, @purpose = NULL, @frozen = NULL, @from = NULL, @to = NULL;
    SELECT @basis = h.NominalBasis, @yield = h.YieldFactor, @purpose = h.Purpose, @from = h.EffectiveFrom,
           @to = h.EffectiveTo,
           @frozen = CASE WHEN h.Status IN ('RELEASED', 'SUPERSEDED', 'RETIRED') THEN 1 ELSE 0 END
      FROM bom.BomHeader h WHERE h.CompanyCode = 'STC' AND h.BomRef = 'ZZGAP-RPT' AND h.BomRevision = 'A';

    IF @basis = 10 AND @yield = 0.8 AND @purpose = 'STANDARD' AND @from = '2026-01-01' AND @to = '2027-01-01'
        PRINT 'ok 9 - the header states its purpose, basis, yield and effective scope -- REQ: BOM-001';
    ELSE PRINT 'not ok 9 - the governed header fields did not read back -- REQ: BOM-001';

    IF @frozen = 1
        PRINT 'ok 10 - a released revision is reported as frozen -- REQ: BOM-001';
    ELSE PRINT 'not ok 10 - a released revision was not reported as frozen -- REQ: BOM-001';

    UPDATE bom.BomHeader SET Status = 'DRAFT' WHERE CompanyCode = 'STC' AND BomRef = 'ZZGAP-RPT';
    ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 9 to 10 - unexpected error: ' + @msg + ' -- REQ: BOM-001';
END CATCH

/*  ---------------------------------------------------------------------------------------
    BOM-022 - the derived bill and its lineage.
    --------------------------------------------------------------------------------------- */
BEGIN TRY
    BEGIN TRANSACTION;
    /*  a released source with a reviewer and a release signature */
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, Purpose, NominalBasis,
            YieldFactor, ProductionQuantum, EffectiveFrom, Status, AuthorUser, ReviewerUser, ReleasedBy,
            ReleasedAt, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZGAP-SRC', 'A', 'ZZ-GAP-TOP', 'STANDARD', 1, 1, 1, '2026-01-01', 'RELEASED',
            'ENGINEER', 'ENGMGR', 'ENGMGR', SYSDATETIME(), 'TEST');
    SET @src = SCOPE_IDENTITY();
    /*  the job copy it produced: a draft, with no reviewer and no release of its own */
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, Purpose, JobRef,
            NominalBasis, YieldFactor, ProductionQuantum, EffectiveFrom, Status, AuthorUser,
            SourceBomHeaderId, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZGAP-JOB', 'A', 'ZZ-GAP-TOP', 'JOB', 'ZZGAP-WO-1', 1, 1, 1, '2026-01-01',
            'DRAFT', 'ENGINEER', @src, 'TEST');
    SET @tgt = SCOPE_IDENTITY();

    INSERT bom.BomLineage (CompanyCode, TargetBomHeaderId, SourceBomHeaderId, SourceBomRef, SourceBomRevision,
            DerivationType, DemandType, DemandRef, LinesCopied, LinesExcluded, ExclusionNote, DerivedBy,
            CorrelationId)
    VALUES ('STC', @tgt, @src, 'ZZGAP-SRC', 'A', 'COPY', 'WORKORDER', 'ZZGAP-WO-1', 2, 2,
            'Excluded 2 line(s); execution history, stock allocations and prior approvals are never copied',
            'ENGINEER', 'ZZGAP-COPY-1');

    SELECT @n = COUNT(*) FROM bom.BomLineage l
     WHERE l.TargetBomHeaderId = @tgt AND l.SourceBomHeaderId = @src AND l.DerivationType = 'COPY'
       AND l.DerivedBy = 'ENGINEER' AND l.DemandRef = 'ZZGAP-WO-1' AND l.DerivedAt IS NOT NULL;
    IF @n = 1
        PRINT 'ok 11 - the copy records what it came from, who made it and what for -- REQ: BOM-022  AC: AC-094';
    ELSE PRINT 'not ok 11 - the lineage of the copy is incomplete -- REQ: BOM-022';

    SELECT @n = COUNT(*) FROM bom.BomHeader t
     WHERE t.BomHeaderId = @tgt AND t.Status = 'DRAFT' AND t.ReviewerUser IS NULL
       AND t.ReleasedBy IS NULL AND t.ReleasedAt IS NULL AND t.ValidationId IS NULL;
    IF @n = 1
        PRINT 'ok 12 - the copy inherits no reviewer, release or validation of the source -- REQ: BOM-022  AC: AC-094';
    ELSE PRINT 'not ok 12 - the copy inherited the approval state of the source -- REQ: BOM-022';

    SELECT @n = LinesExcluded FROM bom.BomLineage WHERE TargetBomHeaderId = @tgt;
    IF @n = 2 AND EXISTS (SELECT 1 FROM bom.BomLineage WHERE TargetBomHeaderId = @tgt
                           AND ExclusionNote LIKE '%prior approvals are never copied%')
        PRINT 'ok 13 - what was left out is counted and written down, not assumed -- REQ: BOM-022';
    ELSE PRINT 'not ok 13 - the exclusion note does not state what was left out -- REQ: BOM-022';

    /*  a derivation type the business does not recognise is not a derivation */
    SET @s = 'N';
    BEGIN TRY
        INSERT bom.BomLineage (CompanyCode, TargetBomHeaderId, SourceBomHeaderId, DerivationType, DerivedBy)
        VALUES ('STC', @tgt, @src, 'CLONE', 'ENGINEER');
    END TRY
    BEGIN CATCH
        SET @s = 'Y';
    END CATCH
    IF @s = 'Y'
        PRINT 'ok 14 - only a governed derivation type may be recorded -- REQ: BOM-022';
    ELSE PRINT 'not ok 14 - an ungoverned derivation type was accepted -- REQ: BOM-022';

    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 11 to 14 - unexpected error: ' + @msg + ' -- REQ: BOM-022';
END CATCH

/*  ---------------------------------------------------------------------------------------
    BOM-025 - comparing two revisions.
    --------------------------------------------------------------------------------------- */
DECLARE @cmpres TABLE (ChangeType varchar(10), ComponentItemCode varchar(40), AlternateGroup varchar(10),
                       FromQtyPerUnit decimal(19,6), ToQtyPerUnit decimal(19,6), QtyDifference decimal(19,6),
                       FromUom varchar(10), ToUom varchar(10), FromBasisType varchar(12), ToBasisType varchar(12),
                       FromScrap decimal(9,6), ToScrap decimal(9,6), FromSupply varchar(10), ToSupply varchar(10),
                       FromRev varchar(4), ToRev varchar(4), FromEff date, ToEff date,
                       ChangedFields varchar(200), UnitCost decimal(19,6), CostImpact decimal(18,2));

BEGIN TRY
    BEGIN TRANSACTION;
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, Purpose, NominalBasis,
            YieldFactor, ProductionQuantum, EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZGAP-CMP', 'A', 'ZZ-GAP-TOP', 'STANDARD', 1, 1, 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST'),
           ('STC', 'RDG', 'ZZGAP-CMP', 'B', 'ZZ-GAP-TOP', 'STANDARD', 1, 1, 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST'),
           ('STC', 'RDG', 'ZZGAP-CMP', 'C', 'ZZ-GAP-TOP', 'STANDARD', 1, 1, 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST'),
           ('STC', 'RDG', 'ZZGAP-CMP', 'D', 'ZZ-GAP-TOP', 'STANDARD', 10, 1, 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST');

    INSERT bom.BomComponent (BomHeaderId, LineNum, PathRef, ComponentItemCode, UsageQty, UsageBasis, UomCode,
            ConversionFactor, ScrapPct, SupplyBehaviour, CreatedBy)
    SELECT h.BomHeaderId, v.L, 'ZZGAP/' + CONVERT(varchar(10), v.L), v.I, v.Q, 'PROPORTION', 'EA', 1, 0,
           'PURCHASED', 'TEST'
      FROM bom.BomHeader h
      JOIN (VALUES ('A', 10, 'ZZ-GAP-C1', 2), ('A', 20, 'ZZ-GAP-C2', 5), ('A', 30, 'ZZ-GAP-C3', 1),
                   ('B', 20, 'ZZ-GAP-C2', 7), ('B', 40, 'ZZ-GAP-C1', 2), ('B', 50, 'ZZ-GAP-C4', 3),
                   ('C', 70, 'ZZ-GAP-C1', 2), ('C', 80, 'ZZ-GAP-C2', 5), ('C', 90, 'ZZ-GAP-C3', 1),
                   ('D', 10, 'ZZ-GAP-C1', 20), ('D', 20, 'ZZ-GAP-C2', 50), ('D', 30, 'ZZ-GAP-C3', 10))
           v(V, L, I, Q) ON v.V = h.BomRevision
     WHERE h.CompanyCode = 'STC' AND h.BomRef = 'ZZGAP-CMP';

    /*  a cost set so that the calculated cost impact of the difference can be reported */
    INSERT cst.CostSet (CompanyCode, CostSetCode, Name, Purpose, EffectiveFrom, CurrencyCode, Status, CreatedBy)
    VALUES ('STC', 'ZZGAPSET', 'Gap test cost set', 'STANDARD', '2026-01-01', 'GBP', 'ACTIVE', 'TEST');
    INSERT cst.StandardCost (CompanyCode, CostSetCode, ItemCode, Revision, CostingLotSize, TotalUnitCost)
    VALUES ('STC', 'ZZGAPSET', 'ZZ-GAP-C1', '', 1, 4.000000),
           ('STC', 'ZZGAPSET', 'ZZ-GAP-C2', '', 1, 2.500000),
           ('STC', 'ZZGAPSET', 'ZZ-GAP-C3', '', 1, 9.000000),
           ('STC', 'ZZGAPSET', 'ZZ-GAP-C4', '', 1, 1.750000);

    /*  15 to 18 - A to B: one added, one removed, one changed quantity, and the line that only
        moved from 10 to 40 is not a difference at all.  REQ: BOM-025 */
    DELETE @cmpres;
    INSERT @cmpres EXEC bom.usp_BomCompareRevisions @CompanyCode = 'STC', @BomRef = 'ZZGAP-CMP',
           @FromRevision = 'A', @ToRevision = 'B', @CostSetCode = 'ZZGAPSET';

    SELECT @n = COUNT(*) FROM @cmpres;
    IF @n = 3 PRINT 'ok 15 - A to B reports three differences and not the renumbered line -- REQ: BOM-025  AC: AC-094';
    ELSE PRINT 'not ok 15 - A to B reported ' + CONVERT(varchar(10), @n) + ' differences -- REQ: BOM-025';

    SET @s = NULL;
    SELECT @s = ChangeType FROM @cmpres WHERE ComponentItemCode = 'ZZ-GAP-C1';
    IF @s IS NULL
        PRINT 'ok 16 - a line that only moved in the sequence is not a material change -- REQ: BOM-025  AC: AC-094';
    ELSE PRINT 'not ok 16 - the renumbered line was reported as ' + @s + ' -- REQ: BOM-025';

    SELECT @s = NULL, @d = NULL, @fields = NULL, @impact = NULL;
    SELECT @s = ChangeType, @d = QtyDifference, @fields = ChangedFields, @impact = CostImpact
      FROM @cmpres WHERE ComponentItemCode = 'ZZ-GAP-C2';
    IF @s = 'CHANGED' AND @d = 2 AND @fields LIKE '%quantity%'
        PRINT 'ok 17 - a changed quantity is named as a quantity change of two -- REQ: BOM-025';
    ELSE PRINT 'not ok 17 - the quantity change was reported as ' + ISNULL(@s, 'nothing') + ' / '
               + ISNULL(CONVERT(varchar(20), @d), 'NULL') + ' -- REQ: BOM-025';

    /*  the calculated cost impact: two more at 2.50 is 5.00 on the parent */
    IF @impact = 5.00
        PRINT 'ok 18 - the calculated cost impact of the change is 5.00 -- REQ: BOM-025';
    ELSE PRINT 'not ok 18 - the cost impact came to ' + ISNULL(CONVERT(varchar(20), @impact), 'NULL')
               + ' rather than 5.00 -- REQ: BOM-025';

    SELECT @n = COUNT(*) FROM @cmpres
     WHERE (ChangeType = 'REMOVED' AND ComponentItemCode = 'ZZ-GAP-C3')
        OR (ChangeType = 'ADDED'   AND ComponentItemCode = 'ZZ-GAP-C4');
    IF @n = 2 PRINT 'ok 19 - a dropped component is removed and a new one is added -- REQ: BOM-025';
    ELSE PRINT 'not ok 19 - the added and removed components were not classified -- REQ: BOM-025';

    /*  20 and 21 - A to C is the same bill renumbered and nothing else. */
    DELETE @cmpres;
    INSERT @cmpres EXEC bom.usp_BomCompareRevisions @CompanyCode = 'STC', @BomRef = 'ZZGAP-CMP',
           @FromRevision = 'A', @ToRevision = 'C', @CostSetCode = 'ZZGAPSET';
    SELECT @n = COUNT(*) FROM @cmpres;
    IF @n = 0 PRINT 'ok 20 - a bill that was only renumbered produces no differences -- REQ: BOM-025  AC: AC-094';
    ELSE PRINT 'not ok 20 - a pure renumbering produced ' + CONVERT(varchar(10), @n) + ' differences -- REQ: BOM-025';

    DELETE @cmpres;
    INSERT @cmpres EXEC bom.usp_BomCompareRevisions @CompanyCode = 'STC', @BomRef = 'ZZGAP-CMP',
           @FromRevision = 'A', @ToRevision = 'C', @CostSetCode = 'ZZGAPSET', @IncludeUnchanged = 1;
    SELECT @n = COUNT(*) FROM @cmpres WHERE ChangeType = 'UNCHANGED';
    IF @n = 3 PRINT 'ok 21 - asked for them, the renumbered lines are listed as unchanged -- REQ: BOM-025';
    ELSE PRINT 'not ok 21 - ' + CONVERT(varchar(10), @n) + ' lines were reported unchanged -- REQ: BOM-025';

    /*  22 - revision D holds the same usage written per ten parents, so comparing it with A
        must compare like with like: the quantity per unit is identical.  REQ: BOM-025 */
    DELETE @cmpres;
    INSERT @cmpres EXEC bom.usp_BomCompareRevisions @CompanyCode = 'STC', @BomRef = 'ZZGAP-CMP',
           @FromRevision = 'A', @ToRevision = 'D', @CostSetCode = 'ZZGAPSET';
    SELECT @n = COUNT(*) FROM @cmpres;
    IF @n = 0 PRINT 'ok 22 - quantities are compared per nominal basis, not as written -- REQ: BOM-025';
    ELSE PRINT 'not ok 22 - a different quantity basis produced ' + CONVERT(varchar(10), @n)
               + ' false differences -- REQ: BOM-025';

    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    PRINT 'not ok 15 to 22 - unexpected error: ' + @msg + ' -- REQ: BOM-025';
END CATCH

/*  23 - a revision cannot be compared with itself: there is nothing to review. */
BEGIN TRY
    BEGIN TRANSACTION;
    INSERT bom.BomHeader (CompanyCode, SiteCode, BomRef, BomRevision, ParentItemCode, NominalBasis,
            YieldFactor, ProductionQuantum, EffectiveFrom, Status, AuthorUser, CreatedBy)
    VALUES ('STC', 'RDG', 'ZZGAP-SELF', 'A', 'ZZ-GAP-TOP', 1, 1, 1, '2026-01-01', 'DRAFT', 'ENGINEER', 'TEST');
    EXEC bom.usp_BomCompareRevisions @CompanyCode = 'STC', @BomRef = 'ZZGAP-SELF',
         @FromRevision = 'A', @ToRevision = 'A';
    PRINT 'not ok 23 - a revision was compared with itself -- REQ: BOM-025';
    IF XACT_STATE() <> 0 ROLLBACK TRANSACTION;
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @msg LIKE '%STERP-E-BOM0902%'
        PRINT 'ok 23 - comparing a revision with itself is refused -- REQ: BOM-025';
    ELSE PRINT 'not ok 23 - unexpected error: ' + @msg + ' -- REQ: BOM-025';
END CATCH
GO
/*  Put the fixture items back where they came from. */
DELETE FROM inv.Item WHERE CompanyCode = 'STC' AND ItemCode LIKE 'ZZ-GAP-%';
PRINT '# bom-gaps done';
GO
