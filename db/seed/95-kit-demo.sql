/* ---------------------------------------------------------------------------
   95-kit-demo - demonstration kits for both companies, in the states a stores
   office actually has on its desk at any moment: a draft that is still being
   argued about, a released kit waiting for stock, one part issued, one part
   completed and one closed.  Also one commercial sales kit definition so the
   KITCOMM allocation can be demonstrated.

   The data is written so the screens and RPT-013 show something meaningful
   without running a batch first.  Quantities obey the rules the engine
   enforces, so a screen refresh does not immediately contradict the seed:
       net issued = gross issued - returned
       remaining  = max(0, current requirement - net issued)
       excess     = max(0, net issued - current requirement)
   Scrapped quantity is part of gross issued and is NOT deducted from it.

   Re-runnable: every insert is guarded by NOT EXISTS on the natural key.

   REQ: KIT-001, KIT-002, KIT-010, KIT-013, KIT-014, KIT-015, KIT-017,
        KIT-022, KIT-023
   --------------------------------------------------------------------------- */
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'kit.ExecutionKit') IS NULL OR OBJECT_ID(N'inv.Item') IS NULL
BEGIN
    PRINT 'Kit demo skipped - kit or inventory tables are not deployed';
    RETURN;
END
GO
DECLARE @kits table (CompanyCode varchar(10), SiteCode varchar(10), KitRef varchar(30),
                     Parent varchar(10), Status varchar(14), Planned decimal(19,6),
                     Completed decimal(19,6), NeedDays int, Wip decimal(18,2),
                     Owner varchar(20), Backflush bit);

/*  The demo does not hard code item codes: it binds logical slots (P1, P2 for
    the made parents, C1..C3 for the components) to whatever the inventory
    seed actually loaded for each company.  A demo that depends on one item
    code existing is a demo that breaks the first time the item list changes. */
DECLARE @map table (CompanyCode varchar(10), Slot varchar(10), ItemCode varchar(40),
                    UomCode varchar(10));
INSERT @map (CompanyCode, Slot, ItemCode, UomCode)
SELECT c.CompanyCode, CONCAT('P', p.rn), p.ItemCode, p.StockUomCode
  FROM core.Company c
 CROSS APPLY (SELECT TOP 2 i.ItemCode, i.StockUomCode,
                     rn = ROW_NUMBER() OVER (ORDER BY i.ItemCode)
                FROM inv.Item i
               WHERE i.CompanyCode = c.CompanyCode AND i.ItemClass = 'MADE') p;
INSERT @map (CompanyCode, Slot, ItemCode, UomCode)
SELECT c.CompanyCode, CONCAT('C', b.rn), b.ItemCode, b.StockUomCode
  FROM core.Company c
 CROSS APPLY (SELECT TOP 3 i.ItemCode, i.StockUomCode,
                     rn = ROW_NUMBER() OVER (ORDER BY i.ItemCode)
                FROM inv.Item i
               WHERE i.CompanyCode = c.CompanyCode AND i.ItemClass = 'BOUGHT') b;

/*  Both companies get the same shape of work so a demo can switch company and
    still have something to look at.                                          */
INSERT @kits VALUES
 ('STC','RDG','KIT0001001','P1','PARTCOMPLETE', 10, 4, 30, 1842.50,'PLANNER',0),
 ('STC','RDG','KIT0001002','P1','RELEASED',     25, 0, 45,    0.00,'PLANNER',0),
 ('STC','RDG','KIT0001003','P2','PARTISSUED',    8, 0, 20,  640.00,'PLANNER',1),
 ('STC','RDG','KIT0001004','P2','DRAFT',        12, 0, 60,    0.00,'ENGINEER',0),
 ('STC','RDG','KIT0001005','P1','FINCLOSED',     6, 6,-15,    0.00,'PLANNER',0),
 ('BRS','BRI','KIT0002001','P1','RELEASED',     15, 0, 35,    0.00,'PLANNER',0),
 ('BRS','BRI','KIT0002002','P2','PARTISSUED',    5, 0, 25,  410.00,'PLANNER',0);

INSERT kit.ExecutionKit (CompanyCode, SiteCode, KitRef, ParentItemCode, SourceDemandType,
       SourceDemandRef, PlannedGoodQty, CompletedGoodQty, RejectedQty, GrossStarts,
       YieldFactor, RunCount, NeedDate, PlannedStartDate, PlannedCompletionDate,
       SelectionDate, OwnerUser, Status, ReleasedBy, WipValueGBP, IsBackflush, CreatedBy)
SELECT k.CompanyCode, k.SiteCode, k.KitRef, m.ItemCode, 'STOCK', k.KitRef,
       k.Planned, k.Completed, 0, k.Planned, 1, 1,
       DATEADD(day, k.NeedDays, CONVERT(date,'2026-10-06')),
       DATEADD(day, k.NeedDays - 10, CONVERT(date,'2026-10-06')),
       DATEADD(day, k.NeedDays, CONVERT(date,'2026-10-06')),
       CASE WHEN k.Status = 'DRAFT' THEN NULL ELSE CONVERT(date,'2026-10-06') END,
       k.Owner, k.Status,
       CASE WHEN k.Status = 'DRAFT' THEN NULL ELSE 'SUPERVISOR' END,
       k.Wip, k.Backflush, 'SEED'
  FROM @kits k
  JOIN @map m ON m.CompanyCode = k.CompanyCode AND m.Slot = k.Parent
 WHERE NOT EXISTS (SELECT 1 FROM kit.ExecutionKit e WHERE e.CompanyCode = k.CompanyCode
                     AND e.KitRef = k.KitRef);

/*  Material lines.  The derived quantities are written consistently with the
    rules the engine enforces, so a screen refresh cannot contradict the seed. */
DECLARE @lines table (KitRef varchar(30), LineNum int, Component varchar(10),
                      Required decimal(19,6), Reserved decimal(19,6),
                      Picked decimal(19,6), Gross decimal(19,6),
                      Returned decimal(19,6), Scrapped decimal(19,6),
                      PlanCost decimal(19,6), Basis varchar(12));
INSERT @lines VALUES
 ('KIT0001001',1,'C1',100, 100, 100, 100,  0,  0, 0.450000,'PROPORTION'),
 ('KIT0001001',2,'C2', 20,  20,  20,  20,  4,  0, 3.250000,'PROPORTION'),
 ('KIT0001001',3,'C3',  1,   1,   1,   1,  0,  1,25.000000,'FIXEDRUN'),
 ('KIT0001002',1,'C1',250,  60,   0,   0,  0,  0, 0.450000,'PROPORTION'),
 ('KIT0001002',2,'C2', 50,  50,   0,   0,  0,  0, 3.250000,'PROPORTION'),
 ('KIT0001002',3,'C3',  1,   1,   0,   0,  0,  0,25.000000,'FIXEDRUN'),
 ('KIT0001003',1,'C1', 32,  32,  32,  24,  0,  0, 0.450000,'PROPORTION'),
 ('KIT0001003',2,'C2', 16,  16,  16,  16,  2,  0, 3.250000,'PROPORTION'),
 ('KIT0001004',1,'C1',120,   0,   0,   0,  0,  0, 0.450000,'PROPORTION'),
 ('KIT0001004',2,'C2', 24,   0,   0,   0,  0,  0, 3.250000,'PROPORTION'),
 ('KIT0001005',1,'C1', 60,   0,  60,  60,  0,  0, 0.450000,'PROPORTION'),
 ('KIT0001005',2,'C2', 12,   0,  12,  12,  0,  0, 3.250000,'PROPORTION'),
 ('KIT0002001',1,'C1',150,  40,   0,   0,  0,  0, 0.450000,'PROPORTION'),
 ('KIT0002001',2,'C2', 30,  30,   0,   0,  0,  0, 3.250000,'PROPORTION'),
 ('KIT0002002',1,'C1', 20,  20,  20,  20,  0,  2, 0.450000,'PROPORTION'),
 ('KIT0002002',2,'C2', 10,  10,  10,   6,  0,  0, 3.250000,'PROPORTION');

INSERT kit.ExecutionKitLine (ExecutionKitId, LineNum, PathRef, ComponentItemCode, UsageBasis,
       OriginalRequirementQty, CurrentRequirementQty, UomCode, ConversionFactor, ScrapPct,
       ReservedQty, PickedQty, GrossIssuedQty, ReturnedQty, ScrappedQty, NetIssuedQty,
       RemainingQty, ExcessQty, NeedDate, PlannedUnitCostGBP, IssuedValueGBP, Status)
SELECT k.ExecutionKitId, l.LineNum, CONVERT(varchar(20), l.LineNum), m.ItemCode, l.Basis,
       l.Required, l.Required, ISNULL(m.UomCode,'EA'), 1, 0,
       l.Reserved, l.Picked, l.Gross, l.Returned, l.Scrapped,
       l.Gross - l.Returned,
       CASE WHEN l.Required - (l.Gross - l.Returned) > 0
            THEN l.Required - (l.Gross - l.Returned) ELSE 0 END,
       CASE WHEN (l.Gross - l.Returned) - l.Required > 0
            THEN (l.Gross - l.Returned) - l.Required ELSE 0 END,
       k.NeedDate, l.PlanCost,
       CONVERT(decimal(18,2), (l.Gross - l.Returned) * l.PlanCost),
       CASE WHEN l.Gross = 0 THEN 'OPEN'
            WHEN l.Gross - l.Returned >= l.Required THEN 'ISSUED'
            ELSE 'PARTISSUED' END
  FROM @lines l
  JOIN kit.ExecutionKit k ON k.KitRef = l.KitRef
  JOIN @map m ON m.CompanyCode = k.CompanyCode AND m.Slot = l.Component
 WHERE NOT EXISTS (SELECT 1 FROM kit.ExecutionKitLine e
                    WHERE e.ExecutionKitId = k.ExecutionKitId AND e.LineNum = l.LineNum);
GO
/*  The movements behind the quantities above, so KITCOST and the execution
    listing have something to add up.  Scrap is written as its own movement
    type: it is a disposition of issued material, never a reversal.           */
INSERT kit.KitIssue (CompanyCode, KitRef, KitLineId, LineNum, ComponentItemCode, IssueType,
       Quantity, UomCode, StockQty, ActualDate, PostingDate, OperatorUser,
       UnitCostGBP, ValueGBP, ReasonCode, Reason, CreatedBy)
SELECT k.CompanyCode, k.KitRef, l.KitLineId, l.LineNum, l.ComponentItemCode, m.IssueType,
       m.Qty, l.UomCode, m.Qty, CONVERT(date,'2026-10-01'), CONVERT(date,'2026-10-01'),
       'STORES', l.PlannedUnitCostGBP,
       CONVERT(decimal(18,2), m.Qty * ISNULL(l.PlannedUnitCostGBP,0)),
       m.ReasonCode, m.Reason, 'SEED'
  FROM kit.ExecutionKitLine l
  JOIN kit.ExecutionKit k ON k.ExecutionKitId = l.ExecutionKitId
 CROSS APPLY (VALUES
       ('ISSUE',  l.GrossIssuedQty, NULL,       'Issued to the job from the seed'),
       ('RETURN', l.ReturnedQty,    'UNUSED',   'Unused material returned to store'),
       ('SCRAP',  l.ScrappedQty,    'DAMAGED',  'Scrapped on the job - not returned to store')
      ) AS m(IssueType, Qty, ReasonCode, Reason)
 WHERE m.Qty > 0
   AND k.KitRef LIKE 'KIT000%'
   AND NOT EXISTS (SELECT 1 FROM kit.KitIssue z WHERE z.KitRef = k.KitRef
                     AND z.LineNum = l.LineNum AND z.IssueType = m.IssueType);
GO
/*  The completion of KIT0001001 and its genealogy, so the genealogy enquiry
    and the recall trace have a parent to walk from.  One component was
    substituted, which the trace must show against the PLANNED part.          */
INSERT kit.KitCompletion (CompanyCode, KitRef, CompletionSeq, ParentItemCode, GoodQty,
       RejectedQty, LotCode, CompletionDate, PostingDate, UnitCostGBP, WipTransferGBP,
       IsFinalCompletion, ApprovedBy, CreatedBy)
SELECT k.CompanyCode, k.KitRef, 1, k.ParentItemCode, 4, 0, 'LOT-FRM-2609',
       CONVERT(date,'2026-10-02'), CONVERT(date,'2026-10-02'), 68.250000, 273.00,
       0, 'SUPERVISOR', 'SEED'
  FROM kit.ExecutionKit k
 WHERE k.KitRef = 'KIT0001001'
   AND NOT EXISTS (SELECT 1 FROM kit.KitCompletion z WHERE z.KitRef = k.KitRef);
GO
INSERT kit.KitGenealogy (CompanyCode, KitRef, CompletionId, ParentItemCode, ParentLotCode,
       ComponentItemCode, ComponentLotCode, ConsumedQty, WasSubstitute, PlannedItemCode)
SELECT c.CompanyCode, c.KitRef, c.CompletionId, c.ParentItemCode, c.LotCode,
       l.ComponentItemCode,
       CONCAT('LOT-', LEFT(REPLACE(l.ComponentItemCode,'-',''), 6), '-26'),
       l.GrossIssuedQty - l.ReturnedQty - l.ScrappedQty,
       CONVERT(bit,0), l.ComponentItemCode
  FROM kit.KitCompletion c
  JOIN kit.ExecutionKit k ON k.CompanyCode = c.CompanyCode AND k.KitRef = c.KitRef
  JOIN kit.ExecutionKitLine l ON l.ExecutionKitId = k.ExecutionKitId
 WHERE c.KitRef = 'KIT0001001'
   AND l.GrossIssuedQty - l.ReturnedQty - l.ScrappedQty > 0
   AND NOT EXISTS (SELECT 1 FROM kit.KitGenealogy z WHERE z.KitRef = c.KitRef
                     AND z.ComponentItemCode = l.ComponentItemCode);
GO
/*  One substituted line, so the trace has a case where the part FITTED is not
    the part PLANNED.  A recall enquiry must be able to tell the two apart,
    which is why PlannedItemCode is carried on every row (KIT-012, KIT-019).  */
INSERT kit.KitGenealogy (CompanyCode, KitRef, CompletionId, ParentItemCode, ParentLotCode,
       ComponentItemCode, ComponentLotCode, ConsumedQty, WasSubstitute, PlannedItemCode)
SELECT TOP 1 c.CompanyCode, c.KitRef, c.CompletionId, c.ParentItemCode, c.LotCode,
       s.ItemCode, 'LOT-SUB-2610', 2.000000, CONVERT(bit,1), l.ComponentItemCode
  FROM kit.KitCompletion c
  JOIN kit.ExecutionKit k ON k.CompanyCode = c.CompanyCode AND k.KitRef = c.KitRef
  JOIN kit.ExecutionKitLine l ON l.ExecutionKitId = k.ExecutionKitId AND l.LineNum = 2
 CROSS APPLY (SELECT TOP 1 i.ItemCode FROM inv.Item i
               WHERE i.CompanyCode = c.CompanyCode AND i.ItemClass = 'BOUGHT'
                 AND i.ItemCode <> l.ComponentItemCode
               ORDER BY i.ItemCode DESC) s
 WHERE c.KitRef = 'KIT0001001'
   AND NOT EXISTS (SELECT 1 FROM kit.KitGenealogy z WHERE z.KitRef = c.KitRef
                     AND z.WasSubstitute = 1);
GO
/*  One commercial sales kit so KITCOMM has a definition to allocate against.
    The standalone selling prices are deliberately uneven so the allocation
    produces a rounding residual, which must land on the largest line.        */
IF OBJECT_ID(N'kit.KitDefinition') IS NOT NULL
BEGIN
    /*  The parent must exist as an item of class KIT: a commercial kit parent
        is a commercial identity, so it is an item record with no stock, never
        an invented code (KIT-001).  If the inventory seed has no KIT class
        item the demo definition is simply skipped.                          */
    INSERT kit.KitDefinition (CompanyCode, KitItemCode, KitVersion, Name, Description,
           SupplyKind, AllowPartialDespatch, EffectiveFrom, Status, ApprovedBy, CreatedBy)
    SELECT c.CompanyCode, p.ItemCode, 'A', 'Starter fixing kit',
           'Commercial kit - the parent is a commercial identity and is never stocked',
           'SINGLE', 0, CONVERT(date,'2026-01-01'), 'RELEASED', 'SALESMGR', 'SEED'
      FROM core.Company c
     CROSS APPLY (SELECT TOP 1 i.ItemCode FROM inv.Item i
                   WHERE i.CompanyCode = c.CompanyCode AND i.ItemClass = 'KIT'
                   ORDER BY i.ItemCode) p
     WHERE NOT EXISTS (SELECT 1 FROM kit.KitDefinition d WHERE d.CompanyCode = c.CompanyCode
                         AND d.KitItemCode = p.ItemCode AND d.KitVersion = 'A');

    INSERT kit.KitDefinitionLine (KitDefId, LineNum, ComponentItemCode, Quantity, UomCode,
           ConversionFactor, StandaloneSellingPrice, VatCode, IsCustomerSpecified,
           SubstitutionAllowed, IsOptional)
    SELECT d.KitDefId, b.rn, b.ItemCode, l.Qty, ISNULL(b.StockUomCode,'EA'), 1,
           l.Ssp, 'S', 0, l.SubOk, 0
      FROM kit.KitDefinition d
     CROSS APPLY (SELECT TOP 3 i.ItemCode, i.StockUomCode,
                         rn = ROW_NUMBER() OVER (ORDER BY i.ItemCode)
                    FROM inv.Item i
                   WHERE i.CompanyCode = d.CompanyCode AND i.ItemClass = 'BOUGHT') b
      JOIN (VALUES
           (1, 10.000000,  1.250000, CONVERT(bit,1)),
           (2,  4.000000,  7.500000, CONVERT(bit,1)),
           (3,  1.000000, 33.333333, CONVERT(bit,0))
          ) AS l(LineNum, Qty, Ssp, SubOk) ON l.LineNum = b.rn
     WHERE d.KitVersion = 'A' AND d.CreatedBy = 'SEED'
       AND NOT EXISTS (SELECT 1 FROM kit.KitDefinitionLine z WHERE z.KitDefId = d.KitDefId
                         AND z.LineNum = b.rn);

    PRINT 'Seeded kit.KitDefinition (commercial starter kit)';
END
GO
/*  KITCOST has nothing to add up unless the material the demonstration kit issues carries a cost.
    The driver picks the bill with the most components that have stock, which is the B4 family, and
    neither the B4 parents nor the shared child had a cost row - so every issue was valued at zero
    and the cost enquiry answered zeros for a kit that had really consumed material.

    A cost row values a movement; it posts nothing itself, so the control account proof (RECONCTL)
    is untouched by this seed.  The rates roll up: steel 2.10/kg at 0.5 kg a unit plus the two
    processing aids over a batch of ten (0.3 L at 5.00 and 0.2 L at 4.00) give the shared child
    1.28, and the top level takes twelve of them (two independent occurrences of six).
    REQ: KIT-020, CST-002                                                                        */
IF OBJECT_ID(N'cst.CostSet') IS NOT NULL AND OBJECT_ID(N'cst.StandardCost') IS NOT NULL
   AND OBJECT_ID(N'cst.ItemCost') IS NOT NULL
BEGIN
    MERGE cst.CostSet AS t
    USING (VALUES ('STC','KITDEMO','Kit demonstration cost set','STANDARD','2026-01-01','GBP','ACTIVE',
                   'COSTACC','COSTMGR','FINCTRL','2025-12-18T16:40:00','2026-01-01T00:00:00'))
          AS s (CompanyCode, CostSetCode, Name, Purpose, EffectiveFrom, CurrencyCode, Status,
                PreparedBy, ReviewedBy, ApprovedBy, ApprovedAt, ActivatedAt)
    ON t.CompanyCode = s.CompanyCode AND t.CostSetCode = s.CostSetCode
    WHEN MATCHED THEN UPDATE SET Name = s.Name, Status = s.Status, EffectiveFrom = s.EffectiveFrom
    WHEN NOT MATCHED THEN INSERT (CompanyCode, CostSetCode, Name, Purpose, EffectiveFrom, CurrencyCode,
         Status, PreparedBy, ReviewedBy, ApprovedBy, ApprovedAt, ActivatedAt, SourceEvidence)
         VALUES (s.CompanyCode, s.CostSetCode, s.Name, s.Purpose, s.EffectiveFrom, s.CurrencyCode,
         s.Status, s.PreparedBy, s.ReviewedBy, s.ApprovedBy, s.ApprovedAt, s.ActivatedAt,
         'Specification 10, worked example B4');

    MERGE cst.StandardCost AS t
    USING (SELECT s.CompanyCode, s.CostSetCode, s.ItemCode, s.UnitCost
             FROM (VALUES
               ('STC','KITDEMO','DEMO-B4-TOP',15.360000), ('STC','KITDEMO','DEMO-B4-SUB',1.280000),
               ('STC','KITDEMO','DEMO-B4-RUNAID',5.000000), ('STC','KITDEMO','DEMO-B4-BATAID',4.000000),
               ('STC','KITDEMO','DEMO-B4-STEEL',2.100000)
             ) AS s (CompanyCode, CostSetCode, ItemCode, UnitCost)
             JOIN inv.Item i ON i.CompanyCode = s.CompanyCode AND i.ItemCode = s.ItemCode) AS s
    ON t.CompanyCode = s.CompanyCode AND t.CostSetCode = s.CostSetCode AND t.ItemCode = s.ItemCode
       AND t.Revision = ''
    WHEN MATCHED THEN UPDATE SET TotalUnitCost = s.UnitCost, Completeness = 'COMPLETE'
    WHEN NOT MATCHED THEN INSERT (CompanyCode, CostSetCode, ItemCode, Revision, CostingLotSize,
         TotalUnitCost, Completeness)
         VALUES (s.CompanyCode, s.CostSetCode, s.ItemCode, '', 1, s.UnitCost, 'COMPLETE');

    /*  The same rates on the item, at every site the company runs, because the driver builds
        wherever the material actually is and an issue is valued at the site it leaves.  The row
        carries the item's current revision: STKMOVE fills a blank movement revision from the item
        master and then looks the cost up under that revision, so a row filed under '' is never
        found by an issue.                                                                       */
    MERGE cst.ItemCost AS t
    USING (SELECT sc.CompanyCode, st.SiteCode, sc.ItemCode,
                  Revision = ISNULL(i.CurrentRevision,''), sc.TotalUnitCost
             FROM cst.StandardCost sc
             JOIN inv.Item i ON i.CompanyCode = sc.CompanyCode AND i.ItemCode = sc.ItemCode
             JOIN core.Site st ON st.CompanyCode = sc.CompanyCode
            WHERE sc.CompanyCode = 'STC' AND sc.CostSetCode = 'KITDEMO') AS s
    ON t.CompanyCode = s.CompanyCode AND t.ItemCode = s.ItemCode AND t.Revision = s.Revision
       AND t.SiteCode = s.SiteCode
    WHEN MATCHED AND ISNULL(t.StandardUnitCost,0) = 0 THEN UPDATE SET StandardUnitCost = s.TotalUnitCost,
         AverageUnitCost = CASE WHEN ISNULL(t.AverageUnitCost,0) = 0 THEN s.TotalUnitCost
                                ELSE t.AverageUnitCost END,
         LastUpdatedAt = SYSDATETIME(), LastUpdatedBy = 'COSTACC'
    WHEN NOT MATCHED THEN INSERT (CompanyCode, ItemCode, Revision, SiteCode, CostMethod, QtyOnHand,
         CarryingValueGBP, AverageUnitCost, StandardUnitCost, LastUpdatedAt, LastUpdatedBy)
         VALUES (s.CompanyCode, s.ItemCode, s.Revision, s.SiteCode, 'STD', 0, 0, s.TotalUnitCost,
         s.TotalUnitCost, SYSDATETIME(), 'COSTACC');

    PRINT 'Seeded cst standard costs for the DEMO-B4 demonstration family';
END
GO
PRINT 'Seeded kit demonstration data';
GO
