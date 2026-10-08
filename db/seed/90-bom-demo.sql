/*  Demonstration transactions for the bills of materials module: the cost set that values the B1
    roll-up, the free stock that makes B2's netted plan reproducible against live balances, a
    released execution kit so the engineering change impact has something to find, and an
    engineering change with its preview.  Everything is keyed so the script re-runs cleanly.
    REQ: BOM-020, BOM-021, BOM-024, BOM-026, BOM-027, BOM-031, BOM-032 */
SET NOCOUNT ON;
GO
/*  B1 leaf unit costs, exactly as published: plate 4.00/kg, pin 0.25/piece, resin 6.00/litre,
    cleaner 8.00/litre and label 0.10/piece.  Conversion lives on the bill header (setup per run
    and a rate per gross start), because it belongs to the process, not to the material.  */
MERGE cst.CostSet AS t
USING (VALUES ('STC','BOMDEMO','Bill of materials demonstration cost set','STANDARD','2026-01-01','GBP','ACTIVE',
               'COSTACC','COSTMGR','FINCTRL','2025-12-18T16:40:00','2026-01-01T00:00:00'))
      AS s (CompanyCode, CostSetCode, Name, Purpose, EffectiveFrom, CurrencyCode, Status,
            PreparedBy, ReviewedBy, ApprovedBy, ApprovedAt, ActivatedAt)
ON t.CompanyCode = s.CompanyCode AND t.CostSetCode = s.CostSetCode
WHEN MATCHED THEN UPDATE SET Name = s.Name, Status = s.Status, EffectiveFrom = s.EffectiveFrom,
     PreparedBy = ISNULL(t.PreparedBy, s.PreparedBy), ReviewedBy = ISNULL(t.ReviewedBy, s.ReviewedBy),
     ApprovedBy = ISNULL(t.ApprovedBy, s.ApprovedBy), ApprovedAt = ISNULL(t.ApprovedAt, s.ApprovedAt),
     ActivatedAt = ISNULL(t.ActivatedAt, s.ActivatedAt)
WHEN NOT MATCHED THEN INSERT (CompanyCode, CostSetCode, Name, Purpose, EffectiveFrom, CurrencyCode, Status,
     PreparedBy, ReviewedBy, ApprovedBy, ApprovedAt, ActivatedAt,
     SourceEvidence) VALUES (s.CompanyCode, s.CostSetCode, s.Name, s.Purpose, s.EffectiveFrom, s.CurrencyCode,
     s.Status, s.PreparedBy, s.ReviewedBy, s.ApprovedBy, s.ApprovedAt, s.ActivatedAt,
     'Specification 10, worked example B1');
GO
MERGE cst.StandardCost AS t
USING (VALUES
   ('STC','BOMDEMO','DEMO-PLATE',4.000000), ('STC','BOMDEMO','DEMO-PIN',0.250000),
   ('STC','BOMDEMO','DEMO-RESIN',6.000000), ('STC','BOMDEMO','DEMO-CLEAN',8.000000),
   ('STC','BOMDEMO','DEMO-LABEL',0.100000), ('STC','BOMDEMO','DEMO-B6-SCREW',0.080000),
   ('STC','BOMDEMO','DEMO-B5-PART',1.750000), ('STC','BOMDEMO','DEMO-B3-GRAN',3.200000),
   ('STC','BOMDEMO','DEMO-B4-RUNAID',5.000000), ('STC','BOMDEMO','DEMO-B4-BATAID',4.000000),
   ('STC','BOMDEMO','DEMO-B4-STEEL',2.100000), ('STC','BOMDEMO','DEMO-OPT-RED',1.200000),
   ('STC','BOMDEMO','DEMO-OPT-BLUE',1.350000), ('STC','BOMDEMO','DEMO-DEEP-20',0.010000)
 ) AS s (CompanyCode, CostSetCode, ItemCode, UnitCost)
ON t.CompanyCode = s.CompanyCode AND t.CostSetCode = s.CostSetCode AND t.ItemCode = s.ItemCode
   AND t.Revision = ''
WHEN MATCHED THEN UPDATE SET TotalUnitCost = s.UnitCost, Completeness = 'COMPLETE'
WHEN NOT MATCHED THEN INSERT (CompanyCode, CostSetCode, ItemCode, Revision, CostingLotSize, TotalUnitCost,
     Completeness) VALUES (s.CompanyCode, s.CostSetCode, s.ItemCode, '', 1, s.UnitCost, 'COMPLETE');
GO
/*  The same rates as the item standard cost column, so the fallback route of BOMCOST can be
    demonstrated when no cost set is named.  */
MERGE cst.ItemCost AS t
USING (SELECT sc.CompanyCode, sc.ItemCode, sc.TotalUnitCost FROM cst.StandardCost sc
       WHERE sc.CompanyCode = 'STC' AND sc.CostSetCode = 'BOMDEMO') AS s
ON t.CompanyCode = s.CompanyCode AND t.ItemCode = s.ItemCode AND t.Revision = '' AND t.SiteCode = 'RDG'
WHEN MATCHED THEN UPDATE SET StandardUnitCost = s.TotalUnitCost, LastUpdatedAt = SYSDATETIME(),
     LastUpdatedBy = 'COSTACC'
WHEN NOT MATCHED THEN INSERT (CompanyCode, ItemCode, Revision, SiteCode, CostMethod, QtyOnHand,
     CarryingValueGBP, AverageUnitCost, StandardUnitCost, LastUpdatedAt, LastUpdatedBy)
     VALUES (s.CompanyCode, s.ItemCode, '', 'RDG', 'STD', 0, 0, s.TotalUnitCost, s.TotalUnitCost,
     SYSDATETIME(), 'COSTACC');
GO
/*  B2's eligible supply as live balances at Reading: five good braces, 8.000 kg plate, 60 free
    pins, 0.400 litres resin, 0.500 litres cleaner and 12 labels.  All accepted, owned and
    unreserved, so the live netted plan reproduces the published trial.  */
MERGE inv.StockBalance AS t
USING (VALUES
   ('DEMO-BRACE', 5.000000), ('DEMO-PLATE', 8.000000), ('DEMO-PIN', 60.000000),
   ('DEMO-RESIN', 0.400000), ('DEMO-CLEAN', 0.500000), ('DEMO-LABEL', 12.000000)
 ) AS s (ItemCode, Qty)
ON t.CompanyCode = 'STC' AND t.SiteCode = 'RDG' AND t.WarehouseCode = 'MAIN'
   AND t.LocationCode = 'RAW01' AND t.ItemCode = s.ItemCode AND t.Revision = ''
   AND t.LotCode = '' AND t.SerialNo = '' AND t.OwnerType = 'OWNED' AND t.OwnerCode = ''
   AND t.QualityStatus = 'ACCEPTED'
WHEN MATCHED THEN UPDATE SET QtyOnHand = s.Qty, QtyReserved = 0, LastMovementAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode, Revision,
     LotCode, SerialNo, OwnerType, OwnerCode, QualityStatus, QtyOnHand, QtyReserved, QtyInTransit,
     LastMovementAt)
     VALUES ('STC', 'RDG', 'MAIN', 'RAW01', s.ItemCode, '', '', '', 'OWNED', '', 'ACCEPTED', s.Qty, 0, 0,
     SYSDATETIME());
GO
/*  A released execution kit for ten frames.  BOM-031: a new standard must not silently re-explode
    it - the change needs an explicit retain, adopt or replan decision.  */
IF OBJECT_ID('kit.ExecutionKit') IS NOT NULL
BEGIN
    MERGE kit.ExecutionKit AS t
    USING (SELECT 'STC' AS CompanyCode, 'RDG' AS SiteCode, 'KIT-BOMDEMO-001' AS KitRef,
                  'DEMO-FRAME' AS ParentItemCode, h.BomHeaderId, h.BomRevision, 10.0 AS PlannedGoodQty,
                  10.0 AS GrossStarts, 1.0 AS YieldFactor, 1.0 AS ProductionQuantum, 10.0 AS BatchSize
           FROM bom.BomHeader h
           WHERE h.CompanyCode = 'STC' AND h.BomRef = 'BOM-DEMO-FRAME' AND h.BomRevision = 'A') AS s
    ON t.CompanyCode = s.CompanyCode AND t.KitRef = s.KitRef
    WHEN MATCHED THEN UPDATE SET BomHeaderId = s.BomHeaderId, BomRevision = s.BomRevision,
         Status = 'RELEASED', PlannedGoodQty = s.PlannedGoodQty
    WHEN NOT MATCHED THEN INSERT (CompanyCode, SiteCode, KitRef, ParentItemCode, ParentRevision,
         BomHeaderId, BomRevision, SourceDemandType, SourceDemandRef, PlannedGoodQty, GrossStarts,
         YieldFactor, ProductionQuantum, BatchSize, RunCount, BatchCount, PlannedStartDate, NeedDate,
         SelectionDate, OwnerUser, Status, ReleasedBy, ReleasedAt, CreatedBy)
         VALUES (s.CompanyCode, s.SiteCode, s.KitRef, s.ParentItemCode, 'A', s.BomHeaderId, s.BomRevision,
         'JOB', 'JOB-BOMDEMO-01', s.PlannedGoodQty, s.GrossStarts, s.YieldFactor, s.ProductionQuantum,
         s.BatchSize, 1, 1, '2026-10-12', '2026-10-26', '2026-10-12', 'ENGINEER', 'RELEASED', 'ENGMGR',
         SYSDATETIME(), 'ENGINEER');
END
GO
/*  An engineering change that replaces the pin with a stronger one everywhere it is used.  It is
    seeded in PREVIEW so the bulk change screen has something to show.  */
MERGE bom.BomChange AS t
USING (VALUES ('STC','ECO-BOMDEMO-001','Replace DEMO-PIN with DEMO-PIN-HD across all bills',
               'Field failures on the standard pin; the heavy duty pin is form, fit and function compatible.',
               'REPLACE','ENGINEER','2026-11-01','PREVIEW'))
      AS s (CompanyCode, ChangeRef, Title, Descr, ChangeType, RequestedBy, EffDate, Status)
ON t.CompanyCode = s.CompanyCode AND t.ChangeRef = s.ChangeRef
WHEN MATCHED THEN UPDATE SET Title = s.Title, Description = s.Descr, ChangeType = s.ChangeType,
     EffectiveDate = s.EffDate, Status = s.Status
WHEN NOT MATCHED THEN INSERT (CompanyCode, ChangeRef, Title, Description, ChangeType, RequestedBy,
     EffectiveDate, Status) VALUES (s.CompanyCode, s.ChangeRef, s.Title, s.Descr, s.ChangeType,
     s.RequestedBy, s.EffDate, s.Status);
GO
/*  The heavy duty pin the change proposes, and a stocked quantity of it.  */
MERGE inv.Item AS t
USING (VALUES ('STC','DEMO-PIN-HD','Demonstration heavy duty pin, whole pieces')) AS s (CompanyCode, ItemCode, Descr)
ON t.CompanyCode = s.CompanyCode AND t.ItemCode = s.ItemCode
WHEN NOT MATCHED THEN INSERT (CompanyCode, ItemCode, Description, ItemClass, Category, StockUomCode,
     QuantumQty, IsIndivisible, MakeBuy, IsManufacturable, IsStocked, IsPurchasable, IsSaleable,
     IsConsumable, CurrentRevision, ValuationGroup, CostMethod, LifecycleStatus, ReleasedAt, ReleasedBy,
     CreatedBy)
     VALUES (s.CompanyCode, s.ItemCode, s.Descr, 'BOUGHT', 'BOMDEMO', 'EA', 1, 1, 'BUY', 0, 1, 1, 0, 1,
     'A', 'BOMDEMO', 'STD', 'RELEASED', SYSDATETIME(), 'ENGMGR', 'ENGINEER');
GO
IF NOT EXISTS (SELECT 1 FROM cst.StandardCost WHERE CompanyCode = 'STC' AND CostSetCode = 'BOMDEMO'
               AND ItemCode = 'DEMO-PIN-HD' AND Revision = '')
    INSERT cst.StandardCost (CompanyCode, CostSetCode, ItemCode, Revision, CostingLotSize, TotalUnitCost,
        Completeness) VALUES ('STC', 'BOMDEMO', 'DEMO-PIN-HD', '', 1, 0.310000, 'COMPLETE');
GO
