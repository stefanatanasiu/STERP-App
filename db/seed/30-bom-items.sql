/*  Items used by the bill of materials worked examples B1 to B6 of specification sections 9 to 11,
    and by the deep chain of acceptance scenario AC-110.  They are deliberately synthetic (DEMO-
    prefix) so that the engineering demonstrations and the COBOL unit tests reproduce the published
    numbers exactly without depending on the commercial catalogue.  Quantum and indivisibility drive
    material rounding; the purchase multiple on the planning site drives the pack proposal.
    REQ: BOM-001, BOM-002, BOM-010, BOM-011, BOM-016, BOM-017, BOM-027, D-09, D-21 */
SET NOCOUNT ON;
GO
IF OBJECT_ID('tempdb..#BomDemoItem') IS NOT NULL DROP TABLE #BomDemoItem;
CREATE TABLE #BomDemoItem (ItemCode varchar(40) COLLATE DATABASE_DEFAULT PRIMARY KEY,
    Description varchar(100) COLLATE DATABASE_DEFAULT, ItemClass varchar(12) COLLATE DATABASE_DEFAULT,
    StockUom varchar(10) COLLATE DATABASE_DEFAULT, Quantum decimal(19,6), Indivisible bit,
    MakeBuy varchar(6) COLLATE DATABASE_DEFAULT, Manu bit, Stocked bit,
    Purch bit, PackMult decimal(19,6));
INSERT #BomDemoItem VALUES
 ('DEMO-FRAME','Demonstration frame assembly (B1 root)','MADE','EA',1,1,'MAKE',1,1,0,NULL),
 ('DEMO-BRACE','Demonstration brace subassembly (B1)','MADE','EA',1,1,'MAKE',1,1,0,NULL),
 ('DEMO-PACK','Demonstration packing group, phantom (B1)','PHANTOM','EA',1,1,'MAKE',1,0,0,NULL),
 ('DEMO-PIN','Demonstration pin, whole pieces (B1)','BOUGHT','EA',1,1,'BUY',0,1,1,NULL),
 ('DEMO-PLATE','Demonstration plate stock, kilograms (B1)','BOUGHT','KG',0.001,0,'BUY',0,1,1,NULL),
 ('DEMO-RESIN','Demonstration resin, litres (B1)','BOUGHT','L',0.001,0,'BUY',0,1,1,NULL),
 ('DEMO-CLEAN','Demonstration cleaner, litres (B1)','BOUGHT','L',0.001,0,'BUY',0,1,1,NULL),
 ('DEMO-LABEL','Demonstration label, whole pieces (B1)','BOUGHT','EA',1,1,'BUY',0,1,1,NULL),
 ('DEMO-B3-PARENT','B3 parent, bill written per ten parents','MADE','EA',1,1,'MAKE',1,1,0,NULL),
 ('DEMO-B3-GRAN','B3 granular component, stocked in kilograms','BOUGHT','KG',0.001,0,'BUY',0,1,1,NULL),
 ('DEMO-B4-TOP','B4 top level with two independent occurrences','MADE','EA',1,1,'MAKE',1,1,0,NULL),
 ('DEMO-B4-SUB','B4 shared child, batch size ten','MADE','EA',1,1,'MAKE',1,1,0,NULL),
 ('DEMO-B4-RUNAID','B4 processing aid, fixed per execution run','BOUGHT','L',0.001,0,'BUY',0,1,1,NULL),
 ('DEMO-B4-BATAID','B4 processing aid, fixed per parent batch','BOUGHT','L',0.001,0,'BUY',0,1,1,NULL),
 ('DEMO-B4-STEEL','B4 steel input for the shared child','BOUGHT','KG',0.001,0,'BUY',0,1,1,NULL),
 ('DEMO-B5-TOP','B5 parent with two 1.2 piece occurrences','MADE','EA',1,1,'MAKE',1,1,0,NULL),
 ('DEMO-B5-PART','B5 whole piece part, bought in packs of five','BOUGHT','EA',1,1,'BUY',0,1,1,5),
 ('DEMO-B6-ROOT','B6 root using two subassemblies','MADE','EA',1,1,'MAKE',1,1,0,NULL),
 ('DEMO-B6-SUBA','B6 subassembly A','MADE','EA',1,1,'MAKE',1,1,0,NULL),
 ('DEMO-B6-SUBB','B6 subassembly B','MADE','EA',1,1,'MAKE',1,1,0,NULL),
 ('DEMO-B6-SCREW','B6 shared screw reached by two valid paths','BOUGHT','EA',1,1,'BUY',0,1,1,NULL),
 ('DEMO-B6-LOOP','B6 subassembly that cycles back to its root','MADE','EA',1,1,'MAKE',1,1,0,NULL),
 ('DEMO-B6-CYC','B6 cyclic root used to prove the refusal','MADE','EA',1,1,'MAKE',1,1,0,NULL),
 ('DEMO-ERR-TOP','Parent whose manufactured child has no bill','MADE','EA',1,1,'MAKE',1,1,0,NULL),
 ('DEMO-OPT-TOP','Options and non-material lines demonstration','MADE','EA',1,1,'MAKE',1,1,0,NULL),
 ('DEMO-OPT-RED','Alternative finish, red','BOUGHT','EA',1,1,'BUY',0,1,1,NULL),
 ('DEMO-OPT-BLUE','Alternative finish, blue','BOUGHT','EA',1,1,'BUY',0,1,1,NULL),
 ('DEMO-SERVICE','Outside heat treatment service','SERVICE','EA',1,0,'BUY',0,0,1,NULL),
 ('DEMO-OVERHEAD','Cost only resource, no stock movement','COSTONLY','EA',1,0,'BUY',0,0,0,NULL),
 ('DEMO-NOTE','Engineering note placeholder, no material','NONSTOCK','EA',1,0,'BUY',0,0,0,NULL),
 ('DEMO-SWARF','Recovered swarf, a by-product','BOUGHT','KG',0.001,0,'BUY',0,1,0,NULL),
 ('DEMO-NOBOM','Manufactured child whose bill is deliberately absent','MADE','EA',1,1,'MAKE',1,1,0,NULL);
/*  AC-110: an acyclic chain of twenty edges, usage two at every edge, yield one.  Levels 00 to 19
    are manufactured; level 20 is the purchased leaf that carries the 1,048,576 total.  */
INSERT #BomDemoItem
SELECT CONCAT('DEMO-DEEP-', RIGHT('0' + CONVERT(varchar(2), n), 2)),
       CONCAT('Deep chain level ', RIGHT('0' + CONVERT(varchar(2), n), 2), ' (AC-110)'),
       CASE WHEN n = 20 THEN 'BOUGHT' ELSE 'MADE' END, 'EA', 1, 1,
       CASE WHEN n = 20 THEN 'BUY' ELSE 'MAKE' END,
       CASE WHEN n = 20 THEN 0 ELSE 1 END, 1, CASE WHEN n = 20 THEN 1 ELSE 0 END, NULL
FROM (SELECT TOP (21) ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) - 1 AS n FROM sys.all_objects) q;
GO
MERGE inv.Item AS t
USING (SELECT c.CompanyCode, i.* FROM (VALUES ('STC')) c(CompanyCode) CROSS JOIN #BomDemoItem i) AS s
ON t.CompanyCode = s.CompanyCode AND t.ItemCode = s.ItemCode
WHEN MATCHED THEN UPDATE SET Description = s.Description, ItemClass = s.ItemClass, Category = 'BOMDEMO',
     StockUomCode = s.StockUom, QuantumQty = s.Quantum, IsIndivisible = s.Indivisible, MakeBuy = s.MakeBuy,
     IsManufacturable = s.Manu, IsStocked = s.Stocked, IsPurchasable = s.Purch, LifecycleStatus = 'RELEASED',
     ModifiedBy = 'ENGINEER', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (CompanyCode, ItemCode, Description, ItemClass, Category, StockUomCode, QuantumQty,
     IsIndivisible, MakeBuy, IsManufacturable, IsStocked, IsPurchasable, IsSaleable, IsConsumable, CurrentRevision,
     ValuationGroup, CostMethod, LifecycleStatus, ReleasedAt, ReleasedBy, CreatedBy)
     VALUES (s.CompanyCode, s.ItemCode, s.Description, s.ItemClass, 'BOMDEMO', s.StockUom, s.Quantum,
     s.Indivisible, s.MakeBuy, s.Manu, s.Stocked, s.Purch, 0, 1, 'A', 'BOMDEMO', 'STD', 'RELEASED',
     SYSDATETIME(), 'ENGMGR', 'ENGINEER');
GO
MERGE inv.ItemRevision AS t
USING (SELECT 'STC' AS CompanyCode, i.ItemCode, 'A' AS Revision FROM #BomDemoItem i) AS s
ON t.CompanyCode = s.CompanyCode AND t.ItemCode = s.ItemCode AND t.Revision = s.Revision
WHEN NOT MATCHED THEN INSERT (CompanyCode, ItemCode, Revision, Description, EffectiveFrom, Status,
     IsInterchangeable, ApprovedBy, ApprovedAt, CreatedBy)
     VALUES (s.CompanyCode, s.ItemCode, s.Revision, 'Baseline revision for the BOM demonstrations',
     '2026-01-01', 'RELEASED', 1, 'ENGMGR', SYSDATETIME(), 'ENGINEER');
GO
/*  Planning data at Reading.  Only DEMO-B5-PART carries a purchase multiple: B5 needs packs of five
    so that the excess supply is visible and separate from the engineering requirement.  */
MERGE inv.ItemSite AS t
USING (SELECT 'STC' AS CompanyCode, 'RDG' AS SiteCode, i.ItemCode, i.PackMult,
              CASE WHEN i.MakeBuy = 'MAKE' THEN 5 ELSE 10 END AS LeadTimeDays
       FROM #BomDemoItem i) AS s
ON t.CompanyCode = s.CompanyCode AND t.SiteCode = s.SiteCode AND t.ItemCode = s.ItemCode
WHEN MATCHED THEN UPDATE SET PurchaseMultipleQty = s.PackMult, LeadTimeDays = s.LeadTimeDays,
     DefaultWarehouseCode = 'MAIN', DefaultLocationCode = 'RAW01', MakeBuy = NULL, IsActive = 1,
     ModifiedBy = 'ENGINEER', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (CompanyCode, SiteCode, ItemCode, DefaultWarehouseCode, DefaultLocationCode,
     LeadTimeDays, PurchaseMultipleQty, AbcClass, IsActive, CreatedBy)
     VALUES (s.CompanyCode, s.SiteCode, s.ItemCode, 'MAIN', 'RAW01', s.LeadTimeDays, s.PackMult, 'C', 1,
     'ENGINEER');
GO
IF NOT EXISTS (SELECT 1 FROM inv.Uom WHERE UomCode = 'PK5')
    INSERT inv.Uom (UomCode, Name, UomDimension, BaseUomCode, FactorToBase, IsMetric, DecimalPlaces, IsActive)
    VALUES ('PK5', 'Pack of five', 'EACH', 'EA', 5.0, 1, 0, 1);
GO
/*  The component units used by the bills: grams for the B3 granular component (stocked in
    kilograms) and the stock unit itself for everything else.  BOM-010 - a conversion must be
    present and positive before compatible aggregation or costing.  */
MERGE inv.ItemUom AS t
USING (VALUES
   ('STC','DEMO-B3-GRAN','G','ISSUE',0.001,'One thousand grams to the kilogram'),
   ('STC','DEMO-PLATE','KG','ISSUE',1.0,'Issued in kilograms'),
   ('STC','DEMO-RESIN','L','ISSUE',1.0,'Issued in litres'),
   ('STC','DEMO-CLEAN','L','ISSUE',1.0,'Issued in litres'),
   ('STC','DEMO-B5-PART','PK5','PACK',5.0,'Purchase pack of five')
 ) AS s (CompanyCode, ItemCode, UomCode, UsageType, StockUnitsPerUom, PackDescription)
ON t.CompanyCode = s.CompanyCode AND t.ItemCode = s.ItemCode AND t.UomCode = s.UomCode
   AND t.UsageType = s.UsageType AND t.EffectiveFrom = '2000-01-01'
WHEN MATCHED THEN UPDATE SET StockUnitsPerUom = s.StockUnitsPerUom, PackDescription = s.PackDescription
WHEN NOT MATCHED THEN INSERT (CompanyCode, ItemCode, UomCode, UsageType, StockUnitsPerUom, PackDescription,
     EffectiveFrom, CreatedBy)
     VALUES (s.CompanyCode, s.ItemCode, s.UomCode, s.UsageType, s.StockUnitsPerUom, s.PackDescription,
     '2000-01-01', 'ENGINEER');
GO
DROP TABLE #BomDemoItem;
GO
