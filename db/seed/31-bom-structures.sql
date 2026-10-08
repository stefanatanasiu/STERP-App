/*  The bills of materials of worked examples B1 to B6 and of acceptance scenarios AC-085 to AC-096
    and AC-110, seeded as real released data so the forms, the reports and the COBOL unit tests all
    work from one definition.  Every number here is the one printed in specification sections 9 to
    11; the tests assert the published results against it.
      B1  DEMO-FRAME   nested structure, yield, phantom, fixed per run and fixed per batch
      B2  (same bills) netted against the stock seeded in 90-bom-demo.sql
      B3  DEMO-B3-PARENT  nominal basis ten, grams to kilograms, four per cent allowance
      B4  DEMO-B4-TOP     two independent occurrences of one child; run consolidation
      B5  DEMO-B5-TOP     two 1.2 piece occurrences, whole piece issue, packs of five
      B6  DEMO-B6-ROOT    valid shared descendant; DEMO-B6-LOOP an invalid cycle
      AC-110 DEMO-DEEP-00 twenty edges, usage two, leaf requirement 1,048,576
    REQ: BOM-001 to BOM-003, BOM-006 to BOM-011, BOM-013, BOM-016 to BOM-019, BOM-026, BOM-027 */
SET NOCOUNT ON;
GO
IF OBJECT_ID('tempdb..#BomDef') IS NOT NULL DROP TABLE #BomDef;
CREATE TABLE #BomDef (BomRef varchar(30), BomRev varchar(4), ParentItem varchar(40),
    Descr varchar(150), Basis decimal(19,6), Yield decimal(9,6), Quantum decimal(19,6),
    BatchSize decimal(19,6), Phantom bit, Consolidate bit, SetupCost decimal(19,6),
    ConvCost decimal(19,6), EffFrom date, EffTo date, Status varchar(12), LossNote varchar(200),
    PRIMARY KEY (BomRef, BomRev));
INSERT #BomDef VALUES
 ('BOM-DEMO-FRAME','A','DEMO-FRAME','B1 frame assembly, ten gross frames per batch',1,1.000000,1,10,0,0,10.000000,0.500000,'2026-01-01',NULL,'RELEASED','Frame yield is one; no component scrap is claimed at this level'),
 ('BOM-DEMO-BRACE','A','DEMO-BRACE','B1 brace subassembly, eighty per cent yield',1,0.800000,1,10,0,0,0.000000,0.600000,'2026-01-01',NULL,'RELEASED','Brace yield models lost brace output; the plate allowance is a separate extra input'),
 ('BOM-DEMO-PACK','A','DEMO-PACK','B1 packing group, phantom, no independent rounding',1,1.000000,1,NULL,1,0,NULL,NULL,'2026-01-01',NULL,'RELEASED','Phantom grouping only'),
 ('BOM-DEMO-B3','A','DEMO-B3-PARENT','B3 bill written per ten parents',10,1.000000,1,NULL,0,0,NULL,NULL,'2026-01-01',NULL,'RELEASED','Allowance is an additional input, not a yield'),
 ('BOM-DEMO-B4T','A','DEMO-B4-TOP','B4 top level, two independent occurrences of one child',1,1.000000,1,NULL,0,0,NULL,NULL,'2026-01-01',NULL,'RELEASED',NULL),
 ('BOM-DEMO-B4TC','A','DEMO-B4-TOP','B4 top level with approved execution consolidation',1,1.000000,1,NULL,0,1,NULL,NULL,'2026-01-01',NULL,'DRAFT',NULL),
 ('BOM-DEMO-B4S','A','DEMO-B4-SUB','B4 shared child, batch size ten',1,1.000000,1,10,0,0,NULL,NULL,'2026-01-01',NULL,'RELEASED',NULL),
 ('BOM-DEMO-B5','A','DEMO-B5-TOP','B5 two 1.2 piece occurrences, packs of five',1,1.000000,1,NULL,0,0,NULL,NULL,'2026-01-01',NULL,'RELEASED',NULL),
 ('BOM-DEMO-B6R','A','DEMO-B6-ROOT','B6 root with two subassemblies sharing one screw',1,1.000000,1,NULL,0,0,NULL,NULL,'2026-01-01',NULL,'RELEASED',NULL),
 ('BOM-DEMO-B6A','A','DEMO-B6-SUBA','B6 subassembly A',1,1.000000,1,NULL,0,0,NULL,NULL,'2026-01-01',NULL,'RELEASED',NULL),
 ('BOM-DEMO-B6B','A','DEMO-B6-SUBB','B6 subassembly B',1,1.000000,1,NULL,0,0,NULL,NULL,'2026-01-01',NULL,'RELEASED',NULL),
 ('BOM-DEMO-B6L','A','DEMO-B6-LOOP','B6 subassembly that uses its root again - a cycle',1,1.000000,1,NULL,0,0,NULL,NULL,'2026-01-01',NULL,'RELEASED',NULL),
 ('BOM-DEMO-B6C','A','DEMO-B6-CYC','B6 cyclic root: root to loop to root',1,1.000000,1,NULL,0,0,NULL,NULL,'2026-01-01',NULL,'RELEASED',NULL),
 ('BOM-DEMO-ERR','A','DEMO-ERR-TOP','Manufactured child with no usable bill (BOM-029)',1,1.000000,1,NULL,0,0,NULL,NULL,'2026-01-01',NULL,'RELEASED',NULL),
 ('BOM-DEMO-OPT','A','DEMO-OPT-TOP','Options, notes, services and cost only resources',1,1.000000,1,NULL,0,0,NULL,NULL,'2026-01-01',NULL,'RELEASED',NULL),
 ('BOM-DEMO-NOBOM','A','DEMO-NOBOM','Deliberately absent: the parent exists, the bill does not',1,1.000000,1,NULL,0,0,NULL,NULL,'2026-01-01',NULL,'DRAFT',NULL);
/*  AC-086: adjacent revisions that change at a boundary.  Revision A ends on 1 July 2026, which is
    exactly when revision B starts; effectivity is [start, end), so one and only one applies on any
    date.  Revision C is a deliberate overlap used to prove the overlap refusal.  */
INSERT #BomDef VALUES
 ('BOM-DEMO-BDY','A','DEMO-OPT-TOP','Boundary demonstration, revision A',1,1.000000,1,NULL,0,0,NULL,NULL,'2026-01-01','2026-07-01','RELEASED',NULL),
 ('BOM-DEMO-BDY','B','DEMO-OPT-TOP','Boundary demonstration, revision B',1,1.000000,1,NULL,0,0,NULL,NULL,'2026-07-01',NULL,'DRAFT',NULL);
INSERT #BomDef
SELECT CONCAT('BOM-DEEP-', RIGHT('0' + CONVERT(varchar(2), n), 2)), 'A',
       CONCAT('DEMO-DEEP-', RIGHT('0' + CONVERT(varchar(2), n), 2)),
       CONCAT('AC-110 deep chain level ', n), 1, 1.000000, 1, NULL, 0, 0, NULL, NULL,
       '2026-01-01', NULL, 'RELEASED', NULL
FROM (SELECT TOP (20) ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) - 1 AS n FROM sys.all_objects) q;
GO
MERGE bom.BomHeader AS t
USING (SELECT 'STC' AS CompanyCode, d.* FROM #BomDef d) AS s
ON t.CompanyCode = s.CompanyCode AND t.BomRef = s.BomRef AND t.BomRevision = s.BomRev
WHEN MATCHED THEN UPDATE SET ParentItemCode = s.ParentItem, Description = s.Descr, NominalBasis = s.Basis,
     YieldFactor = s.Yield, ProductionQuantum = s.Quantum, BatchSize = s.BatchSize, IsPhantom = s.Phantom,
     ConsolidateRuns = s.Consolidate, SetupCostPerRun = s.SetupCost, ConversionCostPerStart = s.ConvCost,
     EffectiveFrom = s.EffFrom, EffectiveTo = s.EffTo, Status = s.Status, LossIntentNote = s.LossNote,
     ParentRevision = 'A', Purpose = 'STANDARD', ReviewerUser = 'ENGMGR',
     ReleasedBy = CASE WHEN s.Status = 'RELEASED' THEN 'ENGMGR' END,
     ReleasedAt = CASE WHEN s.Status = 'RELEASED' THEN SYSDATETIME() END,
     ModifiedBy = 'ENGINEER', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (CompanyCode, BomRef, BomRevision, ParentItemCode, ParentRevision, Purpose,
     Description, NominalBasis, YieldFactor, ProductionQuantum, BatchSize, IsPhantom, ConsolidateRuns,
     SetupCostPerRun, ConversionCostPerStart, EffectiveFrom, EffectiveTo, Status, AuthorUser, ReviewerUser,
     ReleasedBy, ReleasedAt, LossIntentNote, CreatedBy)
     VALUES (s.CompanyCode, s.BomRef, s.BomRev, s.ParentItem, 'A', 'STANDARD', s.Descr, s.Basis, s.Yield,
     s.Quantum, s.BatchSize, s.Phantom, s.Consolidate, s.SetupCost, s.ConvCost, s.EffFrom, s.EffTo, s.Status,
     'ENGINEER', 'ENGMGR', CASE WHEN s.Status = 'RELEASED' THEN 'ENGMGR' END,
     CASE WHEN s.Status = 'RELEASED' THEN SYSDATETIME() END, s.LossNote, 'ENGINEER');
GO
IF OBJECT_ID('tempdb..#BomCmp') IS NOT NULL DROP TABLE #BomCmp;
CREATE TABLE #BomCmp (BomRef varchar(30), BomRev varchar(4), LineNum int, Item varchar(40),
    Qty decimal(19,6), Basis varchar(12), Uom varchar(10), Conv decimal(19,9), Scrap decimal(9,6),
    Supply varchar(10), Optional bit, AltGroup varchar(10), Selected bit, OptionRef varchar(20),
    CostRate decimal(19,6), CostElement varchar(10), Notes varchar(300));
INSERT #BomCmp VALUES
 ('BOM-DEMO-FRAME','A',10,'DEMO-BRACE',2,'PROPORTION','EA',1,0,'MAKE',0,NULL,1,NULL,NULL,NULL,'Two braces per frame'),
 ('BOM-DEMO-FRAME','A',20,'DEMO-PACK',1,'PROPORTION','EA',1,0,'PHANTOM',0,NULL,1,NULL,NULL,NULL,'Phantom packing group'),
 ('BOM-DEMO-FRAME','A',30,'DEMO-PIN',1,'PROPORTION','EA',1,0,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'Pin used directly in the frame'),
 ('BOM-DEMO-FRAME','A',40,'DEMO-CLEAN',0.5,'FIXEDRUN','L',1,0,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'Half a litre per execution run'),
 ('BOM-DEMO-BRACE','A',10,'DEMO-PLATE',0.4,'PROPORTION','KG',1,0.100000,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'Ten per cent additional input'),
 ('BOM-DEMO-BRACE','A',20,'DEMO-PIN',2,'PROPORTION','EA',1,0,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'Two pins per brace'),
 ('BOM-DEMO-BRACE','A',30,'DEMO-RESIN',0.2,'FIXEDBATCH','L',1,0,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'Fixed per parent batch'),
 ('BOM-DEMO-PACK','A',10,'DEMO-LABEL',1,'PROPORTION','EA',1,0,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'One label per pack'),
 ('BOM-DEMO-PACK','A',20,'DEMO-PIN',1,'PROPORTION','EA',1,0,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'Pin inside the phantom pack'),
 ('BOM-DEMO-B3','A',10,'DEMO-B3-GRAN',250,'PROPORTION','G',0.001,0.040000,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'250 grams per basis of ten parents'),
 ('BOM-DEMO-B4T','A',10,'DEMO-B4-SUB',6,'PROPORTION','EA',1,0,'MAKE',0,NULL,1,NULL,NULL,NULL,'First independent occurrence'),
 ('BOM-DEMO-B4T','A',20,'DEMO-B4-SUB',6,'PROPORTION','EA',1,0,'MAKE',0,NULL,1,NULL,NULL,NULL,'Second independent occurrence'),
 ('BOM-DEMO-B4TC','A',10,'DEMO-B4-SUB',6,'PROPORTION','EA',1,0,'MAKE',0,NULL,1,NULL,NULL,NULL,'First occurrence, consolidated'),
 ('BOM-DEMO-B4TC','A',20,'DEMO-B4-SUB',6,'PROPORTION','EA',1,0,'MAKE',0,NULL,1,NULL,NULL,NULL,'Second occurrence, consolidated'),
 ('BOM-DEMO-B4S','A',10,'DEMO-B4-RUNAID',0.3,'FIXEDRUN','L',1,0,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'0.3 litres per execution run'),
 ('BOM-DEMO-B4S','A',20,'DEMO-B4-BATAID',0.2,'FIXEDBATCH','L',1,0,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'0.2 litres per parent batch'),
 ('BOM-DEMO-B4S','A',30,'DEMO-B4-STEEL',0.5,'PROPORTION','KG',1,0,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'Half a kilogram per unit'),
 ('BOM-DEMO-B5','A',10,'DEMO-B5-PART',1.2,'PROPORTION','EA',1,0,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'First independent occurrence'),
 ('BOM-DEMO-B5','A',20,'DEMO-B5-PART',1.2,'PROPORTION','EA',1,0,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'Second independent occurrence'),
 ('BOM-DEMO-B6R','A',10,'DEMO-B6-SUBA',1,'PROPORTION','EA',1,0,'MAKE',0,NULL,1,NULL,NULL,NULL,'Valid shared descendant path one'),
 ('BOM-DEMO-B6R','A',20,'DEMO-B6-SUBB',1,'PROPORTION','EA',1,0,'MAKE',0,NULL,1,NULL,NULL,NULL,'Valid shared descendant path two'),
 ('BOM-DEMO-B6A','A',10,'DEMO-B6-SCREW',3,'PROPORTION','EA',1,0,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'Three screws'),
 ('BOM-DEMO-B6B','A',10,'DEMO-B6-SCREW',4,'PROPORTION','EA',1,0,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'Four screws'),
 ('BOM-DEMO-B6C','A',10,'DEMO-B6-LOOP',1,'PROPORTION','EA',1,0,'MAKE',0,NULL,1,NULL,NULL,NULL,'Leads to the cycle'),
 ('BOM-DEMO-B6L','A',10,'DEMO-B6-CYC',1,'PROPORTION','EA',1,0,'MAKE',0,NULL,1,NULL,NULL,NULL,'Returns to its root - rejected'),
 ('BOM-DEMO-ERR','A',10,'DEMO-NOBOM',1,'PROPORTION','EA',1,0,'MAKE',0,NULL,1,NULL,NULL,NULL,'No released bill exists for this child'),
 ('BOM-DEMO-OPT','A',10,'DEMO-OPT-RED',1,'PROPORTION','EA',1,0,'PURCHASED',1,'FINISH',0,'OPT-RED',NULL,NULL,'Alternative finish, not selected'),
 ('BOM-DEMO-OPT','A',20,'DEMO-OPT-BLUE',1,'PROPORTION','EA',1,0,'PURCHASED',1,'FINISH',0,'OPT-BLUE',NULL,NULL,'Alternative finish, not selected'),
 ('BOM-DEMO-OPT','A',30,'DEMO-NOTE',0,'PROPORTION','EA',1,0,'NOTE',0,NULL,1,NULL,NULL,NULL,'Deburr all edges before assembly'),
 ('BOM-DEMO-OPT','A',40,'DEMO-SERVICE',1,'PROPORTION','EA',1,0,'SERVICE',0,NULL,1,NULL,12.500000,'SUBC','Outside heat treatment'),
 ('BOM-DEMO-OPT','A',50,'DEMO-OVERHEAD',1,'FIXEDRUN','EA',1,0,'COSTONLY',0,NULL,1,NULL,NULL,'OVHF','Cost only resource with no rate held'),
 ('BOM-DEMO-OPT','A',60,'DEMO-PIN',2,'PROPORTION','EA',1,0,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'Ordinary material line'),
 ('BOM-DEMO-BDY','A',10,'DEMO-PIN',1,'PROPORTION','EA',1,0,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'Revision A uses one pin'),
 ('BOM-DEMO-BDY','B',10,'DEMO-PIN',2,'PROPORTION','EA',1,0,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'Revision B uses two pins'),
 ('BOM-DEMO-NOBOM','A',10,'DEMO-PIN',1,'PROPORTION','EA',1,0,'PURCHASED',0,NULL,1,NULL,NULL,NULL,'Placeholder');
INSERT #BomCmp
SELECT CONCAT('BOM-DEEP-', RIGHT('0' + CONVERT(varchar(2), n), 2)), 'A', 10,
       CONCAT('DEMO-DEEP-', RIGHT('0' + CONVERT(varchar(2), n + 1), 2)), 2, 'PROPORTION', 'EA', 1, 0,
       CASE WHEN n + 1 = 20 THEN 'PURCHASED' ELSE 'MAKE' END, 0, NULL, 1, NULL, NULL, NULL,
       'AC-110 edge, usage two'
FROM (SELECT TOP (20) ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) - 1 AS n FROM sys.all_objects) q;
GO
MERGE bom.BomComponent AS t
USING (SELECT h.BomHeaderId, c.* FROM #BomCmp c
       JOIN bom.BomHeader h ON h.CompanyCode = 'STC' AND h.BomRef = c.BomRef AND h.BomRevision = c.BomRev) AS s
ON t.BomHeaderId = s.BomHeaderId AND t.LineNum = s.LineNum
WHEN MATCHED THEN UPDATE SET ComponentItemCode = s.Item, UsageQty = s.Qty, UsageBasis = s.Basis,
     UomCode = s.Uom, ConversionFactor = s.Conv, ScrapPct = s.Scrap, SupplyBehaviour = s.Supply,
     IsOptional = s.Optional, AlternateGroup = s.AltGroup, IsSelected = s.Selected,
     OptionChoiceRef = s.OptionRef, CostOnlyRate = s.CostRate, CostElementCode = s.CostElement,
     Notes = s.Notes, ComponentRevision = 'A', PathRef = CONCAT(s.BomRef, '/', s.LineNum),
     ModifiedBy = 'ENGINEER', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (BomHeaderId, LineNum, PathRef, ComponentItemCode, ComponentRevision, UsageQty,
     UsageBasis, UomCode, ConversionFactor, ScrapPct, SupplyBehaviour, IsOptional, AlternateGroup, IsSelected,
     OptionChoiceRef, CostOnlyRate, CostElementCode, Notes, CreatedBy)
     VALUES (s.BomHeaderId, s.LineNum, CONCAT(s.BomRef, '/', s.LineNum), s.Item, 'A', s.Qty, s.Basis, s.Uom,
     s.Conv, s.Scrap, s.Supply, s.Optional, s.AltGroup, s.Selected, s.OptionRef, s.CostRate, s.CostElement,
     s.Notes, 'ENGINEER')
WHEN NOT MATCHED BY SOURCE AND t.BomHeaderId IN
     (SELECT BomHeaderId FROM bom.BomHeader WHERE CompanyCode = 'STC'
      AND (BomRef LIKE 'BOM-DEMO-%' OR BomRef LIKE 'BOM-DEEP-%'))
     THEN DELETE;
GO
/*  BOM-026: a recovery is a separate positive expected output with an approved valuation and
    disposition, never a negative material consumption.  */
MERGE bom.BomByProduct AS t
USING (SELECT h.BomHeaderId, v.LineNum, v.Item, v.Qty, v.Uom, v.Basis, v.Value, v.DispRule, v.Approver
       FROM (VALUES (10, 'DEMO-SWARF', 2.0, 'KG', 'NRV', 0.450000, 'Sold to the metals merchant monthly', 'COSTACC'))
            v(LineNum, Item, Qty, Uom, Basis, Value, DispRule, Approver)
       JOIN bom.BomHeader h ON h.CompanyCode = 'STC' AND h.BomRef = 'BOM-DEMO-OPT' AND h.BomRevision = 'A') AS s
ON t.BomHeaderId = s.BomHeaderId AND t.LineNum = s.LineNum
WHEN MATCHED THEN UPDATE SET ByProductItemCode = s.Item, RecoveryQty = s.Qty, UomCode = s.Uom,
     ValuationBasis = s.Basis, RecoveryValue = s.Value, DispositionRule = s.DispRule, ApprovedBy = s.Approver
WHEN NOT MATCHED THEN INSERT (BomHeaderId, LineNum, ByProductItemCode, RecoveryQty, UomCode, ConversionFactor,
     ValuationBasis, RecoveryValue, DispositionRule, ApprovedBy)
     VALUES (s.BomHeaderId, s.LineNum, s.Item, s.Qty, s.Uom, 1, s.Basis, s.Value, s.DispRule, s.Approver)
WHEN NOT MATCHED BY SOURCE AND t.BomHeaderId IN
     (SELECT BomHeaderId FROM bom.BomHeader WHERE CompanyCode = 'STC' AND BomRef LIKE 'BOM-DEMO-%')
     THEN DELETE;
GO
DROP TABLE #BomDef;
DROP TABLE #BomCmp;
GO

