/*  Showcase bills of materials for demonstrations: a celebration cake and a commuter bicycle,
    each a released multi-level structure with costs in the BOMDEMO cost set.
      Cake     FG-CAKE-CELEB-8IN   cake > sponge layer > batter > ingredients (and phantom
               decoration > sugar rose > flower paste); grams/millilitres issued from
               kilogram/litre stock, a basis of ten, yield on the oven, a trimming allowance,
               a fixed-per-run line, a note, a labour line and a by-product
      Bicycle  FG-BIKE-COMMUTE-M   bike > frame > front triangle > bottom bracket assembly >
               bearings; wheelset > wheels; rigid fork; cockpit; three phantoms (cables,
               hardware, paint); subcontract services and a swarf recovery.  It uses the
               existing commercial catalogue, which had items and stock but no bills.
    New refs are CK-* and BK-* so the B1-B6 seed (BOM-DEMO-*) never deletes them.
    Re-runnable: identical rewrites of released lines are allowed by tr_BomReleasedImmutable.
    REQ: BOM-001, BOM-002, BOM-006 to BOM-011, BOM-017, BOM-021, BOM-026, BOM-027 */
SET NOCOUNT ON;
GO
IF OBJECT_ID('tempdb..#ScItem') IS NOT NULL DROP TABLE #ScItem;
CREATE TABLE #ScItem (ItemCode varchar(40) COLLATE DATABASE_DEFAULT PRIMARY KEY,
    Description varchar(100) COLLATE DATABASE_DEFAULT, ItemClass varchar(12) COLLATE DATABASE_DEFAULT,
    Category varchar(20) COLLATE DATABASE_DEFAULT, StockUom varchar(10) COLLATE DATABASE_DEFAULT,
    Quantum decimal(19,6), Indivisible bit, MakeBuy varchar(6) COLLATE DATABASE_DEFAULT,
    Manu bit, Stocked bit, Purch bit, Sale bit, Trace varchar(8) COLLATE DATABASE_DEFAULT, ShelfLife int,
    ValGroup varchar(20) COLLATE DATABASE_DEFAULT, Loc varchar(20) COLLATE DATABASE_DEFAULT, Lead smallint);
INSERT #ScItem VALUES
 -- the cake: finished good, sub-recipes and the decoration phantom
 ('FG-CAKE-CELEB-8IN','Celebration cake, three layer, 8 inch, vanilla and raspberry','MADE','BAKERY','EA',1,1,'MAKE',1,1,0,1,'LOT',5,'FINISHED','FG01',1),
 ('SA-CAKE-SPONGE-8IN','Sponge layer, vanilla, 8 inch, baked','MADE','BAKERY','EA',1,1,'MAKE',1,1,0,0,'LOT',3,'SUBASSY','WIP01',1),
 ('SA-CAKE-BATTER-VAN','Vanilla sponge batter, mixed','MADE','BAKERY','KG',0.001,0,'MAKE',1,1,0,0,'LOT',1,'SUBASSY','WIP01',1),
 ('SA-CAKE-BUTTERCREAM','Vanilla buttercream','MADE','BAKERY','KG',0.001,0,'MAKE',1,1,0,0,'LOT',7,'SUBASSY','WIP01',1),
 ('SA-CAKE-FONDANT','Rolled fondant icing, white','MADE','BAKERY','KG',0.001,0,'MAKE',1,1,0,0,'LOT',90,'SUBASSY','WIP01',1),
 ('SA-CAKE-SUGARROSE','Sugar paste rose, hand made, pink','MADE','BAKERY','EA',1,1,'MAKE',1,1,0,0,'LOT',180,'SUBASSY','WIP01',2),
 ('PH-CAKE-DECORATION','Phantom, celebration cake decoration set','PHANTOM','PHANTOM','SET',1,1,'MAKE',1,0,0,0,'NONE',NULL,'PHANTOM',NULL,0),
 -- the cake: bought ingredients, packaging and decoration
 ('ING-FLOUR-SR','Self raising flour','BOUGHT','INGREDIENT','KG',0.001,0,'BUY',0,1,1,0,'LOT',270,'RAWMAT','RAW02',2),
 ('ING-SUGAR-CASTER','Caster sugar, golden','BOUGHT','INGREDIENT','KG',0.001,0,'BUY',0,1,1,0,'LOT',730,'RAWMAT','RAW02',2),
 ('ING-SUGAR-ICING','Icing sugar','BOUGHT','INGREDIENT','KG',0.001,0,'BUY',0,1,1,0,'LOT',730,'RAWMAT','RAW02',2),
 ('ING-BUTTER-UNSALTED','Unsalted butter, chilled','BOUGHT','INGREDIENT','KG',0.001,0,'BUY',0,1,1,0,'LOT',60,'RAWMAT','RAW02',2),
 ('ING-EGG-FREERANGE','Free range eggs, large','BOUGHT','INGREDIENT','EA',1,1,'BUY',0,1,1,0,'LOT',28,'RAWMAT','RAW02',2),
 ('ING-MILK-WHOLE','Whole milk','BOUGHT','INGREDIENT','L',0.001,0,'BUY',0,1,1,0,'LOT',7,'RAWMAT','RAW02',1),
 ('ING-VANILLA-EXTRACT','Vanilla extract, natural','BOUGHT','INGREDIENT','L',0.001,0,'BUY',0,1,1,0,'LOT',720,'RAWMAT','RAW02',5),
 ('ING-BAKING-POWDER','Baking powder','BOUGHT','INGREDIENT','KG',0.001,0,'BUY',0,1,1,0,'LOT',365,'RAWMAT','RAW02',5),
 ('ING-JAM-RASPBERRY','Raspberry jam, seedless','BOUGHT','INGREDIENT','KG',0.001,0,'BUY',0,1,1,0,'LOT',365,'RAWMAT','RAW02',5),
 ('ING-GLUCOSE-SYRUP','Liquid glucose syrup','BOUGHT','INGREDIENT','KG',0.001,0,'BUY',0,1,1,0,'LOT',540,'RAWMAT','RAW02',5),
 ('ING-GLYCERINE','Glycerine, food grade','BOUGHT','INGREDIENT','L',0.001,0,'BUY',0,1,1,0,'LOT',720,'RAWMAT','RAW02',5),
 ('ING-GELATINE','Powdered gelatine','BOUGHT','INGREDIENT','KG',0.001,0,'BUY',0,1,1,0,'LOT',720,'RAWMAT','RAW02',5),
 ('ING-FLOWER-PASTE','Flower paste (gum paste), white','BOUGHT','INGREDIENT','KG',0.001,0,'BUY',0,1,1,0,'LOT',365,'RAWMAT','RAW02',5),
 ('ING-FOOD-COLOUR-PINK','Food colouring gel, pink','BOUGHT','INGREDIENT','L',0.001,0,'BUY',0,1,1,0,'LOT',720,'RAWMAT','RAW02',5),
 ('ING-CAKE-CRUMB','Sponge offcuts, recovered for cake pops','BOUGHT','INGREDIENT','KG',0.001,0,'BUY',0,1,0,0,'LOT',3,'RAWMAT','RAW02',0),
 ('PKG-PARCHMENT-8IN','Baking parchment disc, 8 inch','BOUGHT','CONSUMABLE','EA',1,1,'BUY',0,1,1,0,'NONE',NULL,'CONSUMABLE','RAW02',5),
 ('PKG-CAKE-BOARD-10IN','Packaging, cake drum 10 inch, silver','PACKAGING','PACKAGING','EA',1,1,'BUY',0,1,1,0,'NONE',NULL,'PACKAGING','RAW02',5),
 ('PKG-CAKE-BOX-10IN','Packaging, cake box with window, 10 inch','PACKAGING','PACKAGING','EA',1,1,'BUY',0,1,1,0,'NONE',NULL,'PACKAGING','RAW02',5),
 ('DECOR-RIBBON-15MM','Satin ribbon 15 mm, ivory','BOUGHT','DECORATION','M',0.01,0,'BUY',0,1,1,0,'NONE',NULL,'COMPONENT','RAW02',5),
 ('DECOR-TOPPER-BIRTHDAY','Cake topper, Happy Birthday, acrylic','BOUGHT','DECORATION','EA',1,1,'BUY',0,1,1,0,'NONE',NULL,'COMPONENT','RAW02',5),
 ('DECOR-CANDLE-PACK','Birthday candles, pack of twelve','BOUGHT','DECORATION','EA',1,1,'BUY',0,1,1,0,'NONE',NULL,'COMPONENT','RAW02',5),
 ('DECOR-FLORIST-WIRE','Florist wire stem, 26 gauge','BOUGHT','DECORATION','EA',1,1,'BUY',0,1,1,0,'NONE',NULL,'COMPONENT','RAW02',5),
 ('DECOR-PIPING-BAG','Disposable piping bag, 18 inch','BOUGHT','CONSUMABLE','EA',1,1,'BUY',0,1,1,0,'NONE',NULL,'CONSUMABLE','RAW02',5),
 ('SVC-DECORATING-HOUR','Service, cake decorating time','SERVICE','SERVICE','HR',0.25,0,'MAKE',0,0,0,0,'NONE',NULL,'SERVICE',NULL,0),
 ('NS-WORK-INSTRUCTION','Non-stock, work instruction placeholder, no material','NONSTOCK','NONSTOCK','EA',1,0,'BUY',0,0,0,0,'NONE',NULL,'NONSTOCK',NULL,0),
 -- the bicycle: parts the commercial catalogue did not have yet
 ('SA-COCKPIT-COMMUTE','Subassembly, commuter cockpit, flat bar','MADE','SUBASSY','EA',1,1,'MAKE',1,1,0,0,'LOT',NULL,'SUBASSY','WIP01',3),
 ('SHIFTER-11S-FLATBAR','Shifter, eleven speed, flat bar trigger','BOUGHT','DRIVETRAIN','EA',1,1,'BUY',0,1,1,0,'NONE',NULL,'COMPONENT','STKA01',10),
 ('CABLE-GEAR-INNER','Gear inner cable 2100 mm, stainless','BOUGHT','DRIVETRAIN','EA',1,1,'BUY',0,1,1,0,'NONE',NULL,'COMPONENT','STKA01',10),
 ('HOUSING-GEAR-4MM','Gear cable housing 4 mm, black','BOUGHT','DRIVETRAIN','M',0.01,0,'BUY',0,1,1,0,'NONE',NULL,'COMPONENT','STKA01',10),
 ('HOSE-BRAKE-HYD-5MM','Hydraulic brake hose 5 mm, black','BOUGHT','BRAKING','M',0.01,0,'BUY',0,1,1,0,'NONE',NULL,'COMPONENT','STKA01',10),
 ('FERRULE-4MM','Cable ferrule 4 mm, alloy','BOUGHT','FASTENER','EA',1,1,'BUY',0,1,1,0,'NONE',NULL,'HARDWARE','STKA01',10),
 ('MUDGUARD-SET-700C','Mudguard set 700C x 45 mm, with stays','BOUGHT','ACCESSORY','SET',1,1,'BUY',0,1,1,1,'NONE',NULL,'COMPONENT','STKA02',10),
 ('RACK-REAR-ALU','Rear pannier rack, alloy, 25 kg','BOUGHT','ACCESSORY','EA',1,1,'BUY',0,1,1,1,'NONE',NULL,'COMPONENT','STKA02',10),
 ('LIGHT-SET-USB','Light set, front and rear, USB rechargeable','BOUGHT','ACCESSORY','SET',1,1,'BUY',0,1,1,1,'NONE',NULL,'COMPONENT','STKA02',10),
 ('KICKSTAND-ALU','Kickstand, alloy, rear mount','BOUGHT','ACCESSORY','EA',1,1,'BUY',0,1,1,1,'NONE',NULL,'COMPONENT','STKA02',10),
 ('BELL-ALU','Bell, alloy, 22.2 mm clamp','BOUGHT','ACCESSORY','EA',1,1,'BUY',0,1,1,1,'NONE',NULL,'COMPONENT','STKA02',10),
 ('REFLECTOR-SET','Reflector set, front, rear, pedal and wheel','BOUGHT','ACCESSORY','SET',1,1,'BUY',0,1,1,1,'NONE',NULL,'COMPONENT','STKA02',10),
 ('MANUAL-OWNER-BIKE','Owner manual and declaration of conformity','BOUGHT','LITERATURE','EA',1,1,'BUY',0,1,1,0,'NONE',NULL,'COMPONENT','STKA02',10),
 ('PKG-BOX-BIKE-COMPLETE','Packaging, complete bicycle carton','PACKAGING','PACKAGING','EA',1,1,'BUY',0,1,1,0,'NONE',NULL,'PACKAGING','STKB01',10),
 ('SCRAP-AL-SWARF','Aluminium swarf and tube offcuts, recovered','BOUGHT','SCRAP','KG',0.001,0,'BUY',0,1,0,0,'NONE',NULL,'RAWMAT','SCRAP01',0);
GO
MERGE inv.Item AS t
USING (SELECT 'STC' AS CompanyCode, i.* FROM #ScItem i) AS s
ON t.CompanyCode = s.CompanyCode AND t.ItemCode = s.ItemCode
WHEN MATCHED THEN UPDATE SET Description = s.Description, ItemClass = s.ItemClass, Category = s.Category,
     StockUomCode = s.StockUom, QuantumQty = s.Quantum, IsIndivisible = s.Indivisible, MakeBuy = s.MakeBuy,
     IsManufacturable = s.Manu, IsStocked = s.Stocked, IsPurchasable = s.Purch, IsSaleable = s.Sale,
     TraceabilityType = s.Trace, ShelfLifeDays = s.ShelfLife, ValuationGroup = s.ValGroup,
     LifecycleStatus = 'RELEASED', ModifiedBy = 'ENGINEER', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (CompanyCode, ItemCode, Description, ItemClass, Category, StockUomCode, QuantumQty,
     IsIndivisible, MakeBuy, IsManufacturable, IsStocked, IsPurchasable, IsSaleable, IsConsumable,
     TraceabilityType, ShelfLifeDays, CurrentRevision, ValuationGroup, CostMethod, LifecycleStatus,
     ReleasedAt, ReleasedBy, CreatedBy)
     VALUES (s.CompanyCode, s.ItemCode, s.Description, s.ItemClass, s.Category, s.StockUom, s.Quantum,
     s.Indivisible, s.MakeBuy, s.Manu, s.Stocked, s.Purch, s.Sale, 1, s.Trace, s.ShelfLife, 'A', s.ValGroup,
     'STD', 'RELEASED', SYSDATETIME(), 'ENGMGR', 'ENGINEER');
GO
MERGE inv.ItemRevision AS t
USING (SELECT 'STC' AS CompanyCode, i.ItemCode, 'A' AS Revision FROM #ScItem i) AS s
ON t.CompanyCode = s.CompanyCode AND t.ItemCode = s.ItemCode AND t.Revision = s.Revision
WHEN NOT MATCHED THEN INSERT (CompanyCode, ItemCode, Revision, Description, EffectiveFrom, Status,
     IsInterchangeable, ApprovedBy, ApprovedAt, CreatedBy)
     VALUES (s.CompanyCode, s.ItemCode, s.Revision, 'Baseline revision for the showcase bills',
     '2026-01-01', 'RELEASED', 1, 'ENGMGR', SYSDATETIME(), 'ENGINEER');
GO
MERGE inv.ItemSite AS t
USING (SELECT 'STC' AS CompanyCode, 'RDG' AS SiteCode, i.ItemCode, i.Loc, i.Lead FROM #ScItem i) AS s
ON t.CompanyCode = s.CompanyCode AND t.SiteCode = s.SiteCode AND t.ItemCode = s.ItemCode
WHEN MATCHED THEN UPDATE SET DefaultWarehouseCode = CASE WHEN s.Loc IS NULL THEN NULL ELSE 'MAIN' END,
     DefaultLocationCode = s.Loc, LeadTimeDays = s.Lead, IsActive = 1, ModifiedBy = 'ENGINEER',
     ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (CompanyCode, SiteCode, ItemCode, DefaultWarehouseCode, DefaultLocationCode,
     LeadTimeDays, AbcClass, IsActive, CreatedBy)
     VALUES (s.CompanyCode, s.SiteCode, s.ItemCode, CASE WHEN s.Loc IS NULL THEN NULL ELSE 'MAIN' END, s.Loc,
     s.Lead, 'B', 1, 'ENGINEER');
GO
/*  The recipes are written in grams and millilitres; the stock is held in kilograms and litres.
    BOM-010 - the conversion must be on file before aggregation or costing.  Eggs come in trays. */
MERGE inv.ItemUom AS t
USING (VALUES
   ('ING-FLOUR-SR','G','ISSUE',0.001,'Recipe grams'), ('ING-SUGAR-CASTER','G','ISSUE',0.001,'Recipe grams'),
   ('ING-SUGAR-ICING','G','ISSUE',0.001,'Recipe grams'), ('ING-BUTTER-UNSALTED','G','ISSUE',0.001,'Recipe grams'),
   ('ING-BAKING-POWDER','G','ISSUE',0.001,'Recipe grams'), ('ING-JAM-RASPBERRY','G','ISSUE',0.001,'Recipe grams'),
   ('ING-GLUCOSE-SYRUP','G','ISSUE',0.001,'Recipe grams'), ('ING-GELATINE','G','ISSUE',0.001,'Recipe grams'),
   ('ING-FLOWER-PASTE','G','ISSUE',0.001,'Recipe grams'), ('SA-CAKE-BUTTERCREAM','G','ISSUE',0.001,'Recipe grams'),
   ('SA-CAKE-FONDANT','G','ISSUE',0.001,'Recipe grams'),
   ('ING-MILK-WHOLE','ML','ISSUE',0.001,'Recipe millilitres'), ('ING-VANILLA-EXTRACT','ML','ISSUE',0.001,'Recipe millilitres'),
   ('ING-GLYCERINE','ML','ISSUE',0.001,'Recipe millilitres'), ('ING-FOOD-COLOUR-PINK','ML','ISSUE',0.001,'Recipe millilitres'),
   ('ING-EGG-FREERANGE','BOX','PURCHASE',30.0,'Tray of thirty eggs')
 ) AS s (ItemCode, UomCode, UsageType, StockUnitsPerUom, PackDescription)
ON t.CompanyCode = 'STC' AND t.ItemCode = s.ItemCode AND t.UomCode = s.UomCode
   AND t.UsageType = s.UsageType AND t.EffectiveFrom = '2000-01-01'
WHEN MATCHED THEN UPDATE SET StockUnitsPerUom = s.StockUnitsPerUom, PackDescription = s.PackDescription
WHEN NOT MATCHED THEN INSERT (CompanyCode, ItemCode, UomCode, UsageType, StockUnitsPerUom, PackDescription,
     EffectiveFrom, CreatedBy)
     VALUES ('STC', s.ItemCode, s.UomCode, s.UsageType, s.StockUnitsPerUom, s.PackDescription,
     '2000-01-01', 'ENGINEER');
GO
DROP TABLE #ScItem;
GO
IF OBJECT_ID('tempdb..#ScDef') IS NOT NULL DROP TABLE #ScDef;
CREATE TABLE #ScDef (BomRef varchar(30) COLLATE DATABASE_DEFAULT PRIMARY KEY,
    ParentItem varchar(40) COLLATE DATABASE_DEFAULT, Descr varchar(150) COLLATE DATABASE_DEFAULT,
    Basis decimal(19,6), Yield decimal(9,6), Quantum decimal(19,6), BatchSize decimal(19,6), Phantom bit,
    SetupCost decimal(19,6), ConvCost decimal(19,6), LossNote varchar(200) COLLATE DATABASE_DEFAULT);
INSERT #ScDef VALUES
 ('CK-CELEB-8IN','FG-CAKE-CELEB-8IN','Celebration cake: three sponge layers, filling, fondant cover and decoration',1,1.000000,1,12,0,4.500000,6.000000,'Assembly yield is one; the fondant trimming is a component allowance'),
 ('CK-SPONGE-8IN','SA-CAKE-SPONGE-8IN','Sponge layer, six to an oven load, four per cent cracked or domed',1,0.960000,1,6,0,3.000000,0.850000,'Yield models layers lost in the oven; levelling offcuts are recovered as crumb'),
 ('CK-BATTER-VAN','SA-CAKE-BATTER-VAN','Vanilla sponge batter, recipe written per ten kilograms',10,1.000000,0.5,20,0,2.500000,0.400000,'Mixed in half kilogram steps; the bowl holds twenty kilograms'),
 ('CK-BUTTERCREAM','SA-CAKE-BUTTERCREAM','Vanilla buttercream, recipe written per five kilograms',5,1.000000,0.5,NULL,0,1.500000,0.300000,NULL),
 ('CK-FONDANT','SA-CAKE-FONDANT','Rolled fondant icing, recipe written per kilogram',1,1.000000,0.5,NULL,0,0,0.450000,NULL),
 ('CK-SUGARROSE','SA-CAKE-SUGARROSE','Sugar paste roses, recipe written per ten roses',10,0.900000,1,NULL,0,0,0.600000,'One rose in ten breaks while drying'),
 ('CK-DECORATION','PH-CAKE-DECORATION','Decoration set, phantom - its parts go straight onto the cake',1,1.000000,1,NULL,1,NULL,NULL,'Phantom grouping only'),
 ('BK-COMMUTE-M','FG-BIKE-COMMUTE-M','Commuter bicycle, medium: frame, fork, wheels, drivetrain, cockpit and accessories',1,1.000000,1,10,0,10.000000,14.000000,'Final assembly and pre-delivery inspection'),
 ('BK-FRAME-GRV-M','FG-FRAME-GRAVEL-M','Trailhead frame, medium: triangles joined, heat treated, painted and decaled',1,0.970000,1,12,0,30.000000,16.000000,'Three frames in a hundred fail the alignment check after heat treatment'),
 ('BK-FRONT-TRI-M','SA-FRAME-FRONT-TRI-M','Front triangle, medium: tubes mitred and TIG welded round headset and bottom bracket',1,1.000000,1,20,0,10.000000,8.000000,'Tube allowance covers mitre offcuts, recovered as swarf'),
 ('BK-REAR-TRI-M','SA-FRAME-REAR-TRI-M','Rear triangle, medium: stays, dropouts and brake mount',1,1.000000,1,20,0,0,6.000000,NULL),
 ('BK-HEADSET','SA-HEADSET-ASSY','Headset assembly, pressed',1,1.000000,1,NULL,0,0,0.800000,NULL),
 ('BK-BB-BSA','SA-BB-ASSY-BSA','Bottom bracket assembly, BSA threaded',1,1.000000,1,NULL,0,0,1.000000,NULL),
 ('BK-CHAINSTAY','SA-CHAINSTAY-PAIR','Chainstay pair, bent and welded',1,1.000000,1,NULL,0,0,3.000000,NULL),
 ('BK-SEATSTAY','SA-SEATSTAY-PAIR','Seatstay pair, welded with bridge',1,1.000000,1,NULL,0,0,2.500000,NULL),
 ('BK-DROPOUTS','SA-DROPOUT-SET','Dropout set, anodised outside',1,1.000000,1,NULL,0,0,0.800000,NULL),
 ('BK-FORK-RIGID','SA-FORK-RIGID-ALU','Rigid alloy fork with flat mount brake bracket',1,0.980000,1,NULL,0,0,5.000000,'Two forks in a hundred fail the rake check'),
 ('BK-WHEELSET','FG-WHEELSET-700C-DISC','Wheelset 700C disc: one front and one rear wheel',1,1.000000,1,NULL,0,0,0.500000,NULL),
 ('BK-WHEEL-FRONT','SA-WHEEL-FRONT-700C','Front wheel 700C, hand built and trued',1,0.990000,1,NULL,0,0,6.000000,NULL),
 ('BK-WHEEL-REAR','SA-WHEEL-REAR-700C','Rear wheel 700C, hand built and trued, cassette fitted',1,0.990000,1,NULL,0,0,7.000000,NULL),
 ('BK-COCKPIT','SA-COCKPIT-COMMUTE','Commuter cockpit: bar, stem, grips and shifter pre-assembled',1,1.000000,1,NULL,0,0,2.000000,NULL),
 ('BK-CABLES','PH-CABLE-SET-INT','Internal cable routing set, phantom',1,1.000000,1,NULL,1,NULL,NULL,'Phantom grouping only'),
 ('BK-HARDWARE','PH-HARDWARE-PACK-ROAD','Hardware pack, phantom: rack and mudguard fixings',1,1.000000,1,NULL,1,NULL,NULL,'Phantom grouping only'),
 ('BK-PAINT','PH-PAINT-PROCESS','Paint and finish process group, phantom',1,1.000000,1,NULL,1,NULL,NULL,'Phantom grouping only');
GO
MERGE bom.BomHeader AS t
USING (SELECT 'STC' AS CompanyCode, d.*, i.CurrentRevision FROM #ScDef d
       JOIN inv.Item i ON i.CompanyCode = 'STC' AND i.ItemCode = d.ParentItem) AS s
ON t.CompanyCode = s.CompanyCode AND t.BomRef = s.BomRef AND t.BomRevision = 'A'
WHEN MATCHED THEN UPDATE SET ParentItemCode = s.ParentItem, Description = s.Descr, NominalBasis = s.Basis,
     YieldFactor = s.Yield, ProductionQuantum = s.Quantum, BatchSize = s.BatchSize, IsPhantom = s.Phantom,
     ConsolidateRuns = 0, SetupCostPerRun = s.SetupCost, ConversionCostPerStart = s.ConvCost,
     EffectiveFrom = '2026-01-01', EffectiveTo = NULL, Status = 'RELEASED', LossIntentNote = s.LossNote,
     ParentRevision = s.CurrentRevision, Purpose = 'STANDARD', ReviewerUser = 'ENGMGR', ReleasedBy = 'ENGMGR',
     ReleasedAt = ISNULL(t.ReleasedAt, SYSDATETIME()), ModifiedBy = 'ENGINEER', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (CompanyCode, BomRef, BomRevision, ParentItemCode, ParentRevision, Purpose,
     Description, NominalBasis, YieldFactor, ProductionQuantum, BatchSize, IsPhantom, ConsolidateRuns,
     SetupCostPerRun, ConversionCostPerStart, EffectiveFrom, EffectiveTo, Status, AuthorUser, ReviewerUser,
     ReleasedBy, ReleasedAt, LossIntentNote, CreatedBy)
     VALUES (s.CompanyCode, s.BomRef, 'A', s.ParentItem, s.CurrentRevision, 'STANDARD', s.Descr, s.Basis,
     s.Yield, s.Quantum, s.BatchSize, s.Phantom, 0, s.SetupCost, s.ConvCost, '2026-01-01', NULL, 'RELEASED',
     'ENGINEER', 'ENGMGR', 'ENGMGR', SYSDATETIME(), s.LossNote, 'ENGINEER');
GO
IF OBJECT_ID('tempdb..#ScCmp') IS NOT NULL DROP TABLE #ScCmp;
CREATE TABLE #ScCmp (BomRef varchar(30) COLLATE DATABASE_DEFAULT, LineNum int,
    Item varchar(40) COLLATE DATABASE_DEFAULT, Qty decimal(19,6), Basis varchar(12) COLLATE DATABASE_DEFAULT,
    Uom varchar(10) COLLATE DATABASE_DEFAULT, Conv decimal(19,9), Scrap decimal(9,6),
    Supply varchar(10) COLLATE DATABASE_DEFAULT, CostRate decimal(19,6),
    CostElement varchar(10) COLLATE DATABASE_DEFAULT, Notes varchar(300) COLLATE DATABASE_DEFAULT,
    PRIMARY KEY (BomRef, LineNum));
INSERT #ScCmp VALUES
 ('CK-CELEB-8IN',10,'SA-CAKE-SPONGE-8IN',3,'PROPORTION','EA',1,0,'MAKE',NULL,NULL,'Three sponge layers per cake'),
 ('CK-CELEB-8IN',20,'SA-CAKE-BUTTERCREAM',450,'PROPORTION','G',0.001,0,'MAKE',NULL,NULL,'Filling between the layers and the crumb coat'),
 ('CK-CELEB-8IN',30,'ING-JAM-RASPBERRY',200,'PROPORTION','G',0.001,0,'PURCHASED',NULL,NULL,'Jam on the two lower layers'),
 ('CK-CELEB-8IN',40,'SA-CAKE-FONDANT',750,'PROPORTION','G',0.001,0.080000,'MAKE',NULL,NULL,'Covering, eight per cent trimming allowance'),
 ('CK-CELEB-8IN',50,'PH-CAKE-DECORATION',1,'PROPORTION','SET',1,0,'PHANTOM',NULL,NULL,'Decoration set goes straight onto the cake'),
 ('CK-CELEB-8IN',60,'PKG-CAKE-BOARD-10IN',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,'Cake drum'),
 ('CK-CELEB-8IN',70,'PKG-CAKE-BOX-10IN',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,'Presentation box'),
 ('CK-CELEB-8IN',80,'DECOR-PIPING-BAG',2,'FIXEDRUN','EA',1,0,'PURCHASED',NULL,NULL,'Two piping bags per decorating run, whatever the run size'),
 ('CK-CELEB-8IN',90,'NS-WORK-INSTRUCTION',0,'PROPORTION','EA',1,0,'NOTE',NULL,NULL,'Chill the filled stack for thirty minutes before covering'),
 ('CK-CELEB-8IN',100,'SVC-DECORATING-HOUR',0.75,'PROPORTION','HR',1,0,'SERVICE',18.500000,'LABOUR','Hand finishing and piping, forty five minutes per cake'),
 ('CK-SPONGE-8IN',10,'SA-CAKE-BATTER-VAN',0.65,'PROPORTION','KG',1,0,'MAKE',NULL,NULL,'Batter per tin'),
 ('CK-SPONGE-8IN',20,'PKG-PARCHMENT-8IN',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,'A parchment disc lines each tin'),
 ('CK-BATTER-VAN',10,'ING-FLOUR-SR',2600,'PROPORTION','G',0.001,0,'PURCHASED',NULL,NULL,NULL),
 ('CK-BATTER-VAN',20,'ING-SUGAR-CASTER',2600,'PROPORTION','G',0.001,0,'PURCHASED',NULL,NULL,NULL),
 ('CK-BATTER-VAN',30,'ING-BUTTER-UNSALTED',2600,'PROPORTION','G',0.001,0,'PURCHASED',NULL,NULL,'Softened to room temperature'),
 ('CK-BATTER-VAN',40,'ING-EGG-FREERANGE',44,'PROPORTION','EA',1,0.020000,'PURCHASED',NULL,NULL,'Two per cent allowance for broken eggs'),
 ('CK-BATTER-VAN',50,'ING-MILK-WHOLE',400,'PROPORTION','ML',0.001,0,'PURCHASED',NULL,NULL,NULL),
 ('CK-BATTER-VAN',60,'ING-VANILLA-EXTRACT',40,'PROPORTION','ML',0.001,0,'PURCHASED',NULL,NULL,NULL),
 ('CK-BATTER-VAN',70,'ING-BAKING-POWDER',60,'PROPORTION','G',0.001,0,'PURCHASED',NULL,NULL,NULL),
 ('CK-BUTTERCREAM',10,'ING-BUTTER-UNSALTED',2000,'PROPORTION','G',0.001,0,'PURCHASED',NULL,NULL,NULL),
 ('CK-BUTTERCREAM',20,'ING-SUGAR-ICING',3000,'PROPORTION','G',0.001,0,'PURCHASED',NULL,NULL,'Sifted'),
 ('CK-BUTTERCREAM',30,'ING-MILK-WHOLE',100,'PROPORTION','ML',0.001,0,'PURCHASED',NULL,NULL,NULL),
 ('CK-BUTTERCREAM',40,'ING-VANILLA-EXTRACT',25,'PROPORTION','ML',0.001,0,'PURCHASED',NULL,NULL,NULL),
 ('CK-FONDANT',10,'ING-SUGAR-ICING',850,'PROPORTION','G',0.001,0,'PURCHASED',NULL,NULL,NULL),
 ('CK-FONDANT',20,'ING-GLUCOSE-SYRUP',100,'PROPORTION','G',0.001,0,'PURCHASED',NULL,NULL,NULL),
 ('CK-FONDANT',30,'ING-GLYCERINE',15,'PROPORTION','ML',0.001,0,'PURCHASED',NULL,NULL,NULL),
 ('CK-FONDANT',40,'ING-GELATINE',12,'PROPORTION','G',0.001,0,'PURCHASED',NULL,NULL,NULL),
 ('CK-SUGARROSE',10,'ING-FLOWER-PASTE',150,'PROPORTION','G',0.001,0.100000,'PURCHASED',NULL,NULL,'Ten per cent rolled out and discarded'),
 ('CK-SUGARROSE',20,'ING-FOOD-COLOUR-PINK',5,'PROPORTION','ML',0.001,0,'PURCHASED',NULL,NULL,NULL),
 ('CK-SUGARROSE',30,'DECOR-FLORIST-WIRE',10,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,'One wire stem per rose'),
 ('CK-DECORATION',10,'SA-CAKE-SUGARROSE',5,'PROPORTION','EA',1,0,'MAKE',NULL,NULL,'Five roses on the top'),
 ('CK-DECORATION',20,'DECOR-RIBBON-15MM',0.85,'PROPORTION','M',1,0,'PURCHASED',NULL,NULL,'Ribbon round the cake drum'),
 ('CK-DECORATION',30,'DECOR-TOPPER-BIRTHDAY',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('CK-DECORATION',40,'DECOR-CANDLE-PACK',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL);
INSERT #ScCmp VALUES
 ('BK-COMMUTE-M',10,'FG-FRAME-GRAVEL-M',1,'PROPORTION','EA',1,0,'MAKE',NULL,NULL,'Painted frame'),
 ('BK-COMMUTE-M',20,'SA-FORK-RIGID-ALU',1,'PROPORTION','EA',1,0,'MAKE',NULL,NULL,NULL),
 ('BK-COMMUTE-M',30,'FG-WHEELSET-700C-DISC',1,'PROPORTION','SET',1,0,'MAKE',NULL,NULL,NULL),
 ('BK-COMMUTE-M',40,'TYRE-700X32C',2,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COMMUTE-M',50,'TUBE-INNER-700C',2,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COMMUTE-M',60,'SA-COCKPIT-COMMUTE',1,'PROPORTION','EA',1,0,'MAKE',NULL,NULL,NULL),
 ('BK-COMMUTE-M',70,'CRANKSET-172.5-50-34',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COMMUTE-M',80,'CHAIN-116L-11S',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COMMUTE-M',90,'LINK-QUICK-11S',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COMMUTE-M',100,'DERAILLEUR-RD-11S',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COMMUTE-M',110,'BRAKE-CALIPER-FLAT',2,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,'Front and rear'),
 ('BK-COMMUTE-M',120,'PH-CABLE-SET-INT',1,'PROPORTION','SET',1,0,'PHANTOM',NULL,NULL,NULL),
 ('BK-COMMUTE-M',130,'SADDLE-COMP-143',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COMMUTE-M',140,'SEATPOST-27.2X350',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COMMUTE-M',150,'PEDAL-FLAT-ALU',1,'PROPORTION','PR',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COMMUTE-M',160,'MUDGUARD-SET-700C',1,'PROPORTION','SET',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COMMUTE-M',170,'RACK-REAR-ALU',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COMMUTE-M',180,'LIGHT-SET-USB',1,'PROPORTION','SET',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COMMUTE-M',190,'KICKSTAND-ALU',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COMMUTE-M',200,'BELL-ALU',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COMMUTE-M',210,'REFLECTOR-SET',1,'PROPORTION','SET',1,0,'PURCHASED',NULL,NULL,'Required for sale in the UK'),
 ('BK-COMMUTE-M',220,'BOTTLE-CAGE-ALU',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COMMUTE-M',230,'PH-HARDWARE-PACK-ROAD',1,'PROPORTION','SET',1,0,'PHANTOM',NULL,NULL,NULL),
 ('BK-COMMUTE-M',240,'GREASE-LI2-400G',1,'FIXEDRUN','EA',1,0,'PURCHASED',NULL,NULL,'One cartridge per assembly run'),
 ('BK-COMMUTE-M',250,'NS-WORK-INSTRUCTION',0,'PROPORTION','EA',1,0,'NOTE',NULL,NULL,'Torque every fastener to the assembly card values'),
 ('BK-COMMUTE-M',260,'SVC-INSPECTION',1,'PROPORTION','EA',1,0,'SERVICE',8.000000,'LABOUR','Pre-delivery inspection and certificate'),
 ('BK-COMMUTE-M',270,'MANUAL-OWNER-BIKE',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COMMUTE-M',280,'PKG-BOX-BIKE-COMPLETE',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COMMUTE-M',290,'PKG-FOAM-INSERT',4,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COMMUTE-M',300,'PKG-LABEL-SHIP',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-FRAME-GRV-M',10,'SA-FRAME-FRONT-TRI-M',1,'PROPORTION','EA',1,0,'MAKE',NULL,NULL,NULL),
 ('BK-FRAME-GRV-M',20,'SA-FRAME-REAR-TRI-M',1,'PROPORTION','EA',1,0,'MAKE',NULL,NULL,NULL),
 ('BK-FRAME-GRV-M',30,'WIRE-TIG-4043-5KG',0.08,'PROPORTION','KG',1,0,'PURCHASED',NULL,NULL,'Joining the triangles'),
 ('BK-FRAME-GRV-M',40,'INSERT-M5-AL',6,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,'Bottle, rack and mudguard bosses'),
 ('BK-FRAME-GRV-M',50,'SVC-HEATTREAT-T6',1,'PROPORTION','EA',1,0,'SERVICE',6.000000,'SUBCON','T6 heat treatment, subcontract'),
 ('BK-FRAME-GRV-M',60,'PH-PAINT-PROCESS',1,'PROPORTION','EA',1,0,'PHANTOM',NULL,NULL,NULL),
 ('BK-FRAME-GRV-M',70,'DECAL-SET-GRAVEL',1,'PROPORTION','SET',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-FRONT-TRI-M',10,'TUBE-AL6061-31.8X1.6',1.45,'PROPORTION','M',1,0.060000,'PURCHASED',NULL,NULL,'Top tube and down tube'),
 ('BK-FRONT-TRI-M',20,'TUBE-AL7005-34.9X1.4',0.62,'PROPORTION','M',1,0.060000,'PURCHASED',NULL,NULL,'Seat tube and head tube'),
 ('BK-FRONT-TRI-M',30,'SA-HEADSET-ASSY',1,'PROPORTION','EA',1,0,'MAKE',NULL,NULL,NULL),
 ('BK-FRONT-TRI-M',40,'SA-BB-ASSY-BSA',1,'PROPORTION','EA',1,0,'MAKE',NULL,NULL,NULL),
 ('BK-FRONT-TRI-M',50,'WIRE-TIG-4043-5KG',0.12,'PROPORTION','KG',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-FRONT-TRI-M',60,'GAS-ARGON-50L',1,'FIXEDBATCH','EA',1,0,'PURCHASED',NULL,NULL,'One cylinder per welding batch of twenty'),
 ('BK-REAR-TRI-M',10,'SA-CHAINSTAY-PAIR',1,'PROPORTION','PR',1,0,'MAKE',NULL,NULL,NULL),
 ('BK-REAR-TRI-M',20,'SA-SEATSTAY-PAIR',1,'PROPORTION','PR',1,0,'MAKE',NULL,NULL,NULL),
 ('BK-REAR-TRI-M',30,'SA-DROPOUT-SET',1,'PROPORTION','SET',1,0,'MAKE',NULL,NULL,NULL),
 ('BK-REAR-TRI-M',40,'BRKT-MECH-DISC-FLATMOUNT-160-A',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,'Rear flat mount brake bracket'),
 ('BK-REAR-TRI-M',50,'WIRE-TIG-4043-5KG',0.06,'PROPORTION','KG',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-HEADSET',10,'HEADSET-IS42-52',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-HEADSET',20,'SCREW-M4X10-SS',2,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,'Top cap'),
 ('BK-BB-BSA',10,'BB-SHELL-BSA68',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-BB-BSA',20,'BEARING-6803-2RS',2,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,'Drive and non-drive side'),
 ('BK-CHAINSTAY',10,'TUBE-AL6061-28X1.5',0.86,'PROPORTION','M',1,0.050000,'PURCHASED',NULL,NULL,NULL),
 ('BK-CHAINSTAY',20,'WIRE-TIG-4043-5KG',0.03,'PROPORTION','KG',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-SEATSTAY',10,'TUBE-AL6061-28X1.5',1.04,'PROPORTION','M',1,0.050000,'PURCHASED',NULL,NULL,NULL),
 ('BK-SEATSTAY',20,'WIRE-TIG-4043-5KG',0.03,'PROPORTION','KG',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-DROPOUTS',10,'DROPOUT-UDH-LH',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-DROPOUTS',20,'DROPOUT-UDH-RH',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-DROPOUTS',30,'BOLT-M5X16-SS',2,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-DROPOUTS',40,'SVC-ANODISE-BLACK',1,'PROPORTION','EA',1,0,'SERVICE',2.000000,'SUBCON','Hard anodise, subcontract'),
 ('BK-FORK-RIGID',10,'TUBE-AL6061-28X1.5',0.95,'PROPORTION','M',1,0.050000,'PURCHASED',NULL,NULL,'Blades and steerer'),
 ('BK-FORK-RIGID',20,'BRKT-MECH-DISC-FLATMOUNT-160-B',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-FORK-RIGID',30,'WIRE-TIG-4043-5KG',0.04,'PROPORTION','KG',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-FORK-RIGID',40,'PAINT-2K-BLACK-5L',0.08,'PROPORTION','L',1,0.150000,'PURCHASED',NULL,NULL,NULL),
 ('BK-WHEELSET',10,'SA-WHEEL-FRONT-700C',1,'PROPORTION','EA',1,0,'MAKE',NULL,NULL,NULL),
 ('BK-WHEELSET',20,'SA-WHEEL-REAR-700C',1,'PROPORTION','EA',1,0,'MAKE',NULL,NULL,NULL),
 ('BK-WHEEL-FRONT',10,'RIM-700C-24H',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-WHEEL-FRONT',20,'HUB-FRONT-24H-DISC',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-WHEEL-FRONT',30,'SPOKE-DB-294MM',24,'PROPORTION','EA',1,0.020000,'PURCHASED',NULL,NULL,NULL),
 ('BK-WHEEL-FRONT',40,'NIPPLE-BRASS-12MM',24,'PROPORTION','EA',1,0.040000,'PURCHASED',NULL,NULL,NULL),
 ('BK-WHEEL-FRONT',50,'RIMTAPE-700C',2.2,'PROPORTION','M',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-WHEEL-FRONT',60,'BRAKE-DISC-160MM',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-WHEEL-REAR',10,'RIM-700C-24H',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-WHEEL-REAR',20,'HUB-REAR-24H-11S',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-WHEEL-REAR',30,'SPOKE-DB-294MM',24,'PROPORTION','EA',1,0.020000,'PURCHASED',NULL,NULL,NULL),
 ('BK-WHEEL-REAR',40,'NIPPLE-BRASS-12MM',24,'PROPORTION','EA',1,0.040000,'PURCHASED',NULL,NULL,NULL),
 ('BK-WHEEL-REAR',50,'RIMTAPE-700C',2.2,'PROPORTION','M',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-WHEEL-REAR',60,'BRAKE-DISC-160MM',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-WHEEL-REAR',70,'CASSETTE-11-34-11S',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COCKPIT',10,'HANDLEBAR-420-ALU',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COCKPIT',20,'STEM-100MM-6D',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COCKPIT',30,'GRIP-SET-LOCKON',1,'PROPORTION','PR',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-COCKPIT',40,'SHIFTER-11S-FLATBAR',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-CABLES',10,'CABLE-GEAR-INNER',1,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-CABLES',20,'HOUSING-GEAR-4MM',1.6,'PROPORTION','M',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-CABLES',30,'HOSE-BRAKE-HYD-5MM',2.6,'PROPORTION','M',1,0,'PURCHASED',NULL,NULL,'Front and rear brake hoses'),
 ('BK-CABLES',40,'FERRULE-4MM',4,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-HARDWARE',10,'BOLT-M5X16-SS',4,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-HARDWARE',20,'BOLT-M6X20-TI',2,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-HARDWARE',30,'WASHER-M6-SS',4,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-HARDWARE',40,'NUT-M6-NYLOC',2,'PROPORTION','EA',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-PAINT',10,'PRIMER-PR10-1L',0.12,'PROPORTION','L',1,0,'PURCHASED',NULL,NULL,NULL),
 ('BK-PAINT',20,'PAINT-2K-BLACK-5L',0.30,'PROPORTION','L',1,0.150000,'PURCHASED',NULL,NULL,'Fifteen per cent overspray');
GO
MERGE bom.BomComponent AS t
USING (SELECT h.BomHeaderId, c.* FROM #ScCmp c
       JOIN bom.BomHeader h ON h.CompanyCode = 'STC' AND h.BomRef = c.BomRef AND h.BomRevision = 'A') AS s
ON t.BomHeaderId = s.BomHeaderId AND t.LineNum = s.LineNum
WHEN MATCHED THEN UPDATE SET ComponentItemCode = s.Item, UsageQty = s.Qty, UsageBasis = s.Basis,
     UomCode = s.Uom, ConversionFactor = s.Conv, ScrapPct = s.Scrap, SupplyBehaviour = s.Supply,
     IsOptional = 0, AlternateGroup = NULL, IsSelected = 1, CostOnlyRate = s.CostRate,
     CostElementCode = s.CostElement, Notes = s.Notes, ComponentRevision = NULL,
     PathRef = CONCAT(s.BomRef, '/', s.LineNum), ModifiedBy = 'ENGINEER', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (BomHeaderId, LineNum, PathRef, ComponentItemCode, ComponentRevision, UsageQty,
     UsageBasis, UomCode, ConversionFactor, ScrapPct, SupplyBehaviour, IsOptional, AlternateGroup, IsSelected,
     CostOnlyRate, CostElementCode, Notes, CreatedBy)
     VALUES (s.BomHeaderId, s.LineNum, CONCAT(s.BomRef, '/', s.LineNum), s.Item, NULL, s.Qty, s.Basis, s.Uom,
     s.Conv, s.Scrap, s.Supply, 0, NULL, 1, s.CostRate, s.CostElement, s.Notes, 'ENGINEER');
GO
/*  BOM-026: recoveries are positive expected outputs with an approved valuation.  */
MERGE bom.BomByProduct AS t
USING (SELECT h.BomHeaderId, v.LineNum, v.Item, v.Qty, v.Uom, v.Value, v.DispRule
       FROM (VALUES ('CK-SPONGE-8IN', 10, 'ING-CAKE-CRUMB', 0.040, 'KG', 1.200000, 'Levelling offcuts sold to the cafe as cake pop crumb'),
                    ('BK-FRONT-TRI-M', 10, 'SCRAP-AL-SWARF', 0.150, 'KG', 0.900000, 'Collected by the metals merchant monthly'))
            v(BomRef, LineNum, Item, Qty, Uom, Value, DispRule)
       JOIN bom.BomHeader h ON h.CompanyCode = 'STC' AND h.BomRef = v.BomRef AND h.BomRevision = 'A') AS s
ON t.BomHeaderId = s.BomHeaderId AND t.LineNum = s.LineNum
WHEN MATCHED THEN UPDATE SET ByProductItemCode = s.Item, RecoveryQty = s.Qty, UomCode = s.Uom,
     ValuationBasis = 'NRV', RecoveryValue = s.Value, DispositionRule = s.DispRule, ApprovedBy = 'COSTACC'
WHEN NOT MATCHED THEN INSERT (BomHeaderId, LineNum, ByProductItemCode, RecoveryQty, UomCode, ConversionFactor,
     ValuationBasis, RecoveryValue, DispositionRule, ApprovedBy)
     VALUES (s.BomHeaderId, s.LineNum, s.Item, s.Qty, s.Uom, 1, 'NRV', s.Value, s.DispRule, 'COSTACC');
GO
DROP TABLE #ScDef;
DROP TABLE #ScCmp;
GO
/*  Purchased leaf rates in the BOMDEMO cost set - the set the explosion enquiry opens with - so
    the costed explosion of both showcases is COMPLETE.  Made items are rolled up, never priced
    here, and cst.ItemCost is left alone so stock valuation and the control accounts do not move. */
MERGE cst.StandardCost AS t
USING (VALUES
   ('ING-FLOUR-SR',0.950000), ('ING-SUGAR-CASTER',1.450000), ('ING-SUGAR-ICING',1.600000),
   ('ING-BUTTER-UNSALTED',7.200000), ('ING-EGG-FREERANGE',0.320000), ('ING-MILK-WHOLE',1.100000),
   ('ING-VANILLA-EXTRACT',95.000000), ('ING-BAKING-POWDER',4.800000), ('ING-JAM-RASPBERRY',4.200000),
   ('ING-GLUCOSE-SYRUP',3.400000), ('ING-GLYCERINE',9.500000), ('ING-GELATINE',28.000000),
   ('ING-FLOWER-PASTE',12.500000), ('ING-FOOD-COLOUR-PINK',110.000000), ('PKG-PARCHMENT-8IN',0.060000),
   ('PKG-CAKE-BOARD-10IN',1.350000), ('PKG-CAKE-BOX-10IN',1.100000), ('DECOR-RIBBON-15MM',0.450000),
   ('DECOR-TOPPER-BIRTHDAY',2.750000), ('DECOR-CANDLE-PACK',0.850000), ('DECOR-FLORIST-WIRE',0.040000),
   ('DECOR-PIPING-BAG',0.120000),
   ('TUBE-AL6061-31.8X1.6',9.200000), ('TUBE-AL7005-34.9X1.4',11.400000), ('TUBE-AL6061-28X1.5',7.100000),
   ('WIRE-TIG-4043-5KG',16.500000), ('GAS-ARGON-50L',42.000000), ('HEADSET-IS42-52',9.500000),
   ('SCREW-M4X10-SS',0.060000), ('BB-SHELL-BSA68',11.200000), ('BEARING-6803-2RS',2.400000),
   ('DROPOUT-UDH-LH',4.100000), ('DROPOUT-UDH-RH',6.300000), ('BOLT-M5X16-SS',0.090000),
   ('BRKT-MECH-DISC-FLATMOUNT-160-A',5.800000), ('BRKT-MECH-DISC-FLATMOUNT-160-B',5.800000),
   ('PRIMER-PR10-1L',14.000000), ('PAINT-2K-BLACK-5L',22.000000), ('DECAL-SET-GRAVEL',3.600000),
   ('INSERT-M5-AL',0.140000), ('RIM-700C-24H',12.500000), ('HUB-FRONT-24H-DISC',14.000000),
   ('HUB-REAR-24H-11S',28.000000), ('SPOKE-DB-294MM',0.240000), ('NIPPLE-BRASS-12MM',0.070000),
   ('RIMTAPE-700C',0.550000), ('BRAKE-DISC-160MM',5.600000), ('CASSETTE-11-34-11S',17.000000),
   ('TYRE-700X32C',8.400000), ('TUBE-INNER-700C',1.900000), ('HANDLEBAR-420-ALU',6.900000),
   ('STEM-100MM-6D',5.400000), ('GRIP-SET-LOCKON',3.100000), ('SHIFTER-11S-FLATBAR',13.000000),
   ('CRANKSET-172.5-50-34',32.000000), ('CHAIN-116L-11S',8.600000), ('LINK-QUICK-11S',2.100000),
   ('DERAILLEUR-RD-11S',21.000000), ('BRAKE-CALIPER-FLAT',16.000000), ('CABLE-GEAR-INNER',0.950000),
   ('HOUSING-GEAR-4MM',1.200000), ('HOSE-BRAKE-HYD-5MM',2.400000), ('FERRULE-4MM',0.050000),
   ('SADDLE-COMP-143',7.200000), ('SEATPOST-27.2X350',5.100000), ('PEDAL-FLAT-ALU',4.600000),
   ('MUDGUARD-SET-700C',6.200000), ('RACK-REAR-ALU',9.600000), ('LIGHT-SET-USB',7.400000),
   ('KICKSTAND-ALU',5.300000), ('BELL-ALU',1.100000), ('REFLECTOR-SET',1.400000),
   ('BOTTLE-CAGE-ALU',2.600000), ('BOLT-M6X20-TI',1.150000), ('WASHER-M6-SS',0.030000),
   ('NUT-M6-NYLOC',0.050000), ('MANUAL-OWNER-BIKE',0.900000), ('PKG-BOX-BIKE-COMPLETE',4.800000),
   ('PKG-FOAM-INSERT',0.650000), ('PKG-LABEL-SHIP',0.040000), ('GREASE-LI2-400G',6.800000)
 ) AS s (ItemCode, UnitCost)
ON t.CompanyCode = 'STC' AND t.CostSetCode = 'BOMDEMO' AND t.ItemCode = s.ItemCode AND t.Revision = ''
WHEN MATCHED THEN UPDATE SET TotalUnitCost = s.UnitCost, Completeness = 'COMPLETE'
WHEN NOT MATCHED THEN INSERT (CompanyCode, CostSetCode, ItemCode, Revision, CostingLotSize, TotalUnitCost,
     Completeness) VALUES ('STC', 'BOMDEMO', s.ItemCode, '', 1, s.UnitCost, 'COMPLETE');
GO
