/*  Item master, part B: wheel parts, manufactured subassemblies and finished goods, commercial
    kits, phantoms, packaging, services, non-stock and cost-only items, plus the Caledon
    Assemblies range.  Covers every item classification used by the modules.
    REQ: ITM-001 to ITM-013, ITM-020, ITM-021, KIT-001, D-11, D-21 */
SET NOCOUNT ON;
GO
DECLARE @i TABLE (ItemCode varchar(40), Description varchar(100), ItemClass varchar(10), Category varchar(30),
                  StockUom varchar(10), PurchUom varchar(10), SalesUom varchar(10), Quantum decimal(19,6), Indivisible bit,
                  WeightKg decimal(19,6), Rev varchar(4), Trace varchar(8), ShelfLife int, Inspect bit, MakeBuy varchar(6),
                  ValGroup varchar(20), CostMethod varchar(6), BatchQty decimal(19,6), Purch bit, Sale bit, Manu bit,
                  Stocked bit, Life varchar(10), Retention varchar(20), Successor varchar(40));
INSERT @i VALUES
 ('SPOKE-DB-294MM','Double butted spoke 294 mm, stainless','BOUGHT','WHEEL','EA','PK100',NULL,1.0,1,0.006,'A','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7',NULL),
 ('NIPPLE-BRASS-12MM','Spoke nipple 12 mm, brass','BOUGHT','WHEEL','EA','PK500',NULL,1.0,1,0.001,'A','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7',NULL),
 ('RIM-700C-24H','Rim 700C, 24 hole, tubeless ready','BOUGHT','WHEEL','EA','EA','EA',1.0,1,0.465,'B','LOT',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7',NULL),
 ('HUB-FRONT-24H-DISC','Front hub 24 hole, centre lock disc','BOUGHT','WHEEL','EA','EA','EA',1.0,1,0.142,'A','LOT',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7',NULL),
 ('HUB-REAR-24H-11S','Rear hub 24 hole, eleven speed disc','BOUGHT','WHEEL','EA','EA','EA',1.0,1,0.298,'A','LOT',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7',NULL),
 ('RIMTAPE-700C','Tubeless rim tape 700C, 25 mm','BOUGHT','WHEEL','M','M',NULL,0.001,0,0.004,'A','LOT',1095,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7',NULL),
 ('BOTTLE-CAGE-ALU','Bottle cage, alloy','BOUGHT','FINISHING','EA','EA','EA',1.0,1,0.052,'A','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7',NULL),
 ('PEDAL-FLAT-ALU','Flat pedal set, alloy','BOUGHT','FINISHING','PR','PR','PR',1.0,1,0.360,'A','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7',NULL),
 ('DECAL-SET-ROAD','Decal set, road frame','BOUGHT','FINISHING','SET','SET','SET',1.0,1,0.020,'C','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7',NULL),
 ('DECAL-SET-GRAVEL','Decal set, gravel frame','BOUGHT','FINISHING','SET','SET','SET',1.0,1,0.020,'B','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7',NULL),
 ('BEARING-6802-2RS','Sealed bearing 6802-2RS, superseded','BOUGHT','BEARING','EA','PK10',NULL,1.0,1,0.010,'A','LOT',NULL,0,'BUY','COMPONENT','MWA',NULL,1,0,0,1,'OBSOLETE','FIN7','BEARING-6803-2RS'),
 ('TUBE-AL6061-28X1.2','Aluminium tube 6061-T6, 28.0 x 1.2 mm, withdrawn','BOUGHT','TUBING','M','M',NULL,0.001,0,0.280,'A','LOT',NULL,0,'BUY','RAWMAT','MWA',NULL,0,0,0,1,'RETIRED','FIN7','TUBE-AL6061-28X1.5'),
 ('SA-CHAINSTAY-PAIR','Subassembly, chainstay pair, welded','MADE','SUBASSY','PR',NULL,NULL,1.0,1,0.612,'C','LOT',NULL,0,'MAKE','SUBASSY','STD',20,0,0,1,1,'RELEASED','FIN7',NULL),
 ('SA-SEATSTAY-PAIR','Subassembly, seatstay pair, welded','MADE','SUBASSY','PR',NULL,NULL,1.0,1,0.448,'C','LOT',NULL,0,'MAKE','SUBASSY','STD',20,0,0,1,1,'RELEASED','FIN7',NULL),
 ('SA-FRAME-FRONT-TRI-M','Subassembly, front triangle, medium','MADE','SUBASSY','EA',NULL,NULL,1.0,1,0.985,'D','LOT',NULL,1,'MAKE','SUBASSY','STD',10,0,0,1,1,'RELEASED','FIN7',NULL),
 ('SA-FRAME-REAR-TRI-M','Subassembly, rear triangle, medium','MADE','SUBASSY','EA',NULL,NULL,1.0,1,0.742,'C','LOT',NULL,1,'MAKE','SUBASSY','STD',10,0,0,1,1,'RELEASED','FIN7',NULL),
 ('SA-FORK-RIGID-ALU','Subassembly, rigid fork, alloy','MADE','SUBASSY','EA',NULL,'EA',1.0,1,0.685,'B','LOT',NULL,0,'MAKE','SUBASSY','STD',15,0,1,1,1,'RELEASED','FIN7',NULL),
 ('SA-WHEEL-FRONT-700C','Subassembly, front wheel 700C built','MADE','SUBASSY','EA',NULL,'EA',1.0,1,0.845,'B','LOT',NULL,0,'MAKE','SUBASSY','STD',12,0,1,1,1,'RELEASED','FIN7',NULL),
 ('SA-WHEEL-REAR-700C','Subassembly, rear wheel 700C built','MADE','SUBASSY','EA',NULL,'EA',1.0,1,1.012,'B','LOT',NULL,0,'MAKE','SUBASSY','STD',12,0,1,1,1,'RELEASED','FIN7',NULL),
 ('SA-BB-ASSY-BSA','Subassembly, bottom bracket assembly BSA','MADE','SUBASSY','EA',NULL,NULL,1.0,1,0.235,'A','NONE',NULL,0,'MAKE','SUBASSY','STD',25,0,0,1,1,'RELEASED','FIN7',NULL),
 ('SA-HEADSET-ASSY','Subassembly, headset assembly pressed','MADE','SUBASSY','EA',NULL,NULL,1.0,1,0.148,'A','NONE',NULL,0,'MAKE','SUBASSY','STD',25,0,0,1,1,'RELEASED','FIN7',NULL),
 ('SA-DROPOUT-SET','Subassembly, machined dropout set','MADE','SUBASSY','SET',NULL,NULL,1.0,1,0.112,'B','LOT',NULL,0,'MAKE','SUBASSY','STD',40,0,0,1,1,'RELEASED','FIN7',NULL),
 ('SA-CABLE-LOOM-EBIKE','Subassembly, electric cable loom','MADE','SUBASSY','EA',NULL,NULL,1.0,1,0.185,'B','SERIAL',NULL,1,'MAKE','SUBASSY','STD',10,0,0,1,1,'RELEASED','TRACE11',NULL),
 ('SA-BRAKE-MOUNT-PM','Subassembly, post mount brake bracket','MADE','SUBASSY','EA',NULL,NULL,1.0,1,0.064,'A','NONE',NULL,0,'MAKE','SUBASSY','STD',50,0,0,1,1,'RELEASED','FIN7',NULL),
 ('FG-FRAME-ROAD-SL-52','Frame, road Superlight, 52 cm','MADE','FRAME','EA',NULL,'EA',1.0,1,1.285,'D','LOT',NULL,1,'MAKE','FINISHED','STD',8,0,1,1,1,'RELEASED','TRACE11',NULL),
 ('FG-FRAME-ROAD-SL-54','Frame, road Superlight, 54 cm','MADE','FRAME','EA',NULL,'EA',1.0,1,1.312,'D','LOT',NULL,1,'MAKE','FINISHED','STD',8,0,1,1,1,'RELEASED','TRACE11',NULL),
 ('FG-FRAME-ROAD-SL-56','Frame, road Superlight, 56 cm','MADE','FRAME','EA',NULL,'EA',1.0,1,1.348,'D','LOT',NULL,1,'MAKE','FINISHED','STD',8,0,1,1,1,'RELEASED','TRACE11',NULL),
 ('FG-FRAME-GRAVEL-M','Frame, gravel Trailhead, medium','MADE','FRAME','EA',NULL,'EA',1.0,1,1.485,'C','LOT',NULL,1,'MAKE','FINISHED','STD',8,0,1,1,1,'RELEASED','TRACE11',NULL),
 ('FG-FRAME-GRAVEL-L','Frame, gravel Trailhead, large','MADE','FRAME','EA',NULL,'EA',1.0,1,1.524,'C','LOT',NULL,1,'MAKE','FINISHED','STD',8,0,1,1,1,'RELEASED','TRACE11',NULL),
 ('FG-FRAME-MTB-HT-M','Frame, hardtail mountain, medium','MADE','FRAME','EA',NULL,'EA',1.0,1,1.845,'B','LOT',NULL,1,'MAKE','FINISHED','STD',6,0,1,1,1,'RELEASED','TRACE11',NULL),
 ('FG-FRAME-EBIKE-CITY-M','Frame, electric city bicycle, medium','MADE','FRAME','EA',NULL,'EA',1.0,1,2.640,'B','SERIAL',NULL,1,'MAKE','FINISHED','STD',5,0,1,1,1,'RELEASED','TRACE11',NULL),
 ('FG-FRAME-TRACK-54','Frame, track, 54 cm, withdrawn from sale','MADE','FRAME','EA',NULL,'EA',1.0,1,1.210,'B','LOT',NULL,1,'MAKE','FINISHED','STD',6,0,0,1,1,'OBSOLETE','TRACE11','FG-FRAME-ROAD-SL-54'),
 ('FG-FRAME-ROAD-CL-54','Frame, road Classic, 54 cm, in development','MADE','FRAME','EA',NULL,'EA',1.0,1,1.420,'A','LOT',NULL,1,'MAKE','FINISHED','STD',8,0,0,1,1,'DRAFT','TRACE11',NULL),
 ('FG-FORK-CARBON-700C','Fork, carbon 700C, tapered','MADE','FRAME','EA',NULL,'EA',1.0,1,0.410,'C','LOT',NULL,1,'DUAL','FINISHED','STD',10,1,1,1,1,'RELEASED','TRACE11',NULL),
 ('FG-WHEELSET-700C-DISC','Wheelset, 700C disc, hand built','MADE','WHEEL','SET',NULL,'SET',1.0,1,1.857,'B','LOT',NULL,0,'MAKE','FINISHED','STD',6,0,1,1,1,'RELEASED','FIN7',NULL),
 ('FG-BIKE-COMMUTE-M','Complete bicycle, commuter, medium','MADE','BICYCLE','EA',NULL,'EA',1.0,1,11.240,'A','SERIAL',NULL,1,'MAKE','FINISHED','STD',4,0,1,1,1,'RELEASED','TRACE11',NULL),
 ('KIT-BUILD-ROAD-52','Kit, road build pack, 52 cm frame','KIT','KIT','EA',NULL,'EA',1.0,1,8.450,'B','NONE',NULL,0,'MAKE','FINISHED','STD',1,0,1,1,0,'RELEASED','FIN7',NULL),
 ('KIT-BUILD-GRAVEL-M','Kit, gravel build pack, medium frame','KIT','KIT','EA',NULL,'EA',1.0,1,9.120,'A','NONE',NULL,0,'MAKE','FINISHED','STD',1,0,1,1,0,'RELEASED','FIN7',NULL),
 ('KIT-SPARES-DEALER-A','Kit, dealer spares pack A','KIT','KIT','EA',NULL,'EA',1.0,1,3.280,'A','NONE',NULL,0,'MAKE','FINISHED','STD',1,0,1,1,0,'RELEASED','FIN7',NULL),
 ('KIT-EBIKE-CONV-250W','Kit, electric conversion 250 W','KIT','KIT','EA',NULL,'EA',1.0,1,6.720,'A','SERIAL',NULL,1,'MAKE','FINISHED','STD',1,0,1,1,0,'RELEASED','TRACE11',NULL),
 ('PH-HARDWARE-PACK-ROAD','Phantom, road frame hardware pack','PHANTOM','PHANTOM','SET',NULL,NULL,1.0,1,0.085,'A','NONE',NULL,0,'MAKE','PHANTOM','STD',1,0,0,1,0,'RELEASED','FIN7',NULL),
 ('PH-CABLE-SET-INT','Phantom, internal cable routing set','PHANTOM','PHANTOM','SET',NULL,NULL,1.0,1,0.095,'A','NONE',NULL,0,'MAKE','PHANTOM','STD',1,0,0,1,0,'RELEASED','FIN7',NULL),
 ('PH-PAINT-PROCESS','Phantom, paint and finish process group','PHANTOM','PHANTOM','EA',NULL,NULL,1.0,1,0.000,'A','NONE',NULL,0,'MAKE','PHANTOM','STD',1,0,0,1,0,'RELEASED','FIN7',NULL),
 ('PKG-BOX-FRAME-L','Packaging, frame carton large','PACKAGING','PACKAGING','EA','PK10',NULL,1.0,1,1.240,'A','NONE',NULL,0,'BUY','PACKAGING','MWA',NULL,1,0,0,1,'RELEASED','FIN7',NULL),
 ('PKG-BOX-FRAME-M','Packaging, frame carton medium','PACKAGING','PACKAGING','EA','PK10',NULL,1.0,1,1.080,'A','NONE',NULL,0,'BUY','PACKAGING','MWA',NULL,1,0,0,1,'RELEASED','FIN7',NULL),
 ('PKG-FOAM-INSERT','Packaging, moulded foam insert','PACKAGING','PACKAGING','EA','PK100',NULL,1.0,1,0.085,'A','NONE',NULL,0,'BUY','PACKAGING','MWA',NULL,1,0,0,1,'RELEASED','FIN7',NULL),
 ('PKG-PALLET-EUR','Packaging, euro pallet 1200 x 800','PACKAGING','PACKAGING','EA','EA',NULL,1.0,1,22.000,'A','NONE',NULL,0,'BUY','PACKAGING','MWA',NULL,1,0,0,1,'RELEASED','FIN7',NULL),
 ('PKG-STRETCHWRAP-500','Packaging, stretch wrap 500 mm roll','PACKAGING','PACKAGING','EA','BOX',NULL,1.0,1,2.400,'A','NONE',NULL,0,'BUY','PACKAGING','MWA',NULL,1,0,0,1,'RELEASED','FIN7',NULL),
 ('PKG-LABEL-SHIP','Packaging, shipping label A6','PACKAGING','PACKAGING','EA','PK500',NULL,1.0,1,0.002,'A','NONE',NULL,0,'BUY','PACKAGING','MWA',NULL,1,0,0,1,'RELEASED','FIN7',NULL),
 ('SVC-ANODISE-BLACK','Service, hard anodise black, subcontract','SERVICE','SERVICE','EA','EA',NULL,1.0,1,NULL,'A','NONE',NULL,0,'BUY','SERVICE','MWA',NULL,1,0,0,0,'RELEASED','FIN7',NULL),
 ('SVC-POWDERCOAT','Service, powder coat, subcontract','SERVICE','SERVICE','EA','EA',NULL,1.0,1,NULL,'A','NONE',NULL,0,'BUY','SERVICE','MWA',NULL,1,0,0,0,'RELEASED','FIN7',NULL),
 ('SVC-HEATTREAT-T6','Service, T6 heat treatment, subcontract','SERVICE','SERVICE','EA','EA',NULL,1.0,1,NULL,'A','NONE',NULL,0,'BUY','SERVICE','MWA',NULL,1,0,0,0,'RELEASED','FIN7',NULL),
 ('SVC-CARRIAGE-UK','Service, carriage within the United Kingdom','SERVICE','SERVICE','EA','EA','EA',1.0,1,NULL,'A','NONE',NULL,0,'DUAL','SERVICE','MWA',NULL,1,1,0,0,'RELEASED','FIN7',NULL),
 ('SVC-CARRIAGE-EXPORT','Service, export carriage and documentation','SERVICE','SERVICE','EA','EA','EA',1.0,1,NULL,'A','NONE',NULL,0,'DUAL','SERVICE','MWA',NULL,1,1,0,0,'RELEASED','FIN7',NULL),
 ('SVC-DESIGN-HOUR','Service, design and engineering time','SERVICE','SERVICE','HR',NULL,'HR',0.25,0,NULL,'A','NONE',NULL,0,'MAKE','SERVICE','STD',NULL,0,1,0,0,'RELEASED','FIN7',NULL),
 ('SVC-ASSEMBLY-HOUR','Service, assembly labour time','SERVICE','SERVICE','HR',NULL,'HR',0.25,0,NULL,'A','NONE',NULL,0,'MAKE','SERVICE','STD',NULL,0,1,0,0,'RELEASED','FIN7',NULL),
 ('SVC-INSPECTION','Service, inspection and certification','SERVICE','SERVICE','EA','EA','EA',1.0,1,NULL,'A','NONE',NULL,0,'DUAL','SERVICE','MWA',NULL,1,1,0,0,'RELEASED','FIN7',NULL),
 ('NS-CATALOGUE-BOLT-KIT','Non-stock, catalogue bolt kit bought to order','NONSTOCK','NONSTOCK','EA','EA','EA',1.0,1,0.240,'A','NONE',NULL,0,'BUY','NONSTOCK','MWA',NULL,1,1,0,0,'RELEASED','FIN7',NULL),
 ('NS-TOOLING-JIG','Non-stock, welding jig bought to order','NONSTOCK','NONSTOCK','EA','EA',NULL,1.0,1,18.500,'A','NONE',NULL,0,'BUY','NONSTOCK','MWA',NULL,1,0,0,0,'RELEASED','FIN7',NULL),
 ('NS-SPARE-DECAL-SET','Non-stock, custom decal set','NONSTOCK','NONSTOCK','SET','SET','SET',1.0,1,0.020,'A','NONE',NULL,0,'BUY','NONSTOCK','MWA',NULL,1,1,0,0,'RELEASED','FIN7',NULL),
 ('NS-WORKSHOP-CONSUM','Non-stock, workshop consumables','NONSTOCK','NONSTOCK','EA','EA',NULL,1.0,1,NULL,'A','NONE',NULL,0,'BUY','NONSTOCK','MWA',NULL,1,0,0,0,'RELEASED','FIN7',NULL),
 ('CO-FREIGHT-INWARD','Cost only, inward freight for landed cost','COSTONLY','LANDED','EA','EA',NULL,1.0,1,NULL,'A','NONE',NULL,0,'BUY','LANDED','MWA',NULL,1,0,0,0,'RELEASED','FIN7',NULL),
 ('CO-DUTY-IMPORT','Cost only, import duty for landed cost','COSTONLY','LANDED','EA','EA',NULL,1.0,1,NULL,'A','NONE',NULL,0,'BUY','LANDED','MWA',NULL,1,0,0,0,'RELEASED','FIN7',NULL),
 ('CO-INSURANCE-FREIGHT','Cost only, freight insurance for landed cost','COSTONLY','LANDED','EA','EA',NULL,1.0,1,NULL,'A','NONE',NULL,0,'BUY','LANDED','MWA',NULL,1,0,0,0,'RELEASED','FIN7',NULL);
INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, Category, ProductGroup, StewardUser, StockUomCode, PurchaseUomCode, SalesUomCode,
                 QuantumQty, IsIndivisible, NetWeightKg, CurrentRevision, TraceabilityType, ShelfLifeDays, RequiresInspection,
                 MakeBuy, ValuationGroup, CostMethod, StandardBatchQty, IsPurchasable, IsSaleable, IsManufacturable, IsStocked,
                 LifecycleStatus, RetentionClass, SuccessorItemCode, ReleasedAt, ReleasedBy, CreatedBy)
SELECT 'STC', i.ItemCode, i.Description, i.ItemClass, i.Category, i.Category, 'ENGINEER', i.StockUom, i.PurchUom, i.SalesUom,
       i.Quantum, i.Indivisible, i.WeightKg, i.Rev, i.Trace, i.ShelfLife, i.Inspect,
       i.MakeBuy, i.ValGroup, i.CostMethod, i.BatchQty, i.Purch, i.Sale, i.Manu, i.Stocked,
       i.Life, i.Retention, i.Successor,
       CASE WHEN i.Life IN ('RELEASED','OBSOLETE','RETIRED') THEN CAST('2025-04-01T09:00:00' AS datetime2(3)) END,
       CASE WHEN i.Life IN ('RELEASED','OBSOLETE','RETIRED') THEN 'ENGMGR' END, 'SEED'
  FROM @i i
 WHERE NOT EXISTS (SELECT 1 FROM inv.Item x WHERE x.CompanyCode = 'STC' AND x.ItemCode = i.ItemCode);
GO
/*  ITM-020 - a withdrawal is recorded with the reason it was withdrawn, not only the successor,
    so the engineering change can be explained long after the event. */
UPDATE inv.Item
   SET StatusReason = v.Reason, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
  FROM inv.Item i
  JOIN (VALUES
   ('BEARING-6802-2RS','Superseded by the higher load rating 6803-2RS; no further purchases'),
   ('TUBE-AL6061-28X1.2','Wall thickness withdrawn after the 2025 frame fatigue review'),
   ('FG-FRAME-TRACK-54','Track frame withdrawn from sale at the end of the 2026 range')
  ) AS v (ItemCode, Reason) ON v.ItemCode = i.ItemCode
 WHERE i.CompanyCode = 'STC' AND i.LifecycleStatus IN ('OBSOLETE','RETIRED')
   AND ISNULL(i.StatusReason, '') = '';
GO
/*  Caledon Assemblies buys frames and components from the group range and assembles bicycles. */
INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, Category, ProductGroup, StewardUser, StockUomCode, PurchaseUomCode, SalesUomCode,
                 QuantumQty, IsIndivisible, NetWeightKg, CurrentRevision, TraceabilityType, RequiresInspection,
                 MakeBuy, ValuationGroup, CostMethod, StandardBatchQty, IsPurchasable, IsSaleable, IsManufacturable, IsStocked,
                 LifecycleStatus, RetentionClass, ReleasedAt, ReleasedBy, CreatedBy)
SELECT 'CAL', v.ItemCode, v.Description, v.ItemClass, v.Category, v.Category, 'ENGINEER', v.StockUom, v.PurchUom, v.SalesUom,
       1.0, 1, v.WeightKg, 'A', v.Trace, v.Inspect, v.MakeBuy, v.ValGroup, v.CostMethod, v.BatchQty,
       v.Purch, v.Sale, v.Manu, v.Stocked, 'RELEASED', v.Retention, '2025-04-01T09:00:00', 'ENGMGR', 'SEED'
  FROM (VALUES
   ('FG-FRAME-ROAD-SL-54','Frame, road Superlight, 54 cm, transferred in','BOUGHT','FRAME','EA','EA','EA',1.312,'LOT',1,'BUY','FINISHED','MWA',NULL,1,1,0,1,'TRACE11'),
   ('FG-FRAME-GRAVEL-M','Frame, gravel Trailhead, medium, transferred in','BOUGHT','FRAME','EA','EA','EA',1.485,'LOT',1,'BUY','FINISHED','MWA',NULL,1,1,0,1,'TRACE11'),
   ('FG-FORK-CARBON-700C','Fork, carbon 700C, tapered','BOUGHT','FRAME','EA','EA','EA',0.410,'LOT',1,'BUY','FINISHED','MWA',NULL,1,1,0,1,'TRACE11'),
   ('FG-WHEELSET-700C-DISC','Wheelset, 700C disc','BOUGHT','WHEEL','SET','SET','SET',1.857,'LOT',0,'BUY','FINISHED','MWA',NULL,1,1,0,1,'FIN7'),
   ('CHAIN-116L-11S','Chain 116 links, eleven speed','BOUGHT','DRIVETRAIN','EA','EA','EA',0.252,'NONE',0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'FIN7'),
   ('CASSETTE-11-34-11S','Cassette 11-34, eleven speed','BOUGHT','DRIVETRAIN','EA','EA','EA',0.345,'NONE',0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'FIN7'),
   ('SADDLE-COMP-143','Saddle, composite rail, 143 mm','BOUGHT','FINISHING','EA','EA','EA',0.228,'NONE',0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'FIN7'),
   ('TYRE-700X32C','Tyre 700 x 32C, folding','BOUGHT','WHEEL','EA','EA','EA',0.345,'LOT',0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'FIN7'),
   ('BATTERY-36V-14AH','Lithium ion battery pack 36 V 14 Ah','BOUGHT','ELECTRONIC','EA','EA','EA',3.200,'SERIAL',1,'BUY','ELECTRONIC','FIFO',NULL,1,1,0,1,'TRACE11'),
   ('CAL-BIKE-ROAD-54','Complete bicycle, road, 54 cm','MADE','BICYCLE','EA',NULL,'EA',8.640,'SERIAL',1,'MAKE','FINISHED','STD',4,0,1,1,1,'TRACE11'),
   ('CAL-BIKE-GRAVEL-M','Complete bicycle, gravel, medium','MADE','BICYCLE','EA',NULL,'EA',9.880,'SERIAL',1,'MAKE','FINISHED','STD',4,0,1,1,1,'TRACE11'),
   ('CAL-BIKE-EBIKE-CITY','Complete electric city bicycle','MADE','BICYCLE','EA',NULL,'EA',21.400,'SERIAL',1,'MAKE','FINISHED','STD',2,0,1,1,1,'TRACE11'),
   ('SVC-ASSEMBLY-HOUR','Service, assembly labour time','SERVICE','SERVICE','HR',NULL,'HR',NULL,'NONE',0,'MAKE','SERVICE','STD',NULL,0,1,0,0,'FIN7'),
   ('PKG-BOX-BIKE-L','Packaging, bicycle carton large','PACKAGING','PACKAGING','EA','PK10',NULL,2.150,'NONE',0,'BUY','PACKAGING','MWA',NULL,1,0,0,1,'FIN7')
  ) AS v (ItemCode, Description, ItemClass, Category, StockUom, PurchUom, SalesUom, WeightKg, Trace, Inspect, MakeBuy, ValGroup, CostMethod, BatchQty, Purch, Sale, Manu, Stocked, Retention)
 WHERE NOT EXISTS (SELECT 1 FROM inv.Item x WHERE x.CompanyCode = 'CAL' AND x.ItemCode = v.ItemCode);
GO
