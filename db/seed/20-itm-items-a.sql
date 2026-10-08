/*  Units of measure and the item master, part A: raw material, fasteners, bearings, consumables
    and electronics.  Stellar Components Ltd designs and builds bicycle frames and components.
    Metric base units throughout; imperial purchases are carried through approved conversions.
    REQ: ITM-001 to ITM-013, ITM-021, D-09, D-21 */
SET NOCOUNT ON;
GO
MERGE inv.Uom AS t
USING (VALUES
 ('EA','Each','EACH',NULL,1.0,1,0),          ('PR','Pair','EACH','EA',2.0,1,0),
 ('SET','Set','EACH','EA',1.0,1,0),          ('BOX','Box','EACH','EA',1.0,1,0),
 ('PK10','Pack of ten','EACH','EA',10.0,1,0),('PK100','Pack of one hundred','EACH','EA',100.0,1,0),
 ('PK500','Pack of five hundred','EACH','EA',500.0,1,0),
 ('M','Metre','LENGTH',NULL,1.0,1,3),        ('MM','Millimetre','LENGTH','M',0.001,1,0),
 ('CM','Centimetre','LENGTH','M',0.01,1,1),  ('FT','Foot','LENGTH','M',0.3048,0,3),
 ('IN','Inch','LENGTH','M',0.0254,0,3),      ('YD','Yard','LENGTH','M',0.9144,0,3),
 ('KG','Kilogram','MASS',NULL,1.0,1,3),      ('G','Gram','MASS','KG',0.001,1,0),
 ('T','Tonne','MASS','KG',1000.0,1,3),       ('LB','Pound','MASS','KG',0.45359237,0,3),
 ('OZ','Ounce','MASS','KG',0.028349523,0,3),
 ('L','Litre','VOLUME',NULL,1.0,1,3),        ('ML','Millilitre','VOLUME','L',0.001,1,0),
 ('M3','Cubic metre','VOLUME','L',1000.0,1,4),('GAL','Imperial gallon','VOLUME','L',4.54609,0,3),
 ('M2','Square metre','AREA',NULL,1.0,1,3),  ('SQFT','Square foot','AREA','M2',0.09290304,0,3),
 ('HR','Hour','TIME',NULL,1.0,1,2),          ('MIN','Minute','TIME','HR',0.016666667,1,2),
 ('DAY','Day','TIME','HR',24.0,1,2)
) AS s (UomCode, Name, UomDimension, BaseUomCode, FactorToBase, IsMetric, DecimalPlaces)
ON t.UomCode = s.UomCode
WHEN MATCHED AND EXISTS (SELECT s.Name, s.UomDimension, s.FactorToBase EXCEPT SELECT t.Name, t.UomDimension, t.FactorToBase)
    THEN UPDATE SET Name = s.Name, UomDimension = s.UomDimension, BaseUomCode = s.BaseUomCode,
                    FactorToBase = s.FactorToBase, IsMetric = s.IsMetric, DecimalPlaces = s.DecimalPlaces
WHEN NOT MATCHED THEN INSERT (UomCode, Name, UomDimension, BaseUomCode, FactorToBase, IsMetric, DecimalPlaces, IsActive)
    VALUES (s.UomCode, s.Name, s.UomDimension, s.BaseUomCode, s.FactorToBase, s.IsMetric, s.DecimalPlaces, 1);
GO
DECLARE @i TABLE (ItemCode varchar(40), Description varchar(100), ItemClass varchar(10), Category varchar(30),
                  StockUom varchar(10), PurchUom varchar(10), SalesUom varchar(10), Quantum decimal(19,6), Indivisible bit,
                  WeightKg decimal(19,6), Rev varchar(4), Trace varchar(8), ShelfLife int, Inspect bit, MakeBuy varchar(6),
                  ValGroup varchar(20), CostMethod varchar(6), BatchQty decimal(19,6), Purch bit, Sale bit, Manu bit,
                  Stocked bit, Life varchar(10), Retention varchar(20));
INSERT @i VALUES
 ('TUBE-AL6061-28X1.5','Aluminium tube 6061-T6, 28.0 x 1.5 mm','BOUGHT','TUBING','M','M',NULL,0.001,0,0.345,'B','LOT',NULL,0,'BUY','RAWMAT','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('TUBE-AL6061-31.8X1.6','Aluminium tube 6061-T6, 31.8 x 1.6 mm','BOUGHT','TUBING','M','M',NULL,0.001,0,0.412,'B','LOT',NULL,0,'BUY','RAWMAT','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('TUBE-AL7005-34.9X1.4','Aluminium tube 7005-T6, 34.9 x 1.4 mm','BOUGHT','TUBING','M','M',NULL,0.001,0,0.398,'A','LOT',NULL,1,'BUY','RAWMAT','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('TUBE-CRMO-25.4X0.9','Chromoly tube 4130, 25.4 x 0.9 mm','BOUGHT','TUBING','M','M',NULL,0.001,0,0.544,'A','LOT',NULL,0,'BUY','RAWMAT','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('TUBE-CRMO-28.6X0.8','Chromoly tube 4130, 28.6 x 0.8 mm','BOUGHT','TUBING','M','M',NULL,0.001,0,0.551,'A','LOT',NULL,0,'BUY','RAWMAT','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('TUBE-TI3AL-31.8X1.2','Titanium tube 3Al-2.5V, 31.8 x 1.2 mm','BOUGHT','TUBING','M','M',NULL,0.001,0,0.325,'A','LOT',NULL,1,'BUY','RAWMAT','FIFO',NULL,1,0,0,1,'RELEASED','TRACE11'),
 ('TUBE-CF-UD-30X2.0','Carbon fibre tube unidirectional, 30 x 2.0 mm','BOUGHT','TUBING','M','M',NULL,0.001,0,0.148,'A','LOT',NULL,1,'BUY','RAWMAT','FIFO',NULL,1,0,0,1,'RELEASED','TRACE11'),
 ('TUBE-STEEL-1IN-16G','Mild steel tube 1 inch 16 gauge, bought in feet','BOUGHT','TUBING','M','FT',NULL,0.001,0,0.985,'A','NONE',NULL,0,'BUY','RAWMAT','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('BAR-AL6082-25MM','Aluminium bar 6082-T6, 25 mm round','BOUGHT','BARSTOCK','M','M',NULL,0.001,0,1.325,'A','LOT',NULL,0,'BUY','RAWMAT','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('BAR-SS316-12MM','Stainless bar 316, 12 mm round','BOUGHT','BARSTOCK','M','M',NULL,0.001,0,0.893,'A','LOT',NULL,0,'BUY','RAWMAT','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('BAR-TI6AL4V-16MM','Titanium bar 6Al-4V, 16 mm round','BOUGHT','BARSTOCK','M','M',NULL,0.001,0,0.892,'A','LOT',NULL,1,'BUY','RAWMAT','FIFO',NULL,1,0,0,1,'RELEASED','TRACE11'),
 ('BAR-STEEL-0.5IN','Free cutting steel bar 1/2 inch, bought in feet','BOUGHT','BARSTOCK','M','FT',NULL,0.001,0,1.003,'A','NONE',NULL,0,'BUY','RAWMAT','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('SHEET-AL5052-2MM','Aluminium sheet 5052-H32, 2.0 mm','BOUGHT','SHEET','M2','M2',NULL,0.001,0,5.400,'A','LOT',NULL,0,'BUY','RAWMAT','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('SHEET-SS304-1.5MM','Stainless sheet 304, 1.5 mm','BOUGHT','SHEET','M2','M2',NULL,0.001,0,11.925,'A','LOT',NULL,0,'BUY','RAWMAT','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('SHEET-AL-0.125IN','Aluminium sheet 0.125 inch, bought in square feet','BOUGHT','SHEET','M2','SQFT',NULL,0.001,0,8.580,'A','NONE',NULL,0,'BUY','RAWMAT','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('BOLT-M5X16-SS','Socket cap bolt M5 x 16, stainless A4','BOUGHT','FASTENER','EA','PK100',NULL,1.0,1,0.004,'A','LOT',NULL,0,'BUY','HARDWARE','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('BOLT-M6X20-TI','Socket cap bolt M6 x 20, titanium','BOUGHT','FASTENER','EA','PK10',NULL,1.0,1,0.003,'A','LOT',NULL,0,'BUY','HARDWARE','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('BOLT-M8X35-SS','Hex bolt M8 x 35, stainless A2','BOUGHT','FASTENER','EA','PK100',NULL,1.0,1,0.019,'A','NONE',NULL,0,'BUY','HARDWARE','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('SCREW-M4X10-SS','Button head screw M4 x 10, stainless','BOUGHT','FASTENER','EA','PK500',NULL,1.0,1,0.002,'A','NONE',NULL,0,'BUY','HARDWARE','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('NUT-M6-NYLOC','Nyloc nut M6, stainless A4','BOUGHT','FASTENER','EA','PK500',NULL,1.0,1,0.003,'A','NONE',NULL,0,'BUY','HARDWARE','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('WASHER-M6-SS','Plain washer M6, stainless','BOUGHT','FASTENER','EA','PK500',NULL,1.0,1,0.001,'A','NONE',NULL,0,'BUY','HARDWARE','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('RIVET-4.8X12-AL','Blind rivet 4.8 x 12, aluminium','BOUGHT','FASTENER','EA','PK500',NULL,1.0,1,0.001,'A','NONE',NULL,0,'BUY','HARDWARE','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('INSERT-M5-AL','Threaded insert M5, aluminium','BOUGHT','FASTENER','EA','PK100',NULL,1.0,1,0.002,'A','NONE',NULL,0,'BUY','HARDWARE','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('BEARING-6803-2RS','Sealed bearing 6803-2RS, 17 x 26 x 5','BOUGHT','BEARING','EA','PK10',NULL,1.0,1,0.012,'A','LOT',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('BEARING-6902-2RS','Sealed bearing 6902-2RS, 15 x 28 x 7','BOUGHT','BEARING','EA','PK10',NULL,1.0,1,0.016,'A','LOT',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('BEARING-MR15267','Sealed bearing MR15267, 15 x 26 x 7','BOUGHT','BEARING','EA','PK10',NULL,1.0,1,0.014,'A','LOT',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('HEADSET-IS42-52','Integrated headset IS42/IS52, sealed','BOUGHT','COMPONENT','EA','EA','EA',1.0,1,0.098,'B','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('BB-SHELL-BSA68','Bottom bracket shell BSA 68 mm threaded','BOUGHT','COMPONENT','EA','EA',NULL,1.0,1,0.145,'A','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('BB-T47-PRESS','Bottom bracket shell T47 threaded','BOUGHT','COMPONENT','EA','EA',NULL,1.0,1,0.162,'A','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('DROPOUT-UDH-RH','Universal derailleur hanger, right hand','BOUGHT','COMPONENT','EA','EA','EA',1.0,1,0.042,'C','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('DROPOUT-UDH-LH','Dropout plate, left hand','BOUGHT','COMPONENT','EA','EA','EA',1.0,1,0.040,'C','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('ADHESIVE-EP420-250ML','Structural epoxy EP420, 250 ml cartridge','BOUGHT','CONSUMABLE','EA','BOX',NULL,1.0,1,0.290,'A','LOT',365,1,'BUY','CONSUMABLE','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('PRIMER-PR10-1L','Etch primer PR10, 1 litre','BOUGHT','CONSUMABLE','L','L',NULL,0.001,0,1.050,'A','LOT',540,0,'BUY','CONSUMABLE','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('PAINT-2K-BLACK-5L','Two pack paint, satin black, 5 litre','BOUGHT','CONSUMABLE','L','L',NULL,0.001,0,1.120,'A','LOT',730,0,'BUY','CONSUMABLE','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('PAINT-2K-RED-5L','Two pack paint, gloss red, 5 litre','BOUGHT','CONSUMABLE','L','L',NULL,0.001,0,1.120,'A','LOT',730,0,'BUY','CONSUMABLE','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('GREASE-LI2-400G','Lithium grease NLGI 2, 400 g cartridge','BOUGHT','CONSUMABLE','EA','BOX',NULL,1.0,1,0.420,'A','LOT',1095,0,'BUY','CONSUMABLE','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('THREADLOCK-243-50ML','Medium strength threadlocker, 50 ml','BOUGHT','CONSUMABLE','EA','BOX',NULL,1.0,1,0.060,'A','LOT',730,0,'BUY','CONSUMABLE','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('FLUX-AL-500G','Aluminium brazing flux, 500 g','BOUGHT','CONSUMABLE','EA','BOX',NULL,1.0,1,0.520,'A','LOT',365,1,'BUY','CONSUMABLE','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('WIRE-TIG-4043-5KG','TIG filler wire 4043, 5 kg spool','BOUGHT','CONSUMABLE','KG','KG',NULL,0.001,0,1.000,'A','LOT',NULL,0,'BUY','CONSUMABLE','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('GAS-ARGON-50L','Pure argon, 50 litre cylinder','BOUGHT','CONSUMABLE','EA','EA',NULL,1.0,1,65.000,'A','NONE',NULL,0,'BUY','CONSUMABLE','MWA',NULL,1,0,0,1,'RELEASED','FIN7'),
 ('CTRL-EBIKE-250W','Electric bicycle controller 250 W, 36 V','BOUGHT','ELECTRONIC','EA','EA','EA',1.0,1,0.310,'C','SERIAL',NULL,1,'BUY','ELECTRONIC','FIFO',NULL,1,1,0,1,'RELEASED','TRACE11'),
 ('MOTOR-HUB-250W','Rear hub motor 250 W, 36 V','BOUGHT','ELECTRONIC','EA','EA','EA',1.0,1,2.850,'B','SERIAL',NULL,1,'BUY','ELECTRONIC','FIFO',NULL,1,1,0,1,'RELEASED','TRACE11'),
 ('BATTERY-36V-14AH','Lithium ion battery pack 36 V 14 Ah','BOUGHT','ELECTRONIC','EA','EA','EA',1.0,1,3.200,'B','SERIAL',730,1,'BUY','ELECTRONIC','FIFO',NULL,1,1,0,1,'RELEASED','TRACE11'),
 ('DISPLAY-LCD-C3','Handlebar display unit, LCD C3','BOUGHT','ELECTRONIC','EA','EA','EA',1.0,1,0.145,'A','SERIAL',NULL,0,'BUY','ELECTRONIC','FIFO',NULL,1,1,0,1,'RELEASED','TRACE11'),
 ('SENSOR-TORQUE-BB','Bottom bracket torque sensor','BOUGHT','ELECTRONIC','EA','EA',NULL,1.0,1,0.095,'A','SERIAL',NULL,1,'BUY','ELECTRONIC','FIFO',NULL,1,0,0,1,'RELEASED','TRACE11'),
 ('SUSP-FORK-120MM','Suspension fork 120 mm travel, 29 inch','BOUGHT','COMPONENT','EA','EA','EA',1.0,1,1.980,'B','SERIAL',NULL,1,'BUY','COMPONENT','FIFO',NULL,1,1,0,1,'RELEASED','TRACE11'),
 ('SHOCK-REAR-210X55','Rear shock 210 x 55 mm, air','BOUGHT','COMPONENT','EA','EA','EA',1.0,1,0.410,'A','SERIAL',NULL,1,'BUY','COMPONENT','FIFO',NULL,1,1,0,1,'RELEASED','TRACE11'),
 ('CHAIN-116L-11S','Chain 116 links, eleven speed','BOUGHT','DRIVETRAIN','EA','EA','EA',1.0,1,0.252,'A','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('CASSETTE-11-34-11S','Cassette 11-34, eleven speed','BOUGHT','DRIVETRAIN','EA','EA','EA',1.0,1,0.345,'A','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('CRANKSET-172.5-50-34','Crankset 172.5 mm, 50/34 chainrings','BOUGHT','DRIVETRAIN','EA','EA','EA',1.0,1,0.712,'A','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('DERAILLEUR-RD-11S','Rear derailleur, eleven speed','BOUGHT','DRIVETRAIN','EA','EA','EA',1.0,1,0.256,'A','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('BRAKE-DISC-160MM','Brake rotor 160 mm, six bolt','BOUGHT','BRAKING','EA','EA','EA',1.0,1,0.118,'A','LOT',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('BRAKE-DISC-180MM','Brake rotor 180 mm, six bolt','BOUGHT','BRAKING','EA','EA','EA',1.0,1,0.142,'A','LOT',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('BRAKE-CALIPER-FLAT','Hydraulic disc caliper, flat mount','BOUGHT','BRAKING','EA','EA','EA',1.0,1,0.138,'A','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('SADDLE-COMP-143','Saddle, composite rail, 143 mm','BOUGHT','FINISHING','EA','EA','EA',1.0,1,0.228,'A','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('SEATPOST-27.2X350','Seatpost 27.2 x 350 mm, alloy','BOUGHT','FINISHING','EA','EA','EA',1.0,1,0.265,'A','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('STEM-100MM-6D','Stem 100 mm, six degree','BOUGHT','FINISHING','EA','EA','EA',1.0,1,0.142,'A','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('HANDLEBAR-420-ALU','Handlebar 420 mm, alloy','BOUGHT','FINISHING','EA','EA','EA',1.0,1,0.295,'A','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('GRIP-SET-LOCKON','Lock on grip set','BOUGHT','FINISHING','PR','PR','PR',1.0,1,0.112,'A','NONE',NULL,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('TYRE-700X32C','Tyre 700 x 32C, folding','BOUGHT','WHEEL','EA','EA','EA',1.0,1,0.345,'A','LOT',1825,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7'),
 ('TUBE-INNER-700C','Inner tube 700 x 28-35C','BOUGHT','WHEEL','EA','PK10','EA',1.0,1,0.105,'A','LOT',1825,0,'BUY','COMPONENT','MWA',NULL,1,1,0,1,'RELEASED','FIN7');
INSERT inv.Item (CompanyCode, ItemCode, Description, ItemClass, Category, ProductGroup, StewardUser, StockUomCode, PurchaseUomCode, SalesUomCode,
                 QuantumQty, IsIndivisible, NetWeightKg, CurrentRevision, TraceabilityType, ShelfLifeDays, RequiresInspection,
                 MakeBuy, ValuationGroup, CostMethod, StandardBatchQty, IsPurchasable, IsSaleable, IsManufacturable, IsStocked,
                 LifecycleStatus, RetentionClass, ReleasedAt, ReleasedBy, CreatedBy)
SELECT 'STC', i.ItemCode, i.Description, i.ItemClass, i.Category, i.Category, 'ENGINEER', i.StockUom, i.PurchUom, i.SalesUom,
       i.Quantum, i.Indivisible, i.WeightKg, i.Rev, i.Trace, i.ShelfLife, i.Inspect,
       i.MakeBuy, i.ValGroup, i.CostMethod, i.BatchQty, i.Purch, i.Sale, i.Manu, i.Stocked,
       i.Life, i.Retention, '2025-04-01T09:00:00', 'ENGMGR', 'SEED'
  FROM @i i
 WHERE NOT EXISTS (SELECT 1 FROM inv.Item x WHERE x.CompanyCode = 'STC' AND x.ItemCode = i.ItemCode);
GO
