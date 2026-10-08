/*  Warehouses, stock locations and item/site planning data.  Every site has goods-in,
    quarantine and inspection locations so that items flagged for inspection can be received
    without becoming available (D-09), plus work in progress, despatch, returns and scrap.
    REQ: ITM-013, ITM-014, ITM-018, ITM-022, PO-009, SO-012, D-09 */
SET NOCOUNT ON;
GO
INSERT inv.Warehouse (CompanyCode, SiteCode, WarehouseCode, Name, IsDefault, IsActive, CreatedBy)
SELECT s.CompanyCode, s.SiteCode, w.WarehouseCode, s.SiteCode + ' ' + w.Name, w.IsDefault, 1, 'SEED'
  FROM core.Site s
  CROSS JOIN (VALUES ('MAIN','main warehouse',1), ('SUB','subcontract and external store',0)) AS w (WarehouseCode, Name, IsDefault)
 WHERE NOT EXISTS (SELECT 1 FROM inv.Warehouse x WHERE x.CompanyCode = s.CompanyCode AND x.SiteCode = s.SiteCode AND x.WarehouseCode = w.WarehouseCode);
GO
DECLARE @loc TABLE (WarehouseCode varchar(20), LocationCode varchar(20), Name varchar(60), LocationType varchar(12),
                    IsAvailable bit, IsOwnedStock bit, PickSequence int);
INSERT @loc VALUES
 ('MAIN','GOODSIN','Goods inwards bay','GOODSIN',0,1,10),
 ('MAIN','QUAR01','Quarantine cage 1','QUARANTINE',0,1,20),
 ('MAIN','INSP01','Inspection bench 1','INSPECTION',0,1,30),
 ('MAIN','RAW01','Raw material racking A','STOCK',1,1,100),
 ('MAIN','RAW02','Raw material racking B','STOCK',1,1,110),
 ('MAIN','STKA01','Small parts bins A01','STOCK',1,1,120),
 ('MAIN','STKA02','Small parts bins A02','STOCK',1,1,130),
 ('MAIN','STKB01','Component shelving B01','STOCK',1,1,140),
 ('MAIN','CONSUM','Consumables and chemicals store','STOCK',1,1,150),
 ('MAIN','FG01','Finished goods racking','STOCK',1,1,200),
 ('MAIN','WIP01','Work in progress floor','WIP',0,1,300),
 ('MAIN','DESP01','Despatch staging','DESPATCH',0,1,400),
 ('MAIN','RET01','Customer returns holding','RETURNS',0,1,410),
 ('MAIN','SCRAP01','Scrap and quarantine write off','SCRAP',0,1,420),
 ('MAIN','TRANSIT','Stock in transit between sites','TRANSIT',0,1,430),
 ('SUB','SUBCON01','Stock at subcontractor','SUBCON',0,1,500),
 ('SUB','CUSTOWN','Customer owned material held on site','STOCK',0,0,510);
INSERT inv.Location (CompanyCode, SiteCode, WarehouseCode, LocationCode, Name, LocationType, IsAvailable, IsOwnedStock,
                     AllowNegative, PickSequence, IsActive, CreatedBy)
SELECT w.CompanyCode, w.SiteCode, l.WarehouseCode, l.LocationCode, l.Name, l.LocationType, l.IsAvailable, l.IsOwnedStock,
       0, l.PickSequence, 1, 'SEED'
  FROM inv.Warehouse w JOIN @loc l ON l.WarehouseCode = w.WarehouseCode
 WHERE NOT EXISTS (SELECT 1 FROM inv.Location x WHERE x.CompanyCode = w.CompanyCode AND x.SiteCode = w.SiteCode
                     AND x.WarehouseCode = l.WarehouseCode AND x.LocationCode = l.LocationCode);
GO
/*  Item/site planning data.  Reading and Glasgow are the primary sites; Birmingham holds the
    fabrication range and Livingston a finished goods buffer. */
INSERT inv.ItemSite (CompanyCode, SiteCode, ItemCode, DefaultWarehouseCode, DefaultLocationCode, MakeBuy, LeadTimeDays,
                     MinOrderQty, PurchaseMultipleQty, ReorderPointQty, SafetyStockQty, MaxStockQty, AbcClass, IsActive, CreatedBy)
SELECT i.CompanyCode, s.SiteCode, i.ItemCode, 'MAIN',
       CASE WHEN i.Category IN ('TUBING','BARSTOCK','SHEET') THEN 'RAW01'
            WHEN i.Category = 'CONSUMABLE' THEN 'CONSUM'
            WHEN i.Category IN ('FASTENER','BEARING') THEN 'STKA01'
            WHEN i.ItemClass IN ('MADE','KIT') THEN 'FG01'
            WHEN i.Category = 'PACKAGING' THEN 'STKB01'
            ELSE 'STKB01' END,
       i.MakeBuy,
       CASE WHEN i.MakeBuy = 'MAKE' THEN 10 WHEN i.Category = 'ELECTRONIC' THEN 45 WHEN i.Category = 'TUBING' THEN 21 ELSE 14 END,
       CASE WHEN i.StockUomCode = 'M' THEN 6.0 WHEN i.Category = 'FASTENER' THEN 100.0 ELSE 1.0 END,
       CASE WHEN i.Category = 'FASTENER' THEN 100.0 ELSE 1.0 END,
       CASE WHEN i.Category = 'FASTENER' THEN 500.0 WHEN i.ItemClass = 'MADE' THEN 4.0 ELSE 10.0 END,
       CASE WHEN i.Category = 'FASTENER' THEN 250.0 WHEN i.ItemClass = 'MADE' THEN 2.0 ELSE 5.0 END,
       CASE WHEN i.Category = 'FASTENER' THEN 5000.0 ELSE 250.0 END,
       CASE WHEN i.ItemClass = 'MADE' THEN 'A' WHEN i.Category IN ('ELECTRONIC','TUBING') THEN 'A'
            WHEN i.Category IN ('FASTENER','PACKAGING') THEN 'C' ELSE 'B' END,
       1, 'SEED'
  FROM inv.Item i
  JOIN core.Site s ON s.CompanyCode = i.CompanyCode
 WHERE i.IsStocked = 1 AND i.LifecycleStatus IN ('RELEASED','OBSOLETE')
   AND (s.SiteCode IN ('RDG','GLA')
        OR (s.SiteCode = 'BHM' AND i.Category IN ('TUBING','BARSTOCK','SHEET','CONSUMABLE','SUBASSY','FRAME'))
        OR (s.SiteCode = 'LIV' AND i.ItemClass IN ('MADE','PACKAGING')))
   AND NOT EXISTS (SELECT 1 FROM inv.ItemSite x WHERE x.CompanyCode = i.CompanyCode AND x.SiteCode = s.SiteCode AND x.ItemCode = i.ItemCode);
GO
