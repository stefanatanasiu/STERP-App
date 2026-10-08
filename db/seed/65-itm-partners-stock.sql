/*  Cost elements, item/partner references, lots and serial numbers, opening stock balances,
    FIFO cost layers and moving weighted average carrying values.  No transactions are posted:
    these are the authoritative opening positions at cut over (D-18).
    REQ: ITM-010, ITM-015, ITM-019, ITM-024, CST-005 to CST-009, PO-002, D-04, D-09, D-18 */
SET NOCOUNT ON;
GO
MERGE cst.CostElement AS t
USING (VALUES
 ('MAT','Direct material','MATERIAL',1,10),      ('LAB','Direct labour','LABOUR',1,20),
 ('MACH','Machine time','MACHINE',1,30),          ('SUB','Subcontract','SUBCON',1,40),
 ('OVHV','Variable production overhead','OVHDVAR',1,50), ('OVHF','Fixed production overhead','OVHDFIX',1,60),
 ('LAND','Landed cost','LANDED',1,70),            ('REC','Overhead recovery','RECOVERY',0,80),
 ('VAR','Variance','VARIANCE',0,90)
) AS s (ElementCode, Name, ElementType, IsInventoriable, SortOrder)
ON t.ElementCode = s.ElementCode
WHEN MATCHED AND EXISTS (SELECT s.Name, s.ElementType, s.IsInventoriable EXCEPT SELECT t.Name, t.ElementType, t.IsInventoriable)
    THEN UPDATE SET Name = s.Name, ElementType = s.ElementType, IsInventoriable = s.IsInventoriable, SortOrder = s.SortOrder
WHEN NOT MATCHED THEN INSERT (ElementCode, Name, ElementType, IsInventoriable, SortOrder)
    VALUES (s.ElementCode, s.Name, s.ElementType, s.IsInventoriable, s.SortOrder);
GO
/*  Approved supply sources with the supplier's own part number and price breaks. */
DECLARE @sup TABLE (Category varchar(30), SupplierCode varchar(20), Rank tinyint);
INSERT @sup VALUES
 ('TUBING','S0001',1), ('TUBING','S0002',2), ('BARSTOCK','S0001',1), ('BARSTOCK','S0003',2),
 ('SHEET','S0003',1),  ('FASTENER','S0006',1), ('FASTENER','S0022',2), ('BEARING','S0005',1),
 ('BEARING','S0023',2),('CONSUMABLE','S0009',1), ('ELECTRONIC','S0027',1), ('ELECTRONIC','S0028',2),
 ('DRIVETRAIN','S0025',1), ('BRAKING','S0024',1), ('WHEEL','S0023',1), ('FINISHING','S0024',1),
 ('PACKAGING','S0011',1), ('PACKAGING','S0012',2), ('SERVICE','S0007',1), ('NONSTOCK','S0020',1),
 ('LANDED','S0014',1),  ('FRAME','S0004',2);
INSERT inv.ItemPartner (CompanyCode, ItemCode, PartnerType, PartnerCode, PartnerItemCode, PartnerDescription, UomCode,
                        StockUnitsPerUom, QtyBreak, CurrencyCode, UnitPrice, PricePerQty, LeadTimeDays, MinOrderQty,
                        OrderMultipleQty, PreferenceRank, IsApprovedSource, QuoteRef, EffectiveFrom, CreatedBy)
SELECT i.CompanyCode, i.ItemCode, 'SUPPLIER', p.SupplierCode,
       'SUP-' + RIGHT(REPLACE(i.ItemCode, '-', ''), 12) + '-' + RIGHT(p.SupplierCode, 2),
       i.Description, ISNULL(i.PurchaseUomCode, i.StockUomCode),
       ISNULL((SELECT TOP (1) u.StockUnitsPerUom FROM inv.ItemUom u WHERE u.CompanyCode = i.CompanyCode
                AND u.ItemCode = i.ItemCode AND u.UomCode = i.PurchaseUomCode), 1),
       b.QtyBreak, s.CurrencyCode,
       ROUND((1.0 + (ABS(CHECKSUM(i.ItemCode)) % 4000) / 100.0) * (1.0 - b.Disc) * CASE WHEN p.Rank = 1 THEN 1.0 ELSE 1.06 END, 6),
       1, CASE WHEN s.CountryCode = 'GB' THEN 14 ELSE 42 END,
       CASE WHEN i.Category = 'FASTENER' THEN 500.0 ELSE 10.0 END,
       CASE WHEN i.Category = 'FASTENER' THEN 100.0 ELSE 1.0 END,
       p.Rank, CASE WHEN p.Rank = 1 THEN 1 ELSE 0 END, 'QUO-2026-' + p.SupplierCode, '2026-01-01', 'SEED'
  FROM inv.Item i
  JOIN @sup p ON p.Category = i.Category
  JOIN ap.Supplier s ON s.CompanyCode = i.CompanyCode AND s.SupplierCode = p.SupplierCode
  CROSS APPLY (VALUES (0.0, 0.00), (100.0, 0.04), (1000.0, 0.09)) AS b (QtyBreak, Disc)
 WHERE i.IsPurchasable = 1
   AND NOT EXISTS (SELECT 1 FROM inv.ItemPartner x WHERE x.CompanyCode = i.CompanyCode AND x.ItemCode = i.ItemCode
                     AND x.PartnerType = 'SUPPLIER' AND x.PartnerCode = p.SupplierCode AND x.QtyBreak = b.QtyBreak);
/*  Customer part numbers for the original equipment accounts. */
INSERT inv.ItemPartner (CompanyCode, ItemCode, PartnerType, PartnerCode, PartnerItemCode, PartnerDescription,
                        UomCode, QtyBreak, CurrencyCode, UnitPrice, PricePerQty, PreferenceRank, IsApprovedSource,
                        EffectiveFrom, CreatedBy)
SELECT i.CompanyCode, i.ItemCode, 'CUSTOMER', c.CustomerCode,
       'CUST-' + RIGHT(c.CustomerCode, 3) + '-' + RIGHT(REPLACE(i.ItemCode, '-', ''), 10), i.Description,
       ISNULL(i.SalesUomCode, i.StockUomCode), 0, c.CurrencyCode,
       ROUND((20.0 + (ABS(CHECKSUM(i.ItemCode + c.CustomerCode)) % 60000) / 100.0), 6), 1, 1, 1, '2026-01-01', 'SEED'
  FROM inv.Item i
  JOIN ar.Customer c ON c.CompanyCode = i.CompanyCode AND c.Classification = 'OEM' AND c.Status = 'ACTIVE'
 WHERE i.IsSaleable = 1 AND i.ItemClass IN ('MADE', 'KIT')
   AND NOT EXISTS (SELECT 1 FROM inv.ItemPartner x WHERE x.CompanyCode = i.CompanyCode AND x.ItemCode = i.ItemCode
                     AND x.PartnerType = 'CUSTOMER' AND x.PartnerCode = c.CustomerCode);
GO
/*  Lots for lot controlled items, with expiry where the item has a shelf life (FEFO, D-09). */
INSERT inv.Lot (CompanyCode, ItemCode, LotCode, Revision, SupplierLotRef, SupplierCode, ManufacturedDate, ReceivedDate,
                ExpiryDate, QualityStatus, CertificateRef, OriginCountry, RetentionClass, CreatedBy)
SELECT i.CompanyCode, i.ItemCode, 'L26' + RIGHT('000' + CAST(n.Seq AS varchar(3)), 3) + '-' + RIGHT(REPLACE(i.ItemCode, '-', ''), 6),
       i.CurrentRevision, 'SL-' + CAST(ABS(CHECKSUM(i.ItemCode)) % 900000 + 100000 AS varchar(6)),
       (SELECT TOP (1) PartnerCode FROM inv.ItemPartner p WHERE p.CompanyCode = i.CompanyCode AND p.ItemCode = i.ItemCode
         AND p.PartnerType = 'SUPPLIER' ORDER BY p.PreferenceRank),
       DATEADD(day, -60 * n.Seq, CAST('2026-09-01' AS date)), DATEADD(day, -45 * n.Seq, CAST('2026-09-01' AS date)),
       CASE WHEN i.ShelfLifeDays IS NOT NULL THEN DATEADD(day, i.ShelfLifeDays - 45 * n.Seq, CAST('2026-09-01' AS date)) END,
       CASE WHEN i.RequiresInspection = 1 AND n.Seq = 3 THEN 'INSPECTION' ELSE 'ACCEPTED' END,
       'CERT-2026-' + CAST(ABS(CHECKSUM(i.ItemCode)) % 90000 + 10000 AS varchar(5)),
       CASE WHEN i.Category IN ('ELECTRONIC', 'DRIVETRAIN') THEN 'TW' ELSE 'GB' END,
       i.RetentionClass, 'SEED'
  FROM inv.Item i CROSS JOIN (VALUES (1), (2), (3)) AS n (Seq)
 WHERE i.TraceabilityType = 'LOT' AND i.IsStocked = 1 AND i.LifecycleStatus = 'RELEASED'
   AND NOT EXISTS (SELECT 1 FROM inv.Lot l WHERE l.CompanyCode = i.CompanyCode AND l.ItemCode = i.ItemCode
                     AND l.LotCode = 'L26' + RIGHT('000' + CAST(n.Seq AS varchar(3)), 3) + '-' + RIGHT(REPLACE(i.ItemCode, '-', ''), 6));
GO
/*  Opening stock.  Lot controlled items hold their stock by lot, serial items one unit per
    serial number, and a small amount sits in quarantine, inspection and customer ownership. */
INSERT inv.StockBalance (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode, Revision, LotCode, SerialNo,
                         OwnerType, OwnerCode, QualityStatus, QtyOnHand, QtyReserved, LastMovementAt, LastCountedAt)
SELECT s.CompanyCode, s.SiteCode, 'MAIN', s.DefaultLocationCode, s.ItemCode, i.CurrentRevision, '', '', 'OWNED', '', 'ACCEPTED',
       CASE WHEN i.Category = 'FASTENER' THEN 1200 + (ABS(CHECKSUM(i.ItemCode)) % 2400)
            WHEN i.StockUomCode = 'M' THEN 60 + (ABS(CHECKSUM(i.ItemCode)) % 240)
            WHEN i.ItemClass = 'MADE' THEN 4 + (ABS(CHECKSUM(i.ItemCode)) % 20)
            ELSE 20 + (ABS(CHECKSUM(i.ItemCode)) % 140) END,
       0, '2026-09-30T18:00:00', '2026-09-28T16:00:00'
  FROM inv.ItemSite s
  JOIN inv.Item i ON i.CompanyCode = s.CompanyCode AND i.ItemCode = s.ItemCode
 WHERE i.TraceabilityType = 'NONE' AND i.LifecycleStatus = 'RELEASED' AND s.SiteCode IN ('RDG', 'GLA')
   AND NOT EXISTS (SELECT 1 FROM inv.StockBalance b WHERE b.CompanyCode = s.CompanyCode AND b.SiteCode = s.SiteCode
                     AND b.LocationCode = s.DefaultLocationCode AND b.ItemCode = s.ItemCode AND b.LotCode = '' AND b.SerialNo = '');
INSERT inv.StockBalance (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode, Revision, LotCode, SerialNo,
                         OwnerType, OwnerCode, QualityStatus, QtyOnHand, QtyReserved, LastMovementAt)
SELECT s.CompanyCode, s.SiteCode, 'MAIN',
       CASE WHEN l.QualityStatus = 'INSPECTION' THEN 'INSP01' ELSE s.DefaultLocationCode END,
       s.ItemCode, i.CurrentRevision, l.LotCode, '', 'OWNED', '', l.QualityStatus,
       CASE WHEN i.StockUomCode = 'M' THEN 40 + (ABS(CHECKSUM(l.LotCode)) % 90)
            WHEN i.ItemClass = 'MADE' THEN 3 + (ABS(CHECKSUM(l.LotCode)) % 12)
            ELSE 15 + (ABS(CHECKSUM(l.LotCode)) % 90) END,
       0, '2026-09-30T18:00:00'
  FROM inv.ItemSite s
  JOIN inv.Item i ON i.CompanyCode = s.CompanyCode AND i.ItemCode = s.ItemCode
  JOIN inv.Lot l ON l.CompanyCode = s.CompanyCode AND l.ItemCode = s.ItemCode
 WHERE i.TraceabilityType = 'LOT' AND i.LifecycleStatus = 'RELEASED' AND s.SiteCode IN ('RDG', 'GLA')
   AND NOT EXISTS (SELECT 1 FROM inv.StockBalance b WHERE b.CompanyCode = s.CompanyCode AND b.SiteCode = s.SiteCode
                     AND b.ItemCode = s.ItemCode AND b.LotCode = l.LotCode AND b.SerialNo = '');
GO
INSERT inv.Serial (CompanyCode, ItemCode, SerialNo, Revision, Status, CurrentSiteCode, CurrentLocationCode, QualityStatus,
                   WarrantyEndDate, CreatedBy)
SELECT s.CompanyCode, s.ItemCode, 'SN26' + RIGHT(REPLACE(s.ItemCode, '-', ''), 6) + RIGHT('000' + CAST(n.Seq AS varchar(3)), 3),
       i.CurrentRevision, 'INSTOCK', s.SiteCode, s.DefaultLocationCode, 'ACCEPTED', '2028-03-31', 'SEED'
  FROM inv.ItemSite s
  JOIN inv.Item i ON i.CompanyCode = s.CompanyCode AND i.ItemCode = s.ItemCode
  CROSS JOIN (VALUES (1), (2), (3), (4)) AS n (Seq)
 WHERE i.TraceabilityType = 'SERIAL' AND i.LifecycleStatus = 'RELEASED' AND s.SiteCode IN ('RDG', 'GLA')
   AND NOT EXISTS (SELECT 1 FROM inv.Serial x WHERE x.CompanyCode = s.CompanyCode AND x.ItemCode = s.ItemCode
                     AND x.SerialNo = 'SN26' + RIGHT(REPLACE(s.ItemCode, '-', ''), 6) + RIGHT('000' + CAST(n.Seq AS varchar(3)), 3));
INSERT inv.StockBalance (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode, Revision, LotCode, SerialNo,
                         OwnerType, OwnerCode, QualityStatus, QtyOnHand, QtyReserved, LastMovementAt)
SELECT sn.CompanyCode, sn.CurrentSiteCode, 'MAIN', sn.CurrentLocationCode, sn.ItemCode, ISNULL(sn.Revision, 'A'), '',
       sn.SerialNo, 'OWNED', '', 'ACCEPTED', 1, 0, '2026-09-30T18:00:00'
  FROM inv.Serial sn
 WHERE NOT EXISTS (SELECT 1 FROM inv.StockBalance b WHERE b.CompanyCode = sn.CompanyCode AND b.ItemCode = sn.ItemCode
                     AND b.SerialNo = sn.SerialNo);
/*  Quarantine, inspection and customer owned examples. */
INSERT inv.StockBalance (CompanyCode, SiteCode, WarehouseCode, LocationCode, ItemCode, Revision, LotCode, SerialNo,
                         OwnerType, OwnerCode, QualityStatus, QtyOnHand, QtyReserved, LastMovementAt)
SELECT 'STC', v.SiteCode, v.Warehouse, v.LocationCode, v.ItemCode, 'A', '', '', v.OwnerType, v.OwnerCode, v.Quality, v.Qty, 0, '2026-09-29T11:00:00'
  FROM (VALUES
   ('RDG','MAIN','QUAR01','BEARING-6902-2RS','OWNED','','QUARANTINE',120.0),
   ('RDG','MAIN','INSP01','HEADSET-IS42-52','OWNED','','INSPECTION',40.0),
   ('BHM','MAIN','QUAR01','TUBE-AL7005-34.9X1.4','OWNED','','QUARANTINE',85.0),
   ('BHM','MAIN','INSP01','BAR-AL6082-25MM','OWNED','','INSPECTION',55.0),
   ('RDG','SUB','SUBCON01','SA-FRAME-FRONT-TRI-M','OWNED','','ACCEPTED',14.0),
   ('RDG','SUB','CUSTOWN','DECAL-SET-ROAD','CUSTOMER','C0007','ACCEPTED',300.0),
   ('BHM','SUB','SUBCON01','SA-SEATSTAY-PAIR','OWNED','','ACCEPTED',22.0)
  ) AS v (SiteCode, Warehouse, LocationCode, ItemCode, OwnerType, OwnerCode, Quality, Qty)
 WHERE EXISTS (SELECT 1 FROM inv.Item i WHERE i.CompanyCode = 'STC' AND i.ItemCode = v.ItemCode)
   AND NOT EXISTS (SELECT 1 FROM inv.StockBalance b WHERE b.CompanyCode = 'STC' AND b.SiteCode = v.SiteCode
                     AND b.LocationCode = v.LocationCode AND b.ItemCode = v.ItemCode AND b.OwnerType = v.OwnerType);
GO
/*  FIFO layers for the FIFO valuation group, and carrying values for every valued item. */
WITH src AS (
 SELECT b.CompanyCode, b.ItemCode, b.Revision, b.LotCode, b.QtyOnHand, n.Seq,
        ROUND(0.85 + (ABS(CHECKSUM(b.ItemCode)) % 7500) / 100.0, 6) AS BaseCost,
        ROW_NUMBER() OVER (PARTITION BY b.CompanyCode, b.ItemCode, b.Revision ORDER BY b.LotCode, n.Seq) AS LayerSeq
   FROM inv.StockBalance b
   JOIN inv.Item i ON i.CompanyCode = b.CompanyCode AND i.ItemCode = b.ItemCode
   CROSS JOIN (VALUES (1), (2), (3)) AS n (Seq)
  WHERE i.CostMethod = 'FIFO' AND b.QtyOnHand > 0 AND b.OwnerType = 'OWNED' AND b.SerialNo = '')
INSERT cst.CostLayer (CompanyCode, ItemCode, Revision, LayerSeq, ReceiptDate, SourceDocType, SourceDocRef, LotCode,
                      QtyOriginal, QtyRemaining, UnitCost, ValueRemainingGBP, LandedAdjustmentGBP, IsClosed)
SELECT s.CompanyCode, s.ItemCode, s.Revision, s.LayerSeq,
       DATEADD(day, -30 * s.Seq, CAST('2026-09-30' AS date)), 'OPENING',
       'OPEN-2026-' + RIGHT('00' + CAST(s.Seq AS varchar(2)), 2), s.LotCode,
       ROUND(s.QtyOnHand / 3.0, 6), ROUND(s.QtyOnHand / 3.0, 6),
       ROUND(s.BaseCost * (1 + 0.02 * s.Seq), 6),
       ROUND(s.QtyOnHand / 3.0 * s.BaseCost * (1 + 0.02 * s.Seq), 2),
       ROUND(s.QtyOnHand / 3.0 * s.BaseCost * 0.015, 2), 0
  FROM src s
 WHERE NOT EXISTS (SELECT 1 FROM cst.CostLayer l WHERE l.CompanyCode = s.CompanyCode AND l.ItemCode = s.ItemCode
                     AND l.Revision = s.Revision AND l.LayerSeq = s.LayerSeq);
INSERT cst.ItemCost (CompanyCode, ItemCode, Revision, SiteCode, CostMethod, QtyOnHand, CarryingValueGBP,
                     AverageUnitCost, StandardUnitCost, LastPurchaseCost, NrvUnitValue, AllowanceGBP, LastUpdatedAt, LastUpdatedBy)
SELECT b.CompanyCode, b.ItemCode, b.Revision, b.SiteCode, i.CostMethod, SUM(b.QtyOnHand),
       ROUND(SUM(b.QtyOnHand) * c.BaseCost, 2), c.BaseCost,
       CASE WHEN i.CostMethod = 'STD' THEN ROUND(c.BaseCost * 1.03, 6) END,
       ROUND(c.BaseCost * 1.015, 6),
       CASE WHEN i.LifecycleStatus = 'OBSOLETE' THEN ROUND(c.BaseCost * 0.4, 6) END,
       CASE WHEN i.LifecycleStatus = 'OBSOLETE' THEN ROUND(SUM(b.QtyOnHand) * c.BaseCost * 0.6, 2) ELSE 0 END,
       '2026-09-30T23:30:00', 'COSTACC'
  FROM inv.StockBalance b
  JOIN inv.Item i ON i.CompanyCode = b.CompanyCode AND i.ItemCode = b.ItemCode
  CROSS APPLY (VALUES (ROUND(0.85 + (ABS(CHECKSUM(b.ItemCode)) % 7500) / 100.0, 6))) AS c (BaseCost)
 WHERE b.OwnerType = 'OWNED'
   AND NOT EXISTS (SELECT 1 FROM cst.ItemCost x WHERE x.CompanyCode = b.CompanyCode AND x.ItemCode = b.ItemCode
                     AND x.Revision = b.Revision AND x.SiteCode = b.SiteCode)
 GROUP BY b.CompanyCode, b.ItemCode, b.Revision, b.SiteCode, i.CostMethod, i.LifecycleStatus, c.BaseCost;
GO
