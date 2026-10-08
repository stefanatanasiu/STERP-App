/*  Item detail: unit of measure conversions, revisions, VAT classification, customs data,
    packaging data and approved substitutes.  Derived from the item master so it stays in step.
    REQ: ITM-005 to ITM-012, ITM-016, ITM-017, FIN-016, D-02, D-09, D-20, D-21 */
SET NOCOUNT ON;
GO
/*  Explicit pack sizes first; the generic rule below fills in the dimensional conversions. */
INSERT inv.ItemUom (CompanyCode, ItemCode, UomCode, UsageType, StockUnitsPerUom, PackDescription, EffectiveFrom, CreatedBy)
SELECT 'STC', v.ItemCode, v.UomCode, v.UsageType, v.Factor, v.Pack, '2024-04-01', 'SEED'
  FROM (VALUES
   ('ADHESIVE-EP420-250ML','BOX','PURCHASE',12.0,'Box of twelve cartridges'),
   ('GREASE-LI2-400G','BOX','PURCHASE',10.0,'Box of ten cartridges'),
   ('THREADLOCK-243-50ML','BOX','PURCHASE',24.0,'Box of twenty four bottles'),
   ('FLUX-AL-500G','BOX','PURCHASE',6.0,'Box of six tubs'),
   ('PKG-STRETCHWRAP-500','BOX','PURCHASE',6.0,'Box of six rolls'),
   ('BOLT-M5X16-SS','PK100','PRICE',100.0,'Priced per hundred'),
   ('SPOKE-DB-294MM','PK100','PRICE',100.0,'Priced per hundred'),
   ('TUBE-AL6061-31.8X1.6','M','ISSUE',1.0,'Issued by the metre'),
   ('SA-WHEEL-FRONT-700C','SET','PACK',2.0,'Front and rear wheel pair')
  ) AS v (ItemCode, UomCode, UsageType, Factor, Pack)
 WHERE EXISTS (SELECT 1 FROM inv.Item i WHERE i.CompanyCode = 'STC' AND i.ItemCode = v.ItemCode)
   AND NOT EXISTS (SELECT 1 FROM inv.ItemUom u WHERE u.CompanyCode = 'STC' AND u.ItemCode = v.ItemCode AND u.UomCode = v.UomCode);
/*  Stock unit for every item, then purchase and sales units converted through inv.Uom. */
INSERT inv.ItemUom (CompanyCode, ItemCode, UomCode, UsageType, StockUnitsPerUom, EffectiveFrom, CreatedBy)
SELECT i.CompanyCode, i.ItemCode, i.StockUomCode, 'STOCK', 1.0, '2024-04-01', 'SEED'
  FROM inv.Item i
 WHERE NOT EXISTS (SELECT 1 FROM inv.ItemUom u WHERE u.CompanyCode = i.CompanyCode AND u.ItemCode = i.ItemCode AND u.UomCode = i.StockUomCode);
INSERT inv.ItemUom (CompanyCode, ItemCode, UomCode, UsageType, StockUnitsPerUom, PackDescription, EffectiveFrom, CreatedBy)
SELECT i.CompanyCode, i.ItemCode, i.PurchaseUomCode, 'PURCHASE', ROUND(pu.FactorToBase / su.FactorToBase, 9),
       CASE WHEN pu.IsMetric = 0 THEN 'Approved imperial conversion' END, '2024-04-01', 'SEED'
  FROM inv.Item i
  JOIN inv.Uom su ON su.UomCode = i.StockUomCode
  JOIN inv.Uom pu ON pu.UomCode = i.PurchaseUomCode AND pu.UomDimension = su.UomDimension
 WHERE i.PurchaseUomCode <> i.StockUomCode
   AND NOT EXISTS (SELECT 1 FROM inv.ItemUom u WHERE u.CompanyCode = i.CompanyCode AND u.ItemCode = i.ItemCode AND u.UomCode = i.PurchaseUomCode);
INSERT inv.ItemUom (CompanyCode, ItemCode, UomCode, UsageType, StockUnitsPerUom, EffectiveFrom, CreatedBy)
SELECT i.CompanyCode, i.ItemCode, i.SalesUomCode, 'SALES', ROUND(nu.FactorToBase / su.FactorToBase, 9), '2024-04-01', 'SEED'
  FROM inv.Item i
  JOIN inv.Uom su ON su.UomCode = i.StockUomCode
  JOIN inv.Uom nu ON nu.UomCode = i.SalesUomCode AND nu.UomDimension = su.UomDimension
 WHERE i.SalesUomCode IS NOT NULL AND i.SalesUomCode <> i.StockUomCode
   AND NOT EXISTS (SELECT 1 FROM inv.ItemUom u WHERE u.CompanyCode = i.CompanyCode AND u.ItemCode = i.ItemCode AND u.UomCode = i.SalesUomCode);
GO
/*  Revision history: the current revision plus the superseded revisions that preceded it. */
INSERT inv.ItemRevision (CompanyCode, ItemCode, Revision, Description, EffectiveFrom, DrawingRef, Status,
                         IsInterchangeable, ApprovedBy, ApprovedAt, CreatedBy)
SELECT i.CompanyCode, i.ItemCode, i.CurrentRevision, 'Current released revision', '2025-04-01',
       'DRG-' + LEFT(REPLACE(i.ItemCode, '.', ''), 24) + '-' + i.CurrentRevision,
       CASE WHEN i.LifecycleStatus = 'DRAFT' THEN 'DRAFT' WHEN i.LifecycleStatus = 'RETIRED' THEN 'RETIRED' ELSE 'RELEASED' END,
       1, CASE WHEN i.LifecycleStatus <> 'DRAFT' THEN 'ENGMGR' END,
       CASE WHEN i.LifecycleStatus <> 'DRAFT' THEN CAST('2025-04-01T09:00:00' AS datetime2(3)) END, 'SEED'
  FROM inv.Item i
 WHERE NOT EXISTS (SELECT 1 FROM inv.ItemRevision r WHERE r.CompanyCode = i.CompanyCode AND r.ItemCode = i.ItemCode AND r.Revision = i.CurrentRevision);
INSERT inv.ItemRevision (CompanyCode, ItemCode, Revision, Description, EffectiveFrom, EffectiveTo, Status,
                         IsInterchangeable, ChangeReason, ApprovedBy, ApprovedAt, CreatedBy)
SELECT i.CompanyCode, i.ItemCode, p.Rev, 'Superseded revision', '2024-04-01', '2025-04-01', 'SUPERSEDED',
       CASE WHEN p.Rev = 'A' THEN 1 ELSE 0 END, 'Design improvement, engineering change', 'ENGMGR', '2024-04-01T09:00:00', 'SEED'
  FROM inv.Item i
  CROSS APPLY (VALUES ('A'), ('B'), ('C')) AS p (Rev)
 WHERE p.Rev < i.CurrentRevision
   AND NOT EXISTS (SELECT 1 FROM inv.ItemRevision r WHERE r.CompanyCode = i.CompanyCode AND r.ItemCode = i.ItemCode AND r.Revision = p.Rev);
GO
/*  VAT classification.  Everything is standard rated unless the item is an export service or a
    specified good inside the domestic reverse charge (D-02, D-20). */
INSERT inv.ItemVatClass (CompanyCode, ItemCode, Direction, EffectiveFrom, VatCode, ApprovedBy, CreatedBy)
SELECT i.CompanyCode, i.ItemCode, 'SALE', '2024-04-01',
       CASE WHEN i.ItemCode = 'SVC-CARRIAGE-EXPORT' THEN 'EXP' ELSE 'S' END, 'VATOWNER', 'SEED'
  FROM inv.Item i
 WHERE i.IsSaleable = 1
   AND NOT EXISTS (SELECT 1 FROM inv.ItemVatClass v WHERE v.CompanyCode = i.CompanyCode AND v.ItemCode = i.ItemCode AND v.Direction = 'SALE');
INSERT inv.ItemVatClass (CompanyCode, ItemCode, Direction, EffectiveFrom, VatCode, ApprovedBy, CreatedBy)
SELECT i.CompanyCode, i.ItemCode, 'PURCH', '2024-04-01', 'S', 'VATOWNER', 'SEED'
  FROM inv.Item i
 WHERE i.IsPurchasable = 1
   AND NOT EXISTS (SELECT 1 FROM inv.ItemVatClass v WHERE v.CompanyCode = i.CompanyCode AND v.ItemCode = i.ItemCode AND v.Direction = 'PURCH');
UPDATE inv.Item SET IsDrcSpecifiedGoods = 1, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
 WHERE ItemCode IN ('CTRL-EBIKE-250W', 'DISPLAY-LCD-C3', 'BATTERY-36V-14AH') AND IsDrcSpecifiedGoods = 0;
UPDATE inv.Item SET UkcaMarked = 1, ConformityDocRef = 'DOC-' + LEFT(ItemCode, 20), ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
 WHERE Category IN ('ELECTRONIC', 'BICYCLE') AND UkcaMarked = 0;
UPDATE inv.Item SET DangerousGoodsUn = 'UN3480', DangerousGoodsClass = '9', DangerousGoodsPackGroup = 'II',
       BatteryCategory = 'INDUSTRIAL', ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
 WHERE ItemCode = 'BATTERY-36V-14AH' AND DangerousGoodsUn IS NULL;
UPDATE inv.Item SET DangerousGoodsUn = 'UN1133', DangerousGoodsClass = '3', DangerousGoodsPackGroup = 'II',
       ReachStatus = 'REGISTERED', ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
 WHERE ItemCode IN ('ADHESIVE-EP420-250ML', 'PRIMER-PR10-1L') AND DangerousGoodsUn IS NULL;
UPDATE inv.Item SET WeeeCategory = '6', ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
 WHERE Category = 'ELECTRONIC' AND WeeeCategory IS NULL;
GO
/*  Customs data for purchased items: commodity code, origin and preference evidence. */
INSERT inv.ItemCustoms (CompanyCode, ItemCode, EffectiveFrom, CommodityCode, CountryOfOrigin, PreferentialOrigin,
                        PreferenceEvidenceRef, DutyRatePct, ExportControlled, SupplementaryUnit, NetWeightKgPerStockUnit, CreatedBy)
SELECT i.CompanyCode, i.ItemCode, '2024-04-01',
       CASE i.Category WHEN 'TUBING' THEN '76081000' WHEN 'BARSTOCK' THEN '76042910' WHEN 'SHEET' THEN '76061292'
                       WHEN 'FASTENER' THEN '73181562' WHEN 'BEARING' THEN '84821010' WHEN 'CONSUMABLE' THEN '35061000'
                       WHEN 'ELECTRONIC' THEN '85076000' WHEN 'DRIVETRAIN' THEN '87149310' WHEN 'BRAKING' THEN '87149410'
                       WHEN 'WHEEL' THEN '87149200' WHEN 'FINISHING' THEN '87149990' WHEN 'FRAME' THEN '87149110'
                       WHEN 'SUBASSY' THEN '87149990' WHEN 'BICYCLE' THEN '87120030' WHEN 'PACKAGING' THEN '48191000'
                       ELSE '87149990' END,
       CASE WHEN i.Category IN ('ELECTRONIC', 'DRIVETRAIN') THEN 'TW' WHEN i.Category = 'BEARING' THEN 'DE'
            WHEN i.Category IN ('TUBING', 'BARSTOCK', 'SHEET') THEN 'GB' ELSE 'GB' END,
       CASE WHEN i.Category IN ('BEARING', 'TUBING') THEN 1 ELSE 0 END,
       CASE WHEN i.Category IN ('BEARING', 'TUBING') THEN 'SUPPDEC-2026-' + LEFT(i.Category, 4) END,
       CASE WHEN i.Category IN ('ELECTRONIC', 'DRIVETRAIN') THEN 4.7000 WHEN i.Category = 'BEARING' THEN 8.0000 ELSE 0.0000 END,
       CASE WHEN i.Category = 'ELECTRONIC' THEN 1 ELSE 0 END, 'KG', i.NetWeightKg, 'SEED'
  FROM inv.Item i
 WHERE i.IsPurchasable = 1 AND i.IsStocked = 1
   AND NOT EXISTS (SELECT 1 FROM inv.ItemCustoms c WHERE c.CompanyCode = i.CompanyCode AND c.ItemCode = i.ItemCode);
GO
/*  Packaging data for extended producer responsibility reporting (D-20). */
INSERT inv.ItemPackaging (CompanyCode, ItemCode, PackagingMaterial, PackagingType, WeightGrams, RecycledContentPct,
                          NationOfSupply, IsPlasticPackagingTax, CreatedBy)
SELECT 'STC', v.ItemCode, v.Material, v.PackType, v.Grams, v.Recycled, 'ENG', v.Ppt, 'SEED'
  FROM (VALUES
   ('PKG-BOX-FRAME-L','FIBREBOARD','SECONDARY',1240.0,78.0,0),
   ('PKG-BOX-FRAME-M','FIBREBOARD','SECONDARY',1080.0,78.0,0),
   ('PKG-FOAM-INSERT','PLASTIC','PRIMARY',85.0,12.0,1),
   ('PKG-PALLET-EUR','WOOD','TRANSIT',22000.0,0.0,0),
   ('PKG-STRETCHWRAP-500','PLASTIC','TRANSIT',2400.0,30.0,1),
   ('PKG-LABEL-SHIP','PAPER','PRIMARY',2.0,60.0,0)
  ) AS v (ItemCode, Material, PackType, Grams, Recycled, Ppt)
 WHERE EXISTS (SELECT 1 FROM inv.Item i WHERE i.CompanyCode = 'STC' AND i.ItemCode = v.ItemCode)
   AND NOT EXISTS (SELECT 1 FROM inv.ItemPackaging p WHERE p.CompanyCode = 'STC' AND p.ItemCode = v.ItemCode AND p.PackagingMaterial = v.Material);
GO
/*  Approved substitutes (equal or higher specification only, D-11). */
INSERT inv.ItemSubstitute (CompanyCode, ItemCode, SubstituteItemCode, ConversionRatio, ContextPurchase, ContextSales,
                           ContextKit, ContextBom, RequiresCustomerAgreement, Restrictions, ValidFrom, ApprovedBy, ApprovedAt, CreatedBy)
SELECT 'STC', v.ItemCode, v.Sub, v.Ratio, v.P, v.S, v.K, v.B, v.Agree, v.Note, '2026-01-01', 'QUALITY', '2026-01-05T10:00:00', 'SEED'
  FROM (VALUES
   ('BEARING-6802-2RS','BEARING-6803-2RS',1.0,1,0,1,1,0,'Dimensionally interchangeable, higher load rating'),
   ('BOLT-M5X16-SS','BOLT-M6X20-TI',1.0,0,0,1,0,1,'Customer agreement required, different thread'),
   ('BRAKE-DISC-160MM','BRAKE-DISC-180MM',1.0,0,0,1,0,1,'Larger rotor, frame clearance must be confirmed'),
   ('TUBE-AL6061-28X1.2','TUBE-AL6061-28X1.5',1.0,1,0,1,1,0,'Withdrawn item replaced by the thicker wall'),
   ('PAINT-2K-BLACK-5L','PAINT-2K-RED-5L',1.0,0,0,0,1,1,'Colour change needs written customer agreement')
  ) AS v (ItemCode, Sub, Ratio, P, S, K, B, Agree, Note)
 WHERE EXISTS (SELECT 1 FROM inv.Item i WHERE i.CompanyCode = 'STC' AND i.ItemCode = v.ItemCode)
   AND EXISTS (SELECT 1 FROM inv.Item i WHERE i.CompanyCode = 'STC' AND i.ItemCode = v.Sub)
   AND NOT EXISTS (SELECT 1 FROM inv.ItemSubstitute s WHERE s.CompanyCode = 'STC' AND s.ItemCode = v.ItemCode AND s.SubstituteItemCode = v.Sub);
GO
