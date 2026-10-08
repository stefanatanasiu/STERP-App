/*  Stock control master-data extensions that the item and part maintenance module needs for its
    worked examples: the pack unit SLV with an effective-dated sleeve factor (25 pieces until
    September 2026, 20 from October 2026), two long codes that differ only near their end, a
    phantom and a draft stocked item, an ambiguous supplier alias, an approved substitute with a
    ratio other than one, and the packaging composition behind the packaging data report.
    All MERGE / IF NOT EXISTS, so the script is re-runnable.
    REQ: ITM-001, ITM-003, ITM-005, ITM-007, ITM-009, ITM-019, ITM-025, ITM-026, ITM-027, D-21 */
SET NOCOUNT ON;
GO
MERGE inv.Uom AS t
USING (VALUES ('SLV', 'Sleeve (item specific)', 'EACH', 'EA', 1.0, 1, 0)) AS s
      (UomCode, Name, UomDimension, BaseUomCode, FactorToBase, IsMetric, DecimalPlaces)
   ON t.UomCode = s.UomCode
WHEN NOT MATCHED THEN INSERT (UomCode, Name, UomDimension, BaseUomCode, FactorToBase, IsMetric, DecimalPlaces, IsActive)
     VALUES (s.UomCode, s.Name, s.UomDimension, s.BaseUomCode, s.FactorToBase, s.IsMetric, s.DecimalPlaces, 1);
GO
/*  Items.  LINK-QUICK-11S carries the specification's sleeve example; the two BRKT codes prove
    that similar long codes stay distinct; ITM-DRAFT-TESTRIG is deliberately incomplete.      */
MERGE inv.Item AS t
USING (VALUES
 ('LINK-QUICK-11S','Quick link, 11 speed, nickel plated','BOUGHT','DRIVETRAIN','EA','SLV','EA',1.0,1,0.004,'A','NONE','BUY','BOUGHT','MWA',1,1,0,1,'RELEASED','ENGINEER','STKGOODS','SALESGOODS'),
 ('BRKT-MECH-DISC-FLATMOUNT-160-A','Disc brake bracket, flat mount, 160 mm, type A','BOUGHT','BRAKES','EA','EA','EA',1.0,1,0.042,'A','NONE','BUY','BOUGHT','MWA',1,1,0,1,'RELEASED','ENGINEER','STKGOODS','SALESGOODS'),
 ('BRKT-MECH-DISC-FLATMOUNT-160-B','Disc brake bracket, flat mount, 160 mm, type B','BOUGHT','BRAKES','EA','EA','EA',1.0,1,0.043,'A','NONE','BUY','BOUGHT','MWA',1,1,0,1,'RELEASED','ENGINEER','STKGOODS','SALESGOODS'),
 ('ITM-DRAFT-TESTRIG','Test rig frame jig, draft - not yet reviewed','BOUGHT','TOOLING','EA','EA',NULL,1.0,1,12.500,'A','NONE','BUY','','MWA',1,0,0,1,'DRAFT','ENGINEER',NULL,NULL),
 ('HUB-REAR-148-BOOST-XD','Rear hub 148 mm boost, XD driver, sealed','BOUGHT','WHEELS','EA','EA','EA',1.0,1,0.312,'B','LOT','BUY','BOUGHT','MWA',1,1,0,1,'RELEASED','ENGINEER','STKGOODS','SALESGOODS')
) AS s (ItemCode, Description, ItemClass, Category, StockUomCode, PurchaseUomCode, SalesUomCode, QuantumQty, IsIndivisible,
        NetWeightKg, CurrentRevision, TraceabilityType, MakeBuy, ValuationGroup, CostMethod, IsPurchasable, IsSaleable,
        IsManufacturable, IsStocked, LifecycleStatus, StewardUser, InventoryAccountGroup, RevenueAccountGroup)
   ON t.CompanyCode = 'STC' AND t.ItemCode = s.ItemCode
WHEN NOT MATCHED THEN INSERT (CompanyCode, ItemCode, Description, ItemClass, Category, StockUomCode, PurchaseUomCode,
     SalesUomCode, QuantumQty, IsIndivisible, NetWeightKg, CurrentRevision, TraceabilityType, ShelfLifeDays,
     MakeBuy, ValuationGroup, CostMethod, IsPurchasable, IsSaleable, IsManufacturable, IsConsumable, IsStocked,
     LifecycleStatus, StewardUser, InventoryAccountGroup, RevenueAccountGroup, CreatedBy)
     VALUES ('STC', s.ItemCode, s.Description, s.ItemClass, s.Category, s.StockUomCode, s.PurchaseUomCode,
     s.SalesUomCode, s.QuantumQty, s.IsIndivisible, s.NetWeightKg, s.CurrentRevision, s.TraceabilityType,
     CASE WHEN s.ItemCode = 'HUB-REAR-148-BOOST-XD' THEN 365 ELSE NULL END,
     s.MakeBuy, s.ValuationGroup, s.CostMethod, s.IsPurchasable, s.IsSaleable, s.IsManufacturable, 1, s.IsStocked,
     s.LifecycleStatus, s.StewardUser, s.InventoryAccountGroup, s.RevenueAccountGroup, 'ENGINEER');
GO
INSERT inv.ItemRevision (CompanyCode, ItemCode, Revision, Description, EffectiveFrom, Status, IsInterchangeable, ApprovedBy, ApprovedAt, CreatedBy)
SELECT 'STC', i.ItemCode, i.CurrentRevision, i.Description, '2024-04-01', 'RELEASED', 1, 'ENGMGR', '2024-04-01T09:00:00', 'ENGINEER'
  FROM inv.Item i
 WHERE i.CompanyCode = 'STC' AND i.ItemCode IN ('LINK-QUICK-11S','BRKT-MECH-DISC-FLATMOUNT-160-A','BRKT-MECH-DISC-FLATMOUNT-160-B','HUB-REAR-148-BOOST-XD')
   AND NOT EXISTS (SELECT 1 FROM inv.ItemRevision r WHERE r.CompanyCode = 'STC' AND r.ItemCode = i.ItemCode AND r.Revision = i.CurrentRevision);
GO
/*  The sleeve.  One purchased sleeve held 25 pieces until the supplier changed the pack in
    October 2026; four sleeves bought before the change are 100 pieces and stay 100 pieces.  */
INSERT inv.ItemUom (CompanyCode, ItemCode, UomCode, UsageType, StockUnitsPerUom, PackDescription, EffectiveFrom, EffectiveTo, CreatedBy)
SELECT 'STC', v.ItemCode, v.UomCode, v.UsageType, v.Factor, v.Pack, v.FromDate, v.ToDate, 'SEED'
  FROM (VALUES
   ('LINK-QUICK-11S','EA','STOCK',1.0,'Stock unit', CAST('2024-04-01' AS date), CAST(NULL AS date)),
   ('LINK-QUICK-11S','SLV','PURCHASE',25.0,'Sleeve of twenty five', CAST('2024-04-01' AS date), CAST('2026-09-30' AS date)),
   ('LINK-QUICK-11S','SLV','PURCHASE',20.0,'Sleeve of twenty (new supplier pack)', CAST('2026-10-01' AS date), CAST(NULL AS date)),
   ('LINK-QUICK-11S','SLV','PRICE',25.0,'Priced per sleeve of twenty five', CAST('2024-04-01' AS date), CAST('2026-09-30' AS date)),
   ('HUB-REAR-148-BOOST-XD','EA','STOCK',1.0,'Stock unit', CAST('2024-04-01' AS date), CAST(NULL AS date)),
   ('BRKT-MECH-DISC-FLATMOUNT-160-A','EA','STOCK',1.0,'Stock unit', CAST('2024-04-01' AS date), CAST(NULL AS date)),
   ('BRKT-MECH-DISC-FLATMOUNT-160-B','EA','STOCK',1.0,'Stock unit', CAST('2024-04-01' AS date), CAST(NULL AS date))
  ) AS v (ItemCode, UomCode, UsageType, Factor, Pack, FromDate, ToDate)
 WHERE NOT EXISTS (SELECT 1 FROM inv.ItemUom x WHERE x.CompanyCode = 'STC' AND x.ItemCode = v.ItemCode
                     AND x.UomCode = v.UomCode AND x.UsageType = v.UsageType AND x.EffectiveFrom = v.FromDate);
GO
/*  UK VAT liability, customs data and compliance for the new items (ITM-025, ITM-026).       */
INSERT inv.ItemVatClass (CompanyCode, ItemCode, Direction, EffectiveFrom, VatCode, EvidenceRef, ApprovedBy, CreatedBy)
SELECT 'STC', v.ItemCode, d.Direction, '2024-04-01', 'S', 'Standard rated goods - VAT Act 1994', 'VATOWNER', 'SEED'
  FROM (VALUES ('LINK-QUICK-11S'),('BRKT-MECH-DISC-FLATMOUNT-160-A'),('BRKT-MECH-DISC-FLATMOUNT-160-B'),('HUB-REAR-148-BOOST-XD')) AS v (ItemCode)
 CROSS JOIN (VALUES ('SALE'),('PURCH')) AS d (Direction)
 WHERE NOT EXISTS (SELECT 1 FROM inv.ItemVatClass x WHERE x.CompanyCode = 'STC' AND x.ItemCode = v.ItemCode
                     AND x.Direction = d.Direction AND x.EffectiveFrom = '2024-04-01');
GO
INSERT inv.ItemCustoms (CompanyCode, ItemCode, EffectiveFrom, CommodityCode, CountryOfOrigin, PreferentialOrigin,
       PreferenceEvidenceRef, DutyRatePct, ExportControlled, SupplementaryUnit, NetWeightKgPerStockUnit, CreatedBy)
SELECT 'STC', v.ItemCode, '2024-04-01', v.Commodity, v.Origin, v.Pref, v.Evidence, v.Duty, 0, 'NAR', v.NetKg, 'SEED'
  FROM (VALUES
   ('LINK-QUICK-11S','7315119000','TW',0,NULL,4.7000,0.004),
   ('BRKT-MECH-DISC-FLATMOUNT-160-A','8714940000','DE',1,'EUR1/2024/00881',0.0000,0.042),
   ('BRKT-MECH-DISC-FLATMOUNT-160-B','8714940000','DE',1,'EUR1/2024/00881',0.0000,0.043),
   ('HUB-REAR-148-BOOST-XD','8714930000','TW',0,NULL,4.7000,0.312)
  ) AS v (ItemCode, Commodity, Origin, Pref, Evidence, Duty, NetKg)
 WHERE NOT EXISTS (SELECT 1 FROM inv.ItemCustoms x WHERE x.CompanyCode = 'STC' AND x.ItemCode = v.ItemCode AND x.EffectiveFrom = '2024-04-01');
GO
/*  Packaging composition.  AC-132: 1,000 units despatched, each with 0.05 kg (50 g) of plastic
    packaging at 30% recycled content, reconciles to 50 kg plastic of which 15 kg is recycled.  */
MERGE inv.ItemPackaging AS t
USING (VALUES
 ('HUB-REAR-148-BOOST-XD','PLASTIC','PRIMARY',50.0,30.0000,1),
 ('LINK-QUICK-11S','PAPER','PRIMARY',3.0,60.0000,0),
 ('BRKT-MECH-DISC-FLATMOUNT-160-A','FIBREBOARD','PRIMARY',18.0,78.0000,0),
 ('BRKT-MECH-DISC-FLATMOUNT-160-B','FIBREBOARD','PRIMARY',18.0,78.0000,0)
) AS s (ItemCode, PackagingMaterial, PackagingType, WeightGrams, RecycledContentPct, Ppt)
   ON t.CompanyCode = 'STC' AND t.ItemCode = s.ItemCode AND t.PackagingMaterial = s.PackagingMaterial AND t.PackagingType = s.PackagingType
WHEN MATCHED THEN UPDATE SET WeightGrams = s.WeightGrams, RecycledContentPct = s.RecycledContentPct, IsPlasticPackagingTax = s.Ppt
WHEN NOT MATCHED THEN INSERT (CompanyCode, ItemCode, PackagingMaterial, PackagingType, WeightGrams, RecycledContentPct,
     NationOfSupply, IsPlasticPackagingTax, CreatedBy)
     VALUES ('STC', s.ItemCode, s.PackagingMaterial, s.PackagingType, s.WeightGrams, s.RecycledContentPct, 'GB', s.Ppt, 'SEED');
GO
/*  An approved substitute whose ratio is not one: two 8 mm spacers replace one 16 mm spacer.
    AC-075 - a substitute is proposed with its own ratio, never silently swapped in.          */
MERGE inv.ItemSubstitute AS t
USING (VALUES
 ('BRKT-MECH-DISC-FLATMOUNT-160-A','BRKT-MECH-DISC-FLATMOUNT-160-B',2.000000000,0,0,1,1,1,'Engineering approval 2026/118: two type B brackets replace one type A on the 160 mm caliper.','ENGMGR')
) AS s (ItemCode, SubstituteItemCode, Ratio, CtxPurchase, CtxSales, CtxKit, CtxBom, NeedsAgreement, Restrictions, Approver)
   ON t.CompanyCode = 'STC' AND t.ItemCode = s.ItemCode AND t.SubstituteItemCode = s.SubstituteItemCode
WHEN MATCHED THEN UPDATE SET ConversionRatio = s.Ratio, ContextKit = s.CtxKit, ContextBom = s.CtxBom,
     RequiresCustomerAgreement = s.NeedsAgreement, Restrictions = s.Restrictions, ApprovedBy = s.Approver
WHEN NOT MATCHED THEN INSERT (CompanyCode, ItemCode, SubstituteItemCode, ConversionRatio, ContextPurchase, ContextSales,
     ContextKit, ContextBom, RequiresCustomerAgreement, Restrictions, ValidFrom, ApprovedBy, ApprovedAt, CreatedBy)
     VALUES ('STC', s.ItemCode, s.SubstituteItemCode, s.Ratio, s.CtxPurchase, s.CtxSales, s.CtxKit, s.CtxBom,
     s.NeedsAgreement, s.Restrictions, '2026-04-01', s.Approver, '2026-04-01T10:00:00', 'SEED');
GO
/*  Supplier references.  The last two rows are deliberately ambiguous: supplier SUP001 uses the
    same part number for two different internal items, which must block its use (AC-069).     */
INSERT inv.ItemPartner (CompanyCode, ItemCode, PartnerType, PartnerCode, PartnerItemCode, PartnerDescription,
       UomCode, StockUnitsPerUom, CurrencyCode, UnitPrice, PricePerQty, LeadTimeDays, MinOrderQty, OrderMultipleQty,
       PreferenceRank, IsApprovedSource, EffectiveFrom, CreatedBy)
SELECT 'STC', v.ItemCode, 'SUPPLIER', s.SupplierCode, v.PartnerItemCode, v.PartnerDescription,
       v.UomCode, v.Factor, 'GBP', v.Price, v.PricePer, v.Lead, v.MinQty, v.Mult, v.Rank, 1, '2024-04-01', 'SEED'
  FROM (VALUES
   ('LINK-QUICK-11S','QL-11N-SLEEVE','Quick link 11s nickel, sleeve of 25','SLV',25.0,75.000000,1.0,14,1,1,1),
   ('BRKT-MECH-DISC-FLATMOUNT-160-A','FM-160-A','Flat mount bracket 160 A','EA',1.0,4.250000,1.0,21,25,25,1),
   ('BRKT-MECH-DISC-FLATMOUNT-160-B','FM-160-A','Flat mount bracket 160 (same catalogue code)','EA',1.0,4.100000,1.0,21,25,25,2)
  ) AS v (ItemCode, PartnerItemCode, PartnerDescription, UomCode, Factor, Price, PricePer, Lead, MinQty, Mult, Rank)
 CROSS JOIN (SELECT TOP 1 SupplierCode FROM ap.Supplier WHERE CompanyCode = 'STC' ORDER BY SupplierCode) AS s
 WHERE NOT EXISTS (SELECT 1 FROM inv.ItemPartner x WHERE x.CompanyCode = 'STC' AND x.ItemCode = v.ItemCode
                     AND x.PartnerType = 'SUPPLIER' AND x.PartnerCode = s.SupplierCode AND x.QtyBreak = 0 AND x.EffectiveFrom = '2024-04-01');
GO
/*  Site planning data for the new items, including the purchase multiple that makes procurement
    rounding visible as excess supply rather than changed engineering usage (ITM-008).        */
INSERT inv.ItemSite (CompanyCode, SiteCode, ItemCode, DefaultWarehouseCode, DefaultLocationCode, MakeBuy, LeadTimeDays,
       MinOrderQty, PurchaseMultipleQty, ReorderPointQty, SafetyStockQty, MaxStockQty, AbcClass, CreatedBy)
SELECT 'STC', 'RDG', v.ItemCode, 'MAIN', 'STKA01', 'BUY', v.Lead, v.MinQty, v.Mult, v.Reorder, v.Safety, v.MaxQty, v.Abc, 'SEED'
  FROM (VALUES
   ('LINK-QUICK-11S',14,25.0,25.0,100.0,50.0,1000.0,'B'),
   ('BRKT-MECH-DISC-FLATMOUNT-160-A',21,25.0,25.0,40.0,20.0,400.0,'C'),
   ('BRKT-MECH-DISC-FLATMOUNT-160-B',21,25.0,25.0,40.0,20.0,400.0,'C'),
   ('HUB-REAR-148-BOOST-XD',28,10.0,10.0,30.0,10.0,200.0,'A')
  ) AS v (ItemCode, Lead, MinQty, Mult, Reorder, Safety, MaxQty, Abc)
 WHERE NOT EXISTS (SELECT 1 FROM inv.ItemSite x WHERE x.CompanyCode = 'STC' AND x.SiteCode = 'RDG' AND x.ItemCode = v.ItemCode);
GO
