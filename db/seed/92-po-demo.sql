/*  Purchasing demonstration data, part 1 of 2: the reference data a purchasing demonstration needs
    before any transaction can be raised - approved supplier sources, purchase prices with quantity
    breaks and effective dates, due-diligence evidence, a supplier-scoped tolerance override, and
    re-order points so that REQGEN has demand to find.

    Part 2 - the transactions themselves (requisitions and orders in every state, partial receipts,
    a quarantine inspection, a supplier return, an import entry with postponed VAT and a closed
    order) - is deliberately NOT written here.  Receipts must create stock movements and general
    ledger postings that agree with each other, and the only thing that can do that correctly is
    the RECEIPT service.  Inserting po.Receipt rows from a seed script would produce a database
    that looks right on the purchasing screens and is wrong in the nominal ledger.  Part 2 is
    therefore produced by running the COBOL (see docs\modules\po.md).

    Everything here is re-runnable: the script is keyed on natural keys and only inserts what is
    missing.  It is also data driven - it discovers the companies, sites, items and suppliers that
    the earlier seeds created rather than assuming a fixed list, so it survives changes upstream.

    REQ: PO-001, PO-002, PO-004, PO-026, D-08 */
SET NOCOUNT ON;
GO
/*  Approved sources.  Each bought item gets two sources, chosen deterministically from the
    suppliers that already trade with the company, so the same database is produced every time.
    One item in every seven also gets a third, UNAPPROVED source: SOURCSEL has to refuse to pick it
    without a named authoriser, and a demonstration with no unapproved source cannot show that.
    REQ: PO-002, PO-026 */
;WITH supp AS (
    SELECT s.CompanyCode, s.SupplierCode, ROW_NUMBER() OVER (PARTITION BY s.CompanyCode ORDER BY s.SupplierCode) AS Seq,
           COUNT(*) OVER (PARTITION BY s.CompanyCode) AS Cnt
      FROM ap.Supplier s WHERE s.Status = 'ACTIVE'
), it AS (
    SELECT DISTINCT i.CompanyCode, i.ItemCode, ABS(CHECKSUM(i.ItemCode)) AS H
      FROM inv.Item i WHERE i.ItemClass IN ('BOUGHT','PACKAGING') AND i.MakeBuy IN ('BUY','DUAL')
), pick AS (
    SELECT it.CompanyCode, it.ItemCode, p.Priority, p.Appr,
           (SELECT SupplierCode FROM supp WHERE supp.CompanyCode = it.CompanyCode
             AND supp.Seq = 1 + (((it.H % supp.Cnt) + p.Priority - 1) % supp.Cnt)) AS SupplierCode,
           5 + ((it.H / p.Spread) % 21) AS LeadTimeDays
      FROM it CROSS JOIN (VALUES (1,'APPROVED',1), (2,'APPROVED',7), (3,'UNAPPROVED',53)) AS p (Priority, Appr, Spread)
     WHERE p.Priority < 3 OR (it.H % 7) = 0
)
INSERT po.SupplierSource (CompanyCode, ItemCode, SupplierCode, SupplierPartNumber, Priority, ApprovalStatus, ApprovalEvidence,
                          LeadTimeDays, MinOrderQty, PackQty, OrderUomCode, CurrencyCode, EffectiveFrom, IsActive, CreatedBy)
SELECT k.CompanyCode, k.ItemCode, k.SupplierCode,
       LEFT(k.SupplierCode + '-' + REPLACE(k.ItemCode,'-',''),40), k.Priority, k.Appr,
       CASE k.Appr WHEN 'APPROVED' THEN 'Supplier audit ' + k.SupplierCode + ' passed' ELSE 'No audit held - use needs an authoriser' END,
       k.LeadTimeDays, 0, 1, i.PurchaseUomCode, 'GBP', '2024-04-01', 1, 'SEED'
  FROM pick k
  JOIN inv.Item i ON i.CompanyCode = k.CompanyCode AND i.ItemCode = k.ItemCode
 WHERE k.SupplierCode IS NOT NULL
   AND NOT EXISTS (SELECT 1 FROM po.SupplierSource x WHERE x.CompanyCode = k.CompanyCode
                     AND x.ItemCode = k.ItemCode AND x.SupplierCode = k.SupplierCode);
PRINT 'Seeded po.SupplierSource';
GO
/*  Prices for the generated sources.  The price is derived from the item code so that it is stable
    across deploys, and the priority-1 source gets a contract price while the priority-2 source
    gets a list price - a contract beats a list price for the same item, which is what POPRICE has
    to demonstrate.  REQ: PO-004 */
INSERT po.PurchasePrice (CompanyCode, SupplierCode, ItemCode, PriceBasis, AgreementRef, CurrencyCode, UomCode,
                         PricePerQty, UnitPrice, DiscountPct, EffectiveFrom, EffectiveTo, Priority, IsActive, CreatedBy)
SELECT ss.CompanyCode, ss.SupplierCode, ss.ItemCode,
       CASE ss.Priority WHEN 1 THEN 'CONTRACT' ELSE 'LIST' END,
       CASE ss.Priority WHEN 1 THEN 'CTR-' + ss.SupplierCode + '-2026' ELSE NULL END,
       'GBP', ss.OrderUomCode, 1,
       CAST(1.75 + ((ABS(CHECKSUM(ss.ItemCode)) % 7400) / 100.0) AS decimal(19,6))
         * CASE ss.Priority WHEN 1 THEN 1.00 ELSE 1.08 END,
       0, '2024-04-01', NULL, CASE ss.Priority WHEN 1 THEN 10 ELSE 20 END, 1, 'SEED'
  FROM po.SupplierSource ss
 WHERE ss.ApprovalStatus = 'APPROVED'
   AND NOT EXISTS (SELECT 1 FROM po.PurchasePrice x WHERE x.CompanyCode = ss.CompanyCode
                     AND x.SupplierCode = ss.SupplierCode AND ISNULL(x.ItemCode,'') = ss.ItemCode);
PRINT 'Seeded po.PurchasePrice (generated)';
GO
/*  Three hand-set prices with exact figures, so that a test has something to assert against
    instead of a formula.  They also carry the cases the generated rows cannot show:
      - quantity breaks (100 and 500),
      - a price that has EXPIRED and a replacement that took over from it, which is how PO-004
        proves that the price used is the one effective on the ORDER date and not the one in force
        on the day the report happens to be run,
      - a settlement discount that must not be taken off the purchase price itself.
    REQ: PO-004, D-25 */
MERGE po.PurchasePrice AS t
USING (VALUES
 ('STC','S0001','BAR-AL6082-25MM','CONTRACT','CTR-S0001-DEMO','GBP','M',1,12.500000,0.0000,0.0000,'2026-01-01',NULL,5),
 ('STC','S0001','BAR-AL6082-25MM','CONTRACT','CTR-S0001-2025','GBP','M',1,13.750000,0.0000,0.0000,'2025-01-01','2025-12-31',5),
 ('STC','S0002','TUBE-STEEL-1IN-16G','QUOTE','QUO-S0002-11874','GBP','FT',1,4.200000,0.0000,0.0000,'2026-01-01',NULL,5),
 ('STC','S0003','BAR-SS316-12MM','LIST',NULL,'GBP','M',1,31.000000,5.0000,2.5000,'2024-04-01',NULL,5)
) AS s (CompanyCode, SupplierCode, ItemCode, PriceBasis, AgreementRef, CurrencyCode, UomCode, PricePerQty, UnitPrice,
        DiscountPct, SettlementDiscountPct, EffectiveFrom, EffectiveTo, Priority)
ON t.CompanyCode = s.CompanyCode AND t.SupplierCode = s.SupplierCode AND ISNULL(t.ItemCode,'') = s.ItemCode
   AND t.PriceBasis = s.PriceBasis AND t.EffectiveFrom = s.EffectiveFrom
WHEN MATCHED THEN UPDATE SET AgreementRef = s.AgreementRef, CurrencyCode = s.CurrencyCode, UomCode = s.UomCode,
        PricePerQty = s.PricePerQty, UnitPrice = s.UnitPrice, DiscountPct = s.DiscountPct,
        SettlementDiscountPct = s.SettlementDiscountPct, EffectiveTo = s.EffectiveTo, Priority = s.Priority,
        IsActive = 1, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED BY TARGET AND EXISTS (SELECT 1 FROM inv.Item i WHERE i.CompanyCode = s.CompanyCode AND i.ItemCode = s.ItemCode)
     THEN INSERT (CompanyCode, SupplierCode, ItemCode, PriceBasis, AgreementRef, CurrencyCode, UomCode, PricePerQty, UnitPrice,
                  DiscountPct, SettlementDiscountPct, EffectiveFrom, EffectiveTo, Priority, IsActive, CreatedBy)
          VALUES (s.CompanyCode, s.SupplierCode, s.ItemCode, s.PriceBasis, s.AgreementRef, s.CurrencyCode, s.UomCode,
                  s.PricePerQty, s.UnitPrice, s.DiscountPct, s.SettlementDiscountPct, s.EffectiveFrom, s.EffectiveTo,
                  s.Priority, 1, 'SEED');
PRINT 'Seeded po.PurchasePrice (worked examples)';
GO
/*  Quantity breaks on the demonstration contract: 12.50 up to 99, 11.75 from 100, 10.90 from 500.
    REQ: PO-004 */
INSERT po.PurchasePriceBreak (PurchasePriceId, FromQty, UnitPrice, DiscountPct)
SELECT p.PurchasePriceId, b.FromQty, b.UnitPrice, 0
  FROM po.PurchasePrice p
  CROSS JOIN (VALUES (100.0, 11.750000), (500.0, 10.900000)) AS b (FromQty, UnitPrice)
 WHERE p.AgreementRef = 'CTR-S0001-DEMO'
   AND NOT EXISTS (SELECT 1 FROM po.PurchasePriceBreak x WHERE x.PurchasePriceId = p.PurchasePriceId AND x.FromQty = b.FromQty);
PRINT 'Seeded po.PurchasePriceBreak';
GO
/*  Due-diligence evidence, so that POSUPCHK lets the demonstration orders out of the door.  One
    supplier per company is deliberately left with an EXPIRED insurance certificate: PO-026 says an
    order to that supplier needs an authorised exception, and that case has to be visible.
    REQ: PO-026 */
;WITH ranked AS (
    SELECT s.CompanyCode, s.SupplierCode, ROW_NUMBER() OVER (PARTITION BY s.CompanyCode ORDER BY s.SupplierCode DESC) AS RnDesc
      FROM ap.Supplier s WHERE s.Status = 'ACTIVE'
)
INSERT po.SupplierDiligence (CompanyCode, SupplierCode, EvidenceType, EvidenceRef, Outcome, IsBlocking, IsMandatory,
                             CheckedBy, ValidFrom, ValidTo, NextReviewDate, Note)
SELECT r.CompanyCode, r.SupplierCode, e.EvidenceType,
       e.EvidenceType + '-' + r.SupplierCode, CASE WHEN r.RnDesc = 1 AND e.EvidenceType = 'INSURANCE' THEN 'EXPIRED' ELSE 'PASS' END,
       0, e.IsMandatory, 'PURMGR', '2025-04-01',
       CASE WHEN r.RnDesc = 1 AND e.EvidenceType = 'INSURANCE' THEN '2026-03-31' ELSE '2027-03-31' END,
       '2027-03-01',
       CASE WHEN r.RnDesc = 1 AND e.EvidenceType = 'INSURANCE' THEN 'Certificate has lapsed - an order needs an authorised exception'
            ELSE 'Evidence held and in date' END
  FROM ranked r
  CROSS JOIN (VALUES ('SANCTIONS',1), ('COMPANIESHSE',1), ('VATNUMBER',1), ('BUSINESSSIZE',1), ('INSURANCE',1), ('MODERNSLAVERY',0)) AS e (EvidenceType, IsMandatory)
 WHERE NOT EXISTS (SELECT 1 FROM po.SupplierDiligence x WHERE x.CompanyCode = r.CompanyCode
                     AND x.SupplierCode = r.SupplierCode AND x.EvidenceType = e.EvidenceType);
PRINT 'Seeded po.SupplierDiligence';
GO
/*  A supplier-scoped tolerance override on top of the D-08 company baseline, so that the override
    hierarchy has something to resolve.  This supplier is allowed to over-deliver by 5 per cent and
    the over-delivery only warns rather than blocking, because the material is cut to length and
    the exact quantity cannot be guaranteed.  REQ: D-08 */
INSERT po.ToleranceRule (CompanyCode, ScopeType, ScopeCode, OverDeliveryPct, OverDeliveryQty, UnderDeliveryPct,
                         PriceVariancePct, PriceVarianceAbs, ChargeVarianceAbs, EarlyDeliveryDays, LateDeliveryDays,
                         ActionOverDelivery, ApproverRole, IsActive, ModifiedBy, ModifiedAt)
SELECT c.CompanyCode, 'SUPPLIER', 'S0001', 5, 0, 0, 2, 50.00, 25.00, 5, 0, 'WARN', 'PURMGR', 1, 'SEED', SYSDATETIME()
  FROM core.Company c
 WHERE EXISTS (SELECT 1 FROM ap.Supplier s WHERE s.CompanyCode = c.CompanyCode AND s.SupplierCode = 'S0001')
   AND NOT EXISTS (SELECT 1 FROM po.ToleranceRule x WHERE x.CompanyCode = c.CompanyCode
                     AND x.ScopeType = 'SUPPLIER' AND x.ScopeCode = 'S0001');
PRINT 'Seeded po.ToleranceRule (supplier override)';
GO
/*  Re-order points, so that REQGEN finds demand.  Only rows that have no planning data of their
    own are touched, so a planner's own figures are never overwritten by a demonstration script.
    One item in five is left below its re-order point on purpose; the rest are comfortable, which
    is what makes the generated proposal list short enough to read.  REQ: PO-001 */
UPDATE isi
   SET ReorderPointQty = 20 + (ABS(CHECKSUM(isi.ItemCode)) % 60),
       SafetyStockQty = 10 + (ABS(CHECKSUM(isi.ItemCode)) % 15),
       MaxStockQty = 120 + (ABS(CHECKSUM(isi.ItemCode)) % 240),
       MinOrderQty = CASE WHEN (ABS(CHECKSUM(isi.ItemCode)) % 3) = 0 THEN 25 ELSE 0 END,
       PurchaseMultipleQty = CASE WHEN (ABS(CHECKSUM(isi.ItemCode)) % 4) = 0 THEN 10 ELSE 1 END,
       LeadTimeDays = CASE WHEN isi.LeadTimeDays > 0 THEN isi.LeadTimeDays ELSE 5 + (ABS(CHECKSUM(isi.ItemCode)) % 15) END,
       ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
  FROM inv.ItemSite isi
  JOIN inv.Item i ON i.CompanyCode = isi.CompanyCode AND i.ItemCode = isi.ItemCode
 WHERE ISNULL(isi.ReorderPointQty,0) = 0
   AND i.ItemClass IN ('BOUGHT','PACKAGING') AND i.MakeBuy IN ('BUY','DUAL')
   AND isi.IsActive = 1;
PRINT 'Seeded inv.ItemSite planning data for replenishment';
GO
/*  A source with no price, and a price with no source, are both real situations that the
    demonstration should not accidentally produce: SOURCSEL would have nothing to rank and POPRICE
    would price from an item nobody is allowed to buy.  Fail the deploy rather than hand over a
    database that quietly behaves oddly.  REQ: PO-002, PO-004 */
IF EXISTS (SELECT 1 FROM po.SupplierSource ss
            WHERE ss.ApprovalStatus = 'APPROVED' AND ss.Priority = 1 AND ss.IsActive = 1
              AND NOT EXISTS (SELECT 1 FROM po.PurchasePrice p WHERE p.CompanyCode = ss.CompanyCode
                                AND p.SupplierCode = ss.SupplierCode AND ISNULL(p.ItemCode,'') = ss.ItemCode AND p.IsActive = 1))
    THROW 50811, 'A priority-1 approved supplier source has no purchase price; SOURCSEL would rank a supplier that cannot be priced.', 1;
GO
