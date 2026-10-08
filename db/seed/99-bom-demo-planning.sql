/*  The published bills-of-material examples depend on two pieces of planning data that the
    demonstration scripts of the other modules re-profile for their own purposes: the purchase
    multiple that makes B5 buy a pack of five, and the item standard cost column that BOMCOST
    falls back to when no cost set is named.  This script runs after those scripts and puts the
    documented figures back, so that a freshly built database reproduces the worked examples.
    REQ: BOM-011, BOM-026   AC: AC-093 */
SET NOCOUNT ON;
GO
/*  B5: DEMO-B5-PART is bought in packs of five.  Nothing else in the demonstration bills carries
    a purchase multiple, so the excess supply the example shows comes from this one row.  */
UPDATE isi
   SET PurchaseMultipleQty = 5, ModifiedBy = 'ENGINEER', ModifiedAt = SYSDATETIME()
  FROM inv.ItemSite isi
 WHERE isi.CompanyCode = 'STC' AND isi.SiteCode = 'RDG' AND isi.ItemCode = 'DEMO-B5-PART'
   AND ISNULL(isi.PurchaseMultipleQty, 0) <> 5;
PRINT 'Re-asserted the B5 purchase pack of five';
GO
/*  B1: the item standard cost column at Reading carries the same leaf rates as the BOMDEMO cost
    set, so the fallback route of BOMCOST reaches the published 100.10.  Valuation pools written
    against a revision of their own are left alone - BOMCOST ranks the item level first.  */
UPDATE ic
   SET StandardUnitCost = sc.TotalUnitCost, LastUpdatedAt = SYSDATETIME(), LastUpdatedBy = 'COSTACC'
  FROM cst.ItemCost ic
  JOIN cst.StandardCost sc ON sc.CompanyCode = ic.CompanyCode AND sc.ItemCode = ic.ItemCode
                          AND sc.CostSetCode = 'BOMDEMO' AND sc.Revision = ''
 WHERE ic.CompanyCode = 'STC' AND ic.SiteCode = 'RDG' AND ic.Revision = ''
   AND ISNULL(ic.StandardUnitCost, -1) <> sc.TotalUnitCost;
PRINT 'Re-asserted the B1 item standard cost column at Reading';
GO
