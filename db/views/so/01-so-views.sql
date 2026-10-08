/*  Sales order enquiry views.  Everything here is read only arithmetic over what the COBOL
    engine has already decided: the order book is accepted less cancelled less despatched, the
    billing queue is despatched less billed, and the rebate position is accrued less settled.
    No view decides anything - prices, VAT, margins, releases and postings all come from the
    sales programs (PRICING, SODISC, SOVAT, SOACCEPT, DESPATCH, SOINVRUN, REBATE).
    Document money is converted with the order's own snapshotted FX rate, which is held as
    foreign units per GBP (D-14), so GBP = document / rate.
    REQ: SO-003, SO-008, SO-012, SO-013, SO-014, SO-015, SO-019, SO-022, SO-023, SO-027, RPT-008 */
SET NOCOUNT ON;
GO
/*  Line level progress.  Outstanding quantity is the contract balance a planner cares about:
    what the customer has accepted, less what has been cancelled, less what has left the
    building.  Returned quantity is reported but never added back to the order - a return is a
    new document, it does not reopen a closed line (D-12).  REQ: SO-015, SO-019, RPT-008 */
CREATE OR ALTER VIEW so.vw_SalesOrderLineProgress AS
SELECT  h.CompanyCode, h.SiteCode, h.OrderNumber, h.OrderType, h.CustomerCode, h.CustomerOrderRef,
        h.OrderDate, h.CurrencyCode, h.FxRate, h.SalespersonCode, h.CommercialStatus, h.ReleaseStatus,
        h.FulfilmentStatus, h.BillingStatus, l.SalesOrderLineId, l.LineNum, l.LineRef, l.LineType,
        l.ItemCode, l.Revision, l.Description, l.UomCode, l.AcceptedQty, l.CancelledQty, l.DespatchedQty,
        l.BilledQty, l.ReturnedQty,
        OutstandingQty   = l.AcceptedQty - l.CancelledQty - l.DespatchedQty,
        UnbilledQty      = l.DespatchedQty - l.BilledQty,
        l.UnitPrice, l.PricePerQty, l.PriceSource, l.PriceListCode, l.DiscountPct1, l.DiscountPct2,
        l.NetAmount, l.VatCode, l.VatRatePct, l.VatAmount, l.SupplyType, l.LinkedPoNumber, l.Status,
        UnitNet          = CASE WHEN ISNULL(l.PricePerQty, 0) = 0 THEN l.UnitPrice
                                ELSE l.UnitPrice / l.PricePerQty END
                           * (1 - l.DiscountPct1 / 100.0) * (1 - l.DiscountPct2 / 100.0),
        l.CostSnapshotGBP, l.CostBasis, l.MarginPct, l.JobRef, l.DimCostCentre, l.DimProject,
        s.RequestedDate, s.OriginalPromiseDate, s.CurrentPromiseDate,
        PromiseChanges   = ISNULL(cc.Changes, 0)
  FROM  so.SalesOrder h
  JOIN  so.SalesOrderLine l ON l.SalesOrderId = h.SalesOrderId
 OUTER APPLY (SELECT TOP (1) d.RequestedDate, d.OriginalPromiseDate, d.CurrentPromiseDate
                FROM so.DeliverySchedule d
               WHERE d.SalesOrderLineId = l.SalesOrderLineId AND d.Status <> 'CANCELLED'
               ORDER BY d.ScheduleNo) s
 OUTER APPLY (SELECT Changes = SUM(d.ChangeCount) FROM so.DeliverySchedule d
               WHERE d.SalesOrderLineId = l.SalesOrderLineId) cc;
GO
/*  The open order book - the demand side of RPT-008.  A line is in the book while it still owes
    the customer goods and has not been cancelled; a held order stays in the book because the
    commitment exists whether or not the warehouse may act on it (SO-003).  REQ: RPT-008, SO-022 */
CREATE OR ALTER VIEW so.vw_OpenOrderBook AS
SELECT  p.CompanyCode, p.SiteCode, p.OrderNumber, p.OrderType, p.CustomerCode, p.OrderDate,
        p.CurrencyCode, p.SalespersonCode, p.ReleaseStatus, p.FulfilmentStatus, p.LineNum, p.LineRef,
        p.ItemCode, p.Description, p.UomCode, p.OutstandingQty, p.RequestedDate,
        p.OriginalPromiseDate, p.CurrentPromiseDate, p.PromiseChanges, p.SupplyType, p.LineType,
        BacklogValue    = CAST(p.OutstandingQty * p.UnitNet AS decimal(18,2)),
        BacklogValueGBP = CAST(p.OutstandingQty * p.UnitNet / NULLIF(p.FxRate, 0) AS decimal(18,2)),
        IsLate          = CASE WHEN p.CurrentPromiseDate < CAST(SYSDATETIME() AS date) THEN 1 ELSE 0 END,
        IsRescheduled   = CASE WHEN p.CurrentPromiseDate > p.OriginalPromiseDate THEN 1 ELSE 0 END
  FROM  so.vw_SalesOrderLineProgress p
 WHERE  p.Status = 'OPEN' AND p.CommercialStatus <> 'CANCELLED'
   AND  p.OutstandingQty > 0;
GO
/*  Header enquiry.  The blocker column is a readable summary of why nothing is moving; the
    authoritative list of acceptance blockers is produced by SOACCEPT, this is only what an
    enquiry screen shows in a status bar.  REQ: SO-003, SO-007, SO-011, SO-019 */
CREATE OR ALTER VIEW so.vw_SalesOrderStatus AS
SELECT  h.CompanyCode, h.SiteCode, h.OrderNumber, h.OrderType, h.CustomerCode,
        CustomerName = ISNULL(c.TradingName, c.LegalName), h.CustomerOrderRef, h.OrderDate,
        h.QuoteNumber, h.CurrencyCode, h.FxRate, h.TermsCode, h.SalespersonCode, h.IncotermsRule,
        h.IncotermsNamedPlace, h.RetentionOfTitle, h.NetAmount, h.VatAmount, h.GrossAmount,
        h.GrossAmountGBP, h.CommercialStatus, h.ReleaseStatus, h.FulfilmentStatus, h.BillingStatus,
        h.HoldReason, h.DepositRequiredGBP, h.DepositReceivedGBP, h.AcknowledgementIssuedAt,
        a.LineCount, a.OpenLineCount,
        OutstandingValue = CAST(a.OutstandingValue AS decimal(18,2)),
        a.UnbilledQty, a.EarliestPromise, LatePromiseCount = a.LateCount,
        Blocker          = CASE WHEN h.CommercialStatus = 'CANCELLED'      THEN 'Cancelled'
                                WHEN h.ReleaseStatus = 'CREDITHOLD'        THEN 'Credit hold'
                                WHEN h.ReleaseStatus = 'COMMERCIALHOLD'    THEN 'Commercial hold'
                                WHEN h.ReleaseStatus = 'SUPPLYHOLD'        THEN 'Supply hold'
                                WHEN h.ReleaseStatus = 'AWAITAPPROVAL'     THEN 'Awaiting acceptance'
                                WHEN h.DepositRequiredGBP > h.DepositReceivedGBP THEN 'Deposit outstanding'
                                ELSE '' END
  FROM  so.SalesOrder h
  LEFT  JOIN ar.Customer c ON c.CompanyCode = h.CompanyCode AND c.CustomerCode = h.CustomerCode
 CROSS  APPLY (SELECT LineCount = COUNT(*),
                      OpenLineCount = SUM(CASE WHEN l.Status = 'OPEN' THEN 1 ELSE 0 END),
                      OutstandingValue = SUM(CASE WHEN l.Status = 'OPEN'
                           THEN (l.AcceptedQty - l.CancelledQty - l.DespatchedQty)
                                * CASE WHEN ISNULL(l.PricePerQty,0) = 0 THEN l.UnitPrice
                                       ELSE l.UnitPrice / l.PricePerQty END
                                * (1 - l.DiscountPct1/100.0) * (1 - l.DiscountPct2/100.0)
                           ELSE 0 END),
                      UnbilledQty = SUM(l.DespatchedQty - l.BilledQty),
                      EarliestPromise = (SELECT MIN(d.CurrentPromiseDate) FROM so.DeliverySchedule d
                                          WHERE d.CompanyCode = h.CompanyCode AND d.OrderNumber = h.OrderNumber
                                            AND d.Status IN ('OPEN','PARTIAL')),
                      LateCount = (SELECT COUNT(*) FROM so.DeliverySchedule d
                                    WHERE d.CompanyCode = h.CompanyCode AND d.OrderNumber = h.OrderNumber
                                      AND d.Status IN ('OPEN','PARTIAL')
                                      AND d.CurrentPromiseDate < CAST(SYSDATETIME() AS date))
                 FROM so.SalesOrderLine l WHERE l.SalesOrderId = h.SalesOrderId) a;
GO
/*  What the invoice run will pick up.  Billing follows despatch, never the order (D-03), so the
    queue is despatch lines whose billed quantity has not caught up with what was shipped, on a
    confirmed despatch only.  REQ: SO-013, SO-014 */
CREATE OR ALTER VIEW so.vw_DespatchUninvoiced AS
SELECT  d.CompanyCode, d.SiteCode, d.DespatchRef, d.CustomerCode, d.OrderNumber, d.DespatchDate,
        d.PostingDate, d.IsExport, d.ExportEvidenceStatus, d.Status, dl.DespatchLineId, dl.LineNum,
        dl.OrderLineNum, dl.ItemCode, dl.Description, dl.DespatchedQty, dl.BilledQty,
        UnbilledQty = dl.DespatchedQty - dl.BilledQty, dl.UomCode, dl.CogsGBP,
        l.UnitPrice, l.PricePerQty, l.DiscountPct1, l.DiscountPct2, l.VatCode, l.VatRatePct,
        h.CurrencyCode, h.FxRate, h.TermsCode, h.InvoiceCustomerCode,
        UnbilledValue = CAST((dl.DespatchedQty - dl.BilledQty)
                        * CASE WHEN ISNULL(l.PricePerQty,0) = 0 THEN ISNULL(l.UnitPrice,0)
                               ELSE l.UnitPrice / l.PricePerQty END
                        * (1 - ISNULL(l.DiscountPct1,0)/100.0) * (1 - ISNULL(l.DiscountPct2,0)/100.0)
                        AS decimal(18,2))
  FROM  so.Despatch d
  JOIN  so.DespatchLine dl ON dl.DespatchId = d.DespatchId
  LEFT  JOIN so.SalesOrderLine l ON l.SalesOrderLineId = dl.SalesOrderLineId
  LEFT  JOIN so.SalesOrder h ON h.SalesOrderId = l.SalesOrderId
 WHERE  d.Status = 'CONFIRMED' AND dl.DespatchedQty > dl.BilledQty;
GO
/*  Rebate position.  An accrual is variable consideration already charged against revenue, so
    the outstanding liability is what has been accrued and not yet settled by credit note.
    REQ: SO-027, D-12 */
CREATE OR ALTER VIEW so.vw_RebateStatus AS
SELECT  r.CompanyCode, r.RebateRef, r.CustomerCode, r.Description, r.ItemCode, r.ProductGroup,
        r.Basis, r.ThresholdValue, r.RebatePct, r.RebatePerUnit, r.PeriodStart, r.PeriodEnd,
        r.CurrencyCode, r.Status, r.SettlementCreditNote,
        QualifyingRevenueGBP = ISNULL(a.Revenue, 0), QualifyingQty = ISNULL(a.Qty, 0),
        AccruedGBP = ISNULL(a.Accrued, 0), SettledGBP = ISNULL(a.Settled, 0),
        OutstandingGBP = ISNULL(a.Accrued, 0) - ISNULL(a.Settled, 0),
        ThresholdMet = CASE WHEN r.ThresholdValue IS NULL THEN 1
                            WHEN r.Basis = 'QUANTITY' AND ISNULL(a.Qty, 0) >= r.ThresholdValue THEN 1
                            WHEN r.Basis <> 'QUANTITY' AND ISNULL(a.Revenue, 0) >= r.ThresholdValue THEN 1
                            ELSE 0 END,
        PeriodCount = ISNULL(a.Periods, 0)
  FROM  so.RebateAgreement r
  LEFT  JOIN (SELECT RebateId, Revenue = SUM(QualifyingRevenueGBP), Qty = SUM(QualifyingQty),
                     Accrued = SUM(AccruedGBP), Settled = SUM(SettledGBP), Periods = COUNT(*)
                FROM so.RebateAccrual GROUP BY RebateId) a ON a.RebateId = r.RebateId;
GO
/*  The price lists an enquiry screen shows, in the order PRICING walks them: level 2 contract,
    3 customer, 4 group, 5 standard, with the largest qualifying quantity break first.  Level 1
    is a manual price and never lives in a list.  REQ: SO-004 */
CREATE OR ALTER VIEW so.vw_PriceListEffective AS
SELECT  pl.CompanyCode, pl.PriceListCode, pl.Name, pl.PriceLevel, pl.Scope, pl.CustomerCode,
        pl.PriceGroup, pl.ContractRef, pl.IsActive, ListValidFrom = pl.ValidFrom, ListValidTo = pl.ValidTo,
        pll.PriceLineId, pll.ItemCode, pll.UomCode, pll.QtyBreak, pll.CurrencyCode, pll.UnitPrice,
        pll.PricePerQty, pll.DiscountPct, pll.MinimumMarginPct, pll.ValidFrom, pll.ValidTo,
        NetUnitPrice = CAST(pll.UnitPrice / NULLIF(pll.PricePerQty, 0)
                            * (1 - pll.DiscountPct / 100.0) AS decimal(19,6))
  FROM  so.PriceList pl
  JOIN  so.PriceListLine pll ON pll.CompanyCode = pl.CompanyCode
                            AND pll.PriceListCode = pl.PriceListCode;
GO
PRINT 'Deployed so views';
GO
