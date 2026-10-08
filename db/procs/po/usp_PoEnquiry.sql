/*  Purchasing enquiries (PO-024).  Three questions a buyer asks every morning - what is still
    open, what have we committed and not yet spent, and what is late - plus the commitment
    balance view that stops every caller inventing its own stage arithmetic.
    REQ: PO-008, PO-017, PO-022, PO-024, D-08 */
SET NOCOUNT ON;
GO
/*  po.Commitment is an append-only movement table.  COMMITTED adds, RELEASED and CANCELLED take
    away; ACCRUED and INVOICED are the SAME money moving on and are already in the nominal
    ledger, so they are reported separately and never added to the open commitment. */
CREATE OR ALTER VIEW po.vw_CommitmentBalance
AS
SELECT c.CompanyCode, c.OrderNumber, c.LineNum, c.BudgetCode, c.AccountCode, c.DimCostCentre,
       c.DimProject, c.FiscalYear,
       OpenCommitmentGBP = SUM(CASE c.Stage
            WHEN 'COMMITTED' THEN c.AmountGBP + c.NonRecoverableVatGBP + c.ChargeGBP
            WHEN 'RELEASED'  THEN -(c.AmountGBP + c.NonRecoverableVatGBP + c.ChargeGBP)
            WHEN 'CANCELLED' THEN -(c.AmountGBP + c.NonRecoverableVatGBP + c.ChargeGBP)
            ELSE 0 END),
       AccruedGBP  = SUM(CASE WHEN c.Stage = 'ACCRUED'  THEN c.AmountGBP + c.ChargeGBP ELSE 0 END),
       InvoicedGBP = SUM(CASE WHEN c.Stage = 'INVOICED' THEN c.AmountGBP + c.ChargeGBP ELSE 0 END),
       CommittedQty = SUM(CASE c.Stage WHEN 'COMMITTED' THEN c.Quantity
                                       WHEN 'RELEASED'  THEN -c.Quantity
                                       WHEN 'CANCELLED' THEN -c.Quantity ELSE 0 END),
       MovementCount = COUNT_BIG(*), LastMovementDate = MAX(c.MovementDate)
  FROM po.Commitment c
 GROUP BY c.CompanyCode, c.OrderNumber, c.LineNum, c.BudgetCode, c.AccountCode, c.DimCostCentre,
          c.DimProject, c.FiscalYear;
GO
/*  Open purchase order enquiry.  Outstanding quantity follows section 6 of the specification:
    max(0, ordered - cancelled - received), so an accepted over-delivery leaves zero outstanding
    rather than a negative commitment. */
CREATE OR ALTER PROCEDURE po.usp_OpenOrders
    @CompanyCode varchar(10), @SiteCode varchar(10) = NULL, @SupplierCode varchar(20) = NULL,
    @BuyerUser varchar(20) = NULL, @OrderNumber varchar(30) = NULL, @IncludeClosed bit = 0,
    @MaxRows int = 500
AS
BEGIN
    SET NOCOUNT ON;
    SELECT TOP (@MaxRows)
           o.OrderNumber, o.OrderType, o.SiteCode, o.SupplierCode, o.BuyerUser, o.OrderDate,
           o.CurrencyCode, o.Status, o.DeliveryStatus, o.MatchStatus, o.TermsCode,
           o.TermsSnapshot, o.IncotermsRule, o.NetAmount, o.VatAmount, o.GrossAmount,
           o.CommitmentGBP, o.ApprovedBy, o.ApprovedAt, o.IssuedAt, o.AcknowledgedAt,
           o.BlanketOrderNumber, o.BudgetCode,
           LineCount        = t.LineCount,
           OutstandingQty   = t.OutstandingQty,
           OutstandingGBP   = t.OutstandingGBP,
           ReceivedNotInvoicedGBP = t.GrniGBP,
           EarliestDueDate  = s.EarliestDue,
           LatestDueDate    = s.LatestDue,
           OverdueLines     = s.OverdueLines,
           IsCommitment     = CASE WHEN o.Status IN ('ISSUED', 'ACKNOWLEDGED', 'AMENDED')
                                   THEN CAST(1 AS bit) ELSE CAST(0 AS bit) END
      FROM po.PurchaseOrder o
     CROSS APPLY (
           SELECT LineCount = COUNT(*),
                  OutstandingQty = SUM(CASE WHEN l.OrderedQty - l.CancelledQty - l.ReceivedQty > 0
                                            THEN l.OrderedQty - l.CancelledQty - l.ReceivedQty
                                            ELSE 0 END),
                  OutstandingGBP = SUM(CASE WHEN l.OrderedQty > 0
                                            THEN l.NetAmount *
                                                 (CASE WHEN l.OrderedQty - l.CancelledQty - l.ReceivedQty > 0
                                                       THEN l.OrderedQty - l.CancelledQty - l.ReceivedQty
                                                       ELSE 0 END) / l.OrderedQty
                                            ELSE 0 END),
                  GrniGBP = SUM(CASE WHEN l.OrderedQty > 0 AND l.ReceivedQty - l.InvoicedQty > 0
                                     THEN l.NetAmount * (l.ReceivedQty - l.InvoicedQty)
                                          / l.OrderedQty
                                     ELSE 0 END)
             FROM po.PurchaseOrderLine l
            WHERE l.PurchaseOrderId = o.PurchaseOrderId AND l.Status <> 'CANCELLED') t
     OUTER APPLY (
           SELECT EarliestDue = MIN(ISNULL(sc.CurrentConfirmedDate, sc.RequestedDate)),
                  LatestDue   = MAX(ISNULL(sc.CurrentConfirmedDate, sc.RequestedDate)),
                  OverdueLines = SUM(CASE WHEN sc.Status IN ('OPEN', 'PARTIAL')
                                           AND ISNULL(sc.CurrentConfirmedDate, sc.RequestedDate)
                                               < CAST(SYSDATETIME() AS date)
                                          THEN 1 ELSE 0 END)
             FROM po.PurchaseSchedule sc
             JOIN po.PurchaseOrderLine l2 ON l2.PurchaseOrderLineId = sc.PurchaseOrderLineId
            WHERE l2.PurchaseOrderId = o.PurchaseOrderId) s
     WHERE o.CompanyCode = @CompanyCode
       AND (@SiteCode IS NULL OR o.SiteCode = @SiteCode)
       AND (@SupplierCode IS NULL OR o.SupplierCode = @SupplierCode)
       AND (@BuyerUser IS NULL OR o.BuyerUser = @BuyerUser)
       AND (@OrderNumber IS NULL OR o.OrderNumber = @OrderNumber)
       AND (@IncludeClosed = 1 OR o.Status NOT IN ('CLOSED', 'CANCELLED'))
     ORDER BY s.EarliestDue, o.OrderNumber;
END
GO
/*  Commitment and GRNI enquiry.  The three stages are shown side by side precisely so that
    nobody adds them together: the same purchase is a commitment, then an accrual, then a
    payable, and only one of those is true at a time. */
CREATE OR ALTER PROCEDURE po.usp_CommitmentEnquiry
    @CompanyCode varchar(10), @BudgetCode varchar(20) = NULL, @CostCentre varchar(20) = NULL,
    @AccountCode varchar(12) = NULL, @FiscalYear smallint = NULL, @SupplierCode varchar(20) = NULL,
    @GroupBy varchar(12) = 'ORDER'
AS
BEGIN
    SET NOCOUNT ON;
    IF @GroupBy NOT IN ('ORDER', 'BUDGET', 'SUPPLIER')
        THROW 50801, 'STERP-E-0801: Group the commitment enquiry by ORDER, BUDGET or SUPPLIER.', 1;

    ;WITH b AS (
        SELECT v.OrderNumber, v.BudgetCode, v.AccountCode, v.DimCostCentre, v.FiscalYear,
               v.OpenCommitmentGBP, v.AccruedGBP, v.InvoicedGBP, v.LastMovementDate,
               o.SupplierCode
          FROM po.vw_CommitmentBalance v
          LEFT JOIN po.PurchaseOrder o ON o.CompanyCode = v.CompanyCode
                                      AND o.OrderNumber = v.OrderNumber
         WHERE v.CompanyCode = @CompanyCode
           AND (@BudgetCode IS NULL OR v.BudgetCode = @BudgetCode)
           AND (@CostCentre IS NULL OR v.DimCostCentre = @CostCentre)
           AND (@AccountCode IS NULL OR v.AccountCode = @AccountCode)
           AND (@FiscalYear IS NULL OR v.FiscalYear = @FiscalYear)
           AND (@SupplierCode IS NULL OR o.SupplierCode = @SupplierCode))
    SELECT GroupKey = CASE @GroupBy WHEN 'BUDGET' THEN ISNULL(b.BudgetCode, '(none)')
                                    WHEN 'SUPPLIER' THEN ISNULL(b.SupplierCode, '(none)')
                                    ELSE b.OrderNumber END,
           BudgetCode = MAX(b.BudgetCode), CostCentre = MAX(b.DimCostCentre),
           AccountCode = MAX(b.AccountCode), SupplierCode = MAX(b.SupplierCode),
           FiscalYear = MAX(b.FiscalYear),
           OpenCommitmentGBP = SUM(b.OpenCommitmentGBP),
           AccruedGBP = SUM(b.AccruedGBP),
           InvoicedGBP = SUM(b.InvoicedGBP),
           TotalOrderedGBP = SUM(b.OpenCommitmentGBP + b.AccruedGBP + b.InvoicedGBP),
           LineCount = COUNT(*), LastMovementDate = MAX(b.LastMovementDate)
      FROM b
     GROUP BY CASE @GroupBy WHEN 'BUDGET' THEN ISNULL(b.BudgetCode, '(none)')
                            WHEN 'SUPPLIER' THEN ISNULL(b.SupplierCode, '(none)')
                            ELSE b.OrderNumber END
    HAVING SUM(b.OpenCommitmentGBP) <> 0 OR SUM(b.AccruedGBP) <> 0
     ORDER BY SUM(b.OpenCommitmentGBP) DESC;
END
GO
/*  Overdue schedules (PO-022).  Lateness is measured against BOTH promise bases: the original
    confirmed date the supplier first gave us and the current one after any agreed changes.
    A supplier who moves the date five times is on time on the current basis and very late on
    the original one, and the report refuses to hide either number. */
CREATE OR ALTER PROCEDURE po.usp_OverdueSchedules
    @CompanyCode varchar(10), @AsOfDate date = NULL, @SupplierCode varchar(20) = NULL,
    @SiteCode varchar(10) = NULL, @MinDaysLate int = 1, @MaxRows int = 500
AS
BEGIN
    SET NOCOUNT ON;
    SET @AsOfDate = ISNULL(@AsOfDate, CAST(SYSDATETIME() AS date));
    SELECT TOP (@MaxRows)
           o.OrderNumber, o.SupplierCode, o.SiteCode, o.BuyerUser, l.LineNum, sc.ScheduleNo,
           l.ItemCode, l.Description, sc.Quantity, sc.ReceivedQty,
           OutstandingQty = CASE WHEN sc.Quantity - sc.ReceivedQty > 0
                                 THEN sc.Quantity - sc.ReceivedQty ELSE 0 END,
           sc.RequestedDate, sc.OriginalConfirmedDate, sc.CurrentConfirmedDate,
           sc.ChangeCount,
           PromisedDateOriginal = COALESCE(sc.OriginalConfirmedDate, sc.RequestedDate),
           PromisedDateCurrent  = COALESCE(sc.CurrentConfirmedDate, sc.OriginalConfirmedDate,
                                           sc.RequestedDate),
           DaysLateOriginalBasis = DATEDIFF(day, COALESCE(sc.OriginalConfirmedDate,
                                                          sc.RequestedDate), @AsOfDate),
           DaysLateCurrentBasis  = DATEDIFF(day, COALESCE(sc.CurrentConfirmedDate,
                                                          sc.OriginalConfirmedDate,
                                                          sc.RequestedDate), @AsOfDate),
           ValueAtRiskGBP = CASE WHEN l.OrderedQty > 0
                                 THEN l.NetAmount * (CASE WHEN sc.Quantity - sc.ReceivedQty > 0
                                                          THEN sc.Quantity - sc.ReceivedQty
                                                          ELSE 0 END) / l.OrderedQty
                                 ELSE 0 END,
           DestinationSiteCode = COALESCE(sc.DestinationSiteCode, l.DestinationSiteCode, o.SiteCode),
           LinkedSalesOrderNumber = l.LinkedSalesOrderNumber,
           o.Status, sc.Status AS ScheduleStatus
      FROM po.PurchaseSchedule sc
      JOIN po.PurchaseOrderLine l ON l.PurchaseOrderLineId = sc.PurchaseOrderLineId
      JOIN po.PurchaseOrder o ON o.PurchaseOrderId = l.PurchaseOrderId
     WHERE o.CompanyCode = @CompanyCode
       AND o.Status IN ('ISSUED', 'ACKNOWLEDGED', 'AMENDED')
       AND sc.Status IN ('OPEN', 'PARTIAL')
       AND l.Status = 'OPEN'
       AND sc.Quantity - sc.ReceivedQty > 0
       AND (@SupplierCode IS NULL OR o.SupplierCode = @SupplierCode)
       AND (@SiteCode IS NULL OR o.SiteCode = @SiteCode)
       AND DATEDIFF(day, COALESCE(sc.CurrentConfirmedDate, sc.OriginalConfirmedDate,
                                  sc.RequestedDate), @AsOfDate) >= @MinDaysLate
     ORDER BY DATEDIFF(day, COALESCE(sc.CurrentConfirmedDate, sc.OriginalConfirmedDate,
                                     sc.RequestedDate), @AsOfDate) DESC, o.OrderNumber;
END
GO
