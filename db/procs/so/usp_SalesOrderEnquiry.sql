/*  so.usp_SalesOrderEnquiry - the order enquiry behind the sales desk screen and the customer
    service telephone call: one header, its lines, its delivery schedules and its despatches, in
    four result sets.  It decides nothing.  Prices, VAT, margins, acceptance blockers and release
    state are all written by the COBOL programs; this procedure only reads them back.
    REQ: SO-003, SO-008, SO-011, SO-015, SO-019, SO-022, COM-002 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE so.usp_SalesOrderEnquiry
    @CompanyCode  varchar(10),
    @OrderNumber  varchar(30) = NULL,
    @CustomerCode varchar(20) = NULL,
    @SiteCode     varchar(10) = NULL,
    @FromDate     date        = NULL,
    @ToDate       date        = NULL,
    @Status       varchar(12) = 'ALL',
    @UserCode     varchar(20) = NULL,
    @MaxRows      int         = 200
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;

    IF @CompanyCode IS NULL OR @CompanyCode = ''
        THROW 51901, 'STERP-E-1901: A company must be given for a sales order enquiry.', 1;

    SET @Status = UPPER(ISNULL(NULLIF(@Status, ''), 'ALL'));
    IF @Status NOT IN ('ALL', 'OPEN', 'HELD', 'UNBILLED', 'CANCELLED')
        THROW 51902, 'STERP-E-1902: A sales order enquiry filters on ALL, OPEN, HELD, UNBILLED or CANCELLED.', 1;

    /*  An enquiry is read only but still authority sensitive - an order carries the customer's
        prices and terms (COM-002).  A user is only asked for when one is supplied, so batch
        reports and the COBOL programs can read the same procedure. */
    IF @UserCode IS NOT NULL AND core.fn_HasPermission(@UserCode, 'SO.ENQUIRY.VIEW', @CompanyCode, @SiteCode) = 0
        THROW 51903, 'STERP-E-1903: You are not permitted to enquire on sales orders in this company.', 1;

    SET @MaxRows = CASE WHEN ISNULL(@MaxRows, 0) BETWEEN 1 AND 5000 THEN @MaxRows ELSE 200 END;

    DECLARE @orders table (SalesOrderId bigint PRIMARY KEY, OrderNumber varchar(30));
    INSERT @orders (SalesOrderId, OrderNumber)
    SELECT TOP (@MaxRows) h.SalesOrderId, h.OrderNumber
      FROM so.SalesOrder h
     WHERE h.CompanyCode = @CompanyCode
       AND (@OrderNumber  IS NULL OR h.OrderNumber  = @OrderNumber)
       AND (@CustomerCode IS NULL OR h.CustomerCode = @CustomerCode)
       AND (@SiteCode     IS NULL OR h.SiteCode     = @SiteCode)
       AND (@FromDate     IS NULL OR h.OrderDate   >= @FromDate)
       AND (@ToDate       IS NULL OR h.OrderDate   <= @ToDate)
       AND (@Status = 'ALL'
         OR (@Status = 'OPEN'      AND h.CommercialStatus IN ('DRAFT','ACCEPTED','AMENDED')
                                   AND h.FulfilmentStatus <> 'DESPATCHED')
         OR (@Status = 'HELD'      AND h.ReleaseStatus IN ('CREDITHOLD','COMMERCIALHOLD','SUPPLYHOLD','AWAITAPPROVAL')
                                   AND h.CommercialStatus <> 'CANCELLED')
         OR (@Status = 'UNBILLED'  AND h.BillingStatus IN ('ELIGIBLE','PARTBILLED'))
         OR (@Status = 'CANCELLED' AND h.CommercialStatus = 'CANCELLED'))
     ORDER BY h.OrderDate DESC, h.OrderNumber DESC;

    /*  1 - headers with the enquiry summary (outstanding value, promise, what is blocking). */
    SELECT s.*
      FROM so.vw_SalesOrderStatus s
      JOIN @orders o ON o.OrderNumber = s.OrderNumber
     WHERE s.CompanyCode = @CompanyCode
     ORDER BY s.OrderDate DESC, s.OrderNumber DESC;

    /*  2 - lines with progress quantities and the price provenance PRICING stamped on them. */
    SELECT p.*
      FROM so.vw_SalesOrderLineProgress p
      JOIN @orders o ON o.SalesOrderId = (SELECT l.SalesOrderId FROM so.SalesOrderLine l
                                           WHERE l.SalesOrderLineId = p.SalesOrderLineId)
     ORDER BY p.OrderNumber, p.LineNum;

    /*  3 - delivery schedules: requested, original promise and current promise side by side, so
        the service measure cannot be improved by editing the promise (SO-008). */
    SELECT d.CompanyCode, d.OrderNumber, d.LineNum, d.ScheduleNo, d.Quantity, d.DespatchedQty,
           OutstandingQty = d.Quantity - d.DespatchedQty, d.SiteCode, d.DeliveryAddressRef,
           d.RequestedDate, d.OriginalPromiseDate, d.CurrentPromiseDate, d.ChangeReason,
           d.ChangeCount, d.FulfilmentMethod, d.Priority, d.Status,
           SlippageDays = DATEDIFF(day, d.OriginalPromiseDate, d.CurrentPromiseDate)
      FROM so.DeliverySchedule d
      JOIN @orders o ON o.OrderNumber = d.OrderNumber
     WHERE d.CompanyCode = @CompanyCode
     ORDER BY d.OrderNumber, d.LineNum, d.ScheduleNo;

    /*  4 - despatches and their billing state, newest first. */
    SELECT h.CompanyCode, h.DespatchRef, h.OrderNumber, h.DespatchDate, h.PostingDate, h.Status,
           h.CarrierCode, h.CarrierReference, h.IsExport, h.ExportEvidenceStatus, h.CogsPostedGBP,
           h.JournalId, dl.LineNum, dl.OrderLineNum, dl.ItemCode, dl.DespatchedQty, dl.BilledQty,
           UnbilledQty = dl.DespatchedQty - dl.BilledQty, dl.LotCode, dl.SerialNo, dl.CogsGBP
      FROM so.Despatch h
      JOIN so.DespatchLine dl ON dl.DespatchId = h.DespatchId
      JOIN @orders o ON o.OrderNumber = h.OrderNumber
     WHERE h.CompanyCode = @CompanyCode
     ORDER BY h.DespatchDate DESC, h.DespatchRef, dl.LineNum;
END
GO
PRINT 'Deployed so.usp_SalesOrderEnquiry';
GO
