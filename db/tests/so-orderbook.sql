/*  Sales order book, billing queue and rebate tests.  These pin the arithmetic every sales
    enquiry, the backlog report and the invoice run depend on: what "outstanding" means, that a
    despatched quantity leaves the order book and joins the billing queue exactly once, that a
    cancelled order is out of the book altogether, and that an accrued rebate is a liability
    until it is settled.  Every assertion runs inside a transaction that is rolled back.
    REQ: SO-013, SO-014, SO-015, SO-019, SO-022, SO-027, RPT-008, D-03, D-12
    AC: a despatched line is never counted both as backlog and as something still to ship */
SET NOCOUNT ON;
GO
PRINT '# so-orderbook';
DECLARE @msg varchar(400), @dec decimal(18,2), @qty decimal(19,6), @n int, @co varchar(10) = 'STC',
        @ord varchar(30) = 'ZZ-TEST-SO-1', @d date = CAST(GETDATE() AS date), @id bigint, @lid bigint;

/*  1 - the backlog refuses a grouping the specification does not define rather than silently
    returning something else. */
BEGIN TRY
    EXEC so.usp_SalesBacklog @CompanyCode = @co, @GroupBy = 'SALESPERSON';
    PRINT 'not ok 1 - an unknown backlog grouping was accepted -- REQ: RPT-008';
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @msg LIKE '%STERP-E-1904%' PRINT 'ok 1 - the backlog groups only by ORDER, CUSTOMER, ITEM, SITE or PERIOD -- REQ: RPT-008';
    ELSE PRINT 'not ok 1 - unexpected error: ' + @msg;
END CATCH

/*  2 - outstanding is accepted less cancelled less despatched: 100 - 10 - 30 = 60, valued at
    the net price of 90.00 less 10% = 81.00, so 4,860.00 of backlog. */
BEGIN TRANSACTION;
INSERT so.SalesOrder (CompanyCode, SiteCode, OrderNumber, OrderType, CustomerCode, OrderDate, CurrencyCode,
                      FxRate, CommercialStatus, ReleaseStatus, FulfilmentStatus, BillingStatus, CreatedBy)
VALUES (@co, 'RDG', @ord, 'STANDARD', 'C0001', @d, 'GBP', 1, 'ACCEPTED', 'RELEASED', 'PARTDESPATCH', 'ELIGIBLE', 'TEST');
SELECT @id = SalesOrderId FROM so.SalesOrder WHERE CompanyCode = @co AND OrderNumber = @ord;
INSERT so.SalesOrderLine (SalesOrderId, CompanyCode, OrderNumber, LineNum, LineRef, ItemCode, Description, LineType,
                          AcceptedQty, CancelledQty, DespatchedQty, BilledQty, UomCode, UnitPrice, PricePerQty,
                          DiscountPct1, NetAmount, VatCode, VatRatePct, VatAmount, CostSnapshotGBP, Status)
VALUES (@id, @co, @ord, 1, '10', 'TYRE-700X32C', 'Tyre, test line', 'STOCK',
        100, 10, 30, 20, 'EA', 90.000000, 1, 10.0000, 4860.00, 'S', 20.0000, 972.00, 54.000000, 'OPEN');
SELECT @lid = SalesOrderLineId FROM so.SalesOrderLine WHERE SalesOrderId = @id AND LineNum = 1;
INSERT so.DeliverySchedule (SalesOrderLineId, CompanyCode, OrderNumber, LineNum, ScheduleNo, Quantity, DespatchedQty,
                            SiteCode, RequestedDate, OriginalPromiseDate, CurrentPromiseDate, ChangeCount, Status)
VALUES (@lid, @co, @ord, 1, 1, 90, 30, 'RDG', DATEADD(day, -5, @d), DATEADD(day, -5, @d), DATEADD(day, 3, @d), 1, 'PARTIAL');

SELECT @qty = OutstandingQty, @dec = BacklogValue FROM so.vw_OpenOrderBook
 WHERE CompanyCode = @co AND OrderNumber = @ord;
IF @qty = 60 AND @dec = 4860.00
    PRINT 'ok 2 - outstanding is accepted less cancelled less despatched, valued at the net price -- REQ: SO-015, RPT-008';
ELSE PRINT 'not ok 2 - outstanding ' + ISNULL(CONVERT(varchar(20), @qty), 'NULL') + ' valued '
         + ISNULL(CONVERT(varchar(20), @dec), 'NULL') + ', expected 60 and 4860.00 -- REQ: SO-015';

/*  3 - the despatched quantity that has not been billed is the billing queue, and it is not
    part of the backlog as well: 30 despatched less 20 billed = 10 to bill (D-03). */
SELECT @qty = UnbilledQty FROM so.vw_SalesOrderLineProgress
 WHERE CompanyCode = @co AND OrderNumber = @ord AND LineNum = 1;
IF @qty = 10 PRINT 'ok 3 - despatched less billed is 10, counted once and not as backlog -- REQ: SO-014, D-03';
ELSE PRINT 'not ok 3 - the unbilled quantity was ' + ISNULL(CONVERT(varchar(20), @qty), 'NULL') + ' -- REQ: SO-014';

/*  4 - a re-promised schedule keeps the original promise, so the slippage is still measurable
    after the promise has been moved (SO-008). */
SELECT @n = DATEDIFF(day, OriginalPromiseDate, CurrentPromiseDate) FROM so.vw_SalesOrderLineProgress
 WHERE CompanyCode = @co AND OrderNumber = @ord AND LineNum = 1;
IF @n = 8 PRINT 'ok 4 - the original promise survives a re-promise and the slip is 8 days -- REQ: SO-008';
ELSE PRINT 'not ok 4 - the slip was ' + ISNULL(CONVERT(varchar(10), @n), 'NULL') + ' days, expected 8 -- REQ: SO-008';

/*  5 - the header enquiry names what is blocking the order. */
UPDATE so.SalesOrder SET ReleaseStatus = 'CREDITHOLD',
       HoldReason = 'Test hold' WHERE SalesOrderId = @id;
SELECT @msg = Blocker FROM so.vw_SalesOrderStatus WHERE CompanyCode = @co AND OrderNumber = @ord;
IF @msg = 'Credit hold' PRINT 'ok 5 - an order on credit hold reports its blocker to the enquiry -- REQ: SO-003, SO-007';
ELSE PRINT 'not ok 5 - the blocker was ''' + ISNULL(@msg, 'NULL') + ''' -- REQ: SO-003';

/*  6 - a cancelled order leaves the order book entirely; the commitment no longer exists. */
UPDATE so.SalesOrder SET CommercialStatus = 'CANCELLED' WHERE SalesOrderId = @id;
SELECT @n = COUNT(*) FROM so.vw_OpenOrderBook WHERE CompanyCode = @co AND OrderNumber = @ord;
IF @n = 0 PRINT 'ok 6 - a cancelled order is out of the order book -- REQ: SO-016, RPT-008';
ELSE PRINT 'not ok 6 - ' + CONVERT(varchar(5), @n) + ' cancelled lines were still counted as backlog -- REQ: SO-016';
ROLLBACK TRANSACTION;

/*  7 - a confirmed despatch waits in the billing queue until the invoice run bills it, and
    leaves the queue when the billed quantity catches up. */
BEGIN TRANSACTION;
DECLARE @did bigint;
INSERT so.Despatch (CompanyCode, SiteCode, DespatchRef, CustomerCode, OrderNumber, DespatchDate, PostingDate,
                    Status, CreatedBy)
VALUES (@co, 'RDG', 'ZZ-TEST-DN-1', 'C0001', NULL, @d, @d, 'CONFIRMED', 'TEST');
SELECT @did = DespatchId FROM so.Despatch WHERE CompanyCode = @co AND DespatchRef = 'ZZ-TEST-DN-1';
INSERT so.DespatchLine (DespatchId, LineNum, ItemCode, Description, PickedQty, DespatchedQty, StockQty, BilledQty,
                        UomCode, UnitCostGBP, CogsGBP)
VALUES (@did, 1, 'TYRE-700X32C', 'Tyre, test despatch', 12, 12, 12, 0, 'EA', 21.400000, 256.80);
SELECT @qty = UnbilledQty FROM so.vw_DespatchUninvoiced WHERE CompanyCode = @co AND DespatchRef = 'ZZ-TEST-DN-1';
IF @qty = 12 PRINT 'ok 7 - a confirmed, unbilled despatch is in the invoice run queue -- REQ: SO-014';
ELSE PRINT 'not ok 7 - the queue showed ' + ISNULL(CONVERT(varchar(20), @qty), 'NULL') + ' -- REQ: SO-014';

UPDATE so.DespatchLine SET BilledQty = 12 WHERE DespatchId = @did AND LineNum = 1;
SELECT @n = COUNT(*) FROM so.vw_DespatchUninvoiced WHERE CompanyCode = @co AND DespatchRef = 'ZZ-TEST-DN-1';
IF @n = 0 PRINT 'ok 8 - a fully billed despatch leaves the queue, so it cannot be billed twice -- REQ: SO-014, COM-015';
ELSE PRINT 'not ok 8 - a billed despatch was still queued -- REQ: SO-014';

/*  9 - a draft despatch is not billable: billing follows confirmation, not preparation. */
UPDATE so.DespatchLine SET BilledQty = 0 WHERE DespatchId = @did AND LineNum = 1;
UPDATE so.Despatch SET Status = 'DRAFT' WHERE DespatchId = @did;
SELECT @n = COUNT(*) FROM so.vw_DespatchUninvoiced WHERE CompanyCode = @co AND DespatchRef = 'ZZ-TEST-DN-1';
IF @n = 0 PRINT 'ok 9 - an unconfirmed despatch is not offered to the invoice run -- REQ: SO-013, D-03';
ELSE PRINT 'not ok 9 - a draft despatch was offered for billing -- REQ: SO-013';
ROLLBACK TRANSACTION;

/* 10 - a rebate accrual is a liability until it is settled: 1,200.00 accrued less 500.00
    settled leaves 700.00 outstanding, and the turnover threshold is reported as met. */
BEGIN TRANSACTION;
DECLARE @rid int;
INSERT so.RebateAgreement (CompanyCode, RebateRef, CustomerCode, Description, Basis, ThresholdValue, RebatePct,
                           PeriodStart, PeriodEnd, CurrencyCode, Status, CreatedBy)
VALUES (@co, 'ZZ-TEST-REB-1', 'C0001', 'Test rebate', 'TURNOVER', 40000.000000, 3.0000,
        '2026-04-01', '2027-03-31', 'GBP', 'ACTIVE', 'TEST');
SELECT @rid = RebateId FROM so.RebateAgreement WHERE CompanyCode = @co AND RebateRef = 'ZZ-TEST-REB-1';
INSERT so.RebateAccrual (RebateId, CompanyCode, FiscalYear, PeriodNo, QualifyingRevenueGBP, QualifyingQty,
                         AccruedGBP, SettledGBP)
VALUES (@rid, @co, 2026, 6, 25000.00, 0, 750.00, 500.00),
       (@rid, @co, 2026, 7, 15000.00, 0, 450.00,   0.00);
SELECT @dec = OutstandingGBP, @n = ThresholdMet FROM so.vw_RebateStatus
 WHERE CompanyCode = @co AND RebateRef = 'ZZ-TEST-REB-1';
IF @dec = 700.00 AND @n = 1
    PRINT 'ok 10 - 1,200.00 accrued less 500.00 settled leaves 700.00 owed, threshold met -- REQ: SO-027, D-12';
ELSE PRINT 'not ok 10 - outstanding ' + ISNULL(CONVERT(varchar(20), @dec), 'NULL') + ', threshold flag '
         + ISNULL(CONVERT(varchar(5), @n), 'NULL') + ' -- REQ: SO-027';
ROLLBACK TRANSACTION;

/* 11 - the demonstration data really is in every state the enquiry filters on. */
SELECT @n = COUNT(DISTINCT CommercialStatus) FROM so.SalesOrder WHERE CreatedBy = 'SEED';
IF @n >= 4 PRINT 'ok 11 - the demonstration orders cover draft, accepted, amended and cancelled -- REQ: SO-002';
ELSE PRINT 'not ok 11 - only ' + CONVERT(varchar(5), @n) + ' commercial states are seeded -- REQ: SO-002';
SELECT @n = COUNT(DISTINCT ReleaseStatus) FROM so.SalesOrder WHERE CreatedBy = 'SEED';
IF @n >= 5 PRINT 'ok 12 - every release state (awaiting, credit, commercial, supply, released) is seeded -- REQ: SO-003';
ELSE PRINT 'not ok 12 - only ' + CONVERT(varchar(5), @n) + ' release states are seeded -- REQ: SO-003';
GO
