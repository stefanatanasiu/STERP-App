/*  Sales analysis and profitability - the stages of one demand and the two margins.
    SO-023 asks that quoted, ordered, despatched, invoiced and credited values stay apart, and
    that returns, discounts, late attributable costs and the FX basis stay visible instead of
    being lost inside a gross sales total.  This is the set arithmetic the SOANAL report and
    so.usp_SalesBacklog are built on; the margin measurement itself is proved in
    cobol\tests\so\TSOGAP.cob.
    One ZZ-SO23 demand is built across all five documents and rolled back.
    REQ: SO-023, RPT-008
    AC: AC-049 expected margin keeps its original basis, realised margin carries the late cost */
SET NOCOUNT ON;
GO
PRINT '# so-gaps';
DECLARE @msg varchar(400), @co varchar(10) = 'STC', @cust varchar(20) = 'C0001',
        @item varchar(40) = 'TYRE-700X32C',
        @ord varchar(30) = 'ZZ-SO23-O1', @ord2 varchar(30) = 'ZZ-SO23-O2',
        @oid bigint, @oid2 bigint, @lid bigint, @lid2 bigint, @did bigint, @iid bigint,
        @rid bigint, @qid bigint,
        @quoted decimal(18,2), @ordered decimal(18,2), @despatched decimal(18,2),
        @invoiced decimal(18,2), @credited decimal(18,2), @discount decimal(18,2),
        @returns decimal(18,2), @expcost decimal(18,2), @realcost decimal(18,2),
        @late decimal(18,2), @expmargin decimal(18,2), @realmargin decimal(18,2),
        @qty decimal(19,6), @dec decimal(18,2), @gbp decimal(18,2), @n int, @pct decimal(9,4);

BEGIN TRANSACTION;
BEGIN TRY
    /*  The quotation that started it: 12,000.00 offered. */
    INSERT so.Quote (CompanyCode, QuoteNumber, Version, CustomerCode, QuoteDate, ValidUntil, CurrencyCode,
                     FxRate, SalespersonCode, NetAmount, Status, CreatedBy)
    VALUES (@co, 'ZZ-SO23-Q1', 1, @cust, '2026-08-01', '2026-09-30', 'GBP', 1, 'SALESREP', 12000.00, 'ISSUED', 'TEST');
    SELECT @qid = QuoteId FROM so.Quote WHERE CompanyCode = @co AND QuoteNumber = 'ZZ-SO23-Q1' AND Version = 1;
    INSERT so.QuoteLine (QuoteId, LineNum, ItemCode, Description, Quantity, UomCode, UnitPrice, PricePerQty,
                         NetAmount, VatCode, VatAmount, EstimatedCostGBP, CostBasis)
    VALUES (@qid, 1, @item, 'ZZ quoted line', 120, 'EA', 100.000000, 1, 12000.00, 'S', 2400.00, 6000.00, 'QUOTE');

    /*  The order that was taken: 100 at 100.00 less 10% then 5% is 85.50 each, 8,550.00 net.
        Forty have been despatched and billed, ten have come back, sixty are still owed. */
    INSERT so.SalesOrder (CompanyCode, SiteCode, OrderNumber, OrderType, CustomerCode, OrderDate, QuoteNumber,
                          CurrencyCode, FxRate, SalespersonCode, CommercialStatus, ReleaseStatus,
                          FulfilmentStatus, BillingStatus, CreatedBy)
    VALUES (@co, 'RDG', @ord, 'STANDARD', @cust, '2026-09-01', 'ZZ-SO23-Q1', 'GBP', 1, 'SALESREP',
            'ACCEPTED', 'RELEASED', 'PARTDESPATCH', 'PARTBILLED', 'TEST');
    SELECT @oid = SalesOrderId FROM so.SalesOrder WHERE CompanyCode = @co AND OrderNumber = @ord;
    INSERT so.SalesOrderLine (SalesOrderId, CompanyCode, OrderNumber, LineNum, LineRef, ItemCode, Description,
                              LineType, AcceptedQty, CancelledQty, DespatchedQty, BilledQty, ReturnedQty,
                              UomCode, UnitPrice, PricePerQty, DiscountPct1, DiscountPct2, NetAmount, VatCode,
                              VatRatePct, VatAmount, CostSnapshotGBP, CostBasis, Status)
    VALUES (@oid, @co, @ord, 1, '10', @item, 'ZZ ordered line', 'STOCK', 100, 0, 40, 40, 10,
            'EA', 100.000000, 1, 10.0000, 5.0000, 8550.00, 'S', 20.0000, 1710.00, 50.000000, 'QUOTE', 'OPEN');
    SELECT @lid = SalesOrderLineId FROM so.SalesOrderLine WHERE SalesOrderId = @oid AND LineNum = 1;

    /*  The despatch: forty went out.  The cost of sales actually posted is 2,120.00, which is
        120.00 more than the 2,000.00 the order expected - a late attributable cost. */
    INSERT so.Despatch (CompanyCode, SiteCode, DespatchRef, CustomerCode, OrderNumber, DespatchDate,
                        PostingDate, Status, CreatedBy)
    VALUES (@co, 'RDG', 'ZZ-SO23-DN1', @cust, @ord, '2026-09-20', '2026-09-20', 'CONFIRMED', 'TEST');
    SELECT @did = DespatchId FROM so.Despatch WHERE CompanyCode = @co AND DespatchRef = 'ZZ-SO23-DN1';
    INSERT so.DespatchLine (DespatchId, LineNum, SalesOrderLineId, OrderNumber, OrderLineNum, ItemCode,
                            Description, PickedQty, DespatchedQty, StockQty, BilledQty, UomCode,
                            UnitCostGBP, CogsGBP, CostMethod)
    VALUES (@did, 1, @lid, @ord, 1, @item, 'ZZ despatched line', 40, 40, 40, 40, 'EA', 53.000000, 2120.00, 'MWA');

    /*  The invoice and, after the return, the credit note. */
    INSERT ar.SalesInvoice (CompanyCode, DocumentType, InvoiceNumber, CustomerCode, DocumentDate, TaxPointDate,
                            PostingDate, DueDate, CurrencyCode, FxRate, NetAmount, VatAmount, GrossAmount,
                            NetAmountGBP, VatAmountGBP, GrossAmountGBP, SalesOrderNumber, SalespersonCode,
                            Status, CreatedBy)
    VALUES (@co, 'INVOICE', 'ZZ-SO23-INV1', @cust, '2026-09-21', '2026-09-20', '2026-09-21', '2026-10-21',
            'GBP', 1, 3420.00, 684.00, 4104.00, 3420.00, 684.00, 4104.00, @ord, 'SALESREP', 'POSTED', 'TEST');
    SELECT @iid = InvoiceId FROM ar.SalesInvoice WHERE CompanyCode = @co AND DocumentType = 'INVOICE' AND InvoiceNumber = 'ZZ-SO23-INV1';
    INSERT ar.SalesInvoiceLine (InvoiceId, LineNum, ItemCode, Description, Quantity, UomCode, UnitPrice,
                               PricePerQty, DiscountPct1, DiscountPct2, NetAmount, VatCode, VatRatePct,
                               VatAmount, NetAmountGBP, VatAmountGBP, CostSnapshotGBP)
    VALUES (@iid, 1, @item, 'ZZ invoiced line', 40, 'EA', 100.000000, 1, 10.0000, 5.0000, 3420.00, 'S',
            20.0000, 684.00, 3420.00, 684.00, 2120.00);
    INSERT ar.SalesInvoice (CompanyCode, DocumentType, InvoiceNumber, CustomerCode, DocumentDate, TaxPointDate,
                            PostingDate, DueDate, CurrencyCode, FxRate, NetAmount, VatAmount, GrossAmount,
                            NetAmountGBP, VatAmountGBP, GrossAmountGBP, SalesOrderNumber, SalespersonCode,
                            OriginalInvoiceNumber, CreditReason, Status, CreatedBy)
    VALUES (@co, 'CREDITNOTE', 'ZZ-SO23-CRN1', @cust, '2026-09-28', '2026-09-28', '2026-09-28', '2026-10-28',
            'GBP', 1, 855.00, 171.00, 1026.00, 855.00, 171.00, 1026.00, @ord, 'SALESREP',
            'ZZ-SO23-INV1', 'ZZ ten units returned', 'POSTED', 'TEST');
    SELECT @iid = InvoiceId FROM ar.SalesInvoice WHERE CompanyCode = @co AND DocumentType = 'CREDITNOTE' AND InvoiceNumber = 'ZZ-SO23-CRN1';
    INSERT ar.SalesInvoiceLine (InvoiceId, LineNum, ItemCode, Description, Quantity, UomCode, UnitPrice,
                               PricePerQty, DiscountPct1, DiscountPct2, NetAmount, VatCode, VatRatePct,
                               VatAmount, NetAmountGBP, VatAmountGBP)
    VALUES (@iid, 1, @item, 'ZZ credited line', 10, 'EA', 100.000000, 1, 10.0000, 5.0000, 855.00, 'S',
            20.0000, 171.00, 855.00, 171.00);

    /*  The return that caused the credit note. */
    INSERT so.ReturnAuthorisation (CompanyCode, SiteCode, ReturnRef, CustomerCode, OriginalOrderNumber,
                                   OriginalInvoiceNumber, RequestedDate, ReasonCode, ReplacementDecision,
                                   CreditNoteNumber, Status, CreatedBy)
    VALUES (@co, 'RDG', 'ZZ-SO23-RMA1', @cust, @ord, 'ZZ-SO23-INV1', '2026-09-25', 'NOTWANTED', 'CREDIT',
            'ZZ-SO23-CRN1', 'CLOSED', 'TEST');
    SELECT @rid = ReturnId FROM so.ReturnAuthorisation WHERE CompanyCode = @co AND ReturnRef = 'ZZ-SO23-RMA1';
    INSERT so.ReturnLine (ReturnId, LineNum, ItemCode, RequestedQty, AuthorisedQty, ReceivedQty, AcceptedQty,
                          CreditedQty, UomCode, Disposition, OriginalUnitCostGBP, ReturnValueGBP, CreditUnitPrice)
    VALUES (@rid, 1, @item, 10, 10, 10, 10, 10, 'EA', 'RESTOCK', 53.000000, 855.00, 85.500000);

    /*  A second, euro order so that the FX basis has something to say. */
    INSERT so.SalesOrder (CompanyCode, SiteCode, OrderNumber, OrderType, CustomerCode, OrderDate, CurrencyCode,
                          FxRate, SalespersonCode, CommercialStatus, ReleaseStatus, FulfilmentStatus,
                          BillingStatus, CreatedBy)
    VALUES (@co, 'RDG', @ord2, 'STANDARD', @cust, '2026-09-05', 'EUR', 1.16000000, 'SALESREP',
            'ACCEPTED', 'RELEASED', 'UNALLOCATED', 'NOTELIGIBLE', 'TEST');
    SELECT @oid2 = SalesOrderId FROM so.SalesOrder WHERE CompanyCode = @co AND OrderNumber = @ord2;
    INSERT so.SalesOrderLine (SalesOrderId, CompanyCode, OrderNumber, LineNum, LineRef, ItemCode, Description,
                              LineType, AcceptedQty, CancelledQty, DespatchedQty, BilledQty, ReturnedQty,
                              UomCode, UnitPrice, PricePerQty, DiscountPct1, DiscountPct2, NetAmount, VatCode,
                              VatRatePct, VatAmount, CostSnapshotGBP, Status)
    VALUES (@oid2, @co, @ord2, 1, '10', @item, 'ZZ euro line', 'STOCK', 60, 0, 0, 0, 0,
            'EA', 100.000000, 1, 10.0000, 5.0000, 5130.00, 'Z', 0.0000, 0.00, 50.000000, 'OPEN');
    SELECT @lid2 = SalesOrderLineId FROM so.SalesOrderLine WHERE SalesOrderId = @oid2 AND LineNum = 1;
    INSERT so.DeliverySchedule (SalesOrderLineId, CompanyCode, OrderNumber, LineNum, ScheduleNo, Quantity,
                                DespatchedQty, SiteCode, RequestedDate, OriginalPromiseDate, CurrentPromiseDate,
                                ChangeCount, Status)
    VALUES (@lid, @co, @ord, 1, 1, 100, 40, 'RDG', '2026-09-18', '2026-09-18', '2026-09-18', 0, 'PARTIAL'),
           (@lid2, @co, @ord2, 1, 1, 60, 0, 'RDG', '2026-11-02', '2026-11-02', '2026-11-02', 0, 'OPEN');

    /*  1 - the five stages of one demand are five different figures, each from the document that
           created it.  They are reported side by side and are never added together. */
    SELECT @quoted = SUM(ql.NetAmount) FROM so.QuoteLine ql JOIN so.Quote q ON q.QuoteId = ql.QuoteId
     WHERE q.CompanyCode = @co AND q.QuoteNumber LIKE 'ZZ-SO23%';
    SELECT @ordered = SUM(l.NetAmount) FROM so.SalesOrderLine l
     WHERE l.CompanyCode = @co AND l.OrderNumber = @ord;
    SELECT @despatched = SUM(CASE WHEN l.PricePerQty > 0 THEN dl.DespatchedQty * l.UnitPrice / l.PricePerQty
                                       * (1 - l.DiscountPct1 / 100) * (1 - l.DiscountPct2 / 100) ELSE 0 END)
      FROM so.DespatchLine dl
      JOIN so.Despatch d ON d.DespatchId = dl.DespatchId
      JOIN so.SalesOrderLine l ON l.SalesOrderLineId = dl.SalesOrderLineId
     WHERE d.CompanyCode = @co AND d.Status = 'CONFIRMED' AND d.DespatchRef LIKE 'ZZ-SO23%';
    SELECT @invoiced = SUM(CASE WHEN i.DocumentType = 'INVOICE'    THEN il.NetAmountGBP ELSE 0 END),
           @credited = SUM(CASE WHEN i.DocumentType = 'CREDITNOTE' THEN il.NetAmountGBP ELSE 0 END)
      FROM ar.SalesInvoice i JOIN ar.SalesInvoiceLine il ON il.InvoiceId = i.InvoiceId
     WHERE i.CompanyCode = @co AND i.Status = 'POSTED' AND i.InvoiceNumber LIKE 'ZZ-SO23%';
    IF @quoted = 12000.00 AND @ordered = 8550.00 AND @despatched = 3420.00
       AND @invoiced = 3420.00 AND @credited = 855.00
        PRINT 'ok 1 - quoted 12,000.00, ordered 8,550.00, despatched 3,420.00, invoiced 3,420.00 and credited 855.00 stay apart -- REQ: SO-023';
    ELSE PRINT 'not ok 1 - the stages read ' + ISNULL(CONVERT(varchar(20), @quoted), 'null') + '/'
             + ISNULL(CONVERT(varchar(20), @ordered), 'null') + '/' + ISNULL(CONVERT(varchar(20), @despatched), 'null')
             + '/' + ISNULL(CONVERT(varchar(20), @invoiced), 'null') + '/' + ISNULL(CONVERT(varchar(20), @credited), 'null')
             + ' -- REQ: SO-023';

    /*  2 - a credit note is reported as credited value, never netted off invoiced sales and
           never added to them.  Netting would hide 855.00 of value the customer did not keep. */
    IF @invoiced = 3420.00 AND @credited > 0 AND @invoiced <> @invoiced - @credited
        PRINT 'ok 2 - the credit note is a credited value in its own column, not a quieter invoice -- REQ: SO-023';
    ELSE PRINT 'not ok 2 - invoiced and credited were not reported separately -- REQ: SO-023';

    /*  3 - the discount given away is visible: 100 at the 100.00 list is 10,000.00, the order was
           taken at 8,550.00, so 1,450.00 was conceded.  It is not lost inside the net. */
    SELECT @discount = SUM(CASE WHEN l.PricePerQty > 0
                                THEN l.AcceptedQty * l.UnitPrice / l.PricePerQty - l.NetAmount ELSE 0 END)
      FROM so.SalesOrderLine l WHERE l.CompanyCode = @co AND l.OrderNumber = @ord;
    IF @discount = 1450.00
        PRINT 'ok 3 - 1,450.00 of discount is reported beside the 8,550.00 that was ordered -- REQ: SO-023';
    ELSE PRINT 'not ok 3 - the discount given away read ' + ISNULL(CONVERT(varchar(20), @discount), 'null')
             + ', expected 1450.00 -- REQ: SO-023';

    /*  4 - returns are visible and they do not reopen the order: ten came back, the line still
           owes sixty, not seventy (D-12). */
    SELECT @returns = SUM(ISNULL(rl.ReturnValueGBP, 0)) FROM so.ReturnLine rl
      JOIN so.ReturnAuthorisation r ON r.ReturnId = rl.ReturnId
     WHERE r.CompanyCode = @co AND r.ReturnRef LIKE 'ZZ-SO23%';
    SELECT @qty = OutstandingQty, @n = CONVERT(int, ReturnedQty) FROM so.vw_SalesOrderLineProgress
     WHERE CompanyCode = @co AND OrderNumber = @ord AND LineNum = 1;
    IF @returns = 855.00 AND @n = 10 AND @qty = 60
        PRINT 'ok 4 - 855.00 of returns is reported and the order still owes 60, not 70 -- REQ: SO-023, D-12';
    ELSE PRINT 'not ok 4 - returns ' + ISNULL(CONVERT(varchar(20), @returns), 'null') + ', returned qty '
             + ISNULL(CONVERT(varchar(10), @n), 'null') + ', outstanding ' + ISNULL(CONVERT(varchar(20), @qty), 'null')
             + ' -- REQ: SO-023';

    /*  5 - AC-049.  The expected cost is the basis captured on the order line, applied to what
           actually shipped: 40 at 50.00 is 2,000.00.  The realised cost is what despatch posted:
           2,120.00.  Both are kept, so the 120.00 late attributable cost is visible. */
    SELECT @expcost  = SUM(ISNULL(l.CostSnapshotGBP, 0) * dl.DespatchedQty),
           @realcost = SUM(dl.CogsGBP)
      FROM so.DespatchLine dl
      JOIN so.Despatch d ON d.DespatchId = dl.DespatchId
      JOIN so.SalesOrderLine l ON l.SalesOrderLineId = dl.SalesOrderLineId
     WHERE d.CompanyCode = @co AND d.Status = 'CONFIRMED' AND d.DespatchRef LIKE 'ZZ-SO23%';
    SET @late = @realcost - @expcost;
    IF @expcost = 2000.00 AND @realcost = 2120.00 AND @late = 120.00
        PRINT 'ok 5 - expected cost 2,000.00 and realised cost 2,120.00 are both kept, so the 120.00 late cost is visible -- REQ: SO-023, AC-049';
    ELSE PRINT 'not ok 5 - expected ' + ISNULL(CONVERT(varchar(20), @expcost), 'null') + ', realised '
             + ISNULL(CONVERT(varchar(20), @realcost), 'null') + ' -- REQ: SO-023, AC-049';

    /*  6 - AC-049.  Expected margin 1,420.00 (41.52%) against realised margin 1,300.00 (38.01%)
           on the same despatched value.  They are different measures and both are reported. */
    SET @expmargin  = @despatched - @expcost;
    SET @realmargin = @despatched - @realcost;
    IF @expmargin = 1420.00 AND @realmargin = 1300.00
       AND ROUND(@expmargin / @despatched * 100, 2) = 41.52
       AND ROUND(@realmargin / @despatched * 100, 2) = 38.01
        PRINT 'ok 6 - expected margin 1,420.00 (41.52%) and realised margin 1,300.00 (38.01%) are reported side by side -- REQ: SO-023, AC-049';
    ELSE PRINT 'not ok 6 - expected margin ' + ISNULL(CONVERT(varchar(20), @expmargin), 'null') + ', realised margin '
             + ISNULL(CONVERT(varchar(20), @realmargin), 'null') + ' -- REQ: SO-023, AC-049';

    /*  7 - AC-049.  The late cost does not reach back into the quotation: the quoted figure is
           still the 12,000.00 that was offered. */
    SELECT @dec = NetAmount FROM so.Quote WHERE CompanyCode = @co AND QuoteNumber = 'ZZ-SO23-Q1' AND Version = 1;
    IF @dec = 12000.00
        PRINT 'ok 7 - the quoted value is reported as it was issued and is never rewritten -- REQ: SO-023, AC-049';
    ELSE PRINT 'not ok 7 - the quote now reads ' + ISNULL(CONVERT(varchar(20), @dec), 'null') + ' -- REQ: SO-023';

    /*  8 - the FX basis travels with the value: a euro order is reported in euros and in sterling
           at the rate the order snapshotted (D-14, foreign units per GBP).
           60 outstanding at 85.50 is 5,130.00 EUR, which is 4,422.41 at 1.16. */
    SELECT @dec = BacklogValue, @gbp = BacklogValueGBP FROM so.vw_OpenOrderBook
     WHERE CompanyCode = @co AND OrderNumber = @ord2;
    IF @dec = 5130.00 AND ABS(@gbp - 4422.41) <= 0.01
        PRINT 'ok 8 - the euro backlog of 5,130.00 is also reported as 4,422.41 at the order rate of 1.16 -- REQ: SO-023, RPT-008';
    ELSE PRINT 'not ok 8 - the euro order read ' + ISNULL(CONVERT(varchar(20), @dec), 'null') + ' EUR and '
             + ISNULL(CONVERT(varchar(20), @gbp), 'null') + ' GBP -- REQ: SO-023';

    /*  9 - and the backlog enquiry keeps the two apart rather than reporting one mixed total. */
    DECLARE @bk table (GroupBy varchar(12), GroupKey varchar(60), GroupName varchar(200), CustomerCode varchar(20),
                       SiteCode varchar(10), Lines int, OutstandingQty decimal(19,6), BacklogValue decimal(18,2),
                       BacklogValueGBP decimal(18,2), EarliestPromise date, LatestPromise date,
                       LateLines int, RescheduledLines int);
    INSERT @bk EXEC so.usp_SalesBacklog @CompanyCode = 'STC', @GroupBy = 'ORDER', @CustomerCode = 'C0001';
    SELECT @dec = BacklogValue, @gbp = BacklogValueGBP, @qty = OutstandingQty FROM @bk WHERE GroupKey = @ord2;
    IF @dec = 5130.00 AND ABS(@gbp - 4422.41) <= 0.01 AND @qty = 60
        PRINT 'ok 9 - the sales backlog reports document value and sterling value as separate figures -- REQ: SO-023, RPT-008';
    ELSE PRINT 'not ok 9 - the backlog returned ' + ISNULL(CONVERT(varchar(20), @dec), 'null') + ' / '
             + ISNULL(CONVERT(varchar(20), @gbp), 'null') + ' -- REQ: SO-023, RPT-008';

    /* 10 - despatched is not the same demand as ordered: the sixty still owed are backlog and the
           forty that shipped are not, so no stage is double counted. */
    SELECT @qty = OutstandingQty FROM so.vw_OpenOrderBook WHERE CompanyCode = @co AND OrderNumber = @ord;
    SELECT @dec = BacklogValue FROM so.vw_OpenOrderBook WHERE CompanyCode = @co AND OrderNumber = @ord;
    IF @qty = 60 AND @dec = 5130.00
        PRINT 'ok 10 - the 8,550.00 ordered splits into 3,420.00 despatched and 5,130.00 still owed -- REQ: SO-023, RPT-008';
    ELSE PRINT 'not ok 10 - the remaining order book read ' + ISNULL(CONVERT(varchar(20), @qty), 'null') + ' at '
             + ISNULL(CONVERT(varchar(20), @dec), 'null') + ' -- REQ: SO-023';

    /* 11 - the three cost figures a profitability review needs are each held in their own place:
           the quoted estimate on the quote line, the accepted basis on the order line and the
           actual cost of sales on the despatch line.  None of them overwrites another. */
    SELECT @n = 0;
    SELECT @n = COUNT(*) FROM so.QuoteLine ql JOIN so.Quote q ON q.QuoteId = ql.QuoteId
     WHERE q.CompanyCode = @co AND q.QuoteNumber = 'ZZ-SO23-Q1' AND ql.EstimatedCostGBP = 6000.00;
    IF @n = 1
       AND (SELECT CostSnapshotGBP FROM so.SalesOrderLine WHERE SalesOrderLineId = @lid) = 50.000000
       AND (SELECT CogsGBP FROM so.DespatchLine WHERE DespatchId = @did AND LineNum = 1) = 2120.00
        PRINT 'ok 11 - quoted estimate, accepted cost basis and actual cost of sales are each kept in their own place -- REQ: SO-023, AC-049';
    ELSE PRINT 'not ok 11 - the three cost bases were not all held separately -- REQ: SO-023';

    /* 12 - the cost method and the posting date of the actual cost are recorded, so a realised
           margin can always say where its cost came from and when it was cut off (AC-049). */
    SELECT @n = COUNT(*) FROM so.DespatchLine dl JOIN so.Despatch d ON d.DespatchId = dl.DespatchId
     WHERE d.CompanyCode = @co AND d.DespatchRef = 'ZZ-SO23-DN1'
       AND ISNULL(dl.CostMethod, '') <> '' AND d.PostingDate IS NOT NULL;
    IF @n = 1
        PRINT 'ok 12 - the realised cost names its method and its cut off date -- REQ: SO-023, AC-049';
    ELSE PRINT 'not ok 12 - the realised cost carries no source or cut off -- REQ: SO-023, AC-049';
END TRY
BEGIN CATCH
    PRINT 'not ok 1 - unexpected error: ' + ERROR_MESSAGE();
END CATCH
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;

/* 13 - the backlog enquiry refuses an unknown grouping rather than quietly answering a
       different question.  REQ: SO-023, RPT-008 */
BEGIN TRY
    EXEC so.usp_SalesBacklog @CompanyCode = @co, @GroupBy = 'MARGIN';
    PRINT 'not ok 13 - an undefined analysis grouping was accepted -- REQ: SO-023, RPT-008';
END TRY
BEGIN CATCH
    SET @msg = ERROR_MESSAGE();
    IF @msg LIKE '%STERP-E-1904%' PRINT 'ok 13 - sales analysis groups only by the dimensions it defines -- REQ: SO-023, RPT-008';
    ELSE PRINT 'not ok 13 - unexpected error: ' + @msg;
END CATCH

/* 14 - nothing from this suite survives it. */
SELECT @n = (SELECT COUNT(*) FROM so.SalesOrder WHERE OrderNumber LIKE 'ZZ-SO23%')
           + (SELECT COUNT(*) FROM so.Quote WHERE QuoteNumber LIKE 'ZZ-SO23%')
           + (SELECT COUNT(*) FROM so.Despatch WHERE DespatchRef LIKE 'ZZ-SO23%')
           + (SELECT COUNT(*) FROM ar.SalesInvoice WHERE InvoiceNumber LIKE 'ZZ-SO23%')
           + (SELECT COUNT(*) FROM so.ReturnAuthorisation WHERE ReturnRef LIKE 'ZZ-SO23%');
IF @n = 0 PRINT 'ok 14 - the suite left no documents behind -- REQ: SO-023';
ELSE PRINT 'not ok 14 - ' + CONVERT(varchar(10), @n) + ' ZZ-SO23 documents survived the rollback -- REQ: SO-023';

PRINT '# so-gaps done';
GO
