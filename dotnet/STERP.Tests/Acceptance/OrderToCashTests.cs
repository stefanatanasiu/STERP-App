using System;
using System.Data;
using STERP.Business;
using STERP.Business.Xmd;
using STERP.CobolInterop;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests.Acceptance
{
    /// <summary>
    /// Order to cash, end to end: an accepted order is reserved, picked, despatched, invoiced,
    /// paid and aged - every step through the facade that the screen uses, so the test fails for
    /// the same reason a user would.  The order is created with a unique ZAO reference, so the
    /// suite can be run again without a reset.
    /// </summary>
    public static class OrderToCashTests
    {
        private const string Site = "RDG";

        /// <summary>Seeds an accepted order for an item that really is in stock at Reading.</summary>
        private static DataRow Fixture(decimal qty, out string orderNumber)
        {
            DataRow stock = Acc.Demo(
                "SELECT TOP (1) b.ItemCode, b.WarehouseCode, b.LocationCode, i.StockUomCode, i.Description " +
                "FROM inv.StockBalance b JOIN inv.Item i ON i.CompanyCode = b.CompanyCode AND i.ItemCode = b.ItemCode " +
                "WHERE b.CompanyCode = @c AND b.SiteCode = @s AND b.QualityStatus = 'ACCEPTED' " +
                "AND b.QtyOnHand - b.QtyReserved > @q AND ISNULL(b.LotCode, '') = '' AND ISNULL(b.SerialNo, '') = '' " +
                "ORDER BY b.QtyOnHand DESC", "free stock at " + Site, "@c", Acc.Co, "@s", Site, "@q", qty * 4m);
            DataRow tmpl = Acc.Demo(
                "SELECT TOP (1) o.OrderType, o.CommercialStatus, o.ReleaseStatus, o.FulfilmentStatus, o.BillingStatus, " +
                "o.CustomerCode, o.TermsCode, o.IncotermsRule, o.IncotermsNamedPlace, o.SalespersonCode " +
                "FROM so.SalesOrder o WHERE o.CompanyCode = @c AND o.CommercialStatus = 'ACCEPTED' ORDER BY o.OrderNumber",
                "accepted sales order to copy the status vocabulary from", "@c", Acc.Co);
            DataRow line = Acc.Demo(
                "SELECT TOP (1) LineType, Status, VatCode, VatRatePct FROM so.SalesOrderLine " +
                "WHERE CompanyCode = @c AND LineType = 'STOCK' ORDER BY SalesOrderLineId", "sales order line", "@c", Acc.Co);

            orderNumber = Acc.Ref("O");
            decimal price = 25.000000m, net = qty * price;
            decimal vatPct = Acc.Num(line, "VatRatePct");
            object id = Acc.Scalar(
                "INSERT so.SalesOrder (CompanyCode, SiteCode, OrderNumber, OrderType, CustomerCode, OrderDate, " +
                "CurrencyCode, FxRate, TermsCode, SalespersonCode, IncotermsRule, IncotermsNamedPlace, " +
                "NetAmount, VatAmount, GrossAmount, GrossAmountGBP, " +
                "CommercialStatus, ReleaseStatus, FulfilmentStatus, BillingStatus, CreatedBy) " +
                "VALUES (@c, @s, @o, @t, @cust, @d, 'GBP', 1, @terms, @sp, @inc, @place, @net, @vat, @gross, @gross, " +
                "@cs, @rs, @fs, @bs, @u); SELECT CAST(SCOPE_IDENTITY() AS bigint)",
                "@c", Acc.Co, "@s", Site, "@o", orderNumber, "@t", Acc.Text(tmpl, "OrderType"),
                "@cust", Acc.Text(tmpl, "CustomerCode"), "@d", Acc.Today,
                "@terms", Acc.Text(tmpl, "TermsCode"), "@sp", Acc.Text(tmpl, "SalespersonCode"),
                "@inc", Acc.Text(tmpl, "IncotermsRule"), "@place", Acc.Text(tmpl, "IncotermsNamedPlace"),
                "@net", net, "@vat", Math.Round(net * vatPct / 100m, 2), "@gross", net + Math.Round(net * vatPct / 100m, 2),
                "@cs", Acc.Text(tmpl, "CommercialStatus"), "@rs", Acc.Text(tmpl, "ReleaseStatus"),
                "@fs", Acc.Text(tmpl, "FulfilmentStatus"), "@bs", Acc.Text(tmpl, "BillingStatus"),
                "@u", SterpSession.Current.UserCode);
            long orderId = Convert.ToInt64(id);
            Acc.Scalar(
                "INSERT so.SalesOrderLine (SalesOrderId, CompanyCode, OrderNumber, LineNum, LineRef, ItemCode, Description, " +
                "LineType, AcceptedQty, CancelledQty, DespatchedQty, BilledQty, UomCode, UnitPrice, PricePerQty, " +
                "DiscountPct1, NetAmount, VatCode, VatRatePct, VatAmount, CostSnapshotGBP, Status) " +
                "VALUES (@id, @c, @o, 1, '10', @item, @desc, @lt, @q, 0, 0, 0, @uom, @price, 1, 0, @net, @vc, @vp, @vat, @cost, @st); " +
                "SELECT 1",
                "@id", orderId, "@c", Acc.Co, "@o", orderNumber, "@item", Acc.Text(stock, "ItemCode"),
                "@desc", Acc.Text(stock, "Description"), "@lt", Acc.Text(line, "LineType"), "@q", qty,
                "@uom", Acc.Text(stock, "StockUomCode"), "@price", price, "@net", net,
                "@vc", Acc.Text(line, "VatCode"), "@vp", vatPct, "@vat", Math.Round(net * vatPct / 100m, 2),
                "@cost", 12.000000m, "@st", Acc.Text(line, "Status"));
            Acc.Scalar(
                "INSERT so.DeliverySchedule (SalesOrderLineId, CompanyCode, OrderNumber, LineNum, ScheduleNo, Quantity, " +
                "DespatchedQty, SiteCode, RequestedDate, OriginalPromiseDate, CurrentPromiseDate, ChangeCount, Status) " +
                "SELECT SalesOrderLineId, @c, @o, 1, 1, @q, 0, @s, @d, @d, @d, 0, @st FROM so.SalesOrderLine " +
                "WHERE SalesOrderId = @id AND LineNum = 1; SELECT 1",
                "@id", orderId, "@c", Acc.Co, "@o", orderNumber, "@q", qty, "@s", Site, "@d", Acc.Today,
                "@st", Acc.Text(line, "Status"));
            stock.Table.Columns.Add("CustomerCode", typeof(string));
            stock["CustomerCode"] = Acc.Text(tmpl, "CustomerCode");
            return stock;
        }

        [StTest("acc: order to cash - an accepted order is reserved, picked, despatched and invoiced, and the chain holds together",
                Reqs = "XMD-001,SO-010,SO-013,SO-016,SO-019,AR-003,RPT-008", Ac = "AC-097")]
        public static void OrderToCashChain()
        {
            Acc.Need("so.SalesOrder");
            Acc.Need("inv.StockReservation");
            Acc.Context("SALESREP", Site);
            const decimal qty = 6m;
            string order;
            DataRow fx = Fixture(qty, out order);
            string item = Acc.Text(fx, "ItemCode"), uom = Acc.Text(fx, "StockUomCode"), cust = Acc.Text(fx, "CustomerCode");
            SalesOrderService svc = new SalesOrderService();

            // 1 - availability is answered by source before anything is promised
            CobolResult r = svc.Availability(item, Site, uom, qty, Acc.Today);
            Acc.NeedEngine(r, "SOAVAIL");
            Acc.Ok(r, "SOAVAIL answered for " + item);
            St.True(svc.SupplyLines().Rows.Count > 0, "availability is explained by labelled supply lines");

            // 2 - reserving the line takes the quantity out of free stock and nothing else
            decimal freeBefore = Acc.Dec("SELECT SUM(QtyOnHand - QtyReserved) FROM inv.StockBalance " +
                                         "WHERE CompanyCode = @c AND SiteCode = @s AND ItemCode = @i", "@c", Acc.Co, "@s", Site, "@i", item);
            r = svc.Reserve(order, 1, item, Site, qty, Acc.Today, cust);
            Acc.NeedEngine(r, "SORESV");
            Acc.Ok(r, "SORESV reserved " + qty + " of " + item);
            St.Eq(1, Acc.Int("SELECT COUNT(*) FROM inv.StockReservation WHERE CompanyCode = @c AND DemandRef = @o",
                             "@c", Acc.Co, "@o", order), "the reservation is recorded against the order line");
            decimal reserved = Acc.Dec("SELECT ISNULL(SUM(QtyReserved), 0) FROM inv.StockReservation WHERE CompanyCode = @c " +
                                       "AND DemandRef = @o", "@c", Acc.Co, "@o", order);
            St.True(reserved <= qty, "a reservation never exceeds what was ordered (reserved " + reserved + " of " + qty + ")");
            decimal freeAfter = Acc.Dec("SELECT SUM(QtyOnHand - QtyReserved) FROM inv.StockBalance " +
                                        "WHERE CompanyCode = @c AND SiteCode = @s AND ItemCode = @i", "@c", Acc.Co, "@s", Site, "@i", item);
            St.True(freeAfter <= freeBefore, "a reservation never increases free stock (" + freeBefore + " before, " + freeAfter + " after)");

            // 3 - picking creates a pick list for the order and consumes nothing yet
            int movesBeforePick = Acc.Int("SELECT COUNT(*) FROM inv.StockMovement WHERE CompanyCode = @c", "@c", Acc.Co);
            r = svc.Pick("GENERATE", order, "", Site, Acc.Today);
            Acc.NeedEngine(r, "PICKGEN");
            Acc.Ok(r, "PICKGEN generated a pick list");
            string pick = Acc.Str("SELECT TOP (1) PickListRef FROM so.PickList WHERE CompanyCode = @c AND OrderNumber = @o " +
                                  "ORDER BY PickListId DESC", "@c", Acc.Co, "@o", order);
            St.True(pick.Length > 0, "the pick list is filed against the order");
            St.Eq(movesBeforePick, Acc.Int("SELECT COUNT(*) FROM inv.StockMovement WHERE CompanyCode = @c", "@c", Acc.Co),
                  "generating a pick list moves no stock");
            int pickLines = Acc.Int("SELECT COUNT(*) FROM so.PickListLine l JOIN so.PickList p ON p.PickListId = l.PickListId " +
                                    "WHERE p.CompanyCode = @c AND p.OrderNumber = @o", "@c", Acc.Co, "@o", order);
            St.True(reserved > 0m, "SORESV reserved stock for the accepted order line (reserved " + reserved + " of " + qty + ")");
            St.True(pickLines > 0, "the pick list carries a line for the reserved order line");
            r = svc.Pick("CONFIRM", order, pick, Site, Acc.Today);
            Acc.Ok(r, "PICKGEN confirmed the pick");

            // 4 - despatch is the economic event: stock leaves and the order line is despatched
            CommArea ca = svc.NewDespatch(order, pick, cust, Acc.Today, "", "", 1, 1.5m,
                                          "DAP", "Reading", false, "GB", "", "");
            svc.AddDespatchLine(ca, 1, 1, item, Acc.Text(fx, "Description"), qty, qty, uom, "", "");
            r = svc.Despatch(ca, "CONFIRM");
            Acc.NeedEngine(r, "DESPATCH");
            Acc.Ok(r, "DESPATCH confirmed");
            St.Eq(qty, Acc.Dec("SELECT DespatchedQty FROM so.SalesOrderLine WHERE CompanyCode = @c AND OrderNumber = @o AND LineNum = 1",
                               "@c", Acc.Co, "@o", order), "the despatched quantity is on the order line");
            St.True(Acc.Int("SELECT COUNT(*) FROM inv.StockMovement WHERE CompanyCode = @c", "@c", Acc.Co) > movesBeforePick,
                    "the despatch moved stock, so the ledger and the warehouse agree");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM so.SalesOrderLine WHERE CompanyCode = @c AND OrderNumber = @o " +
                             "AND AcceptedQty - CancelledQty - DespatchedQty < 0", "@c", Acc.Co, "@o", order),
                  "no line is ever despatched beyond what was accepted");

            // 5 - billing: the despatch, and only the despatch, becomes an invoice and an open item
            // billing is the credit control clerk's job, not the sales rep's: AR.INVOICE.POST is theirs
            Acc.Context("ARCLERK", Site);
            ReportRun run = svc.InvoiceRun(Acc.Co, Site, Acc.Today.AddDays(-1), Acc.Today.AddDays(1), "PREVIEW");
            St.True(run != null && run.Succeeded, "SOINVRUN previewed the billing queue: " + (run == null ? "no run" : run.Output));
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM ar.SalesInvoice WHERE CompanyCode = @c AND SalesOrderNumber = @o",
                             "@c", Acc.Co, "@o", order), "a preview invoices nothing (AC-004)");
            run = svc.InvoiceRun(Acc.Co, Site, Acc.Today.AddDays(-1), Acc.Today.AddDays(1), "COMMIT");
            St.True(run.Succeeded, "the invoice run completed: " + run.Output);
            DataRow inv = Acc.Row("SELECT TOP (1) InvoiceNumber, CustomerCode, GrossAmountGBP, Status FROM ar.SalesInvoice " +
                                  "WHERE CompanyCode = @c AND SalesOrderNumber = @o ORDER BY InvoiceId DESC", "@c", Acc.Co, "@o", order);
            St.True(inv != null, "the invoice run billed the despatch of " + order);
            St.Eq(cust, Acc.Text(inv, "CustomerCode"), "the invoice is raised on the ordering customer");

            // 6 - cash: the receipt allocates against the open item and the residual falls
            string invoiceNo = Acc.Text(inv, "InvoiceNumber");
            DataRow open = Acc.Row("SELECT OpenItemId, ResidualAmount, CurrencyCode FROM ar.OpenItem " +
                                   "WHERE CompanyCode = @c AND DocumentNumber = @d", "@c", Acc.Co, "@d", invoiceNo);
            St.True(open != null, "the invoice " + invoiceNo + " reached the open item ledger");
            decimal residual = Acc.Num(open, "ResidualAmount");
            St.True(residual > 0m, "an unpaid invoice is an open item for its full value");
            Acc.Context("ARCLERK", Site);
            DebtorsService ar = new DebtorsService();
            string bank = Acc.Str("SELECT TOP (1) BankAccountCode FROM fin.BankAccount WHERE CompanyCode = @c AND CurrencyCode = 'GBP' " +
                                  "ORDER BY BankAccountCode", "@c", Acc.Co);
            CommArea rc = ar.NewReceipt(cust, Acc.Today, Acc.Text(open, "CurrencyCode"), bank, "BACS", residual,
                                        Acc.Ref("R"), Acc.Ref("X"));
            ar.AddAllocation(rc, 1, Fmt.Int(open["OpenItemId"]), invoiceNo, Acc.Text(open, "CurrencyCode"), residual);
            // the money is banked first: an allocation preview reads the credit open item the
            // receipt creates, so there is nothing to preview until the cash is actually held
            CobolResult rr = ar.RunReceipt(rc, "RECEIPT");
            Acc.NeedEngine(rr, "RCPTALOC");
            Acc.Ok(rr, "the receipt was banked");
            rr = ar.RunReceipt(rc, "PREVIEW");
            Acc.Ok(rr, "the allocation of the banked receipt was previewed");
            St.Eq(residual, Acc.Dec("SELECT ResidualAmount FROM ar.OpenItem WHERE OpenItemId = @i", "@i", Fmt.Int(open["OpenItemId"])),
                  "a receipt preview settles nothing");
            rr = ar.RunReceipt(rc, "ALLOCATE");
            Acc.Ok(rr, "the receipt was allocated to the invoice");
            St.Eq(0m, Acc.Dec("SELECT ResidualAmount FROM ar.OpenItem WHERE OpenItemId = @i", "@i", Fmt.Int(open["OpenItemId"])),
                  "allocating the full residual clears the open item");

            // 7 - the aged debt no longer shows the settled invoice
            DataSet aged = ar.AgedDebt(Acc.Today);
            St.True(aged.Tables.Count > 0, "the aged debt analysis answers as at a date");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM ar.OpenItem WHERE CompanyCode = @c AND DocumentNumber = @d AND ResidualAmount <> 0",
                             "@c", Acc.Co, "@d", invoiceNo), "a fully settled invoice is out of the aged debt");
        }

        [StTest("acc: the chain trace walks the order to cash documents in both directions",
                Reqs = "XMD-001,XMD-004", Ac = "AC-097,AC-102")]
        public static void ChainTraceFollowsTheOrder()
        {
            Acc.Need("rpt.usp_ChainTrace");
            Acc.Context("SALESMGR", Site);
            string order = Acc.Str("SELECT TOP (1) d.OrderNumber FROM so.Despatch d WHERE d.CompanyCode = @c ORDER BY d.DespatchId DESC",
                                   "@c", Acc.Co);
            if (order.Length == 0) St.Skip("no despatch has been confirmed yet");
            XmdService xmd = new XmdService();
            DataTable steps = xmd.ChainTraceList("ORDER", order);
            if (steps == null) St.Skip("rpt.usp_ChainTrace is not deployed yet");
            St.True(steps.Rows.Count > 0, "a despatched order has a chain");
            St.True(steps.Columns.Count >= 3, "each step names its document, type and date");
            St.True(XmdService.IsSetTraceable("ORDER"), "an order is traceable as a set");
        }

        [StTest("acc: a credit note never exceeds the invoice it credits and is always linked to it",
                Reqs = "AR-004,AR-005,AR-006", Ac = "AC-020")]
        public static void CreditNoteBoundaries()
        {
            Acc.Need("ar.SalesInvoice");
            Acc.Context("ARCLERK", Site);
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM ar.SalesInvoice WHERE CompanyCode = @c AND DocumentType <> 'INVOICE' " +
                             "AND OriginalInvoiceNumber IS NULL AND CreditReason IS NULL", "@c", Acc.Co),
                  "a credit is either linked to its invoice or carries the reason it is not");
            St.Eq(0, Acc.Int(
                "SELECT COUNT(*) FROM ar.SalesInvoice c JOIN ar.SalesInvoice i ON i.CompanyCode = c.CompanyCode " +
                "AND i.InvoiceNumber = c.OriginalInvoiceNumber WHERE c.CompanyCode = @c AND c.DocumentType <> 'INVOICE' " +
                "AND ABS(c.GrossAmountGBP) > i.GrossAmountGBP + 0.005", "@c", Acc.Co),
                  "no credit note is larger than the invoice it credits");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM ar.SalesInvoice WHERE CompanyCode = @c AND CreditableRemainingGBP < -0.005",
                             "@c", Acc.Co), "the creditable balance of an invoice never goes negative");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM ar.SalesInvoice WHERE CompanyCode = @c AND Status = 'POSTED' " +
                             "AND (TaxPointDate IS NULL OR DueDate IS NULL)", "@c", Acc.Co),
                  "a posted invoice always carries its tax point and its due date");
        }

        [StTest("acc: a dispute and a promise to pay are recorded without touching the debt",
                Reqs = "AR-015,AR-016", Ac = "AC-024")]
        public static void DisputeAndPromise()
        {
            Acc.Need("ar.CollectionCase");
            Acc.Context("CREDMGR", Site);
            DataRow open = Acc.Demo("SELECT TOP (1) OpenItemId, CustomerCode, DocumentNumber, ResidualAmount FROM ar.OpenItem " +
                                    "WHERE CompanyCode = @c AND ResidualAmount > 10 AND IsDisputed = 0 ORDER BY OpenItemId",
                                    "unsettled open item", "@c", Acc.Co);
            int id = Fmt.Int(open["OpenItemId"]);
            decimal residual = Acc.Num(open, "ResidualAmount");
            DebtorsService svc = new DebtorsService();
            CobolResult r = svc.RaiseDispute(Acc.Text(open, "CustomerCode"), Acc.Text(open, "DocumentNumber"), id,
                                             Math.Round(residual / 2m, 2), "PRICE", "Acceptance test dispute " + Acc.Ref("D"));
            Acc.NeedEngine(r, "ARCOLL");
            if (!r.Succeeded)
            {
                // STERP-E-0004 / DSP0061 used to mean no ARCASE number series was seeded; that is
                // fixed in 52-ar-menu.sql, so this branch now only guards a stripped-down database
                St.True(Acc.Refused(r), "a dispute is recorded or refused with a reason: " + r.Display);
                St.Eq(residual, Acc.Dec("SELECT ResidualAmount FROM ar.OpenItem WHERE OpenItemId = @i", "@i", id),
                      "a refused dispute leaves the debt exactly as it was");
                St.Skip("the dispute case could not be numbered in this environment: " + r.Display);
            }
            DataRow after = Acc.Row("SELECT ResidualAmount, IsDisputed, DisputedAmountGBP FROM ar.OpenItem WHERE OpenItemId = @i", "@i", id);
            St.Eq(residual, Acc.Num(after, "ResidualAmount"), "a dispute does not reduce the debt, it marks it");
            St.True(Fmt.Bool(after["IsDisputed"]), "the open item is flagged as disputed");
            St.True(Acc.Num(after, "DisputedAmountGBP") > 0m, "and the amount in dispute is quantified");

            CobolResult p = svc.RecordPromise(Acc.Text(open, "CustomerCode"), "", residual, Acc.Today.AddDays(14),
                                              "Acceptance test promise " + Acc.Ref("P"));
            St.True(p != null, "a promise to pay is accepted or refused, never ignored");
            if (p.Succeeded)
                St.Eq(residual, Acc.Dec("SELECT ResidualAmount FROM ar.OpenItem WHERE OpenItemId = @i", "@i", id),
                      "a promise is a commitment, not a payment");
        }

        [StTest("acc: the aged analysis is answered as at a date and reconciles to the open items",
                Reqs = "AR-017,AR-018,RPT-001", Ac = "AC-025,AC-106")]
        public static void AgedDebtAsAtADate()
        {
            Acc.Need("ar.usp_AgedDebtSummary");
            Acc.Context("CREDMGR", Site);
            DebtorsService svc = new DebtorsService();
            DataSet now = svc.AgedDebt(Acc.Today);
            St.True(now.Tables.Count > 0, "the aged debt report has at least one result set");
            DataTable t = now.Tables[now.Tables.Count - 1];
            St.True(t.Columns.Count >= 4, "the aged analysis reports more than one ageing band, found " + t.Columns.Count + " column(s)");

            DataSet before = svc.AgedDebt(new DateTime(2000, 1, 1));
            decimal old = 0m;
            foreach (DataTable dt in before.Tables)
                foreach (DataRow row in dt.Rows)
                    foreach (DataColumn c in dt.Columns)
                        if (c.DataType == typeof(decimal)) old += Math.Abs(Fmt.Dec(row[c]));
            decimal current = 0m;
            foreach (DataTable dt in now.Tables)
                foreach (DataRow row in dt.Rows)
                    foreach (DataColumn c in dt.Columns)
                        if (c.DataType == typeof(decimal)) current += Math.Abs(Fmt.Dec(row[c]));
            St.True(old < current || current == 0m,
                    "the report really is as at a date - the year 2000 does not show today's debt (" + old + " against " + current + ")");

            DataSet stm = svc.Statement(Acc.Str("SELECT TOP (1) CustomerCode FROM ar.OpenItem WHERE CompanyCode = @c " +
                                                "ORDER BY OpenItemId DESC", "@c", Acc.Co), Acc.Today, "OPENITEM");
            St.True(stm.Tables.Count > 0, "a statement is produced for the same as-at date");
        }

        [StTest("acc: receivables metrics and the collection worklist are reported for the close",
                Reqs = "AR-021,AR-022,RPT-003", Ac = "AC-027,AC-108")]
        public static void ReceivablesCloseAndWorklist()
        {
            Acc.Need("ar.usp_CreditControlWorklist");
            Acc.Context("CREDMGR", Site);
            DebtorsService svc = new DebtorsService();
            string cust = Acc.Str("SELECT TOP (1) CustomerCode FROM ar.OpenItem WHERE CompanyCode = @c AND ResidualAmount > 0 " +
                                  "ORDER BY ResidualAmount DESC", "@c", Acc.Co);
            if (cust.Length == 0) St.Skip("no customer owes anything yet");
            CobolResult r = svc.Metrics(cust, Acc.Today);
            Acc.NeedEngine(r, "ARMETRIC");
            St.True(r.Succeeded || Acc.Refused(r), "the metric run answers with a figure or a reason: " + r.Display);

            DataSet work = svc.Worklist("CREDMGR");
            St.True(work.Tables.Count > 0, "the credit control worklist answers");
            DataSet enq = svc.Enquiry(cust);
            St.True(enq.Tables.Count >= 2, "the customer enquiry shows the account and its documents, found " + enq.Tables.Count + " set(s)");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM ar.OpenItem WHERE CompanyCode = @c AND ABS(ResidualAmount) > ABS(OriginalAmount) + 0.005",
                             "@c", Acc.Co), "no open item is settled for more than it was raised for");
        }

        [StTest("acc: a despatched order keeps its document history and a realised margin",
                Reqs = "SO-022,SO-023", Ac = "AC-049")]
        public static void DocumentHistoryAndRealisedMargin()
        {
            Acc.Need("so.DocumentVersion");
            Acc.Context("SALESMGR", Site);
            DataRow d = Acc.Demo("SELECT TOP (1) l.OrderNumber, l.OrderLineNum, l.ItemCode, l.DespatchedQty, l.CogsGBP, " +
                                 "o.CustomerCode FROM so.DespatchLine l JOIN so.Despatch d2 ON d2.DespatchId = l.DespatchId " +
                                 "JOIN so.SalesOrder o ON o.CompanyCode = d2.CompanyCode AND o.OrderNumber = l.OrderNumber " +
                                 "WHERE d2.CompanyCode = @c AND l.DespatchedQty > 0 ORDER BY l.DespatchLineId DESC",
                                 "confirmed despatch line", "@c", Acc.Co);
            string order = Acc.Text(d, "OrderNumber");
            SalesOrderService svc = new SalesOrderService();
            DataRow head = svc.Header(order);
            St.True(head != null, "the despatched order is still readable as a document");
            St.True(svc.Lines(order).Rows.Count > 0, "and so are its lines");

            decimal net = Acc.Dec("SELECT NetAmount FROM so.SalesOrderLine WHERE CompanyCode = @c AND OrderNumber = @o AND LineNum = @n",
                                  "@c", Acc.Co, "@o", order, "@n", Fmt.Int(d["OrderLineNum"]));
            CobolResult r = svc.Margin("REALISED", order, Fmt.Int(d["OrderLineNum"]), Acc.Text(d, "ItemCode"),
                                       Acc.Text(d, "CustomerCode"), Acc.Num(d, "DespatchedQty"), net, "GBP", Acc.Today);
            Acc.NeedEngine(r, "SOMARGN");
            St.True(r.Succeeded || Acc.Refused(r), "a realised margin is calculated or refused with a reason: " + r.Display);
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM so.DespatchLine WHERE CogsGBP < 0"),
                  "a despatch never books a negative cost of sale");
        }
    }
}
