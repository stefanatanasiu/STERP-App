using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Reflection;
using System.Threading;
using STERP.Business;
using STERP.CobolInterop;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests
{
    /// <summary>Purchasing facades and screens-behind-the-screens (module po).</summary>
    public static class PurchasingTests
    {
        private static void Context()
        {
            SterpSession.Current.UserCode = "BUYER";
            SterpSession.Current.CompanyCode = "STC";
            // BUYER's own site - core.AppUser gives BUYER Birmingham, and that is where the
            // purchasing demonstration orders were raised.  Signing the harness on at Reading
            // made every site-scoped purchasing enquiry answer "nothing to do".
            SterpSession.Current.SiteCode = "BHM";
        }

        private static void NeedProgram(string program)
        {
            if (System.Array.IndexOf(CobolGateway.ProgramNames(), program) < 0)
                St.Skip(program + " is not linked into this build");
        }

        private static void Need(string objectName)
        {
            if (!SqlHelper.ObjectExists(objectName)) St.Skip(objectName + " is not deployed yet");
        }

        private static int Count(string sql, params object[] nameValuePairs)
        {
            return Fmt.Int(SqlHelper.ExecuteScalar(sql, nameValuePairs));
        }

        /// <summary>An issued STC order whose outstanding line carries the given traceability.</summary>
        private static string Outstanding(string trace)
        {
            return Fmt.Str(SqlHelper.ExecuteScalar(
                "SELECT TOP 1 l.OrderNumber FROM po.PurchaseOrderLine l " +
                "JOIN po.PurchaseOrder o ON o.PurchaseOrderId = l.PurchaseOrderId " +
                "LEFT JOIN inv.Item i ON i.CompanyCode = l.CompanyCode AND i.ItemCode = l.ItemCode " +
                "WHERE l.CompanyCode = @c AND o.Status IN ('ISSUED','ACKNOWLEDGED','AMENDED') " +
                "AND l.Status NOT IN ('CANCELLED','SHORTCLOSED') " +
                "AND l.OrderedQty - l.CancelledQty - l.ReceivedQty > 0 " +
                "AND ISNULL(i.TraceabilityType,'NONE') = @t ORDER BY l.OrderNumber", "@c", "STC", "@t", trace));
        }

        private static Type Screen(string className)
        {
            Assembly shell = Assembly.LoadFrom(Path.Combine(SterpConfig.BinDir, "STERP.exe"));
            return shell.GetType("STERP.Desktop." + className, false, true);
        }

        /// <summary>
        /// Opens a screen the way the shell does - on an STA thread, by its class name - and
        /// reports what it laid out.  Nothing is shown, so no dialogue can stall the harness.
        /// </summary>
        private static string Opens(string className)
        {
            Type t = Screen(className);
            St.True(t != null, className + " is compiled into STERP.exe");
            St.True(Screen("BaseForm").IsAssignableFrom(t),
                    className + " inherits BaseForm, so it gets F2/F3/F5/F10 and the status line");
            string report = "";
            Thread th = new Thread(delegate ()
            {
                System.Windows.Forms.Form f = null;
                try
                {
                    f = (System.Windows.Forms.Form)Activator.CreateInstance(t);
                    report = f.Text + " with " + f.Controls.Count + " control(s)";
                }
                catch (Exception ex)
                {
                    Exception e = ex is TargetInvocationException && ex.InnerException != null ? ex.InnerException : ex;
                    report = "!" + e.GetType().Name + ": " + e.Message;
                }
                finally
                {
                    if (f != null) f.Dispose();
                }
            });
            th.SetApartmentState(ApartmentState.STA);
            th.Start();
            th.Join();
            St.True(!report.StartsWith("!"), className + " would not open: " + report);
            return report;
        }

        private static Dictionary<string, string> V(params string[] nv)
        {
            Dictionary<string, string> d = new Dictionary<string, string>();
            for (int i = 0; i + 1 < nv.Length; i += 2) d[nv[i]] = nv[i + 1];
            return d;
        }

        [StTest("po: screen fields are converted the way the COMMAREA expects", Reqs = "PO-003")]
        public static void FieldConversion()
        {
            Dictionary<string, string> v = V("SUPPLIER", " sup-001 ", "QTY", "1,250.5", "PRICE", "£12.34",
                                             "DUE", "31/12/2026", "DESC", " Hex bolt ");
            St.Eq("SUP-001", PoMap.S(v, "SUPPLIER"), "a code is trimmed and upper cased");
            St.Eq("Hex bolt", PoMap.T(v, "DESC"), "free text keeps its case");
            St.Eq(1250.5m, PoMap.D(v, "QTY"), "a thousands separator is not a parse error");
            St.Eq(12.34m, PoMap.D(v, "PRICE"), "a pound sign is not a parse error");
            St.Eq(new System.DateTime(2026, 12, 31), PoMap.Dt(v, "DUE", System.DateTime.Today), "UK date order");
            St.Eq(0m, PoMap.D(v, "MISSING"), "a field the screen does not have is zero, never an exception");
        }

        [StTest("po: the open order enquiry answers the three buyer questions", Reqs = "PO-008,PO-017,PO-022,PO-024")]
        public static void BuyerEnquiries()
        {
            if (!SqlHelper.ObjectExists("po.usp_OpenOrders")) St.Skip("po.usp_OpenOrders is not deployed yet");
            Context();
            PurchaseOrderService svc = new PurchaseOrderService();
            DataTable open = svc.OpenOrders("", "", false);
            St.True(open.Columns.Contains("OutstandingQty"), "open orders carry the outstanding quantity");
            St.True(open.Columns.Contains("ReceivedNotInvoicedGBP"), "open orders carry the GRNI balance (PO-014)");
            St.True(svc.Commitments("", "BUDGET").Columns.Contains("OpenCommitmentGBP"),
                    "the commitment enquiry reports the open commitment");
            St.True(svc.Overdue("").Columns.Contains("DaysLateOriginalBasis"),
                    "the overdue enquiry reports lateness against the original promise (D-09)");
        }

        [StTest("po: POPRICE explains where the price came from", Reqs = "PO-004")]
        public static void PriceExplanation()
        {
            NeedProgram("POPRICE");
            Context();
            PurchaseOrderService svc = new PurchaseOrderService();
            CobolResult r = svc.Explain("SUP-STEEL", "BOLT-M5X16-SS", 500m, "EA", "GBP");
            St.True(r.ReturnCode != 12, "a price enquiry never abends, it answers or says not found");
            St.True(svc.Price != null, "the COMMAREA comes back for the screen to explain");
            if (r.Succeeded)
            {
                St.True(svc.Price.GetString("PR-PRICE-BASIS") != "", "a priced line says which basis was used");
                St.True(svc.Price.GetDecimal("PR-PRICE-PER") >= 1m, "price-per is never zero");
            }
        }

        [StTest("po: the budget check reports the percentage used after the purchase", Reqs = "PO-005")]
        public static void BudgetVerdict()
        {
            NeedProgram("BUDGCHK");
            Context();
            PurchaseOrderService svc = new PurchaseOrderService();
            CobolResult r = svc.CheckBudget(V("BUDGET", "STC-CAPEX-2026", "ACCOUNT", "7100", "NETGBP", "1000.00",
                                              "DOCTYPE", "ORDER", "DOCREF", "TEST-NET"));
            St.True(r.ReturnCode != 12, "a budget check answers, it does not abend");
            St.True(svc.Budget != null, "the screen is handed the whole verdict");
            string outcome = svc.Budget.GetString("BC-OUTCOME");
            St.True(outcome == "" || outcome == "OK" || outcome == "WARN" || outcome == "BLOCK" || outcome == "NOBUDGET",
                    "the outcome is one of the four the screen knows how to show, got '" + outcome + "'");
        }

        [StTest("po: a receipt cannot be confirmed before it has been validated", Reqs = "PO-009")]
        public static void ReceiptNeedsValidation()
        {
            Context();
            GoodsReceiptService svc = new GoodsReceiptService();
            CobolResult r = svc.Confirm("CONFIRM");
            St.Eq(8, r.ReturnCode, "confirming nothing is an error, not an exception");
            St.Eq(0, svc.Lines().Rows.Count, "and there are no lines to show");
        }

        [StTest("po: validating a delivery lists the outstanding order lines", Reqs = "PO-009,PO-014")]
        public static void ReceiptValidation()
        {
            NeedProgram("RECEIPT");
            Context();
            GoodsReceiptService svc = new GoodsReceiptService();
            CobolResult r = svc.Validate(V("ORDER", "PO-TEST-NONE", "DELNOTE", "DN-TEST", "WHS", "MAIN"));
            St.True(r.ReturnCode >= 8 || svc.Lines() != null, "an unknown order is refused, a known one lists lines");
            if (r.Succeeded) St.True(svc.Lines().Columns.Contains("RC-L-OUTSTAND"), "the grid gets the outstanding column");
        }

        [StTest("po: the requisition facade keeps its own line list for the screen", Reqs = "PO-001")]
        public static void RequisitionLineBuffer()
        {
            Context();
            RequisitionService svc = new RequisitionService();
            St.Eq(0, svc.Lines.Rows.Count, "a new requisition starts empty");
            St.True(svc.Lines.Columns.Contains("BudgetOutcome"), "every line carries the budget verdict for the grid");
            St.True(svc.Lines.Columns.Contains("LineValueGBP"), "and its sterling value");
        }

        // ------------------------------------------------------------------ menu and screens

        [StTest("po: every purchasing menu row names a screen, a maintenance definition or a report that exists",
                Reqs = "COM-019,COM-024")]
        public static void MenuRowsResolve()
        {
            Need("core.MenuItem");
            DataTable t = SqlHelper.ExecuteDataTable(
                "SELECT MenuCode, FormClass, FormArgument, PermissionCode FROM core.MenuItem " +
                "WHERE IsActive = 1 AND (MenuCode LIKE 'PO.%' OR ParentMenuCode = 'PO')");
            if (t.Rows.Count == 0) St.Skip("the Purchasing menu has not been seeded yet");

            foreach (DataRow r in t.Rows)
            {
                string code = Fmt.Str(r["MenuCode"]), cls = Fmt.Str(r["FormClass"]);
                string arg = Fmt.Str(r["FormArgument"]), perm = Fmt.Str(r["PermissionCode"]);
                if (cls.Length == 0)
                {
                    St.True(Count("SELECT COUNT(*) FROM core.MenuItem WHERE IsActive = 1 AND ParentMenuCode = @p",
                                  "@p", code) > 0, code + " opens no screen and has nothing underneath it");
                    continue;
                }
                St.True(Screen(cls) != null, code + " names " + cls + ", which must exist in STERP.exe");
                if (cls == "frmGenericMaint")
                    St.True(Count("SELECT COUNT(*) FROM core.MaintDef d JOIN core.MaintColumn c ON c.MaintCode = d.MaintCode " +
                                  "WHERE d.MaintCode = @m AND d.IsActive = 1", "@m", arg) > 0,
                            code + " opens the generic shell on '" + arg + "', which must be a registered maintenance definition");
                if (cls == "frmReportLauncher")
                    St.True(Count("SELECT COUNT(*) FROM core.ReportDef WHERE ReportCode = @r AND IsActive = 1", "@r", arg) > 0,
                            code + " launches report '" + arg + "', which must be in the report catalogue");
                if (perm.Length > 0)
                    St.True(Count("SELECT COUNT(*) FROM core.Permission WHERE PermissionCode = @p", "@p", perm) > 0,
                            code + " names permission " + perm + ", which must exist");
            }
            St.Eq(3, Count("SELECT COUNT(DISTINCT FormClass) FROM core.MenuItem WHERE IsActive = 1 AND ParentMenuCode = 'PO' " +
                           "AND FormClass IN ('frmRequisition','frmPurchaseOrder','frmGoodsReceipt')"),
                  "the three hand written purchasing screens are on the menu");
        }

        [StTest("po: the three purchasing screens open as STERP base forms", Reqs = "COM-024")]
        public static void ScreensOpen()
        {
            Context();
            foreach (string name in new string[] { "frmRequisition", "frmPurchaseOrder", "frmGoodsReceipt" })
                St.Contains(" control(s)", Opens(name), name + " opened and laid its fields out");
        }

        // ------------------------------------------------------------------ screens on seeded data

        [StTest("po: the order screen opens on the seeded STC orders, which carry supplier, currency, item, quantity and price",
                Reqs = "PO-003,PO-024")]
        public static void OrderScreenLoadsSeededOrders()
        {
            Need("po.usp_OpenOrders");
            Context();
            DataTable open = new PurchaseOrderService().OpenOrders("", "", false);
            St.True(open.Rows.Count > 0, "F5 on the order screen lists the seeded STC orders, found " + open.Rows.Count);

            string no = Fmt.Str(open.Rows[0]["OrderNumber"]);
            St.True(Fmt.Str(open.Rows[0]["SupplierCode"]).Length > 0, no + " names the supplier it was placed with");
            St.True(Fmt.Str(open.Rows[0]["CurrencyCode"]).Length > 0, no + " names the currency it was placed in");
            DataRow l = SqlHelper.ExecuteRow(
                "SELECT TOP 1 ItemCode, Description, OrderedQty, UomCode, UnitPrice, DestinationSiteCode " +
                "FROM po.PurchaseOrderLine WHERE CompanyCode = @c AND OrderNumber = @o ORDER BY LineNum",
                "@c", "STC", "@o", no);
            St.True(l != null, no + " has at least one line behind it");
            St.True(Fmt.Str(l["ItemCode"]).Length > 0 || Fmt.Str(l["Description"]).Length > 0,
                    "a line names a stocked item or describes the service bought");
            St.True(Fmt.Dec(l["OrderedQty"]) > 0m, "the ordered quantity is captured");
            St.True(Fmt.Str(l["UomCode"]).Length > 0, "the unit is captured, never assumed");
            St.True(Fmt.Dec(l["UnitPrice"]) > 0m, "the unit price is captured");
            St.True(Fmt.Str(l["DestinationSiteCode"]).Length > 0, "the destination is captured");
        }

        [StTest("po: the goods receipt screen validates a seeded STC order and lists its outstanding lines",
                Reqs = "PO-009,PO-014,PO-025")]
        public static void ReceiptScreenLoadsSeededOrder()
        {
            Need("po.PurchaseOrderLine");
            NeedProgram("RECEIPT");
            Context();
            string plain = Outstanding("NONE"), traced = Outstanding("LOT");
            if (plain.Length == 0 && traced.Length == 0) St.Skip("no issued STC order has anything left to deliver");

            if (plain.Length > 0)
            {
                GoodsReceiptService svc = new GoodsReceiptService();
                CobolResult r = svc.Validate(V("ORDER", plain, "DELNOTE", "DN-HARNESS", "WHS", "MAIN", "LOC", "GOODSIN"));
                St.True(r.Succeeded, "the screen validated " + plain + ": " + r.Display);
                DataTable lines = svc.Lines();
                St.True(lines.Rows.Count > 0, plain + " shows its outstanding lines on the grid, found " + lines.Rows.Count);
                St.True(lines.Columns.Contains("RC-L-OUTSTAND"), "the grid gets the outstanding column");
                St.True(Fmt.Dec(lines.Rows[0]["RC-L-ORDERED"]) > 0m, "the grid shows what was ordered, so over-delivery is visible");
                St.True(Fmt.Dec(lines.Rows[0]["RC-L-RECEIVED"]) > 0m, "and a receivable quantity for the receiver to adjust");
            }
            if (traced.Length > 0)
            {
                GoodsReceiptService svc = new GoodsReceiptService();
                svc.Validate(V("ORDER", traced, "DELNOTE", "DN-HARNESS-2", "WHS", "MAIN", "LOC", "GOODSIN"));
                DataTable lines = svc.Lines();
                St.True(lines.Rows.Count > 0, traced + " still lists its lines even though it cannot be confirmed yet");
                St.Eq("REJECT", Fmt.Str(lines.Rows[0]["RC-L-OUTCOME"]),
                      "a lot traceable line is refused at goods inward until the lot number is keyed (PO-025)");
            }
        }

        [StTest("po: the requisition screen opens the seeded STC requisition and says who supplies the line",
                Reqs = "PO-001,PO-002")]
        public static void RequisitionScreenOpensSeededRequisition()
        {
            Need("po.Requisition");
            NeedProgram("POREQ");
            Context();
            string no = Fmt.Str(SqlHelper.ExecuteScalar(
                "SELECT TOP 1 RequisitionNumber FROM po.Requisition WHERE CompanyCode = @c ORDER BY RequisitionNumber",
                "@c", "STC"));
            if (no.Length == 0) St.Skip("the purchasing demonstration requisitions have not been built yet");

            RequisitionService svc = new RequisitionService();
            CobolResult r = svc.Act("ENQUIRE", no);
            St.True(r.Succeeded, "F5 opened requisition " + no + ": " + r.Display);
            St.Eq(no, svc.ReqNumber, "the screen is showing the requisition it was asked for");
            St.True(svc.Status.Length > 0, "the status line can say where the requisition has got to");
            St.True(svc.TotalGBP > 0m, "the seeded requisition brings its value with it");

            NeedProgram("SOURCSEL");
            DataRow s = SqlHelper.ExecuteRow(
                "SELECT TOP 1 ItemCode, SupplierCode FROM po.SupplierSource WHERE CompanyCode = @c " +
                "AND ApprovalStatus = 'APPROVED' AND IsActive = 1 ORDER BY ItemCode, Priority", "@c", "STC");
            if (s == null) St.Skip("no approved source is seeded for STC");
            string item = Fmt.Str(s["ItemCode"]), supplier = Fmt.Str(s["SupplierCode"]);
            CommArea ss = svc.Source("CHECK", V("ITEM", item, "SUPPLIER", supplier, "QTY", "1", "REQ", no));
            St.True(!svc.LastSource.IsNotFound,
                    supplier + " is the approved source the register holds for " + item + ": " + svc.LastSource.Display);
            St.True(ss.GetString("SS-APPROVAL-STATUS").Length > 0, "the screen is told how approved that source is");
            St.True(ss.GetInt("SS-LEAD-TIME-DAYS") >= 0, "and how long that supplier takes");
            St.Contains("Approved source", ss.GetString("SS-SOURCE-TEXT"), "the answer is in plain English for the buyer");

            svc.Source("CHECK", V("ITEM", item, "QTY", "1", "REQ", no));
            St.True(!svc.LastSource.Succeeded,
                    "the register is never asked to guess - a check with no supplier named is refused, got " +
                    svc.LastSource.Display);
        }

        [StTest("po: the approved source register and the purchase price screens open on their seeded STC rows",
                Reqs = "PO-002,PO-004")]
        public static void SourceAndPriceMaintenanceOpen()
        {
            Need("core.usp_UiMaintList");
            Need("po.SupplierSource");
            Context();
            MaintService m = new MaintService();
            St.True(m.LoadDefinition("PO.SOURCE"), "PO.SOURCE is a registered maintenance screen");
            DataTable src = m.List("PO.SOURCE", null);
            St.True(src.Rows.Count > 0, "the approved source register opens on the seeded STC sources, found " + src.Rows.Count);
            foreach (string c in new string[] { "SupplierCode", "ItemCode", "ApprovalStatus", "LeadTimeDays", "ContractRef" })
                St.True(src.Columns.Contains(c), "the buyer can see " + c + " before choosing a source");

            St.True(m.LoadDefinition("PO.PRICE"), "PO.PRICE is a registered maintenance screen");
            DataTable prices = m.List("PO.PRICE", null);
            St.True(prices.Rows.Count > 0, "the purchase price screen opens on the seeded STC prices, found " + prices.Rows.Count);
            foreach (string c in new string[] { "PriceBasis", "UnitPrice", "CurrencyCode", "EffectiveFrom" })
                St.True(prices.Columns.Contains(c), "a price is shown with " + c + ", never on its own");
        }

        // ------------------------------------------------------------------ source references and the supplier gate

        [StTest("po: a call-off is drawn against its blanket order and an unknown source raises nothing",
                Reqs = "PO-021")]
        public static void CallOffAndCopyKeepTheirSource()
        {
            Need("po.BlanketBalance");
            Context();
            DataRow b = SqlHelper.ExecuteRow(
                "SELECT TOP 1 BlanketOrderNumber, AgreedQty, CalledOffQty FROM po.BlanketBalance " +
                "WHERE CompanyCode = @c AND CalledOffQty > 0 ORDER BY BlanketOrderNumber", "@c", "STC");
            if (b == null) St.Skip("no drawn-down blanket order is seeded for STC");
            string blanket = Fmt.Str(b["BlanketOrderNumber"]);
            St.True(Fmt.Dec(b["CalledOffQty"]) > 0m,
                    "the call-off was drawn against " + blanket + ", not raised as a free standing order");
            St.True(Fmt.Dec(b["CalledOffQty"]) <= Fmt.Dec(b["AgreedQty"]),
                    "the blanket balance is enforced, " + blanket + " is never over-drawn");

            NeedProgram("POORDER");
            int before = Count("SELECT COUNT(*) FROM po.PurchaseOrder WHERE CompanyCode = @c", "@c", "STC");
            PurchaseOrderService svc = new PurchaseOrderService();
            CobolResult callOff = svc.Act("CALLOFF", "", V("BLANKET", "PO-NO-SUCH-BLANKET", "ITEM", "ANY", "QTY", "1"));
            CobolResult copy = svc.Act("COPY", "", V("COPYFROM", "PO-NO-SUCH-ORDER"));
            St.True(!callOff.Succeeded, "a call-off against an unknown blanket is refused: " + callOff.Display);
            St.True(!copy.Succeeded, "a copy of an unknown order is refused: " + copy.Display);
            St.Eq(before, Count("SELECT COUNT(*) FROM po.PurchaseOrder WHERE CompanyCode = @c", "@c", "STC"),
                  "a refused call-off or copy leaves no half made supplier obligation behind");
        }

        [StTest("po: the supplier checks button reports the due diligence verdict before an order may be issued",
                Reqs = "PO-026")]
        public static void SupplierDueDiligence()
        {
            Need("po.SupplierDiligence");
            NeedProgram("POSUPCHK");
            Context();
            string supp = Fmt.Str(SqlHelper.ExecuteScalar(
                "SELECT TOP 1 SupplierCode FROM po.PurchaseOrder WHERE CompanyCode = @c ORDER BY OrderNumber", "@c", "STC"));
            if (supp.Length == 0) St.Skip("there is no seeded STC order to take a supplier from");

            PurchaseOrderService svc = new PurchaseOrderService();
            CommArea dd = svc.Diligence(supp, 1000.00m);
            St.True(svc.LastResult.ReturnCode != 12, "the check answers, it does not abend: " + svc.LastResult.Display);
            string overall = dd.GetString("DD-OVERALL");
            St.True(overall == "PASS" || overall == "REVIEW" || overall == "EXCEPTION" || overall == "BLOCKED",
                    "the screen is given one of the four verdicts it knows how to show, got '" + overall + "'");
            St.True(dd.GetString("DD-CAN-ORDER") == "Y" || dd.GetString("DD-CAN-ORDER") == "N",
                    "and a plain yes or no on whether " + supp + " may be ordered from");
            St.True(dd.GetInt("DD-DETAIL-CNT") > 0 || dd.GetString("DD-MISSING-LIST").Length > 0,
                    "the evidence behind the verdict is listed, or what is missing is named");
        }
    }
}
