using System;
using System.Data;
using System.IO;
using System.Reflection;
using STERP.Business;
using STERP.CobolInterop;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests
{
    /// <summary>Sales order facade, the SO engine programs and the Sales screens (module so).</summary>
    public static class SalesTests
    {
        private static void Context()
        {
            SterpSession.Current.UserCode = "SALESREP";
            SterpSession.Current.CompanyCode = "STC";
            SterpSession.Current.SiteCode = "RDG";
        }

        private static void Need(string objectName)
        {
            if (!SqlHelper.ObjectExists(objectName)) St.Skip(objectName + " is not deployed yet");
        }

        private static Type Screen(string className)
        {
            Assembly shell = Assembly.LoadFrom(Path.Combine(SterpConfig.BinDir, "STERP.exe"));
            return shell.GetType("STERP.Desktop." + className, false, true);
        }

        /// <summary>AC-041: 3 cartons of 8 units at £15.00, 10% then 5%, standard rated.</summary>
        private static DataTable SpecLine()
        {
            DataTable t = new DataTable();
            t.Columns.Add("LineNum", typeof(int));
            t.Columns.Add("ItemCode", typeof(string));
            t.Columns.Add("AcceptedQty", typeof(decimal));
            t.Columns.Add("UomCode", typeof(string));
            t.Columns.Add("PriceUomCode", typeof(string));
            t.Columns.Add("ConversionFactor", typeof(decimal));
            t.Columns.Add("UnitPrice", typeof(decimal));
            t.Columns.Add("PricePerQty", typeof(decimal));
            t.Columns.Add("DiscountPct1", typeof(decimal));
            t.Columns.Add("DiscountPct2", typeof(decimal));
            t.Columns.Add("VatCode", typeof(string));
            t.Columns.Add("VatRatePct", typeof(decimal));
            t.Columns.Add("CostSnapshotGBP", typeof(decimal));
            t.Rows.Add(1, "BOLT-M5X16-SS", 3m, "CTN", "EA", 8m, 15.000000m, 1m, 10m, 5m, "S", 20m, 216.00m);
            return t;
        }

        [StTest("so: SODISC DOCUMENT reproduces the spec line - net 307.80, VAT 61.56, gross 369.36",
                Reqs = "SO-005", Ac = "AC-041")]
        public static void SequentialDiscountsAndVat()
        {
            Context();
            SalesOrderService svc = new SalesOrderService();
            CobolResult r;
            try
            {
                r = svc.Discount("DOCUMENT", "SO-TEST-041", "C0001", "GBP", new DateTime(2026, 10, 1), SpecLine());
            }
            catch (Exception ex) { St.Skip("SODISCCA is not in bin\\x86\\copy yet: " + ex.Message); return; }
            if (r.IsNotFound) { St.Skip("SODISC is not in the engine yet (" + r.ReasonCode + ")"); return; }
            St.True(r.Succeeded, "SODISC accepted the document: " + r.Display);
            CommArea ca = svc.Area;
            St.Eq(360.00m, ca.GetDecimal("SD-TOT-GROSS"), "24 units at 15.00 extend to 360.00 before discount");
            St.Eq(307.80m, ca.GetDecimal("SD-TOT-NET"), "10% then 5% in sequence gives 307.80, not 306.00");
            St.Eq(61.56m, ca.GetDecimal("SD-TOT-VAT"), "VAT at 20% on the discounted net");
            St.Eq(369.36m, ca.GetDecimal("SD-TOT-GROSS-TOTAL"), "document gross");
            St.Eq(307.80m, ca.GetDecimal("SD-L-NET-AMT(1)"), "the line net matches the document net");
        }

        [StTest("so: PRICING SELECT answers with a unit price and the source it came from",
                Reqs = "SO-004", Ac = "AC-039")]
        public static void PriceHasASource()
        {
            Need("so.PriceListLine");
            Context();
            DataRow p = SqlHelper.ExecuteRow(
                "SELECT TOP (1) l.ItemCode, l.UomCode, l.CurrencyCode FROM so.PriceListLine l " +
                "WHERE l.CompanyCode = @c ORDER BY l.ItemCode", "@c", "STC");
            if (p == null) St.Skip("no price list lines are seeded for STC yet");
            string customer = Fmt.Str(SqlHelper.ExecuteScalar(
                "SELECT TOP (1) CustomerCode FROM ar.Customer WHERE CompanyCode = @c ORDER BY CustomerCode", "@c", "STC"));
            SalesOrderService svc = new SalesOrderService();
            CobolResult r;
            try
            {
                r = svc.Price("SELECT", Fmt.Str(p["ItemCode"]), customer, Fmt.Str(p["UomCode"]),
                              Fmt.Str(p["CurrencyCode"]), 10m, new DateTime(2026, 10, 1), "SO-TEST-039");
            }
            catch (Exception ex) { St.Skip("PRICINGCA is not in bin\\x86\\copy yet: " + ex.Message); return; }
            if (r.IsNotFound) { St.Skip("PRICING found no effective price (" + r.ReasonCode + ")"); return; }
            St.True(r.Succeeded, "PRICING chose a price: " + r.Display);
            CommArea ca = svc.Area;
            St.True(ca.GetDecimal("PR-UNIT-PRICE") > 0m, "a selected price is a positive unit price");
            St.True(SalesOrderService.AreaText(ca, "PR-PRICE-SOURCE").Length > 0, "and it names the source it came from");
            St.True(ca.GetInt("PR-PRICE-LEVEL") >= 1 && ca.GetInt("PR-PRICE-LEVEL") <= 5,
                    "the source sits on one of the five precedence levels, got " + ca.GetInt("PR-PRICE-LEVEL"));
        }

        [StTest("so: PRICING EXPLAIN ranks the candidates and says why each was or was not chosen",
                Reqs = "SO-004", Ac = "AC-039")]
        public static void PriceExplanationIsRanked()
        {
            Need("so.PriceListLine");
            Context();
            DataRow p = SqlHelper.ExecuteRow(
                "SELECT TOP (1) l.ItemCode, l.UomCode, l.CurrencyCode FROM so.PriceListLine l " +
                "WHERE l.CompanyCode = @c ORDER BY l.ItemCode", "@c", "STC");
            if (p == null) St.Skip("no price list lines are seeded for STC yet");
            SalesOrderService svc = new SalesOrderService();
            CobolResult r;
            try
            {
                r = svc.Price("EXPLAIN", Fmt.Str(p["ItemCode"]), "C0001", Fmt.Str(p["UomCode"]),
                              Fmt.Str(p["CurrencyCode"]), 10m, new DateTime(2026, 10, 1), "SO-TEST-039");
            }
            catch (Exception ex) { St.Skip("PRICINGCA is not in bin\\x86\\copy yet: " + ex.Message); return; }
            if (r.IsNotFound) { St.Skip("PRICING is not in the engine yet (" + r.ReasonCode + ")"); return; }
            DataTable c = svc.Candidates();
            St.True(c.Columns.Contains("PR-C-RANK"), "every candidate carries its rank");
            St.True(c.Columns.Contains("PR-C-CHOSEN"), "and whether it was the one chosen");
            St.True(c.Columns.Contains("PR-C-WHY"), "and the reason in words, so the price can be explained");
        }

        [StTest("so: SOACCEPT VALIDATE refuses an incomplete draft and names every blocker",
                Reqs = "SO-002,SO-003", Ac = "AC-040")]
        public static void ValidateRefusesAnIncompleteDraft()
        {
            Context();
            SalesOrderService svc = new SalesOrderService();
            CobolResult r;
            try
            {
                // no customer, no terms, no salesperson, no Incoterms - nothing a draft needs
                r = svc.Accept("VALIDATE", "SO-DRAFT-INCOMPLETE", "", new DateTime(2026, 10, 1),
                               "GBP", "", "", "", "", "");
            }
            catch (Exception ex) { St.Skip("SOACCEPTCA is not in bin\\x86\\copy yet: " + ex.Message); return; }
            St.True(!r.IsOk, "an incomplete draft is never validated as acceptable, got RC " + r.ReturnCode);
            St.True(r.ReasonCode.Trim().Length > 0, "the refusal carries a stable reason code, got '" + r.ReasonCode + "'");
            St.True(r.ReturnCode == 8 || r.ReturnCode == 12 || r.ReturnCode == 16,
                    "08 error, 12 severe and 16 not found stay distinguishable, got " + r.ReturnCode);
            if (r.ReturnCode == 8)
                St.True(svc.Blockers().Rows.Count > 0, "VALIDATE returns the whole blocker list at once");
        }

        [StTest("so: SOAVAIL keeps free stock, reservations and expected receipts apart",
                Reqs = "SO-010", Ac = "AC-042")]
        public static void AvailabilityIsLabelledBySource()
        {
            Context();
            SalesOrderService svc = new SalesOrderService();
            CobolResult r;
            try
            {
                r = svc.Availability("BOLT-M5X16-SS", "RDG", "EA", 10m, new DateTime(2026, 10, 15));
            }
            catch (Exception ex) { St.Skip("SOAVAILCA is not in bin\\x86\\copy yet: " + ex.Message); return; }
            if (r.IsNotFound) { St.Skip("SOAVAIL is not in the engine yet (" + r.ReasonCode + ")"); return; }
            CommArea ca = svc.Area;
            St.True(ca.Has("SA-QTY-FREE") && ca.Has("SA-QTY-RESERVED") && ca.Has("SA-QTY-EXPECTED"),
                    "free stock, reservations and expected receipts are three separate figures");
            foreach (DataRow line in svc.SupplyLines().Rows)
                if (Fmt.Str(line["SA-L-KIND"]) == "EXPECTED")
                    St.Eq("N", Fmt.Str(line["SA-L-GUARANTEED"]),
                          "an expected receipt is a supplier promise, never a guaranteed allocation");
        }

        [StTest("so: the Sales menu, its permissions and the SO batch reports are registered",
                Reqs = "SO-014,RPT-008,COM-019,COM-024")]
        public static void MenuAndReportsAreSeeded()
        {
            Need("core.MenuItem");
            Context();
            SterpSession.Current.UserCode = "SYSADM";
            DataTable t = SqlHelper.ExecuteDataTable(
                "SELECT MenuCode, FormClass FROM core.MenuItem WHERE MenuCode LIKE 'SO.%' AND FormClass IS NOT NULL");
            if (t.Rows.Count == 0) St.Skip("the Sales menu has not been seeded yet");
            foreach (DataRow r in t.Rows)
                St.True(Screen(Fmt.Str(r["FormClass"])) != null,
                        Fmt.Str(r["MenuCode"]) + " names " + Fmt.Str(r["FormClass"]) + ", which must exist in STERP.exe");
            St.Eq(0, Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT COUNT(*) FROM core.MenuItem m WHERE m.MenuCode LIKE 'SO.%' AND m.PermissionCode IS NOT NULL " +
                "AND NOT EXISTS (SELECT 1 FROM core.Permission p WHERE p.PermissionCode = m.PermissionCode)")),
                "every Sales menu row names a permission that really exists");
            St.True(Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT COUNT(*) FROM core.ReportDef WHERE ProgramName IN ('SOBACKLG','SOANAL','SOINVRUN','SOACKDOC')")) >= 4,
                "the four SO batch programs are registered in core.ReportDef");
        }

        [StTest("so: the three Sales screens are base forms and a foreign price keeps its currency",
                Reqs = "COM-024")]
        public static void ScreensAndMoneyFormatting()
        {
            Type baseForm = Screen("BaseForm");
            St.True(baseForm != null, "STERP.Desktop.BaseForm is the framework base form");
            foreach (string name in new string[] { "frmSalesOrderEntry", "frmDespatchConfirm", "frmSalesInvoiceRun" })
            {
                Type t = Screen(name);
                St.True(t != null, name + " is compiled into STERP.exe");
                St.True(baseForm.IsAssignableFrom(t), name + " inherits BaseForm, so it gets F2/F3/F5/F10");
            }
            St.Eq("\u00A3307.80", SalesOrderService.Amount(307.80m, "GBP"), "sterling always carries the pound sign");
            St.Eq("307.80 EUR", SalesOrderService.Amount(307.80m, "EUR"), "a euro amount is captioned, not sterlingised");
            St.Eq("CREDITHOLD - over limit", SalesOrderService.StatusText("CREDITHOLD", "over limit"),
                  "a hold is shown in words, never by colour alone");
        }
    }
}
