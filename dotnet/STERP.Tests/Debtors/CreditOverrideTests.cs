using System;
using System.Data;
using System.Reflection;
using System.IO;
using System.Windows.Forms;
using STERP.Business;
using STERP.CobolInterop;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests
{
    public static class CreditOverrideTests
    {
        private static void Isolated()
        {
            string db = Fmt.Str(SqlHelper.ExecuteScalar("SELECT DB_NAME()"));
            if (!db.StartsWith("STERP_QA_CREDIT_", StringComparison.OrdinalIgnoreCase) ||
                SterpConfig.OdbcConnectionString.IndexOf("Database=" + db + ";", StringComparison.OrdinalIgnoreCase) < 0)
                St.Skip("Mutating credit tests require matching SQL/ODBC STERP_QA_CREDIT_ isolated database");
            SterpSession.Current.CompanyCode = "CAL";
            SterpSession.Current.SiteCode = "LIV";
            SterpSession.Current.BusinessDate = DateTime.Today;
        }

        [StTest("ar: credit override native lifecycle enforces independent scoped authority and audit", Reqs = "AR-008,D-06,COM-002")]
        public static void CreditOverrideLifecycle()
        {
            Isolated();
            string doc = "CRQA-" + Guid.NewGuid().ToString("N").Substring(0, 16);
            var svc = new DebtorsService();
            SterpSession.Current.UserCode = "SALESREP";
            var r = svc.RequestCreditOverride("K0004", doc, 7128m, "Independent credit regression");
            St.Eq(4, r.ReturnCode, r.Display);
            int id = svc.Area.GetInt("CK-OVERRIDE-ID");
            St.True(id > 0, "native request returns the inserted identity");
            St.Eq("PENDING", svc.Area.GetString("CK-OVERRIDE-STATUS"), "request is not authority");
            St.True(svc.Area.GetString("CK-OVERRIDE-EXPIRY").Length == 8, "bounded expiry is calculated");
            St.Eq(6, svc.ApproveCreditOverride("K0004", id).ReturnCode, "requester cannot approve");
            SterpSession.Current.UserCode = "CREDMGR";
            SterpSession.Current.CompanyCode = "STC";
            SterpSession.Current.SiteCode = "RDG";
            St.Eq(16, svc.ApproveCreditOverride("C0001", id).ReturnCode, "other company cannot approve by identity");
            SterpSession.Current.CompanyCode = "CAL";
            SterpSession.Current.SiteCode = "LIV";
            r = svc.ApproveCreditOverride("K0004", id);
            St.Eq(0, r.ReturnCode, r.Display);
            DataRow row = SqlHelper.ExecuteRow("SELECT * FROM ar.CreditOverride WHERE OverrideId=@id", "@id", id);
            St.Eq("ACTIVE", Fmt.Str(row["Status"]), "approval activates the request");
            St.Eq("SALESREP", Fmt.Str(row["RequestedBy"]), "actual requester persisted");
            St.Eq("CREDMGR", Fmt.Str(row["ApprovedBy"]), "independent approver persisted");
            St.True(!Convert.IsDBNull(row["ApprovedAt"]), "approval timestamp persisted");
            St.Eq(2, Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT COUNT(*) FROM core.AuditLog WHERE EntityType='ar.CreditOverride' AND EntityKey=@id " +
                "AND Action IN ('REQUEST','APPROVE') AND Reason='Independent credit regression' AND CorrelationId IS NOT NULL",
                "@id", id.ToString())), "request and approval audited with reason/correlation");
            svc.CreditCheck("K0004", 0m, "GBP", "ORDER", doc);
            St.Eq("Y", svc.Area.GetString("CK-OVERRIDE-ACTIVE"), "matching approved document is active");
            svc.CreditCheck("K0004", 8000m, "GBP", "ORDER", doc);
            St.Eq("HOLD", svc.Area.GetString("CK-DECISION"), "amount cannot cover a larger held commitment");
            svc.CreditCheck("K0004", 0m, "GBP", "ORDER", doc + "-OTHER");
            St.Eq("N", svc.Area.GetString("CK-OVERRIDE-ACTIVE"), "different document has no authority");
            St.Eq(8, svc.ApproveCreditOverride("K0004", id).ReturnCode, "approval cannot be repeated");
            SqlHelper.ExecuteNonQuery("UPDATE ar.CreditOverride SET ExpiresOn=DATEADD(day,-1,@d) WHERE OverrideId=@id",
                "@d", DateTime.Today, "@id", id);
            svc.CreditCheck("K0004", 0m, "GBP", "ORDER", doc);
            St.Eq("N", svc.Area.GetString("CK-OVERRIDE-ACTIVE"), "expired authority cannot release");
            SqlHelper.ExecuteNonQuery("UPDATE ar.CreditOverride SET ExpiresOn=@d, ApprovedBy=NULL WHERE OverrideId=@id",
                "@d", DateTime.Today.AddDays(2), "@id", id);
            svc.CreditCheck("K0004", 0m, "GBP", "ORDER", doc);
            St.Eq("N", svc.Area.GetString("CK-OVERRIDE-ACTIVE"), "ACTIVE without independent approval has no authority");
            SterpSession.Current.UserCode = "SALESREP";
            r = svc.RequestCreditOverride("K0004", doc + "-HIGH", 50001m, "Controller authority regression");
            St.Eq(4, r.ReturnCode, r.Display);
            id = svc.Area.GetInt("CK-OVERRIDE-ID");
            SterpSession.Current.UserCode = "CREDMGR";
            St.Eq(6, svc.ApproveCreditOverride("K0004", id).ReturnCode, "persisted amount demands financial controller");
            SterpSession.Current.UserCode = "FINCTRL";
            r = svc.ApproveCreditOverride("K0004", id);
            St.Eq(0, r.ReturnCode, r.Display);
            SterpSession.Current.UserCode = "CREDMGR";
            r = svc.RequestCreditOverride("K0004", doc + "-MGR", 100m, "Manager may request but not self approve");
            St.Eq(4, r.ReturnCode, r.Display);
            St.Eq(6, svc.ApproveCreditOverride("K0004", svc.Area.GetInt("CK-OVERRIDE-ID")).ReturnCode,
                "even an authorized manager cannot approve their own request");
            SterpSession.Current.UserCode = "WHSE";
            St.Eq(6, svc.RequestCreditOverride("K0004", doc + "-DENIED", 100m, "No request permission").ReturnCode,
                "unrelated warehouse user cannot request credit authority");
        }

        [StTest("ar: credit override native menus preserve sales request and credit approval separation", Reqs = "AR-008,COM-002,COM-024")]
        public static void CreditOverrideMenuReachability()
        {
            Isolated();
            var shell = Assembly.LoadFrom(Path.Combine(SterpConfig.BinDir, "STERP.exe"));
            Type form = shell.GetType("STERP.Desktop.frmCreditOverride");
            St.True(form != null && shell.GetType("STERP.Desktop.BaseForm").IsAssignableFrom(form), "dedicated native form deployed");
            using (var screen = (Form)Activator.CreateInstance(form))
                foreach (string name in new[] { "txtCustomer", "txtDocument", "nudAmount", "txtReason", "btnRequest", "btnApprove", "grdOverrides" })
                    St.Eq(1, screen.Controls.Find(name, true).Length, "native control reachable: " + name);
            DataRow request = SqlHelper.ExecuteRow("SELECT * FROM core.MenuItem WHERE MenuCode='AR.OVERRIDE.REQUEST'");
            DataRow approval = SqlHelper.ExecuteRow("SELECT * FROM core.MenuItem WHERE MenuCode='AR.OVERRIDE.MAINT'");
            St.Eq("SO", Fmt.Str(request["ParentMenuCode"]), "sales request reachable from Sales menu");
            St.Eq("REQUEST", Fmt.Str(request["FormArgument"]), "F10 requests, never approves");
            St.Eq("frmCreditOverride", Fmt.Str(approval["FormClass"]), "approval no longer generic maintenance");
            foreach (var pair in new[] { new[] { "SALESREP", "SO.ORDER.ENTER", "1" }, new[] { "SALESREP", "AR.CREDIT.OVERRIDE", "0" },
                new[] { "CREDMGR", "AR.CREDIT.OVERRIDE", "1" }, new[] { "CREDMGR", "SO.ORDER.ENTER", "0" } })
                St.Eq(pair[2], Fmt.Int(SqlHelper.ExecuteScalar(
                    "SELECT CONVERT(int,core.fn_HasPermission(@u,@p,'CAL','LIV'))", "@u", pair[0], "@p", pair[1])).ToString(),
                    pair[0] + " " + pair[1]);
            St.Eq("K0004", Fmt.Str(SqlHelper.ExecuteScalar(
                "SELECT InvoiceCustomerCode FROM so.SalesOrder WHERE CompanyCode='CAL' AND OrderNumber='SOC900003'")),
                "only known held-order fixture has invoicing party");
        }

        [StTest("ar: Livingston held order resumes through normal sales acceptance after scoped approval", Reqs = "AR-008,SO-007,D-06")]
        public static void CreditOverrideHeldOrderRelease()
        {
            Isolated();
            var sales = new SalesOrderService();
            var credit = new DebtorsService();
            SterpSession.Current.UserCode = "SALESREP";
            DataRow header = sales.Header("SOC900003");
            if (Fmt.Str(header["ReleaseStatus"]) == "RELEASED") St.Skip("isolated fixture already released by this test");
            var r = credit.RequestCreditOverride("K0004", "SOC900003", 7128m, "Livingston first-order scoped authority");
            St.Eq(4, r.ReturnCode, r.Display);
            int id = credit.Area.GetInt("CK-OVERRIDE-ID");
            SterpSession.Current.UserCode = "CREDMGR";
            r = credit.ApproveCreditOverride("K0004", id);
            St.Eq(0, r.ReturnCode, r.Display);
            SterpSession.Current.UserCode = "SALESREP";
            r = sales.Accept("ACCEPT", "SOC900003", "K0004", Convert.ToDateTime(header["OrderDate"]),
                Fmt.Str(header["CurrencyCode"]), Fmt.Str(header["TermsCode"]), Fmt.Str(header["SalespersonCode"]),
                Fmt.Str(header["IncotermsRule"]), Fmt.Str(header["IncotermsNamedPlace"]), Fmt.Str(header["DeliveryAddressRef"]));
            St.True(r.ReturnCode <= 4, "normal SOACCEPT must succeed: " + r.Display);
            St.Eq("RELEASED", Fmt.Str(sales.Header("SOC900003")["ReleaseStatus"]), "engine, not a data edit, releases order");
        }
    }
}
