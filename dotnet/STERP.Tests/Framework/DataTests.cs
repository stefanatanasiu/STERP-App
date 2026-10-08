using System;
using System.Collections.Generic;
using System.Data;
using STERP.Business;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests
{
    /// <summary>SqlHelper, the generic maintenance contract and the permission filter.</summary>
    public static class DataTests
    {
        private static void Need(string objectName)
        {
            if (!SqlHelper.ObjectExists(objectName)) St.Skip(objectName + " is not deployed yet");
        }

        [StTest("sql: SqlHelper runs a core stored procedure and returns a DataTable", Reqs = "COM-011")]
        public static void StoredProcedure()
        {
            Need("core.usp_UiMaintDefList");
            DataTable t = SqlHelper.ExecuteDataTable("core.usp_UiMaintDefList",
                "@UserCode", "SYSADM", "@CompanyCode", "STC");
            St.True(t.Columns.Contains("MaintCode"), "result set has MaintCode");
            St.True(t.Rows.Count >= 10, "at least ten core maintenance screens are registered, found " + t.Rows.Count);
        }

        [StTest("sql: a THROWn 'STERP-E-nnnn: text' becomes a BusinessRuleException", Reqs = "COM-017")]
        public static void ErrorTranslation()
        {
            Need("core.usp_UiMaintSave");
            try
            {
                SqlHelper.ExecuteNonQuery("core.usp_UiMaintSave", "@MaintCode", "NO.SUCH.SCREEN",
                    "@UserCode", "SYSADM", "@CompanyCode", "STC", "@Mode", "ADD", "@Data", "<r/>");
                St.Fail("the procedure should have refused an unknown maintenance code");
            }
            catch (BusinessRuleException ex)
            {
                St.Eq("E-0090", ex.Code, "message id taken from the THROW text");
                St.Eq('E', ex.Severity, "severity");
                St.Contains("not registered", ex.Message, "human readable message");
            }
        }

        [StTest("maint: add, change and retire round trip through core.usp_UiMaintSave", Reqs = "COM-013,COM-016")]
        public static void MaintenanceRoundTrip()
        {
            Need("core.usp_UiMaintSave");
            SterpSession.Current.UserCode = "SYSADM";
            SterpSession.Current.CompanyCode = "STC";
            MaintService svc = new MaintService();
            St.True(svc.LoadDefinition("CORE.CURRENCY"), "CORE.CURRENCY is registered");
            St.True(svc.Columns.Rows.Count >= 4, "its columns are registered");

            Dictionary<string, string> row = new Dictionary<string, string>();
            row["CurrencyCode"] = "ZZT";
            row["Name"] = "Harness currency";
            row["Symbol"] = "Z";
            row["MinorUnits"] = "2";
            row["IsActive"] = "1";
            try
            {
                SqlHelper.ExecuteNonQuery("DELETE FROM core.Currency WHERE CurrencyCode = @c", "@c", "ZZT");
                St.Eq("", svc.Save("CORE.CURRENCY", "ADD", row), "ADD");
                DataTable t = svc.List("CORE.CURRENCY", "ZZT");
                St.Eq(1, t.Rows.Count, "the new row is listed");
                St.Eq("Harness currency", t.Rows[0]["Name"], "inserted name");

                row["Name"] = "Amended by the harness";
                St.Eq("", svc.Save("CORE.CURRENCY", "CHANGE", row), "CHANGE");
                St.Eq("Amended by the harness", svc.List("CORE.CURRENCY", "ZZT").Rows[0]["Name"], "amended name");

                Dictionary<string, string> key = new Dictionary<string, string>();
                key["CurrencyCode"] = "ZZT";
                St.Eq("", svc.Save("CORE.CURRENCY", "RETIRE", key), "RETIRE");
                DataTable after = svc.List("CORE.CURRENCY", "ZZT");
                St.Eq(1, after.Rows.Count, "RETIRE does not delete the row");
                St.Eq(false, after.Rows[0]["IsActive"], "the retire flag is cleared");

                if (SqlHelper.ObjectExists("core.AuditLog"))
                {
                    object n = SqlHelper.ExecuteScalar(
                        "SELECT COUNT(*) FROM core.AuditLog WHERE EntityType = @e AND EntityKey = @k",
                        "@e", "CORE.CURRENCY", "@k", "ZZT");
                    St.True(Fmt.Int(n) >= 3, "three audit rows were written, found " + Fmt.Int(n));
                }
            }
            finally
            {
                SqlHelper.ExecuteNonQuery("DELETE FROM core.Currency WHERE CurrencyCode = @c", "@c", "ZZT");
                if (SqlHelper.ObjectExists("core.AuditLog"))
                    SqlHelper.ExecuteNonQuery("DELETE FROM core.AuditLog WHERE EntityKey = @k", "@k", "ZZT");
            }
        }

        [StTest("maint: the column whitelist rejects a column that is not registered", Reqs = "COM-002")]
        public static void Whitelist()
        {
            Need("core.usp_UiMaintSave");
            SterpSession.Current.UserCode = "SYSADM";
            SterpSession.Current.CompanyCode = "STC";
            MaintService svc = new MaintService();
            Dictionary<string, string> row = new Dictionary<string, string>();
            row["CurrencyCode"] = "ZZW";
            row["Name"] = "Whitelist test";
            row["Symbol"] = "W";
            row["MinorUnits"] = "2";
            row["IsActive"] = "1";
            row["CreatedAt"] = "1900-01-01";          // real column, not registered - must be ignored
            try
            {
                SqlHelper.ExecuteNonQuery("DELETE FROM core.Currency WHERE CurrencyCode = @c", "@c", "ZZW");
                St.Eq("", svc.Save("CORE.CURRENCY", "ADD", row), "ADD with an unregistered column");
                object created = SqlHelper.ExecuteScalar(
                    "SELECT CONVERT(varchar(4), YEAR(CreatedAt)) FROM core.Currency WHERE CurrencyCode = @c", "@c", "ZZW");
                St.Eq(DateTime.Today.Year.ToString(), Fmt.Str(created), "CreatedAt was not taken from the client");
            }
            finally
            {
                SqlHelper.ExecuteNonQuery("DELETE FROM core.Currency WHERE CurrencyCode = @c", "@c", "ZZW");
                if (SqlHelper.ObjectExists("core.AuditLog"))
                    SqlHelper.ExecuteNonQuery("DELETE FROM core.AuditLog WHERE EntityKey = @k", "@k", "ZZW");
            }
        }

        [StTest("prefs: a user preference survives a round trip", Reqs = "COM-024")]
        public static void Preferences()
        {
            Need("core.usp_UiUserPrefSet");
            SterpSession.Current.UserCode = "SYSADM";
            PreferenceService p = new PreferenceService();
            p.SetValue("TEST.HARNESS", "1024x768");
            St.Eq("1024x768", p.GetValue("TEST.HARNESS", ""), "stored value");
            p.SetValue("TEST.HARNESS", "800x600");
            St.Eq("800x600", p.GetValue("TEST.HARNESS", ""), "amended value");
            SqlHelper.ExecuteNonQuery("DELETE FROM core.UserPreference WHERE PrefKey = @k", "@k", "TEST.HARNESS");
            St.Eq("(none)", p.GetValue("TEST.HARNESS", "(none)"), "default after removal");
        }

        [StTest("menu: core.usp_UiMenuLoad returns the shell menus", Reqs = "COM-002,COM-024")]
        public static void MenuLoad()
        {
            Need("core.usp_UiMenuLoad");
            SterpSession.Current.UserCode = "SYSADM";
            SterpSession.Current.CompanyCode = "STC";
            SterpSession.Current.SiteCode = "RDG";
            DataTable t = new MenuService().Load();
            if (t.Rows.Count == 0) St.Skip("core.MenuItem has not been seeded yet");
            St.True(t.Columns.Contains("FormClass"), "FormClass is published to the client");
            int tops = t.Select("ParentMenuCode is null").Length;
            St.True(tops >= 4, "at least File, System, Window and Help exist, found " + tops);
            St.True(t.Select("MenuCode = 'HELP.ABOUT'").Length == 1, "Help/About is seeded");
            St.True(t.Select("FormClass = 'frmGenericMaint'").Length >= 10,
                    "the core masters are reachable through the generic maintenance form");
        }

        [StTest("security: the RESTRICT user is refused company CAL", Reqs = "COM-002", Ac = "AC-006")]
        public static void RestrictedUser()
        {
            Need("core.usp_CheckPermission");
            if (Fmt.Int(SqlHelper.ExecuteScalar("SELECT COUNT(*) FROM core.AppUser WHERE UserCode = 'RESTRICT'")) == 0)
                St.Skip("the RESTRICT demonstration user has not been seeded yet");

            // RESTRICT is seeded with a role scoped to company STC site RDG only. Take a permission the
            // role really holds so the test measures the company/site filter, not the seeded taxonomy.
            string perm = Fmt.Str(SqlHelper.ExecuteScalar(
                "SELECT TOP 1 rp.PermissionCode FROM core.RolePermission rp " +
                "JOIN core.UserRole ur ON ur.RoleCode = rp.RoleCode " +
                "WHERE ur.UserCode = 'RESTRICT' ORDER BY rp.PermissionCode"));
            if (perm.Length == 0) St.Skip("the RESTRICT role holds no permissions yet");

            SterpSession.Current.UserCode = "RESTRICT";
            LoginService login = new LoginService();

            St.Eq(true, Allowed(login, "STC", "RDG", perm), "RESTRICT holds " + perm + " in its own company STC site RDG");
            St.Eq(false, Allowed(login, "CAL", "GLA", perm), "RESTRICT must not reach company CAL");
            St.Eq(false, Allowed(login, "STC", "BHM", perm), "RESTRICT must not reach site BHM");

            DataRow denial = SqlHelper.ExecuteDataTable("core.usp_CheckPermission",
                "@UserCode", "RESTRICT", "@PermissionCode", perm,
                "@CompanyCode", "CAL", "@SiteCode", "GLA").Rows[0];
            St.Eq(false, Fmt.Bool(denial["Allowed"]), "the procedure itself refuses company CAL");
            St.Contains("STERP-E-", Fmt.Str(denial["Reason"]), "the refusal carries a STERP message id");
        }

        private static bool Allowed(LoginService login, string company, string site, string permission)
        {
            SterpSession.Current.CompanyCode = company;
            SterpSession.Current.SiteCode = site;
            SterpSession.Current.PermissionCache.Clear();
            return login.HasPermission(permission);
        }

        [StTest("security: core.usp_GetUserContext maps the Windows login to a STERP user", Reqs = "COM-002")]
        public static void UserContext()
        {
            Need("core.usp_GetUserContext");
            SterpSession.Current = new SterpSession();
            LoginService login = new LoginService();
            string msg = login.SignOn();
            if (msg != "") St.Skip("no STERP user is mapped to " + login.WindowsLogin + " yet");
            St.True(SterpSession.Current.UserCode.Length > 0, "a user code was returned");
            St.True(SterpSession.Current.Access.Count > 0, "at least one company/site was returned");
            login.LoadPeriod(DateTime.Today);
            St.True(SterpSession.Current.CompanyCode.Length > 0, "a company was selected");
        }
    }
}
