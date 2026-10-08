using System;
using System.Collections.Generic;
using System.Data;
using STERP.Business;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests
{
    /// <summary>
    /// Every registered generic maintenance screen must open: core.usp_UiMaintDef and
    /// core.usp_UiMaintList are driven entirely by seed data, so a module that seeds a
    /// definition with an unknown table, an unknown column or - the usual mistake - a
    /// permission code that is not in core.Permission leaves frmGenericMaint with the
    /// fallback caption, an empty grid and no error the user can act on.
    /// </summary>
    public static class MaintDefTests
    {
        private const string TestUser = "SYSADM";
        private const string TestCompany = "STC";

        private static void Need(string objectName)
        {
            if (!SqlHelper.ObjectExists(objectName)) St.Skip(objectName + " is not deployed yet");
        }

        private static DataTable ActiveDefinitions()
        {
            return SqlHelper.ExecuteDataTable(
                "SELECT MaintCode, Title, SchemaName, TableName, CompanyScoped, PermissionCode " +
                "FROM core.MaintDef WHERE IsActive = 1 ORDER BY MaintCode");
        }

        /// <summary>True unless a real, registered permission is withheld from the test user.</summary>
        private static bool UserMay(string permissionCode)
        {
            if (permissionCode == null || permissionCode.Trim().Length == 0) return true;
            if (!SqlHelper.ObjectExists("core.fn_HasPermission")) return true;
            // an unknown code is a seeding defect, not segregation of duties: let the screen
            // be exercised so the authorisation failure is reported here too.
            if (Fmt.Int(SqlHelper.ExecuteScalar(
                    "SELECT COUNT(*) FROM core.Permission WHERE PermissionCode = @p", "@p", permissionCode)) == 0)
                return true;
            return Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT CONVERT(int, core.fn_HasPermission(@u, @p, @c, NULL))",
                "@u", TestUser, "@p", permissionCode, "@c", TestCompany)) == 1;
        }

        [StTest("maint: every registered maintenance definition names a permission that exists",
                Reqs = "COM-002,COM-013", Ac = "AC-006")]
        public static void PermissionCodesAreRegistered()
        {
            Need("core.MaintDef");
            Need("core.Permission");
            DataTable defs = ActiveDefinitions();
            if (defs.Rows.Count == 0) St.Skip("core.MaintDef has not been seeded yet");

            List<string> bad = new List<string>();
            foreach (DataRow d in defs.Rows)
            {
                string perm = Fmt.Str(d["PermissionCode"]);
                if (perm.Length == 0) continue;
                if (Fmt.Int(SqlHelper.ExecuteScalar(
                        "SELECT COUNT(*) FROM core.Permission WHERE PermissionCode = @p", "@p", perm)) == 0)
                    bad.Add(Fmt.Str(d["MaintCode"]) + " -> " + perm);
            }
            // core.usp_UiCheckAccess denies a permission the security module cannot answer
            // for, so an unregistered code locks everyone out of the screen (COM-002).
            St.True(bad.Count == 0,
                    "maintenance definitions naming an unknown permission: " + string.Join(", ", bad.ToArray()));
        }

        [StTest("maint: every registered maintenance definition loads its columns and rows",
                Reqs = "COM-013,COM-016")]
        public static void EveryDefinitionLoads()
        {
            Need("core.usp_UiMaintDef");
            Need("core.usp_UiMaintList");
            DataTable defs = ActiveDefinitions();
            if (defs.Rows.Count == 0) St.Skip("core.MaintDef has not been seeded yet");

            SterpSession.Current.UserCode = TestUser;
            SterpSession.Current.CompanyCode = TestCompany;
            SterpSession.Current.SiteCode = "RDG";

            MaintService svc = new MaintService();
            List<string> failures = new List<string>();
            int checked_ = 0;
            foreach (DataRow d in defs.Rows)
            {
                string code = Fmt.Str(d["MaintCode"]);
                string table = Fmt.Str(d["SchemaName"]) + "." + Fmt.Str(d["TableName"]);
                if (!SqlHelper.ObjectExists(table)) continue;          // module has not deployed its table yet
                if (!UserMay(Fmt.Str(d["PermissionCode"]))) continue;  // segregation of duties, not a defect

                checked_++;
                try
                {
                    if (!svc.LoadDefinition(code)) { failures.Add(code + ": the definition did not load"); continue; }
                    if (Fmt.Str(svc.Definition["Title"]).Length == 0) failures.Add(code + ": the definition has no title");
                    if (svc.Columns.Rows.Count == 0)
                    {
                        failures.Add(code + ": no registered column exists on " + table);
                        continue;
                    }

                    DataTable rows = svc.List(code, null);
                    foreach (DataRow c in svc.Columns.Rows)
                    {
                        if (!Fmt.Bool(c["IsInList"]) && !Fmt.Bool(c["IsKey"])) continue;
                        if (!rows.Columns.Contains(Fmt.Str(c["ColumnName"])))
                            failures.Add(code + ": list column " + c["ColumnName"] + " is missing from the result set");
                    }
                    if (Available(d) > 0 && rows.Rows.Count == 0)
                        failures.Add(code + ": " + table + " holds rows but the screen listed none");
                }
                catch (Exception ex)
                {
                    failures.Add(code + ": " + ex.Message);
                }
            }

            St.True(checked_ >= 10, "at least ten maintenance screens were exercised, found " + checked_);
            St.True(failures.Count == 0, "maintenance screens that do not load: " + string.Join("; ", failures.ToArray()));
        }

        /// <summary>Rows the signed-on company may see, so an empty grid over a populated table is caught.</summary>
        private static int Available(DataRow def)
        {
            string table = "[" + Fmt.Str(def["SchemaName"]) + "].[" + Fmt.Str(def["TableName"]) + "]";
            bool scoped = Fmt.Bool(def["CompanyScoped"]) &&
                          Fmt.Int(SqlHelper.ExecuteScalar(
                              "SELECT CONVERT(int, CASE WHEN COL_LENGTH(@t, 'CompanyCode') IS NULL THEN 0 ELSE 1 END)",
                              "@t", table)) == 1;
            string sql = "SELECT COUNT(*) FROM " + table;
            if (scoped) return Fmt.Int(SqlHelper.ExecuteScalar(sql + " WHERE CompanyCode = @c OR CompanyCode = '*'",
                                                               "@c", TestCompany));
            return Fmt.Int(SqlHelper.ExecuteScalar(sql));
        }
    }
}
