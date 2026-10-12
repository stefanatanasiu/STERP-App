using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading;
using System.Windows.Forms;
using STERP.Business.Bom;
using STERP.CobolInterop;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests
{
    public static class BomBulkGuiTests
    {
        private static void Context(string user = "ENGINEER")
        {
            string database = Fmt.Str(SqlHelper.ExecuteScalar("SELECT DB_NAME()"));
            if (!database.StartsWith("STERP_QA_BOM_", StringComparison.OrdinalIgnoreCase))
                St.Skip("requires dedicated STERP_QA_BOM_ clone");
            SterpSession.Current.UserCode = user;
            SterpSession.Current.CompanyCode = "STC";
            SterpSession.Current.SiteCode = "RDG";
            SterpSession.Current.BusinessDate = new DateTime(2026, 10, 12);
            SterpSession.Current.IsSysAdm = false;
            SterpSession.Current.PermissionCache.Clear();
            var ping = CommArea.For("PINGCA");
            St.True(CobolGateway.Link("PING", ping, "HEALTH").Succeeded, "native health");
            St.Eq(database, ping.GetString("PG-DB-NAME"), "native uses isolated database");
        }

        private static string Snapshot()
        {
            return Fmt.Str(SqlHelper.ExecuteScalar(
                "SELECT CONCAT((SELECT COUNT(*) FROM bom.BomHeader),'/'," +
                "(SELECT CHECKSUM_AGG(BINARY_CHECKSUM(*)) FROM bom.BomHeader),'/'," +
                "(SELECT CHECKSUM_AGG(BINARY_CHECKSUM(*)) FROM bom.BomComponent))"));
        }

        private static IEnumerable<ToolStripMenuItem> MenuItems(ToolStripItemCollection items)
        {
            foreach (var item in items.OfType<ToolStripMenuItem>())
            {
                yield return item;
                foreach (var child in MenuItems(item.DropDownItems)) yield return child;
            }
        }

        [StTest("bom bulk gui: exact rendered preview route does not grant approval", Reqs = "BOM-024,COM-002")]
        public static void RenderedRoute()
        {
            Context();
            var menus = new STERP.Business.MenuService().Load();
            var row = menus.Select("MenuCode='BOM.CHANGE.PREVIEW'").Single();
            St.Eq("frmBomBulkPreview", Fmt.Str(row["FormClass"]), "dedicated native preview screen");
            St.Eq("BOM.VIEW", Fmt.Str(row["PermissionCode"]), "view-only permission");
            St.Eq(0, menus.Select("MenuCode='BOM.CHANGE'").Length, "approval desk remains hidden");
            Exception error = null;
            var thread = new Thread(() =>
            {
                try
                {
                    Context();
                    var assembly = Assembly.LoadFrom(Path.Combine(SterpConfig.BinDir, "STERP.exe"));
                    using (var main = (Form)Activator.CreateInstance(
                        assembly.GetType("STERP.Desktop.frmMain", true)))
                    {
                        main.Show(); Application.DoEvents();
                        var strip = main.Controls.OfType<MenuStrip>().Single();
                        var items = MenuItems(strip.Items).Where(x => x.Tag is DataRow).ToArray();
                        St.True(!items.Any(x => Fmt.Str(((DataRow)x.Tag)["MenuCode"]) == "BOM.CHANGE"),
                            "actual approval menu is absent for ENGINEER");
                        items.Single(x => Fmt.Str(((DataRow)x.Tag)["MenuCode"]) ==
                            "BOM.CHANGE.PREVIEW").PerformClick();
                        St.True(main.MdiChildren.Any(x => x.GetType().Name == "frmBomBulkPreview"),
                            "exact rendered menu opens native preview form");
                        main.Close();
                    }
                }
                catch (Exception ex) { error = ex; }
            });
            thread.SetApartmentState(ApartmentState.STA); thread.Start(); thread.Join();
            if (error != null) throw error;
        }

        [StTest("bom bulk gui: preview refusal and invalid replacement retain outcomes and no bills", Reqs = "BOM-018,BOM-024")]
        public static void NativeRefusals()
        {
            Context();
            string before = Snapshot();
            var service = new BomBulkService();
            DataTable rows = null;
            var r = service.Preview("ECO-BOMDEMO-001", "DEMO-PIN", "DEMO-PIN-HD",
                new DateTime(2026, 11, 1), "ITEM", "", true, ref rows);
            St.Eq("BOM1411", r.ReasonCode, r.Display);
            St.Eq(7, service.TargetCount, "all seven target bills");
            St.Eq(7, rows.Rows.Count, "refused result still renders every bill");
            St.True(service.FailedCount > 0 && !service.MayApply, "whole-run refusal");
            St.True(rows.Rows.Cast<DataRow>().Any(x => Fmt.Str(x["Detail"]).IndexOf("alternate",
                StringComparison.OrdinalIgnoreCase) >= 0), "native alternate-group detail");
            St.Eq(0, service.AppliedCount, "nothing applied");
            St.Eq(before, Snapshot(), "preview leaves bill headers and components unchanged");
            r = service.Preview("QA-BOM-INVALID", "DEMO-PIN", "DEMO-NOT-AN-ITEM",
                new DateTime(2026, 11, 1), "BOMREF", "BOM-DEMO-FRAME", true, ref rows);
            St.Eq("BOM1411", r.ReasonCode, r.Display);
            St.Eq(1, rows.Rows.Count, "invalid item returns its target refusal");
            St.Eq("FAILED", Fmt.Str(rows.Rows[0]["Outcome"]), "native failed target");
            St.Eq(before, Snapshot(), "invalid replacement leaves bills unchanged");
            r = service.Preview("QA-BOM-VALID", "DEMO-PIN", "DEMO-PIN-HD",
                new DateTime(2026, 11, 1), "BOMREF", "BOM-DEMO-FRAME", true, ref rows);
            St.True(r.Succeeded && service.MayApply, r.Display);
            St.Eq(1, rows.Rows.Count, "valid named bill has one pending proposal");
            St.Eq("PENDING", Fmt.Str(rows.Rows[0]["Outcome"]), "valid preview is not applied");
            St.Eq("NEWREV", Fmt.Str(rows.Rows[0]["Treatment"]), "released original gets proposed revision");
            St.Eq(before, Snapshot(), "valid preview also leaves bills unchanged");
        }

        [StTest("bom bulk gui: native authority refuses direct preview and engineer apply", Reqs = "COM-002,BOM-024")]
        public static void NativeAuthority()
        {
            Context("GLCLERK");
            var ca = CommArea.For("BOMBULKCA");
            ca.SetString("BB-CHANGE-REF", "QA-BOM-NOAUTH");
            var result = CobolGateway.Link("BOMBULK", ca, "PREVIEW");
            St.Eq(6, result.ReturnCode, result.Display);
            Context();
            result = CobolGateway.Link("BOMBULK", ca, "APPLY");
            St.Eq(6, result.ReturnCode, "engineer is not a change approver");
            St.Eq(0, Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT COUNT(*) FROM bom.BomChange WHERE ChangeRef='QA-BOM-NOAUTH'")), "no rejected request evidence");
        }

        [StTest("bom bulk gui: shown screen F10 renders native refusal F5 clears stale results", Reqs = "BOM-024,COM-024")]
        public static void FormKeys()
        {
            Exception error = null;
            var thread = new Thread(() =>
            {
                try
                {
                    Context();
                    var assembly = Assembly.LoadFrom(Path.Combine(SterpConfig.BinDir, "STERP.exe"));
                    var type = assembly.GetType("STERP.Desktop.frmBomBulkPreview", true);
                    using (var form = (Form)Activator.CreateInstance(type))
                    {
                        form.Show(); Application.DoEvents();
                        Action<string, string> set = (name, value) =>
                            form.Controls.Find(name, true).Single().Text = value;
                        set("txtChangeRef", "ECO-BOMDEMO-001");
                        set("txtOldItem", "DEMO-PIN");
                        set("txtNewItem", "DEMO-PIN-HD");
                        set("txtEffectiveDate", "01/11/2026");
                        type.GetMethod("OnConfirm", BindingFlags.Instance | BindingFlags.NonPublic).Invoke(form, null);
                        var grid = (DataGridView)form.Controls.Find("grdBulkPreview", true).Single();
                        St.Eq(7, ((DataTable)grid.DataSource).Rows.Count, "seven rendered outcomes");
                        St.True(form.Controls.Find("txtPreviewAnswer", true).Single().Text.Contains("BOM1411"),
                            "complete native refusal visible");
                        type.GetMethod("OnRefreshKey", BindingFlags.Instance | BindingFlags.NonPublic).Invoke(form, null);
                        St.Eq("", form.Controls.Find("txtPreviewAnswer", true).Single().Text, "F5 clears refusal");
                        St.True(grid.DataSource == null, "F5 clears stale grid");
                        form.Close();
                    }
                }
                catch (Exception ex) { error = ex; }
            });
            thread.SetApartmentState(ApartmentState.STA); thread.Start(); thread.Join();
            if (error != null) throw error;
        }
    }
}
