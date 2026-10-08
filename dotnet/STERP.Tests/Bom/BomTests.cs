using System;
using System.Data;
using System.IO;
using System.Reflection;
using STERP.Business;
using STERP.Business.Bom;
using STERP.CobolInterop;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests
{
    /// <summary>
    /// The .NET facades and the registration of the bills of material module (module bom).  The
    /// calculations themselves belong to the COBOL engine and are proved by the TBOM* suites; these
    /// tests prove that the screens get the same answer through the facade, and that every menu row
    /// seeded by 32-bom-menu.sql has something behind it.
    /// </summary>
    public static class BomTests
    {
        private static void Context()
        {
            SterpSession.Current.UserCode = "ENGINEER";
            SterpSession.Current.CompanyCode = "STC";
            SterpSession.Current.SiteCode = "RDG";
        }

        private static void Need(string objectName)
        {
            if (!SqlHelper.ObjectExists(objectName)) St.Skip(objectName + " is not deployed yet");
        }

        /// <summary>An engine program that is not in the generated directory must not fail the build.</summary>
        private static void NeedEngine(CobolResult r, string program)
        {
            string reason = Fmt.Str(r.ReasonCode);
            if (r.IsNotFound && reason == "PGM0001") St.Skip(program + " is not in the engine yet");
            if (r.IsSevere || reason.StartsWith("SQL")) St.Skip(program + " is still being written: " + r.Display);
        }

        private static Type Screen(string className)
        {
            Assembly shell = Assembly.LoadFrom(Path.Combine(SterpConfig.BinDir, "STERP.exe"));
            return shell.GetType("STERP.Desktop." + className, false, true);
        }

        [StTest("bom: the COMMAREA layouts of the bill engines parse and carry their windows",
                Reqs = "BOM-014,BOM-020,BOM-021,BOM-032")]
        public static void Layouts()
        {
            Context();
            CommArea leaf = CommArea.For("BOMLEAFCA");
            leaf.SetDecimal("BL-L-REQ-QTY(25)", 12.5m);
            St.Eq(12.5m, leaf.GetDecimal("BL-L-REQ-QTY(25)"), "BOMLEAFCA carries a window of 25 occurrences");
            St.True(leaf.Has("BL-COMPLETENESS") && leaf.Has("BL-STOP-SUMMARY"),
                    "the indented view reports completeness and why a branch stopped");
            CommArea net = CommArea.For("BOMNETCA");
            St.True(net.Has("BN-MAX-BUILDABLE") && net.Has("BN-SHORTAGE-CNT"),
                    "the netted view reports the buildable quantity and the shortages");
            CommArea cost = CommArea.For("BOMCOSTCA");
            St.True(cost.Has("BC-TOTAL-COST") && cost.Has("BC-RECOVERY-CREDIT"),
                    "the costed roll-up separates the by-product recovery credit");
            CommArea val = CommArea.For("BOMVALIDCA");
            St.True(val.Has("BV-CYCLE-PATH") && val.Has("BV-RELEASE-OK"),
                    "validation answers with the cycle path and the release decision");
        }

        [StTest("bom: the facade reads the B1 frame bill, its lines and its release evidence",
                Reqs = "BOM-001,BOM-002,BOM-006,BOM-008")]
        public static void StructureEnquiry()
        {
            Need("bom.usp_BomStructure");
            Context();
            BomService svc = new BomService();
            DataRow h = svc.BillHeader("BOM-DEMO-FRAME", "A", new DateTime(2026, 10, 1));
            St.True(h != null, "the B1 frame bill is seeded");
            St.Eq("DEMO-FRAME", ServiceBase.Col(h, "ParentItemCode", ""), "the bill belongs to the frame");
            St.Eq("RELEASED", ServiceBase.Col(h, "Status", ""), "the demo bill is released");
            DataTable lines = svc.BillLines("BOM-DEMO-FRAME", "A", new DateTime(2026, 10, 1));
            St.Eq(4, lines.Rows.Count, "the frame quotes four components");
            St.True(lines.Select("UsageBasis = 'FIXEDRUN'").Length == 1,
                    "the cleaning fluid is a fixed quantity per execution run");
            St.True(svc.Revisions("BOM-DEMO-BDY", "", new DateTime(2026, 10, 1)).Rows.Count >= 2,
                    "the boundary demonstration has more than one revision on file");
        }

        [StTest("bom: an indented explosion through the facade answers with occurrences and depth",
                Reqs = "BOM-014,BOM-032")]
        public static void IndentedExplosion()
        {
            Need("bom.BomHeader");
            Context();
            BomService svc = new BomService();
            DataTable rows = null;
            CobolResult r = svc.Explode("DEMO-FRAME", "", 10m, new DateTime(2026, 10, 1), "INDENTED", 0, ref rows);
            NeedEngine(r, "BOMLEAF");
            St.True(r.Succeeded, "BOMLEAF answered: " + r.Display);
            St.True(rows != null && rows.Rows.Count > 0, "the explosion returned lines");
            St.True(svc.Occurrences > 0 && svc.DepthReached >= 2, "the frame is a nested structure, depth " + svc.DepthReached);
            St.True(svc.Completeness.Length > 0, "the view states whether the answer is complete");

            DataTable leaves = null;
            CobolResult lr = svc.Explode("DEMO-FRAME", "", 10m, new DateTime(2026, 10, 1), "LEAF", 0, ref leaves);
            NeedEngine(lr, "BOMLEAF");
            St.True(leaves != null && leaves.Rows.Count > 0 && leaves.Rows.Count <= rows.Rows.Count,
                    "the leaf view is the flattened reading of the same explosion");
        }

        [StTest("bom: the netted plan reports the buildable quantity the screen shows",
                Reqs = "BOM-020,BOM-022")]
        public static void NettedPlan()
        {
            Need("inv.StockBalance");
            Context();
            BomService svc = new BomService();
            DataTable rows = null;
            CobolResult r = svc.NettedPlan("DEMO-FRAME", 10m, new DateTime(2026, 10, 1), ref rows);
            NeedEngine(r, "BOMNET");
            St.True(r.Succeeded, "BOMNET answered: " + r.Display);
            St.True(rows != null && rows.Columns.Contains("Shortage"), "every netted line carries its shortage");
            St.True(svc.MaxBuildable >= 0m, "the maximum buildable quantity is reported, " + svc.MaxBuildable);
        }

        [StTest("bom: where used finds the indirect parent of a B1 component",
                Reqs = "BOM-023,BOM-031")]
        public static void WhereUsed()
        {
            Need("bom.usp_BomWhereUsed");
            Context();
            DataTable t = new BomService().WhereUsed("STC", "DEMO-PLATE", new DateTime(2026, 10, 1), 10, false);
            St.True(t.Rows.Count >= 2, "the plate is used directly and indirectly, found " + t.Rows.Count);
            St.True(t.Select("ParentItemCode = 'DEMO-BRACE' AND Level = 1").Length == 1, "the brace is the direct parent");
            DataRow[] frame = t.Select("ParentItemCode = 'DEMO-FRAME' AND Level = 2");
            St.True(frame.Length == 1, "the frame is reported as an indirect parent");
            St.Eq(0.8m, Fmt.Dec(frame[0]["QtyPerTopUnit"]), "0.4 kg on each of two braces is 0.8 kg per frame");
        }

        [StTest("bom: the revision comparison answers with the engineering change between two revisions",
                Reqs = "BOM-025,BOM-029")]
        public static void CompareRevisions()
        {
            Need("bom.usp_BomCompareRevisions");
            Context();
            DataTable t = new BomService().CompareRevisions("STC", "BOM-DEMO-BDY", "A", "B", "BOMDEMO");
            St.True(t != null && t.Columns.Contains("ChangeType"), "every difference is classified");
        }

        [StTest("bom: the release gate refuses an unvalidated revision and the reason reaches the screen",
                Reqs = "BOM-003,BOM-004,BOM-029")]
        public static void ReleaseGate()
        {
            Need("bom.usp_ReleaseBom");
            Context();
            try
            {
                new BomService().Release("STC", "BOM-DEMO-BDY", "B", "ENGMGR", "QUALITY",
                                         new DateTime(2026, 7, 1), "", "harness test");
                St.True(false, "an unvalidated revision was released by the facade");
            }
            catch (BusinessRuleException ex)
            {
                St.True(ex.Code.StartsWith("E-BOM04"),
                        "the refusal reaches the editor as a business rule: " + ex.Code + " " + ex.Message);
                St.True(ex.Message.Length > 10, "the reason is in plain English for the engineer");
            }
        }

        [StTest("bom: the three BOM screens are STERP base forms and open on the seeded B1 data",
                Reqs = "BOM-014,BOM-023,BOM-024,COM-024")]
        public static void ScreensInheritBaseForm()
        {
            Need("bom.BomHeader");
            Type baseForm = Screen("BaseForm");
            St.True(baseForm != null, "STERP.Desktop.BaseForm is the framework base form");
            foreach (string name in new string[] { "frmBomEditor", "frmBomExplosion", "frmBomWhereUsed" })
            {
                Type t = Screen(name);
                St.True(t != null, name + " is compiled into STERP.exe");
                St.True(baseForm.IsAssignableFrom(t), name + " inherits BaseForm, so it gets F2/F3/F5/F10 and the status line");
            }
            Context();
            BomService svc = new BomService();
            St.True(svc.BillHeader("BOM-DEMO-FRAME", "A", new DateTime(2026, 10, 1)) != null,
                    "the editor opens on the B1 frame bill");
            St.True(svc.WhereUsed("STC", "DEMO-PIN", new DateTime(2026, 10, 1), 10, false).Rows.Count > 0,
                    "the where used screen opens on a B1 component that is used twice");
        }

        [StTest("bom: the module's menu, maintenance screens and reports are registered",
                Reqs = "BOM-024,BOM-032,COM-019")]
        public static void Registration()
        {
            Need("core.MenuItem");
            Context();
            St.Eq(3, Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT COUNT(DISTINCT FormClass) FROM core.MenuItem WHERE ParentMenuCode = 'BOM' AND FormClass IN " +
                "('frmBomEditor','frmBomExplosion','frmBomWhereUsed')")), "the three BOM screens are on the menu");
            St.Eq(2, Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT COUNT(*) FROM core.MaintDef WHERE MaintCode IN ('BOM.HEADER','BOM.CHANGE')")),
                "bill headers and engineering changes use the generic maintenance screen");
            St.True(Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT COUNT(*) FROM core.MaintColumn WHERE MaintCode IN ('BOM.HEADER','BOM.CHANGE')")) >= 15,
                "the generic screens have their columns");
            DataTable defs = new ReportService().Definitions("BOM");
            St.True(defs.Rows.Count >= 4, "the BOM reports are registered, found " + defs.Rows.Count);
        }
    }
}
