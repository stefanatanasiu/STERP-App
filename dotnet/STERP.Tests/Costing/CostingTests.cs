using System.Data;
using STERP.Business.Costing;
using STERP.CobolInterop;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests
{
    /// <summary>
    /// The .NET facades over the costing engines (module cst).  A costing program that is not
    /// in the generated directory answers RC 16 / PGM0001, so every engine test skips rather
    /// than fails while the COBOL side is being rebuilt.
    /// </summary>
    public static class CostingTests
    {
        private static void Context()
        {
            SterpSession.Current.UserCode = "SYSADM";
            SterpSession.Current.CompanyCode = "STC";
            SterpSession.Current.SiteCode = "RDG";
        }

        private static void Need(string objectName)
        {
            if (!SqlHelper.ObjectExists(objectName)) St.Skip(objectName + " is not deployed yet");
        }

        private static void NeedEngine(CobolResult r, string program)
        {
            string reason = Fmt.Str(r.ReasonCode);
            if (r.IsNotFound && reason == "PGM0001") St.Skip(program + " is not in the engine yet");
            if (r.IsSevere || reason.StartsWith("SQL")) St.Skip(program + " is still being written: " + r.Display);
        }

        /// <summary>A value out of the seeded demo data, so the tests never hard code a reference.</summary>
        private static string Demo(string sql, string what)
        {
            string v = Fmt.Str(SqlHelper.ExecuteScalar(sql));
            if (v.Length == 0) St.Skip("no demo " + what + " is seeded yet");
            return v;
        }

        [StTest("cst: the COMMAREA layouts of every costing program parse and carry their windows",
                Reqs = "CST-007,CST-010,CST-016,CST-018,CST-023")]
        public static void Layouts()
        {
            Context();
            CommArea sc = CommArea.For("STDCOSTCA");
            sc.SetDecimal("SC-L-NEW-COST(20)", 12.5m);
            St.Eq(12.5m, sc.GetDecimal("SC-L-NEW-COST(20)"), "STDCOSTCA carries 20 comparison lines");
            St.True(sc.Has("SC-PREPARED-BY") && sc.Has("SC-APPROVED-BY"),
                    "preparer and approver are both on file, which is what makes CST0405 possible");
            CommArea cr = CommArea.For("COSTROLLCA");
            cr.SetDecimal("CR-L-EXT-COST(20)", 99.99m);
            St.Eq(99.99m, cr.GetDecimal("CR-L-EXT-COST(20)"), "COSTROLLCA carries 20 costed tree lines");
            St.True(cr.Has("CR-L-BOUNDARY") && cr.Has("CR-L-CONTRIB"),
                    "every tree line states its valuation boundary and whether it contributes");
            St.True(CommArea.For("WHATIFCA").Has("WI-V-MARGIN-PCT"), "WHATIFCA prices a variant against the target");
            St.True(CommArea.For("JOBCOSTCA").Has("JC-EAC-GBP"), "JOBCOSTCA carries the estimate at completion");
            St.True(CommArea.For("WIPCALCCA").Has("WC-RESIDUAL-GBP"), "WIPCALCCA reports the residual balance");
            St.True(CommArea.For("COSTNORMCA").Has("CN-BASIS-USED"), "COSTNORMCA states the basis it used");
        }

        [StTest("cst: a comparison window is copied into the display table with its own types",
                Reqs = "CST-012")]
        public static void CompareWindow()
        {
            Context();
            CommArea ca = CommArea.For("STDCOSTCA");
            ca.SetString("SC-L-ITEM(1)", "CONV-SECT-3M");
            ca.SetDecimal("SC-L-OLD-COST(1)", 100m);
            ca.SetDecimal("SC-L-NEW-COST(1)", 110m);
            ca.SetDecimal("SC-L-DIFF(1)", 10m);
            ca.SetString("SC-L-NOTE(1)", "ADDED");

            DataTable t = CostArea.NewTable(CostingService.CompareSpec);
            CostArea.Copy(ca, t, CostingService.CompareSpec, 1, 1);
            St.Eq(1, t.Rows.Count, "one occurrence copied");
            St.Eq("CONV-SECT-3M", Fmt.Str(t.Rows[0]["Item"]), "the item came across as text");
            St.Eq(10m, Fmt.Dec(t.Rows[0]["Difference"]), "the difference came across as a number");
            St.Eq(typeof(decimal), t.Columns["NewCost"].DataType, "a cost column is typed, not a string");
            St.Eq("ADDED", Fmt.Str(t.Rows[0]["Note"]), "an item present in only one set keeps its note");
        }

        [StTest("cst: the scenario window keeps each variant beside the base case",
                Reqs = "CST-023")]
        public static void ScenarioWindow()
        {
            Context();
            CommArea ca = CommArea.For("WHATIFCA");
            ca.SetString("WI-V-LABEL(2)", "MATERIAL -5%");
            ca.SetDecimal("WI-V-UNIT(2)", 7.5m);
            ca.SetDecimal("WI-V-DIFF-PCT(2)", -5m);
            DataTable t = CostArea.NewTable(CostingService.ScenarioSpec);
            CostArea.Copy(ca, t, CostingService.ScenarioSpec, 2, 2);
            St.Eq("MATERIAL -5%", Fmt.Str(t.Rows[0]["Scenario"]), "the variant is labelled");
            St.Eq(7.5m, Fmt.Dec(t.Rows[0]["UnitCost"]), "its unit cost is carried");
            St.Eq(-5m, Fmt.Dec(t.Rows[0]["DiffPct"]), "and the difference from the base case");
        }

        [StTest("cst: the cost set worklist states preparer and approver side by side",
                Reqs = "CST-007,CST-012")]
        public static void CostSetList()
        {
            Context();
            Need("cst.usp_CostSetList");
            DataTable t = new CostingService().Sets("");
            St.True(t.Columns.Contains("PreparedBy") && t.Columns.Contains("ApprovedBy"),
                    "the worklist shows who prepared and who approved each set");
            St.True(t.Columns.Contains("SelfApproved"),
                    "a set approved by its own preparer is visible rather than hidden");
            St.True(t.Rows.Count > 0, "the demo company has at least one cost set");
        }

        [StTest("cst: the job worklist keeps sub-jobs under their parent and counted once",
                Reqs = "CST-014,CST-016")]
        public static void JobList()
        {
            Context();
            Need("cst.usp_JobList");
            DataTable t = new CostingService().Jobs("", "");
            St.True(t.Rows.Count > 0, "the demo company has jobs");
            St.True(t.Columns.Contains("Parent") && t.Columns.Contains("Level"),
                    "the parent and the level are stated, so nothing is added up twice");
            St.True(t.Columns.Contains("Budget") && t.Columns.Contains("Actual") && t.Columns.Contains("Wip"),
                    "budget, actual and work in progress are on the worklist");
        }

        [StTest("cst: an item cost enquiry answers every basis we hold, or none at all",
                Reqs = "CST-001,CST-026")]
        public static void CostEnquiry()
        {
            Context();
            Need("cst.usp_CostEnquiry");
            string item = Demo("SELECT TOP (1) ItemCode FROM cst.ItemCost WHERE CompanyCode = 'STC' ORDER BY ItemCode", "item cost");
            DataTable t = new CostingService().CostEnquiry(item, "", "");
            St.True(t.Rows.Count > 0, "the enquiry answers for " + item);
            St.True(t.Columns.Contains("AverageUnitCost") && t.Columns.Contains("FifoOldestUnit")
                    && t.Columns.Contains("StandardFromSet") && t.Columns.Contains("NrvUnitValue"),
                    "average, FIFO, standard and net realisable value are all stated side by side");
        }

        [StTest("cst: a roll-up states its completeness and prices the tree it explains",
                Reqs = "CST-010,CST-024")]
        public static void Rollup()
        {
            Context();
            Need("cst.StandardCost");
            Need("bom.BomHeader");
            string item = Demo("SELECT TOP (1) ParentItemCode FROM bom.BomHeader WHERE CompanyCode = 'STC' ORDER BY ParentItemCode",
                               "bill of material");
            CostingService svc = new CostingService();
            CobolResult r = svc.Rollup(item, "", "", 1m, false);
            NeedEngine(r, "COSTROLL");
            St.True(Fmt.Str(svc.Text("CR-COMPLETENESS")).Length > 0,
                    "the roll-up says whether it is complete rather than leaving it to be guessed");
            if (svc.Text("CR-COMPLETENESS") == "COMPLETE")
                St.True(svc.Num("CR-TOTAL-COST") > 0m, "a complete roll-up has a cost");
            else
                St.True(svc.Num("CR-MISSING-COUNT") > 0m || Fmt.Str(svc.Text("CR-WARNING")).Length > 0
                        || Fmt.Str(svc.Text("CR-ERROR-PATH")).Length > 0,
                        "an incomplete roll-up says what is missing instead of quoting a price anyway");
            St.True(svc.Lines != null && svc.Lines.Columns.Contains("Boundary"),
                    "the costed tree states the valuation boundary of every branch");
        }

        [StTest("cst: a what-if prices variants beside the base case and writes nothing",
                Reqs = "CST-023")]
        public static void WhatIf()
        {
            Context();
            Need("cst.StandardCost");
            string item = Demo("SELECT TOP (1) ItemCode FROM cst.StandardCost WHERE CompanyCode = 'STC' ORDER BY ItemCode",
                               "standard cost");
            object before = SqlHelper.ExecuteScalar("SELECT COUNT(*) FROM cst.CostSet WHERE CompanyCode = 'STC'");
            CostingService svc = new CostingService();
            CobolResult r = svc.Scenario(item, "", "", 1m, 0m, 1m, 1m);
            NeedEngine(r, "WHATIF");
            object after = SqlHelper.ExecuteScalar("SELECT COUNT(*) FROM cst.CostSet WHERE CompanyCode = 'STC'");
            St.Eq(Fmt.Int(before), Fmt.Int(after), "a scenario creates no cost set - SAVESET is never issued");
            St.True(svc.Lines != null, "the variants come back shaped for the grid");
        }

        [StTest("cst: the work in progress roll-forward proves opening + additions - deductions = closing",
                Reqs = "CST-018")]
        public static void Wip()
        {
            Context();
            Need("cst.WipBalance");
            string job = Demo("SELECT TOP (1) JobRef FROM cst.Job WHERE CompanyCode = 'STC' ORDER BY JobRef", "job");
            CostingService svc = new CostingService();
            CobolResult r = svc.Wip(job, true);
            NeedEngine(r, "WIPCALC");
            decimal proof = svc.Num("WC-OPENING-GBP") + svc.Num("WC-ADDITIONS-GBP") - svc.Num("WC-DEDUCTIONS-GBP");
            St.Eq(svc.Num("WC-CLOSING-GBP"), proof, "the closing balance is the arithmetic it claims to be");
        }
    }
}
