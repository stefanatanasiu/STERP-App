using System.Collections.Generic;
using System.Data;
using STERP.Business;
using STERP.CobolInterop;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests
{
    /// <summary>Item and stock service facades (module itm).</summary>
    public static class ItemsTests
    {
        private static void Context()
        {
            SterpSession.Current.UserCode = "WHSE";
            SterpSession.Current.CompanyCode = "STC";
            SterpSession.Current.SiteCode = "RDG";
        }

        private static void Need(string objectName)
        {
            if (!SqlHelper.ObjectExists(objectName)) St.Skip(objectName + " is not deployed yet");
        }

        [StTest("itm: the item master facade reads a seeded item and its tabs", Reqs = "ITM-001,ITM-005")]
        public static void ItemHeaderAndTabs()
        {
            Need("inv.Item");
            Context();
            ItemService svc = new ItemService();
            DataRow h = svc.Header("BOLT-M5X16-SS");
            St.True(h != null, "BOLT-M5X16-SS is seeded in STC");
            St.Eq("RELEASED", ServiceBase.Col(h, "LifecycleStatus", ""), "a seeded item is released");
            DataTable uom = svc.Part("UOM", "BOLT-M5X16-SS");
            St.True(uom.Rows.Count >= 1, "the item has at least its stock unit, found " + uom.Rows.Count);
            St.True(svc.Part("SUPPLY", "BOLT-M5X16-SS").Columns.Contains("ReorderPointQty"), "supply rules tab");
        }

        [StTest("itm: alias search finds an item by its partner part number", Reqs = "ITM-009,D-21")]
        public static void AliasSearch()
        {
            Need("inv.vw_ItemAlias");
            Context();
            DataTable t = new ItemService().Search("BOLT-M5");
            St.True(t != null && t.Rows.Count >= 1, "the search returns the bolt");
        }

        [StTest("itm: stock enquiry reports buckets by condition and ownership", Reqs = "ITM-012,ITM-023")]
        public static void StockEnquiry()
        {
            Need("inv.usp_StockEnquiry");
            Context();
            DataTable t = new StockService().Enquiry("", "RDG", "", false);
            St.True(t.Rows.Count > 0, "Reading holds seeded stock, found " + t.Rows.Count + " bucket(s)");
            St.True(t.Columns.Contains("IsEligible"), "every bucket is labelled eligible or not");
            St.True(t.Columns.Contains("QtyReserved"), "reserved quantity is reported separately");
        }

        [StTest("itm: availability answers with the six buckets", Reqs = "ITM-013")]
        public static void Availability()
        {
            Need("inv.StockBalance");
            Context();
            StockService svc = new StockService();
            CobolResult r = svc.Availability("BOLT-M5X16-SS", "RDG");
            if (r.IsNotFound) St.Skip("AVAILQ is not in the engine yet (" + r.ReasonCode + ")");
            St.True(r.Succeeded, "AVAILQ answered: " + r.Display);
            St.True(svc.Physical >= svc.Eligible, "eligible stock can never exceed physical stock");
            St.True(svc.FreeQty <= svc.Eligible, "free stock can never exceed eligible stock");
        }

        [StTest("itm: a movement of a demo receipt is on file with its source and reason", Reqs = "ITM-015,ITM-017")]
        public static void MovementHistory()
        {
            Need("inv.StockMovement");
            Context();
            DataTable t = new StockService().Movements("", "RDG", 400);
            St.True(t.Rows.Count > 0, "the demo movement history is seeded, found " + t.Rows.Count);
            St.True(t.Columns.Contains("SourceDocRef"), "every movement names its source document");
            St.True(t.Columns.Contains("ReasonCode"), "every movement carries a reason code");
        }

        [StTest("itm: a stock count with a variance and an approved adjustment is seeded", Reqs = "ITM-018")]
        public static void CountVariance()
        {
            Need("inv.StockCountLine");
            Context();
            StockService svc = new StockService();
            DataTable h = svc.Counts("ALL");
            St.True(h.Rows.Count >= 1, "at least one count exists");
            DataTable l = svc.CountLines("CNT-2026-001");
            St.True(l.Rows.Count >= 3, "the demo count has lines, found " + l.Rows.Count);
            bool variance = false;
            foreach (DataRow r in l.Rows) if (Fmt.Dec(r["VarianceQty"]) != 0m) variance = true;
            St.True(variance, "the demo count has at least one counted variance");
        }

        [StTest("itm: the module's menu, maintenance screens and reports are registered", Reqs = "ITM-024,COM-019")]
        public static void Registration()
        {
            Need("core.MenuItem");
            Context();
            SterpSession.Current.UserCode = "SYSADM";
            St.Eq(4, Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT COUNT(*) FROM core.MenuItem WHERE ParentMenuCode = 'ITM' AND FormClass IN " +
                "('frmItemMaster','frmStockEnquiry','frmStockMovement','frmStockCount')")), "the four ITM forms are on the menu");
            St.True(Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT COUNT(*) FROM core.MaintDef WHERE ModuleCode = 'itm'")) >= 10, "the simple ITM masters are generic");
            DataTable defs = new ReportService().Definitions("itm");
            St.True(defs.Rows.Count >= 6, "the ITM batch reports are registered, found " + defs.Rows.Count);
        }
    }
}
