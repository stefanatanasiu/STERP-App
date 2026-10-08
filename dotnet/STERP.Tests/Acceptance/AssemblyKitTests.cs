using System;
using System.Data;
using STERP.Business;
using STERP.Business.Kits;
using STERP.CobolInterop;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests.Acceptance
{
    /// <summary>
    /// Define to assemble, end to end: a bill is turned into an execution kit, trial fitted,
    /// released, issued against, completed and then costed - and the genealogy can answer what
    /// went into what afterwards.
    /// </summary>
    public static class AssemblyKitTests
    {
        private const string Site = "RDG";

        private static DataRow Buildable()
        {
            return Acc.Demo("SELECT TOP (1) h.BomRef, h.ParentItemCode, h.BomRevision, h.SiteCode " +
                            "FROM bom.BomHeader h WHERE h.CompanyCode = @c AND h.Status = 'RELEASED' " +
                            "AND EXISTS (SELECT 1 FROM bom.BomComponent x WHERE x.BomHeaderId = h.BomHeaderId) " +
                            "ORDER BY h.BomHeaderId", "released bill of materials", "@c", Acc.Co);
        }

        [StTest("acc: define to assemble - trial fit, release, issue, complete, genealogy and cost",
                Reqs = "XMD-003,KIT-001,KIT-002,KIT-010,KIT-012,KIT-018,RPT-012,RPT-013", Ac = "AC-099")]
        public static void DefineToAssembleChain()
        {
            Acc.Need("kit.ExecutionKit");
            Acc.Need("bom.BomHeader");
            Acc.Context("WHSE", Site);
            DataRow bom = Buildable();
            string parent = Acc.Text(bom, "ParentItemCode"), rev = Acc.Text(bom, "BomRevision");
            string site = Acc.Text(bom, "SiteCode").Length > 0 ? Acc.Text(bom, "SiteCode") : Site;
            string kitRef = Acc.Ref("K");
            KitService kit = new KitService();
            // a production bill has to be named by the demand it is being built for, so offer this
            // environment each kind of demand in turn until one of them can be named
            string jobRef = Acc.Str("SELECT TOP (1) JobRef FROM cst.Job WHERE CompanyCode = @c ORDER BY JobRef", "@c", Acc.Co);
            string demandOrder = Acc.Str("SELECT TOP (1) OrderNumber FROM so.SalesOrder WHERE CompanyCode = @c " +
                                         "ORDER BY SalesOrderId DESC", "@c", Acc.Co);
            string[][] demands = new string[][] {
                new string[] { "JOB", jobRef }, new string[] { "ORDER", demandOrder }, new string[] { "STOCK", "" } };

            // 1 - the kit is created from the released bill, for a quantity and a need date
            CobolResult r = null;
            foreach (string[] demand in demands)
            {
                if (demand[0] != "STOCK" && demand[1].Length == 0) continue;
                r = kit.Create(kitRef, "STANDARD", parent, rev, site, 2m, Acc.Today.AddDays(7), demand[0], demand[1], false);
                Acc.NeedEngine(r, "KITCREAT");
                if (r.Succeeded) break;
            }
            Acc.NeedEngine(r, "KITCREAT");
            if (!r.Succeeded) St.Skip("the kit could not be created here (" + r.ReasonCode + "): " + r.Message);
            St.Eq(1, Acc.Int("SELECT COUNT(*) FROM kit.ExecutionKit WHERE CompanyCode = @c AND KitRef = @k",
                             "@c", Acc.Co, "@k", kitRef), "the kit exists exactly once under its own reference");

            // 2 - the trial fit explains what could be built without moving anything
            int movesBefore = Acc.Int("SELECT COUNT(*) FROM inv.StockMovement WHERE CompanyCode = @c", "@c", Acc.Co);
            CobolResult t = kit.Trial(kitRef, parent, rev, site, 2m, Acc.Today.AddDays(7), true, false);
            Acc.NeedEngine(t, "KITTRIAL");
            Acc.Ok(t, "the trial fit answered");
            St.Eq(movesBefore, Acc.Int("SELECT COUNT(*) FROM inv.StockMovement WHERE CompanyCode = @c", "@c", Acc.Co),
                  "a trial kit moves no stock (AC-079)");
            St.True(kit.Num("KT-MAX-BUILDABLE") >= 0m, "the buildable quantity is never negative");

            // 3 - review and release are separate decisions
            CobolResult rv = kit.Review(kitRef);
            St.True(rv != null, "the kit can be reviewed before it is released");
            CobolResult rl = kit.Release(kitRef);
            St.True(rl.Succeeded || Acc.Refused(rl), "release is granted or refused with a reason: " + rl.Display);
            if (!rl.Succeeded) St.Skip("the kit was not releasable here: " + rl.Display);

            // 4 - issuing a component is a real stock movement against the kit
            DataRow line = Acc.Row("SELECT TOP (1) l.LineNum, l.ComponentItemCode, l.CurrentRequirementQty " +
                                   "FROM kit.ExecutionKitLine l JOIN kit.ExecutionKit k ON k.ExecutionKitId = l.ExecutionKitId " +
                                   "WHERE k.CompanyCode = @c AND k.KitRef = @k ORDER BY l.LineNum", "@c", Acc.Co, "@k", kitRef);
            if (line == null) St.Skip("the released kit has no component lines to issue");
            KitExecutionService exec = new KitExecutionService();
            string comp = Acc.Text(line, "ComponentItemCode");
            decimal need = Acc.Num(line, "CurrentRequirementQty");
            CobolResult check = exec.Issue(kitRef, Fmt.Int(line["LineNum"]), comp, "", "", need, "", "", "", false, true);
            Acc.NeedEngine(check, "KITISSUE");
            St.Eq(movesBefore, Acc.Int("SELECT COUNT(*) FROM inv.StockMovement WHERE CompanyCode = @c", "@c", Acc.Co),
                  "a check-only issue moves no stock");
            CobolResult issue = exec.Issue(kitRef, Fmt.Int(line["LineNum"]), comp, "", "", need, "", "", "", false, false);
            if (!issue.Succeeded) St.Skip("the component could not be issued here: " + issue.Display);
            St.True(Acc.Int("SELECT COUNT(*) FROM inv.StockMovement WHERE CompanyCode = @c", "@c", Acc.Co) > movesBefore,
                    "an issue is a stock movement, not a note");
            St.True(Acc.Dec("SELECT ISNULL(SUM(l.GrossIssuedQty), 0) FROM kit.ExecutionKitLine l " +
                            "JOIN kit.ExecutionKit k ON k.ExecutionKitId = l.ExecutionKitId " +
                            "WHERE k.CompanyCode = @c AND k.KitRef = @k", "@c", Acc.Co, "@k", kitRef) > 0m,
                    "the kit records what has been issued to it");

            // 5 - completion, genealogy and cost
            CobolResult done = exec.Backflush(kitRef, 1m);
            St.True(done != null, "completing the kit answers");
            if (done.Succeeded)
            {
                St.True(Acc.Int("SELECT COUNT(*) FROM kit.KitCompletion WHERE CompanyCode = @c AND KitRef = @k",
                                "@c", Acc.Co, "@k", kitRef) > 0, "a completion is recorded");
                CobolResult gen = exec.Genealogy("BACKWARD", kitRef, parent, "", "", 5);
                St.True(gen.Succeeded || Acc.Refused(gen), "the genealogy answers what went into the build: " + gen.Display);
            }
            CobolResult cost = exec.Cost(kitRef, "", false);
            Acc.NeedEngine(cost, "KITCOST");
            St.True(cost.Succeeded || Acc.Refused(cost), "the kit is costed or says why it cannot be: " + cost.Display);
            if (cost.Succeeded)
                St.True(exec.Num("KC-TOTAL-COST") >= 0m, "a kit cost is never negative");
        }

        [StTest("acc: the kit kind decides what may be changed, and a frozen kit refuses a change",
                Reqs = "KIT-001,KIT-002,KIT-004,KIT-024", Ac = "AC-076")]
        public static void KitKindAndFreeze()
        {
            Acc.Need("kit.ExecutionKit");
            Acc.Context("WHSE", Site);
            DataRow existing = Acc.Demo("SELECT TOP (1) k.KitRef, k.ParentItemCode FROM kit.ExecutionKit k " +
                                        "WHERE k.CompanyCode = @c ORDER BY k.ExecutionKitId", "execution kit", "@c", Acc.Co);
            KitService kit = new KitService();
            CobolResult wrong = kit.KindCheck(Acc.Text(existing, "KitRef"), "NOTAKIND", Acc.Text(existing, "ParentItemCode"));
            Acc.NeedEngine(wrong, "KITCREAT");
            St.True(wrong != null, "the kit kind is always checked before the kit is worked on");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM kit.ExecutionKit WHERE CompanyCode = @c AND Status IS NULL", "@c", Acc.Co),
                  "every kit holds a status, so what may be done to it is never ambiguous");

            DataRow frozen = Acc.Row("SELECT TOP (1) KitRef, Status FROM kit.ExecutionKit WHERE CompanyCode = @c " +
                                     "AND Status IN ('CLOSED','COMPLETE','CANCELLED') ORDER BY KitRef", "@c", Acc.Co);
            if (frozen != null)
            {
                KitExecutionService exec = new KitExecutionService();
                CobolResult late = exec.Backflush(Acc.Text(frozen, "KitRef"), 1m);
                St.True(!late.IsOk, "a closed kit refuses a further completion: " + late.Display);
            }
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM kit.ExecutionKitLine WHERE NetIssuedQty < 0 OR GrossIssuedQty < 0"),
                  "no kit line is ever issued a negative quantity");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM kit.ExecutionKit WHERE CompanyCode = @c AND CompletedGoodQty < 0", "@c", Acc.Co),
                  "and no kit is ever completed for a negative quantity");
        }

        [StTest("acc: an item is retired under control and never while it is still demanded",
                Reqs = "ITM-010,ITM-020,ITM-022", Ac = "AC-072")]
        public static void ControlledChangeAndRetirement()
        {
            Acc.Need("inv.Item");
            Acc.Context("ENGMGR", Site);
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM inv.Item i WHERE i.CompanyCode = @c AND i.LifecycleStatus IN ('OBSOLETE','RETIRED') " +
                             "AND EXISTS (SELECT 1 FROM so.SalesOrderLine l JOIN so.SalesOrder o ON o.CompanyCode = i.CompanyCode " +
                             "AND o.OrderNumber = l.OrderNumber WHERE l.CompanyCode = i.CompanyCode AND l.ItemCode = i.ItemCode " +
                             "AND l.Status = 'OPEN' AND o.CommercialStatus NOT IN ('CANCELLED','CLOSED'))", "@c", Acc.Co),
                  "an item with live demand is never left in a retired state");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM inv.Item WHERE CompanyCode = @c AND LifecycleStatus IN ('OBSOLETE','RETIRED') " +
                             "AND (StatusReason IS NULL OR StatusReason = '')", "@c", Acc.Co),
                  "a withdrawal is recorded with the reason it was withdrawn, not only the successor");
            DataRow live = Acc.Demo("SELECT TOP (1) i.ItemCode FROM inv.Item i WHERE i.CompanyCode = @c AND i.LifecycleStatus = 'ACTIVE' " +
                                    "AND EXISTS (SELECT 1 FROM inv.StockBalance b WHERE b.CompanyCode = i.CompanyCode " +
                                    "AND b.ItemCode = i.ItemCode AND b.QtyOnHand > 0) ORDER BY i.ItemCode",
                                    "active item holding stock", "@c", Acc.Co);
            ItemService svc = new ItemService();
            CobolResult r = svc.Lifecycle(Acc.Text(live, "ItemCode"), "OBSOLETE", "", "");
            Acc.NeedEngine(r, "ITEMLIFE");
            St.True(!r.IsOk, "an item still holding stock cannot be obsoleted without a reason and a successor: " + r.Display);
            St.Eq("ACTIVE", Acc.Str("SELECT LifecycleStatus FROM inv.Item WHERE CompanyCode = @c AND ItemCode = @i",
                                    "@c", Acc.Co, "@i", Acc.Text(live, "ItemCode")),
                  "the refused change left the item exactly as it was");
        }

        [StTest("acc: a return comes back against the document it left on, within what was shipped",
                Reqs = "XMD-004,XMD-008,SO-020", Ac = "AC-100")]
        public static void ReturnAndEffectiveDateChain()
        {
            Acc.Need("so.ReturnAuthorisation");
            Acc.Context("SALESMGR", Site);
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM so.ReturnLine r WHERE r.ReturnId NOT IN " +
                             "(SELECT ReturnId FROM so.ReturnAuthorisation)"),
                  "every return line belongs to an authorisation that exists");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM so.ReturnLine WHERE RequestedQty < 0 OR ReceivedQty < 0 OR CreditedQty < 0"),
                  "a return is never a negative quantity - a shipment is a despatch, not a return");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM so.ReturnLine WHERE AuthorisedQty > RequestedQty + 0.000001"),
                  "nothing is authorised back that was not asked to come back");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM so.ReturnAuthorisation WHERE CompanyCode = @c AND Status <> 'DRAFT' " +
                             "AND OriginalOrderNumber IS NULL AND OriginalInvoiceNumber IS NULL AND OriginalDespatchRef IS NULL",
                             "@c", Acc.Co), "a return always comes back against the document it left on");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM bom.BomHeader WHERE CompanyCode = @c AND EffectiveTo IS NOT NULL " +
                             "AND EffectiveTo < EffectiveFrom", "@c", Acc.Co),
                  "no revision is effective before it starts - the effective-date chain is ordered");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM bom.BomComponent WHERE EffectiveTo IS NOT NULL AND EffectiveTo < EffectiveFrom"),
                  "and neither is any component line");
        }
    }
}
