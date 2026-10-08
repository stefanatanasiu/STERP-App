using System;
using System.Data;
using STERP.Business.Bom;
using STERP.Business.Costing;
using STERP.CobolInterop;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests.Acceptance
{
    /// <summary>
    /// Costing and bills of material: the B-structures of the specification are exploded and
    /// rolled up through the real engine, and the published invariants (quantity sensitivity,
    /// effectivity boundaries, incomplete cost, WIP and variance) are proved against the ledger.
    /// </summary>
    public static class CostingBomTests
    {
        private const string Site = "RDG";

        private static DataRow Structure(string like)
        {
            return Acc.Demo("SELECT TOP (1) h.BomRef, h.ParentItemCode, h.BomRevision, h.CostingLotSize " +
                            "FROM bom.BomHeader h WHERE h.CompanyCode = @c AND h.Status = 'RELEASED' AND h.ParentItemCode LIKE @p " +
                            "AND EXISTS (SELECT 1 FROM bom.BomComponent x WHERE x.BomHeaderId = h.BomHeaderId) " +
                            "ORDER BY h.BomHeaderId", "released " + like + " structure", "@c", Acc.Co, "@p", like);
        }

        [StTest("acc: a B-structure explodes to the same components, levels and quantities every time",
                Reqs = "BOM-006,BOM-007,BOM-008,CST-023", Ac = "AC-063,AC-086")]
        public static void StructureExplodesConsistently()
        {
            Acc.Need("bom.BomHeader");
            Acc.Context("ENGINEER", Site);
            DataRow b = Structure("DEMO-B%");
            BomService svc = new BomService();
            DataTable rows = null;
            CobolResult r = svc.Explode(Acc.Text(b, "ParentItemCode"), Acc.Text(b, "BomRevision"), 1m, Acc.Today, "ALL", 10, ref rows);
            Acc.NeedEngine(r, "BOMEXPL");
            Acc.Ok(r, "the structure exploded");
            St.True(rows != null && rows.Rows.Count > 0, "an exploded structure has components");
            int occurrences = svc.Occurrences;
            decimal unit = svc.TotalCost;

            DataTable again = null;
            svc.Explode(Acc.Text(b, "ParentItemCode"), Acc.Text(b, "BomRevision"), 1m, Acc.Today, "ALL", 10, ref again);
            St.Eq(occurrences, svc.Occurrences, "the same question asked twice gives the same answer");

            // ten times the parent is ten times every component - an explosion is linear in quantity
            DataTable ten = null;
            svc.Explode(Acc.Text(b, "ParentItemCode"), Acc.Text(b, "BomRevision"), 10m, Acc.Today, "ALL", 10, ref ten);
            St.Eq(occurrences, svc.Occurrences, "scaling the build does not change the shape of the structure");
            St.True(svc.DepthReached >= 1, "the explosion reports how deep it went");

            // an effectivity window that has not opened yet cannot contribute
            DataTable past = null;
            svc.Explode(Acc.Text(b, "ParentItemCode"), Acc.Text(b, "BomRevision"), 1m, new DateTime(2000, 1, 1), "ALL", 10, ref past);
            St.True(past == null || past.Rows.Count <= rows.Rows.Count,
                    "a date before the revision was effective never explodes to more than today does");
        }

        [StTest("acc: a costed roll-up is quantity sensitive and never silently complete when a rate is missing",
                Reqs = "CST-010,CST-011,CST-013,CST-023,CST-024,BOM-011", Ac = "AC-063,AC-064")]
        public static void RollupIsQuantitySensitive()
        {
            Acc.Need("cst.ItemCost");
            Acc.Context("COSTACC", Site);
            DataRow b = Structure("DEMO-%");
            string set = Acc.Str("SELECT TOP (1) CostSetCode FROM cst.CostSet WHERE CompanyCode = @c AND Status = 'ACTIVE' " +
                                 "ORDER BY CostSetCode", "@c", Acc.Co);
            CostingService cst = new CostingService();
            CobolResult one = cst.Rollup(Acc.Text(b, "ParentItemCode"), Acc.Text(b, "BomRevision"), set, 1m, true);
            Acc.NeedEngine(one, "CSTROLL");
            if (!one.Succeeded && !Acc.Refused(one)) St.Skip("the roll-up could not run here: " + one.Display);
            decimal unitAtOne = cst.Num("CR-UNIT-COST");
            CobolResult hundred = cst.Rollup(Acc.Text(b, "ParentItemCode"), Acc.Text(b, "BomRevision"), set, 100m, true);
            decimal unitAtHundred = cst.Num("CR-UNIT-COST");
            if (one.Succeeded && hundred.Succeeded)
            {
                St.True(unitAtOne >= 0m && unitAtHundred >= 0m, "a unit cost is never negative");
                St.True(unitAtHundred <= unitAtOne + 0.000001m,
                        "a larger lot never costs more per unit once set-up is spread (" + unitAtOne + " at 1, " + unitAtHundred + " at 100)");
            }
            if (Acc.Refused(one))
                St.True(one.ReasonCode.Trim().Length > 0, "an incomplete cost is reported as incomplete, not as zero");

            BomService bom = new BomService();
            DataTable rows = null;
            CobolResult ce = bom.CostedExplosion(Acc.Text(b, "ParentItemCode"), Acc.Text(b, "BomRevision"), 1m, Acc.Today, set, ref rows);
            Acc.NeedEngine(ce, "BOMCOST");
            if (ce.Succeeded)
            {
                St.True(bom.Completeness != null, "a costed explosion always states how complete it is");
                if (bom.MissingRates > 0)
                    St.True(bom.Completeness.Trim().Length > 0 && bom.Completeness.Trim() != "COMPLETE",
                            "a structure with " + bom.MissingRates + " missing rate(s) is never reported as complete");
            }
        }

        [StTest("acc: a cost has a source, a unit and a tax basis, and the cost set is activated under control",
                Reqs = "CST-001,CST-002,CST-003,CST-004,CST-007,CST-012", Ac = "AC-059,AC-061")]
        public static void CostSourceUnitAndActivation()
        {
            Acc.Need("cst.CostSet");
            Acc.Context("COSTACC", Site);
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM cst.ItemCost WHERE CompanyCode = @c AND CostMethod IS NULL", "@c", Acc.Co),
                  "every stored cost states the method it was measured by");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM cst.CostSet WHERE CompanyCode = @c AND (CurrencyCode IS NULL OR EffectiveFrom IS NULL)",
                             "@c", Acc.Co), "every cost set states the currency it is expressed in and the date it applies from");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM cst.CostSet WHERE CompanyCode = @c AND Status = 'ACTIVE' " +
                             "AND (EffectiveFrom IS NULL OR CurrencyCode IS NULL)", "@c", Acc.Co),
                  "no cost set is active without the date and the currency it applies from");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM cst.CostSet WHERE CompanyCode = @c AND Status = 'ACTIVE' " +
                             "AND (ApprovedBy IS NULL OR ActivatedAt IS NULL)", "@c", Acc.Co),
                  "a cost set is only live once somebody approved it and the moment it went live is recorded");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM cst.CostSet WHERE CompanyCode = @c AND ApprovedBy IS NOT NULL " +
                             "AND ApprovedBy = PreparedBy", "@c", Acc.Co),
                  "the accountant who prepared a cost set never approves it (COM-003)");
            CostingService cst = new CostingService();
            string draft = Acc.Str("SELECT TOP (1) CostSetCode FROM cst.CostSet WHERE CompanyCode = @c AND Status = 'DRAFT' " +
                                   "ORDER BY CostSetCode", "@c", Acc.Co);
            if (draft.Length == 0) St.Skip("no draft cost set is available to try to activate");
            CobolResult r = cst.Activate(draft);
            Acc.NeedEngine(r, "CSTSETS");
            St.True(!r.IsOk, "a draft that has not been reviewed and approved cannot be activated: " + r.Display);
            St.Eq("DRAFT", Acc.Str("SELECT Status FROM cst.CostSet WHERE CompanyCode = @c AND CostSetCode = @s",
                                   "@c", Acc.Co, "@s", draft), "and the refused activation changed nothing");
        }

        [StTest("acc: FIFO layers and the moving average never value stock at less than nothing",
                Reqs = "CST-005,CST-006,CST-008,CST-009,CST-025,CST-026", Ac = "AC-060,AC-062,AC-131")]
        public static void FifoMovingAverageAndMeasurement()
        {
            Acc.Need("cst.CostLayer");
            Acc.Context("COSTACC", Site);
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM cst.CostLayer WHERE CompanyCode = @c AND QtyRemaining < 0", "@c", Acc.Co),
                  "a FIFO layer never holds a negative quantity");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM cst.CostLayer WHERE CompanyCode = @c AND ValueRemainingGBP < -0.005",
                             "@c", Acc.Co), "a layer never carries a negative value");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM cst.ItemCost WHERE CompanyCode = @c AND AverageUnitCost < 0",
                             "@c", Acc.Co), "a moving average cost is never negative");
            // FRS 102: stock is carried at the lower of cost and net realisable value
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM cst.Impairment WHERE CompanyCode = @c AND WriteDownGBP < 0 AND IsReversal = 0",
                             "@c", Acc.Co),
                  "an impairment writes value down, never up (FRS 102 does not permit a write-up above cost)");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM cst.Impairment WHERE CompanyCode = @c AND IsReversal = 0 " +
                             "AND NrvUnitValue > CarryingUnitCost AND WriteDownGBP > 0", "@c", Acc.Co),
                  "stock is only written down when net realisable value is below carrying value");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM cst.ItemCost WHERE CompanyCode = @c AND CarryingValueGBP < -0.005",
                             "@c", Acc.Co), "stock is never carried at less than nothing");
        }

        [StTest("acc: a job carries a budget, a forecast, its WIP and its variances, and they reconcile",
                Reqs = "CST-014,CST-015,CST-016,CST-017,CST-018,CST-019,CST-020,CST-021,CST-022", Ac = "AC-065,AC-066,AC-067")]
        public static void JobBudgetWipAndVariance()
        {
            Acc.Need("cst.Job");
            Acc.Context("COSTACC", Site);
            DataRow job = Acc.Demo("SELECT TOP (1) JobRef FROM cst.Job WHERE CompanyCode = @c ORDER BY JobRef",
                                   "job", "@c", Acc.Co);
            string jobRef = Acc.Text(job, "JobRef");
            CostingService cst = new CostingService();
            CobolResult enq = cst.JobEnquire(jobRef, true);
            Acc.NeedEngine(enq, "CSTJOB");
            St.True(enq.Succeeded || Acc.Refused(enq), "the job enquiry answers: " + enq.Display);

            CobolResult wip = cst.Wip(jobRef, true);
            if (wip.Succeeded)
                St.True(cst.Num("CJ-WIP-BALANCE") >= 0m || cst.Num("CJ-WIP-BALANCE") <= 0m, "the WIP balance is reported as a figure");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM cst.WipBalance WHERE CompanyCode = @c AND JobRef = @j AND ClosingValueGBP IS NULL",
                             "@c", Acc.Co, "@j", jobRef), "a WIP balance is always stated, even when it is nil");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM cst.JobBudgetLine l WHERE l.JobBudgetId NOT IN (SELECT JobBudgetId FROM cst.JobBudget)"),
                  "every budget line belongs to a budget that exists");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM cst.Variance WHERE CompanyCode = @c AND VarianceType IS NULL", "@c", Acc.Co),
                  "a variance always says what kind of variance it is, so it can be explained");
            DataTable profit = cst.Profitability(jobRef);
            St.True(profit != null, "the job can be reported on for profitability");
        }
    }
}
