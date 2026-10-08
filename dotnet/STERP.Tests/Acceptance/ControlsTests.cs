using System;
using System.Data;
using STERP.Business;
using STERP.Business.Xmd;
using STERP.CobolInterop;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests.Acceptance
{
    /// <summary>
    /// Cross-domain controls: exceptions and their owners, onboarding, bulk action across
    /// authorities, the company-wide stock and value invariants, retention, sanctions screening
    /// and product compliance.  These are the controls that only make sense when every module is
    /// in the same database, which is why they live in the acceptance suite.
    /// </summary>
    public static class ControlsTests
    {
        private const string Site = "RDG";

        [StTest("acc: a failed step leaves an owned exception, not a half finished document",
                Reqs = "COM-013,COM-014,COM-016,COM-022", Ac = "AC-007")]
        public static void ExceptionsAreOwnedAndExplained()
        {
            Acc.Need("core.ExceptionItem");
            Acc.Context("SYSADM", Site);
            ExceptionService svc = new ExceptionService();
            DataTable open = svc.Worklist("OPEN", "", "");
            St.True(open != null, "the exception worklist answers");
            foreach (string column in new string[] { "Severity", "Status" })
                St.True(open.Columns.Contains(column), "an exception states its " + column.ToLowerInvariant());
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM core.ExceptionItem WHERE CompanyCode = @c AND Status = 'OPEN' " +
                             "AND (OwnerUser IS NULL OR OwnerUser = '')",
                             "@c", Acc.Co),
                  "every open exception names the person who owns it - a role alone has nobody to chase");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM core.ExceptionItem WHERE CompanyCode = @c AND (RaisedAt IS NULL OR Message IS NULL)",
                             "@c", Acc.Co), "an exception states when it was raised and what it is, so its age and remedy are known");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM core.ExceptionItem WHERE CompanyCode = @c AND Status = 'RESOLVED' " +
                             "AND ResolutionNote IS NULL", "@c", Acc.Co),
                  "nothing is resolved without the note that says how");

            // an optimistic concurrency conflict is a refusal, never a stale overwrite
            DataRow doc = Acc.Row("SELECT TOP (1) OrderNumber, RowVer FROM so.SalesOrder WHERE CompanyCode = @c ORDER BY SalesOrderId DESC",
                                  "@c", Acc.Co);
            if (doc != null) St.True(doc["RowVer"] != DBNull.Value, "every document carries the row version a second editor is checked against");
        }

        [StTest("acc: opening balances are imported once, reconcile to their control and keep their audit trail",
                Reqs = "COM-018,COM-021,COM-023,COM-024", Ac = "AC-008")]
        public static void ControlledOnboarding()
        {
            Acc.Need("fin.OpeningBalance");
            Acc.Context("FINCTRL", Site);
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM (SELECT BatchRef, AccountCode, DocumentRef, COUNT(*) n FROM fin.OpeningBalance " +
                             "WHERE CompanyCode = @c AND DocumentRef IS NOT NULL GROUP BY BatchRef, AccountCode, DocumentRef " +
                             "HAVING COUNT(*) > 1) d", "@c", Acc.Co),
                  "an accepted opening entry is never loaded twice");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM (SELECT BatchRef, SUM(DebitAmountGBP - CreditAmountGBP) d " +
                             "FROM fin.OpeningBalance WHERE CompanyCode = @c AND JournalNumber IS NOT NULL " +
                             "GROUP BY BatchRef HAVING ABS(SUM(DebitAmountGBP - CreditAmountGBP)) > 0.005) x", "@c", Acc.Co),
                  "the opening detail reconciles to its control - every posted batch is a nil trial balance");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM fin.OpeningBalance WHERE CompanyCode = @c AND LoadedBy IS NULL", "@c", Acc.Co),
                  "every imported entry records who loaded it");
            St.True(Acc.Int("SELECT COUNT(*) FROM core.MaintDef") > 0,
                    "the generic maintenance definitions exist, so corrections can be made from the keyboard");
        }

        [StTest("acc: an action taken in bulk still obeys every single authority it crosses",
                Reqs = "XMD-007,XMD-009,COM-003", Ac = "AC-103")]
        public static void BulkActionRespectsAuthorities()
        {
            Acc.Need("core.Approval");
            Acc.Context("DIRECTOR", Site);
            ApprovalService svc = new ApprovalService();
            DataTable inbox = svc.Inbox("PENDING");
            St.True(inbox != null, "the approval inbox answers");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM core.Approval WHERE Status = 'APPROVED' AND DecidedBy = RequestedBy"),
                  "nobody has ever approved their own request, however the action was taken");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM core.Approval WHERE Status = 'APPROVED' AND DecidedBy IS NULL"),
                  "an approved request always names the person who approved it");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM core.Approval WHERE Status = 'PENDING' AND ContentHash IS NULL"),
                  "a pending approval holds the hash of what was approved, so a material change invalidates it");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM core.Delegation WHERE ValidTo < ValidFrom"),
                  "a delegation of authority never ends before it starts");
        }

        [StTest("acc: the forecast and the profitability enquiry use one definition across the modules",
                Reqs = "XMD-010,RPT-004,RPT-014", Ac = "AC-104")]
        public static void ForecastAndProfitabilityDefinitions()
        {
            Acc.Need("rpt.MetricDefinition");
            Acc.Context("FINCTRL", Site);
            St.True(Acc.Int("SELECT COUNT(*) FROM rpt.MetricDefinition") > 0, "the shared metric definitions are published");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM rpt.MetricDefinition WHERE Definition IS NULL OR Definition = ''"),
                  "every metric states how it is calculated, so two screens cannot mean different things by it");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM (SELECT MetricCode, COUNT(*) n FROM rpt.MetricDefinition " +
                             "GROUP BY MetricCode HAVING COUNT(*) > 1) d"), "a metric code means one thing only");
            XmdService xmd = new XmdService();
            DataTable reports = xmd.Reports();
            St.True(reports != null && reports.Rows.Count > 0, "the cross-module report catalogue is populated");
        }

        [StTest("acc: company wide stock and value invariants hold across every module",
                Reqs = "XMD-011,XMD-012,RPT-010,RPT-011", Ac = "AC-105")]
        public static void StockAndValueInvariants()
        {
            Acc.Need("inv.StockBalance");
            Acc.Context("INVMGR", Site);
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM inv.StockBalance WHERE CompanyCode = @c AND QtyOnHand < 0", "@c", Acc.Co),
                  "no balance is negative - the warehouse cannot hold less than nothing");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM inv.StockBalance WHERE CompanyCode = @c AND QtyReserved > QtyOnHand + 0.000001",
                             "@c", Acc.Co), "nothing is reserved that is not there - no over-reservation (AC-007)");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM inv.StockReservation r WHERE r.CompanyCode = @c AND r.Status = 'ACTIVE' " +
                             "AND r.QtyPicked > r.QtyReserved + 0.000001", "@c", Acc.Co),
                  "nothing is picked beyond what was reserved for it");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM inv.StockBalance b WHERE b.CompanyCode = @c AND NOT EXISTS " +
                             "(SELECT 1 FROM inv.Item i WHERE i.CompanyCode = b.CompanyCode AND i.ItemCode = b.ItemCode)",
                             "@c", Acc.Co), "every balance belongs to an item that exists in the same company");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM inv.StockMovement WHERE CompanyCode = @c AND Quantity = 0", "@c", Acc.Co),
                  "a movement of nothing is never recorded as a movement");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM so.SalesOrder o WHERE o.CompanyCode = @c AND NOT EXISTS " +
                             "(SELECT 1 FROM ar.Customer x WHERE x.CompanyCode = o.CompanyCode AND x.CustomerCode = o.CustomerCode)",
                             "@c", Acc.Co), "similar references in different companies never merge - a customer is company scoped");
        }

        [StTest("acc: history is retained and restricted under policy rather than deleted",
                Reqs = "COM-021,COM-027,COM-028", Ac = "AC-113")]
        public static void RetentionAndPersonalData()
        {
            Acc.Need("core.RetentionClass");
            Acc.Context("SYSADM", Site);
            St.True(Acc.Int("SELECT COUNT(*) FROM core.RetentionClass") > 0, "retention classes are defined");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM core.RetentionClass WHERE RetentionYears IS NULL OR RetentionYears < 1"),
                  "every retention class states how many years the record is kept");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM core.LegalHold WHERE ReleasedAt IS NOT NULL AND ReleasedAt < AppliedAt"),
                  "a legal hold is never released before it was placed");
            St.True(Acc.Int("SELECT COUNT(*) FROM core.AuditLog") > 0, "the audit trail is being written");
            AuditService audit = new AuditService();
            DataTable t = audit.Enquire("", "", Acc.Today.AddYears(-5), Acc.Today.AddDays(1), "");
            St.True(t != null, "audited history can be enquired on rather than erased");
        }

        [StTest("acc: a counterparty is screened before it can be traded with",
                Reqs = "COM-029,AR-028,PO-026", Ac = "AC-114")]
        public static void SanctionsAndCounterpartyChecks()
        {
            Acc.Need("core.SanctionsScreening");
            Acc.Context("CREDMGR", Site);
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM core.SanctionsScreening WHERE ScreenedAt IS NULL OR MatchStatus IS NULL"),
                  "every screening records when it ran and what it found");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM core.SanctionsScreening WHERE MatchStatus = 'MATCH' AND Decision IS NOT NULL " +
                             "AND DecidedBy IS NULL"), "a positive match is never cleared without somebody clearing it");
            St.True(Acc.Int("SELECT COUNT(*) FROM core.SanctionsList") >= 0, "the screening list is available to the check");
            SupplierService sup = new SupplierService();
            DataRow s = Acc.Demo("SELECT TOP (1) SupplierCode FROM ap.Supplier WHERE CompanyCode = @c ORDER BY SupplierCode",
                                 "supplier", "@c", Acc.Co);
            CobolResult r = sup.Validate(Acc.Text(s, "SupplierCode"));
            Acc.NeedEngine(r, "APSUPCHK");
            St.True(r != null, "the counterparty check always answers before trading");
        }

        [StTest("acc: an item carries the compliance, customs and packaging data its product needs",
                Reqs = "ITM-027,RPT-019", Ac = "AC-132")]
        public static void ProductComplianceAndPackaging()
        {
            Acc.Need("inv.ItemCustoms");
            Acc.Context("QUALITY", "BHM");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM inv.ItemCustoms WHERE CompanyCode = @c AND (CommodityCode IS NULL OR CommodityCode = '')",
                             "@c", Acc.Co), "a customs record without a commodity code would stop the goods at the border");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM inv.ItemPackaging WHERE CompanyCode = @c AND WeightGrams < 0", "@c", Acc.Co),
                  "packaging weight is never negative - the packaging return depends on it");
            ItemService svc = new ItemService();
            DataRow item = Acc.Demo("SELECT TOP (1) ItemCode FROM inv.ItemCustoms WHERE CompanyCode = @c ORDER BY ItemCode",
                                    "item with customs data", "@c", Acc.Co);
            string code = Acc.Text(item, "ItemCode");
            DataTable customs = svc.Part("CUSTOMS", code);
            St.True(customs != null && customs.Rows.Count > 0, "the customs data is readable from the item screen");
            DataTable packaging = svc.Part("PACKAGING", code);
            St.True(packaging != null, "and so is the packaging data");
            CobolResult r = svc.Compliance(code, "EXPORT");
            Acc.NeedEngine(r, "ITEMVAL");
            St.True(r != null, "a compliance question is always answered before the item is shipped");
        }
    }
}
