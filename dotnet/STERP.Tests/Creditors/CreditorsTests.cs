using System.Data;
using STERP.Business;
using STERP.CobolInterop;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests
{
    /// <summary>Creditors service facades, enquiries and the AP engine (module ap).</summary>
    public static class CreditorsTests
    {
        private static void Context()
        {
            SterpSession.Current.UserCode = "APCLERK";
            SterpSession.Current.CompanyCode = "STC";
            SterpSession.Current.SiteCode = "RDG";
        }

        private static void Need(string objectName)
        {
            if (!SqlHelper.ObjectExists(objectName)) St.Skip(objectName + " is not deployed yet");
        }

        [StTest("ap: the supplier enquiry reports the ledger balance and the unverified banks", Reqs = "AP-001,AP-002")]
        public static void SupplierEnquiry()
        {
            Need("ap.usp_SupplierEnquiry");
            Context();
            DataTable t = new SupplierService().Suppliers("S0013", "ALL");
            St.Eq(1, t.Rows.Count, "Wessex Logistics is seeded in STC");
            St.True(ServiceBase.Col(t.Rows[0], "BalanceGBP", 0m) != null, "the balance comes from the open items");
            St.Eq(1, Fmt.Int(ServiceBase.Col(t.Rows[0], "UnapprovedBanks", 0)),
                  "the demo bank change is still waiting for an independent approver");
        }

        [StTest("ap: a bank change stays unusable until somebody other than the preparer approves it", Reqs = "AP-002,AP-027,D-19")]
        public static void BankChangeAwaitsApproval()
        {
            Need("ap.usp_ListSupplierBanks");
            Context();
            DataTable t = new SupplierService().Banks("S0013");
            St.True(t.Rows.Count >= 2, "the supplier has an approved record and a pending change, found " + t.Rows.Count);
            bool awaiting = false, usable = false;
            foreach (DataRow r in t.Rows)
            {
                string state = Fmt.Str(r["UsableState"]);
                if (state == "AWAITING APPROVAL")
                {
                    awaiting = true;
                    St.Eq("APCLERK", Fmt.Str(r["PreparedBy"]), "the change names its preparer");
                    St.Eq("", Fmt.Str(r["ApprovedBy"]), "and has no approver yet, so it cannot be paid to");
                }
                if (state == "USABLE") usable = true;
            }
            St.True(awaiting, "the new details are on file but not yet usable");
            St.True(usable, "the old details stay usable until the change is approved");
        }

        [StTest("ap: a Confirmation of Payee mismatch is recorded as evidence, not as a payment", Reqs = "AP-002,D-19")]
        public static void ConfirmationOfPayeeMismatch()
        {
            Need("ap.SupplierBank");
            Context();
            DataTable t = new SupplierService().Banks("S0007");
            bool mismatch = false;
            foreach (DataRow r in t.Rows)
                if (Fmt.Str(r["CopResult"]) == "NOMATCH")
                {
                    mismatch = true;
                    St.True(Fmt.Str(r["Status"]) != "APPROVED", "a CoP mismatch is never approved on the nod");
                }
            St.True(mismatch, "the demo CoP mismatch is seeded");
        }

        [StTest("ap: approving your own bank change is refused by the engine", Reqs = "AP-027,D-07,D-19")]
        public static void SelfApprovalRefused()
        {
            Need("ap.SupplierBank");
            Context();
            SupplierService svc = new SupplierService();
            DataTable t = svc.Banks("S0013");
            int bankId = 0;
            foreach (DataRow r in t.Rows)
                if (Fmt.Str(r["UsableState"]) == "AWAITING APPROVAL") bankId = Fmt.Int(r["SupplierBankId"]);
            St.True(bankId > 0, "there is a change to approve");
            CobolResult res = svc.ApproveBank("S0013", bankId, "APPROVE");
            if (res.IsNotFound) St.Skip("APBANKVR is not in the engine yet (" + res.ReasonCode + ")");
            St.True(!res.Succeeded, "APCLERK prepared the change, so APCLERK may not approve it: " + res.Display);
            St.True(res.Display.Length > 0, "and the refusal carries a message the screen can show");
            St.Eq(0, Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT COUNT(*) FROM ap.SupplierBank WHERE CompanyCode = 'STC' AND SupplierCode = 'S0013' " +
                "AND BankRef = 'BANK2' AND ApprovedBy IS NOT NULL")), "nothing was approved behind the refusal");
        }

        [StTest("ap: the register shows why a blocked invoice cannot be paid", Reqs = "AP-003,AP-008,AP-023")]
        public static void RegisterBlockedReasons()
        {
            Need("ap.usp_ListInvoiceRegister");
            Context();
            DataTable t = new InvoiceService().Register("", "BLOCKED");
            St.True(t.Rows.Count >= 3, "the demo ledger holds blocked documents, found " + t.Rows.Count);
            foreach (DataRow r in t.Rows)
            {
                St.Eq("BLOCKED", Fmt.Str(r["PaymentEligibility"]), Fmt.Str(r["RegisterNumber"]) + " is blocked");
                St.True(Fmt.Str(r["BlockReason"]).Length > 0,
                        Fmt.Str(r["RegisterNumber"]) + " names its reason rather than failing silently");
            }
        }

        [StTest("ap: open items are aged and the overdue ones are visible", Reqs = "AP-016,AP-019")]
        public static void OpenItemAgeing()
        {
            Need("ap.usp_ListApOpenItems");
            Context();
            DataTable t = new InvoiceService().OpenItems("", "OPEN");
            St.True(t.Rows.Count >= 8, "the demo ledger has open items, found " + t.Rows.Count);
            bool overdue = false, settledShown = false;
            foreach (DataRow r in t.Rows)
            {
                if (Fmt.Int(r["DaysOverdue"]) > 30) overdue = true;
                if (Fmt.Str(r["SettlementStatus"]) == "SETTLED") settledShown = true;
            }
            St.True(overdue, "at least one item is more than 30 days overdue");
            St.True(!settledShown, "a settled item is not an open item");
        }

        [StTest("ap: the purchase ledger agrees with the creditors control account", Reqs = "AP-021,AP-022")]
        public static void SubledgerAgreesWithControl()
        {
            Need("ap.OpenItem");
            Context();
            /*  The whole subsidiary ledger against the whole control account: a single posting
                source is only ever a slice of the creditors control, so comparing one SourceRef
                to the full open item ledger disagrees as soon as a payment run, an overhead
                accrual or the demonstration bridge posts to account 2000 as well. */
            decimal ledger = Fmt.Dec(SqlHelper.ExecuteScalar(
                "SELECT SUM(ResidualAmountGBP) FROM ap.OpenItem WHERE CompanyCode = @c AND SettlementStatus <> 'REVERSED'",
                "@c", "STC"));
            decimal control = Fmt.Dec(SqlHelper.ExecuteScalar(
                "SELECT SUM(l.CreditAmountGBP - l.DebitAmountGBP) FROM fin.JournalLine l " +
                "JOIN fin.Journal j ON j.JournalId = l.JournalId " +
                "WHERE j.CompanyCode = @c AND j.Status = 'POSTED' AND l.AccountCode = '2000'", "@c", "STC"));
            St.True(control != 0m, "the demo postings reached the general ledger");
            St.Eq(ledger, control, "the creditors control account equals the open item total");
            St.Eq(0, Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT COUNT(*) FROM ap.OpenItem WHERE CompanyCode = @c AND SettlementStatus <> 'REVERSED' " +
                "AND ABS(ResidualAmountGBP) > ABS(OriginalAmountGBP) + 0.005", "@c", "STC")),
                  "no payable is settled for more than it was recognised for");
        }

        [StTest("ap: the completed payment run carries remittances and an excluded line", Reqs = "AP-011,AP-013,AP-016")]
        public static void PaymentRunHistory()
        {
            Need("ap.usp_ListProposals");
            Context();
            PaymentService svc = new PaymentService();
            St.True(svc.Proposals().Rows.Count >= 1, "a payment proposal is seeded");
            DataTable l = svc.Lines("PP-26-0007");
            St.True(l.Rows.Count >= 4, "the demo run has lines, found " + l.Rows.Count);
            int paid = 0, excluded = 0;
            foreach (DataRow r in l.Rows)
            {
                if (Fmt.Bool(r["IsExcluded"]))
                {
                    excluded++;
                    St.True(Fmt.Str(r["ExclusionReason"]).Length > 0, "an excluded line always says why");
                }
                else if (Fmt.Str(r["RemittanceRef"]).Length > 0) paid++;
            }
            St.True(paid >= 3, "every paid line has a remittance, found " + paid);
            St.Eq(1, excluded, "the blocked invoice was kept out of the run");
        }

        [StTest("ap: the match tolerance test answers without touching the database", Reqs = "AP-006,AP-007")]
        public static void ToleranceCheck()
        {
            Context();
            InvoiceService svc = new InvoiceService();
            CobolResult r = svc.ToleranceCheck(100m, 10.15m, 100m, 10.00m, 100m);
            if (r.IsNotFound) St.Skip("APMATCH is not in the engine yet (" + r.ReasonCode + ")");
            St.True(r.Succeeded, "a 1.5 percent price rise is inside the tolerance: " + r.Display);
            CobolResult bad = svc.ToleranceCheck(1000m, 10.00m, 100m, 10.00m, 100m);
            St.True(!bad.IsOk, "ten times the ordered quantity is never inside the tolerance: " + bad.Display);
        }

        [StTest("ap: the module's forms and reports are registered for the shell", Reqs = "AP-023,COM-019")]
        public static void Registration()
        {
            Need("core.MenuItem");
            Context();
            SterpSession.Current.UserCode = "SYSADM";
            St.Eq(3, Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT COUNT(*) FROM core.MenuItem WHERE ParentMenuCode = 'AP' AND FormClass IN " +
                "('frmSupplierVerify','frmInvoiceRegister','frmPaymentRun')")), "the three AP forms are on the menu");
            DataTable defs = new ReportService().Definitions("ap");
            St.True(defs.Rows.Count >= 5, "the AP batch reports are registered, found " + defs.Rows.Count);
        }
    }
}
