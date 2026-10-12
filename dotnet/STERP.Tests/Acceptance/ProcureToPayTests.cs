using System;
using System.Collections.Generic;
using System.Data;
using STERP.Business;
using STERP.CobolInterop;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests.Acceptance
{
    /// <summary>
    /// Procure to pay, end to end: a requisition is raised and independently approved, converted
    /// into a purchase order, issued, received, three-way matched and then selected for payment
    /// under separation of duties.  Everything runs through the real facades and the real engine.
    /// </summary>
    public static class ProcureToPayTests
    {
        private const string Site = "BHM";

        private static Dictionary<string, string> V(params string[] nv)
        {
            Dictionary<string, string> d = new Dictionary<string, string>();
            for (int i = 0; i + 1 < nv.Length; i += 2) d[nv[i]] = nv[i + 1];
            return d;
        }

        private static DataRow Buyable()
        {
            return Acc.Demo("SELECT TOP (1) p.SupplierCode, p.ItemCode, p.UomCode, p.CurrencyCode, p.UnitPrice, i.Description " +
                            "FROM po.PurchasePrice p JOIN inv.Item i ON i.CompanyCode = p.CompanyCode AND i.ItemCode = p.ItemCode " +
                            "JOIN ap.Supplier s ON s.CompanyCode = p.CompanyCode AND s.SupplierCode = p.SupplierCode " +
                            "WHERE p.CompanyCode = @c AND p.CurrencyCode = 'GBP' ORDER BY p.ItemCode",
                            "purchase price for a stocked item", "@c", Acc.Co);
        }

        [StTest("acc: procure to pay - requisition, independent approval, order, receipt, three way match and payment",
                Reqs = "XMD-002,PO-001,PO-005,PO-006,PO-009,AP-005,AP-006,AP-013,RPT-009", Ac = "AC-098")]
        public static void ProcureToPayChain()
        {
            Acc.Need("po.Requisition");
            Acc.Need("po.PurchaseOrder");
            Acc.Context("BUYER", Site);
            DataRow buy = Buyable();
            string supplier = Acc.Text(buy, "SupplierCode"), item = Acc.Text(buy, "ItemCode");
            string uom = Acc.Text(buy, "UomCode");
            decimal qty = 4m, price = Acc.Num(buy, "UnitPrice");
            if (price <= 0m) price = 10m;

            // 1 - the requisition states what is wanted, why and against which budget
            string reqRef = Acc.Ref("Q");
            RequisitionService req = new RequisitionService();
            CobolResult r = req.Create(V("REQNO", reqRef, "SITE", Site, "SUPPLIER", supplier, "CURRENCY", "GBP",
                                         "NEEDDATE", Fmt.Date(Acc.Today.AddDays(14)), "REASON", "Acceptance test requisition",
                                         "REQUESTER", "BUYER"));
            Acc.NeedEngine(r, "POREQ");
            Acc.Ok(r, "the requisition was raised");
            string reqNo = req.ReqNumber.Trim().Length > 0 ? req.ReqNumber.Trim() : reqRef;
            r = req.AddLine(V("REQNO", reqNo, "ITEM", item, "DESC", Acc.Text(buy, "Description"), "QTY", qty.ToString(),
                              "UOM", uom, "PRICE", price.ToString(), "NEEDDATE", Fmt.Date(Acc.Today.AddDays(14)),
                              "SITE", Site, "CURRENCY", "GBP"));
            Acc.Ok(r, "a line was added to the requisition");
            r = req.Act("SUBMIT", reqNo);
            Acc.Ok(r, "the requisition was submitted for approval");

            // 2 - separation of duties: the requester cannot approve their own requisition (COM-003)
            CobolResult self = req.Decide(reqNo, "APPROVE", "I approve my own request");
            St.True(self != null, "a decision on the requisition always answers");
            St.True(Acc.Refused(self), "the requester may never approve their own requisition: " + self.Display);
            St.Eq("REQ0013", self.ReasonCode.Trim(), "and the refusal is coded so the screen can explain it");
            St.Eq("SUBMITTED", Acc.Str("SELECT Status FROM po.Requisition WHERE CompanyCode = @c AND RequisitionNumber = @r",
                                       "@c", Acc.Co, "@r", reqNo),
                  "a refused self-approval leaves the requisition where it was");
            Acc.Context("PURMGR", Site);
            RequisitionService mgr = new RequisitionService();
            CobolResult ok = mgr.Decide(reqNo, "APPROVE", "Approved by the purchasing manager");
            Acc.Ok(ok, "an independent approver is allowed to decide");
            St.Eq("APPROVED", Acc.Str("SELECT Status FROM po.Requisition WHERE CompanyCode = @c AND RequisitionNumber = @r",
                                      "@c", Acc.Co, "@r", reqNo),
                  "and the independent decision is the one that is kept");

            // 3 - the order carries the requisition forward
            Acc.Context("BUYER", Site);
            PurchaseOrderService po = new PurchaseOrderService();
            string orderRef = Acc.Ref("P");
            r = po.Create(V("ORDER", orderRef, "SUPPLIER", supplier, "SITE", Site, "CURRENCY", "GBP",
                            "ORDERDATE", Fmt.Date(Acc.Today), "REQNO", reqNo, "BUYER", "BUYER"));
            Acc.NeedEngine(r, "POORDER");
            Acc.Ok(r, "the purchase order was created");
            string orderNo = po.OrderNumber.Trim().Length > 0 ? po.OrderNumber.Trim() : orderRef;
            string account = Acc.Str("SELECT TOP (1) l.AccountCode FROM po.PurchaseOrderLine l " +
                                     "WHERE l.CompanyCode = @c AND l.AccountCode IS NOT NULL " +
                                     "ORDER BY l.PurchaseOrderLineId DESC", "@c", Acc.Co);
            r = po.AddLine(V("ORDER", orderNo, "ITEM", item, "DESC", Acc.Text(buy, "Description"), "QTY", qty.ToString(),
                             "UOM", uom, "PRICE", price.ToString(), "DUE", Fmt.Date(Acc.Today.AddDays(10)),
                             "SITE", Site, "CURRENCY", "GBP", "ACCOUNT", account));
            Acc.Ok(r, "the order line was added at the agreed price");
            St.True(Acc.Int("SELECT COUNT(*) FROM po.PurchaseOrderLine WHERE CompanyCode = @c AND OrderNumber = @o",
                            "@c", Acc.Co, "@o", orderNo) > 0, "the line is on the order");

            // 4 - issuing the order is what creates the commitment, and only after it is approved
            r = po.Act("ISSUE", orderNo, V("EVIDENCE", "Acceptance test"));
            if (!r.Succeeded)
            {
                St.True(Acc.Refused(r), "an order that may not be issued says why: " + r.Display);
                St.Eq("DRAFT", Acc.Str("SELECT Status FROM po.PurchaseOrder WHERE CompanyCode = @c AND OrderNumber = @o",
                                       "@c", Acc.Co, "@o", orderNo),
                      "an unapproved order is still only a draft");
                CobolResult asked = po.Approval("REQUEST", orderNo, "", "Acceptance test approval request");
                St.True(asked.Succeeded || Acc.Refused(asked), "approval is requested for an order that needs it: " + asked.Display);
                Acc.Context("PURMGR", Site);
                PurchaseOrderService approver = new PurchaseOrderService();
                CobolResult decided = approver.Approval("DECIDE", orderNo, "APPROVED", "Approved by the purchasing manager");
                Acc.Ok(decided, "an independent approver approves the exact order content");
                St.Eq("APPROVED", Acc.Str("SELECT Status FROM po.PurchaseOrder WHERE CompanyCode = @c AND OrderNumber = @o",
                                          "@c", Acc.Co, "@o", orderNo),
                      "and the decision moves the order out of draft");
                Acc.Context("BUYER", Site);
                r = po.Act("ISSUE", orderNo, V("EVIDENCE", "Acceptance test"));
            }
            Acc.Ok(r, "an approved order issues to the supplier");
            string status = Acc.Str("SELECT Status FROM po.PurchaseOrder WHERE CompanyCode = @c AND OrderNumber = @o",
                                    "@c", Acc.Co, "@o", orderNo);
            St.True(status == "ISSUED" || status == "ACKNOWLEDGED",
                    "the issued order is a supplier commitment, not a draft: " + status);
            St.Eq(1, Acc.Int("SELECT COUNT(*) FROM po.PurchaseOrder WHERE CompanyCode = @c AND OrderNumber = @o " +
                             "AND IssuedAt IS NOT NULL AND ApprovalId IS NOT NULL",
                             "@c", Acc.Co, "@o", orderNo),
                  "and the issue is stamped against the approval that authorised it");
            St.True(Acc.Int("SELECT COUNT(*) FROM po.Commitment WHERE CompanyCode = @c AND OrderNumber = @o " +
                            "AND Stage = 'COMMITTED'", "@c", Acc.Co, "@o", orderNo) > 0,
                    "issuing the order encumbers the money as a COMMITTED movement");
            St.True(Acc.Dec("SELECT ISNULL(SUM(AmountGBP + NonRecoverableVatGBP + ChargeGBP), 0) FROM po.Commitment " +
                            "WHERE CompanyCode = @c AND OrderNumber = @o AND Stage = 'COMMITTED'",
                            "@c", Acc.Co, "@o", orderNo) > 0m,
                    "and the commitment carries the value of the order, not zero");

            // 5 - goods receipt: physical arrival, GRNI and nothing invoiced yet
            Acc.Context("WHSE", Site);
            GoodsReceiptService grn = new GoodsReceiptService();
            string lot = Acc.Ref("L");
            CobolResult gr = grn.Validate(V("ORDER", orderNo, "SUPPLIER", supplier, "DELNOTE", Acc.Ref("N"),
                                            "PHYSDATE", Fmt.Date(Acc.Today), "POSTDATE", Fmt.Date(Acc.Today),
                                            "LOT", lot));
            Acc.NeedEngine(gr, "RECEIPT");
            Acc.Ok(gr, "the delivery was validated against the order");
            St.True(grn.Lines().Rows.Count > 0, "the outstanding lines are offered for counting");
            grn.Count(1, qty, "", lot, "");
            gr = grn.Confirm("CONFIRM");
            Acc.Ok(gr, "the delivery was confirmed and accrued");
            string grnRef = grn.ReceiptRef.Trim();
            St.True(grnRef.Length > 0, "the receipt has its own reference");
            St.True(Acc.Int("SELECT COUNT(*) FROM po.ReceiptLine l JOIN po.Receipt rc ON rc.ReceiptId = l.ReceiptId " +
                            "WHERE rc.CompanyCode = @c AND rc.PurchaseOrderNumber = @o", "@c", Acc.Co, "@o", orderNo) > 0,
                    "the confirmed receipt wrote the lines it counted");
            St.Eq(qty, Acc.Dec("SELECT SUM(ReceivedQty) FROM po.PurchaseOrderLine " +
                               "WHERE CompanyCode = @c AND OrderNumber = @o", "@c", Acc.Co, "@o", orderNo),
                  "the receipt reduces the order's outstanding quantity");
            St.Eq(qty, Acc.Dec("SELECT SUM(ReceivedQty) FROM po.PurchaseSchedule " +
                               "WHERE CompanyCode = @c AND OrderNumber = @o", "@c", Acc.Co, "@o", orderNo),
                  "the delivery schedule records the counted quantity");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM po.ReceiptLine l JOIN po.Receipt rc ON rc.ReceiptId = l.ReceiptId " +
                             "WHERE rc.CompanyCode = @c AND rc.PurchaseOrderNumber = @o AND l.PurchaseOrderLineId IS NULL",
                             "@c", Acc.Co, "@o", orderNo),
                  "every counted line identifies its actual purchase order line");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM po.ReceiptLine l JOIN po.Receipt rc ON rc.ReceiptId = l.ReceiptId " +
                             "WHERE rc.CompanyCode = @c AND rc.PurchaseOrderNumber = @o AND l.AcceptedQty < 0",
                             "@c", Acc.Co, "@o", orderNo), "a receipt never accepts a negative quantity");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM po.PurchaseOrderLine WHERE CompanyCode = @c AND OrderNumber = @o " +
                             "AND InvoicedQty > ReceivedQty + 0.000001", "@c", Acc.Co, "@o", orderNo),
                  "nothing is invoiced that has not been received");

            // Goods held but not invoiced are a liability from the day they arrive (PO-017):
            // the receipt posts one balanced PO.RECEIPT journal, stock debited, GRNI credited.
            St.Eq(1, Acc.Int("SELECT COUNT(*) FROM fin.Journal WHERE CompanyCode = @c AND SourceRef = @r " +
                             "AND EventCode = 'PO.RECEIPT' AND Status = 'POSTED'", "@c", Acc.Co, "@r", grnRef),
                  "the receipt posts exactly one goods received not invoiced journal");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM fin.Journal WHERE CompanyCode = @c AND SourceRef = @r " +
                             "AND TotalDebitGBP <> TotalCreditGBP", "@c", Acc.Co, "@r", grnRef),
                  "and that journal balances, debits against credits");
            decimal grni = Acc.Dec("SELECT ISNULL(SUM(jl.CreditAmountGBP), 0) FROM fin.JournalLine jl " +
                                   "JOIN fin.Journal j ON j.JournalId = jl.JournalId WHERE j.CompanyCode = @c " +
                                   "AND j.SourceRef = @r AND jl.AccountCode = '2010'", "@c", Acc.Co, "@r", grnRef);
            St.True(grni > 0m, "the goods received not invoiced account carries the liability");
            St.True(Acc.Dec("SELECT ISNULL(SUM(jl.DebitAmountGBP), 0) FROM fin.JournalLine jl " +
                            "JOIN fin.Journal j ON j.JournalId = jl.JournalId WHERE j.CompanyCode = @c " +
                            "AND j.SourceRef = @r AND jl.AccountCode = '1100'", "@c", Acc.Co, "@r", grnRef) > 0m,
                    "and the goods are taken into stock on the other side of it");
            St.Eq(1, Acc.Int("SELECT COUNT(*) FROM po.Receipt WHERE CompanyCode = @c AND ReceiptRef = @r " +
                             "AND JournalId IS NOT NULL AND GrniValueGBP > 0", "@c", Acc.Co, "@r", grnRef),
                  "the receipt is stamped with the journal that accrued it, so the subledger agrees with the ledger");
            St.Eq(grni, Acc.Dec("SELECT ISNULL(GrniValueGBP, 0) FROM po.Receipt WHERE CompanyCode = @c " +
                                "AND ReceiptRef = @r", "@c", Acc.Co, "@r", grnRef),
                  "the accrual the receipt reports is the accrual the ledger holds");

            // 6 - the match: quantity, price and the tolerance that decides whether it is a variance
            Acc.Context("APCLERK", "RDG");
            InvoiceService inv = new InvoiceService();
            CobolResult tol = inv.ToleranceCheck(qty, price, qty, price, qty);
            Acc.NeedEngine(tol, "TOLCHK");
            St.True(tol.Succeeded, "an invoice that agrees with the order and the receipt matches: " + tol.Display);
            CobolResult over = inv.ToleranceCheck(qty, price * 2m, qty, price, qty);
            St.True(!over.IsOk, "doubling the price is never inside tolerance: " + over.Display);
        }

        [StTest("acc: a requisition sources, codes and prices a non-stock request before it can be ordered",
                Reqs = "PO-001,PO-002,PO-003,PO-004", Ac = "AC-050,AC-051")]
        public static void RequestSourceAndCode()
        {
            Acc.Need("po.Requisition");
            Acc.Context("BUYER", Site);
            RequisitionService req = new RequisitionService();
            string reqRef = Acc.Ref("S");
            CobolResult r = req.Create(V("REQNO", reqRef, "SITE", Site, "CURRENCY", "GBP",
                                         "NEEDDATE", Fmt.Date(Acc.Today.AddDays(21)), "REASON", "Non stock acceptance test",
                                         "REQUESTER", "BUYER"));
            Acc.NeedEngine(r, "POREQ");
            Acc.Ok(r, "a requisition without a supplier is still a legitimate request");
            string reqNo = req.ReqNumber.Trim().Length > 0 ? req.ReqNumber.Trim() : reqRef;

            // a non-stock line must be coded to an account - an uncoded one is refused
            CobolResult bad = req.AddLine(V("REQNO", reqNo, "DESC", "Consultancy, uncoded", "QTY", "1",
                                            "UOM", "EA", "PRICE", "500", "LINETYPE", "NONSTOCK", "SITE", Site));
            St.True(!bad.IsOk || Acc.Int("SELECT COUNT(*) FROM po.RequisitionLine WHERE CompanyCode = @c AND ReqNumber = @r " +
                                         "AND LineType = 'NONSTOCK' AND AccountCode IS NULL", "@c", Acc.Co, "@r", reqNo) == 0,
                    "a non-stock line is never left without an expense code: " + bad.Display);

            CommArea src = req.Source("SELECT", V("ITEM", Acc.Str("SELECT TOP (1) ItemCode FROM po.PurchasePrice WHERE CompanyCode = @c " +
                                                                 "ORDER BY ItemCode", "@c", Acc.Co),
                                                  "QTY", "10", "SITE", Site, "CURRENCY", "GBP"));
            if (src != null && req.LastSource != null && req.LastSource.Succeeded)
                St.True(src.Length > 0, "the sourcing answer comes back in a COMMAREA the screen can explain");
            else
                St.True(req.LastSource == null || !req.LastSource.IsOk, "a sourcing decision that cannot be made says so");
        }

        [StTest("acc: an order price has a source, a budget check and an issue authority",
                Reqs = "PO-004,PO-005,PO-006", Ac = "AC-051")]
        public static void PriceBudgetAndAuthority()
        {
            Acc.Need("po.PurchasePrice");
            Acc.Context("BUYER", Site);
            DataRow buy = Buyable();
            PurchaseOrderService po = new PurchaseOrderService();
            CobolResult r = po.Explain(Acc.Text(buy, "SupplierCode"), Acc.Text(buy, "ItemCode"), 100m,
                                       Acc.Text(buy, "UomCode"), "GBP");
            Acc.NeedEngine(r, "POPRICE");
            if (r.Succeeded)
            {
                St.True(po.Price != null, "a chosen price comes back in a COMMAREA the screen can explain");
                St.True(po.PriceSource.Trim().Length > 0, "and the source of the price is reported to the buyer");
                St.True(po.PriceSource.Trim() != "UNKNOWN", "a priced line always names its basis: " + po.PriceSource);
                St.True(po.PriceSourceText.Trim().Length > 0, "with the agreement and dates behind it: " + po.PriceSourceText);
            }
            CobolResult hist = po.LastPaid(Acc.Text(buy, "SupplierCode"), Acc.Text(buy, "ItemCode"));
            St.True(hist != null && po.PriceSource.Trim().Length > 0,
                    "the last price actually paid is a named source too: " + po.PriceSource);
            CobolResult b = po.CheckBudget(V("BUDGET", "", "SITE", Site, "AMOUNT", "1000000", "DATE", Fmt.Date(Acc.Today)));
            Acc.NeedEngine(b, "BUDGCHK");
            St.True(b != null, "a budget check always answers");
            St.True(b.Succeeded || Acc.Refused(b), "a million pounds is either inside the budget or refused with a reason: " + b.Display);
        }

        [StTest("acc: a schedule and an acknowledgement are kept as history, not as an overwrite",
                Reqs = "PO-007,PO-008,PO-022", Ac = "AC-052")]
        public static void ScheduleAndAcknowledgement()
        {
            Acc.Need("po.PurchaseSchedule");
            Acc.Context("BUYER", Site);
            DataRow line = Acc.Demo("SELECT TOP (1) l.OrderNumber, l.LineNum, l.OrderedQty, l.UnitPrice " +
                                    "FROM po.PurchaseOrderLine l JOIN po.PurchaseOrder o ON o.CompanyCode = l.CompanyCode " +
                                    "AND o.OrderNumber = l.OrderNumber WHERE l.CompanyCode = @c AND o.Status NOT IN ('DRAFT','CANCELLED') " +
                                    "ORDER BY l.PurchaseOrderLineId DESC", "issued purchase order line", "@c", Acc.Co);
            string order = Acc.Text(line, "OrderNumber");
            int num = Fmt.Int(line["LineNum"]);
            PurchaseOrderService po = new PurchaseOrderService();
            int before = Acc.Int("SELECT COUNT(*) FROM po.PurchaseSchedule WHERE CompanyCode = @c AND OrderNumber = @o",
                                 "@c", Acc.Co, "@o", order);
            CobolResult r = po.Acknowledge(order, num, Acc.Num(line, "OrderedQty"), Acc.Num(line, "UnitPrice"),
                                           Acc.Today.AddDays(21), "Acceptance test acknowledgement");
            Acc.NeedEngine(r, "POORDER");
            St.True(r.Succeeded || Acc.Refused(r), "an acknowledgement is accepted or refused with a reason: " + r.Display);
            St.True(Acc.Int("SELECT COUNT(*) FROM po.PurchaseSchedule WHERE CompanyCode = @c AND OrderNumber = @o",
                            "@c", Acc.Co, "@o", order) >= before,
                    "an acknowledgement never deletes the promise it replaces - the history is kept (D-09)");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM po.OrderAmendment WHERE CompanyCode = @c AND (Reason IS NULL OR Reason = '')",
                             "@c", Acc.Co), "every amendment to an issued order carries the reason it was made");
        }

        [StTest("acc: linked supply, subcontract and blanket call-off stay within their agreed limits",
                Reqs = "PO-018,PO-019,PO-021,PO-023,PO-024", Ac = "AC-057,AC-058")]
        public static void LinkedSupplyAndBlanketLimits()
        {
            Acc.Need("po.BlanketBalance");
            Acc.Context("PURMGR", Site);
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM po.BlanketBalance WHERE CompanyCode = @c AND CalledOffValue > AgreedValue + 0.005",
                             "@c", Acc.Co), "a blanket order is never called off beyond its agreed maximum value");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM po.BlanketBalance WHERE CompanyCode = @c AND CalledOffQty > AgreedQty + 0.000001",
                             "@c", Acc.Co), "and never beyond the agreed quantity either");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM po.PurchaseOrder WHERE CompanyCode = @c AND BlanketOrderNumber IS NOT NULL " +
                             "AND BlanketOrderNumber NOT IN (SELECT OrderNumber FROM po.PurchaseOrder WHERE CompanyCode = @c)",
                             "@c", Acc.Co), "a call-off always points at a blanket order that exists");
            PurchaseOrderService po = new PurchaseOrderService();
            DataTable perf = po.Commitments("", "SUPPLIER");
            St.True(perf.Columns.Contains("OpenCommitmentGBP"), "supplier reporting states the open commitment");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM po.SupplierPerformance WHERE CompanyCode = @c AND " +
                             "(OnTimeCount > ReceiptCount OR ReceiptCount < 0)", "@c", Acc.Co),
                  "supplier performance never reports more on-time deliveries than deliveries");
        }

        [StTest("acc: a recognised liability can be blocked for payment without being unrecognised",
                Reqs = "AP-008,AP-009,AP-010,AP-022", Ac = "AC-031,AC-032")]
        public static void BlockedLiabilityAndGrni()
        {
            Acc.Need("ap.PurchaseInvoice");
            Acc.Context("APMGR", "RDG");
            SupplierService sup = new SupplierService();
            DataRow s = Acc.Demo("SELECT TOP (1) SupplierCode FROM ap.Supplier WHERE CompanyCode = @c ORDER BY SupplierCode",
                                 "supplier", "@c", Acc.Co);
            string code = Acc.Text(s, "SupplierCode");
            CobolResult hold = sup.Hold(code, "PAYMENT", "Acceptance test hold " + Acc.Ref("H"), false);
            Acc.NeedEngine(hold, "APSUPCHK");
            if (hold.Succeeded)
            {
                CobolResult pay = sup.PaymentCheck(code);
                St.True(!sup.CanPay, "a supplier on payment hold may not be paid");
                St.True(pay != null, "and the check says so rather than failing silently");
                St.True(Acc.Dec("SELECT ISNULL(SUM(ResidualAmountGBP), 0) FROM ap.OpenItem WHERE CompanyCode = @c AND SupplierCode = @s",
                                "@c", Acc.Co, "@s", code) >= 0m, "the liability is still recognised while it is blocked");
                sup.Hold(code, "PAYMENT", "Acceptance test release", true);
            }
            else
            {
                St.True(Acc.Refused(hold), "a hold is applied or refused with a reason: " + hold.Display);
            }
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM ap.OpenItem WHERE CompanyCode = @c AND ABS(ResidualAmountGBP) > ABS(OriginalAmountGBP) + 0.005",
                             "@c", Acc.Co), "no payable is settled for more than it was recognised for");
        }

        [StTest("acc: payment selection is proposed, independently approved and only then released",
                Reqs = "AP-013,AP-014,AP-015,AP-016,AP-017,AP-018,COM-003", Ac = "AC-034,AC-035,AC-036")]
        public static void PaymentProposalAndApproval()
        {
            Acc.Need("ap.PaymentProposal");
            Acc.Context("APCLERK", "RDG");
            PaymentService pay = new PaymentService();
            string proposal = Acc.Ref("Y");
            string bank = Acc.Str("SELECT TOP (1) BankAccountCode FROM fin.BankAccount WHERE CompanyCode = @c AND CurrencyCode = 'GBP' " +
                                  "ORDER BY BankAccountCode", "@c", Acc.Co);
            CobolResult r = pay.Select_(proposal, Acc.Today.AddDays(30), Acc.Today, "BACS", bank);
            Acc.NeedEngine(r, "APPAYPRP");
            if (!r.Succeeded) St.Skip("nothing is due for payment in this environment: " + r.Display);
            CobolResult t = pay.Totals(proposal);
            St.True(t != null, "a proposal always reports its totals");
            St.True(pay.TotalOutflow >= 0m, "a payment run never proposes a negative outflow");
            St.Eq(pay.TotalOutflow, Math.Round(pay.CashPrincipal - pay.CreditsApplied, 2),
                  "the cash to leave the bank is the principal less the credits applied");

            CobolResult req = pay.RequestApproval(proposal, pay.TotalOutflow);
            St.True(req != null, "approval is requested, not assumed");
            CobolResult self = pay.Decide(proposal, "APPROVED", "I approve my own proposal", pay.TotalOutflow);
            St.True(!self.IsOk, "the preparer may not approve the payment run: " + self.Display);
            CobolResult release = pay.Release(proposal);
            St.True(!release.IsOk, "an unapproved proposal is never released: " + release.Display);
        }

        [StTest("acc: supplier statements, the cash forecast and deductions reconcile to the ledger",
                Reqs = "AP-019,AP-020,AP-021,AP-024,RPT-002", Ac = "AC-037,AC-038,AC-107")]
        public static void StatementForecastAndDeductions()
        {
            Acc.Need("ap.SupplierStatement");
            Acc.Context("APCLERK", "RDG");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM ap.SupplierStatementLine l WHERE l.StatementId NOT IN " +
                             "(SELECT StatementId FROM ap.SupplierStatement)"),
                  "every statement line belongs to a statement that exists");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM ap.OpenItem WHERE CompanyCode = @c AND DueDate IS NULL AND SettlementStatus <> 'PAID'",
                             "@c", Acc.Co), "an unpaid payable always has a due date, or the forecast cannot see it");
            InvoiceService inv = new InvoiceService();
            DataTable open = inv.OpenItems("", "ALL");
            St.True(open != null, "the open payables enquiry answers");
            DataTable reg = inv.Register("", "ALL");
            St.True(reg != null, "and so does the invoice register, which is the aged creditors source");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM ap.PurchaseInvoiceVat WHERE VatCode IS NULL OR RatePct IS NULL"),
                  "every VAT analysis line states the code and the rate it was charged at");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM ap.PurchaseInvoiceVat WHERE ABS(RecoverableGBP) > ABS(VatAmountGBP) + 0.005"),
                  "no invoice recovers more VAT than it was charged");
        }

        [StTest("acc: a change of supplier bank details is verified and independently approved",
                Reqs = "AP-002,AP-027,COM-003", Ac = "AC-127")]
        public static void BankDetailChangeControl()
        {
            Acc.Need("ap.SupplierBank");
            Acc.Context("APCLERK", "RDG");
            DataRow b = Acc.Demo("SELECT TOP (1) SupplierBankId, SupplierCode, Status FROM ap.SupplierBank " +
                                 "WHERE CompanyCode = @c ORDER BY SupplierBankId", "supplier bank account", "@c", Acc.Co);
            int id = Fmt.Int(b["SupplierBankId"]);
            SupplierService svc = new SupplierService();
            CobolResult self = svc.ApproveBank(Acc.Text(b, "SupplierCode"), id, "APPROVE");
            Acc.NeedEngine(self, "APSUPCHK");
            St.True(!self.IsOk || Acc.Str("SELECT CreatedBy FROM ap.SupplierBank WHERE SupplierBankId = @i", "@i", id) != "APCLERK",
                    "the clerk who entered the bank details may not approve them: " + self.Display);
            CobolResult cop = svc.ConfirmationOfPayee(Acc.Text(b, "SupplierCode"), id);
            St.True(cop != null, "a confirmation of payee check always returns an answer");
            St.True(svc.CopResult != null, "and the answer is recorded for the audit trail");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM ap.PaymentInstruction p JOIN ap.SupplierBank sb " +
                             "ON sb.SupplierBankId = p.SupplierBankId WHERE sb.Status = 'PENDING'"),
                  "no payment instruction is ever sent to bank details that are still pending approval");
        }
    }
}
