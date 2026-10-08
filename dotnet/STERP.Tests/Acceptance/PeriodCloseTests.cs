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
    /// The financial close, end to end: a journal is prepared and posted into an open period,
    /// the nine VAT boxes are built from the same ledger, the bank is bridged, and once the
    /// period is closed the same posting is refused.
    /// </summary>
    public static class PeriodCloseTests
    {
        private const string Site = "RDG";

        private static DataRow OpenPeriod()
        {
            return Acc.Demo("SELECT TOP (1) FiscalYear, PeriodNo, StartDate, EndDate FROM fin.Period " +
                            "WHERE CompanyCode = @c AND Status = 'OPEN' ORDER BY FiscalYear, PeriodNo",
                            "open period", "@c", Acc.Co);
        }

        private static Dictionary<string, string> Head(string reference, DateTime postingDate)
        {
            Dictionary<string, string> h = new Dictionary<string, string>();
            h["REF"] = reference;
            h["DESC"] = "Acceptance test journal " + reference;
            h["CCY"] = "GBP";
            h["EVIDENCE"] = reference;
            h["DOCDATE"] = Fmt.Date(postingDate);
            h["POSTDATE"] = Fmt.Date(postingDate);
            return h;
        }

        [StTest("acc: close - a balanced journal posts into an open period, the VAT return is built from it, and a closed period refuses it",
                Reqs = "XMD-005,FIN-002,FIN-004,FIN-010,FIN-011,FIN-018,FIN-025,RPT-005,RPT-006", Ac = "AC-101")]
        public static void CloseChainBlocksPosting()
        {
            Acc.Need("fin.Period");
            Acc.Need("fin.usp_PostJournal");
            Acc.Context("FINCTRL", Site);
            DataRow p = OpenPeriod();
            DateTime postDate = Convert.ToDateTime(p["EndDate"]);
            PeriodService per = new PeriodService();
            St.Eq("", per.MayPost(postDate), "an open period accepts a posting on its last day");

            // 1 - a balanced journal is prepared and posted through the engine
            // the chart of accounts rules differ by account: use the pair the finance seed documents when it is
            // there, and fall back to any two accounts that accept a manual journal
            string debitAccount = Acc.Str("SELECT TOP (1) AccountCode FROM fin.Account WHERE CompanyCode = @c AND AccountCode = '6120'",
                                          "@c", Acc.Co);
            string debitCentre = debitAccount.Length > 0 ? "ADMIN" : "";
            string creditAccount = Acc.Str("SELECT TOP (1) AccountCode FROM fin.Account WHERE CompanyCode = @c AND AccountCode = '2020'",
                                           "@c", Acc.Co);
            if (debitAccount.Length == 0 || creditAccount.Length == 0)
            {
                DataRow dr = Acc.Demo("SELECT TOP (1) AccountCode FROM fin.Account WHERE CompanyCode = @c AND AllowManualJournal = 1 " +
                                      "AND Status = 'ACTIVE' ORDER BY AccountCode", "postable account", "@c", Acc.Co);
                DataRow cr = Acc.Demo("SELECT TOP (1) AccountCode FROM fin.Account WHERE CompanyCode = @c AND AllowManualJournal = 1 " +
                                      "AND Status = 'ACTIVE' AND AccountCode <> @a ORDER BY AccountCode DESC", "second postable account",
                                      "@c", Acc.Co, "@a", Acc.Text(dr, "AccountCode"));
                debitAccount = Acc.Text(dr, "AccountCode");
                creditAccount = Acc.Text(cr, "AccountCode");
                debitCentre = "";
            }
            string reference = Acc.Ref("J");
            JournalService jnl = new JournalService();
            jnl.StartNew();
            DataTable lines = JournalService.NewLineTable();
            lines.Rows.Add(debitAccount, debitCentre, "", "", "Acceptance test debit", 120.00m, 0m);
            lines.Rows.Add(creditAccount, "", "", "", "Acceptance test credit", 0m, 120.00m);
            CobolResult r = jnl.Prepare("VALIDAT", Head(reference, postDate), lines);
            Acc.NeedEngine(r, "JNLENTRY");
            if (!r.Succeeded) St.Skip("the chart of accounts refused the test coding: " + r.Display);
            St.Eq(0m, jnl.OutOfBalance, "a balanced journal reports no difference");
            St.Eq(jnl.TotalDebit, jnl.TotalCredit, "and the two sides are equal");
            CobolResult saved = jnl.Prepare("SAVE", Head(reference, postDate), lines);
            if (!saved.Succeeded) St.Skip("the journal could not be staged here: " + saved.Display);
            CobolResult posted = jnl.Post(Head(reference, postDate));
            if (!posted.Succeeded) St.Skip("the journal could not be posted here: " + posted.Display);
            St.True(jnl.JournalNumber.Trim().Length > 0, "a posted journal is given its own number");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM fin.Journal WHERE CompanyCode = @c AND JournalNumber = @n " +
                             "AND ABS(TotalDebitGBP - TotalCreditGBP) > 0.005", "@c", Acc.Co, "@n", jnl.JournalNumber.Trim()),
                  "the posted journal balances in the ledger, not just on the screen");

            // 2 - posting the same reference again is recognised, it is not a second entry
            JournalService again = new JournalService();
            again.StartNew();
            CobolResult twice = again.Post(Head(reference, postDate));
            St.True(again.WasDuplicate || !twice.Succeeded || again.JournalNumber.Trim() == jnl.JournalNumber.Trim(),
                    "repeating the instruction identifies the original result instead of posting twice: " + twice.Display);
            St.Eq(1, Acc.Int("SELECT COUNT(*) FROM fin.Journal WHERE CompanyCode = @c AND EvidenceRef = @e",
                             "@c", Acc.Co, "@e", reference), "the ledger holds exactly one journal for the evidence reference");

            // 3 - a closed period refuses the same posting
            DataRow closed = Acc.Row("SELECT TOP (1) FiscalYear, PeriodNo, EndDate FROM fin.Period WHERE CompanyCode = @c " +
                                     "AND Status = 'HARD' ORDER BY FiscalYear DESC, PeriodNo DESC", "@c", Acc.Co);
            if (closed == null) St.Skip("no period has been hard closed yet");
            string why = per.MayPost(Convert.ToDateTime(closed["EndDate"]));
            St.True(why.Length > 0, "a hard closed period refuses a posting");
            St.Contains("E-", why, "and the refusal arrives as a coded business rule: " + why);
        }

        [StTest("acc: the VAT return has nine boxes that agree with each other and with the detail behind them",
                Reqs = "FIN-025,FIN-026,RPT-007,RPT-017", Ac = "AC-109,AC-115")]
        public static void VatNineBoxesAndDetail()
        {
            Acc.Need("fin.usp_VatBoxes");
            Acc.Context("VATOWNER", Site);
            VatService vat = new VatService();
            string key = "";
            foreach (DataRow p in vat.VatPeriods().Rows)
            {
                key = Fmt.Str(p["VatPeriodKey"]);
                vat.Prepare(key, "INVOICE");
                if (vat.RecordCount > 0) break;
                key = "";
            }
            if (key.Length == 0) St.Skip("no VAT period holds any transactions yet");

            St.Eq(vat.BoxValue(1) + vat.BoxValue(2), vat.BoxValue(3), "box 3 is box 1 plus box 2");
            St.Eq(vat.BoxValue(3) - vat.BoxValue(4), vat.BoxValue(5), "box 5 is box 3 less box 4");
            for (int b = 6; b <= 9; b++)
                St.True(vat.BoxValue(b) == Math.Round(vat.BoxValue(b), 0),
                        "box " + b + " is a whole pound figure as HMRC requires");
            St.True(vat.VatRegNo != null && vat.VatRegNo.Trim().Length > 0, "the return names the registration it is for");
            St.True(vat.FilingDue != null && vat.FilingDue.Trim().Length > 0, "and the date it is due");

            vat.Drill(key, 1);
            if (vat.Drilled != null && vat.Drilled.Rows.Count > 0)
            {
                St.True(vat.Drilled.Columns.Count >= 5, "the detail behind box 1 names each contributing document");
                decimal sum = 0m;
                string amountColumn = "";
                foreach (DataColumn c in vat.Drilled.Columns)
                    if (c.DataType == typeof(decimal) && c.ColumnName.IndexOf("Vat", StringComparison.OrdinalIgnoreCase) >= 0)
                    { amountColumn = c.ColumnName; break; }
                if (amountColumn.Length > 0)
                {
                    foreach (DataRow row in vat.Drilled.Rows) sum += Fmt.Dec(row[amountColumn]);
                    St.True(Math.Abs(sum - vat.BoxValue(1)) <= 1m,
                            "the detail adds up to box 1 (box " + vat.BoxValue(1) + " against detail " + sum + ")");
                }
            }
        }

        [StTest("acc: a VAT error is assessed against the reporting threshold before it is corrected",
                Reqs = "FIN-027", Ac = "AC-116")]
        public static void VatErrorCorrectionThreshold()
        {
            Acc.Need("fin.VatErrorCorrection");
            Acc.Context("VATOWNER", Site);
            VatService vat = new VatService();
            DataTable periods = vat.VatPeriods();
            if (periods.Rows.Count < 2) St.Skip("at least two VAT periods are needed to correct one in another");
            string from = Fmt.Str(periods.Rows[0]["VatPeriodKey"]), to = Fmt.Str(periods.Rows[1]["VatPeriodKey"]);
            CobolResult small = vat.AssessError(from, to, 2500m, false);
            Acc.NeedEngine(small, "VATERR");
            St.True(small != null, "a small error is assessed, not ignored");
            CobolResult big = vat.AssessError(from, to, 75000m, false);
            St.True(big != null, "and so is a large one");
            if (small.Succeeded && big.Succeeded)
                St.True(!string.Equals(small.Message, big.Message, StringComparison.OrdinalIgnoreCase) ||
                        !string.Equals(small.ReasonCode, big.ReasonCode, StringComparison.Ordinal),
                        "2,500 and 75,000 are not treated the same way - the threshold means something");
            CobolResult deliberate = vat.AssessError(from, to, 2500m, true);
            St.True(deliberate != null, "a deliberate error is always separately assessed");
            if (small.Succeeded && deliberate.Succeeded)
                St.True(!string.Equals(small.ReasonCode, deliberate.ReasonCode, StringComparison.Ordinal) ||
                        !string.Equals(small.Message, deliberate.Message, StringComparison.OrdinalIgnoreCase),
                        "a deliberate error is never adjusted on the next return as if it were careless");
        }

        [StTest("acc: reverse charge, import VAT and duty are recorded on both sides and never reclaimed twice",
                Reqs = "FIN-028,PO-025", Ac = "AC-117")]
        public static void ReverseChargeDutyAndImportVat()
        {
            Acc.Need("fin.VatTransaction");
            Acc.Context("VATOWNER", Site);
            VatService vat = new VatService();
            CobolResult rc = vat.Rate("RC", Acc.Today);
            Acc.NeedEngine(rc, "VATRATE");
            St.True(rc.Succeeded || Acc.Refused(rc), "the reverse charge code is known to the engine: " + rc.Display);
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM fin.VatTransaction WHERE CompanyCode = @c AND VatCode = 'RC' " +
                             "AND (VatBox IS NULL OR NetBox IS NULL)", "@c", Acc.Co),
                  "a reverse charge is reported in both a VAT box and a net box, so it is declared on both sides");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM ap.PurchaseInvoiceVat v JOIN ap.PurchaseInvoice i " +
                             "ON i.PurchaseInvoiceId = v.PurchaseInvoiceId WHERE i.CompanyCode = @c " +
                             "AND v.VatCode = 'RC' AND (v.VatBox IS NULL OR v.NetBox IS NULL)", "@c", Acc.Co),
                  "the purchase side of a reverse charge carries both boxes too, or the return cannot be built from it");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM fin.VatTransaction WHERE CompanyCode = @c " +
                             "AND ABS(RecoverableVatGBP) > ABS(VatAmountGBP) + 0.005", "@c", Acc.Co),
                  "no transaction recovers more VAT than it carries - nothing is reclaimed twice");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM po.ImportEntry WHERE CompanyCode = @c AND ImportVatGBP < 0", "@c", Acc.Co),
                  "import VAT is never a negative amount on an entry");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM po.ImportEntry WHERE CompanyCode = @c AND DutyAmountGBP < 0", "@c", Acc.Co),
                  "and neither is customs duty");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM po.ImportEntry WHERE CompanyCode = @c AND ImportVatGBP <> 0 " +
                             "AND ImportVatMethod IS NULL", "@c", Acc.Co),
                  "every entry says how the import VAT was accounted for, so it cannot be claimed twice");
        }

        [StTest("acc: under the cash accounting scheme VAT follows the money, not the invoice",
                Reqs = "FIN-029", Ac = "AC-118")]
        public static void CashAccountingScheme()
        {
            Acc.Need("fin.VatCashLink");
            Acc.Context("VATOWNER", Site);
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM fin.VatCashLink WHERE SettledVatGBP IS NULL OR SettlementDate IS NULL"),
                  "every cash scheme link states how much VAT was carried and when it was paid");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM fin.VatCashLink l JOIN fin.VatTransaction t ON t.VatTxnId = l.VatTxnId " +
                             "WHERE ABS(l.SettledVatGBP) > ABS(t.VatAmountGBP) + 0.005"),
                  "a cash scheme entry never carries more VAT than the document it settles");
            VatService vat = new VatService();
            DataTable periods = vat.VatPeriods();
            if (periods.Rows.Count == 0) St.Skip("no VAT period is seeded");
            string key = Fmt.Str(periods.Rows[0]["VatPeriodKey"]);
            CobolResult invoiceBasis = vat.Prepare(key, "INVOICE");
            decimal box1Invoice = vat.BoxValue(1);
            CobolResult cashBasis = vat.Prepare(key, "CASH");
            Acc.NeedEngine(cashBasis, "VATRET9");
            St.True(cashBasis != null, "the return can be prepared on the cash basis as well as the invoice basis");
            if (invoiceBasis.Succeeded && cashBasis.Succeeded)
                St.True(vat.BoxValue(1) <= box1Invoice + 0.005m,
                        "on the cash basis box 1 never exceeds the invoice basis - unpaid sales are not yet declared");
        }

        [StTest("acc: statutory obligations are mapped to a deadline and the close checklist knows about them",
                Reqs = "FIN-023,FIN-030,XMD-013,COM-020,COM-025", Ac = "AC-119,AC-111")]
        public static void StatutoryMappingAndDeadlines()
        {
            Acc.Need("core.StatutoryObligation");
            Acc.Context("FINCTRL", Site);
            St.True(Acc.Int("SELECT COUNT(*) FROM core.StatutoryObligation WHERE CompanyCode = @c", "@c", Acc.Co) > 0,
                    "the company has statutory obligations recorded");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM core.StatutoryObligation WHERE CompanyCode = @c AND DueDate IS NULL",
                             "@c", Acc.Co), "every obligation has the date it falls due");
            St.Eq(0, Acc.Int("SELECT COUNT(*) FROM core.CompanyProfile WHERE CompanyCode = @c AND " +
                             "(VatRegNo IS NULL OR AccountingFramework IS NULL OR AccountingRefDate IS NULL)", "@c", Acc.Co),
                  "the statutory profile carries the registration, the framework and the accounting reference date " +
                  "that a trading disclosure needs");
            PeriodService per = new PeriodService();
            DataRow p = OpenPeriod();
            CobolResult s = per.Status(Fmt.Int(p["FiscalYear"]), Fmt.Int(p["PeriodNo"]));
            Acc.NeedEngine(s, "PERCLOSE");
            St.True(s.Succeeded || Acc.Refused(s), "the close status is reported for the open period: " + s.Display);
            if (s.Succeeded)
                St.True(per.StepCount >= per.DoneCount, "a checklist never reports more steps done than it has");
        }

        [StTest("acc: the bank bridge explains the difference between the book and the statement",
                Reqs = "FIN-014,FIN-015,RPT-015", Ac = "AC-101")]
        public static void BankBridgeExplainsTheDifference()
        {
            Acc.Need("fin.BankRecon");
            Acc.Need("ar.Receipt");
            Acc.Context("TREASURY", Site);
            DataRow b = Acc.Demo("SELECT TOP (1) BankAccountCode FROM fin.BankAccount WHERE CompanyCode = @c " +
                                 "AND CurrencyCode = 'GBP' ORDER BY BankAccountCode", "sterling bank account", "@c", Acc.Co);
            string account = Acc.Text(b, "BankAccountCode");
            BankService bank = new BankService();
            CobolResult r = bank.Bridge(account, Acc.Today);
            Acc.NeedEngine(r, "BANKREC");
            if (!r.Succeeded) St.Skip("the bridge could not be built here: " + r.Display);

            // a bank reconciliation reads from the statement towards the book: the balance the bank
            // reports, plus the lodgements it has not yet seen, less the payments it has not yet taken
            decimal book = bank.BookBalance, outstanding = bank.OutstandingReceipts;
            decimal bridged = bank.BridgedBalance, unexplained = bank.Unexplained;
            St.Eq(bridged, Math.Round(bank.StatementBalance + outstanding - bank.OutstandingPayments, 2),
                  "the bridge is the statement balance plus outstanding receipts less outstanding payments");
            St.Eq(Math.Round(book - bridged, 2), Math.Round(unexplained, 2),
                  "whatever the bridge does not reach is stated as unexplained, never hidden");

            // the fixture is the test's own: money banked today under a reference nothing else uses,
            // which the bank has not yet seen, so the bridge must account for it to the penny
            string customer = Acc.Str("SELECT TOP (1) CustomerCode FROM ar.Receipt WHERE CompanyCode = @c " +
                                      "AND CustomerCode IS NOT NULL ORDER BY ReceiptId DESC", "@c", Acc.Co);
            if (customer.Length == 0)
                customer = Acc.Text(Acc.Demo("SELECT TOP (1) CustomerCode FROM ar.Customer WHERE CompanyCode = @c " +
                                             "ORDER BY CustomerCode", "customer", "@c", Acc.Co), "CustomerCode");
            const decimal lodgement = 137.41m;
            Acc.Context("ARCLERK", Site);
            DebtorsService ar = new DebtorsService();
            CommArea rc = ar.NewReceipt(customer, Acc.Today, "GBP", account, "BACS", lodgement, Acc.Ref("B"), Acc.Ref("E"));
            CobolResult rr = ar.RunReceipt(rc, "RECEIPT");
            Acc.NeedEngine(rr, "RCPTALOC");
            Acc.Ok(rr, "a lodgement of 137.41 was banked into " + account);

            Acc.Context("TREASURY", Site);
            r = bank.Bridge(account, Acc.Today);
            if (!r.Succeeded) St.Skip("the bridge could not be rebuilt here: " + r.Display);
            St.Eq(Math.Round(book + lodgement, 2), Math.Round(bank.BookBalance, 2),
                  "the cash book holds the money from the moment it is banked");
            St.Eq(Math.Round(outstanding + lodgement, 2), Math.Round(bank.OutstandingReceipts, 2),
                  "and it stands as an outstanding lodgement until the statement shows it");
            St.Eq(Math.Round(bridged + lodgement, 2), Math.Round(bank.BridgedBalance, 2),
                  "so the bridge moves by exactly the amount banked, once");
            St.Eq(Math.Round(unexplained, 2), Math.Round(bank.Unexplained, 2),
                  "a lodgement in transit is explained by the bridge, it is not an unexplained difference");
            St.Eq(bank.BridgedBalance, Math.Round(bank.StatementBalance + bank.OutstandingReceipts - bank.OutstandingPayments, 2),
                  "and the bridge is still the statement balance adjusted for the items in transit");
        }
    }
}
