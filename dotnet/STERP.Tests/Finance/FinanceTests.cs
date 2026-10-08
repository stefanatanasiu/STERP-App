using System.Collections.Generic;
using System.Data;
using STERP.Business;
using STERP.CobolInterop;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests
{
    /// <summary>Finance service facades: journals, period close, VAT and bank reconciliation (module fin).</summary>
    public static class FinanceTests
    {
        private static void Context()
        {
            SterpSession.Current.UserCode = "FINCTRL";
            SterpSession.Current.CompanyCode = "STC";
            SterpSession.Current.SiteCode = "RDG";
        }

        private static void Need(string objectName)
        {
            if (!SqlHelper.ObjectExists(objectName)) St.Skip(objectName + " is not deployed yet");
        }

        [StTest("fin: a journal COMMAREA round-trips its header and every line", Reqs = "FIN-004", Ac = "AC-004")]
        public static void JournalCommAreaRoundTrip()
        {
            CommArea ca = CommArea.For("JNLENTRYCA");
            ca.SetString("JE-JOURNAL-REF", "JNL-DEMO-1");
            ca.SetDate("JE-POSTING-DATE", new System.DateTime(2026, 10, 31));
            ca.SetInt("JE-LINE-CNT", 2);
            ca.SetRow("JE-LINES", 1, "JE-L-NUM", 1, "JE-L-ACCOUNT", "6120", "JE-L-DEBIT", 1250.75m,
                      "JE-L-DIM-CC", "ADMIN", "JE-L-NARRATIVE", "Insurance accrual");
            ca.SetRow("JE-LINES", 2, "JE-L-NUM", 2, "JE-L-ACCOUNT", "2020", "JE-L-CREDIT", 1250.75m);
            St.Eq("JNL-DEMO-1", ca.GetString("JE-JOURNAL-REF"), "the journal reference survives the buffer");
            St.Eq(20261031, ca.GetInt("JE-POSTING-DATE"), "the posting date is held as YYYYMMDD");
            St.Eq(1250.75m, ca.GetDecimal("JE-L-DEBIT(1)"), "a packed decimal debit keeps its pence");
            St.Eq("2020", ca.GetString("JE-L-ACCOUNT(2)"), "the second occurrence sits at the right offset");
            DataTable lines = ca.GetTable("JE-LINES", 2);
            St.Eq(2, lines.Rows.Count, "two lines are read back");
            St.Eq(1250.75m, Fmt.Dec(lines.Rows[1]["JE-L-CREDIT"]), "the credit side reads back from the table");
        }

        [StTest("fin: an unbalanced journal is refused and the balance is reported", Reqs = "FIN-002,FIN-003,FIN-004", Ac = "AC-004")]
        public static void UnbalancedJournalIsRefused()
        {
            Need("fin.Account");
            Context();
            JournalService svc = new JournalService();
            svc.StartNew();
            DataTable lines = JournalService.NewLineTable();
            lines.Rows.Add("6120", "ADMIN", "", "", "Insurance for October", 1000.00m, 0m);
            lines.Rows.Add("2020", "", "", "", "Insurance accrual", 0m, 750.00m);
            Dictionary<string, string> head = new Dictionary<string, string>();
            head["REF"] = "JNL-TEST-OOB";
            head["DESC"] = "Deliberately unbalanced test journal";
            head["POSTDATE"] = Fmt.Date(SterpSession.Current.BusinessDate);
            CobolResult r = svc.Prepare("VALIDAT", head, lines);
            if (r.IsNotFound) St.Skip("JNLENTRY is not in the engine yet (" + r.ReasonCode + ")");
            St.True(!r.IsOk, "an out of balance journal is never accepted: " + r.Display);
            St.Eq("OK", Fmt.Str(svc.LineResults.Rows[0]["JE-L-STATUS"]),
                  "the expense line passes the chart of accounts and its cost centre rule");
            St.Eq("OK", Fmt.Str(svc.LineResults.Rows[1]["JE-L-STATUS"]),
                  "the accrual line passes with no cost centre, as its rule demands");
            St.Eq(250.00m, svc.OutOfBalance, "the engine reports the difference, not the screen");
            St.Eq(1000.00m, svc.TotalDebit, "total debits");
        }

        [StTest("fin: the journal enquiry reads posted headers with their reversal link", Reqs = "FIN-006,FIN-023")]
        public static void JournalEnquiry()
        {
            Need("fin.usp_JournalEnquiry");
            Context();
            DataTable t = new JournalService().Enquiry("", "POSTED", "");
            St.True(t.Columns.Contains("ReversalOfNumber"), "a correction is a linked entry, never an edit");
            St.True(t.Columns.Contains("ReversibleGBP"), "the enquiry reports what is still reversible");
            St.True(t.Columns.Contains("EvidenceRef"), "every journal carries its evidence reference");
        }

        [StTest("fin: the close checklist identifies the steps that block a hard close", Reqs = "FIN-008,FIN-010,FIN-011", Ac = "AC-013")]
        public static void CloseChecklist()
        {
            Need("fin.usp_CloseStatus");
            Context();
            PeriodService svc = new PeriodService();
            DataTable periods = svc.Periods(0);
            St.True(periods.Rows.Count > 0, "the company calendar is seeded, found " + periods.Rows.Count + " period(s)");
            DataTable steps = null;
            foreach (DataRow p in periods.Rows)
            {
                steps = svc.Checklist(Fmt.Int(p["FiscalYear"]), Fmt.Int(p["PeriodNo"]));
                if (steps != null && steps.Rows.Count > 0) break;
                steps = null;
            }
            if (steps == null) St.Skip("no close checklist is seeded for any period yet");
            St.True(steps.Columns.Contains("IsBlocking"), "a blocking step is marked as such");
            St.True(steps.Columns.Contains("EvidenceRef"), "each completed step references its evidence");
        }

        [StTest("fin: a date outside the open calendar refuses a posting", Reqs = "FIN-010,FIN-012", Ac = "AC-012")]
        public static void ClosedPeriodBlocksPosting()
        {
            Need("fin.usp_ValidatePeriod");
            Context();
            string msg = new PeriodService().MayPost(new System.DateTime(1990, 1, 1));
            St.True(msg.Length > 0, "a date outside the calendar is never postable");
            St.Contains("E-0009", msg, "the refusal arrives as a business rule with its reason code");
        }

        [StTest("fin: the nine boxes obey box 3 = 1 + 2 and box 5 = 3 - 4", Reqs = "FIN-018,FIN-025,FIN-026", Ac = "AC-016")]
        public static void VatBoxArithmetic()
        {
            Need("fin.usp_VatBoxes");
            Context();
            VatService svc = new VatService();
            string key = "";
            foreach (DataRow p in svc.VatPeriods().Rows)
            {
                key = Fmt.Str(p["VatPeriodKey"]);
                svc.Prepare(key, "INVOICE");
                if (svc.RecordCount > 0) break;
            }
            if (key == "") St.Skip("no VAT period is seeded");
            St.Eq(svc.BoxValue(1) + svc.BoxValue(2), svc.BoxValue(3), "box 3 is box 1 plus box 2");
            St.Eq(svc.BoxValue(3) - svc.BoxValue(4), svc.BoxValue(5), "box 5 is box 3 less box 4");
            St.True(svc.BoxValue(8) == 0m && svc.BoxValue(9) == 0m, "boxes 8 and 9 are nil for a GB only trader (D-23)");
            DataRow agg = svc.Boxes(key, "INVOICE");
            St.Eq(svc.BoxValue(1), Fmt.Dec(agg["Box1"]), "VATRET9 and fin.usp_VatBoxes agree on box 1");
            St.Eq(svc.BoxValue(5), Fmt.Dec(agg["Box5"]), "VATRET9 and fin.usp_VatBoxes agree on box 5");
        }

        [StTest("fin: a box drills down to the records behind it", Reqs = "FIN-018,FIN-026", Ac = "AC-017")]
        public static void VatBoxDrillDown()
        {
            Need("fin.usp_VatBoxDrill");
            Context();
            VatService svc = new VatService();
            foreach (DataRow p in svc.VatPeriods().Rows)
            {
                string key = Fmt.Str(p["VatPeriodKey"]);
                svc.Drill(key, 1);
                if (svc.Drilled == null || svc.Drilled.Rows.Count == 0) continue;
                St.True(svc.Drilled.Columns.Count >= 6, "the drill names the document behind each contribution");
                return;
            }
            St.Skip("no VAT transactions are posted in any period yet");
        }

        [StTest("fin: the MTD submission is written to a file, not sent to HMRC", Reqs = "FIN-018", Ac = "AC-018")]
        public static void VatSubmissionFile()
        {
            Context();
            string path = new VatService().WriteSubmissionFile("26Q3");
            St.True(System.IO.File.Exists(path), "the submission file is written to " + path);
            string text = System.IO.File.ReadAllText(path);
            St.Contains("PERIODKEY 26Q3", text, "the file names the VAT period");
            St.Contains("BOX9", text, "all nine boxes are written");
            System.IO.File.Delete(path);
        }

        [StTest("fin: a grouped bank match needs equal totals or a written explanation", Reqs = "FIN-013,FIN-014,FIN-015", Ac = "AC-014")]
        public static void BankMatchNeedsExplanation()
        {
            Need("fin.usp_BankOutstanding");
            Context();
            BankService svc = new BankService();
            string bank = "";
            foreach (DataRow a in svc.Accounts().Rows)
                if (Fmt.Str(a["CurrencyCode"]) == SterpSession.Current.BaseCurrency) { bank = Fmt.Str(a["BankAccountCode"]); break; }
            if (bank == "") St.Skip("no sterling bank account is seeded");
            St.True(svc.Outstanding(bank, SterpSession.Current.BusinessDate) != null, "the bridge is reported for " + bank);
            List<decimal[]> statement = new List<decimal[]> { new decimal[] { 1m, 500.00m } };
            List<decimal[]> book = new List<decimal[]> { new decimal[] { 1m, 300.00m }, new decimal[] { 2m, 150.00m } };
            CobolResult r = svc.Match(bank, statement, book, "");
            if (r.IsNotFound) St.Skip("BANKREC is not in the engine yet (" + r.ReasonCode + ")");
            St.True(!r.Succeeded, "an unexplained 50.00 difference is never reconciled: " + r.Display);
            St.Eq(50.00m, svc.Difference, "the engine reports the residue");
            St.Eq(500.00m, svc.StatementTotal, "the statement side is totalled by the engine");
        }
    }
}
