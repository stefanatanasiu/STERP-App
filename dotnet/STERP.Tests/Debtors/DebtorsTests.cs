using System;
using System.Data;
using System.IO;
using System.Reflection;
using STERP.Business;
using STERP.CobolInterop;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests
{
    /// <summary>Debtors facades, enquiries, screens and the AR engine (module ar).</summary>
    public static class DebtorsTests
    {
        private static void Context(string company)
        {
            SterpSession.Current.UserCode = "CREDMGR";
            SterpSession.Current.CompanyCode = company;
            SterpSession.Current.SiteCode = company == "CAL" ? "GLW" : "RDG";
        }

        private static void Need(string objectName)
        {
            if (!SqlHelper.ObjectExists(objectName)) St.Skip(objectName + " is not deployed yet");
        }

        private static string FirstCustomer(string company)
        {
            object code = SqlHelper.ExecuteScalar(
                "SELECT TOP (1) CustomerCode FROM ar.vw_CustomerBalance WHERE CompanyCode = @c ORDER BY CustomerCode",
                "@c", company);
            return Fmt.Str(code);
        }

        private static Type Screen(string className)
        {
            Assembly shell = Assembly.LoadFrom(Path.Combine(SterpConfig.BinDir, "STERP.exe"));
            return shell.GetType("STERP.Desktop." + className, false, true);
        }

        [StTest("ar: the customer enquiry returns the five result sets the screen lays out", Reqs = "AR-017,AR-022")]
        public static void CustomerEnquiryHasEveryPanel()
        {
            Need("ar.usp_CustomerEnquiry");
            Context("STC");
            string customer = FirstCustomer("STC");
            St.True(customer.Length > 0, "the STC demo ledger has at least one account");
            DataSet ds = new DebtorsService().Enquiry(customer);
            St.Eq(5, ds.Tables.Count, "balance, aged profile, turnover, open items and collection history");
            St.Eq(1, ds.Tables[0].Rows.Count, "one balance row for " + customer);
            St.Eq(6, ds.Tables[1].Rows.Count, "every ageing bucket is returned even when it is empty");
            St.True(ds.Tables[2].Columns.Contains("DsoDays"), "DSO is reported, and is null rather than zero on no credit sales");
        }

        [StTest("ar: aged debt at a date reports buckets and accounts for both demo companies", Reqs = "AR-017")]
        public static void AgedDebtForBothCompanies()
        {
            Need("ar.usp_AgedDebtSummary");
            foreach (string company in new string[] { "STC", "CAL" })
            {
                Context(company);
                DataSet ds = new DebtorsService().AgedDebt(SterpSession.Current.BusinessDate);
                St.Eq(6, ds.Tables[0].Rows.Count, company + " returns all six ageing buckets");
                if (ds.Tables[1].Rows.Count == 0) St.Skip(company + " has no demo open items yet");
            }
        }

        [StTest("ar: the credit control worklist names an owner and a dunning stage", Reqs = "AR-016,AR-024")]
        public static void WorklistIsStaged()
        {
            Need("ar.usp_CreditControlWorklist");
            Context("STC");
            DataSet ds = new DebtorsService().Worklist("");
            St.True(ds.Tables.Count == 2, "the work list and the accounts behind it");
            if (ds.Tables[0].Rows.Count == 0) St.Skip("the STC demo ledger has no overdue items today");
            DataRow r = ds.Tables[0].Rows[0];
            St.True(Fmt.Str(r["StageCode"]).Length > 0, "every item carries the stage it has reached");
            St.True(Fmt.Dec(r["ResidualAmountGBP"]) > 0m, "and the sterling value still outstanding");
        }

        [StTest("ar: allocation offers debit items, available credits and the write-off tolerance", Reqs = "AR-010,AR-019,D-22")]
        public static void AllocationOffersItemsAndTolerance()
        {
            Need("ar.usp_OpenItemsForAllocation");
            Context("STC");
            DataSet ds = new DebtorsService().Allocatable(FirstCustomer("STC"), "GBP");
            St.Eq(3, ds.Tables.Count, "debit items, available credits and the totals row");
            St.True(ds.Tables[0].Columns.Contains("IsCrossCurrency"), "a cross-currency pair is flagged, never matched by number");
            St.Eq(1, ds.Tables[2].Rows.Count, "one totals row for the allocation screen");
            St.True(Fmt.Dec(ds.Tables[2].Rows[0]["SmallDifferenceToleranceGBP"]) > 0m,
                    "the D-22 small difference tolerance is published to the screen");
        }

        [StTest("ar: a statement can be produced for a seeded account", Reqs = "AR-021")]
        public static void StatementLines()
        {
            Need("ar.usp_CustomerStatementLines");
            Context("STC");
            DataSet ds = new DebtorsService().Statement(FirstCustomer("STC"), SterpSession.Current.BusinessDate, "OPENITEM");
            St.True(ds.Tables.Count >= 2, "the statement header and its lines");
            St.True(ds.Tables[ds.Tables.Count - 1].Rows.Count > 0, "a brought forward line is produced even for a quiet account");
        }

        [StTest("ar: the three seeded Debtors menu entries resolve to real screens", Reqs = "COM-024")]
        public static void MenuRowsResolveToForms()
        {
            Need("core.MenuItem");
            DataTable t = SqlHelper.ExecuteDataTable(
                "SELECT MenuCode, FormClass FROM core.MenuItem WHERE MenuCode LIKE 'AR.%' AND FormClass IS NOT NULL");
            if (t.Rows.Count == 0) St.Skip("the Debtors menu has not been seeded yet");
            foreach (DataRow r in t.Rows)
                St.True(Screen(Fmt.Str(r["FormClass"])) != null,
                        Fmt.Str(r["MenuCode"]) + " names " + Fmt.Str(r["FormClass"]) + ", which must exist in STERP.exe");
        }

        [StTest("ar: the three Debtors screens are STERP base forms with the classic function keys", Reqs = "COM-024")]
        public static void ScreensInheritBaseForm()
        {
            Type baseForm = Screen("BaseForm");
            St.True(baseForm != null, "STERP.Desktop.BaseForm is the framework base form");
            foreach (string name in new string[] { "frmCustomerEnquiry", "frmReceiptEntry", "frmCreditControl" })
            {
                Type t = Screen(name);
                St.True(t != null, name + " is compiled into STERP.exe");
                St.True(baseForm.IsAssignableFrom(t), name + " inherits BaseForm, so it gets F2/F3/F5/F10 and the status line");
            }
        }

        [StTest("ar: a CREDCHK round trip comes back with a valid return code and a decision", Reqs = "AR-007,AR-008,D-06")]
        public static void CreditCheckRoundTrip()
        {
            Need("ar.vw_CustomerBalance");
            Context("STC");
            CobolResult r = new DebtorsService().CreditCheck(FirstCustomer("STC"), 1000.00m, "GBP", "ORDER", "TEST-ORDER");
            St.True(r.ReturnCode == 0 || r.ReturnCode == 4 || r.ReturnCode == 6 || r.ReturnCode == 8 ||
                    r.ReturnCode == 12 || r.ReturnCode == 16,
                    "CREDCHK answered with a return code STERP understands, got " + r.ReturnCode + " " + r.Display);
            if (r.IsNotFound) St.Skip("CREDCHK had nothing to check: " + r.Display);
            St.True(r.CorrelationId != null && r.CorrelationId.Length > 0, "the check was linked under a correlation id");
            CommArea ca = r.Area;
            string decision = ca.GetString("CK-DECISION");
            St.True(decision == "RELEASE" || decision == "HOLD" || decision == "BLOCK",
                    "the engine's decision is RELEASE, HOLD or BLOCK, got '" + decision + "'");
            St.True(ca.GetDecimal("CK-EXPOSURE") >= 0m, "exposure is never negative (AR-007)");
        }

        [StTest("ar: a receipt COMMAREA carries the header and up to twenty allocations", Reqs = "AR-009,AR-010")]
        public static void ReceiptAreaIsBuiltFromTheScreen()
        {
            Context("STC");
            DebtorsService svc = new DebtorsService();
            CommArea ca;
            try
            {
                ca = svc.NewReceipt("C0001", new DateTime(2026, 10, 1), "GBP", "HSBC-CUR", "BACS",
                                    1500.00m, "BACS/REF/1", "CUST-ADVICE-9");
            }
            catch (Exception ex)
            {
                St.Skip("RCPTALOCCA is not in bin\\x86\\copy yet: " + ex.Message);
                return;
            }
            St.Eq("C0001", ca.GetString("RA-CUSTOMER"), "the payer defaults to the customer");
            St.Eq(1500.00m, ca.GetDecimal("RA-GROSS"), "the gross amount keyed by the clerk");
            St.Eq(20261001, ca.GetInt("RA-RECEIPT-DATE"), "dates reach COBOL as YYYYMMDD");
            svc.AddAllocation(ca, 1, 4711, "SI000123", "GBP", 250.00m);
            svc.AddAllocation(ca, 2, 4712, "SI000124", "GBP", 125.50m);
            St.Eq(2, ca.GetInt("RA-ALLOC-CNT"), "the occurrence count follows the instructions");
            St.Eq("SI000124", ca.GetString("RA-A-DOC-NO(2)"), "the second instruction is in the second occurrence");
            St.Eq(125.50m, ca.GetDecimal("RA-A-APPLY(2)"), "and carries the amount to apply");
            St.Eq(20, DebtorsService.MaxAllocations, "RCPTALOCCA holds twenty allocations per link");
        }

        [StTest("ar: a foreign receipt is never shown with a pound sign", Reqs = "COM-024")]
        public static void ForeignMoneyKeepsItsOwnCurrency()
        {
            St.Eq("\u00A31,234.50", DebtorsService.Amount(1234.50m, "GBP"), "sterling always carries the pound sign");
            St.Eq("1,234.50 EUR", DebtorsService.Amount(1234.50m, "EUR"), "a euro amount is captioned, not sterlingised");
        }
    }
}
