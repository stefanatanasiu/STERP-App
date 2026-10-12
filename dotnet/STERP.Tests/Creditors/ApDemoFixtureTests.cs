using System;
using System.Collections.Generic;
using STERP.Business;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests
{
    /// <summary>Normal demo baseline leaves a real payable for a new interactive payment run.</summary>
    public static class ApDemoFixtureTests
    {
        [StTest("ap demo GUI: native utility recognition is idempotent and funds a new independently approved Bacs run",
            Reqs = "AP-003,AP-011,AP-012,AP-013,AP-021,AP-022,D-07")]
        public static void PostedUtilityAfterExampleRun()
        {
            ApWorkflowGuiTests.Context("APCLERK");
            if (!Fmt.Str(SqlHelper.ExecuteScalar("SELECT DB_NAME()")).StartsWith("STERP_QA_AP_DEMO_", StringComparison.OrdinalIgnoreCase))
                St.Skip("requires a fresh full demo baseline in a dedicated STERP_QA_AP_DEMO_ database");
            var row = SqlHelper.ExecuteRow(
                "SELECT p.PurchaseInvoiceId,p.AccountingState,p.MatchState,p.PaymentEligibility,p.NonPoReason," +
                "oi.ResidualAmountGBP,oi.ReservedAmountGBP,oi.IsBlocked,p.JournalId,oi.DueDate " +
                "FROM ap.PurchaseInvoice p JOIN ap.OpenItem oi ON oi.PurchaseInvoiceId=p.PurchaseInvoiceId " +
                "WHERE p.CompanyCode='STC' AND p.SupplierDocRef='APDEMO-GUI-UTILITY-2026'");
            St.True(row != null, "normal APDEMO COMMIT recognises its post-run GUI utility invoice");
            St.Eq("POSTED", Fmt.Str(row["AccountingState"]), "native APPOST proved the journal");
            St.Eq("NOTREQUIRED", Fmt.Str(row["MatchState"]), "authorised non-PO utility, not a fabricated GRNI match");
            St.Eq("UTILITIES", Fmt.Str(row["NonPoReason"]), "native category validation");
            St.Eq("APPROVED", Fmt.Str(row["PaymentEligibility"]), "APMGR released the invoice block through APBLOCK");
            St.Eq(0, Fmt.Int(row["IsBlocked"]), "the legitimate utility payable is unblocked");
            St.Eq(148.14m, Fmt.Dec(row["ResidualAmountGBP"]), "native register amount");
            St.Eq(0m, Fmt.Dec(row["ReservedAmountGBP"]), "fixture was created after the example proposal");
            St.Eq(148.14m, Fmt.Dec(SqlHelper.ExecuteScalar(
                "SELECT SUM(CreditAmountGBP-DebitAmountGBP) FROM fin.JournalLine " +
                "WHERE JournalId=@j AND AccountCode='2000'", "@j", row["JournalId"])), "creditors recognition matches liability");
            St.Eq(123.45m, Fmt.Dec(SqlHelper.ExecuteScalar(
                "SELECT SUM(DebitAmountGBP-CreditAmountGBP) FROM fin.JournalLine " +
                "WHERE JournalId=@j AND AccountCode='6800' AND DimCostCentre='ADMIN'",
                "@j", row["JournalId"])), "proper expense and cost-centre posting");
            St.Eq(24.69m, Fmt.Dec(SqlHelper.ExecuteScalar(
                "SELECT SUM(DebitAmountGBP-CreditAmountGBP) FROM fin.JournalLine " +
                "WHERE JournalId=@j AND AccountCode='1250'", "@j", row["JournalId"])), "recoverable input VAT");
            decimal reservations = Fmt.Dec(SqlHelper.ExecuteScalar("SELECT SUM(ReservedAmountGBP) FROM ap.OpenItem"));
            decimal ledger = Fmt.Dec(SqlHelper.ExecuteScalar(
                "SELECT SUM(ResidualAmountGBP) FROM ap.OpenItem WHERE CompanyCode='STC' AND SettlementStatus<>'REVERSED'"));
            decimal control = Fmt.Dec(SqlHelper.ExecuteScalar(
                "SELECT SUM(l.CreditAmountGBP-l.DebitAmountGBP) FROM fin.JournalLine l JOIN fin.Journal j ON j.JournalId=l.JournalId " +
                "WHERE j.CompanyCode='STC' AND j.Status='POSTED' AND l.AccountCode='2000'"));
            St.Eq(ledger, control, "normal full baseline purchase ledger agrees with creditors control");
            for (int retry = 0; retry < 2; retry++)
            {
                var result = new ReportService().Run("APDEMO", "APDEMO", new Dictionary<string, string> {
                    { "CO", "STC" }, { "USER", "APCLERK" }, { "MODE", "COMMIT" } });
                St.True(result.Succeeded, result.Output);
            }
            St.Eq(1, Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT COUNT(*) FROM ap.PurchaseInvoice WHERE CompanyCode='STC' AND SupplierDocRef='APDEMO-GUI-UTILITY-2026'")),
                "reruns do not duplicate the register");
            St.Eq(1, Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT COUNT(*) FROM fin.Journal WHERE CompanyCode='STC' AND CorrelationId='APDEMO-GUI-STC-2026'")),
                "reruns do not duplicate the nominal posting");
            St.Eq(reservations, Fmt.Dec(SqlHelper.ExecuteScalar("SELECT SUM(ReservedAmountGBP) FROM ap.OpenItem")),
                "existing demonstration instructions keep all their reservations");
            St.Eq(ledger, Fmt.Dec(SqlHelper.ExecuteScalar(
                "SELECT SUM(ResidualAmountGBP) FROM ap.OpenItem WHERE CompanyCode='STC' AND SettlementStatus<>'REVERSED'")),
                "reruns do not fabricate another liability");
            ApWorkflowGuiTests.RunTreasuryAndBacs(new DateTime(2026, 11, 30), new DateTime(2026, 12, 2), true);
            var proposal = SqlHelper.ExecuteRow("SELECT TOP(1) ProposalId,TotalOutflowGBP,CreditsAppliedGBP FROM " +
                "ap.PaymentProposal WHERE CompanyCode='STC' ORDER BY ProposalId DESC");
            St.Eq(148.14m, Fmt.Dec(proposal["TotalOutflowGBP"]), "approved outflow equals the S0002 payment");
            St.Eq(0m, Fmt.Dec(proposal["CreditsAppliedGBP"]), "S0011 credit cannot offset S0002's liability");
            St.Eq(1, Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT COUNT(*) FROM ap.PaymentProposalLine WHERE ProposalId=@p AND SupplierCode='S0011' " +
                "AND DocumentNumber='PC-26-0002' AND IsExcluded=1 AND ExclusionReason LIKE 'No eligible invoice%'",
                "@p", proposal["ProposalId"])), "credit-only supplier is explicitly excluded, not silently applied cross-payee");
            St.Eq(ledger, Fmt.Dec(SqlHelper.ExecuteScalar(
                "SELECT SUM(ResidualAmountGBP) FROM ap.OpenItem WHERE CompanyCode='STC' AND SettlementStatus<>'REVERSED'")),
                "local Bacs submission does not settle or fabricate a ledger liability");
            St.Eq(control, Fmt.Dec(SqlHelper.ExecuteScalar(
                "SELECT SUM(l.CreditAmountGBP-l.DebitAmountGBP) FROM fin.JournalLine l JOIN fin.Journal j ON j.JournalId=l.JournalId " +
                "WHERE j.CompanyCode='STC' AND j.Status='POSTED' AND l.AccountCode='2000'")),
                "payment preparation does not alter the reconciled creditors control");
            var rerun = new ReportService().Run("APDEMO", "APDEMO", new Dictionary<string, string> {
                { "CO", "STC" }, { "USER", "APCLERK" }, { "MODE", "COMMIT" } });
            St.True(rerun.Succeeded, rerun.Output);
            St.Eq(1, Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT COUNT(*) FROM ap.OpenItem WHERE PurchaseInvoiceId=@i", "@i", row["PurchaseInvoiceId"])),
                "rerun after Bacs does not replenish or recreate the demonstration liability");
            St.Eq(148.14m, Fmt.Dec(SqlHelper.ExecuteScalar(
                "SELECT ReservedAmountGBP FROM ap.OpenItem WHERE PurchaseInvoiceId=@i", "@i", row["PurchaseInvoiceId"])),
                "the independently released run keeps its reservation after rerunning APDEMO");
        }
    }
}
