using System;
using System.Collections.Generic;
using System.Data;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using STERP.Business;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests
{
    public static class ApBacsFormatTests
    {
        [StTest("ap Bacs format: exact Standard18 bytes, label fields, balancing contra and asymmetric counts",
            Reqs = "AP-013,AP-015,AP-026,COM-014,COM-018")]
        public static void Standard18PhysicalRecords()
        {
            ApWorkflowGuiTests.Context("TREASURY");
            if (!Fmt.Str(SqlHelper.ExecuteScalar("SELECT DB_NAME()")).StartsWith("STERP_QA_AP_DEMO_", StringComparison.OrdinalIgnoreCase))
                St.Skip("requires an isolated full AP demo baseline and independently approved utility run");
            Check("PPS00000002", 1, 14814L);
            var multiple = SqlHelper.ExecuteRow(
                "SELECT COUNT(*) AS ItemCount,SUM(AmountGBP) AS Total FROM ap.vw_BacsInstruction " +
                "WHERE ProposalRef='PPS00000001' AND CompanyCode='STC' AND CopResult IN ('MATCH','VERIFIED') " +
                "AND AmountGBP>0 AND AmountGBP<=250000 AND PaymentMethod NOT IN ('CHAPS','FPS','FASTER')");
            St.True(Fmt.Int(multiple["ItemCount"]) > 1, "native demonstration provides a multi-credit count regression");
            Check("PPS00000001", Fmt.Int(multiple["ItemCount"]), decimal.ToInt64(Fmt.Dec(multiple["Total"]) * 100));
        }

        private static string Julian(DateTime date)
        {
            return " " + (date.Year % 100).ToString("00") + date.DayOfYear.ToString("000");
        }

        private static void Check(string reference, int credits, long total)
        {
            var proposal = SqlHelper.ExecuteRow(
                "SELECT p.PayDate,p.ProposalId,b.SortCode,b.AccountNumber,b.BacsServiceUserNumber " +
                "FROM ap.PaymentProposal p JOIN fin.BankAccount b ON b.CompanyCode=p.CompanyCode " +
                "AND b.BankAccountCode=p.BankAccountCode WHERE p.CompanyCode='STC' AND p.ProposalRef=@r",
                "@r", reference);
            St.True(proposal != null, "approved native proposal exists");
            var beforeState = SqlHelper.ExecuteDataTable(
                "SELECT PaymentInstructionId,Status,BacsFileRef FROM ap.PaymentInstruction WHERE ProposalId=@p ORDER BY PaymentInstructionId",
                "@p", proposal["ProposalId"]);
            string directory = Path.Combine(SterpConfig.RunDir, "bacs");
            Directory.CreateDirectory(directory);
            Directory.CreateDirectory(Path.Combine(SterpConfig.RunDir, "spool"));
            var before = Directory.GetFiles(directory);
            long beforeJob = Convert.ToInt64(SqlHelper.ExecuteScalar(
                "SELECT ISNULL(MAX(JobRunId),0) FROM core.JobRun WHERE JobName='APBACS'"));
            Thread.Sleep(1100);
            var result = new ReportService().Run("APBACS", "APBACS", new Dictionary<string, string> {
                { "CO", "STC" }, { "RUN", reference }, { "USER", "TREASURY" }, { "MODE", "PREVIEW" } });
            St.True(result.Succeeded && !result.Output.Contains("STERP-E-"), result.Output);
            string path = Directory.GetFiles(directory).Except(before).Single();
            var jobs = SqlHelper.ExecuteDataTable(
                "SELECT JobRunId,Status,ReturnCode,EndedAt,RowsProcessed,LogFile,SpoolFile FROM core.JobRun " +
                "WHERE JobName='APBACS' AND JobRunId>@j ORDER BY JobRunId DESC", "@j", beforeJob);
            St.Eq(2, jobs.Rows.Count, "launcher and native START each register exactly one run");
            bool reproduction = result.Output.Contains("the file is a reproduction");
            foreach (DataRow job in jobs.Rows)
            {
                St.True(job["EndedAt"] != DBNull.Value, "no newly completed APBACS run remains RUNNING");
                St.Eq(reproduction ? "WARNING" : "OK", Fmt.Str(job["Status"]), "stored outcome matches native reproduction severity");
                St.Eq(reproduction ? 4 : 0, Fmt.Int(job["ReturnCode"]), "persisted return code is not downgraded");
            }
            St.Eq(path, Fmt.Str(jobs.Rows[0]["LogFile"]), "native monitoring row references its generated Bacs file");
            St.True(File.Exists(Fmt.Str(jobs.Rows[0]["SpoolFile"])), "native monitoring row has its real control listing");
            St.True(Fmt.Int(jobs.Rows[0]["RowsProcessed"]) >= credits, "native processed-row count includes the generated payments");
            byte[] bytes = File.ReadAllBytes(path);
            St.True(bytes.All(b => b == 13 || b == 10 || (b >= 32 && b <= 126)), "printable ASCII without tabs, NULs or binary record prefixes");
            string[] split = Encoding.ASCII.GetString(bytes).Split(new[] { "\r\n" }, StringSplitOptions.None);
            St.Eq("", split.Last(), "every physical record, including UTL1, ends in CRLF");
            string[] lines = split.Take(split.Length - 1).ToArray();
            St.Eq(credits + 8, lines.Length, "four headers, payments, one contra, EOF1, EOF2, UTL1");
            St.Eq(7 * 80 + (credits + 1) * 100 + lines.Length * 2, bytes.Length, "exact byte count including retained padding and CRLF");
            string[] labels = { "VOL1", "HDR1", "HDR2", "UHL1" };
            for (int i = 0; i < 4; i++) {
                St.Eq(labels[i], lines[i].Substring(0, 4), "mandatory label order");
                St.Eq(80, lines[i].Length, "80-byte header");
            }
            string sun = Fmt.Str(proposal["BacsServiceUserNumber"]);
            St.Eq(sun, lines[0].Substring(4, 6), "volume serial");
            St.Eq(sun, lines[0].Substring(41, 6), "volume owner SUN");
            St.Eq("1", lines[0].Substring(79, 1), "label standard level");
            St.Eq("A" + sun + "S   " + sun, lines[1].Substring(4, 17), "HDR1 file identifier occupies 17 columns");
            St.Eq(sun + "00010001000100", lines[1].Substring(21, 20), "set, section, sequence, generation and version");
            St.Eq(Julian(DateTime.Today), lines[1].Substring(41, 6), "creation date bYYDDD");
            DateTime pay = Convert.ToDateTime(proposal["PayDate"]);
            St.Eq(Julian(pay), lines[1].Substring(47, 6), "expiry is later than processing day");
            St.Eq("000000", lines[1].Substring(54, 6), "HDR1 block count");
            St.Eq("HDR2F0200000100", lines[2].Substring(0, 15), "single-day 100-byte record declaration");
            St.Eq("00", lines[2].Substring(50, 2), "buffer offset");
            DateTime processing = Convert.ToDateTime(SqlHelper.ExecuteScalar(
                "SELECT core.fn_AddBusinessDays('BACS',@d,-1)", "@d", pay));
            St.Eq(Julian(processing), lines[3].Substring(4, 6), "UHL1 processing date, not input/submission date");
            St.Eq("000000001 DAILY  ", lines[3].Substring(20, 17), "currency, country and single-day work code");
            St.True(lines[3].Substring(37, 3).All(char.IsDigit), "numeric file number");
            long creditTotal = 0;
            for (int i = 4; i < 4 + credits; i++) {
                St.Eq(100, lines[i].Length, "single-day payment has no appended date");
                St.Eq("99", lines[i].Substring(15, 2), "direct credit code");
                St.Eq(Fmt.Str(proposal["SortCode"]) + Fmt.Str(proposal["AccountNumber"]), lines[i].Substring(17, 14), "originating bank fields");
                creditTotal += long.Parse(lines[i].Substring(35, 11), CultureInfo.InvariantCulture);
            }
            string contra = lines[4 + credits];
            St.Eq(100, contra.Length, "100-byte debit contra");
            St.Eq("17", contra.Substring(15, 2), "debit contra code");
            St.Eq("CONTRA            ", contra.Substring(64, 18), "contra identifier at columns 65-82");
            St.Eq(total, creditTotal, "credit payment total");
            St.Eq(total.ToString("00000000000"), contra.Substring(35, 11), "11-digit contra amount in correct columns");
            string eof1 = lines[5 + credits], eof2 = lines[6 + credits], trailer = lines[7 + credits];
            St.Eq(80, eof1.Length, "80-byte EOF1");
            St.Eq(80, eof2.Length, "80-byte EOF2");
            St.Eq("EOF1" + lines[1].Substring(4, 50) + new string(' ', 6) + lines[1].Substring(60), eof1, "EOF1 mirrors header fields");
            St.Eq("EOF2" + lines[2].Substring(4), eof2, "EOF2 mirrors HDR2");
            St.Eq(80, trailer.Length, "80-byte UTL1");
            St.Eq("UTL1" + total.ToString("0000000000000") + total.ToString("0000000000000") +
                "0000001" + credits.ToString("0000000") + new string(' ', 36), trailer,
                "trailer debit/credit totals and one debit versus N credits");
            var afterState = SqlHelper.ExecuteDataTable(
                "SELECT PaymentInstructionId,Status,BacsFileRef FROM ap.PaymentInstruction WHERE ProposalId=@p ORDER BY PaymentInstructionId",
                "@p", proposal["ProposalId"]);
            St.Eq(beforeState.Rows.Count, afterState.Rows.Count, "preview retains instruction count");
            for (int i = 0; i < beforeState.Rows.Count; i++)
                foreach (string column in new[] { "Status", "BacsFileRef" })
                    St.Eq(Fmt.Str(beforeState.Rows[i][column]), Fmt.Str(afterState.Rows[i][column]), "preview leaves " + column + " unchanged");
        }
    }
}
