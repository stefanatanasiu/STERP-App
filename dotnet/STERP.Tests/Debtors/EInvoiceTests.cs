using System;
using System.Data;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using STERP.Tests.Acceptance;

namespace STERP.Tests.Debtors
{
    /// <summary>
    /// AR-029 - a posted sales invoice or credit note must also be producible as a structured
    /// e-invoice in the Peppol compatible UBL format decided in D-26, carrying the same values as
    /// the human readable copy, and re-issuing it must not create a second invoice.  EINVUBL is a

    // REQ: AR-029   AC: AC-120
    public static class EInvoiceTests
    {
        private static string RunRoot()
        {
            string run = Environment.GetEnvironmentVariable("STERP_RUN");
            if (!string.IsNullOrEmpty(run)) return run;
            return Path.GetFullPath(Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "..", "run"));
        }

        private static string Exe()
        {
            string exe = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "EINVUBL.exe");
            if (!File.Exists(exe)) St.Skip("EINVUBL.exe is not built");
            return exe;
        }

        private static string Extract(string xml, string localName)
        {
            string open = "<cbc:" + localName;
            int i = xml.IndexOf(open, StringComparison.Ordinal);
            if (i < 0) return null;
            int gt = xml.IndexOf('>', i);
            if (gt < 0) return null;
            int close = xml.IndexOf("</cbc:" + localName + ">", gt, StringComparison.Ordinal);
            if (close < 0) return null;
            return xml.Substring(gt + 1, close - gt - 1).Trim();
        }

        private static decimal Money(string s)
        {
            decimal d;
            if (s == null || !decimal.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture, out d))
                St.Fail("the e-invoice does not carry a readable amount: <" + s + ">");
            decimal.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture, out d);
            return d;
        }

        private static string Produce(string invoiceNumber)
        {
            ProcessStartInfo psi = new ProcessStartInfo(Exe(),
                "CO=" + Acc.Co + " INV=" + invoiceNumber + " USER=ARCLERK1");
            psi.UseShellExecute = false;
            psi.RedirectStandardOutput = true;
            psi.RedirectStandardError = true;
            psi.CreateNoWindow = true;
            psi.WorkingDirectory = AppDomain.CurrentDomain.BaseDirectory;
            using (Process p = Process.Start(psi))
            {
                string stdout = p.StandardOutput.ReadToEnd();
                p.StandardError.ReadToEnd();
                if (!p.WaitForExit(120000))
                {
                    try { p.Kill(); } catch { }
                    St.Fail("EINVUBL did not finish within two minutes");
                }
                if (p.ExitCode > 4) St.Fail("EINVUBL ended with return code " + p.ExitCode + ": " + stdout);
            }
            return Path.Combine(Path.Combine(RunRoot(), "einvoice"), Acc.Co + "-" + invoiceNumber + ".xml");
        }

        [StTest("A posted invoice is produced as a structured e-invoice carrying the same values",
            Reqs = "AR-029", Ac = "AC-120")]
        public static void StructuredInvoiceMatchesTheLedger()
        {
            Acc.Need("ar.SalesInvoice");
            DataRow inv = Acc.Demo(
                "SELECT TOP (1) InvoiceNumber, CustomerCode, NetAmountGBP, VatAmountGBP, " +
                "GrossAmountGBP, CurrencyCode FROM ar.SalesInvoice " +
                "WHERE CompanyCode = @c AND Status = 'POSTED' AND InvoiceNumber LIKE 'SI%' " +
                "ORDER BY InvoiceId", "posted sales invoice", "@c", Acc.Co);
            string number = Acc.Text(inv, "InvoiceNumber");

            string path = Produce(number);
            St.True(File.Exists(path), "no structured e-invoice was written to " + path);
            string xml = File.ReadAllText(path);

            St.Contains("urn:fdc:peppol.eu", xml, "the e-invoice declares the Peppol billing profile");
            St.Eq(number, Extract(xml, "ID"), "the e-invoice identifies the invoice");
            St.Eq(Acc.Num(inv, "NetAmountGBP"), Money(Extract(xml, "LineExtensionAmount")),
                "the structured net equals the ledger net");
            St.Eq(Acc.Num(inv, "VatAmountGBP"), Money(Extract(xml, "TaxAmount")),
                "the structured VAT equals the ledger VAT");
            St.Eq(Acc.Num(inv, "GrossAmountGBP"), Money(Extract(xml, "TaxInclusiveAmount")),
                "the structured gross equals the ledger gross");
            St.Eq(Acc.Num(inv, "GrossAmountGBP"), Money(Extract(xml, "PayableAmount")),
                "the payable amount equals the ledger gross");
        }

        [StTest("Re-issuing an e-invoice does not create a second invoice",
            Reqs = "AR-029", Ac = "AC-120")]
        public static void ReissueDoesNotCreateASecondInvoice()
        {
            Acc.Need("ar.SalesInvoice");
            DataRow inv = Acc.Demo(
                "SELECT TOP (1) InvoiceNumber, GrossAmountGBP FROM ar.SalesInvoice " +
                "WHERE CompanyCode = @c AND Status = 'POSTED' AND InvoiceNumber LIKE 'SI%' " +
                "ORDER BY InvoiceId", "posted sales invoice", "@c", Acc.Co);
            string number = Acc.Text(inv, "InvoiceNumber");

            int before = Acc.Int("SELECT COUNT(*) FROM ar.SalesInvoice WHERE CompanyCode = @c " +
                "AND InvoiceNumber = @n", "@c", Acc.Co, "@n", number);
            decimal gross = Acc.Num(inv, "GrossAmountGBP");

            string path = Produce(number);
            Produce(number);

            string dir = Path.GetDirectoryName(path);
            string[] files = Directory.GetFiles(dir, Acc.Co + "-" + number + "*.xml");
            St.Eq(1, files.Length, "re-issuing wrote more than one structured e-invoice");

            St.Eq(before, Acc.Int("SELECT COUNT(*) FROM ar.SalesInvoice WHERE CompanyCode = @c " +
                "AND InvoiceNumber = @n", "@c", Acc.Co, "@n", number),
                "re-issuing created a second invoice in the ledger");
            St.Eq(gross, Acc.Dec("SELECT GrossAmountGBP FROM ar.SalesInvoice WHERE CompanyCode = @c " +
                "AND InvoiceNumber = @n", "@c", Acc.Co, "@n", number),
                "re-issuing changed the invoice it was asked to reproduce");

            string xml = File.ReadAllText(path);
            St.Eq(gross, Money(Extract(xml, "PayableAmount")),
                "the re-issued copy carries the same payable amount as the ledger");
        }
    }
}
