using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading;
using System.Windows.Forms;
using STERP.Business;
using STERP.CobolInterop;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests
{
    /// <summary>Real VAT engine and reviewer UI regressions, isolated database only.</summary>
    public static class VatWorkflowGuiTests
    {
        private static void Context(string user, string company = "STC")
        {
            string database = Fmt.Str(SqlHelper.ExecuteScalar("SELECT DB_NAME()"));
            if (!database.StartsWith("STERP_QA_VAT_", StringComparison.OrdinalIgnoreCase))
                St.Skip("requires a dedicated STERP_QA_VAT_ database, never production or shared GUI QA");
            SterpSession.Current.UserCode = user;
            SterpSession.Current.CompanyCode = company;
            SterpSession.Current.SiteCode = company == "STC" ? "RDG" : "GLA";
            SterpSession.Current.BusinessDate = new DateTime(2026, 10, 12);
            SterpSession.Current.IsSysAdm = false;
            SterpSession.Current.PermissionCache.Clear();
            var ping = CommArea.For("PINGCA");
            Ok(CobolGateway.Link("PING", ping, "HEALTH"));
            St.Eq(database, ping.GetString("PG-DB-NAME"), "both engines use the isolated VAT database");
        }

        private static void Ok(CobolResult result) { St.True(result.Succeeded, result.Display); }

        private static string Fixture(decimal sales = 1000m, decimal purchases = 200m,
            string company = "STC", string period = null)
        {
            Context("VATOWNER", company);
            period = period ?? "V" + Guid.NewGuid().ToString("N").Substring(0, 8).ToUpperInvariant();
            SqlHelper.ExecuteNonQuery(
                "INSERT fin.VatPeriod (CompanyCode,VatPeriodKey,VatRegNo,StartDate,EndDate,FilingDueDate,PaymentDueDate) " +
                "VALUES (@c,@p,@v,'2098-01-01','2098-03-31','2098-05-07','2098-05-07'); " +
                "INSERT fin.VatTransaction (CompanyCode,SourceModule,DocumentType,DocumentRef,TaxPointDate,PostingDate," +
                "VatPeriodKey,VatCode,RatePct,Direction,NetAmount,VatAmount,NetAmountGBP,VatAmountGBP,RecoverableVatGBP,VatBox,NetBox) " +
                "VALUES (@c,'AR','INVOICE',@p+'S','2098-02-01','2098-02-01',@p,'S',20,'OUTPUT',@s,@s/5,@s,@s/5,0,1,6)," +
                "(@c,'AP','INVOICE',@p+'P','2098-02-01','2098-02-01',@p,'S',20,'INPUT',@b,@b/5,@b,@b/5,@b/5,4,7)",
                "@c", company, "@p", period, "@v", company == "STC" ? "123456789" : "987654321",
                "@s", sales, "@b", purchases);
            return period;
        }

        private static string[] Files()
        {
            string directory = Path.Combine(SterpConfig.RunDir, @"work\vat");
            return Directory.Exists(directory) ? Directory.GetFiles(directory) : new string[0];
        }

        private static void Rejected(Action action)
        {
            bool rejected = false;
            try { action(); }
            catch (UnauthorizedAccessException) { rejected = true; }
            catch (InvalidOperationException) { rejected = true; }
            St.True(rejected, "invalid VAT export must be rejected");
        }

        private static void CheckFile(string path, string company, string period, string vrn, decimal[] boxes)
        {
            string text = File.ReadAllText(path);
            St.True(text.Contains("LOCAL VAT HAND-OFF (NOT TRANSMITTED)"), "local-only header");
            St.True(text.Contains("COMPANY   " + company), "selected company");
            St.True(text.Contains("PERIODKEY " + period), "selected period");
            St.True(text.Contains("VRN       " + vrn), "frozen VRN");
            St.True(text.Contains("PERIOD    " + Fmt.Date(new DateTime(2098, 1, 1)) + " TO " +
                Fmt.Date(new DateTime(2098, 3, 31))), "selected return dates");
            for (int i = 0; i < 9; i++)
                St.True(text.Contains("BOX" + (i + 1) + "      " + boxes[i].ToString("0.00", Fmt.UK).PadLeft(18)),
                    "frozen box " + (i + 1));
            var row = SqlHelper.ExecuteRow(
                "SELECT Status,SubmissionRef,HmrcResponse FROM fin.VatReturn WHERE CompanyCode=@c AND VatPeriodKey=@p",
                "@c", company, "@p", period);
            St.Eq("SUBMITTED", Fmt.Str(row["Status"]), "native local filing record");
            St.Eq(Path.GetFileName(path), Fmt.Str(row["SubmissionRef"]), "local file reference retained");
            St.Eq("LOCAL FILE ONLY; NOT TRANSMITTED", Fmt.Str(row["HmrcResponse"]), "not an HMRC response");
            File.Delete(path);
        }

        [StTest("vat workflow: exact frozen company period VRN nine boxes despite stale caches", Reqs = "FIN-018,FIN-025,FIN-026,COM-002")]
        public static void FrozenLocalFile()
        {
            string first = Fixture();
            var service = new VatService();
            Ok(service.Prepare(first, "INVOICE"));
            St.Eq(160m, service.BoxValue(5), "prepared net VAT");
            Ok(service.Drill(first, 6));
            St.True(service.Drilled != null && service.Drilled.Rows.Count == 1, "preparer drill retained");
            string second = Fixture(3000m, 300m);
            Ok(service.Prepare(second, "INVOICE"));
            Context("FINCTRL");
            St.True(!service.CanAct("FIN.VAT.PREPARE"), "SOD preparer privilege remains absent");
            Ok(service.Record("FINALISE", first, "", ""));
            St.Eq("FINALISED", service.ReturnStatus, "native finalised state");
            St.Eq(160m, service.BoxValue(5), "finalise returns persisted authoritative figures");
            Ok(service.Record("FINALISE", second, "", ""));
            service.LoadReturn(first); // deliberately cache the wrong period
            // Change live sources and period VRN: frozen return boxes and VRN must win.
            SqlHelper.ExecuteNonQuery(
                "UPDATE fin.VatTransaction SET VatAmountGBP=99999,NetAmountGBP=99999 WHERE CompanyCode='STC' AND VatPeriodKey=@p; " +
                "UPDATE fin.VatPeriod SET VatRegNo='111111111' WHERE CompanyCode='STC' AND VatPeriodKey=@p",
                "@p", second);
            CheckFile(service.WriteSubmissionFile(second), "STC", second, "123456789",
                new[] { 600m, 0m, 600m, 60m, 540m, 3000m, 300m, 0m, 0m });
            Fixture(500m, 100m, "CAL", first);
            Ok(service.Prepare(first, "INVOICE"));
            Context("FINCTRL", "CAL");
            Ok(service.Record("FINALISE", first, "", ""));
            Context("FINCTRL"); service.LoadReturn(first);
            Context("FINCTRL", "CAL"); // deliberately cache a different company with the same period
            CheckFile(service.WriteSubmissionFile(first), "CAL", first, "987654321",
                new[] { 100m, 0m, 100m, 20m, 80m, 500m, 100m, 0m, 0m });
        }

        [StTest("vat workflow: unauthorised draft missing and already recorded exports create no files", Reqs = "FIN-018,COM-002")]
        public static void NoRejectedFiles()
        {
            string period = Fixture();
            var service = new VatService();
            Ok(service.Prepare(period, "INVOICE"));
            string[] before = Files();
            Context("FINCTRL");
            Rejected(() => service.WriteSubmissionFile(period));
            Rejected(() => service.WriteSubmissionFile("MISSING"));
            St.True(before.OrderBy(x => x).SequenceEqual(Files().OrderBy(x => x)), "no draft/missing files");
            Ok(service.Record("FINALISE", period, "", ""));
            Context("GLCLERK");
            St.True(!service.CanAct("FIN.VAT.SUBMIT"), "negative user has no submit authority");
            Rejected(() => service.WriteSubmissionFile(period));
            St.True(before.OrderBy(x => x).SequenceEqual(Files().OrderBy(x => x)), "no unauthorised files");
            Context("FINCTRL");
            string path = service.WriteSubmissionFile(period);
            Rejected(() => service.WriteSubmissionFile(period));
            St.Eq(before.Length + 1, Files().Length, "no duplicate submitted file");
            File.Delete(path);
        }

        private static T Field<T>(object target, string name)
        {
            var flags = BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public;
            var field = target.GetType().GetField(name, flags) ?? target.GetType().GetField("_" + name, flags);
            return (T)field.GetValue(target);
        }

        private static IEnumerable<ToolStripMenuItem> MenuItems(ToolStripItemCollection items)
        {
            foreach (var item in items.OfType<ToolStripMenuItem>())
            {
                yield return item;
                foreach (var child in MenuItems(item.DropDownItems)) yield return child;
            }
        }

        [StTest("vat workflow GUI: independent controller menu routes load saved return without PREPARE", Reqs = "FIN-018,COM-002")]
        public static void ReviewerGui()
        {
            string period = Fixture();
            Ok(new VatService().Prepare(period, "INVOICE"));
            Context("FINCTRL");
            var menus = new MenuService().Load();
            foreach (string code in new[] { "FIN.VAT.REVIEW", "FIN.VAT.LOCALFILE" })
                St.True(menus.Rows.Cast<DataRow>().Any(r => Fmt.Str(r["MenuCode"]) == code &&
                    Fmt.Str(r["FormClass"]) == "frmVatReturn"), "controller menu route " + code);
            St.True(!menus.Rows.Cast<DataRow>().Any(r => Fmt.Str(r["MenuCode"]) == "FIN.VAT.RETURN"),
                "no preparer-only menu");
            Exception failure = null;
            var thread = new Thread(() =>
            {
                try
                {
                    var assembly = Assembly.LoadFrom(Path.Combine(SterpConfig.BinDir, "STERP.exe"));
                    using (Form main = (Form)Activator.CreateInstance(assembly.GetType("STERP.Desktop.frmMain", true)))
                    {
                        main.Show(); Application.DoEvents();
                        foreach (string code in new[] { "FIN.VAT.REVIEW", "FIN.VAT.LOCALFILE" })
                        {
                            var menu = MenuItems(Field<MenuStrip>(main, "mnuBar").Items).Single(item =>
                                item.Tag is DataRow && Fmt.Str(((DataRow)item.Tag)["MenuCode"]) == code);
                            menu.PerformClick(); Application.DoEvents();
                            var child = main.MdiChildren.Single(f => f.GetType().Name == "frmVatReturn");
                            St.True(!Field<Button>(child, "_prepareButton").Enabled, code + " opens reviewer UI");
                            child.Close();
                        }
                    }
                    var type = assembly.GetType("STERP.Desktop.frmVatReturn", true);
                    using (Form form = (Form)Activator.CreateInstance(type))
                    {
                        type.GetProperty("FormArgument").SetValue(form, period, null);
                        form.Show(); Application.DoEvents();
                        St.True(!Field<Button>(form, "_prepareButton").Enabled, "no preparer action offered");
                        St.True(Field<Button>(form, "_finaliseButton").Enabled, "finalise enabled");
                        St.True(Field<Button>(form, "_fileButton").Enabled, "local file enabled");
                        var service = Field<VatService>(form, "_svc");
                        St.Eq("DRAFT", service.ReturnStatus, "review loads prepared persisted draft");
                        St.Eq(Fmt.Money(160m), Field<TextBox[]>(form, "_box")[4].Text, "review displays saved boxes");
                        Ok(service.Record("FINALISE", period, "", ""));
                        form.Controls.OfType<Button>().Single(b => b.Text.Contains("Load saved")).PerformClick();
                        St.Eq("FINALISED", service.ReturnStatus, "saved finalised return displayed without PREPARE");
                        St.Eq(Fmt.Money(160m), Field<TextBox[]>(form, "_box")[4].Text, "frozen net VAT displayed");
                        Field<TextBox>(form, "_txtPeriod").Text = "MISSING";
                        St.True(Field<TextBox[]>(form, "_box").All(box => box.Text == ""),
                            "changing period clears stale displayed boxes");
                        St.True(!service.Prepare(period, "INVOICE").Succeeded, "native preparer authority still denied");
                    }
                }
                catch (Exception ex) { failure = ex; }
            });
            thread.SetApartmentState(ApartmentState.STA); thread.IsBackground = true; thread.Start();
            St.True(thread.Join(TimeSpan.FromSeconds(40)), "reviewer GUI timed out");
            if (failure != null) throw failure;
        }
    }
}
