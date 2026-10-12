using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using STERP.Business;
using STERP.CobolInterop;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests
{
    /// <summary>AP GUI actions and real engine controls; writes require an isolated AP QA database.</summary>
    public static class ApWorkflowGuiTests
    {
        internal static void Context(string user)
        {
            string database = Fmt.Str(SqlHelper.ExecuteScalar("SELECT DB_NAME()"));
            if (!database.StartsWith("STERP_QA_AP_", StringComparison.OrdinalIgnoreCase))
                St.Skip("writing tests require a dedicated STERP_QA_AP_ database, not production/shared GUI QA");
            SterpSession.Current.CompanyCode = "STC";
            SterpSession.Current.SiteCode = "RDG";
            SterpSession.Current.UserCode = user;
            SterpSession.Current.BusinessDate = new DateTime(2026, 10, 12);
            var ping = CommArea.For("PINGCA");
            var result = CobolGateway.Link("PING", ping, "HEALTH");
            St.True(result.Succeeded, result.Display);
            St.Eq(database, ping.GetString("PG-DB-NAME"), ".NET and COBOL use the same isolated database");
        }

        private static void Ok(CobolResult result)
        {
            St.True(result.Succeeded, result.Display);
        }

        private static string Ref(string prefix)
        {
            return prefix + Guid.NewGuid().ToString("N").Substring(0, 16).ToUpperInvariant();
        }

        private static object Call(object target, string name, params object[] args)
        {
            for (Type type = target.GetType(); type != null; type = type.BaseType)
            {
                var method = type.GetMethod(name, BindingFlags.Instance | BindingFlags.NonPublic |
                    BindingFlags.Public | BindingFlags.DeclaredOnly);
                if (method != null) return method.Invoke(target, args);
            }
            throw new MissingMethodException(name);
        }

        private static T Field<T>(object target, string name)
        {
            for (Type type = target.GetType(); type != null; type = type.BaseType)
            {
                var flags = BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public;
                var field = type.GetField(name, flags) ?? type.GetField("_" + name, flags);
                if (field != null) return (T)field.GetValue(target);
            }
            throw new MissingFieldException(name);
        }

        private static Form Screen(string name)
        {
            return (Form)Activator.CreateInstance(Assembly.LoadFrom(
                Path.Combine(SterpConfig.BinDir, "STERP.exe")).GetType("STERP.Desktop." + name, true));
        }

        private static void Sta(Action action)
        {
            Exception failure = null;
            var thread = new Thread(() => { try { action(); } catch (Exception ex) { failure = ex; } });
            thread.IsBackground = true;
            thread.SetApartmentState(ApartmentState.STA);
            thread.Start();
            St.True(thread.Join(TimeSpan.FromSeconds(60)), "GUI test timed out (check for a surfaced modal error)");
            if (failure != null) throw failure;
        }

        private delegate bool WindowVisitor(IntPtr window, IntPtr parameter);
        [DllImport("kernel32.dll")]
        private static extern uint GetCurrentThreadId();
        [DllImport("user32.dll")]
        private static extern bool EnumThreadWindows(uint thread, WindowVisitor visitor, IntPtr parameter);
        [DllImport("user32.dll", CharSet = CharSet.Unicode)]
        private static extern int GetWindowText(IntPtr window, StringBuilder text, int length);
        [DllImport("user32.dll")]
        private static extern IntPtr SendMessage(IntPtr window, uint message, IntPtr wparam, IntPtr lparam);
        private static void AcknowledgeOwnedWarning(uint thread)
        {
            EnumThreadWindows(thread, (window, parameter) =>
            {
                var title = new StringBuilder(100);
                GetWindowText(window, title, title.Capacity);
                if (title.ToString() == "STERP - Warning")
                    SendMessage(window, 0x10, IntPtr.Zero, IntPtr.Zero);
                return true;
            }, IntPtr.Zero);
        }

        [StTest("ap workflow GUI: Bacs catalogue and actual launcher expose only company and stored run",
            Reqs = "AP-013,AP-026,COM-017")]
        public static void BacsLauncherParameterContract()
        {
            Context("TREASURY");
            var definition = SqlHelper.ExecuteRow(
                "SELECT ProgramName,ParamSpec,PermissionCode FROM core.ReportDef WHERE ReportCode='APBACS'");
            St.True(definition != null, "seeded Bacs catalogue definition exists");
            St.Eq("APBACS", Fmt.Str(definition["ProgramName"]), "native program contract");
            St.Eq("CO,RUN", Fmt.Str(definition["ParamSpec"]), "catalogue offers no ignored ENTRYDATE override");
            St.Eq("AP.PAYMENT.RELEASE", Fmt.Str(definition["PermissionCode"]), "release authority is not weakened");
            Sta(() =>
            {
                using (Form launcher = Screen("frmReportLauncher"))
                {
                    launcher.GetType().GetProperty("FormArgument").SetValue(launcher, "APBACS", null);
                    var initial = Field<Dictionary<string, string>>(launcher, "InitialParameters");
                    initial["RUN"] = "PPS00000002";
                    launcher.Show();
                    Application.DoEvents();
                    var parameters = Field<Dictionary<string, TextBox>>(launcher, "_params");
                    St.Eq("CO,RUN", string.Join(",", parameters.Keys.OrderBy(k => k)), "actual GUI prompts match native input contract");
                    St.Eq("STC", parameters["CO"].Text, "company is prefilled");
                    St.Eq("PPS00000002", parameters["RUN"].Text, "stored proposal reference is prefilled");
                }
            });
        }

        private static IEnumerable<ToolStripMenuItem> MenuItems(ToolStripItemCollection items)
        {
            foreach (ToolStripMenuItem item in items.OfType<ToolStripMenuItem>())
            {
                yield return item;
                foreach (var child in MenuItems(item.DropDownItems)) yield return child;
            }
        }

        [StTest("ap workflow GUI: payment menus reach the real form without granting proposal authority", Reqs = "AP-011,AP-012,AP-013,COM-002")]
        public static void PaymentMenus()
        {
            foreach (string user in new[] { "APCLERK", "TREASURY", "TREASURY2" })
            {
                Context(user);
                var menus = new MenuService().Load();
                string[] expected = user == "APCLERK" ? new[] { "AP.PAYMENT.RUN" } :
                    new[] { "AP.PAYMENT.AUTH", "AP.PAYMENT.REL", "AP.SUPPLIER.BANKAPP" };
                foreach (string code in expected)
                {
                    string formClass = code == "AP.SUPPLIER.BANKAPP" ? "frmSupplierVerify" : "frmPaymentRun";
                    St.True(menus.Rows.Cast<DataRow>().Any(row => Fmt.Str(row["MenuCode"]) == code &&
                        Fmt.Str(row["FormClass"]) == formClass), user + " has permitted route " + code);
                    Sta(() =>
                    {
                        using (Form main = Screen("frmMain"))
                        {
                            main.Show();
                            var menu = MenuItems(Field<MenuStrip>(main, "mnuBar").Items).Single(item =>
                                item.Tag is DataRow && Fmt.Str(((DataRow)item.Tag)["MenuCode"]) == code);
                            menu.PerformClick();
                            St.True(main.MdiChildren.Any(child => child.GetType().Name == formClass),
                                user + " opens the actual form through " + code);
                        }
                    });
                }
                if (user != "APCLERK")
                {
                    St.True(!menus.Rows.Cast<DataRow>().Any(row => Fmt.Str(row["MenuCode"]) == "AP.PAYMENT.RUN"),
                        user + " is not granted the preparation menu");
                    int before = Fmt.Int(SqlHelper.ExecuteScalar("SELECT COUNT(*) FROM ap.PaymentProposal"));
                    var refused = new PaymentService().Select_("", new DateTime(2026, 10, 12),
                        new DateTime(2026, 10, 14), "BACS", "GBP1");
                    St.True(!refused.Succeeded, user + " still cannot prepare a proposal");
                    St.Eq(before, Fmt.Int(SqlHelper.ExecuteScalar("SELECT COUNT(*) FROM ap.PaymentProposal")),
                        "refused preparation created no proposal");
                }
            }
            Context("APMGR");
            St.True(!new MenuService().Load().Rows.Cast<DataRow>().Any(row =>
                Fmt.Str(row["FormClass"]) == "frmPaymentRun"), "AP enquiry permission alone does not open payment runs");
        }

        [StTest("ap workflow GUI: bank change, callback, proposal and file actions are wired", Reqs = "AP-002,AP-011,AP-013,AP-027")]
        public static void Actions()
        {
            Sta(() =>
            {
                foreach (var screen in new[] {
                    new[] { "frmSupplierVerify", "Change bank", "CoP check", "Record callback", "Approve bank" },
                    new[] { "frmPaymentRun", "Build proposal", "Reserve", "Release reservations", "Request approval",
                        "Approve", "Create instructions", "Bacs file" } })
                {
                    using (Form form = Screen(screen[0]))
                    {
                        Call(form, "BuildTools");
                        var tools = Field<ToolStrip>(form, "Tools");
                        foreach (string text in screen.Skip(1))
                            St.True(tools.Items.Cast<ToolStripItem>().Any(item => item.Text == text),
                                screen[0] + " has " + text);
                    }
                }
            });
        }

        [StTest("ap workflow GUI: bank evidence preserves pending state and permission boundaries", Reqs = "AP-002,AP-027,D-19")]
        public static void BankEvidence()
        {
            Context("APCLERK");
            var svc = new SupplierService();
            string bankRef = Ref("B").Substring(0, 10);
            var denied = svc.PrepareBank("S0013", bankRef, "Fictional QA Logistics", "112233",
                "12345678", "Fictional QA Bank", "LETTER");
            St.True(!denied.Succeeded, "APCLERK has no bank-maintenance permission");
            Context("APMGR");
            Ok(svc.PrepareBank("S0013", bankRef, "Fictional QA Logistics", "112233",
                "12345678", "Fictional QA Bank", "LETTER"));
            int id = svc.BankId;
            St.True(id > 0, "PREPARE returns its bank ID");
            St.Eq("PENDING", svc.BankStatus, "new destination is not usable");
            St.True(!svc.ApproveBank("S0013", id, "APPROVE").Succeeded, "maintainer cannot approve");
            St.True(!svc.ConfirmationOfPayee("S0013", id, "NOMATCH", "QA-COP", "").Succeeded,
                "nonmatch still requires an authorised decision");

            Context("SYSADM");
            Ok(svc.ConfirmationOfPayee("S0013", id, "MATCH", "QA-COP", ""));
            St.True(!svc.RecordCallback("S0013", id, "Contact from change email", false, "QA").Succeeded,
                "a contact supplied with the request cannot verify the change");
            Ok(svc.RecordCallback("S0013", id, "Supplier master contact held before request", true,
                "Independent fictional QA callback confirmed destination"));
            St.Eq("VERIFIED", svc.BankStatus, "callback is evidence, not approval");
            St.Eq("SYSADM", Fmt.Str(SqlHelper.ExecuteScalar(
                "SELECT CallBackVerifiedBy FROM ap.SupplierBank WHERE SupplierBankId=@i", "@i", id)),
                "existing SYSADM verifies independently of APMGR without a new user or role grant");
            Context("TREASURY");
            Ok(svc.ApproveBank("S0013", id, "APPROVE"));
            St.Eq("APPROVED", svc.BankStatus, "the independent decision uses the engine contract");
            St.Eq("TREASURY", Fmt.Str(SqlHelper.ExecuteScalar(
                "SELECT ApprovedBy FROM ap.SupplierBank WHERE SupplierBankId=@i", "@i", id)), "approver identity");
        }

        [StTest("ap workflow GUI: bank identifiers and evidence refuse native-width overflow without writing", Reqs = "AP-002,AP-027,COM-017")]
        public static void BankInputWidths()
        {
            Context("APMGR");
            var svc = new SupplierService();
            string reference = Ref("B").Substring(0, 10);
            int before = Fmt.Int(SqlHelper.ExecuteScalar("SELECT COUNT(*) FROM ap.SupplierBank"));
            int auditBefore = Fmt.Int(SqlHelper.ExecuteScalar("SELECT COUNT(*) FROM core.AuditLog"));
            var refused = svc.PrepareBank("S0002", reference + "X", "Fictional QA", "112233",
                "12345678", "Fictional QA Bank", "LETTER");
            St.True(!refused.Succeeded, "11-character bank reference is explicitly refused");
            St.Eq("APBINPUT", refused.ReasonCode, "refusal is a displayable input error");
            St.Contains("10 characters", refused.Display, "native bank-reference width is explained");
            St.Eq(before, Fmt.Int(SqlHelper.ExecuteScalar("SELECT COUNT(*) FROM ap.SupplierBank")),
                "overlength bank reference created no record");
            St.Eq(auditBefore, Fmt.Int(SqlHelper.ExecuteScalar("SELECT COUNT(*) FROM core.AuditLog")),
                "refusal did not call the engine or create an audit event");

            Ok(svc.PrepareBank("S0002", reference, "Fictional QA", "112233",
                "12345678", "Fictional QA Bank", "LETTER"));
            int id = svc.BankId;
            St.Eq(reference, Fmt.Str(SqlHelper.ExecuteScalar(
                "SELECT BankRef FROM ap.SupplierBank WHERE SupplierBankId=@i", "@i", id)),
                "10-character key is accepted and stored unchanged");
            St.Eq("PENDING", svc.BankStatus, "valid-width details still need verification");
            int count = Fmt.Int(SqlHelper.ExecuteScalar("SELECT COUNT(*) FROM ap.SupplierBank"));
            auditBefore = Fmt.Int(SqlHelper.ExecuteScalar("SELECT COUNT(*) FROM core.AuditLog"));
            St.True(!svc.PrepareBank("S0002", Ref("B").Substring(0, 10), new string('N', 121),
                "112233", "12345678", "Fictional QA Bank", "LETTER").Succeeded, "beneficiary overflow refused");
            St.True(!svc.PrepareBank("S0002", Ref("B").Substring(0, 10), "Fictional QA",
                "1122337", "12345678", "Fictional QA Bank", "LETTER").Succeeded, "sort-code overflow refused");
            St.True(!svc.PrepareBank("S0002", Ref("B").Substring(0, 10), "Fictional QA",
                "112233", "123456789", "Fictional QA Bank", "LETTER").Succeeded, "account overflow refused");
            St.True(!svc.PrepareBank("S0002", Ref("B").Substring(0, 10), "Fictional QA",
                "112233", "12345678", new string('N', 61), "LETTER").Succeeded, "bank-name overflow refused");
            St.True(!svc.PrepareBank("S0002", Ref("B").Substring(0, 10), "Fictional QA",
                "112233", "12345678", "Fictional QA Bank", new string('N', 31)).Succeeded, "source overflow refused");
            St.True(!svc.ConfirmationOfPayee("S0002", id, "MATCH", new string('R', 61), "").Succeeded,
                "CoP reference overflow refused");
            St.True(!svc.ConfirmationOfPayee("S0002", id, "MATCH", "QA-COP", new string('D', 201)).Succeeded,
                "CoP decision overflow refused");
            St.True(!svc.RecordCallback("S0002", id, new string('C', 121), true, "QA").Succeeded,
                "callback contact overflow refused");
            St.True(!svc.RecordCallback("S0002", id, "Existing contact", true, new string('N', 301)).Succeeded,
                "callback note overflow refused");
            St.Eq(count, Fmt.Int(SqlHelper.ExecuteScalar("SELECT COUNT(*) FROM ap.SupplierBank")),
                "coupled overlength fields created no records");
            St.Eq(auditBefore, Fmt.Int(SqlHelper.ExecuteScalar("SELECT COUNT(*) FROM core.AuditLog")),
                "coupled overlength fields caused no audited writes");

            var dialogType = Assembly.LoadFrom(Path.Combine(SterpConfig.BinDir, "STERP.exe"))
                .GetType("STERP.Desktop.ApInputDialog", true);
            var validate = dialogType.GetMethod("WidthError", BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic);
            var fields = new[] { new[] { "BANKREF", "Bank reference", "", "10" } };
            var values = new Dictionary<string, string> { { "BANKREF", reference } };
            St.Eq("", (string)validate.Invoke(null, new object[] { fields, values }), "dialog accepts native-width key");
            values["BANKREF"] = reference + "X";
            St.Contains("10 characters", (string)validate.Invoke(null, new object[] { fields, values }),
                "dialog explicitly refuses programmatic input beyond the textbox bound");
            St.Eq(reference + "X", values["BANKREF"], "dialog does not silently shorten the requested key");
        }

        [StTest("ap workflow GUI: reserve and release retain the existing proposal header", Reqs = "AP-011,AP-020")]
        public static void Reservation()
        {
            Context("APCLERK");
            var svc = new PaymentService();
            string reference = Ref("APRES");
            Ok(svc.Select_(reference, new DateTime(2026, 10, 12), new DateTime(2026, 10, 14), "BACS", "GBP1"));
            reference = svc.ProposalReference;
            int id = Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT ProposalId FROM ap.PaymentProposal WHERE ProposalRef=@r", "@r", reference));
            Ok(svc.Reserve(reference));
            St.True(Fmt.Dec(SqlHelper.ExecuteScalar(
                "SELECT ReservedTotalGBP FROM ap.PaymentProposal WHERE ProposalId=@i", "@i", id)) > 0,
                "the due items are reserved");
            Ok(svc.Release(reference));
            St.Eq(0m, Fmt.Dec(SqlHelper.ExecuteScalar(
                "SELECT ReservedTotalGBP FROM ap.PaymentProposal WHERE ProposalId=@i", "@i", id)), "reservations freed");
            St.Eq(1, Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT COUNT(*) FROM ap.PaymentProposal WHERE ProposalRef=@r", "@r", reference)), "no duplicate header");
            St.Eq(new DateTime(2026, 10, 14), Convert.ToDateTime(SqlHelper.ExecuteScalar(
                "SELECT PayDate FROM ap.PaymentProposal WHERE ProposalId=@i", "@i", id)), "stored payment date retained");
            St.True(!svc.Reserve(Ref("MISSING")).Succeeded, "missing proposal is a surfaced error");
        }

        [StTest("ap workflow GUI: treasury approvals, instructions and local Bacs launcher", Reqs = "AP-012,AP-013,AP-026,D-07")]
        public static void TreasuryAndBacs()
        {
            RunTreasuryAndBacs(new DateTime(2026, 10, 12), new DateTime(2026, 10, 14));
        }

        internal static void RunTreasuryAndBacs(DateTime cutoff, DateTime payDate, bool guiBuild = false)
        {
            Context("APCLERK");
            var svc = new PaymentService();
            string reference = Ref("APPAY");
            if (guiBuild)
            {
                Sta(() =>
                {
                    using (Form payment = Screen("frmPaymentRun"))
                    using (var timer = new System.Windows.Forms.Timer { Interval = 100 })
                    using (var finished = new ManualResetEvent(false))
                    {
                        payment.Show();
                        Application.DoEvents();
                        uint guiThread = GetCurrentThreadId();
                        var warnings = new Thread(() =>
                        {
                            while (!finished.WaitOne(100)) AcknowledgeOwnedWarning(guiThread);
                        }) { IsBackground = true };
                        warnings.Start();
                        timer.Tick += (sender, args) =>
                        {
                            Form input = Application.OpenForms.Cast<Form>().FirstOrDefault(
                                f => f.Text == "Build new payment proposal (preview)");
                            if (input == null) return;
                            timer.Stop();
                            input.Controls.Find("CUTOFF", true).OfType<TextBox>().Single().Text = cutoff.ToString("dd/MM/yyyy");
                            input.Controls.Find("PAYDATE", true).OfType<TextBox>().Single().Text = payDate.ToString("dd/MM/yyyy");
                            ((Button)input.AcceptButton).PerformClick();
                        };
                        timer.Start();
                        try { Call(payment, "BuildClicked", null, EventArgs.Empty); }
                        finally { finished.Set(); warnings.Join(1000); }
                        reference = Field<TextBox>(payment, "_txtProposal").Text;
                        St.True(reference.StartsWith("PPS"), "real keyed GUI proposal dialog returns the native reference");
                    }
                });
            }
            else
            {
                Ok(svc.Select_(reference, cutoff, payDate, "BACS", "GBP1"));
                reference = svc.ProposalReference;
            }
            Ok(svc.Reserve(reference));
            Ok(svc.Totals(reference));
            St.True(svc.TotalOutflow > 0m, "the run contains payable items");
            Ok(svc.RequestApproval(reference, svc.TotalOutflow));
            St.True(!svc.Decide(reference, "APPROVE", "Self", svc.TotalOutflow).Succeeded, "preparer approval refused");
            St.True(!svc.CreateInstructions(reference).Succeeded, "clerk cannot release instructions");
            Context("TREASURY");
            Ok(svc.Decide(reference, "APPROVE", "First independent authoriser", svc.TotalOutflow));
            St.True(!svc.CreateInstructions(reference).Succeeded, "one signature cannot create instructions");
            St.True(!svc.Decide(reference, "APPROVE", "Duplicate", svc.TotalOutflow).Succeeded, "duplicate authoriser refused");
            Context("TREASURY2");
            Ok(svc.Decide(reference, "APPROVE", "Second independent authoriser", svc.TotalOutflow));
            Context("TREASURY");
            Ok(svc.CreateInstructions(reference));
            St.True(svc.InstructionCount > 0, "approved run produced instructions");

            Sta(() =>
            {
                using (Form payment = Screen("frmPaymentRun"))
                {
                    Call(payment, "BuildStatusLine");
                    Call(payment, "BuildFilters");
                    Field<TextBox>(payment, "_txtProposal").Text = reference;
                    Call(payment, "BacsClicked", null, EventArgs.Empty);
                    Form launcher = Application.OpenForms.Cast<Form>().Single(f => f.Name == "frmReportLauncher");
                    using (launcher)
                    {
                        var parameters = Field<Dictionary<string, TextBox>>(launcher, "_params");
                        St.Eq(reference, parameters["RUN"].Text, "launcher carries the selected proposal");
                        string bacsDir = Path.Combine(SterpConfig.RunDir, "bacs");
                        Directory.CreateDirectory(bacsDir);
                        Directory.CreateDirectory(Path.Combine(SterpConfig.RunDir, "spool"));
                        var before = Directory.Exists(bacsDir) ? Directory.GetFiles(bacsDir) : new string[0];
                        Call(launcher, "RunClicked", null, EventArgs.Empty);
                        St.True(!Field<TextBox>(launcher, "txtLog").Text.Contains("STERP-E-"),
                            "signed-on treasury user, not APBACS default APMGR: " + Field<TextBox>(launcher, "txtLog").Text);
                        var created = Directory.GetFiles(bacsDir).Except(before).ToArray();
                        St.True(created.Length > 0, "report launcher produced a new local Bacs file");
                        St.True(created.Any(file => new FileInfo(file).Length > 0), "local file is nonempty");
                    }
                }
            });
        }
    }
}
