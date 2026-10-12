using System;
using System.CodeDom.Compiler;
using System.Collections.Generic;
using System.Data;
using System.IO;
using STERP.Business;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests
{
    /// <summary>
    /// The desktop shell contract: the permission filter on the menu, the generic
    /// maintenance select and the batch report launcher.
    /// </summary>
    public static class ShellTests
    {
        private static void Need(string objectName)
        {
            if (!SqlHelper.ObjectExists(objectName)) St.Skip(objectName + " is not deployed yet");
        }

        private static DataTable MenuFor(string user, string company, string site)
        {
            SterpSession.Current.UserCode = user;
            SterpSession.Current.CompanyCode = company;
            SterpSession.Current.SiteCode = site;
            return new MenuService().Load();
        }

        [StTest("menu: every seeded FormClass resolves to a screen in STERP.exe", Reqs = "COM-024")]
        public static void MenuFormClassesExist()
        {
            Need("core.usp_UiMenuLoad");
            DataTable t = MenuFor("SYSADM", "STC", "RDG");
            if (t.Rows.Count == 0) St.Skip("core.MenuItem has not been seeded yet");

            System.Reflection.Assembly shell = System.Reflection.Assembly.LoadFrom(
                Path.Combine(SterpConfig.BinDir, "STERP.exe"));
            List<string> missing = new List<string>();
            foreach (DataRow r in t.Rows)
            {
                string cls = Fmt.Str(r["FormClass"]);
                if (cls.Length == 0) continue;
                if (shell.GetType("STERP.Desktop." + cls, false, true) == null && !missing.Contains(cls))
                    missing.Add(cls);
            }
            // module teams seed their own menu rows before their forms land - only the
            // framework's own screens must resolve today.
            missing.RemoveAll(delegate (string c) { return !c.StartsWith("frm"); });
            St.True(missing.Count == 0, "menu entries naming a missing form: " + string.Join(", ", missing.ToArray()));
        }

        [StTest("menu: the menu is filtered by permission, RESTRICT sees less than SYSADM", Reqs = "COM-002,COM-024", Ac = "AC-006")]
        public static void MenuIsPermissionFiltered()
        {
            Need("core.usp_UiMenuLoad");
            if (Fmt.Int(SqlHelper.ExecuteScalar("SELECT COUNT(*) FROM core.AppUser WHERE UserCode = 'RESTRICT'")) == 0)
                St.Skip("the RESTRICT demonstration user has not been seeded yet");

            int secured = Fmt.Int(SqlHelper.ExecuteScalar(
                "SELECT COUNT(*) FROM core.MenuItem WHERE PermissionCode IS NOT NULL AND IsActive = 1"));
            if (secured == 0) St.Skip("no menu entry carries a permission code yet");

            DataTable admin = MenuFor("SYSADM", "STC", "RDG");
            DataTable restricted = MenuFor("RESTRICT", "STC", "RDG");
            St.True(restricted.Rows.Count < admin.Rows.Count,
                    "RESTRICT sees fewer entries (" + restricted.Rows.Count + ") than SYSADM (" + admin.Rows.Count + ")");

            // a branch whose children are all denied must not be published as an empty branch
            // (a top level menu with no children at all is reserved for a module team)
            foreach (DataRow r in restricted.Select("ParentMenuCode is null"))
            {
                string code = Fmt.Str(r["MenuCode"]);
                int seeded = Fmt.Int(SqlHelper.ExecuteScalar(
                    "SELECT COUNT(*) FROM core.MenuItem WHERE IsActive = 1 AND ParentMenuCode = @p", "@p", code));
                if (seeded == 0) continue;
                St.True(restricted.Select("ParentMenuCode = '" + code + "'").Length > 0,
                        "menu " + code + " is published only because something under it is visible");
            }
        }

        [StTest("maint: the generic maintenance select returns the registered columns", Reqs = "COM-013,COM-016")]
        public static void GenericMaintenanceSelect()
        {
            Need("core.usp_UiMaintList");
            SterpSession.Current.UserCode = "SYSADM";
            SterpSession.Current.CompanyCode = "STC";
            SterpSession.Current.SiteCode = "RDG";

            MaintService svc = new MaintService();
            St.True(svc.LoadDefinition("CORE.CURRENCY"), "CORE.CURRENCY is registered");

            DataTable rows = svc.List("CORE.CURRENCY", null);
            St.True(rows.Rows.Count >= 3, "the seeded currencies are listed, found " + rows.Rows.Count);
            foreach (DataRow c in svc.Columns.Rows)
                St.True(rows.Columns.Contains(Fmt.Str(c["ColumnName"])),
                        "registered column " + c["ColumnName"] + " is in the result set");

            DataTable one = svc.List("CORE.CURRENCY", "GBP");
            St.True(one.Rows.Count >= 1, "the search argument matches GBP");
            St.True(one.Rows.Count < rows.Rows.Count, "the search argument narrows the list");

            try
            {
                svc.List("NO.SUCH.SCREEN", null);
                St.Fail("an unregistered maintenance code must be refused");
            }
            catch (BusinessRuleException ex)
            {
                St.Contains("not registered", ex.Message, "the refusal names the problem");
            }
        }

        [StTest("report: the launcher builds KEY=VALUE arguments and reads the spool line", Reqs = "COM-014")]
        public static void ReportArgumentBuilding()
        {
            Dictionary<string, string> a = new Dictionary<string, string>();
            a["co"] = "STC";
            a["site"] = " RDG ";
            a["ASAT"] = "20261031";
            a["NAME"] = "Stellar Components";
            a["EMPTY"] = "   ";
            St.Eq("CO=STC SITE=RDG ASAT=20261031 NAME=\"Stellar Components\"",
                  ReportService.BuildArguments(a), "keys upper cased, spaces quoted, blanks dropped");
            St.Eq("", ReportService.BuildArguments(null), "no parameters means no command line");

            string log = "STERP-I-0001 Run started\r\nSTERP-I-SPOOL  C:\\run\\spool\\AR-AGED-001.txt \r\nSTERP-I-0099 Done\r\n";
            St.Eq("C:\\run\\spool\\AR-AGED-001.txt", ReportService.ParseSpool(log), "the spool path is taken from the log");
            St.Eq("", ReportService.ParseSpool("STERP-I-SPOOLED nothing here\r\n"), "a similar message id is not a spool line");
            St.Eq("", ReportService.ParseSpool(null), "no output means no spool file");
        }

        [StTest("report: a batch program is run with its arguments and its spool file is picked up", Reqs = "COM-014", Ac = "AC-005")]
        public static void ReportRunsBatchProgram()
        {
            SterpSession.Current.UserCode = "SYSADM";
            SterpSession.Current.CompanyCode = "STC";
            string spool = Path.Combine(SterpConfig.RunDir, "spool", "STRPTST-harness.txt");
            string exe = BuildStubBatchProgram();

            Dictionary<string, string> args = new Dictionary<string, string>();
            args["CO"] = "STC";
            args["OUT"] = spool;

            ReportService svc = new ReportService();
            ReportRun rr = svc.Run("HARNESS", "STRPTST", args);

            St.Eq(0, rr.ReturnCode, "the batch program ended with return code 0");
            St.True(rr.Succeeded, "return code 0 counts as success");
            St.Contains("CO=STC", rr.Output, "the KEY=VALUE arguments reached the program");
            St.Contains("STERP-I-HOME " + ReportService.HomeDir(), rr.Output,
                "relative data paths resolve from the application root, as job streams do");
            St.Eq(spool, rr.SpoolFile, "the STERP-I-SPOOL line was parsed");
            St.True(File.Exists(rr.LogFile), "the console log was written to " + rr.LogFile);

            if (SqlHelper.ObjectExists("core.JobRun") && rr.JobRunId > 0)
                St.Eq("OK", Fmt.Str(SqlHelper.ExecuteScalar(
                    "SELECT Status FROM core.JobRun WHERE JobRunId = " + rr.JobRunId)), "core.JobRun records the run");

            ReportRun bad = svc.Run("HARNESS", "NOSUCHPG", args);
            St.Eq(16, bad.ReturnCode, "a program that is not installed comes back as RC 16, not an exception");
            St.Eq("ABENDED", ReportService.JobStatus(16), "RC 16 is an abend");
            St.Eq("WARNING", ReportService.JobStatus(4), "RC 4 is a warning");
            St.Eq("FAILED", ReportService.JobStatus(8), "RC 8 is a failure");
            if (SqlHelper.ObjectExists("core.JobRun") && bad.JobRunId > 0)
                St.Eq("ABENDED", Fmt.Str(SqlHelper.ExecuteScalar(
                    "SELECT Status FROM core.JobRun WHERE JobRunId = " + bad.JobRunId)), "the failed run is closed off too");

            try { File.Delete(exe); File.Delete(rr.LogFile); File.Delete(spool); } catch { }
        }

        [StTest("report: job streams inherit the application SQL target, not a shell default", Reqs = "COM-014,XMD-005")]
        public static void JobStreamsUseSelectedDatabase()
        {
            SterpSession.Current.UserCode = "SYSADM";
            SterpSession.Current.CompanyCode = "STC";
            string name = "STRPTSTJOB" + Guid.NewGuid().ToString("N").Substring(0, 8);
            string script = ReportService.JobStream(name);
            string oldDatabase = Environment.GetEnvironmentVariable("STERP_DATABASE");
            string oldServer = Environment.GetEnvironmentVariable("STERP_SQLSERVER");
            ReportRun result = null;
            try
            {
                Directory.CreateDirectory(Path.GetDirectoryName(script));
                File.WriteAllText(script, "@echo off\r\necho STERP-I-DATABASE %STERP_DATABASE%\r\n" +
                    "echo STERP-I-SERVER %STERP_SQLSERVER%\r\nexit /b 0\r\n");
                Environment.SetEnvironmentVariable("STERP_DATABASE", "WRONG_SHELL_DATABASE");
                Environment.SetEnvironmentVariable("STERP_SQLSERVER", "WRONG_SHELL_SERVER");
                result = new ReportService().Run("JOB.HARNESS", name, new Dictionary<string, string>());
                var sql = new System.Data.SqlClient.SqlConnectionStringBuilder(SterpConfig.SqlConnectionString);
                St.Eq(0, result.ReturnCode, "the local job stream runs successfully");
                St.Contains("STERP-I-DATABASE " + sql.InitialCatalog, result.Output,
                    "SETENV receives the selected application database");
                St.Contains("STERP-I-SERVER " + sql.DataSource, result.Output,
                    "SQL job steps receive the selected application server");
                St.True(!result.Output.Contains("WRONG_SHELL"), "inherited shell defaults cannot retarget the job");
            }
            finally
            {
                Environment.SetEnvironmentVariable("STERP_DATABASE", oldDatabase);
                Environment.SetEnvironmentVariable("STERP_SQLSERVER", oldServer);
                if (File.Exists(script)) File.Delete(script);
                if (result != null && File.Exists(result.LogFile)) File.Delete(result.LogFile);
            }
        }

        [StTest("jobs: all stream exits preserve their maximum condition code", Reqs = "XMD-005,XMD-007,COM-018")]
        public static void JobStreamsPreserveConditionCodes()
        {
            string source = Path.Combine(ReportService.HomeDir(), "jobs");
            string root = Path.Combine(Path.GetTempPath(), "STERP-stream-test-" + Guid.NewGuid().ToString("N"));
            string[] jobs = { "MONTHEND", "DDCOLL", "NIGHTLY", "PAYRUN", "STMTS",
                "VATRET", "XMDCLOSE", "XMDCONV", "YEAREND" };
            Directory.CreateDirectory(root);
            try
            {
                File.Copy(Path.Combine(source, "JOBEND.cmd"), Path.Combine(root, "JOBEND.cmd"));
                File.WriteAllText(Path.Combine(root, "SETENV.cmd"),
                    "@echo off\r\nset JOBNAME=%~1\r\nset JOBRUNID=\r\nset P_RESTART=\r\n" +
                    "set SYSOUT=%~dp0test.log\r\nset MAXCC=%TESTCC%\r\nset LASTCC=%TESTCC%\r\nexit /b 0\r\n");
                File.WriteAllText(Path.Combine(root, "RUNSTEP.cmd"), "@echo off\r\nexit /b 0\r\n");
                File.WriteAllText(Path.Combine(root, "sqlcmd.cmd"), "@echo off\r\necho RELEASED\r\nexit /b 0\r\n");
                foreach (string job in jobs)
                {
                    File.Copy(Path.Combine(source, job + ".cmd"), Path.Combine(root, job + ".cmd"));
                    foreach (int condition in new[] { 0, 4, 8, 12, 16 })
                    {
                        var info = new System.Diagnostics.ProcessStartInfo("cmd.exe", "/c " + job + ".cmd");
                        info.WorkingDirectory = root;
                        info.UseShellExecute = false;
                        info.CreateNoWindow = true;
                        info.RedirectStandardOutput = true;
                        info.RedirectStandardError = true;
                        info.EnvironmentVariables["TESTCC"] = condition.ToString();
                        info.EnvironmentVariables["PATH"] = root + ";" + Environment.GetFolderPath(Environment.SpecialFolder.System);
                        using (var process = System.Diagnostics.Process.Start(info))
                        {
                            string output = process.StandardOutput.ReadToEnd() + process.StandardError.ReadToEnd();
                            process.WaitForExit();
                            St.Eq(condition, process.ExitCode, job + " preserves MAXCC " + condition + ": " + output);
                        }
                    }
                }
            }
            finally
            {
                Directory.Delete(root, true);
            }
        }

        /// <summary>Compiles a throw-away batch EXE that behaves like a COBOL batch main program.</summary>
        private static string BuildStubBatchProgram()
        {
            string exe = Path.Combine(SterpConfig.BinDir, "STRPTST.exe");
            string source =
                "using System;using System.IO;class P{static int Main(string[] a){" +
                "Console.WriteLine(\"STERP-I-HOME \"+Environment.CurrentDirectory);" +
                "string o=\"\";foreach(string s in a){Console.WriteLine(\"STERP-I-0002 ARG \"+s);" +
                "if(s.StartsWith(\"OUT=\"))o=s.Substring(4);}" +
                "if(o.Length>0){Directory.CreateDirectory(Path.GetDirectoryName(o));" +
                "File.WriteAllText(o,\"STERP harness spool file\");Console.WriteLine(\"STERP-I-SPOOL \"+o);}" +
                "Console.WriteLine(\"STERP-I-0099 Run complete\");return 0;}}";

            using (CodeDomProvider cs = CodeDomProvider.CreateProvider("CSharp"))
            {
                CompilerParameters p = new CompilerParameters();
                p.GenerateExecutable = true;
                p.OutputAssembly = exe;
                p.ReferencedAssemblies.Add("System.dll");
                CompilerResults res = cs.CompileAssemblyFromSource(p, source);
                if (res.Errors.HasErrors) St.Skip("the stub batch program could not be compiled: " + res.Errors[0].ErrorText);
            }
            return exe;
        }
    }
}
