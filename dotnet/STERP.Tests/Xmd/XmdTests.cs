using System;
using System.Data;
using System.Diagnostics;
using System.IO;
using STERP.Business;
using STERP.Business.Xmd;
using STERP.Common;
using STERP.Data;

namespace STERP.Tests
{
    /// <summary>
    /// Cross module controls and reporting (XMD): the report catalogue, the
    /// JCL style job streams and the SCREEN SECTION operator console.
    /// </summary>
    public static class XmdTests
    {
        private static string Home()
        {
            return Path.GetFullPath(Path.Combine(SterpConfig.BinDir, @"..\.."));
        }

        private static string RunConsole(string args, out int rc)
        {
            string exe = Path.Combine(SterpConfig.BinDir, "STERPOPS.exe");
            if (!File.Exists(exe)) St.Skip("bin\\x86\\STERPOPS.exe is not built yet");
            ProcessStartInfo psi = new ProcessStartInfo(exe, args);
            psi.WorkingDirectory = Home();
            psi.UseShellExecute = false;
            psi.RedirectStandardOutput = true;
            psi.RedirectStandardError = true;
            psi.CreateNoWindow = true;
            psi.EnvironmentVariables["STERP_RUN"] = SterpConfig.RunDir;
            psi.EnvironmentVariables["STERP_ODBC"] = SterpConfig.OdbcConnectionString;
            using (Process p = Process.Start(psi))
            {
                string text = p.StandardOutput.ReadToEnd() + p.StandardError.ReadToEnd();
                p.WaitForExit();
                rc = p.ExitCode;
                return text;
            }
        }

        [StTest("ops console: SMOKE runs clean and paints all six panels", Reqs = "XMD-005,COM-017,COM-018")]
        public static void ConsoleSmokePaintsPanels()
        {
            int rc;
            string outp = RunConsole("SMOKE", out rc);
            St.True(rc == 0, "STERPOPS SMOKE ended with return code " + rc);
            string[] banners = { "SIGN ON", "JOB CATALOGUE", "JOB MONITOR",
                                 "EXCEPTION QUEUE", "PERIOD STATUS", "ENGINE HEALTH" };
            foreach (string b in banners)
                St.True(outp.IndexOf(b, StringComparison.OrdinalIgnoreCase) >= 0,
                        "the console did not paint the " + b + " panel");
            St.True(outp.IndexOf("JOB.", StringComparison.Ordinal) >= 0,
                    "the job catalogue panel listed no JOB. stream");
        }

        [StTest("ops console: SMOKE honours the signed on operator and company", Reqs = "XMD-005,COM-002")]
        public static void ConsoleSmokeTakesArguments()
        {
            int rc;
            string outp = RunConsole("SMOKE USER=OPS CO=STC", out rc);
            St.True(rc == 0, "STERPOPS SMOKE USER=OPS CO=STC ended with return code " + rc);
            St.True(outp.IndexOf("OPS", StringComparison.OrdinalIgnoreCase) >= 0,
                    "the console did not report the signed on operator");
        }

        [StTest("report launcher: a JOB. catalogue entry is submitted as jobs\\<name>.cmd", Reqs = "XMD-005,COM-018")]
        public static void JobStreamsAreLaunchedAsCmd()
        {
            St.True(ReportService.IsJobStream("JOB.NIGHTLY"), "JOB.NIGHTLY is a job stream");
            St.True(!ReportService.IsJobStream("RPT-015"), "RPT-015 is an ordinary report");
            string cmd = ReportService.JobStream("NIGHTLY");
            St.True(cmd.EndsWith(@"jobs\NIGHTLY.cmd", StringComparison.OrdinalIgnoreCase),
                    "the launcher resolved " + cmd);
            St.True(File.Exists(cmd), "jobs\\NIGHTLY.cmd is not installed");
            St.True(File.Exists(ReportService.JobStream("XMDCLOSE")),
                    "jobs\\XMDCLOSE.cmd is not installed");
            St.True(File.Exists(ReportService.JobStream("XMDCONV")),
                    "jobs\\XMDCONV.cmd is not installed");
        }

        [StTest("catalogue: every seeded xmd report names a program that is installed", Reqs = "RPT-004,RPT-015,RPT-016,XMD-013")]
        public static void CatalogueProgramsExist()
        {
            if (!SqlHelper.ObjectExists("core.ReportDef")) St.Skip("core.ReportDef is not deployed yet");
            string[] codes = { "RPT-004", "RPT-007", "RPT-015", "RPT-016", "RPT-017",
                               "XMD.OBLIGREG", "XMD.KPIOTIF", "XMD.CHAINRPT", "XMD.CONVLOAD",
                               "JOB.NIGHTLY", "JOB.MONTHEND" };
            int seen = 0;
            foreach (string code in codes)
            {
                string prog = Fmt.Str(SqlHelper.ExecuteScalar(
                    "SELECT ProgramName FROM core.ReportDef WHERE ReportCode = '" + code + "' AND IsActive = 1"));
                if (prog.Length == 0) continue;
                seen++;
                string path = ReportService.IsJobStream(code)
                    ? ReportService.JobStream(prog)
                    : Path.Combine(SterpConfig.BinDir, prog + ".exe");
                St.True(File.Exists(path), code + " names " + prog + ", which is not installed (" + path + ")");
            }
            if (seen == 0) St.Skip("the xmd report catalogue has not been seeded yet");
        }

        [StTest("catalogue: the ParamSpec of a report matches the program's own arguments", Reqs = "RPT-004,RPT-007,FIN-019,AR-021")]
        public static void ParamSpecMatchesPrograms()
        {
            if (!SqlHelper.ObjectExists("core.ReportDef")) St.Skip("core.ReportDef is not deployed yet");
            string[,] expect = { { "RPT-004", "CASHFCST", "SCENARIO" },
                                 { "RPT-007", "VATAUDIT", "VATPERIOD" },
                                 { "RPT-017", "VATAUDIT", "MODE" },
                                 { "REVALRUN", "REVALRUN", "ASAT" },
                                 { "ARRECON", "ARRECON", "PERIOD" } };
            int seen = 0;
            for (int i = 0; i < expect.GetLength(0); i++)
            {
                DataRow r = SqlHelper.ExecuteRow(
                    "SELECT ProgramName, ParamSpec FROM core.ReportDef WHERE ReportCode = '" + expect[i, 0] + "'");
                if (r == null) continue;
                seen++;
                St.True(string.Equals(Fmt.Str(r["ProgramName"]), expect[i, 1], StringComparison.OrdinalIgnoreCase),
                        expect[i, 0] + " should run " + expect[i, 1] + " but runs " + Fmt.Str(r["ProgramName"]));
                string spec = Fmt.Str(r["ParamSpec"]).ToUpperInvariant();
                St.True(spec.IndexOf(expect[i, 2], StringComparison.Ordinal) >= 0,
                        expect[i, 0] + " ParamSpec '" + spec + "' does not name " + expect[i, 2]);
                if (string.Equals(expect[i, 0], "ARRECON", StringComparison.OrdinalIgnoreCase))
                    St.True(spec.IndexOf("YEAR", StringComparison.Ordinal) < 0,
                            "ARRECON takes PERIOD=yyyypp, not a separate YEAR");
            }
            if (seen == 0) St.Skip("the report catalogue has not been seeded yet");
        }

        [StTest("chain trace: the enquiry screen is installed and reachable from the menu", Reqs = "XMD-001,XMD-004,COM-017")]
        public static void ChainTraceFormIsOnTheMenu()
        {
            St.True(typeof(XmdService).GetMethod("ChainTrace") != null,
                    "XmdService does not expose ChainTrace");
            if (!SqlHelper.ObjectExists("core.MenuItem")) St.Skip("core.MenuItem is not deployed yet");
            string cls = Fmt.Str(SqlHelper.ExecuteScalar(
                "SELECT FormClass FROM core.MenuItem WHERE MenuCode = 'RPT.CONTROLS.CHAIN' AND IsActive = 1"));
            if (cls.Length == 0) St.Skip("the xmd menu has not been seeded yet");
            St.True(cls == "frmChainTrace", "RPT.CONTROLS.CHAIN opens " + cls);
        }

        [StTest("chain trace: the step window is shaped for the 24 step COMMAREA", Reqs = "XMD-001,XMD-002,XMD-003")]
        public static void ChainTraceWindowMatchesCopybook()
        {
            St.True(XmdService.MaxSteps == 24, "CHAINTRCCA holds 24 steps, not " + XmdService.MaxSteps);
            string[] fields = { "CT-SEQ", "CT-STAGE", "CT-DOC-T", "CT-REF", "CT-DATE",
                                "CT-STATUS", "CT-QTY", "CT-VALUE", "CT-KIND", "CT-NOTE" };
            foreach (string f in fields)
                St.True(XmdService.StepSpec.IndexOf("=" + f, StringComparison.Ordinal) >= 0,
                        "the grid does not read " + f + " of CHAINTRCCA");
            string copybook = Path.Combine(Home(), @"cobol\copy\CHAINTRCCA.cpy");
            if (!File.Exists(copybook)) St.Skip("CHAINTRCCA.cpy is not installed");
            string text = File.ReadAllText(copybook);
            St.True(text.IndexOf("OCCURS 24 TIMES", StringComparison.Ordinal) >= 0,
                    "CHAINTRCCA no longer declares 24 steps - the grid spec must follow it");
            foreach (string f in fields)
                St.True(text.IndexOf(f, StringComparison.Ordinal) >= 0,
                        f + " is named by the grid but no longer exists in CHAINTRCCA");
        }

        [StTest("chain trace: the set based twin answers the same question", Reqs = "XMD-004")]
        public static void ChainTraceProcIsDeployed()
        {
            if (!SqlHelper.ObjectExists("rpt.usp_ChainTrace")) St.Skip("rpt.usp_ChainTrace is not deployed yet");
            St.True(typeof(XmdService).GetMethod("ChainTraceList") != null,
                    "XmdService does not expose ChainTraceList");
            St.True(XmdService.IsSetTraceable("ORDER") && XmdService.IsSetTraceable("PO")
                    && XmdService.IsSetTraceable("KIT") && !XmdService.IsSetTraceable(""),
                    "the set based trace must be offered only for the chains it can start from");
            DataTable t = SqlHelper.ExecuteDataTable("rpt.usp_ChainTrace",
                "@CompanyCode", "STC", "@DocType", "ORDER", "@Reference", "NO-SUCH-DOC");
            St.True(t != null, "rpt.usp_ChainTrace returned nothing at all for an unknown reference");
        }
    }
}
