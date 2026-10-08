using System;
using STERP.CobolInterop;
using STERP.Common;

namespace STERP.Tests
{
    /// <summary>End to end checks of the COBOL gateway using the PING program (COM-011).</summary>
    public static class GatewayTests
    {
        [StTest("engine: the program directory is published to the client", Reqs = "COM-011")]
        public static void Directory()
        {
            string[] names = CobolGateway.ProgramNames();
            St.True(names.Length > 0, "sterp_progcount returned no programs - is STERPCOB.dll built?");
            St.Eq(names.Length, CobolGateway.ProgramCount(), "progcount matches progname list");
            St.True(Array.IndexOf(names, "PING") >= 0, "PING is in the engine directory");
        }

        [StTest("engine: LINK PING echoes COMP-3, SIGN SEPARATE and zoned tables", Reqs = "COM-011", Ac = "AC-001")]
        public static void PingRoundTrip()
        {
            SterpSession.Current.UserCode = "SYSADM";
            SterpSession.Current.CompanyCode = "STC";
            SterpSession.Current.SiteCode = "RDG";
            SterpSession.Current.BusinessDate = new DateTime(2026, 10, 4);

            CommArea ca = CommArea.For("PINGCA");
            ca.SetDecimal("PG-ECHO-AMOUNT", 1234.56m);
            ca.SetDecimal("PG-ECHO-QTY(1)", 1.5m);
            ca.SetDecimal("PG-ECHO-QTY(2)", -2.25m);
            ca.SetDecimal("PG-ECHO-QTY(3)", 999999999.123456m);
            ca.SetString("PG-ECHO-CODE(1)", "EA");

            CobolResult r = CobolGateway.Link("PING", ca, "HEALTH");

            St.True(r.Succeeded, "PING returned " + r.ReturnCode + " " + r.ReasonCode + " " + r.Message);
            St.Eq("PING", ca.GetString("CA-PROGRAM"), "the program stamps its own name in the header");
            St.Eq("SYSADM", ca.GetString("CA-USER"), "the gateway filled CA-USER from the session");
            St.Eq("STC", ca.GetString("CA-COMPANY"), "the gateway filled CA-COMPANY");
            St.Eq(20261004m, ca.GetDecimal("CA-BUSINESS-DATE"), "the gateway filled the business date");

            St.Eq(2469.12m, ca.GetDecimal("PG-ECHO-DOUBLE"), "COMP-3 doubled into SIGN LEADING SEPARATE");
            St.Eq(-1.5m, ca.GetDecimal("PG-ECHO-QTY(1)"), "zoned quantity 1 negated");
            St.Eq(2.25m, ca.GetDecimal("PG-ECHO-QTY(2)"), "zoned quantity 2 negated");
            St.Eq(-999999999.123456m, ca.GetDecimal("PG-ECHO-QTY(3)"), "zoned quantity 3 negated");
            St.Eq("EA", ca.GetString("PG-ECHO-CODE(1)"), "alphanumeric table entry survived the call");
            St.Contains("4.2.17", ca.GetString("PG-ENGINE-VERSION"), "engine version");
        }

        [StTest("engine: PING reads the STERP database through SQLIO", Reqs = "COM-011")]
        public static void PingDatabase()
        {
            CommArea ca = CommArea.For("PINGCA");
            CobolResult r = CobolGateway.Link("PING", ca, "HEALTH");
            if (!r.IsOk && ca.GetString("PG-DB-NAME").Length == 0)
                St.Skip("the COBOL engine cannot reach SQL Server: " + r.Display);
            St.Eq("STERP", ca.GetString("PG-DB-NAME"), "database name read by SQLIO");
            St.True(ca.GetString("PG-DB-SERVER-TIME").Length >= 19, "server time returned");
            St.Contains("SQL Server", ca.GetString("PG-DB-VERSION"), "database version");
        }

        [StTest("engine: an unknown program comes back as RC 16 not an exception", Reqs = "COM-011,COM-017")]
        public static void ProgramNotFound()
        {
            CommArea ca = CommArea.For("PINGCA");
            CobolResult r = CobolGateway.Link("NOSUCH", ca, "TEST");
            St.Eq(16, r.ReturnCode, "return code");
            St.True(r.IsNotFound, "IsNotFound");
            St.Eq("PGM0001", r.ReasonCode, "reason code");
            St.Contains("NOSUCH", r.Display, "message names the program");
        }
    }
}
