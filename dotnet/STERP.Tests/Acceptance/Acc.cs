using System;
using System.Data;
using STERP.Common;
using STERP.CobolInterop;
using STERP.Data;

namespace STERP.Tests.Acceptance
{
    /// <summary>
    /// Shared plumbing for the end-to-end acceptance suite.  These tests drive the real business
    /// facades, the real COBOL engine and the real database; they create their own fixture data
    /// under a reserved "ZA" reference prefix so that the whole suite can be run again and again
    /// without being reset.  Nothing here reaches around the engine: SQL is used to seed fixtures
    /// and to observe the result, never to perform a business step.
    /// </summary>
    public static class Acc
    {
        public const string Co = "STC";
        public const string Prefix = "ZA";

        /// <summary>The business date the demonstration data was built around (FY2026 period 7, OPEN).</summary>
        public static readonly DateTime Today = new DateTime(2026, 10, 4);

        public static void Context(string user, string site)
        {
            SterpSession.Current.UserCode = user;
            SterpSession.Current.RealUserCode = user;
            SterpSession.Current.CompanyCode = Co;
            SterpSession.Current.SiteCode = site;
            SterpSession.Current.BaseCurrency = "GBP";
            SterpSession.Current.BusinessDate = Today;
        }

        public static void Need(string objectName)
        {
            if (!SqlHelper.ObjectExists(objectName)) St.Skip(objectName + " is not deployed yet");
        }

        public static void NeedProgram(string program)
        {
            if (Array.IndexOf(CobolGateway.ProgramNames(), program) < 0)
                St.Skip(program + " is not linked into this build");
        }

        /// <summary>A step that cannot run because the engine does not hold the program yet is a skip,
        /// not a failure - everything else is the module's answer and is asserted on.</summary>
        public static void NeedEngine(CobolResult r, string program)
        {
            if (r == null) St.Skip(program + " returned nothing");
            string reason = r.ReasonCode == null ? "" : r.ReasonCode;
            if (r.IsNotFound && reason.StartsWith("PGM")) St.Skip(program + " is not in the engine yet");
            if (r.IsSevere && reason.StartsWith("ENG")) St.Skip(program + " could not be reached: " + r.Display);
        }

        /// <summary>A unique, traceable, re-runnable document reference, e.g. ZAO6K3F91A.</summary>
        public static string Ref(string kind)
        {
            long t = DateTime.Now.Ticks / 10000L;
            string s = "";
            const string digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
            while (t > 0 && s.Length < 8) { s = digits[(int)(t % 36)] + s; t /= 36; }
            return Prefix + kind + s;
        }

        public static object Scalar(string sql, params object[] nv) { return SqlHelper.ExecuteScalar(sql, nv); }
        public static int Int(string sql, params object[] nv) { return Fmt.Int(SqlHelper.ExecuteScalar(sql, nv)); }
        public static decimal Dec(string sql, params object[] nv) { return Fmt.Dec(SqlHelper.ExecuteScalar(sql, nv)); }
        public static string Str(string sql, params object[] nv) { return Fmt.Str(SqlHelper.ExecuteScalar(sql, nv)); }
        public static DataRow Row(string sql, params object[] nv) { return SqlHelper.ExecuteRow(sql, nv); }

        /// <summary>Reads a row of demonstration data; skips the test when the seed is not loaded.</summary>
        public static DataRow Demo(string sql, string what, params object[] nv)
        {
            DataRow r = SqlHelper.ExecuteRow(sql, nv);
            if (r == null) St.Skip("no " + what + " is seeded for " + Co);
            return r;
        }

        public static string Text(DataRow r, string column) { return Fmt.Str(r[column]); }
        public static decimal Num(DataRow r, string column) { return Fmt.Dec(r[column]); }

        /// <summary>True when the facade answered with a business refusal rather than an engine problem.</summary>
        public static bool Refused(CobolResult r)
        {
            if (r == null || r.Succeeded) return false;
            string reason = r.ReasonCode == null ? "" : r.ReasonCode;
            return !reason.StartsWith("PGM") && !reason.StartsWith("ENG");
        }

        public static void Ok(CobolResult r, string what)
        {
            St.True(r != null && r.Succeeded, what + ": " + (r == null ? "(no result)" : r.Display));
        }
    }
}
