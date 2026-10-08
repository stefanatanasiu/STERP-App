using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using STERP.Common;

namespace STERP.CobolInterop
{
    /// <summary>
    /// The CICS "EXEC CICS LINK PROGRAM(x) COMMAREA(y)" replacement: calls a COBOL
    /// program inside STERPCOB.dll with a COMMAREA buffer.  The GnuCOBOL runtime is
    /// not thread safe, so every call is serialised on a process-wide lock.
    /// </summary>
    public static class CobolGateway
    {
        private const string Engine = "STERPCOB.dll";

        [DllImport(Engine, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int sterp_init(string odbc, string rundir);

        [DllImport(Engine, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
        private static extern int sterp_link(string prog, byte[] commarea, int len);

        [DllImport(Engine, CallingConvention = CallingConvention.Cdecl)]
        private static extern int sterp_progcount();

        [DllImport(Engine, CallingConvention = CallingConvention.Cdecl)]
        private static extern IntPtr sterp_progname(int index);

        /// <summary>Serialises access to the single-threaded COBOL runtime.</summary>
        private static readonly object Padlock = new object();
        private static bool _initialised;

        /// <summary>Writes a COMMAREA hex dump before and after every LINK (STERP.Trace=Y).</summary>
        public static bool Trace = SterpConfig.GetBool("STERP.Trace", false);

        public static void Initialise()
        {
            lock (Padlock)
            {
                if (_initialised) return;
                sterp_init(SterpConfig.OdbcConnectionString, SterpConfig.RunDir);
                _initialised = true;
                SterpLog.Info("COBOL engine initialised, " + ProgramCount() + " programs in the directory");
            }
        }

        public static int ProgramCount()
        {
            lock (Padlock) { try { return sterp_progcount(); } catch (Exception ex) { SterpLog.Error("sterp_progcount", ex); return 0; } }
        }

        public static string[] ProgramNames()
        {
            lock (Padlock)
            {
                List<string> names = new List<string>();
                try
                {
                    int n = sterp_progcount();
                    for (int i = 0; i < n; i++) names.Add(Marshal.PtrToStringAnsi(sterp_progname(i)) ?? "");
                }
                catch (Exception ex) { SterpLog.Error("sterp_progname", ex); }
                names.Sort(StringComparer.OrdinalIgnoreCase);
                return names.ToArray();
            }
        }

        /// <summary>Fills the 400 byte STCAHDR header from the sign-on context.</summary>
        public static void PrepareHeader(string program, CommArea ca, string function, string correlationId)
        {
            SterpSession s = SterpSession.Current;
            ca.SetString("CA-PROGRAM", program.ToUpperInvariant());
            if (function != null) ca.SetString("CA-FUNCTION", function.ToUpperInvariant());
            ca.SetString("CA-COMPANY", s.CompanyCode);
            ca.SetString("CA-SITE", s.SiteCode);
            ca.SetString("CA-USER", s.UserCode);
            ca.SetDate("CA-BUSINESS-DATE", s.BusinessDate == DateTime.MinValue ? DateTime.Today : s.BusinessDate);
            ca.SetString("CA-CORRELATION-ID", string.IsNullOrEmpty(correlationId) ? s.NewCorrelationId() : correlationId);
            ca.SetInt("CA-RETURN-CODE", 0);
            ca.SetString("CA-REASON-CODE", "");
            ca.SetString("CA-MESSAGE", "");
            ca.SetInt("CA-MSG-COUNT", 0);
        }

        public static CobolResult Link(string program, CommArea ca)
        {
            return Link(program, ca, null, null);
        }

        public static CobolResult Link(string program, CommArea ca, string function)
        {
            return Link(program, ca, function, null);
        }

        /// <summary>
        /// Fills the header, LINKs to the program and reads the outcome back out of
        /// the header.  A program missing from the directory comes back as RC 16.
        /// </summary>
        public static CobolResult Link(string program, CommArea ca, string function, string correlationId)
        {
            if (ca == null) throw new ArgumentNullException("ca");
            string prog = (program ?? "").Trim().ToUpperInvariant();
            Initialise();
            PrepareHeader(prog, ca, function, correlationId);

            CobolResult r = new CobolResult { Program = prog, Area = ca };
            int rc;
            lock (Padlock)
            {
                if (Trace) SterpLog.Trace("LINK " + prog + " " + (function ?? "") + " in  >>>\r\n" + ca.HexDump());
                DateTime t0 = DateTime.Now;
                try { rc = sterp_link(prog, ca.Buffer, ca.Length); }
                catch (DllNotFoundException ex)
                {
                    SterpLog.Error("STERPCOB.dll could not be loaded", ex);
                    r.ReturnCode = 12; r.ReasonCode = "ENG0001";
                    r.Message = "The STERP transaction engine (STERPCOB.DLL) is not available on this workstation.";
                    return r;
                }
                catch (EntryPointNotFoundException ex)
                {
                    SterpLog.Error("sterp_link entry point missing", ex);
                    r.ReturnCode = 12; r.ReasonCode = "ENG0002";
                    r.Message = "The STERP transaction engine is the wrong version.";
                    return r;
                }
                if (Trace) SterpLog.Trace("LINK " + prog + " out <<<\r\n" + ca.HexDump());
                SterpLog.Info(string.Format("LINK {0,-8} {1,-8} rc={2} {3}ms", prog, function ?? "",
                    rc, (int)(DateTime.Now - t0).TotalMilliseconds));
            }

            if (rc == -1)
            {
                r.ReturnCode = 16; r.ReasonCode = "PGM0001";
                r.Message = "Program " + prog + " is not in the engine program directory.";
                return r;
            }
            r.ReturnCode = ca.GetInt("CA-RETURN-CODE");
            r.ReasonCode = ca.GetString("CA-REASON-CODE");
            r.Message = ca.GetString("CA-MESSAGE");
            r.MessageCount = ca.GetInt("CA-MSG-COUNT");
            r.CorrelationId = ca.GetString("CA-CORRELATION-ID");
            if (r.IsError) SterpLog.Warn("LINK " + prog + " " + r);
            return r;
        }

        /// <summary>Convenience: build the COMMAREA from its copybook and LINK in one step.</summary>
        public static CobolResult Link(string program, string copybook, string function, out CommArea area)
        {
            area = CommArea.For(copybook);
            return Link(program, area, function, null);
        }
    }
}
