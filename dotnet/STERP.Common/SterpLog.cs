using System;
using System.IO;
using System.Text;

namespace STERP.Common
{
    /// <summary>Plain text run log in run\logs\STERP-yyyyMMdd.log (one line per event).</summary>
    public static class SterpLog
    {
        private static readonly object Padlock = new object();
        private static string _dir;

        public static bool TraceEnabled { get { return SterpConfig.GetBool("STERP.Trace", false); } }

        public static string LogFile
        {
            get { return Path.Combine(Dir, "STERP-" + DateTime.Now.ToString("yyyyMMdd") + ".log"); }
        }

        private static string Dir
        {
            get
            {
                if (_dir == null)
                {
                    _dir = Path.Combine(SterpConfig.RunDir, "logs");
                    try { Directory.CreateDirectory(_dir); } catch { _dir = Path.GetTempPath(); }
                }
                return _dir;
            }
        }

        public static void Info(string message) { Write("I", message); }
        public static void Warn(string message) { Write("W", message); }
        public static void Error(string message) { Write("E", message); }
        public static void Trace(string message) { if (TraceEnabled) Write("T", message); }

        public static void Error(string message, Exception ex)
        {
            Write("E", message + " | " + (ex == null ? "" : ex.GetType().Name + ": " + ex.Message));
            if (ex != null) Write("E", ex.StackTrace ?? "");
        }

        public static void Write(string severity, string message)
        {
            try
            {
                string user = SterpSession.Current == null ? "-" : SterpSession.Current.UserCode;
                string line = string.Format("{0:yyyy-MM-dd HH:mm:ss} {1} {2,-10} {3}",
                    DateTime.Now, severity, user ?? "-", message);
                lock (Padlock) File.AppendAllText(LogFile, line + Environment.NewLine, Encoding.UTF8);
            }
            catch { /* logging must never throw into business code */ }
        }
    }
}
