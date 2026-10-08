using System;
using System.Collections.Generic;
using System.Configuration;
using System.IO;
using System.Reflection;

namespace STERP.Common
{
    /// <summary>
    /// Application settings.  App.config &lt;appSettings&gt; is the base; an optional
    /// STERP.INI next to the EXE overrides it (that is how the 1990s site
    /// installations were tuned, and the operators still expect it).
    /// </summary>
    public static class SterpConfig
    {
        /// <summary>The product name shown on every title bar, banner and help screen.</summary>
        public const string ProductName = "STERP - Stefan's ERP";

        private static readonly Dictionary<string, string> Ini =
            new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        private static bool _iniLoaded;

        public static string BinDir
        {
            get { return Path.GetDirectoryName(new Uri(Assembly.GetExecutingAssembly().CodeBase).LocalPath); }
        }

        public static string RunDir
        {
            get
            {
                string d = Get("STERP.RunDir", null);
                if (string.IsNullOrEmpty(d)) d = Environment.GetEnvironmentVariable("STERP_RUN");
                if (string.IsNullOrEmpty(d)) d = Path.GetFullPath(Path.Combine(BinDir, @"..\..\run"));
                return d;
            }
        }

        public static string CopyDir { get { return Get("STERP.CopyDir", Path.Combine(BinDir, "copy")); } }

        public static string SqlConnectionString
        {
            get { return Get("STERP.Sql", "Data Source=.;Initial Catalog=STERP;Integrated Security=SSPI"); }
        }

        /// <summary>ODBC string handed to the COBOL engine by sterp_init.</summary>
        public static string OdbcConnectionString
        {
            get
            {
                string s = Get("STERP.Odbc", null);
                if (string.IsNullOrEmpty(s)) s = Environment.GetEnvironmentVariable("STERP_ODBC");
                if (string.IsNullOrEmpty(s))
                    s = "Driver={ODBC Driver 18 for SQL Server};Server=.;Database=STERP;Trusted_Connection=yes;" +
                        "TrustServerCertificate=yes;MARS_Connection=yes;APP=STERP-COBOL";
                return s;
            }
        }

        public static string Get(string key, string defaultValue)
        {
            LoadIni();
            string v;
            if (Ini.TryGetValue(key, out v) && !string.IsNullOrEmpty(v)) return v;
            v = ConfigurationManager.AppSettings[key];
            return string.IsNullOrEmpty(v) ? defaultValue : v;
        }

        public static bool GetBool(string key, bool defaultValue)
        {
            string v = Get(key, null);
            if (string.IsNullOrEmpty(v)) return defaultValue;
            v = v.Trim().ToUpperInvariant();
            return v == "Y" || v == "YES" || v == "TRUE" || v == "1" || v == "ON";
        }

        public static int GetInt(string key, int defaultValue)
        {
            int n;
            return int.TryParse(Get(key, null), out n) ? n : defaultValue;
        }

        private static void LoadIni()
        {
            if (_iniLoaded) return;
            _iniLoaded = true;
            try
            {
                string f = Path.Combine(BinDir, "STERP.INI");
                if (!File.Exists(f)) return;
                foreach (string raw in File.ReadAllLines(f))
                {
                    string line = raw.Trim();
                    if (line.Length == 0 || line[0] == ';' || line[0] == '[') continue;
                    int eq = line.IndexOf('=');
                    if (eq > 0) Ini[line.Substring(0, eq).Trim()] = line.Substring(eq + 1).Trim();
                }
            }
            catch { /* a bad INI must never stop the client starting */ }
        }
    }
}
