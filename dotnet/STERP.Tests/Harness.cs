using System;
using System.Collections.Generic;
using System.Reflection;
using System.Text.RegularExpressions;

namespace STERP.Tests
{
    /// <summary>
    /// Home-grown TAP test runner (the solution has no NUnit / MSTest dependency).
    ///   STERP.Tests.exe                 run everything
    ///   STERP.Tests.exe /filter:Codec   run tests whose name or method matches the regex
    /// Exit code = number of failures.
    /// </summary>
    public static class Harness
    {
        public static int Main(string[] args)
        {
            string filter = null;
            foreach (string a in args)
            {
                if (a.StartsWith("/filter:", StringComparison.OrdinalIgnoreCase)) filter = a.Substring(8);
                else if (a.StartsWith("--filter=", StringComparison.OrdinalIgnoreCase)) filter = a.Substring(9);
            }
            Regex re = string.IsNullOrEmpty(filter) ? null : new Regex(filter, RegexOptions.IgnoreCase);

            List<KeyValuePair<StTestAttribute, MethodInfo>> tests = new List<KeyValuePair<StTestAttribute, MethodInfo>>();
            foreach (Type t in Assembly.GetExecutingAssembly().GetTypes())
                foreach (MethodInfo m in t.GetMethods(BindingFlags.Public | BindingFlags.Static))
                {
                    StTestAttribute a = (StTestAttribute)Attribute.GetCustomAttribute(m, typeof(StTestAttribute));
                    if (a == null) continue;
                    if (re != null && !re.IsMatch(a.Name) && !re.IsMatch(m.Name) && !re.IsMatch(t.Name)) continue;
                    tests.Add(new KeyValuePair<StTestAttribute, MethodInfo>(a, m));
                }
            tests.Sort(delegate (KeyValuePair<StTestAttribute, MethodInfo> x, KeyValuePair<StTestAttribute, MethodInfo> y)
            {
                int c = string.CompareOrdinal(x.Value.DeclaringType.Name, y.Value.DeclaringType.Name);
                return c != 0 ? c : string.CompareOrdinal(x.Key.Name, y.Key.Name);
            });

            Console.WriteLine("1.." + tests.Count);
            int n = 0, failed = 0;
            foreach (KeyValuePair<StTestAttribute, MethodInfo> kv in tests)
            {
                n++;
                string tags = "";
                if (!string.IsNullOrEmpty(kv.Key.Reqs)) tags += "  # REQ: " + kv.Key.Reqs;
                if (!string.IsNullOrEmpty(kv.Key.Ac)) tags += (tags.Length > 0 ? " " : "  # ") + "AC: " + kv.Key.Ac;
                try
                {
                    kv.Value.Invoke(null, null);
                    Console.WriteLine("ok " + n + " - " + kv.Key.Name + tags);
                }
                catch (Exception ex)
                {
                    Exception e = ex is TargetInvocationException && ex.InnerException != null ? ex.InnerException : ex;
                    if (e is StSkipException)
                    {
                        Console.WriteLine("ok " + n + " - " + kv.Key.Name + " # SKIP " + e.Message);
                        continue;
                    }
                    failed++;
                    Console.WriteLine("not ok " + n + " - " + kv.Key.Name + tags);
                    Console.WriteLine("#   " + e.GetType().Name + ": " + (e.Message ?? "").Replace("\r", "").Replace("\n", " "));
                    if (!(e is StAssertException) && e.StackTrace != null)
                        foreach (string line in e.StackTrace.Split('\n'))
                            Console.WriteLine("#   " + line.TrimEnd());
                }
            }
            Console.WriteLine("# tests " + n + " failures " + failed);
            return failed;
        }
    }
}
