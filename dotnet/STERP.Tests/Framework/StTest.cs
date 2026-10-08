using System;

namespace STERP.Tests
{
    /// <summary>
    /// Marks a public static parameterless method as a STERP framework test.
    /// Requirement / acceptance tags feed build\trace-gen.ps1.
    /// </summary>
    [AttributeUsage(AttributeTargets.Method, AllowMultiple = false)]
    public sealed class StTestAttribute : Attribute
    {
        public string Name { get; private set; }
        public string Reqs { get; set; }
        public string Ac { get; set; }
        public StTestAttribute(string name) { Name = name; }
    }

    /// <summary>A test decided it cannot run (missing database object, engine not built).</summary>
    public class StSkipException : Exception
    {
        public StSkipException(string reason) : base(reason) { }
    }

    public class StAssertException : Exception
    {
        public StAssertException(string message) : base(message) { }
    }

    /// <summary>Assertions - deliberately tiny, no third party test framework in this solution.</summary>
    public static class St
    {
        public static void Skip(string reason) { throw new StSkipException(reason); }
        public static void Fail(string message) { throw new StAssertException(message); }

        public static void True(bool condition, string message)
        {
            if (!condition) throw new StAssertException(message);
        }

        public static void Eq(object expected, object actual, string what)
        {
            string e = Text(expected), a = Text(actual);
            if (!string.Equals(e, a, StringComparison.Ordinal))
                throw new StAssertException(what + ": expected <" + e + "> but was <" + a + ">");
        }

        public static void Contains(string needle, string haystack, string what)
        {
            if (haystack == null || haystack.IndexOf(needle, StringComparison.OrdinalIgnoreCase) < 0)
                throw new StAssertException(what + ": <" + needle + "> not found in <" + haystack + ">");
        }

        private static string Text(object o)
        {
            if (o == null) return "(null)";
            if (o is decimal) return ((decimal)o).ToString("0.######", System.Globalization.CultureInfo.InvariantCulture);
            if (o is double || o is float) return Convert.ToDecimal(o).ToString("0.######", System.Globalization.CultureInfo.InvariantCulture);
            return Convert.ToString(o, System.Globalization.CultureInfo.InvariantCulture);
        }
    }
}
