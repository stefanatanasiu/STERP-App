using System;
using System.Data.SqlClient;
using System.Text.RegularExpressions;

namespace STERP.Data
{
    /// <summary>
    /// A business rule refused the request.  Raised by translating a stored
    /// procedure THROW of the form  'STERP-E-nnnn: text'  (CONVENTIONS 4.4).
    /// </summary>
    [Serializable]
    public class BusinessRuleException : Exception
    {
        /// <summary>Stable message id, e.g. "E-0042".</summary>
        public string Code { get; private set; }
        /// <summary>'E' error, 'W' warning, 'I' information.</summary>
        public char Severity { get; private set; }
        public string ProcedureName { get; private set; }

        public BusinessRuleException(string code, char severity, string text, string procedure, Exception inner)
            : base(text, inner)
        {
            Code = code; Severity = severity; ProcedureName = procedure;
        }

        private static readonly Regex Tag =
            new Regex(@"STERP-(?<s>[EWI])-(?<c>[A-Za-z0-9]{1,8})\s*:\s*(?<t>.*)", RegexOptions.Compiled);

        /// <summary>
        /// Returns a BusinessRuleException when the server error carries a STERP
        /// message id, otherwise null (the caller should rethrow).
        /// </summary>
        public static BusinessRuleException TryTranslate(SqlException ex)
        {
            if (ex == null) return null;
            foreach (SqlError e in ex.Errors)
            {
                Match m = Tag.Match(e.Message ?? "");
                if (!m.Success) continue;
                return new BusinessRuleException(m.Groups["s"].Value + "-" + m.Groups["c"].Value,
                    m.Groups["s"].Value[0], m.Groups["t"].Value.Trim(), e.Procedure, ex);
            }
            return null;
        }
    }
}
