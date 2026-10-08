using System;
using System.Globalization;

namespace STERP.Common
{
    /// <summary>
    /// UK presentation helpers (COM-024): dates dd/MM/yyyy, money £#,##0.00,
    /// quantities to six decimals with an explicit unit caption.
    /// </summary>
    public static class Fmt
    {
        public static readonly CultureInfo UK = CultureInfo.GetCultureInfo("en-GB");

        public static string Str(object v) { return v == null || v == DBNull.Value ? "" : Convert.ToString(v).TrimEnd(); }

        public static decimal Dec(object v)
        {
            if (v == null || v == DBNull.Value) return 0m;
            if (v is decimal) return (decimal)v;
            decimal d;
            return decimal.TryParse(Convert.ToString(v), NumberStyles.Any, UK, out d) ? d : 0m;
        }

        public static int Int(object v)
        {
            if (v == null || v == DBNull.Value) return 0;
            int n;
            return int.TryParse(Convert.ToString(v), NumberStyles.Any, UK, out n) ? n : 0;
        }

        public static bool Bool(object v)
        {
            if (v == null || v == DBNull.Value) return false;
            if (v is bool) return (bool)v;
            string s = Convert.ToString(v).Trim().ToUpperInvariant();
            return s == "1" || s == "Y" || s == "YES" || s == "TRUE";
        }

        public static string Date(object v)
        {
            if (v == null || v == DBNull.Value) return "";
            DateTime d;
            if (v is DateTime) d = (DateTime)v;
            else if (!DateTime.TryParse(Convert.ToString(v), UK, DateTimeStyles.None, out d)) return Str(v);
            return d.ToString("dd/MM/yyyy", UK);
        }

        public static string Stamp(object v)
        {
            if (v == null || v == DBNull.Value) return "";
            DateTime d;
            if (v is DateTime) d = (DateTime)v;
            else if (!DateTime.TryParse(Convert.ToString(v), UK, DateTimeStyles.None, out d)) return Str(v);
            return d.ToString("dd/MM/yyyy HH:mm:ss", UK);
        }

        public static string Money(object v) { return Dec(v).ToString("£#,##0.00;-£#,##0.00;£0.00", UK); }

        /// <summary>Foreign currency: "1,234.50 EUR" (never a £ sign on a non-GBP amount).</summary>
        public static string Money(object v, string currencyCode)
        {
            string c = Str(currencyCode).ToUpperInvariant();
            if (c.Length == 0 || c == "GBP") return Money(v);
            return Dec(v).ToString("#,##0.00;-#,##0.00;0.00", UK) + " " + c;
        }

        /// <summary>Quantity to six decimals, trailing zeros trimmed, with the unit of measure.</summary>
        public static string Qty(object v, string uom)
        {
            string s = Dec(v).ToString("#,##0.000000", UK);
            if (s.Contains(".")) s = s.TrimEnd('0').TrimEnd('.');
            string u = Str(uom);
            return u.Length == 0 ? s : s + " " + u;
        }

        public static string Qty(object v) { return Qty(v, null); }

        public static string Rate(object v) { return Dec(v).ToString("#,##0.00000000", UK); }
        public static string Pct(object v) { return Dec(v).ToString("#,##0.00##", UK) + "%"; }

        /// <summary>COBOL / SQLIO date exchange format.</summary>
        public static string Yyyymmdd(DateTime d) { return d.ToString("yyyyMMdd", UK); }

        public static DateTime? FromYyyymmdd(string s)
        {
            DateTime d;
            if (s != null && s.Trim().Length == 8 &&
                DateTime.TryParseExact(s.Trim(), "yyyyMMdd", UK, DateTimeStyles.None, out d)) return d;
            return null;
        }

        /// <summary>Accepts dd/MM/yyyy, dd/MM/yy, ddMMyyyy and yyyyMMdd (the old 3270 habits).</summary>
        public static DateTime? ParseDate(string s)
        {
            if (string.IsNullOrEmpty(s)) return null;
            s = s.Trim();
            string[] f = { "dd/MM/yyyy", "dd/MM/yy", "ddMMyyyy", "yyyyMMdd", "dd-MM-yyyy", "dd.MM.yyyy" };
            DateTime d;
            if (DateTime.TryParseExact(s, f, UK, DateTimeStyles.None, out d)) return d;
            if (DateTime.TryParse(s, UK, DateTimeStyles.None, out d)) return d;
            return null;
        }

        /// <summary>Fiscal period caption, e.g. 2026/07.</summary>
        public static string Period(int fiscalYear, int periodNo)
        {
            if (fiscalYear <= 0) return "";
            return fiscalYear.ToString("0000", UK) + "/" + periodNo.ToString("00", UK);
        }
    }
}
