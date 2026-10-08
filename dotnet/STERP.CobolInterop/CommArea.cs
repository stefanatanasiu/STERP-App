using System;
using System.Data;
using System.Text;

namespace STERP.CobolInterop
{
    /// <summary>
    /// A COMMAREA buffer plus typed access by COBOL field name.  Subscripts are
    /// written the COBOL way - ca.GetDecimal("PG-ECHO-QTY(2)") - and groups may be
    /// qualified with dots or OF: ca.GetString("PG-ECHO-CODE OF PG-ECHO-TABLE(3)").
    /// </summary>
    public class CommArea
    {
        public readonly CobolLayout Layout;
        public readonly byte[] Buffer;

        public CommArea(CobolLayout layout)
        {
            Layout = layout;
            Buffer = new byte[layout.Length];
            Initialise();
        }

        /// <summary>Loads (and caches) the copybook and returns an initialised COMMAREA.</summary>
        public static CommArea For(string copybookName) { return new CommArea(CopybookParser.Load(copybookName)); }

        public int Length { get { return Buffer.Length; } }

        public void Initialise() { CobolCodec.Initialise(Buffer, Layout.Record); }

        public CobolField Field(string name) { int o; return Layout.Resolve(name, out o); }
        public bool Has(string name) { return Layout.Has(name.Trim()); }

        // ---------------------------------------------------------- accessors
        public string GetString(string name)
        {
            int o; CobolField f = Layout.Resolve(name, out o);
            if (f.IsGroup || f.Type == CobolType.Alphanumeric)
                return CobolCodec.GetText(Buffer, o, f.ElementLength).TrimEnd();
            return Num(CobolCodec.GetNumber(Buffer, o, f));
        }

        public void SetString(string name, string value)
        {
            int o; CobolField f = Layout.Resolve(name, out o);
            if (f.IsNumeric)
            {
                decimal d;
                decimal.TryParse((value ?? "").Trim(), System.Globalization.NumberStyles.Any,
                                 System.Globalization.CultureInfo.InvariantCulture, out d);
                CobolCodec.PutNumber(Buffer, o, f, d);
            }
            else CobolCodec.PutText(Buffer, o, f.ElementLength, value);
        }

        public decimal GetDecimal(string name)
        {
            int o; CobolField f = Layout.Resolve(name, out o);
            return CobolCodec.GetNumber(Buffer, o, f);
        }

        public void SetDecimal(string name, decimal value)
        {
            int o; CobolField f = Layout.Resolve(name, out o);
            if (f.IsNumeric) CobolCodec.PutNumber(Buffer, o, f, value);
            else CobolCodec.PutText(Buffer, o, f.ElementLength, Num(value));
        }

        public int GetInt(string name) { return (int)decimal.Truncate(GetDecimal(name)); }
        public void SetInt(string name, int value) { SetDecimal(name, value); }

        /// <summary>Reads a PIC 9(8) field holding YYYYMMDD.</summary>
        public DateTime? GetDate(string name)
        {
            string s = ((int)GetDecimal(name)).ToString("00000000");
            DateTime d;
            return DateTime.TryParseExact(s, "yyyyMMdd", System.Globalization.CultureInfo.InvariantCulture,
                System.Globalization.DateTimeStyles.None, out d) ? (DateTime?)d : null;
        }

        public void SetDate(string name, DateTime value)
        {
            SetDecimal(name, decimal.Parse(value.ToString("yyyyMMdd")));
        }

        public object Get(string name)
        {
            int o; CobolField f = Layout.Resolve(name, out o);
            return f.IsNumeric ? (object)CobolCodec.GetNumber(Buffer, o, f) : GetString(name);
        }

        public void Set(string name, object value)
        {
            if (value is DateTime) SetDate(name, (DateTime)value);
            else if (value is decimal || value is int || value is long || value is double)
                SetDecimal(name, Convert.ToDecimal(value));
            else SetString(name, value == null ? "" : Convert.ToString(value));
        }

        // ------------------------------------------------------ OCCURS tables
        /// <summary>
        /// Copies the first <paramref name="rowCount"/> occurrences of an OCCURS group
        /// into a DataTable (one column per elementary child, in copybook order).
        /// </summary>
        public DataTable GetTable(string groupName, int rowCount)
        {
            int baseOffset; CobolField g = Layout.Resolve(groupName, out baseOffset);
            DataTable t = new DataTable(g.Name);
            foreach (CobolField c in g.Children)
                if (!c.IsGroup && !c.IsFiller)
                    t.Columns.Add(c.Name, c.IsNumeric ? typeof(decimal) : typeof(string));
            int n = Math.Min(rowCount, g.Occurs);
            for (int i = 0; i < n; i++)
            {
                DataRow r = t.NewRow();
                int rowAt = baseOffset + i * g.ElementLength;
                foreach (CobolField c in g.Children)
                {
                    if (c.IsGroup || c.IsFiller) continue;
                    int at = rowAt + (c.Offset - g.Offset);
                    r[c.Name] = c.IsNumeric
                        ? (object)CobolCodec.GetNumber(Buffer, at, c)
                        : CobolCodec.GetText(Buffer, at, c.ElementLength).TrimEnd();
                }
                t.Rows.Add(r);
            }
            return t;
        }

        /// <summary>Writes one occurrence of an OCCURS group from a name/value list.</summary>
        public void SetRow(string groupName, int index, params object[] nameValuePairs)
        {
            for (int i = 0; i + 1 < nameValuePairs.Length; i += 2)
                Set(Convert.ToString(nameValuePairs[i]) + "(" + index + ")", nameValuePairs[i + 1]);
        }

        public string HexDump() { return CobolCodec.HexDump(Buffer, Buffer.Length); }

        public override string ToString()
        {
            return Layout.CopybookName + " (" + Buffer.Length + " bytes)";
        }

        private static string Num(decimal d)
        {
            return d.ToString("0.######", System.Globalization.CultureInfo.InvariantCulture);
        }
    }
}
