using System;
using System.Collections.Generic;
using System.Text;

namespace STERP.CobolInterop
{
    public enum CobolType
    {
        Group,
        Alphanumeric,       // PIC X(n)
        Unsigned,           // PIC 9(n)[V9(m)]
        ZonedTrailing,      // PIC S9(n)   - sign over-punched on the last digit
        ZonedLeading,       // PIC S9(n) SIGN LEADING
        ZonedTrailingSep,   // PIC S9(n) SIGN TRAILING SEPARATE
        ZonedLeadingSep,    // PIC S9(n) SIGN LEADING SEPARATE
        Packed,             // COMP-3
        Binary              // PIC S9(9) COMP-5 (4 bytes, native byte order)
    }

    /// <summary>One elementary item or group of a COMMAREA copybook.</summary>
    public class CobolField
    {
        public string Name = "";
        public int Level;
        public CobolType Type = CobolType.Group;
        public string Picture = "";
        public int Offset;          // absolute byte offset of occurrence 1
        public int ElementLength;   // bytes of ONE occurrence (incl. children)
        public int Occurs = 1;
        public int Digits;
        public int Scale;
        public bool Signed;
        public string Redefines;
        public CobolField Parent;
        public readonly List<CobolField> Children = new List<CobolField>();

        /// <summary>Total bytes taken by the item (all occurrences).</summary>
        public int Length { get { return ElementLength * Occurs; } }
        public bool IsGroup { get { return Type == CobolType.Group; } }
        public bool IsNumeric { get { return Type != CobolType.Group && Type != CobolType.Alphanumeric; } }
        public bool IsFiller { get { return Name == "FILLER"; } }

        /// <summary>Group-qualified name, e.g. DFHCOMMAREA.PG-BODY.PG-ECHO-TABLE.PG-ECHO-QTY</summary>
        public string Path
        {
            get { return Parent == null ? Name : Parent.Path + "." + Name; }
        }

        /// <summary>Occurring ancestors (outermost first) that a subscript applies to.</summary>
        public List<CobolField> Dimensions()
        {
            List<CobolField> d = new List<CobolField>();
            for (CobolField f = this; f != null; f = f.Parent) if (f.Occurs > 1) d.Insert(0, f);
            return d;
        }

        public override string ToString()
        {
            return string.Format("{0:00} {1,-24} off {2,5} len {3,4} {4} {5}",
                Level, Name, Offset, Length, Type, Picture);
        }
    }

    /// <summary>A parsed copybook: the 01 record plus a name index for fast lookup.</summary>
    public class CobolLayout
    {
        public string CopybookName = "";
        public CobolField Record;
        public readonly List<CobolField> Fields = new List<CobolField>();
        private readonly Dictionary<string, List<CobolField>> _index =
            new Dictionary<string, List<CobolField>>(StringComparer.OrdinalIgnoreCase);

        public int Length { get { return Record == null ? 0 : Record.Length; } }

        public void Index(CobolField f)
        {
            Fields.Add(f);
            List<CobolField> l;
            if (!_index.TryGetValue(f.Name, out l)) { l = new List<CobolField>(); _index[f.Name] = l; }
            l.Add(f);
        }

        /// <summary>
        /// Resolves "PG-ECHO-QTY", "PG-ECHO-QTY(2)", "PG-ECHO-TABLE(2).PG-ECHO-CODE",
        /// "PG-BODY.PG-ECHO-AMOUNT" or the COBOL form "PG-ECHO-CODE OF PG-ECHO-TABLE".
        /// Returns the field and the byte offset of the addressed occurrence.
        /// </summary>
        public CobolField Resolve(string reference, out int offset)
        {
            if (reference == null) throw new ArgumentNullException("reference");
            string r = reference.Trim().ToUpperInvariant();
            string[] parts;
            if (r.Contains(" OF ") || r.Contains(" IN "))
            {
                parts = r.Replace(" IN ", " OF ").Split(new[] { " OF " }, StringSplitOptions.RemoveEmptyEntries);
                Array.Reverse(parts);           // COBOL writes the item first, the group last
            }
            else parts = r.Split('.');

            List<int> subs = new List<int>();
            CobolField found = null;
            foreach (string part in parts)
            {
                string p = part.Trim();
                if (p.Length == 0) continue;
                int br = p.IndexOf('(');
                if (br > 0)
                {
                    foreach (string n in p.Substring(br).Replace('(', ' ').Replace(')', ' ').Replace(',', ' ')
                                          .Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries))
                        subs.Add(int.Parse(n));
                    p = p.Substring(0, br).Trim();
                }
                CobolField c = Find(p, found);
                if (c == null)
                    throw new ArgumentException("Field " + p + " is not in copybook " + CopybookName);
                found = c;
            }
            if (found == null) throw new ArgumentException("Empty field reference");

            offset = found.Offset;
            List<CobolField> dims = found.Dimensions();
            for (int i = 0; i < dims.Count; i++)
            {
                int ix = i < subs.Count ? subs[i] : 1;
                if (ix < 1 || ix > dims[i].Occurs)
                    throw new ArgumentOutOfRangeException("reference",
                        "Subscript " + ix + " is outside OCCURS " + dims[i].Occurs + " of " + dims[i].Name);
                offset += (ix - 1) * dims[i].ElementLength;
            }
            return found;
        }

        private static bool IsAncestor(CobolField maybeAncestor, CobolField f)
        {
            for (CobolField p = f.Parent; p != null; p = p.Parent) if (ReferenceEquals(p, maybeAncestor)) return true;
            return false;
        }

        /// <summary>Finds a field by name, optionally restricted to the subtree of <paramref name="within"/>.</summary>
        private CobolField Find(string name, CobolField within)
        {
            List<CobolField> l;
            if (!_index.TryGetValue(name, out l)) return null;
            if (within == null) return l[0];
            foreach (CobolField f in l) if (IsAncestor(within, f)) return f;
            return null;
        }

        public CobolField Field(string name) { int o; return Resolve(name, out o); }
        public bool Has(string name) { return _index.ContainsKey(name.Trim()); }

        /// <summary>Printable map - used by the engine diagnostics form and the tests.</summary>
        public string Map()
        {
            StringBuilder sb = new StringBuilder();
            sb.AppendLine(CopybookName + "  length " + Length);
            foreach (CobolField f in Fields) sb.AppendLine(new string(' ', f.Level / 2) + f);
            return sb.ToString();
        }
    }
}
