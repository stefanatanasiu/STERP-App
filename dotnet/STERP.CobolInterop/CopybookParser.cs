using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using STERP.Common;

namespace STERP.CobolInterop
{
    /// <summary>
    /// Reads a COMMAREA copybook (.cpy from bin\x86\copy) and works out the byte
    /// layout the COBOL compiler would produce.  Supports the field types allowed in
    /// a COMMAREA by CONVENTIONS 3.3: PIC X / 9 / S9 (trailing, SIGN LEADING or
    /// TRAILING SEPARATE), V, COMP-3, COMP-5, groups, OCCURS n, FILLER, REDEFINES,
    /// nested COPY.  88 levels and VALUE clauses are ignored.  Layouts are cached.
    /// </summary>
    public static class CopybookParser
    {
        private static readonly Dictionary<string, CobolLayout> Cache =
            new Dictionary<string, CobolLayout>(StringComparer.OrdinalIgnoreCase);
        private static readonly object Padlock = new object();

        public static string CopybookDirectory = SterpConfig.CopyDir;

        public static CobolLayout Load(string copybookName)
        {
            string key = copybookName.Trim().ToUpperInvariant();
            lock (Padlock)
            {
                CobolLayout l;
                if (Cache.TryGetValue(key, out l)) return l;
                l = Parse(key, ReadSource(key, 0));
                Cache[key] = l;
                return l;
            }
        }

        public static void ClearCache() { lock (Padlock) Cache.Clear(); }

        private static string FileOf(string name)
        {
            string f = Path.Combine(CopybookDirectory, name + ".cpy");
            if (File.Exists(f)) return f;
            f = Path.Combine(CopybookDirectory, name.ToUpperInvariant() + ".CPY");
            if (File.Exists(f)) return f;
            throw new FileNotFoundException("Copybook " + name + " not found in " + CopybookDirectory);
        }

        /// <summary>Source text with comments removed, continuation joined and nested COPY expanded.</summary>
        private static List<string> ReadSource(string name, int depth)
        {
            if (depth > 8) throw new InvalidOperationException("COPY nesting too deep at " + name);
            List<string> stmts = new List<string>();
            StringBuilder cur = new StringBuilder();
            foreach (string raw in File.ReadAllLines(FileOf(name)))
            {
                string t = raw;
                if (t.Length > 6)
                {
                    string seq = t.Substring(0, 6).Trim();
                    bool seqArea = seq.Length == 0 || IsDigits(seq);
                    if (seqArea)
                    {
                        if (t[6] == '*' || t[6] == '/' || t[6] == 'D') continue;   // comment / debug line
                        if (t.Length > 72) t = t.Substring(0, 72);
                        t = t.Substring(6);
                    }
                }
                int c = t.IndexOf("*>", StringComparison.Ordinal);
                if (c >= 0) t = t.Substring(0, c);
                t = t.Trim();
                if (t.Length == 0) continue;
                cur.Append(' ').Append(t);
                while (true)
                {
                    string s = cur.ToString();
                    int dot = s.IndexOf('.');
                    if (dot < 0) break;
                    string sentence = s.Substring(0, dot).Trim();
                    cur.Remove(0, dot + 1);
                    if (sentence.Length == 0) continue;
                    string[] w = Words(sentence);
                    if (w.Length >= 2 && w[0] == "COPY") stmts.AddRange(ReadSource(w[1], depth + 1));
                    else stmts.Add(sentence);
                }
            }
            if (cur.ToString().Trim().Length > 0) stmts.Add(cur.ToString().Trim());
            return stmts;
        }

        private static string[] Words(string s)
        {
            return s.ToUpperInvariant().Replace("\t", " ")
                    .Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
        }

        private static bool IsDigits(string s)
        {
            foreach (char c in s) if (c < '0' || c > '9') return false;
            return s.Length > 0;
        }

        private static CobolLayout Parse(string name, List<string> sentences)
        {
            CobolLayout layout = new CobolLayout();
            layout.CopybookName = name;
            List<CobolField> stack = new List<CobolField>();
            List<CobolField> redefined = new List<CobolField>();

            foreach (string sentence in sentences)
            {
                string[] w = Words(sentence);
                int level;
                if (w.Length == 0 || !int.TryParse(w[0], out level)) continue;
                if (level == 88 || level == 66) continue;                 // conditions / renames: no storage

                CobolField f = new CobolField { Level = level };
                int i = 1;
                if (i < w.Length && w[i] != "PIC" && w[i] != "PICTURE" && w[i] != "OCCURS" &&
                    w[i] != "REDEFINES" && w[i] != "USAGE" && w[i] != "COMP-3" && w[i] != "COMP-5")
                { f.Name = w[i]; i++; }
                else f.Name = "FILLER";

                string pic = null; CobolType usage = CobolType.Group; string sign = null; bool sep = false;
                for (; i < w.Length; i++)
                {
                    string t = w[i];
                    switch (t)
                    {
                        case "PIC":
                        case "PICTURE": if (i + 1 < w.Length) { if (w[i + 1] == "IS") i++; pic = w[++i]; } break;
                        case "OCCURS": if (i + 1 < w.Length) f.Occurs = int.Parse(w[++i]); break;
                        case "REDEFINES": if (i + 1 < w.Length) f.Redefines = w[++i]; break;
                        case "COMP-3":
                        case "COMPUTATIONAL-3":
                        case "PACKED-DECIMAL": usage = CobolType.Packed; break;
                        case "COMP-5":
                        case "COMPUTATIONAL-5": usage = CobolType.Binary; break;
                        case "SIGN": if (i + 1 < w.Length && w[i + 1] == "IS") i++; break;
                        case "LEADING": sign = "L"; break;
                        case "TRAILING": sign = "T"; break;
                        case "SEPARATE": sep = true; break;
                        case "VALUE":
                        case "VALUES": i = w.Length; break;              // ignored by the client
                    }
                }

                Describe(f, pic, usage, sign, sep);

                while (stack.Count > 0 && stack[stack.Count - 1].Level >= level) stack.RemoveAt(stack.Count - 1);
                if (stack.Count > 0)
                {
                    f.Parent = stack[stack.Count - 1];
                    f.Parent.Children.Add(f);
                }
                else if (layout.Record == null) layout.Record = f;

                if (f.Redefines != null) redefined.Add(f);
                stack.Add(f);
                layout.Index(f);
            }

            if (layout.Record == null) throw new InvalidOperationException("Copybook " + name + " has no 01 level");
            Place(layout.Record, 0);
            foreach (CobolField f in redefined) Overlay(layout, f);
            return layout;
        }

        /// <summary>Turns the PICTURE / USAGE words into a type, digit count and scale.</summary>
        private static void Describe(CobolField f, string pic, CobolType usage, string sign, bool separate)
        {
            if (pic == null) { f.Type = CobolType.Group; return; }
            f.Picture = pic;
            int intDigits = 0, decDigits = 0; bool afterV = false, signed = false, alpha = false;
            for (int p = 0; p < pic.Length; p++)
            {
                char c = pic[p];
                int rep = 1;
                if (p + 1 < pic.Length && pic[p + 1] == '(')
                {
                    int close = pic.IndexOf(')', p);
                    rep = int.Parse(pic.Substring(p + 2, close - p - 2));
                    p = close;
                }
                switch (c)
                {
                    case 'S': signed = true; break;
                    case 'V': afterV = true; break;
                    case '9': if (afterV) decDigits += rep; else intDigits += rep; break;
                    case 'X':
                    case 'A': alpha = true; intDigits += rep; break;
                }
            }
            f.Digits = intDigits + decDigits;
            f.Scale = decDigits;
            f.Signed = signed;

            if (alpha) { f.Type = CobolType.Alphanumeric; f.ElementLength = f.Digits; return; }
            if (usage == CobolType.Packed) { f.Type = CobolType.Packed; f.ElementLength = f.Digits / 2 + 1; return; }
            if (usage == CobolType.Binary) { f.Type = CobolType.Binary; f.ElementLength = 4; return; }
            if (!signed) { f.Type = CobolType.Unsigned; f.ElementLength = f.Digits; return; }
            if (separate)
            {
                f.Type = sign == "L" ? CobolType.ZonedLeadingSep : CobolType.ZonedTrailingSep;
                f.ElementLength = f.Digits + 1;
            }
            else
            {
                f.Type = sign == "L" ? CobolType.ZonedLeading : CobolType.ZonedTrailing;
                f.ElementLength = f.Digits;
            }
        }

        /// <summary>Second pass: assign offsets and roll group lengths up.</summary>
        private static int Place(CobolField f, int offset)
        {
            f.Offset = offset;
            if (f.Children.Count == 0) return f.Length;
            int cur = offset, max = 0;
            foreach (CobolField c in f.Children)
            {
                if (c.Redefines != null) continue;                   // placed by Overlay
                int n = Place(c, cur);
                cur += n; max += n;
            }
            f.ElementLength = max;
            return f.Length;
        }

        private static void Overlay(CobolLayout layout, CobolField f)
        {
            CobolField target = null;
            if (f.Parent != null)
                foreach (CobolField c in f.Parent.Children)
                    if (string.Equals(c.Name, f.Redefines, StringComparison.OrdinalIgnoreCase)) { target = c; break; }
            if (target == null) return;
            Place(f, target.Offset);
        }
    }
}
