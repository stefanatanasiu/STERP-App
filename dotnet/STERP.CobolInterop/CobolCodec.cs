using System;
using System.Text;

namespace STERP.CobolInterop
{
    /// <summary>
    /// Byte-level codecs for the COBOL data types allowed in a COMMAREA.
    /// GnuCOBOL on Windows stores DISPLAY items in ASCII: a positive zoned decimal
    /// uses plain digits, a negative one over-punches the sign digit as 0x70+digit
    /// ('p'..'y').  COMP-3 is standard packed decimal with a C/D/F sign nibble and
    /// COMP-5 is a native little-endian 4 byte integer.
    /// </summary>
    public static class CobolCodec
    {
        public static readonly Encoding Ansi = Encoding.GetEncoding(1252);

        private static decimal Shift(long unscaled, int scale)
        {
            decimal d = unscaled;
            for (int i = 0; i < scale; i++) d /= 10m;
            return d;
        }

        private static long Unshift(decimal value, int scale)
        {
            decimal d = value;
            for (int i = 0; i < scale; i++) d *= 10m;
            return (long)Math.Round(d, 0, MidpointRounding.AwayFromZero);
        }

        // ------------------------------------------------------------- text
        public static string GetText(byte[] b, int offset, int length)
        {
            return Ansi.GetString(b, offset, length);
        }

        public static void PutText(byte[] b, int offset, int length, string value)
        {
            for (int i = 0; i < length; i++) b[offset + i] = 0x20;
            if (string.IsNullOrEmpty(value)) return;
            byte[] src = Ansi.GetBytes(value);
            Array.Copy(src, 0, b, offset, Math.Min(src.Length, length));
        }

        // ------------------------------------------------------ zoned decimal
        /// <summary>Digit value of one DISPLAY byte; sets <paramref name="negative"/> when over-punched.</summary>
        private static int Zone(byte z, ref bool negative)
        {
            if (z >= 0x30 && z <= 0x39) return z - 0x30;              // '0'..'9'  positive
            if (z >= 0x70 && z <= 0x79) { negative = true; return z - 0x70; }   // 'p'..'y'  GnuCOBOL negative
            if (z >= 0x41 && z <= 0x49) return z - 0x40;              // 'A'..'I'  mainframe positive
            if (z >= 0x4A && z <= 0x52) { negative = true; return z - 0x49; }   // 'J'..'R'  mainframe negative
            if (z == 0x7B) return 0;                                  // '{'  +0
            if (z == 0x7D) { negative = true; return 0; }             // '}'  -0
            if (z == 0x20 || z == 0x00) return 0;                     // uninitialised
            throw new FormatException("Byte 0x" + z.ToString("X2") + " is not a valid zoned decimal digit");
        }

        public static decimal GetZoned(byte[] b, int offset, CobolField f)
        {
            int n = f.Digits, start = offset;
            bool neg = false;
            if (f.Type == CobolType.ZonedLeadingSep) { neg = b[offset] == (byte)'-'; start = offset + 1; }
            else if (f.Type == CobolType.ZonedTrailingSep) neg = b[offset + n] == (byte)'-';

            long v = 0;
            for (int i = 0; i < n; i++) v = v * 10 + Zone(b[start + i], ref neg);
            decimal d = Shift(v, f.Scale);
            return neg ? -d : d;
        }

        public static void PutZoned(byte[] b, int offset, CobolField f, decimal value)
        {
            int n = f.Digits, start = offset;
            bool neg = value < 0;
            long v = Math.Abs(Unshift(value, f.Scale));
            string s = v.ToString(System.Globalization.CultureInfo.InvariantCulture);
            if (s.Length > n) s = s.Substring(s.Length - n);          // COBOL truncates high-order digits
            s = s.PadLeft(n, '0');

            if (f.Type == CobolType.ZonedLeadingSep) { b[offset] = (byte)(neg ? '-' : '+'); start = offset + 1; }
            for (int i = 0; i < n; i++) b[start + i] = (byte)s[i];
            if (f.Type == CobolType.ZonedTrailingSep) b[start + n] = (byte)(neg ? '-' : '+');
            else if (neg && f.Type == CobolType.ZonedTrailing) b[start + n - 1] = (byte)(0x70 + (s[n - 1] - '0'));
            else if (neg && f.Type == CobolType.ZonedLeading) b[start] = (byte)(0x70 + (s[0] - '0'));
        }

        // --------------------------------------------------------- COMP-3
        public static decimal GetPacked(byte[] b, int offset, CobolField f)
        {
            int bytes = f.ElementLength;
            int nibbles = bytes * 2 - 1;
            long v = 0;
            for (int i = nibbles - f.Digits; i < nibbles; i++)
            {
                int nib = (i % 2 == 0) ? (b[offset + i / 2] >> 4) & 0x0F : b[offset + i / 2] & 0x0F;
                v = v * 10 + nib;
            }
            int sign = b[offset + bytes - 1] & 0x0F;
            decimal d = Shift(v, f.Scale);
            return (sign == 0x0D || sign == 0x0B) ? -d : d;
        }

        public static void PutPacked(byte[] b, int offset, CobolField f, decimal value)
        {
            int bytes = f.ElementLength, nibbles = bytes * 2 - 1;
            bool neg = value < 0;
            long v = Math.Abs(Unshift(value, f.Scale));
            string s = v.ToString(System.Globalization.CultureInfo.InvariantCulture);
            if (s.Length > f.Digits) s = s.Substring(s.Length - f.Digits);
            s = s.PadLeft(nibbles, '0');
            for (int i = 0; i < bytes; i++) b[offset + i] = 0;
            for (int i = 0; i < nibbles; i++)
            {
                int nib = s[i] - '0';
                if (i % 2 == 0) b[offset + i / 2] |= (byte)(nib << 4); else b[offset + i / 2] |= (byte)nib;
            }
            b[offset + bytes - 1] = (byte)((b[offset + bytes - 1] & 0xF0) |
                                           (f.Signed ? (neg ? 0x0D : 0x0C) : 0x0F));
        }

        // --------------------------------------------------------- COMP-5
        public static decimal GetBinary(byte[] b, int offset, CobolField f)
        {
            return Shift(BitConverter.ToInt32(b, offset), f.Scale);
        }

        public static void PutBinary(byte[] b, int offset, CobolField f, decimal value)
        {
            byte[] src = BitConverter.GetBytes((int)Unshift(value, f.Scale));
            Array.Copy(src, 0, b, offset, 4);
        }

        // --------------------------------------------------------- dispatch
        public static decimal GetNumber(byte[] b, int offset, CobolField f)
        {
            switch (f.Type)
            {
                case CobolType.Packed: return GetPacked(b, offset, f);
                case CobolType.Binary: return GetBinary(b, offset, f);
                case CobolType.Alphanumeric:
                case CobolType.Group:
                    decimal d; return decimal.TryParse(GetText(b, offset, f.ElementLength).Trim(),
                        System.Globalization.NumberStyles.Any,
                        System.Globalization.CultureInfo.InvariantCulture, out d) ? d : 0m;
                default: return GetZoned(b, offset, f);
            }
        }

        public static void PutNumber(byte[] b, int offset, CobolField f, decimal value)
        {
            switch (f.Type)
            {
                case CobolType.Packed: PutPacked(b, offset, f, value); break;
                case CobolType.Binary: PutBinary(b, offset, f, value); break;
                case CobolType.Alphanumeric:
                case CobolType.Group:
                    PutText(b, offset, f.ElementLength,
                        value.ToString(System.Globalization.CultureInfo.InvariantCulture)); break;
                default: PutZoned(b, offset, f, value); break;
            }
        }

        /// <summary>COBOL INITIALIZE: alphanumerics to spaces, numerics to zero.</summary>
        public static void Initialise(byte[] b, CobolField f) { Initialise(b, f, 0); }

        // Child offsets describe the first occurrence of every enclosing OCCURS group, so
        // each occurrence of a group is initialised by shifting its children (DSP0004:
        // DS-LINES(2..30) used to stay LOW-VALUES and read as "not blank" in COBOL).
        private static void Initialise(byte[] b, CobolField f, int shift)
        {
            for (int o = 0; o < f.Occurs; o++)
            {
                int delta = shift + o * f.ElementLength;
                if (f.IsGroup)
                {
                    foreach (CobolField c in f.Children) Initialise(b, c, delta);
                    continue;
                }
                int at = f.Offset + delta;
                if (f.Type == CobolType.Alphanumeric) PutText(b, at, f.ElementLength, "");
                else PutNumber(b, at, f, 0m);
            }
        }

        /// <summary>Classic 16-bytes-per-line dump for the COMMAREA trace.</summary>
        public static string HexDump(byte[] b, int length)
        {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < length; i += 16)
            {
                StringBuilder hex = new StringBuilder(), txt = new StringBuilder();
                for (int j = 0; j < 16; j++)
                {
                    if (i + j < length)
                    {
                        byte v = b[i + j];
                        hex.Append(v.ToString("X2")).Append(j == 7 ? '-' : ' ');
                        txt.Append(v >= 0x20 && v < 0x7F ? (char)v : '.');
                    }
                    else hex.Append("   ");
                }
                sb.AppendLine(i.ToString("X4") + "  " + hex + " |" + txt + "|");
            }
            return sb.ToString();
        }
    }
}
