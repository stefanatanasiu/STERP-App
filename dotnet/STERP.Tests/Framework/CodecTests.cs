using System;
using STERP.CobolInterop;

namespace STERP.Tests
{
    /// <summary>Byte level checks of the zoned / packed / binary codecs (COM-011).</summary>
    public static class CodecTests
    {
        private static CobolField Fld(CobolType t, int digits, int scale, int len)
        {
            return new CobolField
            {
                Name = "WS-N", Type = t, Digits = digits, Scale = scale,
                ElementLength = len, Signed = t != CobolType.Unsigned
            };
        }

        [StTest("codec: unsigned zoned decimal round trip", Reqs = "COM-011")]
        public static void Unsigned()
        {
            CobolField f = Fld(CobolType.Unsigned, 8, 0, 8);
            byte[] b = new byte[8];
            CobolCodec.PutZoned(b, 0, f, 20261004m);
            St.Eq("20261004", CobolCodec.GetText(b, 0, 8), "PIC 9(8) is plain ASCII digits");
            St.Eq(20261004m, CobolCodec.GetZoned(b, 0, f), "decoded value");
        }

        [StTest("codec: signed zoned decimal uses the GnuCOBOL p..y over-punch", Reqs = "COM-011")]
        public static void ZonedTrailing()
        {
            CobolField f = Fld(CobolType.ZonedTrailing, 15, 6, 15);
            byte[] b = new byte[15];
            CobolCodec.PutZoned(b, 0, f, -12.5m);
            St.Eq("00000001250000", CobolCodec.GetText(b, 0, 14), "leading digits unchanged");
            St.Eq((byte)('p' + 0), b[14], "negative last digit 0 is encoded as 'p'");
            St.Eq(-12.5m, CobolCodec.GetZoned(b, 0, f), "decoded negative value");

            CobolCodec.PutZoned(b, 0, f, 999.123456m);
            St.Eq("000999123456", CobolCodec.GetText(b, 3, 12), "positive value stays as digits");
            St.Eq(999.123456m, CobolCodec.GetZoned(b, 0, f), "decoded positive value");
        }

        [StTest("codec: SIGN LEADING SEPARATE keeps the sign in its own byte", Reqs = "COM-011")]
        public static void LeadingSeparate()
        {
            CobolField f = Fld(CobolType.ZonedLeadingSep, 13, 2, 14);
            byte[] b = new byte[14];
            CobolCodec.PutZoned(b, 0, f, -2469.14m);
            St.Eq("-0000000246914", "-" + CobolCodec.GetText(b, 1, 13), "sign byte then 13 digits");
            St.Eq(-2469.14m, CobolCodec.GetZoned(b, 0, f), "decoded value");
            CobolCodec.PutZoned(b, 0, f, 1m);
            St.Eq("+", CobolCodec.GetText(b, 0, 1), "positive sign byte");
        }

        [StTest("codec: COMP-3 packed decimal with C/D sign nibble", Reqs = "COM-011")]
        public static void Packed()
        {
            CobolField f = Fld(CobolType.Packed, 13, 2, 7);
            byte[] b = new byte[7];
            CobolCodec.PutPacked(b, 0, f, 1234.56m);
            St.Eq("0000000123456C", Hex(b), "positive packed value");
            St.Eq(1234.56m, CobolCodec.GetPacked(b, 0, f), "decoded positive");
            CobolCodec.PutPacked(b, 0, f, -1234.56m);
            St.Eq("0000000123456D", Hex(b), "negative packed value");
            St.Eq(-1234.56m, CobolCodec.GetPacked(b, 0, f), "decoded negative");
        }

        [StTest("codec: COMP-5 is a native 4 byte integer", Reqs = "COM-011")]
        public static void Binary()
        {
            CobolField f = Fld(CobolType.Binary, 9, 0, 4);
            byte[] b = new byte[4];
            CobolCodec.PutBinary(b, 0, f, -123456789m);
            St.Eq(-123456789m, CobolCodec.GetBinary(b, 0, f), "decoded value");
            CobolCodec.PutBinary(b, 0, f, 1m);
            St.Eq("01000000", Hex(b), "little endian byte order");
        }

        [StTest("codec: PIC X is space padded and trimmed", Reqs = "COM-011")]
        public static void Text()
        {
            byte[] b = new byte[10];
            CobolCodec.PutText(b, 0, 10, "STC");
            St.Eq("STC       ", CobolCodec.GetText(b, 0, 10), "space padded to the picture length");
            CobolCodec.PutText(b, 0, 10, "LONGER THAN TEN");
            St.Eq("LONGER THA", CobolCodec.GetText(b, 0, 10), "truncated on the right like a MOVE");
        }

        private static string Hex(byte[] b)
        {
            System.Text.StringBuilder sb = new System.Text.StringBuilder();
            foreach (byte x in b) sb.Append(x.ToString("X2"));
            return sb.ToString();
        }
    }
}
