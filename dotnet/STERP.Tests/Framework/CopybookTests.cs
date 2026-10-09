using System;
using System.Data;
using STERP.CobolInterop;

namespace STERP.Tests
{
    /// <summary>Copybook parser: offsets, lengths and subscripted access (COM-011).</summary>
    public static class CopybookTests
    {
        [StTest("copybook: STCAHDR is the 400 byte standard header", Reqs = "COM-011,COM-017")]
        public static void Header()
        {
            CobolLayout l = CopybookParser.Load("STCAHDR");
            St.Eq(400, l.Length, "STCAHDR length");
            St.Eq(0, l.Field("CA-PROGRAM").Offset, "CA-PROGRAM offset");
            St.Eq(8, l.Field("CA-FUNCTION").Offset, "CA-FUNCTION offset");
            St.Eq(16, l.Field("CA-COMPANY").Offset, "CA-COMPANY offset");
            St.Eq(26, l.Field("CA-SITE").Offset, "CA-SITE offset");
            St.Eq(36, l.Field("CA-USER").Offset, "CA-USER offset");
            St.Eq(56, l.Field("CA-BUSINESS-DATE").Offset, "CA-BUSINESS-DATE offset");
            St.Eq(64, l.Field("CA-CORRELATION-ID").Offset, "CA-CORRELATION-ID offset");
            St.Eq(100, l.Field("CA-RETURN-CODE").Offset, "CA-RETURN-CODE offset");
            St.Eq(102, l.Field("CA-REASON-CODE").Offset, "CA-REASON-CODE offset");
            St.Eq(110, l.Field("CA-MESSAGE").Offset, "CA-MESSAGE offset");
            St.Eq(200, l.Field("CA-MESSAGE").Length, "CA-MESSAGE length");
            St.Eq(320, l.Field("CA-MSG-COUNT").Offset, "CA-MSG-COUNT offset");
        }

        [StTest("copybook: PINGCA offsets, OCCURS and usage clauses", Reqs = "COM-011")]
        public static void PingCa()
        {
            CobolLayout l = CopybookParser.Load("PINGCA");
            St.Eq(638, l.Length, "PINGCA length");
            St.Eq(400, l.Field("PG-BODY").Offset, "the body starts after the 400 byte header");
            St.Eq(20, l.Field("PG-ENGINE-VERSION").Length, "PG-ENGINE-VERSION");
            St.Eq(560, l.Field("PG-ECHO-AMOUNT").Offset, "PG-ECHO-AMOUNT offset");
            St.Eq(7, l.Field("PG-ECHO-AMOUNT").Length, "S9(11)V99 COMP-3 occupies 7 bytes");
            St.Eq(CobolType.Packed, l.Field("PG-ECHO-AMOUNT").Type, "COMP-3 recognised");
            St.Eq(14, l.Field("PG-ECHO-DOUBLE").Length, "S9(11)V99 SIGN LEADING SEPARATE occupies 14 bytes");
            St.Eq(CobolType.ZonedLeadingSep, l.Field("PG-ECHO-DOUBLE").Type, "SIGN LEADING SEPARATE recognised");
            St.Eq(3, l.Field("PG-ECHO-TABLE").Occurs, "OCCURS 3 TIMES");
            St.Eq(19, l.Field("PG-ECHO-TABLE").ElementLength, "one table entry is 19 bytes");
            St.Eq(15, l.Field("PG-ECHO-QTY").Length, "S9(9)V9(6) trailing sign occupies 15 bytes");
            St.Eq(6, l.Field("PG-ECHO-QTY").Scale, "six decimal places");
        }

        [StTest("commarea: subscripts and group qualification address the right bytes", Reqs = "COM-011")]
        public static void Subscripts()
        {
            CommArea ca = CommArea.For("PINGCA");
            St.Eq(638, ca.Length, "buffer length");
            ca.SetString("PG-ECHO-CODE(1)", "AAA");
            ca.SetString("PG-ECHO-CODE(2)", "BBB");
            ca.SetString("PG-ECHO-CODE(3)", "CCC");
            ca.SetDecimal("PG-ECHO-QTY(2)", -2.5m);
            St.Eq("BBB", ca.GetString("PG-ECHO-CODE(2)"), "second occurrence");
            St.Eq("CCC", ca.GetString("PG-ECHO-CODE OF PG-ECHO-TABLE(3)"), "COBOL OF qualification");
            St.Eq("AAA", ca.GetString("PG-ECHO-TABLE(1).PG-ECHO-CODE"), "dotted qualification");
            St.Eq(-2.5m, ca.GetDecimal("PG-ECHO-QTY(2)"), "subscripted decimal");
            St.Eq(0m, ca.GetDecimal("PG-ECHO-QTY(1)"), "neighbouring occurrence untouched");

            DataTable t = ca.GetTable("PG-ECHO-TABLE", 3);
            St.Eq(3, t.Rows.Count, "table rows");
            St.Eq(2, t.Columns.Count, "table columns");
            St.Eq("CCC", t.Rows[2]["PG-ECHO-CODE"], "third row code");
            St.Eq(-2.5m, t.Rows[1]["PG-ECHO-QTY"], "second row quantity");
        }

        [StTest("commarea: INITIALIZE leaves text as spaces and numbers as zero", Reqs = "COM-011")]
        public static void Initialised()
        {
            CommArea ca = CommArea.For("PINGCA");
            St.Eq("", ca.GetString("CA-PROGRAM"), "text field blank");
            St.Eq(0m, ca.GetDecimal("CA-BUSINESS-DATE"), "numeric field zero");
            St.Eq(0m, ca.GetDecimal("PG-ECHO-AMOUNT"), "packed field zero");
            St.Eq(0m, ca.GetDecimal("PG-ECHO-DOUBLE"), "sign separate field zero");
            St.True(ca.HexDump().Length > 100, "hex dump produced");
        }

        [StTest("commarea: INITIALIZE reaches every occurrence of an OCCURS group", Reqs = "COM-011")]
        public static void InitialisedOccurrences()
        {
            CommArea ca = CommArea.For("DESPATCHCA");
            CobolField g = ca.Field("DS-LINES");
            CobolField whse = ca.Field("DS-L-WHSE");
            for (int i = 0; i < g.Occurs; i++)
            {
                int at = whse.Offset + i * g.ElementLength;
                for (int k = 0; k < whse.ElementLength; k++)
                    if (ca.Buffer[at + k] != (byte)' ')
                    {
                        St.True(false, "DS-L-WHSE(" + (i + 1) + ") is not spaces (DSP0004 regression)");
                        return;
                    }
                St.Eq(0m, ca.GetDecimal("DS-L-QTY(" + (i + 1) + ")"), "DS-L-QTY(" + (i + 1) + ") zero");
            }
        }
    }
}
