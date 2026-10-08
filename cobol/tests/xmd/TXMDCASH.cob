      *>================================================================
      *> PROGRAM-ID : TXMDCASH
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : CROSS MODULE CONTROLS (XMD) UNIT TESTS
      *> PURPOSE    : CASHFCST - THE DATED CASH FORECAST.  THE TEST
      *>              RUNS THE JOB FOR STC OVER 01/10/2026 TO
      *>              31/12/2026 AND PROVES THAT IT COMPLETES WITH
      *>              RETURN CODE 0 AND NAMES ITS SPOOL FILE.  IT
      *>              THEN PUTS ONE OPEN PURCHASE LEDGER ITEM UNDER AN
      *>              IN FLIGHT PAYMENT INSTRUCTION AND PROVES THE NO
      *>              DOUBLE COUNTING RULE: THAT ITEM LEAVES THE
      *>              CONTRACTUAL COLUMN, THE INSTRUCTION APPEARS IN
      *>              THE CONFIRMED COLUMN, AND THE PAYABLES TOTAL IS
      *>              THE ONE OR THE OTHER BUT NEVER BOTH.  THE
      *>              FIXTURE IS PUT BACK AS IT WAS AT THE END.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: RPT-004
      *> AC: CASHFCST CO=STC FROM=20261001 TO=20261231 ends RC 0 and
      *> AC: prints STERP-I-SPOOL with the listing it wrote
      *> AC: an ap.OpenItem carried by an in flight PaymentInstruction
      *> AC: is forecast once, as CONFIRMED, and its ledger residual
      *> AC: is not added to the contractual payments as well
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXMDCASH.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CAP-FILE ASSIGN TO DYNAMIC WS-CAP-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CAP-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  CAP-FILE.
       01  CAP-REC                     PIC X(200).
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       01  WS-ROW.
           05  WS-COL OCCURS 10 TIMES  PIC X(120).
       01  WS-WORK.
           05  WS-DB-UP                PIC X VALUE "N".
           05  WS-FIXTURE              PIC X VALUE "N".
           05  WS-CMD                  PIC X(400) VALUE SPACES.
           05  WS-ROOT                 PIC X(200) VALUE SPACES.
           05  WS-CAP-NAME             PIC X(240) VALUE SPACES.
           05  WS-CAP-STATUS           PIC X(2) VALUE "00".
           05  WS-SYSRC                PIC S9(9) COMP-5 VALUE 0.
           05  WS-DONE                 PIC X VALUE "N".
           05  WS-SEEN-PROOF           PIC X VALUE "N".
           05  WS-SEEN-SPOOL           PIC X VALUE "N".
           05  WS-SPOOL-LINE           PIC X(200) VALUE SPACES.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-PI-ID                PIC X(20) VALUE SPACES.
           05  WS-OLD-STATUS           PIC X(20) VALUE SPACES.
           05  WS-OLD-VDATE            PIC X(8) VALUE SPACES.
           05  WS-OLD-LINK             PIC X(20) VALUE SPACES.
           05  WS-BASE-SUPP            PIC S9(7) VALUE 0.
       01  WS-ACT.
           05  A-ITEMS                 PIC S9(7) VALUE 0.
           05  A-CONF                  PIC S9(7) VALUE 0.
           05  A-SUPP                  PIC S9(7) VALUE 0.
           05  A-OUT                   PIC S9(13)V99 VALUE 0.
       01  WS-EXP.
           05  E-ITEMS                 PIC S9(7) VALUE 0.
           05  E-ITEM-AMT              PIC S9(13)V99 VALUE 0.
           05  E-CONF                  PIC S9(7) VALUE 0.
           05  E-CONF-AMT              PIC S9(13)V99 VALUE 0.
           05  E-SUPP                  PIC S9(7) VALUE 0.
           05  E-SUPP-AMT              PIC S9(13)V99 VALUE 0.
           05  E-ONCE                  PIC S9(13)V99 VALUE 0.
           05  E-TWICE                 PIC S9(13)V99 VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TXMDCASH cash forecast" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 1000-PLAIN-RUN
               PERFORM 2000-FIXTURE THRU 2000-EXIT
               IF WS-FIXTURE = "Y"
                   PERFORM 3000-NO-DOUBLE-COUNT
                   PERFORM 4000-RESTORE
               ELSE
                   MOVE "payment instruction fixture not on file"
                       TO TK-DETAIL
                   MOVE "payable under an instruction counted once"
                       TO TK-NAME
                   PERFORM TK-SKIP
               END-IF
           END-IF
           PERFORM TK-END
           STOP RUN.

       0100-PROBE-DATABASE.
           MOVE "N" TO WS-DB-UP
           MOVE SPACES TO SQLIO-STMT
           MOVE "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                TO SQLIO-STMT(1:44)
           MOVE "fin.BankAccount" TO SQLIO-STMT(45:15)
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-OK
               MOVE "Y" TO WS-DB-UP
           END-IF.

      *>----------------------------------------------------------------
      *> REQ: RPT-004  the report has to run clean on the defaults a
      *> treasury clerk would type, and say where it put its listing
      *>----------------------------------------------------------------
       1000-PLAIN-RUN.
           PERFORM 8200-RUN-JOB
           MOVE "the forecast ends with return code 0 for STC"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-SYSRC TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8400-READ-CAPTURE THRU 8400-EXIT
           MOVE "the run names its spool file" TO TK-NAME
           IF WS-SEEN-SPOOL = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE "no STERP-I-SPOOL line was printed" TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "the spool line points at a CASHFCST listing"
               TO TK-NAME
           MOVE 0 TO WS-I
           IF WS-SPOOL-LINE NOT = SPACES
               INSPECT WS-SPOOL-LINE TALLYING WS-I
                   FOR ALL "CASHFCST"
           END-IF
           IF WS-I > 0
               PERFORM TK-PASS
           ELSE
               MOVE WS-SPOOL-LINE(1:100) TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "the run prints its counted once proof line"
               TO TK-NAME
           IF WS-SEEN-PROOF = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE "no STERP-I-0404 line was printed" TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> REQ: RPT-004  the fixture: one settled instruction is put
      *> back in flight inside the window and one open payable is
      *> hung on it.  its own values are kept so that the ledger is
      *> left exactly as it was found
      *>----------------------------------------------------------------
       2000-FIXTURE.
           MOVE "N" TO WS-FIXTURE
           PERFORM 8600-EXPECTED
           MOVE E-SUPP TO WS-BASE-SUPP
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(20),"
                  "p.PaymentInstructionId), p.Status, ISNULL("
                  "CONVERT(char(8),p.ValueDate,112),'') FROM "
                  "ap.PaymentInstruction p WHERE p.CompanyCode = "
                  "'STC' AND p.InstructionRef = 'PY-6-0007-01'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 3 TO SQLIO-COL-CNT
           PERFORM 8000-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF NOT SQLIO-OK
               GO TO 2000-EXIT
           END-IF
           MOVE WS-COL(1) TO WS-PI-ID
           MOVE WS-COL(2) TO WS-OLD-STATUS
           MOVE WS-COL(3) TO WS-OLD-VDATE
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(CONVERT(varchar(20),"
                  "o.PaymentInstructionId),'') FROM ap.OpenItem o "
                  "WHERE o.CompanyCode = 'STC' AND "
                  "o.DocumentNumber = 'PI-26-0012'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF NOT SQLIO-OK
               GO TO 2000-EXIT
           END-IF
           MOVE WS-COL(1) TO WS-OLD-LINK
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PaymentInstruction SET Status = "
                  "'SUBMITTED', ValueDate = CONVERT(date,"
                  "'20261015',112) WHERE CompanyCode = 'STC' AND "
                  "InstructionRef = 'PY-6-0007-01'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8500-EXEC-SQL
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.OpenItem SET PaymentInstructionId = ? "
                  "WHERE CompanyCode = 'STC' AND DocumentNumber = "
                  "'PI-26-0012'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-PI-ID TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           PERFORM 8500-EXEC-SQL
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE "Y" TO WS-FIXTURE.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: RPT-004  the rule itself.  the payables side of the
      *> forecast is the open items no instruction carries PLUS the
      *> in flight instructions - never the same money twice
      *>----------------------------------------------------------------
       3000-NO-DOUBLE-COUNT.
           PERFORM 8600-EXPECTED
           COMPUTE E-ONCE = E-ITEM-AMT + E-CONF-AMT
           COMPUTE E-TWICE = E-ONCE + E-SUPP-AMT
           MOVE "the fixture leaves one payable under an instruction"
               TO TK-NAME
           COMPUTE TK-EXPECTED-N = WS-BASE-SUPP + 1
           MOVE E-SUPP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8200-RUN-JOB
           MOVE "the forecast still ends with return code 0"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-SYSRC TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8400-READ-CAPTURE THRU 8400-EXIT
           MOVE "the superseded payable is reported as superseded"
               TO TK-NAME
           MOVE E-SUPP TO TK-EXPECTED-N
           MOVE A-SUPP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the contractual payables exclude that open item"
               TO TK-NAME
           MOVE E-ITEMS TO TK-EXPECTED-N
           MOVE A-ITEMS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the instruction is forecast as a confirmed payment"
               TO TK-NAME
           MOVE E-CONF TO TK-EXPECTED-N
           MOVE A-CONF TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the payables total counts each obligation once"
               TO TK-NAME
           MOVE E-ONCE TO TK-EXPECTED-N
           MOVE A-OUT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the superseded residual is not added a second time"
               TO TK-NAME
           IF E-SUPP-AMT > 0 AND A-OUT NOT = E-TWICE
               PERFORM TK-PASS
           ELSE
               MOVE SPACES TO TK-DETAIL
               MOVE E-TWICE TO TK-NUM-ED
               STRING "double counted total would be "
                   FUNCTION TRIM(TK-NUM-ED)
                   DELIMITED BY SIZE INTO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   the ledger goes back exactly as it was found
       4000-RESTORE.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.OpenItem SET PaymentInstructionId = "
                  "CASE WHEN ? = '' THEN NULL ELSE CONVERT(bigint,"
                  "?) END WHERE CompanyCode = 'STC' AND "
                  "DocumentNumber = 'PI-26-0012'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-OLD-LINK TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-OLD-LINK TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 8500-EXEC-SQL
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PaymentInstruction SET Status = ?, "
                  "ValueDate = CASE WHEN ? = '' THEN NULL ELSE "
                  "CONVERT(date,?,112) END WHERE CompanyCode = "
                  "'STC' AND InstructionRef = 'PY-6-0007-01'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-OLD-STATUS TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-OLD-VDATE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE WS-OLD-VDATE TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           PERFORM 8500-EXEC-SQL
           MOVE 0 TO SQLIO-PARM-CNT
           PERFORM 8600-EXPECTED
           MOVE "the fixture is handed back to the ledger"
               TO TK-NAME
           MOVE WS-BASE-SUPP TO TK-EXPECTED-N
           MOVE E-SUPP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       0900-SKIP-EVERYTHING.
           MOVE "STERP database is not deployed" TO TK-DETAIL
           MOVE "cash forecast (no database)" TO TK-NAME
           PERFORM TK-SKIP.

      *>----------------------------------------------------------------
      *> helpers
      *>----------------------------------------------------------------
       8000-WIDTHS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               MOVE 120 TO SQLIO-COL-WIDTH(WS-I)
           END-PERFORM.

      *>   the harness runs from bin\tests, so the root of the tree is
      *>   taken from %STERP_RUN% rather than assumed
       8200-RUN-JOB.
           MOVE SPACES TO WS-ROOT
           ACCEPT WS-ROOT FROM ENVIRONMENT "STERP_RUN"
           IF WS-ROOT = SPACES
               MOVE "." TO WS-ROOT
           ELSE
               MOVE SPACES TO WS-CMD
               STRING FUNCTION TRIM(WS-ROOT) "\.."
                   DELIMITED BY SIZE INTO WS-CMD
               MOVE WS-CMD(1:200) TO WS-ROOT
           END-IF
           MOVE SPACES TO WS-CAP-NAME
           STRING FUNCTION TRIM(WS-ROOT)
                  "\run\spool\TXMDCASH-RUN.txt"
               DELIMITED BY SIZE INTO WS-CAP-NAME
           MOVE SPACES TO WS-CMD
           STRING "cd /d """ FUNCTION TRIM(WS-ROOT) """ && "
                  "bin\x86\CASHFCST.exe CO=STC FROM=20261001 "
                  "TO=20261231 SCENARIO=BASE USER=TREASURY "
                  "> run\spool\TXMDCASH-RUN.txt"
               DELIMITED BY SIZE INTO WS-CMD
           MOVE 0 TO WS-SYSRC
           CALL "SYSTEM" USING WS-CMD
           MOVE RETURN-CODE TO WS-SYSRC
           MOVE 0 TO RETURN-CODE.

      *>   the proof line is printed in fixed columns so that it can
      *>   be read back mechanically (STERP-I-0404)
       8400-READ-CAPTURE.
           MOVE "N" TO WS-SEEN-PROOF
           MOVE "N" TO WS-SEEN-SPOOL
           MOVE SPACES TO WS-SPOOL-LINE
           MOVE 0 TO A-ITEMS A-CONF A-SUPP
           MOVE 0 TO A-OUT
           MOVE 0 TO WS-I
           OPEN INPUT CAP-FILE
           IF WS-CAP-STATUS NOT = "00"
               GO TO 8400-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-DONE = "Y"
               READ CAP-FILE
                   AT END MOVE "Y" TO WS-DONE
               END-READ
               IF WS-DONE = "N"
                   IF CAP-REC(1:12) = "STERP-I-0404"
                       MOVE "Y" TO WS-SEEN-PROOF
                       COMPUTE A-ITEMS =
                           FUNCTION NUMVAL(CAP-REC(20:5))
                       COMPUTE A-CONF =
                           FUNCTION NUMVAL(CAP-REC(31:5))
                       COMPUTE A-SUPP =
                           FUNCTION NUMVAL(CAP-REC(42:5))
                       COMPUTE A-OUT =
                           FUNCTION NUMVAL(CAP-REC(55:15))
                   END-IF
                   IF CAP-REC(1:13) = "STERP-I-SPOOL"
                       MOVE "Y" TO WS-SEEN-SPOOL
                       MOVE CAP-REC(1:200) TO WS-SPOOL-LINE
                   END-IF
               END-IF
           END-PERFORM
           CLOSE CAP-FILE.
       8400-EXIT.
           EXIT.

       8500-EXEC-SQL.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               DISPLAY "# SQL " SQLIO-ERRMSG(1:90) UPON SYSOUT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

      *>   the same three populations the report walks, read straight
      *>   from the ledger so that the listing is checked against the
      *>   data and not against itself
       8600-EXPECTED.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)), CONVERT("
                  "varchar(24),ISNULL(SUM(o.ResidualAmountGBP),0))"
                  " FROM ap.OpenItem o WHERE o.CompanyCode = 'STC' "
                  "AND o.SettlementStatus <> 'SETTLED' AND "
                  "o.ResidualAmountGBP <> 0 AND CONVERT(char(8),"
                  "o.DueDate,112) <= '20261231' AND NOT EXISTS ("
                  "SELECT 1 FROM ap.PaymentInstruction p WHERE "
                  "p.PaymentInstructionId = o.PaymentInstructionId "
                  "AND p.Status NOT IN ('SETTLED','CANCELLED',"
                  "'REJECTED','RETURNED'))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8700-TWO-VALUES
           COMPUTE E-ITEMS = FUNCTION NUMVAL(WS-COL(1))
           COMPUTE E-ITEM-AMT = FUNCTION NUMVAL(WS-COL(2))
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)), CONVERT("
                  "varchar(24),ISNULL(SUM(p.AmountGBP),0)) FROM "
                  "ap.PaymentInstruction p WHERE p.CompanyCode = "
                  "'STC' AND p.Status NOT IN ('SETTLED',"
                  "'CANCELLED','REJECTED','RETURNED') AND "
                  "p.ValueDate IS NOT NULL AND CONVERT(char(8),"
                  "p.ValueDate,112) <= '20261231'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8700-TWO-VALUES
           COMPUTE E-CONF = FUNCTION NUMVAL(WS-COL(1))
           COMPUTE E-CONF-AMT = FUNCTION NUMVAL(WS-COL(2))
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)), CONVERT("
                  "varchar(24),ISNULL(SUM(o.ResidualAmountGBP),0))"
                  " FROM ap.OpenItem o WHERE o.CompanyCode = 'STC' "
                  "AND o.SettlementStatus <> 'SETTLED' AND "
                  "CONVERT(char(8),o.DueDate,112) <= '20261231' "
                  "AND EXISTS (SELECT 1 FROM ap.PaymentInstruction "
                  "p WHERE p.PaymentInstructionId = "
                  "o.PaymentInstructionId AND p.Status NOT IN ("
                  "'SETTLED','CANCELLED','REJECTED','RETURNED'))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8700-TWO-VALUES
           COMPUTE E-SUPP = FUNCTION NUMVAL(WS-COL(1))
           COMPUTE E-SUPP-AMT = FUNCTION NUMVAL(WS-COL(2)).

       8700-TWO-VALUES.
           MOVE SPACES TO WS-COL(1)
           MOVE SPACES TO WS-COL(2)
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 8000-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF NOT SQLIO-OK
               MOVE "0" TO WS-COL(1)
               MOVE "0" TO WS-COL(2)
               DISPLAY "# SQL " SQLIO-ERRMSG(1:90) UPON SYSOUT
           END-IF.

       COPY TESTKITP.
