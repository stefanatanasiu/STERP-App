      *>================================================================
      *> PROGRAM-ID : TXMDGAP
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : CROSS MODULE CONTROLS (XMD) UNIT TESTS
      *> PURPOSE    : THE COMPANY BOUNDARY AND THE EFFECTIVE CUT OVER
      *>              DATE IN THE LEGACY CONVERSION LOAD (CONVLOAD).
      *>              THE SUPPLIED CALEDON EXTRACT
      *>              data\legacy\CALAROPN.DAT NAMES CUSTOMERS K0001
      *>              TO K0003, WHICH ARE ON FILE IN CAL AND NOT IN
      *>              STC.  THE SAME FILE IS LOADED TWICE AT A CUT
      *>              OVER OF 28 FEBRUARY 2026:
      *>              RUN 1  CO=STC - NOTHING MAY BE ACCEPTED AND NO
      *>                     OPEN ITEM MAY REACH THE STELLAR LEDGER,
      *>                     BECAUSE A CUSTOMER CODE THAT BELONGS TO
      *>                     ANOTHER COMPANY AUTHORISES NOTHING HERE
      *>                     (XMD-011).
      *>              RUN 2  CO=CAL - ONLY THE TWO DOCUMENTS DATED ON
      *>                     OR BEFORE THE CUT OVER ARE CONVERTED; THE
      *>                     FOUR DATED AFTER IT ARE REFUSED, AND THE
      *>                     CONVERTED OPEN ITEM KEEPS THE LEGACY
      *>                     TERMS AND DOCUMENT DATE OF THE OLD
      *>                     SYSTEM RATHER THAN TODAY'S CUSTOMER
      *>                     MASTER (XMD-008).
      *>              THE BATCH REFERENCE CONV-CALAROPN-20260228 IS
      *>              THIS SUITE'S OWN AND IS REMOVED BEFORE AND
      *>              AFTER, SO THE SUITE IS REPEATABLE AND LEAVES
      *>              THE DEMONSTRATION LEDGER UNCHANGED.
      *>----------------------------------------------------------------
      *> 2026-10-07 STERP ORIGINAL
      *>================================================================
      *> REQ: XMD-008, XMD-011   AC: AC-100, AC-105
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXMDGAP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       01  WS-ROW.
           05  WS-COL OCCURS 10 TIMES  PIC X(120).
       01  WS-WORK.
           05  WS-DB-UP                PIC X VALUE "N".
           05  WS-CMD                  PIC X(400) VALUE SPACES.
           05  WS-ROOT                 PIC X(200) VALUE SPACES.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-DOC                  PIC X(20) VALUE SPACES.
           05  WS-BATCH                PIC X(40) VALUE
               "CONV-CALAROPN-20260228".
           05  WS-TOTAL                PIC S9(7) VALUE 0.
           05  WS-ACC                  PIC S9(7) VALUE 0.
           05  WS-REJ                  PIC S9(7) VALUE 0.
           05  WS-DUP                  PIC S9(7) VALUE 0.
           05  WS-ROWS                 PIC S9(7) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TXMDGAP conversion company boundary" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               MOVE "STC" TO WS-CO
               PERFORM 0200-CLEAN-UP
               MOVE "CAL" TO WS-CO
               PERFORM 0200-CLEAN-UP
               PERFORM 1000-OTHER-COMPANY
               PERFORM 2000-OWN-COMPANY
               MOVE "STC" TO WS-CO
               PERFORM 0200-CLEAN-UP
               MOVE "CAL" TO WS-CO
               PERFORM 0200-CLEAN-UP
           END-IF
           PERFORM TK-END
           STOP RUN.

       0100-PROBE-DATABASE.
           MOVE "N" TO WS-DB-UP
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "stg.ImportBatch"
               DELIMITED BY SIZE INTO SQLIO-STMT
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
      *> the suite owns one batch reference per company and the four
      *> legacy document numbers; nothing else is touched
      *>----------------------------------------------------------------
       0200-CLEAN-UP.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE r FROM stg.ImportRow r JOIN "
                  "stg.ImportBatch b ON b.BatchId = r.BatchId WHERE"
                  " b.CompanyCode = ? AND b.BatchRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-RUN-UPDATE
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM stg.ImportBatch WHERE CompanyCode = "
                  "? AND BatchRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-RUN-UPDATE
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM core.ExceptionItem WHERE "
                  "CompanyCode = ? AND DocumentRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-RUN-UPDATE
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM ar.OpenItem WHERE CompanyCode = ? "
                  "AND DocumentNumber IN ('SI-900101','SI-900102',"
                  "'SI-900103','SC-900104')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8150-RUN-ONE-PARM.

      *>----------------------------------------------------------------
      *> REQ: XMD-011  the Caledon extract offered to Stellar
      *> Components: similar codes in another company authorise
      *> nothing here, so nothing is converted and the sales ledger
      *> of the receiving company is left alone
      *>----------------------------------------------------------------
       1000-OTHER-COMPANY.
           MOVE "STC" TO WS-CO
           PERFORM 8200-RUN-JOB
           PERFORM 8300-READ-COUNTS
           MOVE "no record of another company's extract is accepted"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-ACC TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "every one of the eight records is refused"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE WS-REJ TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "%SI-900101%" TO WS-DOC
           PERFORM 8500-REJECT-REASON
           MOVE "a customer of the other company is not on file here"
               TO TK-NAME
           MOVE "Customer is not on file at the cut over"
               TO TK-EXPECTED-X
           MOVE WS-COL(1)(1:100) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8800-COUNT-OPEN-ITEMS
           MOVE "no open item reaches the receiving company ledger"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-ROWS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> REQ: XMD-008  the same extract at its own company and at an
      *> effective cut over of 28 February 2026.  Only what existed
      *> at the cut over is converted, and what is converted keeps
      *> the commercial inputs of the old system
      *>----------------------------------------------------------------
       2000-OWN-COMPANY.
           MOVE "CAL" TO WS-CO
           PERFORM 8200-RUN-JOB
           PERFORM 8300-READ-COUNTS
           MOVE "only documents dated at or before the cut over load"
               TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE WS-ACC TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "later activity is refused, not pulled into history"
               TO TK-NAME
           MOVE 5 TO TK-EXPECTED-N
           MOVE WS-REJ TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "%SI-900103%" TO WS-DOC
           PERFORM 8500-REJECT-REASON
           MOVE "the reason names the effective cut over date"
               TO TK-NAME
           MOVE "Document date is after the cut over date"
               TO TK-EXPECTED-X
           MOVE WS-COL(1)(1:100) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8900-LEGACY-TERMS
           MOVE "the converted item keeps its own terms and date"
               TO TK-NAME
           MOVE "30NET|N30|20260115|20260228" TO TK-EXPECTED-X
           MOVE WS-COL(1)(1:100) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8800-COUNT-OPEN-ITEMS
           MOVE "both converted items belong to the owning company"
               TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE WS-ROWS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "STC" TO WS-CO
           PERFORM 8800-COUNT-OPEN-ITEMS
           MOVE "the other company gains nothing from the load"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-ROWS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "CAL" TO WS-CO.

      *>----------------------------------------------------------------
      *> the harness runs from bin\tests, so the root of the tree is
      *> taken from %STERP_RUN% rather than assumed
      *>----------------------------------------------------------------
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
           MOVE SPACES TO WS-CMD
           STRING "cd /d """ FUNCTION TRIM(WS-ROOT) """ && "
                  "bin\x86\CONVLOAD.exe CO=" FUNCTION TRIM(WS-CO)
                  " FILE=data\legacy\CALAROPN.DAT "
                  "CUTOVER=20260228 MODE=COMMIT USER=FINCTRL > nul"
               DELIMITED BY SIZE INTO WS-CMD
           CALL "SYSTEM" USING WS-CMD.

       8300-READ-COUNTS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),ISNULL("
                  "RowCountTotal,0)), CONVERT(varchar(12),ISNULL("
                  "RowCountAccepted,0)), CONVERT(varchar(12),ISNULL"
                  "(RowCountRejected,0)), CONVERT(varchar(12),"
                  "ISNULL(RowCountDuplicate,0)) FROM "
                  "stg.ImportBatch WHERE CompanyCode = ? AND "
                  "BatchRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8050-TWO-PARMS
           MOVE 4 TO SQLIO-COL-CNT
           PERFORM 8000-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE 0 TO WS-TOTAL
           MOVE 0 TO WS-ACC
           MOVE 0 TO WS-REJ
           MOVE 0 TO WS-DUP
           IF SQLIO-OK
               COMPUTE WS-TOTAL = FUNCTION NUMVAL(WS-COL(1))
               COMPUTE WS-ACC = FUNCTION NUMVAL(WS-COL(2))
               COMPUTE WS-REJ = FUNCTION NUMVAL(WS-COL(3))
               COMPUTE WS-DUP = FUNCTION NUMVAL(WS-COL(4))
           END-IF.

      *>----------------------------------------------------------------
      *> the per record reason, which is what the conversion log and
      *> the operator actually read.  WS-COL(2) carries the document
      *> number the caller is asking about
      *>----------------------------------------------------------------
       8500-REJECT-REASON.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(r.ValidationMessage,'') FROM "
                  "stg.ImportRow r JOIN stg.ImportBatch b ON "
                  "b.BatchId = r.BatchId WHERE b.CompanyCode = ? "
                  "AND b.BatchRef = ? AND r.NaturalKey LIKE ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8050-TWO-PARMS
           MOVE WS-DOC TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF NOT SQLIO-OK
               MOVE SPACES TO WS-COL(1)
           END-IF.

       8800-COUNT-OPEN-ITEMS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "ar.OpenItem WHERE CompanyCode = ? AND "
                  "DocumentNumber IN ('SI-900101','SI-900102',"
                  "'SI-900103','SC-900104')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE 0 TO WS-ROWS
           IF SQLIO-OK
               COMPUTE WS-ROWS = FUNCTION NUMVAL(WS-COL(1))
           END-IF.

      *>----------------------------------------------------------------
      *> REQ: XMD-008  the legacy payment terms 30NET are not a code
      *> this system issues any more - the customer master carries
      *> N30 - so the converted document proves that the conversion
      *> retained the commercial inputs of the source document and
      *> did not restate them from current master data.  The posting
      *> date is the cut over, the document date is the original.
      *>----------------------------------------------------------------
       8900-LEGACY-TERMS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT RTRIM(ISNULL(o.TermsCode,'')) + '|' + "
                  "RTRIM(ISNULL(c.TermsCode,'')) + '|' + "
                  "CONVERT(char(8),o.DocumentDate,112) + '|' + "
                  "CONVERT(char(8),o.PostingDate,112) FROM "
                  "ar.OpenItem o JOIN ar.Customer c ON "
                  "c.CompanyCode = o.CompanyCode AND c.CustomerCode"
                  " = o.CustomerCode WHERE o.CompanyCode = ? AND "
                  "o.DocumentNumber = 'SI-900101'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF NOT SQLIO-OK
               MOVE SPACES TO WS-COL(1)
           END-IF.

       8050-TWO-PARMS.
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-BATCH TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT.

       8100-RUN-UPDATE.
           PERFORM 8050-TWO-PARMS
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

       8150-RUN-ONE-PARM.
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

       8000-WIDTHS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               MOVE 120 TO SQLIO-COL-WIDTH(WS-I)
           END-PERFORM.

       0900-SKIP-EVERYTHING.
           MOVE "conversion company boundary (no database)"
               TO TK-NAME
           PERFORM TK-SKIP.

       COPY TESTKITP.
