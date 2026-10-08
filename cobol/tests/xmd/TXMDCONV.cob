      *>================================================================
      *> PROGRAM-ID : TXMDCONV
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : CROSS MODULE CONTROLS (XMD) UNIT TESTS
      *> PURPOSE    : CONVLOAD - THE LEGACY CONVERSION LOAD.  THE
      *>              SUPPLIED EXTRACT data\legacy\CALAROPN.DAT HOLDS
      *>              EIGHT RECORDS: FOUR GOOD OPEN ITEMS, ONE EXACT
      *>              DUPLICATE OF AN EARLIER LINE, ONE WITH A
      *>              CUSTOMER THAT IS NOT ON FILE, ONE WITH THE
      *>              IMPOSSIBLE DATE 30 FEBRUARY AND ONE WHOSE
      *>              AMOUNT IS NOT NUMERIC.  THE TEST RUNS THE JOB
      *>              IN COMMIT MODE, PROVES THE PER RECORD OUTCOMES,
      *>              THEN RUNS IT AGAIN TO PROVE THAT A RESTART
      *>              RECOGNISES ITS OWN WORK AND DOES NOT LOAD
      *>              ANYTHING TWICE.  THE BATCH IS REMOVED
      *>              AFTERWARDS SO THE SUITE IS REPEATABLE.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: XMD-007, COM-018, COM-023, FIN-024, AR-021, AP-024,
      *> REQ: ITM-024   AC: AC-002
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXMDCONV.
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
           05  WS-SYSRC                PIC S9(9) COMP-5 VALUE 0.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-BATCH                PIC X(40) VALUE
               "CONV-CALAROPN-20260331".
           05  WS-TOTAL                PIC S9(7) VALUE 0.
           05  WS-ACC                  PIC S9(7) VALUE 0.
           05  WS-REJ                  PIC S9(7) VALUE 0.
           05  WS-DUP                  PIC S9(7) VALUE 0.
           05  WS-ROWS                 PIC S9(7) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TXMDCONV conversion load" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 0200-CLEAN-UP
               PERFORM 1000-FIRST-RUN
               PERFORM 2000-RESTART
               PERFORM 0200-CLEAN-UP
           END-IF
           PERFORM TK-END
           STOP RUN.

       0100-PROBE-DATABASE.
           MOVE "N" TO WS-DB-UP
           MOVE SPACES TO SQLIO-STMT
           MOVE "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                TO SQLIO-STMT(1:44)
           MOVE "stg.ImportBatch" TO SQLIO-STMT(45:15)
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-OK
               MOVE "Y" TO WS-DB-UP
           END-IF.

       0200-CLEAN-UP.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE r FROM stg.ImportRow r JOIN "
                  "stg.ImportBatch b ON b.BatchId = r.BatchId WHERE"
                  " b.BatchRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-RUN-UPDATE
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM stg.ImportBatch WHERE BatchRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-RUN-UPDATE
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM core.ExceptionItem WHERE "
                  "DocumentRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8100-RUN-UPDATE
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM ar.OpenItem WHERE CompanyCode = "
                  "'CAL' AND DocumentNumber IN ('SI-900101',"
                  "'SI-900102','SI-900103','SC-900104')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8150-RUN-PLAIN.

      *>----------------------------------------------------------------
      *> REQ: XMD-007  per record accept, reject and duplicate outcome
      *>----------------------------------------------------------------
       1000-FIRST-RUN.
           PERFORM 8200-RUN-JOB
           PERFORM 8300-READ-COUNTS
           MOVE "every record of the extract is reported" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE WS-TOTAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the four good open items are accepted" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE WS-ACC TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "bad date, amount and customer are rejected"
               TO TK-NAME
           MOVE 3 TO TK-EXPECTED-N
           MOVE WS-REJ TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the repeated line is reported as a duplicate"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE WS-DUP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8400-COUNT-ROWS
           MOVE "one staging row is written per source record"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE WS-ROWS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8500-REJECT-REASON
           MOVE "the rejection reason names the impossible date"
               TO TK-NAME
           MOVE "Document date is not a real calendar date"
               TO TK-EXPECTED-X
           MOVE WS-COL(1)(1:100) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8800-COUNT-OPEN-ITEMS
           MOVE "every accepted item reaches the sales ledger"
               TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE WS-ROWS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8900-CREATED-REF
           MOVE "the staging row names the open item it created"
               TO TK-NAME
           MOVE "AROPENITEM|SI-900101" TO TK-EXPECTED-X
           MOVE WS-COL(1)(1:100) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8950-RESIDUAL
           MOVE "the open item is outstanding in full at cut over"
               TO TK-NAME
           MOVE "12450.00|12450.00|OPEN" TO TK-EXPECTED-X
           MOVE WS-COL(1)(1:100) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> REQ: COM-023  a restarted load recognises its own work
      *>----------------------------------------------------------------
       2000-RESTART.
           PERFORM 8200-RUN-JOB
           PERFORM 8400-COUNT-ROWS
           MOVE "a restart does not add a second set of rows"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE WS-ROWS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8300-READ-COUNTS
           MOVE "a restart accepts nothing a second time" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-ACC TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8600-VALID-ROWS
           MOVE "the four loaded rows stay marked valid" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE WS-ROWS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8800-COUNT-OPEN-ITEMS
           MOVE "a restart does not create the items a second time"
               TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE WS-ROWS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       8800-COUNT-OPEN-ITEMS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "ar.OpenItem WHERE CompanyCode = 'CAL' AND "
                  "DocumentNumber IN ('SI-900101','SI-900102',"
                  "'SI-900103','SC-900104')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
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
      *> REQ: XMD-007  the staging row records what it created, so a
      *> converted balance can be traced back to its source line
      *>----------------------------------------------------------------
       8900-CREATED-REF.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT RTRIM(ISNULL(r.CreatedDocumentType,''))"
                  " + '|' + RTRIM(ISNULL(r.CreatedDocumentRef,''))"
                  " FROM stg.ImportRow r JOIN stg.ImportBatch b ON"
                  " b.BatchId = r.BatchId WHERE b.BatchRef = ? AND"
                  " r.NaturalKey LIKE '%SI-900101%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8750-ONE-STRING.

       8950-RESIDUAL.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(20),OriginalAmountGBP) +"
                  " '|' + CONVERT(varchar(20),ResidualAmountGBP) +"
                  " '|' + SettlementStatus FROM ar.OpenItem WHERE "
                  "CompanyCode = 'CAL' AND DocumentNumber = "
                  "'SI-900101'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF NOT SQLIO-OK
               MOVE SPACES TO WS-COL(1)
           END-IF.

       8750-ONE-STRING.
           MOVE WS-BATCH TO SQLIO-PARM-VAL(1)
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

       8150-RUN-PLAIN.
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

       8200-RUN-JOB.
      *>   the test harness runs from bin\tests, so the root of the
      *>   tree is taken from %STERP_RUN% rather than assumed
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
                  "bin\x86\CONVLOAD.exe CO=CAL "
                  "FILE=data\legacy\CALAROPN.DAT CUTOVER=20260331 "
                  "MODE=COMMIT USER=FINCTRL > nul"
               DELIMITED BY SIZE INTO WS-CMD
           CALL "SYSTEM" USING WS-CMD.

       8300-READ-COUNTS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),ISNULL("
                  "RowCountTotal,0)), CONVERT(varchar(12),ISNULL("
                  "RowCountAccepted,0)), CONVERT(varchar(12),ISNULL"
                  "(RowCountRejected,0)), CONVERT(varchar(12),"
                  "ISNULL(RowCountDuplicate,0)) FROM "
                  "stg.ImportBatch WHERE BatchRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-BATCH TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-PARM-CNT
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

       8400-COUNT-ROWS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "stg.ImportRow r JOIN stg.ImportBatch b ON "
                  "b.BatchId = r.BatchId WHERE b.BatchRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8700-ONE-COUNT.

       8600-VALID-ROWS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "stg.ImportRow r JOIN stg.ImportBatch b ON "
                  "b.BatchId = r.BatchId WHERE b.BatchRef = ? AND "
                  "r.ValidationOutcome = 'VALID'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8700-ONE-COUNT.

       8500-REJECT-REASON.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(r.ValidationMessage,'') FROM "
                  "stg.ImportRow r JOIN stg.ImportBatch b ON "
                  "b.BatchId = r.BatchId WHERE b.BatchRef = ? AND "
                  "r.NaturalKey LIKE '%SI-900106%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-BATCH TO SQLIO-PARM-VAL(1)
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

       8700-ONE-COUNT.
           MOVE WS-BATCH TO SQLIO-PARM-VAL(1)
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

       8100-RUN-UPDATE.
           MOVE WS-BATCH TO SQLIO-PARM-VAL(1)
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
           MOVE "conversion load (no database)" TO TK-NAME
           PERFORM TK-SKIP.

       COPY TESTKITP.
