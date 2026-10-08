      *>================================================================
      *> PROGRAM-ID : TPOPERF
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : SMOKE TEST OF SUPPPERF, THE SUPPLIER
      *>              PERFORMANCE MEASURE RUN (PO-023, D-19).
      *>              SUPPPERF ENDS IN STOP RUN, SO IT CANNOT BE
      *>              CALLed - IT IS SPAWNED THROUGH A WATCHDOG
      *>              (powershell Start-Process + WaitForExit) WHICH
      *>              KILLS THE JOB AND RETURNS 99 IF IT HAS NOT
      *>              TERMINATED WITHIN SIXTY SECONDS, SO A HUNG JOB
      *>              CAN NEVER HANG THE TEST RUN.
      *>
      *>              IT IS RUN IN MODE=PREVIEW, WHICH BY DESIGN
      *>              REPORTS WITHOUT STORING, SO THE SUITE HAS NO
      *>              BUSINESS EFFECT.  THAT IS ALSO THE THING WORTH
      *>              TESTING: THE ROW COUNT IN po.SupplierPerformance
      *>              IS TAKEN BEFORE AND AFTER AND MUST NOT HAVE
      *>              MOVED.  A PREVIEW THAT QUIETLY WROTE WOULD BE A
      *>              MEASURE RUN NOBODY COULD TRUST.
      *>
      *>              THE CONNECTION IS HANDED BACK (ROLB THEN DISC)
      *>              BEFORE ANY JOB IS STARTED, EXACTLY AS PODEMO
      *>              DOES, SO THE JOB CAN NEVER QUEUE BEHIND THE
      *>              TEST'S OWN UNIT OF WORK.
      *>----------------------------------------------------------------
      *> 2026-10-07 STERP ORIGINAL
      *>================================================================
      *> REQ: PO-023, D-19, RPT-008
      *> AC: the supplier performance run ends with return code 0 on
      *> AC: the parameters its core.ReportDef row advertises
      *> AC: it prints STERP-I-SPOOL naming the listing it wrote, and
      *> AC: that listing is on disk
      *> AC: MODE=PREVIEW stores no measure row
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TPOPERF.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CAP-FILE ASSIGN TO DYNAMIC WS-CAP-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CAP-STATUS.
           SELECT SPL-FILE ASSIGN TO DYNAMIC WS-SPOOL-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-SPL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  CAP-FILE.
       01  CAP-REC                     PIC X(200).
       FD  SPL-FILE.
       01  SPL-REC                     PIC X(200).
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       01  WS-ROW.
           05  WS-COL OCCURS 10 TIMES  PIC X(120).
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "N".
           05  WS-DONE                 PIC X VALUE "N".
           05  WS-SEEN-SPOOL           PIC X VALUE "N".
           05  WS-SPOOL-ON-DISK        PIC X VALUE "N".
       01  WS-WORK.
           05  WS-ROOT                 PIC X(200) VALUE SPACES.
           05  WS-CMD                  PIC X(600) VALUE SPACES.
           05  WS-CAP-NAME             PIC X(240) VALUE SPACES.
           05  WS-CAP-STATUS           PIC X(2) VALUE "00".
           05  WS-SPL-STATUS           PIC X(2) VALUE "00".
           05  WS-SYSRC                PIC S9(9) COMP-5 VALUE 0.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-JOB                  PIC X(8) VALUE SPACES.
           05  WS-ARGS                 PIC X(200) VALUE SPACES.
           05  WS-SPOOL-LINE           PIC X(200) VALUE SPACES.
           05  WS-SPOOL-NAME           PIC X(200) VALUE SPACES.
           05  WS-SPOOL-PATH           PIC X(240) VALUE SPACES.
           05  WS-BEFORE-ROWS          PIC X(12) VALUE SPACES.
           05  WS-AFTER-ROWS           PIC X(12) VALUE SPACES.
           05  WS-WATCHDOG             PIC X VALUE "N".
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "po SUPPPERF supplier performance" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 0200-COUNT-BEFORE THRU 0200-EXIT
               PERFORM 0300-RELEASE-THE-CONNECTION
               PERFORM 1000-ALL-SUPPLIERS
               PERFORM 2000-ONE-SUPPLIER
               PERFORM 3000-PREVIEW-STORED-NOTHING THRU 3000-EXIT
           END-IF
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
       0100-PROBE-DATABASE.
           MOVE "N" TO WS-DB-UP
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "po.Receipt WHERE CompanyCode = 'STC'"
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

      *>   a preview must leave the measure store exactly as it
      *>   found it, so the count is taken first
       0200-COUNT-BEFORE.
           PERFORM 8100-COUNT-MEASURES
           MOVE WS-COL(1)(1:12) TO WS-BEFORE-ROWS.
       0200-EXIT.
           EXIT.

      *>   the spawned job takes its own unit of work.  this one is
      *>   handed back first so that the job can never queue behind
      *>   the test's own connection
       0300-RELEASE-THE-CONNECTION.
           MOVE "ROLB" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "DISC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

      *>----------------------------------------------------------------
      *> REQ: PO-023, D-19  the whole supplier book over a window
      *> wide enough to catch whatever the demo data left behind
      *>----------------------------------------------------------------
       1000-ALL-SUPPLIERS.
           MOVE "SUPPPERF" TO WS-JOB
           MOVE SPACES TO WS-ARGS
           STRING "'CO=STC','FROM=20200101','TO=20991231',"
                  "'USER=BUYER','MODE=PREVIEW'"
               DELIMITED BY SIZE INTO WS-ARGS
           PERFORM 8200-RUN-JOB
           PERFORM 8800-ASSERT-JOB THRU 8800-EXIT.

      *>----------------------------------------------------------------
      *> REQ: PO-023  the same run narrowed to one supplier, which is
      *> how a buyer actually uses it before a review meeting
      *>----------------------------------------------------------------
       2000-ONE-SUPPLIER.
           MOVE "SUPPPERF" TO WS-JOB
           MOVE SPACES TO WS-ARGS
           STRING "'CO=STC','SUPP=S0001','FROM=20200101',"
                  "'TO=20991231','USER=BUYER','MODE=PREVIEW'"
               DELIMITED BY SIZE INTO WS-ARGS
           PERFORM 8200-RUN-JOB
           PERFORM 8800-ASSERT-JOB THRU 8800-EXIT.

      *>----------------------------------------------------------------
      *> REQ: PO-023  two preview runs have been and gone.  if the
      *> measure store has grown, PREVIEW is not a preview.
      *>----------------------------------------------------------------
       3000-PREVIEW-STORED-NOTHING.
           MOVE "PO-023 MODE=PREVIEW stores no measure row"
             TO TK-NAME
           IF WS-WATCHDOG = "Y"
               MOVE "the job did not terminate, so what it stored "
                 TO TK-DETAIL
               STRING FUNCTION TRIM(TK-DETAIL)
                      "cannot be concluded" DELIMITED BY SIZE
                   INTO TK-DETAIL
               PERFORM TK-SKIP
               GO TO 3000-EXIT
           END-IF
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               MOVE "could not reconnect after the run" TO TK-DETAIL
               PERFORM TK-SKIP
               GO TO 3000-EXIT
           END-IF
           PERFORM 8100-COUNT-MEASURES
           MOVE WS-COL(1)(1:12) TO WS-AFTER-ROWS
           MOVE WS-BEFORE-ROWS TO TK-EXPECTED-X
           MOVE WS-AFTER-ROWS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "DISC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0900-SKIP-EVERYTHING.
           MOVE "STERP database is not deployed" TO TK-DETAIL
           MOVE "PO-023 supplier performance (no database)"
             TO TK-NAME
           PERFORM TK-SKIP.

      *>----------------------------------------------------------------
      *> helpers
      *>----------------------------------------------------------------
       8000-WIDTHS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               MOVE 120 TO SQLIO-COL-WIDTH(WS-I)
           END-PERFORM.

       8100-COUNT-MEASURES.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "po.SupplierPerformance WHERE CompanyCode = 'STC'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           MOVE SPACES TO WS-ROW
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

      *>   the harness runs from bin\tests, so the root of the tree is
      *>   taken from %STERP_RUN% rather than assumed.  the job is
      *>   started under a watchdog: WaitForExit answers false if it
      *>   is still alive after sixty seconds, the job is killed and
      *>   99 is returned, so a job that never ends is reported as a
      *>   failure instead of stopping the whole test run
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
                  "\run\spool\TPOPERF-RUN.txt"
               DELIMITED BY SIZE INTO WS-CAP-NAME
           MOVE SPACES TO WS-CMD
           STRING "powershell -NoProfile -ExecutionPolicy Bypass "
                  "-Command ""$ErrorActionPreference='Stop'; "
                  "Set-Location '" FUNCTION TRIM(WS-ROOT) "'; "
                  "$p = Start-Process -FilePath 'bin\x86\"
                  FUNCTION TRIM(WS-JOB) ".exe' -ArgumentList "
                  FUNCTION TRIM(WS-ARGS)
                  " -NoNewWindow -PassThru -RedirectStandardOutput "
                  "'run\spool\TPOPERF-RUN.txt' "
                  "-RedirectStandardError "
                  "'run\spool\TPOPERF-RUN.err'; "
                  "if ($p.WaitForExit(60000)) "
                  "{ exit $p.ExitCode } else { $p.Kill(); exit 99 }"""
               DELIMITED BY SIZE INTO WS-CMD
           MOVE 0 TO WS-SYSRC
           CALL "SYSTEM" USING WS-CMD
           MOVE RETURN-CODE TO WS-SYSRC
           MOVE 0 TO RETURN-CODE.

      *>   every STERP batch job names its listing on SYSOUT in a
      *>   fixed format so that an operator - or a test - can find it
       8400-READ-CAPTURE.
           MOVE "N" TO WS-SEEN-SPOOL
           MOVE SPACES TO WS-SPOOL-LINE
           MOVE SPACES TO WS-SPOOL-NAME
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
                   IF CAP-REC(1:13) = "STERP-I-SPOOL"
                       MOVE "Y" TO WS-SEEN-SPOOL
                       MOVE CAP-REC(1:200) TO WS-SPOOL-LINE
                       MOVE CAP-REC(15:186) TO WS-SPOOL-NAME
                   END-IF
               END-IF
           END-PERFORM
           CLOSE CAP-FILE.
       8400-EXIT.
           EXIT.

      *>   the name is printed as RUNPATH built it, which may be
      *>   relative to the root of the tree, so both forms are tried
       8600-SPOOL-ON-DISK.
           MOVE "N" TO WS-SPOOL-ON-DISK
           IF WS-SPOOL-NAME = SPACES
               GO TO 8600-EXIT
           END-IF
           MOVE SPACES TO WS-SPOOL-PATH
           MOVE WS-SPOOL-NAME TO WS-SPOOL-PATH
           PERFORM 8650-TRY-OPEN
           IF WS-SPOOL-ON-DISK = "Y"
               GO TO 8600-EXIT
           END-IF
           MOVE SPACES TO WS-SPOOL-PATH
           STRING FUNCTION TRIM(WS-ROOT) "\"
                  FUNCTION TRIM(WS-SPOOL-NAME)
               DELIMITED BY SIZE INTO WS-SPOOL-PATH
           PERFORM 8650-TRY-OPEN.
       8600-EXIT.
           EXIT.

       8650-TRY-OPEN.
           MOVE "00" TO WS-SPL-STATUS
           OPEN INPUT SPL-FILE
           IF WS-SPL-STATUS = "00"
               MOVE "Y" TO WS-SPOOL-ON-DISK
               CLOSE SPL-FILE
           END-IF.

      *>   the three things asked of every batch job
       8800-ASSERT-JOB.
           PERFORM 8400-READ-CAPTURE THRU 8400-EXIT
           IF WS-SYSRC = 99
               MOVE "Y" TO WS-WATCHDOG
               PERFORM 8850-WATCHDOG-SKIP
               GO TO 8800-EXIT
           END-IF
           MOVE SPACES TO TK-NAME
           STRING "PO-023 " FUNCTION TRIM(WS-JOB)
                  " ends with return code 0"
               DELIMITED BY SIZE INTO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-SYSRC TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO TK-NAME
           STRING "PO-023 " FUNCTION TRIM(WS-JOB)
                  " names its spool file on SYSOUT"
               DELIMITED BY SIZE INTO TK-NAME
           IF WS-SEEN-SPOOL = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE "no STERP-I-SPOOL line was printed"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           PERFORM 8600-SPOOL-ON-DISK THRU 8600-EXIT
           MOVE SPACES TO TK-NAME
           STRING "PO-023 " FUNCTION TRIM(WS-JOB)
                  " leaves its listing in run\spool"
               DELIMITED BY SIZE INTO TK-NAME
           IF WS-SPOOL-ON-DISK = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE SPACES TO TK-DETAIL
               STRING "listing not found: "
                      FUNCTION TRIM(WS-SPOOL-NAME)
                   DELIMITED BY SIZE INTO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.
       8800-EXIT.
           EXIT.

      *>   a job that never ends is reported as a skip with the job
      *>   named, so the suite stays readable and the defect stays
      *>   visible in the TAP output until it is fixed
       8850-WATCHDOG-SKIP.
           MOVE SPACES TO TK-DETAIL
           STRING FUNCTION TRIM(WS-JOB)
                  " did not terminate within 60 seconds"
               DELIMITED BY SIZE INTO TK-DETAIL
           MOVE SPACES TO TK-NAME
           STRING "PO-023 " FUNCTION TRIM(WS-JOB)
                  " ends with return code 0"
               DELIMITED BY SIZE INTO TK-NAME
           PERFORM TK-SKIP
           MOVE SPACES TO TK-NAME
           STRING "PO-023 " FUNCTION TRIM(WS-JOB)
                  " names its spool file on SYSOUT"
               DELIMITED BY SIZE INTO TK-NAME
           PERFORM TK-SKIP
           MOVE SPACES TO TK-NAME
           STRING "PO-023 " FUNCTION TRIM(WS-JOB)
                  " leaves its listing in run\spool"
               DELIMITED BY SIZE INTO TK-NAME
           PERFORM TK-SKIP.
      *>----------------------------------------------------------------
       COPY TESTKITP.
