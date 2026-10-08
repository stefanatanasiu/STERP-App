      *>================================================================
      *> PROGRAM-ID : TSOBATCH
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT UNIT TESTS
      *> PURPOSE    : SMOKE TEST OF THE FIVE SALES BATCH JOBS -
      *>              SOINVRUN, SOACKDOC, SOBACKLG, SOANAL AND
      *>              REBATE.  EACH IS RUN AS A SUB PROCESS WITH THE
      *>              PARAMETERS core.ReportDef CARRIES FOR IT (SEE
      *>              db\seed\72-so-menu.sql ParamSpec) AND MUST END
      *>              WITH RETURN CODE 0, NAME ITS SPOOL FILE ON
      *>              SYSOUT AND LEAVE THAT FILE IN run\spool.
      *>              ALL FIVE END IN STOP RUN, SO THEY CANNOT BE
      *>              CALLed - THEY ARE SPAWNED THROUGH A WATCHDOG
      *>              (powershell Start-Process + WaitForExit) WHICH
      *>              KILLS THE JOB AND RETURNS 99 IF IT HAS NOT
      *>              TERMINATED WITHIN SIXTY SECONDS, SO A HUNG JOB
      *>              CAN NEVER HANG THE TEST RUN.  THE TWO
      *>              REPORTING JOBS ARE READ ONLY; SOINVRUN AND
      *>              REBATE ARE RUN IN MODE=PREVIEW, WHICH BY
      *>              DESIGN HAS NO BUSINESS EFFECT (SO-014).
      *>----------------------------------------------------------------
      *> 2026-10-07 STERP ORIGINAL
      *>================================================================
      *> REQ: SO-011, SO-013, SO-014, SO-019, SO-022, SO-024, RPT-008
      *> AC: every sales batch job ends with return code 0 on the
      *> AC: parameters its core.ReportDef row advertises
      *> AC: every sales batch job prints STERP-I-SPOOL naming the
      *> AC: 132 column listing it wrote, and that file is on disk
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSOBATCH.
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
           05  WS-ORDER-REF            PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSOBATCH sales batch jobs" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 0200-FIND-AN-ORDER THRU 0200-EXIT
               PERFORM 0300-RELEASE-THE-CONNECTION
               PERFORM 1000-SOBACKLG
               PERFORM 2000-SOANAL
               PERFORM 3000-SOINVRUN
               PERFORM 4000-SOACKDOC THRU 4000-EXIT
               PERFORM 5000-REBATE
           END-IF
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
       0100-PROBE-DATABASE.
           MOVE "N" TO WS-DB-UP
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "so.SalesOrder WHERE CompanyCode = 'STC'"
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

      *>   SOACKDOC prints one named document, so the test needs a
      *>   real order rather than an invented reference
       0200-FIND-AN-ORDER.
           MOVE SPACES TO WS-ORDER-REF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) o.OrderNumber FROM so.SalesOrder "
                  "o WHERE o.CompanyCode = 'STC' ORDER BY "
                  "o.OrderNumber DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-OK
               MOVE WS-COL(1) TO WS-ORDER-REF
           END-IF.
       0200-EXIT.
           EXIT.

      *>   the spawned jobs take their own unit of work.  this one is
      *>   handed back first, exactly as SODEMO does, so that a job
      *>   can never queue behind the test's own connection
       0300-RELEASE-THE-CONNECTION.
           MOVE "ROLB" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "DISC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

      *>----------------------------------------------------------------
      *> REQ: RPT-008, SO-022  the order book listing, run on the
      *> parameters the report launcher offers (CO, SITE, FROM, TO)
      *>----------------------------------------------------------------
       1000-SOBACKLG.
           MOVE "SOBACKLG" TO WS-JOB
           MOVE SPACES TO WS-ARGS
           STRING "'CO=STC','FROM=20260101','TO=20261231',"
                  "'USER=SALESMGR'"
               DELIMITED BY SIZE INTO WS-ARGS
           PERFORM 8200-RUN-JOB
           PERFORM 8800-ASSERT-JOB THRU 8800-EXIT.

      *>----------------------------------------------------------------
      *> REQ: RPT-008, SO-024  sales analysis over the same window
      *>----------------------------------------------------------------
       2000-SOANAL.
           MOVE "SOANAL" TO WS-JOB
           MOVE SPACES TO WS-ARGS
           STRING "'CO=STC','FROM=20260101','TO=20261231',"
                  "'USER=SALESMGR','GROUPBY=CUSTOMER'"
               DELIMITED BY SIZE INTO WS-ARGS
           PERFORM 8200-RUN-JOB
           PERFORM 8800-ASSERT-JOB THRU 8800-EXIT.

      *>----------------------------------------------------------------
      *> REQ: SO-014  the invoice run in PREVIEW, which lists what
      *> would be billed and must leave the ledger alone
      *>----------------------------------------------------------------
       3000-SOINVRUN.
           MOVE "SOINVRUN" TO WS-JOB
           MOVE SPACES TO WS-ARGS
           STRING "'CO=STC','FROM=20260101','TO=20261231',"
                  "'USER=ARCLERK','MODE=PREVIEW'"
               DELIMITED BY SIZE INTO WS-ARGS
           PERFORM 8200-RUN-JOB
           PERFORM 8800-ASSERT-JOB THRU 8800-EXIT.

      *>----------------------------------------------------------------
      *> REQ: SO-011, SO-013  the order acknowledgement document
      *>----------------------------------------------------------------
       4000-SOACKDOC.
           IF WS-ORDER-REF = SPACES
               MOVE "no sales order is on file for STC" TO TK-DETAIL
               MOVE "SOACKDOC prints an acknowledgement" TO TK-NAME
               PERFORM TK-SKIP
               GO TO 4000-EXIT
           END-IF
           MOVE "SOACKDOC" TO WS-JOB
           MOVE SPACES TO WS-ARGS
           STRING "'CO=STC','DOC=ACK','REF="
                  FUNCTION TRIM(WS-ORDER-REF)
                  "','USER=SALESREP'"
               DELIMITED BY SIZE INTO WS-ARGS
           PERFORM 8200-RUN-JOB
           PERFORM 8800-ASSERT-JOB THRU 8800-EXIT.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: SO-019  the rebate accrual run in PREVIEW
      *>----------------------------------------------------------------
       5000-REBATE.
           MOVE "REBATE" TO WS-JOB
           MOVE SPACES TO WS-ARGS
           STRING "'CO=STC','USER=FINCTRL','MODE=PREVIEW'"
               DELIMITED BY SIZE INTO WS-ARGS
           PERFORM 8200-RUN-JOB
           PERFORM 8800-ASSERT-JOB THRU 8800-EXIT.

      *>----------------------------------------------------------------
       0900-SKIP-EVERYTHING.
           MOVE "STERP database is not deployed" TO TK-DETAIL
           MOVE "sales batch jobs (no database)" TO TK-NAME
           PERFORM TK-SKIP.

      *>----------------------------------------------------------------
      *> helpers
      *>----------------------------------------------------------------
       8000-WIDTHS.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               MOVE 120 TO SQLIO-COL-WIDTH(WS-I)
           END-PERFORM.

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
                  "\run\spool\TSOBATCH-RUN.txt"
               DELIMITED BY SIZE INTO WS-CAP-NAME
           MOVE SPACES TO WS-CMD
           STRING "powershell -NoProfile -ExecutionPolicy Bypass "
                  "-Command ""$ErrorActionPreference='Stop'; "
                  "Set-Location '" FUNCTION TRIM(WS-ROOT) "'; "
                  "$p = Start-Process -FilePath 'bin\x86\"
                  FUNCTION TRIM(WS-JOB) ".exe' -ArgumentList "
                  FUNCTION TRIM(WS-ARGS)
                  " -NoNewWindow -PassThru -RedirectStandardOutput "
                  "'run\spool\TSOBATCH-RUN.txt' "
                  "-RedirectStandardError "
                  "'run\spool\TSOBATCH-RUN.err'; "
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
               PERFORM 8850-WATCHDOG-FAIL
               GO TO 8800-EXIT
           END-IF
           MOVE SPACES TO TK-NAME
           STRING FUNCTION TRIM(WS-JOB)
                  " ends with return code 0"
               DELIMITED BY SIZE INTO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-SYSRC TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO TK-NAME
           STRING FUNCTION TRIM(WS-JOB)
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
           STRING FUNCTION TRIM(WS-JOB)
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

      *>   a job that never ends is a regression.  all five sales jobs
      *>   once wrote their listing and then slept instead of returning
      *>   from STOP RUN; that no longer reproduces (2026-10-07, every
      *>   job ends in seconds), so a timeout now FAILS the suite.  the
      *>   job is named, and the other two assertions are skipped
      *>   because a killed process never flushes its SYSOUT.
       8850-WATCHDOG-FAIL.
           MOVE SPACES TO TK-DETAIL
           STRING FUNCTION TRIM(WS-JOB)
                  " wrote its work but did not terminate within 60 "
                  "seconds - see docs\tech-debt\so.md"
               DELIMITED BY SIZE INTO TK-DETAIL
           MOVE SPACES TO TK-NAME
           STRING FUNCTION TRIM(WS-JOB)
                  " ends with return code 0"
               DELIMITED BY SIZE INTO TK-NAME
           PERFORM TK-FAIL
           MOVE "killed by the watchdog" TO TK-DETAIL
           MOVE SPACES TO TK-NAME
           STRING FUNCTION TRIM(WS-JOB)
                  " names its spool file on SYSOUT"
               DELIMITED BY SIZE INTO TK-NAME
           PERFORM TK-SKIP
           MOVE SPACES TO TK-NAME
           STRING FUNCTION TRIM(WS-JOB)
                  " leaves its listing in run\spool"
               DELIMITED BY SIZE INTO TK-NAME
           PERFORM TK-SKIP.
      *>----------------------------------------------------------------
       COPY TESTKITP.
