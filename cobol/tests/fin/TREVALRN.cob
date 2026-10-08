      *>================================================================
      *> PROGRAM-ID : TREVALRN
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : FOREIGN CURRENCY TEST SUITE
      *> PURPOSE    : SMOKE TEST OF THE REVALRUN BATCH JOB (FIN-019,
      *>              FIN-020).  TFXREVAL PROVES THE FXREVAL SERVICE
      *>              ONE POSITION AT A TIME; THIS SUITE PROVES THE
      *>              BATCH PATH, WHERE REVALRUN SELECTS THE OPEN
      *>              FOREIGN CURRENCY POSITIONS ITSELF (GL BALANCES
      *>              PLUS AR/AP OPEN ITEM RESIDUALS) AND CALLS
      *>              FXREVAL ONCE PER POSITION.  THE JOB IS RUN IN
      *>              MODE=PREVIEW AT THE DEMONSTRATION MONTH END ON
      *>              THE PARAMETERS core.ReportDef ADVERTISES FOR IT
      *>              (CO, ASAT, MODE, RATE, USER) AND MUST END WITH
      *>              RETURN CODE 0, REVALUE MORE THAN ONE POSITION,
      *>              LEAVE NONE UNPRICED AND NAME ITS LISTING.
      *>              REVALRUN ENDS IN STOP RUN, SO IT IS SPAWNED
      *>              THROUGH A WATCHDOG (THE TSOBATCH PATTERN) THAT
      *>              KILLS IT AFTER SIXTY SECONDS.  PREVIEW POSTS
      *>              NOTHING, SO THE SUITE IS RE-RUNNABLE.
      *>----------------------------------------------------------------
      *> 2026-10-07 STERP ORIGINAL
      *>================================================================
      *> REQ: FIN-019, FIN-020
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TREVALRN.
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
           05  WS-COL OCCURS 2 TIMES   PIC X(120).
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "N".
           05  WS-DONE                 PIC X VALUE "N".
           05  WS-SEEN-SPOOL           PIC X VALUE "N".
           05  WS-SEEN-COUNT           PIC X VALUE "N".
           05  WS-SPOOL-ON-DISK        PIC X VALUE "N".
       01  WS-WORK.
           05  WS-ROOT                 PIC X(200) VALUE SPACES.
           05  WS-CMD                  PIC X(600) VALUE SPACES.
           05  WS-CAP-NAME             PIC X(240) VALUE SPACES.
           05  WS-CAP-STATUS           PIC X(2) VALUE "00".
           05  WS-SPL-STATUS           PIC X(2) VALUE "00".
           05  WS-SYSRC                PIC S9(9) COMP-5 VALUE 0.
           05  WS-SPOOL-NAME           PIC X(200) VALUE SPACES.
           05  WS-SPOOL-PATH           PIC X(240) VALUE SPACES.
           05  WS-W1                   PIC X(20) VALUE SPACES.
           05  WS-W2                   PIC X(20) VALUE SPACES.
           05  WS-W3                   PIC X(20) VALUE SPACES.
           05  WS-W4                   PIC X(20) VALUE SPACES.
           05  WS-W5                   PIC X(20) VALUE SPACES.
           05  WS-REVALUED             PIC S9(9) VALUE 0.
           05  WS-UNPRICED             PIC S9(9) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TREVALRN revaluation batch" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               MOVE "STERP database is not deployed" TO TK-DETAIL
               MOVE "revaluation batch (no database)" TO TK-NAME
               PERFORM TK-SKIP
           ELSE
               PERFORM 0300-RELEASE-THE-CONNECTION
               PERFORM 8200-RUN-JOB
               PERFORM 8400-READ-CAPTURE THRU 8400-EXIT
               PERFORM 1000-ASSERT-RUN THRU 1000-EXIT
           END-IF
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
       0100-PROBE-DATABASE.
           MOVE "N" TO WS-DB-UP
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)) FROM "
                  "core.ReportDef WHERE ReportCode = 'REVALRUN'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 120 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-OK
               MOVE "Y" TO WS-DB-UP
           END-IF.

      *>   the spawned job takes its own unit of work, so this one is
      *>   handed back first and can never hold a lock it waits on
       0300-RELEASE-THE-CONNECTION.
           MOVE "ROLB" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "DISC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

      *>----------------------------------------------------------------
      *> REQ: FIN-019, FIN-020  the multi-position path
      *>----------------------------------------------------------------
       1000-ASSERT-RUN.
           MOVE "REVALRUN PREVIEW ends with return code 0" TO TK-NAME
           IF WS-SYSRC = 99
               MOVE "did not terminate within 60 seconds"
                   TO TK-DETAIL
               PERFORM TK-FAIL
               GO TO 1000-EXIT
           END-IF
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-SYSRC TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "REVALRUN revalues more than one open position"
               TO TK-NAME
           IF WS-SEEN-COUNT = "Y" AND WS-REVALUED > 1
               PERFORM TK-PASS
           ELSE
               MOVE "no STERP-I-1002 line, or one position or fewer"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "REVALRUN leaves no position unpriced" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-UNPRICED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "REVALRUN names its spool file on SYSOUT" TO TK-NAME
           IF WS-SEEN-SPOOL = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE "no STERP-I-SPOOL line was printed" TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           PERFORM 8600-SPOOL-ON-DISK THRU 8600-EXIT
           MOVE "REVALRUN leaves its listing in run\spool" TO TK-NAME
           IF WS-SPOOL-ON-DISK = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE SPACES TO TK-DETAIL
               STRING "listing not found: "
                      FUNCTION TRIM(WS-SPOOL-NAME)
                   DELIMITED BY SIZE INTO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> helpers - the harness runs from bin\tests, so the root of the
      *> tree is taken from %STERP_RUN%.  WaitForExit answers false if
      *> the job is still alive after sixty seconds; it is then killed
      *> and 99 returned, so a hung job fails instead of hanging.
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
           MOVE SPACES TO WS-CAP-NAME
           STRING FUNCTION TRIM(WS-ROOT)
                  "\run\spool\TREVALRN-RUN.txt"
               DELIMITED BY SIZE INTO WS-CAP-NAME
           MOVE SPACES TO WS-CMD
           STRING "powershell -NoProfile -ExecutionPolicy Bypass "
                  "-Command ""$ErrorActionPreference='Stop'; "
                  "Set-Location '" FUNCTION TRIM(WS-ROOT) "'; "
                  "$p = Start-Process -FilePath "
                  "'bin\x86\REVALRUN.exe' -ArgumentList "
                  "'CO=STC','ASAT=20261031','MODE=PREVIEW',"
                  "'RATE=SPOT','USER=GLCLERK'"
                  " -NoNewWindow -PassThru -RedirectStandardOutput "
                  "'run\spool\TREVALRN-RUN.txt' "
                  "-RedirectStandardError "
                  "'run\spool\TREVALRN-RUN.err'; "
                  "if ($p.WaitForExit(60000)) "
                  "{ exit $p.ExitCode } else { $p.Kill(); exit 99 }"""
               DELIMITED BY SIZE INTO WS-CMD
           MOVE 0 TO WS-SYSRC
           CALL "SYSTEM" USING WS-CMD
           MOVE RETURN-CODE TO WS-SYSRC
           MOVE 0 TO RETURN-CODE.

      *>   STERP-I-1002 nnnnn+ position(s) revalued, nnnnnnnnn+ could
      *>   not be priced  /  STERP-I-SPOOL <listing>
       8400-READ-CAPTURE.
           MOVE "N" TO WS-SEEN-SPOOL
           MOVE "N" TO WS-SEEN-COUNT
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
                       MOVE CAP-REC(15:186) TO WS-SPOOL-NAME
                   END-IF
                   IF CAP-REC(1:12) = "STERP-I-1002"
                       MOVE "Y" TO WS-SEEN-COUNT
                       UNSTRING CAP-REC DELIMITED BY ALL SPACE
                           INTO WS-W1 WS-W2 WS-W3 WS-W4 WS-W5
                       COMPUTE WS-REVALUED =
                           FUNCTION NUMVAL(WS-W2)
                       COMPUTE WS-UNPRICED =
                           FUNCTION NUMVAL(WS-W5)
                   END-IF
               END-IF
           END-PERFORM
           CLOSE CAP-FILE.
       8400-EXIT.
           EXIT.

      *>   RUNPATH may print the name relative to the root of the tree
       8600-SPOOL-ON-DISK.
           MOVE "N" TO WS-SPOOL-ON-DISK
           IF WS-SPOOL-NAME = SPACES
               GO TO 8600-EXIT
           END-IF
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
      *>----------------------------------------------------------------
       COPY TESTKITP.
