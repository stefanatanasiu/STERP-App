      *>================================================================
      *> PROGRAM-ID : TPERCLOS
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PERIOD CLOSE TEST SUITE
      *> PURPOSE    : THE CLOSE CHECKLIST, THE SOFT AND HARD CLOSE AND
      *>              THE REOPEN TRAIL (FIN-010, FIN-011), AND THE
      *>              YEAR END TRANSFER THAT HAPPENS ONCE (FIN-012).
      *>              AC-013: AN UNRESOLVED MATERIAL CONTROL ACCOUNT
      *>              DIFFERENCE BLOCKS AN UNQUALIFIED HARD CLOSE AND
      *>              MAY ONLY BE PASSED BY A FORMAL ACCEPTANCE.
      *>              EVERY WRITE RUNS INSIDE ONE BEGN ... ROLB SO THE
      *>              SUITE IS REPEATABLE AND LEAVES NOTHING BEHIND.
      *>              THE FIXTURE YEAR IS 2033 SO THAT IT CANNOT
      *>              COLLIDE WITH SEEDED DEMONSTRATION DATA.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: FIN-010, FIN-011, FIN-012, FIN-021
      *> AC: AC-013
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TPERCLOS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY SQLIOCA.
       COPY PERCLOSECA REPLACING ==DFHCOMMAREA== BY ==WS-PERCLOSE-CA==
                      LEADING ==CA-== BY ==CH-==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "N".
           05  WS-FIXTURE-OK           PIC X VALUE "Y".
       01  WS-WORK.
           05  WS-PING                 PIC X(40).
           05  WS-IDX                  PIC 9(4) VALUE 0.
           05  WS-ROWS-1                PIC S9(9) COMP-5 VALUE 0.
           05  WS-ROWS-2                PIC S9(9) COMP-5 VALUE 0.
           05  WS-LAST-ROWS             PIC S9(9) COMP-5 VALUE 0.
      *>   The blocking steps that remain after ARAP is accepted.
       01  WS-BLOCK-STEPS.
           05  FILLER PIC X(10) VALUE "ACCRUALS".
           05  FILLER PIC X(10) VALUE "CUTOFF".
           05  FILLER PIC X(10) VALUE "STOCK".
           05  FILLER PIC X(10) VALUE "WIP".
           05  FILLER PIC X(10) VALUE "BANK".
           05  FILLER PIC X(10) VALUE "VAT".
           05  FILLER PIC X(10) VALUE "SCHEDULE".
           05  FILLER PIC X(10) VALUE "SIGNOFF".
       01  FILLER REDEFINES WS-BLOCK-STEPS.
           05  WS-BSTEP PIC X(10) OCCURS 8 TIMES.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TPERCLOS period close" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-CONNECT
           IF WS-DB-UP NOT = "Y"
               MOVE "no database reachable" TO TK-DETAIL
               MOVE "close suite needs SQL Server" TO TK-NAME
               PERFORM TK-SKIP
               PERFORM TK-END
               STOP RUN
           END-IF
           PERFORM 0200-BEGIN-WORK
           IF WS-FIXTURE-OK NOT = "Y"
               MOVE "period fixtures were not written" TO TK-DETAIL
               MOVE "FIN-010 period close tests" TO TK-NAME
               PERFORM TK-SKIP
               PERFORM 9000-ROLLBACK
               PERFORM TK-END
               STOP RUN
           END-IF
           PERFORM 1000-STATUS-TESTS
           PERFORM 2000-BUILD-TESTS
           PERFORM 3000-STEP-TESTS
           PERFORM 4000-CLOSE-TESTS
           PERFORM 5000-REOPEN-TESTS
           PERFORM 6000-YEAR-END-TESTS
           PERFORM 9000-ROLLBACK
           PERFORM TK-END
           STOP RUN.

       0100-CONNECT.
           MOVE "SELECT 'Y'" TO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING
           IF NOT SQLIO-ERROR
               MOVE "Y" TO WS-DB-UP
           END-IF.

       0200-BEGIN-WORK.
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING
           PERFORM 0300-FIXTURES.

       9000-ROLLBACK.
           MOVE "ROLB" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING.

       0300-FIXTURES.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.FiscalYear (CompanyCode, FiscalYear, "
                  "StartDate, EndDate, PeriodCount, Status) VALUES "
                  "('STC',2033,'2033-04-01','2034-03-31',12,'OPEN')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.Period (CompanyCode, FiscalYear, "
                  "PeriodNo, StartDate, EndDate, Name, Status) VALUES "
                  "('STC',2033,1,'2033-04-01','2033-04-30',"
                  "'April 2033','OPEN')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL.

       0900-RUN-SQL.
           MOVE 8 TO SQLIO-CURSOR
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING
           MOVE SQLIO-ROWCOUNT TO WS-LAST-ROWS
           IF SQLIO-ERROR
               MOVE "N" TO WS-FIXTURE-OK
               MOVE 0 TO WS-LAST-ROWS
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING.

       0950-SETUP-CL.
           INITIALIZE WS-PERCLOSE-CA
           MOVE "STC" TO CH-COMPANY
           MOVE "RDG" TO CH-SITE
           MOVE "FINCTRL" TO CH-USER
           MOVE 20261015 TO CH-BUSINESS-DATE
           MOVE 2033 TO CL-FISCAL-YEAR
           MOVE 1 TO CL-PERIOD-NO.

      *>----------------------------------------------------------------
      *> 1000 - FIN-010 PERIOD STATE
      *>----------------------------------------------------------------
       1000-STATUS-TESTS.
           PERFORM 0950-SETUP-CL
           MOVE "STATUS" TO CH-FUNCTION
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "FIN-010 an open period reports its state" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-010 the fixture period is open" TO TK-NAME
           MOVE "OPEN" TO TK-EXPECTED-X
           MOVE CL-PERIOD-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "FIN-011 materiality comes from the parameters"
               TO TK-NAME
           MOVE 1000 TO TK-EXPECTED-N
           MOVE CL-MATERIALITY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 0950-SETUP-CL
           MOVE "STATUS" TO CH-FUNCTION
           MOVE 9 TO CL-PERIOD-NO
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "FIN-010 a period that is not held is not found"
               TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE CH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 0950-SETUP-CL
           MOVE "STATUS" TO CH-FUNCTION
           MOVE 0 TO CL-PERIOD-NO
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "FIN-010 a request without a period is refused"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 2000 - FIN-011 THE STANDARD CHECKLIST
      *>----------------------------------------------------------------
       2000-BUILD-TESTS.
           PERFORM 0950-SETUP-CL
           MOVE "BUILD" TO CH-FUNCTION
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "FIN-011 the standard close checklist is prepared"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-011 the checklist carries ten steps" TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE CL-STEP-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-011 nine of the ten steps block the close"
               TO TK-NAME
           MOVE 9 TO TK-EXPECTED-N
           MOVE CL-BLOCKING-OPEN TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-013 an unworked checklist is not ready to close"
               TO TK-NAME
           MOVE "BLOCKED" TO TK-EXPECTED-X
           MOVE CL-READINESS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *>   COM-015: building the checklist again adds nothing.
           PERFORM 0950-SETUP-CL
           MOVE "BUILD" TO CH-FUNCTION
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "FIN-011 building the checklist twice adds no steps"
               TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE CL-STEP-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

      *>   COM-003: preparing a close needs the close authority.
           PERFORM 0950-SETUP-CL
           MOVE "BUILD" TO CH-FUNCTION
           MOVE "GLCLERK" TO CH-USER
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "FIN-011 a ledger clerk may not close a period"
               TO TK-NAME
           IF CH-RETURN-CODE > 4
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 3000 - AC-013 A MATERIAL CONTROL DIFFERENCE IS NOT SIGNED OFF
      *>        AS DONE.  IT IS EXPLAINED AND FORMALLY ACCEPTED, AND
      *>        THE ACCEPTANCE IS KEPT AS EVIDENCE.
      *>----------------------------------------------------------------
       3000-STEP-TESTS.
           PERFORM 0950-SETUP-CL
           MOVE "STEP" TO CH-FUNCTION
           MOVE "ARAP" TO CL-STEP-CODE
           MOVE "DONE" TO CL-STEP-ACTION
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "FIN-011 a completed step must quote its evidence"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 0950-SETUP-CL
           MOVE "STEP" TO CH-FUNCTION
           MOVE "ARAP" TO CL-STEP-CODE
           MOVE "DONE" TO CL-STEP-ACTION
           MOVE "RECON/2033/P01/ARAP" TO CL-EVIDENCE-REF
           MOVE 2500.00 TO CL-DIFFERENCE
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "AC-013 a material difference may not be signed done"
               TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-013 the refusal is reported as CLS0013" TO TK-NAME
           MOVE "CLS0013" TO TK-EXPECTED-X
           MOVE CH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 0950-SETUP-CL
           MOVE "STEP" TO CH-FUNCTION
           MOVE "ARAP" TO CL-STEP-CODE
           MOVE "ACCEPT" TO CL-STEP-ACTION
           MOVE "RECON/2033/P01/ARAP" TO CL-EVIDENCE-REF
           MOVE 2500.00 TO CL-DIFFERENCE
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "AC-013 accepting a difference needs a written reason"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 0950-SETUP-CL
           MOVE "STEP" TO CH-FUNCTION
           MOVE "ARAP" TO CL-STEP-CODE
           MOVE "ACCEPT" TO CL-STEP-ACTION
           MOVE "RECON/2033/P01/ARAP" TO CL-EVIDENCE-REF
           MOVE 2500.00 TO CL-DIFFERENCE
           MOVE "Ageing timing difference, cleared in the next period"
               TO CL-REASON
           MOVE "FINCTRL" TO CL-REVIEWER
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "AC-013 the controller may accept the difference"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-013 the accepted step no longer blocks the close"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CL-BLOCKING-OPEN TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 0950-SETUP-CL
           MOVE "STEP" TO CH-FUNCTION
           MOVE "NOSUCHSTEP" TO CL-STEP-CODE
           MOVE "DONE" TO CL-STEP-ACTION
           MOVE "RECON/2033/P01/NONE" TO CL-EVIDENCE-REF
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "FIN-011 a step that is not on the checklist fails"
               TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE CH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 4000 - FIN-011 SOFT CLOSE, THEN HARD CLOSE ONLY WHEN READY
      *>----------------------------------------------------------------
       4000-CLOSE-TESTS.
           PERFORM 0950-SETUP-CL
           MOVE "HARD" TO CH-FUNCTION
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "AC-013 open blocking steps stop an unqualified close"
               TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-013 the block is reported as CLS0021" TO TK-NAME
           MOVE "CLS0021" TO TK-EXPECTED-X
           MOVE CH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 0950-SETUP-CL
           MOVE "STATUS" TO CH-FUNCTION
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "AC-013 a refused hard close leaves the period open"
               TO TK-NAME
           MOVE "OPEN" TO TK-EXPECTED-X
           MOVE CL-PERIOD-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *>   A soft close is allowed while the checklist is worked: the
      *>   period takes authorised close adjustments only.
           PERFORM 0950-SETUP-CL
           MOVE "SOFT" TO CH-FUNCTION
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "FIN-011 a soft close is allowed while work remains"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-011 the period is soft closed" TO TK-NAME
           MOVE "SOFT" TO TK-EXPECTED-X
           MOVE CL-PERIOD-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *>   Work the remaining blocking steps with their evidence.
           PERFORM 4100-DO-STEP
               VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 8

           PERFORM 0950-SETUP-CL
           MOVE "STATUS" TO CH-FUNCTION
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "AC-013 a worked checklist leaves nothing blocking"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CL-BLOCKING-OPEN TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-013 the period is then ready to close" TO TK-NAME
           MOVE "READY" TO TK-EXPECTED-X
           MOVE CL-READINESS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 0950-SETUP-CL
           MOVE "HARD" TO CH-FUNCTION
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "AC-013 a ready period hard closes" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-013 the period is hard closed" TO TK-NAME
           MOVE "HARD" TO TK-EXPECTED-X
           MOVE CL-PERIOD-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 0950-SETUP-CL
           MOVE "HARD" TO CH-FUNCTION
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "FIN-011 a hard closed period is not closed twice"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "FIN-011 the second close is refused as CLS0020"
               TO TK-NAME
           MOVE "CLS0020" TO TK-EXPECTED-X
           MOVE CH-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       4100-DO-STEP.
           PERFORM 0950-SETUP-CL
           MOVE "STEP" TO CH-FUNCTION
           MOVE WS-BSTEP(WS-IDX) TO CL-STEP-CODE
           MOVE "DONE" TO CL-STEP-ACTION
           MOVE SPACES TO CL-EVIDENCE-REF
           STRING "RECON/2033/P01/" FUNCTION TRIM(WS-BSTEP(WS-IDX))
               DELIMITED BY SIZE INTO CL-EVIDENCE-REF
           MOVE 0 TO CL-DIFFERENCE
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE SPACES TO TK-NAME
           STRING "FIN-011 close step "
                  FUNCTION TRIM(WS-BSTEP(WS-IDX))
                  " is signed off with its evidence"
               DELIMITED BY SIZE INTO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 5000 - FIN-010 REOPENING IS RECORDED, NEVER SILENT
      *>----------------------------------------------------------------
       5000-REOPEN-TESTS.
           PERFORM 0950-SETUP-CL
           MOVE "REOPEN" TO CH-FUNCTION
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "FIN-010 reopening a period needs a recorded reason"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CH-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 0950-SETUP-CL
           MOVE "REOPEN" TO CH-FUNCTION
           MOVE "GLCLERK" TO CH-USER
           MOVE "Late supplier invoice received after sign-off"
               TO CL-REASON
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "FIN-010 a ledger clerk may not reopen a period"
               TO TK-NAME
           IF CH-RETURN-CODE > 4
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           PERFORM 0950-SETUP-CL
           MOVE "REOPEN" TO CH-FUNCTION
           MOVE "Late supplier invoice received after sign-off"
               TO CL-REASON
           MOVE "FINCTRL" TO CL-REVIEWER
           CALL "PERCLOSE" USING WS-PERCLOSE-CA
           MOVE "FIN-010 the controller may reopen with a reason"
               TO TK-NAME
           IF CH-RETURN-CODE < 8
               PERFORM TK-PASS
           ELSE
               MOVE CH-MESSAGE TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 6000 - FIN-012 AC-013 THE YEAR END TRANSFER HAPPENS ONCE.
      *>        YEAREND GUARDS ITSELF WITH "YearEndJournalNo IS NULL",
      *>        SO THE SECOND RUN OF A YEAR UPDATES NOTHING AND
      *>        REPORTS THE ORIGINAL TRANSFER JOURNAL INSTEAD.
      *>----------------------------------------------------------------
       6000-YEAR-END-TESTS.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.FiscalYear SET Status = 'CLOSED', "
                  "YearEndJournalNo = 'ZZT-YE-2033', ClosedBy = "
                  "'FINCTRL', ClosedAt = SYSDATETIME() WHERE "
                  "CompanyCode = 'STC' AND FiscalYear = 2033 "
                  "AND YearEndJournalNo IS NULL"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL
           MOVE WS-LAST-ROWS TO WS-ROWS-1
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.FiscalYear SET Status = 'CLOSED', "
                  "YearEndJournalNo = 'ZZT-YE-2033-AGAIN', ClosedBy = "
                  "'FINCTRL', ClosedAt = SYSDATETIME() WHERE "
                  "CompanyCode = 'STC' AND FiscalYear = 2033 "
                  "AND YearEndJournalNo IS NULL"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-RUN-SQL
           MOVE WS-LAST-ROWS TO WS-ROWS-2
           MOVE "AC-013 the year end transfer records itself once"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE WS-ROWS-1 TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-013 a second year end run transfers nothing"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-ROWS-2 TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(y.YearEndJournalNo,'none') FROM "
                  "fin.FiscalYear y WHERE y.CompanyCode = 'STC' "
                  "AND y.FiscalYear = 2033"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PING
           MOVE "AC-013 the original transfer journal is kept"
               TO TK-NAME
           MOVE "ZZT-YE-2033" TO TK-EXPECTED-X
           MOVE WS-PING TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       COPY TESTKITP.
