      *>================================================================
      *> PROGRAM-ID : TFINGAP1
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER (FIN) UNIT TESTS
      *> PURPOSE    : GLADJUST AND STATPROF - FINANCIAL CORRECTIONS
      *>              MUST CARRY AN APPROVED REASON, EVIDENCE AND THE
      *>              RIGHT AUTHORITY FOR THEIR SIZE, AND MAY NEVER
      *>              BE USED TO MOVE PHYSICAL QUANTITY ON THEIR OWN;
      *>              AND EVERY COMPANY CARRIES ITS FRAMEWORK, ITS
      *>              ACCOUNTING REFERENCE DATE AND THE STATUTORY
      *>              DEADLINES THAT FOLLOW FROM IT.  THE WORKED
      *>              DEADLINES ARE THOSE OF AC-119.
      *>              EVERY WRITE RUNS INSIDE BEGN ... ROLB.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2026-10-07 STERP ORIGINAL - CLOSING THE FIN TEST GAPS
      *>================================================================
      *> REQ: FIN-022, FIN-023, FIN-030   AC: AC-017, AC-119
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TFINGAP1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY GLADJUSTCA REPLACING ==DFHCOMMAREA== BY ==WS-GA-CA==.
       COPY STATPROFCA REPLACING ==DFHCOMMAREA== BY ==WS-SP-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "N".
       01  WS-ROW.
           05  WS-R1                   PIC X(30).
           05  WS-R2                   PIC X(30).
       01  WS-WORK.
           05  WS-CLERK-LIM            PIC S9(11)V99 VALUE 0.
           05  WS-MGR-LIM              PIC S9(11)V99 VALUE 0.
           05  WS-AMT                  PIC S9(11)V99 VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TFINGAP1 corrections and the statutory profile"
               TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP NOT = "Y"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 1000-ADJUSTMENT-EDITS
               PERFORM 1100-APPROVAL-LADDER
               PERFORM 1200-CARRYING-VALUE
               PERFORM 1300-QUANTITY-NEEDS-A-MOVEMENT
               PERFORM 2000-COMPANY-PROFILE
               PERFORM 2100-STATUTORY-DEADLINES
               PERFORM 2200-REFERENCE-DATE-CHANGE
               PERFORM 2300-EVERY-ACCOUNT-IS-MAPPED
           END-IF
           PERFORM TK-END
           STOP RUN.

       0100-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CompanyCode, AccountingFramework "
                  "FROM core.Company WHERE CompanyCode = 'STC'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-SQLCODE = 0 AND WS-R1 NOT = SPACES
               MOVE "Y" TO WS-DB-UP
           END-IF.

       0900-SKIP-EVERYTHING.
           MOVE "no database reachable" TO TK-DETAIL
           MOVE "FIN-022 financial corrections" TO TK-NAME
           PERFORM TK-SKIP
           MOVE "no database reachable" TO TK-DETAIL
           MOVE "FIN-030 statutory profile and deadlines" TO TK-NAME
           PERFORM TK-SKIP
           MOVE "no database reachable" TO TK-DETAIL
           MOVE "FIN-023 statutory format mapping" TO TK-NAME
           PERFORM TK-SKIP.

       8000-BEGIN-TRAN.
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

       8100-ROLLBACK-TRAN.
           MOVE "ROLB" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

       8300-PRIME-ADJUSTMENT.
           INITIALIZE WS-GA-CA
           MOVE "STC" TO CA-COMPANY OF WS-GA-CA
           MOVE "RDG" TO CA-SITE OF WS-GA-CA
           MOVE "GLCLERK" TO CA-USER OF WS-GA-CA
           MOVE 20261007 TO CA-BUSINESS-DATE OF WS-GA-CA
           MOVE "ZZFINGAP1-001" TO CA-CORRELATION-ID OF WS-GA-CA
           MOVE "BALANCE" TO GA-TYPE
           MOVE "IRRECOV" TO GA-REASON-CODE
           MOVE "Balance written off after recovery action closed"
               TO GA-REASON
           MOVE "7100" TO GA-ACCOUNT
           MOVE "1200" TO GA-CONTRA-ACCOUNT
           MOVE "Credit committee minute 2026/41" TO GA-EVIDENCE-REF
           MOVE 20261007 TO GA-POSTING-DATE
           MOVE 100.00 TO GA-AMOUNT
           MOVE 1000.00 TO GA-CARRYING-AMOUNT
           MOVE 0 TO GA-QUANTITY.

       8400-PRIME-PROFILE.
           INITIALIZE WS-SP-CA
           MOVE "STC" TO CA-COMPANY OF WS-SP-CA
           MOVE "RDG" TO CA-SITE OF WS-SP-CA
           MOVE "GLCLERK" TO CA-USER OF WS-SP-CA
           MOVE 20261007 TO CA-BUSINESS-DATE OF WS-SP-CA
           MOVE 20270331 TO SP-PERIOD-END
           MOVE "PRIVATE" TO SP-COMPANY-TYPE
           MOVE "N" TO SP-FIRST-PERIOD.

      *>   FIN-022.  A correction is a controlled document: it needs
      *>   a company, a known instruction, an approved reason and
      *>   supporting evidence before it exists at all.
       1000-ADJUSTMENT-EDITS.
           PERFORM 8300-PRIME-ADJUSTMENT
           MOVE "ASSESS" TO CA-FUNCTION OF WS-GA-CA
           MOVE SPACES TO CA-COMPANY OF WS-GA-CA
           CALL "GLADJUST" USING WS-GA-CA
           MOVE "An adjustment without a company is refused"
               TO TK-NAME
           MOVE "ADJ0001" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-GA-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8300-PRIME-ADJUSTMENT
           MOVE "WIBBLE" TO CA-FUNCTION OF WS-GA-CA
           CALL "GLADJUST" USING WS-GA-CA
           MOVE "An unknown adjustment instruction is refused"
               TO TK-NAME
           MOVE "ADJ0009" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-GA-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8300-PRIME-ADJUSTMENT
           MOVE "REQUEST" TO CA-FUNCTION OF WS-GA-CA
           MOVE SPACES TO GA-REASON
           CALL "GLADJUST" USING WS-GA-CA
           MOVE "A write-off needs an approved reason (ADJ0010)"
               TO TK-NAME
           MOVE "ADJ0010" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-GA-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8300-PRIME-ADJUSTMENT
           MOVE "REQUEST" TO CA-FUNCTION OF WS-GA-CA
           MOVE SPACES TO GA-EVIDENCE-REF
           CALL "GLADJUST" USING WS-GA-CA
           MOVE "A write-off needs supporting evidence (ADJ0011)"
               TO TK-NAME
           MOVE "ADJ0011" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-GA-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "A missing evidence reference is reported as such"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE GA-EVIDENCE-OK TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8300-PRIME-ADJUSTMENT
           MOVE "REQUEST" TO CA-FUNCTION OF WS-GA-CA
           MOVE SPACES TO GA-CONTRA-ACCOUNT
           CALL "GLADJUST" USING WS-GA-CA
           MOVE "Both the adjusted account and its contra are needed"
               TO TK-NAME
           MOVE "ADJ0012" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-GA-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   FIN-022.  The authority a correction needs is decided by
      *>   its size against the configured thresholds, and the ladder
      *>   is asked for its own limits so the test is true whatever
      *>   the parameters say.
       1100-APPROVAL-LADDER.
           PERFORM 8300-PRIME-ADJUSTMENT
           MOVE "ASSESS" TO CA-FUNCTION OF WS-GA-CA
           MOVE 1.00 TO GA-AMOUNT
           CALL "GLADJUST" USING WS-GA-CA
           MOVE GA-THRESHOLD TO WS-CLERK-LIM
           MOVE "A small correction is a clerk's decision" TO TK-NAME
           MOVE "CLERK" TO TK-EXPECTED-X
           MOVE GA-APPROVAL-LEVEL TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "A clerk level correction needs no second signature"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE GA-NEEDS-APPROVAL TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "A clerk level correction is posted under" TO TK-NAME
           MOVE "FIN.JOURNAL.ENTER" TO TK-EXPECTED-X
           MOVE GA-PERMISSION TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8300-PRIME-ADJUSTMENT
           MOVE "ASSESS" TO CA-FUNCTION OF WS-GA-CA
           MOVE WS-CLERK-LIM TO GA-AMOUNT
           CALL "GLADJUST" USING WS-GA-CA
           MOVE "The clerk threshold itself is still a" TO TK-NAME
           MOVE "CLERK" TO TK-EXPECTED-X
           MOVE GA-APPROVAL-LEVEL TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8300-PRIME-ADJUSTMENT
           MOVE "ASSESS" TO CA-FUNCTION OF WS-GA-CA
           COMPUTE WS-AMT = WS-CLERK-LIM + 0.01
           MOVE WS-AMT TO GA-AMOUNT
           CALL "GLADJUST" USING WS-GA-CA
           MOVE GA-THRESHOLD TO WS-MGR-LIM
           MOVE "A penny over the clerk limit needs a manager"
               TO TK-NAME
           MOVE "MANAGER" TO TK-EXPECTED-X
           MOVE GA-APPROVAL-LEVEL TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "A manager level correction is held for approval"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE GA-NEEDS-APPROVAL TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8300-PRIME-ADJUSTMENT
           MOVE "ASSESS" TO CA-FUNCTION OF WS-GA-CA
           COMPUTE WS-AMT = WS-MGR-LIM + 0.01
           MOVE WS-AMT TO GA-AMOUNT
           CALL "GLADJUST" USING WS-GA-CA
           MOVE "Above the manager limit the controller decides"
               TO TK-NAME
           MOVE "CONTROLLER" TO TK-EXPECTED-X
           MOVE GA-APPROVAL-LEVEL TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "A negative correction is sized by its magnitude"
               TO TK-NAME
           PERFORM 8300-PRIME-ADJUSTMENT
           MOVE "ASSESS" TO CA-FUNCTION OF WS-GA-CA
           COMPUTE WS-AMT = 0 - (WS-MGR-LIM + 0.01)
           MOVE WS-AMT TO GA-AMOUNT
           CALL "GLADJUST" USING WS-GA-CA
           MOVE "CONTROLLER" TO TK-EXPECTED-X
           MOVE GA-APPROVAL-LEVEL TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   FIN-022 / AC-017.  Writing 100.00 off a carrying value of
      *>   1,000.00 leaves 900.00 carried, and leaves the physical
      *>   quantity exactly where it was.
       1200-CARRYING-VALUE.
           PERFORM 8300-PRIME-ADJUSTMENT
           MOVE "ASSESS" TO CA-FUNCTION OF WS-GA-CA
           MOVE 100.00 TO GA-AMOUNT
           MOVE 1000.00 TO GA-CARRYING-AMOUNT
           MOVE 40.000000 TO GA-QUANTITY
           CALL "GLADJUST" USING WS-GA-CA
           MOVE "1,000.00 less a 100.00 write-down carries 900.00"
               TO TK-NAME
           MOVE 900.00 TO TK-EXPECTED-N
           MOVE GA-NET-CARRYING TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "A value correction leaves the quantity untouched"
               TO TK-NAME
           MOVE 40.000000 TO TK-EXPECTED-N
           MOVE GA-QTY-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   FIN-022.  An adjustment is not a way of moving goods.  If
      *>   it claims a quantity effect, the authorised stock movement
      *>   that actually moved them has to be named and has to exist.
       1300-QUANTITY-NEEDS-A-MOVEMENT.
           PERFORM 8000-BEGIN-TRAN
           PERFORM 8300-PRIME-ADJUSTMENT
           MOVE "REQUEST" TO CA-FUNCTION OF WS-GA-CA
           MOVE "STOCKLOSS" TO GA-TYPE
           MOVE 25.000000 TO GA-QUANTITY
           CALL "GLADJUST" USING WS-GA-CA
           MOVE "A quantity change without a stock movement" TO TK-NAME
           MOVE "ADJ0014" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-GA-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "The refused adjustment records nothing" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE GA-WRITEOFF-ID TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8300-PRIME-ADJUSTMENT
           MOVE "REQUEST" TO CA-FUNCTION OF WS-GA-CA
           MOVE "STOCKLOSS" TO GA-TYPE
           MOVE 25.000000 TO GA-QUANTITY
           MOVE "STOCK" TO GA-SUBLEDGER-TYPE
           MOVE "999999999999999" TO GA-SUBLEDGER-REF
           CALL "GLADJUST" USING WS-GA-CA
           MOVE "A stock movement that does not exist is refused too"
               TO TK-NAME
           MOVE "ADJ0014" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-GA-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CAST(MovementId AS varchar(20)), "
                  "'' FROM inv.StockMovement WHERE CompanyCode = "
                  "'STC' ORDER BY MovementId"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           PERFORM 8300-PRIME-ADJUSTMENT
           MOVE "REQUEST" TO CA-FUNCTION OF WS-GA-CA
           MOVE "STOCKLOSS" TO GA-TYPE
           MOVE 25.000000 TO GA-QUANTITY
           MOVE "STOCK" TO GA-SUBLEDGER-TYPE
           MOVE WS-R1 TO GA-SUBLEDGER-REF
           CALL "GLADJUST" USING WS-GA-CA
           MOVE "With an authorised movement named, the" TO TK-NAME
           IF CA-RETURN-CODE OF WS-GA-CA < 8
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-GA-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           PERFORM 8100-ROLLBACK-TRAN.

      *>   FIN-030.  The framework and the accounting reference date
      *>   are company facts, held once and read back by every
      *>   statutory routine.
       2000-COMPANY-PROFILE.
           PERFORM 8000-BEGIN-TRAN
           PERFORM 8400-PRIME-PROFILE
           MOVE "PROFILE" TO CA-FUNCTION OF WS-SP-CA
           CALL "STATPROF" USING WS-SP-CA
           MOVE "The company's accounting framework is on file"
               TO TK-NAME
           IF SP-FRAMEWORK = "FRS102" OR SP-FRAMEWORK = "FRS105"
               PERFORM TK-PASS
           ELSE
               MOVE SP-FRAMEWORK TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "The accounting reference date is on file" TO TK-NAME
           IF SP-ARD NOT = SPACES
               PERFORM TK-PASS
           ELSE
               MOVE "no accounting reference date held" TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           PERFORM 8100-ROLLBACK-TRAN
           PERFORM 8400-PRIME-PROFILE
           MOVE "WIBBLE" TO CA-FUNCTION OF WS-SP-CA
           CALL "STATPROF" USING WS-SP-CA
           MOVE "An unknown statutory instruction is refused"
               TO TK-NAME
           MOVE "STA0009" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SP-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   FIN-030 / AC-119.  A private company with a 31 March 2027
      *>   year end files its accounts by 31 December 2027, pays its
      *>   corporation tax by 1 January 2028, files the CT600 by
      *>   31 March 2028 and the confirmation statement fourteen days
      *>   after the anniversary.
       2100-STATUTORY-DEADLINES.
           PERFORM 8000-BEGIN-TRAN
           PERFORM 8400-PRIME-PROFILE
           MOVE "DEADLINE" TO CA-FUNCTION OF WS-SP-CA
           CALL "STATPROF" USING WS-SP-CA
           MOVE "Accounts are filed nine months after the year end"
               TO TK-NAME
           MOVE 20271231 TO TK-EXPECTED-N
           MOVE SP-ACCOUNTS-DUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Corporation tax is paid nine months and a day after"
               TO TK-NAME
           MOVE 20280101 TO TK-EXPECTED-N
           MOVE SP-CTPAY-DUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The CT600 is filed twelve months after the year end"
               TO TK-NAME
           MOVE 20280331 TO TK-EXPECTED-N
           MOVE SP-CTRETURN-DUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The confirmation statement follows fourteen days on"
               TO TK-NAME
           MOVE 20280414 TO TK-EXPECTED-N
           MOVE SP-CONFIRM-DUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "All four statutory obligations are listed"
               TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE SP-OBLIG-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8100-ROLLBACK-TRAN.

      *>   FIN-030.  A change of accounting reference date creates a
      *>   period of account of up to eighteen months, approved and
      *>   reasoned.  Longer than that is not a period of account.
       2200-REFERENCE-DATE-CHANGE.
           PERFORM 8400-PRIME-PROFILE
           MOVE "ARDCHANG" TO CA-FUNCTION OF WS-SP-CA
           MOVE 0 TO SP-PERIOD-START
           MOVE 0 TO SP-NEW-PERIOD-END
           CALL "STATPROF" USING WS-SP-CA
           MOVE "A reference date change needs both of its dates"
               TO TK-NAME
           MOVE "STA0020" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SP-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8400-PRIME-PROFILE
           MOVE "ARDCHANG" TO CA-FUNCTION OF WS-SP-CA
           MOVE 20250401 TO SP-PERIOD-START
           MOVE 20270331 TO SP-NEW-PERIOD-END
           MOVE "Aligning with the new parent's year end"
               TO SP-REASON
           CALL "STATPROF" USING WS-SP-CA
           MOVE "A period of account longer than 18 months is refused"
               TO TK-NAME
           MOVE "STA0021" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SP-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "The over long period is reported as invalid"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE SP-ARD-VALID TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8400-PRIME-PROFILE
           MOVE "ARDCHANG" TO CA-FUNCTION OF WS-SP-CA
           MOVE 20260401 TO SP-PERIOD-START
           MOVE 20270331 TO SP-NEW-PERIOD-END
           MOVE SPACES TO SP-REASON
           CALL "STATPROF" USING WS-SP-CA
           MOVE "A reference date change needs a recorded reason"
               TO TK-NAME
           MOVE "STA0022" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SP-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 8000-BEGIN-TRAN
           PERFORM 8400-PRIME-PROFILE
           MOVE "ARDCHANG" TO CA-FUNCTION OF WS-SP-CA
           MOVE 20260401 TO SP-PERIOD-START
           MOVE 20270331 TO SP-NEW-PERIOD-END
           MOVE "Aligning with the new parent's year end"
               TO SP-REASON
           CALL "STATPROF" USING WS-SP-CA
           MOVE "A twelve month period of account is accepted"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SP-ARD-VALID TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "Its length is recorded in months" TO TK-NAME
           MOVE 12 TO TK-EXPECTED-N
           MOVE SP-LENGTH-MONTHS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 8100-ROLLBACK-TRAN.

      *>   FIN-023 / FIN-030.  The statutory statement is only
      *>   meaningful when every posting account has exactly one
      *>   format line to sit on, so the map check has to come back
      *>   complete.
       2300-EVERY-ACCOUNT-IS-MAPPED.
           PERFORM 8400-PRIME-PROFILE
           MOVE "MAPCHECK" TO CA-FUNCTION OF WS-SP-CA
           CALL "STATPROF" USING WS-SP-CA
           MOVE "No posting account is left off the statutory format"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SP-UNMAPPED-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The statutory map is reported complete" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SP-MAP-COMPLETE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "A complete map does not block the statement"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-SP-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       COPY TESTKITP.
