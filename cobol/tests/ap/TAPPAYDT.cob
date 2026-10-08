      *>================================================================
      *> PROGRAM-ID : TAPPAYDT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (AP) UNIT TESTS
      *> PURPOSE    : THE BACS ENTRY DATE ROUTING OF APPAYPRP.
      *>              A PAYMENT RUN WHOSE ENTRY DATE FALLS ON A
      *>              WEEKEND MUST BE MOVED ON TO THE NEXT BACS
      *>              PROCESSING DAY BEFORE THE PROPOSAL IS WRITTEN.
      *>              THE ADJUSTMENT ASKS DATECALC THROUGH APSVC, AND
      *>              DATECALC ACCEPTS NEXT / PREV / NONE ONLY.  THE
      *>              OLD "FWD" REQUEST WAS REFUSED, LEFT A ZERO
      *>              RESULT DATE, AND THE ZERO REACHED SQL SERVER AS
      *>              THE TEXT "0" - WHICH FAILED THE PROPOSAL INSERT
      *>              WITH "CONVERSION FAILED WHEN CONVERTING DATE
      *>              AND/OR TIME FROM CHARACTER STRING" (SQL0001).
      *>              THE SUITE PROVES THE SERVICE ANSWERS A REAL
      *>              DATE AND THAT APPAYPRP BUILDS A PROPOSAL WITH
      *>              A WORKING ENTRY DATE AND WORKING BACS INPUT AND
      *>              PROCESSING DATES.  IT NEEDS THE DATABASE AND
      *>              REMOVES THE PROPOSAL IT CREATED.
      *>----------------------------------------------------------------
      *> 2026-10-07 STERP ORIGINAL
      *>================================================================
      *> REQ: AP-011, AP-026, COM-026, D-16   AC: AC-034, AC-112
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAPPAYDT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY SQLIOCA.
       COPY APSVCARE.
       COPY APPAYPRPCA REPLACING ==DFHCOMMAREA== BY ==WS-PRP-CA==.
       01  WS-ROW.
           05  WS-C-PAY                    PIC X(8).
           05  WS-C-INPUT                  PIC X(8).
           05  FILLER                      PIC X(48).
       01  WS-KEYS.
           05  K-CO                        PIC X(10) VALUE "STC".
           05  K-SITE                      PIC X(10) VALUE "RDG".
           05  K-PREPARER                  PIC X(20) VALUE "APCLERK".
       01  WS-WORK.
           05  W-I                         PIC S9(4) VALUE 0.
           05  W-NOW                       PIC X(21) VALUE SPACES.
           05  W-TODAY                     PIC 9(8) VALUE 0.
           05  W-SUNDAY                    PIC 9(8) VALUE 0.
           05  W-CUTOFF                    PIC 9(8) VALUE 0.
           05  W-INT                       PIC S9(9) VALUE 0.
           05  W-DOW                       PIC S9(4) VALUE 0.
           05  W-GUARD                     PIC S9(4) VALUE 0.
           05  W-ID                        PIC S9(9) VALUE 0.
           05  W-NUM-ED                    PIC -(9)9.
           05  W-REASON                    PIC X(8) VALUE SPACES.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TAPPAYDT Bacs entry date routing" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-SETUP
           PERFORM 1000-ADJUST-NEXT
           PERFORM 1100-ADJUST-DEFAULT
           PERFORM 2000-PROPOSAL-ON-A-SUNDAY
           PERFORM 9000-CLEAN-UP
           PERFORM TK-END
           STOP RUN.

      *>   the first Sunday at least a week out - never a Bacs
      *>   processing day, whatever the bank holiday calendar says
       0100-SETUP.
           MOVE FUNCTION CURRENT-DATE TO W-NOW
           COMPUTE W-TODAY = FUNCTION NUMVAL(W-NOW(1:8))
           COMPUTE W-INT = FUNCTION INTEGER-OF-DATE(W-TODAY) + 7
           MOVE 0 TO W-GUARD
           COMPUTE W-DOW = FUNCTION MOD(W-INT, 7)
           PERFORM UNTIL W-DOW = 0 OR W-GUARD > 10
               ADD 1 TO W-INT
               ADD 1 TO W-GUARD
               COMPUTE W-DOW = FUNCTION MOD(W-INT, 7)
           END-PERFORM
           COMPUTE W-SUNDAY = FUNCTION DATE-OF-INTEGER(W-INT)
           MOVE 20271231 TO W-CUTOFF
           MOVE "a Sunday entry date was found for the test"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF W-DOW = 0 AND W-SUNDAY > 0
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR.

      *>   REQ: COM-026, D-16 - ADJUST NEXT answers a real date
       1000-ADJUST-NEXT.
           INITIALIZE WS-APSVC
           MOVE "ADJUST" TO SVC-FUNC
           MOVE K-CO TO SVC-COMPANY
           MOVE K-SITE TO SVC-SITE
           MOVE K-PREPARER TO SVC-USER
           MOVE W-TODAY TO SVC-DATE
           MOVE W-SUNDAY TO SVC-DATE-1
           MOVE "BACS" TO SVC-TEXT-1
           MOVE "NEXT" TO SVC-TEXT-2
           CALL "APSVC" USING WS-APSVC
           MOVE "the Bacs calendar moves a Sunday to a working day"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF SVC-RC <= 4 AND SVC-DATE-OUT > W-SUNDAY
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR
           MOVE "the adjusted entry date is never zero" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF SVC-DATE-OUT > 0
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR.

      *>   an unstated direction must behave the same way, not be
      *>   refused as an unknown direction
       1100-ADJUST-DEFAULT.
           INITIALIZE WS-APSVC
           MOVE "ADJUST" TO SVC-FUNC
           MOVE K-CO TO SVC-COMPANY
           MOVE K-SITE TO SVC-SITE
           MOVE K-PREPARER TO SVC-USER
           MOVE W-TODAY TO SVC-DATE
           MOVE W-SUNDAY TO SVC-DATE-1
           MOVE "BACS" TO SVC-TEXT-1
           MOVE SPACES TO SVC-TEXT-2
           CALL "APSVC" USING WS-APSVC
           MOVE "an unstated direction still lands on a working day"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF SVC-RC <= 4 AND SVC-DATE-OUT > W-SUNDAY
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR.

      *>   REQ: AP-011, AP-026 - the proposal is written, not refused
      *>   by a date conversion in the database
       2000-PROPOSAL-ON-A-SUNDAY.
           INITIALIZE WS-PRP-CA
           MOVE "SELECT" TO CA-FUNCTION OF WS-PRP-CA
           MOVE K-CO TO CA-COMPANY OF WS-PRP-CA
           MOVE K-SITE TO CA-SITE OF WS-PRP-CA
           MOVE K-PREPARER TO CA-USER OF WS-PRP-CA
           MOVE W-TODAY TO CA-BUSINESS-DATE OF WS-PRP-CA
           MOVE 1 TO PP-VERSION
           MOVE W-CUTOFF TO PP-CUTOFF-DATE
           MOVE W-SUNDAY TO PP-PAY-DATE
           MOVE "BACS" TO PP-METHOD
           MOVE "GBP1" TO PP-BANK-ACCOUNT
           MOVE "GBP" TO PP-CURRENCY
           MOVE "DRAFT" TO PP-MODE
           MOVE 250000 TO PP-CHAPS-THRESHOLD
           MOVE 1000000 TO PP-SINGLE-LIMIT
           CALL "APPAYPRP" USING WS-PRP-CA
           MOVE CA-REASON-CODE OF WS-PRP-CA TO W-REASON
           MOVE PP-PROPOSAL-ID TO W-ID
           MOVE "the selection is not refused by the database"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF W-REASON NOT = "SQL0001"
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR
           MOVE "a proposal is created for the payment run" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF CA-RETURN-CODE OF WS-PRP-CA <= 4 AND W-ID > 0
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR
           MOVE "the Sunday entry date was moved on, not zeroed"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF PP-PAY-DATE > W-SUNDAY AND PP-DAY-ADJUSTED = "Y"
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR
           MOVE "the entry date is reported as a processing day"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE PP-IS-PROC-DAY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the Bacs input date is a real date before entry"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF PP-BACS-INPUT-DATE > 0
              AND PP-BACS-INPUT-DATE < PP-PAY-DATE
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR
           MOVE "the Bacs processing date is a real date" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF PP-BACS-PROC-DATE > 0
              AND PP-BACS-PROC-DATE < PP-PAY-DATE
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR
           PERFORM 9100-READ-HEADER
           MOVE "the stored proposal carries the adjusted pay date"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF WS-C-PAY NOT = SPACES
              AND FUNCTION NUMVAL(WS-C-PAY) = PP-PAY-DATE
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR
           MOVE "the stored proposal carries a Bacs input date"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF WS-C-INPUT NOT = SPACES
              AND FUNCTION NUMVAL(WS-C-INPUT) > 0
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR.

       9100-READ-HEADER.
           MOVE SPACES TO WS-ROW
           IF W-ID > 0
               MOVE SPACES TO SQLIO-STMT
               STRING "SELECT CONVERT(char(8), PayDate, 112), "
                      "ISNULL(CONVERT(char(8), BacsInputDate, 112), "
                      "'00000000') FROM ap.PaymentProposal "
                      "WHERE ProposalId = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 1 TO SQLIO-PARM-CNT
               MOVE W-ID TO W-NUM-ED
               MOVE FUNCTION TRIM(W-NUM-ED) TO SQLIO-PARM-VAL(1)
               PERFORM 9200-NO-NULLS
               MOVE 2 TO SQLIO-COL-CNT
               MOVE 8 TO SQLIO-COL-WIDTH(1)
               MOVE 8 TO SQLIO-COL-WIDTH(2)
               MOVE 4 TO SQLIO-CURSOR
               MOVE "SNGL" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ROW
           END-IF.

       9000-CLEAN-UP.
           IF W-ID > 0
               MOVE SPACES TO SQLIO-STMT
               STRING "DELETE ap.PaymentProposalLine "
                      "WHERE ProposalId = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               PERFORM 9300-EXEC-BY-ID
               MOVE SPACES TO SQLIO-STMT
               STRING "DELETE ap.PaymentProposal WHERE ProposalId = ?"
                      " AND Status IN ('DRAFT','PROPOSED')"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               PERFORM 9300-EXEC-BY-ID
           END-IF.

       9200-NO-NULLS.
           PERFORM VARYING W-I FROM 1 BY 1
                   UNTIL W-I > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(W-I)
           END-PERFORM.

       9300-EXEC-BY-ID.
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE W-ID TO W-NUM-ED
           MOVE FUNCTION TRIM(W-NUM-ED) TO SQLIO-PARM-VAL(1)
           PERFORM 9200-NO-NULLS
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       COPY TESTKITP.
