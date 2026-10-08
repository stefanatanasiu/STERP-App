      *>================================================================
      *> PROGRAM-ID : APPOST
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : THE PURCHASE LEDGER'S ONLY ROUTE INTO THE
      *>              NOMINAL LEDGER.  LINES ARE STAGED IN
      *>              fin.JournalStage UNDER THE CALLER'S CORRELATION
      *>              ID AND THE POSTING IS COMPLETED BY
      *>              fin.usp_PostJournal, WHICH OWNS THE TRANSACTION.
      *>              THE PURCHASE LEDGER THEREFORE NEVER HOLDS AN
      *>              OPEN SQLIO TRANSACTION ACROSS THE POSTING CALL -
      *>              A PROCEDURE THAT THROWS WOULD DOOM IT.
      *>              THE SAME CORRELATION ID POSTED TWICE RETURNS THE
      *>              ORIGINAL JOURNAL WITH WasDuplicate SET, SO A
      *>              RETRIED CONFIRMATION CANNOT POST TWICE (COM-015).
      *> CALLED BY  : APINVREG CALLERS, APALLOC, APCRDLNK, APLATEPY,
      *>              APVATEVD, APPAYRUN, APBULK, APCLOSE
      *> INTERFACE  : CALL "APPOST" USING WS-APPOST-CA
      *>              COPYBOOK APPOSTCA
      *> CALLS      : SQLIO -> fin.JournalStage, fin.usp_PostJournal
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-02-04 RJH  ORIGINAL NOMINAL LEDGER INTERFACE (TAPE)
      *> 1998-05-21 DJM  CR0801 DIRECT POSTING REPLACES THE NIGHTLY
      *>                 INTERFACE TAPE
      *> 2026-10-05 STERP fin.JournalStage + fin.usp_PostJournal
      *>================================================================
      *> REQ: AP-021, AP-022, COM-014, COM-015, FIN-004
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APPOST.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 04/02/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
              01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-POST-ROW.
           05  WS-J-ID                 PIC X(24).
           05  WS-J-NUMBER             PIC X(30).
           05  WS-J-COMPANY            PIC X(10).
           05  WS-J-YEAR               PIC X(8).
           05  WS-J-PERIOD             PIC X(8).
           05  WS-J-DEBIT              PIC X(24).
           05  WS-J-CREDIT             PIC X(24).
           05  WS-J-STATUS             PIC X(12).
           05  WS-J-DUP                PIC X(8).
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(40).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-NUM                  PIC S9(15)V9(6) VALUE 0.
           05  WS-TOT-DR               PIC S9(13)V99 VALUE 0.
           05  WS-TOT-CR               PIC S9(13)V99 VALUE 0.
           05  WS-DIFF-ED              PIC -(11)9.99.
       LINKAGE SECTION.
       COPY APPOSTWK.
       PROCEDURE DIVISION USING WS-APPOST.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE PJ-FUNC
               WHEN "STAGE"
                   PERFORM 2000-STAGE THRU 2000-EXIT
               WHEN "POST"
                   PERFORM 2000-STAGE THRU 2000-EXIT
                   IF NOT WS-FATAL
                       PERFORM 3000-POST THRU 3000-EXIT
                   END-IF
               WHEN "CLEAR"
                   PERFORM 4000-CLEAR THRU 4000-EXIT
               WHEN OTHER
                   PERFORM 9100-BAD-FUNC THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE "N" TO WS-FATAL-SW
           MOVE 0 TO PJ-RC
           MOVE SPACES TO PJ-REASON PJ-MESSAGE
           MOVE "N" TO PJ-WAS-DUPLICATE
           MOVE "N" TO PJ-BALANCED
           MOVE 0 TO WS-TOT-DR WS-TOT-CR
           IF PJ-CURRENCY = SPACES OR PJ-CURRENCY = LOW-VALUES
               MOVE "GBP" TO PJ-CURRENCY
           END-IF
           IF PJ-FX-RATE = 0
               MOVE 1 TO PJ-FX-RATE
           END-IF
           IF PJ-JOURNAL-TYPE = SPACES
               MOVE "AP" TO PJ-JOURNAL-TYPE
           END-IF
           IF PJ-SERIES-CODE = SPACES
               MOVE "JNL" TO PJ-SERIES-CODE
           END-IF
           IF PJ-CORRELATION = SPACES
              OR PJ-CORRELATION = LOW-VALUES
               MOVE "APP0001" TO PJ-REASON
               MOVE SPACES TO PJ-MESSAGE
               STRING "A correlation id is required so that a "
                      "retried posting cannot post twice"
                   DELIMITED BY SIZE INTO PJ-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1000-EXIT
           END-IF
           IF PJ-FUNC NOT = "CLEAR" AND PJ-LINE-CNT = 0
               MOVE "APP0002" TO PJ-REASON
               MOVE "A journal must have lines" TO PJ-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF
           IF PJ-LINE-CNT > 30
               MOVE 30 TO PJ-LINE-CNT
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> ANY RESIDUE OF A PREVIOUS ATTEMPT UNDER THIS CORRELATION ID IS
      *> CLEARED FIRST, SO A RETRY CANNOT DOUBLE THE LINES.
      *>----------------------------------------------------------------
       2000-STAGE SECTION.
       2000-START.
           PERFORM 4000-CLEAR THRU 4000-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > PJ-LINE-CNT OR WS-FATAL
               PERFORM 2100-ONE-LINE THRU 2100-EXIT
           END-PERFORM
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           IF WS-TOT-DR = WS-TOT-CR
               MOVE "Y" TO PJ-BALANCED
           ELSE
               MOVE "N" TO PJ-BALANCED
               COMPUTE WS-NUM = WS-TOT-DR - WS-TOT-CR
               MOVE WS-NUM TO WS-DIFF-ED
               MOVE "APP0003" TO PJ-REASON
               MOVE SPACES TO PJ-MESSAGE
               STRING "The purchase ledger journal does not balance; "
                      "debits exceed credits by "
                      FUNCTION TRIM(WS-DIFF-ED)
                   DELIMITED BY SIZE INTO PJ-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               PERFORM 4000-CLEAR THRU 4000-EXIT
               GO TO 2000-EXIT
           END-IF
           MOVE WS-TOT-DR TO PJ-TOTAL-DR-GBP
           MOVE WS-TOT-CR TO PJ-TOTAL-CR-GBP
           MOVE SPACES TO PJ-MESSAGE
           STRING "Journal lines staged" DELIMITED BY SIZE
               INTO PJ-MESSAGE.
       2000-EXIT.
           EXIT.

       2100-ONE-LINE SECTION.
       2100-START.
           IF PJ-L-ACCOUNT(WS-I) = SPACES
               MOVE "APP0004" TO PJ-REASON
               MOVE SPACES TO PJ-MESSAGE
               STRING "Every journal line needs an account code"
                   DELIMITED BY SIZE INTO PJ-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2100-EXIT
           END-IF
           ADD PJ-L-DR(WS-I) TO WS-TOT-DR
           ADD PJ-L-CR(WS-I) TO WS-TOT-CR
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.JournalStage (CorrelationId, LineNum, "
                  "CompanyCode, AccountCode, DebitAmount, "
                  "CreditAmount, CurrencyCode, DimCostCentre, "
                  "DimProject, Narrative, VatCode, JobRef, "
                  "SubledgerType, SubledgerRef, SourceRef, "
                  "BankAccountCode, CreatedBy) VALUES (?, ?, ?, ?, ?,"
                  " ?, ?, NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE PJ-CORRELATION TO SQLIO-PARM-VAL(1)
           MOVE WS-I TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE PJ-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE PJ-L-ACCOUNT(WS-I) TO SQLIO-PARM-VAL(4)
           MOVE PJ-L-DR(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE PJ-L-CR(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE PJ-CURRENCY TO SQLIO-PARM-VAL(7)
           MOVE PJ-L-COST-CTR(WS-I) TO SQLIO-PARM-VAL(8)
           MOVE PJ-L-PROJECT(WS-I) TO SQLIO-PARM-VAL(9)
           MOVE PJ-L-NARRATIVE(WS-I) TO SQLIO-PARM-VAL(10)
           MOVE PJ-L-VATCODE(WS-I) TO SQLIO-PARM-VAL(11)
           MOVE PJ-L-JOB(WS-I) TO SQLIO-PARM-VAL(12)
           MOVE PJ-SUBLEDGER-TYPE TO SQLIO-PARM-VAL(13)
           MOVE PJ-SUBLEDGER-REF TO SQLIO-PARM-VAL(14)
           MOVE PJ-SOURCE-REF TO SQLIO-PARM-VAL(15)
           MOVE PJ-L-BANK(WS-I) TO SQLIO-PARM-VAL(16)
           MOVE PJ-USER TO SQLIO-PARM-VAL(17)
           MOVE 17 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 17
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> fin.usp_PostJournal VALIDATES THE ACCOUNTS, DIMENSIONS, EVENT
      *> MAPPING, PERIOD AND BALANCE, AND OWNS THE TRANSACTION.  A
      *> BUSINESS REFUSAL ARRIVES AS A THROWN ERROR AND IS REPORTED
      *> HERE AS 08 WITH THE PROCEDURE'S OWN MESSAGE.
      *>----------------------------------------------------------------
       3000-POST SECTION.
       3000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC fin.usp_PostJournal ?, ?, ?, ?, "
                  "?, ?, ?, ?, 'AP', ?, "
                  "?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE PJ-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE PJ-CORRELATION TO SQLIO-PARM-VAL(2)
           MOVE PJ-USER TO SQLIO-PARM-VAL(3)
           MOVE PJ-DESCRIPTION TO SQLIO-PARM-VAL(4)
           MOVE PJ-POSTING-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE PJ-JOURNAL-TYPE TO SQLIO-PARM-VAL(6)
           IF PJ-DOCUMENT-DATE = 0
               MOVE PJ-POSTING-DATE TO PJ-DOCUMENT-DATE
           END-IF
           MOVE PJ-DOCUMENT-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE PJ-CURRENCY TO SQLIO-PARM-VAL(8)
           MOVE PJ-SOURCE-REF TO SQLIO-PARM-VAL(9)
           MOVE PJ-EVENT-CODE TO SQLIO-PARM-VAL(10)
           MOVE PJ-SERIES-CODE TO SQLIO-PARM-VAL(11)
           MOVE PJ-FX-RATE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE PJ-EVIDENCE-REF TO SQLIO-PARM-VAL(13)
           MOVE 13 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 13
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           IF PJ-EVIDENCE-REF = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(13)
           END-IF
           MOVE 9 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 10 TO SQLIO-COL-WIDTH(3)
           MOVE 8 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 12 TO SQLIO-COL-WIDTH(8)
           MOVE 8 TO SQLIO-COL-WIDTH(9)
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-POST-ROW
           IF SQLIO-ERROR
               MOVE "Y" TO WS-FATAL-SW
               MOVE 8 TO PJ-RC
               MOVE "APP0005" TO PJ-REASON
               MOVE SQLIO-ERRMSG(1:200) TO PJ-MESSAGE
               IF PJ-MESSAGE = SPACES
                   MOVE "The nominal ledger refused the posting"
                       TO PJ-MESSAGE
               END-IF
               MOVE 5 TO SQLIO-CURSOR
               MOVE "CLOS" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-POST-ROW
               GO TO 3000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 12 TO PJ-RC
               MOVE "APP0006" TO PJ-REASON
               MOVE SPACES TO PJ-MESSAGE
               STRING "The nominal ledger returned no posting result"
                   DELIMITED BY SIZE INTO PJ-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 3000-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-J-ID)
           MOVE WS-NUM TO PJ-JOURNAL-ID
           MOVE WS-J-NUMBER TO PJ-JOURNAL-NUMBER
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-J-YEAR)
           MOVE WS-NUM TO PJ-FISCAL-YEAR
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-J-PERIOD)
           MOVE WS-NUM TO PJ-PERIOD-NO
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-J-DEBIT)
           MOVE WS-NUM TO PJ-TOTAL-DR-GBP
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-J-CREDIT)
           MOVE WS-NUM TO PJ-TOTAL-CR-GBP
           MOVE SPACES TO PJ-MESSAGE
           IF WS-J-DUP(1:1) = "1"
               MOVE "Y" TO PJ-WAS-DUPLICATE
               MOVE 4 TO PJ-RC
               MOVE "APP0007" TO PJ-REASON
               STRING "This instruction was already posted as "
                      FUNCTION TRIM(PJ-JOURNAL-NUMBER)
                      "; it has not been posted again"
                   DELIMITED BY SIZE INTO PJ-MESSAGE
           ELSE
               MOVE "N" TO PJ-WAS-DUPLICATE
               MOVE 0 TO PJ-RC
               STRING "Posted as journal "
                      FUNCTION TRIM(PJ-JOURNAL-NUMBER)
                   DELIMITED BY SIZE INTO PJ-MESSAGE
           END-IF.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4000-CLEAR SECTION.
       4000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE fin.JournalStage WHERE CorrelationId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE PJ-CORRELATION TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8500-EXEC-UPDATE SECTION.
       8500-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 8500-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       8500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-FATAL-SW
           MOVE 8 TO PJ-RC
           CONTINUE.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNC SECTION.
       9100-START.
           MOVE "APP0099" TO PJ-REASON
           MOVE SPACES TO PJ-MESSAGE
           STRING "APPOST does not support function "
                  FUNCTION TRIM(PJ-FUNC)
               DELIMITED BY SIZE INTO PJ-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE "Y" TO WS-FATAL-SW
           MOVE 12 TO PJ-RC
           MOVE "SQL0001" TO PJ-REASON
           MOVE SQLIO-ERRMSG(1:200) TO PJ-MESSAGE
           IF PJ-MESSAGE = SPACES
               MOVE "SQL failure in APPOST" TO PJ-MESSAGE
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       9300-EXIT.
           EXIT.
