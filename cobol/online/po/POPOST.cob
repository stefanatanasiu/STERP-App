      *>================================================================
      *> PROGRAM-ID : POPOST
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : THE PURCHASING SUITE'S ONLY ROUTE INTO THE
      *>              NOMINAL LEDGER.  LINES ARE STAGED IN
      *>              fin.JournalStage UNDER THE CALLER'S CORRELATION
      *>              ID AND THE POSTING IS COMPLETED BY
      *>              fin.usp_PostJournal, WHICH JOINS A CALLER UOW
      *>              OR OWNS ITS TRANSACTION WHEN CALLED ALONE.
      *>              RECEIPT HOLDS ITS SQLIO UOW THROUGH THE POSTING
      *>              SO STOCK AND GRNI COMMIT OR ROLL BACK TOGETHER.
      *>              THE SAME CORRELATION ID POSTED TWICE RETURNS THE
      *>              ORIGINAL JOURNAL WITH WasDuplicate SET, SO A
      *>              RETRIED RECEIPT CANNOT ACCRUE GRNI TWICE.
      *> CALLED BY  : RECEIPT, SVCACC, SUPPRET, LANDCOST, IMPVAT
      *> INTERFACE  : CALL "POPOST" USING WS-POPOST  (COPY POPOSTWK)
      *> CALLS      : SQLIO -> fin.JournalStage, fin.usp_PostJournal
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-04-02 RJH  CR0788 DISTRIBUTION SUITE NOMINAL INTERFACE
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS ON POSTING DATES
      *> 2021-01-01 TMW  BREXIT - POSTPONED IMPORT VAT LEGS
      *> 2026-10-06 STERP fin.JournalStage + fin.usp_PostJournal
      *>================================================================
      *> REQ: PO-009, PO-014, PO-017, PO-025, COM-014, COM-015
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POPOST.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 02/04/1998.
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
       COPY POPOSTWK.
       PROCEDURE DIVISION USING WS-POPOST.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE PP-FUNC
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
           MOVE 0 TO PP-RC
           MOVE SPACES TO PP-REASON PP-MESSAGE
           MOVE "N" TO PP-WAS-DUPLICATE
           MOVE "N" TO PP-BALANCED
           MOVE 0 TO WS-TOT-DR WS-TOT-CR
           IF PP-CURRENCY = SPACES OR PP-CURRENCY = LOW-VALUES
               MOVE "GBP" TO PP-CURRENCY
           END-IF
           IF PP-FX-RATE = 0
               MOVE 1 TO PP-FX-RATE
           END-IF
           IF PP-JOURNAL-TYPE = SPACES
               MOVE "PO" TO PP-JOURNAL-TYPE
           END-IF
           IF PP-SERIES-CODE = SPACES
               MOVE "JNL" TO PP-SERIES-CODE
           END-IF
           IF PP-CORRELATION = SPACES
              OR PP-CORRELATION = LOW-VALUES
               MOVE "POP0001" TO PP-REASON
               MOVE SPACES TO PP-MESSAGE
               STRING "A correlation id is required so that a "
                      "retried goods receipt cannot accrue twice"
                   DELIMITED BY SIZE INTO PP-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1000-EXIT
           END-IF
           IF PP-FUNC NOT = "CLEAR" AND PP-LINE-CNT = 0
               MOVE "POP0002" TO PP-REASON
               MOVE "A journal must have lines" TO PP-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF
           IF PP-LINE-CNT > 30
               MOVE 30 TO PP-LINE-CNT
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
                   UNTIL WS-I > PP-LINE-CNT OR WS-FATAL
               PERFORM 2100-ONE-LINE THRU 2100-EXIT
           END-PERFORM
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           IF WS-TOT-DR = WS-TOT-CR
               MOVE "Y" TO PP-BALANCED
           ELSE
               MOVE "N" TO PP-BALANCED
               COMPUTE WS-NUM = WS-TOT-DR - WS-TOT-CR
               MOVE WS-NUM TO WS-DIFF-ED
               MOVE "POP0003" TO PP-REASON
               MOVE SPACES TO PP-MESSAGE
               STRING "The purchasing journal does not balance; "
                      "debits exceed credits by "
                      FUNCTION TRIM(WS-DIFF-ED)
                   DELIMITED BY SIZE INTO PP-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               PERFORM 4000-CLEAR THRU 4000-EXIT
               GO TO 2000-EXIT
           END-IF
           MOVE WS-TOT-DR TO PP-TOTAL-DR-GBP
           MOVE WS-TOT-CR TO PP-TOTAL-CR-GBP
           MOVE SPACES TO PP-MESSAGE
           STRING "Journal lines staged" DELIMITED BY SIZE
               INTO PP-MESSAGE.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2100-ONE-LINE SECTION.
       2100-START.
           IF PP-L-ACCOUNT(WS-I) = SPACES
               MOVE "POP0004" TO PP-REASON
               MOVE SPACES TO PP-MESSAGE
               STRING "Every journal line needs an account code"
                   DELIMITED BY SIZE INTO PP-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2100-EXIT
           END-IF
           ADD PP-L-DR(WS-I) TO WS-TOT-DR
           ADD PP-L-CR(WS-I) TO WS-TOT-CR
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.JournalStage (CorrelationId, LineNum, "
                  "CompanyCode, AccountCode, DebitAmount, "
                  "CreditAmount, CurrencyCode, DimCostCentre, "
                  "DimProject, Narrative, VatCode, JobRef, "
                  "SubledgerType, SubledgerRef, SourceRef, "
                  "CreatedBy) VALUES (?, ?, ?, ?, ?, ?, ?, "
                  "NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE PP-CORRELATION TO SQLIO-PARM-VAL(1)
           MOVE WS-I TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE PP-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE PP-L-ACCOUNT(WS-I) TO SQLIO-PARM-VAL(4)
           MOVE PP-L-DR(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE PP-L-CR(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE PP-CURRENCY TO SQLIO-PARM-VAL(7)
           MOVE PP-L-COST-CTR(WS-I) TO SQLIO-PARM-VAL(8)
           MOVE PP-L-PROJECT(WS-I) TO SQLIO-PARM-VAL(9)
           MOVE PP-L-NARRATIVE(WS-I) TO SQLIO-PARM-VAL(10)
           MOVE PP-L-VATCODE(WS-I) TO SQLIO-PARM-VAL(11)
           MOVE PP-L-JOB(WS-I) TO SQLIO-PARM-VAL(12)
           MOVE PP-SUBLEDGER-TYPE TO SQLIO-PARM-VAL(13)
           MOVE PP-SUBLEDGER-REF TO SQLIO-PARM-VAL(14)
           MOVE PP-SOURCE-REF TO SQLIO-PARM-VAL(15)
           MOVE PP-USER TO SQLIO-PARM-VAL(16)
           MOVE 16 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 16
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> fin.usp_PostJournal VALIDATES THE ACCOUNTS, DIMENSIONS, EVENT
      *> MAPPING, PERIOD AND BALANCE, AND OWNS THE TRANSACTION.
      *>----------------------------------------------------------------
       3000-POST SECTION.
       3000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC fin.usp_PostJournal ?, ?, ?, ?, "
                  "?, ?, ?, ?, 'PO', ?, "
                  "?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE PP-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE PP-CORRELATION TO SQLIO-PARM-VAL(2)
           MOVE PP-USER TO SQLIO-PARM-VAL(3)
           MOVE PP-DESCRIPTION TO SQLIO-PARM-VAL(4)
           MOVE PP-POSTING-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE PP-JOURNAL-TYPE TO SQLIO-PARM-VAL(6)
           IF PP-DOCUMENT-DATE = 0
               MOVE PP-POSTING-DATE TO PP-DOCUMENT-DATE
           END-IF
           MOVE PP-DOCUMENT-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE PP-CURRENCY TO SQLIO-PARM-VAL(8)
           MOVE PP-SOURCE-REF TO SQLIO-PARM-VAL(9)
           MOVE PP-EVENT-CODE TO SQLIO-PARM-VAL(10)
           MOVE PP-SERIES-CODE TO SQLIO-PARM-VAL(11)
           MOVE PP-FX-RATE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE PP-EVIDENCE-REF TO SQLIO-PARM-VAL(13)
           MOVE 13 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 13
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           IF PP-EVIDENCE-REF = SPACES
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
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-POST-ROW
           IF SQLIO-ERROR
               MOVE "Y" TO WS-FATAL-SW
               MOVE 8 TO PP-RC
               MOVE "POP0005" TO PP-REASON
               MOVE SQLIO-ERRMSG(1:200) TO PP-MESSAGE
               IF PP-MESSAGE = SPACES
                   MOVE "The nominal ledger refused the posting"
                       TO PP-MESSAGE
               END-IF
               MOVE 6 TO SQLIO-CURSOR
               MOVE "CLOS" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-POST-ROW
               GO TO 3000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 12 TO PP-RC
               MOVE "POP0006" TO PP-REASON
               MOVE SPACES TO PP-MESSAGE
               STRING "The nominal ledger returned no posting result"
                   DELIMITED BY SIZE INTO PP-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 3000-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-J-ID)
           MOVE WS-NUM TO PP-JOURNAL-ID
           MOVE WS-J-NUMBER TO PP-JOURNAL-NUMBER
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-J-YEAR)
           MOVE WS-NUM TO PP-FISCAL-YEAR
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-J-PERIOD)
           MOVE WS-NUM TO PP-PERIOD-NO
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-J-DEBIT)
           MOVE WS-NUM TO PP-TOTAL-DR-GBP
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-J-CREDIT)
           MOVE WS-NUM TO PP-TOTAL-CR-GBP
           MOVE SPACES TO PP-MESSAGE
           IF WS-J-DUP(1:1) = "1"
               MOVE "Y" TO PP-WAS-DUPLICATE
               MOVE 4 TO PP-RC
               MOVE "POP0007" TO PP-REASON
               STRING "This instruction was already posted as "
                      FUNCTION TRIM(PP-JOURNAL-NUMBER)
                      "; it has not been posted again"
                   DELIMITED BY SIZE INTO PP-MESSAGE
           ELSE
               MOVE "N" TO PP-WAS-DUPLICATE
               MOVE 0 TO PP-RC
               STRING "Posted as journal "
                      FUNCTION TRIM(PP-JOURNAL-NUMBER)
                   DELIMITED BY SIZE INTO PP-MESSAGE
           END-IF.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4000-CLEAR SECTION.
       4000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE fin.JournalStage WHERE CorrelationId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE PP-CORRELATION TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8500-EXEC-UPDATE SECTION.
       8500-START.
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               MOVE "Y" TO WS-FATAL-SW
               MOVE 12 TO PP-RC
               MOVE "POP0008" TO PP-REASON
               MOVE SQLIO-ERRMSG(1:200) TO PP-MESSAGE
           END-IF
           MOVE 6 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       8500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE 8 TO PP-RC
           MOVE "Y" TO WS-FATAL-SW.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNC SECTION.
       9100-START.
           MOVE 8 TO PP-RC
           MOVE "POP0009" TO PP-REASON
           MOVE SPACES TO PP-MESSAGE
           STRING "Function " FUNCTION TRIM(PP-FUNC)
                  " is not supported by POPOST"
               DELIMITED BY SIZE INTO PP-MESSAGE.
       9100-EXIT.
           EXIT.
