      *>================================================================
      *> PROGRAM-ID : ACCRREL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER
      *> PURPOSE    : RECURRING JOURNALS, ACCRUAL AND PREPAYMENT
      *>              RELEASE SCHEDULES (FIN-007, FIN-008).
      *>              THE SCHEDULE IS DERIVED FROM THE TEMPLATE: A
      *>              PREPAYMENT OF 300.00 OVER THREE PERIODS RELEASES
      *>              100.00 EACH TIME, THE LAST OCCURRENCE ABSORBS ANY
      *>              ROUNDING RESIDUAL, AND THERE IS NO FOURTH
      *>              RELEASE.  AN OCCURRENCE IS KEYED BY ITS NUMBER,
      *>              SO REPEATING A RUN CANNOT POST IT TWICE: THREE
      *>              OCCURRENCES OF 100.00 TOTAL 300.00 AND NOT 400.00
      *>              (AC-011).
      *>              A SCHEDULED PERIOD THAT IS CLOSED PRODUCES A
      *>              VISIBLE BLOCKED OCCURRENCE AND AN EXCEPTION, NOT
      *>              A SILENT POSTPONEMENT OR A REDATED ENTRY.
      *> CALLED BY  : RECURJNL, frmJournalEntry, .NET CLIENT
      *> INTERFACE  : CALL "ACCRREL" USING WS-ACCRREL-CA
      *>              COPYBOOK ACCRRELCA
      *> CALLS      : RNDALLOC, DATECALC, PERIODCK, EXCPRAIS, JNLPOST,
      *>              SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1995-02-20 DRO CR0220 ORIGINAL - MICRO FOCUS COBOL/2
      *> 1997-10-03 RJH CR0744 OCCURRENCE HISTORY TABLE ADDED AFTER A
      *>                MONTH END WAS RUN TWICE BY MISTAKE
      *> 2026-10-05 STERP BLOCKED OCCURRENCES RAISE AN EXCEPTION
      *>================================================================
      *> REQ: FIN-007, FIN-008
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCRREL.
       AUTHOR. D R OKONJO.
       DATE-WRITTEN. 20/02/1995.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY RNDALLOCCA REPLACING ==DFHCOMMAREA== BY ==WS-RNDALLOC-CA==
                      LEADING ==CA-== BY ==RH-==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATECALC-CA==
                      LEADING ==CA-== BY ==DH-==.
       COPY PERIODCKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERIODCK-CA==
                      LEADING ==CA-== BY ==PH-==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXCPRAIS-CA==
                      LEADING ==CA-== BY ==XH-==.
       COPY JNLPOSTCA REPLACING ==DFHCOMMAREA== BY ==WS-JNLPOST-CA==
                      LEADING ==CA-== BY ==JH-==.
       01  WS-SWITCHES.
           05  WS-REJECTED             PIC X VALUE "N".
               88  WS-IS-REJECTED                VALUE "Y".
           05  WS-FOUND                PIC X VALUE "N".
       01  WS-WORK.
           05  WS-IDX                  PIC S9(4) COMP-5 VALUE 0.
           05  WS-OCC                  PIC 9(4) VALUE 0.
           05  WS-MONTHS               PIC S9(4) VALUE 0.
           05  WS-STEP                 PIC S9(4) VALUE 1.
           05  WS-DATE                 PIC 9(8) VALUE 0.
           05  WS-AMT                  PIC S9(13)V99 VALUE 0.
           05  WS-SCALE                PIC S9(5)V9(8) VALUE 0.
           05  WS-LINE-NO              PIC 9(4) VALUE 0.
           05  WS-CORREL               PIC X(36).
           05  WS-MONEY-ED             PIC -(11)9.99.
           05  WS-OCC-ED               PIC 9(4).
       01  WS-TPL-ROW.
           05  WS-R-NAME               PIC X(80).
           05  WS-R-KIND               PIC X(10).
           05  WS-R-FREQ               PIC X(10).
           05  WS-R-START              PIC X(8).
           05  WS-R-END                PIC X(8).
           05  WS-R-SOURCE             PIC X(22).
           05  WS-R-PLANNED            PIC X(10).
           05  WS-R-POSTED             PIC X(10).
           05  WS-R-RELEASED           PIC X(22).
           05  WS-R-STATUS             PIC X(10).
           05  WS-R-EVENT              PIC X(30).
       01  WS-OCC-ROW.
           05  WS-O-NO                 PIC X(10).
           05  WS-O-DATE               PIC X(8).
           05  WS-O-AMOUNT             PIC X(22).
           05  WS-O-STATUS             PIC X(12).
           05  WS-O-JOURNAL            PIC X(30).
       01  WS-LINE-ROW.
           05  WS-L-NUM                PIC X(10).
           05  WS-L-ACCOUNT            PIC X(12).
           05  WS-L-DEBIT              PIC X(22).
           05  WS-L-CREDIT             PIC X(22).
           05  WS-L-CC                 PIC X(20).
           05  WS-L-PROJ               PIC X(20).
           05  WS-L-NARR               PIC X(80).
       01  WS-TEMPLATE.
           05  WS-T-CNT                PIC 9(3) VALUE 0.
           05  WS-T-LINE OCCURS 20 TIMES.
               10  WS-T-ACCOUNT        PIC X(12).
               10  WS-T-DEBIT          PIC S9(11)V99.
               10  WS-T-CREDIT         PIC S9(11)V99.
               10  WS-T-CC             PIC X(20).
               10  WS-T-PROJ           PIC X(20).
               10  WS-T-NARR           PIC X(80).
       LINKAGE SECTION.
       COPY ACCRRELCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           PERFORM 0200-EDIT-REQUEST THRU 0200-EXIT
           IF WS-IS-REJECTED
               GO TO 0000-RETURN
           END-IF
           PERFORM 0300-READ-TEMPLATE THRU 0300-EXIT
           IF WS-IS-REJECTED
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "SCHEDULE" PERFORM 1000-SCHEDULE THRU 1000-EXIT
               WHEN "NEXTOCC"  PERFORM 2000-NEXT-OCC THRU 2000-EXIT
               WHEN "RELEASE"  PERFORM 3000-RELEASE THRU 3000-EXIT
               WHEN "STATUS"   PERFORM 1000-SCHEDULE THRU 1000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.

       0100-INITIALISE.
           MOVE "N" TO WS-REJECTED
           MOVE "ACCRREL" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "FIN-007" TO CA-REQ-REF
           MOVE 0 TO AK-RELEASE-AMOUNT
           MOVE 0 TO AK-RELEASED-TOTAL
           MOVE 0 TO AK-REMAINING
           MOVE 0 TO AK-OCC-POSTED
           MOVE 0 TO AK-SCHED-CNT
           MOVE 0 TO AK-EXCEPTION-ID
           MOVE SPACES TO AK-JOURNAL-NUMBER
           MOVE SPACES TO AK-BLOCK-REASON
           MOVE "N" TO AK-ALREADY-POSTED
           MOVE "N" TO AK-BLOCKED
           MOVE 0 TO WS-T-CNT
           MOVE FUNCTION UPPER-CASE(AK-RECUR-CODE) TO AK-RECUR-CODE.
       0100-EXIT.
           EXIT.

       0200-EDIT-REQUEST.
           IF CA-COMPANY = SPACES
               MOVE "ACR0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF AK-RECUR-CODE = SPACES
               MOVE "ACR0002" TO CA-REASON-CODE
               MOVE "A recurring journal code is required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       0200-EXIT.
           EXIT.

       0300-READ-TEMPLATE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT r.Name, r.JournalKind, r.Frequency, "
                  "CONVERT(char(8), r.StartDate, 112), "
                  "ISNULL(CONVERT(char(8), r.EndDate, 112),''), "
                  "ISNULL(r.SourceAmount,0), "
                  "CAST(ISNULL(r.OccurrencesPlanned,0) AS varchar(10))"
                  ", CAST(r.OccurrencesPosted AS varchar(10)), "
                  "r.ReleasedAmount, r.Status, ISNULL(r.EventCode,'') "
                  "FROM fin.RecurringJournal r WHERE r.CompanyCode = ?"
                  " AND r.RecurringCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE AK-RECUR-CODE TO SQLIO-PARM-VAL(2)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 11 TO SQLIO-COL-CNT
           MOVE 80 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 10 TO SQLIO-COL-WIDTH(3)
           MOVE 8 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-COL-WIDTH(5)
           MOVE 22 TO SQLIO-COL-WIDTH(6)
           MOVE 10 TO SQLIO-COL-WIDTH(7)
           MOVE 10 TO SQLIO-COL-WIDTH(8)
           MOVE 22 TO SQLIO-COL-WIDTH(9)
           MOVE 10 TO SQLIO-COL-WIDTH(10)
           MOVE 30 TO SQLIO-COL-WIDTH(11)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TPL-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "ACR0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Recurring journal "
                      FUNCTION TRIM(AK-RECUR-CODE)
                      " is not held for " FUNCTION TRIM(CA-COMPANY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               MOVE "Y" TO WS-REJECTED
               ADD 1 TO CA-MSG-COUNT
               GO TO 0300-EXIT
           END-IF
           MOVE WS-R-NAME TO AK-NAME
           MOVE WS-R-KIND TO AK-KIND
           MOVE WS-R-FREQ TO AK-FREQUENCY
           MOVE WS-R-STATUS TO AK-RECUR-STATUS
           MOVE WS-R-EVENT TO AK-EVENT-CODE
           COMPUTE AK-START-DATE = FUNCTION NUMVAL(WS-R-START)
           IF WS-R-END NOT = SPACES
               COMPUTE AK-END-DATE = FUNCTION NUMVAL(WS-R-END)
           END-IF
           COMPUTE AK-SOURCE-AMOUNT = FUNCTION NUMVAL(WS-R-SOURCE)
           COMPUTE AK-OCC-PLANNED = FUNCTION NUMVAL(WS-R-PLANNED)
           COMPUTE AK-OCC-POSTED = FUNCTION NUMVAL(WS-R-POSTED)
           COMPUTE AK-RELEASED-TOTAL = FUNCTION NUMVAL(WS-R-RELEASED)
           COMPUTE AK-REMAINING =
               AK-SOURCE-AMOUNT - AK-RELEASED-TOTAL
           IF AK-OCC-PLANNED = 0
               MOVE 1 TO AK-OCC-PLANNED
           END-IF
           EVALUATE AK-FREQUENCY
               WHEN "QUARTERLY" MOVE 3 TO WS-STEP
               WHEN "ANNUAL"    MOVE 12 TO WS-STEP
               WHEN OTHER       MOVE 1 TO WS-STEP
           END-EVALUATE.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000 - BUILD THE SCHEDULE.  THE RELEASE IS AN EQUAL SPLIT OF
      *>        THE SOURCE BALANCE THAT ADDS BACK EXACTLY (COM-011).
      *>----------------------------------------------------------------
       1000-SCHEDULE.
           PERFORM 1100-SPLIT-AMOUNT THRU 1100-EXIT
           MOVE 0 TO AK-SCHED-CNT
           MOVE AK-START-DATE TO WS-DATE
           PERFORM 1200-ONE-OCCURRENCE THRU 1200-EXIT
               VARYING WS-OCC FROM 1 BY 1
               UNTIL WS-OCC > AK-OCC-PLANNED
                  OR AK-SCHED-CNT NOT < 36
           PERFORM 1300-APPLY-HISTORY THRU 1300-EXIT
           MOVE SPACES TO CA-MESSAGE
           MOVE AK-REMAINING TO WS-MONEY-ED
           STRING FUNCTION TRIM(AK-KIND) " schedule of "
                  FUNCTION TRIM(AK-RECUR-CODE) ", "
                  FUNCTION TRIM(WS-MONEY-ED) " GBP remaining"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       1000-EXIT.
           EXIT.

       1100-SPLIT-AMOUNT.
           IF AK-KIND = "RECURRING" OR AK-SOURCE-AMOUNT = 0
      *>       A fixed recurring journal posts its template value each
      *>       time; there is no source balance to exhaust.
               MOVE 0 TO AK-RELEASE-AMOUNT
               GO TO 1100-EXIT
           END-IF
           INITIALIZE WS-RNDALLOC-CA
           MOVE "ALLOC" TO RH-FUNCTION
           MOVE CA-COMPANY TO RH-COMPANY
           MOVE CA-USER TO RH-USER
           MOVE CA-BUSINESS-DATE TO RH-BUSINESS-DATE
           MOVE AK-SOURCE-AMOUNT TO RA-AMOUNT
           MOVE AK-OCC-PLANNED TO RA-LINE-CNT
           MOVE "EQUAL" TO RA-MODE
           MOVE 2 TO RA-PLACES
           CALL "RNDALLOC" USING WS-RNDALLOC-CA
           IF RH-RETURN-CODE > 4
               COMPUTE AK-RELEASE-AMOUNT ROUNDED =
                   AK-SOURCE-AMOUNT / AK-OCC-PLANNED
           ELSE
               MOVE RA-PART(1) TO AK-RELEASE-AMOUNT
           END-IF.
       1100-EXIT.
           EXIT.

       1200-ONE-OCCURRENCE.
           ADD 1 TO AK-SCHED-CNT
           MOVE AK-SCHED-CNT TO WS-IDX
           MOVE WS-OCC TO AK-S-OCC-NO(WS-IDX)
           IF WS-OCC = 1
               MOVE AK-START-DATE TO WS-DATE
           ELSE
               PERFORM 1250-ADD-PERIOD THRU 1250-EXIT
           END-IF
           MOVE WS-DATE TO AK-S-DATE(WS-IDX)
           IF AK-KIND = "RECURRING"
               MOVE 0 TO AK-S-AMOUNT(WS-IDX)
           ELSE
               IF WS-OCC = AK-OCC-PLANNED
      *>           The last release clears the residual exactly, so the
      *>           schedule totals the approved source balance (AC-011)
                   COMPUTE AK-S-AMOUNT(WS-IDX) = AK-SOURCE-AMOUNT
                       - (AK-RELEASE-AMOUNT * (AK-OCC-PLANNED - 1))
               ELSE
                   MOVE AK-RELEASE-AMOUNT TO AK-S-AMOUNT(WS-IDX)
               END-IF
           END-IF
           MOVE "DUE" TO AK-S-STATUS(WS-IDX)
           MOVE SPACES TO AK-S-JOURNAL(WS-IDX).
       1200-EXIT.
           EXIT.

       1250-ADD-PERIOD.
           INITIALIZE WS-DATECALC-CA
           MOVE "ADDMONTH" TO DH-FUNCTION
           MOVE CA-COMPANY TO DH-COMPANY
           MOVE CA-USER TO DH-USER
           MOVE CA-BUSINESS-DATE TO DH-BUSINESS-DATE
           MOVE WS-DATE TO DC-DATE-1
           MOVE WS-STEP TO DC-NUMBER
           CALL "DATECALC" USING WS-DATECALC-CA
           IF DH-RETURN-CODE < 8
               MOVE DC-RESULT-DATE TO WS-DATE
           END-IF.
       1250-EXIT.
           EXIT.

       1300-APPLY-HISTORY.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(o.OccurrenceNo AS varchar(10)), "
                  "CONVERT(char(8), o.ScheduledDate, 112), o.Amount, "
                  "o.Status, ISNULL(o.JournalNumber,'') "
                  "FROM fin.RecurringOccurrence o "
                  "WHERE o.CompanyCode = ? AND o.RecurringCode = ? "
                  "ORDER BY o.OccurrenceNo"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE AK-RECUR-CODE TO SQLIO-PARM-VAL(2)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 8 TO SQLIO-COL-WIDTH(2)
           MOVE 22 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 30 TO SQLIO-COL-WIDTH(5)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-OCC-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 1300-EXIT
           END-IF
           PERFORM 1350-MERGE-OCC THRU 1350-EXIT
               UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-OCC-ROW.
       1300-EXIT.
           EXIT.

       1350-MERGE-OCC.
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-OCC-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               GO TO 1350-EXIT
           END-IF
           COMPUTE WS-OCC = FUNCTION NUMVAL(WS-O-NO)
           IF WS-OCC < 1 OR WS-OCC > AK-SCHED-CNT
               GO TO 1350-EXIT
           END-IF
           MOVE WS-OCC TO WS-IDX
           MOVE WS-O-STATUS TO AK-S-STATUS(WS-IDX)
           MOVE WS-O-JOURNAL TO AK-S-JOURNAL(WS-IDX)
           IF WS-O-STATUS = "POSTED"
               COMPUTE AK-S-AMOUNT(WS-IDX) =
                   FUNCTION NUMVAL(WS-O-AMOUNT)
           END-IF.
       1350-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000 - THE NEXT OCCURRENCE THAT IS NOT YET POSTED
      *>----------------------------------------------------------------
       2000-NEXT-OCC.
           PERFORM 1000-SCHEDULE THRU 1000-EXIT
           MOVE 0 TO AK-OCCURRENCE-NO
           MOVE "N" TO WS-FOUND
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > AK-SCHED-CNT OR WS-FOUND = "Y"
               IF AK-S-STATUS(WS-IDX) NOT = "POSTED"
                   MOVE AK-S-OCC-NO(WS-IDX) TO AK-OCCURRENCE-NO
                   MOVE AK-S-DATE(WS-IDX) TO AK-NEXT-DUE-DATE
                   MOVE AK-S-AMOUNT(WS-IDX) TO AK-RELEASE-AMOUNT
                   MOVE "Y" TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = "N"
               MOVE 4 TO CA-RETURN-CODE
               MOVE "ACR0020" TO CA-REASON-CODE
               MOVE "The schedule is complete. There is no further occ"
                   TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 2000-EXIT
           END-IF
           MOVE AK-NEXT-DUE-DATE TO AK-POSTING-DATE
           PERFORM 2100-CHECK-PERIOD THRU 2100-EXIT.
       2000-EXIT.
           EXIT.

       2100-CHECK-PERIOD.
           INITIALIZE WS-PERIODCK-CA
           MOVE "CHECK" TO PH-FUNCTION
           MOVE CA-COMPANY TO PH-COMPANY
           MOVE CA-USER TO PH-USER
           MOVE CA-BUSINESS-DATE TO PH-BUSINESS-DATE
           MOVE AK-POSTING-DATE TO PC-POSTING-DATE
           MOVE AK-POSTING-DATE TO PC-DOCUMENT-DATE
           MOVE "POST" TO PC-ACTION
           CALL "PERIODCK" USING WS-PERIODCK-CA
           MOVE PC-PERIOD-STATUS TO AK-PERIOD-STATUS
           MOVE PC-FISCAL-YEAR TO AK-FISCAL-YEAR
           MOVE PC-PERIOD-NO TO AK-PERIOD-NO
           IF PH-RETURN-CODE > 4
               MOVE "Y" TO AK-BLOCKED
               MOVE PH-MESSAGE TO AK-BLOCK-REASON
               MOVE PH-REASON-CODE TO CA-REASON-CODE
               MOVE PH-MESSAGE TO CA-MESSAGE
               MOVE PH-RETURN-CODE TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000 - RELEASE ONE OCCURRENCE, ONCE AND ONCE ONLY
      *>----------------------------------------------------------------
       3000-RELEASE.
           IF AK-OCCURRENCE-NO = 0
               PERFORM 2000-NEXT-OCC THRU 2000-EXIT
               IF CA-FAILED OR AK-OCCURRENCE-NO = 0
                   GO TO 3000-EXIT
               END-IF
           ELSE
               PERFORM 1000-SCHEDULE THRU 1000-EXIT
               MOVE AK-OCCURRENCE-NO TO WS-IDX
               IF WS-IDX > AK-SCHED-CNT
                   MOVE "ACR0030" TO CA-REASON-CODE
                   MOVE "That occurrence is beyond the approved schedu"
                       TO CA-MESSAGE
                   PERFORM 9000-REJECT THRU 9000-EXIT
                   GO TO 3000-EXIT
               END-IF
               MOVE AK-S-DATE(WS-IDX) TO AK-POSTING-DATE
               MOVE AK-S-AMOUNT(WS-IDX) TO AK-RELEASE-AMOUNT
               PERFORM 2100-CHECK-PERIOD THRU 2100-EXIT
           END-IF
           MOVE AK-OCCURRENCE-NO TO WS-IDX
           IF AK-S-STATUS(WS-IDX) = "POSTED"
               MOVE "Y" TO AK-ALREADY-POSTED
               MOVE AK-S-JOURNAL(WS-IDX) TO AK-JOURNAL-NUMBER
               MOVE 4 TO CA-RETURN-CODE
               MOVE "ACR0031" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               MOVE AK-OCCURRENCE-NO TO WS-OCC-ED
               STRING "Occurrence " WS-OCC-ED " has already been "
                      "posted as journal "
                      FUNCTION TRIM(AK-JOURNAL-NUMBER)
                      ". It is not posted a second time"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 3000-EXIT
           END-IF
           IF AK-BLOCKED = "Y"
               PERFORM 3900-BLOCK-OCCURRENCE THRU 3900-EXIT
               GO TO 3000-EXIT
           END-IF
           IF AK-RECUR-STATUS NOT = "ACTIVE"
               MOVE "ACR0032" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Recurring journal "
                      FUNCTION TRIM(AK-RECUR-CODE) " is "
                      FUNCTION TRIM(AK-RECUR-STATUS)
                      " and does not release"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           IF AK-KIND NOT = "RECURRING" AND AK-REMAINING NOT > 0
               MOVE "ACR0033" TO CA-REASON-CODE
               MOVE "The approved source balance is fully released. No"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           PERFORM 3100-READ-LINES THRU 3100-EXIT
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           PERFORM 3200-BUILD-CORREL THRU 3200-EXIT
           PERFORM 3300-STAGE-LINES THRU 3300-EXIT
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           PERFORM 3400-POST THRU 3400-EXIT
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           PERFORM 3500-RECORD-OCC THRU 3500-EXIT
           PERFORM 3600-UPDATE-HEADER THRU 3600-EXIT
           MOVE SPACES TO CA-MESSAGE
           MOVE AK-OCCURRENCE-NO TO WS-OCC-ED
           MOVE AK-RELEASE-AMOUNT TO WS-MONEY-ED
           STRING "Occurrence " WS-OCC-ED " released as journal "
                  FUNCTION TRIM(AK-JOURNAL-NUMBER) " for "
                  FUNCTION TRIM(WS-MONEY-ED) " GBP"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       3000-EXIT.
           EXIT.

       3100-READ-LINES.
           MOVE 0 TO WS-T-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(l.LineNum AS varchar(10)), "
                  "l.AccountCode, l.DebitAmount, l.CreditAmount, "
                  "ISNULL(l.DimCostCentre,''), "
                  "ISNULL(l.DimProject,''), ISNULL(l.Narrative,'') "
                  "FROM fin.RecurringLine l WHERE l.CompanyCode = ? "
                  "AND l.RecurringCode = ? ORDER BY l.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE AK-RECUR-CODE TO SQLIO-PARM-VAL(2)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 7 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 22 TO SQLIO-COL-WIDTH(3)
           MOVE 22 TO SQLIO-COL-WIDTH(4)
           MOVE 20 TO SQLIO-COL-WIDTH(5)
           MOVE 20 TO SQLIO-COL-WIDTH(6)
           MOVE 80 TO SQLIO-COL-WIDTH(7)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3100-EXIT
           END-IF
           PERFORM 3150-TAKE-LINE THRU 3150-EXIT
               UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR OR WS-T-CNT NOT < 20
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           IF WS-T-CNT = 0
               MOVE "ACR0034" TO CA-REASON-CODE
               MOVE "The recurring journal has no template lines"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       3100-EXIT.
           EXIT.

       3150-TAKE-LINE.
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               GO TO 3150-EXIT
           END-IF
           ADD 1 TO WS-T-CNT
           MOVE WS-T-CNT TO WS-LINE-NO
           MOVE WS-L-ACCOUNT TO WS-T-ACCOUNT(WS-LINE-NO)
           COMPUTE WS-T-DEBIT(WS-LINE-NO) =
               FUNCTION NUMVAL(WS-L-DEBIT)
           COMPUTE WS-T-CREDIT(WS-LINE-NO) =
               FUNCTION NUMVAL(WS-L-CREDIT)
           MOVE WS-L-CC TO WS-T-CC(WS-LINE-NO)
           MOVE WS-L-PROJ TO WS-T-PROJ(WS-LINE-NO)
           MOVE WS-L-NARR TO WS-T-NARR(WS-LINE-NO).
       3150-EXIT.
           EXIT.

      *> The correlation id is derived, not random, so a repeated run
      *> presents the same instruction and gets the same answer back
      *> instead of a second journal (COM-015).
       3200-BUILD-CORREL.
           MOVE SPACES TO WS-CORREL
           MOVE AK-OCCURRENCE-NO TO WS-OCC-ED
           STRING "RJ-" FUNCTION TRIM(CA-COMPANY) "-"
                  FUNCTION TRIM(AK-RECUR-CODE) "-" WS-OCC-ED
               DELIMITED BY SIZE INTO WS-CORREL.
       3200-EXIT.
           EXIT.

       3300-STAGE-LINES.
           MOVE 1 TO WS-SCALE
           IF AK-KIND NOT = "RECURRING" AND AK-SOURCE-AMOUNT NOT = 0
               COMPUTE WS-SCALE =
                   AK-RELEASE-AMOUNT / AK-SOURCE-AMOUNT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE fin.JournalStage WHERE CorrelationId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CORREL TO SQLIO-PARM-VAL(1)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           PERFORM 3350-STAGE-ONE THRU 3350-EXIT
               VARYING WS-LINE-NO FROM 1 BY 1
               UNTIL WS-LINE-NO > WS-T-CNT OR CA-FAILED.
       3300-EXIT.
           EXIT.

       3350-STAGE-ONE.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.JournalStage (CorrelationId, LineNum, "
                  "CompanyCode, AccountCode, DebitAmount, "
                  "CreditAmount, CurrencyCode, DimCostCentre, "
                  "DimProject, Narrative, CreatedBy) VALUES (?, ?, ?, "
                  "?, ?, ?, 'GBP', NULLIF(?,''), NULLIF(?,''), ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 10 TO SQLIO-PARM-CNT
           MOVE WS-CORREL TO SQLIO-PARM-VAL(1)
           MOVE WS-LINE-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE WS-T-ACCOUNT(WS-LINE-NO) TO SQLIO-PARM-VAL(4)
           COMPUTE WS-AMT ROUNDED =
               WS-T-DEBIT(WS-LINE-NO) * WS-SCALE
           MOVE WS-AMT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           COMPUTE WS-AMT ROUNDED =
               WS-T-CREDIT(WS-LINE-NO) * WS-SCALE
           MOVE WS-AMT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE WS-T-CC(WS-LINE-NO) TO SQLIO-PARM-VAL(7)
           MOVE WS-T-PROJ(WS-LINE-NO) TO SQLIO-PARM-VAL(8)
           MOVE WS-T-NARR(WS-LINE-NO) TO SQLIO-PARM-VAL(9)
           MOVE CA-USER TO SQLIO-PARM-VAL(10)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       3350-EXIT.
           EXIT.

       3400-POST.
           INITIALIZE WS-JNLPOST-CA
           MOVE "POST" TO JH-FUNCTION
           MOVE CA-COMPANY TO JH-COMPANY
           MOVE CA-SITE TO JH-SITE
           MOVE CA-USER TO JH-USER
           MOVE CA-BUSINESS-DATE TO JH-BUSINESS-DATE
           MOVE WS-CORREL TO JH-CORRELATION-ID
           MOVE AK-POSTING-DATE TO JP-POSTING-DATE
           MOVE AK-POSTING-DATE TO JP-DOCUMENT-DATE
           MOVE "GBP" TO JP-CURRENCY
           MOVE "FIN" TO JP-SOURCE-MODULE
           MOVE AK-RECUR-CODE TO JP-SOURCE-REF
           MOVE AK-EVENT-CODE TO JP-EVENT-CODE
           MOVE "JNL" TO JP-SERIES-CODE
           IF AK-KIND = "ACCRUAL"
               MOVE "ACCRUAL" TO JP-JOURNAL-TYPE
           ELSE
               IF AK-KIND = "PREPAY"
                   MOVE "PREPAY" TO JP-JOURNAL-TYPE
               ELSE
                   MOVE "RECUR" TO JP-JOURNAL-TYPE
               END-IF
           END-IF
           MOVE SPACES TO JP-DESCRIPTION
           MOVE AK-OCCURRENCE-NO TO WS-OCC-ED
           STRING FUNCTION TRIM(AK-NAME) " occurrence " WS-OCC-ED
               DELIMITED BY SIZE INTO JP-DESCRIPTION
           MOVE SPACES TO JP-EVIDENCE-REF
           STRING "fin.RecurringJournal/"
                  FUNCTION TRIM(AK-RECUR-CODE)
               DELIMITED BY SIZE INTO JP-EVIDENCE-REF
           CALL "JNLPOST" USING WS-JNLPOST-CA
           MOVE JP-NEW-JOURNAL-NO TO AK-JOURNAL-NUMBER
           IF JH-RETURN-CODE > 4
               MOVE JH-REASON-CODE TO CA-REASON-CODE
               MOVE JH-MESSAGE TO CA-MESSAGE
               MOVE JH-RETURN-CODE TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       3400-EXIT.
           EXIT.

       3500-RECORD-OCC.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.RecurringOccurrence (CompanyCode, "
                  "RecurringCode, OccurrenceNo, ScheduledDate, "
                  "PostingDate, FiscalYear, PeriodNo, Amount, Status, "
                  "JournalNumber, CorrelationId, PostedBy, PostedAt) "
                  "SELECT ?, ?, ?, ?, ?, ?, ?, ?, 'POSTED', ?, ?, ?, "
                  "SYSDATETIME() WHERE NOT EXISTS (SELECT 1 FROM "
                  "fin.RecurringOccurrence x WHERE x.CompanyCode = ? "
                  "AND x.RecurringCode = ? AND x.OccurrenceNo = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 13 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE AK-RECUR-CODE TO SQLIO-PARM-VAL(2)
           MOVE AK-OCCURRENCE-NO TO SQLIO-PARM-VAL(3)
           MOVE AK-POSTING-DATE TO SQLIO-PARM-VAL(4)
           MOVE AK-POSTING-DATE TO SQLIO-PARM-VAL(5)
           MOVE AK-FISCAL-YEAR TO SQLIO-PARM-VAL(6)
           MOVE AK-PERIOD-NO TO SQLIO-PARM-VAL(7)
           MOVE AK-RELEASE-AMOUNT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE AK-JOURNAL-NUMBER TO SQLIO-PARM-VAL(9)
           MOVE WS-CORREL TO SQLIO-PARM-VAL(10)
           MOVE CA-USER TO SQLIO-PARM-VAL(11)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(12)
           MOVE AK-RECUR-CODE TO SQLIO-PARM-VAL(13)
           MOVE AK-OCCURRENCE-NO TO SQLIO-PARM-VAL(14)
           MOVE 14 TO SQLIO-PARM-CNT
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-OCC-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       3500-EXIT.
           EXIT.

       3600-UPDATE-HEADER.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE r SET OccurrencesPosted = x.Posted, "
                  "ReleasedAmount = x.Released, RemainingAmount = "
                  "ISNULL(r.SourceAmount,0) - x.Released, "
                  "LastOccurrenceNo = x.MaxOcc, Status = CASE WHEN "
                  "r.OccurrencesPlanned IS NOT NULL AND x.Posted >= "
                  "r.OccurrencesPlanned THEN 'ENDED' ELSE r.Status "
                  "END, ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "FROM fin.RecurringJournal r CROSS APPLY (SELECT "
                  "COUNT(*) AS Posted, ISNULL(SUM(o.Amount),0) AS "
                  "Released, ISNULL(MAX(o.OccurrenceNo),0) AS MaxOcc "
                  "FROM fin.RecurringOccurrence o WHERE o.CompanyCode "
                  "= r.CompanyCode AND o.RecurringCode = "
                  "r.RecurringCode AND o.Status = 'POSTED') x "
                  "WHERE r.CompanyCode = ? AND r.RecurringCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE AK-RECUR-CODE TO SQLIO-PARM-VAL(3)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-OCC-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3600-EXIT
           END-IF
           ADD 1 TO AK-OCC-POSTED
           ADD AK-RELEASE-AMOUNT TO AK-RELEASED-TOTAL
           COMPUTE AK-REMAINING =
               AK-SOURCE-AMOUNT - AK-RELEASED-TOTAL.
       3600-EXIT.
           EXIT.

      *> A blocked occurrence is written down and owned.  It is never
      *> quietly moved to another date (FIN-007, COM-022).
       3900-BLOCK-OCCURRENCE.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.RecurringOccurrence (CompanyCode, "
                  "RecurringCode, OccurrenceNo, ScheduledDate, Amount,"
                  " Status, BlockReason) SELECT ?, ?, ?, ?, ?, "
                  "'BLOCKED', ? WHERE NOT EXISTS (SELECT 1 FROM "
                  "fin.RecurringOccurrence x WHERE x.CompanyCode = ? "
                  "AND x.RecurringCode = ? AND x.OccurrenceNo = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 9 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE AK-RECUR-CODE TO SQLIO-PARM-VAL(2)
           MOVE AK-OCCURRENCE-NO TO SQLIO-PARM-VAL(3)
           MOVE AK-POSTING-DATE TO SQLIO-PARM-VAL(4)
           MOVE AK-RELEASE-AMOUNT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE AK-BLOCK-REASON TO SQLIO-PARM-VAL(6)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(7)
           MOVE AK-RECUR-CODE TO SQLIO-PARM-VAL(8)
           MOVE AK-OCCURRENCE-NO TO SQLIO-PARM-VAL(9)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-OCC-ROW
           INITIALIZE WS-EXCPRAIS-CA
           MOVE "RAISE" TO XH-FUNCTION
           MOVE CA-COMPANY TO XH-COMPANY
           MOVE CA-USER TO XH-USER
           MOVE CA-BUSINESS-DATE TO XH-BUSINESS-DATE
           MOVE "FIN" TO EX-MODULE
           MOVE "HOLD" TO EX-SEVERITY
           MOVE "RECURRING OCCURRENCE BLOCKED" TO EX-CATEGORY
           MOVE AK-BLOCK-REASON TO EX-MESSAGE-TEXT
           MOVE "RECURJNL" TO EX-DOCUMENT-TYPE
           MOVE AK-RECUR-CODE TO EX-DOCUMENT-REF
           MOVE "FINCTRL" TO EX-OWNER-ROLE
           MOVE 2 TO EX-PRIORITY
           MOVE AK-RELEASE-AMOUNT TO EX-AMOUNT-GBP
           CALL "EXCPRAIS" USING WS-EXCPRAIS-CA
           MOVE EX-EXCEPTION-ID TO AK-EXCEPTION-ID
           MOVE 6 TO CA-RETURN-CODE
           MOVE "ACR0040" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE AK-OCCURRENCE-NO TO WS-OCC-ED
           STRING "Occurrence " WS-OCC-ED " cannot be released: "
                  FUNCTION TRIM(AK-BLOCK-REASON)
                  " It is recorded as blocked and owned, not redated"
               DELIMITED BY SIZE INTO CA-MESSAGE
           ADD 1 TO CA-MSG-COUNT.
       3900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-NOT-NULL.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-IDX)
           END-PERFORM.
       8000-EXIT.
           EXIT.

       9000-REJECT.
           MOVE "Y" TO WS-REJECTED
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE "ACR0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "ACCRREL does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in ACCRREL" TO CA-MESSAGE
           END-IF
           MOVE 12 TO CA-RETURN-CODE
           MOVE "Y" TO WS-REJECTED
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
