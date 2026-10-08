      *>================================================================
      *> PROGRAM-ID : YEAREND
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER (FIN)
      *> PURPOSE    : YEAR END TRANSFER.  EVERY PROFIT AND LOSS
      *>              ACCOUNT OF THE SELECTED FISCAL YEAR IS CLEARED
      *>              TO NIL AND THE RESULT IS CARRIED TO THE PROFIT
      *>              AND LOSS RESERVE.  BALANCE SHEET ACCOUNTS ARE
      *>              NOT TOUCHED - THEY CARRY FORWARD BY DEFINITION.
      *>              AC-013: THE TRANSFER HAPPENS ONCE.  A SECOND
      *>              RUN FINDS THE YEAR ALREADY CLOSED, REPORTS THE
      *>              ORIGINAL JOURNAL NUMBER AND POSTS NOTHING.  THE
      *>              CORRELATION ID IS DERIVED FROM THE COMPANY AND
      *>              THE YEAR, SO EVEN A FORCED RERUN IS ABSORBED BY
      *>              THE IDEMPOTENT POSTING SERVICE.
      *> RUN AS     : YEAREND CO=STC FY=2026 MODE=COMMIT USER=GLCLERK
      *> OUTPUT     : run\spool\YEAREND-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO, JNLPOST
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-11-04 PGW  CR0302 ORIGINAL YEAR END ROUTINE
      *> 1997-10-22 RJH  CR0781 ONCE ONLY PROTECTION - THE 1997 YEAR
      *>                 END WAS RUN TWICE AND DOUBLED THE RESERVE
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS
      *> 2026-10-06 STERP FIN-011/FIN-012 POSTING THROUGH JNLPOST
      *>================================================================
      *> REQ: FIN-011, FIN-012
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YEAREND.
       AUTHOR. P G WHITELAW.
       DATE-WRITTEN. 04/11/1994.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO WS-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-YEAREND.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY FINBATWK.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY JNLPOSTCA REPLACING ==DFHCOMMAREA== BY ==WS-JNLPOST-CA==
                      LEADING ==CA-== BY ==JH-==.
       01  WS-YEAR.
           05  WS-YEAR-STATUS          PIC X(8) VALUE SPACES.
           05  WS-YEAR-JNL             PIC X(30) VALUE SPACES.
           05  WS-YEAR-END-DATE        PIC 9(8) VALUE 0.
           05  WS-OPEN-PERIODS         PIC S9(4) VALUE 0.
           05  WS-RETAINED             PIC X(12) VALUE SPACES.
           05  WS-LINE-NO              PIC S9(4) VALUE 0.
       01  WS-DET.
           05  D-ACCT                  PIC X(12) VALUE SPACES.
           05  D-NAME                  PIC X(44) VALUE SPACES.
           05  D-CLASS                 PIC X(10) VALUE SPACES.
           05  D-BALANCE               PIC S9(13)V99 VALUE 0.
           05  D-DR                    PIC S9(13)V99 VALUE 0.
           05  D-CR                    PIC S9(13)V99 VALUE 0.
       01  WS-TOT.
           05  T-NET                   PIC S9(13)V99 VALUE 0.
           05  T-DR                    PIC S9(13)V99 VALUE 0.
           05  T-CR                    PIC S9(13)V99 VALUE 0.
           05  T-ACCTS                 PIC S9(7) VALUE 0.
           05  T-RESULT                PIC S9(13)V99 VALUE 0.
       REPORT SECTION.
       RD  R-YEAREND
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 52
           FOOTING 58.
       COPY STRPTHDR.
       01  PH-COLS TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(7) VALUE "Account".
               10  COLUMN 14   PIC X(4) VALUE "Name".
               10  COLUMN 60   PIC X(5) VALUE "Class".
               10  COLUMN 74   PIC X(16) VALUE "Balance in year ".
               10  COLUMN 96   PIC X(15) VALUE "Transfer debit ".
               10  COLUMN 114  PIC X(16) VALUE "Transfer credit ".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(12) SOURCE D-ACCT.
               10  COLUMN 14   PIC X(44) SOURCE D-NAME.
               10  COLUMN 60   PIC X(10) SOURCE D-CLASS.
               10  COLUMN 72   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-BALANCE.
               10  COLUMN 94   PIC Z,ZZZ,ZZZ,ZZ9.99  SOURCE D-DR.
               10  COLUMN 112  PIC Z,ZZZ,ZZZ,ZZ9.99  SOURCE D-CR.
       01  DE-NOTE TYPE IS DETAIL.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(100) SOURCE RPT-STATUS-TEXT.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(26) VALUE
                   "Accounts cleared to nil   ".
               10  COLUMN 28   PIC ZZZ,ZZ9 SOURCE T-ACCTS.
               10  COLUMN 94   PIC Z,ZZZ,ZZZ,ZZ9.99  SOURCE T-DR.
               10  COLUMN 112  PIC Z,ZZZ,ZZZ,ZZ9.99  SOURCE T-CR.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(40) VALUE
                   "Result carried to the profit and loss re".
               10  COLUMN 41   PIC X(6) VALUE "serve ".
               10  COLUMN 72   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-RESULT.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(66) VALUE
                   "A credit result is a profit; balance sheet lines".
               10  COLUMN 70   PIC X(60) VALUE
                   "carry forward untouched.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           MOVE "YEAREND" TO RPT-REPORT-ID
           PERFORM 9100-JOB-START THRU 9100-EXIT
           PERFORM 0200-READ-YEAR THRU 0200-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 9200-OPEN-SPOOL THRU 9200-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           INITIATE R-YEAREND
           PERFORM 1000-GATHER THRU 1000-EXIT
           PERFORM 2000-TRANSFER THRU 2000-EXIT
           TERMINATE R-YEAREND
           PERFORM 9350-CLOSE-SPOOL THRU 9350-EXIT.
       0000-FINISH.
           PERFORM 9150-JOB-END THRU 9150-EXIT
           MOVE WS-RC TO RETURN-CODE
           STOP RUN.

       0100-READ-ARGS.
           PERFORM 9500-STD-ARGS THRU 9500-EXIT
           MOVE "FY" TO FB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF FB-TEXT NOT = SPACES
               COMPUTE WS-FY = FUNCTION NUMVAL(FB-TEXT)
           END-IF
           IF WS-MODE = SPACES
               MOVE "PREVIEW" TO WS-MODE
           END-IF
           IF WS-FY = 0
               PERFORM 9600-RESOLVE-PERIOD THRU 9600-EXIT
           END-IF
           IF WS-FY = 0
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0101 FY=<fiscal year> is required"
               GO TO 0100-EXIT
           END-IF
           DISPLAY "STERP-I-0100 YEAREND " FUNCTION TRIM(WS-MODE)
                   " for " FUNCTION TRIM(WS-CO) " year " WS-FY.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-012  once only - a closed year with a transfer
      *> journal already recorded is reported, never re-posted
      *>----------------------------------------------------------------
       0200-READ-YEAR.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT y.Status, ISNULL(y.YearEndJournalNo,''), "
                  "CONVERT(char(8),y.EndDate,112), (SELECT COUNT(*) "
                  "FROM fin.Period p WHERE p.CompanyCode = "
                  "y.CompanyCode AND p.FiscalYear = y.FiscalYear AND"
                  " p.Status <> 'HARD'), ISNULL((SELECT MIN("
                  "a.AccountCode) FROM fin.Account a WHERE "
                  "a.CompanyCode = y.CompanyCode AND a.ControlType ="
                  " 'RETAINED'),'') FROM fin.FiscalYear y WHERE "
                  "y.CompanyCode = ? AND y.FiscalYear = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-FY TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 5 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF NOT SQLIO-OK
               MOVE 16 TO WS-RC
               DISPLAY "STERP-E-0201 fiscal year not defined"
               GO TO 0200-EXIT
           END-IF
           MOVE BR-COL(1) TO WS-YEAR-STATUS
           MOVE BR-COL(2) TO WS-YEAR-JNL
           COMPUTE WS-YEAR-END-DATE = FUNCTION NUMVAL(BR-COL(3))
           COMPUTE WS-OPEN-PERIODS = FUNCTION NUMVAL(BR-COL(4))
           MOVE BR-COL(5) TO WS-RETAINED
           IF WS-RETAINED = SPACES
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0202 no account carries ControlType"
                       " RETAINED"
               GO TO 0200-EXIT
           END-IF
           MOVE WS-YEAR-END-DATE TO WS-ASAT.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-GATHER.
           MOVE "Year end transfer to reserves" TO RPT-TITLE
           MOVE SPACES TO RPT-SUBTITLE
           MOVE WS-FY TO FB-SEQ-ED
           STRING "Fiscal year " FUNCTION TRIM(FB-SEQ-ED)
                  ", status " FUNCTION TRIM(WS-YEAR-STATUS)
               DELIMITED BY SIZE INTO RPT-SUBTITLE
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "Mode " FUNCTION TRIM(WS-MODE)
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           GENERATE PH-COLS
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT a.AccountCode, a.Name, a.ClassCode, "
                  "CONVERT(varchar(24),ISNULL(t.Dr,0)-ISNULL(t.Cr,0"
                  ")) FROM fin.Account a JOIN fin.AccountClass k ON "
                  "k.ClassCode = a.ClassCode OUTER APPLY (SELECT "
                  "SUM(l.DebitAmountGBP) Dr, SUM(l.CreditAmountGBP) "
                  "Cr FROM fin.JournalLine l JOIN fin.Journal j ON "
                  "j.JournalId = l.JournalId WHERE l.CompanyCode = "
                  "a.CompanyCode AND l.AccountCode = a.AccountCode "
                  "AND j.Status = 'POSTED' AND j.FiscalYear = ?) t "
                  "WHERE a.CompanyCode = ? AND k.Statement = 'PL' "
                  "AND a.AccountKind <> 'SUMMARY' ORDER BY "
                  "a.AccountCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-FY TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(1)
           MOVE WS-CO TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 4 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1001 " SQLIO-ERRMSG
               GO TO 1000-EXIT
           END-IF
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK
                   PERFORM 1100-ONE-ACCOUNT THRU 1100-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           COMPUTE T-RESULT = 0 - T-NET.
       1000-EXIT.
           EXIT.

       1100-ONE-ACCOUNT.
           COMPUTE D-BALANCE = FUNCTION NUMVAL(BR-COL(4))
           IF D-BALANCE = 0
               ADD 1 TO WS-SKIPPED
               GO TO 1100-EXIT
           END-IF
           MOVE BR-COL(1) TO D-ACCT
           MOVE BR-COL(2) TO D-NAME
           MOVE BR-COL(3) TO D-CLASS
           MOVE 0 TO D-DR
           MOVE 0 TO D-CR
           IF D-BALANCE > 0
               MOVE D-BALANCE TO D-CR
           ELSE
               COMPUTE D-DR = 0 - D-BALANCE
           END-IF
           ADD D-BALANCE TO T-NET
           ADD D-DR TO T-DR
           ADD D-CR TO T-CR
           ADD 1 TO T-ACCTS
           ADD 1 TO WS-ROWS-OUT
           IF WS-MODE = "COMMIT" AND WS-YEAR-JNL = SPACES
               PERFORM 1200-STAGE-LINE THRU 1200-EXIT
           END-IF
           GENERATE DE-LINE.
       1100-EXIT.
           EXIT.

       1200-STAGE-LINE.
           ADD 1 TO WS-LINE-NO
           PERFORM 1900-CORRELATION THRU 1900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.JournalStage (CorrelationId, LineNum, "
                  "CompanyCode, AccountCode, DebitAmount, "
                  "CreditAmount, CurrencyCode, DebitAmountGBP, "
                  "CreditAmountGBP, Narrative, CreatedBy) SELECT ?, "
                  "?, ?, ?, ?, ?, 'GBP', ?, ?, ?, ? WHERE NOT "
                  "EXISTS (SELECT 1 FROM fin.JournalStage s WHERE "
                  "s.CorrelationId = ? AND s.LineNum = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE FB-CORREL TO SQLIO-PARM-VAL(1) SQLIO-PARM-VAL(11)
           MOVE WS-LINE-NO TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(2)
                                            SQLIO-PARM-VAL(12)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           MOVE D-ACCT TO SQLIO-PARM-VAL(4)
           MOVE D-DR TO FB-NUM-ED
           MOVE FUNCTION TRIM(FB-NUM-ED) TO SQLIO-PARM-VAL(5)
                                            SQLIO-PARM-VAL(7)
           MOVE D-CR TO FB-NUM-ED
           MOVE FUNCTION TRIM(FB-NUM-ED) TO SQLIO-PARM-VAL(6)
                                            SQLIO-PARM-VAL(8)
           MOVE SPACES TO FB-TEXT
           STRING "Year end transfer, " FUNCTION TRIM(D-ACCT)
               DELIMITED BY SIZE INTO FB-TEXT
           MOVE FB-TEXT TO SQLIO-PARM-VAL(9)
           MOVE WS-USER TO SQLIO-PARM-VAL(10)
           PERFORM VARYING FB-I FROM 1 BY 1 UNTIL FB-I > 12
               MOVE "N" TO SQLIO-PARM-NULL(FB-I)
           END-PERFORM
           MOVE 12 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1201 " SQLIO-ERRMSG
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       1200-EXIT.
           EXIT.

       1900-CORRELATION.
           MOVE SPACES TO FB-CORREL
           MOVE WS-FY TO FB-SEQ-ED
           STRING "YEAREND-" FUNCTION TRIM(WS-CO) "-"
                  FUNCTION TRIM(FB-SEQ-ED)
               DELIMITED BY SIZE INTO FB-CORREL.
       1900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       2000-TRANSFER.
           MOVE SPACES TO RPT-STATUS-TEXT
           IF WS-YEAR-JNL NOT = SPACES
               STRING "Year already transferred by journal "
                      FUNCTION TRIM(WS-YEAR-JNL)
                      " - nothing posted"
                   DELIMITED BY SIZE INTO RPT-STATUS-TEXT
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-2001 year end already run, journal "
                       FUNCTION TRIM(WS-YEAR-JNL)
               GENERATE DE-NOTE
               GO TO 2000-EXIT
           END-IF
           IF WS-OPEN-PERIODS > 0
               STRING "Warning: " DELIMITED BY SIZE
                   INTO RPT-STATUS-TEXT
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-2002 " WS-OPEN-PERIODS
                       " period(s) are not hard closed"
           END-IF
           IF T-ACCTS = 0
               MOVE "No profit and loss balances to transfer"
                   TO RPT-STATUS-TEXT
               GENERATE DE-NOTE
               GO TO 2000-EXIT
           END-IF
           IF WS-MODE NOT = "COMMIT"
               MOVE "PREVIEW ONLY - no journal has been posted"
                   TO RPT-STATUS-TEXT
               GENERATE DE-NOTE
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-STAGE-RESERVE THRU 2100-EXIT
           PERFORM 2200-POST THRU 2200-EXIT
           GENERATE DE-NOTE.
       2000-EXIT.
           EXIT.

       2100-STAGE-RESERVE.
      *>   the balancing leg: a loss debits the reserve, a profit
      *>   credits it
           MOVE WS-RETAINED TO D-ACCT
           MOVE "Profit and loss reserve" TO D-NAME
           MOVE "EQ" TO D-CLASS
           MOVE T-NET TO D-BALANCE
           MOVE 0 TO D-DR
           MOVE 0 TO D-CR
           IF T-NET > 0
               MOVE T-NET TO D-DR
           ELSE
               COMPUTE D-CR = 0 - T-NET
           END-IF
           PERFORM 1200-STAGE-LINE THRU 1200-EXIT
           GENERATE DE-LINE.
       2100-EXIT.
           EXIT.

       2200-POST.
           PERFORM 1900-CORRELATION THRU 1900-EXIT
           INITIALIZE WS-JNLPOST-CA
           MOVE "POST" TO JH-FUNCTION
           MOVE WS-CO TO JH-COMPANY
           MOVE WS-USER TO JH-USER
           MOVE WS-YEAR-END-DATE TO JH-BUSINESS-DATE
           MOVE FB-CORREL TO JH-CORRELATION-ID
           MOVE "YEAREND" TO JP-JOURNAL-TYPE
           MOVE "Year end transfer to reserves" TO JP-DESCRIPTION
           MOVE "FIN" TO JP-SOURCE-MODULE
           MOVE FB-CORREL TO JP-SOURCE-REF
           MOVE WS-YEAR-END-DATE TO JP-DOCUMENT-DATE
           MOVE WS-YEAR-END-DATE TO JP-POSTING-DATE
           MOVE "GBP" TO JP-CURRENCY
           MOVE 1 TO JP-FX-RATE
           CALL "JNLPOST" USING WS-JNLPOST-CA
           IF JH-RETURN-CODE > 4
               MOVE 8 TO WS-RC
               MOVE JH-MESSAGE TO RPT-STATUS-TEXT
               DISPLAY "STERP-E-2201 " JH-MESSAGE
               GO TO 2200-EXIT
           END-IF
           MOVE JP-JOURNAL-NUMBER TO WS-YEAR-JNL
           IF WS-YEAR-JNL = SPACES
               MOVE JP-NEW-JOURNAL-NO TO WS-YEAR-JNL
           END-IF
           MOVE SPACES TO RPT-STATUS-TEXT
           IF JP-WAS-DUPLICATE = "Y"
               STRING "Transfer already existed as journal "
                      FUNCTION TRIM(WS-YEAR-JNL)
                      " - no second posting"
                   DELIMITED BY SIZE INTO RPT-STATUS-TEXT
               MOVE 4 TO WS-RC
           ELSE
               STRING "Transferred by journal "
                      FUNCTION TRIM(WS-YEAR-JNL)
                   DELIMITED BY SIZE INTO RPT-STATUS-TEXT
               PERFORM 2300-CLOSE-YEAR THRU 2300-EXIT
           END-IF
           ADD 1 TO WS-POSTED
           DISPLAY "STERP-I-2202 " FUNCTION TRIM(RPT-STATUS-TEXT).
       2200-EXIT.
           EXIT.

       2300-CLOSE-YEAR.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.FiscalYear SET Status = 'CLOSED', "
                  "YearEndJournalNo = ?, ClosedBy = ?, ClosedAt = "
                  "SYSDATETIME() WHERE CompanyCode = ? AND "
                  "FiscalYear = ? AND YearEndJournalNo IS NULL"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-YEAR-JNL TO SQLIO-PARM-VAL(1)
           MOVE WS-USER TO SQLIO-PARM-VAL(2)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           MOVE WS-FY TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING FB-I FROM 1 BY 1 UNTIL FB-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(FB-I)
           END-PERFORM
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-2301 " SQLIO-ERRMSG
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       2300-EXIT.
           EXIT.

       COPY FINBATPR.
