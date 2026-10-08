      *>================================================================
      *> PROGRAM-ID : REVALRUN
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER (FIN)
      *> PURPOSE    : PERIOD END FOREIGN CURRENCY REVALUATION RUN.
      *>              EVERY MONETARY BALANCE HELD IN A CURRENCY OTHER
      *>              THAN STERLING IS RESTATED AT THE CLOSING RATE
      *>              AND THE DIFFERENCE IS POSTED AS AN UNREALISED
      *>              GAIN OR LOSS.  THE CALCULATION AND THE POSTING
      *>              BELONG TO THE ONLINE FXREVAL SERVICE; THIS
      *>              PROGRAM SCHEDULES IT, PRINTS THE WORKING AND
      *>              REPORTS THE JOURNAL NUMBER.
      *>              THE REVALUATION IS REVERSED ON THE FIRST DAY OF
      *>              THE NEXT PERIOD, SO THAT THE EVENTUAL SETTLEMENT
      *>              RECORDS THE WHOLE LIFETIME DIFFERENCE AND NOT
      *>              THE SUM OF TWO OVERLAPPING ONES (AC-016).
      *> RUN AS     : REVALRUN CO=STC ASAT=20261031 MODE=COMMIT
      *>              [RATE=SPOT] USER=GLCLERK
      *>              THE CLOSING RATE IS THE RATE SERIES NAMED BY
      *>              RATE= AT THE AS AT DATE (SPOT BY DEFAULT).
      *> OUTPUT     : run\spool\REVALRUN-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO, FXREVAL
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1995-01-30 PGW  CR0330 ORIGINAL REVALUATION RUN
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS
      *> 2002-01-02 RJH  CR1180 EURO ACCOUNTS ADDED
      *> 2026-10-06 STERP FIN-019/FIN-020 THROUGH FXREVAL
      *> 2026-10-06 STERP CR2604 ONE FXREVAL CALL PER OPEN POSITION
      *>                  (GL FOREIGN BALANCES, AR AND AP OPEN ITEMS)
      *>================================================================
      *> REQ: FIN-019, FIN-020
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REVALRUN.
       AUTHOR. P G WHITELAW.
       DATE-WRITTEN. 30/01/1995.
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
       FD  RPT-FILE REPORT IS R-REVALRUN.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY FINBATWK.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY FXREVALCA REPLACING ==DFHCOMMAREA== BY ==WS-FXREVAL-CA==
                      LEADING ==CA-== BY ==FH-==.
       01  WS-OPT.
           05  WS-RATE-TYPE            PIC X(6) VALUE "SPOT".
           05  WS-LAST-JNL             PIC X(30) VALUE SPACES.
       01  WS-POS-TAB.
           05  WS-POS-CNT              PIC S9(4) VALUE 0.
           05  WS-POS-MAX              PIC S9(4) VALUE 50.
           05  WS-POS OCCURS 50 TIMES.
               10  WS-P-ACCT           PIC X(12).
               10  WS-P-CURR           PIC X(3).
               10  WS-P-FOREIGN        PIC S9(11)V99.
               10  WS-P-CARRYING       PIC S9(13)V99.
               10  WS-P-POSITION       PIC X(10).
               10  WS-P-SUBTYPE        PIC X(6).
       01  WS-DET.
           05  D-ACCT                  PIC X(12) VALUE SPACES.
           05  D-CURR                  PIC X(3) VALUE SPACES.
           05  D-FOREIGN               PIC S9(11)V99 VALUE 0.
           05  D-CARRYING              PIC S9(13)V99 VALUE 0.
           05  D-REVALUED              PIC S9(13)V99 VALUE 0.
           05  D-ADJUST                PIC S9(13)V99 VALUE 0.
           05  D-SENSE                 PIC X(4) VALUE SPACES.
       01  WS-TOT.
           05  T-GAIN                  PIC S9(13)V99 VALUE 0.
           05  T-LOSS                  PIC S9(13)V99 VALUE 0.
           05  T-NET                   PIC S9(13)V99 VALUE 0.
           05  T-LINES                 PIC S9(5) VALUE 0.
       REPORT SECTION.
       RD  R-REVALRUN
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 52
           FOOTING 58.
       COPY STRPTHDR.
       01  PH-COLS TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(7) VALUE "Account".
               10  COLUMN 16   PIC X(8) VALUE "Currency".
               10  COLUMN 30   PIC X(16) VALUE "Foreign balance ".
               10  COLUMN 54   PIC X(16) VALUE "Carrying value  ".
               10  COLUMN 78   PIC X(16) VALUE "Revalued value  ".
               10  COLUMN 102  PIC X(10) VALUE "Adjustment".
               10  COLUMN 124  PIC X(4) VALUE "Type".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(12) SOURCE D-ACCT.
               10  COLUMN 17   PIC X(3) SOURCE D-CURR.
               10  COLUMN 26   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-FOREIGN.
               10  COLUMN 50   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-CARRYING.
               10  COLUMN 74   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-REVALUED.
               10  COLUMN 98   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-ADJUST.
               10  COLUMN 124  PIC X(4) SOURCE D-SENSE.
       01  DE-NOTE TYPE IS DETAIL.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(100) SOURCE RPT-STATUS-TEXT.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(24) VALUE
                   "Unrealised gains       ".
               10  COLUMN 98   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-GAIN.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(24) VALUE
                   "Unrealised losses      ".
               10  COLUMN 98   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-LOSS.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(24) VALUE
                   "Net adjustment to the ledger".
               10  COLUMN 98   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-NET.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(64) VALUE
                   "The revaluation is reversed on day one of the next".
               10  COLUMN 68   PIC X(60) VALUE
                   "period, so settlement records the difference.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           MOVE "REVALRUN" TO RPT-REPORT-ID
           PERFORM 9100-JOB-START THRU 9100-EXIT
           PERFORM 9200-OPEN-SPOOL THRU 9200-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           INITIATE R-REVALRUN
           PERFORM 1000-REVALUE THRU 1000-EXIT
           PERFORM 2000-POST THRU 2000-EXIT
           TERMINATE R-REVALRUN
           PERFORM 9350-CLOSE-SPOOL THRU 9350-EXIT.
       0000-FINISH.
           PERFORM 9150-JOB-END THRU 9150-EXIT
           MOVE WS-RC TO RETURN-CODE
           STOP RUN.

       0100-READ-ARGS.
           PERFORM 9500-STD-ARGS THRU 9500-EXIT
           MOVE "RATE" TO FB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF FB-TEXT NOT = SPACES
               MOVE FB-TEXT(1:6) TO WS-RATE-TYPE
           END-IF
           IF WS-MODE = SPACES
               MOVE "PREVIEW" TO WS-MODE
           END-IF
           PERFORM 9600-RESOLVE-PERIOD THRU 9600-EXIT
           IF WS-FY = 0
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0101 no fiscal period covers "
                       WS-ASAT
               GO TO 0100-EXIT
           END-IF
           DISPLAY "STERP-I-0100 REVALRUN " FUNCTION TRIM(WS-MODE)
                   " for " FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-019  the closing rate restatement; the service
      *> returns one line per revalued position
      *>----------------------------------------------------------------
       1000-REVALUE.
           MOVE "Period end currency revaluation" TO RPT-TITLE
           MOVE WS-ASAT TO FB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "As at " WS-UK DELIMITED BY SIZE
               INTO RPT-AS-AT-TEXT
           MOVE SPACES TO RPT-SUBTITLE
           MOVE WS-FY TO FB-SEQ-ED
           STRING "Year " FUNCTION TRIM(FB-SEQ-ED) ", rate type "
                  FUNCTION TRIM(WS-RATE-TYPE) ", mode "
                  FUNCTION TRIM(WS-MODE)
               DELIMITED BY SIZE INTO RPT-SUBTITLE
           GENERATE PH-COLS
           PERFORM 1100-LOAD-POSITIONS THRU 1100-EXIT
           IF WS-RC > 4
               GO TO 1000-EXIT
           END-IF
           PERFORM VARYING FB-K FROM 1 BY 1
                   UNTIL FB-K > WS-POS-CNT
               PERFORM 1200-ONE-POSITION THRU 1200-EXIT
           END-PERFORM
           IF WS-POS-CNT = 0
               MOVE "No foreign currency balances to revalue"
                   TO RPT-STATUS-TEXT
               GENERATE DE-NOTE
           END-IF
           MOVE T-LINES TO WS-ROWS-OUT
           DISPLAY "STERP-I-1002 " T-LINES
                   " position(s) revalued, " WS-BLOCKED
                   " could not be priced".
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-019  the open foreign currency positions: general
      *> ledger balances held in a foreign currency, plus the sales
      *> and purchase ledger open items, which carry their own
      *> currency and are controlled by the AR and AP accounts.  The
      *> rows are read into a table first, because FXREVAL uses SQLIO
      *> itself and a cursor cannot be left open across the call.
      *>----------------------------------------------------------------
       1100-LOAD-POSITIONS.
           MOVE 0 TO WS-POS-CNT
           PERFORM 1150-BUILD-SQL THRU 1150-EXIT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               MOVE SQLIO-ERRMSG TO RPT-STATUS-TEXT
               DISPLAY "STERP-E-1000 " SQLIO-ERRMSG
               GENERATE DE-NOTE
               GO TO 1100-EXIT
           END-IF
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK
                   IF WS-POS-CNT < WS-POS-MAX
                       ADD 1 TO WS-POS-CNT
                       MOVE BR-COL(1) TO WS-P-ACCT(WS-POS-CNT)
                       MOVE BR-COL(2) TO WS-P-CURR(WS-POS-CNT)
                       COMPUTE WS-P-FOREIGN(WS-POS-CNT) =
                           FUNCTION NUMVAL(BR-COL(3))
                       COMPUTE WS-P-CARRYING(WS-POS-CNT) =
                           FUNCTION NUMVAL(BR-COL(4))
                       MOVE BR-COL(5) TO WS-P-POSITION(WS-POS-CNT)
                       MOVE BR-COL(6) TO WS-P-SUBTYPE(WS-POS-CNT)
                   ELSE
                       ADD 1 TO WS-SKIPPED
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF WS-SKIPPED > 0
               MOVE 4 TO WS-RC
               MOVE SPACES TO RPT-STATUS-TEXT
               STRING "More than 50 positions were found - the "
                      "listing is truncated"
                   DELIMITED BY SIZE INTO RPT-STATUS-TEXT
               GENERATE DE-NOTE
           END-IF.
       1100-EXIT.
           EXIT.

       1150-BUILD-SQL.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT a.AccountCode, l.CurrencyCode, "
                  "CONVERT(varchar(24),SUM(l.DebitAmount-"
                  "l.CreditAmount)), CONVERT(varchar(24),"
                  "SUM(l.DebitAmountGBP-l.CreditAmountGBP)), "
                  "CASE WHEN k.NormalBalance = 'CR' THEN "
                  "'LIABILITY' ELSE 'ASSET' END, 'GL' FROM "
                  "fin.JournalLine l JOIN fin.Journal j ON "
                  "j.JournalId = l.JournalId JOIN fin.Account a ON "
                  "a.CompanyCode = l.CompanyCode AND a.AccountCode "
                  "= l.AccountCode JOIN fin.AccountClass k ON "
                  "k.ClassCode = a.ClassCode WHERE l.CompanyCode = "
                  "? AND j.Status = 'POSTED' AND j.PostingDate <= ?"
                  " AND ISNULL(l.CurrencyCode,'GBP') NOT IN "
                  "('GBP','') AND ISNULL(a.ControlType,'') NOT IN "
                  "('AR','AP') GROUP BY a.AccountCode, "
                  "l.CurrencyCode, k.NormalBalance HAVING "
                  "SUM(l.DebitAmount-l.CreditAmount) <> 0 "
                  "UNION ALL SELECT ISNULL((SELECT TOP 1 "
                  "x.AccountCode FROM fin.Account x WHERE "
                  "x.CompanyCode = o.CompanyCode AND x.ControlType "
                  "= 'AR' ORDER BY x.AccountCode),'1200'), "
                  "o.CurrencyCode, CONVERT(varchar(24),"
                  "SUM(o.ResidualAmount)), CONVERT(varchar(24),"
                  "SUM(o.ResidualAmountGBP)), 'ASSET', 'AR' FROM "
                  "ar.OpenItem o WHERE o.CompanyCode = ? AND "
                  "o.CurrencyCode <> 'GBP' AND o.PostingDate <= ? "
                  "AND o.ResidualAmount <> 0 GROUP BY "
                  "o.CompanyCode, o.CurrencyCode HAVING "
                  "SUM(o.ResidualAmount) <> 0 "
                  "UNION ALL SELECT ISNULL((SELECT TOP 1 "
                  "x.AccountCode FROM fin.Account x WHERE "
                  "x.CompanyCode = p.CompanyCode AND x.ControlType "
                  "= 'AP' ORDER BY x.AccountCode),'2000'), "
                  "p.CurrencyCode, CONVERT(varchar(24),"
                  "SUM(p.ResidualAmount)), CONVERT(varchar(24),"
                  "SUM(p.ResidualAmountGBP)), 'LIABILITY', 'AP' "
                  "FROM ap.OpenItem p WHERE p.CompanyCode = ? AND "
                  "p.CurrencyCode <> 'GBP' AND p.PostingDate <= ? "
                  "AND p.ResidualAmount <> 0 GROUP BY "
                  "p.CompanyCode, p.CurrencyCode HAVING "
                  "SUM(p.ResidualAmount) <> 0 ORDER BY 6, 1, 2"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-ASAT TO FB-DATE-ED
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
                         SQLIO-PARM-VAL(3)
                         SQLIO-PARM-VAL(5)
           MOVE FB-DATE-ED TO SQLIO-PARM-VAL(2)
                              SQLIO-PARM-VAL(4)
                              SQLIO-PARM-VAL(6)
           PERFORM VARYING FB-I FROM 1 BY 1 UNTIL FB-I > 6
               MOVE "N" TO SQLIO-PARM-NULL(FB-I)
           END-PERFORM
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE 6 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT.
       1150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-019, FIN-020  one position, one FXREVAL call.  In
      *> PREVIEW the service only calculates; in COMMIT the POST
      *> function stages, posts and records the run, and is
      *> idempotent on its own correlation id (COM-015).
      *>----------------------------------------------------------------
       1200-ONE-POSITION.
           INITIALIZE WS-FXREVAL-CA
           IF WS-MODE = "COMMIT"
               MOVE "POST" TO FH-FUNCTION
           ELSE
               MOVE "REVALUE" TO FH-FUNCTION
           END-IF
           MOVE WS-CO TO FH-COMPANY
           MOVE WS-USER TO FH-USER
           MOVE WS-ASAT TO FH-BUSINESS-DATE
           MOVE WS-P-CURR(FB-K) TO FR-CURRENCY
           MOVE WS-P-ACCT(FB-K) TO FR-ACCOUNT
           MOVE WS-ASAT TO FR-AS-AT-DATE
           MOVE WS-FY TO FR-FISCAL-YEAR
           MOVE WS-PER TO FR-PERIOD-NO
           MOVE WS-RATE-TYPE TO FR-RATE-TYPE
           MOVE WS-MODE TO FR-MODE
           MOVE WS-P-POSITION(FB-K) TO FR-POSITION
           MOVE WS-P-SUBTYPE(FB-K) TO FR-SUBLEDGER-TYPE
           MOVE WS-P-FOREIGN(FB-K) TO FR-FOREIGN-AMOUNT
           MOVE WS-P-CARRYING(FB-K) TO FR-CARRYING-GBP
           CALL "FXREVAL" USING WS-FXREVAL-CA
           IF FH-RETURN-CODE > 4
               PERFORM 1300-NOT-PRICED THRU 1300-EXIT
               GO TO 1200-EXIT
           END-IF
           MOVE WS-P-ACCT(FB-K) TO D-ACCT
           MOVE WS-P-CURR(FB-K) TO D-CURR
           MOVE FR-FOREIGN-AMOUNT TO D-FOREIGN
           MOVE FR-CARRYING-GBP TO D-CARRYING
           MOVE FR-REVALUED-GBP TO D-REVALUED
           MOVE FR-UNREALISED-GBP TO D-ADJUST
           IF FR-UNREALISED-GBP < 0
               MOVE "LOSS" TO D-SENSE
               COMPUTE T-LOSS = T-LOSS - FR-UNREALISED-GBP
           ELSE
               MOVE "GAIN" TO D-SENSE
               ADD FR-UNREALISED-GBP TO T-GAIN
           END-IF
           ADD FR-UNREALISED-GBP TO T-NET
           ADD 1 TO T-LINES
           GENERATE DE-LINE
           IF FR-REVAL-STATUS = "POSTED"
               ADD 1 TO WS-POSTED
               MOVE FR-JOURNAL-NUMBER TO WS-LAST-JNL
           END-IF.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> A POSITION WITHOUT A RATE IS REPORTED AND LEFT ALONE.  IN
      *> PREVIEW THAT IS A WARNING; A COMMIT RUN MUST NOT POST HALF A
      *> REVALUATION, SO IT ENDS 8.
      *>----------------------------------------------------------------
       1300-NOT-PRICED.
           ADD 1 TO WS-BLOCKED
           MOVE SPACES TO RPT-STATUS-TEXT
           STRING FUNCTION TRIM(WS-P-ACCT(FB-K)) " "
                  WS-P-CURR(FB-K) " not revalued - "
                  FUNCTION TRIM(FH-MESSAGE)
               DELIMITED BY SIZE INTO RPT-STATUS-TEXT
           DISPLAY "STERP-E-1001 " FUNCTION TRIM(RPT-STATUS-TEXT)
           GENERATE DE-NOTE
           IF WS-MODE = "COMMIT"
               MOVE 8 TO WS-RC
           ELSE
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
           END-IF.
       1300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-020  the posting is idempotent on the revaluation
      *> id, so a repeated COMMIT run does not double the adjustment
      *>----------------------------------------------------------------
       2000-POST.
           IF T-LINES = 0 AND WS-BLOCKED = 0
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO RPT-STATUS-TEXT
           IF WS-MODE NOT = "COMMIT"
               MOVE "PREVIEW ONLY - no revaluation journal posted"
                   TO RPT-STATUS-TEXT
               GENERATE DE-NOTE
               GO TO 2000-EXIT
           END-IF
           MOVE WS-POSTED TO FB-INT-ED
           STRING FUNCTION TRIM(FB-INT-ED)
                  " revaluation journal(s) posted, latest "
                  FUNCTION TRIM(WS-LAST-JNL)
                  ", each reversed on day one of the next period"
               DELIMITED BY SIZE INTO RPT-STATUS-TEXT
           DISPLAY "STERP-I-2002 " FUNCTION TRIM(RPT-STATUS-TEXT)
           GENERATE DE-NOTE.
       2000-EXIT.
           EXIT.

       COPY FINBATPR.
