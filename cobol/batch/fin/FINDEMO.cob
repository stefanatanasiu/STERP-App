      *>================================================================
      *> PROGRAM-ID : FINDEMO
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER (FIN)
      *> PURPOSE    : DEMONSTRATION POSTING GENERATOR.  BUILDS A YEAR
      *>              OF PLAUSIBLE TRADING ACTIVITY FOR A COMPANY SO
      *>              THAT THE TRIAL BALANCE, PROFIT AND LOSS, BALANCE
      *>              SHEET, CASH BOOK AND BUDGET REPORTS HAVE
      *>              SOMETHING TO SHOW.  THE ACCOUNTS ARE DISCOVERED
      *>              FROM THE CHART OF ACCOUNTS BY CLASS, SO THE
      *>              PROGRAM WORKS FOR ANY COMPANY WITHOUT HARD CODED
      *>              ACCOUNT NUMBERS, AND CONTROL ACCOUNTS ARE
      *>              DELIBERATELY AVOIDED - THEY BELONG TO THEIR OWN
      *>              SUBLEDGERS (FIN-002).
      *>              EVERY JOURNAL CARRIES A CORRELATION ID DERIVED
      *>              FROM COMPANY, YEAR, PERIOD AND KIND, SO RUNNING
      *>              THE GENERATOR AGAIN POSTS NOTHING NEW.  A SEED
      *>              SCRIPT MAY THEREFORE CALL IT ON EVERY DEPLOY.
      *> RUN AS     : FINDEMO CO=STC FY=2026 FROM=1 TO=9 MODE=COMMIT
      *>              USER=GLCLERK
      *> OUTPUT     : run\spool\FINDEMO-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO, JNLPOST
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2026-10-06 STERP ORIGINAL - DEMONSTRATION DATA GENERATOR
      *>================================================================
      *> REQ: FIN-004, FIN-005, FIN-024
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINDEMO.
       AUTHOR. STERP.
       DATE-WRITTEN. 06/10/2026.
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
       FD  RPT-FILE REPORT IS R-FINDEMO.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY FINBATWK.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY JNLPOSTCA REPLACING ==DFHCOMMAREA== BY ==WS-JNLPOST-CA==
                      LEADING ==CA-== BY ==JH-==.
       01  WS-ACC.
           05  A-DEBT                  PIC X(12) VALUE SPACES.
           05  A-CRED                  PIC X(12) VALUE SPACES.
           05  A-CASH                  PIC X(12) VALUE SPACES.
           05  A-TURN                  PIC X(12) VALUE SPACES.
           05  A-COS                   PIC X(12) VALUE SPACES.
           05  A-ADMIN                 PIC X(12) VALUE SPACES.
           05  A-DIST                  PIC X(12) VALUE SPACES.
       01  WS-JNL.
           05  J-KIND                  PIC X(10) VALUE SPACES.
           05  J-DESC                  PIC X(60) VALUE SPACES.
           05  J-DATE                  PIC 9(8) VALUE 0.
           05  J-LINE-NO               PIC S9(4) VALUE 0.
           05  J-DR-ACCT               PIC X(12) VALUE SPACES.
           05  J-CR-ACCT               PIC X(12) VALUE SPACES.
           05  J-AMOUNT                PIC S9(11)V99 VALUE 0.
       01  WS-PER-DATES.
           05  P-START                 PIC 9(8) VALUE 0.
           05  P-END                   PIC 9(8) VALUE 0.
      *>   FIN-009.  A demonstration generator has no authority to
      *>   post into a closed period and never had: the ledger
      *>   refused periods 1-6 and 9 with JNP0017/JNP0018 every run
      *>   and the job ended RC 4 on a perfectly healthy database.
      *>   The period's own status is read up front so a closed
      *>   period is reported as skipped, not as a failure.
           05  P-STATUS                PIC X(12) VALUE SPACES.
               88  P-POSTABLE                    VALUE "OPEN".
       01  WS-DET.
           05  D-PERIOD                PIC 9(2) VALUE 0.
           05  D-KIND                  PIC X(10) VALUE SPACES.
           05  D-DESC                  PIC X(44) VALUE SPACES.
           05  D-DATE                  PIC X(10) VALUE SPACES.
           05  D-AMOUNT                PIC S9(11)V99 VALUE 0.
           05  D-JNL                   PIC X(24) VALUE SPACES.
           05  D-OUTCOME               PIC X(16) VALUE SPACES.
       01  WS-TOT.
           05  T-POSTED                PIC S9(5) VALUE 0.
           05  T-EXISTING              PIC S9(5) VALUE 0.
           05  T-FAILED                PIC S9(5) VALUE 0.
           05  T-CLOSED                PIC S9(5) VALUE 0.
           05  T-VALUE                 PIC S9(13)V99 VALUE 0.
       REPORT SECTION.
       RD  R-FINDEMO
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 52
           FOOTING 58.
       COPY STRPTHDR.
       01  PH-COLS TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(6) VALUE "Period".
               10  COLUMN 9    PIC X(4) VALUE "Kind".
               10  COLUMN 21   PIC X(11) VALUE "Description".
               10  COLUMN 67   PIC X(4) VALUE "Date".
               10  COLUMN 82   PIC X(6) VALUE "Amount".
               10  COLUMN 98   PIC X(7) VALUE "Journal".
               10  COLUMN 124  PIC X(7) VALUE "Outcome".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 2    PIC Z9 SOURCE D-PERIOD.
               10  COLUMN 9    PIC X(10) SOURCE D-KIND.
               10  COLUMN 21   PIC X(44) SOURCE D-DESC.
               10  COLUMN 66   PIC X(10) SOURCE D-DATE.
               10  COLUMN 78   PIC ZZZ,ZZZ,ZZ9.99- SOURCE D-AMOUNT.
               10  COLUMN 95   PIC X(24) SOURCE D-JNL.
               10  COLUMN 120  PIC X(13) SOURCE D-OUTCOME.
       01  DE-NOTE TYPE IS DETAIL.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(100) SOURCE RPT-STATUS-TEXT.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(22) VALUE "Journals posted      ".
               10  COLUMN 24   PIC ZZZ,ZZ9 SOURCE T-POSTED.
               10  COLUMN 40   PIC X(22) VALUE "Already present      ".
               10  COLUMN 63   PIC ZZZ,ZZ9 SOURCE T-EXISTING.
               10  COLUMN 78   PIC ZZZ,ZZZ,ZZ9.99- SOURCE T-VALUE.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(22) VALUE "Refused by the ledger".
               10  COLUMN 24   PIC ZZZ,ZZ9 SOURCE T-FAILED.
               10  COLUMN 40   PIC X(22) VALUE "Period closed, skipped".
               10  COLUMN 63   PIC ZZZ,ZZ9 SOURCE T-CLOSED.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(66) VALUE
                   "Demonstration data only.  Each journal is balanced".
               10  COLUMN 70   PIC X(60) VALUE
                   "and idempotent on its correlation id.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           MOVE "FINDEMO" TO RPT-REPORT-ID
           PERFORM 9100-JOB-START THRU 9100-EXIT
           PERFORM 0200-FIND-ACCOUNTS THRU 0200-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 9200-OPEN-SPOOL THRU 9200-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           INITIATE R-FINDEMO
           PERFORM 1000-GENERATE THRU 1000-EXIT
           TERMINATE R-FINDEMO
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
           MOVE "FROM" TO FB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF FB-TEXT NOT = SPACES
               COMPUTE WS-PER-FROM = FUNCTION NUMVAL(FB-TEXT)
           END-IF
           MOVE "TO" TO FB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF FB-TEXT NOT = SPACES
               COMPUTE WS-PER-TO = FUNCTION NUMVAL(FB-TEXT)
           END-IF
           IF WS-MODE = SPACES
               MOVE "COMMIT" TO WS-MODE
           END-IF
           IF WS-FY = 0
               PERFORM 9600-RESOLVE-PERIOD THRU 9600-EXIT
           END-IF
           IF WS-FY = 0
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0101 FY=<fiscal year> is required"
               GO TO 0100-EXIT
           END-IF
           IF WS-PER-FROM = 0
               MOVE 1 TO WS-PER-FROM
           END-IF
           IF WS-PER-TO = 0
               MOVE WS-PER TO WS-PER-TO
           END-IF
           IF WS-PER-TO = 0
               MOVE 12 TO WS-PER-TO
           END-IF
           DISPLAY "STERP-I-0100 FINDEMO " FUNCTION TRIM(WS-MODE)
                   " for " FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-002  a control account belongs to its subledger, so
      *> the generator picks ordinary posting accounts only
      *>----------------------------------------------------------------
       0200-FIND-ACCOUNTS.
           MOVE "DEBT" TO FB-TEXT
           PERFORM 0250-PICK THRU 0250-EXIT
           MOVE FB-TEXT(1:12) TO A-DEBT
           MOVE "CRED" TO FB-TEXT
           PERFORM 0250-PICK THRU 0250-EXIT
           MOVE FB-TEXT(1:12) TO A-CRED
           MOVE "CASH" TO FB-TEXT
           PERFORM 0250-PICK THRU 0250-EXIT
           MOVE FB-TEXT(1:12) TO A-CASH
           MOVE "TURN" TO FB-TEXT
           PERFORM 0250-PICK THRU 0250-EXIT
           MOVE FB-TEXT(1:12) TO A-TURN
           MOVE "COS" TO FB-TEXT
           PERFORM 0250-PICK THRU 0250-EXIT
           MOVE FB-TEXT(1:12) TO A-COS
           MOVE "ADMIN" TO FB-TEXT
           PERFORM 0250-PICK THRU 0250-EXIT
           MOVE FB-TEXT(1:12) TO A-ADMIN
           MOVE "DIST" TO FB-TEXT
           PERFORM 0250-PICK THRU 0250-EXIT
           MOVE FB-TEXT(1:12) TO A-DIST
           IF A-DEBT = SPACES OR A-CRED = SPACES OR
              A-CASH = SPACES OR A-TURN = SPACES OR
              A-COS = SPACES OR A-ADMIN = SPACES
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0201 the chart of accounts does not"
                       " offer a posting account for every class"
           END-IF.
       0200-EXIT.
           EXIT.

       0250-PICK.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 AccountCode FROM fin.Account WHERE "
                  "CompanyCode = ? AND ClassCode = ? AND "
                  "AccountKind <> 'SUMMARY' AND Status = 'ACTIVE' "
                  "AND AllowManualJournal = 1 AND (ControlType IS "
                  "NULL OR ControlType = '') ORDER BY AccountCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE FB-TEXT(1:10) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           MOVE SPACES TO FB-TEXT
           IF SQLIO-OK
               MOVE BR-COL(1) TO FB-TEXT
           END-IF.
       0250-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-GENERATE.
           MOVE "Demonstration posting generator" TO RPT-TITLE
           MOVE SPACES TO RPT-SUBTITLE
           MOVE WS-FY TO FB-SEQ-ED
           STRING "Fiscal year " FUNCTION TRIM(FB-SEQ-ED)
               DELIMITED BY SIZE INTO RPT-SUBTITLE
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "Mode " FUNCTION TRIM(WS-MODE)
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           GENERATE PH-COLS
           PERFORM VARYING WS-PER FROM WS-PER-FROM BY 1
                   UNTIL WS-PER > WS-PER-TO
               PERFORM 1100-ONE-PERIOD THRU 1100-EXIT
           END-PERFORM
           MOVE SPACES TO RPT-STATUS-TEXT
           IF WS-MODE = "COMMIT"
               STRING "Demonstration data is in place; rerunning "
                      "this job posts nothing new."
                   DELIMITED BY SIZE INTO RPT-STATUS-TEXT
           ELSE
               MOVE "PREVIEW ONLY - no journal has been posted"
                   TO RPT-STATUS-TEXT
           END-IF
           GENERATE DE-NOTE
           MOVE T-POSTED TO WS-POSTED
           IF T-FAILED > 0
               MOVE 4 TO WS-RC
           END-IF
           DISPLAY "STERP-I-1001 " T-POSTED " posted, "
                   T-EXISTING " already present, "
                   T-CLOSED " period closed, "
                   T-FAILED " refused".
       1000-EXIT.
           EXIT.

       1100-ONE-PERIOD.
           PERFORM 1150-PERIOD-DATES THRU 1150-EXIT
           IF P-END = 0
               ADD 1 TO WS-SKIPPED
               GO TO 1100-EXIT
           END-IF
      *>   a month of trading: invoice, cost, overhead, collection
           MOVE "SALES" TO J-KIND
           MOVE "Monthly sales invoicing, demonstration data"
               TO J-DESC
           MOVE P-END TO J-DATE
           MOVE A-DEBT TO J-DR-ACCT
           MOVE A-TURN TO J-CR-ACCT
           COMPUTE J-AMOUNT = 12000 + (WS-PER * 375)
           PERFORM 2000-ONE-JOURNAL THRU 2000-EXIT
           MOVE "PURCH" TO J-KIND
           MOVE "Cost of goods sold for the month" TO J-DESC
           MOVE A-COS TO J-DR-ACCT
           MOVE A-CRED TO J-CR-ACCT
           COMPUTE J-AMOUNT = 7200 + (WS-PER * 195)
           PERFORM 2000-ONE-JOURNAL THRU 2000-EXIT
           MOVE "OVHD" TO J-KIND
           MOVE "Administrative overhead for the month" TO J-DESC
           MOVE A-ADMIN TO J-DR-ACCT
           MOVE A-CASH TO J-CR-ACCT
           COMPUTE J-AMOUNT = 2600 + (WS-PER * 40)
           PERFORM 2000-ONE-JOURNAL THRU 2000-EXIT
           IF A-DIST NOT = SPACES
               MOVE "DIST" TO J-KIND
               MOVE "Carriage and distribution for the month"
                   TO J-DESC
               MOVE A-DIST TO J-DR-ACCT
               MOVE A-CASH TO J-CR-ACCT
               COMPUTE J-AMOUNT = 640 + (WS-PER * 15)
               PERFORM 2000-ONE-JOURNAL THRU 2000-EXIT
           END-IF
           MOVE "CASH" TO J-KIND
           MOVE "Customer receipts banked" TO J-DESC
           MOVE A-CASH TO J-DR-ACCT
           MOVE A-DEBT TO J-CR-ACCT
           COMPUTE J-AMOUNT = 11400 + (WS-PER * 300)
           PERFORM 2000-ONE-JOURNAL THRU 2000-EXIT.
       1100-EXIT.
           EXIT.

       1150-PERIOD-DATES.
           MOVE 0 TO P-START
           MOVE 0 TO P-END
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(char(8),StartDate,112), CONVERT("
                  "char(8),EndDate,112), Status FROM fin.Period "
                  "WHERE CompanyCode = ? AND FiscalYear = ? AND "
                  "PeriodNo = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-FY TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(2)
           MOVE WS-PER TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING FB-I FROM 1 BY 1 UNTIL FB-I > 3
               MOVE "N" TO SQLIO-PARM-NULL(FB-I)
           END-PERFORM
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE 3 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               COMPUTE P-START = FUNCTION NUMVAL(BR-COL(1))
               COMPUTE P-END = FUNCTION NUMVAL(BR-COL(2))
               MOVE BR-COL(3) TO P-STATUS
           ELSE
               MOVE SPACES TO P-STATUS
           END-IF.
       1150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-004  two balanced lines staged under a correlation
      *> id made of company, year, period and kind - the posting
      *> service absorbs a repeat run without a duplicate journal
      *>----------------------------------------------------------------
       2000-ONE-JOURNAL.
           MOVE WS-PER TO D-PERIOD
           MOVE J-KIND TO D-KIND
           MOVE J-DESC TO D-DESC
           MOVE J-DATE TO FB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE WS-UK TO D-DATE
           MOVE J-AMOUNT TO D-AMOUNT
           MOVE SPACES TO D-JNL
           MOVE SPACES TO D-OUTCOME
           PERFORM 2100-CORRELATION THRU 2100-EXIT
           IF WS-MODE NOT = "COMMIT"
               MOVE "PREVIEW" TO D-OUTCOME
               GENERATE DE-LINE
               GO TO 2000-EXIT
           END-IF
      *>   FIN-009.  A period that is hard closed, soft closed or
      *>   not yet open is not a failure of this job: nothing is
      *>   staged for it, the reason is on the report, and the run
      *>   still ends clean.  Only an open period is a posting.
           IF NOT P-POSTABLE
               MOVE FUNCTION TRIM(P-STATUS) TO D-JNL
               MOVE "SKIPPED-CLOSED" TO D-OUTCOME
               ADD 1 TO T-CLOSED
               GENERATE DE-LINE
               GO TO 2000-EXIT
           END-IF
           MOVE 0 TO J-LINE-NO
           PERFORM 2150-CLEAR-STAGE THRU 2150-EXIT
           MOVE J-DR-ACCT TO WS-ACCOUNT
           MOVE J-AMOUNT TO FB-AMT
           MOVE 0 TO FB-AMT2
           PERFORM 2200-STAGE-LINE THRU 2200-EXIT
           MOVE J-CR-ACCT TO WS-ACCOUNT
           MOVE 0 TO FB-AMT
           MOVE J-AMOUNT TO FB-AMT2
           PERFORM 2200-STAGE-LINE THRU 2200-EXIT
           PERFORM 2300-POST THRU 2300-EXIT
           GENERATE DE-LINE.
       2000-EXIT.
           EXIT.

       2100-CORRELATION.
           MOVE SPACES TO FB-CORREL
           MOVE WS-FY TO FB-SEQ-ED
           STRING "FINDEMO-" FUNCTION TRIM(WS-CO) "-"
                  FUNCTION TRIM(FB-SEQ-ED) "-"
               DELIMITED BY SIZE INTO FB-CORREL
           PERFORM VARYING FB-J FROM 36 BY -1
               UNTIL FB-J < 2 OR FB-CORREL(FB-J:1) NOT = SPACE
               CONTINUE
           END-PERFORM
           ADD 1 TO FB-J
           MOVE WS-PER TO FB-SEQ-ED
           MOVE FB-SEQ-ED(5:2) TO FB-CORREL(FB-J:2)
           ADD 2 TO FB-J
           MOVE "-" TO FB-CORREL(FB-J:1)
           ADD 1 TO FB-J
           MOVE J-KIND(1:5) TO FB-CORREL(FB-J:5).
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> FIN-004.  Any residue of an earlier attempt under this
      *> correlation id goes before the lines are staged again.  The
      *> protection against a double posting is core.IdempotencyKey,
      *> never a half staged basket: rows left behind by a refused
      *> attempt used to survive a correction and be posted exactly
      *> as they were first written.
      *>----------------------------------------------------------------
       2150-CLEAR-STAGE.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM fin.JournalStage WHERE "
                  "CorrelationId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE FB-CORREL TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       2150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> FIN-004.  A turnover, cost or overhead account carries a
      *> MANDATORY cost centre rule, and fin.usp_PostJournal throws
      *> STERP-E-0013 for a line that has none - which reached this
      *> job as a bare SQL0001 and lost every trading journal of
      *> every open period.  The cost centre is taken from the
      *> company's own analysis values, and only for an account
      *> whose rule actually demands one: a balance sheet account
      *> forbids it and would be refused just as firmly.
      *>----------------------------------------------------------------
       2200-STAGE-LINE.
           ADD 1 TO J-LINE-NO
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.JournalStage (CorrelationId, LineNum, "
                  "CompanyCode, AccountCode, DebitAmount, "
                  "CreditAmount, CurrencyCode, DebitAmountGBP, "
                  "CreditAmountGBP, DimCostCentre, Narrative, "
                  "CreatedBy) SELECT ?, ?, ?, ?, ?, ?, 'GBP', ?, "
                  "?, (SELECT TOP (1) v.DimValue FROM "
                  "fin.DimensionValue v WHERE v.CompanyCode = ? AND "
                  "v.DimensionCode = 'CC' AND v.IsActive = 1 AND "
                  "EXISTS (SELECT 1 FROM fin.AccountDimRule r WHERE "
                  "r.CompanyCode = v.CompanyCode AND r.AccountCode "
                  "= ? AND r.DimensionCode = 'CC' AND "
                  "r.Requirement = 'MANDATORY') ORDER BY "
                  "v.DimValue), ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE FB-CORREL TO SQLIO-PARM-VAL(1)
           MOVE J-LINE-NO TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(2)
           MOVE WS-CO TO SQLIO-PARM-VAL(3) SQLIO-PARM-VAL(9)
           MOVE WS-ACCOUNT TO SQLIO-PARM-VAL(4) SQLIO-PARM-VAL(10)
           MOVE FB-AMT TO FB-NUM-ED
           MOVE FUNCTION TRIM(FB-NUM-ED) TO SQLIO-PARM-VAL(5)
                                            SQLIO-PARM-VAL(7)
           MOVE FB-AMT2 TO FB-NUM-ED
           MOVE FUNCTION TRIM(FB-NUM-ED) TO SQLIO-PARM-VAL(6)
                                            SQLIO-PARM-VAL(8)
           MOVE J-DESC TO SQLIO-PARM-VAL(11)
           MOVE WS-USER TO SQLIO-PARM-VAL(12)
           PERFORM VARYING FB-I FROM 1 BY 1 UNTIL FB-I > 12
               MOVE "N" TO SQLIO-PARM-NULL(FB-I)
           END-PERFORM
           MOVE 12 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       2200-EXIT.
           EXIT.

       2300-POST.
           INITIALIZE WS-JNLPOST-CA
           MOVE "POST" TO JH-FUNCTION
           MOVE WS-CO TO JH-COMPANY
           MOVE WS-USER TO JH-USER
           MOVE J-DATE TO JH-BUSINESS-DATE
           MOVE FB-CORREL TO JH-CORRELATION-ID
           MOVE "GL" TO JP-JOURNAL-TYPE
           MOVE J-DESC TO JP-DESCRIPTION
           MOVE "FIN" TO JP-SOURCE-MODULE
           MOVE FB-CORREL TO JP-SOURCE-REF
           MOVE J-DATE TO JP-DOCUMENT-DATE
           MOVE J-DATE TO JP-POSTING-DATE
           MOVE "GBP" TO JP-CURRENCY
           MOVE 1 TO JP-FX-RATE
           CALL "JNLPOST" USING WS-JNLPOST-CA
           EVALUATE TRUE
      *>       The period can close between the read above and the
      *>       post; the ledger has the last word, and its refusal
      *>       on that ground is still a skip and not a failure.
               WHEN JH-REASON-CODE = "JNP0017"
                 OR JH-REASON-CODE = "JNP0018"
                   MOVE "SKIPPED-CLOSED" TO D-OUTCOME
                   ADD 1 TO T-CLOSED
               WHEN JH-RETURN-CODE > 4
                   MOVE "REFUSED" TO D-OUTCOME
                   ADD 1 TO T-FAILED
               WHEN JP-WAS-DUPLICATE = "Y"
                   MOVE "ALREADY THERE" TO D-OUTCOME
                   ADD 1 TO T-EXISTING
               WHEN OTHER
                   MOVE "POSTED" TO D-OUTCOME
                   ADD 1 TO T-POSTED
                   ADD J-AMOUNT TO T-VALUE
           END-EVALUATE
           MOVE JP-JOURNAL-NUMBER TO D-JNL
           IF D-JNL = SPACES
               MOVE JP-NEW-JOURNAL-NO TO D-JNL
           END-IF
           IF D-JNL = SPACES
               MOVE JH-REASON-CODE TO D-JNL
           END-IF
           ADD 1 TO WS-ROWS-OUT.
       2300-EXIT.
           EXIT.

       COPY FINBATPR.
