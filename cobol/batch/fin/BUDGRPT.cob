      *>================================================================
      *> PROGRAM-ID : BUDGRPT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER (FIN)
      *> PURPOSE    : BUDGET AGAINST ACTUAL FOR A PERIOD RANGE WITH AN
      *>              OPTIONAL COMPARISON VERSION (A REVISED FORECAST).
      *>              THE REPORT NAMES THE BUDGET VERSION IT USED SO
      *>              THAT TWO RUNS CAN NEVER BE CONFUSED, AND STATES
      *>              VARIANCES AS FAVOURABLE OR ADVERSE USING THE
      *>              ACCOUNT CLASS, NOT THE ARITHMETIC SIGN.
      *>              AC-012: AN APPROVED 1,000.00 BUDGET AGAINST A
      *>              1,050.00 ACTUAL IS 50.00 ADVERSE, AND AGAINST A
      *>              1,100.00 FORECAST IS 50.00 UNDERSPENT.
      *> RUN AS     : BUDGRPT CO=STC FY=2026 BUDG=OPEX26 VER=1
      *>              [CMP=FCAST26] [CMPVER=1] FROM=1 TO=12
      *> OUTPUT     : run\spool\BUDGRPT-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1995-09-11 PGW  CR0388 ORIGINAL BUDGET COMPARISON
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS
      *> 2003-02-17 KMP  CR1288 FAVOURABLE/ADVERSE BY ACCOUNT CLASS
      *>                 AFTER THE BOARD MISREAD A NEGATIVE VARIANCE
      *> 2026-10-06 STERP FIN-009 VERSIONED COMPARISON COLUMN
      *>================================================================
      *> REQ: FIN-009, FIN-023
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUDGRPT.
       AUTHOR. P G WHITELAW.
       DATE-WRITTEN. 11/09/1995.
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
       FD  RPT-FILE REPORT IS R-BUDGRPT.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY FINBATWK.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       01  WS-OPT.
           05  WS-BUDG                 PIC X(20) VALUE SPACES.
           05  WS-VER                  PIC 9(4) VALUE 1.
           05  WS-CMP                  PIC X(20) VALUE SPACES.
           05  WS-CMPVER               PIC 9(4) VALUE 1.
           05  WS-BUDG-NAME            PIC X(80) VALUE SPACES.
           05  WS-BUDG-STATUS          PIC X(10) VALUE SPACES.
       01  WS-DET.
           05  D-GROUP                 PIC X(30) VALUE SPACES.
           05  D-ACCT                  PIC X(12) VALUE SPACES.
           05  D-NAME                  PIC X(40) VALUE SPACES.
           05  D-CC                    PIC X(20) VALUE SPACES.
           05  D-BUDGET                PIC S9(13)V99 VALUE 0.
           05  D-ACTUAL                PIC S9(13)V99 VALUE 0.
           05  D-VAR                   PIC S9(13)V99 VALUE 0.
           05  D-SENSE                 PIC X(12) VALUE SPACES.
           05  D-COMPARE               PIC S9(13)V99 VALUE 0.
           05  D-CMPVAR                PIC S9(13)V99 VALUE 0.
           05  D-CLASS                 PIC X(10) VALUE SPACES.
       01  WS-TOT.
           05  T-BUDGET                PIC S9(13)V99 VALUE 0.
           05  T-ACTUAL                PIC S9(13)V99 VALUE 0.
           05  T-VAR                   PIC S9(13)V99 VALUE 0.
           05  T-COMPARE               PIC S9(13)V99 VALUE 0.
           05  T-CMPVAR                PIC S9(13)V99 VALUE 0.
           05  T-ADVERSE               PIC S9(5) VALUE 0.
       REPORT SECTION.
       RD  R-BUDGRPT
           CONTROLS ARE FINAL D-GROUP
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 52
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-GROUP TYPE IS CONTROL HEADING D-GROUP.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(30) SOURCE D-GROUP.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(7) VALUE "Account".
               10  COLUMN 14   PIC X(4) VALUE "Name".
               10  COLUMN 46   PIC X(11) VALUE "Cost centre".
               10  COLUMN 62   PIC X(6) VALUE "Budget".
               10  COLUMN 78   PIC X(6) VALUE "Actual".
               10  COLUMN 93   PIC X(8) VALUE "Variance".
               10  COLUMN 104  PIC X(5) VALUE "Sense".
               10  COLUMN 118  PIC X(10) VALUE "Comparison".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(12) SOURCE D-ACCT.
               10  COLUMN 14   PIC X(30) SOURCE D-NAME.
               10  COLUMN 45   PIC X(14) SOURCE D-CC.
               10  COLUMN 60   PIC ZZZ,ZZZ,ZZ9.99- SOURCE D-BUDGET.
               10  COLUMN 76   PIC ZZZ,ZZZ,ZZ9.99- SOURCE D-ACTUAL.
               10  COLUMN 90   PIC ZZZ,ZZ9.99-     SOURCE D-VAR.
               10  COLUMN 103  PIC X(12) SOURCE D-SENSE.
               10  COLUMN 116  PIC ZZZ,ZZZ,ZZ9.99- SOURCE D-COMPARE.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(14) VALUE "Report total  ".
               10  COLUMN 60   PIC ZZZ,ZZZ,ZZ9.99- SOURCE T-BUDGET.
               10  COLUMN 76   PIC ZZZ,ZZZ,ZZ9.99- SOURCE T-ACTUAL.
               10  COLUMN 90   PIC ZZZ,ZZ9.99-     SOURCE T-VAR.
               10  COLUMN 116  PIC ZZZ,ZZZ,ZZ9.99- SOURCE T-COMPARE.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(24) VALUE
                   "Adverse variance lines: ".
               10  COLUMN 25   PIC ZZZ,ZZ9 SOURCE T-ADVERSE.
               10  COLUMN 40   PIC X(24) VALUE
                   "Variance versus forecast".
               10  COLUMN 66   PIC ZZZ,ZZZ,ZZ9.99- SOURCE T-CMPVAR.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(66) VALUE
                   "Adverse means worse than budget for the class.".
               10  COLUMN 70   PIC X(60) SOURCE RPT-STATUS-TEXT.
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           MOVE "BUDGRPT" TO RPT-REPORT-ID
           PERFORM 9100-JOB-START THRU 9100-EXIT
           PERFORM 0200-READ-BUDGET THRU 0200-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 9200-OPEN-SPOOL THRU 9200-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           INITIATE R-BUDGRPT
           PERFORM 1000-PRODUCE THRU 1000-EXIT
           TERMINATE R-BUDGRPT
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
           MOVE "BUDG" TO FB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           MOVE FB-TEXT(1:20) TO WS-BUDG
           MOVE "VER" TO FB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF FB-TEXT NOT = SPACES
               COMPUTE WS-VER = FUNCTION NUMVAL(FB-TEXT)
           END-IF
           MOVE "CMP" TO FB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           MOVE FB-TEXT(1:20) TO WS-CMP
           MOVE "CMPVER" TO FB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF FB-TEXT NOT = SPACES
               COMPUTE WS-CMPVER = FUNCTION NUMVAL(FB-TEXT)
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
           IF WS-FY = 0
               PERFORM 9600-RESOLVE-PERIOD THRU 9600-EXIT
           END-IF
           IF WS-PER-FROM = 0
               MOVE 1 TO WS-PER-FROM
           END-IF
           IF WS-PER-TO = 0
               MOVE 12 TO WS-PER-TO
           END-IF
           IF WS-BUDG = SPACES
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0101 BUDG=<budget code> is required"
               GO TO 0100-EXIT
           END-IF
           DISPLAY "STERP-I-0100 BUDGRPT budget comparison for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-009  the report must name the version it used
      *>----------------------------------------------------------------
       0200-READ-BUDGET.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Name, Status FROM fin.Budget WHERE "
                  "CompanyCode = ? AND BudgetCode = ? AND "
                  "FiscalYear = ? AND Version = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-BUDG TO SQLIO-PARM-VAL(2)
           MOVE WS-FY TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(3)
           MOVE WS-VER TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING FB-I FROM 1 BY 1 UNTIL FB-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(FB-I)
           END-PERFORM
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF NOT SQLIO-OK
               MOVE 16 TO WS-RC
               DISPLAY "STERP-E-0201 budget version not found: "
                       FUNCTION TRIM(WS-BUDG)
               GO TO 0200-EXIT
           END-IF
           MOVE BR-COL(1) TO WS-BUDG-NAME
           MOVE BR-COL(2) TO WS-BUDG-STATUS
           MOVE SPACES TO RPT-STATUS-TEXT
           STRING "Version status " FUNCTION TRIM(WS-BUDG-STATUS)
               DELIMITED BY SIZE INTO RPT-STATUS-TEXT.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-PRODUCE.
           MOVE "Budget against actual" TO RPT-TITLE
           MOVE SPACES TO RPT-SUBTITLE
           MOVE WS-VER TO FB-SEQ-ED
           STRING FUNCTION TRIM(WS-BUDG) " v"
                  FUNCTION TRIM(FB-SEQ-ED) " "
                  FUNCTION TRIM(WS-BUDG-NAME)
               DELIMITED BY SIZE INTO RPT-SUBTITLE
           MOVE SPACES TO RPT-AS-AT-TEXT
           IF WS-CMP = SPACES
               MOVE "No comparison version" TO RPT-AS-AT-TEXT
           ELSE
               MOVE WS-CMPVER TO FB-SEQ-ED
               STRING "Compared with " FUNCTION TRIM(WS-CMP)
                      " v" FUNCTION TRIM(FB-SEQ-ED)
                   DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           END-IF
           PERFORM 1100-BUILD-SQL THRU 1100-EXIT
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
                   PERFORM 1200-ONE-LINE THRU 1200-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           DISPLAY "STERP-I-1002 " WS-ROWS-OUT " comparison line(s)".
       1000-EXIT.
           EXIT.

       1100-BUILD-SQL.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT a.AccountCode, a.Name, k.Name, "
                  "a.ClassCode, ISNULL(b.DimCostCentre,'*'), "
                  "CONVERT(varchar(24),ISNULL(SUM(b.Amount),0)), "
                  "CONVERT(varchar(24),ISNULL(MAX(t.Net),0)), "
                  "CONVERT(varchar(24),ISNULL(MAX(c.Amt),0)) FROM "
                  "fin.BudgetLine b JOIN fin.Account a ON "
                  "a.CompanyCode = b.CompanyCode AND a.AccountCode "
                  "= b.AccountCode JOIN fin.AccountClass k ON "
                  "k.ClassCode = a.ClassCode OUTER APPLY (SELECT "
                  "SUM(l.DebitAmountGBP - l.CreditAmountGBP) Net "
                  "FROM fin.JournalLine l JOIN fin.Journal j ON "
                  "j.JournalId = l.JournalId WHERE l.CompanyCode = "
                  "b.CompanyCode AND l.AccountCode = b.AccountCode "
                  "AND j.Status = 'POSTED' AND j.FiscalYear = "
                  "b.FiscalYear AND j.PeriodNo BETWEEN ? AND ?) t "
                  "OUTER APPLY (SELECT SUM(c2.Amount) Amt FROM "
                  "fin.BudgetLine c2 WHERE c2.CompanyCode = "
                  "b.CompanyCode AND c2.BudgetCode = ? AND "
                  "c2.Version = ? AND c2.FiscalYear = b.FiscalYear "
                  "AND c2.AccountCode = b.AccountCode AND "
                  "c2.PeriodNo BETWEEN ? AND ?) c WHERE "
                  "b.CompanyCode = ? AND b.BudgetCode = ? AND "
                  "b.FiscalYear = ? AND b.Version = ? AND "
                  "b.PeriodNo BETWEEN ? AND ? GROUP BY "
                  "a.AccountCode, a.Name, k.Name, a.ClassCode, "
                  "b.DimCostCentre ORDER BY k.Name, a.AccountCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-PER-FROM TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(1)
                                            SQLIO-PARM-VAL(5)
                                            SQLIO-PARM-VAL(11)
           MOVE WS-PER-TO TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(2)
                                            SQLIO-PARM-VAL(6)
                                            SQLIO-PARM-VAL(12)
           MOVE WS-CMP TO SQLIO-PARM-VAL(3)
           MOVE WS-CMPVER TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(4)
           MOVE WS-CO TO SQLIO-PARM-VAL(7)
           MOVE WS-BUDG TO SQLIO-PARM-VAL(8)
           MOVE WS-FY TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(9)
           MOVE WS-VER TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(10)
           PERFORM VARYING FB-I FROM 1 BY 1 UNTIL FB-I > 12
               MOVE "N" TO SQLIO-PARM-NULL(FB-I)
           END-PERFORM
           MOVE 12 TO SQLIO-PARM-CNT
           MOVE 8 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-009  AC-012 - an expense over budget is adverse, a
      *> revenue under budget is adverse; the sign alone is not enough
      *>----------------------------------------------------------------
       1200-ONE-LINE.
           MOVE BR-COL(1) TO D-ACCT
           MOVE BR-COL(2) TO D-NAME
           MOVE BR-COL(3) TO D-GROUP
           MOVE BR-COL(4) TO D-CLASS
           MOVE BR-COL(5) TO D-CC
           COMPUTE D-BUDGET = FUNCTION NUMVAL(BR-COL(6))
           COMPUTE D-ACTUAL = FUNCTION NUMVAL(BR-COL(7))
           COMPUTE D-COMPARE = FUNCTION NUMVAL(BR-COL(8))
           IF D-CLASS = "TURN" OR D-CLASS = "OTHINC"
      *>       revenue is held as a credit balance, so flip both sides
               COMPUTE D-ACTUAL = 0 - D-ACTUAL
               COMPUTE D-VAR = D-ACTUAL - D-BUDGET
           ELSE
               COMPUTE D-VAR = D-BUDGET - D-ACTUAL
           END-IF
           COMPUTE D-CMPVAR = D-COMPARE - D-ACTUAL
           EVALUATE TRUE
               WHEN D-VAR < -0.004
                   MOVE "ADVERSE" TO D-SENSE
                   ADD 1 TO T-ADVERSE
               WHEN D-VAR > 0.004
                   MOVE "FAVOURABLE" TO D-SENSE
               WHEN OTHER
                   MOVE "ON BUDGET" TO D-SENSE
           END-EVALUATE
           ADD D-BUDGET TO T-BUDGET
           ADD D-ACTUAL TO T-ACTUAL
           ADD D-VAR TO T-VAR
           ADD D-COMPARE TO T-COMPARE
           ADD D-CMPVAR TO T-CMPVAR
           ADD 1 TO WS-ROWS-OUT
           GENERATE DE-LINE.
       1200-EXIT.
           EXIT.

       COPY FINBATPR.
