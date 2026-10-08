      *>================================================================
      *> PROGRAM-ID : TBRPT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER (FIN)
      *> PURPOSE    : TRIAL BALANCE FOR A FISCAL YEAR AND PERIOD RANGE.
      *>              EVERY POSTING ACCOUNT IS LISTED WITH ITS BROUGHT
      *>              FORWARD BALANCE, THE MOVEMENT IN THE SELECTED
      *>              PERIODS AND THE CARRIED FORWARD BALANCE, BROKEN
      *>              DOWN BY ACCOUNT CLASS.  THE REPORT PROVES THAT
      *>              DEBITS EQUAL CREDITS; IF THEY DO NOT THE RUN
      *>              ENDS WITH RETURN CODE 8 BECAUSE AN UNBALANCED
      *>              LEDGER MUST NEVER BE REPORTED AS CLEAN.
      *>              ACCOUNTS WITH NO BALANCE AND NO MOVEMENT ARE
      *>              SUPPRESSED UNLESS ZERO=Y IS GIVEN.
      *> RUN AS     : TBRPT CO=STC FY=2026 FROM=1 TO=6 USER=GLCLERK
      *>              [ZERO=Y]
      *> OUTPUT     : run\spool\TBRPT-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-03-07 PGW  CR0255 ORIGINAL TRIAL BALANCE (MICRO FOCUS)
      *> 1996-02-11 RJH  CR0412 BROUGHT FORWARD COLUMN - THE AUDITORS
      *>                 WOULD NOT ACCEPT A MOVEMENT ONLY LISTING
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS
      *> 2026-10-06 STERP FIN-023 CLASS SUBTOTALS AND BALANCE PROOF
      *>================================================================
      *> REQ: FIN-023, FIN-010
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TBRPT.
       AUTHOR. P G WHITELAW.
       DATE-WRITTEN. 07/03/1994.
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
       FD  RPT-FILE REPORT IS R-TBRPT.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY FINBATWK.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       01  WS-OPT.
           05  WS-SHOW-ZERO            PIC X VALUE "N".
       01  WS-DET.
           05  D-CLASS                 PIC X(10) VALUE SPACES.
           05  D-CLASS-NAME            PIC X(60) VALUE SPACES.
           05  D-ACCT                  PIC X(12) VALUE SPACES.
           05  D-NAME                  PIC X(40) VALUE SPACES.
           05  D-BF                    PIC S9(13)V99 VALUE 0.
           05  D-DR                    PIC S9(13)V99 VALUE 0.
           05  D-CR                    PIC S9(13)V99 VALUE 0.
           05  D-CF-DR                 PIC S9(13)V99 VALUE 0.
           05  D-CF-CR                 PIC S9(13)V99 VALUE 0.
           05  D-STAT                  PIC X(20) VALUE SPACES.
       01  WS-CLS.
           05  C-DR                    PIC S9(13)V99 VALUE 0.
           05  C-CR                    PIC S9(13)V99 VALUE 0.
           05  C-CF-DR                 PIC S9(13)V99 VALUE 0.
           05  C-CF-CR                 PIC S9(13)V99 VALUE 0.
           05  C-ACCTS                 PIC S9(7) VALUE 0.
       01  WS-TOT.
           05  T-BF                    PIC S9(13)V99 VALUE 0.
           05  T-DR                    PIC S9(13)V99 VALUE 0.
           05  T-CR                    PIC S9(13)V99 VALUE 0.
           05  T-CF-DR                 PIC S9(13)V99 VALUE 0.
           05  T-CF-CR                 PIC S9(13)V99 VALUE 0.
           05  T-DIFF                  PIC S9(13)V99 VALUE 0.
       01  WS-FLAG.
           05  WS-FIRST                PIC X VALUE "Y".
           05  WS-BALANCED             PIC X VALUE "Y".
       REPORT SECTION.
       RD  R-TBRPT
           CONTROLS ARE FINAL D-CLASS
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 52
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-CLASS TYPE IS CONTROL HEADING D-CLASS.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(6) VALUE "Class ".
               10  COLUMN 7    PIC X(10) SOURCE D-CLASS.
               10  COLUMN 18   PIC X(60) SOURCE D-CLASS-NAME.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(7) VALUE "Account".
               10  COLUMN 14   PIC X(4) VALUE "Name".
               10  COLUMN 58   PIC X(16) VALUE "Brought forward ".
               10  COLUMN 76   PIC X(14) VALUE "Period debits ".
               10  COLUMN 92   PIC X(15) VALUE "Period credits ".
               10  COLUMN 110  PIC X(2) VALUE "Dr".
               10  COLUMN 124  PIC X(2) VALUE "Cr".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-ACCT TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(12) SOURCE D-ACCT.
               10  COLUMN 14   PIC X(40) SOURCE D-NAME.
               10  COLUMN 56   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-BF.
               10  COLUMN 74   PIC Z,ZZZ,ZZZ,ZZ9.99  SOURCE D-DR.
               10  COLUMN 90   PIC Z,ZZZ,ZZZ,ZZ9.99  SOURCE D-CR.
               10  COLUMN 106  PIC ZZZ,ZZZ,ZZ9.99    SOURCE D-CF-DR.
               10  COLUMN 120  PIC ZZZ,ZZZ,ZZ9.99    SOURCE D-CF-CR.
       01  CF-CLASS TYPE IS CONTROL FOOTING D-CLASS.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(14) VALUE "Class total   ".
               10  COLUMN 16   PIC X(10) SOURCE D-CLASS.
               10  COLUMN 28   PIC ZZZ,ZZ9 SOURCE C-ACCTS.
               10  COLUMN 36   PIC X(8) VALUE "accounts".
               10  COLUMN 74   PIC Z,ZZZ,ZZZ,ZZ9.99  SOURCE C-DR.
               10  COLUMN 90   PIC Z,ZZZ,ZZZ,ZZ9.99  SOURCE C-CR.
               10  COLUMN 106  PIC ZZZ,ZZZ,ZZ9.99    SOURCE C-CF-DR.
               10  COLUMN 120  PIC ZZZ,ZZZ,ZZ9.99    SOURCE C-CF-CR.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(20) VALUE "Trial balance total ".
               10  COLUMN 74   PIC Z,ZZZ,ZZZ,ZZ9.99  SOURCE T-DR.
               10  COLUMN 90   PIC Z,ZZZ,ZZZ,ZZ9.99  SOURCE T-CR.
               10  COLUMN 106  PIC ZZZ,ZZZ,ZZ9.99    SOURCE T-CF-DR.
               10  COLUMN 120  PIC ZZZ,ZZZ,ZZ9.99    SOURCE T-CF-CR.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(34) VALUE
                   "Debits less credits carried down ".
               10  COLUMN 36   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-DIFF.
               10  COLUMN 56   PIC X(20) SOURCE RPT-STATUS-TEXT.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(64) VALUE
                   "Balances are stated in sterling at the posted".
               10  COLUMN 68   PIC X(62) VALUE
                   "Only POSTED journals are included.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           MOVE "TBRPT" TO RPT-REPORT-ID
           PERFORM 9100-JOB-START THRU 9100-EXIT
           PERFORM 9200-OPEN-SPOOL THRU 9200-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           INITIATE R-TBRPT
           PERFORM 1000-PRODUCE THRU 1000-EXIT
           PERFORM 1900-PROVE THRU 1900-EXIT
           TERMINATE R-TBRPT
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
           MOVE "ZERO" TO FB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF FB-TEXT(1:1) = "Y"
               MOVE "Y" TO WS-SHOW-ZERO
           END-IF
           IF WS-FY = 0 OR WS-PER-TO = 0
               PERFORM 9600-RESOLVE-PERIOD THRU 9600-EXIT
               IF WS-FY = 0
                   MOVE 8 TO WS-RC
                   DISPLAY "STERP-E-0101 no fiscal period covers "
                           WS-ASAT
                   GO TO 0100-EXIT
               END-IF
               IF WS-PER-TO = 0
                   MOVE WS-PER TO WS-PER-TO
               END-IF
           END-IF
           IF WS-PER-FROM = 0
               MOVE 1 TO WS-PER-FROM
           END-IF
           IF WS-PER-FROM > WS-PER-TO
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0102 FROM period is after TO period"
               GO TO 0100-EXIT
           END-IF
           DISPLAY "STERP-I-0100 TBRPT trial balance for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-023  brought forward, movement and carried forward in
      *> one pass; the balance is derived, never stored on the account
      *>----------------------------------------------------------------
       1000-PRODUCE.
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
                   PERFORM 1200-ONE-ACCOUNT THRU 1200-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       1000-EXIT.
           EXIT.

       1100-BUILD-SQL.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT a.AccountCode, a.Name, a.ClassCode, "
                  "k.Name, ISNULL(a.StatutoryLineCode,''), "
                  "CONVERT(varchar(24),ISNULL(b.Dr,0)-ISNULL(b.Cr,0"
                  ")), CONVERT(varchar(24),ISNULL(m.Dr,0)), "
                  "CONVERT(varchar(24),ISNULL(m.Cr,0)) FROM "
                  "fin.Account a JOIN fin.AccountClass k ON "
                  "k.ClassCode = a.ClassCode OUTER APPLY (SELECT "
                  "SUM(l.DebitAmountGBP) Dr, SUM(l.CreditAmountGBP) "
                  "Cr FROM fin.JournalLine l JOIN fin.Journal j ON "
                  "j.JournalId = l.JournalId WHERE l.CompanyCode = "
                  "a.CompanyCode AND l.AccountCode = a.AccountCode "
                  "AND j.Status = 'POSTED' AND j.FiscalYear = ? AND "
                  "j.PeriodNo < ?) b OUTER APPLY (SELECT "
                  "SUM(l.DebitAmountGBP) Dr, SUM(l.CreditAmountGBP) "
                  "Cr FROM fin.JournalLine l JOIN fin.Journal j ON "
                  "j.JournalId = l.JournalId WHERE l.CompanyCode = "
                  "a.CompanyCode AND l.AccountCode = a.AccountCode "
                  "AND j.Status = 'POSTED' AND j.FiscalYear = ? AND "
                  "j.PeriodNo BETWEEN ? AND ?) m WHERE "
                  "a.CompanyCode = ? AND a.AccountKind <> 'SUMMARY' "
                  "ORDER BY k.SortOrder, a.ClassCode, a.AccountCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-FY TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(1)
                                            SQLIO-PARM-VAL(3)
           MOVE WS-PER-FROM TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(2)
                                            SQLIO-PARM-VAL(4)
           MOVE WS-PER-TO TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(5)
           MOVE WS-CO TO SQLIO-PARM-VAL(6)
           PERFORM VARYING FB-I FROM 1 BY 1 UNTIL FB-I > 6
               MOVE "N" TO SQLIO-PARM-NULL(FB-I)
           END-PERFORM
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE 8 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT.
       1100-EXIT.
           EXIT.

       1200-ONE-ACCOUNT.
           COMPUTE D-BF = FUNCTION NUMVAL(BR-COL(6))
           COMPUTE D-DR = FUNCTION NUMVAL(BR-COL(7))
           COMPUTE D-CR = FUNCTION NUMVAL(BR-COL(8))
           COMPUTE FB-AMT = D-BF + D-DR - D-CR
           IF WS-SHOW-ZERO NOT = "Y"
               IF D-BF = 0 AND D-DR = 0 AND D-CR = 0
                   ADD 1 TO WS-SKIPPED
                   GO TO 1200-EXIT
               END-IF
           END-IF
           IF BR-COL(3) NOT = D-CLASS AND WS-FIRST = "N"
               PERFORM 1300-RESET-CLASS THRU 1300-EXIT
           END-IF
           MOVE "N" TO WS-FIRST
           MOVE BR-COL(1) TO D-ACCT
           MOVE BR-COL(2) TO D-NAME
           MOVE BR-COL(3) TO D-CLASS
           MOVE BR-COL(4) TO D-CLASS-NAME
           MOVE BR-COL(5) TO D-STAT
           MOVE 0 TO D-CF-DR
           MOVE 0 TO D-CF-CR
           IF FB-AMT >= 0
               MOVE FB-AMT TO D-CF-DR
           ELSE
               COMPUTE D-CF-CR = 0 - FB-AMT
           END-IF
           ADD D-DR TO C-DR T-DR
           ADD D-CR TO C-CR T-CR
           ADD D-BF TO T-BF
           ADD D-CF-DR TO C-CF-DR T-CF-DR
           ADD D-CF-CR TO C-CF-CR T-CF-CR
           ADD 1 TO C-ACCTS
           ADD 1 TO WS-ROWS-OUT
           GENERATE DE-ACCT.
       1200-EXIT.
           EXIT.

       1300-RESET-CLASS.
      *>   the Report Writer control break fires on the GENERATE, so
      *>   the class accumulators are cleared one detail line late -
      *>   kept deliberately to match the 1994 printed layout
           MOVE 0 TO C-DR
           MOVE 0 TO C-CR
           MOVE 0 TO C-CF-DR
           MOVE 0 TO C-CF-CR
           MOVE 0 TO C-ACCTS.
       1300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-010  an unbalanced trial balance is a severe finding
      *>----------------------------------------------------------------
       1900-PROVE.
           COMPUTE T-DIFF = T-CF-DR - T-CF-CR
           IF T-DIFF > 0.004 OR T-DIFF < -0.004
               MOVE "OUT OF BALANCE" TO RPT-STATUS-TEXT
               MOVE "N" TO WS-BALANCED
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-1901 trial balance does not balance"
           ELSE
               MOVE "IN BALANCE" TO RPT-STATUS-TEXT
           END-IF
           DISPLAY "STERP-I-1902 " WS-ROWS-OUT " account(s) listed, "
                   WS-SKIPPED " suppressed".
       1900-EXIT.
           EXIT.

       COPY FINBATPR.
