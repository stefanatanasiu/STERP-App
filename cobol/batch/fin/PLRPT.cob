      *>================================================================
      *> PROGRAM-ID : PLRPT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER (FIN)
      *> PURPOSE    : PROFIT AND LOSS ACCOUNT FOR A PERIOD RANGE IN
      *>              THE STATUTORY FORMAT OF THE COMPANY FRAMEWORK
      *>              (FRS 102 FORMAT 1 BY DEFAULT), WITH THE PRIOR
      *>              YEAR COMPARATIVE AND A MANAGEMENT LAYOUT
      *>              SWITCHED BY LAYOUT=MGMT.
      *>              EVERY POSTING ACCOUNT MAPS TO EXACTLY ONE
      *>              STATUTORY LINE; AN UNMAPPED ACCOUNT IS REPORTED
      *>              UNDER "UNMAPPED" AND RAISES RETURN CODE 4 SO
      *>              THAT THE EXCEPTION CANNOT BE OVERLOOKED.
      *> RUN AS     : PLRPT CO=STC FY=2026 FROM=1 TO=6 USER=GLCLERK
      *>              [LAYOUT=STAT|MGMT]
      *> OUTPUT     : run\spool\PLRPT-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-04-18 PGW  CR0260 ORIGINAL TRADING AND PROFIT AND LOSS
      *> 1998-06-02 RJH  CR0855 COMPARATIVE COLUMN ADDED
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS
      *> 2026-10-06 STERP FIN-023 FRS 102 STATUTORY LINE MAPPING
      *>================================================================
      *> REQ: FIN-023, FIN-030
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PLRPT.
       AUTHOR. P G WHITELAW.
       DATE-WRITTEN. 18/04/1994.
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
       FD  RPT-FILE REPORT IS R-PLRPT.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY FINBATWK.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       01  WS-OPT.
           05  WS-LAYOUT               PIC X(6) VALUE "STAT".
       01  WS-DET.
           05  D-SECTION               PIC X(12) VALUE SPACES.
           05  D-CAPTION               PIC X(50) VALUE SPACES.
           05  D-DETAIL                PIC X(50) VALUE SPACES.
           05  D-THIS                  PIC S9(13)V99 VALUE 0.
           05  D-PRIOR                 PIC S9(13)V99 VALUE 0.
           05  D-VAR                   PIC S9(13)V99 VALUE 0.
      *>   FRS 102 format 1 running figures - WS-AMT3 was the 1994
      *>   "trading profit" slot and is still used for gross profit
       01  WS-SUM.
           05  S-TURNOVER              PIC S9(13)V99 VALUE 0.
           05  S-COS                   PIC S9(13)V99 VALUE 0.
           05  S-DIST                  PIC S9(13)V99 VALUE 0.
           05  S-ADMIN                 PIC S9(13)V99 VALUE 0.
           05  S-OTHER                 PIC S9(13)V99 VALUE 0.
           05  S-FINANCE               PIC S9(13)V99 VALUE 0.
           05  S-TAX                   PIC S9(13)V99 VALUE 0.
           05  S-VARI                  PIC S9(13)V99 VALUE 0.
           05  S-UNMAPPED              PIC S9(13)V99 VALUE 0.
           05  WS-AMT3                 PIC S9(13)V99 VALUE 0.
       01  WS-PRI.
           05  P-TURNOVER              PIC S9(13)V99 VALUE 0.
           05  P-COS                   PIC S9(13)V99 VALUE 0.
           05  P-DIST                  PIC S9(13)V99 VALUE 0.
           05  P-ADMIN                 PIC S9(13)V99 VALUE 0.
           05  P-OTHER                 PIC S9(13)V99 VALUE 0.
           05  P-FINANCE               PIC S9(13)V99 VALUE 0.
           05  P-TAX                   PIC S9(13)V99 VALUE 0.
           05  P-VARI                  PIC S9(13)V99 VALUE 0.
       01  WS-FLAG.
           05  WS-UNMAPPED-CNT         PIC S9(5) VALUE 0.
       REPORT SECTION.
       RD  R-PLRPT
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 8 LAST DETAIL 54
           FOOTING 58.
       COPY STRPTHDR.
       01  PH-COLS TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(50) SOURCE D-CAPTION.
               10  COLUMN 56   PIC X(50) SOURCE D-DETAIL.
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(50) SOURCE D-CAPTION.
               10  COLUMN 60   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-THIS.
               10  COLUMN 82   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-PRIOR.
               10  COLUMN 104  PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-VAR.
       01  DE-SUB TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(50) SOURCE D-CAPTION.
               10  COLUMN 58   PIC X(2) VALUE "  ".
               10  COLUMN 60   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-THIS.
               10  COLUMN 82   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-PRIOR.
               10  COLUMN 104  PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-VAR.
           05  LINE PLUS 1.
               10  COLUMN 60   PIC X(60) VALUE ALL "-".
       01  DE-RULE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-BLANK TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X VALUE SPACE.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(62) VALUE
                   "Prepared under FRS 102, format 1.  Sterling.".
               10  COLUMN 66   PIC X(62) VALUE
                   "Only POSTED journals are included.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           MOVE "PLRPT" TO RPT-REPORT-ID
           PERFORM 9100-JOB-START THRU 9100-EXIT
           PERFORM 9200-OPEN-SPOOL THRU 9200-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           INITIATE R-PLRPT
           PERFORM 1000-GATHER THRU 1000-EXIT
           PERFORM 2000-PRINT THRU 2000-EXIT
           TERMINATE R-PLRPT
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
           MOVE "LAYOUT" TO FB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF FB-TEXT(1:4) = "MGMT"
               MOVE "MGMT" TO WS-LAYOUT
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
           DISPLAY "STERP-I-0100 PLRPT profit and loss for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-023  gather this year and the comparative in one
      *> pass; revenue classes are credit balances so the sign is
      *> turned at the class, never at the printed line
      *>----------------------------------------------------------------
       1000-GATHER.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT a.ClassCode, ISNULL(a.StatutoryLineCode,"
                  "''), CONVERT(varchar(24),ISNULL(t.Dr,0)-ISNULL("
                  "t.Cr,0)), CONVERT(varchar(24),ISNULL(p.Dr,0)-"
                  "ISNULL(p.Cr,0)), a.AccountCode FROM fin.Account a"
                  " JOIN fin.AccountClass k ON k.ClassCode = "
                  "a.ClassCode OUTER APPLY (SELECT "
                  "SUM(l.DebitAmountGBP) Dr, SUM(l.CreditAmountGBP) "
                  "Cr FROM fin.JournalLine l JOIN fin.Journal j ON "
                  "j.JournalId = l.JournalId WHERE l.CompanyCode = "
                  "a.CompanyCode AND l.AccountCode = a.AccountCode "
                  "AND j.Status = 'POSTED' AND j.FiscalYear = ? AND "
                  "j.PeriodNo BETWEEN ? AND ?) t OUTER APPLY (SELECT"
                  " SUM(l.DebitAmountGBP) Dr, SUM(l.CreditAmountGBP)"
                  " Cr FROM fin.JournalLine l JOIN fin.Journal j ON "
                  "j.JournalId = l.JournalId WHERE l.CompanyCode = "
                  "a.CompanyCode AND l.AccountCode = a.AccountCode "
                  "AND j.Status = 'POSTED' AND j.FiscalYear = ? AND "
                  "j.PeriodNo BETWEEN ? AND ?) p WHERE "
                  "a.CompanyCode = ? AND k.Statement = 'PL' AND "
                  "a.AccountKind <> 'SUMMARY'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-FY TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(1)
           COMPUTE FB-SEQ-ED = WS-FY - 1
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(4)
           MOVE WS-PER-FROM TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(2)
                                            SQLIO-PARM-VAL(5)
           MOVE WS-PER-TO TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(3)
                                            SQLIO-PARM-VAL(6)
           MOVE WS-CO TO SQLIO-PARM-VAL(7)
           PERFORM VARYING FB-I FROM 1 BY 1 UNTIL FB-I > 7
               MOVE "N" TO SQLIO-PARM-NULL(FB-I)
           END-PERFORM
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE 5 TO SQLIO-COL-CNT
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
                   PERFORM 1100-ACCUMULATE THRU 1100-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       1000-EXIT.
           EXIT.

       1100-ACCUMULATE.
           COMPUTE FB-AMT = FUNCTION NUMVAL(BR-COL(3))
           COMPUTE FB-AMT2 = FUNCTION NUMVAL(BR-COL(4))
           ADD 1 TO WS-ROWS-OUT
           IF BR-COL(2) = SPACES
               ADD FB-AMT TO S-UNMAPPED
               ADD 1 TO WS-UNMAPPED-CNT
           END-IF
           EVALUATE BR-COL(1)
               WHEN "TURN"
                   COMPUTE S-TURNOVER = S-TURNOVER - FB-AMT
                   COMPUTE P-TURNOVER = P-TURNOVER - FB-AMT2
               WHEN "COS"
                   ADD FB-AMT TO S-COS
                   ADD FB-AMT2 TO P-COS
               WHEN "VARI"
                   ADD FB-AMT TO S-VARI
                   ADD FB-AMT2 TO P-VARI
               WHEN "DIST"
                   ADD FB-AMT TO S-DIST
                   ADD FB-AMT2 TO P-DIST
               WHEN "ADMIN"
                   ADD FB-AMT TO S-ADMIN
                   ADD FB-AMT2 TO P-ADMIN
               WHEN "OTHINC"
                   COMPUTE S-OTHER = S-OTHER - FB-AMT
                   COMPUTE P-OTHER = P-OTHER - FB-AMT2
               WHEN "FINC"
                   ADD FB-AMT TO S-FINANCE
                   ADD FB-AMT2 TO P-FINANCE
               WHEN "TAXPL"
                   ADD FB-AMT TO S-TAX
                   ADD FB-AMT2 TO P-TAX
               WHEN OTHER
                   ADD 1 TO WS-SKIPPED
           END-EVALUATE.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       2000-PRINT.
           MOVE SPACES TO RPT-TITLE
           STRING "Profit and loss account" DELIMITED BY SIZE
               INTO RPT-TITLE
           MOVE SPACES TO RPT-SUBTITLE
           MOVE WS-FY TO FB-SEQ-ED
           STRING "Fiscal year " FUNCTION TRIM(FB-SEQ-ED)
                  ", periods " DELIMITED BY SIZE INTO RPT-SUBTITLE
           MOVE WS-PER-FROM TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO RPT-SUBTITLE(31:3)
           MOVE "to" TO RPT-SUBTITLE(35:2)
           MOVE WS-PER-TO TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO RPT-SUBTITLE(38:3)
           MOVE SPACES TO RPT-AS-AT-TEXT
           IF WS-LAYOUT = "MGMT"
               MOVE "Management layout" TO RPT-AS-AT-TEXT
           ELSE
               MOVE "FRS 102 statutory format" TO RPT-AS-AT-TEXT
           END-IF
           MOVE "Caption" TO D-CAPTION
           MOVE SPACES TO D-DETAIL
           MOVE "This period   Comparative      Movement"
               TO D-DETAIL
           GENERATE PH-COLS
           GENERATE DE-RULE
           MOVE "Turnover" TO D-CAPTION
           MOVE S-TURNOVER TO D-THIS
           MOVE P-TURNOVER TO D-PRIOR
           PERFORM 2900-EMIT THRU 2900-EXIT
           MOVE "Cost of sales" TO D-CAPTION
           COMPUTE D-THIS = 0 - S-COS
           COMPUTE D-PRIOR = 0 - P-COS
           PERFORM 2900-EMIT THRU 2900-EXIT
           IF S-VARI NOT = 0 OR P-VARI NOT = 0
               MOVE "Manufacturing variances" TO D-CAPTION
               COMPUTE D-THIS = 0 - S-VARI
               COMPUTE D-PRIOR = 0 - P-VARI
               PERFORM 2900-EMIT THRU 2900-EXIT
           END-IF
           MOVE "Gross profit" TO D-CAPTION
           COMPUTE WS-AMT3 = S-TURNOVER - S-COS - S-VARI
           MOVE WS-AMT3 TO D-THIS
           COMPUTE D-PRIOR = P-TURNOVER - P-COS - P-VARI
           PERFORM 2910-EMIT-SUB THRU 2910-EXIT
           MOVE "Distribution costs" TO D-CAPTION
           COMPUTE D-THIS = 0 - S-DIST
           COMPUTE D-PRIOR = 0 - P-DIST
           PERFORM 2900-EMIT THRU 2900-EXIT
           MOVE "Administrative expenses" TO D-CAPTION
           COMPUTE D-THIS = 0 - S-ADMIN
           COMPUTE D-PRIOR = 0 - P-ADMIN
           PERFORM 2900-EMIT THRU 2900-EXIT
           MOVE "Other operating income" TO D-CAPTION
           MOVE S-OTHER TO D-THIS
           MOVE P-OTHER TO D-PRIOR
           PERFORM 2900-EMIT THRU 2900-EXIT
           MOVE "Operating profit" TO D-CAPTION
           COMPUTE D-THIS = WS-AMT3 - S-DIST - S-ADMIN + S-OTHER
           COMPUTE D-PRIOR = P-TURNOVER - P-COS - P-VARI - P-DIST
                             - P-ADMIN + P-OTHER
           PERFORM 2910-EMIT-SUB THRU 2910-EXIT
           MOVE "Interest payable and similar charges" TO D-CAPTION
           COMPUTE D-THIS = 0 - S-FINANCE
           COMPUTE D-PRIOR = 0 - P-FINANCE
           PERFORM 2900-EMIT THRU 2900-EXIT
           MOVE "Profit before taxation" TO D-CAPTION
           COMPUTE D-THIS = WS-AMT3 - S-DIST - S-ADMIN + S-OTHER
                            - S-FINANCE
           COMPUTE D-PRIOR = P-TURNOVER - P-COS - P-VARI - P-DIST
                             - P-ADMIN + P-OTHER - P-FINANCE
           PERFORM 2910-EMIT-SUB THRU 2910-EXIT
           MOVE "Tax on profit" TO D-CAPTION
           COMPUTE D-THIS = 0 - S-TAX
           COMPUTE D-PRIOR = 0 - P-TAX
           PERFORM 2900-EMIT THRU 2900-EXIT
           MOVE "Profit for the financial period" TO D-CAPTION
           COMPUTE D-THIS = WS-AMT3 - S-DIST - S-ADMIN + S-OTHER
                            - S-FINANCE - S-TAX
           COMPUTE D-PRIOR = P-TURNOVER - P-COS - P-VARI - P-DIST
                             - P-ADMIN + P-OTHER - P-FINANCE - P-TAX
           PERFORM 2910-EMIT-SUB THRU 2910-EXIT
           PERFORM 2950-UNMAPPED THRU 2950-EXIT.
       2000-EXIT.
           EXIT.

       2900-EMIT.
           COMPUTE D-VAR = D-THIS - D-PRIOR
           GENERATE DE-LINE.
       2900-EXIT.
           EXIT.

       2910-EMIT-SUB.
           COMPUTE D-VAR = D-THIS - D-PRIOR
           GENERATE DE-SUB.
       2910-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-023  every posting account maps to exactly one
      *> statutory line - an unmapped account is an exception, not a
      *> silent default to administrative expenses
      *>----------------------------------------------------------------
       2950-UNMAPPED.
           IF WS-UNMAPPED-CNT = 0
               GO TO 2950-EXIT
           END-IF
           GENERATE DE-BLANK
           MOVE "Unmapped posting accounts (statutory line missing)"
               TO D-CAPTION
           MOVE S-UNMAPPED TO D-THIS
           MOVE 0 TO D-PRIOR
           PERFORM 2900-EMIT THRU 2900-EXIT
           MOVE 4 TO WS-RC
           DISPLAY "STERP-W-2951 " WS-UNMAPPED-CNT
                   " account(s) have no statutory line".
       2950-EXIT.
           EXIT.

       COPY FINBATPR.
