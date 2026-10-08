      *>================================================================
      *> PROGRAM-ID : BSRPT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER (FIN)
      *> PURPOSE    : BALANCE SHEET AS AT A SELECTED DATE IN THE
      *>              FRS 102 STATUTORY FORMAT - FIXED ASSETS,
      *>              CURRENT ASSETS, CREDITORS WITHIN ONE YEAR, NET
      *>              CURRENT ASSETS, TOTAL ASSETS LESS CURRENT
      *>              LIABILITIES, CREDITORS AFTER ONE YEAR, NET
      *>              ASSETS AND CAPITAL AND RESERVES.
      *>              PROFIT AND LOSS ACCOUNTS OF THE CURRENT FISCAL
      *>              YEAR ARE SHOWN AS "PROFIT FOR THE FINANCIAL
      *>              YEAR" INSIDE RESERVES, SO THE STATEMENT BALANCES
      *>              BEFORE THE YEAR END TRANSFER HAS BEEN RUN.
      *> RUN AS     : BSRPT CO=STC ASAT=20261231 USER=GLCLERK
      *> OUTPUT     : run\spool\BSRPT-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-04-25 PGW  CR0261 ORIGINAL BALANCE SHEET
      *> 1997-01-14 RJH  CR0640 CREDITORS SPLIT OVER AND UNDER A YEAR
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS
      *> 2026-10-06 STERP FIN-023 FRS 102 CAPTIONS AND SIGN CONVENTION
      *>================================================================
      *> REQ: FIN-023, FIN-030, FIN-012
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BSRPT.
       AUTHOR. P G WHITELAW.
       DATE-WRITTEN. 25/04/1994.
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
       FD  RPT-FILE REPORT IS R-BSRPT.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY FINBATWK.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       01  WS-DET.
           05  D-CAPTION               PIC X(50) VALUE SPACES.
           05  D-DETAIL                PIC X(50) VALUE SPACES.
           05  D-THIS                  PIC S9(13)V99 VALUE 0.
           05  D-SUBTOT                PIC S9(13)V99 VALUE 0.
       01  WS-SUM.
           05  B-FA                    PIC S9(13)V99 VALUE 0.
           05  B-FADEP                 PIC S9(13)V99 VALUE 0.
           05  B-STOCK                 PIC S9(13)V99 VALUE 0.
           05  B-DEBT                  PIC S9(13)V99 VALUE 0.
           05  B-CASH                  PIC S9(13)V99 VALUE 0.
           05  B-CRED                  PIC S9(13)V99 VALUE 0.
           05  B-TAXBS                 PIC S9(13)V99 VALUE 0.
           05  B-CREDLT                PIC S9(13)V99 VALUE 0.
           05  B-EQ                    PIC S9(13)V99 VALUE 0.
           05  B-PL                    PIC S9(13)V99 VALUE 0.
           05  B-OTHER                 PIC S9(13)V99 VALUE 0.
       01  WS-RES.
           05  R-FIXED                 PIC S9(13)V99 VALUE 0.
           05  R-CURRENT               PIC S9(13)V99 VALUE 0.
           05  R-CREDITORS             PIC S9(13)V99 VALUE 0.
           05  R-NETCURR               PIC S9(13)V99 VALUE 0.
           05  R-TALCL                 PIC S9(13)V99 VALUE 0.
           05  R-NETASSETS             PIC S9(13)V99 VALUE 0.
           05  R-FUNDS                 PIC S9(13)V99 VALUE 0.
           05  R-DIFF                  PIC S9(13)V99 VALUE 0.
       REPORT SECTION.
       RD  R-BSRPT
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 8 LAST DETAIL 54
           FOOTING 58.
       COPY STRPTHDR.
       01  DE-HEAD TYPE IS DETAIL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(50) SOURCE D-CAPTION.
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(50) SOURCE D-CAPTION.
               10  COLUMN 70   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-THIS.
       01  DE-SUB TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(50) SOURCE D-CAPTION.
               10  COLUMN 92   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-SUBTOT.
       01  DE-TOT TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 92   PIC X(20) VALUE ALL "-".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(50) SOURCE D-CAPTION.
               10  COLUMN 92   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-SUBTOT.
           05  LINE PLUS 1.
               10  COLUMN 92   PIC X(20) VALUE ALL "=".
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(44) VALUE
                   "Net assets less total funds (must be nil) ".
               10  COLUMN 46   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE R-DIFF.
               10  COLUMN 70   PIC X(20) SOURCE RPT-STATUS-TEXT.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(64) VALUE
                   "Prepared under FRS 102.  Amounts in sterling.".
               10  COLUMN 68   PIC X(62) VALUE
                   "Liabilities are shown as negative amounts.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           MOVE "BSRPT" TO RPT-REPORT-ID
           PERFORM 9100-JOB-START THRU 9100-EXIT
           PERFORM 9200-OPEN-SPOOL THRU 9200-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           INITIATE R-BSRPT
           PERFORM 1000-GATHER THRU 1000-EXIT
           PERFORM 2000-PRINT THRU 2000-EXIT
           TERMINATE R-BSRPT
           PERFORM 9350-CLOSE-SPOOL THRU 9350-EXIT.
       0000-FINISH.
           PERFORM 9150-JOB-END THRU 9150-EXIT
           MOVE WS-RC TO RETURN-CODE
           STOP RUN.

       0100-READ-ARGS.
           PERFORM 9500-STD-ARGS THRU 9500-EXIT
           PERFORM 9600-RESOLVE-PERIOD THRU 9600-EXIT
           IF WS-FY = 0
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0101 no fiscal period covers "
                       WS-ASAT
               GO TO 0100-EXIT
           END-IF
           DISPLAY "STERP-I-0100 BSRPT balance sheet for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-023  balance sheet accounts are cumulative to the
      *> as-at date; profit and loss accounts are this year only, the
      *> earlier years having been swept to reserves by YEAREND
      *>----------------------------------------------------------------
       1000-GATHER.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT a.ClassCode, k.Statement, CONVERT("
                  "varchar(24),ISNULL(t.Dr,0)-ISNULL(t.Cr,0)) FROM "
                  "fin.Account a JOIN fin.AccountClass k ON "
                  "k.ClassCode = a.ClassCode OUTER APPLY (SELECT "
                  "SUM(l.DebitAmountGBP) Dr, SUM(l.CreditAmountGBP) "
                  "Cr FROM fin.JournalLine l JOIN fin.Journal j ON "
                  "j.JournalId = l.JournalId WHERE l.CompanyCode = "
                  "a.CompanyCode AND l.AccountCode = a.AccountCode "
                  "AND j.Status = 'POSTED' AND j.PostingDate <= ? "
                  "AND (k.Statement = 'BS' OR j.FiscalYear = ?)) t "
                  "WHERE a.CompanyCode = ? AND a.AccountKind <> "
                  "'SUMMARY'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-ASAT TO FB-DATE-ED
           MOVE FB-DATE-ED TO SQLIO-PARM-VAL(1)
           MOVE WS-FY TO FB-SEQ-ED
           MOVE FUNCTION TRIM(FB-SEQ-ED) TO SQLIO-PARM-VAL(2)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           PERFORM VARYING FB-I FROM 1 BY 1 UNTIL FB-I > 3
               MOVE "N" TO SQLIO-PARM-NULL(FB-I)
           END-PERFORM
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE 3 TO SQLIO-COL-CNT
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
           ADD 1 TO WS-ROWS-OUT
           IF BR-COL(2) = "PL"
               ADD FB-AMT TO B-PL
               GO TO 1100-EXIT
           END-IF
           EVALUATE BR-COL(1)
               WHEN "FA"     ADD FB-AMT TO B-FA
               WHEN "FADEP"  ADD FB-AMT TO B-FADEP
               WHEN "STOCK"  ADD FB-AMT TO B-STOCK
               WHEN "DEBT"   ADD FB-AMT TO B-DEBT
               WHEN "CASH"   ADD FB-AMT TO B-CASH
               WHEN "CRED"   ADD FB-AMT TO B-CRED
               WHEN "TAXBS"  ADD FB-AMT TO B-TAXBS
               WHEN "CREDLT" ADD FB-AMT TO B-CREDLT
               WHEN "EQ"     ADD FB-AMT TO B-EQ
               WHEN OTHER    ADD FB-AMT TO B-OTHER
           END-EVALUATE.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       2000-PRINT.
           MOVE "Balance sheet" TO RPT-TITLE
           MOVE FB-NUM-DATE TO FB-NUM-DATE
           MOVE WS-ASAT TO FB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "As at " WS-UK DELIMITED BY SIZE
               INTO RPT-AS-AT-TEXT
           MOVE SPACES TO RPT-SUBTITLE
           STRING "FRS 102 statutory format, sterling"
               DELIMITED BY SIZE INTO RPT-SUBTITLE
           MOVE "Fixed assets" TO D-CAPTION
           GENERATE DE-HEAD
           MOVE "Tangible assets at cost" TO D-CAPTION
           MOVE B-FA TO D-THIS
           GENERATE DE-LINE
           MOVE "Accumulated depreciation" TO D-CAPTION
           MOVE B-FADEP TO D-THIS
           GENERATE DE-LINE
           COMPUTE R-FIXED = B-FA + B-FADEP
           MOVE "Total fixed assets" TO D-CAPTION
           MOVE R-FIXED TO D-SUBTOT
           GENERATE DE-SUB
           MOVE "Current assets" TO D-CAPTION
           GENERATE DE-HEAD
           MOVE "Stocks" TO D-CAPTION
           MOVE B-STOCK TO D-THIS
           GENERATE DE-LINE
           MOVE "Debtors" TO D-CAPTION
           MOVE B-DEBT TO D-THIS
           GENERATE DE-LINE
           MOVE "Cash at bank and in hand" TO D-CAPTION
           MOVE B-CASH TO D-THIS
           GENERATE DE-LINE
           COMPUTE R-CURRENT = B-STOCK + B-DEBT + B-CASH
           MOVE "Total current assets" TO D-CAPTION
           MOVE R-CURRENT TO D-SUBTOT
           GENERATE DE-SUB
           MOVE "Creditors: amounts falling due within one year"
               TO D-CAPTION
           GENERATE DE-HEAD
           MOVE "Trade and other creditors" TO D-CAPTION
           MOVE B-CRED TO D-THIS
           GENERATE DE-LINE
           MOVE "Taxation and social security" TO D-CAPTION
           MOVE B-TAXBS TO D-THIS
           GENERATE DE-LINE
           COMPUTE R-CREDITORS = B-CRED + B-TAXBS
           MOVE "Total creditors within one year" TO D-CAPTION
           MOVE R-CREDITORS TO D-SUBTOT
           GENERATE DE-SUB
           COMPUTE R-NETCURR = R-CURRENT + R-CREDITORS
           MOVE "Net current assets" TO D-CAPTION
           MOVE R-NETCURR TO D-SUBTOT
           GENERATE DE-SUB
           COMPUTE R-TALCL = R-FIXED + R-NETCURR
           MOVE "Total assets less current liabilities" TO D-CAPTION
           MOVE R-TALCL TO D-SUBTOT
           GENERATE DE-SUB
           MOVE "Creditors: amounts falling due after one year"
               TO D-CAPTION
           MOVE B-CREDLT TO D-SUBTOT
           GENERATE DE-SUB
           COMPUTE R-NETASSETS = R-TALCL + B-CREDLT + B-OTHER
           MOVE "Net assets" TO D-CAPTION
           MOVE R-NETASSETS TO D-SUBTOT
           GENERATE DE-TOT
           PERFORM 2500-RESERVES THRU 2500-EXIT.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-012  the undistributed result of the current year is
      *> shown inside reserves until YEAREND transfers it to 3200
      *>----------------------------------------------------------------
       2500-RESERVES.
           MOVE "Capital and reserves" TO D-CAPTION
           GENERATE DE-HEAD
           MOVE "Called up share capital and reserves brought forward"
               TO D-CAPTION
           COMPUTE D-THIS = 0 - B-EQ
           GENERATE DE-LINE
           MOVE "Profit for the financial year, not yet transferred"
               TO D-CAPTION
           COMPUTE D-THIS = 0 - B-PL
           GENERATE DE-LINE
           COMPUTE R-FUNDS = 0 - B-EQ - B-PL
           MOVE "Total shareholders funds" TO D-CAPTION
           MOVE R-FUNDS TO D-SUBTOT
           GENERATE DE-TOT
           COMPUTE R-DIFF = R-NETASSETS - R-FUNDS
           IF R-DIFF > 0.004 OR R-DIFF < -0.004
               MOVE "DOES NOT BALANCE" TO RPT-STATUS-TEXT
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-2501 balance sheet does not balance"
           ELSE
               MOVE "BALANCED" TO RPT-STATUS-TEXT
           END-IF
           DISPLAY "STERP-I-2502 " WS-ROWS-OUT " account(s) read".
       2500-EXIT.
           EXIT.

       COPY FINBATPR.
