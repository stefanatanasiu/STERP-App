      *>================================================================
      *> PROGRAM-ID : CASHBOOK
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER (FIN)
      *> PURPOSE    : CASH BOOK ANALYSIS FOR ONE OR EVERY BANK ACCOUNT
      *>              OVER A DATE RANGE.  EACH RECEIPT AND PAYMENT IS
      *>              LISTED WITH ITS CONTRA ANALYSIS ACCOUNT AND THE
      *>              RUNNING BOOK BALANCE, FOLLOWED BY THE BRIDGE
      *>              FROM THE BOOK BALANCE TO THE BANK STATEMENT
      *>              BALANCE THROUGH OUTSTANDING RECEIPTS AND
      *>              PAYMENTS.  THE BRIDGE IS OBTAINED FROM THE
      *>              ONLINE BANKREC SERVICE SO THAT THE BATCH AND
      *>              THE SCREEN CAN NEVER DISAGREE.
      *> RUN AS     : CASHBOOK CO=STC FROM=20261001 TO=20261231
      *>              [BANK=GBPCUR] USER=GLCLERK
      *> OUTPUT     : run\spool\CASHBOOK-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO, BANKREC
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-06-30 PGW  CR0275 ORIGINAL CASH BOOK LISTING
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS
      *> 2026-10-06 STERP FIN-013/FIN-014 BRIDGE THROUGH BANKREC
      *>================================================================
      *> REQ: FIN-013, FIN-014, FIN-023
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CASHBOOK.
       AUTHOR. P G WHITELAW.
       DATE-WRITTEN. 30/06/1994.
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
       FD  RPT-FILE REPORT IS R-CASHBOOK.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY FINBATWK.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY BANKRECCA REPLACING ==DFHCOMMAREA== BY ==WS-BANKREC-CA==
                      LEADING ==CA-== BY ==BH-==.
       01  WS-OPT.
           05  WS-BANK                 PIC X(20) VALUE SPACES.
       01  WS-DET.
           05  D-BANK                  PIC X(20) VALUE SPACES.
           05  D-BANK-NAME             PIC X(40) VALUE SPACES.
           05  D-DATE                  PIC X(10) VALUE SPACES.
           05  D-JNL                   PIC X(20) VALUE SPACES.
           05  D-NARR                  PIC X(34) VALUE SPACES.
           05  D-CONTRA                PIC X(12) VALUE SPACES.
           05  D-CONTRA-NAME           PIC X(24) VALUE SPACES.
           05  D-RECEIPT               PIC S9(11)V99 VALUE 0.
           05  D-PAYMENT               PIC S9(11)V99 VALUE 0.
           05  D-RUNNING               PIC S9(13)V99 VALUE 0.
           05  D-RECON                 PIC X(3) VALUE SPACES.
       01  WS-BRI.
           05  G-CAPTION               PIC X(46) VALUE SPACES.
           05  G-AMOUNT                PIC S9(13)V99 VALUE 0.
       01  WS-TOT.
           05  T-RECEIPTS              PIC S9(13)V99 VALUE 0.
           05  T-PAYMENTS              PIC S9(13)V99 VALUE 0.
           05  T-OPENING               PIC S9(13)V99 VALUE 0.
           05  T-CLOSING               PIC S9(13)V99 VALUE 0.
       REPORT SECTION.
       RD  R-CASHBOOK
           CONTROLS ARE FINAL D-BANK
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 52
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-BANK TYPE IS CONTROL HEADING D-BANK.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(13) VALUE "Bank account ".
               10  COLUMN 14   PIC X(20) SOURCE D-BANK.
               10  COLUMN 36   PIC X(40) SOURCE D-BANK-NAME.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(4) VALUE "Date".
               10  COLUMN 13   PIC X(7) VALUE "Journal".
               10  COLUMN 35   PIC X(9) VALUE "Narrative".
               10  COLUMN 70   PIC X(7) VALUE "Analysis".
               10  COLUMN 96   PIC X(8) VALUE "Receipts".
               10  COLUMN 110  PIC X(8) VALUE "Payments".
               10  COLUMN 124  PIC X(7) VALUE "Balance".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-MOVE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(10) SOURCE D-DATE.
               10  COLUMN 13   PIC X(20) SOURCE D-JNL.
               10  COLUMN 35   PIC X(34) SOURCE D-NARR.
               10  COLUMN 70   PIC X(12) SOURCE D-CONTRA.
               10  COLUMN 83   PIC X(10) SOURCE D-CONTRA-NAME.
               10  COLUMN 94   PIC Z,ZZZ,ZZ9.99 SOURCE D-RECEIPT.
               10  COLUMN 108  PIC Z,ZZZ,ZZ9.99 SOURCE D-PAYMENT.
               10  COLUMN 120  PIC ZZZ,ZZZ,ZZ9.99- SOURCE D-RUNNING.
       01  DE-BRIDGE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(46) SOURCE G-CAPTION.
               10  COLUMN 60   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE G-AMOUNT.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(16) VALUE "Cash book totals".
               10  COLUMN 94   PIC Z,ZZZ,ZZ9.99 SOURCE T-RECEIPTS.
               10  COLUMN 108  PIC Z,ZZZ,ZZ9.99 SOURCE T-PAYMENTS.
               10  COLUMN 120  PIC ZZZ,ZZZ,ZZ9.99- SOURCE T-CLOSING.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(62) VALUE
                   "A bank charge is expensed, not netted off.".
               10  COLUMN 66   PIC X(62) SOURCE RPT-STATUS-TEXT.
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           MOVE "CASHBOOK" TO RPT-REPORT-ID
           PERFORM 9100-JOB-START THRU 9100-EXIT
           PERFORM 9200-OPEN-SPOOL THRU 9200-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           INITIATE R-CASHBOOK
           PERFORM 1000-PRODUCE THRU 1000-EXIT
           PERFORM 2000-BRIDGE THRU 2000-EXIT
           TERMINATE R-CASHBOOK
           PERFORM 9350-CLOSE-SPOOL THRU 9350-EXIT.
       0000-FINISH.
           PERFORM 9150-JOB-END THRU 9150-EXIT
           MOVE WS-RC TO RETURN-CODE
           STOP RUN.

       0100-READ-ARGS.
           PERFORM 9500-STD-ARGS THRU 9500-EXIT
           IF CM-FROM NOT = SPACES
               COMPUTE WS-FROM = FUNCTION NUMVAL(CM-FROM)
           END-IF
           IF CM-TO NOT = SPACES
               COMPUTE WS-TO = FUNCTION NUMVAL(CM-TO)
           END-IF
           MOVE "BANK" TO FB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           MOVE FB-TEXT(1:20) TO WS-BANK
           IF WS-TO = 0
               MOVE WS-ASAT TO WS-TO
           END-IF
           IF WS-FROM = 0
               COMPUTE WS-FROM = WS-TO - 100
           END-IF
           IF WS-FROM > WS-TO
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0101 FROM date is after TO date"
               GO TO 0100-EXIT
           END-IF
           DISPLAY "STERP-I-0100 CASHBOOK cash analysis for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-013  every bank movement carries its analysis account
      *>----------------------------------------------------------------
       1000-PRODUCE.
           MOVE "Cash book analysis" TO RPT-TITLE
           MOVE WS-FROM TO FB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE SPACES TO RPT-SUBTITLE
           STRING "Movements from " WS-UK DELIMITED BY SIZE
               INTO RPT-SUBTITLE
           MOVE WS-TO TO FB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "to " WS-UK DELIMITED BY SIZE
               INTO RPT-AS-AT-TEXT
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
                   PERFORM 1200-ONE-MOVE THRU 1200-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           DISPLAY "STERP-I-1002 " WS-ROWS-OUT " cash movement(s)".
       1000-EXIT.
           EXIT.

       1100-BUILD-SQL.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT l.BankAccountCode, ba.Name, CONVERT("
                  "char(8),j.PostingDate,112), j.JournalNumber, "
                  "ISNULL(l.Narrative,j.Description), CONVERT("
                  "varchar(24),l.DebitAmountGBP), CONVERT(varchar("
                  "24),l.CreditAmountGBP), ISNULL((SELECT TOP 1 "
                  "x.AccountCode FROM fin.JournalLine x WHERE "
                  "x.JournalId = l.JournalId AND x.JournalLineId <> "
                  "l.JournalLineId ORDER BY x.LineNum),''), "
                  "CASE WHEN l.ReconId IS NULL THEN '' ELSE 'REC' "
                  "END FROM fin.JournalLine l JOIN fin.Journal j ON "
                  "j.JournalId = l.JournalId JOIN fin.BankAccount "
                  "ba ON ba.CompanyCode = l.CompanyCode AND "
                  "ba.BankAccountCode = l.BankAccountCode WHERE "
                  "l.CompanyCode = ? AND j.Status = 'POSTED' AND "
                  "j.PostingDate BETWEEN ? AND ? AND "
                  "l.BankAccountCode IS NOT NULL AND (? = '' OR "
                  "l.BankAccountCode = ?) ORDER BY "
                  "l.BankAccountCode, j.PostingDate, l.JournalLineId"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-FROM TO FB-DATE-ED
           MOVE FB-DATE-ED TO SQLIO-PARM-VAL(2)
           MOVE WS-TO TO FB-DATE-ED
           MOVE FB-DATE-ED TO SQLIO-PARM-VAL(3)
           MOVE WS-BANK TO SQLIO-PARM-VAL(4) SQLIO-PARM-VAL(5)
           PERFORM VARYING FB-I FROM 1 BY 1 UNTIL FB-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(FB-I)
           END-PERFORM
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE 9 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT.
       1100-EXIT.
           EXIT.

       1200-ONE-MOVE.
           IF BR-COL(1) NOT = D-BANK
               MOVE 0 TO D-RUNNING
           END-IF
           MOVE BR-COL(1) TO D-BANK
           MOVE BR-COL(2) TO D-BANK-NAME
           COMPUTE FB-NUM-DATE = FUNCTION NUMVAL(BR-COL(3))
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE WS-UK TO D-DATE
           MOVE BR-COL(4) TO D-JNL
           MOVE BR-COL(5) TO D-NARR
           COMPUTE D-RECEIPT = FUNCTION NUMVAL(BR-COL(6))
           COMPUTE D-PAYMENT = FUNCTION NUMVAL(BR-COL(7))
           MOVE BR-COL(8) TO D-CONTRA
           MOVE SPACES TO D-CONTRA-NAME
           MOVE BR-COL(9) TO D-RECON
           COMPUTE D-RUNNING = D-RUNNING + D-RECEIPT - D-PAYMENT
           ADD D-RECEIPT TO T-RECEIPTS
           ADD D-PAYMENT TO T-PAYMENTS
           MOVE D-RUNNING TO T-CLOSING
           ADD 1 TO WS-ROWS-OUT
           GENERATE DE-MOVE.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: FIN-014  AC-014 - the book balance, the outstanding
      *> items and the statement balance must bridge exactly.  The
      *> figures come from the online BANKREC service, so the report
      *> and the reconciliation screen cannot drift apart.
      *>----------------------------------------------------------------
       2000-BRIDGE.
           IF WS-BANK = SPACES
               MOVE "Bridge printed only for a single bank account"
                   TO RPT-STATUS-TEXT
               GO TO 2000-EXIT
           END-IF
           INITIALIZE WS-BANKREC-CA
           MOVE "BRIDGE" TO BH-FUNCTION
           MOVE WS-CO TO BH-COMPANY
           MOVE WS-USER TO BH-USER
           MOVE WS-TO TO BH-BUSINESS-DATE
           MOVE WS-BANK TO BR-BANK-ACCOUNT
           MOVE WS-TO TO BR-AS-AT-DATE
           CALL "BANKREC" USING WS-BANKREC-CA
           IF BH-RETURN-CODE > 4
               MOVE 4 TO WS-RC
               MOVE BH-MESSAGE TO RPT-STATUS-TEXT
               DISPLAY "STERP-W-2001 " BH-MESSAGE
               GO TO 2000-EXIT
           END-IF
           MOVE "Balance per cash book (general ledger)"
               TO G-CAPTION
           MOVE BR-BOOK-BALANCE TO G-AMOUNT
           GENERATE DE-BRIDGE
           MOVE "Add: receipts not yet credited by the bank"
               TO G-CAPTION
           MOVE BR-OUT-RECEIPTS TO G-AMOUNT
           GENERATE DE-BRIDGE
           MOVE "Less: payments not yet presented" TO G-CAPTION
           COMPUTE G-AMOUNT = 0 - BR-OUT-PAYMENTS
           GENERATE DE-BRIDGE
           MOVE "Bridged balance" TO G-CAPTION
           MOVE BR-BRIDGED-BALANCE TO G-AMOUNT
           GENERATE DE-BRIDGE
           MOVE "Balance per bank statement" TO G-CAPTION
           MOVE BR-STMT-BALANCE TO G-AMOUNT
           GENERATE DE-BRIDGE
           MOVE "Unexplained difference" TO G-CAPTION
           MOVE BR-UNEXPLAINED TO G-AMOUNT
           GENERATE DE-BRIDGE
           IF BR-UNEXPLAINED > 0.004 OR BR-UNEXPLAINED < -0.004
               MOVE "UNEXPLAINED DIFFERENCE - SEE BANKREC"
                   TO RPT-STATUS-TEXT
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-2002 cash book does not bridge"
           ELSE
               MOVE "Bridge complete" TO RPT-STATUS-TEXT
           END-IF.
       2000-EXIT.
           EXIT.

       COPY FINBATPR.
