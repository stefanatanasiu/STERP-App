      *>================================================================
      *> PROGRAM-ID : CASHFCST
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : CROSS MODULE CONTROLS AND REPORTING (XMD)
      *> PURPOSE    : DATED CASH FLOW FORECAST.  THE AVAILABLE BANK
      *>              FUNDS AT THE START OF THE WINDOW ARE TAKEN FROM
      *>              fin.BankAccount AND THEN MOVED DAY BY DAY BY THE
      *>              DATED FLOWS THAT THE LEDGERS ALREADY KNOW ABOUT:
      *>                CONTRACTUAL - ar.OpenItem DUE DATES IN AND
      *>                              ap.OpenItem DUE DATES OUT
      *>                CONFIRMED   - ap.PaymentInstruction VALUE
      *>                              DATES THAT ARE STILL IN FLIGHT
      *>                EXPECTED    - fin.VatPeriod PAYMENT DUE DATES
      *>                              AND core.StatutoryObligation
      *>                              CORPTAXPAY DUE DATES
      *>              THE THREE CATEGORIES ARE KEPT APART ON THE DAY
      *>              LINE AND IN THE TOTALS SO THAT A COMMITMENT IS
      *>              NEVER CONFUSED WITH AN ESTIMATE.  AN ap.OpenItem
      *>              THAT AN IN FLIGHT PAYMENT INSTRUCTION ALREADY
      *>              COVERS IS COUNTED ONCE ONLY - AS CONFIRMED - SO
      *>              THAT THE SAME MONEY NEVER LEAVES THE FORECAST
      *>              TWICE.  SETTLED LEDGER ITEMS AND SETTLED
      *>              INSTRUCTIONS ARE CASH THAT HAS ALREADY MOVED AND
      *>              ARE NOT FORECAST AT ALL.
      *> RUN AS     : CASHFCST CO=STC FROM=20261001 TO=20261231
      *>              [BANK=GBP1] [SCENARIO=BASE|UPSIDE|DOWNSIDE]
      *>              [USER=TREASURY] [MODE=PREVIEW]
      *> OUTPUT     : run\spool\CASHFCST-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, PARMGET, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-05-11 DJM  CR0171 ORIGINAL - TREASURY CASH FORECAST
      *> 1996-02-08 RJH  CR0472 IN FLIGHT BACS RUNS COUNTED ONCE AFTER
      *>                 THE 1995 DOUBLE COUNTING INCIDENT
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS THROUGHOUT
      *> 2026-10-06 STERP RPT-004 SQL SERVER PORT, SCENARIOS
      *>================================================================
      *> REQ: RPT-004
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CASHFCST.
       AUTHOR. D J MILLER.
       DATE-WRITTEN. 11/05/1993.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO DYNAMIC XB-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS XB-FILE-STATUS.
           SELECT SORT-FILE ASSIGN TO "CASHSORT".
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-CASHFCST.
      *>   every flow is released with its forecast date in front so
      *>   that the listing walks the window in date order whatever
      *>   order the ledgers were read in
       SD  SORT-FILE.
       01  SORT-REC.
           05  SR-DATE                 PIC 9(8).
           05  SR-SEQ                  PIC 9(1).
           05  SR-REF                  PIC X(22).
           05  SR-SRC                  PIC X(8).
           05  SR-PARTY                PIC X(20).
           05  SR-CAT                  PIC X(12).
           05  SR-DIR                  PIC X(3).
           05  SR-AMT                  PIC S9(13)V99.
           05  SR-NOTE                 PIC X(18).
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY XMDBATWK.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARMGET==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXCPRAIS==.
       01  WS-SQL                      PIC X(4000) VALUE SPACES.
      *> REQ: RPT-004  run options
       01  WS-OPT.
           05  WS-BANK                 PIC X(10) VALUE SPACES.
           05  WS-SCEN                 PIC X(10) VALUE SPACES.
               88  WS-BASE                       VALUE "BASE".
               88  WS-UPSIDE                     VALUE "UPSIDE".
               88  WS-DOWNSIDE                   VALUE "DOWNSIDE".
           05  WS-SLIP                 PIC S9(4) VALUE 0.
           05  WS-AR-SLIP              PIC S9(4) VALUE 0.
           05  WS-AP-SLIP              PIC S9(4) VALUE 0.
           05  WS-CORPTAX              PIC S9(13)V99 VALUE 0.
           05  WS-INT1                 PIC S9(9) VALUE 0.
           05  WS-INT2                 PIC S9(9) VALUE 0.
           05  WS-DUE                  PIC 9(8) VALUE 0.
           05  WS-DEFERRED             PIC S9(5) VALUE 0.
       01  WS-BAL.
           05  WS-OPEN-BAL             PIC S9(13)V99 VALUE 0.
           05  WS-RUN-BAL              PIC S9(13)V99 VALUE 0.
           05  WS-LOW-BAL              PIC S9(13)V99 VALUE 0.
           05  WS-LOW-DATE             PIC 9(8) VALUE 0.
           05  WS-PREV-DATE            PIC 9(8) VALUE 0.
           05  WS-ACCTS                PIC S9(4) VALUE 0.
       01  WS-DAY.
           05  WS-DAY-IN               PIC S9(13)V99 VALUE 0.
           05  WS-DAY-OUT              PIC S9(13)V99 VALUE 0.
           05  WS-DAY-CONTR            PIC S9(13)V99 VALUE 0.
           05  WS-DAY-CONF             PIC S9(13)V99 VALUE 0.
           05  WS-DAY-EXP              PIC S9(13)V99 VALUE 0.
       01  WS-TOT.
           05  T-CONTR-IN              PIC S9(13)V99 VALUE 0.
           05  T-CONTR-OUT             PIC S9(13)V99 VALUE 0.
           05  T-CONF-OUT              PIC S9(13)V99 VALUE 0.
           05  T-EXP-IN                PIC S9(13)V99 VALUE 0.
           05  T-EXP-OUT               PIC S9(13)V99 VALUE 0.
           05  T-IN                    PIC S9(13)V99 VALUE 0.
           05  T-OUT                   PIC S9(13)V99 VALUE 0.
           05  T-CLOSE                 PIC S9(13)V99 VALUE 0.
           05  T-AP-ONCE               PIC S9(13)V99 VALUE 0.
           05  T-SUPP-AMT              PIC S9(13)V99 VALUE 0.
           05  T-AP-ITEMS              PIC S9(5) VALUE 0.
           05  T-CONF-ITEMS            PIC S9(5) VALUE 0.
           05  T-SUPP-ITEMS            PIC S9(5) VALUE 0.
           05  T-AR-ITEMS              PIC S9(5) VALUE 0.
           05  T-EXP-ITEMS             PIC S9(5) VALUE 0.
      *>   staging area - one flow before it is released to the sort
       01  WS-FL.
           05  FL-DATE                 PIC 9(8) VALUE 0.
           05  FL-SEQ                  PIC 9(1) VALUE 0.
           05  FL-REF                  PIC X(22) VALUE SPACES.
           05  FL-SRC                  PIC X(8) VALUE SPACES.
           05  FL-PARTY                PIC X(20) VALUE SPACES.
           05  FL-CAT                  PIC X(12) VALUE SPACES.
           05  FL-DIR                  PIC X(3) VALUE SPACES.
           05  FL-AMT                  PIC S9(13)V99 VALUE 0.
           05  FL-NOTE                 PIC X(18) VALUE SPACES.
       01  WS-DET.
           05  D-DATE                  PIC X(10) VALUE SPACES.
           05  D-SRC                   PIC X(8) VALUE SPACES.
           05  D-REF                   PIC X(22) VALUE SPACES.
           05  D-PARTY                 PIC X(20) VALUE SPACES.
           05  D-CAT                   PIC X(12) VALUE SPACES.
           05  D-IN                    PIC S9(13)V99 VALUE 0.
           05  D-OUT                   PIC S9(13)V99 VALUE 0.
           05  D-NOTE                  PIC X(18) VALUE SPACES.
       01  WS-DAYLINE.
           05  Y-DATE                  PIC X(10) VALUE SPACES.
           05  Y-CONTR                 PIC S9(13)V99 VALUE 0.
           05  Y-CONF                  PIC S9(13)V99 VALUE 0.
           05  Y-EXP                   PIC S9(13)V99 VALUE 0.
           05  Y-IN                    PIC S9(13)V99 VALUE 0.
           05  Y-OUT                   PIC S9(13)V99 VALUE 0.
           05  Y-CLOSE                 PIC S9(13)V99 VALUE 0.
       01  WS-BNK.
           05  B-CODE                  PIC X(10) VALUE SPACES.
           05  B-NAME                  PIC X(30) VALUE SPACES.
           05  B-CCY                   PIC X(3) VALUE SPACES.
           05  B-STMT-DATE             PIC X(10) VALUE SPACES.
           05  B-BAL                   PIC S9(13)V99 VALUE 0.
           05  B-NOTE                  PIC X(30) VALUE SPACES.
       01  WS-SCOPE.
           05  S-FROM                  PIC X(10) VALUE SPACES.
           05  S-TO                    PIC X(10) VALUE SPACES.
           05  S-SCEN                  PIC X(10) VALUE SPACES.
           05  S-BANK                  PIC X(12) VALUE SPACES.
           05  S-PERIOD                PIC X(12) VALUE SPACES.
           05  S-SLIP                  PIC X(10) VALUE SPACES.
           05  S-LOW-DATE              PIC X(10) VALUE SPACES.
      *>   the proof line the treasury clerk reads back to the ledger:
      *>   fixed columns so that it can be checked mechanically
       01  WS-PROOF.
           05  FILLER                  PIC X(13) VALUE
               "STERP-I-0404 ".
           05  FILLER                  PIC X(6) VALUE "ITEMS=".
           05  PF-ITEMS                PIC 9(5) VALUE 0.
           05  FILLER                  PIC X(6) VALUE " CONF=".
           05  PF-CONF                 PIC 9(5) VALUE 0.
           05  FILLER                  PIC X(6) VALUE " SUPP=".
           05  PF-SUPP                 PIC 9(5) VALUE 0.
           05  FILLER                  PIC X(8) VALUE " OUTTOT=".
           05  PF-OUT                  PIC -(11)9.99.
       REPORT SECTION.
       RD  R-CASHFCST
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 8 LAST DETAIL 52
           FOOTING 58.
       COPY STRPTHDR.
      *>   the scope block is an ordinary DETAIL group: this compiler
      *>   does not emit CONTROL HEADING FINAL (see tech-debt xmd.md)
       01  DE-SCOPE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(16) VALUE "Forecast window ".
               10  COLUMN 17   PIC X(10) SOURCE S-FROM.
               10  COLUMN 28   PIC X(3) VALUE "to ".
               10  COLUMN 31   PIC X(10) SOURCE S-TO.
               10  COLUMN 45   PIC X(10) VALUE "Scenario  ".
               10  COLUMN 55   PIC X(10) SOURCE S-SCEN.
               10  COLUMN 68   PIC X(6) VALUE "Bank  ".
               10  COLUMN 74   PIC X(12) SOURCE S-BANK.
               10  COLUMN 88   PIC X(8) VALUE "Period  ".
               10  COLUMN 96   PIC X(12) SOURCE S-PERIOD.
               10  COLUMN 110  PIC X(11) VALUE "Slip days  ".
               10  COLUMN 121  PIC X(10) SOURCE S-SLIP.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(64) VALUE
                   "Basis: sterling carrying values, settled cash".
               10  COLUMN 47   PIC X(60) VALUE
                   "excluded, each obligation counted once.".
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(16) VALUE "Opening funds".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-BANK TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(10) SOURCE B-CODE.
               10  COLUMN 14   PIC X(30) SOURCE B-NAME.
               10  COLUMN 46   PIC X(3) SOURCE B-CCY.
               10  COLUMN 52   PIC X(10) SOURCE B-STMT-DATE.
               10  COLUMN 64   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE B-BAL.
               10  COLUMN 84   PIC X(30) SOURCE B-NOTE.
       01  DE-HEAD TYPE IS DETAIL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(4) VALUE "Date".
               10  COLUMN 12   PIC X(6) VALUE "Source".
               10  COLUMN 21   PIC X(9) VALUE "Reference".
               10  COLUMN 44   PIC X(12) VALUE "Counterparty".
               10  COLUMN 65   PIC X(8) VALUE "Category".
               10  COLUMN 87   PIC X(8) VALUE "Receipts".
               10  COLUMN 105  PIC X(8) VALUE "Payments".
               10  COLUMN 114  PIC X(18) VALUE "Note / balance".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-FLOW TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(10) SOURCE D-DATE.
               10  COLUMN 12   PIC X(8) SOURCE D-SRC.
               10  COLUMN 21   PIC X(22) SOURCE D-REF.
               10  COLUMN 44   PIC X(20) SOURCE D-PARTY.
               10  COLUMN 65   PIC X(12) SOURCE D-CAT.
               10  COLUMN 78   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-IN.
               10  COLUMN 96   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-OUT.
               10  COLUMN 114  PIC X(18) SOURCE D-NOTE.
      *>   the day break: the three categories stay in their own
      *>   columns and the closing balance is carried forward
       01  DE-DAY TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(10) SOURCE Y-DATE.
               10  COLUMN 12   PIC X(13) VALUE "Day movement".
               10  COLUMN 27   PIC ZZZ,ZZZ,ZZ9.99- SOURCE Y-CONTR.
               10  COLUMN 43   PIC ZZZ,ZZZ,ZZ9.99- SOURCE Y-CONF.
               10  COLUMN 59   PIC ZZZ,ZZZ,ZZ9.99- SOURCE Y-EXP.
               10  COLUMN 78   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE Y-IN.
               10  COLUMN 96   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE Y-OUT.
               10  COLUMN 114  PIC ZZZ,ZZZ,ZZ9.99- SOURCE Y-CLOSE.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(24) VALUE
                   "Contractual ledger flows".
               10  COLUMN 65   PIC X(12) VALUE "CONTRACTUAL ".
               10  COLUMN 78   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE
                   T-CONTR-IN.
               10  COLUMN 96   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE
                   T-CONTR-OUT.
               10  COLUMN 114  PIC ZZZ9 SOURCE T-AR-ITEMS.
               10  COLUMN 119  PIC X(3) VALUE " in".
               10  COLUMN 123  PIC ZZZ9 SOURCE T-AP-ITEMS.
               10  COLUMN 128  PIC X(4) VALUE " out".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(24) VALUE
                   "Confirmed instructions".
               10  COLUMN 65   PIC X(12) VALUE "CONFIRMED   ".
               10  COLUMN 96   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE
                   T-CONF-OUT.
               10  COLUMN 114  PIC ZZZ9 SOURCE T-CONF-ITEMS.
               10  COLUMN 119  PIC X(12) VALUE " in flight".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(24) VALUE
                   "Expected statutory flows".
               10  COLUMN 65   PIC X(12) VALUE "EXPECTED    ".
               10  COLUMN 78   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-EXP-IN.
               10  COLUMN 96   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-EXP-OUT.
               10  COLUMN 114  PIC ZZZ9 SOURCE T-EXP-ITEMS.
               10  COLUMN 119  PIC X(12) VALUE " estimates".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(24) VALUE "Forecast totals".
               10  COLUMN 27   PIC X(16) VALUE "opening funds".
               10  COLUMN 43   PIC ZZZ,ZZZ,ZZ9.99- SOURCE WS-OPEN-BAL.
               10  COLUMN 78   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-IN.
               10  COLUMN 96   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-OUT.
               10  COLUMN 114  PIC ZZZ,ZZZ,ZZ9.99- SOURCE T-CLOSE.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(30) VALUE
                   "Lowest projected balance".
               10  COLUMN 31   PIC X(10) SOURCE S-LOW-DATE.
               10  COLUMN 43   PIC ZZZ,ZZZ,ZZ9.99- SOURCE WS-LOW-BAL.
               10  COLUMN 65   PIC X(12) SOURCE RPT-STATUS-TEXT.
               10  COLUMN 80   PIC X(28) VALUE
                   "Flows listed".
               10  COLUMN 93   PIC ZZZ,ZZ9 SOURCE XB-ROWS-OUT.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(64) VALUE
                   "Payables already carried by an in flight payment".
               10  COLUMN 52   PIC X(42) VALUE
                   "instruction are counted once, as CONFIRMED:".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(30) VALUE
                   "open items superseded".
               10  COLUMN 34   PIC ZZZ9 SOURCE T-SUPP-ITEMS.
               10  COLUMN 40   PIC X(22) VALUE "for a ledger value of".
               10  COLUMN 63   PIC ZZZ,ZZZ,ZZ9.99- SOURCE T-SUPP-AMT.
               10  COLUMN 80   PIC X(26) VALUE
                   "payables counted once".
               10  COLUMN 107  PIC ZZZ,ZZZ,ZZ9.99- SOURCE T-AP-ONCE.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(64) VALUE
                   "Flows pushed beyond the window by the scenario".
               10  COLUMN 49   PIC ZZZ9 SOURCE WS-DEFERRED.
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF XB-RC > 4
               GO TO 0000-FINISH
           END-IF
           MOVE "CASHFCST" TO RPT-REPORT-ID
           MOVE "Cash flow forecast" TO RPT-TITLE
           MOVE "Dated receipts and payments against bank funds"
               TO RPT-SUBTITLE
           PERFORM 9100-JOB-START THRU 9100-EXIT
           PERFORM 9200-OPEN-SPOOL THRU 9200-EXIT
           IF XB-RC > 4
               GO TO 0000-FINISH
           END-IF
           INITIATE R-CASHFCST
           GENERATE DE-SCOPE
           PERFORM 1000-OPENING-FUNDS THRU 1000-EXIT
           GENERATE DE-HEAD
           SORT SORT-FILE
               ON ASCENDING KEY SR-DATE SR-SEQ SR-REF
               INPUT PROCEDURE IS 2000-COLLECT
                   THRU 2000-COLLECT-EXIT
               OUTPUT PROCEDURE IS 3000-PRINT
                   THRU 3000-PRINT-EXIT
           PERFORM 4000-PROVE THRU 4000-EXIT
           TERMINATE R-CASHFCST
           PERFORM 9350-CLOSE-SPOOL THRU 9350-EXIT.
       0000-FINISH.
           PERFORM 9150-JOB-END THRU 9150-EXIT
           MOVE XB-RC TO RETURN-CODE
           STOP RUN.

      *>----------------------------------------------------------------
      *> REQ: RPT-004  CO, BANK, FROM, TO, SCENARIO, USER, MODE.  a
      *> window that is not given is a quarter from the business date
      *>----------------------------------------------------------------
       0100-READ-ARGS.
           PERFORM 9500-STD-ARGS THRU 9500-EXIT
           IF XB-FROM = 0
               MOVE XB-ASAT TO XB-FROM
           END-IF
           IF XB-TO = 0
               MOVE XB-FROM TO XB-NUM-DATE
               MOVE 90 TO XB-J
               PERFORM 0150-ADD-DAYS THRU 0150-EXIT
               MOVE XB-NUM-DATE TO XB-TO
           END-IF
           IF XB-TO < XB-FROM
               DISPLAY "STERP-W-0401 TO before FROM - window reset"
               MOVE XB-FROM TO XB-TO
               MOVE 4 TO XB-RC
           END-IF
           IF CM-ASAT = SPACES
               MOVE XB-FROM TO XB-ASAT
           END-IF
           MOVE "BANK" TO XB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF XB-TEXT NOT = SPACES
               MOVE FUNCTION UPPER-CASE(XB-TEXT(1:10)) TO WS-BANK
           END-IF
           MOVE "SCENARIO" TO XB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF XB-TEXT NOT = SPACES
               MOVE FUNCTION UPPER-CASE(XB-TEXT(1:10)) TO WS-SCEN
           END-IF
           IF WS-SCEN = SPACES
               MOVE "BASE" TO WS-SCEN
           END-IF
           IF NOT WS-BASE AND NOT WS-UPSIDE AND NOT WS-DOWNSIDE
               DISPLAY "STERP-W-0402 unknown scenario "
                       FUNCTION TRIM(WS-SCEN) " - BASE assumed"
               MOVE "BASE" TO WS-SCEN
               MOVE 4 TO XB-RC
           END-IF
           MOVE "XMD.CASH.SLIPDAYS" TO XB-CODE
           MOVE 14 TO XB-AMT
           PERFORM 9650-GET-PARM-NUM THRU 9650-EXIT
           COMPUTE WS-SLIP = XB-AMT
           MOVE "XMD.CASH.CORPTAX" TO XB-CODE
           MOVE 0 TO XB-AMT
           PERFORM 9650-GET-PARM-NUM THRU 9650-EXIT
           MOVE XB-AMT TO WS-CORPTAX
           PERFORM 0200-SCENARIO THRU 0200-EXIT
           PERFORM 9600-RESOLVE-PERIOD THRU 9600-EXIT
           MOVE XB-FROM TO XB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO S-FROM
           MOVE XB-TO TO XB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO S-TO
           MOVE XB-FROM TO XB-DATE-ED
           MOVE XB-TO TO XB-DATE-ED2
           MOVE WS-SCEN TO S-SCEN
           IF WS-BANK = SPACES
               MOVE "ALL ACTIVE" TO S-BANK
           ELSE
               MOVE WS-BANK TO S-BANK
           END-IF
           MOVE SPACES TO S-PERIOD
           IF XB-FY = 0
               MOVE "UNDATED" TO S-PERIOD
           ELSE
               MOVE XB-FY TO XB-SEQ-ED
               STRING XB-SEQ-ED(6:4) "/" XB-PER " "
                      XB-PERIOD-STATUS(1:4)
                   DELIMITED BY SIZE INTO S-PERIOD
           END-IF
           MOVE SPACES TO S-SLIP
           MOVE WS-SLIP TO XB-INT-ED
           STRING FUNCTION TRIM(XB-INT-ED) " day(s)"
               DELIMITED BY SIZE INTO S-SLIP
           DISPLAY "STERP-I-0400 CASHFCST company "
                   FUNCTION TRIM(XB-CO) " window " XB-DATE-ED
                   " to " XB-DATE-ED2 " scenario "
                   FUNCTION TRIM(WS-SCEN).
       0100-EXIT.
           EXIT.

      *>   XB-NUM-DATE + XB-J days -> XB-NUM-DATE
       0150-ADD-DAYS.
           IF XB-NUM-DATE = 0 OR XB-J = 0
               GO TO 0150-EXIT
           END-IF
           COMPUTE WS-INT1 = FUNCTION INTEGER-OF-DATE(XB-NUM-DATE)
           COMPUTE WS-INT2 = WS-INT1 + XB-J
           COMPUTE XB-NUM-DATE = FUNCTION DATE-OF-INTEGER(WS-INT2).
       0150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: RPT-004  a scenario moves dates, never amounts: the
      *> downside collects late and pays on time, the upside collects
      *> on time and takes the terms it is given
      *>----------------------------------------------------------------
       0200-SCENARIO.
           MOVE 0 TO WS-AR-SLIP
           MOVE 0 TO WS-AP-SLIP
           EVALUATE TRUE
               WHEN WS-DOWNSIDE
                   MOVE WS-SLIP TO WS-AR-SLIP
               WHEN WS-UPSIDE
                   MOVE WS-SLIP TO WS-AP-SLIP
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: RPT-004  opening available funds, one line per bank
      *> account.  a balance in another currency is listed but is not
      *> added to a sterling forecast
      *>----------------------------------------------------------------
       1000-OPENING-FUNDS.
           MOVE 0 TO WS-OPEN-BAL
           MOVE 0 TO WS-ACCTS
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT BankAccountCode, Name, CurrencyCode, "
                  "CONVERT(varchar(24),ISNULL(StatementBalance,0))"
                  ", ISNULL(CONVERT(char(8),StatementDate,112),"
                  "'00000000'), Status FROM fin.BankAccount WHERE "
                  "CompanyCode = ? AND Status = 'ACTIVE'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           IF WS-BANK NOT = SPACES
               MOVE SPACES TO WS-SQL
               STRING FUNCTION TRIM(SQLIO-STMT)
                      " AND BankAccountCode = ?"
                   DELIMITED BY SIZE INTO WS-SQL
               MOVE WS-SQL TO SQLIO-STMT
               MOVE WS-BANK TO SQLIO-PARM-VAL(2)
               MOVE "N" TO SQLIO-PARM-NULL(2)
               MOVE 2 TO SQLIO-PARM-CNT
           END-IF
           MOVE SPACES TO WS-SQL
           STRING FUNCTION TRIM(SQLIO-STMT)
                  " ORDER BY BankAccountCode"
               DELIMITED BY SIZE INTO WS-SQL
           MOVE WS-SQL TO SQLIO-STMT
           MOVE 6 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO XB-RC
               DISPLAY "STERP-E-1001 " SQLIO-ERRMSG(1:100)
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
           IF WS-ACCTS = 0
               MOVE SPACES TO B-CODE
               MOVE "No active bank account on file" TO B-NAME
               MOVE SPACES TO B-CCY
               MOVE SPACES TO B-STMT-DATE
               MOVE 0 TO B-BAL
               MOVE "forecast starts from nil funds" TO B-NOTE
               GENERATE DE-BANK
               IF XB-RC < 4
                   MOVE 4 TO XB-RC
               END-IF
           END-IF
           MOVE WS-OPEN-BAL TO WS-RUN-BAL
           MOVE WS-OPEN-BAL TO WS-LOW-BAL
           MOVE XB-FROM TO WS-LOW-DATE.
       1000-EXIT.
           EXIT.

       1100-ONE-ACCOUNT.
           ADD 1 TO WS-ACCTS
           MOVE BR-COL(1) TO B-CODE
           MOVE BR-COL(2) TO B-NAME
           MOVE BR-COL(3) TO B-CCY
           COMPUTE B-BAL = FUNCTION NUMVAL(BR-COL(4))
           MOVE SPACES TO B-NOTE
           MOVE SPACES TO B-STMT-DATE
           MOVE BR-COL(5) TO XB-TEXT
           PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
           MOVE XB-NUM-DATE TO WS-DUE
           IF WS-DUE NOT = 0
               PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
               MOVE XB-UK TO B-STMT-DATE
           END-IF
           IF B-CCY NOT = "GBP"
               MOVE "not sterling - excluded" TO B-NOTE
           ELSE
               ADD B-BAL TO WS-OPEN-BAL
               IF WS-DUE = 0
                   MOVE "never reconciled" TO B-NOTE
               ELSE
                   IF WS-DUE < XB-FROM
                       MOVE "statement before window" TO B-NOTE
                   END-IF
               END-IF
           END-IF
           GENERATE DE-BANK.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> SORT input phase - every dated flow in the window is released
      *> with its forecast date in front
      *>----------------------------------------------------------------
       2000-COLLECT.
           MOVE 0 TO WS-DEFERRED
           PERFORM 2100-AR-DUE THRU 2100-EXIT
           PERFORM 2200-AP-DUE THRU 2200-EXIT
           PERFORM 2250-SUPERSEDED THRU 2250-EXIT
           PERFORM 2300-INSTRUCTIONS THRU 2300-EXIT
           PERFORM 2400-VAT-DUE THRU 2400-EXIT
           PERFORM 2500-STATUTORY THRU 2500-EXIT.
       2000-COLLECT-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: RPT-004  contractual receipts - open sales ledger items
      *> at their due date.  an item already overdue at the start of
      *> the window is shown on the first day, not hidden
      *>----------------------------------------------------------------
       2100-AR-DUE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(char(8),o.DueDate,112), "
                  "o.DocumentNumber, o.CustomerCode, CONVERT("
                  "varchar(24),o.ResidualAmountGBP), "
                  "o.SettlementStatus, CONVERT(char(1),"
                  "o.IsDisputed) FROM ar.OpenItem o WHERE "
                  "o.CompanyCode = ? AND o.SettlementStatus <> "
                  "'SETTLED' AND o.WrittenOff = 0 AND "
                  "o.ResidualAmountGBP <> 0 AND CONVERT(char(8),"
                  "o.DueDate,112) <= ? ORDER BY o.DueDate, "
                  "o.DocumentNumber"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE XB-DATE-ED2 TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 6 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO XB-RC
               DISPLAY "STERP-E-2101 " SQLIO-ERRMSG(1:100)
               GO TO 2100-EXIT
           END-IF
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK
                   PERFORM 2150-ONE-RECEIPT THRU 2150-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       2100-EXIT.
           EXIT.

       2150-ONE-RECEIPT.
           MOVE SPACES TO WS-FL
           MOVE 0 TO FL-AMT
           MOVE BR-COL(1) TO XB-TEXT
           PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
           MOVE WS-AR-SLIP TO XB-J
           PERFORM 2900-DATE-RULE THRU 2900-EXIT
           MOVE "AR" TO FL-SRC
           MOVE BR-COL(2) TO FL-REF
           MOVE BR-COL(3) TO FL-PARTY
           MOVE "CONTRACTUAL" TO FL-CAT
           MOVE 1 TO FL-SEQ
           MOVE "IN" TO FL-DIR
           COMPUTE FL-AMT = FUNCTION NUMVAL(BR-COL(4))
           IF BR-COL(6)(1:1) = "1"
               MOVE "disputed" TO FL-NOTE
           END-IF
           IF BR-COL(5)(1:7) = "PARTIAL"
               MOVE "part settled" TO FL-NOTE
           END-IF
           PERFORM 2950-RELEASE THRU 2950-EXIT.
       2150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: RPT-004  contractual payments - open purchase ledger
      *> items at their due date, EXCEPT those an in flight payment
      *> instruction already carries.  that exclusion is the whole of
      *> the no double counting rule on the paying side
      *>----------------------------------------------------------------
       2200-AP-DUE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(char(8),o.DueDate,112), "
                  "o.DocumentNumber, o.SupplierCode, CONVERT("
                  "varchar(24),o.ResidualAmountGBP), CONVERT("
                  "char(1),o.IsBlocked), CONVERT(char(1),"
                  "o.IsDisputed) FROM ap.OpenItem o WHERE "
                  "o.CompanyCode = ? AND o.SettlementStatus <> "
                  "'SETTLED' AND o.ResidualAmountGBP <> 0 AND "
                  "CONVERT(char(8),o.DueDate,112) <= ? AND NOT "
                  "EXISTS (SELECT 1 FROM ap.PaymentInstruction p "
                  "WHERE p.PaymentInstructionId = "
                  "o.PaymentInstructionId AND p.Status NOT IN ("
                  "'SETTLED','CANCELLED','REJECTED','RETURNED')) "
                  "ORDER BY o.DueDate, o.DocumentNumber"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE XB-DATE-ED2 TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 6 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO XB-RC
               DISPLAY "STERP-E-2201 " SQLIO-ERRMSG(1:100)
               GO TO 2200-EXIT
           END-IF
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK
                   PERFORM 2210-ONE-PAYABLE THRU 2210-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       2200-EXIT.
           EXIT.

       2210-ONE-PAYABLE.
           MOVE SPACES TO WS-FL
           MOVE 0 TO FL-AMT
           MOVE BR-COL(1) TO XB-TEXT
           PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
           MOVE WS-AP-SLIP TO XB-J
           PERFORM 2900-DATE-RULE THRU 2900-EXIT
           MOVE "AP" TO FL-SRC
           MOVE BR-COL(2) TO FL-REF
           MOVE BR-COL(3) TO FL-PARTY
           MOVE "CONTRACTUAL" TO FL-CAT
           MOVE 1 TO FL-SEQ
           MOVE "OUT" TO FL-DIR
           COMPUTE FL-AMT = FUNCTION NUMVAL(BR-COL(4))
           IF BR-COL(5)(1:1) = "1"
               MOVE "blocked" TO FL-NOTE
           END-IF
           IF BR-COL(6)(1:1) = "1"
               MOVE "disputed" TO FL-NOTE
           END-IF
           PERFORM 2950-RELEASE THRU 2950-EXIT.
       2210-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: RPT-004  the mirror of the exclusion above - how much
      *> payables value the instructions have taken over.  printed so
      *> that the reader can see the money was not lost, only moved
      *>----------------------------------------------------------------
       2250-SUPERSEDED.
           MOVE 0 TO T-SUPP-ITEMS
           MOVE 0 TO T-SUPP-AMT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),COUNT(*)), CONVERT("
                  "varchar(24),ISNULL(SUM(o.ResidualAmountGBP),0))"
                  " FROM ap.OpenItem o WHERE o.CompanyCode = ? AND "
                  "o.SettlementStatus <> 'SETTLED' AND CONVERT("
                  "char(8),o.DueDate,112) <= ? AND EXISTS (SELECT "
                  "1 FROM ap.PaymentInstruction p WHERE "
                  "p.PaymentInstructionId = o.PaymentInstructionId "
                  "AND p.Status NOT IN ('SETTLED','CANCELLED',"
                  "'REJECTED','RETURNED'))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE XB-DATE-ED2 TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               COMPUTE T-SUPP-ITEMS = FUNCTION NUMVAL(BR-COL(1))
               COMPUTE T-SUPP-AMT = FUNCTION NUMVAL(BR-COL(2))
           END-IF.
       2250-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: RPT-004  confirmed payments - payment instructions that
      *> are still in flight, at their value date.  a settled
      *> instruction is cash that has already left and is not forecast
      *>----------------------------------------------------------------
       2300-INSTRUCTIONS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(char(8),p.ValueDate,112), "
                  "p.InstructionRef, p.SupplierCode, CONVERT("
                  "varchar(24),p.AmountGBP), p.Status, p.PaymentMe"
                  "thod FROM ap.PaymentInstruction p WHERE "
                  "p.CompanyCode = ? AND p.Status NOT IN ("
                  "'SETTLED','CANCELLED','REJECTED','RETURNED') "
                  "AND p.ValueDate IS NOT NULL AND CONVERT(char(8)"
                  ",p.ValueDate,112) <= ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE XB-DATE-ED2 TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           IF WS-BANK NOT = SPACES
               MOVE SPACES TO WS-SQL
               STRING FUNCTION TRIM(SQLIO-STMT)
                      " AND p.BankAccountCode = ?"
                   DELIMITED BY SIZE INTO WS-SQL
               MOVE WS-SQL TO SQLIO-STMT
               MOVE WS-BANK TO SQLIO-PARM-VAL(3)
               MOVE "N" TO SQLIO-PARM-NULL(3)
               MOVE 3 TO SQLIO-PARM-CNT
           END-IF
           MOVE SPACES TO WS-SQL
           STRING FUNCTION TRIM(SQLIO-STMT)
                  " ORDER BY p.ValueDate, p.InstructionRef"
               DELIMITED BY SIZE INTO WS-SQL
           MOVE WS-SQL TO SQLIO-STMT
           MOVE 6 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO XB-RC
               DISPLAY "STERP-E-2301 " SQLIO-ERRMSG(1:100)
               GO TO 2300-EXIT
           END-IF
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK
                   PERFORM 2310-ONE-INSTRUCTION THRU 2310-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       2300-EXIT.
           EXIT.

       2310-ONE-INSTRUCTION.
           MOVE SPACES TO WS-FL
           MOVE 0 TO FL-AMT
           MOVE BR-COL(1) TO XB-TEXT
           PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
           MOVE 0 TO XB-J
           PERFORM 2900-DATE-RULE THRU 2900-EXIT
           MOVE "PAYINST" TO FL-SRC
           MOVE BR-COL(2) TO FL-REF
           MOVE BR-COL(3) TO FL-PARTY
           MOVE "CONFIRMED" TO FL-CAT
           MOVE 2 TO FL-SEQ
           MOVE "OUT" TO FL-DIR
           COMPUTE FL-AMT = FUNCTION NUMVAL(BR-COL(4))
           MOVE SPACES TO FL-NOTE
           STRING BR-COL(6)(1:6) " " BR-COL(5)(1:10)
               DELIMITED BY SIZE INTO FL-NOTE
           PERFORM 2950-RELEASE THRU 2950-EXIT.
       2310-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: RPT-004  expected payments - the VAT payment due date of
      *> each period in the window, valued at the latest return.  a
      *> repayment period is a receipt, not a negative payment
      *>----------------------------------------------------------------
       2400-VAT-DUE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(char(8),v.PaymentDueDate,112), "
                  "v.VatPeriodKey, CONVERT(varchar(24),ISNULL("
                  "x.Box5,0)), v.Status FROM fin.VatPeriod v "
                  "OUTER APPLY (SELECT TOP 1 r.Box5 FROM "
                  "fin.VatReturn r WHERE r.CompanyCode = "
                  "v.CompanyCode AND r.VatPeriodKey = "
                  "v.VatPeriodKey ORDER BY r.Version DESC) x "
                  "WHERE v.CompanyCode = ? AND v.PaymentDueDate "
                  "IS NOT NULL AND CONVERT(char(8),"
                  "v.PaymentDueDate,112) BETWEEN ? AND ? ORDER BY "
                  "v.PaymentDueDate"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE XB-DATE-ED2 TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE 4 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO XB-RC
               DISPLAY "STERP-E-2401 " SQLIO-ERRMSG(1:100)
               GO TO 2400-EXIT
           END-IF
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK
                   PERFORM 2410-ONE-VAT THRU 2410-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       2400-EXIT.
           EXIT.

       2410-ONE-VAT.
           MOVE SPACES TO WS-FL
           MOVE 0 TO FL-AMT
           MOVE BR-COL(1) TO XB-TEXT
           PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
           MOVE 0 TO XB-J
           PERFORM 2900-DATE-RULE THRU 2900-EXIT
           MOVE "VAT" TO FL-SRC
           MOVE SPACES TO FL-REF
           STRING "VAT " BR-COL(2)(1:12)
               DELIMITED BY SIZE INTO FL-REF
           MOVE "HMRC" TO FL-PARTY
           MOVE "EXPECTED" TO FL-CAT
           MOVE 3 TO FL-SEQ
           COMPUTE XB-AMT = FUNCTION NUMVAL(BR-COL(3))
           IF XB-AMT < 0
               MOVE "IN" TO FL-DIR
               COMPUTE FL-AMT = 0 - XB-AMT
               MOVE "repayment due" TO FL-NOTE
           ELSE
               MOVE "OUT" TO FL-DIR
               MOVE XB-AMT TO FL-AMT
               MOVE SPACES TO FL-NOTE
               STRING "return " BR-COL(4)(1:10)
                   DELIMITED BY SIZE INTO FL-NOTE
           END-IF
           IF XB-AMT = 0
               MOVE "no return valued" TO FL-NOTE
           END-IF
           PERFORM 2950-RELEASE THRU 2950-EXIT.
       2410-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: RPT-004  expected payments - the corporation tax
      *> instalment dates held on the obligations register, valued
      *> from XMD.CASH.CORPTAX because the register carries the date
      *> and the owner but never the money
      *>----------------------------------------------------------------
       2500-STATUTORY.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(char(8),s.DueDate,112), "
                  "s.ObligationCode, s.PeriodKey, s.Status, "
                  "ISNULL(s.OwnerUser,'') FROM "
                  "core.StatutoryObligation s WHERE "
                  "s.CompanyCode = ? AND s.ObligationCode = "
                  "'CORPTAXPAY' AND s.Status <> 'COMPLETE' AND "
                  "CONVERT(char(8),s.DueDate,112) BETWEEN ? AND ? "
                  "ORDER BY s.DueDate"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE XB-DATE-ED2 TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE 5 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO XB-RC
               DISPLAY "STERP-E-2501 " SQLIO-ERRMSG(1:100)
               GO TO 2500-EXIT
           END-IF
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK
                   PERFORM 2510-ONE-OBLIGATION THRU 2510-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       2500-EXIT.
           EXIT.

       2510-ONE-OBLIGATION.
           MOVE SPACES TO WS-FL
           MOVE 0 TO FL-AMT
           MOVE BR-COL(1) TO XB-TEXT
           PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
           MOVE 0 TO XB-J
           PERFORM 2900-DATE-RULE THRU 2900-EXIT
           MOVE "CORPTAX" TO FL-SRC
           MOVE SPACES TO FL-REF
           STRING BR-COL(2)(1:11) " " BR-COL(3)(1:10)
               DELIMITED BY SIZE INTO FL-REF
           MOVE "HMRC" TO FL-PARTY
           MOVE "EXPECTED" TO FL-CAT
           MOVE 3 TO FL-SEQ
           MOVE "OUT" TO FL-DIR
           MOVE WS-CORPTAX TO FL-AMT
           IF WS-CORPTAX = 0
               MOVE "no estimate held" TO FL-NOTE
           ELSE
               MOVE "estimate XMD.CASH" TO FL-NOTE
           END-IF
           PERFORM 2950-RELEASE THRU 2950-EXIT.
       2510-EXIT.
           EXIT.

      *>   XB-NUM-DATE holds the contractual date and XB-J the
      *>   scenario slippage.  a date before the window is brought
      *>   forward to the first day and marked overdue
       2900-DATE-RULE.
           IF XB-J NOT = 0
               PERFORM 0150-ADD-DAYS THRU 0150-EXIT
               MOVE "slipped" TO FL-NOTE
           END-IF
           IF XB-NUM-DATE < XB-FROM
               MOVE XB-FROM TO FL-DATE
               MOVE "overdue" TO FL-NOTE
           ELSE
               MOVE XB-NUM-DATE TO FL-DATE
           END-IF.
       2900-EXIT.
           EXIT.

       2950-RELEASE.
           ADD 1 TO XB-ROWS-IN
           IF FL-DATE > XB-TO OR FL-DATE < XB-FROM
               ADD 1 TO WS-DEFERRED
               GO TO 2950-EXIT
           END-IF
           MOVE SPACES TO SORT-REC
           MOVE FL-DATE TO SR-DATE
           MOVE FL-SEQ TO SR-SEQ
           MOVE FL-REF TO SR-REF
           MOVE FL-SRC TO SR-SRC
           MOVE FL-PARTY TO SR-PARTY
           MOVE FL-CAT TO SR-CAT
           MOVE FL-DIR TO SR-DIR
           MOVE FL-AMT TO SR-AMT
           MOVE FL-NOTE TO SR-NOTE
           RELEASE SORT-REC.
       2950-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> SORT output phase - the window is walked in date order, the
      *> day is closed off at each break and the balance carried on
      *>----------------------------------------------------------------
       3000-PRINT.
           MOVE 0 TO WS-PREV-DATE
           MOVE "N" TO XB-DONE
           PERFORM UNTIL XB-FINISHED
               RETURN SORT-FILE
                   AT END MOVE "Y" TO XB-DONE
               END-RETURN
               IF NOT XB-FINISHED
                   IF WS-PREV-DATE NOT = 0
                       IF SR-DATE NOT = WS-PREV-DATE
                           PERFORM 3200-DAY-BREAK THRU 3200-EXIT
                       END-IF
                   END-IF
                   MOVE SR-DATE TO WS-PREV-DATE
                   PERFORM 3100-ONE-FLOW THRU 3100-EXIT
               END-IF
           END-PERFORM
           IF WS-PREV-DATE NOT = 0
               PERFORM 3200-DAY-BREAK THRU 3200-EXIT
           END-IF.
       3000-PRINT-EXIT.
           EXIT.

       3100-ONE-FLOW.
           MOVE SR-DATE TO XB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO D-DATE
           MOVE SR-SRC TO D-SRC
           MOVE SR-REF TO D-REF
           MOVE SR-PARTY TO D-PARTY
           MOVE SR-CAT TO D-CAT
           MOVE SR-NOTE TO D-NOTE
           MOVE 0 TO D-IN
           MOVE 0 TO D-OUT
           IF SR-DIR = "IN"
               MOVE SR-AMT TO D-IN
               ADD SR-AMT TO WS-DAY-IN
               ADD SR-AMT TO T-IN
           ELSE
               MOVE SR-AMT TO D-OUT
               ADD SR-AMT TO WS-DAY-OUT
               ADD SR-AMT TO T-OUT
           END-IF
           EVALUATE SR-CAT
               WHEN "CONTRACTUAL"
                   IF SR-DIR = "IN"
                       ADD SR-AMT TO T-CONTR-IN
                       ADD SR-AMT TO WS-DAY-CONTR
                   ELSE
                       ADD SR-AMT TO T-CONTR-OUT
                       SUBTRACT SR-AMT FROM WS-DAY-CONTR
                   END-IF
               WHEN "CONFIRMED"
                   ADD SR-AMT TO T-CONF-OUT
                   SUBTRACT SR-AMT FROM WS-DAY-CONF
               WHEN OTHER
                   IF SR-DIR = "IN"
                       ADD SR-AMT TO T-EXP-IN
                       ADD SR-AMT TO WS-DAY-EXP
                   ELSE
                       ADD SR-AMT TO T-EXP-OUT
                       SUBTRACT SR-AMT FROM WS-DAY-EXP
                   END-IF
           END-EVALUATE
           EVALUATE SR-SRC
               WHEN "AR"      ADD 1 TO T-AR-ITEMS
               WHEN "AP"      ADD 1 TO T-AP-ITEMS
               WHEN "PAYINST" ADD 1 TO T-CONF-ITEMS
               WHEN OTHER     ADD 1 TO T-EXP-ITEMS
           END-EVALUATE
           ADD 1 TO XB-ROWS-OUT
           GENERATE DE-FLOW.
       3100-EXIT.
           EXIT.

       3200-DAY-BREAK.
           MOVE WS-PREV-DATE TO XB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO Y-DATE
           COMPUTE WS-RUN-BAL = WS-RUN-BAL + WS-DAY-IN - WS-DAY-OUT
           MOVE WS-DAY-CONTR TO Y-CONTR
           MOVE WS-DAY-CONF TO Y-CONF
           MOVE WS-DAY-EXP TO Y-EXP
           MOVE WS-DAY-IN TO Y-IN
           MOVE WS-DAY-OUT TO Y-OUT
           MOVE WS-RUN-BAL TO Y-CLOSE
           GENERATE DE-DAY
           IF WS-RUN-BAL < WS-LOW-BAL
               MOVE WS-RUN-BAL TO WS-LOW-BAL
               MOVE WS-PREV-DATE TO WS-LOW-DATE
           END-IF
           MOVE 0 TO WS-DAY-IN
           MOVE 0 TO WS-DAY-OUT
           MOVE 0 TO WS-DAY-CONTR
           MOVE 0 TO WS-DAY-CONF
           MOVE 0 TO WS-DAY-EXP.
       3200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: RPT-004  the proof: the payables side of the forecast is
      *> the contractual items that no instruction covers plus the
      *> instructions themselves - each obligation exactly once
      *>----------------------------------------------------------------
       4000-PROVE.
           MOVE WS-RUN-BAL TO T-CLOSE
           COMPUTE T-AP-ONCE = T-CONTR-OUT + T-CONF-OUT
           MOVE WS-LOW-DATE TO XB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO S-LOW-DATE
           IF WS-LOW-BAL < 0
               MOVE "SHORTFALL" TO RPT-STATUS-TEXT
           ELSE
               MOVE "FUNDED" TO RPT-STATUS-TEXT
           END-IF
           MOVE T-AP-ITEMS TO PF-ITEMS
           MOVE T-CONF-ITEMS TO PF-CONF
           MOVE T-SUPP-ITEMS TO PF-SUPP
           MOVE T-AP-ONCE TO PF-OUT
           DISPLAY "STERP-I-0403 " XB-ROWS-OUT " flow(s) listed, "
                   XB-ROWS-IN " read, " WS-DEFERRED
                   " outside the window"
           DISPLAY WS-PROOF
           IF WS-LOW-BAL < 0
               DISPLAY "STERP-W-0405 projected shortfall on "
                       WS-LOW-DATE
           END-IF.
       4000-EXIT.
           EXIT.

       COPY XMDBATPR.
