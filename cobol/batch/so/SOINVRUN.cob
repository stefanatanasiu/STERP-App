      *>================================================================
      *> PROGRAM-ID : SOINVRUN
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT
      *> PURPOSE    : THE SALES INVOICING RUN.  SELECTS ELIGIBLE
      *>              UNINVOICED DESPATCH AND SERVICE EVIDENCE (A
      *>              CONFIRMED so.Despatch LINE WHERE BilledQty IS
      *>              STILL LESS THAN DespatchedQty), GROUPS IT INTO
      *>              ONE INVOICE PER CUSTOMER, CURRENCY AND DELIVERY
      *>              TERMS - OR PER ORDER WHEN THE PARAMETER
      *>              SO.INVOICE.PERORDER IS "Y" - AND POSTS IT BY
      *>              CALLING ARINVPST.  THE AGREED PRICE, VAT CODE
      *>              AND TERMS CAPTURED ON THE ORDER LINE ARE USED AS
      *>              THEY STAND: NOTHING IS RECALCULATED FROM CURRENT
      *>              MASTER DATA.  THE ORDER DEPOSIT IS APPLIED AS A
      *>              SETTLEMENT, NOT AS NEW REVENUE.  BilledQty IS
      *>              WRITTEN BACK INSIDE THE SAME BEGN..COMT, SO A
      *>              PARTIAL BILL CANNOT REBILL AN EARLIER QUANTITY
      *>              NOR REPEAT ANY STOCK OR COGS EFFECT.
      *> RUN AS     : SOINVRUN CO=STC [SITE=RDG] [CUST=C0001]
      *>              [FROM=yyyymmdd] [TO=yyyymmdd] USER=ARCLERK
      *>              [MODE=PREVIEW|COMMIT]
      *> OUTPUT     : run\spool\SOINVRUN-<CO>-<stamp>.lst  (132 col)
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, PARMGET, PERIODCK,
      *>              RNDALLOC, ARINVPST, AUDITLOG, SQLIO
      *> NOTE       : so.DespatchLine and so.SalesOrderLine hold no
      *>              invoice number column of their own.  The invoice
      *>              reference is held the other way about, by
      *>              ar.SalesInvoice (DespatchRef, SalesOrderNumber)
      *>              and ar.SalesInvoiceLine (SalesOrderLine,
      *>              DespatchLine) which ARINVPST writes, and this run
      *>              additionally records it against every despatch
      *>              line through AUDITLOG inside the same unit of
      *>              work so that the link is attributable.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-04-11 DLS  CR0301 ORIGINAL - DAILY SALES INVOICING RUN
      *> 1996-09-02 RJH  CR0455 ONE INVOICE PER CUSTOMER AND TERMS
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS THROUGHOUT
      *> 2004-06-18 DLS  CR1709 DEPOSITS APPLIED AS A SETTLEMENT AFTER
      *>                 THE DEFERRED INCOME AUDIT POINT
      *> 2021-01-04 DLS  CR3004 BREXIT - EXPORT EVIDENCE MUST BE HELD
      *>                 BEFORE A ZERO RATED EXPORT IS INVOICED
      *> 2026-10-06 STERP SO-014, SO-019, SO-025 PARTIAL BILLING
      *>================================================================
      *> REQ: SO-014, SO-019, SO-025
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOINVRUN.
       AUTHOR. D L SPENCER.
       DATE-WRITTEN. 11/04/1994.
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
       FD  RPT-FILE REPORT IS R-SOINVRN.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARM==.
       COPY PERIODCKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERIOD==.
       COPY RNDALLOCCA REPLACING ==DFHCOMMAREA== BY ==WS-ROUND==.
       COPY ARINVPSTCA REPLACING ==DFHCOMMAREA== BY ==WS-ARINV==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUDIT==.
      *> one wide fetch buffer, house style of the 1994 sales suite
       01  SO-ROW.
           05  SO-COL OCCURS 40 TIMES PIC X(100).
      *>   CR2610.  An invoice is posted inside a transaction, and a
      *>   transaction cannot be started while the selection cursor is
      *>   still being fetched (STERP-E-1404).  The selection is
      *>   therefore read to the end and held here first; the groups
      *>   are then flushed with no cursor open.
       01  WS-SEL-TABLE.
           05  WS-SEL-CNT              PIC S9(9) VALUE 0.
           05  WS-SEL-MAX              PIC S9(9) VALUE 400.
           05  WS-SEL-ROW OCCURS 400 TIMES PIC X(4000).
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-SITE                 PIC X(10) VALUE SPACES.
           05  WS-CUST                 PIC X(20) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-FROM                 PIC X(8) VALUE SPACES.
           05  WS-TO                   PIC X(8) VALUE SPACES.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
               88  WS-PREVIEW                    VALUE "PREVIEW".
           05  WS-PERORDER             PIC X VALUE "N".
       01  WS-SCRATCH.
           05  WS-IX                   PIC S9(4) VALUE 0.
           05  WS-JX                   PIC S9(4) VALUE 0.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-TODAY-N              PIC 9(8) VALUE 0.
           05  WS-DATE-N               PIC 9(8) VALUE 0.
           05  WS-DATE-R REDEFINES WS-DATE-N.
               10  WS-DATE-YYYY        PIC 9(4).
               10  WS-DATE-MM          PIC 9(2).
               10  WS-DATE-DD          PIC 9(2).
           05  WS-DONE                 PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-SKIP-LINE            PIC X VALUE "N".
           05  WS-REASON               PIC X(23) VALUE SPACES.
           05  WS-QTY                  PIC S9(11)V9(6) VALUE 0.
           05  WS-AMT                  PIC S9(13)V9(6) VALUE 0.
           05  WS-AMT2                 PIC S9(13)V9(6) VALUE 0.
           05  WS-DEP-USED             PIC S9(11)V99 VALUE 0.
           05  WS-DEP-APPLY            PIC S9(11)V99 VALUE 0.
           05  WS-CORREL               PIC X(36) VALUE SPACES.
           05  WS-INV-SEQ              PIC 9(4) VALUE 0.
           05  WS-KEY-ID               PIC 9(12) VALUE 0.
           05  WS-JOB-RUN-ID           PIC S9(9) COMP-5 VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-INT-ED               PIC -(9)9.
           05  WS-MONEY-ED             PIC -(11)9.99.
           05  WS-QTY-ED               PIC -(9)9.9(6).
           05  WS-RATE-ED              PIC -(9)9.9(8).
      *> group (invoice) header, captured from the first line of the
      *> group and never recalculated afterwards - SO-014
       01  WS-GRP.
           05  G-KEY                   PIC X(96) VALUE SPACES.
           05  G-PREV-KEY              PIC X(96) VALUE SPACES.
           05  WS-SEL-IX               PIC S9(9) VALUE 0.
           05  G-CUST                  PIC X(20) VALUE SPACES.
           05  G-INV-CUST              PIC X(20) VALUE SPACES.
           05  G-CURR                  PIC X(3) VALUE SPACES.
           05  G-DELTERM               PIC X(3) VALUE SPACES.
           05  G-ORDER                 PIC X(30) VALUE SPACES.
           05  G-DESP                  PIC X(30) VALUE SPACES.
           05  G-TERMS                 PIC X(12) VALUE SPACES.
           05  G-TERMS-SNAP            PIC X(100) VALUE SPACES.
           05  G-SALESP                PIC X(20) VALUE SPACES.
           05  G-CUSTREF               PIC X(40) VALUE SPACES.
           05  G-CUSTVAT               PIC X(15) VALUE SPACES.
           05  G-VATLOC                PIC X(3) VALUE SPACES.
           05  G-BILLADDR              PIC X(300) VALUE SPACES.
           05  G-DELADDR               PIC X(300) VALUE SPACES.
           05  G-FX                    PIC S9(9)V9(8) VALUE 0.
           05  G-POSTDATE              PIC 9(8) VALUE 0.
           05  G-DOCDATE               PIC 9(8) VALUE 0.
           05  G-DEPOSIT               PIC S9(11)V99 VALUE 0.
      *> the invoice under construction.  ARINVPST takes 25 lines, so
      *> a twenty sixth line simply starts another invoice - CR0455.
       01  WS-BUF.
           05  WS-BUF-CNT              PIC S9(4) VALUE 0.
           05  WS-BUF-LINE OCCURS 25 TIMES.
               10  BL-DESP-LINE-ID     PIC 9(12).
               10  BL-SO-LINE-ID       PIC 9(12).
               10  BL-ORDER            PIC X(30).
               10  BL-ORDER-LINE       PIC 9(5).
               10  BL-DESP             PIC X(30).
               10  BL-ITEM             PIC X(40).
               10  BL-DESC             PIC X(60).
               10  BL-QTY              PIC S9(11)V9(6).
               10  BL-UOM              PIC X(10).
               10  BL-PRICE            PIC S9(11)V9(6).
               10  BL-PRICE-PER        PIC S9(11)V9(6).
               10  BL-DISC1            PIC S9(3)V9(4).
               10  BL-DISC2            PIC S9(3)V9(4).
               10  BL-NET              PIC S9(11)V99.
               10  BL-VATC             PIC X(4).
               10  BL-VATR             PIC S9(3)V9(4).
               10  BL-VAT              PIC S9(11)V99.
               10  BL-CC               PIC X(20).
               10  BL-PROJ             PIC X(20).
      *> withheld evidence, listed with its reason at the foot of the
      *> report so that nothing simply disappears from the run
       01  WS-EXC-TAB.
           05  WS-EXC-CNT              PIC S9(4) VALUE 0.
           05  WS-EXC OCCURS 300 TIMES.
               10  EX-ORDER            PIC X(14).
               10  EX-LINE             PIC 9(4).
               10  EX-DESP             PIC X(12).
               10  EX-ITEM             PIC X(16).
               10  EX-QTY              PIC S9(11)V9(3).
               10  EX-REASON           PIC X(23).
       01  WS-DET.
           05  D-GROUP                 PIC X(14) VALUE SPACES.
           05  D-HEAD-CUST             PIC X(20) VALUE SPACES.
           05  D-HEAD-CURR             PIC X(3) VALUE SPACES.
           05  D-HEAD-TERM             PIC X(3) VALUE SPACES.
           05  D-HEAD-INV              PIC X(22) VALUE SPACES.
           05  D-ORDER                 PIC X(14) VALUE SPACES.
           05  D-LINE                  PIC 9(4) VALUE 0.
           05  D-DESP                  PIC X(12) VALUE SPACES.
           05  D-ITEM                  PIC X(16) VALUE SPACES.
           05  D-QTY-DESP              PIC S9(11)V9(3) VALUE 0.
           05  D-QTY-BILL              PIC S9(11)V9(3) VALUE 0.
           05  D-QTY-HELD              PIC S9(11)V9(3) VALUE 0.
           05  D-NET                   PIC S9(11)V99 VALUE 0.
           05  D-VATC                  PIC X(4) VALUE SPACES.
           05  D-VAT                   PIC S9(11)V99 VALUE 0.
           05  D-REASON                PIC X(23) VALUE SPACES.
       01  WS-TOT.
           05  T-INVOICES              PIC S9(7) VALUE 0.
           05  T-LINES                 PIC S9(7) VALUE 0.
           05  T-QTY-BILL              PIC S9(13)V9(3) VALUE 0.
           05  T-QTY-HELD              PIC S9(13)V9(3) VALUE 0.
           05  T-NET                   PIC S9(13)V99 VALUE 0.
           05  T-VAT                   PIC S9(13)V99 VALUE 0.
           05  T-DEPOSIT               PIC S9(13)V99 VALUE 0.
           05  T-X-CREDIT              PIC S9(5) VALUE 0.
           05  T-X-VAT                 PIC S9(5) VALUE 0.
           05  T-X-EXPORT              PIC S9(5) VALUE 0.
           05  T-X-PERIOD              PIC S9(5) VALUE 0.
           05  T-X-OTHER               PIC S9(5) VALUE 0.
       REPORT SECTION.
       RD  R-SOINVRN
           CONTROLS ARE FINAL D-GROUP
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 54
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-GRP TYPE IS CONTROL HEADING D-GROUP.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(9) VALUE "Customer ".
               10  COLUMN 10   PIC X(20) SOURCE D-HEAD-CUST.
               10  COLUMN 32   PIC X(9) VALUE "Currency ".
               10  COLUMN 41   PIC X(3) SOURCE D-HEAD-CURR.
               10  COLUMN 47   PIC X(15) VALUE "Delivery terms ".
               10  COLUMN 62   PIC X(3) SOURCE D-HEAD-TERM.
               10  COLUMN 68   PIC X(22) SOURCE D-HEAD-INV.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(5) VALUE "Order".
               10  COLUMN 16   PIC X(4) VALUE "Line".
               10  COLUMN 21   PIC X(8) VALUE "Despatch".
               10  COLUMN 34   PIC X(4) VALUE "Item".
               10  COLUMN 51   PIC X(9) VALUE "Despatchd".
               10  COLUMN 61   PIC X(9) VALUE "Billed   ".
               10  COLUMN 71   PIC X(9) VALUE "Withheld ".
               10  COLUMN 81   PIC X(11) VALUE "        Net".
               10  COLUMN 93   PIC X(4) VALUE "VAT ".
               10  COLUMN 98   PIC X(11) VALUE "        VAT".
               10  COLUMN 110  PIC X(23) VALUE "Reason if withheld".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(14) SOURCE D-ORDER.
               10  COLUMN 16   PIC ZZZ9 SOURCE D-LINE.
               10  COLUMN 21   PIC X(12) SOURCE D-DESP.
               10  COLUMN 34   PIC X(16) SOURCE D-ITEM.
               10  COLUMN 51   PIC Z(5)9.999 SOURCE D-QTY-DESP.
               10  COLUMN 61   PIC Z(5)9.999 SOURCE D-QTY-BILL.
               10  COLUMN 71   PIC Z(5)9.999 SOURCE D-QTY-HELD.
               10  COLUMN 81   PIC Z(7)9.99- SOURCE D-NET.
               10  COLUMN 93   PIC X(4) SOURCE D-VATC.
               10  COLUMN 98   PIC Z(7)9.99- SOURCE D-VAT.
               10  COLUMN 110  PIC X(23) SOURCE D-REASON.
       01  CF-GRP TYPE IS CONTROL FOOTING D-GROUP.
           05  LINE PLUS 1.
               10  COLUMN 34   PIC X(16) VALUE "Invoice totals".
               10  COLUMN 61   PIC Z(5)9.999 SUM D-QTY-BILL.
               10  COLUMN 71   PIC Z(5)9.999 SUM D-QTY-HELD.
               10  COLUMN 81   PIC Z(7)9.99- SUM D-NET.
               10  COLUMN 98   PIC Z(7)9.99- SUM D-VAT.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(26)
                               VALUE "Invoices in this run".
               10  COLUMN 28   PIC Z(6)9 SOURCE T-INVOICES.
               10  COLUMN 40   PIC X(14) VALUE "lines billed".
               10  COLUMN 55   PIC Z(6)9 SOURCE T-LINES.
               10  COLUMN 68   PIC X(16) VALUE "quantity billed".
               10  COLUMN 85   PIC Z(9)9.999 SOURCE T-QTY-BILL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(26)
                               VALUE "Net billed".
               10  COLUMN 28   PIC Z(9)9.99- SOURCE T-NET.
               10  COLUMN 45   PIC X(4) VALUE "VAT ".
               10  COLUMN 50   PIC Z(9)9.99- SOURCE T-VAT.
               10  COLUMN 68   PIC X(16) VALUE "deposit applied".
               10  COLUMN 85   PIC Z(9)9.99- SOURCE T-DEPOSIT.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(26)
                               VALUE "Quantity withheld".
               10  COLUMN 28   PIC Z(9)9.999 SOURCE T-QTY-HELD.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(26)
                               VALUE "Withheld - credit hold".
               10  COLUMN 28   PIC Z(4)9 SOURCE T-X-CREDIT.
               10  COLUMN 40   PIC X(22) VALUE "missing VAT evidence".
               10  COLUMN 63   PIC Z(4)9 SOURCE T-X-VAT.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(26)
                               VALUE "Withheld - export proof".
               10  COLUMN 28   PIC Z(4)9 SOURCE T-X-EXPORT.
               10  COLUMN 40   PIC X(22) VALUE "closed period".
               10  COLUMN 63   PIC Z(4)9 SOURCE T-X-PERIOD.
               10  COLUMN 75   PIC X(14) VALUE "other".
               10  COLUMN 90   PIC Z(4)9 SOURCE T-X-OTHER.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(60) VALUE
                 "Agreed price, VAT code and terms come from the order".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(60) VALUE
                 "line as accepted.  A deposit settles, it is not sale".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0200-JOB-START THRU 0200-EXIT
           PERFORM 0150-READ-PARMS THRU 0150-EXIT
           PERFORM 0300-OPEN-SPOOL THRU 0300-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 1000-RUN THRU 1000-EXIT
           PERFORM 6000-LIST-WITHHELD THRU 6000-EXIT
           PERFORM 0400-CLOSE-SPOOL THRU 0400-EXIT.
       0000-FINISH.
           PERFORM 0500-JOB-END THRU 0500-EXIT
           MOVE WS-RC TO RETURN-CODE
           STOP RUN.

      *>----------------------------------------------------------------
       0100-READ-ARGS SECTION.
       0100-START.
           INITIALIZE WS-CMDARGS
           MOVE "CMDLINE" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           MOVE CM-CO TO WS-CO
           MOVE CM-SITE TO WS-SITE
           MOVE CM-USER TO WS-USER
           MOVE CM-MODE TO WS-MODE
           MOVE CM-FROM TO WS-FROM
           MOVE CM-TO TO WS-TO
           MOVE "CUST" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-CUST
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "ARCLERK" TO WS-USER
           END-IF
           IF WS-MODE = SPACES
               MOVE "PREVIEW" TO WS-MODE
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           DISPLAY "STERP-I-1400 SOINVRUN sales invoicing run for "
                   FUNCTION TRIM(WS-CO) " mode "
                   FUNCTION TRIM(WS-MODE) UPON SYSOUT.
       0100-EXIT.
           EXIT.

      *>   SO.INVOICE.PERORDER - "Y" keeps one invoice per order, the
      *>   setting most customers with their own order references ask
      *>   for.  A missing parameter must not stop the run (COM-011).
       0150-READ-PARMS SECTION.
       0150-START.
           INITIALIZE WS-PARM
           MOVE "GET" TO CA-FUNCTION OF WS-PARM
           MOVE WS-CO TO CA-COMPANY OF WS-PARM
           MOVE WS-USER TO CA-USER OF WS-PARM
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-PARM
           MOVE "SO.INVOICE.PERORDER" TO PM-PARAM-CODE
           MOVE WS-TODAY-N TO PM-AS-OF
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE "N" TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM
           IF PM-VALUE-TEXT(1:1) = "Y" OR PM-VALUE-TEXT(1:1) = "y"
               MOVE "Y" TO WS-PERORDER
           ELSE
               MOVE "N" TO WS-PERORDER
           END-IF.
       0150-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "SOINVRUN" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE JL-JOB-RUN-ID TO WS-JOB-RUN-ID.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "SOINVRUN" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1401 cannot build the spool path"
                       UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           PERFORM 0350-FILL-HEADINGS THRU 0350-EXIT
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1402 cannot open " WS-SPOOL-NAME
                       UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           INITIATE R-SOINVRN.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "SOINVRUN" TO RPT-REPORT-ID
           MOVE "Sales invoicing run - control report" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE SPACES TO RPT-SUBTITLE
           IF WS-PREVIEW
               STRING "PREVIEW - nothing is posted"
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           ELSE
               STRING "COMMIT - invoices are posted by ARINVPST"
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           END-IF
           MOVE SPACES TO RPT-AS-AT-TEXT
           IF WS-PERORDER = "Y"
               STRING "One invoice per order"
                   DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           ELSE
               STRING "Per customer, currency, terms"
                   DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           END-IF
           PERFORM 0360-READ-COMPANY THRU 0360-EXIT.
       0350-EXIT.
           EXIT.

       0360-READ-COMPANY SECTION.
       0360-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Name, ISNULL(RegisteredNumber,'') FROM "
                  "core.Company WHERE CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 40
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-OK
               MOVE SO-COL(1) TO RPT-COMPANY-NAME
               MOVE SO-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   REQ: SO-014 - the driving selection.  Only CONFIRMED
      *>   despatch evidence with BilledQty < DespatchedQty qualifies,
      *>   which is what makes a second run of the same day harmless.
       1000-RUN SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT d.DespatchRef, d.CustomerCode, "
                  "o.CurrencyCode, ISNULL(o.IncotermsRule,''), "
                  "dl.OrderNumber, ISNULL(dl.OrderLineNum,0), "
                  "dl.ItemCode, ISNULL(dl.Description,''), "
                  "dl.DespatchedQty, dl.BilledQty, "
                  "ISNULL(dl.UomCode,''), l.UnitPrice, l.PricePerQty, "
                  "l.DiscountPct1, l.DiscountPct2, "
                  "ISNULL(l.VatCode,''), ISNULL(l.VatRatePct,0), "
                  "ISNULL(l.VatEvidenceRef,''), "
                  "ISNULL(l.DimCostCentre,''), "
                  "ISNULL(l.DimProject,''), o.ReleaseStatus, "
                  "CASE WHEN d.IsExport = 1 THEN 'Y' ELSE 'N' END, "
                  "d.ExportEvidenceStatus, "
                  "CONVERT(char(8), d.PostingDate, 112), "
                  "CONVERT(char(8), d.DespatchDate, 112), "
                  "o.DepositReceivedGBP, ISNULL(o.TermsCode,''), "
                  "ISNULL(o.TermsSnapshot,''), "
                  "ISNULL(o.SalespersonCode,''), o.FxRate, "
                  "dl.DespatchLineId, dl.SalesOrderLineId, "
                  "ISNULL(o.InvoiceCustomerCode, o.CustomerCode), "
                  "ISNULL(o.CustomerOrderRef,''), "
                  "ISNULL(o.CustomerVatRegNo,''), "
                  "ISNULL(o.VatLocation,''), "
                  "ISNULL(o.BillToAddressSnapshot,''), "
                  "ISNULL(o.DeliveryAddressSnapshot,''), l.LineType, "
                  "ISNULL(l.VatOverrideReason,'')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           STRING FUNCTION TRIM(SQLIO-STMT) " "
                  "FROM so.DespatchLine dl "
                  "JOIN so.Despatch d ON d.DespatchId = dl.DespatchId "
                  "JOIN so.SalesOrderLine l ON "
                  "l.SalesOrderLineId = dl.SalesOrderLineId "
                  "JOIN so.SalesOrder o ON "
                  "o.SalesOrderId = l.SalesOrderId "
                  "WHERE d.CompanyCode = ? AND d.Status = 'CONFIRMED' "
                  "AND dl.BilledQty < dl.DespatchedQty "
                  "AND (? = '' OR d.SiteCode = ?) "
                  "AND (? = '' OR d.CustomerCode = ?) "
                  "AND (? = '' OR CONVERT(char(8),d.DespatchDate,112) "
                  ">= ?) AND (? = '' OR "
                  "CONVERT(char(8),d.DespatchDate,112) <= ?) "
                  "ORDER BY ISNULL(o.InvoiceCustomerCode,"
                  "o.CustomerCode), o.CurrencyCode, "
                  "ISNULL(o.IncotermsRule,''), dl.OrderNumber, "
                  "d.DespatchRef, dl.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 9 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-SITE TO SQLIO-PARM-VAL(2)
           MOVE WS-SITE TO SQLIO-PARM-VAL(3)
           MOVE WS-CUST TO SQLIO-PARM-VAL(4)
           MOVE WS-CUST TO SQLIO-PARM-VAL(5)
           MOVE WS-FROM TO SQLIO-PARM-VAL(6)
           MOVE WS-FROM TO SQLIO-PARM-VAL(7)
           MOVE WS-TO TO SQLIO-PARM-VAL(8)
           MOVE WS-TO TO SQLIO-PARM-VAL(9)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 9
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 40 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1403 " SQLIO-ERRMSG(1:100) UPON SYSOUT
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           MOVE 0 TO WS-SEL-CNT
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA SO-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   IF WS-SEL-CNT < WS-SEL-MAX
                       ADD 1 TO WS-SEL-CNT
                       MOVE SO-ROW TO WS-SEL-ROW(WS-SEL-CNT)
                   ELSE
                       MOVE "Y" TO WS-DONE
                       IF WS-RC < 4
                           MOVE 4 TO WS-RC
                       END-IF
                       DISPLAY "STERP-W-1413 selection truncated at "
                               "400 despatch lines" UPON SYSOUT
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           PERFORM VARYING WS-SEL-IX FROM 1 BY 1
               UNTIL WS-SEL-IX > WS-SEL-CNT
               MOVE WS-SEL-ROW(WS-SEL-IX) TO SO-ROW
               PERFORM 1100-ONE-ROW THRU 1100-EXIT
           END-PERFORM
           IF WS-BUF-CNT > 0
               PERFORM 3000-FLUSH-INVOICE THRU 3000-EXIT
           END-IF.
       1000-EXIT.
           EXIT.

       1100-ONE-ROW SECTION.
       1100-START.
           ADD 1 TO WS-ROWS-OUT
           PERFORM 1200-SCREEN-LINE THRU 1200-EXIT
           IF WS-SKIP-LINE = "Y"
               PERFORM 1900-WITHHOLD THRU 1900-EXIT
               GO TO 1100-EXIT
           END-IF
           PERFORM 1300-BUILD-KEY THRU 1300-EXIT
           IF WS-BUF-CNT > 0 AND G-KEY NOT = G-PREV-KEY
               PERFORM 3000-FLUSH-INVOICE THRU 3000-EXIT
           END-IF
           IF WS-BUF-CNT NOT < 25
               PERFORM 3000-FLUSH-INVOICE THRU 3000-EXIT
           END-IF
           IF WS-BUF-CNT = 0
               PERFORM 1400-CAPTURE-HEADER THRU 1400-EXIT
           END-IF
           PERFORM 1500-ADD-LINE THRU 1500-EXIT
           MOVE G-KEY TO G-PREV-KEY.
       1100-EXIT.
           EXIT.

      *>   REQ: SO-014, SO-025 - every reason a line may be withheld.
      *>   A withheld line is reported, never silently dropped.
       1200-SCREEN-LINE SECTION.
       1200-START.
           MOVE "N" TO WS-SKIP-LINE
           MOVE SPACES TO WS-REASON
           COMPUTE WS-QTY = FUNCTION NUMVAL(SO-COL(9))
                          - FUNCTION NUMVAL(SO-COL(10))
           IF WS-QTY NOT > 0
      *>       already billed in full - idempotency by BilledQty
               MOVE "Y" TO WS-SKIP-LINE
               MOVE "Already billed" TO WS-REASON
               ADD 1 TO T-X-OTHER
               GO TO 1200-EXIT
           END-IF
           IF SO-COL(21)(1:10) = "CREDITHOLD"
               MOVE "Y" TO WS-SKIP-LINE
               MOVE "Credit hold" TO WS-REASON
               ADD 1 TO T-X-CREDIT
               GO TO 1200-EXIT
           END-IF
           IF SO-COL(22)(1:1) = "Y"
              AND (SO-COL(23)(1:7) = "PENDING"
                OR SO-COL(23)(1:7) = "OVERDUE"
                OR SO-COL(23)(1:6) = "FAILED")
               MOVE "Y" TO WS-SKIP-LINE
               MOVE "Export proof outstanding" TO WS-REASON
               ADD 1 TO T-X-EXPORT
               GO TO 1200-EXIT
           END-IF
      *>   a reverse charge or zero rated export needs its evidence on
      *>   the line before it may be invoiced - a missing condition
      *>   blocks invoicing, it never defaults to zero rated
           IF (SO-COL(16)(1:3) = "RC " OR SO-COL(16)(1:3) = "DRC"
               OR SO-COL(16)(1:3) = "EXP")
              AND SO-COL(18) = SPACES
               MOVE "Y" TO WS-SKIP-LINE
               MOVE "Missing VAT evidence" TO WS-REASON
               ADD 1 TO T-X-VAT
               GO TO 1200-EXIT
           END-IF
           IF SO-COL(16) = SPACES
               MOVE "Y" TO WS-SKIP-LINE
               MOVE "Missing VAT evidence" TO WS-REASON
               ADD 1 TO T-X-VAT
               GO TO 1200-EXIT
           END-IF
           PERFORM 1250-CHECK-PERIOD THRU 1250-EXIT.
       1200-EXIT.
           EXIT.

       1250-CHECK-PERIOD SECTION.
       1250-START.
           INITIALIZE WS-PERIOD
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERIOD
           MOVE WS-CO TO CA-COMPANY OF WS-PERIOD
           MOVE WS-USER TO CA-USER OF WS-PERIOD
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-PERIOD
           COMPUTE PC-POSTING-DATE = FUNCTION NUMVAL(SO-COL(24))
           COMPUTE PC-DOCUMENT-DATE = FUNCTION NUMVAL(SO-COL(25))
           MOVE PC-DOCUMENT-DATE TO PC-TAX-POINT
           MOVE "POST" TO PC-ACTION
           CALL "PERIODCK" USING WS-PERIOD
           IF CA-RETURN-CODE OF WS-PERIOD > 6
               MOVE "Y" TO WS-SKIP-LINE
               MOVE "Closed period" TO WS-REASON
               ADD 1 TO T-X-PERIOD
           END-IF.
       1250-EXIT.
           EXIT.

      *>   REQ: SO-014.  The invoice carries ONE despatch reference
      *>   (AI-DESPATCH-REF), and ARINVPST refuses a despatch it has
      *>   already billed, so a group may never span two despatches:
      *>   the whole group was being refused - and silently dropped -
      *>   because the FIRST despatch in it had been invoiced before.
      *>   CR2608.
       1300-BUILD-KEY SECTION.
       1300-START.
           MOVE SPACES TO G-KEY
           IF WS-PERORDER = "Y"
               STRING SO-COL(33)(1:20) SO-COL(3)(1:3)
                      SO-COL(4)(1:3) SO-COL(5)(1:30)
                      SO-COL(1)(1:30)
                   DELIMITED BY SIZE INTO G-KEY
           ELSE
               STRING SO-COL(33)(1:20) SO-COL(3)(1:3)
                      SO-COL(4)(1:3) SO-COL(1)(1:30)
                   DELIMITED BY SIZE INTO G-KEY
           END-IF.
       1300-EXIT.
           EXIT.

       1400-CAPTURE-HEADER SECTION.
       1400-START.
           MOVE SO-COL(2) TO G-CUST
           MOVE SO-COL(33) TO G-INV-CUST
           MOVE SO-COL(3) TO G-CURR
           MOVE SO-COL(4) TO G-DELTERM
           MOVE SO-COL(5) TO G-ORDER
           MOVE SO-COL(1) TO G-DESP
           MOVE SO-COL(27) TO G-TERMS
           MOVE SO-COL(28) TO G-TERMS-SNAP
           MOVE SO-COL(29) TO G-SALESP
           MOVE SO-COL(34) TO G-CUSTREF
           MOVE SO-COL(35) TO G-CUSTVAT
           MOVE SO-COL(36) TO G-VATLOC
           MOVE SO-COL(37) TO G-BILLADDR
           MOVE SO-COL(38) TO G-DELADDR
           COMPUTE G-FX = FUNCTION NUMVAL(SO-COL(30))
           COMPUTE G-POSTDATE = FUNCTION NUMVAL(SO-COL(24))
           COMPUTE G-DOCDATE = FUNCTION NUMVAL(SO-COL(25))
           COMPUTE G-DEPOSIT = FUNCTION NUMVAL(SO-COL(26)).
       1400-EXIT.
           EXIT.

      *>   REQ: SO-014 - the captured price, discounts, VAT code and
      *>   VAT rate are used exactly as the customer accepted them.
       1500-ADD-LINE SECTION.
       1500-START.
           ADD 1 TO WS-BUF-CNT
           MOVE WS-BUF-CNT TO WS-IX
           COMPUTE BL-DESP-LINE-ID(WS-IX) =
               FUNCTION NUMVAL(SO-COL(31))
           COMPUTE BL-SO-LINE-ID(WS-IX) = FUNCTION NUMVAL(SO-COL(32))
           MOVE SO-COL(5) TO BL-ORDER(WS-IX)
           COMPUTE BL-ORDER-LINE(WS-IX) = FUNCTION NUMVAL(SO-COL(6))
           MOVE SO-COL(1) TO BL-DESP(WS-IX)
           MOVE SO-COL(7) TO BL-ITEM(WS-IX)
           MOVE SO-COL(8) TO BL-DESC(WS-IX)
           MOVE WS-QTY TO BL-QTY(WS-IX)
           MOVE SO-COL(11) TO BL-UOM(WS-IX)
           COMPUTE BL-PRICE(WS-IX) = FUNCTION NUMVAL(SO-COL(12))
           COMPUTE BL-PRICE-PER(WS-IX) = FUNCTION NUMVAL(SO-COL(13))
           IF BL-PRICE-PER(WS-IX) = 0
               MOVE 1 TO BL-PRICE-PER(WS-IX)
           END-IF
           COMPUTE BL-DISC1(WS-IX) = FUNCTION NUMVAL(SO-COL(14))
           COMPUTE BL-DISC2(WS-IX) = FUNCTION NUMVAL(SO-COL(15))
           MOVE SO-COL(16) TO BL-VATC(WS-IX)
           COMPUTE BL-VATR(WS-IX) = FUNCTION NUMVAL(SO-COL(17))
           MOVE SO-COL(19) TO BL-CC(WS-IX)
           MOVE SO-COL(20) TO BL-PROJ(WS-IX)
           PERFORM 1600-VALUE-LINE THRU 1600-EXIT.
       1500-EXIT.
           EXIT.

      *>   extend, apply the two line discounts in order, round once.
      *>   RNDALLOC owns the rounding rule so that the invoicing run
      *>   and the order entry screen can never disagree (COM-011).
       1600-VALUE-LINE SECTION.
       1600-START.
           COMPUTE WS-AMT = BL-QTY(WS-IX) * BL-PRICE(WS-IX)
                          / BL-PRICE-PER(WS-IX)
           COMPUTE WS-AMT = WS-AMT * (1 - BL-DISC1(WS-IX) / 100)
           COMPUTE WS-AMT = WS-AMT * (1 - BL-DISC2(WS-IX) / 100)
           PERFORM 1650-ROUND-2 THRU 1650-EXIT
           MOVE WS-AMT2 TO BL-NET(WS-IX)
           COMPUTE WS-AMT = BL-NET(WS-IX) * BL-VATR(WS-IX) / 100
           PERFORM 1650-ROUND-2 THRU 1650-EXIT
           MOVE WS-AMT2 TO BL-VAT(WS-IX).
       1600-EXIT.
           EXIT.

       1650-ROUND-2 SECTION.
       1650-START.
           INITIALIZE WS-ROUND
           MOVE "ROUND" TO CA-FUNCTION OF WS-ROUND
           MOVE WS-CO TO CA-COMPANY OF WS-ROUND
           MOVE WS-USER TO CA-USER OF WS-ROUND
           MOVE 2 TO RA-PLACES
           MOVE WS-AMT TO RA-VALUE
           CALL "RNDALLOC" USING WS-ROUND
           IF CA-RETURN-CODE OF WS-ROUND > 4
               MOVE WS-AMT TO WS-AMT2
           ELSE
               MOVE RA-RESULT TO WS-AMT2
           END-IF.
       1650-EXIT.
           EXIT.

       1900-WITHHOLD SECTION.
       1900-START.
           COMPUTE WS-QTY = FUNCTION NUMVAL(SO-COL(9))
                          - FUNCTION NUMVAL(SO-COL(10))
           IF WS-QTY < 0
               MOVE 0 TO WS-QTY
           END-IF
           ADD WS-QTY TO T-QTY-HELD
           IF WS-EXC-CNT < 300
               ADD 1 TO WS-EXC-CNT
               MOVE WS-EXC-CNT TO WS-JX
               MOVE SO-COL(5) TO EX-ORDER(WS-JX)
               COMPUTE EX-LINE(WS-JX) = FUNCTION NUMVAL(SO-COL(6))
               MOVE SO-COL(1) TO EX-DESP(WS-JX)
               MOVE SO-COL(7) TO EX-ITEM(WS-JX)
               MOVE WS-QTY TO EX-QTY(WS-JX)
               MOVE WS-REASON TO EX-REASON(WS-JX)
           END-IF.
       1900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   REQ: SO-014, SO-019 - post one invoice and write the billed
      *>   quantity back inside the SAME unit of work.  Either both
      *>   happen or neither does; a doomed posting leaves BilledQty
      *>   untouched so the evidence is picked up again tomorrow.
       3000-FLUSH-INVOICE SECTION.
       3000-START.
           IF WS-BUF-CNT = 0
               GO TO 3000-EXIT
           END-IF
           ADD 1 TO WS-INV-SEQ
           PERFORM 3100-BUILD-CORREL THRU 3100-EXIT
           PERFORM 3200-DEPOSIT THRU 3200-EXIT
           MOVE SPACES TO D-HEAD-INV
           IF WS-PREVIEW
               STRING "Preview only" DELIMITED BY SIZE
                   INTO D-HEAD-INV
           ELSE
               PERFORM 3300-POST-INVOICE THRU 3300-EXIT
           END-IF
           PERFORM 3800-PRINT-GROUP THRU 3800-EXIT
           MOVE 0 TO WS-BUF-CNT
           MOVE SPACES TO G-PREV-KEY.
       3000-EXIT.
           EXIT.

      *>   a deterministic correlation id: the same evidence presented
      *>   again produces the same id, so ARINVPST returns the invoice
      *>   it already made instead of making a second one (COM-015).
       3100-BUILD-CORREL SECTION.
       3100-START.
           MOVE BL-DESP-LINE-ID(1) TO WS-KEY-ID
           MOVE 0 TO WS-QTY
           PERFORM VARYING WS-IX FROM 1 BY 1
               UNTIL WS-IX > WS-BUF-CNT
               ADD BL-QTY(WS-IX) TO WS-QTY
           END-PERFORM
           MOVE WS-QTY TO WS-QTY-ED
           MOVE SPACES TO WS-CORREL
           STRING "SOI-" FUNCTION TRIM(WS-CO) "-" WS-KEY-ID "-"
                  FUNCTION TRIM(WS-QTY-ED)
               DELIMITED BY SIZE INTO WS-CORREL.
       3100-EXIT.
           EXIT.

      *>   REQ: SO-019 - a deposit held against the order is applied
      *>   as a settlement of the new invoice.  It is never revenue in
      *>   its own right, and it can only be applied once, so what
      *>   earlier invoices already took is deducted first.
       3200-DEPOSIT SECTION.
       3200-START.
           MOVE 0 TO WS-DEP-APPLY WS-DEP-USED
           IF G-DEPOSIT NOT > 0
               GO TO 3200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(SUM(DepositAppliedGBP),0) FROM "
                  "ar.SalesInvoice WHERE CompanyCode = ? AND "
                  "SalesOrderNumber = ? AND DocumentType = 'INVOICE' "
                  "AND Status IN ('APPROVED','POSTED')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE G-ORDER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-OK
               COMPUTE WS-DEP-USED = FUNCTION NUMVAL(SO-COL(1))
           END-IF
           MOVE 0 TO WS-AMT
           PERFORM VARYING WS-IX FROM 1 BY 1
               UNTIL WS-IX > WS-BUF-CNT
               ADD BL-NET(WS-IX) TO WS-AMT
               ADD BL-VAT(WS-IX) TO WS-AMT
           END-PERFORM
           COMPUTE WS-DEP-APPLY = G-DEPOSIT - WS-DEP-USED
           IF WS-DEP-APPLY < 0
               MOVE 0 TO WS-DEP-APPLY
           END-IF
           IF WS-DEP-APPLY > WS-AMT
               MOVE WS-AMT TO WS-DEP-APPLY
           END-IF
           MOVE 40 TO SQLIO-COL-CNT.
       3200-EXIT.
           EXIT.

       3300-POST-INVOICE SECTION.
       3300-START.
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 3 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1404 cannot start a transaction"
                       UPON SYSOUT
               GO TO 3300-EXIT
           END-IF
           PERFORM 3400-CALL-ARINVPST THRU 3400-EXIT
           IF CA-RETURN-CODE OF WS-ARINV > 4
               PERFORM 3900-ROLLBACK THRU 3900-EXIT
               MOVE SPACES TO D-HEAD-INV
               STRING "REFUSED "
                      FUNCTION TRIM(CA-REASON-CODE OF WS-ARINV)
                   DELIMITED BY SIZE INTO D-HEAD-INV
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
               DISPLAY "STERP-W-1405 "
                       CA-MESSAGE OF WS-ARINV(1:90) UPON SYSOUT
               GO TO 3300-EXIT
           END-IF
           PERFORM 3500-WRITE-BACK THRU 3500-EXIT
           IF WS-RC > 8
               PERFORM 3900-ROLLBACK THRU 3900-EXIT
               GO TO 3300-EXIT
           END-IF
           MOVE "COMT" TO SQLIO-FUNC
           MOVE 3 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1406 commit failed" UPON SYSOUT
               GO TO 3300-EXIT
           END-IF
           ADD 1 TO T-INVOICES
           ADD WS-DEP-APPLY TO T-DEPOSIT
           MOVE SPACES TO D-HEAD-INV
           IF AI-DUPLICATE = "Y"
               STRING "Already " FUNCTION TRIM(AI-INVOICE-NO)
                   DELIMITED BY SIZE INTO D-HEAD-INV
           ELSE
               STRING "Invoice " FUNCTION TRIM(AI-INVOICE-NO)
                   DELIMITED BY SIZE INTO D-HEAD-INV
           END-IF.
       3300-EXIT.
           EXIT.

       3400-CALL-ARINVPST SECTION.
       3400-START.
           INITIALIZE WS-ARINV
           MOVE "ARINVPST" TO CA-PROGRAM OF WS-ARINV
           MOVE "POST" TO CA-FUNCTION OF WS-ARINV
           MOVE WS-CO TO CA-COMPANY OF WS-ARINV
           MOVE WS-SITE TO CA-SITE OF WS-ARINV
           MOVE WS-USER TO CA-USER OF WS-ARINV
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-ARINV
           MOVE WS-CORREL TO CA-CORRELATION-ID OF WS-ARINV
           MOVE "INVOICE" TO AI-DOC-TYPE
           MOVE G-CUST TO AI-CUSTOMER
           MOVE G-INV-CUST TO AI-INVOICE-CUST
           MOVE G-DOCDATE TO AI-DOC-DATE
           MOVE G-DOCDATE TO AI-TAX-POINT
           MOVE G-POSTDATE TO AI-POSTING-DATE
           MOVE G-TERMS TO AI-TERMS-CODE
           MOVE G-TERMS-SNAP TO AI-TERMS-SNAPSHOT
           MOVE "ORDER" TO AI-TERMS-SOURCE
           MOVE G-CURR TO AI-CURRENCY
           MOVE G-FX TO AI-FX-RATE
           MOVE G-DOCDATE TO AI-FX-RATE-DATE
           MOVE "DESPATCH" TO AI-SOURCE
           MOVE G-CUSTREF TO AI-ORDER-REF
           MOVE G-ORDER TO AI-SO-NUMBER
           MOVE G-DESP TO AI-DESPATCH-REF
           MOVE G-SALESP TO AI-SALESPERSON
           MOVE G-BILLADDR TO AI-BILL-ADDRESS
           MOVE G-DELADDR TO AI-DELIVERY-ADDR
           MOVE G-CUSTVAT TO AI-CUST-VAT-NO
           MOVE G-VATLOC TO AI-VAT-LOCATION
           MOVE "N" TO AI-SIMPLIFIED
           MOVE WS-DEP-APPLY TO AI-DEPOSIT-APPLY
           MOVE WS-BUF-CNT TO AI-LINE-CNT
           MOVE 0 TO AI-NET AI-VAT AI-GROSS
           PERFORM VARYING WS-IX FROM 1 BY 1
               UNTIL WS-IX > WS-BUF-CNT
               MOVE BL-ITEM(WS-IX) TO AI-L-ITEM(WS-IX)
               MOVE BL-DESC(WS-IX) TO AI-L-DESC(WS-IX)
               MOVE BL-QTY(WS-IX) TO AI-L-QTY(WS-IX)
               MOVE BL-UOM(WS-IX) TO AI-L-UOM(WS-IX)
               MOVE BL-PRICE(WS-IX) TO AI-L-PRICE(WS-IX)
               MOVE BL-DISC1(WS-IX) TO AI-L-DISC-PCT(WS-IX)
               MOVE 0 TO AI-L-CHARGE(WS-IX)
               MOVE BL-NET(WS-IX) TO AI-L-NET(WS-IX)
               MOVE BL-VATC(WS-IX) TO AI-L-VAT-CODE(WS-IX)
               MOVE BL-VATR(WS-IX) TO AI-L-VAT-RATE(WS-IX)
               MOVE BL-VAT(WS-IX) TO AI-L-VAT-AMT(WS-IX)
               MOVE BL-CC(WS-IX) TO AI-L-COST-CENTRE(WS-IX)
               MOVE BL-PROJ(WS-IX) TO AI-L-PROJECT(WS-IX)
               MOVE BL-ORDER-LINE(WS-IX) TO AI-L-SO-LINE(WS-IX)
               ADD BL-NET(WS-IX) TO AI-NET
               ADD BL-VAT(WS-IX) TO AI-VAT
           END-PERFORM
           COMPUTE AI-GROSS = AI-NET + AI-VAT
           IF G-CURR = "GBP" OR G-FX = 0 OR G-FX = 1
               MOVE AI-NET TO AI-NET-GBP
               MOVE AI-VAT TO AI-VAT-GBP
               MOVE AI-GROSS TO AI-GROSS-GBP
           ELSE
               COMPUTE AI-NET-GBP ROUNDED = AI-NET / G-FX
               COMPUTE AI-VAT-GBP ROUNDED = AI-VAT / G-FX
               COMPUTE AI-GROSS-GBP ROUNDED = AI-GROSS / G-FX
           END-IF
           CALL "ARINVPST" USING WS-ARINV.
       3400-EXIT.
           EXIT.

      *>   REQ: SO-014 - BilledQty is advanced, never set absolutely,
      *>   and the UPDATE refuses to take it past DespatchedQty.  That
      *>   single predicate is what stops a rerun rebilling an earlier
      *>   quantity even if the selection were ever to be wrong.
       3500-WRITE-BACK SECTION.
       3500-START.
           PERFORM VARYING WS-IX FROM 1 BY 1
               UNTIL WS-IX > WS-BUF-CNT OR WS-RC > 8
               PERFORM 3550-ONE-DESP-LINE THRU 3550-EXIT
               PERFORM 3600-ONE-ORDER-LINE THRU 3600-EXIT
               PERFORM 3700-AUDIT-LINE THRU 3700-EXIT
           END-PERFORM
           IF WS-RC > 8
               GO TO 3500-EXIT
           END-IF
           PERFORM 3650-ORDER-STATUS THRU 3650-EXIT.
       3500-EXIT.
           EXIT.

       3550-ONE-DESP-LINE SECTION.
       3550-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.DespatchLine SET BilledQty = "
                  "BilledQty + ? WHERE DespatchLineId = ? AND "
                  "BilledQty + ? <= DespatchedQty"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE BL-QTY(WS-IX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE BL-DESP-LINE-ID(WS-IX) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1407 " SQLIO-ERRMSG(1:90) UPON SYSOUT
               GO TO 3550-EXIT
           END-IF
           IF SQLIO-ROWCOUNT NOT = 1
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1408 despatch line already billed"
                       UPON SYSOUT
               GO TO 3550-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           ADD BL-QTY(WS-IX) TO T-QTY-BILL
           ADD BL-NET(WS-IX) TO T-NET
           ADD BL-VAT(WS-IX) TO T-VAT
           ADD 1 TO T-LINES.
       3550-EXIT.
           EXIT.

       3600-ONE-ORDER-LINE SECTION.
       3600-START.
           IF WS-RC > 8
               GO TO 3600-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrderLine SET BilledQty = "
                  "BilledQty + ? WHERE SalesOrderLineId = ? AND "
                  "BilledQty + ? <= DespatchedQty"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE BL-QTY(WS-IX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE BL-SO-LINE-ID(WS-IX) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1409 " SQLIO-ERRMSG(1:90) UPON SYSOUT
               GO TO 3600-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW.
       3600-EXIT.
           EXIT.

       3650-ORDER-STATUS SECTION.
       3650-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE o SET BillingStatus = CASE WHEN EXISTS "
                  "(SELECT 1 FROM so.SalesOrderLine x WHERE "
                  "x.SalesOrderId = o.SalesOrderId AND x.Status = "
                  "'OPEN' AND x.BilledQty < x.DespatchedQty) THEN "
                  "'PARTBILLED' ELSE 'BILLED' END, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() FROM so.SalesOrder o "
                  "WHERE o.CompanyCode = ? AND o.OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE WS-USER TO SQLIO-PARM-VAL(1)
           MOVE WS-CO TO SQLIO-PARM-VAL(2)
           MOVE G-ORDER TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1410 " SQLIO-ERRMSG(1:90) UPON SYSOUT
               GO TO 3650-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW.
       3650-EXIT.
           EXIT.

      *>   the invoice number written back against the despatch line
       3700-AUDIT-LINE SECTION.
       3700-START.
           IF WS-RC > 8
               GO TO 3700-EXIT
           END-IF
           INITIALIZE WS-AUDIT
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUDIT
           MOVE WS-CO TO CA-COMPANY OF WS-AUDIT
           MOVE WS-USER TO CA-USER OF WS-AUDIT
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-AUDIT
           MOVE WS-CORREL TO CA-CORRELATION-ID OF WS-AUDIT
           MOVE "so.DespatchLine" TO AU-ENTITY-TYPE
           MOVE BL-DESP-LINE-ID(WS-IX) TO AU-ENTITY-KEY
           MOVE "INVOICE" TO AU-ACTION
           MOVE "BilledQty" TO AU-FIELD-NAME
           MOVE BL-QTY(WS-IX) TO WS-QTY-ED
           MOVE FUNCTION TRIM(WS-QTY-ED) TO AU-NEW-VALUE
           MOVE SPACES TO AU-REASON
           STRING "Billed on invoice " FUNCTION TRIM(AI-INVOICE-NO)
                  " order " FUNCTION TRIM(BL-ORDER(WS-IX))
               DELIMITED BY SIZE INTO AU-REASON
           MOVE AI-INVOICE-NO TO AU-AUTHORITY-REF
           MOVE "SOINVRUN" TO AU-SOURCE
           MOVE "N" TO AU-SENSITIVE
           CALL "AUDITLOG" USING WS-AUDIT.
       3700-EXIT.
           EXIT.

       3800-PRINT-GROUP SECTION.
       3800-START.
           MOVE SPACES TO D-GROUP
           STRING "INV" WS-INV-SEQ DELIMITED BY SIZE INTO D-GROUP
           MOVE G-INV-CUST TO D-HEAD-CUST
           MOVE G-CURR TO D-HEAD-CURR
           MOVE G-DELTERM TO D-HEAD-TERM
           PERFORM VARYING WS-IX FROM 1 BY 1
               UNTIL WS-IX > WS-BUF-CNT
               MOVE BL-ORDER(WS-IX) TO D-ORDER
               MOVE BL-ORDER-LINE(WS-IX) TO D-LINE
               MOVE BL-DESP(WS-IX) TO D-DESP
               MOVE BL-ITEM(WS-IX) TO D-ITEM
               MOVE BL-QTY(WS-IX) TO D-QTY-DESP
               MOVE BL-QTY(WS-IX) TO D-QTY-BILL
               MOVE 0 TO D-QTY-HELD
               MOVE BL-NET(WS-IX) TO D-NET
               MOVE BL-VATC(WS-IX) TO D-VATC
               MOVE BL-VAT(WS-IX) TO D-VAT
               MOVE SPACES TO D-REASON
               GENERATE DE-LINE
           END-PERFORM.
       3800-EXIT.
           EXIT.

       3900-ROLLBACK SECTION.
       3900-START.
           MOVE "ROLB" TO SQLIO-FUNC
           MOVE 3 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA SO-ROW.
       3900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       6000-LIST-WITHHELD SECTION.
       6000-START.
           IF WS-EXC-CNT = 0
               GO TO 6000-EXIT
           END-IF
           MOVE "WITHHELD" TO D-GROUP
           MOVE SPACES TO D-HEAD-CUST D-HEAD-CURR D-HEAD-TERM
           MOVE SPACES TO D-HEAD-INV
           STRING "Not invoiced" DELIMITED BY SIZE INTO D-HEAD-INV
           PERFORM VARYING WS-JX FROM 1 BY 1
               UNTIL WS-JX > WS-EXC-CNT
               MOVE EX-ORDER(WS-JX) TO D-ORDER
               MOVE EX-LINE(WS-JX) TO D-LINE
               MOVE EX-DESP(WS-JX) TO D-DESP
               MOVE EX-ITEM(WS-JX) TO D-ITEM
               MOVE EX-QTY(WS-JX) TO D-QTY-DESP
               MOVE 0 TO D-QTY-BILL
               MOVE EX-QTY(WS-JX) TO D-QTY-HELD
               MOVE 0 TO D-NET D-VAT
               MOVE SPACES TO D-VATC
               MOVE EX-REASON(WS-JX) TO D-REASON
               GENERATE DE-LINE
           END-PERFORM.
       6000-EXIT.
           EXIT.

       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-SOINVRN
           CLOSE RPT-FILE
           DISPLAY "STERP-I-1411 " WS-ROWS-OUT
                   " despatch line(s) examined" UPON SYSOUT
           DISPLAY "STERP-I-1412 " T-INVOICES " invoice(s) posted"
                   UPON SYSOUT
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME)
                   UPON SYSOUT.
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "SOINVRUN" TO JL-JOB-NAME
           MOVE WS-JOB-RUN-ID TO JL-JOB-RUN-ID
           EVALUATE TRUE
               WHEN WS-RC = 0
                   MOVE "COMPLETE" TO JL-STATUS
               WHEN WS-RC = 4
                   MOVE "WARNING" TO JL-STATUS
               WHEN OTHER
                   MOVE "FAILED" TO JL-STATUS
           END-EVALUATE
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE T-LINES TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
