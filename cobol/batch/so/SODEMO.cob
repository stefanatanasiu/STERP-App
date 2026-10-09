      *>================================================================
      *> PROGRAM-ID : SODEMO
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : DEMONSTRATION TRANSACTION GENERATOR FOR THE
      *>              SALES MODULE.  IT DRIVES A REALISTIC ORDER TO
      *>              CASH FLOW: QUOTE, ACCEPTED ORDER, CREDIT
      *>              DECISION, DELIVERY SCHEDULE, RESERVATION, PICK
      *>              LIST, DESPATCH CONFIRMATION, THE INVOICE RUN AND
      *>              A CUSTOMER RETURN.
      *>
      *>              NOTHING IS INSERTED DIRECTLY.  EVERY STEP GOES
      *>              THROUGH THE ONLINE SERVICE THAT OWNS IT, SO THE
      *>              STOCK LEDGER, THE COST OF SALE, THE SALES LEDGER
      *>              AND THE GENERAL LEDGER ALL COME FROM ONE PATH
      *>              AND AGREE WITH EACH OTHER.  DESPATCHES, COGS
      *>              JOURNALS AND SALES INVOICES ARE ACCOUNTING
      *>              EVENTS AND ARE NEVER SEEDED BY HAND.
      *>
      *>              THE JOB IS RE-RUNNABLE.  THE ORDER IT BUILDS
      *>              CARRIES CUSTOMER REFERENCE SODEMO-<COMPANY>, SO
      *>              A SECOND RUN FINDS ITS OWN WORK AND CARRIES ON
      *>              FROM WHERE IT GOT TO; EVERY SERVICE CALL ALSO
      *>              CARRIES CA-CORRELATION-ID, SO A REPEATED
      *>              INSTRUCTION ANSWERS 04 AND POSTS NOTHING TWICE.
      *>
      *>              THE INVOICE RUN IS A BATCH PROGRAM THAT ENDS IN
      *>              STOP RUN, SO IT IS SPAWNED AS A PROCESS RATHER
      *>              THAN CALLED - SEE 7000-INVOICE-RUN.
      *> RUN AS     : SODEMO CO=STC SITE= MODE=COMMIT USER=SALESREP
      *> OUTPUT     : run\spool\SODEMO-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO, SOQUOTE,
      *>              SOACCEPT, SOSCHED, SORESV, PICKGEN, DESPATCH,
      *>              RETURNS, SOINVRUN (spawned)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2026-10-06 STERP ORIGINAL - DEMONSTRATION DATA GENERATOR
      *> 2026-10-08 STERP GUI-005 THE SEEDED LOT / SERIAL CONTROLLED
      *>                 PICKED ORDERS ARE RESERVED AND PICKED THROUGH
      *>                 SORESV AND PICKGEN SO THEY CAN BE DESPATCHED
      *>                 ON THE SCREEN (2700-STAGE-PICKED)
      *>================================================================
      *> REQ: SO-001, SO-002, SO-007, SO-008, SO-009, SO-012, SO-013,
      *> REQ: SO-014, SO-017, SO-018, SO-024
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SODEMO.
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
       FD  RPT-FILE REPORT IS R-SODEMO.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY SOQUOTECA REPLACING ==DFHCOMMAREA== BY ==WS-QUO==.
       COPY SOACCEPTCA REPLACING ==DFHCOMMAREA== BY ==WS-ACC==.
       COPY SOSCHEDCA REPLACING ==DFHCOMMAREA== BY ==WS-SCH==.
       COPY SORESVCA REPLACING ==DFHCOMMAREA== BY ==WS-RSV==.
       COPY PICKGENCA REPLACING ==DFHCOMMAREA== BY ==WS-PCK==.
       COPY DESPATCHCA REPLACING ==DFHCOMMAREA== BY ==WS-DSP==.
       COPY RETURNSCA REPLACING ==DFHCOMMAREA== BY ==WS-RET==.
       COPY CREDCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-CRD==.
       01  SD-ROW.
           05  SD-COL OCCURS 30 TIMES PIC X(100).
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-SITE-ARG             PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-MODE                 PIC X(8) VALUE "PREVIEW".
               88  WS-PREVIEW                    VALUE "PREVIEW".
               88  WS-COMMIT                     VALUE "COMMIT".
       01  WS-SCRATCH.
           05  WS-IX                   PIC S9(4) VALUE 0.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-TODAY-N              PIC 9(8) VALUE 0.
           05  WS-INT-DATE             PIC S9(9) VALUE 0.
           05  WS-PROMISE-DATE         PIC 9(8) VALUE 0.
           05  WS-VALID-UNTIL          PIC 9(8) VALUE 0.
           05  WS-ROOT                 PIC X(200) VALUE SPACES.
           05  WS-CMD                  PIC X(400) VALUE SPACES.
           05  WS-SYSRC                PIC S9(9) COMP-5 VALUE 0.
           05  WS-CORR                 PIC X(36) VALUE SPACES.
           05  WS-STEP-CORR            PIC X(36) VALUE SPACES.
      *>   CR2602.  A correlation id names ONE business instruction.
      *>   Work that is keyed by a document (despatch, return) keeps
      *>   WS-CORR; work that creates a brand new document carries the
      *>   run stamp (WS-RUN-CORR) and work on an order carries the
      *>   order number (WS-ORD-CORR).  Without this every run asked
      *>   NUMGEN for the same instruction and was handed back the
      *>   number issued to the first run - core.DocumentNumber keeps
      *>   one number per correlation id - and the insert then broke
      *>   UQ_Quote / UQ_SalesOrder.
           05  WS-RUN-CORR             PIC X(36) VALUE SPACES.
           05  WS-ORD-CORR             PIC X(36) VALUE SPACES.
           05  WS-CALL-RC              PIC 9(2) VALUE 0.
           05  WS-CALL-REASON          PIC X(8) VALUE SPACES.
           05  WS-CALL-MSG             PIC X(200) VALUE SPACES.
           05  WS-FAILED-STEP          PIC X VALUE "N".
           05  WS-OVERRIDE-OK          PIC X VALUE "N".
           05  WS-CREDIT-OK            PIC X VALUE "N".
           05  WS-LAST-ORDER           PIC X(30) VALUE SPACES.
           05  WS-LAST-CUST            PIC X(20) VALUE SPACES.
           05  WS-SEQ-CUST             PIC 9(4) VALUE 0.
           05  WS-TERMS                PIC X(12) VALUE SPACES.
           05  WS-TC-VERSION           PIC X(20) VALUE SPACES.
           05  WS-DELIV-SNAP           PIC X(300) VALUE SPACES.
           05  WS-VAT-BASIS            PIC X(20) VALUE SPACES.
           05  WS-VAT-LOC              PIC X(3) VALUE SPACES.
           05  WS-INCO                 PIC X(3) VALUE SPACES.
           05  WS-INCO-PLACE           PIC X(60) VALUE SPACES.
           05  WS-BX                   PIC S9(4) VALUE 0.
      *>   the company loop - the demonstration dataset holds more
      *>   than one trading company and both of them must show a
      *>   despatch, a cost of sale and a sales invoice
       01  WS-CO-TABLE.
           05  WS-CO-CNT               PIC S9(4) VALUE 0.
           05  WS-CX                   PIC S9(4) VALUE 0.
           05  WS-CO-ROW OCCURS 6 TIMES.
               10  CO-CODE             PIC X(10).
       01  WS-COMPANY-WORK.
           05  WS-CUR-CO               PIC X(10) VALUE SPACES.
           05  WS-SITE                 PIC X(10) VALUE SPACES.
           05  WS-WHS                  PIC X(20) VALUE SPACES.
           05  WS-LOCN                 PIC X(20) VALUE SPACES.
           05  WS-QUAR-LOCN            PIC X(20) VALUE SPACES.
           05  WS-CUSTOMER             PIC X(20) VALUE SPACES.
           05  WS-DEMO-REF             PIC X(40) VALUE SPACES.
           05  WS-ORDER                PIC X(30) VALUE SPACES.
           05  WS-QUOTE                PIC X(30) VALUE SPACES.
           05  WS-PICKLIST             PIC X(30) VALUE SPACES.
           05  WS-DESPATCH             PIC X(30) VALUE SPACES.
           05  WS-RETURN-REF           PIC X(30) VALUE SPACES.
           05  WS-SCENARIO             PIC X(22) VALUE SPACES.
           05  WS-FOUND                PIC X VALUE "N".
           05  WS-SAVE-SITE            PIC X(10) VALUE SPACES.
           05  WS-SAVE-WHS             PIC X(20) VALUE SPACES.
      *>   GUI-005 - Y while 2700 stages the traceable seeded orders:
      *>   the pick confirmation then names the bin and the lot or
      *>   serial actually taken, so DESPATCH can issue from it
           05  WS-NAME-LOT             PIC X VALUE "N".
           05  WS-SRC-QTY              PIC S9(13)V9(6) VALUE 0.
           05  WS-SRC-QTY-ED           PIC Z(9)9.99.
           05  WS-COUNT                PIC S9(9) VALUE 0.
           05  WS-SEQ                  PIC S9(4) VALUE 0.
      *>   one sales order line, read back before any service CALL.
      *>   The SQLIO cursors are a shared resource and every online
      *>   service opens its own, so a cursor is never left open
      *>   across a CALL - each row is read with a single SELECT.
       01  WS-LINE-TABLE.
           05  WS-LN-CNT               PIC S9(4) VALUE 0.
           05  WS-LX                   PIC S9(4) VALUE 0.
           05  WS-LN-ROW OCCURS 20 TIMES.
               10  LN-LINE-NUM         PIC S9(9).
               10  LN-SCHEDULE         PIC S9(9).
               10  LN-ITEM             PIC X(40).
               10  LN-UOM              PIC X(10).
               10  LN-OUTSTANDING      PIC S9(13)V9(6).
               10  LN-AVAILABLE        PIC S9(13)V9(6).
               10  LN-QTY              PIC S9(13)V9(6).
               10  LN-WHSE             PIC X(20).
               10  LN-LOCN             PIC X(20).
               10  LN-LOT              PIC X(40).
       01  WS-ITEM-WORK.
           05  IT-ITEM                 PIC X(40) VALUE SPACES.
           05  IT-DESC                 PIC X(200) VALUE SPACES.
           05  IT-UOM                  PIC X(10) VALUE SPACES.
           05  IT-VAT-CODE             PIC X(4) VALUE SPACES.
           05  IT-PRICE                PIC S9(11)V9(6) VALUE 0.
           05  IT-QTY                  PIC S9(13)V9(6) VALUE 0.
           05  IT-WHSE                 PIC X(20) VALUE SPACES.
           05  IT-LOCN                 PIC X(20) VALUE SPACES.
       01  WS-DET.
           05  D-SEQ                   PIC 9(3) VALUE 0.
           05  D-SCENARIO              PIC X(22) VALUE SPACES.
           05  D-STEP                  PIC X(18) VALUE SPACES.
           05  D-REF                   PIC X(30) VALUE SPACES.
           05  D-AMOUNT                PIC S9(11)V99 VALUE 0.
           05  D-STATUS                PIC X(14) VALUE SPACES.
           05  D-OUTCOME               PIC X(20) VALUE SPACES.
       01  WS-TOT.
           05  T-STEPS                 PIC S9(5) VALUE 0.
           05  T-OK                    PIC S9(5) VALUE 0.
           05  T-WARN                  PIC S9(5) VALUE 0.
           05  T-FAIL                  PIC S9(5) VALUE 0.
           05  T-ORDERS                PIC S9(5) VALUE 0.
           05  T-DESPATCH              PIC S9(5) VALUE 0.
           05  T-VALUE                 PIC S9(13)V99 VALUE 0.
       REPORT SECTION.
       RD  R-SODEMO
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 52
           FOOTING 58.
       COPY STRPTHDR.
       01  PH-COLS TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(3) VALUE "Seq".
               10  COLUMN 6    PIC X(8) VALUE "Scenario".
               10  COLUMN 29   PIC X(4) VALUE "Step".
               10  COLUMN 48   PIC X(9) VALUE "Reference".
               10  COLUMN 86   PIC X(10) VALUE "Value  GBP".
               10  COLUMN 98   PIC X(6) VALUE "Status".
               10  COLUMN 113  PIC X(7) VALUE "Outcome".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC ZZ9 SOURCE D-SEQ.
               10  COLUMN 6    PIC X(22) SOURCE D-SCENARIO.
               10  COLUMN 29   PIC X(18) SOURCE D-STEP.
               10  COLUMN 48   PIC X(30) SOURCE D-REF.
               10  COLUMN 80   PIC ZZZ,ZZZ,ZZ9.99- SOURCE D-AMOUNT.
               10  COLUMN 98   PIC X(14) SOURCE D-STATUS.
               10  COLUMN 113  PIC X(20) SOURCE D-OUTCOME.
       01  DE-NOTE TYPE IS DETAIL.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(120) SOURCE RPT-STATUS-TEXT.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(22) VALUE "Service calls made".
               10  COLUMN 24   PIC ZZZ,ZZ9 SOURCE T-STEPS.
               10  COLUMN 40   PIC X(20) VALUE "Accepted".
               10  COLUMN 62   PIC ZZZ,ZZ9 SOURCE T-OK.
               10  COLUMN 80   PIC ZZZ,ZZZ,ZZ9.99- SOURCE T-VALUE.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(22) VALUE "Warnings".
               10  COLUMN 24   PIC ZZZ,ZZ9 SOURCE T-WARN.
               10  COLUMN 40   PIC X(20) VALUE "Refused".
               10  COLUMN 62   PIC ZZZ,ZZ9 SOURCE T-FAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(22) VALUE "Orders built".
               10  COLUMN 24   PIC ZZZ,ZZ9 SOURCE T-ORDERS.
               10  COLUMN 40   PIC X(20) VALUE "Despatches confirmed".
               10  COLUMN 62   PIC ZZZ,ZZ9 SOURCE T-DESPATCH.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(62) VALUE
                   "Demonstration data only.  Every step was driven".
               10  COLUMN 66   PIC X(62) VALUE
                   "through the online service that owns it.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0200-JOB-START THRU 0200-EXIT
           PERFORM 0300-OPEN-SPOOL THRU 0300-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 1000-PRODUCE THRU 1000-EXIT
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
           MOVE CM-SITE TO WS-SITE-ARG
           MOVE CM-USER TO WS-USER
           MOVE "MODE" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-MODE
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "SALESREP" TO WS-USER
           END-IF
           IF NOT WS-COMMIT
               MOVE "PREVIEW" TO WS-MODE
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           COMPUTE WS-INT-DATE =
               FUNCTION INTEGER-OF-DATE(WS-TODAY-N) + 4
           COMPUTE WS-PROMISE-DATE =
               FUNCTION DATE-OF-INTEGER(WS-INT-DATE)
           COMPUTE WS-INT-DATE =
               FUNCTION INTEGER-OF-DATE(WS-TODAY-N) + 30
           COMPUTE WS-VALID-UNTIL =
               FUNCTION DATE-OF-INTEGER(WS-INT-DATE)
           DISPLAY "STERP-I-1850 SODEMO sales demonstration "
                   FUNCTION TRIM(WS-CO) " mode "
                   FUNCTION TRIM(WS-MODE) UPON SYSOUT.
       0100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "SODEMO" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG.
       0200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "SODEMO" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1851 cannot build the spool path"
                   UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           PERFORM 0370-FIND-ROOT THRU 0370-EXIT
           PERFORM 0350-FILL-HEADINGS THRU 0350-EXIT
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1852 cannot open " WS-SPOOL-NAME
                   UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           INITIATE R-SODEMO.
       0300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "RPT-011" TO RPT-REPORT-ID
           MOVE "Sales demonstration transaction generator"
               TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE SPACES TO RPT-SUBTITLE
           IF WS-COMMIT
               STRING "MODE=COMMIT - every step is driven through "
                      "its online service"
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           ELSE
               STRING "MODE=PREVIEW - the plan only, nothing is "
                      "written"
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           END-IF
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "As at " WS-NOW(7:2) "/" WS-NOW(5:2) "/"
                  WS-NOW(1:4)
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           PERFORM 0360-READ-COMPANY THRU 0360-EXIT.
       0350-EXIT.
           EXIT.
      *>----------------------------------------------------------------
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
           PERFORM 8900-SET-WIDTHS THRU 8900-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SD-ROW
           IF SQLIO-OK
               MOVE SD-COL(1) TO RPT-COMPANY-NAME
               MOVE SD-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> The run directory is the parent of run\spool, which RUNPATH
      *> has just built.  The invoice run is spawned from there.
      *>----------------------------------------------------------------
       0370-FIND-ROOT SECTION.
       0370-START.
           MOVE "." TO WS-ROOT
           PERFORM VARYING WS-IX FROM 180 BY -1 UNTIL WS-IX < 2
               IF WS-SPOOL-NAME(WS-IX:5) = "\run\"
                   MOVE SPACES TO WS-ROOT
                   MOVE WS-SPOOL-NAME(1:WS-IX - 1) TO WS-ROOT
                   MOVE 1 TO WS-IX
               END-IF
           END-PERFORM.
       0370-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-SODEMO
           CLOSE RPT-FILE
           DISPLAY "STERP-I-1853 " T-STEPS " service call(s), "
                   T-OK " accepted, " T-WARN " warned, "
                   T-FAIL " refused" UPON SYSOUT
           DISPLAY "STERP-I-1854 " T-ORDERS " order(s) built, "
                   T-DESPATCH " despatch(es) confirmed" UPON SYSOUT
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME)
               UPON SYSOUT.
       0400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "SODEMO" TO JL-JOB-NAME
           EVALUATE TRUE
               WHEN WS-RC = 0
                   MOVE "OK" TO JL-STATUS
               WHEN WS-RC = 4
                   MOVE "WARNING" TO JL-STATUS
               WHEN OTHER
                   MOVE "FAILED" TO JL-STATUS
           END-EVALUATE
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE T-STEPS TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> The demonstration itself, company by company.
      *>----------------------------------------------------------------
       1000-PRODUCE SECTION.
       1000-START.
           GENERATE PH-COLS
           PERFORM 1010-COMPANY-LIST THRU 1010-EXIT
           PERFORM VARYING WS-CX FROM 1 BY 1
                   UNTIL WS-CX > WS-CO-CNT
               MOVE CO-CODE(WS-CX) TO WS-CUR-CO
               PERFORM 1100-ONE-COMPANY THRU 1100-EXIT
           END-PERFORM
           IF NOT WS-COMMIT
               MOVE SPACES TO RPT-STATUS-TEXT
               STRING "PREVIEW only - run with MODE=COMMIT to "
                      "despatch, post the cost of sale and invoice."
                   DELIMITED BY SIZE INTO RPT-STATUS-TEXT
               GENERATE DE-NOTE
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> The company named on the command line comes first; every
      *> other trading company that has sales orders follows, so one
      *> run of the dataset leaves both ledgers with a despatch, a
      *> cost of sale and a sales invoice.
      *>----------------------------------------------------------------
       1010-COMPANY-LIST SECTION.
       1010-START.
           MOVE 1 TO WS-CO-CNT
           MOVE WS-CO TO CO-CODE(1)
      *>   GUI-005 - the search walks every company from the start of
      *>   the alphabet; starting it at WS-CO skipped any company that
      *>   sorts before it, so CO=STC never reached CAL
           MOVE SPACES TO WS-CUR-CO
           PERFORM UNTIL WS-CO-CNT >= 6
               MOVE SPACES TO SQLIO-STMT
               STRING "SELECT TOP 1 c.CompanyCode FROM core.Company "
                      "c WHERE c.CompanyCode > ? AND c.CompanyCode "
                      "<> ? AND EXISTS (SELECT 1 FROM so.SalesOrder "
                      "o WHERE o.CompanyCode = c.CompanyCode) "
                      "ORDER BY c.CompanyCode"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 2 TO SQLIO-PARM-CNT
               MOVE WS-CUR-CO TO SQLIO-PARM-VAL(1)
               MOVE WS-CO TO SQLIO-PARM-VAL(2)
               MOVE "N" TO SQLIO-PARM-NULL(1)
               MOVE "N" TO SQLIO-PARM-NULL(2)
               MOVE 1 TO SQLIO-COL-CNT
               PERFORM 8900-SET-WIDTHS THRU 8900-EXIT
               MOVE 8 TO SQLIO-CURSOR
               MOVE "SNGL" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA SD-ROW
               IF SQLIO-OK AND SD-COL(1) NOT = SPACES
                   ADD 1 TO WS-CO-CNT
                   MOVE SD-COL(1) TO CO-CODE(WS-CO-CNT)
                   MOVE SD-COL(1) TO WS-CUR-CO
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM.
       1010-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1100-ONE-COMPANY SECTION.
       1100-START.
           MOVE SPACES TO WS-ORDER WS-QUOTE WS-PICKLIST
           MOVE SPACES TO WS-DESPATCH WS-RETURN-REF
           MOVE SPACES TO WS-CORR
           STRING "SODEMO-" FUNCTION TRIM(WS-CUR-CO)
               DELIMITED BY SIZE INTO WS-CORR
           MOVE SPACES TO WS-DEMO-REF
           STRING "SODEMO-" FUNCTION TRIM(WS-CUR-CO)
               DELIMITED BY SIZE INTO WS-DEMO-REF
           MOVE SPACES TO WS-RUN-CORR
           STRING "SODEMO-" FUNCTION TRIM(WS-CUR-CO) "-"
                  WS-NOW(1:14)
               DELIMITED BY SIZE INTO WS-RUN-CORR
           MOVE SPACES TO WS-ORD-CORR
           PERFORM 1110-LOAD-SITE THRU 1110-EXIT
           PERFORM 2000-PICKED-ORDERS THRU 2000-EXIT
           PERFORM 2700-STAGE-PICKED THRU 2700-EXIT
           PERFORM 3000-QUOTE-TO-ORDER THRU 3000-EXIT
           PERFORM 7000-INVOICE-RUN THRU 7000-EXIT
           PERFORM 7500-CUSTOMER-RETURN THRU 7500-EXIT.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1110-LOAD-SITE SECTION.
       1110-START.
           MOVE SPACES TO WS-SITE WS-WHS WS-LOCN WS-QUAR-LOCN
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 w.SiteCode, w.WarehouseCode, "
                  "ISNULL((SELECT TOP 1 l.LocationCode FROM "
                  "inv.Location l WHERE l.CompanyCode = "
                  "w.CompanyCode AND l.WarehouseCode = "
                  "w.WarehouseCode AND l.IsAvailable = 1 AND "
                  "l.IsOwnedStock = 1 ORDER BY l.LocationCode),''), "
                  "ISNULL((SELECT TOP 1 q.LocationCode FROM "
                  "inv.Location q WHERE q.CompanyCode = "
                  "w.CompanyCode AND q.WarehouseCode = "
                  "w.WarehouseCode AND q.LocationType IN "
                  "('QUARANTINE', 'INSPECTION') ORDER BY "
                  "q.LocationCode),'') FROM inv.Warehouse w WHERE "
                  "w.CompanyCode = ? AND (? = '' OR w.SiteCode = ?) "
                  "ORDER BY CASE WHEN w.WarehouseCode = 'MAIN' "
                  "THEN 0 ELSE 1 END, w.SiteCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE WS-CUR-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-SITE-ARG TO SQLIO-PARM-VAL(2)
           MOVE WS-SITE-ARG TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 4 TO SQLIO-COL-CNT
           PERFORM 8900-SET-WIDTHS THRU 8900-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SD-ROW
           IF SQLIO-OK
               MOVE SD-COL(1) TO WS-SITE
               MOVE SD-COL(2) TO WS-WHS
               MOVE SD-COL(3) TO WS-LOCN
               MOVE SD-COL(4) TO WS-QUAR-LOCN
           END-IF
           IF WS-WHS = SPACES
               MOVE "MAIN" TO WS-WHS
           END-IF
           IF WS-QUAR-LOCN = SPACES
               MOVE WS-LOCN TO WS-QUAR-LOCN
           END-IF.
       1110-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> SCENARIO 1 - the orders the seed leaves picked and billing
      *> eligible.  A despatch is an accounting event: it reduces
      *> stock and posts the cost of sale once.  The seed never
      *> writes one, so this is where they come from.
      *>----------------------------------------------------------------
       2000-PICKED-ORDERS SECTION.
       2000-START.
           MOVE "Picked order to cash" TO WS-SCENARIO
           MOVE SPACES TO WS-LAST-ORDER
           PERFORM 6 TIMES
               PERFORM 2100-NEXT-PICKED THRU 2100-EXIT
               IF WS-FOUND = "Y"
                   PERFORM 2300-CREDIT-CHECK THRU 2300-EXIT
                   IF WS-CREDIT-OK = "Y"
                       PERFORM 2500-DESPATCH-ORDER THRU 2500-EXIT
                   END-IF
               END-IF
           END-PERFORM.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2100-NEXT-PICKED SECTION.
       2100-START.
           MOVE "N" TO WS-FOUND
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 o.OrderNumber, o.CustomerCode, "
                  "ISNULL(o.SiteCode,'') FROM so.SalesOrder o "
                  "WHERE o.CompanyCode = ? AND o.CommercialStatus "
                  "IN ('ACCEPTED','AMENDED') AND o.ReleaseStatus = "
                  "'RELEASED' AND o.FulfilmentStatus IN "
                  "('PICKED','PARTPICKED') AND NOT EXISTS (SELECT 1 "
                  "FROM so.Despatch d WHERE d.CompanyCode = "
                  "o.CompanyCode AND d.OrderNumber = o.OrderNumber) "
      *>           a seeded picked order whose item is lot or serial
      *>           controlled is left alone: the demonstration cannot
      *>           name a lot that agrees with the reservation the
      *>           seed made, and DESPATCH rightly refuses (DSP0004).
      *>           The chain is shown end to end by scenario 2, which
      *>           builds, reserves and picks its own order.
                  "AND NOT EXISTS (SELECT 1 FROM so.SalesOrderLine "
                  "sl JOIN inv.Item it ON it.CompanyCode = "
                  "o.CompanyCode AND it.ItemCode = sl.ItemCode "
                  "WHERE sl.SalesOrderId = o.SalesOrderId AND "
                  "sl.Status NOT IN ('CANCELLED','CLOSED') AND "
                  "ISNULL(it.TraceabilityType,'NONE') <> 'NONE') "
                  "AND o.OrderNumber > ? ORDER BY o.OrderNumber"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CUR-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-LAST-ORDER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 3 TO SQLIO-COL-CNT
           PERFORM 8900-SET-WIDTHS THRU 8900-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SD-ROW
           IF SQLIO-OK AND SD-COL(1) NOT = SPACES
               MOVE "Y" TO WS-FOUND
               MOVE SD-COL(1) TO WS-ORDER
               MOVE SD-COL(1) TO WS-LAST-ORDER
               MOVE SD-COL(2) TO WS-CUSTOMER
               IF SD-COL(3) NOT = SPACES
                   MOVE SD-COL(3) TO WS-SITE
               END-IF
           END-IF.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> SO-007 / D-06.  The credit position is asked for before the
      *> despatch is built, so an account that is properly held is
      *> reported as held and the next order is taken - the control
      *> is never worked around.
      *>----------------------------------------------------------------
       2300-CREDIT-CHECK SECTION.
       2300-START.
           MOVE "N" TO WS-CREDIT-OK
           INITIALIZE WS-CRD
           MOVE WS-CUR-CO TO CA-COMPANY OF WS-CRD
           MOVE WS-SITE TO CA-SITE OF WS-CRD
           MOVE WS-USER TO CA-USER OF WS-CRD
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-CRD
           MOVE WS-CORR TO CA-CORRELATION-ID OF WS-CRD
           MOVE "CHECK" TO CA-FUNCTION OF WS-CRD
           MOVE WS-CUSTOMER TO CK-CUSTOMER
           MOVE "DESPATCH" TO CK-CHECK-POINT
           MOVE WS-ORDER TO CK-DOCUMENT-REF
           MOVE WS-TODAY-N TO CK-EVAL-DATE
           MOVE "GBP" TO CK-CURRENCY
           MOVE 1 TO CK-FX-RATE
           MOVE WS-TODAY-N TO CK-RATE-DATE
           CALL "CREDCHK" USING WS-CRD
           IF CA-RETURN-CODE OF WS-CRD < 6
               MOVE "Y" TO WS-CREDIT-OK
           ELSE
               MOVE CA-RETURN-CODE OF WS-CRD TO WS-CALL-RC
               MOVE CA-REASON-CODE OF WS-CRD TO WS-CALL-REASON
               MOVE CK-DECISION-TEXT TO WS-CALL-MSG
               MOVE "credit check" TO D-STEP
               MOVE WS-ORDER TO D-REF
               MOVE CK-OVERDUE-GBP TO D-AMOUNT
               PERFORM 8100-LOG-CALL THRU 8100-EXIT
           END-IF.
       2300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> Read the open lines of WS-ORDER and the delivery schedule
      *> each one belongs to.  The schedule number travels all the
      *> way into so.DespatchLine - without it the service measure
      *> cannot tell which promise was met (RPT OTIF).
      *>----------------------------------------------------------------
       2200-READ-LINES SECTION.
       2200-START.
           MOVE 0 TO WS-LN-CNT
           MOVE 0 TO WS-COUNT
           PERFORM 20 TIMES
               MOVE SPACES TO SQLIO-STMT
               STRING "SELECT TOP 1 CONVERT(varchar(12), l.LineNum), "
                      "l.ItemCode, ISNULL(l.UomCode,'EA'), "
                      "CONVERT(varchar(24), l.AcceptedQty - "
                      "l.CancelledQty - l.DespatchedQty), "
                      "CONVERT(varchar(12), ISNULL((SELECT TOP 1 "
                      "s.ScheduleNo FROM so.DeliverySchedule s WHERE "
                      "s.CompanyCode = o.CompanyCode AND "
                      "s.OrderNumber = o.OrderNumber AND s.LineNum = "
                      "l.LineNum AND s.Status <> 'CANCELLED' ORDER "
                      "BY s.ScheduleNo),0)), ISNULL((SELECT TOP 1 "
                      "b.WarehouseCode FROM inv.StockBalance b JOIN "
                      "inv.Location v ON v.CompanyCode = "
                      "b.CompanyCode AND v.SiteCode = b.SiteCode AND "
                      "v.WarehouseCode = b.WarehouseCode AND "
                      "v.LocationCode = b.LocationCode WHERE "
                      "b.CompanyCode = o.CompanyCode AND b.ItemCode "
                      "= l.ItemCode AND b.QualityStatus = 'ACCEPTED' "
                      "AND b.OwnerType = 'OWNED' AND v.IsAvailable = "
                      "1 AND v.IsOwnedStock = 1 AND b.QtyOnHand > 0 "
                      "ORDER BY b.QtyOnHand DESC),''), "
                      "ISNULL((SELECT TOP 1 b.LocationCode FROM "
                      "inv.StockBalance b JOIN "
                      "inv.Location v ON v.CompanyCode = "
                      "b.CompanyCode AND v.SiteCode = b.SiteCode AND "
                      "v.WarehouseCode = b.WarehouseCode AND "
                      "v.LocationCode = b.LocationCode WHERE "
                      "b.CompanyCode = o.CompanyCode AND b.ItemCode "
                      "= l.ItemCode AND b.QualityStatus = 'ACCEPTED' "
                      "AND b.OwnerType = 'OWNED' AND v.IsAvailable = "
                      "1 AND v.IsOwnedStock = 1 AND b.QtyOnHand > 0 "
                      "ORDER BY b.QtyOnHand DESC),''), "
                      "CONVERT(varchar(24), ISNULL((SELECT TOP 1 "
                      "b.QtyOnHand FROM inv.StockBalance b JOIN "
                      "inv.Location v ON v.CompanyCode = "
                      "b.CompanyCode AND v.SiteCode = b.SiteCode AND "
                      "v.WarehouseCode = b.WarehouseCode AND "
                      "v.LocationCode = b.LocationCode WHERE "
                      "b.CompanyCode = o.CompanyCode AND b.ItemCode "
                      "= l.ItemCode AND b.QualityStatus = 'ACCEPTED' "
                      "AND b.OwnerType = 'OWNED' AND v.IsAvailable = "
                      "1 AND v.IsOwnedStock = 1 AND b.QtyOnHand > 0 "
                      "ORDER BY b.QtyOnHand DESC),0)), "
      *>           a lot controlled item may not move without its
      *>           lot being named (DSP0004), so the balance that
      *>           supplies the warehouse and the location supplies
      *>           the lot as well
                      "ISNULL((SELECT TOP 1 ISNULL(b.LotCode,'') "
                      "FROM inv.StockBalance b JOIN "
                      "inv.Location v ON v.CompanyCode = "
                      "b.CompanyCode AND v.SiteCode = b.SiteCode AND "
                      "v.WarehouseCode = b.WarehouseCode AND "
                      "v.LocationCode = b.LocationCode WHERE "
                      "b.CompanyCode = o.CompanyCode AND b.ItemCode "
                      "= l.ItemCode AND b.QualityStatus = 'ACCEPTED' "
                      "AND b.OwnerType = 'OWNED' AND v.IsAvailable = "
                      "1 AND v.IsOwnedStock = 1 AND b.QtyOnHand > 0 "
                      "ORDER BY b.QtyOnHand DESC),'') "
                      "FROM so.SalesOrderLine l JOIN so.SalesOrder o "
                      "ON o.SalesOrderId = l.SalesOrderId WHERE "
                      "o.CompanyCode = ? AND o.OrderNumber = ? AND "
                      "l.Status NOT IN ('CANCELLED','CLOSED') AND "
                      "l.AcceptedQty - l.CancelledQty - "
                      "l.DespatchedQty > 0 AND l.LineNum > ? "
                      "ORDER BY l.LineNum"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 3 TO SQLIO-PARM-CNT
               MOVE WS-CUR-CO TO SQLIO-PARM-VAL(1)
               MOVE WS-ORDER TO SQLIO-PARM-VAL(2)
               MOVE SPACES TO SQLIO-PARM-VAL(3)
               MOVE WS-COUNT TO SQLIO-PARM-VAL(3)
               MOVE "N" TO SQLIO-PARM-NULL(1)
               MOVE "N" TO SQLIO-PARM-NULL(2)
               MOVE "N" TO SQLIO-PARM-NULL(3)
      *>       eight columns are selected and SD-COL(8) carries the
      *>       saleable quantity; reading only seven of them left
      *>       every line with nothing behind it, so LN-QTY came out
      *>       at zero and the demand was dropped before it reached
      *>       SORESV, PICKGEN and DESPATCH.  SD-COL(9) carries the
      *>       lot of the chosen balance.
               MOVE 9 TO SQLIO-COL-CNT
               PERFORM 8900-SET-WIDTHS THRU 8900-EXIT
               MOVE 8 TO SQLIO-CURSOR
               MOVE "SNGL" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA SD-ROW
               IF SQLIO-OK AND SD-COL(1) NOT = SPACES
                   ADD 1 TO WS-LN-CNT
                   COMPUTE WS-COUNT = FUNCTION NUMVAL(SD-COL(1))
                   MOVE WS-COUNT TO LN-LINE-NUM(WS-LN-CNT)
                   MOVE SD-COL(2) TO LN-ITEM(WS-LN-CNT)
                   MOVE SD-COL(3) TO LN-UOM(WS-LN-CNT)
                   COMPUTE LN-OUTSTANDING(WS-LN-CNT) =
                       FUNCTION NUMVAL(SD-COL(4))
                   COMPUTE LN-SCHEDULE(WS-LN-CNT) =
                       FUNCTION NUMVAL(SD-COL(5))
                   MOVE SD-COL(6) TO LN-WHSE(WS-LN-CNT)
                   MOVE SD-COL(7) TO LN-LOCN(WS-LN-CNT)
                   MOVE SD-COL(9) TO LN-LOT(WS-LN-CNT)
                   COMPUTE LN-AVAILABLE(WS-LN-CNT) =
                       FUNCTION NUMVAL(SD-COL(8))
                   IF LN-AVAILABLE(WS-LN-CNT) <
                      LN-OUTSTANDING(WS-LN-CNT)
                       MOVE LN-AVAILABLE(WS-LN-CNT)
                           TO LN-QTY(WS-LN-CNT)
                   ELSE
                       MOVE LN-OUTSTANDING(WS-LN-CNT)
                           TO LN-QTY(WS-LN-CNT)
                   END-IF
      *>           a line with no saleable stock behind it is left
      *>           on back order rather than despatched short of
      *>           anything at all
                   IF LN-QTY(WS-LN-CNT) <= 0
                       SUBTRACT 1 FROM WS-LN-CNT
                   END-IF
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> DESPATCH CONFIRM - the revenue and control event.  One
      *> transaction: stock down, reservation consumed, cost of sale
      *> posted ONCE under the correlation id.
      *>----------------------------------------------------------------
       2500-DESPATCH-ORDER SECTION.
       2500-START.
           PERFORM 2200-READ-LINES THRU 2200-EXIT
           IF WS-LN-CNT = 0
               MOVE "nothing outstanding" TO D-OUTCOME
               MOVE "despatch" TO D-STEP
               MOVE WS-ORDER TO D-REF
               MOVE 0 TO D-AMOUNT
               MOVE "SKIPPED" TO D-STATUS
               PERFORM 8000-LOG-LINE THRU 8000-EXIT
               GO TO 2500-EXIT
           END-IF
           IF NOT WS-COMMIT
               MOVE "preview only" TO D-OUTCOME
               MOVE "despatch" TO D-STEP
               MOVE WS-ORDER TO D-REF
               MOVE 0 TO D-AMOUNT
               MOVE "PLANNED" TO D-STATUS
               PERFORM 8000-LOG-LINE THRU 8000-EXIT
               GO TO 2500-EXIT
           END-IF
           PERFORM 2600-SEND-DESPATCH THRU 2600-EXIT
      *>   SO-007 - a despatch held because the limit is exceeded is
      *>   not a failure of the demonstration, it is the control
      *>   working.  The credit controller grants a scoped, expiring
      *>   override and the despatch is re-presented; an overdue
      *>   account has no excess to override and stays held.
           IF WS-CALL-RC = 6 AND WS-CALL-REASON = "DSP0006"
               PERFORM 8100-LOG-CALL THRU 8100-EXIT
               PERFORM 2400-CREDIT-OVERRIDE THRU 2400-EXIT
               IF WS-OVERRIDE-OK = "Y"
                   PERFORM 2600-SEND-DESPATCH THRU 2600-EXIT
               END-IF
           END-IF
           MOVE CA-RETURN-CODE OF WS-DSP TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-DSP TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-DSP TO WS-CALL-MSG
           MOVE "despatch confirm" TO D-STEP
           MOVE DS-DESPATCH-REF TO D-REF
           MOVE DS-COGS-GBP TO D-AMOUNT
           IF WS-CALL-RC < 8 AND DS-DESPATCH-REF NOT = SPACES
               MOVE DS-DESPATCH-REF TO WS-DESPATCH
               ADD 1 TO T-DESPATCH
               ADD DS-COGS-GBP TO T-VALUE
           END-IF
           PERFORM 8100-LOG-CALL THRU 8100-EXIT.
       2500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> The credit controller's scoped override (AR-008, D-06).  It
      *> is requested with a reason and approved by a second pair of
      *> eyes - CREDCHK owns both halves, this job only asks.
      *>----------------------------------------------------------------
       2400-CREDIT-OVERRIDE SECTION.
       2400-START.
           MOVE "N" TO WS-OVERRIDE-OK
           INITIALIZE WS-CRD
           MOVE WS-CUR-CO TO CA-COMPANY OF WS-CRD
           MOVE WS-SITE TO CA-SITE OF WS-CRD
           MOVE WS-USER TO CA-USER OF WS-CRD
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-CRD
           MOVE WS-CORR TO CA-CORRELATION-ID OF WS-CRD
           MOVE "OVERRIDE" TO CA-FUNCTION OF WS-CRD
           MOVE WS-CUSTOMER TO CK-CUSTOMER
           MOVE "DESPATCH" TO CK-CHECK-POINT
           MOVE WS-ORDER TO CK-DOCUMENT-REF
           MOVE WS-TODAY-N TO CK-EVAL-DATE
           MOVE "GBP" TO CK-CURRENCY
           MOVE 1 TO CK-FX-RATE
           MOVE WS-TODAY-N TO CK-RATE-DATE
           MOVE "Demonstration dataset - released on a payment plan"
               TO CK-OVERRIDE-REASON
           CALL "CREDCHK" USING WS-CRD
           MOVE CA-RETURN-CODE OF WS-CRD TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-CRD TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-CRD TO WS-CALL-MSG
           MOVE "credit override" TO D-STEP
           MOVE WS-ORDER TO D-REF
           MOVE CK-OVERRIDE-AMT TO D-AMOUNT
           PERFORM 8100-LOG-CALL THRU 8100-EXIT
           IF WS-CALL-RC > 4 OR CK-OVERRIDE-ID = 0
               GO TO 2400-EXIT
           END-IF
           MOVE "APPROVE" TO CA-FUNCTION OF WS-CRD
           MOVE "CREDCTRL" TO CA-USER OF WS-CRD
           MOVE "CREDCTRL" TO CK-OVERRIDE-APPR
           CALL "CREDCHK" USING WS-CRD
           IF CA-RETURN-CODE OF WS-CRD > 4
               MOVE "APPROVE" TO CA-FUNCTION OF WS-CRD
               MOVE "ARCLERK" TO CA-USER OF WS-CRD
               MOVE "ARCLERK" TO CK-OVERRIDE-APPR
               CALL "CREDCHK" USING WS-CRD
           END-IF
           MOVE CA-RETURN-CODE OF WS-CRD TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-CRD TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-CRD TO WS-CALL-MSG
           MOVE "override approve" TO D-STEP
           MOVE WS-ORDER TO D-REF
           MOVE CK-OVERRIDE-AMT TO D-AMOUNT
           IF WS-CALL-RC < 8
               MOVE "Y" TO WS-OVERRIDE-OK
           END-IF
           PERFORM 8100-LOG-CALL THRU 8100-EXIT.
       2400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2600-SEND-DESPATCH SECTION.
       2600-START.
           INITIALIZE WS-DSP
           MOVE WS-CUR-CO TO CA-COMPANY OF WS-DSP
           MOVE WS-SITE TO CA-SITE OF WS-DSP
           MOVE WS-USER TO CA-USER OF WS-DSP
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-DSP
           MOVE SPACES TO WS-STEP-CORR
           STRING FUNCTION TRIM(WS-CORR) "-D-"
                  FUNCTION TRIM(WS-ORDER)
               DELIMITED BY SIZE INTO WS-STEP-CORR
           MOVE WS-STEP-CORR TO CA-CORRELATION-ID OF WS-DSP
           MOVE "CONFIRM" TO CA-FUNCTION OF WS-DSP
           MOVE WS-ORDER TO DS-ORDER-NUMBER
           MOVE WS-CUSTOMER TO DS-CUSTOMER
           MOVE WS-SITE TO DS-SITE
           MOVE WS-TODAY-N TO DS-DESPATCH-DATE
           MOVE WS-TODAY-N TO DS-POSTING-DATE
           MOVE WS-LN-CNT TO DS-LINE-CNT
           PERFORM VARYING WS-LX FROM 1 BY 1 UNTIL WS-LX > WS-LN-CNT
               MOVE LN-LINE-NUM(WS-LX) TO DS-L-ORDER-LINE(WS-LX)
               MOVE LN-SCHEDULE(WS-LX) TO DS-L-SCHEDULE(WS-LX)
               MOVE LN-ITEM(WS-LX) TO DS-L-ITEM(WS-LX)
               MOVE LN-UOM(WS-LX) TO DS-L-UOM(WS-LX)
               MOVE LN-QTY(WS-LX) TO DS-L-QTY(WS-LX)
               MOVE LN-QTY(WS-LX) TO DS-L-PICKED(WS-LX)
               MOVE LN-WHSE(WS-LX) TO DS-L-WHSE(WS-LX)
               MOVE LN-LOCN(WS-LX) TO DS-L-LOCATION(WS-LX)
               MOVE LN-LOT(WS-LX) TO DS-L-LOT(WS-LX)
           END-PERFORM
           CALL "DESPATCH" USING WS-DSP
           MOVE CA-RETURN-CODE OF WS-DSP TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-DSP TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-DSP TO WS-CALL-MSG
           MOVE "despatch confirm" TO D-STEP
           MOVE DS-DESPATCH-REF TO D-REF
           MOVE DS-COGS-GBP TO D-AMOUNT.
       2600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> SCENARIO 1B - GUI-005.  The seeded picked orders that scenario
      *> 1 leaves alone (their items are lot or serial controlled) are
      *> the ones the demonstration despatches on the screen, and a
      *> despatch of a traceable item needs a pick list that names the
      *> lot or serial.  The seed cannot invent one, so it is made the
      *> honest way: SORESV reserves the stock (and so chooses the lot
      *> or serial), PICKGEN generates and confirms the pick.  Nothing
      *> is despatched here - picking moves no stock and posts nothing.
      *>----------------------------------------------------------------
       2700-STAGE-PICKED SECTION.
       2700-START.
           MOVE "Ready to despatch" TO WS-SCENARIO
           MOVE WS-SITE TO WS-SAVE-SITE
           MOVE WS-WHS TO WS-SAVE-WHS
           MOVE SPACES TO WS-LAST-ORDER
           PERFORM 4 TIMES
               PERFORM 2710-NEXT-UNPICKED THRU 2710-EXIT
               IF WS-FOUND = "Y"
                   IF WS-COMMIT
                       MOVE SPACES TO WS-ORD-CORR
                       STRING "SODEMO-" FUNCTION TRIM(WS-CUR-CO) "-P-"
                              FUNCTION TRIM(WS-ORDER)
                           DELIMITED BY SIZE INTO WS-ORD-CORR
                       PERFORM 5000-RESERVE THRU 5000-EXIT
                       MOVE "Y" TO WS-NAME-LOT
                       PERFORM 6000-PICK THRU 6000-EXIT
                       MOVE "N" TO WS-NAME-LOT
                   ELSE
                       MOVE "pick list" TO D-STEP
                       MOVE WS-ORDER TO D-REF
                       MOVE 0 TO D-AMOUNT
                       MOVE "PLANNED" TO D-STATUS
                       MOVE "preview only" TO D-OUTCOME
                       PERFORM 8000-LOG-LINE THRU 8000-EXIT
                   END-IF
               END-IF
           END-PERFORM
           MOVE WS-SAVE-SITE TO WS-SITE
           MOVE WS-SAVE-WHS TO WS-WHS.
       2700-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A seeded, released, picked order with no despatch and no pick
      *> list yet, with the main warehouse of its own site (the run's
      *> SITE= argument may name another site).
      *>----------------------------------------------------------------
       2710-NEXT-UNPICKED SECTION.
       2710-START.
           MOVE "N" TO WS-FOUND
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 o.OrderNumber, o.CustomerCode, "
                  "o.SiteCode, ISNULL((SELECT TOP 1 w.WarehouseCode "
                  "FROM inv.Warehouse w WHERE w.CompanyCode = "
                  "o.CompanyCode AND w.SiteCode = o.SiteCode ORDER "
                  "BY CASE WHEN w.WarehouseCode = 'MAIN' THEN 0 "
                  "ELSE 1 END, w.WarehouseCode),'MAIN') "
                  "FROM so.SalesOrder o WHERE o.CompanyCode = ? "
                  "AND o.CreatedBy = 'SEED' AND o.CommercialStatus "
                  "IN ('ACCEPTED','AMENDED') AND o.ReleaseStatus = "
                  "'RELEASED' AND o.FulfilmentStatus = 'PICKED' "
                  "AND NOT EXISTS (SELECT 1 FROM so.Despatch d WHERE "
                  "d.CompanyCode = o.CompanyCode AND d.OrderNumber = "
                  "o.OrderNumber) AND NOT EXISTS (SELECT 1 FROM "
                  "so.PickList p WHERE p.CompanyCode = o.CompanyCode "
                  "AND p.OrderNumber = o.OrderNumber AND p.Status <> "
                  "'CANCELLED') AND o.OrderNumber > ? "
                  "ORDER BY o.OrderNumber"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CUR-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-LAST-ORDER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 4 TO SQLIO-COL-CNT
           PERFORM 8900-SET-WIDTHS THRU 8900-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SD-ROW
           IF SQLIO-OK AND SD-COL(1) NOT = SPACES
               MOVE "Y" TO WS-FOUND
               MOVE SD-COL(1) TO WS-ORDER
               MOVE SD-COL(1) TO WS-LAST-ORDER
               MOVE SD-COL(2) TO WS-CUSTOMER
               MOVE SD-COL(3) TO WS-SITE
               MOVE SD-COL(4) TO WS-WHS
           END-IF.
       2710-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> The bin and the lot or serial the picker actually takes for
      *> pick line WS-LX: an owned, accepted balance at the order's
      *> site and warehouse, preferring one that covers the whole
      *> quantity.  A despatch line issues from one lot or serial, so
      *> when no single balance covers it the rest is recorded as a
      *> reasoned short pick and stays on back order (CR1051).
      *>----------------------------------------------------------------
       2730-PICK-SOURCE SECTION.
       2730-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 b.LocationCode, ISNULL(b.LotCode,''), "
                  "ISNULL(b.SerialNo,''), CONVERT(varchar(30), "
                  "b.QtyOnHand) FROM inv.StockBalance b JOIN "
                  "inv.Location v ON v.CompanyCode = b.CompanyCode "
                  "AND v.SiteCode = b.SiteCode AND v.WarehouseCode = "
                  "b.WarehouseCode AND v.LocationCode = b.LocationCode "
                  "WHERE b.CompanyCode = ? AND b.SiteCode = ? AND "
                  "b.WarehouseCode = ? AND b.ItemCode = ? AND "
                  "b.QualityStatus = 'ACCEPTED' AND b.OwnerType = "
                  "'OWNED' AND v.IsAvailable = 1 AND v.IsOwnedStock = "
                  "1 AND b.QtyOnHand > 0 ORDER BY CASE WHEN "
                  "b.QtyOnHand >= CONVERT(decimal(19,6), ?) THEN 0 "
                  "ELSE 1 END, b.QtyOnHand DESC, b.LotCode, b.SerialNo"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE WS-CUR-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-SITE TO SQLIO-PARM-VAL(2)
           MOVE WS-WHS TO SQLIO-PARM-VAL(3)
           MOVE LN-ITEM(WS-LX) TO SQLIO-PARM-VAL(4)
           MOVE LN-QTY(WS-LX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 4 TO SQLIO-COL-CNT
           PERFORM 8900-SET-WIDTHS THRU 8900-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SD-ROW
           IF NOT SQLIO-OK OR SD-COL(1) = SPACES
               GO TO 2730-EXIT
           END-IF
           MOVE SD-COL(1) TO PK-LOCATION
           MOVE SD-COL(2) TO PK-LOT-CODE
           MOVE SD-COL(3) TO PK-SERIAL-NO
           COMPUTE WS-SRC-QTY = FUNCTION NUMVAL(SD-COL(4))
           IF WS-SRC-QTY < LN-QTY(WS-LX)
               MOVE WS-SRC-QTY TO PK-PICKED-QTY
               COMPUTE PK-SHORT-QTY = LN-QTY(WS-LX) - WS-SRC-QTY
               MOVE WS-SRC-QTY TO WS-SRC-QTY-ED
               MOVE SPACES TO PK-SHORT-REASON
               STRING "One despatch line issues from one lot or "
                      "serial; "
                      FUNCTION TRIM(SD-COL(2)) FUNCTION TRIM(SD-COL(3))
                      " in " FUNCTION TRIM(SD-COL(1)) " holds "
                      FUNCTION TRIM(WS-SRC-QTY-ED)
                      " - the balance stays on back order"
                   DELIMITED BY SIZE INTO PK-SHORT-REASON
           END-IF.
       2730-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> SCENARIO 2 - the whole order to cash chain, built from
      *> nothing: quotation, acceptance, credit decision, delivery
      *> promise, reservation, pick list, despatch.  The converted
      *> order carries customer reference SODEMO-<CO>, which is how a
      *> second run recognises its own work.
      *>----------------------------------------------------------------
       3000-QUOTE-TO-ORDER SECTION.
       3000-START.
           MOVE "Quote to cash" TO WS-SCENARIO
           PERFORM 3100-FIND-DEMO-ORDER THRU 3100-EXIT
           IF WS-ORDER = SPACES
               PERFORM 3200-CHOOSE-CUSTOMER THRU 3200-EXIT
               PERFORM 3300-CHOOSE-ITEM THRU 3300-EXIT
               IF WS-CUSTOMER = SPACES OR IT-ITEM = SPACES
                   MOVE "quote" TO D-STEP
                   MOVE SPACES TO D-REF
                   MOVE 0 TO D-AMOUNT
                   MOVE "SKIPPED" TO D-STATUS
                   MOVE "no saleable item" TO D-OUTCOME
                   PERFORM 8000-LOG-LINE THRU 8000-EXIT
                   GO TO 3000-EXIT
               END-IF
               IF NOT WS-COMMIT
                   MOVE "quote" TO D-STEP
                   MOVE IT-ITEM TO D-REF
                   COMPUTE D-AMOUNT = IT-PRICE * IT-QTY
                   MOVE "PLANNED" TO D-STATUS
                   MOVE "preview only" TO D-OUTCOME
                   PERFORM 8000-LOG-LINE THRU 8000-EXIT
                   GO TO 3000-EXIT
               END-IF
               PERFORM 3400-CREATE-QUOTE THRU 3400-EXIT
               IF WS-QUOTE = SPACES
                   GO TO 3000-EXIT
               END-IF
               PERFORM 3500-ISSUE-ACCEPT THRU 3500-EXIT
               PERFORM 3600-CONVERT THRU 3600-EXIT
           END-IF
           IF WS-ORDER = SPACES
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO WS-ORD-CORR
           STRING "SODEMO-" FUNCTION TRIM(WS-CUR-CO) "-O-"
                  FUNCTION TRIM(WS-ORDER)
               DELIMITED BY SIZE INTO WS-ORD-CORR
           PERFORM 4000-ACCEPT-ORDER THRU 4000-EXIT
           PERFORM 4500-PROMISE THRU 4500-EXIT
           PERFORM 5000-RESERVE THRU 5000-EXIT
           PERFORM 6000-PICK THRU 6000-EXIT
           PERFORM 2500-DESPATCH-ORDER THRU 2500-EXIT.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3100-FIND-DEMO-ORDER SECTION.
       3100-START.
           MOVE SPACES TO WS-ORDER
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 o.OrderNumber, o.CustomerCode, "
                  "ISNULL(o.SiteCode,'') FROM so.SalesOrder o WHERE "
                  "o.CompanyCode = ? AND o.CustomerOrderRef = ? AND "
                  "o.CommercialStatus IN ('ACCEPTED','AMENDED') AND "
                  "o.InvoiceCustomerCode IS NOT NULL AND "
                  "o.DeliveryAddressSnapshot IS NOT NULL AND "
                  "o.TermsAndConditionsVersion IS NOT NULL AND "
                  "o.VatTreatmentBasis IS NOT NULL "
      *>           an order the demonstration already despatched in
      *>           full has nothing left to show, so it is not
      *>           adopted again - the next run builds a fresh quote
                  "AND EXISTS (SELECT 1 FROM so.SalesOrderLine sl "
                  "WHERE sl.SalesOrderId = o.SalesOrderId AND "
                  "sl.Status NOT IN ('CANCELLED','CLOSED') AND "
                  "sl.AcceptedQty - sl.CancelledQty - "
                  "sl.DespatchedQty > 0) "
                  "ORDER BY o.OrderNumber"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CUR-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-DEMO-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 3 TO SQLIO-COL-CNT
           PERFORM 8900-SET-WIDTHS THRU 8900-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SD-ROW
           IF SQLIO-OK AND SD-COL(1) NOT = SPACES
               MOVE SD-COL(1) TO WS-ORDER
               MOVE SD-COL(2) TO WS-CUSTOMER
               IF SD-COL(3) NOT = SPACES
                   MOVE SD-COL(3) TO WS-SITE
               END-IF
               MOVE "order already built" TO D-OUTCOME
               MOVE "quote to order" TO D-STEP
               MOVE WS-ORDER TO D-REF
               MOVE 0 TO D-AMOUNT
               MOVE "RE-RUN" TO D-STATUS
               PERFORM 8000-LOG-LINE THRU 8000-EXIT
           END-IF.
       3100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> The account the demonstration trades on: the dedicated sales
      *> demonstration customer, in good standing with no overdue
      *> debt, seeded by db\seed\93-so-demo.sql.  Classification
      *> SODEMO is all that marks it - nothing else about it is
      *> special, and the credit check still has to agree before a
      *> single line is quoted against it (SO-007, D-06).
      *>----------------------------------------------------------------
       3150-DEMO-CUSTOMER SECTION.
       3150-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 c.CustomerCode, ISNULL(c.TermsCode,"
                  "''), ISNULL((SELECT TOP 1 LTRIM(RTRIM(CONCAT("
                  "a.Line1, ISNULL(', ' + NULLIF(a.Town,''),''), "
                  "ISNULL(', ' + NULLIF(a.Postcode,''),'')))) FROM "
                  "ar.CustomerAddress a WHERE a.CompanyCode = "
                  "c.CompanyCode AND a.CustomerCode = c.CustomerCode"
                  " AND a.AddressRole = 'DELIVERY' ORDER BY "
                  "a.IsDefault DESC, a.AddressRef),'') FROM "
                  "ar.Customer c JOIN ar.CustomerCredit cc ON "
                  "cc.CompanyCode = c.CompanyCode AND "
                  "cc.CustomerCode = c.CustomerCode WHERE "
                  "c.CompanyCode = ? AND c.Classification = 'SODEMO'"
                  " AND c.Status = 'ACTIVE' AND cc.HoldFlag = 0 AND "
                  "cc.CreditLimitGBP > 0 ORDER BY c.CustomerCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CUR-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 3 TO SQLIO-COL-CNT
           PERFORM 8900-SET-WIDTHS THRU 8900-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SD-ROW
           IF SQLIO-OK AND SD-COL(1) NOT = SPACES
               MOVE SD-COL(1) TO WS-CUSTOMER
               MOVE SD-COL(2) TO WS-TERMS
               MOVE SD-COL(3) TO WS-DELIV-SNAP
               MOVE SPACES TO WS-ORDER
               PERFORM 2300-CREDIT-CHECK THRU 2300-EXIT
               IF WS-CREDIT-OK NOT = "Y"
                   MOVE SPACES TO WS-CUSTOMER
               END-IF
           END-IF.
       3150-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A customer that is trading and not on credit hold - the
      *> demonstration is about the happy path, the held orders are
      *> already in the seed.
      *>----------------------------------------------------------------
       3200-CHOOSE-CUSTOMER SECTION.
       3200-START.
           MOVE SPACES TO WS-CUSTOMER
           MOVE SPACES TO WS-LAST-CUST
           MOVE 0 TO WS-SEQ-CUST
           MOVE "N" TO WS-CREDIT-OK
           PERFORM 3150-DEMO-CUSTOMER THRU 3150-EXIT
           IF WS-CUSTOMER NOT = SPACES
               GO TO 3200-DEFAULTS
           END-IF
           PERFORM 8 TIMES
               MOVE SPACES TO SQLIO-STMT
               STRING "SELECT o.CustomerCode, "
                      "ISNULL(MAX(o.TermsCode),''), "
                      "ISNULL(MAX(o.TermsAndConditionsVersion),''), "
                      "ISNULL(MAX(o.DeliveryAddressSnapshot),''), "
                      "ISNULL(MAX(o.VatTreatmentBasis),''), "
                      "ISNULL(MAX(o.VatLocation),''), "
                      "ISNULL(MAX(o.IncotermsRule),''), "
                      "ISNULL(MAX(o.IncotermsNamedPlace),'') "
                      "FROM so.SalesOrder o WHERE o.CompanyCode = ? "
                      "AND o.CurrencyCode = 'GBP' GROUP BY "
                      "o.CustomerCode ORDER BY MIN(CASE WHEN EXISTS "
                      "(SELECT 1 FROM ar.SalesInvoice i WHERE "
                      "i.CompanyCode = ? AND i.CustomerCode = "
                      "o.CustomerCode) THEN 1 ELSE 0 END), "
                      "o.CustomerCode OFFSET CAST(? AS int) ROWS "
                      "FETCH NEXT 1 ROWS ONLY"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 3 TO SQLIO-PARM-CNT
               MOVE WS-CUR-CO TO SQLIO-PARM-VAL(1)
               MOVE WS-CUR-CO TO SQLIO-PARM-VAL(2)
               MOVE SPACES TO SQLIO-PARM-VAL(3)
               MOVE WS-SEQ-CUST TO SQLIO-PARM-VAL(3)
               MOVE "N" TO SQLIO-PARM-NULL(1)
               MOVE "N" TO SQLIO-PARM-NULL(2)
               MOVE "N" TO SQLIO-PARM-NULL(3)
               ADD 1 TO WS-SEQ-CUST
               MOVE 8 TO SQLIO-COL-CNT
               PERFORM 8900-SET-WIDTHS THRU 8900-EXIT
               MOVE 8 TO SQLIO-CURSOR
               MOVE "SNGL" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA SD-ROW
               IF SQLIO-OK AND SD-COL(1) NOT = SPACES
                   MOVE SD-COL(1) TO WS-CUSTOMER
                   MOVE SD-COL(1) TO WS-LAST-CUST
                   MOVE SD-COL(2) TO WS-TERMS
                   MOVE SD-COL(3) TO WS-TC-VERSION
                   MOVE SD-COL(4) TO WS-DELIV-SNAP
                   MOVE SD-COL(5) TO WS-VAT-BASIS
                   MOVE SD-COL(6) TO WS-VAT-LOC
                   MOVE SD-COL(7) TO WS-INCO
                   MOVE SD-COL(8) TO WS-INCO-PLACE
                   MOVE SPACES TO WS-ORDER
                   PERFORM 2300-CREDIT-CHECK THRU 2300-EXIT
                   IF WS-CREDIT-OK = "Y"
                       EXIT PERFORM
                   END-IF
                   MOVE SPACES TO WS-CUSTOMER
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM.
       3200-DEFAULTS.
           IF WS-TC-VERSION = SPACES
               MOVE "STERP-TCS-2026-04" TO WS-TC-VERSION
           END-IF
           IF WS-TERMS = SPACES
               MOVE "DOM31" TO WS-TERMS
           END-IF
           IF WS-DELIV-SNAP = SPACES
               MOVE "Demonstration delivery address" TO WS-DELIV-SNAP
           END-IF
           IF WS-VAT-BASIS = SPACES
               MOVE "UK standard rated" TO WS-VAT-BASIS
           END-IF
           IF WS-VAT-LOC = SPACES
               MOVE "GB" TO WS-VAT-LOC
           END-IF.
       3200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> An item this company has sold before AND holds in saleable
      *> stock, with the price, unit and VAT code it was last sold
      *> at.  Nothing is invented.
      *>----------------------------------------------------------------
       3300-CHOOSE-ITEM SECTION.
       3300-START.
           INITIALIZE WS-ITEM-WORK
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 b.ItemCode, b.WarehouseCode, "
                  "b.LocationCode, CONVERT(varchar(24), x.UnitPrice)"
                  ", ISNULL(x.VatCode,'S'), ISNULL(x.UomCode,'EA'), "
                  "ISNULL(x.Description,''), b.SiteCode "
                  "FROM inv.StockBalance b "
                  "JOIN inv.Item it ON it.CompanyCode = "
                  "b.CompanyCode AND it.ItemCode = b.ItemCode "
                  "JOIN inv.Location v ON v.CompanyCode = "
                  "b.CompanyCode AND v.SiteCode = b.SiteCode AND "
                  "v.WarehouseCode = b.WarehouseCode AND "
                  "v.LocationCode = b.LocationCode CROSS APPLY "
                  "(SELECT TOP 1 l.UnitPrice, l.VatCode, l.UomCode, "
                  "l.Description FROM so.SalesOrderLine l JOIN "
                  "so.SalesOrder o ON o.SalesOrderId = "
                  "l.SalesOrderId WHERE o.CompanyCode = "
                  "b.CompanyCode AND l.ItemCode = b.ItemCode AND "
                  "l.UnitPrice > 0 ORDER BY l.SalesOrderLineId DESC)"
                  " x WHERE b.CompanyCode = ? AND "
                  "b.QualityStatus = 'ACCEPTED' "
                  "AND b.OwnerType = 'OWNED' AND b.LotCode = '' AND "
                  "b.SerialNo = '' AND v.IsAvailable = 1 AND "
                  "v.IsOwnedStock = 1 AND it.IsSaleable = 1 AND "
                  "it.LifecycleStatus NOT IN ('OBSOLETE','RETIRED',"
                  "'BLOCKED') AND "
                  "(b.QtyOnHand - ISNULL(b.QtyReserved,0)) >= 4 "
      *>           the site the warehouse scan settled on is a
      *>           preference, not a restriction: a demonstration
      *>           that insisted on it found no stock at all once
      *>           the saleable balances sat at another site
                  "ORDER BY CASE WHEN ? = '' OR b.SiteCode = ? "
                  "THEN 0 ELSE 1 END, b.QtyOnHand DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE WS-CUR-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-SITE TO SQLIO-PARM-VAL(2)
           MOVE WS-SITE TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 8 TO SQLIO-COL-CNT
           PERFORM 8900-SET-WIDTHS THRU 8900-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SD-ROW
           IF SQLIO-OK AND SD-COL(1) NOT = SPACES
               MOVE SD-COL(1) TO IT-ITEM
               MOVE SD-COL(2) TO IT-WHSE
               MOVE SD-COL(3) TO IT-LOCN
               COMPUTE IT-PRICE = FUNCTION NUMVAL(SD-COL(4))
               MOVE SD-COL(5) TO IT-VAT-CODE
               MOVE SD-COL(6) TO IT-UOM
               MOVE SD-COL(7) TO IT-DESC
               MOVE 2 TO IT-QTY
               IF SD-COL(8) NOT = SPACES
                   MOVE SD-COL(8) TO WS-SITE
               END-IF
               MOVE IT-WHSE TO WS-WHS
               MOVE IT-LOCN TO WS-LOCN
               IF WS-QUAR-LOCN = SPACES
                   MOVE IT-LOCN TO WS-QUAR-LOCN
               END-IF
           END-IF.
       3300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3400-CREATE-QUOTE SECTION.
       3400-START.
           MOVE SPACES TO WS-QUOTE
           INITIALIZE WS-QUO
           MOVE WS-CUR-CO TO CA-COMPANY OF WS-QUO
           MOVE WS-SITE TO CA-SITE OF WS-QUO
           MOVE WS-USER TO CA-USER OF WS-QUO
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-QUO
           MOVE SPACES TO WS-STEP-CORR
           STRING FUNCTION TRIM(WS-RUN-CORR) "-Q"
               DELIMITED BY SIZE INTO WS-STEP-CORR
           MOVE WS-STEP-CORR TO CA-CORRELATION-ID OF WS-QUO
           MOVE "CREATE" TO CA-FUNCTION OF WS-QUO
           MOVE WS-CUSTOMER TO SQ-CUSTOMER
           MOVE WS-TODAY-N TO SQ-QUOTE-DATE
           MOVE WS-VALID-UNTIL TO SQ-VALID-UNTIL
           MOVE "GBP" TO SQ-CURRENCY
           MOVE 1 TO SQ-FX-RATE
           MOVE WS-SITE TO SQ-SITE-CODE
           MOVE WS-USER TO SQ-SALESPERSON
           MOVE WS-TERMS TO SQ-TERMS-CODE
           MOVE WS-TC-VERSION TO SQ-TERMS-VERSION
           MOVE "Demonstration quotation built by SODEMO"
               TO SQ-DELIVERY-EST
           MOVE 1 TO SQ-LINE-CNT
           MOVE 1 TO SQ-L-LINE-NUM(1)
           MOVE IT-ITEM TO SQ-L-ITEM(1)
           MOVE IT-DESC TO SQ-L-DESC(1)
           MOVE IT-QTY TO SQ-L-QTY(1)
           MOVE IT-UOM TO SQ-L-UOM(1)
           MOVE IT-PRICE TO SQ-L-UNIT-PRICE(1)
           MOVE 1 TO SQ-L-PRICE-PER(1)
           MOVE IT-VAT-CODE TO SQ-L-VAT-CODE(1)
           MOVE "STANDARD" TO SQ-L-COST-BASIS(1)
           COMPUTE SQ-L-NET(1) = IT-PRICE * IT-QTY
           CALL "SOQUOTE" USING WS-QUO
           MOVE CA-RETURN-CODE OF WS-QUO TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-QUO TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-QUO TO WS-CALL-MSG
           MOVE "quote create" TO D-STEP
           MOVE SQ-QUOTE-NUMBER TO D-REF
           MOVE SQ-NET-AMOUNT TO D-AMOUNT
           IF WS-CALL-RC < 8
               MOVE SQ-QUOTE-NUMBER TO WS-QUOTE
           END-IF
           PERFORM 8100-LOG-CALL THRU 8100-EXIT.
       3400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3500-ISSUE-ACCEPT SECTION.
       3500-START.
           MOVE "ISSUE" TO CA-FUNCTION OF WS-QUO
           MOVE WS-QUOTE TO SQ-QUOTE-NUMBER
           CALL "SOQUOTE" USING WS-QUO
           MOVE CA-RETURN-CODE OF WS-QUO TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-QUO TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-QUO TO WS-CALL-MSG
           MOVE "quote issue" TO D-STEP
           MOVE WS-QUOTE TO D-REF
           MOVE SQ-GROSS-AMOUNT TO D-AMOUNT
           PERFORM 8100-LOG-CALL THRU 8100-EXIT
           MOVE "ACCEPT" TO CA-FUNCTION OF WS-QUO
           MOVE WS-QUOTE TO SQ-QUOTE-NUMBER
           MOVE "Customer acceptance by e-mail, demonstration data"
               TO SQ-ACCEPT-EVIDENCE
           CALL "SOQUOTE" USING WS-QUO
           MOVE CA-RETURN-CODE OF WS-QUO TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-QUO TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-QUO TO WS-CALL-MSG
           MOVE "quote accept" TO D-STEP
           MOVE WS-QUOTE TO D-REF
           MOVE SQ-GROSS-AMOUNT TO D-AMOUNT
           PERFORM 8100-LOG-CALL THRU 8100-EXIT.
       3500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3600-CONVERT SECTION.
       3600-START.
           MOVE "CONVERT" TO CA-FUNCTION OF WS-QUO
           MOVE WS-QUOTE TO SQ-QUOTE-NUMBER
           MOVE WS-DEMO-REF TO SQ-CUSTOMER-REF
           MOVE "STANDARD" TO SQ-ORDER-TYPE
           MOVE WS-SITE TO SQ-SITE-CODE
           CALL "SOQUOTE" USING WS-QUO
           MOVE CA-RETURN-CODE OF WS-QUO TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-QUO TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-QUO TO WS-CALL-MSG
           MOVE "quote convert" TO D-STEP
           MOVE SQ-ORDER-NUMBER TO D-REF
           MOVE SQ-NET-AMOUNT TO D-AMOUNT
           IF WS-CALL-RC < 8 AND SQ-ORDER-NUMBER NOT = SPACES
               MOVE SQ-ORDER-NUMBER TO WS-ORDER
               ADD 1 TO T-ORDERS
           END-IF
           PERFORM 8100-LOG-CALL THRU 8100-EXIT.
       3600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> SOACCEPT names every blocker, then accepts, then releases.
      *> The credit decision is taken inside the service, never here.
      *>----------------------------------------------------------------
       4000-ACCEPT-ORDER SECTION.
       4000-START.
           INITIALIZE WS-ACC
           MOVE WS-CUR-CO TO CA-COMPANY OF WS-ACC
           MOVE WS-SITE TO CA-SITE OF WS-ACC
           MOVE WS-USER TO CA-USER OF WS-ACC
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-ACC
           MOVE SPACES TO WS-STEP-CORR
           STRING FUNCTION TRIM(WS-ORD-CORR) "-A"
               DELIMITED BY SIZE INTO WS-STEP-CORR
           MOVE WS-STEP-CORR TO CA-CORRELATION-ID OF WS-ACC
           MOVE "VALIDATE" TO CA-FUNCTION OF WS-ACC
           MOVE WS-ORDER TO SA-ORDER-NUMBER
           MOVE WS-CUSTOMER TO SA-INVOICE-CUST
           MOVE WS-TERMS TO SA-TERMS-CODE
           MOVE WS-TC-VERSION TO SA-TC-VERSION
           MOVE WS-DELIV-SNAP TO SA-DELIVERY-SNAP
           MOVE WS-VAT-BASIS TO SA-VAT-BASIS
           MOVE WS-VAT-LOC TO SA-VAT-LOCATION
           MOVE WS-INCO TO SA-INCOTERMS-RULE
           MOVE WS-INCO-PLACE TO SA-INCOTERMS-PLACE
           MOVE "Y" TO SA-RETENTION-TITLE
           MOVE WS-USER TO SA-SALESPERSON
           CALL "SOACCEPT" USING WS-ACC
           MOVE CA-RETURN-CODE OF WS-ACC TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-ACC TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-ACC TO WS-CALL-MSG
           MOVE "order validate" TO D-STEP
           MOVE WS-ORDER TO D-REF
           MOVE SA-GROSS-AMOUNT TO D-AMOUNT
           PERFORM 8100-LOG-CALL THRU 8100-EXIT
           IF WS-CALL-RC > 4 AND SA-BLOCKER-CNT > 0
               PERFORM VARYING WS-BX FROM 1 BY 1
                       UNTIL WS-BX > SA-BLOCKER-CNT
                   DISPLAY "STERP-W-1856 blocker "
                           SA-B-REASON(WS-BX) " "
                           FUNCTION TRIM(SA-B-TEXT(WS-BX))
                       UPON SYSOUT
               END-PERFORM
           END-IF
           IF SA-COMM-STATUS = "DRAFT"
               MOVE "ACCEPT" TO CA-FUNCTION OF WS-ACC
               MOVE WS-ORDER TO SA-ORDER-NUMBER
      *>       VALIDATE answers with the stored order, so the terms
      *>       of the sale are stated again on the instruction that
      *>       is going to record them
               MOVE WS-CUSTOMER TO SA-INVOICE-CUST
               MOVE WS-TERMS TO SA-TERMS-CODE
               MOVE WS-TC-VERSION TO SA-TC-VERSION
               MOVE WS-DELIV-SNAP TO SA-DELIVERY-SNAP
               MOVE WS-VAT-BASIS TO SA-VAT-BASIS
               MOVE WS-VAT-LOC TO SA-VAT-LOCATION
               MOVE WS-INCO TO SA-INCOTERMS-RULE
               MOVE WS-INCO-PLACE TO SA-INCOTERMS-PLACE
               MOVE "Y" TO SA-RETENTION-TITLE
               CALL "SOACCEPT" USING WS-ACC
               MOVE CA-RETURN-CODE OF WS-ACC TO WS-CALL-RC
               MOVE CA-REASON-CODE OF WS-ACC TO WS-CALL-REASON
               MOVE CA-MESSAGE OF WS-ACC TO WS-CALL-MSG
               MOVE "order accept" TO D-STEP
               MOVE WS-ORDER TO D-REF
               MOVE SA-GROSS-AMOUNT TO D-AMOUNT
               PERFORM 8100-LOG-CALL THRU 8100-EXIT
           END-IF
           IF SA-RELEASE-STATUS NOT = "RELEASED"
               MOVE "RELEASE" TO CA-FUNCTION OF WS-ACC
               MOVE WS-ORDER TO SA-ORDER-NUMBER
               CALL "SOACCEPT" USING WS-ACC
               MOVE CA-RETURN-CODE OF WS-ACC TO WS-CALL-RC
               MOVE CA-REASON-CODE OF WS-ACC TO WS-CALL-REASON
               MOVE CA-MESSAGE OF WS-ACC TO WS-CALL-MSG
               MOVE "credit release" TO D-STEP
               MOVE WS-ORDER TO D-REF
               MOVE SA-CREDIT-HEADROOM TO D-AMOUNT
               PERFORM 8100-LOG-CALL THRU 8100-EXIT
           END-IF.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> The promise.  A line with no delivery schedule cannot be
      *> picked and has no service measure, so the order gets one
      *> schedule per line for the whole active quantity.
      *>----------------------------------------------------------------
       4500-PROMISE SECTION.
       4500-START.
           PERFORM 2200-READ-LINES THRU 2200-EXIT
           PERFORM VARYING WS-LX FROM 1 BY 1 UNTIL WS-LX > WS-LN-CNT
               IF LN-SCHEDULE(WS-LX) = 0
                   INITIALIZE WS-SCH
                   MOVE WS-CUR-CO TO CA-COMPANY OF WS-SCH
                   MOVE WS-SITE TO CA-SITE OF WS-SCH
                   MOVE WS-USER TO CA-USER OF WS-SCH
                   MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-SCH
                   MOVE WS-ORD-CORR TO CA-CORRELATION-ID OF WS-SCH
                   MOVE "SPLIT" TO CA-FUNCTION OF WS-SCH
                   MOVE WS-ORDER TO SS-ORDER-NUMBER
                   MOVE LN-LINE-NUM(WS-LX) TO SS-LINE-NUM
                   MOVE 1 TO SS-SPLIT-CNT
                   MOVE 1 TO SS-S-SCHED-NO(1)
                   MOVE LN-OUTSTANDING(WS-LX) TO SS-S-QTY(1)
                   MOVE WS-SITE TO SS-S-SITE(1)
                   MOVE WS-PROMISE-DATE TO SS-S-REQUESTED(1)
                   MOVE WS-PROMISE-DATE TO SS-S-PROMISE(1)
                   MOVE "WAREHOUSE" TO SS-S-METHOD(1)
                   MOVE 5 TO SS-S-PRIORITY(1)
                   CALL "SOSCHED" USING WS-SCH
                   MOVE CA-RETURN-CODE OF WS-SCH TO WS-CALL-RC
                   MOVE CA-REASON-CODE OF WS-SCH TO WS-CALL-REASON
                   MOVE CA-MESSAGE OF WS-SCH TO WS-CALL-MSG
                   MOVE "delivery promise" TO D-STEP
                   MOVE WS-ORDER TO D-REF
                   MOVE 0 TO D-AMOUNT
                   PERFORM 8100-LOG-CALL THRU 8100-EXIT
               END-IF
           END-PERFORM.
       4500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> The reservation.  SORESV owns the sales rules; the free
      *> quantity race is settled inside inv.usp_ReserveStock.
      *>----------------------------------------------------------------
       5000-RESERVE SECTION.
       5000-START.
           PERFORM 2200-READ-LINES THRU 2200-EXIT
           PERFORM VARYING WS-LX FROM 1 BY 1 UNTIL WS-LX > WS-LN-CNT
               INITIALIZE WS-RSV
               MOVE WS-CUR-CO TO CA-COMPANY OF WS-RSV
               MOVE WS-SITE TO CA-SITE OF WS-RSV
               MOVE WS-USER TO CA-USER OF WS-RSV
               MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-RSV
               MOVE WS-ORD-CORR TO CA-CORRELATION-ID OF WS-RSV
               MOVE "ENQUIRE" TO CA-FUNCTION OF WS-RSV
               MOVE WS-ORDER TO SV-ORDER-NUMBER
               MOVE LN-LINE-NUM(WS-LX) TO SV-LINE-NUM
               MOVE LN-ITEM(WS-LX) TO SV-ITEM-CODE
               MOVE WS-SITE TO SV-SITE
               MOVE "SALESORDER" TO SV-DEMAND-TYPE
               MOVE WS-ORDER TO SV-DEMAND-REF
               MOVE LN-LINE-NUM(WS-LX) TO SV-DEMAND-LINE
               CALL "SORESV" USING WS-RSV
      *>       a reservation that holds nothing is not a reservation:
      *>       it is re-presented rather than reported as done
               IF SV-LINE-CNT > 0 AND SV-L-QTY(1) > 0
                   MOVE "reserve" TO D-STEP
                   MOVE WS-ORDER TO D-REF
                   MOVE 0 TO D-AMOUNT
                   MOVE "RE-RUN" TO D-STATUS
                   MOVE "already reserved" TO D-OUTCOME
                   PERFORM 8000-LOG-LINE THRU 8000-EXIT
               ELSE
                   MOVE "RESERVE" TO CA-FUNCTION OF WS-RSV
                   MOVE WS-WHS TO SV-WAREHOUSE
                   MOVE LN-QTY(WS-LX) TO SV-QTY-REQUESTED
                   MOVE "Y" TO SV-ALLOW-PARTIAL
                   MOVE WS-PROMISE-DATE TO SV-REQUIRED-DATE
                   MOVE WS-PROMISE-DATE TO SV-DESPATCH-DATE
                   MOVE WS-CUSTOMER TO SV-CUSTOMER
                   MOVE 5 TO SV-PRIORITY
                   CALL "SORESV" USING WS-RSV
                   MOVE CA-RETURN-CODE OF WS-RSV TO WS-CALL-RC
                   MOVE CA-REASON-CODE OF WS-RSV TO WS-CALL-REASON
                   MOVE CA-MESSAGE OF WS-RSV TO WS-CALL-MSG
                   MOVE "reserve stock" TO D-STEP
                   MOVE WS-ORDER TO D-REF
                   MOVE SV-QTY-RESERVED TO D-AMOUNT
                   PERFORM 8100-LOG-CALL THRU 8100-EXIT
               END-IF
           END-PERFORM.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> The pick list.  Picking moves no stock and creates no cost
      *> of sale - it is an instruction to a person in a warehouse.
      *>----------------------------------------------------------------
       6000-PICK SECTION.
       6000-START.
           PERFORM 6100-FIND-PICKLIST THRU 6100-EXIT
           IF WS-PICKLIST NOT = SPACES
               MOVE "pick list" TO D-STEP
               MOVE WS-PICKLIST TO D-REF
               MOVE 0 TO D-AMOUNT
               MOVE "RE-RUN" TO D-STATUS
               MOVE "already picked" TO D-OUTCOME
               PERFORM 8000-LOG-LINE THRU 8000-EXIT
               GO TO 6000-EXIT
           END-IF
           INITIALIZE WS-PCK
           MOVE WS-CUR-CO TO CA-COMPANY OF WS-PCK
           MOVE WS-SITE TO CA-SITE OF WS-PCK
           MOVE WS-USER TO CA-USER OF WS-PCK
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-PCK
           MOVE WS-ORD-CORR TO CA-CORRELATION-ID OF WS-PCK
           MOVE "GENERATE" TO CA-FUNCTION OF WS-PCK
           MOVE WS-ORDER TO PK-ORDER-NUMBER
           MOVE WS-SITE TO PK-SITE
           MOVE WS-WHS TO PK-WAREHOUSE
           MOVE WS-CUSTOMER TO PK-CUSTOMER
           MOVE WS-PROMISE-DATE TO PK-REQUIRED-DATE
           MOVE WS-USER TO PK-PICKER-USER
           CALL "PICKGEN" USING WS-PCK
           MOVE CA-RETURN-CODE OF WS-PCK TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-PCK TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-PCK TO WS-CALL-MSG
           MOVE "pick list" TO D-STEP
           MOVE PK-PICKLIST-REF TO D-REF
           MOVE PK-GENERATED-CNT TO D-AMOUNT
           PERFORM 8100-LOG-CALL THRU 8100-EXIT
           IF PK-PICKLIST-REF = SPACES
               GO TO 6000-EXIT
           END-IF
           MOVE PK-PICKLIST-REF TO WS-PICKLIST
      *>   the generated lines are copied out before the next CALL
      *>   overwrites the COMMAREA
           MOVE PK-LINE-CNT TO WS-LN-CNT
           PERFORM VARYING WS-LX FROM 1 BY 1 UNTIL WS-LX > WS-LN-CNT
               MOVE PK-L-LINE-NUM(WS-LX) TO LN-LINE-NUM(WS-LX)
               MOVE PK-L-ITEM(WS-LX) TO LN-ITEM(WS-LX)
               MOVE PK-L-REQUIRED(WS-LX) TO LN-QTY(WS-LX)
               MOVE PK-L-LOCATION(WS-LX) TO LN-LOCN(WS-LX)
               MOVE PK-L-SCHEDULE(WS-LX) TO LN-SCHEDULE(WS-LX)
           END-PERFORM
           PERFORM VARYING WS-LX FROM 1 BY 1 UNTIL WS-LX > WS-LN-CNT
               MOVE "CONFIRM" TO CA-FUNCTION OF WS-PCK
               MOVE WS-PICKLIST TO PK-PICKLIST-REF
               MOVE WS-ORDER TO PK-ORDER-NUMBER
               MOVE LN-LINE-NUM(WS-LX) TO PK-LINE-NUM
               MOVE LN-ITEM(WS-LX) TO PK-ITEM-CODE
               MOVE LN-QTY(WS-LX) TO PK-PICKED-QTY
               MOVE 0 TO PK-SHORT-QTY
               MOVE LN-LOCN(WS-LX) TO PK-LOCATION
               IF WS-NAME-LOT = "Y"
                   PERFORM 2730-PICK-SOURCE THRU 2730-EXIT
               END-IF
               CALL "PICKGEN" USING WS-PCK
               MOVE CA-RETURN-CODE OF WS-PCK TO WS-CALL-RC
               MOVE CA-REASON-CODE OF WS-PCK TO WS-CALL-REASON
               MOVE CA-MESSAGE OF WS-PCK TO WS-CALL-MSG
               MOVE "pick confirm" TO D-STEP
               MOVE WS-PICKLIST TO D-REF
               MOVE PK-PICKED-QTY TO D-AMOUNT
               PERFORM 8100-LOG-CALL THRU 8100-EXIT
           END-PERFORM.
       6000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       6100-FIND-PICKLIST SECTION.
       6100-START.
           MOVE SPACES TO WS-PICKLIST
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 p.PickListRef FROM so.PickList p "
                  "WHERE p.CompanyCode = ? AND p.OrderNumber = ? "
                  "AND p.Status <> 'CANCELLED' ORDER BY "
                  "p.PickListId DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CUR-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-ORDER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8900-SET-WIDTHS THRU 8900-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SD-ROW
           IF SQLIO-OK
               MOVE SD-COL(1) TO WS-PICKLIST
           END-IF.
       6100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> The invoice run.  SOINVRUN is a batch program that ends in
      *> STOP RUN, so it is spawned as a process - a CALL would end
      *> this run unit with it.  It bills only despatch lines whose
      *> billed quantity has not caught up, so a second run of this
      *> job invoices nothing twice.
      *>----------------------------------------------------------------
       7000-INVOICE-RUN SECTION.
       7000-START.
           MOVE "Invoice run" TO WS-SCENARIO
      *>   the invoice run is a separate process on its own
      *>   connection, so this one is settled and closed first - a
      *>   half open unit of work would simply block it
           MOVE "ROLB" TO SQLIO-FUNC
           MOVE 8 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA SD-ROW
           MOVE "DISC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SD-ROW
           MOVE SPACES TO WS-CMD
           STRING "cd /d """ FUNCTION TRIM(WS-ROOT) """ && "
                  "(if not exist run\spool md run\spool) && "
                  "bin\x86\SOINVRUN.exe CO="
                  FUNCTION TRIM(WS-CUR-CO) " MODE="
                  FUNCTION TRIM(WS-MODE) " USER=ARCLERK "
                  ">> run\spool\SODEMO-SOINVRUN.log 2>&1"
               DELIMITED BY SIZE INTO WS-CMD
           MOVE 0 TO WS-SYSRC
           CALL "SYSTEM" USING WS-CMD
           MOVE RETURN-CODE TO WS-SYSRC
           MOVE 0 TO RETURN-CODE
           MOVE "invoice run" TO D-STEP
           MOVE SPACES TO D-REF
           STRING "SOINVRUN " FUNCTION TRIM(WS-CUR-CO)
               DELIMITED BY SIZE INTO D-REF
           MOVE 0 TO D-AMOUNT
           MOVE SPACES TO WS-CALL-REASON WS-CALL-MSG
           IF WS-SYSRC = 0
               MOVE 0 TO WS-CALL-RC
           ELSE
               IF WS-SYSRC = 4
                   MOVE 4 TO WS-CALL-RC
               ELSE
                   MOVE 8 TO WS-CALL-RC
               END-IF
               MOVE "SOINVRUN returned non zero" TO WS-CALL-MSG
           END-IF
           PERFORM 8100-LOG-CALL THRU 8100-EXIT.
       7000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A customer return against one of the despatches just made:
      *> authorisation, receipt into quarantine, inspection and the
      *> disposition.  Without one the returns screens and the
      *> quality figures have nothing to show.
      *>----------------------------------------------------------------
       7500-CUSTOMER-RETURN SECTION.
       7500-START.
           MOVE "Customer return" TO WS-SCENARIO
           IF NOT WS-COMMIT
               GO TO 7500-EXIT
           END-IF
           PERFORM 7600-FIND-DESPATCH THRU 7600-EXIT
           IF WS-FOUND NOT = "Y"
               GO TO 7500-EXIT
           END-IF
           INITIALIZE WS-RET
           MOVE WS-CUR-CO TO CA-COMPANY OF WS-RET
           MOVE WS-SITE TO CA-SITE OF WS-RET
           MOVE WS-USER TO CA-USER OF WS-RET
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-RET
           MOVE SPACES TO WS-STEP-CORR
           STRING FUNCTION TRIM(WS-CORR) "-R-"
                  FUNCTION TRIM(WS-DESPATCH)
               DELIMITED BY SIZE INTO WS-STEP-CORR
           MOVE WS-STEP-CORR TO CA-CORRELATION-ID OF WS-RET
           MOVE "REQUEST" TO CA-FUNCTION OF WS-RET
           MOVE WS-CUSTOMER TO RT-CUSTOMER
           MOVE WS-SITE TO RT-SITE
           MOVE WS-ORDER TO RT-ORIG-ORDER
           MOVE WS-DESPATCH TO RT-ORIG-DESPATCH
           MOVE WS-TODAY-N TO RT-REQUEST-DATE
           MOVE WS-TODAY-N TO RT-DELIVERY-DATE
           MOVE "DAMAGED" TO RT-REASON-CODE
           MOVE "One unit arrived with a damaged carton"
               TO RT-REASON
           MOVE "N" TO RT-IS-FAULTY
           MOVE WS-WHS TO RT-WAREHOUSE
           MOVE WS-QUAR-LOCN TO RT-QUARANTINE-LOC
           MOVE 1 TO RT-LINE-CNT
           MOVE 1 TO RT-L-LINE-NUM(1)
           MOVE IT-ITEM TO RT-L-ITEM(1)
           MOVE IT-UOM TO RT-L-UOM(1)
           MOVE 1 TO RT-L-REQ-QTY(1)
           CALL "RETURNS" USING WS-RET
           MOVE CA-RETURN-CODE OF WS-RET TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-RET TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-RET TO WS-CALL-MSG
           MOVE "return request" TO D-STEP
           MOVE RT-RETURN-REF TO D-REF
           MOVE RT-TOTAL-VALUE-GBP TO D-AMOUNT
           PERFORM 8100-LOG-CALL THRU 8100-EXIT
           IF WS-CALL-RC > 4 OR RT-RETURN-REF = SPACES
               GO TO 7500-EXIT
           END-IF
           MOVE RT-RETURN-REF TO WS-RETURN-REF
      *>   SO-014.  Goods are only received against an AUTHORISED
      *>   return, so the agreed quantity is authorised first.
           MOVE "AUTHORISE" TO CA-FUNCTION OF WS-RET
           MOVE WS-RETURN-REF TO RT-RETURN-REF
           MOVE WS-TODAY-N TO RT-AUTH-DATE
           MOVE "SALESMGR" TO RT-AUTHORISED-BY
           MOVE 1 TO RT-L-AUTH-QTY(1)
           CALL "RETURNS" USING WS-RET
           MOVE CA-RETURN-CODE OF WS-RET TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-RET TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-RET TO WS-CALL-MSG
           MOVE "return authorise" TO D-STEP
           MOVE WS-RETURN-REF TO D-REF
           MOVE 0 TO D-AMOUNT
           PERFORM 8100-LOG-CALL THRU 8100-EXIT
           IF WS-CALL-RC > 4
               GO TO 7500-EXIT
           END-IF
           MOVE "RECEIVE" TO CA-FUNCTION OF WS-RET
           MOVE WS-RETURN-REF TO RT-RETURN-REF
           MOVE WS-TODAY-N TO RT-RECEIVED-DATE
           MOVE 1 TO RT-L-RECV-QTY(1)
           CALL "RETURNS" USING WS-RET
           MOVE CA-RETURN-CODE OF WS-RET TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-RET TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-RET TO WS-CALL-MSG
           MOVE "return receipt" TO D-STEP
           MOVE WS-RETURN-REF TO D-REF
           MOVE RT-QTY-ON-HOLD TO D-AMOUNT
           PERFORM 8100-LOG-CALL THRU 8100-EXIT
           IF WS-CALL-RC > 4
               GO TO 7500-EXIT
           END-IF
           MOVE "INSPECT" TO CA-FUNCTION OF WS-RET
           MOVE WS-RETURN-REF TO RT-RETURN-REF
           MOVE WS-TODAY-N TO RT-INSPECT-DATE
           MOVE 1 TO RT-L-ACC-QTY(1)
           MOVE 0 TO RT-L-REJ-QTY(1)
           MOVE "RESTOCK" TO RT-L-DISPOSITION(1)
           CALL "RETURNS" USING WS-RET
           MOVE CA-RETURN-CODE OF WS-RET TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-RET TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-RET TO WS-CALL-MSG
           MOVE "return inspect" TO D-STEP
           MOVE WS-RETURN-REF TO D-REF
           MOVE RT-QTY-TO-SALEABLE TO D-AMOUNT
           PERFORM 8100-LOG-CALL THRU 8100-EXIT
           IF WS-CALL-RC > 4
               GO TO 7500-EXIT
           END-IF
           MOVE "DISPOSE" TO CA-FUNCTION OF WS-RET
           MOVE WS-RETURN-REF TO RT-RETURN-REF
           CALL "RETURNS" USING WS-RET
           MOVE CA-RETURN-CODE OF WS-RET TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-RET TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-RET TO WS-CALL-MSG
           MOVE "return disposal" TO D-STEP
           MOVE WS-RETURN-REF TO D-REF
           MOVE RT-TOTAL-VALUE-GBP TO D-AMOUNT
           PERFORM 8100-LOG-CALL THRU 8100-EXIT.
       7500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       7600-FIND-DESPATCH SECTION.
       7600-START.
           MOVE "N" TO WS-FOUND
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 d.DespatchRef, d.CustomerCode, "
                  "dl.ItemCode, ISNULL(dl.UomCode,'EA'), "
                  "ISNULL(dl.OrderNumber,''), ISNULL(d.SiteCode,'') "
                  "FROM so.DespatchLine dl JOIN so.Despatch d ON "
                  "d.DespatchId = dl.DespatchId WHERE d.CompanyCode "
                  "= ? AND d.Status = 'CONFIRMED' AND "
                  "dl.DespatchedQty >= 2 AND NOT EXISTS (SELECT 1 "
                  "FROM so.ReturnAuthorisation r WHERE r.CompanyCode "
                  "= d.CompanyCode AND r.OriginalDespatchRef = "
                  "d.DespatchRef) ORDER BY d.DespatchRef, dl.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CUR-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 6 TO SQLIO-COL-CNT
           PERFORM 8900-SET-WIDTHS THRU 8900-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SD-ROW
           IF SQLIO-OK AND SD-COL(1) NOT = SPACES
               MOVE "Y" TO WS-FOUND
               MOVE SD-COL(1) TO WS-DESPATCH
               MOVE SD-COL(2) TO WS-CUSTOMER
               MOVE SD-COL(3) TO IT-ITEM
               MOVE SD-COL(4) TO IT-UOM
               MOVE SD-COL(5) TO WS-ORDER
               IF SD-COL(6) NOT = SPACES
                   MOVE SD-COL(6) TO WS-SITE
               END-IF
           END-IF.
       7600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> Reporting helpers.
      *>----------------------------------------------------------------
       8000-LOG-LINE SECTION.
       8000-START.
           ADD 1 TO WS-SEQ
           MOVE WS-SEQ TO D-SEQ
           MOVE SPACES TO D-SCENARIO
           STRING FUNCTION TRIM(WS-CUR-CO) " "
                  FUNCTION TRIM(WS-SCENARIO)
               DELIMITED BY SIZE INTO D-SCENARIO
           GENERATE DE-LINE.
       8000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8100-LOG-CALL SECTION.
       8100-START.
           ADD 1 TO T-STEPS
           EVALUATE TRUE
               WHEN WS-CALL-RC = 0
                   MOVE "ACCEPTED" TO D-STATUS
                   ADD 1 TO T-OK
               WHEN WS-CALL-RC = 4
                   MOVE "WARNING" TO D-STATUS
                   ADD 1 TO T-WARN
               WHEN WS-CALL-RC = 6
                   MOVE "HELD" TO D-STATUS
                   ADD 1 TO T-WARN
               WHEN OTHER
                   MOVE "REFUSED" TO D-STATUS
                   ADD 1 TO T-FAIL
                   IF WS-RC < 4
                       MOVE 4 TO WS-RC
                   END-IF
           END-EVALUATE
           MOVE SPACES TO D-OUTCOME
           IF WS-CALL-REASON = SPACES
               MOVE WS-CALL-MSG(1:20) TO D-OUTCOME
           ELSE
               STRING FUNCTION TRIM(WS-CALL-REASON) " "
                      WS-CALL-MSG(1:11)
                   DELIMITED BY SIZE INTO D-OUTCOME
           END-IF
           PERFORM 8000-LOG-LINE THRU 8000-EXIT
           IF WS-CALL-RC > 4
               DISPLAY "STERP-W-1855 " FUNCTION TRIM(D-STEP) " "
                       FUNCTION TRIM(WS-CUR-CO) " rc "
                       WS-CALL-RC " " FUNCTION TRIM(WS-CALL-REASON)
                       " " FUNCTION TRIM(WS-CALL-MSG) UPON SYSOUT
           END-IF.
       8100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8900-SET-WIDTHS SECTION.
       8900-START.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 30
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       8900-EXIT.
           EXIT.
