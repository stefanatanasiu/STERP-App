      *>================================================================
      *> PROGRAM-ID : PODEMO
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : DEMONSTRATION TRANSACTION GENERATOR FOR THE
      *>              PURCHASING MODULE.  IT BUILDS A PLAUSIBLE SET OF
      *>              REQUISITIONS, SOURCING EVENTS, ORDERS, RECEIPTS,
      *>              INSPECTIONS, RETURNS, SERVICE ACCEPTANCES AND
      *>              IMPORT ENTRIES SO THAT THE SCREENS, ENQUIRIES
      *>              AND REPORTS HAVE SOMETHING TO SHOW.
      *>
      *>              NOTHING IS INSERTED DIRECTLY.  EVERY STEP GOES
      *>              THROUGH THE ONLINE SERVICE THAT OWNS IT, SO THE
      *>              STOCK LEDGER, THE COMMITMENTS, THE GRNI AND THE
      *>              GENERAL LEDGER ALL COME FROM ONE PATH AND AGREE
      *>              WITH EACH OTHER - THAT IS THE WHOLE POINT OF
      *>              THE JOB.
      *>
      *>              EVERY ORDER CARRIES THE CORRELATION ID
      *>              PODEMO-<COMPANY>, SO A SECOND RUN FINDS ITS OWN
      *>              WORK AND BUILDS NOTHING NEW.  A SEED SCRIPT MAY
      *>              THEREFORE CALL IT ON EVERY DEPLOY.
      *> RUN AS     : PODEMO CO=STC SITE=BHM MODE=COMMIT USER=BUYER1
      *> OUTPUT     : run\spool\PODEMO-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, DATECALC, SQLIO,
      *>              POREQ, SOURCSEL, POORDER, POAPPRV, RECEIPT,
      *>              INSPECT, SUPPRET, SVCACC, IMPVAT, POCLOSE
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2026-10-06 STERP ORIGINAL - DEMONSTRATION DATA GENERATOR
      *>================================================================
      *> REQ: PO-001, PO-002, PO-003, PO-006, PO-008, PO-010, PO-012,
      *> REQ: PO-014, PO-018, PO-024, PO-025, PO-026
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PODEMO.
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
       FD  RPT-FILE REPORT IS R-PODEMO.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY STDATES.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATECALC==.
       COPY POREQCA REPLACING ==DFHCOMMAREA== BY ==WS-POREQ==.
       COPY SOURCSELCA REPLACING ==DFHCOMMAREA== BY ==WS-SOURCSEL==.
       COPY POORDERCA REPLACING ==DFHCOMMAREA== BY ==WS-POORDER==.
       COPY POAPPRVCA REPLACING ==DFHCOMMAREA== BY ==WS-POAPPRV==.
       COPY RECEIPTCA REPLACING ==DFHCOMMAREA== BY ==WS-RECEIPT==.
       COPY INSPECTCA REPLACING ==DFHCOMMAREA== BY ==WS-INSPECT==.
       COPY SUPPRETCA REPLACING ==DFHCOMMAREA== BY ==WS-SUPPRET==.
       COPY SVCACCCA REPLACING ==DFHCOMMAREA== BY ==WS-SVCACC==.
       COPY IMPVATCA REPLACING ==DFHCOMMAREA== BY ==WS-IMPVAT==.
       COPY POCLOSECA REPLACING ==DFHCOMMAREA== BY ==WS-POCLOSE==.
       01  PD-ROW.
           05  PD-COL OCCURS 30 TIMES PIC X(100).
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-SITE                 PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-APPROVER             PIC X(20) VALUE SPACES.
           05  WS-MODE                 PIC X(8) VALUE "PREVIEW".
               88  WS-PREVIEW                    VALUE "PREVIEW".
               88  WS-COMMIT                     VALUE "COMMIT".
       01  WS-SCRATCH.
           05  WS-IX                   PIC S9(4) VALUE 0.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-TODAY-N              PIC 9(8) VALUE 0.
           05  WS-WORK-DATE            PIC 9(8) VALUE 0.
           05  WS-DUE-DATE             PIC 9(8) VALUE 0.
           05  WS-BACK-DATE            PIC 9(8) VALUE 0.
           05  WS-CORR                 PIC X(36) VALUE SPACES.
           05  WS-STEP-CORR            PIC X(36) VALUE SPACES.
           05  WS-CORR-SEQ             PIC 9(3) VALUE 0.
           05  WS-ALREADY              PIC X VALUE "N".
           05  WS-SEQ                  PIC 9(3) VALUE 0.
           05  WS-WHS                  PIC X(20) VALUE SPACES.
           05  WS-LOCN                 PIC X(20) VALUE SPACES.
           05  WS-QUAR-LOCN            PIC X(20) VALUE SPACES.
           05  WS-RET-LOCN             PIC X(20) VALUE SPACES.
           05  WS-RET-COND             PIC X(12) VALUE SPACES.
           05  WS-ORD                  PIC X(30) VALUE SPACES.
           05  WS-BLANKET              PIC X(30) VALUE SPACES.
           05  WS-REQ                  PIC X(30) VALUE SPACES.
           05  WS-RCPT                 PIC X(30) VALUE SPACES.
           05  WS-SVC-ORD              PIC X(30) VALUE SPACES.
           05  WS-IMP-ORD              PIC X(30) VALUE SPACES.
           05  WS-IMP-RCPT             PIC X(30) VALUE SPACES.
           05  WS-SRC-CNT              PIC S9(4) VALUE 0.
           05  WS-SX                   PIC S9(4) VALUE 0.
           05  WS-FAILED-STEP          PIC X VALUE "N".
           05  WS-DATE-ED              PIC X(10) VALUE SPACES.
           05  WS-ED-DD                PIC 9(2) VALUE 0.
           05  WS-ED-MM                PIC 9(2) VALUE 0.
           05  WS-ED-YYYY              PIC 9(4) VALUE 0.
           05  WS-SRC-CNT-ED           PIC ZZ9 VALUE SPACES.
           05  WS-CALL-RC              PIC 9(2) VALUE 0.
           05  WS-CALL-REASON          PIC X(8) VALUE SPACES.
           05  WS-CALL-MSG             PIC X(200) VALUE SPACES.
           05  WS-C-TYPE               PIC X(12) VALUE SPACES.
           05  WS-C-SUPP               PIC X(20) VALUE SPACES.
           05  WS-C-ITEM               PIC X(40) VALUE SPACES.
           05  WS-C-UOM                PIC X(10) VALUE SPACES.
           05  WS-C-DESC               PIC X(60) VALUE SPACES.
           05  WS-C-CURR               PIC X(3) VALUE SPACES.
           05  WS-C-PRICE              PIC S9(11)V9(6) VALUE 0.
           05  WS-C-QTY                PIC S9(13)V9(6) VALUE 0.
           05  WS-C-LINETYPE           PIC X(10) VALUE SPACES.
           05  WS-C-SCEN               PIC X(22) VALUE SPACES.
           05  WS-C-LOT                PIC X(30) VALUE SPACES.
           05  WS-C-REF                PIC X(40) VALUE SPACES.
           05  WS-PICK                 PIC S9(4) VALUE 0.
           05  WS-RCPT-QTY             PIC S9(13)V9(6) VALUE 0.
           05  WS-LINE-NO              PIC S9(9) VALUE 0.
      *>   The supplier/item pairs are extracted into this table
      *>   before any service is called.  The SQLIO cursors are a
      *>   shared resource and an online service opens its own, so a
      *>   cursor must never be left open across a CALL.
       01  WS-SRC-TABLE.
           05  WS-SRC OCCURS 24 TIMES.
               10  S-SUPPLIER          PIC X(20).
               10  S-ITEM              PIC X(40).
               10  S-UOM               PIC X(10).
               10  S-DESC              PIC X(60).
               10  S-CURR              PIC X(3).
               10  S-PRICE             PIC S9(11)V9(6).
       01  WS-DET.
           05  D-SEQ                   PIC 9(3) VALUE 0.
           05  D-SCENARIO              PIC X(22) VALUE SPACES.
           05  D-STEP                  PIC X(18) VALUE SPACES.
           05  D-REF                   PIC X(30) VALUE SPACES.
           05  D-AMOUNT                PIC S9(11)V99 VALUE 0.
           05  D-STATUS                PIC X(14) VALUE SPACES.
           05  D-OUTCOME               PIC X(21) VALUE SPACES.
       01  WS-TOT.
           05  T-STEPS                 PIC S9(5) VALUE 0.
           05  T-OK                    PIC S9(5) VALUE 0.
           05  T-WARN                  PIC S9(5) VALUE 0.
           05  T-FAIL                  PIC S9(5) VALUE 0.
           05  T-ORDERS                PIC S9(5) VALUE 0.
           05  T-RECEIPTS              PIC S9(5) VALUE 0.
           05  T-VALUE                 PIC S9(13)V99 VALUE 0.
       REPORT SECTION.
       RD  R-PODEMO
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
               10  COLUMN 40   PIC X(20) VALUE "Receipts confirmed".
               10  COLUMN 62   PIC ZZZ,ZZ9 SOURCE T-RECEIPTS.
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
           MOVE CM-SITE TO WS-SITE
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
               MOVE "BUYER1" TO WS-USER
           END-IF
           MOVE "PURMGR" TO WS-APPROVER
           IF NOT WS-COMMIT
               MOVE "PREVIEW" TO WS-MODE
           END-IF
           MOVE SPACES TO WS-CORR
           STRING "PODEMO-" FUNCTION TRIM(WS-CO)
               DELIMITED BY SIZE INTO WS-CORR
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           DISPLAY "STERP-I-1800 PODEMO purchasing demonstration "
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
           MOVE "PODEMO" TO JL-JOB-NAME
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
           MOVE "PODEMO" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1801 cannot build the spool path"
                   UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           PERFORM 0350-FILL-HEADINGS THRU 0350-EXIT
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1802 cannot open " WS-SPOOL-NAME
                   UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           INITIATE R-PODEMO.
       0300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "RPT-010" TO RPT-REPORT-ID
           MOVE "Purchasing demonstration transaction generator"
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
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 30
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PD-ROW
           IF SQLIO-OK
               MOVE PD-COL(1) TO RPT-COMPANY-NAME
               MOVE PD-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-PODEMO
           CLOSE RPT-FILE
           DISPLAY "STERP-I-1803 " T-STEPS " service call(s), "
                   T-OK " accepted, " T-WARN " warned, "
                   T-FAIL " refused" UPON SYSOUT
           DISPLAY "STERP-I-1804 " T-ORDERS " order(s), "
                   T-RECEIPTS " receipt(s) confirmed" UPON SYSOUT
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
           MOVE "PODEMO" TO JL-JOB-NAME
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
      *> The demonstration itself.
      *>----------------------------------------------------------------
       1000-PRODUCE SECTION.
       1000-START.
           GENERATE PH-COLS
           PERFORM 1050-ALREADY-BUILT THRU 1050-EXIT
           IF WS-ALREADY = "Y"
               MOVE SPACES TO RPT-STATUS-TEXT
               STRING "The demonstration data for "
                      FUNCTION TRIM(WS-CO)
                      " is already in place - nothing was built."
                   DELIMITED BY SIZE INTO RPT-STATUS-TEXT
               GENERATE DE-NOTE
               GO TO 1000-EXIT
           END-IF
           PERFORM 1060-LOAD-SITE THRU 1060-EXIT
           PERFORM 1070-LOAD-SOURCES THRU 1070-EXIT
           IF WS-SRC-CNT < 3
               MOVE 8 TO WS-RC
               MOVE SPACES TO RPT-STATUS-TEXT
               STRING "No approved supplier source was found for "
                      FUNCTION TRIM(WS-CO)
                      " - run the purchasing seed first."
                   DELIMITED BY SIZE INTO RPT-STATUS-TEXT
               GENERATE DE-NOTE
               GO TO 1000-EXIT
           END-IF
           PERFORM 1080-WORKING-DATES THRU 1080-EXIT
           IF NOT WS-COMMIT
               MOVE SPACES TO RPT-STATUS-TEXT
               STRING "PREVIEW - " WS-SRC-CNT-ED
                      " approved source(s) are available; run with "
                      "MODE=COMMIT to build the demonstration."
                   DELIMITED BY SIZE INTO RPT-STATUS-TEXT
               GENERATE DE-NOTE
               GO TO 1000-EXIT
           END-IF
           PERFORM 2000-REQUISITION THRU 2000-EXIT
           PERFORM 2100-SOURCING THRU 2100-EXIT
           PERFORM 2200-DRAFT-ORDER THRU 2200-EXIT
           PERFORM 2300-AWAITING THRU 2300-EXIT
           PERFORM 2400-APPROVED THRU 2400-EXIT
           PERFORM 2500-ISSUED THRU 2500-EXIT
           PERFORM 2600-ACKNOWLEDGED THRU 2600-EXIT
           PERFORM 2700-AMENDED THRU 2700-EXIT
           PERFORM 2800-CANCELLED THRU 2800-EXIT
           PERFORM 3000-RECEIVED-CLOSED THRU 3000-EXIT
           PERFORM 3500-BLANKET THRU 3500-EXIT
           PERFORM 3700-SERVICE THRU 3700-EXIT
           PERFORM 3800-SUBCONTRACT THRU 3800-EXIT
           MOVE SPACES TO RPT-STATUS-TEXT
           STRING "Rerunning this job builds nothing new - the "
                  "orders carry the correlation id "
                  FUNCTION TRIM(WS-CORR) "."
               DELIMITED BY SIZE INTO RPT-STATUS-TEXT
           GENERATE DE-NOTE
           IF T-FAIL > 0 AND WS-RC < 4
               MOVE 4 TO WS-RC
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A second run must build nothing.  Every order this job makes
      *> carries a correlation id that starts PODEMO-<company>-, so
      *> one count settles it.
      *>----------------------------------------------------------------
       1050-ALREADY-BUILT SECTION.
       1050-START.
           MOVE "N" TO WS-ALREADY
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT COUNT(*) FROM po.PurchaseOrder WHERE "
                  "CompanyCode = ? AND CorrelationId LIKE ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SPACES TO SQLIO-PARM-VAL(2)
           STRING FUNCTION TRIM(WS-CORR) "-%"
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8900-SET-WIDTHS THRU 8900-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PD-ROW
           IF SQLIO-OK AND FUNCTION NUMVAL(PD-COL(1)) > 0
               MOVE "Y" TO WS-ALREADY
               DISPLAY "STERP-I-1805 demonstration data already "
                       "present for " FUNCTION TRIM(WS-CO)
                   UPON SYSOUT
           END-IF.
       1050-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1060-LOAD-SITE SECTION.
       1060-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 w.SiteCode, w.WarehouseCode, "
                  "ISNULL((SELECT TOP 1 l.LocationCode FROM "
                  "inv.Location l WHERE l.CompanyCode = w."
                  "CompanyCode AND l.WarehouseCode = w."
                  "WarehouseCode AND l.LocationType IN ('GOODSIN', "
                  "'STOCK') ORDER BY CASE WHEN l.LocationType = "
                  "'GOODSIN' THEN 0 ELSE 1 END, l.LocationCode),''),"
                  " ISNULL((SELECT TOP 1 q.LocationCode FROM "
                  "inv.Location q WHERE q.CompanyCode = w."
                  "CompanyCode AND q.WarehouseCode = w."
                  "WarehouseCode AND q.LocationType IN "
                  "('QUARANTINE', 'INSPECTION') ORDER BY CASE WHEN "
                  "q.LocationType = 'QUARANTINE' THEN 0 ELSE 1 END, "
                  "q.LocationCode),'') "
                  "FROM inv.Warehouse w WHERE w.CompanyCode = ? "
                  "AND (? = '' OR w.SiteCode = ?) "
                  "AND w.WarehouseCode = 'MAIN' ORDER BY w.SiteCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-SITE TO SQLIO-PARM-VAL(2)
           MOVE WS-SITE TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 4 TO SQLIO-COL-CNT
           PERFORM 8900-SET-WIDTHS THRU 8900-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PD-ROW
           IF SQLIO-OK
               MOVE PD-COL(1) TO WS-SITE
               MOVE PD-COL(2) TO WS-WHS
               MOVE PD-COL(3) TO WS-LOCN
               MOVE PD-COL(4) TO WS-QUAR-LOCN
           END-IF
           IF WS-WHS = SPACES
               MOVE "MAIN" TO WS-WHS
           END-IF
           IF WS-LOCN = SPACES
               MOVE "GOODSIN" TO WS-LOCN
           END-IF
           IF WS-QUAR-LOCN = SPACES
               MOVE WS-LOCN TO WS-QUAR-LOCN
           END-IF.
       1060-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> The sources are extracted into WORKING-STORAGE first.  An
      *> online service opens its own SQLIO cursor, so this program
      *> must not be holding one when it calls POREQ or POORDER.
      *>----------------------------------------------------------------
       1070-LOAD-SOURCES SECTION.
       1070-START.
           MOVE 0 TO WS-SRC-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 24 s.SupplierCode, s.ItemCode, "
                  "ISNULL(s.OrderUomCode,i.StockUomCode), "
                  "i.Description, ISNULL(s.CurrencyCode,'GBP'), "
                  "CONVERT(varchar(30),ISNULL((SELECT TOP 1 "
                  "p.UnitPrice FROM po.PurchasePrice p WHERE "
                  "p.CompanyCode = s.CompanyCode AND p.SupplierCode "
                  "= s.SupplierCode AND p.ItemCode = s.ItemCode AND "
                  "p.IsActive = 1 ORDER BY p.Priority),0)) "
                  "FROM po.SupplierSource s JOIN inv.Item i ON "
                  "i.CompanyCode = s.CompanyCode AND i.ItemCode = "
                  "s.ItemCode WHERE s.CompanyCode = ? AND "
                  "s.ApprovalStatus = 'APPROVED' AND s.IsActive = 1 "
                  "AND i.LifecycleStatus = 'RELEASED' AND "
                  "i.IsStocked = 1 AND i.IsPurchasable = 1 "
                  "ORDER BY s.ItemCode, s.Priority, s.SupplierCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 6 TO SQLIO-COL-CNT
           PERFORM 8900-SET-WIDTHS THRU 8900-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PD-ROW
           IF NOT SQLIO-OK
               GO TO 1070-EXIT
           END-IF
           PERFORM UNTIL WS-SRC-CNT >= 24
               MOVE "FTCH" TO SQLIO-FUNC
               MOVE 8 TO SQLIO-CURSOR
               CALL "SQLIO" USING SQLIO-CA PD-ROW
               IF NOT SQLIO-OK
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-SRC-CNT
               MOVE PD-COL(1) TO S-SUPPLIER(WS-SRC-CNT)
               MOVE PD-COL(2) TO S-ITEM(WS-SRC-CNT)
               MOVE PD-COL(3) TO S-UOM(WS-SRC-CNT)
               MOVE PD-COL(4) TO S-DESC(WS-SRC-CNT)
               MOVE PD-COL(5) TO S-CURR(WS-SRC-CNT)
               COMPUTE S-PRICE(WS-SRC-CNT) =
                   FUNCTION NUMVAL(PD-COL(6))
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           MOVE 8 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA PD-ROW
           MOVE WS-SRC-CNT TO WS-SRC-CNT-ED
           DISPLAY "STERP-I-1806 " WS-SRC-CNT
                   " approved source(s) loaded" UPON SYSOUT.
       1070-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1080-WORKING-DATES SECTION.
       1080-START.
           INITIALIZE WS-DATECALC
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DATECALC
           MOVE WS-CO TO CA-COMPANY OF WS-DATECALC
           MOVE WS-TODAY-N TO DC-DATE-1
           MOVE 14 TO DC-NUMBER
           CALL "DATECALC" USING WS-DATECALC
           IF CA-RETURN-CODE OF WS-DATECALC > 4
               MOVE WS-TODAY-N TO WS-DUE-DATE
           ELSE
               MOVE DC-RESULT-DATE TO WS-DUE-DATE
           END-IF
           INITIALIZE WS-DATECALC
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DATECALC
           MOVE WS-CO TO CA-COMPANY OF WS-DATECALC
           MOVE WS-TODAY-N TO DC-DATE-1
           MOVE -10 TO DC-NUMBER
           CALL "DATECALC" USING WS-DATECALC
           IF CA-RETURN-CODE OF WS-DATECALC > 4
               MOVE WS-TODAY-N TO WS-BACK-DATE
           ELSE
               MOVE DC-RESULT-DATE TO WS-BACK-DATE
           END-IF.
       1080-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> Scenario 1 - PO-001/PO-002 requisition to approval.
      *>----------------------------------------------------------------
       2000-REQUISITION SECTION.
       2000-START.
           MOVE "Requisition" TO D-SCENARIO
           MOVE 1 TO WS-SX
           INITIALIZE WS-POREQ
           PERFORM 8100-REQ-HEADER THRU 8100-EXIT
           MOVE "POREQ" TO CA-PROGRAM OF WS-POREQ
           MOVE "CREATE" TO CA-FUNCTION OF WS-POREQ
           MOVE WS-USER TO RQ-REQUESTER
           MOVE WS-TODAY-N TO RQ-REQ-DATE
           MOVE WS-DUE-DATE TO RQ-NEEDED-BY
           MOVE "MANUAL" TO RQ-DEMAND-TYPE
           MOVE "PODEMO demonstration requisition" TO RQ-DEMAND-REF
           MOVE "Consumables and raw material for the demo build"
               TO RQ-PURPOSE
           CALL "POREQ" USING WS-POREQ
           PERFORM 8050-FROM-REQ THRU 8050-EXIT
           MOVE "CREATE" TO D-STEP
           MOVE RQ-REQ-NUMBER TO D-REF WS-REQ
           MOVE 0 TO D-AMOUNT
           MOVE RQ-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT
           IF WS-FAILED-STEP = "Y"
               GO TO 2000-EXIT
           END-IF
           PERFORM VARYING WS-SX FROM 1 BY 1 UNTIL WS-SX > 2
               INITIALIZE WS-POREQ
               PERFORM 8100-REQ-HEADER THRU 8100-EXIT
               MOVE "ADDLINE" TO CA-FUNCTION OF WS-POREQ
               MOVE WS-REQ TO RQ-REQ-NUMBER
               MOVE WS-SX TO RQ-LINE-NUM
               MOVE S-ITEM(WS-SX) TO RQ-ITEM-CODE
               MOVE S-DESC(WS-SX) TO RQ-DESCRIPTION
               COMPUTE RQ-QUANTITY = 10 * WS-SX
               MOVE S-UOM(WS-SX) TO RQ-UOM-CODE
               MOVE S-PRICE(WS-SX) TO RQ-EST-UNIT-PRICE
               MOVE S-SUPPLIER(WS-SX) TO RQ-SUGGEST-SUPP
               MOVE WS-DUE-DATE TO RQ-LINE-NEEDED-BY
               CALL "POREQ" USING WS-POREQ
               PERFORM 8050-FROM-REQ THRU 8050-EXIT
               MOVE "ADDLINE" TO D-STEP
               MOVE S-ITEM(WS-SX) TO D-REF
               MOVE RQ-LINE-VALUE-GBP TO D-AMOUNT
               MOVE RQ-STATUS TO D-STATUS
               PERFORM 8000-RECORD THRU 8000-EXIT
           END-PERFORM
           INITIALIZE WS-POREQ
           PERFORM 8100-REQ-HEADER THRU 8100-EXIT
           MOVE "SUBMIT" TO CA-FUNCTION OF WS-POREQ
           MOVE WS-REQ TO RQ-REQ-NUMBER
           MOVE WS-USER TO RQ-REQUESTER
           CALL "POREQ" USING WS-POREQ
           PERFORM 8050-FROM-REQ THRU 8050-EXIT
           MOVE "SUBMIT" TO D-STEP
           MOVE WS-REQ TO D-REF
           MOVE RQ-TOTAL-GBP TO D-AMOUNT
           MOVE RQ-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT
           INITIALIZE WS-POREQ
           PERFORM 8100-REQ-HEADER THRU 8100-EXIT
           MOVE "APPROVE" TO CA-FUNCTION OF WS-POREQ
           MOVE WS-APPROVER TO CA-USER OF WS-POREQ
           MOVE WS-REQ TO RQ-REQ-NUMBER
           MOVE "APPROVE" TO RQ-DECISION
           MOVE WS-APPROVER TO RQ-APPROVER
           MOVE "Demonstration data - budget confirmed"
               TO RQ-REASON
           CALL "POREQ" USING WS-POREQ
           PERFORM 8050-FROM-REQ THRU 8050-EXIT
           MOVE "APPROVE" TO D-STEP
           MOVE WS-REQ TO D-REF
           MOVE RQ-TOTAL-GBP TO D-AMOUNT
           MOVE RQ-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> Scenario 2 - PO-002 sourcing: two quotations, ranked, and a
      *> justified selection.
      *>----------------------------------------------------------------
       2100-SOURCING SECTION.
       2100-START.
           IF WS-REQ = SPACES
               GO TO 2100-EXIT
           END-IF
           MOVE "Sourcing event" TO D-SCENARIO
           INITIALIZE WS-SOURCSEL
           PERFORM 8200-SRC-HEADER THRU 8200-EXIT
           MOVE "OPEN" TO CA-FUNCTION OF WS-SOURCSEL
           MOVE WS-REQ TO SS-REQ-NUMBER
           MOVE 1 TO SS-LINE-NUM
           MOVE S-ITEM(1) TO SS-ITEM-CODE
           MOVE S-DESC(1) TO SS-DESCRIPTION
           MOVE 10 TO SS-QUANTITY
           MOVE S-UOM(1) TO SS-UOM-CODE
           MOVE WS-DUE-DATE TO SS-NEEDED-BY
           MOVE WS-USER TO SS-BUYER
           MOVE WS-TODAY-N TO SS-EVENT-DATE
           CALL "SOURCSEL" USING WS-SOURCSEL
           PERFORM 8250-FROM-SRC THRU 8250-EXIT
           MOVE "OPEN" TO D-STEP
           MOVE SS-SOURCING-REF TO D-REF WS-C-REF
           MOVE 0 TO D-AMOUNT
           MOVE SS-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT
           IF WS-FAILED-STEP = "Y"
               GO TO 2100-EXIT
           END-IF
           PERFORM VARYING WS-SX FROM 1 BY 1 UNTIL WS-SX > 2
               INITIALIZE WS-SOURCSEL
               PERFORM 8200-SRC-HEADER THRU 8200-EXIT
               MOVE "QUOTE" TO CA-FUNCTION OF WS-SOURCSEL
               MOVE WS-C-REF TO SS-SOURCING-REF
               MOVE S-SUPPLIER(WS-SX) TO SS-SUPPLIER
               MOVE "PODEMO-QUOTE" TO SS-QUOTE-REF
               MOVE S-CURR(WS-SX) TO SS-CURRENCY
               COMPUTE SS-UNIT-PRICE =
                   S-PRICE(1) * (1 + (WS-SX - 1) * 0.08)
               MOVE 1 TO SS-PRICE-PER
               MOVE 10 TO SS-QUANTITY
               COMPUTE SS-LEAD-TIME-IN = 7 * WS-SX
               MOVE WS-DUE-DATE TO SS-QUOTE-VALID-TO
               CALL "SOURCSEL" USING WS-SOURCSEL
               PERFORM 8250-FROM-SRC THRU 8250-EXIT
               MOVE "QUOTE" TO D-STEP
               MOVE S-SUPPLIER(WS-SX) TO D-REF
               MOVE SS-LINE-VALUE-GBP TO D-AMOUNT
               MOVE SS-STATUS TO D-STATUS
               PERFORM 8000-RECORD THRU 8000-EXIT
           END-PERFORM
           INITIALIZE WS-SOURCSEL
           PERFORM 8200-SRC-HEADER THRU 8200-EXIT
           MOVE "RANK" TO CA-FUNCTION OF WS-SOURCSEL
           MOVE WS-C-REF TO SS-SOURCING-REF
           CALL "SOURCSEL" USING WS-SOURCSEL
           PERFORM 8250-FROM-SRC THRU 8250-EXIT
           MOVE "RANK" TO D-STEP
           MOVE SS-CHEAPEST-SUPP TO D-REF
           MOVE SS-CHEAPEST-GBP TO D-AMOUNT
           MOVE SS-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT
           INITIALIZE WS-SOURCSEL
           PERFORM 8200-SRC-HEADER THRU 8200-EXIT
           MOVE "SELECT" TO CA-FUNCTION OF WS-SOURCSEL
           MOVE WS-C-REF TO SS-SOURCING-REF
           MOVE S-SUPPLIER(1) TO SS-SUPPLIER
           MOVE "Cheapest compliant quotation on the event"
               TO SS-SELECT-REASON
           MOVE WS-USER TO SS-AUTHORISED-BY
           CALL "SOURCSEL" USING WS-SOURCSEL
           PERFORM 8250-FROM-SRC THRU 8250-EXIT
           MOVE "SELECT" TO D-STEP
           MOVE S-SUPPLIER(1) TO D-REF
           MOVE SS-SELECTED-GBP TO D-AMOUNT
           MOVE SS-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> Scenario 3 - a draft order.  PO-006: a draft is not a
      *> commitment, so nothing reaches po.Commitment here.
      *>----------------------------------------------------------------
       2200-DRAFT-ORDER SECTION.
       2200-START.
           MOVE "Draft order" TO WS-C-SCEN
           MOVE "STANDARD" TO WS-C-TYPE
           MOVE 1 TO WS-PICK
           PERFORM 7000-BUILD-ORDER THRU 7000-EXIT.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> Scenario 4 - waiting for approval (PO-006).
      *>----------------------------------------------------------------
       2300-AWAITING SECTION.
       2300-START.
           MOVE "Awaiting approval" TO WS-C-SCEN
           MOVE "STANDARD" TO WS-C-TYPE
           MOVE 2 TO WS-PICK
           PERFORM 7000-BUILD-ORDER THRU 7000-EXIT
           IF WS-ORD NOT = SPACES
               PERFORM 7300-REQUEST-APPR THRU 7300-EXIT
           END-IF.
       2300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> Scenario 5 - approved but not yet issued (PO-006).
      *>----------------------------------------------------------------
       2400-APPROVED SECTION.
       2400-START.
           MOVE "Approved" TO WS-C-SCEN
           MOVE "STANDARD" TO WS-C-TYPE
           MOVE 3 TO WS-PICK
           PERFORM 7000-BUILD-ORDER THRU 7000-EXIT
           IF WS-ORD NOT = SPACES
               PERFORM 7300-REQUEST-APPR THRU 7300-EXIT
               PERFORM 7400-DECIDE-APPR THRU 7400-EXIT
           END-IF.
       2400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> Scenario 6 - issued: PO-006 writes the commitment here.
      *>----------------------------------------------------------------
       2500-ISSUED SECTION.
       2500-START.
           MOVE "Issued" TO WS-C-SCEN
           MOVE "STANDARD" TO WS-C-TYPE
           MOVE 4 TO WS-PICK
           PERFORM 7000-BUILD-ORDER THRU 7000-EXIT
           IF WS-ORD NOT = SPACES
               PERFORM 7300-REQUEST-APPR THRU 7300-EXIT
               PERFORM 7400-DECIDE-APPR THRU 7400-EXIT
               PERFORM 7500-ISSUE THRU 7500-EXIT
           END-IF.
       2500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> Scenario 7 - acknowledged with a later promise (PO-007).
      *>----------------------------------------------------------------
       2600-ACKNOWLEDGED SECTION.
       2600-START.
           MOVE "Acknowledged" TO WS-C-SCEN
           MOVE "STANDARD" TO WS-C-TYPE
           MOVE 5 TO WS-PICK
           PERFORM 7000-BUILD-ORDER THRU 7000-EXIT
           IF WS-ORD = SPACES
               GO TO 2600-EXIT
           END-IF
           PERFORM 7300-REQUEST-APPR THRU 7300-EXIT
           PERFORM 7400-DECIDE-APPR THRU 7400-EXIT
           PERFORM 7500-ISSUE THRU 7500-EXIT
           INITIALIZE WS-POORDER
           PERFORM 8300-ORD-HEADER THRU 8300-EXIT
           MOVE "ACKNOWL" TO CA-FUNCTION OF WS-POORDER
           MOVE WS-ORD TO OR-ORDER-NUMBER
           MOVE 1 TO OR-LINE-NUM
           MOVE WS-C-QTY TO OR-ACK-QTY
           MOVE WS-C-PRICE TO OR-ACK-PRICE
           MOVE WS-DUE-DATE TO OR-ACK-DATE
           MOVE "PODEMO-ACK" TO OR-SUPPLIER-REF
           MOVE "Acknowledged in full on the requested date"
               TO OR-ACK-NOTE
           CALL "POORDER" USING WS-POORDER
           PERFORM 8350-FROM-ORD THRU 8350-EXIT
           MOVE WS-C-SCEN TO D-SCENARIO
           MOVE "ACKNOWL" TO D-STEP
           MOVE WS-ORD TO D-REF
           MOVE OR-NET-AMOUNT TO D-AMOUNT
           MOVE OR-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT.
       2600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> Scenario 8 - approved, issued and then amended.  PO-008: a
      *> material change leaves the re-approval outstanding.
      *>----------------------------------------------------------------
       2700-AMENDED SECTION.
       2700-START.
           MOVE "Amended after issue" TO WS-C-SCEN
           MOVE "STANDARD" TO WS-C-TYPE
           MOVE 6 TO WS-PICK
           PERFORM 7000-BUILD-ORDER THRU 7000-EXIT
           IF WS-ORD = SPACES
               GO TO 2700-EXIT
           END-IF
           PERFORM 7300-REQUEST-APPR THRU 7300-EXIT
           PERFORM 7400-DECIDE-APPR THRU 7400-EXIT
           PERFORM 7500-ISSUE THRU 7500-EXIT
           INITIALIZE WS-POORDER
           PERFORM 8300-ORD-HEADER THRU 8300-EXIT
           MOVE "AMEND" TO CA-FUNCTION OF WS-POORDER
           MOVE WS-ORD TO OR-ORDER-NUMBER
           MOVE 1 TO OR-LINE-NUM
           MOVE "ORDERED-QTY" TO OR-AMEND-FIELD
           MOVE SPACES TO OR-AMEND-OLD
           MOVE SPACES TO OR-AMEND-NEW
           COMPUTE OR-ORDERED-QTY = WS-C-QTY * 2
           MOVE WS-C-QTY TO OR-ACK-QTY
           MOVE "Production pulled the build forward - extra volume"
               TO OR-AMEND-REASON
           CALL "POORDER" USING WS-POORDER
           PERFORM 8350-FROM-ORD THRU 8350-EXIT
           MOVE WS-C-SCEN TO D-SCENARIO
           MOVE "AMEND" TO D-STEP
           MOVE WS-ORD TO D-REF
           MOVE OR-NET-AMOUNT TO D-AMOUNT
           MOVE OR-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT.
       2700-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> Scenario 9 - cancelled before any receipt (PO-025).
      *>----------------------------------------------------------------
       2800-CANCELLED SECTION.
       2800-START.
           MOVE "Cancelled" TO WS-C-SCEN
           MOVE "STANDARD" TO WS-C-TYPE
           MOVE 7 TO WS-PICK
           PERFORM 7000-BUILD-ORDER THRU 7000-EXIT
           IF WS-ORD = SPACES
               GO TO 2800-EXIT
           END-IF
           PERFORM 7300-REQUEST-APPR THRU 7300-EXIT
           PERFORM 7400-DECIDE-APPR THRU 7400-EXIT
           PERFORM 7500-ISSUE THRU 7500-EXIT
           INITIALIZE WS-POCLOSE
           PERFORM 8400-CLS-HEADER THRU 8400-EXIT
           MOVE "CANCEL" TO CA-FUNCTION OF WS-POCLOSE
           MOVE WS-ORD TO PC-ORDER-NUMBER
           MOVE 1 TO PC-LINE-NUM
           MOVE WS-TODAY-N TO PC-CLOSE-DATE
           MOVE "NOLONGER" TO PC-REASON-CODE
           MOVE "The works order behind this demand was dropped"
               TO PC-REASON
           MOVE WS-APPROVER TO PC-AUTHORITY
           CALL "POCLOSE" USING WS-POCLOSE
           PERFORM 8450-FROM-CLS THRU 8450-EXIT
           MOVE WS-C-SCEN TO D-SCENARIO
           MOVE "CANCEL" TO D-STEP
           MOVE WS-ORD TO D-REF
           MOVE PC-RELEASED-GBP TO D-AMOUNT
           MOVE PC-NEW-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT.
       2800-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> Scenario 10 - the one that matters.  The goods are received
      *> in two deliveries through RECEIPT, so the stock ledger, the
      *> GRNI accrual and the commitment relief all come from the one
      *> path and agree (PO-010, PO-012, PO-018).  The remainder is
      *> short closed and the order is then closed (PO-025).
      *>----------------------------------------------------------------
       3000-RECEIVED-CLOSED SECTION.
       3000-START.
           MOVE "Received and closed" TO WS-C-SCEN
           MOVE "STANDARD" TO WS-C-TYPE
           MOVE 8 TO WS-PICK
           PERFORM 7000-BUILD-ORDER THRU 7000-EXIT
           IF WS-ORD = SPACES
               GO TO 3000-EXIT
           END-IF
           PERFORM 7300-REQUEST-APPR THRU 7300-EXIT
           PERFORM 7400-DECIDE-APPR THRU 7400-EXIT
           PERFORM 7500-ISSUE THRU 7500-EXIT
           COMPUTE WS-RCPT-QTY = WS-C-QTY * 0.4
           MOVE "PODEMO-DN-01" TO WS-C-REF
           PERFORM 7600-RECEIVE THRU 7600-EXIT
           COMPUTE WS-RCPT-QTY = WS-C-QTY * 0.4
           MOVE "PODEMO-DN-02" TO WS-C-REF
           PERFORM 7600-RECEIVE THRU 7600-EXIT
           PERFORM 3100-RETURN-SOME THRU 3100-EXIT
           INITIALIZE WS-POCLOSE
           PERFORM 8400-CLS-HEADER THRU 8400-EXIT
           MOVE "SHORTCLS" TO CA-FUNCTION OF WS-POCLOSE
           MOVE WS-ORD TO PC-ORDER-NUMBER
           MOVE 1 TO PC-LINE-NUM
           MOVE WS-TODAY-N TO PC-CLOSE-DATE
           MOVE "SHORTSUPP" TO PC-REASON-CODE
           MOVE "The supplier will not ship the balance"
               TO PC-REASON
           MOVE WS-APPROVER TO PC-AUTHORITY
           CALL "POCLOSE" USING WS-POCLOSE
           PERFORM 8450-FROM-CLS THRU 8450-EXIT
           MOVE WS-C-SCEN TO D-SCENARIO
           MOVE "SHORTCLS" TO D-STEP
           MOVE WS-ORD TO D-REF
           MOVE PC-RELEASED-GBP TO D-AMOUNT
           MOVE PC-NEW-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT
           INITIALIZE WS-POCLOSE
           PERFORM 8400-CLS-HEADER THRU 8400-EXIT
           MOVE "CLOSE" TO CA-FUNCTION OF WS-POCLOSE
           MOVE WS-ORD TO PC-ORDER-NUMBER
           MOVE 1 TO PC-LINE-NUM
           MOVE WS-TODAY-N TO PC-CLOSE-DATE
           MOVE "COMPLETE" TO PC-REASON-CODE
           MOVE "Delivered quantity accepted and matched"
               TO PC-REASON
           MOVE WS-APPROVER TO PC-AUTHORITY
           MOVE "Y" TO PC-FORCE
           CALL "POCLOSE" USING WS-POCLOSE
           PERFORM 8450-FROM-CLS THRU 8450-EXIT
           MOVE WS-C-SCEN TO D-SCENARIO
           MOVE "CLOSE" TO D-STEP
           MOVE WS-ORD TO D-REF
           MOVE PC-RETAINED-GRNI TO D-AMOUNT
           MOVE PC-NEW-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-014 - part of the second delivery goes back to the
      *> supplier with an expected credit.
      *>----------------------------------------------------------------
       3100-RETURN-SOME SECTION.
       3100-START.
           IF WS-RCPT = SPACES
               GO TO 3100-EXIT
           END-IF
           INITIALIZE WS-SUPPRET
           PERFORM 8500-RET-HEADER THRU 8500-EXIT
           MOVE "CREATE" TO CA-FUNCTION OF WS-SUPPRET
           MOVE WS-C-SUPP TO SR-SUPPLIER
           MOVE WS-RCPT TO SR-RECEIPT-REF
           MOVE WS-ORD TO SR-ORDER-NUMBER
           MOVE WS-TODAY-N TO SR-RETURN-DATE
           MOVE WS-TODAY-N TO SR-POSTING-DATE
           MOVE "DAMAGED" TO SR-REASON-CODE
           MOVE "Two cartons crushed in transit - credit expected"
               TO SR-REASON
           MOVE WS-WHS TO SR-WAREHOUSE
           MOVE "Y" TO SR-POST-INVOICE
           MOVE 1 TO SR-LINE-CNT
           MOVE 1 TO SR-L-NUM(1)
           MOVE 1 TO SR-L-RCPT-LINE(1)
           MOVE WS-C-ITEM TO SR-L-ITEM(1)
           MOVE WS-RET-LOCN TO SR-L-LOCATION(1)
           MOVE WS-RET-COND TO SR-L-CONDITION(1)
           MOVE SPACES TO SR-L-LOT(1)
           MOVE WS-C-LOT TO SR-L-LOT(1)
           COMPUTE SR-L-RETURNED(1) = WS-RCPT-QTY * 0.25
           MOVE WS-C-UOM TO SR-L-UOM(1)
           CALL "SUPPRET" USING WS-SUPPRET
           PERFORM 8550-FROM-RET THRU 8550-EXIT
           MOVE "Supplier return" TO D-SCENARIO
           MOVE "CREATE" TO D-STEP
           MOVE SR-RETURN-REF TO D-REF WS-C-REF
           MOVE SR-EXPECTED-CR-GBP TO D-AMOUNT
           MOVE SR-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT
           IF WS-FAILED-STEP = "Y"
               GO TO 3100-EXIT
           END-IF
      *>   DESPATCH IS AN UPSERT - SUPPRET REVALIDATES THE WHOLE NOTE,
      *>   SO THE LINES STAY IN THE COMMAREA FROM THE CREATE CALL.
      *>   DESPATCH RAISES ITS OWN NOTE - SUPPRET REVALIDATES THE WHOLE
      *>   NOTE AND ALLOCATES A NEW NUMBER, SO THE LINES STAY IN THE
      *>   COMMAREA FROM THE CREATE CALL AND THE REFERENCE IS CLEARED.
           PERFORM 8500-RET-HEADER THRU 8500-EXIT
           MOVE "DESPATCH" TO CA-FUNCTION OF WS-SUPPRET
           MOVE SPACES TO SR-RETURN-REF
           MOVE WS-TODAY-N TO SR-RETURN-DATE
           MOVE WS-TODAY-N TO SR-POSTING-DATE
           MOVE "Collected by the supplier's carrier, POD held"
               TO SR-DESPATCH-EVID
           CALL "SUPPRET" USING WS-SUPPRET
           PERFORM 8550-FROM-RET THRU 8550-EXIT
           MOVE "Supplier return" TO D-SCENARIO
           MOVE "DESPATCH" TO D-STEP
           MOVE SR-RETURN-REF TO D-REF
           MOVE SR-TOTAL-GBP TO D-AMOUNT
           MOVE SR-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT.
       3100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> Scenario 11 - a blanket order and one call-off (PO-024).
      *>----------------------------------------------------------------
       3500-BLANKET SECTION.
       3500-START.
           MOVE "Blanket agreement" TO WS-C-SCEN
           MOVE "BLANKET" TO WS-C-TYPE
           MOVE 9 TO WS-PICK
           PERFORM 7000-BUILD-ORDER THRU 7000-EXIT
           IF WS-ORD = SPACES
               GO TO 3500-EXIT
           END-IF
           MOVE WS-ORD TO WS-BLANKET
           PERFORM 7300-REQUEST-APPR THRU 7300-EXIT
           PERFORM 7400-DECIDE-APPR THRU 7400-EXIT
           PERFORM 7500-ISSUE THRU 7500-EXIT
           INITIALIZE WS-POORDER
           PERFORM 8300-ORD-HEADER THRU 8300-EXIT
           MOVE "CALLOFF" TO CA-FUNCTION OF WS-POORDER
           MOVE WS-BLANKET TO OR-BLANKET-NUMBER
           MOVE "CALLOFF" TO OR-ORDER-TYPE
           MOVE WS-C-SUPP TO OR-SUPPLIER
           MOVE WS-USER TO OR-BUYER
           MOVE WS-TODAY-N TO OR-ORDER-DATE
           MOVE WS-C-CURR TO OR-CURRENCY
           MOVE 1 TO OR-LINE-NUM
           MOVE "STOCK" TO OR-LINE-TYPE
           MOVE WS-C-ITEM TO OR-ITEM-CODE
           MOVE WS-C-DESC TO OR-DESCRIPTION
           COMPUTE OR-ORDERED-QTY = WS-C-QTY / 4
           MOVE WS-C-UOM TO OR-UOM-CODE
           MOVE WS-SITE TO OR-DEST-SITE
           MOVE WS-LOCN TO OR-DEST-LOCATION
           MOVE WS-DUE-DATE TO OR-SCH-REQ-DATE
           MOVE "Y" TO OR-AUTO-PRICE
           CALL "POORDER" USING WS-POORDER
           PERFORM 8350-FROM-ORD THRU 8350-EXIT
           MOVE "Blanket call-off" TO D-SCENARIO
           MOVE "CALLOFF" TO D-STEP
           MOVE OR-ORDER-NUMBER TO D-REF
           MOVE OR-NET-AMOUNT TO D-AMOUNT
           MOVE OR-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT
           IF WS-CALL-RC < 8
               ADD 1 TO T-ORDERS
           END-IF.
       3500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> Scenario 12 - a service order accepted on a milestone
      *> (PO-026).  No stock moves; the cost is accrued.
      *>----------------------------------------------------------------
       3700-SERVICE SECTION.
       3700-START.
           MOVE "Service order" TO WS-C-SCEN
           MOVE "SERVICE" TO WS-C-TYPE
           MOVE 10 TO WS-PICK
           MOVE "SERVICE" TO WS-C-LINETYPE
           PERFORM 7000-BUILD-ORDER THRU 7000-EXIT
           MOVE SPACES TO WS-C-LINETYPE
           IF WS-ORD = SPACES
               GO TO 3700-EXIT
           END-IF
           MOVE WS-ORD TO WS-SVC-ORD
           PERFORM 7300-REQUEST-APPR THRU 7300-EXIT
           PERFORM 7400-DECIDE-APPR THRU 7400-EXIT
           PERFORM 7500-ISSUE THRU 7500-EXIT
           INITIALIZE WS-SVCACC
           MOVE "SVCACC" TO CA-PROGRAM OF WS-SVCACC
           MOVE "ACCEPT" TO CA-FUNCTION OF WS-SVCACC
           MOVE WS-CO TO CA-COMPANY OF WS-SVCACC
           MOVE WS-SITE TO CA-SITE OF WS-SVCACC
           MOVE WS-USER TO CA-USER OF WS-SVCACC
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-SVCACC
           PERFORM 8099-NEXT-CORR THRU 8099-EXIT
           MOVE WS-STEP-CORR TO CA-CORRELATION-ID OF WS-SVCACC
           MOVE WS-SVC-ORD TO SA-ORDER-NUMBER
           MOVE 1 TO SA-ORDER-LINE
           MOVE "PERCENT" TO SA-BASIS
           COMPUTE SA-ORDER-VALUE = WS-C-QTY * WS-C-PRICE
           MOVE 50 TO SA-ACCEPT-PCT
           MOVE WS-TODAY-N TO SA-ACCEPT-DATE
           MOVE WS-TODAY-N TO SA-POSTING-DATE
           MOVE "Stage 1 signed off by the project engineer"
               TO SA-EVIDENCE
           MOVE WS-APPROVER TO SA-ACCEPTED-BY
           MOVE WS-USER TO SA-REQUESTED-BY
           CALL "SVCACC" USING WS-SVCACC
           MOVE CA-RETURN-CODE OF WS-SVCACC TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-SVCACC TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-SVCACC TO WS-CALL-MSG
           MOVE "Service acceptance" TO D-SCENARIO
           MOVE "ACCEPT" TO D-STEP
           MOVE WS-SVC-ORD TO D-REF
           MOVE SA-ACCEPT-GBP TO D-AMOUNT
           MOVE SA-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT.
       3700-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> Scenario 13 - a subcontract order (PO-003 order types).
      *>----------------------------------------------------------------
       3800-SUBCONTRACT SECTION.
       3800-START.
           MOVE "Subcontract order" TO WS-C-SCEN
           MOVE "SUBCONTRACT" TO WS-C-TYPE
           MOVE 11 TO WS-PICK
           MOVE "SUBCON" TO WS-C-LINETYPE
           PERFORM 7000-BUILD-ORDER THRU 7000-EXIT
           MOVE SPACES TO WS-C-LINETYPE
           IF WS-ORD = SPACES
               GO TO 3800-EXIT
           END-IF
           PERFORM 7300-REQUEST-APPR THRU 7300-EXIT
           PERFORM 7400-DECIDE-APPR THRU 7400-EXIT
           PERFORM 7500-ISSUE THRU 7500-EXIT.
       3800-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> Order builder - header, one line and one schedule.
      *>----------------------------------------------------------------
       7000-BUILD-ORDER SECTION.
       7000-START.
           MOVE SPACES TO WS-ORD
           PERFORM 7050-PICK-SOURCE THRU 7050-EXIT
           MOVE WS-C-SCEN TO D-SCENARIO
           INITIALIZE WS-POORDER
           PERFORM 8300-ORD-HEADER THRU 8300-EXIT
           MOVE "CREATE" TO CA-FUNCTION OF WS-POORDER
           MOVE WS-C-TYPE TO OR-ORDER-TYPE
           MOVE WS-C-SUPP TO OR-SUPPLIER
           MOVE WS-USER TO OR-BUYER
           MOVE WS-TODAY-N TO OR-ORDER-DATE
           MOVE WS-C-CURR TO OR-CURRENCY
           MOVE "DAP" TO OR-INCOTERMS
           MOVE "Goods inwards, main gate" TO OR-NAMED-PLACE
           MOVE SPACES TO OR-SUPPLIER-REF
           MOVE WS-PICK TO WS-SRC-CNT-ED
           STRING "PODEMO-" FUNCTION TRIM(WS-SRC-CNT-ED)
               DELIMITED BY SIZE INTO OR-SUPPLIER-REF
           CALL "POORDER" USING WS-POORDER
           PERFORM 8350-FROM-ORD THRU 8350-EXIT
           MOVE "CREATE" TO D-STEP
           MOVE OR-ORDER-NUMBER TO D-REF
           MOVE 0 TO D-AMOUNT
           MOVE OR-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT
           IF WS-FAILED-STEP = "Y"
               GO TO 7000-EXIT
           END-IF
           MOVE OR-ORDER-NUMBER TO WS-ORD
           ADD 1 TO T-ORDERS
           PERFORM 7100-ADD-LINE THRU 7100-EXIT
           IF WS-FAILED-STEP = "Y"
               GO TO 7000-EXIT
           END-IF
           PERFORM 7200-SCHEDULE THRU 7200-EXIT.
       7000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       7050-PICK-SOURCE SECTION.
       7050-START.
           COMPUTE WS-SX =
               FUNCTION MOD(WS-PICK - 1, WS-SRC-CNT) + 1
           MOVE S-SUPPLIER(WS-SX) TO WS-C-SUPP
           MOVE S-ITEM(WS-SX) TO WS-C-ITEM
           MOVE S-UOM(WS-SX) TO WS-C-UOM
           MOVE S-DESC(WS-SX) TO WS-C-DESC
           MOVE S-CURR(WS-SX) TO WS-C-CURR
           MOVE S-PRICE(WS-SX) TO WS-C-PRICE
           IF WS-C-PRICE = 0
               MOVE 12.5 TO WS-C-PRICE
           END-IF
           COMPUTE WS-C-QTY = 20 + (WS-PICK * 5).
       7050-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       7100-ADD-LINE SECTION.
       7100-START.
           INITIALIZE WS-POORDER
           PERFORM 8300-ORD-HEADER THRU 8300-EXIT
           MOVE "ADDLINE" TO CA-FUNCTION OF WS-POORDER
           MOVE WS-ORD TO OR-ORDER-NUMBER
           MOVE 1 TO OR-LINE-NUM
           IF WS-C-LINETYPE = SPACES
               MOVE "STOCK" TO OR-LINE-TYPE
               MOVE WS-C-ITEM TO OR-ITEM-CODE
           ELSE
               MOVE WS-C-LINETYPE TO OR-LINE-TYPE
               MOVE SPACES TO OR-ITEM-CODE
               MOVE "60100" TO OR-ACCOUNT-CODE
           END-IF
           MOVE WS-C-DESC TO OR-DESCRIPTION
           MOVE WS-C-QTY TO OR-ORDERED-QTY
           MOVE WS-C-UOM TO OR-UOM-CODE
           MOVE WS-C-PRICE TO OR-UNIT-PRICE
           MOVE 1 TO OR-PRICE-PER
           MOVE WS-SITE TO OR-DEST-SITE
           MOVE WS-LOCN TO OR-DEST-LOCATION
           MOVE "Y" TO OR-AUTO-PRICE
           CALL "POORDER" USING WS-POORDER
           PERFORM 8350-FROM-ORD THRU 8350-EXIT
           MOVE "ADDLINE" TO D-STEP
           MOVE WS-C-ITEM TO D-REF
           IF WS-C-LINETYPE NOT = SPACES
               MOVE WS-C-DESC TO D-REF
           END-IF
           MOVE OR-LINE-NET-GBP TO D-AMOUNT
           MOVE OR-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT
           IF WS-CALL-RC < 8
               MOVE OR-UNIT-PRICE TO WS-C-PRICE
               ADD OR-LINE-NET-GBP TO T-VALUE
           END-IF.
       7100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       7200-SCHEDULE SECTION.
       7200-START.
           INITIALIZE WS-POORDER
           PERFORM 8300-ORD-HEADER THRU 8300-EXIT
           MOVE "SCHEDULE" TO CA-FUNCTION OF WS-POORDER
           MOVE WS-ORD TO OR-ORDER-NUMBER
           MOVE 1 TO OR-LINE-NUM
           MOVE 1 TO OR-SCHEDULE-NO
           MOVE WS-C-QTY TO OR-SCH-QTY
           MOVE WS-DUE-DATE TO OR-SCH-REQ-DATE
           MOVE WS-SITE TO OR-SCH-DEST-SITE
           MOVE WS-LOCN TO OR-SCH-DEST-LOCN
           CALL "POORDER" USING WS-POORDER
           PERFORM 8350-FROM-ORD THRU 8350-EXIT
           MOVE "SCHEDULE" TO D-STEP
           MOVE WS-ORD TO D-REF
           MOVE 0 TO D-AMOUNT
           MOVE OR-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT.
       7200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       7300-REQUEST-APPR SECTION.
       7300-START.
           INITIALIZE WS-POAPPRV
           MOVE "POAPPRV" TO CA-PROGRAM OF WS-POAPPRV
           MOVE "REQUEST" TO CA-FUNCTION OF WS-POAPPRV
           MOVE WS-CO TO CA-COMPANY OF WS-POAPPRV
           MOVE WS-SITE TO CA-SITE OF WS-POAPPRV
           MOVE WS-USER TO CA-USER OF WS-POAPPRV
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-POAPPRV
           PERFORM 8099-NEXT-CORR THRU 8099-EXIT
           MOVE WS-STEP-CORR TO CA-CORRELATION-ID OF WS-POAPPRV
           MOVE WS-ORD TO AV-ORDER-NUMBER
           MOVE "PURCHASEORDER" TO AV-APPROVAL-TYPE
           MOVE WS-USER TO AV-REQUESTED-BY
           CALL "POAPPRV" USING WS-POAPPRV
           PERFORM 8650-FROM-APPR THRU 8650-EXIT
           MOVE WS-C-SCEN TO D-SCENARIO
           MOVE "APPROVAL REQUEST" TO D-STEP
           MOVE WS-ORD TO D-REF
           MOVE AV-AMOUNT-GBP TO D-AMOUNT
           MOVE AV-APPROVAL-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT.
       7300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-006 - the approver is never the raiser.
      *>----------------------------------------------------------------
       7400-DECIDE-APPR SECTION.
       7400-START.
           INITIALIZE WS-POAPPRV
           MOVE "POAPPRV" TO CA-PROGRAM OF WS-POAPPRV
           MOVE "DECIDE" TO CA-FUNCTION OF WS-POAPPRV
           MOVE WS-CO TO CA-COMPANY OF WS-POAPPRV
           MOVE WS-SITE TO CA-SITE OF WS-POAPPRV
           MOVE WS-APPROVER TO CA-USER OF WS-POAPPRV
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-POAPPRV
           PERFORM 8099-NEXT-CORR THRU 8099-EXIT
           MOVE WS-STEP-CORR TO CA-CORRELATION-ID OF WS-POAPPRV
           MOVE WS-ORD TO AV-ORDER-NUMBER
           MOVE WS-APPROVER TO AV-APPROVER
           MOVE "APPROVE" TO AV-DECISION
           MOVE "Demonstration order - priced on agreement"
               TO AV-COMMENT
           CALL "POAPPRV" USING WS-POAPPRV
           PERFORM 8650-FROM-APPR THRU 8650-EXIT
           MOVE WS-C-SCEN TO D-SCENARIO
           MOVE "APPROVE" TO D-STEP
           MOVE WS-ORD TO D-REF
           MOVE AV-AMOUNT-GBP TO D-AMOUNT
           MOVE AV-APPROVAL-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT.
       7400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       7500-ISSUE SECTION.
       7500-START.
           INITIALIZE WS-POORDER
           PERFORM 8300-ORD-HEADER THRU 8300-EXIT
           MOVE "ISSUE" TO CA-FUNCTION OF WS-POORDER
           MOVE WS-ORD TO OR-ORDER-NUMBER
           CALL "POORDER" USING WS-POORDER
           PERFORM 8350-FROM-ORD THRU 8350-EXIT
           MOVE WS-C-SCEN TO D-SCENARIO
           MOVE "ISSUE" TO D-STEP
           MOVE WS-ORD TO D-REF
           MOVE OR-COMMITMENT-GBP TO D-AMOUNT
           MOVE OR-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT.
       7500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-010 - the only way goods enter stock.  RECEIPT moves the
      *> stock, relieves the commitment and raises the GRNI accrual
      *> in one unit of work.
      *>----------------------------------------------------------------
      *>----------------------------------------------------------------
      *> A LOT CONTROLLED ITEM NEEDS A LOT THAT THE STOCK LEDGER
      *> ALREADY KNOWS ABOUT, BECAUSE AN ISSUE (THE SUPPLIER RETURN)
      *> WILL NOT ACCEPT ONE IT CANNOT FIND.  AN EXISTING LOT IS
      *> REUSED WHERE THE ITEM HAS ONE; OTHERWISE THE DEMONSTRATION
      *> NAMES ITS OWN AND A NON TRACEABLE ITEM IGNORES IT.
      *>----------------------------------------------------------------
       7550-PICK-LOT SECTION.
       7550-START.
           MOVE SPACES TO WS-C-LOT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 l.LotCode FROM inv.Lot l WHERE "
                  "l.CompanyCode = ? AND l.ItemCode = ? "
                  "ORDER BY l.LotCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-C-ITEM TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8900-SET-WIDTHS THRU 8900-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PD-ROW
           IF SQLIO-OK AND NOT SQLIO-NOTFOUND
               MOVE PD-COL(1) TO WS-C-LOT
           END-IF
           IF WS-C-LOT = SPACES
               STRING "PODEMO-" FUNCTION TRIM(WS-CO) "-L1"
                   DELIMITED BY SIZE INTO WS-C-LOT
           END-IF.
       7550-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       7600-RECEIVE SECTION.
       7600-START.
           PERFORM 7550-PICK-LOT THRU 7550-EXIT
           INITIALIZE WS-RECEIPT
           MOVE "RECEIPT" TO CA-PROGRAM OF WS-RECEIPT
           MOVE "CONFIRM" TO CA-FUNCTION OF WS-RECEIPT
           MOVE WS-CO TO CA-COMPANY OF WS-RECEIPT
           MOVE WS-SITE TO CA-SITE OF WS-RECEIPT
           MOVE WS-USER TO CA-USER OF WS-RECEIPT
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-RECEIPT
           PERFORM 8099-NEXT-CORR THRU 8099-EXIT
           MOVE WS-STEP-CORR TO CA-CORRELATION-ID OF WS-RECEIPT
           MOVE WS-ORD TO RC-ORDER-NUMBER
           MOVE WS-C-SUPP TO RC-SUPPLIER
           MOVE WS-C-REF TO RC-DELIVERY-NOTE
           MOVE WS-TODAY-N TO RC-PHYSICAL-DATE
           MOVE WS-TODAY-N TO RC-POSTING-DATE
           MOVE WS-WHS TO RC-WAREHOUSE
           MOVE WS-USER TO RC-RECEIVED-BY
           MOVE "BLOCK" TO RC-DUP-POLICY
           MOVE 1 TO RC-LINE-CNT
           MOVE 1 TO RC-L-NUM(1)
           MOVE 1 TO RC-L-PO-LINE(1)
           MOVE 1 TO RC-L-SCHEDULE(1)
           MOVE WS-C-ITEM TO RC-L-ITEM(1)
           MOVE WS-C-DESC TO RC-L-DESC(1)
           MOVE WS-RCPT-QTY TO RC-L-COUNTED(1)
           MOVE WS-RCPT-QTY TO RC-L-RECEIVED(1)
           MOVE WS-C-UOM TO RC-L-UOM(1)
           MOVE WS-LOCN TO RC-L-LOCATION(1)
           MOVE SPACES TO RC-L-LOT(1)
           MOVE WS-C-LOT TO RC-L-LOT(1)
           CALL "RECEIPT" USING WS-RECEIPT
           MOVE CA-RETURN-CODE OF WS-RECEIPT TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-RECEIPT TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-RECEIPT TO WS-CALL-MSG
           IF WS-CALL-RC >= 8 AND RC-L-REASON(1) NOT = SPACES
               MOVE RC-L-REASON(1) TO WS-CALL-REASON
           END-IF
           MOVE WS-C-SCEN TO D-SCENARIO
           MOVE "RECEIPT CONFIRM" TO D-STEP
           MOVE RC-RECEIPT-REF TO D-REF
           MOVE RC-TOTAL-GBP TO D-AMOUNT
           MOVE RC-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT
           IF WS-CALL-RC < 8
               MOVE RC-RECEIPT-REF TO WS-RCPT
               ADD 1 TO T-RECEIPTS
      *>       REMEMBER WHERE THE GOODS ACTUALLY LANDED SO THAT A
      *>       LATER SUPPLIER RETURN ASKS FOR THE CONDITION THEY ARE
      *>       IN.  AN INSPECTED LINE SITS IN QUARANTINE UNTIL THE
      *>       INSPECTION RELEASES IT.
               MOVE WS-LOCN TO WS-RET-LOCN
               MOVE "ACCEPTED" TO WS-RET-COND
               IF RC-LINES-QUARANT > 0
                   MOVE WS-QUAR-LOCN TO WS-RET-LOCN
                   MOVE "QUARANTINE" TO WS-RET-COND
                   PERFORM 7700-INSPECT THRU 7700-EXIT
               END-IF
           END-IF.
       7600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-012 - quarantined goods are inspected and the rejected
      *> part is scrapped.
      *>----------------------------------------------------------------
       7700-INSPECT SECTION.
       7700-START.
           INITIALIZE WS-INSPECT
           MOVE "INSPECT" TO CA-PROGRAM OF WS-INSPECT
           MOVE "RECORD" TO CA-FUNCTION OF WS-INSPECT
           MOVE WS-CO TO CA-COMPANY OF WS-INSPECT
           MOVE WS-SITE TO CA-SITE OF WS-INSPECT
           MOVE WS-USER TO CA-USER OF WS-INSPECT
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-INSPECT
           PERFORM 8099-NEXT-CORR THRU 8099-EXIT
           MOVE WS-STEP-CORR TO CA-CORRELATION-ID OF WS-INSPECT
           MOVE WS-RCPT TO IN-RECEIPT-REF
           MOVE 1 TO IN-RECEIPT-LINE
           MOVE WS-ORD TO IN-ORDER-NUMBER
           MOVE WS-C-SUPP TO IN-SUPPLIER
           MOVE WS-C-ITEM TO IN-ITEM-CODE
           MOVE WS-WHS TO IN-WAREHOUSE
           MOVE WS-LOCN TO IN-GOOD-LOCATION
           MOVE WS-TODAY-N TO IN-INSPECT-DATE
           MOVE "QCINSP1" TO IN-INSPECTOR
           MOVE RC-L-QUARANT(1) TO IN-INSPECTED-QTY
           COMPUTE IN-REJECTED-QTY = RC-L-QUARANT(1) * 0.1
           COMPUTE IN-ACCEPTED-QTY =
               RC-L-QUARANT(1) - IN-REJECTED-QTY
           MOVE "SCRAP" TO IN-DISPOSITION
           MOVE "OUTOFSPEC" TO IN-REJECT-REASON
           MOVE "Dimensional check failed on the sample"
               TO IN-REJECT-NARR
           CALL "INSPECT" USING WS-INSPECT
           MOVE CA-RETURN-CODE OF WS-INSPECT TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-INSPECT TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-INSPECT TO WS-CALL-MSG
           IF WS-CALL-RC < 8
      *>       THE INSPECTION RELEASED THE GOOD PART INTO THE GOODS-IN
      *>       LOCATION, SO A RETURN NOW DRAWS ON ACCEPTED STOCK.
               MOVE WS-LOCN TO WS-RET-LOCN
               MOVE "ACCEPTED" TO WS-RET-COND
           END-IF
           MOVE "Inspection" TO D-SCENARIO
           MOVE "RECORD SCRAP" TO D-STEP
           MOVE WS-RCPT TO D-REF
           MOVE IN-SCRAP-VALUE-GBP TO D-AMOUNT
           MOVE IN-QUALITY-STATUS TO D-STATUS
           PERFORM 8000-RECORD THRU 8000-EXIT.
       7700-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> Common commarea headers and result extraction.
      *>----------------------------------------------------------------
       8000-RECORD SECTION.
       8000-START.
           ADD 1 TO T-STEPS
           ADD 1 TO WS-SEQ
           MOVE WS-SEQ TO D-SEQ
           MOVE "N" TO WS-FAILED-STEP
           EVALUATE TRUE
               WHEN WS-CALL-RC = 0
                   MOVE "accepted" TO D-OUTCOME
                   ADD 1 TO T-OK
               WHEN WS-CALL-RC < 8
                   MOVE SPACES TO D-OUTCOME
                   STRING "warning " WS-CALL-REASON
                       DELIMITED BY SIZE INTO D-OUTCOME
                   ADD 1 TO T-WARN
               WHEN OTHER
                   MOVE SPACES TO D-OUTCOME
                   STRING "REFUSED " WS-CALL-REASON
                       DELIMITED BY SIZE INTO D-OUTCOME
                   ADD 1 TO T-FAIL
                   MOVE "Y" TO WS-FAILED-STEP
                   DISPLAY "STERP-W-1810 " FUNCTION TRIM(D-STEP)
                           " " WS-CALL-REASON " "
                           FUNCTION TRIM(WS-CALL-MSG) UPON SYSOUT
           END-EVALUATE
           GENERATE DE-LINE.
       8000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8050-FROM-REQ SECTION.
       8050-START.
           MOVE CA-RETURN-CODE OF WS-POREQ TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-POREQ TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-POREQ TO WS-CALL-MSG.
       8050-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *>----------------------------------------------------------------
      *> NUMGEN REPLAYS A NUMBER WHEN THE SAME CORRELATION ID COMES
      *> BACK (COM-015), SO EVERY SERVICE CALL IN THE DEMONSTRATION
      *> NEEDS ITS OWN ID.  THE STEM STAYS PODEMO-<CO> SO THAT THE
      *> RERUN CHECK AND THE CLEARDOWN CAN STILL FIND THE WHOLE RUN,
      *> AND THE SEQUENCE IS DRAWN IN A FIXED ORDER SO A REPEATED RUN
      *> ASKS FOR EXACTLY THE SAME NUMBERS.
      *>----------------------------------------------------------------
       8099-NEXT-CORR SECTION.
       8099-START.
           ADD 1 TO WS-CORR-SEQ
           MOVE SPACES TO WS-STEP-CORR
           STRING FUNCTION TRIM(WS-CORR) "-" WS-CORR-SEQ
               DELIMITED BY SIZE INTO WS-STEP-CORR.
       8099-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8100-REQ-HEADER SECTION.
       8100-START.
           MOVE "POREQ" TO CA-PROGRAM OF WS-POREQ
           MOVE WS-CO TO CA-COMPANY OF WS-POREQ
           MOVE WS-SITE TO CA-SITE OF WS-POREQ
           MOVE WS-USER TO CA-USER OF WS-POREQ
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-POREQ
           PERFORM 8099-NEXT-CORR THRU 8099-EXIT
           MOVE WS-STEP-CORR TO CA-CORRELATION-ID OF WS-POREQ.
       8100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8200-SRC-HEADER SECTION.
       8200-START.
           MOVE "SOURCSEL" TO CA-PROGRAM OF WS-SOURCSEL
           MOVE WS-CO TO CA-COMPANY OF WS-SOURCSEL
           MOVE WS-SITE TO CA-SITE OF WS-SOURCSEL
           MOVE WS-USER TO CA-USER OF WS-SOURCSEL
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-SOURCSEL
           PERFORM 8099-NEXT-CORR THRU 8099-EXIT
           MOVE WS-STEP-CORR TO CA-CORRELATION-ID OF WS-SOURCSEL.
       8200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8250-FROM-SRC SECTION.
       8250-START.
           MOVE CA-RETURN-CODE OF WS-SOURCSEL TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-SOURCSEL TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-SOURCSEL TO WS-CALL-MSG.
       8250-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8300-ORD-HEADER SECTION.
       8300-START.
           MOVE "POORDER" TO CA-PROGRAM OF WS-POORDER
           MOVE WS-CO TO CA-COMPANY OF WS-POORDER
           MOVE WS-SITE TO CA-SITE OF WS-POORDER
           MOVE WS-USER TO CA-USER OF WS-POORDER
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-POORDER
           PERFORM 8099-NEXT-CORR THRU 8099-EXIT
           MOVE WS-STEP-CORR TO CA-CORRELATION-ID OF WS-POORDER.
       8300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8350-FROM-ORD SECTION.
       8350-START.
           MOVE CA-RETURN-CODE OF WS-POORDER TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-POORDER TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-POORDER TO WS-CALL-MSG.
       8350-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8400-CLS-HEADER SECTION.
       8400-START.
           MOVE "POCLOSE" TO CA-PROGRAM OF WS-POCLOSE
           MOVE WS-CO TO CA-COMPANY OF WS-POCLOSE
           MOVE WS-SITE TO CA-SITE OF WS-POCLOSE
           MOVE WS-APPROVER TO CA-USER OF WS-POCLOSE
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-POCLOSE
           PERFORM 8099-NEXT-CORR THRU 8099-EXIT
           MOVE WS-STEP-CORR TO CA-CORRELATION-ID OF WS-POCLOSE.
       8400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8450-FROM-CLS SECTION.
       8450-START.
           MOVE CA-RETURN-CODE OF WS-POCLOSE TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-POCLOSE TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-POCLOSE TO WS-CALL-MSG.
       8450-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8500-RET-HEADER SECTION.
       8500-START.
           MOVE "SUPPRET" TO CA-PROGRAM OF WS-SUPPRET
           MOVE WS-CO TO CA-COMPANY OF WS-SUPPRET
           MOVE WS-SITE TO CA-SITE OF WS-SUPPRET
           MOVE WS-USER TO CA-USER OF WS-SUPPRET
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-SUPPRET
           PERFORM 8099-NEXT-CORR THRU 8099-EXIT
           MOVE WS-STEP-CORR TO CA-CORRELATION-ID OF WS-SUPPRET.
       8500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8550-FROM-RET SECTION.
       8550-START.
           MOVE CA-RETURN-CODE OF WS-SUPPRET TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-SUPPRET TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-SUPPRET TO WS-CALL-MSG.
       8550-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8650-FROM-APPR SECTION.
       8650-START.
           MOVE CA-RETURN-CODE OF WS-POAPPRV TO WS-CALL-RC
           MOVE CA-REASON-CODE OF WS-POAPPRV TO WS-CALL-REASON
           MOVE CA-MESSAGE OF WS-POAPPRV TO WS-CALL-MSG.
       8650-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8900-SET-WIDTHS SECTION.
       8900-START.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 30
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       8900-EXIT.
           EXIT.
