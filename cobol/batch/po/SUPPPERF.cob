      *>================================================================
      *> PROGRAM-ID : SUPPPERF
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : SUPPLIER PERFORMANCE MEASUREMENT (PO-023).
      *>              FIVE DISTINCT MEASURES ARE KEPT APART AND NEVER
      *>              BLENDED INTO ONE SCORE:
      *>                ON TIME      did it arrive in the window
      *>                IN FULL      was the scheduled quantity met
      *>                OTIF         both of the above on one receipt
      *>                QUALITY      accepted against presented
      *>                PRICE        receipt price against order price
      *>                LEAD TIME    order date to physical arrival
      *>              D-19 FIXES THE COHORT: supplier on time is
      *>              measured against the ORIGINAL confirmed date and
      *>              a delivery up to five WORKING days early still
      *>              counts as on time.  The same figures are also
      *>              produced on the CURRENT confirmed date so that
      *>              the effect of re-dating is visible; the two
      *>              bases are written as separate rows and are never
      *>              averaged together.
      *>              A measure with no denominator is undefined.  It
      *>              prints as n/a and is stored as zero with a zero
      *>              receipt count, never as 0% or 100%.
      *> RUN AS     : SUPPPERF CO=STC [SUPP=S0001] FROM=yyyymmdd
      *>              TO=yyyymmdd [MODE=PREVIEW|COMMIT] USER=BUYER1
      *> OUTPUT     : run\spool\SUPPPERF-<CO>-<stamp>.lst  (132 col)
      *>              po.SupplierPerformance rows when MODE=COMMIT
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, DATECALC, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-10-07 RJH  CR0702 VENDOR RATING
      *> 2003-04-22 KMP  CR1015 QUALITY AND PRICE SPLIT OUT OF THE
      *>                 SINGLE VENDOR SCORE, WHICH NOBODY TRUSTED
      *> 2011-06-14 DJM  CR1655 ORIGINAL AND CURRENT BASES STORED
      *>                 SEPARATELY (AUDIT POINT 14)
      *> 2026-10-06 STERP PO-023, D-19
      *>================================================================
      *> REQ: PO-023
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUPPPERF.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 07/10/1998.
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
       FD  RPT-FILE REPORT IS R-SUPPPERF.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY STDATES.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATECALC==.
       01  PO-ROW.
           05  PO-COL OCCURS 30 TIMES PIC X(100).
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-SUPP                 PIC X(20) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-FROM                 PIC X(8) VALUE SPACES.
           05  WS-TO                   PIC X(8) VALUE SPACES.
           05  WS-MODE                 PIC X(8) VALUE "COMMIT".
               88  WS-PREVIEW                    VALUE "PREVIEW".
               88  WS-COMMIT                     VALUE "COMMIT".
       01  WS-SCRATCH.
           05  WS-IX                   PIC S9(4) VALUE 0.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-TODAY-N              PIC 9(8) VALUE 0.
           05  WS-DONE                 PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-ROWS-READ            PIC S9(9) VALUE 0.
           05  WS-ROWS-WRITTEN         PIC S9(9) VALUE 0.
           05  WS-TOL-DAYS             PIC S9(4) VALUE 5.
           05  WS-KEY                  PIC X(20) VALUE SPACES.
           05  WS-PREV-KEY             PIC X(20) VALUE SPACES.
           05  WS-OPEN                 PIC X VALUE "N".
           05  WS-NUM-ED               PIC -(14)9.9(6).
           05  WS-INT-ED               PIC -(8)9.
           05  WS-PHYS                 PIC 9(8) VALUE 0.
           05  WS-PROMISE               PIC 9(8) VALUE 0.
           05  WS-EARLY-LIMIT          PIC 9(8) VALUE 0.
           05  WS-ONTIME-FLAG          PIC X VALUE "N".
           05  WS-INFULL-FLAG          PIC X VALUE "N".
           05  WS-BASIS-IX             PIC S9(4) VALUE 0.
      *>   Running accumulators for the supplier/period being built.
       01  WS-ACC.
           05  WS-ACC-SUPP             PIC X(20) VALUE SPACES.
           05  WS-ACC-PERIOD           PIC X(7) VALUE SPACES.
           05  A-RECEIPTS              PIC S9(7) VALUE 0.
           05  A-INFULL                PIC S9(7) VALUE 0.
           05  A-ACCEPTED              PIC S9(13)V9(6) VALUE 0.
           05  A-REJECTED              PIC S9(13)V9(6) VALUE 0.
           05  A-PRESENTED             PIC S9(13)V9(6) VALUE 0.
           05  A-PPV                   PIC S9(13)V99 VALUE 0.
           05  A-VALUE                 PIC S9(13)V99 VALUE 0.
           05  A-LEAD-SUM              PIC S9(11)V99 VALUE 0.
           05  A-QLEAD-SUM             PIC S9(11)V99 VALUE 0.
           05  A-LEAD-CNT              PIC S9(7) VALUE 0.
           05  A-DATED                 PIC S9(7) OCCURS 2 VALUE 0.
           05  A-ONTIME                PIC S9(7) OCCURS 2 VALUE 0.
           05  A-OTIF                  PIC S9(7) OCCURS 2 VALUE 0.
       01  WS-CALC.
           05  C-ONTIME-PCT            PIC S9(5)V9(4) VALUE 0.
           05  C-INFULL-PCT            PIC S9(5)V9(4) VALUE 0.
           05  C-OTIF-PCT              PIC S9(5)V9(4) VALUE 0.
           05  C-QUAL-PCT              PIC S9(5)V9(4) VALUE 0.
           05  C-AVG-LEAD              PIC S9(7)V99 VALUE 0.
           05  C-QUOTED-LEAD           PIC S9(7)V99 VALUE 0.
           05  C-PCT-ED                PIC Z(3)9.99 VALUE ZERO.
       01  WS-DET.
           05  D-SUPP                  PIC X(12) VALUE SPACES.
           05  D-PERIOD                PIC X(7) VALUE SPACES.
           05  D-BASIS                 PIC X(8) VALUE SPACES.
           05  D-RECEIPTS              PIC S9(7) VALUE 0.
           05  D-ONTIME-TX             PIC X(8) VALUE SPACES.
           05  D-INFULL-TX             PIC X(8) VALUE SPACES.
           05  D-OTIF-TX               PIC X(8) VALUE SPACES.
           05  D-QUAL-TX               PIC X(8) VALUE SPACES.
           05  D-PPV                   PIC S9(11)V99 VALUE 0.
           05  D-VALUE                 PIC S9(11)V99 VALUE 0.
           05  D-AVG-LEAD              PIC S9(7)V99 VALUE 0.
           05  D-QUOTED-LEAD           PIC S9(7)V99 VALUE 0.
           05  D-ONE                   PIC 9 VALUE 1.
       01  WS-TOT.
           05  T-ROWS                  PIC S9(7) VALUE 0.
           05  T-RECEIPTS              PIC S9(9) VALUE 0.
           05  T-UNDEFINED             PIC S9(7) VALUE 0.
           05  T-DIVERGENT             PIC S9(7) VALUE 0.
           05  T-PPV                   PIC S9(13)V99 VALUE 0.
           05  T-VALUE                 PIC S9(13)V99 VALUE 0.
       01  WS-CMP.
           05  CMP-ORIG-PCT            PIC S9(5)V9(4) VALUE 0.
       REPORT SECTION.
       RD  R-SUPPPERF
           CONTROLS ARE FINAL D-SUPP
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 54
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-SUPP TYPE IS CONTROL HEADING D-SUPP.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(10) VALUE "Supplier: ".
               10  COLUMN 11   PIC X(12) SOURCE D-SUPP.
               10  COLUMN 28   PIC X(50) VALUE
                   "ORIGINAL is the D-19 measure.  CURRENT is shown".
               10  COLUMN 79   PIC X(50) VALUE
                   "only to expose re-dating; it is not the KPI.".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(6) VALUE "Period".
               10  COLUMN 10   PIC X(5) VALUE "Basis".
               10  COLUMN 20   PIC X(8) VALUE "Receipts".
               10  COLUMN 31   PIC X(8) VALUE " On time".
               10  COLUMN 41   PIC X(8) VALUE " In full".
               10  COLUMN 51   PIC X(8) VALUE "    OTIF".
               10  COLUMN 61   PIC X(8) VALUE " Quality".
               10  COLUMN 72   PIC X(13) VALUE "Price varianc".
               10  COLUMN 88   PIC X(13) VALUE "Purchase vale".
               10  COLUMN 104  PIC X(9) VALUE "Lead act ".
               10  COLUMN 116  PIC X(9) VALUE "Lead quot".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(7) SOURCE D-PERIOD.
               10  COLUMN 10   PIC X(8) SOURCE D-BASIS.
               10  COLUMN 20   PIC ZZZZZZ9 SOURCE D-RECEIPTS.
               10  COLUMN 31   PIC X(8) SOURCE D-ONTIME-TX.
               10  COLUMN 41   PIC X(8) SOURCE D-INFULL-TX.
               10  COLUMN 51   PIC X(8) SOURCE D-OTIF-TX.
               10  COLUMN 61   PIC X(8) SOURCE D-QUAL-TX.
               10  COLUMN 72   PIC Z(9)9.99- SOURCE D-PPV.
               10  COLUMN 88   PIC Z(9)9.99- SOURCE D-VALUE.
               10  COLUMN 104  PIC Z(5)9.99 SOURCE D-AVG-LEAD.
               10  COLUMN 116  PIC Z(5)9.99 SOURCE D-QUOTED-LEAD.
       01  CF-SUPP TYPE IS CONTROL FOOTING D-SUPP.
           05  LINE PLUS 1.
               10  COLUMN 10   PIC X(24) VALUE
                   "Supplier measure rows : ".
               10  COLUMN 34   PIC ZZZZ9 SUM D-ONE.
               10  COLUMN 45   PIC X(22) VALUE
                   "Price variance total: ".
               10  COLUMN 67   PIC Z(11)9.99- SUM D-PPV.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(30) VALUE
                   "Measure rows produced       : ".
               10  COLUMN 35   PIC ZZZZ9 SOURCE T-ROWS.
               10  COLUMN 48   PIC X(30) VALUE
                   "Receipts measured           : ".
               10  COLUMN 78   PIC ZZZZZZZ9 SOURCE T-RECEIPTS.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(30) VALUE
                   "Undefined measures (n/a)    : ".
               10  COLUMN 35   PIC ZZZZ9 SOURCE T-UNDEFINED.
               10  COLUMN 48   PIC X(48) VALUE
                   "no denominator - reported as n/a, never as zero".
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(30) VALUE
                   "Bases that disagree         : ".
               10  COLUMN 35   PIC ZZZZ9 SOURCE T-DIVERGENT.
               10  COLUMN 48   PIC X(48) VALUE
                   "current promise flatters the original: re-dated".
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(30) VALUE
                   "Price variance              : ".
               10  COLUMN 35   PIC Z(11)9.99- SOURCE T-PPV.
               10  COLUMN 53   PIC X(30) VALUE
                   "Purchase value              : ".
               10  COLUMN 83   PIC Z(11)9.99- SOURCE T-VALUE.
           05  LINE PLUS 2.
               10  COLUMN 5    PIC X(62) VALUE
                   "A cancelled schedule is not a supplier failure and".
               10  COLUMN 68   PIC X(60) VALUE
                   "is excluded; a short delivery is, and is counted.".
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
           PERFORM 0600-READ-TOLERANCE THRU 0600-EXIT
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
           MOVE CM-USER TO WS-USER
           MOVE "SUPP" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-SUPP
           END-IF
           MOVE "FROM" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-FROM
           END-IF
           MOVE "TO" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-TO
           END-IF
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
           IF NOT WS-PREVIEW AND NOT WS-COMMIT
               MOVE "COMMIT" TO WS-MODE
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           IF WS-FROM = SPACES
               MOVE "19000101" TO WS-FROM
           END-IF
           IF WS-TO = SPACES
               MOVE WS-TODAY-N TO WS-TO
           END-IF
           DISPLAY "STERP-I-1730 SUPPPERF supplier performance "
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
           MOVE "SUPPPERF" TO JL-JOB-NAME
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
           MOVE "SUPPPERF" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1731 cannot build the spool path"
                   UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           PERFORM 0350-FILL-HEADINGS THRU 0350-EXIT
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1732 cannot open " WS-SPOOL-NAME
                   UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           INITIATE R-SUPPPERF.
       0300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "RPT-023" TO RPT-REPORT-ID
           MOVE "Supplier performance - OTIF, quality, price (PO-023)"
               TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE SPACES TO RPT-SUBTITLE
           IF WS-SUPP = SPACES
               STRING "All suppliers, mode " FUNCTION TRIM(WS-MODE)
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           ELSE
               STRING "Supplier " FUNCTION TRIM(WS-SUPP)
                      ", mode " FUNCTION TRIM(WS-MODE)
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           END-IF
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "Receipts " WS-FROM(7:2) "/" WS-FROM(5:2) "/"
                  WS-FROM(1:4) " to " WS-TO(7:2) "/" WS-TO(5:2) "/"
                  WS-TO(1:4)
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
           CALL "SQLIO" USING SQLIO-CA PO-ROW
           IF SQLIO-OK
               MOVE PO-COL(1) TO RPT-COMPANY-NAME
               MOVE PO-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> D-19 allows a delivery up to five WORKING days early to count
      *> as on time.  The window is a policy value, not a constant, so
      *> it is read from the company tolerance rule.
      *>----------------------------------------------------------------
       0600-READ-TOLERANCE SECTION.
       0600-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(EarlyDeliveryDays AS varchar(10)) "
                  "FROM po.ToleranceRule WHERE CompanyCode = ? AND "
                  "ScopeType = 'COMPANY' AND IsActive = 1"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 30
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW
           IF SQLIO-OK AND PO-COL(1) NOT = SPACES
               COMPUTE WS-TOL-DAYS = FUNCTION NUMVAL(PO-COL(1))
           END-IF
           IF WS-TOL-DAYS < 0
               MOVE 0 TO WS-TOL-DAYS
           END-IF.
       0600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> REQ: PO-023 - one row per confirmed receipt line.  Reversed
      *> receipts and cancelled schedules are excluded; a reversal
      *> never leaves a phantom success behind.  The accepted and
      *> rejected quantities come from the receipt line after
      *> inspection, so a quarantined delivery that is later scrapped
      *> damages the quality measure and not the on-time measure.
      *>----------------------------------------------------------------
       1000-PRODUCE SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT r.SupplierCode, CONVERT(char(7),"
                  "r.PostingDate,126), CONVERT(char(8),"
                  "r.PhysicalDate,112), ISNULL(CONVERT(char(8),"
                  "s.OriginalConfirmedDate,112),''), "
                  "ISNULL(CONVERT(char(8),"
                  "s.CurrentConfirmedDate,112),''), "
                  "CAST(rl.ReceivedQty AS varchar(30)), "
                  "CAST(ISNULL(s.Quantity,rl.ReceivedQty) AS "
                  "varchar(30)), CAST(rl.AcceptedQty AS "
                  "varchar(30)), CAST(rl.RejectedQty + "
                  "rl.ReturnedQty AS varchar(30)), "
                  "CAST(rl.PriceVarianceGBP AS varchar(30)), "
                  "CAST(rl.ProvisionalValueGBP AS varchar(30)), "
                  "CAST(DATEDIFF(day,o.OrderDate,r.PhysicalDate) AS "
                  "varchar(10)), CAST(ISNULL((SELECT TOP 1 "
                  "ss.LeadTimeDays FROM po.SupplierSource ss WHERE "
                  "ss.CompanyCode = r.CompanyCode AND "
                  "ss.SupplierCode = r.SupplierCode AND "
                  "ss.ItemCode = rl.ItemCode AND ss.IsActive = 1 "
                  "ORDER BY ss.Priority),0) AS varchar(10)) "
                  "FROM po.ReceiptLine rl JOIN po.Receipt r ON "
                  "r.ReceiptId = rl.ReceiptId LEFT JOIN "
                  "po.PurchaseOrderLine l ON l.PurchaseOrderLineId "
                  "= rl.PurchaseOrderLineId LEFT JOIN "
                  "po.PurchaseOrder o ON o.PurchaseOrderId = "
                  "l.PurchaseOrderId LEFT JOIN po.PurchaseSchedule "
                  "s ON s.CompanyCode = r.CompanyCode AND "
                  "s.OrderNumber = rl.PurchaseOrderNumber AND "
                  "s.LineNum = rl.PurchaseOrderLine AND "
                  "s.ScheduleNo = rl.ScheduleNo WHERE "
                  "r.CompanyCode = ? AND "
                  "r.Status = 'CONFIRMED' AND CONVERT(char(8),"
                  "r.PostingDate,112) BETWEEN ? AND ? AND "
                  "(? = '' OR r.SupplierCode = ?) ORDER BY "
                  "r.SupplierCode, 2, r.ReceiptRef, rl.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 40
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-FROM TO SQLIO-PARM-VAL(2)
           MOVE WS-TO TO SQLIO-PARM-VAL(3)
           MOVE WS-SUPP TO SQLIO-PARM-VAL(4)
           MOVE WS-SUPP TO SQLIO-PARM-VAL(5)
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE 13 TO SQLIO-COL-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 30
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1733 " FUNCTION TRIM(SQLIO-ERRMSG)
                   UPON SYSOUT
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           MOVE SPACES TO WS-PREV-KEY
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA PO-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   ADD 1 TO WS-ROWS-READ
                   PERFORM 1100-ONE-ROW THRU 1100-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW
           IF WS-OPEN = "Y"
               PERFORM 1300-FLUSH THRU 1300-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1100-ONE-ROW SECTION.
       1100-START.
           MOVE SPACES TO WS-KEY
           STRING PO-COL(1)(1:12) PO-COL(2)(1:7)
               DELIMITED BY SIZE INTO WS-KEY
           IF WS-OPEN = "Y" AND WS-KEY NOT = WS-PREV-KEY
               PERFORM 1300-FLUSH THRU 1300-EXIT
           END-IF
           IF WS-OPEN NOT = "Y"
               PERFORM 1200-START-GROUP THRU 1200-EXIT
           END-IF
           PERFORM 1150-ACCUMULATE THRU 1150-EXIT
           MOVE WS-KEY TO WS-PREV-KEY.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1200-START-GROUP SECTION.
       1200-START.
           INITIALIZE WS-ACC
           MOVE PO-COL(1) TO WS-ACC-SUPP
           MOVE PO-COL(2)(1:7) TO WS-ACC-PERIOD
           MOVE "Y" TO WS-OPEN.
       1200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1150-ACCUMULATE SECTION.
       1150-START.
           ADD 1 TO A-RECEIPTS
           COMPUTE A-PRESENTED = A-PRESENTED
                 + FUNCTION NUMVAL(PO-COL(6))
           COMPUTE A-ACCEPTED = A-ACCEPTED
                 + FUNCTION NUMVAL(PO-COL(8))
           COMPUTE A-REJECTED = A-REJECTED
                 + FUNCTION NUMVAL(PO-COL(9))
           COMPUTE A-PPV = A-PPV + FUNCTION NUMVAL(PO-COL(10))
           COMPUTE A-VALUE = A-VALUE + FUNCTION NUMVAL(PO-COL(11))
           IF PO-COL(12) NOT = SPACES
               ADD 1 TO A-LEAD-CNT
               COMPUTE A-LEAD-SUM = A-LEAD-SUM
                     + FUNCTION NUMVAL(PO-COL(12))
               COMPUTE A-QLEAD-SUM = A-QLEAD-SUM
                     + FUNCTION NUMVAL(PO-COL(13))
           END-IF
      *>     In full: the schedule quantity was met by this receipt.
           MOVE "N" TO WS-INFULL-FLAG
           IF FUNCTION NUMVAL(PO-COL(6)) >=
              FUNCTION NUMVAL(PO-COL(7))
               MOVE "Y" TO WS-INFULL-FLAG
               ADD 1 TO A-INFULL
           END-IF
           MOVE PO-COL(3)(1:8) TO WS-PHYS
           PERFORM VARYING WS-BASIS-IX FROM 1 BY 1
                   UNTIL WS-BASIS-IX > 2
               MOVE 0 TO WS-PROMISE
               IF WS-BASIS-IX = 1
                   IF PO-COL(4)(1:8) NOT = SPACES
                       MOVE PO-COL(4)(1:8) TO WS-PROMISE
                   END-IF
               ELSE
                   IF PO-COL(5)(1:8) NOT = SPACES
                       MOVE PO-COL(5)(1:8) TO WS-PROMISE
                   END-IF
               END-IF
               IF WS-PROMISE NOT = 0
                   ADD 1 TO A-DATED(WS-BASIS-IX)
                   PERFORM 1160-ON-TIME-TEST THRU 1160-EXIT
                   IF WS-ONTIME-FLAG = "Y"
                       ADD 1 TO A-ONTIME(WS-BASIS-IX)
                       IF WS-INFULL-FLAG = "Y"
                           ADD 1 TO A-OTIF(WS-BASIS-IX)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.
       1150-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> D-19 on-time window: on or before the promise, but not more
      *> than WS-TOL-DAYS WORKING days before it.  Arriving a month
      *> early is a planning failure, not a success, because the goods
      *> have to be stored and paid for sooner than the plan assumed.
      *>----------------------------------------------------------------
       1160-ON-TIME-TEST SECTION.
       1160-START.
           MOVE "N" TO WS-ONTIME-FLAG
           IF WS-PHYS > WS-PROMISE
               GO TO 1160-EXIT
           END-IF
           INITIALIZE WS-DATECALC
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DATECALC
           MOVE WS-CO TO CA-COMPANY OF WS-DATECALC
           MOVE WS-PROMISE TO DC-DATE-1
           COMPUTE DC-NUMBER = 0 - WS-TOL-DAYS
           CALL "DATECALC" USING WS-DATECALC
           IF CA-RETURN-CODE OF WS-DATECALC > 4
               MOVE WS-PROMISE TO WS-EARLY-LIMIT
           ELSE
               MOVE DC-RESULT-DATE TO WS-EARLY-LIMIT
           END-IF
           IF WS-PHYS >= WS-EARLY-LIMIT
               MOVE "Y" TO WS-ONTIME-FLAG
           END-IF.
       1160-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> One supplier/period is complete.  Two measure rows are
      *> produced, ORIGINAL first because that is the published KPI.
      *>----------------------------------------------------------------
       1300-FLUSH SECTION.
       1300-START.
           MOVE WS-ACC-SUPP TO D-SUPP
           MOVE WS-ACC-PERIOD TO D-PERIOD
           PERFORM VARYING WS-BASIS-IX FROM 1 BY 1
                   UNTIL WS-BASIS-IX > 2
               PERFORM 1320-ONE-BASIS THRU 1320-EXIT
           END-PERFORM
           ADD A-RECEIPTS TO T-RECEIPTS
           MOVE "N" TO WS-OPEN
           INITIALIZE WS-ACC.
       1300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1320-ONE-BASIS SECTION.
       1320-START.
           IF WS-BASIS-IX = 1
               MOVE "ORIGINAL" TO D-BASIS
           ELSE
               MOVE "CURRENT" TO D-BASIS
           END-IF
           MOVE A-RECEIPTS TO D-RECEIPTS
      *>     Zero denominator is undefined (document 11 section 9).
           IF A-DATED(WS-BASIS-IX) = 0
               MOVE 0 TO C-ONTIME-PCT C-OTIF-PCT
               MOVE "     n/a" TO D-ONTIME-TX D-OTIF-TX
               ADD 2 TO T-UNDEFINED
           ELSE
               COMPUTE C-ONTIME-PCT ROUNDED = 100
                     * A-ONTIME(WS-BASIS-IX) / A-DATED(WS-BASIS-IX)
               COMPUTE C-OTIF-PCT ROUNDED = 100
                     * A-OTIF(WS-BASIS-IX) / A-DATED(WS-BASIS-IX)
               MOVE C-ONTIME-PCT TO C-PCT-ED
               MOVE C-PCT-ED TO D-ONTIME-TX
               MOVE C-OTIF-PCT TO C-PCT-ED
               MOVE C-PCT-ED TO D-OTIF-TX
           END-IF
           IF A-RECEIPTS = 0
               MOVE 0 TO C-INFULL-PCT
               MOVE "     n/a" TO D-INFULL-TX
               IF WS-BASIS-IX = 1
                   ADD 1 TO T-UNDEFINED
               END-IF
           ELSE
               COMPUTE C-INFULL-PCT ROUNDED =
                   100 * A-INFULL / A-RECEIPTS
               MOVE C-INFULL-PCT TO C-PCT-ED
               MOVE C-PCT-ED TO D-INFULL-TX
           END-IF
           IF A-PRESENTED = 0
               MOVE 0 TO C-QUAL-PCT
               MOVE "     n/a" TO D-QUAL-TX
               IF WS-BASIS-IX = 1
                   ADD 1 TO T-UNDEFINED
               END-IF
           ELSE
               COMPUTE C-QUAL-PCT ROUNDED =
                   100 * A-ACCEPTED / A-PRESENTED
               MOVE C-QUAL-PCT TO C-PCT-ED
               MOVE C-PCT-ED TO D-QUAL-TX
           END-IF
           IF A-LEAD-CNT = 0
               MOVE 0 TO C-AVG-LEAD C-QUOTED-LEAD
           ELSE
               COMPUTE C-AVG-LEAD ROUNDED =
                   A-LEAD-SUM / A-LEAD-CNT
               COMPUTE C-QUOTED-LEAD ROUNDED =
                   A-QLEAD-SUM / A-LEAD-CNT
           END-IF
           MOVE A-PPV TO D-PPV
           MOVE A-VALUE TO D-VALUE
           MOVE C-AVG-LEAD TO D-AVG-LEAD
           MOVE C-QUOTED-LEAD TO D-QUOTED-LEAD
           IF WS-BASIS-IX = 1
               MOVE C-ONTIME-PCT TO CMP-ORIG-PCT
               ADD A-PPV TO T-PPV
               ADD A-VALUE TO T-VALUE
           ELSE
               IF C-ONTIME-PCT > CMP-ORIG-PCT
                   ADD 1 TO T-DIVERGENT
               END-IF
      *>         Quality, price and lead time do not depend on which
      *>         promise is used.  They are printed once, on the
      *>         ORIGINAL row, so that nobody adds the two rows up.
               MOVE "    same" TO D-QUAL-TX D-INFULL-TX
               MOVE 0 TO D-PPV D-VALUE D-AVG-LEAD D-QUOTED-LEAD
           END-IF
           ADD 1 TO T-ROWS
           IF WS-COMMIT
               PERFORM 1400-STORE THRU 1400-EXIT
           END-IF
           GENERATE DE-LINE.
       1320-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> The measure row is replaced rather than added to, so the run
      *> may be repeated for the same period without doubling.
      *>----------------------------------------------------------------
       1400-STORE SECTION.
       1400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.SupplierPerformance WHERE "
                  "CompanyCode = ? AND SupplierCode = ? AND "
                  "PeriodKey = ? AND MeasureBasis = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 40
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-ACC-SUPP TO SQLIO-PARM-VAL(2)
           MOVE WS-ACC-PERIOD TO SQLIO-PARM-VAL(3)
           MOVE D-BASIS TO SQLIO-PARM-VAL(4)
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.SupplierPerformance (CompanyCode, "
                  "SupplierCode, PeriodKey, MeasureBasis, "
                  "ToleranceDays, ReceiptCount, OnTimeCount, "
                  "InFullCount, OtifCount, OnTimePct, InFullPct, "
                  "OtifPct, AcceptedQty, RejectedQty, "
                  "QualityAcceptPct, PriceVarianceGBP, "
                  "PurchaseValueGBP, AvgLeadTimeDays, "
                  "QuotedLeadTimeDays) VALUES (?,?,?,?,?,?,?,?,?,"
                  "?,?,?,?,?,?,?,?,?,?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-ACC-SUPP TO SQLIO-PARM-VAL(2)
           MOVE WS-ACC-PERIOD TO SQLIO-PARM-VAL(3)
           MOVE D-BASIS TO SQLIO-PARM-VAL(4)
           MOVE WS-TOL-DAYS TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(5)
           MOVE A-RECEIPTS TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(6)
           MOVE A-ONTIME(WS-BASIS-IX) TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(7)
           MOVE A-INFULL TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(8)
           MOVE A-OTIF(WS-BASIS-IX) TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(9)
           MOVE C-ONTIME-PCT TO WS-NUM-ED
           MOVE FUNCTION TRIM(WS-NUM-ED) TO SQLIO-PARM-VAL(10)
           MOVE C-INFULL-PCT TO WS-NUM-ED
           MOVE FUNCTION TRIM(WS-NUM-ED) TO SQLIO-PARM-VAL(11)
           MOVE C-OTIF-PCT TO WS-NUM-ED
           MOVE FUNCTION TRIM(WS-NUM-ED) TO SQLIO-PARM-VAL(12)
           MOVE A-ACCEPTED TO WS-NUM-ED
           MOVE FUNCTION TRIM(WS-NUM-ED) TO SQLIO-PARM-VAL(13)
           MOVE A-REJECTED TO WS-NUM-ED
           MOVE FUNCTION TRIM(WS-NUM-ED) TO SQLIO-PARM-VAL(14)
           MOVE C-QUAL-PCT TO WS-NUM-ED
           MOVE FUNCTION TRIM(WS-NUM-ED) TO SQLIO-PARM-VAL(15)
           MOVE A-PPV TO WS-NUM-ED
           MOVE FUNCTION TRIM(WS-NUM-ED) TO SQLIO-PARM-VAL(16)
           MOVE A-VALUE TO WS-NUM-ED
           MOVE FUNCTION TRIM(WS-NUM-ED) TO SQLIO-PARM-VAL(17)
           MOVE C-AVG-LEAD TO WS-NUM-ED
           MOVE FUNCTION TRIM(WS-NUM-ED) TO SQLIO-PARM-VAL(18)
           MOVE C-QUOTED-LEAD TO WS-NUM-ED
           MOVE FUNCTION TRIM(WS-NUM-ED) TO SQLIO-PARM-VAL(19)
           MOVE 19 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW
           IF SQLIO-ERROR
               DISPLAY "STERP-W-1735 " FUNCTION TRIM(SQLIO-ERRMSG)
                   UPON SYSOUT
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
           ELSE
               ADD 1 TO WS-ROWS-WRITTEN
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW.
       1400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-SUPPPERF
           CLOSE RPT-FILE
           DISPLAY "STERP-I-1736 " T-ROWS " measure row(s) from "
                   WS-ROWS-READ " receipt line(s)" UPON SYSOUT
           IF WS-COMMIT
               DISPLAY "STERP-I-1737 " WS-ROWS-WRITTEN
                       " row(s) stored in po.SupplierPerformance"
                   UPON SYSOUT
           ELSE
               DISPLAY "STERP-I-1738 PREVIEW - nothing stored"
                   UPON SYSOUT
           END-IF
           IF T-DIVERGENT > 0
               DISPLAY "STERP-W-1739 " T-DIVERGENT
                       " measure(s) better on the current promise"
                   UPON SYSOUT
           END-IF
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
           MOVE "SUPPPERF" TO JL-JOB-NAME
           EVALUATE TRUE
               WHEN WS-RC = 0
                   MOVE "OK" TO JL-STATUS
               WHEN WS-RC = 4
                   MOVE "WARNING" TO JL-STATUS
               WHEN OTHER
                   MOVE "FAILED" TO JL-STATUS
           END-EVALUATE
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE T-ROWS TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
