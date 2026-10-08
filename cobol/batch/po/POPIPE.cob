      *>================================================================
      *> PROGRAM-ID : POPIPE
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : PURCHASING PIPELINE AND GRNI REPORT, REPORT CODE
      *>              RPT-009.  EVERY OPEN PURCHASE ORDER LINE WITH
      *>              ITS ORDERED, CANCELLED, RECEIVED, INVOICED AND
      *>              OUTSTANDING QUANTITIES, THE COMMITMENT STILL TO
      *>              COME AND THE GOODS RECEIVED NOT INVOICED
      *>              BALANCE IT HAS ALREADY CREATED.
      *>              THE THREE STAGES - COMMITTED, ACCRUED AND
      *>              INVOICED - ARE PRINTED SIDE BY SIDE AND NEVER
      *>              ADDED TOGETHER FOR THE SAME PORTION OF A LINE,
      *>              BECAUSE A QUANTITY THAT HAS BEEN RECEIVED IS NO
      *>              LONGER A COMMITMENT AND A QUANTITY THAT HAS
      *>              BEEN INVOICED IS NO LONGER AN ACCRUAL.
      *>              THE ORIGINAL AND THE CURRENT PROMISED DATES ARE
      *>              BOTH SHOWN SO THAT RE-DATING A LATE ORDER
      *>              CANNOT MAKE IT LOOK ON TIME.
      *> RUN AS     : POPIPE CO=STC [SITE=RDG] [SUPP=S0001]
      *>              [ASAT=yyyymmdd] USER=BUYER1
      *> OUTPUT     : run\spool\POPIPE-<CO>-<stamp>.lst  (132 col)
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-02-12 RJH  CR0502 ORDER BOOK AND ACCRUAL LISTING
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS THROUGHOUT
      *> 2007-03-19 DJM  CR1288 STAGES SHOWN SEPARATELY AFTER THE
      *>                 AUDIT FOUND COMMITMENT AND ACCRUAL BEING
      *>                 ADDED TOGETHER IN THE MONTH END PACK
      *> 2026-10-06 STERP PO-017, PO-020, PO-022, RPT-009
      *>================================================================
      *> REQ: PO-017, PO-020, PO-022, RPT-009
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POPIPE.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 12/02/1996.
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
       FD  RPT-FILE REPORT IS R-POPIPE.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY STDATES.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       01  PO-ROW.
           05  PO-COL OCCURS 30 TIMES PIC X(100).
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-SITE                 PIC X(10) VALUE SPACES.
           05  WS-SUPP                 PIC X(20) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-ASAT                 PIC X(8) VALUE SPACES.
       01  WS-SCRATCH.
           05  WS-IX                   PIC S9(4) VALUE 0.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-TODAY-N              PIC 9(8) VALUE 0.
           05  WS-DONE                 PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-WORK-DATE            PIC 9(8) VALUE 0.
       01  WS-DET.
           05  D-SUPP                  PIC X(14) VALUE SPACES.
           05  D-ORDER                 PIC X(14) VALUE SPACES.
           05  D-LINE                  PIC 9(4) VALUE 0.
           05  D-ITEM                  PIC X(18) VALUE SPACES.
           05  D-ORDERED               PIC S9(11)V9(3) VALUE 0.
           05  D-CANCELLED             PIC S9(11)V9(3) VALUE 0.
           05  D-RECEIVED              PIC S9(11)V9(3) VALUE 0.
           05  D-INVOICED              PIC S9(11)V9(3) VALUE 0.
           05  D-OUTSTANDING           PIC S9(11)V9(3) VALUE 0.
           05  D-COMMIT-GBP            PIC S9(11)V99 VALUE 0.
           05  D-GRNI-GBP              PIC S9(11)V99 VALUE 0.
           05  D-INVOICED-GBP          PIC S9(11)V99 VALUE 0.
           05  D-ORIG-PROM             PIC X(10) VALUE SPACES.
           05  D-CURR-PROM             PIC X(10) VALUE SPACES.
           05  D-STATUS                PIC X(12) VALUE SPACES.
       01  WS-TOT.
           05  T-LINES                 PIC S9(7) VALUE 0.
           05  T-COMMIT-GBP            PIC S9(13)V99 VALUE 0.
           05  T-GRNI-GBP              PIC S9(13)V99 VALUE 0.
           05  T-INVOICED-GBP          PIC S9(13)V99 VALUE 0.
           05  T-OVERDUE-LINES         PIC S9(7) VALUE 0.
           05  T-REDATED-LINES         PIC S9(7) VALUE 0.
       01  WS-DATE-ED.
           05  WS-ED-DD                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-ED-MM                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-ED-YYYY              PIC 9(4) VALUE 0.
       REPORT SECTION.
       RD  R-POPIPE
           CONTROLS ARE FINAL D-SUPP
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 54
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-SUPP TYPE IS CONTROL HEADING D-SUPP.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(10) VALUE "Supplier: ".
               10  COLUMN 11   PIC X(14) SOURCE D-SUPP.
               10  COLUMN 30   PIC X(46) VALUE
                   "Committed, accrued and invoiced are STAGES -".
               10  COLUMN 77   PIC X(42) VALUE
                   "never added together for one portion".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(5) VALUE "Order".
               10  COLUMN 16   PIC X(4) VALUE "Line".
               10  COLUMN 21   PIC X(4) VALUE "Item".
               10  COLUMN 40   PIC X(9) VALUE "  Ordered".
               10  COLUMN 50   PIC X(9) VALUE "Cancelled".
               10  COLUMN 60   PIC X(9) VALUE " Received".
               10  COLUMN 70   PIC X(9) VALUE " Invoiced".
               10  COLUMN 80   PIC X(9) VALUE "Outstandg".
               10  COLUMN 90   PIC X(13) VALUE "   Commitment".
               10  COLUMN 104  PIC X(13) VALUE "         GRNI".
               10  COLUMN 118  PIC X(13) VALUE "     Invoiced".
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(16) VALUE "Original promise".
               10  COLUMN 23   PIC X(15) VALUE "Current promise".
               10  COLUMN 40   PIC X(6) VALUE "Status".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(14) SOURCE D-ORDER.
               10  COLUMN 16   PIC ZZZ9 SOURCE D-LINE.
               10  COLUMN 21   PIC X(18) SOURCE D-ITEM.
               10  COLUMN 40   PIC Z(7)9.999 SOURCE D-ORDERED.
               10  COLUMN 50   PIC Z(7)9.999 SOURCE D-CANCELLED.
               10  COLUMN 60   PIC Z(7)9.999 SOURCE D-RECEIVED.
               10  COLUMN 70   PIC Z(7)9.999 SOURCE D-INVOICED.
               10  COLUMN 80   PIC Z(7)9.999 SOURCE D-OUTSTANDING.
               10  COLUMN 90   PIC Z(9)9.99- SOURCE D-COMMIT-GBP.
               10  COLUMN 104  PIC Z(9)9.99- SOURCE D-GRNI-GBP.
               10  COLUMN 118  PIC Z(9)9.99- SOURCE D-INVOICED-GBP.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(10) SOURCE D-ORIG-PROM.
               10  COLUMN 23   PIC X(10) SOURCE D-CURR-PROM.
               10  COLUMN 40   PIC X(12) SOURCE D-STATUS.
       01  CF-SUPP TYPE IS CONTROL FOOTING D-SUPP.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(26) VALUE "Open order lines".
               10  COLUMN 28   PIC Z(6)9 SOURCE T-LINES.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(26)
                               VALUE "Commitment still to come".
               10  COLUMN 28   PIC Z(11)9.99- SOURCE T-COMMIT-GBP.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(26)
                               VALUE "Goods received not invcd".
               10  COLUMN 28   PIC Z(11)9.99- SOURCE T-GRNI-GBP.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(26) VALUE "Invoiced and matched".
               10  COLUMN 28   PIC Z(11)9.99- SOURCE T-INVOICED-GBP.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(26)
                               VALUE "Overdue on orig promise".
               10  COLUMN 28   PIC Z(6)9 SOURCE T-OVERDUE-LINES.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(26)
                               VALUE "Promise re-dated at least".
               10  COLUMN 28   PIC Z(6)9 SOURCE T-REDATED-LINES.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(52) VALUE
                   "Outstanding = ordered less cancelled less".
               10  COLUMN 54   PIC X(60) VALUE
                   "received; commitment values that quantity only.".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(52) VALUE
                   "GRNI is the received but not yet invoiced value:".
               10  COLUMN 54   PIC X(60) VALUE
                   "it is the accrual the nominal ledger carries.".
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
           MOVE CM-ASAT TO WS-ASAT
           MOVE "SUPP" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-SUPP
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "BUYER1" TO WS-USER
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           IF WS-ASAT = SPACES
               MOVE WS-TODAY-N TO WS-ASAT
           END-IF
           DISPLAY "STERP-I-1700 POPIPE purchasing pipeline for "
                   FUNCTION TRIM(WS-CO) UPON SYSOUT.
       0100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "POPIPE" TO JL-JOB-NAME
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
           MOVE "POPIPE" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1701 cannot build the spool path"
                   UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           PERFORM 0350-FILL-HEADINGS THRU 0350-EXIT
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1702 cannot open " WS-SPOOL-NAME
                   UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           INITIATE R-POPIPE.
       0300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "RPT-009" TO RPT-REPORT-ID
           MOVE "Purchasing pipeline and GRNI (RPT-009)"
               TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE SPACES TO RPT-SUBTITLE
           IF WS-SITE = SPACES
               STRING "All sites" DELIMITED BY SIZE
                   INTO RPT-SUBTITLE
           ELSE
               STRING "Site " FUNCTION TRIM(WS-SITE)
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           END-IF
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "As at " WS-ASAT(7:2) "/" WS-ASAT(5:2) "/"
                  WS-ASAT(1:4)
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
      *> REQ: PO-017, PO-020 - one row per open order line.  The
      *> received value comes from the confirmed receipts, the
      *> invoiced value from the matched quantity, and GRNI is the
      *> difference between them.  A reversed receipt is excluded, so
      *> the counted quantity falls when a receipt is reversed.
      *>----------------------------------------------------------------
       1000-PRODUCE SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT o.SupplierCode, l.OrderNumber, "
                  "CAST(l.LineNum AS varchar(10)), "
                  "ISNULL(l.ItemCode, LEFT(ISNULL(l.Description,"
                  "'(no item)'),18)), CAST(l.OrderedQty AS "
                  "varchar(30)), CAST(l.CancelledQty AS "
                  "varchar(30)), CAST(l.ReceivedQty AS "
                  "varchar(30)), CAST(l.InvoicedQty AS "
                  "varchar(30)), CAST(l.UnitPrice / NULLIF("
                  "l.PricePerQty,0) AS varchar(30)), "
                  "ISNULL(CONVERT(char(8),s.OriginalConfirmedDate,"
                  "112),''), ISNULL(CONVERT(char(8),"
                  "s.CurrentConfirmedDate,112),''), l.Status, "
                  "CASE WHEN s.CurrentConfirmedDate <> "
                  "s.OriginalConfirmedDate THEN '1' ELSE '0' END, "
                  "CASE WHEN CONVERT(char(8),"
                  "ISNULL(s.OriginalConfirmedDate,"
                  "s.CurrentConfirmedDate),112) < ? THEN '1' ELSE "
                  "'0' END FROM po.PurchaseOrderLine l JOIN "
                  "po.PurchaseOrder o ON o.PurchaseOrderId = "
                  "l.PurchaseOrderId LEFT JOIN "
                  "po.PurchaseSchedule s ON s.CompanyCode = "
                  "l.CompanyCode AND s.OrderNumber = l.OrderNumber "
                  "AND s.LineNum = l.LineNum WHERE "
                  "l.CompanyCode = ? AND l.Status = 'OPEN' AND "
                  "(? = '' OR o.SupplierCode = ?) ORDER BY "
                  "o.SupplierCode, l.OrderNumber, l.LineNum, "
                  "s.ScheduleNo"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-ASAT TO SQLIO-PARM-VAL(1)
           MOVE WS-CO TO SQLIO-PARM-VAL(2)
           MOVE WS-SUPP TO SQLIO-PARM-VAL(3)
           MOVE WS-SUPP TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE 14 TO SQLIO-COL-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 30
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1703 " FUNCTION TRIM(SQLIO-ERRMSG)
                   UPON SYSOUT
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA PO-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 1100-ONE-ROW THRU 1100-EXIT
                   ADD 1 TO WS-ROWS-OUT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1100-ONE-ROW SECTION.
       1100-START.
           MOVE PO-COL(1) TO D-SUPP
           MOVE PO-COL(2) TO D-ORDER
           COMPUTE D-LINE = FUNCTION NUMVAL(PO-COL(3))
           MOVE PO-COL(4) TO D-ITEM
           COMPUTE D-ORDERED = FUNCTION NUMVAL(PO-COL(5))
           COMPUTE D-CANCELLED = FUNCTION NUMVAL(PO-COL(6))
           COMPUTE D-RECEIVED = FUNCTION NUMVAL(PO-COL(7))
           COMPUTE D-INVOICED = FUNCTION NUMVAL(PO-COL(8))
           COMPUTE D-OUTSTANDING = D-ORDERED - D-CANCELLED
                                 - D-RECEIVED
           IF D-OUTSTANDING < 0
               MOVE 0 TO D-OUTSTANDING
           END-IF
      *>     The three stages are valued separately.  Committed is
      *>     what we still have to buy, GRNI what we have had but not
      *>     been billed for, invoiced what is already a payable.
           COMPUTE D-COMMIT-GBP ROUNDED = D-OUTSTANDING
                 * FUNCTION NUMVAL(PO-COL(9))
           COMPUTE D-GRNI-GBP ROUNDED = (D-RECEIVED - D-INVOICED)
                 * FUNCTION NUMVAL(PO-COL(9))
           IF D-GRNI-GBP < 0
               MOVE 0 TO D-GRNI-GBP
           END-IF
           COMPUTE D-INVOICED-GBP ROUNDED = D-INVOICED
                 * FUNCTION NUMVAL(PO-COL(9))
           MOVE PO-COL(10)(1:8) TO WS-WORK-DATE
           PERFORM 1450-FORMAT-DATE THRU 1450-EXIT
           MOVE WS-DATE-ED TO D-ORIG-PROM
           MOVE PO-COL(11)(1:8) TO WS-WORK-DATE
           PERFORM 1450-FORMAT-DATE THRU 1450-EXIT
           MOVE WS-DATE-ED TO D-CURR-PROM
           MOVE PO-COL(12) TO D-STATUS
           ADD 1 TO T-LINES
           ADD D-COMMIT-GBP TO T-COMMIT-GBP
           ADD D-GRNI-GBP TO T-GRNI-GBP
           ADD D-INVOICED-GBP TO T-INVOICED-GBP
           IF PO-COL(13)(1:1) = "1"
               ADD 1 TO T-REDATED-LINES
           END-IF
           IF PO-COL(14)(1:1) = "1" AND D-OUTSTANDING > 0
               ADD 1 TO T-OVERDUE-LINES
           END-IF
           GENERATE DE-LINE.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1450-FORMAT-DATE SECTION.
       1450-START.
           IF WS-WORK-DATE = 0
               MOVE 0 TO WS-ED-DD WS-ED-MM WS-ED-YYYY
           ELSE
               MOVE WS-WORK-DATE TO STD-DATE-N
               MOVE STD-DD TO WS-ED-DD
               MOVE STD-MM TO WS-ED-MM
               MOVE STD-YYYY TO WS-ED-YYYY
           END-IF.
       1450-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-POPIPE
           CLOSE RPT-FILE
           DISPLAY "STERP-I-1704 " WS-ROWS-OUT
                   " open order line(s) reported" UPON SYSOUT
           IF T-OVERDUE-LINES > 0
               DISPLAY "STERP-W-1705 " T-OVERDUE-LINES
                       " line(s) overdue on the original promise"
                   UPON SYSOUT
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
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
           MOVE "POPIPE" TO JL-JOB-NAME
           EVALUATE TRUE
               WHEN WS-RC = 0
                   MOVE "OK" TO JL-STATUS
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
