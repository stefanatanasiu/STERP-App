      *>================================================================
      *> PROGRAM-ID : OVERDUE
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : OVERDUE PURCHASE ORDERS AND DELIVERY EXCEPTIONS.
      *>              EVERY OPEN SCHEDULE THAT HAS PASSED ITS PROMISED
      *>              DATE, WITH THE BUYER WHO OWNS IT, THE LAST
      *>              ACTION TAKEN, AND THE SALES ORDER OR JOB THAT IS
      *>              WAITING FOR IT.  AN OVERDUE ORDER IS A PROMISE
      *>              TO A CUSTOMER THAT IS AT RISK, SO THE LISTING
      *>              IS DRIVEN BY OWNERSHIP - IT BREAKS ON THE BUYER
      *>              AND NAMES SOMEONE ON EVERY LINE.
      *>              LATENESS IS MEASURED AGAINST THE ORIGINAL
      *>              PROMISE AND AGAINST THE CURRENT PROMISE AND
      *>              BOTH ARE PRINTED.  RE-DATING A LATE LINE
      *>              THEREFORE MOVES THE CURRENT FIGURE ONLY; THE
      *>              ORIGINAL FAILURE STAYS ON THE REPORT AND IS
      *>              FLAGGED AS AN EXCEPTION.
      *> RUN AS     : OVERDUE CO=STC [SITE=RDG] [SUPP=S0001]
      *>              [BUYER=BUYER1] [DAYS=1] [ASAT=yyyymmdd]
      *>              USER=BUYER1
      *> OUTPUT     : run\spool\OVERDUE-<CO>-<stamp>.lst  (132 col)
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-06-02 RJH  CR0611 LATE DELIVERY CHASE LIST
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS THROUGHOUT
      *> 2008-09-15 DJM  CR1402 ORIGINAL PROMISE KEPT AFTER THE
      *>                 BUYERS WERE FOUND RE-DATING LATE LINES THE
      *>                 DAY BEFORE THE MEASURE WAS TAKEN
      *> 2026-10-06 STERP PO-021, PO-022
      *>================================================================
      *> REQ: PO-021, PO-022
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OVERDUE.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 02/06/1997.
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
       FD  RPT-FILE REPORT IS R-OVERDUE.
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
           05  WS-BUYER                PIC X(20) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-ASAT                 PIC X(8) VALUE SPACES.
           05  WS-MIN-DAYS             PIC S9(4) VALUE 1.
       01  WS-SCRATCH.
           05  WS-IX                   PIC S9(4) VALUE 0.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-TODAY-N              PIC 9(8) VALUE 0.
           05  WS-DONE                 PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-ROWS-READ            PIC S9(9) VALUE 0.
           05  WS-WORK-DATE            PIC 9(8) VALUE 0.
           05  WS-ANY                  PIC X VALUE "N".
       01  WS-DET.
           05  D-BUYER                 PIC X(20) VALUE SPACES.
           05  D-SUPP                  PIC X(12) VALUE SPACES.
           05  D-ORDER                 PIC X(14) VALUE SPACES.
           05  D-LINE                  PIC 9(4) VALUE 0.
           05  D-ITEM                  PIC X(18) VALUE SPACES.
           05  D-OUTSTANDING           PIC S9(11)V9(3) VALUE 0.
           05  D-ORIG-PROM             PIC X(10) VALUE SPACES.
           05  D-CURR-PROM             PIC X(10) VALUE SPACES.
           05  D-DAYS-ORIG             PIC S9(5) VALUE 0.
           05  D-DAYS-CURR             PIC S9(5) VALUE 0.
           05  D-VALUE-GBP             PIC S9(11)V99 VALUE 0.
           05  D-STATUS                PIC X(12) VALUE SPACES.
           05  D-EXCEPTION             PIC X(8) VALUE SPACES.
           05  D-SALES-ORDER           PIC X(14) VALUE SPACES.
           05  D-JOB-REF               PIC X(12) VALUE SPACES.
           05  D-ACTION-NOTE           PIC X(80) VALUE SPACES.
           05  D-ONE                   PIC 9 VALUE 1.
       01  WS-DAYS-ED                  PIC Z(3)9 VALUE ZERO.
       01  WS-TOT.
           05  T-LINES                 PIC S9(7) VALUE 0.
           05  T-VALUE-GBP             PIC S9(13)V99 VALUE 0.
           05  T-ONLY-ORIG             PIC S9(7) VALUE 0.
           05  T-NO-ACK                PIC S9(7) VALUE 0.
           05  T-CUSTOMER-HIT          PIC S9(7) VALUE 0.
       01  WS-DATE-ED.
           05  WS-ED-DD                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-ED-MM                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-ED-YYYY              PIC 9(4) VALUE 0.
       REPORT SECTION.
       RD  R-OVERDUE
           CONTROLS ARE FINAL D-BUYER
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 54
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-BUYER TYPE IS CONTROL HEADING D-BUYER.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(7) VALUE "Buyer: ".
               10  COLUMN 8    PIC X(20) SOURCE D-BUYER.
               10  COLUMN 32   PIC X(44) VALUE
                   "Days late are shown against the ORIGINAL and".
               10  COLUMN 77   PIC X(48) VALUE
                   "the CURRENT promise - re-dating never clears it.".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(5) VALUE "Order".
               10  COLUMN 16   PIC X(4) VALUE "Line".
               10  COLUMN 21   PIC X(8) VALUE "Supplier".
               10  COLUMN 34   PIC X(4) VALUE "Item".
               10  COLUMN 53   PIC X(10) VALUE "Outstandng".
               10  COLUMN 64   PIC X(10) VALUE "Orig prom ".
               10  COLUMN 75   PIC X(10) VALUE "Curr prom ".
               10  COLUMN 86   PIC X(5) VALUE "Lt/Or".
               10  COLUMN 92   PIC X(5) VALUE "Lt/Cu".
               10  COLUMN 98   PIC X(13) VALUE " Value at risk".
               10  COLUMN 112  PIC X(6) VALUE "Status".
               10  COLUMN 125  PIC X(8) VALUE "Flag".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(14) SOURCE D-ORDER.
               10  COLUMN 16   PIC ZZZ9 SOURCE D-LINE.
               10  COLUMN 21   PIC X(12) SOURCE D-SUPP.
               10  COLUMN 34   PIC X(18) SOURCE D-ITEM.
               10  COLUMN 53   PIC Z(6)9.999 SOURCE D-OUTSTANDING.
               10  COLUMN 64   PIC X(10) SOURCE D-ORIG-PROM.
               10  COLUMN 75   PIC X(10) SOURCE D-CURR-PROM.
               10  COLUMN 86   PIC ZZZ9- SOURCE D-DAYS-ORIG.
               10  COLUMN 92   PIC ZZZ9- SOURCE D-DAYS-CURR.
               10  COLUMN 98   PIC Z(9)9.99- SOURCE D-VALUE-GBP.
               10  COLUMN 112  PIC X(12) SOURCE D-STATUS.
               10  COLUMN 125  PIC X(8) SOURCE D-EXCEPTION.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(8) VALUE "Impact: ".
               10  COLUMN 13   PIC X(14) SOURCE D-SALES-ORDER.
               10  COLUMN 28   PIC X(12) SOURCE D-JOB-REF.
               10  COLUMN 41   PIC X(6) VALUE "Note: ".
               10  COLUMN 47   PIC X(80) SOURCE D-ACTION-NOTE.
       01  CF-BUYER TYPE IS CONTROL FOOTING D-BUYER.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(24) VALUE
                   "Buyer total late lines: ".
               10  COLUMN 29   PIC ZZZZ9 SUM D-ONE.
               10  COLUMN 40   PIC X(16) VALUE "Value at risk: ".
               10  COLUMN 56   PIC Z(11)9.99- SUM D-VALUE-GBP.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(26) VALUE
                   "Overdue lines in total  : ".
               10  COLUMN 31   PIC ZZZZ9 SOURCE T-LINES.
               10  COLUMN 45   PIC X(26) VALUE
                   "Value at risk           : ".
               10  COLUMN 71   PIC Z(11)9.99- SOURCE T-VALUE-GBP.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(26) VALUE
                   "Late on original only   : ".
               10  COLUMN 31   PIC ZZZZ9 SOURCE T-ONLY-ORIG.
               10  COLUMN 45   PIC X(46) VALUE
                   "these were re-dated and look on time today".
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(26) VALUE
                   "Never acknowledged      : ".
               10  COLUMN 31   PIC ZZZZ9 SOURCE T-NO-ACK.
               10  COLUMN 45   PIC X(46) VALUE
                   "issued but no supplier acknowledgement held".
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(26) VALUE
                   "Holding up a customer   : ".
               10  COLUMN 31   PIC ZZZZ9 SOURCE T-CUSTOMER-HIT.
               10  COLUMN 45   PIC X(46) VALUE
                   "linked to a sales order or a works job".
           05  LINE PLUS 2.
               10  COLUMN 5    PIC X(62) VALUE
                   "Every line names an owner.  A line with no buyer".
               10  COLUMN 68   PIC X(60) VALUE
                   "is listed under (unassigned) and is itself an".
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(62) VALUE
                   "exception: nobody is chasing it.".
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
           MOVE "BUYER" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-BUYER
           END-IF
           MOVE "DAYS" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               COMPUTE WS-MIN-DAYS =
                   FUNCTION NUMVAL(CM-LOOKUP-VALUE)
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
           DISPLAY "STERP-I-1720 OVERDUE late purchase orders for "
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
           MOVE "OVERDUE" TO JL-JOB-NAME
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
           MOVE "OVERDUE" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1721 cannot build the spool path"
                   UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           PERFORM 0350-FILL-HEADINGS THRU 0350-EXIT
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1722 cannot open " WS-SPOOL-NAME
                   UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           INITIATE R-OVERDUE.
       0300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "RPT-022" TO RPT-REPORT-ID
           MOVE "Overdue purchase orders and exceptions (PO-022)"
               TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE SPACES TO RPT-SUBTITLE
           IF WS-BUYER = SPACES
               MOVE WS-MIN-DAYS TO WS-DAYS-ED
               STRING "All buyers, at least " DELIMITED BY SIZE
                      FUNCTION TRIM(WS-DAYS-ED) DELIMITED BY SIZE
                      " day(s) late" DELIMITED BY SIZE
                   INTO RPT-SUBTITLE
           ELSE
               STRING "Buyer " FUNCTION TRIM(WS-BUYER)
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
      *> REQ: PO-022 - one row per open schedule that is past its
      *> promise.  Lateness is calculated twice, against the original
      *> confirmed date and against the current one, so a buyer who
      *> re-dates a line to today still sees the original failure.
      *> The owning buyer, the last amendment note and the sales
      *> order or job the line feeds are carried on the row, because
      *> an exception with no owner and no consequence gets ignored.
      *>----------------------------------------------------------------
       1000-PRODUCE SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(NULLIF(o.BuyerUser,''),"
                  "'(unassigned)'), o.SupplierCode, l.OrderNumber, "
                  "CAST(l.LineNum AS varchar(10)), "
                  "ISNULL(l.ItemCode,LEFT(l.Description,18)), "
                  "CAST(s.Quantity - s.ReceivedQty AS varchar(30)), "
                  "ISNULL(CONVERT(char(8),"
                  "s.OriginalConfirmedDate,112),''), "
                  "ISNULL(CONVERT(char(8),"
                  "s.CurrentConfirmedDate,112),''), "
                  "CAST(DATEDIFF(day,ISNULL(s.OriginalConfirmedDate"
                  ",s.RequestedDate),CAST(? AS date)) AS "
                  "varchar(10)), CAST(DATEDIFF(day,"
                  "ISNULL(s.CurrentConfirmedDate,s.RequestedDate),"
                  "CAST(? AS date)) AS varchar(10)), "
                  "CAST((s.Quantity - s.ReceivedQty) * l.UnitPrice "
                  "/ NULLIF(l.PricePerQty,0) AS varchar(30)), "
                  "o.Status, CASE WHEN o.Status = 'ISSUED' AND "
                  "o.AcknowledgedAt IS NULL THEN 'NOACK' WHEN "
                  "s.CurrentConfirmedDate > "
                  "s.OriginalConfirmedDate THEN 'REDATED' WHEN "
                  "s.ChangeCount > 0 THEN 'RESCHED' ELSE 'LATE' "
                  "END, ISNULL(NULLIF(l.LinkedSalesOrderNumber,''),"
                  "ISNULL(o.LinkedSalesOrderNumber,'')), "
                  "ISNULL(l.JobRef,''), ISNULL((SELECT TOP 1 "
                  "ISNULL(a.ActionNote,ISNULL(a.Reason,'')) FROM "
                  "po.OrderAmendment a WHERE a.CompanyCode = "
                  "l.CompanyCode AND a.OrderNumber = l.OrderNumber "
                  "AND (a.LineNum IS NULL OR a.LineNum = l.LineNum)"
                  " ORDER BY a.AmendmentNo DESC),'') "
                  "FROM po.PurchaseSchedule s JOIN "
                  "po.PurchaseOrderLine l ON l.PurchaseOrderLineId "
                  "= s.PurchaseOrderLineId JOIN po.PurchaseOrder o "
                  "ON o.PurchaseOrderId = l.PurchaseOrderId WHERE "
                  "s.CompanyCode = ? AND l.Status = 'OPEN' AND "
                  "s.Status IN ('OPEN','PARTIAL') AND s.Quantity > "
                  "s.ReceivedQty AND o.Status IN ('APPROVED',"
                  "'ISSUED','ACKNOWLEDGED','AMENDED') AND "
                  "(? = '' OR o.SupplierCode = ?) AND (? = '' OR "
                  "ISNULL(o.BuyerUser,'') = ?) AND (? = '' OR "
                  "o.SiteCode = ?) ORDER BY 1, o.SupplierCode, "
                  "l.OrderNumber, l.LineNum, s.ScheduleNo"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 40
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE WS-ASAT TO SQLIO-PARM-VAL(1)
           MOVE WS-ASAT TO SQLIO-PARM-VAL(2)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           MOVE WS-SUPP TO SQLIO-PARM-VAL(4)
           MOVE WS-SUPP TO SQLIO-PARM-VAL(5)
           MOVE WS-BUYER TO SQLIO-PARM-VAL(6)
           MOVE WS-BUYER TO SQLIO-PARM-VAL(7)
           MOVE WS-SITE TO SQLIO-PARM-VAL(8)
           MOVE WS-SITE TO SQLIO-PARM-VAL(9)
           MOVE 9 TO SQLIO-PARM-CNT
           MOVE 16 TO SQLIO-COL-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 30
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1723 " FUNCTION TRIM(SQLIO-ERRMSG)
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
                   ADD 1 TO WS-ROWS-READ
                   PERFORM 1100-ONE-ROW THRU 1100-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PO-ROW.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1100-ONE-ROW SECTION.
       1100-START.
           COMPUTE D-DAYS-ORIG = FUNCTION NUMVAL(PO-COL(9))
           COMPUTE D-DAYS-CURR = FUNCTION NUMVAL(PO-COL(10))
      *>     A line is reported when it has missed EITHER promise.
      *>     Dropping the original test here would let a re-dated
      *>     order disappear from the chase list (CR1402).
           IF D-DAYS-ORIG < WS-MIN-DAYS AND
              D-DAYS-CURR < WS-MIN-DAYS
               GO TO 1100-EXIT
           END-IF
           MOVE PO-COL(1) TO D-BUYER
           MOVE PO-COL(2) TO D-SUPP
           MOVE PO-COL(3) TO D-ORDER
           COMPUTE D-LINE = FUNCTION NUMVAL(PO-COL(4))
           MOVE PO-COL(5) TO D-ITEM
           COMPUTE D-OUTSTANDING = FUNCTION NUMVAL(PO-COL(6))
           MOVE PO-COL(7)(1:8) TO WS-WORK-DATE
           PERFORM 1450-FORMAT-DATE THRU 1450-EXIT
           MOVE WS-DATE-ED TO D-ORIG-PROM
           MOVE PO-COL(8)(1:8) TO WS-WORK-DATE
           PERFORM 1450-FORMAT-DATE THRU 1450-EXIT
           MOVE WS-DATE-ED TO D-CURR-PROM
           COMPUTE D-VALUE-GBP ROUNDED = FUNCTION NUMVAL(PO-COL(11))
           MOVE PO-COL(12) TO D-STATUS
           MOVE PO-COL(13) TO D-EXCEPTION
           MOVE PO-COL(14) TO D-SALES-ORDER
           MOVE PO-COL(15) TO D-JOB-REF
           MOVE PO-COL(16) TO D-ACTION-NOTE
           IF D-ACTION-NOTE = SPACES
               MOVE "(no chase recorded)" TO D-ACTION-NOTE
           END-IF
           MOVE "Y" TO WS-ANY
           ADD 1 TO T-LINES
           ADD D-VALUE-GBP TO T-VALUE-GBP
           IF D-DAYS-ORIG >= WS-MIN-DAYS AND
              D-DAYS-CURR < WS-MIN-DAYS
               ADD 1 TO T-ONLY-ORIG
           END-IF
           IF D-EXCEPTION = "NOACK"
               ADD 1 TO T-NO-ACK
           END-IF
           IF D-SALES-ORDER NOT = SPACES OR D-JOB-REF NOT = SPACES
               ADD 1 TO T-CUSTOMER-HIT
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
           TERMINATE R-OVERDUE
           CLOSE RPT-FILE
           DISPLAY "STERP-I-1724 " T-LINES
                   " overdue schedule(s) from " WS-ROWS-READ
                   " open schedule(s)" UPON SYSOUT
           IF T-ONLY-ORIG > 0
               DISPLAY "STERP-W-1725 " T-ONLY-ORIG
                       " line(s) late on the original promise only"
                   UPON SYSOUT
           END-IF
           IF T-LINES > 0 AND WS-RC < 4
               MOVE 4 TO WS-RC
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
           MOVE "OVERDUE" TO JL-JOB-NAME
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
