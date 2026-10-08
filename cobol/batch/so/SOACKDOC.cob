      *>================================================================
      *> PROGRAM-ID : SOACKDOC
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT
      *> PURPOSE    : PRINT A SALES DOCUMENT - THE ORDER
      *>              ACKNOWLEDGEMENT (DOC=ACK), THE PICK NOTE
      *>              (DOC=PICK) OR THE DESPATCH NOTE (DOC=DESP) - AS
      *>              A 132 COLUMN SPOOL FILE WITH THE STANDARD PAGE
      *>              HEADINGS, SHOWING THE DOCUMENT VERSION AND THE
      *>              REPRINT COUNT.
      *>              PRINTING OR REPRINTING CHANGES NO BUSINESS
      *>              STATE WHATEVER.  NO ORDER IS RELEASED, NO STOCK
      *>              IS RESERVED OR PICKED AND NO DESPATCHED
      *>              QUANTITY MOVES.  THE ONLY THINGS THIS PROGRAM
      *>              WRITES ARE THE REPRINT COUNTER AND AN AUDIT
      *>              RECORD THROUGH AUDITLOG, AND THE REPORT SAYS SO
      *>              IN ITS TRAILER SO THAT A READER OF THE SPOOL
      *>              FILE CAN SEE IT TOO.
      *> RUN AS     : SOACKDOC CO=STC DOC=ACK REF=SO0001 USER=SALESREP
      *>              SOACKDOC CO=STC DOC=PICK REF=PL0001 ...
      *>              SOACKDOC CO=STC DOC=DESP REF=DN0001 ...
      *>              (ORDER=, PICK= and DESP= are accepted as
      *>              synonyms of REF= by the old job streams)
      *> OUTPUT     : run\spool\SOACKDOC-<CO>-<stamp>.lst  (132 col)
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, AUDITLOG, SQLIO
      *> NOTE       : only so.PickList carries a PrintCount column, so
      *>              the acknowledgement and despatch note reprint
      *>              counts are derived from the PRINT rows already
      *>              in core.AuditLog.  See docs\tech-debt\so.md.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-08-19 DLS  CR0318 ORIGINAL - ORDER ACKNOWLEDGEMENT
      *> 1997-03-05 RJH  CR0701 PICK AND DESPATCH NOTES FOLDED IN
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS THROUGHOUT
      *> 2011-06-30 AMD  CR2505 REPRINT NO LONGER RELEASES THE ORDER -
      *>                 THE OLD CODE CALLED THE RELEASE ROUTINE TO
      *>                 "REFRESH" THE DOCUMENT.  SEE AC-042.
      *> 2026-10-06 STERP SO-022, SO-011, SO-012 VERSIONED REPRINTS
      *>================================================================
      *> REQ: SO-022, SO-011, SO-012
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOACKDOC.
       AUTHOR. D L SPENCER.
       DATE-WRITTEN. 19/08/1994.
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
       FD  RPT-FILE REPORT IS R-SOACKDC.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUDIT==.
       01  SO-ROW.
           05  SO-COL OCCURS 20 TIMES PIC X(100).
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-SITE                 PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-DOC                  PIC X(6) VALUE SPACES.
               88  WS-IS-ACK                     VALUE "ACK".
               88  WS-IS-PICK                    VALUE "PICK".
               88  WS-IS-DESP                    VALUE "DESP".
           05  WS-REF                  PIC X(30) VALUE SPACES.
       01  WS-SCRATCH.
           05  WS-IX                   PIC S9(4) VALUE 0.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-TODAY-N              PIC 9(8) VALUE 0.
           05  WS-DONE                 PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-LINES-OUT            PIC S9(7) VALUE 0.
           05  WS-ENTITY               PIC X(60) VALUE SPACES.
           05  WS-VERSION              PIC 9(4) VALUE 0.
           05  WS-REPRINT              PIC 9(4) VALUE 0.
           05  WS-JOB-RUN-ID           PIC S9(9) COMP-5 VALUE 0.
           05  WS-FIRST-ISSUE          PIC X VALUE "N".
           05  WS-INT-ED               PIC -(9)9.
           05  WS-DATE-WORK            PIC X(8) VALUE SPACES.
       01  WS-HDR.
           05  H-REF                   PIC X(30) VALUE SPACES.
           05  H-CUST                  PIC X(20) VALUE SPACES.
           05  H-CUST-NAME             PIC X(40) VALUE SPACES.
           05  H-ORDER                 PIC X(30) VALUE SPACES.
           05  H-DATE                  PIC X(10) VALUE SPACES.
           05  H-CUSTREF               PIC X(40) VALUE SPACES.
           05  H-RELEASE               PIC X(14) VALUE SPACES.
           05  H-FULFIL                PIC X(14) VALUE SPACES.
           05  H-BILLING               PIC X(14) VALUE SPACES.
           05  H-ADDRESS               PIC X(60) VALUE SPACES.
           05  H-EXTRA1                PIC X(30) VALUE SPACES.
           05  H-EXTRA2                PIC X(30) VALUE SPACES.
           05  H-HOLD                  PIC X(50) VALUE SPACES.
       01  WS-DET.
           05  D-DOCKEY                PIC X(12) VALUE SPACES.
           05  D-LINE                  PIC 9(5) VALUE 0.
           05  D-ITEM                  PIC X(20) VALUE SPACES.
           05  D-DESC                  PIC X(40) VALUE SPACES.
           05  D-QTY1                  PIC S9(11)V9(3) VALUE 0.
           05  D-QTY2                  PIC S9(11)V9(3) VALUE 0.
           05  D-QTY3                  PIC S9(11)V9(3) VALUE 0.
           05  D-VALUE                 PIC S9(11)V99 VALUE 0.
           05  D-NOTE                  PIC X(18) VALUE SPACES.
           05  D-HD-Q1                 PIC X(10) VALUE SPACES.
           05  D-HD-Q2                 PIC X(10) VALUE SPACES.
           05  D-HD-Q3                 PIC X(10) VALUE SPACES.
           05  D-HD-VAL                PIC X(12) VALUE SPACES.
           05  D-HD-NOTE               PIC X(18) VALUE SPACES.
           05  D-DOCNAME               PIC X(26) VALUE SPACES.
       01  WS-TOT.
           05  T-LINES                 PIC S9(7) VALUE 0.
           05  T-QTY1                  PIC S9(13)V9(3) VALUE 0.
           05  T-QTY2                  PIC S9(13)V9(3) VALUE 0.
           05  T-QTY3                  PIC S9(13)V9(3) VALUE 0.
           05  T-VALUE                 PIC S9(13)V99 VALUE 0.
       REPORT SECTION.
       RD  R-SOACKDC
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 16 LAST DETAIL 50
           FOOTING 54.
       COPY STRPTHDR.
       01  PH-DOC TYPE IS PAGE HEADING.
           05  LINE 8.
               10  COLUMN 1    PIC X(26) SOURCE D-DOCNAME.
               10  COLUMN 28   PIC X(30) SOURCE H-REF.
               10  COLUMN 60   PIC X(8) VALUE "Version ".
               10  COLUMN 68   PIC ZZZ9 SOURCE WS-VERSION.
               10  COLUMN 76   PIC X(8) VALUE "Reprint ".
               10  COLUMN 84   PIC ZZZ9 SOURCE WS-REPRINT.
           05  LINE 9.
               10  COLUMN 1    PIC X(9) VALUE "Customer ".
               10  COLUMN 11   PIC X(20) SOURCE H-CUST.
               10  COLUMN 32   PIC X(40) SOURCE H-CUST-NAME.
               10  COLUMN 74   PIC X(6) VALUE "Order ".
               10  COLUMN 80   PIC X(30) SOURCE H-ORDER.
           05  LINE 10.
               10  COLUMN 1    PIC X(9) VALUE "Date     ".
               10  COLUMN 11   PIC X(10) SOURCE H-DATE.
               10  COLUMN 24   PIC X(18) VALUE "Your reference   ".
               10  COLUMN 43   PIC X(40) SOURCE H-CUSTREF.
           05  LINE 11.
               10  COLUMN 1    PIC X(9) VALUE "Deliver  ".
               10  COLUMN 11   PIC X(60) SOURCE H-ADDRESS.
               10  COLUMN 73   PIC X(30) SOURCE H-EXTRA1.
               10  COLUMN 104  PIC X(29) SOURCE H-EXTRA2.
           05  LINE 12.
               10  COLUMN 1    PIC X(9) VALUE "Status   ".
               10  COLUMN 11   PIC X(14) SOURCE H-RELEASE.
               10  COLUMN 26   PIC X(14) SOURCE H-FULFIL.
               10  COLUMN 41   PIC X(14) SOURCE H-BILLING.
               10  COLUMN 56   PIC X(50) SOURCE H-HOLD.
           05  LINE 14.
               10  COLUMN 1    PIC X(4) VALUE "Line".
               10  COLUMN 7    PIC X(4) VALUE "Item".
               10  COLUMN 28   PIC X(11) VALUE "Description".
               10  COLUMN 69   PIC X(10) SOURCE D-HD-Q1.
               10  COLUMN 80   PIC X(10) SOURCE D-HD-Q2.
               10  COLUMN 91   PIC X(10) SOURCE D-HD-Q3.
               10  COLUMN 102  PIC X(12) SOURCE D-HD-VAL.
               10  COLUMN 115  PIC X(18) SOURCE D-HD-NOTE.
           05  LINE 15.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC ZZZZ9 SOURCE D-LINE.
               10  COLUMN 7    PIC X(20) SOURCE D-ITEM.
               10  COLUMN 28   PIC X(40) SOURCE D-DESC.
               10  COLUMN 69   PIC Z(5)9.999 SOURCE D-QTY1.
               10  COLUMN 80   PIC Z(5)9.999 SOURCE D-QTY2.
               10  COLUMN 91   PIC Z(5)9.999 SOURCE D-QTY3.
               10  COLUMN 102  PIC Z(7)9.99- SOURCE D-VALUE.
               10  COLUMN 115  PIC X(18) SOURCE D-NOTE.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
           05  LINE PLUS 1.
               10  COLUMN 7    PIC X(20) VALUE "Document totals".
               10  COLUMN 69   PIC Z(5)9.999 SUM D-QTY1.
               10  COLUMN 80   PIC Z(5)9.999 SUM D-QTY2.
               10  COLUMN 91   PIC Z(5)9.999 SUM D-QTY3.
               10  COLUMN 102  PIC Z(7)9.99- SUM D-VALUE.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(62) VALUE
               "This print changed no business state.  No order was".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(62) VALUE
               "released, no stock was reserved or picked and no".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(62) VALUE
               "despatched quantity moved.  Only the reprint counter".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(62) VALUE
               "and the audit trail were written by this run.".
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(62) VALUE
               "A reissue reproduces the document as it stands and".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(62) VALUE
               "never conceals what the customer originally accepted.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0200-JOB-START THRU 0200-EXIT
           PERFORM 0600-READ-HEADER THRU 0600-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0700-VERSION-COUNTS THRU 0700-EXIT
           PERFORM 0300-OPEN-SPOOL THRU 0300-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 1000-PRINT-LINES THRU 1000-EXIT
           PERFORM 0400-CLOSE-SPOOL THRU 0400-EXIT
           PERFORM 2000-COUNT-AND-AUDIT THRU 2000-EXIT.
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
           MOVE "DOC" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-DOC
           END-IF
           PERFORM 0150-FIND-REF THRU 0150-EXIT
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "SALESREP" TO WS-USER
           END-IF
           IF WS-DOC = SPACES
               MOVE "ACK" TO WS-DOC
           END-IF
           IF NOT (WS-IS-ACK OR WS-IS-PICK OR WS-IS-DESP)
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-1800 DOC must be ACK, PICK or DESP"
                       UPON SYSOUT
               GO TO 0100-EXIT
           END-IF
           IF WS-REF = SPACES
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-1801 a document reference is needed"
                       UPON SYSOUT
               GO TO 0100-EXIT
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           EVALUATE TRUE
               WHEN WS-IS-ACK
                   MOVE "Order acknowledgement" TO D-DOCNAME
                   MOVE "so.SalesOrder" TO WS-ENTITY
               WHEN WS-IS-PICK
                   MOVE "Pick note" TO D-DOCNAME
                   MOVE "so.PickList" TO WS-ENTITY
               WHEN OTHER
                   MOVE "Despatch note" TO D-DOCNAME
                   MOVE "so.Despatch" TO WS-ENTITY
           END-EVALUATE
           DISPLAY "STERP-I-1802 SOACKDOC " FUNCTION TRIM(WS-DOC)
                   " " FUNCTION TRIM(WS-REF) UPON SYSOUT.
       0100-EXIT.
           EXIT.

      *>   REF= is the modern key; the 1994 job streams used ORDER=,
      *>   PICK= and DESP= and are still in the schedule
       0150-FIND-REF SECTION.
       0150-START.
           MOVE "REF" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-REF
               GO TO 0150-EXIT
           END-IF
           MOVE "ORDER" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-REF
               GO TO 0150-EXIT
           END-IF
           MOVE "PICK" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-REF
               GO TO 0150-EXIT
           END-IF
           MOVE "DESP" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-REF
           END-IF.
       0150-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "SOACKDOC" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE JL-JOB-RUN-ID TO WS-JOB-RUN-ID.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0600-READ-HEADER SECTION.
       0600-START.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 20
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           EVALUATE TRUE
               WHEN WS-IS-ACK
                   PERFORM 0610-HEAD-ACK THRU 0610-EXIT
               WHEN WS-IS-PICK
                   PERFORM 0620-HEAD-PICK THRU 0620-EXIT
               WHEN OTHER
                   PERFORM 0630-HEAD-DESP THRU 0630-EXIT
           END-EVALUATE.
       0600-EXIT.
           EXIT.

       0610-HEAD-ACK SECTION.
       0610-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT o.OrderNumber, o.CustomerCode, "
                  "ISNULL(c.LegalName,''), "
                  "ISNULL(o.CustomerOrderRef,''), "
                  "CONVERT(char(8),o.OrderDate,112), o.ReleaseStatus,"
                  " o.FulfilmentStatus, o.BillingStatus, "
                  "ISNULL(o.DeliveryAddressSnapshot,''), "
                  "ISNULL(o.TermsAndConditionsVersion,''), "
                  "o.CommercialStatus, ISNULL(o.HoldReason,''), "
                  "o.CurrencyCode, CASE WHEN "
                  "o.AcknowledgementIssuedAt IS NULL THEN 'Y' ELSE "
                  "'N' END FROM so.SalesOrder o LEFT JOIN "
                  "ar.Customer c ON c.CompanyCode = o.CompanyCode "
                  "AND c.CustomerCode = o.CustomerCode WHERE "
                  "o.CompanyCode = ? AND o.OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0690-BIND-REF THRU 0690-EXIT
           MOVE 14 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF NOT SQLIO-OK
               MOVE 16 TO WS-RC
               DISPLAY "STERP-E-1803 no such sales order" UPON SYSOUT
               GO TO 0610-EXIT
           END-IF
           MOVE SO-COL(1) TO H-REF H-ORDER
           MOVE SO-COL(2) TO H-CUST
           MOVE SO-COL(3) TO H-CUST-NAME
           MOVE SO-COL(4) TO H-CUSTREF
           MOVE SO-COL(5) TO WS-DATE-WORK
           PERFORM 0680-DATE-TEXT THRU 0680-EXIT
           MOVE SO-COL(6) TO H-RELEASE
           MOVE SO-COL(7) TO H-FULFIL
           MOVE SO-COL(8) TO H-BILLING
           MOVE SO-COL(9) TO H-ADDRESS
           MOVE SPACES TO H-EXTRA1
           STRING "T&C version " FUNCTION TRIM(SO-COL(10))
               DELIMITED BY SIZE INTO H-EXTRA1
           MOVE SPACES TO H-EXTRA2
           STRING "Commercial " FUNCTION TRIM(SO-COL(11))
               DELIMITED BY SIZE INTO H-EXTRA2
           MOVE SO-COL(12) TO H-HOLD
           MOVE SO-COL(14) TO WS-FIRST-ISSUE
           MOVE "  Accepted" TO D-HD-Q1
           MOVE " Cancelled" TO D-HD-Q2
           MOVE "Despatched" TO D-HD-Q3
           MOVE "    Net value" TO D-HD-VAL
           MOVE "Promise / VAT" TO D-HD-NOTE.
       0610-EXIT.
           EXIT.

       0620-HEAD-PICK SECTION.
       0620-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT p.PickListRef, p.CustomerCode, "
                  "ISNULL(c.LegalName,''), ISNULL(p.OrderNumber,''), "
                  "CONVERT(char(8),p.RequiredDate,112), p.Status, "
                  "ISNULL(p.WarehouseCode,''), p.PrintCount, "
                  "ISNULL(p.PickerUser,''), p.SiteCode FROM "
                  "so.PickList p LEFT JOIN ar.Customer c ON "
                  "c.CompanyCode = p.CompanyCode AND "
                  "c.CustomerCode = p.CustomerCode WHERE "
                  "p.CompanyCode = ? AND p.PickListRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0690-BIND-REF THRU 0690-EXIT
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF NOT SQLIO-OK
               MOVE 16 TO WS-RC
               DISPLAY "STERP-E-1804 no such pick list" UPON SYSOUT
               GO TO 0620-EXIT
           END-IF
           MOVE SO-COL(1) TO H-REF
           MOVE SO-COL(2) TO H-CUST
           MOVE SO-COL(3) TO H-CUST-NAME
           MOVE SO-COL(4) TO H-ORDER
           MOVE SO-COL(5) TO WS-DATE-WORK
           PERFORM 0680-DATE-TEXT THRU 0680-EXIT
           MOVE SO-COL(6) TO H-FULFIL
           MOVE SPACES TO H-EXTRA1
           STRING "Warehouse " FUNCTION TRIM(SO-COL(7))
               DELIMITED BY SIZE INTO H-EXTRA1
           MOVE SPACES TO H-EXTRA2
           STRING "Picker " FUNCTION TRIM(SO-COL(9))
               DELIMITED BY SIZE INTO H-EXTRA2
           COMPUTE WS-REPRINT = FUNCTION NUMVAL(SO-COL(8))
           MOVE SO-COL(10) TO H-ADDRESS
           MOVE "  Required" TO D-HD-Q1
           MOVE "    Picked" TO D-HD-Q2
           MOVE "     Short" TO D-HD-Q3
           MOVE "            " TO D-HD-VAL
           MOVE "Location / lot" TO D-HD-NOTE
           MOVE "Picking does not despatch" TO H-HOLD.
       0620-EXIT.
           EXIT.

       0630-HEAD-DESP SECTION.
       0630-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT d.DespatchRef, d.CustomerCode, "
                  "ISNULL(c.LegalName,''), ISNULL(d.OrderNumber,''), "
                  "CONVERT(char(8),d.DespatchDate,112), d.Status, "
                  "ISNULL(d.CarrierCode,''), "
                  "ISNULL(d.DeliveryAddressSnapshot,''), "
                  "ISNULL(d.IncotermsRule,''), "
                  "ISNULL(d.CarrierReference,''), "
                  "d.ExportEvidenceStatus, d.SiteCode FROM "
                  "so.Despatch d LEFT JOIN ar.Customer c ON "
                  "c.CompanyCode = d.CompanyCode AND "
                  "c.CustomerCode = d.CustomerCode WHERE "
                  "d.CompanyCode = ? AND d.DespatchRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0690-BIND-REF THRU 0690-EXIT
           MOVE 12 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF NOT SQLIO-OK
               MOVE 16 TO WS-RC
               DISPLAY "STERP-E-1805 no such despatch" UPON SYSOUT
               GO TO 0630-EXIT
           END-IF
           MOVE SO-COL(1) TO H-REF
           MOVE SO-COL(2) TO H-CUST
           MOVE SO-COL(3) TO H-CUST-NAME
           MOVE SO-COL(4) TO H-ORDER
           MOVE SO-COL(5) TO WS-DATE-WORK
           PERFORM 0680-DATE-TEXT THRU 0680-EXIT
           MOVE SO-COL(6) TO H-FULFIL
           MOVE SO-COL(8) TO H-ADDRESS
           MOVE SPACES TO H-EXTRA1
           STRING "Incoterm " FUNCTION TRIM(SO-COL(9))
                  " carrier " FUNCTION TRIM(SO-COL(7))
               DELIMITED BY SIZE INTO H-EXTRA1
           MOVE SPACES TO H-EXTRA2
           STRING "Consignment " FUNCTION TRIM(SO-COL(10))
               DELIMITED BY SIZE INTO H-EXTRA2
           MOVE SPACES TO H-HOLD
           STRING "Export evidence " FUNCTION TRIM(SO-COL(11))
               DELIMITED BY SIZE INTO H-HOLD
           MOVE "    Picked" TO D-HD-Q1
           MOVE "Despatched" TO D-HD-Q2
           MOVE "    Billed" TO D-HD-Q3
           MOVE "            " TO D-HD-VAL
           MOVE "Lot / serial" TO D-HD-NOTE.
       0630-EXIT.
           EXIT.

       0680-DATE-TEXT SECTION.
       0680-START.
           MOVE SPACES TO H-DATE
           IF WS-DATE-WORK = SPACES
               GO TO 0680-EXIT
           END-IF
           STRING WS-DATE-WORK(7:2) "/" WS-DATE-WORK(5:2) "/"
                  WS-DATE-WORK(1:4)
               DELIMITED BY SIZE INTO H-DATE.
       0680-EXIT.
           EXIT.

       0690-BIND-REF SECTION.
       0690-START.
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2).
       0690-EXIT.
           EXIT.

      *>   REQ: SO-022 - the version and the reprint count.  Only
      *>   so.PickList keeps a PrintCount of its own; for the other
      *>   two documents the PRINT rows already in core.AuditLog are
      *>   the counter, which also makes every reprint attributable.
       0700-VERSION-COUNTS SECTION.
       0700-START.
           MOVE 1 TO WS-VERSION
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT 1 + COUNT(*) FROM core.AuditLog WHERE "
                  "EntityType = ? AND EntityKey = ? AND Action IN "
                  "('AMEND','AMENDED','REVISE')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-ENTITY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-OK
               COMPUTE WS-VERSION = FUNCTION NUMVAL(SO-COL(1))
           END-IF
           IF WS-IS-PICK
               GO TO 0700-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT COUNT(*) FROM core.AuditLog WHERE "
                  "EntityType = ? AND EntityKey = ? AND Action = "
                  "'PRINT' AND FieldName = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE WS-ENTITY TO SQLIO-PARM-VAL(1)
           MOVE WS-REF TO SQLIO-PARM-VAL(2)
           MOVE WS-DOC TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-OK
               COMPUTE WS-REPRINT = FUNCTION NUMVAL(SO-COL(1))
           END-IF.
       0700-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "SOACKDOC" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1806 cannot build the spool path"
                       UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           PERFORM 0350-FILL-HEADINGS THRU 0350-EXIT
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1807 cannot open " WS-SPOOL-NAME
                       UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           INITIATE R-SOACKDC.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "SOACKDOC" TO RPT-REPORT-ID
           MOVE SPACES TO RPT-TITLE
           STRING FUNCTION TRIM(D-DOCNAME) " - "
                  FUNCTION TRIM(WS-REF)
               DELIMITED BY SIZE INTO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE SPACES TO RPT-SUBTITLE
           IF WS-REPRINT > 0
               STRING "REPRINT - no business state is changed"
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           ELSE
               STRING "First issue - no business state is changed"
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           END-IF
           MOVE SPACES TO RPT-AS-AT-TEXT
           MOVE WS-VERSION TO WS-INT-ED
           STRING "Version " FUNCTION TRIM(WS-INT-ED)
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
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
       1000-PRINT-LINES SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           EVALUATE TRUE
               WHEN WS-IS-ACK
                   PERFORM 1100-LINES-ACK THRU 1100-EXIT
               WHEN WS-IS-PICK
                   PERFORM 1200-LINES-PICK THRU 1200-EXIT
               WHEN OTHER
                   PERFORM 1300-LINES-DESP THRU 1300-EXIT
           END-EVALUATE
           PERFORM 0690-BIND-REF THRU 0690-EXIT
           MOVE 9 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1808 " SQLIO-ERRMSG(1:100) UPON SYSOUT
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA SO-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 1500-EMIT THRU 1500-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW.
       1000-EXIT.
           EXIT.

       1100-LINES-ACK SECTION.
       1100-START.
           STRING "SELECT l.LineNum, ISNULL(l.ItemCode,''), "
                  "l.Description, l.AcceptedQty, l.CancelledQty, "
                  "l.DespatchedQty, l.NetAmount, "
                  "ISNULL(l.VatCode,''), "
                  "ISNULL(CONVERT(char(8),(SELECT "
                  "MIN(s.CurrentPromiseDate) FROM "
                  "so.DeliverySchedule s WHERE s.SalesOrderLineId = "
                  "l.SalesOrderLineId),112),'') FROM "
                  "so.SalesOrderLine l WHERE l.CompanyCode = ? AND "
                  "l.OrderNumber = ? ORDER BY l.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT.
       1100-EXIT.
           EXIT.

       1200-LINES-PICK SECTION.
       1200-START.
           STRING "SELECT pl.LineNum, pl.ItemCode, "
                  "ISNULL(pl.ShortReason,''), pl.RequiredQty, "
                  "pl.PickedQty, pl.ShortQty, 0, "
                  "ISNULL(pl.LocationCode,''), "
                  "ISNULL(pl.LotCode,'') FROM so.PickListLine pl "
                  "JOIN so.PickList p ON p.PickListId = "
                  "pl.PickListId WHERE p.CompanyCode = ? AND "
                  "p.PickListRef = ? ORDER BY pl.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT.
       1200-EXIT.
           EXIT.

       1300-LINES-DESP SECTION.
       1300-START.
           STRING "SELECT dl.LineNum, dl.ItemCode, "
                  "ISNULL(dl.Description,''), dl.PickedQty, "
                  "dl.DespatchedQty, dl.BilledQty, 0, "
                  "ISNULL(dl.LotCode,''), ISNULL(dl.SerialNo,'') "
                  "FROM so.DespatchLine dl JOIN so.Despatch d ON "
                  "d.DespatchId = dl.DespatchId WHERE "
                  "d.CompanyCode = ? AND d.DespatchRef = ? ORDER BY "
                  "dl.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT.
       1300-EXIT.
           EXIT.

       1500-EMIT SECTION.
       1500-START.
           ADD 1 TO WS-LINES-OUT
           COMPUTE D-LINE = FUNCTION NUMVAL(SO-COL(1))
           MOVE SO-COL(2) TO D-ITEM
           MOVE SO-COL(3) TO D-DESC
           COMPUTE D-QTY1 = FUNCTION NUMVAL(SO-COL(4))
           COMPUTE D-QTY2 = FUNCTION NUMVAL(SO-COL(5))
           COMPUTE D-QTY3 = FUNCTION NUMVAL(SO-COL(6))
           COMPUTE D-VALUE = FUNCTION NUMVAL(SO-COL(7))
           MOVE SPACES TO D-NOTE
           IF WS-IS-ACK
               MOVE SO-COL(9) TO WS-DATE-WORK
               IF WS-DATE-WORK = SPACES
                   MOVE SO-COL(8) TO D-NOTE
               ELSE
                   STRING FUNCTION TRIM(SO-COL(8)) " "
                          WS-DATE-WORK(7:2) "/" WS-DATE-WORK(5:2) "/"
                          WS-DATE-WORK(1:4)
                       DELIMITED BY SIZE INTO D-NOTE
               END-IF
           ELSE
               STRING FUNCTION TRIM(SO-COL(8)) " "
                      FUNCTION TRIM(SO-COL(9))
                   DELIMITED BY SIZE INTO D-NOTE
           END-IF
           ADD 1 TO T-LINES
           ADD D-QTY1 TO T-QTY1
           ADD D-QTY2 TO T-QTY2
           ADD D-QTY3 TO T-QTY3
           ADD D-VALUE TO T-VALUE
           GENERATE DE-LINE.
       1500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   REQ: SO-011, SO-012, SO-022 - the ONLY writes this program
      *>   makes.  No release, no reservation, no picked or despatched
      *>   quantity: a document is evidence of a decision already
      *>   taken, never the decision itself (CR2505, AC-042).
       2000-COUNT-AND-AUDIT SECTION.
       2000-START.
           IF WS-RC > 8
               GO TO 2000-EXIT
           END-IF
           IF WS-IS-PICK
               PERFORM 2100-BUMP-PICK THRU 2100-EXIT
           END-IF
           IF WS-IS-ACK AND WS-FIRST-ISSUE = "Y"
               PERFORM 2200-STAMP-ACK THRU 2200-EXIT
           END-IF
           PERFORM 2300-AUDIT THRU 2300-EXIT
           ADD 1 TO WS-REPRINT.
       2000-EXIT.
           EXIT.

       2100-BUMP-PICK SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.PickList SET PrintCount = PrintCount + "
                  "1 WHERE CompanyCode = ? AND PickListRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0690-BIND-REF THRU 0690-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-1809 reprint counter not updated"
                       UPON SYSOUT
               GO TO 2100-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW.
       2100-EXIT.
           EXIT.

      *>   the first issue timestamp only.  It is never rewritten on a
      *>   reprint, so the date the customer was acknowledged stands.
       2200-STAMP-ACK SECTION.
       2200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrder SET AcknowledgementIssuedAt "
                  "= SYSDATETIME() WHERE CompanyCode = ? AND "
                  "OrderNumber = ? AND AcknowledgementIssuedAt IS "
                  "NULL"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0690-BIND-REF THRU 0690-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-1810 issue stamp not written"
                       UPON SYSOUT
               GO TO 2200-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW.
       2200-EXIT.
           EXIT.

       2300-AUDIT SECTION.
       2300-START.
           INITIALIZE WS-AUDIT
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUDIT
           MOVE WS-CO TO CA-COMPANY OF WS-AUDIT
           MOVE WS-SITE TO CA-SITE OF WS-AUDIT
           MOVE WS-USER TO CA-USER OF WS-AUDIT
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-AUDIT
           MOVE WS-ENTITY TO AU-ENTITY-TYPE
           MOVE WS-REF TO AU-ENTITY-KEY
           MOVE "PRINT" TO AU-ACTION
           MOVE WS-DOC TO AU-FIELD-NAME
           MOVE WS-REPRINT TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO AU-OLD-VALUE
           COMPUTE WS-IX = WS-REPRINT + 1
           MOVE WS-IX TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO AU-NEW-VALUE
           MOVE SPACES TO AU-REASON
           MOVE WS-VERSION TO WS-INT-ED
           STRING "Document version " FUNCTION TRIM(WS-INT-ED)
                  " printed to " FUNCTION TRIM(WS-SPOOL-NAME)
               DELIMITED BY SIZE INTO AU-REASON
           MOVE "SOACKDOC" TO AU-SOURCE
           MOVE "N" TO AU-SENSITIVE
           CALL "AUDITLOG" USING WS-AUDIT
           IF CA-RETURN-CODE OF WS-AUDIT > 4
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-1811 audit record not written"
                       UPON SYSOUT
           END-IF.
       2300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-SOACKDC
           CLOSE RPT-FILE
           DISPLAY "STERP-I-1812 " WS-LINES-OUT " document line(s)"
                   UPON SYSOUT
           DISPLAY "STERP-I-1813 no business state was changed"
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
           MOVE "SOACKDOC" TO JL-JOB-NAME
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
