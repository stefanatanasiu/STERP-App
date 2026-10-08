      *>================================================================
      *> PROGRAM-ID : SOBACKLG
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT
      *> PURPOSE    : SALES BACKLOG AND FULFILMENT REPORT, REPORT CODE
      *>              RPT-008.  EVERY OPEN ORDER LINE AND DELIVERY
      *>              SCHEDULE IN SCOPE WITH ITS ACCEPTED, CANCELLED,
      *>              DESPATCHED, BILLED AND REMAINING QUANTITIES, THE
      *>              VALUE STILL OUTSTANDING AT THE CAPTURED PRICE,
      *>              THE RELEASE STATUS AND THE HOLD REASON.
      *>              THE REQUESTED DATE AND THE ORIGINAL PROMISED
      *>              DATE ARE PRINTED BESIDE THE CURRENT PROMISE AND
      *>              LATENESS IS MEASURED AGAINST THE ORIGINAL
      *>              PROMISE, SO THAT A LATE DELIVERY MEASURE CANNOT
      *>              BE IMPROVED BY EDITING THE CURRENT PROMISE.
      *>              CONTROL TOTALS ARE TAKEN BY PROMISE DATE BUCKET
      *>              (OVERDUE, THIS WEEK, NEXT WEEK, LATER) AND IN
      *>              GRAND TOTAL.
      *> RUN AS     : SOBACKLG CO=STC [SITE=RDG] [CUST=C0001]
      *>              [FROM=yyyymmdd] [TO=yyyymmdd] [ASAT=yyyymmdd]
      *>              USER=SALESMGR
      *> OUTPUT     : run\spool\SOBACKLG-<CO>-<stamp>.lst  (132 col)
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, DATECALC, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1995-01-23 RJH  CR0362 ORIGINAL - ORDER BOOK LISTING
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS THROUGHOUT
      *> 2003-09-08 AMD  CR1455 ORIGINAL PROMISE PRINTED AFTER THE
      *>                 CUSTOMER SERVICE REVIEW FOUND THE CURRENT
      *>                 PROMISE BEING EDITED TO CLEAR THE LATE LIST
      *> 2026-10-06 STERP SO-015, SO-008, SO-023, RPT-008
      *>================================================================
      *> REQ: SO-015, SO-008, SO-023, RPT-008
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOBACKLG.
       AUTHOR. R J HOLLIS.
       DATE-WRITTEN. 23/01/1995.
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
       FD  RPT-FILE REPORT IS R-SOBACKL.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY STDATES.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATE==.
       01  SO-ROW.
           05  SO-COL OCCURS 30 TIMES PIC X(100).
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-SITE                 PIC X(10) VALUE SPACES.
           05  WS-CUST                 PIC X(20) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-FROM                 PIC X(8) VALUE SPACES.
           05  WS-TO                   PIC X(8) VALUE SPACES.
           05  WS-ASAT                 PIC X(8) VALUE SPACES.
       01  WS-SCRATCH.
           05  WS-IX                   PIC S9(4) VALUE 0.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-TODAY-N              PIC 9(8) VALUE 0.
           05  WS-WEEK1                PIC X(8) VALUE SPACES.
           05  WS-WEEK2                PIC X(8) VALUE SPACES.
           05  WS-DONE                 PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-JOB-RUN-ID           PIC S9(9) COMP-5 VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-WORK-DATE            PIC 9(8) VALUE 0.
       01  WS-DET.
           05  D-BUCKET                PIC X(12) VALUE SPACES.
           05  D-ORDER                 PIC X(14) VALUE SPACES.
           05  D-LINE                  PIC 9(4) VALUE 0.
           05  D-SCHED                 PIC 9(3) VALUE 0.
           05  D-CUST                  PIC X(12) VALUE SPACES.
           05  D-ITEM                  PIC X(18) VALUE SPACES.
           05  D-ACCEPTED              PIC S9(11)V9(3) VALUE 0.
           05  D-CANCELLED             PIC S9(11)V9(3) VALUE 0.
           05  D-DESPATCHED            PIC S9(11)V9(3) VALUE 0.
           05  D-BILLED                PIC S9(11)V9(3) VALUE 0.
           05  D-REMAINING             PIC S9(11)V9(3) VALUE 0.
           05  D-VALUE                 PIC S9(11)V99 VALUE 0.
           05  D-RELEASE               PIC X(12) VALUE SPACES.
           05  D-REQUESTED             PIC X(10) VALUE SPACES.
           05  D-ORIG-PROM             PIC X(10) VALUE SPACES.
           05  D-CURR-PROM             PIC X(10) VALUE SPACES.
           05  D-DAYS-LATE             PIC S9(5) VALUE 0.
           05  D-CHANGES               PIC 9(3) VALUE 0.
           05  D-HOLD                  PIC X(54) VALUE SPACES.
       01  WS-TOT.
           05  T-LINES                 PIC S9(7) VALUE 0.
           05  T-REMAINING             PIC S9(13)V9(3) VALUE 0.
           05  T-VALUE                 PIC S9(13)V99 VALUE 0.
           05  T-LATE-LINES            PIC S9(7) VALUE 0.
           05  T-LATE-VALUE            PIC S9(13)V99 VALUE 0.
           05  T-HELD-LINES            PIC S9(7) VALUE 0.
       01  WS-DATE-ED.
           05  WS-ED-DD                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-ED-MM                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-ED-YYYY              PIC 9(4) VALUE 0.
       REPORT SECTION.
       RD  R-SOBACKL
           CONTROLS ARE FINAL D-BUCKET
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 54
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-BUCKET TYPE IS CONTROL HEADING D-BUCKET.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(21)
                               VALUE "Promise date bucket: ".
               10  COLUMN 22   PIC X(12) SOURCE D-BUCKET.
               10  COLUMN 40   PIC X(60) VALUE
                 "Lateness is measured against the ORIGINAL promise".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(5) VALUE "Order".
               10  COLUMN 16   PIC X(4) VALUE "Line".
               10  COLUMN 21   PIC X(3) VALUE "Sch".
               10  COLUMN 25   PIC X(8) VALUE "Customer".
               10  COLUMN 38   PIC X(4) VALUE "Item".
               10  COLUMN 57   PIC X(9) VALUE " Accepted".
               10  COLUMN 67   PIC X(9) VALUE "Cancelled".
               10  COLUMN 77   PIC X(9) VALUE "Despatchd".
               10  COLUMN 87   PIC X(9) VALUE "   Billed".
               10  COLUMN 97   PIC X(9) VALUE "Remaining".
               10  COLUMN 107  PIC X(13) VALUE " Value outstg".
               10  COLUMN 121  PIC X(12) VALUE "Release".
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(9) VALUE "Requested".
               10  COLUMN 17   PIC X(15) VALUE "Original promis".
               10  COLUMN 33   PIC X(14) VALUE "Current promis".
               10  COLUMN 48   PIC X(9) VALUE "Days late".
               10  COLUMN 58   PIC X(7) VALUE "Changes".
               10  COLUMN 66   PIC X(11) VALUE "Hold reason".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(14) SOURCE D-ORDER.
               10  COLUMN 16   PIC ZZZ9 SOURCE D-LINE.
               10  COLUMN 21   PIC ZZ9 SOURCE D-SCHED.
               10  COLUMN 25   PIC X(12) SOURCE D-CUST.
               10  COLUMN 38   PIC X(18) SOURCE D-ITEM.
               10  COLUMN 57   PIC Z(5)9.999 SOURCE D-ACCEPTED.
               10  COLUMN 67   PIC Z(5)9.999 SOURCE D-CANCELLED.
               10  COLUMN 77   PIC Z(5)9.999 SOURCE D-DESPATCHED.
               10  COLUMN 87   PIC Z(5)9.999 SOURCE D-BILLED.
               10  COLUMN 97   PIC Z(5)9.999 SOURCE D-REMAINING.
               10  COLUMN 107  PIC Z(9)9.99- SOURCE D-VALUE.
               10  COLUMN 121  PIC X(12) SOURCE D-RELEASE.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(10) SOURCE D-REQUESTED.
               10  COLUMN 17   PIC X(10) SOURCE D-ORIG-PROM.
               10  COLUMN 33   PIC X(10) SOURCE D-CURR-PROM.
               10  COLUMN 48   PIC ZZZZ9- SOURCE D-DAYS-LATE.
               10  COLUMN 59   PIC ZZ9 SOURCE D-CHANGES.
               10  COLUMN 66   PIC X(54) SOURCE D-HOLD.
       01  CF-BUCKET TYPE IS CONTROL FOOTING D-BUCKET.
           05  LINE PLUS 1.
               10  COLUMN 25   PIC X(14) VALUE "Bucket totals".
               10  COLUMN 97   PIC Z(5)9.999 SUM D-REMAINING.
               10  COLUMN 107  PIC Z(9)9.99- SUM D-VALUE.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(26) VALUE "Open schedules".
               10  COLUMN 28   PIC Z(6)9 SOURCE T-LINES.
               10  COLUMN 40   PIC X(20) VALUE "quantity remaining".
               10  COLUMN 61   PIC Z(11)9.999 SOURCE T-REMAINING.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(26) VALUE "Value outstanding".
               10  COLUMN 28   PIC Z(11)9.99- SOURCE T-VALUE.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(26)
                               VALUE "Late on ORIGINAL promise".
               10  COLUMN 28   PIC Z(6)9 SOURCE T-LATE-LINES.
               10  COLUMN 40   PIC X(20) VALUE "value at risk".
               10  COLUMN 61   PIC Z(11)9.99- SOURCE T-LATE-VALUE.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(26) VALUE "Held, not released".
               10  COLUMN 28   PIC Z(6)9 SOURCE T-HELD-LINES.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(62) VALUE
               "Remaining = accepted less cancelled less despatched".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(62) VALUE
               "promise does not change the original service measure.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0200-JOB-START THRU 0200-EXIT
           PERFORM 0250-WEEK-BOUNDS THRU 0250-EXIT
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
           MOVE CM-FROM TO WS-FROM
           MOVE CM-TO TO WS-TO
           MOVE CM-ASAT TO WS-ASAT
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
               MOVE "SALESMGR" TO WS-USER
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           IF WS-ASAT = SPACES
               MOVE WS-TODAY-N TO WS-ASAT
           END-IF
           DISPLAY "STERP-I-1600 SOBACKLG sales backlog for "
                   FUNCTION TRIM(WS-CO) UPON SYSOUT.
       0100-EXIT.
           EXIT.

      *>   the two week boundaries used by the promise date buckets
       0250-WEEK-BOUNDS SECTION.
       0250-START.
           COMPUTE WS-WORK-DATE = FUNCTION NUMVAL(WS-ASAT)
           INITIALIZE WS-DATE
           MOVE "ADDDAYS" TO CA-FUNCTION OF WS-DATE
           MOVE WS-CO TO CA-COMPANY OF WS-DATE
           MOVE WS-USER TO CA-USER OF WS-DATE
           MOVE WS-WORK-DATE TO DC-DATE-1
           MOVE 7 TO DC-NUMBER
           CALL "DATECALC" USING WS-DATE
           IF CA-RETURN-CODE OF WS-DATE > 4
               MOVE WS-ASAT TO WS-WEEK1
           ELSE
               MOVE DC-RESULT-DATE TO WS-WEEK1
           END-IF
           INITIALIZE WS-DATE
           MOVE "ADDDAYS" TO CA-FUNCTION OF WS-DATE
           MOVE WS-CO TO CA-COMPANY OF WS-DATE
           MOVE WS-USER TO CA-USER OF WS-DATE
           MOVE WS-WORK-DATE TO DC-DATE-1
           MOVE 14 TO DC-NUMBER
           CALL "DATECALC" USING WS-DATE
           IF CA-RETURN-CODE OF WS-DATE > 4
               MOVE WS-WEEK1 TO WS-WEEK2
           ELSE
               MOVE DC-RESULT-DATE TO WS-WEEK2
           END-IF.
       0250-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "SOBACKLG" TO JL-JOB-NAME
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
           MOVE "SOBACKLG" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1601 cannot build the spool path"
                       UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           PERFORM 0350-FILL-HEADINGS THRU 0350-EXIT
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1602 cannot open " WS-SPOOL-NAME
                       UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           INITIATE R-SOBACKL.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "RPT-008" TO RPT-REPORT-ID
           MOVE "Sales backlog and fulfilment (RPT-008)"
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
      *>   REQ: SO-008, SO-015 - one row per open delivery schedule.
      *>   The bucket is produced by the database so that the report
      *>   can be ordered by it and the control break works; lateness
      *>   is always DATEDIFF from OriginalPromiseDate.
       1000-PRODUCE SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CASE WHEN "
                  "CONVERT(char(8),s.CurrentPromiseDate,112) < ? "
                  "THEN '1 Overdue' WHEN "
                  "CONVERT(char(8),s.CurrentPromiseDate,112) <= ? "
                  "THEN '2 This week' WHEN "
                  "CONVERT(char(8),s.CurrentPromiseDate,112) <= ? "
                  "THEN '3 Next week' ELSE '4 Later' END, "
                  "l.OrderNumber, l.LineNum, s.ScheduleNo, "
                  "o.CustomerCode, ISNULL(l.ItemCode,''), "
                  "l.AcceptedQty, l.CancelledQty, l.DespatchedQty, "
                  "l.BilledQty, s.Quantity, s.DespatchedQty, "
                  "l.UnitPrice, l.PricePerQty, l.DiscountPct1, "
                  "l.DiscountPct2, "
                  "CONVERT(char(8),s.RequestedDate,112), "
                  "CONVERT(char(8),s.OriginalPromiseDate,112), "
                  "CONVERT(char(8),s.CurrentPromiseDate,112), "
                  "DATEDIFF(day, s.OriginalPromiseDate, "
                  "CONVERT(date, ?)), s.ChangeCount, o.ReleaseStatus,"
                  " ISNULL(o.HoldReason,''), s.Status, l.Status, "
                  "ISNULL(l.Description,''), o.CurrencyCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           STRING FUNCTION TRIM(SQLIO-STMT) " "
                  "FROM so.DeliverySchedule s JOIN so.SalesOrderLine "
                  "l ON l.SalesOrderLineId = s.SalesOrderLineId "
                  "JOIN so.SalesOrder o ON "
                  "o.SalesOrderId = l.SalesOrderId WHERE "
                  "o.CompanyCode = ? AND l.Status = 'OPEN' AND "
                  "s.Status IN ('OPEN','PARTIAL') AND "
                  "o.CommercialStatus IN ('ACCEPTED','AMENDED') AND "
                  "(? = '' OR ISNULL(s.SiteCode, o.SiteCode) = ?) "
                  "AND (? = '' OR o.CustomerCode = ?) AND "
                  "(? = '' OR CONVERT(char(8),o.OrderDate,112) >= ?) "
                  "AND (? = '' OR CONVERT(char(8),o.OrderDate,112) "
                  "<= ?) ORDER BY 1, o.CustomerCode, l.OrderNumber, "
                  "l.LineNum, s.ScheduleNo"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-ASAT TO SQLIO-PARM-VAL(1)
           MOVE WS-WEEK1 TO SQLIO-PARM-VAL(2)
           MOVE WS-WEEK2 TO SQLIO-PARM-VAL(3)
           MOVE WS-ASAT TO SQLIO-PARM-VAL(4)
           MOVE WS-CO TO SQLIO-PARM-VAL(5)
           MOVE WS-SITE TO SQLIO-PARM-VAL(6)
           MOVE WS-SITE TO SQLIO-PARM-VAL(7)
           MOVE WS-CUST TO SQLIO-PARM-VAL(8)
           MOVE WS-CUST TO SQLIO-PARM-VAL(9)
           MOVE WS-FROM TO SQLIO-PARM-VAL(10)
           MOVE WS-FROM TO SQLIO-PARM-VAL(11)
           MOVE WS-TO TO SQLIO-PARM-VAL(12)
           MOVE WS-TO TO SQLIO-PARM-VAL(13)
           MOVE 13 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 13
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 27 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1603 " SQLIO-ERRMSG(1:100) UPON SYSOUT
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA SO-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 1100-EMIT THRU 1100-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW.
       1000-EXIT.
           EXIT.

       1100-EMIT SECTION.
       1100-START.
           ADD 1 TO WS-ROWS-OUT
           MOVE SO-COL(1)(3:11) TO D-BUCKET
           MOVE SO-COL(2) TO D-ORDER
           COMPUTE D-LINE = FUNCTION NUMVAL(SO-COL(3))
           COMPUTE D-SCHED = FUNCTION NUMVAL(SO-COL(4))
           MOVE SO-COL(5) TO D-CUST
           IF SO-COL(6) = SPACES
               MOVE SO-COL(26) TO D-ITEM
           ELSE
               MOVE SO-COL(6) TO D-ITEM
           END-IF
           COMPUTE D-ACCEPTED = FUNCTION NUMVAL(SO-COL(7))
           COMPUTE D-CANCELLED = FUNCTION NUMVAL(SO-COL(8))
           COMPUTE D-DESPATCHED = FUNCTION NUMVAL(SO-COL(9))
           COMPUTE D-BILLED = FUNCTION NUMVAL(SO-COL(10))
           PERFORM 1200-REMAINING THRU 1200-EXIT
           PERFORM 1300-VALUE THRU 1300-EXIT
           PERFORM 1400-DATES THRU 1400-EXIT
           MOVE SO-COL(22) TO D-RELEASE
           MOVE SO-COL(23) TO D-HOLD
           IF D-RELEASE NOT = "RELEASED"
               ADD 1 TO T-HELD-LINES
               IF D-HOLD = SPACES
                   MOVE SPACES TO D-HOLD
                   STRING "Not released - " FUNCTION TRIM(D-RELEASE)
                       DELIMITED BY SIZE INTO D-HOLD
               END-IF
           END-IF
           ADD 1 TO T-LINES
           ADD D-REMAINING TO T-REMAINING
           ADD D-VALUE TO T-VALUE
           IF D-DAYS-LATE > 0
               ADD 1 TO T-LATE-LINES
               ADD D-VALUE TO T-LATE-VALUE
           END-IF
           GENERATE DE-LINE.
       1100-EXIT.
           EXIT.

      *>   REQ: SO-015 - remaining is what the schedule still owes the
      *>   customer.  Completing one schedule never closes the balance
      *>   of another, which is why the measure is per schedule and
      *>   the line totals are shown beside it, not instead of it.
       1200-REMAINING SECTION.
       1200-START.
           COMPUTE D-REMAINING = FUNCTION NUMVAL(SO-COL(11))
                               - FUNCTION NUMVAL(SO-COL(12))
           IF D-REMAINING < 0
               MOVE 0 TO D-REMAINING
           END-IF.
       1200-EXIT.
           EXIT.

      *>   value at the price captured on the order line - never a
      *>   current price list enquiry (SO-023)
       1300-VALUE SECTION.
       1300-START.
           COMPUTE D-VALUE ROUNDED = D-REMAINING
                 * FUNCTION NUMVAL(SO-COL(13))
                 / FUNCTION NUMVAL(SO-COL(14))
                 * (1 - FUNCTION NUMVAL(SO-COL(15)) / 100)
                 * (1 - FUNCTION NUMVAL(SO-COL(16)) / 100).
       1300-EXIT.
           EXIT.

       1400-DATES SECTION.
       1400-START.
           MOVE SO-COL(17)(1:8) TO WS-WORK-DATE
           PERFORM 1450-FORMAT-DATE THRU 1450-EXIT
           MOVE WS-DATE-ED TO D-REQUESTED
           MOVE SO-COL(18)(1:8) TO WS-WORK-DATE
           PERFORM 1450-FORMAT-DATE THRU 1450-EXIT
           MOVE WS-DATE-ED TO D-ORIG-PROM
           MOVE SO-COL(19)(1:8) TO WS-WORK-DATE
           PERFORM 1450-FORMAT-DATE THRU 1450-EXIT
           MOVE WS-DATE-ED TO D-CURR-PROM
           COMPUTE D-DAYS-LATE = FUNCTION NUMVAL(SO-COL(20))
           COMPUTE D-CHANGES = FUNCTION NUMVAL(SO-COL(21)).
       1400-EXIT.
           EXIT.

       1450-FORMAT-DATE SECTION.
       1450-START.
           MOVE WS-WORK-DATE TO STD-DATE-N
           MOVE STD-DD TO WS-ED-DD
           MOVE STD-MM TO WS-ED-MM
           MOVE STD-YYYY TO WS-ED-YYYY.
       1450-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-SOBACKL
           CLOSE RPT-FILE
           DISPLAY "STERP-I-1604 " WS-ROWS-OUT
                   " open schedule(s) reported" UPON SYSOUT
           IF T-LATE-LINES > 0
               DISPLAY "STERP-W-1605 " T-LATE-LINES
                       " schedule(s) late on the original promise"
                       UPON SYSOUT
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
           END-IF
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
           MOVE "SOBACKLG" TO JL-JOB-NAME
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
