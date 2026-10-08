      *>================================================================
      *> PROGRAM-ID : KPIOTIF
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : CROSS MODULE CONTROLS AND REPORTING (XMD)
      *> PURPOSE    : ON TIME IN FULL FOR CUSTOMERS AND SUPPLIERS ON
      *>              THE SINGLE PUBLISHED DEFINITION HELD IN
      *>              rpt.MetricDefinition.  CUSTOMER OTIF IS
      *>              MEASURED AGAINST THE ORIGINAL PROMISED DATE AND
      *>              THE FULL SCHEDULED QUANTITY USING THE DESPATCH
      *>              DATE; SUPPLIER OTIF AGAINST THE ORIGINAL
      *>              CONFIRMED DATE, WHERE A DELIVERY UP TO FIVE
      *>              WORKING DAYS EARLY STILL COUNTS AS ON TIME
      *>              (D-19).  A COHORT WITH NO ELIGIBLE OBLIGATION
      *>              IS REPORTED AS UNDEFINED - NEVER AS ZERO OR ONE
      *>              HUNDRED PER CENT.
      *> RUN AS     : KPIOTIF CO=STC FROM=20260401 TO=20261031
      *>              USER=FINCTRL [SITE=RDG] [DETAIL=Y]
      *> OUTPUT     : run\spool\KPIOTIF-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, DATECALC, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-04-02 RJH  CR0487 ORIGINAL SERVICE LEVEL STATISTICS
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS THROUGHOUT
      *> 2002-08-19 DJM  CR1455 FIVE WORKING DAY EARLY TOLERANCE FOR
      *>                 SUPPLIERS AFTER THE LOGISTICS REVIEW
      *> 2026-10-06 STERP XMD-010 ONE PUBLISHED MEASURE DEFINITION
      *>================================================================
      *> REQ: XMD-010, XMD-001, XMD-002, RPT-008, RPT-009, D-19
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KPIOTIF.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 02/04/1996.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO DYNAMIC XB-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS XB-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-KPIOTIF.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY XMDBATWK.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARMGET==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXCPRAIS==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATECALC==.
       01  WS-OPT.
           05  WS-DETAIL               PIC X VALUE "N".
           05  WS-EARLY-DAYS           PIC S9(4) VALUE 5.
           05  WS-PROMISE-N            PIC 9(8) VALUE 0.
           05  WS-ACTUAL-N             PIC 9(8) VALUE 0.
           05  WS-EARLIEST-N           PIC 9(8) VALUE 0.
       01  WS-MEAS.
           05  M-CODE                  PIC X(30) VALUE SPACES.
           05  M-NAME                  PIC X(40) VALUE SPACES.
           05  M-TEXT                  PIC X(110) VALUE SPACES.
       01  WS-DET.
           05  D-SIDE                  PIC X(10) VALUE SPACES.
           05  D-PARTY                 PIC X(20) VALUE SPACES.
           05  D-ORDER                 PIC X(20) VALUE SPACES.
           05  D-LINE                  PIC 9(4) VALUE 0.
           05  D-SCHED                 PIC 9(4) VALUE 0.
           05  D-PROMISE               PIC X(10) VALUE SPACES.
           05  D-ACTUAL                PIC X(10) VALUE SPACES.
           05  D-QTY                   PIC S9(11)V9(3) VALUE 0.
           05  D-DONE                  PIC S9(11)V9(3) VALUE 0.
           05  D-DAYS                  PIC S9(5) VALUE 0.
           05  D-RESULT                PIC X(12) VALUE SPACES.
           05  D-REASON                PIC X(28) VALUE SPACES.
       01  WS-COHORT.
           05  K-DUE                   PIC S9(7) VALUE 0.
           05  K-OTIF                  PIC S9(7) VALUE 0.
           05  K-LATE                  PIC S9(7) VALUE 0.
           05  K-SHORT                 PIC S9(7) VALUE 0.
           05  K-EARLY                 PIC S9(7) VALUE 0.
           05  K-OPEN                  PIC S9(7) VALUE 0.
       01  WS-SUMM.
           05  U-LABEL                 PIC X(40) VALUE SPACES.
           05  U-DUE                   PIC S9(7) VALUE 0.
           05  U-OTIF                  PIC S9(7) VALUE 0.
           05  U-LATE                  PIC S9(7) VALUE 0.
           05  U-SHORT                 PIC S9(7) VALUE 0.
           05  U-EARLY                 PIC S9(7) VALUE 0.
           05  U-OPEN                  PIC S9(7) VALUE 0.
           05  U-PCT                   PIC X(12) VALUE SPACES.
       01  WS-SCOPE.
           05  S-FROM                  PIC X(10) VALUE SPACES.
           05  S-TO                    PIC X(10) VALUE SPACES.
           05  S-BASIS                 PIC X(60) VALUE SPACES.
       REPORT SECTION.
       RD  R-KPIOTIF
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 8 LAST DETAIL 52
           FOOTING 58.
       COPY STRPTHDR.
       01  DE-SCOPE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(26) VALUE
                   "Obligations due from     ".
               10  COLUMN 27   PIC X(10) SOURCE S-FROM.
               10  COLUMN 39   PIC X(3) VALUE "to ".
               10  COLUMN 42   PIC X(10) SOURCE S-TO.
               10  COLUMN 56   PIC X(60) SOURCE S-BASIS.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE
                   "Cohort: eligible schedule obligations whose due".
               10  COLUMN 48   PIC X(62) VALUE
                   "date falls in the window. Zero cohort = undefined.".
       01  DE-MEAS TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(30) SOURCE M-CODE.
               10  COLUMN 34   PIC X(40) SOURCE M-NAME.
           05  LINE PLUS 1.
               10  COLUMN 6    PIC X(110) SOURCE M-TEXT.
       01  DE-HEAD TYPE IS DETAIL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(10) VALUE "Side".
               10  COLUMN 12   PIC X(5) VALUE "Party".
               10  COLUMN 33   PIC X(5) VALUE "Order".
               10  COLUMN 54   PIC X(4) VALUE "Line".
               10  COLUMN 60   PIC X(3) VALUE "Sch".
               10  COLUMN 66   PIC X(8) VALUE "Promised".
               10  COLUMN 78   PIC X(8) VALUE "Actual  ".
               10  COLUMN 90   PIC X(4) VALUE "Days".
               10  COLUMN 97   PIC X(9) VALUE "Scheduled".
               10  COLUMN 110  PIC X(9) VALUE "Delivered".
               10  COLUMN 122  PIC X(6) VALUE "Result".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-ROW TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(10) SOURCE D-SIDE.
               10  COLUMN 12   PIC X(20) SOURCE D-PARTY.
               10  COLUMN 33   PIC X(20) SOURCE D-ORDER.
               10  COLUMN 54   PIC ZZZ9 SOURCE D-LINE.
               10  COLUMN 60   PIC ZZZ9 SOURCE D-SCHED.
               10  COLUMN 66   PIC X(10) SOURCE D-PROMISE.
               10  COLUMN 78   PIC X(10) SOURCE D-ACTUAL.
               10  COLUMN 89   PIC -ZZZ9 SOURCE D-DAYS.
               10  COLUMN 96   PIC ZZZ,ZZ9.999 SOURCE D-QTY.
               10  COLUMN 109  PIC ZZZ,ZZ9.999 SOURCE D-DONE.
               10  COLUMN 122  PIC X(11) SOURCE D-RESULT.
       01  DE-SUMM TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(40) SOURCE U-LABEL.
               10  COLUMN 45   PIC X(9) VALUE "Eligible ".
               10  COLUMN 54   PIC ZZZ,ZZ9 SOURCE U-DUE.
               10  COLUMN 63   PIC X(6) VALUE "OTIF  ".
               10  COLUMN 69   PIC ZZZ,ZZ9 SOURCE U-OTIF.
               10  COLUMN 78   PIC X(5) VALUE "Late ".
               10  COLUMN 83   PIC ZZZ,ZZ9 SOURCE U-LATE.
               10  COLUMN 92   PIC X(6) VALUE "Short ".
               10  COLUMN 98   PIC ZZZ,ZZ9 SOURCE U-SHORT.
               10  COLUMN 107  PIC X(5) VALUE "Open ".
               10  COLUMN 112  PIC ZZZ,ZZ9 SOURCE U-OPEN.
               10  COLUMN 121  PIC X(12) SOURCE U-PCT.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(64) VALUE
                   "Measured on the original promise (customer) and".
               10  COLUMN 50   PIC X(62) VALUE
                   "original confirmation (supplier) per decision D-19."
               .
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           MOVE "KPIOTIF" TO RPT-REPORT-ID
           MOVE "On time in full - customer and supplier"
               TO RPT-TITLE
           MOVE "Shared measure definitions (XMD-010, D-19)"
               TO RPT-SUBTITLE
           PERFORM 9100-JOB-START THRU 9100-EXIT
           PERFORM 9200-OPEN-SPOOL THRU 9200-EXIT
           IF XB-RC > 4
               GO TO 0000-FINISH
           END-IF
           INITIATE R-KPIOTIF
           GENERATE DE-SCOPE
           PERFORM 0300-PUBLISH-MEASURES THRU 0300-EXIT
           GENERATE DE-HEAD
           PERFORM 1000-CUSTOMER THRU 1000-EXIT
           PERFORM 2000-SUPPLIER THRU 2000-EXIT
           TERMINATE R-KPIOTIF
           PERFORM 9350-CLOSE-SPOOL THRU 9350-EXIT.
       0000-FINISH.
           PERFORM 9150-JOB-END THRU 9150-EXIT
           MOVE XB-RC TO RETURN-CODE
           STOP RUN.

       0100-READ-ARGS.
           PERFORM 9500-STD-ARGS THRU 9500-EXIT
           IF XB-TO = 0
               MOVE XB-ASAT TO XB-TO
           END-IF
           IF XB-FROM = 0
               COMPUTE XB-FROM = XB-TO - 10000
           END-IF
           MOVE "DETAIL" TO XB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF XB-TEXT(1:1) = "Y"
               MOVE "Y" TO WS-DETAIL
           END-IF
           MOVE "XMD.OTIF.EARLY.DAYS" TO XB-CODE
           MOVE 5 TO XB-AMT
           PERFORM 9650-GET-PARM-NUM THRU 9650-EXIT
           MOVE XB-AMT TO WS-EARLY-DAYS
           MOVE XB-FROM TO XB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO S-FROM
           MOVE XB-TO TO XB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO S-TO
           MOVE XB-FROM TO XB-DATE-ED
           MOVE XB-TO TO XB-DATE-ED2
           MOVE "Basis: original promise, full quantity, despatch"
               TO S-BASIS
           DISPLAY "STERP-I-0100 KPIOTIF " FUNCTION TRIM(XB-CO)
                   " " XB-FROM " to " XB-TO.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-010  one published definition, printed on the report
      *> so that two reports of the same measure cannot drift apart
      *>----------------------------------------------------------------
       0300-PUBLISH-MEASURES.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT MetricCode, Name, LEFT(Definition + ' Zero"
                  " denominator: ' + ISNULL(ZeroDenominatorRule,''),"
                  "110) FROM rpt.MetricDefinition WHERE MetricCode "
                  "IN ('OTIF.CUSTOMER','OTIF.SUPPLIER') AND IsActive"
                  " = 1 ORDER BY MetricCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 3 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE "OTIF.CUSTOMER" TO M-CODE
               MOVE "On time in full - customer" TO M-NAME
               MOVE "Definition unavailable - rpt.MetricDefinition"
                   TO M-TEXT
               GENERATE DE-MEAS
               MOVE 4 TO XB-RC
               GO TO 0300-EXIT
           END-IF
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK
                   MOVE BR-COL(1) TO M-CODE
                   MOVE BR-COL(2) TO M-NAME
                   MOVE BR-COL(3) TO M-TEXT
                   GENERATE DE-MEAS
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-001, RPT-008  customer OTIF on the order to cash
      *> chain - a schedule is satisfied once, in full, by the date
      *> originally promised
      *>----------------------------------------------------------------
       1000-CUSTOMER.
           MOVE "CUSTOMER" TO D-SIDE
           PERFORM 1050-ZERO-COHORT THRU 1050-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT s.OrderNumber, CONVERT(varchar(12),"
                  "s.LineNum), CONVERT(varchar(12),s.ScheduleNo), "
                  "ISNULL(d.Party,''), CONVERT(varchar(24),"
                  "s.Quantity), CONVERT(varchar(24),ISNULL("
                  "s.DespatchedQty,0)), CONVERT(char(8),"
                  "s.OriginalPromiseDate,112), ISNULL(d.Actual,''),"
                  " s.Status FROM so.DeliverySchedule s OUTER APPLY"
                  " (SELECT MAX(CONVERT(char(8),h.DespatchDate,112)"
                  ") AS Actual, MAX(h.CustomerCode) AS Party FROM "
                  "so.DespatchLine dl JOIN so.Despatch h ON "
                  "h.DespatchId = dl.DespatchId WHERE h.CompanyCode "
                  "= s.CompanyCode AND dl.OrderNumber = "
                  "s.OrderNumber AND dl.OrderLineNum = s.LineNum "
                  "AND dl.ScheduleNo = s.ScheduleNo AND h.Status = "
                  "'CONFIRMED') d WHERE s.CompanyCode = ? AND "
                  "s.Status <> 'CANCELLED' AND CONVERT(char(8),"
                  "s.OriginalPromiseDate,112) BETWEEN ? AND ? ORDER"
                  " BY s.OrderNumber, s.LineNum, s.ScheduleNo"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 1060-RUN-COHORT THRU 1060-EXIT
           MOVE "Customer OTIF (original promise, full qty)"
               TO U-LABEL
           PERFORM 3000-SUMMARISE THRU 3000-EXIT.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-002, RPT-009  supplier OTIF on the procure to pay
      *> chain; up to five working days early is still on time (D-19)
      *>----------------------------------------------------------------
       2000-SUPPLIER.
           MOVE "SUPPLIER" TO D-SIDE
           PERFORM 1050-ZERO-COHORT THRU 1050-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT s.OrderNumber, CONVERT(varchar(12),"
                  "s.LineNum), CONVERT(varchar(12),s.ScheduleNo), "
                  "ISNULL(r.Party,''), CONVERT(varchar(24),"
                  "s.Quantity), CONVERT(varchar(24),ISNULL("
                  "s.ReceivedQty,0)), CONVERT(char(8),"
                  "s.OriginalConfirmedDate,112), ISNULL(r.Actual,''"
                  "), s.Status FROM po.PurchaseSchedule s OUTER "
                  "APPLY (SELECT MAX(CONVERT(char(8),h.PhysicalDate"
                  ",112)) AS Actual, MAX(h.SupplierCode) AS Party "
                  "FROM po.Receipt h WHERE h.CompanyCode = "
                  "s.CompanyCode AND h.PurchaseOrderNumber = "
                  "s.OrderNumber AND h.Status = 'CONFIRMED') r "
                  "WHERE s.CompanyCode = ? AND s.Status <> "
                  "'CANCELLED' AND CONVERT(char(8),"
                  "s.OriginalConfirmedDate,112) BETWEEN ? AND ? "
                  "ORDER BY s.OrderNumber, s.LineNum, s.ScheduleNo"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 1060-RUN-COHORT THRU 1060-EXIT
           MOVE "Supplier OTIF (original confirmation, 5 day early)"
               TO U-LABEL
           PERFORM 3000-SUMMARISE THRU 3000-EXIT.
       2000-EXIT.
           EXIT.

       1050-ZERO-COHORT.
           MOVE 0 TO K-DUE
           MOVE 0 TO K-OTIF
           MOVE 0 TO K-LATE
           MOVE 0 TO K-SHORT
           MOVE 0 TO K-EARLY
           MOVE 0 TO K-OPEN.
       1050-EXIT.
           EXIT.

       1060-RUN-COHORT.
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(2)
           MOVE XB-DATE-ED2 TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
                       SQLIO-PARM-NULL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE 9 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               DISPLAY "STERP-W-1061 " SQLIO-ERRMSG(1:90)
               MOVE 4 TO XB-RC
               GO TO 1060-EXIT
           END-IF
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK
                   PERFORM 1100-ONE-SCHEDULE THRU 1100-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       1060-EXIT.
           EXIT.

       1100-ONE-SCHEDULE.
           MOVE BR-COL(1) TO D-ORDER
           COMPUTE D-LINE = FUNCTION NUMVAL(BR-COL(2))
           COMPUTE D-SCHED = FUNCTION NUMVAL(BR-COL(3))
           MOVE BR-COL(4) TO D-PARTY
           COMPUTE D-QTY = FUNCTION NUMVAL(BR-COL(5))
           COMPUTE D-DONE = FUNCTION NUMVAL(BR-COL(6))
           MOVE BR-COL(7) TO XB-TEXT
           PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
           MOVE XB-NUM-DATE TO WS-PROMISE-N
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO D-PROMISE
           MOVE BR-COL(8) TO XB-TEXT
           PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
           MOVE XB-NUM-DATE TO WS-ACTUAL-N
           MOVE 0 TO D-DAYS
           MOVE SPACES TO D-REASON
           ADD 1 TO K-DUE
           IF WS-ACTUAL-N = 0
               MOVE SPACES TO D-ACTUAL
               MOVE "NOT DELIVERED" TO D-RESULT
               ADD 1 TO K-OPEN
               ADD 1 TO K-LATE
           ELSE
               PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
               MOVE XB-UK TO D-ACTUAL
               IF WS-PROMISE-N = 0
                   MOVE 0 TO D-DAYS
               ELSE
                   COMPUTE D-DAYS =
                       FUNCTION INTEGER-OF-DATE(WS-ACTUAL-N)
                     - FUNCTION INTEGER-OF-DATE(WS-PROMISE-N)
               END-IF
               PERFORM 1200-JUDGE THRU 1200-EXIT
           END-IF
           ADD 1 TO XB-ROWS-OUT
           IF WS-DETAIL = "Y" OR D-RESULT NOT = "OTIF"
               GENERATE DE-ROW
           END-IF.
       1100-EXIT.
           EXIT.

       1200-JUDGE.
           MOVE "OTIF" TO D-RESULT
           IF D-DAYS > 0
               MOVE "LATE" TO D-RESULT
               ADD 1 TO K-LATE
           END-IF
           IF D-SIDE = "SUPPLIER" AND D-DAYS < 0
               PERFORM 1250-EARLY-LIMIT THRU 1250-EXIT
               IF WS-ACTUAL-N < WS-EARLIEST-N
                   MOVE "EARLY" TO D-RESULT
                   ADD 1 TO K-EARLY
               END-IF
           END-IF
           IF D-DONE < D-QTY
               IF D-RESULT = "OTIF"
                   MOVE "SHORT" TO D-RESULT
               ELSE
                   MOVE "LATE/SHORT" TO D-RESULT
               END-IF
               ADD 1 TO K-SHORT
           END-IF
           IF D-RESULT = "OTIF"
               ADD 1 TO K-OTIF
           END-IF.
       1200-EXIT.
           EXIT.

       1250-EARLY-LIMIT.
      *>   earliest acceptable arrival = promise less five working
      *>   days on the England and Wales calendar (D-19)
           INITIALIZE WS-DATECALC
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DATECALC
           MOVE XB-CO TO CA-COMPANY OF WS-DATECALC
           MOVE XB-USER TO CA-USER OF WS-DATECALC
           MOVE WS-PROMISE-N TO DC-DATE-1
           COMPUTE DC-NUMBER = 0 - WS-EARLY-DAYS
           MOVE "ENG" TO DC-CALENDAR
           CALL "DATECALC" USING WS-DATECALC
           IF CA-RETURN-CODE OF WS-DATECALC < 8
               MOVE DC-RESULT-DATE TO WS-EARLIEST-N
           ELSE
               COMPUTE WS-EARLIEST-N = WS-PROMISE-N - 7
           END-IF.
       1250-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-010  a zero denominator is undefined.  the report
      *> says so rather than printing nought or one hundred per cent
      *>----------------------------------------------------------------
       3000-SUMMARISE.
           MOVE K-DUE TO U-DUE
           MOVE K-OTIF TO U-OTIF
           MOVE K-LATE TO U-LATE
           MOVE K-SHORT TO U-SHORT
           MOVE K-EARLY TO U-EARLY
           MOVE K-OPEN TO U-OPEN
           IF K-DUE = 0
               MOVE "UNDEFINED" TO U-PCT
           ELSE
               COMPUTE XB-PCT ROUNDED =
                   (K-OTIF * 100) / K-DUE
               MOVE XB-PCT TO XB-NUM-ED
               MOVE SPACES TO U-PCT
               STRING FUNCTION TRIM(XB-NUM-ED)(1:7) " pc"
                   DELIMITED BY SIZE INTO U-PCT
           END-IF
           GENERATE DE-SUMM
           DISPLAY "STERP-I-3001 " FUNCTION TRIM(U-LABEL)
                   " eligible " K-DUE " otif " K-OTIF.
       3000-EXIT.
           EXIT.

       COPY XMDBATPR.
