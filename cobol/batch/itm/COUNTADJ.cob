      *>================================================================
      *> PROGRAM-ID : COUNTADJ
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *> PURPOSE    : STOCK COUNTING.  OPENS A CONTROLLED COUNT WITH AN
      *>              EXPLICIT CUTOFF, PRINTS THE COUNT SHEETS, TAKES
      *>              COUNTED AND RECOUNTED QUANTITIES, ACCOUNTS FOR
      *>              THE MOVEMENTS THAT HAPPENED BETWEEN THE CUTOFF
      *>              AND THE COUNT, AND POSTS THE APPROVED VARIANCE AS
      *>              AN ADJUSTMENT THROUGH STKMOVE.  A VARIANCE IS
      *>              NEVER POSTED FROM AN AMBIGUOUS CUTOFF.
      *> RUN AS     : COUNTADJ CO=STC SITE=RDG USER=WHSE MODE=SHEET
      *>                  [WH=MAIN] [ITEM=xxx]
      *>              COUNTADJ CO=STC MODE=ENTER REF=CNT0001 LINE=1
      *>                  QTY=95 USER=WHSE
      *>              COUNTADJ CO=STC MODE=RECOUNT REF=CNT0001 LINE=1
      *>                  QTY=97 USER=WHSE
      *>              COUNTADJ CO=STC MODE=APPROVE REF=CNT0001
      *>                  USER=WHSEMGR
      *>              COUNTADJ CO=STC MODE=POST REF=CNT0001 USER=WHSE
      *> OUTPUT     : run\spool\COUNTSHT-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, STKMOVE, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-07-29 DJM  CR0444 ORIGINAL - PERPETUAL INVENTORY COUNT
      *> 1998-10-06 DJM  CR0911 INTERVENING MOVEMENTS AFTER THE CUTOFF
      *>                 (THE BHM STOCKTAKE DISPUTE OF MARCH 1998)
      *> 2004-02-18 KMP  CR1144 RECOUNT BEFORE ANY VARIANCE IS POSTED
      *> 2026-10-05 STERP ITM-018 ON usp_PostStockMovement VIA STKMOVE
      *>================================================================
      *> REQ: ITM-018, ITM-015
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNTADJ.
       AUTHOR. D J MARSH.
       DATE-WRITTEN. 29/07/1996.
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
       FD  RPT-FILE REPORT IS R-COUNTSHT.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ITMWORK.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY STKMOVECA REPLACING ==DFHCOMMAREA== BY ==WS-STKMOVE==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-SITE                 PIC X(10) VALUE SPACES.
           05  WS-WH                   PIC X(20) VALUE SPACES.
           05  WS-ITEM                 PIC X(40) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
           05  WS-REF                  PIC X(30) VALUE SPACES.
           05  WS-LINE-NO              PIC S9(9) VALUE 0.
           05  WS-COUNT-QTY            PIC S9(13)V9(6) VALUE 0.
       01  WS-RUN.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-ROW-CNT              PIC S9(9) VALUE 0.
           05  WS-POSTED               PIC S9(9) VALUE 0.
           05  WS-COUNT-ID             PIC S9(9) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-STATUS               PIC X(12) VALUE SPACES.
           05  WS-SPOOL-OPEN           PIC X VALUE "N".
       01  WS-LINE-WORK.
           05  L-EXPECTED              PIC S9(13)V9(6) VALUE 0.
           05  L-COUNTED               PIC S9(13)V9(6) VALUE 0.
           05  L-RECOUNT               PIC S9(13)V9(6) VALUE 0.
           05  L-INTERVENE             PIC S9(13)V9(6) VALUE 0.
           05  L-AT-COUNT              PIC S9(13)V9(6) VALUE 0.
           05  L-ADJUST                PIC S9(13)V9(6) VALUE 0.
       01  WS-DET.
           05  C-LINE                  PIC 9(5) VALUE 0.
           05  C-ITEM                  PIC X(40) VALUE SPACES.
           05  C-LOC                   PIC X(12) VALUE SPACES.
           05  C-LOT                   PIC X(18) VALUE SPACES.
           05  C-EXPECT                PIC S9(11)V9(3) VALUE 0.
           05  C-INTERV                PIC S9(11)V9(3) VALUE 0.
           05  C-COUNTED               PIC S9(11)V9(3) VALUE 0.
           05  C-VARIANCE              PIC S9(11)V9(3) VALUE 0.
           05  C-STATUS                PIC X(10) VALUE SPACES.
       REPORT SECTION.
       RD  R-COUNTSHT
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 56
           FOOTING 58.
       COPY STRPTHDR.
       01  PH-COLS TYPE IS PAGE HEADING.
           05  LINE 8.
               10  COLUMN 1    PIC X(5) VALUE "Line".
               10  COLUMN 7    PIC X(9) VALUE "Item code".
               10  COLUMN 49   PIC X(8) VALUE "Location".
               10  COLUMN 63   PIC X(11) VALUE "Lot/serial".
               10  COLUMN 83   PIC X(8) VALUE "Expected".
               10  COLUMN 99   PIC X(10) VALUE "Since cut".
               10  COLUMN 114  PIC X(7) VALUE "Counted".
               10  COLUMN 124  PIC X(8) VALUE "Variance".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC ZZZZ9 SOURCE C-LINE.
               10  COLUMN 7    PIC X(40) SOURCE C-ITEM.
               10  COLUMN 49   PIC X(12) SOURCE C-LOC.
               10  COLUMN 63   PIC X(18) SOURCE C-LOT.
               10  COLUMN 82   PIC Z(8)9.999 SOURCE C-EXPECT.
               10  COLUMN 97   PIC Z(8)9.999 SOURCE C-INTERV.
               10  COLUMN 110  PIC Z(6)9.999 SOURCE C-COUNTED.
               10  COLUMN 121  PIC Z(6)9.999 SOURCE C-VARIANCE.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(22)
                               VALUE "Lines on this count:".
               10  COLUMN 24   PIC ZZZZ9 SOURCE WS-ROW-CNT.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(41) VALUE
                   "Count the physical quantity and enter it".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(42) VALUE
                   "against the line number.  Movements after".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(42) VALUE
                   "the cutoff are shown separately and are".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(42) VALUE
                   "added to expected at the count instant.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS
           PERFORM 0200-JOB-START
           EVALUATE WS-MODE
               WHEN "SHEET"   PERFORM 1000-OPEN-COUNT
               WHEN "ENTER"   PERFORM 2000-ENTER-COUNT
               WHEN "RECOUNT" PERFORM 2000-ENTER-COUNT
               WHEN "APPROVE" PERFORM 3000-APPROVE
               WHEN "POST"    PERFORM 4000-POST-ADJUSTMENTS
               WHEN OTHER
                   MOVE 8 TO WS-RC
                   DISPLAY "STERP-E-0001 MODE must be SHEET, ENTER, "
                           "RECOUNT, APPROVE or POST"
           END-EVALUATE
           PERFORM 0500-JOB-END
           MOVE WS-RC TO RETURN-CODE
           STOP RUN.
       0000-EXIT.
           EXIT.

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
           MOVE "REF" TO CM-LOOKUP-KEY
           PERFORM 0150-GET-ARG
           MOVE CM-LOOKUP-VALUE TO WS-REF
           MOVE "WH" TO CM-LOOKUP-KEY
           PERFORM 0150-GET-ARG
           MOVE CM-LOOKUP-VALUE TO WS-WH
           MOVE "ITEM" TO CM-LOOKUP-KEY
           PERFORM 0150-GET-ARG
           MOVE CM-LOOKUP-VALUE TO WS-ITEM
           MOVE "LINE" TO CM-LOOKUP-KEY
           PERFORM 0150-GET-ARG
           IF CM-FOUND = "Y"
               COMPUTE WS-LINE-NO = FUNCTION NUMVAL(CM-LOOKUP-VALUE)
           END-IF
           MOVE "QTY" TO CM-LOOKUP-KEY
           PERFORM 0150-GET-ARG
           IF CM-FOUND = "Y"
               COMPUTE WS-COUNT-QTY =
                   FUNCTION NUMVAL(CM-LOOKUP-VALUE)
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "WHSE" TO WS-USER
           END-IF
           IF WS-MODE = SPACES
               MOVE "SHEET" TO WS-MODE
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           DISPLAY "STERP-I-0100 COUNTADJ " FUNCTION TRIM(WS-MODE)
                   " for " FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

       0150-GET-ARG SECTION.
       0150-START.
           MOVE SPACES TO CM-LOOKUP-VALUE
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND NOT = "Y"
               MOVE SPACES TO CM-LOOKUP-VALUE
           END-IF.
       0150-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "COUNTADJ" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG.
       0200-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "COUNTADJ" TO JL-JOB-NAME
           IF WS-RC = 0
               MOVE "COMPLETE" TO JL-STATUS
           ELSE
               IF WS-RC < 8
                   MOVE "WARNING" TO JL-STATUS
               ELSE
                   MOVE "FAILED" TO JL-STATUS
               END-IF
           END-IF
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE WS-ROW-CNT TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  OPEN A COUNT.  THE CUTOFF IS STAMPED ON THE HEADER AND
      *>       THE EXPECTED QUANTITIES ARE FROZEN AT THAT INSTANT.
      *>----------------------------------------------------------------
       1000-OPEN-COUNT SECTION.
       1000-START.
           IF WS-SITE = SPACES
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-1001 SITE is required to open a count"
               GO TO 1000-EXIT
           END-IF
           IF WS-REF = SPACES
               PERFORM 1050-MAKE-REF
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT inv.StockCount (CompanyCode, SiteCode, "
                  "CountRef, CountType, ScopeDescription, "
                  "WarehouseCode, CutoffAt, IsFrozen, Status, "
                  "CreatedBy) OUTPUT inserted.CountId VALUES (?, ?, "
                  "?, 'CYCLE', ?, NULLIF(?,''), SYSDATETIME(), 0, "
                  "'OPEN', ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-SITE TO SQLIO-PARM-VAL(2)
           MOVE WS-REF TO SQLIO-PARM-VAL(3)
           MOVE SPACES TO SQLIO-PARM-VAL(4)
           STRING "Cycle count " FUNCTION TRIM(WS-SITE) " "
                  FUNCTION TRIM(WS-WH) " " FUNCTION TRIM(WS-ITEM)
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(4)
           MOVE WS-WH TO SQLIO-PARM-VAL(5)
           MOVE WS-USER TO SQLIO-PARM-VAL(6)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1002 " SQLIO-ERRMSG(1:100)
               GO TO 1000-EXIT
           END-IF
           COMPUTE WS-COUNT-ID = FUNCTION NUMVAL(ITM-COL(1))
           PERFORM 1100-SNAPSHOT-LINES
           IF WS-RC > 4
               GO TO 1000-EXIT
           END-IF
           PERFORM 1200-PRINT-SHEET
           DISPLAY "STERP-I-1003 count " FUNCTION TRIM(WS-REF)
                   " opened with " WS-ROW-CNT " line(s)".
       1000-EXIT.
           EXIT.

       1050-MAKE-REF SECTION.
       1050-START.
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           MOVE SPACES TO WS-REF
           STRING "CNT" WS-NOW(3:6) WS-NOW(9:4)
               DELIMITED BY SIZE INTO WS-REF.
       1050-EXIT.
           EXIT.

       1100-SNAPSHOT-LINES SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT inv.StockCountLine (CountId, LineNum, "
                  "ItemCode, Revision, WarehouseCode, LocationCode, "
                  "LotCode, SerialNo, ExpectedQty, Status) SELECT ?,"
                  " ROW_NUMBER() OVER (ORDER BY b.ItemCode, "
                  "b.LocationCode, b.LotCode, b.SerialNo), "
                  "b.ItemCode, b.Revision, b.WarehouseCode, "
                  "b.LocationCode, b.LotCode, b.SerialNo, "
                  "b.QtyOnHand, 'OPEN' FROM inv.StockBalance b JOIN "
                  "inv.Location l ON l.CompanyCode = b.CompanyCode "
                  "AND l.SiteCode = b.SiteCode AND l.WarehouseCode ="
                  " b.WarehouseCode AND l.LocationCode = "
                  "b.LocationCode WHERE b.CompanyCode = ? AND "
                  "b.SiteCode = ? AND (? = '' OR b.WarehouseCode = ?"
                  ") AND (? = '' OR b.ItemCode = ?) AND "
                  "b.OwnerType = 'OWNED' AND l.LocationType NOT IN ("
                  "'TRANSIT','SCRAP') AND b.QtyOnHand > 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE WS-COUNT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE WS-CO TO SQLIO-PARM-VAL(2)
           MOVE WS-SITE TO SQLIO-PARM-VAL(3)
           MOVE WS-WH TO SQLIO-PARM-VAL(4)
           MOVE WS-WH TO SQLIO-PARM-VAL(5)
           MOVE WS-ITEM TO SQLIO-PARM-VAL(6)
           MOVE WS-ITEM TO SQLIO-PARM-VAL(7)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 7
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1101 " SQLIO-ERRMSG(1:100)
               GO TO 1100-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW.
       1100-EXIT.
           EXIT.

       1200-PRINT-SHEET SECTION.
       1200-START.
           PERFORM 1250-OPEN-SPOOL
           IF WS-RC > 4
               GO TO 1200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT LineNum, ItemCode, LocationCode, "
                  "ISNULL(NULLIF(SerialNo,''), ISNULL(LotCode,'')),"
                  " ExpectedQty, InterveningMovementQty, "
                  "ISNULL(CountedQty,0), Status FROM "
                  "inv.StockCountLine WHERE CountId = ? ORDER BY "
                  "LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-COUNT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 8 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1201 " SQLIO-ERRMSG(1:100)
               GO TO 1200-EXIT
           END-IF
           MOVE 0 TO WS-ROW-CNT
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA ITM-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   COMPUTE C-LINE = FUNCTION NUMVAL(ITM-COL(1))
                   MOVE ITM-COL(2) TO C-ITEM
                   MOVE ITM-COL(3) TO C-LOC
                   MOVE ITM-COL(4) TO C-LOT
                   COMPUTE C-EXPECT = FUNCTION NUMVAL(ITM-COL(5))
                   COMPUTE C-INTERV = FUNCTION NUMVAL(ITM-COL(6))
                   COMPUTE C-COUNTED = FUNCTION NUMVAL(ITM-COL(7))
                   MOVE ITM-COL(8) TO C-STATUS
                   COMPUTE C-VARIANCE = C-COUNTED - C-EXPECT
                       - C-INTERV
                   ADD 1 TO WS-ROW-CNT
                   GENERATE DE-LINE
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           PERFORM 1290-CLOSE-SPOOL.
       1200-EXIT.
           EXIT.

       1250-OPEN-SPOOL SECTION.
       1250-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "COUNTSHT" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "COUNTSHT" TO RPT-REPORT-ID
           MOVE "Stock count sheet" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE SPACES TO RPT-SUBTITLE
           STRING "Count " FUNCTION TRIM(WS-REF) " at site "
                  FUNCTION TRIM(WS-SITE)
               DELIMITED BY SIZE INTO RPT-SUBTITLE
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "Cutoff " WS-NOW(7:2) "/" WS-NOW(5:2) "/"
                  WS-NOW(1:4) " " WS-NOW(9:2) ":" WS-NOW(11:2)
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           PERFORM 1260-READ-COMPANY
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1251 cannot open " WS-SPOOL-NAME
               GO TO 1250-EXIT
           END-IF
           MOVE "Y" TO WS-SPOOL-OPEN
           INITIATE R-COUNTSHT.
       1250-EXIT.
           EXIT.

       1260-READ-COMPANY SECTION.
       1260-START.
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
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-OK
               MOVE ITM-COL(1) TO RPT-COMPANY-NAME
               MOVE ITM-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       1260-EXIT.
           EXIT.

       1290-CLOSE-SPOOL SECTION.
       1290-START.
           IF WS-SPOOL-OPEN NOT = "Y"
               GO TO 1290-EXIT
           END-IF
           TERMINATE R-COUNTSHT
           CLOSE RPT-FILE
           MOVE "N" TO WS-SPOOL-OPEN
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       1290-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  ENTER A COUNTED OR RECOUNTED QUANTITY.  CR0911: THE
      *>       MOVEMENTS POSTED SINCE THE CUTOFF ARE MEASURED AND
      *>       ADDED TO THE EXPECTED QUANTITY, SO THAT THE COUNT AND
      *>       THE EXPECTATION REFER TO THE SAME INSTANT.
      *>----------------------------------------------------------------
       2000-ENTER-COUNT SECTION.
       2000-START.
           PERFORM 2050-READ-COUNT-HEAD
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           IF WS-LINE-NO = 0
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-2001 LINE= is required"
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-INTERVENING
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           IF WS-MODE = "RECOUNT"
               STRING "UPDATE inv.StockCountLine SET RecountQty = ?,"
                      " InterveningMovementQty = ?, Status = "
                      "'RECOUNT' WHERE CountId = ? AND LineNum = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
           ELSE
               STRING "UPDATE inv.StockCountLine SET CountedQty = ?,"
                      " InterveningMovementQty = ?, Status = "
                      "'COUNTED' WHERE CountId = ? AND LineNum = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
           END-IF
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE WS-COUNT-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE L-INTERVENE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE WS-COUNT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE WS-LINE-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-2002 " SQLIO-ERRMSG(1:100)
               GO TO 2000-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           PERFORM 2150-UPDATE-HEAD
           COMPUTE L-AT-COUNT = L-EXPECTED + L-INTERVENE
           COMPUTE L-ADJUST = WS-COUNT-QTY - L-AT-COUNT
           MOVE L-ADJUST TO WS-NUM-ED
           ADD 1 TO WS-ROW-CNT
           DISPLAY "STERP-I-2003 line " WS-LINE-NO " variance "
                   FUNCTION TRIM(WS-NUM-ED).
       2000-EXIT.
           EXIT.

       2050-READ-COUNT-HEAD SECTION.
       2050-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CountId, SiteCode, Status, "
                  "ISNULL(CONVERT(varchar(23), CutoffAt, 126),'') "
                  "FROM inv.StockCount WHERE CompanyCode = ? AND "
                  "CountRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-2051 " SQLIO-ERRMSG(1:100)
               GO TO 2050-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-2052 count " FUNCTION TRIM(WS-REF)
                       " does not exist"
               GO TO 2050-EXIT
           END-IF
           COMPUTE WS-COUNT-ID = FUNCTION NUMVAL(ITM-COL(1))
           MOVE ITM-COL(2) TO WS-SITE
           MOVE ITM-COL(3) TO WS-STATUS.
       2050-EXIT.
           EXIT.

       2100-INTERVENING SECTION.
       2100-START.
           MOVE 0 TO L-INTERVENE L-EXPECTED
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT cl.ExpectedQty, ISNULL((SELECT "
                  "SUM(m.Quantity) FROM inv.StockMovement m WHERE "
                  "m.CompanyCode = c.CompanyCode AND m.SiteCode = "
                  "c.SiteCode AND m.ItemCode = cl.ItemCode AND "
                  "m.WarehouseCode = cl.WarehouseCode AND "
                  "m.LocationCode = cl.LocationCode AND "
                  "ISNULL(m.LotCode,'') = ISNULL(cl.LotCode,'') AND "
                  "m.CreatedAt > c.CutoffAt),0), cl.ItemCode, "
                  "cl.WarehouseCode, cl.LocationCode, "
                  "ISNULL(cl.LotCode,''), ISNULL(cl.SerialNo,''), "
                  "ISNULL(cl.Revision,'') FROM inv.StockCountLine cl"
                  " JOIN inv.StockCount c ON c.CountId = cl.CountId "
                  "WHERE cl.CountId = ? AND cl.LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-COUNT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-LINE-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 8 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-2101 " SQLIO-ERRMSG(1:100)
               GO TO 2100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-2102 no such count line"
               GO TO 2100-EXIT
           END-IF
           COMPUTE L-EXPECTED = FUNCTION NUMVAL(ITM-COL(1))
           COMPUTE L-INTERVENE = FUNCTION NUMVAL(ITM-COL(2))
           MOVE ITM-COL(3) TO WS-ITEM.
       2100-EXIT.
           EXIT.

       2150-UPDATE-HEAD SECTION.
       2150-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE inv.StockCount SET Status = ?, CountedBy ="
                  " ?, ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE CountId = ? AND Status IN ('OPEN','COUNTED'"
                  ",'RECOUNT')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           IF WS-MODE = "RECOUNT"
               MOVE "RECOUNT" TO SQLIO-PARM-VAL(1)
           ELSE
               MOVE "COUNTED" TO SQLIO-PARM-VAL(1)
           END-IF
           MOVE WS-USER TO SQLIO-PARM-VAL(2)
           MOVE WS-USER TO SQLIO-PARM-VAL(3)
           MOVE WS-COUNT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW.
       2150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  APPROVE.  THE APPROVER MAY NOT BE THE COUNTER.
      *>----------------------------------------------------------------
       3000-APPROVE SECTION.
       3000-START.
           PERFORM 2050-READ-COUNT-HEAD
           IF WS-RC > 4
               GO TO 3000-EXIT
           END-IF
           IF WS-STATUS NOT = "COUNTED" AND WS-STATUS NOT = "RECOUNT"
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-3001 a count must be counted before "
                       "it is approved"
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE inv.StockCount SET Status = 'APPROVED', "
                  "ApprovedBy = ?, ApprovedAt = SYSDATETIME(), "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() WHERE "
                  "CountId = ? AND ISNULL(CountedBy,'') <> ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE WS-USER TO SQLIO-PARM-VAL(1)
           MOVE WS-USER TO SQLIO-PARM-VAL(2)
           MOVE WS-COUNT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE WS-USER TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-3002 " SQLIO-ERRMSG(1:100)
               GO TO 3000-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           PERFORM 2050-READ-COUNT-HEAD
           IF WS-STATUS NOT = "APPROVED"
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-3003 the counter may not approve "
                       "their own count"
           ELSE
               DISPLAY "STERP-I-3004 count " FUNCTION TRIM(WS-REF)
                       " approved by " FUNCTION TRIM(WS-USER)
           END-IF.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  POST THE APPROVED VARIANCES AS ADJUSTMENTS THROUGH
      *>       STKMOVE, WHICH POSTS THEM THROUGH THE SHARED CONTRACT.
      *>----------------------------------------------------------------
       4000-POST-ADJUSTMENTS SECTION.
       4000-START.
           PERFORM 2050-READ-COUNT-HEAD
           IF WS-RC > 4
               GO TO 4000-EXIT
           END-IF
           IF WS-STATUS NOT = "APPROVED"
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-4001 only an approved count may be "
                       "posted"
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT cl.LineNum, cl.ItemCode, "
                  "ISNULL(cl.Revision,''), cl.WarehouseCode, "
                  "cl.LocationCode, ISNULL(cl.LotCode,''), "
                  "ISNULL(cl.SerialNo,''), cl.ExpectedQty, "
                  "cl.InterveningMovementQty, "
                  "ISNULL(cl.RecountQty, cl.CountedQty) FROM "
                  "inv.StockCountLine cl WHERE cl.CountId = ? AND "
                  "ISNULL(cl.RecountQty, cl.CountedQty) IS NOT NULL "
                  "AND cl.Status <> 'POSTED' ORDER BY cl.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-COUNT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-4002 " SQLIO-ERRMSG(1:100)
               GO TO 4000-EXIT
           END-IF
           MOVE 0 TO WS-POSTED
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA ITM-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 4100-POST-ONE
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           PERFORM 4200-CLOSE-COUNT
           MOVE WS-POSTED TO WS-ROW-CNT
           DISPLAY "STERP-I-4003 " WS-POSTED
                   " adjustment(s) posted for count "
                   FUNCTION TRIM(WS-REF).
       4000-EXIT.
           EXIT.

       4100-POST-ONE SECTION.
       4100-START.
           COMPUTE WS-LINE-NO = FUNCTION NUMVAL(ITM-COL(1))
           MOVE ITM-COL(2) TO WS-ITEM
           COMPUTE L-EXPECTED = FUNCTION NUMVAL(ITM-COL(8))
           COMPUTE L-INTERVENE = FUNCTION NUMVAL(ITM-COL(9))
           COMPUTE L-COUNTED = FUNCTION NUMVAL(ITM-COL(10))
           COMPUTE L-AT-COUNT = L-EXPECTED + L-INTERVENE
           COMPUTE L-ADJUST = L-COUNTED - L-AT-COUNT
           IF L-ADJUST = 0
               PERFORM 4150-MARK-LINE
               GO TO 4100-EXIT
           END-IF
           INITIALIZE WS-STKMOVE
           MOVE "ADJUST" TO CA-FUNCTION OF WS-STKMOVE
           MOVE WS-CO TO CA-COMPANY OF WS-STKMOVE
           MOVE WS-SITE TO CA-SITE OF WS-STKMOVE
           MOVE WS-USER TO CA-USER OF WS-STKMOVE
           MOVE WS-ITEM TO SM-ITEM-CODE
           MOVE ITM-COL(3) TO SM-REVISION
           MOVE WS-SITE TO SM-SITE
           MOVE ITM-COL(4) TO SM-WAREHOUSE
           MOVE ITM-COL(5) TO SM-LOCATION
           MOVE ITM-COL(6) TO SM-LOT-CODE
           MOVE ITM-COL(7) TO SM-SERIAL-NO
           MOVE L-ADJUST TO SM-QUANTITY
           MOVE "COUNT" TO SM-REASON-CODE
           MOVE SPACES TO SM-REASON
           STRING "Approved stock count variance on "
                  FUNCTION TRIM(WS-REF) " line "
               DELIMITED BY SIZE INTO SM-REASON
           MOVE WS-LINE-NO TO WS-INT-ED
           MOVE SPACES TO WS-TEXT
           STRING FUNCTION TRIM(SM-REASON) FUNCTION TRIM(WS-INT-ED)
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO SM-REASON
           MOVE "STOCKCOUNT" TO SM-SOURCE-DOC-TYPE
           MOVE WS-REF TO SM-SOURCE-DOC-REF
           MOVE WS-LINE-NO TO SM-SOURCE-DOC-LINE
           MOVE WS-REF TO SM-APPROVAL-REF
           MOVE "ITM" TO SM-SOURCE-MODULE
           CALL "STKMOVE" USING WS-STKMOVE
           IF CA-RETURN-CODE OF WS-STKMOVE > 4
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-4101 line " WS-LINE-NO " "
                       CA-MESSAGE OF WS-STKMOVE(1:90)
               GO TO 4100-EXIT
           END-IF
           ADD 1 TO WS-POSTED
           PERFORM 4150-MARK-LINE.
       4100-EXIT.
           EXIT.

       4150-MARK-LINE SECTION.
       4150-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE inv.StockCountLine SET AdjustmentQty = ?, "
                  "MovementId = NULLIF(?,0), ReasonCode = 'COUNT', "
                  "Status = 'POSTED' WHERE CountId = ? AND LineNum ="
                  " ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE L-ADJUST TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE SM-MOVEMENT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE WS-COUNT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE WS-LINE-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW.
       4150-EXIT.
           EXIT.

       4200-CLOSE-COUNT SECTION.
       4200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE inv.StockCount SET Status = 'POSTED', "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() WHERE "
                  "CountId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-USER TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-COUNT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW.
       4200-EXIT.
           EXIT.
