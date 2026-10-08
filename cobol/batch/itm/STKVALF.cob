      *>================================================================
      *> PROGRAM-ID : STKVALF
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *> PURPOSE    : STOCK VALUATION RUN.  VALUES THE OWNED ON HAND
      *>              QUANTITY OF EVERY STOCK BUCKET IN SCOPE, BY ITEM
      *>              AND LOCATION AND BY LOT WHERE THE ITEM IS LOT
      *>              CONTROLLED, AND PRINTS A 132 COLUMN REPORT.  A
      *>              FLAT VALUATION FILE IS WRITTEN AT THE SAME TIME
      *>              SO THAT THE LEDGER RECONCILIATION DOES NOT HAVE
      *>              TO RE-READ THE SPOOL.  THIRD PARTY OWNED AND IN
      *>              TRANSIT MATERIAL IS LISTED BUT NEVER ADDED TO THE
      *>              OWNED VALUE, AND A BUCKET WITH NO COST EVIDENCE
      *>              IS LABELLED, NOT VALUED AT ZERO.
      *> RUN AS     : STKVALF CO=STC SITE=RDG ITEM=xxx USER=STKACCT
      *>              [ASAT=yyyymmdd] [MODE=ALL|ELIGIBLE]
      *> OUTPUT     : run\spool\STKVALF-<CO>-<stamp>.lst
      *>              run\extracts\STKVALF-<CO>-<stamp>.csv
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-07-22 RJH  CR0448 ORIGINAL - PERIOD END STOCK VALUATION
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS IN THE HEADINGS
      *> 2002-09-11 DJM  CR1056 LOT LEVEL VALUATION FOR THE AUDITORS
      *> 2009-03-04 ANW  CR2118 EXTRACT FILE FOR THE LEDGER TIE UP
      *> 2026-10-05 STERP ITM-012, ITM-013, ITM-023 COST EVIDENCE
      *>================================================================
      *> REQ: ITM-012, ITM-013, ITM-023
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STKVALF.
       AUTHOR. R J HOLLIS.
       DATE-WRITTEN. 22/07/1996.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO WS-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           SELECT VAL-FILE ASSIGN TO WS-EXTRACT-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-VAL-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-STKVALF.
       FD  VAL-FILE.
       01  VAL-REC                     PIC X(220).
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ITMWORK.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-EXTRACT-NAME             PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-VAL-STATUS               PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-SITE                 PIC X(10) VALUE SPACES.
           05  WS-ITEM                 PIC X(40) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-ASAT                 PIC 9(8) VALUE 0.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
       01  WS-RUN.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-NO-COST              PIC S9(9) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-COST-ITEM            PIC X(40) VALUE SPACES.
           05  WS-COST-HELD            PIC S9(11)V9(6) VALUE 0.
           05  WS-COST-FOUND           PIC X VALUE "N".
       01  WS-DET.
           05  V-ITEM                  PIC X(40) VALUE SPACES.
           05  V-DESC                  PIC X(34) VALUE SPACES.
           05  V-SITE                  PIC X(5) VALUE SPACES.
           05  V-WHS                   PIC X(6) VALUE SPACES.
           05  V-LOC                   PIC X(10) VALUE SPACES.
           05  V-LOT                   PIC X(18) VALUE SPACES.
           05  V-OWNER                 PIC X(10) VALUE SPACES.
           05  V-COND                  PIC X(11) VALUE SPACES.
           05  V-QTY                   PIC S9(11)V9(3) VALUE 0.
           05  V-COST                  PIC S9(9)V9(4) VALUE 0.
           05  V-VALUE                 PIC S9(11)V99 VALUE 0.
           05  V-SOURCE                PIC X(9) VALUE SPACES.
           05  V-UOM                   PIC X(5) VALUE SPACES.
           05  V-METHOD                PIC X(4) VALUE SPACES.
       01  WS-TOT.
           05  T-OWNED-VAL             PIC S9(13)V99 VALUE 0.
           05  T-OWNED-QTY             PIC S9(13)V9(3) VALUE 0.
           05  T-ELIG-VAL              PIC S9(13)V99 VALUE 0.
           05  T-THIRD-QTY             PIC S9(13)V9(3) VALUE 0.
           05  T-TRANSIT-QTY           PIC S9(13)V9(3) VALUE 0.
           05  T-BUCKETS               PIC S9(7) VALUE 0.
       01  WS-EDIT.
           05  E-QTY                   PIC -(11)9.999.
           05  E-COST                  PIC -(9)9.9999.
           05  E-VALUE                 PIC -(11)9.99.
       REPORT SECTION.
       RD  R-STKVALF
           CONTROLS ARE FINAL V-ITEM
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 56
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-ITEM TYPE IS CONTROL HEADING V-ITEM.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(5) VALUE "Item ".
               10  COLUMN 6    PIC X(40) SOURCE V-ITEM.
               10  COLUMN 48   PIC X(34) SOURCE V-DESC.
               10  COLUMN 84   PIC X(5) VALUE "Unit ".
               10  COLUMN 89   PIC X(5) SOURCE V-UOM.
               10  COLUMN 96   PIC X(7) VALUE "Method ".
               10  COLUMN 103  PIC X(4) SOURCE V-METHOD.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(5) VALUE "Site ".
               10  COLUMN 9    PIC X(5) VALUE "Whse ".
               10  COLUMN 16   PIC X(8) VALUE "Location".
               10  COLUMN 27   PIC X(13) VALUE "Lot / serial ".
               10  COLUMN 46   PIC X(5) VALUE "Owner".
               10  COLUMN 57   PIC X(9) VALUE "Condition".
               10  COLUMN 72   PIC X(8) VALUE "Quantity".
               10  COLUMN 88   PIC X(9) VALUE "Unit cost".
               10  COLUMN 104  PIC X(5) VALUE "Value".
               10  COLUMN 122  PIC X(11) VALUE "Cost source".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(66) VALUE ALL "-".
               10  COLUMN 69   PIC X(64) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(5) SOURCE V-SITE.
               10  COLUMN 9    PIC X(6) SOURCE V-WHS.
               10  COLUMN 16   PIC X(10) SOURCE V-LOC.
               10  COLUMN 27   PIC X(18) SOURCE V-LOT.
               10  COLUMN 46   PIC X(10) SOURCE V-OWNER.
               10  COLUMN 57   PIC X(11) SOURCE V-COND.
               10  COLUMN 69   PIC Z(10)9.999 SOURCE V-QTY.
               10  COLUMN 85   PIC Z(8)9.9999 SOURCE V-COST.
               10  COLUMN 100  PIC Z(10)9.99 SOURCE V-VALUE.
               10  COLUMN 122  PIC X(9) SOURCE V-SOURCE.
       01  CF-ITEM TYPE IS CONTROL FOOTING V-ITEM.
           05  LINE PLUS 1.
               10  COLUMN 27   PIC X(12) VALUE "Item totals ".
               10  COLUMN 69   PIC Z(10)9.999 SUM V-QTY.
               10  COLUMN 100  PIC Z(10)9.99 SUM V-VALUE.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(22) VALUE "Owned stock quantity:".
               10  COLUMN 26   PIC Z(12)9.999 SOURCE T-OWNED-QTY.
               10  COLUMN 48   PIC X(14) VALUE "Owned value:".
               10  COLUMN 62   PIC Z(12)9.99 SOURCE T-OWNED-VAL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(22) VALUE "Eligible stock value:".
               10  COLUMN 26   PIC Z(12)9.99 SOURCE T-ELIG-VAL.
               10  COLUMN 48   PIC X(14) VALUE "Buckets:".
               10  COLUMN 62   PIC Z(6)9 SOURCE T-BUCKETS.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(22) VALUE "Third party quantity:".
               10  COLUMN 26   PIC Z(12)9.999 SOURCE T-THIRD-QTY.
               10  COLUMN 48   PIC X(14) VALUE "In transit:".
               10  COLUMN 62   PIC Z(12)9.999 SOURCE T-TRANSIT-QTY.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(44) VALUE
                   "Third party owned stock is held in custody".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(44) VALUE
                   "and carries no value in this company.".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(44) VALUE
                   "Buckets marked NO COST have no cost".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(44) VALUE
                   "evidence and are not valued at zero.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0200-JOB-START
           PERFORM 0300-OPEN-SPOOL
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 1000-VALUE-STOCK
           PERFORM 0400-CLOSE-SPOOL.
       0000-FINISH.
           PERFORM 0500-JOB-END
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
           MOVE "ITEM" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-ITEM
           END-IF
           IF CM-ASAT NOT = SPACES
               COMPUTE WS-ASAT = FUNCTION NUMVAL(CM-ASAT)
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "STKACCT" TO WS-USER
           END-IF
           IF WS-MODE = SPACES
               MOVE "ALL" TO WS-MODE
           END-IF
           IF WS-ASAT = 0
               MOVE FUNCTION CURRENT-DATE TO WS-NOW
               COMPUTE WS-ASAT = FUNCTION NUMVAL(WS-NOW(1:8))
           END-IF
           DISPLAY "STERP-I-0100 STKVALF stock valuation for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "STKVALF" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "STKVALF" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0301 cannot build the spool path"
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "EXTRACTS" TO RP-FOLDER
           MOVE "STKVALF" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "csv" TO RP-EXTENSION
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           MOVE RP-FULL-PATH TO WS-EXTRACT-NAME
           PERFORM 0350-FILL-HEADINGS
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0302 cannot open " WS-SPOOL-NAME
               GO TO 0300-EXIT
           END-IF
           OPEN OUTPUT VAL-FILE
           IF WS-VAL-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0303 cannot open " WS-EXTRACT-NAME
               GO TO 0300-EXIT
           END-IF
           MOVE SPACES TO VAL-REC
           STRING "ITEM,SITE,WAREHOUSE,LOCATION,LOT,OWNER,"
                  "CONDITION,QTY,UNITCOST,VALUE,COSTSOURCE"
               DELIMITED BY SIZE INTO VAL-REC
           WRITE VAL-REC
           INITIATE R-STKVALF.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "STKVALF" TO RPT-REPORT-ID
           MOVE "Stock valuation report" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE WS-ASAT TO WS-DATE-N
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "As at " WS-DATE-DD "/" WS-DATE-MM "/"
                  WS-DATE-YYYY
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           MOVE SPACES TO RPT-SUBTITLE
           IF WS-SITE = SPACES
               STRING "All sites - owned stock valued"
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           ELSE
               STRING "Site " FUNCTION TRIM(WS-SITE)
                      " - owned stock valued"
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           END-IF
           PERFORM 0360-READ-COMPANY.
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
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-OK
               MOVE ITM-COL(1) TO RPT-COMPANY-NAME
               MOVE ITM-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  THE VALUATION ITSELF.  THE ENQUIRY CONTRACT RETURNS ONE
      *>       ROW PER BUCKET ALREADY SPLIT BY LOT AND SERIAL, SO THE
      *>       LOT CONTROLLED ITEMS NEED NO SPECIAL CASE HERE.
      *>----------------------------------------------------------------
       1000-VALUE-STOCK SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC inv.usp_StockEnquiry ?, ?, ?, NULL, NULL, "
                  "NULL, NULL, NULL, NULL, NULL, ?, 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           IF WS-ITEM = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(2)
               MOVE SPACES TO SQLIO-PARM-VAL(2)
           ELSE
               MOVE WS-ITEM TO SQLIO-PARM-VAL(2)
               MOVE "N" TO SQLIO-PARM-NULL(2)
           END-IF
           IF WS-SITE = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(3)
               MOVE SPACES TO SQLIO-PARM-VAL(3)
           ELSE
               MOVE WS-SITE TO SQLIO-PARM-VAL(3)
               MOVE "N" TO SQLIO-PARM-NULL(3)
           END-IF
           MOVE WS-ASAT TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE 21 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1001 " SQLIO-ERRMSG(1:100)
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA ITM-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 1100-VALUE-BUCKET
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW.
       1000-EXIT.
           EXIT.

       1100-VALUE-BUCKET SECTION.
       1100-START.
           MOVE ITM-COL(6) TO V-ITEM
           MOVE ITM-COL(2) TO V-SITE
           MOVE ITM-COL(3) TO V-WHS
           MOVE ITM-COL(4) TO V-LOC
           MOVE SPACES TO V-LOT
           IF ITM-COL(9) NOT = SPACES
               MOVE ITM-COL(9) TO V-LOT
           ELSE
               MOVE ITM-COL(8) TO V-LOT
           END-IF
           MOVE ITM-COL(10) TO V-OWNER
           MOVE ITM-COL(12) TO V-COND
           MOVE ITM-COL(15) TO V-UOM
           COMPUTE V-QTY = FUNCTION NUMVAL(ITM-COL(13))
           PERFORM 1200-ITEM-COST
           MOVE WS-COST-HELD TO V-COST
           IF WS-COST-FOUND = "Y"
               MOVE "LASTMOVE" TO V-SOURCE
           ELSE
               MOVE "NO COST" TO V-SOURCE
               ADD 1 TO WS-NO-COST
           END-IF
      *>   third party material is in our custody only - never valued
           IF ITM-COL(10) = "OWNED" AND WS-COST-FOUND = "Y"
               COMPUTE V-VALUE ROUNDED = V-QTY * WS-COST-HELD
           ELSE
               MOVE 0 TO V-VALUE
           END-IF
           IF ITM-COL(10) = "OWNED"
               ADD V-QTY TO T-OWNED-QTY
               ADD V-VALUE TO T-OWNED-VAL
           ELSE
               ADD V-QTY TO T-THIRD-QTY
           END-IF
           IF ITM-COL(5) = "TRANSIT"
               ADD V-QTY TO T-TRANSIT-QTY
           END-IF
           IF ITM-COL(20)(1:1) = "1"
               ADD V-VALUE TO T-ELIG-VAL
           END-IF
           ADD 1 TO T-BUCKETS WS-ROWS-OUT
           IF WS-MODE = "ELIGIBLE" AND ITM-COL(20)(1:1) NOT = "1"
               GO TO 1100-EXIT
           END-IF
           PERFORM 1300-WRITE-EXTRACT
           GENERATE DE-LINE.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1200  COST EVIDENCE.  CR2118: THE VALUATION USES THE COST THAT
      *>       THE LAST VALUED MOVEMENT CARRIED, BECAUSE THAT IS THE
      *>       COST THE LEDGER WAS POSTED WITH.  NO COST EVIDENCE IS
      *>       REPORTED AS SUCH - A ZERO WOULD UNDERSTATE THE STOCK.
      *>       THE RESULT IS CACHED FOR THE ITEM BEING LISTED.
      *>----------------------------------------------------------------
       1200-ITEM-COST SECTION.
       1200-START.
           IF V-ITEM = WS-COST-ITEM
               GO TO 1200-EXIT
           END-IF
           MOVE V-ITEM TO WS-COST-ITEM
           MOVE 0 TO WS-COST-HELD
           MOVE "N" TO WS-COST-FOUND
           MOVE SPACES TO V-DESC
           MOVE SPACES TO V-METHOD
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Description, CostMethod FROM inv.Item "
                  "WHERE CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE V-ITEM TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-OK
               MOVE ITM-COL(1) TO V-DESC
               MOVE ITM-COL(2) TO V-METHOD
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 UnitCost FROM inv.StockMovement "
                  "WHERE CompanyCode = ? AND ItemCode = ? AND "
                  "UnitCost IS NOT NULL AND PostingDate <= ? "
                  "ORDER BY MovementId DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE V-ITEM TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE WS-ASAT TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-OK AND ITM-COL(1) NOT = SPACES
               COMPUTE WS-COST-HELD = FUNCTION NUMVAL(ITM-COL(1))
               MOVE "Y" TO WS-COST-FOUND
           END-IF
           MOVE 21 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR.
       1200-EXIT.
           EXIT.

       1300-WRITE-EXTRACT SECTION.
       1300-START.
           MOVE V-QTY TO E-QTY
           MOVE V-COST TO E-COST
           MOVE V-VALUE TO E-VALUE
           MOVE SPACES TO VAL-REC
           STRING FUNCTION TRIM(V-ITEM) ","
                  FUNCTION TRIM(V-SITE) ","
                  FUNCTION TRIM(V-WHS) ","
                  FUNCTION TRIM(V-LOC) ","
                  FUNCTION TRIM(V-LOT) ","
                  FUNCTION TRIM(V-OWNER) ","
                  FUNCTION TRIM(V-COND) ","
               DELIMITED BY SIZE INTO VAL-REC
           STRING FUNCTION TRIM(VAL-REC)
                  FUNCTION TRIM(E-QTY) ","
                  FUNCTION TRIM(E-COST) ","
                  FUNCTION TRIM(E-VALUE) ","
                  FUNCTION TRIM(V-SOURCE)
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO VAL-REC
           WRITE VAL-REC
           IF WS-VAL-STATUS NOT = "00"
               MOVE 8 TO WS-RC
           END-IF.
       1300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-STKVALF
           CLOSE RPT-FILE
           CLOSE VAL-FILE
           IF WS-NO-COST > 0
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-0402 " WS-NO-COST
                       " bucket(s) have no cost evidence"
           END-IF
           DISPLAY "STERP-I-0403 " WS-ROWS-OUT " bucket(s) valued"
           DISPLAY "STERP-I-0404 " FUNCTION TRIM(WS-EXTRACT-NAME)
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "STKVALF" TO JL-JOB-NAME
           EVALUATE TRUE
               WHEN WS-RC = 0
                   MOVE "COMPLETE" TO JL-STATUS
               WHEN WS-RC = 4
                   MOVE "WARNING" TO JL-STATUS
               WHEN OTHER
                   MOVE "FAILED" TO JL-STATUS
           END-EVALUATE
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE WS-ROWS-OUT TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
