      *>================================================================
      *> PROGRAM-ID : CSTINVAL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *> PURPOSE    : INVENTORY VALUATION REPORT (RPT-011).  132 COLUMN
      *>              REPORT WRITER LISTING OF EVERY COSTED ITEM POOL
      *>              WITH ITS METHOD, QUANTITY, UNIT COST, CARRYING
      *>              VALUE, NET REALISABLE VALUE TEST AND ALLOWANCE.
      *>              THE RUN ENDS WITH THE RECONCILIATION TO THE
      *>              GENERAL LEDGER INVENTORY CONTROL ACCOUNT AT THE
      *>              SAME CUT OFF (CST-022) - A VALUATION THAT IS NOT
      *>              AGREED TO THE LEDGER IS NOT A VALUATION.
      *> RUN AS     : CSTINVAL CO=STC [ASAT=yyyymmdd] [SITE=xxx]
      *>              [GROUP=xxx] [METHOD=MWA|FIFO|STD] [DETAIL=LAYER]
      *>              [MODE=ALL|EXCEPTION] [USER=xxx]
      *> OUTPUT     : run\spool\CSTINVAL-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *>              cst.usp_ValuationRecon
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-09-03 RJH  CR0531 ORIGINAL - STOCK VALUATION LISTING
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS IN THE HEADINGS
      *> 2003-07-21 DJM  CR1142 NRV COLUMNS AFTER THE AUDIT FINDING
      *>                 THAT WRITE DOWNS WERE INVISIBLE ON THE RUN
      *> 2026-10-06 STERP CST-022 LEDGER RECONCILIATION FOOTING
      *>================================================================
      *> REQ: CST-001, CST-002, CST-021, CST-022, CST-026
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CSTINVAL.
       AUTHOR. R J HOLLIS.
       DATE-WRITTEN. 03/09/1997.
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
       FD  RPT-FILE REPORT IS R-CSTINVAL.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY CSTWORK.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-SITE                 PIC X(10) VALUE SPACES.
           05  WS-GROUP                PIC X(20) VALUE SPACES.
           05  WS-METHOD               PIC X(4) VALUE SPACES.
           05  WS-DETAIL               PIC X(10) VALUE SPACES.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-ASAT                 PIC 9(8) VALUE 0.
       01  WS-RUN.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-LAYERS-OUT           PIC S9(9) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-SKIP                 PIC X VALUE "N".
               88  WS-SKIP-ROW                   VALUE "Y".
           05  WS-EOF                  PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-LAYER-EOF            PIC X VALUE "N".
           05  WS-NEW-KEY              PIC X(20) VALUE SPACES.
      *>   band totals.  The footing is printed by the report writer
      *>   during the GENERATE that breaks the control, so the figures
      *>   are moved here first and the accumulators are reset - the
      *>   SUM clause cannot be used when a group has a second detail
      *>   line.
       01  WS-BAND.
           05  A-GROSS                 PIC S9(13)V99 VALUE 0.
           05  A-ALLOW                 PIC S9(13)V99 VALUE 0.
           05  BT-GROSS                PIC S9(13)V99 VALUE 0.
           05  BT-ALLOW                PIC S9(13)V99 VALUE 0.
      *>   detail line - the valuation group drives the control break
       01  WS-DET.
           05  D-GROUP                 PIC X(20) VALUE SPACES.
           05  D-ITEM                  PIC X(28) VALUE SPACES.
           05  D-DESC                  PIC X(30) VALUE SPACES.
           05  D-SITE                  PIC X(6) VALUE SPACES.
           05  D-METHOD                PIC X(4) VALUE SPACES.
           05  D-QTY                   PIC S9(11)V9(3) VALUE 0.
           05  D-UNIT                  PIC S9(9)V9(4) VALUE 0.
           05  D-GROSS                 PIC S9(13)V99 VALUE 0.
           05  D-NRV-UNIT              PIC S9(9)V9(4) VALUE 0.
           05  D-ALLOW                 PIC S9(13)V99 VALUE 0.
           05  D-NET                   PIC S9(13)V99 VALUE 0.
           05  D-FLAG                  PIC X(10) VALUE SPACES.
      *>   FIFO layer sub detail - only printed when DETAIL=LAYER
       01  WS-LAYER.
           05  L-SEQ                   PIC S9(6) VALUE 0.
           05  L-DATE                  PIC X(10) VALUE SPACES.
           05  L-REF                   PIC X(24) VALUE SPACES.
           05  L-QTY                   PIC S9(11)V9(3) VALUE 0.
           05  L-UNIT                  PIC S9(9)V9(4) VALUE 0.
           05  L-VALUE                 PIC S9(13)V99 VALUE 0.
      *>   reconciliation block - cst.usp_ValuationRecon (CST-022)
       01  WS-RECON.
           05  RC-ACCOUNT              PIC X(20) VALUE SPACES.
           05  RC-POOL                 PIC S9(13)V99 VALUE 0.
           05  RC-ALLOW                PIC S9(13)V99 VALUE 0.
           05  RC-NET                  PIC S9(13)V99 VALUE 0.
           05  RC-LAYER                PIC S9(13)V99 VALUE 0.
           05  RC-LEDGER               PIC S9(13)V99 VALUE 0.
           05  RC-DIFF                 PIC S9(13)V99 VALUE 0.
           05  RC-LAYER-DIFF           PIC S9(13)V99 VALUE 0.
           05  RC-STATE                PIC X(50) VALUE SPACES.
       01  WS-TOT.
           05  T-GROSS                 PIC S9(13)V99 VALUE 0.
           05  T-ALLOW                 PIC S9(13)V99 VALUE 0.
           05  T-NET                   PIC S9(13)V99 VALUE 0.
           05  T-POOLS                 PIC S9(7) VALUE 0.
           05  T-WRITTEN-DOWN          PIC S9(7) VALUE 0.
       REPORT SECTION.
       RD  R-CSTINVAL
           CONTROLS ARE FINAL D-GROUP
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 56
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-GROUP TYPE IS CONTROL HEADING D-GROUP.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(17) VALUE "Valuation group ".
               10  COLUMN 18   PIC X(20) SOURCE D-GROUP.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(4) VALUE "Item".
               10  COLUMN 32   PIC X(11) VALUE "Description".
               10  COLUMN 63   PIC X(4) VALUE "Site".
               10  COLUMN 70   PIC X(6) VALUE "Method".
               10  COLUMN 82   PIC X(8) VALUE "Quantity".
               10  COLUMN 97   PIC X(9) VALUE "Unit cost".
               10  COLUMN 111  PIC X(5) VALUE "Gross".
               10  COLUMN 121  PIC X(9) VALUE "Allowance".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(64) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(28) SOURCE D-ITEM.
               10  COLUMN 32   PIC X(30) SOURCE D-DESC.
               10  COLUMN 63   PIC X(6) SOURCE D-SITE.
               10  COLUMN 70   PIC X(4) SOURCE D-METHOD.
               10  COLUMN 76   PIC Z(10)9.999 SOURCE D-QTY.
               10  COLUMN 92   PIC Z(8)9.9999 SOURCE D-UNIT.
               10  COLUMN 107  PIC Z(9)9.99 SOURCE D-GROSS.
               10  COLUMN 120  PIC Z(7)9.99- SOURCE D-ALLOW.
               10  COLUMN 133  PIC X VALUE SPACE.
       01  DN-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 8    PIC X(26) VALUE
                   "net realisable value test".
               10  COLUMN 36   PIC X(10) VALUE "unit NRV".
               10  COLUMN 46   PIC Z(8)9.9999 SOURCE D-NRV-UNIT.
               10  COLUMN 63   PIC X(10) VALUE "carried at".
               10  COLUMN 74   PIC Z(9)9.99 SOURCE D-NET.
               10  COLUMN 89   PIC X(10) SOURCE D-FLAG.
       01  DL-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 8    PIC X(6) VALUE "layer ".
               10  COLUMN 14   PIC Z(5)9 SOURCE L-SEQ.
               10  COLUMN 22   PIC X(10) SOURCE L-DATE.
               10  COLUMN 34   PIC X(24) SOURCE L-REF.
               10  COLUMN 76   PIC Z(10)9.999 SOURCE L-QTY.
               10  COLUMN 92   PIC Z(8)9.9999 SOURCE L-UNIT.
               10  COLUMN 107  PIC Z(9)9.99 SOURCE L-VALUE.
       01  CF-GROUP TYPE IS CONTROL FOOTING D-GROUP.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(14) VALUE "Group total   ".
               10  COLUMN 18   PIC X(20) SOURCE D-GROUP.
               10  COLUMN 107  PIC Z(9)9.99 SOURCE BT-GROSS.
               10  COLUMN 120  PIC Z(7)9.99- SOURCE BT-ALLOW.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(30) VALUE
                   "Valuation totals - gross".
               10  COLUMN 34   PIC Z(11)9.99 SOURCE T-GROSS.
               10  COLUMN 52   PIC X(10) VALUE "allowance".
               10  COLUMN 62   PIC Z(11)9.99- SOURCE T-ALLOW.
               10  COLUMN 82   PIC X(4) VALUE "net".
               10  COLUMN 87   PIC Z(11)9.99 SOURCE T-NET.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(16) VALUE "Pools reported:".
               10  COLUMN 20   PIC Z(6)9 SOURCE T-POOLS.
               10  COLUMN 32   PIC X(26) VALUE
                   "of which written down:".
               10  COLUMN 58   PIC Z(6)9 SOURCE T-WRITTEN-DOWN.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(52) VALUE
                   "RECONCILIATION TO THE GENERAL LEDGER (CST-022)".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(30) VALUE
                   "Inventory control account".
               10  COLUMN 34   PIC X(20) SOURCE RC-ACCOUNT.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(30) VALUE
                   "Cost pool carrying value".
               10  COLUMN 34   PIC Z(11)9.99 SOURCE RC-POOL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(30) VALUE
                   "Less net realisable allowance".
               10  COLUMN 34   PIC Z(11)9.99- SOURCE RC-ALLOW.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(30) VALUE
                   "Net inventory per costing".
               10  COLUMN 34   PIC Z(11)9.99 SOURCE RC-NET.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(30) VALUE
                   "Ledger balance at cut off".
               10  COLUMN 34   PIC Z(11)9.99 SOURCE RC-LEDGER.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(30) VALUE
                   "Difference (costing - ledger)".
               10  COLUMN 34   PIC Z(11)9.99- SOURCE RC-DIFF.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(30) VALUE
                   "FIFO layers less pool value".
               10  COLUMN 34   PIC Z(11)9.99- SOURCE RC-LAYER-DIFF.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(50) SOURCE RC-STATE.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(60) VALUE
                   "Stock is carried at the lower of cost and NRV.".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(60) VALUE
                   "Clear any difference above 0.01 before the close.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS
           PERFORM 0200-JOB-START
           PERFORM 0300-OPEN-SPOOL
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 1000-PRODUCE-REPORT
           PERFORM 1900-READ-RECON
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
           MOVE "GROUP" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-GROUP
           END-IF
           MOVE "METHOD" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-METHOD
           END-IF
           MOVE "DETAIL" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-DETAIL
           END-IF
           IF CM-ASAT NOT = SPACES
               COMPUTE WS-ASAT = FUNCTION NUMVAL(CM-ASAT)
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "COST" TO WS-USER
           END-IF
           IF WS-MODE = SPACES
               MOVE "ALL" TO WS-MODE
           END-IF
           IF WS-DETAIL = SPACES
               MOVE "SUMMARY" TO WS-DETAIL
           END-IF
           IF WS-ASAT = 0
               MOVE FUNCTION CURRENT-DATE TO WS-NOW
               COMPUTE WS-ASAT = FUNCTION NUMVAL(WS-NOW(1:8))
           END-IF
           DISPLAY "STERP-I-0100 CSTINVAL valuation for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "CSTINVAL" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           IF CA-RETURN-CODE OF WS-JOBLOG > 4
               MOVE CA-RETURN-CODE OF WS-JOBLOG TO WS-RC
               DISPLAY "STERP-E-0201 "
                   CA-MESSAGE OF WS-JOBLOG
           END-IF.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "CSTINVAL" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0301 cannot build the spool path"
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           PERFORM 0350-FILL-HEADINGS
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0302 cannot open " WS-SPOOL-NAME
               GO TO 0300-EXIT
           END-IF
           INITIATE R-CSTINVAL.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "CSTINVAL" TO RPT-REPORT-ID
           MOVE "Inventory valuation and ledger reconciliation"
               TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE WS-ASAT TO WS-DATE-NUM
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "As at " WS-DATE-NUM(7:2) "/" WS-DATE-NUM(5:2)
                  "/" WS-DATE-NUM(1:4)
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           MOVE SPACES TO RPT-SUBTITLE
           IF WS-METHOD = SPACES
               STRING "All cost methods" DELIMITED BY SIZE
                   INTO RPT-SUBTITLE
           ELSE
               STRING "Cost method " FUNCTION TRIM(WS-METHOD)
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           END-IF
           IF WS-MODE = "EXCEPTION"
               MOVE "Written down pools only" TO RPT-STATUS-TEXT
           ELSE
               MOVE "All costed pools" TO RPT-STATUS-TEXT
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
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           IF SQLIO-OK
               MOVE WS-COL(1) TO RPT-COMPANY-NAME
               MOVE WS-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000 - the valuation itself.  One row per costed pool, in
      *> valuation group then item order so that the control break
      *> gives the accountant the subtotals they sign off.
      *>----------------------------------------------------------------
       1000-PRODUCE-REPORT SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(i.ValuationGroup,'UNCLASSIFIED'), "
                  "ic.ItemCode, ISNULL(i.Description,''), "
                  "ic.SiteCode, ic.CostMethod, ic.QtyOnHand, "
                  "ic.AverageUnitCost, ic.CarryingValueGBP, "
                  "ISNULL(ic.NrvUnitValue,0), ic.AllowanceGBP, "
                  "ISNULL(ic.StandardUnitCost,0) "
                  "FROM cst.ItemCost ic LEFT JOIN inv.Item i "
                  "ON i.CompanyCode = ic.CompanyCode AND "
                  "i.ItemCode = ic.ItemCode "
                  "WHERE ic.CompanyCode = ? "
                  "AND (? = '' OR ic.SiteCode = ?) "
                  "AND (? = '' OR ISNULL(i.ValuationGroup,'') = ?) "
                  "AND (? = '' OR ic.CostMethod = ?) "
                  "ORDER BY ISNULL(i.ValuationGroup,'UNCLASSIFIED'), "
                  "ic.ItemCode, ic.SiteCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-SITE TO SQLIO-PARM-VAL(2)
           MOVE WS-SITE TO SQLIO-PARM-VAL(3)
           MOVE WS-GROUP TO SQLIO-PARM-VAL(4)
           MOVE WS-GROUP TO SQLIO-PARM-VAL(5)
           MOVE WS-METHOD TO SQLIO-PARM-VAL(6)
           MOVE WS-METHOD TO SQLIO-PARM-VAL(7)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 7
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 11 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1001 " SQLIO-ERRMSG(1:100)
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-EOF
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-EOF
               ELSE
                   PERFORM 1100-EMIT-POOL
               END-IF
           END-PERFORM
           MOVE 1 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW.
       1000-EXIT.
           EXIT.

       1100-EMIT-POOL SECTION.
       1100-START.
           MOVE "N" TO WS-SKIP
           MOVE WS-COL(1) TO WS-NEW-KEY
           MOVE WS-COL(2) TO D-ITEM
           MOVE WS-COL(3) TO D-DESC
           MOVE WS-COL(4) TO D-SITE
           MOVE WS-COL(5) TO D-METHOD
           COMPUTE D-QTY = FUNCTION NUMVAL(WS-COL(6))
           COMPUTE D-UNIT = FUNCTION NUMVAL(WS-COL(7))
           COMPUTE D-GROSS = FUNCTION NUMVAL(WS-COL(8))
           COMPUTE D-NRV-UNIT = FUNCTION NUMVAL(WS-COL(9))
           COMPUTE D-ALLOW = FUNCTION NUMVAL(WS-COL(10))
      *>   a standard costed pool reports its standard, not the
      *>   moving average that is kept for information only
           IF D-METHOD = "STD"
               COMPUTE WS-UNIT1 = FUNCTION NUMVAL(WS-COL(11))
               IF WS-UNIT1 NOT = 0
                   MOVE WS-UNIT1 TO D-UNIT
               END-IF
           END-IF
           COMPUTE D-NET = D-GROSS - D-ALLOW
           MOVE SPACES TO D-FLAG
           IF D-ALLOW > 0
               MOVE "WRITE DOWN" TO D-FLAG
               ADD 1 TO T-WRITTEN-DOWN
           ELSE
               MOVE "AT COST" TO D-FLAG
           END-IF
           IF WS-MODE = "EXCEPTION" AND D-ALLOW = 0
               MOVE "Y" TO WS-SKIP
           END-IF
           IF WS-SKIP-ROW
               GO TO 1100-EXIT
           END-IF
           IF WS-NEW-KEY NOT = D-GROUP AND D-GROUP NOT = SPACES
               PERFORM 1150-CLOSE-GROUP
           END-IF
           MOVE WS-NEW-KEY TO D-GROUP
           ADD D-GROSS TO T-GROSS A-GROSS
           ADD D-ALLOW TO T-ALLOW A-ALLOW
           ADD D-NET TO T-NET
           ADD 1 TO T-POOLS WS-ROWS-OUT
           GENERATE DE-LINE
           IF D-ALLOW > 0 OR D-NRV-UNIT > 0
               GENERATE DN-LINE
           END-IF
           IF WS-DETAIL = "LAYER" AND D-METHOD = "FIFO"
               PERFORM 1200-EMIT-LAYERS
           END-IF.
       1100-EXIT.
           EXIT.

       1150-CLOSE-GROUP SECTION.
       1150-START.
           MOVE A-GROSS TO BT-GROSS
           MOVE A-ALLOW TO BT-ALLOW
           MOVE 0 TO A-GROSS A-ALLOW.
       1150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1200 - FIFO layer detail.  Cursor 3 is nested inside cursor 1,
      *> which is exactly how the 1997 program was written; the column
      *> count has to be restored afterwards because SQLIO keeps one
      *> descriptor per call, not per cursor.
      *>----------------------------------------------------------------
       1200-EMIT-LAYERS SECTION.
       1200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT cl.LayerSeq, "
                  "CONVERT(varchar(10), cl.ReceiptDate, 23), "
                  "ISNULL(cl.SourceDocRef,''), cl.QtyRemaining, "
                  "cl.UnitCost, cl.ValueRemainingGBP "
                  "FROM cst.CostLayer cl WHERE cl.CompanyCode = ? "
                  "AND cl.ItemCode = ? AND cl.IsClosed = 0 "
                  "AND cl.QtyRemaining > 0 "
                  "ORDER BY cl.ReceiptDate, cl.LayerSeq"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE D-ITEM TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           IF SQLIO-ERROR
               GO TO 1200-RESET
           END-IF
           MOVE "N" TO WS-LAYER-EOF
           PERFORM UNTIL WS-LAYER-EOF = "Y"
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-LAYER-EOF
               ELSE
                   COMPUTE L-SEQ = FUNCTION NUMVAL(WS-COL(1))
                   MOVE WS-COL(2) TO L-DATE
                   MOVE WS-COL(3) TO L-REF
                   COMPUTE L-QTY = FUNCTION NUMVAL(WS-COL(4))
                   COMPUTE L-UNIT = FUNCTION NUMVAL(WS-COL(5))
                   COMPUTE L-VALUE = FUNCTION NUMVAL(WS-COL(6))
                   ADD 1 TO WS-LAYERS-OUT
                   GENERATE DL-LINE
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW.
       1200-RESET.
           MOVE 11 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1900 - CST-022.  The ledger side comes from the proc so that
      *> the report and the screen enquiry can never disagree.
      *>----------------------------------------------------------------
       1900-READ-RECON SECTION.
       1900-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC cst.usp_ValuationRecon ?, ?, NULL"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-ASAT TO WS-DATE-NUM
           MOVE SPACES TO WS-DATE-TEXT
           STRING WS-DATE-NUM(1:4) "-" WS-DATE-NUM(5:2) "-"
                  WS-DATE-NUM(7:2)
               DELIMITED BY SIZE INTO WS-DATE-TEXT
           MOVE WS-DATE-TEXT TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 11 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           IF NOT SQLIO-OK
               MOVE "RECONCILIATION NOT AVAILABLE" TO RC-STATE
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-1901 reconciliation unavailable"
               GO TO 1900-EXIT
           END-IF
           MOVE WS-COL(3) TO RC-ACCOUNT
           COMPUTE RC-POOL = FUNCTION NUMVAL(WS-COL(4))
           COMPUTE RC-ALLOW = FUNCTION NUMVAL(WS-COL(5))
           COMPUTE RC-NET = FUNCTION NUMVAL(WS-COL(6))
           COMPUTE RC-LAYER = FUNCTION NUMVAL(WS-COL(7))
           COMPUTE RC-LEDGER = FUNCTION NUMVAL(WS-COL(8))
           COMPUTE RC-DIFF = FUNCTION NUMVAL(WS-COL(9))
           COMPUTE RC-LAYER-DIFF = FUNCTION NUMVAL(WS-COL(11))
           IF WS-COL(10)(1:1) = "Y"
               MOVE "RECONCILED - costing agrees to the ledger"
                   TO RC-STATE
           ELSE
               MOVE "*** NOT RECONCILED - INVESTIGATE BEFORE CLOSE"
                   TO RC-STATE
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
           END-IF.
       1900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           PERFORM 1150-CLOSE-GROUP
           TERMINATE R-CSTINVAL
           CLOSE RPT-FILE
           DISPLAY "STERP-I-0401 " WS-ROWS-OUT " pool(s), "
                   WS-LAYERS-OUT " layer line(s)"
           IF RC-STATE(1:3) = "***"
               DISPLAY "STERP-W-0402 valuation does not agree to "
                       "the inventory control account"
           END-IF
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           IF JL-JOB-RUN-ID <= 0
               GO TO 0500-EXIT
           END-IF
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "CSTINVAL" TO JL-JOB-NAME
           IF WS-RC > 4
               MOVE "FAILED" TO JL-STATUS
           ELSE
               IF WS-RC = 0
                   MOVE "COMPLETE" TO JL-STATUS
               ELSE
                   MOVE "WARNING" TO JL-STATUS
               END-IF
           END-IF
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE WS-ROWS-OUT TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG
           IF CA-RETURN-CODE OF WS-JOBLOG > 4
               MOVE CA-RETURN-CODE OF WS-JOBLOG TO WS-RC
               DISPLAY "STERP-E-0501 "
                   CA-MESSAGE OF WS-JOBLOG
           END-IF.
       0500-EXIT.
           EXIT.
