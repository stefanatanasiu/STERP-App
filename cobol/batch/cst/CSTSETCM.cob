      *>================================================================
      *> PROGRAM-ID : CSTSETCM
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *> PURPOSE    : COST SET COMPARISON AND REVALUATION IMPACT
      *>              (CST-012).  LINE BY LINE COMPARISON OF A
      *>              PROPOSED COST SET WITH THE SET IT WOULD REPLACE,
      *>              BANDED INTO ITEMS THAT MOVE MORE THAN THE
      *>              THRESHOLD, ITEMS THAT MOVE LESS, ITEMS THAT ARE
      *>              NEW AND ITEMS THAT HAVE BEEN DROPPED.  THE
      *>              REVALUATION IMPACT IS THE CHANGE TIMES THE
      *>              QUANTITY ON HAND - WHAT ACTIVATION WOULD POST.
      *>              THE REPORT CHANGES NOTHING (CST-023).
      *> RUN AS     : CSTSETCM CO=STC COSTSET=FY2027DRAFT
      *>              [COMPARETO=FY2026STD] [THRESHOLD=5]
      *>              [MODE=ALL|EXCEPTION] [USER=xxx]
      *> OUTPUT     : run\spool\CSTSETCM-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-06-30 RJH  CR0525 ORIGINAL - STANDARD COST COMPARISON
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT EFFECTIVE DATES
      *> 2002-04-08 DJM  CR0998 BANDS AND THRESHOLD PARAMETER SO THAT
      *>                 THE REVIEW MEETING HAS SOMETHING TO ARGUE
      *>                 ABOUT RATHER THAN A 400 PAGE LISTING
      *> 2026-10-06 STERP CST-012 REVALUATION IMPACT COLUMN
      *>================================================================
      *> REQ: CST-007, CST-009, CST-012, CST-013, CST-023
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CSTSETCM.
       AUTHOR. R J HOLLIS.
       DATE-WRITTEN. 30/06/1997.
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
       FD  RPT-FILE REPORT IS R-CSTSETCM.
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
           05  WS-SET                  PIC X(20) VALUE SPACES.
           05  WS-BASE                 PIC X(20) VALUE SPACES.
           05  WS-MODE                 PIC X(12) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-THRESHOLD            PIC S9(5)V99 VALUE 0.
           05  WS-THRESH-TEXT          PIC X(10) VALUE SPACES.
           05  WS-SAVE-SET             PIC X(20) VALUE SPACES.
       01  WS-SETINFO.
           05  S-NAME                  PIC X(40) VALUE SPACES.
           05  S-STATUS                PIC X(10) VALUE SPACES.
           05  S-PURPOSE               PIC X(12) VALUE SPACES.
           05  B-NAME                  PIC X(40) VALUE SPACES.
           05  B-STATUS                PIC X(10) VALUE SPACES.
       01  WS-RUN.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-EOF                  PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-NEW-KEY              PIC X(28) VALUE SPACES.
      *>   band totals.  SUM cannot be used because the group has a
      *>   second detail line, so the figures are parked here as the
      *>   control breaks and the accumulator starts again.
       01  WS-BAND.
           05  A-IMPACT                PIC S9(13)V99 VALUE 0.
           05  BT-IMPACT               PIC S9(13)V99 VALUE 0.
       01  WS-DET.
           05  D-BAND                  PIC X(28) VALUE SPACES.
           05  D-ITEM                  PIC X(28) VALUE SPACES.
           05  D-REV                   PIC X(4) VALUE SPACES.
           05  D-DESC                  PIC X(26) VALUE SPACES.
           05  D-OLD                   PIC S9(9)V9(4) VALUE 0.
           05  D-NEW                   PIC S9(9)V9(4) VALUE 0.
           05  D-CHANGE                PIC S9(9)V9(4) VALUE 0.
           05  D-PCT                   PIC S9(5)V99 VALUE 0.
           05  D-QTY                   PIC S9(11)V9(3) VALUE 0.
           05  D-IMPACT                PIC S9(11)V99 VALUE 0.
           05  D-NOTE                  PIC X(18) VALUE SPACES.
       01  WS-TOT.
           05  T-IMPACT                PIC S9(13)V99 VALUE 0.
           05  T-UP                    PIC S9(13)V99 VALUE 0.
           05  T-DOWN                  PIC S9(13)V99 VALUE 0.
           05  T-ITEMS                 PIC S9(7) VALUE 0.
           05  T-OVER                  PIC S9(7) VALUE 0.
           05  T-NEW                   PIC S9(7) VALUE 0.
           05  T-DROPPED               PIC S9(7) VALUE 0.
       REPORT SECTION.
       RD  R-CSTSETCM
           CONTROLS ARE FINAL D-BAND
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 56
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-BAND TYPE IS CONTROL HEADING D-BAND.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(28) SOURCE D-BAND.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(4) VALUE "Item".
               10  COLUMN 32   PIC X(3) VALUE "Rev".
               10  COLUMN 37   PIC X(11) VALUE "Description".
               10  COLUMN 66   PIC X(8) VALUE "Previous".
               10  COLUMN 81   PIC X(8) VALUE "Proposed".
               10  COLUMN 96   PIC X(6) VALUE "Change".
               10  COLUMN 106  PIC X(7) VALUE "Pct".
               10  COLUMN 117  PIC X(16) VALUE "Impact on stock".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(64) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(28) SOURCE D-ITEM.
               10  COLUMN 32   PIC X(4) SOURCE D-REV.
               10  COLUMN 37   PIC X(26) SOURCE D-DESC.
               10  COLUMN 64   PIC Z(7)9.9999 SOURCE D-OLD.
               10  COLUMN 79   PIC Z(7)9.9999 SOURCE D-NEW.
               10  COLUMN 92   PIC Z(5)9.9999- SOURCE D-CHANGE.
               10  COLUMN 105  PIC Z(4)9.99- SOURCE D-PCT.
               10  COLUMN 116  PIC Z(8)9.99- SOURCE D-IMPACT.
       01  DQ-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 8    PIC X(16) VALUE "quantity on hand".
               10  COLUMN 26   PIC Z(10)9.999 SOURCE D-QTY.
               10  COLUMN 45   PIC X(18) SOURCE D-NOTE.
       01  CF-BAND TYPE IS CONTROL FOOTING D-BAND.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(10) VALUE "Band total".
               10  COLUMN 14   PIC X(28) SOURCE D-BAND.
               10  COLUMN 116  PIC Z(8)9.99- SOURCE BT-IMPACT.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(32) VALUE
                   "Revaluation impact if activated".
               10  COLUMN 36   PIC Z(11)9.99- SOURCE T-IMPACT.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(32) VALUE
                   "   of which increases stock by".
               10  COLUMN 36   PIC Z(11)9.99 SOURCE T-UP.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(32) VALUE
                   "   of which reduces stock by".
               10  COLUMN 36   PIC Z(11)9.99- SOURCE T-DOWN.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(17) VALUE "Items compared:".
               10  COLUMN 21   PIC Z(6)9 SOURCE T-ITEMS.
               10  COLUMN 33   PIC X(18) VALUE "over threshold:".
               10  COLUMN 51   PIC Z(6)9 SOURCE T-OVER.
               10  COLUMN 63   PIC X(6) VALUE "new:".
               10  COLUMN 69   PIC Z(6)9 SOURCE T-NEW.
               10  COLUMN 81   PIC X(10) VALUE "dropped:".
               10  COLUMN 91   PIC Z(6)9 SOURCE T-DROPPED.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(62) VALUE
                   "This report changes nothing.  Activation is a".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(62) VALUE
                   "separate act, and the approver is not the preparer."
           .
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS
           PERFORM 0200-JOB-START
           PERFORM 0150-READ-SETS
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0300-OPEN-SPOOL
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 1000-PRODUCE-REPORT
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
           MOVE CM-USER TO WS-USER
           MOVE CM-MODE TO WS-MODE
           MOVE "COSTSET" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-SET
           END-IF
           MOVE "COMPARETO" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-BASE
           END-IF
           MOVE "THRESHOLD" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-THRESH-TEXT
               COMPUTE WS-THRESHOLD =
                   FUNCTION NUMVAL(WS-THRESH-TEXT)
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
           IF WS-THRESHOLD = 0
               MOVE 5 TO WS-THRESHOLD
           END-IF
           DISPLAY "STERP-I-0100 CSTSETCM cost set comparison for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 0150 - both sets must exist before a page is printed.  CST0402
      *> and CST0410 are the same reason codes STDCOST uses online, so
      *> the operator sees one vocabulary.
      *>----------------------------------------------------------------
       0150-READ-SETS SECTION.
       0150-START.
           IF WS-SET = SPACES
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0151 CST0401 COSTSET= is required"
               GO TO 0150-EXIT
           END-IF
           PERFORM 0160-LOOKUP-SET
           IF NOT SQLIO-OK
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0152 CST0402 cost set "
                       FUNCTION TRIM(WS-SET) " not found"
               GO TO 0150-EXIT
           END-IF
           MOVE WS-COL(1) TO S-NAME
           MOVE WS-COL(2) TO S-STATUS
           MOVE WS-COL(3) TO S-PURPOSE
      *>   no COMPARETO given - fall back to the set the proposal was
      *>   prepared against, then to the live standard
           IF WS-BASE = SPACES
               MOVE WS-COL(4) TO WS-BASE
           END-IF
           IF WS-BASE = SPACES
               PERFORM 0170-FIND-ACTIVE
           END-IF
           IF WS-BASE = SPACES
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0153 CST0410 no comparison set"
               GO TO 0150-EXIT
           END-IF
           MOVE WS-BASE TO WS-WORK-TEXT
           MOVE WS-SET TO WS-SAVE-SET
           MOVE WS-BASE TO WS-SET
           PERFORM 0160-LOOKUP-SET
           MOVE WS-SAVE-SET TO WS-SET
           IF NOT SQLIO-OK
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0154 CST0410 comparison set "
                       FUNCTION TRIM(WS-BASE) " not found"
               GO TO 0150-EXIT
           END-IF
           MOVE WS-COL(1) TO B-NAME
           MOVE WS-COL(2) TO B-STATUS.
       0150-EXIT.
           EXIT.

       0160-LOOKUP-SET SECTION.
       0160-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Name, Status, Purpose, "
                  "ISNULL(ComparedToSetCode,'') FROM cst.CostSet "
                  "WHERE CompanyCode = ? AND CostSetCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-SET TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 4 TO SQLIO-COL-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW.
       0160-EXIT.
           EXIT.

       0170-FIND-ACTIVE SECTION.
       0170-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CostSetCode FROM cst.CostSet "
                  "WHERE CompanyCode = ? AND Status = 'ACTIVE' "
                  "AND Purpose = 'STANDARD' AND CostSetCode <> ? "
                  "ORDER BY EffectiveFrom DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-SET TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           IF SQLIO-OK
               MOVE WS-COL(1) TO WS-BASE
           END-IF.
       0170-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "CSTSETCM" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "CSTSETCM" TO RP-REPORT-ID
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
           INITIATE R-CSTSETCM.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "CSTSETCM" TO RPT-REPORT-ID
           MOVE "Cost set comparison and revaluation impact"
               TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE SPACES TO RPT-SUBTITLE
           STRING FUNCTION TRIM(WS-SET) " (" FUNCTION TRIM(S-STATUS)
                  ") against " FUNCTION TRIM(WS-BASE) " ("
                  FUNCTION TRIM(B-STATUS) ")"
               DELIMITED BY SIZE INTO RPT-SUBTITLE
           MOVE SPACES TO RPT-AS-AT-TEXT
           MOVE WS-THRESHOLD TO WS-PCT-ED
           STRING "Threshold " FUNCTION TRIM(WS-PCT-ED) " per cent"
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           MOVE S-PURPOSE TO RPT-STATUS-TEXT
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
      *> 1000 - a FULL JOIN so that an item added to the proposal and
      *> an item dropped from it are both visible.  Quantity on hand is
      *> summed over the sites because revaluation is posted on the
      *> company's owned stock, not per site.
      *>----------------------------------------------------------------
       1000-PRODUCE-REPORT SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(n.ItemCode, o.ItemCode), "
                  "ISNULL(n.Revision, o.Revision), "
                  "ISNULL(i.Description,''), "
                  "ISNULL(o.TotalUnitCost, 0), "
                  "ISNULL(n.TotalUnitCost, 0), "
                  "CASE WHEN n.ItemCode IS NULL THEN 'DROPPED' "
                  "WHEN o.ItemCode IS NULL THEN 'NEW' "
                  "ELSE 'BOTH' END, "
                  "ISNULL((SELECT SUM(ic.QtyOnHand) FROM "
                  "cst.ItemCost ic WHERE ic.CompanyCode = ? AND "
                  "ic.ItemCode = ISNULL(n.ItemCode, o.ItemCode)), 0) "
                  "FROM (SELECT ItemCode, Revision, TotalUnitCost "
                  "FROM cst.StandardCost WHERE CompanyCode = ? AND "
                  "CostSetCode = ?) n "
                  "FULL JOIN (SELECT ItemCode, Revision, "
                  "TotalUnitCost FROM cst.StandardCost WHERE "
                  "CompanyCode = ? AND CostSetCode = ?) o "
                  "ON o.ItemCode = n.ItemCode AND "
                  "o.Revision = n.Revision "
                  "LEFT JOIN inv.Item i ON i.CompanyCode = ? AND "
                  "i.ItemCode = ISNULL(n.ItemCode, o.ItemCode) "
                  "ORDER BY ISNULL(n.ItemCode, o.ItemCode), "
                  "ISNULL(n.Revision, o.Revision)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-CO TO SQLIO-PARM-VAL(2)
           MOVE WS-SET TO SQLIO-PARM-VAL(3)
           MOVE WS-CO TO SQLIO-PARM-VAL(4)
           MOVE WS-BASE TO SQLIO-PARM-VAL(5)
           MOVE WS-CO TO SQLIO-PARM-VAL(6)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 7 TO SQLIO-COL-CNT
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
                   PERFORM 1100-EMIT-ITEM
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW.
       1000-EXIT.
           EXIT.

       1100-EMIT-ITEM SECTION.
       1100-START.
           MOVE WS-COL(1) TO D-ITEM
           MOVE WS-COL(2) TO D-REV
           MOVE WS-COL(3) TO D-DESC
           COMPUTE D-OLD = FUNCTION NUMVAL(WS-COL(4))
           COMPUTE D-NEW = FUNCTION NUMVAL(WS-COL(5))
           COMPUTE D-QTY = FUNCTION NUMVAL(WS-COL(7))
           COMPUTE D-CHANGE = D-NEW - D-OLD
           MOVE 0 TO D-PCT
           IF D-OLD NOT = 0
               COMPUTE D-PCT ROUNDED = (D-CHANGE * 100) / D-OLD
           END-IF
           COMPUTE D-IMPACT ROUNDED = D-CHANGE * D-QTY
           MOVE SPACES TO D-NOTE
           EVALUATE WS-COL(6)(1:7)
               WHEN "NEW    "
                   MOVE "NOT IN THE PREVIOUS SET" TO WS-NEW-KEY
                   MOVE "new cost line" TO D-NOTE
                   ADD 1 TO T-NEW
               WHEN "DROPPED"
                   MOVE "DROPPED FROM THE PROPOSAL" TO WS-NEW-KEY
                   MOVE "no proposed cost" TO D-NOTE
                   ADD 1 TO T-DROPPED
               WHEN OTHER
                   IF FUNCTION ABS(D-PCT) >= WS-THRESHOLD
                       MOVE "CHANGE AT OR OVER THRESHOLD"
                           TO WS-NEW-KEY
                       ADD 1 TO T-OVER
                   ELSE
                       MOVE "CHANGE UNDER THRESHOLD" TO WS-NEW-KEY
                   END-IF
           END-EVALUATE
           IF WS-MODE = "EXCEPTION"
               IF WS-NEW-KEY = "CHANGE UNDER THRESHOLD"
                   GO TO 1100-EXIT
               END-IF
           END-IF
           IF WS-NEW-KEY NOT = D-BAND AND D-BAND NOT = SPACES
               PERFORM 1150-CLOSE-BAND
           END-IF
           MOVE WS-NEW-KEY TO D-BAND
           ADD D-IMPACT TO T-IMPACT A-IMPACT
           IF D-IMPACT >= 0
               ADD D-IMPACT TO T-UP
           ELSE
               ADD D-IMPACT TO T-DOWN
           END-IF
           ADD 1 TO T-ITEMS WS-ROWS-OUT
           GENERATE DE-LINE
           IF D-QTY NOT = 0 OR D-NOTE NOT = SPACES
               GENERATE DQ-LINE
           END-IF.
       1100-EXIT.
           EXIT.

       1150-CLOSE-BAND SECTION.
       1150-START.
           MOVE A-IMPACT TO BT-IMPACT
           MOVE 0 TO A-IMPACT.
       1150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           PERFORM 1150-CLOSE-BAND
           TERMINATE R-CSTSETCM
           CLOSE RPT-FILE
           DISPLAY "STERP-I-0401 " WS-ROWS-OUT " item(s) compared"
           DISPLAY "STERP-I-0403 no cost has been changed by this "
                   "run"
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "CSTSETCM" TO JL-JOB-NAME
           IF WS-RC > 4
               MOVE "FAILED" TO JL-STATUS
           ELSE
               MOVE "COMPLETE" TO JL-STATUS
           END-IF
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE WS-ROWS-OUT TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
