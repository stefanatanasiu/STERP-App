>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : BOMRP010
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : INDENTED AND COSTED EXPLOSION REPORT.  ONE 132
*>              COLUMN LISTING THAT PRINTS THE STRUCTURE OF A
*>              PARENT ITEM FOR A STATED REQUIRED QUANTITY AND
*>              EFFECTIVE DATE, INDENTED BY LEVEL, SHOWING FOR
*>              EVERY OCCURRENCE:
*>                - THE USAGE AS RECORDED AND ITS BASIS, SO THAT
*>                  A PROPORTIONAL LINE (PER NOMINAL BASIS), A
*>                  FIXED PER RUN LINE AND A FIXED PER BATCH LINE
*>                  READ DIFFERENTLY ON THE PAGE
*>                - THE GROSS PARENT STARTS THE YIELD PRODUCED
*>                - THE SCRAP ALLOWANCE ADDED TO THE INPUT
*>                - THE STOCK UNIT AND THE CONVERSION USED
*>                - THE UNROUNDED REQUIREMENT BESIDE THE ROUNDED
*>                  ISSUE QUANTITY, SO ROUNDING IS EVIDENCE AND
*>                  NOT A SILENT ADJUSTMENT
*>              IN COSTED MODE EACH LINE ALSO CARRIES THE RATE
*>              FROM THE STATED COST SET AND THE EXTENDED VALUE,
*>              WITH ONE VALUE BOUNDARY PER BRANCH, AND THE
*>              SUMMARY PRINTS MATERIAL, CONVERSION, RECOVERY
*>              CREDIT AND UNIT COST.
*> RUN AS     : BOMRP010 CO=STC ITEM=xxx QTY=n [REV=A]
*>              [ASAT=yyyymmdd] [MODE=INDENT|COSTED|LEAF]
*>              [COSTSET=STD2026] [LEVELS=n] [USER=ENGINEER]
*> OUTPUT     : run\spool\BOMRP010-<CO>-<stamp>.lst
*> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO, BOMLEAF, BOMCOST
*>----------------------------------------------------------------
*> CHANGE LOG
*> 1998-03-02 RJH  CR0611 ORIGINAL - INDENTED PARTS LIST
*> 2001-09-18 KMP  CR1104 COSTED COLUMNS ON THE SAME LISTING
*> 2014-09-30 AJS  CR2240 UNROUNDED EVIDENCE COLUMN
*> 2026-10-05 STERP STOP REASON AND COMPLETENESS FOOTER
*>================================================================
*> REQ: BOM-006, BOM-007, BOM-008, BOM-009, BOM-010, BOM-011
*> REQ: BOM-014, BOM-017, BOM-021, BOM-027, BOM-032
 IDENTIFICATION DIVISION.
 PROGRAM-ID. BOMRP010.
 AUTHOR. R J HOLLIS.
 DATE-WRITTEN. 02/03/1998.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
     SELECT RPT-FILE ASSIGN TO Ws-Spool-Name
         ORGANIZATION IS LINE SEQUENTIAL
         FILE STATUS IS Ws-File-Status.
 DATA DIVISION.
 FILE SECTION.
 FD  RPT-FILE REPORT IS R-BOMEXP.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY STRPTWS.
 COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==Ws-Cmdargs==.
 COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==Ws-Runpath==.
 COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==Ws-Joblog==.
 COPY BOMLEAFCA REPLACING ==DFHCOMMAREA== BY ==Ws-Leaf-Ca==.
 COPY BOMCOSTCA REPLACING ==DFHCOMMAREA== BY ==Ws-Cost-Ca==
      LEADING ==CA-== BY ==KA-==.
 01  Ws-Spool-Name               PIC X(200) VALUE SPACES.
 01  Ws-File-Status              PIC X(2) VALUE "00".
 01  Ws-Row.
     05  Ws-Col OCCURS 30 TIMES  PIC X(100).
 01  Ws-Args.
     05  Ws-Co                   PIC X(10) VALUE SPACES.
     05  Ws-Site                 PIC X(10) VALUE SPACES.
     05  Ws-Item                 PIC X(40) VALUE SPACES.
     05  Ws-Rev                  PIC X(4) VALUE SPACES.
     05  Ws-User                 PIC X(20) VALUE SPACES.
     05  Ws-Mode                 PIC X(10) VALUE SPACES.
     05  Ws-Cost-Set             PIC X(20) VALUE SPACES.
     05  Ws-Qty                  PIC S9(13)V9(6) VALUE 0.
     05  Ws-Asat                 PIC 9(8) VALUE 0.
     05  Ws-Levels               PIC S9(4) VALUE 0.
 01  Ws-Date-Work.
     05  Ws-Date-N               PIC 9(8) VALUE 0.
     05  Ws-Date-R REDEFINES Ws-Date-N.
         10  Ws-D-Yyyy           PIC 9(4).
         10  Ws-D-Mm             PIC 9(2).
         10  Ws-D-Dd             PIC 9(2).
 01  Ws-Run.
     05  Ws-Rc                   PIC S9(4) VALUE 0.
     05  Ws-Rows-Out             PIC S9(9) VALUE 0.
     05  Ws-Ix                   PIC S9(4) VALUE 0.
     05  Ws-W                    PIC S9(4) VALUE 0.
     05  Ws-Now                  PIC X(21) VALUE SPACES.
     05  Ws-Costed               PIC X VALUE "N".
     05  Ws-Fn                   PIC X(8) VALUE SPACES.
 01  Ws-Det.
     05  D-Level                 PIC S9(4) VALUE 0.
     05  D-Indent-Item           PIC X(46) VALUE SPACES.
     05  D-Desc                  PIC X(24) VALUE SPACES.
     05  D-Kind                  PIC X(9) VALUE SPACES.
     05  D-Basis                 PIC X(9) VALUE SPACES.
     05  D-Usage                 PIC S9(11)V9(6) VALUE 0.
     05  D-Uom                   PIC X(5) VALUE SPACES.
     05  D-Unrounded             PIC S9(11)V9(6) VALUE 0.
     05  D-Req-Qty               PIC S9(11)V9(6) VALUE 0.
     05  D-Unit-Cost             PIC S9(9)V9(6) VALUE 0.
     05  D-Ext-Cost              PIC S9(11)V9(2) VALUE 0.
     05  D-Note                  PIC X(20) VALUE SPACES.
 01  Ws-Tot.
     05  T-Lines                 PIC S9(7) VALUE 0.
     05  T-Leaf                  PIC S9(7) VALUE 0.
     05  T-Material              PIC S9(13)V9(2) VALUE 0.
     05  T-Rounded-Up            PIC S9(7) VALUE 0.
 01  Ws-Head.
     05  H-Item                  PIC X(40) VALUE SPACES.
     05  H-Rev                   PIC X(4) VALUE SPACES.
     05  H-Qty                   PIC S9(13)V9(6) VALUE 0.
     05  H-Starts                PIC S9(13)V9(6) VALUE 0.
     05  H-Runs                  PIC S9(9) VALUE 0.
     05  H-Batches               PIC S9(9) VALUE 0.
     05  H-Depth                 PIC S9(4) VALUE 0.
     05  H-Occ                   PIC S9(9) VALUE 0.
     05  H-Complete              PIC X(12) VALUE SPACES.
     05  H-Stop                  PIC X(60) VALUE SPACES.
     05  H-Total-Cost            PIC S9(13)V9(2) VALUE 0.
     05  H-Unit-Cost             PIC S9(11)V9(6) VALUE 0.
     05  H-Conv-Cost             PIC S9(13)V9(2) VALUE 0.
     05  H-Recovery              PIC S9(13)V9(2) VALUE 0.
 REPORT SECTION.
 RD  R-BOMEXP
     CONTROLS ARE FINAL
     PAGE LIMIT 60 LINES
     HEADING 1 FIRST DETAIL 11 LAST DETAIL 56
     FOOTING 58.
 COPY STRPTHDR.
 01  CH-BILL TYPE IS CONTROL HEADING FINAL.
     05  LINE PLUS 1.
         10  COLUMN 1    PIC X(14) VALUE "Parent item   ".
         10  COLUMN 15   PIC X(40) SOURCE H-Item.
         10  COLUMN 57   PIC X(4) VALUE "Rev ".
         10  COLUMN 61   PIC X(4) SOURCE H-Rev.
         10  COLUMN 67   PIC X(10) VALUE "Required  ".
         10  COLUMN 77   PIC Z(9)9.999999 SOURCE H-Qty.
     05  LINE PLUS 1.
         10  COLUMN 1    PIC X(14) VALUE "Gross starts  ".
         10  COLUMN 15   PIC Z(9)9.999999 SOURCE H-Starts.
         10  COLUMN 34   PIC X(6) VALUE "Runs: ".
         10  COLUMN 40   PIC Z(6)9 SOURCE H-Runs.
         10  COLUMN 50   PIC X(9) VALUE "Batches: ".
         10  COLUMN 59   PIC Z(6)9 SOURCE H-Batches.
         10  COLUMN 70   PIC X(7) VALUE "Depth: ".
         10  COLUMN 77   PIC Z(3)9 SOURCE H-Depth.
         10  COLUMN 86   PIC X(13) VALUE "Occurrences: ".
         10  COLUMN 99   PIC Z(8)9 SOURCE H-Occ.
     05  LINE PLUS 2.
         10  COLUMN 1    PIC X(5) VALUE "Lvl  ".
         10  COLUMN 6    PIC X(16) VALUE "Component item  ".
         10  COLUMN 52   PIC X(11) VALUE "Description".
         10  COLUMN 77   PIC X(5) VALUE "Kind ".
         10  COLUMN 86   PIC X(5) VALUE "Basis".
         10  COLUMN 96   PIC X(5) VALUE "Usage".
         10  COLUMN 110  PIC X(4) VALUE "Unit".
         10  COLUMN 117  PIC X(16) VALUE "Required (gross)".
     05  LINE PLUS 1.
         10  COLUMN 1    PIC X(66) VALUE ALL "-".
         10  COLUMN 67   PIC X(66) VALUE ALL "-".
 01  DE-LINE TYPE IS DETAIL.
     05  LINE PLUS 1.
         10  COLUMN 2    PIC Z(2)9 SOURCE D-Level.
         10  COLUMN 6    PIC X(46) SOURCE D-Indent-Item.
         10  COLUMN 52   PIC X(24) SOURCE D-Desc.
         10  COLUMN 77   PIC X(9) SOURCE D-Kind.
         10  COLUMN 86   PIC X(9) SOURCE D-Basis.
         10  COLUMN 95   PIC Z(5)9.999999 SOURCE D-Usage.
         10  COLUMN 110  PIC X(5) SOURCE D-Uom.
         10  COLUMN 116  PIC Z(7)9.999999 SOURCE D-Req-Qty.
 01  DE-COST TYPE IS DETAIL.
     05  LINE PLUS 1.
         10  COLUMN 2    PIC Z(2)9 SOURCE D-Level.
         10  COLUMN 6    PIC X(46) SOURCE D-Indent-Item.
         10  COLUMN 52   PIC X(20) SOURCE D-Desc.
         10  COLUMN 73   PIC X(9) SOURCE D-Kind.
         10  COLUMN 83   PIC Z(5)9.999999 SOURCE D-Req-Qty.
         10  COLUMN 97   PIC X(5) SOURCE D-Uom.
         10  COLUMN 103  PIC Z(5)9.999999 SOURCE D-Unit-Cost.
         10  COLUMN 118  PIC Z(9)9.99 SOURCE D-Ext-Cost.
 01  DE-UNROUND TYPE IS DETAIL.
     05  LINE PLUS 1.
         10  COLUMN 10   PIC X(26)
                         VALUE "unrounded requirement was ".
         10  COLUMN 36   PIC Z(7)9.999999 SOURCE D-Unrounded.
         10  COLUMN 52   PIC X(20) SOURCE D-Note.
 01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
     05  LINE PLUS 2.
         10  COLUMN 1    PIC X(26) VALUE "Occurrences printed      ".
         10  COLUMN 27   PIC Z(6)9 SOURCE T-Lines.
         10  COLUMN 40   PIC X(22) VALUE "of which leaf demand: ".
         10  COLUMN 62   PIC Z(6)9 SOURCE T-Leaf.
         10  COLUMN 76   PIC X(24) VALUE "lines rounded up:      ".
         10  COLUMN 100  PIC Z(6)9 SOURCE T-Rounded-Up.
     05  LINE PLUS 1.
         10  COLUMN 1    PIC X(26) VALUE "Material value           ".
         10  COLUMN 27   PIC Z(11)9.99 SOURCE T-Material.
         10  COLUMN 48   PIC X(14) VALUE "Conversion:   ".
         10  COLUMN 62   PIC Z(11)9.99 SOURCE H-Conv-Cost.
         10  COLUMN 80   PIC X(20) VALUE "Recovery credit:    ".
         10  COLUMN 100  PIC Z(11)9.99 SOURCE H-Recovery.
     05  LINE PLUS 1.
         10  COLUMN 1    PIC X(26) VALUE "Total value of the run   ".
         10  COLUMN 27   PIC Z(11)9.99 SOURCE H-Total-Cost.
         10  COLUMN 48   PIC X(14) VALUE "Unit cost:    ".
         10  COLUMN 62   PIC Z(9)9.999999 SOURCE H-Unit-Cost.
     05  LINE PLUS 2.
         10  COLUMN 1    PIC X(14) VALUE "Completeness: ".
         10  COLUMN 15   PIC X(12) SOURCE H-Complete.
         10  COLUMN 30   PIC X(60) SOURCE H-Stop.
     05  LINE PLUS 1.
         10  COLUMN 1    PIC X(62) VALUE
             "Intermediate lines are a manufacturing requirement, not purchases".
     05  LINE PLUS 1.
         10  COLUMN 1    PIC X(62) VALUE
             "Rounded quantities are the issue quantities (unrounded above)".
 PROCEDURE DIVISION.
 0000-MAIN SECTION.
 0000-START.
     PERFORM 0100-READ-ARGS
     PERFORM 0200-JOB-START
     PERFORM 0300-OPEN-SPOOL
     IF Ws-Rc > 4
         GO TO 0000-FINISH
     END-IF
     PERFORM 1000-RUN-CALCULATION
     IF Ws-Rc < 8
         PERFORM 2000-PRINT-LINES
     END-IF
     PERFORM 0400-CLOSE-SPOOL.
 0000-FINISH.
     PERFORM 0500-JOB-END
     MOVE Ws-Rc TO RETURN-CODE
     STOP RUN.

 0100-READ-ARGS SECTION.
 0100-START.
     INITIALIZE Ws-Cmdargs
     MOVE "CMDLINE" TO CA-FUNCTION OF Ws-Cmdargs
     CALL "CMDARGS" USING Ws-Cmdargs
     MOVE CM-CO TO Ws-Co
     MOVE CM-SITE TO Ws-Site
     MOVE CM-USER TO Ws-User
     MOVE CM-MODE TO Ws-Mode
     MOVE "ITEM" TO CM-LOOKUP-KEY
     PERFORM 0150-GET-ARG
     MOVE CM-LOOKUP-VALUE TO Ws-Item
     MOVE "REV" TO CM-LOOKUP-KEY
     PERFORM 0150-GET-ARG
     MOVE CM-LOOKUP-VALUE TO Ws-Rev
     MOVE "QTY" TO CM-LOOKUP-KEY
     PERFORM 0150-GET-ARG
     IF CM-LOOKUP-VALUE NOT = SPACES
         COMPUTE Ws-Qty = FUNCTION NUMVAL(CM-LOOKUP-VALUE)
     END-IF
     MOVE "COSTSET" TO CM-LOOKUP-KEY
     PERFORM 0150-GET-ARG
     MOVE CM-LOOKUP-VALUE TO Ws-Cost-Set
     MOVE "LEVELS" TO CM-LOOKUP-KEY
     PERFORM 0150-GET-ARG
     IF CM-LOOKUP-VALUE NOT = SPACES
         COMPUTE Ws-Levels = FUNCTION NUMVAL(CM-LOOKUP-VALUE)
     END-IF
     IF CM-ASAT NOT = SPACES
         COMPUTE Ws-Asat = FUNCTION NUMVAL(CM-ASAT)
     END-IF
     IF Ws-Co = SPACES
         MOVE "STC" TO Ws-Co
     END-IF
     IF Ws-User = SPACES
         MOVE "ENGINEER" TO Ws-User
     END-IF
     IF Ws-Mode = SPACES
         MOVE "INDENT" TO Ws-Mode
     END-IF
     IF Ws-Qty = 0
         MOVE 1 TO Ws-Qty
     END-IF
     IF Ws-Levels = 0
         MOVE 20 TO Ws-Levels
     END-IF
     IF Ws-Asat = 0
         MOVE FUNCTION CURRENT-DATE TO Ws-Now
         COMPUTE Ws-Asat = FUNCTION NUMVAL(Ws-Now(1:8))
     END-IF
     IF Ws-Mode = "COSTED"
         MOVE "Y" TO Ws-Costed
     END-IF
     IF Ws-Item = SPACES
         MOVE 12 TO Ws-Rc
         DISPLAY "STERP-E-BOM1001 ITEM= is required"
     END-IF
     DISPLAY "STERP-I-BOM1000 BOMRP010 explosion listing for "
             FUNCTION TRIM(Ws-Item).
 0100-EXIT.
     EXIT.

 0150-GET-ARG SECTION.
 0150-START.
     MOVE "GET" TO CA-FUNCTION OF Ws-Cmdargs
     CALL "CMDARGS" USING Ws-Cmdargs
     IF CM-FOUND NOT = "Y"
         MOVE SPACES TO CM-LOOKUP-VALUE
     END-IF.
 0150-EXIT.
     EXIT.

 0200-JOB-START SECTION.
 0200-START.
     INITIALIZE Ws-Joblog
     MOVE "START" TO CA-FUNCTION OF Ws-Joblog
     MOVE Ws-Co TO CA-COMPANY OF Ws-Joblog
     MOVE Ws-User TO CA-USER OF Ws-Joblog
     MOVE "BOMRP010" TO JL-JOB-NAME
     MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
     CALL "JOBLOG" USING Ws-Joblog.
 0200-EXIT.
     EXIT.

 0300-OPEN-SPOOL SECTION.
 0300-START.
     INITIALIZE Ws-Runpath
     MOVE "BUILD" TO CA-FUNCTION OF Ws-Runpath
     MOVE "SPOOL" TO RP-FOLDER
     MOVE "BOMRP010" TO RP-REPORT-ID
     MOVE Ws-Co TO RP-COMPANY-REF
     MOVE "Y" TO RP-USE-TIMESTAMP
     CALL "RUNPATH" USING Ws-Runpath
     IF CA-RETURN-CODE OF Ws-Runpath > 4
         MOVE 12 TO Ws-Rc
         DISPLAY "STERP-E-BOM1002 cannot build the spool path"
         GO TO 0300-EXIT
     END-IF
     MOVE RP-FULL-PATH TO Ws-Spool-Name
     PERFORM 0350-FILL-HEADINGS
     OPEN OUTPUT RPT-FILE
     IF Ws-File-Status NOT = "00"
         MOVE 12 TO Ws-Rc
         DISPLAY "STERP-E-BOM1003 cannot open " Ws-Spool-Name
         GO TO 0300-EXIT
     END-IF
     INITIATE R-BOMEXP.
 0300-EXIT.
     EXIT.

 0350-FILL-HEADINGS SECTION.
 0350-START.
     MOVE Ws-Co TO RPT-COMPANY-CODE
     MOVE "BOMRP010" TO RPT-REPORT-ID
     IF Ws-Costed = "Y"
         MOVE "Costed explosion" TO RPT-TITLE
     ELSE
         MOVE "Indented explosion" TO RPT-TITLE
     END-IF
     MOVE Ws-User TO RPT-USER
     MOVE FUNCTION CURRENT-DATE TO Ws-Now
     COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(Ws-Now(1:4))
     COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(Ws-Now(5:2))
     COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(Ws-Now(7:2))
     COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(Ws-Now(9:2))
     COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(Ws-Now(11:2))
     COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(Ws-Now(13:2))
     MOVE Ws-Asat TO Ws-Date-N
     MOVE SPACES TO RPT-AS-AT-TEXT
     STRING "Effective " Ws-D-Dd "/" Ws-D-Mm "/" Ws-D-Yyyy
         DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
     MOVE SPACES TO RPT-SUBTITLE
     IF Ws-Costed = "Y"
         STRING "Cost set " FUNCTION TRIM(Ws-Cost-Set)
                " - one value boundary per branch"
             DELIMITED BY SIZE INTO RPT-SUBTITLE
     ELSE
         STRING "Structure to level " FUNCTION TRIM(Ws-Item)
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
     MOVE Ws-Co TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE 2 TO SQLIO-COL-CNT
     PERFORM VARYING Ws-Ix FROM 1 BY 1 UNTIL Ws-Ix > 24
         MOVE 100 TO SQLIO-COL-WIDTH(Ws-Ix)
     END-PERFORM
     MOVE 7 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-OK
         MOVE Ws-Col(1) TO RPT-COMPANY-NAME
         MOVE Ws-Col(2) TO RPT-REG-NUMBER
     ELSE
         MOVE Ws-Co TO RPT-COMPANY-NAME
     END-IF.
 0360-EXIT.
     EXIT.

 0400-CLOSE-SPOOL SECTION.
 0400-START.
     TERMINATE R-BOMEXP
     CLOSE RPT-FILE
     DISPLAY "STERP-I-BOM1004 " Ws-Rows-Out " lines written to "
             FUNCTION TRIM(Ws-Spool-Name).
 0400-EXIT.
     EXIT.

 0500-JOB-END SECTION.
 0500-START.
     MOVE "END" TO CA-FUNCTION OF Ws-Joblog
     MOVE Ws-Rc TO JL-RETURN-CODE
     MOVE Ws-Rows-Out TO JL-ROWS-PROCESSED
     MOVE Ws-Spool-Name TO JL-SPOOL-FILE
     CALL "JOBLOG" USING Ws-Joblog.
 0500-EXIT.
     EXIT.

*>----------------------------------------------------------------
*> The report never calculates anything itself.  It asks the same
*> engine the enquiry screens ask, so a printed listing and the
*> screen cannot disagree about a requirement (BOM-014, BOM-032).
*>----------------------------------------------------------------
 1000-RUN-CALCULATION SECTION.
 1000-START.
     IF Ws-Costed = "Y"
         PERFORM 1100-RUN-COST
     ELSE
         PERFORM 1200-RUN-STRUCTURE
     END-IF.
 1000-EXIT.
     EXIT.

 1100-RUN-COST SECTION.
 1100-START.
     INITIALIZE Ws-Cost-Ca
     MOVE "COST" TO KA-FUNCTION
     MOVE Ws-Co TO KA-COMPANY
     MOVE Ws-Site TO KA-SITE
     MOVE Ws-User TO KA-USER
     MOVE Ws-Item TO BC-PARENT-ITEM
     MOVE Ws-Rev TO BC-PARENT-REV
     MOVE "STANDARD" TO BC-PURPOSE
     MOVE Ws-Qty TO BC-REQ-QTY
     MOVE Ws-Asat TO BC-EFF-DATE
     MOVE Ws-Levels TO BC-MAX-DEPTH
     MOVE Ws-Cost-Set TO BC-COST-SET
     MOVE "GBP" TO BC-CURRENCY
     MOVE Ws-Asat TO BC-COST-DATE
     MOVE "N" TO BC-NETTED
     CALL "BOMCOST" USING Ws-Cost-Ca
     IF KA-RETURN-CODE > 7
         MOVE 8 TO Ws-Rc
         DISPLAY "STERP-E-BOM1005 " KA-MESSAGE
         GO TO 1100-EXIT
     END-IF
     MOVE Ws-Item TO H-Item
     MOVE Ws-Rev TO H-Rev
     MOVE Ws-Qty TO H-Qty
     MOVE BC-GROSS-STARTS TO H-Starts
     MOVE BC-RUN-COUNT TO H-Runs
     MOVE BC-BATCH-COUNT TO H-Batches
     MOVE BC-OCC-COUNT TO H-Occ
     MOVE BC-COMPLETENESS TO H-Complete
     MOVE BC-TOTAL-COST TO H-Total-Cost
     MOVE BC-UNIT-COST TO H-Unit-Cost
     MOVE BC-CONVERSION-COST TO H-Conv-Cost
     MOVE BC-RECOVERY-CREDIT TO H-Recovery
     IF BC-MISSING-RATES > 0
         MOVE SPACES TO H-Stop
         STRING "Rates missing on " BC-MISSING-RATES
                " line(s) - value is incomplete"
             DELIMITED BY SIZE INTO H-Stop
     END-IF.
 1100-EXIT.
     EXIT.

 1200-RUN-STRUCTURE SECTION.
 1200-START.
     INITIALIZE Ws-Leaf-Ca
     IF Ws-Mode = "LEAF"
         MOVE "FLATTEN" TO CA-FUNCTION OF Ws-Leaf-Ca
     ELSE
         MOVE "INDENT" TO CA-FUNCTION OF Ws-Leaf-Ca
     END-IF
     MOVE Ws-Co TO CA-COMPANY OF Ws-Leaf-Ca
     MOVE Ws-Site TO CA-SITE OF Ws-Leaf-Ca
     MOVE Ws-User TO CA-USER OF Ws-Leaf-Ca
     MOVE Ws-Item TO BL-PARENT-ITEM
     MOVE Ws-Rev TO BL-PARENT-REV
     MOVE "STANDARD" TO BL-PURPOSE
     MOVE Ws-Qty TO BL-REQ-QTY
     MOVE Ws-Asat TO BL-EFF-DATE
     MOVE Ws-Levels TO BL-MAX-DEPTH
     MOVE "Y" TO BL-REEXPLODE
     MOVE 0 TO BL-FROM-LINE
     MOVE 25 TO BL-WIN-MAX
     CALL "BOMLEAF" USING Ws-Leaf-Ca
     IF CA-RETURN-CODE OF Ws-Leaf-Ca > 7
         MOVE 8 TO Ws-Rc
         DISPLAY "STERP-E-BOM1006 " CA-MESSAGE OF Ws-Leaf-Ca
         GO TO 1200-EXIT
     END-IF
     MOVE Ws-Item TO H-Item
     MOVE Ws-Rev TO H-Rev
     MOVE Ws-Qty TO H-Qty
     MOVE BL-OCC-COUNT TO H-Occ
     MOVE BL-DEPTH-REACHED TO H-Depth
     MOVE BL-COMPLETENESS TO H-Complete
     MOVE BL-STOP-SUMMARY TO H-Stop.
 1200-EXIT.
     EXIT.

*>----------------------------------------------------------------
 2000-PRINT-LINES SECTION.
 2000-START.
     IF Ws-Costed = "Y"
         PERFORM 2100-PRINT-COSTED
     ELSE
         PERFORM 2200-PRINT-STRUCTURE
     END-IF.
 2000-EXIT.
     EXIT.

 2100-PRINT-COSTED SECTION.
 2100-START.
     MOVE 0 TO BC-FROM-LINE
     MOVE 25 TO BC-WIN-MAX
     MOVE "Y" TO BC-MORE-LINES
     PERFORM UNTIL BC-MORE-LINES NOT = "Y"
         MOVE "GETLINE" TO KA-FUNCTION
         CALL "BOMCOST" USING Ws-Cost-Ca
         IF KA-RETURN-CODE > 7
             MOVE "N" TO BC-MORE-LINES
             EXIT PERFORM
         END-IF
         PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > BC-WIN-CNT
             MOVE BC-L-DEPTH(Ws-W) TO D-Level
             MOVE SPACES TO D-Indent-Item
             PERFORM 2300-INDENT-ITEM
             MOVE BC-L-DESC(Ws-W) TO D-Desc
             MOVE BC-L-SOURCE(Ws-W) TO D-Kind
             MOVE BC-L-UOM(Ws-W) TO D-Uom
             MOVE BC-L-REQ-QTY(Ws-W) TO D-Req-Qty
             MOVE BC-L-UNIT-COST(Ws-W) TO D-Unit-Cost
             MOVE BC-L-EXT-COST(Ws-W) TO D-Ext-Cost
             IF BC-L-ELEMENT(Ws-W) = "MAT" OR
                BC-L-NODE-TYPE(Ws-W) = "LEAF"
                 ADD BC-L-EXT-COST(Ws-W) TO T-Material
             END-IF
             ADD 1 TO T-Lines Ws-Rows-Out
             GENERATE DE-COST
             IF BC-L-WARNING(Ws-W) NOT = SPACES
                 MOVE BC-L-WARNING(Ws-W) TO D-Note
                 MOVE 0 TO D-Unrounded
                 GENERATE DE-UNROUND
             END-IF
         END-PERFORM
*>       BOMCOST has already moved BC-FROM-LINE to the next unread
*>       occurrence; adding the window again skipped lines 26 to 50.
     END-PERFORM.
 2100-EXIT.
     EXIT.

 2200-PRINT-STRUCTURE SECTION.
 2200-START.
     MOVE 0 TO BL-FROM-LINE
     MOVE 25 TO BL-WIN-MAX
     MOVE "Y" TO BL-MORE-LINES
     PERFORM UNTIL BL-MORE-LINES NOT = "Y"
         IF Ws-Mode = "LEAF"
             MOVE "FLATTEN" TO CA-FUNCTION OF Ws-Leaf-Ca
         ELSE
             MOVE "INDENT" TO CA-FUNCTION OF Ws-Leaf-Ca
         END-IF
         MOVE "N" TO BL-REEXPLODE
         CALL "BOMLEAF" USING Ws-Leaf-Ca
         IF CA-RETURN-CODE OF Ws-Leaf-Ca > 7
             MOVE "N" TO BL-MORE-LINES
             EXIT PERFORM
         END-IF
         PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > BL-WIN-CNT
             MOVE BL-L-DEPTH(Ws-W) TO D-Level
             PERFORM 2400-INDENT-LEAF
             MOVE BL-L-DESC(Ws-W) TO D-Desc
             MOVE BL-L-NODE-TYPE(Ws-W) TO D-Kind
             MOVE BL-L-BASIS(Ws-W) TO D-Basis
             MOVE BL-L-USAGE(Ws-W) TO D-Usage
             MOVE BL-L-UOM(Ws-W) TO D-Uom
             MOVE BL-L-REQ-QTY(Ws-W) TO D-Req-Qty
             MOVE BL-L-UNROUNDED(Ws-W) TO D-Unrounded
             ADD 1 TO T-Lines Ws-Rows-Out
             IF BL-L-NODE-TYPE(Ws-W) = "LEAF"
                 ADD 1 TO T-Leaf
             END-IF
             GENERATE DE-LINE
             IF D-Unrounded NOT = D-Req-Qty
                 ADD 1 TO T-Rounded-Up
                 MOVE BL-L-STOP(Ws-W) TO D-Note
                 GENERATE DE-UNROUND
             END-IF
         END-PERFORM
*>       BOMLEAF has already moved BL-FROM-LINE to the next unread
*>       occurrence.
     END-PERFORM.
 2200-EXIT.
     EXIT.

 2300-INDENT-ITEM SECTION.
 2300-START.
     MOVE SPACES TO D-Indent-Item
     COMPUTE Ws-Ix = BC-L-DEPTH(Ws-W) * 2 + 1
     IF Ws-Ix < 1 OR Ws-Ix > 20
         MOVE 1 TO Ws-Ix
     END-IF
     MOVE BC-L-ITEM(Ws-W) TO D-Indent-Item(Ws-Ix:40).
 2300-EXIT.
     EXIT.

 2400-INDENT-LEAF SECTION.
 2400-START.
     MOVE SPACES TO D-Indent-Item
     COMPUTE Ws-Ix = BL-L-DEPTH(Ws-W) * 2 + 1
     IF Ws-Ix < 1 OR Ws-Ix > 20
         MOVE 1 TO Ws-Ix
     END-IF
     MOVE BL-L-ITEM(Ws-W) TO D-Indent-Item(Ws-Ix:40).
 2400-EXIT.
     EXIT.
