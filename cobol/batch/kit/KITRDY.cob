>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : KITRDY
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : RPT-013 KIT READINESS AND EXECUTION.  ONE 132
*>              COLUMN LISTING WITH THREE MODES:
*>
*>                READY  ONE LINE PER OPEN KIT WITH THE NUMBER OF
*>                       MATERIAL LINES COVERED, THE NUMBER STILL
*>                       SHORT AND THE READINESS PERCENTAGE, SO
*>                       THE STORES CAN SEE WHAT CAN ACTUALLY BE
*>                       PICKED TODAY
*>                EXEC   THE SAME KITS WITH THE EXECUTION
*>                       POSITION - ISSUED, RETURNED, SCRAPPED,
*>                       COMPLETED AND WORK IN PROGRESS VALUE
*>                SHORT  THE SHORTAGE LIST: ONE LINE PER MATERIAL
*>                       LINE THAT IS STILL SHORT, WITH THE
*>                       REMAINING REQUIREMENT, WHAT IS RESERVED,
*>                       WHAT IS PICKED AND THE NEED DATE
*>
*>              A KIT IS READY WHEN EVERY MANDATORY LINE IS
*>              COVERED.  READINESS IS NOT AN AVERAGE OF THE
*>              LINES: NINETY PER CENT OF A KIT BUILDS NOTHING,
*>              SO THE LISTING PRINTS BOTH THE PERCENTAGE AND THE
*>              PLAIN YES OR NO (KIT-022).
*> RUN AS     : KITRDY CO=STC [SITE=RDG] [MODE=READY|EXEC|SHORT]
*>              [STATUS=RELEASED] [ASAT=yyyymmdd] [USER=STORES]
*> OUTPUT     : run\spool\KITRDY-<CO>-<stamp>.lst
*> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
*>----------------------------------------------------------------
*> CHANGE LOG
*> 1996-02-19 DRN  CR0355 KIT SHORTAGE LIST
*> 2016-03-14 PMK  CR2463 READINESS AND EXECUTION ON ONE LISTING
*> 2026-10-06 STERP KIT-022, RPT-013
*>================================================================
*> REQ: KIT-022
 IDENTIFICATION DIVISION.
 PROGRAM-ID. KITRDY.
 AUTHOR. D R NEWELL.
 DATE-WRITTEN. 19/02/1996.
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
 FD  RPT-FILE REPORT IS R-KITRDY.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY STRPTWS.
 COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==Ws-Cmdargs==.
 COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==Ws-Runpath==.
 COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==Ws-Joblog==.
 01  Ws-Spool-Name              PIC X(200) VALUE SPACES.
 01  Ws-File-Status             PIC X(2) VALUE "00".
 01  Ws-Row.
     05  Ws-Col OCCURS 20 TIMES PIC X(100).
 01  Ws-Args.
     05  Ws-Co                  PIC X(10) VALUE SPACES.
     05  Ws-Site                PIC X(10) VALUE SPACES.
     05  Ws-User                PIC X(20) VALUE SPACES.
     05  Ws-Mode                PIC X(10) VALUE SPACES.
     05  Ws-Status-Arg          PIC X(14) VALUE SPACES.
     05  Ws-Asat                PIC 9(8) VALUE 0.
 01  Ws-Date-Work.
     05  Ws-Date-N              PIC 9(8) VALUE 0.
     05  Ws-Date-R REDEFINES Ws-Date-N.
         10  Ws-D-Yyyy          PIC 9(4).
         10  Ws-D-Mm            PIC 9(2).
         10  Ws-D-Dd            PIC 9(2).
 01  Ws-Run.
     05  Ws-Rc                  PIC S9(4) VALUE 0.
     05  Ws-Ix                  PIC S9(4) VALUE 0.
     05  Ws-Now                 PIC X(21) VALUE SPACES.
     05  Ws-More                PIC X VALUE "Y".
 01  Ws-Det.
     05  D-Kit-Ref              PIC X(16) VALUE SPACES.
     05  D-Parent               PIC X(22) VALUE SPACES.
     05  D-Status               PIC X(13) VALUE SPACES.
     05  D-Planned              PIC S9(11)V9(3) VALUE 0.
     05  D-Completed            PIC S9(11)V9(3) VALUE 0.
     05  D-Need                 PIC X(10) VALUE SPACES.
     05  D-Lines                PIC S9(5) VALUE 0.
     05  D-Short                PIC S9(5) VALUE 0.
     05  D-Ready-Pct            PIC S9(3)V9(1) VALUE 0.
     05  D-Ready-Flag           PIC X(3) VALUE SPACES.
     05  D-Item                 PIC X(26) VALUE SPACES.
     05  D-Required             PIC S9(11)V9(3) VALUE 0.
     05  D-Remaining            PIC S9(11)V9(3) VALUE 0.
     05  D-Reserved             PIC S9(11)V9(3) VALUE 0.
     05  D-Picked               PIC S9(11)V9(3) VALUE 0.
     05  D-Issued               PIC S9(11)V9(3) VALUE 0.
     05  D-Returned             PIC S9(11)V9(3) VALUE 0.
     05  D-Scrapped             PIC S9(11)V9(3) VALUE 0.
     05  D-Wip                  PIC S9(11)V9(2) VALUE 0.
     05  D-Uom                  PIC X(6) VALUE SPACES.
 01  Ws-Tot.
     05  T-Kits                 PIC S9(7) VALUE 0.
     05  T-Ready                PIC S9(7) VALUE 0.
     05  T-Short-Lines          PIC S9(7) VALUE 0.
     05  T-Wip                  PIC S9(13)V9(2) VALUE 0.
 01  Ws-Head.
     05  H-Mode-Text            PIC X(40) VALUE SPACES.
     05  H-Scope                PIC X(60) VALUE SPACES.
 REPORT SECTION.
 RD  R-KITRDY
     CONTROLS ARE FINAL
     PAGE LIMIT 60 LINES
     HEADING 1 FIRST DETAIL 11 LAST DETAIL 56
     FOOTING 58.
 COPY STRPTHDR.
 01  CH-SCOPE TYPE IS CONTROL HEADING FINAL.
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(8) VALUE "Mode    ".
         10  COLUMN 10  PIC X(40) SOURCE H-Mode-Text.
         10  COLUMN 55  PIC X(60) SOURCE H-Scope.
     05  LINE PLUS 2.
         10  COLUMN 1   PIC X(16) VALUE "Kit reference   ".
         10  COLUMN 18  PIC X(22) VALUE "Parent item           ".
         10  COLUMN 41  PIC X(13) VALUE "Status       ".
         10  COLUMN 55  PIC X(14) VALUE "       Planned".
         10  COLUMN 70  PIC X(14) VALUE "     Completed".
         10  COLUMN 85  PIC X(10) VALUE "Need date ".
         10  COLUMN 96  PIC X(6) VALUE " Lines".
         10  COLUMN 103 PIC X(6) VALUE " Short".
         10  COLUMN 110 PIC X(8) VALUE " Ready %".
         10  COLUMN 119 PIC X(5) VALUE "Ready".
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(130) VALUE ALL "-".

 01  DT-KIT TYPE IS DETAIL.
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(16) SOURCE D-Kit-Ref.
         10  COLUMN 18  PIC X(22) SOURCE D-Parent.
         10  COLUMN 41  PIC X(13) SOURCE D-Status.
         10  COLUMN 55  PIC Z(9)9.999 SOURCE D-Planned.
         10  COLUMN 70  PIC Z(9)9.999 SOURCE D-Completed.
         10  COLUMN 85  PIC X(10) SOURCE D-Need.
         10  COLUMN 96  PIC Z(4)9 SOURCE D-Lines.
         10  COLUMN 103 PIC Z(4)9 SOURCE D-Short.
         10  COLUMN 110 PIC Z(2)9.9 SOURCE D-Ready-Pct.
         10  COLUMN 120 PIC X(3) SOURCE D-Ready-Flag.

 01  DT-EXEC TYPE IS DETAIL.
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(16) SOURCE D-Kit-Ref.
         10  COLUMN 18  PIC X(22) SOURCE D-Parent.
         10  COLUMN 41  PIC X(13) SOURCE D-Status.
         10  COLUMN 55  PIC Z(9)9.999 SOURCE D-Issued.
         10  COLUMN 70  PIC Z(9)9.999 SOURCE D-Returned.
         10  COLUMN 85  PIC Z(9)9.999 SOURCE D-Scrapped.
         10  COLUMN 100 PIC Z(9)9.999 SOURCE D-Completed.
         10  COLUMN 115 PIC Z(9)9.99 SOURCE D-Wip.

 01  DT-SHORT TYPE IS DETAIL.
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(16) SOURCE D-Kit-Ref.
         10  COLUMN 18  PIC X(26) SOURCE D-Item.
         10  COLUMN 45  PIC X(6) SOURCE D-Uom.
         10  COLUMN 52  PIC Z(9)9.999 SOURCE D-Required.
         10  COLUMN 67  PIC Z(9)9.999 SOURCE D-Remaining.
         10  COLUMN 82  PIC Z(9)9.999 SOURCE D-Reserved.
         10  COLUMN 97  PIC Z(9)9.999 SOURCE D-Picked.
         10  COLUMN 112 PIC X(10) SOURCE D-Need.
         10  COLUMN 124 PIC X(6) SOURCE D-Status.

 01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
     05  LINE PLUS 2.
         10  COLUMN 1   PIC X(130) VALUE ALL "=".
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(20) VALUE "Kits listed         ".
         10  COLUMN 22  PIC Z(6)9 SOURCE T-Kits.
         10  COLUMN 34  PIC X(20) VALUE "Ready to pick       ".
         10  COLUMN 55  PIC Z(6)9 SOURCE T-Ready.
         10  COLUMN 68  PIC X(20) VALUE "Short lines         ".
         10  COLUMN 89  PIC Z(6)9 SOURCE T-Short-Lines.
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(28) VALUE "Work in progress value GBP  ".
         10  COLUMN 30  PIC Z(11)9.99 SOURCE T-Wip.
     05  LINE PLUS 2.
         10  COLUMN 1   PIC X(120) VALUE
             "Readiness is all or nothing: a kit with one short line builds nothing, so the percentage is advisory only.".
 PROCEDURE DIVISION.
 0000-MAIN SECTION.
 0000-START.
     PERFORM 0100-READ-ARGS
     PERFORM 0200-JOB-START
     PERFORM 0300-OPEN-SPOOL
     IF Ws-Rc > 4
         GO TO 0000-FINISH
     END-IF
     IF Ws-Mode = "SHORT"
         PERFORM 2000-PRINT-SHORTAGES
     ELSE
         PERFORM 1000-PRINT-KITS
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
     MOVE "STATUS" TO CM-LOOKUP-KEY
     PERFORM 0150-GET-ARG
     MOVE CM-LOOKUP-VALUE TO Ws-Status-Arg
     IF CM-ASAT NOT = SPACES
         COMPUTE Ws-Asat = FUNCTION NUMVAL(CM-ASAT)
     END-IF
     IF Ws-Co = SPACES
         MOVE "STC" TO Ws-Co
     END-IF
     IF Ws-User = SPACES
         MOVE "STORES" TO Ws-User
     END-IF
     IF Ws-Mode = SPACES
         MOVE "READY" TO Ws-Mode
     END-IF
     IF Ws-Asat = 0
         MOVE FUNCTION CURRENT-DATE TO Ws-Now
         COMPUTE Ws-Asat = FUNCTION NUMVAL(Ws-Now(1:8))
     END-IF
     DISPLAY "STERP-I-KIT1000 KITRDY " FUNCTION TRIM(Ws-Mode)
             " listing for " FUNCTION TRIM(Ws-Co).
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
     MOVE "KITRDY" TO JL-JOB-NAME
     MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
     CALL "JOBLOG" USING Ws-Joblog.
 0200-EXIT.
     EXIT.

 0300-OPEN-SPOOL SECTION.
 0300-START.
     INITIALIZE Ws-Runpath
     MOVE "BUILD" TO CA-FUNCTION OF Ws-Runpath
     MOVE "SPOOL" TO RP-FOLDER
     MOVE "KITRDY" TO RP-REPORT-ID
     MOVE Ws-Co TO RP-COMPANY-REF
     MOVE "Y" TO RP-USE-TIMESTAMP
     CALL "RUNPATH" USING Ws-Runpath
     IF CA-RETURN-CODE OF Ws-Runpath > 4
         MOVE 12 TO Ws-Rc
         DISPLAY "STERP-E-KIT1002 cannot build the spool path"
         GO TO 0300-EXIT
     END-IF
     MOVE RP-FULL-PATH TO Ws-Spool-Name
     PERFORM 0350-FILL-HEADINGS
     OPEN OUTPUT RPT-FILE
     IF Ws-File-Status NOT = "00"
         MOVE 12 TO Ws-Rc
         DISPLAY "STERP-E-KIT1003 cannot open " Ws-Spool-Name
         GO TO 0300-EXIT
     END-IF
     INITIATE R-KITRDY.
 0300-EXIT.
     EXIT.

 0350-FILL-HEADINGS SECTION.
 0350-START.
     MOVE Ws-Co TO RPT-COMPANY-CODE
     MOVE "KITRDY" TO RPT-REPORT-ID
     MOVE "Kit readiness and execution" TO RPT-TITLE
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
     STRING "As at " Ws-D-Dd "/" Ws-D-Mm "/" Ws-D-Yyyy
         DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
     EVALUATE Ws-Mode
         WHEN "EXEC"
             MOVE "Execution position (RPT-013)"
                  TO H-Mode-Text
         WHEN "SHORT"
             MOVE "Shortage list (RPT-013)" TO H-Mode-Text
         WHEN OTHER
             MOVE "Readiness to pick (RPT-013)" TO H-Mode-Text
     END-EVALUATE
     MOVE H-Mode-Text TO RPT-SUBTITLE
     MOVE SPACES TO H-Scope
     IF Ws-Site = SPACES
         STRING "All sites, open kits" DELIMITED BY SIZE
             INTO H-Scope
     ELSE
         STRING "Site " FUNCTION TRIM(Ws-Site) ", open kits"
             DELIMITED BY SIZE INTO H-Scope
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
     PERFORM 0370-WIDE-COLUMNS
     MOVE 7 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-OK
         MOVE Ws-Col(1) TO RPT-COMPANY-NAME
     END-IF.
 0360-EXIT.
     EXIT.

 0370-WIDE-COLUMNS SECTION.
 0370-START.
     PERFORM VARYING Ws-Ix FROM 1 BY 1 UNTIL Ws-Ix > 20
         MOVE 100 TO SQLIO-COL-WIDTH(Ws-Ix)
     END-PERFORM.
 0370-EXIT.
     EXIT.

 0380-SET-SCOPE-PARMS SECTION.
 0380-START.
     MOVE Ws-Co TO SQLIO-PARM-VAL(1)
     MOVE Ws-Site TO SQLIO-PARM-VAL(2)
     MOVE Ws-Site TO SQLIO-PARM-VAL(3)
     MOVE Ws-Status-Arg TO SQLIO-PARM-VAL(4)
     MOVE Ws-Status-Arg TO SQLIO-PARM-VAL(5)
     PERFORM VARYING Ws-Ix FROM 1 BY 1 UNTIL Ws-Ix > 5
         MOVE "N" TO SQLIO-PARM-NULL(Ws-Ix)
     END-PERFORM
     MOVE 5 TO SQLIO-PARM-CNT.
 0380-EXIT.
     EXIT.

*>   Readiness is counted on the lines, not averaged over the
*>   quantities: a line is covered when nothing is still needed
*>   or the reservation already covers what is left.
 1000-PRINT-KITS SECTION.
 1000-START.
     MOVE SPACES TO SQLIO-STMT
     STRING
       "SELECT k.KitRef, k.ParentItemCode, k.Status,"
       " CONVERT(varchar(26),k.PlannedGoodQty),"
       " CONVERT(varchar(26),k.CompletedGoodQty),"
       " ISNULL(CONVERT(char(8),k.NeedDate,112),''),"
       " CONVERT(varchar(12),COUNT(l.KitLineId)),"
       " CONVERT(varchar(12),SUM(CASE WHEN l.RemainingQty >"
       "   ISNULL(l.ReservedQty,0) THEN 1 ELSE 0 END)),"
       " CONVERT(varchar(26),ISNULL(SUM(l.GrossIssuedQty),0)),"
       " CONVERT(varchar(26),ISNULL(SUM(l.ReturnedQty),0)),"
       " CONVERT(varchar(26),ISNULL(SUM(l.ScrappedQty),0)),"
       " CONVERT(varchar(20),k.WipValueGBP)"
       " FROM kit.ExecutionKit k"
       " LEFT JOIN kit.ExecutionKitLine l"
       "   ON l.ExecutionKitId = k.ExecutionKitId"
       "  AND l.Status <> 'CANCELLED'"
       " WHERE k.CompanyCode = ?"
       " AND (? = '' OR k.SiteCode = ?)"
       " AND (? = '' OR k.Status = ?)"
       " AND k.Status IN ('DRAFT','REVIEWED','RELEASED',"
       "   'PARTISSUED','ISSUED','PARTCOMPLETE','OPCOMPLETE')"
       " GROUP BY k.KitRef, k.ParentItemCode, k.Status,"
       "   k.PlannedGoodQty, k.CompletedGoodQty, k.NeedDate,"
       "   k.WipValueGBP"
       " ORDER BY k.NeedDate, k.KitRef"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM 0380-SET-SCOPE-PARMS
     MOVE 12 TO SQLIO-COL-CNT
     PERFORM 0370-WIDE-COLUMNS
     MOVE 8 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         MOVE 12 TO Ws-Rc
         DISPLAY "STERP-E-KIT1004 " SQLIO-ERRMSG(1:80)
         GO TO 1000-EXIT
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE "FTCH" TO SQLIO-FUNC
         MOVE 8 TO SQLIO-CURSOR
         CALL "SQLIO" USING SQLIO-CA Ws-Row
         EVALUATE TRUE
             WHEN SQLIO-OK
                 PERFORM 1100-PRINT-ONE-KIT
             WHEN SQLIO-NOTFOUND
                 MOVE "N" TO Ws-More
             WHEN OTHER
                 MOVE 12 TO Ws-Rc
                 MOVE "N" TO Ws-More
         END-EVALUATE
     END-PERFORM
     MOVE "CLOS" TO SQLIO-FUNC
     MOVE 8 TO SQLIO-CURSOR
     CALL "SQLIO" USING SQLIO-CA Ws-Row.
 1000-EXIT.
     EXIT.

 1100-PRINT-ONE-KIT SECTION.
 1100-START.
     MOVE Ws-Col(1) TO D-Kit-Ref
     MOVE Ws-Col(2) TO D-Parent
     MOVE Ws-Col(3) TO D-Status
     COMPUTE D-Planned = FUNCTION NUMVAL(Ws-Col(4))
     COMPUTE D-Completed = FUNCTION NUMVAL(Ws-Col(5))
     PERFORM 1200-FORMAT-NEED-DATE
     COMPUTE D-Lines = FUNCTION NUMVAL(Ws-Col(7))
     COMPUTE D-Short = FUNCTION NUMVAL(Ws-Col(8))
     COMPUTE D-Issued = FUNCTION NUMVAL(Ws-Col(9))
     COMPUTE D-Returned = FUNCTION NUMVAL(Ws-Col(10))
     COMPUTE D-Scrapped = FUNCTION NUMVAL(Ws-Col(11))
     COMPUTE D-Wip = FUNCTION NUMVAL(Ws-Col(12))
     IF D-Lines > 0
         COMPUTE D-Ready-Pct ROUNDED =
             (D-Lines - D-Short) * 100 / D-Lines
     ELSE
         MOVE 0 TO D-Ready-Pct
     END-IF
     IF D-Short = 0 AND D-Lines > 0
         MOVE "YES" TO D-Ready-Flag
         ADD 1 TO T-Ready
     ELSE
         MOVE "NO" TO D-Ready-Flag
     END-IF
     ADD 1 TO T-Kits
     ADD D-Short TO T-Short-Lines
     ADD D-Wip TO T-Wip
     IF Ws-Mode = "EXEC"
         GENERATE DT-EXEC
     ELSE
         GENERATE DT-KIT
     END-IF.
 1100-EXIT.
     EXIT.

 1200-FORMAT-NEED-DATE SECTION.
 1200-START.
     MOVE SPACES TO D-Need
     IF Ws-Col(6)(1:8) NUMERIC
         MOVE Ws-Col(6)(1:8) TO Ws-Date-N
         STRING Ws-D-Dd "/" Ws-D-Mm "/" Ws-D-Yyyy
             DELIMITED BY SIZE INTO D-Need
     END-IF.
 1200-EXIT.
     EXIT.

*>   The shortage list.  One line per material line that still
*>   needs something, because that is the list a storeman works
*>   from - a kit level summary cannot be picked.
 2000-PRINT-SHORTAGES SECTION.
 2000-START.
     MOVE SPACES TO SQLIO-STMT
     STRING
       "SELECT k.KitRef, l.ComponentItemCode,"
       " ISNULL(l.UomCode,''),"
       " CONVERT(varchar(26),l.CurrentRequirementQty),"
       " CONVERT(varchar(26),l.RemainingQty),"
       " CONVERT(varchar(26),ISNULL(l.ReservedQty,0)),"
       " CONVERT(varchar(26),ISNULL(l.PickedQty,0)),"
       " ISNULL(CONVERT(char(8),ISNULL(l.NeedDate,k.NeedDate),112),''),"
       " l.Status"
       " FROM kit.ExecutionKitLine l"
       " INNER JOIN kit.ExecutionKit k"
       "   ON k.ExecutionKitId = l.ExecutionKitId"
       " WHERE k.CompanyCode = ?"
       " AND (? = '' OR k.SiteCode = ?)"
       " AND (? = '' OR k.Status = ?)"
       " AND k.Status IN ('RELEASED','PARTISSUED','ISSUED',"
       "   'PARTCOMPLETE')"
       " AND l.Status <> 'CANCELLED'"
       " AND l.RemainingQty > ISNULL(l.ReservedQty,0)"
       " ORDER BY ISNULL(l.NeedDate,k.NeedDate), k.KitRef,"
       "   l.LineNum"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM 0380-SET-SCOPE-PARMS
     MOVE 9 TO SQLIO-COL-CNT
     PERFORM 0370-WIDE-COLUMNS
     MOVE 8 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         MOVE 12 TO Ws-Rc
         DISPLAY "STERP-E-KIT1005 " SQLIO-ERRMSG(1:80)
         GO TO 2000-EXIT
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE "FTCH" TO SQLIO-FUNC
         MOVE 8 TO SQLIO-CURSOR
         CALL "SQLIO" USING SQLIO-CA Ws-Row
         EVALUATE TRUE
             WHEN SQLIO-OK
                 PERFORM 2100-PRINT-ONE-SHORTAGE
             WHEN SQLIO-NOTFOUND
                 MOVE "N" TO Ws-More
             WHEN OTHER
                 MOVE 12 TO Ws-Rc
                 MOVE "N" TO Ws-More
         END-EVALUATE
     END-PERFORM
     MOVE "CLOS" TO SQLIO-FUNC
     MOVE 8 TO SQLIO-CURSOR
     CALL "SQLIO" USING SQLIO-CA Ws-Row.
 2000-EXIT.
     EXIT.

 2100-PRINT-ONE-SHORTAGE SECTION.
 2100-START.
     MOVE Ws-Col(1) TO D-Kit-Ref
     MOVE Ws-Col(2) TO D-Item
     MOVE Ws-Col(3) TO D-Uom
     COMPUTE D-Required = FUNCTION NUMVAL(Ws-Col(4))
     COMPUTE D-Remaining = FUNCTION NUMVAL(Ws-Col(5))
     COMPUTE D-Reserved = FUNCTION NUMVAL(Ws-Col(6))
     COMPUTE D-Picked = FUNCTION NUMVAL(Ws-Col(7))
     MOVE SPACES TO D-Need
     IF Ws-Col(8)(1:8) NUMERIC
         MOVE Ws-Col(8)(1:8) TO Ws-Date-N
         STRING Ws-D-Dd "/" Ws-D-Mm "/" Ws-D-Yyyy
             DELIMITED BY SIZE INTO D-Need
     END-IF
     MOVE Ws-Col(9) TO D-Status
     ADD 1 TO T-Short-Lines
     GENERATE DT-SHORT.
 2100-EXIT.
     EXIT.

 0400-CLOSE-SPOOL SECTION.
 0400-START.
     TERMINATE R-KITRDY
     CLOSE RPT-FILE
     DISPLAY "STERP-I-KIT1001 listing written to "
             FUNCTION TRIM(Ws-Spool-Name).
 0400-EXIT.
     EXIT.

 0500-JOB-END SECTION.
 0500-START.
     INITIALIZE Ws-Joblog
     MOVE "END" TO CA-FUNCTION OF Ws-Joblog
     MOVE Ws-Co TO CA-COMPANY OF Ws-Joblog
     MOVE Ws-User TO CA-USER OF Ws-Joblog
     MOVE "KITRDY" TO JL-JOB-NAME
     MOVE T-Kits TO JL-ROWS-PROCESSED
     MOVE Ws-Rc TO JL-RETURN-CODE
     MOVE Ws-Spool-Name TO JL-SPOOL-FILE
     CALL "JOBLOG" USING Ws-Joblog.
 0500-EXIT.
     EXIT.
