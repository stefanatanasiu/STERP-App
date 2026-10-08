>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : KITDOCP
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : Kit document print and reprint (RPT-014).
*>
*>              Prints the paperwork that goes with ONE kit: the
*>              header, the material lines with their issue
*>              location and whether the component must be
*>              identified by lot or serial when it is picked.
*>
*>              The document is deliberately NOT an issue note.
*>              KIT-022 asks for kit documents to be reprintable
*>              and for a reprint to be distinguishable from the
*>              original, because a second copy of a picking list
*>              on the shop floor is how material gets issued
*>              twice.  So:
*>
*>                * MODE=REPRINT stamps REPRINT in the heading,
*>                  prints the copy number and the reason, and
*>                  bands the foot of every page with a warning;
*>                * the program never writes to kit.KitIssue,
*>                  kit.KitPick, inv or the ledger - it reads and
*>                  prints, nothing else;
*>                * the footing says in plain words that the paper
*>                  moves no stock.
*>
*> ARGUMENTS   : CO=       company code          (default STC)
*>               SITE=     site filter           (optional)
*>               USER=     operator              (default STORES)
*>               KIT=      kit reference         (REQUIRED)
*>               MODE=     ORIGINAL | REPRINT    (default ORIGINAL)
*>               COPY=     copy number for a reprint (default 2)
*>               REASON=   why it is being reprinted
*>
*> CALLS       : CMDARGS, RUNPATH, JOBLOG, SQLIO
*> OUTPUT      : spool file SPOOL\KITDOCP_<CO>_<timestamp>.txt
*>----------------------------------------------------------------
*> CHANGE LOG
*> 1996-04-22 DRN  ORIGINAL - KIT PICKING DOCUMENT
*> 2004-11-08 KMP  CR0912 REPRINT MARKER AFTER STOCK LOSS AUDIT
*> 2026-10-06 STERP KIT-022, RPT-014
*>================================================================
*> REQ: KIT-022
 IDENTIFICATION DIVISION.
 PROGRAM-ID. KITDOCP.
 AUTHOR. D R NEWELL.
 DATE-WRITTEN. 22/04/1996.
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
 FD  RPT-FILE REPORT IS R-KITDOCP.
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
     05  Ws-Kit-Ref             PIC X(30) VALUE SPACES.
     05  Ws-Copy-No             PIC S9(4) VALUE 0.
     05  Ws-Reason              PIC X(60) VALUE SPACES.
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
     05  Ws-Found               PIC X VALUE "N".
 01  Ws-Hdr.
     05  H-Kit-Ref              PIC X(30) VALUE SPACES.
     05  H-Parent               PIC X(40) VALUE SPACES.
     05  H-Parent-Desc          PIC X(40) VALUE SPACES.
     05  H-Status               PIC X(14) VALUE SPACES.
     05  H-Site                 PIC X(10) VALUE SPACES.
     05  H-Planned              PIC S9(11)V9(3) VALUE 0.
     05  H-Completed            PIC S9(11)V9(3) VALUE 0.
     05  H-Need                 PIC X(10) VALUE SPACES.
     05  H-Source               PIC X(40) VALUE SPACES.
     05  H-Owner                PIC X(20) VALUE SPACES.
     05  H-Backflush            PIC X(3) VALUE SPACES.
     05  H-Doc-Kind             PIC X(28) VALUE SPACES.
     05  H-Copy-Text            PIC X(60) VALUE SPACES.
 01  Ws-Det.
     05  D-Line                 PIC S9(5) VALUE 0.
     05  D-Item                 PIC X(26) VALUE SPACES.
     05  D-Desc                 PIC X(28) VALUE SPACES.
     05  D-Uom                  PIC X(6) VALUE SPACES.
     05  D-Required             PIC S9(11)V9(3) VALUE 0.
     05  D-Remaining            PIC S9(11)V9(3) VALUE 0.
     05  D-Reserved             PIC S9(11)V9(3) VALUE 0.
     05  D-Picked               PIC S9(11)V9(3) VALUE 0.
     05  D-Location             PIC X(14) VALUE SPACES.
     05  D-Trace                PIC X(8) VALUE SPACES.
     05  D-Status               PIC X(11) VALUE SPACES.
 01  Ws-Tot.
     05  T-Lines                PIC S9(7) VALUE 0.
     05  T-Traced               PIC S9(7) VALUE 0.
     05  T-Outstanding          PIC S9(13)V9(3) VALUE 0.
 REPORT SECTION.
 RD  R-KITDOCP
     CONTROLS ARE FINAL
     PAGE LIMIT 60 LINES
     HEADING 1 FIRST DETAIL 6 LAST DETAIL 52
     FOOTING 54.
 COPY STRPTHDR.

*>   The kit header block, then the warning band.  A reprint says
*>   so three times: in the document kind, in the copy line and in
*>   the band, because one of them will be torn off.
*>   It is a DETAIL that the program generates itself: this
*>   compiler does not emit CONTROL HEADING FINAL for a report
*>   whose only control is FINAL, and a picking list without its
*>   header is worse than no picking list at all.
 01  DT-HEAD TYPE IS DETAIL.
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(14) VALUE "Document      ".
         10  COLUMN 16  PIC X(28) SOURCE H-Doc-Kind.
         10  COLUMN 48  PIC X(14) VALUE "Kit reference ".
         10  COLUMN 63  PIC X(30) SOURCE H-Kit-Ref.
         10  COLUMN 96  PIC X(8) VALUE "Status  ".
         10  COLUMN 105 PIC X(14) SOURCE H-Status.
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(14) VALUE "Parent item   ".
         10  COLUMN 16  PIC X(28) SOURCE H-Parent.
         10  COLUMN 48  PIC X(14) VALUE "Description   ".
         10  COLUMN 63  PIC X(40) SOURCE H-Parent-Desc.
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(14) VALUE "Planned qty   ".
         10  COLUMN 16  PIC Z(9)9.999 SOURCE H-Planned.
         10  COLUMN 48  PIC X(14) VALUE "Completed qty ".
         10  COLUMN 63  PIC Z(9)9.999 SOURCE H-Completed.
         10  COLUMN 96  PIC X(8) VALUE "Need by ".
         10  COLUMN 105 PIC X(10) SOURCE H-Need.
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(14) VALUE "Site          ".
         10  COLUMN 16  PIC X(10) SOURCE H-Site.
         10  COLUMN 48  PIC X(14) VALUE "Raised for    ".
         10  COLUMN 63  PIC X(40) SOURCE H-Source.
         10  COLUMN 105 PIC X(10) VALUE "Backflush ".
         10  COLUMN 116 PIC X(3) SOURCE H-Backflush.
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(14) VALUE "Kit owner     ".
         10  COLUMN 16  PIC X(20) SOURCE H-Owner.
         10  COLUMN 48  PIC X(60) SOURCE H-Copy-Text.
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(130) VALUE ALL "=".
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(5) VALUE " Line".
         10  COLUMN 8   PIC X(26) VALUE "Component item            ".
         10  COLUMN 35  PIC X(28) VALUE "Description                 ".
         10  COLUMN 64  PIC X(6) VALUE "UOM   ".
         10  COLUMN 71  PIC X(14) VALUE "      Required".
         10  COLUMN 86  PIC X(14) VALUE "   Outstanding".
         10  COLUMN 101 PIC X(10) VALUE "  Reserved".
         10  COLUMN 112 PIC X(8) VALUE "Identify".
         10  COLUMN 121 PIC X(10) VALUE "Line state".
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(130) VALUE ALL "-".

 01  DT-LINE TYPE IS DETAIL.
     05  LINE PLUS 1.
         10  COLUMN 1   PIC Z(4)9 SOURCE D-Line.
         10  COLUMN 8   PIC X(26) SOURCE D-Item.
         10  COLUMN 35  PIC X(28) SOURCE D-Desc.
         10  COLUMN 64  PIC X(6) SOURCE D-Uom.
         10  COLUMN 71  PIC Z(9)9.999 SOURCE D-Required.
         10  COLUMN 86  PIC Z(9)9.999 SOURCE D-Remaining.
         10  COLUMN 101 PIC Z(5)9.999 SOURCE D-Reserved.
         10  COLUMN 112 PIC X(8) SOURCE D-Trace.
         10  COLUMN 121 PIC X(11) SOURCE D-Status.
     05  LINE PLUS 1.
         10  COLUMN 8   PIC X(14) VALUE "Pick from     ".
         10  COLUMN 23  PIC X(14) SOURCE D-Location.
         10  COLUMN 40  PIC X(14) VALUE "Already picked".
         10  COLUMN 55  PIC Z(5)9.999 SOURCE D-Picked.
         10  COLUMN 71  PIC X(44) VALUE
             "Picked ......... Checked by ................".

 01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(130) VALUE ALL "-".
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(24) VALUE "Material lines printed  ".
         10  COLUMN 26  PIC Z(6)9 SOURCE T-Lines.
         10  COLUMN 40  PIC X(30) VALUE "Lines needing lot or serial   ".
         10  COLUMN 71  PIC Z(6)9 SOURCE T-Traced.
         10  COLUMN 86  PIC X(18) VALUE "Outstanding qty   ".
         10  COLUMN 105 PIC Z(11)9.999 SOURCE T-Outstanding.
     05  LINE PLUS 2.
         10  COLUMN 1   PIC X(118) VALUE
             "This document moves no stock.  Material is only issued when KITISSUE posts it, and only against the reservation.".
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(60) SOURCE H-Copy-Text.
     05  LINE PLUS 2.
         10  COLUMN 1   PIC X(40) VALUE
             "Issued by ...............................".
         10  COLUMN 48  PIC X(40) VALUE
             "Received by .............................".
         10  COLUMN 96  PIC X(28) VALUE "Date ....../....../........".

 PROCEDURE DIVISION.
 0000-MAIN SECTION.
 0000-START.
     PERFORM 0100-READ-ARGS
     PERFORM 0200-JOB-START
     IF Ws-Rc > 4
         GO TO 0000-FINISH
     END-IF
     PERFORM 0300-OPEN-SPOOL
     IF Ws-Rc > 4
         GO TO 0000-FINISH
     END-IF
     PERFORM 1000-PRINT-HEADER
     IF Ws-Found = "Y"
         GENERATE DT-HEAD
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
     MOVE "KIT" TO CM-LOOKUP-KEY
     PERFORM 0150-GET-ARG
     MOVE CM-LOOKUP-VALUE TO Ws-Kit-Ref
     MOVE "COPY" TO CM-LOOKUP-KEY
     PERFORM 0150-GET-ARG
     IF CM-LOOKUP-VALUE NOT = SPACES
         COMPUTE Ws-Copy-No = FUNCTION NUMVAL(CM-LOOKUP-VALUE)
     END-IF
     MOVE "REASON" TO CM-LOOKUP-KEY
     PERFORM 0150-GET-ARG
     MOVE CM-LOOKUP-VALUE TO Ws-Reason
     IF Ws-Co = SPACES
         MOVE "STC" TO Ws-Co
     END-IF
     IF Ws-User = SPACES
         MOVE "STORES" TO Ws-User
     END-IF
     IF Ws-Mode = SPACES
         MOVE "ORIGINAL" TO Ws-Mode
     END-IF
     PERFORM 0180-DESCRIBE-DOCUMENT
     IF Ws-Kit-Ref = SPACES
         MOVE 12 TO Ws-Rc
         DISPLAY "STERP-E-KIT1010 KIT= is required - "
                 "a kit document is printed for one kit"
     ELSE
         DISPLAY "STERP-I-KIT1011 KITDOCP "
                 FUNCTION TRIM(H-Doc-Kind) " for "
                 FUNCTION TRIM(Ws-Kit-Ref)
     END-IF.
 0100-EXIT.
     EXIT.

*>   A reprint is never allowed to look like an original.  If the
*>   operator forgot the copy number it is assumed to be the
*>   second copy, which is still true enough to be a warning.
 0180-DESCRIBE-DOCUMENT SECTION.
 0180-START.
     MOVE SPACES TO H-Copy-Text
     IF Ws-Mode = "REPRINT" OR Ws-Mode = "COPY"
         MOVE "REPRINT - PICKING LIST COPY" TO H-Doc-Kind
         IF Ws-Copy-No < 2
             MOVE 2 TO Ws-Copy-No
         END-IF
         IF Ws-Reason = SPACES
             MOVE "not stated" TO Ws-Reason
         END-IF
         STRING "*** REPRINT copy " Ws-Copy-No
                " - reason: " FUNCTION TRIM(Ws-Reason) " ***"
             DELIMITED BY SIZE INTO H-Copy-Text
     ELSE
         MOVE "ORIGINAL PICKING LIST" TO H-Doc-Kind
         MOVE 1 TO Ws-Copy-No
         MOVE "Original issue of this document." TO H-Copy-Text
     END-IF.
 0180-EXIT.
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
     MOVE "KITDOCP" TO JL-JOB-NAME
     MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
     CALL "JOBLOG" USING Ws-Joblog.
 0200-EXIT.
     EXIT.

 0300-OPEN-SPOOL SECTION.
 0300-START.
     INITIALIZE Ws-Runpath
     MOVE "BUILD" TO CA-FUNCTION OF Ws-Runpath
     MOVE "SPOOL" TO RP-FOLDER
     MOVE "KITDOCP" TO RP-REPORT-ID
     MOVE Ws-Co TO RP-COMPANY-REF
     MOVE "Y" TO RP-USE-TIMESTAMP
     CALL "RUNPATH" USING Ws-Runpath
     IF CA-RETURN-CODE OF Ws-Runpath > 4
         MOVE 12 TO Ws-Rc
         DISPLAY "STERP-E-KIT1012 cannot build the spool path"
         GO TO 0300-EXIT
     END-IF
     MOVE RP-FULL-PATH TO Ws-Spool-Name
     PERFORM 0350-FILL-HEADINGS
     OPEN OUTPUT RPT-FILE
     IF Ws-File-Status NOT = "00"
         MOVE 12 TO Ws-Rc
         DISPLAY "STERP-E-KIT1013 cannot open " Ws-Spool-Name
         GO TO 0300-EXIT
     END-IF
     INITIATE R-KITDOCP.
 0300-EXIT.
     EXIT.

 0350-FILL-HEADINGS SECTION.
 0350-START.
     MOVE Ws-Co TO RPT-COMPANY-CODE
     MOVE "KITDOCP" TO RPT-REPORT-ID
     IF Ws-Mode = "REPRINT" OR Ws-Mode = "COPY"
         MOVE "Kit picking document - REPRINT" TO RPT-TITLE
     ELSE
         MOVE "Kit picking document" TO RPT-TITLE
     END-IF
     MOVE Ws-User TO RPT-USER
     MOVE FUNCTION CURRENT-DATE TO Ws-Now
     COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(Ws-Now(1:4))
     COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(Ws-Now(5:2))
     COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(Ws-Now(7:2))
     COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(Ws-Now(9:2))
     COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(Ws-Now(11:2))
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

*>   One kit, read once.  A site may be given as a safety net so
*>   that a Reading storeman cannot print Coventry's paperwork by
*>   typing the wrong reference.
 1000-PRINT-HEADER SECTION.
 1000-START.
     MOVE "N" TO Ws-Found
     MOVE SPACES TO SQLIO-STMT
     STRING
       "SELECT k.ParentItemCode, ISNULL(i.Description,''),"
       " k.Status, k.SiteCode,"
       " CONVERT(varchar(26),k.PlannedGoodQty),"
       " CONVERT(varchar(26),k.CompletedGoodQty),"
       " ISNULL(CONVERT(char(8),k.NeedDate,112),''),"
       " ISNULL(k.SourceDemandType,'') + ' '"
       "   + ISNULL(k.SourceDemandRef,''),"
       " ISNULL(k.OwnerUser,''),"
       " CASE WHEN k.IsBackflush = 1 THEN 'Yes' ELSE 'No' END"
       " FROM kit.ExecutionKit k"
       " LEFT JOIN inv.Item i ON i.CompanyCode = k.CompanyCode"
       "   AND i.ItemCode = k.ParentItemCode"
       " WHERE k.CompanyCode = ? AND k.KitRef = ?"
       " AND (? = '' OR k.SiteCode = ?)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE Ws-Co TO SQLIO-PARM-VAL(1)
     MOVE Ws-Kit-Ref TO SQLIO-PARM-VAL(2)
     MOVE Ws-Site TO SQLIO-PARM-VAL(3)
     MOVE Ws-Site TO SQLIO-PARM-VAL(4)
     PERFORM VARYING Ws-Ix FROM 1 BY 1 UNTIL Ws-Ix > 4
         MOVE "N" TO SQLIO-PARM-NULL(Ws-Ix)
     END-PERFORM
     MOVE 4 TO SQLIO-PARM-CNT
     MOVE 10 TO SQLIO-COL-CNT
     PERFORM 0370-WIDE-COLUMNS
     MOVE 8 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     EVALUATE TRUE
         WHEN SQLIO-OK
             MOVE "Y" TO Ws-Found
         WHEN SQLIO-NOTFOUND
             MOVE 8 TO Ws-Rc
             DISPLAY "STERP-W-KIT1014 kit "
                     FUNCTION TRIM(Ws-Kit-Ref)
                     " is not known in " FUNCTION TRIM(Ws-Co)
             GO TO 1000-EXIT
         WHEN OTHER
             MOVE 12 TO Ws-Rc
             DISPLAY "STERP-E-KIT1015 " SQLIO-ERRMSG(1:80)
             GO TO 1000-EXIT
     END-EVALUATE
     MOVE Ws-Kit-Ref TO H-Kit-Ref
     MOVE Ws-Col(1) TO H-Parent
     MOVE Ws-Col(2) TO H-Parent-Desc
     MOVE Ws-Col(3) TO H-Status
     MOVE Ws-Col(4) TO H-Site
     COMPUTE H-Planned = FUNCTION NUMVAL(Ws-Col(5))
     COMPUTE H-Completed = FUNCTION NUMVAL(Ws-Col(6))
     MOVE SPACES TO H-Need
     IF Ws-Col(7)(1:8) NUMERIC
         MOVE Ws-Col(7)(1:8) TO Ws-Date-N
         STRING Ws-D-Dd "/" Ws-D-Mm "/" Ws-D-Yyyy
             DELIMITED BY SIZE INTO H-Need
     END-IF
     MOVE Ws-Col(8) TO H-Source
     MOVE Ws-Col(9) TO H-Owner
     MOVE Ws-Col(10) TO H-Backflush.
 1000-EXIT.
     EXIT.

*>   The material lines.  Cancelled lines are not printed: they
*>   are not to be picked and a line on the paper is an
*>   instruction, not a history.
 2000-PRINT-LINES SECTION.
 2000-START.
     MOVE SPACES TO SQLIO-STMT
     STRING
       "SELECT CONVERT(varchar(12),l.LineNum),"
       " l.ComponentItemCode, ISNULL(i.Description,''),"
       " l.UomCode,"
       " CONVERT(varchar(26),l.CurrentRequirementQty),"
       " CONVERT(varchar(26),l.RemainingQty),"
       " CONVERT(varchar(26),l.ReservedQty),"
       " CONVERT(varchar(26),l.PickedQty),"
       " ISNULL(l.IssueLocationCode,''),"
       " ISNULL(i.TraceabilityType,'NONE'), l.Status"
       " FROM kit.ExecutionKitLine l"
       " JOIN kit.ExecutionKit k"
       "   ON k.ExecutionKitId = l.ExecutionKitId"
       " LEFT JOIN inv.Item i ON i.CompanyCode = k.CompanyCode"
       "   AND i.ItemCode = l.ComponentItemCode"
       " WHERE k.CompanyCode = ? AND k.KitRef = ?"
       "   AND l.Status <> 'CANCELLED'"
       " ORDER BY l.LineNum"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE Ws-Co TO SQLIO-PARM-VAL(1)
     MOVE Ws-Kit-Ref TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE 2 TO SQLIO-PARM-CNT
     MOVE 11 TO SQLIO-COL-CNT
     PERFORM 0370-WIDE-COLUMNS
     MOVE 8 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         MOVE 12 TO Ws-Rc
         DISPLAY "STERP-E-KIT1016 " SQLIO-ERRMSG(1:80)
         GO TO 2000-EXIT
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE "FTCH" TO SQLIO-FUNC
         MOVE 8 TO SQLIO-CURSOR
         CALL "SQLIO" USING SQLIO-CA Ws-Row
         EVALUATE TRUE
             WHEN SQLIO-OK
                 PERFORM 2100-PRINT-ONE-LINE
             WHEN SQLIO-NOTFOUND
                 MOVE "N" TO Ws-More
             WHEN OTHER
                 MOVE 12 TO Ws-Rc
                 MOVE "N" TO Ws-More
         END-EVALUATE
     END-PERFORM
     MOVE "CLOS" TO SQLIO-FUNC
     MOVE 8 TO SQLIO-CURSOR
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF T-Lines = 0
         DISPLAY "STERP-W-KIT1017 kit "
                 FUNCTION TRIM(Ws-Kit-Ref)
                 " has no material lines to pick"
     END-IF.
 2000-EXIT.
     EXIT.

*>   D-20: a backflushed component is never lot or serial traced,
*>   so the identify column doubles as the reason a line cannot be
*>   backflushed and must be picked by hand.
 2100-PRINT-ONE-LINE SECTION.
 2100-START.
     COMPUTE D-Line = FUNCTION NUMVAL(Ws-Col(1))
     MOVE Ws-Col(2) TO D-Item
     MOVE Ws-Col(3) TO D-Desc
     MOVE Ws-Col(4) TO D-Uom
     COMPUTE D-Required = FUNCTION NUMVAL(Ws-Col(5))
     COMPUTE D-Remaining = FUNCTION NUMVAL(Ws-Col(6))
     COMPUTE D-Reserved = FUNCTION NUMVAL(Ws-Col(7))
     COMPUTE D-Picked = FUNCTION NUMVAL(Ws-Col(8))
     MOVE Ws-Col(9) TO D-Location
     IF D-Location = SPACES
         MOVE "any" TO D-Location
     END-IF
     EVALUATE Ws-Col(10)(1:6)
         WHEN "LOT   " MOVE "Lot" TO D-Trace
         WHEN "SERIAL" MOVE "Serial" TO D-Trace
         WHEN "BOTH  " MOVE "Lot+Ser" TO D-Trace
         WHEN OTHER    MOVE "-" TO D-Trace
     END-EVALUATE
     IF D-Trace NOT = "-"
         ADD 1 TO T-Traced
     END-IF
     MOVE Ws-Col(11) TO D-Status
     ADD 1 TO T-Lines
     ADD D-Remaining TO T-Outstanding
     GENERATE DT-LINE.
 2100-EXIT.
     EXIT.

 0400-CLOSE-SPOOL SECTION.
 0400-START.
     TERMINATE R-KITDOCP
     CLOSE RPT-FILE
     DISPLAY "STERP-I-KIT1018 "
             FUNCTION TRIM(H-Doc-Kind)
             " written to " FUNCTION TRIM(Ws-Spool-Name).
 0400-EXIT.
     EXIT.

 0500-JOB-END SECTION.
 0500-START.
     INITIALIZE Ws-Joblog
     MOVE "END" TO CA-FUNCTION OF Ws-Joblog
     MOVE Ws-Co TO CA-COMPANY OF Ws-Joblog
     MOVE Ws-User TO CA-USER OF Ws-Joblog
     MOVE "KITDOCP" TO JL-JOB-NAME
     MOVE T-Lines TO JL-ROWS-PROCESSED
     MOVE Ws-Rc TO JL-RETURN-CODE
     MOVE Ws-Spool-Name TO JL-SPOOL-FILE
     CALL "JOBLOG" USING Ws-Joblog.
 0500-EXIT.
     EXIT.
