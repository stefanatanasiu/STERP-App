>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : BOMRP030
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : TWO ENGINEERING CHANGE LISTINGS ON ONE 132 COLUMN
*>              REPORT PROGRAM, SELECTED BY MODE=.
*>
*>              MODE=WHEREUSED (REPORT BOMRP030) ANSWERS "WHAT
*>              WOULD A CHANGE TO THIS PART AFFECT?" IN THE THREE
*>              SECTIONS BOM-023 ASKS FOR: EVERY DIRECT AND
*>              INDIRECT PARENT WITH ITS LEVEL, THE QUANTITY USED
*>              PER PARENT, THE BILL AND REVISION THAT CARRIES THE
*>              USE AND THE PATH BACK TO THE PART; THE TOP LEVEL
*>              ASSEMBLIES EACH PATH ENDS AT; AND THE OPEN
*>              EXECUTION DEMAND THAT WOULD HAVE TO BE DECIDED
*>              (BOM-031).  A USE THAT NO PERMITTED CONFIGURATION
*>              COULD ACTIVATE PRINTS AS POSSIBLE AND IS COUNTED
*>              APART FROM CONFIRMED USAGE.
*>
*>              MODE=COMPARE (REPORT BOMRP040) PRINTS WHAT CHANGED
*>              BETWEEN TWO REVISIONS OF ONE BILL - ADDED, REMOVED
*>              AND CHANGED COMPONENTS WITH THE QUANTITY, UNIT,
*>              BASIS AND SCRAP DIFFERENCE AND THE CALCULATED COST
*>              IMPACT (BOM-025).  A LINE THAT ONLY MOVED IN THE
*>              SEQUENCE IS REPORTED AS SEQUENCE AND NOT AS A
*>              MATERIAL CHANGE.
*>
*>              LIKE BOMRP010 THE REPORT CALCULATES NOTHING: IT
*>              ASKS WHEREUSD AND BOMCOMP, THE SAME PROGRAMS THE
*>              ENQUIRY SCREENS ASK, SO PAPER AND SCREEN CANNOT
*>              DISAGREE (BOM-032).
*> RUN AS     : BOMRP030 CO=STC ITEM=xxx [ASAT=yyyymmdd]
*>                       [LEVELS=n] MODE=WHEREUSED [DRAFT=Y]
*>                       [USER=ENGINEER]
*>              BOMRP030 CO=STC BOMREF=xxx FROMREV=A TOREV=B
*>                       [COSTSET=STD2026] [QTY=n] MODE=COMPARE
*>                       [USER=ENGINEER]
*> OUTPUT     : run\spool\BOMRP030-<CO>-<stamp>.lst
*> CALLS      : CMDARGS, RUNPATH, JOBLOG, EXCPRAIS, SQLIO,
*>              WHEREUSD, BOMCOMP
*>----------------------------------------------------------------
*> CHANGE LOG
*> 1999-06-14 RJH  CR0894 ORIGINAL - WHERE USED LISTING
*> 2015-10-12 AJS  CR2371 REVISION COMPARISON ADDED AS MODE=COMPARE
*> 2016-05-23 AJS  CR2466 OPEN EXECUTION DEMAND SECTION
*> 2026-10-06 STERP POSSIBLE VERSUS CONFIRMED USE COUNTED APART
*>================================================================
*> REQ: BOM-023, BOM-025, BOM-028, BOM-029, BOM-031, BOM-032
 IDENTIFICATION DIVISION.
 PROGRAM-ID. BOMRP030.
 AUTHOR. R J HOLLIS.
 DATE-WRITTEN. 14/06/1999.
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
 FD  RPT-FILE REPORT IS R-BOMCHG.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY STRPTWS.
 COPY STBATLOG.
 COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==Ws-Cmdargs==.
 COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==Ws-Runpath==.
 COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==Ws-Joblog==.
 COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==Ws-Excp==
      LEADING ==CA-== BY ==XA-==.
 COPY WHEREUSDCA REPLACING ==DFHCOMMAREA== BY ==Ws-Wu-Ca==
      LEADING ==CA-== BY ==UA-==.
 COPY BOMCOMPCA REPLACING ==DFHCOMMAREA== BY ==Ws-Cp-Ca==
      LEADING ==CA-== BY ==PA-==.
 01  Ws-Spool-Name               PIC X(200) VALUE SPACES.
 01  Ws-File-Status              PIC X(2) VALUE "00".
 01  Ws-Row.
     05  Ws-Col OCCURS 4 TIMES   PIC X(100).
 01  Ws-Args.
     05  Ws-Co                   PIC X(10) VALUE SPACES.
     05  Ws-Site                 PIC X(10) VALUE SPACES.
     05  Ws-Item                 PIC X(40) VALUE SPACES.
     05  Ws-User                 PIC X(20) VALUE SPACES.
     05  Ws-Mode                 PIC X(10) VALUE SPACES.
     05  Ws-Bom-Ref              PIC X(30) VALUE SPACES.
     05  Ws-From-Rev             PIC X(4) VALUE SPACES.
     05  Ws-To-Rev               PIC X(4) VALUE SPACES.
     05  Ws-Cost-Set             PIC X(20) VALUE SPACES.
     05  Ws-Draft                PIC X VALUE "N".
     05  Ws-Asat                 PIC 9(8) VALUE 0.
     05  Ws-Levels               PIC S9(4) VALUE 0.
     05  Ws-Qty                  PIC S9(13)V9(6) VALUE 0.
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
     05  Ws-Compare              PIC X VALUE "N".
     05  Ws-Section-Rows         PIC S9(9) VALUE 0.
     05  Ws-Money-Ed             PIC -(9)9.99.
     05  Ws-Qty-Ed               PIC -(5)9.9(6).
 01  Ws-Caption.
     05  D-Cap-1                 PIC X(66) VALUE SPACES.
     05  D-Cap-2                 PIC X(66) VALUE SPACES.
 01  Ws-Section.
     05  D-Sec-1                 PIC X(66) VALUE SPACES.
     05  D-Sec-2                 PIC X(66) VALUE SPACES.
 01  Ws-Det.
     05  D-Level                 PIC S9(4) VALUE 0.
     05  D-Parent                PIC X(32) VALUE SPACES.
     05  D-Parent-Rev            PIC X(4) VALUE SPACES.
     05  D-Desc                  PIC X(20) VALUE SPACES.
     05  D-Bom-Ref               PIC X(22) VALUE SPACES.
     05  D-Bom-Rev               PIC X(4) VALUE SPACES.
     05  D-Status                PIC X(12) VALUE SPACES.
     05  D-Line-Num              PIC S9(4) VALUE 0.
     05  D-Usage                 PIC S9(11)V9(6) VALUE 0.
     05  D-Uom                   PIC X(8) VALUE SPACES.
     05  D-Path                  PIC X(70) VALUE SPACES.
     05  D-Basis                 PIC X(12) VALUE SPACES.
     05  D-Cert                  PIC X(10) VALUE SPACES.
     05  D-Demand                PIC X(24) VALUE SPACES.
     05  D-Action                PIC X(8) VALUE SPACES.
     05  D-Item                  PIC X(28) VALUE SPACES.
     05  D-From-Line             PIC S9(4) VALUE 0.
     05  D-To-Line               PIC S9(4) VALUE 0.
     05  D-From-Qty              PIC S9(11)V9(6) VALUE 0.
     05  D-To-Qty                PIC S9(11)V9(6) VALUE 0.
     05  D-Delta                 PIC S9(11)V9(6) VALUE 0.
     05  D-Detail                PIC X(100) VALUE SPACES.
 01  Ws-Head.
     05  H-Subject-Lab           PIC X(16) VALUE SPACES.
     05  H-Subject               PIC X(44) VALUE SPACES.
     05  H-Context               PIC X(70) VALUE SPACES.
     05  H-Line2.
         10  H-Line2-1           PIC X(66) VALUE SPACES.
         10  H-Line2-2           PIC X(66) VALUE SPACES.
     05  H-Complete              PIC X(12) VALUE SPACES.
     05  H-Stop                  PIC X(70) VALUE SPACES.
 01  Ws-Foot.
     05  F-1.
         10  F-1-1               PIC X(66) VALUE SPACES.
         10  F-1-2               PIC X(66) VALUE SPACES.
     05  F-2.
         10  F-2-1               PIC X(66) VALUE SPACES.
         10  F-2-2               PIC X(66) VALUE SPACES.
     05  F-3.
         10  F-3-1               PIC X(66) VALUE SPACES.
         10  F-3-2               PIC X(66) VALUE SPACES.
     05  F-4.
         10  F-4-1               PIC X(66) VALUE SPACES.
         10  F-4-2               PIC X(66) VALUE SPACES.
 01  Ws-Tot.
     05  T-Lines                 PIC S9(7) VALUE 0.
     05  T-Parents               PIC S9(7) VALUE 0.
     05  T-Possible              PIC S9(7) VALUE 0.
     05  T-Top                   PIC S9(7) VALUE 0.
     05  T-Demand                PIC S9(7) VALUE 0.
     05  T-Depth                 PIC S9(4) VALUE 0.
     05  T-Added                 PIC S9(7) VALUE 0.
     05  T-Removed               PIC S9(7) VALUE 0.
     05  T-Changed               PIC S9(7) VALUE 0.
     05  T-Seq-Only              PIC S9(7) VALUE 0.
     05  T-Same                  PIC S9(7) VALUE 0.
     05  T-Cost-Delta            PIC S9(13)V9(2) VALUE 0.
 REPORT SECTION.
 RD  R-BOMCHG
     CONTROLS ARE FINAL
     PAGE LIMIT 60 LINES
     HEADING 1 FIRST DETAIL 8 LAST DETAIL 56
     FOOTING 58.
 COPY STRPTHDR.
 01  DE-SUBJECT TYPE IS DETAIL.
     05  LINE PLUS 1.
         10  COLUMN 1    PIC X(16) SOURCE H-Subject-Lab.
         10  COLUMN 17   PIC X(44) SOURCE H-Subject.
         10  COLUMN 63   PIC X(70) SOURCE H-Context.
     05  LINE PLUS 1.
         10  COLUMN 1    PIC X(66) SOURCE H-Line2-1.
         10  COLUMN 67   PIC X(66) SOURCE H-Line2-2.
     05  LINE PLUS 1.
         10  COLUMN 1    PIC X(66) VALUE ALL "=".
         10  COLUMN 67   PIC X(66) VALUE ALL "=".
 01  DE-SECTION TYPE IS DETAIL.
     05  LINE PLUS 2.
         10  COLUMN 1    PIC X(66) SOURCE D-Sec-1.
         10  COLUMN 67   PIC X(66) SOURCE D-Sec-2.
     05  LINE PLUS 1.
         10  COLUMN 1    PIC X(66) SOURCE D-Cap-1.
         10  COLUMN 67   PIC X(66) SOURCE D-Cap-2.
     05  LINE PLUS 1.
         10  COLUMN 1    PIC X(66) VALUE ALL "-".
         10  COLUMN 67   PIC X(66) VALUE ALL "-".
 01  DE-WUSED TYPE IS DETAIL.
     05  LINE PLUS 1.
         10  COLUMN 2    PIC Z(2)9 SOURCE D-Level.
         10  COLUMN 6    PIC X(32) SOURCE D-Parent.
         10  COLUMN 39   PIC X(4) SOURCE D-Parent-Rev.
         10  COLUMN 44   PIC X(20) SOURCE D-Desc.
         10  COLUMN 65   PIC X(22) SOURCE D-Bom-Ref.
         10  COLUMN 88   PIC X(4) SOURCE D-Bom-Rev.
         10  COLUMN 93   PIC X(10) SOURCE D-Status.
         10  COLUMN 104  PIC Z(3)9 SOURCE D-Line-Num.
         10  COLUMN 109  PIC Z(7)9.999999 SOURCE D-Usage.
         10  COLUMN 125  PIC X(8) SOURCE D-Uom.
 01  DE-WUPATH TYPE IS DETAIL.
     05  LINE PLUS 1.
         10  COLUMN 8    PIC X(6) VALUE "path: ".
         10  COLUMN 14   PIC X(70) SOURCE D-Path.
         10  COLUMN 85   PIC X(12) SOURCE D-Basis.
         10  COLUMN 98   PIC X(10) SOURCE D-Cert.
         10  COLUMN 109  PIC X(24) SOURCE D-Demand.
 01  DE-COMPARE TYPE IS DETAIL.
     05  LINE PLUS 1.
         10  COLUMN 2    PIC X(8) SOURCE D-Action.
         10  COLUMN 11   PIC X(28) SOURCE D-Item.
         10  COLUMN 40   PIC X(4) SOURCE D-Bom-Rev.
         10  COLUMN 45   PIC X(20) SOURCE D-Desc.
         10  COLUMN 66   PIC Z(3)9 SOURCE D-From-Line.
         10  COLUMN 71   PIC Z(3)9 SOURCE D-To-Line.
         10  COLUMN 76   PIC Z(5)9.999999 SOURCE D-From-Qty.
         10  COLUMN 90   PIC Z(5)9.999999 SOURCE D-To-Qty.
         10  COLUMN 104  PIC -(5)9.999999 SOURCE D-Delta.
         10  COLUMN 118  PIC X(4) SOURCE D-Uom.
         10  COLUMN 123  PIC X(10) SOURCE D-Basis.
 01  DE-CPNOTE TYPE IS DETAIL.
     05  LINE PLUS 1.
         10  COLUMN 11   PIC X(9) VALUE "changed: ".
         10  COLUMN 20   PIC X(100) SOURCE D-Detail.
         10  COLUMN 121  PIC X(12) SOURCE D-Status.
 01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
     05  LINE PLUS 2.
         10  COLUMN 1    PIC X(66) SOURCE F-1-1.
         10  COLUMN 67   PIC X(66) SOURCE F-1-2.
     05  LINE PLUS 1.
         10  COLUMN 1    PIC X(66) SOURCE F-2-1.
         10  COLUMN 67   PIC X(66) SOURCE F-2-2.
     05  LINE PLUS 1.
         10  COLUMN 1    PIC X(66) SOURCE F-3-1.
         10  COLUMN 67   PIC X(66) SOURCE F-3-2.
     05  LINE PLUS 2.
         10  COLUMN 1    PIC X(14) VALUE "Completeness: ".
         10  COLUMN 15   PIC X(12) SOURCE H-Complete.
         10  COLUMN 30   PIC X(70) SOURCE H-Stop.
     05  LINE PLUS 1.
         10  COLUMN 1    PIC X(66) SOURCE F-4-1.
         10  COLUMN 67   PIC X(66) SOURCE F-4-2.
 PROCEDURE DIVISION.
 0000-MAIN SECTION.
 0000-START.
     PERFORM 0100-READ-ARGS
     PERFORM 0200-JOB-START
     PERFORM 0300-OPEN-SPOOL
     IF Ws-Rc > 4
         GO TO 0000-FINISH
     END-IF
     PERFORM 1000-RUN-ENQUIRY
     PERFORM 3000-FILL-FOOTER
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
     MOVE "BOMREF" TO CM-LOOKUP-KEY
     PERFORM 0150-GET-ARG
     MOVE CM-LOOKUP-VALUE TO Ws-Bom-Ref
     MOVE "FROMREV" TO CM-LOOKUP-KEY
     PERFORM 0150-GET-ARG
     MOVE CM-LOOKUP-VALUE TO Ws-From-Rev
     MOVE "TOREV" TO CM-LOOKUP-KEY
     PERFORM 0150-GET-ARG
     MOVE CM-LOOKUP-VALUE TO Ws-To-Rev
     MOVE "COSTSET" TO CM-LOOKUP-KEY
     PERFORM 0150-GET-ARG
     MOVE CM-LOOKUP-VALUE TO Ws-Cost-Set
     MOVE "DRAFT" TO CM-LOOKUP-KEY
     PERFORM 0150-GET-ARG
     IF CM-LOOKUP-VALUE(1:1) = "Y"
         MOVE "Y" TO Ws-Draft
     END-IF
     MOVE "LEVELS" TO CM-LOOKUP-KEY
     PERFORM 0150-GET-ARG
     IF CM-LOOKUP-VALUE NOT = SPACES
         COMPUTE Ws-Levels = FUNCTION NUMVAL(CM-LOOKUP-VALUE)
     END-IF
     MOVE "QTY" TO CM-LOOKUP-KEY
     PERFORM 0150-GET-ARG
     IF CM-LOOKUP-VALUE NOT = SPACES
         COMPUTE Ws-Qty = FUNCTION NUMVAL(CM-LOOKUP-VALUE)
     END-IF
     IF CM-ASAT NOT = SPACES
         COMPUTE Ws-Asat = FUNCTION NUMVAL(CM-ASAT)
     END-IF
     PERFORM 0160-APPLY-DEFAULTS
     PERFORM 0170-CHECK-ARGS.
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

 0160-APPLY-DEFAULTS SECTION.
 0160-START.
     IF Ws-Co = SPACES
         MOVE "STC" TO Ws-Co
     END-IF
     IF Ws-User = SPACES
         MOVE "ENGINEER" TO Ws-User
     END-IF
     IF Ws-Mode = SPACES
         MOVE "WHEREUSED" TO Ws-Mode
     END-IF
     IF Ws-Levels = 0
         MOVE 25 TO Ws-Levels
     END-IF
     IF Ws-Qty = 0
         MOVE 1 TO Ws-Qty
     END-IF
     IF Ws-Cost-Set = SPACES
         MOVE "BOMDEMO" TO Ws-Cost-Set
     END-IF
     IF Ws-Asat = 0
         MOVE FUNCTION CURRENT-DATE TO Ws-Now
         COMPUTE Ws-Asat = FUNCTION NUMVAL(Ws-Now(1:8))
     END-IF
     IF Ws-Mode = "COMPARE"
         MOVE "Y" TO Ws-Compare
     END-IF
     MOVE "BOMRP030" TO BL-JOB-NAME.
 0160-EXIT.
     EXIT.

*> A report that cannot name its subject prints nothing and says
*> why; it does not produce an empty listing somebody has to read.
 0170-CHECK-ARGS SECTION.
 0170-START.
     IF Ws-Compare = "Y"
         IF Ws-Bom-Ref = SPACES OR Ws-From-Rev = SPACES
            OR Ws-To-Rev = SPACES
             MOVE 12 TO Ws-Rc
             DISPLAY "STERP-E-BOM1501 BOMREF=, FROMREV= and "
                     "TOREV= are required for MODE=COMPARE"
         END-IF
         DISPLAY "STERP-I-BOM1500 BOMRP030 revision comparison "
                 FUNCTION TRIM(Ws-Bom-Ref) " "
                 FUNCTION TRIM(Ws-From-Rev) " to "
                 FUNCTION TRIM(Ws-To-Rev)
         EXIT PARAGRAPH
     END-IF
     IF Ws-Item = SPACES
         MOVE 12 TO Ws-Rc
         DISPLAY "STERP-E-BOM1502 ITEM= is required for "
                 "MODE=WHEREUSED"
     END-IF
     DISPLAY "STERP-I-BOM1500 BOMRP030 where used listing for "
             FUNCTION TRIM(Ws-Item).
 0170-EXIT.
     EXIT.

 0200-JOB-START SECTION.
 0200-START.
     INITIALIZE Ws-Joblog
     MOVE "START" TO CA-FUNCTION OF Ws-Joblog
     MOVE Ws-Co TO CA-COMPANY OF Ws-Joblog
     MOVE Ws-User TO CA-USER OF Ws-Joblog
     MOVE BL-JOB-NAME TO JL-JOB-NAME
     MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
     CALL "JOBLOG" USING Ws-Joblog.
 0200-EXIT.
     EXIT.

 0300-OPEN-SPOOL SECTION.
 0300-START.
     INITIALIZE Ws-Runpath
     MOVE "BUILD" TO CA-FUNCTION OF Ws-Runpath
     MOVE "SPOOL" TO RP-FOLDER
     MOVE "BOMRP030" TO RP-REPORT-ID
     MOVE Ws-Co TO RP-COMPANY-REF
     MOVE "Y" TO RP-USE-TIMESTAMP
     CALL "RUNPATH" USING Ws-Runpath
     IF CA-RETURN-CODE OF Ws-Runpath > 4
         MOVE 12 TO Ws-Rc
         DISPLAY "STERP-E-BOM1503 cannot build the spool path"
         GO TO 0300-EXIT
     END-IF
     MOVE RP-FULL-PATH TO Ws-Spool-Name
     MOVE RP-FULL-PATH TO BL-SPOOL-FILE
     PERFORM 0350-FILL-HEADINGS
     OPEN OUTPUT RPT-FILE
     IF Ws-File-Status NOT = "00"
         MOVE 12 TO Ws-Rc
         DISPLAY "STERP-E-BOM1504 cannot open " Ws-Spool-Name
         GO TO 0300-EXIT
     END-IF
     INITIATE R-BOMCHG.
 0300-EXIT.
     EXIT.

 0350-FILL-HEADINGS SECTION.
 0350-START.
     MOVE Ws-Co TO RPT-COMPANY-CODE
     MOVE "BOMRP030" TO RPT-REPORT-ID
     MOVE Ws-User TO RPT-USER
     IF Ws-Compare = "Y"
         MOVE "BOMRP040" TO RPT-REPORT-ID
         MOVE "Revision comparison" TO RPT-TITLE
         MOVE SPACES TO RPT-SUBTITLE
         STRING "Cost set " FUNCTION TRIM(Ws-Cost-Set)
                " - sequence changes shown apart"
             DELIMITED BY SIZE INTO RPT-SUBTITLE
     ELSE
         MOVE "Where used - direct and indirect" TO RPT-TITLE
         MOVE SPACES TO RPT-SUBTITLE
         IF Ws-Draft = "Y"
             STRING "Released and draft bills; "
                    "possible uses are labelled"
                 DELIMITED BY SIZE INTO RPT-SUBTITLE
         ELSE
             STRING "Released bills only; "
                    "possible uses are labelled"
                 DELIMITED BY SIZE INTO RPT-SUBTITLE
         END-IF
     END-IF
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
     MOVE 100 TO SQLIO-COL-WIDTH(1)
     MOVE 100 TO SQLIO-COL-WIDTH(2)
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
     TERMINATE R-BOMCHG
     CLOSE RPT-FILE
     MOVE Ws-Rows-Out TO BL-WRITTEN-COUNT
     MOVE Ws-Rows-Out TO BL-COUNT-ED
     DISPLAY "STERP-I-BOM1505 " FUNCTION TRIM(BL-COUNT-ED)
             " lines written"
     DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(Ws-Spool-Name).
 0400-EXIT.
     EXIT.

 0500-JOB-END SECTION.
 0500-START.
     MOVE Ws-Rc TO BL-RETURN-CODE
     MOVE "END" TO CA-FUNCTION OF Ws-Joblog
     MOVE Ws-Rc TO JL-RETURN-CODE
     MOVE Ws-Rows-Out TO JL-ROWS-PROCESSED
     MOVE Ws-Spool-Name TO JL-SPOOL-FILE
     CALL "JOBLOG" USING Ws-Joblog.
 0500-EXIT.
     EXIT.

*>----------------------------------------------------------------
*> An engineering change report that quietly leaves a section out
*> is worse than no report at all, so a refusal from the enquiry
*> engine becomes a worklist exception as well as a console
*> message (COM-017, COM-022, BOM-029).
*>----------------------------------------------------------------
 0600-RAISE-EXCEPTION SECTION.
 0600-START.
     INITIALIZE Ws-Excp
     MOVE "RAISE" TO XA-FUNCTION
     MOVE Ws-Co TO XA-COMPANY
     MOVE Ws-Site TO XA-SITE
     MOVE Ws-User TO XA-USER
     MOVE Ws-Asat TO XA-BUSINESS-DATE
     MOVE "BOM" TO EX-MODULE
     MOVE "WARNING" TO EX-SEVERITY
     MOVE "BOM.CHANGE.REPORT" TO EX-CATEGORY
     MOVE "REPORT" TO EX-DOCUMENT-TYPE
     MOVE BL-JOB-NAME TO EX-DOCUMENT-REF
     MOVE Ws-User TO EX-OWNER-USER
     MOVE "ENGINEER" TO EX-OWNER-ROLE
     MOVE 5 TO EX-PRIORITY
     MOVE BL-MSG-TEXT TO EX-MESSAGE-TEXT
     CALL "EXCPRAIS" USING Ws-Excp.
 0600-EXIT.
     EXIT.

*>----------------------------------------------------------------
 1000-RUN-ENQUIRY SECTION.
 1000-START.
     IF Ws-Compare = "Y"
         PERFORM 2000-PRINT-COMPARISON
     ELSE
         PERFORM 1100-PRINT-WHERE-USED
     END-IF.
 1000-EXIT.
     EXIT.

*>----------------------------------------------------------------
*> Three questions, one traversal engine, three sections.
*>----------------------------------------------------------------
 1100-PRINT-WHERE-USED SECTION.
 1100-START.
     MOVE "Changed part   " TO H-Subject-Lab
     MOVE Ws-Item TO H-Subject
     PERFORM 1110-ALL-PARENTS
     PERFORM 1120-TOP-LEVEL
     PERFORM 1130-OPEN-DEMAND.
 1100-EXIT.
     EXIT.

 1105-SET-UP-REQUEST SECTION.
 1105-START.
     MOVE Ws-Co TO UA-COMPANY
     MOVE Ws-Site TO UA-SITE
     MOVE Ws-User TO UA-USER
     MOVE Ws-Asat TO UA-BUSINESS-DATE
     MOVE Ws-Item TO WU-ITEM
     MOVE Ws-Asat TO WU-EFF-DATE
     MOVE "STANDARD" TO WU-PURPOSE
     MOVE Ws-Levels TO WU-MAX-DEPTH
     MOVE Ws-Draft TO WU-INCLUDE-DRAFT
     MOVE "Y" TO WU-INCLUDE-OPT
     MOVE 1 TO WU-FROM-LINE
     MOVE 25 TO WU-WIN-MAX.
 1105-EXIT.
     EXIT.

 1110-ALL-PARENTS SECTION.
 1110-START.
     INITIALIZE Ws-Wu-Ca
     PERFORM 1105-SET-UP-REQUEST
     MOVE "ALL" TO UA-FUNCTION
     CALL "WHEREUSD" USING Ws-Wu-Ca
     IF UA-RETURN-CODE > 7 AND UA-RETURN-CODE NOT = 16
         PERFORM 1190-REPORT-REFUSAL
     END-IF
     MOVE WU-PARENT-CNT TO T-Parents
     MOVE WU-DEPTH-REACHED TO T-Depth
     MOVE WU-COMPLETENESS TO H-Complete
     MOVE WU-PARENT-CNT TO BL-COUNT-ED
     MOVE SPACES TO H-Line2
     STRING "Ancestor uses found " FUNCTION TRIM(BL-COUNT-ED)
         DELIMITED BY SIZE INTO H-Line2
     MOVE WU-DEPTH-REACHED TO BL-COUNT-ED
     MOVE "deepest level" TO H-Line2(34:13)
     MOVE BL-COUNT-ED TO H-Line2(48:9)
     MOVE "completeness" TO H-Line2(60:12)
     MOVE WU-COMPLETENESS TO H-Line2(73:12)
     MOVE Ws-Asat TO Ws-Date-N
     MOVE "effective" TO H-Line2(88:9)
     MOVE Ws-D-Dd TO H-Line2(98:2)
     MOVE "/" TO H-Line2(100:1)
     MOVE Ws-D-Mm TO H-Line2(101:2)
     MOVE "/" TO H-Line2(103:1)
     MOVE Ws-D-Yyyy TO H-Line2(104:4)
     MOVE Ws-Levels TO BL-COUNT-ED
     MOVE SPACES TO H-Context
     STRING "Levels requested " FUNCTION TRIM(BL-COUNT-ED)
            "   scope " FUNCTION TRIM(WU-PURPOSE)
            " bills for company " FUNCTION TRIM(Ws-Co)
         DELIMITED BY SIZE INTO H-Context
     ADD 1 TO Ws-Rows-Out
     GENERATE DE-SUBJECT
     MOVE "DIRECT AND INDIRECT PARENTS (level 1 is a direct use)"
         TO Ws-Section
     PERFORM 1180-WHERE-USED-CAPTION
     PERFORM 1150-PRINT-WU-WINDOWS.
 1110-EXIT.
     EXIT.

 1120-TOP-LEVEL SECTION.
 1120-START.
     PERFORM 1105-SET-UP-REQUEST
     MOVE "TOPLEVEL" TO UA-FUNCTION
     CALL "WHEREUSD" USING Ws-Wu-Ca
     MOVE "TOP LEVEL ASSEMBLIES AT THE END OF EACH PATH"
         TO Ws-Section
     PERFORM 1180-WHERE-USED-CAPTION
     PERFORM 1150-PRINT-WU-WINDOWS
     MOVE Ws-Section-Rows TO T-Top.
 1120-EXIT.
     EXIT.

 1130-OPEN-DEMAND SECTION.
 1130-START.
     PERFORM 1105-SET-UP-REQUEST
     MOVE "OPENUSE" TO UA-FUNCTION
     CALL "WHEREUSD" USING Ws-Wu-Ca
     MOVE "OPEN EXECUTION DEMAND THAT NEEDS AN EXPLICIT DECISION"
         TO Ws-Section
     PERFORM 1180-WHERE-USED-CAPTION
     PERFORM 1150-PRINT-WU-WINDOWS
     MOVE Ws-Section-Rows TO T-Demand.
 1130-EXIT.
     EXIT.

 1150-PRINT-WU-WINDOWS SECTION.
 1150-START.
     MOVE 0 TO Ws-Section-Rows
     ADD 1 TO Ws-Rows-Out
     GENERATE DE-SECTION
     PERFORM 1160-PRINT-WU-WINDOW
     PERFORM UNTIL WU-MORE-LINES NOT = "Y"
         MOVE "GETLINE" TO UA-FUNCTION
         CALL "WHEREUSD" USING Ws-Wu-Ca
         IF UA-RETURN-CODE > 7
             EXIT PERFORM
         END-IF
         PERFORM 1160-PRINT-WU-WINDOW
     END-PERFORM
     IF Ws-Section-Rows = 0
         MOVE SPACES TO D-Path
         MOVE "nothing to report for this section" TO D-Path
         MOVE SPACES TO D-Basis D-Cert D-Demand
         ADD 1 TO Ws-Rows-Out
         GENERATE DE-WUPATH
     END-IF.
 1150-EXIT.
     EXIT.

 1160-PRINT-WU-WINDOW SECTION.
 1160-START.
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > WU-WIN-CNT
         MOVE WU-L-LEVEL(Ws-W) TO D-Level
         MOVE WU-L-PARENT(Ws-W) TO D-Parent
         MOVE WU-L-PARENT-REV(Ws-W) TO D-Parent-Rev
         MOVE WU-L-DESC(Ws-W) TO D-Desc
         MOVE WU-L-BOM-REF(Ws-W) TO D-Bom-Ref
         MOVE WU-L-BOM-REV(Ws-W) TO D-Bom-Rev
         MOVE WU-L-STATUS(Ws-W) TO D-Status
         MOVE WU-L-LINE-NUM(Ws-W) TO D-Line-Num
         MOVE WU-L-USAGE(Ws-W) TO D-Usage
         MOVE WU-L-UOM(Ws-W) TO D-Uom
         MOVE WU-L-PATH(Ws-W) TO D-Path
         MOVE WU-L-BASIS(Ws-W) TO D-Basis
         MOVE WU-L-CERTAINTY(Ws-W) TO D-Cert
         MOVE WU-L-DEMAND(Ws-W) TO D-Demand
         IF WU-L-CERTAINTY(Ws-W) NOT = "CONFIRMED"
             ADD 1 TO T-Possible
         END-IF
         ADD 1 TO T-Lines Ws-Rows-Out Ws-Section-Rows
         GENERATE DE-WUSED
         IF D-Path NOT = SPACES OR D-Demand NOT = SPACES
             ADD 1 TO Ws-Rows-Out
             GENERATE DE-WUPATH
         END-IF
     END-PERFORM.
 1160-EXIT.
     EXIT.

 1180-WHERE-USED-CAPTION SECTION.
 1180-START.
     MOVE SPACES TO Ws-Caption
     MOVE "Lvl" TO Ws-Caption(2:3)
     MOVE "Parent item" TO Ws-Caption(6:11)
     MOVE "Rev" TO Ws-Caption(39:3)
     MOVE "Description" TO Ws-Caption(44:11)
     MOVE "Bill reference" TO Ws-Caption(65:14)
     MOVE "Rev" TO Ws-Caption(88:3)
     MOVE "Status" TO Ws-Caption(93:6)
     MOVE "Line" TO Ws-Caption(104:4)
     MOVE "Qty per parent" TO Ws-Caption(110:14)
     MOVE "Unit" TO Ws-Caption(125:4).
 1180-EXIT.
     EXIT.

 1190-REPORT-REFUSAL SECTION.
 1190-START.
     MOVE 4 TO Ws-Rc
     MOVE UA-MESSAGE TO H-Stop
     MOVE UA-MESSAGE TO BL-MSG-TEXT
     MOVE "INCOMPLETE" TO H-Complete
     DISPLAY "STERP-W-BOM1506 " FUNCTION TRIM(UA-MESSAGE)
     PERFORM 0600-RAISE-EXCEPTION.
 1190-EXIT.
     EXIT.

*>----------------------------------------------------------------
*> MODE=COMPARE.  BYREF names the two revisions the way an
*> engineer does; IMPACT adds what the difference is worth.
*>----------------------------------------------------------------
 2000-PRINT-COMPARISON SECTION.
 2000-START.
     INITIALIZE Ws-Cp-Ca
     MOVE "BYREF" TO PA-FUNCTION
     MOVE Ws-Co TO PA-COMPANY
     MOVE Ws-Site TO PA-SITE
     MOVE Ws-User TO PA-USER
     MOVE Ws-Asat TO PA-BUSINESS-DATE
     MOVE Ws-Bom-Ref TO BP-BOM-REF
     MOVE Ws-From-Rev TO BP-FROM-REV
     MOVE Ws-To-Rev TO BP-TO-REV
     MOVE Ws-Asat TO BP-EFF-DATE
     MOVE Ws-Qty TO BP-REQ-QTY
     MOVE 1 TO BP-FROM-LINE
     MOVE 25 TO BP-WIN-MAX
     CALL "BOMCOMP" USING Ws-Cp-Ca
     IF PA-RETURN-CODE > 7
         MOVE 8 TO Ws-Rc
         MOVE PA-MESSAGE TO H-Stop
         MOVE PA-MESSAGE TO BL-MSG-TEXT
         MOVE "INCOMPLETE" TO H-Complete
         DISPLAY "STERP-E-BOM1507 " FUNCTION TRIM(PA-MESSAGE)
         PERFORM 0600-RAISE-EXCEPTION
         PERFORM 2400-REFUSAL-PAGE
         GO TO 2000-EXIT
     END-IF
     MOVE "IMPACT" TO PA-FUNCTION
     MOVE 1 TO BP-FROM-LINE
     CALL "BOMCOMP" USING Ws-Cp-Ca
     PERFORM 2100-FILL-COMPARE-HEAD
     PERFORM 2200-PRINT-COMPARE-LINES.
 2000-EXIT.
     EXIT.

 2100-FILL-COMPARE-HEAD SECTION.
 2100-START.
     MOVE "Bill reference " TO H-Subject-Lab
     MOVE Ws-Bom-Ref TO H-Subject
     MOVE SPACES TO H-Context
     STRING "Parent " FUNCTION TRIM(BP-PARENT-ITEM)
            "   revision " FUNCTION TRIM(Ws-From-Rev) " to "
            FUNCTION TRIM(Ws-To-Rev)
         DELIMITED BY SIZE INTO H-Context
     MOVE BP-ADDED-CNT TO T-Added
     MOVE BP-REMOVED-CNT TO T-Removed
     MOVE BP-CHANGED-CNT TO T-Changed
     MOVE BP-SEQONLY-CNT TO T-Seq-Only
     MOVE BP-SAME-CNT TO T-Same
     MOVE BP-COST-DELTA TO T-Cost-Delta
     MOVE BP-COMPLETENESS TO H-Complete
     MOVE SPACES TO H-Line2
     IF BP-HEADER-DIFF = SPACES
         MOVE "Header unchanged" TO H-Line2(1:16)
     ELSE
         MOVE "Header: " TO H-Line2(1:8)
         MOVE BP-HEADER-DIFF(1:110) TO H-Line2(9:110)
     END-IF
     IF BP-MATERIAL-CHANGE = "Y"
         MOVE "MATERIAL CHANGE" TO H-Line2(118:15)
     ELSE
         MOVE "SEQUENCE ONLY" TO H-Line2(118:13)
     END-IF
     MOVE SPACES TO Ws-Section
     MOVE "COMPONENT DIFFERENCES BETWEEN THE TWO REVISIONS"
         TO Ws-Section
     MOVE SPACES TO Ws-Caption
     MOVE "Action" TO Ws-Caption(2:6)
     MOVE "Component item" TO Ws-Caption(11:14)
     MOVE "Rev" TO Ws-Caption(40:3)
     MOVE "Description" TO Ws-Caption(45:11)
     MOVE "From" TO Ws-Caption(66:4)
     MOVE "To" TO Ws-Caption(71:2)
     MOVE "Qty before" TO Ws-Caption(79:10)
     MOVE "Qty after" TO Ws-Caption(93:9)
     MOVE "Difference" TO Ws-Caption(106:10)
     MOVE "Unit" TO Ws-Caption(118:4)
     MOVE "Basis" TO Ws-Caption(123:5)
     ADD 1 TO Ws-Rows-Out
     GENERATE DE-SUBJECT.
 2100-EXIT.
     EXIT.

*> A refused comparison still produces a page, because a listing
*> that silently comes back empty is read as "no differences".
 2400-REFUSAL-PAGE SECTION.
 2400-START.
     MOVE "Bill reference " TO H-Subject-Lab
     MOVE Ws-Bom-Ref TO H-Subject
     MOVE SPACES TO H-Context H-Line2
     STRING "Revision " FUNCTION TRIM(Ws-From-Rev) " to "
            FUNCTION TRIM(Ws-To-Rev) " - refused"
         DELIMITED BY SIZE INTO H-Context
     MOVE PA-MESSAGE(1:120) TO H-Line2(1:120)
     ADD 1 TO Ws-Rows-Out
     GENERATE DE-SUBJECT
     MOVE SPACES TO Ws-Section Ws-Caption
     MOVE "NO COMPARISON WAS PRODUCED" TO Ws-Section
     ADD 1 TO Ws-Rows-Out
     GENERATE DE-SECTION
     MOVE PA-MESSAGE(1:100) TO D-Detail
     MOVE PA-REASON-CODE TO D-Status
     ADD 1 TO Ws-Rows-Out
     GENERATE DE-CPNOTE.
 2400-EXIT.
     EXIT.

 2200-PRINT-COMPARE-LINES SECTION.
 2200-START.
     MOVE 0 TO Ws-Section-Rows
     ADD 1 TO Ws-Rows-Out
     GENERATE DE-SECTION
     MOVE 1 TO BP-FROM-LINE
     MOVE "GETLINE" TO PA-FUNCTION
     CALL "BOMCOMP" USING Ws-Cp-Ca
     PERFORM 2300-PRINT-COMPARE-WINDOW
     PERFORM UNTIL BP-MORE-LINES NOT = "Y"
         MOVE "GETLINE" TO PA-FUNCTION
         CALL "BOMCOMP" USING Ws-Cp-Ca
         IF PA-RETURN-CODE > 7
             EXIT PERFORM
         END-IF
         PERFORM 2300-PRINT-COMPARE-WINDOW
     END-PERFORM
     IF Ws-Section-Rows = 0
         MOVE SPACES TO D-Detail
         MOVE "the two revisions have identical component lines"
             TO D-Detail
         MOVE SPACES TO D-Status
         ADD 1 TO Ws-Rows-Out
         GENERATE DE-CPNOTE
     END-IF.
 2200-EXIT.
     EXIT.

 2300-PRINT-COMPARE-WINDOW SECTION.
 2300-START.
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > BP-WIN-CNT
         MOVE BP-L-ACTION(Ws-W) TO D-Action
         MOVE BP-L-ITEM(Ws-W) TO D-Item
         MOVE BP-L-REV(Ws-W) TO D-Bom-Rev
         MOVE BP-L-DESC(Ws-W) TO D-Desc
         MOVE BP-L-FROM-LINE(Ws-W) TO D-From-Line
         MOVE BP-L-TO-LINE(Ws-W) TO D-To-Line
         MOVE BP-L-FROM-QTY(Ws-W) TO D-From-Qty
         MOVE BP-L-TO-QTY(Ws-W) TO D-To-Qty
         MOVE BP-L-QTY-DELTA(Ws-W) TO D-Delta
         MOVE BP-L-TO-UOM(Ws-W) TO D-Uom
         IF BP-L-TO-UOM(Ws-W) = SPACES
             MOVE BP-L-FROM-UOM(Ws-W) TO D-Uom
         END-IF
         MOVE BP-L-TO-BASIS(Ws-W) TO D-Basis
         IF BP-L-TO-BASIS(Ws-W) = SPACES
             MOVE BP-L-FROM-BASIS(Ws-W) TO D-Basis
         END-IF
         MOVE BP-L-DETAIL(Ws-W) TO D-Detail
         IF BP-L-ACTION(Ws-W) = "SEQUENCE"
             MOVE "not material" TO D-Status
         ELSE
             MOVE SPACES TO D-Status
         END-IF
         ADD 1 TO T-Lines Ws-Rows-Out Ws-Section-Rows
         GENERATE DE-COMPARE
         IF D-Detail NOT = SPACES
             ADD 1 TO Ws-Rows-Out
             GENERATE DE-CPNOTE
         END-IF
     END-PERFORM.
 2300-EXIT.
     EXIT.

*>----------------------------------------------------------------
*> The footer carries the evidence a reviewer needs to trust the
*> page: how many rows, how they were classified, and whether the
*> answer is complete (BOM-029, BOM-032).
*>----------------------------------------------------------------
 3000-FILL-FOOTER SECTION.
 3000-START.
     MOVE SPACES TO F-1 F-2 F-3 F-4
     IF Ws-Compare = "Y"
         PERFORM 3200-COMPARE-FOOTER
     ELSE
         PERFORM 3100-WHERE-USED-FOOTER
     END-IF.
 3000-EXIT.
     EXIT.

 3100-WHERE-USED-FOOTER SECTION.
 3100-START.
     MOVE "Rows printed" TO F-1(1:12)
     MOVE T-Lines TO BL-COUNT-ED
     MOVE BL-COUNT-ED TO F-1(20:9)
     MOVE "ancestor uses listed" TO F-1(32:20)
     MOVE T-Parents TO BL-COUNT-ED
     MOVE BL-COUNT-ED TO F-1(54:9)
     MOVE "deepest level reached" TO F-1(67:21)
     MOVE T-Depth TO BL-COUNT-ED
     MOVE BL-COUNT-ED TO F-1(90:9)
     MOVE "Top level assemblies" TO F-2(1:20)
     MOVE T-Top TO BL-COUNT-ED
     MOVE BL-COUNT-ED TO F-2(22:9)
     MOVE "open demand rows" TO F-2(34:16)
     MOVE T-Demand TO BL-COUNT-ED
     MOVE BL-COUNT-ED TO F-2(52:9)
     MOVE "uses that are only POSSIBLE" TO F-2(64:27)
     MOVE T-Possible TO BL-COUNT-ED
     MOVE BL-COUNT-ED TO F-2(93:9)
     STRING "A POSSIBLE use is an unselected optional or "
            "alternate line: a permitted configuration could "
            "activate it, so it is reported."
         DELIMITED BY SIZE INTO F-3
     STRING "It is never counted as confirmed usage, and released"
            " history is listed and never deleted (BOM-023, "
            "BOM-028)."
         DELIMITED BY SIZE INTO F-4.
 3100-EXIT.
     EXIT.

 3200-COMPARE-FOOTER SECTION.
 3200-START.
     MOVE "Components added" TO F-1(1:16)
     MOVE T-Added TO BL-COUNT-ED
     MOVE BL-COUNT-ED TO F-1(20:9)
     MOVE "removed" TO F-1(32:7)
     MOVE T-Removed TO BL-COUNT-ED
     MOVE BL-COUNT-ED TO F-1(42:9)
     MOVE "changed" TO F-1(54:7)
     MOVE T-Changed TO BL-COUNT-ED
     MOVE BL-COUNT-ED TO F-1(64:9)
     MOVE "resequenced only" TO F-1(76:16)
     MOVE T-Seq-Only TO BL-COUNT-ED
     MOVE BL-COUNT-ED TO F-1(94:9)
     MOVE "unchanged" TO F-1(106:9)
     MOVE T-Same TO BL-COUNT-ED
     MOVE BL-COUNT-ED TO F-1(117:9)
     MOVE "Cost of revision" TO F-2(1:16)
     MOVE Ws-From-Rev TO F-2(18:4)
     MOVE BP-FROM-COST TO Ws-Money-Ed
     MOVE Ws-Money-Ed TO F-2(23:13)
     MOVE "revision" TO F-2(38:8)
     MOVE Ws-To-Rev TO F-2(47:4)
     MOVE BP-TO-COST TO Ws-Money-Ed
     MOVE Ws-Money-Ed TO F-2(52:13)
     MOVE "impact of the change" TO F-2(67:20)
     MOVE T-Cost-Delta TO Ws-Money-Ed
     MOVE Ws-Money-Ed TO F-2(88:13)
     MOVE "GBP for" TO F-2(102:7)
     MOVE Ws-Qty TO Ws-Qty-Ed
     MOVE Ws-Qty-Ed TO F-2(110:13)
     MOVE "unit(s)" TO F-2(124:7)
     STRING "A line that only moved in the sequence is reported "
            "as SEQUENCE and is not a material change (BOM-025)."
         DELIMITED BY SIZE INTO F-3
     IF T-Cost-Delta = 0 AND BP-MATERIAL-CHANGE = "Y"
         STRING "The roll-up values the effective revision of the"
                " parent, so a draft revision can show no cost "
                "movement even for a material change."
             DELIMITED BY SIZE INTO F-4
     ELSE
         STRING "Costs come from the revision roll-up, so a "
                "missing rate makes the impact incomplete rather "
                "than zero."
             DELIMITED BY SIZE INTO F-4
     END-IF.
 3200-EXIT.
     EXIT.
