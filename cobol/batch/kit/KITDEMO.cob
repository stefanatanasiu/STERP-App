>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : KITDEMO
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : The kit demonstration driver.  It walks one real
*>              kit from definition to assembled stock through the
*>              online kit services, so that the demonstration
*>              database shows work in progress, issues, a
*>              completion, genealogy, a kit cost and one
*>              disassembly instead of hand written seed rows:
*>
*>                pick a released bill with stock that can be
*>                     covered at the requested site
*>                KITTRIAL   feasibility, commits nothing
*>                KITCREAT   CREATE / REVIEW / RELEASE
*>                KITRESV    RESERVE / PICKLIST / PICK
*>                KITISSUE   BACKFLSH (and an explicit ISSUE or
*>                           CHECK for whatever backflush leaves)
*>                KITCOMPL   COMPCHK / COMPLETE / FINAL
*>                GENEALOG   FORWARD
*>                KITCOST    COST
*>                DISASSY    CREATE / ADDLINE / VALUE / APPROVE /
*>                           POST
*>
*>              Segregation of duties is respected: the operator
*>              named on USER= is the person who ran the job, not
*>              the person who did the work.  Each step is stamped
*>              with the role that owns it (engineering plans,
*>              stores reserves, picks, issues and completes, the
*>              cost accountant reads the cost, the inventory
*>              manager raises a disassembly and quality approves
*>              it), exactly as the shop floor would.
*>
*>              COM-015.  Every service call gets a correlation id
*>              of its own, derived from a 30 character stem, and
*>              never one id shared by several idempotent
*>              services: KITISSUE BACKFLSH and DISASSY POST both
*>              append their own suffix to the stem (CR2741) and a
*>              reused id would silently post nothing.
*>
*>              The driver is re-runnable.  If a demonstration kit
*>              for the run date is already on file it reports it
*>              and stops with RC 0 - a second run issues nothing,
*>              receives nothing and posts nothing.
*> RUN AS     : KITDEMO CO=STC [SITE=BHM] [MODE=PREVIEW|COMMIT]
*>              [USER=FINCTRL] [ASAT=yyyymmdd] [QTY=2]
*>              (build\demo-data.ps1 runs every bin\x86\*DEMO.exe
*>              with CO= SITE= MODE= USER=)
*> OUTPUT     : run\spool\KITDEMO-<CO>-<stamp>.lst
*> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO, KITTRIAL,
*>              KITCREAT, KITRESV, KITISSUE, KITCOMPL, GENEALOG,
*>              KITCOST, DISASSY
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2016-07-22 PMK  CR2502 KIT DEMONSTRATION LOAD FOR THE PILOT
*> 2026-10-07 STERP KIT-006, KIT-008..KIT-010, KIT-015, KIT-019,
*>                  KIT-020, KIT-021, D-20
*>================================================================
*> REQ: KIT-006, KIT-008, KIT-009, KIT-010, KIT-015, KIT-019
*> REQ: KIT-020, KIT-021
 IDENTIFICATION DIVISION.
 PROGRAM-ID. KITDEMO.
 AUTHOR. P M KEANE.
 DATE-WRITTEN. 22/07/2016.
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
 FD  RPT-FILE REPORT IS R-KITDEMO.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY STRPTWS.
 COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==Ws-Cmdargs==.
 COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==Ws-Runpath==.
 COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==Ws-Joblog==.
 COPY KITTRIALCA REPLACING ==DFHCOMMAREA== BY ==Ws-Trial-Ca==
      LEADING ==CA-== BY ==TR-==.
 COPY KITCREATCA REPLACING ==DFHCOMMAREA== BY ==Ws-Creat-Ca==
      LEADING ==CA-== BY ==CR-==.
 COPY KITRESVCA REPLACING ==DFHCOMMAREA== BY ==Ws-Resv-Ca==
      LEADING ==CA-== BY ==RV-==.
 COPY KITISSUECA REPLACING ==DFHCOMMAREA== BY ==Ws-Issue-Ca==
      LEADING ==CA-== BY ==IS-==.
 COPY KITCOMPLCA REPLACING ==DFHCOMMAREA== BY ==Ws-Compl-Ca==
      LEADING ==CA-== BY ==CP-==.
 COPY KITCOSTCA REPLACING ==DFHCOMMAREA== BY ==Ws-Cost-Ca==
      LEADING ==CA-== BY ==KO-==.
 COPY GENEALOGCA REPLACING ==DFHCOMMAREA== BY ==Ws-Gen-Ca==
      LEADING ==CA-== BY ==GE-==.
 COPY DISASSYCA REPLACING ==DFHCOMMAREA== BY ==Ws-Dis-Ca==
      LEADING ==CA-== BY ==DA-==.
 01  Ws-Spool-Name              PIC X(200) VALUE SPACES.
 01  Ws-File-Status             PIC X(2) VALUE "00".
 01  Ws-Row.
     05  Ws-Col OCCURS 20 TIMES PIC X(100).
 01  Ws-Args.
     05  Ws-Co                  PIC X(10) VALUE SPACES.
     05  Ws-Site-Arg            PIC X(10) VALUE SPACES.
     05  Ws-Site                PIC X(10) VALUE SPACES.
     05  Ws-User                PIC X(20) VALUE SPACES.
     05  Ws-Mode                PIC X(10) VALUE SPACES.
         88  Ws-Commit                    VALUE "COMMIT".
     05  Ws-Asat                PIC 9(8) VALUE 0.
     05  Ws-Qty-Arg             PIC S9(4) VALUE 0.
*>   The operator who started the job is not an actor.  A kit is
*>   planned by engineering, moved by the stores and costed by the
*>   cost office, and D-07 does not stop being true because the
*>   demonstration dataset is loaded by the financial controller.
 01  Ws-Actors.
     05  Ws-Engineer            PIC X(20) VALUE "ENGINEER".
     05  Ws-Eng-Mgr             PIC X(20) VALUE "ENGMGR".
     05  Ws-Stores              PIC X(20) VALUE "WHSE".
     05  Ws-Inv-Mgr             PIC X(20) VALUE "INVMGR".
     05  Ws-Quality             PIC X(20) VALUE "QUALITY".
     05  Ws-Cost-Acc            PIC X(20) VALUE "COSTACC".
     05  Ws-Actor               PIC X(20) VALUE SPACES.
 01  Ws-Date-Work.
     05  Ws-Date-N              PIC 9(8) VALUE 0.
     05  Ws-Date-R REDEFINES Ws-Date-N.
         10  Ws-D-Yyyy          PIC 9(4).
         10  Ws-D-Mm            PIC 9(2).
         10  Ws-D-Dd            PIC 9(2).
 01  Ws-Run.
     05  Ws-Rc                  PIC S9(4) VALUE 0.
     05  Ws-Ix                  PIC S9(4) VALUE 0.
     05  Ws-Jx                  PIC S9(4) VALUE 0.
     05  Ws-Now                 PIC X(21) VALUE SPACES.
     05  Ws-More                PIC X VALUE "Y".
     05  Ws-Already             PIC X VALUE "N".
     05  Ws-Abandon             PIC X VALUE "N".
     05  Ws-Optional            PIC X VALUE "N".
     05  Ws-Seq                 PIC S9(4) VALUE 0.
     05  Ws-Call-Rc             PIC S9(4) VALUE 0.
     05  Ws-Call-Reason         PIC X(8) VALUE SPACES.
     05  Ws-Call-Msg            PIC X(200) VALUE SPACES.
     05  Ws-Num-Edit            PIC -(13)9.9(6).
*>   COM-015 / CR2741.  The stem is 24 characters at most, so that
*>   a service which derives one id per line (KITISSUE BACKFLSH,
*>   DISASSY POST) still has room for its own suffix inside the 36
*>   character commarea field.
 01  Ws-Corr-Work.
     05  Ws-Corr-Stem           PIC X(24) VALUE SPACES.
     05  Ws-Corr-Seq            PIC 9(3) VALUE 0.
     05  Ws-Step-Corr           PIC X(36) VALUE SPACES.
     05  Ws-Demo-Tag            PIC X(40) VALUE SPACES.
     05  Ws-Corr-Like           PIC X(40) VALUE SPACES.
 01  Ws-Kit.
     05  Ws-Kit-Ref             PIC X(30) VALUE SPACES.
     05  Ws-Parent              PIC X(40) VALUE SPACES.
     05  Ws-Parent-Rev          PIC X(4) VALUE SPACES.
     05  Ws-Build-Qty           PIC S9(9)V9(3) VALUE 0.
     05  Ws-First-Qty           PIC S9(9)V9(3) VALUE 0.
     05  Ws-Final-Qty           PIC S9(9)V9(3) VALUE 0.
     05  Ws-Max-Build           PIC S9(13)V9(6) VALUE 0.
     05  Ws-Whse                PIC X(20) VALUE SPACES.
     05  Ws-Loc                 PIC X(20) VALUE SPACES.
     05  Ws-Unit-Cost           PIC S9(11)V9(6) VALUE 0.
     05  Ws-Completion-Id       PIC S9(17) VALUE 0.
     05  Ws-Dis-Ref             PIC X(30) VALUE SPACES.
     05  Ws-Cost-Basis          PIC X(4) VALUE "STD".
     05  Ws-Retry-Fn            PIC X(8) VALUE SPACES.
     05  Ws-Gl-Posted           PIC X VALUE "N".
 01  Ws-Picks.
     05  Ws-Pick-Cnt            PIC S9(4) VALUE 0.
     05  Ws-Pick OCCURS 20 TIMES.
         10  Ws-P-Line          PIC S9(9) COMP-5.
         10  Ws-P-Item          PIC X(40).
         10  Ws-P-Qty           PIC S9(13)V9(6).
         10  Ws-P-Whse          PIC X(20).
         10  Ws-P-Loc           PIC X(20).
         10  Ws-P-Uom           PIC X(10).
*>   The bills the planner is prepared to try today, best first.
*>   OCCURS 10 because a demonstration that tries every bill in
*>   the company takes longer than anybody will watch (CR2502).
 01  Ws-Candidates.
     05  Ws-Cand-Cnt            PIC S9(4) VALUE 0.
     05  Ws-Cx                  PIC S9(4) VALUE 0.
     05  Ws-Chosen              PIC X VALUE "N".
     05  Ws-Cand OCCURS 10 TIMES.
         10  Ws-C-Item          PIC X(40).
         10  Ws-C-Rev           PIC X(4).
         10  Ws-C-Site          PIC X(10).
 01  Ws-Det.
     05  D-Seq                  PIC S9(4) VALUE 0.
     05  D-Step                 PIC X(22) VALUE SPACES.
     05  D-Actor                PIC X(10) VALUE SPACES.
     05  D-Func                 PIC X(10) VALUE SPACES.
     05  D-Ref                  PIC X(30) VALUE SPACES.
     05  D-Qty                  PIC S9(11)V9(3) VALUE 0.
     05  D-Value                PIC S9(11)V99 VALUE 0.
     05  D-Outcome              PIC X(22) VALUE SPACES.
 01  Ws-Tot.
     05  T-Steps                PIC S9(5) VALUE 0.
     05  T-Ok                   PIC S9(5) VALUE 0.
     05  T-Warn                 PIC S9(5) VALUE 0.
     05  T-Fail                 PIC S9(5) VALUE 0.
     05  T-Issued-Gbp           PIC S9(11)V99 VALUE 0.
     05  T-Completed-Gbp        PIC S9(11)V99 VALUE 0.
     05  T-Lines-Issued         PIC S9(5) VALUE 0.
 01  Ws-Head.
     05  H-Scope                PIC X(60) VALUE SPACES.
     05  H-Mode-Text            PIC X(40) VALUE SPACES.
 REPORT SECTION.
 RD  R-KITDEMO
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
         10  COLUMN 1   PIC X(4) VALUE " Seq".
         10  COLUMN 6   PIC X(22) VALUE "Step                  ".
         10  COLUMN 29  PIC X(10) VALUE "Actor     ".
         10  COLUMN 40  PIC X(10) VALUE "Function  ".
         10  COLUMN 51  PIC X(30) VALUE "Reference".
         10  COLUMN 82  PIC X(15) VALUE "       Quantity".
         10  COLUMN 98  PIC X(14) VALUE "    Value GBP".
         10  COLUMN 113 PIC X(19) VALUE "Outcome".
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(131) VALUE ALL "-".

 01  DE-LINE TYPE IS DETAIL.
     05  LINE PLUS 1.
         10  COLUMN 1   PIC Z(3)9 SOURCE D-Seq.
         10  COLUMN 6   PIC X(22) SOURCE D-Step.
         10  COLUMN 29  PIC X(10) SOURCE D-Actor.
         10  COLUMN 40  PIC X(10) SOURCE D-Func.
         10  COLUMN 51  PIC X(30) SOURCE D-Ref.
         10  COLUMN 82  PIC Z(9)9.999 SOURCE D-Qty.
         10  COLUMN 98  PIC Z(9)9.99- SOURCE D-Value.
         10  COLUMN 113 PIC X(19) SOURCE D-Outcome.

 01  CF-TOTALS TYPE IS CONTROL FOOTING FINAL.
     05  LINE PLUS 2.
         10  COLUMN 1   PIC X(131) VALUE ALL "=".
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(28) VALUE "Steps attempted             ".
         10  COLUMN 30  PIC Z(4)9 SOURCE T-Steps.
         10  COLUMN 40  PIC X(10) VALUE "accepted  ".
         10  COLUMN 51  PIC Z(4)9 SOURCE T-Ok.
         10  COLUMN 60  PIC X(10) VALUE "warnings  ".
         10  COLUMN 71  PIC Z(4)9 SOURCE T-Warn.
         10  COLUMN 80  PIC X(10) VALUE "refused   ".
         10  COLUMN 91  PIC Z(4)9 SOURCE T-Fail.
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(28) VALUE "Material lines issued       ".
         10  COLUMN 30  PIC Z(4)9 SOURCE T-Lines-Issued.
         10  COLUMN 40  PIC X(24) VALUE "Issued to work in progr.".
         10  COLUMN 66  PIC Z(9)9.99- SOURCE T-Issued-Gbp.
     05  LINE PLUS 1.
         10  COLUMN 1   PIC X(28) VALUE "Received into stock         ".
         10  COLUMN 30  PIC Z(9)9.99- SOURCE T-Completed-Gbp.

 PROCEDURE DIVISION.
*>================================================================
*> MAIN
*>================================================================
 0000-MAIN SECTION.
 0000-START.
     PERFORM 0100-READ-ARGS
     PERFORM 0200-JOB-START
     PERFORM 0300-OPEN-SPOOL
     IF Ws-Rc > 4
         GO TO 0000-FINISH
     END-IF
     PERFORM 0600-ALREADY-BUILT
     IF Ws-Already = "N"
         PERFORM 0650-CHOOSE-THE-KIT
     END-IF
     IF Ws-Already = "N" AND Ws-Abandon = "N"
         PERFORM 1000-TRIAL
     END-IF
     IF Ws-Already = "N" AND Ws-Abandon = "N" AND Ws-Commit
         PERFORM 1100-CREATE-THE-KIT
         IF Ws-Abandon = "N"
             PERFORM 1200-RESERVE-AND-PICK
         END-IF
         IF Ws-Abandon = "N"
             PERFORM 1300-ISSUE-THE-MATERIAL
         END-IF
         IF Ws-Abandon = "N"
             PERFORM 1400-COMPLETE-THE-KIT
         END-IF
         IF Ws-Abandon = "N"
             PERFORM 1500-GENEALOGY
             PERFORM 1600-KIT-COST
             PERFORM 1700-DISASSEMBLE
         END-IF
     END-IF
     IF T-Fail > 0 AND Ws-Rc < 4
         MOVE 4 TO Ws-Rc
     END-IF
     PERFORM 0400-CLOSE-SPOOL.
 0000-FINISH.
     PERFORM 0500-JOB-END
     MOVE Ws-Rc TO RETURN-CODE
     STOP RUN.

*>================================================================
*> ARGUMENTS.  CO= SITE= MODE= USER= are what demo-data.ps1 hands
*> to a driver it has never heard of; ASAT= and QTY= are for a
*> demonstrator who wants a particular day or a bigger run.
*>================================================================
 0100-READ-ARGS SECTION.
 0100-START.
     INITIALIZE Ws-Cmdargs
     MOVE "CMDLINE" TO CA-FUNCTION OF Ws-Cmdargs
     CALL "CMDARGS" USING Ws-Cmdargs
     MOVE CM-CO TO Ws-Co
     MOVE CM-SITE TO Ws-Site-Arg
     MOVE CM-USER TO Ws-User
     MOVE CM-MODE TO Ws-Mode
     IF CM-ASAT NOT = SPACES
         COMPUTE Ws-Asat = FUNCTION NUMVAL(CM-ASAT)
     END-IF
     MOVE "QTY" TO CM-LOOKUP-KEY
     PERFORM 0150-GET-ARG
     IF CM-LOOKUP-VALUE NOT = SPACES
         COMPUTE Ws-Qty-Arg = FUNCTION NUMVAL(CM-LOOKUP-VALUE)
     END-IF
     IF Ws-Co = SPACES
         MOVE "STC" TO Ws-Co
     END-IF
     IF Ws-User = SPACES
         MOVE "FINCTRL" TO Ws-User
     END-IF
     IF Ws-Mode NOT = "COMMIT"
         MOVE "PREVIEW" TO Ws-Mode
     END-IF
     IF Ws-Asat = 0
         MOVE FUNCTION CURRENT-DATE TO Ws-Now
         COMPUTE Ws-Asat = FUNCTION NUMVAL(Ws-Now(1:8))
     END-IF
     MOVE Ws-Site-Arg TO Ws-Site
     MOVE SPACES TO Ws-Demo-Tag
     STRING "KITDEMO-" Ws-Asat
         DELIMITED BY SIZE INTO Ws-Demo-Tag
     MOVE SPACES TO Ws-Corr-Stem
     STRING "KITDEMO-" FUNCTION TRIM(Ws-Co) "-" Ws-Asat
         DELIMITED BY SIZE INTO Ws-Corr-Stem
     MOVE SPACES TO Ws-Corr-Like
     STRING FUNCTION TRIM(Ws-Corr-Stem) "%"
         DELIMITED BY SIZE INTO Ws-Corr-Like
     DISPLAY "STERP-I-1870 KITDEMO " FUNCTION TRIM(Ws-Mode)
             " run for " FUNCTION TRIM(Ws-Co) " on " Ws-Asat
         UPON SYSOUT.
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

*>================================================================
*> JOB LOG, SPOOL FILE AND HEADINGS
*>================================================================
 0200-JOB-START SECTION.
 0200-START.
     INITIALIZE Ws-Joblog
     MOVE "START" TO CA-FUNCTION OF Ws-Joblog
     MOVE Ws-Co TO CA-COMPANY OF Ws-Joblog
     MOVE Ws-User TO CA-USER OF Ws-Joblog
     MOVE "KITDEMO" TO JL-JOB-NAME
     MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
     CALL "JOBLOG" USING Ws-Joblog.
 0200-EXIT.
     EXIT.

 0300-OPEN-SPOOL SECTION.
 0300-START.
     INITIALIZE Ws-Runpath
     MOVE "BUILD" TO CA-FUNCTION OF Ws-Runpath
     MOVE "SPOOL" TO RP-FOLDER
     MOVE "KITDEMO" TO RP-REPORT-ID
     MOVE Ws-Co TO RP-COMPANY-REF
     MOVE "Y" TO RP-USE-TIMESTAMP
     CALL "RUNPATH" USING Ws-Runpath
     IF CA-RETURN-CODE OF Ws-Runpath > 4
         MOVE 12 TO Ws-Rc
         DISPLAY "STERP-E-1871 cannot build the spool path"
             UPON SYSOUT
         GO TO 0300-EXIT
     END-IF
     MOVE RP-FULL-PATH TO Ws-Spool-Name
     PERFORM 0350-FILL-HEADINGS
     OPEN OUTPUT RPT-FILE
     IF Ws-File-Status NOT = "00"
         MOVE 12 TO Ws-Rc
         DISPLAY "STERP-E-1872 cannot open "
                 FUNCTION TRIM(Ws-Spool-Name) UPON SYSOUT
         GO TO 0300-EXIT
     END-IF
     INITIATE R-KITDEMO.
 0300-EXIT.
     EXIT.

 0350-FILL-HEADINGS SECTION.
 0350-START.
     MOVE Ws-Co TO RPT-COMPANY-CODE
     MOVE "KITDEMO" TO RPT-REPORT-ID
     MOVE "Kit demonstration: define to assemble" TO RPT-TITLE
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
     STRING "Business date " Ws-D-Dd "/" Ws-D-Mm "/" Ws-D-Yyyy
         DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
     IF Ws-Commit
         MOVE "COMMIT - the services post for real"
              TO H-Mode-Text
     ELSE
         MOVE "PREVIEW - feasibility only, nothing posts"
              TO H-Mode-Text
     END-IF
     MOVE H-Mode-Text TO RPT-SUBTITLE
     MOVE SPACES TO H-Scope
     IF Ws-Site-Arg = SPACES
         STRING "Any site with eligible stock" DELIMITED BY SIZE
             INTO H-Scope
     ELSE
         STRING "Requested site " FUNCTION TRIM(Ws-Site-Arg)
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
     MOVE 2 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-OK
         MOVE Ws-Col(1) TO RPT-COMPANY-NAME
         MOVE Ws-Col(2) TO RPT-REG-NUMBER
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

 0400-CLOSE-SPOOL SECTION.
 0400-START.
     TERMINATE R-KITDEMO
     CLOSE RPT-FILE
     DISPLAY "STERP-I-1878 " T-Steps " steps, " T-Ok
             " accepted, " T-Warn " warnings, " T-Fail " refused"
         UPON SYSOUT
     DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(Ws-Spool-Name)
         UPON SYSOUT.
 0400-EXIT.
     EXIT.

 0500-JOB-END SECTION.
 0500-START.
     INITIALIZE Ws-Joblog
     MOVE "END" TO CA-FUNCTION OF Ws-Joblog
     MOVE Ws-Co TO CA-COMPANY OF Ws-Joblog
     MOVE Ws-User TO CA-USER OF Ws-Joblog
     MOVE "KITDEMO" TO JL-JOB-NAME
     MOVE T-Steps TO JL-ROWS-PROCESSED
     MOVE Ws-Rc TO JL-RETURN-CODE
     MOVE Ws-Spool-Name TO JL-SPOOL-FILE
     EVALUATE TRUE
         WHEN Ws-Rc = 0
             MOVE "OK" TO JL-STATUS
         WHEN Ws-Rc < 8
             MOVE "WARNING" TO JL-STATUS
         WHEN OTHER
             MOVE "FAILED" TO JL-STATUS
     END-EVALUATE
     MOVE Ws-Kit-Ref TO JL-MESSAGE-TEXT
     CALL "JOBLOG" USING Ws-Joblog.
 0500-EXIT.
     EXIT.

*>================================================================
*> RE-RUNNABLE.  One demonstration kit per company per run date.
*> The kit reference itself comes from core.NumberSeries, so the
*> run is recognised by the correlation stem it stamped on the
*> execution kit and by the demand reference it quoted, never by
*> guessing a number that NUMGEN owns.
*>================================================================
 0600-ALREADY-BUILT SECTION.
 0600-START.
     MOVE "N" TO Ws-Already
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(12), COUNT(*)) FROM "
            "kit.ExecutionKit k WHERE k.CompanyCode = ? AND "
            "(k.CorrelationId LIKE ? OR k.SourceDemandRef = ?)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 3 TO SQLIO-PARM-CNT
     MOVE Ws-Co TO SQLIO-PARM-VAL(1)
     MOVE Ws-Corr-Like TO SQLIO-PARM-VAL(2)
     MOVE Ws-Demo-Tag TO SQLIO-PARM-VAL(3)
     PERFORM VARYING Ws-Ix FROM 1 BY 1 UNTIL Ws-Ix > 3
         MOVE "N" TO SQLIO-PARM-NULL(Ws-Ix)
     END-PERFORM
     MOVE 1 TO SQLIO-COL-CNT
     PERFORM 0370-WIDE-COLUMNS
     MOVE 2 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         MOVE 12 TO Ws-Rc
         MOVE "Y" TO Ws-Abandon
         DISPLAY "STERP-E-1873 " SQLIO-ERRMSG(1:80) UPON SYSOUT
         GO TO 0600-EXIT
     END-IF
     IF FUNCTION NUMVAL(Ws-Col(1)) > 0
         MOVE "Y" TO Ws-Already
         MOVE "Re-run" TO D-Step
         MOVE SPACES TO D-Actor
         MOVE "SKIP" TO D-Func
         MOVE Ws-Demo-Tag TO D-Ref
         MOVE 0 TO D-Qty D-Value
         MOVE "already on file" TO D-Outcome
         ADD 1 TO Ws-Seq
         MOVE Ws-Seq TO D-Seq
         ADD 1 TO T-Steps
         ADD 1 TO T-Ok
         GENERATE DE-LINE
         DISPLAY "STERP-I-1874 kit demonstration data for "
                 FUNCTION TRIM(Ws-Co) " on " Ws-Asat
                 " is already present - nothing to build"
             UPON SYSOUT
     END-IF.
 0600-EXIT.
     EXIT.

*>================================================================
*> CHOOSE THE KIT.  A released standard bill whose parent can be
*> received into stock, and a site where the components really
*> are.  The requested site wins when it can cover the kit; when
*> it cannot (BHM holds nothing but quarantined and inspection
*> stock) the driver says so and builds where the material is,
*> because a demonstration that refuses itself shows nothing.
*>================================================================
 0650-CHOOSE-THE-KIT SECTION.
 0650-START.
     MOVE 0 TO Ws-Cand-Cnt
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT TOP (10) h.ParentItemCode, "
            "ISNULL(h.ParentRevision,''), b.SiteCode FROM "
            "bom.BomHeader h JOIN bom.BomComponent c ON "
            "c.BomHeaderId = h.BomHeaderId JOIN inv.StockBalance "
            "b ON b.CompanyCode = h.CompanyCode AND b.ItemCode = "
            "c.ComponentItemCode JOIN inv.Location l ON "
            "l.CompanyCode = b.CompanyCode AND l.SiteCode = "
            "b.SiteCode AND l.WarehouseCode = b.WarehouseCode AND "
            "l.LocationCode = b.LocationCode JOIN inv.Item p ON "
            "p.CompanyCode = h.CompanyCode AND p.ItemCode = "
            "h.ParentItemCode WHERE h.CompanyCode = ? AND "
            "h.Status = 'RELEASED' AND h.Purpose = 'STANDARD' AND "
            "b.QtyOnHand > 0 AND b.QualityStatus = 'ACCEPTED' AND "
            "l.IsAvailable = 1 AND l.IsOwnedStock = 1 AND "
            "p.TraceabilityType = 'NONE' AND p.IsStocked = 1 "
            "GROUP BY h.ParentItemCode, h.ParentRevision, "
            "b.SiteCode HAVING COUNT(DISTINCT "
            "c.ComponentItemCode) = COUNT(DISTINCT CASE WHEN "
            "b.QtyOnHand > 0 THEN c.ComponentItemCode END) "
            "ORDER BY CASE WHEN b.SiteCode = ? THEN 0 ELSE 1 END, "
            "COUNT(DISTINCT c.ComponentItemCode) DESC, "
            "h.ParentItemCode"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 2 TO SQLIO-PARM-CNT
     MOVE Ws-Co TO SQLIO-PARM-VAL(1)
     MOVE Ws-Site-Arg TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE 3 TO SQLIO-COL-CNT
     PERFORM 0370-WIDE-COLUMNS
     MOVE 2 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         MOVE 12 TO Ws-Rc
         MOVE "Y" TO Ws-Abandon
         DISPLAY "STERP-E-1873 " SQLIO-ERRMSG(1:80) UPON SYSOUT
         GO TO 0650-EXIT
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE "FTCH" TO SQLIO-FUNC
         MOVE 2 TO SQLIO-CURSOR
         CALL "SQLIO" USING SQLIO-CA Ws-Row
         EVALUATE TRUE
             WHEN SQLIO-OK
                 IF Ws-Cand-Cnt < 10
                     ADD 1 TO Ws-Cand-Cnt
                     MOVE Ws-Col(1) TO Ws-C-Item(Ws-Cand-Cnt)
                     MOVE Ws-Col(2) TO Ws-C-Rev(Ws-Cand-Cnt)
                     MOVE Ws-Col(3) TO Ws-C-Site(Ws-Cand-Cnt)
                 END-IF
             WHEN SQLIO-NOTFOUND
                 MOVE "N" TO Ws-More
             WHEN OTHER
                 MOVE "N" TO Ws-More
                 MOVE 12 TO Ws-Rc
                 MOVE "Y" TO Ws-Abandon
         END-EVALUATE
     END-PERFORM
     MOVE "CLOS" TO SQLIO-FUNC
     MOVE 2 TO SQLIO-CURSOR
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF Ws-Cand-Cnt = 0
         MOVE 8 TO Ws-Rc
         MOVE "Y" TO Ws-Abandon
         DISPLAY "STERP-W-1875 no released bill with eligible "
                 "component stock for " FUNCTION TRIM(Ws-Co)
             UPON SYSOUT
     END-IF.
 0650-EXIT.
     EXIT.

*>   The location the finished assembly is received into, and the
*>   one a disassembly later takes it out of again.
 0680-FIND-STOCK-LOCATION SECTION.
 0680-START.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT TOP (1) b.WarehouseCode, b.LocationCode FROM "
            "inv.StockBalance b JOIN inv.Location l ON "
            "l.CompanyCode = b.CompanyCode AND l.SiteCode = "
            "b.SiteCode AND l.WarehouseCode = b.WarehouseCode AND "
            "l.LocationCode = b.LocationCode WHERE b.CompanyCode "
            "= ? AND b.SiteCode = ? AND b.QtyOnHand > 0 AND "
            "b.QualityStatus = 'ACCEPTED' AND l.IsAvailable = 1 "
            "AND l.IsOwnedStock = 1 AND l.LocationType = 'STOCK' "
            "ORDER BY l.PickSequence, l.LocationCode"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 2 TO SQLIO-PARM-CNT
     MOVE Ws-Co TO SQLIO-PARM-VAL(1)
     MOVE Ws-Site TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE 2 TO SQLIO-COL-CNT
     PERFORM 0370-WIDE-COLUMNS
     MOVE 2 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-OK
         MOVE Ws-Col(1) TO Ws-Whse
         MOVE Ws-Col(2) TO Ws-Loc
     END-IF
     IF Ws-Loc = SPACES
         DISPLAY "STERP-W-1876 no stock location at "
                 FUNCTION TRIM(Ws-Site) " - " SQLIO-ERRMSG(1:60)
             UPON SYSOUT
     END-IF.
 0680-EXIT.
     EXIT.

*>================================================================
*> KIT-006.  The trial commits nothing, so it runs in PREVIEW as
*> well as in COMMIT and it decides how many the stores can
*> actually build today.  A bill whose leaf components are read
*> through the subassemblies answers first; if that scenario can
*> build nothing a second scenario is recorded at the top level
*> rather than overwriting the first (KIT-006).
*>================================================================
 1000-TRIAL SECTION.
 1000-START.
     MOVE "Y" TO Ws-Optional
     MOVE "N" TO Ws-Chosen
     PERFORM VARYING Ws-Cx FROM 1 BY 1
             UNTIL Ws-Cx > Ws-Cand-Cnt OR Ws-Chosen = "Y"
         MOVE Ws-C-Item(Ws-Cx) TO Ws-Parent
         MOVE Ws-C-Rev(Ws-Cx) TO Ws-Parent-Rev
         MOVE Ws-C-Site(Ws-Cx) TO Ws-Site
         PERFORM 1010-PRIME-TRIAL
         MOVE "Y" TO KT-USE-SUBASSY
         MOVE "TRIAL" TO TR-FUNCTION
         CALL "KITTRIAL" USING Ws-Trial-Ca
         PERFORM 8100-FROM-TRIAL
         MOVE "Trial kitting" TO D-Step
         MOVE "TRIAL" TO D-Func
         MOVE Ws-Parent TO D-Ref
         MOVE KT-MAX-BUILDABLE TO D-Qty
         MOVE 0 TO D-Value
         PERFORM 8000-RECORD
         MOVE KT-MAX-BUILDABLE TO Ws-Max-Build
         IF KT-COMMITTED = "Y"
             DISPLAY "STERP-E-1877 a trial committed something - "
                     "KIT-006 is broken" UPON SYSOUT
             MOVE 8 TO Ws-Rc
         END-IF
*>       A second scenario of the same request, read at the top
*>       level instead of through the subassemblies.  KIT-006
*>       wants it recorded as a scenario of its own, never as an
*>       overwrite of the first answer.
         IF Ws-Max-Build < 1
             PERFORM 1010-PRIME-TRIAL
             MOVE "N" TO KT-USE-SUBASSY
             MOVE 2 TO KT-SCENARIO-NO
             MOVE "TRIAL" TO TR-FUNCTION
             CALL "KITTRIAL" USING Ws-Trial-Ca
             PERFORM 8100-FROM-TRIAL
             MOVE "Trial, top level only" TO D-Step
             MOVE "TRIAL" TO D-Func
             MOVE Ws-Parent TO D-Ref
             MOVE KT-MAX-BUILDABLE TO D-Qty
             MOVE 0 TO D-Value
             PERFORM 8000-RECORD
             MOVE KT-MAX-BUILDABLE TO Ws-Max-Build
         END-IF
         IF Ws-Max-Build >= 1
             MOVE 2 TO Ws-Build-Qty
             IF Ws-Qty-Arg > 0
                 MOVE Ws-Qty-Arg TO Ws-Build-Qty
             END-IF
             IF Ws-Max-Build < Ws-Build-Qty
                 MOVE Ws-Max-Build TO Ws-Build-Qty
             END-IF
*>           PREVIEW posts nothing, so the kit is only raised on a
*>           COMMIT run; a preview takes the first feasible bill.
             IF Ws-Commit
                 PERFORM 0680-FIND-STOCK-LOCATION
                 PERFORM 1050-RAISE-THE-KIT
                 IF Ws-Call-Rc < 8
                     MOVE "Y" TO Ws-Chosen
                 END-IF
             ELSE
                 MOVE "Y" TO Ws-Chosen
             END-IF
         END-IF
     END-PERFORM
     MOVE "N" TO Ws-Optional
     IF Ws-Chosen = "N" OR Ws-Build-Qty < 1
         MOVE "Y" TO Ws-Abandon
         DISPLAY "STERP-W-1875 no demonstration bill can be built "
                 "at " FUNCTION TRIM(Ws-Co) " today" UPON SYSOUT
         IF Ws-Rc < 4
             MOVE 4 TO Ws-Rc
         END-IF
         GO TO 1000-EXIT
     END-IF
     PERFORM 0680-FIND-STOCK-LOCATION
     IF Ws-Site-Arg NOT = SPACES AND Ws-Site NOT = Ws-Site-Arg
         DISPLAY "STERP-W-1876 site " FUNCTION TRIM(Ws-Site-Arg)
                 " holds no eligible stock for the kit - building "
                 "at " FUNCTION TRIM(Ws-Site) UPON SYSOUT
         MOVE SPACES TO H-Scope
         STRING "Requested " FUNCTION TRIM(Ws-Site-Arg)
                ", built at " FUNCTION TRIM(Ws-Site)
             DELIMITED BY SIZE INTO H-Scope
     END-IF
     MOVE "Kit selected" TO D-Step
     MOVE Ws-Engineer TO D-Actor
     MOVE "BOM" TO D-Func
     MOVE Ws-Parent TO D-Ref
     MOVE Ws-Max-Build TO D-Qty
     MOVE 0 TO D-Value
     MOVE "released bill" TO D-Outcome
     ADD 1 TO Ws-Seq
     MOVE Ws-Seq TO D-Seq
     ADD 1 TO T-Steps
     ADD 1 TO T-Ok
     GENERATE DE-LINE
*>   Two completions tell the better story - one partial, one
*>   final - but one is enough when that is all the stock allows.
     IF Ws-Build-Qty > 1
         COMPUTE Ws-First-Qty = Ws-Build-Qty - 1
     ELSE
         MOVE 0 TO Ws-First-Qty
     END-IF
     COMPUTE Ws-Final-Qty = Ws-Build-Qty - Ws-First-Qty
     IF NOT Ws-Commit
         DISPLAY "STERP-I-1879 PREVIEW: "
                 FUNCTION TRIM(Ws-Parent) " could build "
                 Ws-Max-Build " at " FUNCTION TRIM(Ws-Site)
                 " - nothing committed" UPON SYSOUT
     END-IF.
 1000-EXIT.
     EXIT.

 1010-PRIME-TRIAL SECTION.
 1010-START.
     INITIALIZE Ws-Trial-Ca
     MOVE Ws-Engineer TO Ws-Actor
     PERFORM 8010-NEXT-CORR
     MOVE "KITTRIAL" TO TR-PROGRAM
     MOVE Ws-Co TO TR-COMPANY
     MOVE Ws-Site TO TR-SITE
     MOVE Ws-Actor TO TR-USER
     MOVE Ws-Asat TO TR-BUSINESS-DATE
     MOVE Ws-Step-Corr TO TR-CORRELATION-ID
     MOVE Ws-Parent TO KT-PARENT-ITEM
     MOVE Ws-Parent-Rev TO KT-PARENT-REV
     MOVE Ws-Site TO KT-SITE
     MOVE 2 TO KT-REQUESTED-QTY
     IF Ws-Qty-Arg > 0
         MOVE Ws-Qty-Arg TO KT-REQUESTED-QTY
     END-IF
     MOVE Ws-Asat TO KT-REQ-DATE
     MOVE Ws-Asat TO KT-SELECTION-DATE
     MOVE "STOCK" TO KT-DEMAND-TYPE
     MOVE Ws-Demo-Tag TO KT-DEMAND-REF
     MOVE "N" TO KT-USE-INBOUND
     MOVE 1 TO KT-SCENARIO-NO
     MOVE 20 TO KT-WIN-MAX.
 1010-EXIT.
     EXIT.

*>================================================================
*> KIT-002 / KIT-004.  Raise the kit against the bill the trial
*> has just proved.  A bill that cannot produce a released
*> requirement is not an argument: the planner simply tries the
*> next one, which is why this runs inside the candidate loop.
*>================================================================
 1050-RAISE-THE-KIT SECTION.
 1050-START.
     MOVE Ws-Engineer TO Ws-Actor
     PERFORM 8200-CREAT-HEADER
     MOVE "STOCKASSY" TO KC-KIT-KIND
     MOVE Ws-Parent TO KC-PARENT-ITEM
     MOVE Ws-Parent-Rev TO KC-PARENT-REV
     MOVE Ws-Site TO KC-SITE
     MOVE Ws-Build-Qty TO KC-PLANNED-QTY
     MOVE "STOCK" TO KC-SOURCE-TYPE
     MOVE Ws-Demo-Tag TO KC-SOURCE-REF
*>   BOMEXPL is asked for the PRODUCTION view of the bill, and a
*>   production bill has to be named by the job it is for, so the
*>   demonstration run is its own job reference (BOM0208).  When
*>   no production view exists KITCREAT reads the standard bill.
     MOVE Ws-Demo-Tag TO KC-JOB-REF
     MOVE Ws-Asat TO KC-NEED-DATE
     MOVE Ws-Asat TO KC-START-DATE
     MOVE Ws-Asat TO KC-SELECTION-DATE
*>   D-20.  Backflush stays switched off on the demonstration kit:
*>   KITCOMPL backflushes whenever the kit carries the flag, and a
*>   second pass over material the stores have already issued by
*>   hand would be an overissue.  The flag is what KITISSUE
*>   BACKFLSH checks first, so the driver's backflush step shows
*>   the KIT0013 refusal - backflush is never implicit.
     MOVE "N" TO KC-BACKFLUSH
     MOVE "Y" TO KC-EXPAND-PHANTOM
     MOVE Ws-Engineer TO KC-OWNER-USER
     MOVE "CREATE" TO CR-FUNCTION
     CALL "KITCREAT" USING Ws-Creat-Ca
     PERFORM 8110-FROM-CREAT
     MOVE "Kit created" TO D-Step
     MOVE "CREATE" TO D-Func
     MOVE KC-KIT-REF TO D-Ref
     MOVE KC-PLANNED-QTY TO D-Qty
     MOVE KC-PLANNED-COST TO D-Value
     PERFORM 8000-RECORD
     IF Ws-Call-Rc < 8
         MOVE KC-KIT-REF TO Ws-Kit-Ref
         DISPLAY "STERP-I-1870 kit " FUNCTION TRIM(Ws-Kit-Ref)
                 " raised for " Ws-Build-Qty " of "
                 FUNCTION TRIM(Ws-Parent) UPON SYSOUT
     END-IF.
 1050-EXIT.
     EXIT.

*>================================================================
*> KIT-002.  Review, then release.  The release gate is a real
*> segregation of duties control: the engineering manager asks for
*> it, and when the permission is not held the service answers
*> 06 / KIT0009 and the kit stays reviewed.  That is the control
*> working, not a failure, so it is recorded as a hold and the run
*> carries on.
*>================================================================
 1100-CREATE-THE-KIT SECTION.
 1100-START.
     MOVE Ws-Engineer TO Ws-Actor
     PERFORM 8200-CREAT-HEADER
     MOVE Ws-Kit-Ref TO KC-KIT-REF
     MOVE "REVIEW" TO CR-FUNCTION
     CALL "KITCREAT" USING Ws-Creat-Ca
     PERFORM 8110-FROM-CREAT
     MOVE "Kit reviewed" TO D-Step
     MOVE "REVIEW" TO D-Func
     MOVE Ws-Kit-Ref TO D-Ref
     MOVE 0 TO D-Qty
     MOVE 0 TO D-Value
     PERFORM 8000-RECORD
     MOVE Ws-Eng-Mgr TO Ws-Actor
     PERFORM 8200-CREAT-HEADER
     MOVE Ws-Kit-Ref TO KC-KIT-REF
     MOVE "RELEASE" TO CR-FUNCTION
     MOVE "Y" TO Ws-Optional
     CALL "KITCREAT" USING Ws-Creat-Ca
     PERFORM 8110-FROM-CREAT
     MOVE "Kit released" TO D-Step
     MOVE "RELEASE" TO D-Func
     MOVE Ws-Kit-Ref TO D-Ref
     MOVE 0 TO D-Qty
     MOVE 0 TO D-Value
     PERFORM 8000-RECORD
     MOVE "N" TO Ws-Optional.
 1100-EXIT.
     EXIT.

*>================================================================
*> KIT-008 / KIT-009.  Reserve the whole kit, print the picking
*> list, then take the material off the shelf.  A reservation has
*> not moved anything and a pick is still stock: only the issue
*> that follows turns it into work in progress.
*>================================================================
 1200-RESERVE-AND-PICK SECTION.
 1200-START.
     PERFORM 8210-RESV-HEADER
     MOVE Ws-Kit-Ref TO KR-KIT-REF
     MOVE Ws-Site TO KR-SITE
     MOVE 0 TO KR-LINE-NUM
     MOVE "Y" TO KR-ALLOW-PARTIAL
     MOVE Ws-Asat TO KR-REQUIRED-DATE
     MOVE "RESERVE" TO RV-FUNCTION
     CALL "KITRESV" USING Ws-Resv-Ca
     PERFORM 8120-FROM-RESV
     MOVE "Stock reserved" TO D-Step
     MOVE "RESERVE" TO D-Func
     MOVE Ws-Kit-Ref TO D-Ref
     MOVE KR-RESERVED-QTY TO D-Qty
     MOVE 0 TO D-Value
     PERFORM 8000-RECORD
     IF Ws-Call-Rc > 6
         MOVE "Y" TO Ws-Abandon
         GO TO 1200-EXIT
     END-IF
     PERFORM 1220-PICKING-LIST
     PERFORM 1240-PICK-THE-LINES.
 1200-EXIT.
     EXIT.

*>   KIT-022.  The picking list says where the material is; the
*>   driver keeps it because PICK and ISSUE both need the bin the
*>   storeman actually went to.
 1220-PICKING-LIST SECTION.
 1220-START.
     MOVE 0 TO Ws-Pick-Cnt
     PERFORM 8210-RESV-HEADER
     MOVE Ws-Kit-Ref TO KR-KIT-REF
     MOVE Ws-Site TO KR-SITE
     MOVE 0 TO KR-LINE-NUM
     MOVE 1 TO KR-FROM-LINE
     MOVE 20 TO KR-WIN-MAX
     MOVE "PICKLIST" TO RV-FUNCTION
     CALL "KITRESV" USING Ws-Resv-Ca
     PERFORM 8120-FROM-RESV
     MOVE "Picking list" TO D-Step
     MOVE "PICKLIST" TO D-Func
     MOVE Ws-Kit-Ref TO D-Ref
     MOVE KR-WIN-CNT TO D-Qty
     MOVE 0 TO D-Value
     PERFORM 8000-RECORD
     IF Ws-Call-Rc > 6
         GO TO 1220-EXIT
     END-IF
     PERFORM VARYING Ws-Ix FROM 1 BY 1
             UNTIL Ws-Ix > KR-WIN-CNT OR Ws-Ix > 20
         IF KR-P-TO-PICK(Ws-Ix) > 0
             ADD 1 TO Ws-Pick-Cnt
             MOVE KR-P-LINE-NUM(Ws-Ix) TO Ws-P-Line(Ws-Pick-Cnt)
             MOVE KR-P-ITEM(Ws-Ix) TO Ws-P-Item(Ws-Pick-Cnt)
             MOVE KR-P-TO-PICK(Ws-Ix) TO Ws-P-Qty(Ws-Pick-Cnt)
             MOVE KR-P-UOM(Ws-Ix) TO Ws-P-Uom(Ws-Pick-Cnt)
             IF KR-P-WAREHOUSE(Ws-Ix) = SPACES
                 MOVE Ws-Whse TO Ws-P-Whse(Ws-Pick-Cnt)
             ELSE
                 MOVE KR-P-WAREHOUSE(Ws-Ix)
                      TO Ws-P-Whse(Ws-Pick-Cnt)
             END-IF
             MOVE KR-P-LOCATION(Ws-Ix) TO Ws-P-Loc(Ws-Pick-Cnt)
*>           The picking list names the line, not always the bin.
*>           Stores cannot issue from "somewhere", so the bin the
*>           component is really in is read here and carried
*>           through the pick and the issue (STK0002).
             IF Ws-P-Loc(Ws-Pick-Cnt) = SPACES
                 PERFORM 1230-FIND-ITEM-LOCATION
             END-IF
         END-IF
     END-PERFORM.
 1220-EXIT.
     EXIT.

 1230-FIND-ITEM-LOCATION SECTION.
 1230-START.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT TOP (1) b.WarehouseCode, b.LocationCode FROM "
            "inv.StockBalance b JOIN inv.Location l ON "
            "l.CompanyCode = b.CompanyCode AND l.SiteCode = "
            "b.SiteCode AND l.WarehouseCode = b.WarehouseCode AND "
            "l.LocationCode = b.LocationCode WHERE b.CompanyCode "
            "= ? AND b.SiteCode = ? AND b.ItemCode = ? AND "
            "b.QtyOnHand > 0 AND b.QualityStatus = 'ACCEPTED' AND "
            "l.IsAvailable = 1 AND l.IsOwnedStock = 1 ORDER BY "
            "b.QtyOnHand DESC"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 3 TO SQLIO-PARM-CNT
     MOVE Ws-Co TO SQLIO-PARM-VAL(1)
     MOVE Ws-Site TO SQLIO-PARM-VAL(2)
     MOVE Ws-P-Item(Ws-Pick-Cnt) TO SQLIO-PARM-VAL(3)
     PERFORM VARYING Ws-Jx FROM 1 BY 1 UNTIL Ws-Jx > 3
         MOVE "N" TO SQLIO-PARM-NULL(Ws-Jx)
     END-PERFORM
     MOVE 2 TO SQLIO-COL-CNT
     PERFORM 0370-WIDE-COLUMNS
     MOVE 2 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-OK AND Ws-Col(2) NOT = SPACES
         MOVE Ws-Col(1) TO Ws-P-Whse(Ws-Pick-Cnt)
         MOVE Ws-Col(2) TO Ws-P-Loc(Ws-Pick-Cnt)
     END-IF.
 1230-EXIT.
     EXIT.

 1240-PICK-THE-LINES SECTION.
 1240-START.
     PERFORM VARYING Ws-Ix FROM 1 BY 1 UNTIL Ws-Ix > Ws-Pick-Cnt
         PERFORM 8210-RESV-HEADER
         MOVE Ws-Kit-Ref TO KR-KIT-REF
         MOVE Ws-Site TO KR-SITE
         MOVE Ws-P-Line(Ws-Ix) TO KR-LINE-NUM
         MOVE Ws-P-Item(Ws-Ix) TO KR-ITEM-CODE
         MOVE Ws-P-Qty(Ws-Ix) TO KR-QUANTITY
         MOVE Ws-P-Uom(Ws-Ix) TO KR-UOM-CODE
         MOVE Ws-P-Whse(Ws-Ix) TO KR-WAREHOUSE
         MOVE Ws-P-Loc(Ws-Ix) TO KR-LOCATION
         MOVE Ws-Asat TO KR-REQUIRED-DATE
         MOVE "PICK" TO RV-FUNCTION
         MOVE "Y" TO Ws-Optional
         CALL "KITRESV" USING Ws-Resv-Ca
         PERFORM 8120-FROM-RESV
         MOVE "Material picked" TO D-Step
         MOVE "PICK" TO D-Func
         MOVE Ws-P-Item(Ws-Ix) TO D-Ref
         MOVE KR-PICKED-QTY TO D-Qty
         MOVE 0 TO D-Value
         PERFORM 8000-RECORD
         MOVE "N" TO Ws-Optional
     END-PERFORM.
 1240-EXIT.
     EXIT.

*>================================================================
*> KIT-010 / D-20.  The kit is flagged for backflush and every
*> component of the demonstration bill is untraced, so one
*> BACKFLSH pass issues the lot - and each line inside it gets its
*> own derived correlation id (CR2741), which is why the second
*> and later components are really issued instead of being taken
*> for a replay of the first.  Whatever backflush will not touch
*> (a traced line) is issued by hand afterwards.
*>================================================================
 1300-ISSUE-THE-MATERIAL SECTION.
 1300-START.
*>   Issue what the storeman actually picked, line by line, from
*>   the bin he took it out of.  KIT-010: the issue is what turns
*>   stock into work in progress, and it is the pick that says
*>   where the material came from.
     PERFORM VARYING Ws-Ix FROM 1 BY 1 UNTIL Ws-Ix > Ws-Pick-Cnt
         PERFORM 8220-ISSUE-HEADER
         MOVE Ws-Kit-Ref TO KS-KIT-REF
         MOVE Ws-Site TO KS-SITE
         MOVE Ws-P-Line(Ws-Ix) TO KS-LINE-NUM
         MOVE Ws-P-Item(Ws-Ix) TO KS-ITEM-CODE
         MOVE Ws-P-Qty(Ws-Ix) TO KS-QUANTITY
         MOVE Ws-P-Uom(Ws-Ix) TO KS-UOM-CODE
         MOVE Ws-P-Whse(Ws-Ix) TO KS-WAREHOUSE
         MOVE Ws-P-Loc(Ws-Ix) TO KS-LOCATION
         MOVE Ws-Asat TO KS-ACTUAL-DATE
         MOVE Ws-Asat TO KS-POSTING-DATE
         MOVE "Issued to the kit from the picked bin."
              TO KS-REASON-TEXT
         MOVE "ISSUE" TO IS-FUNCTION
         CALL "KITISSUE" USING Ws-Issue-Ca
         PERFORM 8130-FROM-ISSUE
         MOVE "Component issued" TO D-Step
         MOVE "ISSUE" TO D-Func
         MOVE Ws-P-Item(Ws-Ix) TO D-Ref
         MOVE KS-QUANTITY TO D-Qty
         MOVE KS-VALUE-GBP TO D-Value
         PERFORM 8000-RECORD
         IF Ws-Call-Rc < 8
             ADD 1 TO T-Lines-Issued
             MOVE KS-WIP-VALUE TO T-Issued-Gbp
         END-IF
     END-PERFORM
     PERFORM 1340-BACKFLUSH-THE-REST.
 1300-EXIT.
     EXIT.

*>   D-20 / KIT-010.  The kit is flagged for backflush and every
*>   component of the demonstration bill is untraced, so one pass
*>   sweeps up whatever the counter issues left - and each line
*>   inside it carries a correlation id derived from the stem
*>   (CR2741), which is why the second and later components are
*>   really issued instead of being taken for a replay of the
*>   first.  The pass is checked first: KIT-010 forbids a
*>   backflush that half works, so the driver asks before it acts.
 1340-BACKFLUSH-THE-REST SECTION.
 1340-START.
     PERFORM 8220-ISSUE-HEADER
     MOVE Ws-Kit-Ref TO KS-KIT-REF
     MOVE Ws-Site TO KS-SITE
     MOVE Ws-Whse TO KS-WAREHOUSE
     MOVE Ws-Loc TO KS-LOCATION
     MOVE Ws-Build-Qty TO KS-GOOD-QTY
     MOVE Ws-Asat TO KS-ACTUAL-DATE
     MOVE Ws-Asat TO KS-POSTING-DATE
     MOVE "Y" TO KS-CHECK-ONLY
     MOVE "Backflush check for the demonstration run."
          TO KS-REASON-TEXT
     MOVE "BACKFLSH" TO IS-FUNCTION
     MOVE "Y" TO Ws-Optional
     CALL "KITISSUE" USING Ws-Issue-Ca
     PERFORM 8130-FROM-ISSUE
     MOVE "Backflush checked" TO D-Step
     MOVE "BACKFLSH" TO D-Func
     MOVE Ws-Kit-Ref TO D-Ref
     MOVE KS-LINES-DONE TO D-Qty
     MOVE 0 TO D-Value
     PERFORM 8000-RECORD
     IF Ws-Call-Rc > 4
         MOVE "N" TO Ws-Optional
         GO TO 1340-EXIT
     END-IF
     PERFORM 8220-ISSUE-HEADER
     MOVE Ws-Kit-Ref TO KS-KIT-REF
     MOVE Ws-Site TO KS-SITE
     MOVE Ws-Whse TO KS-WAREHOUSE
     MOVE Ws-Loc TO KS-LOCATION
     MOVE Ws-Build-Qty TO KS-GOOD-QTY
     MOVE Ws-Asat TO KS-ACTUAL-DATE
     MOVE Ws-Asat TO KS-POSTING-DATE
     MOVE "N" TO KS-CHECK-ONLY
     MOVE "Backflushed on completion of the demonstration run."
          TO KS-REASON-TEXT
     MOVE "BACKFLSH" TO IS-FUNCTION
     CALL "KITISSUE" USING Ws-Issue-Ca
     PERFORM 8130-FROM-ISSUE
     MOVE "Components backflushed" TO D-Step
     MOVE "BACKFLSH" TO D-Func
     MOVE Ws-Kit-Ref TO D-Ref
     MOVE KS-LINES-DONE TO D-Qty
     MOVE KS-WIP-VALUE TO D-Value
     PERFORM 8000-RECORD
     MOVE "N" TO Ws-Optional
     IF Ws-Call-Rc < 8
         ADD KS-LINES-DONE TO T-Lines-Issued
         MOVE KS-WIP-VALUE TO T-Issued-Gbp
     END-IF.
 1340-EXIT.
     EXIT.

*>   Read the kit back and issue, by hand, any line the backflush
*>   left short.  When there is nothing left the same call is made
*>   with KS-CHECK-ONLY so that the listing still shows the manual
*>   issue path being exercised and nothing is issued twice.
 1320-ISSUE-WHAT-IS-LEFT SECTION.
 1320-START.
     MOVE 0 TO Ws-Jx
     MOVE Ws-Stores TO Ws-Actor
     PERFORM 8200-CREAT-HEADER
     MOVE Ws-Kit-Ref TO KC-KIT-REF
     MOVE 1 TO KC-FROM-LINE
     MOVE 20 TO KC-WIN-MAX
     MOVE "ENQUIRE" TO CR-FUNCTION
     CALL "KITCREAT" USING Ws-Creat-Ca
     IF CR-RETURN-CODE < 8
         PERFORM 8200-CREAT-HEADER
         MOVE Ws-Kit-Ref TO KC-KIT-REF
         MOVE 1 TO KC-FROM-LINE
         MOVE 20 TO KC-WIN-MAX
         MOVE "GETLINE" TO CR-FUNCTION
         CALL "KITCREAT" USING Ws-Creat-Ca
         IF CR-RETURN-CODE < 8
             PERFORM VARYING Ws-Ix FROM 1 BY 1
                     UNTIL Ws-Ix > KC-WIN-CNT OR Ws-Ix > 20
                 IF Ws-Jx = 0 AND KC-L-REMAINING(Ws-Ix) > 0
                     MOVE Ws-Ix TO Ws-Jx
                 END-IF
             END-PERFORM
         END-IF
     END-IF
     PERFORM 8220-ISSUE-HEADER
     MOVE Ws-Kit-Ref TO KS-KIT-REF
     MOVE Ws-Site TO KS-SITE
     MOVE Ws-Whse TO KS-WAREHOUSE
     MOVE Ws-Loc TO KS-LOCATION
     MOVE Ws-Asat TO KS-ACTUAL-DATE
     MOVE Ws-Asat TO KS-POSTING-DATE
     IF Ws-Jx > 0
         MOVE KC-L-LINE-NUM(Ws-Jx) TO KS-LINE-NUM
         MOVE KC-L-ITEM(Ws-Jx) TO KS-ITEM-CODE
         MOVE KC-L-REMAINING(Ws-Jx) TO KS-QUANTITY
         MOVE KC-L-UOM(Ws-Jx) TO KS-UOM-CODE
         MOVE "N" TO KS-CHECK-ONLY
         MOVE "Issued over the counter to finish the kit."
              TO KS-REASON-TEXT
         MOVE "Component issued" TO D-Step
         MOVE KC-L-ITEM(Ws-Jx) TO D-Ref
     ELSE
         MOVE 1 TO KS-LINE-NUM
         MOVE 1 TO KS-QUANTITY
         MOVE "Y" TO KS-CHECK-ONLY
         MOVE "Counter issue rehearsed; backflush left nothing."
              TO KS-REASON-TEXT
         MOVE "Counter issue checked" TO D-Step
         MOVE Ws-Kit-Ref TO D-Ref
     END-IF
     MOVE "ISSUE" TO IS-FUNCTION
     MOVE "Y" TO Ws-Optional
     CALL "KITISSUE" USING Ws-Issue-Ca
     PERFORM 8130-FROM-ISSUE
     MOVE "ISSUE" TO D-Func
     MOVE KS-QUANTITY TO D-Qty
     MOVE KS-VALUE-GBP TO D-Value
     PERFORM 8000-RECORD
     MOVE "N" TO Ws-Optional
     IF Ws-Jx > 0 AND Ws-Call-Rc < 8
         ADD 1 TO T-Lines-Issued
         MOVE KS-WIP-VALUE TO T-Issued-Gbp
     END-IF.
 1320-EXIT.
     EXIT.

*>================================================================
*> KIT-015 / KIT-019.  COMPCHK judges without receipting, then the
*> output is received: a partial completion first when the run is
*> big enough, and a final one that takes the residual work in
*> progress out.  The ledger entry is the service's own business
*> (KIT.COMPLETE); if the environment cannot post it the driver
*> falls back to NOGL and says so rather than abandoning the
*> demonstration data.
*>================================================================
 1400-COMPLETE-THE-KIT SECTION.
 1400-START.
     PERFORM 8230-COMPL-HEADER
     MOVE Ws-Build-Qty TO KM-GOOD-QTY
     MOVE "Y" TO KM-CHECK-ONLY
     MOVE "NOGL" TO KM-COST-BASIS
     MOVE "COMPCHK" TO CP-FUNCTION
     CALL "KITCOMPL" USING Ws-Compl-Ca
     PERFORM 8140-FROM-COMPL
     MOVE "Completion checked" TO D-Step
     MOVE "COMPCHK" TO D-Func
     MOVE Ws-Kit-Ref TO D-Ref
     MOVE KM-REMAINING-OUT TO D-Qty
     MOVE 0 TO D-Value
     PERFORM 8000-RECORD
     IF Ws-Call-Rc > 6
         MOVE "Y" TO Ws-Abandon
         GO TO 1400-EXIT
     END-IF
     IF Ws-First-Qty > 0
         PERFORM 8230-COMPL-HEADER
         MOVE Ws-First-Qty TO KM-GOOD-QTY
         MOVE "N" TO KM-CHECK-ONLY
         MOVE Ws-Cost-Basis TO KM-COST-BASIS
         MOVE "First off the line." TO KM-REASON-TEXT
         MOVE "COMPLETE" TO CP-FUNCTION
         CALL "KITCOMPL" USING Ws-Compl-Ca
         PERFORM 8140-FROM-COMPL
         IF Ws-Call-Rc > 6 AND KM-COMPLETION-ID = 0
             PERFORM 1450-RETRY-WITHOUT-LEDGER
         END-IF
         MOVE "Output received" TO D-Step
         MOVE "COMPLETE" TO D-Func
         MOVE Ws-Kit-Ref TO D-Ref
         MOVE KM-GOOD-QTY TO D-Qty
         MOVE KM-WIP-TRANSFER TO D-Value
         PERFORM 8000-RECORD
         IF Ws-Call-Rc < 8
             ADD KM-WIP-TRANSFER TO T-Completed-Gbp
             MOVE KM-COMPLETION-ID TO Ws-Completion-Id
             MOVE KM-UNIT-COST TO Ws-Unit-Cost
         END-IF
     END-IF
     PERFORM 8230-COMPL-HEADER
     MOVE Ws-Final-Qty TO KM-GOOD-QTY
     MOVE "N" TO KM-CHECK-ONLY
     MOVE "Y" TO KM-FINAL
     MOVE Ws-Cost-Basis TO KM-COST-BASIS
     MOVE "The last of the run." TO KM-REASON-TEXT
     MOVE "FINAL" TO CP-FUNCTION
     CALL "KITCOMPL" USING Ws-Compl-Ca
     PERFORM 8140-FROM-COMPL
     IF Ws-Call-Rc > 6 AND KM-COMPLETION-ID = 0
         PERFORM 1450-RETRY-WITHOUT-LEDGER
     END-IF
     MOVE "Final completion" TO D-Step
     MOVE "FINAL" TO D-Func
     MOVE Ws-Kit-Ref TO D-Ref
     MOVE KM-GOOD-QTY TO D-Qty
     MOVE KM-WIP-TRANSFER TO D-Value
     PERFORM 8000-RECORD
     IF Ws-Call-Rc < 8
         ADD KM-WIP-TRANSFER TO T-Completed-Gbp
         MOVE KM-COMPLETION-ID TO Ws-Completion-Id
         MOVE KM-UNIT-COST TO Ws-Unit-Cost
     ELSE
         MOVE "Y" TO Ws-Abandon
     END-IF.
 1400-EXIT.
     EXIT.

*>   A completion that cannot reach the ledger (a closed period, a
*>   business event nobody mapped) is still a completion the stores
*>   made.  It is repeated without the ledger leg, on a correlation
*>   id of its own, and the listing records that the general ledger
*>   was left to the bridge.
 1450-RETRY-WITHOUT-LEDGER SECTION.
 1450-START.
     DISPLAY "STERP-W-1877 completion could not post the ledger ("
             FUNCTION TRIM(Ws-Call-Reason)
             ") - retrying without it" UPON SYSOUT
     MOVE CP-FUNCTION TO Ws-Retry-Fn
     MOVE "NOGL" TO Ws-Cost-Basis
     MOVE "Y" TO Ws-Gl-Posted
     PERFORM 8230-COMPL-HEADER
     MOVE "NOGL" TO KM-COST-BASIS
     MOVE "N" TO KM-CHECK-ONLY
     IF Ws-Retry-Fn = "FINAL"
         MOVE Ws-Final-Qty TO KM-GOOD-QTY
         MOVE "Y" TO KM-FINAL
         MOVE "FINAL" TO CP-FUNCTION
     ELSE
         MOVE Ws-First-Qty TO KM-GOOD-QTY
         MOVE "COMPLETE" TO CP-FUNCTION
     END-IF
     MOVE "Received without a ledger entry." TO KM-REASON-TEXT
     CALL "KITCOMPL" USING Ws-Compl-Ca
     PERFORM 8140-FROM-COMPL.
 1450-EXIT.
     EXIT.

*>================================================================
*> KIT-019.  Forward genealogy: what went into what came out.
*>================================================================
 1500-GENEALOGY SECTION.
 1500-START.
     INITIALIZE Ws-Gen-Ca
     MOVE Ws-Quality TO Ws-Actor
     PERFORM 8010-NEXT-CORR
     MOVE "GENEALOG" TO GE-PROGRAM
     MOVE Ws-Co TO GE-COMPANY
     MOVE Ws-Site TO GE-SITE
     MOVE Ws-Actor TO GE-USER
     MOVE Ws-Asat TO GE-BUSINESS-DATE
     MOVE Ws-Step-Corr TO GE-CORRELATION-ID
     MOVE "FORWARD" TO GN-DIRECTION
     MOVE Ws-Kit-Ref TO GN-KIT-REF
     MOVE Ws-Parent TO GN-ITEM-CODE
     MOVE Ws-Completion-Id TO GN-COMPLETION-ID
     MOVE 5 TO GN-MAX-DEPTH
     MOVE 1 TO GN-FROM-LINE
     MOVE 20 TO GN-WIN-MAX
     MOVE "Y" TO GN-INCLUDE-RET
     MOVE "FORWARD" TO GE-FUNCTION
     MOVE "Y" TO Ws-Optional
     CALL "GENEALOG" USING Ws-Gen-Ca
     MOVE GE-RETURN-CODE TO Ws-Call-Rc
     MOVE GE-REASON-CODE TO Ws-Call-Reason
     MOVE GE-MESSAGE TO Ws-Call-Msg
     MOVE "Genealogy" TO D-Step
     MOVE "FORWARD" TO D-Func
     MOVE Ws-Kit-Ref TO D-Ref
     MOVE GN-LINE-TOTAL TO D-Qty
     MOVE 0 TO D-Value
     PERFORM 8000-RECORD
     MOVE "N" TO Ws-Optional.
 1500-EXIT.
     EXIT.

*>================================================================
*> KIT-020.  What the run cost, read by the cost office.
*>================================================================
 1600-KIT-COST SECTION.
 1600-START.
     INITIALIZE Ws-Cost-Ca
     MOVE Ws-Cost-Acc TO Ws-Actor
     PERFORM 8010-NEXT-CORR
     MOVE "KITCOST" TO KO-PROGRAM
     MOVE Ws-Co TO KO-COMPANY
     MOVE Ws-Site TO KO-SITE
     MOVE Ws-Actor TO KO-USER
     MOVE Ws-Asat TO KO-BUSINESS-DATE
     MOVE Ws-Step-Corr TO KO-CORRELATION-ID
     MOVE Ws-Kit-Ref TO KK-KIT-REF
     MOVE Ws-Asat TO KK-AS-AT
     MOVE "GBP" TO KK-CURRENCY
     MOVE "COST" TO KO-FUNCTION
     MOVE "Y" TO Ws-Optional
     CALL "KITCOST" USING Ws-Cost-Ca
     MOVE KO-RETURN-CODE TO Ws-Call-Rc
     MOVE KO-REASON-CODE TO Ws-Call-Reason
     MOVE KO-MESSAGE TO Ws-Call-Msg
     MOVE "Kit cost" TO D-Step
     MOVE "COST" TO D-Func
     MOVE Ws-Kit-Ref TO D-Ref
     MOVE KK-COMPLETED-QTY TO D-Qty
     MOVE KK-COMPLETED-GBP TO D-Value
     PERFORM 8000-RECORD
     MOVE "N" TO Ws-Optional
     IF Ws-Call-Rc < 8
         DISPLAY "STERP-I-1879 kit " FUNCTION TRIM(Ws-Kit-Ref)
                 " issued " KK-ISSUED-GBP " completed "
                 KK-COMPLETED-GBP " residual " KK-RESIDUAL-GBP
             UPON SYSOUT
     END-IF.
 1600-EXIT.
     EXIT.

*>================================================================
*> KIT-021 / D-12.  One assembled unit is taken apart again: the
*> inventory manager raises it, quality approves it, and the post
*> gives every recovered component its own derived correlation id
*> so that the parent issue does not swallow the receipts.
*>================================================================
 1700-DISASSEMBLE SECTION.
 1700-START.
     MOVE "Y" TO Ws-Optional
     IF Ws-Pick-Cnt = 0 OR Ws-Whse = SPACES OR Ws-Loc = SPACES
         GO TO 1700-EXIT
     END-IF
     PERFORM 8260-DIS-HEADER
     MOVE Ws-Parent TO DS-PARENT-ITEM
     MOVE Ws-Parent-Rev TO DS-REVISION
     MOVE Ws-Site TO DS-SITE
     MOVE Ws-Whse TO DS-WAREHOUSE
     MOVE Ws-Loc TO DS-LOCATION
     MOVE 1 TO DS-CONSUMED-QTY
     MOVE Ws-Asat TO DS-DISASSY-DATE
     MOVE Ws-Asat TO DS-POSTING-DATE
     COMPUTE DS-CARRYING-GBP ROUNDED = Ws-Unit-Cost
     MOVE 0 TO DS-APPROVED-GBP
     MOVE "Demonstration strip down: one assembly recovered for parts."
          TO DS-REASON-TEXT
     MOVE "CREATE" TO DA-FUNCTION
     CALL "DISASSY" USING Ws-Dis-Ca
     PERFORM 8170-FROM-DIS
     MOVE "Disassembly raised" TO D-Step
     MOVE "CREATE" TO D-Func
     MOVE DS-DISASSY-REF TO D-Ref
     MOVE DS-CONSUMED-QTY TO D-Qty
     MOVE DS-AVAILABLE-GBP TO D-Value
     PERFORM 8000-RECORD
     IF Ws-Call-Rc > 6
         GO TO 1700-EXIT
     END-IF
     MOVE DS-DISASSY-REF TO Ws-Dis-Ref
     PERFORM 1720-ADD-A-RECOVERED-LINE
     PERFORM 1740-VALUE-APPROVE-POST.
 1700-EXIT.
     MOVE "N" TO Ws-Optional.

 1720-ADD-A-RECOVERED-LINE SECTION.
 1720-START.
     PERFORM 8260-DIS-HEADER
     MOVE Ws-Dis-Ref TO DS-DISASSY-REF
     MOVE Ws-Parent TO DS-PARENT-ITEM
     MOVE Ws-Site TO DS-SITE
     MOVE 1 TO DS-A-LINE-NUM
     MOVE Ws-P-Item(1) TO DS-A-ITEM
     MOVE 1 TO DS-A-PLANNED
     MOVE 1 TO DS-A-RECOVERED
     MOVE 0 TO DS-A-DAMAGED
     MOVE Ws-P-Uom(1) TO DS-A-UOM
     MOVE "ACCEPTED" TO DS-A-QUALITY
     MOVE Ws-P-Loc(1) TO DS-A-LOCATION
     MOVE "ADDLINE" TO DA-FUNCTION
     CALL "DISASSY" USING Ws-Dis-Ca
     PERFORM 8170-FROM-DIS
     MOVE "Component recovered" TO D-Step
     MOVE "ADDLINE" TO D-Func
     MOVE Ws-P-Item(1) TO D-Ref
     MOVE 1 TO D-Qty
     MOVE 0 TO D-Value
     PERFORM 8000-RECORD.
 1720-EXIT.
     EXIT.

 1740-VALUE-APPROVE-POST SECTION.
 1740-START.
     PERFORM 8260-DIS-HEADER
     MOVE Ws-Dis-Ref TO DS-DISASSY-REF
     MOVE "VALUE" TO DA-FUNCTION
     CALL "DISASSY" USING Ws-Dis-Ca
     PERFORM 8170-FROM-DIS
     MOVE "Recovery valued" TO D-Step
     MOVE "VALUE" TO D-Func
     MOVE Ws-Dis-Ref TO D-Ref
     MOVE DS-LINE-CNT TO D-Qty
     MOVE DS-RECOVERED-GBP TO D-Value
     PERFORM 8000-RECORD
     IF Ws-Call-Rc > 6
         GO TO 1740-EXIT
     END-IF
     PERFORM 8260-DIS-HEADER
     MOVE Ws-Dis-Ref TO DS-DISASSY-REF
     MOVE Ws-Quality TO DS-APPROVED-BY
     MOVE "APPROVE" TO DA-FUNCTION
     CALL "DISASSY" USING Ws-Dis-Ca
     PERFORM 8170-FROM-DIS
     MOVE "Disassembly approved" TO D-Step
     MOVE "APPROVE" TO D-Func
     MOVE Ws-Dis-Ref TO D-Ref
     MOVE 0 TO D-Qty
     MOVE DS-LOSS-GBP TO D-Value
     PERFORM 8000-RECORD
     IF Ws-Call-Rc > 6
         GO TO 1740-EXIT
     END-IF
     PERFORM 8260-DIS-HEADER
     MOVE Ws-Dis-Ref TO DS-DISASSY-REF
     MOVE Ws-Whse TO DS-WAREHOUSE
     MOVE Ws-Loc TO DS-LOCATION
     MOVE "POST" TO DA-FUNCTION
     CALL "DISASSY" USING Ws-Dis-Ca
     PERFORM 8170-FROM-DIS
     MOVE "Disassembly posted" TO D-Step
     MOVE "POST" TO D-Func
     MOVE Ws-Dis-Ref TO D-Ref
     MOVE DS-POSTED-CNT TO D-Qty
     MOVE DS-RECOVERED-GBP TO D-Value
     PERFORM 8000-RECORD.
 1740-EXIT.
     EXIT.

*>================================================================
*> HOUSEKEEPING
*>================================================================
*>   One line of the listing per service call.  A refusal of a
*>   step the demonstration can live without (an authority hold on
*>   the release, a pick the stock will not support, the
*>   disassembly) is a warning: it is shown, it is counted, and it
*>   does not fail the run.  A refusal in the main chain is.
 8000-RECORD SECTION.
 8000-START.
     ADD 1 TO T-Steps
     ADD 1 TO Ws-Seq
     MOVE Ws-Seq TO D-Seq
     MOVE Ws-Actor TO D-Actor
     EVALUATE TRUE
         WHEN Ws-Call-Rc = 0
             MOVE "accepted" TO D-Outcome
             ADD 1 TO T-Ok
         WHEN Ws-Call-Rc = 6
             MOVE SPACES TO D-Outcome
             STRING "hold " Ws-Call-Reason
                 DELIMITED BY SIZE INTO D-Outcome
             ADD 1 TO T-Warn
         WHEN Ws-Call-Rc < 8
             MOVE SPACES TO D-Outcome
             STRING "warning " Ws-Call-Reason
                 DELIMITED BY SIZE INTO D-Outcome
             ADD 1 TO T-Warn
         WHEN Ws-Optional = "Y"
             MOVE SPACES TO D-Outcome
             STRING "not done " Ws-Call-Reason
                 DELIMITED BY SIZE INTO D-Outcome
             ADD 1 TO T-Warn
             DISPLAY "STERP-W-1877 " FUNCTION TRIM(D-Step) " "
                     Ws-Call-Reason " "
                     FUNCTION TRIM(Ws-Call-Msg) UPON SYSOUT
         WHEN OTHER
             MOVE SPACES TO D-Outcome
             STRING "REFUSED " Ws-Call-Reason
                 DELIMITED BY SIZE INTO D-Outcome
             ADD 1 TO T-Fail
             DISPLAY "STERP-E-1877 " FUNCTION TRIM(D-Step) " "
                     Ws-Call-Reason " "
                     FUNCTION TRIM(Ws-Call-Msg) UPON SYSOUT
     END-EVALUATE
     GENERATE DE-LINE.
 8000-EXIT.
     EXIT.

*>   COM-015 / CR2741.  A fresh correlation id for every service
*>   call.  The stem is short on purpose: KITISSUE BACKFLSH and
*>   DISASSY POST cut the id to thirty characters and append a
*>   suffix of their own for each line they post, and an id that
*>   filled the field would lose that suffix and put a whole
*>   backflush back onto one idempotency key.
 8010-NEXT-CORR SECTION.
 8010-START.
     ADD 1 TO Ws-Corr-Seq
     MOVE SPACES TO Ws-Step-Corr
     STRING FUNCTION TRIM(Ws-Corr-Stem) "-" Ws-Corr-Seq
         DELIMITED BY SIZE INTO Ws-Step-Corr.
 8010-EXIT.
     EXIT.

 8100-FROM-TRIAL SECTION.
 8100-START.
     MOVE TR-RETURN-CODE TO Ws-Call-Rc
     MOVE TR-REASON-CODE TO Ws-Call-Reason
     MOVE TR-MESSAGE TO Ws-Call-Msg.
 8100-EXIT.
     EXIT.

 8110-FROM-CREAT SECTION.
 8110-START.
     MOVE CR-RETURN-CODE TO Ws-Call-Rc
     MOVE CR-REASON-CODE TO Ws-Call-Reason
     MOVE CR-MESSAGE TO Ws-Call-Msg.
 8110-EXIT.
     EXIT.

 8120-FROM-RESV SECTION.
 8120-START.
     MOVE RV-RETURN-CODE TO Ws-Call-Rc
     MOVE RV-REASON-CODE TO Ws-Call-Reason
     MOVE RV-MESSAGE TO Ws-Call-Msg.
 8120-EXIT.
     EXIT.

 8130-FROM-ISSUE SECTION.
 8130-START.
     MOVE IS-RETURN-CODE TO Ws-Call-Rc
     MOVE IS-REASON-CODE TO Ws-Call-Reason
     MOVE IS-MESSAGE TO Ws-Call-Msg.
 8130-EXIT.
     EXIT.

 8140-FROM-COMPL SECTION.
 8140-START.
     MOVE CP-RETURN-CODE TO Ws-Call-Rc
     MOVE CP-REASON-CODE TO Ws-Call-Reason
     MOVE CP-MESSAGE TO Ws-Call-Msg.
 8140-EXIT.
     EXIT.

 8170-FROM-DIS SECTION.
 8170-START.
     MOVE DA-RETURN-CODE TO Ws-Call-Rc
     MOVE DA-REASON-CODE TO Ws-Call-Reason
     MOVE DA-MESSAGE TO Ws-Call-Msg.
 8170-EXIT.
     EXIT.

 8200-CREAT-HEADER SECTION.
 8200-START.
     INITIALIZE Ws-Creat-Ca
     PERFORM 8010-NEXT-CORR
     MOVE "KITCREAT" TO CR-PROGRAM
     MOVE Ws-Co TO CR-COMPANY
     MOVE Ws-Site TO CR-SITE
     MOVE Ws-Actor TO CR-USER
     MOVE Ws-Asat TO CR-BUSINESS-DATE
     MOVE Ws-Step-Corr TO CR-CORRELATION-ID.
 8200-EXIT.
     EXIT.

 8210-RESV-HEADER SECTION.
 8210-START.
     INITIALIZE Ws-Resv-Ca
     MOVE Ws-Stores TO Ws-Actor
     PERFORM 8010-NEXT-CORR
     MOVE "KITRESV" TO RV-PROGRAM
     MOVE Ws-Co TO RV-COMPANY
     MOVE Ws-Site TO RV-SITE
     MOVE Ws-Actor TO RV-USER
     MOVE Ws-Asat TO RV-BUSINESS-DATE
     MOVE Ws-Step-Corr TO RV-CORRELATION-ID.
 8210-EXIT.
     EXIT.

 8220-ISSUE-HEADER SECTION.
 8220-START.
     INITIALIZE Ws-Issue-Ca
     MOVE Ws-Stores TO Ws-Actor
     PERFORM 8010-NEXT-CORR
     MOVE "KITISSUE" TO IS-PROGRAM
     MOVE Ws-Co TO IS-COMPANY
     MOVE Ws-Site TO IS-SITE
     MOVE Ws-Actor TO IS-USER
     MOVE Ws-Asat TO IS-BUSINESS-DATE
     MOVE Ws-Step-Corr TO IS-CORRELATION-ID.
 8220-EXIT.
     EXIT.

*>   The storeman books the output; the inventory manager is
*>   named as the person who approved it.  Two names, two duties.
 8230-COMPL-HEADER SECTION.
 8230-START.
     INITIALIZE Ws-Compl-Ca
     MOVE Ws-Stores TO Ws-Actor
     PERFORM 8010-NEXT-CORR
     MOVE "KITCOMPL" TO CP-PROGRAM
     MOVE Ws-Co TO CP-COMPANY
     MOVE Ws-Site TO CP-SITE
     MOVE Ws-Actor TO CP-USER
     MOVE Ws-Asat TO CP-BUSINESS-DATE
     MOVE Ws-Step-Corr TO CP-CORRELATION-ID
     MOVE Ws-Kit-Ref TO KM-KIT-REF
     MOVE Ws-Parent TO KM-PARENT-ITEM
     MOVE Ws-Parent-Rev TO KM-REVISION
     MOVE Ws-Site TO KM-SITE
     MOVE Ws-Whse TO KM-WAREHOUSE
     MOVE Ws-Loc TO KM-LOCATION
     MOVE Ws-Inv-Mgr TO KM-APPROVED-BY
*>   The material has already been backflushed by KITISSUE, so the
*>   completion must not do it again: a second pass would issue
*>   the components twice and be refused as an overissue.
     MOVE "N" TO KM-BACKFLUSH
     MOVE Ws-Asat TO KM-COMPLETION-DATE
     MOVE Ws-Asat TO KM-POSTING-DATE.
 8230-EXIT.
     EXIT.

 8260-DIS-HEADER SECTION.
 8260-START.
     INITIALIZE Ws-Dis-Ca
     MOVE Ws-Inv-Mgr TO Ws-Actor
     PERFORM 8010-NEXT-CORR
     MOVE "DISASSY" TO DA-PROGRAM
     MOVE Ws-Co TO DA-COMPANY
     MOVE Ws-Site TO DA-SITE
     MOVE Ws-Actor TO DA-USER
     MOVE Ws-Asat TO DA-BUSINESS-DATE
     MOVE Ws-Step-Corr TO DA-CORRELATION-ID.
 8260-EXIT.
     EXIT.
