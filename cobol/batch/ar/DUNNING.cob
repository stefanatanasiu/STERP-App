      *>================================================================
      *> PROGRAM-ID : DUNNING
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : COLLECTION (DUNNING) RUN.  WALKS THE OVERDUE
      *>              SALES LEDGER AND WORKS OUT THE NEXT STEP OF THE
      *>              BASELINE COLLECTION SEQUENCE FOR EACH ACCOUNT:
      *>                 7 DAYS  REMINDER 1   COURTEOUS
      *>                21 DAYS  REMINDER 2   FIRM, INTEREST WARNING
      *>                30 DAYS  ACCOUNT STOP NOTIFY THE OWNER
      *>                45 DAYS  FINAL DEMAND / LETTER OF CLAIM
      *>                75 DAYS  EXTERNAL RECOVERY HAND OFF
      *>              DISPUTED AMOUNTS, OPEN PROMISES TO PAY, CONTACT
      *>              RESTRICTIONS, INSOLVENCY AND AMOUNTS ALREADY
      *>              SETTLED SUPPRESS A STEP AND THE REASON IS
      *>              PRINTED AND RECORDED.  MODE=COMMIT MAINTAINS THE
      *>              COLLECTION WORKLIST (ar.CollectionCase AND
      *>              ar.CollectionAction) AND RAISES AN EXCEPTION FOR
      *>              EVERY ESCALATION; MODE=PREVIEW ONLY PRINTS.
      *> RUN AS     : DUNNING CO=STC ASAT=20261004 MODE=PREVIEW
      *>              USER=ARCLERK1
      *> OUTPUT     : run\spool\DUNNING-<CO>-<stamp>.lst   worklist
      *>              run\spool\DUNNLTR-<CO>-<stamp>.lst   letters
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, EXCPRAIS, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-04-16 RJH  ORIGINAL CHASER RUN (MVS PORT)
      *> 1997-02-24 RJH  CR0688 PROMISES TO PAY SUPPRESS A CHASE
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS
      *> 2013-10-07 KMP  CR2510 PRE-ACTION PROTOCOL FOR DEBT CLAIMS
      *>                 WORDING FOR INDIVIDUAL DEBTORS
      *> 2026-10-05 STERP AR-015 / AR-016 WORKLIST AND EXCEPTIONS
      *>================================================================
      *> REQ: AR-016, AR-015
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DUNNING.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 16/04/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO DYNAMIC WS-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           SELECT LTR-FILE ASSIGN TO DYNAMIC WS-LETTER-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LTR-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-DUNNING.
       FD  LTR-FILE.
       01  LTR-REC                     PIC X(132).
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ARWORK.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXCPRAIS==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-LETTER-NAME              PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-LTR-STATUS               PIC X(2) VALUE "00".
       01  BR-ROW.
           05  BR-COL OCCURS 30 TIMES  PIC X(120).
       01  CR-ROW.
           05  CR-COL OCCURS 14 TIMES  PIC X(120).
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
           05  WS-ASAT                 PIC 9(8) VALUE 0.
           05  WS-COMMIT               PIC X VALUE "N".
               88  WS-DO-COMMIT                  VALUE "Y".
       01  WS-RUN.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-JOBID                PIC S9(9) COMP-5 VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-CASE-ID              PIC S9(9) VALUE 0.
           05  WS-LETTERS              PIC S9(7) VALUE 0.
           05  WS-ESCALATED            PIC S9(7) VALUE 0.
           05  WS-DUE-N                PIC 9(8) VALUE 0.
           05  WS-NEXT-N               PIC 9(8) VALUE 0.
           05  WS-PROMISE              PIC X VALUE "N".
               88  WS-PROMISE-OPEN               VALUE "Y".
       01  WS-UK.
           05  WS-UK-DD                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-UK-MM                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-UK-YYYY              PIC 9(4) VALUE 0.
      *>   the 1991 run allowed 300 accounts per pass; the limit is
      *>   still here because the letter stationery was boxed in 300s
       01  WS-STEPS.
           05  WS-STEP-NO              PIC 9 VALUE 0.
               88  WS-STEP-NONE                  VALUE 0.
               88  WS-STEP-REM1                  VALUE 1.
               88  WS-STEP-REM2                  VALUE 2.
               88  WS-STEP-STOP                  VALUE 3.
               88  WS-STEP-DEMAND                VALUE 4.
               88  WS-STEP-RECOVERY              VALUE 5.
           05  WS-STEP-LAST            PIC 9 VALUE 0.
           05  WS-ACTION-TYPE          PIC X(14) VALUE SPACES.
           05  WS-MAX-ACCOUNTS         PIC S9(4) VALUE 300.
       01  WS-DET.
           05  D-CUST                  PIC X(20) VALUE SPACES.
           05  D-NAME                  PIC X(34) VALUE SPACES.
           05  D-LEGAL                 PIC X(12) VALUE SPACES.
           05  D-OLDEST                PIC X(10) VALUE SPACES.
           05  D-DAYS                  PIC S9(5) VALUE 0.
           05  D-ITEMS                 PIC S9(5) VALUE 0.
           05  D-OWED                  PIC S9(13)V99 VALUE 0.
           05  D-DISPUTED              PIC S9(13)V99 VALUE 0.
           05  D-CHASE                 PIC S9(13)V99 VALUE 0.
           05  D-STEP                  PIC X(14) VALUE SPACES.
           05  D-NEXT                  PIC X(10) VALUE SPACES.
           05  D-WHY                   PIC X(42) VALUE SPACES.
           05  D-OWNER                 PIC X(20) VALUE SPACES.
       01  WS-TOT.
           05  T-ACCOUNTS              PIC S9(7) VALUE 0.
           05  T-OWED                  PIC S9(13)V99 VALUE 0.
           05  T-CHASE                 PIC S9(13)V99 VALUE 0.
           05  T-DISPUTED              PIC S9(13)V99 VALUE 0.
           05  T-SUPPRESSED            PIC S9(7) VALUE 0.
       01  WS-LTR.
           05  L-LINE                  PIC X(132) VALUE SPACES.
           05  L-MONEY                 PIC Z,ZZZ,ZZZ,ZZ9.99-.
       REPORT SECTION.
       RD  R-DUNNING
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 8 LAST DETAIL 54
           FOOTING 58.
       COPY STRPTHDR.
      *>   the standard page heading owns lines 1 to 6, so the column
      *>   banner is a FINAL control heading - it is printed once at
      *>   the head of the run (1991 behaviour, never changed)
       01  CH-COLS TYPE IS CONTROL HEADING FINAL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(8) VALUE "Customer".
               10  COLUMN 22   PIC X(4) VALUE "Name".
               10  COLUMN 57   PIC X(6) VALUE "Oldest".
               10  COLUMN 69   PIC X(4) VALUE "Days".
               10  COLUMN 75   PIC X(5) VALUE "Items".
               10  COLUMN 84   PIC X(9) VALUE "Owed GBP".
               10  COLUMN 100  PIC X(9) VALUE "Chaseable".
               10  COLUMN 115  PIC X(10) VALUE "Step".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-ACCT TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(20) SOURCE D-CUST.
               10  COLUMN 22   PIC X(34) SOURCE D-NAME.
               10  COLUMN 57   PIC X(10) SOURCE D-OLDEST.
               10  COLUMN 68   PIC ZZZZ9 SOURCE D-DAYS.
               10  COLUMN 75   PIC ZZZZ9 SOURCE D-ITEMS.
               10  COLUMN 81   PIC Z,ZZZ,ZZ9.99- SOURCE D-OWED.
               10  COLUMN 96   PIC Z,ZZZ,ZZ9.99- SOURCE D-CHASE.
               10  COLUMN 112  PIC X(14) SOURCE D-STEP.
               10  COLUMN 127  PIC X(6) SOURCE D-LEGAL.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(9) VALUE "Disputed".
               10  COLUMN 15   PIC Z,ZZZ,ZZ9.99- SOURCE D-DISPUTED.
               10  COLUMN 32   PIC X(10) VALUE "Next due".
               10  COLUMN 43   PIC X(10) SOURCE D-NEXT.
               10  COLUMN 55   PIC X(6) VALUE "Owner".
               10  COLUMN 62   PIC X(20) SOURCE D-OWNER.
               10  COLUMN 85   PIC X(42) SOURCE D-WHY.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(24) VALUE "Accounts considered".
               10  COLUMN 26   PIC ZZZ,ZZ9 SOURCE T-ACCOUNTS.
               10  COLUMN 40   PIC X(20) VALUE "Steps suppressed".
               10  COLUMN 61   PIC ZZZ,ZZ9 SOURCE T-SUPPRESSED.
               10  COLUMN 75   PIC X(16) VALUE "Escalations".
               10  COLUMN 92   PIC ZZZ,ZZ9 SOURCE WS-ESCALATED.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(24) VALUE "Overdue debt GBP".
               10  COLUMN 24   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-OWED.
               10  COLUMN 45   PIC X(16) VALUE "Chaseable".
               10  COLUMN 62   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-CHASE.
               10  COLUMN 85   PIC X(10) VALUE "Disputed".
               10  COLUMN 96   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-DISPUTED.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(16) VALUE "Letters written".
               10  COLUMN 18   PIC ZZZ,ZZ9 SOURCE WS-LETTERS.
               10  COLUMN 32   PIC X(10) VALUE "Run mode".
               10  COLUMN 43   PIC X(10) SOURCE WS-MODE.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(50) VALUE
                   "A dispute classifies the debt; it does not erase".
               10  COLUMN 52   PIC X(48) VALUE
                   "it.  Collection continues on the undisputed part.".
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
           PERFORM 1000-COLLECTION-RUN
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
           MOVE FUNCTION UPPER-CASE(CM-MODE) TO WS-MODE
           IF CM-ASAT NOT = SPACES
               COMPUTE WS-ASAT = FUNCTION NUMVAL(CM-ASAT)
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "ARCLERK1" TO WS-USER
           END-IF
           IF WS-ASAT = 0
               MOVE FUNCTION CURRENT-DATE TO AR-NOW
               COMPUTE WS-ASAT = FUNCTION NUMVAL(AR-NOW(1:8))
           END-IF
           IF WS-MODE = SPACES
               MOVE "PREVIEW" TO WS-MODE
           END-IF
           IF WS-MODE NOT = "PREVIEW" AND WS-MODE NOT = "COMMIT"
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0103 MODE must be PREVIEW or COMMIT"
               GO TO 0100-EXIT
           END-IF
           IF WS-MODE = "COMMIT"
               MOVE "Y" TO WS-COMMIT
           END-IF
           DISPLAY "STERP-I-0100 DUNNING collection run "
                   FUNCTION TRIM(WS-MODE).
       0100-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE WS-ASAT TO CA-BUSINESS-DATE OF WS-JOBLOG
           MOVE "DUNNING" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE JL-JOB-RUN-ID TO WS-JOBID.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "DUNNING" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0301 cannot build the spool path"
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           MOVE RP-TIMESTAMP TO AR-SCRATCH(1:14)
           MOVE "DUNNLTR" TO RP-REPORT-ID
           MOVE AR-SCRATCH(1:14) TO RP-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           MOVE RP-FULL-PATH TO WS-LETTER-NAME
           PERFORM 0350-FILL-HEADINGS
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0302 cannot open " WS-SPOOL-NAME
               GO TO 0300-EXIT
           END-IF
           OPEN OUTPUT LTR-FILE
           IF WS-LTR-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0303 cannot open " WS-LETTER-NAME
               GO TO 0300-EXIT
           END-IF
           INITIATE R-DUNNING.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "DUNNING" TO RPT-REPORT-ID
           MOVE "Collection worklist and reminder run" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO AR-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(AR-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(AR-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(AR-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(AR-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(AR-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(AR-NOW(13:2))
           MOVE WS-ASAT TO AR-NUM-DATE
           PERFORM 8100-FORMAT-DATE
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "As at " WS-UK DELIMITED BY SIZE
               INTO RPT-AS-AT-TEXT
           MOVE SPACES TO RPT-SUBTITLE
           STRING "Mode " FUNCTION TRIM(WS-MODE)
                  " - baseline sequence 7/21/30/45/75 days"
               DELIMITED BY SIZE INTO RPT-SUBTITLE
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
           PERFORM 8000-SET-WIDTHS
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               MOVE BR-COL(1) TO RPT-COMPANY-NAME
               MOVE BR-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-016  one worklist row per overdue account.  The
      *> aggregation is done in SQL; the business decision is not.
      *>----------------------------------------------------------------
       1000-COLLECTION-RUN SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT o.CustomerCode, MAX(c.LegalName), "
                  "CONVERT(char(8),MIN(o.DueDate),112), "
                  "CONVERT(varchar(24),SUM(o.ResidualAmountGBP)), "
                  "CONVERT(varchar(24),SUM(CASE WHEN o.IsDisputed = "
                  "1 THEN o.DisputedAmountGBP ELSE 0 END)), "
                  "CONVERT(varchar(12),COUNT(*)), "
                  "MAX(ISNULL(c.ContactRestriction,'')), "
                  "MAX(ISNULL(c.InsolvencyType,'')), "
                  "MAX(c.LegalForm), MAX(c.Status), "
                  "CONVERT(varchar(12),MAX(ISNULL("
                  "o.LastReminderStep,0))), MAX(ISNULL("
                  "cr.ControllerUser,'')) FROM ar.OpenItem o JOIN "
                  "ar.Customer c ON c.CompanyCode = o.CompanyCode "
                  "AND c.CustomerCode = o.CustomerCode LEFT JOIN "
                  "ar.CustomerCredit cr ON cr.CompanyCode = "
                  "o.CompanyCode AND cr.CustomerCode = "
                  "o.CustomerCode WHERE o.CompanyCode = ? AND "
                  "o.DueDate < ? AND o.ResidualAmountGBP > 0 AND "
                  "o.SettlementStatus IN ('OPEN','PARTIAL') AND "
                  "o.WrittenOff = 0 GROUP BY o.CustomerCode ORDER BY "
                  "o.CustomerCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-ASAT TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 12 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA CR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1001 " SQLIO-ERRMSG(1:100)
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO AR-DONE
           PERFORM UNTIL AR-FINISHED
               MOVE 1 TO SQLIO-CURSOR
               MOVE 12 TO SQLIO-COL-CNT
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA CR-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO AR-DONE
               ELSE
                   PERFORM 1100-ONE-ACCOUNT
                   IF T-ACCOUNTS >= WS-MAX-ACCOUNTS
                       MOVE "Y" TO AR-DONE
                       MOVE 4 TO WS-RC
                       DISPLAY "STERP-W-1002 account limit reached"
                   END-IF
               END-IF
           END-PERFORM
           MOVE 1 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA CR-ROW.
       1000-EXIT.
           EXIT.

       1100-ONE-ACCOUNT SECTION.
       1100-START.
           ADD 1 TO T-ACCOUNTS WS-ROWS-OUT
           MOVE CR-COL(1) TO D-CUST
           MOVE CR-COL(2) TO D-NAME
           MOVE CR-COL(9) TO D-LEGAL
           MOVE CR-COL(12) TO D-OWNER
           IF D-OWNER = SPACES
               MOVE WS-USER TO D-OWNER
           END-IF
           COMPUTE D-OWED = FUNCTION NUMVAL(CR-COL(4))
           COMPUTE D-DISPUTED = FUNCTION NUMVAL(CR-COL(5))
           COMPUTE D-ITEMS = FUNCTION NUMVAL(CR-COL(6))
           COMPUTE WS-STEP-LAST = FUNCTION NUMVAL(CR-COL(11))
           MOVE CR-COL(3) TO AR-COMPACT-DATE
           COMPUTE WS-DUE-N = FUNCTION NUMVAL(AR-COMPACT-DATE)
           MOVE WS-DUE-N TO AR-NUM-DATE
           PERFORM 8100-FORMAT-DATE
           MOVE WS-UK TO D-OLDEST
           COMPUTE D-DAYS = FUNCTION INTEGER-OF-DATE(WS-ASAT)
                          - FUNCTION INTEGER-OF-DATE(WS-DUE-N)
      *>   AR-015 the dispute stays in the debt but is not chased
           COMPUTE D-CHASE = D-OWED - D-DISPUTED
           IF D-CHASE < 0
               MOVE 0 TO D-CHASE
           END-IF
           ADD D-OWED TO T-OWED
           ADD D-CHASE TO T-CHASE
           ADD D-DISPUTED TO T-DISPUTED
           MOVE SPACES TO D-WHY D-STEP D-NEXT
           PERFORM 1200-DECIDE-STEP
           PERFORM 1300-CHECK-SUPPRESSION
           IF WS-STEP-NONE
               GENERATE DE-ACCT
               GO TO 1100-EXIT
           END-IF
           PERFORM 1400-NEXT-ACTION-DATE
           GENERATE DE-ACCT
           PERFORM 1500-WRITE-LETTER
           IF WS-DO-COMMIT
               PERFORM 1600-UPDATE-WORKLIST
           END-IF
           IF WS-STEP-DEMAND OR WS-STEP-RECOVERY
               PERFORM 1700-RAISE-ESCALATION
           END-IF.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-016  the baseline escalation schedule (D-06).  A step
      *> is never repeated - the account must reach the next stage.
      *>----------------------------------------------------------------
       1200-DECIDE-STEP SECTION.
       1200-START.
           MOVE 0 TO WS-STEP-NO
           EVALUATE TRUE
               WHEN D-DAYS >= 75   MOVE 5 TO WS-STEP-NO
               WHEN D-DAYS >= 45   MOVE 4 TO WS-STEP-NO
               WHEN D-DAYS >= 30   MOVE 3 TO WS-STEP-NO
               WHEN D-DAYS >= 21   MOVE 2 TO WS-STEP-NO
               WHEN D-DAYS >= 7    MOVE 1 TO WS-STEP-NO
               WHEN OTHER          MOVE 0 TO WS-STEP-NO
           END-EVALUATE
           EVALUATE WS-STEP-NO
               WHEN 1  MOVE "REMINDER1" TO WS-ACTION-TYPE D-STEP
               WHEN 2  MOVE "REMINDER2" TO WS-ACTION-TYPE D-STEP
               WHEN 3  MOVE "STOP" TO WS-ACTION-TYPE
                       MOVE "ACCOUNT STOP" TO D-STEP
               WHEN 4  MOVE "DEMAND" TO WS-ACTION-TYPE
                       MOVE "FINAL DEMAND" TO D-STEP
               WHEN 5  MOVE "HANDOFF" TO WS-ACTION-TYPE
                       MOVE "RECOVERY" TO D-STEP
               WHEN OTHER
                       MOVE "NOTE" TO WS-ACTION-TYPE
                       MOVE "none due" TO D-STEP
           END-EVALUATE
           IF WS-STEP-NO > 0 AND WS-STEP-NO <= WS-STEP-LAST
               MOVE 0 TO WS-STEP-NO
               MOVE "already issued" TO D-STEP
               MOVE "Step already issued at this stage" TO D-WHY
               ADD 1 TO T-SUPPRESSED
           END-IF.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-016  approved communication rules and restrictions
      *>----------------------------------------------------------------
       1300-CHECK-SUPPRESSION SECTION.
       1300-START.
           IF WS-STEP-NONE
               GO TO 1300-EXIT
           END-IF
           IF D-CHASE < 0.005
               MOVE 0 TO WS-STEP-NO
               MOVE "Whole balance disputed or settled" TO D-WHY
               ADD 1 TO T-SUPPRESSED
               GO TO 1300-EXIT
           END-IF
           IF CR-COL(7) NOT = SPACES
               MOVE 0 TO WS-STEP-NO
               MOVE SPACES TO D-WHY
               STRING "Contact restriction: "
                      FUNCTION TRIM(CR-COL(7))
                   DELIMITED BY SIZE INTO D-WHY
               ADD 1 TO T-SUPPRESSED
               GO TO 1300-EXIT
           END-IF
           IF CR-COL(8) NOT = SPACES
               MOVE 0 TO WS-STEP-NO
               MOVE SPACES TO D-WHY
               STRING "Insolvency " FUNCTION TRIM(CR-COL(8))
                      " - office holder route"
                   DELIMITED BY SIZE INTO D-WHY
               ADD 1 TO T-SUPPRESSED
               GO TO 1300-EXIT
           END-IF
           PERFORM 1350-CHECK-PROMISE
           IF WS-PROMISE-OPEN
               MOVE 0 TO WS-STEP-NO
               ADD 1 TO T-SUPPRESSED
           END-IF.
       1300-EXIT.
           EXIT.

       1350-CHECK-PROMISE SECTION.
       1350-START.
           MOVE "N" TO WS-PROMISE
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 CONVERT(char(8),a.PromiseDate,112), "
                  "CONVERT(varchar(24),ISNULL(a.PromiseAmountGBP,0)) "
                  "FROM ar.CollectionAction a JOIN ar.CollectionCase "
                  "k ON k.CaseId = a.CaseId WHERE k.CompanyCode = ? "
                  "AND k.CustomerCode = ? AND a.ActionType = "
                  "'PROMISE' AND a.PromiseStatus = 'OPEN' AND "
                  "a.PromiseDate >= ? ORDER BY a.PromiseDate"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE D-CUST TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE WS-ASAT TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               MOVE BR-COL(1) TO AR-COMPACT-DATE
               COMPUTE WS-NEXT-N = FUNCTION NUMVAL(AR-COMPACT-DATE)
               MOVE WS-NEXT-N TO AR-NUM-DATE
               PERFORM 8100-FORMAT-DATE
               MOVE SPACES TO D-WHY
               STRING "Promise to pay open until " WS-UK
                   DELIMITED BY SIZE INTO D-WHY
               MOVE "Y" TO WS-PROMISE
           END-IF.
       1350-EXIT.
           EXIT.

       1400-NEXT-ACTION-DATE SECTION.
       1400-START.
      *>   the follow up is a fortnight later unless the account has
      *>   reached recovery, when the protocol response period applies
           IF WS-STEP-DEMAND
               COMPUTE AR-N = FUNCTION INTEGER-OF-DATE(WS-ASAT) + 30
           ELSE
               COMPUTE AR-N = FUNCTION INTEGER-OF-DATE(WS-ASAT) + 14
           END-IF
           COMPUTE WS-NEXT-N = FUNCTION DATE-OF-INTEGER(AR-N)
           MOVE WS-NEXT-N TO AR-NUM-DATE
           PERFORM 8100-FORMAT-DATE
           MOVE WS-UK TO D-NEXT.
       1400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-016  the reminder letter shows the correct residual
      *>----------------------------------------------------------------
       1500-WRITE-LETTER SECTION.
       1500-START.
           IF WS-STEP-STOP
      *>       an account stop notifies the owner, it is not a letter
               GO TO 1500-EXIT
           END-IF
           ADD 1 TO WS-LETTERS
           MOVE WS-ASAT TO AR-NUM-DATE
           PERFORM 8100-FORMAT-DATE
           MOVE SPACES TO LTR-REC
           MOVE ALL "=" TO LTR-REC(1:80)
           WRITE LTR-REC
           MOVE SPACES TO LTR-REC
           STRING FUNCTION TRIM(RPT-COMPANY-NAME)
                  " - credit control"
               DELIMITED BY SIZE INTO LTR-REC
           WRITE LTR-REC
           MOVE SPACES TO LTR-REC
           STRING "Our reference " FUNCTION TRIM(D-CUST)
                  "    Date " WS-UK
               DELIMITED BY SIZE INTO LTR-REC
           WRITE LTR-REC
           MOVE SPACES TO LTR-REC
           MOVE D-NAME TO LTR-REC(1:34)
           WRITE LTR-REC
           MOVE SPACES TO LTR-REC
           WRITE LTR-REC
           MOVE D-CHASE TO L-MONEY
           MOVE SPACES TO LTR-REC
           EVALUATE TRUE
               WHEN WS-STEP-REM1
                   STRING "Our records show GBP "
                          FUNCTION TRIM(L-MONEY)
                          " overdue.  Please arrange payment."
                       DELIMITED BY SIZE INTO LTR-REC
               WHEN WS-STEP-REM2
                   STRING "GBP " FUNCTION TRIM(L-MONEY)
                          " remains overdue.  We may "
                          "claim statutory interest and compensation."
                       DELIMITED BY SIZE INTO LTR-REC
               WHEN WS-STEP-DEMAND
                   STRING "Final demand: GBP "
                          FUNCTION TRIM(L-MONEY)
                          " is now due in full."
                       DELIMITED BY SIZE INTO LTR-REC
               WHEN OTHER
                   STRING "Your account of GBP "
                          FUNCTION TRIM(L-MONEY)
                          " is being passed for external recovery."
                       DELIMITED BY SIZE INTO LTR-REC
           END-EVALUATE
           WRITE LTR-REC
           PERFORM 1550-LETTER-TAIL.
       1500-EXIT.
           EXIT.

       1550-LETTER-TAIL SECTION.
       1550-START.
           MOVE SPACES TO LTR-REC
           STRING "Oldest item due " D-OLDEST ", "
                  "disputed amounts are excluded from this letter."
               DELIMITED BY SIZE INTO LTR-REC
           WRITE LTR-REC
           IF WS-STEP-DEMAND AND D-LEGAL(1:10) = "SOLETRADER"
      *>       pre-action protocol for debt claims: an individual
      *>       debtor gets the information sheet, reply form and at
      *>       least 30 days to respond
               MOVE SPACES TO LTR-REC
               STRING "Letter of claim under the Pre-Action "
                      "Protocol for Debt Claims.  Information sheet"
                   DELIMITED BY SIZE INTO LTR-REC
               WRITE LTR-REC
               MOVE SPACES TO LTR-REC
               STRING "and reply form enclosed.  You have 30 days "
                      "from the date of this letter to respond."
                   DELIMITED BY SIZE INTO LTR-REC
               WRITE LTR-REC
           END-IF
           MOVE SPACES TO LTR-REC
           STRING "Please contact " FUNCTION TRIM(D-OWNER)
                  " if the amount is wrong or already paid."
               DELIMITED BY SIZE INTO LTR-REC
           WRITE LTR-REC
           MOVE SPACES TO LTR-REC
           WRITE LTR-REC.
       1550-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-015, AR-016  maintain the collection worklist
      *>----------------------------------------------------------------
       1600-UPDATE-WORKLIST SECTION.
       1600-START.
           PERFORM 1650-FIND-CASE
           IF WS-CASE-ID = 0
               MOVE 4 TO WS-RC
               GO TO 1600-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.CollectionCase SET CollectionStep = ?, "
                  "NextActionDate = ?, NextAction = ?, OwnerUser = "
                  "ISNULL(OwnerUser,?), Status = CASE WHEN ? >= 4 "
                  "THEN 'ESCALATED' ELSE Status END, "
                  "PreActionProtocolSentAt = CASE WHEN ? = 4 THEN ? "
                  "ELSE PreActionProtocolSentAt END, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE CaseId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-STEP-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
                                SQLIO-PARM-VAL(5) SQLIO-PARM-VAL(6)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(5)
                       SQLIO-PARM-NULL(6)
           MOVE WS-NEXT-N TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE SPACES TO AR-TEXT
           STRING "Next step after " FUNCTION TRIM(D-STEP)
               DELIMITED BY SIZE INTO AR-TEXT
           MOVE AR-TEXT TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE D-OWNER TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE WS-ASAT TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(7)
           MOVE "N" TO SQLIO-PARM-NULL(7)
           MOVE WS-USER TO SQLIO-PARM-VAL(8)
           MOVE "N" TO SQLIO-PARM-NULL(8)
           MOVE WS-CASE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE "N" TO SQLIO-PARM-NULL(9)
           MOVE 9 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           MOVE 3 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           PERFORM 1680-WRITE-ACTION
           PERFORM 1690-STAMP-ITEMS.
       1600-EXIT.
           EXIT.

       1650-FIND-CASE SECTION.
       1650-START.
           MOVE 0 TO WS-CASE-ID
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 CONVERT(varchar(24),CaseId) FROM "
                  "ar.CollectionCase WHERE CompanyCode = ? AND "
                  "CustomerCode = ? AND CaseType = 'COLLECTION' AND "
                  "Status IN ('OPEN','ESCALATED') ORDER BY CaseId"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE D-CUST TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               COMPUTE WS-CASE-ID = FUNCTION NUMVAL(BR-COL(1))
               GO TO 1650-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO ar.CollectionCase (CompanyCode, "
                  "CustomerCode, CaseRef, CaseType, Reason, "
                  "OwnerUser, CollectionStep, NextActionDate, "
                  "Status, CreatedBy) VALUES (?, ?, ?, 'COLLECTION', "
                  "'Opened by the collection run', ?, 0, ?, 'OPEN', "
                  "?); SELECT CONVERT(varchar(24),SCOPE_IDENTITY())"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE D-CUST TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE SPACES TO AR-TEXT
           STRING "COLL-" FUNCTION TRIM(D-CUST)
               DELIMITED BY SIZE INTO AR-TEXT
           MOVE AR-TEXT(1:30) TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE D-OWNER TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE WS-NEXT-N TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(5)
           MOVE "N" TO SQLIO-PARM-NULL(5)
           MOVE WS-USER TO SQLIO-PARM-VAL(6)
           MOVE "N" TO SQLIO-PARM-NULL(6)
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               COMPUTE WS-CASE-ID = FUNCTION NUMVAL(BR-COL(1))
           END-IF.
       1650-EXIT.
           EXIT.

       1680-WRITE-ACTION SECTION.
       1680-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO ar.CollectionAction (CaseId, "
                  "ActionSeq, ActionType, ActionDate, PerformedBy, "
                  "Narrative) SELECT ?, ISNULL(MAX(ActionSeq),0) + "
                  "1, ?, ?, ?, ? FROM ar.CollectionAction WHERE "
                  "CaseId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CASE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
                                                 SQLIO-PARM-VAL(6)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(6)
           MOVE WS-ACTION-TYPE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE WS-ASAT TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE WS-USER TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE D-CHASE TO AR-MONEY-ED
           MOVE SPACES TO AR-TEXT
           STRING "Step " FUNCTION TRIM(D-STEP) " issued for GBP "
                  FUNCTION TRIM(AR-MONEY-ED) " chaseable"
               DELIMITED BY SIZE INTO AR-TEXT
           MOVE AR-TEXT TO SQLIO-PARM-VAL(5)
           MOVE "N" TO SQLIO-PARM-NULL(5)
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           MOVE 5 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       1680-EXIT.
           EXIT.

       1690-STAMP-ITEMS SECTION.
       1690-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.OpenItem SET LastReminderStep = ?, "
                  "LastReminderDate = ?, CollectionStatus = ?, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() WHERE "
                  "CompanyCode = ? AND CustomerCode = ? AND DueDate "
                  "< ? AND ResidualAmountGBP > 0 AND IsDisputed = 0 "
                  "AND SettlementStatus IN ('OPEN','PARTIAL')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-STEP-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-ASAT TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(2) SQLIO-PARM-VAL(7)
           MOVE "N" TO SQLIO-PARM-NULL(2) SQLIO-PARM-NULL(7)
           IF WS-STEP-NO >= 4
               MOVE "ESCALATED" TO SQLIO-PARM-VAL(3)
           ELSE
               MOVE "REMINDED" TO SQLIO-PARM-VAL(3)
           END-IF
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE WS-USER TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE WS-CO TO SQLIO-PARM-VAL(5)
           MOVE "N" TO SQLIO-PARM-NULL(5)
           MOVE D-CUST TO SQLIO-PARM-VAL(6)
           MOVE "N" TO SQLIO-PARM-NULL(6)
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           MOVE 6 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       1690-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-016  escalation is owned work, never a silent event
      *>----------------------------------------------------------------
       1700-RAISE-ESCALATION SECTION.
       1700-START.
           ADD 1 TO WS-ESCALATED
           INITIALIZE WS-EXCPRAIS
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXCPRAIS
           MOVE WS-CO TO CA-COMPANY OF WS-EXCPRAIS
           MOVE WS-USER TO CA-USER OF WS-EXCPRAIS
           MOVE WS-ASAT TO CA-BUSINESS-DATE OF WS-EXCPRAIS
           MOVE "AR" TO EX-MODULE
           MOVE "WARNING" TO EX-SEVERITY
           MOVE "COLLECTION ESCALATION" TO EX-CATEGORY
           MOVE "CUSTOMER" TO EX-DOCUMENT-TYPE
           MOVE D-CUST TO EX-DOCUMENT-REF
           MOVE D-OWNER TO EX-OWNER-USER
           MOVE "CREDITCTL" TO EX-OWNER-ROLE
           MOVE 2 TO EX-PRIORITY
           MOVE WS-NEXT-N TO EX-DUE-AT
           MOVE D-CHASE TO EX-AMOUNT-GBP
           MOVE SPACES TO EX-MESSAGE-TEXT
           MOVE D-CHASE TO AR-MONEY-UK
           STRING FUNCTION TRIM(D-STEP) " reached for "
                  FUNCTION TRIM(D-CUST) " - GBP "
                  FUNCTION TRIM(AR-MONEY-UK)
                  " chaseable, oldest item due " D-OLDEST
               DELIMITED BY SIZE INTO EX-MESSAGE-TEXT
           CALL "EXCPRAIS" USING WS-EXCPRAIS
           IF CA-RETURN-CODE OF WS-EXCPRAIS > 6
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-1701 exception not raised for "
                       FUNCTION TRIM(D-CUST)
           END-IF.
       1700-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-SET-WIDTHS SECTION.
       8000-START.
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 30
               MOVE 120 TO SQLIO-COL-WIDTH(AR-I)
           END-PERFORM.
       8000-EXIT.
           EXIT.

       8100-FORMAT-DATE SECTION.
       8100-START.
           MOVE AR-NUM-DATE TO AR-DATE-ED
           COMPUTE WS-UK-YYYY = FUNCTION NUMVAL(AR-DATE-ED(1:4))
           COMPUTE WS-UK-MM = FUNCTION NUMVAL(AR-DATE-ED(5:2))
           COMPUTE WS-UK-DD = FUNCTION NUMVAL(AR-DATE-ED(7:2)).
       8100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-DUNNING
           CLOSE RPT-FILE
           CLOSE LTR-FILE
           DISPLAY "STERP-I-0401 " T-ACCOUNTS " overdue account(s), "
                   WS-LETTERS " letter(s)"
           DISPLAY "STERP-I-0402 letters "
                   FUNCTION TRIM(WS-LETTER-NAME)
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "DUNNING" TO JL-JOB-NAME
           MOVE WS-JOBID TO JL-JOB-RUN-ID
           EVALUATE TRUE
               WHEN WS-RC = 0   MOVE "COMPLETE" TO JL-STATUS
               WHEN WS-RC = 4   MOVE "WARNING" TO JL-STATUS
               WHEN OTHER       MOVE "FAILED" TO JL-STATUS
           END-EVALUATE
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE WS-ROWS-OUT TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           MOVE WS-LETTER-NAME TO JL-LOG-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
