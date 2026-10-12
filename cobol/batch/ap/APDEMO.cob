      *>================================================================
      *> PROGRAM-ID : APDEMO
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : CREDITORS AND SUPPLIER SETTLEMENT (AP)
      *> PURPOSE    : DEMONSTRATION PAYMENT RUN DRIVER.  BUILDS A LIVE
      *>              BACS PAYMENT PROPOSAL OUT OF WHATEVER SUPPLIER
      *>              INVOICES ARE DUE, HAS IT APPROVED BY TWO
      *>              INDEPENDENT AUTHORISERS AND RELEASES IT INTO
      *>              PAYMENT INSTRUCTIONS, SO THAT APBACS CAN CUT A
      *>              STANDARD 18 FILE AND APREMIT CAN PRINT THE
      *>              REMITTANCES WITHOUT A HUMAN AT A SCREEN.
      *>              EVERYTHING GOES THROUGH THE ORDINARY COMMAREA
      *>              PROGRAMS - APPAYPRP, APPAYAPR AND APPAYRUN - SO
      *>              THE SEGREGATION OF DUTIES, THE APHASH CONTENT
      *>              HASH AND THE RESERVATION RULES ALL APPLY.  THE
      *>              PREPARER, THE TWO AUTHORISERS AND THE RELEASER
      *>              ARE FOUR BUSINESS ROLES; SYSADM AND THE
      *>              FINANCIAL CONTROLLER MAY NOT ORIGINATE A RUN.
      *>              THE DRIVER IS RE-RUNNABLE: IF A PROPOSAL FOR
      *>              THE PAY MONTH IS ALREADY ON FILE IT REPORTS IT
      *>              AND REUSES IT. A POSTED, UNRESERVED UTILITY
      *>              INVOICE IS THEN LEFT FOR AN INTERACTIVE RUN.
      *> RUN AS     : APDEMO CO=STC USER=APCLERK MODE=COMMIT
      *>              [PAYDATE=yyyymmdd] [CUTOFF=yyyymmdd]
      *>              [PREPARER=] [APPROVER=] [APPROVER2=] [RELEASER=]
      *> OUTPUT     : run\spool\APDEMO-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO, APPAYPRP,
      *>              APPAYAPR, APPAYRUN
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2026-10-06 STERP ORIGINAL - AP-011/AP-012/AP-013 DEMO DRIVER
      *>================================================================
      *> REQ: AP-011, AP-012, AP-013, AP-015, AP-016, AP-020, D-07
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APDEMO.
       AUTHOR. STERP.
       DATE-WRITTEN. 06/10/2026.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO DYNAMIC WS-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-APDEMO.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY APBATWK.
       COPY STRPTWS.
       COPY STBATLOG.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY APPAYPRPCA REPLACING ==DFHCOMMAREA== BY ==WS-PRP-CA==.
       COPY APPAYAPRCA REPLACING ==DFHCOMMAREA== BY ==WS-APR-CA==.
       COPY APPAYRUNCA REPLACING ==DFHCOMMAREA== BY ==WS-RUN-CA==.
       COPY APINVREGCA REPLACING ==DFHCOMMAREA== BY ==WS-GUI-REG==.
       COPY APBLOCKCA REPLACING ==DFHCOMMAREA== BY ==WS-GUI-BLOCK==.
       COPY APPOSTWK.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
           05  WS-PREPARER             PIC X(20) VALUE SPACES.
           05  WS-APPROVER-1           PIC X(20) VALUE SPACES.
           05  WS-APPROVER-2           PIC X(20) VALUE SPACES.
           05  WS-RELEASER             PIC X(20) VALUE SPACES.
       01  WS-RUN-STATE.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-INT                  PIC S9(9) VALUE 0.
           05  WS-PAY-DATE             PIC 9(8) VALUE 0.
           05  WS-CUTOFF-DATE          PIC 9(8) VALUE 0.
           05  WS-MONTH-START          PIC 9(8) VALUE 0.
           05  WS-EXIST-REF            PIC X(30) VALUE SPACES.
           05  WS-EXIST-STATUS         PIC X(14) VALUE SPACES.
           05  WS-PROPOSAL-REF         PIC X(30) VALUE SPACES.
           05  WS-PROPOSAL-ID          PIC S9(9) VALUE 0.
           05  WS-APPROVAL-ID          PIC S9(9) VALUE 0.
           05  WS-SKIPPED              PIC X VALUE "N".
           05  WS-STEP-NO              PIC 9(2) VALUE 0.
       01  WS-GUI-FIXTURE.
           05  WS-GUI-REF              PIC X(40)
               VALUE "APDEMO-GUI-UTILITY-2026".
           05  WS-GUI-CORRELATION      PIC X(64).
           05  WS-GUI-ID               PIC S9(9) VALUE 0.
           05  WS-GUI-READY            PIC X VALUE "N".
      *>   one line of the step log that becomes the listing
       01  WS-DET.
           05  D-STEP                  PIC 9(2) VALUE 0.
           05  D-PROGRAM               PIC X(8) VALUE SPACES.
           05  D-FUNCTION              PIC X(10) VALUE SPACES.
           05  D-USER                  PIC X(20) VALUE SPACES.
           05  D-RC                    PIC S9(4) VALUE 0.
           05  D-REASON                PIC X(8) VALUE SPACES.
           05  D-MESSAGE               PIC X(62) VALUE SPACES.
       01  WS-TOT.
           05  T-STEPS                 PIC S9(4) VALUE 0.
           05  T-SELECTED              PIC S9(6) VALUE 0.
           05  T-EXCLUDED              PIC S9(6) VALUE 0.
           05  T-INSTRUCTIONS          PIC S9(6) VALUE 0.
           05  T-OUTFLOW               PIC S9(13)V99 VALUE 0.
       REPORT SECTION.
       RD  R-APDEMO
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 52
           FOOTING 58.
       COPY STRPTHDR.
       01  PH-COLS TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(4) VALUE "Step".
               10  COLUMN 7    PIC X(7) VALUE "Program".
               10  COLUMN 17   PIC X(8) VALUE "Function".
               10  COLUMN 29   PIC X(4) VALUE "User".
               10  COLUMN 51   PIC X(2) VALUE "RC".
               10  COLUMN 57   PIC X(6) VALUE "Reason".
               10  COLUMN 67   PIC X(7) VALUE "Outcome".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 2    PIC Z9 SOURCE D-STEP.
               10  COLUMN 7    PIC X(8) SOURCE D-PROGRAM.
               10  COLUMN 17   PIC X(10) SOURCE D-FUNCTION.
               10  COLUMN 29   PIC X(20) SOURCE D-USER.
               10  COLUMN 50   PIC ZZ9 SOURCE D-RC.
               10  COLUMN 57   PIC X(8) SOURCE D-REASON.
               10  COLUMN 67   PIC X(62) SOURCE D-MESSAGE.
       01  DE-NOTE TYPE IS DETAIL.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(100) SOURCE RPT-STATUS-TEXT.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(22) VALUE "Proposal             ".
               10  COLUMN 24   PIC X(30) SOURCE WS-PROPOSAL-REF.
               10  COLUMN 57   PIC X(22) VALUE "Items selected       ".
               10  COLUMN 80   PIC ZZZ,ZZ9 SOURCE T-SELECTED.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(22) VALUE "Cash required GBP    ".
               10  COLUMN 24   PIC ZZZ,ZZZ,ZZ9.99- SOURCE T-OUTFLOW.
               10  COLUMN 57   PIC X(22) VALUE "Items excluded       ".
               10  COLUMN 80   PIC ZZZ,ZZ9 SOURCE T-EXCLUDED.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(22) VALUE "Instructions created ".
               10  COLUMN 24   PIC ZZZ,ZZ9 SOURCE T-INSTRUCTIONS.
               10  COLUMN 57   PIC X(22) VALUE "Steps logged         ".
               10  COLUMN 80   PIC ZZZ,ZZ9 SOURCE T-STEPS.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(62) VALUE
                   "Demonstration data only.  The preparer, the two".
               10  COLUMN 66   PIC X(62) VALUE
                   "authorisers and the releaser are four people.".
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           PERFORM 0100-READ-ARGS
           PERFORM 0200-JOB-START
           PERFORM 0300-OPEN-SPOOL
           IF WS-RC < 8
               INITIATE R-APDEMO
               GENERATE PH-COLS
               PERFORM 0350-CHECK-EXISTING
               IF WS-RC < 8 AND WS-SKIPPED = "N"
                   PERFORM 1000-BUILD-RUN
               END-IF
               IF WS-RC < 6 AND WS-MODE = "COMMIT"
                  AND WS-CO = "STC"
                   PERFORM 2000-GUI-PAYABLE
               END-IF
               GENERATE DE-NOTE
               TERMINATE R-APDEMO
               PERFORM 0500-CLOSE-SPOOL
           END-IF.
       0000-FINISH.
           PERFORM 0600-JOB-END
           MOVE WS-RC TO RETURN-CODE
           STOP RUN.

      *>----------------------------------------------------------------
      *>   REQ: D-07 - the four duties are kept apart.  SYSADM and the
      *>   financial controller may not originate a payment run, so a
      *>   USER= of either is taken as the operator, never the
      *>   preparer.
       0100-READ-ARGS SECTION.
       0100-START.
           INITIALIZE WS-CMDARGS
           MOVE "CMDLINE" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           MOVE CM-CO TO WS-CO
           MOVE CM-USER TO WS-USER
           MOVE CM-MODE TO WS-MODE
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "APCLERK" TO WS-USER
           END-IF
           IF WS-MODE = SPACES
               MOVE "COMMIT" TO WS-MODE
           END-IF
           MOVE WS-MODE TO BL-MODE
           MOVE "PREPARER" TO CM-LOOKUP-KEY
           PERFORM 0150-GET-ARG
           MOVE CM-LOOKUP-VALUE TO WS-PREPARER
           MOVE "APPROVER" TO CM-LOOKUP-KEY
           PERFORM 0150-GET-ARG
           MOVE CM-LOOKUP-VALUE TO WS-APPROVER-1
           MOVE "APPROVER2" TO CM-LOOKUP-KEY
           PERFORM 0150-GET-ARG
           MOVE CM-LOOKUP-VALUE TO WS-APPROVER-2
           MOVE "RELEASER" TO CM-LOOKUP-KEY
           PERFORM 0150-GET-ARG
           MOVE CM-LOOKUP-VALUE TO WS-RELEASER
           IF WS-PREPARER = SPACES
               MOVE WS-USER TO WS-PREPARER
           END-IF
           IF WS-PREPARER = "SYSADM" OR WS-PREPARER = "FINCTRL"
              OR WS-PREPARER = "TREASURY"
               MOVE "APCLERK" TO WS-PREPARER
           END-IF
           IF WS-APPROVER-1 = SPACES
               MOVE "TREASURY" TO WS-APPROVER-1
           END-IF
           IF WS-APPROVER-2 = SPACES
               MOVE "TREASURY2" TO WS-APPROVER-2
           END-IF
           IF WS-RELEASER = SPACES
               MOVE "TREASURY" TO WS-RELEASER
           END-IF
           PERFORM 0180-WORK-OUT-DATES
           DISPLAY "STERP-I-0100 APDEMO payment run driver for "
                   FUNCTION TRIM(WS-CO).
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

      *>   A pay date a week out and a cut-off three weeks out pick up
      *>   whatever the trading demo has left unpaid.  APPAYPRP moves
      *>   the pay date onto the next Bacs processing day itself.
       0180-WORK-OUT-DATES SECTION.
       0180-START.
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           COMPUTE WS-INT = FUNCTION INTEGER-OF-DATE(WS-TODAY-N)
           COMPUTE WS-PAY-DATE = FUNCTION DATE-OF-INTEGER(WS-INT + 7)
           COMPUTE WS-CUTOFF-DATE =
               FUNCTION DATE-OF-INTEGER(WS-INT + 21)
           MOVE "PAYDATE" TO CM-LOOKUP-KEY
           PERFORM 0150-GET-ARG
           IF CM-LOOKUP-VALUE NOT = SPACES
               COMPUTE WS-PAY-DATE = FUNCTION NUMVAL(CM-LOOKUP-VALUE)
           END-IF
           MOVE "CUTOFF" TO CM-LOOKUP-KEY
           PERFORM 0150-GET-ARG
           IF CM-LOOKUP-VALUE NOT = SPACES
               COMPUTE WS-CUTOFF-DATE =
                   FUNCTION NUMVAL(CM-LOOKUP-VALUE)
           END-IF
           MOVE WS-PAY-DATE TO WS-DATE-N
           COMPUTE WS-MONTH-START =
               WS-DATE-YYYY * 10000 + WS-DATE-MM * 100 + 1.
       0180-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "APDEMO" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE "APDEMO" TO BL-JOB-NAME.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "APDEMO" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0301 cannot build the spool path"
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           MOVE RP-FULL-PATH TO BL-SPOOL-FILE
           PERFORM 0320-FILL-HEADINGS
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0302 cannot open " WS-SPOOL-NAME
           END-IF.
       0300-EXIT.
           EXIT.

       0320-FILL-HEADINGS SECTION.
       0320-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "APDEMO" TO RPT-REPORT-ID
           MOVE "Demonstration payment run" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE SPACES TO RPT-STATUS-TEXT
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE WS-CUTOFF-DATE TO WS-DATE-N
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "Cut-off " WS-DATE-DD "/" WS-DATE-MM "/"
                  WS-DATE-YYYY
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           MOVE WS-PAY-DATE TO WS-DATE-N
           MOVE SPACES TO RPT-SUBTITLE
           STRING "Pay date " WS-DATE-DD "/" WS-DATE-MM "/"
                  WS-DATE-YYYY " prepared by "
                  FUNCTION TRIM(WS-PREPARER)
               DELIMITED BY SIZE INTO RPT-SUBTITLE
           PERFORM 0330-READ-COMPANY.
       0320-EXIT.
           EXIT.

       0330-READ-COMPANY SECTION.
       0330-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Name, ISNULL(RegisteredNumber,'') FROM "
                  "core.Company WHERE CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 0290-SET-WIDTHS
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-OK
               MOVE AP-COL(1) TO RPT-COMPANY-NAME
               MOVE AP-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0330-EXIT.
           EXIT.

       0290-SET-WIDTHS SECTION.
       0290-START.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0290-EXIT.
           EXIT.

      *>   REQ: AP-011 - re-runnable.  One live demonstration proposal
      *>   per pay month is enough; a second one would reserve the
      *>   same payables twice and the operator would not know which
      *>   file the bureau was sent.
       0350-CHECK-EXISTING SECTION.
       0350-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) ProposalRef, Status FROM "
                  "ap.PaymentProposal WHERE CompanyCode = ? AND "
                  "PreparedBy = ? AND Status IN ('DRAFT','PROPOSED',"
                  "'APPROVED','RELEASED') AND PayDate >= "
                  "CAST(? AS date) AND PayDate < DATEADD(month, 1, "
                  "CAST(? AS date)) ORDER BY ProposalId DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-PREPARER TO SQLIO-PARM-VAL(2)
           MOVE WS-MONTH-START TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 0290-SET-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0351 " SQLIO-ERRMSG
               GO TO 0350-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 0350-EXIT
           END-IF
           MOVE AP-COL(1) TO WS-EXIST-REF
           MOVE AP-COL(2) TO WS-EXIST-STATUS
           MOVE WS-EXIST-REF TO WS-PROPOSAL-REF
           MOVE "Y" TO WS-SKIPPED
           INITIALIZE WS-DET
           ADD 1 TO WS-STEP-NO
           MOVE WS-STEP-NO TO D-STEP
           MOVE "APDEMO" TO D-PROGRAM
           MOVE "SKIP" TO D-FUNCTION
           MOVE WS-PREPARER TO D-USER
           MOVE 0 TO D-RC
           MOVE "APD0001" TO D-REASON
           MOVE SPACES TO D-MESSAGE
           STRING "Proposal " FUNCTION TRIM(WS-EXIST-REF)
                  " is already " FUNCTION TRIM(WS-EXIST-STATUS)
                  " for this pay month"
               DELIMITED BY SIZE INTO D-MESSAGE
           GENERATE DE-LINE
           ADD 1 TO T-STEPS
           MOVE SPACES TO RPT-STATUS-TEXT
           STRING "Nothing to do - run APBACS and APREMIT with RUN="
                  FUNCTION TRIM(WS-EXIST-REF)
               DELIMITED BY SIZE INTO RPT-STATUS-TEXT
           DISPLAY "STERP-I-0352 proposal "
                   FUNCTION TRIM(WS-EXIST-REF)
                   " already exists - nothing to build".
       0350-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   The real sequence, exactly as the Payment proposal and run
      *>   screen drives it.  REQ: AP-011, AP-012, AP-013
       1000-BUILD-RUN SECTION.
       1000-START.
           PERFORM 1100-PROPOSE
           IF WS-RC >= 8
               GO TO 1000-EXIT
           END-IF
           PERFORM 1200-REQUEST
           IF WS-RC >= 8
               GO TO 1000-EXIT
           END-IF
           PERFORM 1300-APPROVE-ONE
           IF WS-RC >= 8
               GO TO 1000-EXIT
           END-IF
           PERFORM 1400-APPROVE-TWO
           IF WS-RC >= 8
               GO TO 1000-EXIT
           END-IF
           PERFORM 1500-RELEASE
           IF WS-RC >= 8
               GO TO 1000-EXIT
           END-IF
           MOVE SPACES TO RPT-STATUS-TEXT
           STRING "Now run APBACS CO=" FUNCTION TRIM(WS-CO)
                  " RUN=" FUNCTION TRIM(WS-PROPOSAL-REF)
                  " and APREMIT for the same run"
               DELIMITED BY SIZE INTO RPT-STATUS-TEXT.
       1000-EXIT.
           EXIT.

      *>   REQ: AP-011, AP-020 - selection and reservation in one go.
       1100-PROPOSE SECTION.
       1100-START.
           INITIALIZE WS-PRP-CA
           MOVE "RESERVE" TO CA-FUNCTION OF WS-PRP-CA
           MOVE WS-CO TO CA-COMPANY OF WS-PRP-CA
           MOVE WS-PREPARER TO CA-USER OF WS-PRP-CA
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-PRP-CA
           MOVE 1 TO PP-VERSION
           MOVE WS-CUTOFF-DATE TO PP-CUTOFF-DATE
           MOVE WS-PAY-DATE TO PP-PAY-DATE
           MOVE "BACS" TO PP-METHOD
           MOVE "GBP1" TO PP-BANK-ACCOUNT
           MOVE "GBP" TO PP-CURRENCY
           MOVE 0 TO PP-MIN-PAYMENT
           MOVE 250000 TO PP-CHAPS-THRESHOLD
           MOVE 1000000 TO PP-SINGLE-LIMIT
           MOVE WS-MODE TO PP-MODE
           CALL "APPAYPRP" USING WS-PRP-CA
           MOVE PP-PROPOSAL-REF TO WS-PROPOSAL-REF
           MOVE PP-PROPOSAL-ID TO WS-PROPOSAL-ID
           MOVE PP-SELECTED-CNT TO T-SELECTED
           MOVE PP-EXCLUDED-CNT TO T-EXCLUDED
           MOVE PP-TOTAL-OUTFLOW TO T-OUTFLOW
           MOVE "APPAYPRP" TO D-PROGRAM
           MOVE "RESERVE" TO D-FUNCTION
           MOVE WS-PREPARER TO D-USER
           MOVE CA-RETURN-CODE OF WS-PRP-CA TO D-RC
           MOVE CA-REASON-CODE OF WS-PRP-CA TO D-REASON
           MOVE CA-MESSAGE OF WS-PRP-CA TO D-MESSAGE
           PERFORM 1900-LOG-STEP
           IF PP-SELECTED-CNT = 0
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-1101 nothing is due for payment"
           END-IF.
       1100-EXIT.
           EXIT.

      *>   REQ: AP-012 - the hash of the exact content is built by
      *>   APHASH inside APPAYAPR, never by a seed script.
       1200-REQUEST SECTION.
       1200-START.
           INITIALIZE WS-APR-CA
           MOVE "REQUEST" TO CA-FUNCTION OF WS-APR-CA
           MOVE WS-CO TO CA-COMPANY OF WS-APR-CA
           MOVE WS-PREPARER TO CA-USER OF WS-APR-CA
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-APR-CA
           MOVE WS-PROPOSAL-REF TO PA-PROPOSAL-REF
           MOVE WS-PROPOSAL-ID TO PA-PROPOSAL-ID
           CALL "APPAYAPR" USING WS-APR-CA
           MOVE PA-APPROVAL-ID TO WS-APPROVAL-ID
           MOVE "APPAYAPR" TO D-PROGRAM
           MOVE "REQUEST" TO D-FUNCTION
           MOVE WS-PREPARER TO D-USER
           MOVE CA-RETURN-CODE OF WS-APR-CA TO D-RC
           MOVE CA-REASON-CODE OF WS-APR-CA TO D-REASON
           MOVE CA-MESSAGE OF WS-APR-CA TO D-MESSAGE
           PERFORM 1900-LOG-STEP.
       1200-EXIT.
           EXIT.

      *>   REQ: AP-012, D-07 - the first authoriser is not the
      *>   preparer; core.usp_DecideApproval enforces it as well.
       1300-APPROVE-ONE SECTION.
       1300-START.
           INITIALIZE WS-APR-CA
           MOVE "DECIDE" TO CA-FUNCTION OF WS-APR-CA
           MOVE WS-CO TO CA-COMPANY OF WS-APR-CA
           MOVE WS-APPROVER-1 TO CA-USER OF WS-APR-CA
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-APR-CA
           MOVE WS-PROPOSAL-REF TO PA-PROPOSAL-REF
           MOVE WS-PROPOSAL-ID TO PA-PROPOSAL-ID
           MOVE WS-APPROVAL-ID TO PA-APPROVAL-ID
           MOVE "APPROVED" TO PA-DECISION
           MOVE "Demonstration payment run" TO PA-COMMENT
           CALL "APPAYAPR" USING WS-APR-CA
           MOVE "APPAYAPR" TO D-PROGRAM
           MOVE "DECIDE 1" TO D-FUNCTION
           MOVE WS-APPROVER-1 TO D-USER
           MOVE CA-RETURN-CODE OF WS-APR-CA TO D-RC
           MOVE CA-REASON-CODE OF WS-APR-CA TO D-REASON
           MOVE CA-MESSAGE OF WS-APR-CA TO D-MESSAGE
           PERFORM 1900-LOG-STEP
           MOVE PA-STATUS TO WS-EXIST-STATUS.
       1300-EXIT.
           EXIT.

      *>   REQ: D-07 - the second authoriser is a different person
      *>   again.  If one signature was enough the step is skipped.
       1400-APPROVE-TWO SECTION.
       1400-START.
           IF WS-EXIST-STATUS = "APPROVED"
               GO TO 1400-EXIT
           END-IF
           INITIALIZE WS-APR-CA
           MOVE "DECIDE" TO CA-FUNCTION OF WS-APR-CA
           MOVE WS-CO TO CA-COMPANY OF WS-APR-CA
           MOVE WS-APPROVER-2 TO CA-USER OF WS-APR-CA
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-APR-CA
           MOVE WS-PROPOSAL-REF TO PA-PROPOSAL-REF
           MOVE WS-PROPOSAL-ID TO PA-PROPOSAL-ID
           MOVE WS-APPROVAL-ID TO PA-APPROVAL-ID
           MOVE "APPROVED" TO PA-DECISION
           MOVE "Second independent authorisation" TO PA-COMMENT
           CALL "APPAYAPR" USING WS-APR-CA
           MOVE "APPAYAPR" TO D-PROGRAM
           MOVE "DECIDE 2" TO D-FUNCTION
           MOVE WS-APPROVER-2 TO D-USER
           MOVE CA-RETURN-CODE OF WS-APR-CA TO D-RC
           MOVE CA-REASON-CODE OF WS-APR-CA TO D-REASON
           MOVE CA-MESSAGE OF WS-APR-CA TO D-MESSAGE
           PERFORM 1900-LOG-STEP.
       1400-EXIT.
           EXIT.

      *>   REQ: AP-013 - releasing turns the approved content into
      *>   payment instructions.  Producing an instruction is not
      *>   payment, so APPAYRUN answers 04 and that is not an error.
       1500-RELEASE SECTION.
       1500-START.
           INITIALIZE WS-RUN-CA
           MOVE "CREATE" TO CA-FUNCTION OF WS-RUN-CA
           MOVE WS-CO TO CA-COMPANY OF WS-RUN-CA
           MOVE WS-RELEASER TO CA-USER OF WS-RUN-CA
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-RUN-CA
           MOVE WS-PROPOSAL-REF TO PR-PROPOSAL-REF
           MOVE WS-PROPOSAL-ID TO PR-PROPOSAL-ID
           MOVE "BACS" TO PR-METHOD
           CALL "APPAYRUN" USING WS-RUN-CA
           MOVE PR-CREATED-CNT TO T-INSTRUCTIONS
           MOVE "APPAYRUN" TO D-PROGRAM
           MOVE "CREATE" TO D-FUNCTION
           MOVE WS-RELEASER TO D-USER
           MOVE CA-RETURN-CODE OF WS-RUN-CA TO D-RC
           MOVE CA-REASON-CODE OF WS-RUN-CA TO D-REASON
           MOVE CA-MESSAGE OF WS-RUN-CA TO D-MESSAGE
           PERFORM 1900-LOG-STEP.
       1500-EXIT.
           EXIT.

      *> Leave one real, unreserved liability AFTER the example run.
      *> Utilities are an authorised non-PO category: no invented PO,
      *> receipt or GRNI match. Retries reuse both register and journal.
       2000-GUI-PAYABLE SECTION.
       2000-START.
           MOVE SPACES TO WS-GUI-CORRELATION
           STRING "APDEMO-GUI-" FUNCTION TRIM(WS-CO) "-2026"
               DELIMITED BY SIZE INTO WS-GUI-CORRELATION
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(p.PurchaseInvoiceId AS varchar(24)), "
                  "CASE WHEN p.AccountingState = 'POSTED' AND EXISTS"
                  " (SELECT 1 FROM ap.OpenItem oi WHERE "
                  "oi.PurchaseInvoiceId = p.PurchaseInvoiceId AND "
                  "oi.IsBlocked = 0) THEN 'Y' ELSE 'N' END FROM "
                  "ap.PurchaseInvoice p WHERE p.CompanyCode = ? "
                  "AND p.SupplierCode = 'S0002' "
                  "AND p.SupplierDocRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT SQLIO-COL-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-GUI-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           PERFORM 0290-SET-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-2001 " SQLIO-ERRMSG
               GO TO 2000-EXIT
           END-IF
           IF SQLIO-OK
               COMPUTE WS-GUI-ID = FUNCTION NUMVAL(AP-COL(1))
               MOVE AP-COL(2)(1:1) TO WS-GUI-READY
           END-IF
           IF WS-GUI-READY = "Y"
               DISPLAY "STERP-I-2002 GUI utility invoice already "
                       "recognised; existing reservations unchanged"
               GO TO 2000-EXIT
           END-IF
           INITIALIZE WS-GUI-REG
           MOVE WS-CO TO CA-COMPANY OF WS-GUI-REG
           MOVE "RDG" TO CA-SITE OF WS-GUI-REG
           MOVE "APCLERK" TO CA-USER OF WS-GUI-REG
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-GUI-REG
           MOVE WS-GUI-CORRELATION TO
               CA-CORRELATION-ID OF WS-GUI-REG
           IF WS-GUI-ID > 0
               MOVE WS-GUI-ID TO IR-INVOICE-ID
               MOVE "GET" TO CA-FUNCTION OF WS-GUI-REG
           ELSE
               MOVE "REGISTER" TO CA-FUNCTION OF WS-GUI-REG
               MOVE "S0002" TO IR-SUPPLIER
               MOVE "INVOICE" TO IR-DOC-TYPE
               MOVE WS-GUI-REF TO IR-SUPPLIER-REF
               MOVE 20261001 TO IR-DOCUMENT-DATE IR-TAX-POINT
               MOVE 20261012 TO IR-DATE-RECEIVED IR-POSTING-DATE
               MOVE "GBP" TO IR-CURRENCY
               MOVE "N45" TO IR-TERMS-CODE
               MOVE 1 TO IR-FX-RATE
               MOVE "PAPER" TO IR-SOURCE-CHANNEL
               MOVE "GB123456789" TO IR-SUPPLIER-VATNO
               MOVE "VATINVOICE" TO IR-EVIDENCE-TYPE
               MOVE "APDEMO fictional utility invoice"
                   TO IR-ATTACHMENT-REF
               MOVE "Y" TO IR-IS-NON-PO
               MOVE "UTILITIES" TO IR-NONPO-REASON
               MOVE "NOTREQUIRED" TO IR-MATCH-STATE
               MOVE 123.45 TO IR-STATED-NET
               MOVE 24.69 TO IR-STATED-VAT
               MOVE 148.14 TO IR-STATED-GROSS
               MOVE 1 TO IR-LINE-CNT IR-L-NUM(1) IR-L-QTY(1)
                         IR-L-CONV(1) IR-L-PRICE-PER(1)
               MOVE "Fictional utility service, October 2026"
                   TO IR-L-DESC(1)
               MOVE "EA" TO IR-L-UOM(1)
               MOVE 123.45 TO IR-L-PRICE(1)
               MOVE "S" TO IR-L-VATCODE(1)
               MOVE "Y" TO IR-L-RECOVER(1)
               MOVE "6800" TO IR-L-ACCOUNT(1)
               MOVE "ADMIN" TO IR-L-COST-CTR(1)
           END-IF
           CALL "APINVREG" USING WS-GUI-REG
           MOVE "APINVREG" TO D-PROGRAM
           MOVE "GUI REG" TO D-FUNCTION
           MOVE "APCLERK" TO D-USER
           MOVE CA-RETURN-CODE OF WS-GUI-REG TO D-RC
           MOVE CA-REASON-CODE OF WS-GUI-REG TO D-REASON
           MOVE CA-MESSAGE OF WS-GUI-REG TO D-MESSAGE
           PERFORM 1900-LOG-STEP
           IF WS-RC >= 6 GO TO 2000-EXIT END-IF
           MOVE IR-INVOICE-ID TO WS-GUI-ID
           PERFORM 2050-READ-GUI-POSTING
           IF WS-RC >= 6 GO TO 2000-EXIT END-IF
           INITIALIZE WS-APPOST
           MOVE "POST" TO PJ-FUNC
           MOVE WS-CO TO PJ-COMPANY
           MOVE "RDG" TO PJ-SITE
           MOVE "APCLERK" TO PJ-USER
           MOVE WS-TODAY-N TO PJ-BUSINESS-DATE
           MOVE WS-GUI-CORRELATION TO PJ-CORRELATION
           MOVE "AP.INVOICE" TO PJ-EVENT-CODE
           MOVE IR-REGISTER-NUMBER TO PJ-SOURCE-REF
           MOVE IR-POSTING-DATE TO PJ-POSTING-DATE
           MOVE IR-DOCUMENT-DATE TO PJ-DOCUMENT-DATE
           MOVE "GBP" TO PJ-CURRENCY
           MOVE 1 TO PJ-FX-RATE
           MOVE "AP" TO PJ-SUBLEDGER-TYPE
           MOVE "S0002" TO PJ-SUBLEDGER-REF
           MOVE WS-GUI-REF TO PJ-EVIDENCE-REF
           MOVE "GUI utility payable" TO PJ-DESCRIPTION
           MOVE 3 TO PJ-LINE-CNT
           MOVE "6800" TO PJ-L-ACCOUNT(1)
           MOVE IR-NET-GBP TO PJ-L-DR(1)
           MOVE "ADMIN" TO PJ-L-COST-CTR(1)
           MOVE "1250" TO PJ-L-ACCOUNT(2)
           MOVE IR-RECOVERABLE-GBP TO PJ-L-DR(2)
           MOVE "S" TO PJ-L-VATCODE(2)
           MOVE "2000" TO PJ-L-ACCOUNT(3)
           MOVE IR-GROSS-GBP TO PJ-L-CR(3)
           CALL "APPOST" USING WS-APPOST
           MOVE "APPOST" TO D-PROGRAM
           MOVE "GUI POST" TO D-FUNCTION
           MOVE PJ-RC TO D-RC
           MOVE PJ-REASON TO D-REASON
           MOVE PJ-MESSAGE TO D-MESSAGE
           PERFORM 1900-LOG-STEP
           IF WS-RC >= 6 GO TO 2000-EXIT END-IF
           PERFORM 2100-RECOGNISE-GUI
           IF WS-RC >= 6 GO TO 2000-EXIT END-IF
           INITIALIZE WS-GUI-BLOCK
           MOVE "RELEASE" TO CA-FUNCTION OF WS-GUI-BLOCK
           MOVE WS-CO TO CA-COMPANY OF WS-GUI-BLOCK
           MOVE "APMGR" TO CA-USER OF WS-GUI-BLOCK
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-GUI-BLOCK
           MOVE WS-GUI-CORRELATION TO
               CA-CORRELATION-ID OF WS-GUI-BLOCK
           MOVE WS-GUI-ID TO BK-INVOICE-ID
           MOVE "S0002" TO BK-SUPPLIER
           MOVE IR-REGISTER-NUMBER TO BK-DOCUMENT-NUMBER
           MOVE "Native posted utility invoice and VAT evidence"
               TO BK-RESOLUTION
           CALL "APBLOCK" USING WS-GUI-BLOCK
           MOVE "APBLOCK" TO D-PROGRAM
           MOVE "GUI READY" TO D-FUNCTION
           MOVE "APMGR" TO D-USER
           MOVE CA-RETURN-CODE OF WS-GUI-BLOCK TO D-RC
           MOVE CA-REASON-CODE OF WS-GUI-BLOCK TO D-REASON
           MOVE CA-MESSAGE OF WS-GUI-BLOCK TO D-MESSAGE
           PERFORM 1900-LOG-STEP
           IF WS-RC >= 6 GO TO 2000-EXIT END-IF
           DISPLAY "STERP-I-2003 GUI payable "
                   FUNCTION TRIM(IR-REGISTER-NUMBER)
                   " S0002 GBP 148.14 remains unreserved".
       2000-EXIT.
           EXIT.

       2050-READ-GUI-POSTING SECTION.
       2050-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT RegisterNumber, "
                  "CAST(NetAmountGBP AS varchar(24)), "
                  "CAST(RecoverableVatGBP AS varchar(24)), "
                  "CAST(GrossAmountGBP AS varchar(24)), "
                  "CONVERT(char(8), PostingDate, 112), "
                  "CONVERT(char(8), DocumentDate, 112) "
                  "FROM ap.PurchaseInvoice WHERE PurchaseInvoiceId=?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-GUI-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 6 TO SQLIO-COL-CNT
           PERFORM 0290-SET-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF NOT SQLIO-OK
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-2051 Cannot read GUI invoice posting"
               GO TO 2050-EXIT
           END-IF
           MOVE AP-COL(1) TO IR-REGISTER-NUMBER
           COMPUTE IR-NET-GBP = FUNCTION NUMVAL(AP-COL(2))
           COMPUTE IR-RECOVERABLE-GBP = FUNCTION NUMVAL(AP-COL(3))
           COMPUTE IR-GROSS-GBP = FUNCTION NUMVAL(AP-COL(4))
           COMPUTE IR-POSTING-DATE = FUNCTION NUMVAL(AP-COL(5))
           COMPUTE IR-DOCUMENT-DATE = FUNCTION NUMVAL(AP-COL(6)).
       2050-EXIT.
           EXIT.

      *> Registration and nominal posting have completed through their
      *> engines. Attach the proved journal and recognise its open item.
       2100-RECOGNISE-GUI SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE p SET AccountingState = 'POSTED', "
                  "JournalId = j.JournalId, FiscalYear = j.FiscalYear,"
                  " PeriodNo = j.PeriodNo FROM ap.PurchaseInvoice p "
                  "JOIN fin.Journal j ON j.JournalId = ? "
                  "AND j.Status = 'POSTED' "
                  "WHERE p.PurchaseInvoiceId = ?; "
                  "INSERT ap.OpenItem (CompanyCode, SupplierCode, "
                  "ItemType, DocumentNumber, SupplierDocRef, "
                  "DocumentDate, DueDate, PostingDate, DateReceived, "
                  "FiscalYear, PeriodNo, CurrencyCode, FxRate, "
                  "OriginalAmount, OriginalAmountGBP, ResidualAmount,"
                  "ResidualAmountGBP, IsBlocked, PurchaseInvoiceId, "
                  "JournalId, CreatedBy) SELECT p.CompanyCode, "
                  "p.SupplierCode, 'INVOICE', p.RegisterNumber, "
                  "p.SupplierDocRef, p.DocumentDate, p.DueDate, "
                  "p.PostingDate, p.DateReceived, p.FiscalYear, "
                  "p.PeriodNo, p.CurrencyCode, p.FxRate, "
                  "p.StatedGross, p.GrossAmountGBP, p.StatedGross, "
                  "p.GrossAmountGBP, 1, p.PurchaseInvoiceId, "
                  "p.JournalId, 'APCLERK' FROM ap.PurchaseInvoice p "
                  "WHERE p.PurchaseInvoiceId = ? "
                  "AND p.AccountingState = 'POSTED' AND NOT EXISTS "
                  "(SELECT 1 FROM ap.OpenItem oi WHERE "
                  "oi.PurchaseInvoiceId = p.PurchaseInvoiceId)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE PJ-JOURNAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE WS-GUI-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
                       SQLIO-PARM-NULL(3)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-2101 " SQLIO-ERRMSG
           END-IF.
       2100-EXIT.
           EXIT.

      *>   One listed line per COMMAREA call.  A 06 or worse stops the
      *>   stream; 04 is information, not a failure.
       1900-LOG-STEP SECTION.
       1900-START.
           ADD 1 TO WS-STEP-NO
           MOVE WS-STEP-NO TO D-STEP
           GENERATE DE-LINE
           ADD 1 TO T-STEPS
           ADD 1 TO BL-WRITTEN-COUNT
           IF D-RC >= 6
               MOVE 8 TO WS-RC
               ADD 1 TO BL-ERROR-COUNT
               DISPLAY "STERP-E-1901 " FUNCTION TRIM(D-PROGRAM)
                       " " FUNCTION TRIM(D-FUNCTION) " rc " D-RC
                       " " FUNCTION TRIM(D-MESSAGE)
           ELSE
               DISPLAY "STERP-I-1900 " FUNCTION TRIM(D-PROGRAM)
                       " " FUNCTION TRIM(D-FUNCTION) " rc " D-RC
           END-IF
           MOVE SPACES TO D-REASON D-MESSAGE.
       1900-EXIT.
           EXIT.

       0500-CLOSE-SPOOL SECTION.
       0500-START.
           CLOSE RPT-FILE.
       0500-EXIT.
           EXIT.

       0600-JOB-END SECTION.
       0600-START.
           MOVE WS-RC TO BL-RETURN-CODE
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "APDEMO" TO JL-JOB-NAME
           MOVE BL-WRITTEN-COUNT TO JL-ROWS-PROCESSED
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE BL-SPOOL-FILE TO JL-SPOOL-FILE
           MOVE WS-RC TO CA-RETURN-CODE OF WS-JOBLOG
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE T-INSTRUCTIONS TO BL-COUNT-ED
           DISPLAY "STERP-I-0600 APDEMO proposal "
                   FUNCTION TRIM(WS-PROPOSAL-REF)
                   " with " BL-COUNT-ED " instruction(s)"
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0600-EXIT.
           EXIT.
