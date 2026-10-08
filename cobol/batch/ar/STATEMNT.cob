      *>================================================================
      *> PROGRAM-ID : STATEMNT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : CUSTOMER STATEMENT RUN.  PRODUCES ONE 132 COLUMN
      *>              STATEMENT PER STATEMENT PARTY IN EITHER STYLE:
      *>                TRANS - OPENING BALANCE, EVERY MOVEMENT IN THE
      *>                        PERIOD, RUNNING AND CLOSING BALANCE
      *>                OPEN  - EVERY ITEM STILL UNSETTLED AT THE
      *>                        STATEMENT DATE WITH ITS AGE
      *>              AUTO TAKES THE STYLE FROM THE TUNING PARAMETER
      *>              AR.STMT.STYLE (PARMGET, DEFAULT OPEN) BECAUSE THE
      *>              CUSTOMER MASTER HAS NO STYLE COLUMN OF ITS OWN.
      *>              A CUSTOMER STATED BY ANOTHER PARTY IS REPORTED
      *>              UNDER THAT PARTY BUT EVERY LINE STILL NAMES ITS
      *>              OWN LEGAL DEBTOR - A GROUPED STATEMENT IS NOT A
      *>              LEGAL CONSOLIDATION.  ISSUE HISTORY (AND THE
      *>              REASON FOR ANY SUPPRESSION) IS RECORDED AGAINST
      *>              THE CUSTOMER COLLECTION CASE.
      *> RUN AS     : STATEMNT CO=STC TO=20261031 MODE=OPEN USER=ARCLK
      *>              [CUST=C0001] [FROM=20261001] [PREVIEW=Y]
      *> OUTPUT     : run\spool\STATEMNT-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, PARMGET, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-04-16 RJH  ORIGINAL MONTHLY STATEMENT RUN (MVS PORT)
      *> 1995-01-30 RJH  CR0366 OPEN ITEM STYLE FOR THE CONTRACT
      *>                 CUSTOMERS WHO WOULD NOT TAKE A BROUGHT
      *>                 FORWARD STATEMENT
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS
      *> 2008-06-02 KMP  CR2210 SUPPRESS STATEMENTS FOR ACCOUNTS WITH
      *>                 A RECORDED CONTACT RESTRICTION
      *> 2026-10-05 STERP AR-018 ISSUE HISTORY IN ar.CollectionAction
      *>================================================================
      *> REQ: AR-018
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STATEMNT.
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
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-STATEMNT.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ARWORK.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARMGET==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  BR-ROW.
           05  BR-COL OCCURS 30 TIMES  PIC X(120).
       01  PR-ROW.
           05  PR-COL OCCURS 12 TIMES  PIC X(120).
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-CUST                 PIC X(20) VALUE SPACES.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
           05  WS-PREVIEW              PIC X VALUE "N".
               88  WS-NO-WRITES                  VALUE "Y".
           05  WS-TO                   PIC 9(8) VALUE 0.
           05  WS-FROM                 PIC 9(8) VALUE 0.
       01  WS-RUN.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-JOBID                PIC S9(9) COMP-5 VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-SUPPRESSED           PIC S9(9) VALUE 0.
           05  WS-CASE-ID              PIC S9(9) VALUE 0.
           05  WS-STYLE                PIC X(6) VALUE SPACES.
           05  WS-RESID                PIC S9(13)V99 VALUE 0.
           05  WS-AMT                  PIC S9(13)V99 VALUE 0.
           05  WS-POST-N               PIC 9(8) VALUE 0.
           05  WS-DUE-N                PIC 9(8) VALUE 0.
           05  WS-PRINTED              PIC X VALUE "N".
           05  WS-SUP-REASON           PIC X(60) VALUE SPACES.
       01  WS-UK.
           05  WS-UK-DD                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-UK-MM                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-UK-YYYY              PIC 9(4) VALUE 0.
       01  WS-PARTY.
           05  P-CODE                  PIC X(20) VALUE SPACES.
           05  P-NAME                  PIC X(40) VALUE SPACES.
           05  P-RESTRICT              PIC X(60) VALUE SPACES.
           05  P-STATUS                PIC X(10) VALUE SPACES.
           05  P-FREQ                  PIC X(10) VALUE SPACES.
           05  P-STYLE                 PIC X(10) VALUE SPACES.
           05  P-FROM-UK               PIC X(10) VALUE SPACES.
           05  P-TO-UK                 PIC X(10) VALUE SPACES.
           05  P-OPEN                  PIC S9(13)V99 VALUE 0.
           05  P-MOVE-DR               PIC S9(13)V99 VALUE 0.
           05  P-MOVE-CR               PIC S9(13)V99 VALUE 0.
           05  P-CLOSE                 PIC S9(13)V99 VALUE 0.
           05  P-NOTDUE                PIC S9(13)V99 VALUE 0.
           05  P-B1                    PIC S9(13)V99 VALUE 0.
           05  P-B2                    PIC S9(13)V99 VALUE 0.
           05  P-B3                    PIC S9(13)V99 VALUE 0.
           05  P-B4                    PIC S9(13)V99 VALUE 0.
           05  P-LINES                 PIC S9(7) VALUE 0.
       01  WS-DET.
           05  D-DATE                  PIC X(10) VALUE SPACES.
           05  D-TYPE                  PIC X(8) VALUE SPACES.
           05  D-DOC                   PIC X(18) VALUE SPACES.
           05  D-DUE                   PIC X(10) VALUE SPACES.
           05  D-DEBTOR                PIC X(20) VALUE SPACES.
           05  D-CURR                  PIC X(3) VALUE SPACES.
           05  D-AMT                   PIC S9(13)V99 VALUE 0.
           05  D-OUT                   PIC S9(13)V99 VALUE 0.
           05  D-BAL                   PIC S9(13)V99 VALUE 0.
           05  D-FLAG                  PIC X(3) VALUE SPACES.
       01  WS-TOT.
           05  T-STMTS                 PIC S9(7) VALUE 0.
           05  T-CLOSE                 PIC S9(13)V99 VALUE 0.
       REPORT SECTION.
       RD  R-STATEMNT
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 8 LAST DETAIL 52
           FOOTING 58.
       COPY STRPTHDR.
       01  DE-PHEAD TYPE IS DETAIL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(10) VALUE "Statement ".
               10  COLUMN 12   PIC X(20) SOURCE P-CODE.
               10  COLUMN 33   PIC X(40) SOURCE P-NAME.
               10  COLUMN 76   PIC X(6) VALUE "Style ".
               10  COLUMN 82   PIC X(10) SOURCE P-STYLE.
               10  COLUMN 95   PIC X(10) VALUE "Frequency ".
               10  COLUMN 106  PIC X(10) SOURCE P-FREQ.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(16) VALUE "Period from".
               10  COLUMN 18   PIC X(10) SOURCE P-FROM-UK.
               10  COLUMN 30   PIC X(3) VALUE "to".
               10  COLUMN 34   PIC X(10) SOURCE P-TO-UK.
               10  COLUMN 50   PIC X(24) VALUE "Opening balance GBP".
               10  COLUMN 76   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE P-OPEN.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(4) VALUE "Date".
               10  COLUMN 12   PIC X(4) VALUE "Type".
               10  COLUMN 21   PIC X(8) VALUE "Document".
               10  COLUMN 40   PIC X(8) VALUE "Due date".
               10  COLUMN 51   PIC X(12) VALUE "Legal debtor".
               10  COLUMN 72   PIC X(3) VALUE "Cur".
               10  COLUMN 85   PIC X(8) VALUE "Amount".
               10  COLUMN 103  PIC X(11) VALUE "Outstanding".
               10  COLUMN 122  PIC X(7) VALUE "Balance".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-ITEM TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(10) SOURCE D-DATE.
               10  COLUMN 12   PIC X(8) SOURCE D-TYPE.
               10  COLUMN 21   PIC X(18) SOURCE D-DOC.
               10  COLUMN 40   PIC X(10) SOURCE D-DUE.
               10  COLUMN 51   PIC X(20) SOURCE D-DEBTOR.
               10  COLUMN 72   PIC X(3) SOURCE D-CURR.
               10  COLUMN 76   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-AMT.
               10  COLUMN 94   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-OUT.
               10  COLUMN 112  PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE D-BAL.
               10  COLUMN 130  PIC X(3) SOURCE D-FLAG.
       01  DE-PFOOT TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(22) VALUE "Movements  debits".
               10  COLUMN 24   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE P-MOVE-DR.
               10  COLUMN 44   PIC X(8) VALUE "credits".
               10  COLUMN 53   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE P-MOVE-CR.
               10  COLUMN 90   PIC X(20) VALUE "Closing balance GBP".
               10  COLUMN 112  PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE P-CLOSE.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(22) VALUE "Of the amount owed".
               10  COLUMN 24   PIC X(11) VALUE "not yet due".
               10  COLUMN 36   PIC Z,ZZZ,ZZ9.99- SOURCE P-NOTDUE.
               10  COLUMN 51   PIC X(4) VALUE "1-30".
               10  COLUMN 56   PIC Z,ZZZ,ZZ9.99- SOURCE P-B1.
               10  COLUMN 71   PIC X(5) VALUE "31-60".
               10  COLUMN 77   PIC Z,ZZZ,ZZ9.99- SOURCE P-B2.
               10  COLUMN 92   PIC X(5) VALUE "61-90".
               10  COLUMN 98   PIC Z,ZZZ,ZZ9.99- SOURCE P-B3.
               10  COLUMN 113  PIC X(4) VALUE "90+".
               10  COLUMN 118  PIC Z,ZZZ,ZZ9.99- SOURCE P-B4.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(46) VALUE
                   "Please quote the document number when paying.".
               10  COLUMN 50   PIC X(44) VALUE
                   "Queries to the credit control department.".
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(24) VALUE "Statements produced".
               10  COLUMN 26   PIC ZZZ,ZZ9 SOURCE T-STMTS.
               10  COLUMN 40   PIC X(22) VALUE "Suppressed".
               10  COLUMN 63   PIC ZZZ,ZZ9 SOURCE WS-SUPPRESSED.
               10  COLUMN 80   PIC X(22) VALUE "Total closing GBP".
               10  COLUMN 105  PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-CLOSE.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(48) VALUE
                   "A grouped statement does not consolidate the".
               10  COLUMN 50   PIC X(46) VALUE
                   "debts of separate legal debtors.".
       01  DE-SUPP TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(10) VALUE "Suppressed".
               10  COLUMN 12   PIC X(20) SOURCE P-CODE.
               10  COLUMN 33   PIC X(40) SOURCE P-NAME.
               10  COLUMN 75   PIC X(55) SOURCE WS-SUP-REASON.
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
           PERFORM 1000-RUN-STATEMENTS
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
           MOVE "CUST" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-CUST
           END-IF
           MOVE "PREVIEW" TO CM-LOOKUP-KEY
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y" AND CM-LOOKUP-VALUE(1:1) = "Y"
               MOVE "Y" TO WS-PREVIEW
           END-IF
           IF CM-TO NOT = SPACES
               COMPUTE WS-TO = FUNCTION NUMVAL(CM-TO)
           END-IF
           IF WS-TO = 0 AND CM-ASAT NOT = SPACES
               COMPUTE WS-TO = FUNCTION NUMVAL(CM-ASAT)
           END-IF
           IF CM-FROM NOT = SPACES
               COMPUTE WS-FROM = FUNCTION NUMVAL(CM-FROM)
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "ARCLERK1" TO WS-USER
           END-IF
           IF WS-TO = 0
               MOVE FUNCTION CURRENT-DATE TO AR-NOW
               COMPUTE WS-TO = FUNCTION NUMVAL(AR-NOW(1:8))
           END-IF
           IF WS-FROM = 0
      *>       default period is the month that the statement date
      *>       falls in - the monthly run after month end close
               COMPUTE WS-FROM = (WS-TO / 100)
               COMPUTE WS-FROM = (WS-FROM * 100) + 1
           END-IF
           IF WS-FROM > WS-TO
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0102 FROM is after TO"
               GO TO 0100-EXIT
           END-IF
           MOVE FUNCTION UPPER-CASE(WS-MODE) TO WS-MODE
           IF WS-MODE = SPACES
               MOVE "AUTO" TO WS-MODE
           END-IF
           PERFORM 0150-DEFAULT-STYLE
           DISPLAY "STERP-I-0100 STATEMNT statement run for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

       0150-DEFAULT-STYLE SECTION.
       0150-START.
      *>   AR-018 the customer master carries no statement style, so
      *>   AUTO asks the tuning parameter; the default never stops a
      *>   statement being produced (COM-025)
           MOVE "OPEN" TO WS-STYLE
           IF WS-MODE = "TRANS" OR WS-MODE = "OPEN"
               MOVE WS-MODE TO WS-STYLE
               GO TO 0150-EXIT
           END-IF
           INITIALIZE WS-PARMGET
           MOVE "GET" TO CA-FUNCTION OF WS-PARMGET
           MOVE WS-CO TO CA-COMPANY OF WS-PARMGET
           MOVE WS-USER TO CA-USER OF WS-PARMGET
           MOVE WS-TO TO CA-BUSINESS-DATE OF WS-PARMGET
           MOVE "AR.STMT.STYLE" TO PM-PARAM-CODE
           MOVE WS-TO TO PM-AS-OF
           MOVE "OPEN" TO PM-DEFAULT-TEXT
           MOVE "Y" TO PM-USE-DEFAULT
           CALL "PARMGET" USING WS-PARMGET
           IF PM-VALUE-TEXT(1:5) = "TRANS"
               MOVE "TRANS" TO WS-STYLE
           ELSE
               MOVE "OPEN" TO WS-STYLE
           END-IF.
       0150-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE WS-TO TO CA-BUSINESS-DATE OF WS-JOBLOG
           MOVE "STATEMNT" TO JL-JOB-NAME
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
           MOVE "STATEMNT" TO RP-REPORT-ID
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
           INITIATE R-STATEMNT.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "STATEMNT" TO RPT-REPORT-ID
           MOVE "Customer statements" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO AR-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(AR-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(AR-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(AR-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(AR-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(AR-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(AR-NOW(13:2))
           MOVE WS-TO TO AR-NUM-DATE
           PERFORM 8100-FORMAT-DATE
           MOVE WS-UK TO P-TO-UK
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "Statement date " WS-UK DELIMITED BY SIZE
               INTO RPT-AS-AT-TEXT
           MOVE WS-FROM TO AR-NUM-DATE
           PERFORM 8100-FORMAT-DATE
           MOVE WS-UK TO P-FROM-UK
           MOVE SPACES TO RPT-SUBTITLE
           STRING "Style " FUNCTION TRIM(WS-STYLE)
                  " - amounts in GBP unless shown otherwise"
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
      *> REQ: AR-018  one statement per statement party
      *>----------------------------------------------------------------
       1000-RUN-STATEMENTS SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT p.CustomerCode, p.LegalName, "
                  "ISNULL(p.ContactRestriction,''), p.Status, "
                  "ISNULL(p.StatementFrequency,'MONTHLY') FROM "
                  "ar.Customer p WHERE p.CompanyCode = ? AND EXISTS "
                  "(SELECT 1 FROM ar.Customer c WHERE c.CompanyCode "
                  "= p.CompanyCode AND ISNULL(NULLIF("
                  "c.StatementCustomerCode,''),c.CustomerCode) = "
                  "p.CustomerCode)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           IF WS-CUST = SPACES
               MOVE 1 TO SQLIO-PARM-CNT
           ELSE
               MOVE 2 TO SQLIO-PARM-CNT
               MOVE WS-CUST TO SQLIO-PARM-VAL(2)
               MOVE "N" TO SQLIO-PARM-NULL(2)
               MOVE " AND p.CustomerCode = ?" TO AR-TEXT
               PERFORM 8200-APPEND-SQL
           END-IF
           MOVE " ORDER BY p.CustomerCode" TO AR-TEXT
           PERFORM 8200-APPEND-SQL
           MOVE 5 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1001 " SQLIO-ERRMSG(1:100)
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO AR-DONE
           PERFORM UNTIL AR-FINISHED
               MOVE "FTCH" TO SQLIO-FUNC
               MOVE 1 TO SQLIO-CURSOR
               MOVE 5 TO SQLIO-COL-CNT
               CALL "SQLIO" USING SQLIO-CA PR-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO AR-DONE
               ELSE
                   PERFORM 1100-ONE-PARTY
               END-IF
           END-PERFORM
           MOVE 1 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA PR-ROW.
       1000-EXIT.
           EXIT.

       1100-ONE-PARTY SECTION.
       1100-START.
           MOVE PR-COL(1) TO P-CODE
           MOVE PR-COL(2) TO P-NAME
           MOVE PR-COL(3) TO P-RESTRICT
           MOVE PR-COL(4) TO P-STATUS
           MOVE PR-COL(5) TO P-FREQ
           MOVE WS-STYLE TO P-STYLE
           MOVE 0 TO P-OPEN P-MOVE-DR P-MOVE-CR P-CLOSE P-LINES
           MOVE 0 TO P-NOTDUE P-B1 P-B2 P-B3 P-B4
           MOVE "N" TO WS-PRINTED
           MOVE SPACES TO WS-SUP-REASON
      *>   AR-016 / AR-018 suppression rules, in the order the credit
      *>   control manual lists them
           IF P-STATUS(1:8) = "PROSPECT"
               MOVE "Prospect account - no ledger statement issued"
                   TO WS-SUP-REASON
               PERFORM 1800-SUPPRESS
               GO TO 1100-EXIT
           END-IF
           IF P-RESTRICT NOT = SPACES
               MOVE SPACES TO WS-SUP-REASON
               STRING "Contact restriction: "
                      FUNCTION TRIM(P-RESTRICT)
                   DELIMITED BY SIZE INTO WS-SUP-REASON
               PERFORM 1800-SUPPRESS
               GO TO 1100-EXIT
           END-IF
           PERFORM 1180-OPENING-BALANCE
           PERFORM 1200-READ-MOVEMENTS
           IF WS-RC > 4
               GO TO 1100-EXIT
           END-IF
           IF P-LINES = 0 AND P-OPEN = 0 AND P-CLOSE = 0
               MOVE "No balance and no movement in the period"
                   TO WS-SUP-REASON
               PERFORM 1800-SUPPRESS
               GO TO 1100-EXIT
           END-IF
           IF WS-PRINTED = "N"
      *>       a party whose only activity was before the period but
      *>       which still owes money gets an open item statement
               PERFORM 1150-PRINT-HEAD
           END-IF
           GENERATE DE-PFOOT
           ADD 1 TO T-STMTS
           ADD P-CLOSE TO T-CLOSE
           PERFORM 1700-RECORD-ISSUE.
       1100-EXIT.
           EXIT.

       1150-PRINT-HEAD SECTION.
       1150-START.
           GENERATE DE-PHEAD
           MOVE "Y" TO WS-PRINTED.
       1150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-018  the opening balance has to be known before the
      *> heading is printed, so it is read in one aggregate first
      *>----------------------------------------------------------------
       1180-OPENING-BALANCE SECTION.
       1180-START.
           MOVE 0 TO P-OPEN
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24),ISNULL(SUM("
                  "o.OriginalAmountGBP),0)) FROM ar.OpenItem o JOIN "
                  "ar.Customer c ON c.CompanyCode = o.CompanyCode "
                  "AND c.CustomerCode = o.CustomerCode WHERE "
                  "o.CompanyCode = ? AND ISNULL(NULLIF("
                  "c.StatementCustomerCode,''),c.CustomerCode) = ? "
                  "AND o.PostingDate < ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE P-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE WS-FROM TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               COMPUTE P-OPEN = FUNCTION NUMVAL(BR-COL(1))
           END-IF.
       1180-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-018  opening balance, movements, closing balance.
      *> Every cash and credit movement is itself an open item, so the
      *> balance on any date is simply the sum of the document amounts
      *> posted up to that date; the residual column is reconstructed
      *> as at the statement date for the open item style.
      *>----------------------------------------------------------------
       1200-READ-MOVEMENTS SECTION.
       1200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(char(8),o.PostingDate,112), "
                  "o.ItemType, o.DocumentNumber, CONVERT(char(8),"
                  "o.DueDate,112), o.CustomerCode, o.CurrencyCode, "
                  "CONVERT(varchar(24),o.OriginalAmountGBP), "
                  "CONVERT(varchar(24), o.OriginalAmountGBP - "
                  "ISNULL((SELECT SUM(a.AppliedAmountGBP + "
                  "a.DiscountAmountGBP + a.WriteOffAmountGBP) FROM "
                  "ar.Allocation a WHERE a.TargetOpenItemId = "
                  "o.OpenItemId AND a.Status = 'ACTIVE' AND "
                  "a.EffectiveDate <= ?),0) + ISNULL((SELECT SUM("
                  "b.AppliedAmountGBP) FROM ar.Allocation b WHERE "
                  "b.SourceOpenItemId = o.OpenItemId AND b.Status = "
                  "'ACTIVE' AND b.EffectiveDate <= ?),0)), CASE WHEN "
                  "o.IsDisputed = 1 THEN 'Y' ELSE 'N' END, "
                  "CONVERT(varchar(24),o.OriginalAmount) FROM "
                  "ar.OpenItem o JOIN ar.Customer c ON c.CompanyCode "
                  "= o.CompanyCode AND c.CustomerCode = "
                  "o.CustomerCode WHERE o.CompanyCode = ? AND "
                  "ISNULL(NULLIF(c.StatementCustomerCode,''),"
                  "c.CustomerCode) = ? AND o.PostingDate <= ? ORDER "
                  "BY o.PostingDate, o.DocumentNumber"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-TO TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(1) SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE P-CODE TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(5)
           MOVE "N" TO SQLIO-PARM-NULL(5)
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE 10 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1201 " SQLIO-ERRMSG(1:100)
               GO TO 1200-EXIT
           END-IF
           MOVE "N" TO AR-ERROR
           PERFORM UNTIL AR-ERROR = "Y"
               MOVE 2 TO SQLIO-CURSOR
               MOVE 10 TO SQLIO-COL-CNT
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO AR-ERROR
               ELSE
                   PERFORM 1300-ONE-MOVEMENT
               END-IF
           END-PERFORM
           MOVE "N" TO AR-ERROR
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           COMPUTE P-CLOSE = P-OPEN + P-MOVE-DR + P-MOVE-CR.
       1200-EXIT.
           EXIT.

       1300-ONE-MOVEMENT SECTION.
       1300-START.
           MOVE BR-COL(1) TO AR-COMPACT-DATE
           COMPUTE WS-POST-N = FUNCTION NUMVAL(AR-COMPACT-DATE)
           COMPUTE WS-AMT = FUNCTION NUMVAL(BR-COL(7))
           COMPUTE WS-RESID = FUNCTION NUMVAL(BR-COL(8))
           IF WS-POST-N < WS-FROM
               CONTINUE
           ELSE
               IF WS-AMT < 0
                   ADD WS-AMT TO P-MOVE-CR
               ELSE
                   ADD WS-AMT TO P-MOVE-DR
               END-IF
           END-IF
           PERFORM 1400-AGE-RESIDUAL
           IF WS-STYLE = "TRANS" AND WS-POST-N < WS-FROM
               GO TO 1300-EXIT
           END-IF
           IF WS-STYLE = "OPEN"
               AND WS-RESID > -0.005 AND WS-RESID < 0.005
               GO TO 1300-EXIT
           END-IF
           IF WS-PRINTED = "N"
               PERFORM 1150-PRINT-HEAD
           END-IF
           MOVE WS-POST-N TO AR-NUM-DATE
           PERFORM 8100-FORMAT-DATE
           MOVE WS-UK TO D-DATE
           MOVE BR-COL(2) TO D-TYPE
           MOVE BR-COL(3) TO D-DOC
           MOVE BR-COL(4) TO AR-COMPACT-DATE
           COMPUTE WS-DUE-N = FUNCTION NUMVAL(AR-COMPACT-DATE)
           MOVE WS-DUE-N TO AR-NUM-DATE
           PERFORM 8100-FORMAT-DATE
           MOVE WS-UK TO D-DUE
           MOVE BR-COL(5) TO D-DEBTOR
           MOVE BR-COL(6) TO D-CURR
           MOVE WS-AMT TO D-AMT
           MOVE WS-RESID TO D-OUT
           IF WS-STYLE = "TRANS"
               COMPUTE D-BAL = P-OPEN + P-MOVE-DR + P-MOVE-CR
           ELSE
               MOVE 0 TO D-BAL
           END-IF
           MOVE SPACES TO D-FLAG
           IF BR-COL(9)(1:1) = "Y"
               MOVE "Dis" TO D-FLAG
           END-IF
           IF D-CURR NOT = "GBP"
      *>       foreign currency documents show their own amount in the
      *>       document column; the balance stays in sterling
               COMPUTE D-AMT = FUNCTION NUMVAL(BR-COL(10))
           END-IF
           ADD 1 TO P-LINES WS-ROWS-OUT
           GENERATE DE-ITEM.
       1300-EXIT.
           EXIT.

       1400-AGE-RESIDUAL SECTION.
       1400-START.
           IF WS-RESID < 0.005
               GO TO 1400-EXIT
           END-IF
           MOVE BR-COL(4) TO AR-COMPACT-DATE
           COMPUTE WS-DUE-N = FUNCTION NUMVAL(AR-COMPACT-DATE)
           COMPUTE AR-DAYS = FUNCTION INTEGER-OF-DATE(WS-TO)
                           - FUNCTION INTEGER-OF-DATE(WS-DUE-N)
           EVALUATE TRUE
               WHEN AR-DAYS <= 0   ADD WS-RESID TO P-NOTDUE
               WHEN AR-DAYS <= 30  ADD WS-RESID TO P-B1
               WHEN AR-DAYS <= 60  ADD WS-RESID TO P-B2
               WHEN AR-DAYS <= 90  ADD WS-RESID TO P-B3
               WHEN OTHER          ADD WS-RESID TO P-B4
           END-EVALUATE.
       1400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-018  recorded issue history.  There is no dedicated
      *> statement history table, so the run writes a STATEMENT action
      *> on the customer collection case - the same place the reminder
      *> steps are logged, which keeps one contact history per account.
      *>----------------------------------------------------------------
       1700-RECORD-ISSUE SECTION.
       1700-START.
           IF WS-NO-WRITES
               GO TO 1700-EXIT
           END-IF
           MOVE SPACES TO AR-TEXT2
           STRING "Statement " FUNCTION TRIM(WS-STYLE)
                  " issued to " FUNCTION TRIM(P-CODE)
                  " for the period ending " P-TO-UK
               DELIMITED BY SIZE INTO AR-TEXT2
           MOVE SPACES TO WS-SUP-REASON
           PERFORM 1750-WRITE-ACTION.
       1700-EXIT.
           EXIT.

       1800-SUPPRESS SECTION.
       1800-START.
           ADD 1 TO WS-SUPPRESSED
           GENERATE DE-SUPP
           IF WS-NO-WRITES
               GO TO 1800-EXIT
           END-IF
           MOVE SPACES TO AR-TEXT2
           STRING "Statement not issued - "
                  FUNCTION TRIM(WS-SUP-REASON)
               DELIMITED BY SIZE INTO AR-TEXT2
           PERFORM 1750-WRITE-ACTION.
       1800-EXIT.
           EXIT.

       1750-WRITE-ACTION SECTION.
       1750-START.
      *>   a single INSERT is atomic, and the statement run keeps the
      *>   customer cursor open, so no explicit BEGN/COMT here
           PERFORM 1760-FIND-CASE
           IF WS-CASE-ID = 0
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-1752 no collection case for "
                       FUNCTION TRIM(P-CODE)
               GO TO 1750-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO ar.CollectionAction (CaseId, "
                  "ActionSeq, ActionType, ActionDate, PerformedBy, "
                  "Narrative, SuppressedReason) SELECT ?, "
                  "ISNULL(MAX(ActionSeq),0) + 1, 'STATEMENT', ?, ?, "
                  "?, ? FROM ar.CollectionAction WHERE CaseId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CASE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
                                                 SQLIO-PARM-VAL(6)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(6)
           MOVE WS-TO TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE WS-USER TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE AR-TEXT2 TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           IF WS-SUP-REASON = SPACES
               MOVE SPACES TO SQLIO-PARM-VAL(5)
               MOVE "Y" TO SQLIO-PARM-NULL(5)
           ELSE
               MOVE WS-SUP-REASON TO SQLIO-PARM-VAL(5)
               MOVE "N" TO SQLIO-PARM-NULL(5)
           END-IF
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-1751 issue history not written for "
                       FUNCTION TRIM(P-CODE)
           END-IF
           MOVE 4 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       1750-EXIT.
           EXIT.

       1760-FIND-CASE SECTION.
       1760-START.
           MOVE 0 TO WS-CASE-ID
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 CONVERT(varchar(24),CaseId) FROM "
                  "ar.CollectionCase WHERE CompanyCode = ? AND "
                  "CustomerCode = ? AND CaseType = 'COLLECTION' AND "
                  "Status IN ('OPEN','ESCALATED') ORDER BY CaseId"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE P-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               COMPUTE WS-CASE-ID = FUNCTION NUMVAL(BR-COL(1))
               GO TO 1760-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO ar.CollectionCase (CompanyCode, "
                  "CustomerCode, CaseRef, CaseType, Reason, "
                  "OwnerUser, CollectionStep, Status, CreatedBy) "
                  "VALUES (?, ?, ?, 'COLLECTION', "
                  "'Contact and collection history', ?, 0, 'OPEN', "
                  "?); SELECT CONVERT(varchar(24),SCOPE_IDENTITY())"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE P-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE SPACES TO AR-TEXT
           STRING "COLL-" FUNCTION TRIM(P-CODE)
               DELIMITED BY SIZE INTO AR-TEXT
           MOVE AR-TEXT(1:30) TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE WS-USER TO SQLIO-PARM-VAL(4) SQLIO-PARM-VAL(5)
           MOVE "N" TO SQLIO-PARM-NULL(4) SQLIO-PARM-NULL(5)
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               COMPUTE WS-CASE-ID = FUNCTION NUMVAL(BR-COL(1))
           END-IF.
       1760-EXIT.
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

       8200-APPEND-SQL SECTION.
       8200-START.
           PERFORM VARYING AR-J FROM 3000 BY -1
               UNTIL AR-J < 2 OR SQLIO-STMT(AR-J:1) NOT = SPACE
               CONTINUE
           END-PERFORM
           ADD 1 TO AR-J
           PERFORM VARYING AR-K FROM 200 BY -1
               UNTIL AR-K < 2 OR AR-TEXT(AR-K:1) NOT = SPACE
               CONTINUE
           END-PERFORM
           MOVE AR-TEXT(1:AR-K) TO SQLIO-STMT(AR-J:AR-K)
           MOVE SPACES TO AR-TEXT.
       8200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-STATEMNT
           CLOSE RPT-FILE
           DISPLAY "STERP-I-0401 " T-STMTS " statement(s), "
                   WS-SUPPRESSED " suppressed"
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "STATEMNT" TO JL-JOB-NAME
           MOVE WS-JOBID TO JL-JOB-RUN-ID
           EVALUATE TRUE
               WHEN WS-RC = 0   MOVE "COMPLETE" TO JL-STATUS
               WHEN WS-RC = 4   MOVE "WARNING" TO JL-STATUS
               WHEN OTHER       MOVE "FAILED" TO JL-STATUS
           END-EVALUATE
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE WS-ROWS-OUT TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
