      *>================================================================
      *> PROGRAM-ID : CSTJOBPR
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *> PURPOSE    : JOB PROFITABILITY AND WORK IN PROGRESS REPORT
      *>              (RPT-014).  ONE BLOCK PER PARENT JOB, WITH ITS
      *>              SUB JOBS INDENTED UNDERNEATH, SHOWING APPROVED
      *>              BUDGET, ACTUAL, COMMITTED, ACCRUED, ESTIMATE AT
      *>              COMPLETION, REVENUE, MARGIN AND CLOSING WIP.
      *>              A JOB WITH NO REVENUE PRINTS "n/a" FOR MARGIN -
      *>              IT IS NOT A HUNDRED PER CENT LOSS (CST-020).
      *> RUN AS     : CSTJOBPR CO=STC [PERIOD=yyyynn] [JOB=xxx]
      *>              [STATUS=OPEN|CLOSED] [CUSTOMER=xxx]
      *>              [MODE=ALL|OVERSPEND] [USER=xxx]
      *> OUTPUT     : run\spool\CSTJOBPR-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *>              cst.usp_JobProfitability
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-02-17 RJH  CR0566 ORIGINAL - CONTRACT COSTING SUMMARY
      *> 1999-11-30 KMP  Y2K - PERIOD AS YYYYNN ON THE PARAMETER
      *> 2004-11-08 DJM  CR1263 SUB JOBS INDENTED AFTER THE DOUBLE
      *>                 COUNTING COMPLAINT FROM THE CONTRACTS DESK
      *> 2026-10-06 STERP CST-016 ESTIMATE AT COMPLETION COLUMN
      *>================================================================
      *> REQ: CST-014, CST-015, CST-016, CST-017, CST-018, CST-020
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CSTJOBPR.
       AUTHOR. R J HOLLIS.
       DATE-WRITTEN. 17/02/1998.
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
       FD  RPT-FILE REPORT IS R-CSTJOBPR.
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
           05  WS-JOB                  PIC X(20) VALUE SPACES.
           05  WS-STATUS               PIC X(12) VALUE SPACES.
           05  WS-CUST                 PIC X(20) VALUE SPACES.
           05  WS-MODE                 PIC X(12) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-PERIOD               PIC X(8) VALUE SPACES.
       01  WS-RUN.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-EOF                  PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-SKIP                 PIC X VALUE "N".
               88  WS-SKIP-ROW                   VALUE "Y".
           05  WS-NEW-KEY              PIC X(20) VALUE SPACES.
      *>   contract totals.  The footing is printed by the report
      *>   writer during the GENERATE that breaks the control, so the
      *>   figures are moved here first and the accumulators reset -
      *>   SUM cannot be used when a group has a second detail line.
       01  WS-BAND.
           05  A-BUDGET                PIC S9(13)V99 VALUE 0.
           05  A-ACTUAL                PIC S9(13)V99 VALUE 0.
           05  A-COMMIT                PIC S9(13)V99 VALUE 0.
           05  A-FORECAST              PIC S9(13)V99 VALUE 0.
           05  A-REVENUE               PIC S9(13)V99 VALUE 0.
           05  BT-BUDGET               PIC S9(13)V99 VALUE 0.
           05  BT-ACTUAL               PIC S9(13)V99 VALUE 0.
           05  BT-COMMIT               PIC S9(13)V99 VALUE 0.
           05  BT-FORECAST             PIC S9(13)V99 VALUE 0.
           05  BT-REVENUE              PIC S9(13)V99 VALUE 0.
      *>   detail line - the parent job drives the control break
       01  WS-DET.
           05  D-ROOT                  PIC X(20) VALUE SPACES.
           05  D-JOB                   PIC X(20) VALUE SPACES.
           05  D-LEVEL                 PIC S9(4) VALUE 0.
           05  D-PRINT-REF             PIC X(24) VALUE SPACES.
           05  D-DESC                  PIC X(26) VALUE SPACES.
           05  D-CUST                  PIC X(12) VALUE SPACES.
           05  D-OPS                   PIC X(10) VALUE SPACES.
           05  D-FIN                   PIC X(10) VALUE SPACES.
           05  D-BUDGET                PIC S9(11)V99 VALUE 0.
           05  D-ACTUAL                PIC S9(11)V99 VALUE 0.
           05  D-COMMIT                PIC S9(11)V99 VALUE 0.
           05  D-ACCRUE                PIC S9(11)V99 VALUE 0.
           05  D-FORECAST              PIC S9(11)V99 VALUE 0.
           05  D-VARIANCE              PIC S9(11)V99 VALUE 0.
           05  D-REVENUE               PIC S9(11)V99 VALUE 0.
           05  D-MARGIN                PIC S9(11)V99 VALUE 0.
           05  D-WIP                   PIC S9(11)V99 VALUE 0.
           05  D-MARGIN-TXT            PIC X(10) VALUE SPACES.
           05  D-NOTE                  PIC X(28) VALUE SPACES.
       01  WS-TOT.
           05  T-BUDGET                PIC S9(13)V99 VALUE 0.
           05  T-ACTUAL                PIC S9(13)V99 VALUE 0.
           05  T-COMMIT                PIC S9(13)V99 VALUE 0.
           05  T-FORECAST              PIC S9(13)V99 VALUE 0.
           05  T-REVENUE               PIC S9(13)V99 VALUE 0.
           05  T-MARGIN                PIC S9(13)V99 VALUE 0.
           05  T-WIP                   PIC S9(13)V99 VALUE 0.
           05  T-JOBS                  PIC S9(7) VALUE 0.
           05  T-OVERSPEND             PIC S9(7) VALUE 0.
           05  T-NO-BUDGET             PIC S9(7) VALUE 0.
           05  T-PCT                   PIC S9(5)V99 VALUE 0.
       REPORT SECTION.
       RD  R-CSTJOBPR
           CONTROLS ARE FINAL D-ROOT
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 56
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-ROOT TYPE IS CONTROL HEADING D-ROOT.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(9) VALUE "Contract ".
               10  COLUMN 10   PIC X(20) SOURCE D-ROOT.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(3) VALUE "Job".
               10  COLUMN 28   PIC X(11) VALUE "Description".
               10  COLUMN 55   PIC X(8) VALUE "Customer".
               10  COLUMN 68   PIC X(6) VALUE "Budget".
               10  COLUMN 81   PIC X(6) VALUE "Actual".
               10  COLUMN 93   PIC X(9) VALUE "Committed".
               10  COLUMN 106  PIC X(3) VALUE "EAC".
               10  COLUMN 119  PIC X(7) VALUE "Revenue".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(64) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(24) SOURCE D-PRINT-REF.
               10  COLUMN 28   PIC X(26) SOURCE D-DESC.
               10  COLUMN 55   PIC X(12) SOURCE D-CUST.
               10  COLUMN 65   PIC Z(8)9.99 SOURCE D-BUDGET.
               10  COLUMN 78   PIC Z(8)9.99 SOURCE D-ACTUAL.
               10  COLUMN 91   PIC Z(8)9.99 SOURCE D-COMMIT.
               10  COLUMN 104  PIC Z(8)9.99 SOURCE D-FORECAST.
               10  COLUMN 117  PIC Z(8)9.99 SOURCE D-REVENUE.
       01  DS-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 8    PIC X(10) VALUE "status".
               10  COLUMN 18   PIC X(10) SOURCE D-OPS.
               10  COLUMN 29   PIC X(10) SOURCE D-FIN.
               10  COLUMN 41   PIC X(7) VALUE "margin".
               10  COLUMN 48   PIC Z(8)9.99- SOURCE D-MARGIN.
               10  COLUMN 62   PIC X(10) SOURCE D-MARGIN-TXT.
               10  COLUMN 74   PIC X(4) VALUE "WIP".
               10  COLUMN 78   PIC Z(8)9.99 SOURCE D-WIP.
               10  COLUMN 93   PIC X(28) SOURCE D-NOTE.
       01  CF-ROOT TYPE IS CONTROL FOOTING D-ROOT.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(20) VALUE "Contract total".
               10  COLUMN 65   PIC Z(8)9.99 SOURCE BT-BUDGET.
               10  COLUMN 78   PIC Z(8)9.99 SOURCE BT-ACTUAL.
               10  COLUMN 91   PIC Z(8)9.99 SOURCE BT-COMMIT.
               10  COLUMN 104  PIC Z(8)9.99 SOURCE BT-FORECAST.
               10  COLUMN 117  PIC Z(8)9.99 SOURCE BT-REVENUE.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(22) VALUE "Report totals".
               10  COLUMN 65   PIC Z(8)9.99 SOURCE T-BUDGET.
               10  COLUMN 78   PIC Z(8)9.99 SOURCE T-ACTUAL.
               10  COLUMN 91   PIC Z(8)9.99 SOURCE T-COMMIT.
               10  COLUMN 104  PIC Z(8)9.99 SOURCE T-FORECAST.
               10  COLUMN 117  PIC Z(8)9.99 SOURCE T-REVENUE.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(16) VALUE "Jobs reported:".
               10  COLUMN 20   PIC Z(6)9 SOURCE T-JOBS.
               10  COLUMN 32   PIC X(28) VALUE
                   "forecast over budget:".
               10  COLUMN 60   PIC Z(6)9 SOURCE T-OVERSPEND.
               10  COLUMN 72   PIC X(26) VALUE
                   "without an approved budget:".
               10  COLUMN 99   PIC Z(6)9 SOURCE T-NO-BUDGET.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(22) VALUE "Closing WIP carried".
               10  COLUMN 26   PIC Z(11)9.99 SOURCE T-WIP.
               10  COLUMN 45   PIC X(16) VALUE "Total margin".
               10  COLUMN 62   PIC Z(11)9.99- SOURCE T-MARGIN.
               10  COLUMN 82   PIC X(9) VALUE "Margin %".
               10  COLUMN 92   PIC Z(4)9.99- SOURCE T-PCT.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(62) VALUE
                   "EAC is actual plus committed plus accrued cost.".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(62) VALUE
                   "Sub job cost belongs to its parent once only.".
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
           PERFORM 1800-FINAL-MARGIN
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
           MOVE CM-PERIOD TO WS-PERIOD
           MOVE "JOB" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-JOB
           END-IF
           MOVE "STATUS" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-STATUS
           END-IF
           MOVE "CUSTOMER" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-CUST
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
           DISPLAY "STERP-I-0100 CSTJOBPR job profitability for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "CSTJOBPR" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "CSTJOBPR" TO RP-REPORT-ID
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
           INITIATE R-CSTJOBPR.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "CSTJOBPR" TO RPT-REPORT-ID
           MOVE "Job profitability and work in progress"
               TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE SPACES TO RPT-AS-AT-TEXT
           IF WS-PERIOD = SPACES
               MOVE "All open periods" TO RPT-AS-AT-TEXT
           ELSE
               STRING "Period " FUNCTION TRIM(WS-PERIOD)
                   DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           END-IF
           MOVE SPACES TO RPT-SUBTITLE
           IF WS-CUST = SPACES
               STRING "All customers" DELIMITED BY SIZE
                   INTO RPT-SUBTITLE
           ELSE
               STRING "Customer " FUNCTION TRIM(WS-CUST)
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           END-IF
           IF WS-MODE = "OVERSPEND"
               MOVE "Overspend only" TO RPT-STATUS-TEXT
           ELSE
               MOVE "All jobs in scope" TO RPT-STATUS-TEXT
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
      *> 1000 - the enquiry proc already roots sub jobs under their
      *> parent and orders them, so the report only has to format and
      *> accumulate.  Status and customer selection stay here because
      *> the proc is shared with the screen enquiry.
      *>----------------------------------------------------------------
       1000-PRODUCE-REPORT SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC cst.usp_JobProfitability ?, ?, 1"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           IF WS-JOB = SPACES
               MOVE SPACES TO SQLIO-PARM-VAL(2)
               MOVE "Y" TO SQLIO-PARM-NULL(2)
           ELSE
               MOVE WS-JOB TO SQLIO-PARM-VAL(2)
               MOVE "N" TO SQLIO-PARM-NULL(2)
           END-IF
           MOVE 18 TO SQLIO-COL-CNT
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
                   PERFORM 1100-EMIT-JOB
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW.
       1000-EXIT.
           EXIT.

       1100-EMIT-JOB SECTION.
       1100-START.
           MOVE "N" TO WS-SKIP
           MOVE WS-COL(1) TO D-JOB
           COMPUTE D-LEVEL = FUNCTION NUMVAL(WS-COL(3))
           IF WS-COL(2) = SPACES
               MOVE WS-COL(1) TO WS-NEW-KEY
           ELSE
               MOVE WS-COL(2) TO WS-NEW-KEY
           END-IF
           MOVE WS-COL(4) TO D-DESC
           MOVE WS-COL(5) TO D-CUST
           MOVE WS-COL(6) TO D-OPS
           MOVE WS-COL(7) TO D-FIN
           COMPUTE D-BUDGET = FUNCTION NUMVAL(WS-COL(8))
           COMPUTE D-ACTUAL = FUNCTION NUMVAL(WS-COL(9))
           COMPUTE D-COMMIT = FUNCTION NUMVAL(WS-COL(10))
           COMPUTE D-ACCRUE = FUNCTION NUMVAL(WS-COL(11))
           COMPUTE D-REVENUE = FUNCTION NUMVAL(WS-COL(12))
           COMPUTE D-WIP = FUNCTION NUMVAL(WS-COL(13))
           COMPUTE D-FORECAST = FUNCTION NUMVAL(WS-COL(14))
           COMPUTE D-VARIANCE = FUNCTION NUMVAL(WS-COL(15))
           COMPUTE D-MARGIN = FUNCTION NUMVAL(WS-COL(16))
           PERFORM 1150-APPLY-SELECTION
           IF WS-SKIP-ROW
               GO TO 1100-EXIT
           END-IF
      *>   a sub job is shown indented under its parent so that nobody
      *>   adds the two together by eye
           MOVE SPACES TO D-PRINT-REF
           IF D-LEVEL > 0
               STRING "  " FUNCTION TRIM(D-JOB)
                   DELIMITED BY SIZE INTO D-PRINT-REF
           ELSE
               MOVE D-JOB TO D-PRINT-REF
           END-IF
           IF WS-NEW-KEY NOT = D-ROOT AND D-ROOT NOT = SPACES
               PERFORM 1160-CLOSE-CONTRACT
           END-IF
           MOVE WS-NEW-KEY TO D-ROOT
           MOVE SPACES TO D-MARGIN-TXT
           IF WS-COL(17)(1:9) = "UNDEFINED"
               MOVE "n/a" TO D-MARGIN-TXT
           ELSE
               COMPUTE WS-CALC-1 = FUNCTION NUMVAL(WS-COL(18))
               MOVE WS-CALC-1 TO WS-PCT-ED
               MOVE WS-PCT-ED TO D-MARGIN-TXT
           END-IF
           MOVE SPACES TO D-NOTE
           IF D-BUDGET = 0
               MOVE "no approved budget" TO D-NOTE
               ADD 1 TO T-NO-BUDGET
           ELSE
               IF D-FORECAST > D-BUDGET
                   MOVE "*** forecast over budget" TO D-NOTE
                   ADD 1 TO T-OVERSPEND
               END-IF
           END-IF
           ADD D-BUDGET TO T-BUDGET A-BUDGET
           ADD D-ACTUAL TO T-ACTUAL A-ACTUAL
           ADD D-COMMIT TO T-COMMIT A-COMMIT
           ADD D-FORECAST TO T-FORECAST A-FORECAST
           ADD D-REVENUE TO T-REVENUE A-REVENUE
           ADD D-MARGIN TO T-MARGIN
           ADD D-WIP TO T-WIP
           ADD 1 TO T-JOBS WS-ROWS-OUT
           GENERATE DE-LINE
           GENERATE DS-LINE.
       1100-EXIT.
           EXIT.

       1160-CLOSE-CONTRACT SECTION.
       1160-START.
           MOVE A-BUDGET TO BT-BUDGET
           MOVE A-ACTUAL TO BT-ACTUAL
           MOVE A-COMMIT TO BT-COMMIT
           MOVE A-FORECAST TO BT-FORECAST
           MOVE A-REVENUE TO BT-REVENUE
           MOVE 0 TO A-BUDGET A-ACTUAL A-COMMIT A-FORECAST
                     A-REVENUE.
       1160-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1150 - selection that the shared proc does not do.  OVERSPEND
      *> mode keeps a job only when the estimate at completion has
      *> already passed the approved budget (CST-016).
      *>----------------------------------------------------------------
       1150-APPLY-SELECTION SECTION.
       1150-START.
           IF WS-CUST NOT = SPACES
               IF FUNCTION TRIM(D-CUST) NOT = FUNCTION TRIM(WS-CUST)
                   MOVE "Y" TO WS-SKIP
                   GO TO 1150-EXIT
               END-IF
           END-IF
           IF WS-STATUS NOT = SPACES
               IF FUNCTION TRIM(D-OPS) NOT = FUNCTION TRIM(WS-STATUS)
                   MOVE "Y" TO WS-SKIP
                   GO TO 1150-EXIT
               END-IF
           END-IF
           IF WS-MODE = "OVERSPEND"
               IF D-BUDGET = 0 OR D-FORECAST NOT > D-BUDGET
                   MOVE "Y" TO WS-SKIP
               END-IF
           END-IF.
       1150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1800 - overall margin percentage.  Division by a zero revenue
      *> is a reporting decision, not an arithmetic one: leave it at
      *> zero and let the "n/a" lines explain why.
      *>----------------------------------------------------------------
       1800-FINAL-MARGIN SECTION.
       1800-START.
           MOVE 0 TO T-PCT
           IF T-REVENUE NOT = 0
               COMPUTE T-PCT ROUNDED = (T-MARGIN * 100) / T-REVENUE
           END-IF.
       1800-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           PERFORM 1160-CLOSE-CONTRACT
           TERMINATE R-CSTJOBPR
           CLOSE RPT-FILE
           DISPLAY "STERP-I-0401 " WS-ROWS-OUT " job(s) reported"
           IF T-OVERSPEND > 0
               DISPLAY "STERP-W-0402 " T-OVERSPEND
                       " job(s) forecast over budget"
           END-IF
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "CSTJOBPR" TO JL-JOB-NAME
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
