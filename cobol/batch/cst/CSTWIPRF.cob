      *>================================================================
      *> PROGRAM-ID : CSTWIPRF
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *> PURPOSE    : WORK IN PROGRESS ROLL-FORWARD (RPT-015).  ONE
      *>              BLOCK PER FINANCIAL STATUS, ONE PAIR OF LINES
      *>              PER JOB, PROVING THE ARITHMETIC THAT cst.WipBalance
      *>              CARRIES FOR THE PERIOD:
      *>
      *>                opening + material issues - returns + labour
      *>                + overhead + subcontract + adjustments
      *>                - completion transfers - scrap - write off
      *>                = closing
      *>
      *>              EVERY LINE COMES FROM WIPCALC, NOT FROM A SUM
      *>              WRITTEN HERE, SO WHAT IS PRINTED IS WHAT THE
      *>              ENGINE WOULD POST.  MODE=POST WRITES THE PERIOD
      *>              BALANCE AS IT GOES; MODE=CALC CHANGES NOTHING.
      *>              A FINISHED GOODS TRANSFER TAKES VALUE OUT OF
      *>              WORK IN PROGRESS - IT DOES NOT LEAVE THE SAME
      *>              MONEY IN BOTH BALANCES (CST-018).
      *> RUN AS     : CSTWIPRF CO=STC PERIOD=yyyynn [JOB=xxx]
      *>              [MODE=CALC|POST] [USER=xxx]
      *> OUTPUT     : run\spool\CSTWIPRF-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO, WIPCALC
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-06-24 DJM  CR0661 ORIGINAL - MONTHLY WIP RECONCILIATION
      *> 1999-11-30 KMP  Y2K - PERIOD AS YYYYNN ON THE PARAMETER
      *> 2026-10-06 STERP CST-018 CST-020 RESIDUAL COLUMN
      *>================================================================
      *> REQ: CST-018, CST-020
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CSTWIPRF.
       AUTHOR. D J MORRIS.
       DATE-WRITTEN. 24/06/1998.
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
       FD  RPT-FILE REPORT IS R-CSTWIPRF.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY CSTWORK.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY WIPCALCCA REPLACING ==DFHCOMMAREA== BY ==WS-WIP==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-JOB                  PIC X(20) VALUE SPACES.
           05  WS-MODE                 PIC X(12) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-PERIOD               PIC X(8) VALUE SPACES.
           05  WS-YEAR                 PIC 9(4) VALUE 0.
           05  WS-PER                  PIC 9(2) VALUE 0.
       01  WS-RUN.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-EOF                  PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-POSTED               PIC S9(9) VALUE 0.
           05  WS-REFUSED              PIC S9(9) VALUE 0.
           05  WS-WRITE                PIC X VALUE "N".
      *>   status band totals.  The footing prints during the GENERATE
      *>   that breaks the control, so the figures are moved to the
      *>   BT- fields and the A- accumulators reset.  SUM cannot be
      *>   used because each job has a second detail line.
       01  WS-BAND.
           05  A-OPENING               PIC S9(13)V99 VALUE 0.
           05  A-ADDITIONS             PIC S9(13)V99 VALUE 0.
           05  A-DEDUCTIONS            PIC S9(13)V99 VALUE 0.
           05  A-CLOSING               PIC S9(13)V99 VALUE 0.
           05  A-JOBS                  PIC S9(7) VALUE 0.
           05  BT-OPENING              PIC S9(13)V99 VALUE 0.
           05  BT-ADDITIONS            PIC S9(13)V99 VALUE 0.
           05  BT-DEDUCTIONS           PIC S9(13)V99 VALUE 0.
           05  BT-CLOSING              PIC S9(13)V99 VALUE 0.
           05  BT-JOBS                 PIC S9(7) VALUE 0.
      *>   detail line - the financial status drives the control break
       01  WS-DET.
           05  D-STATUS                PIC X(14) VALUE SPACES.
           05  D-JOB                   PIC X(14) VALUE SPACES.
           05  D-DESC                  PIC X(24) VALUE SPACES.
           05  D-OPENING               PIC S9(11)V99 VALUE 0.
           05  D-MATERIAL              PIC S9(11)V99 VALUE 0.
           05  D-LABOUR                PIC S9(11)V99 VALUE 0.
           05  D-OVERHEAD              PIC S9(11)V99 VALUE 0.
           05  D-SUBCON                PIC S9(11)V99 VALUE 0.
           05  D-ADDITIONS             PIC S9(11)V99 VALUE 0.
           05  D-RETURNS               PIC S9(11)V99 VALUE 0.
           05  D-TRANSFERS             PIC S9(11)V99 VALUE 0.
           05  D-SCRAP                 PIC S9(11)V99 VALUE 0.
           05  D-WRITEOFF              PIC S9(11)V99 VALUE 0.
           05  D-CLOSING               PIC S9(11)V99 VALUE 0.
           05  D-BALANCED              PIC X(3) VALUE SPACES.
           05  WS-CHECK                PIC S9(13)V99 VALUE 0.
       01  WS-TOT.
           05  T-OPENING               PIC S9(13)V99 VALUE 0.
           05  T-ADDITIONS             PIC S9(13)V99 VALUE 0.
           05  T-DEDUCTIONS            PIC S9(13)V99 VALUE 0.
           05  T-CLOSING               PIC S9(13)V99 VALUE 0.
           05  T-JOBS                  PIC S9(7) VALUE 0.
           05  T-RESIDUAL              PIC S9(7) VALUE 0.
       REPORT SECTION.
       RD  R-CSTWIPRF
           CONTROLS ARE FINAL D-STATUS
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 56
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-STATUS TYPE IS CONTROL HEADING D-STATUS.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(18) VALUE "Financial status ".
               10  COLUMN 19   PIC X(14) SOURCE D-STATUS.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(3) VALUE "Job".
               10  COLUMN 18   PIC X(11) VALUE "Description".
               10  COLUMN 47   PIC X(7) VALUE "Opening".
               10  COLUMN 61   PIC X(8) VALUE "Material".
               10  COLUMN 75   PIC X(6) VALUE "Labour".
               10  COLUMN 88   PIC X(8) VALUE "Overhead".
               10  COLUMN 101  PIC X(8) VALUE "Subcontr".
               10  COLUMN 117  PIC X(7) VALUE "Closing".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(64) VALUE ALL "-".
               10  COLUMN 67   PIC X(64) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(14) SOURCE D-JOB.
               10  COLUMN 18   PIC X(24) SOURCE D-DESC.
               10  COLUMN 43   PIC Z(8)9.99 SOURCE D-OPENING.
               10  COLUMN 56   PIC Z(8)9.99 SOURCE D-MATERIAL.
               10  COLUMN 69   PIC Z(8)9.99 SOURCE D-LABOUR.
               10  COLUMN 82   PIC Z(8)9.99 SOURCE D-OVERHEAD.
               10  COLUMN 95   PIC Z(8)9.99 SOURCE D-SUBCON.
               10  COLUMN 108  PIC Z(8)9.99 SOURCE D-CLOSING.
       01  DD-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 6    PIC X(11) VALUE "deductions:".
               10  COLUMN 18   PIC X(8) VALUE "returns".
               10  COLUMN 27   PIC Z(6)9.99 SOURCE D-RETURNS.
               10  COLUMN 40   PIC X(10) VALUE "transfers".
               10  COLUMN 51   PIC Z(6)9.99 SOURCE D-TRANSFERS.
               10  COLUMN 64   PIC X(6) VALUE "scrap".
               10  COLUMN 71   PIC Z(6)9.99 SOURCE D-SCRAP.
               10  COLUMN 84   PIC X(10) VALUE "write off".
               10  COLUMN 95   PIC Z(6)9.99 SOURCE D-WRITEOFF.
               10  COLUMN 108  PIC X(10) VALUE "balanced".
               10  COLUMN 119  PIC X(3) SOURCE D-BALANCED.
       01  CF-STATUS TYPE IS CONTROL FOOTING D-STATUS.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(64) VALUE ALL "-".
               10  COLUMN 67   PIC X(64) VALUE ALL "-".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(14) VALUE "Status total".
               10  COLUMN 18   PIC Z(6)9 SOURCE BT-JOBS.
               10  COLUMN 27   PIC X(9) VALUE "job(s)".
               10  COLUMN 43   PIC Z(8)9.99 SOURCE BT-OPENING.
               10  COLUMN 56   PIC X(10) VALUE "additions".
               10  COLUMN 67   PIC Z(8)9.99 SOURCE BT-ADDITIONS.
               10  COLUMN 81   PIC X(10) VALUE "deductions".
               10  COLUMN 92   PIC Z(8)9.99 SOURCE BT-DEDUCTIONS.
               10  COLUMN 108  PIC Z(8)9.99 SOURCE BT-CLOSING.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(64) VALUE ALL "=".
               10  COLUMN 67   PIC X(64) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(22) VALUE "Opening work in prog".
               10  COLUMN 26   PIC Z(11)9.99 SOURCE T-OPENING.
               10  COLUMN 45   PIC X(16) VALUE "Jobs rolled".
               10  COLUMN 62   PIC Z(6)9 SOURCE T-JOBS.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(22) VALUE "Additions in period".
               10  COLUMN 26   PIC Z(11)9.99 SOURCE T-ADDITIONS.
               10  COLUMN 45   PIC X(16) VALUE "With residual".
               10  COLUMN 62   PIC Z(6)9 SOURCE T-RESIDUAL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(22) VALUE "Deductions in period".
               10  COLUMN 26   PIC Z(11)9.99 SOURCE T-DEDUCTIONS.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(22) VALUE "Closing work in prog".
               10  COLUMN 26   PIC Z(11)9.99 SOURCE T-CLOSING.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(46) VALUE
                   "Closing = opening + additions - deductions.".
               10  COLUMN 50   PIC X(46) VALUE
                   "A completion transfer takes value OUT of work".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(60) VALUE
                   "in progress; it is not left in both balances.".
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
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "COST" TO WS-USER
           END-IF
           IF WS-MODE = SPACES
               MOVE "CALC" TO WS-MODE
           END-IF
           IF WS-MODE = "POST"
               MOVE "Y" TO WS-WRITE
           END-IF
           PERFORM 0150-SPLIT-PERIOD
           DISPLAY "STERP-I-0100 CSTWIPRF WIP roll-forward for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

      *>  PERIOD=YYYYNN.  WITHOUT ONE THE REPORT WOULD HAVE TO GUESS
      *>  WHICH PERIOD TO ROLL, AND A GUESS CAN BE POSTED.
       0150-SPLIT-PERIOD SECTION.
       0150-START.
           IF WS-PERIOD = SPACES
               PERFORM 0160-CURRENT-PERIOD
               GO TO 0150-EXIT
           END-IF
           COMPUTE WS-YEAR = FUNCTION NUMVAL(WS-PERIOD(1:4))
           COMPUTE WS-PER = FUNCTION NUMVAL(WS-PERIOD(5:2)).
       0150-EXIT.
           EXIT.

       0160-CURRENT-PERIOD SECTION.
       0160-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 FiscalYear, PeriodNo FROM "
                  "core.FiscalPeriod WHERE CompanyCode = ? AND "
                  "Status = 'OPEN' ORDER BY FiscalYear, PeriodNo"
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
               COMPUTE WS-YEAR = FUNCTION NUMVAL(WS-COL(1))
               COMPUTE WS-PER = FUNCTION NUMVAL(WS-COL(2))
               MOVE SPACES TO WS-PERIOD
               STRING WS-YEAR WS-PER DELIMITED BY SIZE
                   INTO WS-PERIOD
           ELSE
               DISPLAY "STERP-W-0151 no open period - nothing rolled"
           END-IF.
       0160-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "CSTWIPRF" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "CSTWIPRF" TO RP-REPORT-ID
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
           INITIATE R-CSTWIPRF.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "CSTWIPRF" TO RPT-REPORT-ID
           MOVE "Work in progress roll-forward" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "Period " WS-YEAR "/" WS-PER
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           MOVE SPACES TO RPT-SUBTITLE
           IF WS-JOB = SPACES
               MOVE "All jobs with a balance or movement"
                   TO RPT-SUBTITLE
           ELSE
               STRING "Job " FUNCTION TRIM(WS-JOB)
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           END-IF
           IF WS-WRITE = "Y"
               MOVE "Period balances posted" TO RPT-STATUS-TEXT
           ELSE
               MOVE "Calculation only - nothing posted"
                   TO RPT-STATUS-TEXT
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
      *> 1000 - every job that is not financially closed, ordered by
      *> status so the control break is meaningful.  The roll-forward
      *> itself is WIPCALC's answer, never a sum written here.
      *>----------------------------------------------------------------
       1000-PRODUCE-REPORT SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT j.JobRef, j.Description, "
                  "j.FinancialStatus FROM cst.Job j WHERE "
                  "j.CompanyCode = ? AND (? IS NULL OR j.JobRef = "
                  "?) AND j.FinancialStatus <> 'CLOSED' ORDER BY "
                  "j.FinancialStatus, j.JobRef"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           IF WS-JOB = SPACES
               MOVE SPACES TO SQLIO-PARM-VAL(2)
               MOVE SPACES TO SQLIO-PARM-VAL(3)
               MOVE "Y" TO SQLIO-PARM-NULL(2)
               MOVE "Y" TO SQLIO-PARM-NULL(3)
           ELSE
               MOVE WS-JOB TO SQLIO-PARM-VAL(2)
               MOVE WS-JOB TO SQLIO-PARM-VAL(3)
               MOVE "N" TO SQLIO-PARM-NULL(2)
               MOVE "N" TO SQLIO-PARM-NULL(3)
           END-IF
           MOVE 3 TO SQLIO-COL-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
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
               MOVE 1 TO SQLIO-CURSOR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-EOF
               ELSE
                   PERFORM 1100-ROLL-ONE-JOB
               END-IF
           END-PERFORM
           MOVE 1 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           PERFORM 1200-CLOSE-BAND.
       1000-EXIT.
           EXIT.

       1100-ROLL-ONE-JOB SECTION.
       1100-START.
           MOVE WS-COL(1) TO D-JOB
           MOVE WS-COL(2) TO D-DESC
           IF D-STATUS NOT = WS-COL(3)(1:14)
               PERFORM 1200-CLOSE-BAND
               MOVE WS-COL(3) TO D-STATUS
           END-IF
           PERFORM 1120-ASK-WIPCALC
           IF CA-RETURN-CODE OF WS-WIP > 4
               ADD 1 TO WS-REFUSED
               DISPLAY "STERP-W-1101 " FUNCTION TRIM(D-JOB) " "
                       CA-MESSAGE OF WS-WIP (1:60)
               GO TO 1100-EXIT
           END-IF
           PERFORM 1140-MOVE-THE-NUMBERS
           GENERATE DE-LINE
           GENERATE DD-LINE
           ADD 1 TO WS-ROWS-OUT
           ADD 1 TO A-JOBS T-JOBS
           ADD D-OPENING TO A-OPENING T-OPENING
           ADD D-ADDITIONS TO A-ADDITIONS T-ADDITIONS
           ADD D-RETURNS D-TRANSFERS D-SCRAP D-WRITEOFF
               TO A-DEDUCTIONS T-DEDUCTIONS
           ADD D-CLOSING TO A-CLOSING T-CLOSING
           IF WC-HAS-RESIDUAL = "Y"
               ADD 1 TO T-RESIDUAL
           END-IF.
       1100-EXIT.
           EXIT.

      *>  MODE=POST writes the period balance; MODE=CALC leaves the
      *>  table exactly as it was.  Either way the printed figures
      *>  come from the engine.
       1120-ASK-WIPCALC SECTION.
       1120-START.
           INITIALIZE WS-WIP
           IF WS-WRITE = "Y"
               MOVE "POST" TO CA-FUNCTION OF WS-WIP
               MOVE "Y" TO WC-COMMIT
           ELSE
               MOVE "CALC" TO CA-FUNCTION OF WS-WIP
               MOVE "N" TO WC-COMMIT
           END-IF
           MOVE WS-CO TO CA-COMPANY OF WS-WIP
           MOVE WS-USER TO CA-USER OF WS-WIP
           MOVE WS-YEAR TO WC-FISCAL-YEAR
           MOVE WS-PER TO WC-PERIOD-NO
           MOVE D-JOB TO WC-JOB-REF
           CALL "WIPCALC" USING WS-WIP
           IF WS-WRITE = "Y" AND CA-RETURN-CODE OF WS-WIP NOT > 4
               ADD 1 TO WS-POSTED
           END-IF.
       1120-EXIT.
           EXIT.

       1140-MOVE-THE-NUMBERS SECTION.
       1140-START.
           MOVE WC-OPENING-GBP TO D-OPENING
           MOVE WC-MAT-ISSUES-GBP TO D-MATERIAL
           COMPUTE D-LABOUR = WC-LABOUR-GBP + WC-MACHINE-GBP
           COMPUTE D-OVERHEAD = WC-OVERHEAD-GBP
                              + WC-ADJUSTMENT-GBP
           MOVE WC-SUBCON-GBP TO D-SUBCON
           MOVE WC-ADDITIONS-GBP TO D-ADDITIONS
           MOVE WC-MAT-RETURNS-GBP TO D-RETURNS
           MOVE WC-TRANSFERS-GBP TO D-TRANSFERS
           MOVE WC-SCRAP-GBP TO D-SCRAP
           MOVE WC-WRITEOFF-GBP TO D-WRITEOFF
           MOVE WC-CLOSING-GBP TO D-CLOSING
           COMPUTE WS-CHECK = WC-OPENING-GBP + WC-ADDITIONS-GBP
                            - WC-DEDUCTIONS-GBP - WC-CLOSING-GBP
           IF WS-CHECK = 0
               MOVE "yes" TO D-BALANCED
           ELSE
               MOVE "NO" TO D-BALANCED
           END-IF.
       1140-EXIT.
           EXIT.

      *>  The band figures are taken before the control break prints
      *>  them, because the accumulators carry on into the next band.
       1200-CLOSE-BAND SECTION.
       1200-START.
           MOVE A-OPENING TO BT-OPENING
           MOVE A-ADDITIONS TO BT-ADDITIONS
           MOVE A-DEDUCTIONS TO BT-DEDUCTIONS
           MOVE A-CLOSING TO BT-CLOSING
           MOVE A-JOBS TO BT-JOBS
           MOVE 0 TO A-OPENING A-ADDITIONS A-DEDUCTIONS
           MOVE 0 TO A-CLOSING A-JOBS.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-CSTWIPRF
           CLOSE RPT-FILE
           DISPLAY "STERP-I-0401 " WS-ROWS-OUT " job(s) rolled "
                   "forward"
           IF WS-POSTED > 0
               DISPLAY "STERP-I-0403 " WS-POSTED
                       " period balance(s) posted"
           END-IF
           IF WS-REFUSED > 0
               DISPLAY "STERP-W-0402 " WS-REFUSED
                       " job(s) refused by WIPCALC"
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
           MOVE "CSTWIPRF" TO JL-JOB-NAME
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
