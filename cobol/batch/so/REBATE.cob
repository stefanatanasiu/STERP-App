      *>================================================================
      *> PROGRAM-ID : REBATE
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT
      *> PURPOSE    : RETROSPECTIVE REBATE AND VOLUME DISCOUNT ACCRUAL
      *>              AND SETTLEMENT.  FOR EVERY ACTIVE
      *>              so.RebateAgreement WHOSE PERIOD COVERS THE RUN
      *>              PERIOD THE QUALIFYING NET REVENUE AND QUANTITY
      *>              ARE TOTALLED FROM THE POSTED SALES INVOICES OF
      *>              THAT CUSTOMER (OPTIONALLY NARROWED TO AN ITEM OR
      *>              A PRODUCT GROUP), THE THRESHOLD AND PERCENTAGE
      *>              ARE APPLIED AND so.RebateAccrual IS WRITTEN OR
      *>              REFRESHED FOR THE FISCAL YEAR AND PERIOD.
      *>              AN ACCRUED REBATE IS VARIABLE CONSIDERATION AND
      *>              THEREFORE A REDUCTION OF REVENUE, NEVER A COST:
      *>              THE JOURNAL IS STAGED IN fin.JournalStage AND
      *>              POSTED BY fin.usp_PostJournal UNDER THE BUSINESS
      *>              EVENT SO.REBATE.ACCRUAL, DEBITING THE SALES
      *>              REBATE ACCOUNT AND CREDITING THE ACCRUED REBATE
      *>              LIABILITY.
      *>              SETTLEMENT ISSUES A CREDIT NOTE THROUGH ARCRNOTE
      *>              WITH THE VAT OF THE SUPPLIES IT RELATES TO AND
      *>              THEN RELEASES THE ACCRUAL, SO THAT PAYING THE
      *>              REBATE DOES NOT REDUCE REVENUE A SECOND TIME.
      *>              WHAT HAS ALREADY BEEN SETTLED IS ALWAYS DEDUCTED
      *>              (SettledGBP, SettlementCreditNote).
      *> RUN AS     : REBATE CO=STC USER=FINCTRL [PERIOD=202607]
      *>              [REBATE=RB001] [FUNCTION=ACCRUE|SETTLE]
      *>              [MODE=PREVIEW|COMMIT|SETTLE]
      *> OUTPUT     : run\spool\REBATE-<CO>-<stamp>.lst  (132 col)
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, RNDALLOC, ARCRNOTE,
      *>              AUDITLOG, SQLIO, fin.usp_PostJournal
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-02-24 KMP  CR0688 ORIGINAL - ANNUAL VOLUME REBATES
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS THROUGHOUT
      *> 2006-10-02 RJH  CR2001 ACCRUAL MOVED OFF COST OF SALES ONTO
      *>                 TURNOVER AFTER THE AUDIT POINT ON FRS 5
      *> 2016-04-04 AMD  CR2770 SETTLEMENT NO LONGER DOUBLE COUNTS -
      *>                 THE CREDIT NOTE RELEASES THE ACCRUAL
      *> 2026-10-06 STERP SO-027, SO-028 FRS 102 S23 RESTATEMENT
      *>================================================================
      *> REQ: SO-027, SO-028
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REBATE.
       AUTHOR. K M PARRY.
       DATE-WRITTEN. 24/02/1997.
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
       FD  RPT-FILE REPORT IS R-REBATE.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY RNDALLOCCA REPLACING ==DFHCOMMAREA== BY ==WS-ROUND==.
       COPY ARCRNOTECA REPLACING ==DFHCOMMAREA== BY ==WS-CRNOTE==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUDIT==.
       01  SO-ROW.
           05  SO-COL OCCURS 24 TIMES PIC X(100).
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-PERIOD               PIC X(6) VALUE SPACES.
           05  WS-REBATE-REF           PIC X(30) VALUE SPACES.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
           05  WS-FUNC                 PIC X(8) VALUE SPACES.
               88  WS-DO-SETTLE                  VALUE "SETTLE".
               88  WS-DO-ACCRUE                  VALUE "ACCRUE".
           05  WS-COMMIT                PIC X VALUE "N".
       01  WS-SCRATCH.
           05  WS-IX                   PIC S9(4) VALUE 0.
           05  WS-JX                   PIC S9(4) VALUE 0.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-TODAY-N              PIC 9(8) VALUE 0.
           05  WS-DONE                 PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-AMT                  PIC S9(13)V9(6) VALUE 0.
           05  WS-AMT2                 PIC S9(13)V9(6) VALUE 0.
           05  WS-CORREL               PIC X(36) VALUE SPACES.
           05  WS-JOB-RUN-ID           PIC S9(9) COMP-5 VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-INT-ED               PIC -(9)9.
           05  WS-MONEY-ED             PIC -(11)9.99.
           05  WS-NOTE                 PIC X(20) VALUE SPACES.
       01  WS-PER.
           05  P-FY                    PIC 9(4) VALUE 0.
           05  P-PP                    PIC 9(2) VALUE 0.
           05  P-START                 PIC X(8) VALUE SPACES.
           05  P-END                   PIC X(8) VALUE SPACES.
           05  P-STATUS                PIC X(10) VALUE SPACES.
           05  P-YEAR-START            PIC X(8) VALUE SPACES.
      *> one rebate agreement under assessment
       01  WS-AGR.
           05  A-ID                    PIC 9(9) VALUE 0.
           05  A-REF                   PIC X(30) VALUE SPACES.
           05  A-CUST                  PIC X(20) VALUE SPACES.
           05  A-ITEM                  PIC X(40) VALUE SPACES.
           05  A-GROUP                 PIC X(20) VALUE SPACES.
           05  A-BASIS                 PIC X(12) VALUE SPACES.
           05  A-THRESHOLD             PIC S9(13)V9(6) VALUE 0.
           05  A-PCT                   PIC S9(5)V9(4) VALUE 0.
           05  A-PER-UNIT              PIC S9(13)V9(6) VALUE 0.
           05  A-FROM                  PIC X(8) VALUE SPACES.
           05  A-TO                    PIC X(8) VALUE SPACES.
           05  A-CURR                  PIC X(3) VALUE SPACES.
           05  A-STATUS                PIC X(12) VALUE SPACES.
           05  A-CRNOTE                PIC X(30) VALUE SPACES.
      *> measured values for the agreement in this fiscal year
       01  WS-MEAS.
           05  M-REVENUE               PIC S9(13)V99 VALUE 0.
           05  M-QTY                   PIC S9(13)V9(3) VALUE 0.
           05  M-INV-COUNT             PIC S9(7) VALUE 0.
           05  M-INV-REFS              PIC X(400) VALUE SPACES.
           05  M-VAT-CODE              PIC X(4) VALUE SPACES.
           05  M-VAT-RATE              PIC S9(3)V9(4) VALUE 0.
           05  M-REQUIRED              PIC S9(13)V99 VALUE 0.
           05  M-PRIOR                 PIC S9(13)V99 VALUE 0.
           05  M-THIS-PERIOD           PIC S9(13)V99 VALUE 0.
           05  M-MOVEMENT              PIC S9(13)V99 VALUE 0.
           05  M-SETTLED               PIC S9(13)V99 VALUE 0.
           05  M-TO-SETTLE             PIC S9(13)V99 VALUE 0.
           05  M-SETTLE-VAT            PIC S9(13)V99 VALUE 0.
       01  WS-DET.
           05  D-REF                   PIC X(14) VALUE SPACES.
           05  D-CUST                  PIC X(12) VALUE SPACES.
           05  D-SCOPE                 PIC X(16) VALUE SPACES.
           05  D-BASIS                 PIC X(8) VALUE SPACES.
           05  D-THRESHOLD             PIC S9(11)V99 VALUE 0.
           05  D-PCT                   PIC S9(3)V9(4) VALUE 0.
           05  D-REVENUE               PIC S9(13)V99 VALUE 0.
           05  D-ACCRUED               PIC S9(11)V99 VALUE 0.
           05  D-MOVEMENT              PIC S9(11)V99 VALUE 0.
           05  D-NOTE                  PIC X(20) VALUE SPACES.
       01  WS-TOT.
           05  T-AGREEMENTS            PIC S9(7) VALUE 0.
           05  T-REVENUE               PIC S9(13)V99 VALUE 0.
           05  T-ACCRUED               PIC S9(13)V99 VALUE 0.
           05  T-MOVEMENT              PIC S9(13)V99 VALUE 0.
           05  T-SETTLED               PIC S9(13)V99 VALUE 0.
           05  T-SETTLE-VAT            PIC S9(13)V99 VALUE 0.
           05  T-SKIPPED               PIC S9(7) VALUE 0.
       REPORT SECTION.
       RD  R-REBATE
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 54
           FOOTING 58.
       COPY STRPTHDR.
       01  PH-COLS TYPE IS PAGE HEADING.
           05  LINE 7.
               10  COLUMN 1    PIC X(9) VALUE "Agreement".
               10  COLUMN 16   PIC X(8) VALUE "Customer".
               10  COLUMN 29   PIC X(12) VALUE "Item / group".
               10  COLUMN 46   PIC X(5) VALUE "Basis".
               10  COLUMN 55   PIC X(11) VALUE "  Threshold".
               10  COLUMN 67   PIC X(7) VALUE "    Pct".
               10  COLUMN 75   PIC X(13) VALUE "   Qualifying".
               10  COLUMN 89   PIC X(11) VALUE "    Accrued".
               10  COLUMN 101  PIC X(11) VALUE "   Movement".
               10  COLUMN 113  PIC X(20) VALUE "Note".
           05  LINE 8.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(14) SOURCE D-REF.
               10  COLUMN 16   PIC X(12) SOURCE D-CUST.
               10  COLUMN 29   PIC X(16) SOURCE D-SCOPE.
               10  COLUMN 46   PIC X(8) SOURCE D-BASIS.
               10  COLUMN 55   PIC Z(7)9.99 SOURCE D-THRESHOLD.
               10  COLUMN 67   PIC ZZ9.9999 SOURCE D-PCT.
               10  COLUMN 75   PIC Z(9)9.99- SOURCE D-REVENUE.
               10  COLUMN 89   PIC Z(7)9.99- SOURCE D-ACCRUED.
               10  COLUMN 101  PIC Z(7)9.99- SOURCE D-MOVEMENT.
               10  COLUMN 113  PIC X(20) SOURCE D-NOTE.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(26) VALUE "Agreements assessed".
               10  COLUMN 28   PIC Z(6)9 SOURCE T-AGREEMENTS.
               10  COLUMN 40   PIC X(14) VALUE "not applicable".
               10  COLUMN 55   PIC Z(6)9 SOURCE T-SKIPPED.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(26) VALUE "Qualifying revenue".
               10  COLUMN 28   PIC Z(11)9.99- SOURCE T-REVENUE.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(26) VALUE "Accrued to date".
               10  COLUMN 28   PIC Z(11)9.99- SOURCE T-ACCRUED.
               10  COLUMN 48   PIC X(18) VALUE "movement this run".
               10  COLUMN 68   PIC Z(11)9.99- SOURCE T-MOVEMENT.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(26) VALUE "Settled by credit note".
               10  COLUMN 28   PIC Z(11)9.99- SOURCE T-SETTLED.
               10  COLUMN 48   PIC X(18) VALUE "VAT on settlement".
               10  COLUMN 68   PIC Z(11)9.99- SOURCE T-SETTLE-VAT.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(62) VALUE
               "An accrued rebate is variable consideration and so".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(62) VALUE
               "reduces revenue, never cost.  Settlement releases".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(62) VALUE
               "the accrual: there is no second reduction".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0200-JOB-START THRU 0200-EXIT
           PERFORM 0250-READ-PERIOD THRU 0250-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0300-OPEN-SPOOL THRU 0300-EXIT
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 1000-RUN THRU 1000-EXIT
           PERFORM 0400-CLOSE-SPOOL THRU 0400-EXIT.
       0000-FINISH.
           PERFORM 0500-JOB-END THRU 0500-EXIT
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
           MOVE "FUNCTION" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-FUNC
           END-IF
           MOVE "REBATE" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-REBATE-REF
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "FINCTRL" TO WS-USER
           END-IF
      *>   MODE=SETTLE is accepted as a synonym of FUNCTION=SETTLE
      *>   because the 1997 job stream had no FUNCTION keyword
           IF WS-MODE = "SETTLE"
               MOVE "SETTLE" TO WS-FUNC
               MOVE "COMMIT" TO WS-MODE
           END-IF
           IF WS-FUNC = SPACES
               MOVE "ACCRUE" TO WS-FUNC
           END-IF
           IF WS-MODE = SPACES
               MOVE "PREVIEW" TO WS-MODE
           END-IF
           IF WS-MODE = "COMMIT"
               MOVE "Y" TO WS-COMMIT
           ELSE
               MOVE "N" TO WS-COMMIT
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           DISPLAY "STERP-I-1500 REBATE " FUNCTION TRIM(WS-FUNC)
                   " for " FUNCTION TRIM(WS-CO) " mode "
                   FUNCTION TRIM(WS-MODE) UPON SYSOUT.
       0100-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "REBATE" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE JL-JOB-RUN-ID TO WS-JOB-RUN-ID.
       0200-EXIT.
           EXIT.

       0250-READ-PERIOD SECTION.
       0250-START.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           MOVE SPACES TO SQLIO-STMT
           IF WS-PERIOD = SPACES
               STRING "SELECT FiscalYear, PeriodNo, "
                      "CONVERT(char(8),StartDate,112), "
                      "CONVERT(char(8),EndDate,112), Status FROM "
                      "fin.Period WHERE CompanyCode = ? AND ? "
                      "BETWEEN CONVERT(char(8),StartDate,112) AND "
                      "CONVERT(char(8),EndDate,112)"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 2 TO SQLIO-PARM-CNT
               MOVE WS-CO TO SQLIO-PARM-VAL(1)
               MOVE WS-TODAY-N TO SQLIO-PARM-VAL(2)
           ELSE
               STRING "SELECT FiscalYear, PeriodNo, "
                      "CONVERT(char(8),StartDate,112), "
                      "CONVERT(char(8),EndDate,112), Status FROM "
                      "fin.Period WHERE CompanyCode = ? AND "
                      "FiscalYear = ? AND PeriodNo = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 3 TO SQLIO-PARM-CNT
               MOVE WS-CO TO SQLIO-PARM-VAL(1)
               MOVE WS-PERIOD(1:4) TO SQLIO-PARM-VAL(2)
               MOVE WS-PERIOD(5:2) TO SQLIO-PARM-VAL(3)
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF NOT SQLIO-OK
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-1501 no financial period for the run"
                       UPON SYSOUT
               GO TO 0250-EXIT
           END-IF
           COMPUTE P-FY = FUNCTION NUMVAL(SO-COL(1))
           COMPUTE P-PP = FUNCTION NUMVAL(SO-COL(2))
           MOVE SO-COL(3) TO P-START
           MOVE SO-COL(4) TO P-END
           MOVE SO-COL(5) TO P-STATUS
           PERFORM 0260-YEAR-START THRU 0260-EXIT.
       0250-EXIT.
           EXIT.

       0260-YEAR-START SECTION.
       0260-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(char(8), MIN(StartDate), 112) FROM "
                  "fin.Period WHERE CompanyCode = ? AND FiscalYear = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE P-FY TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-OK
               MOVE SO-COL(1) TO P-YEAR-START
           ELSE
               MOVE P-START TO P-YEAR-START
           END-IF.
       0260-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "REBATE" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1502 cannot build the spool path"
                       UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           PERFORM 0350-FILL-HEADINGS THRU 0350-EXIT
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1503 cannot open " WS-SPOOL-NAME
                       UPON SYSOUT
               GO TO 0300-EXIT
           END-IF
           INITIATE R-REBATE.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "REBATE" TO RPT-REPORT-ID
           MOVE "Customer rebate accrual and settlement"
               TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE SPACES TO RPT-SUBTITLE
           STRING FUNCTION TRIM(WS-FUNC) " in "
                  FUNCTION TRIM(WS-MODE) " - revenue reduction, "
                  "not cost"
               DELIMITED BY SIZE INTO RPT-SUBTITLE
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "Period " P-FY "/" P-PP " to " P-END(7:2) "/"
                  P-END(5:2) "/" P-END(1:4)
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           PERFORM 0360-READ-COMPANY THRU 0360-EXIT.
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
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-OK
               MOVE SO-COL(1) TO RPT-COMPANY-NAME
               MOVE SO-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-RUN SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT RebateId, RebateRef, CustomerCode, "
                  "ISNULL(ItemCode,''), ISNULL(ProductGroup,''), "
                  "Basis, ISNULL(ThresholdValue,0), "
                  "ISNULL(RebatePct,0), ISNULL(RebatePerUnit,0), "
                  "CONVERT(char(8),PeriodStart,112), "
                  "CONVERT(char(8),PeriodEnd,112), CurrencyCode, "
                  "Status, ISNULL(SettlementCreditNote,'') FROM "
                  "so.RebateAgreement WHERE CompanyCode = ? AND "
                  "Status = 'ACTIVE' AND "
                  "CONVERT(char(8),PeriodStart,112) <= ? AND "
                  "CONVERT(char(8),PeriodEnd,112) >= ? AND "
                  "(? = '' OR RebateRef = ?) ORDER BY RebateRef"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE P-END TO SQLIO-PARM-VAL(2)
           MOVE P-START TO SQLIO-PARM-VAL(3)
           MOVE WS-REBATE-REF TO SQLIO-PARM-VAL(4)
           MOVE WS-REBATE-REF TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 14 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1504 " SQLIO-ERRMSG(1:100) UPON SYSOUT
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               MOVE 1 TO SQLIO-CURSOR
               MOVE 14 TO SQLIO-COL-CNT
               CALL "SQLIO" USING SQLIO-CA SO-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 1100-ONE-AGREEMENT THRU 1100-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           MOVE 1 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA SO-ROW.
       1000-EXIT.
           EXIT.

       1100-ONE-AGREEMENT SECTION.
       1100-START.
           ADD 1 TO WS-ROWS-OUT
           COMPUTE A-ID = FUNCTION NUMVAL(SO-COL(1))
           MOVE SO-COL(2) TO A-REF
           MOVE SO-COL(3) TO A-CUST
           MOVE SO-COL(4) TO A-ITEM
           MOVE SO-COL(5) TO A-GROUP
           MOVE SO-COL(6) TO A-BASIS
           COMPUTE A-THRESHOLD = FUNCTION NUMVAL(SO-COL(7))
           COMPUTE A-PCT = FUNCTION NUMVAL(SO-COL(8))
           COMPUTE A-PER-UNIT = FUNCTION NUMVAL(SO-COL(9))
           MOVE SO-COL(10) TO A-FROM
           MOVE SO-COL(11) TO A-TO
           MOVE SO-COL(12) TO A-CURR
           MOVE SO-COL(13) TO A-STATUS
           MOVE SO-COL(14) TO A-CRNOTE
           INITIALIZE WS-MEAS
           MOVE SPACES TO WS-NOTE
           PERFORM 1200-MEASURE THRU 1200-EXIT
           PERFORM 1300-COMPUTE-ACCRUAL THRU 1300-EXIT
           PERFORM 1400-PRIOR-ACCRUAL THRU 1400-EXIT
           COMPUTE M-MOVEMENT = M-REQUIRED - M-PRIOR
           IF WS-DO-SETTLE
               PERFORM 2000-SETTLE THRU 2000-EXIT
           ELSE
               PERFORM 1500-ACCRUE THRU 1500-EXIT
           END-IF
           PERFORM 1900-PRINT THRU 1900-EXIT.
       1100-EXIT.
           EXIT.

      *>   REQ: SO-027 - qualifying net revenue from the POSTED sales
      *>   invoices only.  Credit notes reduce the measure, except the
      *>   rebate settlements themselves, which would otherwise feed
      *>   back into the measure they settle.
       1200-MEASURE SECTION.
       1200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(SUM(CASE WHEN i.DocumentType = "
                  "'CREDITNOTE' THEN -l.NetAmountGBP ELSE "
                  "l.NetAmountGBP END),0), ISNULL(SUM(CASE WHEN "
                  "i.DocumentType = 'CREDITNOTE' THEN -l.Quantity "
                  "ELSE l.Quantity END),0), "
                  "COUNT(DISTINCT i.InvoiceNumber), "
                  "LEFT(ISNULL(STRING_AGG(i.InvoiceNumber, ','),''),"
                  "400)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           STRING FUNCTION TRIM(SQLIO-STMT) " "
                  "FROM ar.SalesInvoice i JOIN ar.SalesInvoiceLine l "
                  "ON l.InvoiceId = i.InvoiceId WHERE "
                  "i.CompanyCode = ? AND i.CustomerCode = ? AND "
                  "i.Status = 'POSTED' AND i.DocumentType IN "
                  "('INVOICE','CREDITNOTE') AND "
                  "CONVERT(char(8), i.TaxPointDate, 112) BETWEEN ? "
                  "AND ? AND (? = '' OR l.ItemCode = ?) AND (? = '' "
                  "OR EXISTS (SELECT 1 FROM inv.Item t WHERE "
                  "t.CompanyCode = i.CompanyCode AND t.ItemCode = "
                  "l.ItemCode AND t.ProductGroup = ?)) AND "
                  "ISNULL(i.CreditReason,'') NOT LIKE "
                  "'Rebate settlement%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 8 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE A-CUST TO SQLIO-PARM-VAL(2)
           MOVE P-YEAR-START TO SQLIO-PARM-VAL(3)
           MOVE P-END TO SQLIO-PARM-VAL(4)
           MOVE A-ITEM TO SQLIO-PARM-VAL(5)
           MOVE A-ITEM TO SQLIO-PARM-VAL(6)
           MOVE A-GROUP TO SQLIO-PARM-VAL(7)
           MOVE A-GROUP TO SQLIO-PARM-VAL(8)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 8
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-OK
               COMPUTE M-REVENUE = FUNCTION NUMVAL(SO-COL(1))
               COMPUTE M-QTY = FUNCTION NUMVAL(SO-COL(2))
               COMPUTE M-INV-COUNT = FUNCTION NUMVAL(SO-COL(3))
               MOVE SO-COL(4) TO M-INV-REFS
           END-IF
           PERFORM 1250-VAT-BASIS THRU 1250-EXIT.
       1200-EXIT.
           EXIT.

      *>   the credit note must carry the VAT liability of the supplies
      *>   it adjusts, so the rate is taken from those invoices and not
      *>   from today's standard rate.
       1250-VAT-BASIS SECTION.
       1250-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 l.VatCode, l.VatRatePct FROM "
                  "ar.SalesInvoice i JOIN ar.SalesInvoiceLine l ON "
                  "l.InvoiceId = i.InvoiceId WHERE i.CompanyCode = ? "
                  "AND i.CustomerCode = ? AND i.Status = 'POSTED' "
                  "AND i.DocumentType = 'INVOICE' AND "
                  "CONVERT(char(8), i.TaxPointDate, 112) BETWEEN ? "
                  "AND ? AND (? = '' OR l.ItemCode = ?) GROUP BY "
                  "l.VatCode, l.VatRatePct ORDER BY "
                  "SUM(l.NetAmountGBP) DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE A-CUST TO SQLIO-PARM-VAL(2)
           MOVE P-YEAR-START TO SQLIO-PARM-VAL(3)
           MOVE P-END TO SQLIO-PARM-VAL(4)
           MOVE A-ITEM TO SQLIO-PARM-VAL(5)
           MOVE A-ITEM TO SQLIO-PARM-VAL(6)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-OK
               MOVE SO-COL(1) TO M-VAT-CODE
               COMPUTE M-VAT-RATE = FUNCTION NUMVAL(SO-COL(2))
           ELSE
               MOVE "S" TO M-VAT-CODE
               MOVE 20 TO M-VAT-RATE
           END-IF.
       1250-EXIT.
           EXIT.

      *>   REQ: SO-027, AC-130 - the threshold is a qualifying gate,
      *>   not a slice: once annual purchases pass it the percentage
      *>   applies to the whole qualifying revenue.  2% above 100,000
      *>   on 120,000.00 of net sales therefore accrues 2,400.00.
       1300-COMPUTE-ACCRUAL SECTION.
       1300-START.
           MOVE 0 TO M-REQUIRED
           EVALUATE A-BASIS
               WHEN "QUANTITY"
                   IF M-QTY > A-THRESHOLD
                       COMPUTE WS-AMT = M-QTY * A-PER-UNIT
                   ELSE
                       MOVE 0 TO WS-AMT
                       MOVE "Below threshold" TO WS-NOTE
                   END-IF
               WHEN "TURNOVER"
                   PERFORM 1350-TURNOVER THRU 1350-EXIT
               WHEN "TIERED"
                   PERFORM 1350-TURNOVER THRU 1350-EXIT
               WHEN OTHER
                   MOVE 0 TO WS-AMT
                   MOVE "Unknown basis" TO WS-NOTE
                   ADD 1 TO T-SKIPPED
           END-EVALUATE
           PERFORM 1380-ROUND-2 THRU 1380-EXIT
           MOVE WS-AMT2 TO M-REQUIRED.
       1300-EXIT.
           EXIT.

       1350-TURNOVER SECTION.
       1350-START.
           IF M-REVENUE > A-THRESHOLD
               COMPUTE WS-AMT = M-REVENUE * A-PCT / 100
           ELSE
               MOVE 0 TO WS-AMT
               MOVE "Below threshold" TO WS-NOTE
           END-IF.
       1350-EXIT.
           EXIT.

       1380-ROUND-2 SECTION.
       1380-START.
           INITIALIZE WS-ROUND
           MOVE "ROUND" TO CA-FUNCTION OF WS-ROUND
           MOVE WS-CO TO CA-COMPANY OF WS-ROUND
           MOVE WS-USER TO CA-USER OF WS-ROUND
           MOVE 2 TO RA-PLACES
           MOVE WS-AMT TO RA-VALUE
           CALL "RNDALLOC" USING WS-ROUND
           IF CA-RETURN-CODE OF WS-ROUND > 4
               MOVE WS-AMT TO WS-AMT2
           ELSE
               MOVE RA-RESULT TO WS-AMT2
           END-IF.
       1380-EXIT.
           EXIT.

      *>   what earlier periods of the same fiscal year already carry,
      *>   and what has already been settled - both are deducted so
      *>   that no amount is recognised twice.
       1400-PRIOR-ACCRUAL SECTION.
       1400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(MAX(CASE WHEN PeriodNo < ? THEN "
                  "AccruedGBP END),0), ISNULL(SUM(SettledGBP),0), "
                  "ISNULL(MAX(CASE WHEN PeriodNo = ? THEN AccruedGBP "
                  "END),0) FROM so.RebateAccrual WHERE RebateId = ? "
                  "AND CompanyCode = ? AND FiscalYear = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE P-PP TO SQLIO-PARM-VAL(1)
           MOVE P-PP TO SQLIO-PARM-VAL(2)
           MOVE A-ID TO SQLIO-PARM-VAL(3)
           MOVE WS-CO TO SQLIO-PARM-VAL(4)
           MOVE P-FY TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-OK
               COMPUTE M-PRIOR = FUNCTION NUMVAL(SO-COL(1))
               COMPUTE M-SETTLED = FUNCTION NUMVAL(SO-COL(2))
               COMPUTE M-THIS-PERIOD = FUNCTION NUMVAL(SO-COL(3))
           END-IF.
       1400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1500-ACCRUE SECTION.
       1500-START.
           ADD 1 TO T-AGREEMENTS
           ADD M-REVENUE TO T-REVENUE
           ADD M-REQUIRED TO T-ACCRUED
           IF WS-COMMIT NOT = "Y"
               IF WS-NOTE = SPACES
                   MOVE "Preview only" TO WS-NOTE
               END-IF
               ADD M-MOVEMENT TO T-MOVEMENT
               GO TO 1500-EXIT
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 3 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               MOVE "Transaction failed" TO WS-NOTE
               GO TO 1500-EXIT
           END-IF
           PERFORM 1600-WRITE-ACCRUAL THRU 1600-EXIT
           IF WS-RC > 8
               PERFORM 4900-ROLLBACK THRU 4900-EXIT
               GO TO 1500-EXIT
           END-IF
           IF M-MOVEMENT NOT = 0
               PERFORM 1700-POST-JOURNAL THRU 1700-EXIT
               IF WS-RC > 8
                   PERFORM 4900-ROLLBACK THRU 4900-EXIT
                   GO TO 1500-EXIT
               END-IF
           ELSE
               MOVE "No movement" TO WS-NOTE
           END-IF
           MOVE "COMT" TO SQLIO-FUNC
           MOVE 3 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               MOVE "Commit failed" TO WS-NOTE
               GO TO 1500-EXIT
           END-IF
           ADD M-MOVEMENT TO T-MOVEMENT
           IF WS-NOTE = SPACES
               MOVE "Accrued" TO WS-NOTE
           END-IF.
       1500-EXIT.
           EXIT.

      *>   one row per rebate, fiscal year and period, refreshed in
      *>   place so that a rerun of the same period cannot double it
       1600-WRITE-ACCRUAL SECTION.
       1600-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.RebateAccrual SET "
                  "QualifyingRevenueGBP = ?, QualifyingQty = ?, "
                  "AccruedGBP = ?, QualifyingInvoiceRefs = ? WHERE "
                  "RebateId = ? AND CompanyCode = ? AND FiscalYear = "
                  "? AND PeriodNo = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 1650-BIND-ACCRUAL THRU 1650-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1505 " SQLIO-ERRMSG(1:90) UPON SYSOUT
               GO TO 1600-EXIT
           END-IF
           IF SQLIO-ROWCOUNT > 0
               MOVE "CLOS" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA SO-ROW
               GO TO 1600-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT so.RebateAccrual (QualifyingRevenueGBP, "
                  "QualifyingQty, AccruedGBP, QualifyingInvoiceRefs, "
                  "RebateId, CompanyCode, FiscalYear, PeriodNo) "
                  "VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 1650-BIND-ACCRUAL THRU 1650-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1506 " SQLIO-ERRMSG(1:90) UPON SYSOUT
               GO TO 1600-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW.
       1600-EXIT.
           EXIT.

       1650-BIND-ACCRUAL SECTION.
       1650-START.
           MOVE 8 TO SQLIO-PARM-CNT
           MOVE M-REVENUE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE M-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE M-REQUIRED TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE M-INV-REFS(1:256) TO SQLIO-PARM-VAL(4)
           MOVE A-ID TO SQLIO-PARM-VAL(5)
           MOVE WS-CO TO SQLIO-PARM-VAL(6)
           MOVE P-FY TO SQLIO-PARM-VAL(7)
           MOVE P-PP TO SQLIO-PARM-VAL(8)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 8
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM.
       1650-EXIT.
           EXIT.

      *>   REQ: SO-027 - DR sales rebates (turnover), CR accrued
      *>   rebate liability, under the business event
      *>   SO.REBATE.ACCRUAL seeded in db\seed\71-so-eventmap.sql.
      *>   A negative movement simply reverses the two sides.
       1700-POST-JOURNAL SECTION.
       1700-START.
           MOVE SPACES TO WS-CORREL
           STRING "SOREB-" FUNCTION TRIM(A-REF) "-" P-FY "-" P-PP
               DELIMITED BY SIZE INTO WS-CORREL
           PERFORM 1710-CLEAR-STAGE THRU 1710-EXIT
           IF WS-RC > 8
               GO TO 1700-EXIT
           END-IF
           MOVE 1 TO WS-JX
           IF M-MOVEMENT > 0
               MOVE "4120" TO SO-COL(21)
               MOVE "SALES" TO SO-COL(22)
               COMPUTE WS-AMT = M-MOVEMENT
               MOVE 0 TO WS-AMT2
               PERFORM 1750-STAGE-LEG THRU 1750-EXIT
               MOVE "2090" TO SO-COL(21)
               MOVE SPACES TO SO-COL(22)
               MOVE 0 TO WS-AMT
               COMPUTE WS-AMT2 = M-MOVEMENT
               PERFORM 1750-STAGE-LEG THRU 1750-EXIT
           ELSE
               MOVE "2090" TO SO-COL(21)
               MOVE SPACES TO SO-COL(22)
               COMPUTE WS-AMT = 0 - M-MOVEMENT
               MOVE 0 TO WS-AMT2
               PERFORM 1750-STAGE-LEG THRU 1750-EXIT
               MOVE "4120" TO SO-COL(21)
               MOVE "SALES" TO SO-COL(22)
               MOVE 0 TO WS-AMT
               COMPUTE WS-AMT2 = 0 - M-MOVEMENT
               PERFORM 1750-STAGE-LEG THRU 1750-EXIT
           END-IF
           IF WS-RC > 8
               GO TO 1700-EXIT
           END-IF
           PERFORM 1800-CALL-POST THRU 1800-EXIT.
       1700-EXIT.
           EXIT.

       1710-CLEAR-STAGE SECTION.
       1710-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE fin.JournalStage WHERE CorrelationId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CORREL TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1507 " SQLIO-ERRMSG(1:90) UPON SYSOUT
               GO TO 1710-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW.
       1710-EXIT.
           EXIT.

       1750-STAGE-LEG SECTION.
       1750-START.
           IF WS-RC > 8
               GO TO 1750-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.JournalStage (CorrelationId, LineNum, "
                  "CompanyCode, AccountCode, DebitAmount, "
                  "CreditAmount, CurrencyCode, DimCostCentre, "
                  "Narrative, SubledgerType, SubledgerRef, SourceRef,"
                  " CreatedBy) VALUES (?, ?, ?, ?, ?, ?, 'GBP', "
                  "NULLIF(?,''), ?, 'AR', ?, ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CORREL TO SQLIO-PARM-VAL(1)
           MOVE WS-JX TO SQLIO-PARM-VAL(2)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           MOVE SO-COL(21) TO SQLIO-PARM-VAL(4)
           MOVE WS-AMT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE WS-AMT2 TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE SO-COL(22) TO SQLIO-PARM-VAL(7)
           MOVE SPACES TO SQLIO-PARM-VAL(8)
           STRING "Rebate " FUNCTION TRIM(A-REF) " customer "
                  FUNCTION TRIM(A-CUST)
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(8)
           MOVE A-CUST TO SQLIO-PARM-VAL(9)
           MOVE A-REF TO SQLIO-PARM-VAL(10)
           MOVE WS-USER TO SQLIO-PARM-VAL(11)
           MOVE 11 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 11
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1508 " SQLIO-ERRMSG(1:90) UPON SYSOUT
               GO TO 1750-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           ADD 1 TO WS-JX.
       1750-EXIT.
           EXIT.

       1800-CALL-POST SECTION.
       1800-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC fin.usp_PostJournal ?, ?, ?, ?, ?, 'GL', ?, "
                  "'GBP', 'SO', ?, 'SO.REBATE.ACCRUAL', 'JNL', "
                  "NULL, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-CORREL TO SQLIO-PARM-VAL(2)
           MOVE WS-USER TO SQLIO-PARM-VAL(3)
           MOVE SPACES TO SQLIO-PARM-VAL(4)
           STRING "Rebate accrual " FUNCTION TRIM(A-REF)
                  " period " P-FY "/" P-PP
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(4)
           MOVE P-END TO SQLIO-PARM-VAL(5)
           MOVE P-END TO SQLIO-PARM-VAL(6)
           MOVE A-REF TO SQLIO-PARM-VAL(7)
           MOVE M-INV-REFS(1:120) TO SQLIO-PARM-VAL(8)
           MOVE 8 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 8
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               MOVE "Posting refused" TO WS-NOTE
               DISPLAY "STERP-E-1509 " SQLIO-ERRMSG(1:90) UPON SYSOUT
               GO TO 1800-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.RebateAccrual SET JournalId = (SELECT "
                  "TOP 1 JournalId FROM fin.Journal WHERE "
                  "CompanyCode = ? AND CorrelationId = ?) WHERE "
                  "RebateId = ? AND CompanyCode = ? AND FiscalYear = "
                  "? AND PeriodNo = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-CORREL TO SQLIO-PARM-VAL(2)
           MOVE A-ID TO SQLIO-PARM-VAL(3)
           MOVE WS-CO TO SQLIO-PARM-VAL(4)
           MOVE P-FY TO SQLIO-PARM-VAL(5)
           MOVE P-PP TO SQLIO-PARM-VAL(6)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               GO TO 1800-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW.
       1800-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   REQ: SO-027 - settlement.  What is still owed to the
      *>   customer is the accrual less what has already been settled;
      *>   when that is nil there is nothing to issue and the second
      *>   run therefore creates no second reduction of revenue.
       2000-SETTLE SECTION.
       2000-START.
           ADD 1 TO T-AGREEMENTS
           ADD M-REVENUE TO T-REVENUE
           ADD M-REQUIRED TO T-ACCRUED
           COMPUTE M-TO-SETTLE = M-REQUIRED - M-SETTLED
           IF M-TO-SETTLE NOT > 0
               MOVE "Already settled" TO WS-NOTE
               ADD 1 TO T-SKIPPED
               GO TO 2000-EXIT
           END-IF
           COMPUTE WS-AMT = M-TO-SETTLE * M-VAT-RATE / 100
           PERFORM 1380-ROUND-2 THRU 1380-EXIT
           MOVE WS-AMT2 TO M-SETTLE-VAT
           IF WS-COMMIT NOT = "Y"
               MOVE "Settle preview" TO WS-NOTE
               ADD M-TO-SETTLE TO T-SETTLED
               ADD M-SETTLE-VAT TO T-SETTLE-VAT
               GO TO 2000-EXIT
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 3 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               MOVE "Transaction failed" TO WS-NOTE
               GO TO 2000-EXIT
           END-IF
           PERFORM 2050-TOP-UP-ACCRUAL THRU 2050-EXIT
           IF WS-RC > 8
               PERFORM 4900-ROLLBACK THRU 4900-EXIT
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-CREDIT-NOTE THRU 2100-EXIT
           IF WS-RC > 8 OR CA-RETURN-CODE OF WS-CRNOTE > 4
               PERFORM 4900-ROLLBACK THRU 4900-EXIT
               MOVE "Credit note refused" TO WS-NOTE
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
               END-IF
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-RELEASE-ACCRUAL THRU 2200-EXIT
           IF WS-RC > 8
               PERFORM 4900-ROLLBACK THRU 4900-EXIT
               GO TO 2000-EXIT
           END-IF
           PERFORM 2300-MARK-SETTLED THRU 2300-EXIT
           IF WS-RC > 8
               PERFORM 4900-ROLLBACK THRU 4900-EXIT
               GO TO 2000-EXIT
           END-IF
           MOVE "COMT" TO SQLIO-FUNC
           MOVE 3 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               MOVE "Commit failed" TO WS-NOTE
               GO TO 2000-EXIT
           END-IF
           ADD M-TO-SETTLE TO T-SETTLED
           ADD M-SETTLE-VAT TO T-SETTLE-VAT
           MOVE SPACES TO WS-NOTE
           STRING "CN " FUNCTION TRIM(AN-CREDIT-NO)
               DELIMITED BY SIZE INTO WS-NOTE.
       2000-EXIT.
           EXIT.

      *>   the accrual row for this fiscal year and period must exist
      *>   and be up to date before anything is settled against it.
      *>   Without it a settlement would leave no SettledGBP to find
      *>   and a second run really could issue a second credit note.
       2050-TOP-UP-ACCRUAL SECTION.
       2050-START.
           PERFORM 1600-WRITE-ACCRUAL THRU 1600-EXIT
           IF WS-RC > 8
               GO TO 2050-EXIT
           END-IF
           IF M-MOVEMENT NOT = 0
               PERFORM 1700-POST-JOURNAL THRU 1700-EXIT
           END-IF.
       2050-EXIT.
           EXIT.

       2100-CREDIT-NOTE SECTION.
       2100-START.
           INITIALIZE WS-CRNOTE
           MOVE SPACES TO WS-CORREL
           STRING "SORBS-" FUNCTION TRIM(A-REF) "-" P-FY
               DELIMITED BY SIZE INTO WS-CORREL
           MOVE "ARCRNOTE" TO CA-PROGRAM OF WS-CRNOTE
           MOVE "POST" TO CA-FUNCTION OF WS-CRNOTE
           MOVE WS-CO TO CA-COMPANY OF WS-CRNOTE
           MOVE WS-USER TO CA-USER OF WS-CRNOTE
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-CRNOTE
           MOVE WS-CORREL TO CA-CORRELATION-ID OF WS-CRNOTE
           MOVE A-CUST TO AN-CUSTOMER
           MOVE "VALUE" TO AN-CREDIT-TYPE
           MOVE "REBATE" TO AN-REASON-CODE
           MOVE SPACES TO AN-REASON
           STRING "Rebate settlement " FUNCTION TRIM(A-REF)
                  " for period to " P-END(7:2) "/" P-END(5:2) "/"
                  P-END(1:4)
               DELIMITED BY SIZE INTO AN-REASON
           COMPUTE AN-DOC-DATE = FUNCTION NUMVAL(P-END)
           MOVE AN-DOC-DATE TO AN-TAX-POINT
           MOVE AN-DOC-DATE TO AN-POSTING-DATE
           MOVE "GBP" TO AN-CURRENCY
           MOVE 1 TO AN-FX-RATE
           MOVE "N" TO AN-RETURN-STOCK
           MOVE M-INV-REFS(1:30) TO AN-ORIG-INVOICE
           MOVE M-TO-SETTLE TO AN-NET
           MOVE M-SETTLE-VAT TO AN-VAT
           COMPUTE AN-GROSS = AN-NET + AN-VAT
           MOVE AN-NET TO AN-NET-GBP
           MOVE AN-VAT TO AN-VAT-GBP
           MOVE AN-GROSS TO AN-GROSS-GBP
           MOVE 1 TO AN-LINE-CNT
           MOVE SPACES TO AN-L-DESC(1)
           STRING "Retrospective rebate " FUNCTION TRIM(A-REF)
               DELIMITED BY SIZE INTO AN-L-DESC(1)
           MOVE 1 TO AN-L-QTY(1)
           MOVE M-TO-SETTLE TO AN-L-NET(1)
           MOVE M-VAT-CODE TO AN-L-VAT-CODE(1)
           MOVE M-VAT-RATE TO AN-L-VAT-RATE(1)
           MOVE M-SETTLE-VAT TO AN-L-VAT-AMT(1)
           MOVE "4120" TO AN-L-REV-ACCOUNT(1)
           CALL "ARCRNOTE" USING WS-CRNOTE.
       2100-EXIT.
           EXIT.

      *>   the credit note has reduced revenue in the sales ledger, so
      *>   the accrual raised earlier must be released against the same
      *>   rebate account.  Without this the rebate would be charged
      *>   to profit twice - CR2770.
       2200-RELEASE-ACCRUAL SECTION.
       2200-START.
           MOVE SPACES TO WS-CORREL
           STRING "SORBR-" FUNCTION TRIM(A-REF) "-" P-FY
               DELIMITED BY SIZE INTO WS-CORREL
           PERFORM 1710-CLEAR-STAGE THRU 1710-EXIT
           IF WS-RC > 8
               GO TO 2200-EXIT
           END-IF
           MOVE 1 TO WS-JX
           MOVE "2090" TO SO-COL(21)
           MOVE SPACES TO SO-COL(22)
           MOVE M-TO-SETTLE TO WS-AMT
           MOVE 0 TO WS-AMT2
           PERFORM 1750-STAGE-LEG THRU 1750-EXIT
           MOVE "4120" TO SO-COL(21)
           MOVE "SALES" TO SO-COL(22)
           MOVE 0 TO WS-AMT
           MOVE M-TO-SETTLE TO WS-AMT2
           PERFORM 1750-STAGE-LEG THRU 1750-EXIT
           IF WS-RC > 8
               GO TO 2200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC fin.usp_PostJournal ?, ?, ?, ?, ?, 'GL', ?, "
                  "'GBP', 'SO', ?, 'SO.REBATE.ACCRUAL', 'JNL', "
                  "NULL, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-CORREL TO SQLIO-PARM-VAL(2)
           MOVE WS-USER TO SQLIO-PARM-VAL(3)
           MOVE SPACES TO SQLIO-PARM-VAL(4)
           STRING "Rebate accrual released on settlement "
                  FUNCTION TRIM(A-REF)
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(4)
           MOVE P-END TO SQLIO-PARM-VAL(5)
           MOVE P-END TO SQLIO-PARM-VAL(6)
           MOVE A-REF TO SQLIO-PARM-VAL(7)
           MOVE AN-CREDIT-NO TO SQLIO-PARM-VAL(8)
           MOVE 8 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 8
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1510 " SQLIO-ERRMSG(1:90) UPON SYSOUT
           END-IF.
       2200-EXIT.
           EXIT.

       2300-MARK-SETTLED SECTION.
       2300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.RebateAccrual SET SettledGBP = "
                  "SettledGBP + ? WHERE RebateId = ? AND "
                  "CompanyCode = ? AND FiscalYear = ? AND PeriodNo "
                  "= ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE M-TO-SETTLE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE A-ID TO SQLIO-PARM-VAL(2)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           MOVE P-FY TO SQLIO-PARM-VAL(4)
           MOVE P-PP TO SQLIO-PARM-VAL(5)
           MOVE 5 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1511 " SQLIO-ERRMSG(1:90) UPON SYSOUT
               GO TO 2300-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.RebateAgreement SET "
                  "SettlementCreditNote = ?, Status = 'SETTLED', "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() WHERE "
                  "RebateId = ? AND CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE AN-CREDIT-NO TO SQLIO-PARM-VAL(1)
           MOVE WS-USER TO SQLIO-PARM-VAL(2)
           MOVE A-ID TO SQLIO-PARM-VAL(3)
           MOVE WS-CO TO SQLIO-PARM-VAL(4)
           MOVE 4 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               GO TO 2300-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA SO-ROW
           PERFORM 2400-AUDIT THRU 2400-EXIT.
       2300-EXIT.
           EXIT.

       2400-AUDIT SECTION.
       2400-START.
           INITIALIZE WS-AUDIT
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUDIT
           MOVE WS-CO TO CA-COMPANY OF WS-AUDIT
           MOVE WS-USER TO CA-USER OF WS-AUDIT
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-AUDIT
           MOVE WS-CORREL TO CA-CORRELATION-ID OF WS-AUDIT
           MOVE "so.RebateAgreement" TO AU-ENTITY-TYPE
           MOVE A-REF TO AU-ENTITY-KEY
           MOVE "SETTLE" TO AU-ACTION
           MOVE "SettledGBP" TO AU-FIELD-NAME
           MOVE M-SETTLED TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO AU-OLD-VALUE
           COMPUTE WS-AMT = M-SETTLED + M-TO-SETTLE
           MOVE WS-AMT TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO AU-NEW-VALUE
           MOVE SPACES TO AU-REASON
           STRING "Rebate settled by credit note "
                  FUNCTION TRIM(AN-CREDIT-NO)
               DELIMITED BY SIZE INTO AU-REASON
           MOVE AN-CREDIT-NO TO AU-AUTHORITY-REF
           MOVE "REBATE" TO AU-SOURCE
           MOVE "N" TO AU-SENSITIVE
           CALL "AUDITLOG" USING WS-AUDIT.
       2400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1900-PRINT SECTION.
       1900-START.
           MOVE A-REF TO D-REF
           MOVE A-CUST TO D-CUST
           IF A-ITEM NOT = SPACES
               MOVE A-ITEM TO D-SCOPE
           ELSE
               IF A-GROUP NOT = SPACES
                   MOVE A-GROUP TO D-SCOPE
               ELSE
                   MOVE "All items" TO D-SCOPE
               END-IF
           END-IF
           MOVE A-BASIS TO D-BASIS
           MOVE A-THRESHOLD TO D-THRESHOLD
           MOVE A-PCT TO D-PCT
           MOVE M-REVENUE TO D-REVENUE
           MOVE M-REQUIRED TO D-ACCRUED
           IF WS-DO-SETTLE
               MOVE M-TO-SETTLE TO D-MOVEMENT
           ELSE
               MOVE M-MOVEMENT TO D-MOVEMENT
           END-IF
           MOVE WS-NOTE TO D-NOTE
           GENERATE DE-LINE.
       1900-EXIT.
           EXIT.

       4900-ROLLBACK SECTION.
       4900-START.
           MOVE "ROLB" TO SQLIO-FUNC
           MOVE 3 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA SO-ROW.
       4900-EXIT.
           EXIT.

       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-REBATE
           CLOSE RPT-FILE
           DISPLAY "STERP-I-1512 " WS-ROWS-OUT " agreement(s) read"
                   UPON SYSOUT
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME)
                   UPON SYSOUT.
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "REBATE" TO JL-JOB-NAME
           MOVE WS-JOB-RUN-ID TO JL-JOB-RUN-ID
           EVALUATE TRUE
               WHEN WS-RC = 0
                   MOVE "COMPLETE" TO JL-STATUS
               WHEN WS-RC = 4
                   MOVE "WARNING" TO JL-STATUS
               WHEN OTHER
                   MOVE "FAILED" TO JL-STATUS
           END-EVALUATE
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE T-AGREEMENTS TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
