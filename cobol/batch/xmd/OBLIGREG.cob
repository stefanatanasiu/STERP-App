      *>================================================================
      *> PROGRAM-ID : OBLIGREG
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : CROSS MODULE CONTROLS AND REPORTING (XMD)
      *> PURPOSE    : STATUTORY OBLIGATIONS REGISTER FOR ONE COMPANY.
      *>              DUE DATES ARE DERIVED FROM THE COMPANY PROFILE -
      *>              ACCOUNTING REFERENCE DATE, LEGAL FORM, VAT
      *>              STAGGER AND THE PAYMENT PRACTICES AND PACKAGING
      *>              FLAGS - AND MATCHED AGAINST THE EVIDENCE HELD
      *>              IN core.StatutoryObligation.  AN OBLIGATION
      *>              WHOSE DUE DATE IS INSIDE THE ESCALATION WINDOW
      *>              WITH NO COMPLETION EVIDENCE IS ESCALATED TO ITS
      *>              OWNER AND TO THE FINANCIAL CONTROLLER.  MARKING
      *>              ONE COMPLETE NEEDS A SUBMISSION REFERENCE, SO
      *>              THE REGISTER REPORTS A MISSING REFERENCE AS AN
      *>              OPEN OBLIGATION RATHER THAN A CLOSED ONE.
      *> RUN AS     : OBLIGREG CO=STC ASAT=20261031 MODE=COMMIT
      *>              USER=FINCTRL [DAYS=30]
      *> OUTPUT     : run\spool\OBLIGREG-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, PARMGET, DATECALC,
      *>              EXCPRAIS, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-09-28 DJM  CR0201 ORIGINAL STATUTORY DIARY (CT AND VAT)
      *> 1998-02-10 RJH  CR0760 COMPANIES HOUSE FILING DEADLINES
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS THROUGHOUT
      *> 2017-04-06 DJM  CR2203 PAYMENT PRACTICES REPORTING DUTY
      *> 2026-10-06 STERP XMD-013 DERIVED DUE DATES AND ESCALATION
      *>================================================================
      *> REQ: XMD-013, XMD-009, COM-022
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OBLIGREG.
       AUTHOR. D J MILLER.
       DATE-WRITTEN. 28/09/1993.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO DYNAMIC XB-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS XB-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-OBLIGREG.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY STRPTWS.
       COPY XMDBATWK.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARMGET==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXCPRAIS==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATECALC==.
       01  WS-CPY.
           05  CP-ARD                  PIC X(5) VALUE "03-31".
           05  CP-FRAMEWORK            PIC X(10) VALUE SPACES.
           05  CP-VATREG               PIC X(20) VALUE SPACES.
           05  CP-STAGGER              PIC X(12) VALUE SPACES.
           05  CP-SCHEME               PIC X(10) VALUE SPACES.
           05  CP-PPR                  PIC X VALUE "0".
           05  CP-PACK                 PIC X VALUE "0".
           05  CP-FORM                 PIC X(10) VALUE "LTD".
           05  CP-ARD-MM               PIC 9(2) VALUE 3.
           05  CP-ARD-DD               PIC 9(2) VALUE 31.
           05  CP-PE-THIS              PIC 9(8) VALUE 0.
           05  CP-PE-LAST              PIC 9(8) VALUE 0.
       01  WS-OB-TABLE.
           05  WS-OB OCCURS 40 TIMES.
               10  OB-CODE             PIC X(20).
               10  OB-DESC             PIC X(42).
               10  OB-PERIOD           PIC X(20).
               10  OB-PSTART           PIC 9(8).
               10  OB-PEND             PIC 9(8).
               10  OB-DUE              PIC 9(8).
               10  OB-OWNER            PIC X(20).
               10  OB-STATUS           PIC X(12).
               10  OB-EVIDENCE         PIC X(30).
               10  OB-DAYS             PIC S9(5).
       01  WS-OB-CNT                   PIC S9(4) VALUE 0.
       01  WS-DET.
           05  D-CODE                  PIC X(20) VALUE SPACES.
           05  D-DESC                  PIC X(42) VALUE SPACES.
           05  D-PERIOD                PIC X(20) VALUE SPACES.
           05  D-DUE                   PIC X(10) VALUE SPACES.
           05  D-OWNER                 PIC X(14) VALUE SPACES.
           05  D-STATUS                PIC X(12) VALUE SPACES.
           05  D-EVIDENCE              PIC X(30) VALUE SPACES.
           05  D-DAYS                  PIC S9(5) VALUE 0.
           05  D-FLAG                  PIC X(12) VALUE SPACES.
       01  WS-CNT.
           05  C-OPEN                  PIC S9(5) VALUE 0.
           05  C-DONE                  PIC S9(5) VALUE 0.
           05  C-LATE                  PIC S9(5) VALUE 0.
           05  C-SOON                  PIC S9(5) VALUE 0.
       01  WS-SCOPE.
           05  S-ASAT                  PIC X(10) VALUE SPACES.
           05  S-ARD                   PIC X(10) VALUE SPACES.
           05  S-WINDOW                PIC 9(4) VALUE 30.
           05  S-MODE                  PIC X(10) VALUE SPACES.
       REPORT SECTION.
       RD  R-OBLIGREG
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 8 LAST DETAIL 52
           FOOTING 58.
       COPY STRPTHDR.
       01  DE-SCOPE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(10) VALUE "Register  ".
               10  COLUMN 11   PIC X(10) SOURCE S-ASAT.
               10  COLUMN 26   PIC X(28) VALUE
                   "Accounting reference date  ".
               10  COLUMN 54   PIC X(10) SOURCE S-ARD.
               10  COLUMN 68   PIC X(20) VALUE
                   "Escalation window   ".
               10  COLUMN 88   PIC ZZZ9 SOURCE S-WINDOW.
               10  COLUMN 93   PIC X(5) VALUE "days ".
               10  COLUMN 100  PIC X(10) SOURCE S-MODE.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(10) VALUE "Obligation".
               10  COLUMN 22   PIC X(11) VALUE "Description".
               10  COLUMN 65   PIC X(6) VALUE "Period".
               10  COLUMN 86   PIC X(8) VALUE "Due date".
               10  COLUMN 97   PIC X(4) VALUE "Days".
               10  COLUMN 103  PIC X(6) VALUE "Status".
               10  COLUMN 116  PIC X(5) VALUE "Owner".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-OB TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(20) SOURCE D-CODE.
               10  COLUMN 22   PIC X(42) SOURCE D-DESC.
               10  COLUMN 65   PIC X(20) SOURCE D-PERIOD.
               10  COLUMN 86   PIC X(10) SOURCE D-DUE.
               10  COLUMN 97   PIC -ZZZ9 SOURCE D-DAYS.
               10  COLUMN 103  PIC X(12) SOURCE D-STATUS.
               10  COLUMN 116  PIC X(14) SOURCE D-OWNER.
       01  DE-EVID TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 24   PIC X(10) VALUE "Evidence: ".
               10  COLUMN 34   PIC X(30) SOURCE D-EVIDENCE.
               10  COLUMN 66   PIC X(12) SOURCE D-FLAG.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "=".
               10  COLUMN 67   PIC X(66) VALUE ALL "=".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(12) VALUE "Open        ".
               10  COLUMN 14   PIC ZZZ9 SOURCE C-OPEN.
               10  COLUMN 22   PIC X(12) VALUE "Complete    ".
               10  COLUMN 34   PIC ZZZ9 SOURCE C-DONE.
               10  COLUMN 42   PIC X(16) VALUE "Due within window".
               10  COLUMN 60   PIC ZZZ9 SOURCE C-SOON.
               10  COLUMN 68   PIC X(12) VALUE "Overdue     ".
               10  COLUMN 80   PIC ZZZ9 SOURCE C-LATE.
               10  COLUMN 90   PIC X(12) VALUE "Escalated   ".
               10  COLUMN 102  PIC ZZZ9 SOURCE XB-ESCALATED.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(64) VALUE
                   "Completion requires the submission reference or".
               10  COLUMN 50   PIC X(62) VALUE
                   "the filed document; a bare tick is not evidence.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-READ-ARGS THRU 0100-EXIT
           MOVE "OBLIGREG" TO RPT-REPORT-ID
           MOVE "Statutory obligations register" TO RPT-TITLE
           MOVE "Derived due dates, owners and completion evidence"
               TO RPT-SUBTITLE
           PERFORM 9100-JOB-START THRU 9100-EXIT
           PERFORM 9200-OPEN-SPOOL THRU 9200-EXIT
           IF XB-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0200-READ-COMPANY THRU 0200-EXIT
           INITIATE R-OBLIGREG
           GENERATE DE-SCOPE
           PERFORM 1000-DERIVE THRU 1000-EXIT
           PERFORM 2000-MATCH-REGISTER THRU 2000-EXIT
           PERFORM 3000-LIST THRU 3000-EXIT
           TERMINATE R-OBLIGREG
           PERFORM 9350-CLOSE-SPOOL THRU 9350-EXIT.
       0000-FINISH.
           PERFORM 9150-JOB-END THRU 9150-EXIT
           MOVE XB-RC TO RETURN-CODE
           STOP RUN.

       0100-READ-ARGS.
           PERFORM 9500-STD-ARGS THRU 9500-EXIT
           MOVE "XMD.OBLIG.ESCALATE.DAYS" TO XB-CODE
           MOVE 30 TO XB-AMT
           PERFORM 9650-GET-PARM-NUM THRU 9650-EXIT
           MOVE XB-AMT TO S-WINDOW
           MOVE "DAYS" TO XB-TEXT
           PERFORM 9550-GET-ARG THRU 9550-EXIT
           IF XB-TEXT NOT = SPACES
               COMPUTE S-WINDOW = FUNCTION NUMVAL(XB-TEXT)
           END-IF
           MOVE XB-ASAT TO XB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO S-ASAT
           MOVE XB-ASAT TO XB-DATE-ED
           MOVE XB-MODE TO S-MODE
           DISPLAY "STERP-I-0100 OBLIGREG register for "
                   FUNCTION TRIM(XB-CO) " as at " XB-ASAT.
       0100-EXIT.
           EXIT.

       0200-READ-COMPANY.
      *>   9300-READ-COMPANY has already filled the heading; BR-ROW
      *>   still holds the profile columns from that single fetch
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(AccountingRefDate,'03-31'), "
                  "ISNULL(AccountingFramework,'FRS102'), "
                  "ISNULL(VatRegNo,''), ISNULL(VatStagger,''), "
                  "ISNULL(VatScheme,'INVOICE'), CONVERT(char(1),"
                  "ISNULL(PaymentPracticesInScope,0)), "
                  "CONVERT(char(1),ISNULL(PackagingInScope,0)), "
                  "ISNULL(LegalForm,'LTD') FROM core.Company WHERE "
                  "CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE 8 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF NOT SQLIO-OK
               MOVE 8 TO XB-RC
               DISPLAY "STERP-E-0201 company " FUNCTION TRIM(XB-CO)
                       " is not on file"
               GO TO 0200-EXIT
           END-IF
           MOVE BR-COL(1)(1:5) TO CP-ARD
           MOVE BR-COL(2) TO CP-FRAMEWORK
           MOVE BR-COL(3) TO CP-VATREG
           MOVE BR-COL(4) TO CP-STAGGER
           MOVE BR-COL(5) TO CP-SCHEME
           MOVE BR-COL(6)(1:1) TO CP-PPR
           MOVE BR-COL(7)(1:1) TO CP-PACK
           MOVE BR-COL(8) TO CP-FORM
           COMPUTE CP-ARD-MM = FUNCTION NUMVAL(CP-ARD(1:2))
           COMPUTE CP-ARD-DD = FUNCTION NUMVAL(CP-ARD(4:2))
           PERFORM 0250-PERIOD-ENDS THRU 0250-EXIT.
       0200-EXIT.
           EXIT.

       0250-PERIOD-ENDS.
      *>   the accounting reference date falling on or before the as
      *>   at date is the last completed period of account
           MOVE XB-ASAT TO XB-DATE-ED
           COMPUTE XB-I = FUNCTION NUMVAL(XB-DATE-ED(1:4))
           COMPUTE CP-PE-THIS = (XB-I * 10000)
                              + (CP-ARD-MM * 100) + CP-ARD-DD
           IF CP-PE-THIS > XB-ASAT
               COMPUTE CP-PE-THIS = CP-PE-THIS - 10000
           END-IF
           COMPUTE CP-PE-LAST = CP-PE-THIS - 10000
           MOVE CP-PE-THIS TO XB-NUM-DATE
           PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
           MOVE XB-UK TO S-ARD.
       0250-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-013  baseline due date rules of specification 11 s5
      *>----------------------------------------------------------------
       1000-DERIVE.
           MOVE 0 TO WS-OB-CNT
           PERFORM 1100-VAT THRU 1100-EXIT
           PERFORM 1200-ACCOUNTS THRU 1200-EXIT
           PERFORM 1300-CORPTAX THRU 1300-EXIT
           PERFORM 1400-CONFSTMT THRU 1400-EXIT
           IF CP-PPR = "1"
               PERFORM 1500-PPR THRU 1500-EXIT
           END-IF
           IF CP-PACK = "1"
               PERFORM 1600-PACKAGING THRU 1600-EXIT
           END-IF.
       1000-EXIT.
           EXIT.

       1100-VAT.
      *>   the VAT calendar is already held; one calendar month and
      *>   seven days after the period end is the baseline
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT VatPeriodKey, CONVERT(char(8),StartDate,"
                  "112), CONVERT(char(8),EndDate,112), CONVERT("
                  "char(8),FilingDueDate,112), Status, ISNULL("
                  "SubmissionRef,'') FROM fin.VatPeriod WHERE "
                  "CompanyCode = ? AND CONVERT(char(8),EndDate,112)"
                  " <= ? ORDER BY EndDate DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE XB-ASAT TO XB-DATE-ED
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 6 TO SQLIO-COL-CNT
           PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 4 TO XB-RC
               GO TO 1100-EXIT
           END-IF
           MOVE 0 TO XB-J
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR OR XB-J > 5
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK
                   ADD 1 TO XB-J
                   ADD 1 TO WS-OB-CNT
                   MOVE "VATRETURN" TO OB-CODE(WS-OB-CNT)
                   MOVE "VAT return and payment to HMRC"
                       TO OB-DESC(WS-OB-CNT)
                   MOVE BR-COL(1) TO OB-PERIOD(WS-OB-CNT)
                   MOVE BR-COL(2) TO XB-TEXT
                   PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
                   MOVE XB-NUM-DATE TO OB-PSTART(WS-OB-CNT)
                   MOVE BR-COL(3) TO XB-TEXT
                   PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
                   MOVE XB-NUM-DATE TO OB-PEND(WS-OB-CNT)
                   MOVE BR-COL(4) TO XB-TEXT
                   PERFORM 9470-ISO-TO-NUM THRU 9470-EXIT
                   IF XB-NUM-DATE = 0
                       MOVE OB-PEND(WS-OB-CNT) TO XB-NUM-DATE
                       MOVE 1 TO XB-K
                       PERFORM 8100-ADD-MONTHS THRU 8100-EXIT
                       MOVE 7 TO XB-K
                       PERFORM 8200-ADD-DAYS THRU 8200-EXIT
                   END-IF
                   MOVE XB-NUM-DATE TO OB-DUE(WS-OB-CNT)
                   MOVE "VATOWNER" TO OB-OWNER(WS-OB-CNT)
                   MOVE BR-COL(6) TO OB-EVIDENCE(WS-OB-CNT)
                   IF BR-COL(5) = "SUBMITTED"
                       MOVE "COMPLETE" TO OB-STATUS(WS-OB-CNT)
                   ELSE
                       MOVE "OPEN" TO OB-STATUS(WS-OB-CNT)
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       1100-EXIT.
           EXIT.

       1200-ACCOUNTS.
           PERFORM VARYING XB-I FROM 1 BY 1 UNTIL XB-I > 2
               ADD 1 TO WS-OB-CNT
               MOVE "ANNUALACCTS" TO OB-CODE(WS-OB-CNT)
               MOVE "Annual accounts filing at Companies House"
                   TO OB-DESC(WS-OB-CNT)
               IF XB-I = 1
                   MOVE CP-PE-LAST TO OB-PEND(WS-OB-CNT)
               ELSE
                   MOVE CP-PE-THIS TO OB-PEND(WS-OB-CNT)
               END-IF
               MOVE 12 TO XB-K
               PERFORM 8400-PERIOD-START THRU 8400-EXIT
               MOVE OB-PEND(WS-OB-CNT) TO XB-NUM-DATE
               IF CP-FORM = "PLC"
                   MOVE 6 TO XB-K
               ELSE
                   MOVE 9 TO XB-K
               END-IF
               PERFORM 8100-ADD-MONTHS THRU 8100-EXIT
               MOVE XB-NUM-DATE TO OB-DUE(WS-OB-CNT)
               MOVE "FINCTRL" TO OB-OWNER(WS-OB-CNT)
               MOVE "OPEN" TO OB-STATUS(WS-OB-CNT)
               PERFORM 8300-PERIOD-TEXT THRU 8300-EXIT
           END-PERFORM.
       1200-EXIT.
           EXIT.

       1300-CORPTAX.
           ADD 1 TO WS-OB-CNT
           MOVE "CORPTAXPAY" TO OB-CODE(WS-OB-CNT)
           MOVE "Corporation tax payment (nine months and a day)"
               TO OB-DESC(WS-OB-CNT)
           MOVE CP-PE-LAST TO OB-PEND(WS-OB-CNT)
           MOVE 12 TO XB-K
           PERFORM 8400-PERIOD-START THRU 8400-EXIT
           MOVE CP-PE-LAST TO XB-NUM-DATE
           MOVE 9 TO XB-K
           PERFORM 8100-ADD-MONTHS THRU 8100-EXIT
           MOVE 1 TO XB-K
           PERFORM 8200-ADD-DAYS THRU 8200-EXIT
           MOVE XB-NUM-DATE TO OB-DUE(WS-OB-CNT)
           MOVE "FINCTRL" TO OB-OWNER(WS-OB-CNT)
           MOVE "OPEN" TO OB-STATUS(WS-OB-CNT)
           PERFORM 8300-PERIOD-TEXT THRU 8300-EXIT
           ADD 1 TO WS-OB-CNT
           MOVE "CORPTAX" TO OB-CODE(WS-OB-CNT)
           MOVE "Corporation tax return CT600 (twelve months)"
               TO OB-DESC(WS-OB-CNT)
           MOVE CP-PE-LAST TO OB-PEND(WS-OB-CNT)
           MOVE 12 TO XB-K
           PERFORM 8400-PERIOD-START THRU 8400-EXIT
           MOVE CP-PE-LAST TO XB-NUM-DATE
           MOVE 12 TO XB-K
           PERFORM 8100-ADD-MONTHS THRU 8100-EXIT
           MOVE XB-NUM-DATE TO OB-DUE(WS-OB-CNT)
           MOVE "FINCTRL" TO OB-OWNER(WS-OB-CNT)
           MOVE "OPEN" TO OB-STATUS(WS-OB-CNT)
           PERFORM 8300-PERIOD-TEXT THRU 8300-EXIT.
       1300-EXIT.
           EXIT.

       1400-CONFSTMT.
           ADD 1 TO WS-OB-CNT
           MOVE "CONFSTMT" TO OB-CODE(WS-OB-CNT)
           MOVE "Confirmation statement (fourteen days)"
               TO OB-DESC(WS-OB-CNT)
           MOVE CP-PE-THIS TO OB-PEND(WS-OB-CNT)
           MOVE 12 TO XB-K
           PERFORM 8400-PERIOD-START THRU 8400-EXIT
           MOVE CP-PE-THIS TO XB-NUM-DATE
           MOVE 14 TO XB-K
           PERFORM 8200-ADD-DAYS THRU 8200-EXIT
           MOVE XB-NUM-DATE TO OB-DUE(WS-OB-CNT)
           MOVE "SYSADM" TO OB-OWNER(WS-OB-CNT)
           MOVE "OPEN" TO OB-STATUS(WS-OB-CNT)
           PERFORM 8300-PERIOD-TEXT THRU 8300-EXIT.
       1400-EXIT.
           EXIT.

       1500-PPR.
      *>   within 30 days after the end of each half of the financial
      *>   year (qualifying companies only)
           PERFORM VARYING XB-I FROM 1 BY 1 UNTIL XB-I > 2
               ADD 1 TO WS-OB-CNT
               MOVE "PAYPRACTICES" TO OB-CODE(WS-OB-CNT)
               MOVE "Payment practices and performance report"
                   TO OB-DESC(WS-OB-CNT)
               MOVE CP-PE-THIS TO XB-NUM-DATE
               IF XB-I = 1
                   MOVE -6 TO XB-K
                   PERFORM 8100-ADD-MONTHS THRU 8100-EXIT
               END-IF
               MOVE XB-NUM-DATE TO OB-PEND(WS-OB-CNT)
               MOVE 6 TO XB-K
               PERFORM 8400-PERIOD-START THRU 8400-EXIT
               MOVE OB-PEND(WS-OB-CNT) TO XB-NUM-DATE
               MOVE 30 TO XB-K
               PERFORM 8200-ADD-DAYS THRU 8200-EXIT
               MOVE XB-NUM-DATE TO OB-DUE(WS-OB-CNT)
               MOVE "FINCTRL" TO OB-OWNER(WS-OB-CNT)
               MOVE "OPEN" TO OB-STATUS(WS-OB-CNT)
               PERFORM 8300-PERIOD-TEXT THRU 8300-EXIT
           END-PERFORM.
       1500-EXIT.
           EXIT.

       1600-PACKAGING.
           ADD 1 TO WS-OB-CNT
           MOVE "PACKAGINGDATA" TO OB-CODE(WS-OB-CNT)
           MOVE "Plastic Packaging Tax and packaging EPR data"
               TO OB-DESC(WS-OB-CNT)
           MOVE CP-PE-THIS TO OB-PEND(WS-OB-CNT)
           MOVE 12 TO XB-K
           PERFORM 8400-PERIOD-START THRU 8400-EXIT
           MOVE CP-PE-THIS TO XB-NUM-DATE
           MOVE 1 TO XB-K
           PERFORM 8100-ADD-MONTHS THRU 8100-EXIT
           MOVE XB-NUM-DATE TO OB-DUE(WS-OB-CNT)
           MOVE "COSTACC" TO OB-OWNER(WS-OB-CNT)
           MOVE "OPEN" TO OB-STATUS(WS-OB-CNT)
           PERFORM 8300-PERIOD-TEXT THRU 8300-EXIT.
       1600-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-013  evidence of completion comes from the register;
      *> a row with no reference stays open whatever its status says
      *>----------------------------------------------------------------
       2000-MATCH-REGISTER.
           PERFORM VARYING XB-I FROM 1 BY 1
                   UNTIL XB-I > WS-OB-CNT
               MOVE SPACES TO SQLIO-STMT
               STRING "SELECT Status, ISNULL(EvidenceRef,''), "
                      "ISNULL(OwnerUser,''), ISNULL(CONVERT(char(8)"
                      ",CompletedAt,112),'') FROM "
                      "core.StatutoryObligation WHERE CompanyCode = "
                      "? AND ObligationCode = ? AND PeriodKey = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE XB-CO TO SQLIO-PARM-VAL(1)
               MOVE OB-CODE(XB-I) TO SQLIO-PARM-VAL(2)
               MOVE OB-PERIOD(XB-I) TO SQLIO-PARM-VAL(3)
               MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
                           SQLIO-PARM-NULL(3)
               MOVE 3 TO SQLIO-PARM-CNT
               MOVE 4 TO SQLIO-COL-CNT
               PERFORM 9400-SET-WIDTHS THRU 9400-EXIT
               MOVE 4 TO SQLIO-CURSOR
               MOVE "SNGL" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-OK
                   MOVE BR-COL(1) TO OB-STATUS(XB-I)
                   IF BR-COL(2) NOT = SPACES
                       MOVE BR-COL(2) TO OB-EVIDENCE(XB-I)
                   END-IF
                   IF BR-COL(3) NOT = SPACES
                       MOVE BR-COL(3) TO OB-OWNER(XB-I)
                   END-IF
               END-IF
               IF OB-STATUS(XB-I) = "COMPLETE"
                   IF OB-EVIDENCE(XB-I) = SPACES
                       MOVE "NO EVIDENCE" TO OB-STATUS(XB-I)
                   END-IF
               END-IF
               IF OB-DUE(XB-I) = 0
                   MOVE 0 TO OB-DAYS(XB-I)
               ELSE
                   COMPUTE OB-DAYS(XB-I) =
                       FUNCTION INTEGER-OF-DATE(OB-DUE(XB-I))
                     - FUNCTION INTEGER-OF-DATE(XB-ASAT)
               END-IF
           END-PERFORM.
       2000-EXIT.
           EXIT.

       3000-LIST.
           PERFORM VARYING XB-I FROM 1 BY 1
                   UNTIL XB-I > WS-OB-CNT
               MOVE OB-CODE(XB-I) TO D-CODE
               MOVE OB-DESC(XB-I) TO D-DESC
               MOVE OB-PERIOD(XB-I) TO D-PERIOD
               MOVE OB-OWNER(XB-I) TO D-OWNER
               MOVE OB-STATUS(XB-I) TO D-STATUS
               MOVE OB-EVIDENCE(XB-I) TO D-EVIDENCE
               MOVE OB-DAYS(XB-I) TO D-DAYS
               MOVE OB-DUE(XB-I) TO XB-NUM-DATE
               PERFORM 9450-FORMAT-DATE THRU 9450-EXIT
               MOVE XB-UK TO D-DUE
               MOVE SPACES TO D-FLAG
               IF OB-STATUS(XB-I) = "COMPLETE"
                   ADD 1 TO C-DONE
               ELSE
                   ADD 1 TO C-OPEN
                   IF OB-DAYS(XB-I) < 0
                       ADD 1 TO C-LATE
                       MOVE "OVERDUE" TO D-FLAG
                   ELSE
                       IF OB-DAYS(XB-I) <= S-WINDOW
                           ADD 1 TO C-SOON
                           MOVE "DUE SOON" TO D-FLAG
                       END-IF
                   END-IF
               END-IF
               ADD 1 TO XB-ROWS-OUT
               GENERATE DE-OB
               IF D-FLAG NOT = SPACES
                   PERFORM 3100-ESCALATE THRU 3100-EXIT
               END-IF
               IF OB-EVIDENCE(XB-I) NOT = SPACES
                   OR D-FLAG NOT = SPACES
                   GENERATE DE-EVID
               END-IF
               PERFORM 3200-UPSERT THRU 3200-EXIT
           END-PERFORM
           IF C-LATE > 0 AND XB-RC < 4
               MOVE 4 TO XB-RC
           END-IF
           DISPLAY "STERP-I-3001 " WS-OB-CNT " obligation(s), "
                   C-OPEN " open, " C-LATE " overdue, "
                   XB-ESCALATED " escalated".
       3000-EXIT.
           EXIT.

       3100-ESCALATE.
           INITIALIZE WS-EXCPRAIS
           IF D-FLAG = "OVERDUE"
               MOVE "HOLD" TO EX-SEVERITY
               MOVE 1 TO EX-PRIORITY
           ELSE
               MOVE "WARNING" TO EX-SEVERITY
               MOVE 3 TO EX-PRIORITY
           END-IF
           MOVE "STATUTORY OBLIGATION" TO EX-CATEGORY
           MOVE "OBLIGATION" TO EX-DOCUMENT-TYPE
           MOVE SPACES TO XB-TEXT
           STRING FUNCTION TRIM(XB-CO) "/"
                  FUNCTION TRIM(OB-CODE(XB-I)) "/"
                  FUNCTION TRIM(OB-PERIOD(XB-I))
               DELIMITED BY SIZE INTO XB-TEXT
           MOVE XB-TEXT(1:40) TO EX-DOCUMENT-REF
           MOVE SPACES TO XB-TEXT2
           STRING FUNCTION TRIM(OB-DESC(XB-I))
                  " is " FUNCTION TRIM(D-FLAG) " and has no "
                  "completion evidence. Escalated to the owner and "
                  "to the financial controller."
               DELIMITED BY SIZE INTO XB-TEXT2
           MOVE XB-TEXT2(1:200) TO EX-MESSAGE-TEXT
           MOVE OB-OWNER(XB-I) TO EX-OWNER-ROLE
           MOVE OB-DUE(XB-I) TO EX-DUE-AT
           PERFORM 9700-RAISE-EXCEPTION THRU 9700-EXIT
           ADD 1 TO XB-ESCALATED.
       3100-EXIT.
           EXIT.

       3200-UPSERT.
           IF NOT XB-COMMIT
               GO TO 3200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "MERGE core.StatutoryObligation AS t USING ("
                  "SELECT ? AS CompanyCode, ? AS ObligationCode, ? "
                  "AS PeriodKey) AS s ON t.CompanyCode = "
                  "s.CompanyCode AND t.ObligationCode = "
                  "s.ObligationCode AND t.PeriodKey = s.PeriodKey "
                  "WHEN MATCHED THEN UPDATE SET DueDate = CONVERT("
                  "date,?,112), OwnerRole = ?, PeriodStart = "
                  "CONVERT(date,?,112), PeriodEnd = CONVERT(date,?,"
                  "112) WHEN NOT MATCHED THEN INSERT (CompanyCode, "
                  "ObligationCode, PeriodKey, PeriodStart, "
                  "PeriodEnd, DueDate, OwnerRole, Status) VALUES (?"
                  ",?,?,CONVERT(date,?,112),CONVERT(date,?,112),"
                  "CONVERT(date,?,112),?,'OPEN');"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE XB-CO TO SQLIO-PARM-VAL(1) SQLIO-PARM-VAL(8)
           MOVE OB-CODE(XB-I) TO SQLIO-PARM-VAL(2)
                                 SQLIO-PARM-VAL(9)
           MOVE OB-PERIOD(XB-I) TO SQLIO-PARM-VAL(3)
                                   SQLIO-PARM-VAL(10)
           MOVE OB-DUE(XB-I) TO XB-DATE-ED
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(4) SQLIO-PARM-VAL(13)
           MOVE OB-OWNER(XB-I) TO SQLIO-PARM-VAL(5)
                                  SQLIO-PARM-VAL(14)
           MOVE OB-PSTART(XB-I) TO XB-DATE-ED
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(6) SQLIO-PARM-VAL(11)
           MOVE OB-PEND(XB-I) TO XB-DATE-ED
           MOVE XB-DATE-ED TO SQLIO-PARM-VAL(7) SQLIO-PARM-VAL(12)
           PERFORM VARYING XB-J FROM 1 BY 1 UNTIL XB-J > 14
               MOVE "N" TO SQLIO-PARM-NULL(XB-J)
           END-PERFORM
           MOVE 14 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 4 TO XB-RC
               DISPLAY "STERP-W-3201 " SQLIO-ERRMSG(1:90)
           ELSE
               ADD 1 TO XB-POSTED
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       3200-EXIT.
           EXIT.

       8100-ADD-MONTHS.
      *>   XB-NUM-DATE + XB-K months, clamped to the month end
           INITIALIZE WS-DATECALC
           MOVE "ADDMONTH" TO CA-FUNCTION OF WS-DATECALC
           MOVE XB-CO TO CA-COMPANY OF WS-DATECALC
           MOVE XB-USER TO CA-USER OF WS-DATECALC
           MOVE XB-NUM-DATE TO DC-DATE-1
           MOVE XB-K TO DC-NUMBER
           CALL "DATECALC" USING WS-DATECALC
           IF CA-RETURN-CODE OF WS-DATECALC < 8
               MOVE DC-RESULT-DATE TO XB-NUM-DATE
           END-IF.
       8100-EXIT.
           EXIT.

       8200-ADD-DAYS.
           INITIALIZE WS-DATECALC
           MOVE "ADDDAYS" TO CA-FUNCTION OF WS-DATECALC
           MOVE XB-CO TO CA-COMPANY OF WS-DATECALC
           MOVE XB-USER TO CA-USER OF WS-DATECALC
           MOVE XB-NUM-DATE TO DC-DATE-1
           MOVE XB-K TO DC-NUMBER
           CALL "DATECALC" USING WS-DATECALC
           IF CA-RETURN-CODE OF WS-DATECALC < 8
               MOVE DC-RESULT-DATE TO XB-NUM-DATE
           END-IF.
       8200-EXIT.
           EXIT.

       8300-PERIOD-TEXT.
           MOVE OB-PEND(WS-OB-CNT) TO XB-DATE-ED
           MOVE SPACES TO OB-PERIOD(WS-OB-CNT)
           STRING "PE" XB-DATE-ED
               DELIMITED BY SIZE INTO OB-PERIOD(WS-OB-CNT)
           MOVE SPACES TO OB-EVIDENCE(WS-OB-CNT).
       8300-EXIT.
           EXIT.

       8400-PERIOD-START.
      *>   period start = period end less XB-K months plus one day
           MOVE OB-PEND(WS-OB-CNT) TO XB-NUM-DATE
           COMPUTE XB-K = 0 - XB-K
           PERFORM 8100-ADD-MONTHS THRU 8100-EXIT
           MOVE 1 TO XB-K
           PERFORM 8200-ADD-DAYS THRU 8200-EXIT
           MOVE XB-NUM-DATE TO OB-PSTART(WS-OB-CNT).
       8400-EXIT.
           EXIT.

       COPY XMDBATPR.
