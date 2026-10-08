      *>================================================================
      *> PROGRAM-ID : APPPR
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : PAYMENT PRACTICES AND PERFORMANCE REPORT, THE
      *>              HALF YEARLY RETURN A LARGE UK COMPANY HAS TO
      *>              PUBLISH (REPORT RPT-018).  THREE HEADLINE
      *>              FIGURES: THE AVERAGE NUMBER OF DAYS TAKEN TO PAY,
      *>              THE PERCENTAGE PAID IN 30 / 31-60 / OVER 60 DAYS
      *>              AND THE PERCENTAGE NOT PAID WITHIN THE AGREED
      *>              TERMS.  THE DETAIL IS KEPT SO THE FIGURES CAN BE
      *>              DEFENDED LINE BY LINE.
      *> RUN AS     : APPPR CO=STC FROM=yyyymmdd TO=yyyymmdd USER=APMGR
      *>              [MODE=PREVIEW|LIVE]
      *> OUTPUT     : run\spool\APPPR-<CO>-<stamp>.lst
      *>              ap.PaymentPracticesReport + ...Detail when LIVE
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2017-04-06 AMD  CR3110 ORIGINAL - DUTY TO REPORT ON PAYMENT
      *>                 PRACTICES, SI 2017/395
      *> 2026-10-06 STERP AP-028 DISPUTED INVOICES SHOWN SEPARATELY
      *>================================================================
      *> REQ: AP-028, AP-026, RPT-018
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APPPR.
       AUTHOR. A M DANIELS.
       DATE-WRITTEN. 06/04/2017.
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
       FD  RPT-FILE REPORT IS R-APPPR.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY APBATWK.
       COPY STRPTWS.
       COPY STBATLOG.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-FROM                 PIC 9(8) VALUE 0.
           05  WS-TO                   PIC 9(8) VALUE 0.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
       01  WS-RUN-STATE.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-PERIOD-REF           PIC X(30) VALUE SPACES.
       01  WS-DET.
           05  D-BAND                  PIC X(18) VALUE SPACES.
           05  D-SUPPLIER              PIC X(20) VALUE SPACES.
           05  D-DOC-REF               PIC X(24) VALUE SPACES.
           05  D-RECEIVED              PIC X(10) VALUE SPACES.
           05  D-PAID-DATE             PIC X(10) VALUE SPACES.
           05  D-TERM-DAYS             PIC S9(5) VALUE 0.
           05  D-DAYS-TAKEN            PIC S9(5) VALUE 0.
           05  D-AMOUNT                PIC S9(13)V99 VALUE 0.
           05  D-IN-TERMS              PIC X(3) VALUE SPACES.
           05  D-DISPUTED              PIC X(3) VALUE SPACES.
       01  WS-TOT.
           05  T-COUNT                 PIC S9(9) VALUE 0.
           05  T-DAYS                  PIC S9(11) VALUE 0.
           05  T-IN-30                 PIC S9(9) VALUE 0.
           05  T-IN-60                 PIC S9(9) VALUE 0.
           05  T-OVER-60               PIC S9(9) VALUE 0.
           05  T-LATE                  PIC S9(9) VALUE 0.
           05  T-DISPUTED              PIC S9(9) VALUE 0.
           05  T-VALUE                 PIC S9(15)V99 VALUE 0.
       01  WS-CALC.
           05  C-AVG-DAYS              PIC S9(7)V9(2) VALUE 0.
           05  C-PCT-30                PIC S9(5)V9(2) VALUE 0.
           05  C-PCT-60                PIC S9(5)V9(2) VALUE 0.
           05  C-PCT-OVER              PIC S9(5)V9(2) VALUE 0.
           05  C-PCT-LATE              PIC S9(5)V9(2) VALUE 0.
       REPORT SECTION.
       RD  R-APPPR
           CONTROLS ARE FINAL D-BAND
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 50
           FOOTING 52.
       COPY STRPTHDR.
       01  CH-BAND TYPE IS CONTROL HEADING D-BAND.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(14) VALUE "Payment band  ".
               10  COLUMN 16   PIC X(18) SOURCE D-BAND.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(20) VALUE "Supplier            ".
               10  COLUMN 25   PIC X(24) VALUE "Document".
               10  COLUMN 51   PIC X(10) VALUE "Received  ".
               10  COLUMN 63   PIC X(10) VALUE "Paid      ".
               10  COLUMN 76   PIC X(6) VALUE "Terms ".
               10  COLUMN 85   PIC X(5) VALUE "Taken".
               10  COLUMN 97   PIC X(6) VALUE "Amount".
               10  COLUMN 116  PIC X(6) VALUE "InTerm".
               10  COLUMN 124  PIC X(8) VALUE "Disputed".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(66) VALUE ALL "-".
               10  COLUMN 69   PIC X(64) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(20) SOURCE D-SUPPLIER.
               10  COLUMN 25   PIC X(24) SOURCE D-DOC-REF.
               10  COLUMN 51   PIC X(10) SOURCE D-RECEIVED.
               10  COLUMN 63   PIC X(10) SOURCE D-PAID-DATE.
               10  COLUMN 76   PIC Z(3)9 SOURCE D-TERM-DAYS.
               10  COLUMN 84   PIC Z(4)9 SOURCE D-DAYS-TAKEN.
               10  COLUMN 92   PIC Z(9)9.99- SOURCE D-AMOUNT.
               10  COLUMN 118  PIC X(3) SOURCE D-IN-TERMS.
               10  COLUMN 127  PIC X(3) SOURCE D-DISPUTED.
       01  CF-BAND TYPE IS CONTROL FOOTING D-BAND.
           05  LINE PLUS 1.
               10  COLUMN 63   PIC X(18) VALUE "Band total        ".
               10  COLUMN 92   PIC Z(9)9.99- SUM D-AMOUNT.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(44) VALUE
                   "PAYMENT PRACTICES AND PERFORMANCE - RPT-018".
           05  LINE PLUS 2.
               10  COLUMN 5    PIC X(40) VALUE
                   "Average days taken to pay an invoice".
               10  COLUMN 48   PIC Z(5)9.99 SOURCE C-AVG-DAYS.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(40) VALUE
                   "Invoices paid within 30 days        %".
               10  COLUMN 48   PIC Z(3)9.99 SOURCE C-PCT-30.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(40) VALUE
                   "Invoices paid in 31 to 60 days      %".
               10  COLUMN 48   PIC Z(3)9.99 SOURCE C-PCT-60.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(40) VALUE
                   "Invoices paid in over 60 days       %".
               10  COLUMN 48   PIC Z(3)9.99 SOURCE C-PCT-OVER.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(40) VALUE
                   "Invoices NOT paid within agreed terms %".
               10  COLUMN 48   PIC Z(3)9.99 SOURCE C-PCT-LATE.
           05  LINE PLUS 2.
               10  COLUMN 5    PIC X(40) VALUE
                   "Invoices counted".
               10  COLUMN 48   PIC Z(8)9 SOURCE T-COUNT.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(40) VALUE
                   "Of which disputed in the period".
               10  COLUMN 48   PIC Z(8)9 SOURCE T-DISPUTED.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(40) VALUE
                   "Total value paid".
               10  COLUMN 45   PIC Z(12)9.99- SOURCE T-VALUE.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(60) VALUE
                   "Days taken run from the later of invoice receipt".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(60) VALUE
                   "and goods receipt, to the date of payment.".
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           PERFORM 0100-READ-ARGS
           PERFORM 0200-JOB-START
           IF WS-RC < 8
               PERFORM 0300-OPEN-SPOOL
           END-IF
           IF WS-RC < 8
               PERFORM 1000-PRODUCE-REPORT
               PERFORM 2000-CALCULATE-HEADLINES
               PERFORM 0400-CLOSE-SPOOL
               PERFORM 2100-STORE-RESULT
           END-IF.
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
           MOVE "FROM" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               COMPUTE WS-FROM = FUNCTION NUMVAL(CM-LOOKUP-VALUE)
           END-IF
           MOVE "TO" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               COMPUTE WS-TO = FUNCTION NUMVAL(CM-LOOKUP-VALUE)
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "APMGR" TO WS-USER
           END-IF
           IF WS-MODE = SPACES
               MOVE "LIVE" TO WS-MODE
           END-IF
           MOVE WS-MODE TO BL-MODE
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           IF WS-FROM = 0 OR WS-TO = 0
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0101 APPPR needs FROM= and TO="
               GO TO 0100-EXIT
           END-IF
           MOVE SPACES TO WS-PERIOD-REF
           STRING "PPR-" WS-FROM "-" WS-TO
               DELIMITED BY SIZE INTO WS-PERIOD-REF
           DISPLAY "STERP-I-0100 APPPR payment practices RPT-018".
       0100-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "APPPR" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE "APPPR" TO BL-JOB-NAME.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "APPPR" TO RP-REPORT-ID
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
           PERFORM 0350-FILL-HEADINGS
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0302 cannot open the spool file"
               GO TO 0300-EXIT
           END-IF
           INITIATE R-APPPR.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "APPPR" TO RPT-REPORT-ID
           MOVE "Payment practices and performance" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE WS-MODE TO RPT-STATUS-TEXT
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE WS-TO TO WS-DATE-N
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "To " WS-DATE-DD "/" WS-DATE-MM "/"
                  WS-DATE-YYYY
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           MOVE WS-FROM TO WS-DATE-N
           MOVE SPACES TO RPT-SUBTITLE
           STRING "Reporting period from " WS-DATE-DD "/"
                  WS-DATE-MM "/" WS-DATE-YYYY
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
           PERFORM 0290-SET-WIDTHS
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-OK
               MOVE AP-COL(1) TO RPT-COMPANY-NAME
               MOVE AP-COL(2) TO RPT-REG-NUMBER
           ELSE
               MOVE WS-CO TO RPT-COMPANY-NAME
           END-IF.
       0360-EXIT.
           EXIT.

       0290-SET-WIDTHS SECTION.
       0290-START.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0290-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-PRODUCE-REPORT SECTION.
       1000-START.
           PERFORM 1100-OPEN-PAID
           IF WS-SQL-FAILED = "Y"
               MOVE 12 TO WS-RC
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-DONE = "Y"
               PERFORM 1200-FETCH-PAID
               IF WS-DONE NOT = "Y"
                   PERFORM 1300-CLASSIFY
                   ADD 1 TO BL-WRITTEN-COUNT
                   GENERATE DE-LINE
               END-IF
           END-PERFORM
           PERFORM 1900-CLOSE-CURSOR.
       1000-EXIT.
           EXIT.

      *>   The clock starts at the later of invoice receipt and goods
      *>   receipt, and stops on the date the payment settled.  The
      *>   band is precomputed so Report Writer can control break on it.
       1100-OPEN-PAID SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CASE WHEN DATEDIFF(day, d.StartDate, "
                  "d.PaidDate) <= 30 THEN '1 up to 30 days' "
                  "WHEN DATEDIFF(day, d.StartDate, d.PaidDate) <= 60 "
                  "THEN '2 31 to 60 days' "
                  "ELSE '3 over 60 days' END, "
                  "d.SupplierCode, d.DocumentNumber, "
                  "CONVERT(varchar(10), d.StartDate, 103), "
                  "CONVERT(varchar(10), d.PaidDate, 103), "
                  "CONVERT(varchar(10), ISNULL(d.TermDays,0)), "
                  "CONVERT(varchar(10), DATEDIFF(day, d.StartDate, "
                  "d.PaidDate)), "
                  "CONVERT(varchar(30), ISNULL(d.AmountBase,0)), "
                  "CASE WHEN d.PaidDate <= d.DueDate THEN 'Yes' "
                  "ELSE 'No' END, "
                  "CASE WHEN ISNULL(d.WasDisputed,0) = 1 THEN 'Yes' "
                  "ELSE 'No' END "
                  "FROM ap.vw_PaymentPracticesBase d "
                  "WHERE d.CompanyCode = ? AND d.PaidDate >= ? "
                  "AND d.PaidDate <= ? ORDER BY 1, d.SupplierCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-FROM TO WS-DATE-N
           MOVE SPACES TO SQLIO-PARM-VAL(2)
           STRING WS-DATE-YYYY "-" WS-DATE-MM "-" WS-DATE-DD
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE WS-TO TO WS-DATE-N
           MOVE SPACES TO SQLIO-PARM-VAL(3)
           STRING WS-DATE-YYYY "-" WS-DATE-MM "-" WS-DATE-DD
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 10 TO SQLIO-COL-CNT
           PERFORM 0290-SET-WIDTHS
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-ERROR
               MOVE "Y" TO WS-SQL-FAILED
               DISPLAY "STERP-E-1101 " SQLIO-ERRMSG
           END-IF.
       1100-EXIT.
           EXIT.

       1200-FETCH-PAID SECTION.
       1200-START.
           MOVE 5 TO SQLIO-CURSOR
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "Y" TO WS-DONE
               GO TO 1200-EXIT
           END-IF
           ADD 1 TO BL-READ-COUNT
           MOVE AP-COL(1) TO D-BAND
           MOVE AP-COL(2) TO D-SUPPLIER
           MOVE AP-COL(3) TO D-DOC-REF
           MOVE AP-COL(4) TO D-RECEIVED
           MOVE AP-COL(5) TO D-PAID-DATE
           COMPUTE D-TERM-DAYS = FUNCTION NUMVAL(AP-COL(6))
           COMPUTE D-DAYS-TAKEN = FUNCTION NUMVAL(AP-COL(7))
           COMPUTE D-AMOUNT = FUNCTION NUMVAL(AP-COL(8))
           MOVE AP-COL(9) TO D-IN-TERMS
           MOVE AP-COL(10) TO D-DISPUTED.
       1200-EXIT.
           EXIT.

       1300-CLASSIFY SECTION.
       1300-START.
           ADD 1 TO T-COUNT
           ADD D-DAYS-TAKEN TO T-DAYS
           ADD D-AMOUNT TO T-VALUE
           EVALUATE TRUE
               WHEN D-DAYS-TAKEN NOT > 30
                   ADD 1 TO T-IN-30
               WHEN D-DAYS-TAKEN NOT > 60
                   ADD 1 TO T-IN-60
               WHEN OTHER
                   ADD 1 TO T-OVER-60
           END-EVALUATE
           IF D-IN-TERMS NOT = "Yes"
               ADD 1 TO T-LATE
           END-IF
           IF D-DISPUTED = "Yes"
               ADD 1 TO T-DISPUTED
           END-IF.
       1300-EXIT.
           EXIT.

      *>   REQ: AP-028 - the four published figures.  Percentages are
      *>   of the invoice COUNT, not of value - that is what the
      *>   regulations ask for.
       2000-CALCULATE-HEADLINES SECTION.
       2000-START.
           IF T-COUNT = 0
               MOVE 0 TO C-AVG-DAYS C-PCT-30 C-PCT-60
               MOVE 0 TO C-PCT-OVER C-PCT-LATE
               GO TO 2000-EXIT
           END-IF
           COMPUTE C-AVG-DAYS ROUNDED = T-DAYS / T-COUNT
           COMPUTE C-PCT-30 ROUNDED = T-IN-30 * 100 / T-COUNT
           COMPUTE C-PCT-60 ROUNDED = T-IN-60 * 100 / T-COUNT
           COMPUTE C-PCT-OVER ROUNDED = T-OVER-60 * 100 / T-COUNT
           COMPUTE C-PCT-LATE ROUNDED = T-LATE * 100 / T-COUNT.
       2000-EXIT.
           EXIT.

       2100-STORE-RESULT SECTION.
       2100-START.
           IF WS-MODE = "PREVIEW" OR T-COUNT = 0
               GO TO 2100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC ap.usp_PaymentPracticesData "
                  "@CompanyCode = ?, @PeriodRef = ?, "
                  "@AverageDays = ?, @PctWithin30 = ?, "
                  "@Pct31To60 = ?, @PctOver60 = ?, "
                  "@PctNotInTerms = ?, @InvoiceCount = ?, "
                  "@TotalValue = ?, @CreatedBy = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 10 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-PERIOD-REF TO SQLIO-PARM-VAL(2)
           MOVE C-AVG-DAYS TO SQLIO-NUM-EDIT
           MOVE SQLIO-NUM-EDIT TO SQLIO-PARM-VAL(3)
           MOVE C-PCT-30 TO SQLIO-NUM-EDIT
           MOVE SQLIO-NUM-EDIT TO SQLIO-PARM-VAL(4)
           MOVE C-PCT-60 TO SQLIO-NUM-EDIT
           MOVE SQLIO-NUM-EDIT TO SQLIO-PARM-VAL(5)
           MOVE C-PCT-OVER TO SQLIO-NUM-EDIT
           MOVE SQLIO-NUM-EDIT TO SQLIO-PARM-VAL(6)
           MOVE C-PCT-LATE TO SQLIO-NUM-EDIT
           MOVE SQLIO-NUM-EDIT TO SQLIO-PARM-VAL(7)
           MOVE T-COUNT TO SQLIO-INT-EDIT
           MOVE SQLIO-INT-EDIT TO SQLIO-PARM-VAL(8)
           MOVE T-VALUE TO SQLIO-NUM-EDIT
           MOVE SQLIO-NUM-EDIT TO SQLIO-PARM-VAL(9)
           MOVE WS-USER TO SQLIO-PARM-VAL(10)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 10
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-ERROR
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-2101 " SQLIO-ERRMSG
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW.
       2100-EXIT.
           EXIT.

       1900-CLOSE-CURSOR SECTION.
       1900-START.
           MOVE 5 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW.
       1900-EXIT.
           EXIT.

       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-APPPR
           CLOSE RPT-FILE.
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           MOVE WS-RC TO BL-RETURN-CODE
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "APPPR" TO JL-JOB-NAME
           MOVE BL-WRITTEN-COUNT TO JL-ROWS-PROCESSED
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE BL-SPOOL-FILE TO JL-SPOOL-FILE
           MOVE WS-RC TO CA-RETURN-CODE OF WS-JOBLOG
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE T-COUNT TO BL-COUNT-ED
           DISPLAY "STERP-I-0500 APPPR counted " BL-COUNT-ED
                   " invoices for RPT-018"
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0500-EXIT.
           EXIT.
