      *>================================================================
      *> PROGRAM-ID : DDCOLL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : BACS DIRECT DEBIT COLLECTION RUN.  SELECTS THE
      *>              OPEN ITEMS OF CUSTOMERS COLLECTED BY DIRECT
      *>              DEBIT, CHECKS THAT A LIVE MANDATE EXISTS AND
      *>              THAT THE ADVANCE NOTICE PERIOD HAS EXPIRED,
      *>              MOVES THE DUE DATE ONTO A BACS PROCESSING DAY
      *>              (CALENDAR BACS) AND WRITES A BACS STANDARD 18
      *>              SUBMISSION FILE PLUS A COLLECTION LISTING.
      *>              THE MANDATE CARRIES THE AGREED NOTICE PERIOD;
      *>              THE DATE ON WHICH NOTICE WAS ACTUALLY GIVEN IS
      *>              THE ADVANCE NOTICE OF THE PLANNED COLLECTION,
      *>              OR THE LODGEMENT DATE WHEN THE SCHEDULE WAS
      *>              AGREED IN WRITING.
      *>
      *>              STANDARD 18 RECORD SET (ALL RECORDS 100 BYTES,
      *>              THE 80 BYTE LABELS SPACE FILLED TO 100):
      *>                VOL1  VOLUME LABEL, SERVICE USER NUMBER
      *>                HDR1  FILE HEADER, SUN, DATES AS  YYDDD
      *>                HDR2  FILE HEADER 2, F 02000 00100
      *>                UHL1  USER HEADER, PROCESSING DATE
      *>                ...   ONE 100 BYTE PAYMENT RECORD PER
      *>                      COLLECTION, THEN THE 99 CONTRA RECORD
      *>                UTL1  USER TRAILER, VALUE AND ITEM TOTALS
      *>                EOF1  END OF FILE LABEL 1
      *>                EOF2  END OF FILE LABEL 2
      *>              PAYMENT RECORD: 1-6 DESTINATION SORT CODE,
      *>              7-14 ACCOUNT, 15 ACCOUNT TYPE, 16-17 TRANSACTION
      *>              CODE, 18-23 ORIGINATING SORT CODE, 24-31
      *>              ORIGINATING ACCOUNT, 32-35 FREE FORMAT, 36-46
      *>              AMOUNT IN PENCE, 47-64 ORIGINATOR NAME, 65-82
      *>              REFERENCE, 83-100 DESTINATION ACCOUNT NAME.
      *> RUN AS     : DDCOLL CO=STC DUEDATE=20261031 MODE=PREVIEW
      *>              USER=ARCLERK1 [BANK=HSBC01]
      *> OUTPUT     : run\bacs\DDCOLL-<CO>-<stamp>.txt
      *>              run\spool\DDCOLL-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, DATECALC, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-11-07 RJH  CR0352 ORIGINAL BACS DIRECT DEBIT SUBMISSION
      *> 1999-11-30 KMP  Y2K - JULIAN DATES STILL YYDDD, SEE NOTES
      *> 2004-03-15 KMP  CR1703 ADVANCE NOTICE PERIOD ENFORCED AFTER
      *>                 THE SERVICE USER AUDIT
      *> 2026-10-05 STERP AR-025 PROCESSING DAY CALENDAR AND WORKLIST
      *>================================================================
      *> REQ: AR-025
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DDCOLL.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 07/11/1994.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO DYNAMIC WS-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           SELECT BAC-FILE ASSIGN TO DYNAMIC WS-BACS-NAME
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-BAC-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-DDCOLL.
       FD  BAC-FILE.
      *>   Standard 18 records are a fixed 100 bytes.  The file is
      *>   written as a record sequential file with the line ending
      *>   carried in the record, so that no trailing blank of a name
      *>   or reference field can be lost on the way to the bureau.
       01  BAC-REC.
           05  BAC-DATA                PIC X(100).
           05  BAC-EOL                 PIC X(2).
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ARWORK.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATECALC==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-BACREC                   PIC X(100) VALUE SPACES.
       01  WS-BACS-NAME                PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-BAC-STATUS               PIC X(2) VALUE "00".
       01  BR-ROW.
           05  BR-COL OCCURS 20 TIMES  PIC X(120).
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
           05  WS-BANKCODE             PIC X(20) VALUE SPACES.
           05  WS-DUEDATE              PIC 9(8) VALUE 0.
           05  WS-COMMIT               PIC X VALUE "N".
               88  WS-DO-COMMIT                  VALUE "Y".
       01  WS-ORIG.
           05  O-BANK                  PIC X(20) VALUE SPACES.
           05  O-SORT                  PIC X(6) VALUE "000000".
           05  O-ACCT                  PIC X(8) VALUE "00000000".
           05  O-SUN                   PIC X(6) VALUE "000000".
           05  O-NAME                  PIC X(18) VALUE SPACES.
       01  WS-RUN.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-JOBID                PIC S9(9) COMP-5 VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-PROCDATE             PIC 9(8) VALUE 0.
           05  WS-SUBDATE              PIC 9(8) VALUE 0.
           05  WS-NOTICE-OK            PIC 9(8) VALUE 0.
           05  WS-SEQ                  PIC 9(4) VALUE 0.
           05  WS-COLLREF              PIC X(30) VALUE SPACES.
           05  WS-AMT                  PIC S9(11)V99 VALUE 0.
           05  WS-TOT-PENCE            PIC 9(13) VALUE 0.
           05  WS-JULIAN               PIC X(6) VALUE SPACES.
           05  WS-JUL-PROC             PIC X(6) VALUE SPACES.
           05  WS-JUL-TODAY            PIC X(6) VALUE SPACES.
           05  WS-YYDDD                PIC 9(7) VALUE 0.
           05  WS-DDD                  PIC 9(3) VALUE 0.
           05  WS-DDD-ED               PIC 9(3) VALUE 0.
       01  WS-UK.
           05  WS-UK-DD                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-UK-MM                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-UK-YYYY              PIC 9(4) VALUE 0.
       01  WS-DET.
           05  D-CUST                  PIC X(20) VALUE SPACES.
           05  D-NAME                  PIC X(28) VALUE SPACES.
           05  D-MANDATE               PIC X(18) VALUE SPACES.
           05  D-SORT                  PIC X(6) VALUE SPACES.
           05  D-ACCT                  PIC X(8) VALUE SPACES.
           05  D-PAYER                 PIC X(18) VALUE SPACES.
           05  D-ITEMS                 PIC S9(5) VALUE 0.
           05  D-AMT                   PIC S9(11)V99 VALUE 0.
           05  D-TXN                   PIC X(2) VALUE SPACES.
           05  D-STATE                 PIC X(10) VALUE SPACES.
           05  D-WHY                   PIC X(34) VALUE SPACES.
           05  D-ITEMIDS               PIC X(120) VALUE SPACES.
       01  WS-TOT.
           05  T-SELECTED              PIC S9(7) VALUE 0.
           05  T-COLLECTED             PIC S9(7) VALUE 0.
           05  T-REJECTED              PIC S9(7) VALUE 0.
           05  T-VALUE                 PIC S9(13)V99 VALUE 0.
       01  WS-BACS.
           05  B-PAY.
               10  B-DEST-SORT         PIC X(6).
               10  B-DEST-ACCT         PIC X(8).
               10  B-DEST-TYPE         PIC X.
               10  B-TXN-CODE          PIC X(2).
               10  B-ORIG-SORT         PIC X(6).
               10  B-ORIG-ACCT         PIC X(8).
               10  B-FREE              PIC X(4).
               10  B-PENCE             PIC 9(11).
               10  B-ORIG-NAME         PIC X(18).
               10  B-REFERENCE         PIC X(18).
               10  B-DEST-NAME         PIC X(18).
           05  B-UTL.
               10  FILLER              PIC X(4) VALUE "UTL1".
               10  B-DEBIT-VAL         PIC 9(13).
               10  B-CREDIT-VAL        PIC 9(13).
               10  B-DEBIT-CNT         PIC 9(7).
               10  B-CREDIT-CNT        PIC 9(7).
               10  FILLER              PIC X(36) VALUE SPACES.
           05  B-PENCE-W               PIC 9(13) VALUE 0.
       REPORT SECTION.
       RD  R-DDCOLL
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 8 LAST DETAIL 54
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-COLS TYPE IS CONTROL HEADING FINAL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(8) VALUE "Customer".
               10  COLUMN 22   PIC X(4) VALUE "Name".
               10  COLUMN 51   PIC X(8) VALUE "Mandate".
               10  COLUMN 70   PIC X(9) VALUE "Sort code".
               10  COLUMN 80   PIC X(7) VALUE "Account".
               10  COLUMN 90   PIC X(5) VALUE "Items".
               10  COLUMN 99   PIC X(10) VALUE "Amount GBP".
               10  COLUMN 113  PIC X(3) VALUE "Txn".
               10  COLUMN 117  PIC X(10) VALUE "Outcome".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-COLL TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(20) SOURCE D-CUST.
               10  COLUMN 22   PIC X(28) SOURCE D-NAME.
               10  COLUMN 51   PIC X(18) SOURCE D-MANDATE.
               10  COLUMN 70   PIC X(6) SOURCE D-SORT.
               10  COLUMN 80   PIC X(8) SOURCE D-ACCT.
               10  COLUMN 90   PIC ZZZZ9 SOURCE D-ITEMS.
               10  COLUMN 96   PIC Z,ZZZ,ZZ9.99- SOURCE D-AMT.
               10  COLUMN 113  PIC X(2) SOURCE D-TXN.
               10  COLUMN 117  PIC X(10) SOURCE D-STATE.
           05  LINE PLUS 1.
               10  COLUMN 22   PIC X(34) SOURCE D-WHY.
               10  COLUMN 60   PIC X(10) VALUE "Open items".
               10  COLUMN 71   PIC X(60) SOURCE D-ITEMIDS.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(22) VALUE "Mandates considered".
               10  COLUMN 24   PIC ZZZ,ZZ9 SOURCE T-SELECTED.
               10  COLUMN 36   PIC X(20) VALUE "Collected".
               10  COLUMN 57   PIC ZZZ,ZZ9 SOURCE T-COLLECTED.
               10  COLUMN 69   PIC X(20) VALUE "Not collected".
               10  COLUMN 90   PIC ZZZ,ZZ9 SOURCE T-REJECTED.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(22) VALUE "Submission value GBP".
               10  COLUMN 24   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-VALUE.
               10  COLUMN 47   PIC X(16) VALUE "Service user".
               10  COLUMN 64   PIC X(6) SOURCE O-SUN.
               10  COLUMN 74   PIC X(14) VALUE "Originator".
               10  COLUMN 89   PIC X(18) SOURCE O-NAME.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(50) VALUE
                   "No collection is made without an active mandate".
               10  COLUMN 52   PIC X(48) VALUE
                   "or before the advance notice has expired.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0200-JOB-START
           PERFORM 0250-BACS-DATES
           PERFORM 0260-ORIGINATOR
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0300-OPEN-FILES
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0380-BACS-HEADER
           PERFORM 1000-SELECT-COLLECTIONS
           PERFORM 0390-BACS-TRAILER
           PERFORM 0400-CLOSE-FILES.
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
           MOVE "DUEDATE" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               COMPUTE WS-DUEDATE = FUNCTION NUMVAL(CM-LOOKUP-VALUE)
           END-IF
           MOVE "BANK" TO CM-LOOKUP-KEY
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-BANKCODE
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "ARCLERK1" TO WS-USER
           END-IF
           IF WS-DUEDATE = 0
               MOVE FUNCTION CURRENT-DATE TO AR-NOW
               COMPUTE WS-DUEDATE = FUNCTION NUMVAL(AR-NOW(1:8))
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
           DISPLAY "STERP-I-0100 DDCOLL direct debit run "
                   FUNCTION TRIM(WS-MODE).
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-025  collections arrive on a Bacs processing day and
      *> are submitted one processing day earlier
      *>----------------------------------------------------------------
       0250-BACS-DATES SECTION.
       0250-START.
           INITIALIZE WS-DATECALC
           MOVE "ADJUST" TO CA-FUNCTION OF WS-DATECALC
           MOVE WS-CO TO CA-COMPANY OF WS-DATECALC
           MOVE WS-USER TO CA-USER OF WS-DATECALC
           MOVE WS-DUEDATE TO DC-DATE-1
           MOVE "BACS" TO DC-CALENDAR
           MOVE "NEXT" TO DC-DIRECTION
           CALL "DATECALC" USING WS-DATECALC
           IF CA-RETURN-CODE OF WS-DATECALC > 4
               MOVE WS-DUEDATE TO WS-PROCDATE
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-0251 Bacs calendar unavailable"
           ELSE
               MOVE DC-RESULT-DATE TO WS-PROCDATE
           END-IF
           INITIALIZE WS-DATECALC
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DATECALC
           MOVE WS-CO TO CA-COMPANY OF WS-DATECALC
           MOVE WS-USER TO CA-USER OF WS-DATECALC
           MOVE WS-PROCDATE TO DC-DATE-1
           MOVE "BACS" TO DC-CALENDAR
           MOVE -1 TO DC-NUMBER
           CALL "DATECALC" USING WS-DATECALC
           IF CA-RETURN-CODE OF WS-DATECALC > 4
               MOVE WS-PROCDATE TO WS-SUBDATE
           ELSE
               MOVE DC-RESULT-DATE TO WS-SUBDATE
           END-IF
           MOVE WS-PROCDATE TO AR-NUM-DATE
           PERFORM 8300-JULIAN
           MOVE WS-JULIAN TO WS-JUL-PROC
           MOVE FUNCTION CURRENT-DATE TO AR-NOW
           COMPUTE AR-NUM-DATE = FUNCTION NUMVAL(AR-NOW(1:8))
           PERFORM 8300-JULIAN
           MOVE WS-JULIAN TO WS-JUL-TODAY.
       0250-EXIT.
           EXIT.

       0260-ORIGINATOR SECTION.
       0260-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 BankAccountCode, ISNULL(SortCode,"
                  "'000000'), ISNULL(AccountNumber,'00000000'), "
                  "ISNULL(BacsServiceUserNumber,'000000'), Name "
                  "FROM fin.BankAccount WHERE CompanyCode = ? AND "
                  "SupportsBacs = 1 AND Status = 'ACTIVE' AND "
                  "CurrencyCode = 'GBP'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           IF WS-BANKCODE = SPACES
               MOVE 1 TO SQLIO-PARM-CNT
           ELSE
               MOVE 2 TO SQLIO-PARM-CNT
               MOVE WS-BANKCODE TO SQLIO-PARM-VAL(2)
               MOVE "N" TO SQLIO-PARM-NULL(2)
               MOVE " AND BankAccountCode = ?" TO AR-TEXT
               PERFORM 8200-APPEND-SQL
           END-IF
           MOVE " ORDER BY BankAccountCode" TO AR-TEXT
           PERFORM 8200-APPEND-SQL
           MOVE 5 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF NOT SQLIO-OK
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0261 no Bacs capable bank account"
               GO TO 0260-EXIT
           END-IF
           MOVE BR-COL(1) TO O-BANK
           MOVE BR-COL(2) TO O-SORT
           MOVE BR-COL(3) TO O-ACCT
           MOVE BR-COL(4) TO O-SUN
           MOVE BR-COL(5) TO O-NAME.
       0260-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE WS-DUEDATE TO CA-BUSINESS-DATE OF WS-JOBLOG
           MOVE "DDCOLL" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE JL-JOB-RUN-ID TO WS-JOBID.
       0200-EXIT.
           EXIT.

       0300-OPEN-FILES SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "DDCOLL" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0301 cannot build the spool path"
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
           MOVE "BACS" TO RP-FOLDER
           MOVE SPACES TO RP-EXTENSION
           CALL "RUNPATH" USING WS-RUNPATH
           MOVE RP-FULL-PATH TO WS-BACS-NAME
           PERFORM 0350-FILL-HEADINGS
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0302 cannot open " WS-SPOOL-NAME
               GO TO 0300-EXIT
           END-IF
           OPEN OUTPUT BAC-FILE
           IF WS-BAC-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0303 cannot open " WS-BACS-NAME
               GO TO 0300-EXIT
           END-IF
           INITIATE R-DDCOLL.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "DDCOLL" TO RPT-REPORT-ID
           MOVE "Bacs Direct Debit collection run" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO AR-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(AR-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(AR-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(AR-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(AR-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(AR-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(AR-NOW(13:2))
           MOVE WS-PROCDATE TO AR-NUM-DATE
           PERFORM 8100-FORMAT-DATE
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "Processing " WS-UK DELIMITED BY SIZE
               INTO RPT-AS-AT-TEXT
           MOVE WS-SUBDATE TO AR-NUM-DATE
           PERFORM 8100-FORMAT-DATE
           MOVE SPACES TO RPT-SUBTITLE
           STRING "Submit " WS-UK " - mode "
                  FUNCTION TRIM(WS-MODE)
                  " - Standard 18 file"
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
      *> Standard 18 labels.  All seven labels are 80 byte records; the
      *> submission file carries them space filled to the 100 byte
      *> payment record length.
      *>----------------------------------------------------------------
       0380-BACS-HEADER SECTION.
       0380-START.
           MOVE SPACES TO WS-BACREC
           MOVE "VOL1" TO WS-BACREC(1:4)
           MOVE O-SUN TO WS-BACREC(5:6)
           MOVE O-SUN TO WS-BACREC(42:6)
           MOVE "1" TO WS-BACREC(80:1)
           PERFORM 8900-PUT-BACS
           MOVE SPACES TO WS-BACREC
           MOVE "HDR1" TO WS-BACREC(1:4)
           MOVE "A" TO WS-BACREC(5:1)
           MOVE O-SUN TO WS-BACREC(6:6)
           MOVE "S" TO WS-BACREC(12:1)
           MOVE O-SUN TO WS-BACREC(13:6)
           MOVE "0001" TO WS-BACREC(19:4)
           MOVE "0001" TO WS-BACREC(23:4)
           MOVE "0001" TO WS-BACREC(27:4)
           MOVE "01" TO WS-BACREC(31:2)
           MOVE WS-JUL-TODAY TO WS-BACREC(33:6)
           MOVE WS-JUL-PROC TO WS-BACREC(39:6)
           MOVE "000000" TO WS-BACREC(46:6)
           MOVE "BACS       " TO WS-BACREC(52:11)
           PERFORM 8900-PUT-BACS
           MOVE SPACES TO WS-BACREC
           MOVE "HDR2" TO WS-BACREC(1:4)
           MOVE "F" TO WS-BACREC(5:1)
           MOVE "02000" TO WS-BACREC(6:5)
           MOVE "00100" TO WS-BACREC(11:5)
           PERFORM 8900-PUT-BACS
           MOVE SPACES TO WS-BACREC
           MOVE "UHL1" TO WS-BACREC(1:4)
           MOVE WS-JUL-PROC TO WS-BACREC(5:6)
           MOVE "999999" TO WS-BACREC(11:6)
           MOVE "0001" TO WS-BACREC(17:4)
           MOVE "DD  " TO WS-BACREC(21:4)
           MOVE "00000" TO WS-BACREC(44:5)
           PERFORM 8900-PUT-BACS.
       0380-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-025  eligible items, live mandate, notice expired
      *>----------------------------------------------------------------
       1000-SELECT-COLLECTIONS SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT o.CustomerCode, MAX(c.LegalName), "
                  "m.MandateReference, m.SortCode, m.AccountNumber, "
                  "m.PayerName, m.Status, CONVERT(char(8),COALESCE(("
                  "SELECT MAX(d2.AdvanceNoticeSentAt) FROM "
                  "ar.DirectDebitCollection d2 WHERE d2.MandateId = "
                  "m.MandateId AND d2.DueDate = ?), CASE WHEN "
                  "m.AdvanceNoticeAgreedInWriting = 1 THEN "
                  "m.LodgedDate ELSE NULL END,'19000101'),112), "
                  "CONVERT(varchar(12),m.AdvanceNoticeDays), "
                  "CONVERT(varchar(12),CONVERT(int,"
                  "m.AdvanceNoticeAgreedInWriting)), "
                  "CONVERT(varchar(24),SUM(o.ResidualAmountGBP)), "
                  "CONVERT(varchar(12),COUNT(*)), LEFT(ISNULL("
                  "STRING_AGG(CONVERT(varchar(20),o.OpenItemId),"
                  "','),''),300), CONVERT(varchar(12),(SELECT "
                  "COUNT(*) FROM ar.DirectDebitCollection d WHERE "
                  "d.MandateId = m.MandateId AND d.Status IN "
                  "('SUBMITTED','COLLECTED'))), CONVERT(varchar(12),"
                  "m.MandateId), CONVERT(varchar(12),(SELECT "
                  "COUNT(*) FROM ar.DirectDebitCollection d3 WHERE "
                  "d3.MandateId = m.MandateId AND d3.DueDate = ? "
                  "AND d3.Status IN ('PLANNED','SUBMITTED',"
                  "'COLLECTED'))) FROM ar.OpenItem o JOIN ar.Customer "
                  "c ON c.CompanyCode = o.CompanyCode AND "
                  "c.CustomerCode = o.CustomerCode JOIN "
                  "ar.DirectDebitMandate m ON m.CompanyCode = "
                  "o.CompanyCode AND m.CustomerCode = o.CustomerCode "
                  "WHERE o.CompanyCode = ? AND o.DueDate <= ? AND "
                  "o.ResidualAmountGBP > 0 AND o.SettlementStatus IN "
                  "('OPEN','PARTIAL') AND o.IsDisputed = 0 AND "
                  "o.WrittenOff = 0 AND c.CollectionMethod = 'DD' "
                  "AND m.Status <> 'CANCELLED' GROUP BY "
                  "o.CustomerCode, m.MandateId, m.MandateReference, "
                  "m.SortCode, m.AccountNumber, m.PayerName, "
                  "m.Status, m.LodgedDate, m.AdvanceNoticeDays, "
                  "m.AdvanceNoticeAgreedInWriting ORDER BY "
                  "o.CustomerCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-DUEDATE TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(1) SQLIO-PARM-VAL(2)
                              SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
                       SQLIO-PARM-NULL(4)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE 16 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-1001 " SQLIO-ERRMSG(1:100)
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO AR-DONE
           PERFORM UNTIL AR-FINISHED
               MOVE 1 TO SQLIO-CURSOR
               MOVE 16 TO SQLIO-COL-CNT
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO AR-DONE
               ELSE
                   PERFORM 1100-ONE-MANDATE
               END-IF
           END-PERFORM
           MOVE 1 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       1000-EXIT.
           EXIT.

       1100-ONE-MANDATE SECTION.
       1100-START.
           ADD 1 TO T-SELECTED WS-ROWS-OUT
           MOVE BR-COL(1) TO D-CUST
           MOVE BR-COL(2) TO D-NAME
           MOVE BR-COL(3) TO D-MANDATE
           MOVE BR-COL(4) TO D-SORT
           MOVE BR-COL(5) TO D-ACCT
           MOVE BR-COL(6) TO D-PAYER
           COMPUTE D-AMT = FUNCTION NUMVAL(BR-COL(11))
           COMPUTE D-ITEMS = FUNCTION NUMVAL(BR-COL(12))
           MOVE BR-COL(13) TO D-ITEMIDS
           MOVE SPACES TO D-WHY D-TXN
           MOVE "COLLECT" TO D-STATE
           PERFORM 1200-CHECK-ELIGIBLE
           IF D-STATE NOT = "COLLECT"
               ADD 1 TO T-REJECTED
               GENERATE DE-COLL
               GO TO 1100-EXIT
           END-IF
           PERFORM 1300-TXN-CODE
           PERFORM 1400-WRITE-PAYMENT
           ADD 1 TO T-COLLECTED
           ADD D-AMT TO T-VALUE
           GENERATE DE-COLL
           IF WS-DO-COMMIT
               PERFORM 1500-RECORD-COLLECTION
           END-IF.
       1100-EXIT.
           EXIT.

       1200-CHECK-ELIGIBLE SECTION.
       1200-START.
           IF BR-COL(7)(1:6) NOT = "ACTIVE"
               MOVE "NO MANDATE" TO D-STATE
               MOVE SPACES TO D-WHY
               STRING "Mandate status " FUNCTION TRIM(BR-COL(7))
                      " - not collectable"
                   DELIMITED BY SIZE INTO D-WHY
               GO TO 1200-EXIT
           END-IF
           COMPUTE AR-N = FUNCTION NUMVAL(BR-COL(16))
           IF AR-N > 0
      *>       a collection for this mandate and due date has already
      *>       been planned or submitted - never collect twice
               MOVE "DUPLICATE" TO D-STATE
               MOVE "Already submitted for this date" TO D-WHY
               GO TO 1200-EXIT
           END-IF
           IF D-AMT < 0.01
               MOVE "NIL VALUE" TO D-STATE
               MOVE "Nothing outstanding to collect" TO D-WHY
               GO TO 1200-EXIT
           END-IF
           MOVE BR-COL(8) TO AR-COMPACT-DATE
           COMPUTE AR-NUM-DATE = FUNCTION NUMVAL(AR-COMPACT-DATE)
           IF AR-NUM-DATE < 19900101
               MOVE "NO NOTICE" TO D-STATE
               MOVE "Advance notice has not been sent" TO D-WHY
               GO TO 1200-EXIT
           END-IF
           COMPUTE AR-DAYS = FUNCTION NUMVAL(BR-COL(9))
           IF AR-DAYS <= 0
               MOVE 10 TO AR-DAYS
           END-IF
           INITIALIZE WS-DATECALC
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DATECALC
           MOVE WS-CO TO CA-COMPANY OF WS-DATECALC
           MOVE WS-USER TO CA-USER OF WS-DATECALC
           MOVE AR-NUM-DATE TO DC-DATE-1
           MOVE "BACS" TO DC-CALENDAR
           MOVE AR-DAYS TO DC-NUMBER
           CALL "DATECALC" USING WS-DATECALC
           IF CA-RETURN-CODE OF WS-DATECALC > 4
               COMPUTE AR-N = FUNCTION INTEGER-OF-DATE(AR-NUM-DATE)
                            + AR-DAYS
               COMPUTE WS-NOTICE-OK = FUNCTION DATE-OF-INTEGER(AR-N)
           ELSE
               MOVE DC-RESULT-DATE TO WS-NOTICE-OK
           END-IF
           IF WS-PROCDATE < WS-NOTICE-OK
               MOVE "TOO EARLY" TO D-STATE
               MOVE WS-NOTICE-OK TO AR-NUM-DATE
               PERFORM 8100-FORMAT-DATE
               MOVE SPACES TO D-WHY
               STRING "Advance notice expires " WS-UK
                   DELIMITED BY SIZE INTO D-WHY
           END-IF.
       1200-EXIT.
           EXIT.

       1300-TXN-CODE SECTION.
       1300-START.
      *>   01 first collection of a series, 17 regular collection
           COMPUTE AR-N = FUNCTION NUMVAL(BR-COL(14))
           IF AR-N = 0
               MOVE "01" TO D-TXN
           ELSE
               MOVE "17" TO D-TXN
           END-IF.
       1300-EXIT.
           EXIT.

       1400-WRITE-PAYMENT SECTION.
       1400-START.
           MOVE SPACES TO B-PAY
           MOVE D-SORT TO B-DEST-SORT
           MOVE D-ACCT TO B-DEST-ACCT
           MOVE "0" TO B-DEST-TYPE
           MOVE D-TXN TO B-TXN-CODE
           MOVE O-SORT TO B-ORIG-SORT
           MOVE O-ACCT TO B-ORIG-ACCT
           MOVE "0000" TO B-FREE
           COMPUTE B-PENCE-W = D-AMT * 100
           MOVE B-PENCE-W TO B-PENCE
           MOVE O-NAME TO B-ORIG-NAME
           MOVE D-MANDATE TO B-REFERENCE
           MOVE D-PAYER TO B-DEST-NAME
           MOVE SPACES TO WS-BACREC
           MOVE B-PAY TO WS-BACREC(1:100)
           PERFORM 8900-PUT-BACS
           ADD B-PENCE-W TO WS-TOT-PENCE.
       1400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> The contra record balances the submission: the total of the
      *> collections is credited to the service user's own account.
      *>----------------------------------------------------------------
       0390-BACS-TRAILER SECTION.
       0390-START.
           IF T-COLLECTED > 0
               MOVE SPACES TO B-PAY
               MOVE O-SORT TO B-DEST-SORT
               MOVE O-ACCT TO B-DEST-ACCT
               MOVE "0" TO B-DEST-TYPE
               MOVE "99" TO B-TXN-CODE
               MOVE O-SORT TO B-ORIG-SORT
               MOVE O-ACCT TO B-ORIG-ACCT
               MOVE "0000" TO B-FREE
               MOVE WS-TOT-PENCE TO B-PENCE
               MOVE O-NAME TO B-ORIG-NAME
               MOVE "CONTRA" TO B-REFERENCE
               MOVE O-NAME TO B-DEST-NAME
               MOVE SPACES TO WS-BACREC
               MOVE B-PAY TO WS-BACREC(1:100)
               PERFORM 8900-PUT-BACS
           END-IF
           MOVE WS-TOT-PENCE TO B-DEBIT-VAL B-CREDIT-VAL
           MOVE T-COLLECTED TO B-DEBIT-CNT
           IF T-COLLECTED > 0
               MOVE 1 TO B-CREDIT-CNT
           ELSE
               MOVE 0 TO B-CREDIT-CNT
           END-IF
           MOVE SPACES TO WS-BACREC
           MOVE B-UTL TO WS-BACREC(1:80)
           PERFORM 8900-PUT-BACS
           MOVE SPACES TO WS-BACREC
           MOVE "EOF1" TO WS-BACREC(1:4)
           MOVE "A" TO WS-BACREC(5:1)
           MOVE O-SUN TO WS-BACREC(6:6)
           MOVE "S" TO WS-BACREC(12:1)
           MOVE O-SUN TO WS-BACREC(13:6)
           MOVE "0001" TO WS-BACREC(19:4)
           MOVE "0001" TO WS-BACREC(23:4)
           MOVE "0001" TO WS-BACREC(27:4)
           MOVE "01" TO WS-BACREC(31:2)
           MOVE WS-JUL-TODAY TO WS-BACREC(33:6)
           MOVE WS-JUL-PROC TO WS-BACREC(39:6)
           MOVE "000000" TO WS-BACREC(46:6)
           MOVE "BACS       " TO WS-BACREC(52:11)
           PERFORM 8900-PUT-BACS
           MOVE SPACES TO WS-BACREC
           MOVE "EOF2" TO WS-BACREC(1:4)
           MOVE "F" TO WS-BACREC(5:1)
           MOVE "02000" TO WS-BACREC(6:5)
           MOVE "00100" TO WS-BACREC(11:5)
           PERFORM 8900-PUT-BACS.
       0390-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-025  the planned collection is recorded against the
      *> mandate so that an unpaid or indemnity claim can reinstate it
      *>----------------------------------------------------------------
       1500-RECORD-COLLECTION SECTION.
       1500-START.
           ADD 1 TO WS-SEQ
           MOVE WS-DUEDATE TO AR-DATE-ED
           MOVE SPACES TO WS-COLLREF
           STRING "DDC" AR-DATE-ED(3:6) "-" WS-SEQ
               DELIMITED BY SIZE INTO WS-COLLREF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO ar.DirectDebitCollection (MandateId, "
                  "CompanyCode, CustomerCode, CollectionRef, "
                  "AdvanceNoticeSentAt, SubmissionDate, "
                  "ProcessingDate, DueDate, AmountGBP, "
                  "TransactionCode, BacsFileRef, Status, "
                  "OpenItemIds, CreatedBy) VALUES (?, ?, ?, ?, ?, ?, "
                  "?, ?, ?, ?, ?, 'SUBMITTED', ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE BR-COL(15) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-CO TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE D-CUST TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE WS-COLLREF TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(4)
           MOVE BR-COL(8) TO SQLIO-PARM-VAL(5)
           MOVE "N" TO SQLIO-PARM-NULL(5)
           MOVE WS-SUBDATE TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(6)
           MOVE "N" TO SQLIO-PARM-NULL(6)
           MOVE WS-PROCDATE TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(7)
           MOVE "N" TO SQLIO-PARM-NULL(7)
           MOVE WS-DUEDATE TO AR-DATE-ED
           MOVE AR-DATE-ED TO SQLIO-PARM-VAL(8)
           MOVE "N" TO SQLIO-PARM-NULL(8)
           MOVE D-AMT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE "N" TO SQLIO-PARM-NULL(9)
           MOVE D-TXN TO SQLIO-PARM-VAL(10)
           MOVE "N" TO SQLIO-PARM-NULL(10)
           MOVE WS-BACS-NAME(1:60) TO SQLIO-PARM-VAL(11)
           MOVE "N" TO SQLIO-PARM-NULL(11)
           MOVE D-ITEMIDS TO SQLIO-PARM-VAL(12)
           MOVE "N" TO SQLIO-PARM-NULL(12)
           MOVE WS-USER TO SQLIO-PARM-VAL(13)
           MOVE "N" TO SQLIO-PARM-NULL(13)
           MOVE 13 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-ERROR
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-1501 collection not recorded for "
                       FUNCTION TRIM(D-CUST)
           END-IF
           MOVE 3 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       1500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-SET-WIDTHS SECTION.
       8000-START.
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 20
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

       8300-JULIAN SECTION.
       8300-START.
      *>   Bacs labels still carry the 1960s  YYDDD julian date; the
      *>   leading space is part of the field
           COMPUTE AR-N = FUNCTION INTEGER-OF-DATE(AR-NUM-DATE)
           COMPUTE WS-YYDDD = FUNCTION DAY-OF-INTEGER(AR-N)
           COMPUTE WS-DDD = FUNCTION MOD(WS-YYDDD, 1000)
           MOVE SPACES TO WS-JULIAN
           MOVE AR-NUM-DATE TO AR-DATE-ED
           MOVE AR-DATE-ED(3:2) TO WS-JULIAN(2:2)
           MOVE WS-DDD TO WS-DDD-ED
           MOVE WS-DDD-ED TO WS-JULIAN(4:3).
       8300-EXIT.
           EXIT.

       8900-PUT-BACS SECTION.
       8900-START.
           MOVE WS-BACREC TO BAC-DATA
           MOVE X"0D0A" TO BAC-EOL
           WRITE BAC-REC
           MOVE SPACES TO WS-BACREC.
       8900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-FILES SECTION.
       0400-START.
           TERMINATE R-DDCOLL
           CLOSE RPT-FILE
           CLOSE BAC-FILE
           DISPLAY "STERP-I-0401 " T-COLLECTED " collection(s), "
                   T-REJECTED " not collected"
           DISPLAY "STERP-I-0402 Bacs file "
                   FUNCTION TRIM(WS-BACS-NAME)
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "DDCOLL" TO JL-JOB-NAME
           MOVE WS-JOBID TO JL-JOB-RUN-ID
           EVALUATE TRUE
               WHEN WS-RC = 0   MOVE "COMPLETE" TO JL-STATUS
               WHEN WS-RC = 4   MOVE "WARNING" TO JL-STATUS
               WHEN OTHER       MOVE "FAILED" TO JL-STATUS
           END-EVALUATE
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE WS-ROWS-OUT TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           MOVE WS-BACS-NAME TO JL-LOG-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
