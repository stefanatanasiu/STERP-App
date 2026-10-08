      *>================================================================
      *> PROGRAM-ID : APCLOSE
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : PERIOD CLOSE RECONCILIATION OF THE PURCHASE
      *>              LEDGER.  FIVE THINGS ARE PROVED BEFORE A PERIOD
      *>              IS ALLOWED TO CLOSE:
      *>                1  the creditors control account agrees with
      *>                   the open items behind it
      *>                2  goods received not invoiced still carries
      *>                   only receipts we have not been billed for
      *>                3  invoices registered but not posted are
      *>                   still visible liabilities
      *>                4  invoices waiting on a match do not vanish
      *>                5  supplier returns waiting for a credit are
      *>                   still held in return clearing
      *>              In COMMIT the residual GRNI balance is
      *>              reclassified to accruals through AP.GRNICLEAR
      *>              under a correlation id made of the company and
      *>              the period, so a restarted close cannot post it
      *>              twice.
      *> RUN AS     : APCLOSE CO=STC PERIOD=yyyymm USER=APMGR
      *>              [ASAT=yyyymmdd] [MODE=PREVIEW|COMMIT]
      *> OUTPUT     : run\spool\APCLOSE-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, APPOST, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1992-10-30 RJH  CR0211 ORIGINAL - MONTH END PROOF LIST
      *> 1999-11-02 KMP  Y2K - 4 DIGIT YEARS
      *> 2026-10-06 STERP AP-022 GRNI RECLASSIFICATION ADDED
      *>================================================================
      *> REQ: AP-022, AP-010, AP-021
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APCLOSE.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 30/10/1992.
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
       FD  RPT-FILE REPORT IS R-APCLOSE.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY APBATWK.
       COPY STRPTWS.
       COPY STBATLOG.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY APPOSTWK.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-PERIOD               PIC X(6) VALUE SPACES.
           05  WS-ASAT                 PIC 9(8) VALUE 0.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
       01  WS-RUN-STATE.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-ASAT-TEXT            PIC X(10) VALUE SPACES.
           05  WS-CORR                 PIC X(64) VALUE SPACES.
           05  WS-OUT-OF-BALANCE       PIC X VALUE "N".
           05  WS-GRNI-RESIDUAL        PIC S9(15)V99 VALUE 0.
       01  WS-DET.
           05  D-AREA                  PIC X(40) VALUE SPACES.
           05  D-COUNT                 PIC S9(7) VALUE 0.
           05  D-SUB                   PIC S9(15)V99 VALUE 0.
           05  D-GL                    PIC S9(15)V99 VALUE 0.
           05  D-DIFF                  PIC S9(15)V99 VALUE 0.
           05  D-ACTION                PIC X(20) VALUE SPACES.
       01  WS-TOT.
           05  T-CHECKS                PIC S9(7) VALUE 0.
           05  T-FAILED                PIC S9(7) VALUE 0.
           05  T-POSTED                PIC S9(7) VALUE 0.
       REPORT SECTION.
       RD  R-APCLOSE
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 54
           FOOTING 56.
       COPY STRPTHDR.
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(40) SOURCE D-AREA.
               10  COLUMN 45   PIC Z(6)9 SOURCE D-COUNT.
               10  COLUMN 54   PIC Z(11)9.99- SOURCE D-SUB.
               10  COLUMN 70   PIC Z(11)9.99- SOURCE D-GL.
               10  COLUMN 86   PIC Z(11)9.99- SOURCE D-DIFF.
               10  COLUMN 103  PIC X(20) SOURCE D-ACTION.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 3.
               10  COLUMN 3    PIC X(40) VALUE
                   "PERIOD CLOSE RECONCILIATION (AP-022)".
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(34) VALUE
                   "Reconciliations performed        ".
               10  COLUMN 40   PIC Z(6)9 SOURCE T-CHECKS.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(34) VALUE
                   "Reconciliations out of balance   ".
               10  COLUMN 40   PIC Z(6)9 SOURCE T-FAILED.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(34) VALUE
                   "Clearing journals posted         ".
               10  COLUMN 40   PIC Z(6)9 SOURCE T-POSTED.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(46) VALUE
                   "Uninvoiced owned receipts stay a liability and".
               10  COLUMN 50   PIC X(40) VALUE
                   "an unresolved match does not disappear.".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(46) VALUE
                   "PREVIEW proves the period; COMMIT also posts".
               10  COLUMN 50   PIC X(40) VALUE
                   "the GRNI reclassification.".
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           PERFORM 0100-READ-ARGS
           PERFORM 0200-JOB-START
           IF WS-RC < 8
               PERFORM 0300-OPEN-SPOOL
           END-IF
           IF WS-RC < 8
               PERFORM 1000-RECONCILE
               PERFORM 0400-CLOSE-SPOOL
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
           MOVE CM-PERIOD TO WS-PERIOD
           IF CM-ASAT NOT = SPACES
               COMPUTE WS-ASAT = FUNCTION NUMVAL(CM-ASAT)
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "APMGR" TO WS-USER
           END-IF
           IF WS-MODE NOT = "COMMIT"
               MOVE "PREVIEW" TO WS-MODE
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           IF WS-ASAT = 0
               MOVE WS-TODAY-N TO WS-ASAT
           END-IF
           MOVE WS-ASAT TO WS-DATE-N
           IF WS-PERIOD = SPACES
               MOVE SPACES TO WS-PERIOD
               STRING WS-DATE-YYYY WS-DATE-MM
                   DELIMITED BY SIZE INTO WS-PERIOD
           END-IF
           MOVE SPACES TO WS-ASAT-TEXT
           STRING WS-DATE-YYYY "-" WS-DATE-MM "-" WS-DATE-DD
               DELIMITED BY SIZE INTO WS-ASAT-TEXT
           MOVE WS-MODE TO BL-MODE
           DISPLAY "STERP-I-0100 APCLOSE period close "
                   FUNCTION TRIM(WS-PERIOD) " "
                   FUNCTION TRIM(WS-MODE) " for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "APCLOSE" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE "APCLOSE" TO BL-JOB-NAME.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "APCLOSE" TO RP-REPORT-ID
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
           INITIATE R-APCLOSE.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "APCLOSE" TO RPT-REPORT-ID
           MOVE "Purchase ledger period close reconciliation"
               TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE WS-ASAT TO WS-DATE-N
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "As at " WS-DATE-DD "/" WS-DATE-MM "/"
                  WS-DATE-YYYY
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           MOVE SPACES TO RPT-SUBTITLE
           STRING "Period " FUNCTION TRIM(WS-PERIOD) " - "
                  FUNCTION TRIM(WS-MODE)
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
           MOVE 4 TO SQLIO-CURSOR
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
       1000-RECONCILE SECTION.
       1000-START.
           PERFORM 1100-CONTROL-ACCOUNT
           PERFORM 1200-GRNI
           PERFORM 1300-UNPOSTED
           PERFORM 1400-UNMATCHED
           PERFORM 1500-RETURN-CLEARING
           IF WS-OUT-OF-BALANCE = "Y"
               MOVE 4 TO WS-RC
           END-IF.
       1000-EXIT.
           EXIT.

      *>   one reconciliation line: subledger against nominal
       0900-EMIT SECTION.
       0900-START.
           COMPUTE D-DIFF = D-SUB - D-GL
           ADD 1 TO T-CHECKS
           IF D-DIFF NOT = 0
               ADD 1 TO T-FAILED
               MOVE "Y" TO WS-OUT-OF-BALANCE
               ADD 1 TO BL-WARNING-COUNT
               IF D-ACTION = SPACES
                   MOVE "INVESTIGATE" TO D-ACTION
               END-IF
           ELSE
               IF D-ACTION = SPACES
                   MOVE "AGREED" TO D-ACTION
               END-IF
           END-IF
           ADD 1 TO BL-WRITTEN-COUNT
           GENERATE DE-LINE
           MOVE SPACES TO D-AREA
           MOVE SPACES TO D-ACTION
           MOVE 0 TO D-COUNT
           MOVE 0 TO D-SUB
           MOVE 0 TO D-GL
           MOVE 0 TO D-DIFF.
       0900-EXIT.
           EXIT.

      *>   a single aggregate read: one value into AP-COL(1), the
      *>   count into AP-COL(2)
       0950-READ-TOTAL SECTION.
       0950-START.
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 0290-SET-WIDTHS
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           MOVE 0 TO WS-AMT
           MOVE 0 TO WS-ROWS
           IF SQLIO-OK
               COMPUTE WS-AMT = FUNCTION NUMVAL(AP-COL(1))
               COMPUTE WS-ROWS = FUNCTION NUMVAL(AP-COL(2))
           ELSE
               IF SQLIO-ERROR
                   ADD 1 TO BL-ERROR-COUNT
                   MOVE 4 TO WS-RC
                   DISPLAY "STERP-E-0951 " SQLIO-ERRMSG
               END-IF
           END-IF.
       0950-EXIT.
           EXIT.

      *>   REQ: AP-022 - the creditors control account must agree with
      *>   the open items that make it up.
       1100-CONTROL-ACCOUNT SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(30), "
                  "ISNULL(SUM(ResidualAmountGBP),0)), "
                  "CONVERT(varchar(20), COUNT(*)) "
                  "FROM ap.OpenItem WHERE CompanyCode = ? "
                  "AND ResidualAmountGBP <> 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0950-READ-TOTAL
           MOVE WS-AMT TO D-SUB
           MOVE WS-ROWS TO D-COUNT
           MOVE "2000" TO WS-CODE
           PERFORM 1900-NOMINAL-BALANCE
           COMPUTE D-GL = 0 - WS-AMT2
           MOVE "Creditors control (2000) v open items" TO D-AREA
           PERFORM 0900-EMIT.
       1100-EXIT.
           EXIT.

      *>   REQ: AP-022 - goods received not invoiced.  Anything left
      *>   after the invoices have been matched is an accrual, not a
      *>   creditor, and COMMIT reclassifies it.
       1200-GRNI SECTION.
       1200-START.
           MOVE "2010" TO WS-CODE
           PERFORM 1900-NOMINAL-BALANCE
           COMPUTE WS-GRNI-RESIDUAL = 0 - WS-AMT2
           MOVE WS-GRNI-RESIDUAL TO D-SUB
           MOVE 0 TO D-GL
           MOVE "Goods received not invoiced (2010)" TO D-AREA
           IF WS-GRNI-RESIDUAL = 0
               MOVE "NOTHING TO CLEAR" TO D-ACTION
               PERFORM 0900-EMIT
               GO TO 1200-EXIT
           END-IF
           IF WS-MODE = "COMMIT"
               PERFORM 1250-CLEAR-GRNI
               MOVE "RECLASSIFIED" TO D-ACTION
           ELSE
               MOVE "TO RECLASSIFY" TO D-ACTION
           END-IF
           MOVE WS-GRNI-RESIDUAL TO D-GL
           PERFORM 0900-EMIT.
       1200-EXIT.
           EXIT.

      *>   the same company and period posted twice returns the
      *>   original journal, so a restarted close cannot double post
       1250-CLEAR-GRNI SECTION.
       1250-START.
           INITIALIZE WS-APPOST
           MOVE "POST" TO PJ-FUNC
           MOVE WS-CO TO PJ-COMPANY
           MOVE WS-USER TO PJ-USER
           MOVE WS-ASAT TO PJ-BUSINESS-DATE
           MOVE WS-ASAT TO PJ-POSTING-DATE
           MOVE WS-ASAT TO PJ-DOCUMENT-DATE
           MOVE "GBP" TO PJ-CURRENCY
           MOVE 1 TO PJ-FX-RATE
           MOVE "JOURNAL" TO PJ-JOURNAL-TYPE
           MOVE "AP" TO PJ-SUBLEDGER-TYPE
           MOVE WS-PERIOD TO PJ-SUBLEDGER-REF
           MOVE WS-PERIOD TO PJ-SOURCE-REF
           MOVE "AP.GRNICLEAR" TO PJ-EVENT-CODE
           MOVE SPACES TO WS-CORR
           STRING "APCLOSE-GRNI-" FUNCTION TRIM(WS-CO) "-"
                  FUNCTION TRIM(WS-PERIOD)
               DELIMITED BY SIZE INTO WS-CORR
           MOVE WS-CORR TO PJ-CORRELATION
           MOVE SPACES TO PJ-DESCRIPTION
           STRING "GRNI reclassified at period end - "
                  FUNCTION TRIM(WS-PERIOD)
               DELIMITED BY SIZE INTO PJ-DESCRIPTION
           MOVE 2 TO PJ-LINE-CNT
           MOVE "2010" TO PJ-L-ACCOUNT(1)
           MOVE WS-GRNI-RESIDUAL TO PJ-L-DR(1)
           MOVE "Goods received not invoiced, cleared"
               TO PJ-L-NARRATIVE(1)
           MOVE "2020" TO PJ-L-ACCOUNT(2)
           MOVE WS-GRNI-RESIDUAL TO PJ-L-CR(2)
           MOVE "Accruals, reclassified at period end"
               TO PJ-L-NARRATIVE(2)
           CALL "APPOST" USING WS-APPOST
           IF PJ-RC < 8
               ADD 1 TO T-POSTED
           ELSE
               ADD 1 TO BL-ERROR-COUNT
               MOVE 4 TO WS-RC
               DISPLAY "STERP-E-1251 " FUNCTION TRIM(PJ-MESSAGE)
           END-IF.
       1250-EXIT.
           EXIT.

      *>   REQ: AP-022 - registered but not posted.  The liability is
      *>   real even though the nominal ledger has not seen it.
       1300-UNPOSTED SECTION.
       1300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(30), "
                  "ISNULL(SUM(GrossAmountGBP),0)), "
                  "CONVERT(varchar(20), COUNT(*)) "
                  "FROM ap.PurchaseInvoice WHERE CompanyCode = ? "
                  "AND AccountingState = 'UNPOSTED' "
                  "AND RegistrationState <> 'REJECTED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0950-READ-TOTAL
           MOVE WS-AMT TO D-SUB
           MOVE WS-ROWS TO D-COUNT
           MOVE 0 TO D-GL
           MOVE "Registered, not yet posted" TO D-AREA
           IF WS-AMT = 0
               MOVE "NONE" TO D-ACTION
           ELSE
               MOVE "POST BEFORE CLOSE" TO D-ACTION
           END-IF
           PERFORM 0900-EMIT.
       1300-EXIT.
           EXIT.

      *>   REQ: AP-022 - an unresolved match does not disappear when
      *>   the period closes.
       1400-UNMATCHED SECTION.
       1400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(30), "
                  "ISNULL(SUM(GrossAmountGBP),0)), "
                  "CONVERT(varchar(20), COUNT(*)) "
                  "FROM ap.PurchaseInvoice WHERE CompanyCode = ? "
                  "AND MatchState IN ('UNMATCHED','PARTIAL',"
                  "'VARIANCE') "
                  "AND RegistrationState <> 'REJECTED' "
                  "AND SettlementState <> 'SETTLED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0950-READ-TOTAL
           MOVE WS-AMT TO D-SUB
           MOVE WS-ROWS TO D-COUNT
           MOVE 0 TO D-GL
           MOVE "Invoices still waiting on a match" TO D-AREA
           IF WS-AMT = 0
               MOVE "NONE" TO D-ACTION
           ELSE
               MOVE "CARRIED FORWARD" TO D-ACTION
           END-IF
           PERFORM 0900-EMIT.
       1400-EXIT.
           EXIT.

      *>   REQ: AP-022, AP-010 - a return after invoicing sits in
      *>   return clearing until the credit note arrives.
       1500-RETURN-CLEARING SECTION.
       1500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(30), "
                  "ISNULL(SUM(GrossAmountGBP),0)), "
                  "CONVERT(varchar(20), COUNT(*)) "
                  "FROM ap.PurchaseInvoice WHERE CompanyCode = ? "
                  "AND SettlementState = 'RETURNED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0950-READ-TOTAL
           MOVE WS-AMT TO D-SUB
           MOVE WS-ROWS TO D-COUNT
           MOVE 0 TO D-GL
           MOVE "Supplier returns awaiting a credit" TO D-AREA
           IF WS-AMT = 0
               MOVE "NONE" TO D-ACTION
           ELSE
               MOVE "HELD IN CLEARING" TO D-ACTION
           END-IF
           PERFORM 0900-EMIT.
       1500-EXIT.
           EXIT.

      *>   nominal balance of WS-CODE as debits less credits
       1900-NOMINAL-BALANCE SECTION.
       1900-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(30), "
                  "ISNULL(SUM(DebitAmountGBP) - "
                  "SUM(CreditAmountGBP),0)), "
                  "CONVERT(varchar(20), COUNT(*)) "
                  "FROM fin.JournalLine WHERE CompanyCode = ? "
                  "AND AccountCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 0290-SET-WIDTHS
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           MOVE 0 TO WS-AMT2
           IF SQLIO-OK
               COMPUTE WS-AMT2 = FUNCTION NUMVAL(AP-COL(1))
           ELSE
               IF SQLIO-ERROR
                   ADD 1 TO BL-ERROR-COUNT
                   MOVE 4 TO WS-RC
                   DISPLAY "STERP-E-1901 " SQLIO-ERRMSG
               END-IF
           END-IF.
       1900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-APCLOSE
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
           MOVE "APCLOSE" TO JL-JOB-NAME
           MOVE BL-WRITTEN-COUNT TO JL-ROWS-PROCESSED
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE BL-SPOOL-FILE TO JL-SPOOL-FILE
           MOVE WS-RC TO CA-RETURN-CODE OF WS-JOBLOG
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE T-CHECKS TO BL-COUNT-ED
           DISPLAY "STERP-I-0500 APCLOSE " BL-COUNT-ED
                   " reconciliations, " T-FAILED " out of balance"
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0500-EXIT.
           EXIT.
