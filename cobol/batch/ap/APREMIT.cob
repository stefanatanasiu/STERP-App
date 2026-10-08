      *>================================================================
      *> PROGRAM-ID : APREMIT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : REMITTANCE ADVICES FOR A PAYMENT RUN.  ONE ADVICE
      *>              PER SUPPLIER SHOWING EVERY INVOICE, CREDIT NOTE,
      *>              DISCOUNT AND SET-OFF THAT MAKES UP THE PAYMENT,
      *>              SO THE SUPPLIER CAN CASH MATCH IT.  132 COLUMN
      *>              REPORT WRITER, ONE PAGE GROUP PER SUPPLIER.
      *> RUN AS     : APREMIT CO=STC RUN=<payrun> USER=APMGR
      *>              [MODE=ALL|REISSUE]
      *> OUTPUT     : run\spool\APREMIT-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-10-14 RJH  CR0191 ORIGINAL - REMITTANCE ADVICE PRINT
      *> 1997-02-20 RJH  CR0810 SETTLEMENT DISCOUNT SHOWN SEPARATELY
      *> 1999-11-02 KMP  Y2K - 4 DIGIT YEARS
      *> 2026-10-06 STERP AP-016 SET-OFF AND ADVANCE LINES
      *>================================================================
      *> REQ: AP-016, AP-017, AP-013
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APREMIT.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 14/10/1991.
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
       FD  RPT-FILE REPORT IS R-APREMIT.
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
           05  WS-RUN-REF              PIC X(30) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
       01  WS-RUN-STATE.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-SUPPLIERS            PIC S9(7) VALUE 0.
       01  WS-DET.
           05  D-SUPPLIER              PIC X(20) VALUE SPACES.
           05  D-SUPP-NAME             PIC X(40) VALUE SPACES.
           05  D-PAY-REF               PIC X(30) VALUE SPACES.
           05  D-PAY-METHOD            PIC X(10) VALUE SPACES.
           05  D-DOC-TYPE              PIC X(12) VALUE SPACES.
           05  D-DOC-REF               PIC X(30) VALUE SPACES.
           05  D-OUR-REF               PIC X(30) VALUE SPACES.
           05  D-DOC-DATE              PIC X(10) VALUE SPACES.
           05  D-CURRENCY              PIC X(3) VALUE SPACES.
           05  D-GROSS                 PIC S9(13)V99 VALUE 0.
           05  D-DISCOUNT              PIC S9(13)V99 VALUE 0.
           05  D-SETOFF                PIC S9(13)V99 VALUE 0.
           05  D-PAID                  PIC S9(13)V99 VALUE 0.
       01  WS-TOT.
           05  T-PAID                  PIC S9(15)V99 VALUE 0.
           05  T-DOCS                  PIC S9(7) VALUE 0.
       REPORT SECTION.
       RD  R-APREMIT
           CONTROLS ARE FINAL D-SUPPLIER
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 54
           FOOTING 56.
       COPY STRPTHDR.
       01  CH-SUPP TYPE IS CONTROL HEADING D-SUPPLIER
           NEXT GROUP NEXT PAGE.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(20) VALUE "REMITTANCE ADVICE   ".
               10  COLUMN 24   PIC X(20) SOURCE D-SUPPLIER.
               10  COLUMN 46   PIC X(40) SOURCE D-SUPP-NAME.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(18) VALUE "Our payment ref   ".
               10  COLUMN 24   PIC X(30) SOURCE D-PAY-REF.
               10  COLUMN 60   PIC X(8) VALUE "Method  ".
               10  COLUMN 69   PIC X(10) SOURCE D-PAY-METHOD.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(12) VALUE "Document    ".
               10  COLUMN 17   PIC X(20) VALUE "Your reference      ".
               10  COLUMN 39   PIC X(16) VALUE "Our reference   ".
               10  COLUMN 57   PIC X(10) VALUE "Doc date  ".
               10  COLUMN 69   PIC X(3) VALUE "Ccy".
               10  COLUMN 78   PIC X(5) VALUE "Gross".
               10  COLUMN 95   PIC X(8) VALUE "Discount".
               10  COLUMN 110  PIC X(7) VALUE "Set-off".
               10  COLUMN 124  PIC X(8) VALUE "Paid    ".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(66) VALUE ALL "-".
               10  COLUMN 69   PIC X(64) VALUE ALL "-".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(12) SOURCE D-DOC-TYPE.
               10  COLUMN 17   PIC X(20) SOURCE D-DOC-REF.
               10  COLUMN 39   PIC X(16) SOURCE D-OUR-REF.
               10  COLUMN 57   PIC X(10) SOURCE D-DOC-DATE.
               10  COLUMN 69   PIC X(3) SOURCE D-CURRENCY.
               10  COLUMN 73   PIC Z(8)9.99- SOURCE D-GROSS.
               10  COLUMN 89   PIC Z(6)9.99- SOURCE D-DISCOUNT.
               10  COLUMN 103  PIC Z(6)9.99- SOURCE D-SETOFF.
               10  COLUMN 117  PIC Z(8)9.99- SOURCE D-PAID.
       01  CF-SUPP TYPE IS CONTROL FOOTING D-SUPPLIER.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(66) VALUE ALL "-".
               10  COLUMN 69   PIC X(64) VALUE ALL "-".
           05  LINE PLUS 1.
               10  COLUMN 39   PIC X(28) VALUE
                   "Total paid to this supplier".
               10  COLUMN 73   PIC Z(8)9.99- SUM D-GROSS.
               10  COLUMN 89   PIC Z(6)9.99- SUM D-DISCOUNT.
               10  COLUMN 103  PIC Z(6)9.99- SUM D-SETOFF.
               10  COLUMN 117  PIC Z(8)9.99- SUM D-PAID.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(56) VALUE
                   "Settlement discount has been deducted as agreed.".
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(60) VALUE
                   "Queries to the purchase ledger, quoting our ref.".
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(26) VALUE
                   "Advices printed          ".
               10  COLUMN 30   PIC Z(6)9 SOURCE WS-SUPPLIERS.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(26) VALUE
                   "Documents remitted       ".
               10  COLUMN 30   PIC Z(6)9 SOURCE T-DOCS.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(26) VALUE
                   "Total remitted           ".
               10  COLUMN 30   PIC Z(12)9.99- SOURCE T-PAID.
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
           MOVE "RUN" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-RUN-REF
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "APMGR" TO WS-USER
           END-IF
           IF WS-MODE = SPACES
               MOVE "ALL" TO WS-MODE
           END-IF
           MOVE WS-MODE TO BL-MODE
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           IF WS-RUN-REF = SPACES
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0101 APREMIT needs RUN="
           END-IF
           DISPLAY "STERP-I-0100 APREMIT remittances for "
                   FUNCTION TRIM(WS-RUN-REF).
       0100-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "APREMIT" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE "APREMIT" TO BL-JOB-NAME.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "APREMIT" TO RP-REPORT-ID
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
           INITIATE R-APREMIT.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "APREMIT" TO RPT-REPORT-ID
           MOVE "Remittance advice" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE WS-MODE TO RPT-STATUS-TEXT
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE WS-TODAY-N TO WS-DATE-N
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "Printed " WS-DATE-DD "/" WS-DATE-MM "/"
                  WS-DATE-YYYY
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           MOVE SPACES TO RPT-SUBTITLE
           STRING "Payment run " FUNCTION TRIM(WS-RUN-REF)
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
           MOVE 7 TO SQLIO-CURSOR
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
           PERFORM 1100-OPEN-ALLOCATIONS
           IF WS-SQL-FAILED = "Y"
               MOVE 12 TO WS-RC
               GO TO 1000-EXIT
           END-IF
           MOVE SPACES TO WS-CODE
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-DONE = "Y"
               PERFORM 1200-FETCH-LINE
               IF WS-DONE NOT = "Y"
                   IF D-SUPPLIER NOT = WS-CODE
                       ADD 1 TO WS-SUPPLIERS
                       MOVE D-SUPPLIER TO WS-CODE
                   END-IF
                   ADD 1 TO T-DOCS
                   ADD D-PAID TO T-PAID
                   ADD 1 TO BL-WRITTEN-COUNT
                   GENERATE DE-LINE
               END-IF
           END-PERFORM
           PERFORM 1900-CLOSE-CURSOR.
       1000-EXIT.
           EXIT.

      *>   Every document settled by this run, with its settlement
      *>   discount and any agreed set-off (REQ: AP-017) shown on its
      *>   own so the supplier can cash match the payment.
       1100-OPEN-ALLOCATIONS SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT SupplierCode, SupplierName, "
                  "InstructionRef, PaymentMethod, DocumentType, "
                  "SupplierDocRef, DocumentNumber, "
                  "ISNULL(CONVERT(varchar(10),DocumentDate,103),''), "
                  "CurrencyCode, CONVERT(varchar(30), GrossGBP), "
                  "CONVERT(varchar(30), DiscountGBP), "
                  "CONVERT(varchar(30), SetOffGBP), "
                  "CONVERT(varchar(30), PaidGBP) "
                  "FROM ap.vw_RemittanceLine "
                  "WHERE CompanyCode = ? AND ProposalRef = ? "
                  "ORDER BY SupplierCode, DocumentNumber"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-RUN-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 13 TO SQLIO-COL-CNT
           PERFORM 0290-SET-WIDTHS
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-ERROR
               MOVE "Y" TO WS-SQL-FAILED
               DISPLAY "STERP-E-1101 " SQLIO-ERRMSG
           END-IF.
       1100-EXIT.
           EXIT.

       1200-FETCH-LINE SECTION.
       1200-START.
           MOVE 7 TO SQLIO-CURSOR
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "Y" TO WS-DONE
               GO TO 1200-EXIT
           END-IF
           ADD 1 TO BL-READ-COUNT
           MOVE AP-COL(1) TO D-SUPPLIER
           MOVE AP-COL(2) TO D-SUPP-NAME
           MOVE AP-COL(3) TO D-PAY-REF
           MOVE AP-COL(4) TO D-PAY-METHOD
           MOVE AP-COL(5) TO D-DOC-TYPE
           MOVE AP-COL(6) TO D-DOC-REF
           MOVE AP-COL(7) TO D-OUR-REF
           MOVE AP-COL(8) TO D-DOC-DATE
           MOVE AP-COL(9) TO D-CURRENCY
           COMPUTE D-GROSS = FUNCTION NUMVAL(AP-COL(10))
           COMPUTE D-DISCOUNT = FUNCTION NUMVAL(AP-COL(11))
           COMPUTE D-SETOFF = FUNCTION NUMVAL(AP-COL(12))
           COMPUTE D-PAID = FUNCTION NUMVAL(AP-COL(13))
      *>   a credit note reduces the remittance
           IF D-DOC-TYPE = "CREDITNOTE"
               COMPUTE D-PAID = 0 - D-PAID
           END-IF.
       1200-EXIT.
           EXIT.

       1900-CLOSE-CURSOR SECTION.
       1900-START.
           MOVE 7 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW.
       1900-EXIT.
           EXIT.

       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-APREMIT
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
           MOVE "APREMIT" TO JL-JOB-NAME
           MOVE BL-WRITTEN-COUNT TO JL-ROWS-PROCESSED
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE BL-SPOOL-FILE TO JL-SPOOL-FILE
           MOVE WS-RC TO CA-RETURN-CODE OF WS-JOBLOG
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE WS-SUPPLIERS TO BL-COUNT-ED
           DISPLAY "STERP-I-0500 APREMIT printed " BL-COUNT-ED
                   " advices to " FUNCTION TRIM(WS-SPOOL-NAME)
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0500-EXIT.
           EXIT.
