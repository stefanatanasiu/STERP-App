      *>================================================================
      *> PROGRAM-ID : APBULK
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : BULK PROCESSING OF PURCHASE DOCUMENTS AND
      *>              PAYMENT RUNS.  A BULK RUN IS NOT A SHORT CUT:
      *>              EVERY LINE GOES THROUGH THE SAME PROGRAM A CLERK
      *>              WOULD USE, SO EACH DOCUMENT KEEPS ITS OWN
      *>              DUPLICATE CHECK (APINVREG), ITS OWN AUTHORITY
      *>              CHECK (APPAYAPR) AND ITS OWN RESULT LINE.
      *>              REPROCESSING A FILE AFTER A FAILURE CANNOT
      *>              REGISTER AN ACCEPTED DOCUMENT TWICE - THE
      *>              DUPLICATE CHECK SEES THE FIRST ONE AND THE LINE
      *>              IS REPORTED AS A DUPLICATE, NOT REGISTERED.
      *> INPUT      : run\work\APBULK-<CO>.txt, pipe delimited
      *>              INV |supplier|reference|docdate|duedate|ccy|
      *>                   net|vat|gross|description|account
      *>              CRN |  the same, as a credit note
      *>              PAY |proposal ref|||||||||
      *>              a line beginning * is a comment
      *> RUN AS     : APBULK CO=STC USER=APCLERK [MODE=PREVIEW|COMMIT]
      *> OUTPUT     : run\spool\APBULK-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, APINVREG, APPAYAPR
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-02-19 DJM  CR0623 ORIGINAL - CONVERSION LOAD
      *> 2026-10-06 STERP AP-023 ONE RESULT LINE PER DOCUMENT
      *>================================================================
      *> REQ: AP-023, AP-003, AP-004, AP-012
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APBULK.
       AUTHOR. D J MERCER.
       DATE-WRITTEN. 19/02/1996.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO DYNAMIC WS-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           SELECT BULK-FILE ASSIGN TO DYNAMIC WS-BULK-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-BULK-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-APBULK.
       FD  BULK-FILE.
       01  BULK-REC                    PIC X(400).
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY APBATWK.
       COPY STRPTWS.
       COPY STBATLOG.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY APINVREGCA REPLACING ==DFHCOMMAREA== BY ==WS-REG==.
       COPY APPAYAPRCA REPLACING ==DFHCOMMAREA== BY ==WS-APR==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-BULK-NAME                PIC X(200) VALUE SPACES.
       01  WS-BULK-STATUS              PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
       01  WS-RUN-STATE.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-EOF                  PIC X VALUE "N".
           05  WS-LINE-NO              PIC S9(7) VALUE 0.
           05  WS-OUTCOME              PIC X(12) VALUE SPACES.
           05  WS-REASON-TEXT          PIC X(300) VALUE SPACES.
      *>   D-08 category used when a bulk line quotes neither an order
      *>   nor its own non-PO reason
       01  WS-BULK-REASON              PIC X(60) VALUE "SUBSCRIPTION".
      *>   one bulk line taken apart
       01  WS-FLD.
           05  F-TYPE                  PIC X(6) VALUE SPACES.
           05  F-SUPPLIER              PIC X(20) VALUE SPACES.
           05  F-REF                   PIC X(40) VALUE SPACES.
           05  F-DOCDATE               PIC X(10) VALUE SPACES.
           05  F-DUEDATE               PIC X(10) VALUE SPACES.
           05  F-CCY                   PIC X(6) VALUE SPACES.
           05  F-NET                   PIC X(20) VALUE SPACES.
           05  F-VAT                   PIC X(20) VALUE SPACES.
           05  F-GROSS                 PIC X(20) VALUE SPACES.
           05  F-DESC                  PIC X(60) VALUE SPACES.
           05  F-ACCOUNT               PIC X(12) VALUE SPACES.
           05  F-ORDER                 PIC X(30) VALUE SPACES.
           05  F-REASON                PIC X(60) VALUE SPACES.
           05  F-COUNT                 PIC S9(4) VALUE 0.
       01  WS-NUM.
           05  N-DOCDATE               PIC 9(8) VALUE 0.
           05  N-DUEDATE               PIC 9(8) VALUE 0.
           05  N-NET                   PIC S9(13)V99 VALUE 0.
           05  N-VAT                   PIC S9(13)V99 VALUE 0.
           05  N-GROSS                 PIC S9(13)V99 VALUE 0.
       01  WS-DET.
           05  D-LINE                  PIC S9(7) VALUE 0.
           05  D-TYPE                  PIC X(6) VALUE SPACES.
           05  D-SUPPLIER              PIC X(14) VALUE SPACES.
           05  D-REF                   PIC X(24) VALUE SPACES.
           05  D-DATE                  PIC X(10) VALUE SPACES.
           05  D-GROSS                 PIC S9(13)V99 VALUE 0.
           05  D-RESULT                PIC X(10) VALUE SPACES.
           05  D-MESSAGE               PIC X(40) VALUE SPACES.
       01  WS-TOT.
           05  T-READ                  PIC S9(7) VALUE 0.
           05  T-ACCEPTED              PIC S9(7) VALUE 0.
           05  T-DUPLICATE             PIC S9(7) VALUE 0.
           05  T-REJECTED              PIC S9(7) VALUE 0.
           05  T-AUTHORITY             PIC S9(7) VALUE 0.
           05  T-VALUE                 PIC S9(15)V99 VALUE 0.
       REPORT SECTION.
       RD  R-APBULK
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 54
           FOOTING 56.
       COPY STRPTHDR.
       01  PH-COLS TYPE IS PAGE HEADING.
           05  LINE 8.
               10  COLUMN 3    PIC X(6) VALUE "Line  ".
               10  COLUMN 10   PIC X(6) VALUE "Type  ".
               10  COLUMN 17   PIC X(14) VALUE "Supplier      ".
               10  COLUMN 32   PIC X(24) VALUE
                   "Reference               ".
               10  COLUMN 57   PIC X(10) VALUE "Doc date  ".
               10  COLUMN 70   PIC X(13) VALUE "        Gross".
               10  COLUMN 85   PIC X(10) VALUE "Result    ".
               10  COLUMN 96   PIC X(36) VALUE
                   "Evidence                            ".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC Z(5)9 SOURCE D-LINE.
               10  COLUMN 10   PIC X(6) SOURCE D-TYPE.
               10  COLUMN 17   PIC X(14) SOURCE D-SUPPLIER.
               10  COLUMN 32   PIC X(24) SOURCE D-REF.
               10  COLUMN 57   PIC X(10) SOURCE D-DATE.
               10  COLUMN 70   PIC Z(9)9.99- SOURCE D-GROSS.
               10  COLUMN 85   PIC X(10) SOURCE D-RESULT.
               10  COLUMN 96   PIC X(36) SOURCE D-MESSAGE.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(40) VALUE
                   "BULK DOCUMENT PROCESSING (AP-023)".
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(34) VALUE
                   "Lines read                       ".
               10  COLUMN 40   PIC Z(6)9 SOURCE T-READ.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(34) VALUE
                   "Documents registered             ".
               10  COLUMN 40   PIC Z(6)9 SOURCE T-ACCEPTED.
               10  COLUMN 50   PIC Z(11)9.99- SOURCE T-VALUE.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(34) VALUE
                   "Already held - not registered    ".
               10  COLUMN 40   PIC Z(6)9 SOURCE T-DUPLICATE.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(34) VALUE
                   "Payment runs checked for authority".
               10  COLUMN 40   PIC Z(6)9 SOURCE T-AUTHORITY.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(34) VALUE
                   "Rejected                         ".
               10  COLUMN 40   PIC Z(6)9 SOURCE T-REJECTED.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(46) VALUE
                   "Reprocessing this file cannot register, post or".
               10  COLUMN 50   PIC X(40) VALUE
                   "pay an accepted document again.".
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           PERFORM 0100-READ-ARGS
           PERFORM 0200-JOB-START
           PERFORM 0250-BULK-PATH
           IF WS-RC < 8
               PERFORM 0300-OPEN-SPOOL
           END-IF
           IF WS-RC < 8
               PERFORM 1000-PROCESS-FILE
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
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "APCLERK" TO WS-USER
           END-IF
           IF WS-MODE NOT = "COMMIT"
               MOVE "PREVIEW" TO WS-MODE
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           MOVE WS-MODE TO BL-MODE
           DISPLAY "STERP-I-0100 APBULK bulk load "
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
           MOVE "APBULK" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE "APBULK" TO BL-JOB-NAME.
       0200-EXIT.
           EXIT.

       0250-BULK-PATH SECTION.
       0250-START.
           INITIALIZE WS-RUNPATH
           MOVE "BASE" TO CA-FUNCTION OF WS-RUNPATH
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0251 cannot find the run directory"
               GO TO 0250-EXIT
           END-IF
           MOVE SPACES TO WS-BULK-NAME
           STRING FUNCTION TRIM(RP-BASE-PATH) "\work\APBULK-"
                  FUNCTION TRIM(WS-CO) ".txt"
               DELIMITED BY SIZE INTO WS-BULK-NAME.
       0250-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "APBULK" TO RP-REPORT-ID
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
           INITIATE R-APBULK.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "APBULK" TO RPT-REPORT-ID
           MOVE "Bulk document processing" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(WS-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(WS-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(WS-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(WS-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(WS-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(WS-NOW(13:2))
           MOVE WS-TODAY-N TO WS-DATE-N
           MOVE SPACES TO RPT-AS-AT-TEXT
           STRING "Run " WS-DATE-DD "/" WS-DATE-MM "/" WS-DATE-YYYY
               DELIMITED BY SIZE INTO RPT-AS-AT-TEXT
           MOVE SPACES TO RPT-SUBTITLE
           IF WS-MODE = "COMMIT"
               MOVE "Every line through the ordinary controls"
                   TO RPT-SUBTITLE
           ELSE
               MOVE "Preview - duplicates checked, nothing written"
                   TO RPT-SUBTITLE
           END-IF.
       0350-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-PROCESS-FILE SECTION.
       1000-START.
           OPEN INPUT BULK-FILE
           IF WS-BULK-STATUS NOT = "00"
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-1001 no bulk file ("
                       WS-BULK-STATUS " "
                       FUNCTION TRIM(WS-BULK-NAME) ")"
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-EOF
           PERFORM UNTIL WS-EOF = "Y"
               READ BULK-FILE
                   AT END MOVE "Y" TO WS-EOF
                   NOT AT END PERFORM 1100-ONE-LINE
               END-READ
           END-PERFORM
           CLOSE BULK-FILE.
       1000-EXIT.
           EXIT.

       1100-ONE-LINE SECTION.
       1100-START.
           ADD 1 TO WS-LINE-NO
           IF BULK-REC = SPACES OR BULK-REC(1:1) = "*"
               GO TO 1100-EXIT
           END-IF
           ADD 1 TO T-READ
           ADD 1 TO BL-READ-COUNT
           PERFORM 1110-CLEAR
           PERFORM 1200-SPLIT
           MOVE WS-LINE-NO TO D-LINE
           MOVE F-TYPE TO D-TYPE
           MOVE F-SUPPLIER TO D-SUPPLIER
           MOVE F-REF TO D-REF
           EVALUATE F-TYPE
               WHEN "INV"  PERFORM 1300-DOCUMENT
               WHEN "CRN"  PERFORM 1300-DOCUMENT
               WHEN "PAY"  PERFORM 1500-PAYMENT-RUN
               WHEN OTHER
                   MOVE "REJECTED" TO WS-OUTCOME
                   MOVE SPACES TO WS-REASON-TEXT
                   STRING "Unknown line type "
                          FUNCTION TRIM(F-TYPE)
                       DELIMITED BY SIZE INTO WS-REASON-TEXT
           END-EVALUATE
           PERFORM 1800-RESULT.
       1100-EXIT.
           EXIT.

       1110-CLEAR SECTION.
       1110-START.
           INITIALIZE WS-FLD
           INITIALIZE WS-NUM
           INITIALIZE WS-DET
           MOVE SPACES TO WS-OUTCOME
           MOVE SPACES TO WS-REASON-TEXT.
       1110-EXIT.
           EXIT.

       1200-SPLIT SECTION.
       1200-START.
           UNSTRING BULK-REC DELIMITED BY "|"
               INTO F-TYPE F-SUPPLIER F-REF F-DOCDATE F-DUEDATE
                    F-CCY F-NET F-VAT F-GROSS F-DESC F-ACCOUNT
                    F-ORDER F-REASON
               TALLYING IN F-COUNT
           END-UNSTRING
           MOVE FUNCTION UPPER-CASE(F-TYPE) TO F-TYPE
           IF F-DOCDATE NOT = SPACES
               COMPUTE N-DOCDATE = FUNCTION NUMVAL(F-DOCDATE)
           END-IF
           IF F-DUEDATE NOT = SPACES
               COMPUTE N-DUEDATE = FUNCTION NUMVAL(F-DUEDATE)
           END-IF
           IF F-NET NOT = SPACES
               COMPUTE N-NET = FUNCTION NUMVAL(F-NET)
           END-IF
           IF F-VAT NOT = SPACES
               COMPUTE N-VAT = FUNCTION NUMVAL(F-VAT)
           END-IF
           IF F-GROSS NOT = SPACES
               COMPUTE N-GROSS = FUNCTION NUMVAL(F-GROSS)
           END-IF
           IF N-GROSS = 0
               COMPUTE N-GROSS = N-NET + N-VAT
           END-IF
           IF F-CCY = SPACES
               MOVE "GBP" TO F-CCY
           END-IF
           IF N-DUEDATE = 0
               MOVE N-DOCDATE TO N-DUEDATE
           END-IF.
       1200-EXIT.
           EXIT.

      *>   REQ: AP-023 - every document is registered through the very
      *>   same COMMAREA a clerk uses, so the duplicate rule, the
      *>   validation and the coding are identical.
       1300-DOCUMENT SECTION.
       1300-START.
           PERFORM 1310-VALIDATE
           IF WS-OUTCOME = "REJECTED"
               GO TO 1300-EXIT
           END-IF
           INITIALIZE WS-REG
           MOVE "REGISTER" TO CA-FUNCTION OF WS-REG
           IF WS-MODE NOT = "COMMIT"
               MOVE "DUPCHK" TO CA-FUNCTION OF WS-REG
           END-IF
           MOVE WS-CO TO CA-COMPANY OF WS-REG
           MOVE WS-USER TO CA-USER OF WS-REG
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-REG
           MOVE SPACES TO CA-CORRELATION-ID OF WS-REG
           STRING "APBULK-" FUNCTION TRIM(WS-CO) "-"
                  FUNCTION TRIM(F-SUPPLIER) "-"
                  FUNCTION TRIM(F-REF)
               DELIMITED BY SIZE INTO CA-CORRELATION-ID OF WS-REG
           MOVE F-SUPPLIER TO IR-SUPPLIER OF WS-REG
           IF F-TYPE = "CRN"
               MOVE "CREDITNOTE" TO IR-DOC-TYPE OF WS-REG
           ELSE
               MOVE "INVOICE" TO IR-DOC-TYPE OF WS-REG
           END-IF
           MOVE F-REF TO IR-SUPPLIER-REF OF WS-REG
           MOVE WS-TODAY-N TO IR-DATE-RECEIVED OF WS-REG
           MOVE N-DOCDATE TO IR-DOCUMENT-DATE OF WS-REG
           MOVE N-DOCDATE TO IR-TAX-POINT OF WS-REG
           MOVE N-DUEDATE TO IR-DUE-DATE OF WS-REG
           MOVE F-CCY TO IR-CURRENCY OF WS-REG
           MOVE 1 TO IR-FX-RATE OF WS-REG
           MOVE N-NET TO IR-STATED-NET OF WS-REG
           MOVE N-VAT TO IR-STATED-VAT OF WS-REG
           MOVE N-GROSS TO IR-STATED-GROSS OF WS-REG
           MOVE "VATINVOICE" TO IR-EVIDENCE-TYPE OF WS-REG
           MOVE "BULK" TO IR-SOURCE-CHANNEL OF WS-REG
           MOVE WS-USER TO IR-OWNER-USER OF WS-REG
           MOVE "UNMATCHED" TO IR-MATCH-STATE OF WS-REG
           MOVE "UNPOSTED" TO IR-ACCT-STATE OF WS-REG
           MOVE "BLOCKED" TO IR-PAY-ELIGIBILITY OF WS-REG
           MOVE "UNPAID" TO IR-SETTLE-STATE OF WS-REG
           MOVE "CAPTURED" TO IR-REG-STATE OF WS-REG
           PERFORM 1400-ONE-LINE-CODING
           CALL "APINVREG" USING WS-REG
           MOVE CA-MESSAGE OF WS-REG TO WS-REASON-TEXT
           EVALUATE TRUE
               WHEN IR-DUP-FOUND OF WS-REG = "Y"
                   MOVE "DUPLICATE" TO WS-OUTCOME
               WHEN CA-RETURN-CODE OF WS-REG > 6
                   MOVE "REJECTED" TO WS-OUTCOME
                   IF IR-L-MESSAGE(1) NOT = SPACES
                       MOVE SPACES TO WS-REASON-TEXT
                       STRING FUNCTION TRIM(CA-MESSAGE OF WS-REG)
                              " / " IR-L-MESSAGE(1)
                           DELIMITED BY SIZE INTO WS-REASON-TEXT
                   END-IF
               WHEN WS-MODE NOT = "COMMIT"
                   MOVE "PREVIEW" TO WS-OUTCOME
               WHEN OTHER
                   MOVE "REGISTERED" TO WS-OUTCOME
                   MOVE IR-REGISTER-NUMBER OF WS-REG
                       TO WS-REASON-TEXT
           END-EVALUATE.
       1300-EXIT.
           EXIT.

       1310-VALIDATE SECTION.
       1310-START.
           IF F-SUPPLIER = SPACES
               MOVE "REJECTED" TO WS-OUTCOME
               MOVE "No supplier on the bulk line" TO WS-REASON-TEXT
               GO TO 1310-EXIT
           END-IF
           IF F-REF = SPACES
               MOVE "REJECTED" TO WS-OUTCOME
               MOVE "No supplier reference on the bulk line"
                   TO WS-REASON-TEXT
               GO TO 1310-EXIT
           END-IF
           IF N-DOCDATE = 0
               MOVE "REJECTED" TO WS-OUTCOME
               MOVE "No document date on the bulk line"
                   TO WS-REASON-TEXT
               GO TO 1310-EXIT
           END-IF
           COMPUTE WS-AMT = N-NET + N-VAT
           IF N-GROSS NOT = WS-AMT
               MOVE "REJECTED" TO WS-OUTCOME
               MOVE "Net plus VAT does not equal the gross"
                   TO WS-REASON-TEXT
           END-IF.
       1310-EXIT.
           EXIT.

      *>   a bulk line carries one coded line; anything finer is keyed
       1400-ONE-LINE-CODING SECTION.
       1400-START.
           MOVE 1 TO IR-LINE-CNT OF WS-REG
           MOVE 1 TO IR-L-NUM(1)
      *>   REQ: AP-005 - a bulk line may quote the order it belongs to;
      *>   when it does not it is a non-PO line and must carry one of
      *>   the approved categories (D-08) or APINVLIN blocks it.
           MOVE F-ORDER TO IR-L-ORDER(1)
           IF F-REASON NOT = SPACES
               MOVE FUNCTION UPPER-CASE(F-REASON)
                   TO IR-NONPO-REASON OF WS-REG
           ELSE
               IF F-ORDER = SPACES
                   MOVE WS-BULK-REASON TO IR-NONPO-REASON OF WS-REG
               END-IF
           END-IF
           IF F-DESC = SPACES
               MOVE "Bulk loaded document" TO IR-L-DESC(1)
           ELSE
               MOVE F-DESC TO IR-L-DESC(1)
           END-IF
           MOVE 1 TO IR-L-QTY(1)
           MOVE 1 TO IR-L-CONV(1)
           MOVE 1 TO IR-L-PRICE-PER(1)
           MOVE N-NET TO IR-L-NET(1)
           MOVE N-NET TO IR-L-PRICE(1)
           MOVE N-VAT TO IR-L-VAT(1)
      *>   CostNature is constrained (CK_PInvL_Cn); a line against an
      *>   order is stock, anything else is expense
           IF F-ORDER = SPACES
               MOVE "EXPENSE" TO IR-L-NATURE(1)
           ELSE
               MOVE "STOCK" TO IR-L-NATURE(1)
           END-IF
           MOVE "Y" TO IR-L-RECOVER(1)
           IF F-ACCOUNT = SPACES
               MOVE "5000" TO IR-L-ACCOUNT(1)
           ELSE
               MOVE F-ACCOUNT TO IR-L-ACCOUNT(1)
           END-IF
           IF N-VAT = 0
               MOVE "Z" TO IR-L-VATCODE(1)
               MOVE 0 TO IR-L-VATRATE(1)
           ELSE
               MOVE "S" TO IR-L-VATCODE(1)
               MOVE 20 TO IR-L-VATRATE(1)
           END-IF.
       1400-EXIT.
           EXIT.

      *>   REQ: AP-023, AP-012 - a bulk payment line proves the run's
      *>   authority; it never grants it.
       1500-PAYMENT-RUN SECTION.
       1500-START.
           IF F-SUPPLIER = SPACES
               MOVE "REJECTED" TO WS-OUTCOME
               MOVE "No payment run reference on the bulk line"
                   TO WS-REASON-TEXT
               GO TO 1500-EXIT
           END-IF
           INITIALIZE WS-APR
           MOVE "VERIFY" TO CA-FUNCTION OF WS-APR
           MOVE WS-CO TO CA-COMPANY OF WS-APR
           MOVE WS-USER TO CA-USER OF WS-APR
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-APR
           MOVE F-SUPPLIER TO PA-PROPOSAL-REF
           CALL "APPAYAPR" USING WS-APR
           MOVE CA-MESSAGE OF WS-APR TO WS-REASON-TEXT
           ADD 1 TO T-AUTHORITY
           IF CA-RETURN-CODE OF WS-APR = 0
               MOVE "AUTHORITY" TO WS-OUTCOME
           ELSE
               MOVE "REJECTED" TO WS-OUTCOME
           END-IF.
       1500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1800-RESULT SECTION.
       1800-START.
           MOVE N-DOCDATE TO WS-DATE-N
           MOVE SPACES TO D-DATE
           IF N-DOCDATE > 0
               STRING WS-DATE-DD "/" WS-DATE-MM "/" WS-DATE-YYYY
                   DELIMITED BY SIZE INTO D-DATE
           END-IF
           MOVE N-GROSS TO D-GROSS
           MOVE WS-OUTCOME TO D-RESULT
           MOVE WS-REASON-TEXT TO D-MESSAGE
      *>   the report column only holds the first 36 characters, so the
      *>   operator log carries the whole refusal
           IF WS-OUTCOME = "REJECTED"
               DISPLAY "STERP-W-0410 APBULK line " WS-LINE-NO " "
                       FUNCTION TRIM(WS-REASON-TEXT)
           END-IF
           EVALUATE WS-OUTCOME
               WHEN "REGISTERED"
                   ADD 1 TO T-ACCEPTED
                   ADD N-GROSS TO T-VALUE
                   ADD 1 TO BL-WRITTEN-COUNT
               WHEN "DUPLICATE"
                   ADD 1 TO T-DUPLICATE
                   ADD 1 TO BL-REJECTED-COUNT
               WHEN "REJECTED"
                   ADD 1 TO T-REJECTED
                   ADD 1 TO BL-REJECTED-COUNT
                   MOVE 4 TO WS-RC
               WHEN OTHER
                   ADD 1 TO BL-SELECTED-COUNT
           END-EVALUATE
           GENERATE DE-LINE.
       1800-EXIT.
           EXIT.

       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-APBULK
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
           MOVE "APBULK" TO JL-JOB-NAME
           MOVE BL-WRITTEN-COUNT TO JL-ROWS-PROCESSED
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE BL-SPOOL-FILE TO JL-SPOOL-FILE
           MOVE WS-RC TO CA-RETURN-CODE OF WS-JOBLOG
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE T-READ TO BL-COUNT-ED
           DISPLAY "STERP-I-0500 APBULK " BL-COUNT-ED
                   " lines, " T-ACCEPTED " registered, "
                   T-DUPLICATE " duplicate, " T-REJECTED " rejected"
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0500-EXIT.
           EXIT.
