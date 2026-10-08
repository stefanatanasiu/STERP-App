      *>================================================================
      *> PROGRAM-ID : APEINV
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : RECEIVES STRUCTURED SUPPLIER E-INVOICES (PEPPOL
      *>              BIS BILLING 3.0 UBL, DECISION D-26) FROM THE
      *>              ACCESS POINT DROP FOLDER run\einvoice\in INTO
      *>              THE PURCHASE INVOICE REGISTER.  EVERY DOCUMENT
      *>              GOES THROUGH APINVREG - THE SAME COMMAREA, THE
      *>              SAME DUPLICATE, VALIDATION AND CODING RULES AS A
      *>              KEYED PAPER INVOICE - SO A STRUCTURED INVOICE
      *>              AND A PDF OF THE SAME DOCUMENT ARE RECOGNISED AS
      *>              ONE DOCUMENT (AC-125).
      *>              EACH FILE IS LOGGED IN ap.EInvoiceInbox, GETS AN
      *>              ACCEPTED OR REJECTED RESULT LINE, AND IS MOVED
      *>              TO einvoice\processed OR einvoice\rejected SO A
      *>              RESTART CANNOT READ IT TWICE.
      *> RUN AS     : APEINV CO=STC USER=APCLERK [MODE=PREVIEW|COMMIT]
      *> OUTPUT     : run\spool\APEINV-<CO>-<stamp>.lst
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, APXMLGET, APINVREG,
      *>              SQLIO, SYSTEM
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-07-14 DJM  CR0815 ORIGINAL - EDIFACT INVOIC INBOUND
      *> 2026-10-06 STERP AP-031 UBL REPLACES THE EDIFACT TRANSLATOR
      *>================================================================
      *> REQ: AP-031, AP-003, AP-004, AP-023
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APEINV.
       AUTHOR. D J MERCER.
       DATE-WRITTEN. 14/07/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO DYNAMIC WS-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           SELECT LIST-FILE ASSIGN TO DYNAMIC WS-LIST-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LIST-STATUS.
           SELECT XML-FILE ASSIGN TO DYNAMIC WS-XML-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-XML-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-APEINV.
       FD  LIST-FILE.
       01  LIST-REC                    PIC X(200).
       FD  XML-FILE.
       01  XML-REC                     PIC X(300).
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY APBATWK.
       COPY STRPTWS.
       COPY STBATLOG.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       COPY APXMLGTC REPLACING ==DFHCOMMAREA== BY ==WS-XML==.
       COPY APINVREGCA REPLACING ==DFHCOMMAREA== BY ==WS-REG==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-LIST-NAME                PIC X(200) VALUE SPACES.
       01  WS-LIST-STATUS              PIC X(2) VALUE "00".
       01  WS-XML-NAME                 PIC X(200) VALUE SPACES.
       01  WS-XML-STATUS               PIC X(2) VALUE "00".
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-MODE                 PIC X(10) VALUE SPACES.
       01  WS-FOLDERS.
           05  WS-IN-DIR               PIC X(200) VALUE SPACES.
           05  WS-PROC-DIR             PIC X(200) VALUE SPACES.
           05  WS-REJ-DIR              PIC X(200) VALUE SPACES.
           05  WS-CMD                  PIC X(500) VALUE SPACES.
           05  WS-SYS-RC               PIC S9(9) COMP-5 VALUE 0.
       01  WS-RUN-STATE.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
           05  WS-FILE-NAME            PIC X(200) VALUE SPACES.
           05  WS-NAME-LEN             PIC S9(4) VALUE 0.
           05  WS-EOF                  PIC X VALUE "N".
           05  WS-XML-EOF              PIC X VALUE "N".
           05  WS-OUTCOME              PIC X(12) VALUE SPACES.
           05  WS-REASON-TEXT          PIC X(300) VALUE SPACES.
           05  WS-DEST                 PIC X(200) VALUE SPACES.
      *>   a very small context stack - the UBL containers we care
      *>   about are never nested more than a few deep
       01  WS-CTX-STACK.
           05  WS-CTX-DEPTH            PIC S9(4) VALUE 0.
           05  WS-CTX OCCURS 12 TIMES  PIC X(40).
       01  WS-CTX-NOW                  PIC X(40) VALUE SPACES.
       01  WS-DOC.
           05  W-SUPP-CODE             PIC X(20) VALUE SPACES.
           05  W-SUPP-NAME             PIC X(60) VALUE SPACES.
           05  W-SUPP-VAT              PIC X(15) VALUE SPACES.
           05  W-ENDPOINT              PIC X(60) VALUE SPACES.
           05  W-DOC-REF               PIC X(40) VALUE SPACES.
           05  W-DOC-TYPE              PIC X(12) VALUE SPACES.
           05  W-TYPE-CODE             PIC X(6) VALUE SPACES.
           05  W-DOC-DATE              PIC 9(8) VALUE 0.
           05  W-DUE-DATE              PIC 9(8) VALUE 0.
           05  W-TAX-POINT             PIC 9(8) VALUE 0.
           05  W-CURRENCY              PIC X(3) VALUE SPACES.
           05  W-ORDER-REF             PIC X(30) VALUE SPACES.
           05  W-BUYER-REF             PIC X(30) VALUE SPACES.
           05  W-NET                   PIC S9(13)V99 VALUE 0.
           05  W-VAT                   PIC S9(13)V99 VALUE 0.
           05  W-GROSS                 PIC S9(13)V99 VALUE 0.
           05  W-LINE-CNT              PIC S9(4) VALUE 0.
           05  W-L-DESC                PIC X(60) VALUE SPACES.
           05  W-L-QTY                 PIC S9(11)V9(6) VALUE 0.
           05  W-L-NET                 PIC S9(11)V99 VALUE 0.
           05  W-L-RATE                PIC S9(5)V9(4) VALUE 0.
           05  W-L-ORDER               PIC X(30) VALUE SPACES.
           05  W-L-ITEM                PIC X(40) VALUE SPACES.
           05  W-L-OPEN                PIC X VALUE "N".
       01  WS-DET.
           05  D-FILE                  PIC X(30) VALUE SPACES.
           05  D-SUPPLIER              PIC X(14) VALUE SPACES.
           05  D-REF                   PIC X(22) VALUE SPACES.
           05  D-DATE                  PIC X(10) VALUE SPACES.
           05  D-LINES                 PIC S9(4) VALUE 0.
           05  D-NET                   PIC S9(13)V99 VALUE 0.
           05  D-VAT                   PIC S9(13)V99 VALUE 0.
           05  D-GROSS                 PIC S9(13)V99 VALUE 0.
           05  D-RESULT                PIC X(10) VALUE SPACES.
           05  D-MESSAGE               PIC X(60) VALUE SPACES.
       01  WS-TOT.
           05  T-FILES                 PIC S9(7) VALUE 0.
           05  T-ACCEPTED              PIC S9(7) VALUE 0.
           05  T-DUPLICATE             PIC S9(7) VALUE 0.
           05  T-REJECTED              PIC S9(7) VALUE 0.
           05  T-GROSS                 PIC S9(15)V99 VALUE 0.
       REPORT SECTION.
       RD  R-APEINV
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 9 LAST DETAIL 54
           FOOTING 56.
       COPY STRPTHDR.
       01  PH-COLS TYPE IS PAGE HEADING.
           05  LINE 8.
               10  COLUMN 3    PIC X(30) VALUE
                   "File                         ".
               10  COLUMN 34   PIC X(14) VALUE "Supplier      ".
               10  COLUMN 49   PIC X(22) VALUE "Reference             ".
               10  COLUMN 72   PIC X(10) VALUE "Doc date  ".
               10  COLUMN 83   PIC X(5) VALUE "Lines".
               10  COLUMN 89   PIC X(13) VALUE "        Gross".
               10  COLUMN 103  PIC X(10) VALUE "Result    ".
               10  COLUMN 114  PIC X(18) VALUE "Message           ".
       01  DE-LINE TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 3    PIC X(30) SOURCE D-FILE.
               10  COLUMN 34   PIC X(14) SOURCE D-SUPPLIER.
               10  COLUMN 49   PIC X(22) SOURCE D-REF.
               10  COLUMN 72   PIC X(10) SOURCE D-DATE.
               10  COLUMN 83   PIC Z(3)9 SOURCE D-LINES.
               10  COLUMN 89   PIC Z(9)9.99- SOURCE D-GROSS.
               10  COLUMN 103  PIC X(10) SOURCE D-RESULT.
               10  COLUMN 114  PIC X(18) SOURCE D-MESSAGE.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(40) VALUE
                   "STRUCTURED E-INVOICE RECEIPT (AP-031)".
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(34) VALUE
                   "Documents received               ".
               10  COLUMN 40   PIC Z(6)9 SOURCE T-FILES.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(34) VALUE
                   "Registered                       ".
               10  COLUMN 40   PIC Z(6)9 SOURCE T-ACCEPTED.
               10  COLUMN 50   PIC Z(11)9.99- SOURCE T-GROSS.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(34) VALUE
                   "Duplicates of a document we hold ".
               10  COLUMN 40   PIC Z(6)9 SOURCE T-DUPLICATE.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(34) VALUE
                   "Rejected                         ".
               10  COLUMN 40   PIC Z(6)9 SOURCE T-REJECTED.
           05  LINE PLUS 2.
               10  COLUMN 3    PIC X(46) VALUE
                   "A structured invoice and a PDF of the same     ".
               10  COLUMN 50   PIC X(40) VALUE
                   "document are one document, not two.     ".
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           PERFORM 0100-READ-ARGS
           PERFORM 0200-JOB-START
           PERFORM 0250-BUILD-FOLDERS
           IF WS-RC < 8
               PERFORM 0300-OPEN-SPOOL
           END-IF
           IF WS-RC < 8
               PERFORM 1000-PROCESS-INBOX
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
           IF WS-MODE NOT = "PREVIEW"
               MOVE "COMMIT" TO WS-MODE
           END-IF
           MOVE FUNCTION CURRENT-DATE TO WS-NOW
           COMPUTE WS-TODAY-N = FUNCTION NUMVAL(WS-NOW(1:8))
           MOVE WS-MODE TO BL-MODE
           DISPLAY "STERP-I-0100 APEINV e-invoice receipt "
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
           MOVE "APEINV" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE "APEINV" TO BL-JOB-NAME.
       0200-EXIT.
           EXIT.

      *>   the access point drops files into run\einvoice\in; the
      *>   operator never sees the folders, so we make them
       0250-BUILD-FOLDERS SECTION.
       0250-START.
           INITIALIZE WS-RUNPATH
           MOVE "BASE" TO CA-FUNCTION OF WS-RUNPATH
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0251 cannot find the run directory"
               GO TO 0250-EXIT
           END-IF
           MOVE SPACES TO WS-IN-DIR
           STRING FUNCTION TRIM(RP-BASE-PATH) "\einvoice\in"
               DELIMITED BY SIZE INTO WS-IN-DIR
           MOVE SPACES TO WS-PROC-DIR
           STRING FUNCTION TRIM(RP-BASE-PATH) "\einvoice\processed"
               DELIMITED BY SIZE INTO WS-PROC-DIR
           MOVE SPACES TO WS-REJ-DIR
           STRING FUNCTION TRIM(RP-BASE-PATH) "\einvoice\rejected"
               DELIMITED BY SIZE INTO WS-REJ-DIR
           MOVE SPACES TO WS-LIST-NAME
           STRING FUNCTION TRIM(RP-BASE-PATH) "\work\APEINV-INBOX.txt"
               DELIMITED BY SIZE INTO WS-LIST-NAME
           PERFORM 0260-MAKE-DIR-IN
           PERFORM 0270-LIST-INBOX.
       0250-EXIT.
           EXIT.

       0260-MAKE-DIR-IN SECTION.
       0260-START.
           MOVE SPACES TO WS-CMD
           STRING "cmd /c (if not exist """
                  FUNCTION TRIM(WS-IN-DIR) """ mkdir """
                  FUNCTION TRIM(WS-IN-DIR) """) & (if not exist """
                  FUNCTION TRIM(WS-PROC-DIR) """ mkdir """
                  FUNCTION TRIM(WS-PROC-DIR) """) & (if not exist """
                  FUNCTION TRIM(WS-REJ-DIR) """ mkdir """
                  FUNCTION TRIM(WS-REJ-DIR) """)"
               DELIMITED BY SIZE INTO WS-CMD
           CALL "SYSTEM" USING WS-CMD.
       0260-EXIT.
           EXIT.

      *>   there is no directory read in this dialect, so the shell
      *>   writes the inbox listing - see docs\tech-debt\ap.md
       0270-LIST-INBOX SECTION.
       0270-START.
           MOVE SPACES TO WS-CMD
           STRING "cmd /c dir /b /a-d """
                  FUNCTION TRIM(WS-IN-DIR) "\*.xml"" > """
                  FUNCTION TRIM(WS-LIST-NAME) """ 2>nul"
               DELIMITED BY SIZE INTO WS-CMD
           CALL "SYSTEM" USING WS-CMD.
       0270-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "APEINV" TO RP-REPORT-ID
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
           INITIATE R-APEINV.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "APEINV" TO RPT-REPORT-ID
           MOVE "Structured e-invoice receipt" TO RPT-TITLE
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
               MOVE "Peppol BIS Billing 3.0 - registered and filed"
                   TO RPT-SUBTITLE
           ELSE
               MOVE "Preview - nothing registered, nothing moved"
                   TO RPT-SUBTITLE
           END-IF.
       0350-EXIT.
           EXIT.

       0290-SET-WIDTHS SECTION.
       0290-START.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0290-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-PROCESS-INBOX SECTION.
       1000-START.
           OPEN INPUT LIST-FILE
           IF WS-LIST-STATUS NOT = "00"
               DISPLAY "STERP-W-1001 the e-invoice inbox is empty ("
                       WS-LIST-STATUS " "
                       FUNCTION TRIM(WS-LIST-NAME) ")"
               GO TO 1000-EXIT
           END-IF
           MOVE "N" TO WS-EOF
           PERFORM UNTIL WS-EOF = "Y"
               READ LIST-FILE INTO WS-FILE-NAME
                   AT END MOVE "Y" TO WS-EOF
                   NOT AT END PERFORM 1100-ONE-DOCUMENT
               END-READ
           END-PERFORM
           CLOSE LIST-FILE.
       1000-EXIT.
           EXIT.

       1100-ONE-DOCUMENT SECTION.
       1100-START.
           IF WS-FILE-NAME = SPACES
               GO TO 1100-EXIT
           END-IF
           ADD 1 TO T-FILES
           ADD 1 TO BL-READ-COUNT
           PERFORM 1110-CLEAR-DOCUMENT
           MOVE WS-FILE-NAME TO D-FILE
           MOVE SPACES TO WS-XML-NAME
           STRING FUNCTION TRIM(WS-IN-DIR) "\"
                  FUNCTION TRIM(WS-FILE-NAME)
               DELIMITED BY SIZE INTO WS-XML-NAME
           PERFORM 1200-PARSE-DOCUMENT
           IF WS-OUTCOME = "REJECTED"
               PERFORM 1800-RESULT
               GO TO 1100-EXIT
           END-IF
           PERFORM 1300-CHECK-DOCUMENT
           IF WS-OUTCOME = "REJECTED"
               PERFORM 1800-RESULT
               GO TO 1100-EXIT
           END-IF
           PERFORM 1400-REGISTER
           PERFORM 1800-RESULT.
       1100-EXIT.
           EXIT.

       1110-CLEAR-DOCUMENT SECTION.
       1110-START.
           INITIALIZE WS-DOC
           INITIALIZE WS-DET
      *>   the register COMMAREA is reused for every document in the
      *>   run; a stale IR-INVOICE-ID makes APINVREG's duplicate check
      *>   exclude the very row it is looking for (AC-125)
           INITIALIZE WS-REG
           MOVE 0 TO WS-CTX-DEPTH
           MOVE SPACES TO WS-OUTCOME
           MOVE SPACES TO WS-REASON-TEXT
           MOVE "INVOICE" TO W-DOC-TYPE
           MOVE "GBP" TO W-CURRENCY.
       1110-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   REQ: AP-031 - a line at a time through APXMLGET, keeping a
      *>   stack of the cac: containers we are inside.
       1200-PARSE-DOCUMENT SECTION.
       1200-START.
           OPEN INPUT XML-FILE
           IF WS-XML-STATUS NOT = "00"
               MOVE "REJECTED" TO WS-OUTCOME
               MOVE "The file cannot be read" TO WS-REASON-TEXT
               GO TO 1200-EXIT
           END-IF
           MOVE "N" TO WS-XML-EOF
           PERFORM UNTIL WS-XML-EOF = "Y"
               READ XML-FILE
                   AT END MOVE "Y" TO WS-XML-EOF
                   NOT AT END PERFORM 1210-ONE-RECORD
               END-READ
           END-PERFORM
           CLOSE XML-FILE.
       1200-EXIT.
           EXIT.

       1210-ONE-RECORD SECTION.
       1210-START.
           MOVE XML-REC TO XG-RECORD OF WS-XML
           MOVE "ELEMENT" TO XG-FUNC OF WS-XML
           CALL "APXMLGET" USING WS-XML
           IF XG-RC OF WS-XML NOT = 0
               GO TO 1210-EXIT
           END-IF
           IF XG-IS-CLOSE OF WS-XML = "Y"
               PERFORM 1220-POP-CONTEXT
               GO TO 1210-EXIT
           END-IF
           IF XG-IS-OPEN OF WS-XML = "Y"
               PERFORM 1230-PUSH-CONTEXT
               GO TO 1210-EXIT
           END-IF
           IF XG-IS-EMPTY OF WS-XML = "Y"
               GO TO 1210-EXIT
           END-IF
           PERFORM 1240-TAKE-VALUE.
       1210-EXIT.
           EXIT.

       1220-POP-CONTEXT SECTION.
       1220-START.
           IF WS-CTX-DEPTH > 0
               IF WS-CTX(WS-CTX-DEPTH) = "cac:InvoiceLine"
                  OR WS-CTX(WS-CTX-DEPTH) = "cac:CreditNoteLine"
                   PERFORM 1500-CLOSE-LINE
               END-IF
               SUBTRACT 1 FROM WS-CTX-DEPTH
           END-IF
           IF WS-CTX-DEPTH > 0
               MOVE WS-CTX(WS-CTX-DEPTH) TO WS-CTX-NOW
           ELSE
               MOVE SPACES TO WS-CTX-NOW
           END-IF.
       1220-EXIT.
           EXIT.

       1230-PUSH-CONTEXT SECTION.
       1230-START.
           IF WS-CTX-DEPTH < 12
               ADD 1 TO WS-CTX-DEPTH
               MOVE XG-TAG-FOUND OF WS-XML TO WS-CTX(WS-CTX-DEPTH)
               MOVE XG-TAG-FOUND OF WS-XML TO WS-CTX-NOW
           END-IF
           IF WS-CTX-NOW = "cac:InvoiceLine"
              OR WS-CTX-NOW = "cac:CreditNoteLine"
               MOVE "Y" TO W-L-OPEN
               MOVE SPACES TO W-L-DESC
               MOVE SPACES TO W-L-ITEM
      *>       A UBL order reference is carried on the document, not on
      *>       the line, so every line inherits it; without it the line
      *>       is non-PO and APINVLIN blocks it (APL0005).
               MOVE W-ORDER-REF TO W-L-ORDER
               MOVE 0 TO W-L-QTY
               MOVE 0 TO W-L-NET
               MOVE 0 TO W-L-RATE
           END-IF.
       1230-EXIT.
           EXIT.

      *>   the same element name means different things in different
      *>   containers, which is why the stack is kept
       1240-TAKE-VALUE SECTION.
       1240-START.
           EVALUATE TRUE
               WHEN W-L-OPEN = "Y"
                   PERFORM 1280-LINE-LEVEL
               WHEN WS-CTX-NOW = "cac:AccountingSupplierParty"
                   PERFORM 1260-SUPPLIER
               WHEN WS-CTX-NOW = "cac:Party"
                   PERFORM 1260-SUPPLIER
               WHEN WS-CTX-NOW = "cac:PartyLegalEntity"
                   PERFORM 1260-SUPPLIER
               WHEN WS-CTX-NOW = "cac:PartyTaxScheme"
                   PERFORM 1260-SUPPLIER
               WHEN WS-CTX-NOW = "cac:PartyIdentification"
                   PERFORM 1260-SUPPLIER
               WHEN WS-CTX-NOW = "cac:PartyName"
                   PERFORM 1260-SUPPLIER
               WHEN WS-CTX-NOW = "cac:OrderReference"
                   IF XG-TAG-FOUND OF WS-XML = "cbc:ID"
                       MOVE XG-VALUE OF WS-XML TO W-ORDER-REF
                       MOVE XG-VALUE OF WS-XML TO W-L-ORDER
                   END-IF
               WHEN WS-CTX-NOW = "cac:LegalMonetaryTotal"
                   PERFORM 1270-TOTALS
               WHEN WS-CTX-NOW = "cac:TaxTotal"
                   IF XG-TAG-FOUND OF WS-XML = "cbc:TaxAmount"
                       PERFORM 1290-NUMBER
                       MOVE XG-NUMERIC OF WS-XML TO W-VAT
                   END-IF
               WHEN WS-CTX-NOW = "cac:TaxSubtotal"
                   CONTINUE
               WHEN WS-CTX-NOW = "cac:PostalAddress"
                   CONTINUE
               WHEN WS-CTX-NOW = "cac:Country"
                   CONTINUE
               WHEN WS-CTX-NOW = "cac:TaxScheme"
                   CONTINUE
               WHEN WS-CTX-NOW = "cac:AccountingCustomerParty"
                   CONTINUE
               WHEN WS-CTX-DEPTH > 1
                   CONTINUE
               WHEN OTHER
                   PERFORM 1250-DOCUMENT-LEVEL
           END-EVALUATE.
       1240-EXIT.
           EXIT.

       1250-DOCUMENT-LEVEL SECTION.
       1250-START.
           EVALUATE XG-TAG-FOUND OF WS-XML
               WHEN "cbc:ID"
                   MOVE XG-VALUE OF WS-XML TO W-DOC-REF
               WHEN "cbc:IssueDate"
                   PERFORM 1295-DATE
                   MOVE XG-DATE OF WS-XML TO W-DOC-DATE
               WHEN "cbc:DueDate"
                   PERFORM 1295-DATE
                   MOVE XG-DATE OF WS-XML TO W-DUE-DATE
               WHEN "cbc:TaxPointDate"
                   PERFORM 1295-DATE
                   MOVE XG-DATE OF WS-XML TO W-TAX-POINT
               WHEN "cbc:InvoiceTypeCode"
                   MOVE XG-VALUE OF WS-XML TO W-TYPE-CODE
                   MOVE "INVOICE" TO W-DOC-TYPE
               WHEN "cbc:CreditNoteTypeCode"
                   MOVE XG-VALUE OF WS-XML TO W-TYPE-CODE
                   MOVE "CREDITNOTE" TO W-DOC-TYPE
               WHEN "cbc:DocumentCurrencyCode"
                   MOVE XG-VALUE OF WS-XML TO W-CURRENCY
               WHEN "cbc:BuyerReference"
                   MOVE XG-VALUE OF WS-XML TO W-BUYER-REF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
       1250-EXIT.
           EXIT.

       1260-SUPPLIER SECTION.
       1260-START.
           EVALUATE XG-TAG-FOUND OF WS-XML
               WHEN "cbc:EndpointID"
                   MOVE XG-VALUE OF WS-XML TO W-ENDPOINT
               WHEN "cbc:CompanyID"
                   IF W-SUPP-VAT = SPACES
                       MOVE XG-VALUE OF WS-XML TO W-SUPP-VAT
                   END-IF
               WHEN "cbc:ID"
                   IF W-SUPP-CODE = SPACES
                       MOVE XG-VALUE OF WS-XML TO W-SUPP-CODE
                   END-IF
               WHEN "cbc:Name"
                   IF W-SUPP-NAME = SPACES
                       MOVE XG-VALUE OF WS-XML TO W-SUPP-NAME
                   END-IF
               WHEN "cbc:RegistrationName"
                   MOVE XG-VALUE OF WS-XML TO W-SUPP-NAME
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
       1260-EXIT.
           EXIT.

       1270-TOTALS SECTION.
       1270-START.
           EVALUATE XG-TAG-FOUND OF WS-XML
               WHEN "cbc:TaxExclusiveAmount"
                   PERFORM 1290-NUMBER
                   MOVE XG-NUMERIC OF WS-XML TO W-NET
               WHEN "cbc:TaxInclusiveAmount"
                   PERFORM 1290-NUMBER
                   MOVE XG-NUMERIC OF WS-XML TO W-GROSS
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
       1270-EXIT.
           EXIT.

       1280-LINE-LEVEL SECTION.
       1280-START.
           EVALUATE XG-TAG-FOUND OF WS-XML
               WHEN "cbc:InvoicedQuantity"
                   PERFORM 1290-NUMBER
                   MOVE XG-NUMERIC OF WS-XML TO W-L-QTY
               WHEN "cbc:CreditedQuantity"
                   PERFORM 1290-NUMBER
                   MOVE XG-NUMERIC OF WS-XML TO W-L-QTY
               WHEN "cbc:LineExtensionAmount"
                   PERFORM 1290-NUMBER
                   MOVE XG-NUMERIC OF WS-XML TO W-L-NET
               WHEN "cbc:Name"
                   MOVE XG-VALUE OF WS-XML TO W-L-DESC
               WHEN "cbc:Percent"
                   PERFORM 1290-NUMBER
                   MOVE XG-NUMERIC OF WS-XML TO W-L-RATE
               WHEN "cbc:ID"
                   IF W-L-ITEM = SPACES
                       MOVE XG-VALUE OF WS-XML TO W-L-ITEM
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
       1280-EXIT.
           EXIT.

       1290-NUMBER SECTION.
       1290-START.
           MOVE "NUM" TO XG-FUNC OF WS-XML
           CALL "APXMLGET" USING WS-XML
           MOVE "ELEMENT" TO XG-FUNC OF WS-XML.
       1290-EXIT.
           EXIT.

       1295-DATE SECTION.
       1295-START.
           MOVE "DATE" TO XG-FUNC OF WS-XML
           CALL "APXMLGET" USING WS-XML
           MOVE "ELEMENT" TO XG-FUNC OF WS-XML.
       1295-EXIT.
           EXIT.

      *>   a line is finished when its container closes
       1500-CLOSE-LINE SECTION.
       1500-START.
           IF W-L-OPEN NOT = "Y"
               GO TO 1500-EXIT
           END-IF
           MOVE "N" TO W-L-OPEN
           IF W-LINE-CNT NOT < 40
               GO TO 1500-EXIT
           END-IF
           ADD 1 TO W-LINE-CNT
           MOVE W-LINE-CNT TO IR-L-NUM(W-LINE-CNT)
           MOVE W-L-DESC TO IR-L-DESC(W-LINE-CNT)
           MOVE W-L-ITEM TO IR-L-ITEM(W-LINE-CNT)
           MOVE W-L-ORDER TO IR-L-ORDER(W-LINE-CNT)
           MOVE W-L-QTY TO IR-L-QTY(W-LINE-CNT)
           MOVE W-L-NET TO IR-L-NET(W-LINE-CNT)
           MOVE W-L-RATE TO IR-L-VATRATE(W-LINE-CNT)
           MOVE 1 TO IR-L-CONV(W-LINE-CNT)
           MOVE 1 TO IR-L-PRICE-PER(W-LINE-CNT)
           IF W-L-QTY NOT = 0
               COMPUTE IR-L-PRICE(W-LINE-CNT) ROUNDED =
                   W-L-NET / W-L-QTY
           END-IF
           EVALUATE W-L-RATE
               WHEN 20 MOVE "S" TO IR-L-VATCODE(W-LINE-CNT)
               WHEN 5  MOVE "R" TO IR-L-VATCODE(W-LINE-CNT)
               WHEN 0  MOVE "Z" TO IR-L-VATCODE(W-LINE-CNT)
               WHEN OTHER
                   MOVE "S" TO IR-L-VATCODE(W-LINE-CNT)
           END-EVALUATE
           MOVE "Y" TO IR-L-RECOVER(W-LINE-CNT)
      *>   CostNature is constrained (CK_PInvL_Cn); a UBL line quoting
      *>   the document order reference is stock, anything else expense
           IF W-L-ORDER = SPACES
               MOVE "EXPENSE" TO IR-L-NATURE(W-LINE-CNT)
           ELSE
               MOVE "STOCK" TO IR-L-NATURE(W-LINE-CNT)
           END-IF
           MOVE "5000" TO IR-L-ACCOUNT(W-LINE-CNT).
       1500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   REQ: AP-031 - the structural checks the access point does
      *>   not do for us
       1300-CHECK-DOCUMENT SECTION.
       1300-START.
           IF W-DOC-REF = SPACES
               MOVE "REJECTED" TO WS-OUTCOME
               MOVE "No invoice number in the document"
                   TO WS-REASON-TEXT
               GO TO 1300-EXIT
           END-IF
           IF W-SUPP-CODE = SPACES
               MOVE "REJECTED" TO WS-OUTCOME
               MOVE "No supplier identifier in the document"
                   TO WS-REASON-TEXT
               GO TO 1300-EXIT
           END-IF
           IF W-DOC-DATE = 0
               MOVE "REJECTED" TO WS-OUTCOME
               MOVE "No issue date in the document" TO WS-REASON-TEXT
               GO TO 1300-EXIT
           END-IF
           IF W-LINE-CNT = 0
               MOVE "REJECTED" TO WS-OUTCOME
               MOVE "The document has no invoice lines"
                   TO WS-REASON-TEXT
               GO TO 1300-EXIT
           END-IF
           IF W-TAX-POINT = 0
               MOVE W-DOC-DATE TO W-TAX-POINT
           END-IF
           IF W-DUE-DATE = 0
               MOVE W-DOC-DATE TO W-DUE-DATE
           END-IF
           IF W-GROSS = 0
               COMPUTE W-GROSS = W-NET + W-VAT
           END-IF
           COMPUTE WS-AMT = W-NET + W-VAT
           IF W-GROSS NOT = WS-AMT
               MOVE "REJECTED" TO WS-OUTCOME
               MOVE SPACES TO WS-REASON-TEXT
               MOVE W-GROSS TO WS-MONEY-Z
               STRING "Stated totals do not add up to "
                      FUNCTION TRIM(WS-MONEY-Z)
                   DELIMITED BY SIZE INTO WS-REASON-TEXT
           END-IF.
       1300-EXIT.
           EXIT.

      *>   REQ: AP-031, AP-023 - the register is entered through the
      *>   ordinary APINVREG COMMAREA, so the duplicate, validation
      *>   and coding rules are exactly those of a keyed invoice.
       1400-REGISTER SECTION.
       1400-START.
           MOVE "REGISTER" TO CA-FUNCTION OF WS-REG
           MOVE WS-CO TO CA-COMPANY OF WS-REG
           MOVE WS-USER TO CA-USER OF WS-REG
           MOVE WS-TODAY-N TO CA-BUSINESS-DATE OF WS-REG
           MOVE SPACES TO CA-CORRELATION-ID OF WS-REG
           STRING "APEINV-" FUNCTION TRIM(WS-FILE-NAME)
               DELIMITED BY SIZE INTO CA-CORRELATION-ID OF WS-REG
           MOVE W-SUPP-CODE TO IR-SUPPLIER OF WS-REG
           MOVE W-DOC-TYPE TO IR-DOC-TYPE OF WS-REG
           MOVE W-DOC-REF TO IR-SUPPLIER-REF OF WS-REG
           MOVE WS-TODAY-N TO IR-DATE-RECEIVED OF WS-REG
           MOVE W-DOC-DATE TO IR-DOCUMENT-DATE OF WS-REG
           MOVE W-TAX-POINT TO IR-TAX-POINT OF WS-REG
           MOVE W-DUE-DATE TO IR-DUE-DATE OF WS-REG
           MOVE W-CURRENCY TO IR-CURRENCY OF WS-REG
           MOVE 1 TO IR-FX-RATE OF WS-REG
           MOVE W-NET TO IR-STATED-NET OF WS-REG
           MOVE W-VAT TO IR-STATED-VAT OF WS-REG
           MOVE W-GROSS TO IR-STATED-GROSS OF WS-REG
           MOVE W-SUPP-VAT TO IR-SUPPLIER-VATNO OF WS-REG
           MOVE "VATINVOICE" TO IR-EVIDENCE-TYPE OF WS-REG
           MOVE "EINVOICE" TO IR-SOURCE-CHANNEL OF WS-REG
           MOVE WS-FILE-NAME TO IR-ATTACHMENT-REF OF WS-REG
           MOVE W-ENDPOINT TO IR-EINVOICE-REF OF WS-REG
           MOVE WS-USER TO IR-OWNER-USER OF WS-REG
           MOVE W-LINE-CNT TO IR-LINE-CNT OF WS-REG
           MOVE "UNMATCHED" TO IR-MATCH-STATE OF WS-REG
           MOVE "UNPOSTED" TO IR-ACCT-STATE OF WS-REG
           MOVE "BLOCKED" TO IR-PAY-ELIGIBILITY OF WS-REG
           MOVE "UNPAID" TO IR-SETTLE-STATE OF WS-REG
           MOVE "CAPTURED" TO IR-REG-STATE OF WS-REG
           IF WS-MODE NOT = "COMMIT"
               MOVE "DUPCHK" TO CA-FUNCTION OF WS-REG
           END-IF
           CALL "APINVREG" USING WS-REG
           MOVE CA-MESSAGE OF WS-REG TO WS-REASON-TEXT
           IF CA-RETURN-CODE OF WS-REG > 4
               DISPLAY "STERP-W-1401 " FUNCTION TRIM(WS-FILE-NAME)
                       " " FUNCTION TRIM(WS-REASON-TEXT)
           END-IF
           EVALUATE TRUE
               WHEN IR-DUP-FOUND OF WS-REG = "Y"
                   MOVE "DUPLICATE" TO WS-OUTCOME
               WHEN CA-RETURN-CODE OF WS-REG > 6
                   MOVE "REJECTED" TO WS-OUTCOME
               WHEN WS-MODE NOT = "COMMIT"
                   MOVE "PREVIEW" TO WS-OUTCOME
               WHEN OTHER
                   MOVE "REGISTERED" TO WS-OUTCOME
           END-EVALUATE.
       1400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1800-RESULT SECTION.
       1800-START.
           MOVE W-SUPP-CODE TO D-SUPPLIER
           MOVE W-DOC-REF TO D-REF
           MOVE W-DOC-DATE TO WS-DATE-N
           MOVE SPACES TO D-DATE
           IF W-DOC-DATE > 0
               STRING WS-DATE-DD "/" WS-DATE-MM "/" WS-DATE-YYYY
                   DELIMITED BY SIZE INTO D-DATE
           END-IF
           MOVE W-LINE-CNT TO D-LINES
           MOVE W-NET TO D-NET
           MOVE W-VAT TO D-VAT
           MOVE W-GROSS TO D-GROSS
           MOVE WS-OUTCOME TO D-RESULT
           MOVE WS-REASON-TEXT TO D-MESSAGE
           EVALUATE WS-OUTCOME
               WHEN "REGISTERED"
                   ADD 1 TO T-ACCEPTED
                   ADD W-GROSS TO T-GROSS
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
           IF WS-MODE = "COMMIT"
               PERFORM 1810-LOG-INBOX
               PERFORM 1820-MOVE-FILE
           END-IF
           GENERATE DE-LINE.
       1800-EXIT.
           EXIT.

      *>   the inbox row is the receipt evidence; the unique key on
      *>   company and file name is what stops a replay
       1810-LOG-INBOX SECTION.
       1810-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "IF NOT EXISTS (SELECT 1 FROM ap.EInvoiceInbox "
                  "WHERE CompanyCode = ? AND FileName = ?) "
                  "INSERT INTO ap.EInvoiceInbox (CompanyCode, "
                  "FileName, FormatCode, AccessPointRef, "
                  "SupplierCode, SupplierVatRegNo, SupplierDocRef, "
                  "SupplierDocRefNormalised, DocumentType, "
                  "DocumentDate, TaxPointDate, CurrencyCode, "
                  "StatedNet, StatedVat, StatedGross, LineCount, "
                  "Status, RegisterNumber, RejectReason, "
                  "ProcessedBy, ProcessedAt) VALUES (?, ?, "
                  "'PEPPOLBIS3', ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, "
                  "?, ?, ?, ?, ?, ?, SYSDATETIME())"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 21 TO SQLIO-PARM-CNT
           PERFORM 1815-INBOX-PARMS
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA AP-ROW
           IF SQLIO-ERROR
               ADD 1 TO BL-ERROR-COUNT
               MOVE 4 TO WS-RC
               DISPLAY "STERP-E-1811 " SQLIO-ERRMSG
           END-IF.
       1810-EXIT.
           EXIT.

       1815-INBOX-PARMS SECTION.
       1815-START.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 21
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-FILE-NAME TO SQLIO-PARM-VAL(2)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           MOVE WS-FILE-NAME TO SQLIO-PARM-VAL(4)
           MOVE W-ENDPOINT TO SQLIO-PARM-VAL(5)
           MOVE W-SUPP-CODE TO SQLIO-PARM-VAL(6)
           MOVE W-SUPP-VAT TO SQLIO-PARM-VAL(7)
           MOVE W-DOC-REF TO SQLIO-PARM-VAL(8)
           MOVE IR-REF-NORMALISED OF WS-REG TO SQLIO-PARM-VAL(9)
           MOVE W-DOC-TYPE TO SQLIO-PARM-VAL(10)
           MOVE W-DOC-DATE TO WS-DATE-N
           MOVE SPACES TO SQLIO-PARM-VAL(11)
           STRING WS-DATE-YYYY "-" WS-DATE-MM "-" WS-DATE-DD
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(11)
           MOVE W-TAX-POINT TO WS-DATE-N
           MOVE SPACES TO SQLIO-PARM-VAL(12)
           STRING WS-DATE-YYYY "-" WS-DATE-MM "-" WS-DATE-DD
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(12)
           MOVE W-CURRENCY TO SQLIO-PARM-VAL(13)
           MOVE W-NET TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(14)
           MOVE W-VAT TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(15)
           MOVE W-GROSS TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(16)
           MOVE W-LINE-CNT TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(17)
           MOVE WS-OUTCOME TO SQLIO-PARM-VAL(18)
           MOVE IR-REGISTER-NUMBER OF WS-REG TO SQLIO-PARM-VAL(19)
           IF IR-REGISTER-NUMBER OF WS-REG = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(19)
           END-IF
           MOVE WS-REASON-TEXT TO SQLIO-PARM-VAL(20)
           IF WS-OUTCOME = "REGISTERED"
               MOVE "Y" TO SQLIO-PARM-NULL(20)
           END-IF
           MOVE WS-USER TO SQLIO-PARM-VAL(21).
       1815-EXIT.
           EXIT.

      *>   REQ: AP-023 - a restarted run cannot read the same file
      *>   twice because the file is no longer in the inbox
       1820-MOVE-FILE SECTION.
       1820-START.
           IF WS-OUTCOME = "REGISTERED"
               MOVE WS-PROC-DIR TO WS-DEST
           ELSE
               MOVE WS-REJ-DIR TO WS-DEST
           END-IF
           MOVE SPACES TO WS-CMD
           STRING "cmd /c move /Y """
                  FUNCTION TRIM(WS-XML-NAME) """ """
                  FUNCTION TRIM(WS-DEST) """ >nul"
               DELIMITED BY SIZE INTO WS-CMD
           CALL "SYSTEM" USING WS-CMD.
       1820-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-APEINV
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
           MOVE "APEINV" TO JL-JOB-NAME
           MOVE BL-WRITTEN-COUNT TO JL-ROWS-PROCESSED
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE BL-SPOOL-FILE TO JL-SPOOL-FILE
           MOVE WS-RC TO CA-RETURN-CODE OF WS-JOBLOG
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE T-FILES TO BL-COUNT-ED
           DISPLAY "STERP-I-0500 APEINV " BL-COUNT-ED
                   " documents, " T-ACCEPTED " registered, "
                   T-DUPLICATE " duplicate, " T-REJECTED " rejected"
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0500-EXIT.
           EXIT.
