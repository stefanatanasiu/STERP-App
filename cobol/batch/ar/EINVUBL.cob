      *>================================================================
      *> PROGRAM-ID : EINVUBL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : STRUCTURED E-INVOICE EXTRACT.  WRITES ONE UBL
      *>              2.1 XML DOCUMENT PER POSTED SALES INVOICE OR
      *>              CREDIT NOTE IN THE PEPPOL BIS BILLING 3.0
      *>              STRUCTURE DECIDED IN D-26, CARRYING EVERY VAT
      *>              INVOICE DATA ITEM OF SPECIFICATION SECTION 10:
      *>              UNIQUE NUMBER, INVOICE DATE, TAX POINT, SUPPLIER
      *>              NAME ADDRESS AND VAT NUMBER WITH THE STATUTORY
      *>              TRADING DISCLOSURES, CUSTOMER NAME AND ADDRESS,
      *>              PER LINE DESCRIPTION QUANTITY UNIT PRICE VAT
      *>              RATE AND LINE NET, DISCOUNTS AND PROMPT PAYMENT
      *>              TERMS, TOTAL EXCLUDING VAT, TOTAL VAT IN
      *>              STERLING AND TOTAL PAYABLE, PLUS ANY SPECIAL
      *>              WORDING.  THE HUMAN READABLE COPY AND THIS FILE
      *>              CARRY IDENTICAL VALUES AND A REISSUE DOES NOT
      *>              CREATE A SECOND INVOICE - THE SAME NUMBER IS
      *>              SIMPLY RE-EXTRACTED.
      *> RUN AS     : EINVUBL CO=STC FROM=20261001 TO=20261031
      *>              USER=ARCLERK1   |   EINVUBL CO=STC INV=SI000123
      *> OUTPUT     : run\einvoice\<CO>-<number>.xml  (one per document)
      *>              run\spool\EINVUBL-<CO>-<stamp>.lst  (control list)
      *> CALLS      : CMDARGS, RUNPATH, JOBLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2019-04-01 KMP  CR2402 ORIGINAL EDIFACT EXTRACT FOR THE TWO
      *>                 GROCERY CUSTOMERS (WITHDRAWN 2023)
      *> 2023-07-11 KMP  CR2588 REWRITTEN AS UBL FOR PEPPOL PILOTS
      *> 2026-10-05 STERP AR-029 / AR-023 FULL VAT INVOICE CONTENT
      *>================================================================
      *> REQ: AR-029, AR-023
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EINVUBL.
       AUTHOR. K M PRESTON.
       DATE-WRITTEN. 01/04/2019.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RPT-FILE ASSIGN TO DYNAMIC WS-SPOOL-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           SELECT XML-FILE ASSIGN TO DYNAMIC WS-XML-NAME
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-XML-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  RPT-FILE REPORT IS R-EINVUBL.
       FD  XML-FILE.
       01  XML-REC                     PIC X(400).
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ARWORK.
       COPY STRPTWS.
       COPY CMDARGSCA REPLACING ==DFHCOMMAREA== BY ==WS-CMDARGS==.
       COPY RUNPATHCA REPLACING ==DFHCOMMAREA== BY ==WS-RUNPATH==.
       COPY JOBLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-JOBLOG==.
       01  WS-SPOOL-NAME               PIC X(200) VALUE SPACES.
       01  WS-XML-NAME                 PIC X(200) VALUE SPACES.
       01  WS-FILE-STATUS              PIC X(2) VALUE "00".
       01  WS-XML-STATUS               PIC X(2) VALUE "00".
       01  BR-ROW.
           05  BR-COL OCCURS 26 TIMES  PIC X(200).
       01  LN-ROW.
           05  LN-COL OCCURS 16 TIMES  PIC X(200).
       01  VT-ROW.
           05  VT-COL OCCURS 8 TIMES   PIC X(200).
       01  WS-ARGS.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-INV                  PIC X(30) VALUE SPACES.
           05  WS-FROM                 PIC 9(8) VALUE 0.
           05  WS-TO                   PIC 9(8) VALUE 0.
       01  WS-SUPP.
           05  S-NAME                  PIC X(100) VALUE SPACES.
           05  S-VAT                   PIC X(15) VALUE SPACES.
           05  S-REGNO                 PIC X(10) VALUE SPACES.
           05  S-LINE1                 PIC X(60) VALUE SPACES.
           05  S-TOWN                  PIC X(40) VALUE SPACES.
           05  S-POST                  PIC X(10) VALUE SPACES.
           05  S-COUNTRY               PIC X(2) VALUE "GB".
           05  S-JURIS                 PIC X(30) VALUE SPACES.
       01  WS-RUN.
           05  WS-RC                   PIC S9(4) VALUE 0.
           05  WS-JOBID                PIC S9(9) COMP-5 VALUE 0.
           05  WS-ROWS-OUT             PIC S9(9) VALUE 0.
           05  WS-BASE                 PIC X(160) VALUE SPACES.
           05  WS-TAG                  PIC X(40) VALUE SPACES.
           05  WS-VAL                  PIC X(200) VALUE SPACES.
           05  WS-IND                  PIC X(10) VALUE SPACES.
           05  WS-ISO                  PIC X(10) VALUE SPACES.
           05  WS-CUR                  PIC X(3) VALUE "GBP".
           05  WS-ROOT                 PIC X(10) VALUE SPACES.
           05  WS-LINETAG              PIC X(24) VALUE SPACES.
           05  WS-QTYTAG               PIC X(24) VALUE SPACES.
           05  WS-TAXCAT               PIC X(3) VALUE SPACES.
           05  WS-PCT                  PIC X(12) VALUE SPACES.
           05  WS-PCT-ED               PIC -(5)9.99.
           05  WS-INVID                PIC X(20) VALUE SPACES.
           05  WS-ESC                  PIC X(220) VALUE SPACES.
           05  WS-LINES                PIC S9(5) VALUE 0.
           05  WS-SKIPPED              PIC S9(7) VALUE 0.
       01  WS-UK.
           05  WS-UK-DD                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-UK-MM                PIC 9(2) VALUE 0.
           05  FILLER                  PIC X VALUE "/".
           05  WS-UK-YYYY              PIC 9(4) VALUE 0.
       01  WS-DET.
           05  D-TYPE                  PIC X(12) VALUE SPACES.
           05  D-NUMBER                PIC X(30) VALUE SPACES.
           05  D-DATE                  PIC X(10) VALUE SPACES.
           05  D-CUST                  PIC X(20) VALUE SPACES.
           05  D-CURR                  PIC X(3) VALUE SPACES.
           05  D-NET                   PIC S9(13)V99 VALUE 0.
           05  D-VAT                   PIC S9(13)V99 VALUE 0.
           05  D-GROSS                 PIC S9(13)V99 VALUE 0.
           05  D-VATGBP                PIC S9(13)V99 VALUE 0.
           05  D-NLINES                PIC S9(5) VALUE 0.
           05  D-FILE                  PIC X(60) VALUE SPACES.
       01  WS-TOT.
           05  T-DOCS                  PIC S9(7) VALUE 0.
           05  T-NET                   PIC S9(13)V99 VALUE 0.
           05  T-VAT                   PIC S9(13)V99 VALUE 0.
       REPORT SECTION.
       RD  R-EINVUBL
           CONTROLS ARE FINAL
           PAGE LIMIT 60 LINES
           HEADING 1 FIRST DETAIL 8 LAST DETAIL 54
           FOOTING 58.
       COPY STRPTHDR.
       01  CH-COLS TYPE IS CONTROL HEADING FINAL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(10) VALUE "Type".
               10  COLUMN 13   PIC X(6) VALUE "Number".
               10  COLUMN 45   PIC X(4) VALUE "Date".
               10  COLUMN 57   PIC X(8) VALUE "Customer".
               10  COLUMN 78   PIC X(3) VALUE "Cur".
               10  COLUMN 86   PIC X(3) VALUE "Net".
               10  COLUMN 101  PIC X(3) VALUE "VAT".
               10  COLUMN 115  PIC X(5) VALUE "Gross".
               10  COLUMN 128  PIC X(5) VALUE "Lines".
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(66) VALUE ALL "-".
               10  COLUMN 67   PIC X(66) VALUE ALL "-".
       01  DE-DOC TYPE IS DETAIL.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(11) SOURCE D-TYPE.
               10  COLUMN 13   PIC X(30) SOURCE D-NUMBER.
               10  COLUMN 45   PIC X(10) SOURCE D-DATE.
               10  COLUMN 57   PIC X(20) SOURCE D-CUST.
               10  COLUMN 78   PIC X(3) SOURCE D-CURR.
               10  COLUMN 82   PIC Z,ZZZ,ZZ9.99- SOURCE D-NET.
               10  COLUMN 97   PIC Z,ZZZ,ZZ9.99- SOURCE D-VAT.
               10  COLUMN 112  PIC Z,ZZZ,ZZ9.99- SOURCE D-GROSS.
               10  COLUMN 128  PIC ZZZZ9 SOURCE D-NLINES.
           05  LINE PLUS 1.
               10  COLUMN 5    PIC X(10) VALUE "XML file".
               10  COLUMN 16   PIC X(60) SOURCE D-FILE.
               10  COLUMN 80   PIC X(16) VALUE "VAT in sterling".
               10  COLUMN 98   PIC Z,ZZZ,ZZ9.99- SOURCE D-VATGBP.
       01  CF-FINAL TYPE IS CONTROL FOOTING FINAL.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(22) VALUE "Documents extracted".
               10  COLUMN 24   PIC ZZZ,ZZ9 SOURCE T-DOCS.
               10  COLUMN 36   PIC X(20) VALUE "Not extracted".
               10  COLUMN 57   PIC ZZZ,ZZ9 SOURCE WS-SKIPPED.
           05  LINE PLUS 1.
               10  COLUMN 1    PIC X(22) VALUE "Total net".
               10  COLUMN 24   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-NET.
               10  COLUMN 48   PIC X(16) VALUE "Total VAT".
               10  COLUMN 65   PIC Z,ZZZ,ZZZ,ZZ9.99- SOURCE T-VAT.
           05  LINE PLUS 2.
               10  COLUMN 1    PIC X(48) VALUE
                   "Peppol BIS Billing 3.0, UBL 2.1.  The structured".
               10  COLUMN 50   PIC X(46) VALUE
                   "and readable copies carry identical values.".
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-READ-ARGS
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0200-JOB-START
           PERFORM 0300-OPEN-SPOOL
           IF WS-RC > 4
               GO TO 0000-FINISH
           END-IF
           PERFORM 0370-READ-SUPPLIER
           PERFORM 1000-EXTRACT-DOCS
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
           MOVE "INV" TO CM-LOOKUP-KEY
           MOVE "GET" TO CA-FUNCTION OF WS-CMDARGS
           CALL "CMDARGS" USING WS-CMDARGS
           IF CM-FOUND = "Y"
               MOVE CM-LOOKUP-VALUE TO WS-INV
           END-IF
           IF CM-FROM NOT = SPACES
               COMPUTE WS-FROM = FUNCTION NUMVAL(CM-FROM)
           END-IF
           IF CM-TO NOT = SPACES
               COMPUTE WS-TO = FUNCTION NUMVAL(CM-TO)
           END-IF
           IF WS-CO = SPACES
               MOVE "STC" TO WS-CO
           END-IF
           IF WS-USER = SPACES
               MOVE "ARCLERK1" TO WS-USER
           END-IF
           IF WS-INV = SPACES AND WS-FROM = 0 AND WS-TO = 0
               MOVE 8 TO WS-RC
               DISPLAY "STERP-E-0104 give INV= or FROM= and TO="
               GO TO 0100-EXIT
           END-IF
           IF WS-INV = SPACES
               IF WS-FROM = 0
                   MOVE 19900101 TO WS-FROM
               END-IF
               IF WS-TO = 0
                   MOVE 20991231 TO WS-TO
               END-IF
           END-IF
           DISPLAY "STERP-I-0100 EINVUBL e-invoice extract for "
                   FUNCTION TRIM(WS-CO).
       0100-EXIT.
           EXIT.

       0200-JOB-START SECTION.
       0200-START.
           INITIALIZE WS-JOBLOG
           MOVE "START" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE WS-TO TO CA-BUSINESS-DATE OF WS-JOBLOG
           MOVE "EINVUBL" TO JL-JOB-NAME
           MOVE CM-COMMAND-LINE TO JL-ARGUMENTS
           CALL "JOBLOG" USING WS-JOBLOG
           MOVE JL-JOB-RUN-ID TO WS-JOBID.
       0200-EXIT.
           EXIT.

       0300-OPEN-SPOOL SECTION.
       0300-START.
           INITIALIZE WS-RUNPATH
           MOVE "BUILD" TO CA-FUNCTION OF WS-RUNPATH
           MOVE "SPOOL" TO RP-FOLDER
           MOVE "EINVUBL" TO RP-REPORT-ID
           MOVE WS-CO TO RP-COMPANY-REF
           MOVE "Y" TO RP-USE-TIMESTAMP
           CALL "RUNPATH" USING WS-RUNPATH
           IF CA-RETURN-CODE OF WS-RUNPATH > 4
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0301 cannot build the spool path"
               GO TO 0300-EXIT
           END-IF
           MOVE RP-FULL-PATH TO WS-SPOOL-NAME
      *>   the XML file name carries the document number, which is
      *>   longer than the standard report id, so the run directory is
      *>   resolved once and the names are built here
           INITIALIZE WS-RUNPATH
           MOVE "BASE" TO CA-FUNCTION OF WS-RUNPATH
           CALL "RUNPATH" USING WS-RUNPATH
           MOVE RP-BASE-PATH TO WS-BASE
           PERFORM 0350-FILL-HEADINGS
           OPEN OUTPUT RPT-FILE
           IF WS-FILE-STATUS NOT = "00"
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0302 cannot open " WS-SPOOL-NAME
               GO TO 0300-EXIT
           END-IF
           INITIATE R-EINVUBL.
       0300-EXIT.
           EXIT.

       0350-FILL-HEADINGS SECTION.
       0350-START.
           MOVE WS-CO TO RPT-COMPANY-CODE
           MOVE "EINVUBL" TO RPT-REPORT-ID
           MOVE "Peppol e-invoice extract (UBL 2.1)" TO RPT-TITLE
           MOVE WS-USER TO RPT-USER
           MOVE FUNCTION CURRENT-DATE TO AR-NOW
           COMPUTE RPT-RUN-YYYY = FUNCTION NUMVAL(AR-NOW(1:4))
           COMPUTE RPT-RUN-MM = FUNCTION NUMVAL(AR-NOW(5:2))
           COMPUTE RPT-RUN-DD = FUNCTION NUMVAL(AR-NOW(7:2))
           COMPUTE RPT-RUN-HH = FUNCTION NUMVAL(AR-NOW(9:2))
           COMPUTE RPT-RUN-MI = FUNCTION NUMVAL(AR-NOW(11:2))
           COMPUTE RPT-RUN-SS = FUNCTION NUMVAL(AR-NOW(13:2))
           MOVE SPACES TO RPT-AS-AT-TEXT RPT-SUBTITLE
           IF WS-INV NOT = SPACES
               STRING "Document " FUNCTION TRIM(WS-INV)
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
           ELSE
               MOVE WS-FROM TO AR-NUM-DATE
               PERFORM 8100-FORMAT-DATE
               MOVE WS-UK TO AR-TEXT(1:10)
               MOVE WS-TO TO AR-NUM-DATE
               PERFORM 8100-FORMAT-DATE
               STRING "Document date " AR-TEXT(1:10) " to " WS-UK
                   DELIMITED BY SIZE INTO RPT-SUBTITLE
               MOVE SPACES TO AR-TEXT
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
      *> REQ: AR-023  the statutory trading disclosures travel with the
      *> structured invoice, not only with the printed copy
      *>----------------------------------------------------------------
       0370-READ-SUPPLIER SECTION.
       0370-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Name, ISNULL(VatRegNo,''), "
                  "ISNULL(RegisteredNumber,''), "
                  "ISNULL(RegOfficeLine1,''), "
                  "ISNULL(RegOfficeTown,''), "
                  "ISNULL(RegOfficePostcode,''), CASE WHEN LEN("
                  "ISNULL(RegOfficeCountry,'')) = 2 THEN "
                  "RegOfficeCountry ELSE 'GB' END, Jurisdiction "
                  "FROM core.Company WHERE CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 8 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW
           IF SQLIO-OK
               MOVE BR-COL(1) TO S-NAME
               MOVE BR-COL(2) TO S-VAT
               MOVE BR-COL(3) TO S-REGNO
               MOVE BR-COL(4) TO S-LINE1
               MOVE BR-COL(5) TO S-TOWN
               MOVE BR-COL(6) TO S-POST
               MOVE BR-COL(7)(1:2) TO S-COUNTRY
               MOVE BR-COL(8) TO S-JURIS
           ELSE
               MOVE 12 TO WS-RC
               DISPLAY "STERP-E-0371 company not found"
           END-IF.
       0370-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-029  one UBL document per posted invoice/credit note
      *>----------------------------------------------------------------
       1000-EXTRACT-DOCS SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24),i.InvoiceId), "
                  "i.DocumentType, i.InvoiceNumber, CONVERT(char(8),"
                  "i.DocumentDate,112), CONVERT(char(8),"
                  "i.TaxPointDate,112), CONVERT(char(8),i.DueDate,"
                  "112), i.CurrencyCode, CONVERT(varchar(24),"
                  "i.NetAmount), CONVERT(varchar(24),i.VatAmount), "
                  "CONVERT(varchar(24),i.GrossAmount), "
                  "CONVERT(varchar(24),i.VatAmountGBP), "
                  "i.CustomerCode, ISNULL(i.BillToName,c.LegalName),"
                  " ISNULL(i.BillToAddress,''), "
                  "ISNULL(i.CustomerVatRegNo,''), "
                  "ISNULL(i.SpecialWording,''), "
                  "ISNULL(i.CustomerOrderRef,''), "
                  "ISNULL(i.OriginalInvoiceNumber,''), "
                  "ISNULL(i.TermsSnapshot,''), CONVERT(varchar(24),"
                  "ISNULL(i.SettlementDiscountPct,0)), "
                  "CONVERT(varchar(24),i.DepositAppliedGBP), CASE "
                  "WHEN LEN(ISNULL(c.CountryCode,'')) = 2 THEN "
                  "c.CountryCode ELSE 'GB' END, "
                  "ISNULL(i.SalesOrderNumber,''), "
                  "CONVERT(varchar(24),i.FxRate) FROM "
                  "ar.SalesInvoice i JOIN ar.Customer c ON "
                  "c.CompanyCode = i.CompanyCode AND c.CustomerCode "
                  "= i.CustomerCode WHERE i.CompanyCode = ? AND "
                  "i.Status = 'POSTED' AND i.DocumentType IN "
                  "('INVOICE','CREDITNOTE')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           IF WS-INV NOT = SPACES
               MOVE 2 TO SQLIO-PARM-CNT
               MOVE WS-INV TO SQLIO-PARM-VAL(2)
               MOVE "N" TO SQLIO-PARM-NULL(2)
               MOVE " AND i.InvoiceNumber = ?" TO AR-TEXT
               PERFORM 8200-APPEND-SQL
           ELSE
               MOVE 3 TO SQLIO-PARM-CNT
               MOVE WS-FROM TO AR-DATE-ED
               MOVE AR-DATE-ED TO SQLIO-PARM-VAL(2)
               MOVE "N" TO SQLIO-PARM-NULL(2)
               MOVE WS-TO TO AR-DATE-ED
               MOVE AR-DATE-ED TO SQLIO-PARM-VAL(3)
               MOVE "N" TO SQLIO-PARM-NULL(3)
               MOVE " AND i.DocumentDate BETWEEN ? AND ?"
                   TO AR-TEXT
               PERFORM 8200-APPEND-SQL
           END-IF
           MOVE " ORDER BY i.InvoiceNumber" TO AR-TEXT
           PERFORM 8200-APPEND-SQL
           MOVE 24 TO SQLIO-COL-CNT
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
               MOVE 24 TO SQLIO-COL-CNT
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA BR-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO AR-DONE
               ELSE
                   PERFORM 1100-ONE-DOCUMENT
               END-IF
           END-PERFORM
           MOVE 1 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA BR-ROW.
       1000-EXIT.
           EXIT.

       1100-ONE-DOCUMENT SECTION.
       1100-START.
           MOVE BR-COL(1) TO WS-INVID
           MOVE BR-COL(2) TO D-TYPE
           MOVE BR-COL(3) TO D-NUMBER
           MOVE BR-COL(12) TO D-CUST
           MOVE BR-COL(7) TO D-CURR WS-CUR
           COMPUTE D-NET = FUNCTION NUMVAL(BR-COL(8))
           COMPUTE D-VAT = FUNCTION NUMVAL(BR-COL(9))
           COMPUTE D-GROSS = FUNCTION NUMVAL(BR-COL(10))
           COMPUTE D-VATGBP = FUNCTION NUMVAL(BR-COL(11))
           MOVE BR-COL(4) TO AR-COMPACT-DATE
           COMPUTE AR-NUM-DATE = FUNCTION NUMVAL(AR-COMPACT-DATE)
           PERFORM 8100-FORMAT-DATE
           MOVE WS-UK TO D-DATE
           MOVE 0 TO D-NLINES WS-LINES
           IF D-TYPE(1:10) = "CREDITNOTE"
               MOVE "CreditNote" TO WS-ROOT
               MOVE "cac:CreditNoteLine" TO WS-LINETAG
               MOVE "cbc:CreditedQuantity" TO WS-QTYTAG
           ELSE
               MOVE "Invoice" TO WS-ROOT
               MOVE "cac:InvoiceLine" TO WS-LINETAG
               MOVE "cbc:InvoicedQuantity" TO WS-QTYTAG
           END-IF
           PERFORM 1150-BUILD-NAME
           OPEN OUTPUT XML-FILE
           IF WS-XML-STATUS NOT = "00"
               ADD 1 TO WS-SKIPPED
               MOVE 4 TO WS-RC
               DISPLAY "STERP-W-1101 cannot write " WS-XML-NAME
               GO TO 1100-EXIT
           END-IF
           PERFORM 2000-WRITE-HEADER
           PERFORM 2200-WRITE-PARTIES
           PERFORM 2400-WRITE-PAYMENT
           PERFORM 2500-WRITE-TAX
           PERFORM 2600-WRITE-TOTALS
           PERFORM 2700-WRITE-LINES
           MOVE SPACES TO XML-REC
           STRING "</" FUNCTION TRIM(WS-ROOT) ">"
               DELIMITED BY SIZE INTO XML-REC
           WRITE XML-REC
           CLOSE XML-FILE
           MOVE WS-LINES TO D-NLINES
           ADD 1 TO T-DOCS WS-ROWS-OUT
           ADD D-NET TO T-NET
           ADD D-VAT TO T-VAT
           GENERATE DE-DOC.
       1100-EXIT.
           EXIT.

       1150-BUILD-NAME SECTION.
       1150-START.
      *>   run\einvoice\<CO>-<document number>.xml - the number is the
      *>   stable legal reference, so a reissue overwrites the same
      *>   file rather than creating a second invoice
           MOVE SPACES TO WS-XML-NAME D-FILE
           STRING FUNCTION TRIM(WS-BASE) "\einvoice\"
                  FUNCTION TRIM(WS-CO) "-"
                  FUNCTION TRIM(D-NUMBER) ".xml"
               DELIMITED BY SIZE INTO WS-XML-NAME
           STRING FUNCTION TRIM(WS-CO) "-"
                  FUNCTION TRIM(D-NUMBER) ".xml"
               DELIMITED BY SIZE INTO D-FILE.
       1150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-023  unique number, invoice date, tax point, currency
      *>----------------------------------------------------------------
       2000-WRITE-HEADER SECTION.
       2000-START.
           MOVE SPACES TO XML-REC
           MOVE "<?xml version=""1.0"" encoding=""UTF-8""?>"
               TO XML-REC
           WRITE XML-REC
           MOVE SPACES TO XML-REC
           STRING "<" FUNCTION TRIM(WS-ROOT) " xmlns=""urn:oasis:"
                  "names:specification:ubl:schema:xsd:"
                  FUNCTION TRIM(WS-ROOT) "-2"""
               DELIMITED BY SIZE INTO XML-REC
           WRITE XML-REC
           MOVE SPACES TO XML-REC
           STRING "    xmlns:cac=""urn:oasis:names:specification:"
                  "ubl:schema:xsd:CommonAggregateComponents-2"""
               DELIMITED BY SIZE INTO XML-REC
           WRITE XML-REC
           MOVE SPACES TO XML-REC
           STRING "    xmlns:cbc=""urn:oasis:names:specification:"
                  "ubl:schema:xsd:CommonBasicComponents-2"">"
               DELIMITED BY SIZE INTO XML-REC
           WRITE XML-REC
           MOVE SPACES TO XML-REC
           MOVE "  <cbc:CustomizationID>urn:cen.eu:en16931:2017#com"
               TO XML-REC(1:50)
           MOVE "pliant#urn:fdc:peppol.eu:2017:poacc:billing:3.0"
               TO XML-REC(51:47)
           MOVE "</cbc:CustomizationID>" TO XML-REC(98:22)
           WRITE XML-REC
           MOVE SPACES TO XML-REC
           MOVE "  <cbc:ProfileID>urn:fdc:peppol.eu:2017:poacc:bill"
               TO XML-REC(1:50)
           MOVE "ing:01:1.0</cbc:ProfileID>" TO XML-REC(51:26)
           WRITE XML-REC
           MOVE "cbc:ID" TO WS-TAG
           MOVE D-NUMBER TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE BR-COL(4) TO AR-COMPACT-DATE
           PERFORM 8150-ISO-DATE
           MOVE "cbc:IssueDate" TO WS-TAG
           MOVE WS-ISO TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE BR-COL(6) TO AR-COMPACT-DATE
           PERFORM 8150-ISO-DATE
           MOVE "cbc:DueDate" TO WS-TAG
           MOVE WS-ISO TO WS-VAL
           PERFORM 8500-PUT-TAG
           IF WS-ROOT = "CreditNote"
               MOVE "cbc:CreditNoteTypeCode" TO WS-TAG
               MOVE "381" TO WS-VAL
           ELSE
               MOVE "cbc:InvoiceTypeCode" TO WS-TAG
               MOVE "380" TO WS-VAL
           END-IF
           PERFORM 8500-PUT-TAG
           MOVE BR-COL(5) TO AR-COMPACT-DATE
           PERFORM 8150-ISO-DATE
           MOVE "cbc:TaxPointDate" TO WS-TAG
           MOVE WS-ISO TO WS-VAL
           PERFORM 8500-PUT-TAG
           IF BR-COL(16) NOT = SPACES
               MOVE "cbc:Note" TO WS-TAG
               MOVE BR-COL(16)(1:200) TO WS-VAL
               PERFORM 8500-PUT-TAG
           END-IF
           MOVE "cbc:DocumentCurrencyCode" TO WS-TAG
           MOVE WS-CUR TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE "cbc:TaxCurrencyCode" TO WS-TAG
           MOVE "GBP" TO WS-VAL
           PERFORM 8500-PUT-TAG
           IF BR-COL(17) NOT = SPACES
               MOVE "cbc:BuyerReference" TO WS-TAG
               MOVE BR-COL(17)(1:200) TO WS-VAL
               PERFORM 8500-PUT-TAG
           END-IF
           PERFORM 2100-WRITE-REFS.
       2000-EXIT.
           EXIT.

       2100-WRITE-REFS SECTION.
       2100-START.
           IF BR-COL(23) NOT = SPACES
               MOVE "  <cac:OrderReference>" TO WS-VAL
               PERFORM 8560-PUT-LINE
               MOVE "cbc:ID" TO WS-TAG
               MOVE BR-COL(23)(1:200) TO WS-VAL
               PERFORM 8500-PUT-TAG
               MOVE "  </cac:OrderReference>" TO WS-VAL
               PERFORM 8560-PUT-LINE
           END-IF
           IF BR-COL(18) NOT = SPACES
      *>       a VAT credit note must quote the original invoice
               MOVE "  <cac:BillingReference>" TO WS-VAL
               PERFORM 8560-PUT-LINE
               MOVE "    <cac:InvoiceDocumentReference>" TO WS-VAL
               PERFORM 8560-PUT-LINE
               MOVE "cbc:ID" TO WS-TAG
               MOVE BR-COL(18)(1:200) TO WS-VAL
               PERFORM 8500-PUT-TAG
               MOVE "    </cac:InvoiceDocumentReference>" TO WS-VAL
               PERFORM 8560-PUT-LINE
               MOVE "  </cac:BillingReference>" TO WS-VAL
               PERFORM 8560-PUT-LINE
           END-IF.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-023  supplier and customer name, address, VAT number
      *> and the statutory trading disclosures
      *>----------------------------------------------------------------
       2200-WRITE-PARTIES SECTION.
       2200-START.
           MOVE "  <cac:AccountingSupplierParty>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "    <cac:Party>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "cbc:EndpointID" TO WS-TAG
           MOVE S-VAT TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE "      <cac:PartyName>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "cbc:Name" TO WS-TAG
           MOVE S-NAME TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE "      </cac:PartyName>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE S-LINE1 TO AR-TEXT
           MOVE S-TOWN TO AR-TEXT2(1:40)
           MOVE S-POST TO AR-SCRATCH(1:10)
           MOVE S-COUNTRY TO AR-SCRATCH(12:2)
           PERFORM 2300-WRITE-ADDRESS
           MOVE "      <cac:PartyTaxScheme>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "cbc:CompanyID" TO WS-TAG
           MOVE S-VAT TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE "        <cac:TaxScheme><cbc:ID>VAT</cbc:ID>"
               TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "        </cac:TaxScheme>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "      </cac:PartyTaxScheme>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "      <cac:PartyLegalEntity>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "cbc:RegistrationName" TO WS-TAG
           MOVE S-NAME TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE "cbc:CompanyID" TO WS-TAG
           MOVE S-REGNO TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE "cbc:CompanyLegalForm" TO WS-TAG
           MOVE SPACES TO WS-VAL
           STRING "Registered in " FUNCTION TRIM(S-JURIS)
               DELIMITED BY SIZE INTO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE "      </cac:PartyLegalEntity>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "    </cac:Party>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "  </cac:AccountingSupplierParty>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           PERFORM 2250-WRITE-BUYER.
       2200-EXIT.
           EXIT.

       2250-WRITE-BUYER SECTION.
       2250-START.
           MOVE "  <cac:AccountingCustomerParty>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "    <cac:Party>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "cbc:EndpointID" TO WS-TAG
           IF BR-COL(15) NOT = SPACES
               MOVE BR-COL(15)(1:200) TO WS-VAL
           ELSE
               MOVE D-CUST TO WS-VAL
           END-IF
           PERFORM 8500-PUT-TAG
           MOVE "      <cac:PartyName>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "cbc:Name" TO WS-TAG
           MOVE BR-COL(13)(1:200) TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE "      </cac:PartyName>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE BR-COL(14)(1:200) TO AR-TEXT
           MOVE SPACES TO AR-TEXT2 AR-SCRATCH
           MOVE BR-COL(22)(1:2) TO AR-SCRATCH(12:2)
           PERFORM 2300-WRITE-ADDRESS
           IF BR-COL(15) NOT = SPACES
               MOVE "      <cac:PartyTaxScheme>" TO WS-VAL
               PERFORM 8560-PUT-LINE
               MOVE "cbc:CompanyID" TO WS-TAG
               MOVE BR-COL(15)(1:200) TO WS-VAL
               PERFORM 8500-PUT-TAG
               MOVE "        <cac:TaxScheme><cbc:ID>VAT</cbc:ID>"
                   TO WS-VAL
               PERFORM 8560-PUT-LINE
               MOVE "        </cac:TaxScheme>" TO WS-VAL
               PERFORM 8560-PUT-LINE
               MOVE "      </cac:PartyTaxScheme>" TO WS-VAL
               PERFORM 8560-PUT-LINE
           END-IF
           MOVE "      <cac:PartyLegalEntity>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "cbc:RegistrationName" TO WS-TAG
           MOVE BR-COL(13)(1:200) TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE "      </cac:PartyLegalEntity>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "    </cac:Party>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "  </cac:AccountingCustomerParty>" TO WS-VAL
           PERFORM 8560-PUT-LINE.
       2250-EXIT.
           EXIT.

       2300-WRITE-ADDRESS SECTION.
       2300-START.
           MOVE "      <cac:PostalAddress>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "cbc:StreetName" TO WS-TAG
           MOVE AR-TEXT TO WS-VAL
           PERFORM 8500-PUT-TAG
           IF AR-TEXT2 NOT = SPACES
               MOVE "cbc:CityName" TO WS-TAG
               MOVE AR-TEXT2 TO WS-VAL
               PERFORM 8500-PUT-TAG
           END-IF
           IF AR-SCRATCH(1:10) NOT = SPACES
               MOVE "cbc:PostalZone" TO WS-TAG
               MOVE AR-SCRATCH(1:10) TO WS-VAL
               PERFORM 8500-PUT-TAG
           END-IF
           MOVE "        <cac:Country>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "cbc:IdentificationCode" TO WS-TAG
           MOVE AR-SCRATCH(12:2) TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE "        </cac:Country>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "      </cac:PostalAddress>" TO WS-VAL
           PERFORM 8560-PUT-LINE.
       2300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-023  payment means and the prompt payment terms
      *>----------------------------------------------------------------
       2400-WRITE-PAYMENT SECTION.
       2400-START.
           MOVE "  <cac:PaymentMeans>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "cbc:PaymentMeansCode" TO WS-TAG
           MOVE "31" TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE "cbc:PaymentID" TO WS-TAG
           MOVE D-NUMBER TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE "  </cac:PaymentMeans>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "  <cac:PaymentTerms>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "cbc:Note" TO WS-TAG
           MOVE SPACES TO WS-VAL
           COMPUTE AR-PCT1 = FUNCTION NUMVAL(BR-COL(20))
           IF AR-PCT1 > 0
               MOVE AR-PCT1 TO AR-RATE-ED
               STRING FUNCTION TRIM(BR-COL(19))
                      " - prompt payment discount "
                      FUNCTION TRIM(AR-RATE-ED) " per cent"
                   DELIMITED BY SIZE INTO WS-VAL
           ELSE
               MOVE BR-COL(19)(1:200) TO WS-VAL
           END-IF
           PERFORM 8500-PUT-TAG
           MOVE "  </cac:PaymentTerms>" TO WS-VAL
           PERFORM 8560-PUT-LINE.
       2400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-023  total VAT, in sterling as well when the document
      *> is in a foreign currency
      *>----------------------------------------------------------------
       2500-WRITE-TAX SECTION.
       2500-START.
           MOVE "  <cac:TaxTotal>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE D-VAT TO AR-AMT1
           MOVE "cbc:TaxAmount" TO WS-TAG
           PERFORM 8600-PUT-MONEY
           PERFORM 2550-TAX-SUBTOTALS
           MOVE "  </cac:TaxTotal>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           IF WS-CUR NOT = "GBP"
               MOVE "  <cac:TaxTotal>" TO WS-VAL
               PERFORM 8560-PUT-LINE
               MOVE SPACES TO XML-REC
               MOVE D-VATGBP TO AR-MONEY-ED
               STRING "    <cbc:TaxAmount currencyID=""GBP"">"
                      FUNCTION TRIM(AR-MONEY-ED)
                      "</cbc:TaxAmount>"
                   DELIMITED BY SIZE INTO XML-REC
               WRITE XML-REC
               MOVE "  </cac:TaxTotal>" TO WS-VAL
               PERFORM 8560-PUT-LINE
           END-IF.
       2500-EXIT.
           EXIT.

       2550-TAX-SUBTOTALS SECTION.
       2550-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT v.VatCode, CONVERT(varchar(24),v.RatePct),"
                  " CONVERT(varchar(24),v.NetAmount), "
                  "CONVERT(varchar(24),v.VatAmount) FROM "
                  "ar.SalesInvoiceVat v WHERE v.InvoiceId = ? ORDER "
                  "BY v.VatCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-INVID TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE 4 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA VT-ROW
           IF SQLIO-ERROR
               GO TO 2550-EXIT
           END-IF
           MOVE "N" TO AR-ERROR
           PERFORM UNTIL AR-ERROR = "Y"
               MOVE 3 TO SQLIO-CURSOR
               MOVE 4 TO SQLIO-COL-CNT
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA VT-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO AR-ERROR
               ELSE
                   PERFORM 2560-ONE-SUBTOTAL
               END-IF
           END-PERFORM
           MOVE "N" TO AR-ERROR
           MOVE 3 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA VT-ROW.
       2550-EXIT.
           EXIT.

       2560-ONE-SUBTOTAL SECTION.
       2560-START.
           COMPUTE AR-PCT1 = FUNCTION NUMVAL(VT-COL(2))
           MOVE AR-PCT1 TO WS-PCT-ED
           MOVE FUNCTION TRIM(WS-PCT-ED) TO WS-PCT
           IF AR-PCT1 > 0
               MOVE "S" TO WS-TAXCAT
           ELSE
               MOVE "Z" TO WS-TAXCAT
           END-IF
           MOVE "    <cac:TaxSubtotal>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           COMPUTE AR-AMT1 = FUNCTION NUMVAL(VT-COL(3))
           MOVE "cbc:TaxableAmount" TO WS-TAG
           PERFORM 8600-PUT-MONEY
           COMPUTE AR-AMT1 = FUNCTION NUMVAL(VT-COL(4))
           MOVE "cbc:TaxAmount" TO WS-TAG
           PERFORM 8600-PUT-MONEY
           MOVE "      <cac:TaxCategory>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "cbc:ID" TO WS-TAG
           MOVE WS-TAXCAT TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE "cbc:Percent" TO WS-TAG
           MOVE WS-PCT TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE "        <cac:TaxScheme><cbc:ID>VAT</cbc:ID>"
               TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "        </cac:TaxScheme>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "      </cac:TaxCategory>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "    </cac:TaxSubtotal>" TO WS-VAL
           PERFORM 8560-PUT-LINE.
       2560-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-023  total excluding VAT and total payable
      *>----------------------------------------------------------------
       2600-WRITE-TOTALS SECTION.
       2600-START.
           MOVE "  <cac:LegalMonetaryTotal>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE D-NET TO AR-AMT1
           MOVE "cbc:LineExtensionAmount" TO WS-TAG
           PERFORM 8600-PUT-MONEY
           MOVE "cbc:TaxExclusiveAmount" TO WS-TAG
           PERFORM 8600-PUT-MONEY
           MOVE D-GROSS TO AR-AMT1
           MOVE "cbc:TaxInclusiveAmount" TO WS-TAG
           PERFORM 8600-PUT-MONEY
           COMPUTE AR-AMT2 = FUNCTION NUMVAL(BR-COL(21))
           MOVE AR-AMT2 TO AR-AMT1
           MOVE "cbc:PrepaidAmount" TO WS-TAG
           PERFORM 8600-PUT-MONEY
           COMPUTE AR-AMT1 = D-GROSS - AR-AMT2
           MOVE "cbc:PayableAmount" TO WS-TAG
           PERFORM 8600-PUT-MONEY
           MOVE "  </cac:LegalMonetaryTotal>" TO WS-VAL
           PERFORM 8560-PUT-LINE.
       2600-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: AR-023  per line description, quantity, unit price
      *> excluding VAT, VAT rate and the line amount excluding VAT
      *>----------------------------------------------------------------
       2700-WRITE-LINES SECTION.
       2700-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12),l.LineNum), "
                  "ISNULL(l.ItemCode,''), l.Description, "
                  "CONVERT(varchar(24),l.Quantity), "
                  "ISNULL(l.UomCode,'EA'), CONVERT(varchar(24),"
                  "l.UnitPrice), CONVERT(varchar(24),l.NetAmount), "
                  "l.VatCode, CONVERT(varchar(24),l.VatRatePct), "
                  "CONVERT(varchar(24),l.AllowanceAmount), "
                  "ISNULL(l.VatMechanism,''), "
                  "ISNULL(l.VatLiability,'') FROM "
                  "ar.SalesInvoiceLine l WHERE l.InvoiceId = ? "
                  "ORDER BY l.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-INVID TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE 12 TO SQLIO-COL-CNT
           PERFORM 8000-SET-WIDTHS
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA LN-ROW
           IF SQLIO-ERROR
               GO TO 2700-EXIT
           END-IF
           MOVE "N" TO AR-ERROR
           PERFORM UNTIL AR-ERROR = "Y"
               MOVE 2 TO SQLIO-CURSOR
               MOVE 12 TO SQLIO-COL-CNT
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA LN-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO AR-ERROR
               ELSE
                   PERFORM 2750-ONE-LINE
               END-IF
           END-PERFORM
           MOVE "N" TO AR-ERROR
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA LN-ROW.
       2700-EXIT.
           EXIT.

       2750-ONE-LINE SECTION.
       2750-START.
           ADD 1 TO WS-LINES
           COMPUTE AR-PCT1 = FUNCTION NUMVAL(LN-COL(9))
           MOVE AR-PCT1 TO WS-PCT-ED
           MOVE FUNCTION TRIM(WS-PCT-ED) TO WS-PCT
           EVALUATE TRUE
               WHEN LN-COL(11)(1:7) = "REVERSE"
                   MOVE "AE" TO WS-TAXCAT
               WHEN LN-COL(12)(1:6) = "EXEMPT"
                   MOVE "E" TO WS-TAXCAT
               WHEN AR-PCT1 > 0
                   MOVE "S" TO WS-TAXCAT
               WHEN OTHER
                   MOVE "Z" TO WS-TAXCAT
           END-EVALUATE
           MOVE SPACES TO WS-VAL
           STRING "  <" FUNCTION TRIM(WS-LINETAG) ">"
               DELIMITED BY SIZE INTO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "cbc:ID" TO WS-TAG
           MOVE LN-COL(1)(1:200) TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE SPACES TO XML-REC
           COMPUTE AR-QTY1 = FUNCTION NUMVAL(LN-COL(4))
           MOVE AR-QTY1 TO AR-QTY-ED
           STRING "    <" FUNCTION TRIM(WS-QTYTAG) " unitCode="""
                  FUNCTION TRIM(LN-COL(5)) """>"
                  FUNCTION TRIM(AR-QTY-ED)
                  "</" FUNCTION TRIM(WS-QTYTAG) ">"
               DELIMITED BY SIZE INTO XML-REC
           WRITE XML-REC
           COMPUTE AR-AMT1 = FUNCTION NUMVAL(LN-COL(7))
           MOVE "cbc:LineExtensionAmount" TO WS-TAG
           PERFORM 8600-PUT-MONEY
           COMPUTE AR-AMT2 = FUNCTION NUMVAL(LN-COL(10))
           IF AR-AMT2 NOT = 0
               PERFORM 2780-ALLOWANCE
           END-IF
           PERFORM 2790-ITEM-AND-PRICE
           MOVE SPACES TO WS-VAL
           STRING "  </" FUNCTION TRIM(WS-LINETAG) ">"
               DELIMITED BY SIZE INTO WS-VAL
           PERFORM 8560-PUT-LINE.
       2750-EXIT.
           EXIT.

       2780-ALLOWANCE SECTION.
       2780-START.
           MOVE "    <cac:AllowanceCharge>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "cbc:ChargeIndicator" TO WS-TAG
           MOVE "false" TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE "cbc:AllowanceChargeReason" TO WS-TAG
           MOVE "Discount" TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE AR-AMT2 TO AR-AMT1
           MOVE "cbc:Amount" TO WS-TAG
           PERFORM 8600-PUT-MONEY
           MOVE "    </cac:AllowanceCharge>" TO WS-VAL
           PERFORM 8560-PUT-LINE.
       2780-EXIT.
           EXIT.

       2790-ITEM-AND-PRICE SECTION.
       2790-START.
           MOVE "    <cac:Item>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "cbc:Description" TO WS-TAG
           MOVE LN-COL(3)(1:200) TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE "cbc:Name" TO WS-TAG
           MOVE LN-COL(3)(1:200) TO WS-VAL
           PERFORM 8500-PUT-TAG
           IF LN-COL(2) NOT = SPACES
               MOVE "      <cac:SellersItemIdentification>"
                   TO WS-VAL
               PERFORM 8560-PUT-LINE
               MOVE "cbc:ID" TO WS-TAG
               MOVE LN-COL(2)(1:200) TO WS-VAL
               PERFORM 8500-PUT-TAG
               MOVE "      </cac:SellersItemIdentification>"
                   TO WS-VAL
               PERFORM 8560-PUT-LINE
           END-IF
           MOVE "      <cac:ClassifiedTaxCategory>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "cbc:ID" TO WS-TAG
           MOVE WS-TAXCAT TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE "cbc:Percent" TO WS-TAG
           MOVE WS-PCT TO WS-VAL
           PERFORM 8500-PUT-TAG
           MOVE "        <cac:TaxScheme><cbc:ID>VAT</cbc:ID>"
               TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "        </cac:TaxScheme>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "      </cac:ClassifiedTaxCategory>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "    </cac:Item>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           MOVE "    <cac:Price>" TO WS-VAL
           PERFORM 8560-PUT-LINE
           COMPUTE AR-AMT1 = FUNCTION NUMVAL(LN-COL(6))
           MOVE "cbc:PriceAmount" TO WS-TAG
           PERFORM 8600-PUT-MONEY
           MOVE "    </cac:Price>" TO WS-VAL
           PERFORM 8560-PUT-LINE.
       2790-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-SET-WIDTHS SECTION.
       8000-START.
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > 26
               MOVE 200 TO SQLIO-COL-WIDTH(AR-I)
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

       8150-ISO-DATE SECTION.
       8150-START.
      *>   yyyymmdd text in AR-COMPACT-DATE -> yyyy-mm-dd in WS-ISO
           MOVE SPACES TO WS-ISO
           MOVE AR-CMP-Y TO WS-ISO(1:4)
           MOVE "-" TO WS-ISO(5:1)
           MOVE AR-CMP-M TO WS-ISO(6:2)
           MOVE "-" TO WS-ISO(8:1)
           MOVE AR-CMP-D TO WS-ISO(9:2).
       8150-EXIT.
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

      *>----------------------------------------------------------------
      *> XML helpers.  Values are escaped before they are written: a
      *> description with an ampersand must not break the document.
      *>----------------------------------------------------------------
       8400-XML-ESCAPE SECTION.
       8400-START.
           MOVE SPACES TO WS-ESC
           MOVE 1 TO AR-K
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 200
               IF AR-K < 196
                   EVALUATE WS-VAL(AR-J:1)
                       WHEN "&"
                           MOVE "&amp;" TO WS-ESC(AR-K:5)
                           ADD 5 TO AR-K
                       WHEN "<"
                           MOVE "&lt;" TO WS-ESC(AR-K:4)
                           ADD 4 TO AR-K
                       WHEN ">"
                           MOVE "&gt;" TO WS-ESC(AR-K:4)
                           ADD 4 TO AR-K
                       WHEN OTHER
                           MOVE WS-VAL(AR-J:1) TO WS-ESC(AR-K:1)
                           ADD 1 TO AR-K
                   END-EVALUATE
               END-IF
           END-PERFORM.
       8400-EXIT.
           EXIT.

       8500-PUT-TAG SECTION.
       8500-START.
           PERFORM 8400-XML-ESCAPE
           MOVE SPACES TO XML-REC
           STRING "    <" FUNCTION TRIM(WS-TAG) ">"
                  FUNCTION TRIM(WS-ESC)
                  "</" FUNCTION TRIM(WS-TAG) ">"
               DELIMITED BY SIZE INTO XML-REC
           WRITE XML-REC
           MOVE SPACES TO WS-VAL WS-TAG.
       8500-EXIT.
           EXIT.

       8560-PUT-LINE SECTION.
       8560-START.
           MOVE SPACES TO XML-REC
           MOVE WS-VAL(1:200) TO XML-REC(1:200)
           WRITE XML-REC
           MOVE SPACES TO WS-VAL.
       8560-EXIT.
           EXIT.

       8600-PUT-MONEY SECTION.
       8600-START.
           MOVE AR-AMT1 TO AR-MONEY-ED
           MOVE SPACES TO XML-REC
           STRING "    <" FUNCTION TRIM(WS-TAG) " currencyID="""
                  FUNCTION TRIM(WS-CUR) """>"
                  FUNCTION TRIM(AR-MONEY-ED)
                  "</" FUNCTION TRIM(WS-TAG) ">"
               DELIMITED BY SIZE INTO XML-REC
           WRITE XML-REC
           MOVE SPACES TO WS-TAG.
       8600-EXIT.
           EXIT.

       0400-CLOSE-SPOOL SECTION.
       0400-START.
           TERMINATE R-EINVUBL
           CLOSE RPT-FILE
           DISPLAY "STERP-I-0401 " T-DOCS " e-invoice document(s)"
           DISPLAY "STERP-I-SPOOL " FUNCTION TRIM(WS-SPOOL-NAME).
       0400-EXIT.
           EXIT.

       0500-JOB-END SECTION.
       0500-START.
           INITIALIZE WS-JOBLOG
           MOVE "END" TO CA-FUNCTION OF WS-JOBLOG
           MOVE WS-CO TO CA-COMPANY OF WS-JOBLOG
           MOVE WS-USER TO CA-USER OF WS-JOBLOG
           MOVE "EINVUBL" TO JL-JOB-NAME
           MOVE WS-JOBID TO JL-JOB-RUN-ID
           EVALUATE TRUE
               WHEN WS-RC = 0   MOVE "COMPLETE" TO JL-STATUS
               WHEN WS-RC = 4   MOVE "WARNING" TO JL-STATUS
               WHEN OTHER       MOVE "FAILED" TO JL-STATUS
           END-EVALUATE
           MOVE WS-RC TO JL-RETURN-CODE
           MOVE WS-ROWS-OUT TO JL-ROWS-PROCESSED
           MOVE WS-SPOOL-NAME TO JL-SPOOL-FILE
           CALL "JOBLOG" USING WS-JOBLOG.
       0500-EXIT.
           EXIT.
