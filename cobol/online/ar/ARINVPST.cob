      *>================================================================
      *> PROGRAM-ID : ARINVPST
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR)
      *> PURPOSE    : POST ONE SALES INVOICE.  ONE ELIGIBLE INVOICE
      *>              CREATES EXACTLY ONE RECEIVABLE, THE REVENUE AND
      *>              VAT ACCOUNTING AND THE LINK BACK TO ITS SOURCE
      *>              DOCUMENT.  THE DOCUMENT MUST BE A VALID UK VAT
      *>              INVOICE BEFORE IT CAN BE ISSUED, AND A PRO FORMA
      *>              CREATES NEITHER A RECEIVABLE NOR A VAT RECORD.
      *> CALLED BY  : SALES ORDER INVOICING (SO), DIRECT INVOICE ENTRY,
      *>              .NET CLIENT, AR DEMO BATCH
      *> INTERFACE  : CALL "ARINVPST" USING WS-INVPST-CA
      *>              COPYBOOK ARINVPSTCA
      *> CALLS      : SQLIO, TERMCALC, PERIODCK, PERMCHK, NUMGEN,
      *>              IDEMPOT, FXCONV, ARVATWRT, ARPOSTGL, EXCPRAIS
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-05-02 RJH  ORIGINAL MVS SALES LEDGER INVOICE POSTER
      *> 1993-01-18 DJM  CR0244 SETTLEMENT DISCOUNT ON THE FACE OF
      *>                 THE INVOICE
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS
      *> 2015-04-01 KMP  CR2101 VAT ON PROMPT PAYMENT DISCOUNT IS NOW
      *>                 REDUCED ONLY WHEN THE DISCOUNT IS TAKEN
      *> 2021-01-01 KMP  BREXIT - GB / XI / EU / ROW VAT LOCATION
      *> 2026-10-05 STERP SIMPLIFIED INVOICE LIMIT, E-INVOICE FLAG
      *>================================================================
      *> REQ: AR-003, AR-004, AR-005, AR-023
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARINVPST.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 02/05/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ARWORK.
       COPY TERMCALCCA REPLACING ==DFHCOMMAREA== BY ==WS-TERM-CA==.
       COPY PERIODCKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERIOD-CA==.
       COPY PERMCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERM-CA==.
       COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==WS-NUM-CA==.
       COPY IDEMPOTCA REPLACING ==DFHCOMMAREA== BY ==WS-IDEM-CA==.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARM-CA==.
       COPY ARVATWRTCA REPLACING ==DFHCOMMAREA== BY ==WS-VATW-CA==.
       COPY ARPOSTGLCA REPLACING ==DFHCOMMAREA== BY ==WS-POSTGL-CA==.
       01  WS-CUST.
           05  WS-CU-NAME              PIC X(120).
           05  WS-CU-STATUS            PIC X(10).
           05  WS-CU-VAT-NO            PIC X(15).
           05  WS-CU-VAT-LOC           PIC X(3).
           05  WS-CU-CURRENCY          PIC X(3).
           05  WS-CU-TERMS             PIC X(12).
           05  WS-CU-CONTROL           PIC X(12).
           05  WS-CU-INV-CUST          PIC X(20).
           05  WS-CU-ADDRESS           PIC X(300).
           05  WS-CU-EINV              PIC X(2).
           05  WS-CU-PO-REQ            PIC X(2).
           05  WS-CU-OWN-VAT           PIC X(15).
       01  WS-ID-ROW.
           05  WS-NEW-ID               PIC X(24).
           05  WS-ID2                  PIC X(24).
       01  WS-SW.
           05  WS-POST-MODE            PIC X VALUE "N".
           05  WS-TRAN-OPEN            PIC X VALUE "N".
           05  WS-VAT-IX               PIC S9(4) VALUE 0.
           05  WS-FOUND                PIC X VALUE "N".
           05  WS-LEG                  PIC S9(4) VALUE 0.
       01  WS-LIMITS.
           05  WS-SIMPLE-LIMIT         PIC S9(9)V99 VALUE 250.
           05  WS-MAX-TERM-DAYS        PIC S9(5) VALUE 60.
       LINKAGE SECTION.
       COPY ARINVPSTCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE-HEADER
           IF AR-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0300-READ-CUSTOMER
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0500-PRICE-THE-LINES
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0400-TERMS-AND-DUE-DATE
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0600-VAT-INVOICE-CONTENT
           PERFORM 0700-SOURCE-CONTROLS
           IF AR-RC-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE AR-FUNC
               WHEN "VALIDATE" PERFORM 0800-VALIDATE-ONLY
               WHEN "PROFORMA" PERFORM 3000-WRITE-PROFORMA
               WHEN "POST"     PERFORM 1000-POST-INVOICE
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           PERFORM 9900-SET-RESULT
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE AR-WORK
           INITIALIZE AR-CTL
           INITIALIZE WS-CUST
           INITIALIZE WS-SW
           MOVE 250 TO WS-SIMPLE-LIMIT
           MOVE 60 TO WS-MAX-TERM-DAYS
           MOVE CA-FUNCTION OF DFHCOMMAREA TO AR-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO AR-CO
           MOVE CA-SITE OF DFHCOMMAREA TO AR-SITE
           MOVE CA-USER OF DFHCOMMAREA TO AR-USERID
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO AR-BUSDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO AR-CORREL
           MOVE "ARINVPST" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO AR-RC AR-MSG-CNT
           MOVE SPACES TO AR-REASON AR-MSG
           MOVE "AR-004" TO AR-REQ-REF
           MOVE 0 TO AI-MISSING-CNT AI-VAT-CNT
           MOVE 0 TO AI-INVOICE-ID AI-OPEN-ITEM-ID
           MOVE SPACES TO AI-JOURNAL-NO
           MOVE "N" TO AI-DUPLICATE
           MOVE FUNCTION UPPER-CASE(AI-CUSTOMER) TO AI-CUSTOMER
           IF AI-DOC-TYPE = SPACES
               MOVE "INVOICE" TO AI-DOC-TYPE
           END-IF
           IF AI-DOC-DATE = 0
               MOVE AR-BUSDATE TO AI-DOC-DATE
           END-IF
           IF AI-TAX-POINT = 0
               MOVE AI-DOC-DATE TO AI-TAX-POINT
           END-IF
           IF AI-POSTING-DATE = 0
               MOVE AI-DOC-DATE TO AI-POSTING-DATE
           END-IF
           IF AI-CURRENCY = SPACES
               MOVE "GBP" TO AI-CURRENCY
           END-IF
           IF AI-FX-RATE = 0
               MOVE 1 TO AI-FX-RATE
           END-IF
           IF AI-SOURCE = SPACES
               MOVE "DIRECT" TO AI-SOURCE
           END-IF
           IF AI-SIMPLIFIED = SPACE OR AI-SIMPLIFIED = LOW-VALUE
               MOVE "N" TO AI-SIMPLIFIED
           END-IF
           PERFORM 0150-READ-LIMITS.
       0100-EXIT.
           EXIT.

       0150-READ-LIMITS SECTION.
       0150-START.
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE AR-USERID TO CA-USER OF WS-PARM-CA
           MOVE AI-DOC-DATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "AR.SIMPLIFIED.VAT.LIMIT" TO PM-PARAM-CODE
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE "250" TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM-CA
           IF PM-VALUE-NUM > 0
               MOVE PM-VALUE-NUM TO WS-SIMPLE-LIMIT
           END-IF
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO CA-FUNCTION OF WS-PARM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PARM-CA
           MOVE AR-USERID TO CA-USER OF WS-PARM-CA
           MOVE AI-DOC-DATE TO CA-BUSINESS-DATE OF WS-PARM-CA
           MOVE "TERMS.MAX.DAYS" TO PM-PARAM-CODE
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE "60" TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM-CA
           IF PM-VALUE-NUM > 0
               MOVE PM-VALUE-NUM TO WS-MAX-TERM-DAYS
           END-IF.
       0150-EXIT.
           EXIT.

       0200-VALIDATE-HEADER SECTION.
       0200-START.
           IF AR-CO = SPACES
               MOVE "ARI0001" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A company code is required" DELIMITED BY SIZE
                   INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AI-CUSTOMER = SPACES
               MOVE "ARI0002" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A customer account is required"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AI-LINE-CNT < 1 OR AI-LINE-CNT > 25
               MOVE "ARI0003" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "An invoice needs between one and twenty five "
                      "lines" DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF FUNCTION TEST-DATE-YYYYMMDD(AI-DOC-DATE) NOT = 0
               MOVE "ARI0004" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The invoice date is not a calendar date"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF FUNCTION TEST-DATE-YYYYMMDD(AI-TAX-POINT) NOT = 0
               MOVE "ARI0005" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The tax point is not a calendar date"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
           END-IF
           IF AR-FUNC = "POST" AND AR-CORREL = SPACES
               MOVE "ARI0006" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A correlation id is required so a repeated "
                      "instruction cannot bill the same work twice"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0300-READ-CUSTOMER SECTION.
       0300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT c.LegalName, c.Status, "
                  "ISNULL(c.VatRegNo,''), c.VatLocation, "
                  "c.CurrencyCode, ISNULL(c.TermsCode,''), "
                  "ISNULL(c.ArControlAccount,''), "
                  "ISNULL(c.InvoiceCustomerCode, c.CustomerCode), "
                  "ISNULL((SELECT TOP (1) CASE WHEN "
                  "LTRIM(RTRIM(ISNULL(a.Line1,''))) = '' THEN '' "
                  "ELSE RTRIM(a.Line1) + ', ' + "
                  "ISNULL(RTRIM(a.Town) + ', ','') + "
                  "ISNULL(a.Postcode,'') END FROM ar.CustomerAddress a"
                  " WHERE a.CompanyCode = c.CompanyCode AND "
                  "a.CustomerCode = c.CustomerCode AND "
                  "a.AddressRole = 'BILLING'),''), "
                  "CONVERT(char(1), c.EInvoiceEnabled), "
                  "CONVERT(char(1), c.RequiresPurchaseOrderRef), "
                  "ISNULL((SELECT TOP (1) y.VatRegNo FROM core.Company"
                  " y WHERE y.CompanyCode = c.CompanyCode),'') "
                  "FROM ar.Customer c WHERE c.CompanyCode = ? "
                  "AND c.CustomerCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE AI-CUSTOMER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 12 TO SQLIO-COL-CNT
           MOVE 120 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 15 TO SQLIO-COL-WIDTH(3)
           MOVE 3 TO SQLIO-COL-WIDTH(4)
           MOVE 3 TO SQLIO-COL-WIDTH(5)
           MOVE 12 TO SQLIO-COL-WIDTH(6)
           MOVE 12 TO SQLIO-COL-WIDTH(7)
           MOVE 20 TO SQLIO-COL-WIDTH(8)
           MOVE 300 TO SQLIO-COL-WIDTH(9)
           MOVE 2 TO SQLIO-COL-WIDTH(10)
           MOVE 2 TO SQLIO-COL-WIDTH(11)
           MOVE 15 TO SQLIO-COL-WIDTH(12)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CUST
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO AR-RC
               MOVE "ARI0007" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Customer " FUNCTION TRIM(AI-CUSTOMER)
                      " is not held for this company"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 0300-EXIT
           END-IF
           IF AI-BILL-NAME = SPACES
               MOVE WS-CU-NAME TO AI-BILL-NAME
           END-IF
           IF AI-BILL-ADDRESS = SPACES
               MOVE WS-CU-ADDRESS TO AI-BILL-ADDRESS
           END-IF
           IF AI-CUST-VAT-NO = SPACES
               MOVE WS-CU-VAT-NO TO AI-CUST-VAT-NO
           END-IF
           IF AI-OWN-VAT-NO = SPACES
               MOVE WS-CU-OWN-VAT TO AI-OWN-VAT-NO
           END-IF
           IF AI-VAT-LOCATION = SPACES
               MOVE WS-CU-VAT-LOC TO AI-VAT-LOCATION
           END-IF
           IF AI-INVOICE-CUST = SPACES
               MOVE WS-CU-INV-CUST TO AI-INVOICE-CUST
           END-IF
           IF AI-TERMS-CODE = SPACES
               MOVE WS-CU-TERMS TO AI-TERMS-CODE
           END-IF
           IF WS-CU-STATUS = "RETIRED" OR WS-CU-STATUS = "SUSPENDED"
               MOVE 8 TO AR-RC
               MOVE "ARI0008" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Customer " FUNCTION TRIM(AI-CUSTOMER)
                      " is " FUNCTION TRIM(WS-CU-STATUS)
                      " and cannot be invoiced"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 0300-EXIT
           END-IF
           IF WS-CU-PO-REQ(1:1) = "1" AND AI-ORDER-REF = SPACES
               MOVE 8 TO AR-RC
               MOVE "ARI0009" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "This customer requires its own order "
                      "reference on every invoice"
                   DELIMITED BY SIZE INTO AR-MSG
           END-IF.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   AR-003: the agreed terms are captured on the document and a
      *>   later master change does not move the due date
       0400-TERMS-AND-DUE-DATE SECTION.
       0400-START.
           INITIALIZE WS-TERM-CA
           MOVE "DUEDATE" TO CA-FUNCTION OF WS-TERM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-TERM-CA
           MOVE AR-USERID TO CA-USER OF WS-TERM-CA
           MOVE AI-DOC-DATE TO CA-BUSINESS-DATE OF WS-TERM-CA
           MOVE AI-TERMS-CODE TO TC-TERMS-CODE
           MOVE AI-DOC-DATE TO TC-INVOICE-DATE
           MOVE AI-TAX-POINT TO TC-SUPPLY-DATE
           MOVE AI-NET TO TC-NET-AMOUNT
           MOVE AI-VAT TO TC-VAT-AMOUNT
           CALL "TERMCALC" USING WS-TERM-CA
           IF CA-RETURN-CODE OF WS-TERM-CA > 6
               MOVE 8 TO AR-RC
               MOVE CA-REASON-CODE OF WS-TERM-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-TERM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 0400-EXIT
           END-IF
           IF AI-DUE-DATE = 0
               MOVE TC-DUE-DATE TO AI-DUE-DATE
           END-IF
           MOVE TC-DISCOUNT-DATE TO AI-DISCOUNT-DATE
           MOVE TC-DISCOUNT-PCT TO AI-DISCOUNT-PCT
           MOVE TC-TERMS-DESC TO AI-TERMS-SNAPSHOT
           MOVE TC-SOURCE TO AI-TERMS-SOURCE
      *>   a prompt payment discount is shown on the face of the
      *>   invoice, but output VAT is only reduced when it is taken
      *>   (CR2101, 1 April 2015)
           IF TC-DISCOUNT-PCT > 0
               MOVE "DISCOUNT" TO CA-FUNCTION OF WS-TERM-CA
               CALL "TERMCALC" USING WS-TERM-CA
           END-IF
           IF TC-TERM-DAYS > WS-MAX-TERM-DAYS
               MOVE 4 TO AR-RC
               MOVE "ARI0010" TO AR-REASON
               MOVE TC-TERM-DAYS TO AR-INT-ED
               MOVE SPACES TO AR-MSG
               STRING "Payment terms of "
                      FUNCTION TRIM(AR-INT-ED)
                      " days exceed the agreed maximum (D-25)"
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       0400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   price each line, pick up its VAT rate and build the
      *>   per code VAT analysis
       0500-PRICE-THE-LINES SECTION.
       0500-START.
           MOVE 0 TO AI-NET AI-VAT AI-GROSS
           MOVE 0 TO AI-NET-GBP AI-VAT-GBP AI-GROSS-GBP
           MOVE 0 TO AI-VAT-CNT
           PERFORM VARYING AR-I FROM 1 BY 1
               UNTIL AR-I > AI-LINE-CNT OR AR-RC-BAD
               PERFORM 0520-ONE-LINE
           END-PERFORM
           IF AR-RC-BAD
               GO TO 0500-EXIT
           END-IF
           COMPUTE AI-GROSS = AI-NET + AI-VAT
           IF AI-CURRENCY = "GBP"
               MOVE AI-NET TO AI-NET-GBP
               MOVE AI-VAT TO AI-VAT-GBP
               MOVE AI-GROSS TO AI-GROSS-GBP
           ELSE
               COMPUTE AI-GROSS-GBP = AI-NET-GBP + AI-VAT-GBP
           END-IF.
       0500-EXIT.
           EXIT.

       0520-ONE-LINE SECTION.
       0520-START.
           MOVE SPACES TO AI-L-MESSAGE(AR-I)
           IF AI-L-DESC(AR-I) = SPACES
               MOVE "ARI0020" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Every line needs a description sufficient to "
                      "identify the goods or services"
                   DELIMITED BY SIZE INTO AR-MSG
               PERFORM 9000-REJECT
               GO TO 0520-EXIT
           END-IF
           IF AI-L-QTY(AR-I) = 0
               MOVE 1 TO AI-L-QTY(AR-I)
           END-IF
           IF AI-L-NET(AR-I) = 0
      *>       the extension is done in two short steps: a single
      *>       expression over the full COMMAREA precision overflows
      *>       the standard intermediate result
               MOVE AI-L-QTY(AR-I) TO AR-QTY1
               MOVE AI-L-PRICE(AR-I) TO AR-PRICE1
               COMPUTE AR-AMT1 ROUNDED = AR-QTY1 * AR-PRICE1
               IF AI-L-DISC-PCT(AR-I) NOT = 0
                   MOVE AI-L-DISC-PCT(AR-I) TO AR-PCT1
                   COMPUTE AR-AMT1 ROUNDED =
                       AR-AMT1 * (100 - AR-PCT1) / 100
               END-IF
               ADD AI-L-CHARGE(AR-I) TO AR-AMT1
               MOVE AR-AMT1 TO AI-L-NET(AR-I)
           END-IF
      *>   AR-004: the invoice cannot exceed the uninvoiced quantity
           IF AI-L-AVAIL-QTY(AR-I) > 0
              AND AI-L-QTY(AR-I) > AI-L-AVAIL-QTY(AR-I)
               MOVE "ARI0021" TO AR-REASON
               MOVE AI-L-AVAIL-QTY(AR-I) TO AR-QTY-ED
               MOVE SPACES TO AR-MSG
               STRING "Line " DELIMITED BY SIZE
                      AR-I DELIMITED BY SIZE
                      " exceeds the uninvoiced quantity of "
                      FUNCTION TRIM(AR-QTY-ED)
                   DELIMITED BY SIZE INTO AR-MSG
               MOVE AR-MSG TO AI-L-MESSAGE(AR-I)
               PERFORM 9000-REJECT
               GO TO 0520-EXIT
           END-IF
           PERFORM 0540-LINE-VAT
           IF AR-RC-BAD
               GO TO 0520-EXIT
           END-IF
           ADD AI-L-NET(AR-I) TO AI-NET
           ADD AI-L-VAT-AMT(AR-I) TO AI-VAT
           IF AI-CURRENCY NOT = "GBP" AND AI-FX-RATE > 0
               COMPUTE AR-AMT2 ROUNDED =
                   AI-L-NET(AR-I) / AI-FX-RATE
               ADD AR-AMT2 TO AI-NET-GBP
               COMPUTE AR-AMT3 ROUNDED =
                   AI-L-VAT-AMT(AR-I) / AI-FX-RATE
               ADD AR-AMT3 TO AI-VAT-GBP
           ELSE
               MOVE AI-L-NET(AR-I) TO AR-AMT2
               MOVE AI-L-VAT-AMT(AR-I) TO AR-AMT3
           END-IF
           PERFORM 0560-ACCUMULATE-VAT.
       0520-EXIT.
           EXIT.

       0540-LINE-VAT SECTION.
       0540-START.
           INITIALIZE WS-VATW-CA
           MOVE "LOOKUP" TO CA-FUNCTION OF WS-VATW-CA
           MOVE AR-CO TO CA-COMPANY OF WS-VATW-CA
           MOVE AR-USERID TO CA-USER OF WS-VATW-CA
           MOVE AI-TAX-POINT TO CA-BUSINESS-DATE OF WS-VATW-CA
           MOVE AI-L-VAT-CODE(AR-I) TO VW-VAT-CODE
           MOVE AI-TAX-POINT TO VW-TAX-POINT
           MOVE "OUTPUT" TO VW-DIRECTION
           CALL "ARVATWRT" USING WS-VATW-CA
           IF CA-RETURN-CODE OF WS-VATW-CA > 6
               MOVE 8 TO AR-RC
               MOVE CA-REASON-CODE OF WS-VATW-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-VATW-CA TO AR-MSG
               MOVE AR-MSG TO AI-L-MESSAGE(AR-I)
               ADD 1 TO AR-MSG-CNT
               GO TO 0540-EXIT
           END-IF
           MOVE VW-RATE-PCT TO AI-L-VAT-RATE(AR-I)
           COMPUTE AR-AMT1 ROUNDED =
               AI-L-NET(AR-I) * VW-RATE-PCT / 100
           MOVE AR-AMT1 TO AI-L-VAT-AMT(AR-I)
           IF VW-WORDING NOT = SPACES
               AND AI-SPECIAL-WORDING = SPACES
               MOVE VW-WORDING TO AI-SPECIAL-WORDING
           END-IF.
       0540-EXIT.
           EXIT.

       0560-ACCUMULATE-VAT SECTION.
       0560-START.
           MOVE "N" TO WS-FOUND
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > AI-VAT-CNT
               IF AI-V-CODE(AR-J) = AI-L-VAT-CODE(AR-I)
                   MOVE "Y" TO WS-FOUND
                   MOVE AR-J TO WS-VAT-IX
               END-IF
           END-PERFORM
           IF WS-FOUND = "N"
               IF AI-VAT-CNT >= 6
                   MOVE "ARI0022" TO AR-REASON
                   MOVE SPACES TO AR-MSG
                   STRING "An invoice may carry at most six VAT codes"
                       DELIMITED BY SIZE INTO AR-MSG
                   PERFORM 9000-REJECT
                   GO TO 0560-EXIT
               END-IF
               ADD 1 TO AI-VAT-CNT
               MOVE AI-VAT-CNT TO WS-VAT-IX
               MOVE AI-L-VAT-CODE(AR-I) TO AI-V-CODE(WS-VAT-IX)
               MOVE AI-L-VAT-RATE(AR-I) TO AI-V-RATE(WS-VAT-IX)
               MOVE VW-LIABILITY TO AI-V-LIABILITY(WS-VAT-IX)
               MOVE 0 TO AI-V-NET-GBP(WS-VAT-IX)
               MOVE 0 TO AI-V-VAT-GBP(WS-VAT-IX)
           END-IF
           ADD AR-AMT2 TO AI-V-NET-GBP(WS-VAT-IX)
           ADD AR-AMT3 TO AI-V-VAT-GBP(WS-VAT-IX).
       0560-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   AR-023 section 10: an invoice missing a mandatory VAT
      *>   element cannot be issued
       0600-VAT-INVOICE-CONTENT SECTION.
       0600-START.
           MOVE 0 TO AI-MISSING-CNT
           IF AI-BILL-NAME = SPACES
               PERFORM 0650-NOTE-MISSING-NAME
           END-IF
           IF AI-BILL-ADDRESS = SPACES
               PERFORM 0655-NOTE-MISSING-ADDR
           END-IF
           IF AI-OWN-VAT-NO = SPACES
               PERFORM 0660-NOTE-MISSING-VATNO
           END-IF
           IF AI-TAX-POINT = 0
               PERFORM 0665-NOTE-MISSING-TP
           END-IF
           IF AI-IS-PROFORMA
               MOVE SPACES TO AI-SPECIAL-WORDING
               STRING "PRO FORMA - THIS IS NOT A VAT INVOICE"
                   DELIMITED BY SIZE INTO AI-SPECIAL-WORDING
               GO TO 0600-EXIT
           END-IF
           IF AI-SIMPLIFIED = "Y"
               PERFORM 0670-SIMPLIFIED-RULES
           END-IF
           IF AI-DEPOSIT-APPLY > 0
               MOVE AI-DEPOSIT-APPLY TO AR-MONEY-UK
               MOVE SPACES TO AI-SPECIAL-WORDING
               STRING "Deposit of GBP " FUNCTION TRIM(AR-MONEY-UK)
                      " already accounted for on an earlier invoice"
                   DELIMITED BY SIZE INTO AI-SPECIAL-WORDING
           END-IF
           IF AI-CURRENCY NOT = "GBP"
               MOVE AI-VAT-GBP TO AR-MONEY-UK
               MOVE AI-FX-RATE TO AR-RATE-ED
               MOVE SPACES TO AR-TEXT2
               STRING "VAT total GBP " FUNCTION TRIM(AR-MONEY-UK)
                      " at " FUNCTION TRIM(AR-RATE-ED) " "
                      FUNCTION TRIM(AI-CURRENCY) " per GBP"
                   DELIMITED BY SIZE INTO AR-TEXT2
               MOVE SPACES TO AR-SCRATCH
               STRING FUNCTION TRIM(AR-TEXT2) ". "
                      FUNCTION TRIM(AI-SPECIAL-WORDING)
                   DELIMITED BY SIZE INTO AR-SCRATCH
               MOVE AR-SCRATCH TO AI-SPECIAL-WORDING
           END-IF
           IF AI-MISSING-CNT > 0
               MOVE 8 TO AR-RC
               MOVE "ARI0030" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "The document is not a valid VAT invoice: "
                      FUNCTION TRIM(AI-MISSING(1))
                      " is missing" DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       0600-EXIT.
           EXIT.

       0650-NOTE-MISSING-NAME SECTION.
       0650-START.
           ADD 1 TO AI-MISSING-CNT
           MOVE "Customer name" TO AI-MISSING(AI-MISSING-CNT).
       0650-EXIT.
           EXIT.

       0655-NOTE-MISSING-ADDR SECTION.
       0655-START.
           ADD 1 TO AI-MISSING-CNT
           MOVE "Customer address" TO AI-MISSING(AI-MISSING-CNT).
       0655-EXIT.
           EXIT.

       0660-NOTE-MISSING-VATNO SECTION.
       0660-START.
           ADD 1 TO AI-MISSING-CNT
           MOVE "Supplier VAT registration number"
               TO AI-MISSING(AI-MISSING-CNT).
       0660-EXIT.
           EXIT.

       0665-NOTE-MISSING-TP SECTION.
       0665-START.
           ADD 1 TO AI-MISSING-CNT
           MOVE "Tax point" TO AI-MISSING(AI-MISSING-CNT).
       0665-EXIT.
           EXIT.

      *>   a simplified VAT invoice is permitted only for retail type
      *>   supplies of GBP 250 or less including VAT
       0670-SIMPLIFIED-RULES SECTION.
       0670-START.
           COMPUTE AR-AMT1 = AI-GROSS-GBP
           IF AR-AMT1 > WS-SIMPLE-LIMIT
               MOVE 8 TO AR-RC
               MOVE "ARI0031" TO AR-REASON
               MOVE WS-SIMPLE-LIMIT TO AR-MONEY-UK
               MOVE AI-GROSS-GBP TO AR-MONEY-ED
               MOVE SPACES TO AR-MSG
               STRING "A simplified VAT invoice is only permitted up "
                      "to GBP " FUNCTION TRIM(AR-MONEY-UK)
                      " including VAT; this document is GBP "
                      FUNCTION TRIM(AR-MONEY-ED)
                   DELIMITED BY SIZE INTO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       0670-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   AR-004 / AR-005: a despatch is billed once, and direct
      *>   entry cannot be used to invoice an already billed despatch
       0700-SOURCE-CONTROLS SECTION.
       0700-START.
           IF AI-IS-PROFORMA
               GO TO 0700-EXIT
           END-IF
           IF AI-FROM-DIRECT OR AI-FROM-SERVICE
               IF AI-EVIDENCE-REF = SPACES AND AI-ORDER-REF = SPACES
                   MOVE 4 TO AR-RC
                   MOVE "ARI0040" TO AR-REASON
                   MOVE SPACES TO AR-MSG
                   STRING "A direct invoice should carry evidence of "
                          "the charge or the customer reference"
                       DELIMITED BY SIZE INTO AR-MSG
                   ADD 1 TO AR-MSG-CNT
               END-IF
           END-IF
           IF AI-DESPATCH-REF = SPACES
               GO TO 0700-EXIT
           END-IF
           IF AI-FROM-DIRECT
               MOVE 8 TO AR-RC
               MOVE "ARI0041" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "A direct financial invoice may not quote a "
                      "despatch; invoice the despatch itself"
                   DELIMITED BY SIZE INTO AR-MSG
               GO TO 0700-EXIT
           END-IF
           PERFORM 0750-DESPATCH-ALREADY-BILLED.
       0700-EXIT.
           EXIT.

       0750-DESPATCH-ALREADY-BILLED SECTION.
       0750-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) i.InvoiceNumber, '' "
                  "FROM ar.SalesInvoice i WHERE i.CompanyCode = ? "
                  "AND i.DespatchRef = ? AND i.DocumentType = "
                  "'INVOICE' AND i.Status IN ('APPROVED','POSTED')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE AI-DESPATCH-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0750-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               MOVE 8 TO AR-RC
               MOVE "ARI0042" TO AR-REASON
               MOVE SPACES TO AR-MSG
               STRING "Despatch " FUNCTION TRIM(AI-DESPATCH-REF)
                      " has already been invoiced as "
                      FUNCTION TRIM(WS-NEW-ID)
                   DELIMITED BY SIZE INTO AR-MSG
               MOVE "Y" TO AI-DUPLICATE
               ADD 1 TO AR-MSG-CNT
           END-IF.
       0750-EXIT.
           EXIT.

       0800-VALIDATE-ONLY SECTION.
       0800-START.
           IF AR-RC-BAD
               GO TO 0800-EXIT
           END-IF
           MOVE AI-GROSS TO AR-MONEY-UK
           MOVE SPACES TO AR-MSG
           STRING "Invoice validated: "
                  FUNCTION TRIM(AI-CURRENCY) " "
                  FUNCTION TRIM(AR-MONEY-UK) " due "
                  AI-DUE-DATE(7:2) "/" AI-DUE-DATE(5:2) "/"
                  AI-DUE-DATE(1:4)
               DELIMITED BY SIZE INTO AR-MSG.
       0800-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   POST - permission, period, idempotency, number, then the
      *>   document, the journal, the VAT record and the open item in
      *>   one transaction (COM-014 all or nothing)
       1000-POST-INVOICE SECTION.
       1000-START.
           MOVE "Y" TO WS-POST-MODE
           PERFORM 1050-CHECK-PERMISSION
      *>   CR2609.  A hold is a refusal with no business effect, so it
      *>   must stop the posting too.  Testing AR-RC-BAD alone let an
      *>   unauthorised user post the invoice and still be told the
      *>   instruction was held (SOINVRUN then rolled back a committed
      *>   invoice and left BilledQty behind).
           IF AR-RC-BAD OR AR-RC = 6
               GO TO 1000-EXIT
           END-IF
           PERFORM 1100-CHECK-PERIOD
           IF AR-RC-BAD
               GO TO 1000-EXIT
           END-IF
           PERFORM 1150-IDEMPOTENT-BEGIN
           IF AR-RC-BAD OR AI-DUPLICATE = "Y"
               GO TO 1000-EXIT
           END-IF
           PERFORM 1200-DRAW-NUMBER
           IF AR-RC-BAD
               GO TO 1000-EXIT
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "Y" TO WS-TRAN-OPEN
           PERFORM 1300-WRITE-HEADER
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 1400-WRITE-LINES
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 1500-WRITE-VAT-ANALYSIS
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 1600-POST-JOURNAL
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 1700-WRITE-VAT-RECORDS
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 1800-WRITE-OPEN-ITEM
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           PERFORM 1900-MARK-POSTED
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 1000-EXIT
           END-IF
           MOVE "COMT" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "N" TO WS-TRAN-OPEN
           PERFORM 1950-IDEMPOTENT-END
           MOVE AI-GROSS TO AR-MONEY-UK
           MOVE SPACES TO AR-MSG
           STRING "Invoice " FUNCTION TRIM(AI-INVOICE-NO)
                  " posted, " FUNCTION TRIM(AI-CURRENCY) " "
                  FUNCTION TRIM(AR-MONEY-UK) " due "
                  AI-DUE-DATE(7:2) "/" AI-DUE-DATE(5:2) "/"
                  AI-DUE-DATE(1:4)
               DELIMITED BY SIZE INTO AR-MSG.
       1000-EXIT.
           EXIT.

       1050-CHECK-PERMISSION SECTION.
       1050-START.
           INITIALIZE WS-PERM-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PERM-CA
           MOVE AR-SITE TO CA-SITE OF WS-PERM-CA
           MOVE AR-USERID TO CA-USER OF WS-PERM-CA
           MOVE AI-POSTING-DATE TO CA-BUSINESS-DATE OF WS-PERM-CA
           MOVE "AR.INVOICE.POST" TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERM-CA
           IF CA-RETURN-CODE OF WS-PERM-CA > 4
               MOVE 6 TO AR-RC
               MOVE "ARI0050" TO AR-REASON
               MOVE CA-MESSAGE OF WS-PERM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       1050-EXIT.
           EXIT.

       1100-CHECK-PERIOD SECTION.
       1100-START.
           INITIALIZE WS-PERIOD-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-PERIOD-CA
           MOVE AR-CO TO CA-COMPANY OF WS-PERIOD-CA
           MOVE AR-USERID TO CA-USER OF WS-PERIOD-CA
           MOVE AI-POSTING-DATE TO CA-BUSINESS-DATE OF WS-PERIOD-CA
           MOVE AI-POSTING-DATE TO PC-POSTING-DATE
           MOVE AI-DOC-DATE TO PC-DOCUMENT-DATE
           MOVE AI-TAX-POINT TO PC-TAX-POINT
           MOVE "POST" TO PC-ACTION
           CALL "PERIODCK" USING WS-PERIOD-CA
           IF CA-RETURN-CODE OF WS-PERIOD-CA > 4
               MOVE CA-RETURN-CODE OF WS-PERIOD-CA TO AR-RC
               MOVE CA-REASON-CODE OF WS-PERIOD-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-PERIOD-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
           END-IF.
       1100-EXIT.
           EXIT.

      *>   COM-015: a repeated confirmation identifies the original
      *>   invoice instead of billing the same work twice
       1150-IDEMPOTENT-BEGIN SECTION.
       1150-START.
           INITIALIZE WS-IDEM-CA
           MOVE "BEGIN" TO CA-FUNCTION OF WS-IDEM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-IDEM-CA
           MOVE AR-USERID TO CA-USER OF WS-IDEM-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-IDEM-CA
           MOVE AI-POSTING-DATE TO CA-BUSINESS-DATE OF WS-IDEM-CA
           MOVE "ar.SalesInvoice.Post" TO ID-OPERATION
           CALL "IDEMPOT" USING WS-IDEM-CA
           EVALUATE CA-RETURN-CODE OF WS-IDEM-CA
               WHEN 0      CONTINUE
               WHEN 4      MOVE "Y" TO AI-DUPLICATE
                           MOVE ID-RESULT-REF TO AI-INVOICE-NO
                           MOVE 4 TO AR-RC
                           MOVE "ARI0051" TO AR-REASON
                           MOVE CA-MESSAGE OF WS-IDEM-CA TO AR-MSG
                           ADD 1 TO AR-MSG-CNT
               WHEN OTHER  MOVE CA-RETURN-CODE OF WS-IDEM-CA TO AR-RC
                           MOVE CA-REASON-CODE OF WS-IDEM-CA
                               TO AR-REASON
                           MOVE CA-MESSAGE OF WS-IDEM-CA TO AR-MSG
                           ADD 1 TO AR-MSG-CNT
           END-EVALUATE.
       1150-EXIT.
           EXIT.

       1200-DRAW-NUMBER SECTION.
       1200-START.
           IF AI-INVOICE-NO NOT = SPACES
               GO TO 1200-EXIT
           END-IF
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-NUM-CA
           MOVE AR-USERID TO CA-USER OF WS-NUM-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-NUM-CA
           MOVE AI-DOC-DATE TO CA-BUSINESS-DATE OF WS-NUM-CA
           MOVE "SINV" TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF CA-RETURN-CODE OF WS-NUM-CA > 4
               MOVE CA-RETURN-CODE OF WS-NUM-CA TO AR-RC
               MOVE CA-REASON-CODE OF WS-NUM-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-NUM-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 1200-EXIT
           END-IF
           MOVE NG-DOCUMENT-NUMBER TO AI-INVOICE-NO.
       1200-EXIT.
           EXIT.

       1300-WRITE-HEADER SECTION.
       1300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ar.SalesInvoice "
                  "(CompanyCode, DocumentType, InvoiceNumber, "
                  "CustomerCode, InvoiceCustomerCode, DocumentDate, "
                  "TaxPointDate, PostingDate, DueDate, FiscalYear, "
                  "PeriodNo, CurrencyCode, FxRate, NetAmount, "
                  "VatAmount, GrossAmount, NetAmountGBP, "
                  "VatAmountGBP, GrossAmountGBP, SettlementDiscountPct"
                  ", DepositAppliedGBP, TermsCode, TermsSnapshot, "
                  "TermsSource, BillToName, BillToAddress, "
                  "CustomerVatRegNo, CompanyVatRegNo, VatLocation, "
                  "IsSimplifiedInvoice, SpecialWording, "
                  "CustomerOrderRef, SalesOrderNumber, DespatchRef, "
                  "SalespersonCode, Status, CorrelationId, CreatedBy) "
                  "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, NULLIF(?,0), "
                  "NULLIF(?,0), ?, ?, ?, ?, ?, ?, ?, ?, ?, "
                  "?, NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), ?, ?, "
                  "NULLIF(?,''), NULLIF(?,''), ?, ?, NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), 'DRAFT', ?, ?); "
                  "SELECT CONVERT(varchar(24), SCOPE_IDENTITY()), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 37 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           MOVE AI-DOC-TYPE TO SQLIO-PARM-VAL(2)
           MOVE AI-INVOICE-NO TO SQLIO-PARM-VAL(3)
           MOVE AI-CUSTOMER TO SQLIO-PARM-VAL(4)
           MOVE AI-INVOICE-CUST TO SQLIO-PARM-VAL(5)
           MOVE AI-DOC-DATE TO SQLIO-PARM-VAL(6)
           MOVE AI-TAX-POINT TO SQLIO-PARM-VAL(7)
           MOVE AI-POSTING-DATE TO SQLIO-PARM-VAL(8)
           MOVE AI-DUE-DATE TO SQLIO-PARM-VAL(9)
           MOVE PC-FISCAL-YEAR TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(10)
           MOVE PC-PERIOD-NO TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(11)
           MOVE AI-CURRENCY TO SQLIO-PARM-VAL(12)
           MOVE AI-FX-RATE TO AR-RATE-ED
           MOVE FUNCTION TRIM(AR-RATE-ED) TO SQLIO-PARM-VAL(13)
           MOVE AI-NET TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(14)
           MOVE AI-VAT TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(15)
           MOVE AI-GROSS TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(16)
           MOVE AI-NET-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(17)
           MOVE AI-VAT-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(18)
           MOVE AI-GROSS-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(19)
           MOVE AI-DISCOUNT-PCT TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(20)
           MOVE AI-DEPOSIT-APPLY TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(21)
           MOVE AI-TERMS-CODE TO SQLIO-PARM-VAL(22)
           MOVE AI-TERMS-SNAPSHOT TO SQLIO-PARM-VAL(23)
           MOVE AI-TERMS-SOURCE TO SQLIO-PARM-VAL(24)
           MOVE AI-BILL-NAME TO SQLIO-PARM-VAL(25)
           MOVE AI-BILL-ADDRESS TO SQLIO-PARM-VAL(26)
           MOVE AI-CUST-VAT-NO TO SQLIO-PARM-VAL(27)
           MOVE AI-OWN-VAT-NO TO SQLIO-PARM-VAL(28)
           MOVE AI-VAT-LOCATION TO SQLIO-PARM-VAL(29)
           IF AI-SIMPLIFIED = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(30)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(30)
           END-IF
           MOVE AI-SPECIAL-WORDING TO SQLIO-PARM-VAL(31)
           MOVE AI-ORDER-REF TO SQLIO-PARM-VAL(32)
           MOVE AI-SO-NUMBER TO SQLIO-PARM-VAL(33)
           MOVE AI-DESPATCH-REF TO SQLIO-PARM-VAL(34)
           MOVE AI-SALESPERSON TO SQLIO-PARM-VAL(35)
           MOVE AR-CORREL TO SQLIO-PARM-VAL(36)
           MOVE AR-USERID TO SQLIO-PARM-VAL(37)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 37
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1300-EXIT
           END-IF
           COMPUTE AI-INVOICE-ID = FUNCTION NUMVAL(WS-NEW-ID).
       1300-EXIT.
           EXIT.

       1400-WRITE-LINES SECTION.
       1400-START.
           PERFORM VARYING AR-I FROM 1 BY 1
               UNTIL AR-I > AI-LINE-CNT OR AR-RC-BAD
               PERFORM 1450-ONE-LINE-ROW
           END-PERFORM.
       1400-EXIT.
           EXIT.

       1450-ONE-LINE-ROW SECTION.
       1450-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ar.SalesInvoiceLine (InvoiceId, LineNum, "
                  "ItemCode, Description, Quantity, UomCode, "
                  "UnitPrice, PricePerQty, DiscountPct1, NetAmount, "
                  "VatCode, VatRatePct, VatAmount, NetAmountGBP, "
                  "VatAmountGBP, DimCostCentre, DimProject, "
                  "RevenueAccountCode, SalesOrderLine) VALUES "
                  "(?, ?, NULLIF(?,''), ?, ?, NULLIF(?,''), ?, 1, ?, "
                  "?, ?, ?, ?, ?, ?, NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,0))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 18 TO SQLIO-PARM-CNT
           MOVE AI-INVOICE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE AR-I TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE AI-L-ITEM(AR-I) TO SQLIO-PARM-VAL(3)
           MOVE AI-L-DESC(AR-I) TO SQLIO-PARM-VAL(4)
           MOVE AI-L-QTY(AR-I) TO AR-QTY-ED
           MOVE FUNCTION TRIM(AR-QTY-ED) TO SQLIO-PARM-VAL(5)
           MOVE AI-L-UOM(AR-I) TO SQLIO-PARM-VAL(6)
           MOVE AI-L-PRICE(AR-I) TO AR-QTY-ED
           MOVE FUNCTION TRIM(AR-QTY-ED) TO SQLIO-PARM-VAL(7)
           MOVE AI-L-DISC-PCT(AR-I) TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(8)
           MOVE AI-L-NET(AR-I) TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(9)
           MOVE AI-L-VAT-CODE(AR-I) TO SQLIO-PARM-VAL(10)
           MOVE AI-L-VAT-RATE(AR-I) TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(11)
           MOVE AI-L-VAT-AMT(AR-I) TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(12)
           IF AI-CURRENCY = "GBP" OR AI-FX-RATE = 0
               MOVE AI-L-NET(AR-I) TO AR-AMT1
               MOVE AI-L-VAT-AMT(AR-I) TO AR-AMT2
           ELSE
               COMPUTE AR-AMT1 ROUNDED = AI-L-NET(AR-I) / AI-FX-RATE
               COMPUTE AR-AMT2 ROUNDED =
                   AI-L-VAT-AMT(AR-I) / AI-FX-RATE
           END-IF
           MOVE AR-AMT1 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(13)
           MOVE AR-AMT2 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(14)
           MOVE AI-L-COST-CENTRE(AR-I) TO SQLIO-PARM-VAL(15)
           MOVE AI-L-PROJECT(AR-I) TO SQLIO-PARM-VAL(16)
           MOVE AI-L-REV-ACCOUNT(AR-I) TO SQLIO-PARM-VAL(17)
           MOVE AI-L-SO-LINE(AR-I) TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(18)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 18
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1450-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW.
       1450-EXIT.
           EXIT.

       1500-WRITE-VAT-ANALYSIS SECTION.
       1500-START.
           PERFORM VARYING AR-I FROM 1 BY 1
               UNTIL AR-I > AI-VAT-CNT OR AR-RC-BAD
               MOVE SPACES TO SQLIO-STMT
               STRING "INSERT ar.SalesInvoiceVat (InvoiceId, VatCode,"
                      " RatePct, NetAmount, VatAmount, NetAmountGBP, "
                      "VatAmountGBP) VALUES (?, ?, ?, ?, ?, ?, ?)"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 7 TO SQLIO-PARM-CNT
               MOVE AI-INVOICE-ID TO AR-INT-ED
               MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(1)
               MOVE AI-V-CODE(AR-I) TO SQLIO-PARM-VAL(2)
               MOVE AI-V-RATE(AR-I) TO AR-MONEY-ED
               MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(3)
               IF AI-CURRENCY = "GBP" OR AI-FX-RATE = 0
                   MOVE AI-V-NET-GBP(AR-I) TO AR-AMT1
                   MOVE AI-V-VAT-GBP(AR-I) TO AR-AMT2
               ELSE
                   COMPUTE AR-AMT1 ROUNDED =
                       AI-V-NET-GBP(AR-I) * AI-FX-RATE
                   COMPUTE AR-AMT2 ROUNDED =
                       AI-V-VAT-GBP(AR-I) * AI-FX-RATE
               END-IF
               MOVE AR-AMT1 TO AR-MONEY-ED
               MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(4)
               MOVE AR-AMT2 TO AR-MONEY-ED
               MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(5)
               MOVE AI-V-NET-GBP(AR-I) TO AR-MONEY-ED
               MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(6)
               MOVE AI-V-VAT-GBP(AR-I) TO AR-MONEY-ED
               MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(7)
               PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 7
                   MOVE "N" TO SQLIO-PARM-NULL(AR-J)
               END-PERFORM
               MOVE 0 TO SQLIO-COL-CNT
               MOVE 1 TO SQLIO-CURSOR
               MOVE "EXEC" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
               IF SQLIO-ERROR
                   PERFORM 9300-SQL-FAILED
               ELSE
                   MOVE "CLOS" TO SQLIO-FUNC
                   CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
               END-IF
           END-PERFORM.
       1500-EXIT.
           EXIT.

      *>   DR debtors gross, CR revenue per line, CR output VAT.
      *>   Inventory and cost of sales were recognised at despatch and
      *>   are not posted again (specification section 5 step 5).
       1600-POST-JOURNAL SECTION.
       1600-START.
           INITIALIZE WS-POSTGL-CA
           MOVE "POST" TO CA-FUNCTION OF WS-POSTGL-CA
           MOVE AR-CO TO CA-COMPANY OF WS-POSTGL-CA
           MOVE AR-USERID TO CA-USER OF WS-POSTGL-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-POSTGL-CA
           MOVE AI-POSTING-DATE TO CA-BUSINESS-DATE OF WS-POSTGL-CA
           MOVE "AR.INVOICE" TO PG-EVENT-CODE
           MOVE "AR" TO PG-JOURNAL-TYPE
           MOVE "AR" TO PG-SOURCE-MODULE
           MOVE AI-INVOICE-NO TO PG-SOURCE-REF
           MOVE AI-DOC-DATE TO PG-DOC-DATE
           MOVE AI-POSTING-DATE TO PG-POSTING-DATE
           MOVE AI-CURRENCY TO PG-CURRENCY
           MOVE AI-FX-RATE TO PG-FX-RATE
           MOVE "N" TO PG-OWN-TRAN
           MOVE SPACES TO PG-DESCRIPTION
           STRING "Sales invoice " FUNCTION TRIM(AI-INVOICE-NO)
                  " " FUNCTION TRIM(AI-BILL-NAME)
               DELIMITED BY SIZE INTO PG-DESCRIPTION
           MOVE 0 TO WS-LEG
           ADD 1 TO WS-LEG
           MOVE "DEBTOR" TO PG-G-LEG-CODE(WS-LEG)
           MOVE WS-CU-CONTROL TO PG-G-ACCOUNT(WS-LEG)
           MOVE AI-GROSS TO PG-G-DEBIT(WS-LEG)
           MOVE "AR" TO PG-G-SUBLEDGER(WS-LEG)
           MOVE AI-CUSTOMER TO PG-G-SUBREF(WS-LEG)
           MOVE AI-BILL-NAME TO PG-G-NARRATIVE(WS-LEG)
           PERFORM VARYING AR-I FROM 1 BY 1 UNTIL AR-I > AI-LINE-CNT
               ADD 1 TO WS-LEG
               MOVE "REVENUE" TO PG-G-LEG-CODE(WS-LEG)
               MOVE AI-L-REV-ACCOUNT(AR-I) TO PG-G-ACCOUNT(WS-LEG)
               MOVE AI-L-NET(AR-I) TO PG-G-CREDIT(WS-LEG)
               MOVE AI-L-DESC(AR-I) TO PG-G-NARRATIVE(WS-LEG)
               MOVE AI-L-COST-CENTRE(AR-I) TO PG-G-COST-CENTRE(WS-LEG)
               MOVE AI-L-PROJECT(AR-I) TO PG-G-PROJECT(WS-LEG)
               MOVE AI-L-VAT-CODE(AR-I) TO PG-G-VAT-CODE(WS-LEG)
           END-PERFORM
           IF AI-VAT NOT = 0
               ADD 1 TO WS-LEG
               MOVE "VAT" TO PG-G-LEG-CODE(WS-LEG)
               MOVE AI-VAT TO PG-G-CREDIT(WS-LEG)
               MOVE SPACES TO PG-G-NARRATIVE(WS-LEG)
               STRING "Output VAT on " FUNCTION TRIM(AI-INVOICE-NO)
                   DELIMITED BY SIZE INTO PG-G-NARRATIVE(WS-LEG)
           END-IF
           MOVE WS-LEG TO PG-LEG-CNT
           CALL "ARPOSTGL" USING WS-POSTGL-CA
           IF CA-RETURN-CODE OF WS-POSTGL-CA > 4
               MOVE CA-RETURN-CODE OF WS-POSTGL-CA TO AR-RC
               MOVE CA-REASON-CODE OF WS-POSTGL-CA TO AR-REASON
               MOVE CA-MESSAGE OF WS-POSTGL-CA TO AR-MSG
               ADD 1 TO AR-MSG-CNT
               GO TO 1600-EXIT
           END-IF
           MOVE PG-JOURNAL-NO TO AI-JOURNAL-NO.
       1600-EXIT.
           EXIT.

       1700-WRITE-VAT-RECORDS SECTION.
       1700-START.
           PERFORM VARYING AR-I FROM 1 BY 1
               UNTIL AR-I > AI-VAT-CNT OR AR-RC-BAD
               INITIALIZE WS-VATW-CA
               MOVE "WRITE" TO CA-FUNCTION OF WS-VATW-CA
               MOVE AR-CO TO CA-COMPANY OF WS-VATW-CA
               MOVE AR-USERID TO CA-USER OF WS-VATW-CA
               MOVE AI-TAX-POINT TO CA-BUSINESS-DATE OF WS-VATW-CA
               MOVE "AR" TO VW-SOURCE-MODULE
               MOVE "SALESINVOICE" TO VW-DOC-TYPE
               MOVE AI-INVOICE-NO TO VW-DOC-REF
               MOVE "CUSTOMER" TO VW-PARTY-TYPE
               MOVE AI-CUSTOMER TO VW-PARTY-CODE
               MOVE AI-CUST-VAT-NO TO VW-PARTY-VAT-NO
               MOVE AI-TAX-POINT TO VW-TAX-POINT
               MOVE AI-POSTING-DATE TO VW-POSTING-DATE
               MOVE AI-V-CODE(AR-I) TO VW-VAT-CODE
               MOVE "OUTPUT" TO VW-DIRECTION
               MOVE AI-CURRENCY TO VW-CURRENCY
               MOVE AI-FX-RATE TO VW-FX-RATE
               MOVE PG-JOURNAL-ID TO VW-JOURNAL-ID
               IF AI-CURRENCY = "GBP" OR AI-FX-RATE = 0
                   MOVE AI-V-NET-GBP(AR-I) TO VW-NET
                   MOVE AI-V-VAT-GBP(AR-I) TO VW-VAT
               ELSE
                   COMPUTE VW-NET ROUNDED =
                       AI-V-NET-GBP(AR-I) * AI-FX-RATE
                   COMPUTE VW-VAT ROUNDED =
                       AI-V-VAT-GBP(AR-I) * AI-FX-RATE
               END-IF
               MOVE AI-V-NET-GBP(AR-I) TO VW-NET-GBP
               MOVE AI-V-VAT-GBP(AR-I) TO VW-VAT-GBP
               MOVE AI-EVIDENCE-REF TO VW-EVIDENCE-REF
               CALL "ARVATWRT" USING WS-VATW-CA
               IF CA-RETURN-CODE OF WS-VATW-CA > 4
                   MOVE CA-RETURN-CODE OF WS-VATW-CA TO AR-RC
                   MOVE CA-REASON-CODE OF WS-VATW-CA TO AR-REASON
                   MOVE CA-MESSAGE OF WS-VATW-CA TO AR-MSG
                   ADD 1 TO AR-MSG-CNT
               END-IF
           END-PERFORM.
       1700-EXIT.
           EXIT.

      *>   one invoice, one receivable (AR-004)
       1800-WRITE-OPEN-ITEM SECTION.
       1800-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ar.OpenItem (CompanyCode, "
                  "CustomerCode, ItemType, DocumentNumber, "
                  "DocumentDate, DueDate, PostingDate, FiscalYear, "
                  "PeriodNo, CurrencyCode, FxRate, OriginalAmount, "
                  "OriginalAmountGBP, ResidualAmount, "
                  "ResidualAmountGBP, VatAmountGBP, TermsCode, "
                  "DiscountDueDate, DiscountAmount, SourceInvoiceId, "
                  "CreatedBy) VALUES (?, ?, 'INVOICE', ?, ?, ?, ?, "
                  "NULLIF(?,0), NULLIF(?,0), ?, ?, ?, ?, ?, ?, ?, "
                  "NULLIF(?,''), NULLIF(?,''), ?, ?, ?); "
                  "SELECT CONVERT(varchar(24), SCOPE_IDENTITY()), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 21 TO SQLIO-PARM-CNT
           MOVE AR-CO TO SQLIO-PARM-VAL(1)
           IF AI-INVOICE-CUST = SPACES
               MOVE AI-CUSTOMER TO SQLIO-PARM-VAL(2)
           ELSE
               MOVE AI-INVOICE-CUST TO SQLIO-PARM-VAL(2)
           END-IF
           MOVE AI-INVOICE-NO TO SQLIO-PARM-VAL(3)
           MOVE AI-DOC-DATE TO SQLIO-PARM-VAL(4)
           MOVE AI-DUE-DATE TO SQLIO-PARM-VAL(5)
           MOVE AI-POSTING-DATE TO SQLIO-PARM-VAL(6)
           MOVE PC-FISCAL-YEAR TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(7)
           MOVE PC-PERIOD-NO TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(8)
           MOVE AI-CURRENCY TO SQLIO-PARM-VAL(9)
           MOVE AI-FX-RATE TO AR-RATE-ED
           MOVE FUNCTION TRIM(AR-RATE-ED) TO SQLIO-PARM-VAL(10)
           COMPUTE AR-AMT1 = AI-GROSS - AI-DEPOSIT-APPLY
           MOVE AR-AMT1 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(11)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(13)
           COMPUTE AR-AMT2 = AI-GROSS-GBP - AI-DEPOSIT-APPLY
           MOVE AR-AMT2 TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(12)
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(14)
           MOVE AI-VAT-GBP TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(15)
           MOVE AI-TERMS-CODE TO SQLIO-PARM-VAL(16)
           IF AI-DISCOUNT-DATE = 0
               MOVE SPACES TO SQLIO-PARM-VAL(17)
           ELSE
               MOVE AI-DISCOUNT-DATE TO SQLIO-PARM-VAL(17)
           END-IF
           MOVE TC-DISCOUNT-AMT TO AR-MONEY-ED
           MOVE FUNCTION TRIM(AR-MONEY-ED) TO SQLIO-PARM-VAL(18)
           MOVE AI-INVOICE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(19)
           MOVE AR-USERID TO SQLIO-PARM-VAL(20)
           MOVE AR-USERID TO SQLIO-PARM-VAL(21)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 21
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1800-EXIT
           END-IF
           COMPUTE AI-OPEN-ITEM-ID = FUNCTION NUMVAL(WS-NEW-ID).
       1800-EXIT.
           EXIT.

       1900-MARK-POSTED SECTION.
       1900-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.SalesInvoice SET Status = 'POSTED', "
                  "PostedBy = ?, PostedAt = SYSDATETIME(), "
                  "IssuedAt = SYSDATETIME(), JournalId = "
                  "(SELECT TOP (1) j.JournalId FROM fin.Journal j "
                  "WHERE j.CompanyCode = ? AND j.JournalNumber = ?) "
                  "WHERE InvoiceId = ?; "
                  "UPDATE ar.Customer SET LastTransactionDate = ?, "
                  "FirstTradedDate = ISNULL(FirstTradedDate, ?) "
                  "WHERE CompanyCode = ? AND CustomerCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 8 TO SQLIO-PARM-CNT
           MOVE AR-USERID TO SQLIO-PARM-VAL(1)
           MOVE AR-CO TO SQLIO-PARM-VAL(2)
           MOVE AI-JOURNAL-NO TO SQLIO-PARM-VAL(3)
           MOVE AI-INVOICE-ID TO AR-INT-ED
           MOVE FUNCTION TRIM(AR-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE AI-DOC-DATE TO SQLIO-PARM-VAL(5)
           MOVE AI-DOC-DATE TO SQLIO-PARM-VAL(6)
           MOVE AR-CO TO SQLIO-PARM-VAL(7)
           MOVE AI-CUSTOMER TO SQLIO-PARM-VAL(8)
           PERFORM VARYING AR-J FROM 1 BY 1 UNTIL AR-J > 8
               MOVE "N" TO SQLIO-PARM-NULL(AR-J)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1900-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW.
       1900-EXIT.
           EXIT.

       1950-IDEMPOTENT-END SECTION.
       1950-START.
           INITIALIZE WS-IDEM-CA
           MOVE "END" TO CA-FUNCTION OF WS-IDEM-CA
           MOVE AR-CO TO CA-COMPANY OF WS-IDEM-CA
           MOVE AR-USERID TO CA-USER OF WS-IDEM-CA
           MOVE AR-CORREL TO CA-CORRELATION-ID OF WS-IDEM-CA
           MOVE AI-POSTING-DATE TO CA-BUSINESS-DATE OF WS-IDEM-CA
           MOVE "ar.SalesInvoice.Post" TO ID-OPERATION
           MOVE "COMPLETED" TO ID-STATUS
           MOVE AI-INVOICE-NO TO ID-RESULT-REF
           MOVE "Sales invoice posted" TO ID-RESULT-MESSAGE
           CALL "IDEMPOT" USING WS-IDEM-CA.
       1950-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   PROFORMA - clearly marked, creates no receivable and no VAT
       3000-WRITE-PROFORMA SECTION.
       3000-START.
           MOVE "PROFORMA" TO AI-DOC-TYPE
           PERFORM 1200-DRAW-NUMBER
           IF AR-RC-BAD
               GO TO 3000-EXIT
           END-IF
           MOVE 0 TO PC-FISCAL-YEAR PC-PERIOD-NO
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "Y" TO WS-TRAN-OPEN
           PERFORM 1300-WRITE-HEADER
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 3000-EXIT
           END-IF
           PERFORM 1400-WRITE-LINES
           IF AR-RC-BAD
               PERFORM 9500-ROLLBACK
               GO TO 3000-EXIT
           END-IF
           MOVE "COMT" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "N" TO WS-TRAN-OPEN
           MOVE 4 TO AR-RC
           MOVE "ARI0060" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "Pro forma " FUNCTION TRIM(AI-INVOICE-NO)
                  " produced - this is not a VAT invoice and "
                  "creates no receivable"
               DELIMITED BY SIZE INTO AR-MSG
           ADD 1 TO AR-MSG-CNT.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE 8 TO AR-RC
           MOVE "Y" TO AR-ERROR
           ADD 1 TO AR-MSG-CNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE 8 TO AR-RC
           MOVE "ARI0098" TO AR-REASON
           MOVE SPACES TO AR-MSG
           STRING "ARINVPST does not support function ["
                  FUNCTION TRIM(AR-FUNC) "]"
               DELIMITED BY SIZE INTO AR-MSG.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE 12 TO AR-RC
           MOVE "SQL0001" TO AR-REASON
           MOVE SQLIO-ERRMSG TO AR-MSG
           ADD 1 TO AR-MSG-CNT.
       9300-EXIT.
           EXIT.

       9500-ROLLBACK SECTION.
       9500-START.
           IF WS-TRAN-OPEN = "Y"
               MOVE "ROLB" TO SQLIO-FUNC
               MOVE 0 TO SQLIO-COL-CNT
               CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
               MOVE "N" TO WS-TRAN-OPEN
           END-IF.
       9500-EXIT.
           EXIT.

       9900-SET-RESULT SECTION.
       9900-START.
           MOVE AR-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE AR-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE AR-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE AR-MSG-CNT TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE AR-REQ-REF TO CA-REQ-REF OF DFHCOMMAREA.
       9900-EXIT.
           EXIT.
