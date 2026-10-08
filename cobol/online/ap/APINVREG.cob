      *>================================================================
      *> PROGRAM-ID : APINVREG
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : SUPPLIER INVOICE AND CREDIT NOTE REGISTER.
      *>              REGISTRATION IS A RECORD THAT THE DOCUMENT
      *>              EXISTS AND WHO OWNS IT.  IT IS NOT AN EXPENSE,
      *>              NOT A STOCK RECEIPT, NOT AN APPROVED LIABILITY
      *>              AND NOT PERMISSION TO PAY (AP-003) - THE
      *>              DOCUMENT IS ALWAYS WRITTEN PAYMENT BLOCKED.
      *>              EXACT DUPLICATES ARE REFUSED ON THE NORMALISED
      *>              REFERENCE SO THAT CASE OR SURROUNDING SPACE
      *>              CANNOT SLIP ONE THROUGH; SIMILAR DOCUMENTS ARE
      *>              REPORTED FOR REVIEW, NOT REJECTED (AP-004).
      *> CALLED BY  : .NET INVOICE REGISTER FORM, APBULK, APEINV
      *> INTERFACE  : CALL "APINVREG" USING WS-APINVREG-CA
      *>              COPYBOOK APINVREGCA
      *> CALLS      : SQLIO, APSVC, APINVLIN, APSUPVAL
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-04-02 RJH  ORIGINAL MVS PORT
      *> 1992-03-17 DJM  CR0241 DUPLICATE REFERENCE CHECK ADDED AFTER
      *>                 THE DOUBLE PAYMENT TO MIDLAND CASTINGS
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS
      *> 2009-07-13 AJS  CR2450 NORMALISED REFERENCE, NOT RAW TEXT
      *> 2026-10-05 STERP STATE MODEL, OWNER, E-INVOICE CHANNEL
      *>================================================================
      *> REQ: AP-003, AP-004, AP-005, AP-009, AP-021, AP-023, AP-025
      *> REQ: AP-031
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APINVREG.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 02/04/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY APSVCARE.
       COPY APLINWRK.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
           05  WS-TRAN-SW              PIC X VALUE "N".
               88  WS-IN-TRAN                    VALUE "Y".
           05  WS-EOF-SW               PIC X VALUE "N".
               88  WS-EOF                        VALUE "Y".
       01  WS-DUP-ROW.
           05  WS-D-REGISTER           PIC X(30).
       01  WS-SIM-ROW.
           05  WS-S-REGISTER           PIC X(30).
           05  WS-S-REF                PIC X(40).
           05  WS-S-DATE               PIC X(8).
           05  WS-S-GROSS              PIC X(24).
           05  WS-S-REASON             PIC X(40).
       01  WS-ID-ROW.
           05  WS-I-VALUE              PIC X(24).
       01  WS-VAT-SUMMARY.
           05  WS-VS-CNT               PIC 9(3) VALUE 0.
           05  WS-VS OCCURS 8 TIMES.
               10  WS-VS-CODE          PIC X(4).
               10  WS-VS-RATE          PIC S9(5)V9(4).
               10  WS-VS-NET           PIC S9(13)V99.
               10  WS-VS-VAT           PIC S9(13)V99.
               10  WS-VS-NETG          PIC S9(13)V99.
               10  WS-VS-VATG          PIC S9(13)V99.
               10  WS-VS-REC           PIC S9(13)V99.
               10  WS-VS-VBOX          PIC 9(2).
               10  WS-VS-NBOX          PIC 9(2).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-HIT                  PIC S9(4) VALUE 0.
           05  WS-NUM                  PIC S9(15)V9(6) VALUE 0.
           05  WS-ROUND-TOL            PIC S9(11)V99 VALUE 0.
           05  WS-VAT-TOL              PIC S9(11)V99 VALUE 0.
           05  WS-SELF-VAT             PIC S9(13)V99 VALUE 0.
           05  WS-SERIES               PIC X(20) VALUE SPACES.
           05  WS-BLOCKED-LINES        PIC 9(4) VALUE 0.
           05  WS-HELD-LINES           PIC 9(4) VALUE 0.
           05  WS-MONEY-ED             PIC -(11)9.99.
           05  WS-NONPO-LIST           PIC X(200) VALUE SPACES.
       LINKAGE SECTION.
       COPY APINVREGCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           PERFORM 1100-EDIT-HEADER THRU 1100-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "DUPCHK"   PERFORM 2000-DUPLICATE THRU 2000-EXIT
               WHEN "VALIDATE" PERFORM 3000-VALIDATE THRU 3000-EXIT
               WHEN "REGISTER" PERFORM 4000-REGISTER THRU 4000-EXIT
               WHEN "SETSTATE" PERFORM 5000-SETSTATE THRU 5000-EXIT
               WHEN "GET"      PERFORM 6000-GET THRU 6000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNC THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE "N" TO WS-FATAL-SW
           MOVE "N" TO WS-TRAN-SW
           MOVE "APINVREG" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "AP-003" TO CA-REQ-REF
           MOVE "N" TO IR-DUP-FOUND
           MOVE SPACES TO IR-DUP-REGISTER
           MOVE 0 TO IR-SIMILAR-CNT
           MOVE 0 TO WS-BLOCKED-LINES
           MOVE 0 TO WS-HELD-LINES
           MOVE 0 TO WS-SELF-VAT
           MOVE 0 TO WS-VS-CNT
           MOVE FUNCTION UPPER-CASE(IR-SUPPLIER) TO IR-SUPPLIER
           MOVE FUNCTION UPPER-CASE(CA-COMPANY) TO CA-COMPANY
           IF IR-DOC-TYPE = SPACES OR IR-DOC-TYPE = LOW-VALUES
               MOVE "INVOICE" TO IR-DOC-TYPE
           END-IF
           IF IR-SOURCE-CHANNEL = SPACES
               MOVE "PAPER" TO IR-SOURCE-CHANNEL
           END-IF
           IF IR-FX-RATE = 0
               MOVE 1 TO IR-FX-RATE
           END-IF
           IF IR-CURRENCY = SPACES OR IR-CURRENCY = LOW-VALUES
               MOVE "GBP" TO IR-CURRENCY
           END-IF
           PERFORM 1050-NORMALISE THRU 1050-EXIT.
       1000-EXIT.
           EXIT.

       1050-NORMALISE SECTION.
       1050-START.
           INITIALIZE WS-APSVC
           MOVE "NORMREF" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE IR-SUPPLIER-REF TO SVC-TEXT-3
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-TEXT-OUT(1:40) TO IR-REF-NORMALISED.
       1050-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1100-EDIT-HEADER SECTION.
       1100-START.
           IF CA-COMPANY = SPACES OR CA-COMPANY = LOW-VALUES
               MOVE "APR0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1100-EXIT
           END-IF
           IF CA-FUNCTION = "GET" OR CA-FUNCTION = "SETSTATE"
               GO TO 1100-EXIT
           END-IF
           IF IR-SUPPLIER = SPACES OR IR-SUPPLIER = LOW-VALUES
               MOVE "APR0002" TO CA-REASON-CODE
               MOVE "A supplier account code is required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1100-EXIT
           END-IF
           IF IR-SUPPLIER-REF = SPACES
              OR IR-SUPPLIER-REF = LOW-VALUES
               MOVE "APR0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The supplier's own document reference is "
                      "required" DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1100-EXIT
           END-IF
           IF NOT (IR-INVOICE OR IR-CREDITNOTE)
               MOVE "APR0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The document type must be INVOICE or "
                      "CREDITNOTE" DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1100-EXIT
           END-IF
           IF IR-DATE-RECEIVED = 0
               MOVE CA-BUSINESS-DATE TO IR-DATE-RECEIVED
           END-IF
           IF IR-DOCUMENT-DATE = 0
               MOVE "APR0005" TO CA-REASON-CODE
               MOVE "The supplier's document date is required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1100-EXIT
           END-IF
           IF IR-TAX-POINT = 0
               MOVE IR-DOCUMENT-DATE TO IR-TAX-POINT
           END-IF
           IF IR-POSTING-DATE = 0
               MOVE CA-BUSINESS-DATE TO IR-POSTING-DATE
           END-IF
           IF IR-DATE-RECEIVED < IR-DOCUMENT-DATE
               MOVE "APR0006" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A document cannot be received before it was "
                      "issued" DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AP-004.  THE EXACT TEST USES COMPANY, SUPPLIER, DOCUMENT TYPE
      *> AND THE NORMALISED REFERENCE.  A SAME AMOUNT OR SAME DATE
      *> DOCUMENT UNDER A DIFFERENT REFERENCE IS A WARNING THAT NEEDS
      *> A HUMAN DECISION - IT IS NOT PROOF OF DUPLICATION.
      *>----------------------------------------------------------------
       2000-DUPLICATE SECTION.
       2000-START.
           PERFORM 2100-EXACT THRU 2100-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           IF IR-DUP-FOUND = "Y"
               MOVE "DUPLICATE" TO IR-REG-STATE
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APR0010" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Supplier document "
                      FUNCTION TRIM(IR-SUPPLIER-REF)
                      " is already registered as "
                      FUNCTION TRIM(IR-DUP-REGISTER)
                      "; registering it again is refused"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-SIMILAR THRU 2200-EXIT
           IF IR-SIMILAR-CNT > 0
               MOVE 4 TO CA-RETURN-CODE
               MOVE "APR0011" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A similar document is already registered for "
                      "this supplier; review it before proceeding"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           ELSE
               IF CA-RETURN-CODE = 0
                   MOVE SPACES TO CA-MESSAGE
                   STRING "No duplicate or similar document found"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               END-IF
           END-IF.
       2000-EXIT.
           EXIT.

       2100-EXACT SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) RegisterNumber "
                  "FROM ap.PurchaseInvoice WHERE CompanyCode = ? "
                  "AND SupplierCode = ? AND DocumentType = ? "
                  "AND SupplierDocRefNormalised = ? "
                  "AND RegistrationState <> 'REJECTED' "
                  "AND (? = 0 OR PurchaseInvoiceId <> ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE IR-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE IR-DOC-TYPE TO SQLIO-PARM-VAL(3)
           MOVE IR-REF-NORMALISED TO SQLIO-PARM-VAL(4)
           MOVE IR-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-DUP-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2100-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               MOVE "Y" TO IR-DUP-FOUND
               MOVE WS-D-REGISTER TO IR-DUP-REGISTER
           END-IF
           MOVE 1 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-DUP-ROW.
       2100-EXIT.
           EXIT.

       2200-SIMILAR SECTION.
       2200-START.
           MOVE 0 TO IR-SIMILAR-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (10) RegisterNumber, SupplierDocRef, "
                  "CONVERT(char(8), DocumentDate, 112), "
                  "CAST(StatedGross AS varchar(24)), "
                  "CASE WHEN StatedGross = CAST(? AS decimal(18,2)) "
                  "AND DocumentDate = CAST(? AS date) THEN "
                  "'Same amount and date' WHEN StatedGross = "
                  "CAST(? AS decimal(18,2)) THEN 'Same amount' "
                  "ELSE 'Same document date' END "
                  "FROM ap.PurchaseInvoice WHERE CompanyCode = ? "
                  "AND SupplierCode = ? AND DocumentType = ? "
                  "AND SupplierDocRefNormalised <> ? "
                  "AND RegistrationState <> 'REJECTED' AND "
                  "(StatedGross = CAST(? AS decimal(18,2)) OR "
                  "DocumentDate = CAST(? AS date)) "
                  "ORDER BY PurchaseInvoiceId DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 9 TO SQLIO-PARM-CNT
           MOVE IR-STATED-GROSS TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE IR-DOCUMENT-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE SQLIO-PARM-VAL(1) TO SQLIO-PARM-VAL(3)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(4)
           MOVE IR-SUPPLIER TO SQLIO-PARM-VAL(5)
           MOVE IR-DOC-TYPE TO SQLIO-PARM-VAL(6)
           MOVE IR-REF-NORMALISED TO SQLIO-PARM-VAL(7)
           MOVE SQLIO-PARM-VAL(1) TO SQLIO-PARM-VAL(8)
           MOVE SQLIO-PARM-VAL(2) TO SQLIO-PARM-VAL(9)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 40 TO SQLIO-COL-WIDTH(5)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SIM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2200-EXIT
           END-IF
           MOVE "N" TO WS-EOF-SW
           PERFORM UNTIL WS-EOF OR IR-SIMILAR-CNT >= 10
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-SIM-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-EOF-SW
               ELSE
                   ADD 1 TO IR-SIMILAR-CNT
                   MOVE IR-SIMILAR-CNT TO WS-I
                   MOVE WS-S-REGISTER TO IR-S-REGISTER(WS-I)
                   MOVE WS-S-REF TO IR-S-REF(WS-I)
                   COMPUTE WS-NUM = FUNCTION NUMVAL(WS-S-DATE)
                   MOVE WS-NUM TO IR-S-DATE(WS-I)
                   COMPUTE WS-NUM = FUNCTION NUMVAL(WS-S-GROSS)
                   MOVE WS-NUM TO IR-S-GROSS(WS-I)
                   MOVE WS-S-REASON TO IR-S-REASON(WS-I)
               END-IF
           END-PERFORM
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SIM-ROW.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> VALIDATE CODES EVERY LINE, BUILDS THE VAT ANALYSIS AND
      *> COMPARES THE ENTERED DETAIL WITH THE TOTALS THE SUPPLIER
      *> STATED.  A DIFFERENCE BEYOND THE D-08 TOLERANCES STOPS THE
      *> DOCUMENT GOING FURTHER THAN THE REGISTER.
      *>----------------------------------------------------------------
       3000-VALIDATE SECTION.
       3000-START.
           PERFORM 3050-TOLERANCES THRU 3050-EXIT
           PERFORM 3100-DUE-DATES THRU 3100-EXIT
           PERFORM 3200-LINES THRU 3200-EXIT
           PERFORM 3300-TOTALS THRU 3300-EXIT.
       3000-EXIT.
           EXIT.

       3050-TOLERANCES SECTION.
       3050-START.
           INITIALIZE WS-APSVC
           MOVE "PARMNUM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "AP.INVOICE.ROUNDING.GBP" TO SVC-TEXT-1
           MOVE 1 TO SVC-NUM-1
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-NUM-OUT TO WS-ROUND-TOL
           INITIALIZE WS-APSVC
           MOVE "PARMNUM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "AP.VAT.TOL.GBP" TO SVC-TEXT-1
           MOVE 0.05 TO SVC-NUM-1
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-NUM-OUT TO WS-VAT-TOL
           INITIALIZE WS-APSVC
           MOVE "PARMTXT" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "AP.NONPO.CATEGORIES" TO SVC-TEXT-1
           MOVE "UTILITIES,RATES,RENT" TO SVC-TEXT-2
           CALL "APSVC" USING WS-APSVC
           MOVE SVC-TEXT-OUT TO WS-NONPO-LIST.
       3050-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AP-009: THE DUE DATE COMES FROM THE RECORDED TERMS AND THE
      *> SUPPLIER'S DOCUMENT DATE.  INTERNAL DELAY IN GETTING THE PAPER
      *> ONTO THE SYSTEM DOES NOT RESTART THE CONTRACTUAL CLOCK, SO
      *> THE DATE RECEIVED IS NEVER USED AS THE BASIS.
      *>----------------------------------------------------------------
       3100-DUE-DATES SECTION.
       3100-START.
           INITIALIZE WS-APSVC
           MOVE "TERMS" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE IR-TERMS-CODE TO SVC-TEXT-1
           MOVE IR-DOCUMENT-DATE TO SVC-DATE-1
           MOVE IR-STATED-NET TO SVC-NUM-1
           MOVE IR-STATED-VAT TO SVC-NUM-2
           CALL "APSVC" USING WS-APSVC
           IF SVC-RC < 8
               MOVE SVC-DATE-OUT TO IR-DUE-DATE
               MOVE SVC-DATE-OUT2 TO IR-DISCOUNT-DATE
               MOVE SVC-INT-OUT TO IR-TERM-DAYS
           END-IF
           IF IR-DUE-DATE = 0
               MOVE IR-DOCUMENT-DATE TO IR-DUE-DATE
           END-IF.
       3100-EXIT.
           EXIT.

       3200-LINES SECTION.
       3200-START.
           MOVE 0 TO IR-CALC-NET IR-CALC-VAT IR-CALC-GROSS
           MOVE 0 TO IR-NET-GBP IR-VAT-GBP IR-GROSS-GBP
           MOVE 0 TO IR-RECOVERABLE-GBP IR-NONRECOVER-GBP
           MOVE 0 TO WS-VS-CNT WS-SELF-VAT
           MOVE 0 TO WS-BLOCKED-LINES WS-HELD-LINES
           IF IR-LINE-CNT = 0
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APR0012" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A registered document must have at least one "
                      "coded line" DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 3200-EXIT
           END-IF
           IF IR-LINE-CNT > 40
               MOVE 40 TO IR-LINE-CNT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > IR-LINE-CNT
               PERFORM 3210-ONE-LINE THRU 3210-EXIT
           END-PERFORM
           COMPUTE IR-CALC-GROSS = IR-CALC-NET + IR-CALC-VAT
           COMPUTE IR-GROSS-GBP = IR-NET-GBP + IR-VAT-GBP.
       3200-EXIT.
           EXIT.

       3210-ONE-LINE SECTION.
       3210-START.
           INITIALIZE WS-APLIN
           MOVE "BOTH" TO LN-FUNC
           MOVE CA-COMPANY TO LN-COMPANY
           MOVE CA-BUSINESS-DATE TO LN-DATE
           MOVE WS-I TO LN-LINE-NUM
           MOVE IR-L-DESC(WS-I) TO LN-DESCRIPTION
           MOVE IR-L-ITEM(WS-I) TO LN-ITEM
           MOVE IR-L-ORDER(WS-I) TO LN-ORDER
           MOVE IR-L-RECEIPT(WS-I) TO LN-RECEIPT
           MOVE IR-L-SERVICE(WS-I) TO LN-SERVICE
           MOVE IR-L-QTY(WS-I) TO LN-QTY
           MOVE IR-L-UOM(WS-I) TO LN-UOM
           MOVE IR-L-PRICE(WS-I) TO LN-PRICE
           MOVE IR-L-PRICE-PER(WS-I) TO LN-PRICE-PER
           MOVE IR-L-CHARGE(WS-I) TO LN-CHARGE
           MOVE IR-L-NET(WS-I) TO LN-NET
           MOVE IR-L-VATCODE(WS-I) TO LN-VAT-CODE
           MOVE IR-TAX-POINT TO LN-TAX-POINT
           MOVE IR-FX-RATE TO LN-FX-RATE
           MOVE IR-L-ACCOUNT(WS-I) TO LN-ACCOUNT
           MOVE IR-L-COST-CTR(WS-I) TO LN-COST-CTR
           MOVE IR-L-PROJECT(WS-I) TO LN-PROJECT
           MOVE IR-L-NATURE(WS-I) TO LN-NATURE
           MOVE IR-NONPO-REASON TO LN-NONPO-REASON
           MOVE WS-NONPO-LIST TO LN-NONPO-ALLOWED
           IF IR-EVIDENCE-TYPE = "PROFORMA"
              OR IR-EVIDENCE-TYPE = "STATEMENT"
              OR IR-EVIDENCE-TYPE = "NONE"
               MOVE "N" TO LN-VAT-EVIDENCE
           ELSE
               MOVE "Y" TO LN-VAT-EVIDENCE
           END-IF
           CALL "APINVLIN" USING WS-APLIN
           MOVE LN-NET TO IR-L-NET(WS-I)
           MOVE LN-VAT TO IR-L-VAT(WS-I)
           MOVE LN-RATE-PCT TO IR-L-VATRATE(WS-I)
           MOVE LN-NET-GBP TO IR-L-NET-GBP(WS-I)
           MOVE LN-VAT-GBP TO IR-L-VAT-GBP(WS-I)
           MOVE LN-NATURE TO IR-L-NATURE(WS-I)
           MOVE LN-STATUS TO IR-L-STATUS(WS-I)
           MOVE LN-MESSAGE TO IR-L-MESSAGE(WS-I)
           MOVE LN-RECOVERABLE TO IR-L-RECOVER(WS-I)
           MOVE WS-I TO IR-L-NUM(WS-I)
           IF LN-RC > 6
               ADD 1 TO WS-BLOCKED-LINES
           END-IF
           IF LN-RC = 6
               ADD 1 TO WS-HELD-LINES
           END-IF
           ADD LN-NET TO IR-CALC-NET
           ADD LN-VAT TO IR-CALC-VAT
           ADD LN-NET-GBP TO IR-NET-GBP
           ADD LN-VAT-GBP TO IR-VAT-GBP
           ADD LN-SELF-VAT-GBP TO WS-SELF-VAT
           ADD LN-RECOVER-GBP TO IR-RECOVERABLE-GBP
           ADD LN-NONRECOVER-GBP TO IR-NONRECOVER-GBP
           PERFORM 3220-ADD-VAT-SUMMARY THRU 3220-EXIT.
       3210-EXIT.
           EXIT.

       3220-ADD-VAT-SUMMARY SECTION.
       3220-START.
           MOVE 0 TO WS-HIT
           PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-VS-CNT OR WS-HIT > 0
               IF WS-VS-CODE(WS-J) = LN-VAT-CODE
                   MOVE WS-J TO WS-HIT
               END-IF
           END-PERFORM
           IF WS-HIT = 0
               IF WS-VS-CNT >= 8
                   GO TO 3220-EXIT
               END-IF
               ADD 1 TO WS-VS-CNT
               MOVE WS-VS-CNT TO WS-HIT
               MOVE LN-VAT-CODE TO WS-VS-CODE(WS-HIT)
               MOVE LN-RATE-PCT TO WS-VS-RATE(WS-HIT)
               MOVE 0 TO WS-VS-NET(WS-HIT)
               MOVE 0 TO WS-VS-VAT(WS-HIT)
               MOVE 0 TO WS-VS-NETG(WS-HIT)
               MOVE 0 TO WS-VS-VATG(WS-HIT)
               MOVE 0 TO WS-VS-REC(WS-HIT)
               MOVE LN-VAT-BOX TO WS-VS-VBOX(WS-HIT)
               MOVE LN-NET-BOX TO WS-VS-NBOX(WS-HIT)
           END-IF
           ADD LN-NET TO WS-VS-NET(WS-HIT)
           ADD LN-VAT TO WS-VS-VAT(WS-HIT)
           ADD LN-NET-GBP TO WS-VS-NETG(WS-HIT)
           ADD LN-VAT-GBP TO WS-VS-VATG(WS-HIT)
           ADD LN-SELF-VAT-GBP TO WS-VS-VATG(WS-HIT)
           ADD LN-RECOVER-GBP TO WS-VS-REC(WS-HIT).
       3220-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3300-TOTALS SECTION.
       3300-START.
           COMPUTE IR-NET-DIFF = IR-STATED-NET - IR-CALC-NET
           COMPUTE IR-VAT-DIFF = IR-STATED-VAT - IR-CALC-VAT
           IF IR-STATED-GROSS = 0
               COMPUTE IR-STATED-GROSS =
                   IR-STATED-NET + IR-STATED-VAT
           END-IF
           MOVE "READY" TO IR-REG-STATE
           IF WS-BLOCKED-LINES > 0
               MOVE "INCOMPLETE" TO IR-REG-STATE
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APR0013" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "One or more lines are not validly coded; "
                      "posting is blocked until they are"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 3300-EXIT
           END-IF
           IF FUNCTION ABS(IR-NET-DIFF) > WS-ROUND-TOL
               MOVE "INCOMPLETE" TO IR-REG-STATE
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APR0014" TO CA-REASON-CODE
               MOVE IR-NET-DIFF TO WS-MONEY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "The entered lines do not support the stated "
                      "net total; the difference is GBP "
                      FUNCTION TRIM(WS-MONEY-ED)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 3300-EXIT
           END-IF
           IF FUNCTION ABS(IR-VAT-DIFF) > WS-VAT-TOL
               MOVE "INCOMPLETE" TO IR-REG-STATE
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APR0015" TO CA-REASON-CODE
               MOVE IR-VAT-DIFF TO WS-MONEY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "The VAT difference of GBP "
                      FUNCTION TRIM(WS-MONEY-ED)
                      " is beyond the approved tolerance"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 3300-EXIT
           END-IF
           IF WS-HELD-LINES > 0
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APR0016" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "One or more lines need an authorised non-PO "
                      "exception" DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       3300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4000-REGISTER SECTION.
       4000-START.
           INITIALIZE WS-APSVC
           MOVE "PERM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           IF IR-CREDITNOTE
               MOVE "AP.CREDITNOTE.ENTER" TO SVC-TEXT-1
           ELSE
               MOVE "AP.INVOICE.ENTER" TO SVC-TEXT-1
           END-IF
           CALL "APSVC" USING WS-APSVC
           IF SVC-FLAG-1 NOT = "Y"
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APR0017" TO CA-REASON-CODE
               MOVE SVC-MESSAGE TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 4000-EXIT
           END-IF
           PERFORM 2000-DUPLICATE THRU 2000-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           PERFORM 3000-VALIDATE THRU 3000-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           PERFORM 4100-NUMBER THRU 4100-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           PERFORM 4200-BEGIN THRU 4200-EXIT
           PERFORM 4300-INSERT-HEADER THRU 4300-EXIT
           IF WS-FATAL
               PERFORM 4900-ROLLBACK THRU 4900-EXIT
               GO TO 4000-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > IR-LINE-CNT OR WS-FATAL
               PERFORM 4400-INSERT-LINE THRU 4400-EXIT
           END-PERFORM
           IF WS-FATAL
               PERFORM 4900-ROLLBACK THRU 4900-EXIT
               GO TO 4000-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-VS-CNT OR WS-FATAL
               PERFORM 4500-INSERT-VAT THRU 4500-EXIT
           END-PERFORM
           IF WS-FATAL
               PERFORM 4900-ROLLBACK THRU 4900-EXIT
               GO TO 4000-EXIT
           END-IF
           PERFORM 4800-COMMIT THRU 4800-EXIT
           PERFORM 4600-AUDIT THRU 4600-EXIT
           IF CA-RETURN-CODE < 6
               MOVE 0 TO CA-RETURN-CODE
               MOVE SPACES TO CA-REASON-CODE
           END-IF
           MOVE SPACES TO CA-MESSAGE
           STRING "Registered as "
                  FUNCTION TRIM(IR-REGISTER-NUMBER)
                  ". Registration is not approval to pay"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       4000-EXIT.
           EXIT.

       4100-NUMBER SECTION.
       4100-START.
           IF IR-CREDITNOTE
               MOVE "PCRN" TO WS-SERIES
           ELSE
               MOVE "PINV" TO WS-SERIES
           END-IF
           INITIALIZE WS-APSVC
           MOVE "NUMBER" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE WS-SERIES TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-RC > 4
               MOVE "Y" TO WS-FATAL-SW
               MOVE 12 TO CA-RETURN-CODE
               MOVE SVC-REASON TO CA-REASON-CODE
               MOVE SVC-MESSAGE TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 4100-EXIT
           END-IF
           MOVE SVC-TEXT-OUT(1:30) TO IR-REGISTER-NUMBER.
       4100-EXIT.
           EXIT.

       4200-BEGIN SECTION.
       4200-START.
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           MOVE "Y" TO WS-TRAN-SW.
       4200-EXIT.
           EXIT.

       4300-INSERT-HEADER SECTION.
       4300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; "
                  "INSERT ap.PurchaseInvoice (CompanyCode, "
                  "DocumentType, RegisterNumber, SupplierCode, "
                  "SupplierDocRef, SupplierDocRefNormalised, "
                  "DateReceived, DocumentDate, TaxPointDate, "
                  "GoodsReceivedDate, PostingDate, DueDate, "
                  "CurrencyCode, FxRate, FxRateDate, StatedNet, "
                  "StatedVat, StatedGross, NetAmountGBP, "
                  "VatAmountGBP, GrossAmountGBP, RecoverableVatGBP, "
                  "NonRecoverableVatGBP, VatEvidenceValid, TermsCode,"
                  " AgreedTermDays, RegistrationState, MatchState, "
                  "AccountingState, PaymentEligibility, "
                  "SettlementState, OwnerUser, IsNonPo, NonPoReason, "
                  "AttachmentRef, EInvoiceRef, BlockReason, "
                  "CorrelationId, CreatedBy) "
                  "SELECT ?, ?, ?, ?, ?, ?, CAST(? AS date), "
                  "CAST(? AS date), CAST(? AS date), "
                  "CASE WHEN ? = 0 THEN NULL ELSE CAST(? AS date) "
                  "END, CAST(? AS date), CAST(? AS date), ?, ?, "
                  "CAST(? AS date), ?, ?, ?, ?, ?, ?, ?, ?, ?, "
                  "NULLIF(?,''), NULLIF(?,0), ?, ?, 'UNPOSTED', "
                  "'BLOCKED', 'UNPAID', NULLIF(?,''), ?, "
                  "NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), ?, "
                  "NULLIF(?,''), ?; "
                  "SELECT CAST(SCOPE_IDENTITY() AS varchar(24))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE IR-DOC-TYPE TO SQLIO-PARM-VAL(2)
           MOVE IR-REGISTER-NUMBER TO SQLIO-PARM-VAL(3)
           MOVE IR-SUPPLIER TO SQLIO-PARM-VAL(4)
           MOVE IR-SUPPLIER-REF TO SQLIO-PARM-VAL(5)
           MOVE IR-REF-NORMALISED TO SQLIO-PARM-VAL(6)
           MOVE IR-DATE-RECEIVED TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE IR-DOCUMENT-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE IR-TAX-POINT TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE IR-GOODS-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE IR-POSTING-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE IR-DUE-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(13)
           MOVE IR-CURRENCY TO SQLIO-PARM-VAL(14)
           MOVE IR-FX-RATE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(15)
           MOVE IR-DOCUMENT-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(16)
           MOVE IR-STATED-NET TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(17)
           MOVE IR-STATED-VAT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(18)
           MOVE IR-STATED-GROSS TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(19)
           MOVE IR-NET-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(20)
           MOVE IR-VAT-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(21)
           MOVE IR-GROSS-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(22)
           MOVE IR-RECOVERABLE-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(23)
           MOVE IR-NONRECOVER-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(24)
           IF IR-EVIDENCE-TYPE = "PROFORMA"
              OR IR-EVIDENCE-TYPE = "STATEMENT"
              OR IR-EVIDENCE-TYPE = "NONE"
               MOVE "0" TO SQLIO-PARM-VAL(25)
           ELSE
               MOVE "1" TO SQLIO-PARM-VAL(25)
           END-IF
           MOVE IR-TERMS-CODE TO SQLIO-PARM-VAL(26)
           MOVE IR-TERM-DAYS TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(27)
           MOVE IR-REG-STATE TO SQLIO-PARM-VAL(28)
           IF IR-MATCH-STATE = SPACES
               MOVE "UNMATCHED" TO SQLIO-PARM-VAL(29)
           ELSE
               MOVE IR-MATCH-STATE TO SQLIO-PARM-VAL(29)
           END-IF
           MOVE IR-OWNER-USER TO SQLIO-PARM-VAL(30)
           IF IR-IS-NON-PO = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(31)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(31)
           END-IF
           MOVE IR-NONPO-REASON TO SQLIO-PARM-VAL(32)
           MOVE IR-ATTACHMENT-REF TO SQLIO-PARM-VAL(33)
           MOVE IR-EINVOICE-REF TO SQLIO-PARM-VAL(34)
           MOVE SPACES TO SQLIO-PARM-VAL(35)
           STRING "Registered; not yet approved for payment"
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(35)
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(36)
           MOVE CA-USER TO SQLIO-PARM-VAL(37)
           MOVE 37 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 37
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 4300-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-I-VALUE)
           MOVE WS-NUM TO IR-INVOICE-ID
           MOVE 1 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW.
       4300-EXIT.
           EXIT.

       4400-INSERT-LINE SECTION.
       4400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ap.PurchaseInvoiceLine (PurchaseInvoiceId, "
                  "LineNum, Description, ItemCode, "
                  "PurchaseOrderNumber, PurchaseOrderLine, "
                  "ReceiptRef, ReceiptLine, ServiceAcceptanceRef, "
                  "Quantity, UomCode, ConversionFactor, UnitPrice, "
                  "PricePerQty, ChargeAmount, NetAmount, VatCode, "
                  "VatRatePct, VatAmount, NetAmountGBP, VatAmountGBP,"
                  " IsRecoverableVat, AccountCode, DimCostCentre, "
                  "DimProject, JobRef, CostNature) VALUES (?, ?, ?, "
                  "NULLIF(?,''), NULLIF(?,''), NULLIF(?,0), "
                  "NULLIF(?,''), NULLIF(?,0), NULLIF(?,''), ?, "
                  "NULLIF(?,''), ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, "
                  "NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE IR-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE WS-I TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE IR-L-DESC(WS-I) TO SQLIO-PARM-VAL(3)
           MOVE IR-L-ITEM(WS-I) TO SQLIO-PARM-VAL(4)
           MOVE IR-L-ORDER(WS-I) TO SQLIO-PARM-VAL(5)
           MOVE IR-L-ORDER-LINE(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE IR-L-RECEIPT(WS-I) TO SQLIO-PARM-VAL(7)
           MOVE IR-L-RECEIPT-LN(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE IR-L-SERVICE(WS-I) TO SQLIO-PARM-VAL(9)
           MOVE IR-L-QTY(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE IR-L-UOM(WS-I) TO SQLIO-PARM-VAL(11)
           IF IR-L-CONV(WS-I) = 0
               MOVE 1 TO IR-L-CONV(WS-I)
           END-IF
           MOVE IR-L-CONV(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE IR-L-PRICE(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           IF IR-L-PRICE-PER(WS-I) = 0
               MOVE 1 TO IR-L-PRICE-PER(WS-I)
           END-IF
           MOVE IR-L-PRICE-PER(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(14)
           MOVE IR-L-CHARGE(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(15)
           MOVE IR-L-NET(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(16)
           MOVE IR-L-VATCODE(WS-I) TO SQLIO-PARM-VAL(17)
           MOVE IR-L-VATRATE(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(18)
           MOVE IR-L-VAT(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(19)
           MOVE IR-L-NET-GBP(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(20)
           MOVE IR-L-VAT-GBP(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(21)
           IF IR-L-RECOVER(WS-I) = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(22)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(22)
           END-IF
           MOVE IR-L-ACCOUNT(WS-I) TO SQLIO-PARM-VAL(23)
           MOVE IR-L-COST-CTR(WS-I) TO SQLIO-PARM-VAL(24)
           MOVE IR-L-PROJECT(WS-I) TO SQLIO-PARM-VAL(25)
           MOVE IR-L-JOB(WS-I) TO SQLIO-PARM-VAL(26)
           MOVE IR-L-NATURE(WS-I) TO SQLIO-PARM-VAL(27)
           MOVE 27 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 27
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       4400-EXIT.
           EXIT.

       4500-INSERT-VAT SECTION.
       4500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ap.PurchaseInvoiceVat (PurchaseInvoiceId, "
                  "VatCode, RatePct, NetAmount, VatAmount, "
                  "NetAmountGBP, VatAmountGBP, RecoverableGBP, "
                  "VatBox, NetBox) VALUES (?, ?, ?, ?, ?, ?, ?, ?, "
                  "NULLIF(?,0), NULLIF(?,0))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE IR-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE WS-VS-CODE(WS-I) TO SQLIO-PARM-VAL(2)
           MOVE WS-VS-RATE(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE WS-VS-NET(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE WS-VS-VAT(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE WS-VS-NETG(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE WS-VS-VATG(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE WS-VS-REC(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE WS-VS-VBOX(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE WS-VS-NBOX(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE 10 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 10
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       4500-EXIT.
           EXIT.

       4600-AUDIT SECTION.
       4600-START.
           INITIALIZE WS-APSVC
           MOVE "AUDIT" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "ap.PurchaseInvoice" TO SVC-AUD-ENTITY
           MOVE IR-REGISTER-NUMBER TO SVC-AUD-KEY
           MOVE "REGISTER" TO SVC-AUD-ACTION
           MOVE "SupplierDocRef" TO SVC-AUD-FIELD
           MOVE IR-SUPPLIER-REF TO SVC-AUD-NEW
           MOVE IR-SOURCE-CHANNEL TO SVC-AUD-REASON
           MOVE IR-ATTACHMENT-REF(1:40) TO SVC-AUD-AUTH
           MOVE "N" TO SVC-AUD-SENS
           CALL "APSVC" USING WS-APSVC.
       4600-EXIT.
           EXIT.

       4800-COMMIT SECTION.
       4800-START.
           IF WS-IN-TRAN
               MOVE "COMT" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
               MOVE "N" TO WS-TRAN-SW
           END-IF.
       4800-EXIT.
           EXIT.

       4900-ROLLBACK SECTION.
       4900-START.
           IF WS-IN-TRAN
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
               MOVE "N" TO WS-TRAN-SW
           END-IF
           MOVE SPACES TO IR-REGISTER-NUMBER.
       4900-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AP-008: THE REGISTRATION, MATCH, ACCOUNTING, PAYMENT AND
      *> SETTLEMENT STATES ARE SEPARATE.  A POSTED LIABILITY CAN STAY
      *> PAYMENT BLOCKED, AND AN OWNER IS ALWAYS RECORDED.
      *>----------------------------------------------------------------
       5000-SETSTATE SECTION.
       5000-START.
           IF IR-REGISTER-NUMBER = SPACES AND IR-INVOICE-ID = 0
               MOVE "APR0018" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A register number or document identity is "
                      "required" DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PurchaseInvoice SET RegistrationState = "
                  "COALESCE(NULLIF(?,''), RegistrationState), "
                  "OwnerUser = COALESCE(NULLIF(?,''), OwnerUser), "
                  "PaymentEligibility = COALESCE(NULLIF(?,''), "
                  "PaymentEligibility), BlockReason = "
                  "COALESCE(NULLIF(?,''), BlockReason), "
                  "DisputeReason = COALESCE(NULLIF(?,''), "
                  "DisputeReason), ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHERE CompanyCode = ? AND "
                  "((? <> '' AND RegisterNumber = ?) OR "
                  "(? <> 0 AND PurchaseInvoiceId = ?))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE IR-REG-STATE TO SQLIO-PARM-VAL(1)
           MOVE IR-OWNER-USER TO SQLIO-PARM-VAL(2)
           MOVE IR-PAY-ELIGIBILITY TO SQLIO-PARM-VAL(3)
           MOVE IR-BLOCK-REASON TO SQLIO-PARM-VAL(4)
           MOVE IR-DISPUTE-REASON TO SQLIO-PARM-VAL(5)
           MOVE CA-USER TO SQLIO-PARM-VAL(6)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(7)
           MOVE IR-REGISTER-NUMBER TO SQLIO-PARM-VAL(8)
           MOVE IR-REGISTER-NUMBER TO SQLIO-PARM-VAL(9)
           MOVE IR-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE 11 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 11
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO CA-MESSAGE
           STRING "Document state updated" DELIMITED BY SIZE
               INTO CA-MESSAGE
           PERFORM 6000-GET THRU 6000-EXIT.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       6000-GET SECTION.
       6000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CAST(PurchaseInvoiceId AS "
                  "varchar(24)) FROM ap.PurchaseInvoice "
                  "WHERE CompanyCode = ? AND ((? <> '' AND "
                  "RegisterNumber = ?) OR (? <> 0 AND "
                  "PurchaseInvoiceId = ?))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE IR-REGISTER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE IR-REGISTER-NUMBER TO SQLIO-PARM-VAL(3)
           MOVE IR-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 6000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "APR0019" TO CA-REASON-CODE
               MOVE "That registered document was not found"
                   TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               GO TO 6000-EXIT
           END-IF
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-I-VALUE)
           MOVE WS-NUM TO IR-INVOICE-ID.
       6000-EXIT.
           MOVE 1 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           EXIT.
      *>----------------------------------------------------------------
       8500-EXEC-UPDATE SECTION.
       8500-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 8500-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW.
       8500-EXIT.
           EXIT.

       8900-SVC-HEADER SECTION.
       8900-START.
           MOVE CA-COMPANY TO SVC-COMPANY
           MOVE CA-SITE TO SVC-SITE
           MOVE CA-USER TO SVC-USER
           MOVE CA-BUSINESS-DATE TO SVC-DATE
           MOVE CA-CORRELATION-ID TO SVC-CORRELATION.
       8900-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-FATAL-SW
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNC SECTION.
       9100-START.
           MOVE "APR0099" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "APINVREG does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE "Y" TO WS-FATAL-SW
           MOVE 12 TO CA-RETURN-CODE
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in APINVREG" TO CA-MESSAGE
           END-IF
           ADD 1 TO CA-MSG-COUNT
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW.
       9300-EXIT.
           EXIT.
