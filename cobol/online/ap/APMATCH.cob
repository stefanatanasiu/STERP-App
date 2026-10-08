      *>================================================================
      *> PROGRAM-ID : APMATCH
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (CREDITORS)
      *> PURPOSE    : THREE WAY MATCH OF A PURCHASE INVOICE LINE TO
      *>              THE ORDER AND THE ELIGIBLE RECEIPT.
      *>                remaining matchable quantity =
      *>                  commercially accepted receipt quantity
      *>                  - attributable returned quantity
      *>                  - quantity already matched to non reversed
      *>                    invoices
      *>              OWNED QUARANTINE STOCK IS NOT COMMERCIALLY
      *>              ACCEPTED, SO IT NEVER FEEDS AN AUTOMATIC MATCH.
      *>              QUANTITY, PRICE, CHARGE AND VAT ARE ASSESSED
      *>              AGAINST SEPARATE TOLERANCES, AND BEING INSIDE
      *>              THE PRICE TOLERANCE NEVER AUTHORISES PAYMENT FOR
      *>              QUANTITY THAT WAS NOT RECEIVED (AP-007).
      *>              A ZERO REFERENCE PRICE HAS NO MEANINGFUL
      *>              PERCENTAGE: IT FALLS TO THE ABSOLUTE RULE AND
      *>              THE RESULT IS STATED.
      *> CALLED BY  : .NET MATCHING FORM, APBULK, APCLOSE
      *> INTERFACE  : CALL "APMATCH" USING WS-APMATCH-CA
      *>              COPYBOOK APMATCHCA
      *> CALLS      : SQLIO, APSVC
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1991-06-20 RJH  ORIGINAL THREE WAY MATCH
      *> 1996-11-08 DJM  CR0690 RETURNS DEDUCTED FROM ACCEPTED ONLY
      *>                 WHERE THEY WERE PREVIOUSLY ACCEPTED
      *> 2002-09-30 KMP  CR1601 PRICE PER BASIS AND UNIT CONVERSION
      *> 2026-10-05 STERP SEPARATE CHARGE AND VAT TOLERANCES (D-08)
      *>================================================================
      *> REQ: AP-006, AP-007, AP-010, AP-022, D-08
       IDENTIFICATION DIVISION.
       PROGRAM-ID. APMATCH.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 20/06/1991.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY APSVCARE.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
           05  WS-EOF-SW               PIC X VALUE "N".
               88  WS-EOF                        VALUE "Y".
           05  WS-TRAN-SW              PIC X VALUE "N".
               88  WS-IN-TRAN                    VALUE "Y".
       01  WS-ORDER-ROW.
           05  WS-O-ORDERED            PIC X(24).
           05  WS-O-PRICE              PIC X(24).
           05  WS-O-PRICEPER           PIC X(24).
           05  WS-O-CONV               PIC X(24).
           05  WS-O-CHARGE             PIC X(24).
           05  WS-O-ACCEPTED           PIC X(24).
           05  WS-O-RETURNED           PIC X(24).
           05  WS-O-INVOICED           PIC X(24).
           05  WS-O-VATRATE            PIC X(24).
       01  WS-RECEIPT-ROW.
           05  WS-R-ACCEPTED           PIC X(24).
           05  WS-R-QUARANTINE         PIC X(24).
           05  WS-R-RETURNED           PIC X(24).
           05  WS-R-MATCHED            PIC X(24).
       01  WS-PREV-ROW.
           05  WS-P-MATCHED            PIC X(24).
       01  WS-LINE-ROW.
           05  WS-L-NUM                PIC X(8).
           05  WS-L-ORDER              PIC X(30).
           05  WS-L-ORDERLINE          PIC X(8).
           05  WS-L-RECEIPT            PIC X(30).
           05  WS-L-RECEIPTLINE        PIC X(8).
           05  WS-L-SERVICE            PIC X(30).
           05  WS-L-ITEM               PIC X(40).
           05  WS-L-QTY                PIC X(24).
           05  WS-L-CONV               PIC X(24).
           05  WS-L-PRICE              PIC X(24).
           05  WS-L-PRICEPER           PIC X(24).
           05  WS-L-CHARGE             PIC X(24).
           05  WS-L-NET                PIC X(24).
           05  WS-L-VAT                PIC X(24).
           05  WS-L-VATRATE            PIC X(24).
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(40).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-NUM                  PIC S9(15)V9(6) VALUE 0.
           05  WS-INV-UNIT             PIC S9(13)V9(6) VALUE 0.
           05  WS-ORD-UNIT             PIC S9(13)V9(6) VALUE 0.
           05  WS-EXPECTED-VAT         PIC S9(13)V99 VALUE 0.
           05  WS-ABS-PCT              PIC S9(9)V9(4) VALUE 0.
           05  WS-PCT-ED               PIC -(5)9.9999.
           05  WS-QTY-ED               PIC -(9)9.999999.
           05  WS-MONEY-ED             PIC -(11)9.99.
       LINKAGE SECTION.
       COPY APMATCHCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "TOLCHK"   PERFORM 3000-TOLERANCE THRU 3000-EXIT
               WHEN "MATCH"    PERFORM 2000-MATCH THRU 2000-EXIT
               WHEN "MATCHALL" PERFORM 5000-MATCHALL THRU 5000-EXIT
               WHEN "CONFIRM"  PERFORM 4000-CONFIRM THRU 4000-EXIT
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
           MOVE "APMATCH" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "AP-006" TO CA-REQ-REF
           MOVE SPACES TO MT-NOTE MT-TOLERANCE-NOTE
           MOVE "N" TO MT-ZERO-REFERENCE
           MOVE "N" TO MT-NEEDS-APPROVAL
           IF MT-BASIS = SPACES OR MT-BASIS = LOW-VALUES
               MOVE "QUANTITY" TO MT-BASIS
           END-IF
           IF MT-INV-PRICE-PER = 0
               MOVE 1 TO MT-INV-PRICE-PER
           END-IF
           IF MT-ORD-PRICE-PER = 0
               MOVE 1 TO MT-ORD-PRICE-PER
           END-IF
           IF MT-INV-CONV = 0
               MOVE 1 TO MT-INV-CONV
           END-IF
           IF MT-ORD-CONV = 0
               MOVE 1 TO MT-ORD-CONV
           END-IF
           MOVE FUNCTION UPPER-CASE(CA-COMPANY) TO CA-COMPANY
           IF CA-COMPANY = SPACES
               MOVE "APM0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF
           PERFORM 1100-TOLERANCES THRU 1100-EXIT.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> TOLERANCES SUPPLIED BY THE CALLER WIN; ANYTHING LEFT AT ZERO
      *> FALLS BACK TO THE D-08 PARAMETERS.
      *>----------------------------------------------------------------
       1100-TOLERANCES SECTION.
       1100-START.
           IF MT-TOL-PRICE-PCT = 0
               MOVE "PO.PRICE.TOL.PCT" TO SVC-TEXT-1
               MOVE 2 TO SVC-NUM-1
               PERFORM 8100-PARMNUM THRU 8100-EXIT
               MOVE SVC-NUM-OUT TO MT-TOL-PRICE-PCT
           END-IF
           IF MT-TOL-PRICE-GBP = 0
               MOVE "PO.PRICE.TOL.GBP" TO SVC-TEXT-1
               MOVE 50 TO SVC-NUM-1
               PERFORM 8100-PARMNUM THRU 8100-EXIT
               MOVE SVC-NUM-OUT TO MT-TOL-PRICE-GBP
           END-IF
           IF MT-TOL-CHARGE-GBP = 0
               MOVE "PO.FREIGHT.TOL.GBP" TO SVC-TEXT-1
               MOVE 25 TO SVC-NUM-1
               PERFORM 8100-PARMNUM THRU 8100-EXIT
               MOVE SVC-NUM-OUT TO MT-TOL-CHARGE-GBP
           END-IF
           IF MT-TOL-VAT-GBP = 0
               MOVE "AP.VAT.TOL.GBP" TO SVC-TEXT-1
               MOVE 0.05 TO SVC-NUM-1
               PERFORM 8100-PARMNUM THRU 8100-EXIT
               MOVE SVC-NUM-OUT TO MT-TOL-VAT-GBP
           END-IF
           IF MT-TOL-ROUND-GBP = 0
               MOVE "AP.INVOICE.ROUNDING.GBP" TO SVC-TEXT-1
               MOVE 1 TO SVC-NUM-1
               PERFORM 8100-PARMNUM THRU 8100-EXIT
               MOVE SVC-NUM-OUT TO MT-TOL-ROUND-GBP
           END-IF.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2000-MATCH SECTION.
       2000-START.
           PERFORM 2100-READ-ORDER THRU 2100-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-READ-RECEIPT THRU 2200-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           PERFORM 2300-READ-PREVIOUS THRU 2300-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           PERFORM 3000-TOLERANCE THRU 3000-EXIT.
       2000-EXIT.
           EXIT.

       2100-READ-ORDER SECTION.
       2100-START.
           IF MT-ORDER-NUMBER = SPACES
               MOVE "NOTREQUIRED" TO MT-INVOICE-STATE
               GO TO 2100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CAST(ol.OrderedQty AS varchar(24)),"
                  " CAST(ol.UnitPrice AS varchar(24)), "
                  "CAST(ol.PricePerQty AS varchar(24)), "
                  "CAST(ol.ConversionFactor AS varchar(24)), "
                  "CAST(ol.ChargeAmount AS varchar(24)), "
                  "CAST(ol.AcceptedQty AS varchar(24)), "
                  "CAST(ol.ReturnedQty AS varchar(24)), "
                  "CAST(ol.InvoicedQty AS varchar(24)), "
                  "CAST(ISNULL(ol.VatRatePct,0) AS varchar(24)) "
                  "FROM po.PurchaseOrderLine ol "
                  "WHERE ol.CompanyCode = ? AND ol.OrderNumber = ? "
                  "AND ol.LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE MT-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE MT-ORDER-LINE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 9 TO SQLIO-COL-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 9
               MOVE 24 TO SQLIO-COL-WIDTH(WS-I)
           END-PERFORM
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ORDER-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "APM0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Order line "
                      FUNCTION TRIM(MT-ORDER-NUMBER)
                      " was not found, so the invoice line cannot "
                      "be matched" DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 2100-EXIT
           END-IF
           COMPUTE MT-ORD-QTY = FUNCTION NUMVAL(WS-O-ORDERED)
           COMPUTE MT-ORD-PRICE = FUNCTION NUMVAL(WS-O-PRICE)
           COMPUTE MT-ORD-PRICE-PER = FUNCTION NUMVAL(WS-O-PRICEPER)
           COMPUTE MT-ORD-CONV = FUNCTION NUMVAL(WS-O-CONV)
           COMPUTE MT-ORD-CHARGE = FUNCTION NUMVAL(WS-O-CHARGE)
           IF MT-ORD-PRICE-PER = 0
               MOVE 1 TO MT-ORD-PRICE-PER
           END-IF
           IF MT-ORD-CONV = 0
               MOVE 1 TO MT-ORD-CONV
           END-IF.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> COMMERCIAL ACCEPTANCE, NOT OWNERSHIP, FEEDS THE MATCH.  THE
      *> QUARANTINE QUANTITY IS REPORTED SO THE OPERATOR CAN SEE WHY
      *> THE ACCEPTED FIGURE IS LOWER THAN THE DELIVERY NOTE.
      *>----------------------------------------------------------------
       2200-READ-RECEIPT SECTION.
       2200-START.
           IF MT-ORDER-NUMBER = SPACES
               GO TO 2200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(rl.AcceptedQty),0) AS "
                  "varchar(24)), CAST(ISNULL(SUM(rl.QuarantineQty),0)"
                  " AS varchar(24)), "
                  "CAST(ISNULL(SUM(rl.ReturnedQty),0) AS varchar(24))"
                  ", CAST(ISNULL(SUM(rl.MatchedQty),0) AS "
                  "varchar(24)) FROM po.ReceiptLine rl "
                  "JOIN po.Receipt r ON r.ReceiptId = rl.ReceiptId "
                  "WHERE r.CompanyCode = ? AND r.Status = 'CONFIRMED'"
                  " AND rl.PurchaseOrderNumber = ? "
                  "AND rl.PurchaseOrderLine = ? "
                  "AND (? = '' OR r.ReceiptRef = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE MT-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE MT-ORDER-LINE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE MT-RECEIPT-REF TO SQLIO-PARM-VAL(4)
           MOVE MT-RECEIPT-REF TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 4 TO SQLIO-COL-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE 24 TO SQLIO-COL-WIDTH(WS-I)
           END-PERFORM
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-RECEIPT-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2200-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 0 TO MT-RCV-ACCEPTED
               GO TO 2200-EXIT
           END-IF
           COMPUTE MT-RCV-ACCEPTED = FUNCTION NUMVAL(WS-R-ACCEPTED)
           COMPUTE MT-RCV-QUARANTINE =
               FUNCTION NUMVAL(WS-R-QUARANTINE)
           COMPUTE MT-RCV-RETURNED = FUNCTION NUMVAL(WS-R-RETURNED).
       2200-EXIT.
           EXIT.

       2300-READ-PREVIOUS SECTION.
       2300-START.
           IF MT-ORDER-NUMBER = SPACES
               GO TO 2300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(MatchedQty),0) AS "
                  "varchar(24)) FROM ap.InvoiceMatch "
                  "WHERE CompanyCode = ? AND PurchaseOrderNumber = ? "
                  "AND PurchaseOrderLine = ? AND Status IN "
                  "('PROPOSED','MATCHED') AND NOT "
                  "(PurchaseInvoiceId = ? AND "
                  "PurchaseInvoiceLineNum = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE MT-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE MT-ORDER-LINE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE MT-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE MT-LINE-NUM TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PREV-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2300-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               COMPUTE MT-PREV-MATCHED = FUNCTION NUMVAL(WS-P-MATCHED)
           END-IF.
       2300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE ARITHMETIC.  THIS SECTION TOUCHES NO DATABASE SO THAT THE
      *> UNIT TESTS CAN DRIVE THE WORKED EXAMPLES DIRECTLY.
      *>----------------------------------------------------------------
       3000-TOLERANCE SECTION.
       3000-START.
           PERFORM 3100-QUANTITY THRU 3100-EXIT
           PERFORM 3200-PRICE THRU 3200-EXIT
           PERFORM 3300-CHARGE-VAT THRU 3300-EXIT
           PERFORM 3400-VERDICT THRU 3400-EXIT.
       3000-EXIT.
           EXIT.

       3100-QUANTITY SECTION.
       3100-START.
           IF MT-BY-VALUE OR MT-BY-MILESTONE
               MOVE MT-INV-QTY TO MT-MATCHED-QTY
               MOVE 0 TO MT-UNMATCHED-QTY
               MOVE MT-INV-QTY TO MT-AVAILABLE-QTY
               MOVE "Y" TO MT-QTY-OK
               GO TO 3100-EXIT
           END-IF
           COMPUTE MT-INV-STOCK-QTY = MT-INV-QTY * MT-INV-CONV
           COMPUTE MT-AVAILABLE-QTY =
               MT-RCV-ACCEPTED - MT-RCV-RETURNED - MT-PREV-MATCHED
           IF MT-AVAILABLE-QTY < 0
               MOVE 0 TO MT-AVAILABLE-QTY
           END-IF
           IF MT-INV-STOCK-QTY <= MT-AVAILABLE-QTY
               MOVE MT-INV-STOCK-QTY TO MT-MATCHED-QTY
               MOVE 0 TO MT-UNMATCHED-QTY
               MOVE "Y" TO MT-QTY-OK
           ELSE
               MOVE MT-AVAILABLE-QTY TO MT-MATCHED-QTY
               COMPUTE MT-UNMATCHED-QTY =
                   MT-INV-STOCK-QTY - MT-AVAILABLE-QTY
               MOVE "N" TO MT-QTY-OK
           END-IF
           COMPUTE MT-QTY-VARIANCE = MT-UNMATCHED-QTY.
       3100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PRICES ARE BROUGHT TO A COMMON PRICE-PER BASIS BEFORE THEY ARE
      *> COMPARED.  BOTH THE PERCENTAGE AND THE ABSOLUTE RULES MUST
      *> PASS WHERE BOTH ARE CONFIGURED (D-08).
      *>----------------------------------------------------------------
       3200-PRICE SECTION.
       3200-START.
           COMPUTE WS-INV-UNIT = MT-INV-PRICE / MT-INV-PRICE-PER
           COMPUTE WS-ORD-UNIT = MT-ORD-PRICE / MT-ORD-PRICE-PER
           COMPUTE MT-PRICE-VAR-UNIT = WS-INV-UNIT - WS-ORD-UNIT
           COMPUTE MT-MATCHED-VALUE ROUNDED =
               MT-MATCHED-QTY * WS-INV-UNIT
           COMPUTE MT-GRNI-CLEARED ROUNDED =
               MT-MATCHED-QTY * WS-ORD-UNIT
           COMPUTE MT-PRICE-VAR-GBP =
               MT-MATCHED-VALUE - MT-GRNI-CLEARED
           MOVE "N" TO MT-ZERO-REFERENCE
           IF WS-ORD-UNIT = 0
               MOVE "Y" TO MT-ZERO-REFERENCE
               MOVE 0 TO MT-PRICE-VAR-PCT
           ELSE
               COMPUTE MT-PRICE-VAR-PCT ROUNDED =
                   (MT-PRICE-VAR-UNIT / WS-ORD-UNIT) * 100
           END-IF
           MOVE "Y" TO MT-PRICE-OK
           COMPUTE WS-ABS-PCT = FUNCTION ABS(MT-PRICE-VAR-PCT)
           IF MT-ZERO-REFERENCE = "Y"
      *>       A ZERO REFERENCE PRICE HAS NO MEANINGFUL PERCENTAGE.
      *>       THE ABSOLUTE RULE DECIDES AND THE RESULT IS STATED.
               IF FUNCTION ABS(MT-PRICE-VAR-GBP) > MT-TOL-PRICE-GBP
                   MOVE "N" TO MT-PRICE-OK
               END-IF
               MOVE SPACES TO MT-TOLERANCE-NOTE
               STRING "Zero reference price: percentage test not "
                      "meaningful, absolute value rule applied"
                   DELIMITED BY SIZE INTO MT-TOLERANCE-NOTE
           ELSE
               IF WS-ABS-PCT > MT-TOL-PRICE-PCT
                   MOVE "N" TO MT-PRICE-OK
               END-IF
               IF MT-TOL-PRICE-UNIT > 0
                 AND FUNCTION ABS(MT-PRICE-VAR-UNIT)
                     > MT-TOL-PRICE-UNIT
                   MOVE "N" TO MT-PRICE-OK
               END-IF
               IF MT-TOL-PRICE-GBP > 0
                 AND FUNCTION ABS(MT-PRICE-VAR-GBP)
                     > MT-TOL-PRICE-GBP
                   MOVE "N" TO MT-PRICE-OK
               END-IF
           END-IF.
       3200-EXIT.
           EXIT.

       3300-CHARGE-VAT SECTION.
       3300-START.
           COMPUTE MT-CHARGE-VAR-GBP = MT-INV-CHARGE - MT-ORD-CHARGE
           MOVE "Y" TO MT-CHARGE-OK
           IF FUNCTION ABS(MT-CHARGE-VAR-GBP) > MT-TOL-CHARGE-GBP
               MOVE "N" TO MT-CHARGE-OK
           END-IF
           IF MT-INV-VATRATE = 0
               MOVE 0 TO WS-EXPECTED-VAT
           ELSE
               COMPUTE WS-EXPECTED-VAT ROUNDED =
                   ((MT-INV-NET + MT-INV-CHARGE) * MT-INV-VATRATE)
                   / 100
           END-IF
           COMPUTE MT-VAT-VAR-GBP = MT-INV-VAT - WS-EXPECTED-VAT
           MOVE "Y" TO MT-VAT-OK
           IF FUNCTION ABS(MT-VAT-VAR-GBP) > MT-TOL-VAT-GBP
               MOVE "N" TO MT-VAT-OK
           END-IF.
       3300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AP-007.  THE FOUR TESTS ARE INDEPENDENT.  A PRICE INSIDE
      *> TOLERANCE NEVER CURES A MISSING RECEIPT QUANTITY: THE LINE
      *> COMES BACK AS A VARIANCE NEEDING APPROVAL, AND THE UNMATCHED
      *> QUANTITY IS NAMED IN THE MESSAGE.
      *>----------------------------------------------------------------
       3400-VERDICT SECTION.
       3400-START.
           IF MT-QTY-OK = "Y" AND MT-PRICE-OK = "Y"
              AND MT-CHARGE-OK = "Y" AND MT-VAT-OK = "Y"
               MOVE "Y" TO MT-WITHIN-TOLERANCE
               MOVE "N" TO MT-NEEDS-APPROVAL
               MOVE "MATCHED" TO MT-MATCH-STATUS
               MOVE "MATCHED" TO MT-INVOICE-STATE
               MOVE 0 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               MOVE MT-MATCHED-QTY TO WS-QTY-ED
               STRING "Matched " FUNCTION TRIM(WS-QTY-ED)
                      " within the approved tolerances"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 3400-EXIT
           END-IF
           MOVE "N" TO MT-WITHIN-TOLERANCE
           MOVE "Y" TO MT-NEEDS-APPROVAL
           MOVE "PROPOSED" TO MT-MATCH-STATUS
           MOVE "VARIANCE" TO MT-INVOICE-STATE
           MOVE 6 TO CA-RETURN-CODE
           MOVE SPACES TO CA-MESSAGE
           EVALUATE TRUE
               WHEN MT-QTY-OK = "N"
                   MOVE "APM0010" TO CA-REASON-CODE
                   MOVE MT-UNMATCHED-QTY TO WS-QTY-ED
                   STRING FUNCTION TRIM(WS-QTY-ED)
                          " invoiced unit(s) have no commercially "
                          "accepted receipt and cannot be matched"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               WHEN MT-PRICE-OK = "N"
                   MOVE "APM0011" TO CA-REASON-CODE
                   MOVE MT-PRICE-VAR-PCT TO WS-PCT-ED
                   MOVE MT-PRICE-VAR-GBP TO WS-MONEY-ED
                   STRING "Price variance "
                          FUNCTION TRIM(WS-PCT-ED)
                          " per cent (GBP "
                          FUNCTION TRIM(WS-MONEY-ED)
                          ") is outside the approved tolerance"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               WHEN MT-CHARGE-OK = "N"
                   MOVE "APM0012" TO CA-REASON-CODE
                   MOVE MT-CHARGE-VAR-GBP TO WS-MONEY-ED
                   STRING "Charge variance of GBP "
                          FUNCTION TRIM(WS-MONEY-ED)
                          " is outside the approved tolerance"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               WHEN OTHER
                   MOVE "APM0013" TO CA-REASON-CODE
                   MOVE MT-VAT-VAR-GBP TO WS-MONEY-ED
                   STRING "VAT difference of GBP "
                          FUNCTION TRIM(WS-MONEY-ED)
                          " is beyond the approved tolerance"
                       DELIMITED BY SIZE INTO CA-MESSAGE
           END-EVALUATE
           ADD 1 TO CA-MSG-COUNT.
       3400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4000-CONFIRM SECTION.
       4000-START.
           INITIALIZE WS-APSVC
           MOVE "PERM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE "AP.INVOICE.MATCH" TO SVC-TEXT-1
           CALL "APSVC" USING WS-APSVC
           IF SVC-FLAG-1 NOT = "Y"
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APM0014" TO CA-REASON-CODE
               MOVE SVC-MESSAGE TO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 4000-EXIT
           END-IF
           PERFORM 2000-MATCH THRU 2000-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           IF MT-MATCHED-QTY = 0 AND MT-MATCHED-VALUE = 0
               MOVE 8 TO CA-RETURN-CODE
               MOVE "APM0015" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "There is nothing to match on this line"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
               MOVE "Y" TO WS-FATAL-SW
               GO TO 4000-EXIT
           END-IF
           PERFORM 4100-WRITE-MATCH THRU 4100-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           PERFORM 4200-UPDATE-ORDER THRU 4200-EXIT
           PERFORM 4300-UPDATE-INVOICE THRU 4300-EXIT.
       4000-EXIT.
           EXIT.

       4100-WRITE-MATCH SECTION.
       4100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT ap.InvoiceMatch (CompanyCode, "
                  "PurchaseInvoiceId, PurchaseInvoiceLineNum, "
                  "PurchaseOrderNumber, PurchaseOrderLine, "
                  "ReceiptRef, ItemCode, OrderedQty, "
                  "ReceivedAcceptedQty, ReturnedQty, "
                  "PreviouslyMatchedQty, MatchedQty, OrderUnitPrice, "
                  "InvoiceUnitPrice, QtyVariance, PriceVariancePct, "
                  "PriceVarianceGBP, ChargeVarianceGBP, "
                  "VatVarianceGBP, GrniClearedGBP, WithinTolerance, "
                  "ToleranceNote, Status, CreatedBy) VALUES (?, ?, ?,"
                  " NULLIF(?,''), NULLIF(?,0), NULLIF(?,''), "
                  "NULLIF(?,''), ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, "
                  "?, ?, NULLIF(?,''), ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE MT-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE MT-LINE-NUM TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE MT-ORDER-NUMBER TO SQLIO-PARM-VAL(4)
           MOVE MT-ORDER-LINE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE MT-RECEIPT-REF TO SQLIO-PARM-VAL(6)
           MOVE MT-ITEM TO SQLIO-PARM-VAL(7)
           MOVE MT-ORD-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE MT-RCV-ACCEPTED TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE MT-RCV-RETURNED TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE MT-PREV-MATCHED TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE MT-MATCHED-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE MT-ORD-PRICE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           MOVE MT-INV-PRICE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(14)
           MOVE MT-QTY-VARIANCE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(15)
           MOVE MT-PRICE-VAR-PCT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(16)
           MOVE MT-PRICE-VAR-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(17)
           MOVE MT-CHARGE-VAR-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(18)
           MOVE MT-VAT-VAR-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(19)
           MOVE MT-GRNI-CLEARED TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(20)
           IF MT-WITHIN-TOLERANCE = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(21)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(21)
           END-IF
           MOVE MT-TOLERANCE-NOTE TO SQLIO-PARM-VAL(22)
           MOVE MT-MATCH-STATUS TO SQLIO-PARM-VAL(23)
           MOVE CA-USER TO SQLIO-PARM-VAL(24)
           MOVE 24 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 24
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       4100-EXIT.
           EXIT.

       4200-UPDATE-ORDER SECTION.
       4200-START.
           IF MT-ORDER-NUMBER = SPACES
               GO TO 4200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.PurchaseOrderLine SET InvoicedQty = "
                  "InvoicedQty + CAST(? AS decimal(19,6)) "
                  "WHERE CompanyCode = ? AND OrderNumber = ? "
                  "AND LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE MT-MATCHED-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE MT-ORDER-NUMBER TO SQLIO-PARM-VAL(3)
           MOVE MT-ORDER-LINE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL OR MT-RECEIPT-REF = SPACES
               GO TO 4200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE rl SET MatchedQty = rl.MatchedQty + "
                  "CAST(? AS decimal(19,6)) FROM po.ReceiptLine rl "
                  "JOIN po.Receipt r ON r.ReceiptId = rl.ReceiptId "
                  "WHERE r.CompanyCode = ? AND r.ReceiptRef = ? "
                  "AND rl.PurchaseOrderNumber = ? "
                  "AND rl.PurchaseOrderLine = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE MT-MATCHED-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE MT-RECEIPT-REF TO SQLIO-PARM-VAL(3)
           MOVE MT-ORDER-NUMBER TO SQLIO-PARM-VAL(4)
           MOVE MT-ORDER-LINE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       4200-EXIT.
           EXIT.

       4300-UPDATE-INVOICE SECTION.
       4300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ap.PurchaseInvoice SET MatchState = "
                  "CASE WHEN EXISTS (SELECT 1 FROM ap.InvoiceMatch m "
                  "WHERE m.PurchaseInvoiceId = ? AND m.Status IN "
                  "('PROPOSED','DISPUTED') AND m.WithinTolerance = 0)"
                  " THEN 'VARIANCE' WHEN (SELECT COUNT(DISTINCT "
                  "m2.PurchaseInvoiceLineNum) FROM ap.InvoiceMatch m2"
                  " WHERE m2.PurchaseInvoiceId = ? AND m2.Status <> "
                  "'REVERSED') >= (SELECT COUNT(*) FROM "
                  "ap.PurchaseInvoiceLine l WHERE "
                  "l.PurchaseInvoiceId = ?) THEN 'MATCHED' "
                  "ELSE 'PARTIAL' END, PendingMatchClearingGBP = "
                  "PendingMatchClearingGBP, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() "
                  "WHERE PurchaseInvoiceId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE MT-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       4300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> MATCHALL WALKS EVERY LINE OF ONE REGISTERED DOCUMENT.  LINES
      *> THAT FALL OUTSIDE TOLERANCE ARE COUNTED AND LEFT AS PROPOSED
      *> VARIANCES FOR APPROVAL - THEY ARE NOT QUIETLY ACCEPTED.
      *>----------------------------------------------------------------
       5000-MATCHALL SECTION.
       5000-START.
           MOVE 0 TO MT-LINES-MATCHED
           MOVE 0 TO MT-LINES-VARIANCE
           MOVE 0 TO MT-LINES-UNMATCHED
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(l.LineNum AS varchar(8)), "
                  "ISNULL(l.PurchaseOrderNumber,''), "
                  "CAST(ISNULL(l.PurchaseOrderLine,0) AS varchar(8)),"
                  " ISNULL(l.ReceiptRef,''), "
                  "CAST(ISNULL(l.ReceiptLine,0) AS varchar(8)), "
                  "ISNULL(l.ServiceAcceptanceRef,''), "
                  "ISNULL(l.ItemCode,''), "
                  "CAST(ISNULL(l.Quantity,0) AS varchar(24)), "
                  "CAST(l.ConversionFactor AS varchar(24)), "
                  "CAST(ISNULL(l.UnitPrice,0) AS varchar(24)), "
                  "CAST(l.PricePerQty AS varchar(24)), "
                  "CAST(l.ChargeAmount AS varchar(24)), "
                  "CAST(l.NetAmount AS varchar(24)), "
                  "CAST(l.VatAmount AS varchar(24)), "
                  "CAST(l.VatRatePct AS varchar(24)) "
                  "FROM ap.PurchaseInvoiceLine l "
                  "WHERE l.PurchaseInvoiceId = ? ORDER BY l.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE MT-INVOICE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 15 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-COL-WIDTH(3)
           MOVE 30 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-COL-WIDTH(5)
           MOVE 30 TO SQLIO-COL-WIDTH(6)
           MOVE 40 TO SQLIO-COL-WIDTH(7)
           PERFORM VARYING WS-I FROM 8 BY 1 UNTIL WS-I > 15
               MOVE 24 TO SQLIO-COL-WIDTH(WS-I)
           END-PERFORM
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 5000-EXIT
           END-IF
           MOVE "N" TO WS-EOF-SW
           PERFORM UNTIL WS-EOF
               MOVE 3 TO SQLIO-CURSOR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-EOF-SW
               ELSE
                   PERFORM 5100-ONE-LINE THRU 5100-EXIT
               END-IF
           END-PERFORM
           MOVE 3 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           PERFORM 4300-UPDATE-INVOICE THRU 4300-EXIT
           MOVE SPACES TO CA-MESSAGE
           MOVE MT-LINES-VARIANCE TO SQLIO-INT-EDIT
           IF MT-LINES-VARIANCE > 0
               MOVE 6 TO CA-RETURN-CODE
               MOVE "APM0016" TO CA-REASON-CODE
               STRING FUNCTION TRIM(SQLIO-INT-EDIT)
                      " line(s) are outside tolerance and need "
                      "approval before payment"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           ELSE
               MOVE 0 TO CA-RETURN-CODE
               MOVE MT-LINES-MATCHED TO SQLIO-INT-EDIT
               STRING FUNCTION TRIM(SQLIO-INT-EDIT)
                      " line(s) matched within tolerance"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       5000-EXIT.
           EXIT.

       5100-ONE-LINE SECTION.
       5100-START.
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-L-NUM)
           MOVE WS-NUM TO MT-LINE-NUM
           MOVE WS-L-ORDER TO MT-ORDER-NUMBER
           COMPUTE WS-NUM = FUNCTION NUMVAL(WS-L-ORDERLINE)
           MOVE WS-NUM TO MT-ORDER-LINE
           MOVE WS-L-RECEIPT TO MT-RECEIPT-REF
           MOVE WS-L-SERVICE TO MT-SERVICE-REF
           MOVE WS-L-ITEM TO MT-ITEM
           COMPUTE MT-INV-QTY = FUNCTION NUMVAL(WS-L-QTY)
           COMPUTE MT-INV-CONV = FUNCTION NUMVAL(WS-L-CONV)
           COMPUTE MT-INV-PRICE = FUNCTION NUMVAL(WS-L-PRICE)
           COMPUTE MT-INV-PRICE-PER = FUNCTION NUMVAL(WS-L-PRICEPER)
           COMPUTE MT-INV-CHARGE = FUNCTION NUMVAL(WS-L-CHARGE)
           COMPUTE MT-INV-NET = FUNCTION NUMVAL(WS-L-NET)
           COMPUTE MT-INV-VAT = FUNCTION NUMVAL(WS-L-VAT)
           COMPUTE MT-INV-VATRATE = FUNCTION NUMVAL(WS-L-VATRATE)
           MOVE 0 TO MT-PREV-MATCHED MT-RCV-ACCEPTED
           MOVE 0 TO MT-RCV-RETURNED MT-RCV-QUARANTINE
           IF MT-INV-PRICE-PER = 0
               MOVE 1 TO MT-INV-PRICE-PER
           END-IF
           IF MT-INV-CONV = 0
               MOVE 1 TO MT-INV-CONV
           END-IF
           IF MT-ORDER-NUMBER = SPACES
               MOVE "VALUE" TO MT-BASIS
               MOVE MT-INV-NET TO MT-ORD-PRICE
               MOVE 1 TO MT-ORD-PRICE-PER
           ELSE
               MOVE "QUANTITY" TO MT-BASIS
           END-IF
           PERFORM 4000-CONFIRM THRU 4000-EXIT
           IF MT-WITHIN-TOLERANCE = "Y"
               ADD 1 TO MT-LINES-MATCHED
           ELSE
               ADD 1 TO MT-LINES-VARIANCE
           END-IF
           IF MT-UNMATCHED-QTY > 0
               ADD 1 TO MT-LINES-UNMATCHED
           END-IF.
       5100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8100-PARMNUM SECTION.
       8100-START.
           MOVE SVC-TEXT-1 TO WS-ONE-COL
           MOVE SVC-NUM-1 TO WS-NUM
           INITIALIZE WS-APSVC
           MOVE "PARMNUM" TO SVC-FUNC
           PERFORM 8900-SVC-HEADER THRU 8900-EXIT
           MOVE WS-ONE-COL TO SVC-TEXT-1
           MOVE WS-NUM TO SVC-NUM-1
           CALL "APSVC" USING WS-APSVC.
       8100-EXIT.
           EXIT.

       8500-EXEC-UPDATE SECTION.
       8500-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 8500-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
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
           MOVE "APM0099" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "APMATCH does not support function "
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
               MOVE "SQL failure in APMATCH" TO CA-MESSAGE
           END-IF
           ADD 1 TO CA-MSG-COUNT
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       9300-EXIT.
           EXIT.
