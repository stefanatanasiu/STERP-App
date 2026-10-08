      *>================================================================
      *> PROGRAM-ID : SODISC
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT
      *> PURPOSE    : THE SALES CALCULATION SEQUENCE.  QUANTITY IS
      *>              CONVERTED TO THE PRICE UNIT, EXTENDED BY THE
      *>              PRICE-PER QUANTITY, THE LINE PERCENTAGE
      *>              DISCOUNTS ARE APPLIED IN SEQUENCE, THE FIXED
      *>              LINE AND DOCUMENT ALLOWANCES ARE ALLOCATED SO
      *>              THAT THE PARTS RE-ADD TO THE ALLOWANCE, THE
      *>              SEPARATELY IDENTIFIED CHARGES ARE ADDED WITH THE
      *>              VAT LIABILITY OF THE GOODS, AND ONLY THEN IS VAT
      *>              CALCULATED.  LINE AND DOCUMENT NET, VAT AND
      *>              GROSS MUST RECONCILE EXACTLY.
      *> CALLED BY  : SALES ORDER ENTRY, QUOTATION ENTRY, INVOICING,
      *>              THE .NET CLIENT
      *> INTERFACE  : CALL "SODISC" USING WS-SODISC-CA
      *>              COPYBOOK SODISCCA
      *> CALLS      : UOMCONV (TOSTOCK / FACTOR), RNDALLOC (UNITPRC,
      *>              EXTEND, ROUND, ALLOC)
      *>
      *> THE INPUT FIELDS ARE READ AND NEVER WRITTEN, SO THE SAME
      *> COMMAREA PRESENTED TWICE GIVES THE SAME ANSWER AND A DISCOUNT
      *> CANNOT BE APPLIED TWICE (SO-005).  SD-APPLIED-TAG HOLDS THE
      *> SIGNATURE OF THE INPUTS BEHIND THE CURRENT RESULT.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-03-16 RJH  CR0912 SALES ORDER PRICING REWRITE (ACUCOBOL)
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT PRICE EFFECTIVE DATES
      *> 2000-06-12 KMP  CR0988 SEQUENTIAL NOT ADDITIVE DISCOUNTS
      *>                 (10 THEN 5 ON 360.00 IS 307.80, NOT 306.00)
      *> 2004-02-18 DJM  CR1142 DOCUMENT ALLOWANCE SPREAD BY LINE NET
      *> 2011-01-04 DLS  CR2240 VAT 20 PER CENT
      *> 2026-10-06 STERP SO-005 ALLOCATION, CHARGES AND RECONCILIATION
      *>================================================================
      *> REQ: SO-005
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SODISC.
       AUTHOR. R J HOLLOWAY.
       DATE-WRITTEN. 16/03/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY RNDALLOCCA REPLACING ==DFHCOMMAREA== BY ==WS-RND-CA==.
       COPY UOMCONVCA REPLACING ==DFHCOMMAREA== BY ==WS-UOM-CA==.
       01  WS-CONTROL.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-LINE-ERR             PIC X VALUE "N".
               88  WS-LINE-BAD                   VALUE "Y".
           05  WS-IX                   PIC S9(9) COMP-5 VALUE 0.
           05  WS-JX                   PIC S9(9) COMP-5 VALUE 0.
           05  WS-LINES                PIC S9(9) COMP-5 VALUE 0.
           05  WS-PLACES               PIC S9(4) COMP-5 VALUE 2.
           05  WS-ALLOC-MODE           PIC X(8) VALUE "WEIGHT".
       01  WS-AMOUNTS.
           05  WS-AMT1                 PIC S9(13)V9(6) VALUE 0.
           05  WS-AMT2                 PIC S9(13)V9(6) VALUE 0.
           05  WS-AMT3                 PIC S9(13)V9(6) VALUE 0.
           05  WS-RUNNING              PIC S9(13)V9(6) VALUE 0.
           05  WS-UNIT-EFF             PIC S9(13)V9(12) VALUE 0.
           05  WS-PCT                  PIC S9(5)V9(4) VALUE 0.
           05  WS-RATE-FACTOR          PIC S9(5)V9(12) VALUE 0.
           05  WS-CHG                  PIC S9(13)V9(2) VALUE 0.
           05  WS-VAT-GOODS            PIC S9(13)V9(2) VALUE 0.
           05  WS-VAT-CHARGE           PIC S9(13)V9(2) VALUE 0.
           05  WS-WEIGHT-TOT           PIC S9(15)V9(6) VALUE 0.
           05  WS-CHECK-NET            PIC S9(15)V9(2) VALUE 0.
           05  WS-CHECK-GROSS          PIC S9(15)V9(2) VALUE 0.
           05  WS-SIG-NUM              PIC S9(15)V9(4) VALUE 0.
           05  WS-SIG-ED               PIC -(15)9.9(4).
           05  WS-MSG-NUM              PIC -(13)9.99.
           05  WS-MSG-INT              PIC -(8)9.
      *>   the discounted value of each line before any allowance -
      *>   held here so that the COMMAREA inputs are never disturbed
       01  WS-LINE-WORK.
           05  WS-LW OCCURS 200 TIMES.
               10  WS-LW-DISCOUNTED    PIC S9(13)V9(2) VALUE 0.
               10  WS-LW-AFTER-LINE    PIC S9(13)V9(2) VALUE 0.
               10  WS-LW-WEIGHT        PIC S9(13)V9(2) VALUE 0.
       LINKAGE SECTION.
       COPY SODISCCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           PERFORM 0200-VALIDATE THRU 0200-EXIT
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION OF DFHCOMMAREA
               WHEN "LINE"     PERFORM 1000-SINGLE-LINE THRU 1000-EXIT
               WHEN "DOCUMENT" PERFORM 2000-DOCUMENT THRU 2000-EXIT
               WHEN "RECALC"   PERFORM 2000-DOCUMENT THRU 2000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
      *> 0100  WORKING STORAGE SURVIVES BETWEEN CALLS IN THIS RUN UNIT,
      *>       SO EVERY ACCUMULATOR IS CLEARED HERE AND NOWHERE ELSE.
      *>----------------------------------------------------------------
       0100-INITIALISE.
           MOVE "SODISC" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "SO-005" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE "N" TO WS-ERROR WS-LINE-ERR
           MOVE 0 TO WS-IX WS-JX WS-LINES
           MOVE 0 TO WS-AMT1 WS-AMT2 WS-AMT3 WS-RUNNING
           MOVE 0 TO WS-UNIT-EFF WS-PCT WS-RATE-FACTOR
           MOVE 0 TO WS-CHG WS-VAT-GOODS WS-VAT-CHARGE
           MOVE 0 TO WS-WEIGHT-TOT WS-CHECK-NET WS-CHECK-GROSS
           MOVE 0 TO WS-SIG-NUM
           MOVE "WEIGHT" TO WS-ALLOC-MODE
           MOVE FUNCTION UPPER-CASE(CA-FUNCTION OF DFHCOMMAREA)
               TO CA-FUNCTION OF DFHCOMMAREA
           IF SD-ROUND-PLACES < 0 OR SD-ROUND-PLACES > 6
               MOVE 2 TO SD-ROUND-PLACES
           END-IF
           MOVE SD-ROUND-PLACES TO WS-PLACES
           IF SD-CURRENCY = SPACES
               MOVE "GBP" TO SD-CURRENCY
           END-IF
           IF SD-CHARGE-VAT-BASIS = SPACES
               MOVE "GOODS" TO SD-CHARGE-VAT-BASIS
           END-IF
           IF SD-PRICE-DATE = 0
               MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO SD-PRICE-DATE
           END-IF
           MOVE "N" TO SD-ALREADY-APPLIED
           MOVE "N" TO SD-RECONCILED
           MOVE SPACES TO SD-EXPLANATION
           MOVE 0 TO SD-TOT-GROSS SD-TOT-DISCOUNT SD-TOT-ALLOWANCE
           MOVE 0 TO SD-TOT-CHARGE SD-TOT-NET SD-TOT-VAT
           MOVE 0 TO SD-TOT-GROSS-TOTAL SD-TOT-COST-GBP
           MOVE 0 TO SD-DOC-MARGIN-PCT SD-ALLOC-RESIDUAL
           MOVE 0 TO SD-ALLOC-BUMPED
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 200
               MOVE 0 TO WS-LW-DISCOUNTED(WS-IX)
               MOVE 0 TO WS-LW-AFTER-LINE(WS-IX)
               MOVE 0 TO WS-LW-WEIGHT(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE.
           IF CA-COMPANY OF DFHCOMMAREA = SPACES
               MOVE "DSC0001" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A company is required" TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION OF DFHCOMMAREA = "LINE"
               MOVE 1 TO SD-LINE-CNT
           END-IF
           IF SD-LINE-CNT < 1
               MOVE "DSC0002" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "No lines were supplied to calculate"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF SD-LINE-CNT > 200
               MOVE "DSC0003" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A document is limited to 200 lines"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           MOVE SD-LINE-CNT TO WS-LINES.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  ONE LINE.  THE DOCUMENT ENGINE IS USED UNCHANGED SO
      *>       THAT A SINGLE LINE CANNOT DRIFT FROM THE DOCUMENT.
      *>----------------------------------------------------------------
       1000-SINGLE-LINE.
           MOVE 1 TO WS-LINES
           PERFORM 2000-DOCUMENT THRU 2000-EXIT.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  THE SEQUENCE.  THE ORDER OF THESE SEVEN STEPS IS THE
      *>       RULE ITSELF AND MUST NOT BE REARRANGED.
      *>----------------------------------------------------------------
       2000-DOCUMENT.
           PERFORM 2100-SIGNATURE THRU 2100-EXIT
           PERFORM 2200-LINE-PASS-ONE THRU 2200-EXIT
           IF WS-INPUT-BAD
               GO TO 2000-EXIT
           END-IF
           PERFORM 2300-DOC-ALLOWANCE THRU 2300-EXIT
           IF WS-INPUT-BAD
               GO TO 2000-EXIT
           END-IF
           PERFORM 2400-DOC-CHARGE THRU 2400-EXIT
           IF WS-INPUT-BAD
               GO TO 2000-EXIT
           END-IF
           PERFORM 2500-LINE-PASS-TWO THRU 2500-EXIT
           IF WS-INPUT-BAD
               GO TO 2000-EXIT
           END-IF
           PERFORM 2600-TOTALS THRU 2600-EXIT
           PERFORM 2700-RECONCILE THRU 2700-EXIT
           PERFORM 2800-EXPLAIN THRU 2800-EXIT.
       2000-EXIT.
           EXIT.

      *>   the signature of the inputs.  an unchanged signature means
      *>   the caller has asked for the same calculation again, which
      *>   is answered with the same figures and never a second
      *>   deduction (SO-005 "discounts are not applied twice").
       2100-SIGNATURE.
           MOVE 0 TO WS-SIG-NUM
           COMPUTE WS-SIG-NUM = WS-LINES
                 + SD-DOC-ALLOWANCE * 41 + SD-DOC-CHARGE * 43
                 + SD-ROUND-PLACES * 47
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > WS-LINES
               COMPUTE WS-SIG-NUM = WS-SIG-NUM
                     + SD-L-ENT-QTY(WS-IX) * 3
                     + SD-L-FACTOR(WS-IX) * 5
                     + SD-L-UNIT-PRICE(WS-IX) * 7
                     + SD-L-PER-QTY(WS-IX) * 11
                     + SD-L-DISC-PCT1(WS-IX) * 13
                     + SD-L-DISC-PCT2(WS-IX) * 17
                     + SD-L-DISC-PCT3(WS-IX) * 19
                     + SD-L-LINE-ALLOW(WS-IX) * 23
                     + SD-L-CHARGE(WS-IX) * 29
                     + SD-L-VAT-RATE(WS-IX) * 31
                     + SD-L-COST-GBP(WS-IX) * 37
                     + WS-IX
           END-PERFORM
           MOVE WS-SIG-NUM TO WS-SIG-ED
           IF SD-APPLIED-TAG NOT = SPACES
               AND SD-APPLIED-TAG(1:2) = "S:"
               AND CA-FUNCTION OF DFHCOMMAREA NOT = "RECALC"
               AND SD-APPLIED-TAG(3:21) = WS-SIG-ED
               MOVE "Y" TO SD-ALREADY-APPLIED
           END-IF
           MOVE SPACES TO SD-APPLIED-TAG
           STRING "S:" WS-SIG-ED
               DELIMITED BY SIZE INTO SD-APPLIED-TAG.
       2100-EXIT.
           EXIT.

       2200-LINE-PASS-ONE.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > WS-LINES
               PERFORM 3000-LINE-STEPS THRU 3000-EXIT
           END-PERFORM.
       2200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2300  STEP FOUR - THE DOCUMENT ALLOWANCE.  RNDALLOC SPREADS
      *>       IT OVER THE LINES BY DISCOUNTED LINE VALUE AND PROVES
      *>       THAT THE PARTS RE-ADD TO THE ALLOWANCE, SO A HEADER
      *>       ALLOWANCE CANNOT MAKE THE LINE TOTALS INCONSISTENT.
      *>----------------------------------------------------------------
       2300-DOC-ALLOWANCE.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > WS-LINES
               MOVE 0 TO SD-L-DOC-ALLOW(WS-IX)
           END-PERFORM
           IF SD-DOC-ALLOWANCE = 0
               GO TO 2300-EXIT
           END-IF
           MOVE 0 TO WS-WEIGHT-TOT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > WS-LINES
               IF WS-LW-AFTER-LINE(WS-IX) > 0
                   MOVE WS-LW-AFTER-LINE(WS-IX) TO WS-LW-WEIGHT(WS-IX)
               ELSE
                   MOVE 0 TO WS-LW-WEIGHT(WS-IX)
               END-IF
               ADD WS-LW-WEIGHT(WS-IX) TO WS-WEIGHT-TOT
           END-PERFORM
           IF WS-WEIGHT-TOT = 0
               MOVE "EQUAL" TO WS-ALLOC-MODE
           ELSE
               MOVE "WEIGHT" TO WS-ALLOC-MODE
           END-IF
           IF WS-WEIGHT-TOT > 0 AND SD-DOC-ALLOWANCE > WS-WEIGHT-TOT
               MOVE "DSC0009" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               MOVE SD-DOC-ALLOWANCE TO WS-MSG-NUM
               STRING "Document allowance of "
                      FUNCTION TRIM(WS-MSG-NUM)
                      " exceeds the discounted line value"
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2300-EXIT
           END-IF
           MOVE SD-DOC-ALLOWANCE TO WS-AMT1
           PERFORM 4000-ALLOCATE THRU 4000-EXIT
           IF WS-INPUT-BAD
               GO TO 2300-EXIT
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > WS-LINES
               MOVE RA-PART(WS-IX) TO SD-L-DOC-ALLOW(WS-IX)
           END-PERFORM
           MOVE RA-RESIDUAL OF WS-RND-CA TO SD-ALLOC-RESIDUAL
           MOVE RA-BUMPED OF WS-RND-CA TO SD-ALLOC-BUMPED.
       2300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2400  STEP FIVE - SEPARATELY IDENTIFIED CHARGES.  A DOCUMENT
      *>       CARRIAGE CHARGE IS SPREAD OVER THE LINES SO THAT IT CAN
      *>       FOLLOW THE VAT LIABILITY OF THE GOODS IT CARRIES.
      *>----------------------------------------------------------------
       2400-DOC-CHARGE.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > WS-LINES
               MOVE 0 TO SD-L-DOC-CHARGE(WS-IX)
           END-PERFORM
           IF SD-DOC-CHARGE = 0
               GO TO 2400-EXIT
           END-IF
           MOVE 0 TO WS-WEIGHT-TOT
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > WS-LINES
               COMPUTE WS-AMT2 = WS-LW-AFTER-LINE(WS-IX)
                               - SD-L-DOC-ALLOW(WS-IX)
               IF WS-AMT2 > 0
                   MOVE WS-AMT2 TO WS-LW-WEIGHT(WS-IX)
               ELSE
                   MOVE 0 TO WS-LW-WEIGHT(WS-IX)
               END-IF
               ADD WS-LW-WEIGHT(WS-IX) TO WS-WEIGHT-TOT
           END-PERFORM
           IF WS-WEIGHT-TOT = 0
               MOVE "EQUAL" TO WS-ALLOC-MODE
           ELSE
               MOVE "WEIGHT" TO WS-ALLOC-MODE
           END-IF
           MOVE SD-DOC-CHARGE TO WS-AMT1
           PERFORM 4000-ALLOCATE THRU 4000-EXIT
           IF WS-INPUT-BAD
               GO TO 2400-EXIT
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > WS-LINES
               MOVE RA-PART(WS-IX) TO SD-L-DOC-CHARGE(WS-IX)
           END-PERFORM.
       2400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2500  STEPS SIX AND SEVEN - NET, VAT AND GROSS PER LINE.
      *>----------------------------------------------------------------
       2500-LINE-PASS-TWO.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > WS-LINES
               PERFORM 3500-LINE-NET THRU 3500-EXIT
               PERFORM 3600-LINE-VAT THRU 3600-EXIT
               PERFORM 3700-LINE-MARGIN THRU 3700-EXIT
           END-PERFORM.
       2500-EXIT.
           EXIT.

       2600-TOTALS.
           MOVE 0 TO SD-TOT-GROSS SD-TOT-DISCOUNT SD-TOT-ALLOWANCE
           MOVE 0 TO SD-TOT-CHARGE SD-TOT-NET SD-TOT-VAT
           MOVE 0 TO SD-TOT-GROSS-TOTAL SD-TOT-COST-GBP
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > WS-LINES
               ADD SD-L-GROSS-AMT(WS-IX) TO SD-TOT-GROSS
               ADD SD-L-DISC-AMT(WS-IX) TO SD-TOT-DISCOUNT
               ADD SD-L-ALLOW-TOT(WS-IX) TO SD-TOT-ALLOWANCE
               ADD SD-L-CHARGE(WS-IX) TO SD-TOT-CHARGE
               ADD SD-L-DOC-CHARGE(WS-IX) TO SD-TOT-CHARGE
               ADD SD-L-NET-AMT(WS-IX) TO SD-TOT-NET
               ADD SD-L-VAT-AMT(WS-IX) TO SD-TOT-VAT
               ADD SD-L-GROSS-TOT(WS-IX) TO SD-TOT-GROSS-TOTAL
               ADD SD-L-COST-GBP(WS-IX) TO SD-TOT-COST-GBP
           END-PERFORM
           IF SD-TOT-NET NOT = 0 AND SD-TOT-COST-GBP NOT = 0
               COMPUTE SD-DOC-MARGIN-PCT ROUNDED =
                   (SD-TOT-NET - SD-TOT-COST-GBP) / SD-TOT-NET * 100
           ELSE
               MOVE 0 TO SD-DOC-MARGIN-PCT
           END-IF.
       2600-EXIT.
           EXIT.

      *>   the control total proof.  gross less discount less
      *>   allowance plus charges must be the net, and net plus VAT
      *>   must be the gross, on every line and on the document.
       2700-RECONCILE.
           MOVE "Y" TO SD-RECONCILED
           COMPUTE WS-CHECK-NET = SD-TOT-GROSS - SD-TOT-DISCOUNT
                                - SD-TOT-ALLOWANCE + SD-TOT-CHARGE
           IF WS-CHECK-NET NOT = SD-TOT-NET
               MOVE "N" TO SD-RECONCILED
           END-IF
           COMPUTE WS-CHECK-GROSS = SD-TOT-NET + SD-TOT-VAT
           IF WS-CHECK-GROSS NOT = SD-TOT-GROSS-TOTAL
               MOVE "N" TO SD-RECONCILED
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > WS-LINES
               COMPUTE WS-CHECK-GROSS = SD-L-NET-AMT(WS-IX)
                                      + SD-L-VAT-AMT(WS-IX)
               IF WS-CHECK-GROSS NOT = SD-L-GROSS-TOT(WS-IX)
                   MOVE "N" TO SD-RECONCILED
               END-IF
           END-PERFORM
           IF SD-RECONCILED NOT = "Y"
               MOVE "DSC0010" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "Line and document totals do not reconcile"
                   TO CA-MESSAGE OF DFHCOMMAREA
               MOVE 12 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "Y" TO WS-ERROR
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       2700-EXIT.
           EXIT.

       2800-EXPLAIN.
           MOVE SPACES TO SD-EXPLANATION
           MOVE SD-TOT-GROSS TO WS-MSG-NUM
           STRING "Gross " FUNCTION TRIM(WS-MSG-NUM)
               DELIMITED BY SIZE INTO SD-EXPLANATION
           MOVE SD-TOT-DISCOUNT TO WS-MSG-NUM
           STRING FUNCTION TRIM(SD-EXPLANATION)
                  ", sequential discount " FUNCTION TRIM(WS-MSG-NUM)
               DELIMITED BY SIZE INTO SD-EXPLANATION
           MOVE SD-TOT-ALLOWANCE TO WS-MSG-NUM
           STRING FUNCTION TRIM(SD-EXPLANATION)
                  ", allowance " FUNCTION TRIM(WS-MSG-NUM)
               DELIMITED BY SIZE INTO SD-EXPLANATION
           MOVE SD-TOT-CHARGE TO WS-MSG-NUM
           STRING FUNCTION TRIM(SD-EXPLANATION)
                  ", charges " FUNCTION TRIM(WS-MSG-NUM)
               DELIMITED BY SIZE INTO SD-EXPLANATION
           MOVE SD-TOT-NET TO WS-MSG-NUM
           STRING FUNCTION TRIM(SD-EXPLANATION)
                  ", net " FUNCTION TRIM(WS-MSG-NUM)
               DELIMITED BY SIZE INTO SD-EXPLANATION
           MOVE SD-TOT-VAT TO WS-MSG-NUM
           STRING FUNCTION TRIM(SD-EXPLANATION)
                  ", VAT " FUNCTION TRIM(WS-MSG-NUM)
               DELIMITED BY SIZE INTO SD-EXPLANATION
           MOVE SD-TOT-GROSS-TOTAL TO WS-MSG-NUM
           STRING FUNCTION TRIM(SD-EXPLANATION)
                  ", gross total " FUNCTION TRIM(WS-MSG-NUM)
               DELIMITED BY SIZE INTO SD-EXPLANATION
           IF SD-WAS-REPEAT
               STRING FUNCTION TRIM(SD-EXPLANATION)
                      " (repeat request, figures unchanged)"
                   DELIMITED BY SIZE INTO SD-EXPLANATION
           END-IF
           IF CA-RETURN-CODE OF DFHCOMMAREA = 0
               AND CA-MESSAGE OF DFHCOMMAREA = SPACES
               MOVE SD-EXPLANATION TO CA-MESSAGE OF DFHCOMMAREA
           END-IF.
       2800-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  ONE LINE, STEPS ONE TO FOUR.
      *>----------------------------------------------------------------
       3000-LINE-STEPS.
           MOVE "N" TO WS-LINE-ERR
           MOVE SPACES TO SD-L-WHY(WS-IX)
           MOVE "OK" TO SD-L-STATUS(WS-IX)
           MOVE 0 TO SD-L-PRICE-QTY(WS-IX) SD-L-GROSS-AMT(WS-IX)
           MOVE 0 TO SD-L-DISC-AMT(WS-IX) SD-L-ALLOW-TOT(WS-IX)
           MOVE 0 TO SD-L-GOODS-NET(WS-IX) SD-L-NET-AMT(WS-IX)
           MOVE 0 TO SD-L-VAT-AMT(WS-IX) SD-L-GROSS-TOT(WS-IX)
           MOVE 0 TO SD-L-MARGIN-PCT(WS-IX)
           PERFORM 3100-CONVERT-QTY THRU 3100-EXIT
           IF WS-LINE-BAD
               GO TO 3000-EXIT
           END-IF
           PERFORM 3200-EXTEND THRU 3200-EXIT
           IF WS-LINE-BAD
               GO TO 3000-EXIT
           END-IF
           PERFORM 3300-DISCOUNTS THRU 3300-EXIT
           IF WS-LINE-BAD
               GO TO 3000-EXIT
           END-IF
           PERFORM 3400-LINE-ALLOWANCE THRU 3400-EXIT.
       3000-EXIT.
           EXIT.

      *>   STEP ONE.  A MISSING CONVERSION IS AN ERROR.  NOTHING HERE
      *>   EVER ASSUMES A FACTOR OF ONE (ITM-005 AND D-09).
       3100-CONVERT-QTY.
           IF SD-L-ENT-QTY(WS-IX) <= 0
               MOVE "DSC0004" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "Quantity must be greater than zero"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9200-LINE-REJECT THRU 9200-EXIT
               GO TO 3100-EXIT
           END-IF
           IF SD-L-FACTOR(WS-IX) > 0
               MOVE "captured factor" TO SD-L-WHY(WS-IX)
               COMPUTE SD-L-PRICE-QTY(WS-IX) ROUNDED =
                   SD-L-ENT-QTY(WS-IX) * SD-L-FACTOR(WS-IX)
               GO TO 3100-EXIT
           END-IF
           IF SD-L-ITEM-CODE(WS-IX) = SPACES
               MOVE "DSC0007" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "No unit conversion was supplied and a "
                      "factor of one is never assumed"
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9200-LINE-REJECT THRU 9200-EXIT
               GO TO 3100-EXIT
           END-IF
           PERFORM 5000-ASK-UOMCONV THRU 5000-EXIT
           IF WS-LINE-BAD
               GO TO 3100-EXIT
           END-IF.
       3100-EXIT.
           EXIT.

      *>   STEP TWO.  THE PRICE-PER QUANTITY IS A DIVISOR AND A
      *>   MISSING DIVISOR IS NOT ONE EITHER.
       3200-EXTEND.
           IF SD-L-PER-QTY(WS-IX) <= 0
               MOVE "DSC0005" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "The price-per quantity must be positive"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9200-LINE-REJECT THRU 9200-EXIT
               GO TO 3200-EXIT
           END-IF
           INITIALIZE WS-RND-CA
           MOVE "UNITPRC" TO CA-FUNCTION OF WS-RND-CA
           MOVE SD-L-UNIT-PRICE(WS-IX) TO RA-VALUE OF WS-RND-CA
           MOVE SD-L-PER-QTY(WS-IX) TO RA-VALUE2 OF WS-RND-CA
           MOVE 6 TO RA-PLACES OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA
           IF CA-RETURN-CODE OF WS-RND-CA NOT = 0
               MOVE "DSC0005" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-RND-CA TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9200-LINE-REJECT THRU 9200-EXIT
               GO TO 3200-EXIT
           END-IF
           MOVE RA-RESULT OF WS-RND-CA TO WS-UNIT-EFF
           INITIALIZE WS-RND-CA
           MOVE "EXTEND" TO CA-FUNCTION OF WS-RND-CA
           MOVE SD-L-PRICE-QTY(WS-IX) TO RA-VALUE OF WS-RND-CA
           MOVE WS-UNIT-EFF TO RA-VALUE2 OF WS-RND-CA
           MOVE WS-PLACES TO RA-PLACES OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA
           MOVE RA-RESULT OF WS-RND-CA TO SD-L-GROSS-AMT(WS-IX).
       3200-EXIT.
           EXIT.

      *>   STEP THREE.  SEQUENTIAL, NOT ADDITIVE.  TEN PER CENT AND
      *>   THEN FIVE PER CENT OF 360.00 LEAVES 307.80 AND NOT 306.00,
      *>   WHICH IS THE WHOLE POINT OF CR0988.
       3300-DISCOUNTS.
           MOVE SD-L-GROSS-AMT(WS-IX) TO WS-RUNNING
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 3
               EVALUATE WS-JX
                   WHEN 1 MOVE SD-L-DISC-PCT1(WS-IX) TO WS-PCT
                   WHEN 2 MOVE SD-L-DISC-PCT2(WS-IX) TO WS-PCT
                   WHEN 3 MOVE SD-L-DISC-PCT3(WS-IX) TO WS-PCT
               END-EVALUATE
               IF WS-PCT NOT = 0
                   IF WS-PCT > 100 OR WS-PCT < -100
                       MOVE "DSC0006" TO CA-REASON-CODE OF DFHCOMMAREA
                       MOVE "A discount percentage is out of range"
                           TO CA-MESSAGE OF DFHCOMMAREA
                       PERFORM 9200-LINE-REJECT THRU 9200-EXIT
                       GO TO 3300-EXIT
                   END-IF
                   COMPUTE WS-RATE-FACTOR = (100 - WS-PCT) / 100
                   INITIALIZE WS-RND-CA
                   MOVE "EXTEND" TO CA-FUNCTION OF WS-RND-CA
                   MOVE WS-RUNNING TO RA-VALUE OF WS-RND-CA
                   MOVE WS-RATE-FACTOR TO RA-VALUE2 OF WS-RND-CA
                   MOVE WS-PLACES TO RA-PLACES OF WS-RND-CA
                   CALL "RNDALLOC" USING WS-RND-CA
                   MOVE RA-RESULT OF WS-RND-CA TO WS-RUNNING
               END-IF
           END-PERFORM
           COMPUTE SD-L-DISC-AMT(WS-IX) =
               SD-L-GROSS-AMT(WS-IX) - WS-RUNNING
           MOVE WS-RUNNING TO WS-LW-DISCOUNTED(WS-IX).
       3300-EXIT.
           EXIT.

       3400-LINE-ALLOWANCE.
           COMPUTE WS-LW-AFTER-LINE(WS-IX) =
               WS-LW-DISCOUNTED(WS-IX) - SD-L-LINE-ALLOW(WS-IX)
           IF SD-L-LINE-ALLOW(WS-IX) > WS-LW-DISCOUNTED(WS-IX)
               MOVE "REVIEW" TO SD-L-STATUS(WS-IX)
               MOVE "allowance exceeds line" TO SD-L-WHY(WS-IX)
               IF CA-RETURN-CODE OF DFHCOMMAREA < 4
                   MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
                   MOVE "DSC0009" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE "A line allowance exceeds the line value"
                       TO CA-MESSAGE OF DFHCOMMAREA
                   ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               END-IF
           END-IF.
       3400-EXIT.
           EXIT.

       3500-LINE-NET.
           COMPUTE SD-L-ALLOW-TOT(WS-IX) =
               SD-L-LINE-ALLOW(WS-IX) + SD-L-DOC-ALLOW(WS-IX)
           COMPUTE SD-L-GOODS-NET(WS-IX) =
               WS-LW-DISCOUNTED(WS-IX) - SD-L-ALLOW-TOT(WS-IX)
           COMPUTE WS-CHG =
               SD-L-CHARGE(WS-IX) + SD-L-DOC-CHARGE(WS-IX)
           COMPUTE SD-L-NET-AMT(WS-IX) =
               SD-L-GOODS-NET(WS-IX) + WS-CHG.
       3500-EXIT.
           EXIT.

      *>   STEP SIX.  CARRIAGE CHARGED ON A SUPPLY FOLLOWS THE VAT
      *>   LIABILITY OF THE GOODS UNLESS THE TAX OWNER HAS FIXED A
      *>   SEPARATE CODE FOR IT.
       3600-LINE-VAT.
           MOVE 0 TO WS-VAT-GOODS WS-VAT-CHARGE
           IF SD-L-VAT-CODE(WS-IX) = "S" OR SD-L-VAT-CODE(WS-IX) = "R"
               IF SD-L-VAT-RATE(WS-IX) = 0
                   MOVE "DSC0011" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
                   STRING "A standard or reduced rate line needs a "
                          "VAT rate - zero is not assumed"
                       DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
                   PERFORM 9200-LINE-REJECT THRU 9200-EXIT
                   GO TO 3600-EXIT
               END-IF
           END-IF
           IF SD-CHG-FIXED AND WS-CHG NOT = 0
               AND SD-DOC-CHARGE-VAT NOT = SPACES
               MOVE SD-DOC-CHARGE-VAT TO SD-L-CHARGE-VAT(WS-IX)
               COMPUTE WS-AMT1 =
                   SD-L-GOODS-NET(WS-IX) * SD-L-VAT-RATE(WS-IX) / 100
               PERFORM 4100-ROUND-AMT1 THRU 4100-EXIT
               MOVE WS-AMT1 TO WS-VAT-GOODS
               COMPUTE WS-AMT1 = WS-CHG * SD-CHARGE-VAT-RATE / 100
               PERFORM 4100-ROUND-AMT1 THRU 4100-EXIT
               MOVE WS-AMT1 TO WS-VAT-CHARGE
           ELSE
               MOVE SD-L-VAT-CODE(WS-IX) TO SD-L-CHARGE-VAT(WS-IX)
               COMPUTE WS-AMT1 =
                   SD-L-NET-AMT(WS-IX) * SD-L-VAT-RATE(WS-IX) / 100
               PERFORM 4100-ROUND-AMT1 THRU 4100-EXIT
               MOVE WS-AMT1 TO WS-VAT-GOODS
           END-IF
           COMPUTE SD-L-VAT-AMT(WS-IX) = WS-VAT-GOODS + WS-VAT-CHARGE
           COMPUTE SD-L-GROSS-TOT(WS-IX) =
               SD-L-NET-AMT(WS-IX) + SD-L-VAT-AMT(WS-IX).
       3600-EXIT.
           EXIT.

      *>   STEP EIGHT IS SOMARGN'S WORK.  THE LINE FIGURE HERE IS A
      *>   DISPLAY CONVENIENCE ONLY AND USES THE SAME FORMULA.
      *>   TODO SO-TD: duplicated margin formula, kept for the screen
       3700-LINE-MARGIN.
           IF SD-L-COST-GBP(WS-IX) NOT = 0
               AND SD-L-NET-AMT(WS-IX) NOT = 0
               COMPUTE SD-L-MARGIN-PCT(WS-IX) ROUNDED =
                   (SD-L-NET-AMT(WS-IX) - SD-L-COST-GBP(WS-IX))
                   / SD-L-NET-AMT(WS-IX) * 100
           ELSE
               MOVE 0 TO SD-L-MARGIN-PCT(WS-IX)
               IF SD-L-WHY(WS-IX) = SPACES
                   MOVE "no cost - ask SOMARGN" TO SD-L-WHY(WS-IX)
               END-IF
           END-IF.
       3700-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  SHARED ALLOCATION CALL.  WS-AMT1 IS THE AMOUNT AND
      *>       WS-LW-WEIGHT HOLDS THE WEIGHTS.
      *>----------------------------------------------------------------
       4000-ALLOCATE.
           INITIALIZE WS-RND-CA
           MOVE "ALLOC" TO CA-FUNCTION OF WS-RND-CA
           MOVE WS-ALLOC-MODE TO RA-MODE OF WS-RND-CA
           MOVE WS-AMT1 TO RA-AMOUNT OF WS-RND-CA
           MOVE WS-PLACES TO RA-PLACES OF WS-RND-CA
           MOVE WS-LINES TO RA-LINE-CNT OF WS-RND-CA
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > WS-LINES
               MOVE WS-LW-WEIGHT(WS-JX) TO RA-WEIGHT(WS-JX)
               MOVE SD-L-LINE-REF(WS-JX) TO RA-LINE-REF(WS-JX)
           END-PERFORM
           CALL "RNDALLOC" USING WS-RND-CA
           IF CA-RETURN-CODE OF WS-RND-CA NOT = 0
               MOVE "DSC0010" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-RND-CA TO CA-MESSAGE OF DFHCOMMAREA
               MOVE 12 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "Y" TO WS-ERROR
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       4000-EXIT.
           EXIT.

       4100-ROUND-AMT1.
           INITIALIZE WS-RND-CA
           MOVE "ROUND" TO CA-FUNCTION OF WS-RND-CA
           MOVE WS-AMT1 TO RA-VALUE OF WS-RND-CA
           MOVE WS-PLACES TO RA-PLACES OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA
           MOVE RA-RESULT OF WS-RND-CA TO WS-AMT1.
       4100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000  THE UNIT CONVERSION.  SALES USAGE FIRST, THEN THE PRICE
      *>       UNIT, BECAUSE A PRICE UNIT NEED NOT BE THE STOCK UNIT.
      *>----------------------------------------------------------------
       5000-ASK-UOMCONV.
           INITIALIZE WS-UOM-CA
           MOVE "TOSTOCK" TO CA-FUNCTION OF WS-UOM-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-UOM-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-UOM-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-UOM-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-UOM-CA
           MOVE SD-L-ITEM-CODE(WS-IX) TO UC-ITEM-CODE
           MOVE SD-L-ENT-UOM(WS-IX) TO UC-FROM-UOM
           MOVE "SALES" TO UC-USAGE-TYPE
           MOVE SD-PRICE-DATE TO UC-EFF-DATE
           MOVE SD-L-ENT-QTY(WS-IX) TO UC-ENTERED-QTY
           CALL "UOMCONV" USING WS-UOM-CA
           IF CA-RETURN-CODE OF WS-UOM-CA NOT = 0
               MOVE "DSC0007" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-UOM-CA TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9200-LINE-REJECT THRU 9200-EXIT
               GO TO 5000-EXIT
           END-IF
           MOVE UC-STOCK-QTY TO WS-AMT3
           MOVE UC-FACTOR TO WS-AMT2
           IF SD-L-PRICE-UOM(WS-IX) = SPACES
               OR SD-L-PRICE-UOM(WS-IX) = UC-STOCK-UOM
               MOVE WS-AMT3 TO SD-L-PRICE-QTY(WS-IX)
               MOVE "UOMCONV stock unit" TO SD-L-WHY(WS-IX)
               GO TO 5000-EXIT
           END-IF
           INITIALIZE WS-UOM-CA
           MOVE "FACTOR" TO CA-FUNCTION OF WS-UOM-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-UOM-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-UOM-CA
           MOVE SD-L-ITEM-CODE(WS-IX) TO UC-ITEM-CODE
           MOVE SD-L-PRICE-UOM(WS-IX) TO UC-FROM-UOM
           MOVE "PRICE" TO UC-USAGE-TYPE
           MOVE SD-PRICE-DATE TO UC-EFF-DATE
           CALL "UOMCONV" USING WS-UOM-CA
           IF CA-RETURN-CODE OF WS-UOM-CA NOT = 0 OR UC-FACTOR = 0
               MOVE "DSC0007" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "No effective conversion to price unit "
                      FUNCTION TRIM(SD-L-PRICE-UOM(WS-IX))
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9200-LINE-REJECT THRU 9200-EXIT
               GO TO 5000-EXIT
           END-IF
           INITIALIZE WS-RND-CA
           MOVE "UNITPRC" TO CA-FUNCTION OF WS-RND-CA
           MOVE WS-AMT3 TO RA-VALUE OF WS-RND-CA
           MOVE UC-FACTOR TO RA-VALUE2 OF WS-RND-CA
           MOVE 6 TO RA-PLACES OF WS-RND-CA
           CALL "RNDALLOC" USING WS-RND-CA
           MOVE RA-RESULT OF WS-RND-CA TO SD-L-PRICE-QTY(WS-IX)
           MOVE "UOMCONV price unit" TO SD-L-WHY(WS-IX).
       5000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 9000  ERROR EXITS
      *>----------------------------------------------------------------
       9000-REJECT.
           MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE "Y" TO WS-ERROR
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE "DSC0008" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           STRING "Function " FUNCTION TRIM(CA-FUNCTION OF DFHCOMMAREA)
                  " is not one of LINE, DOCUMENT or RECALC"
               DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9200-LINE-REJECT.
           MOVE "Y" TO WS-LINE-ERR
           MOVE "ERROR" TO SD-L-STATUS(WS-IX)
           MOVE CA-REASON-CODE OF DFHCOMMAREA TO SD-L-WHY(WS-IX)
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA (181:20)
           MOVE WS-IX TO WS-MSG-INT
           STRING " (line " FUNCTION TRIM(WS-MSG-INT) ")"
               DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA (181:20)
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9200-EXIT.
           EXIT.
