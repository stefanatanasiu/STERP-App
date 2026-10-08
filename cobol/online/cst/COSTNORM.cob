      *>================================================================
      *> PROGRAM-ID : COSTNORM
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *> PURPOSE    : NORMALISE EVERY COST INPUT BEFORE IT IS COMPARED
      *>              OR ADDED.  A PER CARTON EURO PRICE IS NOT A PER
      *>              UNIT STERLING COST.  ALSO BUILDS THE ACQUISITION
      *>              COST OF A RECEIPT INCLUDING CUSTOMS DUTY AND THE
      *>              NON RECOVERABLE PART OF THE VAT, AND PROVES THAT
      *>              THE COST ELEMENTS RECONCILE TO THE TOTAL.
      *> CALLED BY  : FIFOVAL, MWAVAL, STDCOST, COSTROLL, LANDED,
      *>              PURCHASING, THE .NET CLIENT
      *> INTERFACE  : CALL "COSTNORM" USING WS-COSTNORM-CA
      *>              COPYBOOK COSTNORMCA
      *> CALLS      : UOMCONV (ITEM UNIT FACTORS), FXCONV (RATES),
      *>              RNDALLOC (ROUNDING), SQLIO (cst.ItemCost,
      *>              cst.StandardCost, cst.CostSet, cst.CostLayer)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-03-17 RJH  CR0502 ORIGINAL - DECIMAL CURRENCY ONLY
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT EFFECTIVE DATES
      *> 2003-01-06 RJH  CR1021 PRICE-PER QUANTITY FOR IMPORTED PARTS
      *> 2021-01-01 DJM  CR2188 BREXIT - CUSTOMS DUTY ON GB IMPORTS
      *> 2026-10-06 STERP CST-001 CST-003 CST-004 CST-024
      *> 2026-10-07 STERP CST-001 PROVENANCE WAS NEVER SET OR CLEARED:
      *>                 A QUOTE WAS NOT MARKED PROVISIONAL AND A
      *>                 STALE APPROVAL STATE SURVIVED A FAILED ENQUIRY
      *>================================================================
      *> REQ: CST-001, CST-002, CST-003, CST-004, CST-024
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COSTNORM.
       AUTHOR. R J HARTLEY.
       DATE-WRITTEN. 17/03/1997.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY CSTWORK.
       COPY UOMCONVCA REPLACING ==DFHCOMMAREA== BY ==WS-UOM-CA==.
       COPY FXCONVCA  REPLACING ==DFHCOMMAREA== BY ==WS-FX-CA==.
       COPY RNDALLOCCA REPLACING ==DFHCOMMAREA== BY ==WS-RND-CA==.
       01  WS-FLAGS.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-BASIS-FOUND          PIC X VALUE "N".
               88  WS-GOT-BASIS                  VALUE "Y".
           05  WS-FACTOR               PIC S9(9)V9(9) VALUE 0.
           05  WS-RATE                 PIC S9(9)V9(8) VALUE 0.
           05  WS-NET-GOODS            PIC S9(13)V9(2) VALUE 0.
           05  WS-DUTIABLE             PIC S9(13)V9(2) VALUE 0.
           05  WS-VAT-BASE             PIC S9(13)V9(2) VALUE 0.
           05  WS-ELEM-SUM             PIC S9(13)V9(2) VALUE 0.
       LINKAGE SECTION.
       COPY COSTNORMCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION OF DFHCOMMAREA
               WHEN "CONTEXT"  PERFORM 1000-CONTEXT
               WHEN "NORMCOST" PERFORM 2000-NORMALISE
               WHEN "PURCOST"  PERFORM 3000-PURCHASE-COST
               WHEN "ELEMENTS" PERFORM 4000-ELEMENT-PROOF
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
      *> 0100  EVERY CALL STARTS CLEAN.  WORKING STORAGE SURVIVES
      *>       BETWEEN CALLS INSIDE THE DLL SO NOTHING MAY BE ASSUMED.
      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-SQL-FAILED WS-BASIS-FOUND
           MOVE "COSTNORM" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "CST-003" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE 0 TO WS-FACTOR WS-RATE WS-NET-GOODS WS-DUTIABLE
           MOVE 0 TO WS-VAT-BASE WS-ELEM-SUM
           MOVE 0 TO CN-FACTOR CN-GOODS-GBP CN-DISCOUNT-GBP
           MOVE 0 TO CN-DUTY-GBP CN-VAT-TOTAL-GBP CN-VAT-RECOVER-GBP
           MOVE 0 TO CN-NONREC-VAT-GBP CN-UNIT-COST-GBP CN-EXT-COST-GBP
           MOVE 0 TO CN-EL-MATERIAL CN-EL-LANDED CN-EL-SUBCON
           MOVE 0 TO CN-EL-RECOVERY CN-EL-EXCLUDED
           MOVE SPACES TO CN-WARNING CN-SOURCE CN-BASIS-USED
      *>   CST-001.  THE PROVENANCE BELONGS TO THIS CALL ALONE.  THE
      *>   COMMAREA IS THE CALLER'S BUFFER, SO AN APPROVAL STATE LEFT
      *>   BY AN EARLIER ENQUIRY MUST NOT SURVIVE INTO ONE WHOSE
      *>   BASIS TURNS OUT TO BE UNAVAILABLE.
           MOVE SPACES TO CN-APPROVAL-STATE CN-EVIDENCE
           MOVE "N" TO CN-PROVISIONAL
           MOVE "COMPLETE" TO CN-COMPLETENESS
           MOVE FUNCTION UPPER-CASE(CN-ITEM-CODE) TO CN-ITEM-CODE
           MOVE FUNCTION UPPER-CASE(CN-PURPOSE) TO CN-PURPOSE
           MOVE FUNCTION UPPER-CASE(CN-BASIS) TO CN-BASIS
           MOVE FUNCTION UPPER-CASE(CN-CURRENCY) TO CN-CURRENCY
           IF CN-CURRENCY = SPACES
               MOVE "GBP" TO CN-CURRENCY
           END-IF
           IF CN-PURPOSE = SPACES
               MOVE "ENQUIRY" TO CN-PURPOSE
           END-IF
      *>   CST-001.  A QUOTE IS AN ESTIMATE.  IT IS STAMPED AS ONE SO
      *>   THAT NOTHING DOWNSTREAM CAN PRESENT IT AS AN ACTUAL
      *>   INVENTORY VALUE.
           IF CN-FOR-QUOTE
               MOVE "Y" TO CN-PROVISIONAL
               MOVE "Quote estimate - not an inventory valuation"
                   TO CN-EVIDENCE
           END-IF
           IF CN-EFF-DATE = 0
               MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO CN-EFF-DATE
           END-IF
           IF CN-PRICE-PER-QTY = 0
               MOVE 1 TO CN-PRICE-PER-QTY
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 0200  CST-001.  AN ENQUIRY WITHOUT A COMPANY, AN ITEM OR A
      *>       QUANTITY HAS NO MEANING AND IS REFUSED.
      *>----------------------------------------------------------------
       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY OF DFHCOMMAREA = SPACES
              OR CN-ITEM-CODE = SPACES
               MOVE "CST0101" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A company and an item code are required"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION OF DFHCOMMAREA NOT = "CONTEXT"
              AND CA-FUNCTION OF DFHCOMMAREA NOT = "ELEMENTS"
               IF CN-QUANTITY <= 0
                   MOVE "CST0103" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE "Quantity must be greater than zero"
                       TO CA-MESSAGE OF DFHCOMMAREA
                   PERFORM 9000-REJECT
                   GO TO 0200-EXIT
               END-IF
               IF CN-PRICE-PER-QTY <= 0
                   MOVE "CST0104" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE "Price-per quantity must be positive"
                       TO CA-MESSAGE OF DFHCOMMAREA
                   PERFORM 9000-REJECT
                   GO TO 0200-EXIT
               END-IF
           END-IF
           IF CN-VAT-RECOVER-PCT < 0 OR CN-VAT-RECOVER-PCT > 100
               MOVE "CST0107" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "VAT recovery percentage must be 0 to 100"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  CST-001.  STATE THE BASIS AND FETCH IT.  IF THE ASKED
      *>       FOR BASIS IS NOT HELD THE ANSWER IS "NOT AVAILABLE".
      *>       WE DO NOT QUIETLY HAND BACK A DIFFERENT NUMBER - THAT
      *>       IS HOW A QUOTE ESTIMATE ENDS UP IN THE BALANCE SHEET.
      *>----------------------------------------------------------------
       1000-CONTEXT SECTION.
       1000-START.
           IF CN-BASIS = SPACES
               MOVE "AVERAGE" TO CN-BASIS
           END-IF
           EVALUATE CN-BASIS
               WHEN "AVERAGE"   PERFORM 1100-AVERAGE-BASIS
               WHEN "STANDARD"  PERFORM 1200-STANDARD-BASIS
               WHEN "LASTPURCH" PERFORM 1300-LASTPUR-BASIS
               WHEN "FIFO"      PERFORM 1400-FIFO-BASIS
               WHEN OTHER
                   MOVE "CST0102" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
                   STRING "Cost basis " FUNCTION TRIM(CN-BASIS)
                          " is not one of AVERAGE, STANDARD, "
                          "LASTPURCH or FIFO"
                       DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
                   PERFORM 9000-REJECT
                   GO TO 1000-EXIT
           END-EVALUATE
           IF WS-SQL-BAD
               GO TO 1000-EXIT
           END-IF
           IF NOT WS-GOT-BASIS
               MOVE "INCOMPLETE" TO CN-COMPLETENESS
               MOVE 0 TO CN-UNIT-COST-GBP
               MOVE SPACES TO CN-BASIS-USED
               MOVE "CST0102" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "No " FUNCTION TRIM(CN-BASIS)
                      " cost is held for item "
                      FUNCTION TRIM(CN-ITEM-CODE)
                      " - another basis will not be substituted"
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               MOVE 16 TO CA-RETURN-CODE OF DFHCOMMAREA
               GO TO 1000-EXIT
           END-IF
           MOVE CN-BASIS TO CN-BASIS-USED
           MOVE "GBP" TO CN-CURRENCY
           COMPUTE CN-EXT-COST-GBP ROUNDED =
               CN-UNIT-COST-GBP * CN-QUANTITY
           MOVE CN-EXT-COST-GBP TO CN-EL-MATERIAL.
       1000-EXIT.
           EXIT.

       1100-AVERAGE-BASIS SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT AverageUnitCost, CostMethod FROM "
                  "cst.ItemCost WHERE CompanyCode = ? AND ItemCode "
                  "= ? AND Revision = ? AND SiteCode = '*'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE CN-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE CN-REVISION TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-GOT-ROW
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO CN-UNIT-COST-GBP
               MOVE "Y" TO WS-BASIS-FOUND
               MOVE "cst.ItemCost" TO CN-SOURCE
               MOVE WS-COL(2) TO CN-APPROVAL-STATE
           END-IF.
       1100-EXIT.
           EXIT.

       1200-STANDARD-BASIS SECTION.
       1200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT sc.TotalUnitCost, cs.CostSetCode FROM "
                  "cst.StandardCost sc JOIN cst.CostSet cs ON "
                  "cs.CompanyCode = sc.CompanyCode AND "
                  "cs.CostSetCode = sc.CostSetCode WHERE "
                  "sc.CompanyCode = ? AND sc.ItemCode = ? AND "
                  "cs.Status = 'ACTIVE' AND cs.Purpose = 'STANDARD' "
                  "AND cs.EffectiveFrom <= ? ORDER BY "
                  "cs.EffectiveFrom DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE CN-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE CN-EFF-DATE TO WS-DATE-NUM
           MOVE WS-DATE-NUM TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-GOT-ROW
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO CN-UNIT-COST-GBP
               MOVE "Y" TO WS-BASIS-FOUND
               MOVE "cst.StandardCost" TO CN-SOURCE
               MOVE WS-COL(2) TO CN-COST-SET
               MOVE "ACTIVE" TO CN-APPROVAL-STATE
           END-IF.
       1200-EXIT.
           EXIT.

       1300-LASTPUR-BASIS SECTION.
       1300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT LastPurchaseCost FROM cst.ItemCost WHERE "
                  "CompanyCode = ? AND ItemCode = ? AND "
                  "LastPurchaseCost IS NOT NULL"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE CN-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-GOT-ROW
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO CN-UNIT-COST-GBP
               MOVE "Y" TO WS-BASIS-FOUND
               MOVE "cst.ItemCost" TO CN-SOURCE
               MOVE "REFERENCE" TO CN-APPROVAL-STATE
               MOVE "A last purchase price is a commercial reference"
                   TO CN-WARNING
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
           END-IF.
       1300-EXIT.
           EXIT.

       1400-FIFO-BASIS SECTION.
       1400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT SUM(QtyRemaining), "
                  "SUM(ValueRemainingGBP + LandedAdjustmentGBP) "
                  "FROM cst.CostLayer WHERE CompanyCode = ? AND "
                  "ItemCode = ? AND IsClosed = 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE CN-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-GOT-ROW
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO WS-QTY1
               MOVE FUNCTION NUMVAL(WS-COL(2)) TO WS-AMT1
               IF WS-QTY1 > 0
                   COMPUTE CN-UNIT-COST-GBP ROUNDED =
                       WS-AMT1 / WS-QTY1
                   MOVE "Y" TO WS-BASIS-FOUND
                   MOVE "cst.CostLayer" TO CN-SOURCE
                   MOVE "LAYERED" TO CN-APPROVAL-STATE
               END-IF
           END-IF.
       1400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  CST-003.  THE ONLY SANCTIONED ROUTE FROM A QUOTED PRICE
      *>       TO A STERLING COST PER STOCK UNIT.
      *>----------------------------------------------------------------
       2000-NORMALISE SECTION.
       2000-START.
           PERFORM 2100-UNIT-FACTOR
           IF WS-INPUT-BAD
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-EXCHANGE-RATE
           IF WS-INPUT-BAD
               GO TO 2000-EXIT
           END-IF
           COMPUTE WS-CALC-1 =
               CN-FOREIGN-PRICE / (CN-PRICE-PER-QTY * WS-FACTOR)
           COMPUTE WS-CALC-1 = WS-CALC-1 / WS-RATE
           IF CN-DISCOUNT-PCT NOT = 0
               COMPUTE WS-CALC-1 =
                   WS-CALC-1 * (1 - (CN-DISCOUNT-PCT / 100))
           END-IF
           PERFORM 2300-ROUND-UNIT
           MOVE WS-FACTOR TO CN-FACTOR
           MOVE "GBP" TO CN-CURRENCY
           COMPUTE CN-EXT-COST-GBP ROUNDED =
               CN-UNIT-COST-GBP * CN-QUANTITY
           MOVE CN-EXT-COST-GBP TO CN-EL-MATERIAL
           MOVE "NORMALISED" TO CN-BASIS-USED.
       2000-EXIT.
           EXIT.

      *>  STOCK UNITS PER PRICE UNIT.  A MISSING CONVERSION IS AN
      *>  ERROR - IT IS NEVER AN IMPLIED ONE FOR ONE (ITM-005).
       2100-UNIT-FACTOR SECTION.
       2100-START.
           MOVE 1 TO WS-FACTOR
           IF CN-PRICE-UOM = SPACES OR CN-PRICE-UOM = CN-STOCK-UOM
               GO TO 2100-EXIT
           END-IF
           INITIALIZE WS-UOM-CA
           MOVE "UOMCONV" TO CA-PROGRAM OF WS-UOM-CA
           MOVE "FACTOR" TO CA-FUNCTION OF WS-UOM-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-UOM-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-UOM-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-UOM-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-UOM-CA
           MOVE CN-ITEM-CODE TO UC-ITEM-CODE
           MOVE CN-PRICE-UOM TO UC-FROM-UOM
           MOVE "PRICE" TO UC-USAGE-TYPE
           MOVE CN-EFF-DATE TO UC-EFF-DATE
           CALL "UOMCONV" USING WS-UOM-CA
           IF CA-RETURN-CODE OF WS-UOM-CA > 4 OR UC-FACTOR <= 0
               MOVE "CST0106" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "No conversion from " FUNCTION TRIM(CN-PRICE-UOM)
                      " to the stock unit of "
                      FUNCTION TRIM(CN-ITEM-CODE)
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 2100-EXIT
           END-IF
           MOVE UC-FACTOR TO WS-FACTOR
           MOVE UC-STOCK-UOM TO CN-STOCK-UOM.
       2100-EXIT.
           EXIT.

       2200-EXCHANGE-RATE SECTION.
       2200-START.
           MOVE 1 TO WS-RATE
           IF CN-CURRENCY = "GBP" OR CN-CURRENCY = SPACES
               GO TO 2200-EXIT
           END-IF
           IF CN-FX-RATE > 0
               MOVE CN-FX-RATE TO WS-RATE
               GO TO 2200-EXIT
           END-IF
           INITIALIZE WS-FX-CA
           MOVE "FXCONV" TO CA-PROGRAM OF WS-FX-CA
           MOVE "GETRATE" TO CA-FUNCTION OF WS-FX-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-FX-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-FX-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-FX-CA
           MOVE CN-CURRENCY TO FX-CURRENCY
           IF CN-FX-RATE-DATE = 0
               MOVE CN-EFF-DATE TO FX-RATE-DATE
           ELSE
               MOVE CN-FX-RATE-DATE TO FX-RATE-DATE
           END-IF
           IF CN-FX-RATE-TYPE = SPACES
               MOVE "SPOT" TO FX-RATE-TYPE
           ELSE
               MOVE CN-FX-RATE-TYPE TO FX-RATE-TYPE
           END-IF
           MOVE "FCYPERGBP" TO FX-QUOTE-BASIS
           CALL "FXCONV" USING WS-FX-CA
           IF CA-RETURN-CODE OF WS-FX-CA > 4 OR FX-RATE <= 0
               MOVE "CST0105" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "No usable exchange rate for "
                      FUNCTION TRIM(CN-CURRENCY)
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 2200-EXIT
           END-IF
           MOVE FX-RATE TO WS-RATE CN-FX-RATE
           MOVE FX-RATE-FOUND-DATE TO CN-FX-RATE-DATE.
       2200-EXIT.
           EXIT.

      *>  UNIT COSTS ARE KEPT TO SIX DECIMALS - NO PREMATURE ROUNDING.
       2300-ROUND-UNIT SECTION.
       2300-START.
           INITIALIZE WS-RND-CA
           MOVE "RNDALLOC" TO CA-PROGRAM OF WS-RND-CA
           MOVE "ROUND" TO CA-FUNCTION OF WS-RND-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-RND-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-RND-CA
           MOVE 6 TO RA-PLACES
           MOVE WS-CALC-1 TO RA-VALUE
           CALL "RNDALLOC" USING WS-RND-CA
           IF CA-RETURN-CODE OF WS-RND-CA > 4
               COMPUTE CN-UNIT-COST-GBP ROUNDED = WS-CALC-1
           ELSE
               MOVE RA-RESULT TO CN-UNIT-COST-GBP
           END-IF.
       2300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  CST-004.  ACQUISITION COST OF A RECEIPT.  RECOVERABLE
      *>       VAT IS NEVER CAPITALISED; THE IRRECOVERABLE PART IS.
      *>       DUTY IS CHARGED ON THE NET GOODS VALUE (CR2188).
      *>----------------------------------------------------------------
       3000-PURCHASE-COST SECTION.
       3000-START.
           PERFORM 2100-UNIT-FACTOR
           IF WS-INPUT-BAD
               GO TO 3000-EXIT
           END-IF
           PERFORM 2200-EXCHANGE-RATE
           IF WS-INPUT-BAD
               GO TO 3000-EXIT
           END-IF
           COMPUTE WS-CALC-1 =
               CN-FOREIGN-PRICE / (CN-PRICE-PER-QTY * WS-FACTOR)
           COMPUTE WS-CALC-1 = WS-CALC-1 / WS-RATE
           COMPUTE CN-GOODS-GBP ROUNDED = WS-CALC-1 * CN-QUANTITY
           COMPUTE CN-DISCOUNT-GBP ROUNDED =
               CN-GOODS-GBP * CN-DISCOUNT-PCT / 100
           COMPUTE WS-NET-GOODS = CN-GOODS-GBP - CN-DISCOUNT-GBP
           IF CN-DUTY-AMOUNT NOT = 0
               MOVE CN-DUTY-AMOUNT TO CN-DUTY-GBP
           ELSE
               COMPUTE CN-DUTY-GBP ROUNDED =
                   WS-NET-GOODS * CN-DUTY-PCT / 100
           END-IF
           COMPUTE WS-VAT-BASE = WS-NET-GOODS + CN-DUTY-GBP
                                 + CN-FREIGHT-GBP
                                 + CN-OTHER-CHARGE-GBP
           COMPUTE CN-VAT-TOTAL-GBP ROUNDED =
               WS-VAT-BASE * CN-VAT-RATE / 100
           COMPUTE CN-VAT-RECOVER-GBP ROUNDED =
               CN-VAT-TOTAL-GBP * CN-VAT-RECOVER-PCT / 100
           COMPUTE CN-NONREC-VAT-GBP =
               CN-VAT-TOTAL-GBP - CN-VAT-RECOVER-GBP
           COMPUTE CN-EXT-COST-GBP = WS-NET-GOODS + CN-DUTY-GBP
                                     + CN-FREIGHT-GBP
                                     + CN-OTHER-CHARGE-GBP
                                     + CN-NONREC-VAT-GBP
           COMPUTE CN-UNIT-COST-GBP ROUNDED =
               CN-EXT-COST-GBP / CN-QUANTITY
           MOVE WS-FACTOR TO CN-FACTOR
           MOVE WS-NET-GOODS TO CN-EL-MATERIAL
           COMPUTE CN-EL-LANDED = CN-DUTY-GBP + CN-FREIGHT-GBP
                                  + CN-OTHER-CHARGE-GBP
                                  + CN-NONREC-VAT-GBP
           MOVE CN-VAT-RECOVER-GBP TO CN-EL-EXCLUDED
           MOVE "Recoverable VAT excluded from inventory cost"
               TO CN-EL-EXCL-NOTE
           MOVE "ACQUISITION" TO CN-BASIS-USED
           MOVE "GBP" TO CN-CURRENCY
           PERFORM 4000-ELEMENT-PROOF.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  CST-002.  THE ELEMENTS MUST ADD BACK TO THE TOTAL OR
      *>       THE RESULT IS NOT PUBLISHABLE.
      *>----------------------------------------------------------------
       4000-ELEMENT-PROOF SECTION.
       4000-START.
           COMPUTE WS-ELEM-SUM = CN-EL-MATERIAL + CN-EL-LANDED
                                 + CN-EL-SUBCON - CN-EL-RECOVERY
           IF WS-ELEM-SUM NOT = CN-EXT-COST-GBP
               MOVE "CST0108" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               MOVE WS-ELEM-SUM TO WS-MONEY-ED
               STRING "Cost elements total " FUNCTION TRIM(WS-MONEY-ED)
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               MOVE CN-EXT-COST-GBP TO WS-MONEY-ED
               STRING FUNCTION TRIM(CA-MESSAGE OF DFHCOMMAREA)
                      " but the cost is "
                      FUNCTION TRIM(WS-MONEY-ED)
                   DELIMITED BY SIZE INTO WS-WORK-TEXT
               MOVE WS-WORK-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               MOVE "INCOMPLETE" TO CN-COMPLETENESS
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           END-IF.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 8000  SHARED SQL HELPERS
      *>----------------------------------------------------------------
       8000-SINGLE-ROW SECTION.
       8000-START.
           MOVE "N" TO WS-ROW-FOUND
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           EVALUATE TRUE
               WHEN SQLIO-SQLCODE = 0
                   MOVE "Y" TO WS-ROW-FOUND
               WHEN SQLIO-SQLCODE = 100
                   CONTINUE
               WHEN OTHER
                   PERFORM 8900-SQL-ERROR
           END-EVALUATE.
       8000-EXIT.
           EXIT.

       8900-SQL-ERROR SECTION.
       8900-START.
           MOVE "Y" TO WS-SQL-FAILED
           MOVE "CST9001" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SQLIO-ERRMSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE 12 TO CA-RETURN-CODE OF DFHCOMMAREA.
       8900-EXIT.
           EXIT.

       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "CST0109" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           STRING "Function " FUNCTION TRIM(CA-FUNCTION OF DFHCOMMAREA)
                  " is not supported by COSTNORM"
               DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
