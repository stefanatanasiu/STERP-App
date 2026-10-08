      *>================================================================
      *> PROGRAM-ID : UOMCONV
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *> PURPOSE    : THE UNIT OF MEASURE SERVICE.  TURNS AN ENTERED
      *>              QUANTITY INTO A STOCK QUANTITY USING AN EXPLICIT,
      *>              EFFECTIVE DATED, POSITIVE FACTOR.  A MISSING OR
      *>              ZERO CONVERSION IS AN ERROR - THE SUITE NEVER
      *>              ASSUMES ONE FOR ONE.  ALSO POLICES THE PERMITTED
      *>              QUANTITY PRECISION AND INDIVISIBILITY OF AN ITEM
      *>              AND COMPUTES THE STERLING UNIT COST OF A
      *>              PURCHASE PRICE.
      *> CALLED BY  : STKMOVE, AVAILQ, ITEMENQ, PURCHASE ORDER ENTRY,
      *>              SALES ORDER ENTRY, KIT ISSUE, THE .NET CLIENT
      *> INTERFACE  : CALL "UOMCONV" USING WS-UOMCONV-CA
      *>              COPYBOOK UOMCONVCA
      *> CALLS      : SQLIO (inv.Item, inv.Uom, inv.ItemUom)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-04-22 RJH  CR0418 ORIGINAL - STOCK CONTROL CONVERSION
      *> 1997-01-15 RJH  CR0601 PACK UNITS MUST BE ITEM SPECIFIC AFTER
      *>                 THE SLEEVE INCIDENT (SEE 3400-GUARD-PACK)
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS ON EFFECTIVE DATES
      *> 2004-07-02 RJH  CR1180 PRICE-PER QUANTITY FOR IMPORTED PARTS
      *> 2026-10-05 STERP ITM-005 TO ITM-007, SQLIO REWRITE
      *>================================================================
      *> REQ: ITM-005, ITM-006, ITM-007
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UOMCONV.
       AUTHOR. R J HOLLIS.
       DATE-WRITTEN. 22/04/1996.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ITMWORK.
       01  WS-CONTROL.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-ITEM-READ            PIC X VALUE "N".
           05  WS-USAGE                PIC X(10) VALUE SPACES.
           05  WS-FROM-DIM             PIC X(10) VALUE SPACES.
           05  WS-FROM-BASE            PIC X(10) VALUE SPACES.
           05  WS-FROM-FACTOR          PIC S9(9)V9(9) VALUE 0.
           05  WS-FROM-DP              PIC 9(2) VALUE 0.
           05  WS-TO-DIM               PIC X(10) VALUE SPACES.
           05  WS-TO-BASE              PIC X(10) VALUE SPACES.
           05  WS-TO-FACTOR            PIC S9(9)V9(9) VALUE 0.
           05  WS-TO-DP                PIC 9(2) VALUE 0.
           05  WS-TARGET-UOM           PIC X(10) VALUE SPACES.
           05  WS-MULTIPLES            PIC S9(15)V9(6) VALUE 0.
           05  WS-WHOLE                PIC S9(15) VALUE 0.
           05  WS-REMAIN               PIC S9(9)V9(9) VALUE 0.
           05  WS-DIVISOR              PIC S9(13)V9(9) VALUE 0.
           05  WS-LOOKUP-DONE          PIC X VALUE "N".
       LINKAGE SECTION.
       COPY UOMCONVCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "TOSTOCK"  PERFORM 1000-TO-STOCK
               WHEN "FROMSTK"  PERFORM 2000-FROM-STOCK
               WHEN "FACTOR"   PERFORM 3000-RESOLVE-FACTOR
               WHEN "CONVERT"  PERFORM 4000-DIMENSIONAL
               WHEN "CHECKQTY" PERFORM 5000-CHECK-QUANTUM
               WHEN "UNITCOST" PERFORM 6000-UNIT-COST
               WHEN "ITEMUOM"  PERFORM 7000-LIST-CONVERSIONS
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-ITEM-READ WS-SQL-FAILED
           MOVE "UOMCONV" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "ITM-005" TO CA-REQ-REF
           MOVE 0 TO UC-RESULT-QTY UC-FACTOR
           IF CA-FUNCTION NOT = "FROMSTK"
               MOVE 0 TO UC-STOCK-QTY
           END-IF
           MOVE 0 TO UC-INVERSE-FACTOR UC-ROUNDED-QTY UC-DECIMALS
           MOVE 0 TO UC-QUANTUM UC-EFF-FROM UC-EFF-TO UC-LINE-CNT
           MOVE 0 TO UC-UNIT-COST-GBP
           MOVE SPACES TO UC-STOCK-UOM UC-FROM-DIM UC-TO-DIM
           MOVE SPACES TO UC-PACK-DESC UC-SOURCE UC-TRACEABILITY
           MOVE "N" TO UC-ADJUSTED UC-INDIVISIBLE
           MOVE FUNCTION UPPER-CASE(UC-FROM-UOM) TO UC-FROM-UOM
           MOVE FUNCTION UPPER-CASE(UC-TO-UOM) TO UC-TO-UOM
           MOVE FUNCTION UPPER-CASE(UC-USAGE-TYPE) TO UC-USAGE-TYPE
           MOVE FUNCTION UPPER-CASE(UC-ITEM-CODE) TO UC-ITEM-CODE
           MOVE UC-USAGE-TYPE TO WS-USAGE
           IF WS-USAGE = SPACES
               MOVE "PURCHASE" TO WS-USAGE
           END-IF
           IF UC-EFF-DATE = 0
               MOVE CA-BUSINESS-DATE TO UC-EFF-DATE
           END-IF
           IF UC-EFF-DATE = 0
               MOVE 20260401 TO UC-EFF-DATE
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES OR CA-COMPANY = LOW-VALUES
               MOVE "UOM0001" TO CA-REASON-CODE
               MOVE "A company must be supplied to convert units"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION = "CONVERT"
               GO TO 0200-EXIT
           END-IF
           IF UC-ITEM-CODE = SPACES OR UC-ITEM-CODE = LOW-VALUES
               MOVE "UOM0001" TO CA-REASON-CODE
               MOVE "An item code is required for this conversion"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 0300  READ THE ITEM.  THE STOCK UNIT, THE QUANTUM AND THE
      *>       TRACEABILITY TYPE ALL GOVERN WHAT A QUANTITY MAY BE.
      *>----------------------------------------------------------------
       0300-READ-ITEM SECTION.
       0300-START.
           IF WS-ITEM-READ = "Y"
               GO TO 0300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT StockUomCode, QuantumQty, IsIndivisible, "
                  "TraceabilityType, LifecycleStatus FROM inv.Item "
                  "WHERE CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE UC-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "UOM0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(UC-ITEM-CODE)
                      " is not held in company "
                      FUNCTION TRIM(CA-COMPANY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               MOVE "Y" TO WS-ERROR
               ADD 1 TO CA-MSG-COUNT
               GO TO 0300-EXIT
           END-IF
           MOVE ITM-COL(1) TO UC-STOCK-UOM
           COMPUTE UC-QUANTUM = FUNCTION NUMVAL(ITM-COL(2))
           IF ITM-COL(3)(1:1) = "1"
               MOVE "Y" TO UC-INDIVISIBLE
           ELSE
               MOVE "N" TO UC-INDIVISIBLE
           END-IF
           MOVE ITM-COL(4) TO UC-TRACEABILITY
           IF UC-TRACEABILITY = "SERIAL"
               MOVE "Y" TO UC-INDIVISIBLE
               IF UC-QUANTUM NOT = 1
                   MOVE 1 TO UC-QUANTUM
               END-IF
           END-IF
           MOVE "Y" TO WS-ITEM-READ.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 0400  READ ONE UNIT OF MEASURE FROM THE REFERENCE TABLE.
      *>----------------------------------------------------------------
       0400-READ-UOM SECTION.
       0400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT UomDimension, FactorToBase, DecimalPlaces,"
                  " ISNULL(BaseUomCode,'') FROM inv.Uom WHERE "
                  "UomCode = ? AND IsActive = 1"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CODE TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           MOVE SPACES TO WS-TEXT
           MOVE 0 TO WS-FACTOR
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0400-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "UOM0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Unit of measure " FUNCTION TRIM(WS-CODE)
                      " is not known"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0400-EXIT
           END-IF
           MOVE ITM-COL(1) TO WS-TEXT
           COMPUTE WS-FACTOR = FUNCTION NUMVAL(ITM-COL(2))
           COMPUTE WS-IX = FUNCTION NUMVAL(ITM-COL(3))
           MOVE ITM-COL(4) TO WS-TEXT2.
       0400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  ENTERED QUANTITY -> STOCK QUANTITY (ITM-007).
      *>       stock quantity = entered quantity x stock units per unit
      *>----------------------------------------------------------------
       1000-TO-STOCK SECTION.
       1000-START.
           PERFORM 3000-RESOLVE-FACTOR
           IF CA-FAILED
               GO TO 1000-EXIT
           END-IF
           COMPUTE UC-STOCK-QTY ROUNDED =
               UC-ENTERED-QTY * UC-FACTOR
           MOVE UC-STOCK-QTY TO UC-RESULT-QTY
           MOVE UC-STOCK-UOM TO UC-TO-UOM
           PERFORM 5000-CHECK-QUANTUM
           IF CA-FAILED
               GO TO 1000-EXIT
           END-IF
           MOVE SPACES TO CA-MESSAGE
           MOVE UC-STOCK-QTY TO WS-QTY-ED
           STRING FUNCTION TRIM(WS-QTY-ED) " "
                  FUNCTION TRIM(UC-STOCK-UOM) " at "
                  DELIMITED BY SIZE INTO CA-MESSAGE
           MOVE UC-FACTOR TO WS-NUM-ED
           MOVE SPACES TO WS-TEXT
           STRING FUNCTION TRIM(CA-MESSAGE) FUNCTION TRIM(WS-NUM-ED)
                  " per " FUNCTION TRIM(UC-FROM-UOM)
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  STOCK QUANTITY -> ANOTHER UNIT.
      *>----------------------------------------------------------------
       2000-FROM-STOCK SECTION.
       2000-START.
           MOVE UC-TO-UOM TO WS-TARGET-UOM
           MOVE UC-TO-UOM TO UC-FROM-UOM
           PERFORM 3000-RESOLVE-FACTOR
           IF CA-FAILED
               GO TO 2000-EXIT
           END-IF
           MOVE WS-TARGET-UOM TO UC-TO-UOM
           IF UC-FACTOR = 0
               MOVE "UOM0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The conversion factor is zero, so the stock "
                      "quantity cannot be shown in that unit"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           COMPUTE UC-RESULT-QTY ROUNDED = UC-STOCK-QTY / UC-FACTOR
           COMPUTE UC-INVERSE-FACTOR ROUNDED = 1 / UC-FACTOR.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  RESOLVE THE EFFECTIVE FACTOR.  ORDER OF PREFERENCE
      *>       (1) AN APPROVED ITEM SPECIFIC ROW FOR THE USAGE
      *>       (2) AN APPROVED ITEM SPECIFIC ROW FOR ANY USAGE
      *>       (3) THE ENTERED UNIT IS THE STOCK UNIT        -> 1
      *>       (4) A DIMENSIONAL CONVERSION THROUGH inv.Uom
      *>       ANYTHING ELSE IS AN ERROR.  THERE IS NO IMPLIED 1:1.
      *>----------------------------------------------------------------
       3000-RESOLVE-FACTOR SECTION.
       3000-START.
           PERFORM 0300-READ-ITEM
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           IF UC-FROM-UOM = SPACES
               MOVE UC-STOCK-UOM TO UC-FROM-UOM
           END-IF
           PERFORM 3100-ITEM-FACTOR
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           IF UC-FACTOR NOT = 0
               GO TO 3000-CHECK
           END-IF
           IF UC-FROM-UOM = UC-STOCK-UOM
               MOVE 1 TO UC-FACTOR
               MOVE "IDENTITY" TO UC-SOURCE
               GO TO 3000-CHECK
           END-IF
           PERFORM 3200-DIMENSIONAL-FACTOR
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           IF UC-FACTOR = 0
               MOVE "UOM0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No effective conversion from "
                      FUNCTION TRIM(UC-FROM-UOM) " to "
                      FUNCTION TRIM(UC-STOCK-UOM) " for item "
                      FUNCTION TRIM(UC-ITEM-CODE)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           PERFORM 3400-GUARD-PACK.
       3000-CHECK.
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           IF UC-FACTOR <= 0
               MOVE "UOM0004" TO CA-REASON-CODE
               MOVE "A conversion factor must be greater than zero"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           COMPUTE UC-INVERSE-FACTOR ROUNDED = 1 / UC-FACTOR.
       3000-EXIT.
           EXIT.

       3100-ITEM-FACTOR SECTION.
       3100-START.
           MOVE 0 TO UC-FACTOR
           MOVE "N" TO WS-LOOKUP-DONE
      *>   first pass on the stated usage, second pass on any usage:
      *>   a pack relationship approved for purchasing is still the
      *>   right relationship when the same pack is sold or issued.
           PERFORM UNTIL WS-LOOKUP-DONE = "Y"
               PERFORM 3150-ITEM-FACTOR-READ
               IF CA-FAILED
                   MOVE "Y" TO WS-LOOKUP-DONE
               ELSE
                   IF UC-FACTOR NOT = 0 OR WS-USAGE = "ANY"
                       MOVE "Y" TO WS-LOOKUP-DONE
                   ELSE
                       MOVE "ANY" TO WS-USAGE
                   END-IF
               END-IF
           END-PERFORM.
       3100-EXIT.
           EXIT.

       3150-ITEM-FACTOR-READ SECTION.
       3150-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 StockUnitsPerUom, "
                  "ISNULL(PackDescription,''), "
                  "CONVERT(char(8), EffectiveFrom, 112), "
                  "ISNULL(CONVERT(char(8), EffectiveTo, 112),'') "
                  "FROM inv.ItemUom WHERE CompanyCode = ? AND "
                  "ItemCode = ? AND UomCode = ? AND (UsageType = ? "
                  "OR ? = 'ANY') AND EffectiveFrom <= "
                  "CONVERT(date, ?, 112) AND (EffectiveTo IS NULL OR "
                  "EffectiveTo >= CONVERT(date, ?, 112)) "
                  "ORDER BY EffectiveFrom DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE UC-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE UC-FROM-UOM TO SQLIO-PARM-VAL(3)
           MOVE WS-USAGE TO SQLIO-PARM-VAL(4)
           MOVE WS-USAGE TO SQLIO-PARM-VAL(5)
           MOVE UC-EFF-DATE TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(6)
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(7)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 7
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3150-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 3150-EXIT
           END-IF
           COMPUTE UC-FACTOR = FUNCTION NUMVAL(ITM-COL(1))
           MOVE ITM-COL(2) TO UC-PACK-DESC
           COMPUTE UC-EFF-FROM = FUNCTION NUMVAL(ITM-COL(3))
           IF ITM-COL(4) = SPACES
               MOVE 0 TO UC-EFF-TO
           ELSE
               COMPUTE UC-EFF-TO = FUNCTION NUMVAL(ITM-COL(4))
           END-IF
           MOVE "ITEMUOM" TO UC-SOURCE.
       3150-EXIT.
           EXIT.

       3200-DIMENSIONAL-FACTOR SECTION.
       3200-START.
           MOVE UC-FROM-UOM TO WS-CODE
           PERFORM 0400-READ-UOM
           IF CA-FAILED
               GO TO 3200-EXIT
           END-IF
           MOVE WS-TEXT TO WS-FROM-DIM
           MOVE WS-FACTOR TO WS-FROM-FACTOR
           MOVE WS-IX TO WS-FROM-DP
           MOVE WS-TEXT2 TO WS-FROM-BASE
           MOVE UC-STOCK-UOM TO WS-CODE
           PERFORM 0400-READ-UOM
           IF CA-FAILED
               GO TO 3200-EXIT
           END-IF
           MOVE WS-TEXT TO WS-TO-DIM
           MOVE WS-FACTOR TO WS-TO-FACTOR
           MOVE WS-IX TO WS-TO-DP
           MOVE WS-FROM-DIM TO UC-FROM-DIM
           MOVE WS-TO-DIM TO UC-TO-DIM
           MOVE WS-TO-DP TO UC-DECIMALS
           IF WS-FROM-DIM NOT = WS-TO-DIM
               MOVE "UOM0005" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Units " FUNCTION TRIM(UC-FROM-UOM) " ("
                      FUNCTION TRIM(WS-FROM-DIM) ") and "
                      FUNCTION TRIM(UC-STOCK-UOM) " ("
                      FUNCTION TRIM(WS-TO-DIM)
                      ") measure different things"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 3200-EXIT
           END-IF
           IF WS-TO-FACTOR = 0
               MOVE "UOM0004" TO CA-REASON-CODE
               MOVE "The stock unit has no factor to its base unit"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 3200-EXIT
           END-IF
           COMPUTE UC-FACTOR ROUNDED =
               WS-FROM-FACTOR / WS-TO-FACTOR
           MOVE "UOM" TO UC-SOURCE.
       3200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3400  CR0601.  A PACK UNIT (BOX, SET, SLEEVE) CARRIES A
      *>       NOMINAL FACTOR OF ONE IN THE REFERENCE TABLE BECAUSE ITS
      *>       CONTENTS ARE ITEM SPECIFIC.  TAKING THAT ONE AS A
      *>       CONVERSION ONCE RECEIVED ONE BOX AS ONE PIECE, SO A
      *>       DIMENSIONAL FACTOR OF EXACTLY ONE BETWEEN TWO DIFFERENT
      *>       "EACH" UNITS IS REFUSED UNTIL SOMEBODY APPROVES AN ITEM
      *>       SPECIFIC ROW.
      *>----------------------------------------------------------------
       3400-GUARD-PACK SECTION.
       3400-START.
           IF UC-SOURCE NOT = "UOM"
               GO TO 3400-EXIT
           END-IF
           IF UC-FROM-DIM NOT = "EACH"
               GO TO 3400-EXIT
           END-IF
           IF UC-FACTOR NOT = 1
               GO TO 3400-EXIT
           END-IF
           IF UC-FROM-UOM = UC-STOCK-UOM
               GO TO 3400-EXIT
           END-IF
           MOVE 0 TO UC-FACTOR
           MOVE "UOM0003" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Unit " FUNCTION TRIM(UC-FROM-UOM)
                  " is a pack unit: approve how many "
                  FUNCTION TRIM(UC-STOCK-UOM) " of item "
                  FUNCTION TRIM(UC-ITEM-CODE) " it holds"
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       3400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  PURE DIMENSIONAL CONVERSION, NO ITEM INVOLVED.
      *>----------------------------------------------------------------
       4000-DIMENSIONAL SECTION.
       4000-START.
           MOVE UC-FROM-UOM TO WS-CODE
           PERFORM 0400-READ-UOM
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           MOVE WS-TEXT TO UC-FROM-DIM
           MOVE WS-FACTOR TO WS-FROM-FACTOR
           MOVE UC-TO-UOM TO WS-CODE
           PERFORM 0400-READ-UOM
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           MOVE WS-TEXT TO UC-TO-DIM
           MOVE WS-FACTOR TO WS-TO-FACTOR
           MOVE WS-IX TO UC-DECIMALS
           IF UC-FROM-DIM NOT = UC-TO-DIM
               MOVE "UOM0005" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A conversion between "
                      FUNCTION TRIM(UC-FROM-DIM) " and "
                      FUNCTION TRIM(UC-TO-DIM)
                      " is not defined and may not be assumed"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 4000-EXIT
           END-IF
           IF WS-TO-FACTOR = 0 OR WS-FROM-FACTOR = 0
               MOVE "UOM0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A unit without a positive factor to its base "
                      "cannot take part in a conversion"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 4000-EXIT
           END-IF
           COMPUTE UC-FACTOR ROUNDED =
               WS-FROM-FACTOR / WS-TO-FACTOR
           COMPUTE UC-INVERSE-FACTOR ROUNDED = 1 / UC-FACTOR
           COMPUTE UC-RESULT-QTY ROUNDED =
               UC-ENTERED-QTY * UC-FACTOR
           MOVE "UOM" TO UC-SOURCE.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000  PERMITTED PRECISION AND INDIVISIBILITY (ITM-006).
      *>       A SERIALISED WHOLE ITEM CANNOT MOVE AS A FRACTION.
      *>----------------------------------------------------------------
       5000-CHECK-QUANTUM SECTION.
       5000-START.
           PERFORM 0300-READ-ITEM
           IF CA-FAILED
               GO TO 5000-EXIT
           END-IF
           IF UC-STOCK-QTY = 0
               MOVE UC-ENTERED-QTY TO UC-STOCK-QTY
           END-IF
           MOVE UC-STOCK-QTY TO UC-ROUNDED-QTY
           MOVE "N" TO UC-ADJUSTED
           IF UC-QUANTUM <= 0
               MOVE 1 TO UC-QUANTUM
           END-IF
           IF UC-INDIVISIBLE NOT = "Y"
               GO TO 5000-EXIT
           END-IF
           COMPUTE WS-MULTIPLES = UC-STOCK-QTY / UC-QUANTUM
           COMPUTE WS-WHOLE = FUNCTION INTEGER(WS-MULTIPLES)
           COMPUTE WS-REMAIN = WS-MULTIPLES - WS-WHOLE
           IF WS-REMAIN < 0.0000005 AND WS-REMAIN > -0.0000005
               GO TO 5000-EXIT
           END-IF
           COMPUTE UC-ROUNDED-QTY = (WS-WHOLE + 1) * UC-QUANTUM
           MOVE "Y" TO UC-ADJUSTED
           MOVE "UOM0006" TO CA-REASON-CODE
           MOVE UC-QUANTUM TO WS-QTY-ED
           MOVE SPACES TO CA-MESSAGE
           IF UC-TRACEABILITY = "SERIAL"
               STRING "Item " FUNCTION TRIM(UC-ITEM-CODE)
                      " is serialised and moves in whole units of "
                      FUNCTION TRIM(WS-QTY-ED)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               STRING "Item " FUNCTION TRIM(UC-ITEM-CODE)
                      " moves in multiples of "
                      FUNCTION TRIM(WS-QTY-ED) " "
                      FUNCTION TRIM(UC-STOCK-UOM)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           PERFORM 9000-REJECT.
       5000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 6000  STERLING UNIT COST (ITM-007, SPECIFICATION SECTION 5).
      *>   sterling unit cost = foreign price
      *>        / (price-per qty x stock units per price unit)
      *>        / foreign currency units per GBP
      *>   A PRICE OF 75.00 PER SLEEVE OF 25 PIECES IS 3.00 PER PIECE.
      *>----------------------------------------------------------------
       6000-UNIT-COST SECTION.
       6000-START.
           IF UC-PRICE-UOM NOT = SPACES
               MOVE UC-PRICE-UOM TO UC-FROM-UOM
           END-IF
           MOVE "PRICE" TO WS-USAGE
           PERFORM 3000-RESOLVE-FACTOR
           IF CA-FAILED
               GO TO 6000-EXIT
           END-IF
           IF UC-PRICE-PER-QTY = 0
               MOVE 1 TO UC-PRICE-PER-QTY
           END-IF
           IF UC-FX-PER-GBP = 0
               MOVE 1 TO UC-FX-PER-GBP
           END-IF
           COMPUTE WS-DIVISOR = UC-PRICE-PER-QTY * UC-FACTOR
           IF WS-DIVISOR = 0
               MOVE "UOM0007" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A price-per quantity of zero cannot produce "
                      "a unit cost"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 6000-EXIT
           END-IF
           COMPUTE UC-UNIT-COST-GBP ROUNDED =
               UC-FOREIGN-PRICE / WS-DIVISOR / UC-FX-PER-GBP
           MOVE UC-UNIT-COST-GBP TO WS-NUM-ED
           MOVE SPACES TO CA-MESSAGE
           STRING "Sterling cost " FUNCTION TRIM(WS-NUM-ED) " per "
                  FUNCTION TRIM(UC-STOCK-UOM)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       6000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 7000  THE ITEM'S EFFECTIVE CONVERSIONS, NEWEST FIRST.  THE
      *>       HISTORY MATTERS: AN EARLIER RECEIPT KEEPS THE FACTOR IT
      *>       WAS POSTED WITH.
      *>----------------------------------------------------------------
       7000-LIST-CONVERSIONS SECTION.
       7000-START.
           PERFORM 0300-READ-ITEM
           IF CA-FAILED
               GO TO 7000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT UomCode, UsageType, StockUnitsPerUom, "
                  "CONVERT(char(8), EffectiveFrom, 112), "
                  "ISNULL(CONVERT(char(8), EffectiveTo, 112),''), "
                  "ISNULL(PackDescription,'') FROM inv.ItemUom "
                  "WHERE CompanyCode = ? AND ItemCode = ? "
                  "ORDER BY UsageType, EffectiveFrom DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE UC-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 7000-EXIT
           END-IF
           MOVE 0 TO UC-LINE-CNT
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA ITM-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   IF UC-LINE-CNT < 20
                       ADD 1 TO UC-LINE-CNT
                       MOVE ITM-COL(1) TO UC-L-UOM(UC-LINE-CNT)
                       MOVE ITM-COL(2) TO UC-L-USAGE(UC-LINE-CNT)
                       COMPUTE UC-L-FACTOR(UC-LINE-CNT) =
                           FUNCTION NUMVAL(ITM-COL(3))
                       COMPUTE UC-L-FROM(UC-LINE-CNT) =
                           FUNCTION NUMVAL(ITM-COL(4))
                       IF ITM-COL(5) = SPACES
                           MOVE 0 TO UC-L-TO(UC-LINE-CNT)
                       ELSE
                           COMPUTE UC-L-TO(UC-LINE-CNT) =
                               FUNCTION NUMVAL(ITM-COL(5))
                       END-IF
                       MOVE ITM-COL(6) TO UC-L-PACK(UC-LINE-CNT)
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF UC-LINE-CNT = 0
               MOVE 16 TO CA-RETURN-CODE
               MOVE "UOM0003" TO CA-REASON-CODE
               MOVE "No unit conversions are recorded for this item"
                   TO CA-MESSAGE
           END-IF.
       7000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "UOM0008" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "UOMCONV does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE "Y" TO WS-ERROR WS-SQL-FAILED
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF SQLIO-ERRMSG(1:512) = SPACES
               MOVE "SQL failure in UOMCONV" TO CA-MESSAGE
           END-IF
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE 12 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
