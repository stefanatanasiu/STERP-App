      *>================================================================
      *> PROGRAM-ID : LANDCOST
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : RECORD ESTIMATED LANDED COST CATEGORIES, ALLOCATE
      *>              THEM TO ELIGIBLE RECEIPT LINE GROUPS ON A
      *>              JUSTIFIED BASIS, AND LATER RECONCILE THE ACTUAL
      *>              CHARGE SO THAT ONLY THE DIFFERENCE MOVES.  THE
      *>              TOTAL ATTRIBUTABLE CHARGE IS CAPITALISED EXACTLY
      *>              ONCE AND RECOVERABLE VAT NEVER ENTERS INVENTORY
      *>              (PO-015, PO-016, FRS 102 SECTION 13).
      *> CALLED BY  : .NET PURCHASING FACADE, IMPVAT (duty), APMATCH
      *> INTERFACE  : CALL "LANDCOST" USING DFHCOMMAREA (LANDCOSTCA)
      *> CALLS      : RNDALLOC (weighted allocation, COM-011), POPOST,
      *>              SQLIO -> cst.LandedCost, cst.LandedCostAllocation
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-09-02 RJH  CR0833 SHIPMENT COSTING
      *> 2005-11-14 DJM  CR1310 ESTIMATE AND ACTUAL KEPT APART
      *> 2021-01-01 TMW  BREXIT - DUTY IS LANDED, IMPORT VAT IS NOT
      *> 2026-10-06 STERP PO-015, PO-016 AND AC-056
      *>================================================================
      *> REQ: PO-015, PO-016, PO-025   AC: AC-056
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LANDCOST.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 02/09/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY POACCNTS.
       COPY POPOSTWK.
       COPY RNDALLOCCA REPLACING ==DFHCOMMAREA== BY ==WS-RNDALLOC-CA==
                      LEADING ==CA-== BY ==RH-==.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-ID-ROW.
           05  WS-ID-VALUE             PIC X(16).
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(40).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-N                    PIC S9(4) VALUE 0.
           05  WS-NUM                  PIC S9(15)V9(6) VALUE 0.
           05  WS-TARGET-GBP           PIC S9(13)V99 VALUE 0.
           05  WS-SUM-SHARE            PIC S9(13)V99 VALUE 0.
           05  WS-INV-PART             PIC S9(13)V99 VALUE 0.
           05  WS-COGS-PART            PIC S9(13)V99 VALUE 0.
           05  WS-TOT-QTY              PIC S9(13)V9(6) VALUE 0.
           05  WS-LINE-IX              PIC S9(4) VALUE 0.
           05  WS-AMT-ED               PIC -(11)9.99.
           05  WS-QTY-ED               PIC -(13)9.999999.
           05  WS-REF-ED               PIC X(8).
       LINKAGE SECTION.
       COPY LANDCOSTCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "CALC"
                   PERFORM 2000-ALLOCATE THRU 2000-EXIT
               WHEN "ESTIMATE"
                   PERFORM 3000-ESTIMATE THRU 3000-EXIT
               WHEN "ALLOCATE"
                   PERFORM 2000-ALLOCATE THRU 2000-EXIT
                   IF NOT WS-FATAL
                       PERFORM 3000-ESTIMATE THRU 3000-EXIT
                       PERFORM 3500-WRITE-ALLOC THRU 3500-EXIT
                       PERFORM 6000-POST THRU 6000-EXIT
                   END-IF
               WHEN "ACTUAL"
                   PERFORM 4000-ACTUAL THRU 4000-EXIT
               WHEN "ENQUIRE"
                   PERFORM 5000-ENQUIRE THRU 5000-EXIT
               WHEN OTHER
                   MOVE "LND0006" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                          " is not supported by LANDCOST"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE 8 TO CA-RETURN-CODE
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE "N" TO WS-FATAL-SW
           MOVE "LANDCOST" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE 0 TO LC-BASIS-TOTAL LC-ALLOCATED-GBP
           MOVE 0 TO LC-RESIDUAL-GBP LC-TO-INVENTORY LC-TO-COGS
           MOVE 0 TO LC-TO-VARIANCE LC-MISSING-BASIS
           MOVE 0 TO LC-INELIGIBLE-CNT
           MOVE SPACES TO LC-JOURNAL-NUMBER
           IF LC-CURRENCY = SPACES
               MOVE "GBP" TO LC-CURRENCY
           END-IF
           IF LC-FX-RATE = 0
               MOVE 1 TO LC-FX-RATE
           END-IF
           IF LC-BASIS = SPACES
               MOVE "VALUE" TO LC-BASIS
           END-IF
           IF LC-ALLOC-DATE = 0
               MOVE CA-BUSINESS-DATE TO LC-ALLOC-DATE
           END-IF
           IF LC-ESTIMATE-GBP = 0 AND LC-ESTIMATE-AMT NOT = 0
               COMPUTE LC-ESTIMATE-GBP ROUNDED =
                       LC-ESTIMATE-AMT / LC-FX-RATE
           END-IF
           IF LC-ACTUAL-GBP = 0 AND LC-ACTUAL-AMT NOT = 0
               COMPUTE LC-ACTUAL-GBP ROUNDED =
                       LC-ACTUAL-AMT / LC-FX-RATE
           END-IF
           IF LC-CHARGE-REF = SPACES AND CA-FUNCTION NOT = "CALC"
               MOVE "LND0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A charge reference is required"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1000-EXIT
           END-IF
           PERFORM 1200-ELIGIBILITY THRU 1200-EXIT.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> DEMURRAGE, UNNECESSARY STORAGE, ABNORMAL WASTE AND GENERAL
      *> ADMINISTRATION ARE EXPENSED, NOT CAPITALISED.  RECOVERABLE
      *> VAT ON A FREIGHT INVOICE IS INPUT VAT, NEVER LANDED COST.
      *>----------------------------------------------------------------
       1200-ELIGIBILITY SECTION.
       1200-START.
           IF LC-IS-CAPITALISE = SPACES
               MOVE "Y" TO LC-IS-CAPITALISE
           END-IF
           IF LC-OTHER
               MOVE "N" TO LC-IS-CAPITALISE
           END-IF
           IF NOT LC-CAPITALISE
               MOVE "LND0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Category " FUNCTION TRIM(LC-CATEGORY)
                      " is not necessary to bring the goods to their "
                      "present location and condition, so it is "
                      "expensed and not capitalised"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 4 TO CA-RETURN-CODE
           END-IF
           IF LC-RECOVERABLE-VAT NOT = 0
               MOVE "LND0007" TO CA-REASON-CODE
               MOVE 4 TO CA-RETURN-CODE
           END-IF.
       1200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> ALLOCATION.  THE CHARGE IS SPREAD OVER THE ELIGIBLE RECIPIENTS
      *> BY THE CHOSEN BASIS THROUGH RNDALLOC, WHICH OWNS THE PENNY
      *> ROUNDING RULE, SO THE PARTS ALWAYS ADD BACK TO THE CHARGE.
      *> AC-056: 600.00 AND 400.00 WITH A 75.00 FREIGHT CHARGE GIVE
      *> 45.00 AND 30.00.
      *>----------------------------------------------------------------
       2000-ALLOCATE SECTION.
       2000-START.
           IF LC-LINE-CNT = 0
               MOVE "LND0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No eligible receipt lines were supplied for "
                      "this charge"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF LC-LINE-CNT > 40
               MOVE 40 TO LC-LINE-CNT
           END-IF
           MOVE LC-ESTIMATE-GBP TO WS-TARGET-GBP
           IF CA-FUNCTION = "ACTUAL" OR LC-DELTA-GBP NOT = 0
               MOVE LC-DELTA-GBP TO WS-TARGET-GBP
           END-IF
           MOVE 0 TO LC-BASIS-TOTAL
           MOVE 0 TO LC-MISSING-BASIS LC-INELIGIBLE-CNT
           MOVE 0 TO WS-LINE-IX
           INITIALIZE WS-RNDALLOC-CA
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LC-LINE-CNT
               MOVE 0 TO LC-L-SHARE-GBP(WS-I)
               MOVE 0 TO LC-L-DELTA-GBP(WS-I)
               MOVE 0 TO LC-L-TO-INV(WS-I)
               MOVE 0 TO LC-L-TO-COGS(WS-I)
               IF LC-L-ELIGIBLE(WS-I) = SPACES
                   MOVE "Y" TO LC-L-ELIGIBLE(WS-I)
               END-IF
               IF LC-L-ELIGIBLE(WS-I) NOT = "Y"
                   ADD 1 TO LC-INELIGIBLE-CNT
               ELSE
                   IF LC-L-BASIS-VAL(WS-I) <= 0
                       ADD 1 TO LC-MISSING-BASIS
                   END-IF
                   ADD LC-L-BASIS-VAL(WS-I) TO LC-BASIS-TOTAL
                   ADD 1 TO WS-LINE-IX
                   MOVE WS-I TO WS-REF-ED
                   MOVE WS-REF-ED TO RA-LINE-REF(WS-LINE-IX)
                   MOVE LC-L-BASIS-VAL(WS-I)
                     TO RA-WEIGHT(WS-LINE-IX)
               END-IF
           END-PERFORM
           IF LC-BASIS-TOTAL <= 0
               MOVE "LND0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The aggregate "
                      FUNCTION TRIM(LC-BASIS)
                      " basis is zero, so the charge cannot be "
                      "allocated; review the missing weights or "
                      "values first"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF LC-BY-EXPLICIT
               PERFORM 2200-EXPLICIT THRU 2200-EXIT
               GO TO 2000-CHECK
           END-IF
           MOVE "ALLOC" TO RH-FUNCTION
           MOVE "WEIGHT" TO RA-MODE
           MOVE WS-TARGET-GBP TO RA-AMOUNT
           MOVE 2 TO RA-PLACES
           MOVE WS-LINE-IX TO RA-LINE-CNT
           MOVE CA-COMPANY TO RH-COMPANY
           MOVE CA-USER TO RH-USER
           CALL "RNDALLOC" USING WS-RNDALLOC-CA
           IF RH-RETURN-CODE > 4
               MOVE "LND0003" TO CA-REASON-CODE
               MOVE RH-MESSAGE TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           MOVE 0 TO WS-LINE-IX
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LC-LINE-CNT
               IF LC-L-ELIGIBLE(WS-I) = "Y"
                   ADD 1 TO WS-LINE-IX
                   MOVE RA-PART(WS-LINE-IX) TO LC-L-SHARE-GBP(WS-I)
               END-IF
           END-PERFORM.
       2000-CHECK.
           MOVE 0 TO WS-SUM-SHARE
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LC-LINE-CNT
               ADD LC-L-SHARE-GBP(WS-I) TO WS-SUM-SHARE
           END-PERFORM
           MOVE WS-SUM-SHARE TO LC-ALLOCATED-GBP
           COMPUTE LC-RESIDUAL-GBP = WS-TARGET-GBP - WS-SUM-SHARE
           PERFORM 2400-SPLIT-CONSUMED THRU 2400-EXIT
           IF LC-RESIDUAL-GBP NOT = 0
               MOVE "LND0008" TO CA-REASON-CODE
               MOVE LC-RESIDUAL-GBP TO WS-AMT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "The allocation differs from the approved "
                      "charge by " FUNCTION TRIM(WS-AMT-ED)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF CA-RETURN-CODE = 0
               MOVE WS-SUM-SHARE TO WS-AMT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Charge of " FUNCTION TRIM(WS-AMT-ED)
                      " allocated by " FUNCTION TRIM(LC-BASIS)
                      " over " DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE LC-LINE-CNT TO WS-REF-ED
               STRING FUNCTION TRIM(CA-MESSAGE) " "
                      FUNCTION TRIM(WS-REF-ED) " receipt lines"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           IF LC-MISSING-BASIS > 0 AND CA-RETURN-CODE = 0
               MOVE 4 TO CA-RETURN-CODE
               MOVE "LND0003" TO CA-REASON-CODE
           END-IF.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> EXPLICIT REVIEWED AMOUNTS ARE TAKEN AS GIVEN; THE CALLER HAS
      *> ALREADY PUT THEM IN THE BASIS COLUMN.
      *>----------------------------------------------------------------
       2200-EXPLICIT SECTION.
       2200-START.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LC-LINE-CNT
               IF LC-L-ELIGIBLE(WS-I) = "Y"
                   MOVE LC-L-BASIS-VAL(WS-I)
                     TO LC-L-SHARE-GBP(WS-I)
               END-IF
           END-PERFORM.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> UNITS ALREADY SOLD OR CONSUMED CANNOT BE REVALUED IN STOCK;
      *> THEIR SHARE FOLLOWS THE COSTING RULES AND GOES TO COST OF
      *> SALES.  THE SPLIT IS BY REMAINING AGAINST TOTAL QUANTITY.
      *>----------------------------------------------------------------
       2400-SPLIT-CONSUMED SECTION.
       2400-START.
           MOVE 0 TO LC-TO-INVENTORY LC-TO-COGS
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LC-LINE-CNT
               IF LC-L-ELIGIBLE(WS-I) = "Y"
                   COMPUTE WS-TOT-QTY = LC-L-QTY-REMAIN(WS-I)
                                      + LC-L-QTY-USED(WS-I)
                   IF WS-TOT-QTY <= 0
                       MOVE LC-L-SHARE-GBP(WS-I)
                         TO LC-L-TO-INV(WS-I)
                       MOVE 0 TO LC-L-TO-COGS(WS-I)
                   ELSE
                       COMPUTE WS-INV-PART ROUNDED =
                               LC-L-SHARE-GBP(WS-I)
                               * LC-L-QTY-REMAIN(WS-I) / WS-TOT-QTY
                       MOVE WS-INV-PART TO LC-L-TO-INV(WS-I)
                       COMPUTE WS-COGS-PART =
                               LC-L-SHARE-GBP(WS-I) - WS-INV-PART
                       MOVE WS-COGS-PART TO LC-L-TO-COGS(WS-I)
                   END-IF
                   ADD LC-L-TO-INV(WS-I) TO LC-TO-INVENTORY
                   ADD LC-L-TO-COGS(WS-I) TO LC-TO-COGS
               END-IF
           END-PERFORM.
       2400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3000-ESTIMATE SECTION.
       3000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "MERGE cst.LandedCost AS t USING (SELECT ? AS c, ? "
                  "AS r) AS s ON t.CompanyCode = s.c AND t.ChargeRef "
                  "= s.r WHEN MATCHED THEN UPDATE SET EstimatedAmount"
                  " = CAST(? AS decimal(18,2)), EstimatedAmountGBP = "
                  "CAST(? AS decimal(18,2)), AllocationBasis = ?, "
                  "Category = ?, ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHEN NOT MATCHED THEN INSERT "
                  "(CompanyCode, ChargeRef, ShipmentRef, SupplierCode,"
                  " ImportEntryRef, Category, IsEligibleToCapitalise, "
                  "AllocationBasis, CurrencyCode, FxRate, "
                  "EstimatedAmount, EstimatedAmountGBP, AllocationDate"
                  ", Status, ApprovedBy, CreatedBy) VALUES (s.c, s.r, "
                  "NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), ?, ?, ?, "
                  "?, CAST(? AS decimal(18,8)), CAST(? AS decimal(18,2"
                  ")), CAST(? AS decimal(18,2)), CAST(? AS date), "
                  "'ESTIMATED', NULLIF(?,''), ?);"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE LC-CHARGE-REF TO SQLIO-PARM-VAL(2)
           MOVE LC-ESTIMATE-AMT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE LC-ESTIMATE-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE LC-BASIS TO SQLIO-PARM-VAL(5)
           MOVE LC-CATEGORY TO SQLIO-PARM-VAL(6)
           MOVE CA-USER TO SQLIO-PARM-VAL(7)
           MOVE LC-SHIPMENT-REF TO SQLIO-PARM-VAL(8)
           MOVE LC-SUPPLIER TO SQLIO-PARM-VAL(9)
           MOVE LC-IMPORT-ENTRY TO SQLIO-PARM-VAL(10)
           MOVE LC-CATEGORY TO SQLIO-PARM-VAL(11)
           IF LC-CAPITALISE
               MOVE "1" TO SQLIO-PARM-VAL(12)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(12)
           END-IF
           MOVE LC-BASIS TO SQLIO-PARM-VAL(13)
           MOVE LC-CURRENCY TO SQLIO-PARM-VAL(14)
           MOVE LC-FX-RATE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(15)
           MOVE LC-ESTIMATE-AMT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(16)
           MOVE LC-ESTIMATE-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(17)
           MOVE LC-ALLOC-DATE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(18)
           MOVE LC-APPROVED-BY TO SQLIO-PARM-VAL(19)
           MOVE CA-USER TO SQLIO-PARM-VAL(20)
           MOVE 20 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 20
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           PERFORM 3200-READ-ID THRU 3200-EXIT
           MOVE "ESTIMATED" TO LC-STATUS.
       3000-EXIT.
           EXIT.

       3200-READ-ID SECTION.
       3200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(LandedCostId AS varchar(16)) FROM "
                  "cst.LandedCost WHERE CompanyCode = ? AND "
                  "ChargeRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE LC-CHARGE-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 16 TO SQLIO-COL-WIDTH(1)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ID-ROW
           IF SQLIO-OK
               COMPUTE WS-NUM = FUNCTION NUMVAL(WS-ID-VALUE)
               MOVE WS-NUM TO LC-LANDED-ID
           END-IF.
       3200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3500-WRITE-ALLOC SECTION.
       3500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE cst.LandedCostAllocation WHERE "
                  "LandedCostId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE LC-LANDED-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > LC-LINE-CNT OR WS-FATAL
               IF LC-L-ELIGIBLE(WS-I) = "Y"
                   PERFORM 3600-ONE-ALLOC THRU 3600-EXIT
               END-IF
           END-PERFORM
           IF NOT WS-FATAL
               MOVE SPACES TO SQLIO-STMT
               STRING "UPDATE cst.LandedCost SET AllocatedGBP = "
                      "CAST(? AS decimal(18,2)), Status = "
                      "'ALLOCATED', AllocationDate = CAST(? AS date),"
                      " ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                      "WHERE LandedCostId = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE LC-ALLOCATED-GBP TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
               MOVE LC-ALLOC-DATE TO SQLIO-INT-EDIT
               MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
               MOVE CA-USER TO SQLIO-PARM-VAL(3)
               MOVE LC-LANDED-ID TO SQLIO-INT-EDIT
               MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
               PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
                   MOVE "N" TO SQLIO-PARM-NULL(WS-J)
               END-PERFORM
               MOVE 4 TO SQLIO-PARM-CNT
               PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
               MOVE "ALLOCATED" TO LC-STATUS
           END-IF.
       3500-EXIT.
           EXIT.

       3600-ONE-ALLOC SECTION.
       3600-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT cst.LandedCostAllocation (LandedCostId, "
                  "LineNum, ReceiptRef, ReceiptLine, ItemCode, "
                  "LotCode, BasisValue, BasisShare, EstimatedShareGBP"
                  ", QtyRemaining, QtyConsumed, ToInventoryGBP, "
                  "ToCogsGBP) VALUES (?, ?, ?, ?, ?, NULLIF(?,''), "
                  "CAST(? AS decimal(19,6)), CAST(? AS decimal(19,9))"
                  ", CAST(? AS decimal(18,2)), CAST(? AS decimal(19,6"
                  ")), CAST(? AS decimal(19,6)), CAST(? AS decimal("
                  "18,2)), CAST(? AS decimal(18,2)))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE LC-LANDED-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE WS-I TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE LC-L-RECEIPT(WS-I) TO SQLIO-PARM-VAL(3)
           MOVE LC-L-RCP-LINE(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE LC-L-ITEM(WS-I) TO SQLIO-PARM-VAL(5)
           MOVE LC-L-LOT(WS-I) TO SQLIO-PARM-VAL(6)
           MOVE LC-L-BASIS-VAL(WS-I) TO WS-QTY-ED
           MOVE FUNCTION TRIM(WS-QTY-ED) TO SQLIO-PARM-VAL(7)
           COMPUTE WS-NUM ROUNDED =
                   LC-L-BASIS-VAL(WS-I) / LC-BASIS-TOTAL
           MOVE WS-NUM TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE LC-L-SHARE-GBP(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE LC-L-QTY-REMAIN(WS-I) TO WS-QTY-ED
           MOVE FUNCTION TRIM(WS-QTY-ED) TO SQLIO-PARM-VAL(10)
           MOVE LC-L-QTY-USED(WS-I) TO WS-QTY-ED
           MOVE FUNCTION TRIM(WS-QTY-ED) TO SQLIO-PARM-VAL(11)
           MOVE LC-L-TO-INV(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE LC-L-TO-COGS(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 13
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 13 TO SQLIO-PARM-CNT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       3600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> ACTUAL TRUE UP (PO-016).  ONLY ACTUAL MINUS ESTIMATE MOVES, SO
      *> AN ACTUAL FREIGHT INVOICE CANNOT CAPITALISE BOTH THE FULL
      *> ESTIMATE AND THE FULL ACTUAL COST.
      *>----------------------------------------------------------------
       4000-ACTUAL SECTION.
       4000-START.
           COMPUTE LC-DELTA-GBP = LC-ACTUAL-GBP - LC-ESTIMATE-GBP
           PERFORM 2000-ALLOCATE THRU 2000-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE cst.LandedCost SET ActualAmount = CAST(? AS"
                  " decimal(18,2)), ActualAmountGBP = CAST(? AS "
                  "decimal(18,2)), SupplierInvoiceRef = NULLIF(?,''),"
                  " Status = 'ACTUALISED', AllocatedGBP = "
                  "AllocatedGBP + CAST(? AS decimal(18,2)), "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() WHERE "
                  "CompanyCode = ? AND ChargeRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE LC-ACTUAL-AMT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE LC-ACTUAL-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE LC-SUPPLIER-INV TO SQLIO-PARM-VAL(3)
           MOVE LC-DELTA-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE CA-USER TO SQLIO-PARM-VAL(5)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(6)
           MOVE LC-CHARGE-REF TO SQLIO-PARM-VAL(7)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 7
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 7 TO SQLIO-PARM-CNT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF NOT WS-FATAL
               MOVE "ACTUALISED" TO LC-STATUS
               PERFORM 6000-POST THRU 6000-EXIT
               MOVE LC-DELTA-GBP TO WS-AMT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Actual charge reconciled; only the difference "
                      "of " FUNCTION TRIM(WS-AMT-ED)
                      " has been allocated"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       5000-ENQUIRE SECTION.
       5000-START.
           PERFORM 3200-READ-ID THRU 3200-EXIT
           IF LC-LANDED-ID = 0
               MOVE 16 TO CA-RETURN-CODE
               MOVE "LND0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Charge " FUNCTION TRIM(LC-CHARGE-REF)
                      " is not known"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE ESTIMATE IS ACCRUED (DR STOCK, CR LANDED COST ABSORBED);
      *> THE ACTUAL TRUE UP MOVES ONLY THE DELTA.  RECOVERABLE VAT ON
      *> THE CARRIER INVOICE IS NOT PART OF THIS JOURNAL.
      *>----------------------------------------------------------------
       6000-POST SECTION.
       6000-START.
           IF LC-POST-JOURNAL NOT = "Y"
               GO TO 6000-EXIT
           END-IF
           IF LC-TO-INVENTORY = 0 AND LC-TO-COGS = 0
               GO TO 6000-EXIT
           END-IF
           INITIALIZE WS-POPOST
           MOVE "POST" TO PP-FUNC
           MOVE CA-COMPANY TO PP-COMPANY
           MOVE CA-SITE TO PP-SITE
           MOVE CA-USER TO PP-USER
           MOVE CA-BUSINESS-DATE TO PP-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO PP-CORRELATION
           MOVE LC-ALLOC-DATE TO PP-POSTING-DATE
           MOVE LC-ALLOC-DATE TO PP-DOCUMENT-DATE
           MOVE "GBP" TO PP-CURRENCY
           MOVE 1 TO PP-FX-RATE
           MOVE LC-CHARGE-REF TO PP-SOURCE-REF
           MOVE "PO" TO PP-SUBLEDGER-TYPE
           MOVE LC-CHARGE-REF TO PP-SUBLEDGER-REF
           IF CA-FUNCTION = "ACTUAL"
               MOVE PO-EV-LANDED-ACT TO PP-EVENT-CODE
           ELSE
               MOVE PO-EV-LANDED-EST TO PP-EVENT-CODE
           END-IF
           MOVE SPACES TO PP-DESCRIPTION
           STRING "Landed " FUNCTION TRIM(LC-CATEGORY)
                  " charge " FUNCTION TRIM(LC-CHARGE-REF)
               DELIMITED BY SIZE INTO PP-DESCRIPTION
           MOVE 0 TO WS-N
           IF LC-TO-INVENTORY NOT = 0
               ADD 1 TO WS-N
               MOVE PO-ACC-STOCK TO PP-L-ACCOUNT(WS-N)
               MOVE LC-TO-INVENTORY TO PP-L-DR(WS-N)
               MOVE 0 TO PP-L-CR(WS-N)
               MOVE "Landed cost into stock" TO PP-L-NARRATIVE(WS-N)
           END-IF
           IF LC-TO-COGS NOT = 0
               ADD 1 TO WS-N
               MOVE PO-ACC-SCRAP TO PP-L-ACCOUNT(WS-N)
               MOVE LC-TO-COGS TO PP-L-DR(WS-N)
               MOVE 0 TO PP-L-CR(WS-N)
               MOVE "Landed cost on units already consumed"
                 TO PP-L-NARRATIVE(WS-N)
           END-IF
           ADD 1 TO WS-N
           MOVE PO-ACC-LANDED TO PP-L-ACCOUNT(WS-N)
           MOVE 0 TO PP-L-DR(WS-N)
           COMPUTE PP-L-CR(WS-N) = LC-TO-INVENTORY + LC-TO-COGS
           MOVE "PURCH" TO PP-L-COST-CTR(WS-N)
           MOVE "Landed cost absorbed" TO PP-L-NARRATIVE(WS-N)
           MOVE WS-N TO PP-LINE-CNT
           CALL "POPOST" USING WS-POPOST
           MOVE PP-JOURNAL-NUMBER TO LC-JOURNAL-NUMBER
           IF PP-RC > 4
               MOVE PP-RC TO CA-RETURN-CODE
               MOVE PP-REASON TO CA-REASON-CODE
               MOVE PP-MESSAGE TO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
           END-IF.
       6000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8500-EXEC-UPDATE SECTION.
       8500-START.
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               MOVE "Y" TO WS-FATAL-SW
               MOVE 12 TO CA-RETURN-CODE
               MOVE "SQL0001" TO CA-REASON-CODE
               MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           END-IF
           MOVE 4 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       8500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE 8 TO CA-RETURN-CODE
           MOVE "Y" TO WS-FATAL-SW.
       9000-EXIT.
           EXIT.
