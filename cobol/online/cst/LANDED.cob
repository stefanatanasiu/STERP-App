      *>================================================================
      *> PROGRAM-ID : LANDED
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *> PURPOSE    : ATTRIBUTE AN ACQUISITION CHARGE - FREIGHT, DUTY,
      *>              INSURANCE, CLEARANCE - TO THE RECEIPT LINES THAT
      *>              CARRY IT, EXACTLY ONCE.  WHEN THE SUPPLIER'S
      *>              ACTUAL CHARGE ARRIVES ONLY THE DIFFERENCE AGAINST
      *>              THE ESTIMATE IS SPREAD, AND THAT DIFFERENCE
      *>              FOLLOWS WHERE THE GOODS NOW SIT - STOCK, WIP OR
      *>              ALREADY SOLD.  NOTHING IS ATTRIBUTED TWICE AND
      *>              NOTHING IS LOST IN THE ROUNDING.
      *> CALLED BY  : PURCHASING (CHARGE REGISTRATION), THE COSTING
      *>              CLIENT, THE INVENTORY VALUATION REPORT
      *> INTERFACE  : CALL "LANDED" USING WS-LANDED-CA
      *>              COPYBOOK LANDEDCA
      *> CALLS      : RNDALLOC (ALLOC), SQLIO (cst.LandedCost,
      *>              cst.LandedCostAllocation)
      *> NOTE       : THE STOCK SHARE IS HANDED TO THE VALUATION POOL
      *>              BY FIFOVAL "LANDADJ" OR MWAVAL "REVALUE".  THIS
      *>              PROGRAM DECIDES THE SPLIT, IT DOES NOT REVALUE.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1999-05-20 RJH  CR0756 ORIGINAL FREIGHT ABSORPTION
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT SHIPMENT DATES
      *> 2021-01-01 DJM  CR2190 BREXIT - CUSTOMS DUTY AND CLEARANCE
      *> 2026-10-06 STERP CST-008 DIFFERENCE ONLY ON ACTUALISATION
      *>================================================================
      *> REQ: CST-004, CST-008   AC: AC-C4
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LANDED.
       AUTHOR. R J HARTLEY.
       DATE-WRITTEN. 20/05/1999.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY CSTWORK.
       COPY RNDALLOCCA REPLACING ==DFHCOMMAREA== BY ==WS-RND-CA==.
       01  WS-FLAGS.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-OWNS-TXN             PIC X VALUE "N".
               88  WS-OWNS-IT                    VALUE "Y".
           05  WS-WRITE                PIC X VALUE "N".
               88  WS-WRITING                    VALUE "Y".
           05  WS-HDR-FOUND            PIC X VALUE "N".
               88  WS-GOT-HEADER                 VALUE "Y".
           05  WS-TARGET               PIC 9 VALUE 1.
               88  WS-ON-ESTIMATE                VALUE 1.
               88  WS-ON-DELTA                   VALUE 2.
       01  WS-AMOUNTS.
           05  WS-ALLOC-AMT            PIC S9(13)V9(2) VALUE 0.
           05  WS-SPLIT-AMT            PIC S9(13)V9(2) VALUE 0.
           05  WS-CHECK                PIC S9(13)V9(2) VALUE 0.
           05  WS-QTY-ACCT             PIC S9(13)V9(6) VALUE 0.
           05  WS-LINE-IX              PIC S9(4) VALUE 0.
           05  WS-NEW-STATUS           PIC X(12) VALUE SPACES.
       LINKAGE SECTION.
       COPY LANDEDCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0300-READ-CHARGE
           IF WS-INPUT-BAD OR WS-SQL-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 7100-BEGIN-TXN
           EVALUATE CA-FUNCTION OF DFHCOMMAREA
               WHEN "ESTIMATE" PERFORM 1000-ESTIMATE
               WHEN "ACTUAL"   PERFORM 2000-ACTUAL
               WHEN "SPLIT"    PERFORM 3000-SPLIT-ALL
               WHEN "ENQUIRE"  PERFORM 4000-ENQUIRE
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE
           IF WS-INPUT-BAD OR WS-SQL-BAD
               PERFORM 7300-ROLLBACK
           ELSE
               PERFORM 7200-COMMIT-TXN
           END-IF.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-SQL-FAILED WS-HDR-FOUND
           MOVE "LANDED" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "CST-008" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE SPACES TO LD-WARNING
           MOVE 0 TO LD-ALLOCATED-GBP LD-TO-INVENTORY LD-TO-WIP
           MOVE 0 TO LD-TO-COGS LD-TO-VARIANCE LD-BASIS-TOTAL
           MOVE 0 TO LD-DELTA-GBP WS-CHECK
           MOVE FUNCTION UPPER-CASE(LD-BASIS) TO LD-BASIS
           IF LD-FX-RATE = 0
               MOVE 1 TO LD-FX-RATE
           END-IF
           IF LD-COMMIT = SPACE OR LOW-VALUE
               MOVE "N" TO LD-COMMIT
           END-IF
           MOVE FUNCTION UPPER-CASE(LD-COMMIT) TO LD-COMMIT
           EVALUATE LD-COMMIT
               WHEN "Y" MOVE "Y" TO WS-OWNS-TXN WS-WRITE
               WHEN "T" MOVE "N" TO WS-OWNS-TXN
                        MOVE "Y" TO WS-WRITE
               WHEN OTHER MOVE "N" TO WS-OWNS-TXN WS-WRITE
           END-EVALUATE
           IF CA-FUNCTION OF DFHCOMMAREA = "ENQUIRE"
               MOVE "N" TO WS-WRITE WS-OWNS-TXN
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 0200  AN ATTRIBUTION NEEDS A COMPANY, A CHARGE AND A BASIS THE
      *>       SYSTEM UNDERSTANDS.  A BASIS NOBODY RECOGNISES IS NOT
      *>       QUIETLY REPLACED WITH ONE WE DO - CST-001.
      *>----------------------------------------------------------------
       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY OF DFHCOMMAREA = SPACES
              OR LD-CHARGE-REF = SPACES
               MOVE "CST1301" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A company and a charge reference are required"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF LD-LINE-CNT > 25
               MOVE "CST1307" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "More receipt lines than this window can carry"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION OF DFHCOMMAREA = "ESTIMATE"
              OR CA-FUNCTION OF DFHCOMMAREA = "ACTUAL"
               IF NOT (LD-ON-VALUE OR LD-ON-WEIGHT OR LD-ON-VOLUME
                       OR LD-ON-QUANTITY)
                   MOVE "CST1303" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
                   STRING "Allocation basis "
                          FUNCTION TRIM(LD-BASIS)
                          " is not recognised"
                       DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
                   PERFORM 9000-REJECT
               END-IF
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 0300  THE REGISTERED CHARGE.  A CALLER MAY ALSO ASK FOR A PURE
      *>       SIMULATION (COMMIT "N" WITH THE LINES SUPPLIED), WHICH IS
      *>       WHAT THE VALUATION REPORT AND THE UNIT TESTS USE.
      *>----------------------------------------------------------------
       0300-READ-CHARGE SECTION.
       0300-START.
           IF NOT WS-WRITING AND LD-LINE-CNT > 0
              AND CA-FUNCTION OF DFHCOMMAREA NOT = "ENQUIRE"
      *>       a pure simulation on lines the caller supplied - the
      *>       register is not read and cannot be touched
               IF LD-STATUS = SPACES
                   MOVE "SIMULATED" TO LD-STATUS
               END-IF
               IF LD-ELIGIBLE = SPACE OR LOW-VALUE
                   MOVE "Y" TO LD-ELIGIBLE
               END-IF
               GO TO 0300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT LandedCostId, Category, AllocationBasis, "
                  "FxRate, EstimatedAmount, EstimatedAmountGBP, "
                  "ISNULL(ActualAmountGBP,0), AllocatedGBP, Status, "
                  "CASE WHEN IsEligibleToCapitalise = 1 THEN 'Y' "
                  "ELSE 'N' END FROM cst.LandedCost WHERE "
                  "CompanyCode = ? AND ChargeRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE LD-CHARGE-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 10 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-SQL-BAD
               GO TO 0300-EXIT
           END-IF
           IF WS-GOT-ROW
               MOVE "Y" TO WS-HDR-FOUND
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO LD-LANDED-ID
               IF LD-CATEGORY = SPACES
                   MOVE WS-COL(2) TO LD-CATEGORY
               END-IF
               IF LD-BASIS = SPACES
                   MOVE WS-COL(3) TO LD-BASIS
               END-IF
               IF LD-ESTIMATED = 0
                   MOVE FUNCTION NUMVAL(WS-COL(5)) TO LD-ESTIMATED
               END-IF
               IF LD-ESTIMATED-GBP = 0
                   MOVE FUNCTION NUMVAL(WS-COL(6)) TO LD-ESTIMATED-GBP
               END-IF
               MOVE WS-COL(9) TO LD-STATUS
               MOVE WS-COL(10) TO LD-ELIGIBLE
           ELSE
               IF WS-WRITING
                   MOVE "CST1302" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
                   STRING "Charge " FUNCTION TRIM(LD-CHARGE-REF)
                          " is not registered"
                       DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
                   PERFORM 9000-REJECT
                   GO TO 0300-EXIT
               END-IF
               MOVE "SIMULATED" TO LD-STATUS
               IF LD-ELIGIBLE = SPACE OR LOW-VALUE
                   MOVE "Y" TO LD-ELIGIBLE
               END-IF
           END-IF
           IF (CA-FUNCTION OF DFHCOMMAREA = "ESTIMATE"
               OR CA-FUNCTION OF DFHCOMMAREA = "ACTUAL")
              AND LD-ELIGIBLE = "N"
               MOVE "CST1305" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "This charge is not eligible to be capitalised"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
           END-IF.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  CST-008 ESTIMATE.  THE PROVISIONAL CHARGE IS SPREAD ONCE
      *>       OVER THE RECEIPT LINES ON THE STATED BASIS.  SPEC C4:
      *>       1,000.00 OF GOODS AND 80.00 OF FREIGHT OVER 100 UNITS
      *>       GIVES A PROVISIONAL UNIT COST OF 10.80.
      *>----------------------------------------------------------------
       1000-ESTIMATE SECTION.
       1000-START.
           IF LD-ESTIMATED-GBP = 0 AND LD-ESTIMATED NOT = 0
               COMPUTE LD-ESTIMATED-GBP ROUNDED =
                   LD-ESTIMATED * LD-FX-RATE
           END-IF
           PERFORM 0400-LOAD-LINES
           IF WS-INPUT-BAD OR WS-SQL-BAD
               GO TO 1000-EXIT
           END-IF
           PERFORM 1100-BASIS-TOTAL
           IF WS-INPUT-BAD
               GO TO 1000-EXIT
           END-IF
           MOVE LD-ESTIMATED-GBP TO WS-ALLOC-AMT
           MOVE 1 TO WS-TARGET
           PERFORM 1200-ALLOCATE
           PERFORM 3000-SPLIT-ALL
           PERFORM 1300-UNIT-COST
           MOVE "ALLOCATED" TO WS-NEW-STATUS
           IF WS-WRITING
               PERFORM 7000-WRITE-ALLOCATIONS
               PERFORM 7500-UPDATE-HEADER
           ELSE
               MOVE "SIMULATED" TO LD-STATUS
           END-IF.
       1000-EXIT.
           EXIT.

      *>   the basis a charge is spread on.  a zero basis total is
      *>   reported, never divided by - CST1304.
       1100-BASIS-TOTAL SECTION.
       1100-START.
           MOVE 0 TO LD-BASIS-TOTAL
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > LD-LINE-CNT
               IF LD-ON-QUANTITY AND LD-L-BASIS-VAL(WS-IX) = 0
                   MOVE LD-L-QTY-RECVD(WS-IX)
                       TO LD-L-BASIS-VAL(WS-IX)
               END-IF
               IF LD-L-BASIS-VAL(WS-IX) < 0
                   MOVE 0 TO LD-L-BASIS-VAL(WS-IX)
               END-IF
               ADD LD-L-BASIS-VAL(WS-IX) TO LD-BASIS-TOTAL
           END-PERFORM
           IF LD-BASIS-TOTAL = 0
               MOVE "CST1304" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "The " FUNCTION TRIM(LD-BASIS)
                      " basis totals zero - nothing to allocate over"
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
           END-IF.
       1100-EXIT.
           EXIT.

      *>   RNDALLOC owns the rounding residual so the shares always add
      *>   back to the charge (D-22).  WS-TARGET says which bucket the
      *>   parts belong in - the estimate or the difference.
       1200-ALLOCATE SECTION.
       1200-START.
           INITIALIZE WS-RND-CA
           MOVE "ALLOC" TO CA-FUNCTION OF WS-RND-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-RND-CA
           MOVE "WEIGHT" TO RA-MODE
           MOVE 2 TO RA-PLACES
           MOVE WS-ALLOC-AMT TO RA-AMOUNT
           MOVE LD-LINE-CNT TO RA-LINE-CNT
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > LD-LINE-CNT
               MOVE LD-L-BASIS-VAL(WS-IX) TO RA-WEIGHT(WS-IX)
           END-PERFORM
           CALL "RNDALLOC" USING WS-RND-CA
           IF CA-RETURN-CODE OF WS-RND-CA > 4
               MOVE "CST1304" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-RND-CA TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 1200-EXIT
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > LD-LINE-CNT
               IF WS-ON-ESTIMATE
                   MOVE RA-PART(WS-IX) TO LD-L-EST-SHARE(WS-IX)
                   MOVE 0 TO LD-L-DELTA(WS-IX)
                   MOVE LD-L-EST-SHARE(WS-IX)
                       TO LD-L-ACT-SHARE(WS-IX)
               ELSE
                   MOVE RA-PART(WS-IX) TO LD-L-DELTA(WS-IX)
                   COMPUTE LD-L-ACT-SHARE(WS-IX) =
                       LD-L-EST-SHARE(WS-IX) + LD-L-DELTA(WS-IX)
               END-IF
           END-PERFORM
           MOVE RA-ALLOCATED TO LD-ALLOCATED-GBP.
       1200-EXIT.
           EXIT.

      *>   the landed unit cost the line now carries.  on a VALUE basis
      *>   the basis value is the goods value, so the unit cost is the
      *>   full acquisition cost per unit (spec C4 - 10.80).
       1300-UNIT-COST SECTION.
       1300-START.
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > LD-LINE-CNT
               IF LD-L-QTY-RECVD(WS-IX) NOT = 0
                   IF LD-ON-VALUE
                       COMPUTE LD-L-UNIT-COST(WS-IX) ROUNDED =
                           (LD-L-BASIS-VAL(WS-IX)
                            + LD-L-ACT-SHARE(WS-IX))
                           / LD-L-QTY-RECVD(WS-IX)
                   ELSE
                       COMPUTE LD-L-UNIT-COST(WS-IX) ROUNDED =
                           LD-L-ACT-SHARE(WS-IX)
                           / LD-L-QTY-RECVD(WS-IX)
                   END-IF
               ELSE
                   MOVE 0 TO LD-L-UNIT-COST(WS-IX)
               END-IF
           END-PERFORM.
       1300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  CST-008 ACTUAL.  ONLY THE DIFFERENCE IS SPREAD - THE
      *>       ESTIMATE HAS ALREADY BEEN ATTRIBUTED AND IS NOT
      *>       ATTRIBUTED AGAIN.  SPEC C4: ACTUAL 100.00 AGAINST AN
      *>       ESTIMATE OF 80.00 SPREADS 20.00, OF WHICH 8.00 FOLLOWS
      *>       THE 40 UNITS ALREADY SOLD AND 12.00 STAYS IN STOCK.
      *>----------------------------------------------------------------
       2000-ACTUAL SECTION.
       2000-START.
           IF LD-STATUS = "ACTUALISED"
               MOVE "CST1306" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "This charge has already been actualised"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           IF LD-ACTUAL-GBP = 0 AND LD-ACTUAL NOT = 0
               COMPUTE LD-ACTUAL-GBP ROUNDED =
                   LD-ACTUAL * LD-FX-RATE
           END-IF
           IF LD-ESTIMATED-GBP = 0 AND LD-ESTIMATED NOT = 0
               COMPUTE LD-ESTIMATED-GBP ROUNDED =
                   LD-ESTIMATED * LD-FX-RATE
           END-IF
           COMPUTE LD-DELTA-GBP = LD-ACTUAL-GBP - LD-ESTIMATED-GBP
           PERFORM 0400-LOAD-LINES
           IF WS-INPUT-BAD OR WS-SQL-BAD
               GO TO 2000-EXIT
           END-IF
           PERFORM 1100-BASIS-TOTAL
           IF WS-INPUT-BAD
               GO TO 2000-EXIT
           END-IF
           MOVE LD-DELTA-GBP TO WS-ALLOC-AMT
           MOVE 2 TO WS-TARGET
           PERFORM 1200-ALLOCATE
           IF WS-INPUT-BAD
               GO TO 2000-EXIT
           END-IF
           PERFORM 3000-SPLIT-ALL
           PERFORM 1300-UNIT-COST
           MOVE LD-ACTUAL-GBP TO LD-ALLOCATED-GBP
           MOVE "ACTUALISED" TO WS-NEW-STATUS
           IF LD-DELTA-GBP = 0
               MOVE "Actual equals the estimate - no adjustment"
                   TO LD-WARNING
           END-IF
           IF WS-WRITING
               PERFORM 7000-WRITE-ALLOCATIONS
               PERFORM 7500-UPDATE-HEADER
           ELSE
               MOVE "SIMULATED" TO LD-STATUS
           END-IF.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 0400  THE LINES THAT CARRY THE CHARGE.  THE CALLER MAY SUPPLY
      *>       THEM, OR WE READ BACK THE ONES THE ESTIMATE USED SO THE
      *>       DIFFERENCE FOLLOWS EXACTLY THE SAME GOODS.
      *>----------------------------------------------------------------
       0400-LOAD-LINES SECTION.
       0400-START.
           IF LD-LINE-CNT > 0
               GO TO 0400-EXIT
           END-IF
           IF NOT WS-GOT-HEADER
               MOVE "CST1302" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "No receipt lines supplied and no charge on file"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 0400-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT LineNum, ReceiptRef, ISNULL(ReceiptLine,0), "
                  "ItemCode, BasisValue, EstimatedShareGBP, "
                  "ISNULL(QtyRemaining,0), ISNULL(QtyConsumed,0) "
                  "FROM cst.LandedCostAllocation WHERE LandedCostId "
                  "= ? ORDER BY LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE LD-LANDED-ID TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 8 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           IF SQLIO-SQLCODE < 0
               PERFORM 8900-SQL-ERROR
               GO TO 0400-EXIT
           END-IF
           MOVE 0 TO LD-LINE-CNT
           PERFORM UNTIL LD-LINE-CNT >= 25 OR WS-SQL-BAD
               MOVE 2 TO SQLIO-CURSOR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
               IF SQLIO-SQLCODE < 0
                   PERFORM 8900-SQL-ERROR
                   EXIT PERFORM
               END-IF
               IF SQLIO-SQLCODE = 100
                   EXIT PERFORM
               END-IF
               ADD 1 TO LD-LINE-CNT
               MOVE LD-LINE-CNT TO WS-LINE-IX
               MOVE FUNCTION NUMVAL(WS-COL(1))
                   TO LD-L-NUM(WS-LINE-IX)
               MOVE WS-COL(2) TO LD-L-RECEIPT(WS-LINE-IX)
               MOVE FUNCTION NUMVAL(WS-COL(3))
                   TO LD-L-RCPT-LINE(WS-LINE-IX)
               MOVE WS-COL(4) TO LD-L-ITEM(WS-LINE-IX)
               MOVE FUNCTION NUMVAL(WS-COL(5))
                   TO LD-L-BASIS-VAL(WS-LINE-IX)
               MOVE FUNCTION NUMVAL(WS-COL(6))
                   TO LD-L-EST-SHARE(WS-LINE-IX)
               MOVE FUNCTION NUMVAL(WS-COL(7))
                   TO LD-L-QTY-LEFT(WS-LINE-IX)
               MOVE FUNCTION NUMVAL(WS-COL(8))
                   TO LD-L-QTY-SOLD(WS-LINE-IX)
               COMPUTE LD-L-QTY-RECVD(WS-LINE-IX) =
                   LD-L-QTY-LEFT(WS-LINE-IX)
                   + LD-L-QTY-SOLD(WS-LINE-IX)
                   + LD-L-QTY-WIP(WS-LINE-IX)
           END-PERFORM
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW.
       0400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  CST-008.  EACH LINE'S ADJUSTMENT FOLLOWS THE GOODS.  THE
      *>       SPLIT IS OVER THE QUANTITIES STILL IN STOCK, DRAWN INTO
      *>       WIP AND ALREADY SOLD, SO STOCK, WIP AND COGS TOGETHER
      *>       RECEIVE EXACTLY THE ADJUSTMENT AND NOTHING OVERLAPS.
      *>----------------------------------------------------------------
       3000-SPLIT-ALL SECTION.
       3000-START.
           MOVE 0 TO LD-TO-INVENTORY LD-TO-WIP LD-TO-COGS
           MOVE 0 TO LD-TO-VARIANCE WS-CHECK
           PERFORM VARYING WS-LINE-IX FROM 1 BY 1
                   UNTIL WS-LINE-IX > LD-LINE-CNT
               PERFORM 3100-SPLIT-LINE
               ADD LD-L-TO-STOCK(WS-LINE-IX) TO LD-TO-INVENTORY
               ADD LD-L-TO-WIP(WS-LINE-IX) TO LD-TO-WIP
               ADD LD-L-TO-COGS(WS-LINE-IX) TO LD-TO-COGS
           END-PERFORM
           COMPUTE WS-CHECK = LD-TO-INVENTORY + LD-TO-WIP
               + LD-TO-COGS + LD-TO-VARIANCE
           IF WS-CHECK NOT = WS-ALLOC-AMT
              AND CA-FUNCTION OF DFHCOMMAREA NOT = "SPLIT"
               MOVE SPACES TO LD-WARNING
               MOVE WS-CHECK TO WS-MONEY-ED
               STRING "Destination split "
                      FUNCTION TRIM(WS-MONEY-ED)
                      " does not equal the amount attributed"
                   DELIMITED BY SIZE INTO LD-WARNING
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
           END-IF.
       3000-EXIT.
           EXIT.

       3100-SPLIT-LINE SECTION.
       3100-START.
           MOVE 0 TO LD-L-TO-STOCK(WS-LINE-IX)
           MOVE 0 TO LD-L-TO-WIP(WS-LINE-IX)
           MOVE 0 TO LD-L-TO-COGS(WS-LINE-IX)
           IF WS-ON-ESTIMATE
               MOVE LD-L-EST-SHARE(WS-LINE-IX) TO WS-SPLIT-AMT
           ELSE
               MOVE LD-L-DELTA(WS-LINE-IX) TO WS-SPLIT-AMT
           END-IF
           IF WS-SPLIT-AMT = 0
               GO TO 3100-EXIT
           END-IF
           COMPUTE WS-QTY-ACCT = LD-L-QTY-LEFT(WS-LINE-IX)
               + LD-L-QTY-WIP(WS-LINE-IX)
               + LD-L-QTY-SOLD(WS-LINE-IX)
           IF WS-QTY-ACCT = 0
      *>       the goods are gone and nobody can say where - the
      *>       adjustment is disclosed as a variance, not buried
               ADD WS-SPLIT-AMT TO LD-TO-VARIANCE
               MOVE "Receipt quantity unaccounted - taken to variance"
                   TO LD-WARNING
               GO TO 3100-EXIT
           END-IF
           INITIALIZE WS-RND-CA
           MOVE "ALLOC" TO CA-FUNCTION OF WS-RND-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-RND-CA
           MOVE "WEIGHT" TO RA-MODE
           MOVE 2 TO RA-PLACES
           MOVE WS-SPLIT-AMT TO RA-AMOUNT
           MOVE 3 TO RA-LINE-CNT
           MOVE LD-L-QTY-LEFT(WS-LINE-IX) TO RA-WEIGHT(1)
           MOVE LD-L-QTY-WIP(WS-LINE-IX) TO RA-WEIGHT(2)
           MOVE LD-L-QTY-SOLD(WS-LINE-IX) TO RA-WEIGHT(3)
           CALL "RNDALLOC" USING WS-RND-CA
           IF CA-RETURN-CODE OF WS-RND-CA > 4
               ADD WS-SPLIT-AMT TO LD-TO-VARIANCE
               GO TO 3100-EXIT
           END-IF
           MOVE RA-PART(1) TO LD-L-TO-STOCK(WS-LINE-IX)
           MOVE RA-PART(2) TO LD-L-TO-WIP(WS-LINE-IX)
           MOVE RA-PART(3) TO LD-L-TO-COGS(WS-LINE-IX).
       3100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  RECONCILIATION.  WHAT WAS CHARGED, WHAT WAS ATTRIBUTED
      *>       AND WHERE IT WENT - IN ONE PLACE, SO CST-022 CAN TIE THE
      *>       VALUATION BACK TO THE LEDGER.
      *>----------------------------------------------------------------
       4000-ENQUIRE SECTION.
       4000-START.
           IF NOT WS-GOT-HEADER
               MOVE "CST1302" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "Charge not found" TO CA-MESSAGE OF DFHCOMMAREA
               MOVE 16 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "Y" TO WS-ERROR
               GO TO 4000-EXIT
           END-IF
           MOVE 0 TO LD-LINE-CNT
           PERFORM 0400-LOAD-LINES
           IF WS-SQL-BAD
               GO TO 4000-EXIT
           END-IF
           MOVE 0 TO LD-BASIS-TOTAL LD-ALLOCATED-GBP
           MOVE 0 TO LD-TO-INVENTORY LD-TO-WIP LD-TO-COGS
           PERFORM VARYING WS-IX FROM 1 BY 1
                   UNTIL WS-IX > LD-LINE-CNT
               ADD LD-L-BASIS-VAL(WS-IX) TO LD-BASIS-TOTAL
               ADD LD-L-EST-SHARE(WS-IX) TO LD-ALLOCATED-GBP
               ADD LD-L-DELTA(WS-IX) TO LD-ALLOCATED-GBP
               ADD LD-L-TO-STOCK(WS-IX) TO LD-TO-INVENTORY
               ADD LD-L-TO-WIP(WS-IX) TO LD-TO-WIP
               ADD LD-L-TO-COGS(WS-IX) TO LD-TO-COGS
           END-PERFORM
           IF LD-STATUS = "ACTUALISED"
               MOVE LD-ACTUAL-GBP TO WS-ALLOC-AMT
           ELSE
               MOVE LD-ESTIMATED-GBP TO WS-ALLOC-AMT
           END-IF
           IF LD-ALLOCATED-GBP NOT = WS-ALLOC-AMT
               MOVE SPACES TO LD-WARNING
               STRING "Attributed value does not agree with the "
                      "charge - review before the valuation run"
                   DELIMITED BY SIZE INTO LD-WARNING
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
           END-IF.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 7000  PERSISTENCE.  THE ALLOCATION SET IS REPLACED WHOLE SO A
      *>       RERUN CANNOT LEAVE TWO GENERATIONS OF SHARES BEHIND.
      *>----------------------------------------------------------------
       7000-WRITE-ALLOCATIONS SECTION.
       7000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM cst.LandedCostAllocation WHERE "
                  "LandedCostId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE LD-LANDED-ID TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           PERFORM 8100-EXECUTE
           PERFORM VARYING WS-LINE-IX FROM 1 BY 1
                   UNTIL WS-LINE-IX > LD-LINE-CNT OR WS-SQL-BAD
               PERFORM 7050-WRITE-ONE
           END-PERFORM.
       7000-EXIT.
           EXIT.

       7050-WRITE-ONE SECTION.
       7050-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO cst.LandedCostAllocation "
                  "(LandedCostId, LineNum, ReceiptRef, ReceiptLine, "
                  "ItemCode, BasisValue, BasisShare, "
                  "EstimatedShareGBP, ActualShareGBP, DeltaGBP, "
                  "QtyRemaining, QtyConsumed, ToInventoryGBP, "
                  "ToCogsGBP, ToWipGBP, ToVarianceGBP, PostedAt) "
                  "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, "
                  "?, ?, 0, SYSDATETIME())"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 15 TO SQLIO-PARM-CNT
           MOVE LD-LANDED-ID TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE WS-LINE-IX TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE LD-L-RECEIPT(WS-LINE-IX) TO SQLIO-PARM-VAL(3)
           MOVE LD-L-RCPT-LINE(WS-LINE-IX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE LD-L-ITEM(WS-LINE-IX) TO SQLIO-PARM-VAL(5)
           MOVE LD-L-BASIS-VAL(WS-LINE-IX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           IF LD-BASIS-TOTAL NOT = 0
               COMPUTE WS-CALC-1 ROUNDED =
                   LD-L-BASIS-VAL(WS-LINE-IX) / LD-BASIS-TOTAL
           ELSE
               MOVE 0 TO WS-CALC-1
           END-IF
           MOVE WS-CALC-1 TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE LD-L-EST-SHARE(WS-LINE-IX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE LD-L-ACT-SHARE(WS-LINE-IX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE LD-L-DELTA(WS-LINE-IX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE LD-L-QTY-LEFT(WS-LINE-IX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE LD-L-QTY-SOLD(WS-LINE-IX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE LD-L-TO-STOCK(WS-LINE-IX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           MOVE LD-L-TO-COGS(WS-LINE-IX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(14)
           MOVE LD-L-TO-WIP(WS-LINE-IX) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(15)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 15
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           PERFORM 8100-EXECUTE.
       7050-EXIT.
           EXIT.

       7500-UPDATE-HEADER SECTION.
       7500-START.
           IF WS-SQL-BAD
               GO TO 7500-EXIT
           END-IF
           IF LD-ALLOCATION-DATE = 0
               MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE-TEXT
               MOVE WS-DATE-TEXT(1:8) TO LD-ALLOCATION-DATE
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE cst.LandedCost SET AllocatedGBP = ?, "
                  "Status = ?, AllocationDate = CONVERT(date, ?, "
                  "112), EstimatedAmountGBP = ?, ActualAmount = ?, "
                  "ActualAmountGBP = ?, ModifiedBy = ?, ModifiedAt "
                  "= SYSDATETIME() WHERE CompanyCode = ? AND "
                  "ChargeRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 9 TO SQLIO-PARM-CNT
           MOVE LD-ALLOCATED-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE WS-NEW-STATUS TO SQLIO-PARM-VAL(2)
           MOVE LD-ALLOCATION-DATE TO WS-DATE-TEXT
           MOVE WS-DATE-TEXT TO SQLIO-PARM-VAL(3)
           MOVE LD-ESTIMATED-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE LD-ACTUAL TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE LD-ACTUAL-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE CA-USER OF DFHCOMMAREA TO SQLIO-PARM-VAL(7)
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(8)
           MOVE LD-CHARGE-REF TO SQLIO-PARM-VAL(9)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 9
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           IF CA-FUNCTION OF DFHCOMMAREA = "ESTIMATE"
               MOVE "Y" TO SQLIO-PARM-NULL(5)
               MOVE "Y" TO SQLIO-PARM-NULL(6)
           END-IF
           PERFORM 8100-EXECUTE
           IF NOT WS-SQL-BAD
               MOVE WS-NEW-STATUS TO LD-STATUS
           END-IF.
       7500-EXIT.
           EXIT.

       7100-BEGIN-TXN SECTION.
       7100-START.
           IF WS-OWNS-IT
               MOVE "BEGN" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           END-IF.
       7100-EXIT.
           EXIT.

       7200-COMMIT-TXN SECTION.
       7200-START.
           IF WS-OWNS-IT
               MOVE "COMT" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           END-IF.
       7200-EXIT.
           EXIT.

       7300-ROLLBACK SECTION.
       7300-START.
           IF WS-OWNS-IT
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           END-IF.
       7300-EXIT.
           EXIT.

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

       8100-EXECUTE SECTION.
       8100-START.
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           IF SQLIO-SQLCODE < 0
               PERFORM 8900-SQL-ERROR
           ELSE
               MOVE 1 TO SQLIO-CURSOR
               MOVE "CLOS" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           END-IF.
       8100-EXIT.
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
           MOVE "CST1308" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           STRING "Function " FUNCTION TRIM(CA-FUNCTION OF DFHCOMMAREA)
                  " is not supported by LANDED"
               DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
