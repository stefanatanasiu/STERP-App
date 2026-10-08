      *>================================================================
      *> PROGRAM-ID : FIFOVAL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *> PURPOSE    : FIRST IN FIRST OUT VALUATION.  AN ISSUE CONSUMES
      *>              VALUE FROM THE OLDEST ELIGIBLE FINANCIAL RECEIPT
      *>              LAYERS OF THE POOL, KEEPING THE EXACT QUANTITIES
      *>              TAKEN FROM EACH LAYER SO THAT A LATER RETURN OR
      *>              LANDED COST DIFFERENCE CAN BE ATTRIBUTED BACK.
      *>              THE PHYSICAL PICKING ORDER IS SOMEBODY ELSE'S
      *>              PROBLEM - THIS IS THE FINANCIAL ORDER.
      *> CALLED BY  : STKMOVE (ITEM MODULE), KIT ISSUE, THE VALUATION
      *>              REPORT, WHATIF, THE .NET CLIENT
      *> INTERFACE  : CALL "FIFOVAL" USING WS-FIFOVAL-CA
      *>              COPYBOOK FIFOVALCA
      *> CALLS      : SQLIO (cst.CostLayer, cst.ItemCost)
      *> NOTE       : FV-COMMIT "Y" writes and commits, "T" writes
      *>              inside the caller's transaction, "N" simulates.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-05-30 RJH  CR0518 ORIGINAL FIFO LAYERS
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT RECEIPT DATES
      *> 2008-04-02 DJM  CR1620 LANDED COST ADJUSTMENT OF OPEN LAYERS
      *> 2026-10-06 STERP CST-005 CST-009 PARTIAL LAYER CONSUMPTION
      *>================================================================
      *> REQ: CST-005, CST-009, CST-024
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIFOVAL.
       AUTHOR. R J HARTLEY.
       DATE-WRITTEN. 30/05/1997.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY CSTWORK.
       01  WS-FLAGS.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-WRITE                PIC X VALUE "N".
               88  WS-DO-WRITE                   VALUE "Y".
           05  WS-OWN-TXN              PIC X VALUE "N".
               88  WS-OWNS-TXN                   VALUE "Y".
           05  WS-NEED                 PIC S9(13)V9(6) VALUE 0.
           05  WS-TAKE                 PIC S9(13)V9(6) VALUE 0.
           05  WS-LAYER-UNIT           PIC S9(13)V9(12) VALUE 0.
           05  WS-TAKEN-VALUE          PIC S9(13)V9(2) VALUE 0.
           05  WS-AVAILABLE            PIC S9(13)V9(6) VALUE 0.
           05  WS-NEW-SEQ              PIC S9(15) VALUE 0.
       LINKAGE SECTION.
       COPY FIFOVALCA.
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
               WHEN "RECEIPT" PERFORM 1000-RECEIPT
               WHEN "ISSUE"   PERFORM 2000-ISSUE
               WHEN "RETURN"  PERFORM 3000-RETURN
               WHEN "QUERY"   PERFORM 4000-QUERY
               WHEN "LANDADJ" PERFORM 5000-LANDED-ADJUST
               WHEN OTHER     PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-SQL-FAILED WS-WRITE WS-OWN-TXN
           MOVE "FIFOVAL" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "CST-005" TO CA-REQ-REF
           MOVE 0 TO FV-ISSUE-VALUE FV-ISSUE-UNIT-AVG FV-LAYERS-USED
           MOVE 0 TO FV-POOL-QTY FV-POOL-VALUE FV-REMAIN-QTY
           MOVE 0 TO FV-REMAIN-VALUE FV-NEW-LAYER-SEQ FV-LINE-CNT
           MOVE 0 TO FV-OPEN-LAYER-CNT
           MOVE "N" TO FV-MORE-LAYERS
           MOVE 0 TO WS-NEED WS-TAKE WS-TAKEN-VALUE WS-NEW-SEQ
           MOVE FUNCTION UPPER-CASE(FV-ITEM-CODE) TO FV-ITEM-CODE
           IF FV-EFF-DATE = 0
               MOVE CA-BUSINESS-DATE TO FV-EFF-DATE
           END-IF
           IF FV-COMMIT = "Y"
               MOVE "Y" TO WS-WRITE WS-OWN-TXN
           END-IF
           IF FV-COMMIT = "T"
               MOVE "Y" TO WS-WRITE
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 30
               MOVE 0 TO FV-L-SEQ(WS-IX) FV-L-DATE(WS-IX)
               MOVE 0 TO FV-L-UNIT-COST(WS-IX) FV-L-QTY-TAKEN(WS-IX)
               MOVE 0 TO FV-L-VALUE(WS-IX) FV-L-QTY-LEFT(WS-IX)
               MOVE 0 TO FV-L-VALUE-LEFT(WS-IX) FV-L-LANDED(WS-IX)
               MOVE SPACES TO FV-L-DOC-REF(WS-IX) FV-L-CLOSED(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES OR FV-ITEM-CODE = SPACES
               MOVE "CST0201" TO CA-REASON-CODE
               MOVE "A company and an item code are required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION = "RECEIPT" OR CA-FUNCTION = "ISSUE"
              OR CA-FUNCTION = "RETURN"
               IF FV-QUANTITY <= 0
                   MOVE "CST0202" TO CA-REASON-CODE
                   MOVE "Quantity must be greater than zero"
                       TO CA-MESSAGE
                   PERFORM 9000-REJECT
                   GO TO 0200-EXIT
               END-IF
           END-IF
           IF CA-FUNCTION = "RECEIPT" AND FV-UNIT-COST < 0
               MOVE "CST0203" TO CA-REASON-CODE
               MOVE "A receipt layer cost may not be negative"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  A NEW FINANCIAL LAYER.  THE SEQUENCE IS THE STABLE TIE
      *>       BREAK WHEN TWO LAYERS SHARE A RECEIPT DATE.
      *>----------------------------------------------------------------
       1000-RECEIPT SECTION.
       1000-START.
           IF FV-VALUE-GBP = 0
               COMPUTE FV-VALUE-GBP ROUNDED =
                   FV-QUANTITY * FV-UNIT-COST
           END-IF
           IF FV-UNIT-COST = 0 AND FV-QUANTITY NOT = 0
               COMPUTE FV-UNIT-COST ROUNDED =
                   FV-VALUE-GBP / FV-QUANTITY
           END-IF
           PERFORM 7000-NEXT-SEQUENCE
           IF WS-SQL-BAD
               GO TO 1000-EXIT
           END-IF
           MOVE WS-NEW-SEQ TO FV-NEW-LAYER-SEQ
           IF WS-DO-WRITE
               PERFORM 7100-BEGIN-TXN
               MOVE SPACES TO SQLIO-STMT
               STRING "INSERT INTO cst.CostLayer (CompanyCode, "
                      "ItemCode, Revision, LayerSeq, ReceiptDate, "
                      "SourceDocType, SourceDocRef, LotCode, "
                      "QtyOriginal, QtyRemaining, UnitCost, "
                      "ValueRemainingGBP) VALUES (?, ?, ?, ?, ?, ?, "
                      "?, ?, ?, ?, ?, ?)"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 12 TO SQLIO-PARM-CNT
               MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
               MOVE FV-ITEM-CODE TO SQLIO-PARM-VAL(2)
               MOVE FV-REVISION TO SQLIO-PARM-VAL(3)
               MOVE WS-NEW-SEQ TO SQLIO-INT-EDIT
               MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
               MOVE FV-EFF-DATE TO SQLIO-PARM-VAL(5)
               MOVE FV-DOC-TYPE TO SQLIO-PARM-VAL(6)
               MOVE FV-DOC-REF TO SQLIO-PARM-VAL(7)
               MOVE FV-LOT-CODE TO SQLIO-PARM-VAL(8)
               MOVE FV-QUANTITY TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
               MOVE FV-UNIT-COST TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
               MOVE FV-VALUE-GBP TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
               PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 12
                   MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
               END-PERFORM
               IF FV-LOT-CODE = SPACES
                   MOVE "Y" TO SQLIO-PARM-NULL(8)
               END-IF
               PERFORM 8100-EXECUTE
               IF WS-SQL-BAD
                   PERFORM 7300-ROLLBACK
                   GO TO 1000-EXIT
               END-IF
               PERFORM 7200-COMMIT-TXN
           END-IF
           MOVE 1 TO FV-LINE-CNT
           MOVE WS-NEW-SEQ TO FV-L-SEQ(1)
           MOVE FV-EFF-DATE TO FV-L-DATE(1)
           MOVE FV-DOC-REF TO FV-L-DOC-REF(1)
           MOVE FV-UNIT-COST TO FV-L-UNIT-COST(1)
           MOVE FV-QUANTITY TO FV-L-QTY-LEFT(1)
           MOVE FV-VALUE-GBP TO FV-L-VALUE-LEFT(1)
           PERFORM 6000-POOL-TOTALS.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  CST-005.  CONSUME THE OLDEST LAYERS.  THE LAYERS ARE
      *>       READ INTO THE WINDOW FIRST AND UPDATED AFTERWARDS SO
      *>       THAT WE NEVER UPDATE A TABLE WE ARE STILL READING.
      *>----------------------------------------------------------------
       2000-ISSUE SECTION.
       2000-START.
           MOVE FV-QUANTITY TO WS-NEED
           PERFORM 2100-READ-LAYERS
           IF WS-SQL-BAD OR WS-INPUT-BAD
               GO TO 2000-EXIT
           END-IF
           IF WS-NEED > 0
               MOVE "CST0204" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               MOVE WS-NEED TO WS-QTY-ED
               STRING "Only part of the issue can be valued - "
                      FUNCTION TRIM(WS-QTY-ED)
                      " units have no cost layer"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           IF FV-QUANTITY NOT = 0
               COMPUTE FV-ISSUE-UNIT-AVG ROUNDED =
                   FV-ISSUE-VALUE / FV-QUANTITY
           END-IF
           IF WS-DO-WRITE
               PERFORM 7100-BEGIN-TXN
               PERFORM 2300-APPLY-LAYERS
               IF WS-SQL-BAD
                   PERFORM 7300-ROLLBACK
                   GO TO 2000-EXIT
               END-IF
               PERFORM 7200-COMMIT-TXN
           END-IF
           PERFORM 6000-POOL-TOTALS.
       2000-EXIT.
           EXIT.

       2100-READ-LAYERS SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT LayerSeq, CONVERT(char(8), ReceiptDate, "
                  "112), QtyRemaining, UnitCost, ValueRemainingGBP, "
                  "LandedAdjustmentGBP, SourceDocRef FROM "
                  "cst.CostLayer WHERE CompanyCode = ? AND ItemCode "
                  "= ? AND Revision = ? AND IsClosed = 0 AND "
                  "QtyRemaining > 0 ORDER BY ReceiptDate, LayerSeq"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE FV-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE FV-REVISION TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 7 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           IF SQLIO-SQLCODE < 0
               PERFORM 8900-SQL-ERROR
               GO TO 2100-EXIT
           END-IF
           PERFORM UNTIL WS-NEED <= 0 OR WS-SQL-BAD
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
               PERFORM 2200-TAKE-FROM-LAYER
           END-PERFORM
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW.
       2100-EXIT.
           EXIT.

       2200-TAKE-FROM-LAYER SECTION.
       2200-START.
           IF FV-LINE-CNT >= 30
               MOVE "CST0206" TO CA-REASON-CODE
               MOVE "This issue spans more than thirty cost layers"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2200-EXIT
           END-IF
           MOVE FUNCTION NUMVAL(WS-COL(3)) TO WS-AVAILABLE
           IF WS-AVAILABLE <= 0
               GO TO 2200-EXIT
           END-IF
           IF WS-AVAILABLE >= WS-NEED
               MOVE WS-NEED TO WS-TAKE
           ELSE
               MOVE WS-AVAILABLE TO WS-TAKE
           END-IF
           MOVE FUNCTION NUMVAL(WS-COL(5)) TO WS-AMT1
           MOVE FUNCTION NUMVAL(WS-COL(6)) TO WS-AMT2
           COMPUTE WS-LAYER-UNIT =
               (WS-AMT1 + WS-AMT2) / WS-AVAILABLE
           COMPUTE WS-TAKEN-VALUE ROUNDED = WS-TAKE * WS-LAYER-UNIT
           ADD 1 TO FV-LINE-CNT
           MOVE FV-LINE-CNT TO WS-IX2
           MOVE FUNCTION NUMVAL(WS-COL(1)) TO FV-L-SEQ(WS-IX2)
           MOVE FUNCTION NUMVAL(WS-COL(2)) TO FV-L-DATE(WS-IX2)
           MOVE WS-COL(7) TO FV-L-DOC-REF(WS-IX2)
           MOVE WS-LAYER-UNIT TO FV-L-UNIT-COST(WS-IX2)
           MOVE WS-TAKE TO FV-L-QTY-TAKEN(WS-IX2)
           MOVE WS-TAKEN-VALUE TO FV-L-VALUE(WS-IX2)
           COMPUTE FV-L-QTY-LEFT(WS-IX2) = WS-AVAILABLE - WS-TAKE
           COMPUTE FV-L-VALUE-LEFT(WS-IX2) =
               WS-AMT1 + WS-AMT2 - WS-TAKEN-VALUE
           MOVE WS-AMT2 TO FV-L-LANDED(WS-IX2)
           IF FV-L-QTY-LEFT(WS-IX2) = 0
               MOVE "Y" TO FV-L-CLOSED(WS-IX2)
           ELSE
               MOVE "N" TO FV-L-CLOSED(WS-IX2)
           END-IF
           ADD WS-TAKEN-VALUE TO FV-ISSUE-VALUE
           ADD 1 TO FV-LAYERS-USED
           SUBTRACT WS-TAKE FROM WS-NEED.
       2200-EXIT.
           EXIT.

       2300-APPLY-LAYERS SECTION.
       2300-START.
           PERFORM VARYING WS-IX FROM 1 BY 1
               UNTIL WS-IX > FV-LINE-CNT OR WS-SQL-BAD
               MOVE SPACES TO SQLIO-STMT
               STRING "UPDATE cst.CostLayer SET QtyRemaining = ?, "
                      "ValueRemainingGBP = ?, LandedAdjustmentGBP = "
                      "0, IsClosed = CASE WHEN CAST(? AS "
                      "decimal(19,6)) = 0 THEN 1 ELSE 0 "
                      "END WHERE CompanyCode = ? AND ItemCode = ? "
                      "AND Revision = ? AND LayerSeq = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 7 TO SQLIO-PARM-CNT
               MOVE FV-L-QTY-LEFT(WS-IX) TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
               MOVE FV-L-VALUE-LEFT(WS-IX) TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
               MOVE CA-COMPANY TO SQLIO-PARM-VAL(4)
               MOVE FV-ITEM-CODE TO SQLIO-PARM-VAL(5)
               MOVE FV-REVISION TO SQLIO-PARM-VAL(6)
               MOVE FV-L-SEQ(WS-IX) TO SQLIO-INT-EDIT
               MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(7)
               PERFORM VARYING WS-IX3 FROM 1 BY 1 UNTIL WS-IX3 > 7
                   MOVE "N" TO SQLIO-PARM-NULL(WS-IX3)
               END-PERFORM
               PERFORM 8100-EXECUTE
           END-PERFORM.
       2300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  CST-009.  A RETURN PUTS STOCK BACK AT ITS ATTRIBUTABLE
      *>       ORIGINAL COST.  IT IS A NEW LAYER DATED AS AT THE
      *>       RETURN SO THAT THE REMAINING SEQUENCE STAYS STABLE,
      *>       AND IT NEVER RELIEVES COST TWICE.
      *>----------------------------------------------------------------
       3000-RETURN SECTION.
       3000-START.
           IF FV-UNIT-COST <= 0
               PERFORM 3100-ORIGINAL-COST
               IF WS-SQL-BAD OR WS-INPUT-BAD
                   GO TO 3000-EXIT
               END-IF
           END-IF
           COMPUTE FV-VALUE-GBP ROUNDED = FV-QUANTITY * FV-UNIT-COST
           MOVE "RETURN" TO FV-DOC-TYPE
           PERFORM 1000-RECEIPT.
       3000-EXIT.
           EXIT.

       3100-ORIGINAL-COST SECTION.
       3100-START.
           MOVE SPACES TO SQLIO-STMT
           IF FV-LAYER-SEQ > 0
               STRING "SELECT UnitCost FROM cst.CostLayer WHERE "
                      "CompanyCode = ? AND ItemCode = ? AND "
                      "LayerSeq = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 3 TO SQLIO-PARM-CNT
               MOVE FV-LAYER-SEQ TO SQLIO-INT-EDIT
               MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
               MOVE "N" TO SQLIO-PARM-NULL(3)
           ELSE
               STRING "SELECT TOP 1 UnitCost FROM cst.CostLayer "
                      "WHERE CompanyCode = ? AND ItemCode = ? "
                      "ORDER BY ReceiptDate, LayerSeq"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 2 TO SQLIO-PARM-CNT
           END-IF
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE FV-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF NOT WS-GOT-ROW
               MOVE "CST0205" TO CA-REASON-CODE
               MOVE "No original cost layer - controlled valuation "
                   TO CA-MESSAGE
               STRING FUNCTION TRIM(CA-MESSAGE) "evidence is required"
                   DELIMITED BY SIZE INTO WS-WORK-TEXT
               MOVE WS-WORK-TEXT TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 3100-EXIT
           END-IF
           MOVE FUNCTION NUMVAL(WS-COL(1)) TO FV-UNIT-COST.
       3100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  OPEN LAYERS AND THE POOL TOTALS - CHANGES NOTHING.
      *>----------------------------------------------------------------
       4000-QUERY SECTION.
       4000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT LayerSeq, CONVERT(char(8), ReceiptDate, "
                  "112), QtyRemaining, UnitCost, ValueRemainingGBP, "
                  "LandedAdjustmentGBP, SourceDocRef FROM "
                  "cst.CostLayer WHERE CompanyCode = ? AND ItemCode "
                  "= ? AND Revision = ? AND IsClosed = 0 ORDER BY "
                  "ReceiptDate, LayerSeq"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE FV-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE FV-REVISION TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 7 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           IF SQLIO-SQLCODE < 0
               PERFORM 8900-SQL-ERROR
               GO TO 4000-EXIT
           END-IF
           PERFORM UNTIL WS-SQL-BAD
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
               ADD 1 TO FV-OPEN-LAYER-CNT
               IF FV-LINE-CNT < 30
                   ADD 1 TO FV-LINE-CNT
                   MOVE FV-LINE-CNT TO WS-IX2
                   MOVE FUNCTION NUMVAL(WS-COL(1)) TO FV-L-SEQ(WS-IX2)
                   MOVE FUNCTION NUMVAL(WS-COL(2))
                       TO FV-L-DATE(WS-IX2)
                   MOVE WS-COL(7) TO FV-L-DOC-REF(WS-IX2)
                   MOVE FUNCTION NUMVAL(WS-COL(4))
                       TO FV-L-UNIT-COST(WS-IX2)
                   MOVE FUNCTION NUMVAL(WS-COL(3))
                       TO FV-L-QTY-LEFT(WS-IX2)
                   MOVE FUNCTION NUMVAL(WS-COL(5))
                       TO FV-L-VALUE-LEFT(WS-IX2)
                   MOVE FUNCTION NUMVAL(WS-COL(6))
                       TO FV-L-LANDED(WS-IX2)
                   MOVE "N" TO FV-L-CLOSED(WS-IX2)
               ELSE
                   MOVE "Y" TO FV-MORE-LAYERS
               END-IF
           END-PERFORM
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           PERFORM 6000-POOL-TOTALS.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000  CST-008.  A LATE LANDED COST DIFFERENCE IS ADDED TO THE
      *>       REMAINING PART OF A NAMED LAYER ONLY.  THE PART THAT
      *>       HAS ALREADY BEEN ISSUED IS SOMEONE ELSE'S ADJUSTMENT.
      *>----------------------------------------------------------------
       5000-LANDED-ADJUST SECTION.
       5000-START.
           IF FV-LAYER-SEQ <= 0
               MOVE "CST0205" TO CA-REASON-CODE
               MOVE "A layer sequence is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 5000-EXIT
           END-IF
           IF WS-DO-WRITE
               PERFORM 7100-BEGIN-TXN
               MOVE SPACES TO SQLIO-STMT
               STRING "UPDATE cst.CostLayer SET LandedAdjustmentGBP "
                      "= LandedAdjustmentGBP + ? WHERE CompanyCode = "
                      "? AND ItemCode = ? AND LayerSeq = ? AND "
                      "IsClosed = 0"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 4 TO SQLIO-PARM-CNT
               MOVE FV-VALUE-GBP TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
               MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
               MOVE FV-ITEM-CODE TO SQLIO-PARM-VAL(3)
               MOVE FV-LAYER-SEQ TO SQLIO-INT-EDIT
               MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
               PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 4
                   MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
               END-PERFORM
               PERFORM 8100-EXECUTE
               IF WS-SQL-BAD
                   PERFORM 7300-ROLLBACK
                   GO TO 5000-EXIT
               END-IF
               IF SQLIO-ROWCOUNT = 0
                   MOVE 4 TO CA-RETURN-CODE
                   MOVE "CST0205" TO CA-REASON-CODE
                   MOVE "The layer is closed - the adjustment "
                       TO CA-MESSAGE
                   STRING FUNCTION TRIM(CA-MESSAGE)
                          "belongs to issued stock"
                       DELIMITED BY SIZE INTO WS-WORK-TEXT
                   MOVE WS-WORK-TEXT TO CA-MESSAGE
               END-IF
               PERFORM 7200-COMMIT-TXN
           END-IF
           PERFORM 6000-POOL-TOTALS.
       5000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 6000  POOL TOTALS AFTER WHATEVER JUST HAPPENED
      *>----------------------------------------------------------------
       6000-POOL-TOTALS SECTION.
       6000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(SUM(QtyRemaining),0), "
                  "ISNULL(SUM(ValueRemainingGBP + "
                  "LandedAdjustmentGBP),0), COUNT(*) FROM "
                  "cst.CostLayer WHERE CompanyCode = ? AND ItemCode "
                  "= ? AND Revision = ? AND IsClosed = 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE FV-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE FV-REVISION TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 3 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-GOT-ROW
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO FV-POOL-QTY
               MOVE FUNCTION NUMVAL(WS-COL(2)) TO FV-POOL-VALUE
               MOVE FV-POOL-QTY TO FV-REMAIN-QTY
               MOVE FV-POOL-VALUE TO FV-REMAIN-VALUE
           END-IF.
       6000-EXIT.
           EXIT.

       7000-NEXT-SEQUENCE SECTION.
       7000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(MAX(LayerSeq),0) + 1 FROM "
                  "cst.CostLayer WHERE CompanyCode = ? AND ItemCode "
                  "= ? AND Revision = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE FV-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE FV-REVISION TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-GOT-ROW
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO WS-NEW-SEQ
           ELSE
               MOVE 1 TO WS-NEW-SEQ
           END-IF.
       7000-EXIT.
           EXIT.

       7100-BEGIN-TXN SECTION.
       7100-START.
           IF WS-OWNS-TXN
               MOVE "BEGN" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           END-IF.
       7100-EXIT.
           EXIT.

       7200-COMMIT-TXN SECTION.
       7200-START.
           IF WS-OWNS-TXN
               MOVE "COMT" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           END-IF.
       7200-EXIT.
           EXIT.

       7300-ROLLBACK SECTION.
       7300-START.
           IF WS-OWNS-TXN
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
           MOVE "CST9001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG TO CA-MESSAGE
           MOVE 12 TO CA-RETURN-CODE.
       8900-EXIT.
           EXIT.

       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "CST0207" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                  " is not supported by FIFOVAL"
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
