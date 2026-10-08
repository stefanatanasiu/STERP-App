      *>================================================================
      *> PROGRAM-ID : MWAVAL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *> PURPOSE    : MOVING WEIGHTED AVERAGE VALUATION.  THE AVERAGE
      *>              IS RECALCULATED FROM THE EXISTING CARRYING VALUE
      *>              AND THE INCOMING ATTRIBUTABLE VALUE AT EVERY
      *>              RECEIPT.  A ZERO DENOMINATOR DOES NOT INVENT A
      *>              COST AND NEGATIVE PHYSICAL STOCK IS REFUSED.
      *>              EVERY RECALCULATION LEAVES A HISTORY ROW SO THAT
      *>              A VALUATION CAN BE EXPLAINED MONTHS LATER.
      *> CALLED BY  : STKMOVE (ITEM MODULE), PURCHASE RECEIPT, KIT
      *>              ISSUE, LANDED, STDCOST, THE .NET CLIENT
      *> INTERFACE  : CALL "MWAVAL" USING WS-MWAVAL-CA
      *>              COPYBOOK MWAVALCA
      *> CALLS      : SQLIO (cst.ItemCost, cst.ItemCostHistory)
      *> NOTE       : MW-COMMIT "Y" writes and commits, "T" writes
      *>              inside the caller's transaction, "N" simulates.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-05-30 RJH  CR0519 ORIGINAL WEIGHTED AVERAGE
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT EFFECTIVE DATES
      *> 2012-09-14 DJM  CR1904 HISTORY ROW FOR EVERY RECALCULATION
      *> 2026-10-06 STERP CST-006 CST-009 RESIDUAL ON ZERO QUANTITY
      *>================================================================
      *> REQ: CST-006, CST-009, CST-024
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MWAVAL.
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
           05  WS-POOL-EXISTS          PIC X VALUE "N".
               88  WS-HAVE-POOL                  VALUE "Y".
           05  WS-POOL-SITE            PIC X(10) VALUE "*".
       LINKAGE SECTION.
       COPY MWAVALCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0300-READ-POOL
           IF WS-SQL-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "RECEIPT" PERFORM 1000-RECEIPT
               WHEN "ISSUE"   PERFORM 2000-ISSUE
               WHEN "RETURN"  PERFORM 3000-RETURN
               WHEN "QUERY"   PERFORM 4000-QUERY
               WHEN "REVALUE" PERFORM 5000-REVALUE
               WHEN "OPENING" PERFORM 6000-OPENING
               WHEN OTHER     PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-SQL-FAILED WS-WRITE WS-OWN-TXN
           MOVE "N" TO WS-POOL-EXISTS
           MOVE "MWAVAL" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "CST-006" TO CA-REQ-REF
           MOVE 0 TO MW-QTY-BEFORE MW-VALUE-BEFORE MW-AVG-BEFORE
           MOVE 0 TO MW-QTY-CHANGE MW-VALUE-CHANGE MW-QTY-AFTER
           MOVE 0 TO MW-VALUE-AFTER MW-AVG-AFTER MW-ISSUE-VALUE
           MOVE 0 TO MW-RESIDUAL-GBP MW-HISTORY-ID
           MOVE SPACES TO MW-WARNING MW-COST-METHOD
           MOVE FUNCTION UPPER-CASE(MW-ITEM-CODE) TO MW-ITEM-CODE
           IF MW-POOL-SITE = SPACES
               MOVE "*" TO MW-POOL-SITE
           END-IF
           MOVE MW-POOL-SITE TO WS-POOL-SITE
           IF MW-EFF-DATE = 0
               MOVE CA-BUSINESS-DATE TO MW-EFF-DATE
           END-IF
           IF MW-COMMIT = "Y"
               MOVE "Y" TO WS-WRITE WS-OWN-TXN
           END-IF
           IF MW-COMMIT = "T"
               MOVE "Y" TO WS-WRITE
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES OR MW-ITEM-CODE = SPACES
               MOVE "CST0301" TO CA-REASON-CODE
               MOVE "A company and an item code are required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION = "RECEIPT" OR CA-FUNCTION = "ISSUE"
              OR CA-FUNCTION = "RETURN"
               IF MW-QUANTITY <= 0
                   MOVE "CST0302" TO CA-REASON-CODE
                   MOVE "Quantity must be greater than zero"
                       TO CA-MESSAGE
                   PERFORM 9000-REJECT
               END-IF
           END-IF.
       0200-EXIT.
           EXIT.

       0300-READ-POOL SECTION.
       0300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT QtyOnHand, CarryingValueGBP, "
                  "AverageUnitCost, CostMethod FROM cst.ItemCost "
                  "WHERE CompanyCode = ? AND ItemCode = ? AND "
                  "Revision = ? AND SiteCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE MW-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE MW-REVISION TO SQLIO-PARM-VAL(3)
           MOVE WS-POOL-SITE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 4 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-GOT-ROW
               MOVE "Y" TO WS-POOL-EXISTS
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO MW-QTY-BEFORE
               MOVE FUNCTION NUMVAL(WS-COL(2)) TO MW-VALUE-BEFORE
               MOVE FUNCTION NUMVAL(WS-COL(3)) TO MW-AVG-BEFORE
               MOVE WS-COL(4) TO MW-COST-METHOD
           END-IF
           MOVE MW-QTY-BEFORE TO MW-QTY-AFTER
           MOVE MW-VALUE-BEFORE TO MW-VALUE-AFTER
           MOVE MW-AVG-BEFORE TO MW-AVG-AFTER.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  CST-006.  NEW AVERAGE = (EXISTING CARRYING VALUE +
      *>       INCOMING ATTRIBUTABLE VALUE) / (EXISTING QUANTITY +
      *>       RECEIVED QUANTITY).  SPEC EXAMPLE C2.
      *>----------------------------------------------------------------
       1000-RECEIPT SECTION.
       1000-START.
           IF MW-VALUE-GBP = 0
               COMPUTE MW-VALUE-GBP ROUNDED =
                   MW-QUANTITY * MW-UNIT-COST
           END-IF
           COMPUTE MW-QTY-AFTER = MW-QTY-BEFORE + MW-QUANTITY
           COMPUTE MW-VALUE-AFTER = MW-VALUE-BEFORE + MW-VALUE-GBP
           IF MW-QTY-AFTER <= 0
               MOVE "CST0304" TO CA-REASON-CODE
               MOVE "The closing quantity is not positive - no "
                   TO CA-MESSAGE
               STRING FUNCTION TRIM(CA-MESSAGE)
                      "average can be calculated"
                   DELIMITED BY SIZE INTO WS-WORK-TEXT
               MOVE WS-WORK-TEXT TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1000-EXIT
           END-IF
           COMPUTE MW-AVG-AFTER ROUNDED =
               MW-VALUE-AFTER / MW-QTY-AFTER
           MOVE MW-QUANTITY TO MW-QTY-CHANGE
           MOVE MW-VALUE-GBP TO MW-VALUE-CHANGE
           MOVE "RECEIPT" TO MW-CHANGE-TYPE
           PERFORM 7000-APPLY.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  ISSUES USE THE CURRENT APPROVED AVERAGE.  THEY NEVER
      *>       DRIVE THE QUANTITY NEGATIVE AND THEY NEVER LEAVE AN
      *>       UNEXPLAINED VALUE BEHIND ON A ZERO BALANCE.
      *>----------------------------------------------------------------
       2000-ISSUE SECTION.
       2000-START.
           IF NOT WS-HAVE-POOL
               MOVE "CST0305" TO CA-REASON-CODE
               MOVE "No carrying value is held for this item"
                   TO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               GO TO 2000-EXIT
           END-IF
           IF MW-QUANTITY > MW-QTY-BEFORE
               MOVE "CST0303" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               MOVE MW-QTY-BEFORE TO WS-QTY-ED
               STRING "Issue exceeds the quantity on hand of "
                      FUNCTION TRIM(WS-QTY-ED)
                      " - negative physical stock is prohibited"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           COMPUTE MW-ISSUE-VALUE ROUNDED =
               MW-QUANTITY * MW-AVG-BEFORE
           COMPUTE MW-QTY-AFTER = MW-QTY-BEFORE - MW-QUANTITY
           COMPUTE MW-VALUE-AFTER = MW-VALUE-BEFORE - MW-ISSUE-VALUE
           IF MW-QTY-AFTER = 0
               IF MW-VALUE-AFTER NOT = 0
                   MOVE MW-VALUE-AFTER TO MW-RESIDUAL-GBP
                   ADD MW-RESIDUAL-GBP TO MW-ISSUE-VALUE
                   MOVE 0 TO MW-VALUE-AFTER
                   MOVE "Rounding residual cleared to the issue"
                       TO MW-WARNING
                   MOVE 4 TO CA-RETURN-CODE
               END-IF
               MOVE 0 TO MW-AVG-AFTER
           ELSE
               COMPUTE MW-AVG-AFTER ROUNDED =
                   MW-VALUE-AFTER / MW-QTY-AFTER
           END-IF
           COMPUTE MW-QTY-CHANGE = 0 - MW-QUANTITY
           COMPUTE MW-VALUE-CHANGE = 0 - MW-ISSUE-VALUE
           MOVE "ISSUE" TO MW-CHANGE-TYPE
           PERFORM 7000-APPLY.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  CST-009.  A RETURN RESTORES THE ATTRIBUTABLE ORIGINAL
      *>       COST WHEN IT IS KNOWN, OTHERWISE THE CURRENT AVERAGE.
      *>       IT IS A QUANTITY EVENT, NOT A VALUE ONLY CORRECTION.
      *>----------------------------------------------------------------
       3000-RETURN SECTION.
       3000-START.
           IF MW-UNIT-COST <= 0
               MOVE MW-AVG-BEFORE TO MW-UNIT-COST
               MOVE "Returned at the current average - no original "
                   TO MW-WARNING
               MOVE 4 TO CA-RETURN-CODE
           END-IF
           COMPUTE MW-VALUE-GBP ROUNDED = MW-QUANTITY * MW-UNIT-COST
           COMPUTE MW-QTY-AFTER = MW-QTY-BEFORE + MW-QUANTITY
           COMPUTE MW-VALUE-AFTER = MW-VALUE-BEFORE + MW-VALUE-GBP
           IF MW-QTY-AFTER > 0
               COMPUTE MW-AVG-AFTER ROUNDED =
                   MW-VALUE-AFTER / MW-QTY-AFTER
           END-IF
           MOVE MW-QUANTITY TO MW-QTY-CHANGE
           MOVE MW-VALUE-GBP TO MW-VALUE-CHANGE
           MOVE "RECEIPT" TO MW-CHANGE-TYPE
           PERFORM 7000-APPLY.
       3000-EXIT.
           EXIT.

       4000-QUERY SECTION.
       4000-START.
           IF NOT WS-HAVE-POOL
               MOVE "CST0305" TO CA-REASON-CODE
               MOVE "No carrying value is held for this item"
                   TO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
           END-IF.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000  A VALUE ONLY CHANGE - LATE LANDED COST, A SUPPLIER
      *>       CREDIT OR A STANDARD ACTIVATION.  THE QUANTITY DOES NOT
      *>       MOVE, SO A ZERO QUANTITY HAS NOTHING TO CARRY THE VALUE.
      *>----------------------------------------------------------------
       5000-REVALUE SECTION.
       5000-START.
           IF NOT WS-HAVE-POOL
               MOVE "CST0305" TO CA-REASON-CODE
               MOVE "No carrying value is held for this item"
                   TO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               GO TO 5000-EXIT
           END-IF
           IF MW-QTY-BEFORE = 0
               MOVE "CST0306" TO CA-REASON-CODE
               MOVE "A value only change needs stock to carry it"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 5000-EXIT
           END-IF
           COMPUTE MW-VALUE-AFTER = MW-VALUE-BEFORE + MW-VALUE-GBP
           COMPUTE MW-AVG-AFTER ROUNDED =
               MW-VALUE-AFTER / MW-QTY-BEFORE
           MOVE 0 TO MW-QTY-CHANGE
           MOVE MW-VALUE-GBP TO MW-VALUE-CHANGE
           IF MW-CHANGE-TYPE = SPACES
               MOVE "REVALUE" TO MW-CHANGE-TYPE
           END-IF
           PERFORM 7000-APPLY.
       5000-EXIT.
           EXIT.

       6000-OPENING SECTION.
       6000-START.
           IF WS-HAVE-POOL
               MOVE 4 TO CA-RETURN-CODE
               MOVE "The pool already exists" TO MW-WARNING
               GO TO 6000-EXIT
           END-IF
           MOVE MW-QUANTITY TO MW-QTY-AFTER MW-QTY-CHANGE
           MOVE MW-VALUE-GBP TO MW-VALUE-AFTER MW-VALUE-CHANGE
           IF MW-QTY-AFTER > 0
               COMPUTE MW-AVG-AFTER ROUNDED =
                   MW-VALUE-AFTER / MW-QTY-AFTER
           END-IF
           MOVE "OPENING" TO MW-CHANGE-TYPE
           PERFORM 7000-APPLY.
       6000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 7000  WRITE THE POOL AND ITS HISTORY ROW.  EVERY MOVING
      *>       AVERAGE RECALCULATION IS EXPLAINABLE AFTERWARDS.
      *>----------------------------------------------------------------
       7000-APPLY SECTION.
       7000-START.
           IF NOT WS-DO-WRITE
               GO TO 7000-EXIT
           END-IF
           PERFORM 7100-BEGIN-TXN
           IF WS-HAVE-POOL
               MOVE SPACES TO SQLIO-STMT
               STRING "UPDATE cst.ItemCost SET QtyOnHand = ?, "
                      "CarryingValueGBP = ?, AverageUnitCost = ?, "
                      "LastUpdatedAt = SYSDATETIME(), LastUpdatedBy "
                      "= ? WHERE CompanyCode = ? AND ItemCode = ? "
                      "AND Revision = ? AND SiteCode = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 8 TO SQLIO-PARM-CNT
               MOVE MW-QTY-AFTER TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
               MOVE MW-VALUE-AFTER TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
               MOVE MW-AVG-AFTER TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
               MOVE CA-USER TO SQLIO-PARM-VAL(4)
               MOVE CA-COMPANY TO SQLIO-PARM-VAL(5)
               MOVE MW-ITEM-CODE TO SQLIO-PARM-VAL(6)
               MOVE MW-REVISION TO SQLIO-PARM-VAL(7)
               MOVE WS-POOL-SITE TO SQLIO-PARM-VAL(8)
               PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 8
                   MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
               END-PERFORM
           ELSE
               MOVE SPACES TO SQLIO-STMT
               STRING "INSERT INTO cst.ItemCost (CompanyCode, "
                      "ItemCode, Revision, SiteCode, CostMethod, "
                      "QtyOnHand, CarryingValueGBP, AverageUnitCost, "
                      "LastUpdatedAt, LastUpdatedBy) VALUES (?, ?, "
                      "?, ?, 'MWA', ?, ?, ?, SYSDATETIME(), ?)"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 8 TO SQLIO-PARM-CNT
               MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
               MOVE MW-ITEM-CODE TO SQLIO-PARM-VAL(2)
               MOVE MW-REVISION TO SQLIO-PARM-VAL(3)
               MOVE WS-POOL-SITE TO SQLIO-PARM-VAL(4)
               MOVE MW-QTY-AFTER TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
               MOVE MW-VALUE-AFTER TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
               MOVE MW-AVG-AFTER TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
               MOVE CA-USER TO SQLIO-PARM-VAL(8)
               PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 8
                   MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
               END-PERFORM
           END-IF
           PERFORM 8100-EXECUTE
           IF WS-SQL-BAD
               PERFORM 7300-ROLLBACK
               GO TO 7000-EXIT
           END-IF
           PERFORM 7400-WRITE-HISTORY
           IF WS-SQL-BAD
               PERFORM 7300-ROLLBACK
               GO TO 7000-EXIT
           END-IF
           PERFORM 7200-COMMIT-TXN.
       7000-EXIT.
           EXIT.

       7400-WRITE-HISTORY SECTION.
       7400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO cst.ItemCostHistory (CompanyCode, "
                  "ItemCode, Revision, ChangeType, QtyBefore, "
                  "ValueBeforeGBP, AvgBefore, QtyChange, "
                  "ValueChangeGBP, QtyAfter, ValueAfterGBP, "
                  "AvgAfter, SourceDocType, SourceDocRef, CreatedBy) "
                  "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, "
                  "?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 15 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE MW-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE MW-REVISION TO SQLIO-PARM-VAL(3)
           MOVE MW-CHANGE-TYPE TO SQLIO-PARM-VAL(4)
           MOVE MW-QTY-BEFORE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE MW-VALUE-BEFORE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE MW-AVG-BEFORE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE MW-QTY-CHANGE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE MW-VALUE-CHANGE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE MW-QTY-AFTER TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE MW-VALUE-AFTER TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE MW-AVG-AFTER TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE MW-DOC-TYPE TO SQLIO-PARM-VAL(13)
           MOVE MW-DOC-REF TO SQLIO-PARM-VAL(14)
           MOVE CA-USER TO SQLIO-PARM-VAL(15)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 15
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           PERFORM 8100-EXECUTE.
       7400-EXIT.
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
           MOVE "CST0307" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                  " is not supported by MWAVAL"
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
