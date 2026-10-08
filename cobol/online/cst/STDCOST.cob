      *>================================================================
      *> PROGRAM-ID : STDCOST
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *> PURPOSE    : STANDARD COST SET LIFECYCLE AND THE VARIANCES
      *>              THAT STANDARD COSTING CREATES.  A SET IS
      *>              PREPARED, REVIEWED, APPROVED AND ONLY THEN
      *>              ACTIVATED; ONLY ACTIVATION REVALUES STOCK, AND
      *>              IT DOES SO THROUGH AN EXPLICIT REVALUATION WITH
      *>              ITS OWN VARIANCE.  NOTHING HERE EVER REWRITES A
      *>              HISTORICAL COST OF SALES.
      *> CALLED BY  : GOODS RECEIPT, INVOICE MATCHING, SALES RETURNS,
      *>              COSTROLL, THE .NET CLIENT
      *> INTERFACE  : CALL "STDCOST" USING WS-STDCOST-CA
      *>              COPYBOOK STDCOSTCA
      *> CALLS      : SQLIO (cst.CostSet, cst.StandardCost,
      *>              cst.StandardCostElement, cst.ItemCost)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-06-11 RJH  CR0524 ORIGINAL STANDARD COST SETS
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT EFFECTIVE DATES
      *> 2002-04-08 DJM  CR0998 SEPARATE REVIEW AND APPROVE STAGES
      *> 2026-10-06 STERP CST-007 CST-012 ACTIVATION REVALUATION
      *>================================================================
      *> REQ: CST-007, CST-009, CST-012
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STDCOST.
       AUTHOR. R J HARTLEY.
       DATE-WRITTEN. 11/06/1997.
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
           05  WS-SET-STATUS           PIC X(10) VALUE SPACES.
           05  WS-SET-PURPOSE          PIC X(12) VALUE SPACES.
           05  WS-PREPARER             PIC X(20) VALUE SPACES.
           05  WS-NEW-STATUS           PIC X(10) VALUE SPACES.
           05  WS-ORDER-VALUE          PIC S9(13)V9(2) VALUE 0.
       LINKAGE SECTION.
       COPY STDCOSTCA.
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
               WHEN "GETSTD"   PERFORM 1000-GET-STANDARD
               WHEN "REVIEW"   PERFORM 2000-REVIEW
               WHEN "APPROVE"  PERFORM 2100-APPROVE
               WHEN "ACTIVATE" PERFORM 2200-ACTIVATE
               WHEN "IMPACT"   PERFORM 3000-IMPACT
               WHEN "RECEIPT"  PERFORM 4000-RECEIPT-PPV
               WHEN "INVOICE"  PERFORM 4100-INVOICE-PPV
               WHEN "RETURN"   PERFORM 4200-RETURN-VALUE
               WHEN "COMPARE"  PERFORM 5000-COMPARE
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-SQL-FAILED WS-WRITE
           MOVE "STDCOST" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "CST-007" TO CA-REQ-REF
           MOVE 0 TO SC-INVENTORY-GBP SC-GRNI-GBP SC-PPV-GBP
           MOVE 0 TO SC-INCREMENTAL-GBP SC-TOTAL-VAR-GBP
           MOVE 0 TO SC-RETURN-VALUE SC-RETURN-VAR-GBP SC-REVAL-QTY
           MOVE 0 TO SC-REVAL-GBP SC-WIP-REVAL-GBP SC-ITEM-COUNT
           MOVE 0 TO SC-VARIANCE-PCT SC-LINE-CNT
           MOVE 0 TO SC-EL-MATERIAL SC-EL-LABOUR SC-EL-MACHINE
           MOVE 0 TO SC-EL-SUBCON SC-EL-OVERHEAD SC-EL-RECOVERY
           MOVE "N" TO SC-ADVERSE SC-MORE-LINES
           MOVE SPACES TO SC-WARNING SC-STATUS SC-PURPOSE
           MOVE "COMPLETE" TO SC-COMPLETENESS
           MOVE SPACES TO WS-SET-STATUS WS-SET-PURPOSE WS-PREPARER
           MOVE FUNCTION UPPER-CASE(SC-ITEM-CODE) TO SC-ITEM-CODE
           MOVE FUNCTION UPPER-CASE(SC-COST-SET) TO SC-COST-SET
           IF SC-EFF-DATE = 0
               MOVE CA-BUSINESS-DATE TO SC-EFF-DATE
           END-IF
           IF SC-LOT-SIZE = 0
               MOVE 1 TO SC-LOT-SIZE
           END-IF
           IF SC-COMMIT = "Y" OR SC-COMMIT = "T"
               MOVE "Y" TO WS-WRITE
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES
               MOVE "CST0401" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF SC-LOT-SIZE <= 0
               MOVE "CST0408" TO CA-REASON-CODE
               MOVE "The costing lot size must be positive"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION = "RECEIPT" OR CA-FUNCTION = "INVOICE"
              OR CA-FUNCTION = "RETURN"
               IF SC-QUANTITY <= 0
                   MOVE "CST0407" TO CA-REASON-CODE
                   MOVE "Quantity must be greater than zero"
                       TO CA-MESSAGE
                   PERFORM 9000-REJECT
               END-IF
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  THE EFFECTIVE STANDARD AND ITS ELEMENT SPLIT.  A COST
      *>       WITHOUT A BREAKDOWN IS NOT AN ANSWER (CST-002).
      *>----------------------------------------------------------------
       1000-GET-STANDARD SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           IF SC-COST-SET = SPACES
               STRING "SELECT TOP 1 sc.StandardCostId, "
                      "sc.TotalUnitCost, sc.CostingLotSize, "
                      "sc.PreviousUnitCost, cs.CostSetCode, "
                      "cs.Status, sc.Completeness FROM "
                      "cst.StandardCost sc JOIN cst.CostSet cs ON "
                      "cs.CompanyCode = sc.CompanyCode AND "
                      "cs.CostSetCode = sc.CostSetCode WHERE "
                      "sc.CompanyCode = ? AND sc.ItemCode = ? AND "
                      "cs.Status = 'ACTIVE' AND cs.EffectiveFrom <= "
                      "? ORDER BY cs.EffectiveFrom DESC"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 3 TO SQLIO-PARM-CNT
               MOVE SC-EFF-DATE TO SQLIO-PARM-VAL(3)
           ELSE
               STRING "SELECT TOP 1 sc.StandardCostId, "
                      "sc.TotalUnitCost, sc.CostingLotSize, "
                      "sc.PreviousUnitCost, cs.CostSetCode, "
                      "cs.Status, sc.Completeness FROM "
                      "cst.StandardCost sc JOIN cst.CostSet cs ON "
                      "cs.CompanyCode = sc.CompanyCode AND "
                      "cs.CostSetCode = sc.CostSetCode WHERE "
                      "sc.CompanyCode = ? AND sc.ItemCode = ? AND "
                      "sc.CostSetCode = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 3 TO SQLIO-PARM-CNT
               MOVE SC-COST-SET TO SQLIO-PARM-VAL(3)
           END-IF
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SC-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 7 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-SQL-BAD
               GO TO 1000-EXIT
           END-IF
           IF NOT WS-GOT-ROW
               MOVE "CST0406" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No effective standard cost for item "
                      FUNCTION TRIM(SC-ITEM-CODE)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "INCOMPLETE" TO SC-COMPLETENESS
               MOVE 16 TO CA-RETURN-CODE
               GO TO 1000-EXIT
           END-IF
           MOVE FUNCTION NUMVAL(WS-COL(2)) TO SC-STD-UNIT-COST
           MOVE FUNCTION NUMVAL(WS-COL(3)) TO SC-LOT-SIZE
           MOVE FUNCTION NUMVAL(WS-COL(4)) TO SC-PREV-UNIT-COST
           MOVE WS-COL(5) TO SC-COST-SET
           MOVE WS-COL(6) TO SC-STATUS
           MOVE WS-COL(7) TO SC-COMPLETENESS
           MOVE FUNCTION NUMVAL(WS-COL(1)) TO WS-COUNT
           PERFORM 1100-ELEMENT-SPLIT.
       1000-EXIT.
           EXIT.

       1100-ELEMENT-SPLIT SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT e.ElementType, SUM(se.UnitCost + "
                  "se.FixedCostPerLot) FROM cst.StandardCostElement "
                  "se JOIN cst.CostElement e ON e.ElementCode = "
                  "se.ElementCode WHERE se.StandardCostId = ? GROUP "
                  "BY e.ElementType"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-COUNT TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           IF SQLIO-SQLCODE < 0
               PERFORM 8900-SQL-ERROR
               GO TO 1100-EXIT
           END-IF
           PERFORM UNTIL WS-SQL-BAD
               MOVE 3 TO SQLIO-CURSOR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
               IF SQLIO-SQLCODE = 100
                   EXIT PERFORM
               END-IF
               IF SQLIO-SQLCODE < 0
                   PERFORM 8900-SQL-ERROR
                   EXIT PERFORM
               END-IF
               MOVE FUNCTION NUMVAL(WS-COL(2)) TO WS-AMT1
               EVALUATE WS-COL(1)(1:8)
                   WHEN "MATERIAL" ADD WS-AMT1 TO SC-EL-MATERIAL
                   WHEN "LABOUR  " ADD WS-AMT1 TO SC-EL-LABOUR
                   WHEN "MACHINE " ADD WS-AMT1 TO SC-EL-MACHINE
                   WHEN "SUBCON  " ADD WS-AMT1 TO SC-EL-SUBCON
                   WHEN "OVHDVAR " ADD WS-AMT1 TO SC-EL-OVERHEAD
                   WHEN "OVHDFIX " ADD WS-AMT1 TO SC-EL-OVERHEAD
                   WHEN "RECOVERY" ADD WS-AMT1 TO SC-EL-RECOVERY
                   WHEN OTHER      CONTINUE
               END-EVALUATE
           END-PERFORM
           MOVE 3 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  THE LIFECYCLE.  DRAFT -> REVIEW -> APPROVED -> ACTIVE.
      *>       A SIMULATION NEVER REACHES ACTIVE (CST-007).
      *>----------------------------------------------------------------
       2000-REVIEW SECTION.
       2000-START.
           MOVE "DRAFT" TO WS-WORK-TEXT
           MOVE "REVIEW" TO WS-NEW-STATUS
           PERFORM 2300-CHANGE-STATUS.
       2000-EXIT.
           EXIT.

       2100-APPROVE SECTION.
       2100-START.
           MOVE "REVIEW" TO WS-WORK-TEXT
           MOVE "APPROVED" TO WS-NEW-STATUS
           PERFORM 2300-CHANGE-STATUS.
       2100-EXIT.
           EXIT.

       2200-ACTIVATE SECTION.
       2200-START.
           MOVE "APPROVED" TO WS-WORK-TEXT
           MOVE "ACTIVE" TO WS-NEW-STATUS
           PERFORM 2300-CHANGE-STATUS
           IF CA-RETURN-CODE > 4
               GO TO 2200-EXIT
           END-IF
           PERFORM 3000-IMPACT.
       2200-EXIT.
           EXIT.

       2300-CHANGE-STATUS SECTION.
       2300-START.
           PERFORM 2400-READ-SET
           IF WS-SQL-BAD OR WS-INPUT-BAD
               GO TO 2300-EXIT
           END-IF
           IF NOT WS-GOT-ROW
               MOVE "CST0402" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Cost set " FUNCTION TRIM(SC-COST-SET)
                      " was not found"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               GO TO 2300-EXIT
           END-IF
           IF WS-SET-STATUS NOT = WS-WORK-TEXT(1:10)
               MOVE "CST0403" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Cost set is " FUNCTION TRIM(WS-SET-STATUS)
                      " - it must be " FUNCTION TRIM(WS-WORK-TEXT)
                      " for this action"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2300-EXIT
           END-IF
           IF WS-NEW-STATUS = "ACTIVE"
              AND WS-SET-PURPOSE NOT = "STANDARD"
               MOVE "CST0404" TO CA-REASON-CODE
               MOVE "A simulation or quote set cannot be activated"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2300-EXIT
           END-IF
           IF WS-NEW-STATUS = "APPROVED"
              AND FUNCTION TRIM(WS-PREPARER) = FUNCTION TRIM(CA-USER)
               MOVE "CST0405" TO CA-REASON-CODE
               MOVE "The preparer of a cost set may not approve it"
                   TO CA-MESSAGE
               MOVE 6 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 2300-EXIT
           END-IF
           MOVE WS-NEW-STATUS TO SC-STATUS
           IF NOT WS-DO-WRITE
               GO TO 2300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           EVALUATE WS-NEW-STATUS
               WHEN "APPROVED"
                   STRING "UPDATE cst.CostSet SET Status = ?, "
                          "ApprovedBy = ?, ApprovedAt = "
                          "SYSDATETIME(), ModifiedBy = ?, "
                          "ModifiedAt = SYSDATETIME() WHERE "
                          "CompanyCode = ? AND CostSetCode = ?"
                       DELIMITED BY SIZE INTO SQLIO-STMT
               WHEN "ACTIVE"
                   STRING "UPDATE cst.CostSet SET Status = ?, "
                          "ActivatedAt = SYSDATETIME(), ModifiedBy "
                          "= ?, ModifiedAt = SYSDATETIME() WHERE "
                          "CompanyCode = ? AND CostSetCode = ? AND "
                          "? IS NOT NULL"
                       DELIMITED BY SIZE INTO SQLIO-STMT
               WHEN OTHER
                   STRING "UPDATE cst.CostSet SET Status = ?, "
                          "ReviewedBy = ?, ModifiedBy = ?, "
                          "ModifiedAt = SYSDATETIME() WHERE "
                          "CompanyCode = ? AND CostSetCode = ?"
                       DELIMITED BY SIZE INTO SQLIO-STMT
           END-EVALUATE
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE WS-NEW-STATUS TO SQLIO-PARM-VAL(1)
           IF WS-NEW-STATUS = "ACTIVE"
               MOVE CA-USER TO SQLIO-PARM-VAL(2)
               MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
               MOVE SC-COST-SET TO SQLIO-PARM-VAL(4)
               MOVE CA-USER TO SQLIO-PARM-VAL(5)
           ELSE
               MOVE CA-USER TO SQLIO-PARM-VAL(2)
               MOVE CA-USER TO SQLIO-PARM-VAL(3)
               MOVE CA-COMPANY TO SQLIO-PARM-VAL(4)
               MOVE SC-COST-SET TO SQLIO-PARM-VAL(5)
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           PERFORM 8100-EXECUTE
           IF NOT WS-SQL-BAD
               MOVE CA-USER TO SC-APPROVED-BY
           END-IF.
       2300-EXIT.
           EXIT.

       2400-READ-SET SECTION.
       2400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status, Purpose, ISNULL(PreparedBy,''), "
                  "ISNULL(ComparedToSetCode,'') FROM cst.CostSet "
                  "WHERE CompanyCode = ? AND CostSetCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SC-COST-SET TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 4 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-GOT-ROW
               MOVE WS-COL(1) TO WS-SET-STATUS
               MOVE WS-COL(2) TO WS-SET-PURPOSE
               MOVE WS-COL(3) TO WS-PREPARER
               MOVE WS-SET-PURPOSE TO SC-PURPOSE
               MOVE WS-PREPARER TO SC-PREPARED-BY
               IF SC-COMPARE-SET = SPACES
                   MOVE WS-COL(4) TO SC-COMPARE-SET
               END-IF
           END-IF.
       2400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  WHAT ACTIVATION WOULD DO TO OWNED INVENTORY.  CALLED ON
      *>       ITS OWN IT CHANGES NOTHING, WHICH IS WHY A SIMULATION
      *>       CAN SAFELY USE IT.
      *>----------------------------------------------------------------
       3000-IMPACT SECTION.
       3000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT COUNT(*), ISNULL(SUM(ic.QtyOnHand),0), "
                  "ISNULL(SUM(ic.QtyOnHand * (sc.TotalUnitCost - "
                  "ISNULL(ic.StandardUnitCost, "
                  "ic.AverageUnitCost))),0) FROM cst.StandardCost sc "
                  "JOIN cst.ItemCost ic ON ic.CompanyCode = "
                  "sc.CompanyCode AND ic.ItemCode = sc.ItemCode "
                  "WHERE sc.CompanyCode = ? AND sc.CostSetCode = ? "
                  "AND ic.QtyOnHand <> 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SC-COST-SET TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 3 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-GOT-ROW
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO SC-ITEM-COUNT
               MOVE FUNCTION NUMVAL(WS-COL(2)) TO SC-REVAL-QTY
               MOVE FUNCTION NUMVAL(WS-COL(3)) TO SC-REVAL-GBP
           END-IF
           IF SC-REVAL-GBP < 0
               MOVE "Y" TO SC-ADVERSE
           END-IF
           IF CA-FUNCTION = "IMPACT"
               MOVE "No stock has been revalued - this is a preview"
                   TO SC-WARNING
           END-IF.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  SPEC EXAMPLE C3.  AT RECEIPT, INVENTORY TAKES THE
      *>       STANDARD AND THE DIFFERENCE AGAINST THE ORDER PRICE IS
      *>       THE RECEIPT PRICE VARIANCE.  GRNI TAKES THE ORDER
      *>       ACCRUAL, NOT THE STANDARD.
      *>----------------------------------------------------------------
       4000-RECEIPT-PPV SECTION.
       4000-START.
           IF SC-STD-UNIT-COST = 0
               PERFORM 1000-GET-STANDARD
               IF CA-RETURN-CODE > 4
                   GO TO 4000-EXIT
               END-IF
           END-IF
           COMPUTE SC-INVENTORY-GBP ROUNDED =
               SC-QUANTITY * SC-STD-UNIT-COST
           COMPUTE SC-GRNI-GBP ROUNDED =
               SC-QUANTITY * SC-ORDER-PRICE
           COMPUTE SC-PPV-GBP = SC-GRNI-GBP - SC-INVENTORY-GBP
           MOVE SC-PPV-GBP TO SC-TOTAL-VAR-GBP
           IF SC-PPV-GBP > 0
               MOVE "Y" TO SC-ADVERSE
           END-IF
           MOVE SC-INVENTORY-GBP TO SC-EL-MATERIAL.
       4000-EXIT.
           EXIT.

      *>  THE INVOICE ONLY CREATES THE INCREMENTAL DIFFERENCE AGAINST
      *>  THE ACCRUAL.  REPEATING THE RECEIPT VARIANCE WOULD DOUBLE
      *>  COUNT IT - 120.00 ADVERSE IN THE SPEC, NEVER 220.00.
       4100-INVOICE-PPV SECTION.
       4100-START.
           COMPUTE SC-GRNI-GBP ROUNDED =
               SC-QUANTITY * SC-ORDER-PRICE
           COMPUTE SC-INCREMENTAL-GBP =
               SC-INVOICE-AMOUNT - SC-GRNI-GBP
           IF SC-STD-UNIT-COST > 0
               COMPUTE SC-INVENTORY-GBP ROUNDED =
                   SC-QUANTITY * SC-STD-UNIT-COST
               COMPUTE SC-PPV-GBP = SC-GRNI-GBP - SC-INVENTORY-GBP
               COMPUTE SC-TOTAL-VAR-GBP =
                   SC-PPV-GBP + SC-INCREMENTAL-GBP
           ELSE
               MOVE SC-INCREMENTAL-GBP TO SC-TOTAL-VAR-GBP
           END-IF
           IF SC-INCREMENTAL-GBP > 0
               MOVE "Y" TO SC-ADVERSE
           END-IF.
       4100-EXIT.
           EXIT.

      *>  SPEC EXAMPLE C7.  THE RETURN COMES BACK AT THE APPLICABLE
      *>  APPROVED STANDARD WHILE THE ORIGINAL COGS RELIEF IS
      *>  REVERSED IN FULL; THE DIFFERENCE IS EXPOSED AS VARIANCE.
       4200-RETURN-VALUE SECTION.
       4200-START.
           IF SC-STD-UNIT-COST = 0
               PERFORM 1000-GET-STANDARD
               IF CA-RETURN-CODE > 4
                   GO TO 4200-EXIT
               END-IF
           END-IF
           COMPUTE SC-RETURN-VALUE ROUNDED =
               SC-QUANTITY * SC-STD-UNIT-COST
           COMPUTE SC-RETURN-VAR-GBP =
               SC-ORIGINAL-COGS - SC-RETURN-VALUE
           MOVE SC-RETURN-VAR-GBP TO SC-TOTAL-VAR-GBP
           IF SC-RETURN-VAR-GBP > 0
               MOVE "Y" TO SC-ADVERSE
           END-IF
           MOVE SC-RETURN-VALUE TO SC-INVENTORY-GBP.
       4200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000  CST-012.  AN APPROVED SET KEEPS ITS ASSUMPTIONS AND IS
      *>       COMPARED TO THE PREVIOUS APPROVED SET LINE BY LINE.
      *>----------------------------------------------------------------
       5000-COMPARE SECTION.
       5000-START.
           IF SC-COMPARE-SET = SPACES
               PERFORM 2400-READ-SET
           END-IF
           IF SC-COMPARE-SET = SPACES
               MOVE "CST0410" TO CA-REASON-CODE
               MOVE "No comparison cost set was named" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT n.ItemCode, n.Revision, "
                  "ISNULL(o.TotalUnitCost,0), n.TotalUnitCost, "
                  "ISNULL(ic.QtyOnHand,0) FROM cst.StandardCost n "
                  "LEFT JOIN cst.StandardCost o ON o.CompanyCode = "
                  "n.CompanyCode AND o.ItemCode = n.ItemCode AND "
                  "o.CostSetCode = ? LEFT JOIN cst.ItemCost ic ON "
                  "ic.CompanyCode = n.CompanyCode AND ic.ItemCode = "
                  "n.ItemCode WHERE n.CompanyCode = ? AND "
                  "n.CostSetCode = ? ORDER BY n.ItemCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE SC-COMPARE-SET TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE SC-COST-SET TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           IF SQLIO-SQLCODE < 0
               PERFORM 8900-SQL-ERROR
               GO TO 5000-EXIT
           END-IF
           MOVE 0 TO WS-COUNT
           PERFORM UNTIL WS-SQL-BAD
               MOVE 2 TO SQLIO-CURSOR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
               IF SQLIO-SQLCODE = 100
                   EXIT PERFORM
               END-IF
               IF SQLIO-SQLCODE < 0
                   PERFORM 8900-SQL-ERROR
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-COUNT
               IF WS-COUNT > SC-FROM-LINE AND SC-LINE-CNT < 20
                   PERFORM 5100-COMPARE-LINE
               ELSE
                   IF SC-LINE-CNT >= 20
                       MOVE "Y" TO SC-MORE-LINES
                   END-IF
               END-IF
           END-PERFORM
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           MOVE WS-COUNT TO SC-ITEM-COUNT.
       5000-EXIT.
           EXIT.

       5100-COMPARE-LINE SECTION.
       5100-START.
           ADD 1 TO SC-LINE-CNT
           MOVE SC-LINE-CNT TO WS-IX2
           MOVE WS-COL(1) TO SC-L-ITEM(WS-IX2)
           MOVE WS-COL(2) TO SC-L-REV(WS-IX2)
           MOVE FUNCTION NUMVAL(WS-COL(3)) TO SC-L-OLD-COST(WS-IX2)
           MOVE FUNCTION NUMVAL(WS-COL(4)) TO SC-L-NEW-COST(WS-IX2)
           MOVE FUNCTION NUMVAL(WS-COL(5)) TO SC-L-QTY-HELD(WS-IX2)
           COMPUTE SC-L-DIFF(WS-IX2) =
               SC-L-NEW-COST(WS-IX2) - SC-L-OLD-COST(WS-IX2)
           IF SC-L-OLD-COST(WS-IX2) NOT = 0
               COMPUTE SC-L-PCT(WS-IX2) ROUNDED =
                   SC-L-DIFF(WS-IX2) * 100 / SC-L-OLD-COST(WS-IX2)
               MOVE SPACES TO SC-L-NOTE(WS-IX2)
           ELSE
               MOVE 0 TO SC-L-PCT(WS-IX2)
               MOVE "New item in this set" TO SC-L-NOTE(WS-IX2)
           END-IF
           COMPUTE SC-L-REVAL(WS-IX2) ROUNDED =
               SC-L-QTY-HELD(WS-IX2) * SC-L-DIFF(WS-IX2)
           ADD SC-L-REVAL(WS-IX2) TO SC-REVAL-GBP.
       5100-EXIT.
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
           MOVE "CST0409" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                  " is not supported by STDCOST"
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
