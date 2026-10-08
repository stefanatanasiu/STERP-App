      *>================================================================
      *> PROGRAM-ID : ITMIMPCT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE
      *> PURPOSE    : LIFECYCLE CHANGE IMPACT REVIEW.  BEFORE AN ITEM
      *>              IS BLOCKED, MADE OBSOLETE, SUPERSEDED OR
      *>              RETIRED, AND BEFORE A PROTECTED FINANCIAL
      *>              ATTRIBUTE IS CHANGED, THE USER SEES THE STOCK,
      *>              RESERVATIONS, OPEN ORDERS, ACTIVE BILLS,
      *>              RELEASED KITS AND QUALITY OBLIGATIONS THAT STILL
      *>              DEPEND ON IT.  A REFERENCED ITEM IS RETIRED,
      *>              NEVER DELETED; DELETION IS RESTRICTED TO AN
      *>              UNUSED DRAFT.
      *> CALLED BY  : ITEM MASTER MAINTENANCE, THE .NET CLIENT
      *> INTERFACE  : CALL "ITMIMPCT" USING WS-ITMIMPCT-CA
      *>              COPYBOOK ITMIMPCTCA
      *> CALLS      : SQLIO -> inv.usp_ItemImpact, inv.Item,
      *>              inv.ItemRevision; ITMAUDIT
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-11-12 RJH  CR0488 ORIGINAL - OBSOLESCENCE CONTROL
      *> 2002-02-28 DJM  CR1051 WHERE USED REVIEW BEFORE BLOCKING
      *> 2011-04-05 ANW  CR2530 PROTECTED ATTRIBUTES AFTER HISTORY
      *> 2026-10-05 STERP ITM-010, ITM-020, ITM-022
      *>================================================================
      *> REQ: ITM-010, ITM-020, ITM-022
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ITMIMPCT.
       AUTHOR. R J HOLLIS.
       DATE-WRITTEN. 12/11/1996.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ITMWORK.
       COPY ITMAUDTW.
       01  WS-CONTROL.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-LIFECYCLE            PIC X(10) VALUE SPACES.
           05  WS-ITEM-CLASS           PIC X(12) VALUE SPACES.
           05  WS-VALUATION            PIC X(20) VALUE SPACES.
           05  WS-COST-METHOD          PIC X(4) VALUE SPACES.
           05  WS-STOCK-UOM            PIC X(10) VALUE SPACES.
           05  WS-TRACE                PIC X(8) VALUE SPACES.
           05  WS-DESCRIPTION          PIC X(100) VALUE SPACES.
           05  WS-OWN-BOMS             PIC S9(9) VALUE 0.
           05  WS-COUNT-LINES          PIC S9(9) VALUE 0.
           05  WS-LIVE-SERIALS         PIC S9(9) VALUE 0.
           05  WS-AREA                 PIC X(16) VALUE SPACES.
           05  WS-SEVERITY             PIC X(8) VALUE SPACES.
           05  WS-COUNT                PIC S9(9) VALUE 0.
           05  WS-LQTY                 PIC S9(13)V9(6) VALUE 0.
       LINKAGE SECTION.
       COPY ITMIMPCTCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 1000-GATHER-IMPACT
           IF CA-FAILED
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "IMPACT"   PERFORM 2000-REPORT-IMPACT
               WHEN "BLOCK"    PERFORM 3000-BLOCK
               WHEN "OBSOLETE" PERFORM 4000-OBSOLETE
               WHEN "SUCCEED"  PERFORM 5000-SUCCESSOR
               WHEN "RETIRE"   PERFORM 6000-RETIRE
               WHEN "DELCHECK" PERFORM 7000-DELETE-CHECK
               WHEN "PROTECT"  PERFORM 8000-PROTECTED-CHANGE
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-SQL-FAILED
           MOVE "ITMIMPCT" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "ITM-020" TO CA-REQ-REF
           MOVE 0 TO IM-QTY-ON-HAND IM-QTY-INTRANSIT IM-QTY-RESERVED
           MOVE 0 TO IM-MOVEMENT-CNT IM-OPEN-SO-CNT IM-OPEN-PO-CNT
           MOVE 0 TO IM-BOM-CNT IM-KIT-CNT IM-SUBST-CNT IM-HOLD-CNT
           MOVE 0 TO IM-LINE-CNT
           MOVE "N" TO IM-BLOCKING IM-ALLOWED IM-PROTECTED
           MOVE SPACES TO IM-TREATMENT IM-OLD-VALUE
           MOVE FUNCTION UPPER-CASE(IM-ITEM-CODE) TO IM-ITEM-CODE
           MOVE FUNCTION UPPER-CASE(IM-SUCCESSOR) TO IM-SUCCESSOR
           IF IM-SCOPE = SPACES
               MOVE "ALL" TO IM-SCOPE
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES OR IM-ITEM-CODE = SPACES
               MOVE "IMP0001" TO CA-REASON-CODE
               MOVE "A company and an item are required" TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  EVERYTHING THAT STILL POINTS AT THIS ITEM.
      *>----------------------------------------------------------------
       1000-GATHER-IMPACT SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC inv.usp_ItemImpact ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE IM-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE IM-REVISION TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 21 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "IMP0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(IM-ITEM-CODE)
                      " is not held in company "
                      FUNCTION TRIM(CA-COMPANY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               MOVE "Y" TO WS-ERROR
               ADD 1 TO CA-MSG-COUNT
               GO TO 1000-EXIT
           END-IF
           COMPUTE IM-QTY-ON-HAND = FUNCTION NUMVAL(ITM-COL(1))
           COMPUTE IM-QTY-INTRANSIT = FUNCTION NUMVAL(ITM-COL(2))
           COMPUTE IM-QTY-RESERVED = FUNCTION NUMVAL(ITM-COL(3))
           COMPUTE IM-MOVEMENT-CNT = FUNCTION NUMVAL(ITM-COL(4))
           COMPUTE IM-OPEN-SO-CNT = FUNCTION NUMVAL(ITM-COL(5))
           COMPUTE IM-OPEN-PO-CNT = FUNCTION NUMVAL(ITM-COL(6))
           COMPUTE IM-BOM-CNT = FUNCTION NUMVAL(ITM-COL(7))
           COMPUTE WS-OWN-BOMS = FUNCTION NUMVAL(ITM-COL(8))
           COMPUTE IM-KIT-CNT = FUNCTION NUMVAL(ITM-COL(9))
           COMPUTE IM-SUBST-CNT = FUNCTION NUMVAL(ITM-COL(10))
           COMPUTE IM-HOLD-CNT = FUNCTION NUMVAL(ITM-COL(11))
           COMPUTE WS-COUNT-LINES = FUNCTION NUMVAL(ITM-COL(12))
           COMPUTE WS-LIVE-SERIALS = FUNCTION NUMVAL(ITM-COL(13))
           MOVE ITM-COL(14) TO WS-LIFECYCLE
           MOVE ITM-COL(15) TO WS-ITEM-CLASS
           MOVE ITM-COL(16) TO WS-VALUATION
           MOVE ITM-COL(17) TO WS-COST-METHOD
           MOVE ITM-COL(18) TO WS-STOCK-UOM
           MOVE ITM-COL(19) TO WS-TRACE
           MOVE ITM-COL(20) TO IM-SUCCESSOR
           MOVE ITM-COL(21) TO WS-DESCRIPTION
           PERFORM 1100-BUILD-LINES.
       1000-EXIT.
           EXIT.

       1100-BUILD-LINES SECTION.
       1100-START.
           MOVE 0 TO IM-LINE-CNT
           MOVE "N" TO IM-BLOCKING
           MOVE "STOCK" TO WS-AREA
           MOVE IM-QTY-ON-HAND TO WS-LQTY
           MOVE 0 TO WS-COUNT
           MOVE "Stock on hand must be run out, transferred or "
               TO WS-TEXT
           MOVE SPACES TO WS-TEXT2
           STRING FUNCTION TRIM(WS-TEXT) " disposed of"
               DELIMITED BY SIZE INTO WS-TEXT2
           MOVE WS-TEXT2 TO WS-TEXT
           IF IM-QTY-ON-HAND > 0
               MOVE "WARNING" TO WS-SEVERITY
           ELSE
               MOVE "INFO" TO WS-SEVERITY
           END-IF
           PERFORM 1150-ADD-LINE
           MOVE "INTRANSIT" TO WS-AREA
           MOVE IM-QTY-INTRANSIT TO WS-LQTY
           MOVE "Stock in transit arrives after the change"
               TO WS-TEXT
           IF IM-QTY-INTRANSIT > 0
               MOVE "WARNING" TO WS-SEVERITY
           ELSE
               MOVE "INFO" TO WS-SEVERITY
           END-IF
           PERFORM 1150-ADD-LINE
           MOVE "RESERVATION" TO WS-AREA
           MOVE IM-QTY-RESERVED TO WS-LQTY
           MOVE "Active reservations must be reallocated"
               TO WS-TEXT
           IF IM-QTY-RESERVED > 0
               MOVE "ERROR" TO WS-SEVERITY
               MOVE "Y" TO IM-BLOCKING
           ELSE
               MOVE "INFO" TO WS-SEVERITY
           END-IF
           PERFORM 1150-ADD-LINE
           MOVE "SALESORDER" TO WS-AREA
           MOVE 0 TO WS-LQTY
           MOVE IM-OPEN-SO-CNT TO WS-COUNT
           MOVE "Outstanding sales order lines" TO WS-TEXT
           IF IM-OPEN-SO-CNT > 0
               MOVE "ERROR" TO WS-SEVERITY
               MOVE "Y" TO IM-BLOCKING
           ELSE
               MOVE "INFO" TO WS-SEVERITY
           END-IF
           PERFORM 1150-ADD-LINE
           MOVE "PURCHASEORDER" TO WS-AREA
           MOVE IM-OPEN-PO-CNT TO WS-COUNT
           MOVE "Outstanding purchase order lines" TO WS-TEXT
           IF IM-OPEN-PO-CNT > 0
               MOVE "ERROR" TO WS-SEVERITY
               MOVE "Y" TO IM-BLOCKING
           ELSE
               MOVE "INFO" TO WS-SEVERITY
           END-IF
           PERFORM 1150-ADD-LINE
           MOVE "BOM" TO WS-AREA
           MOVE IM-BOM-CNT TO WS-COUNT
           MOVE "Released bills of material use this item"
               TO WS-TEXT
           IF IM-BOM-CNT > 0
               MOVE "WARNING" TO WS-SEVERITY
           ELSE
               MOVE "INFO" TO WS-SEVERITY
           END-IF
           PERFORM 1150-ADD-LINE
           MOVE "KIT" TO WS-AREA
           MOVE IM-KIT-CNT TO WS-COUNT
           MOVE "Released kits still need this component"
               TO WS-TEXT
           IF IM-KIT-CNT > 0
               MOVE "ERROR" TO WS-SEVERITY
               MOVE "Y" TO IM-BLOCKING
           ELSE
               MOVE "INFO" TO WS-SEVERITY
           END-IF
           PERFORM 1150-ADD-LINE
           MOVE "QUALITYHOLD" TO WS-AREA
           MOVE IM-HOLD-CNT TO WS-COUNT
           MOVE "Unresolved quality holds" TO WS-TEXT
           IF IM-HOLD-CNT > 0
               MOVE "ERROR" TO WS-SEVERITY
               MOVE "Y" TO IM-BLOCKING
           ELSE
               MOVE "INFO" TO WS-SEVERITY
           END-IF
           PERFORM 1150-ADD-LINE
           MOVE "COUNT" TO WS-AREA
           MOVE WS-COUNT-LINES TO WS-COUNT
           MOVE "Stock count lines are still open" TO WS-TEXT
           IF WS-COUNT-LINES > 0
               MOVE "WARNING" TO WS-SEVERITY
           ELSE
               MOVE "INFO" TO WS-SEVERITY
           END-IF
           PERFORM 1150-ADD-LINE
           MOVE "SERIAL" TO WS-AREA
           MOVE WS-LIVE-SERIALS TO WS-COUNT
           MOVE "Serial identities are still live" TO WS-TEXT
           IF WS-LIVE-SERIALS > 0
               MOVE "WARNING" TO WS-SEVERITY
           ELSE
               MOVE "INFO" TO WS-SEVERITY
           END-IF
           PERFORM 1150-ADD-LINE
           MOVE "HISTORY" TO WS-AREA
           MOVE IM-MOVEMENT-CNT TO WS-COUNT
           MOVE "Posted stock movements reference this item"
               TO WS-TEXT
           MOVE "INFO" TO WS-SEVERITY
           PERFORM 1150-ADD-LINE
           MOVE "SUBSTITUTE" TO WS-AREA
           MOVE IM-SUBST-CNT TO WS-COUNT
           MOVE "Substitution rules name this item" TO WS-TEXT
           MOVE "INFO" TO WS-SEVERITY
           PERFORM 1150-ADD-LINE.
       1100-EXIT.
           EXIT.

       1150-ADD-LINE SECTION.
       1150-START.
           IF IM-LINE-CNT < 24
               ADD 1 TO IM-LINE-CNT
               MOVE WS-AREA TO IM-L-AREA(IM-LINE-CNT)
               MOVE WS-SEVERITY TO IM-L-SEVERITY(IM-LINE-CNT)
               MOVE WS-COUNT TO IM-L-COUNT(IM-LINE-CNT)
               MOVE WS-LQTY TO IM-L-QTY(IM-LINE-CNT)
               MOVE IM-ITEM-CODE TO IM-L-REF(IM-LINE-CNT)
               MOVE WS-TEXT TO IM-L-TEXT(IM-LINE-CNT)
           END-IF
           MOVE 0 TO WS-COUNT WS-LQTY
           MOVE SPACES TO WS-TEXT.
       1150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       2000-REPORT-IMPACT SECTION.
       2000-START.
           IF IM-BLOCKING = "Y"
               MOVE 4 TO CA-RETURN-CODE
               MOVE "IMP0002" TO CA-REASON-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF
           MOVE IM-QTY-ON-HAND TO WS-QTY-ED
           MOVE SPACES TO WS-TEXT
           STRING "On hand " FUNCTION TRIM(WS-QTY-ED)
                  ", open orders "
               DELIMITED BY SIZE INTO WS-TEXT
           COMPUTE WS-COUNT = IM-OPEN-SO-CNT + IM-OPEN-PO-CNT
           MOVE WS-COUNT TO WS-INT-ED
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(WS-TEXT) FUNCTION TRIM(WS-INT-ED)
                  ", bills " DELIMITED BY SIZE INTO CA-MESSAGE
           MOVE IM-BOM-CNT TO WS-INT-ED
           MOVE SPACES TO WS-TEXT
           STRING FUNCTION TRIM(CA-MESSAGE) FUNCTION TRIM(WS-INT-ED)
                  ", kits " DELIMITED BY SIZE INTO WS-TEXT
           MOVE IM-KIT-CNT TO WS-INT-ED
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(WS-TEXT) FUNCTION TRIM(WS-INT-ED)
               DELIMITED BY SIZE INTO CA-MESSAGE
           MOVE "Y" TO IM-ALLOWED.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  BLOCK.  EXISTING OBLIGATIONS ARE NOT CANCELLED; THEY
      *>       NEED AN AUTHORISED RESOLUTION.  NEW USE IS PREVENTED.
      *>----------------------------------------------------------------
       3000-BLOCK SECTION.
       3000-START.
           IF IM-REASON = SPACES
               MOVE "IMP0003" TO CA-REASON-CODE
               MOVE "A block must record why it was applied"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           MOVE "BLOCKED" TO IM-NEW-STATUS
           PERFORM 9400-SET-STATUS
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           MOVE "Y" TO IM-ALLOWED
           MOVE SPACES TO CA-MESSAGE
           STRING "Item " FUNCTION TRIM(IM-ITEM-CODE)
                  " is blocked for " FUNCTION TRIM(IM-SCOPE)
                  "; outstanding obligations still require an "
                  "authorised resolution"
               DELIMITED BY SIZE INTO CA-MESSAGE
           IF IM-BLOCKING = "Y"
               MOVE 4 TO CA-RETURN-CODE
               MOVE "IMP0002" TO CA-REASON-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  OBSOLETE / RUN OUT.  A DISPOSITION IS MANDATORY WHEN
      *>       THERE IS STILL STOCK TO DEPLETE.
      *>----------------------------------------------------------------
       4000-OBSOLETE SECTION.
       4000-START.
           IF IM-QTY-ON-HAND > 0 AND IM-DISPOSITION = SPACES
               MOVE "IMP0003" TO CA-REASON-CODE
               MOVE IM-QTY-ON-HAND TO WS-QTY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "There is still " FUNCTION TRIM(WS-QTY-ED)
                      " in stock: record the approved run out, "
                      "substitution or disposal plan first"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 4000-EXIT
           END-IF
           MOVE "OBSOLETE" TO IM-NEW-STATUS
           PERFORM 9400-SET-STATUS
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           MOVE "Y" TO IM-ALLOWED
           MOVE IM-QTY-ON-HAND TO WS-QTY-ED
           MOVE SPACES TO CA-MESSAGE
           STRING "Item " FUNCTION TRIM(IM-ITEM-CODE)
                  " is obsolete; " FUNCTION TRIM(WS-QTY-ED)
                  " may be depleted under the approved disposition"
               DELIMITED BY SIZE INTO CA-MESSAGE
           IF IM-BLOCKING = "Y"
               MOVE 4 TO CA-RETURN-CODE
               MOVE "IMP0002" TO CA-REASON-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       5000-SUCCESSOR SECTION.
       5000-START.
           IF IM-SUCCESSOR = SPACES
               MOVE "IMP0006" TO CA-REASON-CODE
               MOVE "Name the successor item" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT LifecycleStatus FROM inv.Item WHERE "
                  "CompanyCode = ? AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE IM-SUCCESSOR TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 5000-EXIT
           END-IF
           IF SQLIO-NOTFOUND OR ITM-COL(1) NOT = "RELEASED"
               MOVE "IMP0006" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Successor " FUNCTION TRIM(IM-SUCCESSOR)
                      " must be a released item"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE inv.Item SET SuccessorItemCode = ?, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() WHERE "
                  "CompanyCode = ? AND ItemCode = ?; UPDATE inv.Item"
                  " SET PredecessorItemCode = ?, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE CompanyCode = ? "
                  "AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 8 TO SQLIO-PARM-CNT
           MOVE IM-SUCCESSOR TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE IM-ITEM-CODE TO SQLIO-PARM-VAL(4)
           MOVE IM-ITEM-CODE TO SQLIO-PARM-VAL(5)
           MOVE CA-USER TO SQLIO-PARM-VAL(6)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(7)
           MOVE IM-SUCCESSOR TO SQLIO-PARM-VAL(8)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 8
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 5000-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           PERFORM 9450-AUDIT-CHANGE
           MOVE "Y" TO IM-ALLOWED
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(IM-SUCCESSOR) " succeeds "
                  FUNCTION TRIM(IM-ITEM-CODE)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       5000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 6000  RETIRE, NEVER DELETE (ITM-022).
      *>----------------------------------------------------------------
       6000-RETIRE SECTION.
       6000-START.
           IF IM-QTY-ON-HAND > 0 OR IM-QTY-INTRANSIT > 0
               MOVE "IMP0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(IM-ITEM-CODE)
                      " still holds stock and cannot be retired "
                      "until the balance is cleared"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 6000-EXIT
           END-IF
           IF IM-BLOCKING = "Y"
               MOVE "IMP0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Open obligations prevent retirement; resolve"
                      " the orders, kits and holds listed first"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 6000-EXIT
           END-IF
           MOVE "RETIRED" TO IM-NEW-STATUS
           PERFORM 9400-SET-STATUS
           IF CA-FAILED
               GO TO 6000-EXIT
           END-IF
           MOVE "Y" TO IM-ALLOWED
           MOVE SPACES TO CA-MESSAGE
           STRING "Item " FUNCTION TRIM(IM-ITEM-CODE)
                  " is retired; history and enquiry remain, the "
                  "record is never deleted"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       6000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 7000  DELETION IS RESTRICTED TO AN UNUSED DRAFT (ITM-022).
      *>----------------------------------------------------------------
       7000-DELETE-CHECK SECTION.
       7000-START.
           MOVE "N" TO IM-ALLOWED
           IF WS-LIFECYCLE NOT = "DRAFT"
               MOVE "IMP0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Only an unused draft may be deleted; "
                      FUNCTION TRIM(IM-ITEM-CODE) " is "
                      FUNCTION TRIM(WS-LIFECYCLE)
                      " and must be retired instead"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 7000-EXIT
           END-IF
           IF IM-MOVEMENT-CNT > 0 OR IM-BOM-CNT > 0
              OR IM-KIT-CNT > 0 OR IM-OPEN-SO-CNT > 0
              OR IM-OPEN-PO-CNT > 0 OR IM-QTY-ON-HAND > 0
              OR IM-SUBST-CNT > 0 OR WS-OWN-BOMS > 0
               MOVE "IMP0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Draft " FUNCTION TRIM(IM-ITEM-CODE)
                      " is already referenced, so deleting it would"
                      " orphan those references; retire it instead"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 7000-EXIT
           END-IF
           MOVE "Y" TO IM-ALLOWED
           MOVE SPACES TO CA-MESSAGE
           STRING "Draft " FUNCTION TRIM(IM-ITEM-CODE)
                  " has no dependencies and may be deleted"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       7000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 8000  PROTECTED ATTRIBUTES (ITM-010).  CR2530: ONCE STOCK OR
      *>       HISTORY EXISTS, A CHANGE OF VALUATION GROUP, COST
      *>       METHOD, STOCK UNIT, CLASS OR TRACEABILITY IS NOT AN
      *>       ORDINARY FIELD EDIT.  IT NEEDS AN APPROVED CONVERSION
      *>       OR DISPOSITION PLAN AND A QUANTITY AND VALUE
      *>       RECONCILIATION, OR A NEW ITEM IDENTITY.
      *>----------------------------------------------------------------
       8000-PROTECTED-CHANGE SECTION.
       8000-START.
           MOVE "N" TO IM-PROTECTED
           MOVE "N" TO IM-ALLOWED
           MOVE FUNCTION UPPER-CASE(IM-FIELD-NAME) TO IM-FIELD-NAME
           EVALUATE IM-FIELD-NAME
               WHEN "VALUATIONGROUP"
                   MOVE WS-VALUATION TO IM-OLD-VALUE
                   MOVE "Y" TO IM-PROTECTED
               WHEN "COSTMETHOD"
                   MOVE WS-COST-METHOD TO IM-OLD-VALUE
                   MOVE "Y" TO IM-PROTECTED
               WHEN "STOCKUOMCODE"
                   MOVE WS-STOCK-UOM TO IM-OLD-VALUE
                   MOVE "Y" TO IM-PROTECTED
               WHEN "ITEMCLASS"
                   MOVE WS-ITEM-CLASS TO IM-OLD-VALUE
                   MOVE "Y" TO IM-PROTECTED
               WHEN "TRACEABILITYTYPE"
                   MOVE WS-TRACE TO IM-OLD-VALUE
                   MOVE "Y" TO IM-PROTECTED
               WHEN OTHER
                   MOVE "N" TO IM-PROTECTED
           END-EVALUATE
           IF IM-PROTECTED NOT = "Y"
               MOVE "Y" TO IM-ALLOWED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(IM-FIELD-NAME)
                      " is an ordinary descriptive attribute and "
                      "may be changed"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 8000-EXIT
           END-IF
           IF IM-QTY-ON-HAND = 0 AND IM-MOVEMENT-CNT = 0
               MOVE "Y" TO IM-ALLOWED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(IM-FIELD-NAME)
                      " may be changed: there is no stock and no "
                      "posted history to reconcile"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 8000-EXIT
           END-IF
           IF IM-APPROVAL-REF NOT = SPACES
              AND IM-DISPOSITION NOT = SPACES
               MOVE "Y" TO IM-ALLOWED
               MOVE 4 TO CA-RETURN-CODE
               MOVE "IMP0005" TO CA-REASON-CODE
               ADD 1 TO CA-MSG-COUNT
               MOVE IM-DISPOSITION TO IM-TREATMENT
               MOVE SPACES TO CA-MESSAGE
               STRING "Approved treatment "
                      FUNCTION TRIM(IM-APPROVAL-REF)
                      " permits the change; historic postings are "
                      "not restated"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 8000-EXIT
           END-IF
           MOVE IM-MOVEMENT-CNT TO WS-INT-ED
           MOVE SPACES TO IM-TREATMENT
           STRING "Approved conversion or disposition plan with a "
                  "quantity and value reconciliation, or a new item"
                  " identity"
               DELIMITED BY SIZE INTO IM-TREATMENT
           MOVE "IMP0005" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(IM-FIELD-NAME) " is protected: "
                  FUNCTION TRIM(WS-INT-ED)
                  " posted movement(s) exist, so the change needs "
                  "an approved treatment"
               DELIMITED BY SIZE INTO CA-MESSAGE
           MOVE 6 TO CA-RETURN-CODE
           MOVE "Y" TO WS-ERROR
           ADD 1 TO CA-MSG-COUNT.
       8000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE "N" TO IM-ALLOWED
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "IMP0008" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "ITMIMPCT does not support function "
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
               MOVE "SQL failure in ITMIMPCT" TO CA-MESSAGE
           END-IF
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE 12 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.

       9400-SET-STATUS SECTION.
       9400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE inv.Item SET LifecycleStatus = ?, "
                  "StatusReason = ?, ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHERE CompanyCode = ? AND ItemCode "
                  "= ? AND LifecycleStatus <> 'RETIRED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE IM-NEW-STATUS TO SQLIO-PARM-VAL(1)
           MOVE SPACES TO SQLIO-PARM-VAL(2)
           STRING FUNCTION TRIM(IM-REASON) " "
                  FUNCTION TRIM(IM-DISPOSITION)
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(4)
           MOVE IM-ITEM-CODE TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 9400-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           MOVE WS-LIFECYCLE TO IM-OLD-VALUE
           MOVE IM-NEW-STATUS TO IM-NEW-VALUE
           MOVE "LifecycleStatus" TO IM-FIELD-NAME
           PERFORM 9450-AUDIT-CHANGE.
       9400-EXIT.
           EXIT.

       9450-AUDIT-CHANGE SECTION.
       9450-START.
           INITIALIZE ITA-BLOCK
           MOVE CA-COMPANY TO ITA-COMPANY
           MOVE CA-USER TO ITA-USER
           MOVE "inv.Item" TO ITA-ENTITY-TYPE
           MOVE IM-ITEM-CODE TO ITA-ENTITY-KEY
           MOVE CA-FUNCTION TO ITA-ACTION
           MOVE IM-FIELD-NAME TO ITA-FIELD-NAME
           MOVE IM-OLD-VALUE TO ITA-OLD-VALUE
           MOVE IM-NEW-VALUE TO ITA-NEW-VALUE
           MOVE IM-REASON TO ITA-REASON
           MOVE IM-APPROVAL-REF TO ITA-AUTHORITY-REF
           MOVE CA-CORRELATION-ID TO ITA-CORRELATION
           CALL "ITMAUDIT" USING ITA-BLOCK.
       9450-EXIT.
           EXIT.
