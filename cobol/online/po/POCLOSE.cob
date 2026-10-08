      *>================================================================
      *> PROGRAM-ID : POCLOSE
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : CANCEL, SHORT CLOSE, CLOSE OR REOPEN A PURCHASE
      *>              ORDER LINE.  CLOSURE RELEASES THE REMAINING
      *>              COMMITMENT AND LEAVES EVERYTHING ALREADY
      *>              RECEIVED EXACTLY AS IT IS: THE GOODS ARE HERE,
      *>              THE LIABILITY IS REAL AND GOODS RECEIVED NOT
      *>              INVOICED IS NEVER CLEARED BY CLOSING AN ORDER.
      *>              EXPECTED SUPPLIER CREDITS STAY VISIBLE AND
      *>              CHASED AFTER CLOSURE.
      *> CALLED BY  : PURCHASE ORDER MAINTENANCE SCREEN
      *> INTERFACE  : CALL "POCLOSE" USING DFHCOMMAREA  (POCLOSECA)
      *> CALLS      : SQLIO -> po.PurchaseOrderLine, po.Commitment
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-10-07 RJH  CR0839 ORDER CLOSURE AND SHORT CLOSE
      *> 2007-03-19 DJM  CR1288 CLOSURE MUST NOT CLEAR GRNI
      *> 2026-10-06 STERP PO-020, AC-055
      *>================================================================
      *> REQ: PO-020, PO-017, PO-013
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POCLOSE.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 07/10/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY POACCNTS.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(60).
       01  WS-WORK.
           05  WS-QTY-ED               PIC -(9)9.999999.
           05  WS-AMT-ED               PIC -(11)9.99.
           05  WS-LINE-ED              PIC -(8)9.
       LINKAGE SECTION.
       COPY POCLOSECA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           PERFORM 2000-MEASURE THRU 2000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "CHECK"
                   PERFORM 2500-ADVISE THRU 2500-EXIT
               WHEN "CANCEL"
                   PERFORM 3000-CANCEL THRU 3000-EXIT
               WHEN "SHORTCLS"
                   PERFORM 3500-SHORT THRU 3500-EXIT
               WHEN "CLOSE"
                   PERFORM 4000-CLOSE THRU 4000-EXIT
               WHEN "REOPEN"
                   PERFORM 5000-REOPEN THRU 5000-EXIT
               WHEN OTHER
                   MOVE "PCL0006" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                          " is not supported by POCLOSE"
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
           MOVE "POCLOSE" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE 0 TO PC-OUTSTAND-QTY PC-RELEASED-QTY
           MOVE 0 TO PC-RELEASED-GBP PC-RETAINED-GRNI
           MOVE 0 TO PC-UNMATCHED-GBP PC-COMMIT-ROWS PC-WARN-COUNT
           MOVE "OPEN" TO PC-NEW-STATUS
           MOVE "OK" TO PC-OUTCOME
           IF PC-CLOSE-DATE = 0
               MOVE CA-BUSINESS-DATE TO PC-CLOSE-DATE
           END-IF
           IF CA-FUNCTION NOT = "CHECK" AND PC-REASON-CODE = SPACES
               MOVE "PCL0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Closing an order is a decision somebody "
                      "took; record the reason so that the next "
                      "buyer knows why we stopped buying it"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> WHAT IS ACTUALLY OUTSTANDING.  THE SAME ARITHMETIC AS TOLCHK
      *> AND THE SAME AS THE PIPELINE REPORT, SO THE SCREEN, THE
      *> REPORT AND THE COMMITMENT CANNOT DISAGREE:
      *>   outstanding = max(0, ordered - cancelled - received)
      *>----------------------------------------------------------------
       2000-MEASURE SECTION.
       2000-START.
           IF PC-ORDERED-QTY = 0
               PERFORM 1500-READ-LINE THRU 1500-EXIT
           END-IF
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           COMPUTE PC-OUTSTAND-QTY = PC-ORDERED-QTY
                   - PC-CANCELLED-QTY - PC-RECEIVED-QTY
           IF PC-OUTSTAND-QTY < 0
               MOVE 0 TO PC-OUTSTAND-QTY
           END-IF
           COMPUTE PC-RETAINED-GRNI = PC-RECEIVED-GBP
           COMPUTE PC-UNMATCHED-GBP =
                   PC-RECEIVED-GBP - PC-MATCHED-GBP
           IF PC-UNMATCHED-GBP < 0
               MOVE 0 TO PC-UNMATCHED-GBP
           END-IF.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1500-READ-LINE SECTION.
       1500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(OrderedQty AS varchar(40)) FROM "
                  "po.PurchaseOrderLine WHERE CompanyCode = ? AND "
                  "OrderNumber = ? AND LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE PC-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE PC-LINE-NUM TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 60 TO SQLIO-COL-WIDTH(1)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-SQLCODE NOT = 0 OR WS-C-VALUE = SPACES
               MOVE 16 TO CA-RETURN-CODE
               MOVE "PCL0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Purchase order line not found, or the order "
                      "is already closed"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 1500-EXIT
           END-IF
           MOVE FUNCTION NUMVAL(WS-C-VALUE) TO PC-ORDERED-QTY.
       1500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> WHAT WOULD HAPPEN.  USED BY THE SCREEN BEFORE THE BUYER
      *> COMMITS, SO THAT THE CONSEQUENCES ARE STATED BEFORE AND NOT
      *> DISCOVERED AFTERWARDS.
      *>----------------------------------------------------------------
       2500-ADVISE SECTION.
       2500-START.
           COMPUTE PC-RELEASED-QTY = PC-OUTSTAND-QTY
           COMPUTE PC-RELEASED-GBP ROUNDED =
                   PC-RELEASED-QTY * PC-UNIT-PRICE
           MOVE PC-RELEASED-GBP TO WS-AMT-ED
           MOVE SPACES TO CA-MESSAGE
           STRING "Closing releases " FUNCTION TRIM(WS-AMT-ED)
                  " of commitment; everything already received "
                  "stays on the ledger"
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 2600-WARNINGS THRU 2600-EXIT.
       2500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2600-WARNINGS SECTION.
       2600-START.
           IF PC-QUARANTINE-QTY > 0
               ADD 1 TO PC-WARN-COUNT
               MOVE 4 TO CA-RETURN-CODE
               MOVE "PCL0004" TO CA-REASON-CODE
               MOVE PC-QUARANTINE-QTY TO WS-QTY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(WS-QTY-ED)
                      " is still in quarantine awaiting inspection; "
                      "closing the order does not disposition it"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           IF PC-UNMATCHED-GBP > 0
               ADD 1 TO PC-WARN-COUNT
               MOVE 4 TO CA-RETURN-CODE
               MOVE "PCL0005" TO CA-REASON-CODE
               MOVE PC-UNMATCHED-GBP TO WS-AMT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(WS-AMT-ED)
                      " of received goods is not yet matched to a "
                      "supplier invoice; the accrual stays on the "
                      "ledger after closure"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           IF PC-OPEN-CREDIT-GBP > 0
               ADD 1 TO PC-WARN-COUNT
               MOVE 4 TO CA-RETURN-CODE
               MOVE "PCL0008" TO CA-REASON-CODE
               MOVE PC-OPEN-CREDIT-GBP TO WS-AMT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(WS-AMT-ED)
                      " of supplier credit is still expected and "
                      "remains visible and chased after closure"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       2600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> CANCELLATION BEFORE ANY RECEIPT.  ONCE GOODS HAVE ARRIVED A
      *> CANCELLATION WOULD PRETEND THE ORDER NEVER EXISTED, WHICH
      *> WOULD STRAND THE RECEIVED LIABILITY; A SHORT CLOSE IS THE
      *> HONEST INSTRUMENT.
      *>----------------------------------------------------------------
       3000-CANCEL SECTION.
       3000-START.
           IF PC-RECEIVED-QTY > 0 AND PC-FORCE NOT = "Y"
               MOVE "PCL0009" TO CA-REASON-CODE
               MOVE PC-RECEIVED-QTY TO WS-QTY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(WS-QTY-ED)
                      " has already been received against this "
                      "line, so it cannot be cancelled; short "
                      "close it instead so the received liability "
                      "survives"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           MOVE "CANCELLED" TO PC-NEW-STATUS
           PERFORM 3800-RELEASE THRU 3800-EXIT.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> SHORT CLOSE.  THE DELIVERED QUANTITY IS ACCEPTED AS FINAL
      *> AND THE REST OF THE COMMITMENT IS RELEASED.  THIS IS A
      *> DECISION WITH A MONEY CONSEQUENCE, SO IT NEEDS AN AUTHORITY.
      *>----------------------------------------------------------------
       3500-SHORT SECTION.
       3500-START.
           IF PC-AUTHORITY = SPACES
               MOVE "PCL0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A short close writes a quantity off the "
                      "order; name the person who authorised it"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3500-EXIT
           END-IF
           MOVE "SHORTCLOSED" TO PC-NEW-STATUS
           PERFORM 3800-RELEASE THRU 3800-EXIT.
       3500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> RELEASE THE REMAINING COMMITMENT.  po.Commitment HOLDS ONE
      *> ROW PER STAGE MOVEMENT, SO THE RELEASE IS AN ADDITIONAL ROW
      *> AND NOT AN EDIT OF THE ORIGINAL COMMITMENT - THE HISTORY OF
      *> WHAT WAS COMMITTED AND WHEN SURVIVES.
      *>----------------------------------------------------------------
       3800-RELEASE SECTION.
       3800-START.
           COMPUTE PC-RELEASED-QTY = PC-OUTSTAND-QTY
           COMPUTE PC-RELEASED-GBP ROUNDED =
                   PC-RELEASED-QTY * PC-UNIT-PRICE
           IF PC-RELEASED-QTY > 0
               MOVE SPACES TO SQLIO-STMT
               STRING "INSERT po.Commitment (CompanyCode, "
                      "OrderNumber, LineNum, Stage, MovementDate, "
                      "Quantity, AmountGBP, SourceDocType, "
                      "SourceDocRef, Narrative, CreatedBy) VALUES "
                      "(?,?,?,'RELEASED',?,?,?,'ORDERCLOSURE',?,?,"
                      "?)"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
               MOVE PC-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
               MOVE PC-LINE-NUM TO SQLIO-INT-EDIT
               MOVE FUNCTION TRIM(SQLIO-INT-EDIT)
                 TO SQLIO-PARM-VAL(3)
               MOVE PC-CLOSE-DATE TO SQLIO-PARM-VAL(4)
               COMPUTE PC-RELEASED-QTY = 0 - PC-RELEASED-QTY
               MOVE PC-RELEASED-QTY TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT)
                 TO SQLIO-PARM-VAL(5)
               COMPUTE PC-RELEASED-QTY = 0 - PC-RELEASED-QTY
               COMPUTE PC-RELEASED-GBP = 0 - PC-RELEASED-GBP
               MOVE PC-RELEASED-GBP TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT)
                 TO SQLIO-PARM-VAL(6)
               COMPUTE PC-RELEASED-GBP = 0 - PC-RELEASED-GBP
               MOVE PC-APPROVAL-REF TO SQLIO-PARM-VAL(7)
               IF PC-APPROVAL-REF = SPACES
                   MOVE "Y" TO SQLIO-PARM-NULL(7)
               END-IF
               MOVE SPACES TO SQLIO-PARM-VAL(8)
               STRING FUNCTION TRIM(PC-NEW-STATUS) " - "
                      FUNCTION TRIM(PC-REASON-CODE) " - "
                      FUNCTION TRIM(PC-REASON)
                   DELIMITED BY SIZE INTO SQLIO-PARM-VAL(8)
               MOVE CA-USER TO SQLIO-PARM-VAL(9)
               MOVE 9 TO SQLIO-PARM-CNT
               MOVE 8 TO SQLIO-CURSOR
               MOVE "EXEC" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
               IF SQLIO-SQLCODE = 0
                   MOVE 1 TO PC-COMMIT-ROWS
               ELSE
                   MOVE "PCL0007" TO CA-REASON-CODE
                   MOVE SQLIO-ERRMSG TO CA-MESSAGE
                   MOVE "CLOS" TO SQLIO-FUNC
                   CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
                   PERFORM 9000-REJECT THRU 9000-EXIT
                   GO TO 3800-EXIT
               END-IF
               MOVE "CLOS" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           END-IF
           PERFORM 3900-STAMP-LINE THRU 3900-EXIT
           PERFORM 2600-WARNINGS THRU 2600-EXIT
           IF NOT CA-FAILED
               MOVE PC-RELEASED-GBP TO WS-AMT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(PC-NEW-STATUS) ": "
                      FUNCTION TRIM(WS-AMT-ED)
                      " of commitment released; received goods and "
                      "their GRNI accrual are untouched"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       3800-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3900-STAMP-LINE SECTION.
       3900-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.PurchaseOrderLine SET Status = ?, "
                  "CancelledQty = CancelledQty + ?, "
                  "ShortCloseReason = ? WHERE CompanyCode = ? AND "
                  "OrderNumber = ? AND LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE PC-NEW-STATUS TO SQLIO-PARM-VAL(1)
           MOVE PC-RELEASED-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE SPACES TO SQLIO-PARM-VAL(3)
           IF PC-AUTHORITY = SPACES
               STRING FUNCTION TRIM(PC-REASON-CODE) " - "
                      FUNCTION TRIM(PC-REASON) " (by "
                      FUNCTION TRIM(CA-USER) ")"
                   DELIMITED BY SIZE INTO SQLIO-PARM-VAL(3)
           ELSE
               STRING FUNCTION TRIM(PC-REASON-CODE) " - "
                      FUNCTION TRIM(PC-REASON) " (authorised by "
                      FUNCTION TRIM(PC-AUTHORITY) ")"
                   DELIMITED BY SIZE INTO SQLIO-PARM-VAL(3)
           END-IF
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(4)
           MOVE PC-ORDER-NUMBER TO SQLIO-PARM-VAL(5)
           MOVE PC-LINE-NUM TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       3900-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> NORMAL CLOSURE OF A FULLY RECEIVED LINE.  NOTHING IS
      *> RELEASED BECAUSE NOTHING IS OUTSTANDING; THE LINE IS SIMPLY
      *> MARKED CLOSED SO THAT IT LEAVES THE BUYER'S WORK LIST.
      *>----------------------------------------------------------------
       4000-CLOSE SECTION.
       4000-START.
           IF PC-OUTSTAND-QTY > 0
               MOVE 4 TO CA-RETURN-CODE
               MOVE PC-OUTSTAND-QTY TO WS-QTY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(WS-QTY-ED)
                      " is still outstanding; use a short close so "
                      "that the released commitment is recorded "
                      "with its reason"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
           MOVE "CLOSED" TO PC-NEW-STATUS
           PERFORM 3900-STAMP-LINE THRU 3900-EXIT
           PERFORM 2600-WARNINGS THRU 2600-EXIT.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> REOPENING.  A CLOSED ORDER IS REOPENED ONLY ON AN AUTHORITY,
      *> AND A RETURN TO THE SUPPLIER NEVER REOPENS ONE BY ITSELF -
      *> WHETHER WE STILL WANT THE GOODS IS A COMMERCIAL DECISION.
      *>----------------------------------------------------------------
       5000-REOPEN SECTION.
       5000-START.
           IF PC-AUTHORITY = SPACES
               MOVE "PCL0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Reopening a closed order recreates a "
                      "commitment; name the person who authorised it"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           MOVE "OPEN" TO PC-NEW-STATUS
           PERFORM 3900-STAMP-LINE THRU 3900-EXIT
           MOVE 4 TO CA-RETURN-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Order line reopened on the authority of "
                  FUNCTION TRIM(PC-AUTHORITY)
                  "; the released commitment is NOT restored "
                  "automatically"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE 8 TO CA-RETURN-CODE
           MOVE "Y" TO WS-FATAL-SW
           MOVE "REJECTED" TO PC-OUTCOME.
       9000-EXIT.
           EXIT.
       END PROGRAM POCLOSE.
