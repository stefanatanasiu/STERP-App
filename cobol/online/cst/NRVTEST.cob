      *>================================================================
      *> PROGRAM-ID : NRVTEST
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *> PURPOSE    : MEASURE INVENTORY AT THE LOWER OF COST AND NET
      *>              REALISABLE VALUE (FRS 102).  THE WRITE-DOWN IS A
      *>              VALUE ADJUSTMENT HELD IN AN ALLOWANCE - THE
      *>              QUANTITY ON HAND IS NEVER TOUCHED, BECAUSE THE
      *>              STOCK IS STILL THERE.  A LATER RECOVERY MAY BE
      *>              REVERSED, BUT NEVER ABOVE THE ORIGINAL COST.
      *> CALLED BY  : THE PERIOD END VALUATION RUN, THE COSTING CLIENT
      *> INTERFACE  : CALL "NRVTEST" USING WS-NRVTEST-CA
      *>              COPYBOOK NRVTESTCA
      *> CALLS      : SQLIO (cst.Impairment, cst.ItemCost)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-09-08 RJH  CR0702 ORIGINAL STOCK PROVISION
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT ASSESSMENT DATES
      *> 2015-04-06 DJM  CR2301 FRS 102 REPLACES SSAP 9
      *> 2026-10-06 STERP CST-021 CST-025 REVERSAL CAPPED AT COST
      *>================================================================
      *> REQ: CST-021, CST-025
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NRVTEST.
       AUTHOR. R J HARTLEY.
       DATE-WRITTEN. 08/09/1998.
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
           05  WS-OWNS-TXN             PIC X VALUE "N".
               88  WS-OWNS-IT                    VALUE "Y".
           05  WS-WRITE                PIC X VALUE "N".
               88  WS-WRITING                    VALUE "Y".
           05  WS-POOL-SITE            PIC X(10) VALUE "*".
           05  WS-EXPLICIT             PIC X VALUE "N".
           05  WS-PREPARER             PIC X(20) VALUE SPACES.
           05  WS-IMP-STATUS           PIC X(12) VALUE SPACES.
       01  WS-WORK.
           05  WS-PRIOR-ALLOW          PIC S9(13)V9(2) VALUE 0.
           05  WS-PRIOR-WD             PIC S9(13)V9(2) VALUE 0.
           05  WS-REV-PRIOR            PIC S9(13)V9(2) VALUE 0.
           05  WS-HEADROOM             PIC S9(13)V9(2) VALUE 0.
           05  WS-REQUESTED            PIC S9(13)V9(2) VALUE 0.
           05  WS-COST-GBP             PIC S9(13)V9(2) VALUE 0.
       LINKAGE SECTION.
       COPY NRVTESTCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 7100-BEGIN-TXN
           EVALUATE CA-FUNCTION
               WHEN "TEST"     PERFORM 1000-TEST
               WHEN "SCAN"     PERFORM 2000-SCAN
               WHEN "APPROVE"  PERFORM 3000-APPROVE
               WHEN "POST"     PERFORM 4000-POST
               WHEN "REVERSE"  PERFORM 5000-REVERSE
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
           MOVE "N" TO WS-ERROR WS-SQL-FAILED
           MOVE "NRVTEST" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "CST-021" TO CA-REQ-REF
           MOVE SPACES TO NR-WARNING NR-STATUS
           MOVE 0 TO NR-CARRYING-GBP NR-NRV-GBP NR-WRITEDOWN-GBP
           MOVE 0 TO NR-ALLOWANCE-GBP NR-NEW-CARRYING
      *>   a reversal amount the preparer asked for is an input, so it
      *>   survives initialisation - the rest of the results do not
           IF CA-FUNCTION NOT = "REVERSE"
               MOVE 0 TO NR-REVERSAL-GBP
           END-IF
           MOVE 0 TO NR-ITEMS-TESTED NR-ITEMS-IMPAIRED
           MOVE "N" TO NR-IMPAIRED NR-CAPPED NR-MORE-LINES
           MOVE 0 TO WS-PRIOR-ALLOW WS-PRIOR-WD WS-REV-PRIOR
           IF CA-FUNCTION NOT = "SCAN"
               MOVE 0 TO NR-LINE-CNT
           END-IF
           IF NR-COMMIT = SPACE OR LOW-VALUE
               MOVE "N" TO NR-COMMIT
           END-IF
           MOVE FUNCTION UPPER-CASE(NR-COMMIT) TO NR-COMMIT
           EVALUATE NR-COMMIT
               WHEN "Y" MOVE "Y" TO WS-OWNS-TXN WS-WRITE
               WHEN "T" MOVE "N" TO WS-OWNS-TXN
                        MOVE "Y" TO WS-WRITE
               WHEN OTHER MOVE "N" TO WS-OWNS-TXN WS-WRITE
           END-EVALUATE
           IF CA-FUNCTION = "SCAN"
               MOVE "N" TO WS-WRITE WS-OWNS-TXN
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES
               MOVE "CST1101" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION = "TEST" AND NR-ITEM-CODE = SPACES
               MOVE "CST1101" TO CA-REASON-CODE
               MOVE "An item is required to test net realisable value"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF (CA-FUNCTION = "TEST" OR CA-FUNCTION = "SCAN")
              AND NR-ASSESS-DATE = 0
               MOVE "CST1102" TO CA-REASON-CODE
               MOVE "An assessment date is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF (CA-FUNCTION = "APPROVE" OR CA-FUNCTION = "POST")
              AND NR-IMPAIRMENT-ID = 0
               MOVE "CST1107" TO CA-REASON-CODE
               MOVE "An impairment reference is required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  CST-021.  THE TEST ITSELF.
      *>         nrv unit   = selling price - costs to complete and sell
      *>         write-down = (carrying unit - nrv unit) x quantity
      *>       WHEN NRV IS AT OR ABOVE COST NOTHING HAPPENS - STOCK IS
      *>       NEVER WRITTEN UP BY THIS TEST.
      *>----------------------------------------------------------------
       1000-TEST SECTION.
       1000-START.
           PERFORM 0300-READ-POOL
           IF WS-INPUT-BAD OR WS-SQL-BAD
               GO TO 1000-EXIT
           END-IF
           IF NR-QUANTITY <= 0
               MOVE "Quantity is zero - nothing to measure"
                   TO NR-WARNING
               MOVE "NOT TESTED" TO NR-STATUS
               MOVE 4 TO CA-RETURN-CODE
               GO TO 1000-EXIT
           END-IF
           IF NR-SELLING-PRICE = 0 AND NR-NRV-UNIT = 0
               MOVE "CST1104" TO CA-REASON-CODE
               MOVE "An estimated selling price is required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1000-EXIT
           END-IF
           IF NR-NRV-UNIT = 0
               COMPUTE NR-NRV-UNIT =
                   NR-SELLING-PRICE - NR-COST-TO-SELL
           END-IF
           IF NR-NRV-UNIT < 0
      *>       a negative realisable value is written down to nil, not
      *>       carried as a negative asset
               MOVE 0 TO NR-NRV-UNIT
               MOVE "Net realisable value is negative - floored at nil"
                   TO NR-WARNING
           END-IF
           MOVE 1 TO NR-ITEMS-TESTED
           COMPUTE NR-CARRYING-GBP ROUNDED =
               NR-CARRYING-UNIT * NR-QUANTITY
           COMPUTE NR-NRV-GBP ROUNDED = NR-NRV-UNIT * NR-QUANTITY
           IF NR-NRV-UNIT >= NR-CARRYING-UNIT
               MOVE 0 TO NR-WRITEDOWN-GBP
               MOVE "N" TO NR-IMPAIRED
               MOVE "AT COST" TO NR-STATUS
               MOVE NR-CARRYING-GBP TO NR-NEW-CARRYING
               MOVE WS-PRIOR-ALLOW TO NR-ALLOWANCE-GBP
               GO TO 1000-EXIT
           END-IF
           COMPUTE NR-WRITEDOWN-GBP ROUNDED =
               (NR-CARRYING-UNIT - NR-NRV-UNIT) * NR-QUANTITY
           MOVE "Y" TO NR-IMPAIRED
           MOVE 1 TO NR-ITEMS-IMPAIRED
           COMPUTE NR-ALLOWANCE-GBP =
               WS-PRIOR-ALLOW + NR-WRITEDOWN-GBP
           MOVE NR-NRV-GBP TO NR-NEW-CARRYING
           IF NR-REASON-CODE = SPACES
               MOVE "NRV" TO NR-REASON-CODE
           END-IF
           MOVE "PROPOSED" TO NR-STATUS
           IF WS-WRITING
               PERFORM 7000-WRITE-IMPAIRMENT
           END-IF.
       1000-EXIT.
           EXIT.

      *>   the pool being measured.  a carrying value supplied by the
      *>   caller is used as given (the what-if and the report do that);
      *>   otherwise it comes from cst.ItemCost.
       0300-READ-POOL SECTION.
       0300-START.
           IF NR-CARRYING-UNIT NOT = 0 AND NR-QUANTITY NOT = 0
               COMPUTE WS-COST-GBP ROUNDED =
                   NR-CARRYING-UNIT * NR-QUANTITY
               IF NR-ORIGINAL-UNIT = 0
                   MOVE NR-CARRYING-UNIT TO NR-ORIGINAL-UNIT
               END-IF
               GO TO 0300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT QtyOnHand, AverageUnitCost, "
                  "CarryingValueGBP, AllowanceGBP, "
                  "ISNULL(NrvUnitValue,0) FROM cst.ItemCost WHERE "
                  "CompanyCode = ? AND ItemCode = ? AND Revision = ? "
                  "AND SiteCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE NR-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE NR-REVISION TO SQLIO-PARM-VAL(3)
           MOVE WS-POOL-SITE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 5 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-SQL-BAD
               GO TO 0300-EXIT
           END-IF
           IF NOT WS-GOT-ROW
               MOVE "CST1103" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No carrying value held for "
                      FUNCTION TRIM(NR-ITEM-CODE)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0300-EXIT
           END-IF
           IF NR-QUANTITY = 0
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO NR-QUANTITY
           END-IF
           IF NR-CARRYING-UNIT = 0
               MOVE FUNCTION NUMVAL(WS-COL(2)) TO NR-CARRYING-UNIT
           END-IF
           MOVE FUNCTION NUMVAL(WS-COL(3)) TO WS-COST-GBP
           MOVE FUNCTION NUMVAL(WS-COL(4)) TO WS-PRIOR-ALLOW
           IF NR-ORIGINAL-UNIT = 0
      *>       the allowance already carried is added back, so original
      *>       cost is what the stock cost before any write-down
               IF NR-QUANTITY NOT = 0
                   COMPUTE NR-ORIGINAL-UNIT ROUNDED =
                       NR-CARRYING-UNIT
                       + (WS-PRIOR-ALLOW / NR-QUANTITY)
               ELSE
                   MOVE NR-CARRYING-UNIT TO NR-ORIGINAL-UNIT
               END-IF
           END-IF.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  CST-021.  THE EXPOSURE LIST - EVERY POOL WHOSE HELD NRV
      *>       IS BELOW ITS CARRYING COST.  A WINDOW, NOT A DUMP.
      *>----------------------------------------------------------------
       2000-SCAN SECTION.
       2000-START.
           IF NR-FROM-LINE < 1
               MOVE 1 TO NR-FROM-LINE
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ic.ItemCode, ISNULL(i.Description,''), "
                  "ic.QtyOnHand, ic.AverageUnitCost, "
                  "ISNULL(ic.NrvUnitValue,0), ic.AllowanceGBP FROM "
                  "cst.ItemCost ic LEFT JOIN itm.Item i ON "
                  "i.CompanyCode = ic.CompanyCode AND i.ItemCode = "
                  "ic.ItemCode WHERE ic.CompanyCode = ? AND "
                  "ic.QtyOnHand > 0 AND ic.NrvUnitValue IS NOT NULL "
                  "AND ic.NrvUnitValue < ic.AverageUnitCost ORDER BY "
                  "ic.ItemCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           IF SQLIO-SQLCODE < 0
               PERFORM 8900-SQL-ERROR
               GO TO 2000-EXIT
           END-IF
           MOVE 0 TO NR-LINE-CNT WS-COUNT
           MOVE 0 TO NR-WRITEDOWN-GBP NR-CARRYING-GBP NR-NRV-GBP
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
               ADD 1 TO WS-COUNT
               PERFORM 2100-TAKE-ROW
               IF NR-LINE-CNT >= 20 AND WS-COUNT >= NR-FROM-LINE
                   MOVE "Y" TO NR-MORE-LINES
               END-IF
           END-PERFORM
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           MOVE WS-COUNT TO NR-ITEMS-TESTED
           MOVE WS-COUNT TO NR-ITEMS-IMPAIRED
           IF WS-COUNT = 0
               MOVE "No pool is held below cost" TO NR-WARNING
           END-IF.
       2000-EXIT.
           EXIT.

       2100-TAKE-ROW SECTION.
       2100-START.
           MOVE FUNCTION NUMVAL(WS-COL(3)) TO WS-QTY1
           MOVE FUNCTION NUMVAL(WS-COL(4)) TO WS-UNIT1
           MOVE FUNCTION NUMVAL(WS-COL(5)) TO WS-AMT1
           COMPUTE WS-AMT2 ROUNDED = (WS-UNIT1 - WS-AMT1) * WS-QTY1
           ADD WS-AMT2 TO NR-WRITEDOWN-GBP
           COMPUTE WS-AMT3 ROUNDED = WS-UNIT1 * WS-QTY1
           ADD WS-AMT3 TO NR-CARRYING-GBP
           COMPUTE WS-AMT3 ROUNDED = WS-AMT1 * WS-QTY1
           ADD WS-AMT3 TO NR-NRV-GBP
           IF WS-COUNT < NR-FROM-LINE OR NR-LINE-CNT >= 20
               GO TO 2100-EXIT
           END-IF
           ADD 1 TO NR-LINE-CNT
           MOVE WS-COL(1) TO NR-L-ITEM(NR-LINE-CNT)
           MOVE WS-COL(2) TO NR-L-DESC(NR-LINE-CNT)
           MOVE WS-QTY1 TO NR-L-QTY(NR-LINE-CNT)
           MOVE WS-UNIT1 TO NR-L-UNIT-COST(NR-LINE-CNT)
           MOVE WS-AMT1 TO NR-L-NRV-UNIT(NR-LINE-CNT)
           MOVE WS-AMT2 TO NR-L-WRITEDOWN(NR-LINE-CNT)
           MOVE "NRV" TO NR-L-REASON(NR-LINE-CNT).
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  APPROVAL.  A WRITE-DOWN IS AN ESTIMATE, AND THE PERSON
      *>       WHO MADE IT MAY NOT BE THE PERSON WHO ACCEPTS IT.
      *>----------------------------------------------------------------
       3000-APPROVE SECTION.
       3000-START.
           PERFORM 0400-READ-IMPAIRMENT
           IF WS-INPUT-BAD OR WS-SQL-BAD
               GO TO 3000-EXIT
           END-IF
           IF WS-IMP-STATUS NOT = "PROPOSED"
               MOVE "CST1106" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Impairment is " FUNCTION TRIM(WS-IMP-STATUS)
                      " - only a proposed write-down may be approved"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           IF FUNCTION UPPER-CASE(CA-USER)
              = FUNCTION UPPER-CASE(WS-PREPARER)
               MOVE "CST1108" TO CA-REASON-CODE
               MOVE "The approver may not be the preparer"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           MOVE "APPROVED" TO NR-STATUS
           IF WS-WRITING
               MOVE SPACES TO SQLIO-STMT
               STRING "UPDATE cst.Impairment SET Status = "
                      "'APPROVED', ApprovedBy = ?, ApprovedAt = "
                      "SYSDATETIME() WHERE ImpairmentId = ? AND "
                      "Status = 'PROPOSED'"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE 2 TO SQLIO-PARM-CNT
               MOVE CA-USER TO SQLIO-PARM-VAL(1)
               MOVE NR-IMPAIRMENT-ID TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT)
                   TO SQLIO-PARM-VAL(2)
               MOVE "N" TO SQLIO-PARM-NULL(1)
               MOVE "N" TO SQLIO-PARM-NULL(2)
               PERFORM 8100-EXECUTE
           END-IF.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  POSTING.  THE ALLOWANCE MOVES, THE QUANTITY DOES NOT.
      *>       CST-021 - A WRITE-DOWN IS A VALUE ADJUSTMENT.
      *>----------------------------------------------------------------
       4000-POST SECTION.
       4000-START.
           PERFORM 0400-READ-IMPAIRMENT
           IF WS-INPUT-BAD OR WS-SQL-BAD
               GO TO 4000-EXIT
           END-IF
           IF WS-IMP-STATUS NOT = "APPROVED"
               MOVE "CST1106" TO CA-REASON-CODE
               MOVE "Only an approved write-down may be posted"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 4000-EXIT
           END-IF
           COMPUTE NR-ALLOWANCE-GBP =
               WS-PRIOR-ALLOW + NR-WRITEDOWN-GBP - NR-REVERSAL-GBP
           COMPUTE NR-NEW-CARRYING =
               WS-COST-GBP - NR-WRITEDOWN-GBP + NR-REVERSAL-GBP
           MOVE "POSTED" TO NR-STATUS
           IF WS-WRITING
               PERFORM 7200-UPDATE-POOL
               IF NOT WS-SQL-BAD
                   MOVE SPACES TO SQLIO-STMT
                   STRING "UPDATE cst.Impairment SET Status = "
                          "'POSTED' WHERE ImpairmentId = ?"
                       DELIMITED BY SIZE INTO SQLIO-STMT
                   MOVE 1 TO SQLIO-PARM-CNT
                   MOVE NR-IMPAIRMENT-ID TO SQLIO-NUM-EDIT
                   MOVE FUNCTION TRIM(SQLIO-NUM-EDIT)
                       TO SQLIO-PARM-VAL(1)
                   MOVE "N" TO SQLIO-PARM-NULL(1)
                   PERFORM 8100-EXECUTE
               END-IF
           END-IF.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000  CST-025.  CIRCUMSTANCES CHANGE AND A WRITE-DOWN MAY BE
      *>       REVERSED - BUT ONLY UP TO WHAT WAS WRITTEN DOWN, AND
      *>       NEVER ABOVE ORIGINAL COST.  WHEN THE REQUEST WOULD GO
      *>       FURTHER WE CAP IT AND SAY SO IN NR-CAPPED, RATHER THAN
      *>       QUIETLY WRITING THE STOCK UP.
      *>----------------------------------------------------------------
       5000-REVERSE SECTION.
       5000-START.
           MOVE "CST-025" TO CA-REQ-REF
           PERFORM 0300-READ-POOL
           IF WS-INPUT-BAD OR WS-SQL-BAD
               GO TO 5000-EXIT
           END-IF
           IF NR-REVERSE-OF-ID NOT = 0
               PERFORM 0500-READ-PRIOR-WD
               IF WS-INPUT-BAD OR WS-SQL-BAD
                   GO TO 5000-EXIT
               END-IF
           ELSE
               MOVE WS-PRIOR-ALLOW TO WS-PRIOR-WD
               IF WS-PRIOR-WD = 0
      *>           nothing named and no allowance on file - what is
      *>           still written down is cost less carrying value
                   COMPUTE WS-PRIOR-WD ROUNDED =
                       (NR-ORIGINAL-UNIT - NR-CARRYING-UNIT)
                       * NR-QUANTITY
               END-IF
           END-IF
           MOVE NR-REVERSAL-GBP TO WS-REQUESTED
           IF WS-REQUESTED NOT = 0
               MOVE "Y" TO WS-EXPLICIT
           ELSE
               MOVE "N" TO WS-EXPLICIT
      *>       no amount asked for - recover as much as the recovered
      *>       net realisable value supports
               IF NR-NRV-UNIT = 0 AND NR-SELLING-PRICE NOT = 0
                   COMPUTE NR-NRV-UNIT =
                       NR-SELLING-PRICE - NR-COST-TO-SELL
               END-IF
               COMPUTE WS-REQUESTED ROUNDED =
                   (NR-NRV-UNIT - NR-CARRYING-UNIT) * NR-QUANTITY
           END-IF
           IF WS-REQUESTED <= 0
               MOVE 0 TO NR-REVERSAL-GBP
               MOVE "Nothing to reverse - value has not recovered"
                   TO NR-WARNING
               MOVE "NO CHANGE" TO NR-STATUS
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               GO TO 5000-EXIT
           END-IF
      *>   an amount the preparer typed in is checked against what was
      *>   written down.  an amount we worked out from the recovered
      *>   realisable value is capped instead - the recovery is real,
      *>   it is only the write-back that has a ceiling.
           IF WS-EXPLICIT = "Y" AND WS-REQUESTED > WS-PRIOR-WD
               MOVE "CST1105" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A reversal may not exceed the earlier write-down"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 5000-EXIT
           END-IF
      *>   headroom back to original cost - CST-025 cap
           COMPUTE WS-HEADROOM ROUNDED =
               (NR-ORIGINAL-UNIT - NR-CARRYING-UNIT) * NR-QUANTITY
           IF WS-HEADROOM < 0
               MOVE 0 TO WS-HEADROOM
           END-IF
           IF WS-REQUESTED > WS-HEADROOM
               MOVE WS-HEADROOM TO NR-REVERSAL-GBP
               MOVE "Y" TO NR-CAPPED
               MOVE "Reversal capped at original cost (CST-025)"
                   TO NR-WARNING
               MOVE 4 TO CA-RETURN-CODE
           ELSE
               MOVE WS-REQUESTED TO NR-REVERSAL-GBP
               MOVE "N" TO NR-CAPPED
           END-IF
           COMPUTE NR-CARRYING-GBP ROUNDED =
               NR-CARRYING-UNIT * NR-QUANTITY
           COMPUTE NR-NEW-CARRYING =
               NR-CARRYING-GBP + NR-REVERSAL-GBP
           COMPUTE NR-ALLOWANCE-GBP =
               WS-PRIOR-ALLOW - NR-REVERSAL-GBP
           IF NR-ALLOWANCE-GBP < 0
               MOVE 0 TO NR-ALLOWANCE-GBP
           END-IF
           MOVE 0 TO NR-WRITEDOWN-GBP
           MOVE "REVERSED" TO NR-STATUS
           IF NR-REASON-CODE = SPACES
               MOVE "RECOVERY" TO NR-REASON-CODE
           END-IF
           IF WS-WRITING
               PERFORM 7000-WRITE-IMPAIRMENT
               IF NOT WS-SQL-BAD
                   PERFORM 7200-UPDATE-POOL
               END-IF
           END-IF.
       5000-EXIT.
           EXIT.

       0400-READ-IMPAIRMENT SECTION.
       0400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ItemCode, ISNULL(Revision,''), Quantity, "
                  "CarryingUnitCost, NrvUnitValue, WriteDownGBP, "
                  "Status, ISNULL(CreatedBy,'') FROM cst.Impairment "
                  "WHERE ImpairmentId = ? AND CompanyCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE NR-IMPAIRMENT-ID TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 8 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-SQL-BAD
               GO TO 0400-EXIT
           END-IF
           IF NOT WS-GOT-ROW
               MOVE "CST1107" TO CA-REASON-CODE
               MOVE "Impairment not found" TO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               MOVE "Y" TO WS-ERROR
               GO TO 0400-EXIT
           END-IF
           MOVE WS-COL(1) TO NR-ITEM-CODE
           MOVE WS-COL(2) TO NR-REVISION
           MOVE FUNCTION NUMVAL(WS-COL(3)) TO NR-QUANTITY
           MOVE FUNCTION NUMVAL(WS-COL(4)) TO NR-CARRYING-UNIT
           MOVE FUNCTION NUMVAL(WS-COL(5)) TO NR-NRV-UNIT
           MOVE FUNCTION NUMVAL(WS-COL(6)) TO NR-WRITEDOWN-GBP
           MOVE WS-COL(7) TO WS-IMP-STATUS
           MOVE WS-COL(8) TO WS-PREPARER
           MOVE 0 TO NR-CARRYING-UNIT
           PERFORM 0300-READ-POOL.
       0400-EXIT.
           EXIT.

       0500-READ-PRIOR-WD SECTION.
       0500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT WriteDownGBP, Status FROM cst.Impairment "
                  "WHERE ImpairmentId = ? AND CompanyCode = ? AND "
                  "IsReversal = 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE NR-REVERSE-OF-ID TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-SQL-BAD
               GO TO 0500-EXIT
           END-IF
           IF NOT WS-GOT-ROW
               MOVE "CST1107" TO CA-REASON-CODE
               MOVE "The write-down being reversed was not found"
                   TO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               MOVE "Y" TO WS-ERROR
               GO TO 0500-EXIT
           END-IF
           MOVE FUNCTION NUMVAL(WS-COL(1)) TO WS-PRIOR-WD.
       0500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 7000  THE PROPOSAL ITSELF.  EVIDENCE IS STORED WITH IT - A
      *>       WRITE-DOWN WITHOUT EVIDENCE IS AN ASSERTION.
      *>----------------------------------------------------------------
       7000-WRITE-IMPAIRMENT SECTION.
       7000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO cst.Impairment (CompanyCode, "
                  "ItemCode, Revision, SiteCode, AssessmentDate, "
                  "Quantity, CarryingUnitCost, EstimatedSellingPrice, "
                  "CostsToCompleteAndSell, NrvUnitValue, "
                  "WriteDownGBP, ReasonCode, Evidence, IsReversal, "
                  "ReversalOfImpairmentId, Status, CreatedBy) VALUES "
                  "(?, ?, ?, ?, CONVERT(date, ?, 112), ?, ?, ?, ?, "
                  "?, ?, ?, ?, ?, ?, 'PROPOSED', ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 16 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE NR-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE NR-REVISION TO SQLIO-PARM-VAL(3)
           MOVE WS-POOL-SITE TO SQLIO-PARM-VAL(4)
           MOVE NR-ASSESS-DATE TO WS-DATE-TEXT
           MOVE WS-DATE-TEXT TO SQLIO-PARM-VAL(5)
           MOVE NR-QUANTITY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE NR-CARRYING-UNIT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE NR-SELLING-PRICE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE NR-COST-TO-SELL TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE NR-NRV-UNIT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           IF CA-FUNCTION = "REVERSE"
               MOVE NR-REVERSAL-GBP TO SQLIO-NUM-EDIT
           ELSE
               MOVE NR-WRITEDOWN-GBP TO SQLIO-NUM-EDIT
           END-IF
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE NR-REASON-CODE TO SQLIO-PARM-VAL(12)
           MOVE NR-EVIDENCE TO SQLIO-PARM-VAL(13)
           IF CA-FUNCTION = "REVERSE"
               MOVE "1" TO SQLIO-PARM-VAL(14)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(14)
           END-IF
           MOVE NR-REVERSE-OF-ID TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(15)
           MOVE CA-USER TO SQLIO-PARM-VAL(16)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 16
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           IF NR-REVERSE-OF-ID = 0
               MOVE "Y" TO SQLIO-PARM-NULL(15)
           END-IF
           PERFORM 8100-EXECUTE
           IF NOT WS-SQL-BAD
               PERFORM 7100-READ-BACK-ID
           END-IF.
       7000-EXIT.
           EXIT.

       7100-READ-BACK-ID SECTION.
       7100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(MAX(ImpairmentId),0) FROM "
                  "cst.Impairment WHERE CompanyCode = ? AND "
                  "ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE NR-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-GOT-ROW
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO NR-IMPAIRMENT-ID
           END-IF.
       7100-EXIT.
           EXIT.

      *>   the allowance carries the write-down.  QtyOnHand is not in
      *>   this statement and never will be - CST-021.
       7200-UPDATE-POOL SECTION.
       7200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE cst.ItemCost SET AllowanceGBP = ?, "
                  "NrvUnitValue = ?, LastUpdatedBy = ?, "
                  "LastUpdatedAt = SYSDATETIME() WHERE CompanyCode "
                  "= ? AND ItemCode = ? AND Revision = ? AND "
                  "SiteCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE NR-ALLOWANCE-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE NR-NRV-UNIT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(4)
           MOVE NR-ITEM-CODE TO SQLIO-PARM-VAL(5)
           MOVE NR-REVISION TO SQLIO-PARM-VAL(6)
           MOVE WS-POOL-SITE TO SQLIO-PARM-VAL(7)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 7
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           PERFORM 8100-EXECUTE.
       7200-EXIT.
           EXIT.

       7100-BEGIN-TXN SECTION.
       7100-BSTART.
           IF WS-OWNS-IT
               MOVE "BEGN" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           END-IF.
       7100-BEXIT.
           EXIT.

       7200-COMMIT-TXN SECTION.
       7200-CSTART.
           IF WS-OWNS-IT
               MOVE "COMT" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           END-IF.
       7200-CEXIT.
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
           MOVE "CST1109" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                  " is not supported by NRVTEST"
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
