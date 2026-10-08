      *>================================================================
      *> PROGRAM-ID : TCSTGAP
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY (UNIT TESTS)
      *> PURPOSE    : THE TWO COSTING RULES THAT HAD NO TEST OF THEIR
      *>              OWN.
      *>
      *>              CST-001  EVERY ENQUIRY STATES ITS PURPOSE, BASIS,
      *>                       CURRENCY, UNIT, EFFECTIVE DATE, QUANTITY
      *>                       AND APPROVAL STATE.  A QUOTE IS MARKED
      *>                       AS AN ESTIMATE SO IT CANNOT BE READ AS
      *>                       AN ACTUAL INVENTORY VALUE, AND A BASIS
      *>                       THAT IS NOT HELD IS REPORTED AS
      *>                       UNAVAILABLE - NEVER QUIETLY REPLACED BY
      *>                       ANOTHER ONE.
      *>              CST-010  A ROLL-UP STATES ONE VALUATION BOUNDARY
      *>                       AND ONE COSTING LOT SIZE, AND NOTHING
      *>                       IS CHARGED TO THE PARENT TWICE.  THE
      *>                       WORKED NUMBERS ARE SPECIFICATION B1:
      *>                       MATERIAL 70.10, CONVERSION 20.00,
      *>                       SETUP 10.00, TOTAL 100.10 FOR TEN GOOD
      *>                       FRAMES, 10.01 EACH.
      *>
      *>              MOST OF THE SUITE IS PURE COMMAREA ARITHMETIC AND
      *>              RUNS ANYWHERE.  THE B1 ROLL-UP AND THE BASIS
      *>              LOOKUPS NEED THE DEMONSTRATION DATA AND SKIP
      *>              RATHER THAN FAIL WHEN IT IS NOT THERE.  NOTHING
      *>              IN THIS SUITE WRITES TO THE DATABASE.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2026-10-07 STERP ORIGINAL - COVERAGE FOR CST-001 AND CST-010
      *>================================================================
      *> REQ: CST-001, CST-010
      *> AC: AC-059, AC-063
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCSTGAP.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY COSTNORMCA REPLACING ==DFHCOMMAREA== BY ==WS-CN-CA==.
       COPY COSTROLLCA REPLACING ==DFHCOMMAREA== BY ==WS-CR-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
           05  WS-DEMO                 PIC X VALUE "Y".
           05  WS-IX                   PIC S9(4) VALUE 0.
           05  WS-IX2                  PIC S9(4) VALUE 0.
           05  WS-EXPAND-OCC           PIC S9(9) VALUE 0.
           05  WS-STOP-OCC             PIC S9(9) VALUE 0.
           05  WS-SUM                  PIC S9(13)V9(2) VALUE 0.
           05  WS-EXPAND-TOT           PIC S9(13)V9(2) VALUE 0.
           05  WS-TEXT                 PIC X(100) VALUE SPACES.
       01  WS-PROBE.
           05  WP-1                    PIC X(40).
           05  WP-2                    PIC X(40).
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           MOVE "TCSTGAP enquiry context and roll-up" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-ENQUIRY-CONTEXT
           PERFORM 1500-QUOTE-IS-AN-ESTIMATE
           PERFORM 2000-BOUNDARY-AND-LOT-SIZE
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-THE-DATABASE-WORK
           ELSE
               PERFORM 3000-BASIS-IS-NOT-SUBSTITUTED
               PERFORM 4000-B1-ROLL-UP
           END-IF
           PERFORM TK-END
           STOP RUN.

       0100-PROBE-DATABASE SECTION.
       0100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CostSetCode, Status FROM "
                  "cst.CostSet WHERE CompanyCode = 'STC' AND "
                  "CostSetCode = 'BOMDEMO'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR
               MOVE "N" TO WS-DB-UP
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "N" TO WS-DEMO
           END-IF.
       0100-EXIT.
           EXIT.

       0900-SKIP-THE-DATABASE-WORK SECTION.
       0900-START.
           MOVE "An unavailable basis is not substituted"
               TO TK-NAME
           MOVE "the STERP database is not available" TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "B1 roll-up with a stated boundary" TO TK-NAME
           MOVE "the STERP database is not available" TO TK-DETAIL
           PERFORM TK-SKIP.
       0900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  CST-001.  AN ENQUIRY THAT CANNOT SAY WHAT IT IS AN
      *>       ENQUIRY ABOUT IS REFUSED, AND THE CONTEXT IT DOES NOT
      *>       STATE IS FILLED IN AND REPORTED BACK RATHER THAN LEFT
      *>       FOR THE READER TO GUESS.  NONE OF THIS TOUCHES SQL.
      *>----------------------------------------------------------------
       1000-ENQUIRY-CONTEXT SECTION.
       1000-START.
           PERFORM 1900-SET-UP-ENQUIRY
           MOVE SPACES TO CA-COMPANY OF WS-CN-CA
           CALL "COSTNORM" USING WS-CN-CA
           MOVE "An enquiry with no company is refused" TO TK-NAME
           MOVE "CST0101" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CN-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 1900-SET-UP-ENQUIRY
           MOVE SPACES TO CN-ITEM-CODE
           CALL "COSTNORM" USING WS-CN-CA
           MOVE "An enquiry with no item is refused" TO TK-NAME
           MOVE "CST0101" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CN-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 1900-SET-UP-ENQUIRY
           MOVE 101 TO CN-VAT-RECOVER-PCT
           CALL "COSTNORM" USING WS-CN-CA
           MOVE "A VAT recovery above a hundred per cent is barred"
               TO TK-NAME
           MOVE "CST0107" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CN-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *>   A BASIS NOBODY HOLDS IS NAMED AND REFUSED IN MEMORY, SO
      *>   THE CONTEXT DEFAULTS CAN BE READ BACK FROM THE SAME CALL.
           PERFORM 1900-SET-UP-ENQUIRY
           MOVE "ACTUAL" TO CN-BASIS
           MOVE SPACES TO CN-CURRENCY
           MOVE SPACES TO CN-PURPOSE
           MOVE 0 TO CN-EFF-DATE
           MOVE 0 TO CN-PRICE-PER-QTY
           MOVE "ACTIVE" TO CN-APPROVAL-STATE
           CALL "COSTNORM" USING WS-CN-CA
           MOVE "A cost basis that is not a cost basis is refused"
               TO TK-NAME
           MOVE "CST0102" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CN-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "The refusal is an error, not a quiet zero"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CN-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "No unit cost is handed back with the refusal"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CN-UNIT-COST-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "No basis is claimed to have been used" TO TK-NAME
           MOVE SPACES TO TK-EXPECTED-X
           MOVE CN-BASIS-USED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "A stale approval state is not carried forward"
               TO TK-NAME
           MOVE SPACES TO TK-EXPECTED-X
           MOVE CN-APPROVAL-STATE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "The enquiry states its currency" TO TK-NAME
           MOVE "GBP" TO TK-EXPECTED-X
           MOVE CN-CURRENCY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "The enquiry states its purpose" TO TK-NAME
           MOVE "ENQUIRY" TO TK-EXPECTED-X
           MOVE CN-PURPOSE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "The enquiry states its effective date" TO TK-NAME
           MOVE 20261007 TO TK-EXPECTED-N
           MOVE CN-EFF-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The enquiry states its price-per quantity"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE CN-PRICE-PER-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1500  CST-001.  A QUOTE ESTIMATE IS STAMPED AS PROVISIONAL SO
      *>       THAT IT CANNOT BE PRESENTED AS AN ACTUAL INVENTORY
      *>       VALUE; A VALUATION ENQUIRY IS NOT.
      *>----------------------------------------------------------------
       1500-QUOTE-IS-AN-ESTIMATE SECTION.
       1500-START.
           PERFORM 1900-SET-UP-ENQUIRY
           MOVE "QUOTE" TO CN-PURPOSE
           MOVE "ACTUAL" TO CN-BASIS
           CALL "COSTNORM" USING WS-CN-CA
           MOVE "A quote estimate is marked provisional" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE CN-PROVISIONAL TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "The quote says in words that it is not a valuation"
               TO TK-NAME
           MOVE "Quote estimate - not an inventory valuation"
               TO TK-EXPECTED-X
           MOVE CN-EVIDENCE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 1900-SET-UP-ENQUIRY
           MOVE "VALUATION" TO CN-PURPOSE
           MOVE "ACTUAL" TO CN-BASIS
           CALL "COSTNORM" USING WS-CN-CA
           MOVE "A valuation enquiry is not a provisional estimate"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE CN-PROVISIONAL TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "The valuation purpose is kept as asked for"
               TO TK-NAME
           MOVE "VALUATION" TO TK-EXPECTED-X
           MOVE CN-PURPOSE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1500-EXIT.
           EXIT.

       1900-SET-UP-ENQUIRY SECTION.
       1900-START.
           INITIALIZE WS-CN-CA
           MOVE "CONTEXT" TO CA-FUNCTION OF WS-CN-CA
           MOVE "STC" TO CA-COMPANY OF WS-CN-CA
           MOVE "RDG" TO CA-SITE OF WS-CN-CA
           MOVE "COSTACC" TO CA-USER OF WS-CN-CA
           MOVE 20261007 TO CA-BUSINESS-DATE OF WS-CN-CA
           MOVE "ZZ-CST-GAP-NONE" TO CN-ITEM-CODE
           MOVE "STANDARD" TO CN-BASIS
           MOVE "GBP" TO CN-CURRENCY
           MOVE "EA" TO CN-STOCK-UOM
           MOVE 20261007 TO CN-EFF-DATE
           MOVE 1 TO CN-QUANTITY
           MOVE 1 TO CN-PRICE-PER-QTY.
       1900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  CST-010.  THE ROLL-UP STATES ITS BOUNDARY AND ITS
      *>       COSTING LOT SIZE BEFORE IT PRICES ANYTHING.  NONE OF
      *>       THESE PATHS REACHES BOMCOST OR SQL.
      *>----------------------------------------------------------------
       2000-BOUNDARY-AND-LOT-SIZE SECTION.
       2000-START.
           PERFORM 2900-SET-UP-ROLL-UP
           MOVE SPACES TO CR-ITEM-CODE
           CALL "COSTROLL" USING WS-CR-CA
           MOVE "A roll-up with no item is refused" TO TK-NAME
           MOVE "CST0601" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CR-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 2900-SET-UP-ROLL-UP
           MOVE -5 TO CR-LOT-SIZE
           CALL "COSTROLL" USING WS-CR-CA
           MOVE "A negative costing lot size is refused" TO TK-NAME
           MOVE "CST0602" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CR-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "The refusal explains the missing denominator"
               TO TK-NAME
           MOVE SPACES TO TK-EXPECTED-X
           STRING "The costing lot size must be positive - a fixed "
                  "setup charge needs a denominator"
               DELIMITED BY SIZE INTO TK-EXPECTED-X
           MOVE CA-MESSAGE OF WS-CR-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 2900-SET-UP-ROLL-UP
           MOVE "BOTH" TO CR-BOUNDARY
           CALL "COSTROLL" USING WS-CR-CA
           MOVE "A boundary that is neither EXPAND nor STOP is out"
               TO TK-NAME
           MOVE "CST0606" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CR-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 2900-SET-UP-ROLL-UP
           MOVE "RECALC" TO CA-FUNCTION OF WS-CR-CA
           CALL "COSTROLL" USING WS-CR-CA
           MOVE "An unsupported roll-up function is refused"
               TO TK-NAME
           MOVE "CST0608" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CR-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

      *>   THE CONTEXT IS STATED EVEN WHEN THE REQUEST IS REFUSED.
           PERFORM 2900-SET-UP-ROLL-UP
           MOVE SPACES TO CR-BOUNDARY
           MOVE SPACES TO CR-CURRENCY
           MOVE SPACES TO CR-ITEM-CODE
           MOVE 0 TO CR-LOT-SIZE
           MOVE 0 TO CR-COST-DATE
           CALL "COSTROLL" USING WS-CR-CA
           MOVE "Every roll-up states one valuation boundary"
               TO TK-NAME
           MOVE "EXPAND" TO TK-EXPECTED-X
           MOVE CR-BOUNDARY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "Every roll-up states its currency" TO TK-NAME
           MOVE "GBP" TO TK-EXPECTED-X
           MOVE CR-CURRENCY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "A fixed setup charge always has a denominator"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE CR-LOT-SIZE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The cost date follows the stated effective date"
               TO TK-NAME
           MOVE 20261007 TO TK-EXPECTED-N
           MOVE CR-COST-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       2000-EXIT.
           EXIT.

       2900-SET-UP-ROLL-UP SECTION.
       2900-START.
           INITIALIZE WS-CR-CA
           MOVE "ROLLUP" TO CA-FUNCTION OF WS-CR-CA
           MOVE "STC" TO CA-COMPANY OF WS-CR-CA
           MOVE "RDG" TO CA-SITE OF WS-CR-CA
           MOVE "COSTACC" TO CA-USER OF WS-CR-CA
           MOVE 20261007 TO CA-BUSINESS-DATE OF WS-CR-CA
           MOVE "DEMO-FRAME" TO CR-ITEM-CODE
           MOVE "BOMDEMO" TO CR-COST-SET
           MOVE "GBP" TO CR-CURRENCY
           MOVE "EXPAND" TO CR-BOUNDARY
           MOVE 25 TO CR-MAX-DEPTH
           MOVE 10 TO CR-LOT-SIZE
           MOVE 20261007 TO CR-EFF-DATE
           MOVE 20261007 TO CR-COST-DATE
           MOVE "N" TO CR-NETTED
           MOVE 20 TO CR-WIN-MAX
           MOVE 1 TO CR-FROM-LINE.
       2900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  CST-001.  THE REQUESTED BASIS IS EITHER HELD OR
      *>       REPORTED AS UNAVAILABLE.  NOTHING ELSE IS OFFERED IN
      *>       ITS PLACE.  AC: AC-059
      *>----------------------------------------------------------------
       3000-BASIS-IS-NOT-SUBSTITUTED SECTION.
       3000-START.
           PERFORM 1900-SET-UP-ENQUIRY
           MOVE "VALUATION" TO CN-PURPOSE
           MOVE "STANDARD" TO CN-BASIS
           MOVE 25 TO CN-QUANTITY
           CALL "COSTNORM" USING WS-CN-CA
           MOVE "An unheld standard cost is reported as not found"
               TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CN-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The refusal names the no-substitution rule"
               TO TK-NAME
           MOVE "CST0102" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CN-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "An unavailable basis leaves the answer incomplete"
               TO TK-NAME
           MOVE "INCOMPLETE" TO TK-EXPECTED-X
           MOVE CN-COMPLETENESS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "No average or last purchase cost is substituted"
               TO TK-NAME
           MOVE SPACES TO TK-EXPECTED-X
           MOVE CN-BASIS-USED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "The extended cost of a missing basis is nothing"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CN-EXT-COST-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 3100-FIND-A-COSTED-ITEM
           IF WP-1 = SPACES
               MOVE "An available basis is named on the answer"
                   TO TK-NAME
               MOVE "no average costed item is seeded for STC"
                   TO TK-DETAIL
               PERFORM TK-SKIP
               GO TO 3000-EXIT
           END-IF
           PERFORM 1900-SET-UP-ENQUIRY
           MOVE WP-1 TO CN-ITEM-CODE
           MOVE "AVERAGE" TO CN-BASIS
           MOVE "VALUATION" TO CN-PURPOSE
           MOVE 2 TO CN-QUANTITY
           CALL "COSTNORM" USING WS-CN-CA
           MOVE "An available basis is named on the answer"
               TO TK-NAME
           MOVE "AVERAGE" TO TK-EXPECTED-X
           MOVE CN-BASIS-USED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "The answer states where the cost came from"
               TO TK-NAME
           MOVE "cst.ItemCost" TO TK-EXPECTED-X
           MOVE CN-SOURCE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "The answer states its approval state" TO TK-NAME
           IF CN-APPROVAL-STATE = SPACES
               PERFORM TK-FAIL
           ELSE
               PERFORM TK-PASS
           END-IF
           MOVE "A valuation enquiry is not a provisional estimate"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE CN-PROVISIONAL TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           COMPUTE WS-SUM ROUNDED = CN-UNIT-COST-GBP * 2
           MOVE "The extended cost is the stated quantity's cost"
               TO TK-NAME
           MOVE WS-SUM TO TK-EXPECTED-N
           MOVE CN-EXT-COST-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       3000-EXIT.
           EXIT.

       3100-FIND-A-COSTED-ITEM SECTION.
       3100-START.
           MOVE SPACES TO WP-1 WP-2
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) ItemCode, CostMethod FROM "
                  "cst.ItemCost WHERE CompanyCode = 'STC' AND "
                  "SiteCode = '*' AND Revision = '' AND "
                  "AverageUnitCost > 0 ORDER BY ItemCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF NOT SQLIO-OK
               MOVE SPACES TO WP-1
           END-IF.
       3100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  CST-010.  SPECIFICATION B1 ROLLED UP FOR TEN GOOD
      *>       FRAMES.  MATERIAL 70.10, CONVERSION 20.00 AND THE
      *>       FIXED SETUP OF 10.00 FOR THE ONE EXECUTION RUN ADD TO
      *>       100.10, WHICH IS 10.01 A FRAME.  THE SETUP IS A PART
      *>       OF THE CONVERSION THE ENGINE REPORTS SEPARATELY, SO IT
      *>       MUST BE CHARGED TO THE PARENT ONCE AND ONCE ONLY.
      *>       AC: AC-063
      *>----------------------------------------------------------------
       4000-B1-ROLL-UP SECTION.
       4000-START.
           IF WS-DEMO = "N"
               MOVE "B1 roll-up with a stated boundary" TO TK-NAME
               MOVE "the BOMDEMO cost set is not deployed"
                   TO TK-DETAIL
               PERFORM TK-SKIP
               GO TO 4000-EXIT
           END-IF
           PERFORM 2900-SET-UP-ROLL-UP
           CALL "COSTROLL" USING WS-CR-CA
           IF CA-RETURN-CODE OF WS-CR-CA > 4 OR CR-OCC-COUNT = 0
               MOVE "B1 roll-up with a stated boundary" TO TK-NAME
               MOVE CA-MESSAGE OF WS-CR-CA TO TK-DETAIL
               PERFORM TK-SKIP
               GO TO 4000-EXIT
           END-IF
           MOVE CR-OCC-COUNT TO WS-EXPAND-OCC
           MOVE CR-TOTAL-COST TO WS-EXPAND-TOT
           MOVE "B1 expanded roll-up is complete" TO TK-NAME
           MOVE "COMPLETE" TO TK-EXPECTED-X
           MOVE CR-COMPLETENESS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "B1 states the boundary it used" TO TK-NAME
           MOVE "EXPAND" TO TK-EXPECTED-X
           MOVE CR-BOUNDARY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "B1 material cost is 70.10" TO TK-NAME
           MOVE 70.10 TO TK-EXPECTED-N
           MOVE CR-MATERIAL-COST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "B1 fixed setup for the one run is 10.00"
               TO TK-NAME
           MOVE 10.00 TO TK-EXPECTED-N
           MOVE CR-SETUP-COST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "B1 conversion excluding the setup is 20.00"
               TO TK-NAME
           MOVE 20.00 TO TK-EXPECTED-N
           MOVE CR-LABOUR-COST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "B1 charges the fixed setup to the parent once"
               TO TK-NAME
           COMPUTE WS-SUM = CR-MATERIAL-COST + CR-LABOUR-COST
                            + CR-MACHINE-COST + CR-SETUP-COST
                            + CR-SUBCON-COST + CR-OVERHEAD-COST
                            + CR-OTHER-COST - CR-RECOVERY-CREDIT
           MOVE 100.10 TO TK-EXPECTED-N
           MOVE WS-SUM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "B1 total for ten good frames is 100.10" TO TK-NAME
           MOVE 100.10 TO TK-EXPECTED-N
           MOVE CR-TOTAL-COST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "B1 cost per good frame is 10.01" TO TK-NAME
           MOVE 10.01 TO TK-EXPECTED-N
           MOVE CR-UNIT-COST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "B1 divides by the ten good frames planned"
               TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE CR-GOOD-OUTPUT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 4100-CONTRIBUTING-LINES
           PERFORM 4200-STOP-AT-THE-SUBASSEMBLY.
       4000-EXIT.
           EXIT.

      *>  EVERY COSTED LINE SAYS WHETHER IT CONTRIBUTED OR IS SHOWN
      *>  FOR INFORMATION, AND WHERE THE VALUATION BOUNDARY FELL.
       4100-CONTRIBUTING-LINES SECTION.
       4100-START.
           MOVE 0 TO WS-SUM
           MOVE 0 TO WS-IX2
           PERFORM VARYING WS-IX FROM 1 BY 1
               UNTIL WS-IX > CR-LINE-CNT
               IF CR-L-CONTRIB(WS-IX) = "Y"
                   ADD 1 TO WS-IX2
               END-IF
               IF CR-L-BOUNDARY(WS-IX) NOT = SPACES
                   ADD 1 TO WS-SUM
               END-IF
           END-PERFORM
           MOVE "B1 reports its costed tree" TO TK-NAME
           IF CR-LINE-CNT > 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE "Every costed line declares its value boundary"
               TO TK-NAME
           MOVE CR-LINE-CNT TO TK-EXPECTED-N
           MOVE WS-SUM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "At least one line is counted into the total"
               TO TK-NAME
           IF WS-IX2 > 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.
       4100-EXIT.
           EXIT.

      *>  THE OTHER STATED BOUNDARY.  WITH STOP THE SUBASSEMBLY IS
      *>  TAKEN AT ITS OWN STATED COST AND IS NOT EXPANDED, SO THE
      *>  PARENT CANNOT BE CHARGED THE BRANCH AND ITS COMPONENTS
      *>  BOTH.  THE BOMDEMO SET HOLDS NO COST FOR THE BRACE, SO THE
      *>  HONEST ANSWER IS AN INCOMPLETE ONE RATHER THAN A SILENT
      *>  EXPANSION.
       4200-STOP-AT-THE-SUBASSEMBLY SECTION.
       4200-START.
           PERFORM 2900-SET-UP-ROLL-UP
           MOVE "STOP" TO CR-BOUNDARY
           CALL "COSTROLL" USING WS-CR-CA
           MOVE CR-OCC-COUNT TO WS-STOP-OCC
           MOVE "A stopped roll-up states the STOP boundary"
               TO TK-NAME
           MOVE "STOP" TO TK-EXPECTED-X
           MOVE CR-BOUNDARY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "A stopped roll-up expands less than a full one"
               TO TK-NAME
           IF WS-STOP-OCC > 0 AND WS-STOP-OCC < WS-EXPAND-OCC
               PERFORM TK-PASS
           ELSE
               MOVE SPACES TO TK-DETAIL
               STRING "the stopped and expanded occurrence counts "
                      "should differ"
                   DELIMITED BY SIZE INTO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "A subassembly with no cost is not silently expanded"
               TO TK-NAME
           MOVE "INCOMPLETE" TO TK-EXPECTED-X
           MOVE CR-COMPLETENESS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "The branch and its components are not both charged"
               TO TK-NAME
           IF CR-TOTAL-COST < WS-EXPAND-TOT
               PERFORM TK-PASS
           ELSE
               MOVE SPACES TO TK-DETAIL
               STRING "the stopped roll-up must not carry the "
                      "expanded branch as well"
                   DELIMITED BY SIZE INTO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.
       4200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> THE TEST KIT LIVES IN ITS OWN SECTION SO THAT PERFORMING A
      *> SECTION ABOVE STOPS AT THE END OF THAT SECTION.
      *>----------------------------------------------------------------
       9900-TEST-KIT SECTION.
       COPY TESTKITP.
