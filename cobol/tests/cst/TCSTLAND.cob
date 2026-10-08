      *>================================================================
      *> PROGRAM-ID : TCSTLAND
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY (UNIT TESTS)
      *> PURPOSE    : LANDED COST ATTRIBUTION, NET REALISABLE VALUE
      *>              TESTING AND SCENARIO SAFETY.  EVERY PATH HERE IS
      *>              A SIMULATION (COMMIT "N") SO THE SUITE RUNS WITH
      *>              NO DATABASE AND LEAVES NOTHING BEHIND - WHICH IS
      *>              ALSO THE POINT OF CST-023.
      *> COVERS     : C4 LANDED COST, ESTIMATE THEN ACTUAL
      *>              CST-008 THE DIFFERENCE ONLY, FOLLOWING THE GOODS
      *>              CST-021 LOWER OF COST AND NRV, QUANTITY UNCHANGED
      *>              CST-025 A REVERSAL CAPPED AT ORIGINAL COST
      *>              CST-023 A SCENARIO WRITES NOTHING
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: CST-004, CST-008, CST-021, CST-023, CST-025
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCSTLAND.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY LANDEDCA  REPLACING ==DFHCOMMAREA== BY ==WS-LD-CA==.
       COPY NRVTESTCA REPLACING ==DFHCOMMAREA== BY ==WS-NR-CA==.
       COPY WHATIFCA  REPLACING ==DFHCOMMAREA== BY ==WS-WI-CA==.
       01  WS-COGS                     PIC S9(13)V9(2) VALUE 0.
       01  WS-STOCK                    PIC S9(13)V9(2) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TCSTLAND landed cost, NRV and scenarios" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-LANDED-ESTIMATE
           PERFORM 1100-LANDED-ACTUAL
           PERFORM 1200-LANDED-TWO-LINES
           PERFORM 1300-LANDED-REFUSALS
           PERFORM 2000-NRV-WRITEDOWN
           PERFORM 2100-NRV-AT-COST
           PERFORM 2200-NRV-REVERSAL-CAP
           PERFORM 2300-NRV-REFUSALS
           PERFORM 3000-SCENARIO-SAFETY
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
      *> AC: C4 - 100 UNITS, GOODS 1,000.00, ESTIMATED FREIGHT 80.00.
      *>          THE PROVISIONAL UNIT COST IS 10.80 AND THE WHOLE
      *>          ESTIMATE SITS IN STOCK WHILE THE STOCK IS STILL THERE.
      *>----------------------------------------------------------------
       1000-LANDED-ESTIMATE.
           PERFORM 9000-C4-SETUP
           MOVE "ESTIMATE" TO CA-FUNCTION OF WS-LD-CA
           MOVE 80.00 TO LD-ESTIMATED
           MOVE 100 TO LD-L-QTY-LEFT(1)
           MOVE 0 TO LD-L-QTY-SOLD(1)
           CALL "LANDED" USING WS-LD-CA
           MOVE "C4 the estimate is accepted" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-LD-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C4 the whole 80.00 freight is attributed" TO TK-NAME
           MOVE 80.00 TO TK-EXPECTED-N
           MOVE LD-ALLOCATED-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C4 the provisional unit cost is 10.80" TO TK-NAME
           MOVE 10.80 TO TK-EXPECTED-N
           MOVE LD-L-UNIT-COST(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C4 nothing is sold yet so stock carries it all"
               TO TK-NAME
           MOVE 80.00 TO TK-EXPECTED-N
           MOVE LD-TO-INVENTORY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C4 a simulated estimate writes nothing" TO TK-NAME
           MOVE "SIMULATED" TO TK-EXPECTED-X
           MOVE LD-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> AC: C4 - THE ACTUAL FREIGHT IS 100.00.  ONLY THE 20.00
      *>          DIFFERENCE IS SPREAD: 8.00 FOLLOWS THE 40 UNITS
      *>          ALREADY SOLD AND 12.00 STAYS WITH THE 60 IN STOCK.
      *>          COGS BECOMES 440.00 AND INVENTORY 660.00 - TOGETHER
      *>          1,100.00, THE GOODS PLUS THE ACTUAL CHARGE.
      *>----------------------------------------------------------------
       1100-LANDED-ACTUAL.
           PERFORM 9000-C4-SETUP
           MOVE "ACTUAL" TO CA-FUNCTION OF WS-LD-CA
           MOVE 80.00 TO LD-ESTIMATED
           MOVE 100.00 TO LD-ACTUAL
           MOVE 80.00 TO LD-L-EST-SHARE(1)
           MOVE 60 TO LD-L-QTY-LEFT(1)
           MOVE 40 TO LD-L-QTY-SOLD(1)
           CALL "LANDED" USING WS-LD-CA
           MOVE "C4 only the 20.00 difference is spread" TO TK-NAME
           MOVE 20.00 TO TK-EXPECTED-N
           MOVE LD-DELTA-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C4 8.00 of the increase follows the units sold"
               TO TK-NAME
           MOVE 8.00 TO TK-EXPECTED-N
           MOVE LD-TO-COGS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C4 12.00 of the increase stays in stock" TO TK-NAME
           MOVE 12.00 TO TK-EXPECTED-N
           MOVE LD-TO-INVENTORY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C4 the line now carries the full 100.00" TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE LD-L-ACT-SHARE(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C4 the final unit cost is 11.00" TO TK-NAME
           MOVE 11.00 TO TK-EXPECTED-N
           MOVE LD-L-UNIT-COST(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           COMPUTE WS-COGS = 432.00 + LD-TO-COGS
           MOVE "C4 cost of sales ends at 440.00" TO TK-NAME
           MOVE 440.00 TO TK-EXPECTED-N
           MOVE WS-COGS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           COMPUTE WS-STOCK = 648.00 + LD-TO-INVENTORY
           MOVE "C4 inventory ends at 660.00" TO TK-NAME
           MOVE 660.00 TO TK-EXPECTED-N
           MOVE WS-STOCK TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           COMPUTE WS-STOCK = WS-STOCK + WS-COGS
           MOVE "C4 goods and charge together are 1,100.00" TO TK-NAME
           MOVE 1100.00 TO TK-EXPECTED-N
           MOVE WS-STOCK TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>  REQ: CST-008 - TWO LINES ON A WEIGHT BASIS.  THE SHARES ADD
      *>  BACK TO THE CHARGE EXACTLY; THE ROUNDING RESIDUAL IS OWNED,
      *>  NOT DROPPED (D-22).  100.00 OVER 3 KG AND 4 KG.
       1200-LANDED-TWO-LINES.
           INITIALIZE WS-LD-CA
           MOVE "ESTIMATE" TO CA-FUNCTION OF WS-LD-CA
           MOVE "STC" TO CA-COMPANY OF WS-LD-CA
           MOVE "COSTACC" TO CA-USER OF WS-LD-CA
           MOVE "CST-C4-FRT2" TO LD-CHARGE-REF
           MOVE "FREIGHT" TO LD-CATEGORY
           MOVE "WEIGHT" TO LD-BASIS
           MOVE 1 TO LD-FX-RATE
           MOVE "N" TO LD-COMMIT
           MOVE 100.00 TO LD-ESTIMATED
           MOVE 2 TO LD-LINE-CNT
           MOVE "RCPT-1" TO LD-L-RECEIPT(1)
           MOVE "ITEM-A" TO LD-L-ITEM(1)
           MOVE 3 TO LD-L-BASIS-VAL(1)
           MOVE 10 TO LD-L-QTY-RECVD(1)
           MOVE 10 TO LD-L-QTY-LEFT(1)
           MOVE "RCPT-1" TO LD-L-RECEIPT(2)
           MOVE "ITEM-B" TO LD-L-ITEM(2)
           MOVE 4 TO LD-L-BASIS-VAL(2)
           MOVE 20 TO LD-L-QTY-RECVD(2)
           MOVE 20 TO LD-L-QTY-LEFT(2)
           CALL "LANDED" USING WS-LD-CA
           MOVE "Weight basis totals 7 kg" TO TK-NAME
           MOVE 7 TO TK-EXPECTED-N
           MOVE LD-BASIS-TOTAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "3 kg of 7 carries 42.86" TO TK-NAME
           MOVE 42.86 TO TK-EXPECTED-N
           MOVE LD-L-EST-SHARE(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "4 kg of 7 carries 57.14" TO TK-NAME
           MOVE 57.14 TO TK-EXPECTED-N
           MOVE LD-L-EST-SHARE(2) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           COMPUTE WS-STOCK =
               LD-L-EST-SHARE(1) + LD-L-EST-SHARE(2)
           MOVE "The shares still add back to 100.00" TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE WS-STOCK TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>  REQ: CST-008 - WHAT THE PROGRAM REFUSES TO DO.
       1300-LANDED-REFUSALS.
           PERFORM 9000-C4-SETUP
           MOVE "ESTIMATE" TO CA-FUNCTION OF WS-LD-CA
           MOVE "TONNAGE" TO LD-BASIS
           MOVE 80.00 TO LD-ESTIMATED
           CALL "LANDED" USING WS-LD-CA
           MOVE "An unknown basis is refused, not substituted"
               TO TK-NAME
           MOVE "CST1303" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-LD-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 9000-C4-SETUP
           MOVE "ESTIMATE" TO CA-FUNCTION OF WS-LD-CA
           MOVE 80.00 TO LD-ESTIMATED
           MOVE 0 TO LD-L-BASIS-VAL(1)
           MOVE 0 TO LD-L-QTY-RECVD(1)
           CALL "LANDED" USING WS-LD-CA
           MOVE "A zero basis total is reported, not divided by"
               TO TK-NAME
           MOVE "CST1304" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-LD-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 9000-C4-SETUP
           MOVE "ACTUAL" TO CA-FUNCTION OF WS-LD-CA
           MOVE "ACTUALISED" TO LD-STATUS
           MOVE 80.00 TO LD-ESTIMATED
           MOVE 100.00 TO LD-ACTUAL
           CALL "LANDED" USING WS-LD-CA
           MOVE "A charge is never actualised twice" TO TK-NAME
           MOVE "CST1306" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-LD-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 9000-C4-SETUP
           MOVE "SHUFFLE" TO CA-FUNCTION OF WS-LD-CA
           CALL "LANDED" USING WS-LD-CA
           MOVE "An unsupported function is rejected" TO TK-NAME
           MOVE "CST1308" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-LD-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> REQ: CST-021 - 100 UNITS AT 10.00 WHOSE SELLING PRICE HAS
      *>      FALLEN TO 9.00 WITH 1.50 TO SELL.  NRV IS 7.50, SO THE
      *>      WRITE-DOWN IS 250.00 AND THE STOCK IS CARRIED AT 750.00.
      *>      THE QUANTITY DOES NOT MOVE - THE STOCK IS STILL THERE.
      *>----------------------------------------------------------------
       2000-NRV-WRITEDOWN.
           PERFORM 9100-NRV-SETUP
           MOVE "TEST" TO CA-FUNCTION OF WS-NR-CA
           MOVE 9.00 TO NR-SELLING-PRICE
           MOVE 1.50 TO NR-COST-TO-SELL
           CALL "NRVTEST" USING WS-NR-CA
           MOVE "NRV per unit is 7.50" TO TK-NAME
           MOVE 7.50 TO TK-EXPECTED-N
           MOVE NR-NRV-UNIT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The write-down is 250.00" TO TK-NAME
           MOVE 250.00 TO TK-EXPECTED-N
           MOVE NR-WRITEDOWN-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Carrying value falls to 750.00" TO TK-NAME
           MOVE 750.00 TO TK-EXPECTED-N
           MOVE NR-NEW-CARRYING TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The quantity on hand is untouched" TO TK-NAME
           MOVE 100 TO TK-EXPECTED-N
           MOVE NR-QUANTITY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The item is flagged as impaired" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE NR-IMPAIRED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>  REQ: CST-021 - NRV ABOVE COST LEAVES THE STOCK AT COST.  THE
      *>  TEST NEVER WRITES INVENTORY UP.
       2100-NRV-AT-COST.
           PERFORM 9100-NRV-SETUP
           MOVE "TEST" TO CA-FUNCTION OF WS-NR-CA
           MOVE 14.00 TO NR-SELLING-PRICE
           MOVE 1.00 TO NR-COST-TO-SELL
           CALL "NRVTEST" USING WS-NR-CA
           MOVE "No write-down when NRV is above cost" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE NR-WRITEDOWN-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The stock stays at cost, 1,000.00" TO TK-NAME
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE NR-NEW-CARRYING TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The status says it is held at cost" TO TK-NAME
           MOVE "AT COST" TO TK-EXPECTED-X
           MOVE NR-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> REQ: CST-025 - STOCK WRITTEN DOWN TO 6.00 FROM AN ORIGINAL
      *>      10.00.  THE MARKET RECOVERS TO 15.00, WHICH WOULD SUPPORT
      *>      A 900.00 WRITE-BACK, BUT ONLY 400.00 MAY BE REVERSED -
      *>      CARRYING VALUE STOPS AT ORIGINAL COST AND THE CAP IS
      *>      DECLARED RATHER THAN APPLIED QUIETLY.
      *>----------------------------------------------------------------
       2200-NRV-REVERSAL-CAP.
           PERFORM 9100-NRV-SETUP
           MOVE "REVERSE" TO CA-FUNCTION OF WS-NR-CA
           MOVE 6.00 TO NR-CARRYING-UNIT
           MOVE 10.00 TO NR-ORIGINAL-UNIT
           MOVE 15.00 TO NR-SELLING-PRICE
           MOVE 0 TO NR-COST-TO-SELL
           CALL "NRVTEST" USING WS-NR-CA
           MOVE "The reversal is capped at 400.00" TO TK-NAME
           MOVE 400.00 TO TK-EXPECTED-N
           MOVE NR-REVERSAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Carrying value returns to cost, 1,000.00" TO TK-NAME
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE NR-NEW-CARRYING TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The cap is declared, not applied silently" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE NR-CAPPED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 9100-NRV-SETUP
           MOVE "REVERSE" TO CA-FUNCTION OF WS-NR-CA
           MOVE 6.00 TO NR-CARRYING-UNIT
           MOVE 10.00 TO NR-ORIGINAL-UNIT
           MOVE 300.00 TO NR-REVERSAL-GBP
           CALL "NRVTEST" USING WS-NR-CA
           MOVE "A reversal inside the write-down is allowed"
               TO TK-NAME
           MOVE 300.00 TO TK-EXPECTED-N
           MOVE NR-REVERSAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "That reversal is not capped" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE NR-CAPPED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 9100-NRV-SETUP
           MOVE "REVERSE" TO CA-FUNCTION OF WS-NR-CA
           MOVE 6.00 TO NR-CARRYING-UNIT
           MOVE 10.00 TO NR-ORIGINAL-UNIT
           MOVE 500.00 TO NR-REVERSAL-GBP
           CALL "NRVTEST" USING WS-NR-CA
           MOVE "A reversal above the earlier write-down is refused"
               TO TK-NAME
           MOVE "CST1105" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-NR-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>  REQ: CST-021 - EVIDENCE AND DATES ARE NOT OPTIONAL.
       2300-NRV-REFUSALS.
           PERFORM 9100-NRV-SETUP
           MOVE "TEST" TO CA-FUNCTION OF WS-NR-CA
           MOVE 0 TO NR-ASSESS-DATE
           CALL "NRVTEST" USING WS-NR-CA
           MOVE "An assessment needs a date" TO TK-NAME
           MOVE "CST1102" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-NR-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 9100-NRV-SETUP
           MOVE "TEST" TO CA-FUNCTION OF WS-NR-CA
           MOVE 0 TO NR-SELLING-PRICE
           CALL "NRVTEST" USING WS-NR-CA
           MOVE "A write-down needs selling price evidence" TO TK-NAME
           MOVE "CST1104" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-NR-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 9100-NRV-SETUP
           MOVE "GUESS" TO CA-FUNCTION OF WS-NR-CA
           CALL "NRVTEST" USING WS-NR-CA
           MOVE "An unsupported NRV function is rejected" TO TK-NAME
           MOVE "CST1109" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-NR-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> REQ: CST-023 - A SCENARIO IS A QUESTION.  THE VALIDATION
      *>      REFUSES NONSENSE BEFORE IT REACHES THE ROLL-UP, AND THE
      *>      SIDE EFFECT FLAG IS ALWAYS "N" ON THE WAY OUT.
      *>----------------------------------------------------------------
       3000-SCENARIO-SAFETY.
           INITIALIZE WS-WI-CA
           MOVE "COMPARE" TO CA-FUNCTION OF WS-WI-CA
           MOVE "STC" TO CA-COMPANY OF WS-WI-CA
           MOVE "COSTACC" TO CA-USER OF WS-WI-CA
           MOVE "FG-100" TO WI-ITEM-CODE
           MOVE 100 TO WI-BASE-LOT-SIZE
           MOVE 0 TO WI-VARIANT-CNT
           CALL "WHATIF" USING WS-WI-CA
           MOVE "A comparison with no variant is refused" TO TK-NAME
           MOVE "CST1202" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-WI-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "A scenario never reports a side effect" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE WI-SIDE-EFFECTS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-WI-CA
           MOVE "SCENARIO" TO CA-FUNCTION OF WS-WI-CA
           MOVE "STC" TO CA-COMPANY OF WS-WI-CA
           MOVE SPACES TO WI-ITEM-CODE
           CALL "WHATIF" USING WS-WI-CA
           MOVE "A scenario without an item is refused" TO TK-NAME
           MOVE "CST1201" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-WI-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-WI-CA
           MOVE "SAVESET" TO CA-FUNCTION OF WS-WI-CA
           MOVE "STC" TO CA-COMPANY OF WS-WI-CA
           MOVE "FG-100" TO WI-ITEM-CODE
           MOVE 1 TO WI-BASE-LOT-SIZE
           CALL "WHATIF" USING WS-WI-CA
           MOVE "WHATIF will not save a cost set" TO TK-NAME
           MOVE "CST1206" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-WI-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> SHARED FIXTURES
      *>----------------------------------------------------------------
       9000-C4-SETUP.
           INITIALIZE WS-LD-CA
           MOVE "STC" TO CA-COMPANY OF WS-LD-CA
           MOVE "COSTACC" TO CA-USER OF WS-LD-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-LD-CA
           MOVE "CST-C4-FRT" TO LD-CHARGE-REF
           MOVE "SHIP-C4" TO LD-SHIPMENT-REF
           MOVE "FREIGHT" TO LD-CATEGORY
           MOVE "VALUE" TO LD-BASIS
           MOVE "GBP" TO LD-CURRENCY
           MOVE 1 TO LD-FX-RATE
           MOVE "Y" TO LD-ELIGIBLE
           MOVE "N" TO LD-COMMIT
           MOVE 20261006 TO LD-ALLOCATION-DATE
           MOVE 1 TO LD-LINE-CNT
           MOVE 1 TO LD-L-NUM(1)
           MOVE "RCPT-C4" TO LD-L-RECEIPT(1)
           MOVE 1 TO LD-L-RCPT-LINE(1)
           MOVE "CST-C4-ITEM" TO LD-L-ITEM(1)
           MOVE 1000.00 TO LD-L-BASIS-VAL(1)
           MOVE 100 TO LD-L-QTY-RECVD(1)
           MOVE 100 TO LD-L-QTY-LEFT(1).

       9100-NRV-SETUP.
           INITIALIZE WS-NR-CA
           MOVE "STC" TO CA-COMPANY OF WS-NR-CA
           MOVE "COSTACC" TO CA-USER OF WS-NR-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-NR-CA
           MOVE "CST-NRV-ITEM" TO NR-ITEM-CODE
           MOVE 20261231 TO NR-ASSESS-DATE
           MOVE 100 TO NR-QUANTITY
           MOVE 10.00 TO NR-CARRYING-UNIT
           MOVE 10.00 TO NR-ORIGINAL-UNIT
           MOVE 9.00 TO NR-SELLING-PRICE
           MOVE 1.50 TO NR-COST-TO-SELL
           MOVE "NRV" TO NR-REASON-CODE
           MOVE "Price list Nov 2026" TO NR-EVIDENCE
           MOVE "N" TO NR-COMMIT.

       COPY TESTKITP.
