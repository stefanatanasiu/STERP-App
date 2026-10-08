      *>================================================================
      *> PROGRAM-ID : TTOLCHK
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : UNIT TESTS FOR TOLCHK, THE PURCHASING TOLERANCE
      *>              SERVICE.  THE EXPECTED VALUES ARE THE WORKED
      *>              NUMBERS OF THE SPECIFICATION (SECTION 6 RECEIPT
      *>              EXAMPLE AND THE D-08 TOLERANCE BASELINE).
      *>              THESE TESTS NEED NO DATABASE: THE LIMITS ARE
      *>              PASSED IN WITH TC-RULE-SOURCE = "PASSED".
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: PO-011, PO-022, D-08   AC: AC-053
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TTOLCHK.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY TOLCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-TC==.
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           MOVE "po TOLCHK" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-QTY-WITHIN THRU 1000-EXIT
           PERFORM 1100-QTY-COMPLETE THRU 1100-EXIT
           PERFORM 1200-QTY-AT-LIMIT THRU 1200-EXIT
           PERFORM 1300-QTY-ABOVE-LIMIT THRU 1300-EXIT
           PERFORM 1400-QTY-CAP-OVERRIDE THRU 1400-EXIT
           PERFORM 2000-PRICE-BOTH-FAIL THRU 2000-EXIT
           PERFORM 2100-PRICE-ONE-FAIL THRU 2100-EXIT
           PERFORM 2200-PRICE-WITHIN THRU 2200-EXIT
           PERFORM 3000-CHARGE-WITHIN THRU 3000-EXIT
           PERFORM 3100-CHARGE-ABOVE THRU 3100-EXIT
           PERFORM TK-END
           STOP RUN.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-QTY-SETUP SECTION.
       1000-SETUP.
           INITIALIZE WS-TC
           MOVE "QTY" TO CA-FUNCTION
           MOVE "STC" TO CA-COMPANY
           MOVE "BUYER" TO CA-USER
           MOVE 20261006 TO CA-BUSINESS-DATE
           MOVE "PASSED" TO TC-RULE-SOURCE
           MOVE "BLOCK" TO TC-ACTION-OVER.
       1000-SETUP-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AC-053: ORDER 30, RECEIVE 20 - TEN REMAIN OUTSTANDING AND NO
      *> OVER DELIVERY ARISES.
      *>----------------------------------------------------------------
       1000-QTY-WITHIN SECTION.
       1000-START.
           PERFORM 1000-QTY-SETUP THRU 1000-SETUP-EXIT
           MOVE 30 TO TC-ORDERED-QTY
           MOVE 0 TO TC-COUNTED-QTY
           MOVE 20 TO TC-PROPOSED-QTY
           MOVE 0 TO TC-OVER-PCT
           CALL "TOLCHK" USING WS-TC
           MOVE "AC-053 receive 20 of 30 leaves 10 outstanding"
             TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE TC-OUTSTANDING-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-053 no over delivery on a part receipt"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE TC-OVER-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-053 part receipt is within tolerance" TO TK-NAME
           MOVE "WITHIN" TO TK-EXPECTED-X
           MOVE TC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1100-QTY-COMPLETE SECTION.
       1100-START.
           PERFORM 1000-QTY-SETUP THRU 1000-SETUP-EXIT
           MOVE 30 TO TC-ORDERED-QTY
           MOVE 20 TO TC-COUNTED-QTY
           MOVE 10 TO TC-PROPOSED-QTY
           CALL "TOLCHK" USING WS-TC
           MOVE "PO-011 second receipt closes the outstanding ten"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE TC-OUTSTANDING-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-011 permitted quantity is the approved quantity"
             TO TK-NAME
           MOVE 30 TO TK-EXPECTED-N
           MOVE TC-PERMITTED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A RECEIPT EXACTLY AT THE APPROVED FIVE PER CENT LIMIT IS
      *> ACCEPTED, LEAVES NO OUTSTANDING QUANTITY AND SHOWS THE EXCESS
      *> SEPARATELY - IT IS NEVER HIDDEN.
      *>----------------------------------------------------------------
       1200-QTY-AT-LIMIT SECTION.
       1200-START.
           PERFORM 1000-QTY-SETUP THRU 1000-SETUP-EXIT
           MOVE 100 TO TC-ORDERED-QTY
           MOVE 0 TO TC-COUNTED-QTY
           MOVE 105 TO TC-PROPOSED-QTY
           MOVE 5 TO TC-OVER-PCT
           CALL "TOLCHK" USING WS-TC
           MOVE "AC-053 receipt at the approved limit is accepted"
             TO TK-NAME
           MOVE "ATLIMIT" TO TK-EXPECTED-X
           MOVE TC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-053 five excess units stay separately visible"
             TO TK-NAME
           MOVE 5 TO TK-EXPECTED-N
           MOVE TC-OVER-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-053 at-limit receipt leaves no outstanding"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE TC-OUTSTANDING-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-053 at-limit receipt needs no amendment"
             TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE TC-NEEDS-AMEND TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1300-QTY-ABOVE-LIMIT SECTION.
       1300-START.
           PERFORM 1000-QTY-SETUP THRU 1000-SETUP-EXIT
           MOVE 100 TO TC-ORDERED-QTY
           MOVE 0 TO TC-COUNTED-QTY
           MOVE 106 TO TC-PROPOSED-QTY
           MOVE 5 TO TC-OVER-PCT
           CALL "TOLCHK" USING WS-TC
           MOVE "AC-053 one unit above the limit blocks" TO TK-NAME
           MOVE "BLOCK" TO TK-EXPECTED-X
           MOVE TC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-053 above-limit receipt needs an amendment"
             TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE TC-NEEDS-AMEND TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-053 excess above the limit is one unit" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE TC-EXCESS-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-053 above-limit receipt returns hold 06"
             TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> D-08: AN ITEM OVERRIDE ABOVE FIVE PER CENT IS NOT PERMITTED
      *> AND IS CAPPED.
      *>----------------------------------------------------------------
       1400-QTY-CAP-OVERRIDE SECTION.
       1400-START.
           PERFORM 1000-QTY-SETUP THRU 1000-SETUP-EXIT
           MOVE 100 TO TC-ORDERED-QTY
           MOVE 110 TO TC-PROPOSED-QTY
           MOVE 10 TO TC-OVER-PCT
           CALL "TOLCHK" USING WS-TC
           MOVE "D-08 over delivery override is capped at 5 per cent"
             TO TK-NAME
           MOVE 5 TO TK-EXPECTED-N
           MOVE TC-OVER-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "D-08 permitted quantity after the cap is 105"
             TO TK-NAME
           MOVE 105 TO TK-EXPECTED-N
           MOVE TC-PERMITTED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2000-PRICE-SETUP SECTION.
       2000-SETUP.
           INITIALIZE WS-TC
           MOVE "PRICE" TO CA-FUNCTION
           MOVE "STC" TO CA-COMPANY
           MOVE "APCLERK" TO CA-USER
           MOVE "PASSED" TO TC-RULE-SOURCE
           MOVE 2 TO TC-PRICE-PCT-LIM
           MOVE 50 TO TC-PRICE-ABS-LIM.
       2000-SETUP-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> D-08: BOTH THE 2% AND THE 50.00 TEST MUST PASS.  10.50 AGAINST
      *> 10.00 ON 200 UNITS IS 5% AND 100.00 - BOTH FAIL, SO IT BLOCKS.
      *>----------------------------------------------------------------
       2000-PRICE-BOTH-FAIL SECTION.
       2000-START.
           PERFORM 2000-PRICE-SETUP THRU 2000-SETUP-EXIT
           MOVE 10 TO TC-ORDER-PRICE
           MOVE 10.5 TO TC-INVOICE-PRICE
           MOVE 200 TO TC-PRICE-QTY
           CALL "TOLCHK" USING WS-TC
           MOVE "D-08 price variance failing both tests blocks"
             TO TK-NAME
           MOVE "BLOCK" TO TK-EXPECTED-X
           MOVE TC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "D-08 price variance amount is 100.00" TO TK-NAME
           MOVE 100 TO TK-EXPECTED-N
           MOVE TC-VARIANCE-AMT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "D-08 price variance percentage is 5" TO TK-NAME
           MOVE 5 TO TK-EXPECTED-N
           MOVE TC-VARIANCE-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE SAME 5% ON 50 UNITS IS ONLY 25.00, SO THE CASH TEST
      *> PASSES AND THE RESULT IS A WARNING, NOT A BLOCK.
      *>----------------------------------------------------------------
       2100-PRICE-ONE-FAIL SECTION.
       2100-START.
           PERFORM 2000-PRICE-SETUP THRU 2000-SETUP-EXIT
           MOVE 10 TO TC-ORDER-PRICE
           MOVE 10.5 TO TC-INVOICE-PRICE
           MOVE 50 TO TC-PRICE-QTY
           CALL "TOLCHK" USING WS-TC
           MOVE "D-08 only the percentage test fails, so it warns"
             TO TK-NAME
           MOVE "WARN" TO TK-EXPECTED-X
           MOVE TC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "D-08 price variance amount is 25.00" TO TK-NAME
           MOVE 25 TO TK-EXPECTED-N
           MOVE TC-VARIANCE-AMT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2200-PRICE-WITHIN SECTION.
       2200-START.
           PERFORM 2000-PRICE-SETUP THRU 2000-SETUP-EXIT
           MOVE 10 TO TC-ORDER-PRICE
           MOVE 10.1 TO TC-INVOICE-PRICE
           MOVE 100 TO TC-PRICE-QTY
           CALL "TOLCHK" USING WS-TC
           MOVE "D-08 one per cent and 10.00 is within tolerance"
             TO TK-NAME
           MOVE "WITHIN" TO TK-EXPECTED-X
           MOVE TC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "D-08 a within-tolerance price returns 00" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3000-CHARGE-WITHIN SECTION.
       3000-START.
           INITIALIZE WS-TC
           MOVE "CHARGE" TO CA-FUNCTION
           MOVE "STC" TO CA-COMPANY
           MOVE "PASSED" TO TC-RULE-SOURCE
           MOVE 25 TO TC-CHARGE-ABS-LIM
           MOVE 75 TO TC-ESTIMATE-GBP
           MOVE 95 TO TC-ACTUAL-GBP
           CALL "TOLCHK" USING WS-TC
           MOVE "D-08 charge variance of 20.00 is within 25.00"
             TO TK-NAME
           MOVE "WITHIN" TO TK-EXPECTED-X
           MOVE TC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3100-CHARGE-ABOVE SECTION.
       3100-START.
           INITIALIZE WS-TC
           MOVE "CHARGE" TO CA-FUNCTION
           MOVE "STC" TO CA-COMPANY
           MOVE "PASSED" TO TC-RULE-SOURCE
           MOVE 25 TO TC-CHARGE-ABS-LIM
           MOVE 75 TO TC-ESTIMATE-GBP
           MOVE 105 TO TC-ACTUAL-GBP
           CALL "TOLCHK" USING WS-TC
           MOVE "D-08 charge variance of 30.00 needs authority"
             TO TK-NAME
           MOVE "BLOCK" TO TK-EXPECTED-X
           MOVE TC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "D-08 charge variance amount is 30.00" TO TK-NAME
           MOVE 30 TO TK-EXPECTED-N
           MOVE TC-VARIANCE-AMT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       3100-EXIT.
           EXIT.
       COPY TESTKITP.
