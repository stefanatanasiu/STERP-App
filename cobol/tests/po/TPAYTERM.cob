      *>================================================================
      *> PROGRAM-ID : TPAYTERM
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : UNIT TESTS FOR PAYTERMC, THE PAYMENT TERMS
      *>              POLICY CHECK.  THE LIMITS ARE PASSED IN WITH
      *>              PT-LIMIT-SOURCE = "PASSED" AND NO TERMS CODE IS
      *>              USED, SO THESE TESTS NEED NO DATABASE.
      *>              EXPECTED VALUES ARE THE D-25 BASELINE: 30 DAY
      *>              COMPANY POLICY, 60 DAY STATUTORY MAXIMUM FROM
      *>              RECEIPT OF A VALID INVOICE.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: PO-027, D-25
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TPAYTERM.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY PAYTERMCCA REPLACING ==DFHCOMMAREA== BY ==WS-PT==.
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           MOVE "po PAYTERMC" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-WITHIN THRU 1000-EXIT
           PERFORM 1100-AT-POLICY THRU 1100-EXIT
           PERFORM 1200-OVER-POLICY THRU 1200-EXIT
           PERFORM 1300-AT-STATUTORY THRU 1300-EXIT
           PERFORM 1400-BREACH THRU 1400-EXIT
           PERFORM 1500-EXCEPTED THRU 1500-EXIT
           PERFORM 1600-HALF-EXCEPTION THRU 1600-EXIT
           PERFORM 1700-SME-TIGHTER THRU 1700-EXIT
           PERFORM 1800-SNAPSHOT THRU 1800-EXIT
           PERFORM 1900-BAD-INPUT THRU 1900-EXIT
           PERFORM TK-END
           STOP RUN.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0100-SETUP SECTION.
       0100-START.
           INITIALIZE WS-PT
           MOVE "CHECK" TO CA-FUNCTION
           MOVE "STC" TO CA-COMPANY
           MOVE "STC-MAIN" TO CA-SITE
           MOVE "BUYER" TO CA-USER
           MOVE 20261006 TO CA-BUSINESS-DATE
           MOVE "ACME" TO PT-SUPPLIER
           MOVE 20261006 TO PT-ORDER-DATE
           MOVE "PASSED" TO PT-LIMIT-SOURCE
           MOVE "LARGE" TO PT-SUPPLIER-SIZE
           MOVE 30 TO PT-POLICY-DAYS-IN
           MOVE 60 TO PT-STAT-MAX-IN.
       0100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 20 DAYS IS INSIDE THE 30 DAY COMPANY POLICY.
      *>----------------------------------------------------------------
       1000-WITHIN SECTION.
       1000-START.
           PERFORM 0100-SETUP THRU 0100-EXIT
           MOVE 20 TO PT-TERMS-DAYS
           CALL "PAYTERMC" USING WS-PT
           MOVE "20 days is within the 30 day policy" TO TK-NAME
           MOVE "WITHIN" TO TK-EXPECTED-X
           MOVE PT-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "20 days returns return code zero" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "20 days is not reported in payment practices"
             TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE PT-REPORTABLE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> EXACTLY AT THE POLICY IS STILL WITHIN IT.
      *>----------------------------------------------------------------
       1100-AT-POLICY SECTION.
       1100-START.
           PERFORM 0100-SETUP THRU 0100-EXIT
           MOVE 30 TO PT-TERMS-DAYS
           CALL "PAYTERMC" USING WS-PT
           MOVE "30 days exactly at policy is within" TO TK-NAME
           MOVE "WITHIN" TO TK-EXPECTED-X
           MOVE PT-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "30 days leaves no excess" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE PT-EXCESS-DAYS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 45 DAYS IS LONGER THAN POLICY BUT INSIDE THE STATUTORY
      *> MAXIMUM: A WARNING, AND REPORTABLE.
      *>----------------------------------------------------------------
       1200-OVER-POLICY SECTION.
       1200-START.
           PERFORM 0100-SETUP THRU 0100-EXIT
           MOVE 45 TO PT-TERMS-DAYS
           CALL "PAYTERMC" USING WS-PT
           MOVE "45 days warns against the 30 day policy"
             TO TK-NAME
           MOVE "WARN" TO TK-EXPECTED-X
           MOVE PT-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "45 days gives return code 04" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "45 days is 15 days over policy" TO TK-NAME
           MOVE 15 TO TK-EXPECTED-N
           MOVE PT-EXCESS-DAYS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "45 days is reported in payment practices"
             TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE PT-REPORTABLE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "45 days needs no exception" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE PT-NEEDS-EXCEPTION TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> D-25: 60 DAYS IS THE BASELINE MAXIMUM, SO 60 IS ALLOWED.
      *>----------------------------------------------------------------
       1300-AT-STATUTORY SECTION.
       1300-START.
           PERFORM 0100-SETUP THRU 0100-EXIT
           MOVE 60 TO PT-TERMS-DAYS
           CALL "PAYTERMC" USING WS-PT
           MOVE "D-25 60 days exactly at the maximum only warns"
             TO TK-NAME
           MOVE "WARN" TO TK-EXPECTED-X
           MOVE PT-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "60 days does not need an exception" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE PT-NEEDS-EXCEPTION TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 75 DAYS BREACHES THE STATUTORY MAXIMUM.
      *>----------------------------------------------------------------
       1400-BREACH SECTION.
       1400-START.
           PERFORM 0100-SETUP THRU 0100-EXIT
           MOVE 75 TO PT-TERMS-DAYS
           CALL "PAYTERMC" USING WS-PT
           MOVE "75 days breaches the 60 day maximum" TO TK-NAME
           MOVE "BREACH" TO TK-EXPECTED-X
           MOVE PT-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a breach is a hold, return code 06" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "75 days is 15 days over the maximum" TO TK-NAME
           MOVE 15 TO TK-EXPECTED-N
           MOVE PT-EXCESS-DAYS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "a breach needs an authorised exception" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE PT-NEEDS-EXCEPTION TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a breach reason code is PTC0004" TO TK-NAME
           MOVE "PTC0004" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AN AUTHORISED EXCEPTION LETS THE ORDER THROUGH, BUT IT IS
      *> STILL REPORTED IN THE PAYMENT PRACTICES DATA.
      *>----------------------------------------------------------------
       1500-EXCEPTED SECTION.
       1500-START.
           PERFORM 0100-SETUP THRU 0100-EXIT
           MOVE 75 TO PT-TERMS-DAYS
           MOVE "FINDIR" TO PT-EXCEPTION-BY
           MOVE "Strategic supplier, board approved" TO
                PT-EXCEPTION-REASON
           CALL "PAYTERMC" USING WS-PT
           MOVE "an authorised exception clears the breach"
             TO TK-NAME
           MOVE "EXCEPTED" TO TK-EXPECTED-X
           MOVE PT-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "an excepted order proceeds with a warning"
             TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "an excepted order is still reportable" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE PT-REPORTABLE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AN APPROVER WITHOUT A REASON IS NOT AN AUTHORISED EXCEPTION.
      *>----------------------------------------------------------------
       1600-HALF-EXCEPTION SECTION.
       1600-START.
           PERFORM 0100-SETUP THRU 0100-EXIT
           MOVE 75 TO PT-TERMS-DAYS
           MOVE "FINDIR" TO PT-EXCEPTION-BY
           CALL "PAYTERMC" USING WS-PT
           MOVE "an approver without a reason still breaches"
             TO TK-NAME
           MOVE "BREACH" TO TK-EXPECTED-X
           MOVE PT-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a half exception reports PTC0005" TO TK-NAME
           MOVE "PTC0005" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A TIGHTER MAXIMUM FOR A SMALL SUPPLIER BITES EARLIER.
      *>----------------------------------------------------------------
       1700-SME-TIGHTER SECTION.
       1700-START.
           PERFORM 0100-SETUP THRU 0100-EXIT
           MOVE "SME" TO PT-SUPPLIER-SIZE
           MOVE 30 TO PT-STAT-MAX-IN
           MOVE 45 TO PT-TERMS-DAYS
           CALL "PAYTERMC" USING WS-PT
           MOVE "45 days breaches a 30 day small supplier maximum"
             TO TK-NAME
           MOVE "BREACH" TO TK-EXPECTED-X
           MOVE PT-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the small supplier limit is the effective one"
             TO TK-NAME
           MOVE 30 TO TK-EXPECTED-N
           MOVE PT-EFFECTIVE-LIMIT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1700-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE SNAPSHOT IS THE WORDING THAT GOES TO THE SUPPLIER.
      *>----------------------------------------------------------------
       1800-SNAPSHOT SECTION.
       1800-START.
           PERFORM 0100-SETUP THRU 0100-EXIT
           MOVE 30 TO PT-TERMS-DAYS
           CALL "PAYTERMC" USING WS-PT
           MOVE "the agreed terms snapshot is self contained"
             TO TK-NAME
           MOVE "30 days from receipt of a valid invoice"
             TO TK-EXPECTED-X
           MOVE PT-TERMS-SNAPSHOT TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the default basis is the invoice date" TO TK-NAME
           MOVE "INVOICE" TO TK-EXPECTED-X
           MOVE PT-TERMS-BASIS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1800-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1900-BAD-INPUT SECTION.
       1900-START.
           PERFORM 0100-SETUP THRU 0100-EXIT
           MOVE SPACES TO CA-COMPANY
           MOVE 30 TO PT-TERMS-DAYS
           CALL "PAYTERMC" USING WS-PT
           MOVE "a missing company code is rejected" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0100-SETUP THRU 0100-EXIT
           MOVE "WIBBLE" TO CA-FUNCTION
           CALL "PAYTERMC" USING WS-PT
           MOVE "an unknown function is rejected" TO TK-NAME
           MOVE "PTC0006" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0100-SETUP THRU 0100-EXIT
           MOVE 0 TO PT-TERMS-DAYS
           CALL "PAYTERMC" USING WS-PT
           MOVE "terms with neither a code nor a day count fail"
             TO TK-NAME
           MOVE "PTC0001" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1900-EXIT.
           EXIT.
       COPY TESTKITP.
       END PROGRAM TPAYTERM.
