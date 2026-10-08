>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : TKITQTY
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION (TESTS)
*> PURPOSE    : The quantity rules of specification 09 section 5
*>              and acceptance scenario AC-077, using the
*>              specification's own worked numbers.  No database
*>              is touched: KITQTY is pure arithmetic.
*>----------------------------------------------------------------
*> REQ: KIT-003, KIT-005, KIT-011, KIT-016
*> AC: AC-077
*>================================================================
 IDENTIFICATION DIVISION.
 PROGRAM-ID. TKITQTY.
 AUTHOR. STERP.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY TESTKITW.
 COPY KITQTYCA REPLACING ==DFHCOMMAREA== BY ==Ws-Qty-Ca==
      LEADING ==CA-== BY ==QA-==.
 PROCEDURE DIVISION.
 Main-Entry.
     MOVE "KIT quantity rules (spec 09 s5)" TO TK-SUITE
     PERFORM TK-BEGIN
     PERFORM Test-Original-Example
     PERFORM Test-Reduced-Requirement
     PERFORM Test-Scrap-Is-Not-A-Return
     PERFORM Test-Return-Bounded
     PERFORM Test-Fixed-Per-Run-Not-Repeated
     PERFORM Test-Proportional-Scaling
     PERFORM Test-Zero-Output
     PERFORM Test-Accountability
     PERFORM Test-Unexplained-Material
     PERFORM Test-Bad-Function
     PERFORM TK-END
     STOP RUN.

 Reset-Case.
     INITIALIZE Ws-Qty-Ca
     MOVE "STC" TO QA-COMPANY
     MOVE "RDG" TO QA-SITE
     MOVE "ENGINEER" TO QA-USER
     MOVE 20261006 TO QA-BUSINESS-DATE
     MOVE "KIT000001" TO KQ-KIT-REF
     MOVE 1 TO KQ-LINE-NUM
     MOVE "PIN-8X40" TO KQ-ITEM-CODE
     MOVE "EA" TO KQ-UOM-CODE
     MOVE "PROPORTION" TO KQ-USAGE-BASIS.

*>----------------------------------------------------------------
*> AC-077: "Require ten, issue seven and return two" gives net
*> issued five and remaining requirement five.
 Test-Original-Example.
     PERFORM Reset-Case
     MOVE "LINEQTY" TO QA-FUNCTION
     MOVE 10 TO KQ-ORIGINAL-REQ
     MOVE 10 TO KQ-CURRENT-REQ
     MOVE 7 TO KQ-GROSS-ISSUED
     MOVE 2 TO KQ-RETURNED
     CALL "KITQTY" USING Ws-Qty-Ca
     MOVE "AC-077 net issued is 5 after issuing 7 and returning 2"
          TO TK-NAME
     MOVE 5 TO TK-EXPECTED-N
     MOVE KQ-NET-ISSUED TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "AC-077 remaining requirement is 5" TO TK-NAME
     MOVE 5 TO TK-EXPECTED-N
     MOVE KQ-REMAINING TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "AC-077 no excess while the requirement is still ten"
          TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KQ-EXCESS TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> AC-077 continued: "Reduce approved requirement to four" gives
*> excess one - not a hidden issue reduction - and it needs
*> authority.
 Test-Reduced-Requirement.
     PERFORM Reset-Case
     MOVE "REQCHG" TO QA-FUNCTION
     MOVE 10 TO KQ-ORIGINAL-REQ
     MOVE 10 TO KQ-CURRENT-REQ
     MOVE 4 TO KQ-PROPOSED-REQ
     MOVE 7 TO KQ-GROSS-ISSUED
     MOVE 2 TO KQ-RETURNED
     MOVE 1.500000 TO KQ-UNIT-COST
     CALL "KITQTY" USING Ws-Qty-Ca
     MOVE "AC-077 reducing the requirement to four leaves excess 1"
          TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE KQ-EXCESS TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "AC-077 remaining requirement becomes zero" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KQ-REMAINING TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "AC-077 the actual issue is not rewritten (net stays 5)"
          TO TK-NAME
     MOVE 5 TO TK-EXPECTED-N
     MOVE KQ-NET-ISSUED TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "AC-077 a reduction that creates excess needs authority"
          TO TK-NAME
     IF KQ-NEEDS-AUTH = "Y" AND QA-RETURN-CODE = 6
         PERFORM TK-PASS
     ELSE
         MOVE QA-REASON-CODE TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     MOVE "AC-077 cost impact of removing six units at 1.50 is -9.00"
          TO TK-NAME
     MOVE -9 TO TK-EXPECTED-N
     MOVE KQ-COST-IMPACT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> KIT-014: scrap is a disposition of issued material, so it does
*> not reduce net issued the way a return does.
 Test-Scrap-Is-Not-A-Return.
     PERFORM Reset-Case
     MOVE "LINEQTY" TO QA-FUNCTION
     MOVE 10 TO KQ-ORIGINAL-REQ
     MOVE 10 TO KQ-CURRENT-REQ
     MOVE 7 TO KQ-GROSS-ISSUED
     MOVE 0 TO KQ-RETURNED
     MOVE 3 TO KQ-SCRAPPED
     CALL "KITQTY" USING Ws-Qty-Ca
     MOVE "KIT-014 scrapping three does not reduce net issued"
          TO TK-NAME
     MOVE 7 TO TK-EXPECTED-N
     MOVE KQ-NET-ISSUED TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "KIT-014 scrap does not increase the remaining requirement"
          TO TK-NAME
     MOVE 3 TO TK-EXPECTED-N
     MOVE KQ-REMAINING TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> KIT-013: a return cannot exceed the quantity issued.
 Test-Return-Bounded.
     PERFORM Reset-Case
     MOVE "LINEQTY" TO QA-FUNCTION
     MOVE 10 TO KQ-CURRENT-REQ
     MOVE 4 TO KQ-GROSS-ISSUED
     MOVE 5 TO KQ-RETURNED
     CALL "KITQTY" USING Ws-Qty-Ca
     MOVE "KIT-013 returning more than was issued is refused"
          TO TK-NAME
     IF QA-RETURN-CODE = 8 AND QA-REASON-CODE = "KIT0011"
         PERFORM TK-PASS
     ELSE
         MOVE QA-REASON-CODE TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF.

*>----------------------------------------------------------------
*> AC-081 / D-10: a ten unit run completed as six then four is
*> still ONE run, so fixed-per-run usage is not repeated and does
*> not scale with the output.
 Test-Fixed-Per-Run-Not-Repeated.
     PERFORM Reset-Case
     MOVE "SCALE" TO QA-FUNCTION
     MOVE "FIXEDRUN" TO KQ-USAGE-BASIS
     MOVE 0.300000 TO KQ-USAGE-PER
     MOVE 10 TO KQ-OLD-OUTPUT
     MOVE 6 TO KQ-NEW-OUTPUT
     MOVE 1 TO KQ-YIELD
     MOVE 1 TO KQ-RUN-COUNT
     MOVE 0.300000 TO KQ-CURRENT-REQ
     CALL "KITQTY" USING Ws-Qty-Ca
     MOVE "AC-081 fixed-per-run aid stays 0.3 litres for one run"
          TO TK-NAME
     MOVE 0.300000 TO TK-EXPECTED-N
     MOVE KQ-NEW-REQ TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "AC-081 a partial completion does not change the fixed usage"
          TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KQ-REQ-DELTA TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> Proportional usage scales with the gross starts, and the
*> additional-input scrap fraction is applied once.
 Test-Proportional-Scaling.
     PERFORM Reset-Case
     MOVE "SCALE" TO QA-FUNCTION
     MOVE "PROPORTION" TO KQ-USAGE-BASIS
     MOVE 2 TO KQ-USAGE-PER
     MOVE 10 TO KQ-OLD-OUTPUT
     MOVE 20 TO KQ-NEW-OUTPUT
     MOVE 1 TO KQ-YIELD
     MOVE 0 TO KQ-SCRAP-PCT
     MOVE 20 TO KQ-CURRENT-REQ
     CALL "KITQTY" USING Ws-Qty-Ca
     MOVE "KIT-016 doubling the output doubles proportional usage"
          TO TK-NAME
     MOVE 40 TO TK-EXPECTED-N
     MOVE KQ-NEW-REQ TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Reset-Case
     MOVE "SCALE" TO QA-FUNCTION
     MOVE "PROPORTION" TO KQ-USAGE-BASIS
     MOVE 1 TO KQ-USAGE-PER
     MOVE 10 TO KQ-NEW-OUTPUT
     MOVE 1 TO KQ-YIELD
     MOVE 4 TO KQ-SCRAP-PCT
     CALL "KITQTY" USING Ws-Qty-Ca
     MOVE "BOM B3 4 per cent allowance on ten gives 10.4" TO TK-NAME
     MOVE 10.400000 TO TK-EXPECTED-N
     MOVE KQ-NEW-REQ TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> AC-087: zero output requires no runs, no batches and no fixed
*> material at all.
 Test-Zero-Output.
     PERFORM Reset-Case
     MOVE "SCALE" TO QA-FUNCTION
     MOVE "FIXEDRUN" TO KQ-USAGE-BASIS
     MOVE 5 TO KQ-USAGE-PER
     MOVE 0 TO KQ-NEW-OUTPUT
     MOVE 1 TO KQ-YIELD
     MOVE 1 TO KQ-RUN-COUNT
     CALL "KITQTY" USING Ws-Qty-Ca
     MOVE "AC-087 zero output needs no fixed material" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KQ-NEW-REQ TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "AC-087 zero output needs no runs" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KQ-RUN-COUNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> KIT-017: gross issued = returned + scrapped + consumed + still
*> held on the job.  Seven issued, two returned, one scrapped,
*> three consumed and one still on the job balances exactly.
 Test-Accountability.
     PERFORM Reset-Case
     MOVE "ACCOUNT" TO QA-FUNCTION
     MOVE 10 TO KQ-CURRENT-REQ
     MOVE 7 TO KQ-GROSS-ISSUED
     MOVE 2 TO KQ-RETURNED
     MOVE 1 TO KQ-SCRAPPED
     MOVE 3 TO KQ-CONSUMED
     MOVE 1 TO KQ-ON-JOB
     CALL "KITQTY" USING Ws-Qty-Ca
     MOVE "KIT-017 issued material balances to zero difference"
          TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KQ-ACCOUNT-DIFF TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "KIT-017 unconsumed material on the job is still flagged"
          TO TK-NAME
     IF QA-RETURN-CODE = 4 AND QA-REASON-CODE = "KIT0017"
         PERFORM TK-PASS
     ELSE
         MOVE QA-REASON-CODE TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF.

 Test-Unexplained-Material.
     PERFORM Reset-Case
     MOVE "ACCOUNT" TO QA-FUNCTION
     MOVE 10 TO KQ-CURRENT-REQ
     MOVE 7 TO KQ-GROSS-ISSUED
     MOVE 2 TO KQ-RETURNED
     MOVE 0 TO KQ-SCRAPPED
     MOVE 3 TO KQ-CONSUMED
     MOVE 0 TO KQ-ON-JOB
     CALL "KITQTY" USING Ws-Qty-Ca
     MOVE "KIT-017 two unexplained units are reported, not hidden"
          TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     MOVE KQ-ACCOUNT-DIFF TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "KIT-017 the line is not accounted for" TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE KQ-ACCOUNTED TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

 Test-Bad-Function.
     PERFORM Reset-Case
     MOVE "WIBBLE" TO QA-FUNCTION
     CALL "KITQTY" USING Ws-Qty-Ca
     MOVE "An unsupported function is refused with 08 / KIT0002"
          TO TK-NAME
     IF QA-RETURN-CODE = 8 AND QA-REASON-CODE = "KIT0002"
         PERFORM TK-PASS
     ELSE
         MOVE QA-REASON-CODE TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF.

 COPY TESTKITP.
