      *>================================================================
      *> PROGRAM-ID : TPORCV
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : UNIT TESTS FOR THE RECEIPT, SERVICE ACCEPTANCE,
      *>              IMPORT ENTRY AND ORDER CLOSURE ENGINES.  ONLY
      *>              THE FUNCTIONS WITH NO DATABASE EFFECT ARE
      *>              EXERCISED HERE - RECEIPT "VALIDATE", SVCACC
      *>              "CALC", IMPVAT "CALC" AND POCLOSE "CHECK" -
      *>              SO THE SUITE RUNS WITHOUT A CONNECTION.
      *>              THE WORKED NUMBERS ARE THOSE OF AC-054 AND OF
      *>              SPECIFICATION 06 SECTIONS 9 AND 14.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: PO-009, PO-010, PO-012, PO-014, PO-020, PO-025
      *> AC: AC-054, AC-055
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TPORCV.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY RECEIPTCA REPLACING ==DFHCOMMAREA== BY ==WS-RC==.
       COPY SVCACCCA REPLACING ==DFHCOMMAREA== BY ==WS-SA==
                      LEADING ==CA-== BY ==SV-==.
       COPY IMPVATCA REPLACING ==DFHCOMMAREA== BY ==WS-IM==
                      LEADING ==CA-== BY ==IV-==.
       COPY POCLOSECA REPLACING ==DFHCOMMAREA== BY ==WS-PC==
                      LEADING ==CA-== BY ==PX-==.
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           MOVE "po RECEIPT SVCACC IMPVAT POCLOSE" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-RCV-GOOD THRU 1000-EXIT
           PERFORM 1100-RCV-LOT THRU 1100-EXIT
           PERFORM 1200-RCV-SERIAL THRU 1200-EXIT
           PERFORM 1300-RCV-OVER THRU 1300-EXIT
           PERFORM 1400-RCV-INSPECT THRU 1400-EXIT
           PERFORM 1500-RCV-TRANSIT THRU 1500-EXIT
           PERFORM 1600-RCV-CORR THRU 1600-EXIT
           PERFORM 2000-SVC-AC054 THRU 2000-EXIT
           PERFORM 2100-SVC-EXCESS THRU 2100-EXIT
           PERFORM 2200-SVC-EVIDENCE THRU 2200-EXIT
           PERFORM 3000-IMP-DUTY THRU 3000-EXIT
           PERFORM 3100-IMP-CONSUMED THRU 3100-EXIT
           PERFORM 3200-IMP-PREFERENCE THRU 3200-EXIT
           PERFORM 4000-CLS-CHECK THRU 4000-EXIT
           PERFORM 4100-CLS-CANCEL THRU 4100-EXIT
           PERFORM 4200-CLS-AUTHORITY THRU 4200-EXIT
           PERFORM TK-END
           STOP RUN.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0500-RCV-SETUP SECTION.
       0500-START.
           INITIALIZE WS-RC
           MOVE "VALIDATE" TO CA-FUNCTION
           MOVE "STC" TO CA-COMPANY
           MOVE "RDG" TO CA-SITE
           MOVE "STORES1" TO CA-USER
           MOVE 20261006 TO CA-BUSINESS-DATE
           MOVE "GRN90001" TO RC-RECEIPT-REF
           MOVE "PO90001" TO RC-ORDER-NUMBER
           MOVE "S0001" TO RC-SUPPLIER
           MOVE "DN-55012" TO RC-DELIVERY-NOTE
           MOVE 20261006 TO RC-PHYSICAL-DATE
           MOVE "MAIN" TO RC-WAREHOUSE
           MOVE "RECEIPT" TO RC-OWNERSHIP
           MOVE 1 TO RC-LINE-CNT
           MOVE 1 TO RC-L-NUM(1)
           MOVE 1 TO RC-L-PO-LINE(1)
           MOVE "FRM-001" TO RC-L-ITEM(1)
           MOVE 30 TO RC-L-ORDERED(1)
           MOVE 20 TO RC-L-RECEIVED(1)
           MOVE "EA" TO RC-L-UOM(1)
           MOVE 1 TO RC-L-FACTOR(1)
           MOVE "A-01-01" TO RC-L-LOCATION(1)
           MOVE "NONE" TO RC-L-TRACE(1)
           MOVE "Y" TO RC-L-STOCKED(1)
           MOVE 5 TO RC-L-PRICE(1).
       0500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AC-053 FIXTURE: ORDER 30, RECEIVE 20, TEN STILL OUTSTANDING.
      *>----------------------------------------------------------------
       1000-RCV-GOOD SECTION.
       1000-START.
           PERFORM 0500-RCV-SETUP THRU 0500-EXIT
           CALL "RECEIPT" USING WS-RC
           MOVE "Receipt of 20 against an order for 30 is accepted"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Ten units remain outstanding on the line"
             TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE RC-L-OUTSTAND(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Nothing is over delivered" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE RC-L-OVER-QTY(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Twenty units are accepted into good stock"
             TO TK-NAME
           MOVE 20 TO TK-EXPECTED-N
           MOVE RC-L-ACCEPTED(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The receipt is valued at the order price, 100.00"
             TO TK-NAME
           MOVE 100 TO TK-EXPECTED-N
           MOVE RC-TOTAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-009: TRACEABILITY IS RECORDED AT GOODS INWARD BECAUSE IT
      *> CANNOT BE RECONSTRUCTED AFTERWARDS.
      *>----------------------------------------------------------------
       1100-RCV-LOT SECTION.
       1100-START.
           PERFORM 0500-RCV-SETUP THRU 0500-EXIT
           MOVE "LOT" TO RC-L-TRACE(1)
           CALL "RECEIPT" USING WS-RC
           MOVE "A lot traceable line without a lot number fails"
             TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The rejection names the missing lot" TO TK-NAME
           MOVE "RCP0004" TO TK-EXPECTED-X
           MOVE RC-L-REASON(1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0500-RCV-SETUP THRU 0500-EXIT
           MOVE "LOT" TO RC-L-TRACE(1)
           MOVE "LOT-2610-A" TO RC-L-LOT(1)
           CALL "RECEIPT" USING WS-RC
           MOVE "The same line with its lot number is accepted"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1200-RCV-SERIAL SECTION.
       1200-START.
           PERFORM 0500-RCV-SETUP THRU 0500-EXIT
           MOVE "SERIAL" TO RC-L-TRACE(1)
           MOVE "SN-00001" TO RC-L-SERIAL(1)
           CALL "RECEIPT" USING WS-RC
           MOVE "A serial line of twenty units is refused"
             TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0500-RCV-SETUP THRU 0500-EXIT
           MOVE "SERIAL" TO RC-L-TRACE(1)
           MOVE "SN-00001" TO RC-L-SERIAL(1)
           MOVE 1 TO RC-L-RECEIVED(1)
           CALL "RECEIPT" USING WS-RC
           MOVE "One unit with one serial number is accepted"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> D-08: OVER DELIVERY IS ZERO PER CENT BY DEFAULT, SO A
      *> THIRTY ONE UNIT DELIVERY AGAINST A THIRTY UNIT ORDER NEEDS
      *> AN AMENDED ORDER BEFORE IT CAN BE CONFIRMED (PO-011).
      *>----------------------------------------------------------------
       1300-RCV-OVER SECTION.
       1300-START.
           PERFORM 0500-RCV-SETUP THRU 0500-EXIT
           MOVE 31 TO RC-L-RECEIVED(1)
           CALL "RECEIPT" USING WS-RC
           MOVE "An over delivery is held, not silently accepted"
             TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The held line is counted" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE RC-LINES-HELD TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0500-RCV-SETUP THRU 0500-EXIT
           MOVE 31 TO RC-L-RECEIVED(1)
           MOVE 5 TO RC-L-OVER-PCT(1)
           CALL "RECEIPT" USING WS-RC
           MOVE "With a 5% agreed tolerance the same receipt passes"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The excess of one unit stays separately visible"
             TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE RC-L-OVER-QTY(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "An accepted tolerance leaves zero outstanding"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE RC-L-OUTSTAND(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-010: GOODS THAT NEED INSPECTION ARRIVE IN QUARANTINE AND
      *> ARE NOT AVAILABLE, BUT THEY ARE STILL RECEIVED AND STILL
      *> ACCRUED - QUARANTINE IS NOT A FINANCIAL LIMBO.
      *>----------------------------------------------------------------
       1400-RCV-INSPECT SECTION.
       1400-START.
           PERFORM 0500-RCV-SETUP THRU 0500-EXIT
           MOVE "Y" TO RC-L-INSPECT(1)
           CALL "RECEIPT" USING WS-RC
           MOVE "Goods for inspection are quarantined, not accepted"
             TO TK-NAME
           MOVE 20 TO TK-EXPECTED-N
           MOVE RC-L-QUARANT(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Nothing goes to good stock before inspection"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE RC-L-ACCEPTED(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Quarantined goods are still valued and accrued"
             TO TK-NAME
           MOVE 100 TO TK-EXPECTED-N
           MOVE RC-TOTAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-014: AN EX WORKS SHIPMENT IS OURS WHILE IT IS STILL ON
      *> THE WATER, BUT ONLY AGAINST SHIPMENT EVIDENCE.
      *>----------------------------------------------------------------
       1500-RCV-TRANSIT SECTION.
       1500-START.
           PERFORM 0500-RCV-SETUP THRU 0500-EXIT
           MOVE "INCOTERMS" TO RC-OWNERSHIP
           MOVE "EXW" TO RC-INCOTERM
           CALL "RECEIPT" USING WS-RC
           MOVE "Ownership at the supplier's door needs evidence"
             TO TK-NAME
           MOVE "RCP0009" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0500-RCV-SETUP THRU 0500-EXIT
           MOVE "INCOTERMS" TO RC-OWNERSHIP
           MOVE "EXW" TO RC-INCOTERM
           MOVE "BL-SHA-77219" TO RC-SHIP-EVIDENCE
           MOVE SPACES TO RC-L-LOCATION(1)
           CALL "RECEIPT" USING WS-RC
           MOVE "With the bill of lading the goods are in transit"
             TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE RC-IS-TRANSIT TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "An in transit receipt needs no put away location"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-017.  THE RECEIPT, ITS STOCK MOVEMENTS AND ITS GRNI
      *> JOURNAL ARE THREE IDEMPOTENT OPERATIONS THAT SHARE ONE
      *> core.IdempotencyKey TABLE, SO THEY MUST NEVER FILE UNDER THE
      *> SAME ID - A JOURNAL FILED BEHIND A STOCK MOVEMENT LOOKS LIKE
      *> A REPLAY AND IS NEVER WRITTEN.  THE COMMAREA FIELD HOLDS 36
      *> CHARACTERS, WHICH IS EXACTLY THE LENGTH OF THE GUID THE
      *> CLIENT SENDS, SO THE DERIVED IDS ARE BUILT FROM A SHORTENED
      *> STEM THAT LEAVES ROOM FOR THEIR SUFFIX.
      *>----------------------------------------------------------------
       1600-RCV-CORR SECTION.
       1600-START.
           PERFORM 0500-RCV-SETUP THRU 0500-EXIT
           MOVE "11111111-2222-3333-4444-555555555555"
             TO CA-CORRELATION-ID
           CALL "RECEIPT" USING WS-RC
           MOVE "A caller's own correlation id is left alone"
             TO TK-NAME
           MOVE "11111111-2222-3333-4444-555555555555"
             TO TK-EXPECTED-X
           MOVE CA-CORRELATION-ID TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "A 36 character correlation id is still accepted"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0500-RCV-SETUP THRU 0500-EXIT
           MOVE SPACES TO CA-CORRELATION-ID
           CALL "RECEIPT" USING WS-RC
           MOVE "A derived correlation id names the receipt itself"
             TO TK-NAME
           MOVE "RCP-STC-GRN90001" TO TK-EXPECTED-X
           MOVE CA-CORRELATION-ID TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0600-SVC-SETUP SECTION.
       0600-START.
           INITIALIZE WS-SA
           MOVE "CALC" TO SV-FUNCTION
           MOVE "STC" TO SV-COMPANY
           MOVE "RDG" TO SV-SITE
           MOVE "PROJMGR" TO SV-USER
           MOVE 20261006 TO SV-BUSINESS-DATE
           MOVE "SVA90001" TO SA-ACCEPT-REF
           MOVE "PO90009" TO SA-ORDER-NUMBER
           MOVE 1 TO SA-ORDER-LINE
           MOVE "S0004" TO SA-SUPPLIER
           MOVE "VALUE" TO SA-BASIS
           MOVE 2000 TO SA-ORDER-VALUE
           MOVE 2000 TO SA-ORDER-VALUE-GBP
           MOVE "Signed stage report SR-14" TO SA-EVIDENCE
           MOVE "GBP" TO SA-CURRENCY
           MOVE 1 TO SA-FX-RATE.
       0600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AC-054: ACCEPTING 40% OF A 2,000.00 APPROVED SERVICE ORDER
      *> ACCRUES 800.00 AND LEAVES 1,200.00 OF COMMITMENT.
      *>----------------------------------------------------------------
       2000-SVC-AC054 SECTION.
       2000-START.
           PERFORM 0600-SVC-SETUP THRU 0600-EXIT
           MOVE 40 TO SA-ACCEPT-PCT
           CALL "SVCACC" USING WS-SA
           MOVE "AC-054 forty per cent of 2,000.00 accrues 800.00"
             TO TK-NAME
           MOVE 800 TO TK-EXPECTED-N
           MOVE SA-ACCEPT-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-054 leaves 1,200.00 of commitment" TO TK-NAME
           MOVE 1200 TO TK-EXPECTED-N
           MOVE SA-REMAIN-COMM-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "A service acceptance creates no stock quantity"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SA-STOCK-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0600-SVC-SETUP THRU 0600-EXIT
           MOVE "QUANTITY" TO SA-BASIS
           MOVE 16 TO SA-ACCEPTED-QTY
           MOVE 50 TO SA-UNIT-PRICE
           CALL "SVCACC" USING WS-SA
           MOVE "Sixteen hours at 50.00 accrues the same 800.00"
             TO TK-NAME
           MOVE 800 TO TK-EXPECTED-N
           MOVE SA-ACCEPT-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2100-SVC-EXCESS SECTION.
       2100-START.
           PERFORM 0600-SVC-SETUP THRU 0600-EXIT
           MOVE 1800 TO SA-ALREADY-ACC-GBP
           MOVE 40 TO SA-ACCEPT-PCT
           CALL "SVCACC" USING WS-SA
           MOVE "Excess service billing is refused, not accrued"
             TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE SV-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The refusal states the remaining 200.00" TO TK-NAME
           MOVE 200 TO TK-EXPECTED-N
           MOVE SA-REMAIN-COMM-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2200-SVC-EVIDENCE SECTION.
       2200-START.
           PERFORM 0600-SVC-SETUP THRU 0600-EXIT
           MOVE SPACES TO SA-EVIDENCE
           MOVE 40 TO SA-ACCEPT-PCT
           CALL "SVCACC" USING WS-SA
           MOVE "A service acceptance without evidence is refused"
             TO TK-NAME
           MOVE "SVC0004" TO TK-EXPECTED-X
           MOVE SV-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0600-SVC-SETUP THRU 0600-EXIT
           MOVE "MILESTONE" TO SA-BASIS
           MOVE 500 TO SA-MILESTONE-VALUE
           CALL "SVCACC" USING WS-SA
           MOVE "A milestone acceptance must name its deliverable"
             TO TK-NAME
           MOVE "SVC0005" TO TK-EXPECTED-X
           MOVE SV-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0600-SVC-SETUP THRU 0600-EXIT
           MOVE "REQUESTER" TO SA-REQUESTED-BY
           MOVE "REQUESTER" TO SA-ACCEPTED-BY
           MOVE 10 TO SA-ACCEPT-PCT
           CALL "SVCACC" USING WS-SA
           MOVE "The requester may not sign off their own service"
             TO TK-NAME
           MOVE "SVC0009" TO TK-EXPECTED-X
           MOVE SV-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0700-IMP-SETUP SECTION.
       0700-START.
           INITIALIZE WS-IM
           MOVE "CALC" TO IV-FUNCTION
           MOVE "STC" TO IV-COMPANY
           MOVE "RDG" TO IV-SITE
           MOVE "IMPCLERK" TO IV-USER
           MOVE 20261006 TO IV-BUSINESS-DATE
           MOVE "IMP90001" TO IM-ENTRY-REF
           MOVE "26GB00AB1234567890" TO IM-MRN
           MOVE "PVA" TO IM-VAT-METHOD
           MOVE 20 TO IM-VAT-RATE-PCT
           MOVE "26Q3" TO IM-VAT-PERIOD-KEY
           MOVE 1 TO IM-LINE-CNT
           MOVE 1 TO IM-L-NUM(1)
           MOVE "FRM-001" TO IM-L-ITEM(1)
           MOVE "73181500" TO IM-L-COMMODITY(1)
           MOVE "CN" TO IM-L-ORIGIN(1)
           MOVE 1000 TO IM-L-CUST-VALUE(1)
           MOVE 5 TO IM-L-DUTY-PCT(1).
       0700-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-025: DUTY OF 5% ON A CUSTOMS VALUE OF 1,000.00 IS 50.00
      *> AND IS CAPITALISED; IMPORT VAT OF 20% IS CHARGED ON THE
      *> CUSTOMS VALUE PLUS THE DUTY - 210.00 - AND IS NEVER LANDED
      *> COST.  UNDER POSTPONED ACCOUNTING THE NET EFFECT IS NIL.
      *>----------------------------------------------------------------
       3000-IMP-DUTY SECTION.
       3000-START.
           PERFORM 0700-IMP-SETUP THRU 0700-EXIT
           CALL "IMPVAT" USING WS-IM
           MOVE "Duty of 5% on 1,000.00 is 50.00" TO TK-NAME
           MOVE 50 TO TK-EXPECTED-N
           MOVE IM-DUTY-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Import VAT is 20% of value plus duty, 210.00"
             TO TK-NAME
           MOVE 210 TO TK-EXPECTED-N
           MOVE IM-VAT-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "All of the duty is capitalised into stock"
             TO TK-NAME
           MOVE 50 TO TK-EXPECTED-N
           MOVE IM-DUTY-TO-STOCK TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Postponed import VAT nets to nil in profit"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE IM-NET-PROFIT-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Box 1 and box 4 carry the same 210.00" TO TK-NAME
           MOVE 210 TO TK-EXPECTED-N
           MOVE IM-VAT-BOX4-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Duty is landed cost on the line" TO TK-NAME
           MOVE 50 TO TK-EXPECTED-N
           MOVE IM-L-LANDED-GBP(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> DUTY ON GOODS THAT HAVE ALREADY BEEN SOLD CANNOT BE
      *> CAPITALISED INTO STOCK THAT IS NO LONGER THERE.
      *>----------------------------------------------------------------
       3100-IMP-CONSUMED SECTION.
       3100-START.
           PERFORM 0700-IMP-SETUP THRU 0700-EXIT
           MOVE 40 TO IM-CONSUMED-PCT
           CALL "IMPVAT" USING WS-IM
           MOVE "Forty per cent of the duty goes to cost of sales"
             TO TK-NAME
           MOVE 20 TO TK-EXPECTED-N
           MOVE IM-DUTY-TO-COGS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The balance of the duty stays in stock" TO TK-NAME
           MOVE 30 TO TK-EXPECTED-N
           MOVE IM-DUTY-TO-STOCK TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       3100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3200-IMP-PREFERENCE SECTION.
       3200-START.
           PERFORM 0700-IMP-SETUP THRU 0700-EXIT
           MOVE "Y" TO IM-L-PREFERENCE(1)
           MOVE 0 TO IM-L-DUTY-PCT(1)
           CALL "IMPVAT" USING WS-IM
           MOVE "A preference claim without proof of origin fails"
             TO TK-NAME
           MOVE "IMP0009" TO TK-EXPECTED-X
           MOVE IV-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0700-IMP-SETUP THRU 0700-EXIT
           MOVE "Y" TO IM-L-PREFERENCE(1)
           MOVE 0 TO IM-L-DUTY-PCT(1)
           MOVE "EUR1-884412" TO IM-L-PROOF-REF(1)
           CALL "IMPVAT" USING WS-IM
           MOVE "With proof of origin the zero rate is allowed"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE IM-DUTY-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "VAT on a duty free import is 200.00" TO TK-NAME
           MOVE 200 TO TK-EXPECTED-N
           MOVE IM-VAT-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       3200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0800-CLS-SETUP SECTION.
       0800-START.
           INITIALIZE WS-PC
           MOVE "CHECK" TO PX-FUNCTION
           MOVE "STC" TO PX-COMPANY
           MOVE "RDG" TO PX-SITE
           MOVE "BUYER1" TO PX-USER
           MOVE 20261006 TO PX-BUSINESS-DATE
           MOVE "PO90001" TO PC-ORDER-NUMBER
           MOVE 1 TO PC-LINE-NUM
           MOVE "S0001" TO PC-SUPPLIER
           MOVE 30 TO PC-ORDERED-QTY
           MOVE 20 TO PC-RECEIVED-QTY
           MOVE 5 TO PC-UNIT-PRICE
           MOVE 100 TO PC-RECEIVED-GBP.
       0800-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AC-055: SHORT CLOSING THE REMAINING TEN UNITS RELEASES THE
      *> COMMITMENT AND LEAVES THE RECEIVED LIABILITY ALONE.
      *>----------------------------------------------------------------
       4000-CLS-CHECK SECTION.
       4000-START.
           PERFORM 0800-CLS-SETUP THRU 0800-EXIT
           CALL "POCLOSE" USING WS-PC
           MOVE "Ten units are outstanding on the line" TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE PC-OUTSTAND-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Closing would release 50.00 of commitment"
             TO TK-NAME
           MOVE 50 TO TK-EXPECTED-N
           MOVE PC-RELEASED-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The received 100.00 of GRNI is retained"
             TO TK-NAME
           MOVE 100 TO TK-EXPECTED-N
           MOVE PC-RETAINED-GRNI TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0800-CLS-SETUP THRU 0800-EXIT
           MOVE 500 TO PC-OPEN-CREDIT-GBP
           CALL "POCLOSE" USING WS-PC
           MOVE "An expected supplier credit survives closure"
             TO TK-NAME
           MOVE "PCL0008" TO TK-EXPECTED-X
           MOVE PX-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4100-CLS-CANCEL SECTION.
       4100-START.
           PERFORM 0800-CLS-SETUP THRU 0800-EXIT
           MOVE "CANCEL" TO PX-FUNCTION
           MOVE "SUPPLIERFAIL" TO PC-REASON-CODE
           MOVE "Supplier cannot supply the balance"
             TO PC-REASON
           CALL "POCLOSE" USING WS-PC
           MOVE "A received line cannot be cancelled away"
             TO TK-NAME
           MOVE "PCL0009" TO TK-EXPECTED-X
           MOVE PX-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0800-CLS-SETUP THRU 0800-EXIT
           MOVE "CANCEL" TO PX-FUNCTION
           CALL "POCLOSE" USING WS-PC
           MOVE "A cancellation without a reason is refused"
             TO TK-NAME
           MOVE "PCL0002" TO TK-EXPECTED-X
           MOVE PX-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       4100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4200-CLS-AUTHORITY SECTION.
       4200-START.
           PERFORM 0800-CLS-SETUP THRU 0800-EXIT
           MOVE "SHORTCLS" TO PX-FUNCTION
           MOVE "NOLONGER" TO PC-REASON-CODE
           MOVE "Requirement withdrawn" TO PC-REASON
           CALL "POCLOSE" USING WS-PC
           MOVE "A short close without an authority is refused"
             TO TK-NAME
           MOVE "PCL0003" TO TK-EXPECTED-X
           MOVE PX-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0800-CLS-SETUP THRU 0800-EXIT
           MOVE "REOPEN" TO PX-FUNCTION
           MOVE "RESUPPLY" TO PC-REASON-CODE
           MOVE "Supplier found stock after all" TO PC-REASON
           CALL "POCLOSE" USING WS-PC
           MOVE "Reopening a closed order also needs an authority"
             TO TK-NAME
           MOVE "PCL0003" TO TK-EXPECTED-X
           MOVE PX-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       4200-EXIT.
           EXIT.
       COPY TESTKITP.
