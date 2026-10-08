      *>================================================================
      *> PROGRAM-ID : TCSTCALC
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY (UNIT TESTS)
      *> PURPOSE    : THE CALCULATION RULES OF THE COSTING MODULE,
      *>              USING THE WORKED EXAMPLES PRINTED IN SECTION 8
      *>              OF SPECIFICATION 07.  NONE OF THESE PATHS TOUCH
      *>              THE DATABASE, SO THE SUITE RUNS ANYWHERE.
      *> COVERS     : C3 STANDARD RECEIPT AND INVOICE VARIANCE
      *>              C4 PROVISIONAL LANDED UNIT COST
      *>              C5 MATERIAL USAGE AND PRICE VARIANCE
      *>              C7 RETURN AFTER A STANDARD COST CHANGE
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: CST-002, CST-003, CST-004, CST-007, CST-009, CST-011
      *> REQ: CST-013, CST-019, CST-026
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCSTCALC.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY STDCOSTCA  REPLACING ==DFHCOMMAREA== BY ==WS-SC-CA==.
       COPY VARIANCECA REPLACING ==DFHCOMMAREA== BY ==WS-VA-CA==.
       COPY MARGINCA   REPLACING ==DFHCOMMAREA== BY ==WS-MG-CA==.
       COPY LABOVHCA   REPLACING ==DFHCOMMAREA== BY ==WS-LO-CA==.
       COPY COSTNORMCA REPLACING ==DFHCOMMAREA== BY ==WS-CN-CA==.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TCSTCALC costing calculations" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-STANDARD-RECEIPT
           PERFORM 1100-INVOICE-VARIANCE
           PERFORM 1200-RETURN-VARIANCE
           PERFORM 2000-MATERIAL-VARIANCE
           PERFORM 3000-MARGIN-RULES
           PERFORM 4000-LABOUR-OVERHEAD
           PERFORM 5000-ACQUISITION-COST
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
      *> AC: C3 - RECEIVE 100 AT STANDARD 9.00 ORDERED AT 10.00
      *>          Dr INVENTORY 900.00  Dr PRICE VARIANCE 100.00
      *>----------------------------------------------------------------
       1000-STANDARD-RECEIPT.
           INITIALIZE WS-SC-CA
           MOVE "RECEIPT" TO CA-FUNCTION OF WS-SC-CA
           MOVE "STC" TO CA-COMPANY OF WS-SC-CA
           MOVE "COSTACC" TO CA-USER OF WS-SC-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-SC-CA
           MOVE "CST-C3-ITEM" TO SC-ITEM-CODE
           MOVE 100 TO SC-QUANTITY
           MOVE 9.00 TO SC-STD-UNIT-COST
           MOVE 10.00 TO SC-ORDER-PRICE
           MOVE 1 TO SC-LOT-SIZE
           MOVE "N" TO SC-COMMIT
           CALL "STDCOST" USING WS-SC-CA
           MOVE "C3 receipt values inventory at standard 900.00"
               TO TK-NAME
           MOVE 900.00 TO TK-EXPECTED-N
           MOVE SC-INVENTORY-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C3 goods received not invoiced accrues 1,000.00"
               TO TK-NAME
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE SC-GRNI-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C3 receipt price variance is 100.00 adverse"
               TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE SC-PPV-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C3 the receipt variance is flagged adverse"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SC-ADVERSE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>  AC: C3 - THE INVOICE ADDS ONLY THE INCREMENTAL 20.00, SO THE
      *>  TOTAL ADVERSE VARIANCE IS 120.00 AND NEVER 220.00.
       1100-INVOICE-VARIANCE.
           INITIALIZE WS-SC-CA
           MOVE "INVOICE" TO CA-FUNCTION OF WS-SC-CA
           MOVE "STC" TO CA-COMPANY OF WS-SC-CA
           MOVE "COSTACC" TO CA-USER OF WS-SC-CA
           MOVE "CST-C3-ITEM" TO SC-ITEM-CODE
           MOVE 100 TO SC-QUANTITY
           MOVE 9.00 TO SC-STD-UNIT-COST
           MOVE 10.00 TO SC-ORDER-PRICE
           MOVE 1020.00 TO SC-INVOICE-AMOUNT
           MOVE 1 TO SC-LOT-SIZE
           MOVE "N" TO SC-COMMIT
           CALL "STDCOST" USING WS-SC-CA
           MOVE "C3 incremental invoice price variance is 20.00"
               TO TK-NAME
           MOVE 20.00 TO TK-EXPECTED-N
           MOVE SC-INCREMENTAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C3 total adverse variance is 120.00 not 220.00"
               TO TK-NAME
           MOVE 120.00 TO TK-EXPECTED-N
           MOVE SC-TOTAL-VAR-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>  AC: C7 - A RETURNED UNIT COMES BACK AT THE CURRENT APPROVED
      *>  STANDARD WHILE THE ORIGINAL 12.00 COGS RELIEF IS REVERSED.
       1200-RETURN-VARIANCE.
           INITIALIZE WS-SC-CA
           MOVE "RETURN" TO CA-FUNCTION OF WS-SC-CA
           MOVE "STC" TO CA-COMPANY OF WS-SC-CA
           MOVE "COSTACC" TO CA-USER OF WS-SC-CA
           MOVE "CST-C7-ITEM" TO SC-ITEM-CODE
           MOVE 1 TO SC-QUANTITY
           MOVE 10.00 TO SC-STD-UNIT-COST
           MOVE 12.00 TO SC-ORIGINAL-COGS
           MOVE 1 TO SC-LOT-SIZE
           MOVE "N" TO SC-COMMIT
           CALL "STDCOST" USING WS-SC-CA
           MOVE "C7 return at a lower standard restores 10.00"
               TO TK-NAME
           MOVE 10.00 TO TK-EXPECTED-N
           MOVE SC-RETURN-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C7 return valuation variance is 2.00 adverse"
               TO TK-NAME
           MOVE 2.00 TO TK-EXPECTED-N
           MOVE SC-RETURN-VAR-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           INITIALIZE WS-SC-CA
           MOVE "RETURN" TO CA-FUNCTION OF WS-SC-CA
           MOVE "STC" TO CA-COMPANY OF WS-SC-CA
           MOVE "COSTACC" TO CA-USER OF WS-SC-CA
           MOVE "CST-C7-ITEM" TO SC-ITEM-CODE
           MOVE 1 TO SC-QUANTITY
           MOVE 14.00 TO SC-STD-UNIT-COST
           MOVE 12.00 TO SC-ORIGINAL-COGS
           MOVE 1 TO SC-LOT-SIZE
           MOVE "N" TO SC-COMMIT
           CALL "STDCOST" USING WS-SC-CA
           MOVE "C7 return at a higher standard restores 14.00"
               TO TK-NAME
           MOVE 14.00 TO TK-EXPECTED-N
           MOVE SC-RETURN-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C7 the credit variance is 2.00 favourable"
               TO TK-NAME
           MOVE -2.00 TO TK-EXPECTED-N
           MOVE SC-RETURN-VAR-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> AC: C5 - ALLOWED 20 AT 4.00, ACTUAL 22 AT 4.50
      *>          USAGE 8.00, PRICE 11.00, TOTAL 19.00 ADVERSE
      *>----------------------------------------------------------------
       2000-MATERIAL-VARIANCE.
           INITIALIZE WS-VA-CA
           MOVE "MATERIAL" TO CA-FUNCTION OF WS-VA-CA
           MOVE "STC" TO CA-COMPANY OF WS-VA-CA
           MOVE "COSTACC" TO CA-USER OF WS-VA-CA
           MOVE "CST-C5-MAT" TO VA-ITEM-CODE
           MOVE 20 TO VA-BASELINE-QTY
           MOVE 22 TO VA-ACTUAL-QTY
           MOVE 4.00 TO VA-BASELINE-RATE
           MOVE 4.50 TO VA-ACTUAL-RATE
           MOVE "N" TO VA-COMMIT
           CALL "VARIANCE" USING WS-VA-CA
           MOVE "C5 allowed standard cost is 80.00" TO TK-NAME
           MOVE 80.00 TO TK-EXPECTED-N
           MOVE VA-BASELINE-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C5 actual cost is 99.00" TO TK-NAME
           MOVE 99.00 TO TK-EXPECTED-N
           MOVE VA-ACTUAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C5 usage variance is 8.00" TO TK-NAME
           MOVE 8.00 TO TK-EXPECTED-N
           MOVE VA-QTY-VAR-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C5 price variance is 11.00" TO TK-NAME
           MOVE 11.00 TO TK-EXPECTED-N
           MOVE VA-RATE-VAR-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C5 total adverse variance is 19.00" TO TK-NAME
           MOVE 19.00 TO TK-EXPECTED-N
           MOVE VA-TOTAL-VAR-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C5 components reconcile to the total difference"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE VA-RECONCILED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the baseline quantity is disclosed" TO TK-NAME
           MOVE 20 TO TK-EXPECTED-N
           MOVE VA-ALLOWED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> CST-013 - MARGIN IS ON EX VAT NET REVENUE AND A ZERO
      *>           DENOMINATOR IS UNDEFINED, NOT ZERO.
      *>----------------------------------------------------------------
       3000-MARGIN-RULES.
           INITIALIZE WS-MG-CA
           MOVE "EXPECTED" TO CA-FUNCTION OF WS-MG-CA
           MOVE "STC" TO CA-COMPANY OF WS-MG-CA
           MOVE "SALESMGR" TO CA-USER OF WS-MG-CA
           MOVE 1000.00 TO MG-GROSS-REVENUE
           MOVE 600.00 TO MG-COST-AMOUNT
           MOVE "STANDARD" TO MG-COST-BASIS
           CALL "MARGIN" USING WS-MG-CA
           MOVE "margin on 1,000.00 less 600.00 is 400.00"
               TO TK-NAME
           MOVE 400.00 TO TK-EXPECTED-N
           MOVE MG-MARGIN-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "margin percentage is 40.0000" TO TK-NAME
           MOVE 40.0000 TO TK-EXPECTED-N
           MOVE MG-MARGIN-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the result is reported as expected not realised"
               TO TK-NAME
           MOVE "EXPECTED" TO TK-EXPECTED-X
           MOVE MG-RESULT-NATURE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-MG-CA
           MOVE "EXPECTED" TO CA-FUNCTION OF WS-MG-CA
           MOVE "STC" TO CA-COMPANY OF WS-MG-CA
           MOVE "SALESMGR" TO CA-USER OF WS-MG-CA
           MOVE 1200.00 TO MG-GROSS-REVENUE
           MOVE 20.0000 TO MG-VAT-RATE
           MOVE "Y" TO MG-REVENUE-INCL-VAT
           MOVE 600.00 TO MG-COST-AMOUNT
           CALL "MARGIN" USING WS-MG-CA
           MOVE "VAT is stripped from a gross 1,200.00 to 1,000.00"
               TO TK-NAME
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE MG-NET-REVENUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           INITIALIZE WS-MG-CA
           MOVE "EXPECTED" TO CA-FUNCTION OF WS-MG-CA
           MOVE "STC" TO CA-COMPANY OF WS-MG-CA
           MOVE "SALESMGR" TO CA-USER OF WS-MG-CA
           MOVE 0 TO MG-GROSS-REVENUE
           MOVE 600.00 TO MG-COST-AMOUNT
           CALL "MARGIN" USING WS-MG-CA
           MOVE "zero revenue makes the margin percentage undefined"
               TO TK-NAME
           MOVE "UNDEFINED" TO TK-EXPECTED-X
           MOVE MG-MARGIN-STATE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-MG-CA
           MOVE "EXPECTED" TO CA-FUNCTION OF WS-MG-CA
           MOVE "STC" TO CA-COMPANY OF WS-MG-CA
           MOVE "SALESMGR" TO CA-USER OF WS-MG-CA
           MOVE 1000.00 TO MG-GROSS-REVENUE
           MOVE 0 TO MG-COST-AMOUNT MG-QUANTITY
           CALL "MARGIN" USING WS-MG-CA
           MOVE "a missing cost makes the markup undefined"
               TO TK-NAME
           MOVE "UNDEFINED" TO TK-EXPECTED-X
           MOVE MG-MARKUP-STATE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a missing cost makes the result incomplete"
               TO TK-NAME
           MOVE "INCOMPLETE" TO TK-EXPECTED-X
           MOVE MG-COMPLETENESS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> CST-011 AND CST-026 - SETUP IS PER RUN, OVERHEAD IS A RATE
      *>           PER HOUR AT NORMAL CAPACITY.
      *>----------------------------------------------------------------
       4000-LABOUR-OVERHEAD.
           INITIALIZE WS-LO-CA
           MOVE "LABOUR" TO CA-FUNCTION OF WS-LO-CA
           MOVE "STC" TO CA-COMPANY OF WS-LO-CA
           MOVE "ENGMGR" TO CA-USER OF WS-LO-CA
           MOVE 100 TO LO-GROSS-UNITS
           MOVE 2 TO LO-RUN-COUNT
           MOVE 2 TO LO-SETUP-HOURS
           MOVE 30.00 TO LO-SETUP-RATE
           MOVE 0.5 TO LO-RUN-HOURS-UNIT
           MOVE 20.00 TO LO-RUN-RATE
           MOVE "UNIT" TO LO-RATE-BASIS
           CALL "LABOVH" USING WS-LO-CA
           MOVE "setup is charged per run - 2h x 30.00 x 2 = 120.00"
               TO TK-NAME
           MOVE 120.00 TO TK-EXPECTED-N
           MOVE LO-SETUP-COST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "run labour is 0.5h x 100 units x 20.00 = 1,000.00"
               TO TK-NAME
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE LO-RUN-COST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "total labour is 1,120.00" TO TK-NAME
           MOVE 1120.00 TO TK-EXPECTED-N
           MOVE LO-LABOUR-COST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "labour hours are 4 setup plus 50 run" TO TK-NAME
           MOVE 54 TO TK-EXPECTED-N
           MOVE LO-TOTAL-HOURS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           INITIALIZE WS-LO-CA
           MOVE "OVHRATE" TO CA-FUNCTION OF WS-LO-CA
           MOVE "STC" TO CA-COMPANY OF WS-LO-CA
           MOVE "COSTACC" TO CA-USER OF WS-LO-CA
           MOVE 120000.00 TO LO-BUDGET-OVHD-GBP
           MOVE 8000 TO LO-NORMAL-CAP-HRS
           CALL "LABOVH" USING WS-LO-CA
           MOVE "recovery rate is budget over normal capacity hours"
               TO TK-NAME
           MOVE 15.00 TO TK-EXPECTED-N
           MOVE LO-DERIVED-RATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           INITIALIZE WS-LO-CA
           MOVE "ABSORB" TO CA-FUNCTION OF WS-LO-CA
           MOVE "STC" TO CA-COMPANY OF WS-LO-CA
           MOVE "COSTACC" TO CA-USER OF WS-LO-CA
           MOVE 8000 TO LO-NORMAL-CAP-HRS
           MOVE 6000 TO LO-ACTUAL-HOURS
           MOVE 15.00 TO LO-OVHD-RATE
           CALL "LABOVH" USING WS-LO-CA
           MOVE "low production under recovers 30,000.00 of burden"
               TO TK-NAME
           MOVE -30000.00 TO TK-EXPECTED-N
           MOVE LO-RECOVERY-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "under recovery is flagged as a period expense"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE LO-UNDER-RECOVERED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> CST-003 AND CST-004 - NORMALISATION AND THE ACQUISITION COST
      *>           OF A RECEIPT.  AC: C4 PROVISIONAL UNIT COST 10.80.
      *>----------------------------------------------------------------
       5000-ACQUISITION-COST.
           INITIALIZE WS-CN-CA
           MOVE "NORMCOST" TO CA-FUNCTION OF WS-CN-CA
           MOVE "STC" TO CA-COMPANY OF WS-CN-CA
           MOVE "BUYER" TO CA-USER OF WS-CN-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-CN-CA
           MOVE "CST-C4-ITEM" TO CN-ITEM-CODE
           MOVE "EUR" TO CN-CURRENCY
           MOVE 60.00 TO CN-FOREIGN-PRICE
           MOVE 1 TO CN-PRICE-PER-QTY
           MOVE 1.20 TO CN-FX-RATE
           MOVE 1 TO CN-QUANTITY
           MOVE "ENQUIRY" TO CN-PURPOSE
           CALL "COSTNORM" USING WS-CN-CA
           MOVE "EUR 60.00 at 1.20 per GBP is a 50.00 unit cost"
               TO TK-NAME
           MOVE 50.000000 TO TK-EXPECTED-N
           MOVE CN-UNIT-COST-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the normalised result is reported in sterling"
               TO TK-NAME
           MOVE "GBP" TO TK-EXPECTED-X
           MOVE CN-CURRENCY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-CN-CA
           MOVE "PURCOST" TO CA-FUNCTION OF WS-CN-CA
           MOVE "STC" TO CA-COMPANY OF WS-CN-CA
           MOVE "BUYER" TO CA-USER OF WS-CN-CA
           MOVE "CST-C4-ITEM" TO CN-ITEM-CODE
           MOVE "GBP" TO CN-CURRENCY
           MOVE 10.00 TO CN-FOREIGN-PRICE
           MOVE 1 TO CN-PRICE-PER-QTY
           MOVE 100 TO CN-QUANTITY
           MOVE 80.00 TO CN-FREIGHT-GBP
           MOVE 20.0000 TO CN-VAT-RATE
           MOVE 100.0000 TO CN-VAT-RECOVER-PCT
           CALL "COSTNORM" USING WS-CN-CA
           MOVE "C4 goods 1,000.00 plus freight 80.00 is 1,080.00"
               TO TK-NAME
           MOVE 1080.00 TO TK-EXPECTED-N
           MOVE CN-EXT-COST-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C4 provisional unit cost is 10.80" TO TK-NAME
           MOVE 10.800000 TO TK-EXPECTED-N
           MOVE CN-UNIT-COST-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "recoverable VAT is excluded from inventory cost"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CN-NONREC-VAT-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the excluded recoverable VAT is reported as 216.00"
               TO TK-NAME
           MOVE 216.00 TO TK-EXPECTED-N
           MOVE CN-EL-EXCLUDED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "cost elements reconcile to the total cost"
               TO TK-NAME
           MOVE 1080.00 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = CN-EL-MATERIAL + CN-EL-LANDED
           PERFORM TK-ASSERT-NUM
           INITIALIZE WS-CN-CA
           MOVE "PURCOST" TO CA-FUNCTION OF WS-CN-CA
           MOVE "STC" TO CA-COMPANY OF WS-CN-CA
           MOVE "BUYER" TO CA-USER OF WS-CN-CA
           MOVE "CST-DUTY-ITEM" TO CN-ITEM-CODE
           MOVE "GBP" TO CN-CURRENCY
           MOVE 10.00 TO CN-FOREIGN-PRICE
           MOVE 1 TO CN-PRICE-PER-QTY
           MOVE 100 TO CN-QUANTITY
           MOVE 5.0000 TO CN-DUTY-PCT
           MOVE 20.0000 TO CN-VAT-RATE
           MOVE 0 TO CN-VAT-RECOVER-PCT
           CALL "COSTNORM" USING WS-CN-CA
           MOVE "customs duty of 5 per cent adds 50.00" TO TK-NAME
           MOVE 50.00 TO TK-EXPECTED-N
           MOVE CN-DUTY-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "irrecoverable VAT of 210.00 is capitalised"
               TO TK-NAME
           MOVE 210.00 TO TK-EXPECTED-N
           MOVE CN-NONREC-VAT-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the landed acquisition cost is 1,260.00" TO TK-NAME
           MOVE 1260.00 TO TK-EXPECTED-N
           MOVE CN-EXT-COST-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           INITIALIZE WS-CN-CA
           MOVE "PURCOST" TO CA-FUNCTION OF WS-CN-CA
           MOVE "STC" TO CA-COMPANY OF WS-CN-CA
           MOVE "BUYER" TO CA-USER OF WS-CN-CA
           MOVE "CST-BAD-ITEM" TO CN-ITEM-CODE
           MOVE 10.00 TO CN-FOREIGN-PRICE
           MOVE 0 TO CN-QUANTITY
           CALL "COSTNORM" USING WS-CN-CA
           MOVE "a zero quantity is rejected, not divided by"
               TO TK-NAME
           MOVE "CST0103" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CN-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       COPY TESTKITP.
