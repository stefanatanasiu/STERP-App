      *>================================================================
      *> PROGRAM-ID : TLANDCOS
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : UNIT TESTS FOR LANDCOST USING THE WORKED EXAMPLE
      *>              OF SPECIFICATION SECTION 8 AND AC-056: TWO
      *>              RECEIVED PRODUCT GROUPS WITH ACQUISITION VALUES
      *>              OF 600.00 AND 400.00 AND A FREIGHT CHARGE OF
      *>              75.00 ALLOCATED BY VALUE PRODUCE 45.00 AND 30.00
      *>              - NOT 75.00 ON EACH GROUP.
      *>              THE CALC FUNCTION HAS NO DATABASE EFFECT.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: PO-015, PO-016   AC: AC-056
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLANDCOS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY LANDCOSTCA REPLACING ==DFHCOMMAREA== BY ==WS-LC==.
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           MOVE "po LANDCOST" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-BY-VALUE THRU 1000-EXIT
           PERFORM 1100-ZERO-BASIS THRU 1100-EXIT
           PERFORM 1200-DELTA-ONLY THRU 1200-EXIT
           PERFORM 1300-CONSUMED-SPLIT THRU 1300-EXIT
           PERFORM 1400-UNEVEN-ROUNDING THRU 1400-EXIT
           PERFORM 1500-NOT-CAPITALISED THRU 1500-EXIT
           PERFORM TK-END
           STOP RUN.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0500-SETUP SECTION.
       0500-START.
           INITIALIZE WS-LC
           MOVE "CALC" TO CA-FUNCTION
           MOVE "STC" TO CA-COMPANY
           MOVE "RDG" TO CA-SITE
           MOVE "BUYER" TO CA-USER
           MOVE 20261006 TO CA-BUSINESS-DATE
           MOVE "FREIGHT" TO LC-CATEGORY
           MOVE "VALUE" TO LC-BASIS
           MOVE "GBP" TO LC-CURRENCY
           MOVE 1 TO LC-FX-RATE
           MOVE "N" TO LC-POST-JOURNAL
           MOVE "Y" TO LC-IS-CAPITALISE.
       0500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AC-056 ORIGINAL ALLOCATION EXAMPLE.
      *>----------------------------------------------------------------
       1000-BY-VALUE SECTION.
       1000-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE 75 TO LC-ESTIMATE-GBP
           MOVE 2 TO LC-LINE-CNT
           MOVE "GRN0001" TO LC-L-RECEIPT(1)
           MOVE 1 TO LC-L-RCP-LINE(1)
           MOVE 600 TO LC-L-BASIS-VAL(1)
           MOVE "Y" TO LC-L-ELIGIBLE(1)
           MOVE "GRN0001" TO LC-L-RECEIPT(2)
           MOVE 2 TO LC-L-RCP-LINE(2)
           MOVE 400 TO LC-L-BASIS-VAL(2)
           MOVE "Y" TO LC-L-ELIGIBLE(2)
           CALL "LANDCOST" USING WS-LC
           MOVE "AC-056 group of 600.00 receives 45.00" TO TK-NAME
           MOVE 45 TO TK-EXPECTED-N
           MOVE LC-L-SHARE-GBP(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-056 group of 400.00 receives 30.00" TO TK-NAME
           MOVE 30 TO TK-EXPECTED-N
           MOVE LC-L-SHARE-GBP(2) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-056 allocation totals the 75.00 charge"
             TO TK-NAME
           MOVE 75 TO TK-EXPECTED-N
           MOVE LC-ALLOCATED-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-056 nothing is left unallocated" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE LC-RESIDUAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-056 basis total is the 1000.00 of both groups"
             TO TK-NAME
           MOVE 1000 TO TK-EXPECTED-N
           MOVE LC-BASIS-TOTAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-056 allocation succeeds with return code 00"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A MISSING OR ZERO AGGREGATE BASIS BLOCKS ALLOCATION.
      *>----------------------------------------------------------------
       1100-ZERO-BASIS SECTION.
       1100-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE 75 TO LC-ESTIMATE-GBP
           MOVE 2 TO LC-LINE-CNT
           MOVE 0 TO LC-L-BASIS-VAL(1)
           MOVE "Y" TO LC-L-ELIGIBLE(1)
           MOVE 0 TO LC-L-BASIS-VAL(2)
           MOVE "Y" TO LC-L-ELIGIBLE(2)
           CALL "LANDCOST" USING WS-LC
           MOVE "AC-056 a zero aggregate basis blocks allocation"
             TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-056 the zero basis reason is reported" TO TK-NAME
           MOVE "LND0003" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-016: ONLY ACTUAL MINUS ESTIMATE MOVES.  AN ACTUAL OF 90.00
      *> AGAINST AN ESTIMATE OF 75.00 ALLOCATES 15.00, NOT 90.00.
      *>----------------------------------------------------------------
       1200-DELTA-ONLY SECTION.
       1200-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE 75 TO LC-ESTIMATE-GBP
           MOVE 15 TO LC-DELTA-GBP
           MOVE 2 TO LC-LINE-CNT
           MOVE 600 TO LC-L-BASIS-VAL(1)
           MOVE "Y" TO LC-L-ELIGIBLE(1)
           MOVE 400 TO LC-L-BASIS-VAL(2)
           MOVE "Y" TO LC-L-ELIGIBLE(2)
           CALL "LANDCOST" USING WS-LC
           MOVE "PO-016 only the 15.00 difference is allocated"
             TO TK-NAME
           MOVE 15 TO TK-EXPECTED-N
           MOVE LC-ALLOCATED-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-016 the 600.00 group takes 9.00 of the delta"
             TO TK-NAME
           MOVE 9 TO TK-EXPECTED-N
           MOVE LC-L-SHARE-GBP(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-016 the 400.00 group takes 6.00 of the delta"
             TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE LC-L-SHARE-GBP(2) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> UNITS ALREADY SOLD OR CONSUMED CANNOT BE REVALUED IN STOCK.
      *> HALF REMAINING AND HALF CONSUMED SPLITS THE SHARE EVENLY.
      *>----------------------------------------------------------------
       1300-CONSUMED-SPLIT SECTION.
       1300-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE 75 TO LC-ESTIMATE-GBP
           MOVE 2 TO LC-LINE-CNT
           MOVE 600 TO LC-L-BASIS-VAL(1)
           MOVE "Y" TO LC-L-ELIGIBLE(1)
           MOVE 50 TO LC-L-QTY-REMAIN(1)
           MOVE 50 TO LC-L-QTY-USED(1)
           MOVE 400 TO LC-L-BASIS-VAL(2)
           MOVE "Y" TO LC-L-ELIGIBLE(2)
           MOVE 100 TO LC-L-QTY-REMAIN(2)
           MOVE 0 TO LC-L-QTY-USED(2)
           CALL "LANDCOST" USING WS-LC
           MOVE "AC-056 consumed half of 45.00 goes to cost of sales"
             TO TK-NAME
           MOVE 22.5 TO TK-EXPECTED-N
           MOVE LC-L-TO-COGS(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-056 remaining half of 45.00 stays in stock"
             TO TK-NAME
           MOVE 22.5 TO TK-EXPECTED-N
           MOVE LC-L-TO-INV(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-056 unsold group keeps its whole 30.00 in stock"
             TO TK-NAME
           MOVE 30 TO TK-EXPECTED-N
           MOVE LC-L-TO-INV(2) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-056 inventory and cost of sales add to 75.00"
             TO TK-NAME
           MOVE 75 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = LC-TO-INVENTORY
                               + LC-TO-COGS
           PERFORM TK-ASSERT-NUM.
       1300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THREE EQUAL GROUPS AND A 100.00 CHARGE CANNOT DIVIDE EXACTLY;
      *> THE PARTS MUST STILL ADD BACK TO THE APPROVED CHARGE.
      *>----------------------------------------------------------------
       1400-UNEVEN-ROUNDING SECTION.
       1400-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE 100 TO LC-ESTIMATE-GBP
           MOVE 3 TO LC-LINE-CNT
           MOVE 100 TO LC-L-BASIS-VAL(1)
           MOVE "Y" TO LC-L-ELIGIBLE(1)
           MOVE 100 TO LC-L-BASIS-VAL(2)
           MOVE "Y" TO LC-L-ELIGIBLE(2)
           MOVE 100 TO LC-L-BASIS-VAL(3)
           MOVE "Y" TO LC-L-ELIGIBLE(3)
           CALL "LANDCOST" USING WS-LC
           MOVE "COM-011 three uneven shares still total 100.00"
             TO TK-NAME
           MOVE 100 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = LC-L-SHARE-GBP(1)
                               + LC-L-SHARE-GBP(2)
                               + LC-L-SHARE-GBP(3)
           PERFORM TK-ASSERT-NUM
           MOVE "COM-011 no residual is left behind" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE LC-RESIDUAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-015: ADMINISTRATION AND DEMURRAGE ARE EXPENSED, NOT
      *> CAPITALISED; THE SERVICE SAYS SO RATHER THAN SILENTLY
      *> ADDING THEM TO STOCK.
      *>----------------------------------------------------------------
       1500-NOT-CAPITALISED SECTION.
       1500-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE "OTHER" TO LC-CATEGORY
           MOVE 75 TO LC-ESTIMATE-GBP
           MOVE 1 TO LC-LINE-CNT
           MOVE 600 TO LC-L-BASIS-VAL(1)
           MOVE "Y" TO LC-L-ELIGIBLE(1)
           CALL "LANDCOST" USING WS-LC
           MOVE "PO-015 a non capitalisable category warns" TO TK-NAME
           MOVE "LND0004" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-015 the non capitalisable flag is cleared"
             TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE LC-IS-CAPITALISE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1500-EXIT.
           EXIT.
       COPY TESTKITP.
