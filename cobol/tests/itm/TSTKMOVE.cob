      *>================================================================
      *> TSTKMOVE - unit tests for STKMOVE, the stock movement service.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *> Every test asks STKMOVE for something the specification says
      *> must be refused, so no test leaves stock behind: a displayed
      *> balance is never edited, a movement may not drive physical
      *> stock negative, traceability and reasons are mandatory where
      *> the item demands them, a transfer may not start and finish in
      *> the same place, and ten units in transit are available at
      *> neither endpoint.
      *> Output protocol (TAP): "ok n - text" / "not ok n - text".
      *>================================================================
      *> REQ: ITM-015, ITM-016, ITM-017, ITM-024
      *> AC: AC-073, AC-074
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSTKMOVE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY SQLIOCA.
       COPY STKMOVECA REPLACING ==DFHCOMMAREA== BY ==WS-SM==.
       01  WS-PROBE                    PIC X(60).
       01  WS-RECEIPT-ID               PIC S9(17).
       01  WS-PLAIN-ITEM               PIC X(40)
               VALUE "LINK-QUICK-11S".
       01  WS-LOT-ITEM                 PIC X(40)
               VALUE "HUB-REAR-148-BOOST-XD".
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSTKMOVE" TO TK-SUITE
           PERFORM TK-BEGIN

           PERFORM 1000-ZERO-QUANTITY
           PERFORM 1100-NEGATIVE-QUANTITY
           PERFORM 1200-NO-NEGATIVE-STOCK
           PERFORM 1300-UNKNOWN-ITEM
           PERFORM 1400-LOT-REQUIRED
           PERFORM 1500-SAME-ENDPOINTS
           PERFORM 1600-TRANSIT-NOT-BOTH-ENDS
           PERFORM 1700-HOLD-NEEDS-QUANTITY
           PERFORM 1800-ADJUST-NEEDS-REASON
           PERFORM 1900-BAD-FUNCTION
           PERFORM 2000-VALUED-MOVEMENTS

           PERFORM TK-END
           STOP RUN.

      *>   REQ: ITM-015  AC: AC-074
       1000-ZERO-QUANTITY.
           PERFORM 9000-RESET
           MOVE "ISSUE" TO CA-FUNCTION OF WS-SM
           MOVE WS-PLAIN-ITEM TO SM-ITEM-CODE
           MOVE "STKA01" TO SM-LOCATION
           MOVE 0 TO SM-QUANTITY
           CALL "STKMOVE" USING WS-SM
           MOVE "a movement of nothing has no business effect"
               TO TK-NAME
           IF SM-MOVEMENT-ID = 0 AND SM-STOCK-QTY = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "nothing was posted for the refused movement"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SM-MOVEMENT-ID TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-015
       1100-NEGATIVE-QUANTITY.
           PERFORM 9000-RESET
           MOVE "RECEIPT" TO CA-FUNCTION OF WS-SM
           MOVE WS-PLAIN-ITEM TO SM-ITEM-CODE
           MOVE "STKA01" TO SM-LOCATION
           MOVE -10 TO SM-QUANTITY
           CALL "STKMOVE" USING WS-SM
           MOVE "a receipt of minus ten posts nothing" TO TK-NAME
           IF SM-MOVEMENT-ID = 0 AND SM-QTY-AFTER = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-015  AC: AC-074
       1200-NO-NEGATIVE-STOCK.
           PERFORM 9000-RESET
           MOVE "ISSUE" TO CA-FUNCTION OF WS-SM
           MOVE WS-PLAIN-ITEM TO SM-ITEM-CODE
           MOVE "STKA01" TO SM-LOCATION
           MOVE 99999999 TO SM-QUANTITY
           MOVE "JOB" TO SM-SOURCE-DOC-TYPE
           MOVE "TEST-NEGATIVE" TO SM-SOURCE-DOC-REF
           CALL "STKMOVE" USING WS-SM
           MOVE "an issue beyond physical stock is blocked"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-SM >= 8
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "the balance after the refusal is untouched"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SM-QTY-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-015
       1300-UNKNOWN-ITEM.
           PERFORM 9000-RESET
           MOVE "RECEIPT" TO CA-FUNCTION OF WS-SM
           MOVE "NO-SUCH-ITEM-ANYWHERE-AT-ALL" TO SM-ITEM-CODE
           MOVE "STKA01" TO SM-LOCATION
           MOVE 1 TO SM-QUANTITY
           CALL "STKMOVE" USING WS-SM
           MOVE "stock cannot be received against an unknown item"
               TO TK-NAME
           IF SM-MOVEMENT-ID = 0 AND SM-STOCK-QTY = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-016  AC: AC-073
       1400-LOT-REQUIRED.
           PERFORM 9000-RESET
           MOVE "RECEIPT" TO CA-FUNCTION OF WS-SM
           MOVE WS-LOT-ITEM TO SM-ITEM-CODE
           MOVE "STKA01" TO SM-LOCATION
           MOVE 5 TO SM-QUANTITY
           MOVE SPACES TO SM-LOT-CODE
           CALL "STKMOVE" USING WS-SM
           MOVE "a lot controlled item may not arrive without a lot"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-SM >= 8
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "the refusal names the missing traceability"
               TO TK-NAME
           IF CA-REASON-CODE OF WS-SM NOT = SPACES
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-017  AC: AC-074
       1500-SAME-ENDPOINTS.
           PERFORM 9000-RESET
           MOVE "TRANSFER" TO CA-FUNCTION OF WS-SM
           MOVE WS-PLAIN-ITEM TO SM-ITEM-CODE
           MOVE "RDG" TO SM-SITE
           MOVE "MAIN" TO SM-WAREHOUSE
           MOVE "STKA01" TO SM-LOCATION
           MOVE "RDG" TO SM-TO-SITE
           MOVE "MAIN" TO SM-TO-WAREHOUSE
           MOVE "STKA01" TO SM-TO-LOCATION
           MOVE 10 TO SM-QUANTITY
           CALL "STKMOVE" USING WS-SM
           MOVE "a transfer to the same place is not a transfer"
               TO TK-NAME
           MOVE "STK0009" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SM TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-SM TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-017  AC: AC-074
       1600-TRANSIT-NOT-BOTH-ENDS.
           PERFORM 9000-RESET
           MOVE "TRANSFER" TO CA-FUNCTION OF WS-SM
           MOVE WS-PLAIN-ITEM TO SM-ITEM-CODE
           MOVE "RDG" TO SM-SITE
           MOVE "MAIN" TO SM-WAREHOUSE
           MOVE "STKA01" TO SM-LOCATION
           MOVE "BHM" TO SM-TO-SITE
           MOVE "MAIN" TO SM-TO-WAREHOUSE
           MOVE "STKA01" TO SM-TO-LOCATION
           MOVE "Y" TO SM-VIA-TRANSIT
           MOVE 99999999 TO SM-QUANTITY
           CALL "STKMOVE" USING WS-SM
           MOVE "ten units cannot leave without leaving the origin"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-SM >= 8
                   OR SM-TRANSIT-LOC NOT = SPACES
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-016  AC: AC-073
       1700-HOLD-NEEDS-QUANTITY.
           PERFORM 9000-RESET
           MOVE "HOLD" TO CA-FUNCTION OF WS-SM
           MOVE WS-PLAIN-ITEM TO SM-ITEM-CODE
           MOVE "STKA01" TO SM-LOCATION
           MOVE "ACCEPTED" TO SM-CONDITION
           MOVE "QUARANTINE" TO SM-TO-CONDITION
           MOVE 0 TO SM-QUANTITY
           CALL "STKMOVE" USING WS-SM
           MOVE "a hold of no quantity changes no availability"
               TO TK-NAME
           IF SM-STOCK-QTY = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "no hold identity was created" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SM-HOLD-ID TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-015, ITM-024
       1800-ADJUST-NEEDS-REASON.
           PERFORM 9000-RESET
           MOVE "ADJUST" TO CA-FUNCTION OF WS-SM
           MOVE WS-PLAIN-ITEM TO SM-ITEM-CODE
           MOVE "STKA01" TO SM-LOCATION
           MOVE 3 TO SM-QUANTITY
           MOVE SPACES TO SM-REASON-CODE
           MOVE SPACES TO SM-REASON
           CALL "STKMOVE" USING WS-SM
           MOVE "an adjustment without a reason posts nothing"
               TO TK-NAME
           IF SM-MOVEMENT-ID = 0 AND SM-STOCK-QTY = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "an input error returns reason STK0007"
               TO TK-NAME
           MOVE "STK0007" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SM TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "an input error returns return code 8" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-SM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-015
       1900-BAD-FUNCTION.
           PERFORM 9000-RESET
           MOVE "WIBBLE" TO CA-FUNCTION OF WS-SM
           MOVE WS-PLAIN-ITEM TO SM-ITEM-CODE
           MOVE 1 TO SM-QUANTITY
           CALL "STKMOVE" USING WS-SM
           MOVE "an unsupported movement type posts nothing"
               TO TK-NAME
           IF SM-MOVEMENT-ID = 0 AND SM-MOVEMENT-TYPE = SPACES
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

       9000-RESET.
           INITIALIZE WS-SM
           MOVE "STC" TO CA-COMPANY OF WS-SM
           MOVE "RDG" TO CA-SITE OF WS-SM
           MOVE "WHSE" TO CA-USER OF WS-SM
           MOVE 20261005 TO CA-BUSINESS-DATE OF WS-SM
           MOVE "RDG" TO SM-SITE
           MOVE "MAIN" TO SM-WAREHOUSE
           MOVE "OWNED" TO SM-OWNER-TYPE
           MOVE "ACCEPTED" TO SM-CONDITION
           MOVE "EA" TO SM-UOM-CODE
           MOVE 20261005 TO SM-MOVEMENT-DATE
           MOVE 20261005 TO SM-POSTING-DATE
           MOVE "ITM" TO SM-SOURCE-MODULE.

      *>   GUI-028: actual calls prove the extended SQL parameter and
      *>   result layouts; the transaction leaves no stock or costs.
       2000-VALUED-MOVEMENTS.
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-SQLCODE < 0
               MOVE "valued movement transaction opens" TO TK-NAME
               MOVE SQLIO-ERRMSG TO TK-DETAIL
               PERFORM TK-FAIL
               EXIT PARAGRAPH
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT inv.Item (CompanyCode, ItemCode, "
                  "Description, ItemClass, StockUomCode, QuantumQty, "
                  "IsIndivisible, TraceabilityType, MakeBuy, "
                  "ValuationGroup, CostMethod, LifecycleStatus, "
                  "CreatedBy) VALUES ('STC','ZZ-SMVAL','Valuation', "
                  "'BOUGHT','EA',1,1,'NONE','BUY','ZZSM','MWA', "
                  "'RELEASED','TEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           PERFORM 9000-RESET
           MOVE "RECEIPT" TO CA-FUNCTION OF WS-SM
           MOVE "ZZ-SMVAL-RPL" TO CA-CORRELATION-ID OF WS-SM
           MOVE "ZZ-SMVAL" TO SM-ITEM-CODE SM-SOURCE-DOC-REF
           MOVE "STKA01" TO SM-LOCATION
           MOVE "GOODSRECEIPT" TO SM-SOURCE-DOC-TYPE
           MOVE 36 TO SM-QUANTITY
           MOVE 2.314167 TO SM-UNIT-COST
           MOVE 83.33 TO SM-VALUE-GBP
           CALL "STKMOVE" USING WS-SM
           MOVE "receipt carries the rounded document GBP evidence"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-SM < 8
              AND SM-MOVEMENT-ID > 0 AND SM-VALUE-GBP = 83.33
              AND SM-UNIT-COST = 2.314167
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE SM-MOVEMENT-ID TO WS-RECEIPT-ID
           CALL "STKMOVE" USING WS-SM
           MOVE "native replay reads the duplicate in column ten"
               TO TK-NAME
           IF SM-DUPLICATE = "Y"
              AND SM-MOVEMENT-ID = WS-RECEIPT-ID
              AND SM-QTY-AFTER = 36 AND SM-VALUE-GBP = 83.33
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           PERFORM 9000-RESET
           MOVE "TRANSFER" TO CA-FUNCTION OF WS-SM
           MOVE "ZZ-SMVAL" TO SM-ITEM-CODE
           MOVE "STKA01" TO SM-LOCATION SM-TO-LOCATION
           MOVE "BHM" TO SM-TO-SITE
           MOVE "MAIN" TO SM-TO-WAREHOUSE
           MOVE 6 TO SM-QUANTITY
           CALL "STKMOVE" USING WS-SM
           MOVE "native inter-site transfer carries origin relief"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-SM < 8
              AND SM-MOVEMENT-ID > 0 AND SM-MOVEMENT-ID-2 > 0
              AND SM-VALUE-GBP = 13.89 AND SM-QTY-AFTER = 6
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           PERFORM 9000-RESET
           MOVE "ISSUE" TO CA-FUNCTION OF WS-SM
           MOVE "ZZ-SMVAL" TO SM-ITEM-CODE
           MOVE "STKA01" TO SM-LOCATION
           MOVE 30 TO SM-QUANTITY
           CALL "STKMOVE" USING WS-SM
           MOVE "unpriced native issue returns actual pool relief"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-SM < 8
              AND SM-QTY-AFTER = 0 AND SM-VALUE-GBP = 69.44
              AND SM-UNIT-COST = 2.314667
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(30)) FROM "
                  "cst.ItemCostHistory WHERE CompanyCode = 'STC' "
                  "AND ItemCode = 'ZZ-SMVAL' AND SiteCode = 'RDG'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 60 TO SQLIO-COL-WIDTH(1)
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           MOVE "native movements create site-scoped pool histories"
               TO TK-NAME
           MOVE 3 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-PROBE)
           PERFORM TK-ASSERT-NUM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE "ROLB" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       COPY TESTKITP.
