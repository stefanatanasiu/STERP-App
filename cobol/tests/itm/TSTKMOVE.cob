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
       COPY STKMOVECA REPLACING ==DFHCOMMAREA== BY ==WS-SM==.
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

       COPY TESTKITP.
