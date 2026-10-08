      *>================================================================
      *> TUOMCONV - unit tests for UOMCONV (unit of measure service).
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : ITEM AND PART MAINTENANCE
      *> The expected values are the specification's worked numbers:
      *>   four sleeves of 25 pieces are 100 pieces; after the supplier
      *>   changes the sleeve to 20 the earlier receipt is still 100
      *>   and new orders use 20; a price of 75.00 per sleeve of 25 is
      *>   3.00 per piece; a fractional serialised quantity and a zero
      *>   or incompatible conversion fail with no stock effect.
      *> Output protocol (TAP): "ok n - text" / "not ok n - text".
      *>================================================================
      *> REQ: ITM-005, ITM-006, ITM-007
      *> AC: AC-070
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TUOMCONV.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY UOMCONVCA REPLACING ==DFHCOMMAREA== BY ==WS-UC==.
       01  WS-SERIAL-ITEM              PIC X(40)
                                       VALUE "MOTOR-HUB-250W".
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TUOMCONV" TO TK-SUITE
           PERFORM TK-BEGIN

           PERFORM 1000-SLEEVE-OF-25
           PERFORM 1100-SLEEVE-OF-20
           PERFORM 1200-UNIT-COST
           PERFORM 1300-BACK-TO-SLEEVES
           PERFORM 1400-FRACTIONAL-SERIAL
           PERFORM 1500-INCOMPATIBLE-UNITS
           PERFORM 1600-PACK-UNIT-GUARD
           PERFORM 1700-DIMENSIONAL
           PERFORM 1800-UNKNOWN-ITEM
           PERFORM 1900-LIST-CONVERSIONS

           PERFORM TK-END
           STOP RUN.

      *>   REQ: ITM-007  AC: AC-070
       1000-SLEEVE-OF-25.
           PERFORM 9000-RESET
           MOVE "TOSTOCK" TO CA-FUNCTION OF WS-UC
           MOVE "LINK-QUICK-11S" TO UC-ITEM-CODE
           MOVE "SLV" TO UC-FROM-UOM
           MOVE "PURCHASE" TO UC-USAGE-TYPE
           MOVE 20260915 TO UC-EFF-DATE
           MOVE 4 TO UC-ENTERED-QTY
           CALL "UOMCONV" USING WS-UC
           MOVE "four sleeves of 25 pieces are 100 pieces" TO TK-NAME
           MOVE 100 TO TK-EXPECTED-N
           MOVE UC-STOCK-QTY TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-UC TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the factor used is 25 stock units per sleeve"
               TO TK-NAME
           MOVE 25 TO TK-EXPECTED-N
           MOVE UC-FACTOR TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the factor comes from the item, not a guess"
               TO TK-NAME
           MOVE "ITEMUOM" TO TK-EXPECTED-X
           MOVE UC-SOURCE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-005  AC: AC-070
       1100-SLEEVE-OF-20.
           PERFORM 9000-RESET
           MOVE "TOSTOCK" TO CA-FUNCTION OF WS-UC
           MOVE "LINK-QUICK-11S" TO UC-ITEM-CODE
           MOVE "SLV" TO UC-FROM-UOM
           MOVE "PURCHASE" TO UC-USAGE-TYPE
           MOVE 20261002 TO UC-EFF-DATE
           MOVE 4 TO UC-ENTERED-QTY
           CALL "UOMCONV" USING WS-UC
           MOVE "after the pack change four sleeves are 80 pieces"
               TO TK-NAME
           MOVE 80 TO TK-EXPECTED-N
           MOVE UC-STOCK-QTY TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-UC TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the new effective factor is 20" TO TK-NAME
           MOVE 20 TO TK-EXPECTED-N
           MOVE UC-FACTOR TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-007  AC: AC-070
       1200-UNIT-COST.
           PERFORM 9000-RESET
           MOVE "UNITCOST" TO CA-FUNCTION OF WS-UC
           MOVE "LINK-QUICK-11S" TO UC-ITEM-CODE
           MOVE "SLV" TO UC-PRICE-UOM
           MOVE 20260915 TO UC-EFF-DATE
           MOVE 75.00 TO UC-FOREIGN-PRICE
           MOVE 1 TO UC-PRICE-PER-QTY
           MOVE 1 TO UC-FX-PER-GBP
           CALL "UOMCONV" USING WS-UC
           MOVE "75.00 per sleeve of 25 is 3.00 per piece"
               TO TK-NAME
           MOVE 3.00 TO TK-EXPECTED-N
           MOVE UC-UNIT-COST-GBP TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-UC TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-005
       1300-BACK-TO-SLEEVES.
           PERFORM 9000-RESET
           MOVE "FROMSTK" TO CA-FUNCTION OF WS-UC
           MOVE "LINK-QUICK-11S" TO UC-ITEM-CODE
           MOVE "SLV" TO UC-TO-UOM
           MOVE "PURCHASE" TO UC-USAGE-TYPE
           MOVE 20260915 TO UC-EFF-DATE
           MOVE 100 TO UC-STOCK-QTY
           CALL "UOMCONV" USING WS-UC
           MOVE "100 pieces express as four sleeves" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE UC-RESULT-QTY TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-UC TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-006  AC: AC-070
       1400-FRACTIONAL-SERIAL.
           PERFORM 9000-RESET
           MOVE "CHECKQTY" TO CA-FUNCTION OF WS-UC
           MOVE WS-SERIAL-ITEM TO UC-ITEM-CODE
           MOVE 0.5 TO UC-ENTERED-QTY
           CALL "UOMCONV" USING WS-UC
           MOVE "a serialised item cannot move as half a unit"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-UC = 8
              AND CA-REASON-CODE OF WS-UC = "UOM0006"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-UC TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "the refusal offers the next whole unit" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE UC-ROUNDED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 9000-RESET
           MOVE "CHECKQTY" TO CA-FUNCTION OF WS-UC
           MOVE WS-SERIAL-ITEM TO UC-ITEM-CODE
           MOVE 3 TO UC-ENTERED-QTY
           CALL "UOMCONV" USING WS-UC
           MOVE "three whole serialised units are accepted"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-UC = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-UC TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-005  AC: AC-070
       1500-INCOMPATIBLE-UNITS.
           PERFORM 9000-RESET
           MOVE "CONVERT" TO CA-FUNCTION OF WS-UC
           MOVE "KG" TO UC-FROM-UOM
           MOVE "L" TO UC-TO-UOM
           MOVE 10 TO UC-ENTERED-QTY
           CALL "UOMCONV" USING WS-UC
           MOVE "mass cannot be converted to volume" TO TK-NAME
           IF CA-RETURN-CODE OF WS-UC = 8
              AND CA-REASON-CODE OF WS-UC = "UOM0005"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-UC TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "no quantity is produced by a refused conversion"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE UC-RESULT-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-005  AC: AC-070
       1600-PACK-UNIT-GUARD.
           PERFORM 9000-RESET
           MOVE "TOSTOCK" TO CA-FUNCTION OF WS-UC
           MOVE "LINK-QUICK-11S" TO UC-ITEM-CODE
           MOVE "BOX" TO UC-FROM-UOM
           MOVE "PURCHASE" TO UC-USAGE-TYPE
           MOVE 20260915 TO UC-EFF-DATE
           MOVE 1 TO UC-ENTERED-QTY
           CALL "UOMCONV" USING WS-UC
           MOVE "an unapproved pack unit is not assumed to hold one"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-UC = 8
              AND CA-REASON-CODE OF WS-UC = "UOM0003"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-UC TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-005
       1700-DIMENSIONAL.
           PERFORM 9000-RESET
           MOVE "CONVERT" TO CA-FUNCTION OF WS-UC
           MOVE "FT" TO UC-FROM-UOM
           MOVE "M" TO UC-TO-UOM
           MOVE 1 TO UC-ENTERED-QTY
           CALL "UOMCONV" USING WS-UC
           MOVE "one foot is 0.304800 metres" TO TK-NAME
           MOVE 0.304800 TO TK-EXPECTED-N
           MOVE UC-RESULT-QTY TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-UC TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-001
       1800-UNKNOWN-ITEM.
           PERFORM 9000-RESET
           MOVE "TOSTOCK" TO CA-FUNCTION OF WS-UC
           MOVE "NO-SUCH-ITEM-AT-ALL" TO UC-ITEM-CODE
           MOVE "EA" TO UC-FROM-UOM
           MOVE 1 TO UC-ENTERED-QTY
           CALL "UOMCONV" USING WS-UC
           MOVE "an unknown item is reported, never defaulted"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-UC = 16
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-UC TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-005
       1900-LIST-CONVERSIONS.
           PERFORM 9000-RESET
           MOVE "ITEMUOM" TO CA-FUNCTION OF WS-UC
           MOVE "LINK-QUICK-11S" TO UC-ITEM-CODE
           CALL "UOMCONV" USING WS-UC
           MOVE "the conversion history is kept, not overwritten"
               TO TK-NAME
           IF UC-LINE-CNT >= 4
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-UC TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

       9000-RESET.
           INITIALIZE WS-UC
           MOVE "STC" TO CA-COMPANY OF WS-UC
           MOVE "RDG" TO CA-SITE OF WS-UC
           MOVE "WHSE" TO CA-USER OF WS-UC
           MOVE 20261005 TO CA-BUSINESS-DATE OF WS-UC.

       COPY TESTKITP.
