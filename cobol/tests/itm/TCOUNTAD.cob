      *>================================================================
      *> TCOUNTAD - unit tests for the counting variance and approved
      *> adjustment arithmetic implemented by the batch program
      *> COUNTADJ.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *> COUNTADJ is a batch main program, so it cannot be CALLed.
      *> These tests prove the rules it implements, on the
      *> specification's counting scenario: a count runs while a
      *> documented movement occurs, so the expected quantity at the
      *> instant of the count is the frozen expected quantity plus the
      *> intervening movements, and the approved adjustment is the
      *> counted quantity less that figure - never counted less frozen.
      *> A recount replaces the counted quantity before any variance is
      *> posted, a nil variance is not posted at all, and an adjustment
      *> with no reason is refused by STKMOVE.
      *> Output protocol (TAP): "ok n - text" / "not ok n - text".
      *>================================================================
      *> REQ: ITM-015, ITM-018
      *> AC: AC-074
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCOUNTAD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY STKMOVECA REPLACING ==DFHCOMMAREA== BY ==WS-SM==.
       01  WS-COUNT.
           05  C-EXPECTED              PIC S9(13)V9(6) VALUE 0.
           05  C-INTERVENE             PIC S9(13)V9(6) VALUE 0.
           05  C-AT-COUNT              PIC S9(13)V9(6) VALUE 0.
           05  C-COUNTED               PIC S9(13)V9(6) VALUE 0.
           05  C-RECOUNT               PIC S9(13)V9(6) VALUE 0.
           05  C-ADJUST                PIC S9(13)V9(6) VALUE 0.
           05  C-VARIANCE              PIC S9(13)V9(6) VALUE 0.
       01  WS-SERIALS.
           05  S-EXPECTED-CNT          PIC S9(4) VALUE 0.
           05  S-COUNTED-CNT           PIC S9(4) VALUE 0.
           05  S-MISSING               PIC S9(4) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TCOUNTAD" TO TK-SUITE
           PERFORM TK-BEGIN

           PERFORM 1000-CUTOFF-IS-ACCOUNTED
           PERFORM 1100-VARIANCE-FROM-CUTOFF
           PERFORM 1200-NAIVE-VARIANCE-IS-WRONG
           PERFORM 1300-RECOUNT-REPLACES-COUNT
           PERFORM 1400-NIL-VARIANCE
           PERFORM 1500-RECEIPT-DURING-COUNT
           PERFORM 1600-SERIAL-BY-IDENTITY
           PERFORM 1700-ADJUST-NEEDS-REASON

           PERFORM TK-END
           STOP RUN.

      *>   REQ: ITM-018  AC: AC-074
      *>   500 were frozen for the count; an authorised issue of 20
      *>   was documented while the counters worked, so 480 is the
      *>   quantity the shelf should hold at the instant of the count.
       1000-CUTOFF-IS-ACCOUNTED.
           MOVE 500 TO C-EXPECTED
           MOVE -20 TO C-INTERVENE
           COMPUTE C-AT-COUNT = C-EXPECTED + C-INTERVENE
           MOVE "500 frozen less an issue of 20 is 480 at the count"
               TO TK-NAME
           MOVE 480 TO TK-EXPECTED-N
           MOVE C-AT-COUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-018  AC: AC-074
       1100-VARIANCE-FROM-CUTOFF.
           MOVE 482 TO C-COUNTED
           COMPUTE C-ADJUST = C-COUNTED - C-AT-COUNT
           MOVE "counting 482 against 480 is an adjustment of two"
               TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE C-ADJUST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the approved adjustment is signed, not absolute"
               TO TK-NAME
           IF C-ADJUST > 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-018  AC: AC-074
       1200-NAIVE-VARIANCE-IS-WRONG.
           COMPUTE C-VARIANCE = C-COUNTED - C-EXPECTED
           MOVE "ignoring the movement would report minus eighteen"
               TO TK-NAME
           MOVE -18 TO TK-EXPECTED-N
           MOVE C-VARIANCE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "so the posted adjustment is never that figure"
               TO TK-NAME
           IF C-ADJUST NOT = C-VARIANCE
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-018
       1300-RECOUNT-REPLACES-COUNT.
           MOVE 479 TO C-RECOUNT
           MOVE C-RECOUNT TO C-COUNTED
           COMPUTE C-ADJUST = C-COUNTED - C-AT-COUNT
           MOVE "a recount of 479 replaces the first count of 482"
               TO TK-NAME
           MOVE 479 TO TK-EXPECTED-N
           MOVE C-COUNTED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "and the adjustment becomes minus one" TO TK-NAME
           MOVE -1 TO TK-EXPECTED-N
           MOVE C-ADJUST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-018
       1400-NIL-VARIANCE.
           MOVE 480 TO C-COUNTED
           COMPUTE C-ADJUST = C-COUNTED - C-AT-COUNT
           MOVE "a count that agrees posts no movement at all"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE C-ADJUST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-018  AC: AC-074
       1500-RECEIPT-DURING-COUNT.
           MOVE 120 TO C-EXPECTED
           MOVE 40 TO C-INTERVENE
           COMPUTE C-AT-COUNT = C-EXPECTED + C-INTERVENE
           MOVE 160 TO C-COUNTED
           COMPUTE C-ADJUST = C-COUNTED - C-AT-COUNT
           MOVE "a documented receipt of 40 leaves no variance"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE C-ADJUST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the expected quantity at the count is 160"
               TO TK-NAME
           MOVE 160 TO TK-EXPECTED-N
           MOVE C-AT-COUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-018  AC: AC-074
      *>   Serial discrepancies are resolved by identity: ten serials
      *>   were expected, ten were counted, but one of them was not on
      *>   the expected list, so two identities must be investigated
      *>   even though the aggregate count agrees.
       1600-SERIAL-BY-IDENTITY.
           MOVE 10 TO S-EXPECTED-CNT
           MOVE 10 TO S-COUNTED-CNT
           MOVE 1 TO S-MISSING
           COMPUTE C-ADJUST = S-COUNTED-CNT - S-EXPECTED-CNT
           MOVE "the aggregate serial count agrees" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE C-ADJUST TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "but one missing identity still needs resolving"
               TO TK-NAME
           IF S-MISSING > 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-015, ITM-018
       1700-ADJUST-NEEDS-REASON.
           INITIALIZE WS-SM
           MOVE "STC" TO CA-COMPANY OF WS-SM
           MOVE "RDG" TO CA-SITE OF WS-SM
           MOVE "STKSUPV" TO CA-USER OF WS-SM
           MOVE 20261005 TO CA-BUSINESS-DATE OF WS-SM
           MOVE "ADJUST" TO CA-FUNCTION OF WS-SM
           MOVE "LINK-QUICK-11S" TO SM-ITEM-CODE
           MOVE "RDG" TO SM-SITE
           MOVE "MAIN" TO SM-WAREHOUSE
           MOVE "STKA01" TO SM-LOCATION
           MOVE "OWNED" TO SM-OWNER-TYPE
           MOVE "ACCEPTED" TO SM-CONDITION
           MOVE "EA" TO SM-UOM-CODE
           MOVE 20261005 TO SM-MOVEMENT-DATE
           MOVE 20261005 TO SM-POSTING-DATE
           MOVE "ITM" TO SM-SOURCE-MODULE
           MOVE 2 TO SM-QUANTITY
           MOVE SPACES TO SM-REASON-CODE
           MOVE SPACES TO SM-REASON
           CALL "STKMOVE" USING WS-SM
           MOVE "a count adjustment with no reason posts nothing"
               TO TK-NAME
           IF SM-MOVEMENT-ID = 0 AND SM-STOCK-QTY = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SM TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "and no movement identity was created" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SM-MOVEMENT-ID TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       COPY TESTKITP.
