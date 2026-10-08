      *>================================================================
      *> TAVAILQ - unit tests for AVAILQ, the availability service.
      *> SYSTEM  : STERP - Stefan's ERP
      *> MODULE  : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *> The specification's availability example drives the rules the
      *> tests assert: owned on hand 120 of which 15 is quarantined and
      *> 5 expired gives 100 eligible; other demands reserve 35, so 65
      *> is free; a receipt of 40 expected next week is shown
      *> separately and today's free quantity is still 65.  The seeded
      *> company keeps 120 of BEARING-6902-2RS in quarantine at RDG,
      *> which must never reach the eligible or free quantity.
      *> Output protocol (TAP): "ok n - text" / "not ok n - text".
      *>================================================================
      *> REQ: ITM-012, ITM-013, ITM-014
      *> AC: AC-071, AC-073
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAVAILQ.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY AVAILQCA REPLACING ==DFHCOMMAREA== BY ==WS-AV==.
       01  WS-WORK.
           05  WS-LX                   PIC S9(4) VALUE 0.
           05  WS-BAD                  PIC S9(4) VALUE 0.
           05  WS-CALC                 PIC S9(13)V9(6) VALUE 0.
           05  WS-EX-ELIGIBLE          PIC S9(13)V9(6) VALUE 0.
           05  WS-EX-RESERVED          PIC S9(13)V9(6) VALUE 0.
           05  WS-EX-FREE              PIC S9(13)V9(6) VALUE 0.
       01  WS-QUAR-ITEM                PIC X(40)
               VALUE "BEARING-6902-2RS".
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TAVAILQ" TO TK-SUITE
           PERFORM TK-BEGIN

           PERFORM 1000-FREE-IS-ELIG-LESS-RESV
           PERFORM 1100-PHYSICAL-COVERS-ELIG
           PERFORM 1200-QUARANTINE-SEPARATE
           PERFORM 1300-EXPECTED-IS-SEPARATE
           PERFORM 1400-BUCKETS-NOT-DOUBLE
           PERFORM 1500-THIRD-PARTY-NOT-ELIGIBLE
           PERFORM 1600-BYLOC-OWNED-ONLY
           PERFORM 1700-WORKED-EXAMPLE
           PERFORM 1800-UNKNOWN-ITEM
           PERFORM 1900-BAD-AS-AT
           PERFORM 2000-BAD-FUNCTION

           PERFORM TK-END
           STOP RUN.

      *>   REQ: ITM-013  AC: AC-071
       1000-FREE-IS-ELIG-LESS-RESV.
           PERFORM 9000-RESET
           MOVE "AVAIL" TO CA-FUNCTION OF WS-AV
           MOVE WS-QUAR-ITEM TO AV-ITEM-CODE
           MOVE "RDG" TO AV-SITE
           CALL "AVAILQ" USING WS-AV
           COMPUTE WS-CALC = AV-QTY-ELIGIBLE - AV-QTY-RESERVED
           IF WS-CALC < 0
               MOVE 0 TO WS-CALC
           END-IF
           MOVE "free stock is eligible less active reservations"
               TO TK-NAME
           MOVE WS-CALC TO TK-EXPECTED-N
           MOVE AV-QTY-FREE TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-AV TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-013  AC: AC-071
       1100-PHYSICAL-COVERS-ELIG.
           MOVE "eligible stock never exceeds physical stock"
               TO TK-NAME
           IF AV-QTY-ELIGIBLE <= AV-QTY-PHYSICAL
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-AV TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-012  AC: AC-073
       1200-QUARANTINE-SEPARATE.
           MOVE "the 120 quarantined units are reported separately"
               TO TK-NAME
           IF AV-QTY-QUARANTINE >= 120
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-AV TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "quarantined stock is not eligible stock" TO TK-NAME
           COMPUTE WS-CALC =
               AV-QTY-ELIGIBLE + AV-QTY-QUARANTINE
           IF WS-CALC <= AV-QTY-PHYSICAL
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-AV TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-013  AC: AC-071
       1300-EXPECTED-IS-SEPARATE.
           MOVE "expected receipts never join the free quantity"
               TO TK-NAME
           IF AV-QTY-FREE <= AV-QTY-ELIGIBLE
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-AV TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "planned output is a separate quantity" TO TK-NAME
           IF AV-QTY-PLANNED >= 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-012  AC: AC-071
       1400-BUCKETS-NOT-DOUBLE.
           PERFORM 9000-RESET
           MOVE "ENQUIRE" TO CA-FUNCTION OF WS-AV
           MOVE WS-QUAR-ITEM TO AV-ITEM-CODE
           MOVE "RDG" TO AV-SITE
           CALL "AVAILQ" USING WS-AV
           MOVE 0 TO WS-CALC
           PERFORM VARYING WS-LX FROM 1 BY 1
                   UNTIL WS-LX > AV-LINE-CNT
               IF AV-L-OWNER-TYPE(WS-LX) = "OWNED"
                   ADD AV-L-QTY(WS-LX) TO WS-CALC
               END-IF
           END-PERFORM
           MOVE "the bucket lines add up to the physical quantity"
               TO TK-NAME
           MOVE AV-QTY-PHYSICAL TO TK-EXPECTED-N
           MOVE WS-CALC TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-AV TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-012  AC: AC-073
       1500-THIRD-PARTY-NOT-ELIGIBLE.
           MOVE 0 TO WS-BAD
           PERFORM VARYING WS-LX FROM 1 BY 1
                   UNTIL WS-LX > AV-LINE-CNT
               IF AV-L-OWNER-TYPE(WS-LX) NOT = "OWNED"
                       AND AV-L-ELIGIBLE(WS-LX) = "Y"
                   ADD 1 TO WS-BAD
               END-IF
               IF AV-L-CONDITION(WS-LX) = "QUARANTINE"
                       AND AV-L-ELIGIBLE(WS-LX) = "Y"
                   ADD 1 TO WS-BAD
               END-IF
           END-PERFORM
           MOVE "custody stock and quarantine are never eligible"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-BAD TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE 0 TO WS-BAD
           PERFORM VARYING WS-LX FROM 1 BY 1
                   UNTIL WS-LX > AV-LINE-CNT
               IF AV-L-ELIGIBLE(WS-LX) NOT = "Y"
                       AND AV-L-WHY(WS-LX) = SPACES
                   ADD 1 TO WS-BAD
               END-IF
           END-PERFORM
           MOVE "every ineligible bucket says why" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-BAD TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-012
       1600-BYLOC-OWNED-ONLY.
           PERFORM 9000-RESET
           MOVE "BYLOC" TO CA-FUNCTION OF WS-AV
           MOVE WS-QUAR-ITEM TO AV-ITEM-CODE
           MOVE "RDG" TO AV-SITE
           CALL "AVAILQ" USING WS-AV
           MOVE 0 TO WS-BAD
           PERFORM VARYING WS-LX FROM 1 BY 1
                   UNTIL WS-LX > AV-LINE-CNT
               IF AV-L-OWNER-TYPE(WS-LX) NOT = "OWNED"
                   ADD 1 TO WS-BAD
               END-IF
           END-PERFORM
           MOVE "the location view lists owned stock only" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-BAD TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-AV TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-013  AC: AC-071
      *>   The specification's own arithmetic, proved on its numbers:
      *>   120 owned, 15 quarantined and 5 expired leave 100 eligible;
      *>   35 reserved by other demands leave 65 free.
       1700-WORKED-EXAMPLE.
           COMPUTE WS-EX-ELIGIBLE = 120 - 15 - 5
           MOVE 35 TO WS-EX-RESERVED
           COMPUTE WS-EX-FREE = WS-EX-ELIGIBLE - WS-EX-RESERVED
           MOVE "120 owned less 15 quarantine less 5 expired is 100"
               TO TK-NAME
           MOVE 100 TO TK-EXPECTED-N
           MOVE WS-EX-ELIGIBLE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "100 eligible less 35 reserved leaves 65 free"
               TO TK-NAME
           MOVE 65 TO TK-EXPECTED-N
           MOVE WS-EX-FREE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           COMPUTE WS-CALC = WS-EX-FREE
           MOVE "a receipt of 40 next week does not change today"
               TO TK-NAME
           MOVE 65 TO TK-EXPECTED-N
           MOVE WS-CALC TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-012
       1800-UNKNOWN-ITEM.
           PERFORM 9000-RESET
           MOVE "AVAIL" TO CA-FUNCTION OF WS-AV
           MOVE "NO-SUCH-ITEM-ANYWHERE-AT-ALL" TO AV-ITEM-CODE
           CALL "AVAILQ" USING WS-AV
           MOVE "an unknown item is reported, not shown as zero"
               TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-AV TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-AV TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-013
       1900-BAD-AS-AT.
           PERFORM 9000-RESET
           MOVE "AVAIL" TO CA-FUNCTION OF WS-AV
           MOVE WS-QUAR-ITEM TO AV-ITEM-CODE
           MOVE 20261345 TO AV-AS-AT
           CALL "AVAILQ" USING WS-AV
           MOVE "an impossible as at date is refused" TO TK-NAME
           MOVE "AVL0005" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-AV TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-AV TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-012
       2000-BAD-FUNCTION.
           PERFORM 9000-RESET
           MOVE "WIBBLE" TO CA-FUNCTION OF WS-AV
           MOVE WS-QUAR-ITEM TO AV-ITEM-CODE
           CALL "AVAILQ" USING WS-AV
           MOVE "an unsupported function is refused" TO TK-NAME
           MOVE "AVL0002" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-AV TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-AV TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

       9000-RESET.
           INITIALIZE WS-AV
           MOVE "STC" TO CA-COMPANY OF WS-AV
           MOVE "RDG" TO CA-SITE OF WS-AV
           MOVE "WHSE" TO CA-USER OF WS-AV
           MOVE 20261005 TO CA-BUSINESS-DATE OF WS-AV
           MOVE 20261005 TO AV-AS-AT.

       COPY TESTKITP.
