      *>================================================================
      *> TITEMVAL - unit tests for ITEMVAL, the item identity, class
      *> and activation service.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : ITEM AND PART MAINTENANCE
      *> The expected values are the specification's worked cases:
      *>   two long codes that differ only near their end stay two
      *>   different items, a supplier catalogue number that matches
      *>   two internal items is never silently resolved, a draft item
      *>   with missing process data cannot be released, and a released
      *>   stocked item is allowed to hold physical stock.
      *> Output protocol (TAP): "ok n - text" / "not ok n - text".
      *>================================================================
      *> REQ: ITM-001, ITM-002, ITM-003, ITM-004
      *> AC: AC-068, AC-069
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TITEMVAL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY ITEMVALCA REPLACING ==DFHCOMMAREA== BY ==WS-IV==.
       01  WS-CODE-A                   PIC X(40)
               VALUE "BRKT-MECH-DISC-FLATMOUNT-160-A".
       01  WS-CODE-B                   PIC X(40)
               VALUE "BRKT-MECH-DISC-FLATMOUNT-160-B".
       01  WS-DESC-A                   PIC X(100) VALUE SPACES.
       01  WS-DESC-B                   PIC X(100) VALUE SPACES.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TITEMVAL" TO TK-SUITE
           PERFORM TK-BEGIN

           PERFORM 1000-LONG-CODE-A
           PERFORM 1100-LONG-CODE-B
           PERFORM 1200-CODES-DISTINCT
           PERFORM 1300-UNKNOWN-ITEM
           PERFORM 1400-CLASS-MAY-STOCK
           PERFORM 1500-DRAFT-NOT-READY
           PERFORM 1600-DRAFT-NOT-RELEASED
           PERFORM 1700-AMBIGUOUS-ALIAS
           PERFORM 1800-REVISION
           PERFORM 1900-BAD-FUNCTION
           PERFORM 2000-SEARCH-NO-TEXT

           PERFORM TK-END
           STOP RUN.

      *>   REQ: ITM-001  AC: AC-068
       1000-LONG-CODE-A.
           PERFORM 9000-RESET
           MOVE "FETCH" TO CA-FUNCTION OF WS-IV
           MOVE WS-CODE-A TO IV-ITEM-CODE
           CALL "ITEMVAL" USING WS-IV
           MOVE IV-DESCRIPTION TO WS-DESC-A
           MOVE "the full 30 character code is read, not truncated"
               TO TK-NAME
           MOVE WS-CODE-A TO TK-EXPECTED-X
           MOVE IV-ITEM-CODE TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-IV TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           MOVE "the item is known in the company" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-IV TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-IV TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-001  AC: AC-068
       1100-LONG-CODE-B.
           PERFORM 9000-RESET
           MOVE "FETCH" TO CA-FUNCTION OF WS-IV
           MOVE WS-CODE-B TO IV-ITEM-CODE
           CALL "ITEMVAL" USING WS-IV
           MOVE IV-DESCRIPTION TO WS-DESC-B
           MOVE "the sister code reads back as itself" TO TK-NAME
           MOVE WS-CODE-B TO TK-EXPECTED-X
           MOVE IV-ITEM-CODE TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-IV TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-001  AC: AC-068
       1200-CODES-DISTINCT.
           MOVE "two codes differing near their end are two items"
               TO TK-NAME
           IF WS-DESC-A NOT = WS-DESC-B
               PERFORM TK-PASS
           ELSE
               MOVE WS-DESC-A TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-001
       1300-UNKNOWN-ITEM.
           PERFORM 9000-RESET
           MOVE "FETCH" TO CA-FUNCTION OF WS-IV
           MOVE "NO-SUCH-ITEM-ANYWHERE-AT-ALL" TO IV-ITEM-CODE
           CALL "ITEMVAL" USING WS-IV
           MOVE "an unknown item is reported, never invented"
               TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-IV TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-IV TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-002  AC: AC-068
       1400-CLASS-MAY-STOCK.
           PERFORM 9000-RESET
           MOVE "CLASSCHK" TO CA-FUNCTION OF WS-IV
           MOVE "LINK-QUICK-11S" TO IV-ITEM-CODE
           MOVE "STOCK" TO IV-ACTION
           CALL "ITEMVAL" USING WS-IV
           MOVE "a released stocked item may hold physical stock"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE IV-ALLOWED TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-IV TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-003  AC: AC-068
       1500-DRAFT-NOT-READY.
           PERFORM 9000-RESET
           MOVE "VALIDATE" TO CA-FUNCTION OF WS-IV
           MOVE "ITM-DRAFT-TESTRIG" TO IV-ITEM-CODE
           CALL "ITEMVAL" USING WS-IV
           MOVE "an incomplete draft is not ready for release"
               TO TK-NAME
           IF IV-READY NOT = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-IV TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "the review lists what is missing, by requirement"
               TO TK-NAME
           IF IV-FAULT-CNT > 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-IV TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-003  AC: AC-068
       1600-DRAFT-NOT-RELEASED.
           PERFORM 9000-RESET
           MOVE "ACTIVATE" TO CA-FUNCTION OF WS-IV
           MOVE "ITM-DRAFT-TESTRIG" TO IV-ITEM-CODE
           MOVE "ENGMGR" TO CA-USER OF WS-IV
           CALL "ITEMVAL" USING WS-IV
           MOVE "a draft missing information cannot be activated"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-IV NOT = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-IV TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "the refusal carries a stable reason code" TO TK-NAME
           IF CA-REASON-CODE OF WS-IV NOT = SPACES
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-004  AC: AC-069
       1700-AMBIGUOUS-ALIAS.
           PERFORM 9000-RESET
           MOVE "RESOLVE" TO CA-FUNCTION OF WS-IV
           MOVE "FM-160-A" TO IV-SEARCH-TEXT
           MOVE "SUPPLIER" TO IV-PARTNER-TYPE
           CALL "ITEMVAL" USING WS-IV
           MOVE "one supplier alias on two items is not resolved"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-IV NOT = 0
                   OR IV-MATCH-CNT > 1
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-IV TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-001
       1800-REVISION.
           PERFORM 9000-RESET
           MOVE "REVISION" TO CA-FUNCTION OF WS-IV
           MOVE WS-CODE-A TO IV-ITEM-CODE
           MOVE "A" TO IV-REVISION
           CALL "ITEMVAL" USING WS-IV
           MOVE "the revision carries its own effectivity" TO TK-NAME
           IF CA-RETURN-CODE OF WS-IV = 0
                   AND IV-REV-FROM > 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-IV TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-002
       1900-BAD-FUNCTION.
           PERFORM 9000-RESET
           MOVE "WIBBLE" TO CA-FUNCTION OF WS-IV
           MOVE WS-CODE-A TO IV-ITEM-CODE
           CALL "ITEMVAL" USING WS-IV
           MOVE "an unsupported function is refused, not guessed"
               TO TK-NAME
           MOVE "ITM0008" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-IV TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-IV TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-001  AC: AC-069
       2000-SEARCH-NO-TEXT.
           PERFORM 9000-RESET
           MOVE "SEARCH" TO CA-FUNCTION OF WS-IV
           MOVE SPACES TO IV-SEARCH-TEXT
           CALL "ITEMVAL" USING WS-IV
           MOVE "an empty search is refused, not a whole file scan"
               TO TK-NAME
           MOVE "ITM0009" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-IV TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-IV TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

       9000-RESET.
           INITIALIZE WS-IV
           MOVE "STC" TO CA-COMPANY OF WS-IV
           MOVE "RDG" TO CA-SITE OF WS-IV
           MOVE "ENGINEER" TO CA-USER OF WS-IV
           MOVE 20261005 TO CA-BUSINESS-DATE OF WS-IV.

       COPY TESTKITP.
