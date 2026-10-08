      *>================================================================
      *> TSUBSTCK - unit tests for SUBSTCHK, substitution and
      *> equivalent revision rules.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : ITEM AND PART MAINTENANCE
      *> The specification's worked rule is seeded: two type B flat
      *> mount brackets replace one type A, approved by engineering
      *> for kit and bill contexts and requiring customer agreement.
      *> A substitute is a proposal, so a pair with no rule, a rule
      *> used outside its context or outside its validity dates, and a
      *> substitution with no agreement on record are all refused.
      *> Output protocol (TAP): "ok n - text" / "not ok n - text".
      *>================================================================
      *> REQ: ITM-019
      *> AC: AC-075
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSUBSTCK.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY SUBSTCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-SB==.
       01  WS-ITEM-A                   PIC X(40)
               VALUE "BRKT-MECH-DISC-FLATMOUNT-160-A".
       01  WS-ITEM-B                   PIC X(40)
               VALUE "BRKT-MECH-DISC-FLATMOUNT-160-B".
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSUBSTCK" TO TK-SUITE
           PERFORM TK-BEGIN

           PERFORM 1000-RATIO-OF-TWO
           PERFORM 1100-QUANTITY-SCALES
           PERFORM 1200-AGREEMENT-REQUIRED
           PERFORM 1300-WRONG-WAY-ROUND
           PERFORM 1400-NO-RULE-AT-ALL
           PERFORM 1500-BEFORE-VALID-FROM
           PERFORM 1600-LIST-SHOWS-RULE
           PERFORM 1700-REVISION-CHECK
           PERFORM 1800-BAD-FUNCTION

           PERFORM TK-END
           STOP RUN.

      *>   REQ: ITM-019  AC: AC-075
       1000-RATIO-OF-TWO.
           PERFORM 9000-RESET
           MOVE "CHECK" TO CA-FUNCTION OF WS-SB
           MOVE WS-ITEM-A TO SB-ITEM-CODE
           MOVE WS-ITEM-B TO SB-SUBSTITUTE
           MOVE "KIT" TO SB-CONTEXT
           MOVE 1 TO SB-QTY-REQUIRED
           CALL "SUBSTCHK" USING WS-SB
           MOVE "two type B brackets replace one type A" TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE SB-RATIO TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-SB TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the approved rule names its approver" TO TK-NAME
           IF SB-APPROVED-BY NOT = SPACES
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SB TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-019  AC: AC-075
       1100-QUANTITY-SCALES.
           PERFORM 9000-RESET
           MOVE "CHECK" TO CA-FUNCTION OF WS-SB
           MOVE WS-ITEM-A TO SB-ITEM-CODE
           MOVE WS-ITEM-B TO SB-SUBSTITUTE
           MOVE "KIT" TO SB-CONTEXT
           MOVE 25 TO SB-QTY-REQUIRED
           CALL "SUBSTCHK" USING WS-SB
           MOVE "25 required becomes 50 of the substitute"
               TO TK-NAME
           MOVE 50 TO TK-EXPECTED-N
           MOVE SB-QTY-SUBSTITUTE TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-SB TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-019  AC: AC-075
       1200-AGREEMENT-REQUIRED.
           PERFORM 9000-RESET
           MOVE "CHECK" TO CA-FUNCTION OF WS-SB
           MOVE WS-ITEM-A TO SB-ITEM-CODE
           MOVE WS-ITEM-B TO SB-SUBSTITUTE
           MOVE "KIT" TO SB-CONTEXT
           MOVE 1 TO SB-QTY-REQUIRED
           MOVE SPACES TO SB-AGREEMENT-REF
           CALL "SUBSTCHK" USING WS-SB
           MOVE "the rule states that agreement is required"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SB-NEEDS-AGREEMENT TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-SB TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           MOVE "without that agreement it is not permitted yet"
               TO TK-NAME
           IF SB-PERMITTED NOT = "Y"
                   OR CA-RETURN-CODE OF WS-SB NOT = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SB TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-019  AC: AC-075
       1300-WRONG-WAY-ROUND.
           PERFORM 9000-RESET
           MOVE "CHECK" TO CA-FUNCTION OF WS-SB
           MOVE WS-ITEM-B TO SB-ITEM-CODE
           MOVE WS-ITEM-A TO SB-SUBSTITUTE
           MOVE "KIT" TO SB-CONTEXT
           MOVE 1 TO SB-QTY-REQUIRED
           CALL "SUBSTCHK" USING WS-SB
           MOVE "a rule does not work in the opposite direction"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-SB NOT = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SB TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-019
       1400-NO-RULE-AT-ALL.
           PERFORM 9000-RESET
           MOVE "CHECK" TO CA-FUNCTION OF WS-SB
           MOVE WS-ITEM-A TO SB-ITEM-CODE
           MOVE "LINK-QUICK-11S" TO SB-SUBSTITUTE
           MOVE "KIT" TO SB-CONTEXT
           MOVE 1 TO SB-QTY-REQUIRED
           CALL "SUBSTCHK" USING WS-SB
           MOVE "an unapproved pair is never silently swapped in"
               TO TK-NAME
           MOVE "SUB0001" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SB TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-SB TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-019  AC: AC-075
       1500-BEFORE-VALID-FROM.
           PERFORM 9000-RESET
           MOVE "CHECK" TO CA-FUNCTION OF WS-SB
           MOVE WS-ITEM-A TO SB-ITEM-CODE
           MOVE WS-ITEM-B TO SB-SUBSTITUTE
           MOVE "KIT" TO SB-CONTEXT
           MOVE 20250101 TO SB-AS-AT
           MOVE 1 TO SB-QTY-REQUIRED
           CALL "SUBSTCHK" USING WS-SB
           MOVE "the rule is not valid before it was approved"
               TO TK-NAME
           MOVE "SUB0003" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SB TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-SB TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-019
       1600-LIST-SHOWS-RULE.
           PERFORM 9000-RESET
           MOVE "LIST" TO CA-FUNCTION OF WS-SB
           MOVE WS-ITEM-A TO SB-ITEM-CODE
           MOVE "KIT" TO SB-CONTEXT
           CALL "SUBSTCHK" USING WS-SB
           MOVE "the approved alternative is offered as a proposal"
               TO TK-NAME
           IF SB-LINE-CNT >= 1
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SB TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "the proposal carries its own ratio" TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           IF SB-LINE-CNT >= 1
               MOVE SB-L-RATIO(1) TO TK-ACTUAL-N
           ELSE
               MOVE 0 TO TK-ACTUAL-N
           END-IF
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-019
       1700-REVISION-CHECK.
           PERFORM 9000-RESET
           MOVE "REVCHECK" TO CA-FUNCTION OF WS-SB
           MOVE WS-ITEM-A TO SB-ITEM-CODE
           MOVE "A" TO SB-REVISION
           MOVE "Z" TO SB-SUBSTITUTE-REV
           CALL "SUBSTCHK" USING WS-SB
           MOVE "an unknown revision is not interchangeable"
               TO TK-NAME
           IF SB-INTERCHANGEABLE NOT = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SB TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-019
       1800-BAD-FUNCTION.
           PERFORM 9000-RESET
           MOVE "WIBBLE" TO CA-FUNCTION OF WS-SB
           MOVE WS-ITEM-A TO SB-ITEM-CODE
           CALL "SUBSTCHK" USING WS-SB
           MOVE "an unsupported function is refused" TO TK-NAME
           MOVE "SUB0008" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SB TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-SB TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

       9000-RESET.
           INITIALIZE WS-SB
           MOVE "STC" TO CA-COMPANY OF WS-SB
           MOVE "RDG" TO CA-SITE OF WS-SB
           MOVE "ENGINEER" TO CA-USER OF WS-SB
           MOVE 20261005 TO CA-BUSINESS-DATE OF WS-SB
           MOVE 20261005 TO SB-AS-AT.

       COPY TESTKITP.
