      *>================================================================
      *> TSOPRICE - unit tests for PRICING, the deterministic selling
      *>            price precedence.
      *>   AC-040  the selected price can be explained, a price that
      *>           does not exist is reported rather than invented,
      *>           and equally ranked prices block confirmation
      *> The validation cases are pure: PRICING rejects them before it
      *> reaches the database.  The cases that need so.PriceList are
      *> wrapped in BEGN ... ROLB and are skipped when the tables are
      *> not deployed on this machine.
      *> Output protocol (TAP): "ok n - text" / "not ok n - text".
      *>================================================================
      *> REQ: SO-004   AC: AC-040
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSOPRICE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY SQLIOCA.
       01  WS-I                        PIC S9(9) COMP-5 VALUE 0.
       01  WS-DB-OK                    PIC X VALUE "?".
           88  WS-DB-AVAILABLE                   VALUE "Y".
       01  WS-ROW.
           05  WS-R-COL1               PIC X(100).
       COPY PRICINGCA REPLACING ==DFHCOMMAREA== BY ==WS-PRICE-CA==.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSOPRICE" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-VALIDATION
           PERFORM 2000-DEFAULTS
           PERFORM 3000-DATABASE-CASES
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
      *> 1000  INPUT VALIDATION - NO DATABASE IS TOUCHED
      *>----------------------------------------------------------------
       1000-VALIDATION.
           PERFORM 9100-SET-UP
           MOVE SPACES TO CA-COMPANY OF WS-PRICE-CA
           PERFORM 9000-CALL-PRICING
           MOVE "SO-004 a missing company is rejected PRC0001"
               TO TK-NAME
           MOVE "PRC0001" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-PRICE-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 9100-SET-UP
           MOVE SPACES TO PR-ITEM-CODE
           PERFORM 9000-CALL-PRICING
           MOVE "SO-004 a missing item is rejected PRC0001"
               TO TK-NAME
           MOVE "PRC0001" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-PRICE-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "SO-004 a rejected request returns 08" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-PRICE-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 9100-SET-UP
           MOVE 0 TO PR-QUANTITY
           PERFORM 9000-CALL-PRICING
           MOVE "SO-004 a zero quantity is rejected PRC0002"
               TO TK-NAME
           MOVE "PRC0002" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-PRICE-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 9100-SET-UP
           MOVE 12.500000 TO PR-MANUAL-PRICE
           MOVE SPACES TO PR-MANUAL-APPROVAL
           PERFORM 9000-CALL-PRICING
           MOVE "SO-004 a manual price needs an approval reference"
               TO TK-NAME
           MOVE "PRC0009" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-PRICE-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           PERFORM 9100-SET-UP
           MOVE "GUESS" TO CA-FUNCTION OF WS-PRICE-CA
           PERFORM 9000-CALL-PRICING
           MOVE "SO-004 an unsupported function is rejected"
               TO TK-NAME
           MOVE "PRC0008" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-PRICE-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "SO-004 a bad function does not invent a price"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE PR-UNIT-PRICE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 2000  THE DEFAULTS THAT MAKE THE ANSWER REPRODUCIBLE
      *>----------------------------------------------------------------
       2000-DEFAULTS.
           PERFORM 9100-SET-UP
           MOVE 0 TO PR-PRICE-DATE
           MOVE 20261006 TO PR-ORDER-DATE
           MOVE 0 TO PR-QUANTITY
           PERFORM 9000-CALL-PRICING
           MOVE "SO-004 the order date is the default price date"
               TO TK-NAME
           MOVE 20261006 TO TK-EXPECTED-N
           MOVE PR-PRICE-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 9100-SET-UP
           MOVE 0 TO PR-PRICE-DATE PR-ORDER-DATE
           MOVE 20261101 TO CA-BUSINESS-DATE OF WS-PRICE-CA
           MOVE 0 TO PR-QUANTITY
           PERFORM 9000-CALL-PRICING
           MOVE "SO-004 without an order date the business date is"
               TO TK-NAME
           MOVE 20261101 TO TK-EXPECTED-N
           MOVE PR-PRICE-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 9100-SET-UP
           MOVE 9 TO PR-MAX-LEVEL
           MOVE 0 TO PR-QUANTITY
           PERFORM 9000-CALL-PRICING
           MOVE "SO-004 the precedence never goes beyond level 5"
               TO TK-NAME
           MOVE 5 TO TK-EXPECTED-N
           MOVE PR-MAX-LEVEL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 9100-SET-UP
           MOVE SPACES TO PR-CURRENCY
           MOVE 0 TO PR-QUANTITY
           PERFORM 9000-CALL-PRICING
           MOVE "SO-004 the base currency is assumed to be GBP"
               TO TK-NAME
           MOVE "GBP" TO TK-EXPECTED-X
           MOVE PR-CURRENCY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 3000  CASES THAT READ so.PriceList.  BEGN ... ROLB keeps the
      *>       test read only; a machine without the tables skips.
      *>----------------------------------------------------------------
       3000-DATABASE-CASES.
           PERFORM 9200-PROBE-DATABASE
           IF NOT WS-DB-AVAILABLE
               PERFORM 3900-SKIP-DB-CASES
               GO TO 3000-EXIT
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW

           PERFORM 9100-SET-UP
           MOVE "ZZZ-NO-SUCH-ITEM" TO PR-ITEM-CODE
           PERFORM 9000-CALL-PRICING
           MOVE "AC-040 an item with no price is reported, not made"
               TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-PRICE-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-040 the not found reason is PRC0003" TO TK-NAME
           MOVE "PRC0003" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-PRICE-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "AC-040 no price means no unit price is returned"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE PR-UNIT-PRICE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 9100-SET-UP
           MOVE "EXPLAIN" TO CA-FUNCTION OF WS-PRICE-CA
           MOVE "ZZZ-NO-SUCH-ITEM" TO PR-ITEM-CODE
           PERFORM 9000-CALL-PRICING
           MOVE "AC-040 EXPLAIN returns an empty candidate list"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE PR-CAND-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 9100-SET-UP
           MOVE "BREAKS" TO CA-FUNCTION OF WS-PRICE-CA
           MOVE "ZZZ-NO-SUCH-ITEM" TO PR-ITEM-CODE
           PERFORM 9000-CALL-PRICING
           MOVE "AC-040 BREAKS reports that no breaks are held"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE PR-BREAK-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 9100-SET-UP
           MOVE "EXPLAIN" TO CA-FUNCTION OF WS-PRICE-CA
           MOVE "ZZZ-NO-SUCH-ITEM" TO PR-ITEM-CODE
           MOVE 12.500000 TO PR-MANUAL-PRICE
           MOVE 1 TO PR-MANUAL-PER-QTY
           MOVE "APPR000401" TO PR-MANUAL-APPROVAL
           MOVE "Agreed by the sales manager" TO PR-MANUAL-REASON
           PERFORM 9000-CALL-PRICING

           MOVE "AC-040 an approved manual price is level 1"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE PR-PRICE-LEVEL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-040 the manual price is the price used"
               TO TK-NAME
           MOVE 12.500000 TO TK-EXPECTED-N
           MOVE PR-UNIT-PRICE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-040 the source is reported as MANUAL" TO TK-NAME
           MOVE "MANUAL" TO TK-EXPECTED-X
           MOVE PR-PRICE-SOURCE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "AC-040 the selected price carries an explanation"
               TO TK-NAME
           IF PR-EXPLANATION NOT = SPACES
               PERFORM TK-PASS
           ELSE
               MOVE "no explanation was returned" TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "AC-040 one unambiguous price does not block"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE PR-BLOCK-CONFIRM TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "ROLB" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       3000-EXIT.
           EXIT.

       3900-SKIP-DB-CASES.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               MOVE "AC-040 price list case needs so.PriceList"
                   TO TK-NAME
               MOVE "the sales price tables are not deployed here"
                   TO TK-DETAIL
               PERFORM TK-SKIP
           END-PERFORM.

      *>----------------------------------------------------------------
      *> 9000  SHARED SET UP
      *>----------------------------------------------------------------
       9000-CALL-PRICING.
           CALL "PRICING" USING WS-PRICE-CA.

       9100-SET-UP.
           INITIALIZE WS-PRICE-CA
           MOVE "SELECT" TO CA-FUNCTION OF WS-PRICE-CA
           MOVE "STC" TO CA-COMPANY OF WS-PRICE-CA
           MOVE "RDG" TO CA-SITE OF WS-PRICE-CA
           MOVE "SALESREP" TO CA-USER OF WS-PRICE-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-PRICE-CA
           MOVE "WID-001" TO PR-ITEM-CODE
           MOVE "C000001" TO PR-CUSTOMER
           MOVE "TRADE" TO PR-PRICE-GROUP
           MOVE "EA" TO PR-UOM-CODE
           MOVE "GBP" TO PR-CURRENCY
           MOVE 24 TO PR-QUANTITY
           MOVE 20261006 TO PR-ORDER-DATE
           MOVE 20261006 TO PR-PRICE-DATE
           MOVE "SALESORDER" TO PR-DOC-TYPE
           MOVE "SO100040" TO PR-DOC-REF
           MOVE 1 TO PR-DOC-VERSION
           MOVE 5 TO PR-MAX-LEVEL
      *>   "N" keeps PARMGET out of the pure cases; the cross
      *>   currency policy itself is proved by the PARMGET tests
           MOVE "N" TO PR-XCURR-ALLOW.

      *>   a single harmless read tells us whether the sales price
      *>   tables are deployed on this machine
       9200-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT COUNT(*) FROM so.PriceList"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 100 TO SQLIO-COL-WIDTH(1)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               MOVE "N" TO WS-DB-OK
           ELSE
               MOVE "Y" TO WS-DB-OK
           END-IF.

       COPY TESTKITP.
