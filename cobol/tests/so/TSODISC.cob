      *>================================================================
      *> TSODISC - unit tests for SODISC, the sales calculation
      *>           sequence, and for the margin measures of SOMARGN.
      *> The worked example is the one in the functional specification
      *> and in the acceptance catalogue:
      *>   AC-041  three cartons of eight units at 15.00 each
      *>           gross 360.00, sequential 10% then 5% -> net 307.80
      *>           (emphatically NOT 306.00), VAT at 20% -> 61.56,
      *>           gross 369.36, comparable cost 216.00 ->
      *>           margin 29.8246% and markup 42.5%
      *> Nothing here needs the database: the unit conversion is the
      *> captured factor and the comparable cost is supplied.
      *> Output protocol (TAP): "ok n - text" / "not ok n - text".
      *>================================================================
      *> REQ: SO-005, SO-006   AC: AC-041
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSODISC.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       01  WS-I                        PIC S9(9) COMP-5 VALUE 0.
       01  WS-SUM                      PIC S9(13)V9(2) VALUE 0.
       COPY SODISCCA REPLACING ==DFHCOMMAREA== BY ==WS-SODISC-CA==.
       COPY SOMARGNCA REPLACING ==DFHCOMMAREA== BY ==WS-MARGIN-CA==.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSODISC" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-WORKED-EXAMPLE
           PERFORM 2000-NOT-ADDITIVE
           PERFORM 3000-IDEMPOTENT
           PERFORM 4000-ALLOWANCE-SPREAD
           PERFORM 5000-CARRIAGE-FOLLOWS-GOODS
           PERFORM 6000-NO-CONVERSION
           PERFORM 7000-MARGIN-AND-MARKUP
           PERFORM 8000-MISSING-COST
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
      *> 1000  AC-041 LINE BY LINE
      *>----------------------------------------------------------------
       1000-WORKED-EXAMPLE.
           PERFORM 9100-SET-UP-AC041
           PERFORM 9000-CALL-SODISC

           MOVE "AC-041 3 cartons of 8 units convert to 24 units"
               TO TK-NAME
           MOVE 24 TO TK-EXPECTED-N
           MOVE SD-L-PRICE-QTY(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-041 24 units at 15.00 extend to 360.00"
               TO TK-NAME
           MOVE 360.00 TO TK-EXPECTED-N
           MOVE SD-L-GROSS-AMT(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-041 10% then 5% on 360.00 give net 307.80"
               TO TK-NAME
           MOVE 307.80 TO TK-EXPECTED-N
           MOVE SD-L-NET-AMT(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-041 the discount given away is 52.20" TO TK-NAME
           MOVE 52.20 TO TK-EXPECTED-N
           MOVE SD-L-DISC-AMT(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-041 VAT at 20% on 307.80 is 61.56" TO TK-NAME
           MOVE 61.56 TO TK-EXPECTED-N
           MOVE SD-L-VAT-AMT(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-041 gross is 369.36" TO TK-NAME
           MOVE 369.36 TO TK-EXPECTED-N
           MOVE SD-L-GROSS-TOT(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-041 document net agrees with the line" TO TK-NAME
           MOVE 307.80 TO TK-EXPECTED-N
           MOVE SD-TOT-NET OF WS-SODISC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-041 document VAT agrees with the line" TO TK-NAME
           MOVE 61.56 TO TK-EXPECTED-N
           MOVE SD-TOT-VAT OF WS-SODISC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-041 document gross agrees with the line"
               TO TK-NAME
           MOVE 369.36 TO TK-EXPECTED-N
           MOVE SD-TOT-GROSS-TOTAL OF WS-SODISC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-041 net, VAT and gross reconcile" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SD-RECONCILED OF WS-SODISC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "AC-041 the calculation returns 00" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-SODISC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 2000  THE WHOLE POINT OF CR0988
      *>----------------------------------------------------------------
       2000-NOT-ADDITIVE.
           PERFORM 9100-SET-UP-AC041
           PERFORM 9000-CALL-SODISC

           MOVE "AC-041 sequential discounts are not 15% off"
               TO TK-NAME
           IF SD-L-NET-AMT(1) NOT = 306.00
               PERFORM TK-PASS
           ELSE
               MOVE "306.00 means the percentages were added"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "AC-041 VAT is not the 61.20 of an additive discount"
               TO TK-NAME
           IF SD-L-VAT-AMT(1) NOT = 61.20
               PERFORM TK-PASS
           ELSE
               MOVE "VAT followed an additive discount" TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "a single 14.5% discount also gives 307.80"
               TO TK-NAME
           PERFORM 9100-SET-UP-AC041
           MOVE 14.5 TO SD-L-DISC-PCT1(1)
           MOVE 0 TO SD-L-DISC-PCT2(1)
           PERFORM 9000-CALL-SODISC
           MOVE 307.80 TO TK-EXPECTED-N
           MOVE SD-L-NET-AMT(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 3000  THE SAME REQUEST TWICE MUST NOT DISCOUNT TWICE
      *>----------------------------------------------------------------
       3000-IDEMPOTENT.
           PERFORM 9100-SET-UP-AC041
           PERFORM 9000-CALL-SODISC
           PERFORM 9000-CALL-SODISC

           MOVE "SO-005 a repeated call still gives net 307.80"
               TO TK-NAME
           MOVE 307.80 TO TK-EXPECTED-N
           MOVE SD-L-NET-AMT(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "SO-005 the repeat is reported as a repeat"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SD-ALREADY-APPLIED OF WS-SODISC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "SO-005 the gross before discount is untouched"
               TO TK-NAME
           MOVE 360.00 TO TK-EXPECTED-N
           MOVE SD-L-GROSS-AMT(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 4000  A HEADER ALLOWANCE MUST RE-ADD TO ITSELF
      *>----------------------------------------------------------------
       4000-ALLOWANCE-SPREAD.
           INITIALIZE WS-SODISC-CA
           MOVE "DOCUMENT" TO CA-FUNCTION OF WS-SODISC-CA
           MOVE "STC" TO CA-COMPANY OF WS-SODISC-CA
           MOVE "SALESREP" TO CA-USER OF WS-SODISC-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-SODISC-CA
           MOVE "GBP" TO SD-CURRENCY
           MOVE 2 TO SD-ROUND-PLACES
           MOVE 3 TO SD-LINE-CNT
           MOVE 10.00 TO SD-DOC-ALLOWANCE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               MOVE WS-I TO SD-L-LINE-NUM(WS-I)
               MOVE "EA" TO SD-L-ENT-UOM(WS-I)
               MOVE "EA" TO SD-L-PRICE-UOM(WS-I)
               MOVE 1 TO SD-L-ENT-QTY(WS-I)
               MOVE 1 TO SD-L-FACTOR(WS-I)
               MOVE 100.00 TO SD-L-UNIT-PRICE(WS-I)
               MOVE 1 TO SD-L-PER-QTY(WS-I)
               MOVE "S" TO SD-L-VAT-CODE(WS-I)
               MOVE 20.0000 TO SD-L-VAT-RATE(WS-I)
           END-PERFORM
           PERFORM 9000-CALL-SODISC

           MOVE "SO-005 the allocated parts re-add to 10.00"
               TO TK-NAME
           MOVE 0 TO WS-SUM
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               ADD SD-L-DOC-ALLOW(WS-I) TO WS-SUM
           END-PERFORM
           MOVE 10.00 TO TK-EXPECTED-N
           MOVE WS-SUM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "SO-005 the spare penny goes to the first line"
               TO TK-NAME
           MOVE 3.34 TO TK-EXPECTED-N
           MOVE SD-L-DOC-ALLOW(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "SO-005 document net is 300.00 less 10.00"
               TO TK-NAME
           MOVE 290.00 TO TK-EXPECTED-N
           MOVE SD-TOT-NET OF WS-SODISC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "SO-005 line totals stay consistent with the header"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SD-RECONCILED OF WS-SODISC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 5000  CARRIAGE TAKES THE VAT LIABILITY OF THE GOODS
      *>----------------------------------------------------------------
       5000-CARRIAGE-FOLLOWS-GOODS.
           PERFORM 9100-SET-UP-AC041
           MOVE 0 TO SD-L-DISC-PCT1(1) SD-L-DISC-PCT2(1)
           MOVE 10.00 TO SD-L-CHARGE(1)
           PERFORM 9000-CALL-SODISC

           MOVE "SO-005 carriage is added to the line net"
               TO TK-NAME
           MOVE 370.00 TO TK-EXPECTED-N
           MOVE SD-L-NET-AMT(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "SO-005 carriage carries the goods VAT code"
               TO TK-NAME
           MOVE "S" TO TK-EXPECTED-X
           MOVE SD-L-CHARGE-VAT(1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "SO-005 VAT is charged on goods plus carriage"
               TO TK-NAME
           MOVE 74.00 TO TK-EXPECTED-N
           MOVE SD-L-VAT-AMT(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 6000  A MISSING CONVERSION IS NEVER AN IMPLIED ONE
      *>----------------------------------------------------------------
       6000-NO-CONVERSION.
           PERFORM 9100-SET-UP-AC041
           MOVE 0 TO SD-L-FACTOR(1)
           MOVE SPACES TO SD-L-ITEM-CODE(1)
           PERFORM 9000-CALL-SODISC

           MOVE "SO-005 no conversion and no item is rejected 08"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-SODISC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "SO-005 the reason is DSC0007" TO TK-NAME
           MOVE "DSC0007" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SODISC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 7000  AC-041 MARGIN AND MARKUP ARE DIFFERENT MEASURES
      *>----------------------------------------------------------------
       7000-MARGIN-AND-MARKUP.
           INITIALIZE WS-MARGIN-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-MARGIN-CA
           MOVE "STC" TO CA-COMPANY OF WS-MARGIN-CA
           MOVE "SALESREP" TO CA-USER OF WS-MARGIN-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-MARGIN-CA
           MOVE "SALESORDER" TO SM-DOC-TYPE
           MOVE "SO100041" TO SM-DOC-REF
           MOVE "WID-001" TO SM-ITEM-CODE
           MOVE 24 TO SM-QUANTITY
           MOVE 307.80 TO SM-NET-AMOUNT
           MOVE 216.00 TO SM-COST-AMOUNT
           MOVE "SUPPLIED" TO SM-COST-BASIS
           MOVE 30.0000 TO SM-MIN-MARGIN-PCT
           MOVE "N" TO SM-RAISE-EXCEPTION
           CALL "SOMARGN" USING WS-MARGIN-CA

           MOVE "AC-041 margin on 307.80 less 216.00 is 29.8246%"
               TO TK-NAME
           MOVE 29.8246 TO TK-EXPECTED-N
           MOVE SM-MARGIN-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-041 markup on the same figures is 42.5%"
               TO TK-NAME
           MOVE 42.5000 TO TK-EXPECTED-N
           MOVE SM-MARKUP-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-041 margin and markup are not the same number"
               TO TK-NAME
           IF SM-MARGIN-PCT NOT = SM-MARKUP-PCT
               PERFORM TK-PASS
           ELSE
               MOVE "the two denominators have been confused"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "AC-041 the margin value is 91.80" TO TK-NAME
           MOVE 91.80 TO TK-EXPECTED-N
           MOVE SM-MARGIN-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-041 29.8246% is below a 30% policy minimum"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SM-BELOW-MINIMUM TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "AC-041 a below policy margin is held at 06"
               TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-MARGIN-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 8000  A MISSING COST IS NOT ZERO
      *>----------------------------------------------------------------
       8000-MISSING-COST.
           INITIALIZE WS-MARGIN-CA
           MOVE "LINE" TO CA-FUNCTION OF WS-MARGIN-CA
           MOVE "STC" TO CA-COMPANY OF WS-MARGIN-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-MARGIN-CA
           MOVE 307.80 TO SM-NET-AMOUNT
           MOVE 24 TO SM-QUANTITY
           MOVE 0 TO SM-COST-AMOUNT SM-UNIT-COST
           MOVE SPACES TO SM-ITEM-CODE
           CALL "SOMARGN" USING WS-MARGIN-CA

           MOVE "SO-006 a missing cost holds the line at 06"
               TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-MARGIN-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "SO-006 the reason is MRG0002" TO TK-NAME
           MOVE "MRG0002" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-MARGIN-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "SO-006 a missing cost is not a 100% margin"
               TO TK-NAME
           MOVE "INCOMPLETE" TO TK-EXPECTED-X
           MOVE SM-MARGIN-STATE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "SO-006 a zero net makes the margin undefined"
               TO TK-NAME
           INITIALIZE WS-MARGIN-CA
           MOVE "LINE" TO CA-FUNCTION OF WS-MARGIN-CA
           MOVE "STC" TO CA-COMPANY OF WS-MARGIN-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-MARGIN-CA
           MOVE 0 TO SM-NET-AMOUNT
           MOVE 1 TO SM-QUANTITY
           MOVE 216.00 TO SM-COST-AMOUNT
           MOVE "SUPPLIED" TO SM-COST-BASIS
           CALL "SOMARGN" USING WS-MARGIN-CA
           MOVE "UNDEFINED" TO TK-EXPECTED-X
           MOVE SM-MARGIN-STATE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "SO-006 an undefined margin is not reported as zero"
               TO TK-NAME
           IF SM-REVIEW-REASON NOT = SPACES
               PERFORM TK-PASS
           ELSE
               MOVE "no review condition was raised" TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> 9000  SHARED SET UP
      *>----------------------------------------------------------------
       9000-CALL-SODISC.
           CALL "SODISC" USING WS-SODISC-CA.

       9100-SET-UP-AC041.
           INITIALIZE WS-SODISC-CA
           MOVE "LINE" TO CA-FUNCTION OF WS-SODISC-CA
           MOVE "STC" TO CA-COMPANY OF WS-SODISC-CA
           MOVE "RDG" TO CA-SITE OF WS-SODISC-CA
           MOVE "SALESREP" TO CA-USER OF WS-SODISC-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-SODISC-CA
           MOVE "SALESORDER" TO SD-DOC-TYPE
           MOVE "SO100041" TO SD-DOC-REF
           MOVE "GBP" TO SD-CURRENCY
           MOVE 20261006 TO SD-PRICE-DATE
           MOVE 2 TO SD-ROUND-PLACES
           MOVE "GOODS" TO SD-CHARGE-VAT-BASIS
           MOVE 1 TO SD-LINE-CNT
           MOVE 1 TO SD-L-LINE-NUM(1)
           MOVE "0000001" TO SD-L-LINE-REF(1)
           MOVE "WID-001" TO SD-L-ITEM-CODE(1)
           MOVE 3 TO SD-L-ENT-QTY(1)
           MOVE "CARTON" TO SD-L-ENT-UOM(1)
           MOVE "EA" TO SD-L-PRICE-UOM(1)
           MOVE 8 TO SD-L-FACTOR(1)
           MOVE 15.000000 TO SD-L-UNIT-PRICE(1)
           MOVE 1 TO SD-L-PER-QTY(1)
           MOVE 10.0000 TO SD-L-DISC-PCT1(1)
           MOVE 5.0000 TO SD-L-DISC-PCT2(1)
           MOVE "S" TO SD-L-VAT-CODE(1)
           MOVE 20.0000 TO SD-L-VAT-RATE(1)
           MOVE 216.00 TO SD-L-COST-GBP(1).

       COPY TESTKITP.
