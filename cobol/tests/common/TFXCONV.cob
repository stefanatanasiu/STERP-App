      *>================================================================
      *> TFXCONV - unit tests for FXCONV (currency conversion)
      *> D-14 quotes rates as foreign currency units per one pound
      *> sterling, so sterling = foreign / rate.  COM-010 blocks a
      *> missing, zero, negative or reversed direction rate and never
      *> assumes parity.  Worked examples use the go-live currencies
      *> EUR and USD (D-14) and the AC-005 rate scenarios.
      *>================================================================
      *> REQ: COM-010, COM-011   AC: AC-005
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TFXCONV.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       01  WS-OK                       PIC X VALUE "Y".
       COPY FXCONVCA REPLACING ==DFHCOMMAREA== BY ==WS-FX-CA==.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TFXCONV" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-CONVERSION-TESTS
           PERFORM 2000-DIRECTION-TESTS
           PERFORM 3000-REALISED-TESTS
           PERFORM 4000-ERROR-TESTS
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
      *> US$1.25 = GBP 1, so US$1,250.00 is GBP 1,000.00
      *>----------------------------------------------------------------
       1000-CONVERSION-TESTS.
           MOVE "USD 1,250.00 at 1.25 per GBP is GBP 1,000.00"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "USD" TO FX-CURRENCY OF WS-FX-CA
           MOVE 1.25 TO FX-RATE OF WS-FX-CA
           MOVE 1250.00 TO FX-FOREIGN-AMOUNT OF WS-FX-CA
           PERFORM 8100-CALL-TOGBP
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE FX-GBP-AMOUNT OF WS-FX-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "EUR 1,150.00 at 1.15 per GBP is GBP 1,000.00"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "EUR" TO FX-CURRENCY OF WS-FX-CA
           MOVE 1.15 TO FX-RATE OF WS-FX-CA
           MOVE 1150.00 TO FX-FOREIGN-AMOUNT OF WS-FX-CA
           PERFORM 8100-CALL-TOGBP
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE FX-GBP-AMOUNT OF WS-FX-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "GBP 1,000.00 at 1.15 per GBP is EUR 1,150.00"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "EUR" TO FX-CURRENCY OF WS-FX-CA
           MOVE 1.15 TO FX-RATE OF WS-FX-CA
           MOVE 1000.00 TO FX-GBP-AMOUNT OF WS-FX-CA
           MOVE "TOFCY" TO CA-FUNCTION OF WS-FX-CA
           CALL "FXCONV" USING WS-FX-CA
           MOVE 1150.00 TO TK-EXPECTED-N
           MOVE FX-FOREIGN-AMOUNT OF WS-FX-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "an awkward rate rounds once to the penny" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "EUR" TO FX-CURRENCY OF WS-FX-CA
           MOVE 1.17 TO FX-RATE OF WS-FX-CA
           MOVE 1000.00 TO FX-FOREIGN-AMOUNT OF WS-FX-CA
           PERFORM 8100-CALL-TOGBP
           MOVE 854.70 TO TK-EXPECTED-N
           MOVE FX-GBP-AMOUNT OF WS-FX-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "the residual is reported, not hidden in the amount"
               TO TK-NAME
           IF FX-RESIDUAL OF WS-FX-CA NOT = 0
              AND FX-RESIDUAL OF WS-FX-CA < 0.01
              AND FX-RESIDUAL OF WS-FX-CA > -0.01
               PERFORM TK-PASS
           ELSE
               MOVE FX-RESIDUAL OF WS-FX-CA TO TK-NUM-ED
               MOVE TK-NUM-ED TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "twelve decimals are kept before rounding" TO TK-NAME
           IF FX-UNROUNDED OF WS-FX-CA > 854.700854
              AND FX-UNROUNDED OF WS-FX-CA < 854.700855
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "GBP to GBP passes through at a rate of one"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "GBP" TO FX-CURRENCY OF WS-FX-CA
           MOVE 500.00 TO FX-FOREIGN-AMOUNT OF WS-FX-CA
           PERFORM 8100-CALL-TOGBP
           IF FX-GBP-AMOUNT OF WS-FX-CA = 500.00
              AND FX-RATE OF WS-FX-CA = 1
              AND CA-RETURN-CODE OF WS-FX-CA = 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "the rate is reported with its direction in words"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "USD" TO FX-CURRENCY OF WS-FX-CA
           MOVE 1.25 TO FX-RATE OF WS-FX-CA
           MOVE 100.00 TO FX-FOREIGN-AMOUNT OF WS-FX-CA
           PERFORM 8100-CALL-TOGBP
           MOVE "1.25000000 USD per GBP 1" TO TK-EXPECTED-X
           MOVE FX-RATE-TEXT OF WS-FX-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "a six decimal conversion is available" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "EUR" TO FX-CURRENCY OF WS-FX-CA
           MOVE 1.17 TO FX-RATE OF WS-FX-CA
           MOVE 1000.00 TO FX-FOREIGN-AMOUNT OF WS-FX-CA
           MOVE 6 TO FX-PLACES OF WS-FX-CA
           PERFORM 8100-CALL-TOGBP
           MOVE 854.700855 TO TK-EXPECTED-N
           MOVE FX-GBP-AMOUNT OF WS-FX-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> COM-010 - the direction must never be inferred from magnitude
      *>----------------------------------------------------------------
       2000-DIRECTION-TESTS.
           MOVE "a reversed quotation is refused by default" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "USD" TO FX-CURRENCY OF WS-FX-CA
           MOVE 0.8 TO FX-RATE OF WS-FX-CA
           MOVE "GBPPERFCY" TO FX-QUOTE-BASIS OF WS-FX-CA
           MOVE 1250.00 TO FX-FOREIGN-AMOUNT OF WS-FX-CA
           PERFORM 8100-CALL-TOGBP
           IF CA-RETURN-CODE OF WS-FX-CA = 8
              AND CA-REASON-CODE OF WS-FX-CA = "FX0003"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-FX-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "a reversed quotation is turned round on request"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "USD" TO FX-CURRENCY OF WS-FX-CA
           MOVE 0.8 TO FX-RATE OF WS-FX-CA
           MOVE "GBPPERFCY" TO FX-QUOTE-BASIS OF WS-FX-CA
           MOVE "Y" TO FX-ALLOW-INVERT OF WS-FX-CA
           MOVE 1250.00 TO FX-FOREIGN-AMOUNT OF WS-FX-CA
           PERFORM 8100-CALL-TOGBP
           IF FX-RATE OF WS-FX-CA = 1.25
              AND FX-GBP-AMOUNT OF WS-FX-CA = 1000.00
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-FX-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "both quotations are retained after inversion"
               TO TK-NAME
           IF FX-RATE-ENTERED OF WS-FX-CA = 0.8
              AND FX-RATE OF WS-FX-CA = 1.25
              AND FX-QUOTE-BASIS OF WS-FX-CA = "FCYPERGBP"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "the inversion is reported as a warning" TO TK-NAME
           IF CA-RETURN-CODE OF WS-FX-CA = 4
              AND CA-REASON-CODE OF WS-FX-CA = "FX0004"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> Realised exchange difference on settlement (D-14).
      *> EUR 1,150.00 booked at 1.15 is GBP 1,000.00; settled at 1.20
      *> it realises GBP 958.33, a loss of GBP 41.67 on a receivable
      *> and a gain of GBP 41.67 on a payable.
      *>----------------------------------------------------------------
       3000-REALISED-TESTS.
           MOVE "a receivable settling at a weaker rate makes a loss"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "EUR" TO FX-CURRENCY OF WS-FX-CA
           MOVE 1.15 TO FX-RATE OF WS-FX-CA
           MOVE 1.20 TO FX-RATE-2 OF WS-FX-CA
           MOVE 1150.00 TO FX-FOREIGN-AMOUNT OF WS-FX-CA
           MOVE "ASSET" TO FX-POSITION OF WS-FX-CA
           MOVE "REALISED" TO CA-FUNCTION OF WS-FX-CA
           CALL "FXCONV" USING WS-FX-CA
           MOVE -41.67 TO TK-EXPECTED-N
           MOVE FX-REALISED OF WS-FX-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "both sterling legs are reported" TO TK-NAME
           IF FX-GBP-AMOUNT OF WS-FX-CA = 1000.00
              AND FX-GBP-AMOUNT-2 OF WS-FX-CA = 958.33
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "the same movement on a payable is a gain" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "EUR" TO FX-CURRENCY OF WS-FX-CA
           MOVE 1.15 TO FX-RATE OF WS-FX-CA
           MOVE 1.20 TO FX-RATE-2 OF WS-FX-CA
           MOVE 1150.00 TO FX-FOREIGN-AMOUNT OF WS-FX-CA
           MOVE "LIABILITY" TO FX-POSITION OF WS-FX-CA
           MOVE "REALISED" TO CA-FUNCTION OF WS-FX-CA
           CALL "FXCONV" USING WS-FX-CA
           MOVE 41.67 TO TK-EXPECTED-N
           MOVE FX-REALISED OF WS-FX-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "no movement realises nothing" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "USD" TO FX-CURRENCY OF WS-FX-CA
           MOVE 1.25 TO FX-RATE OF WS-FX-CA
           MOVE 1.25 TO FX-RATE-2 OF WS-FX-CA
           MOVE 1250.00 TO FX-FOREIGN-AMOUNT OF WS-FX-CA
           MOVE "REALISED" TO CA-FUNCTION OF WS-FX-CA
           CALL "FXCONV" USING WS-FX-CA
           MOVE 0 TO TK-EXPECTED-N
           MOVE FX-REALISED OF WS-FX-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "a settlement rate of zero is rejected" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "USD" TO FX-CURRENCY OF WS-FX-CA
           MOVE 1.25 TO FX-RATE OF WS-FX-CA
           MOVE 0 TO FX-RATE-2 OF WS-FX-CA
           MOVE 1250.00 TO FX-FOREIGN-AMOUNT OF WS-FX-CA
           MOVE "REALISED" TO CA-FUNCTION OF WS-FX-CA
           CALL "FXCONV" USING WS-FX-CA
           IF CA-RETURN-CODE OF WS-FX-CA = 8
              AND CA-REASON-CODE OF WS-FX-CA = "FX0009"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> COM-010 - missing, zero or ambiguous rates block confirmation
      *>----------------------------------------------------------------
       4000-ERROR-TESTS.
           MOVE "a missing rate is not parity (FX0001)" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "USD" TO FX-CURRENCY OF WS-FX-CA
           MOVE 0 TO FX-RATE OF WS-FX-CA
           MOVE 1250.00 TO FX-FOREIGN-AMOUNT OF WS-FX-CA
           PERFORM 8100-CALL-TOGBP
           IF CA-RETURN-CODE OF WS-FX-CA = 8
              AND CA-REASON-CODE OF WS-FX-CA = "FX0001"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-FX-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "the amount is left alone when the rate is missing"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE FX-GBP-AMOUNT OF WS-FX-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "a negative rate is rejected (FX0002)" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "USD" TO FX-CURRENCY OF WS-FX-CA
           MOVE -1.25 TO FX-RATE OF WS-FX-CA
           MOVE 1250.00 TO FX-FOREIGN-AMOUNT OF WS-FX-CA
           PERFORM 8100-CALL-TOGBP
           IF CA-RETURN-CODE OF WS-FX-CA = 8
              AND CA-REASON-CODE OF WS-FX-CA = "FX0002"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "a blank currency is rejected (FX0005)" TO TK-NAME
           PERFORM 8000-RESET
           MOVE SPACES TO FX-CURRENCY OF WS-FX-CA
           MOVE 1.25 TO FX-RATE OF WS-FX-CA
           MOVE 1250.00 TO FX-FOREIGN-AMOUNT OF WS-FX-CA
           PERFORM 8100-CALL-TOGBP
           IF CA-RETURN-CODE OF WS-FX-CA = 8
              AND CA-REASON-CODE OF WS-FX-CA = "FX0005"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "too many decimal places is rejected (FX0006)"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "USD" TO FX-CURRENCY OF WS-FX-CA
           MOVE 1.25 TO FX-RATE OF WS-FX-CA
           MOVE 9 TO FX-PLACES OF WS-FX-CA
           MOVE 1250.00 TO FX-FOREIGN-AMOUNT OF WS-FX-CA
           PERFORM 8100-CALL-TOGBP
           IF CA-RETURN-CODE OF WS-FX-CA = 8
              AND CA-REASON-CODE OF WS-FX-CA = "FX0006"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "an invalid rate date is rejected (FX0007)" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "USD" TO FX-CURRENCY OF WS-FX-CA
           MOVE 20260230 TO FX-RATE-DATE OF WS-FX-CA
           MOVE "GETRATE" TO CA-FUNCTION OF WS-FX-CA
           CALL "FXCONV" USING WS-FX-CA
           IF CA-RETURN-CODE OF WS-FX-CA = 8
              AND CA-REASON-CODE OF WS-FX-CA = "FX0007"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "an unknown function is rejected (FX0010)" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "USD" TO FX-CURRENCY OF WS-FX-CA
           MOVE "REVALUE" TO CA-FUNCTION OF WS-FX-CA
           CALL "FXCONV" USING WS-FX-CA
           IF CA-RETURN-CODE OF WS-FX-CA = 8
              AND CA-REASON-CODE OF WS-FX-CA = "FX0010"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
       8000-RESET.
           INITIALIZE WS-FX-CA
           MOVE "STC" TO CA-COMPANY OF WS-FX-CA.

       8100-CALL-TOGBP.
           MOVE "TOGBP" TO CA-FUNCTION OF WS-FX-CA
           CALL "FXCONV" USING WS-FX-CA.

       COPY TESTKITP.
