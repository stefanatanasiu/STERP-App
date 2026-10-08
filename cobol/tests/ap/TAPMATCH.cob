      *>================================================================
      *> PROGRAM-ID : TAPMATCH
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (AP) UNIT TESTS
      *> PURPOSE    : APMATCH FUNCTION TOLCHK - THE FOUR SEPARATE
      *>              TOLERANCES OF DECISION D-08 (QUANTITY, PRICE,
      *>              CHARGE AND VAT), THE AVAILABLE RECEIPT QUANTITY
      *>              AND THE ZERO REFERENCE RULE.  TOLCHK IS PURE
      *>              ARITHMETIC AND TOUCHES NO DATABASE, SO THE WHOLE
      *>              SUITE RUNS WITH THE SERVER DOWN PROVIDED EVERY
      *>              TOLERANCE IS SUPPLIED BY THE CALLER.
      *>              THE WORKED NUMBERS ARE THOSE OF ACCEPTANCE
      *>              SCENARIO AC-030 (PO 100 AT 8.00, ACCEPTED 80,
      *>              INVOICE 90 AT 8.40, CORRECTED 80 AT 8.20).
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: AP-005, AP-006, AP-007   AC: AC-030
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAPMATCH.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY APMATCHCA REPLACING ==DFHCOMMAREA== BY ==WS-MATCH-CA==.
       01  WS-WORK.
           05  WS-AP-PAYABLE           PIC S9(13)V99 VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TAPMATCH match tolerances" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-TEN-UNITS-CANNOT-MATCH
           PERFORM 1100-CORRECTED-INVOICE
           PERFORM 1200-PERCENTAGE-RULE-BITES
           PERFORM 1300-QUARANTINE-NOT-AVAILABLE
           PERFORM 1400-CHARGE-TOLERANCE
           PERFORM 1500-VAT-TOLERANCE
           PERFORM 1600-ZERO-REFERENCE
           PERFORM 1700-UNIT-OF-MEASURE
           PERFORM 1800-PRICE-PER-HUNDRED
           PERFORM 1900-SERVICE-BY-VALUE
           PERFORM TK-END
           STOP RUN.

      *>   AC-030.  Purchase order 100 at 8.00 a unit, 80 accepted on
      *>   the goods receipt, test tolerances of 5 per cent and 0.50
      *>   a unit on price, 25.00 on carriage and 0.05 on VAT.
      *>   Every tolerance is stated so that no parameter is read.
       0200-BUILD-BASE.
           INITIALIZE WS-MATCH-CA
           MOVE "STC" TO CA-COMPANY OF WS-MATCH-CA
           MOVE "RDG" TO CA-SITE OF WS-MATCH-CA
           MOVE "APCLERK" TO CA-USER OF WS-MATCH-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-MATCH-CA
           MOVE "ZZAPTEST-TOLCHK" TO CA-CORRELATION-ID
               OF WS-MATCH-CA
           MOVE "S0001" TO MT-SUPPLIER
           MOVE "ZZAPTEST-R1" TO MT-REGISTER-NUMBER
           MOVE "PO-100441" TO MT-ORDER-NUMBER
           MOVE 1 TO MT-ORDER-LINE
           MOVE 1 TO MT-LINE-NUM
           MOVE "QUANTITY" TO MT-BASIS
           MOVE 1 TO MT-INV-CONV
           MOVE 1 TO MT-ORD-CONV
           MOVE 1 TO MT-INV-PRICE-PER
           MOVE 1 TO MT-ORD-PRICE-PER
           MOVE 100 TO MT-ORD-QTY
           MOVE 8.00 TO MT-ORD-PRICE
           MOVE 80 TO MT-RCV-ACCEPTED
           MOVE 2.0000 TO MT-TOL-QTY-PCT
           MOVE 5.0000 TO MT-TOL-PRICE-PCT
           MOVE 0.50 TO MT-TOL-PRICE-UNIT
           MOVE 50.00 TO MT-TOL-PRICE-GBP
           MOVE 25.00 TO MT-TOL-CHARGE-GBP
           MOVE 0.05 TO MT-TOL-VAT-GBP
           MOVE 1.00 TO MT-TOL-ROUND-GBP
           MOVE "TOLCHK" TO CA-FUNCTION OF WS-MATCH-CA.

      *>   AC-030 - ten invoiced units have no accepted receipt.  The
      *>   price variance is inside every price rule, and that does
      *>   not cure the missing quantity (AP-007).
       1000-TEN-UNITS-CANNOT-MATCH.
           PERFORM 0200-BUILD-BASE
           MOVE 90 TO MT-INV-QTY
           MOVE 8.40 TO MT-INV-PRICE
           MOVE 756.00 TO MT-INV-NET
           CALL "APMATCH" USING WS-MATCH-CA
           MOVE "80 accepted units are available" TO TK-NAME
           MOVE 80 TO TK-EXPECTED-N
           MOVE MT-AVAILABLE-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "ten invoiced units cannot be matched" TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE MT-UNMATCHED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the quantity test fails" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE MT-QTY-OK TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a 5 per cent price variance still passes" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE MT-PRICE-OK TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "price inside tolerance does not cure quantity"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE MT-WITHIN-TOLERANCE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the line is returned for approval" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE MT-NEEDS-APPROVAL TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the refusal names the receipt shortfall" TO TK-NAME
           MOVE "APM0010" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-MATCH-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-MATCH-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           MOVE "a variance is reported as return code 6" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-MATCH-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AC-030 - the corrected invoice of 80 at 8.20 matches.  Net
      *>   656.00 clears goods received not invoiced of 640.00 with
      *>   16.00 attributed difference; input VAT at 20 per cent is
      *>   131.20 and the payable is 787.20.
       1100-CORRECTED-INVOICE.
           PERFORM 0200-BUILD-BASE
           MOVE 80 TO MT-INV-QTY
           MOVE 8.20 TO MT-INV-PRICE
           MOVE 656.00 TO MT-INV-NET
           MOVE 20.0000 TO MT-INV-VATRATE
           MOVE 131.20 TO MT-INV-VAT
           CALL "APMATCH" USING WS-MATCH-CA
           MOVE "the corrected invoice matches" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-MATCH-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-MATCH-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "all eighty units are matched" TO TK-NAME
           MOVE 80 TO TK-EXPECTED-N
           MOVE MT-MATCHED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "matched invoice value is 656.00" TO TK-NAME
           MOVE 656.00 TO TK-EXPECTED-N
           MOVE MT-MATCHED-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "GRNI cleared at order price is 640.00" TO TK-NAME
           MOVE 640.00 TO TK-EXPECTED-N
           MOVE MT-GRNI-CLEARED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "16.00 is attributed as price difference" TO TK-NAME
           MOVE 16.00 TO TK-EXPECTED-N
           MOVE MT-PRICE-VAR-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the price variance is 2.5 per cent" TO TK-NAME
           MOVE 2.5000 TO TK-EXPECTED-N
           MOVE MT-PRICE-VAR-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "stated input VAT of 131.20 is accepted" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE MT-VAT-OK TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           COMPUTE WS-AP-PAYABLE =
               MT-MATCHED-VALUE + MT-INV-VAT
           MOVE "the amount payable is 787.20" TO TK-NAME
           MOVE 787.20 TO TK-EXPECTED-N
           MOVE WS-AP-PAYABLE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the line is matched" TO TK-NAME
           MOVE "MATCHED" TO TK-EXPECTED-X
           MOVE MT-MATCH-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AC-030 - 8.4008 against 8.00 is 5.01 per cent.  The unit
      *>   and sterling rules both pass, the percentage rule does not,
      *>   and any one failure is enough (D-08).
       1200-PERCENTAGE-RULE-BITES.
           PERFORM 0200-BUILD-BASE
           MOVE 80 TO MT-INV-QTY
           MOVE 8.400800 TO MT-INV-PRICE
           MOVE 672.06 TO MT-INV-NET
           CALL "APMATCH" USING WS-MATCH-CA
           MOVE "the variance is 5.01 per cent" TO TK-NAME
           MOVE 5.0100 TO TK-EXPECTED-N
           MOVE MT-PRICE-VAR-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the quantity test passes" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE MT-QTY-OK TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "5.01 per cent fails the percentage rule" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE MT-PRICE-OK TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the price variance is quoted" TO TK-NAME
           MOVE "APM0011" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-MATCH-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-MATCH-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   AP-006 - only commercially accepted goods are matchable.
      *>   Quarantined stock has not been accepted and returned stock
      *>   has gone back, so neither is available; what has already
      *>   been matched is not available a second time.
       1300-QUARANTINE-NOT-AVAILABLE.
           PERFORM 0200-BUILD-BASE
           MOVE 90 TO MT-INV-QTY
           MOVE 8.00 TO MT-INV-PRICE
           MOVE 720.00 TO MT-INV-NET
           MOVE 15 TO MT-RCV-QUARANTINE
           MOVE 5 TO MT-RCV-RETURNED
           MOVE 10 TO MT-PREV-MATCHED
           CALL "APMATCH" USING WS-MATCH-CA
           MOVE "quarantine and returns are not available" TO TK-NAME
           MOVE 65 TO TK-EXPECTED-N
           MOVE MT-AVAILABLE-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "only the available quantity is matched" TO TK-NAME
           MOVE 65 TO TK-EXPECTED-N
           MOVE MT-MATCHED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "twenty five units remain unmatched" TO TK-NAME
           MOVE 25 TO TK-EXPECTED-N
           MOVE MT-UNMATCHED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the line cannot be matched in full" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE MT-WITHIN-TOLERANCE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AP-007, D-08 - carriage has its own sterling tolerance and
      *>   is never netted against the price variance.
       1400-CHARGE-TOLERANCE.
           PERFORM 0200-BUILD-BASE
           MOVE 80 TO MT-INV-QTY
           MOVE 8.00 TO MT-INV-PRICE
           MOVE 640.00 TO MT-INV-NET
           MOVE 60.00 TO MT-INV-CHARGE
           MOVE 25.00 TO MT-ORD-CHARGE
           CALL "APMATCH" USING WS-MATCH-CA
           MOVE "the carriage variance is 35.00" TO TK-NAME
           MOVE 35.00 TO TK-EXPECTED-N
           MOVE MT-CHARGE-VAR-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "quantity and price are still in order" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE MT-PRICE-OK TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the carriage tolerance of 25.00 is breached"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE MT-CHARGE-OK TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the carriage variance is quoted" TO TK-NAME
           MOVE "APM0012" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-MATCH-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-MATCH-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   AP-007, D-08 - VAT is checked against the net plus charge
      *>   at the stated rate, with its own narrow tolerance.
       1500-VAT-TOLERANCE.
           PERFORM 0200-BUILD-BASE
           MOVE 80 TO MT-INV-QTY
           MOVE 8.00 TO MT-INV-PRICE
           MOVE 640.00 TO MT-INV-NET
           MOVE 20.0000 TO MT-INV-VATRATE
           MOVE 131.95 TO MT-INV-VAT
           CALL "APMATCH" USING WS-MATCH-CA
           MOVE "VAT is 3.95 more than the rate gives" TO TK-NAME
           MOVE 3.95 TO TK-EXPECTED-N
           MOVE MT-VAT-VAR-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the VAT tolerance of 0.05 is breached" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE MT-VAT-OK TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the VAT difference is quoted" TO TK-NAME
           MOVE "APM0013" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-MATCH-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-MATCH-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   AP-007 - no order price to compare with is a failure to
      *>   match, not a match within tolerance.  The percentage test
      *>   is meaningless and the sterling rule decides.
       1600-ZERO-REFERENCE.
           PERFORM 0200-BUILD-BASE
           MOVE 80 TO MT-INV-QTY
           MOVE 8.40 TO MT-INV-PRICE
           MOVE 672.00 TO MT-INV-NET
           MOVE 0 TO MT-ORD-PRICE
           CALL "APMATCH" USING WS-MATCH-CA
           MOVE "the missing order price is flagged" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE MT-ZERO-REFERENCE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "no percentage is invented" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE MT-PRICE-VAR-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "a zero reference is not within tolerance" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE MT-WITHIN-TOLERANCE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the reason is explained to the clerk" TO TK-NAME
           IF MT-TOLERANCE-NOTE NOT = SPACES
               MOVE MT-TOLERANCE-NOTE TO TK-DETAIL
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>   AP-006 - the invoice is in boxes of twelve and the order
      *>   and receipt are in each; quantities are converted before
      *>   they are compared.
       1700-UNIT-OF-MEASURE.
           PERFORM 0200-BUILD-BASE
           MOVE 5 TO MT-INV-QTY
           MOVE "BOX" TO MT-INV-UOM
           MOVE 12 TO MT-INV-CONV
           MOVE 8.00 TO MT-INV-PRICE
           MOVE 480.00 TO MT-INV-NET
           MOVE 60 TO MT-RCV-ACCEPTED
           CALL "APMATCH" USING WS-MATCH-CA
           MOVE "five boxes of twelve are sixty units" TO TK-NAME
           MOVE 60 TO TK-EXPECTED-N
           MOVE MT-INV-STOCK-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "sixty converted units match" TO TK-NAME
           MOVE 60 TO TK-EXPECTED-N
           MOVE MT-MATCHED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the converted line is within tolerance" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE MT-WITHIN-TOLERANCE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AP-006 - a price stated per hundred is brought to the
      *>   order basis before the comparison; the difference of 0.40
      *>   a unit is a price variance, not an error.
       1800-PRICE-PER-HUNDRED.
           PERFORM 0200-BUILD-BASE
           MOVE 80 TO MT-INV-QTY
           MOVE 840.00 TO MT-INV-PRICE
           MOVE 100 TO MT-INV-PRICE-PER
           MOVE 672.00 TO MT-INV-NET
           CALL "APMATCH" USING WS-MATCH-CA
           MOVE "840.00 per hundred is 8.40 each" TO TK-NAME
           MOVE 0.40 TO TK-EXPECTED-N
           MOVE MT-PRICE-VAR-UNIT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the matched value is 672.00" TO TK-NAME
           MOVE 672.00 TO TK-EXPECTED-N
           MOVE MT-MATCHED-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the differing price basis is not an error"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE MT-PRICE-OK TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AP-006 - a service has no goods receipt, so the match is
      *>   made on value against the agreed contract amount.
       1900-SERVICE-BY-VALUE.
           PERFORM 0200-BUILD-BASE
           MOVE "VALUE" TO MT-BASIS
           MOVE "SRV-2026-0188" TO MT-SERVICE-REF
           MOVE 1 TO MT-INV-QTY
           MOVE 1500.00 TO MT-INV-PRICE
           MOVE 1500.00 TO MT-INV-NET
           MOVE 1500.00 TO MT-ORD-PRICE
           MOVE 0 TO MT-RCV-ACCEPTED
           CALL "APMATCH" USING WS-MATCH-CA
           MOVE "a service needs no goods receipt" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE MT-QTY-OK TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the agreed service value matches" TO TK-NAME
           MOVE 1500.00 TO TK-EXPECTED-N
           MOVE MT-MATCHED-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the service line is matched" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-MATCH-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-MATCH-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

       COPY TESTKITP.
