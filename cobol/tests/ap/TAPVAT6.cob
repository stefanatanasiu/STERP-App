      *>================================================================
      *> PROGRAM-ID : TAPVAT6
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE LEDGER (AP) UNIT TESTS
      *> PURPOSE    : AP6MDATE - THE SIX MONTH UNPAID INPUT VAT RULE.
      *>              THE WORKED NUMBERS ARE THOSE OF ACCEPTANCE
      *>              SCENARIO AC-126: A VALID INVOICE OF 5,000.00
      *>              PLUS 1,000.00 VAT DUE 31 JANUARY 2027, STILL
      *>              UNPAID AT 31 JULY 2027 AND PAID IN SEPTEMBER
      *>              2027.  THE INPUT VAT IS REPAID ON THE RETURN
      *>              COVERING 31 JULY 2027 AND RECLAIMED ONCE WHEN
      *>              IT IS PAID.
      *>              AP6MDATE IS PURE ARITHMETIC AND TOUCHES NO
      *>              DATABASE, SO THIS SUITE RUNS WITH THE SERVER
      *>              DOWN.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: AP-029, AP-025   AC: AC-126
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAPVAT6.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY AP6MDTCA REPLACING ==DFHCOMMAREA== BY ==WS-6M-CA==.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TAPVAT6 six month input VAT" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-AC126-TRIGGER-DATE
           PERFORM 1100-ONE-DAY-EARLY
           PERFORM 1200-LATER-OF-THE-TWO
           PERFORM 1300-MONTH-END-CLAMP
           PERFORM 1400-LEAP-YEAR-CLAMP
           PERFORM 1500-THIRTY-DAY-MONTH
           PERFORM 1600-AC126-FULL-REPAYMENT
           PERFORM 1700-PART-PAID-PROPORTION
           PERFORM 1800-HALF-PENNY-ROUNDS-UP
           PERFORM 1900-NEVER-REPAID-TWICE
           PERFORM 2000-AC126-RECLAIM-ON-PAYMENT
           PERFORM 2100-PART-PAID-RECLAIM
           PERFORM 2200-NOTHING-TO-RECLAIM
           PERFORM 2300-PERIOD-KEYS
           PERFORM 2400-NO-DATES-AT-ALL
           PERFORM TK-END
           STOP RUN.

      *>   the AC-126 invoice: supply 20 December 2026, 30 day terms
      *>   from the January statement, due 31 January 2027
       0200-BUILD-AC126.
           INITIALIZE WS-6M-CA
           MOVE 20261220 TO SM-SUPPLY-DATE
           MOVE 20270131 TO SM-DUE-DATE
           MOVE 6 TO SM-MONTHS
           MOVE 1000.00 TO SM-CLAIMED-VAT
           MOVE 6000.00 TO SM-GROSS
           MOVE 6000.00 TO SM-RESIDUAL
           MOVE 0 TO SM-ALREADY-REPAID.

      *>   AC-126 - six months after the later of supply and due date
       1000-AC126-TRIGGER-DATE.
           PERFORM 0200-BUILD-AC126
           MOVE 20270731 TO SM-AS-AT
           MOVE "TRIGGER" TO SM-FUNC
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE "the later date is the 31 January due date" TO TK-NAME
           MOVE 20270131 TO TK-EXPECTED-N
           MOVE SM-LATER-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "six months later is 31 July 2027" TO TK-NAME
           MOVE 20270731 TO TK-EXPECTED-N
           MOVE SM-TRIGGER-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the invoice is repayable on that very day" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SM-DUE-FOR-REPAY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "it is nought days past the six month date" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SM-DAYS-OVER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the return covering 31 July 2027 is 2027Q3"
               TO TK-NAME
           MOVE "2027Q3" TO TK-EXPECTED-X
           MOVE SM-PERIOD-KEY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   the day before, nothing is due back
       1100-ONE-DAY-EARLY.
           PERFORM 0200-BUILD-AC126
           MOVE 20270730 TO SM-AS-AT
           MOVE "REPAY" TO SM-FUNC
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE "30 July 2027 is one day too early" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE SM-DUE-FOR-REPAY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "nothing is repaid before the six month date"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SM-REPAY-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the shortfall is one day" TO TK-NAME
           MOVE -1 TO TK-EXPECTED-N
           MOVE SM-DAYS-OVER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AP-029 - a tax point after the due date moves the clock
       1200-LATER-OF-THE-TWO.
           PERFORM 0200-BUILD-AC126
           MOVE 20270228 TO SM-SUPPLY-DATE
           MOVE 20270131 TO SM-DUE-DATE
           MOVE 20270828 TO SM-AS-AT
           MOVE "TRIGGER" TO SM-FUNC
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE "the later of the two dates is the supply date"
               TO TK-NAME
           MOVE 20270228 TO TK-EXPECTED-N
           MOVE SM-LATER-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the clock runs from the supply date" TO TK-NAME
           MOVE 20270828 TO TK-EXPECTED-N
           MOVE SM-TRIGGER-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "a due date with no supply date still works"
               TO TK-NAME
           MOVE 0 TO SM-SUPPLY-DATE
           MOVE 20270131 TO SM-DUE-DATE
           MOVE "TRIGGER" TO SM-FUNC
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE 20270731 TO TK-EXPECTED-N
           MOVE SM-TRIGGER-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   six months is a calendar shift, not 180 days
       1300-MONTH-END-CLAMP.
           PERFORM 0200-BUILD-AC126
           MOVE 0 TO SM-SUPPLY-DATE
           MOVE 20260831 TO SM-DUE-DATE
           MOVE "TRIGGER" TO SM-FUNC
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE "31 August 2026 becomes 28 February 2027" TO TK-NAME
           MOVE 20270228 TO TK-EXPECTED-N
           MOVE SM-TRIGGER-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       1400-LEAP-YEAR-CLAMP.
           PERFORM 0200-BUILD-AC126
           MOVE 0 TO SM-SUPPLY-DATE
           MOVE 20230831 TO SM-DUE-DATE
           MOVE "TRIGGER" TO SM-FUNC
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE "31 August 2023 becomes 29 February 2024" TO TK-NAME
           MOVE 20240229 TO TK-EXPECTED-N
           MOVE SM-TRIGGER-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the year rolls over with the months" TO TK-NAME
           MOVE 0 TO SM-SUPPLY-DATE
           MOVE 20261130 TO SM-DUE-DATE
           MOVE "TRIGGER" TO SM-FUNC
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE 20270530 TO TK-EXPECTED-N
           MOVE SM-TRIGGER-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       1500-THIRTY-DAY-MONTH.
           PERFORM 0200-BUILD-AC126
           MOVE 0 TO SM-SUPPLY-DATE
           MOVE 20260531 TO SM-DUE-DATE
           MOVE "TRIGGER" TO SM-FUNC
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE "31 May 2026 becomes 30 November 2026" TO TK-NAME
           MOVE 20261130 TO TK-EXPECTED-N
           MOVE SM-TRIGGER-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AC-126 - the whole 1,000.00 of input VAT goes back
       1600-AC126-FULL-REPAYMENT.
           PERFORM 0200-BUILD-AC126
           MOVE 20270731 TO SM-AS-AT
           MOVE "REPAY" TO SM-FUNC
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE "the whole input VAT claim is repaid" TO TK-NAME
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE SM-REPAY-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the invoice is 100 per cent unpaid" TO TK-NAME
           MOVE 100 TO TK-EXPECTED-N
           MOVE SM-UNPAID-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the adjustment is clean" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SM-RC TO TK-ACTUAL-N
           MOVE SM-MESSAGE TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

      *>   AP-029 - only the unpaid proportion is repaid
       1700-PART-PAID-PROPORTION.
           PERFORM 0200-BUILD-AC126
           MOVE 20270731 TO SM-AS-AT
           MOVE 1500.00 TO SM-RESIDUAL
           MOVE "REPAY" TO SM-FUNC
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE "a quarter outstanding repays a quarter of the VAT"
               TO TK-NAME
           MOVE 250.00 TO TK-EXPECTED-N
           MOVE SM-REPAY-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the unpaid proportion is 25 per cent" TO TK-NAME
           MOVE 25 TO TK-EXPECTED-N
           MOVE SM-UNPAID-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   D-02 - half a penny rounds up
       1800-HALF-PENNY-ROUNDS-UP.
           PERFORM 0200-BUILD-AC126
           MOVE 20270731 TO SM-AS-AT
           MOVE 100.01 TO SM-CLAIMED-VAT
           MOVE 300.00 TO SM-GROSS
           MOVE 100.00 TO SM-RESIDUAL
           MOVE "REPAY" TO SM-FUNC
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE "33.336666 rounds up to 33.34" TO TK-NAME
           MOVE 33.34 TO TK-EXPECTED-N
           MOVE SM-REPAY-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AP-029 - the adjustment is made exactly once
       1900-NEVER-REPAID-TWICE.
           PERFORM 0200-BUILD-AC126
           MOVE 20271031 TO SM-AS-AT
           MOVE 1000.00 TO SM-ALREADY-REPAID
           MOVE "REPAY" TO SM-FUNC
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE "an invoice already repaid is not repaid again"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SM-REPAY-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the second pass is a warning, not an error"
               TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE SM-RC TO TK-ACTUAL-N
           MOVE SM-MESSAGE TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "and it is no longer flagged as repayable" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE SM-DUE-FOR-REPAY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AC-126 - paid in September 2027, reclaimed once
       2000-AC126-RECLAIM-ON-PAYMENT.
           PERFORM 0200-BUILD-AC126
           MOVE 0 TO SM-RESIDUAL
           MOVE 1000.00 TO SM-ALREADY-REPAID
           MOVE 20270915 TO SM-PAID-DATE
           MOVE "RECLAIM" TO SM-FUNC
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE "the whole repayment is reclaimed on payment"
               TO TK-NAME
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE SM-RECLAIM-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the reclaim falls in the 2027Q3 return" TO TK-NAME
           MOVE "2027Q3" TO TK-EXPECTED-X
           MOVE SM-PERIOD-KEY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the reclaim is clean" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SM-RC TO TK-ACTUAL-N
           MOVE SM-MESSAGE TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

       2100-PART-PAID-RECLAIM.
           PERFORM 0200-BUILD-AC126
           MOVE 1500.00 TO SM-RESIDUAL
           MOVE 250.00 TO SM-ALREADY-REPAID
           MOVE 20270915 TO SM-PAID-DATE
           MOVE "RECLAIM" TO SM-FUNC
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE "a part payment reclaims the paid proportion"
               TO TK-NAME
           MOVE 187.50 TO TK-EXPECTED-N
           MOVE SM-RECLAIM-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       2200-NOTHING-TO-RECLAIM.
           PERFORM 0200-BUILD-AC126
           MOVE 0 TO SM-RESIDUAL
           MOVE 0 TO SM-ALREADY-REPAID
           MOVE 20270915 TO SM-PAID-DATE
           MOVE "RECLAIM" TO SM-FUNC
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE "an invoice never repaid reclaims nothing" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SM-RECLAIM-VAT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "and says so with a warning" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE SM-RC TO TK-ACTUAL-N
           MOVE SM-MESSAGE TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

       2300-PERIOD-KEYS.
           INITIALIZE WS-6M-CA
           MOVE "PERIOD" TO SM-FUNC
           MOVE 20270131 TO SM-PERIOD-DATE
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE "January 2027 is the first quarter" TO TK-NAME
           MOVE "2027Q1" TO TK-EXPECTED-X
           MOVE SM-PERIOD-KEY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PERIOD" TO SM-FUNC
           MOVE 20271231 TO SM-PERIOD-DATE
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE "December 2027 is the fourth quarter" TO TK-NAME
           MOVE "2027Q4" TO TK-EXPECTED-X
           MOVE SM-PERIOD-KEY TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       2400-NO-DATES-AT-ALL.
           INITIALIZE WS-6M-CA
           MOVE "TRIGGER" TO SM-FUNC
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE "an invoice with no dates is rejected" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE SM-RC TO TK-ACTUAL-N
           MOVE SM-MESSAGE TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "an unknown function is rejected" TO TK-NAME
           MOVE "WIBBLE" TO SM-FUNC
           CALL "AP6MDATE" USING WS-6M-CA
           MOVE 8 TO TK-EXPECTED-N
           MOVE SM-RC TO TK-ACTUAL-N
           MOVE SM-MESSAGE TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

       COPY TESTKITP.
