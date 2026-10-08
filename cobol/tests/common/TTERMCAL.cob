      *>================================================================
      *> TTERMCAL - unit tests for TERMCALC (payment terms, COM-012)
      *> Worked numbers:
      *>   AC-005  terms "due on day 31 of the following month" applied
      *>           to January invoices give 28 February 2027 and
      *>           29 February 2028
      *>   COM-012 an invoice without agreed terms is due 30 days after
      *>           the later of supply and receipt of the invoice
      *>           (Late Payment of Commercial Debts (Interest) Act)
      *>   D-25    terms longer than the 60 day limit are flagged
      *>   AC-112  a Bacs due date that is not a processing day moves
      *> The tests supply the basis explicitly rather than a terms code
      *> so that they do not depend on seeded core.PaymentTerms rows.
      *>================================================================
      *> REQ: COM-012, COM-009, COM-026   AC: AC-005
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TTERMCAL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       01  WS-OK                       PIC X VALUE "Y".
       COPY TERMCALCCA REPLACING ==DFHCOMMAREA== BY ==WS-TC-CA==.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TTERMCAL" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-DAY-OF-NEXT-MONTH
           PERFORM 2000-DAYS-AND-EOM
           PERFORM 3000-STATUTORY
           PERFORM 4000-DISCOUNT
           PERFORM 5000-LIMITS-AND-BACS
           PERFORM 6000-ERROR-TESTS
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
      *> AC-005 - the day does not exist in February, so the due date
      *> becomes that month's final day: 28 Feb 2027 and 29 Feb 2028
      *>----------------------------------------------------------------
       1000-DAY-OF-NEXT-MONTH.
           MOVE "AC-005 day 31 of next month, Jan 2027 invoice"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "DAYOFNEXTMONTH" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 31 TO TC-DAY-OF-MONTH OF WS-TC-CA
           MOVE 20270115 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           MOVE 20270228 TO TK-EXPECTED-N
           MOVE TC-DUE-DATE OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-005 day 31 of next month, Jan 2028 invoice"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "DAYOFNEXTMONTH" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 31 TO TC-DAY-OF-MONTH OF WS-TC-CA
           MOVE 20280115 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           MOVE 20280229 TO TK-EXPECTED-N
           MOVE TC-DUE-DATE OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "AC-005 the last day of January gives the same answer"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "DAYOFNEXTMONTH" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 31 TO TC-DAY-OF-MONTH OF WS-TC-CA
           MOVE 20270131 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           MOVE 20270228 TO TK-EXPECTED-N
           MOVE TC-DUE-DATE OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "day 15 of next month from 20 Oct 2026 is 15 Nov 2026"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "DAYOFNEXTMONTH" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 15 TO TC-DAY-OF-MONTH OF WS-TC-CA
           MOVE 20261020 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           MOVE 20261115 TO TK-EXPECTED-N
           MOVE TC-DUE-DATE OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "day 31 of next month from December crosses the year"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "DAYOFNEXTMONTH" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 31 TO TC-DAY-OF-MONTH OF WS-TC-CA
           MOVE 20261210 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           MOVE 20270131 TO TK-EXPECTED-N
           MOVE TC-DUE-DATE OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "the resolved terms are described for the document"
               TO TK-NAME
           MOVE "Day 31 of the following month" TO TK-EXPECTED-X
           MOVE TC-TERMS-DESC OF WS-TC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
       2000-DAYS-AND-EOM.
           MOVE "30 days from 5 October 2026 is 4 November 2026"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "INVOICE" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 30 TO TC-DAYS OF WS-TC-CA
           MOVE 20261005 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           MOVE 20261104 TO TK-EXPECTED-N
           MOVE TC-DUE-DATE OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "the term length is reported as 30 days" TO TK-NAME
           MOVE 30 TO TK-EXPECTED-N
           MOVE TC-TERM-DAYS OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "30 days end of month from 5 Oct 2026 is 30 Nov 2026"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "EOM" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 30 TO TC-DAYS OF WS-TC-CA
           MOVE 20261005 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           MOVE 20261130 TO TK-EXPECTED-N
           MOVE TC-DUE-DATE OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "end of month terms describe themselves" TO TK-NAME
           MOVE "30 days end of month" TO TK-EXPECTED-X
           MOVE TC-TERMS-DESC OF WS-TC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "0 days end of month is the month end itself"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "EOM" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 0 TO TC-DAYS OF WS-TC-CA
           MOVE 20280210 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           MOVE 20280229 TO TK-EXPECTED-N
           MOVE TC-DUE-DATE OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "the due date is also returned as dd/mm/yyyy"
               TO TK-NAME
           MOVE "29/02/2028" TO TK-EXPECTED-X
           MOVE TC-DUE-DATE-UK OF WS-TC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> COM-012 - where no terms are agreed, payment is due 30 days
      *> after the later of supply and receipt of the invoice
      *>----------------------------------------------------------------
       3000-STATUTORY.
           MOVE "no terms: 30 days from the invoice date" TO TK-NAME
           PERFORM 8000-RESET
           MOVE 20261005 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           MOVE 20261104 TO TK-EXPECTED-N
           MOVE TC-DUE-DATE OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "no terms: the source is recorded as STATUTORY"
               TO TK-NAME
           MOVE "STATUTORY" TO TK-EXPECTED-X
           MOVE TC-SOURCE OF WS-TC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "no terms: the Act is named in the description"
               TO TK-NAME
           MOVE "30 days - Late Payment Act default" TO TK-EXPECTED-X
           MOVE TC-TERMS-DESC OF WS-TC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR

           MOVE "no terms: a later invoice receipt moves the due date"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE 20261001 TO TC-SUPPLY-DATE OF WS-TC-CA
           MOVE 20261001 TO TC-INVOICE-DATE OF WS-TC-CA
           MOVE 20261012 TO TC-RECEIVED-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           MOVE 20261111 TO TK-EXPECTED-N
           MOVE TC-DUE-DATE OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "no terms: a later supply date moves the due date"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE 20261020 TO TC-SUPPLY-DATE OF WS-TC-CA
           MOVE 20261001 TO TC-INVOICE-DATE OF WS-TC-CA
           MOVE 20261005 TO TC-RECEIVED-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           MOVE 20261119 TO TK-EXPECTED-N
           MOVE TC-DUE-DATE OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "an unknown terms code falls back to the statute"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "NOSUCH" TO TC-TERMS-CODE OF WS-TC-CA
           MOVE 20261005 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           IF TC-DUE-DATE OF WS-TC-CA = 20261104
              AND CA-RETURN-CODE OF WS-TC-CA = 4
              AND TC-SOURCE OF WS-TC-CA = "STATUTORY"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-TC-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
      *> Settlement discount: the deadline and the eligible amount with
      *> its VAT basis (spec 01 section 9)
      *>----------------------------------------------------------------
       4000-DISCOUNT.
           MOVE "2.5 per cent of net 1,000.00 is 25.00" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "INVOICE" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 30 TO TC-DAYS OF WS-TC-CA
           MOVE 10 TO TC-DISCOUNT-DAYS OF WS-TC-CA
           MOVE 2.5 TO TC-DISCOUNT-PCT OF WS-TC-CA
           MOVE 1000.00 TO TC-NET-AMOUNT OF WS-TC-CA
           MOVE 200.00 TO TC-VAT-AMOUNT OF WS-TC-CA
           MOVE "NET" TO TC-DISCOUNT-BASIS OF WS-TC-CA
           MOVE 20261005 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           MOVE 25.00 TO TK-EXPECTED-N
           MOVE TC-DISCOUNT-AMT OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "the discount deadline is 15 October 2026" TO TK-NAME
           MOVE 20261015 TO TK-EXPECTED-N
           MOVE TC-DISCOUNT-DATE OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "2.5 per cent of gross 1,200.00 is 30.00" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "INVOICE" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 30 TO TC-DAYS OF WS-TC-CA
           MOVE 10 TO TC-DISCOUNT-DAYS OF WS-TC-CA
           MOVE 2.5 TO TC-DISCOUNT-PCT OF WS-TC-CA
           MOVE 1000.00 TO TC-NET-AMOUNT OF WS-TC-CA
           MOVE 200.00 TO TC-VAT-AMOUNT OF WS-TC-CA
           MOVE "GROSS" TO TC-DISCOUNT-BASIS OF WS-TC-CA
           MOVE 20261005 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           MOVE 30.00 TO TK-EXPECTED-N
           MOVE TC-DISCOUNT-AMT OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "a discount of 1.5 per cent of 33.33 rounds to 0.50"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "INVOICE" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 30 TO TC-DAYS OF WS-TC-CA
           MOVE 1.5 TO TC-DISCOUNT-PCT OF WS-TC-CA
           MOVE 33.33 TO TC-NET-AMOUNT OF WS-TC-CA
           MOVE 20261005 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           MOVE 0.50 TO TK-EXPECTED-N
           MOVE TC-DISCOUNT-AMT OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "no discount days means no discount deadline"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "INVOICE" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 30 TO TC-DAYS OF WS-TC-CA
           MOVE 20261005 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           MOVE 0 TO TK-EXPECTED-N
           MOVE TC-DISCOUNT-DATE OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
       5000-LIMITS-AND-BACS.
           MOVE "D-25 90 day terms are flagged for review" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "INVOICE" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 90 TO TC-DAYS OF WS-TC-CA
           MOVE 20261005 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           IF TC-LONG-TERM-FLAG OF WS-TC-CA = "Y"
              AND CA-RETURN-CODE OF WS-TC-CA = 4
              AND CA-REASON-CODE OF WS-TC-CA = "TRM0007"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-TC-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "D-25 60 day terms are within the limit" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "INVOICE" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 60 TO TC-DAYS OF WS-TC-CA
           MOVE 20261005 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           IF TC-LONG-TERM-FLAG OF WS-TC-CA = "N"
              AND CA-RETURN-CODE OF WS-TC-CA = 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "the term limit can be tightened to 45 days"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "INVOICE" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 60 TO TC-DAYS OF WS-TC-CA
           MOVE 45 TO TC-MAX-TERM-DAYS OF WS-TC-CA
           MOVE 20261005 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           IF TC-LONG-TERM-FLAG OF WS-TC-CA = "Y"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

      *>   25 December 2026 is a Friday and a bank holiday; the next
      *>   Bacs processing day is Tuesday 29 December (AC-112)
           MOVE "AC-112 a Bacs due date of 25 Dec 2026 moves to the 29"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "INVOICE" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 81 TO TC-DAYS OF WS-TC-CA
           MOVE 20261005 TO TC-INVOICE-DATE OF WS-TC-CA
           MOVE 200 TO TC-MAX-TERM-DAYS OF WS-TC-CA
           MOVE "Y" TO TC-BACS-ADJUST OF WS-TC-CA
           MOVE "NEXT" TO TC-BACS-DIRECTION OF WS-TC-CA
           MOVE "BACS" TO TC-CALENDAR OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           IF TC-DUE-DATE-RAW OF WS-TC-CA = 20261225
              AND TC-DUE-DATE OF WS-TC-CA = 20261229
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-TC-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "the unadjusted due date is kept for the audit trail"
               TO TK-NAME
           MOVE 20261225 TO TK-EXPECTED-N
           MOVE TC-DUE-DATE-RAW OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "a Bacs due date can move backwards instead"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "INVOICE" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 81 TO TC-DAYS OF WS-TC-CA
           MOVE 20261005 TO TC-INVOICE-DATE OF WS-TC-CA
           MOVE 200 TO TC-MAX-TERM-DAYS OF WS-TC-CA
           MOVE "Y" TO TC-BACS-ADJUST OF WS-TC-CA
           MOVE "PREV" TO TC-BACS-DIRECTION OF WS-TC-CA
           MOVE "BACS" TO TC-CALENDAR OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           MOVE 20261224 TO TK-EXPECTED-N
           MOVE TC-DUE-DATE OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           MOVE "without the Bacs option the calendar date stands"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "INVOICE" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 81 TO TC-DAYS OF WS-TC-CA
           MOVE 200 TO TC-MAX-TERM-DAYS OF WS-TC-CA
           MOVE 20261005 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           MOVE 20261225 TO TK-EXPECTED-N
           MOVE TC-DUE-DATE OF WS-TC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
       6000-ERROR-TESTS.
           MOVE "an invalid invoice date is rejected with TRM0003"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "INVOICE" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 30 TO TC-DAYS OF WS-TC-CA
           MOVE 20260230 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           IF CA-RETURN-CODE OF WS-TC-CA = 8
              AND CA-REASON-CODE OF WS-TC-CA = "TRM0003"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "an unsupported basis is rejected with TRM0004"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "QUARTERLY" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 20261005 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           IF CA-RETURN-CODE OF WS-TC-CA = 8
              AND CA-REASON-CODE OF WS-TC-CA = "TRM0004"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "day of month 0 is rejected with TRM0005" TO TK-NAME
           PERFORM 8000-RESET
           MOVE "DAYOFNEXTMONTH" TO TC-BASIS-TYPE OF WS-TC-CA
           MOVE 0 TO TC-DAY-OF-MONTH OF WS-TC-CA
           MOVE 20261005 TO TC-INVOICE-DATE OF WS-TC-CA
           PERFORM 8100-CALL-DUE
           IF CA-RETURN-CODE OF WS-TC-CA = 8
              AND CA-REASON-CODE OF WS-TC-CA = "TRM0005"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "an unknown function is rejected with TRM0009"
               TO TK-NAME
           PERFORM 8000-RESET
           MOVE "AGEING" TO CA-FUNCTION OF WS-TC-CA
           MOVE 20261005 TO TC-INVOICE-DATE OF WS-TC-CA
           CALL "TERMCALC" USING WS-TC-CA
           IF CA-RETURN-CODE OF WS-TC-CA = 8
              AND CA-REASON-CODE OF WS-TC-CA = "TRM0009"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>----------------------------------------------------------------
       8000-RESET.
           INITIALIZE WS-TC-CA
           MOVE "STC" TO CA-COMPANY OF WS-TC-CA.

       8100-CALL-DUE.
           MOVE "DUEDATE" TO CA-FUNCTION OF WS-TC-CA
           CALL "TERMCALC" USING WS-TC-CA.

       COPY TESTKITP.
