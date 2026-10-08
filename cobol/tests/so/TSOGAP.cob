      *>================================================================
      *> PROGRAM-ID : TSOGAP
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT UNIT TESTS
      *> PURPOSE    : SO-023 - SALES ANALYSIS AND PROFITABILITY.  THE
      *>              EXPECTED MARGIN KEEPS THE COST BASIS THE ORDER
      *>              WAS TAKEN ON; THE REALISED MARGIN ADDS THE LATE
      *>              ATTRIBUTABLE COST ADJUSTMENT POSTED AFTER
      *>              INVOICING.  THE TWO ARE DIFFERENT NUMBERS AND
      *>              BOTH ARE REPORTED, SO A LATE COST IS VISIBLE IN
      *>              PROFITABILITY INSTEAD OF DISAPPEARING INTO A
      *>              GROSS SALES TOTAL.  THE FX BASIS TRAVELS WITH
      *>              THE FIGURES: THE DOCUMENT AMOUNT, THE STERLING
      *>              AMOUNT AND THE RATE ARE ALL KEPT.  REPRICING AN
      *>              ACCEPTED LINE PROPOSES A PRICE AND NEVER
      *>              REWRITES THE ACCEPTED ONE (AC-049).
      *>              EVERY CASE IS PURE: THE COMPARABLE COST, THE
      *>              POLICY MINIMUM AND THE EXCEPTION SWITCH ARE ALL
      *>              SUPPLIED IN THE COMMAREA, SO SOMARGN ANSWERS
      *>              WITHOUT READING THE DATABASE AND THE SUITE RUNS
      *>              ANYWHERE.  THE QUOTED, ORDERED, DESPATCHED,
      *>              INVOICED AND CREDITED STAGES THEMSELVES ARE SET
      *>              OPERATIONS AND ARE PROVED IN db\tests\so-gaps.sql
      *> OUTPUT     : TAP - "ok n - text" / "not ok n - text".  Every
      *>              DISPLAY is UPON SYSOUT because -std acu puts the
      *>              console on the curses screen.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2026-10-07 STERP ORIGINAL - SO-023
      *>================================================================
      *> REQ: SO-023   AC: AC-049
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSOGAP.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY SOMARGNCA REPLACING ==DFHCOMMAREA== BY ==WS-MG-CA==.
       01  WS-SAVED.
           05  WS-EXPECTED-PCT         PIC S9(7)V9(4) VALUE 0.
           05  WS-EXPECTED-VALUE       PIC S9(13)V9(2) VALUE 0.
           05  WS-EXPECTED-COST        PIC S9(13)V9(2) VALUE 0.
           05  WS-REALISED-PCT         PIC S9(7)V9(4) VALUE 0.
           05  WS-REALISED-VALUE       PIC S9(13)V9(2) VALUE 0.
           05  WS-REALISED-COST        PIC S9(13)V9(2) VALUE 0.
       PROCEDURE DIVISION.
      *>----------------------------------------------------------------
       0000-MAIN.
           MOVE "TSOGAP sales analysis margins" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-EXPECTED-MARGIN
           PERFORM 2000-REALISED-MARGIN
           PERFORM 3000-BOTH-ARE-REPORTED
           PERFORM 4000-QUOTE-NOT-REWRITTEN
           PERFORM 5000-POLICY-ON-REALISED
           PERFORM 6000-FX-BASIS-TRAVELS
           PERFORM 7000-ACCEPTED-PRICE-STANDS
           PERFORM 8000-MARGIN-NEVER-INVENTED
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
      *> 1000  THE EXPECTED MARGIN.  A LINE OF 100 TAKEN AT 1,000.00
      *>       NET AGAINST THE QUOTED COST OF 7.00 EACH IS 300.00 OF
      *>       MARGIN, 30% ON SALES AND 42.8571% MARK UP.
      *>----------------------------------------------------------------
      *>   REQ: SO-023   AC: AC-049
       1000-EXPECTED-MARGIN.
           PERFORM 9000-SET-UP
           MOVE "EXPLAIN" TO CA-FUNCTION OF WS-MG-CA
           CALL "SOMARGN" USING WS-MG-CA
           MOVE "the expected margin is measured on the order"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-MG-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-MG-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "net 1,000.00 less cost 700.00 is 300.00" TO TK-NAME
           MOVE 300.00 TO TK-EXPECTED-N
           MOVE SM-MARGIN-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the expected margin is 30.0000 per cent" TO TK-NAME
           MOVE 30.0000 TO TK-EXPECTED-N
           MOVE SM-MARGIN-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the mark up on the same line is 42.8571 per cent"
               TO TK-NAME
           MOVE 42.8571 TO TK-EXPECTED-N
           MOVE SM-MARKUP-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the expected margin keeps the quoted cost basis"
               TO TK-NAME
           MOVE "QUOTE" TO TK-EXPECTED-X
           MOVE SM-COST-BASIS-USED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SM-MARGIN-PCT TO WS-EXPECTED-PCT
           MOVE SM-MARGIN-VALUE TO WS-EXPECTED-VALUE
           MOVE SM-COST-AMOUNT TO WS-EXPECTED-COST.

      *>----------------------------------------------------------------
      *> 2000  THE REALISED MARGIN.  AFTER INVOICING, 50.00 OF
      *>       ATTRIBUTABLE COST IS POSTED LATE.  IT BELONGS TO THIS
      *>       SALE, SO THE REALISED COST IS 750.00 AND THE REALISED
      *>       MARGIN IS 250.00, 25% ON SALES AND 33.3333% MARK UP.
      *>----------------------------------------------------------------
      *>   REQ: SO-023   AC: AC-049
       2000-REALISED-MARGIN.
           PERFORM 9000-SET-UP
           MOVE "EXPLAIN" TO CA-FUNCTION OF WS-MG-CA
           MOVE 50.00 TO SM-COST-ADJUSTMENT
           CALL "SOMARGN" USING WS-MG-CA
           MOVE "the late cost joins the cost of this sale"
               TO TK-NAME
           MOVE 750.00 TO TK-EXPECTED-N
           MOVE SM-COST-AMOUNT TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-MG-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the realised margin is 250.00, not 300.00"
               TO TK-NAME
           MOVE 250.00 TO TK-EXPECTED-N
           MOVE SM-MARGIN-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the realised margin is 25.0000 per cent" TO TK-NAME
           MOVE 25.0000 TO TK-EXPECTED-N
           MOVE SM-MARGIN-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the realised mark up is 33.3333 per cent"
               TO TK-NAME
           MOVE 33.3333 TO TK-EXPECTED-N
           MOVE SM-MARKUP-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the sale itself is unchanged by the late cost"
               TO TK-NAME
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE SM-NET-AMOUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SM-MARGIN-PCT TO WS-REALISED-PCT
           MOVE SM-MARGIN-VALUE TO WS-REALISED-VALUE
           MOVE SM-COST-AMOUNT TO WS-REALISED-COST.

      *>----------------------------------------------------------------
      *> 3000  THE TWO MEASURES ARE DIFFERENT AND THE DIFFERENCE IS
      *>       EXACTLY THE LATE COST - IT IS VISIBLE, NOT LOST INSIDE
      *>       A GROSS SALES FIGURE.
      *>----------------------------------------------------------------
      *>   REQ: SO-023   AC: AC-049
       3000-BOTH-ARE-REPORTED.
           MOVE "expected and realised margin are not the same"
               TO TK-NAME
           IF WS-EXPECTED-PCT NOT = WS-REALISED-PCT
               PERFORM TK-PASS
           ELSE
               MOVE "both measures returned the same percentage"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "the difference in margin is the 50.00 late cost"
               TO TK-NAME
           MOVE 50.00 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N =
               WS-EXPECTED-VALUE - WS-REALISED-VALUE
           PERFORM TK-ASSERT-NUM
           MOVE "the late cost is visible on the cost side too"
               TO TK-NAME
           MOVE 50.00 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N =
               WS-REALISED-COST - WS-EXPECTED-COST
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 4000  A LATE COST DOES NOT REACH BACK.  MEASURED AGAIN ON
      *>       THE BASIS THE ORDER WAS TAKEN ON, THE ANSWER IS STILL
      *>       THE ORIGINAL 30 PER CENT.
      *>----------------------------------------------------------------
      *>   REQ: SO-023   AC: AC-049
       4000-QUOTE-NOT-REWRITTEN.
           PERFORM 9000-SET-UP
           MOVE "LINE" TO CA-FUNCTION OF WS-MG-CA
           CALL "SOMARGN" USING WS-MG-CA
           MOVE "the original basis still gives 30.0000 per cent"
               TO TK-NAME
           MOVE 30.0000 TO TK-EXPECTED-N
           MOVE SM-MARGIN-PCT TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-MG-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "and the original cost of 700.00 is untouched"
               TO TK-NAME
           MOVE 700.00 TO TK-EXPECTED-N
           MOVE SM-COST-AMOUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 5000  THE COMMERCIAL ANSWER CAN DIFFER BETWEEN THE TWO
      *>       MEASURES.  AT THE POLICY MINIMUM OF 30 PER CENT THE
      *>       ORDER WAS ACCEPTABLE WHEN IT WAS TAKEN; ONCE THE LATE
      *>       COST IS IN, THE REALISED POSITION NEEDS APPROVAL.
      *>----------------------------------------------------------------
      *>   REQ: SO-023
       5000-POLICY-ON-REALISED.
           PERFORM 9000-SET-UP
           MOVE "CHECK" TO CA-FUNCTION OF WS-MG-CA
           MOVE 30.0000 TO SM-MIN-MARGIN-PCT
           CALL "SOMARGN" USING WS-MG-CA
           MOVE "the expected margin meets the policy minimum"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE SM-BELOW-MINIMUM TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-MG-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           MOVE "so the order needed no margin approval" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-MG-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM

           PERFORM 9000-SET-UP
           MOVE "CHECK" TO CA-FUNCTION OF WS-MG-CA
           MOVE 30.0000 TO SM-MIN-MARGIN-PCT
           MOVE 50.00 TO SM-COST-ADJUSTMENT
           CALL "SOMARGN" USING WS-MG-CA
           MOVE "the realised margin falls below the minimum"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SM-BELOW-MINIMUM TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-MG-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           MOVE "the realised position is held for approval"
               TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-MG-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the policy rule is named" TO TK-NAME
           MOVE "MRG0005" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-MG-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the minimum that was applied is named" TO TK-NAME
           MOVE "PRICELIST" TO TK-EXPECTED-X
           MOVE SM-MIN-SOURCE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 6000  THE FX BASIS TRAVELS WITH THE PROFITABILITY.  THE SAME
      *>       SALE IN EUROS AT 1.16 PER POUND IS 1,160.00 EUR AGAINST
      *>       812.00 EUR OF COST - THE SAME 30 PER CENT - AND THE
      *>       STERLING AMOUNT AND THE RATE ARE BOTH STILL THERE.
      *>----------------------------------------------------------------
      *>   REQ: SO-023
       6000-FX-BASIS-TRAVELS.
           PERFORM 9000-SET-UP
           MOVE "EXPLAIN" TO CA-FUNCTION OF WS-MG-CA
           MOVE "EUR" TO SM-CURRENCY
           MOVE 1.16000000 TO SM-FX-PER-GBP
           MOVE 1160.00 TO SM-NET-AMOUNT
           MOVE 1000.00 TO SM-NET-AMOUNT-GBP
           MOVE 8.120000 TO SM-UNIT-COST
           MOVE 0 TO SM-COST-AMOUNT
           CALL "SOMARGN" USING WS-MG-CA
           MOVE "a euro sale of the same shape is still 30 per cent"
               TO TK-NAME
           MOVE 30.0000 TO TK-EXPECTED-N
           MOVE SM-MARGIN-PCT TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-MG-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the margin value is in the document currency"
               TO TK-NAME
           MOVE 348.00 TO TK-EXPECTED-N
           MOVE SM-MARGIN-VALUE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the sterling amount is kept beside it" TO TK-NAME
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE SM-NET-AMOUNT-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the rate the sale was struck at is kept" TO TK-NAME
           MOVE 1.16000000 TO TK-EXPECTED-N
           MOVE SM-FX-PER-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the explanation names the currency it measured"
               TO TK-NAME
           MOVE "Net EUR" TO TK-EXPECTED-X
           MOVE SM-EXPLANATION(1:7) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 7000  A MASTER PRICE CHANGE AFTER ACCEPTANCE PROPOSES A NEW
      *>       PRICE AND CHANGES NOTHING.  THE ACCEPTED TERMS STAND
      *>       UNTIL A CONTROLLED AMENDMENT IS MADE (AC-049).
      *>----------------------------------------------------------------
      *>   REQ: SO-023   AC: AC-049
       7000-ACCEPTED-PRICE-STANDS.
           PERFORM 9000-SET-UP
           MOVE "PROPOSE" TO CA-FUNCTION OF WS-MG-CA
           MOVE "Y" TO SM-PRICE-ACCEPTED
           MOVE 40.0000 TO SM-TARGET-MARGIN
           CALL "SOMARGN" USING WS-MG-CA
           MOVE "a 40 per cent target would need 1,166.67 of net"
               TO TK-NAME
           MOVE 1166.67 TO TK-EXPECTED-N
           MOVE SM-PROPOSED-NET TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-MG-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the accepted price is not touched" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE SM-PRICE-CHANGED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a controlled amendment is required instead"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE SM-AMENDMENT-REQD TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the amendment rule is named" TO TK-NAME
           MOVE "MRG0007" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-MG-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the margin actually earned is still reported"
               TO TK-NAME
           MOVE 30.0000 TO TK-EXPECTED-N
           MOVE SM-MARGIN-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 8000  PROFITABILITY NEVER INVENTS A NUMBER.  WITH NO
      *>       COMPARABLE COST THE LINE IS INCOMPLETE AND HELD, NOT
      *>       REPORTED AS A HUNDRED PER CENT MARGIN.
      *>----------------------------------------------------------------
      *>   REQ: SO-023
       8000-MARGIN-NEVER-INVENTED.
           PERFORM 9000-SET-UP
           MOVE "LINE" TO CA-FUNCTION OF WS-MG-CA
           MOVE SPACES TO SM-ITEM-CODE
           MOVE 0 TO SM-UNIT-COST SM-COST-AMOUNT
           CALL "SOMARGN" USING WS-MG-CA
           MOVE "a margin with no cost basis is labelled incomplete"
               TO TK-NAME
           MOVE "INCOMPLETE" TO TK-EXPECTED-X
           MOVE SM-MARGIN-STATE TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-MG-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           MOVE "it is never reported as a 100 per cent margin"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SM-MARGIN-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the line is held until a cost basis exists"
               TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-MG-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> 9000  ONE SALES ORDER LINE, MEASURED MANY WAYS.  THE COST IS
      *>       SUPPLIED SO THAT NOTHING HERE DEPENDS ON THE DATABASE.
      *>----------------------------------------------------------------
       9000-SET-UP.
           INITIALIZE WS-MG-CA
           MOVE "STC" TO CA-COMPANY OF WS-MG-CA
           MOVE "RDG" TO CA-SITE OF WS-MG-CA
           MOVE "SALESMGR" TO CA-USER OF WS-MG-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-MG-CA
           MOVE "ZZSOGAP-MARGIN-001"
               TO CA-CORRELATION-ID OF WS-MG-CA
           MOVE "SALESORDER" TO SM-DOC-TYPE
           MOVE "ZZSOGAP-SO-1" TO SM-DOC-REF
           MOVE 1 TO SM-LINE-NUM
           MOVE "TYRE-700X32C" TO SM-ITEM-CODE
           MOVE "C0001" TO SM-CUSTOMER
           MOVE "RDG" TO SM-SITE-CODE
           MOVE 20261006 TO SM-EFF-DATE
           MOVE 100 TO SM-QUANTITY
           MOVE "GBP" TO SM-CURRENCY
           MOVE 1 TO SM-FX-PER-GBP
           MOVE 1000.00 TO SM-NET-AMOUNT
           MOVE 1000.00 TO SM-NET-AMOUNT-GBP
           MOVE 10.000000 TO SM-UNIT-PRICE
           MOVE 1 TO SM-PRICE-PER-QTY
           MOVE "N" TO SM-PRICE-ACCEPTED
           MOVE "QUOTE" TO SM-COST-BASIS
           MOVE 7.000000 TO SM-UNIT-COST
           MOVE 0 TO SM-COST-AMOUNT SM-COST-ADJUSTMENT
           MOVE 0 TO SM-MIN-MARGIN-PCT SM-TARGET-MARGIN
      *>   "N" keeps the exception service, and the database it
      *>   writes to, out of these pure cases
           MOVE "N" TO SM-RAISE-EXCEPTION.

       COPY TESTKITP.
