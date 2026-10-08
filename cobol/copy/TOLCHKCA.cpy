      *>================================================================
      *> TOLCHKCA - COMMAREA for TOLCHK, the purchasing tolerance test.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> D-08 baseline: over delivery 0% by default with an item or
      *> supplier override of up to 5%; price tolerance per line must
      *> pass BOTH 2% and 50.00; charge variance up to 25.00; delivery
      *> more than five working days early is flagged.  An over receipt
      *> beyond the approved tolerance requires a changed order and a
      *> new approval before confirmation (PO-011).
      *>
      *> Outstanding and over delivery follow section 6 of the spec:
      *>   counted      = confirmed receipts less true reversals
      *>   outstanding  = max(0, ordered - cancelled - counted)
      *>   overdelivery = max(0, counted - (ordered - cancelled))
      *> so an accepted tolerance leaves zero outstanding and a
      *> separately visible positive over delivery, never a negative
      *> commitment and never a hidden excess.
      *>
      *> CA-FUNCTION values
      *>   QTY      quantity tolerance on a proposed receipt
      *>   PRICE    price variance of an invoice against the order
      *>   DATE     early / late delivery against the promise
      *>   CHARGE   freight or charge variance against the estimate
      *>
      *> TC-RULE-SOURCE "PASSED" uses the limits supplied by the
      *> caller (unit tests and the receiving screen do this once per
      *> session); "LOOKUP" reads po.ToleranceRule for the company,
      *> supplier and item, most specific scope first.
      *>
      *> CA-RETURN-CODE 00 within tolerance, 04 warn, 06 hold pending
      *>                an approved amendment, 08 rejected.
      *>
      *> REASON CODES
      *>   TOL0001 ordered quantity is zero or negative
      *>   TOL0002 receipt is above the approved over delivery limit
      *>   TOL0003 receipt is exactly at the approved limit
      *>   TOL0004 price variance above 2% and 50.00
      *>   TOL0005 delivery is more than five working days early
      *>   TOL0006 delivery is late against the current promise
      *>   TOL0007 charge variance above 25.00
      *>   TOL0008 unsupported function
      *>   TOL0009 an item override above 5% is not permitted
      *>----------------------------------------------------------------
      *> 1998-06-18 RJH  CR0815 GOODS INWARD TOLERANCES
      *> 2003-09-22 DJM  CR1101 PRICE TOLERANCE NEEDS BOTH TESTS
      *> 2026-10-06 STERP D-08 BASELINE, WORKING DAY EARLY TEST
      *>================================================================
      *> REQ: PO-011, PO-022, D-08
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  TC-BODY.
               10  TC-ORDER-NUMBER     PIC X(30).
               10  TC-LINE-NUM         PIC S9(9) COMP-5.
               10  TC-SUPPLIER         PIC X(20).
               10  TC-ITEM-CODE        PIC X(40).
               10  TC-RULE-SOURCE      PIC X(8).
                   88  TC-RULE-PASSED            VALUE "PASSED".
                   88  TC-RULE-LOOKUP            VALUE "LOOKUP".
      *>     ---- quantity test ----
               10  TC-ORDERED-QTY      PIC S9(13)V9(6).
               10  TC-CANCELLED-QTY    PIC S9(13)V9(6).
               10  TC-COUNTED-QTY      PIC S9(13)V9(6).
               10  TC-PROPOSED-QTY     PIC S9(13)V9(6).
               10  TC-OVER-PCT         PIC S9(5)V9(4).
               10  TC-OVER-QTY-ALLOW   PIC S9(13)V9(6).
               10  TC-UNDER-PCT        PIC S9(5)V9(4).
               10  TC-ACTION-OVER      PIC X(6).
      *>     ---- price test ----
               10  TC-ORDER-PRICE      PIC S9(11)V9(6).
               10  TC-INVOICE-PRICE    PIC S9(11)V9(6).
               10  TC-PRICE-QTY        PIC S9(13)V9(6).
               10  TC-PRICE-PCT-LIM    PIC S9(5)V9(4).
               10  TC-PRICE-ABS-LIM    PIC S9(9)V99.
      *>     ---- date test ----
               10  TC-PROMISED-DATE    PIC 9(8).
               10  TC-ACTUAL-DATE      PIC 9(8).
               10  TC-EARLY-DAY-LIM    PIC S9(4).
               10  TC-LATE-DAY-LIM     PIC S9(4).
               10  TC-CALENDAR-CODE    PIC X(10).
      *>     ---- charge test ----
               10  TC-ESTIMATE-GBP     PIC S9(13)V99.
               10  TC-ACTUAL-GBP       PIC S9(13)V99.
               10  TC-CHARGE-ABS-LIM   PIC S9(9)V99.
      *>     ---- answers ----
               10  TC-PERMITTED-QTY    PIC S9(13)V9(6).
               10  TC-OUTSTANDING-QTY  PIC S9(13)V9(6).
               10  TC-OVER-QTY         PIC S9(13)V9(6).
               10  TC-EXCESS-QTY       PIC S9(13)V9(6).
               10  TC-VARIANCE-AMT     PIC S9(13)V99.
               10  TC-VARIANCE-PCT     PIC S9(7)V9(4).
               10  TC-DAYS-EARLY       PIC S9(5).
               10  TC-DAYS-LATE        PIC S9(5).
               10  TC-WORK-DAYS-EARLY  PIC S9(5).
               10  TC-OUTCOME          PIC X(10).
                   88  TC-WITHIN                 VALUE "WITHIN".
                   88  TC-AT-LIMIT               VALUE "ATLIMIT".
                   88  TC-WARNED                 VALUE "WARN".
                   88  TC-BLOCKED                VALUE "BLOCK".
               10  TC-NEEDS-AMEND      PIC X.
                   88  TC-AMEND-NEEDED           VALUE "Y".
               10  TC-RULE-SCOPE       PIC X(10).
               10  TC-RULE-SCOPE-CODE  PIC X(40).
