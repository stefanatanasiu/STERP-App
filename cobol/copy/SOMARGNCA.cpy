      *>================================================================
      *> SOMARGNCA - COMMAREA for SOMARGN, sales margin and markup
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : SALES ORDER MANAGEMENT
      *> CALLED BY : QUOTATION ENTRY, SALES ORDER ENTRY, SALES
      *>             ANALYSIS, THE .NET CLIENT
      *>
      *> MARGIN IS MEASURED ON THE EX VAT NET SELLING AMOUNT AGAINST AN
      *> EXPLICITLY SELECTED COMPARABLE COST BASIS (SO-006, SO-023):
      *>   margin value = net - cost
      *>   margin pct   = (net - cost) / net  x 100
      *>   markup pct   = (net - cost) / cost x 100
      *> THESE ARE DIFFERENT MEASURES AND BOTH ARE RETURNED.  A ZERO
      *> DENOMINATOR IS "NOT DEFINED" PLUS A REVIEW CONDITION, NEVER
      *> ZERO.  A MISSING COST IS NOT ZERO EITHER: THE ANSWER IS
      *> INCOMPLETE, RETURN CODE 06 AND REASON MRG0002.
      *> BELOW THE POLICY MINIMUM (SO.MARGIN.MIN.PCT, BASELINE 30) THE
      *> LINE NEEDS APPROVAL, AN EXCEPTION IS RAISED AND CA-HOLD IS
      *> RETURNED.  PROPOSE TURNS A TARGET MARGIN INTO A PROPOSED
      *> PRICE AND NEVER CHANGES AN ACCEPTED PRICE - THAT NEEDS A
      *> CONTROLLED AMENDMENT.
      *>
      *> CA-FUNCTION VALUES
      *>   LINE      margin of one line from net, quantity and cost
      *>   CHECK     LINE plus the policy minimum margin test and the
      *>             exception when the line is below it
      *>   PROPOSE   the price that would deliver SM-TARGET-MARGIN-PCT
      *>   EXPLAIN   LINE plus the narrative of the cost basis chosen
      *>
      *> REASON CODES
      *>   MRG0001 company, item or net amount missing
      *>   MRG0002 comparable cost not available - margin incomplete
      *>   MRG0003 net selling amount is zero - margin not defined
      *>   MRG0004 comparable cost is zero - markup not defined
      *>   MRG0005 margin is below the policy minimum - approval needed
      *>   MRG0006 target margin must be below 100 per cent
      *>   MRG0007 an accepted price needs a controlled amendment
      *>   MRG0008 unsupported function
      *>   MRG0009 quantity must be positive
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-11-05 RJH  CR0601 ORDER MARGIN ENQUIRY (SALES COPY)
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT COST EFFECTIVE DATES
      *> 2006-10-16 DJM  CR1455 MARGIN AND MARKUP SHOWN SEPARATELY
      *> 2026-10-06 STERP SO-006, SO-023 MISSING COST IS NOT ZERO
      *>================================================================
      *> REQ: SO-006, SO-023
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SM-BODY.
      *>       ---- what is being measured ----
               10  SM-DOC-TYPE         PIC X(20).
               10  SM-DOC-REF          PIC X(40).
               10  SM-LINE-NUM         PIC S9(9) COMP-5.
               10  SM-ITEM-CODE        PIC X(40).
               10  SM-REVISION         PIC X(4).
               10  SM-CUSTOMER         PIC X(20).
               10  SM-SITE-CODE        PIC X(10).
               10  SM-EFF-DATE         PIC 9(8).
               10  SM-QUANTITY         PIC S9(13)V9(6).
               10  SM-CURRENCY         PIC X(3).
               10  SM-FX-PER-GBP       PIC S9(9)V9(8).
      *>       ---- the revenue side, always ex VAT ----
               10  SM-NET-AMOUNT       PIC S9(13)V9(2).
               10  SM-NET-AMOUNT-GBP   PIC S9(13)V9(2).
               10  SM-UNIT-PRICE       PIC S9(11)V9(6).
               10  SM-PRICE-PER-QTY    PIC S9(9)V9(6).
               10  SM-PRICE-ACCEPTED   PIC X.
                   88  SM-IS-ACCEPTED            VALUE "Y".
      *>       ---- the cost side ----
               10  SM-COST-BASIS       PIC X(12).
                   88  SM-ON-SUPPLIED            VALUE "SUPPLIED".
                   88  SM-ON-STANDARD            VALUE "STANDARD".
                   88  SM-ON-AVERAGE             VALUE "AVERAGE".
                   88  SM-ON-LASTBUY             VALUE "LASTBUY".
                   88  SM-ON-QUOTE               VALUE "QUOTE".
               10  SM-UNIT-COST        PIC S9(11)V9(6).
               10  SM-COST-AMOUNT      PIC S9(13)V9(2).
               10  SM-COST-ADJUSTMENT  PIC S9(13)V9(2).
               10  SM-COST-BASIS-USED  PIC X(12).
               10  SM-COST-SOURCE      PIC X(20).
               10  SM-COST-FOUND       PIC X.
                   88  SM-HAS-COST               VALUE "Y".
      *>       ---- policy ----
               10  SM-MIN-MARGIN-PCT   PIC S9(5)V9(4).
               10  SM-MIN-SOURCE       PIC X(20).
               10  SM-TARGET-MARGIN    PIC S9(5)V9(4).
               10  SM-RAISE-EXCEPTION  PIC X.
                   88  SM-DO-RAISE               VALUE "Y".
      *>       ---- results ----
               10  SM-MARGIN-VALUE     PIC S9(13)V9(2).
               10  SM-MARGIN-PCT       PIC S9(7)V9(4).
               10  SM-MARKUP-PCT       PIC S9(7)V9(4).
               10  SM-MARGIN-STATE     PIC X(12).
                   88  SM-MARGIN-OK              VALUE "OK".
                   88  SM-MARGIN-UNDEF           VALUE "UNDEFINED".
                   88  SM-MARGIN-INCOMP          VALUE "INCOMPLETE".
               10  SM-MARKUP-STATE     PIC X(12).
                   88  SM-MARKUP-OK              VALUE "OK".
                   88  SM-MARKUP-UNDEF           VALUE "UNDEFINED".
                   88  SM-MARKUP-INCOMP          VALUE "INCOMPLETE".
               10  SM-BELOW-MINIMUM    PIC X.
                   88  SM-IS-BELOW               VALUE "Y".
               10  SM-APPROVAL-REQD    PIC X.
                   88  SM-NEEDS-APPROVAL         VALUE "Y".
               10  SM-EXCEPTION-ID     PIC S9(9) COMP-5.
               10  SM-REVIEW-REASON    PIC X(100).
      *>       ---- PROPOSE only - never writes the live price ----
               10  SM-PROPOSED-PRICE   PIC S9(11)V9(6).
               10  SM-PROPOSED-NET     PIC S9(13)V9(2).
               10  SM-PROPOSED-MARGIN  PIC S9(7)V9(4).
               10  SM-PRICE-CHANGED    PIC X.
                   88  SM-PRICE-UNTOUCHED        VALUE "N".
               10  SM-AMENDMENT-REQD   PIC X.
               10  SM-EXPLANATION      PIC X(200).
               10  SM-USED-CST-MARGIN  PIC X.
