      *>================================================================
      *> PRICINGCA - COMMAREA for PRICING, the sales price selection
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : SALES ORDER MANAGEMENT
      *> CALLED BY : SALES ORDER ENTRY, QUOTATION ENTRY, SODISC,
      *>             THE .NET CLIENT PRICE ENQUIRY
      *>
      *> A SELLING PRICE IS CHOSEN BY A DETERMINISTIC PRECEDENCE AND
      *> THE USER MUST ALWAYS BE ABLE TO EXPLAIN IT (SO-004):
      *>   LEVEL 1  APPROVED MANUAL PRICE FOR THIS DOCUMENT VERSION
      *>   LEVEL 2  SIGNED CONTRACT / CUSTOMER ITEM AGREEMENT
      *>   LEVEL 3  CUSTOMER SPECIFIC ITEM PRICE
      *>   LEVEL 4  CUSTOMER PRICE GROUP / ASSIGNED LIST
      *>   LEVEL 5  ITEM STANDARD SELLING LIST
      *> WITHIN A LEVEL THE CURRENCY, THE UNIT, THE EFFECTIVE DATE AND
      *> THE QUANTITY BAND MUST ALL MATCH.  TWO OR MORE EQUALLY RANKED
      *> CANDIDATES THAT DISAGREE ON UNIT PRICE ARE AMBIGUOUS, AND AN
      *> AMBIGUOUS PRICE BLOCKS CONFIRMATION (RETURN CODE 08 PRC0007).
      *> A MISSING CURRENCY IS NOT CONVERTED FROM ANOTHER LIST UNLESS
      *> PARAMETER SO.PRICE.XCURR.ALLOW IS "Y".
      *>
      *> CA-FUNCTION VALUES
      *>   SELECT    choose the price for the company, customer, item,
      *>             unit, currency, quantity and effective date
      *>   EXPLAIN   SELECT and also return the ranked candidate list
      *>             with the reason each row was or was not chosen
      *>   BREAKS    list the quantity breaks held for an item on one
      *>             price list (or on the selected list)
      *>
      *> REASON CODES
      *>   PRC0001 company, customer or item missing
      *>   PRC0002 quantity must be positive
      *>   PRC0003 no effective price for that item and date
      *>   PRC0004 no price in the requested currency
      *>   PRC0005 no price for the requested unit of measure
      *>   PRC0006 quantity is below the lowest quantity break
      *>   PRC0007 equally ranked prices disagree - confirmation blocked
      *>   PRC0008 unsupported function
      *>   PRC0009 manual price claimed without an approval reference
      *>   PRC0010 price list is not active on the price date
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-03-16 RJH  CR0912 SALES ORDER PRICING REWRITE (ACUCOBOL)
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT PRICE EFFECTIVE DATES
      *> 2003-05-09 DJM  CR1077 QUANTITY BREAKS AND PRICE-PER QTY
      *> 2026-10-06 STERP SO-004 DETERMINISTIC PRICE PRECEDENCE
      *>================================================================
      *> REQ: SO-004
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  PR-BODY.
      *>       ---- what is being priced ----
               10  PR-ITEM-CODE        PIC X(40).
               10  PR-REVISION         PIC X(4).
               10  PR-CUSTOMER         PIC X(20).
               10  PR-PRICE-GROUP      PIC X(20).
               10  PR-CONTRACT-REF     PIC X(40).
               10  PR-UOM-CODE         PIC X(10).
               10  PR-CURRENCY         PIC X(3).
               10  PR-QUANTITY         PIC S9(13)V9(6).
               10  PR-ORDER-DATE       PIC 9(8).
               10  PR-PRICE-DATE       PIC 9(8).
               10  PR-DOC-TYPE         PIC X(20).
               10  PR-DOC-REF          PIC X(40).
               10  PR-DOC-VERSION      PIC S9(4) SIGN LEADING SEPARATE.
      *>       ---- level 1, the approved manual price ----
               10  PR-MANUAL-PRICE     PIC S9(11)V9(6).
               10  PR-MANUAL-PER-QTY   PIC S9(9)V9(6).
               10  PR-MANUAL-APPROVAL  PIC X(20).
               10  PR-MANUAL-REASON    PIC X(100).
      *>       ---- selection controls ----
               10  PR-LIST-CODE-IN     PIC X(20).
               10  PR-MAX-LEVEL        PIC S9(4) SIGN LEADING SEPARATE.
               10  PR-XCURR-ALLOW      PIC X.
                   88  PR-XCURR-IS-ON            VALUE "Y".
      *>       ---- the selected price ----
               10  PR-UNIT-PRICE       PIC S9(11)V9(6).
               10  PR-PRICE-PER-QTY    PIC S9(9)V9(6).
               10  PR-DISCOUNT-PCT     PIC S9(5)V9(4).
               10  PR-MIN-MARGIN-PCT   PIC S9(5)V9(4).
               10  PR-PRICE-LEVEL      PIC S9(4) SIGN LEADING SEPARATE.
               10  PR-PRICE-SCOPE      PIC X(12).
                   88  PR-ON-MANUAL              VALUE "MANUAL".
                   88  PR-ON-CONTRACT            VALUE "CONTRACT".
                   88  PR-ON-CUSTOMER            VALUE "CUSTOMER".
                   88  PR-ON-GROUP               VALUE "GROUP".
                   88  PR-ON-STANDARD            VALUE "STANDARD".
               10  PR-PRICE-LIST-CODE  PIC X(20).
               10  PR-PRICE-CURRENCY   PIC X(3).
               10  PR-PRICE-UOM        PIC X(10).
               10  PR-QTY-BREAK        PIC S9(13)V9(6).
               10  PR-VALID-FROM       PIC 9(8).
               10  PR-VALID-TO         PIC 9(8).
               10  PR-PRICE-SOURCE     PIC X(10).
               10  PR-PRICE-FOUND      PIC X.
                   88  PR-WAS-FOUND              VALUE "Y".
               10  PR-AMBIGUOUS        PIC X.
                   88  PR-IS-AMBIGUOUS           VALUE "Y".
               10  PR-BLOCK-CONFIRM    PIC X.
                   88  PR-IS-BLOCKED             VALUE "Y".
               10  PR-RIVAL-LIST-CODE  PIC X(20).
               10  PR-EXPLANATION      PIC X(200).
      *>       ---- the ranked candidates (EXPLAIN) ----
               10  PR-CAND-CNT         PIC 9(3).
               10  PR-CAND-TRUNC       PIC X.
               10  PR-CAND OCCURS 40 TIMES.
                   15  PR-C-LEVEL      PIC 9(1).
                   15  PR-C-SCOPE      PIC X(12).
                   15  PR-C-LIST       PIC X(20).
                   15  PR-C-CURRENCY   PIC X(3).
                   15  PR-C-UOM        PIC X(10).
                   15  PR-C-QTYBRK     PIC S9(13)V9(6).
                   15  PR-C-PRICE      PIC S9(11)V9(6).
                   15  PR-C-PER-QTY    PIC S9(9)V9(6).
                   15  PR-C-DISC       PIC S9(5)V9(4).
                   15  PR-C-MINMGN     PIC S9(5)V9(4).
                   15  PR-C-FROM       PIC 9(8).
                   15  PR-C-TO         PIC 9(8).
                   15  PR-C-RANK       PIC 9(3).
                   15  PR-C-CHOSEN     PIC X.
                   15  PR-C-WHY        PIC X(70).
      *>       ---- the quantity breaks (BREAKS) ----
               10  PR-BREAK-CNT        PIC 9(3).
               10  PR-BREAK OCCURS 20 TIMES.
                   15  PR-B-QTYBRK     PIC S9(13)V9(6).
                   15  PR-B-PRICE      PIC S9(11)V9(6).
                   15  PR-B-PER-QTY    PIC S9(9)V9(6).
                   15  PR-B-DISC       PIC S9(5)V9(4).
                   15  PR-B-CURRENCY   PIC X(3).
                   15  PR-B-UOM        PIC X(10).
                   15  PR-B-LIST       PIC X(20).
                   15  PR-B-FROM       PIC 9(8).
                   15  PR-B-TO         PIC 9(8).
