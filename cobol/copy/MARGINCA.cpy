      *>================================================================
      *> MARGINCA  - COMMAREA FOR MARGIN, SALES MARGIN AND MARKUP
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : COSTING AND PROFITABILITY
      *> CALLED BY : SALES ORDER ENTRY AND INVOICING, THE .NET CLIENT,
      *>             THE JOB PROFITABILITY REPORT
      *>
      *> REVENUE IS ALWAYS THE NET, EX VAT, POST DISCOUNT AMOUNT IN
      *> STERLING.  VAT IS NEVER PART OF MARGIN (CST-013).
      *>
      *>   margin value = net revenue - cost
      *>   margin pct   = margin value / net revenue x 100
      *>   markup pct   = margin value / cost        x 100
      *>
      *> A ZERO REVENUE MAKES THE MARGIN PERCENTAGE UNDEFINED AND A
      *> ZERO COST MAKES THE MARKUP UNDEFINED.  NEITHER IS REPORTED AS
      *> ZERO OR ONE HUNDRED.  MG-MARGIN-STATE AND MG-MARKUP-STATE SAY
      *> "OK", "UNDEFINED" OR "INCOMPLETE" AND THE CALLER MUST SHOW IT.
      *>
      *> EXPECTED MARGIN USES A CHOSEN ESTIMATE BASIS; REALISED MARGIN
      *> USES THE ATTRIBUTABLE ACTUAL OR VALUED COST OF SALES.  THE TWO
      *> ARE NEVER MIXED IN ONE FIGURE.
      *>
      *> CA-FUNCTION VALUES
      *>   EXPECTED  MARGIN ON A CHOSEN ESTIMATE BASIS
      *>   REALISED  MARGIN ON ATTRIBUTABLE ACTUAL COST OF SALES
      *>   LINE      MARGIN OF ONE DOCUMENT LINE (QTY x PRICE)
      *>   ORDER     MARGIN OF A WHOLE SALES ORDER FROM THE DATABASE
      *>   JOB       MARGIN OF A JOB - REVENUE LESS RECOGNISED COST
      *>
      *> REASON CODES
      *>   CST0701 COMPANY MISSING
      *>   CST0702 NET REVENUE IS ZERO - MARGIN PERCENTAGE UNDEFINED
      *>   CST0703 COST IS ZERO - MARKUP UNDEFINED
      *>   CST0704 COST BASIS NOT AVAILABLE - RESULT INCOMPLETE
      *>   CST0705 QUANTITY MUST BE POSITIVE
      *>   CST0706 DOCUMENT NOT FOUND
      *>   CST0707 VAT INCLUSIVE REVENUE SUPPLIED WITHOUT A VAT RATE
      *>   CST0708 UNSUPPORTED FUNCTION
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-11-05 RJH  CR0601 ORIGINAL ORDER MARGIN ENQUIRY
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT DOCUMENT DATES
      *> 2006-10-16 DJM  CR1455 EXPECTED AND REALISED SEPARATED
      *> 2026-10-06 STERP CST-013 EX VAT REVENUE, UNDEFINED STATES
      *>================================================================
      *> REQ: CST-013
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  MG-BODY.
               10  MG-DOC-TYPE         PIC X(20).
               10  MG-DOC-REF          PIC X(40).
               10  MG-LINE-NUM         PIC S9(9).
               10  MG-ITEM-CODE        PIC X(40).
               10  MG-JOB-REF          PIC X(20).
               10  MG-CUSTOMER         PIC X(20).
               10  MG-EFF-DATE         PIC 9(8).
               10  MG-CURRENCY         PIC X(3).
               10  MG-FX-RATE          PIC S9(9)V9(8).
      *>       ---- revenue side ----
               10  MG-QUANTITY         PIC S9(13)V9(6).
               10  MG-UNIT-PRICE       PIC S9(11)V9(6).
               10  MG-GROSS-REVENUE    PIC S9(13)V9(2).
               10  MG-DISCOUNT         PIC S9(13)V9(2).
               10  MG-VAT-RATE         PIC S9(5)V9(4).
               10  MG-REVENUE-INCL-VAT PIC X.
                   88  MG-IS-VAT-INCL            VALUE "Y".
               10  MG-NET-REVENUE      PIC S9(13)V9(2).
               10  MG-VAT-AMOUNT       PIC S9(13)V9(2).
      *>       ---- cost side ----
               10  MG-COST-BASIS       PIC X(12).
                   88  MG-ON-STANDARD            VALUE "STANDARD".
                   88  MG-ON-AVERAGE             VALUE "AVERAGE".
                   88  MG-ON-FIFO                VALUE "FIFO".
                   88  MG-ON-ACTUAL              VALUE "ACTUAL".
                   88  MG-ON-QUOTE               VALUE "QUOTE".
               10  MG-UNIT-COST        PIC S9(11)V9(6).
               10  MG-COST-AMOUNT      PIC S9(13)V9(2).
               10  MG-ADJUSTMENTS      PIC S9(13)V9(2).
               10  MG-COST-BASIS-USED  PIC X(12).
      *>       ---- results ----
               10  MG-MARGIN-VALUE     PIC S9(13)V9(2).
               10  MG-MARGIN-PCT       PIC S9(7)V9(4).
               10  MG-MARKUP-PCT       PIC S9(7)V9(4).
               10  MG-MARGIN-STATE     PIC X(12).
                   88  MG-MARGIN-OK              VALUE "OK".
                   88  MG-MARGIN-UNDEF           VALUE "UNDEFINED".
               10  MG-MARKUP-STATE     PIC X(12).
                   88  MG-MARKUP-OK              VALUE "OK".
                   88  MG-MARKUP-UNDEF           VALUE "UNDEFINED".
               10  MG-RESULT-NATURE    PIC X(10).
                   88  MG-IS-EXPECTED            VALUE "EXPECTED".
                   88  MG-IS-REALISED            VALUE "REALISED".
               10  MG-COMPLETENESS     PIC X(12).
               10  MG-LINES-COUNTED    PIC S9(9).
               10  MG-LINES-NO-COST    PIC S9(9).
               10  MG-WARNING          PIC X(80).
