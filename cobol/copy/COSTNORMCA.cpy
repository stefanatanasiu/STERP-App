      *>================================================================
      *> COSTNORMCA - COMMAREA FOR COSTNORM, THE COST NORMALISATION
      *>            AND PURCHASE COST SERVICE.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : COSTING AND PROFITABILITY
      *>
      *> NOTHING IN COSTING MAY COMPARE OR ADD TWO COST FIGURES UNTIL
      *> BOTH CARRY THE SAME UNIT, THE SAME CURRENCY AND A STATED
      *> EFFECTIVE DATE (CST-003).  A PER CARTON EURO PRICE IS NOT A
      *> PER UNIT STERLING COST AND THIS PROGRAM IS THE ONLY PLACE
      *> ALLOWED TO TURN ONE INTO THE OTHER.
      *>
      *>   sterling unit cost = foreign price
      *>            / (price-per qty x stock units per price unit)
      *>            / foreign currency units per GBP
      *>
      *> CA-FUNCTION VALUES
      *>   CONTEXT   STATE THE ENQUIRY CONTEXT - PURPOSE, BASIS,
      *>             CURRENCY, UNIT, EFFECTIVE DATE, QUANTITY AND
      *>             APPROVAL STATE.  REFUSES TO SUBSTITUTE A BASIS
      *>             THAT WAS NOT ASKED FOR (CST-001).
      *>   NORMCOST  NORMALISE CN-FOREIGN-PRICE TO A STERLING COST PER
      *>             STOCK UNIT -> CN-UNIT-COST-GBP
      *>   PURCOST   ACQUISITION COST OF A RECEIPT INCLUDING DISCOUNT,
      *>             CUSTOMS DUTY, ATTRIBUTABLE FREIGHT AND THE
      *>             NON RECOVERABLE PART OF THE VAT (CST-004).
      *>             RECOVERABLE VAT IS NEVER CAPITALISED.
      *>   ELEMENTS  SPLIT CN-EXT-COST-GBP OVER THE COST ELEMENTS AND
      *>             PROVE THAT THE PARTS EQUAL THE TOTAL (CST-002).
      *>
      *> REASON CODES
      *>   CST0101 COMPANY, ITEM OR FUNCTION MISSING
      *>   CST0102 REQUESTED BASIS IS NOT AVAILABLE - NO SUBSTITUTION
      *>   CST0103 QUANTITY MUST BE POSITIVE
      *>   CST0104 PRICE-PER QUANTITY IS ZERO OR NEGATIVE
      *>   CST0105 EXCHANGE RATE IS ZERO OR NEGATIVE
      *>   CST0106 UNIT CONVERSION UNAVAILABLE (NO IMPLIED ONE TO ONE)
      *>   CST0107 VAT RECOVERY PERCENTAGE OUT OF RANGE
      *>   CST0108 ELEMENTS DO NOT RECONCILE TO THE TOTAL
      *>   CST0109 UNSUPPORTED FUNCTION
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-03-17 RJH  CR0502 ORIGINAL - DECIMAL CURRENCY ONLY
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT EFFECTIVE DATES
      *> 2003-01-06 RJH  CR1021 PRICE-PER QUANTITY FOR IMPORTED PARTS
      *> 2021-01-01 DJM  CR2188 BREXIT - CUSTOMS DUTY ON GB IMPORTS
      *> 2026-10-06 STERP CST-001 CST-003 CST-004 CST-024
      *>================================================================
      *> REQ: CST-001, CST-002, CST-003, CST-004, CST-024
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  CN-BODY.
      *>       ---- enquiry context (CST-001) ----
               10  CN-ITEM-CODE        PIC X(40).
               10  CN-REVISION         PIC X(4).
               10  CN-PURPOSE          PIC X(12).
                   88  CN-FOR-QUOTE              VALUE "QUOTE".
                   88  CN-FOR-STANDARD           VALUE "STANDARD".
                   88  CN-FOR-VALUATION          VALUE "VALUATION".
                   88  CN-FOR-ENQUIRY            VALUE "ENQUIRY".
               10  CN-BASIS            PIC X(12).
                   88  CN-BASIS-LASTPUR          VALUE "LASTPURCH".
                   88  CN-BASIS-STD              VALUE "STANDARD".
                   88  CN-BASIS-AVERAGE          VALUE "AVERAGE".
                   88  CN-BASIS-FIFO             VALUE "FIFO".
               10  CN-BASIS-USED       PIC X(12).
               10  CN-CURRENCY         PIC X(3).
               10  CN-PRICE-UOM        PIC X(10).
               10  CN-STOCK-UOM        PIC X(10).
               10  CN-EFF-DATE         PIC 9(8).
               10  CN-QUANTITY         PIC S9(13)V9(6).
               10  CN-APPROVAL-STATE   PIC X(12).
               10  CN-COST-SET         PIC X(20).
      *>       ---- price inputs ----
               10  CN-FOREIGN-PRICE    PIC S9(11)V9(6).
               10  CN-PRICE-PER-QTY    PIC S9(9)V9(6).
               10  CN-FX-RATE          PIC S9(9)V9(8).
               10  CN-FX-RATE-DATE     PIC 9(8).
               10  CN-FX-RATE-TYPE     PIC X(8).
               10  CN-DISCOUNT-PCT     PIC S9(5)V9(4).
               10  CN-DUTY-PCT         PIC S9(5)V9(4).
               10  CN-DUTY-AMOUNT      PIC S9(11)V9(2).
               10  CN-FREIGHT-GBP      PIC S9(11)V9(2).
               10  CN-OTHER-CHARGE-GBP PIC S9(11)V9(2).
               10  CN-VAT-CODE         PIC X(4).
               10  CN-VAT-RATE         PIC S9(5)V9(4).
               10  CN-VAT-RECOVER-PCT  PIC S9(5)V9(4).
      *>       ---- normalised results ----
               10  CN-FACTOR           PIC S9(9)V9(9).
               10  CN-GOODS-GBP        PIC S9(13)V9(2).
               10  CN-DISCOUNT-GBP     PIC S9(13)V9(2).
               10  CN-DUTY-GBP         PIC S9(13)V9(2).
               10  CN-VAT-TOTAL-GBP    PIC S9(13)V9(2).
               10  CN-VAT-RECOVER-GBP  PIC S9(13)V9(2).
               10  CN-NONREC-VAT-GBP   PIC S9(13)V9(2).
               10  CN-UNIT-COST-GBP    PIC S9(11)V9(6).
               10  CN-EXT-COST-GBP     PIC S9(13)V9(2).
      *>       ---- element split (CST-002) ----
               10  CN-EL-MATERIAL      PIC S9(13)V9(2).
               10  CN-EL-LANDED        PIC S9(13)V9(2).
               10  CN-EL-SUBCON        PIC S9(13)V9(2).
               10  CN-EL-RECOVERY      PIC S9(13)V9(2).
               10  CN-EL-EXCLUDED      PIC S9(13)V9(2).
               10  CN-EL-EXCL-NOTE     PIC X(60).
      *>       ---- provenance (CST-024) ----
               10  CN-PROVISIONAL      PIC X.
                   88  CN-IS-PROVISIONAL         VALUE "Y".
               10  CN-COMPLETENESS     PIC X(12).
                   88  CN-COMPLETE               VALUE "COMPLETE".
                   88  CN-INCOMPLETE             VALUE "INCOMPLETE".
               10  CN-SOURCE           PIC X(20).
               10  CN-EVIDENCE         PIC X(60).
               10  CN-WARNING          PIC X(80).
