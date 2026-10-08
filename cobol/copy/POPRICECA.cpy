      *>================================================================
      *> POPRICECA - COMMAREA for POPRICE, purchase price determination.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> Purchase pricing uses the captured effective dated agreement,
      *> quotation or list terms that applied on the pricing date, with
      *> quantity breaks, discounts and price-per units (PO-004).  The
      *> pricing date of an issued order is its order date, so a later
      *> supplier price change cannot silently reprice it; a caller
      *> that wants today's price must say so.
      *>
      *> CA-FUNCTION values
      *>   PRICE    determine the unit price for a quantity
      *>   BREAKS   return the break table for a supplier and item
      *>   HISTORY  last price actually paid (PO-002 price history)
      *>
      *> Price-per: a price of 12.500000 per 100 EA means a unit price
      *> of 0.125000.  PR-PRICE-PER is never zero on output.
      *>
      *> net line value = round(qty * unit price / price per, 2)
      *>                  less discount, to 2 decimals (D-09, COM-011)
      *>
      *> REASON CODES
      *>   POP1001 supplier or item not supplied
      *>   POP1002 no effective price found for the pricing date
      *>   POP1003 quantity is zero or negative
      *>   POP1004 unsupported function
      *>   POP1005 price is held in another currency
      *>----------------------------------------------------------------
      *> 1998-04-14 RJH  CR0791 PURCHASE PRICE BOOK
      *> 2004-02-09 DJM  CR1203 PRICE PER QUANTITY FOR FASTENERS
      *> 2026-10-06 STERP PO-004 EFFECTIVE DATED AGREEMENTS
      *>================================================================
      *> REQ: PO-004, PO-002
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  PR-BODY.
               10  PR-SUPPLIER         PIC X(20).
               10  PR-ITEM-CODE        PIC X(40).
               10  PR-ITEM-GROUP       PIC X(20).
               10  PR-UOM-CODE         PIC X(10).
               10  PR-QUANTITY         PIC S9(13)V9(6).
               10  PR-PRICING-DATE     PIC 9(8).
               10  PR-CURRENCY         PIC X(3).
      *>     ---- answers ----
               10  PR-UNIT-PRICE       PIC S9(11)V9(6).
               10  PR-PRICE-PER        PIC S9(11)V9(6).
               10  PR-EFFECTIVE-PRICE  PIC S9(11)V9(6).
               10  PR-DISCOUNT-PCT     PIC S9(5)V9(4).
               10  PR-SETT-DISC-PCT    PIC S9(5)V9(4).
               10  PR-NET-VALUE        PIC S9(13)V99.
               10  PR-GROSS-VALUE      PIC S9(13)V99.
               10  PR-PRICE-BASIS      PIC X(10).
               10  PR-AGREEMENT-REF    PIC X(40).
               10  PR-PRICE-ID         PIC S9(9) COMP-5.
               10  PR-EFFECTIVE-FROM   PIC 9(8).
               10  PR-EFFECTIVE-TO     PIC 9(8).
               10  PR-BREAK-QTY-USED   PIC S9(13)V9(6).
               10  PR-SOURCE-TEXT      PIC X(120).
               10  PR-LAST-PAID-PRICE  PIC S9(11)V9(6).
               10  PR-LAST-PAID-DATE   PIC 9(8).
               10  PR-BREAK-CNT        PIC 9(2).
               10  PR-BREAK-TABLE.
                   15  PR-BREAK OCCURS 10 TIMES.
                       20  PR-B-FROM-QTY   PIC S9(13)V9(6).
                       20  PR-B-UNIT-PRICE PIC S9(11)V9(6).
                       20  PR-B-DISC-PCT   PIC S9(5)V9(4).
