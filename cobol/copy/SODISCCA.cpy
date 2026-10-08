      *>================================================================
      *> SODISCCA - COMMAREA for SODISC, the sales order discount,
      *>            allowance, charge and VAT calculation sequence
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : SALES ORDER MANAGEMENT
      *> CALLED BY : SALES ORDER ENTRY, QUOTATION ENTRY, INVOICING,
      *>             THE .NET CLIENT
      *>
      *> THE SEQUENCE IS FIXED (SO-005) AND NOTHING MAY BE REORDERED:
      *>   1  convert the entered quantity to the price unit
      *>   2  extend by the price-per quantity
      *>   3  apply the line percentage discounts IN SEQUENCE
      *>      (10% then 5% on 360.00 is 307.80, it is not 306.00)
      *>   4  allocate the fixed line and document allowances with
      *>      RNDALLOC ALLOC / WEIGHT so the parts re-add exactly
      *>   5  add separately identified charges; carriage charged on a
      *>      supply follows the VAT liability of the goods
      *>   6  VAT and rounding
      *>   7  present line and document net, VAT and gross
      *>
      *> THE INPUT FIELDS ARE NEVER OVERWRITTEN, SO CALLING SODISC
      *> TWICE WITH THE SAME COMMAREA GIVES THE SAME ANSWER AND A
      *> DISCOUNT CANNOT BE APPLIED TWICE.  SD-APPLIED-TAG CARRIES THE
      *> SIGNATURE OF THE INPUTS THAT PRODUCED THE CURRENT RESULT AND
      *> SD-ALREADY-APPLIED SAYS THAT THE REQUEST WAS A REPEAT.
      *>
      *> CA-FUNCTION VALUES
      *>   LINE      one line only, using SD-LINE(1)
      *>   DOCUMENT  up to 200 lines with document allowance
      *>             allocation and reconciled document totals
      *>   RECALC    DOCUMENT, forcing a recalculation even when the
      *>             input signature is unchanged
      *>
      *> REASON CODES
      *>   DSC0001 company missing
      *>   DSC0002 no lines supplied
      *>   DSC0003 too many lines for this COMMAREA (200)
      *>   DSC0004 quantity must be positive
      *>   DSC0005 price-per quantity must be positive
      *>   DSC0006 a discount percentage is out of range
      *>   DSC0007 no unit conversion - a factor of one is never assumed
      *>   DSC0008 unsupported function
      *>   DSC0009 document allowance exceeds the discounted line value
      *>   DSC0010 line and document totals do not reconcile
      *>   DSC0011 VAT rate missing for a standard or reduced rate line
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-03-16 RJH  CR0912 SALES ORDER PRICING REWRITE (ACUCOBOL)
      *> 2000-06-12 KMP  CR0988 SEQUENTIAL NOT ADDITIVE DISCOUNTS
      *> 2011-01-04 DLS  CR2240 VAT 20 PER CENT
      *> 2026-10-06 STERP SO-005 ALLOWANCE ALLOCATION AND RECONCILIATION
      *>================================================================
      *> REQ: SO-005
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SD-BODY.
      *>       ---- document level input ----
               10  SD-DOC-TYPE         PIC X(20).
               10  SD-DOC-REF          PIC X(40).
               10  SD-CUSTOMER         PIC X(20).
               10  SD-CURRENCY         PIC X(3).
               10  SD-PRICE-DATE       PIC 9(8).
               10  SD-ROUND-PLACES     PIC S9(4) SIGN LEADING SEPARATE.
               10  SD-DOC-ALLOWANCE    PIC S9(13)V9(2) COMP-3.
               10  SD-DOC-ALLOW-DESC   PIC X(40).
               10  SD-DOC-CHARGE       PIC S9(13)V9(2) COMP-3.
               10  SD-DOC-CHARGE-DESC  PIC X(40).
               10  SD-DOC-CHARGE-VAT   PIC X(4).
               10  SD-CHARGE-VAT-RATE  PIC S9(5)V9(4) COMP-3.
               10  SD-CHARGE-VAT-BASIS PIC X(8).
                   88  SD-CHG-FOLLOWS            VALUE "GOODS".
                   88  SD-CHG-FIXED              VALUE "FIXED".
               10  SD-APPLIED-TAG      PIC X(32).
               10  SD-ALREADY-APPLIED  PIC X.
                   88  SD-WAS-REPEAT             VALUE "Y".
               10  SD-LINE-CNT         PIC 9(3).
      *>       ---- document level results ----
               10  SD-TOT-GROSS        PIC S9(13)V9(2) COMP-3.
               10  SD-TOT-DISCOUNT     PIC S9(13)V9(2) COMP-3.
               10  SD-TOT-ALLOWANCE    PIC S9(13)V9(2) COMP-3.
               10  SD-TOT-CHARGE       PIC S9(13)V9(2) COMP-3.
               10  SD-TOT-NET          PIC S9(13)V9(2) COMP-3.
               10  SD-TOT-VAT          PIC S9(13)V9(2) COMP-3.
               10  SD-TOT-GROSS-TOTAL  PIC S9(13)V9(2) COMP-3.
               10  SD-TOT-COST-GBP     PIC S9(13)V9(2) COMP-3.
               10  SD-DOC-MARGIN-PCT   PIC S9(5)V9(4) COMP-3.
               10  SD-ALLOC-RESIDUAL   PIC S9(13)V9(2) COMP-3.
               10  SD-ALLOC-BUMPED     PIC S9(9) COMP-5.
               10  SD-RECONCILED       PIC X.
                   88  SD-IS-RECONCILED          VALUE "Y".
               10  SD-EXPLANATION      PIC X(200).
      *>       ---- the lines (hard limit 200, CR0912) ----
               10  SD-LINE OCCURS 200 TIMES.
                   15  SD-L-LINE-NUM   PIC S9(9) COMP-5.
                   15  SD-L-LINE-REF   PIC X(8).
                   15  SD-L-ITEM-CODE  PIC X(40).
                   15  SD-L-ENT-QTY    PIC S9(13)V9(6) COMP-3.
                   15  SD-L-ENT-UOM    PIC X(10).
                   15  SD-L-PRICE-UOM  PIC X(10).
                   15  SD-L-FACTOR     PIC S9(9)V9(9) COMP-3.
                   15  SD-L-PRICE-QTY  PIC S9(13)V9(6) COMP-3.
                   15  SD-L-UNIT-PRICE PIC S9(11)V9(6) COMP-3.
                   15  SD-L-PER-QTY    PIC S9(9)V9(6) COMP-3.
                   15  SD-L-GROSS-AMT  PIC S9(13)V9(2) COMP-3.
                   15  SD-L-DISC-PCT1  PIC S9(5)V9(4) COMP-3.
                   15  SD-L-DISC-PCT2  PIC S9(5)V9(4) COMP-3.
                   15  SD-L-DISC-PCT3  PIC S9(5)V9(4) COMP-3.
                   15  SD-L-DISC-AMT   PIC S9(13)V9(2) COMP-3.
                   15  SD-L-LINE-ALLOW PIC S9(13)V9(2) COMP-3.
                   15  SD-L-DOC-ALLOW  PIC S9(13)V9(2) COMP-3.
                   15  SD-L-ALLOW-TOT  PIC S9(13)V9(2) COMP-3.
                   15  SD-L-CHARGE     PIC S9(13)V9(2) COMP-3.
                   15  SD-L-DOC-CHARGE PIC S9(13)V9(2) COMP-3.
                   15  SD-L-CHARGE-VAT PIC X(4).
                   15  SD-L-GOODS-NET  PIC S9(13)V9(2) COMP-3.
                   15  SD-L-NET-AMT    PIC S9(13)V9(2) COMP-3.
                   15  SD-L-VAT-CODE   PIC X(4).
                   15  SD-L-VAT-RATE   PIC S9(5)V9(4) COMP-3.
                   15  SD-L-VAT-AMT    PIC S9(13)V9(2) COMP-3.
                   15  SD-L-GROSS-TOT  PIC S9(13)V9(2) COMP-3.
                   15  SD-L-COST-GBP   PIC S9(13)V9(2) COMP-3.
                   15  SD-L-MARGIN-PCT PIC S9(5)V9(4) COMP-3.
                   15  SD-L-STATUS     PIC X(8).
                   15  SD-L-WHY        PIC X(30).
