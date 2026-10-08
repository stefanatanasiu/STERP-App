      *>================================================================
      *> SOURCSELCA - COMMAREA for SOURCSEL, approved source selection.
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> PO-002.  The buyer compares approved sources on price, price
      *> history, lead time, performance and commercial restriction.
      *> The cheapest quotation is NEVER selected automatically: the
      *> program ranks the quotations, names the cheapest, and makes
      *> the buyer give a reason whenever the selected quotation is
      *> not the cheapest one.  An unapproved, suspended or expired
      *> source can only be selected with an explicit authorisation
      *> recorded against the sourcing event.
      *>
      *> CA-FUNCTION values
      *>   CHECK     is this supplier an approved source for the item
      *>   OPEN      open a sourcing event for a requisition line
      *>   QUOTE     add a supplier quotation to the event
      *>   RANK      rank the quotations and report the cheapest
      *>   SELECT    select a supplier with a justification
      *>
      *> CA-RETURN-CODE 00 approved source, 04 conditional source or
      *>   selection above the cheapest quote, 06 unapproved source -
      *>   authority required, 08 rejected, 16 no source on file
      *>
      *> REASON CODES
      *>   SRC0001 company, item or supplier missing
      *>   SRC0002 no approved source for the item
      *>   SRC0003 the source is suspended
      *>   SRC0004 the source is outside its effective dates
      *>   SRC0005 unapproved source needs explicit authorisation
      *>   SRC0006 sourcing event not found
      *>   SRC0007 a selection above the cheapest quote needs a reason
      *>   SRC0008 unsupported function
      *>   SRC0009 the quotation has expired
      *>   SRC0010 commercial restriction must be acknowledged
      *>   SRC0011 quantity is below the supplier minimum order
      *>   SRC0012 no quotations have been recorded
      *>----------------------------------------------------------------
      *> 1998-05-14 RJH  CR0795 APPROVED SOURCE REGISTER
      *> 2002-02-18 DJM  CR1056 CHEAPEST IS NOT AUTOMATIC
      *> 2026-10-06 STERP PRICE HISTORY AND PERFORMANCE COMPARISON
      *>================================================================
      *> REQ: PO-002, PO-004, PO-023
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SS-BODY.
               10  SS-SOURCING-REF     PIC X(30).
               10  SS-REQ-NUMBER       PIC X(30).
               10  SS-ITEM-CODE        PIC X(40).
               10  SS-DESCRIPTION      PIC X(200).
               10  SS-QUANTITY         PIC S9(13)V9(6).
               10  SS-UOM-CODE         PIC X(10).
               10  SS-NEEDED-BY        PIC 9(8).
               10  SS-BUYER            PIC X(20).
               10  SS-EVENT-DATE       PIC 9(8).
      *>     ---- quotation fields ----
               10  SS-LINE-NUM         PIC S9(9) COMP-5.
               10  SS-SUPPLIER         PIC X(20).
               10  SS-QUOTE-REF        PIC X(40).
               10  SS-CURRENCY         PIC X(3).
               10  SS-UNIT-PRICE       PIC S9(11)V9(6).
               10  SS-PRICE-PER        PIC S9(11)V9(6).
               10  SS-DISCOUNT-PCT     PIC S9(5)V9(4).
               10  SS-LEAD-TIME-IN     PIC S9(5).
               10  SS-QUOTE-VALID-TO   PIC 9(8).
               10  SS-ONTIME-PCT       PIC S9(5)V9(4).
               10  SS-QUALITY-PCT      PIC S9(5)V9(4).
               10  SS-RESTRICTION      PIC X(200).
      *>     ---- selection fields ----
               10  SS-SELECT-REASON    PIC X(300).
               10  SS-AUTHORISED-BY    PIC X(20).
               10  SS-ACCEPT-RESTRICT  PIC X.
                   88  SS-RESTRICT-OK            VALUE "Y".
      *>     ---- answers ----
               10  SS-EVENT-ID         PIC S9(9) COMP-5.
               10  SS-QUOTE-ID         PIC S9(9) COMP-5.
               10  SS-SOURCE-ID        PIC S9(9) COMP-5.
               10  SS-APPROVAL-STATUS  PIC X(12).
                   88  SS-SRC-APPROVED           VALUE "APPROVED".
                   88  SS-SRC-CONDITION          VALUE "CONDITION".
                   88  SS-SRC-UNAPPROVED         VALUE "UNAPPROVED".
                   88  SS-SRC-SUSPENDED          VALUE "SUSPENDED".
               10  SS-SUPPLIER-PART    PIC X(40).
               10  SS-CONTRACT-REF     PIC X(40).
               10  SS-LEAD-TIME-DAYS   PIC S9(5).
               10  SS-MIN-ORDER-QTY    PIC S9(13)V9(6).
               10  SS-PACK-QTY         PIC S9(13)V9(6).
               10  SS-SRC-CURRENCY     PIC X(3).
               10  SS-SRC-RESTRICTION  PIC X(200).
               10  SS-LINE-VALUE-GBP   PIC S9(13)V99.
               10  SS-LAST-PAID-PRICE  PIC S9(11)V9(6).
               10  SS-LAST-PAID-DATE   PIC 9(8).
               10  SS-CHEAPEST-SUPP    PIC X(20).
               10  SS-CHEAPEST-GBP     PIC S9(13)V99.
               10  SS-SELECTED-GBP     PIC S9(13)V99.
               10  SS-PREMIUM-GBP      PIC S9(13)V99.
               10  SS-RANK             PIC S9(4).
               10  SS-QUOTE-CNT        PIC 9(4).
               10  SS-NEEDS-AUTH       PIC X.
                   88  SS-AUTH-REQUIRED          VALUE "Y".
               10  SS-ON-PRICE          PIC X.
                   88  SS-WON-ON-PRICE           VALUE "Y".
               10  SS-STATUS           PIC X(12).
               10  SS-APPROVAL-ID      PIC S9(17).
               10  SS-SOURCE-TEXT      PIC X(200).
