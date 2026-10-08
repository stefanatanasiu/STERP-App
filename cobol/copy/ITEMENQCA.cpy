      *>================================================================
      *> ITEMENQCA - COMMAREA for ITEMENQ, the item enquiry service:
      *>             supply rules, partner references, where used and
      *>             usage history under role control.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : ITEM AND PART MAINTENANCE
      *>
      *> CA-FUNCTION values
      *>   SUPPLY    make/buy authority, preferred and alternate
      *>             approved sources, lead time, minimum order,
      *>             purchase multiple, reorder and safety targets for
      *>             the site (ITM-008).  IE-ORDER-QTY is rounded up to
      *>             the minimum and the purchase multiple, and the
      *>             excess is reported separately so that procurement
      *>             rounding never changes engineering usage.
      *>   PARTNER   supplier, customer and manufacturer references
      *>             with unit, validity and approved source flags
      *>             (ITM-009).  An alias that selects more than one
      *>             active internal item is reported as ambiguous.
      *>   WHEREUSED parents that use the item: bills of material,
      *>             kits and substitution rules (ITM-023)
      *>   USAGE     movement history summarised by type and period
      *>   SUMMARY   stock, demand, supply and valuation in one call,
      *>             with restricted figures suppressed rather than
      *>             shown as zero
      *>
      *> IE-COST-VISIBLE is "N" when the user does not hold the cost
      *> enquiry permission: the cost fields are then left at zero and
      *> IE-RESTRICTED-TEXT says so, because an unavailable figure must
      *> be labelled, never invented (ITM-023).
      *>
      *> REASON CODES
      *>   ENQ0001 item not known in this company
      *>   ENQ0002 no site planning record
      *>   ENQ0003 alias is ambiguous in this context
      *>   ENQ0004 purchase multiple or minimum is not positive
      *>   ENQ0005 restricted - permission not held
      *>   ENQ0006 unsupported function
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-08-19 DJM  CR0450 WHERE USED ENQUIRY
      *> 1999-04-07 RJH  CR0961 PURCHASE MULTIPLE ROUNDING
      *> 2015-09-22 ANW  CR3005 COST VISIBILITY BY ROLE
      *> 2026-10-05 STERP ITM-008, ITM-009, ITM-023
      *>================================================================
      *> REQ: ITM-008, ITM-009, ITM-023
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  IE-BODY.
               10  IE-ITEM-CODE        PIC X(40).
               10  IE-REVISION         PIC X(4).
               10  IE-SITE             PIC X(10).
               10  IE-PARTNER-TYPE     PIC X(10).
               10  IE-PARTNER-CODE     PIC X(20).
               10  IE-ALIAS            PIC X(40).
               10  IE-AS-AT            PIC 9(8).
               10  IE-FROM-DATE        PIC 9(8).
               10  IE-TO-DATE          PIC 9(8).
               10  IE-DESCRIPTION      PIC X(100).
               10  IE-MAKE-BUY         PIC X(6).
               10  IE-LEAD-TIME-DAYS   PIC S9(5).
               10  IE-MIN-ORDER-QTY    PIC S9(13)V9(6).
               10  IE-PURCH-MULTIPLE   PIC S9(13)V9(6).
               10  IE-REORDER-POINT    PIC S9(13)V9(6).
               10  IE-SAFETY-STOCK     PIC S9(13)V9(6).
               10  IE-MAX-STOCK        PIC S9(13)V9(6).
               10  IE-ABC-CLASS        PIC X.
               10  IE-DEFAULT-WHS      PIC X(20).
               10  IE-DEFAULT-LOC      PIC X(20).
               10  IE-ORDER-QTY        PIC S9(13)V9(6).
               10  IE-ROUNDED-QTY      PIC S9(13)V9(6).
               10  IE-EXCESS-QTY       PIC S9(13)V9(6).
               10  IE-PREFERRED-SUPP   PIC X(20).
               10  IE-PREFERRED-ALIAS  PIC X(40).
               10  IE-RESOLVED-ITEM    PIC X(40).
               10  IE-AMBIGUOUS        PIC X.
               10  IE-QTY-ON-HAND      PIC S9(13)V9(6).
               10  IE-QTY-FREE         PIC S9(13)V9(6).
               10  IE-QTY-RESERVED     PIC S9(13)V9(6).
               10  IE-QTY-DEMAND       PIC S9(13)V9(6).
               10  IE-QTY-SUPPLY       PIC S9(13)V9(6).
               10  IE-QTY-ISSUED       PIC S9(13)V9(6).
               10  IE-QTY-RECEIVED     PIC S9(13)V9(6).
               10  IE-UNIT-COST        PIC S9(11)V9(6).
               10  IE-STOCK-VALUE-GBP  PIC S9(13)V99.
               10  IE-COST-VISIBLE     PIC X.
                   88  IE-CAN-SEE-COST           VALUE "Y".
               10  IE-RESTRICTED-TEXT  PIC X(80).
               10  IE-LINE-CNT         PIC 9(3).
               10  IE-LINES OCCURS 25 TIMES.
                   15  IE-L-KIND       PIC X(12).
                   15  IE-L-CODE       PIC X(40).
                   15  IE-L-DESC       PIC X(60).
                   15  IE-L-REF        PIC X(40).
                   15  IE-L-QTY        PIC S9(13)V9(6).
                   15  IE-L-UOM        PIC X(10).
                   15  IE-L-VALUE      PIC S9(13)V99.
                   15  IE-L-DATE       PIC 9(8).
                   15  IE-L-STATUS     PIC X(12).
                   15  IE-L-FLAG       PIC X.
