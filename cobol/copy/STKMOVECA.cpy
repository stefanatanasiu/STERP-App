      *>================================================================
      *> STKMOVECA - COMMAREA for STKMOVE, the stock movement service.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *>
      *> A displayed balance is never edited.  Every change of physical
      *> stock is an authorised movement that states its source, reason,
      *> ownership, traceability and quality condition, and no movement
      *> may create a negative physical balance (ITM-015).
      *>
      *> STKMOVE validates in COBOL (item state, class, units, quantum,
      *> lot/serial, shelf life, location) and then posts through the
      *> shared contract inv.usp_PostStockMovement so that the balance,
      *> the movement row and the audit evidence are one transaction.
      *>
      *> CA-FUNCTION values
      *>   RECEIPT     goods in                           (increase)
      *>   ISSUE       issue to a job, kit or department   (decrease)
      *>   RETURN      return from a job or a customer     (increase)
      *>   ADJUST      authorised adjustment, SM-QUANTITY signed
      *>   COMPLETE    production completion               (increase)
      *>   OPENING     opening stock load                  (increase)
      *>   TRANSFER    location/site transfer.  When the destination is
      *>               not instant (SM-VIA-TRANSIT = "Y") the stock
      *>               moves to the site transit location and is
      *>               available at neither endpoint until CONFIRM.
      *>   CONFIRM     confirm the receipt of an in-transit transfer
      *>   HOLD        move quantity from one quality condition to
      *>               another and open a quality hold  (ITM-016)
      *>   RELEASE     close a quality hold with its outcome
      *>   SCRAP       scrap with reason                   (decrease)
      *>
      *> SM-QUANTITY is in SM-UOM-CODE; STKMOVE converts it through
      *> UOMCONV and reports the stock quantity actually posted in
      *> SM-STOCK-QTY together with the factor used, which is kept on
      *> the movement so a later packaging change cannot rewrite it.
      *> SM-UNIT-COST is per stock unit. SM-VALUE-GBP accepts the
      *> document's rounded GBP magnitude (zero means no total evidence
      *> for legacy callers). On return both fields are the database
      *> valuation: the value is a magnitude, as in legacy callers;
      *> the database movement and history carry the signed value.
      *> Pools, history and FIFO layers post atomically with stock.
      *>
      *> REASON CODES
      *>   STK0001 item not known / not usable in this state
      *>   STK0002 location not known or not active
      *>   STK0003 quantity is zero, negative or out of range
      *>   STK0004 quantity breaks the item quantum
      *>   STK0005 lot or serial number is required
      *>   STK0006 insufficient physical stock
      *>   STK0007 a reason is required for this movement type
      *>   STK0008 unsupported function
      *>   STK0009 transfer endpoints are the same
      *>   STK0010 serial number is already in stock elsewhere
      *>   STK0011 stock is expired or inside its minimum shelf life
      *>   STK0012 class does not permit physical stock
      *>   STK0013 no transit location is defined for the site
      *>   STK0014 hold quantity exceeds the quantity in condition
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-03-08 RJH  CR0415 STOCK MOVEMENT POSTING
      *> 1997-11-04 DJM  CR0744 IN TRANSIT STAGE FOR SITE TRANSFERS
      *> 2003-06-16 KMP  CR1098 QUALITY HOLD KEEPS VALUATION HISTORY
      *> 2026-10-05 STERP ITM-015 TO ITM-017 VIA usp_PostStockMovement
      *>================================================================
      *> REQ: ITM-015, ITM-016, ITM-017, ITM-024
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SM-BODY.
               10  SM-ITEM-CODE        PIC X(40).
               10  SM-REVISION         PIC X(4).
               10  SM-SITE             PIC X(10).
               10  SM-WAREHOUSE        PIC X(20).
               10  SM-LOCATION         PIC X(20).
               10  SM-TO-SITE          PIC X(10).
               10  SM-TO-WAREHOUSE     PIC X(20).
               10  SM-TO-LOCATION      PIC X(20).
               10  SM-LOT-CODE         PIC X(30).
               10  SM-SERIAL-NO        PIC X(40).
               10  SM-OWNER-TYPE       PIC X(10).
               10  SM-OWNER-CODE       PIC X(20).
               10  SM-CONDITION        PIC X(12).
               10  SM-TO-CONDITION     PIC X(12).
               10  SM-QUANTITY         PIC S9(13)V9(6).
               10  SM-UOM-CODE         PIC X(10).
               10  SM-STOCK-QTY        PIC S9(13)V9(6).
               10  SM-FACTOR           PIC S9(9)V9(9).
               10  SM-UNIT-COST        PIC S9(11)V9(6).
               10  SM-VALUE-GBP        PIC S9(11)V99.
               10  SM-MOVEMENT-DATE    PIC 9(8).
               10  SM-POSTING-DATE     PIC 9(8).
               10  SM-EXPIRY-DATE      PIC 9(8).
               10  SM-REASON-CODE      PIC X(20).
               10  SM-REASON           PIC X(200).
               10  SM-APPROVAL-REF     PIC X(40).
               10  SM-SOURCE-MODULE    PIC X(6).
               10  SM-SOURCE-DOC-TYPE  PIC X(20).
               10  SM-SOURCE-DOC-REF   PIC X(40).
               10  SM-SOURCE-DOC-LINE  PIC S9(9) COMP-5.
               10  SM-VIA-TRANSIT      PIC X.
                   88  SM-USE-TRANSIT            VALUE "Y".
               10  SM-HOLD-TYPE        PIC X(12).
               10  SM-HOLD-ID          PIC S9(9) COMP-5.
               10  SM-HOLD-OUTCOME     PIC X(12).
               10  SM-MOVEMENT-ID      PIC S9(17).
               10  SM-MOVEMENT-ID-2    PIC S9(17).
               10  SM-QTY-BEFORE       PIC S9(13)V9(6).
               10  SM-QTY-AFTER        PIC S9(13)V9(6).
               10  SM-MOVEMENT-TYPE    PIC X(14).
               10  SM-DUPLICATE        PIC X.
               10  SM-TRANSIT-LOC      PIC X(20).
               10  SM-NARRATIVE        PIC X(100).
