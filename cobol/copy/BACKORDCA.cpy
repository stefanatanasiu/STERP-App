      *>================================================================
      *> BACKORDCA - COMMAREA for BACKORD, partial fulfilment and the
      *>             back order balance.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : SALES ORDER MANAGEMENT
      *>
      *> The two quantity rules of specification 05 section 4 are the
      *> whole of this COMMAREA:
      *>
      *>   remaining to despatch = AcceptedQty - CancelledQty
      *>                           - DespatchedQty
      *>   remaining eligible to invoice = chargeable confirmed
      *>           fulfilment - withdrawn before billing - billed
      *>
      *> A confirmed despatch is never erased by a cancellation, and a
      *> return of billed goods goes through credit/return processes -
      *> it does not change what remains to despatch and does not
      *> create new billing eligibility (SO-014, SO-015).
      *>
      *> CA-FUNCTION values
      *>   EVALUATE  per line and per schedule, recompute the two
      *>             remaining quantities and the billed net.  Nothing
      *>             is written.
      *>   CREATEBO  carry the remaining quantity forward with the
      *>             customer's instruction KEEP, CANCEL or SUBSTITUTE
      *>             and the promise dates preserved.
      *>   CLOSE     close the lines and schedules that have no
      *>             remaining quantity.  Completing one schedule does
      *>             NOT close an unfulfilled balance on another, and
      *>             header completion is derived from the line
      *>             outcomes plus outstanding financial and return
      *>             exceptions - it is never a way to force residual
      *>             quantities to zero.
      *>   LIST      the open back order book for the order
      *>
      *> REASON CODES
      *>   BKO0001 a company and an order are required
      *>   BKO0002 order not found
      *>   BKO0003 unsupported function
      *>   BKO0004 the customer's instruction is not KEEP, CANCEL or
      *>           SUBSTITUTE
      *>   BKO0005 a substitution needs an approved substitute item
      *>   BKO0006 there is no remaining quantity to carry forward
      *>   BKO0007 the header cannot be completed while quantities or
      *>           financial exceptions are outstanding
      *>   BKO0008 order line not found
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1992-06-30 RJH  CR0188 BACK ORDER BOOK
      *> 2007-09-11 KMP  CR1944 PER SCHEDULE COMPLETION
      *> 2026-10-06 STERP SO-014, SO-015 REMAINING QUANTITY RULES
      *>================================================================
      *> REQ: SO-014, SO-015
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  BO-BODY.
               10  BO-ORDER-NUMBER     PIC X(30).
               10  BO-SALES-ORDER-ID   PIC S9(17).
               10  BO-LINE-NUM         PIC S9(9) COMP-5.
               10  BO-SCHEDULE-NO      PIC S9(9) COMP-5.
               10  BO-CUSTOMER         PIC X(20).
               10  BO-CURRENCY         PIC X(3).
               10  BO-INSTRUCTION      PIC X(12).
                   88  BO-KEEP                   VALUE "KEEP".
                   88  BO-CANCEL                 VALUE "CANCEL".
                   88  BO-SUBSTITUTE             VALUE "SUBSTITUTE".
               10  BO-SUBST-ITEM       PIC X(40).
               10  BO-SUBST-APPROVAL   PIC S9(9) COMP-5.
               10  BO-BACKORDER-NUMBER PIC X(30).
               10  BO-REASON-TEXT      PIC X(200).
               10  BO-UNIT-PRICE       PIC S9(11)V9(6).
      *>       order level totals of the two remaining quantities
               10  BO-ACCEPTED-QTY     PIC S9(13)V9(6).
               10  BO-CANCELLED-QTY    PIC S9(13)V9(6).
               10  BO-DESPATCHED-QTY   PIC S9(13)V9(6).
               10  BO-BILLED-QTY       PIC S9(13)V9(6).
               10  BO-RETURNED-QTY     PIC S9(13)V9(6).
               10  BO-WITHDRAWN-QTY    PIC S9(13)V9(6).
               10  BO-REMAIN-DESPATCH  PIC S9(13)V9(6).
               10  BO-REMAIN-INVOICE   PIC S9(13)V9(6).
               10  BO-BILLED-NET       PIC S9(11)V99 COMP-3.
               10  BO-OPEN-NET         PIC S9(11)V99 COMP-3.
      *>       completion is derived, never asserted
               10  BO-HEADER-COMPLETE  PIC X.
                   88  BO-IS-COMPLETE            VALUE "Y".
               10  BO-OPEN-LINE-CNT    PIC S9(9) COMP-5.
               10  BO-OPEN-SCHED-CNT   PIC S9(9) COMP-5.
               10  BO-EXCEPTION-CNT    PIC S9(9) COMP-5.
               10  BO-EXCEPTION-TEXT   PIC X(200).
               10  BO-CLOSED-LINES     PIC S9(9) COMP-5.
               10  BO-CLOSED-SCHEDS    PIC S9(9) COMP-5.
               10  BO-CARRIED-QTY      PIC S9(13)V9(6).
               10  BO-COMM-STATUS      PIC X(12).
               10  BO-FULFIL-STATUS    PIC X(14).
               10  BO-BILLING-STATUS   PIC X(14).
      *>       one row per line, or per schedule when BO-BY-SCHEDULE
               10  BO-BY-SCHEDULE      PIC X.
                   88  BO-SCHEDULE-VIEW          VALUE "Y".
               10  BO-LINE-CNT         PIC 9(3).
               10  BO-LINES OCCURS 40 TIMES.
                   15  BO-L-LINE-NUM   PIC S9(9) COMP-5.
                   15  BO-L-SCHED-NO   PIC S9(9) COMP-5.
                   15  BO-L-ITEM       PIC X(40).
                   15  BO-L-DESC       PIC X(100).
                   15  BO-L-UOM        PIC X(10).
                   15  BO-L-ACCEPTED   PIC S9(13)V9(6).
                   15  BO-L-CANCELLED  PIC S9(13)V9(6).
                   15  BO-L-DESPATCHED PIC S9(13)V9(6).
                   15  BO-L-BILLED     PIC S9(13)V9(6).
                   15  BO-L-RETURNED   PIC S9(13)V9(6).
                   15  BO-L-WITHDRAWN  PIC S9(13)V9(6).
                   15  BO-L-REM-DESP   PIC S9(13)V9(6).
                   15  BO-L-REM-INV    PIC S9(13)V9(6).
                   15  BO-L-UNIT-PRICE PIC S9(11)V9(6).
                   15  BO-L-BILLED-NET PIC S9(11)V99 COMP-3.
                   15  BO-L-PROMISE    PIC 9(8).
                   15  BO-L-REQUESTED  PIC 9(8).
                   15  BO-L-INSTRUCT   PIC X(12).
                   15  BO-L-STATUS     PIC X(10).
