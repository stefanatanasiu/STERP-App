      *>================================================================
      *> POCLOSECA - COMMAREA for POCLOSE, purchase order cancellation
      *>             and closure.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> Closing an order is an accounting act, not a tidying up
      *> exercise (PO-020).  Three rules hold whatever the reason:
      *>   * the remaining COMMITMENT is released, because we are no
      *>     longer going to buy it;
      *>   * everything already RECEIVED stays exactly as it is - the
      *>     goods are here and the liability is real, so GRNI is
      *>     never cleared by closing an order;
      *>   * expected supplier credits stay visible and chased after
      *>     closure, so a closed order cannot hide money owed to us.
      *>
      *> A short close needs a reason and an authority.  "Nobody
      *> remembers why we stopped buying it" is not a reason, and a
      *> quantity written off the order is a decision somebody took.
      *>
      *> CA-FUNCTION values
      *>   CHECK     report what would happen, change nothing
      *>   CANCEL    cancel outstanding quantity before any receipt
      *>   SHORTCLS short close: accept the delivered quantity as
      *>             final and release the rest of the commitment
      *>   CLOSE     close a fully received and fully matched order
      *>   REOPEN    reopen a closed order with an authority
      *>
      *> REASON CODES
      *>   PCL0001 order not found or already closed
      *>   PCL0002 a reason is required
      *>   PCL0003 an authority is required for a short close
      *>   PCL0004 goods are still in quarantine awaiting inspection
      *>   PCL0005 receipts are not yet matched to invoices
      *>   PCL0006 unsupported function
      *>   PCL0007 nominal or commitment update refused
      *>   PCL0008 expected supplier credits are still outstanding
      *>   PCL0009 cancellation after receipt is not allowed - use
      *>           a short close so the received liability survives
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-10-07 RJH  CR0839 ORDER CLOSURE AND SHORT CLOSE
      *> 2007-03-19 DJM  CR1288 CLOSURE MUST NOT CLEAR GRNI
      *> 2026-10-06 STERP PO-020, AC-055
      *>================================================================
      *> REQ: PO-020, PO-017, PO-013
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  PC-BODY.
               10  PC-ORDER-NUMBER     PIC X(30).
               10  PC-LINE-NUM         PIC S9(9) COMP-5.
               10  PC-SUPPLIER         PIC X(20).
               10  PC-CLOSE-DATE       PIC 9(8).
               10  PC-REASON-CODE      PIC X(20).
               10  PC-REASON           PIC X(300).
               10  PC-AUTHORITY        PIC X(20).
               10  PC-APPROVAL-REF     PIC X(40).
               10  PC-ORDERED-QTY      PIC S9(13)V9(6).
               10  PC-RECEIVED-QTY     PIC S9(13)V9(6).
               10  PC-CANCELLED-QTY    PIC S9(13)V9(6).
               10  PC-QUARANTINE-QTY   PIC S9(13)V9(6).
               10  PC-UNIT-PRICE       PIC S9(11)V9(6).
               10  PC-MATCHED-GBP      PIC S9(13)V99.
               10  PC-RECEIVED-GBP     PIC S9(13)V99.
               10  PC-OPEN-CREDIT-GBP  PIC S9(13)V99.
               10  PC-FORCE            PIC X.
      *>         ---- answers ----
               10  PC-OUTSTAND-QTY     PIC S9(13)V9(6).
               10  PC-RELEASED-QTY     PIC S9(13)V9(6).
               10  PC-RELEASED-GBP     PIC S9(13)V99.
               10  PC-RETAINED-GRNI    PIC S9(13)V99.
               10  PC-UNMATCHED-GBP    PIC S9(13)V99.
               10  PC-NEW-STATUS       PIC X(12).
               10  PC-COMMIT-ROWS      PIC S9(4).
               10  PC-WARN-COUNT       PIC S9(4).
               10  PC-OUTCOME          PIC X(10).
