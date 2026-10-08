      *>================================================================
      *> ARDEPOSCA - COMMAREA FOR ARDEPOS, DEPOSITS AND REFUNDS
      *>   CA-FUNCTION: DEPOSIT   receive an advance (creates a tax
      *>                          point - spec F6)
      *>                APPLY     apply a deposit to an invoice
      *>                REFUND    refund the remaining balance
      *>                ENQUIRE   residual by purpose
      *>   A deposit cannot reduce exposure twice or be refunded after
      *>   the same amount has been applied (AR-013).
      *>   REQ: AR-013, AR-020
      *> 1994-02-14 RJH CR0366 CUSTOMER DEPOSIT CONTROL
      *> 2026-10-05 STERP VAT TAX POINT ON ADVANCE
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  DP-BODY.
               10  DP-CUSTOMER         PIC X(20).
               10  DP-DEPOSIT-REF      PIC X(30).
               10  DP-DEPOSIT-ID       PIC S9(9) COMP-5.
               10  DP-PURPOSE          PIC X(12).
                   88  DP-ADVANCE                VALUE "ADVANCE".
                   88  DP-UNAPPLIED              VALUE "UNAPPLIED".
                   88  DP-REFUNDABLE             VALUE "REFUNDABLE".
               10  DP-METHOD           PIC X(10).
               10  DP-BANK-ACCOUNT     PIC X(20).
               10  DP-DOC-DATE         PIC 9(8).
               10  DP-POSTING-DATE     PIC 9(8).
               10  DP-CURRENCY         PIC X(3).
               10  DP-FX-RATE          PIC S9(9)V9(8) COMP-3.
               10  DP-AMOUNT           PIC S9(11)V99 COMP-3.
               10  DP-AMOUNT-GBP       PIC S9(11)V99 COMP-3.
               10  DP-VAT-CODE         PIC X(4).
               10  DP-VAT-RATE         PIC S9(3)V9(4) COMP-3.
               10  DP-VAT-GBP          PIC S9(11)V99 COMP-3.
               10  DP-NET-GBP          PIC S9(11)V99 COMP-3.
               10  DP-CLEARED          PIC X.
      *>       application / refund
               10  DP-APPLY-TO         PIC X(30).
               10  DP-APPLY-ITEM-ID    PIC S9(9) COMP-5.
               10  DP-APPLY-AMOUNT     PIC S9(11)V99 COMP-3.
               10  DP-REFUND-AMOUNT    PIC S9(11)V99 COMP-3.
               10  DP-REFUND-REF       PIC X(30).
               10  DP-REASON           PIC X(200).
               10  DP-APPROVAL-ID      PIC S9(9) COMP-5.
      *>       results
               10  DP-APPLIED-TOTAL    PIC S9(11)V99 COMP-3.
               10  DP-REFUNDED-TOTAL   PIC S9(11)V99 COMP-3.
               10  DP-RESIDUAL         PIC S9(11)V99 COMP-3.
               10  DP-OPEN-ITEM-ID     PIC S9(9) COMP-5.
               10  DP-ALLOC-ID         PIC S9(9) COMP-5.
               10  DP-JOURNAL-NO       PIC X(30).
               10  FILLER              PIC X(40).
