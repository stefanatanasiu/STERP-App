      *>================================================================
      *> APCRDLNKCA - COMMAREA for APCRDLNK (supplier credits linked to
      *>              physical returns, prepayments/advances, refunds
      *>              and agreed legal set-off).
      *> FUNCTIONS  : CRDLINK   link a credit note to a supplier return
      *>              RETSTAT   remaining return quantity and value
      *>              ADVANCE   record a supplier advance (asset)
      *>              APPLYADV  apply an advance to an invoice
      *>              REFUND    refund an unapplied advance or debit AP
      *>              SETOFF    record an agreed legal set-off
      *> 1993-09-08 original SUPPLIER CREDIT AND DEPOSIT CONTROL
      *> 2026-10-05 STERP return clearing, set-off approval
      *>================================================================
      *> REQ: AP-010, AP-017, AP-022
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  CL-BODY.
               10  CL-SUPPLIER         PIC X(20).
               10  CL-CREDIT-REGISTER  PIC X(30).
               10  CL-CREDIT-ID        PIC S9(9) COMP-5.
               10  CL-RETURN-REF       PIC X(30).
               10  CL-RETURN-LINE      PIC 9(5).
               10  CL-RMA-REF          PIC X(40).
               10  CL-ITEM             PIC X(40).
               10  CL-RETURNED-QTY     PIC S9(11)V9(6) COMP-3.
               10  CL-CREDITED-QTY     PIC S9(11)V9(6) COMP-3.
               10  CL-REMAINING-QTY    PIC S9(11)V9(6) COMP-3.
               10  CL-EXPECTED-GBP     PIC S9(13)V99 COMP-3.
               10  CL-CREDITED-GBP     PIC S9(13)V99 COMP-3.
               10  CL-REMAINING-GBP    PIC S9(13)V99 COMP-3.
               10  CL-CLEARING-GBP     PIC S9(13)V99 COMP-3.
               10  CL-PRICE-ONLY       PIC X.
               10  CL-ADVANCE-NUMBER   PIC X(30).
               10  CL-ADVANCE-ID       PIC S9(9) COMP-5.
               10  CL-ADVANCE-CCY      PIC X(3).
               10  CL-ADVANCE-AMT      PIC S9(13)V99 COMP-3.
               10  CL-ADVANCE-GBP      PIC S9(13)V99 COMP-3.
               10  CL-ADVANCE-RESID    PIC S9(13)V99 COMP-3.
               10  CL-APPLY-AMT        PIC S9(13)V99 COMP-3.
               10  CL-TARGET-NUMBER    PIC X(30).
               10  CL-TARGET-ID        PIC S9(9) COMP-5.
               10  CL-TARGET-RESIDUAL  PIC S9(13)V99 COMP-3.
               10  CL-REFUND-TYPE      PIC X(10).
                   88  CL-RF-ADVANCE             VALUE "ADVANCE".
                   88  CL-RF-DEBITBAL            VALUE "DEBITBAL".
               10  CL-REFUND-AMT       PIC S9(13)V99 COMP-3.
               10  CL-SETOFF-CUSTOMER  PIC X(20).
               10  CL-SETOFF-AMT       PIC S9(13)V99 COMP-3.
               10  CL-SETOFF-AGREED    PIC X.
               10  CL-APPROVAL-ID      PIC S9(9) COMP-5.
               10  CL-APPROVER         PIC X(20).
               10  CL-EFFECTIVE-DATE   PIC 9(8).
               10  CL-ALLOCATION-ID    PIC S9(9) COMP-5.
               10  CL-STOCK-AFFECTED   PIC X.
               10  CL-REASON           PIC X(200).
               10  CL-NOTE             PIC X(200).
