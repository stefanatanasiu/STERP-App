      *>================================================================
      *> APLATEPYCA - COMMAREA for APLATEPY (a supplier's statutory late
      *>              payment interest and compensation claim, recorded
      *>              as a finance cost linked to the late invoice, and
      *>              the check of agreed terms against the statutory
      *>              and policy maximum).
      *> FUNCTIONS  : CALC      calculate interest and compensation
      *>              CLAIM     record the supplier's claim
      *>              AGREE     agree, reject or waive the claim
      *>              TERMCHK   agreed terms against the D-25 maximum
      *> 1999-04-06 original LATE PAYMENT OF COMMERCIAL DEBTS ACT 1998
      *> 2026-10-05 STERP compensation bands, finance cost account
      *>================================================================
      *> REQ: AP-030, D-25, D-28
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  LP-BODY.
               10  LP-SUPPLIER         PIC X(20).
               10  LP-CLAIM-REF        PIC X(30).
               10  LP-CLAIM-ID         PIC S9(9) COMP-5.
               10  LP-REGISTER-NUMBER  PIC X(30).
               10  LP-INVOICE-ID       PIC S9(9) COMP-5.
               10  LP-OPEN-ITEM-ID     PIC S9(9) COMP-5.
               10  LP-DOCUMENT-NUMBER  PIC X(30).
               10  LP-SUPPLIER-REF     PIC X(40).
               10  LP-CLAIM-DATE       PIC 9(8).
               10  LP-DUE-DATE         PIC 9(8).
               10  LP-PAID-DATE        PIC 9(8).
               10  LP-DAYS-LATE        PIC S9(7) SIGN LEADING SEPARATE.
               10  LP-DEBT-GBP         PIC S9(13)V99 COMP-3.
               10  LP-BASE-RATE-PCT    PIC S9(5)V9(4) COMP-3.
               10  LP-MARGIN-PCT       PIC S9(5)V9(4) COMP-3.
               10  LP-STATUTORY-PCT    PIC S9(5)V9(4) COMP-3.
               10  LP-INTEREST-GBP     PIC S9(13)V99 COMP-3.
               10  LP-COMPENSATION-GBP PIC S9(13)V99 COMP-3.
               10  LP-CLAIMED-GBP      PIC S9(13)V99 COMP-3.
               10  LP-AGREED-GBP       PIC S9(13)V99 COMP-3.
               10  LP-BAND1-GBP        PIC S9(11)V99 COMP-3.
               10  LP-BAND2-GBP        PIC S9(11)V99 COMP-3.
               10  LP-BAND3-GBP        PIC S9(11)V99 COMP-3.
               10  LP-STATUTE-REF      PIC X(40).
               10  LP-DECISION         PIC X(10).
                   88  LP-DEC-AGREE              VALUE "AGREED".
                   88  LP-DEC-REJECT             VALUE "REJECTED".
                   88  LP-DEC-WAIVE              VALUE "WAIVED".
               10  LP-STATUS           PIC X(12).
               10  LP-IS-DISPUTED      PIC X.
               10  LP-DISPUTE-REASON   PIC X(300).
               10  LP-FINANCE-ACCOUNT  PIC X(12).
               10  LP-APPROVAL-ID      PIC S9(9) COMP-5.
               10  LP-APPROVER         PIC X(20).
               10  LP-JOURNAL-NUMBER   PIC X(30).
               10  LP-TERMS-CODE       PIC X(12).
               10  LP-TERM-DAYS        PIC S9(5) SIGN LEADING SEPARATE.
               10  LP-MAX-TERM-DAYS    PIC S9(5) SIGN LEADING SEPARATE.
               10  LP-BUSINESS-SIZE    PIC X(8).
               10  LP-TERM-EXCEPTION   PIC X.
               10  LP-NOTE             PIC X(200).
