      *>================================================================
      *> APALLOCCA - COMMAREA for APALLOC (settlement allocation of
      *>             cash, credits, advances and valid discounts to
      *>             supplier open items, with realised exchange
      *>             differences).
      *> FUNCTIONS : ALLOCATE  allocate a source item to target items
      *>             UNALLOC   reverse an allocation
      *>             RESIDUAL  residual enquiry for one open item
      *>             FXREAL    realised exchange difference only
      *> 1991-08-14 original PURCHASE LEDGER CASH ALLOCATION
      *> 2026-10-05 STERP realised FX kept out of price variance
      *>================================================================
      *> REQ: AP-014, AP-018, AP-016, D-22
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  AL-BODY.
               10  AL-SUPPLIER         PIC X(20).
               10  AL-SOURCE-TYPE      PIC X(10).
                   88  AL-SRC-PAYMENT            VALUE "PAYMENT".
                   88  AL-SRC-CREDIT             VALUE "CREDIT".
                   88  AL-SRC-ADVANCE            VALUE "ADVANCE".
                   88  AL-SRC-ADJUST             VALUE "ADJUST".
               10  AL-SOURCE-ITEM-ID   PIC S9(9) COMP-5.
               10  AL-SOURCE-NUMBER    PIC X(30).
               10  AL-SOURCE-CCY       PIC X(3).
               10  AL-SOURCE-AMOUNT    PIC S9(13)V99 COMP-3.
               10  AL-SOURCE-RESIDUAL  PIC S9(13)V99 COMP-3.
               10  AL-SOURCE-RATE      PIC S9(9)V9(8) COMP-3.
               10  AL-SETTLE-RATE      PIC S9(9)V9(8) COMP-3.
               10  AL-INSTRUCTION-ID   PIC S9(9) COMP-5.
               10  AL-REMITTANCE-REF   PIC X(30).
               10  AL-EFFECTIVE-DATE   PIC 9(8).
               10  AL-DIFF-TOLERANCE   PIC S9(11)V99 COMP-3.
               10  AL-APPLIED-TOTAL    PIC S9(13)V99 COMP-3.
               10  AL-APPLIED-GBP      PIC S9(13)V99 COMP-3.
               10  AL-DISCOUNT-GBP     PIC S9(13)V99 COMP-3.
               10  AL-DIFFERENCE-GBP   PIC S9(13)V99 COMP-3.
               10  AL-REALISED-FX-GBP  PIC S9(13)V99 COMP-3.
               10  AL-FX-POSITION      PIC X(10).
               10  AL-UNAPPLIED        PIC S9(13)V99 COMP-3.
               10  AL-ALLOCATION-ID    PIC S9(9) COMP-5.
               10  AL-REVERSE-ID       PIC S9(9) COMP-5.
               10  AL-REVERSE-AMOUNT   PIC S9(13)V99 COMP-3.
               10  AL-BANK-RECONCILED  PIC X.
               10  AL-POSTED           PIC X.
               10  AL-JOURNAL-NUMBER   PIC X(30).
               10  AL-TARGET-CNT       PIC 9(3).
               10  AL-TARGET-TABLE.
                   15  AL-TARGET OCCURS 30 TIMES.
                       20  AL-T-ITEM-ID    PIC S9(9) COMP-5.
                       20  AL-T-DOCUMENT   PIC X(30).
                       20  AL-T-CURRENCY   PIC X(3).
                       20  AL-T-RESIDUAL   PIC S9(13)V99 COMP-3.
                       20  AL-T-APPLY      PIC S9(13)V99 COMP-3.
                       20  AL-T-APPLY-GBP  PIC S9(13)V99 COMP-3.
                       20  AL-T-DISCOUNT   PIC S9(11)V99 COMP-3.
                       20  AL-T-DIFF       PIC S9(11)V99 COMP-3.
                       20  AL-T-FX-GBP     PIC S9(11)V99 COMP-3.
                       20  AL-T-NEW-RESID  PIC S9(13)V99 COMP-3.
                       20  AL-T-STATUS     PIC X(12).
                       20  AL-T-MESSAGE    PIC X(80).
               10  AL-REASON           PIC X(200).
               10  AL-NOTE             PIC X(200).
