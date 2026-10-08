      *>================================================================
      *> DISHONORCA - COMMAREA FOR DISHONOR, RETURNED CUSTOMER MONEY
      *>   CA-FUNCTION: ASSESS    what would be reinstated, no effect
      *>                DISHONR   reverse the receipt and allocations
      *>   Bank fees are separate; a receipt used in several
      *>   allocations is corrected across all affected items (AR-014).
      *>   Sources: BANK (cheque/credit), ARUDD unpaid Direct Debit,
      *>   DDICA indemnity claim (AR-025).
      *>   REQ: AR-014, AR-025
      *> 1991-06-11 ORIGINAL UNPAID CHEQUE HANDLING (MVS)
      *> 2026-10-05 STERP ARUDD / DDICA SOURCES
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  DH-BODY.
               10  DH-RECEIPT-NO       PIC X(30).
               10  DH-RECEIPT-ID       PIC S9(9) COMP-5.
               10  DH-CUSTOMER         PIC X(20).
               10  DH-SOURCE           PIC X(10).
                   88  DH-FROM-BANK              VALUE "BANK".
                   88  DH-FROM-ARUDD             VALUE "ARUDD".
                   88  DH-FROM-DDICA             VALUE "DDICA".
               10  DH-REASON           PIC X(200).
               10  DH-UNPAID-CODE      PIC X(4).
               10  DH-DISHONOUR-DATE   PIC 9(8).
               10  DH-POSTING-DATE     PIC 9(8).
               10  DH-BANK-FEE         PIC S9(11)V99 COMP-3.
               10  DH-FEE-ACCOUNT      PIC X(12).
               10  DH-MANDATE-REF      PIC X(18).
               10  DH-COLLECTION-REF   PIC X(30).
               10  DH-INDEMNITY-REF    PIC X(40).
      *>       results
               10  DH-RECEIPT-GBP      PIC S9(11)V99 COMP-3.
               10  DH-REINSTATED       PIC S9(11)V99 COMP-3.
               10  DH-UNAPPLIED-BACK   PIC S9(11)V99 COMP-3.
               10  DH-ALLOC-REVERSED   PIC 9(3).
               10  DH-ITEM-CNT         PIC 9(3).
               10  DH-ITEM-TAB.
                   15  DH-ITEMS        OCCURS 20 TIMES.
                       20  DH-I-DOC-NO         PIC X(30).
                       20  DH-I-AMOUNT         PIC S9(11)V99 COMP-3.
                       20  DH-I-RESID-AFTER    PIC S9(11)V99 COMP-3.
               10  DH-JOURNAL-NO       PIC X(30).
               10  DH-EXPOSURE-AFTER   PIC S9(11)V99 COMP-3.
               10  DH-EXCEPTION-ID     PIC S9(9) COMP-5.
               10  FILLER              PIC X(40).
