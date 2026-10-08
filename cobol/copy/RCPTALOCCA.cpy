      *>================================================================
      *> RCPTALOCCA - COMMAREA FOR RCPTALOC, RECEIPTS AND ALLOCATION
      *>   CA-FUNCTION: RECEIPT   record one receipt (cash effect)
      *>                CONFIRM   turn unconfirmed evidence into funds
      *>                PREVIEW   residual preview, no business effect
      *>                ALLOCATE  apply a receipt/credit to open items
      *>                UNALLOC   authorised allocation correction
      *>   Allocation alone has no second cash effect (AR-010).
      *>   REQ: AR-009 to AR-012, AR-019
      *> 1991-06-11 ORIGINAL CASH MATCHING PORT (MVS)
      *> 1999-11-30 KMP Y2K - 4 DIGIT YEARS
      *> 2026-10-05 STERP CROSS CURRENCY AND WRITE OFF COMPONENTS
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  RA-BODY.
               10  RA-RECEIPT-NO       PIC X(30).
               10  RA-RECEIPT-ID       PIC S9(9) COMP-5.
               10  RA-CUSTOMER         PIC X(20).
               10  RA-PAYER            PIC X(20).
               10  RA-PAYER-NAME       PIC X(120).
               10  RA-METHOD           PIC X(10).
               10  RA-BANK-ACCOUNT     PIC X(20).
               10  RA-RECEIPT-DATE     PIC 9(8).
               10  RA-VALUE-DATE       PIC 9(8).
               10  RA-POSTING-DATE     PIC 9(8).
               10  RA-CURRENCY         PIC X(3).
               10  RA-FX-RATE          PIC S9(9)V9(8) COMP-3.
               10  RA-GROSS            PIC S9(11)V99 COMP-3.
               10  RA-GROSS-GBP        PIC S9(11)V99 COMP-3.
               10  RA-BANK-FEE         PIC S9(11)V99 COMP-3.
               10  RA-CONFIRM-STATE    PIC X(12).
                   88  RA-EXPECTED               VALUE "EXPECTED".
                   88  RA-CONFIRMED              VALUE "CONFIRMED".
               10  RA-BANK-REF         PIC X(60).
               10  RA-EXTERNAL-REF     PIC X(60).
               10  RA-DD-COLLECT-ID    PIC S9(9) COMP-5.
               10  RA-UNIDENTIFIED     PIC X.
               10  RA-EXCEPTION-ID     PIC S9(9) COMP-5.
      *>       allocation instructions / results
               10  RA-EFFECTIVE-DATE   PIC 9(8).
               10  RA-SOURCE-TYPE      PIC X(10).
                   88  RA-SRC-RECEIPT            VALUE "RECEIPT".
                   88  RA-SRC-CREDIT             VALUE "CREDIT".
                   88  RA-SRC-DEPOSIT            VALUE "DEPOSIT".
               10  RA-SOURCE-ITEM-ID   PIC S9(9) COMP-5.
               10  RA-SOURCE-DOC       PIC X(30).
               10  RA-SOURCE-AVAIL     PIC S9(11)V99 COMP-3.
               10  RA-CROSS-CCY-RATE   PIC S9(9)V9(8) COMP-3.
               10  RA-ALLOC-CNT        PIC 9(3).
               10  RA-ALLOC-TAB.
                   15  RA-ALLOCS       OCCURS 20 TIMES.
                       20  RA-A-ITEM-ID        PIC S9(9) COMP-5.
                       20  RA-A-DOC-NO         PIC X(30).
                       20  RA-A-CURRENCY       PIC X(3).
                       20  RA-A-APPLY          PIC S9(11)V99 COMP-3.
                       20  RA-A-DISCOUNT       PIC S9(11)V99 COMP-3.
                       20  RA-A-WRITEOFF       PIC S9(11)V99 COMP-3.
                       20  RA-A-WO-REASON      PIC X(100).
                       20  RA-A-RESID-BEFORE   PIC S9(11)V99 COMP-3.
                       20  RA-A-RESID-AFTER    PIC S9(11)V99 COMP-3.
                       20  RA-A-FX-REALISED    PIC S9(11)V99 COMP-3.
                       20  RA-A-ALLOC-ID       PIC S9(9) COMP-5.
                       20  RA-A-KIND           PIC X(10).
                       20  RA-A-MESSAGE        PIC X(80).
               10  RA-TOT-APPLIED      PIC S9(11)V99 COMP-3.
               10  RA-TOT-DISCOUNT     PIC S9(11)V99 COMP-3.
               10  RA-TOT-WRITEOFF     PIC S9(11)V99 COMP-3.
               10  RA-TOT-FX           PIC S9(11)V99 COMP-3.
               10  RA-RESIDUAL         PIC S9(11)V99 COMP-3.
               10  RA-UNALLOCATED      PIC S9(11)V99 COMP-3.
               10  RA-WRITEOFF-LIMIT   PIC S9(11)V99 COMP-3.
               10  RA-DISC-GRACE-DAYS  PIC S9(5).
      *>       correction (UNALLOC) - history is preserved (AR-011)
               10  RA-ALLOC-ID         PIC S9(9) COMP-5.
               10  RA-CORRECT-REASON   PIC X(200).
               10  RA-CORRECT-DATE     PIC 9(8).
               10  RA-REVERSED-CNT     PIC 9(3).
               10  RA-JOURNAL-NO       PIC X(30).
               10  FILLER              PIC X(40).
