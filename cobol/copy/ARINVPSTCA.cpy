      *>================================================================
      *> ARINVPSTCA - COMMAREA FOR ARINVPST, SALES INVOICE POSTING
      *>   CA-FUNCTION: VALIDATE  VAT content and commercial checks only
      *>                POST      post one receivable + revenue + VAT
      *>                PROFORMA  produce a pro forma (no AR, no VAT)
      *>   CALLED BY : SALES ORDER INVOICING, DIRECT INVOICE ENTRY,
      *>               .NET CLIENT, AR BATCH
      *>   REQ: AR-003, AR-004, AR-005, AR-023, AR-029
      *> 1991-05-02 ORIGINAL SALES LEDGER PORT (MVS)
      *> 2021-01-01 BREXIT - VAT LOCATION GB / XI / EU / ROW
      *> 2026-10-05 STERP SIMPLIFIED INVOICE LIMIT GBP 250
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  AI-BODY.
               10  AI-DOC-TYPE         PIC X(12).
                   88  AI-IS-INVOICE             VALUE "INVOICE".
                   88  AI-IS-PROFORMA            VALUE "PROFORMA".
               10  AI-INVOICE-NO       PIC X(30).
               10  AI-CUSTOMER         PIC X(20).
               10  AI-INVOICE-CUST     PIC X(20).
               10  AI-DOC-DATE         PIC 9(8).
               10  AI-TAX-POINT        PIC 9(8).
               10  AI-POSTING-DATE     PIC 9(8).
               10  AI-DUE-DATE         PIC 9(8).
               10  AI-TERMS-CODE       PIC X(12).
               10  AI-TERMS-SNAPSHOT   PIC X(100).
               10  AI-TERMS-SOURCE     PIC X(20).
               10  AI-DISCOUNT-PCT     PIC S9(3)V9(4) COMP-3.
               10  AI-DISCOUNT-DATE    PIC 9(8).
               10  AI-CURRENCY         PIC X(3).
               10  AI-FX-RATE          PIC S9(9)V9(8) COMP-3.
               10  AI-FX-RATE-DATE     PIC 9(8).
               10  AI-SOURCE           PIC X(10).
                   88  AI-FROM-DESPATCH          VALUE "DESPATCH".
                   88  AI-FROM-DIRECT            VALUE "DIRECT".
                   88  AI-FROM-SERVICE           VALUE "SERVICE".
               10  AI-ORDER-REF        PIC X(40).
               10  AI-SO-NUMBER        PIC X(30).
               10  AI-DESPATCH-REF     PIC X(30).
               10  AI-SALESPERSON      PIC X(20).
      *>       snapshotted VAT invoice content (AR-023, section 10)
               10  AI-BILL-NAME        PIC X(120).
               10  AI-BILL-ADDRESS     PIC X(300).
               10  AI-DELIVERY-ADDR    PIC X(300).
               10  AI-CUST-VAT-NO      PIC X(15).
               10  AI-OWN-VAT-NO       PIC X(15).
               10  AI-VAT-LOCATION     PIC X(3).
               10  AI-SIMPLIFIED       PIC X.
               10  AI-SPECIAL-WORDING  PIC X(300).
               10  AI-EVIDENCE-REF     PIC X(60).
               10  AI-DEPOSIT-APPLY    PIC S9(11)V99 COMP-3.
      *>       totals (document currency and sterling)
               10  AI-NET              PIC S9(11)V99 COMP-3.
               10  AI-VAT              PIC S9(11)V99 COMP-3.
               10  AI-GROSS            PIC S9(11)V99 COMP-3.
               10  AI-NET-GBP          PIC S9(11)V99 COMP-3.
               10  AI-VAT-GBP          PIC S9(11)V99 COMP-3.
               10  AI-GROSS-GBP        PIC S9(11)V99 COMP-3.
      *>       lines
               10  AI-LINE-CNT         PIC 9(3).
               10  AI-LINE-TAB.
                   15  AI-LINES        OCCURS 25 TIMES.
                       20  AI-L-ITEM           PIC X(40).
                       20  AI-L-DESC           PIC X(60).
                       20  AI-L-QTY            PIC S9(9)V9(6) COMP-3.
                       20  AI-L-UOM            PIC X(10).
                       20  AI-L-PRICE          PIC S9(11)V9(6) COMP-3.
                       20  AI-L-DISC-PCT       PIC S9(3)V9(4) COMP-3.
                       20  AI-L-CHARGE         PIC S9(11)V99 COMP-3.
                       20  AI-L-NET            PIC S9(11)V99 COMP-3.
                       20  AI-L-VAT-CODE       PIC X(4).
                       20  AI-L-VAT-RATE       PIC S9(3)V9(4) COMP-3.
                       20  AI-L-VAT-AMT        PIC S9(11)V99 COMP-3.
                       20  AI-L-REV-ACCOUNT    PIC X(12).
                       20  AI-L-COST-CENTRE    PIC X(20).
                       20  AI-L-PROJECT        PIC X(20).
                       20  AI-L-SO-LINE        PIC S9(9) COMP-5.
                       20  AI-L-AVAIL-QTY      PIC S9(9)V9(6) COMP-3.
                       20  AI-L-MESSAGE        PIC X(80).
      *>       VAT analysis returned per code
               10  AI-VAT-CNT          PIC 9(2).
               10  AI-VAT-TAB.
                   15  AI-VATS         OCCURS 6 TIMES.
                       20  AI-V-CODE           PIC X(4).
                       20  AI-V-RATE           PIC S9(3)V9(4) COMP-3.
                       20  AI-V-NET-GBP        PIC S9(11)V99 COMP-3.
                       20  AI-V-VAT-GBP        PIC S9(11)V99 COMP-3.
                       20  AI-V-LIABILITY      PIC X(12).
      *>       results
               10  AI-INVOICE-ID       PIC S9(9) COMP-5.
               10  AI-OPEN-ITEM-ID     PIC S9(9) COMP-5.
               10  AI-JOURNAL-NO       PIC X(30).
               10  AI-DUPLICATE        PIC X.
               10  AI-MISSING-CNT      PIC 9(2).
               10  AI-MISSING-TAB.
                   15  AI-MISSING      OCCURS 10 TIMES
                                       PIC X(40).
               10  FILLER              PIC X(40).
