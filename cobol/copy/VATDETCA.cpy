      *>================================================================
      *> VATDETCA - COMMAREA for VATDET, VAT code lookup, rate by tax
      *>            point, line level determination and the special
      *>            mechanisms (FIN-016, FIN-017, FIN-028, FIN-029).
      *> Functions: RATE     effective rate and treatment at a tax point
      *>            LINE     VAT on one line, rounded per line
      *>            DOC      total a document from its lines
      *>            SELFACC  reverse charge / postponed import VAT legs
      *> 1994-08-30 A P WHITTAKER   2021-01-01 BREXIT PVA AND DRC
      *> 2026-10-05 STERP PER LINE ROUNDING (AC-015)
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  VD-BODY.
               10  VD-VAT-CODE         PIC X(4).
               10  VD-TAX-POINT        PIC 9(8).
               10  VD-DIRECTION        PIC X(6).
               10  VD-NET-AMOUNT       PIC S9(11)V99 COMP-3.
               10  VD-DISCOUNT-PCT     PIC S9(5)V9(4) COMP-3.
               10  VD-PLACE-OF-SUPPLY  PIC X(20).
               10  VD-PARTY-VAT-NO     PIC X(15).
               10  VD-EVIDENCE-REF     PIC X(60).
               10  VD-CURRENCY         PIC X(3).
               10  VD-FX-RATE          PIC S9(10)V9(8) COMP-3.
               10  VD-LINE-CNT         PIC 9(3).
               10  VD-LINES OCCURS 30 TIMES.
                   15  VD-L-VAT-CODE   PIC X(4).
                   15  VD-L-NET        PIC S9(11)V99 COMP-3.
                   15  VD-L-RATE       PIC S9(5)V9(4) COMP-3.
                   15  VD-L-VAT        PIC S9(11)V99 COMP-3.
                   15  VD-L-GROSS      PIC S9(11)V99 COMP-3.
      *>     ---- answers ----
               10  VD-RATE-PCT         PIC S9(5)V9(4) COMP-3.
               10  VD-RATE-FROM        PIC 9(8).
               10  VD-VAT-AMOUNT       PIC S9(11)V99 COMP-3.
               10  VD-VAT-AMOUNT-GBP   PIC S9(11)V99 COMP-3.
               10  VD-NET-AMOUNT-GBP   PIC S9(11)V99 COMP-3.
               10  VD-GROSS-AMOUNT     PIC S9(11)V99 COMP-3.
               10  VD-TOTAL-NET        PIC S9(13)V99 COMP-3.
               10  VD-TOTAL-VAT        PIC S9(13)V99 COMP-3.
               10  VD-TOTAL-GROSS      PIC S9(13)V99 COMP-3.
               10  VD-LIABILITY        PIC X(12).
               10  VD-MECHANISM        PIC X(12).
               10  VD-RECOVERABLE      PIC X.
               10  VD-SELF-ACCOUNTS    PIC X.
               10  VD-OUT-VAT-BOX      PIC 9(2).
               10  VD-IN-VAT-BOX       PIC 9(2).
               10  VD-OUT-NET-BOX      PIC 9(2).
               10  VD-IN-NET-BOX       PIC 9(2).
               10  VD-VAT-BOX          PIC 9(2).
               10  VD-NET-BOX          PIC 9(2).
               10  VD-RECOVERABLE-VAT  PIC S9(11)V99 COMP-3.
               10  VD-SELF-OUTPUT-VAT  PIC S9(11)V99 COMP-3.
               10  VD-SELF-INPUT-VAT   PIC S9(11)V99 COMP-3.
               10  VD-INVOICE-WORDING  PIC X(200).
               10  VD-EVIDENCE-REQD    PIC X(100).
               10  VD-VAT-PERIOD-KEY   PIC X(10).
               10  VD-SCHEME           PIC X(12).
               10  VD-IS-LATE-ENTRY    PIC X.
               10  VD-TAX-POINT-KIND   PIC X(8).
