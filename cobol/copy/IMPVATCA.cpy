      *>================================================================
      *> IMPVATCA - COMMAREA for IMPVAT, customs import entries.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> An import entry is the customs declaration that accompanies
      *> imported goods.  It carries two quite different amounts and
      *> STERP never confuses them (PO-025):
      *>
      *>   CUSTOMS DUTY is an irrecoverable tax that forms part of the
      *>   cost of the goods, so it is capitalised into stock - or
      *>   charged to cost of sales for the units already sold - and
      *>   credited to goods received not invoiced until the customs
      *>   agent invoices for it.
      *>
      *>   IMPORT VAT is recoverable and is NEVER landed cost.  Under
      *>   postponed VAT accounting both legs are posted on the same
      *>   journal: the output side is declared in box 1 and the input
      *>   side recovered in box 4 of the same return, so the net
      *>   effect on profit and on cash is nil.  The monthly postponed
      *>   import VAT statement is the evidence; a missing statement
      *>   leaves the entry unreconciled rather than estimated.
      *>
      *> CA-FUNCTION values
      *>   CALC      duty and VAT arithmetic only, no database effect
      *>   DECLARE   record the entry and its lines
      *>   POST      capitalise the duty and post the import VAT
      *>   RECONCILE match a monthly statement (PVA or C79) to the
      *>             entries declared for that period
      *>
      *> IM-VAT-METHOD values
      *>   PVA        postponed VAT accounting, both legs on one
      *>              journal, no cash paid at the border
      *>   BORDER     VAT paid at the border and recovered on the C79
      *>   DEFERMENT  VAT and duty deferred to a duty deferment
      *>              account, settled monthly
      *>
      *> REASON CODES
      *>   IMP0001 movement reference number is required
      *>   IMP0002 commodity code and country of origin are required
      *>   IMP0003 customs value is zero or negative
      *>   IMP0004 duty rate is out of range
      *>   IMP0005 unsupported function
      *>   IMP0006 nominal posting refused
      *>   IMP0007 entry not found
      *>   IMP0008 statement differs from the declared entries
      *>   IMP0009 a preference claim needs its proof of origin
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-11-05 RJH  CR0841 IMPORT ENTRIES AND DUTY
      *> 2021-01-01 TMW  BREXIT - POSTPONED IMPORT VAT ACCOUNTING
      *> 2026-10-06 STERP PO-025 DUTY INTO LANDED COST
      *>================================================================
      *> REQ: PO-025, PO-015, PO-017
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  IM-BODY.
               10  IM-ENTRY-REF        PIC X(30).
               10  IM-MRN              PIC X(40).
               10  IM-ENTRY-DATE       PIC 9(8).
               10  IM-POSTING-DATE     PIC 9(8).
               10  IM-AGENT-CODE       PIC X(20).
               10  IM-SUPPLIER         PIC X(20).
               10  IM-ORDER-NUMBER     PIC X(30).
               10  IM-RECEIPT-REF      PIC X(30).
               10  IM-VAT-METHOD       PIC X(10).
                   88  IM-PVA                    VALUE "PVA".
                   88  IM-BORDER                 VALUE "BORDER".
                   88  IM-DEFERMENT              VALUE "DEFERMENT".
               10  IM-DEFERMENT-ACC    PIC X(20).
               10  IM-VAT-PERIOD-KEY   PIC X(10).
               10  IM-VAT-RATE-PCT     PIC S9(5)V9(4).
               10  IM-STATEMENT-REF    PIC X(40).
               10  IM-STATEMENT-DUTY   PIC S9(13)V99.
               10  IM-STATEMENT-VAT    PIC S9(13)V99.
               10  IM-CONSUMED-PCT     PIC S9(5)V9(4).
               10  IM-LINE-CNT         PIC S9(4).
               10  IM-LINE-TABLE.
                   15  IM-LINE OCCURS 15 TIMES.
                       20  IM-L-NUM        PIC S9(9) COMP-5.
                       20  IM-L-ITEM       PIC X(40).
                       20  IM-L-RCPT-LINE  PIC S9(9) COMP-5.
                       20  IM-L-COMMODITY  PIC X(10).
                       20  IM-L-ORIGIN     PIC X(2).
                       20  IM-L-PREFERENCE PIC X.
                       20  IM-L-PROOF-REF  PIC X(40).
                       20  IM-L-QUANTITY   PIC S9(13)V9(6).
                       20  IM-L-WEIGHT-KG  PIC S9(13)V9(6).
                       20  IM-L-CUST-VALUE PIC S9(13)V99.
                       20  IM-L-DUTY-PCT   PIC S9(5)V9(4).
      *>                 ---- answers, per line ----
                       20  IM-L-DUTY-GBP   PIC S9(13)V99.
                       20  IM-L-VAT-GBP    PIC S9(13)V99.
                       20  IM-L-LANDED-GBP PIC S9(13)V99.
                       20  IM-L-OUTCOME    PIC X(10).
      *>         ---- answers, header ----
               10  IM-ENTRY-ID         PIC S9(9).
               10  IM-CUSTOMS-VAL-GBP  PIC S9(13)V99.
               10  IM-DUTY-GBP         PIC S9(13)V99.
               10  IM-VAT-GBP          PIC S9(13)V99.
               10  IM-DUTY-TO-STOCK    PIC S9(13)V99.
               10  IM-DUTY-TO-COGS     PIC S9(13)V99.
               10  IM-VAT-BOX1-GBP     PIC S9(13)V99.
               10  IM-VAT-BOX4-GBP     PIC S9(13)V99.
               10  IM-NET-PROFIT-GBP   PIC S9(13)V99.
               10  IM-STMT-DIFF-GBP    PIC S9(13)V99.
               10  IM-JOURNAL-NUMBER   PIC X(30).
               10  IM-VAT-JOURNAL      PIC X(30).
               10  IM-STATEMENT-STATUS PIC X(12).
               10  IM-OUTCOME          PIC X(10).
