      *>================================================================
      *> SVCACCCA - COMMAREA for SVCACC, service and milestone
      *>            acceptance.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> A service has no goods inward note, so the evidence that the
      *> work was done IS the acceptance (PO-012).  Acceptance, and not
      *> the supplier's invoice, is what creates the accrual: the cost
      *> belongs to the period in which the service was delivered.
      *>
      *> Nothing here touches stock.  A service receipt that created a
      *> stock quantity would put a quantity on an item that cannot be
      *> counted, so SVCACC never calls STKMOVE.
      *>
      *> CA-FUNCTION values
      *>   ACCEPT   record an acceptance and accrue the cost
      *>   REVERSE  reverse an acceptance made in error
      *>   ENQUIRE  report accepted value and remaining commitment
      *>
      *> SA-BASIS values
      *>   QUANTITY  a quantity of a measured service (hours, visits)
      *>   MILESTONE a named deliverable with its own reference
      *>   VALUE     a stated value or a percentage of the order
      *>
      *> AC-054: accepting 40% of a 2,000.00 approved service order
      *> accrues 800.00 and leaves 1,200.00 of commitment.
      *>
      *> REASON CODES
      *>   SVC0001 order line not found or not a service line
      *>   SVC0002 acceptance value is zero or negative
      *>   SVC0003 acceptance exceeds the remaining commitment
      *>   SVC0004 evidence of delivery is required
      *>   SVC0005 a milestone acceptance must name its milestone
      *>   SVC0006 unsupported function
      *>   SVC0007 nominal posting refused
      *>   SVC0008 acceptance not found, or already reversed
      *>   SVC0009 the acceptor may not accept their own request
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1999-01-26 RJH  CR0851 SERVICE ORDERS AND ACCEPTANCE
      *> 2005-05-04 DJM  CR1207 MILESTONE ACCEPTANCE FOR PROJECTS
      *> 2026-10-06 STERP PO-012 ACCRUAL THROUGH POPOST
      *>================================================================
      *> REQ: PO-012, PO-017
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SA-BODY.
               10  SA-ACCEPT-REF       PIC X(30).
               10  SA-ORDER-NUMBER     PIC X(30).
               10  SA-ORDER-LINE       PIC S9(9) COMP-5.
               10  SA-SUPPLIER         PIC X(20).
               10  SA-DESCRIPTION      PIC X(200).
               10  SA-BASIS            PIC X(10).
                   88  SA-BY-QUANTITY            VALUE "QUANTITY".
                   88  SA-BY-MILESTONE           VALUE "MILESTONE".
                   88  SA-BY-VALUE               VALUE "VALUE".
               10  SA-ORDER-VALUE      PIC S9(13)V99.
               10  SA-ORDER-VALUE-GBP  PIC S9(13)V99.
               10  SA-ALREADY-ACC-GBP  PIC S9(13)V99.
               10  SA-ORDER-QTY        PIC S9(13)V9(6).
               10  SA-UNIT-PRICE       PIC S9(11)V9(6).
               10  SA-ACCEPTED-QTY     PIC S9(13)V9(6).
               10  SA-ACCEPT-PCT       PIC S9(5)V9(4).
               10  SA-ACCEPT-VALUE     PIC S9(13)V99.
               10  SA-MILESTONE-REF    PIC X(40).
               10  SA-MILESTONE-VALUE  PIC S9(13)V99.
               10  SA-ACCEPT-DATE      PIC 9(8).
               10  SA-POSTING-DATE     PIC 9(8).
               10  SA-CURRENCY         PIC X(3).
               10  SA-FX-RATE          PIC S9(9)V9(8).
               10  SA-EVIDENCE         PIC X(300).
               10  SA-ACCEPTED-BY      PIC X(20).
               10  SA-REQUESTED-BY     PIC X(20).
               10  SA-EXPENSE-ACCOUNT  PIC X(12).
               10  SA-COST-CENTRE      PIC X(20).
               10  SA-PROJECT          PIC X(20).
               10  SA-JOB-REF          PIC X(20).
               10  SA-REVERSE-REF      PIC X(30).
               10  SA-REVERSE-REASON   PIC X(200).
      *>         ---- answers ----
               10  SA-ACCEPT-GBP       PIC S9(13)V99.
               10  SA-ACCRUED-GBP      PIC S9(13)V99.
               10  SA-REMAIN-COMM-GBP  PIC S9(13)V99.
               10  SA-TOTAL-ACC-GBP    PIC S9(13)V99.
               10  SA-ACCEPTANCE-ID    PIC S9(17).
               10  SA-JOURNAL-NUMBER   PIC X(30).
               10  SA-STOCK-QTY        PIC S9(13)V9(6).
               10  SA-STATUS           PIC X(12).
               10  SA-OUTCOME          PIC X(10).
