      *>================================================================
      *> SOACCEPTCA - COMMAREA for SOACCEPT, order completeness,
      *>              commercial acceptance, release and deposits.
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT
      *>
      *> An incomplete draft can never be treated as accepted demand.
      *> VALIDATE returns the whole blocker list in SA-BLOCKERS so the
      *> user sees every missing condition at once rather than one at
      *> a time (SO-002).
      *>
      *> CA-FUNCTION values
      *>   VALIDATE  completeness and eligibility only.  No business
      *>             effect; SA-BLOCKER-CNT and SA-BLOCKERS carry the
      *>             findings and CA-RETURN-CODE is 08 when at least
      *>             one BLOCKER was raised.
      *>   ACCEPT    VALIDATE, then the credit position from CREDCHK,
      *>             then CommercialStatus ACCEPTED and ReleaseStatus
      *>             RELEASED, CREDITHOLD or COMMERCIALHOLD with
      *>             SA-HOLD-REASON, inside one BEGN..COMT, with a
      *>             core.AuditLog row (SO-003, SO-007).
      *>   RELEASE   re-run the CURRENT commercial, credit, supply and
      *>             approval checks.  A previously accepted order IS
      *>             held when the customer's position has materially
      *>             worsened.  Printing or reprinting an
      *>             acknowledgement or a pick document does NOT
      *>             release a held order, and RELEASE never reserves
      *>             stock by itself (SO-011).
      *>   DEPOSIT   record the required deposit on the order and hand
      *>             the receipt to ARDEPOS.  A deposit is not sales
      *>             revenue and only cleared funds count (SO-019).
      *>
      *> SA-B-SEVERITY is "BLOCKER" (08, no acceptance) or "WARNING"
      *> (04, proceed with review).  A policy-governed authority in
      *> SA-ITEM-AUTHORITY downgrades an obsolete or restricted item
      *> from a blocker to a warning - nothing else does.
      *>
      *> REASON CODES
      *>   SOA0001 a company, a customer and an order are required
      *>   SOA0002 a mandatory commercial field is missing
      *>   SOA0003 item or revision is not eligible for sale
      *>   SOA0004 price source or VAT code missing on a line
      *>   SOA0005 the line unit is not compatible with the item
      *>   SOA0006 order not found
      *>   SOA0007 the order has no lines
      *>   SOA0008 credit hold - see SA-HOLD-REASON (06)
      *>   SOA0009 unsupported function
      *>   SOA0010 the order is cancelled or already completed
      *>   SOA0011 an Incoterms rule and named place are required for
      *>           an international sale
      *>   SOA0012 a deposit must be a positive cleared amount
      *>   SOA0013 printing does not release a held order
      *>   SOA0014 the order must be accepted before it is released
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-09-12 RJH  CR0301 ORDER ACCEPTANCE AND CREDIT RELEASE
      *> 1999-11-30 KMP  Y2K - ORDER DATES HOLD FOUR DIGIT YEARS
      *> 2011-05-19 DLS  CR2416 SCOPED CREDIT OVERRIDES WITH EXPIRY
      *> 2021-01-04 DLS  CR3004 BREXIT - INCOTERMS AND NAMED PLACE
      *> 2026-10-06 STERP SO-002, SO-003, SO-007, SO-011, SO-019
      *>================================================================
      *> REQ: SO-002, SO-003, SO-007, SO-011, SO-019
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SA-BODY.
               10  SA-ORDER-NUMBER     PIC X(30).
               10  SA-SALES-ORDER-ID   PIC S9(17).
               10  SA-CUSTOMER         PIC X(20).
               10  SA-INVOICE-CUST     PIC X(20).
               10  SA-ORDER-TYPE       PIC X(12).
               10  SA-ORDER-DATE       PIC 9(8).
               10  SA-CURRENCY         PIC X(3).
               10  SA-FX-RATE          PIC S9(9)V9(8) COMP-3.
               10  SA-TERMS-CODE       PIC X(12).
               10  SA-TERMS-SNAPSHOT   PIC X(100).
               10  SA-SALESPERSON      PIC X(20).
               10  SA-DELIVERY-REF     PIC X(10).
               10  SA-DELIVERY-SNAP    PIC X(300).
               10  SA-BILLTO-SNAP      PIC X(300).
               10  SA-INCOTERMS-RULE   PIC X(3).
               10  SA-INCOTERMS-PLACE  PIC X(60).
               10  SA-VAT-LOCATION     PIC X(3).
               10  SA-INTERNATIONAL    PIC X.
                   88  SA-IS-INTERNATIONAL       VALUE "Y".
               10  SA-TC-VERSION       PIC X(20).
               10  SA-RETENTION-TITLE  PIC X.
               10  SA-VAT-BASIS        PIC X(20).
               10  SA-CUST-VAT-REG     PIC X(15).
               10  SA-ITEM-AUTHORITY   PIC X(40).
               10  SA-NET-AMOUNT       PIC S9(11)V99 COMP-3.
               10  SA-VAT-AMOUNT       PIC S9(11)V99 COMP-3.
               10  SA-GROSS-AMOUNT     PIC S9(11)V99 COMP-3.
               10  SA-GROSS-GBP        PIC S9(11)V99 COMP-3.
               10  SA-LINE-COUNT       PIC S9(9) COMP-5.
      *>       state before and after the instruction
               10  SA-COMM-STATUS      PIC X(12).
               10  SA-PREV-COMM-STATUS PIC X(12).
               10  SA-RELEASE-STATUS   PIC X(14).
                   88  SA-RELEASED               VALUE "RELEASED".
                   88  SA-CREDIT-HELD            VALUE "CREDITHOLD".
                   88  SA-COMM-HELD              VALUE "COMMERCIALHOLD".
               10  SA-PREV-REL-STATUS  PIC X(14).
               10  SA-FULFIL-STATUS    PIC X(14).
               10  SA-BILLING-STATUS   PIC X(14).
               10  SA-HOLD-REASON      PIC X(200).
      *>       credit position taken from CREDCHK (D-06)
               10  SA-CREDIT-DECISION  PIC X(10).
               10  SA-CREDIT-EXPOSURE  PIC S9(11)V99 COMP-3.
               10  SA-CREDIT-LIMIT     PIC S9(11)V99 COMP-3.
               10  SA-CREDIT-HEADROOM  PIC S9(11)V99 COMP-3.
               10  SA-CREDIT-EXCESS    PIC S9(11)V99 COMP-3.
               10  SA-OVERDUE-GBP      PIC S9(11)V99 COMP-3.
               10  SA-OVERDUE-DAYS     PIC S9(5).
               10  SA-OVERRIDE-ID      PIC S9(9) COMP-5.
               10  SA-OVERRIDE-EXPIRY  PIC 9(8).
               10  SA-OVERRIDE-ACTIVE  PIC X.
               10  SA-APPROVAL-ID      PIC S9(9) COMP-5.
               10  SA-ACCOUNT-STATUS   PIC X(10).
      *>       SO-011 - a print is evidence, never a release
               10  SA-PRINT-ONLY       PIC X.
                   88  SA-PRINT-REQUEST          VALUE "Y".
               10  SA-ACK-PRINT-COUNT  PIC S9(9) COMP-5.
      *>       SO-019 deposit control; only cleared funds count
               10  SA-DEPOSIT-REQ-GBP  PIC S9(11)V99 COMP-3.
               10  SA-DEPOSIT-RCVD-GBP PIC S9(11)V99 COMP-3.
               10  SA-DEPOSIT-AMOUNT   PIC S9(11)V99 COMP-3.
               10  SA-DEPOSIT-REF      PIC X(30).
               10  SA-DEPOSIT-METHOD   PIC X(10).
               10  SA-DEPOSIT-VAT-CODE PIC X(4).
               10  SA-DEPOSIT-CLEARED  PIC X.
               10  SA-DEPOSIT-RESIDUAL PIC S9(11)V99 COMP-3.
               10  SA-DEPOSIT-OUTSTAND PIC S9(11)V99 COMP-3.
               10  SA-DEPOSIT-ID       PIC S9(9) COMP-5.
      *>       the blocker list returned by VALIDATE and ACCEPT
               10  SA-BLOCKER-CNT      PIC 9(3).
               10  SA-HARD-CNT         PIC 9(3).
               10  SA-BLOCKERS OCCURS 30 TIMES.
                   15  SA-B-SEVERITY   PIC X(8).
                   15  SA-B-REASON     PIC X(8).
                   15  SA-B-LINE-NUM   PIC S9(9) COMP-5.
                   15  SA-B-FIELD      PIC X(40).
                   15  SA-B-TEXT       PIC X(160).
