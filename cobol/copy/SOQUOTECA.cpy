      *>================================================================
      *> SOQUOTECA - COMMAREA for SOQUOTE, quotations, conversion,
      *>             templates and blanket call-offs.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : SALES ORDER MANAGEMENT
      *>
      *> A quote is not a sales commitment.  Each issued version is
      *> retained: revising an issued quote creates a NEW version and
      *> leaves the previous one REVISED, so what the customer actually
      *> accepted stays reproducible (SO-022).
      *>
      *> CA-FUNCTION values
      *>   CREATE    new quote, version 1, status DRAFT.  The number
      *>             comes from NUMGEN series SOQUOTE.
      *>   REVISE    copy SQ-QUOTE-NUMBER into a new version; the
      *>             previous version becomes REVISED
      *>   ISSUE     DRAFT -> ISSUED
      *>   ACCEPT    ISSUED -> ACCEPTED, recording SQ-ACCEPTED-AT and
      *>             SQ-ACCEPT-EVIDENCE.  A quote past SQ-VALID-UNTIL
      *>             cannot be accepted.
      *>   DECLINE   ISSUED -> DECLINED with SQ-DECLINE-REASON
      *>   EXPIRE    ISSUED -> EXPIRED once past SQ-VALID-UNTIL
      *>   CONVERT   accepted version -> so.SalesOrder + lines, keeping
      *>             the accepted terms, prices, VAT codes and the
      *>             provenance so.SalesOrder.QuoteNumber.  Repeating
      *>             the instruction (same version or the same
      *>             CA-CORRELATION-ID) returns the order already
      *>             created with CA-RETURN-CODE 04 - never a second
      *>             demand (SO-001, COM-015).
      *>   TEMPLATE  copy an order or quote into a NEW draft order.
      *>             No fulfilment, despatch, billing or payment state
      *>             is copied: cancelled, despatched, billed and
      *>             returned quantities are zero, the order is DRAFT
      *>             and no deposit is treated as received (SO-024).
      *>   CALLOFF   a CALLOFF order against SQ-BLANKET-NUMBER.  The
      *>             call-off may not exceed the blanket's remaining
      *>             approved quantity or value.
      *>
      *> REASON CODES
      *>   SOQ0001 a mandatory input is missing (company, customer or
      *>           prospect, quote number, evidence or reason)
      *>   SOQ0002 quote or source document not found
      *>   SOQ0003 unsupported function
      *>   SOQ0004 duplicate customer order reference - review (04)
      *>   SOQ0005 the current status does not allow this action
      *>   SOQ0006 the quote validity date blocks this action
      *>   SOQ0007 no accepted version is available to convert
      *>   SOQ0008 blanket order not found or not accepted
      *>   SOQ0009 the call-off exceeds the blanket's remaining
      *>           approved quantity or value - blocked pending an
      *>           approved blanket change (06)
      *>   SOQ0010 document numbering failed
      *>   SOQ0011 this conversion has already produced an order (04)
      *>   SOQ0012 another attempt at this conversion is running (06)
      *>   SOQ0013 at least one quote line is required
      *>   SOQ0014 the customer master could not supply part of the
      *>           order header - the order is raised but cannot be
      *>           accepted until it is stated (04)
      *>
      *> ORDER HEADER DEFAULTING (CR2601)
      *>   A quotation states price, quantity and validity; it does
      *>   not re-state the account.  When a quote becomes an order
      *>   the invoiced party, the delivery and billing addresses,
      *>   the payment terms, the trading terms in issue and the
      *>   basis of the VAT treatment are taken from the customer
      *>   master, exactly as a clerk keying the order would take
      *>   them.  Anything the caller states in SQ-ORDER-DEFAULTS
      *>   before the call is kept; the rest is defaulted and the
      *>   values actually used are returned in the same fields.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-07-08 RJH  CR0455 QUOTATION REGISTER (ACUCOBOL)
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT VALIDITY DATES
      *> 2004-02-11 RJH  CR1142 BLANKET ORDERS AND CALL-OFFS
      *> 2026-10-06 STERP SO-001, SO-022, SO-024
      *> 2026-10-07 STERP CR2601 ORDER HEADER FROM CUSTOMER MASTER
      *>================================================================
      *> REQ: SO-001, SO-022, SO-024
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SQ-BODY.
               10  SQ-QUOTE-NUMBER     PIC X(30).
               10  SQ-VERSION          PIC S9(4) COMP-5.
               10  SQ-QUOTE-ID         PIC S9(17).
               10  SQ-CUSTOMER         PIC X(20).
               10  SQ-PROSPECT-NAME    PIC X(120).
               10  SQ-QUOTE-DATE       PIC 9(8).
               10  SQ-VALID-UNTIL      PIC 9(8).
               10  SQ-VALID-DAYS       PIC S9(4) COMP-5.
               10  SQ-CURRENCY         PIC X(3).
               10  SQ-FX-RATE          PIC S9(9)V9(8) COMP-3.
               10  SQ-TERMS-CODE       PIC X(12).
               10  SQ-TERMS-VERSION    PIC X(20).
               10  SQ-DELIVERY-EST     PIC X(100).
               10  SQ-ASSUMPTIONS      PIC X(400).
               10  SQ-SALESPERSON      PIC X(20).
               10  SQ-STATUS           PIC X(12).
                   88  SQ-IS-DRAFT               VALUE "DRAFT".
                   88  SQ-IS-ISSUED              VALUE "ISSUED".
                   88  SQ-IS-ACCEPTED            VALUE "ACCEPTED".
                   88  SQ-IS-CONVERTED           VALUE "CONVERTED".
               10  SQ-PREV-STATUS      PIC X(12).
               10  SQ-NET-AMOUNT       PIC S9(11)V99 COMP-3.
               10  SQ-VAT-AMOUNT       PIC S9(11)V99 COMP-3.
               10  SQ-GROSS-AMOUNT     PIC S9(11)V99 COMP-3.
               10  SQ-COST-BASIS-GBP   PIC S9(11)V99 COMP-3.
               10  SQ-MARGIN-PCT       PIC S9(5)V9(4) COMP-3.
               10  SQ-MARGIN-DEFINED   PIC X.
               10  SQ-ACCEPTED-AT      PIC X(23).
               10  SQ-ACCEPT-EVIDENCE  PIC X(200).
               10  SQ-DECLINE-REASON   PIC X(200).
      *>       conversion, template and call-off working fields
               10  SQ-ORDER-NUMBER     PIC X(30).
               10  SQ-SOURCE-REF       PIC X(30).
               10  SQ-SOURCE-TYPE      PIC X(10).
                   88  SQ-FROM-ORDER             VALUE "ORDER".
                   88  SQ-FROM-QUOTE             VALUE "QUOTE".
               10  SQ-ORDER-TYPE       PIC X(12).
               10  SQ-SITE-CODE        PIC X(10).
               10  SQ-CUSTOMER-REF     PIC X(40).
               10  SQ-DUP-REF-FOUND    PIC X.
                   88  SQ-DUP-REF                VALUE "Y".
               10  SQ-DUP-ORDER-NUMBER PIC X(30).
               10  SQ-ALREADY-DONE     PIC X.
                   88  SQ-WAS-DONE-BEFORE        VALUE "Y".
               10  SQ-LINES-CREATED    PIC S9(9) COMP-5.
      *>       blanket control (SO-024) - remaining commitment limits
               10  SQ-BLANKET-NUMBER   PIC X(30).
               10  SQ-BLANKET-STATUS   PIC X(12).
               10  SQ-BLANKET-QTY      PIC S9(13)V9(6).
               10  SQ-BLANKET-USED-QTY PIC S9(13)V9(6).
               10  SQ-BLANKET-FREE-QTY PIC S9(13)V9(6).
               10  SQ-BLANKET-VALUE    PIC S9(11)V99 COMP-3.
               10  SQ-BLANKET-USED-VAL PIC S9(11)V99 COMP-3.
               10  SQ-BLANKET-FREE-VAL PIC S9(11)V99 COMP-3.
               10  SQ-CALLOFF-QTY      PIC S9(13)V9(6).
               10  SQ-CALLOFF-VALUE    PIC S9(11)V99 COMP-3.
               10  SQ-APPROVAL-ID      PIC S9(9) COMP-5.
      *>       order header defaults (CR2601).  Supplied by the
      *>       caller OR taken from the customer master, and always
      *>       returned with the values written to so.SalesOrder.
               10  SQ-ORDER-DEFAULTS.
                   15  SQ-INVOICE-CUST PIC X(20).
                   15  SQ-DELIV-REF    PIC X(10).
                   15  SQ-DELIV-SNAP   PIC X(300).
                   15  SQ-BILL-SNAP    PIC X(300).
                   15  SQ-VAT-BASIS    PIC X(20).
                   15  SQ-VAT-LOCATION PIC X(3).
                   15  SQ-VAT-REG-NO   PIC X(15).
                   15  SQ-INCOTERMS    PIC X(3).
                   15  SQ-INCO-PLACE   PIC X(60).
                   15  SQ-DEFAULTS-SRC PIC X.
                       88  SQ-FROM-MASTER        VALUE "M".
                       88  SQ-FROM-CALLER        VALUE "C".
      *>       the quote lines.  OCCURS 40 since CR0455 - a quotation
      *>       longer than forty lines is raised as a schedule.
               10  SQ-LINE-CNT         PIC 9(3).
               10  SQ-LINES OCCURS 40 TIMES.
                   15  SQ-L-LINE-NUM   PIC S9(9) COMP-5.
                   15  SQ-L-ITEM       PIC X(40).
                   15  SQ-L-REVISION   PIC X(4).
                   15  SQ-L-DESC       PIC X(200).
                   15  SQ-L-QTY        PIC S9(13)V9(6).
                   15  SQ-L-UOM        PIC X(10).
                   15  SQ-L-UNIT-PRICE PIC S9(11)V9(6).
                   15  SQ-L-PRICE-PER  PIC S9(9)V9(6).
                   15  SQ-L-DISC1      PIC S9(5)V9(4) COMP-3.
                   15  SQ-L-DISC2      PIC S9(5)V9(4) COMP-3.
                   15  SQ-L-NET        PIC S9(11)V99 COMP-3.
                   15  SQ-L-VAT-CODE   PIC X(4).
                   15  SQ-L-VAT-AMT    PIC S9(11)V99 COMP-3.
                   15  SQ-L-COST-GBP   PIC S9(11)V99 COMP-3.
                   15  SQ-L-COST-BASIS PIC X(20).
                   15  SQ-L-MARGIN-PCT PIC S9(5)V9(4) COMP-3.
                   15  SQ-L-LEAD-DAYS  PIC S9(4) COMP-5.
                   15  SQ-L-PRICE-SRC  PIC X(20).
