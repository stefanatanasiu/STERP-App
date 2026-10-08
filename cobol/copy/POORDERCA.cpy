      *>================================================================
      *> POORDERCA - COMMAREA for POORDER, purchase order capture,
      *>             scheduling, issue, acknowledgement and amendment.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> The OR- prefix is used rather than PO- so that a program may
      *> COPY both POORDERCA and POACCNTS without ambiguity.
      *>
      *> A DRAFT order is never presented as a commitment (PO-006):
      *> only ISSUE moves the order to ISSUED, writes the COMMITTED
      *> movements to po.Commitment and stamps the issue time.  The
      *> approval itself belongs to POAPPRV.
      *>
      *> Acknowledgements are never silent overwrites (PO-007): a
      *> difference between the ordered and acknowledged price,
      *> quantity or date is written to po.OrderAmendment as an
      *> original/revised pair for the buyer to action, and a
      *> material difference sets OR-NEEDS-REAPPR.
      *>
      *> CA-FUNCTION values
      *>   CREATE    open an order header (draft)
      *>   ADDLINE   add or replace a line, priced through POPRICE
      *>   SCHEDULE  add or replace a delivery schedule on a line
      *>   ISSUE     issue the approved order and commit the value
      *>   ACKNOWL   record a supplier acknowledgement
      *>   AMEND     record a buyer amendment
      *>   CALLOFF   create a call-off against a blanket order
      *>   COPY      copy an existing order without receipt state
      *>   ENQUIRE   read header totals and outstanding quantities
      *>
      *> CA-RETURN-CODE 00 ok, 04 warning, 06 hold (approval, budget
      *>   or due diligence), 08 rejected, 12 severe, 16 not found
      *>
      *> REASON CODES
      *>   ORD0001 company, site, supplier or buyer missing
      *>   ORD0002 order not found
      *>   ORD0003 the order status does not allow this function
      *>   ORD0004 line quantity, price or description invalid
      *>   ORD0005 a non stock or service line needs an account code
      *>   ORD0006 destination site or location missing
      *>   ORD0007 schedule total does not reconcile to the line
      *>   ORD0008 unsupported function
      *>   ORD0009 the order has no lines
      *>   ORD0010 number series failed
      *>   ORD0011 blanket balance exhausted or expired
      *>   ORD0012 the order is not approved for the exact content
      *>   ORD0013 supplier due diligence blocks issue
      *>   ORD0014 payment terms exceed the approved maximum
      *>   ORD0015 acknowledgement differs - buyer action raised
      *>   ORD0016 an amendment needs a reason
      *>   ORD0017 blanket call-off must name a blanket order
      *>   ORD0018 a stocked item cannot be created from free text
      *>----------------------------------------------------------------
      *> 1998-03-09 RJH  CR0780 PURCHASE ORDER ENTRY
      *> 2001-06-25 DJM  CR0998 MULTIPLE SCHEDULES PER LINE
      *> 2004-10-11 DJM  CR1190 BLANKET CALL-OFFS
      *> 2026-10-06 STERP ORIGINAL VERSUS REVISED AMENDMENT HISTORY
      *>================================================================
      *> REQ: PO-003, PO-007, PO-008, PO-021
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  OR-BODY.
               10  OR-ORDER-NUMBER     PIC X(30).
               10  OR-ORDER-TYPE       PIC X(12).
                   88  OR-STANDARD               VALUE "STANDARD".
                   88  OR-BLANKET                VALUE "BLANKET".
                   88  OR-CALLOFF                VALUE "CALLOFF".
                   88  OR-SUBCONTRACT            VALUE "SUBCONTRACT".
                   88  OR-DROPSHIP               VALUE "DROPSHIP".
                   88  OR-SERVICE                VALUE "SERVICE".
                   88  OR-TYPE-KNOWN             VALUE "STANDARD",
                       "BLANKET", "CALLOFF", "SUBCONTRACT",
                       "DROPSHIP", "SERVICE".
               10  OR-BLANKET-NUMBER   PIC X(30).
               10  OR-SUPPLIER         PIC X(20).
               10  OR-BUYER            PIC X(20).
               10  OR-ORDER-DATE       PIC 9(8).
               10  OR-CURRENCY         PIC X(3).
               10  OR-FX-RATE          PIC S9(9)V9(8).
               10  OR-TERMS-CODE       PIC X(12).
               10  OR-TERMS-SNAPSHOT   PIC X(100).
               10  OR-INCOTERMS        PIC X(3).
               10  OR-NAMED-PLACE      PIC X(60).
               10  OR-OWNERSHIP-AT     PIC X(20).
               10  OR-DELIVERY-ADDR    PIC X(300).
               10  OR-TC-VERSION       PIC X(20).
               10  OR-SUPPLIER-REF     PIC X(40).
               10  OR-BUDGET-CODE      PIC X(20).
               10  OR-LINKED-SO        PIC X(30).
      *>     ---- line fields ----
               10  OR-LINE-NUM         PIC S9(9) COMP-5.
               10  OR-LINE-TYPE        PIC X(10).
                   88  OR-L-STOCK                VALUE "STOCK".
                   88  OR-L-NONSTOCK             VALUE "NONSTOCK".
                   88  OR-L-SERVICE              VALUE "SERVICE".
                   88  OR-L-SUBCON               VALUE "SUBCON".
                   88  OR-L-CHARGE               VALUE "CHARGE".
                   88  OR-L-TEXT                 VALUE "TEXT".
                   88  OR-L-KNOWN                VALUE "STOCK",
                       "NONSTOCK", "SERVICE", "SUBCON", "CHARGE",
                       "TEXT".
               10  OR-ITEM-CODE        PIC X(40).
               10  OR-REVISION         PIC X(4).
               10  OR-DESCRIPTION      PIC X(200).
               10  OR-ORDERED-QTY      PIC S9(13)V9(6).
               10  OR-UOM-CODE         PIC X(10).
               10  OR-CONV-FACTOR      PIC S9(9)V9(9).
               10  OR-UNIT-PRICE       PIC S9(11)V9(6).
               10  OR-PRICE-PER        PIC S9(11)V9(6).
               10  OR-DISCOUNT-PCT     PIC S9(5)V9(4).
               10  OR-CHARGE-AMT       PIC S9(13)V99.
               10  OR-VAT-CODE         PIC X(4).
               10  OR-VAT-RATE-PCT     PIC S9(5)V9(4).
               10  OR-RECOVERABLE-VAT  PIC X.
                   88  OR-VAT-RECOVERABLE        VALUE "Y".
               10  OR-ACCOUNT-CODE     PIC X(12).
               10  OR-COST-CENTRE      PIC X(20).
               10  OR-PROJECT          PIC X(20).
               10  OR-JOB-REF          PIC X(20).
               10  OR-DEST-SITE        PIC X(10).
               10  OR-DEST-LOCATION    PIC X(20).
               10  OR-REQ-NUMBER       PIC X(30).
               10  OR-LINKED-SO-LINE   PIC S9(9) COMP-5.
               10  OR-AUTO-PRICE       PIC X.
                   88  OR-PRICE-FROM-FILE        VALUE "Y".
      *>     ---- schedule fields ----
               10  OR-SCHEDULE-NO      PIC S9(9) COMP-5.
               10  OR-SCH-QTY          PIC S9(13)V9(6).
               10  OR-SCH-REQ-DATE     PIC 9(8).
               10  OR-SCH-CONF-DATE    PIC 9(8).
               10  OR-SCH-DEST-SITE    PIC X(10).
               10  OR-SCH-DEST-LOCN    PIC X(20).
      *>     ---- acknowledgement and amendment fields ----
               10  OR-ACK-QTY          PIC S9(13)V9(6).
               10  OR-ACK-PRICE        PIC S9(11)V9(6).
               10  OR-ACK-DATE         PIC 9(8).
               10  OR-ACK-NOTE         PIC X(200).
               10  OR-AMEND-FIELD      PIC X(40).
               10  OR-AMEND-OLD        PIC X(100).
               10  OR-AMEND-NEW        PIC X(100).
               10  OR-AMEND-REASON     PIC X(300).
               10  OR-COPY-FROM        PIC X(30).
      *>     ---- answers ----
               10  OR-ORDER-ID         PIC S9(17).
               10  OR-LINE-ID          PIC S9(17).
               10  OR-SCHEDULE-ID      PIC S9(17).
               10  OR-LINE-NET-GBP     PIC S9(13)V99.
               10  OR-LINE-VAT-GBP     PIC S9(13)V99.
               10  OR-LINE-NONREC-VAT  PIC S9(13)V99.
               10  OR-NET-AMOUNT       PIC S9(13)V99.
               10  OR-VAT-AMOUNT       PIC S9(13)V99.
               10  OR-GROSS-AMOUNT     PIC S9(13)V99.
               10  OR-COMMITMENT-GBP   PIC S9(13)V99.
               10  OR-OUTSTANDING-QTY  PIC S9(13)V9(6).
               10  OR-SCHEDULED-QTY    PIC S9(13)V9(6).
               10  OR-LINE-CNT         PIC 9(4).
               10  OR-SCHEDULE-CNT     PIC 9(4).
               10  OR-AMEND-NO         PIC S9(9) COMP-5.
               10  OR-AMEND-CNT        PIC 9(4).
               10  OR-PRICE-SOURCE     PIC X(20).
               10  OR-CONTENT-HASH     PIC X(64).
               10  OR-BLANKET-QTY-LEFT PIC S9(13)V9(6).
               10  OR-BLANKET-VAL-LEFT PIC S9(13)V99.
               10  OR-NEEDS-REAPPR     PIC X.
                   88  OR-REAPPROVE              VALUE "Y".
               10  OR-MATERIAL-CHANGE  PIC X.
                   88  OR-IS-MATERIAL            VALUE "Y".
               10  OR-STATUS           PIC X(14).
               10  OR-OUTCOME          PIC X(12).
                   88  OR-CAPTURED               VALUE "CAPTURED".
                   88  OR-ISSUED                 VALUE "ISSUED".
                   88  OR-ACKNOWLEDGED           VALUE "ACKNOWLEDGE".
                   88  OR-AMENDED                VALUE "AMENDED".
                   88  OR-HELD                   VALUE "HELD".
