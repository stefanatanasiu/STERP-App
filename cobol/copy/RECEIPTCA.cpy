      *>================================================================
      *> RECEIPTCA - COMMAREA for RECEIPT, goods inward confirmation.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> A receipt records WHAT physically arrived, WHEN it arrived and
      *> WHO owns it.  Quantity, traceability, location and ownership
      *> are recorded at the point of receipt because none of them can
      *> be reconstructed afterwards (PO-009).
      *>
      *> Ownership, not the gate, decides the accounting date (D-03):
      *>   RECEIPT     title passes when we take the goods in; the
      *>               physical date and the ownership date agree.
      *>   INCOTERMS   title passed at the supplier's door (EXW, FCA,
      *>               FOB).  The goods are ours while they are still
      *>               on the water, so they are taken into STOCK IN
      *>               TRANSIT against shipment evidence and confirmed
      *>               into the warehouse when they land (PO-014).
      *>   DROPSHIP    the goods never reach our warehouse; the receipt
      *>               recognises the liability and the cost of sale
      *>               without a stock balance (PO-018).
      *>
      *> CA-FUNCTION values
      *>   VALIDATE  check the lines and the tolerances only - no
      *>             stock movement and no nominal posting
      *>   CONFIRM   validate, move the stock through STKMOVE, record
      *>             po.Receipt/po.ReceiptLine, then accrue goods
      *>             received not invoiced through POPOST
      *>   TRANSIT   confirm an in-transit receipt into the warehouse
      *>   REVERSE   reverse a confirmed receipt in full (a true
      *>             reversal, so it reduces the COUNTED quantity)
      *>
      *> GRNI (PO-017).  The accrual is the purchase order price for
      *> the quantity actually received, so the ledger carries a
      *> liability for goods held but not invoiced from the moment the
      *> goods arrive.  POPOST is the only route to the nominal ledger
      *> and the correlation id makes a retried receipt idempotent: a
      *> delivery confirmed twice can never accrue GRNI twice.
      *>
      *> RC-DUP-POLICY governs a repeated supplier delivery note:
      *>   BLOCK  (default) the second use of the same delivery note
      *>          reference for the same supplier is refused
      *>   WARN   allowed with a warning - used when a supplier
      *>          genuinely reuses references across sites
      *>
      *> REASON CODES
      *>   RCP0001 no lines supplied
      *>   RCP0002 purchase order is not in a receivable state
      *>   RCP0003 quantity is zero or negative
      *>   RCP0004 lot or serial number is required for this item
      *>   RCP0005 a location is required for a stocked item
      *>   RCP0006 over delivery above tolerance - amend the order
      *>   RCP0007 duplicate supplier delivery note
      *>   RCP0008 unsupported function
      *>   RCP0009 ownership basis INCOTERMS needs shipment evidence
      *>   RCP0010 stock movement refused by STKMOVE
      *>   RCP0011 nominal posting refused
      *>   RCP0012 receipt already reversed or not confirmed
      *>   RCP0013 physical date is in the future
      *>   RCP0014 goods receipt date is outside an open period
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-05-12 RJH  CR0801 GOODS INWARD
      *> 2000-02-19 KMP  CR0902 LOT AND SERIAL AT GOODS INWARD
      *> 2021-01-01 TMW  BREXIT - STOCK IN TRANSIT FOR IMPORTS
      *> 2026-10-06 STERP PO-009/014/017, STKMOVE AND POPOST
      *>================================================================
      *> REQ: PO-009, PO-014, PO-017, PO-018, PO-011
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  RC-BODY.
               10  RC-RECEIPT-REF      PIC X(30).
               10  RC-ORDER-NUMBER     PIC X(30).
               10  RC-SUPPLIER         PIC X(20).
               10  RC-DELIVERY-NOTE    PIC X(40).
               10  RC-PHYSICAL-DATE    PIC 9(8).
               10  RC-POSTING-DATE     PIC 9(8).
               10  RC-WAREHOUSE        PIC X(20).
               10  RC-OWNERSHIP        PIC X(16).
                   88  RC-OWN-RECEIPT            VALUE "RECEIPT".
                   88  RC-OWN-INCOTERMS          VALUE "INCOTERMS".
                   88  RC-OWN-DROPSHIP           VALUE "DROPSHIP".
               10  RC-INCOTERM         PIC X(3).
               10  RC-SHIP-EVIDENCE    PIC X(60).
               10  RC-IMPORT-ENTRY     PIC X(30).
               10  RC-RETENTION-TITLE  PIC X.
               10  RC-SUBCONTRACT-JOB  PIC X(20).
               10  RC-DUP-POLICY       PIC X(6).
                   88  RC-DUP-BLOCK              VALUE "BLOCK".
                   88  RC-DUP-WARN               VALUE "WARN".
               10  RC-RECEIVED-BY      PIC X(20).
               10  RC-CURRENCY         PIC X(3).
               10  RC-FX-RATE          PIC S9(9)V9(8).
               10  RC-REVERSE-REF      PIC X(30).
               10  RC-REVERSE-REASON   PIC X(200).
               10  RC-LINE-CNT         PIC S9(4).
               10  RC-LINE-TABLE.
                   15  RC-LINE OCCURS 20 TIMES.
                       20  RC-L-NUM        PIC S9(9) COMP-5.
                       20  RC-L-PO-LINE    PIC S9(9) COMP-5.
                       20  RC-L-SCHEDULE   PIC S9(9) COMP-5.
                       20  RC-L-ITEM       PIC X(40).
                       20  RC-L-REVISION   PIC X(4).
                       20  RC-L-DESC       PIC X(200).
                       20  RC-L-ORDERED    PIC S9(13)V9(6).
                       20  RC-L-CANCELLED  PIC S9(13)V9(6).
                       20  RC-L-COUNTED    PIC S9(13)V9(6).
                       20  RC-L-RECEIVED   PIC S9(13)V9(6).
                       20  RC-L-UOM        PIC X(10).
                       20  RC-L-FACTOR     PIC S9(9)V9(9).
                       20  RC-L-STOCK-QTY  PIC S9(13)V9(6).
                       20  RC-L-LOCATION   PIC X(20).
                       20  RC-L-LOT        PIC X(30).
                       20  RC-L-SERIAL     PIC X(40).
                       20  RC-L-SUPP-LOT   PIC X(40).
                       20  RC-L-EXPIRY     PIC 9(8).
                       20  RC-L-TRACE      PIC X(8).
                           88  RC-L-TRACE-NONE   VALUE "NONE".
                           88  RC-L-TRACE-LOT    VALUE "LOT".
                           88  RC-L-TRACE-SERIAL VALUE "SERIAL".
                       20  RC-L-INSPECT    PIC X.
                           88  RC-L-NEEDS-INSP   VALUE "Y".
                       20  RC-L-STOCKED    PIC X.
                       20  RC-L-PRICE      PIC S9(11)V9(6).
                       20  RC-L-STD-COST   PIC S9(11)V9(6).
                       20  RC-L-PROMISED   PIC 9(8).
                       20  RC-L-OVER-PCT   PIC S9(5)V9(4).
      *>             ---- answers, per line ----
                       20  RC-L-ACCEPTED   PIC S9(13)V9(6).
                       20  RC-L-QUARANT    PIC S9(13)V9(6).
                       20  RC-L-OVER-QTY   PIC S9(13)V9(6).
                       20  RC-L-OUTSTAND   PIC S9(13)V9(6).
                       20  RC-L-VALUE-GBP  PIC S9(13)V99.
                       20  RC-L-PPV-GBP    PIC S9(13)V99.
                       20  RC-L-MOVEMENT   PIC S9(17).
                       20  RC-L-OUTCOME    PIC X(10).
                       20  RC-L-REASON     PIC X(8).
      *>         ---- answers, header ----
               10  RC-RECEIPT-ID       PIC S9(17).
               10  RC-TOTAL-GBP        PIC S9(13)V99.
               10  RC-GRNI-GBP         PIC S9(13)V99.
               10  RC-PPV-GBP          PIC S9(13)V99.
               10  RC-JOURNAL-NUMBER   PIC X(30).
               10  RC-LINES-ACCEPTED   PIC S9(4).
               10  RC-LINES-HELD       PIC S9(4).
               10  RC-LINES-QUARANT    PIC S9(4).
               10  RC-IS-TRANSIT       PIC X.
               10  RC-WAS-DUPLICATE    PIC X.
               10  RC-STATUS           PIC X(12).
