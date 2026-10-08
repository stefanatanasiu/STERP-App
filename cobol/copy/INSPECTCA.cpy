      *>================================================================
      *> INSPECTCA - COMMAREA for INSPECT, goods inward inspection.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> Goods that require inspection are received into QUARANTINE and
      *> are not available to pick, issue or sell until they are
      *> dispositioned.  Quarantine is a quality condition of real
      *> stock, not a separate ledger: the goods are on the balance
      *> sheet from the moment they were received (PO-010, ITM-016).
      *>
      *> CA-FUNCTION values
      *>   RECORD    record the inspection result and move the
      *>             accepted, rejected and reworked quantities
      *>   ENQUIRE   report what is still in quarantine for a receipt
      *>
      *> IN-DISPOSITION applies to the REJECTED quantity:
      *>   RETURN      send back to the supplier - the stock stays in
      *>               quarantine and SUPPRET moves and values it, so
      *>               the goods are never removed twice
      *>   SCRAP       destroyed on site; stock leaves against the
      *>               material usage variance and a supplier claim is
      *>               a separate commercial matter
      *>   REWORK      corrected in house; the quantity returns to
      *>               good condition and the rework cost is recorded
      *>   CONCESSION  accepted outside specification on a named
      *>               authority, with the concession reference held
      *>               against the receipt line
      *>   HOLD        left in quarantine pending the supplier's
      *>               answer; nothing moves
      *>
      *> A concession or a use-as-is decision must name the person who
      *> took it.  "The stores accepted it" is not an authority.
      *>
      *> REASON CODES
      *>   INS0001 receipt line not found or not in quarantine
      *>   INS0002 the quantities do not add back to the inspected
      *>           quantity
      *>   INS0003 inspected quantity exceeds the quarantined quantity
      *>   INS0004 a rejection must state a reason
      *>   INS0005 a concession must name its authority
      *>   INS0006 unsupported function
      *>   INS0007 stock movement refused
      *>   INS0008 nominal posting refused
      *>   INS0009 certificate of conformity is required for this item
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-07-30 RJH  CR0824 GOODS INWARD INSPECTION
      *> 2004-02-11 DJM  CR1142 CONCESSION NEEDS A NAMED AUTHORITY
      *> 2026-10-06 STERP PO-010 THROUGH STKMOVE AND POPOST
      *>================================================================
      *> REQ: PO-010, PO-023
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  IN-BODY.
               10  IN-RECEIPT-REF      PIC X(30).
               10  IN-RECEIPT-LINE     PIC S9(9) COMP-5.
               10  IN-ORDER-NUMBER     PIC X(30).
               10  IN-SUPPLIER         PIC X(20).
               10  IN-ITEM-CODE        PIC X(40).
               10  IN-LOT-CODE         PIC X(30).
               10  IN-SERIAL-NO        PIC X(40).
               10  IN-WAREHOUSE        PIC X(20).
               10  IN-LOCATION         PIC X(20).
               10  IN-GOOD-LOCATION    PIC X(20).
               10  IN-UOM-CODE         PIC X(10).
               10  IN-INSPECT-DATE     PIC 9(8).
               10  IN-INSPECTOR        PIC X(20).
               10  IN-QUARANTINE-QTY   PIC S9(13)V9(6).
               10  IN-INSPECTED-QTY    PIC S9(13)V9(6).
               10  IN-ACCEPTED-QTY     PIC S9(13)V9(6).
               10  IN-REJECTED-QTY     PIC S9(13)V9(6).
               10  IN-REWORK-QTY       PIC S9(13)V9(6).
               10  IN-DISPOSITION      PIC X(12).
                   88  IN-DISP-RETURN            VALUE "RETURN".
                   88  IN-DISP-SCRAP             VALUE "SCRAP".
                   88  IN-DISP-REWORK            VALUE "REWORK".
                   88  IN-DISP-CONCESSION        VALUE "CONCESSION".
                   88  IN-DISP-HOLD              VALUE "HOLD".
               10  IN-REJECT-REASON    PIC X(20).
               10  IN-REJECT-NARR      PIC X(200).
               10  IN-CONCESSION-REF   PIC X(40).
               10  IN-CONCESSION-BY    PIC X(20).
               10  IN-CERT-REQUIRED    PIC X.
               10  IN-CERT-REF         PIC X(60).
               10  IN-UNIT-COST        PIC S9(11)V9(6).
               10  IN-REWORK-COST-GBP  PIC S9(13)V99.
      *>         ---- answers ----
               10  IN-REMAINING-QTY    PIC S9(13)V9(6).
               10  IN-TO-GOOD-QTY      PIC S9(13)V9(6).
               10  IN-TO-SCRAP-QTY     PIC S9(13)V9(6).
               10  IN-HELD-FOR-RET-QTY PIC S9(13)V9(6).
               10  IN-SCRAP-VALUE-GBP  PIC S9(13)V99.
               10  IN-QUALITY-STATUS   PIC X(12).
               10  IN-MOVEMENT-ID      PIC S9(17).
               10  IN-MOVEMENT-ID-2    PIC S9(17).
               10  IN-JOURNAL-NUMBER   PIC X(30).
               10  IN-OUTCOME          PIC X(10).
