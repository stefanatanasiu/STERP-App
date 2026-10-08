      *>================================================================
      *> SOAMENDCA - COMMAREA for SOAMEND, order amendment and
      *>             cancellation.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : SALES ORDER MANAGEMENT
      *>
      *> An amendment respects what has already happened.  A line can
      *> never be reduced below its despatched or billed quantity, and
      *> a cancellation only ever touches the UNFULFILLED balance: it
      *> does not reverse a despatch, an invoice or a supplier
      *> commitment, and it releases only the unused reservations
      *> (SO-016).
      *>
      *> Every amendment re-runs the credit check, invalidates the
      *> approvals that depended on the old content and bumps the
      *> document version so the history stays reproducible (SO-007,
      *> SO-022).
      *>
      *> CA-FUNCTION values  (CA-FUNCTION is X(8), so a longer name
      *> arrives truncated: AMENDPRICE is matched as AMENDPRI,
      *> AMENDDATE as AMENDDAT and CANCELORD as CANCELOR)
      *>   AMENDQTY   change AcceptedQty to AM-NEW-QTY
      *>   AMENDPRICE change UnitPrice to AM-NEW-PRICE.  After
      *>              commercial acceptance this needs AM-APPROVAL-ID.
      *>   AMENDDATE  change the line's promise date to AM-NEW-DATE
      *>              (the requested and original promises survive)
      *>   CANCELLN   cancel AM-CANCEL-QTY, or the whole unfulfilled
      *>              balance when AM-CANCEL-QTY is zero
      *>   CANCELORD  cancel the unfulfilled balance of every open
      *>              line and set CommercialStatus CANCELLED
      *>
      *> REASON CODES
      *>   AMD0001 a company, an order and a function are required
      *>   AMD0002 order or order line not found
      *>   AMD0003 the amendment would reduce the line below the
      *>           quantity already despatched or billed
      *>   AMD0004 a price amendment after acceptance needs an
      *>           approval id
      *>   AMD0005 a cancellation quantity exceeds the unfulfilled
      *>           balance
      *>   AMD0006 the order is already cancelled or completed
      *>   AMD0007 credit hold - the amendment raises exposure (06)
      *>   AMD0008 unsupported function
      *>   AMD0009 an amendment reason is required
      *>   AMD0010 a quantity or price must be positive
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-10-05 RJH  CR0255 ORDER AMENDMENT CONTROL
      *> 2002-04-18 KMP  CR0981 RELEASE ONLY THE UNUSED RESERVATION
      *> 2016-02-22 DLS  CR2901 APPROVAL INVALIDATION ON CONTENT CHANGE
      *> 2026-10-06 STERP SO-016, SO-007, SO-022
      *>================================================================
      *> REQ: SO-016, SO-007, SO-022
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  AM-BODY.
               10  AM-ORDER-NUMBER     PIC X(30).
               10  AM-LINE-NUM         PIC S9(9) COMP-5.
               10  AM-SALES-ORDER-ID   PIC S9(17).
               10  AM-ORDER-LINE-ID    PIC S9(17).
               10  AM-CUSTOMER         PIC X(20).
               10  AM-CURRENCY         PIC X(3).
               10  AM-FX-RATE          PIC S9(9)V9(8) COMP-3.
               10  AM-ITEM-CODE        PIC X(40).
               10  AM-REVISION         PIC X(4).
               10  AM-SITE-CODE        PIC X(10).
               10  AM-UOM-CODE         PIC X(10).
               10  AM-LINE-TYPE        PIC X(10).
      *>       the instruction
               10  AM-NEW-QTY          PIC S9(13)V9(6).
               10  AM-NEW-PRICE        PIC S9(11)V9(6).
               10  AM-NEW-DATE         PIC 9(8).
               10  AM-CANCEL-QTY       PIC S9(13)V9(6).
               10  AM-REASON-TEXT      PIC X(200).
               10  AM-APPROVAL-ID      PIC S9(9) COMP-5.
      *>       the line as it stood before the amendment
               10  AM-OLD-QTY          PIC S9(13)V9(6).
               10  AM-OLD-PRICE        PIC S9(11)V9(6).
               10  AM-OLD-DATE         PIC 9(8).
               10  AM-ACCEPTED-QTY     PIC S9(13)V9(6).
               10  AM-CANCELLED-QTY    PIC S9(13)V9(6).
               10  AM-DESPATCHED-QTY   PIC S9(13)V9(6).
               10  AM-BILLED-QTY       PIC S9(13)V9(6).
               10  AM-RETURNED-QTY     PIC S9(13)V9(6).
               10  AM-UNFULFILLED-QTY  PIC S9(13)V9(6).
               10  AM-REMAIN-DESPATCH  PIC S9(13)V9(6).
               10  AM-FLOOR-QTY        PIC S9(13)V9(6).
      *>       what the amendment did
               10  AM-APPLIED-QTY      PIC S9(13)V9(6).
               10  AM-RELEASED-QTY     PIC S9(13)V9(6).
               10  AM-RESERVED-BEFORE  PIC S9(13)V9(6).
               10  AM-LINES-CANCELLED  PIC S9(9) COMP-5.
               10  AM-LINE-NET         PIC S9(11)V99 COMP-3.
               10  AM-ORDER-NET        PIC S9(11)V99 COMP-3.
               10  AM-NET-DELTA        PIC S9(11)V99 COMP-3.
               10  AM-DOC-VERSION      PIC S9(9) COMP-5.
               10  AM-CONTENT-HASH     PIC X(64).
               10  AM-APPROVALS-VOID   PIC S9(9) COMP-5.
               10  AM-AUDIT-ID         PIC S9(9) COMP-5.
      *>       the current credit position after the amendment
               10  AM-CREDIT-DECISION  PIC X(10).
               10  AM-CREDIT-EXPOSURE  PIC S9(11)V99 COMP-3.
               10  AM-CREDIT-LIMIT     PIC S9(11)V99 COMP-3.
               10  AM-CREDIT-HEADROOM  PIC S9(11)V99 COMP-3.
               10  AM-RELEASE-STATUS   PIC X(14).
               10  AM-HOLD-REASON      PIC X(200).
               10  AM-LINE-STATUS      PIC X(10).
               10  AM-ORDER-STATUS     PIC X(12).
               10  AM-FULFIL-STATUS    PIC X(14).
               10  FILLER              PIC X(60).
