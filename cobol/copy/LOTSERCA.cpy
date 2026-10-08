      *>================================================================
      *> LOTSERCA - COMMAREA for LOTSER, lot, serial and shelf life.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *>
      *> CA-FUNCTION values
      *>   CHKMOVE   is the lot / serial supplied by a movement
      *>             acceptable?  Enforces that a lot controlled item
      *>             carries a lot, a serial controlled item carries a
      *>             serial of quantity one, that a serial is not
      *>             already representing another unit in stock and
      *>             that expiry controlled stock has an expiry that
      *>             still leaves the minimum remaining shelf life
      *>             (ITM-006, ITM-011)
      *>   NEWLOT    create or refresh inv.Lot for a receipt, deriving
      *>             the expiry from the item shelf life when the
      *>             supplier has not given one
      *>   NEWSER    register inv.Serial for a receipt
      *>   SETSTAT   move a serial to a new status / location
      *>   EXPCHK    is this lot usable at LS-AS-AT?
      *>   TRACE     genealogy: the movements of a lot or serial
      *>
      *> LS-DIRECTION "I" increase or "O" decrease; a decrease of a
      *> serial checks that the serial is in stock, an increase checks
      *> that it is not.
      *>
      *> REASON CODES
      *>   LOT0001 lot reference required for a lot controlled item
      *>   LOT0002 serial number required for a serial controlled item
      *>   LOT0003 a serialised unit moves in whole units of one
      *>   LOT0004 serial already in stock (duplicate identity)
      *>   LOT0005 serial not in stock, so it cannot be issued
      *>   LOT0006 lot not known for this item
      *>   LOT0007 expiry date missing on an expiry controlled item
      *>   LOT0008 lot has expired
      *>   LOT0009 lot is inside its minimum remaining shelf life
      *>   LOT0010 lot or serial is not in an acceptable condition
      *>   LOT0011 unsupported function
      *>   LOT0012 traceability not required for this item
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-09-30 DJM  CR0462 LOT TRACEABILITY
      *> 2000-05-08 KMP  CR1003 SERIAL GENEALOGY FOR WARRANTY CLAIMS
      *> 2012-02-20 ANW  CR2740 MINIMUM REMAINING SHELF LIFE ON ISSUE
      *> 2026-10-05 STERP ITM-011 ENFORCED ACROSS EVERY MOVEMENT
      *>================================================================
      *> REQ: ITM-011
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  LS-BODY.
               10  LS-ITEM-CODE        PIC X(40).
               10  LS-REVISION         PIC X(4).
               10  LS-LOT-CODE         PIC X(30).
               10  LS-SERIAL-NO        PIC X(40).
               10  LS-SITE             PIC X(10).
               10  LS-WAREHOUSE        PIC X(20).
               10  LS-LOCATION         PIC X(20).
               10  LS-DIRECTION        PIC X.
               10  LS-QUANTITY         PIC S9(13)V9(6).
               10  LS-AS-AT            PIC 9(8).
               10  LS-TRACEABILITY     PIC X(8).
               10  LS-SHELF-LIFE-DAYS  PIC S9(5).
               10  LS-MIN-REMAIN-DAYS  PIC S9(5).
               10  LS-EXPIRY-DATE      PIC 9(8).
               10  LS-MANUFACTURED     PIC 9(8).
               10  LS-RECEIVED-DATE    PIC 9(8).
               10  LS-DAYS-REMAINING   PIC S9(7).
               10  LS-SUPPLIER-CODE    PIC X(20).
               10  LS-SUPPLIER-LOT     PIC X(40).
               10  LS-CERTIFICATE-REF  PIC X(60).
               10  LS-ORIGIN-COUNTRY   PIC X(2).
               10  LS-CONDITION        PIC X(12).
               10  LS-SERIAL-STATUS    PIC X(12).
               10  LS-USABLE           PIC X.
                   88  LS-IS-USABLE              VALUE "Y".
               10  LS-CREATED          PIC X.
               10  LS-TRACE-CNT        PIC 9(3).
               10  LS-TRACE OCCURS 25 TIMES.
                   15  LS-T-MOVE-ID    PIC S9(17).
                   15  LS-T-TYPE       PIC X(14).
                   15  LS-T-DATE       PIC 9(8).
                   15  LS-T-QTY        PIC S9(13)V9(6).
                   15  LS-T-SITE       PIC X(10).
                   15  LS-T-LOCATION   PIC X(20).
                   15  LS-T-DOC-TYPE   PIC X(20).
                   15  LS-T-DOC-REF    PIC X(40).
                   15  LS-T-USER       PIC X(20).
