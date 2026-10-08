      *>================================================================
      *> PICKGENCA - COMMAREA for PICKGEN, pick list generation and
      *>             confirmation.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : SALES ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> SO-012.  PICKING IS NOT DESPATCH.  A pick list is an
      *> instruction to a person in a warehouse.  It identifies the
      *> item, the warehouse, the location, the lot or serial and the
      *> reservation it draws on - and it changes no balance, creates
      *> no cost of sale and sets no despatched quantity.  Only
      *> DESPATCH does those, and it does them in one transaction.
      *>
      *> A short pick is a controlled, reasoned event: the picked
      *> quantity and the short quantity are both recorded and the
      *> outstanding demand SURVIVES so the back order is still real.
      *>
      *> CA-FUNCTION values
      *>   GENERATE  build so.PickList and so.PickListLine from the
      *>             released delivery schedules of PK-ORDER-NUMBER.
      *>             The reference comes from NUMGEN series SOPICK.
      *>   CONFIRM   record PK-PICKED-QTY, any PK-SHORT-QTY with
      *>             PK-SHORT-REASON, and the lot or serial actually
      *>             taken for pick line PK-LINE-NUM
      *>   SUBST     pick an alternate item or lot.  Outside policy
      *>             this needs an approval id, checked through
      *>             SUBSTCHK; without one the answer is CA-HOLD with
      *>             reason PCK0006 and nothing is written.
      *>   PRINT     increment PrintCount.  Printing or reprinting is
      *>             not a release, not a reservation and not an
      *>             override (SO-011, SO-022).
      *>   CANCEL    cancel the pick list, leaving the demand intact
      *>
      *> REASON CODES
      *>   PCK0001 company, order or pick list reference required
      *>   PCK0002 pick list or pick line not known
      *>   PCK0003 nothing releasable to pick on that order
      *>   PCK0004 picked quantity is negative or exceeds the required
      *>   PCK0005 a short pick needs a reason
      *>   PCK0006 substitution outside policy needs an approval id
      *>   PCK0007 the pick list is cancelled or already complete
      *>   PCK0008 unsupported function
      *>   PCK0009 lot or serial is required for this item
      *>   PCK0010 more lines than the table can carry
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-06-29 DJM  CR0944 WAREHOUSE PICKING (ACUCOBOL)
      *> 2001-09-04 KMP  CR1051 SHORT PICK KEEPS THE BACK ORDER
      *> 2005-11-30 ANW  CR1320 SUBSTITUTION NEEDS AN APPROVAL
      *> 2026-10-06 STERP SO-012 PICKING HAS NO STOCK OR COGS EFFECT
      *>================================================================
      *> REQ: SO-012
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  PK-BODY.
               10  PK-ORDER-NUMBER     PIC X(30).
               10  PK-PICKLIST-REF     PIC X(30).
               10  PK-PICKLIST-ID      PIC S9(17).
               10  PK-SITE             PIC X(10).
               10  PK-WAREHOUSE        PIC X(20).
               10  PK-CUSTOMER         PIC X(20).
               10  PK-REQUIRED-DATE    PIC 9(8).
               10  PK-PICKER-USER      PIC X(20).
               10  PK-PRINT-COUNT      PIC S9(9) COMP-5.
               10  PK-STATUS           PIC X(12).
      *>       CONFIRM / SUBST work on one pick line
               10  PK-LINE-NUM         PIC S9(9) COMP-5.
               10  PK-ORDER-LINE       PIC S9(9) COMP-5.
               10  PK-SCHEDULE-NO      PIC S9(9) COMP-5.
               10  PK-ITEM-CODE        PIC X(40).
               10  PK-REVISION         PIC X(4).
               10  PK-REQUIRED-QTY     PIC S9(13)V9(6).
               10  PK-PICKED-QTY       PIC S9(13)V9(6).
               10  PK-SHORT-QTY        PIC S9(13)V9(6).
               10  PK-SHORT-REASON     PIC X(200).
               10  PK-OUTSTANDING-QTY  PIC S9(13)V9(6).
               10  PK-LOCATION         PIC X(20).
               10  PK-LOT-CODE         PIC X(30).
               10  PK-SERIAL-NO        PIC X(40).
               10  PK-EXPIRY-DATE      PIC 9(8).
               10  PK-RESERVATION-ID   PIC S9(17).
      *>       substitution - a proposal, never a silent replacement
               10  PK-SUBST-ITEM       PIC X(40).
               10  PK-SUBST-REVISION   PIC X(4).
               10  PK-SUBST-LOT        PIC X(30).
               10  PK-SUBST-APPR-ID    PIC S9(17).
               10  PK-SUBST-APPR-REF   PIC X(40).
               10  PK-SUBST-RATIO      PIC S9(9)V9(9).
               10  PK-SUBST-QTY        PIC S9(13)V9(6).
               10  PK-SUBST-IN-POLICY  PIC X.
                   88  PK-SUBST-OK               VALUE "Y".
               10  PK-CANCEL-REASON    PIC X(200).
      *>       evidence that picking moved nothing.  DESPATCH sets the
      *>       despatched quantity and the cost of sale, not PICKGEN.
               10  PK-STOCK-MOVED      PIC X.
               10  PK-COGS-CREATED     PIC X.
               10  PK-DESPATCHED-QTY   PIC S9(13)V9(6).
               10  PK-GENERATED-CNT    PIC S9(9) COMP-5.
               10  PK-CONFIRMED-CNT    PIC S9(9) COMP-5.
               10  PK-LINE-CNT         PIC 9(3).
               10  PK-LINES OCCURS 40 TIMES.
                   15  PK-L-LINE-NUM   PIC S9(9) COMP-5.
                   15  PK-L-ORDER-LINE PIC S9(9) COMP-5.
                   15  PK-L-SCHEDULE   PIC S9(9) COMP-5.
                   15  PK-L-SOL-ID     PIC S9(17).
                   15  PK-L-ITEM       PIC X(40).
                   15  PK-L-REVISION   PIC X(4).
                   15  PK-L-REQUIRED   PIC S9(13)V9(6).
                   15  PK-L-PICKED     PIC S9(13)V9(6).
                   15  PK-L-SHORT      PIC S9(13)V9(6).
                   15  PK-L-WHSE       PIC X(20).
                   15  PK-L-LOCATION   PIC X(20).
                   15  PK-L-LOT        PIC X(30).
                   15  PK-L-SERIAL     PIC X(40).
                   15  PK-L-EXPIRY     PIC 9(8).
                   15  PK-L-RES-ID     PIC S9(17).
                   15  PK-L-SUBST-ITM  PIC X(40).
                   15  PK-L-SHORT-WHY  PIC X(100).
                   15  PK-L-STATUS     PIC X(12).
