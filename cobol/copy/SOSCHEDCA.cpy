      *>================================================================
      *> SOSCHEDCA - COMMAREA for SOSCHED, delivery schedules.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : SALES ORDER MANAGEMENT
      *>
      *> A line's schedules always account for the whole active order
      *> quantity: schedule total = AcceptedQty - CancelledQty.  The
      *> requested date and the ORIGINAL promise are service evidence
      *> and are never overwritten; only CurrentPromiseDate moves, and
      *> only with a reason and a bumped ChangeCount (SO-008).
      *>
      *>   CA-FUNCTION values  (CA-FUNCTION is X(8), so a longer name
      *>   arrives truncated - REPROMISE is matched as REPROMIS)
      *>   SPLIT      replace the line's open schedules with the
      *>              SS-SPLITS rows (date and destination per row).
      *>              The split total must equal the active order
      *>              quantity, otherwise 08 / SCH0002.
      *>   REPROMISE  move SS-SCHEDULE-NO to SS-NEW-PROMISE with
      *>              SS-CHANGE-REASON and ChangeCount + 1.
      *>              RequestedDate and OriginalPromiseDate are left
      *>              exactly as they were.
      *>   LIST       return the line's schedules in SS-LIST
      *>   CANCEL     cancel one open schedule (the despatched part of
      *>              a schedule is never cancelled)
      *>
      *> REASON CODES
      *>   SCH0001 a company, an order and a line are required
      *>   SCH0002 the schedule total does not equal the active order
      *>           quantity
      *>   SCH0003 order line not found
      *>   SCH0004 schedule not found
      *>   SCH0005 a promise date before the requested date needs a
      *>           reason
      *>   SCH0006 the destination is not a site of this company
      *>   SCH0007 a schedule quantity must be positive
      *>   SCH0008 unsupported function
      *>   SCH0009 a change reason is required to move a promise
      *>   SCH0010 a despatched schedule cannot be cancelled or
      *>           reduced below its despatched quantity
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1995-11-20 RJH  CR0392 CALL-OFF SCHEDULES BY DATE
      *> 1999-11-30 KMP  Y2K - PROMISE DATES HOLD FOUR DIGIT YEARS
      *> 2013-06-03 DLS  CR2688 KEEP THE ORIGINAL PROMISE FOR OTIF
      *> 2026-10-06 STERP SO-008 REQUESTED / ORIGINAL / CURRENT DATES
      *>================================================================
      *> REQ: SO-008
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SS-BODY.
               10  SS-ORDER-NUMBER     PIC X(30).
               10  SS-LINE-NUM         PIC S9(9) COMP-5.
               10  SS-ORDER-LINE-ID    PIC S9(17).
               10  SS-SCHEDULE-NO      PIC S9(9) COMP-5.
               10  SS-SCHEDULE-ID      PIC S9(17).
               10  SS-ITEM-CODE        PIC X(40).
               10  SS-UOM-CODE         PIC X(10).
      *>       quantities, all in the order line's selling unit
               10  SS-ACCEPTED-QTY     PIC S9(13)V9(6).
               10  SS-CANCELLED-QTY    PIC S9(13)V9(6).
               10  SS-DESPATCHED-QTY   PIC S9(13)V9(6).
               10  SS-ACTIVE-QTY       PIC S9(13)V9(6).
               10  SS-SCHEDULE-TOTAL   PIC S9(13)V9(6).
               10  SS-QUANTITY         PIC S9(13)V9(6).
               10  SS-VARIANCE-QTY     PIC S9(13)V9(6).
      *>       the three dates that must all survive (SO-008)
               10  SS-REQUESTED-DATE   PIC 9(8).
               10  SS-ORIG-PROMISE     PIC 9(8).
               10  SS-CURR-PROMISE     PIC 9(8).
               10  SS-NEW-PROMISE      PIC 9(8).
               10  SS-CHANGE-REASON    PIC X(200).
               10  SS-CHANGE-COUNT     PIC S9(9) COMP-5.
               10  SS-EARLY-REASON     PIC X(200).
               10  SS-SITE-CODE        PIC X(10).
               10  SS-ADDRESS-REF      PIC X(10).
               10  SS-FULFIL-METHOD    PIC X(12).
               10  SS-PRIORITY         PIC 9(2).
               10  SS-STATUS           PIC X(10).
               10  SS-CANCEL-REASON    PIC X(200).
               10  SS-SCHED-CREATED    PIC S9(9) COMP-5.
               10  SS-SCHED-CLOSED     PIC S9(9) COMP-5.
      *>       requested split - supplied by the caller
               10  SS-SPLIT-CNT        PIC 9(3).
               10  SS-SPLITS OCCURS 20 TIMES.
                   15  SS-S-SCHED-NO   PIC S9(9) COMP-5.
                   15  SS-S-QTY        PIC S9(13)V9(6).
                   15  SS-S-SITE       PIC X(10).
                   15  SS-S-ADDRESS    PIC X(10).
                   15  SS-S-REQUESTED  PIC 9(8).
                   15  SS-S-PROMISE    PIC 9(8).
                   15  SS-S-METHOD     PIC X(12).
                   15  SS-S-PRIORITY   PIC 9(2).
                   15  SS-S-REASON     PIC X(200).
      *>       schedules read back - LIST, SPLIT and REPROMISE all
      *>       refresh this so the caller can see the evidence
               10  SS-LIST-CNT         PIC 9(3).
               10  SS-LIST OCCURS 20 TIMES.
                   15  SS-X-SCHED-NO   PIC S9(9) COMP-5.
                   15  SS-X-SCHED-ID   PIC S9(17).
                   15  SS-X-QTY        PIC S9(13)V9(6).
                   15  SS-X-DESPATCHED PIC S9(13)V9(6).
                   15  SS-X-SITE       PIC X(10).
                   15  SS-X-ADDRESS    PIC X(10).
                   15  SS-X-REQUESTED  PIC 9(8).
                   15  SS-X-ORIG-PROM  PIC 9(8).
                   15  SS-X-CURR-PROM  PIC 9(8).
                   15  SS-X-CHANGE-CNT PIC S9(9) COMP-5.
                   15  SS-X-REASON     PIC X(200).
                   15  SS-X-METHOD     PIC X(12).
                   15  SS-X-PRIORITY   PIC 9(2).
                   15  SS-X-STATUS     PIC X(10).
