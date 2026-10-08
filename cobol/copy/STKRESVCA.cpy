      *>================================================================
      *> STKRESVCA - COMMAREA for STKRESV, stock reservation primitives.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *>
      *> A reservation is demand specific and quantity bounded.  It is
      *> not consumption: the physical stock stays where it is until an
      *> issue or a despatch moves it.  Competing reservations can
      *> never exceed the eligible quantity, so the arithmetic is done
      *> inside inv.usp_ReserveStock under UPDLOCK (ITM-014, COM-005).
      *>
      *> CA-FUNCTION values
      *>   RESERVE   reserve SR-QTY-REQUESTED for the demand.  When
      *>             SR-ALLOW-PARTIAL is "Y" the service reserves what
      *>             is free and returns 04 with the shortfall.
      *>   RELEASE   release the outstanding reservation
      *>   CONSUME   record that SR-QTY-REQUESTED was issued against
      *>             the reservation (final use revalidates first)
      *>   REVALID   revalidate the demand's reservations against the
      *>             material now eligible; reservations that point at
      *>             blocked, quarantined or expired stock are marked
      *>             INVALID and an exception is raised for the demand
      *>             owner, who must reallocate
      *>   ENQUIRE   list the reservations for the demand or the item
      *>
      *> REASON CODES
      *>   RSV0001 item not known in this company
      *>   RSV0002 requested quantity is zero or negative
      *>   RSV0003 insufficient free stock for the whole request
      *>   RSV0004 demand reference is required
      *>   RSV0005 reservation not found
      *>   RSV0006 reservation is no longer valid - reallocate
      *>   RSV0007 consumption exceeds the reserved quantity
      *>   RSV0008 unsupported function
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-02-25 RJH  CR0688 ALLOCATION OF FREE STOCK TO ORDERS
      *> 2008-10-14 KMP  CR2105 OVERRESERVATION UNDER CONCURRENT USERS
      *> 2026-10-05 STERP ITM-014 REVALIDATION ON MATERIAL CHANGE
      *>================================================================
      *> REQ: ITM-014
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SR-BODY.
               10  SR-ITEM-CODE        PIC X(40).
               10  SR-REVISION         PIC X(4).
               10  SR-SITE             PIC X(10).
               10  SR-WAREHOUSE        PIC X(20).
               10  SR-LOCATION         PIC X(20).
               10  SR-LOT-CODE         PIC X(30).
               10  SR-SERIAL-NO        PIC X(40).
               10  SR-DEMAND-TYPE      PIC X(12).
               10  SR-DEMAND-REF       PIC X(40).
               10  SR-DEMAND-LINE      PIC S9(9) COMP-5.
               10  SR-FIRMNESS         PIC X(10).
               10  SR-PRIORITY         PIC 9(2).
               10  SR-REQUIRED-DATE    PIC 9(8).
               10  SR-QTY-REQUESTED    PIC S9(13)V9(6).
               10  SR-QTY-RESERVED     PIC S9(13)V9(6).
               10  SR-QTY-SHORTFALL    PIC S9(13)V9(6).
               10  SR-QTY-FREE-BEFORE  PIC S9(13)V9(6).
               10  SR-QTY-FREE-AFTER   PIC S9(13)V9(6).
               10  SR-QTY-CONSUMED     PIC S9(13)V9(6).
               10  SR-ALLOW-PARTIAL    PIC X.
                   88  SR-PARTIAL-OK             VALUE "Y".
               10  SR-RESERVATION-ID   PIC S9(17).
               10  SR-INVALID-CNT      PIC 9(3).
               10  SR-STATUS           PIC X(10).
               10  SR-INVALID-REASON   PIC X(200).
               10  SR-LINE-CNT         PIC 9(3).
               10  SR-LINES OCCURS 25 TIMES.
                   15  SR-L-RES-ID     PIC S9(17).
                   15  SR-L-ITEM       PIC X(40).
                   15  SR-L-SITE       PIC X(10).
                   15  SR-L-LOCATION   PIC X(20).
                   15  SR-L-LOT        PIC X(30).
                   15  SR-L-DEM-TYPE   PIC X(12).
                   15  SR-L-DEM-REF    PIC X(40).
                   15  SR-L-DEM-LINE   PIC S9(9) COMP-5.
                   15  SR-L-QTY        PIC S9(13)V9(6).
                   15  SR-L-PICKED     PIC S9(13)V9(6).
                   15  SR-L-CONSUMED   PIC S9(13)V9(6).
                   15  SR-L-STATUS     PIC X(10).
                   15  SR-L-FIRMNESS   PIC X(10).
                   15  SR-L-REQUIRED   PIC 9(8).
