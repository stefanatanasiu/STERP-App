      *>================================================================
      *> SORESVCA - COMMAREA for SORESV, the sales reservation driver.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : SALES ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> SO-009.  SORESV owns the SALES rules around a reservation -
      *> which stock is saleable, which lot may still be shipped, what
      *> the customer's remaining shelf life demands and how a
      *> shortfall is reported to the order desk.  It does NOT own the
      *> arithmetic.  The question "is there enough free stock and may
      *> I have it?" is answered once, inside inv.usp_ReserveStock
      *> under UPDLOCK, reached through CALL "STKRESV".  Two orders
      *> therefore can never reserve the same available units twice.
      *>
      *> Nothing in this module may read free stock, decide for itself
      *> and then write a reservation; the 1998 version did exactly
      *> that and oversold the RDG warehouse every month end (CR1188).
      *>
      *> CA-FUNCTION values
      *>   RESERVE   reserve SV-QTY-REQUESTED for the sales demand.
      *>             Demand type SALESORDER, demand reference the
      *>             order number, demand line the order line number.
      *>             SV-ALLOW-PARTIAL = "Y" takes what is free and
      *>             reports the shortfall with return code 04.
      *>   RELEASE   release the outstanding reservation for the line
      *>   CONSUME   record the issue of SV-QTY-REQUESTED against the
      *>             reservation (STKRESV revalidates first)
      *>   REVALID   revalidate the line's reservations against the
      *>             material now eligible
      *>   ENQUIRE   list the reservations held for the demand
      *>
      *> REASON CODES
      *>   SRV0001 company, order and item are required
      *>   SRV0002 quantity must be greater than zero
      *>   SRV0003 reserved short of the requested quantity
      *>   SRV0004 order or order line not known
      *>   SRV0005 the order line is cancelled or closed
      *>   SRV0006 quarantined, expired, rejected or third party owned
      *>           stock is not ordinary saleable stock
      *>   SRV0007 lot expires before the planned despatch date plus
      *>           the customer's remaining shelf life requirement
      *>   SRV0008 unsupported function
      *>   SRV0009 reservations are no longer valid - reallocate
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-04-02 RJH  CR0918 SALES ALLOCATION (ACUCOBOL)
      *> 2008-10-14 KMP  CR1188 OVERSELL - ARITHMETIC MOVED INTO THE
      *>                 DATABASE UNDER UPDLOCK, NEVER COMPUTED HERE
      *> 2012-02-20 ANW  CR2740 CUSTOMER REMAINING SHELF LIFE
      *> 2026-10-06 STERP SO-009 SALEABILITY AND EXPIRY AT DESPATCH
      *>================================================================
      *> REQ: SO-009
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SV-BODY.
               10  SV-ORDER-NUMBER     PIC X(30).
               10  SV-LINE-NUM         PIC S9(9) COMP-5.
               10  SV-SCHEDULE-NO      PIC S9(9) COMP-5.
               10  SV-ITEM-CODE        PIC X(40).
               10  SV-REVISION         PIC X(4).
               10  SV-SITE             PIC X(10).
               10  SV-WAREHOUSE        PIC X(20).
               10  SV-LOCATION         PIC X(20).
               10  SV-LOT-CODE         PIC X(30).
               10  SV-SERIAL-NO        PIC X(40).
               10  SV-CUSTOMER         PIC X(20).
      *>       the demand identity handed to STKRESV unchanged
               10  SV-DEMAND-TYPE      PIC X(12).
               10  SV-DEMAND-REF       PIC X(40).
               10  SV-DEMAND-LINE      PIC S9(9) COMP-5.
               10  SV-FIRMNESS         PIC X(10).
               10  SV-PRIORITY         PIC 9(2).
               10  SV-REQUIRED-DATE    PIC 9(8).
      *>       quantities - every one of them produced by STKRESV
               10  SV-QTY-REQUESTED    PIC S9(13)V9(6).
               10  SV-QTY-RESERVED     PIC S9(13)V9(6).
               10  SV-QTY-SHORTFALL    PIC S9(13)V9(6).
               10  SV-QTY-CONSUMED     PIC S9(13)V9(6).
               10  SV-QTY-FREE-BEFORE  PIC S9(13)V9(6).
               10  SV-QTY-FREE-AFTER   PIC S9(13)V9(6).
               10  SV-ALLOW-PARTIAL    PIC X.
                   88  SV-PARTIAL-OK             VALUE "Y".
               10  SV-RESERVATION-ID   PIC S9(17).
               10  SV-STATUS           PIC X(10).
               10  SV-INVALID-CNT      PIC 9(3).
               10  SV-INVALID-REASON   PIC X(200).
      *>       saleability - spec 05 section 6.  None of this stock is
      *>       ordinary saleable stock (reason SRV0006).
               10  SV-QTY-QUARANTINE   PIC S9(13)V9(6).
               10  SV-QTY-EXPIRED      PIC S9(13)V9(6).
               10  SV-QTY-REJECTED     PIC S9(13)V9(6).
               10  SV-QTY-THIRDPARTY   PIC S9(13)V9(6).
               10  SV-QTY-UNSALEABLE   PIC S9(13)V9(6).
               10  SV-UNSALEABLE-TEXT  PIC X(200).
      *>       lot expiry is judged at the planned despatch date plus
      *>       the customer's remaining shelf life requirement
               10  SV-DESPATCH-DATE    PIC 9(8).
               10  SV-SHELF-DAYS-REQ   PIC S9(5).
               10  SV-SHELF-TEST-DATE  PIC 9(8).
               10  SV-LOT-EXPIRY       PIC 9(8).
               10  SV-DAYS-REMAINING   PIC S9(7).
               10  SV-LOT-USABLE       PIC X.
                   88  SV-LOT-IS-USABLE          VALUE "Y".
               10  SV-EXPIRY-CHECKED   PIC X.
      *>       what the order desk is told
               10  SV-ORDER-STATUS     PIC X(14).
               10  SV-LINE-STATUS      PIC X(10).
               10  SV-ACCEPTED-QTY     PIC S9(13)V9(6).
               10  SV-CANCELLED-QTY    PIC S9(13)V9(6).
               10  SV-DESPATCHED-QTY   PIC S9(13)V9(6).
               10  SV-OUTSTANDING-QTY  PIC S9(13)V9(6).
               10  SV-LINE-CNT         PIC 9(3).
               10  SV-LINES OCCURS 25 TIMES.
                   15  SV-L-RES-ID     PIC S9(17).
                   15  SV-L-ITEM       PIC X(40).
                   15  SV-L-SITE       PIC X(10).
                   15  SV-L-LOCATION   PIC X(20).
                   15  SV-L-LOT        PIC X(30).
                   15  SV-L-DEM-REF    PIC X(40).
                   15  SV-L-DEM-LINE   PIC S9(9) COMP-5.
                   15  SV-L-QTY        PIC S9(13)V9(6).
                   15  SV-L-PICKED     PIC S9(13)V9(6).
                   15  SV-L-CONSUMED   PIC S9(13)V9(6).
                   15  SV-L-STATUS     PIC X(10).
                   15  SV-L-REQUIRED   PIC 9(8).
