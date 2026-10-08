      *>================================================================
      *> LANDEDCA  - COMMAREA FOR LANDED, LANDED COST ATTRIBUTION
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : COSTING AND PROFITABILITY
      *>
      *> AN ATTRIBUTABLE ACQUISITION CHARGE IS ATTRIBUTED EXACTLY ONCE
      *> (CST-008).  WHEN THE ACTUAL CHARGE ARRIVES, ONLY THE
      *> DIFFERENCE AGAINST THE ESTIMATE IS SPREAD, AND IT FOLLOWS THE
      *> CURRENT CARRYING DESTINATION OF EACH RECEIPT LINE - STOCK, WIP
      *> OR ALREADY SOLD.  STOCK, WIP, FINISHED GOODS AND SOLD PORTIONS
      *> TOGETHER RECEIVE EXACTLY THE ATTRIBUTABLE ADJUSTMENT, WITHOUT
      *> OVERLAP.
      *>
      *> WORKED EXAMPLE C4 OF THE SPECIFICATION
      *>   100 UNITS, GOODS 1,000.00, ESTIMATED FREIGHT 80.00
      *>   PROVISIONAL UNIT COST 10.80, SELL 40 -> COGS 432.00
      *>   ACTUAL FREIGHT 100.00, A 20.00 INCREASE
      *>   8.00 TO THE 40 SOLD, 12.00 TO THE 60 REMAINING
      *>   FINAL COGS 440.00, INVENTORY 660.00, COMBINED 1,100.00
      *>
      *> CA-FUNCTION VALUES
      *>   ESTIMATE  ALLOCATE AN ESTIMATED CHARGE OVER RECEIPT LINES
      *>   ACTUAL    ALLOCATE THE DIFFERENCE WHEN THE ACTUAL ARRIVES
      *>   SPLIT     SPLIT ONE LINE'S ADJUSTMENT OVER STOCK/WIP/COGS
      *>   ENQUIRE   LANDED COST RECONCILIATION FOR A CHARGE
      *>
      *> REASON CODES
      *>   CST1301 COMPANY OR CHARGE REFERENCE MISSING
      *>   CST1302 CHARGE NOT FOUND
      *>   CST1303 ALLOCATION BASIS NOT RECOGNISED
      *>   CST1304 BASIS TOTAL IS ZERO - NOTHING TO ALLOCATE OVER
      *>   CST1305 CHARGE IS NOT ELIGIBLE TO BE CAPITALISED
      *>   CST1306 CHARGE ALREADY ACTUALISED - NO SECOND ATTRIBUTION
      *>   CST1307 TOO MANY RECEIPT LINES
      *>   CST1308 UNSUPPORTED FUNCTION
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1999-05-20 RJH  CR0756 ORIGINAL FREIGHT ABSORPTION
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT SHIPMENT DATES
      *> 2021-01-01 DJM  CR2190 BREXIT - CUSTOMS DUTY AND CLEARANCE
      *> 2026-10-06 STERP CST-008 DIFFERENCE ONLY ON ACTUALISATION
      *>================================================================
      *> REQ: CST-004, CST-008   AC: AC-C4
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  LD-BODY.
               10  LD-CHARGE-REF       PIC X(30).
               10  LD-SHIPMENT-REF     PIC X(30).
               10  LD-CATEGORY         PIC X(14).
               10  LD-BASIS            PIC X(10).
                   88  LD-ON-VALUE               VALUE "VALUE".
                   88  LD-ON-WEIGHT              VALUE "WEIGHT".
                   88  LD-ON-VOLUME              VALUE "VOLUME".
                   88  LD-ON-QUANTITY            VALUE "QUANTITY".
               10  LD-CURRENCY         PIC X(3).
               10  LD-FX-RATE          PIC S9(9)V9(8).
               10  LD-ESTIMATED        PIC S9(13)V9(2).
               10  LD-ACTUAL           PIC S9(13)V9(2).
               10  LD-ESTIMATED-GBP    PIC S9(13)V9(2).
               10  LD-ACTUAL-GBP       PIC S9(13)V9(2).
               10  LD-DELTA-GBP        PIC S9(13)V9(2).
               10  LD-ALLOCATION-DATE  PIC 9(8).
               10  LD-ELIGIBLE         PIC X.
               10  LD-COMMIT           PIC X.
      *>       ---- results ----
               10  LD-ALLOCATED-GBP    PIC S9(13)V9(2).
               10  LD-TO-INVENTORY     PIC S9(13)V9(2).
               10  LD-TO-WIP           PIC S9(13)V9(2).
               10  LD-TO-COGS          PIC S9(13)V9(2).
               10  LD-TO-VARIANCE      PIC S9(13)V9(2).
               10  LD-BASIS-TOTAL      PIC S9(13)V9(6).
               10  LD-STATUS           PIC X(12).
               10  LD-LANDED-ID        PIC S9(9).
               10  LD-WARNING          PIC X(80).
      *>       ---- receipt lines to carry the charge ----
               10  LD-LINE-CNT         PIC S9(4).
               10  LD-LINES OCCURS 25 TIMES.
                   15  LD-L-NUM        PIC S9(9).
                   15  LD-L-RECEIPT    PIC X(30).
                   15  LD-L-RCPT-LINE  PIC S9(9).
                   15  LD-L-ITEM       PIC X(40).
                   15  LD-L-BASIS-VAL  PIC S9(13)V9(6).
                   15  LD-L-QTY-RECVD  PIC S9(13)V9(6).
                   15  LD-L-QTY-LEFT   PIC S9(13)V9(6).
                   15  LD-L-QTY-SOLD   PIC S9(13)V9(6).
                   15  LD-L-QTY-WIP    PIC S9(13)V9(6).
                   15  LD-L-EST-SHARE  PIC S9(13)V9(2).
                   15  LD-L-ACT-SHARE  PIC S9(13)V9(2).
                   15  LD-L-DELTA      PIC S9(13)V9(2).
                   15  LD-L-TO-STOCK   PIC S9(13)V9(2).
                   15  LD-L-TO-WIP     PIC S9(13)V9(2).
                   15  LD-L-TO-COGS    PIC S9(13)V9(2).
                   15  LD-L-UNIT-COST  PIC S9(11)V9(6).
