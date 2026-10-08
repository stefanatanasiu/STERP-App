      *>================================================================
      *> FIFOVALCA - COMMAREA FOR FIFOVAL, FIRST IN FIRST OUT VALUATION
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : COSTING AND PROFITABILITY
      *>
      *> AN ISSUE CONSUMES VALUE FROM THE OLDEST ELIGIBLE FINANCIAL
      *> RECEIPT LAYERS OF THE POOL (COMPANY + ITEM [+ REVISION]).
      *> THE PHYSICAL PICKING ORDER AND THE FINANCIAL CONSUMPTION
      *> ORDER ARE DIFFERENT THINGS AND MUST NOT BE CONFUSED.
      *> TIES ARE BROKEN BY THE STABLE LAYER SEQUENCE.
      *>
      *> WORKED EXAMPLE C1 OF THE SPECIFICATION
      *>   RECEIVE 10 AT 5.00 THEN 10 AT 8.00, ISSUE 12
      *>   ISSUE COST 10 x 5 + 2 x 8 = 66.00, REMAINING 8 WORTH 64.00
      *>
      *> CA-FUNCTION VALUES
      *>   RECEIPT   ADD A LAYER OF FV-QUANTITY AT FV-UNIT-COST
      *>   ISSUE     CONSUME FV-QUANTITY FROM THE OLDEST LAYERS
      *>             -> FV-ISSUE-VALUE AND THE CONSUMED LAYER DETAIL
      *>   RETURN    PUT FV-QUANTITY BACK AT ITS ORIGINAL LAYER COST
      *>             (FV-LAYER-SEQ) OR AT THE OLDEST OPEN LAYER COST
      *>   QUERY     REPORT THE OPEN LAYERS AND THE POOL TOTALS
      *>   LANDADJ   ADD FV-VALUE-GBP TO THE REMAINING PART OF A LAYER
      *>
      *> FV-COMMIT "Y" WRITES THE LAYERS, "N" SIMULATES ONLY, WHICH IS
      *> HOW WHATIF AND THE VALUATION REPORT USE THIS PROGRAM.
      *>
      *> REASON CODES
      *>   CST0201 COMPANY OR ITEM MISSING
      *>   CST0202 QUANTITY MUST BE POSITIVE
      *>   CST0203 UNIT COST MUST NOT BE NEGATIVE
      *>   CST0204 NOT ENOUGH LAYER QUANTITY - NEGATIVE STOCK REFUSED
      *>   CST0205 LAYER NOT FOUND
      *>   CST0206 TOO MANY LAYERS FOR ONE ISSUE
      *>   CST0207 UNSUPPORTED FUNCTION
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-05-30 RJH  CR0518 ORIGINAL FIFO LAYERS
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT RECEIPT DATES
      *> 2008-04-02 DJM  CR1620 LANDED COST ADJUSTMENT OF OPEN LAYERS
      *> 2026-10-06 STERP CST-005 CST-009 PARTIAL LAYER CONSUMPTION
      *>================================================================
      *> REQ: CST-005, CST-009, CST-024   AC: AC-C1
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  FV-BODY.
               10  FV-ITEM-CODE        PIC X(40).
               10  FV-REVISION         PIC X(4).
               10  FV-QUANTITY         PIC S9(13)V9(6).
               10  FV-UNIT-COST        PIC S9(11)V9(6).
               10  FV-VALUE-GBP        PIC S9(13)V9(2).
               10  FV-EFF-DATE         PIC 9(8).
               10  FV-DOC-TYPE         PIC X(20).
               10  FV-DOC-REF          PIC X(40).
               10  FV-LOT-CODE         PIC X(30).
               10  FV-MOVEMENT-ID      PIC S9(15).
               10  FV-LAYER-SEQ        PIC S9(15).
               10  FV-COMMIT           PIC X.
                   88  FV-DO-COMMIT              VALUE "Y".
      *>       ---- results ----
               10  FV-ISSUE-VALUE      PIC S9(13)V9(2).
               10  FV-ISSUE-UNIT-AVG   PIC S9(11)V9(6).
               10  FV-LAYERS-USED      PIC S9(4).
               10  FV-POOL-QTY         PIC S9(13)V9(6).
               10  FV-POOL-VALUE       PIC S9(13)V9(2).
               10  FV-REMAIN-QTY       PIC S9(13)V9(6).
               10  FV-REMAIN-VALUE     PIC S9(13)V9(2).
               10  FV-NEW-LAYER-SEQ    PIC S9(15).
               10  FV-OPEN-LAYER-CNT   PIC S9(4).
               10  FV-MORE-LAYERS      PIC X.
               10  FV-LINE-CNT         PIC S9(4).
               10  FV-LINES OCCURS 30 TIMES.
                   15  FV-L-SEQ        PIC S9(15).
                   15  FV-L-DATE       PIC 9(8).
                   15  FV-L-DOC-REF    PIC X(40).
                   15  FV-L-UNIT-COST  PIC S9(11)V9(6).
                   15  FV-L-QTY-TAKEN  PIC S9(13)V9(6).
                   15  FV-L-VALUE      PIC S9(13)V9(2).
                   15  FV-L-QTY-LEFT   PIC S9(13)V9(6).
                   15  FV-L-VALUE-LEFT PIC S9(13)V9(2).
                   15  FV-L-LANDED     PIC S9(13)V9(2).
                   15  FV-L-CLOSED     PIC X.
