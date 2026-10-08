      *>================================================================
      *> WHATIFCA  - COMMAREA FOR WHATIF, SCENARIO COSTING
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : COSTING AND PROFITABILITY
      *>
      *> A SCENARIO COMPARES ALTERNATIVE QUANTITIES, PRICES, REVISIONS,
      *> SUPPLY CHOICES AND RATES.  IT NEVER RESERVES STOCK, RELEASES A
      *> KIT, WRITES A COST LAYER OR ACTIVATES A STANDARD (CST-023).
      *> EVERY PROGRAM IT CALLS IS CALLED WITH ITS COMMIT FLAG SET TO
      *> "N" AND NO DATABASE TRANSACTION IS EVER OPENED HERE.
      *>
      *> CA-FUNCTION VALUES
      *>   SCENARIO  RUN ONE SCENARIO - LOT SIZE, COST SET, RATES
      *>   COMPARE   RUN A BASE CASE AND UP TO FIVE VARIANTS AND SHOW
      *>             THE DIFFERENCE AGAINST THE BASE
      *>   MARGINWI  SCENARIO COST COMPARED WITH A TARGET SELLING PRICE
      *>
      *> REASON CODES
      *>   CST1201 COMPANY OR ITEM MISSING
      *>   CST1202 AT LEAST ONE VARIANT IS REQUIRED
      *>   CST1203 LOT SIZE MUST BE POSITIVE
      *>   CST1204 SCENARIO ROLL-UP INCOMPLETE
      *>   CST1205 TOO MANY VARIANTS
      *>   CST1206 UNSUPPORTED FUNCTION
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1999-02-15 DJM  CR0733 ORIGINAL WHAT-IF COSTING
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT EFFECTIVE DATES
      *> 2026-10-06 STERP CST-023 NO LIVE SIDE EFFECTS
      *>================================================================
      *> REQ: CST-023
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  WI-BODY.
               10  WI-ITEM-CODE        PIC X(40).
               10  WI-REVISION         PIC X(4).
               10  WI-SCENARIO-REF     PIC X(30).
               10  WI-EFF-DATE         PIC 9(8).
               10  WI-BASE-COST-SET    PIC X(20).
               10  WI-BASE-LOT-SIZE    PIC S9(13)V9(6).
               10  WI-BASE-WORK-CTR    PIC X(20).
               10  WI-TARGET-PRICE     PIC S9(11)V9(6).
               10  WI-SIDE-EFFECTS     PIC X.
                   88  WI-NO-SIDE-EFFECTS        VALUE "N".
      *>       ---- base case result ----
               10  WI-BASE-TOTAL       PIC S9(13)V9(2).
               10  WI-BASE-UNIT        PIC S9(11)V9(6).
               10  WI-BASE-COMPLETE    PIC X(12).
      *>       ---- variants ----
               10  WI-VARIANT-CNT      PIC S9(4).
               10  WI-VARIANTS OCCURS 6 TIMES.
                   15  WI-V-LABEL      PIC X(30).
                   15  WI-V-COST-SET   PIC X(20).
                   15  WI-V-LOT-SIZE   PIC S9(13)V9(6).
                   15  WI-V-WORK-CTR   PIC X(20).
                   15  WI-V-MAT-FACTOR PIC S9(5)V9(4).
                   15  WI-V-RATE-FACTR PIC S9(5)V9(4).
                   15  WI-V-BOUNDARY   PIC X(8).
                   15  WI-V-MATERIAL   PIC S9(13)V9(2).
                   15  WI-V-CONVERSION PIC S9(13)V9(2).
                   15  WI-V-OVERHEAD   PIC S9(13)V9(2).
                   15  WI-V-TOTAL      PIC S9(13)V9(2).
                   15  WI-V-UNIT       PIC S9(11)V9(6).
                   15  WI-V-DIFF       PIC S9(13)V9(2).
                   15  WI-V-DIFF-PCT   PIC S9(7)V9(4).
                   15  WI-V-MARGIN-PCT PIC S9(7)V9(4).
                   15  WI-V-COMPLETE   PIC X(12).
                   15  WI-V-WARNING    PIC X(60).
               10  WI-BEST-VARIANT     PIC S9(4).
               10  WI-WARNING          PIC X(80).
