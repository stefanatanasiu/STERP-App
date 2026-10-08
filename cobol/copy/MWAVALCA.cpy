      *>================================================================
      *> MWAVALCA - COMMAREA FOR MWAVAL, MOVING WEIGHTED AVERAGE
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : COSTING AND PROFITABILITY
      *>
      *>   new average = (existing carrying value + incoming value)
      *>                 / (existing quantity + received quantity)
      *>
      *> A ZERO OR NEGATIVE DENOMINATOR DOES NOT INVENT A COST, IT IS
      *> AN ERROR (CST-006).  NEGATIVE PHYSICAL STOCK IS PROHIBITED.
      *> A ZERO CLOSING QUANTITY MUST NOT RETAIN AN UNEXPLAINED VALUE;
      *> THE RESIDUAL IS RETURNED IN MW-RESIDUAL-GBP SO THAT THE
      *> CALLER CAN POST IT UNDER AN AUTHORISED POLICY (D-22).
      *>
      *> WORKED EXAMPLE C2 OF THE SPECIFICATION
      *>   100 UNITS WORTH 400.00, RECEIVE 50 WORTH 350.00
      *>   NEW AVERAGE 750 / 150 = 5.00
      *>   ISSUE 40 COSTS 200.00, REMAINING 110 WORTH 550.00
      *>
      *> CA-FUNCTION VALUES
      *>   RECEIPT   RECALCULATE THE AVERAGE FROM AN INCOMING VALUE
      *>   ISSUE     RELIEVE MW-QUANTITY AT THE CURRENT AVERAGE
      *>   RETURN    PUT STOCK BACK AT THE ATTRIBUTABLE ORIGINAL COST
      *>             (MW-UNIT-COST) OR THE CURRENT AVERAGE IF UNKNOWN
      *>   QUERY     REPORT THE POOL WITHOUT CHANGING IT
      *>   REVALUE   VALUE ONLY CHANGE (LANDED COST, SUPPLIER CREDIT,
      *>             STANDARD ACTIVATION) - QUANTITY UNCHANGED
      *>   OPENING   ESTABLISH AN OPENING BALANCE FOR A NEW POOL
      *>
      *> REASON CODES
      *>   CST0301 COMPANY OR ITEM MISSING
      *>   CST0302 QUANTITY MUST BE POSITIVE
      *>   CST0303 ISSUE EXCEEDS QUANTITY ON HAND - NEGATIVE REFUSED
      *>   CST0304 ZERO DENOMINATOR - NO AVERAGE CAN BE CALCULATED
      *>   CST0305 POOL NOT KNOWN
      *>   CST0306 VALUE ONLY CHANGE NEEDS A NON ZERO QUANTITY
      *>   CST0307 UNSUPPORTED FUNCTION
      *>   CST0308 POOL IS NOT ON THE MOVING AVERAGE METHOD
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-05-30 RJH  CR0519 ORIGINAL WEIGHTED AVERAGE
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT EFFECTIVE DATES
      *> 2012-09-14 DJM  CR1904 HISTORY ROW FOR EVERY RECALCULATION
      *> 2026-10-06 STERP CST-006 CST-009 RESIDUAL ON ZERO QUANTITY
      *>================================================================
      *> REQ: CST-006, CST-009, CST-024   AC: AC-C2
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  MW-BODY.
               10  MW-ITEM-CODE        PIC X(40).
               10  MW-REVISION         PIC X(4).
               10  MW-POOL-SITE        PIC X(10).
               10  MW-QUANTITY         PIC S9(13)V9(6).
               10  MW-VALUE-GBP        PIC S9(13)V9(2).
               10  MW-UNIT-COST        PIC S9(11)V9(6).
               10  MW-EFF-DATE         PIC 9(8).
               10  MW-CHANGE-TYPE      PIC X(14).
               10  MW-DOC-TYPE         PIC X(20).
               10  MW-DOC-REF          PIC X(40).
               10  MW-MOVEMENT-ID      PIC S9(15).
               10  MW-COMMIT           PIC X.
                   88  MW-DO-COMMIT              VALUE "Y".
      *>       ---- before and after (CST-024 breakdown) ----
               10  MW-QTY-BEFORE       PIC S9(13)V9(6).
               10  MW-VALUE-BEFORE     PIC S9(13)V9(2).
               10  MW-AVG-BEFORE       PIC S9(11)V9(6).
               10  MW-QTY-CHANGE       PIC S9(13)V9(6).
               10  MW-VALUE-CHANGE     PIC S9(13)V9(2).
               10  MW-QTY-AFTER        PIC S9(13)V9(6).
               10  MW-VALUE-AFTER      PIC S9(13)V9(2).
               10  MW-AVG-AFTER        PIC S9(11)V9(6).
               10  MW-ISSUE-VALUE      PIC S9(13)V9(2).
               10  MW-RESIDUAL-GBP     PIC S9(13)V9(2).
               10  MW-COST-METHOD      PIC X(4).
               10  MW-HISTORY-ID       PIC S9(15).
               10  MW-WARNING          PIC X(80).
