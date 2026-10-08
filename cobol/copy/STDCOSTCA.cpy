      *>================================================================
      *> STDCOSTCA - COMMAREA FOR STDCOST, STANDARD COST SET CONTROL
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : COSTING AND PROFITABILITY
      *>
      *> A STANDARD COST SET IS PREPARED, REVIEWED, APPROVED AND THEN
      *> ACTIVATED.  ONLY ACTIVATION REVALUES INVENTORY, AND IT DOES SO
      *> THROUGH AN EXPLICIT REVALUATION WITH ITS OWN VARIANCE.  A
      *> SIMULATION OR A MASTER PRICE EDIT CAN NEVER REVALUE STOCK OR
      *> REWRITE HISTORICAL COST OF SALES (CST-007).
      *>
      *> WORKED EXAMPLE C3 OF THE SPECIFICATION
      *>   RECEIVE 100 AT STANDARD 9.00, ORDERED AT 10.00
      *>     Dr INVENTORY 900.00  Dr PRICE VARIANCE 100.00  Cr GRNI
      *>   SUPPLIER INVOICES 1,020.00
      *>     Dr GRNI 1,000.00  Dr INCREMENTAL VARIANCE 20.00  Cr AP
      *>   TOTAL ADVERSE VARIANCE 120.00 - NOT 220.00
      *>
      *> WORKED EXAMPLE C7 - RETURN AFTER A STANDARD CHANGE
      *>   ORIGINAL COGS RELIEF 12.00, CURRENT STANDARD 10.00
      *>     Dr INVENTORY 10.00  Dr RETURN VARIANCE 2.00  Cr COGS 12.00
      *>
      *> CA-FUNCTION VALUES
      *>   GETSTD    EFFECTIVE STANDARD UNIT COST AND ELEMENT SPLIT
      *>   REVIEW    MOVE A DRAFT SET TO REVIEW AND SCORE IT AGAINST
      *>             THE PREVIOUS APPROVED SET (CST-012)
      *>   APPROVE   APPROVE A REVIEWED SET (NEVER SELF APPROVAL)
      *>   ACTIVATE  ACTIVATE AN APPROVED SET, REVALUE OWNED STOCK AND
      *>             RAISE THE REVALUATION VARIANCE
      *>   IMPACT    SHOW WHAT ACTIVATION WOULD DO - CHANGES NOTHING
      *>   RECEIPT   RECEIPT PRICE VARIANCE AT STANDARD
      *>   INVOICE   INCREMENTAL INVOICE PRICE VARIANCE
      *>   RETURN    RETURN VALUATION AT THE APPLICABLE STANDARD
      *>   COMPARE   LINE BY LINE COMPARISON OF TWO COST SETS
      *>
      *> REASON CODES
      *>   CST0401 COMPANY OR COST SET MISSING
      *>   CST0402 COST SET NOT FOUND
      *>   CST0403 WRONG STATUS FOR THIS ACTION
      *>   CST0404 A SIMULATION SET CANNOT BE ACTIVATED
      *>   CST0405 APPROVER MAY NOT BE THE PREPARER
      *>   CST0406 NO EFFECTIVE STANDARD COST FOR THE ITEM
      *>   CST0407 QUANTITY MUST BE POSITIVE
      *>   CST0408 COSTING LOT SIZE MUST BE POSITIVE
      *>   CST0409 UNSUPPORTED FUNCTION
      *>   CST0410 COMPARISON SET NOT FOUND
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-06-11 RJH  CR0524 ORIGINAL STANDARD COST SETS
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT EFFECTIVE DATES
      *> 2002-04-08 DJM  CR0998 SEPARATE REVIEW AND APPROVE STAGES
      *> 2026-10-06 STERP CST-007 CST-012 ACTIVATION REVALUATION
      *>================================================================
      *> REQ: CST-007, CST-009, CST-012   AC: AC-C3, AC-C7
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SC-BODY.
               10  SC-COST-SET         PIC X(20).
               10  SC-COMPARE-SET      PIC X(20).
               10  SC-ITEM-CODE        PIC X(40).
               10  SC-REVISION         PIC X(4).
               10  SC-EFF-DATE         PIC 9(8).
               10  SC-QUANTITY         PIC S9(13)V9(6).
               10  SC-LOT-SIZE         PIC S9(13)V9(6).
               10  SC-DOC-TYPE         PIC X(20).
               10  SC-DOC-REF          PIC X(40).
               10  SC-COMMIT           PIC X.
                   88  SC-DO-COMMIT              VALUE "Y".
      *>       ---- prices in play ----
               10  SC-STD-UNIT-COST    PIC S9(11)V9(6).
               10  SC-PREV-UNIT-COST   PIC S9(11)V9(6).
               10  SC-ORDER-PRICE      PIC S9(11)V9(6).
               10  SC-INVOICE-AMOUNT   PIC S9(13)V9(2).
               10  SC-ORIGINAL-COGS    PIC S9(13)V9(2).
      *>       ---- results ----
               10  SC-INVENTORY-GBP    PIC S9(13)V9(2).
               10  SC-GRNI-GBP         PIC S9(13)V9(2).
               10  SC-PPV-GBP          PIC S9(13)V9(2).
               10  SC-INCREMENTAL-GBP  PIC S9(13)V9(2).
               10  SC-TOTAL-VAR-GBP    PIC S9(13)V9(2).
               10  SC-RETURN-VALUE     PIC S9(13)V9(2).
               10  SC-RETURN-VAR-GBP   PIC S9(13)V9(2).
               10  SC-REVAL-QTY        PIC S9(13)V9(6).
               10  SC-REVAL-GBP        PIC S9(13)V9(2).
               10  SC-WIP-REVAL-GBP    PIC S9(13)V9(2).
               10  SC-ITEM-COUNT       PIC S9(9).
               10  SC-VARIANCE-PCT     PIC S9(5)V9(4).
               10  SC-ADVERSE          PIC X.
                   88  SC-IS-ADVERSE             VALUE "Y".
               10  SC-STATUS           PIC X(10).
               10  SC-PURPOSE          PIC X(12).
               10  SC-PREPARED-BY      PIC X(20).
               10  SC-APPROVED-BY      PIC X(20).
               10  SC-COMPLETENESS     PIC X(12).
               10  SC-WARNING          PIC X(80).
      *>       ---- element split of the standard (CST-002) ----
               10  SC-EL-MATERIAL      PIC S9(13)V9(2).
               10  SC-EL-LABOUR        PIC S9(13)V9(2).
               10  SC-EL-MACHINE       PIC S9(13)V9(2).
               10  SC-EL-SUBCON        PIC S9(13)V9(2).
               10  SC-EL-OVERHEAD      PIC S9(13)V9(2).
               10  SC-EL-RECOVERY      PIC S9(13)V9(2).
      *>       ---- comparison window (CST-012) ----
               10  SC-FROM-LINE        PIC S9(9).
               10  SC-LINE-CNT         PIC S9(4).
               10  SC-MORE-LINES       PIC X.
               10  SC-LINES OCCURS 20 TIMES.
                   15  SC-L-ITEM       PIC X(40).
                   15  SC-L-REV        PIC X(4).
                   15  SC-L-OLD-COST   PIC S9(11)V9(6).
                   15  SC-L-NEW-COST   PIC S9(11)V9(6).
                   15  SC-L-DIFF       PIC S9(11)V9(6).
                   15  SC-L-PCT        PIC S9(5)V9(4).
                   15  SC-L-QTY-HELD   PIC S9(13)V9(6).
                   15  SC-L-REVAL      PIC S9(13)V9(2).
                   15  SC-L-NOTE       PIC X(40).
