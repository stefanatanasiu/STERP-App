      *>================================================================
      *> KITCOSTCA - COMMAREA for KITCOST, kit cost enquiry.
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : KIT MAINTENANCE AND EXECUTION
      *>
      *> KIT-020 wants seven numbers that reconcile, using the costing
      *> rules of specification 07:
      *>   planned     the approved requirement at plan cost
      *>   issued      what actually went onto the job, at issue value
      *>   returned    the attributable value of confirmed returns
      *>   scrapped    the value of material scrapped on the job
      *>   absorbed    conversion and overhead absorbed on the run
      *>   completed   value transferred out to finished stock
      *>   residual    what is still in work in progress
      *>
      *>   residual = issued - returned + absorbed - completed
      *>
      *> Scrapped value is part of issued, not a deduction from it:
      *> scrap is a disposition, not a reversal (KIT-014).
      *>
      *> A made subassembly is charged to the parent kit at the value
      *> of the subassembly ONLY.  Its own child materials were
      *> charged to the child kit and were relieved when the child was
      *> completed, so charging them again would double count
      *> (KIT-020, AC-081).  KK-DOUBLE-CHECK reports that test.
      *>
      *> CA-FUNCTION values  COST, GETELEM (element breakdown),
      *>   VARIANCE (planned versus actual by element)
      *>
      *> REASON CODES  KIT0001, KIT0002, KIT0027 (no cost set).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2015-07-13 PMK  CR2366 KIT COST ENQUIRY
      *> 2016-10-04 PMK  CR2499 SUBASSEMBLY CHARGED ONCE
      *> 2026-10-06 STERP KIT-020
      *>================================================================
      *> REQ: KIT-020
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  KK-BODY.
               10  KK-KIT-REF          PIC X(30).
               10  KK-COST-SET         PIC X(20).
               10  KK-AS-AT            PIC 9(8).
               10  KK-CURRENCY         PIC X(3).
      *>       ---- the seven values ----
               10  KK-PLANNED-GBP      PIC S9(11)V99.
               10  KK-ISSUED-GBP       PIC S9(11)V99.
               10  KK-RETURNED-GBP     PIC S9(11)V99.
               10  KK-SCRAPPED-GBP     PIC S9(11)V99.
               10  KK-ABSORBED-GBP     PIC S9(11)V99.
               10  KK-COMPLETED-GBP    PIC S9(11)V99.
               10  KK-RESIDUAL-GBP     PIC S9(11)V99.
               10  KK-VARIANCE-GBP     PIC S9(11)V99.
      *>       ---- context ----
               10  KK-PLANNED-QTY      PIC S9(13)V9(6).
               10  KK-COMPLETED-QTY    PIC S9(13)V9(6).
               10  KK-UNIT-COST        PIC S9(11)V9(6).
               10  KK-PLAN-UNIT-COST   PIC S9(11)V9(6).
               10  KK-SUBASSY-CNT      PIC S9(4).
               10  KK-SUBASSY-GBP      PIC S9(11)V99.
               10  KK-DOUBLE-CHECK     PIC X.
                   88  KK-NO-DOUBLE-COUNT        VALUE "Y".
               10  KK-RECONCILED       PIC X.
               10  KK-KIT-STATUS       PIC X(14).
               10  KK-NOTE-TEXT        PIC X(200).
      *>       ---- element breakdown ----
               10  KK-ELEM-CNT         PIC S9(4).
               10  KK-ELEMS OCCURS 12 TIMES.
                   15  KK-E-ELEMENT    PIC X(10).
                   15  KK-E-DESC       PIC X(30).
                   15  KK-E-PLANNED    PIC S9(11)V99.
                   15  KK-E-ACTUAL     PIC S9(11)V99.
                   15  KK-E-VARIANCE   PIC S9(11)V99.
                   15  KK-E-QTY        PIC S9(13)V9(6).
                   15  KK-E-UOM        PIC X(10).
