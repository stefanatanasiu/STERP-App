      *>================================================================
      *> LANDCOSTCA - COMMAREA for LANDCOST, landed cost estimation,
      *>              allocation and actual true up.
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> Eligible categories are freight and insurance to the receiving
      *> site, customs duty, customs agent clearance fees and handling
      *> necessary to bring goods to their present location and
      *> condition (FRS 102 Section 13).  Recoverable import VAT and
      *> recoverable VAT on charges are NEVER landed cost.  Demurrage,
      *> unnecessary storage, abnormal waste and administration are
      *> expensed (PO-015).
      *>
      *> Estimated and actual charges keep separate identities.  When
      *> the actual invoice arrives only the DIFFERENCE is allocated,
      *> so the total attributable charge is capitalised exactly once
      *> (PO-016).
      *>
      *> Supported bases: VALUE, WEIGHT, VOLUME, QUANTITY (only across
      *> comparable units) and EXPLICIT reviewed amounts.  A missing or
      *> zero aggregate basis BLOCKS allocation.
      *>
      *> CA-FUNCTION values
      *>   CALC      allocate the charge over the supplied recipients
      *>             with no database effect (screen preview, tests)
      *>   ESTIMATE  record the estimated charge in cst.LandedCost
      *>   ALLOCATE  allocate and write cst.LandedCostAllocation,
      *>             posting PO.LANDEDEST
      *>   ACTUAL    reconcile the actual charge to the estimate and
      *>             allocate only the delta, posting PO.LANDEDACT
      *>   ENQUIRE   estimate versus actual for a charge reference
      *>
      *> Worked example (AC-056): two received groups of 600.00 and
      *> 400.00 and a freight charge of 75.00 allocated by value give
      *> 45.00 and 30.00 - not 75.00 on each group.
      *>
      *> REASON CODES
      *>   LND0001 charge reference missing
      *>   LND0002 no eligible recipients supplied
      *>   LND0003 the aggregate allocation basis is zero or missing
      *>   LND0004 category is not capitalisable - expense it
      *>   LND0005 the charge is already allocated
      *>   LND0006 unsupported function or basis
      *>   LND0007 recoverable VAT cannot be landed cost
      *>   LND0008 allocation does not equal the approved charge
      *>----------------------------------------------------------------
      *> 1998-09-02 RJH  CR0833 SHIPMENT COSTING
      *> 2005-11-14 DJM  CR1310 ESTIMATE AND ACTUAL KEPT APART
      *> 2021-01-01 TMW  BREXIT - DUTY IS LANDED, IMPORT VAT IS NOT
      *> 2026-10-06 STERP PO-015, PO-016 AND FRS 102 SECTION 13
      *>================================================================
      *> REQ: PO-015, PO-016, PO-025   AC: AC-056
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  LC-BODY.
               10  LC-CHARGE-REF       PIC X(30).
               10  LC-SHIPMENT-REF     PIC X(30).
               10  LC-SHIPMENT-DATE    PIC 9(8).
               10  LC-SUPPLIER         PIC X(20).
               10  LC-IMPORT-ENTRY     PIC X(30).
               10  LC-CATEGORY         PIC X(14).
                   88  LC-FREIGHT                VALUE "FREIGHT".
                   88  LC-INSURANCE              VALUE "INSURANCE".
                   88  LC-DUTY                   VALUE "DUTY".
                   88  LC-CLEARANCE              VALUE "CLEARANCE".
                   88  LC-HANDLING               VALUE "HANDLING".
                   88  LC-OTHER                  VALUE "OTHER".
               10  LC-BASIS            PIC X(10).
                   88  LC-BY-VALUE               VALUE "VALUE".
                   88  LC-BY-WEIGHT              VALUE "WEIGHT".
                   88  LC-BY-VOLUME              VALUE "VOLUME".
                   88  LC-BY-QUANTITY            VALUE "QUANTITY".
                   88  LC-BY-EXPLICIT            VALUE "EXPLICIT".
               10  LC-CURRENCY         PIC X(3).
               10  LC-FX-RATE          PIC S9(9)V9(8).
               10  LC-ESTIMATE-AMT     PIC S9(13)V99.
               10  LC-ESTIMATE-GBP     PIC S9(13)V99.
               10  LC-ACTUAL-AMT       PIC S9(13)V99.
               10  LC-ACTUAL-GBP       PIC S9(13)V99.
               10  LC-RECOVERABLE-VAT  PIC S9(13)V99.
               10  LC-IS-CAPITALISE    PIC X.
                   88  LC-CAPITALISE             VALUE "Y".
               10  LC-ALLOC-DATE       PIC 9(8).
               10  LC-SUPPLIER-INV     PIC X(40).
               10  LC-APPROVED-BY      PIC X(20).
               10  LC-POST-JOURNAL     PIC X.
      *>     ---- answers ----
               10  LC-LANDED-ID        PIC S9(9) COMP-5.
               10  LC-BASIS-TOTAL      PIC S9(15)V9(6).
               10  LC-ALLOCATED-GBP    PIC S9(13)V99.
               10  LC-DELTA-GBP        PIC S9(13)V99.
               10  LC-RESIDUAL-GBP     PIC S9(13)V99.
               10  LC-TO-INVENTORY     PIC S9(13)V99.
               10  LC-TO-COGS          PIC S9(13)V99.
               10  LC-TO-VARIANCE      PIC S9(13)V99.
               10  LC-JOURNAL-NUMBER   PIC X(30).
               10  LC-STATUS           PIC X(12).
               10  LC-MISSING-BASIS    PIC 9(3).
               10  LC-INELIGIBLE-CNT   PIC 9(3).
      *>     ---- recipients: receipt lines sharing the charge ----
               10  LC-LINE-CNT         PIC 9(3).
               10  LC-LINE-TABLE.
                   15  LC-LINE OCCURS 40 TIMES.
                       20  LC-L-RECEIPT    PIC X(30).
                       20  LC-L-RCP-LINE   PIC S9(9) COMP-5.
                       20  LC-L-ITEM       PIC X(40).
                       20  LC-L-LOT        PIC X(30).
                       20  LC-L-BASIS-VAL  PIC S9(13)V9(6).
                       20  LC-L-QTY-REMAIN PIC S9(13)V9(6).
                       20  LC-L-QTY-USED   PIC S9(13)V9(6).
                       20  LC-L-ELIGIBLE   PIC X.
                       20  LC-L-SHARE-GBP  PIC S9(13)V99.
                       20  LC-L-PRIOR-GBP  PIC S9(13)V99.
                       20  LC-L-DELTA-GBP  PIC S9(13)V99.
                       20  LC-L-TO-INV     PIC S9(13)V99.
                       20  LC-L-TO-COGS    PIC S9(13)V99.
