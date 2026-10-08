      *>================================================================
      *> ITMIMPCTCA - COMMAREA for ITMIMPCT, lifecycle change impact.
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE
      *>
      *> Before an item is blocked, made obsolete, superseded, retired
      *> or has a protected financial attribute changed, the user must
      *> see what still depends on it: stock on hand and in transit,
      *> reservations, outstanding sales and purchase orders, active
      *> bills of material, released kits and unresolved quality or
      *> traceability obligations (ITM-010, ITM-020, ITM-022).
      *>
      *> CA-FUNCTION values
      *>   IMPACT    the dependency picture for IM-ITEM-CODE
      *>   BLOCK     block the item for IM-SCOPE after the review
      *>   OBSOLETE  mark obsolete / run out with a disposition
      *>   SUCCEED   record a successor (and the predecessor on it)
      *>   RETIRE    retire the item - never a physical delete
      *>   DELCHECK  may this item or revision be deleted?  Only an
      *>             unused draft qualifies.
      *>   PROTECT   may IM-FIELD-NAME be changed retrospectively?
      *>             Valuation group, cost method, stock unit, class
      *>             and traceability are protected once stock or
      *>             history exists and need an approved treatment.
      *>
      *> IM-SCOPE : ALL PURCHASE SALES ISSUE MAKE
      *> IM-BLOCKING is "Y" when at least one dependency prevents the
      *> action outright; IM-DISPOSITION carries the approved run out,
      *> substitution or scrapping plan.
      *>
      *> REASON CODES
      *>   IMP0001 item not known in this company
      *>   IMP0002 dependencies block this action
      *>   IMP0003 a disposition is required before obsoleting
      *>   IMP0004 referenced item or revision cannot be deleted
      *>   IMP0005 attribute is protected while stock or history exists
      *>   IMP0006 successor is not a usable item
      *>   IMP0007 state change is not permitted from here
      *>   IMP0008 unsupported function
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-11-12 RJH  CR0488 OBSOLESCENCE CONTROL
      *> 2002-02-28 DJM  CR1051 WHERE USED BEFORE BLOCKING
      *> 2026-10-05 STERP ITM-020, ITM-022 IMPACT REVIEW
      *>================================================================
      *> REQ: ITM-010, ITM-020, ITM-022
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  IM-BODY.
               10  IM-ITEM-CODE        PIC X(40).
               10  IM-REVISION         PIC X(4).
               10  IM-SCOPE            PIC X(10).
               10  IM-NEW-STATUS       PIC X(10).
               10  IM-SUCCESSOR        PIC X(40).
               10  IM-DISPOSITION      PIC X(200).
               10  IM-REASON           PIC X(200).
               10  IM-FIELD-NAME       PIC X(24).
               10  IM-NEW-VALUE        PIC X(100).
               10  IM-OLD-VALUE        PIC X(100).
               10  IM-APPROVAL-REF     PIC X(40).
               10  IM-QTY-ON-HAND      PIC S9(13)V9(6).
               10  IM-QTY-INTRANSIT    PIC S9(13)V9(6).
               10  IM-QTY-RESERVED     PIC S9(13)V9(6).
               10  IM-MOVEMENT-CNT     PIC S9(9) COMP-5.
               10  IM-OPEN-SO-CNT      PIC S9(9) COMP-5.
               10  IM-OPEN-PO-CNT      PIC S9(9) COMP-5.
               10  IM-BOM-CNT          PIC S9(9) COMP-5.
               10  IM-KIT-CNT          PIC S9(9) COMP-5.
               10  IM-SUBST-CNT        PIC S9(9) COMP-5.
               10  IM-HOLD-CNT         PIC S9(9) COMP-5.
               10  IM-BLOCKING         PIC X.
                   88  IM-IS-BLOCKING            VALUE "Y".
               10  IM-ALLOWED          PIC X.
                   88  IM-IS-ALLOWED             VALUE "Y".
               10  IM-PROTECTED        PIC X.
               10  IM-TREATMENT        PIC X(200).
               10  IM-LINE-CNT         PIC 9(3).
               10  IM-LINES OCCURS 24 TIMES.
                   15  IM-L-AREA       PIC X(16).
                   15  IM-L-SEVERITY   PIC X(8).
                   15  IM-L-COUNT      PIC S9(9) COMP-5.
                   15  IM-L-QTY        PIC S9(13)V9(6).
                   15  IM-L-REF        PIC X(40).
                   15  IM-L-TEXT       PIC X(80).
