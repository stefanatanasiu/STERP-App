      *>================================================================
      *> ITEMVALCA - COMMAREA for ITEMVAL, the item identity, class and
      *>             activation service.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *>
      *> CA-FUNCTION values
      *>   FETCH     read one item by full code (no truncation and no
      *>             case folding of the stored code - D-21)
      *>   SEARCH    full code / alias search.  IV-SEARCH-TEXT is
      *>             matched against the item code, the description and
      *>             the partner aliases.  Ambiguous aliases are
      *>             reported, never silently resolved (ITM-001).
      *>   RESOLVE   resolve exactly one item from IV-SEARCH-TEXT in
      *>             the context of IV-PARTNER-TYPE / IV-PARTNER-CODE
      *>   CLASSCHK  may this class do IV-ACTION? (ITM-002)
      *>             IV-ACTION: STOCK SELL BUY MAKE BOM COST KIT
      *>   VALIDATE  activation review: list everything missing before
      *>             the item may be released for its enabled
      *>             processes (ITM-003, ITM-010, ITM-025 to ITM-027)
      *>   ACTIVATE  run VALIDATE and, when clean and the reviewer is
      *>             not the author, move DRAFT/REVIEW -> RELEASED
      *>   REVISION  read one revision and its effectivity (ITM-001)
      *>
      *> IV-FAULTS lists one line per missing or unsafe attribute, each
      *> with the requirement that demands it, so the screen can show a
      *> reviewer exactly what to fix.
      *>
      *> REASON CODES
      *>   ITM0001 item not known in this company
      *>   ITM0002 search text matches more than one item (ambiguous)
      *>   ITM0003 class does not permit that action
      *>   ITM0004 activation review found missing information
      *>   ITM0005 item is not in a state that may be released
      *>   ITM0006 a reviewer may not release their own draft
      *>   ITM0007 revision not known
      *>   ITM0008 unsupported function
      *>   ITM0009 search text was not supplied
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-02-11 RJH  CR0412 ITEM MASTER VALIDATION
      *> 1998-09-14 RJH  CR0905 SUPPLIER ALIAS SEARCH
      *> 2021-01-01 ANW  CR4410 BREXIT - COMMODITY CODE AND ORIGIN
      *> 2026-10-05 STERP ITM-003 ACTIVATION REVIEW
      *>================================================================
      *> REQ: ITM-001, ITM-002, ITM-003, ITM-004
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  IV-BODY.
               10  IV-ITEM-CODE        PIC X(40).
               10  IV-SEARCH-TEXT      PIC X(40).
               10  IV-PARTNER-TYPE     PIC X(10).
               10  IV-PARTNER-CODE     PIC X(20).
               10  IV-ACTION           PIC X(8).
               10  IV-REVISION         PIC X(4).
               10  IV-DESCRIPTION      PIC X(100).
               10  IV-ITEM-CLASS       PIC X(12).
               10  IV-CATEGORY         PIC X(20).
               10  IV-STEWARD          PIC X(20).
               10  IV-LIFECYCLE        PIC X(10).
               10  IV-STOCK-UOM        PIC X(10).
               10  IV-TRACEABILITY     PIC X(8).
               10  IV-SHELF-LIFE-DAYS  PIC S9(5).
               10  IV-QUANTUM          PIC S9(13)V9(6).
               10  IV-INDIVISIBLE      PIC X.
               10  IV-MAKE-BUY         PIC X(6).
               10  IV-VALUATION-GRP    PIC X(20).
               10  IV-COST-METHOD      PIC X(4).
               10  IV-CURRENT-REV      PIC X(4).
               10  IV-IS-STOCKED       PIC X.
               10  IV-IS-PURCHASABLE   PIC X.
               10  IV-IS-SALEABLE      PIC X.
               10  IV-IS-MANUFACTURED  PIC X.
               10  IV-IS-CONSUMABLE    PIC X.
               10  IV-REQ-INSPECTION   PIC X.
               10  IV-DRC-GOODS        PIC X.
               10  IV-SUCCESSOR        PIC X(40).
               10  IV-PREDECESSOR      PIC X(40).
               10  IV-REV-STATUS       PIC X(10).
               10  IV-REV-FROM         PIC 9(8).
               10  IV-REV-TO           PIC 9(8).
               10  IV-REV-INTERCHANGE  PIC X.
               10  IV-ALLOWED          PIC X.
                   88  IV-IS-ALLOWED             VALUE "Y".
               10  IV-READY            PIC X.
                   88  IV-IS-READY               VALUE "Y".
               10  IV-FAULT-CNT        PIC 9(3).
               10  IV-FAULTS OCCURS 24 TIMES.
                   15  IV-F-REQ        PIC X(8).
                   15  IV-F-SEVERITY   PIC X(8).
                   15  IV-F-FIELD      PIC X(24).
                   15  IV-F-TEXT       PIC X(90).
               10  IV-MATCH-CNT        PIC 9(3).
               10  IV-MATCHES OCCURS 15 TIMES.
                   15  IV-M-ITEM       PIC X(40).
                   15  IV-M-DESC       PIC X(60).
                   15  IV-M-SOURCE     PIC X(10).
                   15  IV-M-LIFECYCLE  PIC X(10).
                   15  IV-M-ALIAS      PIC X(40).
