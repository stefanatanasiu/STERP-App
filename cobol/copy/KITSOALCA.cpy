      *>================================================================
      *> KITSOALCA - COMMAREA for KITSOAL, commercial kit allocation
      *>             and completeness.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : SALES ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> SO-020, SO-028 and D-11.  A commercial kit is one offer made
      *> of several deliverables.  Three questions are answered here
      *> and nowhere else in the sales order module:
      *>
      *> ALLOCATE  (SO-028)  The parent transaction price is allocated
      *>           to the components by RELATIVE STANDALONE SELLING
      *>           PRICE.  The spread is done by RNDALLOC function
      *>           ALLOC mode WEIGHT with the component standalone
      *>           prices as the weights, so the parts re-add exactly
      *>           to the parent net amount - no residual penny is
      *>           lost or invented.  Where the components carry
      *>           different VAT liabilities the supply is MIXED and
      *>           VAT is calculated per rate on the allocated
      *>           amounts; otherwise it is SINGLE.  The decision is
      *>           RECORDED in KA-SUPPLY-KIND, never guessed.
      *>           Worked case: a GBP 100.00 kit whose two components
      *>           have standalone prices of GBP 60.00 each allocates
      *>           GBP 50.00 to each; if only one is standard rated
      *>           the VAT is GBP 10.00.
      *>
      *> COMPLETE  (D-11)  A complete commercial kit is required for
      *>           each despatched parent quantity.  Partial component
      *>           despatch is not permitted.  Given the deliverable
      *>           component quantities the service returns the number
      *>           of COMPLETE kits possible and refuses a proposed
      *>           despatch larger than that (reason KIT0003).
      *>
      *> CHECKDUP  (SO-020)  The parent and its children can never
      *>           both create the same inventory demand nor be billed
      *>           twice.  A non stock commercial kit parent has no
      *>           warehouse balance: component demand, issue, COGS
      *>           and traceability happen once at the components and
      *>           pricing and invoicing at the parent (or at
      *>           explicitly allocated component commercial lines) -
      *>           never both.
      *>
      *> The structure is read from kit.KitDefinition /
      *> kit.KitDefinitionLine when they exist.  When they do not, it
      *> is driven from so.SalesOrderLine rows whose LineType is KIT
      *> or KITCOMP linked by KitParentLineNum, and KA-STRUCT-SOURCE
      *> says which was used so the answer can be explained.
      *>
      *> A caller that already holds the structure - a trial, a what
      *> if, or a unit test - sets KA-STRUCT-SOURCE to CALLER on
      *> input and fills KA-LINES itself.  Nothing is then read from
      *> the database and nothing is written: these three functions
      *> have no side effect at all.
      *>
      *> REASON CODES
      *>   KIT0001 company, order and parent line are required
      *>   KIT0002 no kit structure for that parent
      *>   KIT0003 the proposed despatch exceeds the complete kits
      *>           that the deliverable components allow
      *>   KIT0004 the parent net amount is zero or negative
      *>   KIT0005 no component carries a standalone selling price
      *>   KIT0006 unsupported function
      *>   KIT0007 the parent and a component both carry the same
      *>           inventory demand or billing - one of them is wrong
      *>   KIT0008 a non stock kit parent has a warehouse balance
      *>   KIT0009 more components than the table can carry
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-09-14 DJM  CR0967 COMMERCIAL KITS (ACUCOBOL)
      *> 2007-03-22 KMP  CR1402 MIXED SUPPLY VAT ON KIT COMPONENTS
      *> 2016-05-30 PMK  CR2488 COMPLETE KIT RULE MADE EXPLICIT
      *> 2026-10-06 STERP SO-020, SO-028, D-11 RELATIVE SSP
      *>================================================================
      *> REQ: SO-020, SO-028
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  KA-BODY.
               10  KA-ORDER-NUMBER     PIC X(30).
               10  KA-PARENT-LINE      PIC S9(9) COMP-5.
               10  KA-PARENT-SOL-ID    PIC S9(17).
               10  KA-KIT-ITEM         PIC X(40).
               10  KA-KIT-VERSION      PIC X(4).
               10  KA-KIT-REF          PIC X(30).
               10  KA-SITE             PIC X(10).
               10  KA-STRUCT-SOURCE    PIC X(12).
                   88  KA-FROM-KITDEF            VALUE "KITDEF".
                   88  KA-FROM-ORDER             VALUE "ORDERLINE".
                   88  KA-FROM-CALLER            VALUE "CALLER".
                   88  KA-NO-STRUCTURE           VALUE "NONE".
               10  KA-STRUCT-NOTE      PIC X(200).
      *>       ALLOCATE - relative standalone selling price (SO-028)
               10  KA-PARENT-QTY       PIC S9(13)V9(6).
               10  KA-PARENT-NET       PIC S9(11)V99.
               10  KA-PARENT-VAT-CODE  PIC X(4).
               10  KA-SSP-TOTAL        PIC S9(13)V9(6).
               10  KA-ALLOCATED        PIC S9(11)V99.
               10  KA-RESIDUAL         PIC S9(11)V99.
               10  KA-BUMPED           PIC S9(9) COMP-5.
               10  KA-VAT-TOTAL        PIC S9(11)V99.
               10  KA-GROSS-TOTAL      PIC S9(11)V99.
               10  KA-SUPPLY-KIND      PIC X(8).
                   88  KA-SINGLE-SUPPLY          VALUE "SINGLE".
                   88  KA-MIXED-SUPPLY           VALUE "MIXED".
               10  KA-SUPPLY-RECORDED  PIC X.
               10  KA-SUPPLY-DECIDER   PIC X(20).
               10  KA-SUPPLY-TEXT      PIC X(200).
      *>       COMPLETE - D-11, no partial component despatch
               10  KA-PROPOSED-KITS    PIC S9(13)V9(6).
               10  KA-COMPLETE-KITS    PIC S9(13)V9(6).
               10  KA-PARTIAL-ALLOWED  PIC X.
               10  KA-LIMIT-ITEM       PIC X(40).
               10  KA-LIMIT-SHORT      PIC S9(13)V9(6).
               10  KA-REFUSED          PIC X.
                   88  KA-IS-REFUSED             VALUE "Y".
      *>       CHECKDUP - SO-020, one demand and one billing only
               10  KA-PARENT-IS-STOCK  PIC X.
               10  KA-PARENT-DEMAND    PIC S9(13)V9(6).
               10  KA-PARENT-BALANCE   PIC S9(13)V9(6).
               10  KA-PARENT-BILLED    PIC X.
               10  KA-COMP-BILLED      PIC X.
               10  KA-DUP-DEMAND       PIC X.
               10  KA-DUP-BILLING      PIC X.
               10  KA-DUP-COGS         PIC X.
               10  KA-DUP-CNT          PIC 9(3).
               10  KA-DUP-TEXT         PIC X(200).
      *>       VAT analysis, one row per rate on the allocated amounts
               10  KA-VAT-CNT          PIC 9(2).
               10  KA-VAT-RATES OCCURS 6 TIMES.
                   15  KA-V-CODE       PIC X(4).
                   15  KA-V-RATE       PIC S9(5)V9(4).
                   15  KA-V-NET        PIC S9(11)V99.
                   15  KA-V-VAT        PIC S9(11)V99.
               10  KA-LINE-CNT         PIC 9(3).
               10  KA-LINES OCCURS 40 TIMES.
                   15  KA-L-LINE-NUM   PIC S9(9) COMP-5.
                   15  KA-L-ORDER-LINE PIC S9(9) COMP-5.
                   15  KA-L-SOL-ID     PIC S9(17).
                   15  KA-L-ITEM       PIC X(40).
                   15  KA-L-REVISION   PIC X(4).
                   15  KA-L-DESC       PIC X(60).
                   15  KA-L-QTY-PER    PIC S9(13)V9(6).
                   15  KA-L-REQUIRED   PIC S9(13)V9(6).
                   15  KA-L-DELIVER    PIC S9(13)V9(6).
                   15  KA-L-KITS-POSS  PIC S9(13)V9(6).
                   15  KA-L-SSP        PIC S9(11)V9(6).
                   15  KA-L-ALLOC-NET  PIC S9(11)V99.
                   15  KA-L-ALLOC-PCT  PIC S9(5)V9(6).
                   15  KA-L-VAT-CODE   PIC X(4).
                   15  KA-L-VAT-RATE   PIC S9(5)V9(4).
                   15  KA-L-VAT-AMT    PIC S9(11)V99.
                   15  KA-L-UOM        PIC X(10).
                   15  KA-L-OPTIONAL   PIC X.
                   15  KA-L-STATUS     PIC X(12).
                   15  KA-L-TEXT       PIC X(100).
