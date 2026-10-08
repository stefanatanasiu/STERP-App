      *>================================================================
      *> KITCOMMCA - COMMAREA for KITCOMM, commercial kit fulfilment.
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : KIT MAINTENANCE AND EXECUTION
      *>
      *> A commercial sales kit is a named group of components the
      *> customer buys.  The parent is a COMMERCIAL IDENTITY, not an
      *> item balance: it is never received into stock, never
      *> completed and never disassembled (KIT-001, KIT-023).
      *>
      *> Decision D-11 fixes the defaults this program enforces:
      *>   - the parent quantity counts COMPLETE component sets
      *>   - the parent commercial price is captured at acceptance
      *>   - components are reserved, despatched and costed ONCE
      *>   - revenue is allocated to components by relative
      *>     standalone selling price, and must reconcile to the
      *>     parent net amount (the rounding residual is given to the
      *>     largest component so the set still adds up)
      *>   - partial despatch is NOT permitted: an incomplete set is
      *>     not a kit
      *>   - substitution needs an approved equal or higher
      *>     specification equivalent, and a customer specified
      *>     component needs the customer's written agreement
      *>   - a partial return is credited at the allocated component
      *>     value and creates NO parent stock
      *>
      *> CA-FUNCTION values
      *>   SETS      how many complete sets the component stock
      *>             supports, and which component limits it
      *>   ALLOCATE  allocate KV-PARENT-NET across the components by
      *>             relative standalone selling price
      *>   FULFIL    reconcile parent sold quantity to component
      *>             delivery and cost without duplication
      *>   RETURNCK  check a partial component return and value the
      *>             credit at the allocated component value
      *>
      *> REASON CODES  KIT0001, KIT0002, KIT0003, KIT0006, KIT0014,
      *>   KIT0020, KIT0028 (standalone prices missing or all zero).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2015-10-26 PMK  CR2399 COMMERCIAL KIT ALLOCATION (D-11)
      *> 2016-12-13 PMK  CR2514 RESIDUAL PENNY TO THE LARGEST LINE
      *> 2026-10-06 STERP KIT-001, KIT-023
      *>================================================================
      *> REQ: KIT-001, KIT-023
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  KV-BODY.
               10  KV-KIT-ITEM         PIC X(40).
               10  KV-KIT-VERSION      PIC X(4).
               10  KV-KIT-DEF-ID       PIC S9(9) COMP-5.
               10  KV-SITE             PIC X(10).
               10  KV-ORDER-REF        PIC X(40).
               10  KV-ORDER-LINE       PIC S9(9) COMP-5.
               10  KV-PARENT-QTY       PIC S9(13)V9(6).
               10  KV-PARENT-NET       PIC S9(11)V99.
               10  KV-CURRENCY         PIC X(3).
               10  KV-SUPPLY-KIND      PIC X(10).
               10  KV-DELIVERED-SETS   PIC S9(13)V9(6).
               10  KV-RETURN-LINE      PIC S9(4).
               10  KV-RETURN-QTY       PIC S9(13)V9(6).
               10  KV-AS-AT            PIC 9(8).
      *>       ---- result ----
               10  KV-COMPLETE-SETS    PIC S9(13)V9(6).
               10  KV-LIMIT-ITEM       PIC X(40).
               10  KV-LIMIT-SHORT      PIC S9(13)V9(6).
               10  KV-ALLOC-TOTAL      PIC S9(11)V99.
               10  KV-RESIDUAL         PIC S9(7)V99.
               10  KV-COST-TOTAL       PIC S9(11)V99.
               10  KV-MARGIN-GBP       PIC S9(11)V99.
               10  KV-MARGIN-PCT       PIC S9(5)V9(4).
               10  KV-CREDIT-GBP       PIC S9(11)V99.
               10  KV-RECONCILED       PIC X.
                   88  KV-IS-RECONCILED          VALUE "Y".
               10  KV-MAY-DESPATCH     PIC X.
               10  KV-PARENT-STOCK     PIC X.
               10  KV-LINE-CNT         PIC S9(4).
               10  KV-NOTE-TEXT        PIC X(200).
               10  KV-LINES OCCURS 15 TIMES.
                   15  KV-L-LINE-NUM   PIC S9(4).
                   15  KV-L-ITEM       PIC X(40).
                   15  KV-L-DESC       PIC X(40).
                   15  KV-L-QTY-PER    PIC S9(13)V9(6).
                   15  KV-L-UOM        PIC X(10).
                   15  KV-L-REQUIRED   PIC S9(13)V9(6).
                   15  KV-L-FREE       PIC S9(13)V9(6).
                   15  KV-L-DELIVERED  PIC S9(13)V9(6).
                   15  KV-L-SETS       PIC S9(13)V9(6).
                   15  KV-L-SSP        PIC S9(11)V9(6).
                   15  KV-L-ALLOC-PCT  PIC S9(5)V9(6).
                   15  KV-L-ALLOC-NET  PIC S9(11)V99.
                   15  KV-L-UNIT-COST  PIC S9(11)V9(6).
                   15  KV-L-COST-GBP   PIC S9(11)V99.
                   15  KV-L-VAT-CODE   PIC X(4).
                   15  KV-L-CUST-SPEC  PIC X.
                   15  KV-L-SUB-OK     PIC X.
                   15  KV-L-LIMITING   PIC X.
