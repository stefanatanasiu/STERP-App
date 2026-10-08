      *>================================================================
      *> BOMEXPLCA - COMMAREA of BOMEXPL, the recursive nested
      *>             explosion engine, and of the programs that shape
      *>             its result (BOMLEAF, BOMNET, BOMCOST).
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : BILLS OF MATERIAL
      *> FUNCTIONS :
      *>   EXPLODE  gross structural explosion of BX-REQ-QTY good
      *>            parent output at BX-EFF-DATE.  Fills the BOMWORK
      *>            occurrence pool and the summary fields here.
      *>   NODE     internal recursive entry - BOMEXPL calls itself
      *>            with this function; external callers must not.
      *>   GETLINE  read BX-WIN-CNT occurrences from BX-FROM-LINE of
      *>            the retained result (BX-VIEW = ALL / LEAF /
      *>            INTERMED / LEVEL1 / INDENT).
      *>   LEAFQTY  BX-KEY-TOTAL = total requirement of BX-KEY-ITEM.
      *>   STORE    write the retained result to bom.BomExplosion and
      *>            bom.BomExplosionLine under BX-CALC-REF (BOM-032).
      *> A result stays in the pool until the next EXPLODE, so the
      *> client can page a large tree without re-exploding it.  That is
      *> also why a read-only repeat has no live effect (BOM-030).
      *> Reason codes BOM0501..BOM0535.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2014-07-21 AJS  CR2228 ENGINEERING REWRITE - IS RECURSIVE
      *> 2015-03-11 AJS  CR2301 PHANTOM GROUPING WITHOUT STOCK
      *> 2016-11-08 AJS  CR2502 APPROVED RUN CONSOLIDATION (D-10)
      *> 2026-10-05 STERP STOP REASONS AND COMPLETENESS STATE
      *>================================================================
      *> REQ: BOM-012, BOM-014, BOM-016, BOM-017, BOM-019, BOM-029,
      *> REQ: BOM-030, BOM-032
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  BX-BODY.
               10  BX-CALC-TYPE        PIC X(10).
               10  BX-CALC-REF         PIC X(30).
               10  BX-PARENT-ITEM      PIC X(40).
               10  BX-PARENT-REV       PIC X(4).
               10  BX-PURPOSE          PIC X(10).
               10  BX-JOB-REF          PIC X(20).
               10  BX-VARIANT          PIC X(20).
               10  BX-REQ-QTY          PIC S9(15)V9(6).
               10  BX-EFF-DATE         PIC 9(8).
               10  BX-MAX-DEPTH        PIC S9(4).
               10  BX-CONSOLIDATE      PIC X.
               10  BX-EXPAND-PHANTOM   PIC X.
               10  BX-INCLUDE-NOTES    PIC X.
               10  BX-INCLUDE-BYPROD   PIC X.
               10  BX-COST-SET         PIC X(20).
               10  BX-CURRENCY         PIC X(3).
               10  BX-VIEW             PIC X(8).
               10  BX-SPLIT-CNT        PIC S9(4).
               10  BX-SPLITS OCCURS 10 TIMES.
                   15  BX-SPLIT-QTY    PIC S9(15)V9(6).
               10  BX-OPT-CNT          PIC S9(4).
               10  BX-OPTIONS OCCURS 10 TIMES.
                   15  BX-O-GROUP      PIC X(10).
                   15  BX-O-CHOICE     PIC X(40).
      *>       ---- summary (specification section 12) ----
               10  BX-BOM-ID           PIC S9(9) COMP-5.
               10  BX-BOM-REF          PIC X(30).
               10  BX-BOM-REV          PIC X(4).
               10  BX-NOMINAL-BASIS    PIC S9(13)V9(6).
               10  BX-YIELD            PIC S9(5)V9(6).
               10  BX-QUANTUM          PIC S9(13)V9(6).
               10  BX-BATCH-SIZE       PIC S9(13)V9(6).
               10  BX-GROSS-STARTS     PIC S9(15)V9(6).
               10  BX-RUN-COUNT        PIC S9(9) COMP-5.
               10  BX-BATCH-COUNT      PIC S9(9) COMP-5.
               10  BX-EXCESS           PIC S9(15)V9(6).
               10  BX-OCC-COUNT        PIC S9(9) COMP-5.
               10  BX-LEAF-COUNT       PIC S9(9) COMP-5.
               10  BX-DEPTH-REACHED    PIC S9(4).
               10  BX-SHORTAGE-CNT     PIC S9(4).
               10  BX-MAX-BUILDABLE    PIC S9(15)V9(6).
               10  BX-MATERIAL-COST    PIC S9(13)V9(2).
               10  BX-CONVERSION-COST  PIC S9(13)V9(2).
               10  BX-RECOVERY-CREDIT  PIC S9(13)V9(2).
               10  BX-TOTAL-COST       PIC S9(13)V9(2).
               10  BX-UNIT-COST        PIC S9(11)V9(6).
               10  BX-UNIT-DENOM       PIC S9(15)V9(6).
               10  BX-COMPLETENESS     PIC X(12).
                   88  BX-COMPLETE               VALUE "COMPLETE".
                   88  BX-INCOMPLETE             VALUE "INCOMPLETE".
                   88  BX-IN-ERROR               VALUE "ERROR".
               10  BX-ERROR-PATH       PIC X(200).
               10  BX-CYCLE-PATH       PIC X(200).
               10  BX-WARNING-CNT      PIC S9(4).
               10  BX-EXPLOSION-ID     PIC S9(9) COMP-5.
               10  BX-KEY-ITEM         PIC X(40).
               10  BX-KEY-TOTAL        PIC S9(15)V9(6).
      *>       ---- recursion state (function NODE only) ----
               10  BX-NODE-SEQ         PIC S9(9) COMP-5.
               10  BX-NODE-DEPTH       PIC S9(4).
               10  BX-NODE-PATH        PIC X(160).
               10  BX-NODE-PHANTOM     PIC X.
               10  BX-NODE-RUNS        PIC S9(9) COMP-5.
               10  BX-NODE-BATCHES     PIC S9(9) COMP-5.
               10  BX-STOCK-SOURCE     PIC X(8).
               10  BX-USE-INBOUND      PIC X.
      *>       ---- paging window ----
               10  BX-FROM-LINE        PIC S9(9) COMP-5.
               10  BX-WIN-MAX          PIC S9(4).
               10  BX-WIN-CNT          PIC S9(4).
               10  BX-MORE-LINES       PIC X.
               10  BX-LINES OCCURS 25 TIMES.
                   15  BX-L-SEQ        PIC S9(9) COMP-5.
                   15  BX-L-DEPTH      PIC S9(4).
                   15  BX-L-PATH       PIC X(80).
                   15  BX-L-ITEM       PIC X(40).
                   15  BX-L-REV        PIC X(4).
                   15  BX-L-DESC       PIC X(40).
                   15  BX-L-NODE-TYPE  PIC X(8).
                   15  BX-L-SUPPLY     PIC X(10).
                   15  BX-L-STOP       PIC X(20).
                   15  BX-L-BASIS      PIC X(12).
                   15  BX-L-UOM        PIC X(10).
                   15  BX-L-BOM-REV    PIC X(4).
                   15  BX-L-USAGE      PIC S9(13)V9(6).
                   15  BX-L-UNROUNDED  PIC S9(15)V9(9).
                   15  BX-L-ROUND-INC  PIC S9(13)V9(6).
                   15  BX-L-REQ-QTY    PIC S9(15)V9(6).
                   15  BX-L-GOOD-QTY   PIC S9(15)V9(6).
                   15  BX-L-GROSS      PIC S9(15)V9(6).
                   15  BX-L-RUNS       PIC S9(9) COMP-5.
                   15  BX-L-BATCHES    PIC S9(9) COMP-5.
                   15  BX-L-STOCK-USED PIC S9(15)V9(6).
                   15  BX-L-SHORTAGE   PIC S9(15)V9(6).
                   15  BX-L-PACK-QTY   PIC S9(15)V9(6).
                   15  BX-L-EXCESS     PIC S9(15)V9(6).
                   15  BX-L-UNIT-COST  PIC S9(11)V9(6).
                   15  BX-L-EXT-COST   PIC S9(13)V9(2).
                   15  BX-L-BRANCH     PIC S9(13)V9(2).
                   15  BX-L-ELEMENT    PIC X(10).
                   15  BX-L-WARNING    PIC X(60).
