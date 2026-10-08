      *>================================================================
      *> BOMOCCUR - one expanded component occurrence.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : BILLS OF MATERIAL
      *> PURPOSE  : The unit of currency of every BOM calculation.  One
      *>            row = one appearance of a component along one
      *>            parent/line path, carrying the whole of the
      *>            specification section 12 output contract: path and
      *>            depth, selected revisions, basis/yield/quantum,
      *>            run and batch counts, the unrounded requirement,
      *>            the yield and scrap contributions, the rounding
      *>            increment, the final stock unit requirement, the
      *>            netting evidence, the valuation and why expansion
      *>            stopped.  The same record is reused for an
      *>            aggregated leaf line, where BO-SEQ holds the number
      *>            of contributing occurrences.
      *>            Compiles in fixed and free format (cols 8-72, *>).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2014-03-17 AJS  CR2201 ENGINEERING REWRITE - ONE RECORD FOR
      *>                 EXPLOSION, NETTING, COSTING AND REPORTING
      *> 2015-07-02 AJS  CR2318 PACK AND EXCESS EVIDENCE (B5)
      *> 2026-10-05 STERP OUTPUT CONTRACT SECTION 12
      *>================================================================
      *> REQ: BOM-011, BOM-012, BOM-014, BOM-032
       01  BOM-OCC-ROW.
           05  BO-SEQ                  PIC S9(9) COMP-5.
           05  BO-PARENT-SEQ           PIC S9(9) COMP-5.
           05  BO-DEPTH                PIC S9(4).
           05  BO-LINE-NUM             PIC S9(4).
           05  BO-PATH                 PIC X(160).
           05  BO-OCC-REF              PIC X(40).
           05  BO-PARENT-ITEM          PIC X(40).
           05  BO-PARENT-REV           PIC X(4).
           05  BO-ITEM                 PIC X(40).
           05  BO-ITEM-REV             PIC X(4).
           05  BO-DESCRIPTION          PIC X(60).
           05  BO-BOM-ID               PIC S9(9) COMP-5.
           05  BO-BOM-REV              PIC X(4).
           05  BO-NODE-TYPE            PIC X(8).
               88  BO-IS-LEAF                    VALUE "LEAF".
               88  BO-IS-INTERMED                VALUE "INTERMED".
               88  BO-IS-PHANTOM                 VALUE "PHANTOM".
               88  BO-IS-BYPRODUCT               VALUE "BYPROD".
               88  BO-IS-NOTE                    VALUE "NOTE".
           05  BO-SUPPLY               PIC X(10).
           05  BO-STOP-REASON          PIC X(20).
           05  BO-USAGE-BASIS          PIC X(12).
           05  BO-UOM                  PIC X(10).
           05  BO-STOCK-UOM            PIC X(10).
           05  BO-ALT-GROUP            PIC X(10).
           05  BO-OPTIONAL             PIC X.
           05  BO-SELECTED             PIC X.
           05  BO-USAGE-QTY            PIC S9(13)V9(6).
           05  BO-CONV-FACT            PIC S9(9)V9(9).
           05  BO-SCRAP-PCT            PIC S9(5)V9(6).
           05  BO-UNROUNDED            PIC S9(15)V9(9).
           05  BO-YIELD-CONTRIB        PIC S9(15)V9(9).
           05  BO-SCRAP-CONTRIB        PIC S9(15)V9(9).
           05  BO-ROUND-INC            PIC S9(13)V9(6).
           05  BO-REQ-QTY              PIC S9(15)V9(6).
           05  BO-GOOD-QTY             PIC S9(15)V9(6).
           05  BO-GROSS-STARTS         PIC S9(15)V9(6).
           05  BO-NOMINAL-BASIS        PIC S9(13)V9(6).
           05  BO-YIELD                PIC S9(5)V9(6).
           05  BO-QUANTUM              PIC S9(13)V9(6).
           05  BO-BATCH-SIZE           PIC S9(13)V9(6).
           05  BO-RUN-COUNT            PIC S9(9) COMP-5.
           05  BO-BATCH-COUNT          PIC S9(9) COMP-5.
           05  BO-STOCK-USED           PIC S9(15)V9(6).
           05  BO-RESERVED-USED        PIC S9(15)V9(6).
           05  BO-INBOUND-USED         PIC S9(15)V9(6).
           05  BO-SHORTAGE             PIC S9(15)V9(6).
           05  BO-PACK-QTY             PIC S9(15)V9(6).
           05  BO-PACK-MULT            PIC S9(13)V9(6).
           05  BO-EXCESS               PIC S9(15)V9(6).
           05  BO-UNIT-COST            PIC S9(11)V9(6).
           05  BO-EXT-COST             PIC S9(13)V9(2).
           05  BO-BRANCH-COST          PIC S9(13)V9(2).
           05  BO-COST-ELEMENT         PIC X(10).
           05  BO-COST-SOURCE          PIC X(12).
           05  BO-FIXED-COST           PIC X.
           05  BO-NEED-DATE            PIC 9(8).
           05  BO-OPERATION            PIC X(20).
           05  BO-WARNING              PIC X(100).
