      *>================================================================
      *> BOMBILL  - one selected bill in memory: the read request, the
      *>            header and its component lines.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : BILLS OF MATERIAL
      *> PURPOSE  : Parameter group of the internal reader BOMREAD
      *>              CALL "BOMREAD" USING BOM-RD-CTL BOM-HDR-REC
      *>                                   BOM-CMP-TAB
      *>            BOMREAD resolves the component item attributes that
      *>            the calculation needs (stock unit, allowed stock
      *>            quantum, class, make/buy, purchase multiple) at the
      *>            same time, because a missing item identity or a
      *>            missing/zero conversion must block a complete
      *>            result rather than default to one (BOM-010).
      *>            Place these in LOCAL-STORAGE in a recursive caller.
      *>            Compiles in fixed and free format (cols 8-72, *>).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2014-03-17 AJS  CR2201 ENGINEERING REWRITE
      *> 2016-05-24 AJS  CR2466 OPTION CHOICE REFERENCE (BOM-018)
      *> 2026-10-05 STERP COST-ONLY RATE AND ELEMENT (BOM-027)
      *>================================================================
      *> REQ: BOM-002, BOM-010, BOM-016, BOM-018, BOM-027
       01  BOM-RD-CTL.
           05  BR-FUNC                 PIC X(8).
           05  BR-COMPANY              PIC X(10).
           05  BR-SITE                 PIC X(10).
           05  BR-BOM-ID               PIC S9(9) COMP-5.
           05  BR-ITEM-CODE            PIC X(40).
           05  BR-EFF-DATE             PIC 9(8).
           05  BR-WANT-OPTIONS         PIC X.
           05  BR-STATUS               PIC X(8).
               88  BR-OK                         VALUE "OK".
               88  BR-NOTFOUND                   VALUE "NOTFOUND".
               88  BR-TOOMANY                    VALUE "TOOMANY".
               88  BR-SQLFAIL                    VALUE "SQLFAIL".
           05  BR-REASON               PIC X(8).
           05  BR-MESSAGE              PIC X(200).
           05  BR-LINE-COUNT           PIC S9(4).
           05  BR-MAX-LINES            PIC S9(4) VALUE 500.
       01  BOM-HDR-REC.
           05  BH-BOM-ID               PIC S9(9) COMP-5.
           05  BH-COMPANY              PIC X(10).
           05  BH-SITE                 PIC X(10).
           05  BH-BOM-REF              PIC X(30).
           05  BH-BOM-REV              PIC X(4).
           05  BH-PARENT-ITEM          PIC X(40).
           05  BH-PARENT-REV           PIC X(4).
           05  BH-PURPOSE              PIC X(10).
           05  BH-JOB-REF              PIC X(20).
           05  BH-VARIANT              PIC X(20).
           05  BH-DESCRIPTION          PIC X(60).
           05  BH-NOMINAL-BASIS        PIC S9(13)V9(6).
           05  BH-YIELD                PIC S9(5)V9(6).
           05  BH-QUANTUM              PIC S9(13)V9(6).
           05  BH-BATCH-SIZE           PIC S9(13)V9(6).
           05  BH-IS-PHANTOM           PIC X.
           05  BH-CONSOLIDATE          PIC X.
           05  BH-COSTING-LOT          PIC S9(13)V9(6).
           05  BH-SETUP-COST           PIC S9(11)V9(6).
           05  BH-CONV-COST            PIC S9(11)V9(6).
           05  BH-EFF-FROM             PIC 9(8).
           05  BH-EFF-TO               PIC 9(8).
           05  BH-STATUS               PIC X(12).
           05  BH-VALIDATION-ID        PIC S9(9) COMP-5.
           05  BH-LOSS-INTENT          PIC X(60).
       01  BOM-CMP-TAB.
           05  BOM-CMP-LINE OCCURS 500 TIMES.
               10  BC-LINE-NUM         PIC S9(4).
               10  BC-COMPONENT-ID     PIC S9(9) COMP-5.
               10  BC-ITEM             PIC X(40).
               10  BC-REV              PIC X(4).
               10  BC-DESC             PIC X(40).
               10  BC-USAGE-QTY        PIC S9(13)V9(6).
               10  BC-USAGE-BASIS      PIC X(12).
               10  BC-UOM              PIC X(10).
               10  BC-STOCK-UOM        PIC X(10).
               10  BC-CONV-FACT        PIC S9(9)V9(9).
               10  BC-SCRAP-PCT        PIC S9(5)V9(6).
               10  BC-SUPPLY           PIC X(10).
               10  BC-OPTIONAL         PIC X.
               10  BC-SELECTED         PIC X.
               10  BC-ALT-GROUP        PIC X(10).
               10  BC-OPTION-REF       PIC X(20).
               10  BC-EFF-FROM         PIC 9(8).
               10  BC-EFF-TO           PIC 9(8).
               10  BC-NEED-OFFSET      PIC S9(4).
               10  BC-OPERATION        PIC X(20).
               10  BC-COST-RATE        PIC S9(11)V9(6).
               10  BC-COST-ELEMENT     PIC X(10).
               10  BC-ITEM-CLASS       PIC X(12).
               10  BC-MAKE-BUY         PIC X(6).
               10  BC-QUANTUM          PIC S9(13)V9(6).
               10  BC-INDIVISIBLE      PIC X.
               10  BC-STOCKED          PIC X.
               10  BC-PACK-QTY         PIC S9(13)V9(6).
               10  BC-ITEM-FOUND       PIC X.
