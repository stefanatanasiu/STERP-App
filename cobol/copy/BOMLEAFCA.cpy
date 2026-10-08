      *>================================================================
      *> BOMLEAFCA - COMMAREA of BOMLEAF: single level, indented and
      *>             flattened leaf views of one calculation.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : BILLS OF MATERIAL
      *> PURPOSE   : BOM-014/BOM-015.  The three views must reconcile
      *>             for the same inputs, and a displayed intermediate
      *>             subtotal is explanatory - never another amount to
      *>             add to its children.  Compatible leaves merge only
      *>             on identity, revision, unit and supply context; a
      *>             matching description is not a reason to merge.
      *> FUNCTIONS :
      *>   SINGLE   one level below BL-PARENT-ITEM (no expansion)
      *>   INDENT   indented multi level view, one page at a time
      *>   FLATTEN  aggregated leaf requirement, one page at a time
      *>   ITEMQTY  total requirement of BL-KEY-ITEM in stock units
      *> BL-STOP-SUMMARY counts why expansion stopped, so a depth limit
      *> is labelled and never passed off as a complete leaf list.
      *> Reason codes BOM0601..BOM0612.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2014-08-19 AJS  CR2233 ENGINEERING REWRITE
      *> 2026-10-05 STERP STOP REASON SUMMARY ON EVERY VIEW
      *>================================================================
      *> REQ: BOM-014, BOM-015, BOM-032
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  BL-BODY.
               10  BL-PARENT-ITEM      PIC X(40).
               10  BL-PARENT-REV       PIC X(4).
               10  BL-PURPOSE          PIC X(10).
               10  BL-JOB-REF          PIC X(20).
               10  BL-VARIANT          PIC X(20).
               10  BL-REQ-QTY          PIC S9(15)V9(6).
               10  BL-EFF-DATE         PIC 9(8).
               10  BL-MAX-DEPTH        PIC S9(4).
               10  BL-REEXPLODE        PIC X.
               10  BL-CALC-REF         PIC X(30).
               10  BL-OCC-COUNT        PIC S9(9) COMP-5.
               10  BL-LEAF-COUNT       PIC S9(9) COMP-5.
               10  BL-INTERMED-COUNT   PIC S9(9) COMP-5.
               10  BL-DEPTH-REACHED    PIC S9(4).
               10  BL-COMPLETENESS     PIC X(12).
               10  BL-STOP-SUMMARY     PIC X(120).
               10  BL-KEY-ITEM         PIC X(40).
               10  BL-KEY-TOTAL        PIC S9(15)V9(6).
               10  BL-KEY-UOM          PIC X(10).
               10  BL-FROM-LINE        PIC S9(9) COMP-5.
               10  BL-WIN-MAX          PIC S9(4).
               10  BL-WIN-CNT          PIC S9(4).
               10  BL-MORE-LINES       PIC X.
               10  BL-LINES OCCURS 25 TIMES.
                   15  BL-L-SEQ        PIC S9(9) COMP-5.
                   15  BL-L-DEPTH      PIC S9(4).
                   15  BL-L-INDENT     PIC X(50).
                   15  BL-L-PATH       PIC X(80).
                   15  BL-L-ITEM       PIC X(40).
                   15  BL-L-REV        PIC X(4).
                   15  BL-L-DESC       PIC X(40).
                   15  BL-L-UOM        PIC X(10).
                   15  BL-L-NODE-TYPE  PIC X(8).
                   15  BL-L-SUPPLY     PIC X(10).
                   15  BL-L-STOP       PIC X(20).
                   15  BL-L-BASIS      PIC X(12).
                   15  BL-L-USAGE      PIC S9(13)V9(6).
                   15  BL-L-UNROUNDED  PIC S9(15)V9(9).
                   15  BL-L-ROUND-INC  PIC S9(13)V9(6).
                   15  BL-L-REQ-QTY    PIC S9(15)V9(6).
                   15  BL-L-GOOD-QTY   PIC S9(15)V9(6).
                   15  BL-L-GROSS      PIC S9(15)V9(6).
                   15  BL-L-STOCK-USED PIC S9(15)V9(6).
                   15  BL-L-SHORTAGE   PIC S9(15)V9(6).
                   15  BL-L-PACK-QTY   PIC S9(15)V9(6).
                   15  BL-L-EXCESS     PIC S9(15)V9(6).
                   15  BL-L-EXT-COST   PIC S9(13)V9(2).
                   15  BL-L-CONTRIB    PIC S9(9) COMP-5.
                   15  BL-L-WARNING    PIC X(60).
