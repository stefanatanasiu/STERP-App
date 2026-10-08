      *>================================================================
      *> BOMCOMPCA - COMMAREA of BOMCOMP, revision comparison.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : BILLS OF MATERIAL
      *> PURPOSE   : BOM-025.  Added, removed and changed components,
      *>             with quantity, unit, basis, scrap and effectivity
      *>             differences - and a pure sequence change marked as
      *>             such so it is distinguishable from a material one.
      *> FUNCTIONS :
      *>   COMPARE  compare BP-FROM-BOM-ID with BP-TO-BOM-ID
      *>   BYREF    compare two revisions of the same BomRef
      *>   IMPACT   add the calculated quantity impact for BP-REQ-QTY
      *> Reason codes BOM1001..BOM1009.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2015-09-02 AJS  CR2355 REVISION COMPARISON ADDED
      *> 2026-10-05 STERP SEQUENCE ONLY CHANGES MARKED SEPARATELY
      *>================================================================
      *> REQ: BOM-025, BOM-032
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  BP-BODY.
               10  BP-BOM-REF          PIC X(30).
               10  BP-FROM-REV         PIC X(4).
               10  BP-TO-REV           PIC X(4).
               10  BP-FROM-BOM-ID      PIC S9(9) COMP-5.
               10  BP-TO-BOM-ID        PIC S9(9) COMP-5.
               10  BP-PARENT-ITEM      PIC X(40).
               10  BP-REQ-QTY          PIC S9(15)V9(6).
               10  BP-EFF-DATE         PIC 9(8).
               10  BP-ADDED-CNT        PIC S9(4).
               10  BP-REMOVED-CNT      PIC S9(4).
               10  BP-CHANGED-CNT      PIC S9(4).
               10  BP-SEQONLY-CNT      PIC S9(4).
               10  BP-SAME-CNT         PIC S9(4).
               10  BP-HEADER-DIFF      PIC X(200).
               10  BP-MATERIAL-CHANGE  PIC X.
               10  BP-COST-DELTA       PIC S9(13)V9(2).
               10  BP-FROM-COST        PIC S9(13)V9(2).
               10  BP-TO-COST          PIC S9(13)V9(2).
               10  BP-COMPLETENESS     PIC X(12).
               10  BP-FROM-LINE        PIC S9(9) COMP-5.
               10  BP-WIN-MAX          PIC S9(4).
               10  BP-WIN-CNT          PIC S9(4).
               10  BP-MORE-LINES       PIC X.
               10  BP-LINES OCCURS 25 TIMES.
                   15  BP-L-SEQ        PIC S9(9) COMP-5.
                   15  BP-L-ACTION     PIC X(10).
                   15  BP-L-ITEM       PIC X(40).
                   15  BP-L-REV        PIC X(4).
                   15  BP-L-DESC       PIC X(40).
                   15  BP-L-FROM-LINE  PIC S9(4).
                   15  BP-L-TO-LINE    PIC S9(4).
                   15  BP-L-FROM-QTY   PIC S9(13)V9(6).
                   15  BP-L-TO-QTY     PIC S9(13)V9(6).
                   15  BP-L-FROM-UOM   PIC X(10).
                   15  BP-L-TO-UOM     PIC X(10).
                   15  BP-L-FROM-BASIS PIC X(12).
                   15  BP-L-TO-BASIS   PIC X(12).
                   15  BP-L-FROM-SCRAP PIC S9(5)V9(6).
                   15  BP-L-TO-SCRAP   PIC S9(5)V9(6).
                   15  BP-L-QTY-DELTA  PIC S9(15)V9(6).
                   15  BP-L-DETAIL     PIC X(100).
