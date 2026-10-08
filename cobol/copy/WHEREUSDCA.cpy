      *>================================================================
      *> WHEREUSDCA - COMMAREA of WHEREUSD, where-used enquiry.
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : BILLS OF MATERIAL
      *> PURPOSE    : BOM-023.  Direct parents, all ancestors, the
      *>              selection context of each use and the open
      *>              execution demand that would be affected.  A use
      *>              that no permitted configuration could activate is
      *>              reported as POSSIBLE, never as confirmed usage.
      *> FUNCTIONS  :
      *>   DIRECT   immediate parents of WU-ITEM at WU-EFF-DATE
      *>   ALL      every ancestor, breadth first, with the path
      *>   OPENUSE  released kits and open demand that use the item
      *>   TOPLEVEL only the end products at the top of each path
      *> Reason codes BOM0901..BOM0909.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2014-10-07 AJS  CR2248 WHERE USED REWRITTEN ITERATIVELY
      *> 2026-10-05 STERP OPEN EXECUTION USE (BOM-023, BOM-031)
      *>================================================================
      *> REQ: BOM-023, BOM-028, BOM-031
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  WU-BODY.
               10  WU-ITEM             PIC X(40).
               10  WU-ITEM-REV         PIC X(4).
               10  WU-EFF-DATE         PIC 9(8).
               10  WU-PURPOSE          PIC X(10).
               10  WU-MAX-DEPTH        PIC S9(4).
               10  WU-INCLUDE-DRAFT    PIC X.
               10  WU-INCLUDE-OPT      PIC X.
               10  WU-PARENT-CNT       PIC S9(9) COMP-5.
               10  WU-TOP-CNT          PIC S9(9) COMP-5.
               10  WU-DEMAND-CNT       PIC S9(9) COMP-5.
               10  WU-DEPTH-REACHED    PIC S9(4).
               10  WU-COMPLETENESS     PIC X(12).
               10  WU-FROM-LINE        PIC S9(9) COMP-5.
               10  WU-WIN-MAX          PIC S9(4).
               10  WU-WIN-CNT          PIC S9(4).
               10  WU-MORE-LINES       PIC X.
               10  WU-LINES OCCURS 25 TIMES.
                   15  WU-L-SEQ        PIC S9(9) COMP-5.
                   15  WU-L-LEVEL      PIC S9(4).
                   15  WU-L-PARENT     PIC X(40).
                   15  WU-L-PARENT-REV PIC X(4).
                   15  WU-L-DESC       PIC X(40).
                   15  WU-L-BOM-REF    PIC X(30).
                   15  WU-L-BOM-REV    PIC X(4).
                   15  WU-L-STATUS     PIC X(12).
                   15  WU-L-PURPOSE    PIC X(10).
                   15  WU-L-VARIANT    PIC X(20).
                   15  WU-L-SITE       PIC X(10).
                   15  WU-L-LINE-NUM   PIC S9(4).
                   15  WU-L-USAGE      PIC S9(13)V9(6).
                   15  WU-L-UOM        PIC X(10).
                   15  WU-L-BASIS      PIC X(12).
                   15  WU-L-EFF-FROM   PIC 9(8).
                   15  WU-L-EFF-TO     PIC 9(8).
                   15  WU-L-CERTAINTY  PIC X(10).
                   15  WU-L-PATH       PIC X(120).
                   15  WU-L-DEMAND     PIC X(60).
