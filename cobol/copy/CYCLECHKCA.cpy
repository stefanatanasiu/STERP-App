      *>================================================================
      *> CYCLECHKCA - COMMAREA of CYCLECHK, direct and indirect cycle
      *>              detection over every permitted selection.
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : BILLS OF MATERIAL
      *> PURPOSE    : BOM-013.  Reject a direct self use and any
      *>              ancestry path that returns to the same selected
      *>              item / revision / bill context, and report the
      *>              offending path and component lines.  Two
      *>              independent paths that reach the same leaf or the
      *>              same subassembly are NOT a cycle (B6).
      *> FUNCTIONS  :
      *>   CHECK   walk the structure below CC-ROOT-ITEM at CC-EFF-DATE
      *>   EDGE    would adding component CC-CHILD-ITEM to the bill of
      *>           CC-ROOT-ITEM create a cycle?  (editor / bulk change)
      *> CC-INCLUDE-OPT = "Y" also walks unselected alternates, because
      *> a permitted configuration could activate the complete loop.
      *> Reason codes BOM0401..BOM0409.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2014-06-10 AJS  CR2219 ENGINEERING REWRITE - RECURSIVE
      *> 2017-02-14 AJS  CR2588 WALK PERMITTED ALTERNATIVES AS WELL
      *>================================================================
      *> REQ: BOM-013, BOM-019, BOM-029
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  CC-BODY.
               10  CC-ROOT-ITEM        PIC X(40).
               10  CC-ROOT-REV         PIC X(4).
               10  CC-CHILD-ITEM       PIC X(40).
               10  CC-CHILD-REV        PIC X(4).
               10  CC-PURPOSE          PIC X(10).
               10  CC-VARIANT          PIC X(20).
               10  CC-EFF-DATE         PIC 9(8).
               10  CC-MAX-DEPTH        PIC S9(4).
               10  CC-INCLUDE-OPT      PIC X.
               10  CC-CYCLE-FOUND      PIC X.
               10  CC-CYCLE-ITEM       PIC X(40).
               10  CC-CYCLE-PATH       PIC X(240).
               10  CC-CYCLE-LINE       PIC S9(4).
               10  CC-NODES-VISITED    PIC S9(9) COMP-5.
               10  CC-DEPTH-REACHED    PIC S9(4).
               10  CC-SHARED-CNT       PIC S9(4).
               10  CC-LIMIT-HIT        PIC X.
