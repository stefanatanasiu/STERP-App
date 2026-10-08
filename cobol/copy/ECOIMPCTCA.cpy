      *>================================================================
      *> ECOIMPCTCA - COMMAREA of ECOIMPCT, engineering change impact.
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : BILLS OF MATERIAL
      *> PURPOSE    : BOM-031/BOM-028.  Before an engineering change is
      *>              activated, list the released execution kits, the
      *>              open demand, the job bills and the parent bills
      *>              it touches.  Each released kit needs an explicit
      *>              RETAIN, ADOPT or REPLAN decision - new standard
      *>              effectivity never silently re-explodes a released
      *>              kit.  RETIRE refuses to retire a bill that
      *>              released history still depends on, so old kit
      *>              requirements stay reproducible.
      *> FUNCTIONS  :
      *>   IMPACT   build / refresh the impact list of EI-CHANGE-REF
      *>   DECIDE   record EI-DECISION for one impact row
      *>   PENDING  count the undecided rows (release gate)
      *>   RETIRE   check whether EI-BOM-ID may be retired
      *> Reason codes BOM1301..BOM1314.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2016-07-06 AJS  CR2488 CHANGE IMPACT ON RELEASED KITS
      *> 2026-10-05 STERP RETIREMENT REFERENCE CHECK (BOM-028)
      *>================================================================
      *> REQ: BOM-028, BOM-031
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  EI-BODY.
               10  EI-CHANGE-REF       PIC X(30).
               10  EI-CHANGE-ID        PIC S9(9) COMP-5.
               10  EI-ITEM             PIC X(40).
               10  EI-NEW-ITEM         PIC X(40).
               10  EI-BOM-ID           PIC S9(9) COMP-5.
               10  EI-EFF-DATE         PIC 9(8).
               10  EI-IMPACT-ID        PIC S9(9) COMP-5.
               10  EI-DECISION         PIC X(10).
               10  EI-DECISION-NOTE    PIC X(200).
               10  EI-KIT-CNT          PIC S9(4).
               10  EI-DEMAND-CNT       PIC S9(4).
               10  EI-JOBBOM-CNT       PIC S9(4).
               10  EI-PARENT-CNT       PIC S9(4).
               10  EI-PENDING-CNT      PIC S9(4).
               10  EI-TOTAL-CNT        PIC S9(4).
               10  EI-MAY-RETIRE       PIC X.
               10  EI-MAY-ACTIVATE     PIC X.
               10  EI-BLOCK-REASON     PIC X(200).
               10  EI-FROM-LINE        PIC S9(9) COMP-5.
               10  EI-WIN-MAX          PIC S9(4).
               10  EI-WIN-CNT          PIC S9(4).
               10  EI-MORE-LINES       PIC X.
               10  EI-LINES OCCURS 25 TIMES.
                   15  EI-L-IMPACT-ID  PIC S9(9) COMP-5.
                   15  EI-L-TYPE       PIC X(12).
                   15  EI-L-DOC-TYPE   PIC X(20).
                   15  EI-L-DOC-REF    PIC X(40).
                   15  EI-L-DOC-STATUS PIC X(12).
                   15  EI-L-PARENT     PIC X(40).
                   15  EI-L-COMPONENT  PIC X(40).
                   15  EI-L-BOM-ID     PIC S9(9) COMP-5.
                   15  EI-L-QTY        PIC S9(15)V9(6).
                   15  EI-L-NEED-DATE  PIC 9(8).
                   15  EI-L-DECISION   PIC X(10).
                   15  EI-L-DECIDED-BY PIC X(20).
                   15  EI-L-PATH       PIC X(100).
