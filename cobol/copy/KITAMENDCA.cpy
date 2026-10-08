      *>================================================================
      *> KITAMENDCA - COMMAREA for KITAMEND, kit amendment.
      *> SYSTEM      : STERP - Stefan's ERP
      *> MODULE      : KIT MAINTENANCE AND EXECUTION
      *>
      *> KIT-005: an amendment must SHOW its impact - quantity,
      *> reservation, cost and source demand - and must be authorised
      *> for the changed version.  KIT-003: it may never rewrite an
      *> actual issue or silently clamp away excess.  Deleting an
      *> issued line is therefore refused outright (KIT0015); an
      *> issued line whose requirement becomes zero stays on the kit
      *> with its history and its excess.
      *>
      *> CA-FUNCTION values
      *>   IMPACT    report the impact of the proposed change without
      *>             applying it
      *>   AMEND     apply an authorised requirement change to one
      *>             line (KA-LINE-NUM, KA-PROPOSED-REQ)
      *>   SCALE     change the planned good output of the kit and
      *>             recalculate the affected lines.  Proportional
      *>             usage scales, fixed per run usage does NOT
      *>             repeat, fixed per batch follows the batch plan
      *>             (D-10, KIT-016).
      *>   ADDLINE   add an approved extra requirement (for example
      *>             replacement material after scrap)
      *>   CANCELLN  cancel an UNISSUED line and release its
      *>             reservations
      *>   SUBST     approve a context specific substitute for a line.
      *>             The standard bill is not touched (KIT-012).
      *>
      *> REASON CODES  KIT0001..KIT0003, KIT0005, KIT0007, KIT0008,
      *>   KIT0009, KIT0014, KIT0015.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2015-04-20 PMK  CR2320 KIT AMENDMENT WITH IMPACT
      *> 2016-05-30 PMK  CR2488 EXCESS STAYS VISIBLE AFTER A REDUCTION
      *> 2026-10-06 STERP KIT-003, KIT-005, KIT-012
      *>================================================================
      *> REQ: KIT-003, KIT-005, KIT-012
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  KA-BODY.
               10  KA-KIT-REF          PIC X(30).
               10  KA-LINE-NUM         PIC S9(9) COMP-5.
               10  KA-ITEM-CODE        PIC X(40).
               10  KA-REVISION         PIC X(4).
               10  KA-PROPOSED-REQ     PIC S9(13)V9(6).
               10  KA-NEW-OUTPUT       PIC S9(13)V9(6).
               10  KA-UOM-CODE         PIC X(10).
               10  KA-USAGE-BASIS      PIC X(12).
               10  KA-SUBSTITUTE       PIC X(40).
               10  KA-SUB-RATIO        PIC S9(9)V9(6).
               10  KA-SUB-SPEC-OK      PIC X.
               10  KA-CUST-SPECIFIED   PIC X.
               10  KA-CUST-AGREED      PIC X.
               10  KA-REASON-CODE      PIC X(20).
               10  KA-REASON-TEXT      PIC X(300).
               10  KA-APPROVAL-ID      PIC S9(17).
               10  KA-AUTHORITY-REF    PIC X(40).
               10  KA-NEED-DATE        PIC 9(8).
               10  KA-LOCATION         PIC X(20).
      *>       ---- impact ----
               10  KA-OLD-REQ          PIC S9(13)V9(6).
               10  KA-NEW-REQ          PIC S9(13)V9(6).
               10  KA-REQ-DELTA        PIC S9(13)V9(6).
               10  KA-NET-ISSUED       PIC S9(13)V9(6).
               10  KA-REMAINING        PIC S9(13)V9(6).
               10  KA-EXCESS           PIC S9(13)V9(6).
               10  KA-RESV-IMPACT      PIC S9(13)V9(6).
               10  KA-COST-IMPACT      PIC S9(11)V99.
               10  KA-LINES-CHANGED    PIC S9(4).
               10  KA-FIXED-KEPT       PIC S9(4).
               10  KA-SOURCE-TYPE      PIC X(12).
               10  KA-SOURCE-REF       PIC X(40).
               10  KA-NEEDS-AUTH       PIC X.
               10  KA-HAS-HISTORY      PIC X.
               10  KA-NEW-LINE-NUM     PIC S9(9) COMP-5.
               10  KA-KIT-STATUS       PIC X(14).
               10  KA-IMPACT-TEXT      PIC X(300).
