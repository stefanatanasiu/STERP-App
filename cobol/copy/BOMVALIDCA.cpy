      *>================================================================
      *> BOMVALIDCA - COMMAREA of BOMVALID, complete bill validation.
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : BILLS OF MATERIAL
      *> PURPOSE    : BOM-004 - release requires complete validation and
      *>              an authorised engineering review.  BOMVALID is the
      *>              "complete validation" half: it checks the header
      *>              arithmetic, every component occurrence, the
      *>              alternate/optional configuration, the by-products,
      *>              the non-material lines and (through CYCLECHK) the
      *>              ancestry, and records the outcome in
      *>              bom.BomValidation so the release procedure can
      *>              insist on it.  Nothing is released from here.
      *> FUNCTIONS  :
      *>   VALIDATE  check and record a validation run (BV-VALID-ID)
      *>   CHECK     check only, record nothing (editor feedback)
      *> Each finding carries its path and reason (BOM-029).
      *> Reason codes BOM0301..BOM0340.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2014-05-15 AJS  CR2211 ENGINEERING REWRITE
      *> 2016-05-24 AJS  CR2466 UNCONFIGURED ALTERNATE GROUPS (BOM-018)
      *> 2026-10-05 STERP COST-ONLY RATES AND NOTE LINES (BOM-027)
      *> 2026-10-07 STERP BOM0317 INCOMPATIBLE PARENT CLASSIFICATION
      *>================================================================
      *> REQ: BOM-001, BOM-002, BOM-004, BOM-010, BOM-013, BOM-018,
      *> REQ: BOM-026, BOM-027, BOM-029
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  BV-BODY.
               10  BV-BOM-ID           PIC S9(9) COMP-5.
               10  BV-BOM-REF          PIC X(30).
               10  BV-BOM-REV          PIC X(4).
               10  BV-PARENT-ITEM      PIC X(40).
               10  BV-EFF-DATE         PIC 9(8).
               10  BV-VALID-REF        PIC X(30).
               10  BV-VALID-ID         PIC S9(9) COMP-5.
               10  BV-OUTCOME          PIC X(12).
               10  BV-ERROR-CNT        PIC S9(4).
               10  BV-WARNING-CNT      PIC S9(4).
               10  BV-COMPONENT-CNT    PIC S9(4).
               10  BV-CYCLE-FOUND      PIC X.
               10  BV-CYCLE-PATH       PIC X(200).
               10  BV-RELEASE-OK       PIC X.
               10  BV-FIND-CNT         PIC S9(4).
               10  BV-FIND-MAX         PIC S9(4).
               10  BV-FINDINGS OCCURS 50 TIMES.
                   15  BV-F-SEVERITY   PIC X(8).
                   15  BV-F-REASON     PIC X(8).
                   15  BV-F-LINE-NUM   PIC S9(4).
                   15  BV-F-ITEM       PIC X(40).
                   15  BV-F-PATH       PIC X(80).
                   15  BV-F-MESSAGE    PIC X(120).
