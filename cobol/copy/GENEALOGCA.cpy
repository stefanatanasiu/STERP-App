      *>================================================================
      *> GENEALOGCA - COMMAREA for GENEALOG, kit genealogy enquiry.
      *> SYSTEM      : STERP - Stefan's ERP
      *> MODULE      : KIT MAINTENANCE AND EXECUTION
      *>
      *> KIT-019 wants the traceability to work in BOTH directions and
      *> to show what actually happened, not the plan:
      *>   FORWARD   this finished parent lot or serial was built from
      *>             these component lots and serials (and these were
      *>             substitutes, these were returned, these were
      *>             scrapped)
      *>   BACKWARD  this component lot or serial went into these
      *>             finished parents, kits and despatches - the
      *>             question a recall actually asks
      *>   WHEREUSD  open kits that still carry a requirement for this
      *>             component (section 10 enquiry)
      *>
      *> A recall enquiry must be able to tell a PLANNED component
      *> from the component ACTUALLY issued, so every line carries
      *> GN-L-PLANNED-ITEM as well as GN-L-ITEM and the substitution
      *> flag.
      *>
      *> Depth: GN-MAX-DEPTH levels of nested kits are followed, so a
      *> subassembly built on its own kit is traced through to its own
      *> components.  GN-TRUNCATED says the answer was cut short.
      *>
      *> REASON CODES  KIT0001, KIT0002, KIT0012, KIT0026 (no
      *>   genealogy recorded for the identity given).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2015-06-29 PMK  CR2361 GENEALOGY ENQUIRY
      *> 2016-08-02 PMK  CR2494 BACKWARD TRACE FOR RECALLS
      *> 2026-10-06 STERP KIT-019
      *>================================================================
      *> REQ: KIT-019
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  GN-BODY.
               10  GN-DIRECTION        PIC X(10).
               10  GN-KIT-REF          PIC X(30).
               10  GN-ITEM-CODE        PIC X(40).
               10  GN-LOT-CODE         PIC X(30).
               10  GN-SERIAL-NO        PIC X(40).
               10  GN-COMPLETION-ID    PIC S9(17).
               10  GN-FROM-DATE        PIC 9(8).
               10  GN-TO-DATE          PIC 9(8).
               10  GN-MAX-DEPTH        PIC S9(4).
               10  GN-INCLUDE-RET      PIC X.
               10  GN-INCLUDE-SCRAP    PIC X.
      *>       ---- result ----
               10  GN-LINE-TOTAL       PIC S9(9) COMP-5.
               10  GN-DEPTH-REACHED    PIC S9(4).
               10  GN-SUBSTITUTE-CNT   PIC S9(4).
               10  GN-RETURN-CNT       PIC S9(4).
               10  GN-SCRAP-CNT        PIC S9(4).
               10  GN-KIT-CNT          PIC S9(4).
               10  GN-TRUNCATED        PIC X.
               10  GN-SUMMARY-TEXT     PIC X(200).
      *>       ---- paging window ----
               10  GN-FROM-LINE        PIC S9(9) COMP-5.
               10  GN-WIN-MAX          PIC S9(4).
               10  GN-WIN-CNT          PIC S9(4).
               10  GN-MORE-LINES       PIC X.
               10  GN-LINES OCCURS 20 TIMES.
                   15  GN-L-SEQ        PIC S9(9) COMP-5.
                   15  GN-L-DEPTH      PIC S9(4).
                   15  GN-L-KIT-REF    PIC X(30).
                   15  GN-L-PARENT     PIC X(40).
                   15  GN-L-PAR-LOT    PIC X(30).
                   15  GN-L-PAR-SERIAL PIC X(40).
                   15  GN-L-ITEM       PIC X(40).
                   15  GN-L-LOT        PIC X(30).
                   15  GN-L-SERIAL     PIC X(40).
                   15  GN-L-QTY        PIC S9(13)V9(6).
                   15  GN-L-UOM        PIC X(10).
                   15  GN-L-PLANNED    PIC X(40).
                   15  GN-L-SUBSTITUTE PIC X.
                   15  GN-L-EVENT      PIC X(12).
                   15  GN-L-WHEN       PIC 9(8).
                   15  GN-L-OPERATOR   PIC X(20).
                   15  GN-L-NOTE       PIC X(60).
