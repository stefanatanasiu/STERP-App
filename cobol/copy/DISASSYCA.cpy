      *>================================================================
      *> DISASSYCA - COMMAREA for DISASSY, controlled disassembly.
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : KIT MAINTENANCE AND EXECUTION
      *>
      *> Disassembly is a conditional capability (D-20) and applies to
      *> STOCKED assemblies only - never to a non-stock commercial
      *> parent (KIT-021, specification 09 section 9).
      *>
      *> It consumes an actual parent quantity, records the components
      *> ACTUALLY recovered in inspection status, and allocates the
      *> recoverable value on an approved basis.  The planned bill is
      *> a proposal, not proof that every original component came back
      *> undamaged, so DS-PLANNED-QTY and DS-RECOVERED-QTY are kept
      *> apart on every line.
      *>
      *> The value rule is absolute (D-12):
      *>
      *>   recovered value + recognised loss
      *>       = parent carrying value + approved disassembly costs
      *>
      *> Recovery can never create more carrying value than that.
      *> Specification example AC-084: carrying value 100.00, approved
      *> cost 10.00, recovery 95.00, loss 15.00 - 95 + 15 = 110.
      *>
      *> CA-FUNCTION values
      *>   CREATE    raise a draft disassembly order
      *>   ADDLINE   add one recovered (or damaged) component line
      *>   VALUE     allocate recoverable value and compute the loss
      *>   APPROVE   approve the draft
      *>   POST      consume the parent, receive the components in
      *>             inspection condition and post the loss
      *>   ENQUIRE   read back the order and its lines
      *>
      *> REASON CODES  KIT0002, KIT0003, KIT0006, KIT0008, KIT0009,
      *>   KIT0019.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2015-09-07 PMK  CR2381 CONTROLLED DISASSEMBLY
      *> 2016-04-11 PMK  CR2476 RECOVERY CAPPED AT CARRYING VALUE
      *> 2026-10-06 STERP KIT-021, D-12
      *>================================================================
      *> REQ: KIT-021
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  DS-BODY.
               10  DS-DISASSY-REF      PIC X(30).
               10  DS-SITE             PIC X(10).
               10  DS-PARENT-ITEM      PIC X(40).
               10  DS-REVISION         PIC X(4).
               10  DS-LOT-CODE         PIC X(30).
               10  DS-SERIAL-NO        PIC X(40).
               10  DS-CONSUMED-QTY     PIC S9(13)V9(6).
               10  DS-WAREHOUSE        PIC X(20).
               10  DS-LOCATION         PIC X(20).
               10  DS-DISASSY-DATE     PIC 9(8).
               10  DS-POSTING-DATE     PIC 9(8).
               10  DS-BOM-ID           PIC S9(9) COMP-5.
               10  DS-CARRYING-GBP     PIC S9(11)V99.
               10  DS-APPROVED-GBP     PIC S9(11)V99.
               10  DS-ALLOC-BASIS      PIC X(12).
               10  DS-REASON-TEXT      PIC X(300).
               10  DS-APPROVED-BY      PIC X(20).
      *>       ---- ADDLINE input ----
               10  DS-ADD.
                   15  DS-A-LINE-NUM   PIC S9(4).
                   15  DS-A-ITEM       PIC X(40).
                   15  DS-A-REV        PIC X(4).
                   15  DS-A-LOT        PIC X(30).
                   15  DS-A-SERIAL     PIC X(40).
                   15  DS-A-PLANNED    PIC S9(13)V9(6).
                   15  DS-A-RECOVERED  PIC S9(13)V9(6).
                   15  DS-A-DAMAGED    PIC S9(13)V9(6).
                   15  DS-A-UOM        PIC X(10).
                   15  DS-A-QUALITY    PIC X(12).
                   15  DS-A-UNIT-VAL   PIC S9(11)V9(6).
                   15  DS-A-LOCATION   PIC X(20).
      *>       ---- result ----
               10  DS-DISASSY-ID       PIC S9(9) COMP-5.
               10  DS-STATUS           PIC X(12).
               10  DS-RECOVERED-GBP    PIC S9(11)V99.
               10  DS-LOSS-GBP         PIC S9(11)V99.
               10  DS-AVAILABLE-GBP    PIC S9(11)V99.
               10  DS-LINE-CNT         PIC S9(4).
               10  DS-POSTED-CNT       PIC S9(4).
               10  DS-MOVEMENT-ID      PIC S9(17).
               10  DS-JOURNAL-ID       PIC S9(17).
               10  DS-BALANCED         PIC X.
               10  DS-IMPACT-TEXT      PIC X(200).
      *>       ---- line window ----
               10  DS-FROM-LINE        PIC S9(4).
               10  DS-WIN-CNT          PIC S9(4).
               10  DS-MORE-LINES       PIC X.
               10  DS-LINES OCCURS 15 TIMES.
                   15  DS-L-LINE-NUM   PIC S9(4).
                   15  DS-L-ITEM       PIC X(40).
                   15  DS-L-REV        PIC X(4).
                   15  DS-L-LOT        PIC X(30).
                   15  DS-L-SERIAL     PIC X(40).
                   15  DS-L-PLANNED    PIC S9(13)V9(6).
                   15  DS-L-RECOVERED  PIC S9(13)V9(6).
                   15  DS-L-DAMAGED    PIC S9(13)V9(6).
                   15  DS-L-UOM        PIC X(10).
                   15  DS-L-QUALITY    PIC X(12).
                   15  DS-L-VALUE      PIC S9(11)V99.
                   15  DS-L-LOCATION   PIC X(20).
