      *>================================================================
      *> KITCOMPLCA - COMMAREA for KITCOMPL, parent completion.
      *> SYSTEM      : STERP - Stefan's ERP
      *> MODULE      : KIT MAINTENANCE AND EXECUTION
      *>
      *> Completion records the ACTUAL good output of an execution
      *> run, captures the genealogy of the units produced, validates
      *> the component and approval conditions and transfers the
      *> appropriate work in progress value to finished stock
      *> (KIT-015).  It creates the parent balance and nothing else -
      *> there is no additional "kit" stock balance.
      *>
      *> Partial completion (KIT-016) keeps the remaining planned
      *> output, the remaining requirements and the residual work in
      *> progress open.  Six units then four units of a ten unit run
      *> remain ONE execution run: fixed per run material and setup
      *> are not repeated, and the second receipt does not imply that
      *> all planned output was completed.
      *>
      *> A non-stock commercial parent may never be completed here
      *> (KIT-001): KITCOMPL refuses with KIT0006.
      *>
      *> CA-FUNCTION values
      *>   COMPLETE  record good output (and rejected output) with its
      *>             lot or serial, transfer WIP and write genealogy
      *>   COMPCHK   dry run: report what would be received, the WIP
      *>             that would transfer and any blocking condition
      *>   FINAL     complete and declare the run operationally
      *>             finished (KC-FINAL = "Y" has the same effect)
      *>
      *> REASON CODES  KIT0001..KIT0003, KIT0005, KIT0006, KIT0008,
      *>   KIT0009, KIT0012, KIT0016, KIT0017, KIT0018.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2015-03-23 PMK  CR2305 COMPLETION AND WIP TRANSFER
      *> 2015-11-09 PMK  CR2401 PARTIAL COMPLETION KEEPS ONE RUN
      *> 2016-06-21 PMK  CR2490 GENEALOGY WRITTEN AT COMPLETION
      *> 2026-10-06 STERP KIT-015, KIT-016, KIT-019
      *>================================================================
      *> REQ: KIT-015, KIT-016, KIT-019
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  KM-BODY.
               10  KM-KIT-REF          PIC X(30).
               10  KM-PARENT-ITEM      PIC X(40).
               10  KM-REVISION         PIC X(4).
               10  KM-SITE             PIC X(10).
               10  KM-WAREHOUSE        PIC X(20).
               10  KM-LOCATION         PIC X(20).
               10  KM-LOT-CODE         PIC X(30).
               10  KM-SERIAL-NO        PIC X(40).
               10  KM-GOOD-QTY         PIC S9(13)V9(6).
               10  KM-REJECTED-QTY     PIC S9(13)V9(6).
               10  KM-SCRAP-OUT-QTY    PIC S9(13)V9(6).
               10  KM-UOM-CODE         PIC X(10).
               10  KM-COMPLETION-DATE  PIC 9(8).
               10  KM-POSTING-DATE     PIC 9(8).
               10  KM-COST-BASIS       PIC X(4).
               10  KM-FINAL            PIC X.
                   88  KM-IS-FINAL               VALUE "Y".
               10  KM-BACKFLUSH        PIC X.
                   88  KM-DO-BACKFLUSH           VALUE "Y".
               10  KM-APPROVED-BY      PIC X(20).
               10  KM-CHECK-ONLY       PIC X.
               10  KM-REASON-TEXT      PIC X(300).
      *>       ---- result ----
               10  KM-COMPLETION-ID    PIC S9(17).
               10  KM-COMPLETION-SEQ   PIC S9(4).
               10  KM-MOVEMENT-ID      PIC S9(17).
               10  KM-UNIT-COST        PIC S9(11)V9(6).
               10  KM-WIP-TRANSFER     PIC S9(11)V99.
               10  KM-WIP-RESIDUAL     PIC S9(11)V99.
               10  KM-TOTAL-GOOD       PIC S9(13)V9(6).
               10  KM-REMAINING-OUT    PIC S9(13)V9(6).
               10  KM-KIT-STATUS       PIC X(14).
               10  KM-GENEALOGY-CNT    PIC S9(4).
               10  KM-BACKFLUSH-CNT    PIC S9(4).
               10  KM-SHORTAGE-CNT     PIC S9(4).
               10  KM-FAILED-ITEM      PIC X(40).
               10  KM-FIXED-APPLIED    PIC X.
               10  KM-IMPACT-TEXT      PIC X(200).
