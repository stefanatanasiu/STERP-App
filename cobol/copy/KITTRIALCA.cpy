      *>================================================================
      *> KITTRIALCA - COMMAREA for KITTRIAL, non-committing material
      *>              feasibility ("trial kitting").
      *> SYSTEM      : STERP - Stefan's ERP
      *> MODULE      : KIT MAINTENANCE AND EXECUTION
      *>
      *> A trial changes NO stock, NO reservation, NO supply order and
      *> NO ledger balance (KIT-006).  It is a reading of the world at
      *> KT-SNAPSHOT, written to kit.TrialKit for evidence only.
      *>
      *> The answer separates, per specification 09 section 6:
      *>   gross structural requirement      KT-L-GROSS
      *>   requirement after netting         KT-L-NET-REQ
      *>   free eligible stock               KT-L-FREE
      *>   reservations owned by this demand KT-L-OWN-RESV
      *>   reservations owned by others      KT-L-OTHER-RESV
      *>   expected inbound and its date     KT-L-INBOUND / DATE
      *>   allocated to this trial           KT-L-ALLOCATED
      *>   immediate shortage                KT-L-SHORTAGE
      *>   time phased shortage date         KT-L-SHORT-DATE
      *> and reports the limiting component and the maximum complete
      *> output that satisfies EVERY constraint after yield, fixed
      *> requirements, batch steps, rounding and supply netting - not a
      *> naive minimum of component ratios.
      *>
      *> CA-FUNCTION values
      *>   TRIAL     feasibility for KT-REQUESTED-QTY at KT-REQ-DATE
      *>   MAXBUILD  TRIAL plus the maximum complete output search
      *>   GETLINE   page the retained result from KT-FROM-LINE
      *>   SAVE      write the retained result to kit.TrialKit and
      *>             kit.TrialKitLine under KT-TRIAL-REF (evidence
      *>             only - still no commitment)
      *>
      *> The result stays in working storage until the next TRIAL, so
      *> a client may page a large result without recalculating it and
      *> a repeated read has no live effect.
      *>
      *> REASON CODES  KIT0002, KIT0003, KIT0004, KIT0021, plus the
      *>   BOM reason codes passed through from BOMEXPL / BOMNET.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2015-01-19 PMK  CR2281 TRIAL KITTING
      *> 2015-08-04 PMK  CR2374 TRIAL MUST NOT RESERVE.  THE 1998
      *>                 VERSION "SOFT ALLOCATED" AND PLANNERS LOST
      *>                 STOCK TO ABANDONED WHAT-IFS.
      *> 2026-10-06 STERP KIT-006, SO-010
      *>================================================================
      *> REQ: KIT-006
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  KT-BODY.
               10  KT-TRIAL-REF        PIC X(30).
               10  KT-SCENARIO-NO      PIC S9(4).
               10  KT-KIT-REF          PIC X(30).
               10  KT-PARENT-ITEM      PIC X(40).
               10  KT-PARENT-REV       PIC X(4).
               10  KT-SITE             PIC X(10).
               10  KT-BOM-ID           PIC S9(9) COMP-5.
               10  KT-REQUESTED-QTY    PIC S9(13)V9(6).
               10  KT-REQ-DATE         PIC 9(8).
               10  KT-SELECTION-DATE   PIC 9(8).
               10  KT-SNAPSHOT         PIC X(20).
               10  KT-USE-INBOUND      PIC X.
                   88  KT-INBOUND-OK             VALUE "Y".
               10  KT-USE-SUBASSY      PIC X.
               10  KT-ELIG-POLICY      PIC X(40).
               10  KT-PRIORITY-SEQ     PIC X(60).
               10  KT-DEMAND-TYPE      PIC X(12).
               10  KT-DEMAND-REF       PIC X(40).
      *>       ---- result summary ----
               10  KT-TRIAL-ID         PIC S9(17).
               10  KT-MAX-BUILDABLE    PIC S9(13)V9(6).
               10  KT-LIMITING-ITEM    PIC X(40).
               10  KT-LIMITING-SHORT   PIC S9(13)V9(6).
               10  KT-SHORTAGE-CNT     PIC S9(4).
               10  KT-LINE-TOTAL       PIC S9(9) COMP-5.
               10  KT-COMPLETENESS     PIC X(12).
                   88  KT-COMPLETE               VALUE "COMPLETE".
                   88  KT-INCOMPLETE             VALUE "INCOMPLETE".
               10  KT-FEASIBLE         PIC X.
                   88  KT-IS-FEASIBLE            VALUE "Y".
               10  KT-COMMITTED        PIC X.
               10  KT-WARNINGS         PIC X(300).
               10  KT-ACTION-HINT      PIC X(60).
      *>       ---- paging window ----
               10  KT-FROM-LINE        PIC S9(9) COMP-5.
               10  KT-WIN-MAX          PIC S9(4).
               10  KT-WIN-CNT          PIC S9(4).
               10  KT-MORE-LINES       PIC X.
               10  KT-LINES OCCURS 20 TIMES.
                   15  KT-L-LINE-NUM   PIC S9(9) COMP-5.
                   15  KT-L-PATH       PIC X(20).
                   15  KT-L-DEPTH      PIC S9(4).
                   15  KT-L-ITEM       PIC X(40).
                   15  KT-L-REV        PIC X(4).
                   15  KT-L-UOM        PIC X(10).
                   15  KT-L-GROSS      PIC S9(13)V9(6).
                   15  KT-L-NET-REQ    PIC S9(13)V9(6).
                   15  KT-L-FREE       PIC S9(13)V9(6).
                   15  KT-L-OWN-RESV   PIC S9(13)V9(6).
                   15  KT-L-OTHER-RESV PIC S9(13)V9(6).
                   15  KT-L-INBOUND    PIC S9(13)V9(6).
                   15  KT-L-INB-DATE   PIC 9(8).
                   15  KT-L-ALLOCATED  PIC S9(13)V9(6).
                   15  KT-L-SHORTAGE   PIC S9(13)V9(6).
                   15  KT-L-SHORT-DATE PIC 9(8).
                   15  KT-L-LIMITING   PIC X.
                   15  KT-L-ACTION     PIC X(40).
                   15  KT-L-WARNING    PIC X(60).
