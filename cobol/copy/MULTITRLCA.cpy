      *>================================================================
      *> MULTITRLCA - COMMAREA for MULTITRL, the multi-kit trial.
      *> SYSTEM      : STERP - Stefan's ERP
      *> MODULE      : KIT MAINTENANCE AND EXECUTION
      *>
      *> Several kits compete for the same free stock.  KIT-007 says
      *> the shared available stock and inbound supply are allocated
      *> ONCE, using an explicit priority sequence, and the assumptions
      *> are recorded.  Two competing kits can never both be reported
      *> complete using the same units.
      *>
      *> The baseline sequence is approved priority, then required
      *> date, then the stable demand reference.  Any manual
      *> resequencing is recorded as a NEW scenario (MT-SCENARIO-NO)
      *> rather than overwriting the first answer.
      *>
      *> Specification 09 section 6 worked example: two kits each need
      *> four identical pins and six eligible pins are free.  With kit
      *> A ahead of kit B the trial covers A's four and B's two,
      *> leaving B short by two.  It must not label both complete.
      *>
      *> CA-FUNCTION values
      *>   CLEAR     start a new multi-kit scenario
      *>   ADDKIT    add one demand to the scenario (MT-K-* fields in
      *>             MT-ADD) with its priority and required date
      *>   RUN       expand every demand, pool the requirements by
      *>             component, allocate the one reading of free stock
      *>             in priority order and report per kit and per
      *>             component
      *>   GETKIT    page the per kit verdicts
      *>   GETCOMP   page the per component allocation evidence
      *>
      *> Nothing is reserved, ordered or posted.  MT-COMMITTED is
      *> always "N" and is checked by the acceptance tests.
      *>
      *> REASON CODES  KIT0002, KIT0003, KIT0021, KIT0025 (too many
      *>   kits for the scenario table).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2015-03-05 PMK  CR2299 MULTI KIT TRIAL
      *> 2016-02-16 PMK  CR2451 ALLOCATE SHARED LEAVES ONCE.  BEFORE
      *>                 THIS EACH KIT WAS TRIALLED ALONE AND THE
      *>                 SHOP FLOOR WAS PROMISED STOCK TWICE.
      *> 2026-10-06 STERP KIT-007
      *>================================================================
      *> REQ: KIT-007
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  MT-BODY.
               10  MT-SCENARIO-REF     PIC X(30).
               10  MT-SCENARIO-NO      PIC S9(4).
               10  MT-SITE             PIC X(10).
               10  MT-AS-AT            PIC 9(8).
               10  MT-SEQUENCE-RULE    PIC X(60).
               10  MT-MANUAL-SEQ       PIC X.
               10  MT-USE-INBOUND      PIC X.
               10  MT-COMMITTED        PIC X.
      *>       ---- ADDKIT input ----
               10  MT-ADD.
                   15  MT-A-KIT-REF    PIC X(30).
                   15  MT-A-PARENT     PIC X(40).
                   15  MT-A-REVISION   PIC X(4).
                   15  MT-A-QTY        PIC S9(13)V9(6).
                   15  MT-A-PRIORITY   PIC 9(2).
                   15  MT-A-REQ-DATE   PIC 9(8).
                   15  MT-A-DEMAND-REF PIC X(40).
      *>       ---- result summary ----
               10  MT-KIT-CNT          PIC S9(4).
               10  MT-COMP-CNT         PIC S9(4).
               10  MT-COMPLETE-CNT     PIC S9(4).
               10  MT-SHORT-CNT        PIC S9(4).
               10  MT-CONFLICT-CNT     PIC S9(4).
               10  MT-ASSUMPTIONS      PIC X(300).
      *>       ---- per kit verdicts (GETKIT) ----
               10  MT-KFROM            PIC S9(4).
               10  MT-KWIN-CNT         PIC S9(4).
               10  MT-KMORE            PIC X.
               10  MT-KITS OCCURS 12 TIMES.
                   15  MT-K-SEQ        PIC S9(4).
                   15  MT-K-KIT-REF    PIC X(30).
                   15  MT-K-PARENT     PIC X(40).
                   15  MT-K-QTY        PIC S9(13)V9(6).
                   15  MT-K-PRIORITY   PIC 9(2).
                   15  MT-K-REQ-DATE   PIC 9(8).
                   15  MT-K-COVERED    PIC S9(13)V9(6).
                   15  MT-K-SHORT-CNT  PIC S9(4).
                   15  MT-K-LIMITING   PIC X(40).
                   15  MT-K-LIMIT-SHRT PIC S9(13)V9(6).
                   15  MT-K-VERDICT    PIC X(12).
                   15  MT-K-NOTE       PIC X(60).
      *>       ---- per component allocation (GETCOMP) ----
               10  MT-CFROM            PIC S9(4).
               10  MT-CWIN-CNT         PIC S9(4).
               10  MT-CMORE            PIC X.
               10  MT-COMPS OCCURS 20 TIMES.
                   15  MT-C-ITEM       PIC X(40).
                   15  MT-C-UOM        PIC X(10).
                   15  MT-C-FREE       PIC S9(13)V9(6).
                   15  MT-C-INBOUND    PIC S9(13)V9(6).
                   15  MT-C-DEMAND     PIC S9(13)V9(6).
                   15  MT-C-ALLOCATED  PIC S9(13)V9(6).
                   15  MT-C-SHORTAGE   PIC S9(13)V9(6).
                   15  MT-C-CLAIMS     PIC S9(4).
                   15  MT-C-CONFLICT   PIC X.
                   15  MT-C-FIRST-KIT  PIC X(30).
                   15  MT-C-SHORT-KIT  PIC X(30).
