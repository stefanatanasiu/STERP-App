      *>================================================================
      *> KITQTYCA - COMMAREA for KITQTY, the kit quantity arithmetic.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : KIT MAINTENANCE AND EXECUTION
      *>
      *> Every rule of specification 09 section 5 lives here once, so
      *> the issue, return, scrap, amendment and close programs cannot
      *> each invent their own version of "outstanding".
      *>
      *>   net issued    = gross issued - confirmed issue returns
      *>   remaining     = max(0, current approved requirement
      *>                            - net issued)
      *>   excess issued = max(0, net issued
      *>                            - current approved requirement)
      *>
      *> and for material accountability
      *>
      *>   gross issued  = returned + scrapped
      *>                 + consumed in accepted production
      *>                 + unconsumed material still held on the job
      *>
      *> Scrap is a DISPOSITION of issued material, never a stock
      *> return, so it does not reduce net issued (KIT-014).  Reducing
      *> an approved requirement produces visible excess; it never
      *> rewrites an actual issue (KIT-003).
      *>
      *> CA-FUNCTION values
      *>   LINEQTY   derive net issued, remaining and excess from the
      *>             movement totals supplied
      *>   REQCHG    impact of moving the approved requirement from
      *>             KQ-CURRENT-REQ to KQ-PROPOSED-REQ: quantity
      *>             delta, resulting excess, cost impact and whether
      *>             authority is needed (KIT-005)
      *>   SCALE     recalculate one line for a new planned output.
      *>             Proportional usage scales; fixed-per-run usage
      *>             does not repeat; fixed-per-batch follows the
      *>             batch plan (KIT-016, D-10)
      *>   ACCOUNT   material accountability check at operational
      *>             close: anything unexplained is reported in
      *>             KQ-ACCOUNT-DIFF and KQ-ACCOUNTED is "N"
      *>
      *> KITQTY touches no database and no stock; it is pure
      *> arithmetic and is therefore safe to call inside a trial.
      *>
      *> REASON CODES  KIT0002 unsupported function,
      *>   KIT0003 quantity out of range, KIT0017 unexplained material.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2014-09-02 PMK  CR2240 ASSEMBLY EXECUTION REWRITE
      *> 2016-05-30 PMK  CR2488 EXCESS SEPARATED FROM OUTSTANDING.  THE
      *>                 1998 VERSION CLAMPED THE ISSUE BACK TO THE
      *>                 REQUIREMENT AND LOST THE EVIDENCE.
      *> 2026-10-06 STERP KIT-003, KIT-005, KIT-011, KIT-016
      *>================================================================
      *> REQ: KIT-003, KIT-005, KIT-011, KIT-016
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  KQ-BODY.
               10  KQ-KIT-REF          PIC X(30).
               10  KQ-LINE-NUM         PIC S9(9) COMP-5.
               10  KQ-ITEM-CODE        PIC X(40).
               10  KQ-UOM-CODE         PIC X(10).
               10  KQ-USAGE-BASIS      PIC X(12).
      *>       ---- requirement ----
               10  KQ-ORIGINAL-REQ     PIC S9(13)V9(6).
               10  KQ-CURRENT-REQ      PIC S9(13)V9(6).
               10  KQ-PROPOSED-REQ     PIC S9(13)V9(6).
      *>       ---- actual movement totals ----
               10  KQ-GROSS-ISSUED     PIC S9(13)V9(6).
               10  KQ-RETURNED         PIC S9(13)V9(6).
               10  KQ-SCRAPPED         PIC S9(13)V9(6).
               10  KQ-CONSUMED         PIC S9(13)V9(6).
               10  KQ-ON-JOB           PIC S9(13)V9(6).
      *>       ---- derived ----
               10  KQ-NET-ISSUED       PIC S9(13)V9(6).
               10  KQ-REMAINING        PIC S9(13)V9(6).
               10  KQ-EXCESS           PIC S9(13)V9(6).
               10  KQ-REQ-DELTA        PIC S9(13)V9(6).
               10  KQ-NEW-REQ          PIC S9(13)V9(6).
               10  KQ-ACCOUNT-DIFF     PIC S9(13)V9(6).
               10  KQ-ACCOUNTED        PIC X.
                   88  KQ-IS-ACCOUNTED           VALUE "Y".
      *>       ---- scaling context ----
               10  KQ-USAGE-PER        PIC S9(13)V9(6).
               10  KQ-SCRAP-PCT        PIC S9(5)V9(6).
               10  KQ-OLD-OUTPUT       PIC S9(13)V9(6).
               10  KQ-NEW-OUTPUT       PIC S9(13)V9(6).
               10  KQ-YIELD            PIC S9(5)V9(6).
               10  KQ-QUANTUM          PIC S9(13)V9(6).
               10  KQ-BATCH-SIZE       PIC S9(13)V9(6).
               10  KQ-RUN-COUNT        PIC S9(9) COMP-5.
               10  KQ-BATCH-COUNT      PIC S9(9) COMP-5.
               10  KQ-GROSS-STARTS     PIC S9(13)V9(6).
      *>       ---- impact and authority ----
               10  KQ-UNIT-COST        PIC S9(11)V9(6).
               10  KQ-COST-IMPACT      PIC S9(11)V99.
               10  KQ-RESV-IMPACT      PIC S9(13)V9(6).
               10  KQ-NEEDS-AUTH       PIC X.
                   88  KQ-AUTH-NEEDED            VALUE "Y".
               10  KQ-ISSUED-HISTORY   PIC X.
                   88  KQ-HAS-HISTORY            VALUE "Y".
               10  KQ-IMPACT-TEXT      PIC X(200).
