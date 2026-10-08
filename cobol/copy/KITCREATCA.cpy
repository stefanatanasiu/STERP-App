      *>================================================================
      *> KITCREATCA - COMMAREA for KITCREAT, execution kit creation,
      *>              review, release and enquiry.
      *> SYSTEM      : STERP - Stefan's ERP
      *> MODULE      : KIT MAINTENANCE AND EXECUTION
      *>
      *> An execution kit is the material requirement and the actual
      *> activity for ONE assembly, job or order.  It is never an extra
      *> item balance (KIT-001).  It is created from an approved bill
      *> of material or from an approved job specific requirement, and
      *> it preserves the source demand and the planned output
      *> (KIT-002).
      *>
      *> RELEASE is the freeze point (KIT-004): the selected effective
      *> bill and item revisions, units, option choices, yield, scrap,
      *> quantum, batch plan and selection date are copied onto the kit
      *> and no later standard change may alter them.  A standard bill
      *> edit afterwards is reported for review, never applied.
      *>
      *> CA-FUNCTION values
      *>   CREATE    build a draft execution kit.  KC-SOURCE-TYPE is
      *>             SALESORDER, JOB, STOCK, REWORK or PARENTKIT, and
      *>             KC-KIT-KIND decides which rules apply.  Explodes
      *>             the bill through BOMEXPL (or BOMJOB when a job
      *>             specific bill exists) and writes the requirement
      *>             lines with original = current requirement.
      *>   REVIEW    quantity, revision, supply choice and cost
      *>             completeness checks; DRAFT -> REVIEWED
      *>   RELEASE   freeze and authorise; REVIEWED -> RELEASED
      *>   ENQUIRE   header and derived quantities for KC-KIT-REF
      *>   GETLINE   page the material lines from KC-FROM-LINE
      *>   KINDCHK   answer "may this parent be received into stock as
      *>             a finished assembly?" for the caller's kit kind
      *>
      *> OUTPUT the Sales team needs: after CREATE, KC-KIT-REF holds
      *> the allocated reference and KC-EXEC-KIT-ID the surrogate key.
      *> A commercial kit (KC-KIT-KIND = "COMMERCIAL") never creates
      *> parent stock demand - see KITCOMM.
      *>
      *> REASON CODES - see KITWORK.  Chiefly KIT0001, KIT0002,
      *>   KIT0003, KIT0004, KIT0005, KIT0006, KIT0024.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2014-09-02 PMK  CR2240 ASSEMBLY EXECUTION REWRITE
      *> 2015-06-17 PMK  CR2356 JOB SPECIFIC BILLS (BOMJOB)
      *> 2016-11-22 PMK  CR2505 RELEASE FREEZES THE CALCULATION
      *> 2026-10-06 STERP KIT-001, KIT-002, KIT-004, KIT-024
      *>================================================================
      *> REQ: KIT-001, KIT-002, KIT-004, KIT-024
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  KC-BODY.
               10  KC-KIT-REF          PIC X(30).
               10  KC-KIT-KIND         PIC X(12).
                   88  KC-COMMERCIAL             VALUE "COMMERCIAL".
                   88  KC-STOCK-ASSY             VALUE "STOCKASSY".
                   88  KC-EXECUTION              VALUE "EXECUTION".
                   88  KC-PHANTOM                VALUE "PHANTOM".
                   88  KC-TRIAL-KIND             VALUE "TRIAL".
               10  KC-PARENT-ITEM      PIC X(40).
               10  KC-PARENT-REV       PIC X(4).
               10  KC-PARENT-DESC      PIC X(40).
               10  KC-SITE             PIC X(10).
               10  KC-BOM-ID           PIC S9(9) COMP-5.
               10  KC-BOM-REF          PIC X(30).
               10  KC-BOM-REV          PIC X(4).
               10  KC-SOURCE-TYPE      PIC X(12).
               10  KC-SOURCE-REF       PIC X(40).
               10  KC-JOB-REF          PIC X(20).
               10  KC-PARENT-KIT-REF   PIC X(30).
               10  KC-PLANNED-QTY      PIC S9(13)V9(6).
               10  KC-COMPLETED-QTY    PIC S9(13)V9(6).
               10  KC-REJECTED-QTY     PIC S9(13)V9(6).
               10  KC-NEED-DATE        PIC 9(8).
               10  KC-START-DATE       PIC 9(8).
               10  KC-COMPLETE-DATE    PIC 9(8).
               10  KC-SELECTION-DATE   PIC 9(8).
               10  KC-BACKFLUSH        PIC X.
               10  KC-OWNER-USER       PIC X(20).
               10  KC-STATUS           PIC X(14).
               10  KC-PREV-STATUS      PIC X(14).
               10  KC-EXEC-KIT-ID      PIC S9(17).
      *>       ---- frozen calculation assumptions (KIT-004) ----
               10  KC-GROSS-STARTS     PIC S9(13)V9(6).
               10  KC-YIELD            PIC S9(5)V9(6).
               10  KC-QUANTUM          PIC S9(13)V9(6).
               10  KC-BATCH-SIZE       PIC S9(13)V9(6).
               10  KC-RUN-COUNT        PIC S9(9) COMP-5.
               10  KC-BATCH-COUNT      PIC S9(9) COMP-5.
               10  KC-CONSOLIDATE      PIC X.
               10  KC-EXPAND-PHANTOM   PIC X.
      *>       ---- result summary ----
               10  KC-LINE-TOTAL       PIC S9(9) COMP-5.
               10  KC-SHORTAGE-CNT     PIC S9(4).
               10  KC-COMPLETENESS     PIC X(12).
                   88  KC-COMPLETE               VALUE "COMPLETE".
                   88  KC-INCOMPLETE             VALUE "INCOMPLETE".
               10  KC-PLANNED-COST     PIC S9(11)V99.
               10  KC-WIP-VALUE        PIC S9(11)V99.
               10  KC-APPROVAL-ID      PIC S9(17).
               10  KC-NEEDS-AUTH       PIC X.
               10  KC-MAY-RECEIVE      PIC X.
               10  KC-REASON-TEXT      PIC X(300).
               10  KC-WARNING-TEXT     PIC X(200).
      *>       ---- paging window ----
               10  KC-FROM-LINE        PIC S9(9) COMP-5.
               10  KC-WIN-MAX          PIC S9(4).
               10  KC-WIN-CNT          PIC S9(4).
               10  KC-MORE-LINES       PIC X.
               10  KC-LINES OCCURS 20 TIMES.
                   15  KC-L-LINE-NUM   PIC S9(9) COMP-5.
                   15  KC-L-PATH       PIC X(20).
                   15  KC-L-ITEM       PIC X(40).
                   15  KC-L-REV        PIC X(4).
                   15  KC-L-DESC       PIC X(40).
                   15  KC-L-BASIS      PIC X(12).
                   15  KC-L-UOM        PIC X(10).
                   15  KC-L-FACTOR     PIC S9(9)V9(9).
                   15  KC-L-ORIG-REQ   PIC S9(13)V9(6).
                   15  KC-L-CURR-REQ   PIC S9(13)V9(6).
                   15  KC-L-SCRAP-PCT  PIC S9(5)V9(6).
                   15  KC-L-RESERVED   PIC S9(13)V9(6).
                   15  KC-L-PICKED     PIC S9(13)V9(6).
                   15  KC-L-GROSS-ISS  PIC S9(13)V9(6).
                   15  KC-L-RETURNED   PIC S9(13)V9(6).
                   15  KC-L-SCRAPPED   PIC S9(13)V9(6).
                   15  KC-L-NET-ISS    PIC S9(13)V9(6).
                   15  KC-L-REMAINING  PIC S9(13)V9(6).
                   15  KC-L-EXCESS     PIC S9(13)V9(6).
                   15  KC-L-SUBST      PIC X(40).
                   15  KC-L-NEED-DATE  PIC 9(8).
                   15  KC-L-LOCATION   PIC X(20).
                   15  KC-L-UNIT-COST  PIC S9(11)V9(6).
                   15  KC-L-STATUS     PIC X(12).
