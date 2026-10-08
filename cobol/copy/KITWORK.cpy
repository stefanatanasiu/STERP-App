      *>================================================================
      *> KITWORK  - shared working storage for the kit engine.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : KIT MAINTENANCE AND EXECUTION
      *>
      *> The kit module keeps one description of a kit header and one
      *> of a material line so that KITCREAT, KITAMEND, KITRESV,
      *> KITISSUE, KITRET, KITSCRAP, KITCOMPL, KITCLOSE and KITCOST all
      *> read the same columns in the same order.  It is NOT a
      *> COMMAREA: it is copied into WORKING-STORAGE only.
      *>
      *> KIT KINDS (specification 09 section 1) - the word "kit" must
      *> not hide five different economic processes:
      *>   COMMERCIAL  non-stock parent sold as a named group; the
      *>               components carry stock, cost and VAT
      *>   STOCKASSY   components transformed into a stocked parent
      *>   EXECUTION   the requirement and actual issues for one
      *>               assembly, job or order - never a stock balance
      *>   PHANTOM     a grouping expanded into the enclosing parent;
      *>               no receipt, no run, no separate stock
      *>   TRIAL       a calculation only; no commitment of any kind
      *>
      *> EXECUTION STATES (section 4).  Reservation completeness and
      *> accounting closure are deliberately separate from these.
      *>   DRAFT -> REVIEWED -> RELEASED -> PARTISSUED -> ISSUED
      *>         -> PARTCOMPLETE -> OPCOMPLETE -> FINCLOSED
      *>   CANCELLED may be reached from DRAFT..ISSUED.
      *>
      *> REASON CODE CATALOGUE (CA-REASON-CODE)
      *>   KIT0001 kit reference not known in this company
      *>   KIT0002 unsupported function
      *>   KIT0003 quantity is zero, negative or out of range
      *>   KIT0004 the bill of material is not approved or incomplete
      *>   KIT0005 kit status does not allow this action
      *>   KIT0006 parent item may not be stocked / may not be received
      *>   KIT0007 material line not known on this kit
      *>   KIT0008 a reason is required for this action
      *>   KIT0009 authority is required for this action
      *>   KIT0010 insufficient eligible stock - shortage
      *>   KIT0011 return exceeds the unreversed issued quantity
      *>   KIT0012 lot or serial identity is required
      *>   KIT0013 backflush is not permitted for traced material
      *>   KIT0014 substitution is not approved for this context
      *>   KIT0015 issued material cannot be deleted from history
      *>   KIT0016 the accounting period is closed
      *>   KIT0017 unconsumed material must be returned or disposed
      *>   KIT0018 completion exceeds the planned good output
      *>   KIT0029 completion is partial; the run stays open
      *>   KIT0019 recovery exceeds carrying value plus approved cost
      *>   KIT0020 incomplete component set cannot be despatched
      *>   KIT0021 trial produced no feasible quantity
      *>   KIT0022 overissue approval does not create physical stock
      *>   KIT0023 kit has already been financially closed
      *>   KIT0024 source demand is required for this kit kind
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2014-09-02 PMK  CR2240 ASSEMBLY EXECUTION REWRITE
      *> 2016-05-30 PMK  CR2488 EXCESS AND ACCOUNTABILITY COLUMNS
      *> 2026-10-06 STERP KIT-001 TO KIT-024
      *>================================================================
       01  KIT-HEADER-REC.
           05  KH-EXEC-KIT-ID          PIC S9(17) VALUE 0.
           05  KH-COMPANY              PIC X(10) VALUE SPACES.
           05  KH-SITE                 PIC X(10) VALUE SPACES.
           05  KH-KIT-REF              PIC X(30) VALUE SPACES.
           05  KH-PARENT-ITEM          PIC X(40) VALUE SPACES.
           05  KH-PARENT-REV           PIC X(4) VALUE SPACES.
           05  KH-BOM-ID               PIC S9(9) COMP-5 VALUE 0.
           05  KH-BOM-REV              PIC X(4) VALUE SPACES.
           05  KH-SOURCE-TYPE          PIC X(12) VALUE SPACES.
           05  KH-SOURCE-REF           PIC X(40) VALUE SPACES.
           05  KH-JOB-REF              PIC X(20) VALUE SPACES.
           05  KH-PARENT-KIT-REF       PIC X(30) VALUE SPACES.
           05  KH-PLANNED-QTY          PIC S9(13)V9(6) VALUE 0.
           05  KH-COMPLETED-QTY        PIC S9(13)V9(6) VALUE 0.
           05  KH-REJECTED-QTY         PIC S9(13)V9(6) VALUE 0.
           05  KH-GROSS-STARTS         PIC S9(13)V9(6) VALUE 0.
           05  KH-YIELD                PIC S9(5)V9(6) VALUE 1.
           05  KH-QUANTUM              PIC S9(13)V9(6) VALUE 0.
           05  KH-BATCH-SIZE           PIC S9(13)V9(6) VALUE 0.
           05  KH-RUN-COUNT            PIC S9(9) COMP-5 VALUE 1.
           05  KH-BATCH-COUNT          PIC S9(9) COMP-5 VALUE 0.
           05  KH-NEED-DATE            PIC 9(8) VALUE 0.
           05  KH-START-DATE           PIC 9(8) VALUE 0.
           05  KH-PLAN-COMP-DATE       PIC 9(8) VALUE 0.
           05  KH-ACT-COMP-DATE        PIC 9(8) VALUE 0.
           05  KH-SELECTION-DATE       PIC 9(8) VALUE 0.
           05  KH-OWNER-USER           PIC X(20) VALUE SPACES.
           05  KH-STATUS               PIC X(14) VALUE SPACES.
               88  KH-IS-DRAFT                   VALUE "DRAFT".
               88  KH-IS-REVIEWED                VALUE "REVIEWED".
               88  KH-IS-RELEASED                VALUE "RELEASED".
               88  KH-IS-PARTISSUED              VALUE "PARTISSUED".
               88  KH-IS-ISSUED                  VALUE "ISSUED".
               88  KH-IS-PARTCOMP                VALUE "PARTCOMPLETE".
               88  KH-IS-OPCOMPLETE              VALUE "OPCOMPLETE".
               88  KH-IS-FINCLOSED               VALUE "FINCLOSED".
               88  KH-IS-CANCELLED               VALUE "CANCELLED".
               88  KH-MAY-ISSUE                  VALUE "RELEASED"
                                                 "PARTISSUED"
                                                 "ISSUED"
                                                 "PARTCOMPLETE".
               88  KH-MAY-AMEND                  VALUE "DRAFT"
                                                 "REVIEWED"
                                                 "RELEASED"
                                                 "PARTISSUED"
                                                 "ISSUED".
               88  KH-IS-OPEN                    VALUE "DRAFT"
                                                 "REVIEWED"
                                                 "RELEASED"
                                                 "PARTISSUED"
                                                 "ISSUED"
                                                 "PARTCOMPLETE".
           05  KH-RELEASED-BY          PIC X(20) VALUE SPACES.
           05  KH-APPROVAL-ID          PIC S9(17) VALUE 0.
           05  KH-WIP-VALUE            PIC S9(11)V99 VALUE 0.
           05  KH-BACKFLUSH            PIC X VALUE "N".
               88  KH-BACKFLUSH-ON               VALUE "Y".
           05  KH-LINE-TOTAL           PIC S9(9) COMP-5 VALUE 0.
      *>================================================================
      *> One material line.  Original and current requirement are kept
      *> apart for ever: KIT-003 forbids a requirement change from
      *> rewriting an actual issue or silently clamping away excess.
      *>================================================================
       01  KIT-LINE-REC.
           05  KL-LINE-ID              PIC S9(17) VALUE 0.
           05  KL-LINE-NUM             PIC S9(9) COMP-5 VALUE 0.
           05  KL-PATH-REF             PIC X(20) VALUE SPACES.
           05  KL-ITEM-CODE            PIC X(40) VALUE SPACES.
           05  KL-REVISION             PIC X(4) VALUE SPACES.
           05  KL-BOM-COMP-ID          PIC S9(9) COMP-5 VALUE 0.
           05  KL-USAGE-BASIS          PIC X(12) VALUE "PROPORTION".
               88  KL-PROPORTIONAL               VALUE "PROPORTION".
               88  KL-FIXED-PER-RUN              VALUE "FIXEDRUN".
               88  KL-FIXED-PER-BATCH            VALUE "FIXEDBATCH".
           05  KL-ORIGINAL-REQ         PIC S9(13)V9(6) VALUE 0.
           05  KL-CURRENT-REQ          PIC S9(13)V9(6) VALUE 0.
           05  KL-UOM-CODE             PIC X(10) VALUE SPACES.
           05  KL-FACTOR               PIC S9(9)V9(9) VALUE 1.
           05  KL-SCRAP-PCT            PIC S9(5)V9(6) VALUE 0.
           05  KL-RESERVED-QTY         PIC S9(13)V9(6) VALUE 0.
           05  KL-PICKED-QTY           PIC S9(13)V9(6) VALUE 0.
           05  KL-GROSS-ISSUED         PIC S9(13)V9(6) VALUE 0.
           05  KL-RETURNED-QTY         PIC S9(13)V9(6) VALUE 0.
           05  KL-SCRAPPED-QTY         PIC S9(13)V9(6) VALUE 0.
           05  KL-NET-ISSUED           PIC S9(13)V9(6) VALUE 0.
           05  KL-REMAINING-QTY        PIC S9(13)V9(6) VALUE 0.
           05  KL-EXCESS-QTY           PIC S9(13)V9(6) VALUE 0.
           05  KL-SUBSTITUTE           PIC X(40) VALUE SPACES.
           05  KL-SUB-APPROVAL         PIC S9(17) VALUE 0.
           05  KL-DEVIATION            PIC X(300) VALUE SPACES.
           05  KL-DEV-APPROVER         PIC X(20) VALUE SPACES.
           05  KL-NEED-DATE            PIC 9(8) VALUE 0.
           05  KL-LOCATION             PIC X(20) VALUE SPACES.
           05  KL-PLAN-UNIT-COST       PIC S9(11)V9(6) VALUE 0.
           05  KL-ISSUED-VALUE         PIC S9(11)V99 VALUE 0.
           05  KL-STATUS               PIC X(12) VALUE "OPEN".
               88  KL-IS-OPEN                    VALUE "OPEN".
               88  KL-IS-PARTISSUED              VALUE "PARTISSUED".
               88  KL-IS-ISSUED                  VALUE "ISSUED".
               88  KL-IS-CLOSED                  VALUE "CLOSED".
               88  KL-IS-CANCELLED               VALUE "CANCELLED".
      *>================================================================
      *> Item traits the kit engine must respect before it touches
      *> stock.  Loaded from inv.Item by KITCREAT and KITISSUE.
      *>================================================================
       01  KIT-ITEM-REC.
           05  KI-ITEM-CODE            PIC X(40) VALUE SPACES.
           05  KI-DESCRIPTION          PIC X(40) VALUE SPACES.
           05  KI-ITEM-CLASS           PIC X(12) VALUE SPACES.
           05  KI-STOCK-UOM            PIC X(10) VALUE SPACES.
           05  KI-IS-STOCKED           PIC X VALUE "N".
               88  KI-STOCKED                    VALUE "Y".
           05  KI-LOT-CONTROL          PIC X VALUE "N".
               88  KI-LOT-TRACED                 VALUE "Y".
           05  KI-SERIAL-CONTROL       PIC X VALUE "N".
               88  KI-SERIAL-TRACED              VALUE "Y".
           05  KI-TRACED               PIC X VALUE "N".
               88  KI-IS-TRACED                  VALUE "Y".
           05  KI-STD-COST             PIC S9(11)V9(6) VALUE 0.
           05  KI-AVG-COST             PIC S9(11)V9(6) VALUE 0.
           05  KI-STATUS               PIC X(12) VALUE SPACES.
           05  KI-COST-METHOD          PIC X(4) VALUE SPACES.
               88  KI-STANDARD-COSTED            VALUE "STD".
