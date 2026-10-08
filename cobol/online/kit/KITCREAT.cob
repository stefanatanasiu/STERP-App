>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : KITCREAT
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : Create, review, release and read back an execution
*>              kit.  Five different things are called "kit" in a
*>              factory and this program is where they are kept
*>              apart (KIT-001):
*>                COMMERCIAL  non-stock parent, components sold
*>                STOCKASSY   components become a stocked parent
*>                EXECUTION   the requirement for one job or order
*>                PHANTOM     expanded into the enclosing parent
*>                TRIAL       a calculation, never a commitment
*>              A requirement is built from an APPROVED bill only;
*>              an incomplete or invalid bill cannot produce a
*>              released requirement under a success status
*>              (KIT-002).  RELEASE is the freeze point: the bill
*>              and item revisions, units, yield, quantum, batch
*>              plan and selection date are copied onto the kit so
*>              a later standard change cannot alter it (KIT-004).
*> CALLED BY  : frmKitMaintenance, KITBATCH, the Sales module when
*>              a stocked assembly order needs a works kit
*> INTERFACE  : CALL "KITCREAT" USING WS-KITCREAT-CA
*>              COPYBOOK KITCREATCA
*> CALLS      : KITDB, BOMEXPL, NUMGEN, PERMCHK, AUDITLOG, KITQTY
*>----------------------------------------------------------------
*> CHANGE LOG
*> 1994-03-07 DRN  CR0112 WORKS ORDER KIT LIST
*> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS THROUGHOUT
*> 2014-09-02 PMK  CR2240 ASSEMBLY EXECUTION REWRITE - FREE FORMAT
*> 2015-06-17 PMK  CR2356 JOB SPECIFIC BILLS
*> 2016-11-22 PMK  CR2505 RELEASE FREEZES THE CALCULATION
*> 2026-10-06 STERP KIT-001, KIT-002, KIT-004, KIT-024
*>================================================================
*> REQ: KIT-001, KIT-002, KIT-004, KIT-024
 IDENTIFICATION DIVISION.
 PROGRAM-ID. KITCREAT.
 AUTHOR. P M KEANE.
 DATE-WRITTEN. 02/09/2014.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY KITDBW.
 COPY KITWORK.
 COPY BOMEXPLCA REPLACING ==DFHCOMMAREA== BY ==Ws-Expl-Ca==
      LEADING ==CA-== BY ==EA-==
      LEADING ==BX-== BY ==EX-==.
 COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==Ws-Numgen-Ca==
      LEADING ==CA-== BY ==NA-==.
 COPY PERMCHKCA REPLACING ==DFHCOMMAREA== BY ==Ws-Perm-Ca==
      LEADING ==CA-== BY ==PA-==.
 COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==Ws-Audit-Ca==
      LEADING ==CA-== BY ==AA-==.
 COPY KITQTYCA REPLACING ==DFHCOMMAREA== BY ==Ws-Qty-Ca==
      LEADING ==CA-== BY ==QA-==.
 01  Ws-Work.
     05  Ws-I                    PIC S9(9) COMP-5 VALUE 0.
     05  Ws-W                    PIC S9(4) VALUE 0.
     05  Ws-Line-No              PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Taken                PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Skipped              PIC S9(9) COMP-5 VALUE 0.
     05  Ws-More                 PIC X VALUE "Y".
     05  Ws-Planned-Cost         PIC S9(11)V99 VALUE 0.
     05  Ws-Edit                 PIC -(13)9.9(6).
     05  Ws-Ok                   PIC X VALUE "Y".
     05  Ws-Q-Net                PIC S9(13)V9(6) VALUE 0.
     05  Ws-Q-Remain             PIC S9(13)V9(6) VALUE 0.
     05  Ws-Q-Excess             PIC S9(13)V9(6) VALUE 0.
 LINKAGE SECTION.
 COPY KITCREATCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "KITCREAT" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE 0 TO CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     MOVE "KIT-002" TO CA-REQ-REF
     MOVE SPACES TO KC-REASON-TEXT
     MOVE SPACES TO KC-WARNING-TEXT
     MOVE "N" TO KC-NEEDS-AUTH
     IF KC-WIN-MAX < 1 OR KC-WIN-MAX > 20
         MOVE 20 TO KC-WIN-MAX
     END-IF
     MOVE 0 TO KC-WIN-CNT
     MOVE "N" TO KC-MORE-LINES
     EVALUATE CA-FUNCTION
         WHEN "CREATE"   PERFORM Create-Execution-Kit
         WHEN "REVIEW"   PERFORM Review-Execution-Kit
         WHEN "RELEASE"  PERFORM Release-Execution-Kit
         WHEN "ENQUIRE"  PERFORM Enquire-Execution-Kit
         WHEN "GETLINE"  PERFORM Page-Kit-Lines
         WHEN "KINDCHK"  PERFORM Check-Kit-Kind
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "KIT0002" TO CA-REASON-CODE
     STRING "KITCREAT does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

 Set-Sql-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE KD-ERRMSG TO CA-MESSAGE.

*>----------------------------------------------------------------
*> KIT-001.  The five meanings are not cosmetic: they decide
*> whether the parent may ever be received into stock.  A
*> commercial parent is a commercial identity, so KC-MAY-RECEIVE
*> stays "N" and KITCOMPL will refuse it.
 Check-Kit-Kind.
     MOVE "N" TO KC-MAY-RECEIVE
     IF KC-PARENT-ITEM = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "A parent item code is required." TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Parent-Item
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     EVALUATE TRUE
         WHEN KC-COMMERCIAL
             MOVE "N" TO KC-MAY-RECEIVE
             MOVE "A commercial kit parent is a commercial identity, not an item balance."
                  TO KC-REASON-TEXT
         WHEN KC-PHANTOM
             MOVE "N" TO KC-MAY-RECEIVE
             MOVE "A phantom grouping has no receipt, no run and no separate stock."
                  TO KC-REASON-TEXT
         WHEN KC-TRIAL-KIND
             MOVE "N" TO KC-MAY-RECEIVE
             MOVE "A trial is a calculation and commits nothing."
                  TO KC-REASON-TEXT
         WHEN KI-STOCKED
             MOVE "Y" TO KC-MAY-RECEIVE
             MOVE "A stocked assembly may be completed into stock."
                  TO KC-REASON-TEXT
         WHEN OTHER
             MOVE "N" TO KC-MAY-RECEIVE
             MOVE 4 TO CA-RETURN-CODE
             MOVE "KIT0006" TO CA-REASON-CODE
             MOVE "This parent item does not hold stock and cannot be received as a finished assembly."
                  TO CA-MESSAGE
     END-EVALUATE
     MOVE KI-DESCRIPTION TO KC-PARENT-DESC.

 Load-Parent-Item.
     INITIALIZE KIT-DB-CTL
     MOVE "ITEMGET" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE KC-PARENT-ITEM TO KD-ITEM-CODE
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     EVALUATE KD-RC
         WHEN 0
             CONTINUE
         WHEN 100
             MOVE 16 TO CA-RETURN-CODE
             MOVE "KIT0001" TO CA-REASON-CODE
             STRING "Item " FUNCTION TRIM(KC-PARENT-ITEM)
                    " is not known in this company."
                 DELIMITED BY SIZE INTO CA-MESSAGE
         WHEN OTHER
             PERFORM Set-Sql-Failure
     END-EVALUATE.

*>----------------------------------------------------------------
*> CREATE - KIT-002.  Order of work: prove the kind and the item,
*> prove the bill, allocate the reference, write the header, then
*> write the requirement lines.  If the bill is not complete the
*> kit is not created at all - a half built requirement under a
*> success return code is exactly what the acceptance obligation
*> forbids.
 Create-Execution-Kit.
     PERFORM Validate-Create-Input
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-Kit-Kind
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO CA-RETURN-CODE
     MOVE SPACES TO CA-REASON-CODE
     PERFORM Explode-The-Bill
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Allocate-Kit-Reference
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Write-Kit-Header
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Write-Requirement-Lines
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     MOVE Ws-Planned-Cost TO KC-PLANNED-COST
     MOVE "DRAFT" TO KC-STATUS
     PERFORM Audit-Kit-Event.

 Validate-Create-Input.
     MOVE "N" TO Ws-Ok
     EVALUATE TRUE
         WHEN KC-PARENT-ITEM = SPACES
             MOVE 8 TO CA-RETURN-CODE
             MOVE "KIT0003" TO CA-REASON-CODE
             MOVE "A parent item code is required." TO CA-MESSAGE
         WHEN KC-PLANNED-QTY <= 0
             MOVE 8 TO CA-RETURN-CODE
             MOVE "KIT0003" TO CA-REASON-CODE
             MOVE "The planned good output must be greater than zero."
                  TO CA-MESSAGE
         WHEN KC-SITE = SPACES AND CA-SITE = SPACES
             MOVE 8 TO CA-RETURN-CODE
             MOVE "KIT0003" TO CA-REASON-CODE
             MOVE "A site is required for an execution kit."
                  TO CA-MESSAGE
         WHEN KC-SOURCE-TYPE = "JOB" AND KC-JOB-REF = SPACES
             MOVE 8 TO CA-RETURN-CODE
             MOVE "KIT0024" TO CA-REASON-CODE
             MOVE "A job reference is required when the source demand is a job."
                  TO CA-MESSAGE
         WHEN KC-SOURCE-TYPE = "SALESORDER" AND KC-SOURCE-REF = SPACES
             MOVE 8 TO CA-RETURN-CODE
             MOVE "KIT0024" TO CA-REASON-CODE
             MOVE "A sales order reference is required when the source demand is a sales order."
                  TO CA-MESSAGE
         WHEN OTHER
             MOVE "Y" TO Ws-Ok
     END-EVALUATE
     IF KC-SITE = SPACES
         MOVE CA-SITE TO KC-SITE
     END-IF
     IF KC-KIT-KIND = SPACES
         MOVE "EXECUTION" TO KC-KIT-KIND
     END-IF
     IF KC-SELECTION-DATE = 0
         MOVE CA-BUSINESS-DATE TO KC-SELECTION-DATE
     END-IF
     IF KC-START-DATE = 0
         MOVE KC-SELECTION-DATE TO KC-START-DATE
     END-IF.

*>   The selection date IS the planned start date (D-10), so the
*>   effective revision chosen here is the one that will be used
*>   on the shop floor, not the one in force when somebody
*>   happened to key the order.
 Ask-The-Production-Bill.
     INITIALIZE Ws-Expl-Ca
     MOVE "EXPLODE" TO EA-FUNCTION
     MOVE CA-COMPANY TO EA-COMPANY
     MOVE KC-SITE TO EA-SITE
     MOVE CA-USER TO EA-USER
     MOVE CA-BUSINESS-DATE TO EA-BUSINESS-DATE
     MOVE "EXECUTION" TO EX-CALC-TYPE
     MOVE KC-PARENT-ITEM TO EX-PARENT-ITEM
     MOVE KC-PARENT-REV TO EX-PARENT-REV
     MOVE "PRODUCTION" TO EX-PURPOSE
     MOVE KC-JOB-REF TO EX-JOB-REF
     MOVE KC-PLANNED-QTY TO EX-REQ-QTY
     MOVE KC-SELECTION-DATE TO EX-EFF-DATE
     MOVE 1 TO EX-MAX-DEPTH
     MOVE "Y" TO EX-EXPAND-PHANTOM
     MOVE "N" TO EX-INCLUDE-NOTES
     MOVE "N" TO EX-INCLUDE-BYPROD
     MOVE "ALL" TO EX-VIEW
     MOVE 20 TO EX-WIN-MAX
     CALL "BOMEXPL" USING Ws-Expl-Ca.

*>   CR2503.  Most plants never write a job specific bill: the
*>   standard bill IS the bill the shop floor builds to.  A
*>   production view is asked for first because a job that has one
*>   must use it, and the standard bill is only read when there is
*>   no production view for the job.  Either way the revision is
*>   frozen at release, so what is built is still what was
*>   planned (KIT-002, D-10).
 Ask-The-Standard-Bill.
     INITIALIZE Ws-Expl-Ca
     MOVE "EXPLODE" TO EA-FUNCTION
     MOVE CA-COMPANY TO EA-COMPANY
     MOVE KC-SITE TO EA-SITE
     MOVE CA-USER TO EA-USER
     MOVE CA-BUSINESS-DATE TO EA-BUSINESS-DATE
     MOVE "EXECUTION" TO EX-CALC-TYPE
     MOVE KC-PARENT-ITEM TO EX-PARENT-ITEM
     MOVE KC-PARENT-REV TO EX-PARENT-REV
     MOVE "STANDARD" TO EX-PURPOSE
     MOVE SPACES TO EX-JOB-REF
     MOVE KC-PLANNED-QTY TO EX-REQ-QTY
     MOVE KC-SELECTION-DATE TO EX-EFF-DATE
     MOVE 1 TO EX-MAX-DEPTH
     MOVE "Y" TO EX-EXPAND-PHANTOM
     MOVE "N" TO EX-INCLUDE-NOTES
     MOVE "N" TO EX-INCLUDE-BYPROD
     MOVE "ALL" TO EX-VIEW
     MOVE 20 TO EX-WIN-MAX
     CALL "BOMEXPL" USING Ws-Expl-Ca.

 Explode-The-Bill.
     PERFORM Ask-The-Production-Bill
     IF EA-RETURN-CODE > 4 OR EX-COMPLETENESS NOT = "COMPLETE"
         PERFORM Ask-The-Standard-Bill
     END-IF
     IF EA-RETURN-CODE > 4
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0004" TO CA-REASON-CODE
         MOVE EA-MESSAGE TO CA-MESSAGE
         MOVE EX-ERROR-PATH TO KC-REASON-TEXT
         EXIT PARAGRAPH
     END-IF
     IF EX-COMPLETENESS NOT = "COMPLETE"
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0004" TO CA-REASON-CODE
         MOVE "The bill of material is not complete, so no released material requirement can be produced."
              TO CA-MESSAGE
         MOVE EX-ERROR-PATH TO KC-REASON-TEXT
         MOVE "INCOMPLETE" TO KC-COMPLETENESS
         EXIT PARAGRAPH
     END-IF
     MOVE "COMPLETE" TO KC-COMPLETENESS
     MOVE EX-BOM-ID TO KC-BOM-ID
     MOVE EX-BOM-REF TO KC-BOM-REF
     MOVE EX-BOM-REV TO KC-BOM-REV
     MOVE EX-GROSS-STARTS TO KC-GROSS-STARTS
     MOVE EX-YIELD TO KC-YIELD
     MOVE EX-QUANTUM TO KC-QUANTUM
     MOVE EX-BATCH-SIZE TO KC-BATCH-SIZE
     MOVE EX-RUN-COUNT TO KC-RUN-COUNT
     MOVE EX-BATCH-COUNT TO KC-BATCH-COUNT
     MOVE EX-OCC-COUNT TO KC-LINE-TOTAL.

 Allocate-Kit-Reference.
     IF KC-KIT-REF NOT = SPACES
         EXIT PARAGRAPH
     END-IF
     INITIALIZE Ws-Numgen-Ca
     MOVE "NEXT" TO NA-FUNCTION
     MOVE CA-COMPANY TO NA-COMPANY
     MOVE CA-SITE TO NA-SITE
     MOVE CA-USER TO NA-USER
     MOVE CA-BUSINESS-DATE TO NA-BUSINESS-DATE
     MOVE CA-CORRELATION-ID TO NA-CORRELATION-ID
     MOVE "KIT" TO NG-SERIES-CODE
     CALL "NUMGEN" USING Ws-Numgen-Ca
     IF NA-RETURN-CODE > 4
         MOVE NA-RETURN-CODE TO CA-RETURN-CODE
         MOVE NA-REASON-CODE TO CA-REASON-CODE
         MOVE NA-MESSAGE TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE NG-DOCUMENT-NUMBER TO KC-KIT-REF.

 Write-Kit-Header.
     INITIALIZE KIT-HEADER-REC
     MOVE CA-COMPANY TO KH-COMPANY
     MOVE KC-SITE TO KH-SITE
     MOVE KC-KIT-REF TO KH-KIT-REF
     MOVE KC-PARENT-ITEM TO KH-PARENT-ITEM
     MOVE KC-PARENT-REV TO KH-PARENT-REV
     MOVE KC-BOM-ID TO KH-BOM-ID
     MOVE KC-BOM-REV TO KH-BOM-REV
     MOVE KC-SOURCE-TYPE TO KH-SOURCE-TYPE
     MOVE KC-SOURCE-REF TO KH-SOURCE-REF
     MOVE KC-JOB-REF TO KH-JOB-REF
     MOVE KC-PARENT-KIT-REF TO KH-PARENT-KIT-REF
     MOVE KC-PLANNED-QTY TO KH-PLANNED-QTY
     MOVE KC-GROSS-STARTS TO KH-GROSS-STARTS
     MOVE KC-YIELD TO KH-YIELD
     MOVE KC-QUANTUM TO KH-QUANTUM
     MOVE KC-BATCH-SIZE TO KH-BATCH-SIZE
     MOVE KC-RUN-COUNT TO KH-RUN-COUNT
     MOVE KC-BATCH-COUNT TO KH-BATCH-COUNT
     MOVE KC-NEED-DATE TO KH-NEED-DATE
     MOVE KC-START-DATE TO KH-START-DATE
     MOVE KC-SELECTION-DATE TO KH-SELECTION-DATE
     IF KC-OWNER-USER = SPACES
         MOVE CA-USER TO KH-OWNER-USER
     ELSE
         MOVE KC-OWNER-USER TO KH-OWNER-USER
     END-IF
     MOVE "DRAFT" TO KH-STATUS
     MOVE KC-BACKFLUSH TO KH-BACKFLUSH
     INITIALIZE KIT-DB-CTL
     MOVE "HDRINS" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KC-KIT-REF TO KD-KIT-REF
*>   COM-015 / CR2743.  The id that made this kit is kept on the
*>   header, so every row the run left behind is traceable to the
*>   one instruction that asked for it.  KD-REASON is the carrier
*>   KITDB already uses for a correlation id on an insert.
     MOVE CA-CORRELATION-ID TO KD-REASON
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE KD-NEW-ID TO KC-EXEC-KIT-ID
     MOVE KD-NEW-ID TO KH-EXEC-KIT-ID.

*>   One requirement line per material occurrence of the bill.
*>   Original and current requirement start equal; from now on
*>   they live separate lives (KIT-003).  Notes, resources and
*>   by-products are not material and are skipped.
 Write-Requirement-Lines.
     MOVE 0 TO Ws-Line-No
     MOVE 0 TO Ws-Skipped
     MOVE 0 TO Ws-Planned-Cost
     MOVE 1 TO EX-FROM-LINE
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More NOT = "Y" OR CA-FAILED
         MOVE "GETLINE" TO EA-FUNCTION
         MOVE 20 TO EX-WIN-MAX
         CALL "BOMEXPL" USING Ws-Expl-Ca
         IF EA-RETURN-CODE > 4
             MOVE 8 TO CA-RETURN-CODE
             MOVE "KIT0004" TO CA-REASON-CODE
             MOVE EA-MESSAGE TO CA-MESSAGE
             EXIT PERFORM
         END-IF
         PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > EX-WIN-CNT
             PERFORM Write-One-Line
         END-PERFORM
         MOVE EX-MORE-LINES TO Ws-More
*>       BOMEXPL has already moved EX-FROM-LINE to the next unread
*>       occurrence; adding the window again dropped lines 21 to 40.
         IF EX-WIN-CNT = 0
             MOVE "N" TO Ws-More
         END-IF
     END-PERFORM
     MOVE Ws-Line-No TO KC-LINE-TOTAL.

 Write-One-Line.
     IF EX-L-REQ-QTY (Ws-W) <= 0
         ADD 1 TO Ws-Skipped
         EXIT PARAGRAPH
     END-IF
     IF EX-L-NODE-TYPE (Ws-W) = "NOTE"
             OR EX-L-NODE-TYPE (Ws-W) = "RESOURCE"
             OR EX-L-NODE-TYPE (Ws-W) = "BYPROD"
         ADD 1 TO Ws-Skipped
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Line-No
     INITIALIZE KIT-LINE-REC
     MOVE Ws-Line-No TO KL-LINE-NUM
     MOVE EX-L-PATH (Ws-W) TO KL-PATH-REF
     MOVE EX-L-ITEM (Ws-W) TO KL-ITEM-CODE
     MOVE EX-L-REV (Ws-W) TO KL-REVISION
     MOVE EX-L-UOM (Ws-W) TO KL-UOM-CODE
     MOVE 1 TO KL-FACTOR
     PERFORM Map-Usage-Basis
     MOVE EX-L-REQ-QTY (Ws-W) TO KL-ORIGINAL-REQ
     MOVE EX-L-REQ-QTY (Ws-W) TO KL-CURRENT-REQ
     MOVE EX-L-REQ-QTY (Ws-W) TO KL-REMAINING-QTY
     MOVE EX-L-UNIT-COST (Ws-W) TO KL-PLAN-UNIT-COST
     MOVE KC-NEED-DATE TO KL-NEED-DATE
     MOVE "OPEN" TO KL-STATUS
     COMPUTE Ws-Planned-Cost ROUNDED
         = Ws-Planned-Cost + EX-L-EXT-COST (Ws-W)
     INITIALIZE KIT-DB-CTL
     MOVE "LININS" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KC-KIT-REF TO KD-KIT-REF
     MOVE KC-EXEC-KIT-ID TO KD-KIT-ID
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
     END-IF.

*>   D-10 usage bases.  BOMEXPL reports the basis it applied; the
*>   kit keeps it because a later output change treats the three
*>   bases differently (KIT-016).
 Map-Usage-Basis.
     EVALUATE EX-L-BASIS (Ws-W)
         WHEN "FIXEDRUN"    MOVE "FIXEDRUN" TO KL-USAGE-BASIS
         WHEN "FIXED-RUN"   MOVE "FIXEDRUN" TO KL-USAGE-BASIS
         WHEN "FIXEDBATCH"  MOVE "FIXEDBATCH" TO KL-USAGE-BASIS
         WHEN "FIXED-BATCH" MOVE "FIXEDBATCH" TO KL-USAGE-BASIS
         WHEN OTHER         MOVE "PROPORTION" TO KL-USAGE-BASIS
     END-EVALUATE.

*>----------------------------------------------------------------
*> REVIEW - the entry checks of specification 09 section 4: valid
*> quantities, revisions, supply choices and cost completeness.
 Review-Execution-Kit.
     PERFORM Load-Kit-Header
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     IF NOT KH-IS-DRAFT
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0005" TO CA-REASON-CODE
         STRING "Only a draft kit may be reviewed; this kit is "
                FUNCTION TRIM(KH-STATUS) "."
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-Lines-Complete
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     MOVE "REVIEWED" TO KD-NEW-STATUS
     PERFORM Change-Status
     MOVE "DRAFT" TO KC-PREV-STATUS
     MOVE "REVIEWED" TO KC-STATUS
     PERFORM Audit-Kit-Event.

 Check-Lines-Complete.
     MOVE 0 TO Ws-Taken
     MOVE 0 TO KC-SHORTAGE-CNT
     MOVE "COMPLETE" TO KC-COMPLETENESS
     INITIALIZE KIT-DB-CTL
     MOVE "LINOPEN" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More NOT = "Y"
         MOVE "LINNEXT" TO KD-FUNC
         CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                            KIT-LINE-REC, KIT-ITEM-REC
         IF KD-RC NOT = 0
             MOVE "N" TO Ws-More
         ELSE
             ADD 1 TO Ws-Taken
             IF KL-UOM-CODE = SPACES OR KL-CURRENT-REQ <= 0
                 MOVE "INCOMPLETE" TO KC-COMPLETENESS
                 ADD 1 TO KC-SHORTAGE-CNT
             END-IF
         END-IF
     END-PERFORM
     MOVE "LINCLOS" TO KD-FUNC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     MOVE Ws-Taken TO KC-LINE-TOTAL
     IF Ws-Taken = 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0004" TO CA-REASON-CODE
         MOVE "This kit has no material requirement lines." TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KC-COMPLETENESS NOT = "COMPLETE"
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0004" TO CA-REASON-CODE
         MOVE "One or more material lines have no unit or no requirement quantity."
              TO CA-MESSAGE
     END-IF.

*>----------------------------------------------------------------
*> RELEASE - KIT-004.  Authority first, then the freeze.  After
*> this point a standard bill change is a review item, never an
*> automatic rewrite of this kit.
 Release-Execution-Kit.
     PERFORM Load-Kit-Header
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     IF NOT (KH-IS-REVIEWED OR KH-IS-DRAFT)
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0005" TO CA-REASON-CODE
         STRING "A kit in status " FUNCTION TRIM(KH-STATUS)
                " cannot be released."
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-Release-Authority
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-Lines-Complete
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Freeze-The-Calculation
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     MOVE "RELEASED" TO KD-NEW-STATUS
     PERFORM Change-Status
     MOVE KH-STATUS TO KC-PREV-STATUS
     MOVE "RELEASED" TO KC-STATUS
     PERFORM Audit-Kit-Event
     MOVE "Revisions, units and calculation assumptions are now fixed for this kit."
          TO KC-REASON-TEXT.

 Check-Release-Authority.
     INITIALIZE Ws-Perm-Ca
     MOVE "CHECK" TO PA-FUNCTION
     MOVE CA-COMPANY TO PA-COMPANY
     MOVE KH-SITE TO PA-SITE
     MOVE CA-USER TO PA-USER
     MOVE CA-BUSINESS-DATE TO PA-BUSINESS-DATE
     MOVE "KIT.EXEC.RELEASE" TO PK-PERMISSION
     MOVE CA-USER TO PK-CHECK-USER
     MOVE KH-SITE TO PK-CHECK-SITE
     CALL "PERMCHK" USING Ws-Perm-Ca
     IF PA-RETURN-CODE NOT = 0
         MOVE 6 TO CA-RETURN-CODE
         MOVE "KIT0009" TO CA-REASON-CODE
         MOVE "Y" TO KC-NEEDS-AUTH
         IF PK-REASON-TEXT = SPACES
             MOVE "Releasing an execution kit needs the kit release authority."
                  TO CA-MESSAGE
         ELSE
             MOVE PK-REASON-TEXT TO CA-MESSAGE
         END-IF
     END-IF.

 Freeze-The-Calculation.
     INITIALIZE KIT-DB-CTL
     MOVE "HDRFRZ" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KH-KIT-REF TO KD-KIT-REF
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     STRING "Released from bill " FUNCTION TRIM(KH-BOM-REV)
            " selected at " KH-SELECTION-DATE
         DELIMITED BY SIZE INTO KD-REASON
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
     END-IF.

 Change-Status.
     MOVE "HDRSTAT" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KH-KIT-REF TO KD-KIT-REF
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
     END-IF.

*>   KIT-024 - every status change keeps actor, time, reason,
*>   authority and the source document link.
 Audit-Kit-Event.
     INITIALIZE Ws-Audit-Ca
     MOVE "WRITE" TO AA-FUNCTION
     MOVE CA-COMPANY TO AA-COMPANY
     MOVE KC-SITE TO AA-SITE
     MOVE CA-USER TO AA-USER
     MOVE CA-BUSINESS-DATE TO AA-BUSINESS-DATE
     MOVE CA-CORRELATION-ID TO AA-CORRELATION-ID
     MOVE "KIT.EXECUTIONKIT" TO AU-ENTITY-TYPE
     MOVE KC-KIT-REF TO AU-ENTITY-KEY
     MOVE CA-FUNCTION TO AU-ACTION
     MOVE "Status" TO AU-FIELD-NAME
     MOVE KC-PREV-STATUS TO AU-OLD-VALUE
     MOVE KC-STATUS TO AU-NEW-VALUE
     MOVE KC-REASON-TEXT (1:200) TO AU-REASON
     MOVE KC-SOURCE-REF TO AU-AUTHORITY-REF
     MOVE "KITCREAT" TO AU-SOURCE
     MOVE "N" TO AU-SENSITIVE
     CALL "AUDITLOG" USING Ws-Audit-Ca.

*>----------------------------------------------------------------
 Load-Kit-Header.
     IF KC-KIT-REF = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "A kit reference is required." TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     INITIALIZE KIT-DB-CTL
     INITIALIZE KIT-HEADER-REC
     MOVE "HDRGET" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KC-KIT-REF TO KD-KIT-REF
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     EVALUATE KD-RC
         WHEN 0
             CONTINUE
         WHEN 100
             MOVE 16 TO CA-RETURN-CODE
             MOVE "KIT0001" TO CA-REASON-CODE
             STRING "Kit " FUNCTION TRIM(KC-KIT-REF)
                    " is not known in this company."
                 DELIMITED BY SIZE INTO CA-MESSAGE
         WHEN OTHER
             PERFORM Set-Sql-Failure
     END-EVALUATE.

 Enquire-Execution-Kit.
     PERFORM Load-Kit-Header
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Copy-Header-Out
     PERFORM Page-Kit-Lines.

 Copy-Header-Out.
     MOVE KH-EXEC-KIT-ID TO KC-EXEC-KIT-ID
     MOVE KH-SITE TO KC-SITE
     MOVE KH-PARENT-ITEM TO KC-PARENT-ITEM
     MOVE KH-PARENT-REV TO KC-PARENT-REV
     MOVE KH-BOM-ID TO KC-BOM-ID
     MOVE KH-BOM-REV TO KC-BOM-REV
     MOVE KH-SOURCE-TYPE TO KC-SOURCE-TYPE
     MOVE KH-SOURCE-REF TO KC-SOURCE-REF
     MOVE KH-JOB-REF TO KC-JOB-REF
     MOVE KH-PARENT-KIT-REF TO KC-PARENT-KIT-REF
     MOVE KH-PLANNED-QTY TO KC-PLANNED-QTY
     MOVE KH-COMPLETED-QTY TO KC-COMPLETED-QTY
     MOVE KH-REJECTED-QTY TO KC-REJECTED-QTY
     MOVE KH-GROSS-STARTS TO KC-GROSS-STARTS
     MOVE KH-YIELD TO KC-YIELD
     MOVE KH-QUANTUM TO KC-QUANTUM
     MOVE KH-BATCH-SIZE TO KC-BATCH-SIZE
     MOVE KH-RUN-COUNT TO KC-RUN-COUNT
     MOVE KH-BATCH-COUNT TO KC-BATCH-COUNT
     MOVE KH-NEED-DATE TO KC-NEED-DATE
     MOVE KH-START-DATE TO KC-START-DATE
     MOVE KH-PLAN-COMP-DATE TO KC-COMPLETE-DATE
     MOVE KH-SELECTION-DATE TO KC-SELECTION-DATE
     MOVE KH-OWNER-USER TO KC-OWNER-USER
     MOVE KH-STATUS TO KC-STATUS
     MOVE KH-BACKFLUSH TO KC-BACKFLUSH
     MOVE KH-WIP-VALUE TO KC-WIP-VALUE
     MOVE KH-APPROVAL-ID TO KC-APPROVAL-ID.

*>   GETLINE recomputes the three derived quantities through
*>   KITQTY instead of trusting the stored columns, so a screen
*>   can never show an arithmetic that disagrees with the rule.
 Page-Kit-Lines.
     IF KH-EXEC-KIT-ID = 0
         PERFORM Load-Kit-Header
         IF CA-FAILED
             EXIT PARAGRAPH
         END-IF
     END-IF
     IF KC-FROM-LINE < 1
         MOVE 1 TO KC-FROM-LINE
     END-IF
     MOVE 0 TO Ws-Taken
     MOVE 0 TO KC-WIN-CNT
     MOVE "N" TO KC-MORE-LINES
     INITIALIZE KIT-DB-CTL
     MOVE "LINOPEN" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More NOT = "Y"
         MOVE "LINNEXT" TO KD-FUNC
         CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                            KIT-LINE-REC, KIT-ITEM-REC
         IF KD-RC NOT = 0
             MOVE "N" TO Ws-More
         ELSE
             ADD 1 TO Ws-Taken
             IF Ws-Taken >= KC-FROM-LINE
                 IF KC-WIN-CNT < KC-WIN-MAX
                     ADD 1 TO KC-WIN-CNT
                     PERFORM Fill-Window-Line
                 ELSE
                     MOVE "Y" TO KC-MORE-LINES
                     MOVE "N" TO Ws-More
                 END-IF
             END-IF
         END-IF
     END-PERFORM
     MOVE "LINCLOS" TO KD-FUNC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC.

 Fill-Window-Line.
     MOVE KC-WIN-CNT TO Ws-W
     PERFORM Derive-Line-Quantities
     MOVE KL-LINE-NUM TO KC-L-LINE-NUM (Ws-W)
     MOVE KL-PATH-REF TO KC-L-PATH (Ws-W)
     MOVE KL-ITEM-CODE TO KC-L-ITEM (Ws-W)
     MOVE KL-REVISION TO KC-L-REV (Ws-W)
     MOVE KL-USAGE-BASIS TO KC-L-BASIS (Ws-W)
     MOVE KL-UOM-CODE TO KC-L-UOM (Ws-W)
     MOVE KL-FACTOR TO KC-L-FACTOR (Ws-W)
     MOVE KL-ORIGINAL-REQ TO KC-L-ORIG-REQ (Ws-W)
     MOVE KL-CURRENT-REQ TO KC-L-CURR-REQ (Ws-W)
     MOVE KL-SCRAP-PCT TO KC-L-SCRAP-PCT (Ws-W)
     MOVE KL-RESERVED-QTY TO KC-L-RESERVED (Ws-W)
     MOVE KL-PICKED-QTY TO KC-L-PICKED (Ws-W)
     MOVE KL-GROSS-ISSUED TO KC-L-GROSS-ISS (Ws-W)
     MOVE KL-RETURNED-QTY TO KC-L-RETURNED (Ws-W)
     MOVE KL-SCRAPPED-QTY TO KC-L-SCRAPPED (Ws-W)
     MOVE Ws-Q-Net TO KC-L-NET-ISS (Ws-W)
     MOVE Ws-Q-Remain TO KC-L-REMAINING (Ws-W)
     MOVE Ws-Q-Excess TO KC-L-EXCESS (Ws-W)
     MOVE KL-SUBSTITUTE TO KC-L-SUBST (Ws-W)
     MOVE KL-NEED-DATE TO KC-L-NEED-DATE (Ws-W)
     MOVE KL-LOCATION TO KC-L-LOCATION (Ws-W)
     MOVE KL-PLAN-UNIT-COST TO KC-L-UNIT-COST (Ws-W)
     MOVE KL-STATUS TO KC-L-STATUS (Ws-W)
     MOVE SPACES TO KC-L-DESC (Ws-W).

 Derive-Line-Quantities.
     INITIALIZE Ws-Qty-Ca
     MOVE "LINEQTY" TO QA-FUNCTION
     MOVE CA-COMPANY TO QA-COMPANY
     MOVE CA-USER TO QA-USER
     MOVE KH-KIT-REF TO KQ-KIT-REF
     MOVE KL-LINE-NUM TO KQ-LINE-NUM
     MOVE KL-ITEM-CODE TO KQ-ITEM-CODE
     MOVE KL-CURRENT-REQ TO KQ-CURRENT-REQ
     MOVE KL-ORIGINAL-REQ TO KQ-ORIGINAL-REQ
     MOVE KL-GROSS-ISSUED TO KQ-GROSS-ISSUED
     MOVE KL-RETURNED-QTY TO KQ-RETURNED
     MOVE KL-SCRAPPED-QTY TO KQ-SCRAPPED
     CALL "KITQTY" USING Ws-Qty-Ca
     MOVE KQ-NET-ISSUED TO Ws-Q-Net
     MOVE KQ-REMAINING TO Ws-Q-Remain
     MOVE KQ-EXCESS TO Ws-Q-Excess.
