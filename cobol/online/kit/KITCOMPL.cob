>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : KITCOMPL
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : Complete an execution kit - record actual good
*>              output, capture genealogy and move value out of
*>              work in progress (KIT-015, KIT-016, KIT-019).
*>
*>              Three rules are the whole program:
*>                * only a parent that may be received into stock
*>                  is ever completed.  A commercial kit, a
*>                  phantom or a trial has no parent balance, so
*>                  completing one would invent stock (KIT-015);
*>                * a partial completion keeps the run open and
*>                  does NOT repeat fixed-per-run material or
*>                  setup - six then four units of a ten unit run
*>                  consume the fixed material once (KIT-016);
*>                * the value transferred out of WIP is the value
*>                  that is in WIP, spread over the output still
*>                  to come, so that the last completion leaves
*>                  nothing stranded.
*> CALLED BY  : frmKitExecution, KITBATCH
*> INTERFACE  : CALL "KITCOMPL" USING WS-KITCOMPL-CA
*>              COPYBOOK KITCOMPLCA
*> CALLS      : KITDB, KITCREAT (KINDCHK), KITISSUE (BACKFLSH),
*>              STKMOVE, PERIODCK, EXCPRAIS, SQLIO
*>              -> fin.JournalStage + fin.usp_PostJournal
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2015-03-23 PMK  CR2305 COMPLETION AND WIP TRANSFER
*> 2015-11-09 PMK  CR2401 PARTIAL COMPLETION KEEPS ONE RUN
*> 2016-06-21 PMK  CR2490 GENEALOGY WRITTEN AT COMPLETION
*> 2026-10-06 STERP KIT-015, KIT-016, KIT-019
*>================================================================
*> REQ: KIT-015, KIT-016, KIT-019
 IDENTIFICATION DIVISION.
 PROGRAM-ID. KITCOMPL.
 AUTHOR. P M KEANE.
 DATE-WRITTEN. 23/03/2015.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY KITDBW.
 COPY KITWORK.
 COPY KITCREATCA REPLACING ==DFHCOMMAREA== BY ==Ws-Creat-Ca==
      LEADING ==CA-== BY ==CC-==.
 COPY KITISSUECA REPLACING ==DFHCOMMAREA== BY ==Ws-Issue-Ca==
      LEADING ==CA-== BY ==IA-==.
 COPY STKMOVECA REPLACING ==DFHCOMMAREA== BY ==Ws-Move-Ca==
      LEADING ==CA-== BY ==MA-==.
 COPY PERIODCKCA REPLACING ==DFHCOMMAREA== BY ==Ws-Period-Ca==
      LEADING ==CA-== BY ==PA-==.
 COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==Ws-Exc-Ca==
      LEADING ==CA-== BY ==XA-==.
 01  Ws-Gen-Row.
     05  Ws-G-Issue-Id           PIC X(20).
     05  Ws-G-Item               PIC X(40).
     05  Ws-G-Planned            PIC X(40).
     05  Ws-G-Lot                PIC X(30).
     05  Ws-G-Serial             PIC X(40).
     05  Ws-G-Qty                PIC X(24).
     05  Ws-G-Taken              PIC X(24).
     05  Ws-G-Line-Tot           PIC X(24).
     05  Ws-G-Line               PIC X(12).
 01  Ws-Id-Row                   PIC X(20).
 01  Ws-Work.
     05  Ws-More                 PIC X VALUE "Y".
     05  Ws-J                    PIC S9(4) VALUE 0.
     05  Ws-Total-Good           PIC S9(13)V9(6) VALUE 0.
     05  Ws-Remaining-Out        PIC S9(13)V9(6) VALUE 0.
     05  Ws-Share                PIC S9(9)V9(9) VALUE 0.
     05  Ws-Gen-Qty              PIC S9(13)V9(6) VALUE 0.
     05  Ws-Available            PIC S9(13)V9(6) VALUE 0.
     05  Ws-Line-Total           PIC S9(13)V9(6) VALUE 0.
     05  Ws-Line-Target          PIC S9(13)V9(6) VALUE 0.
     05  Ws-Line-Taken           PIC S9(13)V9(6) VALUE 0.
     05  Ws-Line-Num             PIC S9(9) VALUE 0.
     05  Ws-Last-Line            PIC S9(9) VALUE -1.
     05  Ws-Unit-Cost            PIC S9(11)V9(6) VALUE 0.
     05  Ws-Transfer             PIC S9(11)V99 VALUE 0.
*>   COM-015.  The backflush has already spent the caller's id on
*>   the material it issued (KITISSUE derives one id per line from
*>   it), so the parent receipt needs a derived id of its own or
*>   inv.usp_PostStockMovement hands back a component movement and
*>   no parent is ever received (CR2741).
     05  Ws-Corr-Stem            PIC X(30) VALUE SPACES.
     05  Ws-Recv-Corr            PIC X(36) VALUE SPACES.
     05  Ws-Is-Last              PIC X VALUE "N".
     05  Ws-Jnl-Ref              PIC X(40) VALUE SPACES.
     05  Ws-Post-Corr            PIC X(64) VALUE SPACES.
     05  Ws-Edit                 PIC -(13)9.9(6).
     05  Ws-Money                PIC -(11)9.99.
 LINKAGE SECTION.
 COPY KITCOMPLCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "KITCOMPL" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE 0 TO CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     MOVE "KIT-015" TO CA-REQ-REF
     MOVE 0 TO KM-COMPLETION-ID
     MOVE 0 TO KM-COMPLETION-SEQ
     MOVE 0 TO KM-MOVEMENT-ID
     MOVE 0 TO KM-WIP-TRANSFER
     MOVE 0 TO KM-GENEALOGY-CNT
     MOVE 0 TO KM-BACKFLUSH-CNT
     MOVE 0 TO KM-SHORTAGE-CNT
     MOVE SPACES TO KM-FAILED-ITEM
     MOVE SPACES TO KM-IMPACT-TEXT
     MOVE "N" TO KM-FIXED-APPLIED
     IF KM-COMPLETION-DATE = 0
         MOVE CA-BUSINESS-DATE TO KM-COMPLETION-DATE
     END-IF
     IF KM-POSTING-DATE = 0
         MOVE KM-COMPLETION-DATE TO KM-POSTING-DATE
     END-IF
     IF KM-COST-BASIS = SPACES
         MOVE "ACT" TO KM-COST-BASIS
     END-IF
     EVALUATE CA-FUNCTION
         WHEN "COMPCHK"  PERFORM Check-The-Completion
         WHEN "COMPLETE" PERFORM Do-The-Completion
         WHEN "FINAL"    PERFORM Do-Final-Completion
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "KIT0002" TO CA-REASON-CODE
     STRING "KITCOMPL does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

 Set-Sql-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE KD-ERRMSG TO CA-MESSAGE.

 Set-Sqlio-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE SQLIO-ERRMSG (1:200) TO CA-MESSAGE.

*>----------------------------------------------------------------
 Load-Kit.
     INITIALIZE KIT-DB-CTL
     INITIALIZE KIT-HEADER-REC
     MOVE "HDRGET" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KM-KIT-REF TO KD-KIT-REF
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     EVALUATE KD-RC
         WHEN 0
             CONTINUE
         WHEN 100
             MOVE 16 TO CA-RETURN-CODE
             MOVE "KIT0001" TO CA-REASON-CODE
             STRING "Kit " FUNCTION TRIM(KM-KIT-REF)
                    " is not known in this company."
                 DELIMITED BY SIZE INTO CA-MESSAGE
         WHEN OTHER
             PERFORM Set-Sql-Failure
     END-EVALUATE
     IF CA-RETURN-CODE NOT = 0
         EXIT PARAGRAPH
     END-IF
     MOVE KH-STATUS TO KM-KIT-STATUS
     IF KM-SITE = SPACES
         MOVE KH-SITE TO KM-SITE
     END-IF
     IF KM-PARENT-ITEM = SPACES
         MOVE KH-PARENT-ITEM TO KM-PARENT-ITEM
     END-IF
     IF KM-REVISION = SPACES
         MOVE KH-PARENT-REV TO KM-REVISION
     END-IF
*>   KIT-016.  The receipt being recorded is part of the total the
*>   caller is told about: a COMPCHK that answered only what the kit
*>   had completed BEFORE this run left the caller unable to tell
*>   whether the run would be finished by it (CR2743).  The COMPLETE
*>   path recomputes this from the updated header.  KM-REMAINING-OUT
*>   is deliberately left as the output the kit may STILL receive,
*>   which is the balance before this receipt is posted.
     COMPUTE KM-TOTAL-GOOD = KH-COMPLETED-QTY + KM-GOOD-QTY
     COMPUTE Ws-Remaining-Out = KH-PLANNED-QTY - KH-COMPLETED-QTY
                              - KH-REJECTED-QTY
     IF Ws-Remaining-Out < 0
         MOVE 0 TO Ws-Remaining-Out
     END-IF
     MOVE Ws-Remaining-Out TO KM-REMAINING-OUT
     IF NOT KH-MAY-ISSUE AND NOT KH-IS-PARTCOMP
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0005" TO CA-REASON-CODE
         STRING "A kit in status " FUNCTION TRIM(KH-STATUS)
                " cannot be completed."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF.

*>----------------------------------------------------------------
*> COMPCHK - everything that can refuse, decided first.
 Check-The-Completion.
     PERFORM Load-Kit
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     IF KM-GOOD-QTY <= 0 AND KM-REJECTED-QTY <= 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "A completion must record some good or rejected output."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-Parent-May-Be-Received
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-Output-Within-Plan
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-Identity
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-The-Period
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Value-The-Output
     PERFORM Note-Fixed-Usage
     MOVE KM-WIP-TRANSFER TO Ws-Money
     STRING "Ready to complete; " FUNCTION TRIM(Ws-Money)
            " would move out of work in progress."
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>   KIT-015 - the parent must be an item that may hold stock,
*>   and KITCREAT KINDCHK is the single place that knows which
*>   kit kinds may ever be received.
 Check-Parent-May-Be-Received.
     INITIALIZE Ws-Creat-Ca
     MOVE "KINDCHK" TO CC-FUNCTION
     MOVE CA-COMPANY TO CC-COMPANY
     MOVE KM-SITE TO CC-SITE
     MOVE CA-USER TO CC-USER
     MOVE CA-BUSINESS-DATE TO CC-BUSINESS-DATE
     MOVE KM-KIT-REF TO KC-KIT-REF
     MOVE KM-PARENT-ITEM TO KC-PARENT-ITEM
     MOVE KM-SITE TO KC-SITE
     IF KH-SOURCE-TYPE = "SALESORDER"
         MOVE "EXECUTION" TO KC-KIT-KIND
     ELSE
         MOVE "STOCKASSY" TO KC-KIT-KIND
     END-IF
     CALL "KITCREAT" USING Ws-Creat-Ca
     IF KC-MAY-RECEIVE NOT = "Y"
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0006" TO CA-REASON-CODE
         IF KC-REASON-TEXT = SPACES
             MOVE "This parent may not be received into stock, so the kit cannot be completed."
                  TO CA-MESSAGE
         ELSE
             MOVE KC-REASON-TEXT (1:200) TO CA-MESSAGE
         END-IF
     END-IF.

*>   KIT-016 - two partial receipts must not between them claim
*>   more than the run was planned to produce.
 Check-Output-Within-Plan.
     COMPUTE Ws-Total-Good = KH-COMPLETED-QTY + KM-GOOD-QTY
     IF Ws-Total-Good > KH-PLANNED-QTY
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0018" TO CA-REASON-CODE
         COMPUTE Ws-Edit = KH-PLANNED-QTY - KH-COMPLETED-QTY
         STRING "Only " FUNCTION TRIM(Ws-Edit)
                " more unit(s) were planned on this run; amend the kit to complete more."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF.

*>   A traced parent must carry an identity at the moment it
*>   becomes stock, or the genealogy written below is attached to
*>   nothing (KIT-019).
 Check-Identity.
     MOVE "ITEMGET" TO KD-FUNC
     MOVE KM-PARENT-ITEM TO KD-ITEM-CODE
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0 AND KD-RC NOT = 100
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     IF KM-UOM-CODE = SPACES
         MOVE KI-STOCK-UOM TO KM-UOM-CODE
     END-IF
     IF KI-LOT-TRACED AND KM-LOT-CODE = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0012" TO CA-REASON-CODE
         MOVE "This parent is lot controlled; a lot code is required to complete it."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KI-SERIAL-TRACED AND KM-SERIAL-NO = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0012" TO CA-REASON-CODE
         MOVE "This parent is serial controlled; a serial number is required to complete it."
              TO CA-MESSAGE
     END-IF.

 Check-The-Period.
     INITIALIZE Ws-Period-Ca
     MOVE "CHECK" TO PA-FUNCTION
     MOVE CA-COMPANY TO PA-COMPANY
     MOVE KM-SITE TO PA-SITE
     MOVE CA-USER TO PA-USER
     MOVE CA-BUSINESS-DATE TO PA-BUSINESS-DATE
     MOVE KM-POSTING-DATE TO PC-POSTING-DATE
     MOVE KM-COMPLETION-DATE TO PC-DOCUMENT-DATE
     MOVE "POST" TO PC-ACTION
     CALL "PERIODCK" USING Ws-Period-Ca
     IF PC-ALLOWED NOT = "Y"
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0016" TO CA-REASON-CODE
         IF PC-REASON-TEXT = SPACES
             MOVE "The accounting period for the posting date is closed."
                  TO CA-MESSAGE
         ELSE
             MOVE PC-REASON-TEXT TO CA-MESSAGE
         END-IF
     END-IF.

*>   The value that leaves WIP.  On the last completion the whole
*>   residual goes, so a rounding remainder can never be stranded
*>   in work in progress for ever.
 Value-The-Output.
     MOVE "N" TO Ws-Is-Last
     IF KM-IS-FINAL OR Ws-Total-Good >= KH-PLANNED-QTY
         MOVE "Y" TO Ws-Is-Last
     END-IF
     IF Ws-Remaining-Out <= 0
         MOVE KM-GOOD-QTY TO Ws-Remaining-Out
     END-IF
     IF Ws-Is-Last = "Y"
         MOVE KH-WIP-VALUE TO Ws-Transfer
     ELSE
         COMPUTE Ws-Transfer ROUNDED
             = KH-WIP-VALUE * KM-GOOD-QTY / Ws-Remaining-Out
     END-IF
     IF Ws-Transfer < 0
         MOVE 0 TO Ws-Transfer
     END-IF
     IF KM-GOOD-QTY > 0
         COMPUTE Ws-Unit-Cost ROUNDED = Ws-Transfer / KM-GOOD-QTY
     ELSE
         MOVE 0 TO Ws-Unit-Cost
     END-IF
     MOVE Ws-Unit-Cost TO KM-UNIT-COST
     MOVE Ws-Transfer TO KM-WIP-TRANSFER
     COMPUTE KM-WIP-RESIDUAL = KH-WIP-VALUE - Ws-Transfer.

*>   KIT-016 in one sentence: the fixed material belongs to the
*>   run, and the run happens once however many times output is
*>   received.
 Note-Fixed-Usage.
     IF KH-COMPLETED-QTY > 0
         MOVE "Y" TO KM-FIXED-APPLIED
         MOVE "Fixed-per-run material and setup were charged on the first completion and are not repeated."
              TO KM-IMPACT-TEXT
     ELSE
         MOVE "N" TO KM-FIXED-APPLIED
     END-IF.

*>----------------------------------------------------------------
 Do-Final-Completion.
     MOVE "Y" TO KM-FINAL
     PERFORM Do-The-Completion.

 Do-The-Completion.
     PERFORM Check-The-Completion
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     IF KM-CHECK-ONLY = "Y"
         EXIT PARAGRAPH
     END-IF
*>   Backflush before the receipt: KIT-010 forbids a partial
*>   success, so if the material cannot be issued the parent is
*>   never received either.
     IF KM-DO-BACKFLUSH OR KH-BACKFLUSH-ON
         PERFORM Backflush-The-Material
         IF CA-RETURN-CODE > 4
             EXIT PARAGRAPH
         END-IF
     END-IF
     PERFORM Receive-The-Parent
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Insert-Completion-Row
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Write-The-Genealogy
     PERFORM Update-Kit-Header
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Post-Wip-Transfer
     PERFORM Describe-The-Completion.

 Backflush-The-Material.
     INITIALIZE Ws-Issue-Ca
     MOVE "BACKFLSH" TO IA-FUNCTION
     MOVE CA-COMPANY TO IA-COMPANY
     MOVE KM-SITE TO IA-SITE
     MOVE CA-USER TO IA-USER
     MOVE CA-BUSINESS-DATE TO IA-BUSINESS-DATE
     MOVE CA-CORRELATION-ID TO IA-CORRELATION-ID
     MOVE KM-KIT-REF TO KS-KIT-REF
     MOVE KM-SITE TO KS-SITE
     MOVE KM-GOOD-QTY TO KS-GOOD-QTY
     MOVE KM-COMPLETION-DATE TO KS-ACTUAL-DATE
     MOVE KM-POSTING-DATE TO KS-POSTING-DATE
     CALL "KITISSUE" USING Ws-Issue-Ca
     MOVE KS-LINES-DONE TO KM-BACKFLUSH-CNT
     IF IA-RETURN-CODE > 4
         MOVE IA-RETURN-CODE TO CA-RETURN-CODE
         MOVE IA-REASON-CODE TO CA-REASON-CODE
         MOVE IA-MESSAGE TO CA-MESSAGE
         MOVE KS-FAILED-ITEM TO KM-FAILED-ITEM
         MOVE KS-LINES-FAILED TO KM-SHORTAGE-CNT
         MOVE "The completion was refused; no parent was received and no success is claimed."
              TO KM-IMPACT-TEXT
     END-IF.

 Receive-The-Parent.
     IF KM-GOOD-QTY <= 0
         EXIT PARAGRAPH
     END-IF
     INITIALIZE Ws-Move-Ca
     MOVE "RECEIPT" TO MA-FUNCTION
     MOVE CA-COMPANY TO MA-COMPANY
     MOVE KM-SITE TO MA-SITE
     MOVE CA-USER TO MA-USER
     MOVE CA-BUSINESS-DATE TO MA-BUSINESS-DATE
     MOVE CA-CORRELATION-ID (1:30) TO Ws-Corr-Stem
     MOVE SPACES TO Ws-Recv-Corr
     STRING FUNCTION TRIM(Ws-Corr-Stem) "-RCV"
         DELIMITED BY SIZE INTO Ws-Recv-Corr
     MOVE Ws-Recv-Corr TO MA-CORRELATION-ID
     MOVE KM-PARENT-ITEM TO SM-ITEM-CODE
     MOVE KM-REVISION TO SM-REVISION
     MOVE KM-SITE TO SM-SITE
     MOVE KM-WAREHOUSE TO SM-WAREHOUSE
     MOVE KM-LOCATION TO SM-LOCATION
     MOVE KM-LOT-CODE TO SM-LOT-CODE
     MOVE KM-SERIAL-NO TO SM-SERIAL-NO
     MOVE KM-GOOD-QTY TO SM-QUANTITY
     MOVE KM-UOM-CODE TO SM-UOM-CODE
     MOVE Ws-Unit-Cost TO SM-UNIT-COST
     MOVE KM-COMPLETION-DATE TO SM-MOVEMENT-DATE
     MOVE KM-POSTING-DATE TO SM-POSTING-DATE
     MOVE "KIT" TO SM-SOURCE-MODULE
     MOVE "EXECUTIONKIT" TO SM-SOURCE-DOC-TYPE
     MOVE KM-KIT-REF TO SM-SOURCE-DOC-REF
     MOVE "KITCOMPLETE" TO SM-REASON-CODE
     MOVE KM-REASON-TEXT (1:200) TO SM-REASON
     CALL "STKMOVE" USING Ws-Move-Ca
     IF MA-RETURN-CODE > 4
         MOVE MA-RETURN-CODE TO CA-RETURN-CODE
         MOVE MA-REASON-CODE TO CA-REASON-CODE
         MOVE MA-MESSAGE TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE SM-MOVEMENT-ID TO KM-MOVEMENT-ID.

 Insert-Completion-Row.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.KitCompletion (CompanyCode, KitRef, "
            "CompletionSeq, ParentItemCode, Revision, GoodQty, "
            "RejectedQty, ScrappedOutputQty, LotCode, SerialNo, "
            "WarehouseCode, LocationCode, CompletionDate, "
            "PostingDate, CostBasis, UnitCostGBP, WipTransferGBP, "
            "IsFinalCompletion, ApprovedBy, MovementId, "
            "CorrelationId, CreatedBy) OUTPUT INSERTED.CompletionId "
            "SELECT ?, ?, ISNULL(MAX(c.CompletionSeq),0)+1, ?, "
            "NULLIF(?,''), ?, ?, ?, NULLIF(?,''), NULLIF(?,''), "
            "NULLIF(?,''), NULLIF(?,''), CONVERT(date, ?, 112), "
            "CONVERT(date, ?, 112), NULLIF(?,''), ?, ?, ?, "
            "NULLIF(?,''), NULLIF(CONVERT(bigint, ?),0), "
            "NULLIF(?,''), ? FROM kit.KitCompletion c WHERE "
            "c.CompanyCode = ? AND c.KitRef = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE KM-KIT-REF TO SQLIO-PARM-VAL (2)
     MOVE KM-PARENT-ITEM TO SQLIO-PARM-VAL (3)
     MOVE KM-REVISION TO SQLIO-PARM-VAL (4)
     MOVE KM-GOOD-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (5)
     MOVE KM-REJECTED-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (6)
     MOVE KM-SCRAP-OUT-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (7)
     MOVE KM-LOT-CODE TO SQLIO-PARM-VAL (8)
     MOVE KM-SERIAL-NO TO SQLIO-PARM-VAL (9)
     MOVE KM-WAREHOUSE TO SQLIO-PARM-VAL (10)
     MOVE KM-LOCATION TO SQLIO-PARM-VAL (11)
     MOVE KM-COMPLETION-DATE TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (12)
     MOVE KM-POSTING-DATE TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (13)
     MOVE KM-COST-BASIS TO SQLIO-PARM-VAL (14)
     MOVE Ws-Unit-Cost TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (15)
     MOVE Ws-Transfer TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (16)
     IF Ws-Is-Last = "Y"
         MOVE "1" TO SQLIO-PARM-VAL (17)
     ELSE
         MOVE "0" TO SQLIO-PARM-VAL (17)
     END-IF
     MOVE KM-APPROVED-BY TO SQLIO-PARM-VAL (18)
     MOVE KM-MOVEMENT-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (19)
     MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL (20)
     MOVE CA-USER TO SQLIO-PARM-VAL (21)
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (22)
     MOVE KM-KIT-REF TO SQLIO-PARM-VAL (23)
     MOVE 23 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-J FROM 1 BY 1 UNTIL Ws-J > 23
         MOVE "N" TO SQLIO-PARM-NULL (Ws-J)
     END-PERFORM
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 20 TO SQLIO-COL-WIDTH (1)
     MOVE 7 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Id-Row
     IF SQLIO-ERROR
         PERFORM Set-Sqlio-Failure
         EXIT PARAGRAPH
     END-IF
     COMPUTE KM-COMPLETION-ID = FUNCTION NUMVAL (Ws-Id-Row).

*>----------------------------------------------------------------
*> KIT-019 - what actually went in, not what was planned.  The
*> issues are read back and attributed to this completion in
*> proportion to the output it represents, which is the only
*> honest attribution when a run is received in two parts and the
*> components were issued in one.
 Write-The-Genealogy.
     IF KM-GOOD-QTY <= 0 OR KH-PLANNED-QTY <= 0
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ws-Share = KM-GOOD-QTY / KH-PLANNED-QTY
     MOVE -1 TO Ws-Last-Line
     MOVE 0 TO Ws-Line-Total
     MOVE 0 TO Ws-Line-Target
     MOVE 0 TO Ws-Line-Taken
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(20), i.KitIssueId), "
            "CONVERT(varchar(40), i.ComponentItemCode), "
            "CONVERT(varchar(40), ISNULL(l.ComponentItemCode, "
            "i.ComponentItemCode)), "
            "CONVERT(varchar(30), ISNULL(NULLIF(i.LotCode,''), "
            "ISNULL(p.LotCode,''))), "
            "CONVERT(varchar(40), ISNULL(NULLIF(i.SerialNo,''), "
            "ISNULL(p.SerialNo,''))), "
            "CONVERT(varchar(24), i.StockQty), "
            "CONVERT(varchar(24), ISNULL(g.Taken,0)), "
            "CONVERT(varchar(24), SUM(i.StockQty) OVER "
            "(PARTITION BY ISNULL(i.LineNum,0))), "
            "CONVERT(varchar(12), ISNULL(i.LineNum,0)) "
            "FROM kit.KitIssue i "
            "LEFT JOIN kit.ExecutionKitLine l ON "
            "l.KitLineId = i.KitLineId "
            "OUTER APPLY (SELECT TOP (1) k.LotCode, k.SerialNo "
            "FROM kit.KitPick k WHERE k.CompanyCode = "
            "i.CompanyCode AND k.KitRef = i.KitRef AND "
            "k.LineNum = i.LineNum AND k.PickType <> 'UNPICK' AND "
            "(k.LotCode IS NOT NULL OR k.SerialNo IS NOT NULL) "
            "ORDER BY k.KitPickId) p "
            "LEFT JOIN (SELECT KitIssueId, SUM(ConsumedQty) AS "
            "Taken FROM kit.KitGenealogy WHERE CompanyCode = ? "
            "AND KitRef = ? GROUP BY KitIssueId) g ON "
            "g.KitIssueId = i.KitIssueId "
            "WHERE i.CompanyCode = ? "
            "AND i.KitRef = ? AND i.IssueType IN "
            "('ISSUE','BACKFLUSH') "
            "ORDER BY ISNULL(i.LineNum,0), i.KitIssueId"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE KM-KIT-REF TO SQLIO-PARM-VAL (2)
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (3)
     MOVE KM-KIT-REF TO SQLIO-PARM-VAL (4)
     MOVE "N" TO SQLIO-PARM-NULL (1)
     MOVE "N" TO SQLIO-PARM-NULL (2)
     MOVE "N" TO SQLIO-PARM-NULL (3)
     MOVE "N" TO SQLIO-PARM-NULL (4)
     MOVE 4 TO SQLIO-PARM-CNT
     MOVE 9 TO SQLIO-COL-CNT
     MOVE 20 TO SQLIO-COL-WIDTH (1)
     MOVE 40 TO SQLIO-COL-WIDTH (2)
     MOVE 40 TO SQLIO-COL-WIDTH (3)
     MOVE 30 TO SQLIO-COL-WIDTH (4)
     MOVE 40 TO SQLIO-COL-WIDTH (5)
     MOVE 24 TO SQLIO-COL-WIDTH (6)
     MOVE 24 TO SQLIO-COL-WIDTH (7)
     MOVE 24 TO SQLIO-COL-WIDTH (8)
     MOVE 12 TO SQLIO-COL-WIDTH (9)
     MOVE 8 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Gen-Row
     IF SQLIO-ERROR
         PERFORM Set-Sqlio-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE 8 TO SQLIO-CURSOR
         MOVE "FTCH" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA, Ws-Gen-Row
         IF SQLIO-ERROR OR SQLIO-NOTFOUND
             MOVE "N" TO Ws-More
         ELSE
             PERFORM Write-One-Genealogy-Row
         END-IF
     END-PERFORM
     MOVE 8 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Gen-Row.

 Write-One-Genealogy-Row.
     COMPUTE Ws-Line-Num = FUNCTION NUMVAL (Ws-G-Line)
     IF Ws-Line-Num NOT = Ws-Last-Line
         MOVE Ws-Line-Num TO Ws-Last-Line
         COMPUTE Ws-Line-Total = FUNCTION NUMVAL (Ws-G-Line-Tot)
         COMPUTE Ws-Line-Target ROUNDED = Ws-Line-Total * Ws-Share
         MOVE 0 TO Ws-Line-Taken
     END-IF
     COMPUTE Ws-Available = FUNCTION NUMVAL (Ws-G-Qty)
                          - FUNCTION NUMVAL (Ws-G-Taken)
     IF Ws-Available <= 0
         EXIT PARAGRAPH
     END-IF
*>   AC-083 wants the lot that is really in the unit, so each
*>   completion draws from the issues in the order they were made
*>   and only takes what earlier completions left.  The first six
*>   frames therefore contain the first lot picked and the last
*>   four contain the second, instead of every unit containing an
*>   arithmetical smear of both.  Material that is not attached to
*>   a kit line cannot be sequenced and keeps the old proportional
*>   treatment; a final completion sweeps up whatever is left so
*>   that nothing issued stays unattributed.
     IF Ws-Line-Num = 0 OR Ws-Line-Total <= 0
         COMPUTE Ws-Gen-Qty ROUNDED
             = FUNCTION NUMVAL (Ws-G-Qty) * Ws-Share
     ELSE
         IF KM-IS-FINAL
             MOVE Ws-Available TO Ws-Gen-Qty
         ELSE
             COMPUTE Ws-Gen-Qty = Ws-Line-Target - Ws-Line-Taken
         END-IF
     END-IF
     IF Ws-Gen-Qty > Ws-Available
         MOVE Ws-Available TO Ws-Gen-Qty
     END-IF
     IF Ws-Gen-Qty <= 0
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ws-Line-Taken = Ws-Line-Taken + Ws-Gen-Qty
     MOVE "GENINS" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KM-KIT-REF TO KD-KIT-REF
     MOVE KM-COMPLETION-ID TO KD-G-COMPLETION
     MOVE KM-PARENT-ITEM TO KD-G-PARENT
     MOVE KM-LOT-CODE TO KD-G-PAR-LOT
     MOVE KM-SERIAL-NO TO KD-G-PAR-SERIAL
     MOVE Ws-G-Item TO KD-G-ITEM
     MOVE Ws-G-Lot TO KD-G-LOT
     MOVE Ws-G-Serial TO KD-G-SERIAL
     MOVE Ws-Gen-Qty TO KD-G-QTY
     MOVE Ws-G-Planned TO KD-G-PLANNED
     COMPUTE KD-G-ISSUE-ID = FUNCTION NUMVAL (Ws-G-Issue-Id)
*>   A substitution is disclosed in the genealogy, because the
*>   unit really does contain the substitute (KIT-012, KIT-019).
     IF Ws-G-Item = Ws-G-Planned
         MOVE "N" TO KD-G-SUBSTITUTE
     ELSE
         MOVE "Y" TO KD-G-SUBSTITUTE
     END-IF
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC = 0
         ADD 1 TO KM-GENEALOGY-CNT
     END-IF.

*>----------------------------------------------------------------
 Update-Kit-Header.
     COMPUTE KH-COMPLETED-QTY = KH-COMPLETED-QTY + KM-GOOD-QTY
     COMPUTE KH-REJECTED-QTY = KH-REJECTED-QTY + KM-REJECTED-QTY
                             + KM-SCRAP-OUT-QTY
     COMPUTE KH-WIP-VALUE = KH-WIP-VALUE - Ws-Transfer
     IF KH-WIP-VALUE < 0
         MOVE 0 TO KH-WIP-VALUE
     END-IF
     MOVE KM-COMPLETION-DATE TO KH-ACT-COMP-DATE
     MOVE "HDRQTY" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KH-KIT-REF TO KD-KIT-REF
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
*>   KH-COMPLETED-QTY already carries the receipt just recorded
*>   (Update-Kit-Header adds it above), so the total good is the
*>   whole run to date including this one (CR2743).
     MOVE KH-COMPLETED-QTY TO KM-TOTAL-GOOD
     COMPUTE KM-REMAINING-OUT = KH-PLANNED-QTY - KH-COMPLETED-QTY
                              - KH-REJECTED-QTY
     IF KM-REMAINING-OUT < 0
         MOVE 0 TO KM-REMAINING-OUT
     END-IF
     MOVE KH-WIP-VALUE TO KM-WIP-RESIDUAL
*>   PARTCOMPLETE keeps the run open: the remaining output, the
*>   remaining requirement and the remaining WIP are all still
*>   live (KIT-016).
     IF Ws-Is-Last = "Y" OR KM-REMAINING-OUT <= 0
         MOVE "OPCOMPLETE" TO KD-NEW-STATUS
     ELSE
         MOVE "PARTCOMPLETE" TO KD-NEW-STATUS
     END-IF
     MOVE "HDRSTAT" TO KD-FUNC
     MOVE KM-REASON-TEXT (1:200) TO KD-REASON
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE KD-NEW-STATUS TO KM-KIT-STATUS.

*>----------------------------------------------------------------
*> The nominal ledger entry: work in progress is credited and the
*> finished goods / stock account is debited by exactly what left
*> WIP.  fin.usp_PostJournal owns the account mapping through the
*> business event KIT.COMPLETE, so this program never decides
*> an account code for itself.
 Post-Wip-Transfer.
     IF Ws-Transfer = 0 OR KM-COST-BASIS = "NOGL"
         EXIT PARAGRAPH
     END-IF
     MOVE KM-COMPLETION-ID TO SQLIO-INT-EDIT
     PERFORM Build-Posting-Correlation
     MOVE SPACES TO Ws-Jnl-Ref
     STRING FUNCTION TRIM(KM-KIT-REF) "/"
            FUNCTION TRIM(SQLIO-INT-EDIT)
         DELIMITED BY SIZE INTO Ws-Jnl-Ref
     PERFORM Stage-Journal-Lines
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Call-Post-Journal.

*>   The ledger entry gets a correlation id of its own, built from
*>   the completion it belongs to.  It must NOT be the caller's:
*>   that one has already been spent on the stock receipt, and
*>   fin.usp_PostJournal would treat the posting as a repeat of
*>   work it had already done and return without posting anything
*>   at all (CR2514 - the completion was in stock and the ledger
*>   knew nothing about it).  Keyed on the completion id, a
*>   retried completion is still posted exactly once.
 Build-Posting-Correlation.
     MOVE SPACES TO Ws-Post-Corr
     STRING "KITCOMPL/" FUNCTION TRIM(KM-KIT-REF) "/"
            FUNCTION TRIM(SQLIO-INT-EDIT)
         DELIMITED BY SIZE INTO Ws-Post-Corr.

 Stage-Journal-Lines.
     MOVE SPACES TO SQLIO-STMT
*>   The accounts are the event map's business, never this
*>   program's: the two legs of KIT.COMPLETE are read straight out
*>   of fin.EventAccountMap, so moving the work in progress
*>   account is a seed change and not a recompile (CR2514).
     STRING "INSERT fin.JournalStage (CorrelationId, LineNum, "
            "CompanyCode, AccountCode, DebitAmount, CreditAmount, "
            "CurrencyCode, Narrative, SubledgerType, SubledgerRef, "
            "SourceRef, DimCostCentre, DimProject, CreatedBy) "
            "SELECT ?, ROW_NUMBER() OVER (ORDER BY m.SortOrder), "
            "m.CompanyCode, m.AccountCode, "
            "CASE WHEN m.DrCr = 'DR' THEN "
            "CONVERT(decimal(18,2), ?) ELSE 0 END, "
            "CASE WHEN m.DrCr = 'CR' THEN "
            "CONVERT(decimal(18,2), ?) ELSE 0 END, "
            "'GBP', CASE WHEN m.DrCr = 'DR' THEN "
            "'Kit completion into stock' ELSE "
            "'Work in progress relieved' END, 'KIT', ?, ?, "
            "m.DimCostCentre, m.DimProject, ? "
            "FROM fin.EventAccountMap m WHERE m.CompanyCode = ? "
            "AND m.EventCode = 'KIT.COMPLETE' AND m.IsActive = 1"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE Ws-Post-Corr TO SQLIO-PARM-VAL (1)
     MOVE Ws-Transfer TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (2)
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (3)
     MOVE KM-KIT-REF TO SQLIO-PARM-VAL (4)
     MOVE Ws-Jnl-Ref TO SQLIO-PARM-VAL (5)
     MOVE CA-USER TO SQLIO-PARM-VAL (6)
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (7)
     PERFORM VARYING Ws-J FROM 1 BY 1 UNTIL Ws-J > 7
         MOVE "N" TO SQLIO-PARM-NULL (Ws-J)
     END-PERFORM
     MOVE 7 TO SQLIO-PARM-CNT
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 7 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Id-Row
     IF SQLIO-ERROR
         PERFORM Set-Sqlio-Failure
     END-IF
     MOVE 7 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Id-Row.

 Call-Post-Journal.
     MOVE SPACES TO SQLIO-STMT
*>   The dates go across as plain YYYYMMDD parameters: a procedure
*>   argument may not be an expression, so CAST(? AS date) is a
*>   syntax error at the server (CR2514).
*>
*>   @SuppressResult = 1 and a read-back of our own: the posting
*>   procedure's own result set does not survive the SQLIO call
*>   interface, and a posting that quietly does nothing is worse
*>   than one that fails, so the journal is looked up by the
*>   source reference and a zero is treated as a failure.
     STRING "EXEC fin.usp_PostJournal ?, ?, ?, ?, "
            "?, 'KIT', ?, 'GBP', "
            "'KIT', ?, 'KIT.COMPLETE', 'JNL', NULL, NULL, 1;"
            " SELECT CONVERT(varchar(20), ISNULL((SELECT TOP (1)"
            " j.JournalId FROM fin.Journal j WHERE"
            " j.CompanyCode = ? AND j.SourceRef = ?"
            " ORDER BY j.JournalId DESC), 0))"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE Ws-Post-Corr TO SQLIO-PARM-VAL (2)
     MOVE CA-USER TO SQLIO-PARM-VAL (3)
     MOVE SPACES TO SQLIO-PARM-VAL (4)
     STRING "Kit completion " FUNCTION TRIM(KM-KIT-REF)
         DELIMITED BY SIZE INTO SQLIO-PARM-VAL (4)
     MOVE KM-POSTING-DATE TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (5)
     MOVE KM-COMPLETION-DATE TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (6)
     MOVE Ws-Jnl-Ref TO SQLIO-PARM-VAL (7)
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (8)
     MOVE Ws-Jnl-Ref TO SQLIO-PARM-VAL (9)
     MOVE 9 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-J FROM 1 BY 1 UNTIL Ws-J > 9
         MOVE "N" TO SQLIO-PARM-NULL (Ws-J)
     END-PERFORM
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 20 TO SQLIO-COL-WIDTH (1)
*>   A cursor of its own: the staging loop above has just used
*>   and closed cursor 7, and the posting call must not inherit
*>   whatever state that left behind.
     MOVE 4 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     MOVE SPACES TO Ws-Id-Row
     CALL "SQLIO" USING SQLIO-CA, Ws-Id-Row
     IF SQLIO-ERROR
*>       The receipt and the completion are real and stay real.
*>       The ledger entry has to be repaired by somebody, so an
*>       exception is raised rather than a silent warning.
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0016" TO CA-REASON-CODE
         MOVE SQLIO-ERRMSG (1:200) TO CA-MESSAGE
         PERFORM Raise-Posting-Exception
         EXIT PARAGRAPH
     END-IF
     IF FUNCTION NUMVAL (Ws-Id-Row) = 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0016" TO CA-REASON-CODE
         MOVE SQLIO-SQLCODE TO SQLIO-INT-EDIT
         STRING "The work in progress transfer for "
                FUNCTION TRIM(Ws-Jnl-Ref)
                " was not posted to the nominal ledger (sqlcode "
                FUNCTION TRIM(SQLIO-INT-EDIT) ", id ["
                FUNCTION TRIM(Ws-Id-Row) "])."
             DELIMITED BY SIZE INTO CA-MESSAGE
         PERFORM Raise-Posting-Exception
     END-IF.

 Raise-Posting-Exception.
     INITIALIZE Ws-Exc-Ca
     MOVE "RAISE" TO XA-FUNCTION
     MOVE CA-COMPANY TO XA-COMPANY
     MOVE KM-SITE TO XA-SITE
     MOVE CA-USER TO XA-USER
     MOVE CA-BUSINESS-DATE TO XA-BUSINESS-DATE
     MOVE "KIT" TO EX-MODULE
     MOVE "ERROR" TO EX-SEVERITY
     MOVE "KIT WIP TRANSFER NOT POSTED" TO EX-CATEGORY
     MOVE CA-MESSAGE (1:200) TO EX-MESSAGE-TEXT
     MOVE "EXECUTIONKIT" TO EX-DOCUMENT-TYPE
     MOVE KM-KIT-REF TO EX-DOCUMENT-REF
     MOVE KH-OWNER-USER TO EX-OWNER-USER
     MOVE 1 TO EX-PRIORITY
     MOVE Ws-Transfer TO EX-AMOUNT-GBP
     CALL "EXCPRAIS" USING Ws-Exc-Ca.

 Describe-The-Completion.
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     MOVE Ws-Transfer TO Ws-Money
     MOVE KM-TOTAL-GOOD TO Ws-Edit
     IF KM-KIT-STATUS = "OPCOMPLETE"
         STRING "Completed; " FUNCTION TRIM(Ws-Edit)
                " good units in total and "
                FUNCTION TRIM(Ws-Money)
                " transferred out of work in progress."
             DELIMITED BY SIZE INTO CA-MESSAGE
     ELSE
         MOVE 4 TO CA-RETURN-CODE
*>       CR2743.  A partial completion is not an over-completion:
*>       KIT0018 (RC 8, refused) used to answer for both, so a
*>       caller could not tell a refusal from "the run is still
*>       open".  Partial completion now has its own code.
         MOVE "KIT0029" TO CA-REASON-CODE
         STRING "Partly completed; " FUNCTION TRIM(Ws-Edit)
                " good so far. The run stays open for the balance."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF.
