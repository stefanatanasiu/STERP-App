>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : KITISSUE
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : Issue material to an execution kit - the moment
*>              stock becomes work in progress (KIT-010).
*>              Everything is validated in COBOL first and the
*>              stock effect is created exactly ONCE through
*>              STKMOVE, so a refusal leaves nothing behind.
*>
*>              Three conditional rules are enforced here:
*>                D-20   backflush is allowed only for components
*>                       that are neither lot nor serial
*>                       controlled, and only on a kit explicitly
*>                       flagged for it;
*>                KIT-011 overissue needs a visible reason and
*>                       approval, and that approval raises the
*>                       PERMITTED quantity - it never creates
*>                       physical stock, so an issue that would
*>                       drive a balance negative is still refused;
*>                KIT-012 an approved substitute is issued against
*>                       the planned line and disclosed, without
*>                       touching the standard bill.
*> CALLED BY  : frmKitExecution, KITCOMPL (backflush), KITBATCH
*> INTERFACE  : CALL "KITISSUE" USING WS-KITISSUE-CA
*>              COPYBOOK KITISSUECA
*> CALLS      : KITDB, KITQTY, STKMOVE, STKRESV, AVAILQ, EXCPRAIS
*>----------------------------------------------------------------
*> CHANGE LOG
*> 1994-06-21 DRN  CR0130 ISSUE TO WORKS ORDER
*> 2014-12-08 PMK  CR2271 KIT ISSUE THROUGH STKMOVE
*> 2015-09-30 PMK  CR2388 BACKFLUSH RESTRICTED TO UNTRACED (D-20)
*> 2016-05-30 PMK  CR2488 OVERISSUE NEEDS AUTHORITY AND A REASON
*> 2026-10-06 STERP KIT-010, KIT-011, KIT-012
*> 2026-10-07 STERP CR2744 ISSUES VALUED AT THE ITEM'S SITE COST
*>================================================================
*> REQ: KIT-010, KIT-011, KIT-012
 IDENTIFICATION DIVISION.
 PROGRAM-ID. KITISSUE.
 AUTHOR. P M KEANE.
 DATE-WRITTEN. 08/12/2014.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY KITDBW.
 COPY KITWORK.
 COPY KITQTYCA REPLACING ==DFHCOMMAREA== BY ==Ws-Qty-Ca==
      LEADING ==CA-== BY ==QA-==.
 COPY STKMOVECA REPLACING ==DFHCOMMAREA== BY ==Ws-Move-Ca==
      LEADING ==CA-== BY ==MA-==.
 COPY AVAILQCA REPLACING ==DFHCOMMAREA== BY ==Ws-Avail-Ca==
      LEADING ==CA-== BY ==VA-==.
 COPY STKRESVCA REPLACING ==DFHCOMMAREA== BY ==Ws-Resv-Ca==
      LEADING ==CA-== BY ==RA-==.
 COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==Ws-Exc-Ca==
      LEADING ==CA-== BY ==XA-==.
 01  Ws-Work.
     05  Ws-More                 PIC X VALUE "Y".
     05  Ws-Issue-Item           PIC X(40) VALUE SPACES.
     05  Ws-Permitted            PIC S9(13)V9(6) VALUE 0.
     05  Ws-Would-Be-Net         PIC S9(13)V9(6) VALUE 0.
     05  Ws-Over-Qty             PIC S9(13)V9(6) VALUE 0.
     05  Ws-Backflush-Qty        PIC S9(13)V9(6) VALUE 0.
     05  Ws-Usage-Per            PIC S9(13)V9(6) VALUE 0.
     05  Ws-Done                 PIC S9(4) VALUE 0.
     05  Ws-Failed               PIC S9(4) VALUE 0.
     05  Ws-Edit                 PIC -(13)9.9(6).
*>   COM-015.  core.IdempotencyKey is keyed on the correlation id
*>   alone, so one backflush pass issuing several components must
*>   give each line a derived id.  The caller's id is cut to 30
*>   characters first because the commarea field is only 36 and a
*>   guid would swallow the suffix (CR2741).
     05  Ws-Corr-Stem            PIC X(30) VALUE SPACES.
     05  Ws-Sub-Corr             PIC X(36) VALUE SPACES.
     05  Ws-Sub-Suffix           PIC X(6) VALUE SPACES.
     05  Ws-Sub-Seq              PIC 9(4) VALUE 0.
*>   CR2742.  A backflush covers every open line of the kit, and the
*>   components of one kit do not live in one bin.  The scope the
*>   caller asked for is kept here and used as the SEARCH, not as the
*>   answer: each line is then sourced from its own eligible balance.
     05  Ws-Req-Site             PIC X(10) VALUE SPACES.
     05  Ws-Req-Warehouse        PIC X(20) VALUE SPACES.
     05  Ws-Req-Location         PIC X(20) VALUE SPACES.
     05  Ws-Bucket-Ix            PIC S9(4) VALUE 0.
     05  Ws-Best-Ix              PIC S9(4) VALUE 0.
     05  Ws-Best-Free            PIC S9(13)V9(6) VALUE 0.
     05  Ws-Outstanding          PIC S9(13)V9(6) VALUE 0.
*>   CR2742.  The open lines of the kit, listed before anything is
*>   posted.  Issuing a line changes its status and takes it out of
*>   the "only open" cursor the pass was reading, and the fetch that
*>   followed ended the loop with the rest of the kit unflushed.
     05  Ws-Line-Cnt             PIC S9(4) VALUE 0.
     05  Ws-Lx                   PIC S9(4) VALUE 0.
 01  Ws-Open-Lines.
     05  Ws-Open-Line OCCURS 300 TIMES PIC S9(9) COMP-5.
 LINKAGE SECTION.
 COPY KITISSUECA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "KITISSUE" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE 0 TO CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     MOVE "KIT-010" TO CA-REQ-REF
     MOVE SPACES TO KS-IMPACT-TEXT
     MOVE SPACES TO KS-FAILED-ITEM
     MOVE 0 TO KS-LINES-DONE
     MOVE 0 TO KS-LINES-FAILED
     MOVE 0 TO KS-KIT-ISSUE-ID
     MOVE 0 TO KS-MOVEMENT-ID
     IF KS-ACTUAL-DATE = 0
         MOVE CA-BUSINESS-DATE TO KS-ACTUAL-DATE
     END-IF
     IF KS-POSTING-DATE = 0
         MOVE KS-ACTUAL-DATE TO KS-POSTING-DATE
     END-IF
     EVALUATE CA-FUNCTION
         WHEN "ISSUE"    PERFORM Issue-One-Component
         WHEN "SUBST"    PERFORM Issue-One-Component
         WHEN "CHECK"    PERFORM Check-Only-Issue
         WHEN "BACKFLSH" PERFORM Backflush-The-Kit
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "KIT0002" TO CA-REASON-CODE
     STRING "KITISSUE does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

 Set-Sql-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE KD-ERRMSG TO CA-MESSAGE.

 Check-Only-Issue.
     MOVE "Y" TO KS-CHECK-ONLY
     PERFORM Issue-One-Component.

*>----------------------------------------------------------------
 Load-Kit.
     INITIALIZE KIT-DB-CTL
     INITIALIZE KIT-HEADER-REC
     MOVE "HDRGET" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KS-KIT-REF TO KD-KIT-REF
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     EVALUATE KD-RC
         WHEN 0
             MOVE KH-STATUS TO KS-KIT-STATUS
             MOVE KH-WIP-VALUE TO KS-WIP-VALUE
         WHEN 100
             MOVE 16 TO CA-RETURN-CODE
             MOVE "KIT0001" TO CA-REASON-CODE
             STRING "Kit " FUNCTION TRIM(KS-KIT-REF)
                    " is not known in this company."
                 DELIMITED BY SIZE INTO CA-MESSAGE
         WHEN OTHER
             PERFORM Set-Sql-Failure
     END-EVALUATE
     IF CA-RETURN-CODE = 0 AND NOT KH-MAY-ISSUE
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0005" TO CA-REASON-CODE
         STRING "Material cannot be issued to a kit in status "
                FUNCTION TRIM(KH-STATUS) "."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF
     IF KS-SITE = SPACES
         MOVE KH-SITE TO KS-SITE
     END-IF.

 Load-Line.
     MOVE "LINGET" TO KD-FUNC
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     MOVE KS-LINE-NUM TO KD-LINE-NUM
     INITIALIZE KIT-LINE-REC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     EVALUATE KD-RC
         WHEN 0
             CONTINUE
         WHEN 100
             MOVE 16 TO CA-RETURN-CODE
             MOVE "KIT0007" TO CA-REASON-CODE
             MOVE "That material line is not on this kit." TO CA-MESSAGE
         WHEN OTHER
             PERFORM Set-Sql-Failure
     END-EVALUATE.

*>   The item ACTUALLY being issued decides the traceability
*>   rules - which matters when a substitute is traced and the
*>   planned component is not.
 Load-Issue-Item.
     MOVE "ITEMGET" TO KD-FUNC
     MOVE Ws-Issue-Item TO KD-ITEM-CODE
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC = 100
         MOVE 16 TO CA-RETURN-CODE
         MOVE "KIT0001" TO CA-REASON-CODE
         STRING "Item " FUNCTION TRIM(Ws-Issue-Item)
                " is not known in this company."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF
     IF KD-RC NOT = 0 AND KD-RC NOT = 100
         PERFORM Set-Sql-Failure
     END-IF
     MOVE KI-TRACED TO KS-TRACED.

*>----------------------------------------------------------------
*> ISSUE / SUBST - one component, one stock effect.
 Issue-One-Component.
     PERFORM Load-Kit
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Line
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Decide-Issue-Item
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Issue-Item
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Validate-Issue-Quantity
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Validate-Traceability
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-Permitted-Quantity
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-Free-Stock
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     IF KS-IS-CHECK
         MOVE "Issue would be accepted." TO KS-IMPACT-TEXT
         EXIT PARAGRAPH
     END-IF
     PERFORM Post-Stock-Issue
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Record-Kit-Issue
     PERFORM Update-Line-Quantities.

*>   KIT-012: a substitute is issued against the PLANNED line, so
*>   the plan and the actual stay visible side by side.
 Decide-Issue-Item.
     MOVE KL-ITEM-CODE TO KS-PLANNED-ITEM
     IF CA-FUNCTION = "SUBST" OR
            (KS-ITEM-CODE NOT = SPACES AND
             KS-ITEM-CODE NOT = KL-ITEM-CODE)
         MOVE KS-ITEM-CODE TO Ws-Issue-Item
         IF KL-SUBSTITUTE NOT = Ws-Issue-Item
             MOVE 6 TO CA-RETURN-CODE
             MOVE "KIT0014" TO CA-REASON-CODE
             STRING "Item " FUNCTION TRIM(Ws-Issue-Item)
                    " is not the approved substitute for this line."
                 DELIMITED BY SIZE INTO CA-MESSAGE
             EXIT PARAGRAPH
         END-IF
         IF KL-SUB-APPROVAL = 0 AND KS-SUB-APPROVAL = 0
             MOVE 6 TO CA-RETURN-CODE
             MOVE "KIT0014" TO CA-REASON-CODE
             MOVE "This substitution has no recorded approval." TO CA-MESSAGE
             EXIT PARAGRAPH
         END-IF
     ELSE
         MOVE KL-ITEM-CODE TO Ws-Issue-Item
     END-IF
     MOVE Ws-Issue-Item TO KS-ITEM-CODE.

 Validate-Issue-Quantity.
     IF KS-QUANTITY <= 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "An issue quantity must be greater than zero." TO CA-MESSAGE
     END-IF
     IF KS-UOM-CODE = SPACES
         MOVE KL-UOM-CODE TO KS-UOM-CODE
     END-IF.

*>   D-20 and KIT-010: traced material must carry its actual
*>   identity.  Nothing may be guessed.
 Validate-Traceability.
     IF NOT KI-IS-TRACED
         EXIT PARAGRAPH
     END-IF
     IF KI-LOT-TRACED AND KS-LOT-CODE = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0012" TO CA-REASON-CODE
         STRING "Item " FUNCTION TRIM(Ws-Issue-Item)
                " is lot controlled: an actual lot code is required."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF
     IF KI-SERIAL-TRACED AND KS-SERIAL-NO = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0012" TO CA-REASON-CODE
         STRING "Item " FUNCTION TRIM(Ws-Issue-Item)
                " is serial controlled: an actual serial number is required."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF.

*>----------------------------------------------------------------
*> KIT-011.  The permitted quantity is the remaining approved
*> requirement.  More than that is an overissue: it needs a
*> reason AND an approval, and even then it only raises what may
*> be issued - never what physically exists.
 Check-Permitted-Quantity.
     INITIALIZE Ws-Qty-Ca
     MOVE "LINEQTY" TO QA-FUNCTION
     MOVE CA-COMPANY TO QA-COMPANY
     MOVE CA-USER TO QA-USER
     MOVE KS-KIT-REF TO KQ-KIT-REF
     MOVE KL-LINE-NUM TO KQ-LINE-NUM
     MOVE KL-CURRENT-REQ TO KQ-CURRENT-REQ
     MOVE KL-ORIGINAL-REQ TO KQ-ORIGINAL-REQ
     MOVE KL-GROSS-ISSUED TO KQ-GROSS-ISSUED
     MOVE KL-RETURNED-QTY TO KQ-RETURNED
     MOVE KL-SCRAPPED-QTY TO KQ-SCRAPPED
     CALL "KITQTY" USING Ws-Qty-Ca
     MOVE KQ-REMAINING TO Ws-Permitted
     MOVE KQ-REMAINING TO KS-LINE-REMAINING
     COMPUTE Ws-Would-Be-Net = KQ-NET-ISSUED + KS-QUANTITY
     COMPUTE Ws-Over-Qty = Ws-Would-Be-Net - KL-CURRENT-REQ
     IF Ws-Over-Qty <= 0
         MOVE 0 TO Ws-Over-Qty
         EXIT PARAGRAPH
     END-IF
     IF KS-REASON-TEXT = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0008" TO CA-REASON-CODE
         MOVE Ws-Over-Qty TO Ws-Edit
         STRING "This issue exceeds the approved requirement by "
                FUNCTION TRIM(Ws-Edit)
                " and needs a visible reason."
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF NOT KS-OVER-OK OR KS-APPROVAL-ID = 0
         MOVE 6 TO CA-RETURN-CODE
         MOVE "KIT0009" TO CA-REASON-CODE
         MOVE Ws-Over-Qty TO Ws-Edit
         STRING "Overissue of " FUNCTION TRIM(Ws-Edit)
                " needs approval before the material can be issued."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF.

*>   An overissue approval is NOT a stock creation.  AVAILQ is
*>   asked what is really free and the issue is refused if the
*>   physical balance cannot carry it (KIT-011, ITM-015).
 Check-Free-Stock.
     INITIALIZE Ws-Avail-Ca
     MOVE "AVAIL" TO VA-FUNCTION
     MOVE CA-COMPANY TO VA-COMPANY
     MOVE KS-SITE TO VA-SITE
     MOVE CA-USER TO VA-USER
     MOVE CA-BUSINESS-DATE TO VA-BUSINESS-DATE
     MOVE Ws-Issue-Item TO AV-ITEM-CODE
     MOVE KS-SITE TO AV-SITE
     MOVE KS-WAREHOUSE TO AV-WAREHOUSE
     MOVE KS-LOCATION TO AV-LOCATION
     MOVE KS-LOT-CODE TO AV-LOT-CODE
     MOVE KS-SERIAL-NO TO AV-SERIAL-NO
     MOVE "KIT" TO AV-DEMAND-TYPE
     MOVE KS-KIT-REF TO AV-DEMAND-REF
     MOVE KL-LINE-NUM TO AV-DEMAND-LINE
     MOVE CA-BUSINESS-DATE TO AV-AS-AT
     CALL "AVAILQ" USING Ws-Avail-Ca
     IF VA-RETURN-CODE > 4
         MOVE VA-RETURN-CODE TO CA-RETURN-CODE
         MOVE VA-REASON-CODE TO CA-REASON-CODE
         MOVE VA-MESSAGE TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE AV-QTY-FOR-DEMAND TO KS-FREE-QTY
     IF AV-QTY-ELIGIBLE < KS-QUANTITY
         COMPUTE KS-SHORTAGE-QTY = KS-QUANTITY - AV-QTY-ELIGIBLE
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0010" TO CA-REASON-CODE
         MOVE KS-SHORTAGE-QTY TO Ws-Edit
         STRING "Insufficient eligible stock: short by "
                FUNCTION TRIM(Ws-Edit)
                ". An approval cannot create physical stock."
             DELIMITED BY SIZE INTO CA-MESSAGE
         PERFORM Raise-Shortage-Exception
     ELSE
         MOVE 0 TO KS-SHORTAGE-QTY
     END-IF.

 Raise-Shortage-Exception.
     IF KS-IS-CHECK
         EXIT PARAGRAPH
     END-IF
     INITIALIZE Ws-Exc-Ca
     MOVE "RAISE" TO XA-FUNCTION
     MOVE CA-COMPANY TO XA-COMPANY
     MOVE KS-SITE TO XA-SITE
     MOVE CA-USER TO XA-USER
     MOVE CA-BUSINESS-DATE TO XA-BUSINESS-DATE
     MOVE "KIT" TO EX-MODULE
     MOVE "ERROR" TO EX-SEVERITY
     MOVE "KIT MATERIAL SHORTAGE" TO EX-CATEGORY
     MOVE CA-MESSAGE (1:200) TO EX-MESSAGE-TEXT
     MOVE "EXECUTIONKIT" TO EX-DOCUMENT-TYPE
     MOVE KS-KIT-REF TO EX-DOCUMENT-REF
     MOVE KH-OWNER-USER TO EX-OWNER-USER
     MOVE 2 TO EX-PRIORITY
     MOVE KS-SHORTAGE-QTY TO EX-QUANTITY
     CALL "EXCPRAIS" USING Ws-Exc-Ca.

*>----------------------------------------------------------------
*>   A derived correlation id for this kit line.  CR2741: a
*>   backflush pass passed the caller's id to every component, so
*>   inv.usp_PostStockMovement treated the second and later lines
*>   as replays of the first and issued no material for them while
*>   the parent was still received.
 Derive-Line-Correlation.
     MOVE CA-CORRELATION-ID (1:30) TO Ws-Corr-Stem
     MOVE KL-LINE-NUM TO Ws-Sub-Seq
     MOVE SPACES TO Ws-Sub-Suffix
     STRING "-L" Ws-Sub-Seq DELIMITED BY SIZE INTO Ws-Sub-Suffix
     MOVE SPACES TO Ws-Sub-Corr
     STRING FUNCTION TRIM(Ws-Corr-Stem)
            FUNCTION TRIM(Ws-Sub-Suffix)
         DELIMITED BY SIZE INTO Ws-Sub-Corr.

 Post-Stock-Issue.
     INITIALIZE Ws-Move-Ca
     MOVE "ISSUE" TO MA-FUNCTION
     MOVE CA-COMPANY TO MA-COMPANY
     MOVE KS-SITE TO MA-SITE
     MOVE CA-USER TO MA-USER
     MOVE CA-BUSINESS-DATE TO MA-BUSINESS-DATE
     PERFORM Derive-Line-Correlation
     MOVE Ws-Sub-Corr TO MA-CORRELATION-ID
     MOVE Ws-Issue-Item TO SM-ITEM-CODE
     MOVE KS-REVISION TO SM-REVISION
     MOVE KS-SITE TO SM-SITE
     MOVE KS-WAREHOUSE TO SM-WAREHOUSE
     MOVE KS-LOCATION TO SM-LOCATION
     MOVE KS-LOT-CODE TO SM-LOT-CODE
     MOVE KS-SERIAL-NO TO SM-SERIAL-NO
     MOVE KS-QUANTITY TO SM-QUANTITY
     MOVE KS-UOM-CODE TO SM-UOM-CODE
     MOVE KS-ACTUAL-DATE TO SM-MOVEMENT-DATE
     MOVE KS-POSTING-DATE TO SM-POSTING-DATE
     MOVE "KIT" TO SM-SOURCE-MODULE
     MOVE "EXECUTIONKIT" TO SM-SOURCE-DOC-TYPE
     MOVE KS-KIT-REF TO SM-SOURCE-DOC-REF
     MOVE KL-LINE-NUM TO SM-SOURCE-DOC-LINE
     MOVE KS-REASON-CODE TO SM-REASON-CODE
     MOVE KS-REASON-TEXT (1:200) TO SM-REASON
     PERFORM Price-The-Issue
     CALL "STKMOVE" USING Ws-Move-Ca
     IF MA-RETURN-CODE > 4
         MOVE MA-RETURN-CODE TO CA-RETURN-CODE
         MOVE MA-REASON-CODE TO CA-REASON-CODE
         MOVE MA-MESSAGE TO CA-MESSAGE
         MOVE Ws-Issue-Item TO KS-FAILED-ITEM
         ADD 1 TO KS-LINES-FAILED
         EXIT PARAGRAPH
     END-IF
     MOVE SM-STOCK-QTY TO KS-STOCK-QTY
     MOVE SM-FACTOR TO KS-FACTOR
     MOVE SM-UNIT-COST TO KS-UNIT-COST
     MOVE SM-VALUE-GBP TO KS-VALUE-GBP
     MOVE SM-MOVEMENT-ID TO KS-MOVEMENT-ID
     MOVE SM-DUPLICATE TO KS-DUPLICATE.

*>   CR2744.  STKMOVE never derives a cost, so until this every kit
*>   issue was posted at nil: the movement had no value, the kit
*>   carried no work in progress and the completion receipt, which
*>   is valued from that WIP, came out at zero too.  The issue is
*>   now valued at the item's cost at the issuing site - standard
*>   for a standard-costed item, otherwise the moving average - the
*>   basis DESPATCH uses for a cost of sale.
 Price-The-Issue.
     MOVE "ITEMGET" TO KD-FUNC
     MOVE KS-SITE TO KD-SITE
     MOVE Ws-Issue-Item TO KD-ITEM-CODE
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         EXIT PARAGRAPH
     END-IF
     IF KI-STANDARD-COSTED AND KI-STD-COST NOT = 0
         MOVE KI-STD-COST TO SM-UNIT-COST
     ELSE
         IF KI-AVG-COST NOT = 0
             MOVE KI-AVG-COST TO SM-UNIT-COST
         ELSE
             MOVE KI-STD-COST TO SM-UNIT-COST
         END-IF
     END-IF.

 Record-Kit-Issue.
     MOVE "ISSINS" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KS-KIT-REF TO KD-KIT-REF
     MOVE CA-CORRELATION-ID TO KD-REASON
     IF CA-FUNCTION = "BACKFLSH"
         MOVE "BACKFLUSH" TO KD-I-TYPE
     ELSE
         MOVE "ISSUE" TO KD-I-TYPE
     END-IF
     MOVE Ws-Issue-Item TO KD-I-ITEM
     MOVE KS-REVISION TO KD-I-REV
     MOVE KS-QUANTITY TO KD-I-QTY
     MOVE KS-UOM-CODE TO KD-I-UOM
     MOVE KS-STOCK-QTY TO KD-I-STOCK-QTY
     MOVE KS-WAREHOUSE TO KD-I-WAREHOUSE
     MOVE KS-LOCATION TO KD-I-LOCATION
     MOVE KS-LOT-CODE TO KD-I-LOT
     MOVE KS-SERIAL-NO TO KD-I-SERIAL
     MOVE KS-ACTUAL-DATE TO KD-I-ACT-DATE
     MOVE KS-POSTING-DATE TO KD-I-POST-DATE
     MOVE KS-UNIT-COST TO KD-I-UNIT-COST
     MOVE KS-VALUE-GBP TO KD-I-VALUE
     MOVE KS-REASON-CODE TO KD-I-REASON-CODE
     MOVE KS-REASON-TEXT (1:200) TO KD-I-REASON
     MOVE KS-APPROVAL-ID TO KD-I-APPROVAL
     MOVE 0 TO KD-I-ORIG-ISSUE
     MOVE KS-MOVEMENT-ID TO KD-I-MOVEMENT
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
     ELSE
         MOVE KD-NEW-ID TO KS-KIT-ISSUE-ID
         PERFORM Consume-Open-Picks
     END-IF.

*>   KIT-009/KIT-010.  The picks of this line are what the warehouse
*>   already took off the shelf for the kit; issuing spends them,
*>   oldest first.  Anything still unconsumed when the kit is
*>   cancelled is material that is physically out of the store and
*>   has to be put back - KITCLOSE deliberately leaves it visible.
*>   A pick trail is optional, so nothing here is an error: an issue
*>   with no picks simply consumes nothing.
 Consume-Open-Picks.
     MOVE "PCKUSE" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KS-KIT-REF TO KD-KIT-REF
     MOVE KL-LINE-NUM TO KD-LINE-NUM
     MOVE KS-STOCK-QTY TO KD-P-QTY
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
     END-IF.

 Update-Line-Quantities.
     COMPUTE KL-GROSS-ISSUED = KL-GROSS-ISSUED + KS-STOCK-QTY
     COMPUTE KL-ISSUED-VALUE ROUNDED
         = KL-ISSUED-VALUE + KS-VALUE-GBP
     IF KL-RESERVED-QTY > 0
         PERFORM Consume-The-Reservation
     END-IF
     INITIALIZE Ws-Qty-Ca
     MOVE "LINEQTY" TO QA-FUNCTION
     MOVE CA-COMPANY TO QA-COMPANY
     MOVE KL-CURRENT-REQ TO KQ-CURRENT-REQ
     MOVE KL-ORIGINAL-REQ TO KQ-ORIGINAL-REQ
     MOVE KL-GROSS-ISSUED TO KQ-GROSS-ISSUED
     MOVE KL-RETURNED-QTY TO KQ-RETURNED
     MOVE KL-SCRAPPED-QTY TO KQ-SCRAPPED
     CALL "KITQTY" USING Ws-Qty-Ca
     MOVE KQ-NET-ISSUED TO KL-NET-ISSUED
     MOVE KQ-REMAINING TO KL-REMAINING-QTY
     MOVE KQ-EXCESS TO KL-EXCESS-QTY
     IF KL-REMAINING-QTY > 0
         MOVE "PARTISSUED" TO KL-STATUS
     ELSE
         MOVE "ISSUED" TO KL-STATUS
     END-IF
     MOVE "LINSAVE" TO KD-FUNC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE KL-GROSS-ISSUED TO KS-LINE-GROSS
     MOVE KL-NET-ISSUED TO KS-LINE-NET
     MOVE KL-REMAINING-QTY TO KS-LINE-REMAINING
     MOVE KL-EXCESS-QTY TO KS-LINE-EXCESS
     MOVE KL-STATUS TO KS-LINE-STATUS
     ADD 1 TO KS-LINES-DONE
     PERFORM Advance-Kit-Status
     IF KS-LINE-EXCESS > 0
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0022" TO CA-REASON-CODE
         MOVE KS-LINE-EXCESS TO Ws-Edit
         STRING "Issued. Excess material of " FUNCTION TRIM(Ws-Edit)
                " is now on this line and needs a disposition."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF.

*>   KIT-008 / ITM-014: issuing CONSUMES the reservation that
*>   covered it.  Until CR2510 this was arithmetic on the kit
*>   line, which drifted whenever stock was issued outside
*>   KITISSUE; now inv.usp_ReserveStock is told, and the kit line
*>   only records what the service actually consumed.
 Consume-The-Reservation.
     INITIALIZE Ws-Resv-Ca
     MOVE "CONSUME" TO RA-FUNCTION
     MOVE CA-COMPANY TO RA-COMPANY
     MOVE KS-SITE TO RA-SITE
     MOVE CA-USER TO RA-USER
     MOVE CA-BUSINESS-DATE TO RA-BUSINESS-DATE
     MOVE CA-CORRELATION-ID TO RA-CORRELATION-ID
     MOVE KL-ITEM-CODE TO SR-ITEM-CODE
     MOVE KS-REVISION TO SR-REVISION
     MOVE KS-SITE TO SR-SITE
     MOVE KS-WAREHOUSE TO SR-WAREHOUSE
     MOVE KS-LOCATION TO SR-LOCATION
     MOVE KS-LOT-CODE TO SR-LOT-CODE
     MOVE KS-SERIAL-NO TO SR-SERIAL-NO
     MOVE "KIT" TO SR-DEMAND-TYPE
     MOVE KS-KIT-REF TO SR-DEMAND-REF
     MOVE KL-LINE-NUM TO SR-DEMAND-LINE
     MOVE KS-STOCK-QTY TO SR-QTY-REQUESTED
     CALL "STKRESV" USING Ws-Resv-Ca
     IF RA-RETURN-CODE > 4 OR SR-QTY-CONSUMED = 0
*>       An issue is allowed to exceed the reservation, so a
*>       refusal here is not a refusal of the issue: the line
*>       simply gives up as much reservation as it had.
         IF KL-RESERVED-QTY > KS-STOCK-QTY
             COMPUTE KL-RESERVED-QTY = KL-RESERVED-QTY - KS-STOCK-QTY
         ELSE
             MOVE 0 TO KL-RESERVED-QTY
         END-IF
     ELSE
         COMPUTE KL-RESERVED-QTY = KL-RESERVED-QTY - SR-QTY-CONSUMED
     END-IF
     IF KL-RESERVED-QTY < 0
         MOVE 0 TO KL-RESERVED-QTY
     END-IF.

 Advance-Kit-Status.
     IF KH-IS-RELEASED
         MOVE "PARTISSUED" TO KD-NEW-STATUS
         MOVE "HDRSTAT" TO KD-FUNC
         MOVE CA-COMPANY TO KD-COMPANY
         MOVE CA-USER TO KD-USER
         MOVE KH-KIT-REF TO KD-KIT-REF
         CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                            KIT-LINE-REC, KIT-ITEM-REC
         MOVE "PARTISSUED" TO KS-KIT-STATUS
     END-IF.

*>----------------------------------------------------------------
*> BACKFLSH - D-20 allows backflush for non-lot and non-serial
*> components only, and KIT-010 forbids a partial success.  So
*> every open line is CHECKED first; only if every one of them
*> can be issued does anything get posted.  That is why a missing
*> mandatory serial or an insufficient component makes the whole
*> combined action fail without claiming success.
 Backflush-The-Kit.
     PERFORM Load-Kit
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     IF NOT KH-BACKFLUSH-ON
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0013" TO CA-REASON-CODE
         MOVE "Backflush is not enabled on this kit; it must be explicitly authorised."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KS-GOOD-QTY <= 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "A good parent quantity is required to backflush."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
*>   CR2742.  Remember what the caller asked for before the first
*>   line overwrites it: both passes start from the same scope.
     MOVE KS-SITE TO Ws-Req-Site
     MOVE KS-WAREHOUSE TO Ws-Req-Warehouse
     MOVE KS-LOCATION TO Ws-Req-Location
     MOVE "Y" TO KS-CHECK-ONLY
     PERFORM Run-Backflush-Pass
     IF CA-FAILED
         MOVE SPACES TO KS-IMPACT-TEXT
         STRING "Backflush refused: no material was issued and no parent was received. First failure on "
                FUNCTION TRIM(KS-FAILED-ITEM) "."
             DELIMITED BY SIZE INTO KS-IMPACT-TEXT
         EXIT PARAGRAPH
     END-IF
     MOVE "N" TO KS-CHECK-ONLY
     MOVE 0 TO KS-LINES-DONE
     PERFORM Run-Backflush-Pass
     IF NOT CA-FAILED
         MOVE SPACES TO KS-IMPACT-TEXT
         STRING "Backflush issued " KS-LINES-DONE
                " component line(s) once for the good output."
             DELIMITED BY SIZE INTO KS-IMPACT-TEXT
     END-IF.

 Run-Backflush-Pass.
     MOVE 0 TO Ws-Done
     MOVE 0 TO Ws-Failed
     PERFORM Collect-Open-Lines
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM VARYING Ws-Lx FROM 1 BY 1
             UNTIL Ws-Lx > Ws-Line-Cnt
         MOVE Ws-Open-Line (Ws-Lx) TO KS-LINE-NUM
         PERFORM Load-Line
         IF CA-FAILED
             EXIT PERFORM
         END-IF
         PERFORM Backflush-One-Line
         IF CA-FAILED
             EXIT PERFORM
         END-IF
     END-PERFORM.

*>   The list is taken, and the cursor given back, before a single
*>   movement is posted (CR2742).  Reading and posting through one
*>   filtered cursor lost every line after the first.
 Collect-Open-Lines.
     MOVE 0 TO Ws-Line-Cnt
     INITIALIZE KIT-DB-CTL
     MOVE "LINOPEN" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     MOVE "Y" TO KD-ONLY-OPEN
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
             IF Ws-Line-Cnt < 300
                 ADD 1 TO Ws-Line-Cnt
                 MOVE KL-LINE-NUM TO Ws-Open-Line (Ws-Line-Cnt)
             ELSE
                 MOVE "N" TO Ws-More
                 MOVE 8 TO CA-RETURN-CODE
                 MOVE "KIT0003" TO CA-REASON-CODE
                 MOVE "This kit has more open lines than one backflush pass may carry; issue them in parts."
                      TO CA-MESSAGE
             END-IF
         END-IF
     END-PERFORM
     MOVE "LINCLOS" TO KD-FUNC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC.

*>----------------------------------------------------------------
*>   CR2742.  Where this line's material actually is.  A backflush
*>   used to take KS-SITE / KS-WAREHOUSE / KS-LOCATION from the
*>   commarea for every line, so a kit whose components sat in two
*>   bins could not be flushed at all (no location gave STK0002 from
*>   STKMOVE, one location gave a shortage on the other component).
*>   The eligible balances decide instead - the same rule KITRESV
*>   reserves against - and the caller's scope only narrows the
*>   search.  AVAILQ is the one service that knows which balance is
*>   eligible (quality, expiry, location availability, ownership),
*>   so nothing here re-implements that test.
 Source-Line-Stock.
     MOVE Ws-Req-Site TO KS-SITE
     MOVE Ws-Req-Warehouse TO KS-WAREHOUSE
     MOVE Ws-Req-Location TO KS-LOCATION
     IF KS-SITE = SPACES
         MOVE KH-SITE TO KS-SITE
     END-IF
     IF KS-LOCATION = SPACES
         MOVE KL-LOCATION TO KS-LOCATION
     END-IF
     INITIALIZE Ws-Avail-Ca
     MOVE "ENQUIRE" TO VA-FUNCTION
     MOVE CA-COMPANY TO VA-COMPANY
     MOVE KS-SITE TO VA-SITE
     MOVE CA-USER TO VA-USER
     MOVE CA-BUSINESS-DATE TO VA-BUSINESS-DATE
     MOVE Ws-Issue-Item TO AV-ITEM-CODE
     MOVE KS-SITE TO AV-SITE
     MOVE Ws-Req-Warehouse TO AV-WAREHOUSE
*>   Deliberately every bin of that warehouse: one location for the
*>   whole kit is the defect being fixed.
     MOVE SPACES TO AV-LOCATION
     MOVE "OWNED" TO AV-OWNER-TYPE
     MOVE "KIT" TO AV-DEMAND-TYPE
     MOVE KS-KIT-REF TO AV-DEMAND-REF
     MOVE KL-LINE-NUM TO AV-DEMAND-LINE
     MOVE CA-BUSINESS-DATE TO AV-AS-AT
     CALL "AVAILQ" USING Ws-Avail-Ca
     IF VA-RETURN-CODE > 4
         MOVE VA-RETURN-CODE TO CA-RETURN-CODE
         MOVE VA-REASON-CODE TO CA-REASON-CODE
         MOVE VA-MESSAGE TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Choose-Eligible-Bucket.

*>   The line's own bin wins when it can carry the issue; otherwise
*>   the eligible bin with the most free stock does.  A traced
*>   balance is never a candidate: backflush may not guess a lot or
*>   a serial (D-20), and an issue quoting neither must match a
*>   balance that carries neither.  Finding nothing leaves the scope
*>   as it was: Check-Free-Stock then reports the shortage (KIT0010)
*>   rather than this paragraph inventing a refusal of its own.
 Choose-Eligible-Bucket.
     MOVE 0 TO Ws-Best-Ix
     MOVE 0 TO Ws-Best-Free
     PERFORM VARYING Ws-Bucket-Ix FROM 1 BY 1
             UNTIL Ws-Bucket-Ix > AV-LINE-CNT
         IF AV-L-ELIGIBLE (Ws-Bucket-Ix) = "Y"
            AND AV-L-FREE (Ws-Bucket-Ix) > 0
            AND AV-L-LOT (Ws-Bucket-Ix) = SPACES
            AND AV-L-SERIAL (Ws-Bucket-Ix) = SPACES
            AND AV-L-OWNER-TYPE (Ws-Bucket-Ix) = "OWNED"
             IF KS-LOCATION NOT = SPACES
                AND AV-L-LOCATION (Ws-Bucket-Ix) = KS-LOCATION
                AND AV-L-FREE (Ws-Bucket-Ix) >= KS-QUANTITY
                 MOVE Ws-Bucket-Ix TO Ws-Best-Ix
                 MOVE AV-L-FREE (Ws-Bucket-Ix) TO Ws-Best-Free
                 EXIT PERFORM
             END-IF
             IF AV-L-FREE (Ws-Bucket-Ix) > Ws-Best-Free
                 MOVE Ws-Bucket-Ix TO Ws-Best-Ix
                 MOVE AV-L-FREE (Ws-Bucket-Ix) TO Ws-Best-Free
             END-IF
         END-IF
     END-PERFORM
     IF Ws-Best-Ix > 0
         MOVE AV-L-SITE (Ws-Best-Ix) TO KS-SITE
         MOVE AV-L-WAREHOUSE (Ws-Best-Ix) TO KS-WAREHOUSE
         MOVE AV-L-LOCATION (Ws-Best-Ix) TO KS-LOCATION
*>       The balance's revision comes with it.  STKMOVE fills a
*>       blank revision from the item master, which is not
*>       necessarily the revision the stock in that bin carries,
*>       and the movement would then be posted against a balance
*>       that does not exist.
         MOVE AV-L-REVISION (Ws-Best-Ix) TO KS-REVISION
     END-IF.

 Backflush-One-Line.
     MOVE KL-ITEM-CODE TO Ws-Issue-Item
     IF KL-SUBSTITUTE NOT = SPACES
         MOVE KL-SUBSTITUTE TO Ws-Issue-Item
     END-IF
     PERFORM Load-Issue-Item
     IF CA-FAILED
         MOVE Ws-Issue-Item TO KS-FAILED-ITEM
         EXIT PARAGRAPH
     END-IF
     IF KI-IS-TRACED
*>       D-20: backflush may not guess a lot or a serial.
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0013" TO CA-REASON-CODE
         MOVE Ws-Issue-Item TO KS-FAILED-ITEM
         STRING "Item " FUNCTION TRIM(Ws-Issue-Item)
                " is lot or serial controlled and cannot be backflushed."
             DELIMITED BY SIZE INTO CA-MESSAGE
         ADD 1 TO KS-LINES-FAILED
         EXIT PARAGRAPH
     END-IF
     IF KH-PLANNED-QTY <= 0
         EXIT PARAGRAPH
     END-IF
*>   Fixed-per-run material is issued by the run, not by the unit,
*>   so a backflush of part of the output must not repeat it.
     IF KL-FIXED-PER-RUN OR KL-FIXED-PER-BATCH
         IF KL-GROSS-ISSUED > 0
             EXIT PARAGRAPH
         END-IF
         MOVE KL-CURRENT-REQ TO Ws-Backflush-Qty
     ELSE
         COMPUTE Ws-Usage-Per ROUNDED
             = KL-CURRENT-REQ / KH-PLANNED-QTY
         COMPUTE Ws-Backflush-Qty ROUNDED
             = Ws-Usage-Per * KS-GOOD-QTY
     END-IF
*>   CR2742.  Material the stores have already issued by hand - or
*>   flushed on an earlier completion - is in work in progress
*>   already, so the flush may only cover what the line still owes.
*>   Until this was deducted, a backflush after a manual issue
*>   issued the usage a SECOND time and over-issued the line.
     COMPUTE Ws-Outstanding = KL-CURRENT-REQ - KL-NET-ISSUED
     IF Ws-Outstanding < 0
         MOVE 0 TO Ws-Outstanding
     END-IF
     IF Ws-Backflush-Qty > Ws-Outstanding
         MOVE Ws-Outstanding TO Ws-Backflush-Qty
     END-IF
     IF Ws-Backflush-Qty <= 0
         EXIT PARAGRAPH
     END-IF
     MOVE Ws-Backflush-Qty TO KS-QUANTITY
     MOVE KL-LINE-NUM TO KS-LINE-NUM
     MOVE KL-UOM-CODE TO KS-UOM-CODE
     MOVE SPACES TO KS-LOT-CODE
     MOVE SPACES TO KS-SERIAL-NO
     PERFORM Source-Line-Stock
     IF CA-FAILED
         MOVE Ws-Issue-Item TO KS-FAILED-ITEM
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-Permitted-Quantity
     IF CA-FAILED
         MOVE Ws-Issue-Item TO KS-FAILED-ITEM
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-Free-Stock
     IF CA-FAILED
         MOVE Ws-Issue-Item TO KS-FAILED-ITEM
         EXIT PARAGRAPH
     END-IF
     IF KS-IS-CHECK
         ADD 1 TO Ws-Done
         EXIT PARAGRAPH
     END-IF
     PERFORM Post-Stock-Issue
     IF CA-FAILED
         MOVE Ws-Issue-Item TO KS-FAILED-ITEM
         EXIT PARAGRAPH
     END-IF
     PERFORM Record-Kit-Issue
     PERFORM Update-Line-Quantities
     ADD 1 TO Ws-Done.
