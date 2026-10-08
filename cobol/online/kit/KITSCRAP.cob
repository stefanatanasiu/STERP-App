>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : KITSCRAP
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : Record scrap against an execution kit (KIT-014,
*>              D-12).
*>
*>              Scrap is a DISPOSITION, not a return.  The 1998
*>              version treated a scrap as a negative issue, which
*>              quietly removed the cost from the job and then
*>              issued a replacement automatically; a job could
*>              consume three times its material and still show
*>              the planned cost.  CR2497 stopped both halves of
*>              that:
*>                * scrapped material stays charged to the job -
*>                  the cost is where the loss happened;
*>                * a replacement is a NEW requirement that needs
*>                  authority, so KITSCRAP only reports how much
*>                  would be needed (KP-REPLACE-NEEDED).
*>
*>              Three kinds are recognised, because they are
*>              economically different:
*>                COMPONENT  material already issued to the job
*>                           and spoiled on the job
*>                OUTPUT     finished parent units rejected
*>                SUBCON     material lost at a subcontractor
*> CALLED BY  : frmKitExecution, KITCOMPL (rejected output)
*> INTERFACE  : CALL "KITSCRAP" USING WS-KITSCRAP-CA
*>              COPYBOOK KITSCRAPCA
*> CALLS      : KITDB, KITQTY, EXCPRAIS, AUDITLOG
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2015-02-10 PMK  CR2290 SCRAP RECORDING
*> 2016-09-14 PMK  CR2497 NO AUTOMATIC REPLACEMENT ISSUE
*> 2026-10-06 STERP KIT-014
*>================================================================
*> REQ: KIT-014
 IDENTIFICATION DIVISION.
 PROGRAM-ID. KITSCRAP.
 AUTHOR. P M KEANE.
 DATE-WRITTEN. 10/02/2015.
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
 COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==Ws-Exc-Ca==
      LEADING ==CA-== BY ==XA-==.
 COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==Ws-Audit-Ca==
      LEADING ==CA-== BY ==AA-==.
 01  Ws-Work.
     05  Ws-On-Job               PIC S9(13)V9(6) VALUE 0.
     05  Ws-Limit                PIC S9(13)V9(6) VALUE 0.
     05  Ws-Edit                 PIC -(13)9.9(6).
     05  Ws-Money                PIC -(11)9.99.
 LINKAGE SECTION.
 COPY KITSCRAPCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "KITSCRAP" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE 0 TO CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     MOVE "KIT-014" TO CA-REQ-REF
     MOVE 0 TO KP-UNIT-COST
     MOVE 0 TO KP-VALUE-GBP
     MOVE 0 TO KP-KIT-ISSUE-ID
     MOVE 0 TO KP-MOVEMENT-ID
     MOVE 0 TO KP-REPLACE-NEEDED
     MOVE 0 TO KP-EXCEPTION-ID
     MOVE "N" TO KP-NEEDS-AUTH
     MOVE SPACES TO KP-IMPACT-TEXT
     IF KP-ACTUAL-DATE = 0
         MOVE CA-BUSINESS-DATE TO KP-ACTUAL-DATE
     END-IF
     IF KP-POSTING-DATE = 0
         MOVE KP-ACTUAL-DATE TO KP-POSTING-DATE
     END-IF
     IF KP-SCRAP-KIND = SPACES
         MOVE "COMPONENT" TO KP-SCRAP-KIND
     END-IF
     EVALUATE CA-FUNCTION
         WHEN "SCRAPCK" PERFORM Check-The-Scrap
         WHEN "SCRAP"   PERFORM Record-The-Scrap
         WHEN OTHER     PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "KIT0002" TO CA-REASON-CODE
     STRING "KITSCRAP does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

 Set-Sql-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE KD-ERRMSG TO CA-MESSAGE.

*>----------------------------------------------------------------
 Load-Kit.
     INITIALIZE KIT-DB-CTL
     INITIALIZE KIT-HEADER-REC
     MOVE "HDRGET" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KP-KIT-REF TO KD-KIT-REF
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     EVALUATE KD-RC
         WHEN 0
             CONTINUE
         WHEN 100
             MOVE 16 TO CA-RETURN-CODE
             MOVE "KIT0001" TO CA-REASON-CODE
             STRING "Kit " FUNCTION TRIM(KP-KIT-REF)
                    " is not known in this company."
                 DELIMITED BY SIZE INTO CA-MESSAGE
         WHEN OTHER
             PERFORM Set-Sql-Failure
     END-EVALUATE
     IF CA-RETURN-CODE = 0 AND KH-IS-FINCLOSED
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0023" TO CA-REASON-CODE
         MOVE "This kit has been financially closed; scrap must be recorded before closure."
              TO CA-MESSAGE
     END-IF
     IF KP-SITE = SPACES
         MOVE KH-SITE TO KP-SITE
     END-IF
     MOVE KH-REJECTED-QTY TO KP-REJECTED-TOTAL.

 Load-Line.
     MOVE "LINGET" TO KD-FUNC
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     MOVE KP-LINE-NUM TO KD-LINE-NUM
     INITIALIZE KIT-LINE-REC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     EVALUATE KD-RC
         WHEN 0
             IF KP-ITEM-CODE = SPACES
                 MOVE KL-ITEM-CODE TO KP-ITEM-CODE
             END-IF
             IF KP-UOM-CODE = SPACES
                 MOVE KL-UOM-CODE TO KP-UOM-CODE
             END-IF
         WHEN 100
             MOVE 16 TO CA-RETURN-CODE
             MOVE "KIT0007" TO CA-REASON-CODE
             MOVE "That material line is not on this kit." TO CA-MESSAGE
         WHEN OTHER
             PERFORM Set-Sql-Failure
     END-EVALUATE.

*>----------------------------------------------------------------
*> SCRAPCK - the whole decision, with no side effect.
 Check-The-Scrap.
     PERFORM Load-Kit
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     IF KP-QUANTITY <= 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "The scrapped quantity must be greater than zero."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
*>   KIT-014 wants a cause on every scrap.  "Shrinkage" with no
*>   explanation is how a works order quietly absorbs theft.
     IF KP-CAUSE-CODE = SPACES AND KP-CAUSE-TEXT = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0008" TO CA-REASON-CODE
         MOVE "Scrap needs a cause code or a cause description."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     EVALUATE TRUE
         WHEN KP-OUTPUT    PERFORM Check-Output-Scrap
         WHEN KP-SUBCON    PERFORM Check-Subcon-Scrap
         WHEN KP-COMPONENT PERFORM Check-Component-Scrap
         WHEN OTHER
             MOVE 8 TO CA-RETURN-CODE
             MOVE "KIT0002" TO CA-REASON-CODE
             STRING "Scrap kind " FUNCTION TRIM(KP-SCRAP-KIND)
                    " is not recognised."
                 DELIMITED BY SIZE INTO CA-MESSAGE
     END-EVALUATE
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-Authority
     PERFORM Compute-Replacement.

*>   Component scrap can only cover material that is actually on
*>   the job: net issued, less what has already been scrapped.
*>   Scrapping more than that would be scrapping stores stock,
*>   which is a stock adjustment, not a kit disposition.
 Check-Component-Scrap.
     IF KP-LINE-NUM <= 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0007" TO CA-REASON-CODE
         MOVE "Component scrap must name the kit material line."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Line
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ws-On-Job = KL-NET-ISSUED - KL-SCRAPPED-QTY
     IF Ws-On-Job < 0
         MOVE 0 TO Ws-On-Job
     END-IF
     MOVE Ws-On-Job TO KP-ON-JOB-QTY
     IF KP-QUANTITY > Ws-On-Job
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE Ws-On-Job TO Ws-Edit
         STRING "Only " FUNCTION TRIM(Ws-Edit)
                " of this component is on the job and can be scrapped."
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Value-The-Scrap.

*>   Rejected parent output is measured against what the run has
*>   actually produced plus what it may still produce; it is not
*>   a material line at all.
 Check-Output-Scrap.
     IF KP-ITEM-CODE = SPACES
         MOVE KH-PARENT-ITEM TO KP-ITEM-CODE
     END-IF
     COMPUTE Ws-Limit = KH-PLANNED-QTY - KH-COMPLETED-QTY
                      - KH-REJECTED-QTY
     IF Ws-Limit < 0
         MOVE 0 TO Ws-Limit
     END-IF
     MOVE Ws-Limit TO KP-ON-JOB-QTY
     IF KP-QUANTITY > Ws-Limit
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0018" TO CA-REASON-CODE
         MOVE Ws-Limit TO Ws-Edit
         STRING "At most " FUNCTION TRIM(Ws-Edit)
                " units of output remain to be accounted for on this run."
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Value-The-Output.

 Check-Subcon-Scrap.
     IF KP-SUBCON-REF = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0008" TO CA-REASON-CODE
         MOVE "A subcontract loss must name the subcontract order it happened on."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KP-LINE-NUM > 0
         PERFORM Check-Component-Scrap
     ELSE
         PERFORM Value-The-Output
     END-IF.

 Value-The-Scrap.
*>   The attributable cost is the cost the job was charged, so a
*>   scrap neither creates nor destroys value in the ledger: it
*>   moves it from work in progress to a loss.
     IF KL-NET-ISSUED > 0 AND KL-ISSUED-VALUE NOT = 0
         COMPUTE KP-UNIT-COST ROUNDED
             = KL-ISSUED-VALUE / KL-NET-ISSUED
     ELSE
         MOVE KL-PLAN-UNIT-COST TO KP-UNIT-COST
     END-IF
     COMPUTE KP-VALUE-GBP ROUNDED = KP-UNIT-COST * KP-QUANTITY.

 Value-The-Output.
     MOVE "ITEMGET" TO KD-FUNC
     MOVE KP-ITEM-CODE TO KD-ITEM-CODE
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC = 0
         IF KI-AVG-COST > 0
             MOVE KI-AVG-COST TO KP-UNIT-COST
         ELSE
             MOVE KI-STD-COST TO KP-UNIT-COST
         END-IF
     END-IF
     IF KP-UNIT-COST = 0 AND KH-COMPLETED-QTY > 0
         COMPUTE KP-UNIT-COST ROUNDED
             = KH-WIP-VALUE / KH-COMPLETED-QTY
     END-IF
     COMPUTE KP-VALUE-GBP ROUNDED = KP-UNIT-COST * KP-QUANTITY.

*>   KIT-014 - scrap has to be authorised.  The threshold lives
*>   in the kit: output scrap and subcontract loss always need
*>   it, component scrap needs it once there is real value at
*>   stake.
 Check-Authority.
     IF KP-OUTPUT OR KP-SUBCON OR KP-VALUE-GBP > 0
         IF KP-APPROVAL-ID = 0
             MOVE "Y" TO KP-NEEDS-AUTH
             MOVE 6 TO CA-RETURN-CODE
             MOVE "KIT0009" TO CA-REASON-CODE
             MOVE KP-VALUE-GBP TO Ws-Money
             STRING "Scrap of " FUNCTION TRIM(Ws-Money)
                    " needs an authorisation before it can be recorded."
                 DELIMITED BY SIZE INTO CA-MESSAGE
         END-IF
     END-IF.

*>   What would have to be issued again to finish the job - a
*>   number, not an action.  Someone with authority decides.
 Compute-Replacement.
     IF NOT KP-WANTS-REPLACE
         EXIT PARAGRAPH
     END-IF
     IF KP-COMPONENT OR KP-LINE-NUM > 0
         MOVE KP-QUANTITY TO KP-REPLACE-NEEDED
         MOVE KP-REPLACE-NEEDED TO Ws-Edit
         STRING "Replacing the scrapped material needs a further "
                FUNCTION TRIM(Ws-Edit)
                " to be requisitioned and approved; nothing was issued."
             DELIMITED BY SIZE INTO KP-IMPACT-TEXT
     ELSE
         MOVE KP-QUANTITY TO KP-REPLACE-NEEDED
         MOVE "Re-making the rejected output is a new run; it is not started automatically."
              TO KP-IMPACT-TEXT
     END-IF.

*>----------------------------------------------------------------
*> SCRAP - record it.  Note what is deliberately NOT done: no
*> stock movement for component scrap, because the material left
*> stock when it was issued; and no reduction of the historical
*> issue value, because the job really did consume it.
 Record-The-Scrap.
     PERFORM Check-The-Scrap
     IF CA-RETURN-CODE > 4 OR CA-RETURN-CODE = 6
         EXIT PARAGRAPH
     END-IF
     IF KP-CHECK-ONLY = "Y"
         EXIT PARAGRAPH
     END-IF
     PERFORM Record-Kit-Scrap
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     IF KP-OUTPUT OR (KP-SUBCON AND KP-LINE-NUM = 0)
         PERFORM Update-Rejected-Output
     ELSE
         PERFORM Update-Line-Scrapped
     END-IF
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Raise-Scrap-Exception
     PERFORM Audit-Scrap-Event
     MOVE KP-VALUE-GBP TO Ws-Money
     STRING "Scrap recorded; " FUNCTION TRIM(Ws-Money)
            " stays charged to the job as a loss, not as stock."
         DELIMITED BY SIZE INTO CA-MESSAGE
     IF KP-REPLACE-NEEDED > 0
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0017" TO CA-REASON-CODE
     END-IF.

 Record-Kit-Scrap.
     MOVE "ISSINS" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KP-KIT-REF TO KD-KIT-REF
     MOVE CA-CORRELATION-ID TO KD-REASON
     MOVE "SCRAP" TO KD-I-TYPE
     MOVE KP-ITEM-CODE TO KD-I-ITEM
     MOVE KP-REVISION TO KD-I-REV
     MOVE KP-QUANTITY TO KD-I-QTY
     MOVE KP-UOM-CODE TO KD-I-UOM
     MOVE KP-QUANTITY TO KD-I-STOCK-QTY
     MOVE SPACES TO KD-I-WAREHOUSE
     MOVE SPACES TO KD-I-LOCATION
     MOVE KP-LOT-CODE TO KD-I-LOT
     MOVE KP-SERIAL-NO TO KD-I-SERIAL
     MOVE KP-ACTUAL-DATE TO KD-I-ACT-DATE
     MOVE KP-POSTING-DATE TO KD-I-POST-DATE
     MOVE KP-UNIT-COST TO KD-I-UNIT-COST
     MOVE KP-VALUE-GBP TO KD-I-VALUE
     MOVE KP-CAUSE-CODE TO KD-I-REASON-CODE
     MOVE KP-CAUSE-TEXT (1:200) TO KD-I-REASON
     MOVE KP-APPROVAL-ID TO KD-I-APPROVAL
     MOVE 0 TO KD-I-ORIG-ISSUE
     MOVE 0 TO KD-I-MOVEMENT
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
     ELSE
         MOVE KD-NEW-ID TO KP-KIT-ISSUE-ID
     END-IF.

*>   KITQTY is told about the scrap so that the accountability
*>   test (issued = consumed + on job + returned + scrapped) can
*>   be answered at close.  Scrap does NOT raise the remaining
*>   requirement: that would be an automatic replacement by the
*>   back door.
 Update-Line-Scrapped.
     COMPUTE KL-SCRAPPED-QTY = KL-SCRAPPED-QTY + KP-QUANTITY
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
     MOVE "LINSAVE" TO KD-FUNC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE KL-SCRAPPED-QTY TO KP-LINE-SCRAPPED
     MOVE KL-NET-ISSUED TO KP-LINE-NET
     MOVE KL-REMAINING-QTY TO KP-LINE-REMAINING
     MOVE KL-EXCESS-QTY TO KP-LINE-EXCESS
     COMPUTE KP-ON-JOB-QTY = KL-NET-ISSUED - KL-SCRAPPED-QTY.

 Update-Rejected-Output.
     COMPUTE KH-REJECTED-QTY = KH-REJECTED-QTY + KP-QUANTITY
     COMPUTE KH-WIP-VALUE ROUNDED = KH-WIP-VALUE - KP-VALUE-GBP
     IF KH-WIP-VALUE < 0
         MOVE 0 TO KH-WIP-VALUE
     END-IF
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
     MOVE KH-REJECTED-QTY TO KP-REJECTED-TOTAL.

 Raise-Scrap-Exception.
     INITIALIZE Ws-Exc-Ca
     MOVE "RAISE" TO XA-FUNCTION
     MOVE CA-COMPANY TO XA-COMPANY
     MOVE KP-SITE TO XA-SITE
     MOVE CA-USER TO XA-USER
     MOVE CA-BUSINESS-DATE TO XA-BUSINESS-DATE
     MOVE "KIT" TO EX-MODULE
     MOVE "WARNING" TO EX-SEVERITY
     MOVE "KIT SCRAP" TO EX-CATEGORY
     STRING FUNCTION TRIM(KP-SCRAP-KIND) " scrap of "
            FUNCTION TRIM(KP-ITEM-CODE) " on kit "
            FUNCTION TRIM(KP-KIT-REF)
         DELIMITED BY SIZE INTO EX-MESSAGE-TEXT
     MOVE "EXECUTIONKIT" TO EX-DOCUMENT-TYPE
     MOVE KP-KIT-REF TO EX-DOCUMENT-REF
     MOVE KH-OWNER-USER TO EX-OWNER-USER
     MOVE 3 TO EX-PRIORITY
     MOVE KP-QUANTITY TO EX-QUANTITY
     MOVE KP-VALUE-GBP TO EX-AMOUNT-GBP
     CALL "EXCPRAIS" USING Ws-Exc-Ca
     MOVE EX-EXCEPTION-ID TO KP-EXCEPTION-ID.

 Audit-Scrap-Event.
     INITIALIZE Ws-Audit-Ca
     MOVE "WRITE" TO AA-FUNCTION
     MOVE CA-COMPANY TO AA-COMPANY
     MOVE KP-SITE TO AA-SITE
     MOVE CA-USER TO AA-USER
     MOVE CA-BUSINESS-DATE TO AA-BUSINESS-DATE
     MOVE CA-CORRELATION-ID TO AA-CORRELATION-ID
     MOVE "KIT.EXECUTIONKIT" TO AU-ENTITY-TYPE
     MOVE KP-KIT-REF TO AU-ENTITY-KEY
     MOVE "SCRAP" TO AU-ACTION
     MOVE KP-SCRAP-KIND TO AU-FIELD-NAME
     MOVE KP-ITEM-CODE TO AU-OLD-VALUE
     MOVE KP-QUANTITY TO Ws-Edit
     MOVE Ws-Edit TO AU-NEW-VALUE
     MOVE KP-CAUSE-TEXT (1:200) TO AU-REASON
     MOVE KP-SUBCON-REF TO AU-AUTHORITY-REF
     MOVE "KITSCRAP" TO AU-SOURCE
     MOVE "N" TO AU-SENSITIVE
     CALL "AUDITLOG" USING Ws-Audit-Ca.
