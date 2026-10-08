>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : KITAMEND
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : Amend a released execution kit.  Two rules govern
*>              everything here:
*>                KIT-005 an amendment must SHOW its quantity,
*>                        reservation, cost and source demand
*>                        impact and be authorised for the
*>                        changed version;
*>                KIT-003 an amendment may never rewrite an actual
*>                        issue nor silently clamp away excess.
*>              So an issued line can have its requirement reduced
*>              - which makes the excess visible - but it can
*>              never be deleted.  Its history and its cost stay.
*>              SUBST approves a context specific substitute for
*>              this kit only; the standard bill is untouched
*>              (KIT-012, D-11 equal or higher specification).
*> CALLED BY  : frmKitMaintenance, KITSCRAP (replacement material)
*> INTERFACE  : CALL "KITAMEND" USING WS-KITAMEND-CA
*>              COPYBOOK KITAMENDCA
*> CALLS      : KITDB, KITQTY, PERMCHK, AUDITLOG
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2015-04-20 PMK  CR2320 KIT AMENDMENT WITH IMPACT
*> 2016-05-30 PMK  CR2488 EXCESS STAYS VISIBLE AFTER A REDUCTION
*> 2026-10-06 STERP KIT-003, KIT-005, KIT-012
*>================================================================
*> REQ: KIT-003, KIT-005, KIT-012
 IDENTIFICATION DIVISION.
 PROGRAM-ID. KITAMEND.
 AUTHOR. P M KEANE.
 DATE-WRITTEN. 20/04/2015.
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
 COPY PERMCHKCA REPLACING ==DFHCOMMAREA== BY ==Ws-Perm-Ca==
      LEADING ==CA-== BY ==PA-==.
 COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==Ws-Audit-Ca==
      LEADING ==CA-== BY ==AA-==.
 01  Ws-Work.
     05  Ws-More                 PIC X VALUE "Y".
     05  Ws-Changed              PIC S9(4) VALUE 0.
     05  Ws-Fixed-Kept           PIC S9(4) VALUE 0.
     05  Ws-Max-Line             PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Usage-Per            PIC S9(13)V9(6) VALUE 0.
     05  Ws-Old-Req              PIC S9(13)V9(6) VALUE 0.
     05  Ws-Edit                 PIC -(13)9.9(6).
     05  Ws-Num-Ed               PIC Z(8)9.
 LINKAGE SECTION.
 COPY KITAMENDCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "KITAMEND" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE 0 TO CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     MOVE "KIT-005" TO CA-REQ-REF
     MOVE SPACES TO KA-IMPACT-TEXT
     MOVE "N" TO KA-NEEDS-AUTH
     MOVE 0 TO KA-LINES-CHANGED
     MOVE 0 TO KA-FIXED-KEPT
     EVALUATE CA-FUNCTION
         WHEN "IMPACT"   PERFORM Report-Impact
         WHEN "AMEND"    PERFORM Apply-Amendment
         WHEN "SCALE"    PERFORM Scale-Planned-Output
         WHEN "ADDLINE"  PERFORM Add-Approved-Line
         WHEN "CANCELLN" PERFORM Cancel-Unissued-Line
         WHEN "SUBST"    PERFORM Approve-Substitute
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "KIT0002" TO CA-REASON-CODE
     STRING "KITAMEND does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

 Set-Sql-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE KD-ERRMSG TO CA-MESSAGE.

 Load-Kit.
     INITIALIZE KIT-DB-CTL
     INITIALIZE KIT-HEADER-REC
     MOVE "HDRGET" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KA-KIT-REF TO KD-KIT-REF
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     EVALUATE KD-RC
         WHEN 0
             MOVE KH-STATUS TO KA-KIT-STATUS
         WHEN 100
             MOVE 16 TO CA-RETURN-CODE
             MOVE "KIT0001" TO CA-REASON-CODE
             STRING "Kit " FUNCTION TRIM(KA-KIT-REF)
                    " is not known in this company."
                 DELIMITED BY SIZE INTO CA-MESSAGE
         WHEN OTHER
             PERFORM Set-Sql-Failure
     END-EVALUATE
     IF CA-RETURN-CODE = 0 AND NOT KH-MAY-AMEND
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0005" TO CA-REASON-CODE
         STRING "A kit in status " FUNCTION TRIM(KH-STATUS)
                " can no longer be amended."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF.

 Load-Line.
     MOVE "LINGET" TO KD-FUNC
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     MOVE KA-LINE-NUM TO KD-LINE-NUM
     INITIALIZE KIT-LINE-REC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     EVALUATE KD-RC
         WHEN 0
             MOVE KL-ITEM-CODE TO KA-ITEM-CODE
         WHEN 100
             MOVE 16 TO CA-RETURN-CODE
             MOVE "KIT0007" TO CA-REASON-CODE
             MOVE "That material line is not on this kit." TO CA-MESSAGE
         WHEN OTHER
             PERFORM Set-Sql-Failure
     END-EVALUATE.

*>----------------------------------------------------------------
*> The impact calculation is shared by IMPACT and AMEND so that
*> what the user was shown is exactly what gets applied.
 Compute-Change-Impact.
     INITIALIZE Ws-Qty-Ca
     MOVE "REQCHG" TO QA-FUNCTION
     MOVE CA-COMPANY TO QA-COMPANY
     MOVE CA-USER TO QA-USER
     MOVE KA-KIT-REF TO KQ-KIT-REF
     MOVE KL-LINE-NUM TO KQ-LINE-NUM
     MOVE KL-ITEM-CODE TO KQ-ITEM-CODE
     MOVE KL-ORIGINAL-REQ TO KQ-ORIGINAL-REQ
     MOVE KL-CURRENT-REQ TO KQ-CURRENT-REQ
     MOVE KA-PROPOSED-REQ TO KQ-PROPOSED-REQ
     MOVE KL-GROSS-ISSUED TO KQ-GROSS-ISSUED
     MOVE KL-RETURNED-QTY TO KQ-RETURNED
     MOVE KL-SCRAPPED-QTY TO KQ-SCRAPPED
     MOVE KL-PLAN-UNIT-COST TO KQ-UNIT-COST
     CALL "KITQTY" USING Ws-Qty-Ca
     MOVE KL-CURRENT-REQ TO KA-OLD-REQ
     MOVE KQ-NEW-REQ TO KA-NEW-REQ
     MOVE KQ-REQ-DELTA TO KA-REQ-DELTA
     MOVE KQ-NET-ISSUED TO KA-NET-ISSUED
     MOVE KQ-REMAINING TO KA-REMAINING
     MOVE KQ-EXCESS TO KA-EXCESS
     MOVE KQ-COST-IMPACT TO KA-COST-IMPACT
     MOVE KQ-NEEDS-AUTH TO KA-NEEDS-AUTH
     MOVE KQ-ISSUED-HISTORY TO KA-HAS-HISTORY
     MOVE KQ-IMPACT-TEXT TO KA-IMPACT-TEXT
     MOVE KH-SOURCE-TYPE TO KA-SOURCE-TYPE
     MOVE KH-SOURCE-REF TO KA-SOURCE-REF
*>   A reduction frees reservation, an increase asks for more,
*>   but reservation never falls below what is already reserved
*>   and not yet issued.
     COMPUTE KA-RESV-IMPACT = KQ-REQ-DELTA
     IF KA-RESV-IMPACT < 0
             AND FUNCTION ABS (KA-RESV-IMPACT) > KL-RESERVED-QTY
         COMPUTE KA-RESV-IMPACT = 0 - KL-RESERVED-QTY
     END-IF
     IF KQ-NEEDS-AUTH = "Y"
         MOVE 6 TO CA-RETURN-CODE
         MOVE QA-REASON-CODE TO CA-REASON-CODE
         MOVE QA-MESSAGE TO CA-MESSAGE
     END-IF.

 Report-Impact.
     PERFORM Load-Kit
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Line
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Compute-Change-Impact
     IF CA-RETURN-CODE = 6
*>       IMPACT is an enquiry: the hold is information, not a
*>       refusal, so it is reported as a warning.
         MOVE 4 TO CA-RETURN-CODE
     END-IF.

*>----------------------------------------------------------------
*> AMEND - apply the change.  An authorised change still cannot
*> remove an issue; it only moves the approved requirement and
*> therefore the remaining and excess quantities.
 Apply-Amendment.
     PERFORM Load-Kit
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Line
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     IF KA-REASON-TEXT = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0008" TO CA-REASON-CODE
         MOVE "A reason is required for a kit amendment." TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Compute-Change-Impact
     IF KA-NEEDS-AUTH = "Y" AND KA-APPROVAL-ID = 0
*>       The hold stands: the caller must come back with the
*>       approval that covers the changed version.
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-Amend-Authority
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     MOVE KL-CURRENT-REQ TO Ws-Old-Req
     MOVE KA-NEW-REQ TO KL-CURRENT-REQ
     MOVE KA-REMAINING TO KL-REMAINING-QTY
     MOVE KA-EXCESS TO KL-EXCESS-QTY
     MOVE KA-NET-ISSUED TO KL-NET-ISSUED
     MOVE KA-REASON-TEXT TO KL-DEVIATION
     MOVE CA-USER TO KL-DEV-APPROVER
     PERFORM Set-Line-Status
     PERFORM Save-Line
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     MOVE 1 TO KA-LINES-CHANGED
     MOVE 0 TO CA-RETURN-CODE
     MOVE SPACES TO CA-REASON-CODE
     IF KA-EXCESS > 0
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0022" TO CA-REASON-CODE
         MOVE KA-EXCESS TO Ws-Edit
         STRING "Requirement amended; excess issued material of "
                FUNCTION TRIM(Ws-Edit)
                " must be returned or disposed of with authority."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF
     PERFORM Audit-Amendment.

*>   An issued line whose requirement falls to zero is NOT closed
*>   and NOT deleted.  It stays, carrying its history, so the
*>   excess remains visible on every enquiry and document.
 Set-Line-Status.
     EVALUATE TRUE
         WHEN KL-GROSS-ISSUED <= 0
             MOVE "OPEN" TO KL-STATUS
         WHEN KL-REMAINING-QTY > 0
             MOVE "PARTISSUED" TO KL-STATUS
         WHEN OTHER
             MOVE "ISSUED" TO KL-STATUS
     END-EVALUATE.

 Save-Line.
     MOVE "LINSAVE" TO KD-FUNC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
     END-IF.

 Check-Amend-Authority.
     INITIALIZE Ws-Perm-Ca
     MOVE "CHECK" TO PA-FUNCTION
     MOVE CA-COMPANY TO PA-COMPANY
     MOVE KH-SITE TO PA-SITE
     MOVE CA-USER TO PA-USER
     MOVE CA-BUSINESS-DATE TO PA-BUSINESS-DATE
     MOVE "KIT.EXEC.AMEND" TO PK-PERMISSION
     MOVE CA-USER TO PK-CHECK-USER
     MOVE KH-SITE TO PK-CHECK-SITE
     CALL "PERMCHK" USING Ws-Perm-Ca
     IF PA-RETURN-CODE NOT = 0
         MOVE 6 TO CA-RETURN-CODE
         MOVE "KIT0009" TO CA-REASON-CODE
         MOVE "Y" TO KA-NEEDS-AUTH
         MOVE "Amending a released kit needs the kit amendment authority."
              TO CA-MESSAGE
     END-IF.

 Audit-Amendment.
     INITIALIZE Ws-Audit-Ca
     MOVE "WRITE" TO AA-FUNCTION
     MOVE CA-COMPANY TO AA-COMPANY
     MOVE KH-SITE TO AA-SITE
     MOVE CA-USER TO AA-USER
     MOVE CA-BUSINESS-DATE TO AA-BUSINESS-DATE
     MOVE CA-CORRELATION-ID TO AA-CORRELATION-ID
     MOVE "KIT.EXECUTIONKITLINE" TO AU-ENTITY-TYPE
     MOVE KL-LINE-NUM TO Ws-Num-Ed
     STRING FUNCTION TRIM(KA-KIT-REF) "/"
            FUNCTION TRIM(Ws-Num-Ed)
         DELIMITED BY SIZE INTO AU-ENTITY-KEY
     MOVE CA-FUNCTION TO AU-ACTION
     MOVE "CurrentRequirementQty" TO AU-FIELD-NAME
     MOVE Ws-Old-Req TO Ws-Edit
     MOVE Ws-Edit TO AU-OLD-VALUE
     MOVE KL-CURRENT-REQ TO Ws-Edit
     MOVE Ws-Edit TO AU-NEW-VALUE
     MOVE KA-REASON-TEXT (1:200) TO AU-REASON
     MOVE KA-AUTHORITY-REF TO AU-AUTHORITY-REF
     MOVE "KITAMEND" TO AU-SOURCE
     MOVE "N" TO AU-SENSITIVE
     CALL "AUDITLOG" USING Ws-Audit-Ca.

*>----------------------------------------------------------------
*> SCALE - the planned good output changed.  D-10 and KIT-016:
*> proportional usage scales, fixed-per-run usage is NOT repeated
*> and fixed-per-batch follows the approved batch plan.  The
*> per-unit usage is recovered from the ORIGINAL requirement and
*> the original planned output, so repeated rescaling does not
*> drift.
 Scale-Planned-Output.
     PERFORM Load-Kit
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     IF KA-NEW-OUTPUT <= 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "A new planned good output must be greater than zero."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-Amend-Authority
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO Ws-Changed
     MOVE 0 TO Ws-Fixed-Kept
     INITIALIZE KIT-DB-CTL
     MOVE "LINOPEN" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
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
             PERFORM Scale-One-Line
         END-IF
     END-PERFORM
     MOVE "LINCLOS" TO KD-FUNC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     MOVE Ws-Changed TO KA-LINES-CHANGED
     MOVE Ws-Fixed-Kept TO KA-FIXED-KEPT
     MOVE SPACES TO KA-IMPACT-TEXT
     STRING "Rescaled " Ws-Changed " proportional line(s); "
            Ws-Fixed-Kept " fixed-per-run or per-batch line(s) unchanged."
         DELIMITED BY SIZE INTO KA-IMPACT-TEXT.

 Scale-One-Line.
     IF KL-FIXED-PER-RUN OR KL-FIXED-PER-BATCH
         ADD 1 TO Ws-Fixed-Kept
         EXIT PARAGRAPH
     END-IF
     IF KH-PLANNED-QTY <= 0
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ws-Usage-Per ROUNDED
         = KL-ORIGINAL-REQ / KH-PLANNED-QTY
     INITIALIZE Ws-Qty-Ca
     MOVE "SCALE" TO QA-FUNCTION
     MOVE CA-COMPANY TO QA-COMPANY
     MOVE CA-USER TO QA-USER
     MOVE KL-USAGE-BASIS TO KQ-USAGE-BASIS
     MOVE Ws-Usage-Per TO KQ-USAGE-PER
     MOVE KL-SCRAP-PCT TO KQ-SCRAP-PCT
     MOVE KH-PLANNED-QTY TO KQ-OLD-OUTPUT
     MOVE KA-NEW-OUTPUT TO KQ-NEW-OUTPUT
     MOVE KH-YIELD TO KQ-YIELD
     MOVE KH-QUANTUM TO KQ-QUANTUM
     MOVE KH-BATCH-SIZE TO KQ-BATCH-SIZE
     MOVE KH-RUN-COUNT TO KQ-RUN-COUNT
     MOVE KH-BATCH-COUNT TO KQ-BATCH-COUNT
     MOVE KL-CURRENT-REQ TO KQ-CURRENT-REQ
     MOVE KL-PLAN-UNIT-COST TO KQ-UNIT-COST
     CALL "KITQTY" USING Ws-Qty-Ca
     IF QA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     MOVE KQ-NEW-REQ TO KL-CURRENT-REQ
     COMPUTE KL-NET-ISSUED = KL-GROSS-ISSUED - KL-RETURNED-QTY
     COMPUTE KL-REMAINING-QTY = KL-CURRENT-REQ - KL-NET-ISSUED
     IF KL-REMAINING-QTY < 0
         MOVE 0 TO KL-REMAINING-QTY
     END-IF
     COMPUTE KL-EXCESS-QTY = KL-NET-ISSUED - KL-CURRENT-REQ
     IF KL-EXCESS-QTY < 0
         MOVE 0 TO KL-EXCESS-QTY
     END-IF
     PERFORM Set-Line-Status
     PERFORM Save-Line
     ADD 1 TO Ws-Changed
     ADD KQ-COST-IMPACT TO KA-COST-IMPACT.

*>----------------------------------------------------------------
*> ADDLINE - approved extra requirement, typically replacement
*> material after scrap.  D-12 wants this to be an explicit
*> authorised act, not an automatic consequence of scrapping.
 Add-Approved-Line.
     PERFORM Load-Kit
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     IF KA-ITEM-CODE = SPACES OR KA-PROPOSED-REQ <= 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "An item code and a positive requirement are needed to add a kit line."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KA-REASON-TEXT = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0008" TO CA-REASON-CODE
         MOVE "A reason is required for an additional material requirement."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-Amend-Authority
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Find-Next-Line-Number
     MOVE "ITEMGET" TO KD-FUNC
     MOVE KA-ITEM-CODE TO KD-ITEM-CODE
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     INITIALIZE KIT-LINE-REC
     MOVE Ws-Max-Line TO KL-LINE-NUM
     MOVE KA-ITEM-CODE TO KL-ITEM-CODE
     MOVE KA-REVISION TO KL-REVISION
     MOVE Ws-Max-Line TO Ws-Num-Ed
     STRING "ADD/" FUNCTION TRIM(Ws-Num-Ed)
         DELIMITED BY SIZE INTO KL-PATH-REF
     IF KA-UOM-CODE = SPACES
         MOVE KI-STOCK-UOM TO KL-UOM-CODE
     ELSE
         MOVE KA-UOM-CODE TO KL-UOM-CODE
     END-IF
     MOVE 1 TO KL-FACTOR
     IF KA-USAGE-BASIS = SPACES
         MOVE "PROPORTION" TO KL-USAGE-BASIS
     ELSE
         MOVE KA-USAGE-BASIS TO KL-USAGE-BASIS
     END-IF
     MOVE KA-PROPOSED-REQ TO KL-ORIGINAL-REQ
     MOVE KA-PROPOSED-REQ TO KL-CURRENT-REQ
     MOVE KA-PROPOSED-REQ TO KL-REMAINING-QTY
     MOVE KI-STD-COST TO KL-PLAN-UNIT-COST
     MOVE KA-NEED-DATE TO KL-NEED-DATE
     MOVE KA-LOCATION TO KL-LOCATION
     MOVE "OPEN" TO KL-STATUS
     MOVE "LININS" TO KD-FUNC
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     MOVE KH-KIT-REF TO KD-KIT-REF
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE Ws-Max-Line TO KA-NEW-LINE-NUM
     MOVE 1 TO KA-LINES-CHANGED
     MOVE KA-PROPOSED-REQ TO KA-NEW-REQ
     MOVE KA-PROPOSED-REQ TO KA-REQ-DELTA
     COMPUTE KA-COST-IMPACT ROUNDED
         = KA-PROPOSED-REQ * KI-STD-COST
     MOVE "Additional approved material requirement added." TO KA-IMPACT-TEXT
     PERFORM Audit-Amendment.

 Find-Next-Line-Number.
     MOVE 0 TO Ws-Max-Line
     MOVE "LINOPEN" TO KD-FUNC
     MOVE "N" TO KD-ONLY-OPEN
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More NOT = "Y"
         MOVE "LINNEXT" TO KD-FUNC
         CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                            KIT-LINE-REC, KIT-ITEM-REC
         IF KD-RC NOT = 0
             MOVE "N" TO Ws-More
         ELSE
             IF KL-LINE-NUM > Ws-Max-Line
                 MOVE KL-LINE-NUM TO Ws-Max-Line
             END-IF
         END-IF
     END-PERFORM
     MOVE "LINCLOS" TO KD-FUNC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     ADD 1 TO Ws-Max-Line.

*>----------------------------------------------------------------
*> CANCELLN - only an UNISSUED line may be cancelled, and then its
*> reservations are released.  KIT-003 and KIT-005: deleting an
*> issued line would return stock and remove cost history, which
*> is exactly what the acceptance obligation forbids.
 Cancel-Unissued-Line.
     PERFORM Load-Kit
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Line
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     IF KL-GROSS-ISSUED > 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0015" TO CA-REASON-CODE
         MOVE "This line has issued material: it cannot be removed, only amended. Its history and cost remain."
              TO CA-MESSAGE
         MOVE "Y" TO KA-HAS-HISTORY
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-Amend-Authority
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     MOVE KL-RESERVED-QTY TO KA-RESV-IMPACT
     MOVE KL-CURRENT-REQ TO KA-OLD-REQ
     COMPUTE KA-COST-IMPACT ROUNDED
         = 0 - (KL-CURRENT-REQ * KL-PLAN-UNIT-COST)
     MOVE 0 TO KL-CURRENT-REQ
     MOVE 0 TO KL-REMAINING-QTY
     MOVE 0 TO KL-RESERVED-QTY
     MOVE "CANCELLED" TO KL-STATUS
     MOVE KA-REASON-TEXT TO KL-DEVIATION
     MOVE CA-USER TO KL-DEV-APPROVER
     PERFORM Save-Line
     MOVE 1 TO KA-LINES-CHANGED
     MOVE "Unissued line cancelled and its reservations released."
          TO KA-IMPACT-TEXT
     PERFORM Audit-Amendment.

*>----------------------------------------------------------------
*> SUBST - KIT-012 and D-11.  An approved equivalent of equal or
*> higher specification only; a customer specified component also
*> needs the customer's written agreement.  The standard bill is
*> never touched, and the substitution is disclosed on the kit,
*> the genealogy and the documents.
 Approve-Substitute.
     PERFORM Load-Kit
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Line
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     EVALUATE TRUE
         WHEN KA-SUBSTITUTE = SPACES
             MOVE 8 TO CA-RETURN-CODE
             MOVE "KIT0014" TO CA-REASON-CODE
             MOVE "A substitute item code is required." TO CA-MESSAGE
         WHEN KA-SUB-SPEC-OK NOT = "Y"
             MOVE 6 TO CA-RETURN-CODE
             MOVE "KIT0014" TO CA-REASON-CODE
             MOVE "Y" TO KA-NEEDS-AUTH
             MOVE "A substitute needs an approved equivalence of equal or higher specification."
                  TO CA-MESSAGE
         WHEN KA-CUST-SPECIFIED = "Y" AND KA-CUST-AGREED NOT = "Y"
             MOVE 6 TO CA-RETURN-CODE
             MOVE "KIT0014" TO CA-REASON-CODE
             MOVE "Y" TO KA-NEEDS-AUTH
             MOVE "A customer specified component cannot be substituted without the customer's written agreement."
                  TO CA-MESSAGE
         WHEN KA-REASON-TEXT = SPACES
             MOVE 8 TO CA-RETURN-CODE
             MOVE "KIT0008" TO CA-REASON-CODE
             MOVE "A reason is required to approve a substitution."
                  TO CA-MESSAGE
         WHEN OTHER
             PERFORM Record-Substitute
     END-EVALUATE.

 Record-Substitute.
     IF KA-SUB-RATIO <= 0
         MOVE 1 TO KA-SUB-RATIO
     END-IF
     MOVE KA-SUBSTITUTE TO KL-SUBSTITUTE
     MOVE KA-APPROVAL-ID TO KL-SUB-APPROVAL
     MOVE KA-REASON-TEXT TO KL-DEVIATION
     MOVE CA-USER TO KL-DEV-APPROVER
     COMPUTE KL-CURRENT-REQ ROUNDED
         = KL-CURRENT-REQ * KA-SUB-RATIO
     COMPUTE KL-REMAINING-QTY = KL-CURRENT-REQ - KL-NET-ISSUED
     IF KL-REMAINING-QTY < 0
         MOVE 0 TO KL-REMAINING-QTY
     END-IF
     PERFORM Save-Line
     MOVE 1 TO KA-LINES-CHANGED
     MOVE KL-CURRENT-REQ TO KA-NEW-REQ
     MOVE KL-REMAINING-QTY TO KA-REMAINING
     MOVE SPACES TO KA-IMPACT-TEXT
     STRING "Substitute " FUNCTION TRIM(KA-SUBSTITUTE)
            " approved for this kit only; the standard bill of material is unchanged."
         DELIMITED BY SIZE INTO KA-IMPACT-TEXT
     PERFORM Audit-Amendment.
