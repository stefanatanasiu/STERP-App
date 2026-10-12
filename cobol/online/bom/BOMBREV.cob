>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : BOMBREV
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : Build ONE proposed bill revision for ONE bulk
*>              change target and say whether it would be valid.
*>
*>              BOM-024 is two rules wearing one coat.  The first
*>              is that a released revision is never written to:
*>              it gets a new draft revision beside it and the
*>              released content stays exactly as it was.  The
*>              second is that the proposal must be validated as a
*>              whole bill and not as a clever diff - a
*>              replacement can close a loop, orphan an alternate
*>              group or leave a bill with no material at all, and
*>              only a full validation of the result finds that.
*>
*>              So this program really does build the revision,
*>              every time, even for a preview.  BOMBULK wraps the
*>              run in BEGN / COMT and throws the work away when
*>              the engineer only asked to look, which is why
*>              BOMBREV is not LINKable and owns no transaction.
*> CALLED BY  : BOMBULK only
*> INTERFACE  : CALL "BOMBREV" USING BOM-BR-CTL BOM-BR-MSG
*>              COPYBOOK BOMBREVC
*> CALLS      : SQLIO, BOMREAD, CYCLECHK, BOMVALID
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2016-03-21 AJS  CR2442 SPLIT OUT OF BOMBULK (SIZE)
*> 2017-02-28 AJS  CR2601 RELEASED TARGETS GET A NEW REVISION
*> 2026-10-06 STERP FULL VALIDATION OF THE PROPOSED REVISION
*>================================================================
*> REQ: BOM-004, BOM-013, BOM-018, BOM-024, BOM-029
 IDENTIFICATION DIVISION.
 PROGRAM-ID. BOMBREV.
 AUTHOR. A J SPENCER.
 DATE-WRITTEN. 21/03/2016.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY BOMBILL.
 COPY CYCLECHKCA REPLACING ==DFHCOMMAREA== BY ==Ws-Cyc-Ca==
      LEADING ==CA-== BY ==YA-==
      LEADING ==CC-== BY ==YC-==.
 COPY BOMVALIDCA REPLACING ==DFHCOMMAREA== BY ==Ws-Val-Ca==
      LEADING ==CA-== BY ==VA-==
      LEADING ==BV-== BY ==VV-==.
 COPY WHEREUSDCA REPLACING ==DFHCOMMAREA== BY ==Ws-Wu-Ca==
      LEADING ==CA-== BY ==UA-==.
 01  Ws-Row.
     05  Ws-Col OCCURS 6 TIMES   PIC X(60).
 01  Ws-Alphabet                 PIC X(26)
                                 VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
 01  Ws-Work.
     05  Ws-I                    PIC S9(4) VALUE 0.
     05  Ws-N                    PIC S9(4) VALUE 0.
     05  Ws-Hit                  PIC S9(4) VALUE 0.
     05  Ws-Free                 PIC X VALUE "N".
     05  Ws-Try-Rev              PIC X(4) VALUE SPACES.
     05  Ws-Ref20                PIC X(20) VALUE SPACES.
     05  Ws-Line-Edit            PIC Z(3)9.
     05  Ws-Seq-Edit             PIC Z(3)9.
     05  Ws-Path-Ref             PIC X(20) VALUE SPACES.
     05  Ws-Found                PIC X VALUE "N".
     05  Ws-Cycle-Path           PIC X(120) VALUE SPACES.
     05  Ws-Note                 PIC X(100) VALUE SPACES.
 01  Ws-Was.
     05  Ws-Old-Uom              PIC X(10) VALUE SPACES.
     05  Ws-Old-Basis            PIC X(12) VALUE SPACES.
     05  Ws-Old-Scrap            PIC S9(5)V9(6) VALUE 0.
     05  Ws-Use-Uom              PIC X(10) VALUE SPACES.
     05  Ws-Use-Basis            PIC X(12) VALUE SPACES.
     05  Ws-Use-Qty              PIC S9(13)V9(6) VALUE 0.
     05  Ws-Use-Scrap            PIC S9(5)V9(6) VALUE 0.
 LINKAGE SECTION.
 COPY BOMBREVC.
 PROCEDURE DIVISION USING BOM-BR-CTL BOM-BR-MSG.
 Main-Entry.
     MOVE SPACES TO BOM-BR-MSG Ws-Note
     MOVE SPACES TO PR-REASON PR-TREATMENT PR-NEW-REV
     MOVE SPACES TO PR-BOM-REF PR-BOM-REV PR-PARENT PR-SRC-STATUS
     MOVE SPACES TO Ws-Old-Uom Ws-Old-Basis
     MOVE 0 TO Ws-Old-Scrap
     MOVE 0 TO PR-NEW-BOM-ID PR-LINE-NUM PR-OLD-QTY PR-RESULT-QTY
     MOVE 0 TO PR-ERROR-CNT PR-WARNING-CNT
     MOVE "N" TO PR-SQL-DEAD
     MOVE "SKIPPED" TO PR-OUTCOME
     MOVE FUNCTION UPPER-CASE(PR-ACTION) TO PR-ACTION
     EVALUATE PR-FUNC
         WHEN "PROPOSE" PERFORM Propose-One-Revision
                            THRU Propose-Exit
         WHEN OTHER     PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE "FAILED" TO PR-OUTCOME
     MOVE "BOM1430" TO PR-REASON
     MOVE "BOMBREV only supports the PROPOSE function"
         TO BOM-BR-MSG.

*>----------------------------------------------------------------
*> The order below is deliberate: everything that can refuse a
*> target without touching the database is tested first, so a
*> preview of two hundred bills does not write two hundred draft
*> headers before noticing the replacement item is withdrawn.
*>----------------------------------------------------------------
 Propose-One-Revision.
     PERFORM Read-The-Source-Bill
     IF PR-REASON NOT = SPACES GO TO Propose-Exit END-IF
     PERFORM Test-The-Status
     IF PR-REASON NOT = SPACES GO TO Propose-Exit END-IF
     PERFORM Find-The-Affected-Line
     IF PR-REASON NOT = SPACES GO TO Propose-Exit END-IF
     PERFORM Test-The-New-Item
     IF PR-REASON NOT = SPACES GO TO Propose-Exit END-IF
     PERFORM Test-For-A-New-Cycle
     IF PR-REASON NOT = SPACES GO TO Propose-Exit END-IF
     PERFORM Decide-The-Treatment
     IF PR-REASON NOT = SPACES GO TO Propose-Exit END-IF
     IF PR-TREATMENT = "NEWREV"
         PERFORM Create-The-New-Header
         IF PR-REASON NOT = SPACES GO TO Propose-Exit END-IF
         PERFORM Read-Back-The-New-Id
         IF PR-REASON NOT = SPACES GO TO Propose-Exit END-IF
         PERFORM Copy-The-Unchanged-Lines
         IF PR-REASON NOT = SPACES GO TO Propose-Exit END-IF
     ELSE
         MOVE PR-BOM-ID TO PR-NEW-BOM-ID
         MOVE PR-BOM-REV TO PR-NEW-REV
     END-IF
     PERFORM Apply-The-Delta
     IF PR-REASON NOT = SPACES GO TO Propose-Exit END-IF
     PERFORM Validate-The-Proposal.
 Propose-Exit.
     EXIT.

*>----------------------------------------------------------------
 Read-The-Source-Bill.
     INITIALIZE BOM-RD-CTL
     MOVE "BILL" TO BR-FUNC
     MOVE PR-COMPANY TO BR-COMPANY
     MOVE PR-SITE TO BR-SITE
     MOVE PR-BOM-ID TO BR-BOM-ID
     MOVE PR-EFF-DATE TO BR-EFF-DATE
     MOVE 500 TO BR-MAX-LINES
     CALL "BOMREAD" USING BOM-RD-CTL BOM-HDR-REC BOM-CMP-TAB
     IF BR-STATUS NOT = "OK" AND BR-STATUS NOT = "TOOMANY"
         MOVE "FAILED" TO PR-OUTCOME
         MOVE "BOM1443" TO PR-REASON
         MOVE BR-MESSAGE TO BOM-BR-MSG
         IF BR-STATUS = "SQLFAIL"
             MOVE "Y" TO PR-SQL-DEAD
         END-IF
         EXIT PARAGRAPH
     END-IF
     MOVE BH-BOM-REF TO PR-BOM-REF
     MOVE BH-BOM-REV TO PR-BOM-REV
     MOVE BH-PARENT-ITEM TO PR-PARENT
     MOVE BH-STATUS TO PR-SRC-STATUS.

*> A retired or superseded revision is history.  BOM-028 keeps
*> history reproducible, so a bulk change leaves it alone and says
*> so rather than quietly including it.
 Test-The-Status.
     IF PR-SRC-STATUS = "RETIRED" OR PR-SRC-STATUS = "SUPERSEDED"
         MOVE "SKIPPED" TO PR-OUTCOME
         MOVE "BOM1433" TO PR-REASON
         MOVE SPACES TO BOM-BR-MSG
         STRING "Revision " FUNCTION TRIM(PR-BOM-REV) " of "
                FUNCTION TRIM(PR-BOM-REF) " is "
                FUNCTION TRIM(PR-SRC-STATUS)
                " and is left as history"
             DELIMITED BY SIZE INTO BOM-BR-MSG
     END-IF.

*> REQ: BOM-002 - repeated component lines stay distinguishable,
*> so a bulk change touches the FIRST occurrence of the item and
*> says that the others are still there.  Changing all of them
*> silently has cost an engineering department a day before now.
 Find-The-Affected-Line.
     MOVE 0 TO Ws-Hit PR-LINE-NUM
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > BR-LINE-COUNT
         IF PR-ACTION = "ADD"
             IF BC-ITEM(Ws-I) = PR-NEW-ITEM
                 ADD 1 TO Ws-Hit
             END-IF
         ELSE
             IF BC-ITEM(Ws-I) = PR-OLD-ITEM
                 ADD 1 TO Ws-Hit
                 IF PR-LINE-NUM = 0
                     MOVE BC-LINE-NUM(Ws-I) TO PR-LINE-NUM
                     MOVE BC-USAGE-QTY(Ws-I) TO PR-OLD-QTY
                     MOVE BC-UOM(Ws-I) TO Ws-Old-Uom
                     MOVE BC-USAGE-BASIS(Ws-I) TO Ws-Old-Basis
                     MOVE BC-SCRAP-PCT(Ws-I) TO Ws-Old-Scrap
                 END-IF
             END-IF
         END-IF
     END-PERFORM
     IF PR-ACTION = "ADD"
         IF Ws-Hit > 0
             MOVE "SKIPPED" TO PR-OUTCOME
             MOVE "BOM1432" TO PR-REASON
             MOVE SPACES TO BOM-BR-MSG
             STRING FUNCTION TRIM(PR-NEW-ITEM) " is already on "
                    FUNCTION TRIM(PR-BOM-REF) "/"
                    FUNCTION TRIM(PR-BOM-REV) " - nothing to add"
                 DELIMITED BY SIZE INTO BOM-BR-MSG
         END-IF
         EXIT PARAGRAPH
     END-IF
     IF PR-LINE-NUM = 0
         MOVE "SKIPPED" TO PR-OUTCOME
         MOVE "BOM1431" TO PR-REASON
         MOVE SPACES TO BOM-BR-MSG
         STRING FUNCTION TRIM(PR-OLD-ITEM)
                " is not a component of "
                FUNCTION TRIM(PR-BOM-REF) "/"
                FUNCTION TRIM(PR-BOM-REV) " - nothing to change"
             DELIMITED BY SIZE INTO BOM-BR-MSG
         EXIT PARAGRAPH
     END-IF
     IF Ws-Hit > 1
         MOVE Ws-Hit TO Ws-Line-Edit
         MOVE SPACES TO Ws-Note
         STRING FUNCTION TRIM(Ws-Line-Edit)
                " occurrences of this component exist; only the"
                " first is changed"
             DELIMITED BY SIZE INTO Ws-Note
     END-IF.

*> BOM-010 / BOM-029: a replacement that names an item the stock
*> ledger has never heard of must refuse, not default.
 Test-The-New-Item.
     IF PR-ACTION NOT = "REPLACE" AND PR-ACTION NOT = "ADD"
         EXIT PARAGRAPH
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT i.ItemCode, ISNULL(i.StockUomCode,'') "
            "FROM inv.Item i WHERE i.CompanyCode = ? "
            "AND i.ItemCode = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 2 TO SQLIO-PARM-CNT
     MOVE PR-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE PR-NEW-ITEM TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE 2 TO SQLIO-COL-CNT
     MOVE 60 TO SQLIO-COL-WIDTH(1)
     MOVE 60 TO SQLIO-COL-WIDTH(2)
     MOVE 2 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     IF SQLIO-NOTFOUND
         MOVE "FAILED" TO PR-OUTCOME
         MOVE "BOM1435" TO PR-REASON
         MOVE SPACES TO BOM-BR-MSG
         STRING "Item " FUNCTION TRIM(PR-NEW-ITEM)
                " is not held in the item file for company "
                FUNCTION TRIM(PR-COMPANY)
             DELIMITED BY SIZE INTO BOM-BR-MSG
         EXIT PARAGRAPH
     END-IF
     IF Ws-Old-Uom = SPACES
         MOVE Ws-Col(2) TO Ws-Old-Uom
     END-IF.

*> REQ: BOM-013 - a replacement is the classic way an engineer
*> closes a loop by accident, because the new component is an
*> assembly that already sits above the bill being changed.
*> Two questions are asked.  CYCLECHK answers "is this item below
*> itself", which catches a direct self use and a loop inside the
*> new component's own structure.  The where-used engine answers
*> the one that matters here - "is the new component already an
*> ancestor of this bill's parent" - because the proposed edge
*> does not exist yet and so cannot be walked.
 Test-For-A-New-Cycle.
     IF PR-ACTION NOT = "REPLACE" AND PR-ACTION NOT = "ADD"
         EXIT PARAGRAPH
     END-IF
     INITIALIZE Ws-Cyc-Ca
     MOVE "EDGE" TO YA-FUNCTION
     MOVE PR-COMPANY TO YA-COMPANY
     MOVE PR-SITE TO YA-SITE
     MOVE PR-USER TO YA-USER
     MOVE PR-EFF-DATE TO YA-BUSINESS-DATE
     MOVE PR-PARENT TO YC-ROOT-ITEM
     MOVE PR-NEW-ITEM TO YC-CHILD-ITEM
     MOVE PR-EFF-DATE TO YC-EFF-DATE
     MOVE "STANDARD" TO YC-PURPOSE
     MOVE 25 TO YC-MAX-DEPTH
     MOVE "Y" TO YC-INCLUDE-OPT
     CALL "CYCLECHK" USING Ws-Cyc-Ca
     IF YC-CYCLE-FOUND = "Y"
         MOVE "FAILED" TO PR-OUTCOME
         MOVE "BOM1434" TO PR-REASON
         MOVE SPACES TO BOM-BR-MSG
         STRING "Cycle refused: " YC-CYCLE-PATH(1:105)
             DELIMITED BY SIZE INTO BOM-BR-MSG
         EXIT PARAGRAPH
     END-IF
     PERFORM Test-The-Ancestry.

 Test-The-Ancestry.
     MOVE "N" TO Ws-Found
     INITIALIZE Ws-Wu-Ca
     MOVE "ALL" TO UA-FUNCTION
     MOVE PR-COMPANY TO UA-COMPANY
     MOVE PR-SITE TO UA-SITE
     MOVE PR-USER TO UA-USER
     MOVE PR-EFF-DATE TO UA-BUSINESS-DATE
     MOVE PR-PARENT TO WU-ITEM
     MOVE PR-EFF-DATE TO WU-EFF-DATE
     MOVE "STANDARD" TO WU-PURPOSE
     MOVE 25 TO WU-MAX-DEPTH
     MOVE "Y" TO WU-INCLUDE-DRAFT
     MOVE "Y" TO WU-INCLUDE-OPT
     MOVE 1 TO WU-FROM-LINE
     MOVE 25 TO WU-WIN-MAX
     CALL "WHEREUSD" USING Ws-Wu-Ca
     PERFORM Scan-One-Window
     PERFORM UNTIL WU-MORE-LINES NOT = "Y" OR Ws-Found = "Y"
         MOVE "GETLINE" TO UA-FUNCTION
         CALL "WHEREUSD" USING Ws-Wu-Ca
         IF UA-RETURN-CODE > 7
             EXIT PERFORM
         END-IF
         PERFORM Scan-One-Window
     END-PERFORM
     IF Ws-Found NOT = "Y"
         EXIT PARAGRAPH
     END-IF
     MOVE "FAILED" TO PR-OUTCOME
     MOVE "BOM1434" TO PR-REASON
     MOVE SPACES TO BOM-BR-MSG
     STRING "Cycle refused: " FUNCTION TRIM(PR-NEW-ITEM)
            " is already above " FUNCTION TRIM(PR-PARENT)
            " - " Ws-Cycle-Path(1:60)
         DELIMITED BY SIZE INTO BOM-BR-MSG.

 Scan-One-Window.
     PERFORM VARYING Ws-I FROM 1 BY 1
             UNTIL Ws-I > WU-WIN-CNT OR Ws-Found = "Y"
         IF WU-L-PARENT(Ws-I) = PR-NEW-ITEM
             MOVE "Y" TO Ws-Found
             MOVE WU-L-PATH(Ws-I) TO Ws-Cycle-Path
         END-IF
     END-PERFORM.

*> BOM-004.  A released revision is immutable, so it is copied to
*> the next free revision letter and the copy carries the change.
*> A draft is the engineer's own working paper and is amended.
 Decide-The-Treatment.
     IF PR-SRC-STATUS = "DRAFT" OR PR-SRC-STATUS = "REVIEW"
         MOVE "AMEND" TO PR-TREATMENT
         EXIT PARAGRAPH
     END-IF
     MOVE "NEWREV" TO PR-TREATMENT
     PERFORM Pick-A-Free-Revision
     IF PR-NEW-REV = SPACES AND PR-REASON = SPACES
         MOVE "FAILED" TO PR-OUTCOME
         MOVE "BOM1436" TO PR-REASON
         MOVE SPACES TO BOM-BR-MSG
         STRING "Every revision letter of "
                FUNCTION TRIM(PR-BOM-REF)
                " is used - no proposal can be raised"
             DELIMITED BY SIZE INTO BOM-BR-MSG
     END-IF.

*> Twenty six probes is not elegant.  It is, however, exactly how
*> the 1996 program did it, and it keeps the revision letters in
*> the order engineering reads them.
 Pick-A-Free-Revision.
     MOVE SPACES TO PR-NEW-REV
     PERFORM VARYING Ws-N FROM 1 BY 1
             UNTIL Ws-N > 26 OR PR-NEW-REV NOT = SPACES
                OR PR-REASON NOT = SPACES
         MOVE SPACES TO Ws-Try-Rev
         MOVE Ws-Alphabet(Ws-N:1) TO Ws-Try-Rev(1:1)
         PERFORM Look-For-That-Revision
         IF Ws-Free = "Y"
             MOVE Ws-Try-Rev TO PR-NEW-REV
         END-IF
     END-PERFORM.

*> Doubles as the read-back of a revision just inserted: when the
*> row is there, Ws-Col(1) holds its identity.
 Look-For-That-Revision.
     MOVE "N" TO Ws-Free
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT h.BomHeaderId FROM bom.BomHeader h WHERE "
            "h.CompanyCode = ? AND h.BomRef = ? "
            "AND h.BomRevision = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 3 TO SQLIO-PARM-CNT
     MOVE PR-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE PR-BOM-REF TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE Ws-Try-Rev TO SQLIO-PARM-VAL(3)
     MOVE "N" TO SQLIO-PARM-NULL(3)
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 60 TO SQLIO-COL-WIDTH(1)
     MOVE 2 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     IF SQLIO-NOTFOUND
         MOVE "Y" TO Ws-Free
     END-IF.

*>----------------------------------------------------------------
*> The new header is a straight copy of the old apart from its
*> revision, its effectivity and its status.  It is written with
*> INSERT ... SELECT so a column added to bom.BomHeader by a later
*> change request is carried across without being retyped into a
*> parameter list for the third time.
 Create-The-New-Header.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT INTO bom.BomHeader (CompanyCode, SiteCode, "
            "BomRef, BomRevision, ParentItemCode, ParentRevision, "
            "Purpose, JobRef, VariantCode, Description, "
            "NominalBasis, YieldFactor, ProductionQuantum, "
            "BatchSize, IsPhantom, ConsolidateRuns, CostingLotSize,"
            " SetupCostPerRun, ConversionCostPerStart, "
            "EffectiveFrom, Status, AuthorUser, ChangeRef, "
            "ChangeReason, LossIntentNote, SourceBomHeaderId, "
            "CreatedBy) SELECT h.CompanyCode, h.SiteCode, "
            "h.BomRef, ?, h.ParentItemCode, h.ParentRevision, "
            "h.Purpose, h.JobRef, h.VariantCode, h.Description, "
            "h.NominalBasis, h.YieldFactor, h.ProductionQuantum, "
            "h.BatchSize, h.IsPhantom, h.ConsolidateRuns, "
            "h.CostingLotSize, h.SetupCostPerRun, "
            "h.ConversionCostPerStart, CONVERT(date,?,112), "
            "'DRAFT', ?, ?, ?, h.LossIntentNote, h.BomHeaderId, ? "
            "FROM bom.BomHeader h WHERE h.BomHeaderId = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 7 TO SQLIO-PARM-CNT
     PERFORM Clear-The-Parameters
     MOVE PR-NEW-REV TO SQLIO-PARM-VAL(1)
     MOVE PR-EFF-DATE TO SQLIO-PARM-VAL(2)
     MOVE PR-USER TO SQLIO-PARM-VAL(3)
     MOVE PR-CHANGE-REF TO SQLIO-PARM-VAL(4)
     STRING "Bulk " FUNCTION TRIM(PR-ACTION) " under change "
            FUNCTION TRIM(PR-CHANGE-REF)
         DELIMITED BY SIZE INTO SQLIO-PARM-VAL(5)
     MOVE PR-USER TO SQLIO-PARM-VAL(6)
     MOVE PR-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(7)
     PERFORM Run-The-Statement.

 Read-Back-The-New-Id.
     MOVE 0 TO PR-NEW-BOM-ID
     MOVE PR-NEW-REV TO Ws-Try-Rev
     PERFORM Look-For-That-Revision
     IF PR-REASON NOT = SPACES
         EXIT PARAGRAPH
     END-IF
     IF Ws-Free = "Y"
         MOVE "FAILED" TO PR-OUTCOME
         MOVE "BOM1437" TO PR-REASON
         MOVE "The proposed revision could not be read back"
             TO BOM-BR-MSG
         EXIT PARAGRAPH
     END-IF
     COMPUTE PR-NEW-BOM-ID = FUNCTION NUMVAL(Ws-Col(1)).

*> REMOVE drops its line here by simply not copying it; every
*> other action copies the lot and edits one line afterwards.
 Copy-The-Unchanged-Lines.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT INTO bom.BomComponent (BomHeaderId, LineNum, "
            "PathRef, ComponentItemCode, ComponentRevision, "
            "UsageQty, UsageBasis, UomCode, ConversionFactor, "
            "ScrapPct, SupplyBehaviour, IsOptional, "
            "AlternateGroup, IsSelected, OptionChoiceRef, "
            "EffectiveFrom, EffectiveTo, NeedOffsetDays, "
            "OperationRef, IssueLocationCode, QualityRestriction, "
            "Notes, CostOnlyRate, CostElementCode, CreatedBy) "
            "SELECT ?, c.LineNum, c.PathRef, c.ComponentItemCode, "
            "c.ComponentRevision, c.UsageQty, c.UsageBasis, "
            "c.UomCode, c.ConversionFactor, c.ScrapPct, "
            "c.SupplyBehaviour, c.IsOptional, c.AlternateGroup, "
            "c.IsSelected, c.OptionChoiceRef, c.EffectiveFrom, "
            "c.EffectiveTo, c.NeedOffsetDays, c.OperationRef, "
            "c.IssueLocationCode, c.QualityRestriction, c.Notes, "
            "c.CostOnlyRate, c.CostElementCode, ? FROM "
            "bom.BomComponent c WHERE c.BomHeaderId = ? "
            "AND (? = 'N' OR c.LineNum <> ?)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 5 TO SQLIO-PARM-CNT
     PERFORM Clear-The-Parameters
     MOVE PR-NEW-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
     MOVE PR-USER TO SQLIO-PARM-VAL(2)
     MOVE PR-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
     IF PR-ACTION = "REMOVE"
         MOVE "Y" TO SQLIO-PARM-VAL(4)
     ELSE
         MOVE "N" TO SQLIO-PARM-VAL(4)
     END-IF
     MOVE PR-LINE-NUM TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
     PERFORM Run-The-Statement.

*>----------------------------------------------------------------
*> Every "keep what was there unless the request names something
*> else" decision is made here in COBOL, so the UPDATE statements
*> carry plain values.  Deciding it inside the SQL with CASE WHEN
*> ? ... would make the gateway convert a parameter twice.
 Apply-The-Delta.
     MOVE Ws-Old-Uom TO Ws-Use-Uom
     MOVE Ws-Old-Basis TO Ws-Use-Basis
     MOVE PR-OLD-QTY TO Ws-Use-Qty
     MOVE Ws-Old-Scrap TO Ws-Use-Scrap
     IF PR-NEW-UOM NOT = SPACES
         MOVE PR-NEW-UOM TO Ws-Use-Uom
     END-IF
     IF PR-NEW-BASIS NOT = SPACES
         MOVE PR-NEW-BASIS TO Ws-Use-Basis
     END-IF
     IF PR-NEW-QTY > 0
         MOVE PR-NEW-QTY TO Ws-Use-Qty
     END-IF
     IF PR-NEW-SCRAP >= 0
         MOVE PR-NEW-SCRAP TO Ws-Use-Scrap
     END-IF
     IF Ws-Use-Basis = SPACES
         MOVE "PROPORTION" TO Ws-Use-Basis
     END-IF
     EVALUATE PR-ACTION
         WHEN "REPLACE" PERFORM Swap-The-Component
         WHEN "QTYCHG"  PERFORM Change-The-Quantity
         WHEN "ADD"     PERFORM Add-The-Component
         WHEN "REMOVE"  PERFORM Drop-The-Component
         WHEN OTHER
             MOVE "FAILED" TO PR-OUTCOME
             MOVE "BOM1444" TO PR-REASON
             MOVE "Unsupported bulk change action" TO BOM-BR-MSG
     END-EVALUATE.

 Swap-The-Component.
     MOVE SPACES TO SQLIO-STMT
     STRING "UPDATE bom.BomComponent SET ComponentItemCode = ?, "
            "ComponentRevision = NULL, UsageQty = ?, UomCode = ?, "
            "Notes = ?, ModifiedBy = ?, ModifiedAt = SYSDATETIME()"
            " WHERE BomHeaderId = ? AND LineNum = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 7 TO SQLIO-PARM-CNT
     PERFORM Clear-The-Parameters
     MOVE PR-NEW-ITEM TO SQLIO-PARM-VAL(1)
     MOVE Ws-Use-Qty TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
     MOVE Ws-Use-Uom TO SQLIO-PARM-VAL(3)
     STRING FUNCTION TRIM(PR-OLD-ITEM) " replaced by "
            FUNCTION TRIM(PR-NEW-ITEM) " under "
            FUNCTION TRIM(PR-CHANGE-REF)
         DELIMITED BY SIZE INTO SQLIO-PARM-VAL(4)
     MOVE PR-USER TO SQLIO-PARM-VAL(5)
     MOVE 5 TO Ws-N
     PERFORM Bind-Bill-And-Line
     MOVE Ws-Use-Qty TO PR-RESULT-QTY.

*> BOM-009: the scrap allowance is an additional input fraction,
*> so a change of it is a quantity change like any other and the
*> two travel together on one request.
 Change-The-Quantity.
     IF PR-NEW-QTY <= 0 AND PR-NEW-SCRAP < 0
         MOVE "FAILED" TO PR-OUTCOME
         MOVE "BOM1442" TO PR-REASON
         MOVE "A quantity change needs a new quantity or a new scrap allowance"
             TO BOM-BR-MSG
         EXIT PARAGRAPH
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "UPDATE bom.BomComponent SET UsageQty = ?, "
            "ScrapPct = ?, UsageBasis = ?, UomCode = ?, "
            "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
            "WHERE BomHeaderId = ? AND LineNum = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 7 TO SQLIO-PARM-CNT
     PERFORM Clear-The-Parameters
     MOVE Ws-Use-Qty TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
     MOVE Ws-Use-Scrap TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
     MOVE Ws-Use-Basis TO SQLIO-PARM-VAL(3)
     MOVE Ws-Use-Uom TO SQLIO-PARM-VAL(4)
     MOVE PR-USER TO SQLIO-PARM-VAL(5)
     MOVE 5 TO Ws-N
     PERFORM Bind-Bill-And-Line
     MOVE Ws-Use-Qty TO PR-RESULT-QTY.

 Add-The-Component.
     PERFORM Next-Free-Line-Number
     IF PR-REASON NOT = SPACES
         EXIT PARAGRAPH
     END-IF
     IF Ws-Use-Qty <= 0
         MOVE 1 TO Ws-Use-Qty
     END-IF
     MOVE PR-LINE-NUM TO Ws-Line-Edit
     MOVE SPACES TO Ws-Path-Ref
     STRING FUNCTION TRIM(PR-BOM-REF) "/"
            FUNCTION TRIM(Ws-Line-Edit)
         DELIMITED BY SIZE INTO Ws-Path-Ref
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT INTO bom.BomComponent (BomHeaderId, LineNum, "
            "PathRef, ComponentItemCode, UsageQty, UsageBasis, "
            "UomCode, ConversionFactor, ScrapPct, "
            "SupplyBehaviour, IsOptional, IsSelected, Notes, "
            "CreatedBy) VALUES (?,?,?,?,?,?,?,1,?,"
            "'PURCHASED',0,1,?,?)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 10 TO SQLIO-PARM-CNT
     PERFORM Clear-The-Parameters
     MOVE PR-NEW-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
     MOVE PR-LINE-NUM TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
     MOVE Ws-Path-Ref TO SQLIO-PARM-VAL(3)
     MOVE PR-NEW-ITEM TO SQLIO-PARM-VAL(4)
     MOVE Ws-Use-Qty TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
     MOVE Ws-Use-Basis TO SQLIO-PARM-VAL(6)
     MOVE Ws-Use-Uom TO SQLIO-PARM-VAL(7)
     MOVE Ws-Use-Scrap TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
     STRING "Added by bulk change "
            FUNCTION TRIM(PR-CHANGE-REF)
         DELIMITED BY SIZE INTO SQLIO-PARM-VAL(9)
     MOVE PR-USER TO SQLIO-PARM-VAL(10)
     PERFORM Run-The-Statement
     MOVE 0 TO PR-OLD-QTY
     MOVE Ws-Use-Qty TO PR-RESULT-QTY.

*> On a new revision the line was never copied, so there is
*> nothing left to do.  On an amended draft the row is deleted.
 Drop-The-Component.
     MOVE 0 TO PR-RESULT-QTY
     IF PR-TREATMENT = "NEWREV"
         EXIT PARAGRAPH
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE FROM bom.BomComponent WHERE BomHeaderId = ? "
            "AND LineNum = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 2 TO SQLIO-PARM-CNT
     PERFORM Clear-The-Parameters
     MOVE 0 TO Ws-N
     PERFORM Bind-Bill-And-Line.

 Next-Free-Line-Number.
     IF PR-NEW-LINE-NUM > 0
         MOVE PR-NEW-LINE-NUM TO PR-LINE-NUM
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO PR-LINE-NUM
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT ISNULL(MAX(c.LineNum),0) + 10 FROM "
            "bom.BomComponent c WHERE c.BomHeaderId = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 1 TO SQLIO-PARM-CNT
     PERFORM Clear-The-Parameters
     MOVE PR-NEW-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 60 TO SQLIO-COL-WIDTH(1)
     MOVE 2 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     COMPUTE PR-LINE-NUM = FUNCTION NUMVAL(Ws-Col(1)).

 Clear-The-Parameters.
     PERFORM VARYING Ws-I FROM 1 BY 1
             UNTIL Ws-I > SQLIO-PARM-CNT
         MOVE SPACES TO SQLIO-PARM-VAL(Ws-I)
         MOVE "N" TO SQLIO-PARM-NULL(Ws-I)
     END-PERFORM.

*> The last two parameters of every delta statement are the bill
*> and the line, so they are bound in one place.  Ws-N carries how
*> many parameters the caller has already filled in.
 Bind-Bill-And-Line.
     ADD 1 TO Ws-N
     MOVE PR-NEW-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(Ws-N)
     MOVE "N" TO SQLIO-PARM-NULL(Ws-N)
     ADD 1 TO Ws-N
     MOVE PR-LINE-NUM TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(Ws-N)
     MOVE "N" TO SQLIO-PARM-NULL(Ws-N)
     MOVE Ws-N TO SQLIO-PARM-CNT
     PERFORM Run-The-Statement.

 Run-The-Statement.
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 2 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
     END-IF
     MOVE 2 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row.

*>----------------------------------------------------------------
*> REQ: BOM-024, BOM-029.  The proposal is validated as a whole
*> bill.  PR-RECORD-VALID = "Y" keeps the evidence in
*> bom.BomValidation (an APPLY run); "N" only reads the answer
*> back, because a preview must leave no trace.
 Validate-The-Proposal.
     INITIALIZE Ws-Val-Ca
     IF PR-RECORD-VALID = "Y"
         MOVE "VALIDATE" TO VA-FUNCTION
     ELSE
         MOVE "CHECK" TO VA-FUNCTION
     END-IF
     MOVE PR-COMPANY TO VA-COMPANY
     MOVE PR-SITE TO VA-SITE
     MOVE PR-USER TO VA-USER
     MOVE PR-EFF-DATE TO VA-BUSINESS-DATE
     MOVE PR-NEW-BOM-ID TO VV-BOM-ID
     MOVE PR-EFF-DATE TO VV-EFF-DATE
     MOVE PR-CHANGE-REF TO Ws-Ref20
     MOVE PR-SEQ TO Ws-Seq-Edit
     MOVE SPACES TO VV-VALID-REF
     STRING FUNCTION TRIM(Ws-Ref20) "/"
            FUNCTION TRIM(Ws-Seq-Edit) "/"
            FUNCTION TRIM(PR-NEW-REV)
         DELIMITED BY SIZE INTO VV-VALID-REF
     CALL "BOMVALID" USING Ws-Val-Ca
     MOVE VV-ERROR-CNT TO PR-ERROR-CNT
     MOVE VV-WARNING-CNT TO PR-WARNING-CNT
     IF VA-RETURN-CODE = 12
         PERFORM Report-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     IF VV-ERROR-CNT > 0 OR VV-OUTCOME = "FAIL"
         MOVE "FAILED" TO PR-OUTCOME
         MOVE "BOM1440" TO PR-REASON
         MOVE SPACES TO BOM-BR-MSG
         IF VV-FIND-CNT > 0
             IF VV-FIND-CNT > 1
                 STRING VV-F-MESSAGE(1)(1:45)
                        " [" FUNCTION TRIM(VV-F-REASON(1)) "]; "
                        VV-F-MESSAGE(2)(1:45)
                        " [" FUNCTION TRIM(VV-F-REASON(2)) "]"
                     DELIMITED BY SIZE INTO BOM-BR-MSG
             ELSE
                 STRING "Proposed revision "
                        FUNCTION TRIM(PR-NEW-REV)
                        " fails validation: "
                        VV-F-MESSAGE(1)(1:60)
                        " [" FUNCTION TRIM(VV-F-REASON(1)) "]"
                     DELIMITED BY SIZE INTO BOM-BR-MSG
             END-IF
         ELSE
             MOVE VA-MESSAGE TO BOM-BR-MSG
         END-IF
         EXIT PARAGRAPH
     END-IF
     MOVE "APPLIED" TO PR-OUTCOME
     MOVE SPACES TO PR-REASON BOM-BR-MSG
     IF PR-TREATMENT = "NEWREV"
         STRING "New draft revision "
                FUNCTION TRIM(PR-NEW-REV) " of "
                FUNCTION TRIM(PR-BOM-REF)
                "; the released revision is untouched. "
                FUNCTION TRIM(Ws-Note)
             DELIMITED BY SIZE INTO BOM-BR-MSG
     ELSE
         STRING "Draft revision "
                FUNCTION TRIM(PR-BOM-REV) " of "
                FUNCTION TRIM(PR-BOM-REF) " amended in place. "
                FUNCTION TRIM(Ws-Note)
             DELIMITED BY SIZE INTO BOM-BR-MSG
     END-IF.

 Report-Sql-Failure.
     MOVE "FAILED" TO PR-OUTCOME
     MOVE "BOM1441" TO PR-REASON
     MOVE "Y" TO PR-SQL-DEAD
     MOVE SPACES TO BOM-BR-MSG
     STRING "Bill database access failed - " SQLIO-ERRMSG(1:85)
         DELIMITED BY SIZE INTO BOM-BR-MSG.
