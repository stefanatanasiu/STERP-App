>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : BOMBULK
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : Bulk component change across many bills: replace
*>              one component with another, add a component,
*>              remove one, or change the usage quantity, scrap
*>              allowance, unit or basis of one.
*>
*>              BOM-024 insists on three things and this program
*>              exists to make all three impossible to avoid: the
*>              engineer sees a complete preview of every bill and
*>              every revision the change would touch BEFORE
*>              anything is applied; every proposed revision is
*>              validated as a whole bill and one failure refuses
*>              the whole run, so a failed bill is never counted
*>              as changed; and a released revision is never
*>              written to - the change lands on a new draft
*>              revision beside it.  BOM-030 adds the fourth:
*>              repeating the same APPLY returns the first
*>              outcome instead of raising a second set of
*>              revisions.
*>
*>              PREVIEW and APPLY run exactly the same code over
*>              exactly the same transaction.  The only difference
*>              is whether it is committed, which is the only way
*>              to be sure that what was previewed is what will be
*>              applied.
*> CALLED BY  : the bulk change form, the engineering change desk
*> INTERFACE  : CALL "BOMBULK" USING WS-BOMBULK-CA
*>              COPYBOOK BOMBULKCA
*> CALLS      : SQLIO, BOMBREV (which calls BOMREAD, CYCLECHK and
*>              BOMVALID), AUDITLOG
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2016-03-21 AJS  CR2442 ORIGINAL - BULK ENGINEERING CHANGE
*> 2017-02-28 AJS  CR2601 RELEASED TARGETS GET A NEW REVISION
*> 2026-10-06 STERP ALL OR NOTHING ON VALIDATION (BOM-024)
*>================================================================
*> REQ: BOM-018, BOM-024, BOM-028, BOM-029, BOM-030, BOM-031
 IDENTIFICATION DIVISION.
 PROGRAM-ID. BOMBULK.
 AUTHOR. A J SPENCER.
 DATE-WRITTEN. 21/03/2016.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY BOMBREVC.
 COPY PERMCHKCA REPLACING ==DFHCOMMAREA== BY ==Ws-Perm==
                  LEADING ==CA-== BY ==PKH-==.
 01  Ws-Limits.
     05  Ws-Tgt-Max              PIC S9(4) VALUE 200.
 01  Ws-Row.
     05  Ws-Col OCCURS 8 TIMES   PIC X(60).
 01  Ws-Work.
     05  Ws-I                    PIC S9(4) VALUE 0.
     05  Ws-T                    PIC S9(4) VALUE 0.
     05  Ws-W                    PIC S9(4) VALUE 0.
     05  Ws-Res-Cnt              PIC S9(4) VALUE 0.
     05  Ws-More                 PIC X VALUE "Y".
     05  Ws-Keep                 PIC X VALUE "N".
     05  Ws-Record-Valid         PIC X VALUE "N".
     05  Ws-Dead                 PIC X VALUE "N".
     05  Ws-Limit-Hit            PIC X VALUE "N".
     05  Ws-Db-Action            PIC X(12) VALUE SPACES.
     05  Ws-Found-Status         PIC X(12) VALUE SPACES.
     05  Ws-Would                PIC S9(4) VALUE 0.
     05  Ws-Edit                 PIC Z(3)9.
 01  Ws-Edits.
     05  Ws-E1                   PIC Z(3)9.
     05  Ws-E2                   PIC Z(3)9.
     05  Ws-E3                   PIC Z(3)9.
     05  Ws-E4                   PIC Z(3)9.
     05  Ws-E5                   PIC Z(3)9.
     05  Ws-E6                   PIC Z(3)9.
 01  Ws-Results.
     05  Ws-Res OCCURS 200 TIMES.
         10  Wr-Bom-Id           PIC S9(9) COMP-5.
         10  Wr-New-Bom-Id       PIC S9(9) COMP-5.
         10  Wr-Bom-Ref          PIC X(30).
         10  Wr-Bom-Rev          PIC X(4).
         10  Wr-New-Rev          PIC X(4).
         10  Wr-Parent           PIC X(40).
         10  Wr-Status           PIC X(12).
         10  Wr-Treatment        PIC X(10).
         10  Wr-Line-Num         PIC S9(4).
         10  Wr-Old-Qty          PIC S9(13)V9(6).
         10  Wr-New-Qty          PIC S9(13)V9(6).
         10  Wr-Outcome          PIC X(10).
         10  Wr-Reason           PIC X(8).
         10  Wr-Errors           PIC S9(4).
         10  Wr-Detail           PIC X(120).
 LINKAGE SECTION.
 COPY BOMBULKCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "BOMBULK" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
     MOVE "BOM-024" TO CA-REQ-REF
     MOVE FUNCTION UPPER-CASE(BB-ACTION) TO BB-ACTION
     MOVE FUNCTION UPPER-CASE(BB-SCOPE) TO BB-SCOPE
     MOVE FUNCTION UPPER-CASE(BB-OLD-ITEM) TO BB-OLD-ITEM
     MOVE FUNCTION UPPER-CASE(BB-NEW-ITEM) TO BB-NEW-ITEM
     IF BB-WIN-MAX < 1 OR BB-WIN-MAX > 25
         MOVE 25 TO BB-WIN-MAX
     END-IF
     MOVE 0 TO BB-WIN-CNT
     MOVE "N" TO BB-MORE-LINES
     PERFORM Check-Authority
     IF CA-RETURN-CODE > 4
         GOBACK
     END-IF
     IF BB-EFF-DATE = 0
         MOVE CA-BUSINESS-DATE TO BB-EFF-DATE
     END-IF
     IF BB-REQUESTED-BY = SPACES
         MOVE CA-USER TO BB-REQUESTED-BY
     END-IF
     EVALUATE CA-FUNCTION
         WHEN "PREVIEW" MOVE "N" TO Ws-Keep
                        PERFORM Run-The-Change
         WHEN "APPLY"   MOVE "Y" TO Ws-Keep
                        PERFORM Run-The-Change
         WHEN "COMMIT"  MOVE "Y" TO Ws-Keep
                        PERFORM Run-The-Change
         WHEN "GETLINE" PERFORM Page-The-Outcome
         WHEN "RESULT"  PERFORM Page-The-Outcome
         WHEN OTHER     PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Check-Authority.
     INITIALIZE Ws-Perm
     EVALUATE CA-FUNCTION
         WHEN "PREVIEW" WHEN "GETLINE" WHEN "RESULT"
             MOVE "BOM.VIEW" TO PK-PERMISSION
         WHEN "APPLY" WHEN "COMMIT"
             MOVE "BOM.CHANGE.APPROVE" TO PK-PERMISSION
         WHEN OTHER EXIT PARAGRAPH
     END-EVALUATE
     MOVE "CHECK" TO PKH-FUNCTION
     MOVE CA-COMPANY TO PKH-COMPANY
     MOVE CA-SITE TO PKH-SITE
     MOVE CA-USER TO PKH-USER
     MOVE CA-BUSINESS-DATE TO PKH-BUSINESS-DATE
     CALL "PERMCHK" USING Ws-Perm
     IF PKH-RETURN-CODE > 4
         MOVE PKH-RETURN-CODE TO CA-RETURN-CODE
         MOVE PKH-REASON-CODE TO CA-REASON-CODE
         MOVE PKH-MESSAGE TO CA-MESSAGE
         MOVE PKH-MSG-COUNT TO CA-MSG-COUNT
     END-IF.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "BOM1400" TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "BOMBULK does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE
     ADD 1 TO CA-MSG-COUNT.

*>----------------------------------------------------------------
 Run-The-Change.
     MOVE 0 TO Ws-Res-Cnt BB-TARGET-CNT BB-APPLIED-CNT
     MOVE 0 TO BB-FAILED-CNT BB-SKIPPED-CNT BB-NEWREV-CNT
     MOVE 0 TO BB-AMEND-CNT BB-ERROR-CNT BB-WARNING-CNT
     MOVE 0 TO BB-CHANGE-ID Ws-Would
     MOVE "N" TO BB-DUPLICATE Ws-Dead Ws-Limit-Hit BB-MAY-APPLY
     MOVE "COMPLETE" TO BB-COMPLETENESS
     MOVE SPACES TO BB-SUMMARY BB-REFUSAL BB-STATUS
     MOVE 1 TO BB-FROM-LINE
     PERFORM Validate-The-Request
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Find-The-Change
     IF CA-RETURN-CODE > 4 OR BB-DUPLICATE = "Y"
         EXIT PARAGRAPH
     END-IF
     PERFORM Find-The-Targets
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     IF Ws-Res-Cnt = 0
         PERFORM Report-No-Targets
         EXIT PARAGRAPH
     END-IF
     PERFORM Record-The-Change-Header
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Run-The-Proposals
     PERFORM Record-The-Change-Lines
     PERFORM Summarise-The-Run
     PERFORM Page-The-Outcome.

*>----------------------------------------------------------------
*> Nothing here needs the database.  A bulk change that cannot say
*> what it is changing is refused before a single bill is read.
 Validate-The-Request.
     IF BB-CHANGE-REF = SPACES
         PERFORM Say-Error
         MOVE "BOM1401" TO CA-REASON-CODE
         MOVE "A change reference is required - a bulk change must be auditable"
             TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF CA-USER = SPACES
         PERFORM Say-Error
         MOVE "BOM1408" TO CA-REASON-CODE
         MOVE "A bulk change must be attributable to a user"
             TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF BB-ACTION NOT = "REPLACE" AND BB-ACTION NOT = "ADD"
        AND BB-ACTION NOT = "REMOVE" AND BB-ACTION NOT = "QTYCHG"
         PERFORM Say-Error
         MOVE "BOM1402" TO CA-REASON-CODE
         MOVE "The action must be REPLACE, ADD, REMOVE or QTYCHG"
             TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF BB-SCOPE = SPACES
         IF BB-ACTION = "ADD" AND BB-OLD-ITEM = SPACES
             MOVE "PARENT" TO BB-SCOPE
         ELSE
             MOVE "ITEM" TO BB-SCOPE
         END-IF
     END-IF
     IF BB-ACTION = "REPLACE" AND
        (BB-OLD-ITEM = SPACES OR BB-NEW-ITEM = SPACES)
         PERFORM Say-Error
         MOVE "BOM1403" TO CA-REASON-CODE
         MOVE "A replacement needs the component to remove and the component to use"
             TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF BB-ACTION = "ADD" AND BB-NEW-ITEM = SPACES
         PERFORM Say-Error
         MOVE "BOM1404" TO CA-REASON-CODE
         MOVE "An addition needs the component to add" TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF (BB-ACTION = "REMOVE" OR BB-ACTION = "QTYCHG")
        AND BB-OLD-ITEM = SPACES
         PERFORM Say-Error
         MOVE "BOM1405" TO CA-REASON-CODE
         MOVE "The component to change must be named" TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF BB-ACTION = "QTYCHG" AND BB-NEW-QTY <= 0
        AND BB-NEW-SCRAP <= 0 AND BB-NEW-UOM = SPACES
        AND BB-NEW-BASIS = SPACES
         PERFORM Say-Error
         MOVE "BOM1406" TO CA-REASON-CODE
         MOVE "A quantity change needs a quantity, a scrap allowance, a unit or a basis"
             TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF BB-SCOPE = "ITEM" AND BB-OLD-ITEM = SPACES
         PERFORM Say-Error
         MOVE "BOM1407" TO CA-REASON-CODE
         MOVE "Scope ITEM needs the component whose bills are the targets"
             TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF (BB-SCOPE = "PARENT" OR BB-SCOPE = "BOMREF")
        AND BB-SCOPE-ITEM = SPACES
         PERFORM Say-Error
         MOVE "BOM1407" TO CA-REASON-CODE
         MOVE "Scope PARENT and scope BOMREF need the parent item or bill reference"
             TO CA-MESSAGE
     END-IF.

 Say-Error.
     MOVE 8 TO CA-RETURN-CODE
     ADD 1 TO CA-MSG-COUNT.

*>----------------------------------------------------------------
*> COM-015 / BOM-030.  A change reference that has already been
*> applied gives its first answer back; it does not raise a second
*> set of revisions because somebody pressed the button twice.
 Find-The-Change.
     PERFORM Read-The-Change-Id
     IF CA-RETURN-CODE > 4 OR BB-CHANGE-ID = 0
         EXIT PARAGRAPH
     END-IF
     IF Ws-Found-Status NOT = "APPLIED"
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO BB-DUPLICATE
     MOVE "APPLIED" TO BB-STATUS
     PERFORM Read-Back-The-Outcome
     MOVE 4 TO CA-RETURN-CODE
     MOVE "BOM1410" TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "Change " FUNCTION TRIM(BB-CHANGE-REF)
            " has already been applied - the original outcome is "
            "returned and no further revision is raised"
         DELIMITED BY SIZE INTO CA-MESSAGE
     ADD 1 TO CA-MSG-COUNT
     PERFORM Summarise-The-Run
     PERFORM Page-The-Outcome.

 Read-The-Change-Id.
     MOVE 0 TO BB-CHANGE-ID
     MOVE SPACES TO Ws-Found-Status
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT c.ChangeId, c.Status FROM bom.BomChange c "
            "WHERE c.CompanyCode = ? AND c.ChangeRef = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 2 TO SQLIO-PARM-CNT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE BB-CHANGE-REF TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE 2 TO SQLIO-COL-CNT
     MOVE 60 TO SQLIO-COL-WIDTH(1)
     MOVE 60 TO SQLIO-COL-WIDTH(2)
     MOVE 6 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     IF SQLIO-NOTFOUND
         EXIT PARAGRAPH
     END-IF
     COMPUTE BB-CHANGE-ID = FUNCTION NUMVAL(Ws-Col(1))
     MOVE Ws-Col(2) TO Ws-Found-Status.

 Read-Back-The-Outcome.
     MOVE 0 TO Ws-Res-Cnt
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT l.BomHeaderId, ISNULL(l.BomRef,''), "
            "ISNULL(l.BomRevision,''), ISNULL(l.NewBomRevision,''),"
            " ISNULL(l.OldUsageQty,0), ISNULL(l.NewUsageQty,0), "
            "ISNULL(l.Outcome,''), ISNULL(l.FailureReason,'') "
            "FROM bom.BomChangeLine l WHERE l.ChangeId = ? "
            "ORDER BY l.LineNum"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 1 TO SQLIO-PARM-CNT
     MOVE BB-CHANGE-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE 8 TO SQLIO-COL-CNT
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > 8
         MOVE 60 TO SQLIO-COL-WIDTH(Ws-I)
     END-PERFORM
     MOVE 6 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More NOT = "Y" OR Ws-Res-Cnt >= Ws-Tgt-Max
         MOVE 6 TO SQLIO-CURSOR
         MOVE "FTCH" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA Ws-Row
         IF SQLIO-OK
             ADD 1 TO Ws-Res-Cnt
             MOVE Ws-Res-Cnt TO Ws-T
             INITIALIZE Ws-Res(Ws-T)
             COMPUTE Wr-Bom-Id(Ws-T) = FUNCTION NUMVAL(Ws-Col(1))
             MOVE Ws-Col(2) TO Wr-Bom-Ref(Ws-T)
             MOVE Ws-Col(3) TO Wr-Bom-Rev(Ws-T)
             MOVE Ws-Col(4) TO Wr-New-Rev(Ws-T)
             COMPUTE Wr-Old-Qty(Ws-T) = FUNCTION NUMVAL(Ws-Col(5))
             COMPUTE Wr-New-Qty(Ws-T) = FUNCTION NUMVAL(Ws-Col(6))
             MOVE Ws-Col(7) TO Wr-Outcome(Ws-T)
             MOVE Ws-Col(8) TO Wr-Detail(Ws-T)
             EVALUATE Ws-Col(7)(1:7)
                 WHEN "APPLIED" ADD 1 TO BB-APPLIED-CNT
                 WHEN "FAILED " ADD 1 TO BB-FAILED-CNT
                 WHEN OTHER     ADD 1 TO BB-SKIPPED-CNT
             END-EVALUATE
         ELSE
             MOVE "N" TO Ws-More
         END-IF
     END-PERFORM
     MOVE 6 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     MOVE Ws-Res-Cnt TO BB-TARGET-CNT.

*>----------------------------------------------------------------
*> REQ: BOM-023 - the target list is a where-used question asked
*> of the component, so the three scopes answer it three ways.  A
*> superseded or retired revision is never a target: history is
*> left reproducible (BOM-028).
 Find-The-Targets.
     MOVE 0 TO Ws-Res-Cnt
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT h.BomHeaderId, h.BomRef, h.BomRevision, "
            "h.ParentItemCode, h.Status FROM bom.BomHeader h "
            "WHERE h.CompanyCode = ? AND ((? = 'ITEM' AND EXISTS "
            "(SELECT 1 FROM bom.BomComponent c WHERE c.BomHeaderId"
            " = h.BomHeaderId AND c.ComponentItemCode = ?)) OR "
            "(? = 'PARENT' AND h.ParentItemCode = ?) OR "
            "(? = 'BOMREF' AND h.BomRef = ?)) AND h.Status IN "
            "('RELEASED','DRAFT','REVIEW') AND (? = 'Y' OR "
            "h.Status = 'RELEASED') ORDER BY h.BomRef, "
            "h.BomRevision"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 8 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > 8
         MOVE SPACES TO SQLIO-PARM-VAL(Ws-I)
         MOVE "N" TO SQLIO-PARM-NULL(Ws-I)
     END-PERFORM
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE BB-SCOPE TO SQLIO-PARM-VAL(2)
     MOVE BB-OLD-ITEM TO SQLIO-PARM-VAL(3)
     MOVE BB-SCOPE TO SQLIO-PARM-VAL(4)
     MOVE BB-SCOPE-ITEM TO SQLIO-PARM-VAL(5)
     MOVE BB-SCOPE TO SQLIO-PARM-VAL(6)
     MOVE BB-SCOPE-ITEM TO SQLIO-PARM-VAL(7)
     IF BB-INCLUDE-DRAFT = "N"
         MOVE "N" TO SQLIO-PARM-VAL(8)
     ELSE
         MOVE "Y" TO SQLIO-PARM-VAL(8)
     END-IF
     MOVE 5 TO SQLIO-COL-CNT
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > 5
         MOVE 60 TO SQLIO-COL-WIDTH(Ws-I)
     END-PERFORM
     MOVE 6 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More NOT = "Y"
         MOVE 6 TO SQLIO-CURSOR
         MOVE "FTCH" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA Ws-Row
         IF SQLIO-OK
             IF Ws-Res-Cnt >= Ws-Tgt-Max
                 MOVE "Y" TO Ws-Limit-Hit
             ELSE
                 ADD 1 TO Ws-Res-Cnt
                 MOVE Ws-Res-Cnt TO Ws-T
                 INITIALIZE Ws-Res(Ws-T)
                 COMPUTE Wr-Bom-Id(Ws-T) =
                     FUNCTION NUMVAL(Ws-Col(1))
                 MOVE Ws-Col(2) TO Wr-Bom-Ref(Ws-T)
                 MOVE Ws-Col(3) TO Wr-Bom-Rev(Ws-T)
                 MOVE Ws-Col(4) TO Wr-Parent(Ws-T)
                 MOVE Ws-Col(5) TO Wr-Status(Ws-T)
                 MOVE "PENDING" TO Wr-Outcome(Ws-T)
             END-IF
         ELSE
             MOVE "N" TO Ws-More
         END-IF
     END-PERFORM
     MOVE 6 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     MOVE Ws-Res-Cnt TO BB-TARGET-CNT
     IF Ws-Limit-Hit = "Y"
         MOVE "INCOMPLETE" TO BB-COMPLETENESS
         MOVE 4 TO CA-RETURN-CODE
         MOVE "BOM1413" TO CA-REASON-CODE
         MOVE "More bills use this component than one bulk run can carry - narrow the scope"
             TO CA-MESSAGE
         ADD 1 TO CA-MSG-COUNT
     END-IF.

 Report-No-Targets.
     MOVE 16 TO CA-RETURN-CODE
     MOVE "BOM1409" TO CA-REASON-CODE
     MOVE "NONE" TO BB-STATUS
     MOVE SPACES TO CA-MESSAGE
     STRING "No bill in scope " FUNCTION TRIM(BB-SCOPE)
            " would be touched by change "
            FUNCTION TRIM(BB-CHANGE-REF)
         DELIMITED BY SIZE INTO CA-MESSAGE
     ADD 1 TO CA-MSG-COUNT.

*>----------------------------------------------------------------
*> The change set is recorded outside the proposal transaction, so
*> that a preview that is thrown away still leaves the evidence of
*> what was asked for and what the answer was.
 Record-The-Change-Header.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT INTO bom.BomChange (CompanyCode, ChangeRef, "
            "Title, ChangeType, RequestedBy, EffectiveDate, "
            "Status) SELECT ?, ?, ?, ?, ?, CONVERT(date,?,112), "
            "'DRAFT' WHERE NOT EXISTS (SELECT 1 FROM "
            "bom.BomChange x WHERE x.CompanyCode = ? "
            "AND x.ChangeRef = ?)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 8 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > 8
         MOVE SPACES TO SQLIO-PARM-VAL(Ws-I)
         MOVE "N" TO SQLIO-PARM-NULL(Ws-I)
     END-PERFORM
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE BB-CHANGE-REF TO SQLIO-PARM-VAL(2)
     IF BB-TITLE = SPACES
         STRING "Bulk " FUNCTION TRIM(BB-ACTION) " of "
                FUNCTION TRIM(BB-OLD-ITEM)
             DELIMITED BY SIZE INTO BB-TITLE
     END-IF
     MOVE BB-TITLE TO SQLIO-PARM-VAL(3)
     PERFORM Translate-The-Action
     MOVE Ws-Db-Action TO SQLIO-PARM-VAL(4)
     MOVE BB-REQUESTED-BY TO SQLIO-PARM-VAL(5)
     MOVE BB-EFF-DATE TO SQLIO-PARM-VAL(6)
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(7)
     MOVE BB-CHANGE-REF TO SQLIO-PARM-VAL(8)
     PERFORM Run-One-Statement
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Read-The-Change-Id.

*> The database calls a quantity change QTYCHANGE; the engineers
*> and every screen since 1996 call it QTYCHG.  CR2442 chose not
*> to rename either of them.
 Translate-The-Action.
     IF BB-ACTION = "QTYCHG"
         MOVE "QTYCHANGE" TO Ws-Db-Action
     ELSE
         MOVE BB-ACTION TO Ws-Db-Action
     END-IF.

*>----------------------------------------------------------------
*> REQ: BOM-024, BOM-030.  One transaction, one decision.  Every
*> proposed revision is built and validated; the run is kept only
*> when the engineer asked to apply it AND not one proposal
*> failed.  Anything else is rolled back in full, which is how a
*> released original is guaranteed to survive a bad change.
 Run-The-Proposals.
     IF Ws-Keep = "Y"
         MOVE "Y" TO Ws-Record-Valid
     ELSE
         MOVE "N" TO Ws-Record-Valid
     END-IF
     MOVE "BEGN" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     PERFORM VARYING Ws-T FROM 1 BY 1
             UNTIL Ws-T > Ws-Res-Cnt OR Ws-Dead = "Y"
         PERFORM Propose-One-Target
     END-PERFORM
     IF Ws-Keep = "Y" AND BB-FAILED-CNT = 0 AND Ws-Dead = "N"
         MOVE "COMT" TO SQLIO-FUNC
         MOVE "APPLIED" TO BB-STATUS
     ELSE
         MOVE "ROLB" TO SQLIO-FUNC
         IF Ws-Keep = "Y"
             MOVE "REJECTED" TO BB-STATUS
         ELSE
             MOVE "PREVIEW" TO BB-STATUS
         END-IF
         PERFORM Downgrade-Applied-Lines
     END-IF
     CALL "SQLIO" USING SQLIO-CA Ws-Row.

 Propose-One-Target.
     INITIALIZE BOM-BR-CTL
     MOVE SPACES TO BOM-BR-MSG
     MOVE "PROPOSE" TO PR-FUNC
     MOVE CA-COMPANY TO PR-COMPANY
     MOVE CA-SITE TO PR-SITE
     MOVE CA-USER TO PR-USER
     MOVE BB-CHANGE-REF TO PR-CHANGE-REF
     MOVE BB-CHANGE-ID TO PR-CHANGE-ID
     MOVE Ws-T TO PR-SEQ
     MOVE Wr-Bom-Id(Ws-T) TO PR-BOM-ID
     MOVE BB-ACTION TO PR-ACTION
     MOVE BB-OLD-ITEM TO PR-OLD-ITEM
     MOVE BB-NEW-ITEM TO PR-NEW-ITEM
     MOVE BB-NEW-QTY TO PR-NEW-QTY
     MOVE BB-NEW-UOM TO PR-NEW-UOM
     MOVE BB-NEW-BASIS TO PR-NEW-BASIS
     MOVE BB-NEW-SCRAP TO PR-NEW-SCRAP
     MOVE BB-NEW-LINE-NUM TO PR-NEW-LINE-NUM
     MOVE BB-EFF-DATE TO PR-EFF-DATE
     MOVE Ws-Record-Valid TO PR-RECORD-VALID
     CALL "BOMBREV" USING BOM-BR-CTL BOM-BR-MSG
     IF PR-BOM-REF NOT = SPACES
         MOVE PR-BOM-REF TO Wr-Bom-Ref(Ws-T)
         MOVE PR-BOM-REV TO Wr-Bom-Rev(Ws-T)
         MOVE PR-PARENT TO Wr-Parent(Ws-T)
         MOVE PR-SRC-STATUS TO Wr-Status(Ws-T)
     END-IF
     MOVE PR-NEW-BOM-ID TO Wr-New-Bom-Id(Ws-T)
     MOVE PR-NEW-REV TO Wr-New-Rev(Ws-T)
     MOVE PR-TREATMENT TO Wr-Treatment(Ws-T)
     MOVE PR-LINE-NUM TO Wr-Line-Num(Ws-T)
     MOVE PR-OLD-QTY TO Wr-Old-Qty(Ws-T)
     MOVE PR-RESULT-QTY TO Wr-New-Qty(Ws-T)
     MOVE PR-OUTCOME TO Wr-Outcome(Ws-T)
     MOVE PR-REASON TO Wr-Reason(Ws-T)
     MOVE PR-ERROR-CNT TO Wr-Errors(Ws-T)
     MOVE BOM-BR-MSG TO Wr-Detail(Ws-T)
     ADD PR-ERROR-CNT TO BB-ERROR-CNT
     ADD PR-WARNING-CNT TO BB-WARNING-CNT
     EVALUATE PR-OUTCOME
         WHEN "APPLIED" ADD 1 TO BB-APPLIED-CNT
                        IF PR-TREATMENT = "NEWREV"
                            ADD 1 TO BB-NEWREV-CNT
                        ELSE
                            ADD 1 TO BB-AMEND-CNT
                        END-IF
         WHEN "FAILED"  ADD 1 TO BB-FAILED-CNT
         WHEN OTHER     ADD 1 TO BB-SKIPPED-CNT
     END-EVALUATE
     IF PR-SQL-DEAD = "Y"
         MOVE "Y" TO Ws-Dead
     END-IF.

*> Nothing was kept, so nothing may claim to have been applied.
*> BOM-024: a bill that was not changed is never counted as one
*> that was.
 Downgrade-Applied-Lines.
     MOVE BB-APPLIED-CNT TO Ws-Would
     MOVE 0 TO BB-APPLIED-CNT
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > Ws-Res-Cnt
         IF Wr-Outcome(Ws-I) = "APPLIED"
             MOVE "PENDING" TO Wr-Outcome(Ws-I)
             MOVE 0 TO Wr-New-Bom-Id(Ws-I)
         END-IF
     END-PERFORM.

*>----------------------------------------------------------------
 Record-The-Change-Lines.
     IF BB-CHANGE-ID = 0
         EXIT PARAGRAPH
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE FROM bom.BomChangeLine WHERE ChangeId = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 1 TO SQLIO-PARM-CNT
     MOVE BB-CHANGE-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     PERFORM Run-One-Statement
     PERFORM VARYING Ws-T FROM 1 BY 1 UNTIL Ws-T > Ws-Res-Cnt
         PERFORM Record-One-Change-Line
     END-PERFORM
     PERFORM Update-The-Change-Header
     IF BB-STATUS = "APPLIED"
         PERFORM Raise-The-Impact-Rows
     END-IF.

 Record-One-Change-Line.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT INTO bom.BomChangeLine (ChangeId, LineNum, "
            "BomHeaderId, BomRef, BomRevision, Action, "
            "ComponentItemCode, NewComponentItemCode, OldUsageQty,"
            " NewUsageQty, NewBomRevision, Outcome, FailureReason)"
            " VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 13 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > 13
         MOVE SPACES TO SQLIO-PARM-VAL(Ws-I)
         MOVE "N" TO SQLIO-PARM-NULL(Ws-I)
     END-PERFORM
     MOVE BB-CHANGE-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
     MOVE Ws-T TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
     MOVE Wr-Bom-Id(Ws-T) TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
     MOVE Wr-Bom-Ref(Ws-T) TO SQLIO-PARM-VAL(4)
     MOVE Wr-Bom-Rev(Ws-T) TO SQLIO-PARM-VAL(5)
     MOVE Ws-Db-Action TO SQLIO-PARM-VAL(6)
     MOVE BB-OLD-ITEM TO SQLIO-PARM-VAL(7)
     IF BB-OLD-ITEM = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(7)
     END-IF
     MOVE BB-NEW-ITEM TO SQLIO-PARM-VAL(8)
     IF BB-NEW-ITEM = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(8)
     END-IF
     MOVE Wr-Old-Qty(Ws-T) TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
     MOVE Wr-New-Qty(Ws-T) TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
     MOVE Wr-New-Rev(Ws-T) TO SQLIO-PARM-VAL(11)
     IF Wr-New-Rev(Ws-T) = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(11)
     END-IF
     MOVE Wr-Outcome(Ws-T) TO SQLIO-PARM-VAL(12)
     MOVE SPACES TO SQLIO-PARM-VAL(13)
     IF Wr-Reason(Ws-T) = SPACES
         MOVE Wr-Detail(Ws-T) TO SQLIO-PARM-VAL(13)
     ELSE
         STRING "STERP-E-" FUNCTION TRIM(Wr-Reason(Ws-T)) ": "
                Wr-Detail(Ws-T)
             DELIMITED BY SIZE INTO SQLIO-PARM-VAL(13)
     END-IF
     PERFORM Run-One-Statement.

 Update-The-Change-Header.
     MOVE SPACES TO SQLIO-STMT
     STRING "UPDATE bom.BomChange SET Title = ?, ChangeType = ?, "
            "RequestedBy = ?, EffectiveDate = CONVERT(date,?,112),"
            " Status = ?, AffectedBills = ?, AppliedBills = ?, "
            "ImpactNotes = ? WHERE CompanyCode = ? AND "
            "ChangeRef = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 10 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > 10
         MOVE SPACES TO SQLIO-PARM-VAL(Ws-I)
         MOVE "N" TO SQLIO-PARM-NULL(Ws-I)
     END-PERFORM
     MOVE BB-TITLE TO SQLIO-PARM-VAL(1)
     MOVE Ws-Db-Action TO SQLIO-PARM-VAL(2)
     MOVE BB-REQUESTED-BY TO SQLIO-PARM-VAL(3)
     MOVE BB-EFF-DATE TO SQLIO-PARM-VAL(4)
     MOVE BB-STATUS TO SQLIO-PARM-VAL(5)
     MOVE BB-TARGET-CNT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
     MOVE BB-APPLIED-CNT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(7)
     MOVE BB-SUMMARY TO SQLIO-PARM-VAL(8)
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(9)
     MOVE BB-CHANGE-REF TO SQLIO-PARM-VAL(10)
     PERFORM Run-One-Statement.

*> REQ: BOM-031.  Every released bill the change really touched is
*> handed to the engineering change desk as an impact row that has
*> to be decided before the change can be activated.  ECOIMPCT
*> refreshes the same rows on the same key, so the two programs
*> cooperate instead of duplicating.
 Raise-The-Impact-Rows.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT INTO bom.BomChangeImpact (ChangeId, "
            "CompanyCode, ChangeRef, ImpactType, BomHeaderId, "
            "ParentItemCode, ComponentItemCode, DocumentType, "
            "DocumentRef, DocumentStatus, PathText) SELECT "
            "l.ChangeId, h.CompanyCode, ?, 'PARENTBOM', "
            "l.BomHeaderId, h.ParentItemCode, "
            "l.ComponentItemCode, 'BOM', h.BomRef + '/' + "
            "h.BomRevision, h.Status, 'bulk change target' FROM "
            "bom.BomChangeLine l JOIN bom.BomHeader h ON "
            "h.BomHeaderId = l.BomHeaderId WHERE l.ChangeId = ? "
            "AND h.Status = 'RELEASED' AND l.Outcome = 'APPLIED' "
            "AND NOT EXISTS (SELECT 1 FROM bom.BomChangeImpact x "
            "WHERE x.CompanyCode = h.CompanyCode AND "
            "x.ChangeRef = ? AND x.ImpactType = 'PARENTBOM' AND "
            "x.BomHeaderId = l.BomHeaderId AND x.DocumentRef = "
            "h.BomRef + '/' + h.BomRevision)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 3 TO SQLIO-PARM-CNT
     MOVE BB-CHANGE-REF TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE BB-CHANGE-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE BB-CHANGE-REF TO SQLIO-PARM-VAL(3)
     MOVE "N" TO SQLIO-PARM-NULL(3)
     PERFORM Run-One-Statement.

 Run-One-Statement.
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 6 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
     END-IF
     MOVE 6 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row.

*>----------------------------------------------------------------
 Summarise-The-Run.
     IF BB-DUPLICATE = "Y"
         MOVE BB-APPLIED-CNT TO Ws-Would
     END-IF
     IF Ws-Keep = "Y" AND BB-FAILED-CNT = 0 AND Ws-Dead = "N"
         MOVE BB-APPLIED-CNT TO Ws-Would
     END-IF
     MOVE BB-TARGET-CNT TO Ws-E1
     MOVE Ws-Would TO Ws-E2
     MOVE BB-NEWREV-CNT TO Ws-E3
     MOVE BB-AMEND-CNT TO Ws-E4
     MOVE BB-SKIPPED-CNT TO Ws-E5
     MOVE BB-FAILED-CNT TO Ws-E6
     MOVE SPACES TO BB-SUMMARY
     STRING FUNCTION TRIM(Ws-E1) " bill(s) in scope; "
            FUNCTION TRIM(Ws-E2) " to change ("
            FUNCTION TRIM(Ws-E3) " as a new revision, "
            FUNCTION TRIM(Ws-E4) " as a draft amendment); "
            FUNCTION TRIM(Ws-E5) " skipped; "
            FUNCTION TRIM(Ws-E6) " failed validation"
         DELIMITED BY SIZE INTO BB-SUMMARY
     PERFORM Decide-The-Return-Code.

 Decide-The-Return-Code.
     IF BB-DUPLICATE = "Y"
         EXIT PARAGRAPH
     END-IF
     IF Ws-Dead = "Y"
         PERFORM Report-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     IF BB-FAILED-CNT > 0
         MOVE "N" TO BB-MAY-APPLY
         MOVE SPACES TO BB-REFUSAL
         MOVE BB-FAILED-CNT TO Ws-Edit
         STRING "STERP-E-BOM1411: " FUNCTION TRIM(Ws-Edit)
                " proposed revision(s) fail validation, so no bill"
                " is changed at all - correct them and preview "
                "again"
             DELIMITED BY SIZE INTO BB-REFUSAL
         MOVE 8 TO CA-RETURN-CODE
         MOVE "BOM1411" TO CA-REASON-CODE
         MOVE BB-REFUSAL TO CA-MESSAGE
         ADD 1 TO CA-MSG-COUNT
         EXIT PARAGRAPH
     END-IF
     IF Ws-Keep = "Y"
         MOVE "Y" TO BB-MAY-APPLY
         MOVE SPACES TO CA-MESSAGE
         MOVE BB-APPLIED-CNT TO Ws-Edit
         STRING "STERP-I-BOM1414: " FUNCTION TRIM(Ws-Edit)
                " bill(s) changed under "
                FUNCTION TRIM(BB-CHANGE-REF)
             DELIMITED BY SIZE INTO CA-MESSAGE
         MOVE "BOM1414" TO CA-REASON-CODE
         EXIT PARAGRAPH
     END-IF
     IF Ws-Would > 0
         MOVE "Y" TO BB-MAY-APPLY
     ELSE
         MOVE "N" TO BB-MAY-APPLY
     END-IF
     MOVE "BOM1415" TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "STERP-I-BOM1415: preview only - nothing has been "
            "changed. " FUNCTION TRIM(BB-SUMMARY)
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>----------------------------------------------------------------
 Page-The-Outcome.
     IF BB-FROM-LINE < 1
         MOVE 1 TO BB-FROM-LINE
     END-IF
     MOVE 0 TO BB-WIN-CNT
     MOVE "N" TO BB-MORE-LINES
     MOVE BB-FROM-LINE TO Ws-I
     PERFORM UNTIL Ws-I > Ws-Res-Cnt OR BB-WIN-CNT >= BB-WIN-MAX
         ADD 1 TO BB-WIN-CNT
         MOVE BB-WIN-CNT TO Ws-W
         MOVE Ws-I TO BB-L-SEQ(Ws-W)
         MOVE Wr-Bom-Id(Ws-I) TO BB-L-BOM-ID(Ws-W)
         MOVE Wr-New-Bom-Id(Ws-I) TO BB-L-NEW-BOM-ID(Ws-W)
         MOVE Wr-Bom-Ref(Ws-I) TO BB-L-BOM-REF(Ws-W)
         MOVE Wr-Bom-Rev(Ws-I) TO BB-L-BOM-REV(Ws-W)
         MOVE Wr-New-Rev(Ws-I) TO BB-L-NEW-REV(Ws-W)
         MOVE Wr-Parent(Ws-I) TO BB-L-PARENT(Ws-W)
         MOVE Wr-Status(Ws-I) TO BB-L-STATUS(Ws-W)
         MOVE Wr-Treatment(Ws-I) TO BB-L-TREATMENT(Ws-W)
         MOVE Wr-Line-Num(Ws-I) TO BB-L-LINE-NUM(Ws-W)
         MOVE Wr-Old-Qty(Ws-I) TO BB-L-OLD-QTY(Ws-W)
         MOVE Wr-New-Qty(Ws-I) TO BB-L-NEW-QTY(Ws-W)
         MOVE Wr-Outcome(Ws-I) TO BB-L-OUTCOME(Ws-W)
         MOVE Wr-Reason(Ws-I) TO BB-L-REASON(Ws-W)
         MOVE Wr-Errors(Ws-I) TO BB-L-ERRORS(Ws-W)
         MOVE Wr-Detail(Ws-I) TO BB-L-DETAIL(Ws-W)
         ADD 1 TO Ws-I
     END-PERFORM
     MOVE Ws-I TO BB-FROM-LINE
     IF Ws-I <= Ws-Res-Cnt
         MOVE "Y" TO BB-MORE-LINES
     END-IF.

 Report-Sql-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "BOM1412" TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "STERP-E-BOM1412: bulk change database access failed "
            "- " SQLIO-ERRMSG(1:120)
         DELIMITED BY SIZE INTO CA-MESSAGE
     ADD 1 TO CA-MSG-COUNT.
