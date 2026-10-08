>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : ECOIMPCT
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : ENGINEERING CHANGE IMPACT (BOM-031).  A NEW
*>              STANDARD EFFECTIVITY DOES NOT SILENTLY REWRITE
*>              WORK THAT IS ALREADY COMMITTED.  BEFORE A CHANGE
*>              MAY BE ACTIVATED THIS PROGRAM LISTS EVERYTHING
*>              THAT IS ALREADY POINTING AT THE AFFECTED
*>              STRUCTURE:
*>                KIT       released kits built from the bill
*>                DEMAND    open orders and planned demand
*>                JOBBOM    job/order bills copied from it
*>                PARENTBOM higher level bills that quote it
*>              EVERY ROW MUST CARRY AN EXPLICIT DECISION -
*>              RETAIN (keep the old content for that document),
*>              ADOPT (take the new content), REPLAN (reissue the
*>              demand) OR CANCEL.  WHILE ANY ROW IS PENDING THE
*>              CHANGE CANNOT BE ACTIVATED AND THE SUPERSEDED
*>              REVISION CANNOT BE RETIRED (BOM-028), WHICH IS
*>              WHAT EI-MAY-ACTIVATE AND EI-MAY-RETIRE REPORT.
*> FUNCTIONS  : ASSESS   build / refresh the impact list
*>              GETLINE  page the impact list
*>              DECIDE   record one decision (EI-IMPACT-ID)
*>              DECIDEALL record the same decision for the rest
*>              CHECK    may the change be activated?
*> INTERFACE  : CALL "ECOIMPCT" USING WS-ECOIMPCT-CA
*>              COPYBOOK ECOIMPCTCA
*> CALLS      : SQLIO, WHEREUSD, AUDITLOG
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2016-05-23 AJS  CR2466 ORIGINAL - ECO IMPACT WORKSHEET
*> 2026-10-05 STERP EXPLICIT DECISION BEFORE ACTIVATION
*>================================================================
*> REQ: BOM-023, BOM-028, BOM-029, BOM-030, BOM-031
 IDENTIFICATION DIVISION.
 PROGRAM-ID. ECOIMPCT.
 AUTHOR. A J SPENCER.
 DATE-WRITTEN. 23/05/2016.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 01  Ws-Row.
     05  Ws-Col OCCURS 20 TIMES  PIC X(100).
 01  Ws-Work.
     05  Ws-W                    PIC S9(4) VALUE 0.
     05  Ws-Ix                   PIC S9(4) VALUE 0.
     05  Ws-Seq                  PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Rows                 PIC S9(9) COMP-5 VALUE 0.
     05  Ws-More                 PIC X VALUE "Y".
     05  Ws-Num-Edit             PIC -(9)9.
     05  Ws-Qty-Edit             PIC -(15)9.999999.
     05  Ws-Date-Edit            PIC 9(8).
     05  Ws-Decision             PIC X(10) VALUE SPACES.
     05  Ws-Stmt-Kind            PIC X(10) VALUE SPACES.
 LINKAGE SECTION.
 COPY ECOIMPCTCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "ECOIMPCT" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
     MOVE "BOM-031" TO CA-REQ-REF
     IF EI-WIN-MAX < 1 OR EI-WIN-MAX > 25
         MOVE 25 TO EI-WIN-MAX
     END-IF
     MOVE 0 TO EI-WIN-CNT
     MOVE "N" TO EI-MORE-LINES
     EVALUATE CA-FUNCTION
         WHEN "ASSESS"    PERFORM Assess-The-Change
                          PERFORM Summarise-The-Impact
         WHEN "GETLINE"   PERFORM Page-The-Impact
         WHEN "DECIDE"    PERFORM Record-One-Decision
                          PERFORM Summarise-The-Impact
         *> CA-FUNCTION is X(8): "DECIDEALL" could never match (CR2614)
         WHEN "DECIDALL"  PERFORM Record-Remaining
                          PERFORM Summarise-The-Impact
         WHEN "CHECK"     PERFORM Summarise-The-Impact
         WHEN OTHER       PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "BOM1101" TO CA-REASON-CODE
     MOVE "STERP-E-BOM1101: unsupported ECOIMPCT function"
         TO CA-MESSAGE
     ADD 1 TO CA-MSG-COUNT.

*>----------------------------------------------------------------
*> ASSESS.  The impact rows are rebuilt from the live documents
*> every time, except that a decision already recorded is kept -
*> an engineer who has decided RETAIN for a job must not have that
*> answer thrown away by the next refresh (BOM-030).
*>----------------------------------------------------------------
 Assess-The-Change.
     IF EI-CHANGE-REF = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "BOM1102" TO CA-REASON-CODE
         MOVE "STERP-E-BOM1102: a change reference is required"
             TO CA-MESSAGE
         ADD 1 TO CA-MSG-COUNT
         EXIT PARAGRAPH
     END-IF
     IF EI-ITEM = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "BOM1103" TO CA-REASON-CODE
         MOVE "STERP-E-BOM1103: the changed item is required"
             TO CA-MESSAGE
         ADD 1 TO CA-MSG-COUNT
         EXIT PARAGRAPH
     END-IF
     PERFORM Insert-Parent-Bill-Impact
     PERFORM Insert-Job-Bill-Impact
     PERFORM Insert-Demand-Impact.

 Insert-Parent-Bill-Impact.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT bom.BomChangeImpact (CompanyCode, ChangeRef, "
            "ImpactType, BomHeaderId, ParentItemCode, "
            "ComponentItemCode, DocumentType, DocumentRef, "
            "DocumentStatus, PathText) SELECT h.CompanyCode, ?, "
            "'PARENTBOM', h.BomHeaderId, h.ParentItemCode, ?, "
            "'BOM', h.BomRef + '/' + h.BomRevision, h.Status, "
            "h.ParentItemCode + ' <- ' + c.ComponentItemCode "
            "FROM bom.BomComponent c JOIN bom.BomHeader h ON "
            "h.BomHeaderId = c.BomHeaderId WHERE h.CompanyCode = ? "
            "AND c.ComponentItemCode = ? AND h.Status IN "
            "('RELEASED','DRAFT','REVIEW') AND NOT EXISTS (SELECT "
            "1 FROM bom.BomChangeImpact x WHERE x.CompanyCode = "
            "h.CompanyCode AND x.ChangeRef = ? AND x.ImpactType = "
            "'PARENTBOM' AND x.BomHeaderId = h.BomHeaderId AND "
            "x.DocumentRef = h.BomRef + '/' + h.BomRevision)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 5 TO SQLIO-PARM-CNT
     MOVE EI-CHANGE-REF TO SQLIO-PARM-VAL(1)
     MOVE EI-ITEM TO SQLIO-PARM-VAL(2)
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
     MOVE EI-ITEM TO SQLIO-PARM-VAL(4)
     MOVE EI-CHANGE-REF TO SQLIO-PARM-VAL(5)
     PERFORM VARYING Ws-Ix FROM 1 BY 1 UNTIL Ws-Ix > 5
         MOVE "N" TO SQLIO-PARM-NULL(Ws-Ix)
     END-PERFORM
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 4 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
     END-IF.

 Insert-Job-Bill-Impact.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT bom.BomChangeImpact (CompanyCode, ChangeRef, "
            "ImpactType, BomHeaderId, ParentItemCode, "
            "ComponentItemCode, DocumentType, DocumentRef, "
            "DocumentStatus, PathText) SELECT h.CompanyCode, ?, "
            "'JOBBOM', h.BomHeaderId, h.ParentItemCode, ?, "
            "h.Purpose, ISNULL(h.JobRef, h.BomRef), h.Status, "
            "'job bill copied from the standard structure' "
            "FROM bom.BomHeader h WHERE h.CompanyCode = ? AND "
            "h.Purpose IN ('JOB','ORDER') AND h.Status IN "
            "('DRAFT','REVIEW','RELEASED') AND EXISTS (SELECT 1 "
            "FROM bom.BomComponent c WHERE c.BomHeaderId = "
            "h.BomHeaderId AND c.ComponentItemCode = ?) AND NOT "
            "EXISTS (SELECT 1 FROM bom.BomChangeImpact x WHERE "
            "x.CompanyCode = h.CompanyCode AND x.ChangeRef = ? "
            "AND x.ImpactType = 'JOBBOM' AND x.BomHeaderId = "
            "h.BomHeaderId)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 5 TO SQLIO-PARM-CNT
     MOVE EI-CHANGE-REF TO SQLIO-PARM-VAL(1)
     MOVE EI-ITEM TO SQLIO-PARM-VAL(2)
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
     MOVE EI-ITEM TO SQLIO-PARM-VAL(4)
     MOVE EI-CHANGE-REF TO SQLIO-PARM-VAL(5)
     PERFORM VARYING Ws-Ix FROM 1 BY 1 UNTIL Ws-Ix > 5
         MOVE "N" TO SQLIO-PARM-NULL(Ws-Ix)
     END-PERFORM
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 4 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
     END-IF.

*>  Released kits and open demand live in other modules.  The
*>  impact table is the agreed hand-over point: the kit and order
*>  teams post their rows, this program only has to make sure a
*>  decision exists for each of them before the change goes live.
 Insert-Demand-Impact.
     MOVE SPACES TO SQLIO-STMT
     STRING "UPDATE bom.BomChangeImpact SET ComponentItemCode = ? "
            "WHERE CompanyCode = ? AND ChangeRef = ? AND "
            "ComponentItemCode IS NULL"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 3 TO SQLIO-PARM-CNT
     MOVE EI-ITEM TO SQLIO-PARM-VAL(1)
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
     MOVE EI-CHANGE-REF TO SQLIO-PARM-VAL(3)
     PERFORM VARYING Ws-Ix FROM 1 BY 1 UNTIL Ws-Ix > 3
         MOVE "N" TO SQLIO-PARM-NULL(Ws-Ix)
     END-PERFORM
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 4 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
     END-IF.

*>----------------------------------------------------------------
 Summarise-The-Impact.
     MOVE 0 TO EI-KIT-CNT EI-DEMAND-CNT EI-JOBBOM-CNT
               EI-PARENT-CNT EI-PENDING-CNT EI-TOTAL-CNT
     MOVE SPACES TO EI-BLOCK-REASON
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT ImpactType, COUNT(*), SUM(CASE WHEN Decision "
            "= 'PENDING' THEN 1 ELSE 0 END) FROM "
            "bom.BomChangeImpact WHERE CompanyCode = ? AND "
            "ChangeRef = ? GROUP BY ImpactType"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 2 TO SQLIO-PARM-CNT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE EI-CHANGE-REF TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE 3 TO SQLIO-COL-CNT
     MOVE 12 TO SQLIO-COL-WIDTH(1)
     MOVE 10 TO SQLIO-COL-WIDTH(2)
     MOVE 10 TO SQLIO-COL-WIDTH(3)
     MOVE 4 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More NOT = "Y"
         MOVE "FTCH" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA Ws-Row
         IF SQLIO-OK
             COMPUTE Ws-W = FUNCTION NUMVAL(Ws-Col(2))
             COMPUTE Ws-Ix = FUNCTION NUMVAL(Ws-Col(3))
             ADD Ws-W TO EI-TOTAL-CNT
             ADD Ws-Ix TO EI-PENDING-CNT
             EVALUATE Ws-Col(1)(1:12)
                 WHEN "KIT         " ADD Ws-W TO EI-KIT-CNT
                 WHEN "DEMAND      " ADD Ws-W TO EI-DEMAND-CNT
                 WHEN "JOBBOM      " ADD Ws-W TO EI-JOBBOM-CNT
                 WHEN "PARENTBOM   " ADD Ws-W TO EI-PARENT-CNT
                 WHEN OTHER CONTINUE
             END-EVALUATE
         ELSE
             MOVE "N" TO Ws-More
         END-IF
     END-PERFORM
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF EI-PENDING-CNT > 0
         MOVE "N" TO EI-MAY-ACTIVATE
         MOVE "N" TO EI-MAY-RETIRE
         MOVE SPACES TO EI-BLOCK-REASON
         MOVE EI-PENDING-CNT TO Ws-Num-Edit
         STRING "STERP-W-BOM1104: " FUNCTION TRIM(Ws-Num-Edit)
                " impacted item(s) still need an explicit "
                "adopt, retain or replan decision"
             DELIMITED BY SIZE INTO EI-BLOCK-REASON
         MOVE 4 TO CA-RETURN-CODE
         MOVE "BOM1104" TO CA-REASON-CODE
         MOVE EI-BLOCK-REASON TO CA-MESSAGE
         ADD 1 TO CA-MSG-COUNT
     ELSE
         MOVE "Y" TO EI-MAY-ACTIVATE
         MOVE "Y" TO EI-MAY-RETIRE
         MOVE SPACES TO EI-BLOCK-REASON
     END-IF.

*>----------------------------------------------------------------
 Page-The-Impact.
     IF EI-FROM-LINE < 0
         MOVE 0 TO EI-FROM-LINE
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT ImpactId, ImpactType, ISNULL(DocumentType,''),"
            " DocumentRef, ISNULL(DocumentStatus,''), "
            "ISNULL(ParentItemCode,''), ISNULL(ComponentItemCode,"
            "''), BomHeaderId, ISNULL(AffectedQty,0), "
            "ISNULL(CONVERT(varchar(8), NeedDate, 112),'0'), "
            "Decision, ISNULL(DecisionBy,''), ISNULL(PathText,'') "
            "FROM bom.BomChangeImpact WHERE CompanyCode = ? AND "
            "ChangeRef = ? ORDER BY ImpactType, DocumentRef "
            "OFFSET ? ROWS FETCH NEXT ? ROWS ONLY"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 4 TO SQLIO-PARM-CNT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE EI-CHANGE-REF TO SQLIO-PARM-VAL(2)
     MOVE EI-FROM-LINE TO Ws-Num-Edit
     MOVE FUNCTION TRIM(Ws-Num-Edit) TO SQLIO-PARM-VAL(3)
     MOVE EI-WIN-MAX TO Ws-Num-Edit
     MOVE FUNCTION TRIM(Ws-Num-Edit) TO SQLIO-PARM-VAL(4)
     PERFORM VARYING Ws-Ix FROM 1 BY 1 UNTIL Ws-Ix > 4
         MOVE "N" TO SQLIO-PARM-NULL(Ws-Ix)
     END-PERFORM
     MOVE 13 TO SQLIO-COL-CNT
     PERFORM VARYING Ws-Ix FROM 1 BY 1 UNTIL Ws-Ix > 13
         MOVE 40 TO SQLIO-COL-WIDTH(Ws-Ix)
     END-PERFORM
     MOVE 100 TO SQLIO-COL-WIDTH(13)
     MOVE 4 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO EI-WIN-CNT
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More NOT = "Y"
                OR EI-WIN-CNT >= EI-WIN-MAX
         MOVE "FTCH" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA Ws-Row
         IF SQLIO-OK
             ADD 1 TO EI-WIN-CNT
             MOVE EI-WIN-CNT TO Ws-W
             COMPUTE EI-L-IMPACT-ID(Ws-W) =
                 FUNCTION NUMVAL(Ws-Col(1))
             MOVE Ws-Col(2) TO EI-L-TYPE(Ws-W)
             MOVE Ws-Col(3) TO EI-L-DOC-TYPE(Ws-W)
             MOVE Ws-Col(4) TO EI-L-DOC-REF(Ws-W)
             MOVE Ws-Col(5) TO EI-L-DOC-STATUS(Ws-W)
             MOVE Ws-Col(6) TO EI-L-PARENT(Ws-W)
             MOVE Ws-Col(7) TO EI-L-COMPONENT(Ws-W)
             COMPUTE EI-L-BOM-ID(Ws-W) = FUNCTION NUMVAL(Ws-Col(8))
             COMPUTE EI-L-QTY(Ws-W) = FUNCTION NUMVAL(Ws-Col(9))
             COMPUTE EI-L-NEED-DATE(Ws-W) =
                 FUNCTION NUMVAL(Ws-Col(10))
             MOVE Ws-Col(11) TO EI-L-DECISION(Ws-W)
             MOVE Ws-Col(12) TO EI-L-DECIDED-BY(Ws-W)
             MOVE Ws-Col(13) TO EI-L-PATH(Ws-W)
         ELSE
             MOVE "N" TO Ws-More
         END-IF
     END-PERFORM
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF EI-WIN-CNT = EI-WIN-MAX
         MOVE "Y" TO EI-MORE-LINES
     ELSE
         MOVE "N" TO EI-MORE-LINES
     END-IF.

*>----------------------------------------------------------------
 Record-One-Decision.
     PERFORM Validate-Decision
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "UPDATE bom.BomChangeImpact SET Decision = ?, "
            "DecisionBy = ?, DecisionAt = SYSDATETIME(), "
            "DecisionNote = ? WHERE CompanyCode = ? AND ChangeRef "
            "= ? AND ImpactId = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 6 TO SQLIO-PARM-CNT
     MOVE Ws-Decision TO SQLIO-PARM-VAL(1)
     MOVE CA-USER TO SQLIO-PARM-VAL(2)
     MOVE EI-DECISION-NOTE TO SQLIO-PARM-VAL(3)
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(4)
     MOVE EI-CHANGE-REF TO SQLIO-PARM-VAL(5)
     MOVE EI-IMPACT-ID TO Ws-Num-Edit
     MOVE FUNCTION TRIM(Ws-Num-Edit) TO SQLIO-PARM-VAL(6)
     PERFORM VARYING Ws-Ix FROM 1 BY 1 UNTIL Ws-Ix > 6
         MOVE "N" TO SQLIO-PARM-NULL(Ws-Ix)
     END-PERFORM
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 4 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
     END-IF.

 Record-Remaining.
     PERFORM Validate-Decision
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "UPDATE bom.BomChangeImpact SET Decision = ?, "
            "DecisionBy = ?, DecisionAt = SYSDATETIME(), "
            "DecisionNote = ? WHERE CompanyCode = ? AND ChangeRef "
            "= ? AND Decision = 'PENDING'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 5 TO SQLIO-PARM-CNT
     MOVE Ws-Decision TO SQLIO-PARM-VAL(1)
     MOVE CA-USER TO SQLIO-PARM-VAL(2)
     MOVE EI-DECISION-NOTE TO SQLIO-PARM-VAL(3)
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(4)
     MOVE EI-CHANGE-REF TO SQLIO-PARM-VAL(5)
     PERFORM VARYING Ws-Ix FROM 1 BY 1 UNTIL Ws-Ix > 5
         MOVE "N" TO SQLIO-PARM-NULL(Ws-Ix)
     END-PERFORM
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 4 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
     END-IF.

 Validate-Decision.
     MOVE FUNCTION UPPER-CASE(EI-DECISION) TO Ws-Decision
     IF Ws-Decision NOT = "RETAIN" AND Ws-Decision NOT = "ADOPT"
        AND Ws-Decision NOT = "REPLAN" AND Ws-Decision NOT = "CANCEL"
         MOVE 8 TO CA-RETURN-CODE
         MOVE "BOM1105" TO CA-REASON-CODE
         MOVE "STERP-E-BOM1105: decision must be RETAIN, ADOPT, REPLAN or CANCEL"
             TO CA-MESSAGE
         ADD 1 TO CA-MSG-COUNT
     END-IF
     IF CA-USER = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "BOM1106" TO CA-REASON-CODE
         MOVE "STERP-E-BOM1106: a decision must be attributable to a user"
             TO CA-MESSAGE
         ADD 1 TO CA-MSG-COUNT
     END-IF.

 Report-Sql-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "BOM1107" TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "STERP-E-BOM1107: impact database access failed - "
            SQLIO-ERRMSG(1:100)
         DELIMITED BY SIZE INTO CA-MESSAGE
     ADD 1 TO CA-MSG-COUNT.
