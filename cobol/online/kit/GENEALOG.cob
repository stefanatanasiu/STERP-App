>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : GENEALOG
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : Kit genealogy enquiry in BOTH directions.
*>
*>                FORWARD   a finished parent lot or serial, down
*>                          to the component lots and serials that
*>                          were actually consumed
*>                BACKWARD  a component lot or serial, up to every
*>                          finished parent and kit that carries
*>                          it - the question a recall asks
*>                WHEREUSD  open kits that still carry a
*>                          requirement for the component
*>
*>              The trace reports WHAT HAPPENED, not the plan.
*>              Every line carries the PLANNED component beside
*>              the component actually issued, so a substitution
*>              is visible rather than inferred (KIT-012), and
*>              returns and scrap may be included because a lot
*>              that came back off the job is part of the story
*>              even though it was never built in (KIT-013,
*>              KIT-014).
*>
*>              Nested kits are followed to GN-MAX-DEPTH levels:
*>              a subassembly built on its own kit is traced
*>              through to its own components.  Depth is bounded
*>              because a recall enquiry must answer, and an
*>              unbounded walk of a cyclic structure does not.
*> CALLED BY   : the genealogy enquiry form, KITCOMPL (check), the
*>               recall listing and the unit tests
*> INTERFACE   : CALL "GENEALOG" USING WS-GENEALOG-CA
*>               COPYBOOK GENEALOGCA
*> CALLS       : SQLIO
*>----------------------------------------------------------------
*> CHANGE LOG
*> 1996-11-12 DRN  CR0402 WHERE USED ENQUIRY
*> 2015-06-29 PMK  CR2361 GENEALOGY ENQUIRY - FREE FORMAT
*> 2016-08-02 PMK  CR2494 BACKWARD TRACE FOR RECALLS, SUBSTITUTES
*>                 SHOWN AGAINST THE PLANNED COMPONENT
*> 2026-10-06 STERP KIT-019
*>================================================================
*> REQ: KIT-019
 IDENTIFICATION DIVISION.
 PROGRAM-ID. GENEALOG.
 AUTHOR. P M KEANE.
 DATE-WRITTEN. 29/06/2015.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 01  Ws-Row.
     05  Ws-Col OCCURS 20 TIMES  PIC X(100).
*>   The collected answer.  A recall enquiry is read once and
*>   paged many times; the window is cut from this table.
 01  Ws-Result.
     05  Ws-Res-Cnt              PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Res-Trunc            PIC X VALUE "N".
     05  Ws-Res-Line OCCURS 400 TIMES.
         10  Rs-Depth            PIC S9(4).
         10  Rs-Kit-Ref          PIC X(30).
         10  Rs-Parent           PIC X(40).
         10  Rs-Par-Lot          PIC X(30).
         10  Rs-Par-Serial       PIC X(40).
         10  Rs-Item             PIC X(40).
         10  Rs-Lot              PIC X(30).
         10  Rs-Serial           PIC X(40).
         10  Rs-Qty              PIC S9(13)V9(6).
         10  Rs-Uom              PIC X(10).
         10  Rs-Planned          PIC X(40).
         10  Rs-Substitute       PIC X.
         10  Rs-Event            PIC X(12).
         10  Rs-When             PIC 9(8).
         10  Rs-Operator         PIC X(20).
         10  Rs-Note             PIC X(60).
*>   Breadth first work list.  One entry is one identity still to
*>   be expanded, with the level it was found at.
 01  Ws-Queue.
     05  Ws-Q-Cnt                PIC S9(4) VALUE 0.
     05  Ws-Q-Ptr                PIC S9(4) VALUE 0.
     05  Ws-Q-Entry OCCURS 60 TIMES.
         10  Qe-Item             PIC X(40).
         10  Qe-Lot              PIC X(30).
         10  Qe-Serial           PIC X(40).
         10  Qe-Depth            PIC S9(4).
 01  Ws-Kits.
     05  Ws-Kit-Cnt              PIC S9(4) VALUE 0.
     05  Ws-Kit-Seen OCCURS 40 TIMES PIC X(30).
 01  Ws-Work.
     05  Ws-I                    PIC S9(9) COMP-5 VALUE 0.
     05  Ws-J                    PIC S9(9) COMP-5 VALUE 0.
     05  Ws-W                    PIC S9(4) VALUE 0.
     05  Ws-Depth                PIC S9(4) VALUE 0.
     05  Ws-Cur-Item             PIC X(40) VALUE SPACES.
     05  Ws-Cur-Lot              PIC X(30) VALUE SPACES.
     05  Ws-Cur-Serial           PIC X(40) VALUE SPACES.
*>   SQLIO only has eight cursor slots (sterpsql.c MAXCUR) and KITDB
*>   holds 5 and 6, so the kit enquiry programs share 3,4,7,8.
     05  Ws-Cursor               PIC S9(9) COMP-5 VALUE 7.
     05  Ws-More                 PIC X VALUE "Y".
     05  Ws-Found                PIC X VALUE "N".
     05  Ws-Edit                 PIC -(13)9.9(6).
     05  Ws-Cnt-Edit             PIC Z(8)9.
     05  Ws-Dep-Edit             PIC Z(3)9.
     05  Ws-Summary              PIC X(200) VALUE SPACES.
 LINKAGE SECTION.
 COPY GENEALOGCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "GENEALOG" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE 0 TO CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     MOVE "KIT-019" TO CA-REQ-REF
     PERFORM Clear-Answer
     EVALUATE CA-FUNCTION
         WHEN "FORWARD"  PERFORM Trace-Forward
         WHEN "BACKWARD" PERFORM Trace-Backward
         WHEN "WHEREUSD" PERFORM Where-Used-Open-Kits
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     IF NOT CA-FAILED
         PERFORM Fill-Window
         PERFORM Summarise
     END-IF
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "KIT0002" TO CA-REASON-CODE
     STRING "GENEALOG does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

 Set-Sql-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE SQLIO-ERRMSG (1:200) TO CA-MESSAGE.

 Clear-Answer.
     MOVE 0 TO Ws-Res-Cnt
     MOVE "N" TO Ws-Res-Trunc
     MOVE 0 TO Ws-Q-Cnt
     MOVE 0 TO Ws-Q-Ptr
     MOVE 0 TO Ws-Kit-Cnt
     MOVE 0 TO GN-LINE-TOTAL
     MOVE 0 TO GN-DEPTH-REACHED
     MOVE 0 TO GN-SUBSTITUTE-CNT
     MOVE 0 TO GN-RETURN-CNT
     MOVE 0 TO GN-SCRAP-CNT
     MOVE 0 TO GN-KIT-CNT
     MOVE 0 TO GN-WIN-CNT
     MOVE "N" TO GN-TRUNCATED
     MOVE "N" TO GN-MORE-LINES
     MOVE SPACES TO GN-SUMMARY-TEXT
     IF GN-MAX-DEPTH <= 0
         MOVE 5 TO GN-MAX-DEPTH
     END-IF
     IF GN-MAX-DEPTH > 12
         MOVE 12 TO GN-MAX-DEPTH
     END-IF
     IF GN-WIN-MAX <= 0 OR GN-WIN-MAX > 20
         MOVE 20 TO GN-WIN-MAX
     END-IF
     IF GN-FROM-LINE <= 0
         MOVE 1 TO GN-FROM-LINE
     END-IF.

*>----------------------------------------------------------------
*> An enquiry with no identity at all would return the whole
*> factory.  KIT-019 wants a traceable answer, not a dump.
 Validate-Identity.
     IF GN-ITEM-CODE = SPACES AND GN-KIT-REF = SPACES
             AND GN-COMPLETION-ID = 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0012" TO CA-REASON-CODE
         MOVE "Give an item, a kit reference or a completion identity to trace."
              TO CA-MESSAGE
     END-IF.

*>----------------------------------------------------------------
*> FORWARD - from a built parent down to what went into it.
 Trace-Forward.
     PERFORM Validate-Identity
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Seed-Queue
     PERFORM UNTIL Ws-Q-Ptr >= Ws-Q-Cnt OR Ws-Res-Trunc = "Y"
         ADD 1 TO Ws-Q-Ptr
         MOVE Qe-Item (Ws-Q-Ptr) TO Ws-Cur-Item
         MOVE Qe-Lot (Ws-Q-Ptr) TO Ws-Cur-Lot
         MOVE Qe-Serial (Ws-Q-Ptr) TO Ws-Cur-Serial
         MOVE Qe-Depth (Ws-Q-Ptr) TO Ws-Depth
         PERFORM Read-Children
         IF CA-FAILED
             EXIT PERFORM
         END-IF
     END-PERFORM
     IF NOT CA-FAILED
         PERFORM Add-Disposition-Lines
     END-IF
     IF NOT CA-FAILED AND Ws-Res-Cnt = 0
         PERFORM No-Genealogy-Found
     END-IF.

*>----------------------------------------------------------------
*> BACKWARD - from a component lot or serial up to every parent.
 Trace-Backward.
     PERFORM Validate-Identity
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Seed-Queue
     PERFORM UNTIL Ws-Q-Ptr >= Ws-Q-Cnt OR Ws-Res-Trunc = "Y"
         ADD 1 TO Ws-Q-Ptr
         MOVE Qe-Item (Ws-Q-Ptr) TO Ws-Cur-Item
         MOVE Qe-Lot (Ws-Q-Ptr) TO Ws-Cur-Lot
         MOVE Qe-Serial (Ws-Q-Ptr) TO Ws-Cur-Serial
         MOVE Qe-Depth (Ws-Q-Ptr) TO Ws-Depth
         PERFORM Read-Parents
         IF CA-FAILED
             EXIT PERFORM
         END-IF
     END-PERFORM
     IF NOT CA-FAILED
         PERFORM Add-Disposition-Lines
     END-IF
     IF NOT CA-FAILED AND Ws-Res-Cnt = 0
         PERFORM No-Genealogy-Found
     END-IF.

 No-Genealogy-Found.
     MOVE 16 TO CA-RETURN-CODE
     MOVE "KIT0026" TO CA-REASON-CODE
     STRING "No genealogy is recorded for "
            FUNCTION TRIM(GN-ITEM-CODE) " "
            FUNCTION TRIM(GN-LOT-CODE)
            FUNCTION TRIM(GN-SERIAL-NO) "."
         DELIMITED BY SIZE INTO CA-MESSAGE.

 Seed-Queue.
     MOVE 1 TO Ws-Q-Cnt
     MOVE 0 TO Ws-Q-Ptr
     MOVE GN-ITEM-CODE TO Qe-Item (1)
     MOVE GN-LOT-CODE TO Qe-Lot (1)
     MOVE GN-SERIAL-NO TO Qe-Serial (1)
     MOVE 0 TO Qe-Depth (1).

*>   An identity is queued once.  Without this guard a component
*>   that appears twice on the same parent walks the tree twice
*>   and the recall list double counts - CR2494.
 Queue-Identity.
     IF Ws-Depth + 1 >= GN-MAX-DEPTH
         EXIT PARAGRAPH
     END-IF
     IF Ws-Cur-Lot = SPACES AND Ws-Cur-Serial = SPACES
         EXIT PARAGRAPH
     END-IF
     MOVE "N" TO Ws-Found
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > Ws-Q-Cnt
         IF Qe-Item (Ws-W) = Ws-Cur-Item
                 AND Qe-Lot (Ws-W) = Ws-Cur-Lot
                 AND Qe-Serial (Ws-W) = Ws-Cur-Serial
             MOVE "Y" TO Ws-Found
         END-IF
     END-PERFORM
     IF Ws-Found = "Y"
         EXIT PARAGRAPH
     END-IF
     IF Ws-Q-Cnt >= 60
         MOVE "Y" TO Ws-Res-Trunc
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Q-Cnt
     MOVE Ws-Cur-Item TO Qe-Item (Ws-Q-Cnt)
     MOVE Ws-Cur-Lot TO Qe-Lot (Ws-Q-Cnt)
     MOVE Ws-Cur-Serial TO Qe-Serial (Ws-Q-Cnt)
     COMPUTE Qe-Depth (Ws-Q-Cnt) = Ws-Depth + 1.

 Note-Kit.
     MOVE "N" TO Ws-Found
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > Ws-Kit-Cnt
         IF Ws-Kit-Seen (Ws-W) = Rs-Kit-Ref (Ws-Res-Cnt)
             MOVE "Y" TO Ws-Found
         END-IF
     END-PERFORM
     IF Ws-Found = "N" AND Ws-Kit-Cnt < 40
         ADD 1 TO Ws-Kit-Cnt
         MOVE Rs-Kit-Ref (Ws-Res-Cnt) TO Ws-Kit-Seen (Ws-Kit-Cnt)
     END-IF.

*>----------------------------------------------------------------
*> The genealogy statement.  ParentItemCode / ComponentItemCode
*> swap round between the two directions; everything else about
*> the row is the same, so one column layout serves both.
 Build-Genealogy-Select.
     MOVE SPACES TO SQLIO-STMT
     STRING
       "SELECT g.KitRef, g.ParentItemCode,"
       " ISNULL(g.ParentLotCode,''), ISNULL(g.ParentSerialNo,''),"
       " g.ComponentItemCode, ISNULL(g.ComponentLotCode,''),"
       " ISNULL(g.ComponentSerialNo,''),"
       " CONVERT(varchar(26),g.ConsumedQty),"
       " CONVERT(varchar(2),g.WasSubstitute),"
       " ISNULL(g.PlannedItemCode,''),"
       " ISNULL(i.UomCode,''),"
       " ISNULL(CONVERT(char(8),i.ActualDate,112),''),"
       " ISNULL(i.OperatorUser,''),"
       " ISNULL(i.ReasonCode,'')"
       " FROM kit.KitGenealogy g"
       " LEFT JOIN kit.KitIssue i ON i.KitIssueId = g.KitIssueId"
       " WHERE g.CompanyCode = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT.

 Set-Genealogy-Columns.
     MOVE 14 TO SQLIO-COL-CNT
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > 20
         MOVE 100 TO SQLIO-COL-WIDTH (Ws-W)
     END-PERFORM
     MOVE Ws-Cursor TO SQLIO-CURSOR.

 Clear-Parms.
     MOVE 0 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > 10
         MOVE SPACES TO SQLIO-PARM-VAL (Ws-W)
         MOVE "N" TO SQLIO-PARM-NULL (Ws-W)
     END-PERFORM.

 Read-Children.
     PERFORM Clear-Parms
     PERFORM Build-Genealogy-Select
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE 1 TO SQLIO-PARM-CNT
     STRING FUNCTION TRIM(SQLIO-STMT)
            " AND g.ParentItemCode = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE Ws-Cur-Item TO SQLIO-PARM-VAL (2)
     MOVE 2 TO SQLIO-PARM-CNT
     PERFORM Add-Identity-Filter
     PERFORM Add-Common-Filters
     PERFORM Fetch-Genealogy-Rows.

 Read-Parents.
     PERFORM Clear-Parms
     PERFORM Build-Genealogy-Select
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE 1 TO SQLIO-PARM-CNT
     STRING FUNCTION TRIM(SQLIO-STMT)
            " AND g.ComponentItemCode = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE Ws-Cur-Item TO SQLIO-PARM-VAL (2)
     MOVE 2 TO SQLIO-PARM-CNT
     PERFORM Add-Identity-Filter
     PERFORM Add-Common-Filters
     PERFORM Fetch-Genealogy-Rows.

*>   A blank lot or serial means "any", which is how a trace by
*>   item code alone still works.  The parameter is never
*>   concatenated into the text.
 Add-Identity-Filter.
     IF CA-FUNCTION = "FORWARD"
         STRING FUNCTION TRIM(SQLIO-STMT)
                " AND (? = '' OR ISNULL(g.ParentLotCode,'') = ?)"
                " AND (? = '' OR ISNULL(g.ParentSerialNo,'') = ?)"
             DELIMITED BY SIZE INTO SQLIO-STMT
     ELSE
         STRING FUNCTION TRIM(SQLIO-STMT)
                " AND (? = '' OR ISNULL(g.ComponentLotCode,'') = ?)"
                " AND (? = '' OR ISNULL(g.ComponentSerialNo,'') = ?)"
             DELIMITED BY SIZE INTO SQLIO-STMT
     END-IF
     MOVE Ws-Cur-Lot TO SQLIO-PARM-VAL (3)
     MOVE Ws-Cur-Lot TO SQLIO-PARM-VAL (4)
     MOVE Ws-Cur-Serial TO SQLIO-PARM-VAL (5)
     MOVE Ws-Cur-Serial TO SQLIO-PARM-VAL (6)
     MOVE 6 TO SQLIO-PARM-CNT.

 Add-Common-Filters.
     STRING FUNCTION TRIM(SQLIO-STMT)
            " AND (? = '' OR g.KitRef = ?)"
            " AND (? = 0 OR g.CompletionId = ?)"
            " ORDER BY g.GenealogyId"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE GN-KIT-REF TO SQLIO-PARM-VAL (7)
     MOVE GN-KIT-REF TO SQLIO-PARM-VAL (8)
     MOVE GN-COMPLETION-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (9)
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (10)
     MOVE 10 TO SQLIO-PARM-CNT.

 Fetch-Genealogy-Rows.
     PERFORM Set-Genealogy-Columns
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Row
     IF SQLIO-ERROR
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE "FTCH" TO SQLIO-FUNC
         MOVE Ws-Cursor TO SQLIO-CURSOR
         CALL "SQLIO" USING SQLIO-CA, Ws-Row
         EVALUATE TRUE
             WHEN SQLIO-OK
                 PERFORM Store-Genealogy-Row
                 IF Ws-Res-Trunc = "Y"
                     MOVE "N" TO Ws-More
                 END-IF
             WHEN SQLIO-NOTFOUND
                 MOVE "N" TO Ws-More
             WHEN OTHER
                 PERFORM Set-Sql-Failure
                 MOVE "N" TO Ws-More
         END-EVALUATE
     END-PERFORM
     MOVE "CLOS" TO SQLIO-FUNC
     MOVE Ws-Cursor TO SQLIO-CURSOR
     CALL "SQLIO" USING SQLIO-CA, Ws-Row.

 Store-Genealogy-Row.
     IF Ws-Res-Cnt >= 400
         MOVE "Y" TO Ws-Res-Trunc
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Res-Cnt
     COMPUTE Rs-Depth (Ws-Res-Cnt) = Ws-Depth + 1
     MOVE Ws-Col (1) TO Rs-Kit-Ref (Ws-Res-Cnt)
     MOVE Ws-Col (2) TO Rs-Parent (Ws-Res-Cnt)
     MOVE Ws-Col (3) TO Rs-Par-Lot (Ws-Res-Cnt)
     MOVE Ws-Col (4) TO Rs-Par-Serial (Ws-Res-Cnt)
     MOVE Ws-Col (5) TO Rs-Item (Ws-Res-Cnt)
     MOVE Ws-Col (6) TO Rs-Lot (Ws-Res-Cnt)
     MOVE Ws-Col (7) TO Rs-Serial (Ws-Res-Cnt)
     COMPUTE Rs-Qty (Ws-Res-Cnt) = FUNCTION NUMVAL (Ws-Col (8))
     MOVE Ws-Col (11) TO Rs-Uom (Ws-Res-Cnt)
     MOVE Ws-Col (10) TO Rs-Planned (Ws-Res-Cnt)
     MOVE "CONSUMED" TO Rs-Event (Ws-Res-Cnt)
     IF Ws-Col (12) (1:8) NUMERIC
         MOVE Ws-Col (12) (1:8) TO Rs-When (Ws-Res-Cnt)
     ELSE
         MOVE 0 TO Rs-When (Ws-Res-Cnt)
     END-IF
     MOVE Ws-Col (13) TO Rs-Operator (Ws-Res-Cnt)
     PERFORM Mark-Substitution
     IF Rs-Depth (Ws-Res-Cnt) > GN-DEPTH-REACHED
         MOVE Rs-Depth (Ws-Res-Cnt) TO GN-DEPTH-REACHED
     END-IF
     PERFORM Note-Kit
     PERFORM Queue-Next-Identity.

*>   A substitution is evidence, not a footnote: the planned part
*>   stays on the line beside the part that was really fitted.
 Mark-Substitution.
     IF Ws-Col (9) (1:1) = "1"
         MOVE "Y" TO Rs-Substitute (Ws-Res-Cnt)
         ADD 1 TO GN-SUBSTITUTE-CNT
         MOVE SPACES TO Rs-Note (Ws-Res-Cnt)
         STRING "Substituted for "
                FUNCTION TRIM(Rs-Planned (Ws-Res-Cnt))
             DELIMITED BY SIZE INTO Rs-Note (Ws-Res-Cnt)
     ELSE
         MOVE "N" TO Rs-Substitute (Ws-Res-Cnt)
         MOVE SPACES TO Rs-Note (Ws-Res-Cnt)
         IF Rs-Planned (Ws-Res-Cnt) = SPACES
             MOVE Rs-Item (Ws-Res-Cnt)
                  TO Rs-Planned (Ws-Res-Cnt)
         END-IF
     END-IF.

 Queue-Next-Identity.
     IF CA-FUNCTION = "FORWARD"
         MOVE Rs-Item (Ws-Res-Cnt) TO Ws-Cur-Item
         MOVE Rs-Lot (Ws-Res-Cnt) TO Ws-Cur-Lot
         MOVE Rs-Serial (Ws-Res-Cnt) TO Ws-Cur-Serial
     ELSE
         MOVE Rs-Parent (Ws-Res-Cnt) TO Ws-Cur-Item
         MOVE Rs-Par-Lot (Ws-Res-Cnt) TO Ws-Cur-Lot
         MOVE Rs-Par-Serial (Ws-Res-Cnt) TO Ws-Cur-Serial
     END-IF
     PERFORM Queue-Identity.

*>----------------------------------------------------------------
*> Returns and scrap.  Neither built anything, so neither belongs
*> in the consumption tree, but a recall still has to know that a
*> lot came back off the job or was scrapped on it (D-12).
 Add-Disposition-Lines.
     IF GN-INCLUDE-RET NOT = "Y" AND GN-INCLUDE-SCRAP NOT = "Y"
         EXIT PARAGRAPH
     END-IF
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "SELECT i.KitRef, i.ComponentItemCode,"
       " ISNULL(i.LotCode,''), ISNULL(i.SerialNo,''),"
       " CONVERT(varchar(26),i.Quantity), ISNULL(i.UomCode,''),"
       " i.IssueType,"
       " ISNULL(CONVERT(char(8),i.ActualDate,112),''),"
       " ISNULL(i.OperatorUser,''), ISNULL(i.Reason,'')"
       " FROM kit.KitIssue i"
       " WHERE i.CompanyCode = ?"
       " AND i.IssueType IN (?,?)"
       " AND (? = '' OR i.ComponentItemCode = ?)"
       " AND (? = '' OR ISNULL(i.LotCode,'') = ?)"
       " AND (? = '' OR ISNULL(i.SerialNo,'') = ?)"
       " AND (? = '' OR i.KitRef = ?)"
       " ORDER BY i.KitIssueId"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (1)
     IF GN-INCLUDE-RET = "Y"
         MOVE "RETURN" TO SQLIO-PARM-VAL (2)
     ELSE
         MOVE "~NONE~" TO SQLIO-PARM-VAL (2)
     END-IF
     IF GN-INCLUDE-SCRAP = "Y"
         MOVE "SCRAP" TO SQLIO-PARM-VAL (3)
     ELSE
         MOVE "~NONE~" TO SQLIO-PARM-VAL (3)
     END-IF
     MOVE GN-ITEM-CODE TO SQLIO-PARM-VAL (4)
     MOVE GN-ITEM-CODE TO SQLIO-PARM-VAL (5)
     MOVE GN-LOT-CODE TO SQLIO-PARM-VAL (6)
     MOVE GN-LOT-CODE TO SQLIO-PARM-VAL (7)
     MOVE GN-SERIAL-NO TO SQLIO-PARM-VAL (8)
     MOVE GN-SERIAL-NO TO SQLIO-PARM-VAL (9)
     MOVE GN-KIT-REF TO SQLIO-PARM-VAL (10)
     MOVE GN-KIT-REF TO SQLIO-PARM-VAL (11)
     MOVE 11 TO SQLIO-PARM-CNT
     MOVE 10 TO SQLIO-COL-CNT
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > 20
         MOVE 100 TO SQLIO-COL-WIDTH (Ws-W)
     END-PERFORM
     MOVE Ws-Cursor TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Row
     IF SQLIO-ERROR
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE "FTCH" TO SQLIO-FUNC
         MOVE Ws-Cursor TO SQLIO-CURSOR
         CALL "SQLIO" USING SQLIO-CA, Ws-Row
         EVALUATE TRUE
             WHEN SQLIO-OK
                 PERFORM Store-Disposition-Row
             WHEN SQLIO-NOTFOUND
                 MOVE "N" TO Ws-More
             WHEN OTHER
                 PERFORM Set-Sql-Failure
                 MOVE "N" TO Ws-More
         END-EVALUATE
     END-PERFORM
     MOVE "CLOS" TO SQLIO-FUNC
     MOVE Ws-Cursor TO SQLIO-CURSOR
     CALL "SQLIO" USING SQLIO-CA, Ws-Row.

 Store-Disposition-Row.
     IF Ws-Res-Cnt >= 400
         MOVE "Y" TO Ws-Res-Trunc
         MOVE "N" TO Ws-More
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Res-Cnt
     MOVE 1 TO Rs-Depth (Ws-Res-Cnt)
     MOVE Ws-Col (1) TO Rs-Kit-Ref (Ws-Res-Cnt)
     MOVE SPACES TO Rs-Parent (Ws-Res-Cnt)
     MOVE SPACES TO Rs-Par-Lot (Ws-Res-Cnt)
     MOVE SPACES TO Rs-Par-Serial (Ws-Res-Cnt)
     MOVE Ws-Col (2) TO Rs-Item (Ws-Res-Cnt)
     MOVE Ws-Col (3) TO Rs-Lot (Ws-Res-Cnt)
     MOVE Ws-Col (4) TO Rs-Serial (Ws-Res-Cnt)
     COMPUTE Rs-Qty (Ws-Res-Cnt) = FUNCTION NUMVAL (Ws-Col (5))
     MOVE Ws-Col (6) TO Rs-Uom (Ws-Res-Cnt)
     MOVE Ws-Col (2) TO Rs-Planned (Ws-Res-Cnt)
     MOVE "N" TO Rs-Substitute (Ws-Res-Cnt)
     MOVE Ws-Col (7) TO Rs-Event (Ws-Res-Cnt)
     IF Ws-Col (8) (1:8) NUMERIC
         MOVE Ws-Col (8) (1:8) TO Rs-When (Ws-Res-Cnt)
     ELSE
         MOVE 0 TO Rs-When (Ws-Res-Cnt)
     END-IF
     MOVE Ws-Col (9) TO Rs-Operator (Ws-Res-Cnt)
     MOVE Ws-Col (10) TO Rs-Note (Ws-Res-Cnt)
     IF Ws-Col (7) (1:6) = "RETURN"
         ADD 1 TO GN-RETURN-CNT
     ELSE
         ADD 1 TO GN-SCRAP-CNT
     END-IF
     PERFORM Note-Kit.

*>----------------------------------------------------------------
*> WHEREUSD - section 10 enquiry.  Which OPEN kits still want this
*> component?  This is a planning question, not a trace, so it
*> reads the requirement and not the genealogy.
 Where-Used-Open-Kits.
     IF GN-ITEM-CODE = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0012" TO CA-REASON-CODE
         MOVE "An item code is required for a where used enquiry."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "SELECT k.KitRef, k.ParentItemCode, l.ComponentItemCode,"
       " CONVERT(varchar(26),l.RemainingQty), ISNULL(l.UomCode,''),"
       " k.Status, ISNULL(CONVERT(char(8),k.NeedDate,112),''),"
       " ISNULL(k.OwnerUser,''), CONVERT(varchar(12),l.LineNum)"
       " FROM kit.ExecutionKitLine l"
       " INNER JOIN kit.ExecutionKit k"
       "   ON k.ExecutionKitId = l.ExecutionKitId"
       " WHERE k.CompanyCode = ?"
       " AND (l.ComponentItemCode = ?"
       "      OR ISNULL(l.SubstituteItemCode,'') = ?)"
       " AND k.Status IN ('DRAFT','REVIEWED','RELEASED',"
       "   'PARTISSUED','ISSUED','PARTCOMPLETE')"
       " AND l.RemainingQty > 0"
       " ORDER BY k.NeedDate, k.KitRef, l.LineNum"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE GN-ITEM-CODE TO SQLIO-PARM-VAL (2)
     MOVE GN-ITEM-CODE TO SQLIO-PARM-VAL (3)
     MOVE 3 TO SQLIO-PARM-CNT
     MOVE 9 TO SQLIO-COL-CNT
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > 20
         MOVE 100 TO SQLIO-COL-WIDTH (Ws-W)
     END-PERFORM
     MOVE Ws-Cursor TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Row
     IF SQLIO-ERROR
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE "FTCH" TO SQLIO-FUNC
         MOVE Ws-Cursor TO SQLIO-CURSOR
         CALL "SQLIO" USING SQLIO-CA, Ws-Row
         EVALUATE TRUE
             WHEN SQLIO-OK
                 PERFORM Store-Where-Used-Row
             WHEN SQLIO-NOTFOUND
                 MOVE "N" TO Ws-More
             WHEN OTHER
                 PERFORM Set-Sql-Failure
                 MOVE "N" TO Ws-More
         END-EVALUATE
     END-PERFORM
     MOVE "CLOS" TO SQLIO-FUNC
     MOVE Ws-Cursor TO SQLIO-CURSOR
     CALL "SQLIO" USING SQLIO-CA, Ws-Row.

 Store-Where-Used-Row.
     IF Ws-Res-Cnt >= 400
         MOVE "Y" TO Ws-Res-Trunc
         MOVE "N" TO Ws-More
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Res-Cnt
     MOVE 1 TO Rs-Depth (Ws-Res-Cnt)
     MOVE Ws-Col (1) TO Rs-Kit-Ref (Ws-Res-Cnt)
     MOVE Ws-Col (2) TO Rs-Parent (Ws-Res-Cnt)
     MOVE SPACES TO Rs-Par-Lot (Ws-Res-Cnt)
     MOVE SPACES TO Rs-Par-Serial (Ws-Res-Cnt)
     MOVE Ws-Col (3) TO Rs-Item (Ws-Res-Cnt)
     MOVE SPACES TO Rs-Lot (Ws-Res-Cnt)
     MOVE SPACES TO Rs-Serial (Ws-Res-Cnt)
     COMPUTE Rs-Qty (Ws-Res-Cnt) = FUNCTION NUMVAL (Ws-Col (4))
     MOVE Ws-Col (5) TO Rs-Uom (Ws-Res-Cnt)
     MOVE Ws-Col (3) TO Rs-Planned (Ws-Res-Cnt)
     MOVE "N" TO Rs-Substitute (Ws-Res-Cnt)
     MOVE "REQUIRED" TO Rs-Event (Ws-Res-Cnt)
     IF Ws-Col (7) (1:8) NUMERIC
         MOVE Ws-Col (7) (1:8) TO Rs-When (Ws-Res-Cnt)
     ELSE
         MOVE 0 TO Rs-When (Ws-Res-Cnt)
     END-IF
     MOVE Ws-Col (8) TO Rs-Operator (Ws-Res-Cnt)
     MOVE SPACES TO Rs-Note (Ws-Res-Cnt)
     STRING "Open kit in status " FUNCTION TRIM(Ws-Col (6))
            " line " FUNCTION TRIM(Ws-Col (9))
         DELIMITED BY SIZE INTO Rs-Note (Ws-Res-Cnt)
     PERFORM Note-Kit.

*>----------------------------------------------------------------
*> Paging.  The trace is re-run on every call and the window is
*> cut from the answer - see docs\tech-debt\kit.md.
 Fill-Window.
     MOVE Ws-Res-Cnt TO GN-LINE-TOTAL
     MOVE Ws-Kit-Cnt TO GN-KIT-CNT
     MOVE Ws-Res-Trunc TO GN-TRUNCATED
     MOVE 0 TO GN-WIN-CNT
     MOVE "N" TO GN-MORE-LINES
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > 20
         MOVE 0 TO GN-L-SEQ (Ws-W)
         MOVE 0 TO GN-L-DEPTH (Ws-W)
         MOVE SPACES TO GN-L-KIT-REF (Ws-W)
         MOVE SPACES TO GN-L-PARENT (Ws-W)
         MOVE SPACES TO GN-L-PAR-LOT (Ws-W)
         MOVE SPACES TO GN-L-PAR-SERIAL (Ws-W)
         MOVE SPACES TO GN-L-ITEM (Ws-W)
         MOVE SPACES TO GN-L-LOT (Ws-W)
         MOVE SPACES TO GN-L-SERIAL (Ws-W)
         MOVE 0 TO GN-L-QTY (Ws-W)
         MOVE SPACES TO GN-L-UOM (Ws-W)
         MOVE SPACES TO GN-L-PLANNED (Ws-W)
         MOVE "N" TO GN-L-SUBSTITUTE (Ws-W)
         MOVE SPACES TO GN-L-EVENT (Ws-W)
         MOVE 0 TO GN-L-WHEN (Ws-W)
         MOVE SPACES TO GN-L-OPERATOR (Ws-W)
         MOVE SPACES TO GN-L-NOTE (Ws-W)
     END-PERFORM
     MOVE GN-FROM-LINE TO Ws-I
     PERFORM UNTIL Ws-I > Ws-Res-Cnt
                OR GN-WIN-CNT >= GN-WIN-MAX
         ADD 1 TO GN-WIN-CNT
         MOVE GN-WIN-CNT TO Ws-J
         MOVE Ws-I TO GN-L-SEQ (Ws-J)
         MOVE Rs-Depth (Ws-I) TO GN-L-DEPTH (Ws-J)
         MOVE Rs-Kit-Ref (Ws-I) TO GN-L-KIT-REF (Ws-J)
         MOVE Rs-Parent (Ws-I) TO GN-L-PARENT (Ws-J)
         MOVE Rs-Par-Lot (Ws-I) TO GN-L-PAR-LOT (Ws-J)
         MOVE Rs-Par-Serial (Ws-I) TO GN-L-PAR-SERIAL (Ws-J)
         MOVE Rs-Item (Ws-I) TO GN-L-ITEM (Ws-J)
         MOVE Rs-Lot (Ws-I) TO GN-L-LOT (Ws-J)
         MOVE Rs-Serial (Ws-I) TO GN-L-SERIAL (Ws-J)
         MOVE Rs-Qty (Ws-I) TO GN-L-QTY (Ws-J)
         MOVE Rs-Uom (Ws-I) TO GN-L-UOM (Ws-J)
         MOVE Rs-Planned (Ws-I) TO GN-L-PLANNED (Ws-J)
         MOVE Rs-Substitute (Ws-I) TO GN-L-SUBSTITUTE (Ws-J)
         MOVE Rs-Event (Ws-I) TO GN-L-EVENT (Ws-J)
         MOVE Rs-When (Ws-I) TO GN-L-WHEN (Ws-J)
         MOVE Rs-Operator (Ws-I) TO GN-L-OPERATOR (Ws-J)
         MOVE Rs-Note (Ws-I) TO GN-L-NOTE (Ws-J)
         ADD 1 TO Ws-I
     END-PERFORM
     IF Ws-I <= Ws-Res-Cnt
         MOVE "Y" TO GN-MORE-LINES
     END-IF.

 Summarise.
     MOVE SPACES TO Ws-Summary
     MOVE Ws-Res-Cnt TO Ws-Cnt-Edit
     MOVE GN-DEPTH-REACHED TO Ws-Dep-Edit
     EVALUATE CA-FUNCTION
         WHEN "FORWARD"
             STRING FUNCTION TRIM(Ws-Cnt-Edit)
                    " consumption records to depth "
                    FUNCTION TRIM(Ws-Dep-Edit)
                 DELIMITED BY SIZE INTO Ws-Summary
         WHEN "BACKWARD"
             STRING FUNCTION TRIM(Ws-Cnt-Edit)
                    " parent records to depth "
                    FUNCTION TRIM(Ws-Dep-Edit)
                 DELIMITED BY SIZE INTO Ws-Summary
         WHEN OTHER
             STRING FUNCTION TRIM(Ws-Cnt-Edit)
                    " open kits still require this component"
                 DELIMITED BY SIZE INTO Ws-Summary
     END-EVALUATE
     PERFORM Append-Counts
     MOVE Ws-Summary TO GN-SUMMARY-TEXT
     IF Ws-Res-Trunc = "Y"
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0026" TO CA-REASON-CODE
         MOVE "The trace was cut short - narrow the identity or reduce the depth."
              TO CA-MESSAGE
     END-IF.

 Append-Counts.
     MOVE GN-SUBSTITUTE-CNT TO Ws-Cnt-Edit
     STRING FUNCTION TRIM(Ws-Summary)
            "; substitutions " FUNCTION TRIM(Ws-Cnt-Edit)
         DELIMITED BY SIZE INTO GN-SUMMARY-TEXT
     MOVE GN-SUMMARY-TEXT TO Ws-Summary
     MOVE GN-RETURN-CNT TO Ws-Cnt-Edit
     STRING FUNCTION TRIM(Ws-Summary)
            ", returns " FUNCTION TRIM(Ws-Cnt-Edit)
         DELIMITED BY SIZE INTO GN-SUMMARY-TEXT
     MOVE GN-SUMMARY-TEXT TO Ws-Summary
     MOVE GN-SCRAP-CNT TO Ws-Cnt-Edit
     STRING FUNCTION TRIM(Ws-Summary)
            ", scrap " FUNCTION TRIM(Ws-Cnt-Edit)
         DELIMITED BY SIZE INTO GN-SUMMARY-TEXT
     MOVE GN-SUMMARY-TEXT TO Ws-Summary
     MOVE GN-KIT-CNT TO Ws-Cnt-Edit
     STRING FUNCTION TRIM(Ws-Summary)
            ", kits " FUNCTION TRIM(Ws-Cnt-Edit) "."
         DELIMITED BY SIZE INTO GN-SUMMARY-TEXT
     MOVE GN-SUMMARY-TEXT TO Ws-Summary.
 END PROGRAM GENEALOG.
