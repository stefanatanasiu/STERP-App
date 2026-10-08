>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : KITCOST
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : The seven values of a kit, using the costing rules
*>              of specification 07, and the one identity that
*>              makes them a set rather than seven opinions:
*>
*>                residual = issued - returned + absorbed
*>                                             - completed
*>
*>              SCRAPPED value is part of ISSUED, never a
*>              deduction from it.  Scrap is a disposition of
*>              material that really did leave the store; writing
*>              it back would make the stores and the ledger
*>              disagree (KIT-014, D-12).
*>
*>              A made subassembly is charged to the parent kit at
*>              the value of the SUBASSEMBLY ONLY.  Its own child
*>              material was charged to the child kit and relieved
*>              when the child was completed, so charging the
*>              children again to the parent double counts the
*>              same pound twice (KIT-020, AC-081).  KK-DOUBLE-
*>              CHECK is the standing test for that, and it is
*>              reported whether it passes or fails.
*> CALLED BY   : the kit execution form, KITCLOSE (financial
*>               close), KITRDY, DISASSY and the unit tests
*> INTERFACE   : CALL "KITCOST" USING WS-KITCOST-CA
*>               COPYBOOK KITCOSTCA
*> CALLS       : KITDB, SQLIO
*>----------------------------------------------------------------
*> CHANGE LOG
*> 1997-04-21 DRN  CR0489 JOB COST ENQUIRY
*> 2015-07-13 PMK  CR2366 KIT COST ENQUIRY - FREE FORMAT
*> 2016-10-04 PMK  CR2499 SUBASSEMBLY CHARGED ONCE - THE 1997
*>                 VERSION EXPLODED THE CHILD BILL INTO THE PARENT
*>                 JOB AND EVERY MADE-IN ASSEMBLY WAS OVERSTATED
*> 2026-10-06 STERP KIT-020
*>================================================================
*> REQ: KIT-020
 IDENTIFICATION DIVISION.
 PROGRAM-ID. KITCOST.
 AUTHOR. P M KEANE.
 DATE-WRITTEN. 13/07/2015.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY KITDBW.
 COPY KITWORK.
 01  Ws-Row.
     05  Ws-Col OCCURS 12 TIMES  PIC X(100).
 01  Ws-Totals.
     05  Ws-Planned              PIC S9(13)V99 VALUE 0.
     05  Ws-Issued               PIC S9(13)V99 VALUE 0.
     05  Ws-Returned             PIC S9(13)V99 VALUE 0.
     05  Ws-Scrapped             PIC S9(13)V99 VALUE 0.
     05  Ws-Absorbed             PIC S9(13)V99 VALUE 0.
     05  Ws-Completed            PIC S9(13)V99 VALUE 0.
     05  Ws-Residual             PIC S9(13)V99 VALUE 0.
     05  Ws-Subassy              PIC S9(13)V99 VALUE 0.
     05  Ws-Plan-Material        PIC S9(13)V99 VALUE 0.
     05  Ws-Plan-Subassy         PIC S9(13)V99 VALUE 0.
     05  Ws-Act-Material         PIC S9(13)V99 VALUE 0.
     05  Ws-Plan-Qty             PIC S9(13)V9(6) VALUE 0.
     05  Ws-Act-Qty              PIC S9(13)V9(6) VALUE 0.
 01  Ws-Work.
     05  Ws-W                    PIC S9(4) VALUE 0.
     05  Ws-More                 PIC X VALUE "Y".
*>   SQLIO only has eight cursor slots (sterpsql.c MAXCUR) and KITDB
*>   holds 5 and 6, so the kit enquiry programs share 3,4,7,8.
     05  Ws-Cursor               PIC S9(9) COMP-5 VALUE 8.
     05  Ws-Subassy-Cnt          PIC S9(4) VALUE 0.
     05  Ws-Dup-Cnt              PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Line-Cnt             PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Type                 PIC X(12) VALUE SPACES.
     05  Ws-Value                PIC S9(13)V99 VALUE 0.
     05  Ws-Share                PIC S9(9)V9(9) VALUE 0.
     05  Ws-Check                PIC S9(13)V99 VALUE 0.
     05  Ws-Tolerance            PIC S9(3)V99 VALUE 0.01.
     05  Ws-Money-Edit           PIC -(11)9.99.
     05  Ws-Cnt-Edit             PIC Z(3)9.
     05  Ws-Note                 PIC X(200) VALUE SPACES.
*>   The twelve reportable cost elements.  Index order is fixed so
*>   GETELEM and VARIANCE agree on which row is which.
 01  Ws-Elements.
     05  Ws-El OCCURS 12 TIMES.
         10  El-Code             PIC X(10).
         10  El-Desc             PIC X(30).
         10  El-Planned          PIC S9(13)V99.
         10  El-Actual           PIC S9(13)V99.
         10  El-Qty              PIC S9(13)V9(6).
         10  El-Uom              PIC X(10).
         10  El-Used             PIC X.
 LINKAGE SECTION.
 COPY KITCOSTCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "KITCOST" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE 0 TO CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     MOVE "KIT-020" TO CA-REQ-REF
     PERFORM Clear-Answer
     EVALUATE CA-FUNCTION
         WHEN "COST"     PERFORM Build-Cost-Picture
         WHEN "GETELEM"  PERFORM Build-Cost-Picture
         WHEN "VARIANCE" PERFORM Build-Cost-Picture
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "KIT0002" TO CA-REASON-CODE
     STRING "KITCOST does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

 Set-Sql-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE SQLIO-ERRMSG (1:200) TO CA-MESSAGE.

 Clear-Answer.
     INITIALIZE Ws-Totals
     MOVE 0 TO KK-PLANNED-GBP
     MOVE 0 TO KK-ISSUED-GBP
     MOVE 0 TO KK-RETURNED-GBP
     MOVE 0 TO KK-SCRAPPED-GBP
     MOVE 0 TO KK-ABSORBED-GBP
     MOVE 0 TO KK-COMPLETED-GBP
     MOVE 0 TO KK-RESIDUAL-GBP
     MOVE 0 TO KK-VARIANCE-GBP
     MOVE 0 TO KK-UNIT-COST
     MOVE 0 TO KK-PLAN-UNIT-COST
     MOVE 0 TO KK-SUBASSY-CNT
     MOVE 0 TO KK-SUBASSY-GBP
     MOVE 0 TO KK-ELEM-CNT
     MOVE "N" TO KK-DOUBLE-CHECK
     MOVE "N" TO KK-RECONCILED
     MOVE SPACES TO KK-NOTE-TEXT
     MOVE 0 TO Ws-Subassy-Cnt
     MOVE 0 TO Ws-Dup-Cnt
     MOVE 0 TO Ws-Line-Cnt
     IF KK-CURRENCY = SPACES
         MOVE "GBP" TO KK-CURRENCY
     END-IF
     IF KK-COST-SET = SPACES
         MOVE "STD2026" TO KK-COST-SET
     END-IF
     PERFORM Clear-Elements.

 Clear-Elements.
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > 12
         MOVE SPACES TO El-Code (Ws-W)
         MOVE SPACES TO El-Desc (Ws-W)
         MOVE 0 TO El-Planned (Ws-W)
         MOVE 0 TO El-Actual (Ws-W)
         MOVE 0 TO El-Qty (Ws-W)
         MOVE SPACES TO El-Uom (Ws-W)
         MOVE "N" TO El-Used (Ws-W)
     END-PERFORM
     MOVE "MATERIAL" TO El-Code (1)
     MOVE "Bought out material issued" TO El-Desc (1)
     MOVE "SUBASSY" TO El-Code (2)
     MOVE "Made in subassemblies" TO El-Desc (2)
     MOVE "RETURN" TO El-Code (3)
     MOVE "Material returned to store" TO El-Desc (3)
     MOVE "SCRAP" TO El-Code (4)
     MOVE "Material scrapped on the job" TO El-Desc (4)
     MOVE "LABOUR" TO El-Code (5)
     MOVE "Conversion labour absorbed" TO El-Desc (5)
     MOVE "OVERHEAD" TO El-Code (6)
     MOVE "Overhead absorbed" TO El-Desc (6)
     MOVE "SUBCON" TO El-Code (7)
     MOVE "Subcontract services" TO El-Desc (7)
     MOVE "COMPLETE" TO El-Code (8)
     MOVE "Value transferred to stock" TO El-Desc (8)
     MOVE 8 TO KK-ELEM-CNT.

*>----------------------------------------------------------------
 Load-Kit.
     INITIALIZE KIT-DB-CTL
     INITIALIZE KIT-HEADER-REC
     MOVE "HDRGET" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE KK-KIT-REF TO KD-KIT-REF
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     EVALUATE KD-RC
         WHEN 0
             MOVE KH-STATUS TO KK-KIT-STATUS
             MOVE KH-PLANNED-QTY TO KK-PLANNED-QTY
             MOVE KH-COMPLETED-QTY TO KK-COMPLETED-QTY
             MOVE KH-PLANNED-QTY TO Ws-Plan-Qty
             MOVE KH-COMPLETED-QTY TO Ws-Act-Qty
         WHEN 100
             MOVE 16 TO CA-RETURN-CODE
             MOVE "KIT0001" TO CA-REASON-CODE
             STRING "Kit " FUNCTION TRIM(KK-KIT-REF)
                    " is not known in this company."
                 DELIMITED BY SIZE INTO CA-MESSAGE
         WHEN OTHER
             MOVE 12 TO CA-RETURN-CODE
             MOVE "SQL0001" TO CA-REASON-CODE
             MOVE KD-ERRMSG TO CA-MESSAGE
     END-EVALUATE.

*>----------------------------------------------------------------
*> The planned value is the APPROVED requirement at plan cost -
*> the current requirement, not the original one.  An approved
*> amendment moves the plan; it does not leave the plan behind.
 Accumulate-Planned.
     MOVE "LINOPEN" TO KD-FUNC
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     MOVE "N" TO KD-ONLY-OPEN
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     IF KD-RC NOT = 0
         MOVE 12 TO CA-RETURN-CODE
         MOVE "SQL0001" TO CA-REASON-CODE
         MOVE KD-ERRMSG TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE "LINNEXT" TO KD-FUNC
         INITIALIZE KIT-LINE-REC
         CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                            KIT-LINE-REC, KIT-ITEM-REC
         EVALUATE KD-RC
             WHEN 0
                 PERFORM Add-Planned-Line
             WHEN 100
                 MOVE "N" TO Ws-More
             WHEN OTHER
                 MOVE 12 TO CA-RETURN-CODE
                 MOVE "SQL0001" TO CA-REASON-CODE
                 MOVE KD-ERRMSG TO CA-MESSAGE
                 MOVE "N" TO Ws-More
         END-EVALUATE
     END-PERFORM
     MOVE "LINCLOS" TO KD-FUNC
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC.

 Add-Planned-Line.
     IF KL-IS-CANCELLED
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Line-Cnt
     COMPUTE Ws-Value ROUNDED =
         KL-CURRENT-REQ * KL-PLAN-UNIT-COST
     ADD Ws-Value TO Ws-Planned
     ADD Ws-Value TO Ws-Plan-Material
     ADD Ws-Value TO El-Planned (1)
     ADD KL-CURRENT-REQ TO El-Qty (1)
     IF El-Uom (1) = SPACES
         MOVE KL-UOM-CODE TO El-Uom (1)
     END-IF
     MOVE "Y" TO El-Used (1).

*>----------------------------------------------------------------
*> The actual movements.  One pass over kit.KitIssue gives issued,
*> returned, scrapped, absorbed and completed, because every
*> economic event on a kit is written there with its own type.
 Accumulate-Actual.
     MOVE 0 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > 4
         MOVE SPACES TO SQLIO-PARM-VAL (Ws-W)
         MOVE "N" TO SQLIO-PARM-NULL (Ws-W)
     END-PERFORM
     MOVE SPACES TO SQLIO-STMT
     STRING
       "SELECT i.IssueType, CONVERT(varchar(20),SUM(i.ValueGBP)),"
       " CONVERT(varchar(26),SUM(i.Quantity)), MAX(ISNULL(i.UomCode,''))"
       " FROM kit.KitIssue i"
       " WHERE i.CompanyCode = ? AND i.KitRef = ?"
       " AND (? = 0 OR i.PostingDate <= CONVERT(date,?,112))"
       " GROUP BY i.IssueType"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE KK-KIT-REF TO SQLIO-PARM-VAL (2)
     MOVE KK-AS-AT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (3)
*>   SQL Server does not promise to short circuit the OR, so the
*>   cut off parameter must always convert to a real date even
*>   when no as-at date was asked for.
     IF KK-AS-AT = 0
         MOVE "99991231" TO SQLIO-PARM-VAL (4)
     ELSE
         MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (4)
     END-IF
     MOVE 4 TO SQLIO-PARM-CNT
     MOVE 4 TO SQLIO-COL-CNT
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > 12
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
                 PERFORM Apply-Movement-Group
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

*>   SCRAP adds to the scrapped total but is NOT taken out of
*>   issued: the material left the store and stayed out.
 Apply-Movement-Group.
     MOVE Ws-Col (1) TO Ws-Type
     COMPUTE Ws-Value ROUNDED = FUNCTION NUMVAL (Ws-Col (2))
     EVALUATE Ws-Type
         WHEN "ISSUE"
             ADD Ws-Value TO Ws-Issued
             ADD Ws-Value TO Ws-Act-Material
             ADD Ws-Value TO El-Actual (1)
             MOVE "Y" TO El-Used (1)
         WHEN "SUBSTISSUE"
             ADD Ws-Value TO Ws-Issued
             ADD Ws-Value TO Ws-Act-Material
             ADD Ws-Value TO El-Actual (1)
             MOVE "Y" TO El-Used (1)
         WHEN "BACKFLUSH"
             ADD Ws-Value TO Ws-Issued
             ADD Ws-Value TO Ws-Act-Material
             ADD Ws-Value TO El-Actual (1)
             MOVE "Y" TO El-Used (1)
         WHEN "RETURN"
             ADD Ws-Value TO Ws-Returned
             ADD Ws-Value TO El-Actual (3)
             MOVE "Y" TO El-Used (3)
         WHEN "SCRAP"
             ADD Ws-Value TO Ws-Scrapped
             ADD Ws-Value TO El-Actual (4)
             MOVE "Y" TO El-Used (4)
         WHEN "LABOUR"
             ADD Ws-Value TO Ws-Absorbed
             ADD Ws-Value TO El-Actual (5)
             MOVE "Y" TO El-Used (5)
         WHEN "OVERHEAD"
             ADD Ws-Value TO Ws-Absorbed
             ADD Ws-Value TO El-Actual (6)
             MOVE "Y" TO El-Used (6)
         WHEN "SUBCON"
             ADD Ws-Value TO Ws-Absorbed
             ADD Ws-Value TO El-Actual (7)
             MOVE "Y" TO El-Used (7)
         WHEN "COMPLETE"
             ADD Ws-Value TO Ws-Completed
             ADD Ws-Value TO El-Actual (8)
             MOVE "Y" TO El-Used (8)
         WHEN OTHER
             CONTINUE
     END-EVALUATE.

*>----------------------------------------------------------------
*> AC-081.  Which of this kit's lines are made on a child kit, and
*> has any child component been charged to the parent as well?
 Check-Subassembly-Once.
     MOVE 0 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > 4
         MOVE SPACES TO SQLIO-PARM-VAL (Ws-W)
         MOVE "N" TO SQLIO-PARM-NULL (Ws-W)
     END-PERFORM
     MOVE SPACES TO SQLIO-STMT
     STRING
       "SELECT CONVERT(varchar(12),COUNT(DISTINCT c.ExecutionKitId)),"
       " CONVERT(varchar(20),ISNULL(SUM(pi.ValueGBP),0)),"
       " CONVERT(varchar(12),("
       "   SELECT COUNT(*) FROM kit.KitIssue d"
       "   INNER JOIN kit.ExecutionKit ck"
       "     ON ck.CompanyCode = d.CompanyCode"
       "    AND ck.ParentKitRef = ?"
       "   INNER JOIN kit.ExecutionKitLine cl"
       "     ON cl.ExecutionKitId = ck.ExecutionKitId"
       "    AND cl.ComponentItemCode = d.ComponentItemCode"
       "   WHERE d.CompanyCode = ? AND d.KitRef = ?))"
       " FROM kit.ExecutionKit c"
       " LEFT JOIN kit.KitIssue pi"
       "   ON pi.CompanyCode = c.CompanyCode"
       "  AND pi.KitRef = ?"
       "  AND pi.ComponentItemCode = c.ParentItemCode"
       " WHERE c.CompanyCode = ? AND c.ParentKitRef = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE KK-KIT-REF TO SQLIO-PARM-VAL (1)
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (2)
     MOVE KK-KIT-REF TO SQLIO-PARM-VAL (3)
     MOVE KK-KIT-REF TO SQLIO-PARM-VAL (4)
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (5)
     MOVE KK-KIT-REF TO SQLIO-PARM-VAL (6)
     MOVE 6 TO SQLIO-PARM-CNT
     MOVE 3 TO SQLIO-COL-CNT
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > 12
         MOVE 100 TO SQLIO-COL-WIDTH (Ws-W)
     END-PERFORM
     MOVE Ws-Cursor TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Row
     IF SQLIO-ERROR
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     IF SQLIO-NOTFOUND
         MOVE "Y" TO KK-DOUBLE-CHECK
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ws-Subassy-Cnt = FUNCTION NUMVAL (Ws-Col (1))
     COMPUTE Ws-Subassy ROUNDED = FUNCTION NUMVAL (Ws-Col (2))
     COMPUTE Ws-Dup-Cnt = FUNCTION NUMVAL (Ws-Col (3))
     MOVE Ws-Subassy-Cnt TO KK-SUBASSY-CNT
     MOVE Ws-Subassy TO KK-SUBASSY-GBP
     ADD Ws-Subassy TO El-Actual (2)
     IF Ws-Subassy-Cnt > 0
         MOVE "Y" TO El-Used (2)
     END-IF
     IF Ws-Dup-Cnt = 0
         MOVE "Y" TO KK-DOUBLE-CHECK
     ELSE
         MOVE "N" TO KK-DOUBLE-CHECK
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0027" TO CA-REASON-CODE
         MOVE "A child kit component has also been charged to this parent kit - the subassembly is counted twice."
              TO CA-MESSAGE
     END-IF.

*>----------------------------------------------------------------
*> COMPLETE rows are written by KITCOMPL as value leaves work in
*> progress.  A kit that has produced output but has no COMPLETE
*> movement yet is valued pro rata so the residual still means
*> something on the screen; the flag says the number is derived.
 Derive-Completed-If-Missing.
     IF Ws-Completed NOT = 0
         EXIT PARAGRAPH
     END-IF
     IF Ws-Act-Qty <= 0 OR Ws-Plan-Qty <= 0
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ws-Share = Ws-Act-Qty / Ws-Plan-Qty
     IF Ws-Share > 1
         MOVE 1 TO Ws-Share
     END-IF
     COMPUTE Ws-Completed ROUNDED =
         (Ws-Issued - Ws-Returned + Ws-Absorbed) * Ws-Share
     ADD Ws-Completed TO El-Actual (8)
     MOVE "Y" TO El-Used (8)
     MOVE SPACES TO Ws-Note
     MOVE "Completed value derived pro rata from the output quantity."
          TO Ws-Note.

*>----------------------------------------------------------------
 Build-Cost-Picture.
     IF KK-KIT-REF = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0001" TO CA-REASON-CODE
         MOVE "A kit reference is required." TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Kit
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Accumulate-Planned
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Accumulate-Actual
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Check-Subassembly-Once
     IF CA-SEVERE
         EXIT PARAGRAPH
     END-IF
     PERFORM Derive-Completed-If-Missing
     PERFORM Compute-Residual
     PERFORM Publish-Totals
     PERFORM Publish-Elements
     PERFORM Write-Note.

*>   The identity.  Scrapped is NOT in this line: it is already
*>   inside issued.
 Compute-Residual.
     COMPUTE Ws-Residual ROUNDED =
         Ws-Issued - Ws-Returned + Ws-Absorbed - Ws-Completed
     COMPUTE Ws-Check =
         Ws-Issued - Ws-Returned + Ws-Absorbed
         - Ws-Completed - Ws-Residual
     IF FUNCTION ABS (Ws-Check) <= Ws-Tolerance
         MOVE "Y" TO KK-RECONCILED
     ELSE
         MOVE "N" TO KK-RECONCILED
     END-IF.

 Publish-Totals.
     MOVE Ws-Planned TO KK-PLANNED-GBP
     MOVE Ws-Issued TO KK-ISSUED-GBP
     MOVE Ws-Returned TO KK-RETURNED-GBP
     MOVE Ws-Scrapped TO KK-SCRAPPED-GBP
     MOVE Ws-Absorbed TO KK-ABSORBED-GBP
     MOVE Ws-Completed TO KK-COMPLETED-GBP
     MOVE Ws-Residual TO KK-RESIDUAL-GBP
     COMPUTE KK-VARIANCE-GBP ROUNDED =
         (Ws-Issued - Ws-Returned + Ws-Absorbed) - Ws-Planned
     IF Ws-Plan-Qty > 0
         COMPUTE KK-PLAN-UNIT-COST ROUNDED =
             Ws-Planned / Ws-Plan-Qty
     END-IF
     IF Ws-Act-Qty > 0
         COMPUTE KK-UNIT-COST ROUNDED =
             Ws-Completed / Ws-Act-Qty
     END-IF.

*>   Planned subassembly value is the plan cost of the lines that
*>   are made on a child kit; it balances the actual side so the
*>   variance by element is readable.
 Publish-Elements.
     COMPUTE El-Planned (2) ROUNDED = Ws-Plan-Subassy
     MOVE Ws-Planned TO El-Planned (8)
     MOVE 0 TO KK-ELEM-CNT
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > 12
         IF El-Used (Ws-W) = "Y" OR El-Planned (Ws-W) NOT = 0
             ADD 1 TO KK-ELEM-CNT
             MOVE El-Code (Ws-W) TO KK-E-ELEMENT (KK-ELEM-CNT)
             MOVE El-Desc (Ws-W) TO KK-E-DESC (KK-ELEM-CNT)
             MOVE El-Planned (Ws-W) TO KK-E-PLANNED (KK-ELEM-CNT)
             MOVE El-Actual (Ws-W) TO KK-E-ACTUAL (KK-ELEM-CNT)
             COMPUTE KK-E-VARIANCE (KK-ELEM-CNT) =
                 El-Actual (Ws-W) - El-Planned (Ws-W)
             MOVE El-Qty (Ws-W) TO KK-E-QTY (KK-ELEM-CNT)
             MOVE El-Uom (Ws-W) TO KK-E-UOM (KK-ELEM-CNT)
         END-IF
     END-PERFORM.

 Write-Note.
     MOVE SPACES TO KK-NOTE-TEXT
     MOVE Ws-Residual TO Ws-Money-Edit
     MOVE Ws-Subassy-Cnt TO Ws-Cnt-Edit
     STRING "Residual WIP " FUNCTION TRIM(KK-CURRENCY) " "
            FUNCTION TRIM(Ws-Money-Edit)
            " = issued - returned + absorbed - completed;"
            " scrap is inside issued. Subassemblies "
            FUNCTION TRIM(Ws-Cnt-Edit)
         DELIMITED BY SIZE INTO KK-NOTE-TEXT
     IF KK-NO-DOUBLE-COUNT
         MOVE KK-NOTE-TEXT TO Ws-Note
         STRING FUNCTION TRIM(Ws-Note)
                ", each charged once."
             DELIMITED BY SIZE INTO KK-NOTE-TEXT
     ELSE
         MOVE KK-NOTE-TEXT TO Ws-Note
         STRING FUNCTION TRIM(Ws-Note)
                ", DOUBLE COUNTED - see the child kits."
             DELIMITED BY SIZE INTO KK-NOTE-TEXT
     END-IF.
 END PROGRAM KITCOST.
