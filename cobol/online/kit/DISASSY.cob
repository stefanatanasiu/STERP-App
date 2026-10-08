>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : DISASSY
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : Controlled disassembly of a stocked assembly.
*>
*>              Disassembly is NOT a reversed completion.  The
*>              parent is consumed, the components that actually
*>              came back are received in INSPECTION condition,
*>              and the value rule of D-12 is absolute:
*>
*>                recovered value + recognised loss
*>                    = parent carrying value
*>                      + approved disassembly costs
*>
*>              So recovery can never invent carrying value.  The
*>              planned bill is a proposal, not proof that every
*>              component came back undamaged, which is why
*>              planned and recovered quantities are separate
*>              columns on every line (KIT-021).
*>
*>              A commercial kit parent has no stock balance and
*>              therefore nothing to disassemble (KIT-001).
*> CALLED BY   : the disassembly form and the unit tests
*> INTERFACE   : CALL "DISASSY" USING WS-DISASSY-CA
*>               COPYBOOK DISASSYCA
*> CALLS       : NUMGEN, KITDB, STKMOVE, SQLIO
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2015-09-07 PMK  CR2381 CONTROLLED DISASSEMBLY
*> 2016-04-11 PMK  CR2476 RECOVERY CAPPED AT CARRYING VALUE PLUS
*>                 APPROVED COSTS - THE 2015 VERSION ALLOCATED AT
*>                 STANDARD AND MANUFACTURED A CREDIT
*> 2026-10-06 STERP KIT-021, D-12
*>================================================================
*> REQ: KIT-021
 IDENTIFICATION DIVISION.
 PROGRAM-ID. DISASSY.
 AUTHOR. P M KEANE.
 DATE-WRITTEN. 07/09/2015.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY KITDBW.
 COPY KITWORK.
 COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==Ws-Numgen-Ca==
      LEADING ==CA-== BY ==NA-==.
 COPY STKMOVECA REPLACING ==DFHCOMMAREA== BY ==Ws-Move-Ca==
      LEADING ==CA-== BY ==MA-==.
 01  Ws-Row.
     05  Ws-Col OCCURS 14 TIMES  PIC X(100).
 01  Ws-Work.
     05  Ws-W                    PIC S9(4) VALUE 0.
     05  Ws-C                    PIC S9(4) VALUE 0.
     05  Ws-In-Recovered         PIC S9(11)V99 VALUE 0.
     05  Ws-In-Approver          PIC X(20) VALUE SPACES.
     05  Ws-More                 PIC X VALUE "Y".
*>   SQLIO only has eight cursor slots (sterpsql.c MAXCUR) and KITDB
*>   holds 5 and 6, so the kit enquiry programs share 3,4,7,8.
     05  Ws-Cursor               PIC S9(9) COMP-5 VALUE 4.
     05  Ws-Available            PIC S9(13)V99 VALUE 0.
     05  Ws-Recovered            PIC S9(13)V99 VALUE 0.
     05  Ws-Loss                 PIC S9(13)V99 VALUE 0.
     05  Ws-Basis-Total          PIC S9(13)V9(6) VALUE 0.
     05  Ws-Line-Basis           PIC S9(13)V9(6) VALUE 0.
     05  Ws-Alloc                PIC S9(13)V99 VALUE 0.
     05  Ws-Running              PIC S9(13)V99 VALUE 0.
     05  Ws-Largest              PIC S9(4) VALUE 0.
     05  Ws-Largest-Val          PIC S9(13)V99 VALUE 0.
     05  Ws-Count                PIC S9(4) VALUE 0.
     05  Ws-Posted               PIC S9(4) VALUE 0.
     05  Ws-Status               PIC X(12) VALUE SPACES.
     05  Ws-Money-Edit           PIC -(11)9.99.
     05  Ws-Note                 PIC X(200) VALUE SPACES.
*>   COM-015.  inv.usp_PostStockMovement keys core.IdempotencyKey on
*>   the correlation id by itself, so the parent issue and every
*>   component receipt of one disassembly must each carry a derived
*>   id.  The caller's id is cut to 30 characters first: it is
*>   normally a 36 character guid and the suffix would otherwise be
*>   truncated away, putting the whole order back on one key.
     05  Ws-Corr-Stem            PIC X(30) VALUE SPACES.
     05  Ws-Sub-Corr             PIC X(36) VALUE SPACES.
     05  Ws-Sub-Suffix           PIC X(6) VALUE SPACES.
     05  Ws-Sub-Seq              PIC 9(3) VALUE 0.
*>   Lines are loaded into store so VALUE can allocate across the
*>   whole order and still give the rounding residual to one line.
 01  Ws-Lines.
     05  Ws-Ln-Cnt               PIC S9(4) VALUE 0.
     05  Ws-Ln OCCURS 60 TIMES.
         10  Ln-Id               PIC S9(9) COMP-5.
         10  Ln-Num              PIC S9(4).
         10  Ln-Item             PIC X(40).
         10  Ln-Rev              PIC X(4).
         10  Ln-Lot              PIC X(30).
         10  Ln-Serial           PIC X(40).
         10  Ln-Planned          PIC S9(13)V9(6).
         10  Ln-Recovered        PIC S9(13)V9(6).
         10  Ln-Damaged          PIC S9(13)V9(6).
         10  Ln-Uom              PIC X(10).
         10  Ln-Quality          PIC X(12).
         10  Ln-Value            PIC S9(11)V99.
         10  Ln-Unit-Val         PIC S9(11)V9(6).
         10  Ln-Location         PIC X(20).
 LINKAGE SECTION.
 COPY DISASSYCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "DISASSY" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE 0 TO CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     MOVE "KIT-021" TO CA-REQ-REF
     MOVE SPACES TO DS-IMPACT-TEXT
     MOVE "N" TO DS-BALANCED
     MOVE 0 TO Ws-Ln-Cnt
     IF DS-DISASSY-DATE = 0
         MOVE CA-BUSINESS-DATE TO DS-DISASSY-DATE
     END-IF
     IF DS-POSTING-DATE = 0
         MOVE DS-DISASSY-DATE TO DS-POSTING-DATE
     END-IF
     EVALUATE CA-FUNCTION
         WHEN "CREATE"   PERFORM Create-Order
         WHEN "ADDLINE"  PERFORM Add-One-Line
         WHEN "VALUE"    PERFORM Allocate-Value
         WHEN "APPROVE"  PERFORM Approve-Order
         WHEN "POST"     PERFORM Post-Order
         WHEN "ENQUIRE"  PERFORM Enquire-Order
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "KIT0002" TO CA-REASON-CODE
     STRING "DISASSY does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

 Set-Sql-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE SQLIO-ERRMSG (1:200) TO CA-MESSAGE.

 Clear-Parms.
     MOVE 0 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-C FROM 1 BY 1 UNTIL Ws-C > 20
         MOVE SPACES TO SQLIO-PARM-VAL (Ws-C)
         MOVE "N" TO SQLIO-PARM-NULL (Ws-C)
     END-PERFORM.

 Set-Wide-Columns.
     PERFORM VARYING Ws-C FROM 1 BY 1 UNTIL Ws-C > 14
         MOVE 100 TO SQLIO-COL-WIDTH (Ws-C)
     END-PERFORM
     MOVE Ws-Cursor TO SQLIO-CURSOR.

*>----------------------------------------------------------------
*> A non-stocked parent has no carrying value to take apart.  That
*> is the commercial kit case and it is refused outright.
 Validate-Parent.
     INITIALIZE KIT-DB-CTL
     MOVE "ITEMGET" TO KD-FUNC
     MOVE CA-COMPANY TO KD-COMPANY
     MOVE CA-USER TO KD-USER
     MOVE DS-PARENT-ITEM TO KD-ITEM-CODE
     CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
                        KIT-LINE-REC, KIT-ITEM-REC
     EVALUATE KD-RC
         WHEN 0
             CONTINUE
         WHEN 100
             MOVE 16 TO CA-RETURN-CODE
             MOVE "KIT0001" TO CA-REASON-CODE
             STRING "Item " FUNCTION TRIM(DS-PARENT-ITEM)
                    " is not known in this company."
                 DELIMITED BY SIZE INTO CA-MESSAGE
             EXIT PARAGRAPH
         WHEN OTHER
             MOVE 12 TO CA-RETURN-CODE
             MOVE "SQL0001" TO CA-REASON-CODE
             MOVE KD-ERRMSG TO CA-MESSAGE
             EXIT PARAGRAPH
     END-EVALUATE
     IF NOT KI-STOCKED
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0006" TO CA-REASON-CODE
         STRING "Item " FUNCTION TRIM(DS-PARENT-ITEM)
                " is not stocked, so it has no carrying value to disassemble."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF
     IF CA-RETURN-CODE = 0 AND KI-IS-TRACED
             AND DS-LOT-CODE = SPACES AND DS-SERIAL-NO = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0012" TO CA-REASON-CODE
         MOVE "A traced parent must be identified by lot or serial before it is taken apart."
              TO CA-MESSAGE
     END-IF.

 Validate-Create.
     IF DS-CONSUMED-QTY <= 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "The quantity to disassemble must be greater than zero."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF DS-CARRYING-GBP < 0 OR DS-APPROVED-GBP < 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "Carrying value and approved costs must not be negative."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF DS-REASON-TEXT = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0008" TO CA-REASON-CODE
         MOVE "A reason is required before an assembly is taken apart."
              TO CA-MESSAGE
     END-IF.

 Allocate-Reference.
     INITIALIZE Ws-Numgen-Ca
     MOVE "NEXT" TO NA-FUNCTION
     MOVE CA-COMPANY TO NA-COMPANY
     MOVE DS-SITE TO NA-SITE
     MOVE CA-USER TO NA-USER
     MOVE CA-BUSINESS-DATE TO NA-BUSINESS-DATE
     MOVE CA-CORRELATION-ID TO NA-CORRELATION-ID
     MOVE "DIS" TO NG-SERIES-CODE
     CALL "NUMGEN" USING Ws-Numgen-Ca
     IF NA-RETURN-CODE > 4
         MOVE NA-RETURN-CODE TO CA-RETURN-CODE
         MOVE NA-REASON-CODE TO CA-REASON-CODE
         MOVE NA-MESSAGE TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE NG-DOCUMENT-NUMBER TO DS-DISASSY-REF.

 Create-Order.
     PERFORM Validate-Create
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Validate-Parent
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     IF DS-ALLOC-BASIS = SPACES
         MOVE "STANDARD" TO DS-ALLOC-BASIS
     END-IF
     PERFORM Allocate-Reference
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "INSERT kit.Disassembly (CompanyCode, SiteCode,"
       " DisassemblyRef, ParentItemCode, Revision, LotCode,"
       " SerialNo, ConsumedQty, DisassemblyDate, PostingDate,"
       " BomHeaderId, ParentCarryingValueGBP, ApprovedCostsGBP,"
       " AllocationBasis, Reason, Status, CorrelationId, CreatedBy)"
       " VALUES (?,?,?,?,NULLIF(?,''),NULLIF(?,''),NULLIF(?,''),?,"
       " CONVERT(date,?,112),CONVERT(date,?,112),NULLIF(?,0),?,?,"
       " ?,NULLIF(?,''),'DRAFT',NULLIF(?,''),?);"
       " SELECT CONVERT(varchar(20),SCOPE_IDENTITY())"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE DS-SITE TO SQLIO-PARM-VAL (2)
     MOVE DS-DISASSY-REF TO SQLIO-PARM-VAL (3)
     MOVE DS-PARENT-ITEM TO SQLIO-PARM-VAL (4)
     MOVE DS-REVISION TO SQLIO-PARM-VAL (5)
     MOVE DS-LOT-CODE TO SQLIO-PARM-VAL (6)
     MOVE DS-SERIAL-NO TO SQLIO-PARM-VAL (7)
     MOVE DS-CONSUMED-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (8)
     MOVE DS-DISASSY-DATE TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (9)
     MOVE DS-POSTING-DATE TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (10)
     MOVE DS-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (11)
     MOVE DS-CARRYING-GBP TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (12)
     MOVE DS-APPROVED-GBP TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (13)
     MOVE DS-ALLOC-BASIS TO SQLIO-PARM-VAL (14)
     MOVE DS-REASON-TEXT (1:200) TO SQLIO-PARM-VAL (15)
     MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL (16)
     MOVE CA-USER TO SQLIO-PARM-VAL (17)
     MOVE 17 TO SQLIO-PARM-CNT
     MOVE 1 TO SQLIO-COL-CNT
     PERFORM Set-Wide-Columns
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Row
     IF SQLIO-ERROR
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     COMPUTE DS-DISASSY-ID = FUNCTION NUMVAL (Ws-Col (1))
     MOVE "DRAFT" TO DS-STATUS
     COMPUTE Ws-Available = DS-CARRYING-GBP + DS-APPROVED-GBP
     MOVE Ws-Available TO DS-AVAILABLE-GBP
     MOVE Ws-Available TO Ws-Money-Edit
     MOVE SPACES TO DS-IMPACT-TEXT
     STRING "Disassembly " FUNCTION TRIM(DS-DISASSY-REF)
            " raised. Value available to recover is "
            FUNCTION TRIM(Ws-Money-Edit)
            " (carrying value plus approved costs)."
         DELIMITED BY SIZE INTO DS-IMPACT-TEXT.

*>----------------------------------------------------------------
*> One recovered component.  Recovered and damaged are recorded
*> apart: a part that came back broken is evidence of the loss,
*> not an absence of evidence.
 Add-One-Line.
     PERFORM Load-Order
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     IF Ws-Status NOT = "DRAFT"
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0005" TO CA-REASON-CODE
         MOVE "Lines may only be added to a draft disassembly."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF DS-A-ITEM = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "A component item code is required." TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF DS-A-RECOVERED < 0 OR DS-A-DAMAGED < 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "Recovered and damaged quantities must not be negative."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF DS-A-QUALITY = SPACES
         MOVE "INSPECTION" TO DS-A-QUALITY
     END-IF
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "INSERT kit.DisassemblyLine (DisassemblyId, LineNum,"
       " ComponentItemCode, Revision, LotCode, SerialNo,"
       " PlannedQty, RecoveredQty, DamagedQty, UomCode,"
       " QualityStatus, AllocatedValueGBP, LocationCode)"
       " VALUES (?,"
       " ISNULL(NULLIF(?,0),(SELECT ISNULL(MAX(LineNum),0)+1"
       "   FROM kit.DisassemblyLine WHERE DisassemblyId = ?)),"
       " ?,NULLIF(?,''),NULLIF(?,''),NULLIF(?,''),"
       " NULLIF(CONVERT(decimal(19,6),?),0),?,?,"
       " NULLIF(?,''),?,0,NULLIF(?,''))"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE DS-DISASSY-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (1)
     MOVE DS-A-LINE-NUM TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (2)
     MOVE DS-DISASSY-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (3)
     MOVE DS-A-ITEM TO SQLIO-PARM-VAL (4)
     MOVE DS-A-REV TO SQLIO-PARM-VAL (5)
     MOVE DS-A-LOT TO SQLIO-PARM-VAL (6)
     MOVE DS-A-SERIAL TO SQLIO-PARM-VAL (7)
     MOVE DS-A-PLANNED TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (8)
     MOVE DS-A-RECOVERED TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (9)
     MOVE DS-A-DAMAGED TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (10)
     MOVE DS-A-UOM TO SQLIO-PARM-VAL (11)
     MOVE DS-A-QUALITY TO SQLIO-PARM-VAL (12)
     MOVE DS-A-LOCATION TO SQLIO-PARM-VAL (13)
     MOVE 13 TO SQLIO-PARM-CNT
     MOVE 0 TO SQLIO-COL-CNT
     MOVE Ws-Cursor TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Row
     IF SQLIO-ERROR
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "CLOS" TO SQLIO-FUNC
     MOVE Ws-Cursor TO SQLIO-CURSOR
     CALL "SQLIO" USING SQLIO-CA, Ws-Row
     PERFORM Load-Lines
     MOVE Ws-Ln-Cnt TO DS-LINE-CNT.

*>----------------------------------------------------------------
 Load-Order.
     IF DS-DISASSY-REF = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0001" TO CA-REASON-CODE
         MOVE "A disassembly reference is required." TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
*>   CR2491: the caller may hand in a recovery cap (VALUE) and an
*>   approver name (APPROVE).  Remember them - the stored row must
*>   not silently replace what the operator typed.
     MOVE DS-RECOVERED-GBP TO Ws-In-Recovered
     MOVE DS-APPROVED-BY TO Ws-In-Approver
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "SELECT CONVERT(varchar(12),DisassemblyId), SiteCode,"
       " ParentItemCode, ISNULL(Revision,''), ISNULL(LotCode,''),"
       " ISNULL(SerialNo,''), CONVERT(varchar(26),ConsumedQty),"
       " CONVERT(varchar(20),ParentCarryingValueGBP),"
       " CONVERT(varchar(20),ApprovedCostsGBP),"
       " CONVERT(varchar(20),RecoveredValueGBP),"
       " CONVERT(varchar(20),LossGBP), AllocationBasis, Status,"
       " ISNULL(ApprovedBy,'')"
       " FROM kit.Disassembly"
       " WHERE CompanyCode = ? AND DisassemblyRef = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE DS-DISASSY-REF TO SQLIO-PARM-VAL (2)
     MOVE 2 TO SQLIO-PARM-CNT
     MOVE 14 TO SQLIO-COL-CNT
     PERFORM Set-Wide-Columns
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Row
     EVALUATE TRUE
         WHEN SQLIO-OK
             CONTINUE
         WHEN SQLIO-NOTFOUND
             MOVE 16 TO CA-RETURN-CODE
             MOVE "KIT0001" TO CA-REASON-CODE
             STRING "Disassembly " FUNCTION TRIM(DS-DISASSY-REF)
                    " is not known in this company."
                 DELIMITED BY SIZE INTO CA-MESSAGE
             EXIT PARAGRAPH
         WHEN OTHER
             PERFORM Set-Sql-Failure
             EXIT PARAGRAPH
     END-EVALUATE
     COMPUTE DS-DISASSY-ID = FUNCTION NUMVAL (Ws-Col (1))
     MOVE Ws-Col (2) TO DS-SITE
     MOVE Ws-Col (3) TO DS-PARENT-ITEM
     MOVE Ws-Col (4) TO DS-REVISION
     MOVE Ws-Col (5) TO DS-LOT-CODE
     MOVE Ws-Col (6) TO DS-SERIAL-NO
     COMPUTE DS-CONSUMED-QTY = FUNCTION NUMVAL (Ws-Col (7))
     COMPUTE DS-CARRYING-GBP = FUNCTION NUMVAL (Ws-Col (8))
     COMPUTE DS-APPROVED-GBP = FUNCTION NUMVAL (Ws-Col (9))
     COMPUTE DS-RECOVERED-GBP = FUNCTION NUMVAL (Ws-Col (10))
     COMPUTE DS-LOSS-GBP = FUNCTION NUMVAL (Ws-Col (11))
     MOVE Ws-Col (12) TO DS-ALLOC-BASIS
     MOVE Ws-Col (13) TO DS-STATUS
     MOVE Ws-Col (13) TO Ws-Status
     MOVE Ws-Col (14) TO DS-APPROVED-BY
     IF Ws-In-Recovered NOT = 0
         MOVE Ws-In-Recovered TO DS-RECOVERED-GBP
     END-IF
     IF Ws-In-Approver NOT = SPACES
         MOVE Ws-In-Approver TO DS-APPROVED-BY
     END-IF
     COMPUTE Ws-Available = DS-CARRYING-GBP + DS-APPROVED-GBP
     MOVE Ws-Available TO DS-AVAILABLE-GBP.

 Load-Lines.
     MOVE 0 TO Ws-Ln-Cnt
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "SELECT CONVERT(varchar(12),DisassemblyLineId),"
       " CONVERT(varchar(12),LineNum), ComponentItemCode,"
       " ISNULL(Revision,''), ISNULL(LotCode,''),"
       " ISNULL(SerialNo,''), CONVERT(varchar(26),ISNULL(PlannedQty,0)),"
       " CONVERT(varchar(26),RecoveredQty),"
       " CONVERT(varchar(26),DamagedQty), ISNULL(UomCode,''),"
       " QualityStatus, CONVERT(varchar(20),AllocatedValueGBP),"
       " ISNULL(LocationCode,'')"
       " FROM kit.DisassemblyLine WHERE DisassemblyId = ?"
       " ORDER BY LineNum"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE DS-DISASSY-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (1)
     MOVE 1 TO SQLIO-PARM-CNT
     MOVE 13 TO SQLIO-COL-CNT
     PERFORM Set-Wide-Columns
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
                 PERFORM Store-Line
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

 Store-Line.
     IF Ws-Ln-Cnt >= 60
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Ln-Cnt
     COMPUTE Ln-Id (Ws-Ln-Cnt) = FUNCTION NUMVAL (Ws-Col (1))
     COMPUTE Ln-Num (Ws-Ln-Cnt) = FUNCTION NUMVAL (Ws-Col (2))
     MOVE Ws-Col (3) TO Ln-Item (Ws-Ln-Cnt)
     MOVE Ws-Col (4) TO Ln-Rev (Ws-Ln-Cnt)
     MOVE Ws-Col (5) TO Ln-Lot (Ws-Ln-Cnt)
     MOVE Ws-Col (6) TO Ln-Serial (Ws-Ln-Cnt)
     COMPUTE Ln-Planned (Ws-Ln-Cnt) = FUNCTION NUMVAL (Ws-Col (7))
     COMPUTE Ln-Recovered (Ws-Ln-Cnt) = FUNCTION NUMVAL (Ws-Col (8))
     COMPUTE Ln-Damaged (Ws-Ln-Cnt) = FUNCTION NUMVAL (Ws-Col (9))
     MOVE Ws-Col (10) TO Ln-Uom (Ws-Ln-Cnt)
     MOVE Ws-Col (11) TO Ln-Quality (Ws-Ln-Cnt)
     COMPUTE Ln-Value (Ws-Ln-Cnt) = FUNCTION NUMVAL (Ws-Col (12))
     MOVE Ws-Col (13) TO Ln-Location (Ws-Ln-Cnt)
     MOVE 0 TO Ln-Unit-Val (Ws-Ln-Cnt).

*>----------------------------------------------------------------
*> VALUE - the rule of D-12.  Allocate no more than the carrying
*> value plus approved costs, and call the remainder a loss.  The
*> caller may cap the recovery below the available value
*> (DS-RECOVERED-GBP given in), which is the realistic case: the
*> parts coming back are worth less than the assembly was.
 Allocate-Value.
     PERFORM Load-Order
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Lines
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     IF Ws-Ln-Cnt = 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "No components have been recorded on this disassembly."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Total-Allocation-Basis
     PERFORM Decide-Recovery-Total
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Spread-Recovery
     PERFORM Save-Allocation
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Publish-Lines
     PERFORM Report-Balance.

*>   STANDARD spreads by recovered quantity at the unit value the
*>   caller supplied; EQUAL spreads evenly over the lines that
*>   recovered something.  Damaged quantity carries no value.
 Total-Allocation-Basis.
     MOVE 0 TO Ws-Basis-Total
     MOVE 0 TO Ws-Count
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > Ws-Ln-Cnt
         IF Ln-Recovered (Ws-W) > 0
             ADD 1 TO Ws-Count
             IF DS-ALLOC-BASIS = "EQUAL"
                 ADD 1 TO Ws-Basis-Total
             ELSE
                 ADD Ln-Recovered (Ws-W) TO Ws-Basis-Total
             END-IF
         END-IF
     END-PERFORM.

 Decide-Recovery-Total.
     COMPUTE Ws-Available = DS-CARRYING-GBP + DS-APPROVED-GBP
     IF DS-RECOVERED-GBP > 0
         MOVE DS-RECOVERED-GBP TO Ws-Recovered
     ELSE
         MOVE Ws-Available TO Ws-Recovered
     END-IF
     IF Ws-Recovered > Ws-Available
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0019" TO CA-REASON-CODE
         MOVE Ws-Available TO Ws-Money-Edit
         STRING "Recovery cannot exceed the carrying value plus approved costs of "
                FUNCTION TRIM(Ws-Money-Edit) "."
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF Ws-Count = 0
         MOVE 0 TO Ws-Recovered
     END-IF
     COMPUTE Ws-Loss = Ws-Available - Ws-Recovered.

*>   The rounding residual goes to the largest line so the set
*>   still adds up to the available value exactly - CR2476.
 Spread-Recovery.
     MOVE 0 TO Ws-Running
     MOVE 0 TO Ws-Largest
     MOVE 0 TO Ws-Largest-Val
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > Ws-Ln-Cnt
         MOVE 0 TO Ln-Value (Ws-W)
         IF Ln-Recovered (Ws-W) > 0 AND Ws-Basis-Total > 0
             IF DS-ALLOC-BASIS = "EQUAL"
                 MOVE 1 TO Ws-Line-Basis
             ELSE
                 MOVE Ln-Recovered (Ws-W) TO Ws-Line-Basis
             END-IF
             COMPUTE Ws-Alloc ROUNDED =
                 Ws-Recovered * Ws-Line-Basis / Ws-Basis-Total
             MOVE Ws-Alloc TO Ln-Value (Ws-W)
             ADD Ws-Alloc TO Ws-Running
             IF Ws-Alloc > Ws-Largest-Val
                 MOVE Ws-Alloc TO Ws-Largest-Val
                 MOVE Ws-W TO Ws-Largest
             END-IF
             IF Ln-Recovered (Ws-W) > 0
                 COMPUTE Ln-Unit-Val (Ws-W) ROUNDED =
                     Ws-Alloc / Ln-Recovered (Ws-W)
             END-IF
         END-IF
     END-PERFORM
     IF Ws-Largest > 0 AND Ws-Running NOT = Ws-Recovered
         COMPUTE Ln-Value (Ws-Largest) =
             Ln-Value (Ws-Largest) + (Ws-Recovered - Ws-Running)
         IF Ln-Recovered (Ws-Largest) > 0
             COMPUTE Ln-Unit-Val (Ws-Largest) ROUNDED =
                 Ln-Value (Ws-Largest) / Ln-Recovered (Ws-Largest)
         END-IF
     END-IF.

 Save-Allocation.
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > Ws-Ln-Cnt
         PERFORM Clear-Parms
         MOVE SPACES TO SQLIO-STMT
         STRING
           "UPDATE kit.DisassemblyLine SET AllocatedValueGBP = ?"
           " WHERE DisassemblyLineId = ?"
             DELIMITED BY SIZE INTO SQLIO-STMT
         MOVE Ln-Value (Ws-W) TO SQLIO-NUM-EDIT
         MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (1)
         MOVE Ln-Id (Ws-W) TO SQLIO-INT-EDIT
         MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (2)
         MOVE 2 TO SQLIO-PARM-CNT
         MOVE 0 TO SQLIO-COL-CNT
         MOVE Ws-Cursor TO SQLIO-CURSOR
         MOVE "EXEC" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA, Ws-Row
         MOVE "CLOS" TO SQLIO-FUNC
         MOVE Ws-Cursor TO SQLIO-CURSOR
         CALL "SQLIO" USING SQLIO-CA, Ws-Row
     END-PERFORM
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "UPDATE kit.Disassembly SET RecoveredValueGBP = ?,"
       " LossGBP = ? WHERE DisassemblyId = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE Ws-Recovered TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (1)
     MOVE Ws-Loss TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (2)
     MOVE DS-DISASSY-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (3)
     MOVE 3 TO SQLIO-PARM-CNT
     MOVE 0 TO SQLIO-COL-CNT
     MOVE Ws-Cursor TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Row
     IF SQLIO-ERROR
         PERFORM Set-Sql-Failure
     END-IF
     MOVE "CLOS" TO SQLIO-FUNC
     MOVE Ws-Cursor TO SQLIO-CURSOR
     CALL "SQLIO" USING SQLIO-CA, Ws-Row
     MOVE Ws-Recovered TO DS-RECOVERED-GBP
     MOVE Ws-Loss TO DS-LOSS-GBP.

*>   AC-084: carrying 100.00 plus approved cost 10.00 against
*>   recovery 95.00 and loss 15.00.  95 + 15 = 110 balances.
 Report-Balance.
     COMPUTE Ws-Alloc =
         DS-RECOVERED-GBP + DS-LOSS-GBP
         - DS-CARRYING-GBP - DS-APPROVED-GBP
     IF Ws-Alloc = 0
         MOVE "Y" TO DS-BALANCED
     ELSE
         MOVE "N" TO DS-BALANCED
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0019" TO CA-REASON-CODE
         MOVE "Recovered value plus loss does not equal carrying value plus approved costs."
              TO CA-MESSAGE
     END-IF
     MOVE DS-RECOVERED-GBP TO Ws-Money-Edit
     MOVE SPACES TO DS-IMPACT-TEXT
     STRING "Recovered " FUNCTION TRIM(Ws-Money-Edit)
         DELIMITED BY SIZE INTO DS-IMPACT-TEXT
     MOVE DS-LOSS-GBP TO Ws-Money-Edit
     MOVE DS-IMPACT-TEXT TO Ws-Note
     STRING FUNCTION TRIM(Ws-Note) " plus loss "
            FUNCTION TRIM(Ws-Money-Edit)
         DELIMITED BY SIZE INTO DS-IMPACT-TEXT
     MOVE Ws-Available TO Ws-Money-Edit
     MOVE DS-IMPACT-TEXT TO Ws-Note
     STRING FUNCTION TRIM(Ws-Note) " equals carrying value plus approved costs "
            FUNCTION TRIM(Ws-Money-Edit) "."
         DELIMITED BY SIZE INTO DS-IMPACT-TEXT.

 Publish-Lines.
     MOVE Ws-Ln-Cnt TO DS-LINE-CNT
     MOVE 0 TO DS-WIN-CNT
     MOVE "N" TO DS-MORE-LINES
     IF DS-FROM-LINE <= 0
         MOVE 1 TO DS-FROM-LINE
     END-IF
     PERFORM VARYING Ws-W FROM DS-FROM-LINE BY 1
             UNTIL Ws-W > Ws-Ln-Cnt OR DS-WIN-CNT >= 15
         ADD 1 TO DS-WIN-CNT
         MOVE Ln-Num (Ws-W) TO DS-L-LINE-NUM (DS-WIN-CNT)
         MOVE Ln-Item (Ws-W) TO DS-L-ITEM (DS-WIN-CNT)
         MOVE Ln-Rev (Ws-W) TO DS-L-REV (DS-WIN-CNT)
         MOVE Ln-Lot (Ws-W) TO DS-L-LOT (DS-WIN-CNT)
         MOVE Ln-Serial (Ws-W) TO DS-L-SERIAL (DS-WIN-CNT)
         MOVE Ln-Planned (Ws-W) TO DS-L-PLANNED (DS-WIN-CNT)
         MOVE Ln-Recovered (Ws-W) TO DS-L-RECOVERED (DS-WIN-CNT)
         MOVE Ln-Damaged (Ws-W) TO DS-L-DAMAGED (DS-WIN-CNT)
         MOVE Ln-Uom (Ws-W) TO DS-L-UOM (DS-WIN-CNT)
         MOVE Ln-Quality (Ws-W) TO DS-L-QUALITY (DS-WIN-CNT)
         MOVE Ln-Value (Ws-W) TO DS-L-VALUE (DS-WIN-CNT)
         MOVE Ln-Location (Ws-W) TO DS-L-LOCATION (DS-WIN-CNT)
     END-PERFORM
     IF Ws-W <= Ws-Ln-Cnt
         MOVE "Y" TO DS-MORE-LINES
     END-IF.

*>----------------------------------------------------------------
 Approve-Order.
     PERFORM Load-Order
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     IF Ws-Status NOT = "DRAFT"
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0005" TO CA-REASON-CODE
         STRING "A disassembly in status " FUNCTION TRIM(Ws-Status)
                " cannot be approved."
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF DS-APPROVED-BY = SPACES
         MOVE 6 TO CA-RETURN-CODE
         MOVE "KIT0009" TO CA-REASON-CODE
         MOVE "Disassembly needs a named approver." TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Lines
     PERFORM Report-Balance
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     MOVE "APPROVED" TO Ws-Status
     PERFORM Save-Status
     MOVE "APPROVED" TO DS-STATUS.

 Save-Status.
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "UPDATE kit.Disassembly SET Status = ?,"
       " ApprovedBy = COALESCE(NULLIF(?,''),ApprovedBy)"
       " WHERE DisassemblyId = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE Ws-Status TO SQLIO-PARM-VAL (1)
     MOVE DS-APPROVED-BY TO SQLIO-PARM-VAL (2)
     MOVE DS-DISASSY-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (3)
     MOVE 3 TO SQLIO-PARM-CNT
     MOVE 0 TO SQLIO-COL-CNT
     MOVE Ws-Cursor TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Row
     IF SQLIO-ERROR
         PERFORM Set-Sql-Failure
     END-IF
     MOVE "CLOS" TO SQLIO-FUNC
     MOVE Ws-Cursor TO SQLIO-CURSOR
     CALL "SQLIO" USING SQLIO-CA, Ws-Row.

*>----------------------------------------------------------------
*> POST - the parent leaves stock, the components arrive in
*> inspection condition.  Recovered parts are never received as
*> good stock without inspection: that is the whole point of a
*> controlled disassembly (KIT-021).
 Post-Order.
     PERFORM Load-Order
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     IF Ws-Status NOT = "APPROVED"
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0009" TO CA-REASON-CODE
         MOVE "Only an approved disassembly may be posted."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Lines
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Report-Balance
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Consume-Parent
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO Ws-Posted
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > Ws-Ln-Cnt
         IF Ln-Recovered (Ws-W) > 0 AND NOT CA-FAILED
             PERFORM Receive-Component
         END-IF
     END-PERFORM
     MOVE Ws-Posted TO DS-POSTED-CNT
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     MOVE "POSTED" TO Ws-Status
     PERFORM Save-Status
     MOVE "POSTED" TO DS-STATUS
     PERFORM Publish-Lines.

*>   A derived correlation id for one sub-instruction of this
*>   disassembly.  CR2741: the parent issue and the component
*>   receipts used to share the caller's id, so only the issue was
*>   ever posted - the recovered components came back as duplicates
*>   of it and no stock was received at all.
 Derive-Sub-Correlation.
     MOVE CA-CORRELATION-ID (1:30) TO Ws-Corr-Stem
     MOVE SPACES TO Ws-Sub-Corr
     STRING FUNCTION TRIM(Ws-Corr-Stem)
            FUNCTION TRIM(Ws-Sub-Suffix)
         DELIMITED BY SIZE INTO Ws-Sub-Corr.

 Consume-Parent.
     INITIALIZE Ws-Move-Ca
     MOVE "ISSUE" TO MA-FUNCTION
     MOVE CA-COMPANY TO MA-COMPANY
     MOVE DS-SITE TO MA-SITE
     MOVE CA-USER TO MA-USER
     MOVE CA-BUSINESS-DATE TO MA-BUSINESS-DATE
     MOVE "-P" TO Ws-Sub-Suffix
     PERFORM Derive-Sub-Correlation
     MOVE Ws-Sub-Corr TO MA-CORRELATION-ID
     MOVE DS-PARENT-ITEM TO SM-ITEM-CODE
     MOVE DS-REVISION TO SM-REVISION
     MOVE DS-SITE TO SM-SITE
     MOVE DS-WAREHOUSE TO SM-WAREHOUSE
     MOVE DS-LOCATION TO SM-LOCATION
     MOVE DS-LOT-CODE TO SM-LOT-CODE
     MOVE DS-SERIAL-NO TO SM-SERIAL-NO
     MOVE DS-CONSUMED-QTY TO SM-QUANTITY
     MOVE DS-DISASSY-DATE TO SM-MOVEMENT-DATE
     MOVE DS-POSTING-DATE TO SM-POSTING-DATE
     MOVE "KIT" TO SM-SOURCE-MODULE
     MOVE "DISASSEMBLY" TO SM-SOURCE-DOC-TYPE
     MOVE DS-DISASSY-REF TO SM-SOURCE-DOC-REF
     MOVE 0 TO SM-SOURCE-DOC-LINE
     MOVE "DISASSY" TO SM-REASON-CODE
     MOVE DS-REASON-TEXT (1:200) TO SM-REASON
     CALL "STKMOVE" USING Ws-Move-Ca
     IF MA-RETURN-CODE > 4
         MOVE MA-RETURN-CODE TO CA-RETURN-CODE
         MOVE MA-REASON-CODE TO CA-REASON-CODE
         MOVE MA-MESSAGE TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE SM-MOVEMENT-ID TO DS-MOVEMENT-ID.

 Receive-Component.
     INITIALIZE Ws-Move-Ca
     MOVE "RECEIPT" TO MA-FUNCTION
     MOVE CA-COMPANY TO MA-COMPANY
     MOVE DS-SITE TO MA-SITE
     MOVE CA-USER TO MA-USER
     MOVE CA-BUSINESS-DATE TO MA-BUSINESS-DATE
     MOVE Ln-Num (Ws-W) TO Ws-Sub-Seq
     MOVE SPACES TO Ws-Sub-Suffix
     STRING "-C" Ws-Sub-Seq DELIMITED BY SIZE INTO Ws-Sub-Suffix
     PERFORM Derive-Sub-Correlation
     MOVE Ws-Sub-Corr TO MA-CORRELATION-ID
     MOVE Ln-Item (Ws-W) TO SM-ITEM-CODE
     MOVE Ln-Rev (Ws-W) TO SM-REVISION
     MOVE DS-SITE TO SM-SITE
     MOVE DS-WAREHOUSE TO SM-WAREHOUSE
     MOVE Ln-Location (Ws-W) TO SM-LOCATION
     MOVE Ln-Lot (Ws-W) TO SM-LOT-CODE
     MOVE Ln-Serial (Ws-W) TO SM-SERIAL-NO
     MOVE Ln-Recovered (Ws-W) TO SM-QUANTITY
     MOVE Ln-Uom (Ws-W) TO SM-UOM-CODE
     MOVE DS-DISASSY-DATE TO SM-MOVEMENT-DATE
     MOVE DS-POSTING-DATE TO SM-POSTING-DATE
     MOVE "KIT" TO SM-SOURCE-MODULE
     MOVE "DISASSEMBLY" TO SM-SOURCE-DOC-TYPE
     MOVE DS-DISASSY-REF TO SM-SOURCE-DOC-REF
     MOVE Ln-Num (Ws-W) TO SM-SOURCE-DOC-LINE
     MOVE "RECOVERED" TO SM-REASON-CODE
     MOVE SPACES TO Ws-Note
     STRING "Recovered by disassembly in "
            FUNCTION TRIM(Ln-Quality (Ws-W)) " condition"
         DELIMITED BY SIZE INTO Ws-Note
     MOVE Ws-Note TO SM-REASON
     CALL "STKMOVE" USING Ws-Move-Ca
     IF MA-RETURN-CODE > 4
         MOVE MA-RETURN-CODE TO CA-RETURN-CODE
         MOVE MA-REASON-CODE TO CA-REASON-CODE
         MOVE MA-MESSAGE TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Posted.

 Enquire-Order.
     PERFORM Load-Order
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Load-Lines
     IF CA-FAILED
         EXIT PARAGRAPH
     END-IF
     PERFORM Publish-Lines
     PERFORM Report-Balance.
 END PROGRAM DISASSY.
