>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : KITDB
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : All SQL for the execution kit tables in one place.
*>              The business programs state rules; this one knows
*>              column names.  Every statement is parameterised -
*>              no caller text is ever concatenated into SQL.
*> CALLED BY  : KITCREAT, KITAMEND, KITRESV, KITISSUE, KITRET,
*>              KITSCRAP, KITCOMPL, KITCLOSE, KITCOST, GENEALOG
*> INTERFACE  : CALL "KITDB" USING KIT-DB-CTL, KIT-HEADER-REC,
*>                                 KIT-LINE-REC, KIT-ITEM-REC
*>              COPYBOOKS KITDBW, KITWORK
*> CALLS      : SQLIO
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2014-09-02 PMK  CR2240 ASSEMBLY EXECUTION REWRITE
*> 2016-02-16 PMK  CR2451 CURSORS 5 AND 6 RESERVED SO A CALLER MAY
*>                 HOLD ITS OWN CURSOR ACROSS A KITDB CALL
*> 2026-10-06 STERP KIT-002, KIT-024
*> 2026-10-07 STERP CR2744 ITEMGET PREFERS THE SITE'S COST ROW
*>================================================================
*> REQ: KIT-002, KIT-024
 IDENTIFICATION DIVISION.
 PROGRAM-ID. KITDB.
 AUTHOR. P M KEANE.
 DATE-WRITTEN. 02/09/2014.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 01  Ws-Hdr-Row.
     05  Hr-Id                   PIC X(20).
     05  Hr-Site                 PIC X(10).
     05  Hr-Parent               PIC X(40).
     05  Hr-Rev                  PIC X(4).
     05  Hr-Bom-Id               PIC X(12).
     05  Hr-Bom-Rev              PIC X(4).
     05  Hr-Src-Type             PIC X(12).
     05  Hr-Src-Ref              PIC X(40).
     05  Hr-Job-Ref              PIC X(20).
     05  Hr-Par-Kit              PIC X(30).
     05  Hr-Planned              PIC X(26).
     05  Hr-Completed            PIC X(26).
     05  Hr-Rejected             PIC X(26).
     05  Hr-Gross                PIC X(26).
     05  Hr-Yield                PIC X(20).
     05  Hr-Quantum              PIC X(26).
     05  Hr-Batch                PIC X(26).
     05  Hr-Run-Cnt              PIC X(12).
     05  Hr-Bat-Cnt              PIC X(12).
     05  Hr-Need-Dt              PIC X(8).
     05  Hr-Start-Dt             PIC X(8).
     05  Hr-Plan-Cmp             PIC X(8).
     05  Hr-Act-Cmp              PIC X(8).
     05  Hr-Sel-Dt               PIC X(8).
     05  Hr-Owner                PIC X(20).
     05  Hr-Status               PIC X(14).
     05  Hr-Rel-By               PIC X(20).
     05  Hr-Appr-Id              PIC X(20).
     05  Hr-Wip                  PIC X(20).
     05  Hr-Backflush            PIC X(2).
 01  Ws-Lin-Row.
     05  Lr-Id                   PIC X(20).
     05  Lr-Num                  PIC X(12).
     05  Lr-Path                 PIC X(20).
     05  Lr-Item                 PIC X(40).
     05  Lr-Rev                  PIC X(4).
     05  Lr-Bom-Comp             PIC X(12).
     05  Lr-Basis                PIC X(12).
     05  Lr-Orig-Req             PIC X(26).
     05  Lr-Curr-Req             PIC X(26).
     05  Lr-Uom                  PIC X(10).
     05  Lr-Factor               PIC X(26).
     05  Lr-Scrap-Pct            PIC X(20).
     05  Lr-Reserved             PIC X(26).
     05  Lr-Picked               PIC X(26).
     05  Lr-Gross                PIC X(26).
     05  Lr-Returned             PIC X(26).
     05  Lr-Scrapped             PIC X(26).
     05  Lr-Net                  PIC X(26).
     05  Lr-Remain               PIC X(26).
     05  Lr-Excess               PIC X(26).
     05  Lr-Subst                PIC X(40).
     05  Lr-Sub-Appr             PIC X(20).
     05  Lr-Need-Dt              PIC X(8).
     05  Lr-Loc                  PIC X(20).
     05  Lr-Plan-Cost            PIC X(26).
     05  Lr-Iss-Value            PIC X(20).
     05  Lr-Status               PIC X(12).
 01  Ws-Itm-Row.
     05  Ir-Desc                 PIC X(40).
     05  Ir-Class                PIC X(12).
     05  Ir-Uom                  PIC X(10).
     05  Ir-Stocked              PIC X(2).
     05  Ir-Trace                PIC X(8).
     05  Ir-Std                  PIC X(26).
     05  Ir-Avg                  PIC X(26).
     05  Ir-Status               PIC X(10).
     05  Ir-Meth                 PIC X(4).
 01  Ws-Id-Row.
     05  Idr-Value               PIC X(20).
 01  Ws-Pick-Row.
     05  Pkr-Consumed            PIC X(20).
     05  Pkr-Rows                PIC X(10).
 01  Ws-Work.
     05  Ws-P                    PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Line-Cursor          PIC S9(9) COMP-5 VALUE 5.
     05  Ws-Single-Cursor        PIC S9(9) COMP-5 VALUE 6.
 LINKAGE SECTION.
 COPY KITDBW.
 COPY KITWORK.
 PROCEDURE DIVISION USING KIT-DB-CTL, KIT-HEADER-REC,
                          KIT-LINE-REC, KIT-ITEM-REC.
 Main-Entry.
     MOVE 0 TO KD-RC
     MOVE 0 TO KD-ROWS
     MOVE SPACES TO KD-ERRMSG
     EVALUATE KD-FUNC
         WHEN "HDRGET"   PERFORM Read-Header
         WHEN "HDRINS"   PERFORM Insert-Header
         WHEN "HDRSTAT"  PERFORM Update-Status
         WHEN "HDRQTY"   PERFORM Update-Header-Quantities
         WHEN "HDRFRZ"   PERFORM Freeze-Assumptions
         WHEN "LINOPEN"  PERFORM Open-Lines
         WHEN "LINNEXT"  PERFORM Fetch-Line
         WHEN "LINCLOS"  PERFORM Close-Lines
         WHEN "LINGET"   PERFORM Read-One-Line
         WHEN "LININS"   PERFORM Insert-Line
         WHEN "LINSAVE"  PERFORM Save-Line
         WHEN "ITEMGET"  PERFORM Read-Item
         WHEN "ISSINS"   PERFORM Insert-Issue
         WHEN "GENINS"   PERFORM Insert-Genealogy
         WHEN "PCKINS"   PERFORM Insert-Pick
         WHEN "PCKUSE"   PERFORM Consume-Picks
         WHEN "SUPINS"   PERFORM Insert-Supply-Commit
         WHEN "SUPSUM"   PERFORM Sum-Supply-Commit
         WHEN "SUPFREE"  PERFORM Free-Supply-Commit
         WHEN OTHER      MOVE -1 TO KD-RC
                         MOVE "KITDB function not supported"
                              TO KD-ERRMSG
     END-EVALUATE
     GOBACK.

 Set-Sql-Result.
     MOVE SQLIO-SQLCODE TO KD-RC
     MOVE SQLIO-ROWCOUNT TO KD-ROWS
     IF SQLIO-ERROR
         MOVE SQLIO-ERRMSG (1:200) TO KD-ERRMSG
     END-IF.

 Clear-Parms.
     MOVE 0 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-P FROM 1 BY 1 UNTIL Ws-P > 40
         MOVE SPACES TO SQLIO-PARM-VAL (Ws-P)
         MOVE "N" TO SQLIO-PARM-NULL (Ws-P)
     END-PERFORM.

 Add-Text-Parm.
     ADD 1 TO SQLIO-PARM-CNT.

*>----------------------------------------------------------------
*> HDRGET - one execution kit.  Dates arrive as YYYYMMDD because
*> CONVERT(char(8), d, 112) is cheaper than parsing here.
 Read-Header.
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "SELECT CONVERT(varchar(20),ExecutionKitId), SiteCode,"
       " ParentItemCode, ISNULL(ParentRevision,''),"
       " CONVERT(varchar(12),ISNULL(BomHeaderId,0)),"
       " ISNULL(BomRevision,''), ISNULL(SourceDemandType,''),"
       " ISNULL(SourceDemandRef,''), ISNULL(JobRef,''),"
       " ISNULL(ParentKitRef,''),"
       " CONVERT(varchar(26),PlannedGoodQty),"
       " CONVERT(varchar(26),CompletedGoodQty),"
       " CONVERT(varchar(26),RejectedQty),"
       " CONVERT(varchar(26),ISNULL(GrossStarts,0)),"
       " CONVERT(varchar(20),ISNULL(YieldFactor,1)),"
       " CONVERT(varchar(26),ISNULL(ProductionQuantum,0)),"
       " CONVERT(varchar(26),ISNULL(BatchSize,0)),"
       " CONVERT(varchar(12),RunCount),"
       " CONVERT(varchar(12),ISNULL(BatchCount,0)),"
       " ISNULL(CONVERT(char(8),NeedDate,112),''),"
       " ISNULL(CONVERT(char(8),PlannedStartDate,112),''),"
       " ISNULL(CONVERT(char(8),PlannedCompletionDate,112),''),"
       " ISNULL(CONVERT(char(8),ActualCompletionDate,112),''),"
       " ISNULL(CONVERT(char(8),SelectionDate,112),''),"
       " ISNULL(OwnerUser,''), Status, ISNULL(ReleasedBy,''),"
       " CONVERT(varchar(20),ISNULL(ApprovalId,0)),"
       " CONVERT(varchar(20),WipValueGBP),"
       " CONVERT(varchar(2),IsBackflush)"
       " FROM kit.ExecutionKit"
       " WHERE CompanyCode = ? AND KitRef = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE KD-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE KD-KIT-REF TO SQLIO-PARM-VAL (2)
     MOVE 2 TO SQLIO-PARM-CNT
     MOVE 30 TO SQLIO-COL-CNT
     PERFORM Set-Header-Widths
     MOVE Ws-Single-Cursor TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Hdr-Row
     PERFORM Set-Sql-Result
     IF SQLIO-SQLCODE NOT = 0
         EXIT PARAGRAPH
     END-IF
     PERFORM Move-Header-In.

 Set-Header-Widths.
     MOVE 20 TO SQLIO-COL-WIDTH (1)
     MOVE 10 TO SQLIO-COL-WIDTH (2)
     MOVE 40 TO SQLIO-COL-WIDTH (3)
     MOVE 4 TO SQLIO-COL-WIDTH (4)
     MOVE 12 TO SQLIO-COL-WIDTH (5)
     MOVE 4 TO SQLIO-COL-WIDTH (6)
     MOVE 12 TO SQLIO-COL-WIDTH (7)
     MOVE 40 TO SQLIO-COL-WIDTH (8)
     MOVE 20 TO SQLIO-COL-WIDTH (9)
     MOVE 30 TO SQLIO-COL-WIDTH (10)
     MOVE 26 TO SQLIO-COL-WIDTH (11)
     MOVE 26 TO SQLIO-COL-WIDTH (12)
     MOVE 26 TO SQLIO-COL-WIDTH (13)
     MOVE 26 TO SQLIO-COL-WIDTH (14)
     MOVE 20 TO SQLIO-COL-WIDTH (15)
     MOVE 26 TO SQLIO-COL-WIDTH (16)
     MOVE 26 TO SQLIO-COL-WIDTH (17)
     MOVE 12 TO SQLIO-COL-WIDTH (18)
     MOVE 12 TO SQLIO-COL-WIDTH (19)
     MOVE 8 TO SQLIO-COL-WIDTH (20)
     MOVE 8 TO SQLIO-COL-WIDTH (21)
     MOVE 8 TO SQLIO-COL-WIDTH (22)
     MOVE 8 TO SQLIO-COL-WIDTH (23)
     MOVE 8 TO SQLIO-COL-WIDTH (24)
     MOVE 20 TO SQLIO-COL-WIDTH (25)
     MOVE 14 TO SQLIO-COL-WIDTH (26)
     MOVE 20 TO SQLIO-COL-WIDTH (27)
     MOVE 20 TO SQLIO-COL-WIDTH (28)
     MOVE 20 TO SQLIO-COL-WIDTH (29)
     MOVE 2 TO SQLIO-COL-WIDTH (30).

 Move-Header-In.
     MOVE KD-COMPANY TO KH-COMPANY
     MOVE KD-KIT-REF TO KH-KIT-REF
     COMPUTE KH-EXEC-KIT-ID = FUNCTION NUMVAL (Hr-Id)
     MOVE KH-EXEC-KIT-ID TO KD-KIT-ID
     MOVE Hr-Site TO KH-SITE
     MOVE Hr-Parent TO KH-PARENT-ITEM
     MOVE Hr-Rev TO KH-PARENT-REV
     COMPUTE KH-BOM-ID = FUNCTION NUMVAL (Hr-Bom-Id)
     MOVE Hr-Bom-Rev TO KH-BOM-REV
     MOVE Hr-Src-Type TO KH-SOURCE-TYPE
     MOVE Hr-Src-Ref TO KH-SOURCE-REF
     MOVE Hr-Job-Ref TO KH-JOB-REF
     MOVE Hr-Par-Kit TO KH-PARENT-KIT-REF
     COMPUTE KH-PLANNED-QTY = FUNCTION NUMVAL (Hr-Planned)
     COMPUTE KH-COMPLETED-QTY = FUNCTION NUMVAL (Hr-Completed)
     COMPUTE KH-REJECTED-QTY = FUNCTION NUMVAL (Hr-Rejected)
     COMPUTE KH-GROSS-STARTS = FUNCTION NUMVAL (Hr-Gross)
     COMPUTE KH-YIELD = FUNCTION NUMVAL (Hr-Yield)
     COMPUTE KH-QUANTUM = FUNCTION NUMVAL (Hr-Quantum)
     COMPUTE KH-BATCH-SIZE = FUNCTION NUMVAL (Hr-Batch)
     COMPUTE KH-RUN-COUNT = FUNCTION NUMVAL (Hr-Run-Cnt)
     COMPUTE KH-BATCH-COUNT = FUNCTION NUMVAL (Hr-Bat-Cnt)
     PERFORM Move-Header-Dates
     MOVE Hr-Owner TO KH-OWNER-USER
     MOVE Hr-Status TO KH-STATUS
     MOVE Hr-Rel-By TO KH-RELEASED-BY
     COMPUTE KH-APPROVAL-ID = FUNCTION NUMVAL (Hr-Appr-Id)
     COMPUTE KH-WIP-VALUE = FUNCTION NUMVAL (Hr-Wip)
     IF Hr-Backflush (1:1) = "1"
         MOVE "Y" TO KH-BACKFLUSH
     ELSE
         MOVE "N" TO KH-BACKFLUSH
     END-IF.

 Move-Header-Dates.
     IF Hr-Need-Dt NUMERIC
         MOVE Hr-Need-Dt TO KH-NEED-DATE
     ELSE
         MOVE 0 TO KH-NEED-DATE
     END-IF
     IF Hr-Start-Dt NUMERIC
         MOVE Hr-Start-Dt TO KH-START-DATE
     ELSE
         MOVE 0 TO KH-START-DATE
     END-IF
     IF Hr-Plan-Cmp NUMERIC
         MOVE Hr-Plan-Cmp TO KH-PLAN-COMP-DATE
     ELSE
         MOVE 0 TO KH-PLAN-COMP-DATE
     END-IF
     IF Hr-Act-Cmp NUMERIC
         MOVE Hr-Act-Cmp TO KH-ACT-COMP-DATE
     ELSE
         MOVE 0 TO KH-ACT-COMP-DATE
     END-IF
     IF Hr-Sel-Dt NUMERIC
         MOVE Hr-Sel-Dt TO KH-SELECTION-DATE
     ELSE
         MOVE 0 TO KH-SELECTION-DATE
     END-IF.

*>----------------------------------------------------------------
 Insert-Header.
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "INSERT kit.ExecutionKit (CompanyCode, SiteCode, KitRef,"
       " ParentItemCode, ParentRevision, BomHeaderId, BomRevision,"
       " SourceDemandType, SourceDemandRef, JobRef, ParentKitRef,"
       " PlannedGoodQty, GrossStarts, YieldFactor,"
       " ProductionQuantum, BatchSize, RunCount, BatchCount,"
       " NeedDate, PlannedStartDate, SelectionDate, OwnerUser,"
       " Status, IsBackflush, CorrelationId, CreatedBy)"
       " VALUES (?,?,?,?,?,NULLIF(?,0),?,NULLIF(?,''),NULLIF(?,''),"
       " NULLIF(?,''),NULLIF(?,''),?,?,?,?,?,?,?,"
       " TRY_CONVERT(date,NULLIF(?,''),112),"
       " TRY_CONVERT(date,NULLIF(?,''),112),"
       " TRY_CONVERT(date,NULLIF(?,''),112),?,?,?,NULLIF(?,''),?);"
       " SELECT CONVERT(varchar(20),SCOPE_IDENTITY())"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE KD-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE KH-SITE TO SQLIO-PARM-VAL (2)
     MOVE KD-KIT-REF TO SQLIO-PARM-VAL (3)
     MOVE KH-PARENT-ITEM TO SQLIO-PARM-VAL (4)
     MOVE KH-PARENT-REV TO SQLIO-PARM-VAL (5)
     MOVE KH-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (6)
     MOVE KH-BOM-REV TO SQLIO-PARM-VAL (7)
     MOVE KH-SOURCE-TYPE TO SQLIO-PARM-VAL (8)
     MOVE KH-SOURCE-REF TO SQLIO-PARM-VAL (9)
     MOVE KH-JOB-REF TO SQLIO-PARM-VAL (10)
     MOVE KH-PARENT-KIT-REF TO SQLIO-PARM-VAL (11)
     MOVE KH-PLANNED-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (12)
     MOVE KH-GROSS-STARTS TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (13)
     MOVE KH-YIELD TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (14)
     MOVE KH-QUANTUM TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (15)
     MOVE KH-BATCH-SIZE TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (16)
     MOVE KH-RUN-COUNT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (17)
     MOVE KH-BATCH-COUNT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (18)
     PERFORM Move-Insert-Dates
     MOVE KH-OWNER-USER TO SQLIO-PARM-VAL (22)
     MOVE KH-STATUS TO SQLIO-PARM-VAL (23)
     IF KH-BACKFLUSH-ON
         MOVE "1" TO SQLIO-PARM-VAL (24)
     ELSE
         MOVE "0" TO SQLIO-PARM-VAL (24)
     END-IF
*>   COM-015.  KD-REASON carries the caller's correlation id on an
*>   insert, so the kit a run created can be traced back to the one
*>   instruction that asked for it (CR2743).  The column is 64.
     MOVE KD-REASON (1:64) TO SQLIO-PARM-VAL (25)
     MOVE KD-USER TO SQLIO-PARM-VAL (26)
     MOVE 26 TO SQLIO-PARM-CNT
     PERFORM Run-Identity-Insert.
 Move-Insert-Dates.
     IF KH-NEED-DATE > 0
         MOVE KH-NEED-DATE TO SQLIO-PARM-VAL (19)
     ELSE
         MOVE SPACES TO SQLIO-PARM-VAL (19)
     END-IF
     IF KH-START-DATE > 0
         MOVE KH-START-DATE TO SQLIO-PARM-VAL (20)
     ELSE
         MOVE SPACES TO SQLIO-PARM-VAL (20)
     END-IF
     IF KH-SELECTION-DATE > 0
         MOVE KH-SELECTION-DATE TO SQLIO-PARM-VAL (21)
     ELSE
         MOVE SPACES TO SQLIO-PARM-VAL (21)
     END-IF.

 Run-Identity-Insert.
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 20 TO SQLIO-COL-WIDTH (1)
     MOVE Ws-Single-Cursor TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Id-Row
     PERFORM Set-Sql-Result
     IF SQLIO-SQLCODE = 0
         COMPUTE KD-NEW-ID = FUNCTION NUMVAL (Idr-Value)
     ELSE
         MOVE 0 TO KD-NEW-ID
     END-IF.

 Run-Update.
     MOVE Ws-Single-Cursor TO SQLIO-CURSOR
     MOVE 0 TO SQLIO-COL-CNT
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Id-Row
     PERFORM Set-Sql-Result
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Id-Row.

 Update-Status.
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "UPDATE kit.ExecutionKit SET Status = ?,"
       " ReleasedBy = CASE WHEN ? = 'RELEASED' THEN ?"
       "   ELSE ReleasedBy END,"
       " ReleasedAt = CASE WHEN ? = 'RELEASED' THEN SYSDATETIME()"
       "   ELSE ReleasedAt END,"
       " ReopenedBy = CASE WHEN ? = 'REOPEN' THEN ?"
       "   ELSE ReopenedBy END,"
       " ReopenedAt = CASE WHEN ? = 'REOPEN' THEN SYSDATETIME()"
       "   ELSE ReopenedAt END,"
       " ReopenReason = CASE WHEN ? = 'REOPEN' THEN ?"
       "   ELSE ReopenReason END,"
       " ModifiedBy = ?, ModifiedAt = SYSDATETIME()"
       " WHERE CompanyCode = ? AND KitRef = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE KD-NEW-STATUS TO SQLIO-PARM-VAL (1)
     MOVE KD-NEW-STATUS TO SQLIO-PARM-VAL (2)
     MOVE KD-USER TO SQLIO-PARM-VAL (3)
     MOVE KD-NEW-STATUS TO SQLIO-PARM-VAL (4)
     MOVE KD-FUNC TO SQLIO-PARM-VAL (5)
     MOVE KD-USER TO SQLIO-PARM-VAL (6)
     MOVE KD-FUNC TO SQLIO-PARM-VAL (7)
     MOVE KD-FUNC TO SQLIO-PARM-VAL (8)
     MOVE KD-REASON TO SQLIO-PARM-VAL (9)
     MOVE KD-USER TO SQLIO-PARM-VAL (10)
     MOVE KD-COMPANY TO SQLIO-PARM-VAL (11)
     MOVE KD-KIT-REF TO SQLIO-PARM-VAL (12)
     MOVE 12 TO SQLIO-PARM-CNT
     PERFORM Run-Update.

 Update-Header-Quantities.
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "UPDATE kit.ExecutionKit SET CompletedGoodQty = ?,"
       " RejectedQty = ?, WipValueGBP = ?,"
       " ActualCompletionDate = TRY_CONVERT(date,NULLIF(?,''),112),"
       " ModifiedBy = ?, ModifiedAt = SYSDATETIME()"
       " WHERE CompanyCode = ? AND KitRef = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE KH-COMPLETED-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (1)
     MOVE KH-REJECTED-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (2)
     MOVE KH-WIP-VALUE TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (3)
     IF KH-ACT-COMP-DATE > 0
         MOVE KH-ACT-COMP-DATE TO SQLIO-PARM-VAL (4)
     ELSE
         MOVE SPACES TO SQLIO-PARM-VAL (4)
     END-IF
     MOVE KD-USER TO SQLIO-PARM-VAL (5)
     MOVE KD-COMPANY TO SQLIO-PARM-VAL (6)
     MOVE KD-KIT-REF TO SQLIO-PARM-VAL (7)
     MOVE 7 TO SQLIO-PARM-CNT
     PERFORM Run-Update.

*>   HDRFRZ is the release freeze of KIT-004.  After this the
*>   standard bill may change as often as engineering likes; the
*>   kit keeps the revision, units and assumptions it was released
*>   with until an approved amendment says otherwise.
 Freeze-Assumptions.
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "UPDATE kit.ExecutionKit SET BomHeaderId = NULLIF(?,0),"
       " BomRevision = ?, ParentRevision = ?, GrossStarts = ?,"
       " YieldFactor = ?, ProductionQuantum = ?, BatchSize = ?,"
       " RunCount = ?, BatchCount = ?,"
       " SelectionDate = TRY_CONVERT(date,NULLIF(?,''),112),"
       " SupplyChoiceNote = ?, ModifiedBy = ?,"
       " ModifiedAt = SYSDATETIME()"
       " WHERE CompanyCode = ? AND KitRef = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE KH-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (1)
     MOVE KH-BOM-REV TO SQLIO-PARM-VAL (2)
     MOVE KH-PARENT-REV TO SQLIO-PARM-VAL (3)
     MOVE KH-GROSS-STARTS TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (4)
     MOVE KH-YIELD TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (5)
     MOVE KH-QUANTUM TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (6)
     MOVE KH-BATCH-SIZE TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (7)
     MOVE KH-RUN-COUNT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (8)
     MOVE KH-BATCH-COUNT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (9)
     IF KH-SELECTION-DATE > 0
         MOVE KH-SELECTION-DATE TO SQLIO-PARM-VAL (10)
     ELSE
         MOVE SPACES TO SQLIO-PARM-VAL (10)
     END-IF
     MOVE KD-REASON TO SQLIO-PARM-VAL (11)
     MOVE KD-USER TO SQLIO-PARM-VAL (12)
     MOVE KD-COMPANY TO SQLIO-PARM-VAL (13)
     MOVE KD-KIT-REF TO SQLIO-PARM-VAL (14)
     MOVE 14 TO SQLIO-PARM-CNT
     PERFORM Run-Update.

*>----------------------------------------------------------------
 Build-Line-Select.
     MOVE SPACES TO SQLIO-STMT
     STRING
       "SELECT CONVERT(varchar(20),KitLineId),"
       " CONVERT(varchar(12),LineNum), PathRef, ComponentItemCode,"
       " ISNULL(ComponentRevision,''),"
       " CONVERT(varchar(12),ISNULL(BomComponentId,0)), UsageBasis,"
       " CONVERT(varchar(26),OriginalRequirementQty),"
       " CONVERT(varchar(26),CurrentRequirementQty), UomCode,"
       " CONVERT(varchar(26),ConversionFactor),"
       " CONVERT(varchar(20),ScrapPct),"
       " CONVERT(varchar(26),ReservedQty),"
       " CONVERT(varchar(26),PickedQty),"
       " CONVERT(varchar(26),GrossIssuedQty),"
       " CONVERT(varchar(26),ReturnedQty),"
       " CONVERT(varchar(26),ScrappedQty),"
       " CONVERT(varchar(26),NetIssuedQty),"
       " CONVERT(varchar(26),RemainingQty),"
       " CONVERT(varchar(26),ExcessQty),"
       " ISNULL(SubstituteItemCode,''),"
       " CONVERT(varchar(20),ISNULL(SubstituteApprovalId,0)),"
       " ISNULL(CONVERT(char(8),NeedDate,112),''),"
       " ISNULL(IssueLocationCode,''),"
       " CONVERT(varchar(26),ISNULL(PlannedUnitCostGBP,0)),"
       " CONVERT(varchar(20),IssuedValueGBP), Status"
       " FROM kit.ExecutionKitLine WHERE ExecutionKitId = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 27 TO SQLIO-COL-CNT
     PERFORM Set-Line-Widths.

 Set-Line-Widths.
     MOVE 20 TO SQLIO-COL-WIDTH (1)
     MOVE 12 TO SQLIO-COL-WIDTH (2)
     MOVE 20 TO SQLIO-COL-WIDTH (3)
     MOVE 40 TO SQLIO-COL-WIDTH (4)
     MOVE 4 TO SQLIO-COL-WIDTH (5)
     MOVE 12 TO SQLIO-COL-WIDTH (6)
     MOVE 12 TO SQLIO-COL-WIDTH (7)
     MOVE 26 TO SQLIO-COL-WIDTH (8)
     MOVE 26 TO SQLIO-COL-WIDTH (9)
     MOVE 10 TO SQLIO-COL-WIDTH (10)
     MOVE 26 TO SQLIO-COL-WIDTH (11)
     MOVE 20 TO SQLIO-COL-WIDTH (12)
     MOVE 26 TO SQLIO-COL-WIDTH (13)
     MOVE 26 TO SQLIO-COL-WIDTH (14)
     MOVE 26 TO SQLIO-COL-WIDTH (15)
     MOVE 26 TO SQLIO-COL-WIDTH (16)
     MOVE 26 TO SQLIO-COL-WIDTH (17)
     MOVE 26 TO SQLIO-COL-WIDTH (18)
     MOVE 26 TO SQLIO-COL-WIDTH (19)
     MOVE 26 TO SQLIO-COL-WIDTH (20)
     MOVE 40 TO SQLIO-COL-WIDTH (21)
     MOVE 20 TO SQLIO-COL-WIDTH (22)
     MOVE 8 TO SQLIO-COL-WIDTH (23)
     MOVE 20 TO SQLIO-COL-WIDTH (24)
     MOVE 26 TO SQLIO-COL-WIDTH (25)
     MOVE 20 TO SQLIO-COL-WIDTH (26)
     MOVE 12 TO SQLIO-COL-WIDTH (27).

 Open-Lines.
     PERFORM Clear-Parms
     PERFORM Build-Line-Select
     IF KD-ONLY-OPEN = "Y"
         STRING FUNCTION TRIM (SQLIO-STMT)
                " AND Status NOT IN ('CLOSED','CANCELLED')"
                " ORDER BY LineNum"
             DELIMITED BY SIZE INTO SQLIO-STMT
     ELSE
         STRING FUNCTION TRIM (SQLIO-STMT) " ORDER BY LineNum"
             DELIMITED BY SIZE INTO SQLIO-STMT
     END-IF
     MOVE KD-KIT-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (1)
     MOVE 1 TO SQLIO-PARM-CNT
     MOVE Ws-Line-Cursor TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Lin-Row
     PERFORM Set-Sql-Result.

 Fetch-Line.
     MOVE Ws-Line-Cursor TO SQLIO-CURSOR
     MOVE 27 TO SQLIO-COL-CNT
     PERFORM Set-Line-Widths
     MOVE "FTCH" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Lin-Row
     PERFORM Set-Sql-Result
     IF SQLIO-SQLCODE = 0
         PERFORM Move-Line-In
     END-IF.

 Close-Lines.
     MOVE Ws-Line-Cursor TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Lin-Row
     PERFORM Set-Sql-Result.

 Read-One-Line.
     PERFORM Clear-Parms
     PERFORM Build-Line-Select
     STRING FUNCTION TRIM (SQLIO-STMT) " AND LineNum = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE KD-KIT-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (1)
     MOVE KD-LINE-NUM TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (2)
     MOVE 2 TO SQLIO-PARM-CNT
     MOVE 27 TO SQLIO-COL-CNT
     PERFORM Set-Line-Widths
     MOVE Ws-Single-Cursor TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Lin-Row
     PERFORM Set-Sql-Result
     IF SQLIO-SQLCODE = 0
         PERFORM Move-Line-In
     END-IF.

 Move-Line-In.
     COMPUTE KL-LINE-ID = FUNCTION NUMVAL (Lr-Id)
     COMPUTE KL-LINE-NUM = FUNCTION NUMVAL (Lr-Num)
     MOVE Lr-Path TO KL-PATH-REF
     MOVE Lr-Item TO KL-ITEM-CODE
     MOVE Lr-Rev TO KL-REVISION
     COMPUTE KL-BOM-COMP-ID = FUNCTION NUMVAL (Lr-Bom-Comp)
     MOVE Lr-Basis TO KL-USAGE-BASIS
     COMPUTE KL-ORIGINAL-REQ = FUNCTION NUMVAL (Lr-Orig-Req)
     COMPUTE KL-CURRENT-REQ = FUNCTION NUMVAL (Lr-Curr-Req)
     MOVE Lr-Uom TO KL-UOM-CODE
     COMPUTE KL-FACTOR = FUNCTION NUMVAL (Lr-Factor)
     COMPUTE KL-SCRAP-PCT = FUNCTION NUMVAL (Lr-Scrap-Pct)
     COMPUTE KL-RESERVED-QTY = FUNCTION NUMVAL (Lr-Reserved)
     COMPUTE KL-PICKED-QTY = FUNCTION NUMVAL (Lr-Picked)
     COMPUTE KL-GROSS-ISSUED = FUNCTION NUMVAL (Lr-Gross)
     COMPUTE KL-RETURNED-QTY = FUNCTION NUMVAL (Lr-Returned)
     COMPUTE KL-SCRAPPED-QTY = FUNCTION NUMVAL (Lr-Scrapped)
     COMPUTE KL-NET-ISSUED = FUNCTION NUMVAL (Lr-Net)
     COMPUTE KL-REMAINING-QTY = FUNCTION NUMVAL (Lr-Remain)
     COMPUTE KL-EXCESS-QTY = FUNCTION NUMVAL (Lr-Excess)
     MOVE Lr-Subst TO KL-SUBSTITUTE
     COMPUTE KL-SUB-APPROVAL = FUNCTION NUMVAL (Lr-Sub-Appr)
     IF Lr-Need-Dt NUMERIC
         MOVE Lr-Need-Dt TO KL-NEED-DATE
     ELSE
         MOVE 0 TO KL-NEED-DATE
     END-IF
     MOVE Lr-Loc TO KL-LOCATION
     COMPUTE KL-PLAN-UNIT-COST = FUNCTION NUMVAL (Lr-Plan-Cost)
     COMPUTE KL-ISSUED-VALUE = FUNCTION NUMVAL (Lr-Iss-Value)
     MOVE Lr-Status TO KL-STATUS.

 Insert-Line.
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "INSERT kit.ExecutionKitLine (ExecutionKitId, LineNum,"
       " PathRef, ComponentItemCode, ComponentRevision,"
       " BomComponentId, UsageBasis, OriginalRequirementQty,"
       " CurrentRequirementQty, UomCode, ConversionFactor,"
       " ScrapPct, NeedDate, IssueLocationCode,"
       " PlannedUnitCostGBP, RemainingQty, Status)"
       " VALUES (?,?,?,?,NULLIF(?,''),NULLIF(?,0),?,?,?,?,?,?,"
       " TRY_CONVERT(date,NULLIF(?,''),112),NULLIF(?,''),?,?,?);"
       " SELECT CONVERT(varchar(20),SCOPE_IDENTITY())"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE KD-KIT-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (1)
     MOVE KL-LINE-NUM TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (2)
     MOVE KL-PATH-REF TO SQLIO-PARM-VAL (3)
     MOVE KL-ITEM-CODE TO SQLIO-PARM-VAL (4)
     MOVE KL-REVISION TO SQLIO-PARM-VAL (5)
     MOVE KL-BOM-COMP-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (6)
     MOVE KL-USAGE-BASIS TO SQLIO-PARM-VAL (7)
     MOVE KL-ORIGINAL-REQ TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (8)
     MOVE KL-CURRENT-REQ TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (9)
     MOVE KL-UOM-CODE TO SQLIO-PARM-VAL (10)
     MOVE KL-FACTOR TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (11)
     MOVE KL-SCRAP-PCT TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (12)
     IF KL-NEED-DATE > 0
         MOVE KL-NEED-DATE TO SQLIO-PARM-VAL (13)
     ELSE
         MOVE SPACES TO SQLIO-PARM-VAL (13)
     END-IF
     MOVE KL-LOCATION TO SQLIO-PARM-VAL (14)
     MOVE KL-PLAN-UNIT-COST TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (15)
     MOVE KL-CURRENT-REQ TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (16)
     MOVE KL-STATUS TO SQLIO-PARM-VAL (17)
     MOVE 17 TO SQLIO-PARM-CNT
     PERFORM Run-Identity-Insert
     MOVE KD-NEW-ID TO KL-LINE-ID.

 Save-Line.
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "UPDATE kit.ExecutionKitLine SET CurrentRequirementQty = ?,"
       " ReservedQty = ?, PickedQty = ?, GrossIssuedQty = ?,"
       " ReturnedQty = ?, ScrappedQty = ?, NetIssuedQty = ?,"
       " RemainingQty = ?, ExcessQty = ?,"
       " SubstituteItemCode = NULLIF(?,''),"
       " SubstituteApprovalId = NULLIF(?,0),"
       " DeviationReason = NULLIF(?,''),"
       " DeviationApprovedBy = NULLIF(?,''),"
       " IssuedValueGBP = ?, Status = ?"
       " WHERE KitLineId = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE KL-CURRENT-REQ TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (1)
     MOVE KL-RESERVED-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (2)
     MOVE KL-PICKED-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (3)
     MOVE KL-GROSS-ISSUED TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (4)
     MOVE KL-RETURNED-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (5)
     MOVE KL-SCRAPPED-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (6)
     MOVE KL-NET-ISSUED TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (7)
     MOVE KL-REMAINING-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (8)
     MOVE KL-EXCESS-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (9)
     MOVE KL-SUBSTITUTE TO SQLIO-PARM-VAL (10)
     MOVE KL-SUB-APPROVAL TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (11)
     MOVE KL-DEVIATION (1:200) TO SQLIO-PARM-VAL (12)
     MOVE KL-DEV-APPROVER TO SQLIO-PARM-VAL (13)
     MOVE KL-ISSUED-VALUE TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (14)
     MOVE KL-STATUS TO SQLIO-PARM-VAL (15)
     MOVE KL-LINE-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (16)
     MOVE 16 TO SQLIO-PARM-CNT
     PERFORM Run-Update.

*>----------------------------------------------------------------
*> ITEMGET - the traits the kit engine must respect: may this item
*> hold stock at all, is it lot or serial traced (which forbids
*> backflush under D-20), and what does a unit cost today.
*>   CR2744.  The cost row of KD-SITE is preferred when the caller
*>   names one, then the company-wide '*' row; the item's current
*>   revision wins within a site.  With KD-SITE blank only the '*'
*>   row can match, exactly as before.
 Read-Item.
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "SELECT i.Description, i.ItemClass, i.StockUomCode,"
       " CONVERT(varchar(2),i.IsStocked), i.TraceabilityType,"
       " CONVERT(varchar(26),ISNULL(c.StandardUnitCost,0)),"
       " CONVERT(varchar(26),ISNULL(c.AverageUnitCost,0)),"
       " i.LifecycleStatus, ISNULL(c.CostMethod,'')"
       " FROM inv.Item i OUTER APPLY (SELECT TOP (1)"
       " x.StandardUnitCost, x.AverageUnitCost, x.CostMethod"
       " FROM cst.ItemCost x WHERE x.CompanyCode = i.CompanyCode"
       "  AND x.ItemCode = i.ItemCode"
       "  AND (x.SiteCode = '*' OR x.SiteCode = ?)"
       " ORDER BY CASE WHEN x.SiteCode = '*' THEN 1 ELSE 0 END,"
       " CASE WHEN x.Revision = i.CurrentRevision THEN 0 ELSE 1 END)"
       " c WHERE i.CompanyCode = ? AND i.ItemCode = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE KD-SITE TO SQLIO-PARM-VAL (1)
     MOVE KD-COMPANY TO SQLIO-PARM-VAL (2)
     MOVE KD-ITEM-CODE TO SQLIO-PARM-VAL (3)
     MOVE 3 TO SQLIO-PARM-CNT
     MOVE 9 TO SQLIO-COL-CNT
     MOVE 40 TO SQLIO-COL-WIDTH (1)
     MOVE 12 TO SQLIO-COL-WIDTH (2)
     MOVE 10 TO SQLIO-COL-WIDTH (3)
     MOVE 2 TO SQLIO-COL-WIDTH (4)
     MOVE 8 TO SQLIO-COL-WIDTH (5)
     MOVE 26 TO SQLIO-COL-WIDTH (6)
     MOVE 26 TO SQLIO-COL-WIDTH (7)
     MOVE 10 TO SQLIO-COL-WIDTH (8)
     MOVE 4 TO SQLIO-COL-WIDTH (9)
     MOVE Ws-Single-Cursor TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Itm-Row
     PERFORM Set-Sql-Result
     IF SQLIO-SQLCODE NOT = 0
         EXIT PARAGRAPH
     END-IF
     MOVE KD-ITEM-CODE TO KI-ITEM-CODE
     MOVE Ir-Desc TO KI-DESCRIPTION
     MOVE Ir-Class TO KI-ITEM-CLASS
     MOVE Ir-Uom TO KI-STOCK-UOM
     MOVE Ir-Status TO KI-STATUS
     IF Ir-Stocked (1:1) = "1"
         MOVE "Y" TO KI-IS-STOCKED
     ELSE
         MOVE "N" TO KI-IS-STOCKED
     END-IF
     MOVE "N" TO KI-LOT-CONTROL
     MOVE "N" TO KI-SERIAL-CONTROL
     MOVE "N" TO KI-TRACED
     EVALUATE Ir-Trace (1:6)
         WHEN "LOT   "
             MOVE "Y" TO KI-LOT-CONTROL
             MOVE "Y" TO KI-TRACED
         WHEN "SERIAL"
             MOVE "Y" TO KI-SERIAL-CONTROL
             MOVE "Y" TO KI-TRACED
     END-EVALUATE
     COMPUTE KI-STD-COST = FUNCTION NUMVAL (Ir-Std)
     COMPUTE KI-AVG-COST = FUNCTION NUMVAL (Ir-Avg)
     MOVE Ir-Meth TO KI-COST-METHOD.

*>----------------------------------------------------------------
 Insert-Issue.
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "INSERT kit.KitIssue (CompanyCode, KitRef, KitLineId,"
       " LineNum, ComponentItemCode, Revision, IssueType,"
       " Quantity, UomCode, StockQty, WarehouseCode, LocationCode,"
       " LotCode, SerialNo, ActualDate, PostingDate, OperatorUser,"
       " UnitCostGBP, ValueGBP, ReasonCode, Reason, ApprovalId,"
       " OriginalKitIssueId, MovementId, CorrelationId, CreatedBy)"
       " VALUES (?,?,NULLIF(?,0),NULLIF(?,0),?,NULLIF(?,''),?,?,"
       " NULLIF(?,''),?,NULLIF(?,''),NULLIF(?,''),NULLIF(?,''),"
       " NULLIF(?,''),CONVERT(date,?,112),CONVERT(date,?,112),?,"
       " ?,?,NULLIF(?,''),NULLIF(?,''),NULLIF(?,0),NULLIF(?,0),"
       " NULLIF(?,0),NULLIF(?,''),?);"
       " SELECT CONVERT(varchar(20),SCOPE_IDENTITY())"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE KD-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE KD-KIT-REF TO SQLIO-PARM-VAL (2)
     MOVE KL-LINE-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (3)
     MOVE KL-LINE-NUM TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (4)
     MOVE KD-I-ITEM TO SQLIO-PARM-VAL (5)
     MOVE KD-I-REV TO SQLIO-PARM-VAL (6)
     MOVE KD-I-TYPE TO SQLIO-PARM-VAL (7)
     MOVE KD-I-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (8)
     MOVE KD-I-UOM TO SQLIO-PARM-VAL (9)
     MOVE KD-I-STOCK-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (10)
     MOVE KD-I-WAREHOUSE TO SQLIO-PARM-VAL (11)
     MOVE KD-I-LOCATION TO SQLIO-PARM-VAL (12)
     MOVE KD-I-LOT TO SQLIO-PARM-VAL (13)
     MOVE KD-I-SERIAL TO SQLIO-PARM-VAL (14)
     MOVE KD-I-ACT-DATE TO SQLIO-PARM-VAL (15)
     MOVE KD-I-POST-DATE TO SQLIO-PARM-VAL (16)
     MOVE KD-USER TO SQLIO-PARM-VAL (17)
     MOVE KD-I-UNIT-COST TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (18)
     MOVE KD-I-VALUE TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (19)
     MOVE KD-I-REASON-CODE TO SQLIO-PARM-VAL (20)
     MOVE KD-I-REASON TO SQLIO-PARM-VAL (21)
     MOVE KD-I-APPROVAL TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (22)
     MOVE KD-I-ORIG-ISSUE TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (23)
     MOVE KD-I-MOVEMENT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (24)
     MOVE KD-REASON (1:64) TO SQLIO-PARM-VAL (25)
     MOVE KD-USER TO SQLIO-PARM-VAL (26)
     MOVE 26 TO SQLIO-PARM-CNT
     PERFORM Run-Identity-Insert.

 Insert-Genealogy.
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "INSERT kit.KitGenealogy (CompanyCode, KitRef, CompletionId,"
       " ParentItemCode, ParentLotCode, ParentSerialNo,"
       " ComponentItemCode, ComponentLotCode, ComponentSerialNo,"
       " ConsumedQty, WasSubstitute, PlannedItemCode, KitIssueId)"
       " VALUES (?,?,NULLIF(?,0),?,NULLIF(?,''),NULLIF(?,''),?,"
       " NULLIF(?,''),NULLIF(?,''),?,?,NULLIF(?,''),NULLIF(?,0));"
       " SELECT CONVERT(varchar(20),SCOPE_IDENTITY())"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE KD-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE KD-KIT-REF TO SQLIO-PARM-VAL (2)
     MOVE KD-G-COMPLETION TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (3)
     MOVE KD-G-PARENT TO SQLIO-PARM-VAL (4)
     MOVE KD-G-PAR-LOT TO SQLIO-PARM-VAL (5)
     MOVE KD-G-PAR-SERIAL TO SQLIO-PARM-VAL (6)
     MOVE KD-G-ITEM TO SQLIO-PARM-VAL (7)
     MOVE KD-G-LOT TO SQLIO-PARM-VAL (8)
     MOVE KD-G-SERIAL TO SQLIO-PARM-VAL (9)
     MOVE KD-G-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (10)
     IF KD-G-SUBSTITUTE = "Y"
         MOVE "1" TO SQLIO-PARM-VAL (11)
     ELSE
         MOVE "0" TO SQLIO-PARM-VAL (11)
     END-IF
     MOVE KD-G-PLANNED TO SQLIO-PARM-VAL (12)
     MOVE KD-G-ISSUE-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (13)
     MOVE 13 TO SQLIO-PARM-CNT
     PERFORM Run-Identity-Insert.

*>----------------------------------------------------------------
*> PCKINS - a pick is not an issue and is not a reservation, so it
*> gets its own row.  The lot or serial recorded here is the one
*> the storeman actually took, which is what genealogy wants back
*> (KIT-009, KIT-019).  An UNPICK is written as a negative row so
*> that the trail keeps the mistake instead of hiding it.
 Insert-Pick.
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "INSERT kit.KitPick (CompanyCode, KitRef, KitLineId,"
       " LineNum, ComponentItemCode, Revision, PickType,"
       " PickedQty, UomCode, WarehouseCode, LocationCode,"
       " LotCode, SerialNo, PickDate, ReasonCode, Reason,"
       " PickedBy, CorrelationId)"
       " VALUES (?,?,NULLIF(?,0),NULLIF(?,0),?,NULLIF(?,''),?,?,"
       " NULLIF(?,''),NULLIF(?,''),NULLIF(?,''),NULLIF(?,''),"
       " NULLIF(?,''),CONVERT(date,?,112),NULLIF(?,''),"
       " NULLIF(?,''),?,NULLIF(?,''));"
       " SELECT CONVERT(varchar(20),SCOPE_IDENTITY())"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE KD-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE KD-KIT-REF TO SQLIO-PARM-VAL (2)
     MOVE KL-LINE-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (3)
     MOVE KL-LINE-NUM TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (4)
     MOVE KL-ITEM-CODE TO SQLIO-PARM-VAL (5)
     MOVE KL-REVISION TO SQLIO-PARM-VAL (6)
     MOVE KD-P-TYPE TO SQLIO-PARM-VAL (7)
     MOVE KD-P-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (8)
     MOVE KD-P-UOM TO SQLIO-PARM-VAL (9)
     MOVE KD-P-WAREHOUSE TO SQLIO-PARM-VAL (10)
     MOVE KD-P-LOCATION TO SQLIO-PARM-VAL (11)
     MOVE KD-P-LOT TO SQLIO-PARM-VAL (12)
     MOVE KD-P-SERIAL TO SQLIO-PARM-VAL (13)
     MOVE KD-P-PICK-DATE TO SQLIO-PARM-VAL (14)
     MOVE KD-P-REASON-CODE TO SQLIO-PARM-VAL (15)
     MOVE KD-P-REASON TO SQLIO-PARM-VAL (16)
     MOVE KD-USER TO SQLIO-PARM-VAL (17)
     MOVE KD-REASON (1:64) TO SQLIO-PARM-VAL (18)
     MOVE 18 TO SQLIO-PARM-CNT
     PERFORM Run-Identity-Insert.

*>----------------------------------------------------------------
*> PCKUSE - KIT-009/KIT-010.  A pick is material already off the
*> shelf; issuing is what finally spends it.  The open picks of the
*> line are consumed oldest first (the box that left the shelf
*> first is the box that goes into the kit first), so that whatever
*> is left in kit.KitPick after a cancellation is exactly the
*> material still sitting on the trolley and owed back to stock.
*> An issue may legitimately exceed the picks - the picker took
*> more, or nothing was picked at all - so the shortfall is simply
*> not covered by picks and is not an error.
 Consume-Picks.
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "DECLARE @q decimal(19,6) = CONVERT(decimal(19,6),?);"
       " DECLARE @p decimal(19,6); DECLARE @u decimal(19,6);"
       " DECLARE @c decimal(19,6); DECLARE @t decimal(19,6);"
       " DECLARE @r int;"
       " SELECT @p = ISNULL(SUM(CASE WHEN PickType = 'PICK'"
       " THEN PickedQty - ConsumedQty ELSE 0 END),0),"
       " @u = ISNULL(SUM(CASE WHEN PickType = 'UNPICK'"
       " THEN ConsumedQty - PickedQty ELSE 0 END),0)"
       " FROM kit.KitPick WHERE CompanyCode = ? AND KitRef = ?"
       " AND LineNum = ?;"
       " SET @c = CASE WHEN @p - @u <= 0 THEN 0"
       " WHEN @q < @p - @u THEN @q ELSE @p - @u END;"
       " SET @t = @c + @u;"
       " WITH p AS (SELECT KitPickId, PickedQty, ConsumedQty,"
       " SUM(PickedQty - ConsumedQty) OVER (ORDER BY KitPickId"
       " ROWS UNBOUNDED PRECEDING) AS RunTot FROM kit.KitPick"
       " WHERE CompanyCode = ? AND KitRef = ? AND LineNum = ?"
       " AND PickType = 'PICK' AND PickedQty > ConsumedQty)"
       " UPDATE p SET ConsumedQty = ConsumedQty + CASE"
       " WHEN RunTot <= @t THEN PickedQty - ConsumedQty"
       " ELSE @t - (RunTot - (PickedQty - ConsumedQty)) END"
       " WHERE RunTot - (PickedQty - ConsumedQty) < @t;"
       " SET @r = @@ROWCOUNT;"
       " UPDATE kit.KitPick SET ConsumedQty = PickedQty"
       " WHERE CompanyCode = ? AND KitRef = ? AND LineNum = ?"
       " AND PickType = 'UNPICK' AND ConsumedQty <> PickedQty"
       " AND @t >= @u;"
       " SELECT CONVERT(varchar(20), @c),"
       " CONVERT(varchar(10), @r)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE KD-P-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (1)
     MOVE KD-COMPANY TO SQLIO-PARM-VAL (2)
     MOVE KD-KIT-REF TO SQLIO-PARM-VAL (3)
     MOVE KD-LINE-NUM TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (4)
     MOVE KD-COMPANY TO SQLIO-PARM-VAL (5)
     MOVE KD-KIT-REF TO SQLIO-PARM-VAL (6)
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (7)
     MOVE KD-COMPANY TO SQLIO-PARM-VAL (8)
     MOVE KD-KIT-REF TO SQLIO-PARM-VAL (9)
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (10)
     MOVE 10 TO SQLIO-PARM-CNT
     MOVE 2 TO SQLIO-COL-CNT
     MOVE 20 TO SQLIO-COL-WIDTH (1)
     MOVE 10 TO SQLIO-COL-WIDTH (2)
     MOVE Ws-Single-Cursor TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     MOVE SPACES TO Ws-Pick-Row
     CALL "SQLIO" USING SQLIO-CA, Ws-Pick-Row
     PERFORM Set-Sql-Result
     MOVE 0 TO KD-P-CONSUMED
     MOVE 0 TO KD-ROWS
     IF SQLIO-SQLCODE = 0
         IF Pkr-Consumed NOT = SPACES
             COMPUTE KD-P-CONSUMED = FUNCTION NUMVAL (Pkr-Consumed)
         END-IF
         IF Pkr-Rows NOT = SPACES
             COMPUTE KD-ROWS = FUNCTION NUMVAL (Pkr-Rows)
         END-IF
     END-IF.

*>----------------------------------------------------------------
*> SUPINS - the other half of KIT-008.  Nothing in inv is touched:
*> the kit is simply pinned to a named inbound order line, and the
*> outstanding requirement falls so that free stock is not reserved
*> for a need that the purchase order already covers.
 Insert-Supply-Commit.
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "INSERT kit.KitSupplyCommit (CompanyCode, KitRef,"
       " KitLineId, LineNum, ComponentItemCode, Revision,"
       " SupplyType, SupplyRef, SupplyLine, CommittedQty, UomCode,"
       " DueDate, RequiredDate, Status, IsLate, ReasonCode,"
       " Reason, CommittedBy, CorrelationId)"
       " VALUES (?,?,NULLIF(?,0),NULLIF(?,0),?,NULLIF(?,''),?,?,"
       " NULLIF(?,0),?,NULLIF(?,''),"
       " CASE WHEN ? = '0' THEN NULL ELSE CONVERT(date,?,112) END,"
       " CASE WHEN ? = '0' THEN NULL ELSE CONVERT(date,?,112) END,"
       " 'ACTIVE',?,NULLIF(?,''),NULLIF(?,''),?,NULLIF(?,''));"
       " SELECT CONVERT(varchar(20),SCOPE_IDENTITY())"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE KD-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE KD-KIT-REF TO SQLIO-PARM-VAL (2)
     MOVE KL-LINE-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (3)
     MOVE KL-LINE-NUM TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (4)
     MOVE KL-ITEM-CODE TO SQLIO-PARM-VAL (5)
     MOVE KL-REVISION TO SQLIO-PARM-VAL (6)
     MOVE KD-S-TYPE TO SQLIO-PARM-VAL (7)
     MOVE KD-S-REF TO SQLIO-PARM-VAL (8)
     MOVE KD-S-LINE TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (9)
     MOVE KD-S-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM (SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (10)
     MOVE KD-S-UOM TO SQLIO-PARM-VAL (11)
     MOVE KD-S-DUE-DATE TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (12)
     MOVE KD-S-DUE-DATE TO SQLIO-PARM-VAL (13)
     MOVE KD-S-REQ-DATE TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (14)
     MOVE KD-S-REQ-DATE TO SQLIO-PARM-VAL (15)
     IF KD-S-LATE = "Y"
         MOVE "1" TO SQLIO-PARM-VAL (16)
     ELSE
         MOVE "0" TO SQLIO-PARM-VAL (16)
     END-IF
     MOVE KD-S-REASON-CODE TO SQLIO-PARM-VAL (17)
     MOVE KD-S-REASON TO SQLIO-PARM-VAL (18)
     MOVE KD-USER TO SQLIO-PARM-VAL (19)
     MOVE KD-REASON (1:64) TO SQLIO-PARM-VAL (20)
     MOVE 20 TO SQLIO-PARM-CNT
     PERFORM Run-Identity-Insert.

 Sum-Supply-Commit.
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "SELECT CONVERT(varchar(20), ISNULL(SUM(CommittedQty),0))"
       " FROM kit.KitSupplyCommit WHERE CompanyCode = ?"
       " AND KitRef = ? AND Status = 'ACTIVE'"
       " AND (? = 0 OR LineNum = ?)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE KD-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE KD-KIT-REF TO SQLIO-PARM-VAL (2)
     MOVE KD-LINE-NUM TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (3)
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (4)
     MOVE 4 TO SQLIO-PARM-CNT
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 20 TO SQLIO-COL-WIDTH (1)
     MOVE Ws-Single-Cursor TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     MOVE SPACES TO Ws-Id-Row
     CALL "SQLIO" USING SQLIO-CA, Ws-Id-Row
     PERFORM Set-Sql-Result
     IF SQLIO-SQLCODE = 0 AND Idr-Value NOT = SPACES
         COMPUTE KD-S-COMMITTED = FUNCTION NUMVAL (Idr-Value)
     ELSE
         MOVE 0 TO KD-S-COMMITTED
     END-IF.

 Free-Supply-Commit.
     PERFORM Clear-Parms
     MOVE SPACES TO SQLIO-STMT
     STRING
       "UPDATE kit.KitSupplyCommit SET Status = 'RELEASED'"
       " WHERE CompanyCode = ? AND KitRef = ? AND Status = 'ACTIVE'"
       " AND (? = 0 OR LineNum = ?)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE KD-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE KD-KIT-REF TO SQLIO-PARM-VAL (2)
     MOVE KD-LINE-NUM TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (3)
     MOVE FUNCTION TRIM (SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (4)
     MOVE 4 TO SQLIO-PARM-CNT
     PERFORM Run-Update.
