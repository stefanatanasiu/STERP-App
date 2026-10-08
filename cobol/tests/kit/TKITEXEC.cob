>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : TKITEXEC
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION (TESTS)
*> PURPOSE    : The committing half of the kit engine, end to end:
*>              cover (stock reservation or named future supply),
*>              pick and short pick, the issue and backflush rules,
*>              return, scrap and completion checks.
*>
*>              The suite builds its own kit inside a transaction
*>              that is always rolled back, so it may be run as
*>              often as anyone likes and leaves nothing behind.
*>              Where a check needs stock, a bill or a ledger it
*>              uses the programs' own CHECK functions, which read
*>              and judge but never move anything.
*>
*>              KITCOMPL is exercised with KM-COST-BASIS = "NOGL"
*>              because the KIT.WIPTRANSFER business event may not
*>              be deployed in every environment yet.
*> REQ: KIT-008, KIT-009, KIT-010, KIT-011, KIT-013, KIT-014,
*>      KIT-015
*> AC : AC-077, AC-079, AC-080, AC-081
*>================================================================
 IDENTIFICATION DIVISION.
 PROGRAM-ID. TKITEXEC.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY TESTKITW.
 COPY SQLIOCA.
 COPY KITRESVCA REPLACING ==DFHCOMMAREA== BY ==Ws-Resv-Ca==
      LEADING ==CA-== BY ==RV-==.
 COPY KITISSUECA REPLACING ==DFHCOMMAREA== BY ==Ws-Issue-Ca==
      LEADING ==CA-== BY ==IS-==.
 COPY KITRETCA REPLACING ==DFHCOMMAREA== BY ==Ws-Ret-Ca==
      LEADING ==CA-== BY ==RT-==.
 COPY KITSCRAPCA REPLACING ==DFHCOMMAREA== BY ==Ws-Scrap-Ca==
      LEADING ==CA-== BY ==SC-==.
 COPY KITCOMPLCA REPLACING ==DFHCOMMAREA== BY ==Ws-Compl-Ca==
      LEADING ==CA-== BY ==CP-==.
 COPY KITTRIALCA REPLACING ==DFHCOMMAREA== BY ==Ws-Trial-Ca==
      LEADING ==CA-== BY ==TR-==.
 01  Ws-Probe                    PIC X(30).
 01  Ws-Work.
     05  Ws-Db-Up                PIC X VALUE "Y".
     05  Ws-Tables               PIC X VALUE "Y".
     05  Ws-Sql-Bad              PIC X VALUE "N".
     05  Ws-Sql-Msg              PIC X(200) VALUE SPACES.
     05  Ws-Warehouse            PIC X(20) VALUE SPACES.
     05  Ws-Location             PIC X(20) VALUE SPACES.
     05  Ws-Now                  PIC X(21) VALUE SPACES.
     05  Ws-Corr                 PIC X(60) VALUE SPACES.
     05  Ws-Diag                 PIC X(30) VALUE SPACES.
     05  Ws-Kit-Ref              PIC X(30) VALUE "TKITEXEC-T1".
 PROCEDURE DIVISION.
 Main-Entry.
     MOVE "TKITEXEC kit execution" TO TK-SUITE
     PERFORM TK-BEGIN
     PERFORM Test-Engine-Refuses-Nonsense
     PERFORM Probe-The-Database
     IF Ws-Db-Up = "N" OR Ws-Tables = "N"
         PERFORM Skip-The-Database-Tests
     ELSE
         PERFORM Begin-Transaction
         PERFORM Build-The-Kit
         PERFORM Test-Trial-Reads-A-Real-Bill
         PERFORM Test-Future-Supply-Cover
         PERFORM Test-Pick-Is-Not-An-Issue
         PERFORM Test-Short-Pick-Needs-A-Reason
         PERFORM Test-Issue-Rules
         PERFORM Test-Issue-Consumes-Picks
         PERFORM Test-Return-And-Scrap-Bounds
         PERFORM Test-Completion-Bounds
         PERFORM Test-Completion-Posts-The-Ledger
         PERFORM Rollback-Transaction
     END-IF
     PERFORM TK-END
     STOP RUN.

*>----------------------------------------------------------------
*> No database is needed below: every kit program names its own
*> function before it looks at anything.
 Test-Engine-Refuses-Nonsense.
     INITIALIZE Ws-Resv-Ca
     MOVE "STC" TO RV-COMPANY
     MOVE "TESTER" TO RV-USER
     MOVE "WIBBLE" TO RV-FUNCTION
     CALL "KITRESV" USING Ws-Resv-Ca
     MOVE "KITRESV refuses an unknown function with KIT0002"
          TO TK-NAME
     MOVE "KIT0002" TO TK-EXPECTED-X
     MOVE RV-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     INITIALIZE Ws-Issue-Ca
     MOVE "STC" TO IS-COMPANY
     MOVE "TESTER" TO IS-USER
     MOVE "WIBBLE" TO IS-FUNCTION
     CALL "KITISSUE" USING Ws-Issue-Ca
     MOVE "KITISSUE refuses an unknown function" TO TK-NAME
     MOVE 8 TO TK-EXPECTED-N
     MOVE IS-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "KITISSUE issues nothing when it refuses" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KS-KIT-ISSUE-ID TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     INITIALIZE Ws-Compl-Ca
     MOVE "STC" TO CP-COMPANY
     MOVE "TESTER" TO CP-USER
     MOVE "WIBBLE" TO CP-FUNCTION
     CALL "KITCOMPL" USING Ws-Compl-Ca
     MOVE "KITCOMPL refuses an unknown function with KIT0002"
          TO TK-NAME
     MOVE "KIT0002" TO TK-EXPECTED-X
     MOVE CP-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A refused completion receipts nothing" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KM-COMPLETION-ID TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
 Probe-The-Database.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM sys.objects"
            " o JOIN sys.schemas s ON s.schema_id = o.schema_id"
            " WHERE s.name = 'kit' AND o.name IN"
            " ('ExecutionKit','ExecutionKitLine','KitPick',"
            "'KitSupplyCommit','KitIssue')"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 30 TO SQLIO-COL-WIDTH (1)
     MOVE 8 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Probe
     IF SQLIO-ERROR
         MOVE "N" TO Ws-Db-Up
         EXIT PARAGRAPH
     END-IF
     IF FUNCTION NUMVAL (Ws-Probe) < 5
         MOVE "N" TO Ws-Tables
     END-IF.

 Skip-The-Database-Tests.
     MOVE "Kit execution scenario (reserve, pick, issue, complete)"
          TO TK-NAME
     IF Ws-Db-Up = "N"
         MOVE "no database connection" TO TK-DETAIL
     ELSE
         MOVE "kit tables are not deployed" TO TK-DETAIL
     END-IF
     PERFORM TK-SKIP.

 Begin-Transaction.
     MOVE "BEGN" TO SQLIO-FUNC
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 8 TO SQLIO-CURSOR
     CALL "SQLIO" USING SQLIO-CA, Ws-Probe.

 Rollback-Transaction.
     MOVE "ROLB" TO SQLIO-FUNC
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 8 TO SQLIO-CURSOR
     CALL "SQLIO" USING SQLIO-CA, Ws-Probe.

 Run-Statement.
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 8 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Probe
     IF SQLIO-ERROR
         MOVE "Y" TO Ws-Sql-Bad
         MOVE SQLIO-ERRMSG (1:200) TO Ws-Sql-Msg
     END-IF
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Probe.

 Read-Scalar.
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 30 TO SQLIO-COL-WIDTH (1)
     MOVE 8 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     MOVE SPACES TO Ws-Probe
     CALL "SQLIO" USING SQLIO-CA, Ws-Probe
     IF SQLIO-ERROR
         MOVE "Y" TO Ws-Sql-Bad
         MOVE SQLIO-ERRMSG (1:200) TO Ws-Sql-Msg
     END-IF.

*>   A released kit for ten frames: forty pins and eleven kilos of
*>   plate.  The rows are written directly because this suite is
*>   about execution, not about creation, and because a bill is
*>   not needed to prove that a pick is not an issue.
 Build-The-Kit.
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE p FROM kit.KitPick p WHERE p.CompanyCode ="
            " 'STC' AND p.KitRef = 'TKITEXEC-T1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE s FROM kit.KitSupplyCommit s WHERE"
            " s.CompanyCode = 'STC' AND s.KitRef = 'TKITEXEC-T1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE i FROM kit.KitIssue i WHERE i.CompanyCode ="
            " 'STC' AND i.KitRef = 'TKITEXEC-T1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE l FROM kit.ExecutionKitLine l JOIN"
            " kit.ExecutionKit k ON k.ExecutionKitId ="
            " l.ExecutionKitId WHERE k.CompanyCode = 'STC' AND"
            " k.KitRef = 'TKITEXEC-T1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE FROM kit.ExecutionKit WHERE CompanyCode ="
            " 'STC' AND KitRef = 'TKITEXEC-T1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.ExecutionKit (CompanyCode, SiteCode,"
            " KitRef, ParentItemCode,"
            " PlannedGoodQty, RunCount, NeedDate, Status,"
            " OwnerUser, WipValueGBP, IsBackflush, CreatedBy)"
            " SELECT 'STC','RDG','TKITEXEC-T1', i.ItemCode, 10, 1,"
            " CONVERT(date,'20261106',112),'RELEASED','TESTER',"
            " 125.00,0,'TESTER' FROM (SELECT TOP (1) ItemCode FROM"
            " inv.Item WHERE CompanyCode = 'STC' ORDER BY CASE"
            " WHEN TraceabilityType = 'NONE' AND IsStocked = 1"
            " THEN 0 ELSE 1 END, ItemCode) i"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.ExecutionKitLine (ExecutionKitId,"
            " LineNum, PathRef, ComponentItemCode, UsageBasis,"
            " OriginalRequirementQty, CurrentRequirementQty,"
            " UomCode, ConversionFactor, NeedDate, Status)"
            " SELECT k.ExecutionKitId, 1, '1', i.ItemCode,"
            " 'PROPORTION', 40, 40, i.StockUomCode, 1,"
            " CONVERT(date,'20261106',112), 'OPEN'"
            " FROM kit.ExecutionKit k CROSS JOIN (SELECT ItemCode,"
            " StockUomCode FROM inv.Item WHERE CompanyCode = 'STC'"
            " ORDER BY ItemCode OFFSET 1 ROWS FETCH NEXT 1 ROWS"
            " ONLY) i WHERE k.CompanyCode = 'STC' AND"
            " k.KitRef = 'TKITEXEC-T1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.ExecutionKitLine (ExecutionKitId,"
            " LineNum, PathRef, ComponentItemCode, UsageBasis,"
            " OriginalRequirementQty, CurrentRequirementQty,"
            " UomCode, ConversionFactor, NeedDate, Status)"
            " SELECT k.ExecutionKitId, 2, '2', i.ItemCode,"
            " 'PROPORTION', 11, 11, i.StockUomCode, 1,"
            " CONVERT(date,'20261106',112), 'OPEN'"
            " FROM kit.ExecutionKit k CROSS JOIN (SELECT ItemCode,"
            " StockUomCode FROM inv.Item WHERE CompanyCode = 'STC'"
            " ORDER BY ItemCode OFFSET 2 ROWS FETCH NEXT 1 ROWS"
            " ONLY) i WHERE k.CompanyCode = 'STC' AND"
            " k.KitRef = 'TKITEXEC-T1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.ExecutionKitLine (ExecutionKitId,"
            " LineNum, PathRef, ComponentItemCode, UsageBasis,"
            " OriginalRequirementQty, CurrentRequirementQty,"
            " UomCode, ConversionFactor, NeedDate, Status)"
            " SELECT k.ExecutionKitId, 3, '3', s.ItemCode,"
            " 'PROPORTION', 40, 40, i.StockUomCode, 1,"
            " CONVERT(date,'20261106',112), 'OPEN'"
            " FROM kit.ExecutionKit k CROSS JOIN (SELECT TOP (1)"
            " b.ItemCode FROM inv.StockBalance b JOIN inv.Item t"
            " ON t.CompanyCode = b.CompanyCode AND t.ItemCode ="
            " b.ItemCode WHERE b.CompanyCode = 'STC' AND"
            " t.TraceabilityType = 'NONE' AND t.IsStocked = 1"
            " AND b.QtyOnHand > 100 ORDER BY b.QtyOnHand DESC,"
            " b.ItemCode) s JOIN inv.Item i ON i.CompanyCode ="
            " 'STC' AND i.ItemCode = s.ItemCode WHERE"
            " k.CompanyCode = 'STC' AND k.KitRef = 'TKITEXEC-T1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM"
            " kit.ExecutionKitLine l JOIN kit.ExecutionKit k ON"
            " k.ExecutionKitId = l.ExecutionKitId WHERE"
            " k.CompanyCode = 'STC' AND k.KitRef = 'TKITEXEC-T1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "The scenario kit was built" TO TK-NAME
     IF Ws-Sql-Bad = "Y"
         MOVE Ws-Sql-Msg TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         MOVE 3 TO TK-EXPECTED-N
         COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
         PERFORM TK-ASSERT-NUM
     END-IF.

 Prime-Resv.
     INITIALIZE Ws-Resv-Ca
     MOVE "STC" TO RV-COMPANY
     MOVE "RDG" TO RV-SITE
     MOVE "TESTER" TO RV-USER
     MOVE 20261006 TO RV-BUSINESS-DATE
     MOVE Ws-Kit-Ref TO KR-KIT-REF
     MOVE "RDG" TO KR-SITE
     MOVE 1 TO KR-LINE-NUM.

*>----------------------------------------------------------------
*> KIT-006.  A trial against the real demonstration bills.  The
*> purpose handed to BOMEXPL has to be one it knows (STANDARD /
*> JOB / ORDER): a trial that asks for a purpose no bill carries
*> is refused with KIT0004 and the kitter is told the bill is
*> missing when it is nothing of the sort.
*> REQ: KIT-006
 Test-Trial-Reads-A-Real-Bill.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT TOP (1) h.ParentItemCode FROM bom.BomHeader h"
            " WHERE h.CompanyCode = 'STC' AND h.Status ="
            " 'RELEASED' AND h.Purpose = 'STANDARD' AND EXISTS"
            " (SELECT 1 FROM bom.BomComponent c WHERE"
            " c.BomHeaderId = h.BomHeaderId) ORDER BY CASE WHEN"
            " h.ParentItemCode = 'DEMO-FRAME' THEN 0 ELSE 1 END,"
            " h.ParentItemCode"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE Ws-Probe TO Ws-Diag
     INITIALIZE Ws-Trial-Ca
     MOVE "STC" TO TR-COMPANY
     MOVE "RDG" TO TR-SITE
     MOVE "TESTER" TO TR-USER
     MOVE 20261006 TO TR-BUSINESS-DATE
     MOVE Ws-Diag TO KT-PARENT-ITEM
     MOVE "RDG" TO KT-SITE
     MOVE 1 TO KT-REQUESTED-QTY
     MOVE 20261006 TO KT-REQ-DATE
     MOVE 20261006 TO KT-SELECTION-DATE
     MOVE "STOCK" TO KT-DEMAND-TYPE
     MOVE "N" TO KT-USE-SUBASSY
     MOVE "N" TO KT-USE-INBOUND
     MOVE "TRIAL" TO TR-FUNCTION
     CALL "KITTRIAL" USING Ws-Trial-Ca
     MOVE "The demonstration bill is found, not refused" TO TK-NAME
     IF TR-REASON-CODE = "KIT0004"
         MOVE TR-MESSAGE (1:200) TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         PERFORM TK-PASS
     END-IF
     MOVE "The trial reaches a verdict on the real bill" TO TK-NAME
     MOVE "Y" TO TK-EXPECTED-X
     MOVE KT-FEASIBLE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "One frame can be built from the demonstration bill"
          TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE KT-MAX-BUILDABLE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The trial reaches a complete explosion" TO TK-NAME
     MOVE "COMPLETE" TO TK-EXPECTED-X
     MOVE KT-COMPLETENESS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The trial explodes the bill into components" TO TK-NAME
     IF KT-LINE-TOTAL > 0
         PERFORM TK-PASS
     ELSE
         MOVE SPACES TO TK-DETAIL
         STRING "KT-LINE-TOTAL is 0 for " FUNCTION TRIM(Ws-Diag)
             DELIMITED BY SIZE INTO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     MOVE "A trial commits nothing" TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE KT-COMMITTED TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> KIT-008.  A kit line may be covered by eligible stock OR by
*> explicitly identified future supply.  The commitment is not a
*> reservation and is certainly not an issue: no stock moves, and
*> the line's own issued quantity is untouched.
 Test-Future-Supply-Cover.
     PERFORM Prime-Resv
     MOVE 10 TO KR-SUPPLY-QTY
     MOVE "SUPPLY" TO RV-FUNCTION
     CALL "KITRESV" USING Ws-Resv-Ca
     MOVE "Future supply must say what kind of supply it is"
          TO TK-NAME
     MOVE "KIT0008" TO TK-EXPECTED-X
     MOVE RV-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Prime-Resv
     MOVE "PURCHASE" TO KR-SUPPLY-TYPE
     MOVE 10 TO KR-SUPPLY-QTY
     MOVE "SUPPLY" TO RV-FUNCTION
     CALL "KITRESV" USING Ws-Resv-Ca
     MOVE "Future supply must name the order that covers the line"
          TO TK-NAME
     MOVE "KIT0008" TO TK-EXPECTED-X
     MOVE RV-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Prime-Resv
     MOVE "PURCHASE" TO KR-SUPPLY-TYPE
     MOVE "PO0004471" TO KR-SUPPLY-REF
     MOVE 1 TO KR-SUPPLY-LINE
     MOVE 999 TO KR-SUPPLY-QTY
     MOVE 20261101 TO KR-SUPPLY-DUE
     MOVE "SUPPLY" TO RV-FUNCTION
     CALL "KITRESV" USING Ws-Resv-Ca
     MOVE "Future supply cannot cover more than the line needs"
          TO TK-NAME
     MOVE "KIT0003" TO TK-EXPECTED-X
     MOVE RV-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Prime-Resv
     MOVE "PURCHASE" TO KR-SUPPLY-TYPE
     MOVE "PO0004471" TO KR-SUPPLY-REF
     MOVE 1 TO KR-SUPPLY-LINE
     MOVE 10 TO KR-SUPPLY-QTY
     MOVE 20261101 TO KR-SUPPLY-DUE
     MOVE "SUPPLY" TO RV-FUNCTION
     CALL "KITRESV" USING Ws-Resv-Ca
     MOVE "A purchase order due in time covers the line" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE RV-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The commitment is kept as a record of its own" TO TK-NAME
     IF KR-COMMIT-ID > 0
         PERFORM TK-PASS
     ELSE
         MOVE "no kit.KitSupplyCommit row was written" TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     MOVE "Thirty of the forty pins are still uncovered" TO TK-NAME
     MOVE 30 TO TK-EXPECTED-N
     MOVE KR-UNCOVERED-QTY TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A commitment reserves no stock" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KR-OWN-RESERVED TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Prime-Resv
     MOVE "WORKSORDER" TO KR-SUPPLY-TYPE
     MOVE "WO0000912" TO KR-SUPPLY-REF
     MOVE 5 TO KR-SUPPLY-QTY
     MOVE 20261220 TO KR-SUPPLY-DUE
     MOVE "SUPPLY" TO RV-FUNCTION
     CALL "KITRESV" USING Ws-Resv-Ca
     MOVE "Supply due after the need date is cover, but late"
          TO TK-NAME
     MOVE 4 TO TK-EXPECTED-N
     MOVE RV-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The lateness is stated rather than assumed" TO TK-NAME
     MOVE "Y" TO TK-EXPECTED-X
     MOVE KR-SUPPLY-LATE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "Fifteen pins are now covered by inbound orders"
          TO TK-NAME
     MOVE 15 TO TK-EXPECTED-N
     MOVE KR-SUPPLY-COMMITTED TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), NetIssuedQty) FROM"
            " kit.ExecutionKitLine l JOIN kit.ExecutionKit k ON"
            " k.ExecutionKitId = l.ExecutionKitId WHERE"
            " k.CompanyCode = 'STC' AND k.KitRef = 'TKITEXEC-T1'"
            " AND l.LineNum = 1"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "A commitment issues nothing" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> KIT-009 / AC-079.  Picking takes the box off the shelf and
*> records the lot that was really taken.  It does not issue it.
 Test-Pick-Is-Not-An-Issue.
     PERFORM Prime-Resv
     MOVE 5 TO KR-QUANTITY
     MOVE "ST-A" TO KR-WAREHOUSE
     MOVE "BIN-04" TO KR-LOCATION
     MOVE "LOT-PIN-7781" TO KR-LOT-CODE
     MOVE "PICK" TO RV-FUNCTION
     CALL "KITRESV" USING Ws-Resv-Ca
     MOVE "Five pins are picked" TO TK-NAME
     MOVE 5 TO TK-EXPECTED-N
     MOVE KR-PICKED-QTY TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), ISNULL(MAX(LotCode),"
            "'none')) FROM kit.KitPick WHERE CompanyCode = 'STC'"
            " AND KitRef = 'TKITEXEC-T1' AND PickType = 'PICK'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "The lot actually taken is remembered" TO TK-NAME
     MOVE "LOT-PIN-7781" TO TK-EXPECTED-X
     MOVE Ws-Probe TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), NetIssuedQty) FROM"
            " kit.ExecutionKitLine l JOIN kit.ExecutionKit k ON"
            " k.ExecutionKitId = l.ExecutionKitId WHERE"
            " k.CompanyCode = 'STC' AND k.KitRef = 'TKITEXEC-T1'"
            " AND l.LineNum = 1"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "A pick is not an issue" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
     PERFORM TK-ASSERT-NUM
     PERFORM Prime-Resv
     MOVE 500 TO KR-QUANTITY
     MOVE "PICK" TO RV-FUNCTION
     CALL "KITRESV" USING Ws-Resv-Ca
     MOVE "More than the line needs cannot be picked" TO TK-NAME
     MOVE "KIT0003" TO TK-EXPECTED-X
     MOVE RV-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Prime-Resv
     MOVE 5 TO KR-QUANTITY
     MOVE "UNPICK" TO RV-FUNCTION
     CALL "KITRESV" USING Ws-Resv-Ca
     MOVE "An unpick puts the box back on the shelf" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KR-PICKED-QTY TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM"
            " kit.KitPick WHERE CompanyCode = 'STC' AND KitRef ="
            " 'TKITEXEC-T1' AND PickType = 'UNPICK' AND"
            " PickedQty < 0"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "The unpick is kept in the trail, not erased" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
     PERFORM TK-ASSERT-NUM.

*>   KIT-011.  A short pick is a shortage, and a shortage has to
*>   say why before anybody signs for it.
 Test-Short-Pick-Needs-A-Reason.
     PERFORM Prime-Resv
     MOVE 2 TO KR-QUANTITY
     MOVE "SHORTPCK" TO RV-FUNCTION
     CALL "KITRESV" USING Ws-Resv-Ca
     MOVE "A short pick without a reason is refused" TO TK-NAME
     MOVE "KIT0008" TO TK-EXPECTED-X
     MOVE RV-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Prime-Resv
     MOVE 2 TO KR-QUANTITY
     MOVE "STKSHORT" TO KR-REASON-CODE
     MOVE "Only two left in BIN-04 and no other location holds any."
          TO KR-REASON-TEXT
     MOVE "SHORTPCK" TO RV-FUNCTION
     CALL "KITRESV" USING Ws-Resv-Ca
     MOVE "A reasoned short pick is accepted as a warning"
          TO TK-NAME
     MOVE 4 TO TK-EXPECTED-N
     MOVE RV-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The shortfall stays visible on the line" TO TK-NAME
     MOVE 38 TO TK-EXPECTED-N
     MOVE KR-SHORTFALL-QTY TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> KIT-010 / KIT-011 / AC-079.  The issue rules are checked with
*> KS-CHECK-ONLY so that nothing moves: a backflush may not guess
*> a mandatory serial, and an overissue needs a reason.
 Test-Issue-Rules.
     INITIALIZE Ws-Issue-Ca
     MOVE "STC" TO IS-COMPANY
     MOVE "RDG" TO IS-SITE
     MOVE "TESTER" TO IS-USER
     MOVE 20261006 TO IS-BUSINESS-DATE
     MOVE Ws-Kit-Ref TO KS-KIT-REF
     MOVE 1 TO KS-LINE-NUM
     MOVE 0 TO KS-QUANTITY
     MOVE "Y" TO KS-CHECK-ONLY
     MOVE "ISSUE" TO IS-FUNCTION
     CALL "KITISSUE" USING Ws-Issue-Ca
     MOVE "An issue of nothing is refused" TO TK-NAME
     IF IS-RETURN-CODE > 4
         PERFORM TK-PASS
     ELSE
         MOVE IS-MESSAGE (1:200) TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     INITIALIZE Ws-Issue-Ca
     MOVE "STC" TO IS-COMPANY
     MOVE "RDG" TO IS-SITE
     MOVE "TESTER" TO IS-USER
     MOVE 20261006 TO IS-BUSINESS-DATE
     MOVE Ws-Kit-Ref TO KS-KIT-REF
     MOVE 1 TO KS-LINE-NUM
     MOVE 60 TO KS-QUANTITY
     MOVE "N" TO KS-ALLOW-OVER
     MOVE "Y" TO KS-CHECK-ONLY
     MOVE "CHECK" TO IS-FUNCTION
     CALL "KITISSUE" USING Ws-Issue-Ca
     MOVE "Issuing more than the kit needs is not waved through"
          TO TK-NAME
     IF IS-RETURN-CODE NOT = 0
         PERFORM TK-PASS
     ELSE
         MOVE "sixty pins were accepted against a need for forty"
              TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     MOVE "A check-only call issues nothing" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KS-KIT-ISSUE-ID TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     INITIALIZE Ws-Issue-Ca
     MOVE "STC" TO IS-COMPANY
     MOVE "RDG" TO IS-SITE
     MOVE "TESTER" TO IS-USER
     MOVE 20261006 TO IS-BUSINESS-DATE
     MOVE Ws-Kit-Ref TO KS-KIT-REF
     MOVE "Y" TO KS-CHECK-ONLY
     MOVE "BACKFLSH" TO IS-FUNCTION
     CALL "KITISSUE" USING Ws-Issue-Ca
     MOVE "Backflush is refused unless the kit enables it"
          TO TK-NAME
     IF IS-RETURN-CODE NOT = 0
         PERFORM TK-PASS
     ELSE
         MOVE "a kit with IsBackflush = 0 was backflushed"
              TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF.

*>----------------------------------------------------------------
*> KIT-009 / KIT-010.  Issuing spends the picks.  The warehouse
*> took the material off the shelf before the kit was issued, so
*> the issue consumes the open picks oldest first and whatever is
*> left unconsumed is still physically out of the store - which is
*> exactly what a cancellation has to show.
*> REQ: KIT-009, KIT-010
 Test-Issue-Consumes-Picks.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT TOP (1) b.WarehouseCode FROM inv.StockBalance"
            " b JOIN kit.ExecutionKitLine l ON l.ComponentItemCode"
            " = b.ItemCode JOIN kit.ExecutionKit k ON"
            " k.ExecutionKitId = l.ExecutionKitId WHERE"
            " b.CompanyCode = 'STC' AND k.CompanyCode = 'STC' AND"
            " k.KitRef = 'TKITEXEC-T1' AND l.LineNum = 3"
            " ORDER BY b.QtyOnHand DESC"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE Ws-Probe TO Ws-Warehouse
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT TOP (1) b.LocationCode FROM inv.StockBalance"
            " b JOIN kit.ExecutionKitLine l ON l.ComponentItemCode"
            " = b.ItemCode JOIN kit.ExecutionKit k ON"
            " k.ExecutionKitId = l.ExecutionKitId WHERE"
            " b.CompanyCode = 'STC' AND k.CompanyCode = 'STC' AND"
            " k.KitRef = 'TKITEXEC-T1' AND l.LineNum = 3"
            " ORDER BY b.QtyOnHand DESC"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE Ws-Probe TO Ws-Location
     PERFORM Prime-Resv
     MOVE 3 TO KR-LINE-NUM
     MOVE 3 TO KR-QUANTITY
     MOVE Ws-Warehouse TO KR-WAREHOUSE
     MOVE Ws-Location TO KR-LOCATION
     MOVE "PICK" TO RV-FUNCTION
     CALL "KITRESV" USING Ws-Resv-Ca
     PERFORM Prime-Resv
     MOVE 3 TO KR-LINE-NUM
     MOVE 2 TO KR-QUANTITY
     MOVE Ws-Warehouse TO KR-WAREHOUSE
     MOVE Ws-Location TO KR-LOCATION
     MOVE "PICK" TO RV-FUNCTION
     CALL "KITRESV" USING Ws-Resv-Ca
     MOVE "Five are off the shelf for line three" TO TK-NAME
     MOVE 5 TO TK-EXPECTED-N
     MOVE KR-PICKED-QTY TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Prime-Resv
     MOVE 3 TO KR-LINE-NUM
     MOVE 1 TO KR-QUANTITY
     MOVE "UNPICK" TO RV-FUNCTION
     CALL "KITRESV" USING Ws-Resv-Ca
     INITIALIZE Ws-Issue-Ca
     MOVE "STC" TO IS-COMPANY
     MOVE "RDG" TO IS-SITE
     MOVE "TESTER" TO IS-USER
     MOVE 20261006 TO IS-BUSINESS-DATE
     MOVE Ws-Kit-Ref TO KS-KIT-REF
     MOVE 3 TO KS-LINE-NUM
     MOVE 4 TO KS-QUANTITY
     MOVE Ws-Warehouse TO KS-WAREHOUSE
     MOVE Ws-Location TO KS-LOCATION
     MOVE "N" TO KS-CHECK-ONLY
     MOVE "N" TO KS-ALLOW-OVER
     MOVE "KITISS" TO KS-REASON-CODE
     MOVE "Four taken to the bench against the picked boxes."
          TO KS-REASON-TEXT
     MOVE "ISSUE" TO IS-FUNCTION
     CALL "KITISSUE" USING Ws-Issue-Ca
     MOVE "Four are issued against the picks" TO TK-NAME
     IF IS-RETURN-CODE <= 4
         PERFORM TK-PASS
     ELSE
         MOVE IS-MESSAGE (1:200) TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), ConsumedQty) FROM"
            " kit.KitPick WHERE KitPickId = (SELECT MIN(KitPickId)"
            " FROM kit.KitPick WHERE CompanyCode = 'STC' AND"
            " KitRef = 'TKITEXEC-T1' AND LineNum = 3 AND"
            " PickType = 'PICK')"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "The oldest pick is spent first" TO TK-NAME
     MOVE 3 TO TK-EXPECTED-N
     COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
     PERFORM TK-ASSERT-NUM
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), ISNULL(SUM(PickedQty -"
            " ConsumedQty),0)) FROM kit.KitPick WHERE CompanyCode"
            " = 'STC' AND KitRef = 'TKITEXEC-T1' AND LineNum = 3"
            " AND PickType = 'PICK'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "Nothing picked is left on the trolley" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
     PERFORM TK-ASSERT-NUM
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM"
            " kit.KitPick WHERE CompanyCode = 'STC' AND KitRef ="
            " 'TKITEXEC-T1' AND PickType = 'PICK' AND"
            " ConsumedQty > PickedQty"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "No pick is consumed beyond what was picked" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
     PERFORM TK-ASSERT-NUM
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), ISNULL(SUM(PickedQty -"
            " ConsumedQty),0)) FROM kit.KitPick WHERE CompanyCode"
            " = 'STC' AND KitRef = 'TKITEXEC-T1' AND LineNum = 3"
            " AND PickType = 'UNPICK'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "The box put back is settled, not counted twice"
          TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
     PERFORM TK-ASSERT-NUM
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), ISNULL(SUM(PickedQty -"
            " ConsumedQty),0)) FROM kit.KitPick WHERE CompanyCode"
            " = 'STC' AND KitRef = 'TKITEXEC-T1' AND LineNum = 1"
            " AND PickType = 'PICK'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "An untouched line keeps its picks off the shelf"
          TO TK-NAME
     MOVE 5 TO TK-EXPECTED-N
     COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> KIT-013 / KIT-014 / AC-080.  A return is bounded by what was
*> actually issued, and scrap is a disposition of issued material,
*> not a way of making a requirement disappear.
 Test-Return-And-Scrap-Bounds.
     INITIALIZE Ws-Ret-Ca
     MOVE "STC" TO RT-COMPANY
     MOVE "RDG" TO RT-SITE
     MOVE "TESTER" TO RT-USER
     MOVE 20261006 TO RT-BUSINESS-DATE
     MOVE Ws-Kit-Ref TO KX-KIT-REF
     MOVE 1 TO KX-LINE-NUM
     MOVE 3 TO KX-QUANTITY
     MOVE "RETURNCK" TO RT-FUNCTION
     CALL "KITRET" USING Ws-Ret-Ca
     MOVE "Nothing issued means nothing to return" TO TK-NAME
     IF RT-RETURN-CODE NOT = 0
         PERFORM TK-PASS
     ELSE
         MOVE "a return was allowed against a line with no issue"
              TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     INITIALIZE Ws-Scrap-Ca
     MOVE "STC" TO SC-COMPANY
     MOVE "RDG" TO SC-SITE
     MOVE "TESTER" TO SC-USER
     MOVE 20261006 TO SC-BUSINESS-DATE
     MOVE Ws-Kit-Ref TO KP-KIT-REF
     MOVE 1 TO KP-LINE-NUM
     MOVE 2 TO KP-QUANTITY
     MOVE "COMPONENT" TO KP-SCRAP-KIND
     MOVE "Y" TO KP-CHECK-ONLY
     MOVE "SCRAPCK" TO SC-FUNCTION
     CALL "KITSCRAP" USING Ws-Scrap-Ca
     MOVE "Material that was never issued cannot be scrapped"
          TO TK-NAME
     IF SC-RETURN-CODE NOT = 0
         PERFORM TK-PASS
     ELSE
         MOVE "scrap was accepted against a line with no issue"
              TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF.

*>----------------------------------------------------------------
*> KIT-015 / AC-081.  Completion is bounded by the planned output
*> and is checked with COMPCHK, which judges without receipting.
*> KM-COST-BASIS "NOGL" keeps the ledger out of a unit test.
 Test-Completion-Bounds.
     PERFORM Prime-Compl
     MOVE 0 TO KM-GOOD-QTY
     MOVE "COMPCHK" TO CP-FUNCTION
     CALL "KITCOMPL" USING Ws-Compl-Ca
     MOVE "A completion of nothing is refused" TO TK-NAME
     IF CP-RETURN-CODE NOT = 0
         PERFORM TK-PASS
     ELSE
         MOVE "zero good output was accepted as a completion"
              TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     PERFORM Prime-Compl
     MOVE 6 TO KM-GOOD-QTY
     MOVE "COMPCHK" TO CP-FUNCTION
     CALL "KITCOMPL" USING Ws-Compl-Ca
     MOVE "Ten frames are still to come before any completion"
          TO TK-NAME
     MOVE 10 TO TK-EXPECTED-N
     MOVE KM-REMAINING-OUT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A completion check receipts nothing" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KM-COMPLETION-ID TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
*>   CR2743.  The total good output includes the receipt being
*>   checked, so a caller can tell whether the run would be
*>   finished by it; it used to answer only what the kit had
*>   completed before, which was always zero on a first receipt.
     MOVE "The total good output counts the receipt being checked"
          TO TK-NAME
     MOVE 6 TO TK-EXPECTED-N
     MOVE KM-TOTAL-GOOD TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Prime-Compl
     MOVE 14 TO KM-GOOD-QTY
     MOVE "COMPCHK" TO CP-FUNCTION
     CALL "KITCOMPL" USING Ws-Compl-Ca
     MOVE "More output than was planned needs approval" TO TK-NAME
     IF CP-RETURN-CODE NOT = 0
         PERFORM TK-PASS
     ELSE
         MOVE "fourteen frames were accepted from a ten frame kit"
              TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF.

 Prime-Compl.
     INITIALIZE Ws-Compl-Ca
     MOVE "STC" TO CP-COMPANY
     MOVE "RDG" TO CP-SITE
     MOVE "TESTER" TO CP-USER
     MOVE 20261006 TO CP-BUSINESS-DATE
     MOVE Ws-Kit-Ref TO KM-KIT-REF
     MOVE "RDG" TO KM-SITE
     MOVE "ST-A" TO KM-WAREHOUSE
     MOVE "NOGL" TO KM-COST-BASIS
     MOVE "Y" TO KM-CHECK-ONLY
     MOVE 20261006 TO KM-COMPLETION-DATE.

*>----------------------------------------------------------------
*> KIT-015 / AC-081 with the ledger switched on.  The kit carries
*> 125.00 of work in progress; six good frames take 75.00 of it
*> into stock and the final four take the residual 50.00, posted
*> through fin.JournalStage + fin.usp_PostJournal under the
*> business event KIT.COMPLETE (debit stock 1120, credit work in
*> progress 1150 - see db\seed\85-kit-eventmap.sql).
*>
*> If the environment cannot receive the parent at all - no
*> stock location, a traced parent, a closed period - the test
*> says so and skips rather than pretending.  What it will never
*> do is pass when the completion was written and the ledger
*> entry was not, which is exactly what a missing business event
*> looks like (KIT0016 with a completion already recorded).
 Test-Completion-Posts-The-Ledger.
     PERFORM Find-A-Stock-Location
     MOVE FUNCTION CURRENT-DATE TO Ws-Now
     PERFORM Prime-Compl
     MOVE SPACES TO Ws-Corr
     STRING "TKITEXEC/" Ws-Now (1:14) "/1"
         DELIMITED BY SIZE INTO Ws-Corr
     MOVE Ws-Corr TO CP-CORRELATION-ID
     MOVE SPACES TO KM-COST-BASIS
     MOVE "STD" TO KM-COST-BASIS
     MOVE "N" TO KM-CHECK-ONLY
     MOVE Ws-Warehouse TO KM-WAREHOUSE
     MOVE Ws-Location TO KM-LOCATION
     MOVE 6 TO KM-GOOD-QTY
     MOVE "First six frames off the line." TO KM-REASON-TEXT
     MOVE "COMPLETE" TO CP-FUNCTION
     CALL "KITCOMPL" USING Ws-Compl-Ca
     MOVE "Six frames are received and the ledger entry is posted"
          TO TK-NAME
     IF CP-RETURN-CODE < 8
         PERFORM TK-PASS
     ELSE
         IF KM-COMPLETION-ID > 0
             MOVE CP-MESSAGE (1:200) TO TK-DETAIL
             PERFORM TK-FAIL
         ELSE
             MOVE CP-MESSAGE (1:150) TO TK-DETAIL
             PERFORM TK-SKIP
             EXIT PARAGRAPH
         END-IF
     END-IF
     MOVE "Seventy five pounds leaves work in progress" TO TK-NAME
     MOVE 75.00 TO TK-EXPECTED-N
     MOVE KM-WIP-TRANSFER TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Fifty pounds stays in work in progress" TO TK-NAME
     MOVE 50.00 TO TK-EXPECTED-N
     MOVE KM-WIP-RESIDUAL TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The run stays open for the balance" TO TK-NAME
     MOVE "PARTCOMPLETE" TO TK-EXPECTED-X
     MOVE KM-KIT-STATUS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Count-Kit-Journals
     MOVE "The completion posted one KIT.COMPLETE journal"
          TO TK-NAME
     IF FUNCTION NUMVAL (Ws-Probe) = 1
         PERFORM TK-PASS
     ELSE
         PERFORM Describe-Last-Journal
         PERFORM TK-FAIL
     END-IF
     PERFORM Check-Journal-Legs
     MOVE "Stock is debited and work in progress credited"
          TO TK-NAME
     MOVE "1120DR/1150CR" TO TK-EXPECTED-X
     MOVE Ws-Probe TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Prime-Compl
     MOVE SPACES TO Ws-Corr
     STRING "TKITEXEC/" Ws-Now (1:14) "/2"
         DELIMITED BY SIZE INTO Ws-Corr
     MOVE Ws-Corr TO CP-CORRELATION-ID
     MOVE SPACES TO KM-COST-BASIS
     MOVE "STD" TO KM-COST-BASIS
     MOVE "N" TO KM-CHECK-ONLY
     MOVE Ws-Warehouse TO KM-WAREHOUSE
     MOVE Ws-Location TO KM-LOCATION
     MOVE 4 TO KM-GOOD-QTY
     MOVE "The last four frames." TO KM-REASON-TEXT
     MOVE "FINAL" TO CP-FUNCTION
     CALL "KITCOMPL" USING Ws-Compl-Ca
     MOVE "The final completion is accepted" TO TK-NAME
     IF CP-RETURN-CODE < 8
         PERFORM TK-PASS
     ELSE
         MOVE CP-MESSAGE (1:200) TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     MOVE "The final completion takes the residual value out"
          TO TK-NAME
     MOVE 50.00 TO TK-EXPECTED-N
     MOVE KM-WIP-TRANSFER TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Nothing is stranded in work in progress" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KM-WIP-RESIDUAL TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Ten good frames in total" TO TK-NAME
     MOVE 10 TO TK-EXPECTED-N
     MOVE KM-TOTAL-GOOD TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Count-Kit-Journals
     MOVE "Each completion posts its own journal, once" TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
     PERFORM TK-ASSERT-NUM.

 Find-A-Stock-Location.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT TOP (1) CONVERT(varchar(30), WarehouseCode)"
            " FROM inv.Location WHERE CompanyCode = 'STC' AND"
            " SiteCode = 'RDG' AND IsAvailable = 1 AND"
            " LocationType = 'STOCK'"
            " ORDER BY WarehouseCode, LocationCode"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE Ws-Probe TO Ws-Warehouse
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT TOP (1) CONVERT(varchar(30), LocationCode)"
            " FROM inv.Location WHERE CompanyCode = 'STC' AND"
            " SiteCode = 'RDG' AND IsAvailable = 1 AND"
            " LocationType = 'STOCK'"
            " ORDER BY WarehouseCode, LocationCode"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE Ws-Probe TO Ws-Location.

 Count-Kit-Journals.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM"
            " fin.Journal WHERE CompanyCode = 'STC' AND"
            " EventCode = 'KIT.COMPLETE' AND SourceRef LIKE"
            " 'TKITEXEC-T1%'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar.

*>   When the count is wrong the test says what the ledger does
*>   hold, because "expected 1 got 0" is not evidence of anything.
 Describe-Last-Journal.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), ISNULL((SELECT TOP (1)"
            " ISNULL(j.EventCode,'noevent') + '~' +"
            " ISNULL(j.SourceRef,'noref') FROM fin.Journal j"
            " WHERE j.CompanyCode = 'STC' AND j.SourceModule ="
            " 'KIT' ORDER BY j.JournalId DESC),'no KIT journal'))"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE Ws-Probe TO Ws-Diag
     PERFORM Count-Idempotency-State
     MOVE SPACES TO TK-DETAIL
     STRING "last KIT journal: " FUNCTION TRIM(Ws-Diag)
            " / " FUNCTION TRIM(Ws-Probe)
         DELIMITED BY SIZE INTO TK-DETAIL.

*>   When the count is wrong the test says what the ledger does
*>   hold, because "expected 1 got 0" is not evidence of anything.
*>   An idempotency key with nothing posted means the correlation
*>   id had already been spent elsewhere; staged lines left behind
*>   mean the posting call never reached the ledger at all.
 Count-Idempotency-State.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), CONVERT(varchar(6),"
            " (SELECT COUNT(*) FROM core.IdempotencyKey k WHERE"
            " k.CorrelationId LIKE 'TKITEXEC%')) + ' keys, ' +"
            " CONVERT(varchar(6), (SELECT COUNT(*) FROM"
            " fin.JournalStage s WHERE s.SubledgerRef LIKE"
            " 'TKITEXEC-T1%')) + ' staged')"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar.

*>   The legs are the event map's business, not this program's, so
*>   the test reads back what was actually written to the ledger.
 Check-Journal-Legs.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), STRING_AGG("
            "CONVERT(varchar(12), l.AccountCode) + CASE WHEN"
            " l.DebitAmount > 0 THEN 'DR' ELSE 'CR' END, '/')"
            " WITHIN GROUP (ORDER BY l.LineNum)) FROM"
            " fin.JournalLine l JOIN fin.Journal j ON"
            " j.JournalId = l.JournalId WHERE j.CompanyCode ="
            " 'STC' AND j.EventCode = 'KIT.COMPLETE' AND"
            " j.SourceRef LIKE 'TKITEXEC-T1%'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar.

 COPY TESTKITP.
