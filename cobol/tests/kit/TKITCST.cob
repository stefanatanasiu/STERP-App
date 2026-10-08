>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : TKITCST
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION (TESTS)
*> PURPOSE    : KITCOST, specification 09 section 8.  Seven numbers
*>              that reconcile:
*>
*>                residual = issued - returned + absorbed - completed
*>
*>              and the rule that scrap is a DISPOSITION, not a
*>              reversal - scrapped value stays inside issued and is
*>              never taken out again (KIT-014, KIT-020).
*>
*>              The suite also proves AC-081: a made subassembly is
*>              charged to the parent kit at the value of the
*>              subassembly only.  Its own child material was
*>              charged to the child kit, so charging it again to
*>              the parent is a double count and KK-DOUBLE-CHECK
*>              must say so.
*>
*>              Everything is built inside a transaction that is
*>              always rolled back, so the suite leaves nothing
*>              behind and may be run as often as anyone likes.
*> REQ: KIT-014, KIT-020
*> AC : AC-081
*>================================================================
 IDENTIFICATION DIVISION.
 PROGRAM-ID. TKITCST.
 AUTHOR. STERP.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY TESTKITW.
 COPY SQLIOCA.
 COPY KITCOSTCA REPLACING ==DFHCOMMAREA== BY ==Ws-Cost-Ca==
      LEADING ==CA-== BY ==KA-==.
 01  Ws-Probe                    PIC X(60).
 01  Ws-Work.
     05  Ws-Db-Up                PIC X VALUE "Y".
     05  Ws-Tables               PIC X VALUE "Y".
     05  Ws-Sql-Bad              PIC X VALUE "N".
     05  Ws-Sql-Msg              PIC X(200) VALUE SPACES.
     05  Ws-W                    PIC S9(4) VALUE 0.
     05  Ws-Kit-Ref              PIC X(30) VALUE "TKITCST-P1".
     05  Ws-Child-Ref            PIC X(30) VALUE "TKITCST-C1".
     05  Ws-Identity             PIC S9(13)V99 VALUE 0.
 PROCEDURE DIVISION.
 Main-Entry.
     MOVE "TKITCST kit cost (spec 09 s8)" TO TK-SUITE
     PERFORM TK-BEGIN
     PERFORM Test-Engine-Refuses-Nonsense
     PERFORM Probe-The-Database
     IF Ws-Db-Up = "N" OR Ws-Tables = "N"
         PERFORM Skip-The-Database-Tests
     ELSE
         PERFORM Begin-Transaction
         PERFORM Build-The-Kit
         PERFORM Test-Seven-Values-Reconcile
         PERFORM Test-Scrap-Is-Not-Deducted
         PERFORM Test-Subassembly-Charged-Once
         PERFORM Test-Double-Count-Is-Reported
         PERFORM Test-Element-Breakdown
         PERFORM Rollback-Transaction
     END-IF
     PERFORM TK-END
     STOP RUN.

*>----------------------------------------------------------------
*> No database is needed: KITCOST names its own function before it
*> looks at anything, and it answers nothing when it refuses.
 Test-Engine-Refuses-Nonsense.
     PERFORM Reset-Case
     MOVE "WIBBLE" TO KA-FUNCTION
     CALL "KITCOST" USING Ws-Cost-Ca
     MOVE "KITCOST refuses an unknown function with KIT0002"
          TO TK-NAME
     MOVE "KIT0002" TO TK-EXPECTED-X
     MOVE KA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A refused cost enquiry answers no residual" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KK-RESIDUAL-GBP TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Reset-Case
     MOVE "COST" TO KA-FUNCTION
     MOVE SPACES TO KK-KIT-REF
     CALL "KITCOST" USING Ws-Cost-Ca
     MOVE "KITCOST refuses a cost enquiry with no kit reference"
          TO TK-NAME
     MOVE 8 TO TK-EXPECTED-N
     MOVE KA-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

 Reset-Case.
     INITIALIZE Ws-Cost-Ca
     MOVE "STC" TO KA-COMPANY
     MOVE "RDG" TO KA-SITE
     MOVE "COSTCLRK" TO KA-USER
     MOVE 20261006 TO KA-BUSINESS-DATE
     MOVE Ws-Kit-Ref TO KK-KIT-REF
     MOVE "GBP" TO KK-CURRENCY.

 Ask-For-The-Cost.
     PERFORM Reset-Case
     MOVE "COST" TO KA-FUNCTION
     CALL "KITCOST" USING Ws-Cost-Ca.

*>----------------------------------------------------------------
*> AC-081 and KIT-020.  Issued 100.00, returned 20.00, absorbed
*> 40.00 (labour 30.00 plus overhead 10.00) and completed 60.00
*> leave 60.00 still in work in progress.
 Test-Seven-Values-Reconcile.
     PERFORM Ask-For-The-Cost
     MOVE "KITCOST answers a built kit" TO TK-NAME
     IF KA-RETURN-CODE > 4
         MOVE KA-MESSAGE (1:100) TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         PERFORM TK-PASS
     END-IF
     MOVE "Planned value is the approved requirement at plan cost"
          TO TK-NAME
     MOVE 100.00 TO TK-EXPECTED-N
     MOVE KK-PLANNED-GBP TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Issued is 100.00 - material plus the subassembly"
          TO TK-NAME
     MOVE 100.00 TO TK-EXPECTED-N
     MOVE KK-ISSUED-GBP TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Returned is 20.00" TO TK-NAME
     MOVE 20.00 TO TK-EXPECTED-N
     MOVE KK-RETURNED-GBP TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Scrapped is 15.00" TO TK-NAME
     MOVE 15.00 TO TK-EXPECTED-N
     MOVE KK-SCRAPPED-GBP TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Absorbed is 40.00 - labour 30.00 and overhead 10.00"
          TO TK-NAME
     MOVE 40.00 TO TK-EXPECTED-N
     MOVE KK-ABSORBED-GBP TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Completed is the 60.00 actually transferred out"
          TO TK-NAME
     MOVE 60.00 TO TK-EXPECTED-N
     MOVE KK-COMPLETED-GBP TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Residual is 60.00" TO TK-NAME
     MOVE 60.00 TO TK-EXPECTED-N
     MOVE KK-RESIDUAL-GBP TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     COMPUTE Ws-Identity = KK-ISSUED-GBP - KK-RETURNED-GBP
                         + KK-ABSORBED-GBP - KK-COMPLETED-GBP
                         - KK-RESIDUAL-GBP
     MOVE "The seven values satisfy the residual identity"
          TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE Ws-Identity TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "KITCOST says the picture reconciles" TO TK-NAME
     MOVE "Y" TO TK-EXPECTED-X
     MOVE KK-RECONCILED TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> KIT-014.  Scrap is a disposition, not a reversal.  Had the
*> 15.00 of scrap been taken out of issued the residual would read
*> 45.00, which would say the job owns material it does not have.
 Test-Scrap-Is-Not-Deducted.
     MOVE "Scrap is reported but is not deducted from issued"
          TO TK-NAME
     COMPUTE Ws-Identity = KK-ISSUED-GBP - KK-SCRAPPED-GBP
     MOVE 85.00 TO TK-EXPECTED-N
     MOVE Ws-Identity TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Residual is 60.00, not the 45.00 a reversal would give"
          TO TK-NAME
     MOVE "60.00" TO TK-EXPECTED-X
     MOVE SPACES TO TK-ACTUAL-X
     IF KK-RESIDUAL-GBP = 60.00
         MOVE "60.00" TO TK-ACTUAL-X
     ELSE
         MOVE "deducted" TO TK-ACTUAL-X
     END-IF
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> AC-081.  One child kit makes the subassembly.  The parent is
*> charged 40.00 - the value of the subassembly - and nothing of
*> the child's own material, so the check passes.
 Test-Subassembly-Charged-Once.
     MOVE "One made subassembly is recognised on the parent"
          TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE KK-SUBASSY-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The subassembly is charged at 40.00, its own value"
          TO TK-NAME
     MOVE 40.00 TO TK-EXPECTED-N
     MOVE KK-SUBASSY-GBP TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "AC-081 the subassembly is charged once" TO TK-NAME
     MOVE "Y" TO TK-EXPECTED-X
     MOVE KK-DOUBLE-CHECK TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> AC-081 the other way round.  Charge one of the CHILD kit's own
*> components to the parent as well and the subassembly is counted
*> twice.  KITCOST must refuse to stay quiet about it.
 Test-Double-Count-Is-Reported.
     PERFORM Charge-Child-Material-To-Parent
     IF Ws-Sql-Bad = "Y"
         MOVE "AC-081 a double charged child component is reported"
              TO TK-NAME
         MOVE Ws-Sql-Msg TO TK-DETAIL
         PERFORM TK-FAIL
         EXIT PARAGRAPH
     END-IF
     PERFORM Ask-For-The-Cost
     MOVE "AC-081 a double charged child component is reported"
          TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE KK-DOUBLE-CHECK TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A double count warns with KIT0027" TO TK-NAME
     MOVE "KIT0027" TO TK-EXPECTED-X
     MOVE KA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A double count is a warning, not a failure" TO TK-NAME
     MOVE 4 TO TK-EXPECTED-N
     MOVE KA-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> GETELEM.  Eight elements are defined but only the ones that
*> carry a number are published, so a screen never shows an empty
*> row: material, subassembly, return, scrap, labour, overhead and
*> completion are used here and subcontract is not.
 Test-Element-Breakdown.
     PERFORM Reset-Case
     MOVE "GETELEM" TO KA-FUNCTION
     CALL "KITCOST" USING Ws-Cost-Ca
     MOVE "GETELEM publishes only the elements that carry a value"
          TO TK-NAME
     MOVE 7 TO TK-EXPECTED-N
     MOVE KK-ELEM-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Element 1 is bought out material" TO TK-NAME
     MOVE "MATERIAL" TO TK-EXPECTED-X
     MOVE KK-E-ELEMENT (1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "Element 4 keeps the scrap apart from the material"
          TO TK-NAME
     MOVE "SCRAP" TO TK-EXPECTED-X
     MOVE KK-E-ELEMENT (4) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The scrap element carries the 15.00" TO TK-NAME
     MOVE 15.00 TO TK-EXPECTED-N
     MOVE KK-E-ACTUAL (4) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The subassembly element carries the 40.00" TO TK-NAME
     MOVE 40.00 TO TK-EXPECTED-N
     MOVE KK-E-ACTUAL (2) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>================================================================
*> Database plumbing.  The same shape as TKITEXEC: probe, begin,
*> build, roll back.
*>================================================================
 Probe-The-Database.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM sys.objects"
            " o JOIN sys.schemas s ON s.schema_id = o.schema_id"
            " WHERE s.name = 'kit' AND o.name IN"
            " ('ExecutionKit','ExecutionKitLine','KitIssue')"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     IF Ws-Sql-Bad = "Y"
         MOVE "N" TO Ws-Db-Up
         MOVE "N" TO Ws-Sql-Bad
         EXIT PARAGRAPH
     END-IF
     IF FUNCTION NUMVAL (Ws-Probe) < 3
         MOVE "N" TO Ws-Tables
     END-IF.

 Skip-The-Database-Tests.
     MOVE "Kit cost scenario (issue, return, scrap, absorb, complete)"
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
     MOVE 7 TO SQLIO-CURSOR
     CALL "SQLIO" USING SQLIO-CA, Ws-Probe.

 Rollback-Transaction.
     MOVE "ROLB" TO SQLIO-FUNC
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 7 TO SQLIO-CURSOR
     CALL "SQLIO" USING SQLIO-CA, Ws-Probe.

 Run-Statement.
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 7 TO SQLIO-CURSOR
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
     MOVE 60 TO SQLIO-COL-WIDTH (1)
     MOVE 7 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     MOVE SPACES TO Ws-Probe
     CALL "SQLIO" USING SQLIO-CA, Ws-Probe
     IF SQLIO-ERROR
         MOVE "Y" TO Ws-Sql-Bad
         MOVE SQLIO-ERRMSG (1:200) TO Ws-Sql-Msg
     END-IF.

*>   Four real items are borrowed from inv.Item so the foreign
*>   keys and the unit of measure are genuine.  i0 is the parent,
*>   i1 the made subassembly, i2 bought out material and i3 a
*>   component that belongs to the CHILD kit only.
 Build-The-Kit.
     PERFORM Clear-Previous-Rows
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.ExecutionKit (CompanyCode, SiteCode,"
            " KitRef, ParentItemCode, PlannedGoodQty,"
            " CompletedGoodQty, RunCount, NeedDate, Status,"
            " OwnerUser, WipValueGBP, IsBackflush, CreatedBy)"
            " SELECT 'STC','RDG','TKITCST-P1', i.ItemCode, 10, 0,"
            " 1, CONVERT(date,'20261106',112),'RELEASED',"
            " 'COSTCLRK', 0, 0, 'TESTER' FROM (SELECT ItemCode"
            " FROM inv.Item WHERE CompanyCode = 'STC'"
            " ORDER BY ItemCode OFFSET 0 ROWS FETCH NEXT 1 ROWS"
            " ONLY) i"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.ExecutionKit (CompanyCode, SiteCode,"
            " KitRef, ParentKitRef, ParentItemCode,"
            " PlannedGoodQty, CompletedGoodQty, RunCount,"
            " NeedDate, Status, OwnerUser, WipValueGBP,"
            " IsBackflush, CreatedBy)"
            " SELECT 'STC','RDG','TKITCST-C1','TKITCST-P1',"
            " i.ItemCode, 10, 10, 1,"
            " CONVERT(date,'20261020',112),'FINCLOSED','COSTCLRK',"
            " 0, 0, 'TESTER' FROM (SELECT ItemCode FROM inv.Item"
            " WHERE CompanyCode = 'STC' ORDER BY ItemCode"
            " OFFSET 1 ROWS FETCH NEXT 1 ROWS ONLY) i"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     PERFORM Add-Parent-Lines
     PERFORM Add-Child-Line
     PERFORM Add-Movements
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM"
            " kit.KitIssue WHERE CompanyCode = 'STC' AND"
            " KitRef = 'TKITCST-P1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "The cost scenario was built" TO TK-NAME
     IF Ws-Sql-Bad = "Y"
         MOVE Ws-Sql-Msg TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         MOVE 7 TO TK-EXPECTED-N
         COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
         PERFORM TK-ASSERT-NUM
     END-IF.

 Clear-Previous-Rows.
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE FROM kit.KitIssue WHERE CompanyCode = 'STC'"
            " AND KitRef LIKE 'TKITCST-%'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE l FROM kit.ExecutionKitLine l JOIN"
            " kit.ExecutionKit k ON k.ExecutionKitId ="
            " l.ExecutionKitId WHERE k.CompanyCode = 'STC' AND"
            " k.KitRef LIKE 'TKITCST-%'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE FROM kit.ExecutionKit WHERE CompanyCode ="
            " 'STC' AND KitRef LIKE 'TKITCST-%'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement.

*>   Line 1 is the made subassembly: ten at 4.00 is 40.00.
*>   Line 2 is bought out material: forty at 1.50 is 60.00.
*>   Planned is therefore 100.00.
 Add-Parent-Lines.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.ExecutionKitLine (ExecutionKitId, LineNum,"
            " PathRef, ComponentItemCode, UsageBasis,"
            " OriginalRequirementQty, CurrentRequirementQty,"
            " UomCode, ConversionFactor, NeedDate,"
            " PlannedUnitCostGBP, Status)"
            " SELECT k.ExecutionKitId, 1, '1', i.ItemCode,"
            " 'PROPORTION', 10, 10, i.StockUomCode, 1,"
            " CONVERT(date,'20261106',112), 4.00, 'OPEN'"
            " FROM kit.ExecutionKit k CROSS JOIN (SELECT ItemCode,"
            " StockUomCode FROM inv.Item WHERE CompanyCode = 'STC'"
            " ORDER BY ItemCode OFFSET 1 ROWS FETCH NEXT 1 ROWS"
            " ONLY) i WHERE k.CompanyCode = 'STC' AND"
            " k.KitRef = 'TKITCST-P1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.ExecutionKitLine (ExecutionKitId, LineNum,"
            " PathRef, ComponentItemCode, UsageBasis,"
            " OriginalRequirementQty, CurrentRequirementQty,"
            " UomCode, ConversionFactor, NeedDate,"
            " PlannedUnitCostGBP, Status)"
            " SELECT k.ExecutionKitId, 2, '2', i.ItemCode,"
            " 'PROPORTION', 40, 40, i.StockUomCode, 1,"
            " CONVERT(date,'20261106',112), 1.50, 'OPEN'"
            " FROM kit.ExecutionKit k CROSS JOIN (SELECT ItemCode,"
            " StockUomCode FROM inv.Item WHERE CompanyCode = 'STC'"
            " ORDER BY ItemCode OFFSET 2 ROWS FETCH NEXT 1 ROWS"
            " ONLY) i WHERE k.CompanyCode = 'STC' AND"
            " k.KitRef = 'TKITCST-P1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement.

*>   The child kit consumes a component of its own.  That material
*>   belongs to the child and must never appear on the parent.
 Add-Child-Line.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.ExecutionKitLine (ExecutionKitId, LineNum,"
            " PathRef, ComponentItemCode, UsageBasis,"
            " OriginalRequirementQty, CurrentRequirementQty,"
            " UomCode, ConversionFactor, NeedDate,"
            " PlannedUnitCostGBP, Status)"
            " SELECT k.ExecutionKitId, 1, '1', i.ItemCode,"
            " 'PROPORTION', 20, 20, i.StockUomCode, 1,"
            " CONVERT(date,'20261020',112), 0.25, 'CLOSED'"
            " FROM kit.ExecutionKit k CROSS JOIN (SELECT ItemCode,"
            " StockUomCode FROM inv.Item WHERE CompanyCode = 'STC'"
            " ORDER BY ItemCode OFFSET 3 ROWS FETCH NEXT 1 ROWS"
            " ONLY) i WHERE k.CompanyCode = 'STC' AND"
            " k.KitRef = 'TKITCST-C1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement.

 Add-Movements.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.KitIssue (CompanyCode, KitRef, LineNum,"
            " ComponentItemCode, IssueType, Quantity, UomCode,"
            " StockQty, ActualDate, PostingDate, OperatorUser,"
            " UnitCostGBP, ValueGBP, CreatedBy)"
            " SELECT 'STC','TKITCST-P1', l.LineNum,"
            " l.ComponentItemCode, v.IssueType, v.Qty,"
            " l.UomCode, v.Qty,"
            " CONVERT(date,'20261010',112),"
            " CONVERT(date,'20261010',112), 'COSTCLRK',"
            " CASE WHEN v.Qty = 0 THEN 0 ELSE v.Val / v.Qty END,"
            " v.Val, 'TESTER'"
            " FROM kit.ExecutionKit k"
            " JOIN kit.ExecutionKitLine l"
            "   ON l.ExecutionKitId = k.ExecutionKitId"
            " JOIN (VALUES"
            " (1,'ISSUE',10,40.00),(2,'ISSUE',40,60.00),"
            " (2,'RETURN',10,20.00),(2,'SCRAP',8,15.00),"
            " (2,'LABOUR',0,30.00),(2,'OVERHEAD',0,10.00))"
            " v(Ln,IssueType,Qty,Val) ON v.Ln = l.LineNum"
            " WHERE k.CompanyCode = 'STC' AND"
            " k.KitRef = 'TKITCST-P1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.KitIssue (CompanyCode, KitRef,"
            " ComponentItemCode, IssueType, Quantity, UomCode,"
            " StockQty, ActualDate, PostingDate, OperatorUser,"
            " UnitCostGBP, ValueGBP, CreatedBy)"
            " SELECT 'STC','TKITCST-P1', k.ParentItemCode,"
            " 'COMPLETE', 6, 'EA', 6,"
            " CONVERT(date,'20261012',112),"
            " CONVERT(date,'20261012',112), 'COSTCLRK', 10.00,"
            " 60.00, 'TESTER' FROM kit.ExecutionKit k"
            " WHERE k.CompanyCode = 'STC' AND"
            " k.KitRef = 'TKITCST-P1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement.

*>   The mistake AC-081 is about: the child kit's own component
*>   charged to the parent as well as to the child.
 Charge-Child-Material-To-Parent.
     MOVE "N" TO Ws-Sql-Bad
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.KitIssue (CompanyCode, KitRef,"
            " ComponentItemCode, IssueType, Quantity, UomCode,"
            " StockQty, ActualDate, PostingDate, OperatorUser,"
            " UnitCostGBP, ValueGBP, CreatedBy)"
            " SELECT 'STC','TKITCST-P1', cl.ComponentItemCode,"
            " 'ISSUE', 20, cl.UomCode, 20,"
            " CONVERT(date,'20261011',112),"
            " CONVERT(date,'20261011',112), 'COSTCLRK', 0.25,"
            " 5.00, 'TESTER' FROM kit.ExecutionKit ck"
            " JOIN kit.ExecutionKitLine cl"
            "   ON cl.ExecutionKitId = ck.ExecutionKitId"
            " WHERE ck.CompanyCode = 'STC' AND"
            " ck.KitRef = 'TKITCST-C1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement.

 COPY TESTKITP.
