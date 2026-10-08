>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : TKITCLS
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION (TESTS)
*> PURPOSE    : Operational close, cancellation and reopening
*>              (KIT-017, KIT-018, AC-082).
*>
*>              The scenario is the awkward one from AC-082: a kit
*>              that has had material issued to it and is then
*>              cancelled.  The material does not go back to
*>              stores because somebody cancelled a piece of
*>              paper - it stays on the job until a real return
*>              movement happens - and reopening the kit needs
*>              authority and an open period.
*>
*>              Everything runs inside a transaction that is
*>              rolled back, so the suite is repeatable.
*> REQ: KIT-017, KIT-018, KIT-024
*> AC : AC-082
*>================================================================
 IDENTIFICATION DIVISION.
 PROGRAM-ID. TKITCLS.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY TESTKITW.
 COPY SQLIOCA.
 COPY KITCLOSECA REPLACING ==DFHCOMMAREA== BY ==Ws-Close-Ca==
      LEADING ==CA-== BY ==CL-==.
 01  Ws-Probe                    PIC X(30).
 01  Ws-Work.
     05  Ws-Db-Up                PIC X VALUE "Y".
     05  Ws-Tables               PIC X VALUE "Y".
     05  Ws-Sql-Bad              PIC X VALUE "N".
     05  Ws-Sql-Msg              PIC X(200) VALUE SPACES.
     05  Ws-Kit-Ref              PIC X(30) VALUE "TKITCLS-T1".
 PROCEDURE DIVISION.
 Main-Entry.
     MOVE "TKITCLS kit close and reopen" TO TK-SUITE
     PERFORM TK-BEGIN
     PERFORM Test-Close-Refuses-Nonsense
     PERFORM Probe-The-Database
     IF Ws-Db-Up = "N" OR Ws-Tables = "N"
         PERFORM Skip-The-Database-Tests
     ELSE
         PERFORM Begin-Transaction
         PERFORM Build-The-Kit
         PERFORM Test-Close-Check-States-The-Truth
         PERFORM Test-Cancel-Needs-A-Reason
         PERFORM Test-Cancel-Leaves-Material-On-The-Job
         PERFORM Test-Cancel-Unissued-Kit
         PERFORM Test-Reopen-Needs-Authority
         PERFORM Test-History-Is-Readable
         PERFORM Rollback-Transaction
     END-IF
     PERFORM TK-END
     STOP RUN.

 Test-Close-Refuses-Nonsense.
     PERFORM Prime-Close
     MOVE "WIBBLE" TO CL-FUNCTION
     CALL "KITCLOSE" USING Ws-Close-Ca
     MOVE "KITCLOSE refuses an unknown function with KIT0002"
          TO TK-NAME
     MOVE "KIT0002" TO TK-EXPECTED-X
     MOVE CL-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A refused close changes no status" TO TK-NAME
     MOVE SPACES TO TK-EXPECTED-X
     MOVE KZ-NEW-STATUS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

 Prime-Close.
     INITIALIZE Ws-Close-Ca
     MOVE "STC" TO CL-COMPANY
     MOVE "RDG" TO CL-SITE
     MOVE "TESTER" TO CL-USER
     MOVE 20261006 TO CL-BUSINESS-DATE
     MOVE Ws-Kit-Ref TO KZ-KIT-REF.

*>----------------------------------------------------------------
 Probe-The-Database.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM sys.objects"
            " o JOIN sys.schemas s ON s.schema_id = o.schema_id"
            " WHERE s.name = 'kit' AND o.name IN"
            " ('ExecutionKit','ExecutionKitLine','KitIssue')"
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
     IF FUNCTION NUMVAL (Ws-Probe) < 3
         MOVE "N" TO Ws-Tables
     END-IF.

 Skip-The-Database-Tests.
     MOVE "Cancel and reopen a partly issued kit" TO TK-NAME
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
     CALL "SQLIO" USING SQLIO-CA, Ws-Probe.

*>   A kit for ten frames that has had seven of its forty pins
*>   issued to it and two of them returned: five are on the job.
 Build-The-Kit.
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE i FROM kit.KitIssue i WHERE i.CompanyCode ="
            " 'STC' AND i.KitRef LIKE 'TKITCLS-T%'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE l FROM kit.ExecutionKitLine l JOIN"
            " kit.ExecutionKit k ON k.ExecutionKitId ="
            " l.ExecutionKitId WHERE k.CompanyCode = 'STC' AND"
            " k.KitRef LIKE 'TKITCLS-T%'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE FROM kit.ExecutionKit WHERE CompanyCode ="
            " 'STC' AND KitRef LIKE 'TKITCLS-T%'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.ExecutionKit (CompanyCode, SiteCode,"
            " KitRef, ParentItemCode,"
            " PlannedGoodQty, RunCount, NeedDate, Status,"
            " OwnerUser, WipValueGBP, IsBackflush, CreatedBy)"
            " SELECT 'STC','RDG','TKITCLS-T1', i.ItemCode, 10, 1,"
            " CONVERT(date,'20261106',112),'PARTISSUED','TESTER',"
            " 62.50,0,'TESTER' FROM (SELECT ItemCode FROM inv.Item"
            " WHERE CompanyCode = 'STC' ORDER BY ItemCode OFFSET 0"
            " ROWS FETCH NEXT 1 ROWS ONLY) i"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.ExecutionKitLine (ExecutionKitId,"
            " LineNum, PathRef, ComponentItemCode, UsageBasis,"
            " OriginalRequirementQty, CurrentRequirementQty,"
            " UomCode, ConversionFactor, GrossIssuedQty,"
            " ReturnedQty, NetIssuedQty, RemainingQty,"
            " IssuedValueGBP, NeedDate, Status)"
            " SELECT k.ExecutionKitId, 1, '1', i.ItemCode,"
            " 'PROPORTION', 40, 40, i.StockUomCode, 1, 7, 2, 5,"
            " 35, 62.50, CONVERT(date,'20261106',112),"
            " 'PARTISSUED' FROM kit.ExecutionKit k CROSS JOIN"
            " (SELECT ItemCode, StockUomCode FROM inv.Item WHERE"
            " CompanyCode = 'STC' ORDER BY ItemCode OFFSET 1 ROWS"
            " FETCH NEXT 1 ROWS ONLY) i WHERE k.CompanyCode ="
            " 'STC' AND k.KitRef = 'TKITCLS-T1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.KitIssue (CompanyCode, KitRef, KitLineId,"
            " LineNum, ComponentItemCode, IssueType, Quantity,"
            " UomCode, StockQty, ActualDate, PostingDate,"
            " OperatorUser, UnitCostGBP, ValueGBP, CreatedBy)"
            " SELECT 'STC','TKITCLS-T1', l.KitLineId, l.LineNum,"
            " l.ComponentItemCode, 'ISSUE', 7, l.UomCode, 7,"
            " CONVERT(date,'20261002',112),"
            " CONVERT(date,'20261002',112),'TESTER',12.50,87.50,"
            " 'TESTER' FROM kit.ExecutionKitLine l JOIN"
            " kit.ExecutionKit k ON k.ExecutionKitId ="
            " l.ExecutionKitId WHERE k.CompanyCode = 'STC' AND"
            " k.KitRef = 'TKITCLS-T1' AND l.LineNum = 1"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM"
            " kit.ExecutionKitLine l JOIN kit.ExecutionKit k ON"
            " k.ExecutionKitId = l.ExecutionKitId WHERE"
            " k.CompanyCode = 'STC' AND k.KitRef = 'TKITCLS-T1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "The partly issued kit was built" TO TK-NAME
     IF Ws-Sql-Bad = "Y"
         MOVE Ws-Sql-Msg TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         MOVE 1 TO TK-EXPECTED-N
         COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
         PERFORM TK-ASSERT-NUM
     END-IF
*>   T2 is the same kit already cancelled, so that reopening can be
*>   tested without first having to cancel anything, and T3 is a
*>   released kit that nothing has been issued to - the first half
*>   of AC-082.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.ExecutionKit (CompanyCode, SiteCode,"
            " KitRef, ParentItemCode, PlannedGoodQty, RunCount,"
            " NeedDate, Status, OwnerUser, IsBackflush, CreatedBy)"
            " SELECT 'STC','RDG','TKITCLS-T2', i.ItemCode, 10, 1,"
            " CONVERT(date,'20261106',112),'CANCELLED','TESTER',"
            " 0,'TESTER' FROM (SELECT ItemCode FROM inv.Item"
            " WHERE CompanyCode = 'STC' ORDER BY ItemCode OFFSET 0"
            " ROWS FETCH NEXT 1 ROWS ONLY) i"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.ExecutionKit (CompanyCode, SiteCode,"
            " KitRef, ParentItemCode, PlannedGoodQty, RunCount,"
            " NeedDate, Status, OwnerUser, IsBackflush, CreatedBy)"
            " SELECT 'STC','RDG','TKITCLS-T3', i.ItemCode, 10, 1,"
            " CONVERT(date,'20261106',112),'RELEASED','TESTER',"
            " 0,'TESTER' FROM (SELECT ItemCode FROM inv.Item"
            " WHERE CompanyCode = 'STC' ORDER BY ItemCode OFFSET 0"
            " ROWS FETCH NEXT 1 ROWS ONLY) i"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.ExecutionKitLine (ExecutionKitId,"
            " LineNum, PathRef, ComponentItemCode, UsageBasis,"
            " OriginalRequirementQty, CurrentRequirementQty,"
            " UomCode, ConversionFactor, RemainingQty, NeedDate,"
            " Status) SELECT k.ExecutionKitId, 1, '1', i.ItemCode,"
            " 'PROPORTION', 40, 40, i.StockUomCode, 1, 40,"
            " CONVERT(date,'20261106',112), 'OPEN'"
            " FROM kit.ExecutionKit k CROSS JOIN (SELECT ItemCode,"
            " StockUomCode FROM inv.Item WHERE CompanyCode = 'STC'"
            " ORDER BY ItemCode OFFSET 1 ROWS FETCH NEXT 1 ROWS"
            " ONLY) i WHERE k.CompanyCode = 'STC' AND"
            " k.KitRef = 'TKITCLS-T3'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement.

*>----------------------------------------------------------------
*> KIT-017.  CLOSECK resolves nothing; it states what is still
*> outstanding so that the screen and the action cannot disagree.
 Test-Close-Check-States-The-Truth.
     PERFORM Prime-Close
     MOVE "CLOSECK" TO CL-FUNCTION
     CALL "KITCLOSE" USING Ws-Close-Ca
     MOVE "One line still has material on the job" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE KZ-ON-JOB-LINES TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Seven issued less two returned leaves five on the job"
          TO TK-NAME
     MOVE 5 TO TK-EXPECTED-N
     MOVE KZ-ON-JOB-QTY TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "All ten frames are still uncompleted" TO TK-NAME
     MOVE 10 TO TK-EXPECTED-N
     MOVE KZ-UNCOMPLETED-QTY TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The residual work in progress is stated" TO TK-NAME
     MOVE 62.50 TO TK-EXPECTED-N
     MOVE KZ-WIP-RESIDUAL TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A kit with material on it is not ready to close"
          TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE KZ-READY TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A readiness check changes no status" TO TK-NAME
     MOVE "PARTISSUED" TO TK-EXPECTED-X
     MOVE KZ-NEW-STATUS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

 Test-Cancel-Needs-A-Reason.
     PERFORM Prime-Close
     MOVE "CANCEL" TO CL-FUNCTION
     CALL "KITCLOSE" USING Ws-Close-Ca
     MOVE "A cancellation without a reason is refused" TO TK-NAME
     MOVE "KIT0008" TO TK-EXPECTED-X
     MOVE CL-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), Status) FROM"
            " kit.ExecutionKit WHERE CompanyCode = 'STC' AND"
            " KitRef = 'TKITCLS-T1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "A refused cancellation leaves the kit alone" TO TK-NAME
     MOVE "PARTISSUED" TO TK-EXPECTED-X
     MOVE Ws-Probe TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>   AC-082.  Cancelling a partly issued kit does not put the
*>   material back into stores.  Five pins are still on the job
*>   afterwards and only a KITRET movement can change that.
 Test-Cancel-Leaves-Material-On-The-Job.
     PERFORM Prime-Close
     MOVE "KITCANCEL" TO KZ-REASON-CODE
     MOVE "Customer cancelled the order; the frames are not needed."
          TO KZ-REASON-TEXT
     MOVE "AUTH-4471" TO KZ-AUTHORITY-REF
     MOVE "RETURN" TO KZ-DISPOSITION
     MOVE "CANCEL" TO CL-FUNCTION
     CALL "KITCLOSE" USING Ws-Close-Ca
     MOVE "Cancelling a kit with material on it needs authority"
          TO TK-NAME
     IF CL-RETURN-CODE NOT = 0
         PERFORM TK-PASS
     ELSE
         MOVE "a partly issued kit was cancelled by anybody"
              TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     MOVE "The cancellation still reports the material on the job"
          TO TK-NAME
     MOVE 5 TO TK-EXPECTED-N
     MOVE KZ-ON-JOB-QTY TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), NetIssuedQty) FROM"
            " kit.ExecutionKitLine l JOIN kit.ExecutionKit k ON"
            " k.ExecutionKitId = l.ExecutionKitId WHERE"
            " k.CompanyCode = 'STC' AND k.KitRef = 'TKITCLS-T1'"
            " AND l.LineNum = 1"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "Cancelling returns nothing to stores by itself"
          TO TK-NAME
     MOVE 5 TO TK-EXPECTED-N
     COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
     PERFORM TK-ASSERT-NUM
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM"
            " kit.KitIssue WHERE CompanyCode = 'STC' AND KitRef ="
            " 'TKITCLS-T1' AND IssueType = 'RETURN'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "No return movement is invented by the cancellation"
          TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
     PERFORM TK-ASSERT-NUM
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), Status) FROM"
            " kit.ExecutionKit WHERE CompanyCode = 'STC' AND"
            " KitRef = 'TKITCLS-T1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "The kit stays as it was until somebody may act"
          TO TK-NAME
     MOVE "PARTISSUED" TO TK-EXPECTED-X
     MOVE Ws-Probe TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>   AC-082, first half.  A kit that nothing was issued to can be
*>   cancelled outright: there is no material to account for, so
*>   only the reason is required.
 Test-Cancel-Unissued-Kit.
     PERFORM Prime-Close
     MOVE "TKITCLS-T3" TO KZ-KIT-REF
     MOVE "KITCANCEL" TO KZ-REASON-CODE
     MOVE "The order was cancelled before anything was picked."
          TO KZ-REASON-TEXT
     MOVE "CANCEL" TO CL-FUNCTION
     CALL "KITCLOSE" USING Ws-Close-Ca
     MOVE "An unissued kit is cancelled without special authority"
          TO TK-NAME
     IF CL-RETURN-CODE < 8
         PERFORM TK-PASS
     ELSE
         MOVE CL-MESSAGE (1:200) TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     MOVE "Nothing was on the job to account for" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KZ-ON-JOB-QTY TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), Status) FROM"
            " kit.ExecutionKit WHERE CompanyCode = 'STC' AND"
            " KitRef = 'TKITCLS-T3'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "The unissued kit really is cancelled" TO TK-NAME
     MOVE "CANCELLED" TO TK-EXPECTED-X
     MOVE Ws-Probe TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>   KIT-018.  Reopening is not an undo button.
 Test-Reopen-Needs-Authority.
     PERFORM Prime-Close
     MOVE "TKITCLS-T2" TO KZ-KIT-REF
     MOVE "REOPEN" TO CL-FUNCTION
     CALL "KITCLOSE" USING Ws-Close-Ca
     MOVE "Reopening without a reason is refused" TO TK-NAME
     MOVE "KIT0008" TO TK-EXPECTED-X
     MOVE CL-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Prime-Close
     MOVE "TKITCLS-T2" TO KZ-KIT-REF
     MOVE "KITREOPEN" TO KZ-REASON-CODE
     MOVE "The customer changed their mind again." TO KZ-REASON-TEXT
     MOVE "Y" TO KZ-FORCE
     MOVE "REOPEN" TO CL-FUNCTION
     CALL "KITCLOSE" USING Ws-Close-Ca
     MOVE "Reopening needs authority or an open period, and says so"
          TO TK-NAME
     IF CL-RETURN-CODE NOT = 0
         PERFORM TK-PASS
     ELSE
         MOVE "a kit was reopened with neither authority nor a period check"
              TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     MOVE "Force does not buy authority or an open period"
          TO TK-NAME
     IF CL-REASON-CODE = "KIT0009" OR CL-REASON-CODE = "KIT0016"
         PERFORM TK-PASS
     ELSE
         MOVE CL-MESSAGE (1:200) TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), Status) FROM"
            " kit.ExecutionKit WHERE CompanyCode = 'STC' AND"
            " KitRef = 'TKITCLS-T2'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "A refused reopen leaves the kit closed" TO TK-NAME
     MOVE "CANCELLED" TO TK-EXPECTED-X
     MOVE Ws-Probe TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM"
            " kit.KitIssue WHERE CompanyCode = 'STC' AND KitRef ="
            " 'TKITCLS-T1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "A refused reopen deletes no history" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
     PERFORM TK-ASSERT-NUM.

*>   KIT-024.  Every state change keeps its actor, time and
*>   reason, and HISTORY reads them back without changing a thing.
 Test-History-Is-Readable.
     PERFORM Prime-Close
     MOVE "HISTORY" TO CL-FUNCTION
     CALL "KITCLOSE" USING Ws-Close-Ca
     MOVE "The status history can be read back" TO TK-NAME
     IF CL-RETURN-CODE < 8
         PERFORM TK-PASS
     ELSE
         MOVE CL-MESSAGE (1:200) TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     MOVE "Reading the history changes no status" TO TK-NAME
     MOVE "PARTISSUED" TO TK-EXPECTED-X
     MOVE KZ-OLD-STATUS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

 COPY TESTKITP.
