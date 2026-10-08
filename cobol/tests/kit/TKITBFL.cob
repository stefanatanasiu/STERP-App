>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : TKITBFL
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION (TESTS)
*> PURPOSE    : The two things a backflush used to get wrong
*>              (CR2742), proved against the real engine:
*>
*>              1  one scope for every line.  KITISSUE BACKFLSH
*>                 took the site, warehouse and location from the
*>                 commarea and used them for every component, so
*>                 a kit whose components live in two bins could
*>                 not be flushed at all.  Each line must now be
*>                 sourced from its OWN eligible balance - the
*>                 stock KITRESV would have reserved - and the
*>                 caller's scope only narrows the search.
*>
*>              2  the arithmetic ignored what was already issued.
*>                 usage per unit times the good output was issued
*>                 in full even when the stores had issued part of
*>                 the line by hand, which over-issued the line.
*>                 A flush may only cover what the line still owes.
*>
*>              3  every flushed line is valued at the item's cost
*>                 at the site (CR2744) - it used to post at nil.
*>
*>              The suite builds its own kit, its own stock and its
*>              own two bins inside a transaction that is ALWAYS
*>              rolled back, so it may be run as often as anyone
*>              likes and leaves nothing behind.
*> REQ: KIT-010, KIT-011
*> AC : AC-079, AC-080
*>================================================================
 IDENTIFICATION DIVISION.
 PROGRAM-ID. TKITBFL.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY TESTKITW.
 COPY SQLIOCA.
 COPY KITISSUECA REPLACING ==DFHCOMMAREA== BY ==Ws-Issue-Ca==
      LEADING ==CA-== BY ==IS-==.
 01  Ws-Probe                    PIC X(80).
 01  Ws-Work.
     05  Ws-Db-Up                PIC X VALUE "Y".
     05  Ws-Tables               PIC X VALUE "Y".
     05  Ws-Sql-Bad              PIC X VALUE "N".
     05  Ws-Sql-Msg              PIC X(200) VALUE SPACES.
     05  Ws-Ready                PIC X VALUE "Y".
     05  Ws-Whse-1               PIC X(20) VALUE SPACES.
     05  Ws-Loc-1                PIC X(20) VALUE SPACES.
     05  Ws-Whse-2               PIC X(20) VALUE SPACES.
     05  Ws-Loc-2                PIC X(20) VALUE SPACES.
     05  Ws-Item-1               PIC X(40) VALUE SPACES.
     05  Ws-Item-2               PIC X(40) VALUE SPACES.
     05  Ws-Bal-Item             PIC X(40) VALUE SPACES.
     05  Ws-Bal-Whse             PIC X(20) VALUE SPACES.
     05  Ws-Bal-Loc              PIC X(20) VALUE SPACES.
     05  Ws-Stamp                PIC X(21) VALUE SPACES.
     05  Ws-Seq                  PIC 9(2) VALUE 0.
     05  Ws-Corr                 PIC X(36) VALUE SPACES.
     05  Ws-Line-No              PIC S9(4) VALUE 0.
     05  Ws-Line-Ed              PIC Z(3)9.
     05  Ws-Want                 PIC S9(13)V9(6) VALUE 0.
     05  Ws-Want-Ed              PIC -(13)9.9(6).
     05  Ws-Kit-Ref              PIC X(30) VALUE "TKITBFL-T1".
 PROCEDURE DIVISION.
 Main-Entry.
     MOVE "TKITBFL kit backflush" TO TK-SUITE
     PERFORM TK-BEGIN
     PERFORM Test-Engine-Refuses-Nonsense
     PERFORM Probe-The-Database
     IF Ws-Db-Up = "N" OR Ws-Tables = "N"
         PERFORM Skip-The-Database-Tests
     ELSE
         PERFORM Begin-Transaction
         PERFORM Find-Two-Bins
         PERFORM Find-Two-Items
         IF Ws-Ready = "N"
             PERFORM Skip-The-Database-Tests
         ELSE
             PERFORM Build-The-Kit
             PERFORM Test-Backflush-Crosses-Bins
             PERFORM Test-Backflush-Deducts-Issued
         END-IF
         PERFORM Rollback-Transaction
     END-IF
     PERFORM TK-END
     STOP RUN.

*>----------------------------------------------------------------
*> No database is needed for this one: a backflush names the kit
*> it is flushing before it looks at anything.
 Test-Engine-Refuses-Nonsense.
     INITIALIZE Ws-Issue-Ca
     MOVE "STC" TO IS-COMPANY
     MOVE "TESTER" TO IS-USER
     MOVE "FLUSHIT" TO IS-FUNCTION
     CALL "KITISSUE" USING Ws-Issue-Ca
     MOVE "An unknown issue function is refused, not guessed"
          TO TK-NAME
     MOVE "KIT0002" TO TK-EXPECTED-X
     MOVE IS-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
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
     MOVE "Backflush across two bins and after a manual issue"
          TO TK-NAME
     EVALUATE TRUE
         WHEN Ws-Db-Up = "N"
             MOVE "no database connection" TO TK-DETAIL
         WHEN Ws-Tables = "N"
             MOVE "kit tables are not deployed" TO TK-DETAIL
         WHEN OTHER
             MOVE "two available bins and two untraced stocked items are needed"
                  TO TK-DETAIL
     END-EVALUATE
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
     MOVE 80 TO SQLIO-COL-WIDTH (1)
     MOVE 8 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     MOVE SPACES TO Ws-Probe
     CALL "SQLIO" USING SQLIO-CA, Ws-Probe
     IF SQLIO-ERROR
         MOVE "Y" TO Ws-Sql-Bad
         MOVE SQLIO-ERRMSG (1:200) TO Ws-Sql-Msg
     END-IF
*>   The result set is given back before any kit program is called:
*>   one connection may not carry two live cursors at once.
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Probe.

*>   A number read back from the database, as an assertion.
 Assert-Scalar.
     IF Ws-Sql-Bad = "Y"
         MOVE Ws-Sql-Msg TO TK-DETAIL
         MOVE "N" TO Ws-Sql-Bad
         PERFORM TK-FAIL
     ELSE
         COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
         PERFORM TK-ASSERT-NUM
     END-IF.

*>----------------------------------------------------------------
*> Two bins that really are available for owned stock, and two
*> untraced stocked items.  The whole point of the fix is that the
*> two components do NOT share a bin.
 Find-Two-Bins.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(60), l.WarehouseCode + '|' +"
            " l.LocationCode) FROM inv.Location l WHERE"
            " l.CompanyCode = 'STC' AND l.SiteCode = 'RDG' AND"
            " l.IsAvailable = 1 AND l.IsOwnedStock = 1 AND"
            " l.LocationType = 'STOCK' ORDER BY l.WarehouseCode,"
            " l.LocationCode OFFSET 0 ROWS FETCH NEXT 1 ROWS ONLY"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     UNSTRING Ws-Probe DELIMITED BY "|" INTO Ws-Whse-1, Ws-Loc-1
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(60), l.WarehouseCode + '|' +"
            " l.LocationCode) FROM inv.Location l WHERE"
            " l.CompanyCode = 'STC' AND l.SiteCode = 'RDG' AND"
            " l.IsAvailable = 1 AND l.IsOwnedStock = 1 AND"
            " l.LocationType = 'STOCK' ORDER BY l.WarehouseCode,"
            " l.LocationCode OFFSET 1 ROWS FETCH NEXT 1 ROWS ONLY"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     UNSTRING Ws-Probe DELIMITED BY "|" INTO Ws-Whse-2, Ws-Loc-2
     IF Ws-Sql-Bad = "Y" OR Ws-Loc-1 = SPACES OR Ws-Loc-2 = SPACES
        OR Ws-Loc-1 = Ws-Loc-2
         MOVE "N" TO Ws-Ready
         MOVE "N" TO Ws-Sql-Bad
     END-IF.

 Find-Two-Items.
     IF Ws-Ready = "N"
         EXIT PARAGRAPH
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(60), i.ItemCode) FROM inv.Item"
            " i WHERE i.CompanyCode = 'STC' AND i.IsStocked = 1 AND"
            " i.TraceabilityType = 'NONE' AND EXISTS (SELECT 1 FROM"
            " inv.StockBalance b WHERE b.CompanyCode ="
            " i.CompanyCode AND b.ItemCode = i.ItemCode AND"
            " b.QualityStatus = 'ACCEPTED' AND b.QtyOnHand > 0)"
            " ORDER BY i.ItemCode OFFSET 0 ROWS FETCH NEXT 1 ROWS"
            " ONLY"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE Ws-Probe (1:40) TO Ws-Item-1
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(60), i.ItemCode) FROM inv.Item"
            " i WHERE i.CompanyCode = 'STC' AND i.IsStocked = 1 AND"
            " i.TraceabilityType = 'NONE' AND EXISTS (SELECT 1 FROM"
            " inv.StockBalance b WHERE b.CompanyCode ="
            " i.CompanyCode AND b.ItemCode = i.ItemCode AND"
            " b.QualityStatus = 'ACCEPTED' AND b.QtyOnHand > 0)"
            " ORDER BY i.ItemCode OFFSET 1 ROWS FETCH NEXT 1 ROWS"
            " ONLY"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE Ws-Probe (1:40) TO Ws-Item-2
     IF Ws-Sql-Bad = "Y" OR Ws-Item-1 = SPACES OR Ws-Item-2 = SPACES
        OR Ws-Item-1 = Ws-Item-2
         MOVE "N" TO Ws-Ready
         MOVE "N" TO Ws-Sql-Bad
     END-IF.

*>----------------------------------------------------------------
*> A released kit flagged for backflush: ten parents, two of the
*> first component each (twenty) and three of the second (thirty).
*> The components sit in different bins on purpose.
 Build-The-Kit.
     PERFORM Clear-The-Kit
     MOVE Ws-Item-1 TO Ws-Bal-Item
     MOVE Ws-Whse-1 TO Ws-Bal-Whse
     MOVE Ws-Loc-1 TO Ws-Bal-Loc
     PERFORM Stock-The-Bin
     MOVE Ws-Item-2 TO Ws-Bal-Item
     MOVE Ws-Whse-2 TO Ws-Bal-Whse
     MOVE Ws-Loc-2 TO Ws-Bal-Loc
     PERFORM Stock-The-Bin
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.ExecutionKit (CompanyCode, SiteCode,"
            " KitRef, ParentItemCode, PlannedGoodQty, RunCount,"
            " NeedDate, Status, OwnerUser, WipValueGBP,"
            " IsBackflush, CreatedBy) SELECT 'STC','RDG','"
            FUNCTION TRIM(Ws-Kit-Ref) "', i.ItemCode, 10, 1,"
            " CONVERT(date,'20261106',112),'RELEASED','TESTER',"
            " 0,1,'TESTER' FROM (SELECT TOP (1) ItemCode FROM"
            " inv.Item WHERE CompanyCode = 'STC' ORDER BY CASE"
            " WHEN TraceabilityType = 'NONE' AND IsStocked = 1"
            " THEN 0 ELSE 1 END, ItemCode) i"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE 1 TO Ws-Line-No
     MOVE Ws-Item-1 TO Ws-Bal-Item
     MOVE 20 TO Ws-Want
     PERFORM Add-A-Line
     MOVE 2 TO Ws-Line-No
     MOVE Ws-Item-2 TO Ws-Bal-Item
     MOVE 30 TO Ws-Want
     PERFORM Add-A-Line
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM"
            " kit.ExecutionKitLine l JOIN kit.ExecutionKit k ON"
            " k.ExecutionKitId = l.ExecutionKitId WHERE"
            " k.CompanyCode = 'STC' AND k.KitRef = '"
            FUNCTION TRIM(Ws-Kit-Ref) "'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "The backflush scenario kit was built" TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     PERFORM Assert-Scalar.

*>   Ws-Line-No is the line number and Ws-Want the requirement, so
*>   one paragraph can write either line.
 Add-A-Line.
     MOVE Ws-Line-No TO Ws-Line-Ed
     MOVE Ws-Want TO Ws-Want-Ed
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.ExecutionKitLine (ExecutionKitId, LineNum,"
            " PathRef, ComponentItemCode, UsageBasis,"
            " OriginalRequirementQty, CurrentRequirementQty,"
            " UomCode, ConversionFactor, NeedDate, Status)"
            " SELECT k.ExecutionKitId, " FUNCTION TRIM(Ws-Line-Ed)
            ", '" FUNCTION TRIM(Ws-Line-Ed) "', '"
            FUNCTION TRIM(Ws-Bal-Item) "', 'PROPORTION', "
            FUNCTION TRIM(Ws-Want-Ed) ", "
            FUNCTION TRIM(Ws-Want-Ed) ", i.StockUomCode, 1,"
            " CONVERT(date,'20261106',112), 'OPEN' FROM"
            " kit.ExecutionKit k JOIN inv.Item i ON i.CompanyCode"
            " = k.CompanyCode AND i.ItemCode = '"
            FUNCTION TRIM(Ws-Bal-Item) "' WHERE k.CompanyCode ="
            " 'STC' AND k.KitRef = '" FUNCTION TRIM(Ws-Kit-Ref) "'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement.

 Clear-The-Kit.
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE i FROM kit.KitIssue i WHERE i.CompanyCode ="
            " 'STC' AND i.KitRef = '" FUNCTION TRIM(Ws-Kit-Ref) "'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE l FROM kit.ExecutionKitLine l JOIN"
            " kit.ExecutionKit k ON k.ExecutionKitId ="
            " l.ExecutionKitId WHERE k.CompanyCode = 'STC' AND"
            " k.KitRef = '" FUNCTION TRIM(Ws-Kit-Ref) "'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE FROM kit.ExecutionKit WHERE CompanyCode ="
            " 'STC' AND KitRef = '" FUNCTION TRIM(Ws-Kit-Ref) "'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement.

*>   Five hundred of the component in that bin, accepted and owned,
*>   so the balance is eligible and the flush can never be short.
*>   An existing balance is topped up rather than replaced: the
*>   transaction is rolled back either way.
 Stock-The-Bin.
     MOVE SPACES TO SQLIO-STMT
     STRING "DECLARE @rev varchar(4) = ISNULL((SELECT TOP 1"
            " b.Revision FROM inv.StockBalance b WHERE"
            " b.CompanyCode = 'STC' AND b.ItemCode = '"
            FUNCTION TRIM(Ws-Bal-Item) "' AND b.QualityStatus ="
            " 'ACCEPTED' AND b.QtyOnHand > 0 ORDER BY b.QtyOnHand"
            " DESC), ''); UPDATE inv.StockBalance SET"
            " QtyOnHand = 500, QtyReserved = 0 WHERE CompanyCode"
            " = 'STC' AND SiteCode = 'RDG' AND WarehouseCode = '"
            FUNCTION TRIM(Ws-Bal-Whse) "' AND LocationCode = '"
            FUNCTION TRIM(Ws-Bal-Loc) "' AND ItemCode = '"
            FUNCTION TRIM(Ws-Bal-Item) "' AND Revision = @rev AND"
            " LotCode = '' AND SerialNo = '' AND OwnerType ="
            " 'OWNED' AND OwnerCode = '' AND QualityStatus ="
            " 'ACCEPTED'; IF @@ROWCOUNT = 0 INSERT"
            " inv.StockBalance (CompanyCode, SiteCode,"
            " WarehouseCode, LocationCode, ItemCode, Revision,"
            " LotCode, SerialNo, OwnerType, OwnerCode,"
            " QualityStatus, QtyOnHand, QtyReserved, QtyInTransit,"
            " LastMovementAt) VALUES ('STC','RDG','"
            FUNCTION TRIM(Ws-Bal-Whse) "','"
            FUNCTION TRIM(Ws-Bal-Loc) "','"
            FUNCTION TRIM(Ws-Bal-Item) "',@rev,'','','OWNED','',"
            "'ACCEPTED',500,0,0,SYSDATETIME())"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement.

*>----------------------------------------------------------------
*> CR2742 (1).  The commarea carries NO warehouse and NO location:
*> every line has to find its own stock.  Before the fix this
*> answered STK0002 (STKMOVE will not issue from nowhere), and
*> naming one bin answered a shortage on the other component.
*> REQ: KIT-010
 Test-Backflush-Crosses-Bins.
     PERFORM Prime-Issue
     MOVE "BACKFLSH" TO IS-FUNCTION
     MOVE 5 TO KS-GOOD-QTY
     MOVE "Backflush of five good parents." TO KS-REASON-TEXT
     CALL "KITISSUE" USING Ws-Issue-Ca
     MOVE "Backflush succeeds with no bin named by the caller"
          TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE IS-RETURN-CODE TO TK-ACTUAL-N
     MOVE IS-MESSAGE TO TK-DETAIL
     PERFORM TK-ASSERT-NUM
     MOVE "Both component lines were flushed" TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     MOVE KS-LINES-DONE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(DISTINCT"
            " i.LocationCode)) FROM kit.KitIssue i WHERE"
            " i.CompanyCode = 'STC' AND i.KitRef = '"
            FUNCTION TRIM(Ws-Kit-Ref) "' AND i.IssueType ="
            " 'BACKFLUSH'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "The two lines were issued from two different bins"
          TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     PERFORM Assert-Scalar
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM"
            " kit.KitIssue i JOIN inv.StockBalance b ON"
            " b.CompanyCode = i.CompanyCode AND b.ItemCode ="
            " i.ComponentItemCode AND b.WarehouseCode ="
            " i.WarehouseCode AND b.LocationCode = i.LocationCode"
            " AND b.QualityStatus = 'ACCEPTED' AND b.OwnerType ="
            " 'OWNED' WHERE i.CompanyCode = 'STC' AND i.KitRef ="
            " '" FUNCTION TRIM(Ws-Kit-Ref) "' AND i.IssueType ="
            " 'BACKFLUSH'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "Every flushed line came off an eligible balance of its own item"
          TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     PERFORM Assert-Scalar
*>   CR2744.  STKMOVE never derives a cost, so every kit issue used
*>   to be posted at nil.  Each flushed line must now carry a cost
*>   the item really holds at the site (or company-wide), and the
*>   stock movement behind it must carry the same value.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM"
            " kit.KitIssue i JOIN inv.StockMovement m ON"
            " m.MovementId = i.MovementId WHERE i.CompanyCode ="
            " 'STC' AND i.KitRef = '" FUNCTION TRIM(Ws-Kit-Ref)
            "' AND i.IssueType = 'BACKFLUSH' AND i.UnitCostGBP > 0"
            " AND m.UnitCost = i.UnitCostGBP AND m.ValueGBP < 0"
            " AND EXISTS (SELECT 1 FROM cst.ItemCost c WHERE"
            " c.CompanyCode = i.CompanyCode AND c.ItemCode ="
            " i.ComponentItemCode AND c.SiteCode IN ('RDG','*')"
            " AND i.UnitCostGBP IN (c.StandardUnitCost,"
            " c.AverageUnitCost))"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "Every flushed line is valued at the item's own cost"
          TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     PERFORM Assert-Scalar
     MOVE 1 TO Ws-Line-No
     MOVE "Line one was flushed with two per parent" TO TK-NAME
     MOVE 10 TO Ws-Want
     PERFORM Assert-Line-Gross
     MOVE 2 TO Ws-Line-No
     MOVE "Line two was flushed with three per parent" TO TK-NAME
     MOVE 15 TO Ws-Want
     PERFORM Assert-Line-Gross.

*>----------------------------------------------------------------
*> CR2742 (2).  The stores issue eight of line one by hand, so the
*> line has eighteen of its twenty.  A backflush for five more
*> parents wants ten - it may only take the two that are still
*> owed.  Before the fix it asked for the whole ten and the kit
*> was refused as an overissue (KIT-011), or worse, approved.
*> REQ: KIT-010, KIT-011
 Test-Backflush-Deducts-Issued.
     PERFORM Prime-Issue
     MOVE "ISSUE" TO IS-FUNCTION
     MOVE 1 TO KS-LINE-NUM
     MOVE Ws-Item-1 TO KS-ITEM-CODE
     MOVE Ws-Whse-1 TO KS-WAREHOUSE
     MOVE Ws-Loc-1 TO KS-LOCATION
     MOVE 8 TO KS-QUANTITY
     MOVE "Issued by hand at the bench." TO KS-REASON-TEXT
     CALL "KITISSUE" USING Ws-Issue-Ca
     MOVE "The stores may still issue a line by hand" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE IS-RETURN-CODE TO TK-ACTUAL-N
     MOVE IS-MESSAGE TO TK-DETAIL
     PERFORM TK-ASSERT-NUM
     PERFORM Prime-Issue
     MOVE "BACKFLSH" TO IS-FUNCTION
     MOVE 5 TO KS-GOOD-QTY
     MOVE "Backflush of the remaining five parents." TO KS-REASON-TEXT
     CALL "KITISSUE" USING Ws-Issue-Ca
     MOVE "Backflush after a manual issue is accepted" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE IS-RETURN-CODE TO TK-ACTUAL-N
     MOVE IS-MESSAGE TO TK-DETAIL
     PERFORM TK-ASSERT-NUM
     MOVE 1 TO Ws-Line-No
     MOVE "Line one stops at its requirement, not at usage times output"
          TO TK-NAME
     MOVE 20 TO Ws-Want
     PERFORM Assert-Line-Gross
     MOVE 2 TO Ws-Line-No
     MOVE "Line two was flushed for the whole output" TO TK-NAME
     MOVE 30 TO Ws-Want
     PERFORM Assert-Line-Gross
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM"
            " kit.ExecutionKitLine l JOIN kit.ExecutionKit k ON"
            " k.ExecutionKitId = l.ExecutionKitId WHERE"
            " k.CompanyCode = 'STC' AND k.KitRef = '"
            FUNCTION TRIM(Ws-Kit-Ref) "' AND l.GrossIssuedQty >"
            " l.CurrentRequirementQty"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "No line was over-issued by the backflush" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     PERFORM Assert-Scalar.

*>   Ws-Line-No names the line and Ws-Want the quantity the line
*>   must show.
 Assert-Line-Gross.
     MOVE Ws-Line-No TO Ws-Line-Ed
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), l.GrossIssuedQty) FROM"
            " kit.ExecutionKitLine l JOIN kit.ExecutionKit k ON"
            " k.ExecutionKitId = l.ExecutionKitId WHERE"
            " k.CompanyCode = 'STC' AND k.KitRef = '"
            FUNCTION TRIM(Ws-Kit-Ref) "' AND l.LineNum = "
            FUNCTION TRIM(Ws-Line-Ed)
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE Ws-Want TO TK-EXPECTED-N
     PERFORM Assert-Scalar.

*>   A fresh correlation id for every call: core.IdempotencyKey is
*>   keyed on it alone and a reused id posts nothing (COM-015).
 Prime-Issue.
     INITIALIZE Ws-Issue-Ca
     ADD 1 TO Ws-Seq
     MOVE FUNCTION CURRENT-DATE TO Ws-Stamp
     MOVE SPACES TO Ws-Corr
     STRING "TKITBFL-" Ws-Stamp (9:8) "-" Ws-Seq
         DELIMITED BY SIZE INTO Ws-Corr
     MOVE "STC" TO IS-COMPANY
     MOVE "RDG" TO IS-SITE
     MOVE "TESTER" TO IS-USER
     MOVE 20261006 TO IS-BUSINESS-DATE
     MOVE Ws-Corr TO IS-CORRELATION-ID
     MOVE Ws-Kit-Ref TO KS-KIT-REF
     MOVE 20261006 TO KS-ACTUAL-DATE
     MOVE 20261006 TO KS-POSTING-DATE.

 COPY TESTKITP.
