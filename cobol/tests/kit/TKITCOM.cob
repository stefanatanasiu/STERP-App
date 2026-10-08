>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : TKITCOM
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION (TESTS)
*> PURPOSE    : KITCOMM, specification 09 and decision D-11.
*>
*>              A commercial sales kit parent is a COMMERCIAL
*>              IDENTITY, not an item balance.  It is never
*>              received into stock, never completed and never
*>              disassembled, so KV-PARENT-STOCK is "N" on every
*>              answer the program ever gives - including the
*>              answer to a customer return (KIT-001).
*>
*>              Revenue is allocated to the components by
*>              RELATIVE STANDALONE SELLING PRICE and has to
*>              reconcile to the parent net amount to the penny.
*>              100.00 split 200 : 50 : 50 rounds to 66.67,
*>              16.67 and 16.67, which is 100.01 - a penny the
*>              customer never paid - so the residual is taken
*>              off the largest line and the set adds back to
*>              100.00 exactly (CR2514, KIT-023).
*>
*>              A kit whose components carry no standalone price
*>              cannot be allocated at all: KIT0028.
*>
*>              Everything is built inside a transaction that is
*>              always rolled back, so the suite leaves nothing
*>              behind and may be run as often as anyone likes.
*> REQ: KIT-001, KIT-023
*>================================================================
 IDENTIFICATION DIVISION.
 PROGRAM-ID. TKITCOM.
 AUTHOR. STERP.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY TESTKITW.
 COPY SQLIOCA.
 COPY KITCOMMCA REPLACING ==DFHCOMMAREA== BY ==Ws-Comm-Ca==
      LEADING ==CA-== BY ==KC-==.
 01  Ws-Probe                    PIC X(60).
 01  Ws-Work.
     05  Ws-Db-Up                PIC X VALUE "Y".
     05  Ws-Tables               PIC X VALUE "Y".
     05  Ws-Sql-Bad              PIC X VALUE "N".
     05  Ws-Sql-Msg              PIC X(200) VALUE SPACES.
     05  Ws-W                    PIC S9(4) VALUE 0.
     05  Ws-Kit-Item             PIC X(40) VALUE "TKITCOM-KIT".
     05  Ws-Nsp-Item             PIC X(40) VALUE "TKITCOM-NSP".
     05  Ws-Sum                  PIC S9(13)V99 VALUE 0.
 PROCEDURE DIVISION.
 Main-Entry.
     MOVE "TKITCOM commercial kits (D-11)" TO TK-SUITE
     PERFORM TK-BEGIN
     PERFORM Test-Engine-Refuses-Nonsense
     PERFORM Probe-The-Database
     IF Ws-Db-Up = "N" OR Ws-Tables = "N"
         PERFORM Skip-The-Database-Tests
     ELSE
         PERFORM Begin-Transaction
         PERFORM Build-The-Definitions
         PERFORM Test-Revenue-Is-Allocated-By-SSP
         PERFORM Test-Allocation-Reconciles
         PERFORM Test-No-Price-Is-Refused
         PERFORM Test-Return-Credits-Allocated-Value
         PERFORM Test-Partial-Set-Is-Not-A-Kit
         PERFORM Test-Partial-Delivery-Is-Not-A-Kit
         PERFORM Rollback-Transaction
     END-IF
     PERFORM TK-END
     STOP RUN.

*>----------------------------------------------------------------
*> No database is needed: KITCOMM names its own function before it
*> reads anything, and it clears its answer before it starts, so a
*> refused call still says the parent has no stock.
*> REQ: KIT-001, KIT-023
 Test-Engine-Refuses-Nonsense.
     PERFORM Reset-Case
     MOVE "WIBBLE" TO KC-FUNCTION
     CALL "KITCOMM" USING Ws-Comm-Ca
     MOVE "KITCOMM refuses an unknown function with KIT0002"
          TO TK-NAME
     MOVE "KIT0002" TO TK-EXPECTED-X
     MOVE KC-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "An unknown function is an error, not a warning"
          TO TK-NAME
     MOVE 8 TO TK-EXPECTED-N
     MOVE KC-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Reset-Case
     MOVE "ALLOCATE" TO KC-FUNCTION
     MOVE SPACES TO KV-KIT-ITEM
     CALL "KITCOMM" USING Ws-Comm-Ca
     MOVE "An allocation with no kit item is refused with KIT0001"
          TO TK-NAME
     MOVE "KIT0001" TO TK-EXPECTED-X
     MOVE KC-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "KIT-001 even a refused call creates no parent stock"
          TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE KV-PARENT-STOCK TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A refused allocation allocates nothing" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KV-ALLOC-TOTAL TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

 Reset-Case.
     INITIALIZE Ws-Comm-Ca
     MOVE "STC" TO KC-COMPANY
     MOVE "RDG" TO KC-SITE
     MOVE "SALESCLK" TO KC-USER
     MOVE 20261006 TO KC-BUSINESS-DATE
     MOVE "RDG" TO KV-SITE
     MOVE Ws-Kit-Item TO KV-KIT-ITEM
     MOVE "GBP" TO KV-CURRENCY
     MOVE "TKITCOM-SO-1" TO KV-ORDER-REF
     MOVE 1 TO KV-ORDER-LINE
     MOVE 1 TO KV-PARENT-QTY
     MOVE 100.00 TO KV-PARENT-NET.

 Ask-To-Allocate.
     PERFORM Reset-Case
     MOVE "ALLOCATE" TO KC-FUNCTION
     CALL "KITCOMM" USING Ws-Comm-Ca.

*>----------------------------------------------------------------
*> D-11.  100.00 of parent revenue against standalone prices of
*> 200.00, 2 x 25.00 and 2 x 25.00 - weights 200 : 50 : 50 - is
*> two thirds and a sixth twice.  Nobody can invoice a third of a
*> penny, so the rounded shares are 66.67, 16.67 and 16.67.
*> REQ: KIT-023
 Test-Revenue-Is-Allocated-By-SSP.
     PERFORM Ask-To-Allocate
     MOVE "ALLOCATE answers a released commercial kit" TO TK-NAME
     IF KC-RETURN-CODE > 4
         MOVE KC-MESSAGE (1:100) TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         PERFORM TK-PASS
     END-IF
     MOVE "The definition has three components" TO TK-NAME
     MOVE 3 TO TK-EXPECTED-N
     MOVE KV-LINE-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The dearest component takes two thirds of the revenue"
          TO TK-NAME
     MOVE 66.666667 TO TK-EXPECTED-N
     MOVE KV-L-ALLOC-PCT (1) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The largest line carries the rounding residual"
          TO TK-NAME
     MOVE 66.66 TO TK-EXPECTED-N
     MOVE KV-L-ALLOC-NET (1) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Component two is allocated 16.67" TO TK-NAME
     MOVE 16.67 TO TK-EXPECTED-N
     MOVE KV-L-ALLOC-NET (2) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Component three is allocated 16.67" TO TK-NAME
     MOVE 16.67 TO TK-EXPECTED-N
     MOVE KV-L-ALLOC-NET (3) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The residual is the single penny of rounding"
          TO TK-NAME
     MOVE -0.01 TO TK-EXPECTED-N
     MOVE KV-RESIDUAL TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> CR2514.  The allocated amounts must add back to the parent net
*> amount EXACTLY - the sales ledger has to agree with the
*> invoice, and 99.99 or 100.01 is a difference somebody has to
*> write off every month.
*> REQ: KIT-023
 Test-Allocation-Reconciles.
     MOVE "The allocated total is the parent net amount exactly"
          TO TK-NAME
     MOVE 100.00 TO TK-EXPECTED-N
     MOVE KV-ALLOC-TOTAL TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     COMPUTE Ws-Sum = KV-ALLOC-TOTAL - KV-PARENT-NET
     MOVE "Allocated total less parent net is nil" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE Ws-Sum TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     COMPUTE Ws-Sum = KV-L-ALLOC-NET (1) + KV-L-ALLOC-NET (2)
                    + KV-L-ALLOC-NET (3)
     MOVE "The three component lines add back to 100.00"
          TO TK-NAME
     MOVE 100.00 TO TK-EXPECTED-N
     MOVE Ws-Sum TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "KITCOMM says the allocation reconciles" TO TK-NAME
     MOVE "Y" TO TK-EXPECTED-X
     MOVE KV-RECONCILED TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "KIT-001 allocating revenue creates no parent stock"
          TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE KV-PARENT-STOCK TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> A relative price allocation needs prices to be relative to.
*> With no standalone selling price anywhere on the kit there is
*> no defensible basis at all, and KITCOMM says so rather than
*> inventing an even split.
*> REQ: KIT-023
 Test-No-Price-Is-Refused.
     PERFORM Reset-Case
     MOVE "ALLOCATE" TO KC-FUNCTION
     MOVE Ws-Nsp-Item TO KV-KIT-ITEM
     CALL "KITCOMM" USING Ws-Comm-Ca
     MOVE "A kit with no standalone prices is refused - KIT0028"
          TO TK-NAME
     MOVE "KIT0028" TO TK-EXPECTED-X
     MOVE KC-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "An unpriced kit is refused with 8" TO TK-NAME
     MOVE 8 TO TK-EXPECTED-N
     MOVE KC-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "An unpriced kit allocates nothing" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KV-ALLOC-TOTAL TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "An unpriced kit does not claim to reconcile" TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE KV-RECONCILED TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> D-11.  A customer may send one component back.  The credit is
*> the ALLOCATED value of that component - which for the largest
*> line is 66.66, the figure that was actually invoiced, not the
*> 66.67 a fresh calculation from list would produce - and the
*> parent kit is NOT returned to stock, because it never was
*> stock (KIT-001).
*> REQ: KIT-001, KIT-023
 Test-Return-Credits-Allocated-Value.
     PERFORM Reset-Case
     MOVE "RETURNCK" TO KC-FUNCTION
     MOVE 1 TO KV-RETURN-LINE
     MOVE 1 TO KV-RETURN-QTY
     CALL "KITCOMM" USING Ws-Comm-Ca
     MOVE "RETURNCK answers a partial component return" TO TK-NAME
     IF KC-RETURN-CODE > 4
         MOVE KC-MESSAGE (1:100) TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         PERFORM TK-PASS
     END-IF
     MOVE "The credit is the allocated 66.66, not list 66.67"
          TO TK-NAME
     MOVE 66.66 TO TK-EXPECTED-N
     MOVE KV-CREDIT-GBP TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "KIT-001 a commercial kit return creates no parent stock"
          TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE KV-PARENT-STOCK TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A return is not a despatch" TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE KV-MAY-DESPATCH TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Reset-Case
     MOVE "RETURNCK" TO KC-FUNCTION
     MOVE 2 TO KV-RETURN-LINE
     MOVE 1 TO KV-RETURN-QTY
     CALL "KITCOMM" USING Ws-Comm-Ca
     MOVE "Half of a two piece component credits half its 16.67"
          TO TK-NAME
     MOVE 8.34 TO TK-EXPECTED-N
     MOVE KV-CREDIT-GBP TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Reset-Case
     MOVE "RETURNCK" TO KC-FUNCTION
     MOVE 2 TO KV-RETURN-LINE
     MOVE 2 TO KV-RETURN-QTY
     CALL "KITCOMM" USING Ws-Comm-Ca
     MOVE "The whole of component two credits its full 16.67"
          TO TK-NAME
     MOVE 16.67 TO TK-EXPECTED-N
     MOVE KV-CREDIT-GBP TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Reset-Case
     MOVE "RETURNCK" TO KC-FUNCTION
     MOVE 2 TO KV-RETURN-LINE
     MOVE 9 TO KV-RETURN-QTY
     CALL "KITCOMM" USING Ws-Comm-Ca
     MOVE "More cannot come back than went out - KIT0011"
          TO TK-NAME
     MOVE "KIT0011" TO TK-EXPECTED-X
     MOVE KC-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> KIT-023 and D-11.  An incomplete component set is not a kit.
*> None of the test components has any free stock, so two kits
*> cannot be despatched and KITCOMM names the component that is
*> holding the order up.
*> REQ: KIT-023
 Test-Partial-Set-Is-Not-A-Kit.
     PERFORM Reset-Case
     MOVE "SETS" TO KC-FUNCTION
     MOVE 2 TO KV-PARENT-QTY
     CALL "KITCOMM" USING Ws-Comm-Ca
     MOVE "Free stock supports no complete sets" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KV-COMPLETE-SETS TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "An incomplete set may not be despatched" TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE KV-MAY-DESPATCH TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A partial set is reported with KIT0020" TO TK-NAME
     MOVE "KIT0020" TO TK-EXPECTED-X
     MOVE KC-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A shortage is a warning the order desk can act on"
          TO TK-NAME
     MOVE 4 TO TK-EXPECTED-N
     MOVE KC-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The limiting component is named" TO TK-NAME
     MOVE "TKITCOM-C1" TO TK-EXPECTED-X
     MOVE KV-LIMIT-ITEM TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "Two of the limiting component are short" TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     MOVE KV-LIMIT-SHORT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "KIT-001 counting sets creates no parent stock"
          TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE KV-PARENT-STOCK TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> KIT-023.  FULFIL reconciles the parent sold quantity to the
*> component delivery.  One set delivered out of two sold is not
*> two kits, however the despatch note is worded.
*> REQ: KIT-023
 Test-Partial-Delivery-Is-Not-A-Kit.
     PERFORM Reset-Case
     MOVE "FULFIL" TO KC-FUNCTION
     MOVE 2 TO KV-PARENT-QTY
     MOVE 1 TO KV-DELIVERED-SETS
     CALL "KITCOMM" USING Ws-Comm-Ca
     MOVE "One set out of two is not a complete parent kit"
          TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE KV-MAY-DESPATCH TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "FULFIL reports the one set that was delivered"
          TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE KV-COMPLETE-SETS TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Reset-Case
     MOVE "FULFIL" TO KC-FUNCTION
     MOVE 2 TO KV-PARENT-QTY
     MOVE 2 TO KV-DELIVERED-SETS
     CALL "KITCOMM" USING Ws-Comm-Ca
     MOVE "Both sets delivered is a complete parent kit" TO TK-NAME
     MOVE "Y" TO TK-EXPECTED-X
     MOVE KV-MAY-DESPATCH TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A fulfilled kit still reconciles to the parent net"
          TO TK-NAME
     MOVE "Y" TO TK-EXPECTED-X
     MOVE KV-RECONCILED TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "KIT-001 fulfilment creates no parent stock" TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE KV-PARENT-STOCK TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>================================================================
*> Database plumbing.  The same shape as TKITCST: probe, begin,
*> build, roll back.  Cursor 7 belongs to the test; KITCOMM uses
*> 3 and KITDB 5 and 6.
*>================================================================
 Probe-The-Database.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM sys.objects"
            " o JOIN sys.schemas s ON s.schema_id = o.schema_id"
            " WHERE s.name = 'kit' AND o.name IN"
            " ('KitDefinition','KitDefinitionLine')"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     IF Ws-Sql-Bad = "Y"
         MOVE "N" TO Ws-Db-Up
         MOVE "N" TO Ws-Sql-Bad
         EXIT PARAGRAPH
     END-IF
     IF FUNCTION NUMVAL (Ws-Probe) < 2
         MOVE "N" TO Ws-Tables
     END-IF.

 Skip-The-Database-Tests.
     MOVE "Commercial kit allocation scenario (D-11)" TO TK-NAME
     IF Ws-Db-Up = "N"
         MOVE "no database connection" TO TK-DETAIL
     ELSE
         MOVE "kit definition tables are not deployed" TO TK-DETAIL
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

*>   kit.KitDefinition points at inv.Item for its parent, so the
*>   two commercial identities are real catalogue rows - flagged
*>   NOT stocked, which is what a commercial kit parent is.  The
*>   components are deliberately unknown to stock so the SETS
*>   shortage is the same every time the suite runs.
 Build-The-Definitions.
     PERFORM Clear-Previous-Rows
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT inv.Item (CompanyCode, ItemCode, Description,"
            " ItemClass, StockUomCode, IsStocked, IsSaleable,"
            " LifecycleStatus, CreatedBy)"
            " SELECT 'STC', v.c, v.d, 'KIT', u.UomCode, 0, 1,"
            " 'RELEASED', 'TESTER'"
            " FROM (SELECT TOP 1 UomCode FROM inv.Uom"
            " ORDER BY UomCode) u CROSS JOIN (VALUES"
            " ('TKITCOM-KIT','Commercial kit test parent'),"
            " ('TKITCOM-NSP','Commercial kit with no prices'))"
            " v(c,d)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.KitDefinition (CompanyCode, KitItemCode,"
            " KitVersion, Name, SupplyKind, AllowPartialDespatch,"
            " EffectiveFrom, Status, CreatedBy) VALUES"
            " ('STC','TKITCOM-KIT','A','Commercial kit test',"
            " 'SINGLE',0,CONVERT(date,'20200101',112),"
            " 'RELEASED','TESTER'),"
            " ('STC','TKITCOM-NSP','A','Commercial kit unpriced',"
            " 'SINGLE',0,CONVERT(date,'20200101',112),"
            " 'RELEASED','TESTER')"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.KitDefinitionLine (KitDefId, LineNum,"
            " ComponentItemCode, Quantity, UomCode,"
            " ConversionFactor, StandaloneSellingPrice,"
            " IsCustomerSpecified, SubstitutionAllowed,"
            " IsOptional) SELECT d.KitDefId, v.n, v.c, v.q, 'EA',"
            " 1, v.p, 0, 1, 0 FROM kit.KitDefinition d CROSS JOIN"
            " (VALUES (1,'TKITCOM-C1',1,200.00),"
            " (2,'TKITCOM-C2',2,25.00),"
            " (3,'TKITCOM-C3',2,25.00)) v(n,c,q,p)"
            " WHERE d.CompanyCode = 'STC' AND"
            " d.KitItemCode = 'TKITCOM-KIT'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.KitDefinitionLine (KitDefId, LineNum,"
            " ComponentItemCode, Quantity, UomCode,"
            " ConversionFactor, StandaloneSellingPrice,"
            " IsOptional) SELECT d.KitDefId, v.n, v.c, v.q, 'EA',"
            " 1, NULL, 0 FROM kit.KitDefinition d CROSS JOIN"
            " (VALUES (1,'TKITCOM-N1',1),(2,'TKITCOM-N2',1))"
            " v(n,c,q) WHERE d.CompanyCode = 'STC' AND"
            " d.KitItemCode = 'TKITCOM-NSP'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM"
            " kit.KitDefinitionLine l JOIN kit.KitDefinition d"
            " ON d.KitDefId = l.KitDefId WHERE d.CompanyCode ="
            " 'STC' AND d.KitItemCode LIKE 'TKITCOM-%'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "Two commercial kit definitions were built" TO TK-NAME
     IF Ws-Sql-Bad = "Y"
         MOVE Ws-Sql-Msg TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         MOVE 5 TO TK-EXPECTED-N
         COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
         PERFORM TK-ASSERT-NUM
     END-IF.

 Clear-Previous-Rows.
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE l FROM kit.KitDefinitionLine l JOIN"
            " kit.KitDefinition d ON d.KitDefId = l.KitDefId"
            " WHERE d.CompanyCode = 'STC' AND"
            " d.KitItemCode LIKE 'TKITCOM-%'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE FROM kit.KitDefinition WHERE CompanyCode ="
            " 'STC' AND KitItemCode LIKE 'TKITCOM-%'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE FROM inv.Item WHERE CompanyCode = 'STC' AND"
            " ItemCode LIKE 'TKITCOM-%'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement.

 COPY TESTKITP.
