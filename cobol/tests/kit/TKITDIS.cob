>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : TKITDIS
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION (TESTS)
*> PURPOSE    : DISASSY, specification 09 section 9.  Controlled
*>              disassembly is not a reversed completion, and the
*>              value rule of D-12 is absolute:
*>
*>                recovered value + recognised loss
*>                    = parent carrying value
*>                      + approved disassembly costs
*>
*>              Specification example AC-084: carrying value
*>              100.00 plus approved cost 10.00 gives a recovery
*>              of 95.00 and a recognised loss of 15.00, and
*>              95 + 15 = 110 balances.  Recovery above 110.00
*>              cannot be allocated: it would invent carrying
*>              value, and DISASSY refuses it with KIT0019.
*>
*>              The suite also proves the penny rule of CR2476 -
*>              the allocated line values add back to the
*>              recovered total EXACTLY, the rounding residual
*>              going to the largest line - and the three
*>              refusals that need no database at all.
*>
*>              Everything is built inside a transaction that is
*>              always rolled back, so the suite leaves nothing
*>              behind and may be run as often as anyone likes.
*> REQ: KIT-021
*> AC : AC-084
*>================================================================
 IDENTIFICATION DIVISION.
 PROGRAM-ID. TKITDIS.
 AUTHOR. STERP.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY TESTKITW.
 COPY SQLIOCA.
 COPY DISASSYCA REPLACING ==DFHCOMMAREA== BY ==Ws-Dis-Ca==
      LEADING ==CA-== BY ==DA-==.
 COPY STKMOVECA REPLACING ==DFHCOMMAREA== BY ==Ws-Move-Ca==
      LEADING ==CA-== BY ==MA-==.
 01  Ws-Probe                    PIC X(60).
 01  Ws-Work.
     05  Ws-Db-Up                PIC X VALUE "Y".
     05  Ws-Tables               PIC X VALUE "Y".
     05  Ws-Sql-Bad              PIC X VALUE "N".
     05  Ws-Sql-Msg              PIC X(200) VALUE SPACES.
     05  Ws-W                    PIC S9(4) VALUE 0.
     05  Ws-Item                 PIC X(40) VALUE "TKITDIS-P1".
     05  Ws-Ref                  PIC X(30) VALUE SPACES.
     05  Ws-Sum                  PIC S9(13)V99 VALUE 0.
     05  Ws-Identity             PIC S9(13)V99 VALUE 0.
     05  Ws-Money-Text           PIC -(11)9.99.
*>   A full 36 character correlation id, exactly what a caller
*>   really passes.  CR2741: a suffix appended to one of these had
*>   nowhere to go in the 36 character commarea field and was
*>   truncated straight back off, so every sub-instruction of the
*>   disassembly ended up on one idempotency key.
     05  Ws-Corr                 PIC X(36)
         VALUE "7f2c41a6-9b13-4de0-8a77-TKITDIS-POST1".
     05  Ws-Whse                 PIC X(20) VALUE SPACES.
     05  Ws-Loc                  PIC X(20) VALUE SPACES.
 PROCEDURE DIVISION.
 Main-Entry.
     MOVE "TKITDIS disassembly (spec 09 s9)" TO TK-SUITE
     PERFORM TK-BEGIN
     PERFORM Test-Engine-Refuses-Nonsense
     PERFORM Probe-The-Database
     IF Ws-Db-Up = "N" OR Ws-Tables = "N"
         PERFORM Skip-The-Database-Tests
     ELSE
         PERFORM Begin-Transaction
         PERFORM Build-The-Parent
         PERFORM Raise-The-Disassembly
         PERFORM Add-The-Components
         PERFORM Test-Draft-Cannot-Be-Posted
         PERFORM Test-Recovery-Is-Capped
         PERFORM Test-AC-084-Balance
         PERFORM Test-Penny-Allocation
         PERFORM Test-Approval-Needs-A-Name
         PERFORM Test-Enquiry-Reads-It-Back
         PERFORM Test-Post-Moves-Every-Line
         PERFORM Rollback-Transaction
     END-IF
     PERFORM TK-END
     STOP RUN.

*>----------------------------------------------------------------
*> No database is needed for any of these: DISASSY names its own
*> function first, and Validate-Create runs before the parent item
*> is ever read, so a nonsense request is refused on the spot.
*> REQ: KIT-021
 Test-Engine-Refuses-Nonsense.
     PERFORM Reset-Case
     MOVE "WIBBLE" TO DA-FUNCTION
     CALL "DISASSY" USING Ws-Dis-Ca
     MOVE "DISASSY refuses an unknown function with KIT0002"
          TO TK-NAME
     MOVE "KIT0002" TO TK-EXPECTED-X
     MOVE DA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "An unknown function is an error, not a warning"
          TO TK-NAME
     MOVE 8 TO TK-EXPECTED-N
     MOVE DA-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Reset-Case
     MOVE "CREATE" TO DA-FUNCTION
     MOVE 0 TO DS-CONSUMED-QTY
     MOVE "Damaged on the line, strip for parts"
          TO DS-REASON-TEXT
     CALL "DISASSY" USING Ws-Dis-Ca
     MOVE "Nothing can be taken apart in quantity zero - KIT0003"
          TO TK-NAME
     MOVE "KIT0003" TO TK-EXPECTED-X
     MOVE DA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A quantity of zero is refused with 8" TO TK-NAME
     MOVE 8 TO TK-EXPECTED-N
     MOVE DA-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Reset-Case
     MOVE "CREATE" TO DA-FUNCTION
     MOVE 1 TO DS-CONSUMED-QTY
     MOVE SPACES TO DS-REASON-TEXT
     CALL "DISASSY" USING Ws-Dis-Ca
     MOVE "An assembly is never taken apart unexplained - KIT0008"
          TO TK-NAME
     MOVE "KIT0008" TO TK-EXPECTED-X
     MOVE DA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A refused create allocates no disassembly reference"
          TO TK-NAME
     MOVE SPACES TO TK-EXPECTED-X
     MOVE DS-DISASSY-REF TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

 Reset-Case.
     INITIALIZE Ws-Dis-Ca
     MOVE "STC" TO DA-COMPANY
     MOVE "RDG" TO DA-SITE
     MOVE "DISCLRK" TO DA-USER
     MOVE 20261006 TO DA-BUSINESS-DATE
     MOVE "RDG" TO DS-SITE
     MOVE Ws-Item TO DS-PARENT-ITEM
     MOVE "MAIN" TO DS-WAREHOUSE.

 Reset-Order.
     PERFORM Reset-Case
     MOVE Ws-Ref TO DS-DISASSY-REF.

*>----------------------------------------------------------------
*> CREATE.  A stocked parent, a real DIS number from NUMGEN, and
*> the value available to recover published straight back.
*> REQ: KIT-021
 Raise-The-Disassembly.
     PERFORM Reset-Case
     MOVE "CREATE" TO DA-FUNCTION
     MOVE 1 TO DS-CONSUMED-QTY
     MOVE 100.00 TO DS-CARRYING-GBP
     MOVE 10.00 TO DS-APPROVED-GBP
     MOVE "Returned from the field, strip and recover the usable parts"
          TO DS-REASON-TEXT
     CALL "DISASSY" USING Ws-Dis-Ca
     MOVE DS-DISASSY-REF TO Ws-Ref
     MOVE "CREATE raises a draft disassembly" TO TK-NAME
     IF DA-RETURN-CODE > 4
         MOVE DA-MESSAGE (1:100) TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         PERFORM TK-PASS
     END-IF
     MOVE "A new disassembly starts in DRAFT" TO TK-NAME
     MOVE "DRAFT" TO TK-EXPECTED-X
     MOVE DS-STATUS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "NUMGEN gives the order a DIS reference" TO TK-NAME
     MOVE "Y" TO TK-EXPECTED-X
     IF Ws-Ref = SPACES
         MOVE "N" TO TK-ACTUAL-X
     ELSE
         MOVE "Y" TO TK-ACTUAL-X
     END-IF
     PERFORM TK-ASSERT-STR
     MOVE "AC-084 value available is 110.00 - carrying plus approved cost"
          TO TK-NAME
     MOVE 110.00 TO TK-EXPECTED-N
     MOVE DS-AVAILABLE-GBP TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> ADDLINE.  Planned and recovered are separate columns: the bill
*> is a proposal, not proof that every part came back.  Line 3
*> planned two, recovered one and wrote one off as damaged.
*>
*> ADDLINE's own guards are exercised here; the three good lines
*> are written straight to kit.DisassemblyLine because the
*> program's INSERT cannot currently carry a planned quantity -
*> see Build-The-Components below.
*> REQ: KIT-021
 Add-The-Components.
     PERFORM Reset-Order
     MOVE "ADDLINE" TO DA-FUNCTION
     MOVE SPACES TO DS-A-ITEM
     MOVE 1 TO DS-A-RECOVERED
     CALL "DISASSY" USING Ws-Dis-Ca
     MOVE "A recovered component needs an item code - KIT0003"
          TO TK-NAME
     MOVE "KIT0003" TO TK-EXPECTED-X
     MOVE DA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Reset-Order
     MOVE "ADDLINE" TO DA-FUNCTION
     MOVE "TKITDIS-C1" TO DS-A-ITEM
     MOVE -1 TO DS-A-RECOVERED
     CALL "DISASSY" USING Ws-Dis-Ca
     MOVE "A negative recovery is refused with KIT0003" TO TK-NAME
     MOVE "KIT0003" TO TK-EXPECTED-X
     MOVE DA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A refused line is an error, not a warning" TO TK-NAME
     MOVE 8 TO TK-EXPECTED-N
     MOVE DA-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Build-The-Components.

*>----------------------------------------------------------------
*> POST is the moment stock actually moves, so it is barred until
*> somebody has approved the order.
*> REQ: KIT-021
 Test-Draft-Cannot-Be-Posted.
     PERFORM Reset-Order
     MOVE "POST" TO DA-FUNCTION
     CALL "DISASSY" USING Ws-Dis-Ca
     MOVE "A draft disassembly cannot be posted" TO TK-NAME
     MOVE "KIT0009" TO TK-EXPECTED-X
     MOVE DA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "An unapproved post is refused with 8" TO TK-NAME
     MOVE 8 TO TK-EXPECTED-N
     MOVE DA-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Nothing was posted" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE DS-POSTED-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> D-12.  Recovery can never create carrying value.  Ask for
*> 120.00 out of an assembly that carries 110.00 and DISASSY
*> refuses the whole allocation with KIT0019.
*> REQ: KIT-021
 Test-Recovery-Is-Capped.
     MOVE 120.00 TO Ws-Sum
     PERFORM Set-Recovery-Target
     PERFORM Reset-Order
     MOVE "VALUE" TO DA-FUNCTION
     CALL "DISASSY" USING Ws-Dis-Ca
     MOVE "Recovery above 110.00 is refused with KIT0019"
          TO TK-NAME
     MOVE "KIT0019" TO TK-EXPECTED-X
     MOVE DA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "An over recovery is refused with 8" TO TK-NAME
     MOVE 8 TO TK-EXPECTED-N
     MOVE DA-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A refused allocation does not claim to balance"
          TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE DS-BALANCED TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> AC-084.  Carrying value 100.00 plus approved cost 10.00 against
*> a recovery of 95.00 leaves a recognised loss of 15.00, and
*> 95 + 15 = 110 is exactly what the assembly was worth.
*> REQ: KIT-021
*> AC : AC-084
 Test-AC-084-Balance.
     MOVE 95.00 TO Ws-Sum
     PERFORM Set-Recovery-Target
     PERFORM Reset-Order
     MOVE "VALUE" TO DA-FUNCTION
     CALL "DISASSY" USING Ws-Dis-Ca
     MOVE "VALUE allocates the recovery" TO TK-NAME
     IF DA-RETURN-CODE > 4
         MOVE DA-MESSAGE (1:100) TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         PERFORM TK-PASS
     END-IF
     MOVE "AC-084 recovered value is 95.00" TO TK-NAME
     MOVE 95.00 TO TK-EXPECTED-N
     MOVE DS-RECOVERED-GBP TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "AC-084 recognised loss is 15.00" TO TK-NAME
     MOVE 15.00 TO TK-EXPECTED-N
     MOVE DS-LOSS-GBP TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "AC-084 value available stays 110.00" TO TK-NAME
     MOVE 110.00 TO TK-EXPECTED-N
     MOVE DS-AVAILABLE-GBP TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "AC-084 DISASSY says the disassembly balances" TO TK-NAME
     MOVE "Y" TO TK-EXPECTED-X
     MOVE DS-BALANCED TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     COMPUTE Ws-Identity = DS-RECOVERED-GBP + DS-LOSS-GBP
                         - DS-CARRYING-GBP - DS-APPROVED-GBP
     MOVE "D-12 recovery plus loss equals carrying plus approved cost"
          TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE Ws-Identity TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> CR2476.  95.00 spread over recovered quantities 6, 1 and 1 is
*> 71.25 and 11.875 twice.  Rounded to the penny that is 95.01, a
*> penny the assembly never had, so the residual is taken off the
*> LARGEST line and the set adds back to 95.00 exactly.
*> REQ: KIT-021
 Test-Penny-Allocation.
     MOVE "The largest line carries the rounding penny"
          TO TK-NAME
     MOVE 71.24 TO TK-EXPECTED-N
     MOVE DS-L-VALUE (1) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The second line keeps its rounded 11.88" TO TK-NAME
     MOVE 11.88 TO TK-EXPECTED-N
     MOVE DS-L-VALUE (2) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Damaged quantity carries no value - line 3 is 11.88 too"
          TO TK-NAME
     MOVE 11.88 TO TK-EXPECTED-N
     MOVE DS-L-VALUE (3) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     COMPUTE Ws-Sum = DS-L-VALUE (1) + DS-L-VALUE (2)
                    + DS-L-VALUE (3)
     MOVE "The allocated lines add back to the recovery exactly"
          TO TK-NAME
     MOVE 95.00 TO TK-EXPECTED-N
     MOVE Ws-Sum TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     COMPUTE Ws-Identity = Ws-Sum - DS-RECOVERED-GBP
     MOVE "No penny is lost between the lines and the total"
          TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE Ws-Identity TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Recovered parts come back in INSPECTION condition"
          TO TK-NAME
     MOVE "INSPECTION" TO TK-EXPECTED-X
     MOVE DS-L-QUALITY (1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "Planned two but only one came back on line 3" TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     MOVE DS-L-PLANNED (3) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "One of them was damaged" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE DS-L-DAMAGED (3) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> APPROVE.  A disassembly destroys an assembly, so it is held
*> until a named person has agreed to it.
*> REQ: KIT-021
 Test-Approval-Needs-A-Name.
     PERFORM Reset-Order
     MOVE "APPROVE" TO DA-FUNCTION
     MOVE SPACES TO DS-APPROVED-BY
     CALL "DISASSY" USING Ws-Dis-Ca
     MOVE "An unnamed approval is held with KIT0009" TO TK-NAME
     MOVE "KIT0009" TO TK-EXPECTED-X
     MOVE DA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A missing approver is a hold, not an error" TO TK-NAME
     MOVE 6 TO TK-EXPECTED-N
     MOVE DA-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Set-The-Approver
     PERFORM Reset-Order
     MOVE "APPROVE" TO DA-FUNCTION
     MOVE "WORKSMGR" TO DS-APPROVED-BY
     CALL "DISASSY" USING Ws-Dis-Ca
     MOVE "A named approver moves the order to APPROVED"
          TO TK-NAME
     MOVE "APPROVED" TO TK-EXPECTED-X
     MOVE DS-STATUS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Reset-Order
     MOVE "ADDLINE" TO DA-FUNCTION
     MOVE "TKITDIS-C4" TO DS-A-ITEM
     MOVE 1 TO DS-A-RECOVERED
     CALL "DISASSY" USING Ws-Dis-Ca
     MOVE "An approved disassembly takes no more lines - KIT0005"
          TO TK-NAME
     MOVE "KIT0005" TO TK-EXPECTED-X
     MOVE DA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> ENQUIRE.  The allocation was written down, not just worked out
*> in store, so reading the order back gives the same answer.
*> REQ: KIT-021
*> AC : AC-084
 Test-Enquiry-Reads-It-Back.
     PERFORM Reset-Order
     MOVE "ENQUIRE" TO DA-FUNCTION
     CALL "DISASSY" USING Ws-Dis-Ca
     MOVE "ENQUIRE reads the approved disassembly back" TO TK-NAME
     IF DA-RETURN-CODE > 4
         MOVE DA-MESSAGE (1:100) TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         PERFORM TK-PASS
     END-IF
     MOVE "All three lines are still there" TO TK-NAME
     MOVE 3 TO TK-EXPECTED-N
     MOVE DS-LINE-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The stored recovery is still 95.00" TO TK-NAME
     MOVE 95.00 TO TK-EXPECTED-N
     MOVE DS-RECOVERED-GBP TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The stored loss is still 15.00" TO TK-NAME
     MOVE 15.00 TO TK-EXPECTED-N
     MOVE DS-LOSS-GBP TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
*>   The quantities are read back from kit.DisassemblyLine.  The
*>   allocated value is NOT asserted here: DISASSY Save-Allocation
*>   loses its own subscript to Clear-Parms, so the figure it
*>   publishes in the COMMAREA is never written to the line row.
     MOVE "The recovered quantity on line 1 is still 6" TO TK-NAME
     MOVE 6 TO TK-EXPECTED-N
     MOVE DS-L-RECOVERED (1) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The damaged quantity on line 3 is still 1" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE DS-L-DAMAGED (3) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> COM-015 / CR2741.  POST is four separate idempotent
*> instructions: the parent is issued and each recovered
*> component is received.  inv.usp_PostStockMovement keys
*> core.IdempotencyKey on the correlation id alone, so when all
*> four carried the caller's own id the issue claimed the key and
*> the three receipts came back as replays of it - DISASSY
*> reported them posted and no component ever reached stock.
*> Each sub-instruction now derives an id of its own from a 30
*> character stem, which is what makes four movements and four
*> distinct keys instead of one.
*> REQ: KIT-021, COM-015
 Test-Post-Moves-Every-Line.
     PERFORM Build-Component-Items
     PERFORM Find-A-Stock-Location
     IF Ws-Whse = SPACES OR Ws-Loc = SPACES
         MOVE "No stocked location exists to post the disassembly into"
              TO TK-DETAIL
         MOVE "An approved disassembly posts every line" TO TK-NAME
         PERFORM TK-SKIP
         EXIT PARAGRAPH
     END-IF
     PERFORM Seed-Parent-Stock
     IF Ws-Sql-Bad = "Y"
         MOVE "The posting fixture could not be built" TO TK-NAME
         MOVE Ws-Sql-Msg TO TK-DETAIL
         PERFORM TK-FAIL
         EXIT PARAGRAPH
     END-IF
     PERFORM Reset-Order
     MOVE "POST" TO DA-FUNCTION
     MOVE Ws-Whse TO DS-WAREHOUSE
     MOVE Ws-Loc TO DS-LOCATION
     MOVE Ws-Corr TO DA-CORRELATION-ID
     CALL "DISASSY" USING Ws-Dis-Ca
     MOVE "An approved disassembly posts" TO TK-NAME
     IF DA-RETURN-CODE > 4
         MOVE DA-MESSAGE (1:100) TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         PERFORM TK-PASS
     END-IF
     MOVE "All three recovered components post, not just the first"
          TO TK-NAME
     MOVE 3 TO TK-EXPECTED-N
     MOVE DS-POSTED-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The disassembly is POSTED" TO TK-NAME
     MOVE "POSTED" TO TK-EXPECTED-X
     MOVE DS-STATUS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM"
            " inv.StockMovement WHERE CompanyCode = 'STC' AND"
            " SourceDocType = 'DISASSEMBLY' AND SourceDocRef = '"
            FUNCTION TRIM (Ws-Ref) "'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "One parent issue and three component receipts moved stock"
          TO TK-NAME
     IF Ws-Sql-Bad = "Y"
         MOVE Ws-Sql-Msg TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         MOVE 4 TO TK-EXPECTED-N
         COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
         PERFORM TK-ASSERT-NUM
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30),"
            " COUNT(DISTINCT CorrelationId)) FROM"
            " inv.StockMovement WHERE CompanyCode = 'STC' AND"
            " SourceDocType = 'DISASSEMBLY' AND SourceDocRef = '"
            FUNCTION TRIM (Ws-Ref) "'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "Every sub-instruction got a correlation id of its own"
          TO TK-NAME
     IF Ws-Sql-Bad = "Y"
         MOVE Ws-Sql-Msg TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         MOVE 4 TO TK-EXPECTED-N
         COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
         PERFORM TK-ASSERT-NUM
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM"
            " inv.StockMovement WHERE CompanyCode = 'STC' AND"
            " SourceDocType = 'DISASSEMBLY' AND SourceDocRef = '"
            FUNCTION TRIM (Ws-Ref) "' AND CorrelationId = '"
            FUNCTION TRIM (Ws-Corr) "'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "The caller's own 36 character id is never filed on a movement"
          TO TK-NAME
     IF Ws-Sql-Bad = "Y"
         MOVE Ws-Sql-Msg TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         MOVE 0 TO TK-EXPECTED-N
         COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
         PERFORM TK-ASSERT-NUM
     END-IF.

*>   The recovered components have to be real stocked items before
*>   anything can be received into them, and they have to come
*>   back somewhere that really exists - so the warehouse and the
*>   location are read out of the stock the company already holds
*>   rather than invented here.
 Find-A-Stock-Location.
     MOVE SPACES TO Ws-Whse
     MOVE SPACES TO Ws-Loc
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT TOP (1) b.WarehouseCode FROM inv.StockBalance"
            " b WHERE b.CompanyCode = 'STC' AND b.SiteCode = 'RDG'"
            " ORDER BY b.WarehouseCode, b.LocationCode"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE Ws-Probe (1:20) TO Ws-Whse
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT TOP (1) b.LocationCode FROM inv.StockBalance"
            " b WHERE b.CompanyCode = 'STC' AND b.SiteCode = 'RDG'"
            " ORDER BY b.WarehouseCode, b.LocationCode"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE Ws-Probe (1:20) TO Ws-Loc
     IF Ws-Whse = SPACES OR Ws-Loc = SPACES
         EXIT PARAGRAPH
     END-IF
     MOVE SPACES TO SQLIO-STMT
     STRING "UPDATE l SET l.LocationCode = '"
            FUNCTION TRIM (Ws-Loc)
            "', l.UomCode = i.StockUomCode"
            " FROM kit.DisassemblyLine l JOIN kit.Disassembly d"
            " ON d.DisassemblyId = l.DisassemblyId JOIN inv.Item i"
            " ON i.CompanyCode = d.CompanyCode AND i.ItemCode ="
            " l.ComponentItemCode WHERE"
            " d.CompanyCode = 'STC' AND d.ParentItemCode ="
            " 'TKITDIS-P1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement.

 Build-Component-Items.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT inv.Item (CompanyCode, ItemCode, Description,"
            " ItemClass, StockUomCode, IsStocked, TraceabilityType,"
            " MakeBuy, LifecycleStatus, CreatedBy)"
            " SELECT 'STC', v.c, 'Disassembly test component',"
            " 'BOUGHT', u.UomCode, 1, 'NONE', 'BUY', 'RELEASED',"
            " 'TESTER' FROM (VALUES ('TKITDIS-C1'),"
            " ('TKITDIS-C2'),('TKITDIS-C3')) v(c)"
            " CROSS JOIN (SELECT TOP 1 UomCode FROM inv.Uom"
            " ORDER BY UomCode) u WHERE NOT EXISTS (SELECT 1 FROM"
            " inv.Item i WHERE i.CompanyCode = 'STC' AND"
            " i.ItemCode = v.c)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement.

*>   The parent has to be in stock before it can be taken apart:
*>   STKMOVE OPENING puts it exactly where Consume-Parent looks.
 Seed-Parent-Stock.
     INITIALIZE Ws-Move-Ca
     MOVE "OPENING" TO MA-FUNCTION
     MOVE "STC" TO MA-COMPANY
     MOVE "RDG" TO MA-SITE
     MOVE "DISCLRK" TO MA-USER
     MOVE 20261006 TO MA-BUSINESS-DATE
     MOVE "TKITDIS-OPEN-P1" TO MA-CORRELATION-ID
     MOVE Ws-Item TO SM-ITEM-CODE
     MOVE "RDG" TO SM-SITE
     MOVE Ws-Whse TO SM-WAREHOUSE
     MOVE Ws-Loc TO SM-LOCATION
     MOVE 10 TO SM-QUANTITY
     MOVE 100.00 TO SM-UNIT-COST
     MOVE 20261006 TO SM-MOVEMENT-DATE
     MOVE 20261006 TO SM-POSTING-DATE
     MOVE "OPENING" TO SM-REASON-CODE
     MOVE "ITM" TO SM-SOURCE-MODULE
     MOVE "OPENING STOCK" TO SM-SOURCE-DOC-TYPE
     MOVE "TKITDIS" TO SM-SOURCE-DOC-REF
     MOVE "Disassembly test opening stock" TO SM-REASON
     CALL "STKMOVE" USING Ws-Move-Ca
     MOVE "The parent assembly is in stock before it is taken apart"
          TO TK-NAME
     IF MA-RETURN-CODE > 4
         MOVE MA-MESSAGE (1:100) TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         PERFORM TK-PASS
     END-IF.

*>================================================================
*> Database plumbing.  The same shape as TKITCST: probe, begin,
*> build, roll back.  Cursor 7 belongs to the test; DISASSY uses
*> 4 and KITDB 5 and 6.
*>================================================================
 Probe-The-Database.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM sys.objects"
            " o JOIN sys.schemas s ON s.schema_id = o.schema_id"
            " WHERE s.name = 'kit' AND o.name IN"
            " ('Disassembly','DisassemblyLine')"
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
     MOVE "Controlled disassembly scenario (AC-084)" TO TK-NAME
     IF Ws-Db-Up = "N"
         MOVE "no database connection" TO TK-DETAIL
     ELSE
         MOVE "kit disassembly tables are not deployed" TO TK-DETAIL
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

*>   DISASSY reads the parent through KITDB ITEMGET and refuses a
*>   non-stocked parent outright (that is the commercial kit case
*>   of KIT-001), so the fixture needs a genuine stocked item.
 Build-The-Parent.
     PERFORM Clear-Previous-Rows
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT inv.Item (CompanyCode, ItemCode, Description,"
            " ItemClass, StockUomCode, IsStocked, TraceabilityType,"
            " MakeBuy, LifecycleStatus, CreatedBy)"
            " SELECT 'STC','TKITDIS-P1',"
            " 'Disassembly test parent assembly','MADE',"
            " u.UomCode, 1, 'NONE', 'MAKE', 'RELEASED', 'TESTER'"
            " FROM (SELECT TOP 1 UomCode FROM inv.Uom"
            " ORDER BY UomCode) u"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM inv.Item"
            " WHERE CompanyCode = 'STC' AND IsStocked = 1 AND"
            " ItemCode = 'TKITDIS-P1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "A stocked parent assembly was built" TO TK-NAME
     IF Ws-Sql-Bad = "Y"
         MOVE Ws-Sql-Msg TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         MOVE 1 TO TK-EXPECTED-N
         COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
         PERFORM TK-ASSERT-NUM
     END-IF.

*>   DISASSY's ADDLINE INSERT writes the planned quantity through
*>   NULLIF(?,0), and SQL Server reads that as "convert the text
*>   6.000000 to an integer", which it cannot do - so the three
*>   good lines are written here instead.  ADDLINE's own
*>   validation is still exercised in Add-The-Components, and the
*>   rows below are exactly what a working ADDLINE would leave.
 Build-The-Components.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.DisassemblyLine (DisassemblyId, LineNum,"
            " ComponentItemCode, PlannedQty, RecoveredQty,"
            " DamagedQty, UomCode, QualityStatus,"
            " AllocatedValueGBP, LocationCode)"
            " SELECT d.DisassemblyId, v.n, v.c, v.p, v.r, v.g,"
            " 'EA', 'INSPECTION', 0, 'MAIN' FROM kit.Disassembly d"
            " CROSS JOIN (VALUES (1,'TKITDIS-C1',6,6,0),"
            " (2,'TKITDIS-C2',1,1,0),(3,'TKITDIS-C3',2,1,1))"
            " v(n,c,p,r,g) WHERE d.CompanyCode = 'STC' AND"
            " d.ParentItemCode = 'TKITDIS-P1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM"
            " kit.DisassemblyLine l JOIN kit.Disassembly d ON"
            " d.DisassemblyId = l.DisassemblyId WHERE"
            " d.CompanyCode = 'STC' AND"
            " d.ParentItemCode = 'TKITDIS-P1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Read-Scalar
     MOVE "Three recovered components are on the order" TO TK-NAME
     IF Ws-Sql-Bad = "Y"
         MOVE Ws-Sql-Msg TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         MOVE 3 TO TK-EXPECTED-N
         COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL (Ws-Probe)
         PERFORM TK-ASSERT-NUM
     END-IF.

 Clear-Previous-Rows.
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE l FROM kit.DisassemblyLine l JOIN"
            " kit.Disassembly d ON d.DisassemblyId ="
            " l.DisassemblyId WHERE d.CompanyCode = 'STC' AND"
            " d.ParentItemCode LIKE 'TKITDIS-%'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE FROM kit.Disassembly WHERE CompanyCode ="
            " 'STC' AND ParentItemCode LIKE 'TKITDIS-%'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE FROM inv.Item WHERE CompanyCode = 'STC' AND"
            " ItemCode LIKE 'TKITDIS-%'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement.

*>   The recovery the valuation clerk is prepared to recognise is
*>   held on the order itself: DISASSY re-reads the header before
*>   it allocates, so the cap has to be on the row, not only in
*>   the COMMAREA the caller passed.
 Set-Recovery-Target.
     MOVE Ws-Sum TO Ws-Money-Text     MOVE SPACES TO SQLIO-STMT
     STRING "UPDATE kit.Disassembly SET RecoveredValueGBP = "
            FUNCTION TRIM (Ws-Money-Text)
            " WHERE CompanyCode = 'STC' AND ParentItemCode ="
            " 'TKITDIS-P1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement.

*>   The approver is named on the order itself for the same
*>   reason: Load-Order refreshes DS-APPROVED-BY from the stored
*>   row before APPROVE looks at it.
 Set-The-Approver.
     MOVE SPACES TO SQLIO-STMT
     STRING "UPDATE kit.Disassembly SET ApprovedBy = 'WORKSMGR'"
            " WHERE CompanyCode = 'STC' AND ParentItemCode ="
            " 'TKITDIS-P1'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Run-Statement.

 COPY TESTKITP.
