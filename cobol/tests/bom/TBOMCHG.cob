>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : TBOMCHG
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL - UNIT TESTS
*> PURPOSE    : BOMBULK - bulk component change.  The obligations
*>              of BOM-024 and BOM-030 are the ones worth testing
*>              because they are the ones a bulk utility usually
*>              gets wrong:
*>                - a complete preview before anything is applied
*>                - one failed bill refuses the whole run
*>                - a released revision is never written to
*>                - repeating an APPLY does not change twice
*>              The targets are the seeded B1 to B6 demonstration
*>              bills, so the before and after quantities are the
*>              published ones.
*>              Needs the database and the BOM demonstration seed;
*>              without them the suite reports TAP # SKIP.
*>              Everything it writes carries the change reference
*>              prefix TBOMCHG- and is removed again at the end.
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2026-10-06 STERP BULK CHANGE PREVIEW, REFUSAL AND IDEMPOTENCY
*>================================================================
*> REQ: BOM-002, BOM-004, BOM-009, BOM-010, BOM-013, BOM-018,
*> REQ: BOM-023, BOM-024, BOM-028, BOM-029, BOM-030
*> AC: AC-085, AC-088, AC-090, AC-092, AC-094, AC-095, AC-096
 IDENTIFICATION DIVISION.
 PROGRAM-ID. TBOMCHG.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY TESTKITW.
 COPY SQLIOCA.
 COPY BOMBULKCA REPLACING ==DFHCOMMAREA== BY ==Ws-Bulk==
      LEADING ==CA-== BY ==BA-==.
 01  Ws-Row.
     05  Ws-Col OCCURS 4 TIMES   PIC X(60).
 01  Ws-Flags.
     05  Ws-Db-Ok                PIC X VALUE "Y".
     05  Ws-I                    PIC S9(4) VALUE 0.
     05  Ws-Hit                  PIC S9(4) VALUE 0.
     05  Ws-A-Qty                PIC S9(13)V9(6) VALUE 0.
     05  Ws-Skip-Note            PIC X(60) VALUE SPACES.
 PROCEDURE DIVISION.
 Main-Entry.
     MOVE "TBOMCHG" TO TK-SUITE
     PERFORM TK-BEGIN
     PERFORM Check-Database
     IF Ws-Db-Ok = "N"
         PERFORM Skip-Everything
         PERFORM TK-END
         STOP RUN
     END-IF
     PERFORM Bad-Requests-Are-Refused
     PERFORM Preview-Finds-Every-Bill
     PERFORM Released-Gets-A-New-Revision
     PERFORM Cycle-Is-Refused
     PERFORM Unknown-Item-Is-Refused
     PERFORM Nothing-To-Change-Is-Skipped
     PERFORM B3-Quantity-Change-Preview
     PERFORM B5-Remove-First-Occurrence
     PERFORM B5-Add-A-Component
     PERFORM Apply-Then-Apply-Again
     PERFORM Tidy-Up
     PERFORM TK-END
     STOP RUN.

*>----------------------------------------------------------------
 Check-Database.
     PERFORM Set-Up-Change
     MOVE "TBOMCHG-PROBE" TO BB-CHANGE-REF
     MOVE "BOMREF" TO BB-SCOPE
     MOVE "BOM-DEMO-FRAME" TO BB-SCOPE-ITEM
     MOVE "QTYCHG" TO BB-ACTION
     MOVE "DEMO-PIN" TO BB-OLD-ITEM
     MOVE 1 TO BB-NEW-QTY
     MOVE "PREVIEW" TO BA-FUNCTION
     CALL "BOMBULK" USING Ws-Bulk
     IF BA-RETURN-CODE = 12 OR BB-TARGET-CNT = 0
         MOVE "N" TO Ws-Db-Ok
         MOVE BA-MESSAGE TO Ws-Skip-Note
     END-IF.

 Skip-Everything.
     MOVE "BOM demonstration data or database not available"
         TO TK-DETAIL
     MOVE "bulk change preview, refusal and idempotency"
         TO TK-NAME
     PERFORM TK-SKIP.

 Set-Up-Change.
     INITIALIZE Ws-Bulk
     MOVE "STC" TO BA-COMPANY
     MOVE "RDG" TO BA-SITE
     MOVE "ENGINEER" TO BA-USER
     MOVE 20261101 TO BA-BUSINESS-DATE
     MOVE 20261101 TO BB-EFF-DATE
     MOVE "ENGINEER" TO BB-REQUESTED-BY
     MOVE "Y" TO BB-INCLUDE-DRAFT
     MOVE 25 TO BB-WIN-MAX
     MOVE 1 TO BB-FROM-LINE.

 Find-The-Line-For-Rev.
     MOVE 0 TO Ws-Hit
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > BB-WIN-CNT
         IF BB-L-BOM-REV(Ws-I) = TK-EXPECTED-X(1:4)
             MOVE Ws-I TO Ws-Hit
         END-IF
     END-PERFORM.

*>----------------------------------------------------------------
*> REQ: BOM-024.  A bulk change that cannot say what it changes is
*> refused before a bill is read, not half way through.
 Bad-Requests-Are-Refused.
     PERFORM Set-Up-Change
     MOVE "REPLACE" TO BB-ACTION
     MOVE "DEMO-PIN" TO BB-OLD-ITEM
     MOVE "DEMO-PIN-HD" TO BB-NEW-ITEM
     MOVE "PREVIEW" TO BA-FUNCTION
     CALL "BOMBULK" USING Ws-Bulk
     MOVE "A bulk change without a reference is refused"
         TO TK-NAME
     MOVE "BOM1401" TO TK-EXPECTED-X
     MOVE BA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Set-Up-Change
     MOVE "TBOMCHG-BADACT" TO BB-CHANGE-REF
     MOVE "SWAP" TO BB-ACTION
     MOVE "DEMO-PIN" TO BB-OLD-ITEM
     MOVE "PREVIEW" TO BA-FUNCTION
     CALL "BOMBULK" USING Ws-Bulk
     MOVE "An unknown action is refused" TO TK-NAME
     MOVE "BOM1402" TO TK-EXPECTED-X
     MOVE BA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Set-Up-Change
     MOVE "TBOMCHG-BADFUN" TO BB-CHANGE-REF
     MOVE "REMOVE" TO BB-ACTION
     MOVE "DEMO-PIN" TO BB-OLD-ITEM
     MOVE "DESTROY" TO BA-FUNCTION
     CALL "BOMBULK" USING Ws-Bulk
     MOVE "An unknown function is refused" TO TK-NAME
     MOVE "BOM1400" TO TK-EXPECTED-X
     MOVE BA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> REQ: BOM-023, BOM-024.  DEMO-PIN is used on seven seeded bills
*> - the frame, the brace, the phantom pack, the option bill and
*> both boundary revisions, plus the draft that has no released
*> parent.  All seven have to appear in the preview, and because
*> the option bill has an unconfigured alternate group (BOM-018)
*> at least one proposal fails, which refuses the whole run.
 Preview-Finds-Every-Bill.
     PERFORM Set-Up-Change
     MOVE "TBOMCHG-REPL-ITEM" TO BB-CHANGE-REF
     MOVE "Replace the pin everywhere it is used" TO BB-TITLE
     MOVE "REPLACE" TO BB-ACTION
     MOVE "ITEM" TO BB-SCOPE
     MOVE "DEMO-PIN" TO BB-OLD-ITEM
     MOVE "DEMO-PIN-HD" TO BB-NEW-ITEM
     MOVE "PREVIEW" TO BA-FUNCTION
     CALL "BOMBULK" USING Ws-Bulk
     MOVE "Every bill that uses the pin is in the preview"
         TO TK-NAME
     MOVE 7 TO TK-EXPECTED-N
     MOVE BB-TARGET-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A preview changes nothing" TO TK-NAME
     MOVE "PREVIEW" TO TK-EXPECTED-X
     MOVE BB-STATUS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A preview applies nothing" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE BB-APPLIED-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "An unconfigured alternate group fails the proposal"
         TO TK-NAME
     IF BB-FAILED-CNT > 0
         PERFORM TK-PASS
     ELSE
         MOVE BB-SUMMARY TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     MOVE "One failed bill refuses the whole bulk change"
         TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE BB-MAY-APPLY TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The refusal is reported as BOM1411" TO TK-NAME
     MOVE "BOM1411" TO TK-EXPECTED-X
     MOVE BA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The refusal names the reason" TO TK-NAME
     IF BB-REFUSAL NOT = SPACES
         PERFORM TK-PASS
     ELSE
         PERFORM TK-FAIL
     END-IF
     MOVE "All seven outcomes can be paged back" TO TK-NAME
     MOVE 7 TO TK-EXPECTED-N
     MOVE BB-WIN-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> REQ: BOM-004, BOM-024.  The B1 frame bill is RELEASED, so the
*> change lands on a new draft revision B and revision A is left
*> exactly as it was.
 Released-Gets-A-New-Revision.
     PERFORM Set-Up-Change
     MOVE "TBOMCHG-REPL-FRAME" TO BB-CHANGE-REF
     MOVE "REPLACE" TO BB-ACTION
     MOVE "BOMREF" TO BB-SCOPE
     MOVE "BOM-DEMO-FRAME" TO BB-SCOPE-ITEM
     MOVE "DEMO-PIN" TO BB-OLD-ITEM
     MOVE "DEMO-PIN-HD" TO BB-NEW-ITEM
     MOVE "PREVIEW" TO BA-FUNCTION
     CALL "BOMBULK" USING Ws-Bulk
     MOVE "One named bill is one target" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE BB-TARGET-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The released frame bill is treated as a new revision"
         TO TK-NAME
     MOVE "NEWREV" TO TK-EXPECTED-X
     MOVE BB-L-TREATMENT(1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The released revision A is the source, not the target"
         TO TK-NAME
     MOVE "A" TO TK-EXPECTED-X
     MOVE BB-L-BOM-REV(1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The proposal is the next free revision B" TO TK-NAME
     MOVE "B" TO TK-EXPECTED-X
     MOVE BB-L-NEW-REV(1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The pin line of the B1 frame is line thirty"
         TO TK-NAME
     MOVE 30 TO TK-EXPECTED-N
     MOVE BB-L-LINE-NUM(1) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "One pin per frame before the change" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE BB-L-OLD-QTY(1) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A replacement keeps the quantity it found" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE BB-L-NEW-QTY(1) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A valid proposal is pending, not applied" TO TK-NAME
     MOVE "PENDING" TO TK-EXPECTED-X
     MOVE BB-L-OUTCOME(1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The frame proposal may be applied" TO TK-NAME
     MOVE "Y" TO TK-EXPECTED-X
     MOVE BB-MAY-APPLY TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> REQ: BOM-013, BOM-024, BOM-029.  B6: the frame is above the
*> brace, so putting the frame inside the brace closes the loop.
*> The refusal must name the path, not just say "invalid".
 Cycle-Is-Refused.
     PERFORM Set-Up-Change
     MOVE "TBOMCHG-CYCLE" TO BB-CHANGE-REF
     MOVE "REPLACE" TO BB-ACTION
     MOVE "BOMREF" TO BB-SCOPE
     MOVE "BOM-DEMO-BRACE" TO BB-SCOPE-ITEM
     MOVE "DEMO-PIN" TO BB-OLD-ITEM
     MOVE "DEMO-FRAME" TO BB-NEW-ITEM
     MOVE "PREVIEW" TO BA-FUNCTION
     CALL "BOMBULK" USING Ws-Bulk
     MOVE "A replacement that closes a loop fails" TO TK-NAME
     MOVE "FAILED" TO TK-EXPECTED-X
     MOVE BB-L-OUTCOME(1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The cycle refusal carries reason BOM1434" TO TK-NAME
     MOVE "BOM1434" TO TK-EXPECTED-X
     MOVE BB-L-REASON(1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The cycle refusal names the offending path" TO TK-NAME
     IF BB-L-DETAIL(1)(1:5) = "Cycle"
         PERFORM TK-PASS
     ELSE
         MOVE BB-L-DETAIL(1) TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     MOVE "A cycle refuses the run" TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE BB-MAY-APPLY TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The run reports return code eight" TO TK-NAME
     MOVE 8 TO TK-EXPECTED-N
     MOVE BA-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> REQ: BOM-010, BOM-029.  A replacement that names an item the
*> stock ledger has never heard of refuses; it does not default.
 Unknown-Item-Is-Refused.
     PERFORM Set-Up-Change
     MOVE "TBOMCHG-BADITEM" TO BB-CHANGE-REF
     MOVE "REPLACE" TO BB-ACTION
     MOVE "BOMREF" TO BB-SCOPE
     MOVE "BOM-DEMO-FRAME" TO BB-SCOPE-ITEM
     MOVE "DEMO-PIN" TO BB-OLD-ITEM
     MOVE "DEMO-NOT-AN-ITEM" TO BB-NEW-ITEM
     MOVE "PREVIEW" TO BA-FUNCTION
     CALL "BOMBULK" USING Ws-Bulk
     MOVE "An unknown replacement item fails" TO TK-NAME
     MOVE "BOM1435" TO TK-EXPECTED-X
     MOVE BB-L-REASON(1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "An unknown replacement item changes nothing"
         TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE BB-APPLIED-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> REQ: BOM-024.  A target that does not carry the component is
*> skipped and said to be skipped - it is never counted as changed
*> and it never fails the run for the others.
 Nothing-To-Change-Is-Skipped.
     PERFORM Set-Up-Change
     MOVE "TBOMCHG-NOLINE" TO BB-CHANGE-REF
     MOVE "REPLACE" TO BB-ACTION
     MOVE "BOMREF" TO BB-SCOPE
     MOVE "BOM-DEMO-B3" TO BB-SCOPE-ITEM
     MOVE "DEMO-PIN" TO BB-OLD-ITEM
     MOVE "DEMO-PIN-HD" TO BB-NEW-ITEM
     MOVE "PREVIEW" TO BA-FUNCTION
     CALL "BOMBULK" USING Ws-Bulk
     MOVE "A bill without the component is skipped" TO TK-NAME
     MOVE "SKIPPED" TO TK-EXPECTED-X
     MOVE BB-L-OUTCOME(1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A skipped bill carries reason BOM1431" TO TK-NAME
     MOVE "BOM1431" TO TK-EXPECTED-X
     MOVE BB-L-REASON(1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A skipped bill is not a failure" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE BB-FAILED-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A skipped bill is counted as skipped" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE BB-SKIPPED-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> REQ: BOM-006, BOM-009, BOM-024.  B3 is written per ten parents
*> and asks for 250 grams with a four per cent allowance.  The
*> preview must show the published 250 before and the proposed
*> 500 after, on a new revision because B3 is released.
 B3-Quantity-Change-Preview.
     PERFORM Set-Up-Change
     MOVE "TBOMCHG-QTY" TO BB-CHANGE-REF
     MOVE "QTYCHG" TO BB-ACTION
     MOVE "BOMREF" TO BB-SCOPE
     MOVE "BOM-DEMO-B3" TO BB-SCOPE-ITEM
     MOVE "DEMO-B3-GRAN" TO BB-OLD-ITEM
     MOVE 500 TO BB-NEW-QTY
     MOVE "PREVIEW" TO BA-FUNCTION
     CALL "BOMBULK" USING Ws-Bulk
     MOVE "B3 uses 250 grams per basis before the change"
         TO TK-NAME
     MOVE 250 TO TK-EXPECTED-N
     MOVE BB-L-OLD-QTY(1) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "B3 would use 500 grams per basis after the change"
         TO TK-NAME
     MOVE 500 TO TK-EXPECTED-N
     MOVE BB-L-NEW-QTY(1) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The B3 quantity change is a new revision" TO TK-NAME
     MOVE "NEWREV" TO TK-EXPECTED-X
     MOVE BB-L-TREATMENT(1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The B3 quantity change validates" TO TK-NAME
     MOVE "PENDING" TO TK-EXPECTED-X
     MOVE BB-L-OUTCOME(1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> REQ: BOM-002, BOM-024.  B5 has two independent 1.2 piece
*> occurrences of the same part.  A bulk removal takes the first
*> occurrence only and says the others are still there, because
*> repeated component lines stay distinguishable.
 B5-Remove-First-Occurrence.
     PERFORM Set-Up-Change
     MOVE "TBOMCHG-REMOVE" TO BB-CHANGE-REF
     MOVE "REMOVE" TO BB-ACTION
     MOVE "BOMREF" TO BB-SCOPE
     MOVE "BOM-DEMO-B5" TO BB-SCOPE-ITEM
     MOVE "DEMO-B5-PART" TO BB-OLD-ITEM
     MOVE "PREVIEW" TO BA-FUNCTION
     CALL "BOMBULK" USING Ws-Bulk
     MOVE "B5 removal takes the first occurrence, line ten"
         TO TK-NAME
     MOVE 10 TO TK-EXPECTED-N
     MOVE BB-L-LINE-NUM(1) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "B5 each occurrence is 1.2 pieces" TO TK-NAME
     MOVE 1.2 TO TK-EXPECTED-N
     MOVE BB-L-OLD-QTY(1) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A removed line leaves nothing behind" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE BB-L-NEW-QTY(1) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The second occurrence is reported, not removed"
         TO TK-NAME
     IF BB-L-DETAIL(1) NOT = SPACES
         PERFORM TK-PASS
     ELSE
         PERFORM TK-FAIL
     END-IF
     MOVE "The B5 removal validates" TO TK-NAME
     MOVE "PENDING" TO TK-EXPECTED-X
     MOVE BB-L-OUTCOME(1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> REQ: BOM-024.  An addition lands on a new line number of its
*> own and keeps the quantity the request asked for.
 B5-Add-A-Component.
     PERFORM Set-Up-Change
     MOVE "TBOMCHG-ADD" TO BB-CHANGE-REF
     MOVE "ADD" TO BB-ACTION
     MOVE "BOMREF" TO BB-SCOPE
     MOVE "BOM-DEMO-B5" TO BB-SCOPE-ITEM
     MOVE "DEMO-PIN" TO BB-NEW-ITEM
     MOVE 2 TO BB-NEW-QTY
     MOVE "EA" TO BB-NEW-UOM
     MOVE "PREVIEW" TO BA-FUNCTION
     CALL "BOMBULK" USING Ws-Bulk
     MOVE "An added component gets the next line number thirty"
         TO TK-NAME
     MOVE 30 TO TK-EXPECTED-N
     MOVE BB-L-LINE-NUM(1) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "An added component uses the requested quantity"
         TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     MOVE BB-L-NEW-QTY(1) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The B5 addition validates" TO TK-NAME
     MOVE "PENDING" TO TK-EXPECTED-X
     MOVE BB-L-OUTCOME(1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> REQ: BOM-004, BOM-028, BOM-030.  The only test that keeps its
*> work.  It applies one quantity change, proves the released
*> original is still the released original, and then applies the
*> same change reference again to prove the second attempt does
*> not raise a second revision.
 Apply-Then-Apply-Again.
     PERFORM Set-Up-Change
     MOVE "TBOMCHG-APPLY" TO BB-CHANGE-REF
     MOVE "Double the B3 granulate" TO BB-TITLE
     MOVE "QTYCHG" TO BB-ACTION
     MOVE "BOMREF" TO BB-SCOPE
     MOVE "BOM-DEMO-B3" TO BB-SCOPE-ITEM
     MOVE "DEMO-B3-GRAN" TO BB-OLD-ITEM
     MOVE 500 TO BB-NEW-QTY
     MOVE "APPLY" TO BA-FUNCTION
     CALL "BOMBULK" USING Ws-Bulk
     MOVE "An applied bulk change reports APPLIED" TO TK-NAME
     MOVE "APPLIED" TO TK-EXPECTED-X
     MOVE BB-STATUS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "One bill was changed" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE BB-APPLIED-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "It was changed by raising a new revision" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE BB-NEWREV-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The new revision has its own identity" TO TK-NAME
     IF BB-L-NEW-BOM-ID(1) > 0
        AND BB-L-NEW-BOM-ID(1) NOT = BB-L-BOM-ID(1)
         PERFORM TK-PASS
     ELSE
         PERFORM TK-FAIL
     END-IF
     PERFORM Repeat-The-Same-Apply
     PERFORM Released-Original-Is-Intact.

 Repeat-The-Same-Apply.
     PERFORM Set-Up-Change
     MOVE "TBOMCHG-APPLY" TO BB-CHANGE-REF
     MOVE "QTYCHG" TO BB-ACTION
     MOVE "BOMREF" TO BB-SCOPE
     MOVE "BOM-DEMO-B3" TO BB-SCOPE-ITEM
     MOVE "DEMO-B3-GRAN" TO BB-OLD-ITEM
     MOVE 500 TO BB-NEW-QTY
     MOVE "APPLY" TO BA-FUNCTION
     CALL "BOMBULK" USING Ws-Bulk
     MOVE "Repeating the same change is recognised" TO TK-NAME
     MOVE "Y" TO TK-EXPECTED-X
     MOVE BB-DUPLICATE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A repeat returns the first outcome as a warning"
         TO TK-NAME
     MOVE 4 TO TK-EXPECTED-N
     MOVE BA-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A repeat is reported as BOM1410" TO TK-NAME
     MOVE "BOM1410" TO TK-EXPECTED-X
     MOVE BA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "A repeat still reports the one bill it changed"
         TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE BB-APPLIED-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*> The released revision A must still say 250 grams; only the new
*> draft revision B carries the 500 (BOM-004, BOM-024).
 Released-Original-Is-Intact.
     PERFORM Set-Up-Change
     MOVE "TBOMCHG-AFTER" TO BB-CHANGE-REF
     MOVE "QTYCHG" TO BB-ACTION
     MOVE "BOMREF" TO BB-SCOPE
     MOVE "BOM-DEMO-B3" TO BB-SCOPE-ITEM
     MOVE "DEMO-B3-GRAN" TO BB-OLD-ITEM
     MOVE 250 TO BB-NEW-QTY
     MOVE "PREVIEW" TO BA-FUNCTION
     CALL "BOMBULK" USING Ws-Bulk
     MOVE "The applied change left a second revision behind"
         TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     MOVE BB-TARGET-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A" TO TK-EXPECTED-X
     PERFORM Find-The-Line-For-Rev
     MOVE "The released revision A still asks for 250 grams"
         TO TK-NAME
     MOVE 0 TO Ws-A-Qty
     IF Ws-Hit > 0
         MOVE BB-L-OLD-QTY(Ws-Hit) TO Ws-A-Qty
     END-IF
     MOVE 250 TO TK-EXPECTED-N
     MOVE Ws-A-Qty TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "B" TO TK-EXPECTED-X
     PERFORM Find-The-Line-For-Rev
     MOVE "The new draft revision B carries the 500 grams"
         TO TK-NAME
     MOVE 0 TO Ws-A-Qty
     IF Ws-Hit > 0
         MOVE BB-L-OLD-QTY(Ws-Hit) TO Ws-A-Qty
     END-IF
     MOVE 500 TO TK-EXPECTED-N
     MOVE Ws-A-Qty TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A draft revision is amended in place, not copied"
         TO TK-NAME
     MOVE "AMEND" TO TK-EXPECTED-X
     MOVE SPACES TO TK-ACTUAL-X
     IF Ws-Hit > 0
         MOVE BB-L-TREATMENT(Ws-Hit) TO TK-ACTUAL-X
     END-IF
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> Everything this suite wrote carries the TBOMCHG- prefix, so it
*> can be taken out again without touching the seeded bills.
 Tidy-Up.
     MOVE "DELETE FROM bom.BomValidationFinding WHERE ValidationId IN (SELECT ValidationId FROM bom.BomValidation WHERE ValidationRef LIKE 'TBOMCHG-%')"
         TO SQLIO-STMT
     PERFORM Run-Clean-Up
     MOVE "UPDATE bom.BomHeader SET ValidationId = NULL WHERE ChangeRef LIKE 'TBOMCHG-%'"
         TO SQLIO-STMT
     PERFORM Run-Clean-Up
     MOVE "DELETE FROM bom.BomValidation WHERE ValidationRef LIKE 'TBOMCHG-%'"
         TO SQLIO-STMT
     PERFORM Run-Clean-Up
     MOVE "DELETE FROM bom.BomComponent WHERE BomHeaderId IN (SELECT BomHeaderId FROM bom.BomHeader WHERE ChangeRef LIKE 'TBOMCHG-%')"
         TO SQLIO-STMT
     PERFORM Run-Clean-Up
     MOVE "DELETE FROM bom.BomHeader WHERE ChangeRef LIKE 'TBOMCHG-%'"
         TO SQLIO-STMT
     PERFORM Run-Clean-Up
     MOVE "DELETE FROM bom.BomChangeImpact WHERE ChangeRef LIKE 'TBOMCHG-%'"
         TO SQLIO-STMT
     PERFORM Run-Clean-Up
     MOVE "DELETE FROM bom.BomChangeLine WHERE ChangeId IN (SELECT ChangeId FROM bom.BomChange WHERE ChangeRef LIKE 'TBOMCHG-%')"
         TO SQLIO-STMT
     PERFORM Run-Clean-Up
     MOVE "DELETE FROM bom.BomChange WHERE ChangeRef LIKE 'TBOMCHG-%'"
         TO SQLIO-STMT
     PERFORM Run-Clean-Up
     MOVE "The suite leaves the seeded bills as it found them"
         TO TK-NAME
     MOVE "BOM-DEMO-B3" TO TK-EXPECTED-X
     PERFORM Count-Leftovers
     IF Ws-Hit = 0
         PERFORM TK-PASS
     ELSE
         MOVE "a TBOMCHG revision was left behind" TO TK-DETAIL
         PERFORM TK-FAIL
     END-IF.

 Run-Clean-Up.
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 3 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     MOVE 3 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row.

 Count-Leftovers.
     MOVE 0 TO Ws-Hit
     MOVE "SELECT COUNT(*) FROM bom.BomHeader WHERE ChangeRef LIKE 'TBOMCHG-%'"
         TO SQLIO-STMT
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 60 TO SQLIO-COL-WIDTH(1)
     MOVE 3 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-OK
         COMPUTE Ws-Hit = FUNCTION NUMVAL(Ws-Col(1))
     END-IF.

 COPY TESTKITP.
