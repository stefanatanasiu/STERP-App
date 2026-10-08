>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : TKITTRL
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION (TESTS)
*> PURPOSE    : Unit tests for the non-committing half of the kit
*>              engine: KITTRIAL (KIT-006) and the scenario
*>              bookkeeping and priority sequencing of MULTITRL
*>              (KIT-007).
*>
*>              Everything asserted here runs WITHOUT a database,
*>              which is itself part of the point: a trial that
*>              needed to write something would not be a trial.
*>              The stock-dependent half of AC-078 (two kits, four
*>              pins each, six free) is covered by the SQL tests
*>              and by the TKITEXEC suite once demo data exists.
*> REQ: KIT-006, KIT-007
*> AC : AC-078
*>================================================================
 IDENTIFICATION DIVISION.
 PROGRAM-ID. TKITTRL.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY TESTKITW.
 COPY KITTRIALCA REPLACING ==DFHCOMMAREA== BY ==Ws-Trial-Ca==
      LEADING ==CA-== BY ==TA-==.
 COPY MULTITRLCA REPLACING ==DFHCOMMAREA== BY ==Ws-Multi-Ca==
      LEADING ==CA-== BY ==MA-==.
 01  Ws-Work.
     05  Ws-I                    PIC S9(4) VALUE 0.
 PROCEDURE DIVISION.
 Main-Entry.
     MOVE "TKITTRL" TO TK-SUITE
     PERFORM TK-BEGIN
     PERFORM Test-Trial-Never-Commits
     PERFORM Test-Trial-Rejects-Nonsense
     PERFORM Test-Multi-Scenario-Bookkeeping
     PERFORM Test-Multi-Priority-Sequence
     PERFORM Test-Multi-Table-Limit
     PERFORM TK-END
     GOBACK.

 Prime-Trial.
     INITIALIZE Ws-Trial-Ca
     MOVE "STC" TO TA-COMPANY
     MOVE "RDG" TO TA-SITE
     MOVE "TESTER" TO TA-USER
     MOVE 20261006 TO TA-BUSINESS-DATE
     MOVE "FRAME-A10" TO KT-PARENT-ITEM
     MOVE "RDG" TO KT-SITE
     MOVE 10 TO KT-REQUESTED-QTY
     MOVE 20261106 TO KT-REQ-DATE
     MOVE "N" TO KT-USE-INBOUND.

*>   KIT-006 / AC-078.  The trial's single promise: whatever it
*>   is asked, and however it answers, it commits nothing.  The
*>   flag is set to the wrong value first so that a program that
*>   simply left the field alone would fail this test.
 Test-Trial-Never-Commits.
     PERFORM Prime-Trial
     MOVE "Y" TO KT-COMMITTED
     MOVE "WIBBLE" TO TA-FUNCTION
     CALL "KITTRIAL" USING Ws-Trial-Ca
     MOVE "KITTRIAL leaves KT-COMMITTED N after an unknown function"
          TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE KT-COMMITTED TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "KITTRIAL refuses an unknown function with KIT0002"
          TO TK-NAME
     MOVE "KIT0002" TO TK-EXPECTED-X
     MOVE TA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "KITTRIAL returns 08 for an unknown function" TO TK-NAME
     MOVE 8 TO TK-EXPECTED-N
     MOVE TA-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Prime-Trial
     MOVE "Y" TO KT-COMMITTED
     MOVE "GETLINE" TO TA-FUNCTION
     CALL "KITTRIAL" USING Ws-Trial-Ca
     MOVE "Paging a trial result still commits nothing" TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE KT-COMMITTED TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>   A trial of nothing is not feasibility information, it is a
*>   keying error, and KIT-003 wants it named as one.
 Test-Trial-Rejects-Nonsense.
     PERFORM Prime-Trial
     MOVE 0 TO KT-REQUESTED-QTY
     MOVE "TRIAL" TO TA-FUNCTION
     CALL "KITTRIAL" USING Ws-Trial-Ca
     MOVE "A zero trial quantity is refused with KIT0003" TO TK-NAME
     MOVE "KIT0003" TO TK-EXPECTED-X
     MOVE TA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Prime-Trial
     MOVE SPACES TO KT-PARENT-ITEM
     MOVE "TRIAL" TO TA-FUNCTION
     CALL "KITTRIAL" USING Ws-Trial-Ca
     MOVE "A trial without a parent item is refused" TO TK-NAME
     MOVE 8 TO TK-EXPECTED-N
     MOVE TA-RETURN-CODE TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Prime-Trial
     MOVE "SAVE" TO TA-FUNCTION
     CALL "KITTRIAL" USING Ws-Trial-Ca
     MOVE "SAVE without a result first is refused with KIT0021"
          TO TK-NAME
     MOVE "KIT0021" TO TK-EXPECTED-X
     MOVE TA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

 Prime-Multi.
     INITIALIZE Ws-Multi-Ca
     MOVE "STC" TO MA-COMPANY
     MOVE "RDG" TO MA-SITE
     MOVE "TESTER" TO MA-USER
     MOVE 20261006 TO MA-BUSINESS-DATE
     MOVE "RDG" TO MT-SITE
     MOVE 20261006 TO MT-AS-AT.

 Clear-Multi.
     PERFORM Prime-Multi
     MOVE "CLEAR" TO MA-FUNCTION
     CALL "MULTITRL" USING Ws-Multi-Ca.

 Add-Kit-B-Then-A.
     PERFORM Clear-Multi
*>   B is added FIRST and A second, so an implementation that
*>   kept entry order would put B in front of A.
     PERFORM Prime-Multi
     MOVE "ADDKIT" TO MA-FUNCTION
     MOVE "KITB" TO MT-A-KIT-REF
     MOVE "FRAME-A10" TO MT-A-PARENT
     MOVE 4 TO MT-A-QTY
     MOVE 9 TO MT-A-PRIORITY
     MOVE 20261120 TO MT-A-REQ-DATE
     CALL "MULTITRL" USING Ws-Multi-Ca
     PERFORM Prime-Multi
     MOVE "ADDKIT" TO MA-FUNCTION
     MOVE "KITA" TO MT-A-KIT-REF
     MOVE "FRAME-A10" TO MT-A-PARENT
     MOVE 4 TO MT-A-QTY
     MOVE 1 TO MT-A-PRIORITY
     MOVE 20261130 TO MT-A-REQ-DATE
     CALL "MULTITRL" USING Ws-Multi-Ca.

 Test-Multi-Scenario-Bookkeeping.
     PERFORM Add-Kit-B-Then-A
     MOVE "MULTITRL holds both competing kits in one scenario"
          TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     MOVE MT-KIT-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "A multi-kit trial commits nothing either" TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE MT-COMMITTED TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Clear-Multi
     MOVE "CLEAR empties the scenario" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE MT-KIT-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Prime-Multi
     MOVE "ADDKIT" TO MA-FUNCTION
     MOVE "KITZ" TO MT-A-KIT-REF
     MOVE "FRAME-A10" TO MT-A-PARENT
     MOVE 0 TO MT-A-QTY
     CALL "MULTITRL" USING Ws-Multi-Ca
     MOVE "A kit for zero units is refused with KIT0003" TO TK-NAME
     MOVE "KIT0003" TO TK-EXPECTED-X
     MOVE MA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>   AC-078, the half that needs no stock: with six pins and two
*>   kits needing four each, which kit is covered is decided by
*>   the explicit priority sequence.  Priority 1 beats priority 9
*>   even though priority 9 was keyed first and wants the pins
*>   ten days earlier.  The run itself then draws the pool down
*>   once, in this order, so B is the one that comes up short.
 Test-Multi-Priority-Sequence.
     PERFORM Add-Kit-B-Then-A
     PERFORM Prime-Multi
     MOVE "GETKIT" TO MA-FUNCTION
     MOVE 0 TO MT-KFROM
     CALL "MULTITRL" USING Ws-Multi-Ca
     MOVE "GETKIT returns both kits" TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     MOVE MT-KWIN-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The higher priority kit is sequenced first" TO TK-NAME
     MOVE "KITA" TO TK-EXPECTED-X
     MOVE MT-K-KIT-REF (1) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The lower priority kit is sequenced second" TO TK-NAME
     MOVE "KITB" TO TK-EXPECTED-X
     MOVE MT-K-KIT-REF (2) TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "Sequence number 1 is the priority 1 kit" TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE MT-K-SEQ (1) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Each competing kit still wants four units" TO TK-NAME
     MOVE 4 TO TK-EXPECTED-N
     MOVE MT-K-QTY (2) TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "The sequence rule is recorded, not implied" TO TK-NAME
     IF MT-ASSUMPTIONS NOT = SPACES
         PERFORM TK-PASS
     ELSE
         PERFORM TK-FAIL
     END-IF.

*>   The OCCURS 12 window is a hard limit a caller must be told
*>   about rather than silently losing the thirteenth kit.
 Test-Multi-Table-Limit.
     PERFORM Clear-Multi
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > 13
         PERFORM Prime-Multi
         MOVE "ADDKIT" TO MA-FUNCTION
         MOVE "FRAME-A10" TO MT-A-PARENT
         MOVE 1 TO MT-A-QTY
         MOVE 5 TO MT-A-PRIORITY
         CALL "MULTITRL" USING Ws-Multi-Ca
     END-PERFORM
     MOVE "The thirteenth kit is refused with KIT0025" TO TK-NAME
     MOVE "KIT0025" TO TK-EXPECTED-X
     MOVE MA-REASON-CODE TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The scenario still holds exactly twelve kits" TO TK-NAME
     MOVE 12 TO TK-EXPECTED-N
     MOVE MT-KIT-CNT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Clear-Multi.
 COPY TESTKITP.
