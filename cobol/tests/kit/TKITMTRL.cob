>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : TKITMTRL
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION (TESTS)
*> PURPOSE    : MULTITRL RUN against the real demonstration bills
*>              and live stock - the half of AC-078 that TKITTRL
*>              (database free) cannot reach.
*>
*>              Live balances move whenever the demonstration
*>              drivers run, so nothing here asserts a stock
*>              figure.  It asserts what must hold for ANY stock:
*>              every stocked leaf of the bill is pooled (also the
*>              ones beyond the first 25-line window), services are
*>              not, the pool balances, a kit is one claimant
*>              however often it uses a part, the higher priority
*>              kit is served first whatever the entry order, the
*>              same units are never given to two kits, and an
*>              incomplete bill is never called complete.
*>
*>              Before this suite MULTITRL called EXPLODE in a loop,
*>              never read a line back, pooled nothing and reported
*>              every kit complete - and no test noticed because no
*>              test ever called RUN.
*> REQ: KIT-007
*> AC : AC-078
*>================================================================
 IDENTIFICATION DIVISION.
 PROGRAM-ID. TKITMTRL.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY TESTKITW.
 COPY SQLIOCA.
 COPY MULTITRLCA REPLACING ==DFHCOMMAREA== BY ==Ws-Multi-Ca==
      LEADING ==CA-== BY ==MA-==.
 COPY KITTRIALCA REPLACING ==DFHCOMMAREA== BY ==Ws-Trial-Ca==
      LEADING ==CA-== BY ==TR-==.
 01  Ws-Probe                    PIC X(30).
 01  Ws-Work.
     05  Ws-Db-Up                PIC X VALUE "Y".
     05  Ws-I                    PIC S9(4) VALUE 0.
     05  Ws-Add-Ref              PIC X(30) VALUE SPACES.
     05  Ws-Add-Parent           PIC X(40) VALUE SPACES.
     05  Ws-Add-Qty              PIC S9(13)V9(6) VALUE 0.
     05  Ws-Add-Prio             PIC 9(2) VALUE 0.
     05  Ws-Run-Rc               PIC S9(4) VALUE 0.
     05  Ws-Run-Conflicts        PIC S9(4) VALUE 0.
     05  Ws-Run-Comps            PIC S9(4) VALUE 0.
     05  Ws-Want-Ref             PIC X(30) VALUE SPACES.
     05  Ws-Verdict              PIC X(12) VALUE SPACES.
     05  Ws-Limiting             PIC X(40) VALUE SPACES.
     05  Ws-Solo-Verdict         PIC X(12) VALUE SPACES.
     05  Ws-Want-Item            PIC X(40) VALUE SPACES.
     05  Ws-Item-Claims          PIC S9(4) VALUE 0.
     05  Ws-Seen                 PIC S9(4) VALUE 0.
     05  Ws-Svc-Seen             PIC S9(4) VALUE 0.
     05  Ws-Unbalanced           PIC S9(4) VALUE 0.
     05  Ws-Over-Given           PIC S9(4) VALUE 0.
     05  Ws-Short-Comps          PIC S9(4) VALUE 0.
     05  Ws-Max-Claims           PIC S9(4) VALUE 0.
     05  Ws-Bad-Item             PIC X(40) VALUE SPACES.
     05  Ws-Guard                PIC S9(4) VALUE 0.
 PROCEDURE DIVISION.
 Main-Entry.
     MOVE "TKITMTRL multi-kit trial run" TO TK-SUITE
     PERFORM TK-BEGIN
     PERFORM Probe-The-Database
     IF Ws-Db-Up = "N"
         MOVE "Multi-kit trial against the demonstration bills"
              TO TK-NAME
         PERFORM TK-SKIP
     ELSE
         PERFORM Test-Every-Stocked-Leaf-Is-Pooled
         PERFORM Test-Priority-Decides-Who-Is-Served
         PERFORM Test-Incomplete-Bill-Is-Not-Complete
         PERFORM Test-Single-Trial-Reads-The-Requirement
     END-IF
     PERFORM TK-END
     STOP RUN.

*>----------------------------------------------------------------
 Probe-The-Database.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(DISTINCT"
            " ParentItemCode)) FROM bom.BomHeader WHERE"
            " CompanyCode = 'STC' AND Status = 'RELEASED' AND"
            " Purpose = 'STANDARD' AND ParentItemCode IN"
            " ('DEMO-FRAME','FG-BIKE-COMMUTE-M','DEMO-ERR-TOP')"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 30 TO SQLIO-COL-WIDTH (1)
     MOVE 8 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     MOVE SPACES TO Ws-Probe
     CALL "SQLIO" USING SQLIO-CA, Ws-Probe
     IF SQLIO-ERROR
         MOVE "N" TO Ws-Db-Up
         MOVE "no database connection" TO TK-DETAIL
         EXIT PARAGRAPH
     END-IF
     IF FUNCTION NUMVAL (Ws-Probe) < 3
         MOVE "N" TO Ws-Db-Up
         MOVE "the demonstration bills are not seeded" TO TK-DETAIL
     END-IF.

*>----------------------------------------------------------------
 Prime-Multi.
     INITIALIZE Ws-Multi-Ca
     MOVE "STC" TO MA-COMPANY
     MOVE "RDG" TO MA-SITE
     MOVE "TESTER" TO MA-USER
     MOVE 20261007 TO MA-BUSINESS-DATE
     MOVE "RDG" TO MT-SITE
     MOVE 20261007 TO MT-AS-AT
     MOVE "N" TO MT-USE-INBOUND.

 Clear-Multi.
     PERFORM Prime-Multi
     MOVE "CLEAR" TO MA-FUNCTION
     CALL "MULTITRL" USING Ws-Multi-Ca.

 Add-Kit.
     PERFORM Prime-Multi
     MOVE "ADDKIT" TO MA-FUNCTION
     MOVE Ws-Add-Ref TO MT-A-KIT-REF
     MOVE Ws-Add-Parent TO MT-A-PARENT
     MOVE Ws-Add-Qty TO MT-A-QTY
     MOVE Ws-Add-Prio TO MT-A-PRIORITY
     MOVE 20261130 TO MT-A-REQ-DATE
     CALL "MULTITRL" USING Ws-Multi-Ca.

 Run-Multi.
     PERFORM Prime-Multi
     MOVE "RUN" TO MA-FUNCTION
     CALL "MULTITRL" USING Ws-Multi-Ca
     MOVE MA-RETURN-CODE TO Ws-Run-Rc
     MOVE MT-CONFLICT-CNT TO Ws-Run-Conflicts
     MOVE MT-COMP-CNT TO Ws-Run-Comps
     MOVE "A multi-kit trial run commits nothing" TO TK-NAME
     MOVE "N" TO TK-EXPECTED-X
     MOVE MT-COMMITTED TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>   Ws-Want-Ref in, Ws-Verdict / Ws-Limiting out.
 Find-Verdict.
     MOVE SPACES TO Ws-Verdict Ws-Limiting
     PERFORM Prime-Multi
     MOVE "GETKIT" TO MA-FUNCTION
     MOVE 0 TO MT-KFROM
     CALL "MULTITRL" USING Ws-Multi-Ca
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > MT-KWIN-CNT
         IF MT-K-KIT-REF (Ws-I) = Ws-Want-Ref
             MOVE MT-K-VERDICT (Ws-I) TO Ws-Verdict
             MOVE MT-K-LIMITING (Ws-I) TO Ws-Limiting
         END-IF
     END-PERFORM.

*>   Pages every pooled component and checks the bookkeeping.  With
*>   no inbound in play, what a component was asked for is exactly
*>   what it gave plus what it fell short by, and it never gave
*>   more than was free.  Ws-Want-Item's claim count comes back too.
 Scan-Components.
     MOVE 0 TO Ws-Seen Ws-Svc-Seen Ws-Unbalanced Ws-Over-Given
     MOVE 0 TO Ws-Short-Comps Ws-Max-Claims Ws-Item-Claims Ws-Guard
     MOVE SPACES TO Ws-Bad-Item
     PERFORM Prime-Multi
     MOVE "GETCOMP" TO MA-FUNCTION
     MOVE 0 TO MT-CFROM
     MOVE "Y" TO MT-CMORE
     PERFORM UNTIL MT-CMORE NOT = "Y" OR Ws-Guard > 50
         ADD 1 TO Ws-Guard
         CALL "MULTITRL" USING Ws-Multi-Ca
         PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > MT-CWIN-CNT
             PERFORM Check-One-Component
         END-PERFORM
         IF MT-CWIN-CNT = 0
             EXIT PERFORM
         END-IF
         ADD MT-CWIN-CNT TO MT-CFROM
     END-PERFORM.

 Check-One-Component.
     ADD 1 TO Ws-Seen
     IF MT-C-ITEM (Ws-I) (1:4) = "SVC-"
         ADD 1 TO Ws-Svc-Seen
     END-IF
     IF MT-C-ALLOCATED (Ws-I) + MT-C-SHORTAGE (Ws-I)
        NOT = MT-C-DEMAND (Ws-I)
         ADD 1 TO Ws-Unbalanced
         MOVE MT-C-ITEM (Ws-I) TO Ws-Bad-Item
     END-IF
     IF MT-C-ALLOCATED (Ws-I) > MT-C-FREE (Ws-I)
         ADD 1 TO Ws-Over-Given
         MOVE MT-C-ITEM (Ws-I) TO Ws-Bad-Item
     END-IF
     IF MT-C-SHORTAGE (Ws-I) > 0
         ADD 1 TO Ws-Short-Comps
     END-IF
     IF MT-C-CLAIMS (Ws-I) > Ws-Max-Claims
         MOVE MT-C-CLAIMS (Ws-I) TO Ws-Max-Claims
     END-IF
     IF MT-C-ITEM (Ws-I) = Ws-Want-Item
         MOVE MT-C-CLAIMS (Ws-I) TO Ws-Item-Claims
     END-IF.

 Assert-Pool-Balances.
     MOVE "Every pooled component gave what was asked or fell short by the rest"
          TO TK-NAME
     MOVE Ws-Bad-Item TO TK-DETAIL
     MOVE 0 TO TK-EXPECTED-N
     MOVE Ws-Unbalanced TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "No component gave more than was free" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE Ws-Over-Given TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> The commuter bicycle has fifty-nine distinct stocked parts over
*> five levels and three phantoms, three subcontract or labour
*> services and a note.  The TIG wire is used in six places.
 Test-Every-Stocked-Leaf-Is-Pooled.
     PERFORM Clear-Multi
     MOVE "BIKE1" TO Ws-Add-Ref
     MOVE "FG-BIKE-COMMUTE-M" TO Ws-Add-Parent
     MOVE 1 TO Ws-Add-Qty
     MOVE 1 TO Ws-Add-Prio
     PERFORM Add-Kit
     PERFORM Run-Multi
     MOVE "The bicycle trial runs without an error" TO TK-NAME
     IF Ws-Run-Rc > 4
         MOVE MA-MESSAGE (1:200) TO TK-DETAIL
         PERFORM TK-FAIL
     ELSE
         PERFORM TK-PASS
     END-IF
     MOVE "Components beyond the first 25-line window are pooled"
          TO TK-NAME
     IF Ws-Run-Comps > 25
         PERFORM TK-PASS
     ELSE
         MOVE SPACES TO TK-DETAIL
         MOVE Ws-Run-Comps TO TK-NUM-ED
         STRING "only " FUNCTION TRIM(TK-NUM-ED) " components pooled"
             DELIMITED BY SIZE INTO TK-DETAIL
         PERFORM TK-FAIL
     END-IF
     MOVE "WIRE-TIG-4043-5KG" TO Ws-Want-Item
     PERFORM Scan-Components
     MOVE "GETCOMP pages every pooled component" TO TK-NAME
     MOVE Ws-Run-Comps TO TK-EXPECTED-N
     MOVE Ws-Seen TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "Services are not drawn from the stock pool" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE Ws-Svc-Seen TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Assert-Pool-Balances
     MOVE "A part used in six places is one claim by one kit"
          TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE Ws-Item-Claims TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "One kit on its own is never in conflict" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE Ws-Run-Conflicts TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "BIKE1" TO Ws-Want-Ref
     PERFORM Find-Verdict
     MOVE "The verdict agrees with the component shortages" TO TK-NAME
     IF (Ws-Short-Comps > 0 AND Ws-Verdict = "SHORT"
            AND Ws-Limiting NOT = SPACES)
        OR (Ws-Short-Comps = 0 AND Ws-Verdict = "COMPLETE")
         PERFORM TK-PASS
     ELSE
         MOVE SPACES TO TK-DETAIL
         MOVE Ws-Short-Comps TO TK-NUM-ED
         STRING "verdict " FUNCTION TRIM(Ws-Verdict) " with "
                FUNCTION TRIM(TK-NUM-ED) " short components"
             DELIMITED BY SIZE INTO TK-DETAIL
         PERFORM TK-FAIL
     END-IF.

*>----------------------------------------------------------------
*> AC-078 on live stock.  SMALL is entered SECOND in both runs; only
*> the priority changes.  Served first, SMALL gets exactly what it
*> would get on its own.  Served after a kit for a million frames,
*> it gets nothing, because those units have already been promised.
 Test-Priority-Decides-Who-Is-Served.
     PERFORM Clear-Multi
     MOVE "SOLO" TO Ws-Add-Ref
     MOVE "DEMO-FRAME" TO Ws-Add-Parent
     MOVE 1 TO Ws-Add-Qty
     MOVE 1 TO Ws-Add-Prio
     PERFORM Add-Kit
     PERFORM Run-Multi
     MOVE "SOLO" TO Ws-Want-Ref
     PERFORM Find-Verdict
     MOVE Ws-Verdict TO Ws-Solo-Verdict
     MOVE "DEMO-PIN" TO Ws-Want-Item
     PERFORM Scan-Components
     MOVE "The pin in frame, brace and pack is still one claim"
          TO TK-NAME
     MOVE 1 TO TK-EXPECTED-N
     MOVE Ws-Item-Claims TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM

     PERFORM Clear-Multi
     MOVE "HUGE" TO Ws-Add-Ref
     MOVE 1000000 TO Ws-Add-Qty
     MOVE 9 TO Ws-Add-Prio
     PERFORM Add-Kit
     MOVE "SMALL" TO Ws-Add-Ref
     MOVE 1 TO Ws-Add-Qty
     MOVE 1 TO Ws-Add-Prio
     PERFORM Add-Kit
     PERFORM Run-Multi
     MOVE "SMALL" TO Ws-Want-Ref
     PERFORM Find-Verdict
     MOVE "Served first, the small kit fares exactly as it does alone"
          TO TK-NAME
     MOVE Ws-Solo-Verdict TO TK-EXPECTED-X
     MOVE Ws-Verdict TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "HUGE" TO Ws-Want-Ref
     PERFORM Find-Verdict
     MOVE "A million frames cannot be covered" TO TK-NAME
     MOVE "SHORT" TO TK-EXPECTED-X
     MOVE Ws-Verdict TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "Competing kits that run short are reported as a conflict"
          TO TK-NAME
     IF Ws-Run-Conflicts > 0
         PERFORM TK-PASS
     ELSE
         PERFORM TK-FAIL
     END-IF
     MOVE SPACES TO Ws-Want-Item
     PERFORM Scan-Components
     PERFORM Assert-Pool-Balances
     MOVE "Both kits are counted as claimants of the shared parts"
          TO TK-NAME
     MOVE 2 TO TK-EXPECTED-N
     MOVE Ws-Max-Claims TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM

     PERFORM Clear-Multi
     MOVE "HUGE" TO Ws-Add-Ref
     MOVE 1000000 TO Ws-Add-Qty
     MOVE 1 TO Ws-Add-Prio
     PERFORM Add-Kit
     MOVE "SMALL" TO Ws-Add-Ref
     MOVE 1 TO Ws-Add-Qty
     MOVE 9 TO Ws-Add-Prio
     PERFORM Add-Kit
     PERFORM Run-Multi
     MOVE "SMALL" TO Ws-Want-Ref
     PERFORM Find-Verdict
     MOVE "Served after the big kit, the same units are not promised again"
          TO TK-NAME
     MOVE "SHORT" TO TK-EXPECTED-X
     MOVE Ws-Verdict TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR.

*>----------------------------------------------------------------
*> DEMO-ERR-TOP's manufactured child has no released bill (BOM-029).
 Test-Incomplete-Bill-Is-Not-Complete.
     PERFORM Clear-Multi
     MOVE "ERR" TO Ws-Add-Ref
     MOVE "DEMO-ERR-TOP" TO Ws-Add-Parent
     MOVE 1 TO Ws-Add-Qty
     MOVE 1 TO Ws-Add-Prio
     PERFORM Add-Kit
     PERFORM Run-Multi
     MOVE "ERR" TO Ws-Want-Ref
     PERFORM Find-Verdict
     MOVE "A kit whose bill is incomplete is never called complete"
          TO TK-NAME
     MOVE "INCOMPLETE" TO TK-EXPECTED-X
     MOVE Ws-Verdict TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     PERFORM Clear-Multi.

*>----------------------------------------------------------------
*> KITTRIAL had the same fault as MULTITRL: it netted each leaf's
*> gross STARTS (zero on a leaf) instead of its requirement, so
*> every bill was feasible.  Nothing for the cake is ever stocked by
*> the seeds, so a cake cannot be feasible; and a service on the
*> bicycle is bought in, never a stock shortage.
 Prime-Trial.
     INITIALIZE Ws-Trial-Ca
     MOVE "STC" TO TR-COMPANY
     MOVE "RDG" TO TR-SITE
     MOVE "TESTER" TO TR-USER
     MOVE 20261007 TO TR-BUSINESS-DATE
     MOVE "RDG" TO KT-SITE
     MOVE 1 TO KT-REQUESTED-QTY
     MOVE 20261020 TO KT-REQ-DATE
     MOVE 20261007 TO KT-SELECTION-DATE
     MOVE "STOCK" TO KT-DEMAND-TYPE
     MOVE "N" TO KT-USE-SUBASSY
     MOVE "N" TO KT-USE-INBOUND
     MOVE "TRIAL" TO TR-FUNCTION.

 Test-Single-Trial-Reads-The-Requirement.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM"
            " inv.StockBalance WHERE CompanyCode = 'STC' AND"
            " SiteCode = 'RDG' AND QtyOnHand > 0 AND ItemCode IN"
            " ('ING-FLOUR-SR','ING-EGG-FREERANGE','ING-SUGAR-ICING')"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 0 TO SQLIO-PARM-CNT
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 30 TO SQLIO-COL-WIDTH (1)
     MOVE 8 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     MOVE SPACES TO Ws-Probe
     CALL "SQLIO" USING SQLIO-CA, Ws-Probe
     PERFORM Prime-Trial
     MOVE "FG-CAKE-CELEB-8IN" TO KT-PARENT-ITEM
     CALL "KITTRIAL" USING Ws-Trial-Ca
     MOVE "A cake with no ingredients in stock is not feasible"
          TO TK-NAME
     IF FUNCTION NUMVAL (Ws-Probe) > 0
         MOVE "cake ingredients have been stocked at RDG" TO TK-DETAIL
         PERFORM TK-SKIP
     ELSE
         MOVE "N" TO TK-EXPECTED-X
         MOVE KT-FEASIBLE TO TK-ACTUAL-X
         PERFORM TK-ASSERT-STR
         MOVE "The trial names the limiting ingredient" TO TK-NAME
         IF KT-LIMITING-ITEM NOT = SPACES
             PERFORM TK-PASS
         ELSE
             PERFORM TK-FAIL
         END-IF
     END-IF
     PERFORM Prime-Trial
     MOVE "FG-BIKE-COMMUTE-M" TO KT-PARENT-ITEM
     CALL "KITTRIAL" USING Ws-Trial-Ca
     MOVE 0 TO Ws-Svc-Seen Ws-Seen Ws-Guard
     MOVE "GETLINE" TO TR-FUNCTION
     MOVE 0 TO KT-FROM-LINE
     MOVE 20 TO KT-WIN-MAX
     MOVE "Y" TO KT-MORE-LINES
     PERFORM UNTIL KT-MORE-LINES NOT = "Y" OR Ws-Guard > 50
         ADD 1 TO Ws-Guard
         CALL "KITTRIAL" USING Ws-Trial-Ca
         PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > KT-WIN-CNT
             ADD 1 TO Ws-Seen
             IF KT-L-ITEM (Ws-I) (1:4) = "SVC-"
                 ADD 1 TO Ws-Svc-Seen
             END-IF
         END-PERFORM
         IF KT-WIN-CNT = 0
             EXIT PERFORM
         END-IF
         ADD KT-WIN-CNT TO KT-FROM-LINE
     END-PERFORM
     MOVE "The bicycle trial assesses its stocked parts" TO TK-NAME
     IF Ws-Seen > 25
         PERFORM TK-PASS
     ELSE
         PERFORM TK-FAIL
     END-IF
     MOVE "Bought-in services are not assessed against stock"
          TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE Ws-Svc-Seen TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.
 COPY TESTKITP.
