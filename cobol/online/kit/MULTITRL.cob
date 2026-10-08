>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : MULTITRL
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : Multi-kit trial (KIT-007).  Several competing kits
*>              are trialled against ONE pool of free stock and
*>              expected supply, drawn down in an explicit
*>              priority sequence, so the same units can never be
*>              promised to two kits at once.
*>
*>              Before CR2451 each kit was trialled on its own and
*>              every one of them came back "complete" on the same
*>              six pins.  The shop floor believed all of them.
*>              The pool in this program is the fix: once a unit
*>              is allocated to the higher priority kit it is gone
*>              for everybody else in the same scenario.
*>
*>              Nothing is committed.  MT-COMMITTED is "N" on
*>              every exit, exactly as in KITTRIAL.
*> CALLED BY  : frmTrialKitting (multi), KITRDY batch
*> INTERFACE  : CALL "MULTITRL" USING WS-MULTITRL-CA
*>              COPYBOOK MULTITRLCA
*> CALLS      : BOMEXPL, AVAILQ
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2015-03-05 PMK  CR2299 MULTI KIT TRIAL
*> 2016-02-16 PMK  CR2451 ALLOCATE SHARED LEAVES ONCE
*> 2026-10-06 STERP KIT-007
*>================================================================
*> REQ: KIT-007
 IDENTIFICATION DIVISION.
 PROGRAM-ID. MULTITRL.
 AUTHOR. P M KEANE.
 DATE-WRITTEN. 05/03/2015.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY BOMEXPLCA REPLACING ==DFHCOMMAREA== BY ==Ws-Expl-Ca==
      LEADING ==CA-== BY ==EA-==.
 COPY AVAILQCA REPLACING ==DFHCOMMAREA== BY ==Ws-Avail-Ca==
      LEADING ==CA-== BY ==VA-==.
*>   The scenario.  It is built by repeated ADDKIT calls and
*>   survives in working storage until CLEAR, so a client can
*>   assemble a list of twelve kits over twelve round trips.
 01  Ws-Scenario.
     05  Ws-Kit-Cnt              PIC S9(4) VALUE 0.
     05  Ws-Assumptions          PIC X(300) VALUE SPACES.
     05  Ws-Kit OCCURS 12 TIMES.
         10  Ws-K-Order          PIC S9(4).
         10  Ws-K-Added          PIC S9(4).
         10  Ws-K-Ref            PIC X(30).
         10  Ws-K-Parent         PIC X(40).
         10  Ws-K-Rev            PIC X(4).
         10  Ws-K-Qty            PIC S9(13)V9(6).
         10  Ws-K-Priority       PIC 9(2).
         10  Ws-K-Req-Date       PIC 9(8).
         10  Ws-K-Demand-Ref     PIC X(40).
         10  Ws-K-Covered        PIC S9(13)V9(6).
         10  Ws-K-Short-Cnt      PIC S9(4).
         10  Ws-K-Limiting       PIC X(40).
         10  Ws-K-Limit-Short    PIC S9(13)V9(6).
         10  Ws-K-Verdict        PIC X(12).
         10  Ws-K-Note           PIC X(60).
*>   The shared pool.  Ws-P-Remain is the only number that
*>   matters: it is what is left after everybody ahead of you in
*>   the sequence has been served.
 01  Ws-Pool-Area.
     05  Ws-Pool-Cnt             PIC S9(4) VALUE 0.
*>   Two hundred distinct stocked components: one commuter bicycle
*>   alone has fifty nine, and the old limit of sixty dropped the
*>   next kit's components without a word.
     05  Ws-Pool-Max             PIC S9(4) VALUE 200.
     05  Ws-Pool OCCURS 200 TIMES.
         10  Ws-P-Item           PIC X(40).
         10  Ws-P-Uom            PIC X(10).
         10  Ws-P-Free           PIC S9(13)V9(6).
         10  Ws-P-Inbound        PIC S9(13)V9(6).
         10  Ws-P-Remain         PIC S9(13)V9(6).
         10  Ws-P-Inb-Remain     PIC S9(13)V9(6).
         10  Ws-P-Demand         PIC S9(13)V9(6).
         10  Ws-P-Allocated      PIC S9(13)V9(6).
         10  Ws-P-Shortage       PIC S9(13)V9(6).
         10  Ws-P-Claims         PIC S9(4).
         10  Ws-P-Conflict       PIC X.
         10  Ws-P-First-Kit      PIC X(30).
         10  Ws-P-Short-Kit      PIC X(30).
         10  Ws-P-Last-Kit       PIC X(30).
 01  Ws-Work.
     05  Ws-I                    PIC S9(4) VALUE 0.
     05  Ws-J                    PIC S9(4) VALUE 0.
     05  Ws-K                    PIC S9(4) VALUE 0.
     05  Ws-Slot                 PIC S9(4) VALUE 0.
     05  Ws-Hit                  PIC S9(4) VALUE 0.
     05  Ws-Best                 PIC S9(4) VALUE 0.
     05  Ws-Need                 PIC S9(13)V9(6) VALUE 0.
     05  Ws-Given                PIC S9(13)V9(6) VALUE 0.
     05  Ws-Short                PIC S9(13)V9(6) VALUE 0.
     05  Ws-More-Pages           PIC X VALUE "Y".
     05  Ws-Kit-Incomplete       PIC X VALUE "N".
     05  Ws-Swap-Order           PIC S9(4) VALUE 0.
     05  Ws-Want-Order           PIC S9(4) VALUE 0.
     05  Ws-Edit                 PIC -(13)9.9(6).
     05  Ws-Int-Edit             PIC -(9)9.
 LINKAGE SECTION.
 COPY MULTITRLCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "MULTITRL" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE 0 TO CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     MOVE "KIT-007" TO CA-REQ-REF
     MOVE "N" TO MT-COMMITTED
     EVALUATE CA-FUNCTION
         WHEN "CLEAR"   PERFORM Clear-Scenario
         WHEN "ADDKIT"  PERFORM Add-One-Kit
         WHEN "RUN"     PERFORM Run-The-Scenario
         WHEN "GETKIT"  PERFORM Fill-Kit-Window
         WHEN "GETCOMP" PERFORM Fill-Comp-Window
         WHEN OTHER     PERFORM Reject-Function
     END-EVALUATE
     MOVE "N" TO MT-COMMITTED
     MOVE Ws-Kit-Cnt TO MT-KIT-CNT
     MOVE Ws-Pool-Cnt TO MT-COMP-CNT
*>   The assumptions belong to the scenario, not to one call, so
*>   a client that pages the result still sees how the shared
*>   stock was sequenced.
     IF MT-ASSUMPTIONS = SPACES
         MOVE Ws-Assumptions TO MT-ASSUMPTIONS
     END-IF
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "KIT0002" TO CA-REASON-CODE
     STRING "MULTITRL does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>----------------------------------------------------------------
 Clear-Scenario.
     MOVE 0 TO Ws-Kit-Cnt
     MOVE 0 TO Ws-Pool-Cnt
     MOVE 0 TO MT-COMPLETE-CNT
     MOVE 0 TO MT-SHORT-CNT
     MOVE 0 TO MT-CONFLICT-CNT
     MOVE 0 TO MT-KFROM
     MOVE 0 TO MT-CFROM
     MOVE 0 TO MT-KWIN-CNT
     MOVE 0 TO MT-CWIN-CNT
     MOVE "N" TO MT-KMORE
     MOVE "N" TO MT-CMORE
     MOVE SPACES TO MT-ASSUMPTIONS
     MOVE SPACES TO Ws-Assumptions
     IF MT-SCENARIO-NO <= 0
         MOVE 1 TO MT-SCENARIO-NO
     END-IF
     MOVE "Scenario cleared. Add the competing kits with ADDKIT."
          TO CA-MESSAGE.

 Add-One-Kit.
     IF Ws-Kit-Cnt >= 12
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0025" TO CA-REASON-CODE
         MOVE "This scenario holds twelve kits. Run it, then start another."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF MT-A-PARENT = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "A parent item is required for every kit in the scenario."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF MT-A-QTY <= 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "The requested quantity must be greater than zero."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Kit-Cnt
     MOVE Ws-Kit-Cnt TO Ws-Slot
     MOVE Ws-Slot TO Ws-K-Added (Ws-Slot)
     MOVE Ws-Slot TO Ws-K-Order (Ws-Slot)
     MOVE MT-A-KIT-REF TO Ws-K-Ref (Ws-Slot)
     MOVE MT-A-PARENT TO Ws-K-Parent (Ws-Slot)
     MOVE MT-A-REVISION TO Ws-K-Rev (Ws-Slot)
     MOVE MT-A-QTY TO Ws-K-Qty (Ws-Slot)
     MOVE MT-A-PRIORITY TO Ws-K-Priority (Ws-Slot)
     MOVE MT-A-REQ-DATE TO Ws-K-Req-Date (Ws-Slot)
     MOVE MT-A-DEMAND-REF TO Ws-K-Demand-Ref (Ws-Slot)
     MOVE 0 TO Ws-K-Covered (Ws-Slot)
     MOVE 0 TO Ws-K-Short-Cnt (Ws-Slot)
     MOVE 0 TO Ws-K-Limit-Short (Ws-Slot)
     MOVE SPACES TO Ws-K-Limiting (Ws-Slot)
     MOVE "UNTRIED" TO Ws-K-Verdict (Ws-Slot)
     MOVE SPACES TO Ws-K-Note (Ws-Slot)
     IF Ws-K-Ref (Ws-Slot) = SPACES
         MOVE "KIT" TO Ws-K-Ref (Ws-Slot) (1:3)
         MOVE Ws-Slot TO Ws-Int-Edit
         MOVE FUNCTION TRIM(Ws-Int-Edit) TO Ws-K-Ref (Ws-Slot) (4:6)
     END-IF
*>   Re-sequence now rather than at RUN, so that a client can
*>   show the order in which the shared stock WILL be drawn
*>   before it commits to running the scenario.
     PERFORM Sequence-The-Kits
     MOVE Ws-Kit-Cnt TO Ws-Int-Edit
     STRING "Kit " FUNCTION TRIM(Ws-K-Ref (Ws-Slot))
            " added; the scenario now holds "
            FUNCTION TRIM(Ws-Int-Edit) " kits."
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>----------------------------------------------------------------
*> RUN - the whole point of the program.
 Run-The-Scenario.
     IF Ws-Kit-Cnt = 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0021" TO CA-REASON-CODE
         MOVE "There are no kits in this scenario. Use ADDKIT first."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF MT-SITE = SPACES
         MOVE CA-SITE TO MT-SITE
     END-IF
     IF MT-AS-AT = 0
         MOVE CA-BUSINESS-DATE TO MT-AS-AT
     END-IF
     MOVE 0 TO Ws-Pool-Cnt
     MOVE 0 TO MT-COMPLETE-CNT
     MOVE 0 TO MT-SHORT-CNT
     MOVE 0 TO MT-CONFLICT-CNT
     PERFORM Sequence-The-Kits
     PERFORM VARYING Ws-K FROM 1 BY 1 UNTIL Ws-K > Ws-Kit-Cnt
                                        OR CA-RETURN-CODE > 4
         MOVE Ws-K TO Ws-Want-Order
         PERFORM Find-Kit-In-Order
         PERFORM Trial-One-Kit
     END-PERFORM
     PERFORM Count-Conflicts
     PERFORM Describe-The-Scenario.

*>   The sequence is explicit, recorded and reproducible.  A
*>   different sequence is a different scenario, not a correction
*>   of this one.
 Sequence-The-Kits.
     IF MT-MANUAL-SEQ = "Y"
         PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > Ws-Kit-Cnt
             MOVE Ws-K-Added (Ws-I) TO Ws-K-Order (Ws-I)
         END-PERFORM
         MOVE "Manual sequence as entered; shared stock drawn once in that order."
              TO MT-ASSUMPTIONS
         MOVE MT-ASSUMPTIONS TO Ws-Assumptions
         EXIT PARAGRAPH
     END-IF
*>   Selection ordering: priority, then required date, then the
*>   order the kits were added - never anything implicit.
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > Ws-Kit-Cnt
         MOVE 0 TO Ws-K-Order (Ws-I)
     END-PERFORM
     PERFORM VARYING Ws-Swap-Order FROM 1 BY 1
         UNTIL Ws-Swap-Order > Ws-Kit-Cnt
         MOVE 0 TO Ws-Best
         PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > Ws-Kit-Cnt
             IF Ws-K-Order (Ws-I) = 0
                 IF Ws-Best = 0
                     MOVE Ws-I TO Ws-Best
                 ELSE
                     PERFORM Compare-Two-Kits
                 END-IF
             END-IF
         END-PERFORM
         IF Ws-Best > 0
             MOVE Ws-Swap-Order TO Ws-K-Order (Ws-Best)
         END-IF
     END-PERFORM
     IF MT-SEQUENCE-RULE = SPACES
         MOVE "PRIORITY,REQDATE,ENTRY" TO MT-SEQUENCE-RULE
     END-IF
     MOVE "Priority then required date then entry order; shared stock drawn once in that order."
          TO MT-ASSUMPTIONS
     MOVE MT-ASSUMPTIONS TO Ws-Assumptions.

 Compare-Two-Kits.
     IF Ws-K-Priority (Ws-I) < Ws-K-Priority (Ws-Best)
         MOVE Ws-I TO Ws-Best
         EXIT PARAGRAPH
     END-IF
     IF Ws-K-Priority (Ws-I) > Ws-K-Priority (Ws-Best)
         EXIT PARAGRAPH
     END-IF
     IF Ws-K-Req-Date (Ws-I) < Ws-K-Req-Date (Ws-Best)
         MOVE Ws-I TO Ws-Best
         EXIT PARAGRAPH
     END-IF
     IF Ws-K-Req-Date (Ws-I) > Ws-K-Req-Date (Ws-Best)
         EXIT PARAGRAPH
     END-IF
     IF Ws-K-Added (Ws-I) < Ws-K-Added (Ws-Best)
         MOVE Ws-I TO Ws-Best
     END-IF.

 Find-Kit-In-Order.
     MOVE 0 TO Ws-Slot
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > Ws-Kit-Cnt
         IF Ws-K-Order (Ws-I) = Ws-Want-Order
             MOVE Ws-I TO Ws-Slot
             EXIT PERFORM
         END-IF
     END-PERFORM.

*>----------------------------------------------------------------
 Trial-One-Kit.
     IF Ws-Slot = 0
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO Ws-K-Short-Cnt (Ws-Slot)
     MOVE 0 TO Ws-K-Limit-Short (Ws-Slot)
     MOVE SPACES TO Ws-K-Limiting (Ws-Slot)
     MOVE Ws-K-Qty (Ws-Slot) TO Ws-K-Covered (Ws-Slot)
     MOVE "N" TO Ws-Kit-Incomplete
     PERFORM Explode-This-Kit
     IF CA-RETURN-CODE > 4
         MOVE "ERROR" TO Ws-K-Verdict (Ws-Slot)
         EXIT PARAGRAPH
     END-IF
*>   A bill that cannot be fully exploded is not evidence that the
*>   kit can be built, so it is never labelled complete.
     IF Ws-Kit-Incomplete = "Y"
         MOVE "INCOMPLETE" TO Ws-K-Verdict (Ws-Slot)
         MOVE 0 TO Ws-K-Covered (Ws-Slot)
         MOVE "The bill of material is incomplete; nothing was allocated."
              TO Ws-K-Note (Ws-Slot)
         ADD 1 TO MT-SHORT-CNT
         EXIT PARAGRAPH
     END-IF
     IF Ws-K-Short-Cnt (Ws-Slot) = 0
         MOVE "COMPLETE" TO Ws-K-Verdict (Ws-Slot)
         MOVE "Covered from the stock still free at this point in the sequence."
              TO Ws-K-Note (Ws-Slot)
         ADD 1 TO MT-COMPLETE-CNT
     ELSE
         MOVE "SHORT" TO Ws-K-Verdict (Ws-Slot)
         MOVE 0 TO Ws-K-Covered (Ws-Slot)
         MOVE Ws-K-Limit-Short (Ws-Slot) TO Ws-Edit
         STRING "Short of " FUNCTION TRIM(Ws-K-Limiting (Ws-Slot))
                " by " FUNCTION TRIM(Ws-Edit)
             DELIMITED BY SIZE INTO Ws-K-Note (Ws-Slot)
         ADD 1 TO MT-SHORT-CNT
     END-IF.

 Explode-This-Kit.
     INITIALIZE Ws-Expl-Ca
     MOVE "EXPLODE" TO EA-FUNCTION
     MOVE CA-COMPANY TO EA-COMPANY
     MOVE MT-SITE TO EA-SITE
     MOVE CA-USER TO EA-USER
     MOVE CA-BUSINESS-DATE TO EA-BUSINESS-DATE
     MOVE "EXECUTION" TO BX-CALC-TYPE
     MOVE Ws-K-Parent (Ws-Slot) TO BX-PARENT-ITEM
     MOVE Ws-K-Rev (Ws-Slot) TO BX-PARENT-REV
*>   BOMEXPL takes STANDARD / JOB / ORDER only (docs\modules\bom.md).
*>   A multi-kit trial compares standard bills - the per-kit demand
*>   reference is a priority/traceability note, not a bill selector.
     MOVE "STANDARD" TO BX-PURPOSE
     MOVE Ws-K-Qty (Ws-Slot) TO BX-REQ-QTY
     MOVE MT-AS-AT TO BX-EFF-DATE
     MOVE 0 TO BX-MAX-DEPTH
     MOVE "Y" TO BX-EXPAND-PHANTOM
     MOVE "N" TO BX-INCLUDE-NOTES
     MOVE "N" TO BX-INCLUDE-BYPROD
     MOVE "LEAF" TO BX-VIEW
     MOVE 25 TO BX-WIN-MAX
     MOVE 1 TO BX-FROM-LINE
     CALL "BOMEXPL" USING Ws-Expl-Ca
     IF EA-RETURN-CODE > 4
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0004" TO CA-REASON-CODE
         MOVE EA-MESSAGE TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF BX-COMPLETENESS NOT = "COMPLETE"
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0004" TO CA-REASON-CODE
         MOVE "Y" TO Ws-Kit-Incomplete
         EXIT PARAGRAPH
     END-IF
*>   EXPLODE only computes and retains the result; the lines are
*>   read back a window at a time with GETLINE, and BOMEXPL itself
*>   moves BX-FROM-LINE on to the next unread occurrence (the same
*>   contract KITTRIAL follows).  Until 2026 this paragraph called
*>   EXPLODE again instead, read no lines at all, claimed nothing
*>   from the pool and reported every kit complete.
     MOVE "GETLINE" TO EA-FUNCTION
     MOVE 1 TO BX-FROM-LINE
     MOVE "Y" TO Ws-More-Pages
     PERFORM UNTIL Ws-More-Pages = "N" OR CA-RETURN-CODE > 4
         CALL "BOMEXPL" USING Ws-Expl-Ca
         IF EA-RETURN-CODE > 4
             MOVE 8 TO CA-RETURN-CODE
             MOVE "KIT0004" TO CA-REASON-CODE
             MOVE EA-MESSAGE TO CA-MESSAGE
             EXIT PERFORM
         END-IF
         PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > BX-WIN-CNT
                                            OR CA-RETURN-CODE > 4
             PERFORM Claim-One-Component
         END-PERFORM
         IF BX-MORE-LINES NOT = "Y" OR BX-WIN-CNT = 0
             MOVE "N" TO Ws-More-Pages
         END-IF
     END-PERFORM.

*>   The single allocation.  Whatever this kit takes is removed
*>   from the pool before the next kit in the sequence is asked.
*>   Services, cost-only resources and non-stock lines are leaves
*>   of the bill but are never drawn from stock, so they take no
*>   part in the pool.
 Claim-One-Component.
     IF BX-L-NODE-TYPE (Ws-I) = "NOTE"
         OR BX-L-NODE-TYPE (Ws-I) = "RESOURCE"
         OR BX-L-NODE-TYPE (Ws-I) = "BYPROD"
         EXIT PARAGRAPH
     END-IF
     IF BX-L-SUPPLY (Ws-I) = "SERVICE"
         OR BX-L-SUPPLY (Ws-I) = "COSTONLY"
         OR BX-L-SUPPLY (Ws-I) = "NONSTOCK"
         OR BX-L-SUPPLY (Ws-I) = "NOTE"
         EXIT PARAGRAPH
     END-IF
*>   The material requirement of a leaf is BX-L-REQ-QTY.  BX-L-GROSS
*>   is the gross STARTS of a manufactured node and is zero on a
*>   leaf, so reading it asked the pool for nothing at all.
     COMPUTE Ws-Need = BX-L-REQ-QTY (Ws-I) - BX-L-STOCK-USED (Ws-I)
     IF Ws-Need <= 0
         EXIT PARAGRAPH
     END-IF
     PERFORM Find-Pool-Entry
     IF Ws-Hit = 0
*>       The component table is full.  Leaving the line out would
*>       report the kit complete on stock nobody looked at.
         MOVE "ERROR" TO Ws-K-Verdict (Ws-Slot)
         MOVE "Too many distinct components for one scenario."
              TO Ws-K-Note (Ws-Slot)
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0025" TO CA-REASON-CODE
         MOVE Ws-Pool-Max TO Ws-Int-Edit
         MOVE SPACES TO CA-MESSAGE
         STRING "This scenario holds "
                FUNCTION TRIM(Ws-Int-Edit)
                " distinct components. Run fewer kits at a time."
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ws-P-Demand (Ws-Hit) = Ws-P-Demand (Ws-Hit) + Ws-Need
*>   Claims counts KITS, not occurrences: a kit that uses the same
*>   wire in six places is still one claimant.
     IF Ws-P-First-Kit (Ws-Hit) = SPACES
         MOVE Ws-K-Ref (Ws-Slot) TO Ws-P-First-Kit (Ws-Hit)
         MOVE Ws-K-Ref (Ws-Slot) TO Ws-P-Last-Kit (Ws-Hit)
     ELSE
         IF Ws-P-Last-Kit (Ws-Hit) NOT = Ws-K-Ref (Ws-Slot)
             ADD 1 TO Ws-P-Claims (Ws-Hit)
             MOVE Ws-K-Ref (Ws-Slot) TO Ws-P-Last-Kit (Ws-Hit)
         END-IF
     END-IF
     IF Ws-Need > Ws-P-Remain (Ws-Hit)
         MOVE Ws-P-Remain (Ws-Hit) TO Ws-Given
     ELSE
         MOVE Ws-Need TO Ws-Given
     END-IF
     COMPUTE Ws-P-Remain (Ws-Hit) = Ws-P-Remain (Ws-Hit) - Ws-Given
     COMPUTE Ws-P-Allocated (Ws-Hit) = Ws-P-Allocated (Ws-Hit)
                                     + Ws-Given
     COMPUTE Ws-Short = Ws-Need - Ws-Given
     IF Ws-Short > 0 AND MT-USE-INBOUND = "Y"
         IF Ws-Short > Ws-P-Inb-Remain (Ws-Hit)
             COMPUTE Ws-P-Inb-Remain (Ws-Hit) = 0
         ELSE
             COMPUTE Ws-P-Inb-Remain (Ws-Hit) =
                 Ws-P-Inb-Remain (Ws-Hit) - Ws-Short
             MOVE 0 TO Ws-Short
         END-IF
     END-IF
     IF Ws-Short > 0
         COMPUTE Ws-P-Shortage (Ws-Hit) = Ws-P-Shortage (Ws-Hit)
                                        + Ws-Short
         IF Ws-P-Short-Kit (Ws-Hit) = SPACES
             MOVE Ws-K-Ref (Ws-Slot) TO Ws-P-Short-Kit (Ws-Hit)
         END-IF
         ADD 1 TO Ws-K-Short-Cnt (Ws-Slot)
         IF Ws-Short > Ws-K-Limit-Short (Ws-Slot)
             MOVE Ws-Short TO Ws-K-Limit-Short (Ws-Slot)
             MOVE BX-L-ITEM (Ws-I) TO Ws-K-Limiting (Ws-Slot)
         END-IF
     END-IF.

 Find-Pool-Entry.
     MOVE 0 TO Ws-Hit
     PERFORM VARYING Ws-J FROM 1 BY 1 UNTIL Ws-J > Ws-Pool-Cnt
         IF Ws-P-Item (Ws-J) = BX-L-ITEM (Ws-I)
             MOVE Ws-J TO Ws-Hit
             EXIT PERFORM
         END-IF
     END-PERFORM
     IF Ws-Hit > 0
         EXIT PARAGRAPH
     END-IF
     IF Ws-Pool-Cnt >= Ws-Pool-Max
         EXIT PARAGRAPH
     END-IF
     INITIALIZE Ws-Avail-Ca
     MOVE "AVAIL" TO VA-FUNCTION
     MOVE CA-COMPANY TO VA-COMPANY
     MOVE MT-SITE TO VA-SITE
     MOVE CA-USER TO VA-USER
     MOVE CA-BUSINESS-DATE TO VA-BUSINESS-DATE
     MOVE BX-L-ITEM (Ws-I) TO AV-ITEM-CODE
     MOVE MT-SITE TO AV-SITE
     MOVE MT-AS-AT TO AV-AS-AT
     MOVE "KIT" TO AV-DEMAND-TYPE
     MOVE SPACES TO AV-DEMAND-REF
     CALL "AVAILQ" USING Ws-Avail-Ca
     ADD 1 TO Ws-Pool-Cnt
     MOVE Ws-Pool-Cnt TO Ws-Hit
     MOVE BX-L-ITEM (Ws-I) TO Ws-P-Item (Ws-Hit)
     MOVE BX-L-UOM (Ws-I) TO Ws-P-Uom (Ws-Hit)
     MOVE 0 TO Ws-P-Demand (Ws-Hit)
     MOVE 0 TO Ws-P-Allocated (Ws-Hit)
     MOVE 0 TO Ws-P-Shortage (Ws-Hit)
     MOVE 1 TO Ws-P-Claims (Ws-Hit)
     MOVE "N" TO Ws-P-Conflict (Ws-Hit)
     MOVE SPACES TO Ws-P-First-Kit (Ws-Hit)
     MOVE SPACES TO Ws-P-Short-Kit (Ws-Hit)
     MOVE SPACES TO Ws-P-Last-Kit (Ws-Hit)
     IF VA-RETURN-CODE > 4
         MOVE 0 TO Ws-P-Free (Ws-Hit)
         MOVE 0 TO Ws-P-Inbound (Ws-Hit)
     ELSE
         MOVE AV-QTY-FREE TO Ws-P-Free (Ws-Hit)
         MOVE AV-QTY-EXPECTED TO Ws-P-Inbound (Ws-Hit)
     END-IF
     IF Ws-P-Free (Ws-Hit) < 0
         MOVE 0 TO Ws-P-Free (Ws-Hit)
     END-IF
     MOVE Ws-P-Free (Ws-Hit) TO Ws-P-Remain (Ws-Hit)
     MOVE Ws-P-Inbound (Ws-Hit) TO Ws-P-Inb-Remain (Ws-Hit).

*>   A conflict is a component that more than one kit asked for
*>   and that could not satisfy all of them.  Two kits sharing a
*>   plentiful component is not a conflict, it is just sharing.
 Count-Conflicts.
     MOVE 0 TO MT-CONFLICT-CNT
     PERFORM VARYING Ws-J FROM 1 BY 1 UNTIL Ws-J > Ws-Pool-Cnt
         IF Ws-P-Claims (Ws-J) > 1 AND Ws-P-Shortage (Ws-J) > 0
             MOVE "Y" TO Ws-P-Conflict (Ws-J)
             ADD 1 TO MT-CONFLICT-CNT
         ELSE
             MOVE "N" TO Ws-P-Conflict (Ws-J)
         END-IF
     END-PERFORM.

 Describe-The-Scenario.
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     MOVE MT-COMPLETE-CNT TO Ws-Int-Edit
     IF MT-SHORT-CNT = 0
         MOVE 0 TO CA-RETURN-CODE
         STRING "All " FUNCTION TRIM(Ws-Int-Edit)
                " kits can be covered from the same pool."
             DELIMITED BY SIZE INTO CA-MESSAGE
     ELSE
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0010" TO CA-REASON-CODE
         STRING FUNCTION TRIM(Ws-Int-Edit)
                " kits covered; "
             DELIMITED BY SIZE INTO CA-MESSAGE
         MOVE MT-SHORT-CNT TO Ws-Int-Edit
         STRING FUNCTION TRIM(CA-MESSAGE)
                FUNCTION TRIM(Ws-Int-Edit)
                " short. The shared stock was allocated once, in sequence."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF.

*>----------------------------------------------------------------
 Fill-Kit-Window.
     IF MT-KFROM < 0
         MOVE 0 TO MT-KFROM
     END-IF
     MOVE 0 TO MT-KWIN-CNT
     MOVE "N" TO MT-KMORE
     PERFORM VARYING Ws-K FROM 1 BY 1
         UNTIL Ws-K > 12 OR MT-KWIN-CNT >= 12
         COMPUTE Ws-J = MT-KFROM + Ws-K
         IF Ws-J > Ws-Kit-Cnt
             EXIT PERFORM
         END-IF
         MOVE Ws-J TO Ws-Want-Order
         PERFORM Find-Kit-In-Order
         IF Ws-Slot = 0
             MOVE Ws-J TO Ws-Slot
         END-IF
         ADD 1 TO MT-KWIN-CNT
         MOVE Ws-K-Order (Ws-Slot) TO MT-K-SEQ (MT-KWIN-CNT)
         MOVE Ws-K-Ref (Ws-Slot) TO MT-K-KIT-REF (MT-KWIN-CNT)
         MOVE Ws-K-Parent (Ws-Slot) TO MT-K-PARENT (MT-KWIN-CNT)
         MOVE Ws-K-Qty (Ws-Slot) TO MT-K-QTY (MT-KWIN-CNT)
         MOVE Ws-K-Priority (Ws-Slot) TO MT-K-PRIORITY (MT-KWIN-CNT)
         MOVE Ws-K-Req-Date (Ws-Slot) TO MT-K-REQ-DATE (MT-KWIN-CNT)
         MOVE Ws-K-Covered (Ws-Slot) TO MT-K-COVERED (MT-KWIN-CNT)
         MOVE Ws-K-Short-Cnt (Ws-Slot)
              TO MT-K-SHORT-CNT (MT-KWIN-CNT)
         MOVE Ws-K-Limiting (Ws-Slot) TO MT-K-LIMITING (MT-KWIN-CNT)
         MOVE Ws-K-Limit-Short (Ws-Slot)
              TO MT-K-LIMIT-SHRT (MT-KWIN-CNT)
         MOVE Ws-K-Verdict (Ws-Slot) TO MT-K-VERDICT (MT-KWIN-CNT)
         MOVE Ws-K-Note (Ws-Slot) TO MT-K-NOTE (MT-KWIN-CNT)
     END-PERFORM
     IF (MT-KFROM + MT-KWIN-CNT) < Ws-Kit-Cnt
         MOVE "Y" TO MT-KMORE
     END-IF.

 Fill-Comp-Window.
     IF MT-CFROM < 0
         MOVE 0 TO MT-CFROM
     END-IF
     MOVE 0 TO MT-CWIN-CNT
     MOVE "N" TO MT-CMORE
     PERFORM VARYING Ws-K FROM 1 BY 1
         UNTIL Ws-K > 20 OR MT-CWIN-CNT >= 20
         COMPUTE Ws-J = MT-CFROM + Ws-K
         IF Ws-J > Ws-Pool-Cnt
             EXIT PERFORM
         END-IF
         ADD 1 TO MT-CWIN-CNT
         MOVE Ws-P-Item (Ws-J) TO MT-C-ITEM (MT-CWIN-CNT)
         MOVE Ws-P-Uom (Ws-J) TO MT-C-UOM (MT-CWIN-CNT)
         MOVE Ws-P-Free (Ws-J) TO MT-C-FREE (MT-CWIN-CNT)
         MOVE Ws-P-Inbound (Ws-J) TO MT-C-INBOUND (MT-CWIN-CNT)
         MOVE Ws-P-Demand (Ws-J) TO MT-C-DEMAND (MT-CWIN-CNT)
         MOVE Ws-P-Allocated (Ws-J) TO MT-C-ALLOCATED (MT-CWIN-CNT)
         MOVE Ws-P-Shortage (Ws-J) TO MT-C-SHORTAGE (MT-CWIN-CNT)
         MOVE Ws-P-Claims (Ws-J) TO MT-C-CLAIMS (MT-CWIN-CNT)
         MOVE Ws-P-Conflict (Ws-J) TO MT-C-CONFLICT (MT-CWIN-CNT)
         MOVE Ws-P-First-Kit (Ws-J) TO MT-C-FIRST-KIT (MT-CWIN-CNT)
         MOVE Ws-P-Short-Kit (Ws-J) TO MT-C-SHORT-KIT (MT-CWIN-CNT)
     END-PERFORM
     IF (MT-CFROM + MT-CWIN-CNT) < Ws-Pool-Cnt
         MOVE "Y" TO MT-CMORE
     END-IF.
