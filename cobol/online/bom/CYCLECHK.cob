>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : CYCLECHK  (IS RECURSIVE)
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : Reject a direct self use and any ancestry path
*>              that returns to the same selected item, and say
*>              which path offends.  Two independent paths that
*>              reach the same leaf or the same subassembly are
*>              NOT a cycle - that is an ordinary shared
*>              descendant and both quantities contribute (B6).
*>
*>              Before a release, the permitted alternatives are
*>              walked as well (CC-INCLUDE-OPT = "Y"), because a
*>              configuration that is not selected today could be
*>              selected tomorrow and would then close the loop.
*>
*>              EDGE answers the editor's question - may this
*>              component be added to this bill? - by asking
*>              whether the proposed parent is reachable below the
*>              proposed child.
*> CALLED BY  : BOMVALID, BOMBULK, the BOM editor
*> INTERFACE  : CALL "CYCLECHK" USING WS-CYCLECHK-CA
*>              COPYBOOK CYCLECHKCA
*> CALLS      : BOMSELCT, BOMREAD, itself
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2014-06-10 AJS  CR2219 ENGINEERING REWRITE - RECURSIVE
*> 2017-02-14 AJS  CR2588 WALK PERMITTED ALTERNATIVES AS WELL.
*>                 BILL 44120 PASSED RELEASE AND THEN LOOPED THE
*>                 MOMENT THE OTHER FINISH WAS CHOSEN.
*> 2026-10-05 STERP SHARED DESCENDANTS COUNTED, NOT REJECTED
*>================================================================
*> REQ: BOM-013, BOM-019, BOM-029
 IDENTIFICATION DIVISION.
 PROGRAM-ID. CYCLECHK IS RECURSIVE.
 AUTHOR. A J SPENCER.
 DATE-WRITTEN. 10/06/2014.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY BOMSELCTCA REPLACING ==DFHCOMMAREA== BY ==Ws-Sel-Ca==
      LEADING ==CA-== BY ==SA-==
      LEADING ==BS-== BY ==SS-==.
 01  Ws-Run.
     05  Ws-Company              PIC X(10) VALUE SPACES.
     05  Ws-Site                 PIC X(10) VALUE SPACES.
     05  Ws-User                 PIC X(20) VALUE SPACES.
     05  Ws-Eff-Date             PIC 9(8) VALUE 0.
     05  Ws-Variant              PIC X(20) VALUE SPACES.
     05  Ws-Purpose              PIC X(10) VALUE SPACES.
     05  Ws-Target               PIC X(40) VALUE SPACES.
     05  Ws-Include-Opt          PIC X VALUE "N".
     05  Ws-Max-Depth            PIC S9(4) VALUE 25.
     05  Ws-Visited              PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Deepest              PIC S9(4) VALUE 0.
     05  Ws-Shared               PIC S9(4) VALUE 0.
     05  Ws-Cycle                PIC X VALUE "N".
     05  Ws-Limit-Hit            PIC X VALUE "N".
     05  Ws-Stop                 PIC X VALUE "N".
     05  Ws-Cycle-Path           PIC X(240) VALUE SPACES.
     05  Ws-Cycle-Item           PIC X(40) VALUE SPACES.
     05  Ws-Cycle-Line           PIC S9(4) VALUE 0.
 01  Ws-Ancestry.
     05  Ws-Anc OCCURS 30 TIMES.
         10  Wa-Item             PIC X(40).
 01  Ws-Seen.
     05  Ws-Seen-Cnt             PIC S9(4) VALUE 0.
     05  Ws-Seen-Item OCCURS 300 TIMES PIC X(40).
 LOCAL-STORAGE SECTION.
 COPY BOMBILL.
 01  Ls-Node.
     05  Ls-I                    PIC S9(4) VALUE 0.
     05  Ls-J                    PIC S9(4) VALUE 0.
     05  Ls-Depth                PIC S9(4) VALUE 0.
     05  Ls-Path                 PIC X(240) VALUE SPACES.
     05  Ls-Skip                 PIC X VALUE "N".
     05  Ls-Seen                 PIC X VALUE "N".
     05  Ls-Bill-Id              PIC S9(9) COMP-5 VALUE 0.
 COPY CYCLECHKCA REPLACING ==DFHCOMMAREA== BY ==Ls-Child-Ca==
      LEADING ==CA-== BY ==CB-==
      LEADING ==CC-== BY ==CD-==.
 LINKAGE SECTION.
 COPY CYCLECHKCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     EVALUATE CA-FUNCTION
         WHEN "CHECK"  PERFORM Start-A-Check
         WHEN "EDGE"   PERFORM Check-One-Edge
         WHEN "WALK"   PERFORM Walk-One-Node
         WHEN OTHER    PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE "CYCLECHK" TO CA-PROGRAM
     MOVE 8 TO CA-RETURN-CODE
     MOVE "BOM0401" TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "CYCLECHK does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>----------------------------------------------------------------
 Start-A-Check.
     PERFORM Initialise-Run
     MOVE CC-ROOT-ITEM TO Ws-Target
     PERFORM Walk-From-Root
     PERFORM Publish-Outcome.

*> EDGE: adding CC-CHILD-ITEM under CC-ROOT-ITEM closes a loop
*> when the parent is already reachable below the child, or when
*> the two are the same item (a direct self use).
 Check-One-Edge.
     PERFORM Initialise-Run
     MOVE CC-ROOT-ITEM TO Ws-Target
     IF CC-CHILD-ITEM = CC-ROOT-ITEM
         MOVE "Y" TO Ws-Cycle
         MOVE CC-ROOT-ITEM TO Ws-Cycle-Item
         MOVE SPACES TO Ws-Cycle-Path
         STRING FUNCTION TRIM(CC-ROOT-ITEM) " uses itself directly"
             DELIMITED BY SIZE INTO Ws-Cycle-Path
         PERFORM Publish-Outcome
         EXIT PARAGRAPH
     END-IF
     MOVE CC-CHILD-ITEM TO CC-ROOT-ITEM
     PERFORM Walk-From-Root
     MOVE Ws-Target TO CC-ROOT-ITEM
     PERFORM Publish-Outcome.

 Initialise-Run.
     MOVE "CYCLECHK" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
     MOVE "BOM-013" TO CA-REQ-REF
     MOVE CA-COMPANY TO Ws-Company
     MOVE CA-SITE TO Ws-Site
     MOVE CA-USER TO Ws-User
     MOVE FUNCTION UPPER-CASE(CC-ROOT-ITEM) TO CC-ROOT-ITEM
     MOVE FUNCTION UPPER-CASE(CC-CHILD-ITEM) TO CC-CHILD-ITEM
     IF CC-EFF-DATE = 0
         MOVE CA-BUSINESS-DATE TO CC-EFF-DATE
     END-IF
     MOVE CC-EFF-DATE TO Ws-Eff-Date
     IF CC-PURPOSE = SPACES
         MOVE "STANDARD" TO CC-PURPOSE
     END-IF
     MOVE CC-PURPOSE TO Ws-Purpose
     MOVE CC-VARIANT TO Ws-Variant
     MOVE CC-INCLUDE-OPT TO Ws-Include-Opt
     IF CC-MAX-DEPTH < 1 OR CC-MAX-DEPTH > 25
         MOVE 25 TO CC-MAX-DEPTH
     END-IF
     MOVE CC-MAX-DEPTH TO Ws-Max-Depth
     MOVE 0 TO Ws-Visited Ws-Deepest Ws-Shared Ws-Seen-Cnt
     MOVE 0 TO Ws-Cycle-Line
     MOVE "N" TO Ws-Cycle Ws-Limit-Hit Ws-Stop
     MOVE SPACES TO Ws-Cycle-Path Ws-Cycle-Item
     PERFORM VARYING Ls-I FROM 1 BY 1 UNTIL Ls-I > 30
         MOVE SPACES TO Wa-Item(Ls-I)
     END-PERFORM.

 Walk-From-Root.
     INITIALIZE Ls-Child-Ca
     MOVE "WALK" TO CB-FUNCTION
     MOVE Ws-Company TO CB-COMPANY
     MOVE Ws-Site TO CB-SITE
     MOVE Ws-User TO CB-USER
     MOVE Ws-Eff-Date TO CB-BUSINESS-DATE
     MOVE CC-ROOT-ITEM TO CD-ROOT-ITEM
     MOVE CC-ROOT-REV TO CD-ROOT-REV
     MOVE 0 TO CD-MAX-DEPTH
     MOVE CC-ROOT-ITEM TO CD-CYCLE-PATH
     CALL "CYCLECHK" USING Ls-Child-Ca.

*>----------------------------------------------------------------
*> One node of the walk.  CD-MAX-DEPTH carries the current depth
*> and CD-CYCLE-PATH the path so far; both belong to this level
*> only, which is why they travel in LOCAL-STORAGE.
 Walk-One-Node.
     MOVE CC-MAX-DEPTH TO Ls-Depth
     MOVE CC-CYCLE-PATH TO Ls-Path
     IF Ws-Stop = "Y"
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Visited
     IF Ls-Depth > Ws-Deepest
         MOVE Ls-Depth TO Ws-Deepest
     END-IF
     COMPUTE Ls-J = Ls-Depth + 1
     MOVE CC-ROOT-ITEM TO Wa-Item(Ls-J)
     PERFORM Note-If-Shared
     PERFORM Select-This-Bill
     IF Ls-Bill-Id = 0
         EXIT PARAGRAPH
     END-IF
     PERFORM Read-This-Bill
     IF Ls-Skip = "Y"
         EXIT PARAGRAPH
     END-IF
     PERFORM VARYING Ls-I FROM 1 BY 1
         UNTIL Ls-I > BR-LINE-COUNT OR Ws-Stop = "Y"
         PERFORM Examine-One-Component
     END-PERFORM.

*> Reaching the same item twice on different paths is normal and
*> is counted, not refused.
 Note-If-Shared.
     MOVE "N" TO Ls-Seen
     PERFORM VARYING Ls-J FROM 1 BY 1
         UNTIL Ls-J > Ws-Seen-Cnt OR Ls-Seen = "Y"
         IF Ws-Seen-Item(Ls-J) = CC-ROOT-ITEM
             MOVE "Y" TO Ls-Seen
         END-IF
     END-PERFORM
     IF Ls-Seen = "Y"
         ADD 1 TO Ws-Shared
         EXIT PARAGRAPH
     END-IF
     IF Ws-Seen-Cnt < 300
         ADD 1 TO Ws-Seen-Cnt
         MOVE CC-ROOT-ITEM TO Ws-Seen-Item(Ws-Seen-Cnt)
     END-IF.

 Select-This-Bill.
     MOVE 0 TO Ls-Bill-Id
     INITIALIZE Ws-Sel-Ca
     MOVE "SELECT" TO SA-FUNCTION
     MOVE Ws-Company TO SA-COMPANY
     MOVE Ws-Site TO SA-SITE
     MOVE Ws-User TO SA-USER
     MOVE Ws-Eff-Date TO SA-BUSINESS-DATE
     MOVE CC-ROOT-ITEM TO SS-PARENT-ITEM
     MOVE CC-ROOT-REV TO SS-PARENT-REV
     MOVE Ws-Purpose TO SS-PURPOSE
     MOVE Ws-Variant TO SS-VARIANT
     MOVE Ws-Eff-Date TO SS-EFF-DATE
     MOVE "Y" TO SS-ALLOW-DRAFT
     CALL "BOMSELCT" USING Ws-Sel-Ca
     IF SA-RETURN-CODE <= 4
         MOVE SS-BOM-ID TO Ls-Bill-Id
     END-IF.

 Read-This-Bill.
     MOVE "N" TO Ls-Skip
     INITIALIZE BOM-RD-CTL
     MOVE "BILL" TO BR-FUNC
     MOVE Ws-Company TO BR-COMPANY
     MOVE Ws-Site TO BR-SITE
     MOVE Ls-Bill-Id TO BR-BOM-ID
     MOVE Ws-Eff-Date TO BR-EFF-DATE
     MOVE 500 TO BR-MAX-LINES
     CALL "BOMREAD" USING BOM-RD-CTL BOM-HDR-REC BOM-CMP-TAB
     IF BR-STATUS NOT = "OK"
         MOVE "Y" TO Ls-Skip
     END-IF.

*>----------------------------------------------------------------
 Examine-One-Component.
     IF BC-SUPPLY(Ls-I) = "NOTE" OR BC-SUPPLY(Ls-I) = "SERVICE"
        OR BC-SUPPLY(Ls-I) = "COSTONLY"
         EXIT PARAGRAPH
     END-IF
*>   an unselected alternative is walked only when the caller
*>   asked for every permitted configuration (CR2588)
     IF BC-ALT-GROUP(Ls-I) NOT = SPACES
        AND BC-SELECTED(Ls-I) NOT = "Y"
        AND Ws-Include-Opt NOT = "Y"
         EXIT PARAGRAPH
     END-IF
     IF BC-EFF-FROM(Ls-I) NOT = 0
        AND BC-EFF-FROM(Ls-I) > Ws-Eff-Date
         EXIT PARAGRAPH
     END-IF
     IF BC-EFF-TO(Ls-I) NOT = 0
        AND BC-EFF-TO(Ls-I) <= Ws-Eff-Date
         EXIT PARAGRAPH
     END-IF
     PERFORM Test-Ancestry
     IF Ws-Cycle = "Y"
         EXIT PARAGRAPH
     END-IF
     IF BC-SUPPLY(Ls-I) NOT = "MAKE"
        AND BC-SUPPLY(Ls-I) NOT = "PHANTOM"
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ls-J = Ls-Depth + 1
     IF Ls-J >= Ws-Max-Depth
         MOVE "Y" TO Ws-Limit-Hit
         EXIT PARAGRAPH
     END-IF
     PERFORM Walk-Into-Child.

 Test-Ancestry.
     COMPUTE Ls-J = Ls-Depth + 1
     PERFORM VARYING Ws-Cycle-Line FROM 1 BY 1
         UNTIL Ws-Cycle-Line > Ls-J OR Ws-Cycle = "Y"
         IF Wa-Item(Ws-Cycle-Line) = BC-ITEM(Ls-I)
             MOVE "Y" TO Ws-Cycle
             MOVE "Y" TO Ws-Stop
             MOVE BC-ITEM(Ls-I) TO Ws-Cycle-Item
             MOVE SPACES TO Ws-Cycle-Path
             STRING FUNCTION TRIM(Ls-Path) " -> "
                    FUNCTION TRIM(BC-ITEM(Ls-I))
                    " which is already on this path"
                 DELIMITED BY SIZE INTO Ws-Cycle-Path
             MOVE BC-LINE-NUM(Ls-I) TO Ws-Cycle-Line
         END-IF
     END-PERFORM.

 Walk-Into-Child.
     INITIALIZE Ls-Child-Ca
     MOVE "WALK" TO CB-FUNCTION
     MOVE Ws-Company TO CB-COMPANY
     MOVE Ws-Site TO CB-SITE
     MOVE Ws-User TO CB-USER
     MOVE Ws-Eff-Date TO CB-BUSINESS-DATE
     MOVE BC-ITEM(Ls-I) TO CD-ROOT-ITEM
     MOVE BC-REV(Ls-I) TO CD-ROOT-REV
     COMPUTE CD-MAX-DEPTH = Ls-Depth + 1
     MOVE SPACES TO CD-CYCLE-PATH
     STRING FUNCTION TRIM(Ls-Path) " -> "
            FUNCTION TRIM(BC-ITEM(Ls-I))
         DELIMITED BY SIZE INTO CD-CYCLE-PATH
     CALL "CYCLECHK" USING Ls-Child-Ca.

*>----------------------------------------------------------------
 Publish-Outcome.
     MOVE Ws-Visited TO CC-NODES-VISITED
     MOVE Ws-Deepest TO CC-DEPTH-REACHED
     MOVE Ws-Shared TO CC-SHARED-CNT
     MOVE Ws-Limit-Hit TO CC-LIMIT-HIT
     MOVE Ws-Cycle TO CC-CYCLE-FOUND
     MOVE Ws-Cycle-Item TO CC-CYCLE-ITEM
     MOVE Ws-Cycle-Path TO CC-CYCLE-PATH
     MOVE Ws-Cycle-Line TO CC-CYCLE-LINE
     IF Ws-Cycle = "Y"
         MOVE 8 TO CA-RETURN-CODE
         MOVE "BOM0402" TO CA-REASON-CODE
         MOVE SPACES TO CA-MESSAGE
         STRING "Cyclic structure: " FUNCTION TRIM(Ws-Cycle-Path)
             DELIMITED BY SIZE INTO CA-MESSAGE
         ADD 1 TO CA-MSG-COUNT
         EXIT PARAGRAPH
     END-IF
     IF Ws-Limit-Hit = "Y"
         MOVE 4 TO CA-RETURN-CODE
         MOVE "BOM0403" TO CA-REASON-CODE
         MOVE "The walk reached its depth limit before the end"
             TO CA-MESSAGE
         ADD 1 TO CA-MSG-COUNT
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO CA-RETURN-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "No cyclic path found; shared descendants are valid"
         DELIMITED BY SIZE INTO CA-MESSAGE.
