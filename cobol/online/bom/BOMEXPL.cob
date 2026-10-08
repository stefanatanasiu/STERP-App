>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : BOMEXPL  (IS RECURSIVE)
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : Nested explosion of a bill of materials.  The
*>              program calls itself once per manufactured or
*>              phantom node, so each level keeps its own bill, its
*>              own counters and its own child request in
*>              LOCAL-STORAGE while the result pool, the ancestry
*>              stack and the completeness state stay in
*>              WORKING-STORAGE, shared by every level.
*>
*>              Rules that matter and are easy to get wrong:
*>              - a child requirement becomes that child's required
*>                GOOD output; the parent's yield is not applied to
*>                it again (BOM-008, BOM-012);
*>              - a phantom has no stock, no independent rounding,
*>                no run or batch of its own and adds no value; it
*>                simply scales its children (BOM-017);
*>              - the same subassembly under two parents is two
*>                occurrences, not a cycle and not a duplicate to be
*>                suppressed (BOM-019);
*>              - a purchased stop stays purchased even when a
*>                reference bill exists (BOM-016);
*>              - nothing is ever silently truncated: a depth
*>                limit, a capacity limit, a missing child bill, a
*>                missing item or a cycle all produce a named
*>                incomplete outcome with the path (BOM-029).
*> CALLED BY  : BOMLEAF, BOMNET, BOMCOST, BOMCOMP, ECOIMPCT, the
*>              batch reports, the .NET client, itself
*> INTERFACE  : CALL "BOMEXPL" USING WS-BOMEXPL-CA
*>              COPYBOOK BOMEXPLCA
*> CALLS      : BOMSELCT, BOMREAD, BOMMATH, BOMWORK, BOMSUPPL,
*>              SQLIO (STORE only)
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2014-07-21 AJS  CR2228 ENGINEERING REWRITE.  THE 1996 PROGRAM
*>                 BOMX01 FLATTENED AS IT WENT AND COULD NOT SHOW
*>                 A PATH; THIS ONE KEEPS EVERY OCCURRENCE.
*> 2015-03-11 AJS  CR2301 PHANTOM GROUPING WITHOUT STOCK
*> 2015-10-05 AJS  CR2377 EXPLICIT GOOD OUTPUT SPLIT INTO RUNS
*> 2016-11-08 AJS  CR2502 APPROVED RUN CONSOLIDATION (D-10)
*> 2026-10-05 STERP STOP REASONS, COMPLETENESS, STORED RESULTS
*>================================================================
*> REQ: BOM-006, BOM-007, BOM-008, BOM-010, BOM-011, BOM-012,
*> REQ: BOM-013, BOM-014, BOM-016, BOM-017, BOM-018, BOM-019,
*> REQ: BOM-020, BOM-026, BOM-027, BOM-029, BOM-030, BOM-032
 IDENTIFICATION DIVISION.
 PROGRAM-ID. BOMEXPL IS RECURSIVE.
 AUTHOR. A J SPENCER.
 DATE-WRITTEN. 21/07/2014.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY BOMWKCTL.
 COPY BOMOCCUR.
 COPY BOMSUPCT.
*> one calculation context, shared by every recursion level
 01  Ws-Run.
     05  Ws-Company              PIC X(10) VALUE SPACES.
     05  Ws-Site                 PIC X(10) VALUE SPACES.
     05  Ws-User                 PIC X(20) VALUE SPACES.
     05  Ws-Eff-Date             PIC 9(8) VALUE 0.
     05  Ws-Purpose              PIC X(10) VALUE SPACES.
     05  Ws-Variant              PIC X(20) VALUE SPACES.
     05  Ws-Job-Ref              PIC X(20) VALUE SPACES.
     05  Ws-Calc-Type            PIC X(10) VALUE SPACES.
     05  Ws-Max-Depth            PIC S9(4) VALUE 25.
     05  Ws-Consolidate          PIC X VALUE "N".
     05  Ws-Expand-Phantom       PIC X VALUE "Y".
     05  Ws-Include-Notes        PIC X VALUE "Y".
     05  Ws-Include-Byprod       PIC X VALUE "Y".
     05  Ws-Net-Mode             PIC X VALUE "N".
     05  Ws-Use-Inbound          PIC X VALUE "N".
     05  Ws-Stock-Source         PIC X(8) VALUE SPACES.
     05  Ws-Completeness         PIC X(12) VALUE "COMPLETE".
     05  Ws-Reason               PIC X(8) VALUE SPACES.
     05  Ws-Message              PIC X(200) VALUE SPACES.
     05  Ws-Error-Path           PIC X(200) VALUE SPACES.
     05  Ws-Cycle-Path           PIC X(200) VALUE SPACES.
     05  Ws-Warning-Cnt          PIC S9(4) VALUE 0.
     05  Ws-Abort                PIC X VALUE "N".
     05  Ws-Opt-Cnt              PIC S9(4) VALUE 0.
     05  Ws-Options OCCURS 10 TIMES.
         10  Wo-Group            PIC X(10).
         10  Wo-Choice           PIC X(40).
 01  Ws-Ancestry.
     05  Ws-Anc OCCURS 30 TIMES.
         10  Wa-Item             PIC X(40).
         10  Wa-Rev              PIC X(4).
         10  Wa-Bom-Id           PIC S9(9) COMP-5.
 01  Ws-Shared-Work.
     05  Ws-Depth-Reached        PIC S9(4) VALUE 0.
     05  Ws-Occ-Total            PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Root-Seq             PIC S9(9) COMP-5 VALUE 0.
     05  Ws-View-I               PIC S9(9) COMP-5 VALUE 0.
     05  Ws-View-W               PIC S9(4) VALUE 0.
     05  Ws-Keep                 PIC X VALUE "N".
     05  Ws-Store-Line           PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Edit                 PIC -(14)9.9(6).
     05  Ws-Mny-Edit             PIC -(12)9.9(2).
 01  Ws-Byp-Row.
     05  By-Line-Num             PIC X(8).
     05  By-Item                 PIC X(40).
     05  By-Qty                  PIC X(24).
     05  By-Uom                  PIC X(10).
     05  By-Conv                 PIC X(24).
     05  By-Basis                PIC X(12).
     05  By-Value                PIC X(24).
     05  By-Disposition          PIC X(60).
     05  By-Approved             PIC X(20).
 01  Ws-Id-Row.
     05  Xi-Id                   PIC X(12).
 COPY BOMMATHCA REPLACING ==DFHCOMMAREA== BY ==Ws-Math-Ca==
      LEADING ==CA-== BY ==MA-==
      LEADING ==BM-== BY ==MM-==.
 COPY BOMSELCTCA REPLACING ==DFHCOMMAREA== BY ==Ws-Sel-Ca==
      LEADING ==CA-== BY ==SA-==
      LEADING ==BS-== BY ==SS-==.
 LOCAL-STORAGE SECTION.
 COPY BOMBILL.
 01  Ls-Node.
     05  Ls-I                    PIC S9(4) VALUE 0.
     05  Ls-J                    PIC S9(4) VALUE 0.
     05  Ls-Depth                PIC S9(4) VALUE 0.
     05  Ls-My-Seq               PIC S9(9) COMP-5 VALUE 0.
     05  Ls-Child-Seq            PIC S9(9) COMP-5 VALUE 0.
     05  Ls-Good-Qty             PIC S9(15)V9(6) VALUE 0.
     05  Ls-Gross                PIC S9(15)V9(6) VALUE 0.
     05  Ls-Runs                 PIC S9(9) COMP-5 VALUE 0.
     05  Ls-Batches              PIC S9(9) COMP-5 VALUE 0.
     05  Ls-Basis                PIC S9(13)V9(6) VALUE 0.
     05  Ls-Math-Qty             PIC S9(15)V9(6) VALUE 0.
     05  Ls-Path                 PIC X(160) VALUE SPACES.
     05  Ls-Child-Path           PIC X(160) VALUE SPACES.
     05  Ls-Is-Phantom           PIC X VALUE "N".
     05  Ls-Line-Edit            PIC Z(3)9.
     05  Ls-Skip                 PIC X VALUE "N".
     05  Ls-Expand               PIC X VALUE "N".
     05  Ls-Node-Type            PIC X(8) VALUE SPACES.
     05  Ls-Stop                 PIC X(20) VALUE SPACES.
     05  Ls-Unrounded            PIC S9(15)V9(9) VALUE 0.
     05  Ls-Required             PIC S9(15)V9(6) VALUE 0.
     05  Ls-Round-Inc            PIC S9(13)V9(6) VALUE 0.
     05  Ls-Taken                PIC S9(15)V9(6) VALUE 0.
     05  Ls-To-Make              PIC S9(15)V9(6) VALUE 0.
     05  Ls-Group-Hit            PIC S9(4) VALUE 0.
     05  Ls-Chosen-Line          PIC S9(4) VALUE 0.
     05  Ls-K                    PIC S9(4) VALUE 0.
     05  Ls-Cycle                PIC X VALUE "N".
     05  Ls-Bill-Ok              PIC X VALUE "N".
     05  Ls-Group-Set            PIC X VALUE "N".
     05  Ls-More                 PIC X VALUE "Y".
     05  Ls-Yield-Contrib        PIC S9(15)V9(9) VALUE 0.
     05  Ls-Scrap-Contrib        PIC S9(15)V9(9) VALUE 0.
     05  Ls-Pack-Qty             PIC S9(15)V9(6) VALUE 0.
     05  Ls-Excess               PIC S9(15)V9(6) VALUE 0.
     05  Ls-Warning              PIC X(100) VALUE SPACES.
*> per level: what each component line resolved to, so that an
*> approved run consolidation can merge sibling demands for the
*> same made child before any of them is expanded
 01  Ls-Demand.
     05  Ls-Dem-Cnt              PIC S9(4) VALUE 0.
     05  Ls-Dem OCCURS 500 TIMES.
         10  Ld-Line             PIC S9(4).
         10  Ld-Seq              PIC S9(9) COMP-5.
         10  Ld-Item             PIC X(40).
         10  Ld-Rev              PIC X(4).
         10  Ld-Qty              PIC S9(15)V9(6).
         10  Ld-Phantom          PIC X.
         10  Ld-Expand           PIC X.
 COPY BOMEXPLCA REPLACING ==DFHCOMMAREA== BY ==Ls-Child-Ca==
      LEADING ==CA-== BY ==CC-==
      LEADING ==BX-== BY ==CX-==.
 LINKAGE SECTION.
 COPY BOMEXPLCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     EVALUATE CA-FUNCTION
         WHEN "EXPLODE"  PERFORM Run-Explosion
         WHEN "NODE"     PERFORM Expand-One-Node
         WHEN "GETLINE"  PERFORM Page-The-Result
         WHEN "LEAFQTY"  PERFORM Total-One-Item
         WHEN "STORE"    PERFORM Store-The-Result
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE "BOMEXPL" TO CA-PROGRAM
     MOVE 8 TO CA-RETURN-CODE
     MOVE "BOM0501" TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "BOMEXPL does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>================================================================
*> EXPLODE - the outermost call.  Sets the context, selects the
*> root bill, seeds the root occurrence and hands over to the
*> recursive node expansion.
 Run-Explosion.
     PERFORM Initialise-Run
     IF Ws-Abort = "Y"
         PERFORM Publish-Summary
         EXIT PARAGRAPH
     END-IF
     PERFORM Clear-The-Pool
     PERFORM Resolve-Root-Item
     IF Ws-Abort = "Y"
         PERFORM Publish-Summary
         EXIT PARAGRAPH
     END-IF
     PERFORM Select-Root-Bill
     IF Ws-Abort = "Y"
         PERFORM Publish-Summary
         EXIT PARAGRAPH
     END-IF
     PERFORM Seed-Root-Occurrence
     IF Ws-Abort = "N"
         PERFORM Call-Root-Node
     END-IF
     PERFORM Aggregate-And-Publish.

 Initialise-Run.
     MOVE "BOMEXPL" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
     MOVE "BOM-012" TO CA-REQ-REF
     MOVE "N" TO Ws-Abort
     MOVE "COMPLETE" TO Ws-Completeness
     MOVE SPACES TO Ws-Reason Ws-Message Ws-Error-Path Ws-Cycle-Path
     MOVE 0 TO Ws-Warning-Cnt Ws-Depth-Reached Ws-Occ-Total
     MOVE 0 TO Ws-Root-Seq
     MOVE CA-COMPANY TO Ws-Company
     MOVE CA-SITE TO Ws-Site
     MOVE CA-USER TO Ws-User
     MOVE FUNCTION UPPER-CASE(BX-PARENT-ITEM) TO BX-PARENT-ITEM
     IF BX-EFF-DATE = 0
         MOVE CA-BUSINESS-DATE TO BX-EFF-DATE
     END-IF
     MOVE BX-EFF-DATE TO Ws-Eff-Date
     IF BX-PURPOSE = SPACES
         MOVE "STANDARD" TO BX-PURPOSE
     END-IF
     MOVE BX-PURPOSE TO Ws-Purpose
     MOVE BX-VARIANT TO Ws-Variant
     MOVE BX-JOB-REF TO Ws-Job-Ref
     IF BX-CALC-TYPE = SPACES
         MOVE "GROSS" TO BX-CALC-TYPE
     END-IF
     MOVE BX-CALC-TYPE TO Ws-Calc-Type
     IF BX-CALC-TYPE = "NET"
         MOVE "Y" TO Ws-Net-Mode
     ELSE
         MOVE "N" TO Ws-Net-Mode
     END-IF
     IF BX-MAX-DEPTH < 1 OR BX-MAX-DEPTH > 25
         MOVE 25 TO BX-MAX-DEPTH
     END-IF
     MOVE BX-MAX-DEPTH TO Ws-Max-Depth
     MOVE BX-CONSOLIDATE TO Ws-Consolidate
     IF BX-EXPAND-PHANTOM = SPACES
         MOVE "Y" TO BX-EXPAND-PHANTOM
     END-IF
     MOVE BX-EXPAND-PHANTOM TO Ws-Expand-Phantom
     MOVE BX-INCLUDE-NOTES TO Ws-Include-Notes
     MOVE BX-INCLUDE-BYPROD TO Ws-Include-Byprod
     IF BX-STOCK-SOURCE = SPACES
         MOVE "LIVE" TO BX-STOCK-SOURCE
     END-IF
     MOVE BX-STOCK-SOURCE TO Ws-Stock-Source
     MOVE BX-USE-INBOUND TO Ws-Use-Inbound
     MOVE 0 TO Ws-Opt-Cnt
     IF BX-OPT-CNT > 0 AND BX-OPT-CNT <= 10
         MOVE BX-OPT-CNT TO Ws-Opt-Cnt
         PERFORM VARYING Ls-I FROM 1 BY 1 UNTIL Ls-I > Ws-Opt-Cnt
             MOVE BX-O-GROUP(Ls-I) TO Wo-Group(Ls-I)
             MOVE BX-O-CHOICE(Ls-I) TO Wo-Choice(Ls-I)
         END-PERFORM
     END-IF
     PERFORM VARYING Ls-I FROM 1 BY 1 UNTIL Ls-I > 30
         MOVE SPACES TO Wa-Item(Ls-I) Wa-Rev(Ls-I)
         MOVE 0 TO Wa-Bom-Id(Ls-I)
     END-PERFORM
     IF BX-PARENT-ITEM = SPACES
         MOVE "BOM0502" TO Ws-Reason
         MOVE "A parent item is required" TO Ws-Message
         PERFORM Fail-The-Run
         EXIT PARAGRAPH
     END-IF
     IF BX-REQ-QTY < 0
         MOVE "BOM0503" TO Ws-Reason
         MOVE "Required good output cannot be negative"
             TO Ws-Message
         PERFORM Fail-The-Run
     END-IF.

 Fail-The-Run.
     MOVE "ERROR" TO Ws-Completeness
     MOVE "Y" TO Ws-Abort.

 Clear-The-Pool.
     MOVE "CLEAR" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW.

*> The root parent must exist in the item master before anything
*> is calculated against it.
 Resolve-Root-Item.
     INITIALIZE BOM-RD-CTL
     MOVE "ITEM" TO BR-FUNC
     MOVE Ws-Company TO BR-COMPANY
     MOVE Ws-Site TO BR-SITE
     MOVE BX-PARENT-ITEM TO BR-ITEM-CODE
     MOVE 500 TO BR-MAX-LINES
     CALL "BOMREAD" USING BOM-RD-CTL BOM-HDR-REC BOM-CMP-TAB
     EVALUATE BR-STATUS
         WHEN "OK"       CONTINUE
         WHEN "SQLFAIL"
             MOVE "SQL0001" TO Ws-Reason
             MOVE BR-MESSAGE TO Ws-Message
             PERFORM Fail-The-Run
         WHEN OTHER
             MOVE "BOM0504" TO Ws-Reason
             MOVE BR-MESSAGE TO Ws-Message
             MOVE BX-PARENT-ITEM TO Ws-Error-Path
             PERFORM Fail-The-Run
     END-EVALUATE.

 Select-Root-Bill.
     INITIALIZE Ws-Sel-Ca
     MOVE "SELECT" TO SA-FUNCTION
     MOVE Ws-Company TO SA-COMPANY
     MOVE Ws-Site TO SA-SITE
     MOVE Ws-User TO SA-USER
     MOVE Ws-Eff-Date TO SA-BUSINESS-DATE
     MOVE BX-PARENT-ITEM TO SS-PARENT-ITEM
     MOVE BX-PARENT-REV TO SS-PARENT-REV
     MOVE Ws-Purpose TO SS-PURPOSE
     MOVE Ws-Job-Ref TO SS-JOB-REF
     MOVE Ws-Variant TO SS-VARIANT
     MOVE Ws-Eff-Date TO SS-EFF-DATE
     CALL "BOMSELCT" USING Ws-Sel-Ca
     IF SA-RETURN-CODE > 4
         MOVE SA-REASON-CODE TO Ws-Reason
         MOVE SA-MESSAGE TO Ws-Message
         MOVE BX-PARENT-ITEM TO Ws-Error-Path
         PERFORM Fail-The-Run
         EXIT PARAGRAPH
     END-IF
     MOVE SS-BOM-ID TO BX-BOM-ID
     MOVE SS-BOM-REF TO BX-BOM-REF
     MOVE SS-BOM-REV TO BX-BOM-REV
     MOVE SS-NOMINAL-BASIS TO BX-NOMINAL-BASIS
     MOVE SS-YIELD TO BX-YIELD
     MOVE SS-QUANTUM TO BX-QUANTUM
     MOVE SS-BATCH-SIZE TO BX-BATCH-SIZE
     IF SS-CONSOLIDATE = "Y"
         MOVE "Y" TO Ws-Consolidate
     END-IF.

*> The root occurrence is the calculation's own demand, recorded
*> so that the tree, the leaf list and the cost reconcile against
*> one visible starting point.
 Seed-Root-Occurrence.
     INITIALIZE BOM-OCC-ROW
     MOVE 0 TO BO-PARENT-SEQ OF BOM-OCC-ROW
     MOVE 0 TO BO-DEPTH OF BOM-OCC-ROW
     MOVE 0 TO BO-LINE-NUM OF BOM-OCC-ROW
     MOVE BX-PARENT-ITEM TO BO-PATH OF BOM-OCC-ROW
     MOVE BX-PARENT-ITEM TO BO-OCC-REF OF BOM-OCC-ROW
     MOVE SPACES TO BO-PARENT-ITEM OF BOM-OCC-ROW
     MOVE BX-PARENT-ITEM TO BO-ITEM OF BOM-OCC-ROW
     MOVE BX-PARENT-REV TO BO-ITEM-REV OF BOM-OCC-ROW
     MOVE BC-DESC(1) TO BO-DESCRIPTION OF BOM-OCC-ROW
     MOVE BX-BOM-ID TO BO-BOM-ID OF BOM-OCC-ROW
     MOVE BX-BOM-REV TO BO-BOM-REV OF BOM-OCC-ROW
     MOVE "INTERMED" TO BO-NODE-TYPE OF BOM-OCC-ROW
     MOVE "MAKE" TO BO-SUPPLY OF BOM-OCC-ROW
     MOVE "ROOT" TO BO-STOP-REASON OF BOM-OCC-ROW
     MOVE "PROPORTION" TO BO-USAGE-BASIS OF BOM-OCC-ROW
     MOVE BC-STOCK-UOM(1) TO BO-UOM OF BOM-OCC-ROW
     MOVE BC-STOCK-UOM(1) TO BO-STOCK-UOM OF BOM-OCC-ROW
     MOVE 1 TO BO-USAGE-QTY OF BOM-OCC-ROW
     MOVE 1 TO BO-CONV-FACT OF BOM-OCC-ROW
     MOVE 0 TO BO-SCRAP-PCT OF BOM-OCC-ROW
     MOVE BX-REQ-QTY TO BO-UNROUNDED OF BOM-OCC-ROW
     MOVE BX-REQ-QTY TO BO-REQ-QTY OF BOM-OCC-ROW
     MOVE BX-REQ-QTY TO BO-GOOD-QTY OF BOM-OCC-ROW
     MOVE BX-NOMINAL-BASIS TO BO-NOMINAL-BASIS OF BOM-OCC-ROW
     MOVE BX-YIELD TO BO-YIELD OF BOM-OCC-ROW
     MOVE BX-QUANTUM TO BO-QUANTUM OF BOM-OCC-ROW
     MOVE BX-BATCH-SIZE TO BO-BATCH-SIZE OF BOM-OCC-ROW
     MOVE "ADD" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     IF BW-STATUS NOT = "OK"
         PERFORM Report-Capacity
         EXIT PARAGRAPH
     END-IF
     MOVE BW-INDEX TO Ws-Root-Seq.

 Report-Capacity.
     MOVE "BOM0524" TO Ws-Reason
     MOVE SPACES TO Ws-Message
     STRING "The calculation exceeded the supported occurrence "
            "capacity and is incomplete"
         DELIMITED BY SIZE INTO Ws-Message
     MOVE "INCOMPLETE" TO Ws-Completeness
     MOVE "Y" TO Ws-Abort.

*> Hand the root to the recursive expansion.  Everything below is
*> the same program entered again.
 Call-Root-Node.
     INITIALIZE Ls-Child-Ca
     MOVE "NODE" TO CC-FUNCTION
     MOVE Ws-Company TO CC-COMPANY
     MOVE Ws-Site TO CC-SITE
     MOVE Ws-User TO CC-USER
     MOVE Ws-Eff-Date TO CC-BUSINESS-DATE
     MOVE BX-PARENT-ITEM TO CX-PARENT-ITEM
     MOVE BX-PARENT-REV TO CX-PARENT-REV
     MOVE BX-BOM-ID TO CX-BOM-ID
     MOVE BX-BOM-REV TO CX-BOM-REV
     MOVE BX-REQ-QTY TO CX-REQ-QTY
     MOVE Ws-Root-Seq TO CX-NODE-SEQ
     MOVE 0 TO CX-NODE-DEPTH
     MOVE BX-PARENT-ITEM TO CX-NODE-PATH
     MOVE "N" TO CX-NODE-PHANTOM
     MOVE BX-SPLIT-CNT TO CX-SPLIT-CNT
     PERFORM VARYING Ls-I FROM 1 BY 1 UNTIL Ls-I > 10
         MOVE BX-SPLIT-QTY(Ls-I) TO CX-SPLIT-QTY(Ls-I)
     END-PERFORM
     CALL "BOMEXPL" USING Ls-Child-Ca
     MOVE CX-GROSS-STARTS TO BX-GROSS-STARTS
     MOVE CX-RUN-COUNT TO BX-RUN-COUNT
     MOVE CX-BATCH-COUNT TO BX-BATCH-COUNT.

 Aggregate-And-Publish.
     MOVE "AGGREG" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     MOVE BW-COUNT TO BX-LEAF-COUNT
     PERFORM Propose-Leaf-Packs
     MOVE "COUNT" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     MOVE BW-COUNT TO BX-OCC-COUNT
     MOVE BW-MAX-DEPTH TO BX-DEPTH-REACHED
     MOVE BW-COUNT TO Ws-Occ-Total
     PERFORM Publish-Summary.

 Publish-Summary.
     MOVE Ws-Completeness TO BX-COMPLETENESS
     MOVE Ws-Error-Path TO BX-ERROR-PATH
     MOVE Ws-Cycle-Path TO BX-CYCLE-PATH
     MOVE Ws-Warning-Cnt TO BX-WARNING-CNT
     EVALUATE Ws-Completeness
         WHEN "COMPLETE"
             IF Ws-Warning-Cnt > 0
                 MOVE 4 TO CA-RETURN-CODE
             ELSE
                 MOVE 0 TO CA-RETURN-CODE
             END-IF
             IF CA-MESSAGE = SPACES
                 MOVE SPACES TO CA-MESSAGE
                 MOVE BX-OCC-COUNT TO Ls-Line-Edit
                 STRING "Explosion complete - "
                        FUNCTION TRIM(Ls-Line-Edit)
                        " occurrence(s)"
                     DELIMITED BY SIZE INTO CA-MESSAGE
             END-IF
         WHEN "INCOMPLETE"
             MOVE 4 TO CA-RETURN-CODE
             MOVE Ws-Reason TO CA-REASON-CODE
             MOVE Ws-Message TO CA-MESSAGE
             ADD 1 TO CA-MSG-COUNT
         WHEN OTHER
             MOVE 8 TO CA-RETURN-CODE
             MOVE Ws-Reason TO CA-REASON-CODE
             MOVE Ws-Message TO CA-MESSAGE
             ADD 1 TO CA-MSG-COUNT
     END-EVALUATE
     IF Ws-Reason = "SQL0001"
         MOVE 12 TO CA-RETURN-CODE
     END-IF.
*>================================================================
*> NODE - one manufactured or phantom node.  Entered by the
*> program calling itself, so Ls- items below belong to this level
*> only while Ws- items are the shared state of the calculation.
 Expand-One-Node.
     MOVE "BOMEXPL" TO CA-PROGRAM
     MOVE BX-NODE-DEPTH TO Ls-Depth
     MOVE BX-NODE-SEQ TO Ls-My-Seq
     MOVE BX-NODE-PATH TO Ls-Path
     MOVE BX-NODE-PHANTOM TO Ls-Is-Phantom
     MOVE BX-REQ-QTY TO Ls-Good-Qty
     MOVE 0 TO Ls-Dem-Cnt
     IF Ls-Depth > Ws-Depth-Reached
         MOVE Ls-Depth TO Ws-Depth-Reached
     END-IF
     IF Ws-Abort = "Y"
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ls-J = Ls-Depth + 1
     MOVE BX-PARENT-ITEM TO Wa-Item(Ls-J)
     MOVE BX-PARENT-REV TO Wa-Rev(Ls-J)
     MOVE BX-BOM-ID TO Wa-Bom-Id(Ls-J)
     PERFORM Read-This-Bill
     IF Ws-Abort = "Y"
         EXIT PARAGRAPH
     END-IF
     PERFORM Compute-Parent-Quantities
     IF Ws-Abort = "Y"
         EXIT PARAGRAPH
     END-IF
     PERFORM Update-Node-Occurrence
     PERFORM Build-Child-Demands
     IF Ws-Include-Byprod = "Y" AND Ls-Is-Phantom NOT = "Y"
         PERFORM Add-By-Products
     END-IF
     IF Ws-Consolidate = "Y"
         PERFORM Consolidate-Sibling-Runs
     END-IF
     PERFORM Expand-Child-Demands.

 Read-This-Bill.
     INITIALIZE BOM-RD-CTL
     MOVE "BILL" TO BR-FUNC
     MOVE Ws-Company TO BR-COMPANY
     MOVE Ws-Site TO BR-SITE
     MOVE BX-BOM-ID TO BR-BOM-ID
     MOVE Ws-Eff-Date TO BR-EFF-DATE
     MOVE 500 TO BR-MAX-LINES
     CALL "BOMREAD" USING BOM-RD-CTL BOM-HDR-REC BOM-CMP-TAB
     EVALUATE BR-STATUS
         WHEN "OK"      CONTINUE
         WHEN "TOOMANY"
             MOVE "BOM0525" TO Ws-Reason
             MOVE BR-MESSAGE TO Ws-Message
             MOVE Ls-Path TO Ws-Error-Path
             MOVE "INCOMPLETE" TO Ws-Completeness
             MOVE "Y" TO Ws-Abort
         WHEN "SQLFAIL"
             MOVE "SQL0001" TO Ws-Reason
             MOVE BR-MESSAGE TO Ws-Message
             PERFORM Fail-The-Run
         WHEN OTHER
             MOVE "BOM0505" TO Ws-Reason
             MOVE BR-MESSAGE TO Ws-Message
             MOVE Ls-Path TO Ws-Error-Path
             PERFORM Fail-The-Run
     END-EVALUATE.

*>----------------------------------------------------------------
*> Gross starts, runs and batches for this node.  A phantom has
*> none of its own: it is a grouping, so its quantity simply
*> scales its children through its nominal basis (BOM-017).
*> An explicitly recorded split is calculated run by run and then
*> summed - pooling the gross starts first would understate the
*> batch count (specification section 5).
 Compute-Parent-Quantities.
     MOVE BH-NOMINAL-BASIS TO Ls-Basis
     IF Ls-Is-Phantom = "Y"
         MOVE Ls-Good-Qty TO Ls-Gross
         MOVE 0 TO Ls-Runs
         MOVE 0 TO Ls-Batches
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO Ls-Gross Ls-Runs Ls-Batches
     IF BX-SPLIT-CNT > 1
         PERFORM VARYING Ls-I FROM 1 BY 1 UNTIL Ls-I > BX-SPLIT-CNT
             IF BX-SPLIT-QTY(Ls-I) > 0
                 MOVE BX-SPLIT-QTY(Ls-I) TO Ls-Math-Qty
                 PERFORM Call-Parent-Maths
                 IF Ws-Abort = "Y"
                     EXIT PARAGRAPH
                 END-IF
                 ADD MM-GROSS-STARTS TO Ls-Gross
                 ADD MM-BATCH-COUNT TO Ls-Batches
                 ADD 1 TO Ls-Runs
             END-IF
         END-PERFORM
     ELSE
         MOVE Ls-Good-Qty TO Ls-Math-Qty
         PERFORM Call-Parent-Maths
         IF Ws-Abort = "Y"
             EXIT PARAGRAPH
         END-IF
         MOVE MM-GROSS-STARTS TO Ls-Gross
         MOVE MM-BATCH-COUNT TO Ls-Batches
         IF Ls-Good-Qty > 0
             MOVE 1 TO Ls-Runs
         END-IF
     END-IF
     MOVE Ls-Gross TO BX-GROSS-STARTS
     MOVE Ls-Runs TO BX-RUN-COUNT
     MOVE Ls-Batches TO BX-BATCH-COUNT.

 Call-Parent-Maths.
     INITIALIZE Ws-Math-Ca
     MOVE "PARENT" TO MA-FUNCTION
     MOVE Ws-Company TO MA-COMPANY
     MOVE Ws-Eff-Date TO MA-BUSINESS-DATE
     MOVE Ls-Math-Qty TO MM-GOOD-QTY
     MOVE BH-YIELD TO MM-YIELD
     MOVE BH-QUANTUM TO MM-QUANTUM
     MOVE BH-BATCH-SIZE TO MM-BATCH-SIZE
     MOVE 1 TO MM-RUN-COUNT
     CALL "BOMMATH" USING Ws-Math-Ca
     IF MA-RETURN-CODE > 4
         MOVE MA-REASON-CODE TO Ws-Reason
         MOVE MA-MESSAGE TO Ws-Message
         MOVE Ls-Path TO Ws-Error-Path
         PERFORM Fail-The-Run
     END-IF.

 Update-Node-Occurrence.
     MOVE Ls-My-Seq TO BW-INDEX
     MOVE "GET" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     IF BW-STATUS NOT = "OK"
         EXIT PARAGRAPH
     END-IF
     MOVE Ls-Gross TO BO-GROSS-STARTS
     MOVE Ls-Runs TO BO-RUN-COUNT
     MOVE Ls-Batches TO BO-BATCH-COUNT
     MOVE BH-NOMINAL-BASIS TO BO-NOMINAL-BASIS
     MOVE BH-YIELD TO BO-YIELD
     MOVE BH-QUANTUM TO BO-QUANTUM
     MOVE BH-BATCH-SIZE TO BO-BATCH-SIZE
     MOVE BX-BOM-REV TO BO-BOM-REV
     MOVE BH-SETUP-COST TO BO-UNIT-COST
     MOVE BH-CONV-COST TO BO-BRANCH-COST
     MOVE Ls-My-Seq TO BW-INDEX
     MOVE "PUT" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW.

*>================================================================
*> One pass over the component lines of this bill.  Every line
*> that is in force produces exactly one occurrence, even when it
*> is a note, a service, an unusable item or a stop - BOM-029
*> forbids skipping anything silently.
 Build-Child-Demands.
     PERFORM VARYING Ls-I FROM 1 BY 1 UNTIL Ls-I > BR-LINE-COUNT
         IF Ws-Abort = "N"
             PERFORM Examine-One-Line
         END-IF
     END-PERFORM.

 Examine-One-Line.
     MOVE "N" TO Ls-Skip
     MOVE "N" TO Ls-Expand
     MOVE SPACES TO Ls-Warning Ls-Stop Ls-Node-Type
     MOVE 0 TO Ls-Unrounded Ls-Required Ls-Round-Inc
     MOVE 0 TO Ls-Taken Ls-To-Make
     PERFORM Test-Line-Effectivity
     IF Ls-Skip = "Y"
         EXIT PARAGRAPH
     END-IF
     PERFORM Test-Option-Selection
     IF Ls-Skip = "Y"
         EXIT PARAGRAPH
     END-IF
     PERFORM Classify-The-Line
     IF Ls-Skip = "Y"
         EXIT PARAGRAPH
     END-IF
     PERFORM Compute-Line-Requirement
     PERFORM Apply-Material-Rounding
     IF Ws-Net-Mode = "Y"
         PERFORM Consume-Eligible-Supply
     END-IF
     PERFORM Propose-Purchase-Pack
     PERFORM Record-The-Occurrence.

*> Half open [start, end): in force on the start date, out of
*> force on the end date (D-10).
 Test-Line-Effectivity.
     IF BC-EFF-FROM(Ls-I) NOT = 0
        AND BC-EFF-FROM(Ls-I) > Ws-Eff-Date
         MOVE "Y" TO Ls-Skip
         EXIT PARAGRAPH
     END-IF
     IF BC-EFF-TO(Ls-I) NOT = 0
        AND BC-EFF-TO(Ls-I) <= Ws-Eff-Date
         MOVE "Y" TO Ls-Skip
     END-IF.

*>----------------------------------------------------------------
*> Optional and alternate lines.  An alternate group needs exactly
*> one valid selected choice; with none, the configuration is
*> incomplete and the alternatives are certainly NOT all summed
*> (BOM-018).
 Test-Option-Selection.
     IF BC-ALT-GROUP(Ls-I) = SPACES
         IF BC-OPTIONAL(Ls-I) = "Y" AND BC-SELECTED(Ls-I) NOT = "Y"
             MOVE "Y" TO Ls-Skip
         END-IF
         EXIT PARAGRAPH
     END-IF
     PERFORM Find-Group-Choice
     IF Ls-Chosen-Line = 0
         IF Ls-Group-Hit = Ls-I
             MOVE "BOM0518" TO Ws-Reason
             MOVE SPACES TO Ws-Message
             STRING "Alternate group "
                    FUNCTION TRIM(BC-ALT-GROUP(Ls-I))
                    " has no selected choice - the configuration "
                    "is incomplete"
                 DELIMITED BY SIZE INTO Ws-Message
             MOVE Ls-Path TO Ws-Error-Path
             MOVE "INCOMPLETE" TO Ws-Completeness
             ADD 1 TO Ws-Warning-Cnt
             MOVE "Unconfigured alternate group" TO Ls-Warning
             MOVE "NOCHOICE" TO Ls-Stop
             MOVE "NOTE" TO Ls-Node-Type
             PERFORM Record-Unusable-Line
         END-IF
         MOVE "Y" TO Ls-Skip
         EXIT PARAGRAPH
     END-IF
     IF Ls-Chosen-Line NOT = Ls-I
         MOVE "Y" TO Ls-Skip
     END-IF.

*> The explicit configuration choice wins; otherwise the bill's
*> own selected flag decides.  Scanning the whole group first is
*> what makes "no selected choice" detectable instead of silently
*> summing every alternative.
 Find-Group-Choice.
     MOVE 0 TO Ls-Chosen-Line
     MOVE 0 TO Ls-Group-Hit
     MOVE "N" TO Ls-Group-Set
     IF Ws-Opt-Cnt > 0
         PERFORM VARYING Ls-K FROM 1 BY 1 UNTIL Ls-K > Ws-Opt-Cnt
             IF Wo-Group(Ls-K) = BC-ALT-GROUP(Ls-I)
                 MOVE "Y" TO Ls-Group-Set
             END-IF
         END-PERFORM
     END-IF
     PERFORM VARYING Ls-J FROM 1 BY 1 UNTIL Ls-J > BR-LINE-COUNT
         IF BC-ALT-GROUP(Ls-J) = BC-ALT-GROUP(Ls-I)
             IF Ls-Group-Hit = 0
                 MOVE Ls-J TO Ls-Group-Hit
             END-IF
             IF Ls-Group-Set = "Y"
                 PERFORM VARYING Ls-K FROM 1 BY 1
                     UNTIL Ls-K > Ws-Opt-Cnt
                     IF Wo-Group(Ls-K) = BC-ALT-GROUP(Ls-J)
                        AND (Wo-Choice(Ls-K) = BC-ITEM(Ls-J)
                             OR Wo-Choice(Ls-K) =
                                BC-OPTION-REF(Ls-J))
                         MOVE Ls-J TO Ls-Chosen-Line
                     END-IF
                 END-PERFORM
             ELSE
                 IF BC-SELECTED(Ls-J) = "Y"
                     MOVE Ls-J TO Ls-Chosen-Line
                 END-IF
             END-IF
         END-IF
     END-PERFORM.

*>----------------------------------------------------------------
*> Supply behaviour decides what kind of node this is and whether
*> expansion continues.  A purchased stop stays purchased even if
*> a reference bill exists for the item (BOM-016).
 Classify-The-Line.
     IF BC-ITEM-FOUND(Ls-I) NOT = "Y"
         MOVE "BOM0510" TO Ws-Reason
         MOVE SPACES TO Ws-Message
         STRING "Component " FUNCTION TRIM(BC-ITEM(Ls-I))
                " is not held in the item master"
             DELIMITED BY SIZE INTO Ws-Message
         MOVE Ls-Path TO Ws-Error-Path
         MOVE "INCOMPLETE" TO Ws-Completeness
         ADD 1 TO Ws-Warning-Cnt
         MOVE "Missing component identity" TO Ls-Warning
         MOVE "MISSINGITEM" TO Ls-Stop
         MOVE "LEAF" TO Ls-Node-Type
         PERFORM Record-Unusable-Line
         MOVE "Y" TO Ls-Skip
         EXIT PARAGRAPH
     END-IF
     IF BC-STOCK-UOM(Ls-I) = SPACES OR BC-CONV-FACT(Ls-I) <= 0
         MOVE "BOM0509" TO Ws-Reason
         MOVE SPACES TO Ws-Message
         STRING "Component " FUNCTION TRIM(BC-ITEM(Ls-I))
                " has no usable unit conversion"
             DELIMITED BY SIZE INTO Ws-Message
         MOVE Ls-Path TO Ws-Error-Path
         MOVE "INCOMPLETE" TO Ws-Completeness
         ADD 1 TO Ws-Warning-Cnt
         MOVE "Missing or zero conversion factor" TO Ls-Warning
         MOVE "BADUNIT" TO Ls-Stop
         MOVE "LEAF" TO Ls-Node-Type
         PERFORM Record-Unusable-Line
         MOVE "Y" TO Ls-Skip
         EXIT PARAGRAPH
     END-IF
     EVALUATE BC-SUPPLY(Ls-I)
         WHEN "NOTE"
             MOVE "NOTE" TO Ls-Node-Type
             MOVE "NOTE" TO Ls-Stop
             IF Ws-Include-Notes NOT = "Y"
                 MOVE "Y" TO Ls-Skip
             END-IF
         WHEN "SERVICE"
             MOVE "LEAF" TO Ls-Node-Type
             MOVE "SERVICE" TO Ls-Stop
         WHEN "COSTONLY"
             MOVE "LEAF" TO Ls-Node-Type
             MOVE "COSTONLY" TO Ls-Stop
         WHEN "NONSTOCK"
             MOVE "LEAF" TO Ls-Node-Type
             MOVE "NONSTOCK" TO Ls-Stop
         WHEN "PHANTOM"
             MOVE "PHANTOM" TO Ls-Node-Type
             IF Ws-Expand-Phantom = "Y"
                 MOVE "Y" TO Ls-Expand
                 MOVE "PHANTOM" TO Ls-Stop
             ELSE
                 MOVE "LEAF" TO Ls-Node-Type
                 MOVE "PHANTOMHELD" TO Ls-Stop
             END-IF
         WHEN "MAKE"
             MOVE "INTERMED" TO Ls-Node-Type
             MOVE "Y" TO Ls-Expand
             MOVE "MAKE" TO Ls-Stop
         WHEN "USESTOCK"
             MOVE "LEAF" TO Ls-Node-Type
             MOVE "USESTOCK" TO Ls-Stop
         WHEN OTHER
             MOVE "LEAF" TO Ls-Node-Type
             MOVE "PURCHASED" TO Ls-Stop
     END-EVALUATE.

 Compute-Line-Requirement.
     INITIALIZE Ws-Math-Ca
     MOVE "COMPON" TO MA-FUNCTION
     MOVE Ws-Company TO MA-COMPANY
     MOVE Ws-Eff-Date TO MA-BUSINESS-DATE
     MOVE Ls-Good-Qty TO MM-GOOD-QTY
     MOVE Ls-Gross TO MM-GROSS-STARTS
     MOVE Ls-Basis TO MM-NOMINAL-BASIS
     MOVE BC-USAGE-QTY(Ls-I) TO MM-USAGE-QTY
     MOVE BC-CONV-FACT(Ls-I) TO MM-CONV-FACT
     MOVE BC-SCRAP-PCT(Ls-I) TO MM-SCRAP-PCT
     MOVE BC-USAGE-BASIS(Ls-I) TO MM-USAGE-BASIS
     MOVE Ls-Runs TO MM-RUN-COUNT
     MOVE Ls-Batches TO MM-BATCH-COUNT
     CALL "BOMMATH" USING Ws-Math-Ca
     IF MA-RETURN-CODE > 4
         MOVE MA-REASON-CODE TO Ws-Reason
         MOVE MA-MESSAGE TO Ws-Message
         MOVE Ls-Path TO Ws-Error-Path
         MOVE "INCOMPLETE" TO Ws-Completeness
         ADD 1 TO Ws-Warning-Cnt
         MOVE MA-MESSAGE TO Ls-Warning
         MOVE 0 TO Ls-Unrounded
         EXIT PARAGRAPH
     END-IF
     MOVE MM-UNROUNDED TO Ls-Unrounded
     MOVE MM-YIELD-CONTRIB TO Ls-Yield-Contrib
     MOVE MM-SCRAP-CONTRIB TO Ls-Scrap-Contrib.

*> Round upward once, here, at an independently supplied or issued
*> material occurrence.  A phantom grouping and a note are not
*> such occurrences, so they keep full precision (BOM-011).
 Apply-Material-Rounding.
     MOVE Ls-Unrounded TO Ls-Required
     MOVE 0 TO Ls-Round-Inc
     IF Ls-Node-Type = "NOTE"
         MOVE 0 TO Ls-Required
         EXIT PARAGRAPH
     END-IF
     IF Ls-Node-Type = "PHANTOM"
         EXIT PARAGRAPH
     END-IF
     IF BC-SUPPLY(Ls-I) = "SERVICE" OR BC-SUPPLY(Ls-I) = "COSTONLY"
         EXIT PARAGRAPH
     END-IF
     IF BC-QUANTUM(Ls-I) <= 0
         ADD 1 TO Ws-Warning-Cnt
         MOVE "No allowed stock quantum - unrounded amount kept"
             TO Ls-Warning
         EXIT PARAGRAPH
     END-IF
     INITIALIZE Ws-Math-Ca
     MOVE "ROUNDUP" TO MA-FUNCTION
     MOVE Ls-Unrounded TO MM-UNROUNDED
     MOVE BC-QUANTUM(Ls-I) TO MM-QUANTUM
     CALL "BOMMATH" USING Ws-Math-Ca
     IF MA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     MOVE MM-REQ-QTY TO Ls-Required
     MOVE MM-ROUND-INC TO Ls-Round-Inc.

*>----------------------------------------------------------------
*> Netting.  Eligible supply is consumed once for the whole demand
*> scope.  A subassembly covered from stock is not expanded for
*> new material for the same units (BOM-020).
 Consume-Eligible-Supply.
     MOVE Ls-Required TO Ls-To-Make
     IF Ls-Required <= 0 OR Ls-Node-Type = "NOTE"
         EXIT PARAGRAPH
     END-IF
     IF BC-STOCKED(Ls-I) NOT = "Y"
         EXIT PARAGRAPH
     END-IF
     INITIALIZE BOM-SUP-CTL
     MOVE "TAKE" TO BP-FUNC
     MOVE Ws-Company TO BP-COMPANY
     MOVE Ws-Site TO BP-SITE
     MOVE Ws-Stock-Source TO BP-SOURCE
     MOVE BC-ITEM(Ls-I) TO BP-ITEM
     MOVE BC-REV(Ls-I) TO BP-REV
     MOVE Ls-Required TO BP-WANT
     MOVE Ws-Use-Inbound TO BP-USE-INBOUND
     CALL "BOMSUPPL" USING BOM-SUP-CTL
     MOVE BP-TAKEN TO Ls-Taken
     COMPUTE Ls-To-Make = Ls-Required - BP-TAKEN - BP-TAKEN-INBOUND
     IF Ls-To-Make < 0
         MOVE 0 TO Ls-To-Make
     END-IF
     IF Ls-Expand = "Y" AND Ls-To-Make = 0
         MOVE "N" TO Ls-Expand
         MOVE "FROMSTOCK" TO Ls-Stop
     END-IF.

*> Procurement pack rounding happens AFTER the compatible net
*> purchasing requirement is known - on the aggregated leaf, not
*> on each occurrence.  It creates excess purchased supply and
*> leaves the engineering requirement alone (B5, BOM-011).
 Propose-Purchase-Pack.
     MOVE 0 TO Ls-Pack-Qty Ls-Excess.

 Propose-Leaf-Packs.
     MOVE "LEAFCNT" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     MOVE BW-COUNT TO BX-LEAF-COUNT
     PERFORM VARYING Ws-View-I FROM 1 BY 1
         UNTIL Ws-View-I > BX-LEAF-COUNT
         MOVE Ws-View-I TO BW-INDEX
         MOVE "GETLEAF" TO BW-FUNC
         CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
         IF BW-STATUS = "OK" AND BO-PACK-MULT > 0
             INITIALIZE Ws-Math-Ca
             MOVE "PACK" TO MA-FUNCTION
             MOVE BO-REQ-QTY TO MM-REQ-QTY
             MOVE BO-STOCK-USED TO MM-FREE-STOCK
             MOVE BO-PACK-MULT TO MM-PACK-MULT
             CALL "BOMMATH" USING Ws-Math-Ca
             MOVE MM-PACK-QTY TO BO-PACK-QTY
             MOVE MM-EXCESS TO BO-EXCESS
             MOVE Ws-View-I TO BW-INDEX
             MOVE "PUTLEAF" TO BW-FUNC
             CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
         END-IF
     END-PERFORM.

 Record-The-Occurrence.
     PERFORM Build-Occurrence-Shell
     MOVE Ls-Unrounded TO BO-UNROUNDED
     MOVE Ls-Yield-Contrib TO BO-YIELD-CONTRIB
     MOVE Ls-Scrap-Contrib TO BO-SCRAP-CONTRIB
     MOVE Ls-Round-Inc TO BO-ROUND-INC
     MOVE Ls-Required TO BO-REQ-QTY
     MOVE Ls-Required TO BO-GOOD-QTY
     MOVE Ls-Taken TO BO-STOCK-USED
     IF Ws-Net-Mode = "Y" AND Ls-Node-Type = "LEAF"
         COMPUTE BO-SHORTAGE = Ls-Required - Ls-Taken
         IF BO-SHORTAGE < 0
             MOVE 0 TO BO-SHORTAGE
         END-IF
     END-IF
     IF Ws-Net-Mode = "Y" AND Ls-Expand = "Y"
         MOVE Ls-To-Make TO BO-GOOD-QTY
     END-IF
     MOVE Ls-Pack-Qty TO BO-PACK-QTY
     MOVE Ls-Excess TO BO-EXCESS
     PERFORM Add-Occurrence-To-Pool
     IF BW-STATUS NOT = "OK"
         EXIT PARAGRAPH
     END-IF
     IF Ls-Expand = "Y"
         PERFORM Remember-Child-Demand
     END-IF.

 Record-Unusable-Line.
     PERFORM Build-Occurrence-Shell
     MOVE 0 TO BO-UNROUNDED BO-REQ-QTY BO-GOOD-QTY
     MOVE Ls-Warning TO BO-WARNING
     PERFORM Add-Occurrence-To-Pool.

 Build-Occurrence-Shell.
     INITIALIZE BOM-OCC-ROW
     MOVE Ls-My-Seq TO BO-PARENT-SEQ
     COMPUTE BO-DEPTH = Ls-Depth + 1
     MOVE BC-LINE-NUM(Ls-I) TO BO-LINE-NUM
     MOVE BC-LINE-NUM(Ls-I) TO Ls-Line-Edit
     MOVE SPACES TO Ls-Child-Path
     STRING FUNCTION TRIM(Ls-Path) "/"
            FUNCTION TRIM(Ls-Line-Edit) ":"
            FUNCTION TRIM(BC-ITEM(Ls-I))
         DELIMITED BY SIZE INTO Ls-Child-Path
     MOVE Ls-Child-Path TO BO-PATH
     MOVE Ls-Child-Path TO BO-OCC-REF
     MOVE BX-PARENT-ITEM TO BO-PARENT-ITEM
     MOVE BX-PARENT-REV TO BO-PARENT-REV
     MOVE BC-ITEM(Ls-I) TO BO-ITEM
     MOVE BC-REV(Ls-I) TO BO-ITEM-REV
     MOVE BC-DESC(Ls-I) TO BO-DESCRIPTION
     MOVE Ls-Node-Type TO BO-NODE-TYPE
     MOVE BC-SUPPLY(Ls-I) TO BO-SUPPLY
     MOVE Ls-Stop TO BO-STOP-REASON
     MOVE BC-USAGE-BASIS(Ls-I) TO BO-USAGE-BASIS
     MOVE BC-UOM(Ls-I) TO BO-UOM
     MOVE BC-STOCK-UOM(Ls-I) TO BO-STOCK-UOM
     MOVE BC-ALT-GROUP(Ls-I) TO BO-ALT-GROUP
     MOVE BC-OPTIONAL(Ls-I) TO BO-OPTIONAL
     MOVE BC-SELECTED(Ls-I) TO BO-SELECTED
     MOVE BC-USAGE-QTY(Ls-I) TO BO-USAGE-QTY
     MOVE BC-CONV-FACT(Ls-I) TO BO-CONV-FACT
     MOVE BC-SCRAP-PCT(Ls-I) TO BO-SCRAP-PCT
     MOVE BC-QUANTUM(Ls-I) TO BO-QUANTUM
     MOVE BC-PACK-QTY(Ls-I) TO BO-PACK-MULT
     MOVE BC-COST-RATE(Ls-I) TO BO-UNIT-COST
     MOVE BC-COST-ELEMENT(Ls-I) TO BO-COST-ELEMENT
     MOVE BC-OPERATION(Ls-I) TO BO-OPERATION
     MOVE Ls-Warning TO BO-WARNING.

 Add-Occurrence-To-Pool.
     MOVE "ADD" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     IF BW-STATUS NOT = "OK"
         PERFORM Report-Capacity
         EXIT PARAGRAPH
     END-IF
     MOVE BW-INDEX TO Ls-Child-Seq.

 Remember-Child-Demand.
     IF Ls-Dem-Cnt >= 500
         PERFORM Report-Capacity
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ls-Dem-Cnt
     MOVE BC-LINE-NUM(Ls-I) TO Ld-Line(Ls-Dem-Cnt)
     MOVE Ls-Child-Seq TO Ld-Seq(Ls-Dem-Cnt)
     MOVE BC-ITEM(Ls-I) TO Ld-Item(Ls-Dem-Cnt)
     MOVE BC-REV(Ls-I) TO Ld-Rev(Ls-Dem-Cnt)
     IF Ws-Net-Mode = "Y"
         MOVE Ls-To-Make TO Ld-Qty(Ls-Dem-Cnt)
     ELSE
         MOVE Ls-Required TO Ld-Qty(Ls-Dem-Cnt)
     END-IF
     IF Ls-Node-Type = "PHANTOM"
         MOVE "Y" TO Ld-Phantom(Ls-Dem-Cnt)
     ELSE
         MOVE "N" TO Ld-Phantom(Ls-Dem-Cnt)
     END-IF
     MOVE "Y" TO Ld-Expand(Ls-Dem-Cnt).

*>----------------------------------------------------------------
*> An APPROVED execution consolidation merges sibling demands for
*> the same made child into one run before any of them is
*> expanded, which changes setup material, batch count, rounding
*> and need dates.  Aggregating a report is never permission to do
*> this - it happens only when the bill or the request says so
*> (D-10, BOM-019).
 Consolidate-Sibling-Runs.
     PERFORM VARYING Ls-I FROM 1 BY 1 UNTIL Ls-I > Ls-Dem-Cnt
         IF Ld-Expand(Ls-I) = "Y" AND Ld-Phantom(Ls-I) = "N"
             PERFORM VARYING Ls-J FROM Ls-I BY 1
                 UNTIL Ls-J >= Ls-Dem-Cnt
                 COMPUTE Ls-Chosen-Line = Ls-J + 1
                 IF Ld-Expand(Ls-Chosen-Line) = "Y"
                    AND Ld-Phantom(Ls-Chosen-Line) = "N"
                    AND Ld-Item(Ls-Chosen-Line) = Ld-Item(Ls-I)
                    AND Ld-Rev(Ls-Chosen-Line) = Ld-Rev(Ls-I)
                     ADD Ld-Qty(Ls-Chosen-Line) TO Ld-Qty(Ls-I)
                     MOVE "N" TO Ld-Expand(Ls-Chosen-Line)
                     PERFORM Mark-Consolidated-Occurrence
                 END-IF
             END-PERFORM
         END-IF
     END-PERFORM.

 Mark-Consolidated-Occurrence.
     MOVE Ld-Seq(Ls-Chosen-Line) TO BW-INDEX
     MOVE "GET" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     IF BW-STATUS NOT = "OK"
         EXIT PARAGRAPH
     END-IF
     MOVE "CONSOLIDATED" TO BO-STOP-REASON
     MOVE "Execution consolidated into an earlier run"
         TO BO-WARNING
     MOVE Ld-Seq(Ls-Chosen-Line) TO BW-INDEX
     MOVE "PUT" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW.

*>================================================================
*> Expansion.  Each demand that is still marked for expansion gets
*> its own selected revision, its own cycle test and its own
*> recursive call.  A shared descendant on two separate acyclic
*> paths is expanded twice on purpose (B6).
 Expand-Child-Demands.
     PERFORM VARYING Ls-I FROM 1 BY 1 UNTIL Ls-I > Ls-Dem-Cnt
         IF Ld-Expand(Ls-I) = "Y" AND Ws-Abort = "N"
             PERFORM Expand-One-Demand
         END-IF
     END-PERFORM.

 Expand-One-Demand.
     PERFORM Test-For-Cycle
     IF Ws-Abort = "Y" OR Ls-Cycle = "Y"
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ls-J = Ls-Depth + 1
     IF Ls-J >= Ws-Max-Depth
         MOVE "DEPTHLIMIT" TO Ls-Stop
         MOVE "BOM0520" TO Ws-Reason
         MOVE SPACES TO Ws-Message
         STRING "The calculation stopped at the stated depth "
                "limit and is not a complete leaf requirement"
             DELIMITED BY SIZE INTO Ws-Message
         MOVE "INCOMPLETE" TO Ws-Completeness
         ADD 1 TO Ws-Warning-Cnt
         MOVE "Depth limit reached" TO Ls-Warning
         PERFORM Stamp-Stop-Reason
         EXIT PARAGRAPH
     END-IF
     PERFORM Select-Child-Bill
     IF Ls-Bill-Ok = "N"
         EXIT PARAGRAPH
     END-IF
     PERFORM Recurse-Into-Child.

*> A cycle is an ancestry path that returns to the same selected
*> item.  Two independent paths reaching the same subassembly are
*> not a cycle, which is why only the ancestry is searched.
 Test-For-Cycle.
     MOVE "N" TO Ls-Cycle
     COMPUTE Ls-J = Ls-Depth + 1
     PERFORM VARYING Ls-K FROM 1 BY 1
         UNTIL Ls-K > Ls-J OR Ls-Cycle = "Y"
         IF Wa-Item(Ls-K) = Ld-Item(Ls-I)
             MOVE "Y" TO Ls-Cycle
             MOVE "BOM0513" TO Ws-Reason
             MOVE SPACES TO Ws-Cycle-Path
             STRING FUNCTION TRIM(Ls-Path) "/"
                    FUNCTION TRIM(Ld-Item(Ls-I))
                    " returns to "
                    FUNCTION TRIM(Wa-Item(Ls-K))
                 DELIMITED BY SIZE INTO Ws-Cycle-Path
             MOVE SPACES TO Ws-Message
             STRING "Cycle rejected: "
                    FUNCTION TRIM(Ws-Cycle-Path)
                 DELIMITED BY SIZE INTO Ws-Message
             MOVE Ws-Cycle-Path TO Ws-Error-Path
             MOVE "ERROR" TO Ws-Completeness
             MOVE "CYCLE" TO Ls-Stop
             MOVE "Cyclic structure" TO Ls-Warning
             PERFORM Stamp-Stop-Reason
             MOVE "Y" TO Ws-Abort
         END-IF
     END-PERFORM.

 Stamp-Stop-Reason.
     MOVE Ld-Seq(Ls-I) TO BW-INDEX
     MOVE "GET" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     IF BW-STATUS NOT = "OK"
         EXIT PARAGRAPH
     END-IF
     MOVE Ls-Stop TO BO-STOP-REASON
     MOVE Ls-Warning TO BO-WARNING
     MOVE Ld-Seq(Ls-I) TO BW-INDEX
     MOVE "PUT" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW.

 Select-Child-Bill.
     MOVE "N" TO Ls-Bill-Ok
     INITIALIZE Ws-Sel-Ca
     MOVE "SELECT" TO SA-FUNCTION
     MOVE Ws-Company TO SA-COMPANY
     MOVE Ws-Site TO SA-SITE
     MOVE Ws-User TO SA-USER
     MOVE Ws-Eff-Date TO SA-BUSINESS-DATE
     MOVE Ld-Item(Ls-I) TO SS-PARENT-ITEM
     MOVE Ld-Rev(Ls-I) TO SS-PARENT-REV
     MOVE "STANDARD" TO SS-PURPOSE
     MOVE Ws-Variant TO SS-VARIANT
     MOVE Ws-Eff-Date TO SS-EFF-DATE
     CALL "BOMSELCT" USING Ws-Sel-Ca
     IF SA-RETURN-CODE <= 4 AND SS-BOM-ID NOT = 0
         MOVE "Y" TO Ls-Bill-Ok
         EXIT PARAGRAPH
     END-IF
     MOVE "BOM0512" TO Ws-Reason
     MOVE SPACES TO Ws-Message
     STRING "No usable bill for the manufactured component "
            FUNCTION TRIM(Ld-Item(Ls-I)) " - "
            FUNCTION TRIM(SA-MESSAGE)
         DELIMITED BY SIZE INTO Ws-Message
     MOVE SPACES TO Ws-Error-Path
     STRING FUNCTION TRIM(Ls-Path) "/"
            FUNCTION TRIM(Ld-Item(Ls-I))
         DELIMITED BY SIZE INTO Ws-Error-Path
     MOVE "INCOMPLETE" TO Ws-Completeness
     ADD 1 TO Ws-Warning-Cnt
     MOVE "NOCHILDBOM" TO Ls-Stop
     MOVE "Missing manufactured child bill" TO Ls-Warning
     PERFORM Stamp-Stop-Reason.

 Recurse-Into-Child.
     INITIALIZE Ls-Child-Ca
     MOVE "NODE" TO CC-FUNCTION
     MOVE Ws-Company TO CC-COMPANY
     MOVE Ws-Site TO CC-SITE
     MOVE Ws-User TO CC-USER
     MOVE Ws-Eff-Date TO CC-BUSINESS-DATE
     MOVE Ld-Item(Ls-I) TO CX-PARENT-ITEM
     MOVE Ld-Rev(Ls-I) TO CX-PARENT-REV
     MOVE SS-BOM-ID TO CX-BOM-ID
     MOVE SS-BOM-REV TO CX-BOM-REV
     MOVE Ld-Qty(Ls-I) TO CX-REQ-QTY
     MOVE Ld-Seq(Ls-I) TO CX-NODE-SEQ
     COMPUTE CX-NODE-DEPTH = Ls-Depth + 1
     MOVE Ld-Phantom(Ls-I) TO CX-NODE-PHANTOM
     MOVE 0 TO CX-SPLIT-CNT
     MOVE Ld-Seq(Ls-I) TO BW-INDEX
     MOVE "GET" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     MOVE BO-PATH TO CX-NODE-PATH
     MOVE SS-BOM-REV TO BO-BOM-REV
     MOVE SS-BOM-ID TO BO-BOM-ID
     MOVE Ld-Seq(Ls-I) TO BW-INDEX
     MOVE "PUT" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     CALL "BOMEXPL" USING Ls-Child-Ca.

*>================================================================
*> By-products are expected recoveries: separate positive outputs
*> with their own valuation and disposition, never a negative
*> material consumption (BOM-026).
 Add-By-Products.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT p.LineNum, p.ByProductItemCode, p.RecoveryQty, "
            "p.UomCode, p.ConversionFactor, p.ValuationBasis, "
            "ISNULL(p.RecoveryValue,0), "
            "ISNULL(p.DispositionRule,''), ISNULL(p.ApprovedBy,'') "
            "FROM bom.BomByProduct p WHERE p.BomHeaderId = ? "
            "ORDER BY p.LineNum"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 1 TO SQLIO-PARM-CNT
     MOVE BX-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE 9 TO SQLIO-COL-CNT
     MOVE 8 TO SQLIO-COL-WIDTH(1)
     MOVE 40 TO SQLIO-COL-WIDTH(2)
     MOVE 24 TO SQLIO-COL-WIDTH(3)
     MOVE 10 TO SQLIO-COL-WIDTH(4)
     MOVE 24 TO SQLIO-COL-WIDTH(5)
     MOVE 12 TO SQLIO-COL-WIDTH(6)
     MOVE 24 TO SQLIO-COL-WIDTH(7)
     MOVE 60 TO SQLIO-COL-WIDTH(8)
     MOVE 20 TO SQLIO-COL-WIDTH(9)
     MOVE 6 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Byp-Row
     IF SQLIO-ERROR
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ls-More
     PERFORM UNTIL Ls-More = "N"
         MOVE 6 TO SQLIO-CURSOR
         MOVE "FTCH" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA Ws-Byp-Row
         IF SQLIO-OK
             PERFORM Record-One-By-Product
         ELSE
             MOVE "N" TO Ls-More
         END-IF
     END-PERFORM
     MOVE 6 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Byp-Row.

 Record-One-By-Product.
     INITIALIZE BOM-OCC-ROW
     MOVE Ls-My-Seq TO BO-PARENT-SEQ
     COMPUTE BO-DEPTH = Ls-Depth + 1
     COMPUTE BO-LINE-NUM = FUNCTION NUMVAL(By-Line-Num)
     MOVE BO-LINE-NUM TO Ls-Line-Edit
     MOVE SPACES TO Ls-Child-Path
     STRING FUNCTION TRIM(Ls-Path) "/BP"
            FUNCTION TRIM(Ls-Line-Edit) ":"
            FUNCTION TRIM(By-Item)
         DELIMITED BY SIZE INTO Ls-Child-Path
     MOVE Ls-Child-Path TO BO-PATH
     MOVE Ls-Child-Path TO BO-OCC-REF
     MOVE BX-PARENT-ITEM TO BO-PARENT-ITEM
     MOVE By-Item TO BO-ITEM
     MOVE By-Disposition TO BO-DESCRIPTION
     MOVE "BYPROD" TO BO-NODE-TYPE
     MOVE "BYPRODUCT" TO BO-SUPPLY
     MOVE "RECOVERY" TO BO-STOP-REASON
     MOVE "PROPORTION" TO BO-USAGE-BASIS
     MOVE By-Uom TO BO-UOM
     MOVE By-Uom TO BO-STOCK-UOM
     COMPUTE BO-USAGE-QTY = FUNCTION NUMVAL(By-Qty)
     COMPUTE BO-CONV-FACT = FUNCTION NUMVAL(By-Conv)
     COMPUTE BO-UNROUNDED =
         Ls-Gross / Ls-Basis * BO-USAGE-QTY * BO-CONV-FACT
     MOVE BO-UNROUNDED TO BO-REQ-QTY
     MOVE BO-UNROUNDED TO BO-GOOD-QTY
     COMPUTE BO-UNIT-COST = FUNCTION NUMVAL(By-Value)
     MOVE By-Basis TO BO-COST-SOURCE
     IF By-Approved = SPACES
         MOVE "Recovery valuation is not approved" TO BO-WARNING
         ADD 1 TO Ws-Warning-Cnt
     END-IF
     MOVE "ADD" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW.

*>================================================================
*> Reading the retained result back.  A read-only repeat has no
*> live effect, which is what BOM-030 asks of a repeated enquiry.
 Page-The-Result.
     MOVE "BOMEXPL" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE SPACES TO CA-REASON-CODE
     IF BX-WIN-MAX < 1 OR BX-WIN-MAX > 25
         MOVE 25 TO BX-WIN-MAX
     END-IF
     MOVE 0 TO BX-WIN-CNT
     MOVE "N" TO BX-MORE-LINES
     MOVE "COUNT" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     MOVE BW-COUNT TO BX-OCC-COUNT
     IF BX-FROM-LINE < 1
         MOVE 1 TO BX-FROM-LINE
     END-IF
     MOVE BX-FROM-LINE TO Ws-View-I
     PERFORM UNTIL Ws-View-I > BX-OCC-COUNT
                   OR BX-WIN-CNT >= BX-WIN-MAX
         MOVE Ws-View-I TO BW-INDEX
         MOVE "GET" TO BW-FUNC
         CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
         PERFORM Test-View-Filter
         IF Ws-Keep = "Y"
             ADD 1 TO BX-WIN-CNT
             MOVE BX-WIN-CNT TO Ws-View-W
             PERFORM Fill-Window-Line
         END-IF
         ADD 1 TO Ws-View-I
     END-PERFORM
     MOVE Ws-View-I TO BX-FROM-LINE
     IF Ws-View-I <= BX-OCC-COUNT
         MOVE "Y" TO BX-MORE-LINES
     END-IF.

 Test-View-Filter.
     MOVE "Y" TO Ws-Keep
     EVALUATE BX-VIEW
         WHEN "LEAF"
             IF BO-NODE-TYPE NOT = "LEAF"
                 MOVE "N" TO Ws-Keep
             END-IF
         WHEN "INTERMED"
             IF BO-NODE-TYPE NOT = "INTERMED"
                 MOVE "N" TO Ws-Keep
             END-IF
         WHEN "LEVEL1"
             IF BO-DEPTH NOT = 1
                 MOVE "N" TO Ws-Keep
             END-IF
         WHEN OTHER
             CONTINUE
     END-EVALUATE.

 Fill-Window-Line.
     MOVE BO-SEQ TO BX-L-SEQ(Ws-View-W)
     MOVE BO-DEPTH TO BX-L-DEPTH(Ws-View-W)
     MOVE BO-PATH TO BX-L-PATH(Ws-View-W)
     MOVE BO-ITEM TO BX-L-ITEM(Ws-View-W)
     MOVE BO-ITEM-REV TO BX-L-REV(Ws-View-W)
     MOVE BO-DESCRIPTION TO BX-L-DESC(Ws-View-W)
     MOVE BO-NODE-TYPE TO BX-L-NODE-TYPE(Ws-View-W)
     MOVE BO-SUPPLY TO BX-L-SUPPLY(Ws-View-W)
     MOVE BO-STOP-REASON TO BX-L-STOP(Ws-View-W)
     MOVE BO-USAGE-BASIS TO BX-L-BASIS(Ws-View-W)
     MOVE BO-STOCK-UOM TO BX-L-UOM(Ws-View-W)
     MOVE BO-BOM-REV TO BX-L-BOM-REV(Ws-View-W)
     MOVE BO-USAGE-QTY TO BX-L-USAGE(Ws-View-W)
     MOVE BO-UNROUNDED TO BX-L-UNROUNDED(Ws-View-W)
     MOVE BO-ROUND-INC TO BX-L-ROUND-INC(Ws-View-W)
     MOVE BO-REQ-QTY TO BX-L-REQ-QTY(Ws-View-W)
     MOVE BO-GOOD-QTY TO BX-L-GOOD-QTY(Ws-View-W)
     MOVE BO-GROSS-STARTS TO BX-L-GROSS(Ws-View-W)
     MOVE BO-RUN-COUNT TO BX-L-RUNS(Ws-View-W)
     MOVE BO-BATCH-COUNT TO BX-L-BATCHES(Ws-View-W)
     MOVE BO-STOCK-USED TO BX-L-STOCK-USED(Ws-View-W)
     MOVE BO-SHORTAGE TO BX-L-SHORTAGE(Ws-View-W)
     MOVE BO-PACK-QTY TO BX-L-PACK-QTY(Ws-View-W)
     MOVE BO-EXCESS TO BX-L-EXCESS(Ws-View-W)
     MOVE BO-UNIT-COST TO BX-L-UNIT-COST(Ws-View-W)
     MOVE BO-EXT-COST TO BX-L-EXT-COST(Ws-View-W)
     MOVE BO-BRANCH-COST TO BX-L-BRANCH(Ws-View-W)
     MOVE BO-COST-ELEMENT TO BX-L-ELEMENT(Ws-View-W)
     MOVE BO-WARNING TO BX-L-WARNING(Ws-View-W).

 Total-One-Item.
     MOVE "BOMEXPL" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE FUNCTION UPPER-CASE(BX-KEY-ITEM) TO BX-KEY-ITEM
     MOVE BX-KEY-ITEM TO BW-KEY-ITEM
     MOVE SPACES TO BW-KEY-REV BW-KEY-UOM
     MOVE "ALL" TO BW-FILTER
     MOVE "TOTAL" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     MOVE BW-TOTAL TO BX-KEY-TOTAL
     IF BW-COUNT = 0
         MOVE 16 TO CA-RETURN-CODE
         MOVE "BOM0530" TO CA-REASON-CODE
         MOVE SPACES TO CA-MESSAGE
         STRING "No occurrence of " FUNCTION TRIM(BX-KEY-ITEM)
                " in the retained calculation"
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF.
*>================================================================
*> STORE - BOM-032.  The whole result is written away so that a
*> reviewer can derive every material total later and tell a gross
*> explosion, a net plan and a cost roll-up apart.  The run is
*> keyed by its calculation reference, so re-storing the same
*> reference replaces it rather than breeding duplicates.
 Store-The-Result.
     MOVE "BOMEXPL" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE SPACES TO CA-REASON-CODE
     IF BX-CALC-REF = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "BOM0531" TO CA-REASON-CODE
         MOVE "A calculation reference is required to store a run"
             TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE "BEGN" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Byp-Row
     PERFORM Delete-Previous-Run
     IF CA-RETURN-CODE > 4
         MOVE "ROLB" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA Ws-Byp-Row
         EXIT PARAGRAPH
     END-IF
     PERFORM Insert-Run-Header
     IF CA-RETURN-CODE > 4
         MOVE "ROLB" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA Ws-Byp-Row
         EXIT PARAGRAPH
     END-IF
     PERFORM Read-Run-Identity
     IF CA-RETURN-CODE > 4
         MOVE "ROLB" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA Ws-Byp-Row
         EXIT PARAGRAPH
     END-IF
     PERFORM Insert-Run-Lines
     IF CA-RETURN-CODE > 4
         MOVE "ROLB" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA Ws-Byp-Row
         EXIT PARAGRAPH
     END-IF
     MOVE "COMT" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Byp-Row
     MOVE SPACES TO CA-MESSAGE
     STRING "Calculation " FUNCTION TRIM(BX-CALC-REF) " stored"
         DELIMITED BY SIZE INTO CA-MESSAGE.

 Delete-Previous-Run.
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE l FROM bom.BomExplosionLine l "
            "JOIN bom.BomExplosion x ON x.ExplosionId = "
            "l.ExplosionId WHERE x.CompanyCode = ? "
            "AND x.CalcRef = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Bind-Calc-Key
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 2 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Byp-Row
     IF SQLIO-ERROR
         PERFORM Report-Store-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE 2 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Byp-Row
     MOVE SPACES TO SQLIO-STMT
     STRING "DELETE FROM bom.BomExplosion WHERE CompanyCode = ? "
            "AND CalcRef = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Bind-Calc-Key
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 2 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Byp-Row
     IF SQLIO-ERROR
         PERFORM Report-Store-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE 2 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Byp-Row.

 Bind-Calc-Key.
     MOVE 2 TO SQLIO-PARM-CNT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE BX-CALC-REF TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2).

 Insert-Run-Header.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT INTO bom.BomExplosion (CompanyCode, SiteCode, "
            "CalcRef, CalcType, Purpose, ParentItemCode, "
            "ParentRevision, BomHeaderId, RequiredQty, "
            "EffectiveDate, GrossStarts, RunCount, BatchCount, "
            "MaxDepth, OccurrenceCount, CostSetCode, CurrencyCode, "
            "TotalCost, UnitCost, MaxBuildableQty, Completeness, "
            "WarningCount, ErrorMessage, CyclePath, RequestedBy, "
            "CorrelationId, NominalBasis, YieldFactor, "
            "ProductionQuantum, BatchSize, ExcessQty, "
            "ConsolidateRuns, ReasonCode, MaterialCost, "
            "ConversionCost, RecoveryCredit, UnitDenominator, "
            "ShortageCount) VALUES (?,?,?,?,?,?,?,?,?,"
            "CONVERT(date,?,112),?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,"
            "?,?,?,?,?,?,?,?,?,?,?,?)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 38 TO SQLIO-PARM-CNT
     PERFORM Bind-Header-Values
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 2 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Byp-Row
     IF SQLIO-ERROR
         PERFORM Report-Store-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE 2 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Byp-Row.

 Bind-Header-Values.
     PERFORM VARYING Ws-View-W FROM 1 BY 1 UNTIL Ws-View-W > 38
         MOVE SPACES TO SQLIO-PARM-VAL(Ws-View-W)
         MOVE "N" TO SQLIO-PARM-NULL(Ws-View-W)
     END-PERFORM
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE CA-SITE TO SQLIO-PARM-VAL(2)
     MOVE BX-CALC-REF TO SQLIO-PARM-VAL(3)
     MOVE BX-CALC-TYPE TO SQLIO-PARM-VAL(4)
     MOVE BX-PURPOSE TO SQLIO-PARM-VAL(5)
     MOVE BX-PARENT-ITEM TO SQLIO-PARM-VAL(6)
     MOVE BX-PARENT-REV TO SQLIO-PARM-VAL(7)
     MOVE BX-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(8)
     MOVE BX-REQ-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
     MOVE BX-EFF-DATE TO SQLIO-PARM-VAL(10)
     MOVE BX-GROSS-STARTS TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
     MOVE BX-RUN-COUNT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(12)
     MOVE BX-BATCH-COUNT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(13)
     MOVE BX-DEPTH-REACHED TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(14)
     MOVE BX-OCC-COUNT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(15)
     MOVE BX-COST-SET TO SQLIO-PARM-VAL(16)
     IF BX-COST-SET = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(16)
     END-IF
     MOVE BX-CURRENCY TO SQLIO-PARM-VAL(17)
     IF BX-CURRENCY = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(17)
     END-IF
     MOVE BX-TOTAL-COST TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(18)
     MOVE BX-UNIT-COST TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(19)
     MOVE BX-MAX-BUILDABLE TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(20)
     MOVE BX-COMPLETENESS TO SQLIO-PARM-VAL(21)
     MOVE BX-WARNING-CNT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(22)
     MOVE Ws-Message TO SQLIO-PARM-VAL(23)
     MOVE BX-CYCLE-PATH TO SQLIO-PARM-VAL(24)
     MOVE CA-USER TO SQLIO-PARM-VAL(25)
     MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(26)
     MOVE BX-NOMINAL-BASIS TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(27)
     MOVE BX-YIELD TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(28)
     MOVE BX-QUANTUM TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(29)
     MOVE BX-BATCH-SIZE TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(30)
     MOVE BX-EXCESS TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(31)
     IF Ws-Consolidate = "Y"
         MOVE "1" TO SQLIO-PARM-VAL(32)
     ELSE
         MOVE "0" TO SQLIO-PARM-VAL(32)
     END-IF
     MOVE Ws-Reason TO SQLIO-PARM-VAL(33)
     MOVE BX-MATERIAL-COST TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(34)
     MOVE BX-CONVERSION-COST TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(35)
     MOVE BX-RECOVERY-CREDIT TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(36)
     MOVE BX-UNIT-DENOM TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(37)
     MOVE BX-SHORTAGE-CNT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(38).

 Insert-Run-Lines.
     MOVE "COUNT" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     MOVE BW-COUNT TO Ws-Occ-Total
     MOVE 0 TO Ws-Store-Line
     PERFORM VARYING Ws-View-I FROM 1 BY 1
         UNTIL Ws-View-I > Ws-Occ-Total OR CA-RETURN-CODE > 4
         MOVE Ws-View-I TO BW-INDEX
         MOVE "GET" TO BW-FUNC
         CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
         ADD 1 TO Ws-Store-Line
         PERFORM Insert-One-Line
     END-PERFORM.

 Insert-One-Line.
     MOVE SPACES TO SQLIO-STMT
     MOVE BX-EXPLOSION-ID TO SQLIO-INT-EDIT
     STRING "INSERT INTO bom.BomExplosionLine (ExplosionId, "
            "LineNum, Depth, PathText, ParentItemCode, "
            "ParentLineNum, ComponentItemCode, ComponentRevision, "
            "BomHeaderId, UsageBasis, UomCode, ConversionFactor, "
            "UnroundedQty, YieldContribution, ScrapContribution, "
            "RoundingIncrement, RequiredQty, NodeType, StopReason, "
            "StockUsedQty, ShortageQty, UnitCost, ExtendedCost, "
            "CostElementCode, IsFixedCost, WarningText, GoodQty, "
            "GrossStarts, RunCount, BatchCount, BomRevision, "
            "NominalBasis, YieldFactor, UsageQty, ScrapPct, "
            "StockUomCode, PackQty, ExcessQty, OccurrenceRef, "
            "AlternateGroup, SupplyBehaviour) VALUES ("
            FUNCTION TRIM(SQLIO-INT-EDIT) ","
            "?,?,?,?,?,?,?,?,?,?,"
            "?,?,?,?,?,?,?,?,?,?,"
            "?,?,?,?,?,?,?,?,?,?,"
            "?,?,?,?,?,?,?,?,?,?)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 40 TO SQLIO-PARM-CNT
     PERFORM Bind-Line-Values
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 2 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Byp-Row
     IF SQLIO-ERROR
         PERFORM Report-Store-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE 2 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Byp-Row.

*> The identity of the run just written.  It is the only value put
*> into the statement text rather than bound, because it is a key
*> this program generated itself a moment ago - no user text ever
*> reaches a statement (CONVENTIONS 3.4).
 Read-Run-Identity.
     MOVE 0 TO BX-EXPLOSION-ID
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT x.ExplosionId FROM bom.BomExplosion x "
            "WHERE x.CompanyCode = ? AND x.CalcRef = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     PERFORM Bind-Calc-Key
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 12 TO SQLIO-COL-WIDTH(1)
     MOVE 2 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     IF SQLIO-ERROR OR SQLIO-NOTFOUND
         PERFORM Report-Store-Failure
         EXIT PARAGRAPH
     END-IF
     COMPUTE BX-EXPLOSION-ID = FUNCTION NUMVAL(Xi-Id).

 Bind-Line-Values.
     PERFORM VARYING Ws-View-W FROM 1 BY 1 UNTIL Ws-View-W > 40
         MOVE SPACES TO SQLIO-PARM-VAL(Ws-View-W)
         MOVE "N" TO SQLIO-PARM-NULL(Ws-View-W)
     END-PERFORM
     MOVE Ws-Store-Line TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
     MOVE BO-DEPTH TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
     MOVE BO-PATH TO SQLIO-PARM-VAL(3)
     IF BO-PARENT-ITEM = SPACES
         MOVE BO-ITEM TO SQLIO-PARM-VAL(4)
     ELSE
         MOVE BO-PARENT-ITEM TO SQLIO-PARM-VAL(4)
     END-IF
     MOVE BO-LINE-NUM TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
     MOVE BO-ITEM TO SQLIO-PARM-VAL(6)
     MOVE BO-ITEM-REV TO SQLIO-PARM-VAL(7)
     MOVE BO-BOM-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(8)
     IF BO-BOM-ID = 0
         MOVE "Y" TO SQLIO-PARM-NULL(8)
     END-IF
     MOVE BO-USAGE-BASIS TO SQLIO-PARM-VAL(9)
     MOVE BO-UOM TO SQLIO-PARM-VAL(10)
     MOVE BO-CONV-FACT TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
     MOVE BO-UNROUNDED TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
     MOVE BO-YIELD-CONTRIB TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
     MOVE BO-SCRAP-CONTRIB TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(14)
     MOVE BO-ROUND-INC TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(15)
     MOVE BO-REQ-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(16)
     IF BO-NODE-TYPE = "BYPROD"
         MOVE "BYPRODUCT" TO SQLIO-PARM-VAL(17)
     ELSE
         MOVE BO-NODE-TYPE TO SQLIO-PARM-VAL(17)
     END-IF
     MOVE BO-STOP-REASON TO SQLIO-PARM-VAL(18)
     MOVE BO-STOCK-USED TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(19)
     MOVE BO-SHORTAGE TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(20)
     MOVE BO-UNIT-COST TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(21)
     MOVE BO-EXT-COST TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(22)
     MOVE BO-COST-ELEMENT TO SQLIO-PARM-VAL(23)
     IF BO-COST-ELEMENT = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(23)
     END-IF
     MOVE BO-FIXED-COST TO SQLIO-PARM-VAL(24)
     IF BO-FIXED-COST NOT = "Y"
         MOVE "0" TO SQLIO-PARM-VAL(24)
     ELSE
         MOVE "1" TO SQLIO-PARM-VAL(24)
     END-IF
     MOVE BO-WARNING TO SQLIO-PARM-VAL(25)
     MOVE BO-GOOD-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(26)
     MOVE BO-GROSS-STARTS TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(27)
     MOVE BO-RUN-COUNT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(28)
     MOVE BO-BATCH-COUNT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(29)
     MOVE BO-BOM-REV TO SQLIO-PARM-VAL(30)
     MOVE BO-NOMINAL-BASIS TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(31)
     MOVE BO-YIELD TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(32)
     MOVE BO-USAGE-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(33)
     MOVE BO-SCRAP-PCT TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(34)
     MOVE BO-STOCK-UOM TO SQLIO-PARM-VAL(35)
     MOVE BO-PACK-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(36)
     MOVE BO-EXCESS TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(37)
     MOVE BO-OCC-REF TO SQLIO-PARM-VAL(38)
     MOVE BO-ALT-GROUP TO SQLIO-PARM-VAL(39)
     IF BO-ALT-GROUP = SPACES
         MOVE "Y" TO SQLIO-PARM-NULL(39)
     END-IF
     MOVE BO-SUPPLY TO SQLIO-PARM-VAL(40)
     MOVE 40 TO SQLIO-PARM-CNT.

 Report-Store-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE SQLIO-ERRMSG TO CA-MESSAGE.

