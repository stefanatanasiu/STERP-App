>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : BOMWORK
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : The work pool of one bill of materials calculation.
*>              BOMEXPL fills it occurrence by occurrence; BOMLEAF,
*>              BOMNET, BOMCOST, BOMCOMP, the batch reports and the
*>              client then read it in whatever shape they need.
*>              Keeping one pool is what makes the single level,
*>              indented and flattened views reconcile for the same
*>              inputs - they are three readings of one result,
*>              not three calculations.
*>
*>              The pool is a fixed table of fixed length slots.
*>              When it fills, the status is FULL and the caller
*>              must report an incomplete calculation: silent
*>              truncation is prohibited at every depth (BOM-029).
*> CALLED BY  : BOMEXPL, BOMLEAF, BOMNET, BOMCOST, BOMCOMP, the
*>              batch reports and the unit tests
*> INTERFACE  : CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
*>              COPYBOOKS BOMWKCTL, BOMOCCUR  (not LINKable)
*> CALLS      : nothing
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2014-03-17 AJS  CR2201 ENGINEERING REWRITE
*> 2014-11-11 AJS  CR2259 AGGREGATION KEY EXTENDED - IDENTITY,
*>                 REVISION, UNIT AND SUPPLY CONTEXT.  TWO ITEMS
*>                 WITH THE SAME DESCRIPTION ARE STILL TWO ITEMS.
*> 2016-11-08 AJS  CR2502 COSTSUM BRANCH FILTER (BOM-021)
*> 2026-10-05 STERP CAPACITY REPORTED, NEVER TRUNCATED
*>================================================================
*> REQ: BOM-014, BOM-015, BOM-019, BOM-021, BOM-029
 IDENTIFICATION DIVISION.
 PROGRAM-ID. BOMWORK.
 AUTHOR. A J SPENCER.
 DATE-WRITTEN. 17/03/2014.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
*> Hard limits, deliberately generous for the demonstration data
*> and deliberately visible - see docs\tech-debt\bom.md.  The slot
*> is wider than the occurrence record so the record may grow
*> without re-cutting the pool.
 01  Ws-Limits.
     05  Ws-Occ-Max              PIC S9(9) COMP-5 VALUE 2000.
     05  Ws-Leaf-Max             PIC S9(9) COMP-5 VALUE 500.
 01  Ws-Counters.
     05  Ws-Occ-Cnt              PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Leaf-Cnt             PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Deepest              PIC S9(4) VALUE 0.
     05  Ws-I                    PIC S9(9) COMP-5 VALUE 0.
     05  Ws-J                    PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Hit                  PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Walk                 PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Guard                PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Found                PIC X VALUE "N".
     05  Ws-In-Branch            PIC X VALUE "N".
     05  Ws-Accum                PIC S9(15)V9(6) VALUE 0.
     05  Ws-Money                PIC S9(13)V9(2) VALUE 0.
 01  Ws-Occ-Pool.
     05  Ws-Occ-Slot             PIC X(1200) OCCURS 2000 TIMES.
 01  Ws-Leaf-Pool.
     05  Ws-Leaf-Slot            PIC X(1200) OCCURS 500 TIMES.
 COPY BOMOCCUR REPLACING ==BOM-OCC-ROW== BY ==Ws-Row-A==.
 COPY BOMOCCUR REPLACING ==BOM-OCC-ROW== BY ==Ws-Row-B==.
 LINKAGE SECTION.
 COPY BOMWKCTL.
 COPY BOMOCCUR.
 PROCEDURE DIVISION USING BOM-WK-CTL BOM-OCC-ROW.
 Main-Entry.
     MOVE "OK" TO BW-STATUS
     EVALUATE BW-FUNC
         WHEN "CLEAR"    PERFORM Clear-Pools
         WHEN "ADD"      PERFORM Add-Occurrence
         WHEN "GET"      PERFORM Get-Occurrence
         WHEN "PUT"      PERFORM Put-Occurrence
         WHEN "COUNT"    PERFORM Report-Count
         WHEN "AGGREG"   PERFORM Aggregate-Leaves
         WHEN "LEAFCNT"  MOVE Ws-Leaf-Cnt TO BW-COUNT
         WHEN "GETLEAF"  PERFORM Get-Leaf
         WHEN "PUTLEAF"  PERFORM Put-Leaf
         WHEN "FINDLEAF" PERFORM Find-Leaf
         WHEN "TOTAL"    PERFORM Total-For-Item
         WHEN "COSTSUM"  PERFORM Sum-Cost
         WHEN OTHER      MOVE "BADFUNC" TO BW-STATUS
     END-EVALUATE
     GOBACK.

 Clear-Pools.
     MOVE 0 TO Ws-Occ-Cnt Ws-Leaf-Cnt Ws-Deepest
     MOVE 0 TO BW-COUNT BW-INDEX
     MOVE 0 TO BW-TOTAL BW-MONEY
     MOVE 0 TO BW-MAX-DEPTH
     MOVE Ws-Occ-Max TO BW-CAPACITY.

 Report-Count.
     MOVE Ws-Occ-Cnt TO BW-COUNT
     MOVE Ws-Occ-Max TO BW-CAPACITY
     MOVE Ws-Deepest TO BW-MAX-DEPTH.

*> ADD appends and hands back the sequence number it was given, so
*> a caller can record the parent of the next occurrence.
 Add-Occurrence.
     IF Ws-Occ-Cnt >= Ws-Occ-Max
         MOVE "FULL" TO BW-STATUS
         MOVE Ws-Occ-Cnt TO BW-COUNT
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Occ-Cnt
     MOVE Ws-Occ-Cnt TO BO-SEQ OF BOM-OCC-ROW
     MOVE BOM-OCC-ROW TO Ws-Occ-Slot(Ws-Occ-Cnt)
     MOVE Ws-Occ-Cnt TO BW-INDEX
     MOVE Ws-Occ-Cnt TO BW-COUNT
     IF BO-DEPTH OF BOM-OCC-ROW > Ws-Deepest
         MOVE BO-DEPTH OF BOM-OCC-ROW TO Ws-Deepest
     END-IF
     MOVE Ws-Deepest TO BW-MAX-DEPTH.

 Get-Occurrence.
     IF BW-INDEX < 1 OR BW-INDEX > Ws-Occ-Cnt
         MOVE "NOTFOUND" TO BW-STATUS
         INITIALIZE BOM-OCC-ROW
         EXIT PARAGRAPH
     END-IF
     MOVE Ws-Occ-Slot(BW-INDEX) TO BOM-OCC-ROW
     MOVE Ws-Occ-Cnt TO BW-COUNT.

 Put-Occurrence.
     IF BW-INDEX < 1 OR BW-INDEX > Ws-Occ-Cnt
         MOVE "NOTFOUND" TO BW-STATUS
         EXIT PARAGRAPH
     END-IF
     MOVE BW-INDEX TO BO-SEQ OF BOM-OCC-ROW
     MOVE BOM-OCC-ROW TO Ws-Occ-Slot(BW-INDEX).

*>----------------------------------------------------------------
*> AGGREG - flatten the leaf occurrences.  CR2259: compatible means
*> the same item identity, the same revision, the same stock unit
*> and the same supply behaviour.  Different revisions or
*> incompatible supply contexts stay apart even when the
*> descriptions match (BOM-015).  BO-SEQ of an aggregated line
*> holds the number of contributing occurrences and BO-PATH keeps
*> the first contributing path, so the total stays traceable.
 Aggregate-Leaves.
     MOVE 0 TO Ws-Leaf-Cnt
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > Ws-Occ-Cnt
         MOVE Ws-Occ-Slot(Ws-I) TO Ws-Row-A
         IF BO-NODE-TYPE OF Ws-Row-A = "LEAF"
             PERFORM Merge-One-Leaf
         END-IF
     END-PERFORM
     MOVE Ws-Leaf-Cnt TO BW-COUNT.

 Merge-One-Leaf.
     MOVE "N" TO Ws-Found
     MOVE 0 TO Ws-Hit
     PERFORM VARYING Ws-J FROM 1 BY 1
         UNTIL Ws-J > Ws-Leaf-Cnt OR Ws-Found = "Y"
         MOVE Ws-Leaf-Slot(Ws-J) TO Ws-Row-B
         IF BO-ITEM OF Ws-Row-B = BO-ITEM OF Ws-Row-A
            AND BO-ITEM-REV OF Ws-Row-B = BO-ITEM-REV OF Ws-Row-A
            AND BO-STOCK-UOM OF Ws-Row-B = BO-STOCK-UOM OF Ws-Row-A
            AND BO-SUPPLY OF Ws-Row-B = BO-SUPPLY OF Ws-Row-A
             MOVE "Y" TO Ws-Found
             MOVE Ws-J TO Ws-Hit
         END-IF
     END-PERFORM
     IF Ws-Found = "Y"
         PERFORM Accumulate-Into-Leaf
         EXIT PARAGRAPH
     END-IF
     IF Ws-Leaf-Cnt >= Ws-Leaf-Max
         MOVE "FULL" TO BW-STATUS
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Leaf-Cnt
     MOVE 1 TO BO-SEQ OF Ws-Row-A
     MOVE 0 TO BO-PARENT-SEQ OF Ws-Row-A
     MOVE Ws-Row-A TO Ws-Leaf-Slot(Ws-Leaf-Cnt).

 Accumulate-Into-Leaf.
     MOVE Ws-Leaf-Slot(Ws-Hit) TO Ws-Row-B
     ADD 1 TO BO-SEQ OF Ws-Row-B
     ADD BO-REQ-QTY OF Ws-Row-A TO BO-REQ-QTY OF Ws-Row-B
     ADD BO-UNROUNDED OF Ws-Row-A TO BO-UNROUNDED OF Ws-Row-B
     ADD BO-ROUND-INC OF Ws-Row-A TO BO-ROUND-INC OF Ws-Row-B
     ADD BO-SCRAP-CONTRIB OF Ws-Row-A
         TO BO-SCRAP-CONTRIB OF Ws-Row-B
     ADD BO-YIELD-CONTRIB OF Ws-Row-A
         TO BO-YIELD-CONTRIB OF Ws-Row-B
     ADD BO-STOCK-USED OF Ws-Row-A TO BO-STOCK-USED OF Ws-Row-B
     ADD BO-INBOUND-USED OF Ws-Row-A
         TO BO-INBOUND-USED OF Ws-Row-B
     ADD BO-SHORTAGE OF Ws-Row-A TO BO-SHORTAGE OF Ws-Row-B
     ADD BO-PACK-QTY OF Ws-Row-A TO BO-PACK-QTY OF Ws-Row-B
     ADD BO-EXCESS OF Ws-Row-A TO BO-EXCESS OF Ws-Row-B
     ADD BO-EXT-COST OF Ws-Row-A TO BO-EXT-COST OF Ws-Row-B
     IF BO-DEPTH OF Ws-Row-A < BO-DEPTH OF Ws-Row-B
         MOVE BO-DEPTH OF Ws-Row-A TO BO-DEPTH OF Ws-Row-B
     END-IF
     MOVE Ws-Row-B TO Ws-Leaf-Slot(Ws-Hit).

 Get-Leaf.
     IF BW-INDEX < 1 OR BW-INDEX > Ws-Leaf-Cnt
         MOVE "NOTFOUND" TO BW-STATUS
         INITIALIZE BOM-OCC-ROW
         EXIT PARAGRAPH
     END-IF
     MOVE Ws-Leaf-Slot(BW-INDEX) TO BOM-OCC-ROW
     MOVE Ws-Leaf-Cnt TO BW-COUNT.

 Put-Leaf.
     IF BW-INDEX < 1 OR BW-INDEX > Ws-Leaf-Cnt
         MOVE "NOTFOUND" TO BW-STATUS
         EXIT PARAGRAPH
     END-IF
     MOVE BOM-OCC-ROW TO Ws-Leaf-Slot(BW-INDEX).

 Find-Leaf.
     MOVE "NOTFOUND" TO BW-STATUS
     INITIALIZE BOM-OCC-ROW
     MOVE 0 TO BW-TOTAL
     PERFORM VARYING Ws-J FROM 1 BY 1
         UNTIL Ws-J > Ws-Leaf-Cnt OR BW-STATUS = "OK"
         MOVE Ws-Leaf-Slot(Ws-J) TO Ws-Row-B
         IF BO-ITEM OF Ws-Row-B = BW-KEY-ITEM
            AND (BW-KEY-UOM = SPACES
                 OR BO-STOCK-UOM OF Ws-Row-B = BW-KEY-UOM)
            AND (BW-KEY-REV = SPACES
                 OR BO-ITEM-REV OF Ws-Row-B = BW-KEY-REV)
             MOVE Ws-Row-B TO BOM-OCC-ROW
             MOVE Ws-J TO BW-INDEX
             MOVE BO-REQ-QTY OF Ws-Row-B TO BW-TOTAL
             MOVE "OK" TO BW-STATUS
         END-IF
     END-PERFORM.

*> TOTAL - every occurrence of one item on every valid path, which
*> is what "shared leaf counted on all valid paths" means (B1).
 Total-For-Item.
     MOVE 0 TO Ws-Accum
     MOVE 0 TO BW-COUNT
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > Ws-Occ-Cnt
         MOVE Ws-Occ-Slot(Ws-I) TO Ws-Row-A
         IF BO-ITEM OF Ws-Row-A = BW-KEY-ITEM
            AND (BW-FILTER = SPACES OR BW-FILTER = "ALL"
                 OR BW-FILTER = BO-NODE-TYPE OF Ws-Row-A)
             ADD BO-REQ-QTY OF Ws-Row-A TO Ws-Accum
             ADD 1 TO BW-COUNT
         END-IF
     END-PERFORM
     MOVE Ws-Accum TO BW-TOTAL.

*>----------------------------------------------------------------
*> COSTSUM - one valuation boundary per branch.  ALL totals the
*> whole calculation, LEAF only the expanded leaf values, and
*> BRANCH totals the sub-tree rooted at BW-INDEX.  A branch total
*> is for presentation: it already contains the children, so it is
*> never added on top of them (BOM-021).
 Sum-Cost.
     MOVE 0 TO Ws-Money
     MOVE 0 TO BW-COUNT
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > Ws-Occ-Cnt
         MOVE Ws-Occ-Slot(Ws-I) TO Ws-Row-A
         EVALUATE BW-FILTER
             WHEN "LEAF"
                 IF BO-NODE-TYPE OF Ws-Row-A = "LEAF"
                     PERFORM Take-The-Value
                 END-IF
             WHEN "INTERMED"
                 IF BO-NODE-TYPE OF Ws-Row-A = "INTERMED"
                     PERFORM Take-The-Value
                 END-IF
             WHEN "BRANCH"
                 PERFORM Test-In-Branch
                 IF Ws-In-Branch = "Y"
                     PERFORM Take-The-Value
                 END-IF
             WHEN OTHER
                 PERFORM Take-The-Value
         END-EVALUATE
     END-PERFORM
     MOVE Ws-Money TO BW-MONEY
     MOVE Ws-Money TO BW-TOTAL.

 Take-The-Value.
     ADD BO-EXT-COST OF Ws-Row-A TO Ws-Money
     ADD 1 TO BW-COUNT.

*> Walk up the parent chain of occurrence Ws-I looking for the
*> branch root.  Ws-Guard stops a damaged chain from looping.
 Test-In-Branch.
     MOVE "N" TO Ws-In-Branch
     MOVE Ws-I TO Ws-Walk
     MOVE 0 TO Ws-Guard
     PERFORM UNTIL Ws-Walk < 1 OR Ws-In-Branch = "Y"
                   OR Ws-Guard > Ws-Occ-Max
         IF Ws-Walk = BW-INDEX
             MOVE "Y" TO Ws-In-Branch
         ELSE
             MOVE Ws-Occ-Slot(Ws-Walk) TO Ws-Row-B
             MOVE BO-PARENT-SEQ OF Ws-Row-B TO Ws-Walk
         END-IF
         ADD 1 TO Ws-Guard
     END-PERFORM.
