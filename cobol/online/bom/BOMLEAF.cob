>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : BOMLEAF
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : The three enquiry shapes of one calculation:
*>              single level, indented multi level and flattened
*>              leaf requirement.  They are three readings of the
*>              SAME occurrence pool, which is what makes them
*>              reconcile for the same inputs (BOM-014).
*>
*>              A displayed intermediate subtotal is explanatory:
*>              twenty good braces are an intermediate manufacture
*>              requirement, not an extra purchased leaf, and are
*>              never added to the materials they already contain.
*>              Compatible leaves merge on identity, revision,
*>              stock unit and supply context only - a matching
*>              description is not a reason to merge (BOM-015).
*>
*>              Every view carries the stop reason summary, so a
*>              depth or capacity limit is labelled rather than
*>              presented as a complete leaf list.
*> CALLED BY  : the explosion enquiry form, the indented and
*>              costed explosion reports, the unit tests
*> INTERFACE  : CALL "BOMLEAF" USING WS-BOMLEAF-CA
*>              COPYBOOK BOMLEAFCA
*> CALLS      : BOMEXPL, BOMWORK
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2014-08-19 AJS  CR2233 ENGINEERING REWRITE
*> 2026-10-05 STERP STOP REASON SUMMARY ON EVERY VIEW
*>================================================================
*> REQ: BOM-014, BOM-015, BOM-019, BOM-029, BOM-032
 IDENTIFICATION DIVISION.
 PROGRAM-ID. BOMLEAF.
 AUTHOR. A J SPENCER.
 DATE-WRITTEN. 19/08/2014.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY BOMWKCTL.
 COPY BOMOCCUR.
 COPY BOMEXPLCA REPLACING ==DFHCOMMAREA== BY ==Ws-Expl-Ca==
      LEADING ==CA-== BY ==EA-==
      LEADING ==BX-== BY ==EX-==.
 01  Ws-Work.
     05  Ws-I                    PIC S9(9) COMP-5 VALUE 0.
     05  Ws-W                    PIC S9(4) VALUE 0.
     05  Ws-Keep                 PIC X VALUE "Y".
     05  Ws-Total                PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Leaf-Total           PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Interm               PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Indent               PIC X(50) VALUE SPACES.
     05  Ws-Dot                  PIC S9(4) VALUE 0.
     05  Ws-Stop-Purch           PIC S9(4) VALUE 0.
     05  Ws-Stop-Depth           PIC S9(4) VALUE 0.
     05  Ws-Stop-Nobom           PIC S9(4) VALUE 0.
     05  Ws-Stop-Stock           PIC S9(4) VALUE 0.
     05  Ws-Stop-Other           PIC S9(4) VALUE 0.
     05  Ws-Edit                 PIC Z(8)9.
 LINKAGE SECTION.
 COPY BOMLEAFCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "BOMLEAF" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE "BOM-014" TO CA-REQ-REF
     IF BL-WIN-MAX < 1 OR BL-WIN-MAX > 25
         MOVE 25 TO BL-WIN-MAX
     END-IF
     MOVE 0 TO BL-WIN-CNT
     MOVE "N" TO BL-MORE-LINES
     EVALUATE CA-FUNCTION
         WHEN "SINGLE"   PERFORM Ensure-Calculation
                         PERFORM Page-Single-Level
         WHEN "INDENT"   PERFORM Ensure-Calculation
                         PERFORM Page-Indented
         WHEN "FLATTEN"  PERFORM Ensure-Calculation
                         PERFORM Page-Flattened
         WHEN "ITEMQTY"  PERFORM Ensure-Calculation
                         PERFORM Total-One-Item
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "BOM0601" TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "BOMLEAF does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>----------------------------------------------------------------
*> One explosion serves all three views.  BL-REEXPLODE = "N" pages
*> the result that is already held, so turning the page of a large
*> tree does not re-read the whole structure, and a repeated
*> read-only enquiry has no live effect (BOM-030).
 Ensure-Calculation.
     IF BL-REEXPLODE = "N"
         PERFORM Count-The-Pool
         EXIT PARAGRAPH
     END-IF
     INITIALIZE Ws-Expl-Ca
     MOVE "EXPLODE" TO EA-FUNCTION
     MOVE CA-COMPANY TO EA-COMPANY
     MOVE CA-SITE TO EA-SITE
     MOVE CA-USER TO EA-USER
     MOVE CA-BUSINESS-DATE TO EA-BUSINESS-DATE
     MOVE "GROSS" TO EX-CALC-TYPE
     MOVE BL-CALC-REF TO EX-CALC-REF
     MOVE BL-PARENT-ITEM TO EX-PARENT-ITEM
     MOVE BL-PARENT-REV TO EX-PARENT-REV
     MOVE BL-PURPOSE TO EX-PURPOSE
     MOVE BL-JOB-REF TO EX-JOB-REF
     MOVE BL-VARIANT TO EX-VARIANT
     MOVE BL-REQ-QTY TO EX-REQ-QTY
     MOVE BL-EFF-DATE TO EX-EFF-DATE
     MOVE BL-MAX-DEPTH TO EX-MAX-DEPTH
     MOVE "Y" TO EX-EXPAND-PHANTOM
     MOVE "Y" TO EX-INCLUDE-NOTES
     MOVE "Y" TO EX-INCLUDE-BYPROD
     CALL "BOMEXPL" USING Ws-Expl-Ca
     MOVE EX-COMPLETENESS TO BL-COMPLETENESS
     MOVE EX-DEPTH-REACHED TO BL-DEPTH-REACHED
     MOVE EA-RETURN-CODE TO CA-RETURN-CODE
     MOVE EA-REASON-CODE TO CA-REASON-CODE
     MOVE EA-MESSAGE TO CA-MESSAGE
     PERFORM Count-The-Pool.

 Count-The-Pool.
     MOVE "COUNT" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     MOVE BW-COUNT TO Ws-Total
     MOVE BW-COUNT TO BL-OCC-COUNT
     MOVE BW-MAX-DEPTH TO BL-DEPTH-REACHED
     MOVE "LEAFCNT" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     MOVE BW-COUNT TO Ws-Leaf-Total
     MOVE BW-COUNT TO BL-LEAF-COUNT
     PERFORM Summarise-Stop-Reasons.

*> Why expansion stopped, counted by reason.  BOM-014 insists that
*> a limit is labelled; the caller prints this line under the view.
 Summarise-Stop-Reasons.
     MOVE 0 TO Ws-Stop-Purch Ws-Stop-Depth Ws-Stop-Nobom
     MOVE 0 TO Ws-Stop-Stock Ws-Stop-Other Ws-Interm
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > Ws-Total
         MOVE Ws-I TO BW-INDEX
         MOVE "GET" TO BW-FUNC
         CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
         IF BO-NODE-TYPE = "INTERMED"
             ADD 1 TO Ws-Interm
         END-IF
         EVALUATE BO-STOP-REASON
             WHEN "PURCHASED"  ADD 1 TO Ws-Stop-Purch
             WHEN "DEPTHLIMIT" ADD 1 TO Ws-Stop-Depth
             WHEN "NOCHILDBOM" ADD 1 TO Ws-Stop-Nobom
             WHEN "USESTOCK"   ADD 1 TO Ws-Stop-Stock
             WHEN "FROMSTOCK"  ADD 1 TO Ws-Stop-Stock
             WHEN OTHER        ADD 1 TO Ws-Stop-Other
         END-EVALUATE
     END-PERFORM
     MOVE Ws-Interm TO BL-INTERMED-COUNT
     MOVE SPACES TO BL-STOP-SUMMARY
     MOVE Ws-Stop-Purch TO Ws-Edit
     STRING "Stopped: purchased " FUNCTION TRIM(Ws-Edit)
         DELIMITED BY SIZE INTO BL-STOP-SUMMARY
     MOVE Ws-Stop-Stock TO Ws-Edit
     STRING FUNCTION TRIM(BL-STOP-SUMMARY) ", use stock "
            FUNCTION TRIM(Ws-Edit)
         DELIMITED BY SIZE INTO BL-STOP-SUMMARY
     MOVE Ws-Stop-Depth TO Ws-Edit
     STRING FUNCTION TRIM(BL-STOP-SUMMARY) ", depth limit "
            FUNCTION TRIM(Ws-Edit)
         DELIMITED BY SIZE INTO BL-STOP-SUMMARY
     MOVE Ws-Stop-Nobom TO Ws-Edit
     STRING FUNCTION TRIM(BL-STOP-SUMMARY) ", no child bill "
            FUNCTION TRIM(Ws-Edit)
         DELIMITED BY SIZE INTO BL-STOP-SUMMARY.

*>----------------------------------------------------------------
*> Single level: the components of the requested parent only, with
*> no expansion at all.
 Page-Single-Level.
     IF BL-FROM-LINE < 1
         MOVE 1 TO BL-FROM-LINE
     END-IF
     MOVE BL-FROM-LINE TO Ws-I
     PERFORM UNTIL Ws-I > Ws-Total OR BL-WIN-CNT >= BL-WIN-MAX
         MOVE Ws-I TO BW-INDEX
         MOVE "GET" TO BW-FUNC
         CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
         IF BO-DEPTH = 1
             ADD 1 TO BL-WIN-CNT
             MOVE BL-WIN-CNT TO Ws-W
             PERFORM Fill-One-Line
         END-IF
         ADD 1 TO Ws-I
     END-PERFORM
     MOVE Ws-I TO BL-FROM-LINE
     IF Ws-I <= Ws-Total
         MOVE "Y" TO BL-MORE-LINES
     END-IF.

*> Indented: every occurrence in generation order, with an indent
*> that shows the path depth.
 Page-Indented.
     IF BL-FROM-LINE < 1
         MOVE 1 TO BL-FROM-LINE
     END-IF
     MOVE BL-FROM-LINE TO Ws-I
     PERFORM UNTIL Ws-I > Ws-Total OR BL-WIN-CNT >= BL-WIN-MAX
         MOVE Ws-I TO BW-INDEX
         MOVE "GET" TO BW-FUNC
         CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
         ADD 1 TO BL-WIN-CNT
         MOVE BL-WIN-CNT TO Ws-W
         PERFORM Fill-One-Line
         ADD 1 TO Ws-I
     END-PERFORM
     MOVE Ws-I TO BL-FROM-LINE
     IF Ws-I <= Ws-Total
         MOVE "Y" TO BL-MORE-LINES
     END-IF.

*> Flattened: the aggregated leaf requirement, each line carrying
*> the number of occurrences that contributed to it.
 Page-Flattened.
     IF BL-FROM-LINE < 1
         MOVE 1 TO BL-FROM-LINE
     END-IF
     MOVE BL-FROM-LINE TO Ws-I
     PERFORM UNTIL Ws-I > Ws-Leaf-Total OR BL-WIN-CNT >= BL-WIN-MAX
         MOVE Ws-I TO BW-INDEX
         MOVE "GETLEAF" TO BW-FUNC
         CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
         ADD 1 TO BL-WIN-CNT
         MOVE BL-WIN-CNT TO Ws-W
         PERFORM Fill-One-Line
         MOVE BO-SEQ TO BL-L-CONTRIB(Ws-W)
         MOVE 0 TO BL-L-DEPTH(Ws-W)
         ADD 1 TO Ws-I
     END-PERFORM
     MOVE Ws-I TO BL-FROM-LINE
     IF Ws-I <= Ws-Leaf-Total
         MOVE "Y" TO BL-MORE-LINES
     END-IF.

 Fill-One-Line.
     MOVE BO-SEQ TO BL-L-SEQ(Ws-W)
     MOVE BO-DEPTH TO BL-L-DEPTH(Ws-W)
     PERFORM Build-Indent
     MOVE Ws-Indent TO BL-L-INDENT(Ws-W)
     MOVE BO-PATH TO BL-L-PATH(Ws-W)
     MOVE BO-ITEM TO BL-L-ITEM(Ws-W)
     MOVE BO-ITEM-REV TO BL-L-REV(Ws-W)
     MOVE BO-DESCRIPTION TO BL-L-DESC(Ws-W)
     MOVE BO-STOCK-UOM TO BL-L-UOM(Ws-W)
     MOVE BO-NODE-TYPE TO BL-L-NODE-TYPE(Ws-W)
     MOVE BO-SUPPLY TO BL-L-SUPPLY(Ws-W)
     MOVE BO-STOP-REASON TO BL-L-STOP(Ws-W)
     MOVE BO-USAGE-BASIS TO BL-L-BASIS(Ws-W)
     MOVE BO-USAGE-QTY TO BL-L-USAGE(Ws-W)
     MOVE BO-UNROUNDED TO BL-L-UNROUNDED(Ws-W)
     MOVE BO-ROUND-INC TO BL-L-ROUND-INC(Ws-W)
     MOVE BO-REQ-QTY TO BL-L-REQ-QTY(Ws-W)
     MOVE BO-GOOD-QTY TO BL-L-GOOD-QTY(Ws-W)
     MOVE BO-GROSS-STARTS TO BL-L-GROSS(Ws-W)
     MOVE BO-STOCK-USED TO BL-L-STOCK-USED(Ws-W)
     MOVE BO-SHORTAGE TO BL-L-SHORTAGE(Ws-W)
     MOVE BO-PACK-QTY TO BL-L-PACK-QTY(Ws-W)
     MOVE BO-EXCESS TO BL-L-EXCESS(Ws-W)
     MOVE BO-EXT-COST TO BL-L-EXT-COST(Ws-W)
     MOVE 1 TO BL-L-CONTRIB(Ws-W)
     MOVE BO-WARNING TO BL-L-WARNING(Ws-W).

 Build-Indent.
     MOVE SPACES TO Ws-Indent
     IF BO-DEPTH <= 0
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ws-Dot = BO-DEPTH * 2
     IF Ws-Dot > 40
         MOVE 40 TO Ws-Dot
     END-IF
     MOVE ALL "." TO Ws-Indent(1:Ws-Dot).

 Total-One-Item.
     MOVE FUNCTION UPPER-CASE(BL-KEY-ITEM) TO BL-KEY-ITEM
     MOVE BL-KEY-ITEM TO BW-KEY-ITEM
     MOVE BL-KEY-UOM TO BW-KEY-UOM
     MOVE SPACES TO BW-KEY-REV
     MOVE "ALL" TO BW-FILTER
     MOVE "TOTAL" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     MOVE BW-TOTAL TO BL-KEY-TOTAL
     IF BW-COUNT = 0
         MOVE 16 TO CA-RETURN-CODE
         MOVE "BOM0605" TO CA-REASON-CODE
         MOVE SPACES TO CA-MESSAGE
         STRING "No occurrence of " FUNCTION TRIM(BL-KEY-ITEM)
                " in this calculation"
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF.
