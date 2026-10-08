>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : BOMCOST
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : Costed explosion with ONE value boundary per
*>              branch.  Material is valued where it is consumed;
*>              conversion is valued at the manufactured node that
*>              performs it; a branch roll-up is reported for
*>              presentation and is never added on top of the
*>              expanded values it already contains (BOM-021).
*>
*>              Material rates come from the stated cost set
*>              (cst.StandardCost) and, when that set holds
*>              nothing for the item, from the item's standard
*>              cost column (cst.ItemCost.StandardUnitCost).  A
*>              cost-only resource carries its own rate on the
*>              component line; without one the cost is INCOMPLETE
*>              rather than falsely complete (BOM-027).  A
*>              by-product recovery is a separate positive credit
*>              with an approved valuation, never a negative
*>              consumption (BOM-026).
*>
*>              Specification B1: material 70.10, brace conversion
*>              15.00, frame setup 10.00, frame conversion 5.00,
*>              total 100.10 and 10.01 per good frame, with the
*>              brace branch worth 75.10 once.
*> CALLED BY  : the explosion enquiry form, the costed explosion
*>              report, the costing module, the unit tests
*> INTERFACE  : CALL "BOMCOST" USING WS-BOMCOST-CA
*>              COPYBOOK BOMCOSTCA
*> CALLS      : BOMEXPL, BOMWORK, SQLIO
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2015-06-15 AJS  CR2324 COST ROLL-UP ADDED
*> 2016-02-29 AJS  CR2431 BY-PRODUCT RECOVERY CREDIT
*> 2018-04-17 KMP  CR2701 RATE CACHE - THE FIRST VERSION READ THE
*>                 COST SET ONCE PER OCCURRENCE
*> 2026-10-05 STERP MISSING RATE BLOCKS A COMPLETE COST
*>================================================================
*> REQ: BOM-021, BOM-026, BOM-027, BOM-029, BOM-032
 IDENTIFICATION DIVISION.
 PROGRAM-ID. BOMCOST.
 AUTHOR. A J SPENCER.
 DATE-WRITTEN. 15/06/2015.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY BOMWKCTL.
 COPY BOMOCCUR.
 COPY BOMEXPLCA REPLACING ==DFHCOMMAREA== BY ==Ws-Expl-Ca==
      LEADING ==CA-== BY ==EA-==
      LEADING ==BX-== BY ==EX-==.
 01  Ws-Work.
     05  Ws-I                    PIC S9(9) COMP-5 VALUE 0.
     05  Ws-W                    PIC S9(4) VALUE 0.
     05  Ws-Total                PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Rate                 PIC S9(11)V9(6) VALUE 0.
     05  Ws-Source               PIC X(12) VALUE SPACES.
     05  Ws-Ext                  PIC S9(13)V9(2) VALUE 0.
     05  Ws-Material             PIC S9(13)V9(2) VALUE 0.
     05  Ws-Conversion           PIC S9(13)V9(2) VALUE 0.
     05  Ws-Setup                PIC S9(13)V9(2) VALUE 0.
     05  Ws-Service              PIC S9(13)V9(2) VALUE 0.
     05  Ws-Recovery             PIC S9(13)V9(2) VALUE 0.
     05  Ws-Missing              PIC S9(4) VALUE 0.
     05  Ws-Edit                 PIC -(12)9.9(2).
 01  Ws-Rate-Cache.
     05  Ws-Cache-Cnt            PIC S9(4) VALUE 0.
     05  Ws-Cache OCCURS 300 TIMES.
         10  Wc-Item             PIC X(40).
         10  Wc-Rate             PIC S9(11)V9(6).
         10  Wc-Source           PIC X(12).
 01  Ws-Cost-Row.
     05  Cs-Rate                 PIC X(24).
     05  Cs-Source               PIC X(12).
 LINKAGE SECTION.
 COPY BOMCOSTCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "BOMCOST" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE "BOM-021" TO CA-REQ-REF
     IF BC-WIN-MAX < 1 OR BC-WIN-MAX > 25
         MOVE 25 TO BC-WIN-MAX
     END-IF
     MOVE 0 TO BC-WIN-CNT
     MOVE "N" TO BC-MORE-LINES
     IF BC-CURRENCY = SPACES
         MOVE "GBP" TO BC-CURRENCY
     END-IF
     EVALUATE CA-FUNCTION
         WHEN "COST"     PERFORM Run-Cost-Rollup
         WHEN "GETLINE"  PERFORM Page-The-Cost
         WHEN "BRANCH"   PERFORM Value-One-Branch
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "BOM0801" TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "BOMCOST does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>----------------------------------------------------------------
 Run-Cost-Rollup.
     MOVE 0 TO Ws-Cache-Cnt Ws-Missing
     MOVE 0 TO Ws-Material Ws-Conversion Ws-Setup
     MOVE 0 TO Ws-Service Ws-Recovery
     PERFORM Explode-For-Costing
     IF CA-RETURN-CODE > 8
         EXIT PARAGRAPH
     END-IF
     PERFORM Value-Every-Occurrence
     PERFORM Publish-Totals.

 Explode-For-Costing.
     INITIALIZE Ws-Expl-Ca
     MOVE "EXPLODE" TO EA-FUNCTION
     MOVE CA-COMPANY TO EA-COMPANY
     MOVE CA-SITE TO EA-SITE
     MOVE CA-USER TO EA-USER
     MOVE CA-BUSINESS-DATE TO EA-BUSINESS-DATE
     IF BC-NETTED = "Y"
         MOVE "NET" TO EX-CALC-TYPE
     ELSE
         MOVE "GROSS" TO EX-CALC-TYPE
     END-IF
     MOVE BC-CALC-REF TO EX-CALC-REF
     MOVE BC-PARENT-ITEM TO EX-PARENT-ITEM
     MOVE BC-PARENT-REV TO EX-PARENT-REV
     MOVE BC-PURPOSE TO EX-PURPOSE
     MOVE BC-VARIANT TO EX-VARIANT
     MOVE BC-REQ-QTY TO EX-REQ-QTY
     MOVE BC-EFF-DATE TO EX-EFF-DATE
     MOVE BC-MAX-DEPTH TO EX-MAX-DEPTH
     MOVE "Y" TO EX-EXPAND-PHANTOM
     MOVE "Y" TO EX-INCLUDE-NOTES
     MOVE "Y" TO EX-INCLUDE-BYPROD
     MOVE BC-COST-SET TO EX-COST-SET
     MOVE BC-CURRENCY TO EX-CURRENCY
     CALL "BOMEXPL" USING Ws-Expl-Ca
     MOVE EX-COMPLETENESS TO BC-COMPLETENESS
     MOVE EX-ERROR-PATH TO BC-ERROR-PATH
     MOVE EX-GROSS-STARTS TO BC-GROSS-STARTS
     MOVE EX-RUN-COUNT TO BC-RUN-COUNT
     MOVE EX-BATCH-COUNT TO BC-BATCH-COUNT
     MOVE EX-OCC-COUNT TO BC-OCC-COUNT
     MOVE EA-RETURN-CODE TO CA-RETURN-CODE
     MOVE EA-REASON-CODE TO CA-REASON-CODE
     MOVE EA-MESSAGE TO CA-MESSAGE.

*>----------------------------------------------------------------
*> One pass over the occurrence pool.  Each kind of node is valued
*> in exactly one place, which is what stops an intermediate value
*> being added twice.
 Value-Every-Occurrence.
     MOVE "COUNT" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     MOVE BW-COUNT TO Ws-Total
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > Ws-Total
         MOVE Ws-I TO BW-INDEX
         MOVE "GET" TO BW-FUNC
         CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
         PERFORM Value-One-Occurrence
         MOVE Ws-I TO BW-INDEX
         MOVE "PUT" TO BW-FUNC
         CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     END-PERFORM.

 Value-One-Occurrence.
     MOVE 0 TO Ws-Ext
     EVALUATE TRUE
         WHEN BO-NODE-TYPE = "NOTE"
             MOVE 0 TO BO-UNIT-COST
             MOVE "NONE" TO BO-COST-SOURCE
         WHEN BO-NODE-TYPE = "BYPROD"
             PERFORM Value-A-By-Product
         WHEN BO-NODE-TYPE = "PHANTOM"
             MOVE 0 TO BO-UNIT-COST
             MOVE "PHANTOM" TO BO-COST-SOURCE
         WHEN BO-NODE-TYPE = "INTERMED"
             PERFORM Value-A-Conversion
         WHEN BO-SUPPLY = "SERVICE" OR BO-SUPPLY = "COSTONLY"
             PERFORM Value-A-Resource
         WHEN OTHER
             PERFORM Value-A-Material
     END-EVALUATE
     MOVE Ws-Ext TO BO-EXT-COST.

*> Material: the cost set first, the item standard cost second.
 Value-A-Material.
     PERFORM Find-Material-Rate
     MOVE Ws-Rate TO BO-UNIT-COST
     MOVE Ws-Source TO BO-COST-SOURCE
     MOVE "MAT" TO BO-COST-ELEMENT
     MOVE "N" TO BO-FIXED-COST
     IF Ws-Source = "MISSING"
         ADD 1 TO Ws-Missing
         MOVE "No cost held for this item in the stated cost set"
             TO BO-WARNING
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ws-Ext ROUNDED = BO-REQ-QTY * Ws-Rate
     ADD Ws-Ext TO Ws-Material.

*> Conversion: setup per execution run plus a rate per gross
*> start, held on the bill of the manufactured node.  The setup
*> and the per-start rate arrive on the occurrence from BOMEXPL.
 Value-A-Conversion.
     MOVE "LAB" TO BO-COST-ELEMENT
     MOVE "BOMHEADER" TO BO-COST-SOURCE
     MOVE "Y" TO BO-FIXED-COST
     COMPUTE Ws-Ext ROUNDED =
         BO-RUN-COUNT * BO-UNIT-COST
         + BO-GROSS-STARTS * BO-BRANCH-COST
     IF BO-RUN-COUNT > 0 AND BO-UNIT-COST > 0
         COMPUTE Ws-Setup ROUNDED =
             Ws-Setup + BO-RUN-COUNT * BO-UNIT-COST
     END-IF
     ADD Ws-Ext TO Ws-Conversion.

*> A service or a cost-only resource is valued from the rate held
*> on its own component line.  No rate, no complete cost.
 Value-A-Resource.
     MOVE "CSTONLY" TO BO-COST-SOURCE
     IF BO-COST-ELEMENT = SPACES
         MOVE "SUBC" TO BO-COST-ELEMENT
     END-IF
     MOVE "N" TO BO-FIXED-COST
     IF BO-UNIT-COST <= 0
         ADD 1 TO Ws-Missing
         MOVE "MISSING" TO BO-COST-SOURCE
         MOVE "No rate is held for this resource line"
             TO BO-WARNING
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ws-Ext ROUNDED = BO-REQ-QTY * BO-UNIT-COST
     ADD Ws-Ext TO Ws-Service.

*> A recovery is a positive expected output valued on its approved
*> basis; it is credited once, at the end.
 Value-A-By-Product.
     MOVE "RECY" TO BO-COST-ELEMENT
     IF BO-COST-SOURCE = SPACES
         MOVE "NRV" TO BO-COST-SOURCE
     END-IF
     COMPUTE Ws-Ext ROUNDED = BO-REQ-QTY * BO-UNIT-COST
     ADD Ws-Ext TO Ws-Recovery
     MOVE 0 TO Ws-Ext.

*>----------------------------------------------------------------
 Publish-Totals.
     MOVE Ws-Material TO BC-MATERIAL-COST
     MOVE Ws-Conversion TO BC-CONVERSION-COST
     MOVE Ws-Setup TO BC-SETUP-COST
     MOVE Ws-Service TO BC-SERVICE-COST
     MOVE Ws-Recovery TO BC-RECOVERY-CREDIT
     COMPUTE BC-TOTAL-COST =
         Ws-Material + Ws-Conversion + Ws-Service - Ws-Recovery
     MOVE Ws-Missing TO BC-MISSING-RATES
     IF BC-REQ-QTY > 0
         MOVE BC-REQ-QTY TO BC-UNIT-DENOM
         COMPUTE BC-UNIT-COST ROUNDED =
             BC-TOTAL-COST / BC-REQ-QTY
     ELSE
         MOVE 0 TO BC-UNIT-DENOM BC-UNIT-COST
     END-IF
     IF Ws-Missing > 0
         MOVE "INCOMPLETE" TO BC-COMPLETENESS
         MOVE 4 TO CA-RETURN-CODE
         MOVE "BOM0810" TO CA-REASON-CODE
         MOVE SPACES TO CA-MESSAGE
         STRING "A missing rate prevents a complete approved cost"
             DELIMITED BY SIZE INTO CA-MESSAGE
         ADD 1 TO CA-MSG-COUNT
         EXIT PARAGRAPH
     END-IF
     IF BC-COMPLETENESS = "COMPLETE"
         MOVE 0 TO CA-RETURN-CODE
         MOVE BC-TOTAL-COST TO Ws-Edit
         MOVE SPACES TO CA-MESSAGE
         STRING "Cost roll-up total " FUNCTION TRIM(Ws-Edit) " "
                FUNCTION TRIM(BC-CURRENCY)
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF.

*>----------------------------------------------------------------
*> BRANCH - the roll-up value of one sub-tree, for presentation.
*> It already contains its children; a report that shows it must
*> not also add the children again (BOM-021, B1's 75.10).
 Value-One-Branch.
     MOVE BC-BRANCH-SEQ TO BW-INDEX
     MOVE "GET" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     IF BW-STATUS NOT = "OK"
         MOVE 16 TO CA-RETURN-CODE
         MOVE "BOM0811" TO CA-REASON-CODE
         MOVE "That branch is not part of the retained calculation"
             TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE BO-ITEM TO BC-BRANCH-ITEM
     MOVE BO-REQ-QTY TO BC-BRANCH-QTY
     MOVE BC-BRANCH-SEQ TO BW-INDEX
     MOVE "BRANCH" TO BW-FILTER
     MOVE "COSTSUM" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     MOVE BW-MONEY TO BC-BRANCH-COST
     IF BC-BRANCH-QTY > 0
         COMPUTE BC-BRANCH-UNIT ROUNDED =
             BC-BRANCH-COST / BC-BRANCH-QTY
     ELSE
         MOVE 0 TO BC-BRANCH-UNIT
     END-IF
     MOVE BC-BRANCH-COST TO Ws-Edit
     MOVE SPACES TO CA-MESSAGE
     STRING "Branch " FUNCTION TRIM(BC-BRANCH-ITEM)
            " rolls up to " FUNCTION TRIM(Ws-Edit)
            " - included once, never added again"
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>----------------------------------------------------------------
 Page-The-Cost.
     MOVE "COUNT" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     MOVE BW-COUNT TO Ws-Total
     MOVE BW-COUNT TO BC-OCC-COUNT
     IF BC-FROM-LINE < 1
         MOVE 1 TO BC-FROM-LINE
     END-IF
     MOVE BC-FROM-LINE TO Ws-I
     PERFORM UNTIL Ws-I > Ws-Total OR BC-WIN-CNT >= BC-WIN-MAX
         MOVE Ws-I TO BW-INDEX
         MOVE "GET" TO BW-FUNC
         CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
         ADD 1 TO BC-WIN-CNT
         MOVE BC-WIN-CNT TO Ws-W
         PERFORM Fill-Cost-Line
         ADD 1 TO Ws-I
     END-PERFORM
     MOVE Ws-I TO BC-FROM-LINE
     IF Ws-I <= Ws-Total
         MOVE "Y" TO BC-MORE-LINES
     END-IF.

 Fill-Cost-Line.
     MOVE BO-SEQ TO BC-L-SEQ(Ws-W)
     MOVE BO-DEPTH TO BC-L-DEPTH(Ws-W)
     MOVE BO-PATH TO BC-L-PATH(Ws-W)
     MOVE BO-ITEM TO BC-L-ITEM(Ws-W)
     MOVE BO-ITEM-REV TO BC-L-REV(Ws-W)
     MOVE BO-DESCRIPTION TO BC-L-DESC(Ws-W)
     MOVE BO-STOCK-UOM TO BC-L-UOM(Ws-W)
     MOVE BO-NODE-TYPE TO BC-L-NODE-TYPE(Ws-W)
     MOVE BO-COST-ELEMENT TO BC-L-ELEMENT(Ws-W)
     MOVE BO-COST-SOURCE TO BC-L-SOURCE(Ws-W)
     MOVE BO-REQ-QTY TO BC-L-REQ-QTY(Ws-W)
     MOVE BO-UNIT-COST TO BC-L-UNIT-COST(Ws-W)
     MOVE BO-EXT-COST TO BC-L-EXT-COST(Ws-W)
     MOVE 0 TO BC-L-BRANCH(Ws-W)
     MOVE BO-FIXED-COST TO BC-L-FIXED(Ws-W)
     MOVE BO-WARNING TO BC-L-WARNING(Ws-W).

*>----------------------------------------------------------------
*> The rate cache.  CR2701: one reading per item instead of one
*> per occurrence - B1 alone asks for pins three times.
 Find-Material-Rate.
     MOVE 0 TO Ws-Rate
     MOVE SPACES TO Ws-Source
     PERFORM VARYING Ws-W FROM 1 BY 1
         UNTIL Ws-W > Ws-Cache-Cnt OR Ws-Source NOT = SPACES
         IF Wc-Item(Ws-W) = BO-ITEM
             MOVE Wc-Rate(Ws-W) TO Ws-Rate
             MOVE Wc-Source(Ws-W) TO Ws-Source
         END-IF
     END-PERFORM
     IF Ws-Source NOT = SPACES
         EXIT PARAGRAPH
     END-IF
     PERFORM Read-Material-Rate
     IF Ws-Cache-Cnt < 300
         ADD 1 TO Ws-Cache-Cnt
         MOVE BO-ITEM TO Wc-Item(Ws-Cache-Cnt)
         MOVE Ws-Rate TO Wc-Rate(Ws-Cache-Cnt)
         MOVE Ws-Source TO Wc-Source(Ws-Cache-Cnt)
     END-IF.

*> An item can carry several standard cost rows - one per
*> revision and per valuation site - so the reading is ranked
*> before it is taken: the named cost set first, then the item
*> standard column for this site, the item level (blank revision)
*> before a revision of its own, and only then another site.
*> Without that ranking the rate taken depends on the order the
*> database happens to return, which is not a cost at all.
 Read-Material-Rate.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT TOP (1) r.UnitCost, r.Src FROM ("
            "SELECT sc.TotalUnitCost AS UnitCost, "
            "'COSTSET' AS Src, "
            "CASE WHEN sc.Revision = '' THEN 1 ELSE 2 END AS Pref, "
            "sc.Revision AS Rv, '' AS St "
            "FROM cst.StandardCost sc "
            "WHERE sc.CompanyCode = ? AND sc.ItemCode = ? "
            "AND sc.CostSetCode = ? "
            "UNION ALL "
            "SELECT ic.StandardUnitCost, 'ITEMSTD', "
            "3 + CASE WHEN ic.SiteCode = ? THEN 0 ELSE 2 END "
            "+ CASE WHEN ic.Revision = '' THEN 0 ELSE 1 END, "
            "ic.Revision, ic.SiteCode "
            "FROM cst.ItemCost ic "
            "WHERE ic.CompanyCode = ? AND ic.ItemCode = ? "
            "AND ic.StandardUnitCost IS NOT NULL) r "
            "ORDER BY r.Pref, r.St, r.Rv"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 6 TO SQLIO-PARM-CNT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE BO-ITEM TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE BC-COST-SET TO SQLIO-PARM-VAL(3)
     MOVE "N" TO SQLIO-PARM-NULL(3)
     MOVE CA-SITE TO SQLIO-PARM-VAL(4)
     MOVE "N" TO SQLIO-PARM-NULL(4)
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(5)
     MOVE "N" TO SQLIO-PARM-NULL(5)
     MOVE BO-ITEM TO SQLIO-PARM-VAL(6)
     MOVE "N" TO SQLIO-PARM-NULL(6)
     MOVE 2 TO SQLIO-COL-CNT
     MOVE 24 TO SQLIO-COL-WIDTH(1)
     MOVE 12 TO SQLIO-COL-WIDTH(2)
     MOVE 8 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Cost-Row
     IF SQLIO-ERROR OR SQLIO-NOTFOUND
         MOVE 0 TO Ws-Rate
         MOVE "MISSING" TO Ws-Source
         EXIT PARAGRAPH
     END-IF
     COMPUTE Ws-Rate = FUNCTION NUMVAL(Cs-Rate)
     MOVE Cs-Source TO Ws-Source
     IF Ws-Rate = 0
         MOVE "MISSING" TO Ws-Source
     END-IF.
