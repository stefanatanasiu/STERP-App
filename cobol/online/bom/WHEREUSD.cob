>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : WHEREUSD
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : Where-used: which bills use this item, directly
*>              and indirectly, with the selection context of each
*>              use and the open execution demand it would touch.
*>              Named WHEREUSD rather than WHEREUSED because a
*>              program identifier is eight characters.
*>
*>              BOM-023 draws a careful line: a material change
*>              must be able to find every potentially affected
*>              active product, but a use that no permitted
*>              configuration could activate is reported as
*>              POSSIBLE, never as confirmed usage.  An
*>              unselected optional or alternate line is therefore
*>              listed and labelled, not counted as fact.
*>
*>              The walk is iterative rather than recursive: a
*>              where-used fans out upwards and the frontier is
*>              easier to bound that way.
*> CALLED BY  : the where-used form and report, ECOIMPCT, BOMBULK
*> INTERFACE  : CALL "WHEREUSD" USING WS-WHEREUSD-CA
*>              COPYBOOK WHEREUSDCA
*> CALLS      : SQLIO
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2014-10-07 AJS  CR2248 WHERE USED REWRITTEN ITERATIVELY
*> 2026-10-05 STERP OPEN EXECUTION USE AND CERTAINTY LABEL
*>================================================================
*> REQ: BOM-023, BOM-028, BOM-031, BOM-029
 IDENTIFICATION DIVISION.
 PROGRAM-ID. WHEREUSD.
 AUTHOR. A J SPENCER.
 DATE-WRITTEN. 07/10/2014.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 01  Ws-Limits.
     05  Ws-Res-Max              PIC S9(4) VALUE 400.
     05  Ws-Front-Max            PIC S9(4) VALUE 200.
 01  Ws-Work.
     05  Ws-I                    PIC S9(4) VALUE 0.
     05  Ws-J                    PIC S9(4) VALUE 0.
     05  Ws-W                    PIC S9(4) VALUE 0.
     05  Ws-Level                PIC S9(4) VALUE 0.
     05  Ws-More                 PIC X VALUE "Y".
     05  Ws-Found                PIC X VALUE "N".
     05  Ws-Res-Cnt              PIC S9(4) VALUE 0.
     05  Ws-Cur-Cnt              PIC S9(4) VALUE 0.
     05  Ws-Next-Cnt             PIC S9(4) VALUE 0.
     05  Ws-Cur-Item             PIC X(40) VALUE SPACES.
     05  Ws-Cur-Path             PIC X(120) VALUE SPACES.
     05  Ws-Limit-Hit            PIC X VALUE "N".
     05  Ws-Yielded              PIC X VALUE "N".
 01  Ws-Frontier.
     05  Ws-Cur OCCURS 200 TIMES.
         10  Wf-Item             PIC X(40).
         10  Wf-Path             PIC X(120).
     05  Ws-Next OCCURS 200 TIMES.
         10  Wn-Item             PIC X(40).
         10  Wn-Path             PIC X(120).
 01  Ws-Results.
     05  Ws-Res OCCURS 400 TIMES.
         10  Wr-Level            PIC S9(4).
         10  Wr-Parent           PIC X(40).
         10  Wr-Parent-Rev       PIC X(4).
         10  Wr-Desc             PIC X(40).
         10  Wr-Bom-Ref          PIC X(30).
         10  Wr-Bom-Rev          PIC X(4).
         10  Wr-Status           PIC X(12).
         10  Wr-Purpose          PIC X(10).
         10  Wr-Variant          PIC X(20).
         10  Wr-Site             PIC X(10).
         10  Wr-Line-Num         PIC S9(4).
         10  Wr-Usage            PIC S9(13)V9(6).
         10  Wr-Uom              PIC X(10).
         10  Wr-Basis            PIC X(12).
         10  Wr-Eff-From         PIC 9(8).
         10  Wr-Eff-To           PIC 9(8).
         10  Wr-Certainty        PIC X(10).
         10  Wr-Has-Parent       PIC X.
         10  Wr-Path             PIC X(120).
         10  Wr-Demand           PIC X(60).
 01  Ws-Row.
     05  Rw-Bom-Id               PIC X(12).
     05  Rw-Parent               PIC X(40).
     05  Rw-Parent-Rev           PIC X(4).
     05  Rw-Desc                 PIC X(40).
     05  Rw-Bom-Ref              PIC X(30).
     05  Rw-Bom-Rev              PIC X(4).
     05  Rw-Status               PIC X(12).
     05  Rw-Purpose              PIC X(10).
     05  Rw-Variant              PIC X(20).
     05  Rw-Site                 PIC X(10).
     05  Rw-Line-Num             PIC X(8).
     05  Rw-Usage                PIC X(24).
     05  Rw-Uom                  PIC X(10).
     05  Rw-Basis                PIC X(12).
     05  Rw-Eff-From             PIC X(8).
     05  Rw-Eff-To               PIC X(8).
     05  Rw-Certainty            PIC X(10).
 01  Ws-Dem-Row.
     05  Dm-Type                 PIC X(20).
     05  Dm-Ref                  PIC X(40).
     05  Dm-Status               PIC X(12).
     05  Dm-Parent               PIC X(40).
     05  Dm-Qty                  PIC X(24).
     05  Dm-Date                 PIC X(8).
 LINKAGE SECTION.
 COPY WHEREUSDCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "WHEREUSD" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE "BOM-023" TO CA-REQ-REF
     MOVE FUNCTION UPPER-CASE(WU-ITEM) TO WU-ITEM
     IF WU-EFF-DATE = 0
         MOVE CA-BUSINESS-DATE TO WU-EFF-DATE
     END-IF
     IF WU-MAX-DEPTH < 1 OR WU-MAX-DEPTH > 25
         MOVE 25 TO WU-MAX-DEPTH
     END-IF
     IF WU-WIN-MAX < 1 OR WU-WIN-MAX > 25
         MOVE 25 TO WU-WIN-MAX
     END-IF
     MOVE 0 TO WU-WIN-CNT
     MOVE "N" TO WU-MORE-LINES
     EVALUATE CA-FUNCTION
         WHEN "DIRECT"   PERFORM Walk-Upwards-One-Level
                         PERFORM Page-The-Result
         WHEN "ALL"      PERFORM Walk-Upwards-Fully
                         PERFORM Page-The-Result
         WHEN "TOPLEVEL" PERFORM Walk-Upwards-Fully
                         PERFORM Keep-Only-Top-Level
                         PERFORM Page-The-Result
         WHEN "OPENUSE"  PERFORM Find-Open-Demand
                         PERFORM Page-The-Result
         WHEN "GETLINE"  PERFORM Page-The-Result
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "BOM0901" TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "WHEREUSD does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>----------------------------------------------------------------
 Start-The-Walk.
     MOVE 0 TO Ws-Res-Cnt WU-PARENT-CNT WU-TOP-CNT WU-DEMAND-CNT
     MOVE 0 TO WU-DEPTH-REACHED
     MOVE "N" TO Ws-Limit-Hit
     MOVE "COMPLETE" TO WU-COMPLETENESS
     MOVE 1 TO Ws-Cur-Cnt
     MOVE WU-ITEM TO Wf-Item(1)
     MOVE WU-ITEM TO Wf-Path(1)
     MOVE 0 TO Ws-Level.

 Walk-Upwards-One-Level.
     PERFORM Start-The-Walk
     PERFORM One-Level-Up
     PERFORM Finish-The-Walk.

 Walk-Upwards-Fully.
     PERFORM Start-The-Walk
     PERFORM UNTIL Ws-Cur-Cnt = 0 OR Ws-Level >= WU-MAX-DEPTH
                   OR Ws-Limit-Hit = "Y"
         PERFORM One-Level-Up
         PERFORM Promote-The-Frontier
     END-PERFORM
     PERFORM Finish-The-Walk.

 Finish-The-Walk.
     MOVE Ws-Res-Cnt TO WU-PARENT-CNT
     MOVE Ws-Level TO WU-DEPTH-REACHED
     IF Ws-Limit-Hit = "Y"
         MOVE "INCOMPLETE" TO WU-COMPLETENESS
         MOVE 4 TO CA-RETURN-CODE
         MOVE "BOM0902" TO CA-REASON-CODE
         MOVE "More parents exist than this enquiry can carry"
             TO CA-MESSAGE
         ADD 1 TO CA-MSG-COUNT
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO CA-RETURN-CODE
     IF Ws-Res-Cnt = 0
         MOVE 16 TO CA-RETURN-CODE
         MOVE "BOM0903" TO CA-REASON-CODE
         MOVE SPACES TO CA-MESSAGE
         STRING FUNCTION TRIM(WU-ITEM)
                " is not used on any bill for that date"
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF.

 Promote-The-Frontier.
     MOVE 0 TO Ws-Cur-Cnt
     PERFORM VARYING Ws-J FROM 1 BY 1 UNTIL Ws-J > Ws-Next-Cnt
         ADD 1 TO Ws-Cur-Cnt
         MOVE Wn-Item(Ws-J) TO Wf-Item(Ws-Cur-Cnt)
         MOVE Wn-Path(Ws-J) TO Wf-Path(Ws-Cur-Cnt)
     END-PERFORM
     MOVE 0 TO Ws-Next-Cnt.

 One-Level-Up.
     ADD 1 TO Ws-Level
     MOVE 0 TO Ws-Next-Cnt
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > Ws-Cur-Cnt
         MOVE Wf-Item(Ws-I) TO Ws-Cur-Item
         MOVE Wf-Path(Ws-I) TO Ws-Cur-Path
         PERFORM Read-Direct-Parents
     END-PERFORM.

*>----------------------------------------------------------------
*> One level of parents.  An optional or alternate line that is
*> not selected is POSSIBLE usage; everything else in force on the
*> date is CONFIRMED.
 Read-Direct-Parents.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT h.BomHeaderId, h.ParentItemCode, "
            "ISNULL(h.ParentRevision,''), "
            "ISNULL(i.Description,''), h.BomRef, h.BomRevision, "
            "h.Status, h.Purpose, ISNULL(h.VariantCode,''), "
            "ISNULL(h.SiteCode,''), c.LineNum, c.UsageQty, "
            "c.UomCode, c.UsageBasis, "
            "ISNULL(CONVERT(char(8),c.EffectiveFrom,112),"
            "'00000000'), "
            "ISNULL(CONVERT(char(8),c.EffectiveTo,112),'00000000'),"
            " CASE WHEN c.IsOptional = 1 AND c.IsSelected = 0 "
            "THEN 'POSSIBLE' WHEN c.AlternateGroup IS NOT NULL "
            "AND c.IsSelected = 0 THEN 'POSSIBLE' "
            "ELSE 'CONFIRMED' END "
            "FROM bom.BomComponent c "
            "JOIN bom.BomHeader h ON h.BomHeaderId = c.BomHeaderId "
            "LEFT JOIN inv.Item i ON i.CompanyCode = h.CompanyCode "
            "AND i.ItemCode = h.ParentItemCode "
            "WHERE h.CompanyCode = ? AND c.ComponentItemCode = ? "
            "AND (h.Status = 'RELEASED' OR ? = 'Y') "
            "AND h.Status <> 'RETIRED' "
            "AND h.EffectiveFrom <= CONVERT(date,?,112) "
            "AND (h.EffectiveTo IS NULL "
            "OR h.EffectiveTo > CONVERT(date,?,112)) "
            "ORDER BY h.ParentItemCode, c.LineNum"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 5 TO SQLIO-PARM-CNT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE Ws-Cur-Item TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     IF WU-INCLUDE-DRAFT = "Y"
         MOVE "Y" TO SQLIO-PARM-VAL(3)
     ELSE
         MOVE "N" TO SQLIO-PARM-VAL(3)
     END-IF
     MOVE "N" TO SQLIO-PARM-NULL(3)
     MOVE WU-EFF-DATE TO SQLIO-PARM-VAL(4)
     MOVE "N" TO SQLIO-PARM-NULL(4)
     MOVE WU-EFF-DATE TO SQLIO-PARM-VAL(5)
     MOVE "N" TO SQLIO-PARM-NULL(5)
     PERFORM Set-Parent-Widths
     MOVE 1 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "N" TO Ws-Yielded
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE 1 TO SQLIO-CURSOR
         MOVE "FTCH" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA Ws-Row
         IF SQLIO-OK
             MOVE "Y" TO Ws-Yielded
             PERFORM Keep-One-Parent
         ELSE
             MOVE "N" TO Ws-More
         END-IF
     END-PERFORM
     MOVE 1 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Row
     PERFORM Mark-Parent-Has-Parent.

*> Everything that used Ws-Cur-Item is no longer at the top of its
*> path, because something above it has just been found.
 Mark-Parent-Has-Parent.
     IF Ws-Yielded = "N"
         EXIT PARAGRAPH
     END-IF
     PERFORM VARYING Ws-J FROM 1 BY 1 UNTIL Ws-J > Ws-Res-Cnt
         IF Wr-Parent(Ws-J) = Ws-Cur-Item
             MOVE "Y" TO Wr-Has-Parent(Ws-J)
         END-IF
     END-PERFORM.

 Set-Parent-Widths.
     MOVE 17 TO SQLIO-COL-CNT
     MOVE 12 TO SQLIO-COL-WIDTH(1)
     MOVE 40 TO SQLIO-COL-WIDTH(2)
     MOVE 4 TO SQLIO-COL-WIDTH(3)
     MOVE 40 TO SQLIO-COL-WIDTH(4)
     MOVE 30 TO SQLIO-COL-WIDTH(5)
     MOVE 4 TO SQLIO-COL-WIDTH(6)
     MOVE 12 TO SQLIO-COL-WIDTH(7)
     MOVE 10 TO SQLIO-COL-WIDTH(8)
     MOVE 20 TO SQLIO-COL-WIDTH(9)
     MOVE 10 TO SQLIO-COL-WIDTH(10)
     MOVE 8 TO SQLIO-COL-WIDTH(11)
     MOVE 24 TO SQLIO-COL-WIDTH(12)
     MOVE 10 TO SQLIO-COL-WIDTH(13)
     MOVE 12 TO SQLIO-COL-WIDTH(14)
     MOVE 8 TO SQLIO-COL-WIDTH(15)
     MOVE 8 TO SQLIO-COL-WIDTH(16)
     MOVE 10 TO SQLIO-COL-WIDTH(17).

 Keep-One-Parent.
     IF Ws-Res-Cnt >= Ws-Res-Max
         MOVE "Y" TO Ws-Limit-Hit
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Res-Cnt
     MOVE Ws-Level TO Wr-Level(Ws-Res-Cnt)
     MOVE Rw-Parent TO Wr-Parent(Ws-Res-Cnt)
     MOVE Rw-Parent-Rev TO Wr-Parent-Rev(Ws-Res-Cnt)
     MOVE Rw-Desc TO Wr-Desc(Ws-Res-Cnt)
     MOVE Rw-Bom-Ref TO Wr-Bom-Ref(Ws-Res-Cnt)
     MOVE Rw-Bom-Rev TO Wr-Bom-Rev(Ws-Res-Cnt)
     MOVE Rw-Status TO Wr-Status(Ws-Res-Cnt)
     MOVE Rw-Purpose TO Wr-Purpose(Ws-Res-Cnt)
     MOVE Rw-Variant TO Wr-Variant(Ws-Res-Cnt)
     MOVE Rw-Site TO Wr-Site(Ws-Res-Cnt)
     COMPUTE Wr-Line-Num(Ws-Res-Cnt) = FUNCTION NUMVAL(Rw-Line-Num)
     COMPUTE Wr-Usage(Ws-Res-Cnt) = FUNCTION NUMVAL(Rw-Usage)
     MOVE Rw-Uom TO Wr-Uom(Ws-Res-Cnt)
     MOVE Rw-Basis TO Wr-Basis(Ws-Res-Cnt)
     IF Rw-Eff-From IS NUMERIC
         MOVE Rw-Eff-From TO Wr-Eff-From(Ws-Res-Cnt)
     ELSE
         MOVE 0 TO Wr-Eff-From(Ws-Res-Cnt)
     END-IF
     IF Rw-Eff-To IS NUMERIC
         MOVE Rw-Eff-To TO Wr-Eff-To(Ws-Res-Cnt)
     ELSE
         MOVE 0 TO Wr-Eff-To(Ws-Res-Cnt)
     END-IF
     MOVE Rw-Certainty TO Wr-Certainty(Ws-Res-Cnt)
     MOVE SPACES TO Wr-Path(Ws-Res-Cnt)
     STRING FUNCTION TRIM(Rw-Parent) " <- "
            FUNCTION TRIM(Ws-Cur-Path)
         DELIMITED BY SIZE INTO Wr-Path(Ws-Res-Cnt)
     MOVE SPACES TO Wr-Demand(Ws-Res-Cnt)
     PERFORM Queue-The-Parent.

*> A parent appears on the next frontier once, however many lines
*> reach it: the structure above it is the same either way.
 Queue-The-Parent.
     MOVE "N" TO Ws-Found
     PERFORM VARYING Ws-J FROM 1 BY 1
         UNTIL Ws-J > Ws-Next-Cnt OR Ws-Found = "Y"
         IF Wn-Item(Ws-J) = Rw-Parent
             MOVE "Y" TO Ws-Found
         END-IF
     END-PERFORM
     IF Ws-Found = "Y" OR Ws-Next-Cnt >= Ws-Front-Max
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Next-Cnt
     MOVE Rw-Parent TO Wn-Item(Ws-Next-Cnt)
     MOVE Wr-Path(Ws-Res-Cnt) TO Wn-Path(Ws-Next-Cnt).

*> Only the products at the top of each path.  A parent is at the
*> top when nothing above it was ever found: that is recorded as
*> the walk goes, which is cheaper and clearer than deducing it
*> from the paths afterwards.
 Keep-Only-Top-Level.
     MOVE 0 TO Ws-W
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > Ws-Res-Cnt
         IF Wr-Has-Parent(Ws-I) NOT = "Y"
             ADD 1 TO Ws-W
             MOVE Ws-Res(Ws-I) TO Ws-Res(Ws-W)
         END-IF
     END-PERFORM
     MOVE Ws-W TO Ws-Res-Cnt
     MOVE Ws-W TO WU-TOP-CNT
     MOVE Ws-W TO WU-PARENT-CNT.

*>----------------------------------------------------------------
*> Released kits and open stock reservations that use the item.
*> This is the half of BOM-023 that a change needs: not just the
*> definitions, but the work already committed against them.
 Find-Open-Demand.
     PERFORM Start-The-Walk
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT 'EXECUTIONKIT', k.KitRef, k.Status, "
            "k.ParentItemCode, k.PlannedGoodQty, "
            "ISNULL(CONVERT(char(8),k.NeedDate,112),'00000000') "
            "FROM kit.ExecutionKit k "
            "WHERE k.CompanyCode = ? AND k.Status IN "
            "('PLANNED','RELEASED','PICKED','INPROGRESS') "
            "AND (k.ParentItemCode = ? OR EXISTS "
            "(SELECT 1 FROM kit.ExecutionKitLine l "
            "WHERE l.ExecutionKitId = k.ExecutionKitId "
            "AND l.ComponentItemCode = ?)) "
            "UNION ALL "
            "SELECT 'RESERVATION', r.DemandRef, r.Status, "
            "r.ItemCode, r.QtyReserved, "
            "ISNULL(CONVERT(char(8),r.RequiredDate,112),'00000000') "
            "FROM inv.StockReservation r "
            "WHERE r.CompanyCode = ? AND r.ItemCode = ? "
            "AND r.Status = 'ACTIVE'"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 5 TO SQLIO-PARM-CNT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE WU-ITEM TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE WU-ITEM TO SQLIO-PARM-VAL(3)
     MOVE "N" TO SQLIO-PARM-NULL(3)
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(4)
     MOVE "N" TO SQLIO-PARM-NULL(4)
     MOVE WU-ITEM TO SQLIO-PARM-VAL(5)
     MOVE "N" TO SQLIO-PARM-NULL(5)
     MOVE 6 TO SQLIO-COL-CNT
     MOVE 20 TO SQLIO-COL-WIDTH(1)
     MOVE 40 TO SQLIO-COL-WIDTH(2)
     MOVE 12 TO SQLIO-COL-WIDTH(3)
     MOVE 40 TO SQLIO-COL-WIDTH(4)
     MOVE 24 TO SQLIO-COL-WIDTH(5)
     MOVE 8 TO SQLIO-COL-WIDTH(6)
     MOVE 1 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Dem-Row
     IF SQLIO-ERROR
         PERFORM Report-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-More
     PERFORM UNTIL Ws-More = "N"
         MOVE 1 TO SQLIO-CURSOR
         MOVE "FTCH" TO SQLIO-FUNC
         CALL "SQLIO" USING SQLIO-CA Ws-Dem-Row
         IF SQLIO-OK
             PERFORM Keep-One-Demand
         ELSE
             MOVE "N" TO Ws-More
         END-IF
     END-PERFORM
     MOVE 1 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Dem-Row
     MOVE Ws-Res-Cnt TO WU-DEMAND-CNT
     IF Ws-Res-Cnt = 0
         MOVE 16 TO CA-RETURN-CODE
         MOVE "BOM0904" TO CA-REASON-CODE
         MOVE "No open execution demand uses that item"
             TO CA-MESSAGE
     END-IF.

 Keep-One-Demand.
     IF Ws-Res-Cnt >= Ws-Res-Max
         MOVE "Y" TO Ws-Limit-Hit
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Res-Cnt
     MOVE 0 TO Wr-Level(Ws-Res-Cnt)
     MOVE Dm-Parent TO Wr-Parent(Ws-Res-Cnt)
     MOVE SPACES TO Wr-Parent-Rev(Ws-Res-Cnt)
     MOVE Dm-Type TO Wr-Desc(Ws-Res-Cnt)
     MOVE Dm-Ref TO Wr-Bom-Ref(Ws-Res-Cnt)
     MOVE SPACES TO Wr-Bom-Rev(Ws-Res-Cnt)
     MOVE Dm-Status TO Wr-Status(Ws-Res-Cnt)
     MOVE "DEMAND" TO Wr-Purpose(Ws-Res-Cnt)
     MOVE SPACES TO Wr-Variant(Ws-Res-Cnt)
     MOVE CA-SITE TO Wr-Site(Ws-Res-Cnt)
     MOVE 0 TO Wr-Line-Num(Ws-Res-Cnt)
     COMPUTE Wr-Usage(Ws-Res-Cnt) = FUNCTION NUMVAL(Dm-Qty)
     MOVE SPACES TO Wr-Uom(Ws-Res-Cnt)
     MOVE "DEMAND" TO Wr-Basis(Ws-Res-Cnt)
     IF Dm-Date IS NUMERIC
         MOVE Dm-Date TO Wr-Eff-From(Ws-Res-Cnt)
     ELSE
         MOVE 0 TO Wr-Eff-From(Ws-Res-Cnt)
     END-IF
     MOVE 0 TO Wr-Eff-To(Ws-Res-Cnt)
     MOVE "CONFIRMED" TO Wr-Certainty(Ws-Res-Cnt)
     MOVE SPACES TO Wr-Path(Ws-Res-Cnt)
     STRING FUNCTION TRIM(Dm-Type) " "
            FUNCTION TRIM(Dm-Ref)
         DELIMITED BY SIZE INTO Wr-Path(Ws-Res-Cnt)
     MOVE Wr-Path(Ws-Res-Cnt) TO Wr-Demand(Ws-Res-Cnt).

*>----------------------------------------------------------------
 Page-The-Result.
     IF WU-FROM-LINE < 1
         MOVE 1 TO WU-FROM-LINE
     END-IF
     MOVE WU-FROM-LINE TO Ws-I
     PERFORM UNTIL Ws-I > Ws-Res-Cnt OR WU-WIN-CNT >= WU-WIN-MAX
         ADD 1 TO WU-WIN-CNT
         MOVE WU-WIN-CNT TO Ws-W
         MOVE Ws-I TO WU-L-SEQ(Ws-W)
         MOVE Wr-Level(Ws-I) TO WU-L-LEVEL(Ws-W)
         MOVE Wr-Parent(Ws-I) TO WU-L-PARENT(Ws-W)
         MOVE Wr-Parent-Rev(Ws-I) TO WU-L-PARENT-REV(Ws-W)
         MOVE Wr-Desc(Ws-I) TO WU-L-DESC(Ws-W)
         MOVE Wr-Bom-Ref(Ws-I) TO WU-L-BOM-REF(Ws-W)
         MOVE Wr-Bom-Rev(Ws-I) TO WU-L-BOM-REV(Ws-W)
         MOVE Wr-Status(Ws-I) TO WU-L-STATUS(Ws-W)
         MOVE Wr-Purpose(Ws-I) TO WU-L-PURPOSE(Ws-W)
         MOVE Wr-Variant(Ws-I) TO WU-L-VARIANT(Ws-W)
         MOVE Wr-Site(Ws-I) TO WU-L-SITE(Ws-W)
         MOVE Wr-Line-Num(Ws-I) TO WU-L-LINE-NUM(Ws-W)
         MOVE Wr-Usage(Ws-I) TO WU-L-USAGE(Ws-W)
         MOVE Wr-Uom(Ws-I) TO WU-L-UOM(Ws-W)
         MOVE Wr-Basis(Ws-I) TO WU-L-BASIS(Ws-W)
         MOVE Wr-Eff-From(Ws-I) TO WU-L-EFF-FROM(Ws-W)
         MOVE Wr-Eff-To(Ws-I) TO WU-L-EFF-TO(Ws-W)
         MOVE Wr-Certainty(Ws-I) TO WU-L-CERTAINTY(Ws-W)
         MOVE Wr-Path(Ws-I) TO WU-L-PATH(Ws-W)
         MOVE Wr-Demand(Ws-I) TO WU-L-DEMAND(Ws-W)
         ADD 1 TO Ws-I
     END-PERFORM
     MOVE Ws-I TO WU-FROM-LINE
     IF Ws-I <= Ws-Res-Cnt
         MOVE "Y" TO WU-MORE-LINES
     END-IF.

 Report-Sql-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE SQLIO-ERRMSG TO CA-MESSAGE.
