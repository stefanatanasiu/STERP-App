>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : BOMCOMP
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : Compare two revisions of a bill and say exactly
*>              what changed: components added, removed or
*>              changed, and in what respect - quantity, unit,
*>              usage basis, scrap allowance or effectivity.
*>
*>              BOM-025 asks for one distinction that is easy to
*>              get wrong: a line that only moved in the sequence
*>              is NOT a material change.  Engineers renumber
*>              bills all the time; a renumbering that is reported
*>              as a change costs a review meeting.
*> CALLED BY  : the revision comparison report and form, BOMBULK
*> INTERFACE  : CALL "BOMCOMP" USING WS-BOMCOMP-CA
*>              COPYBOOK BOMCOMPCA
*> CALLS      : BOMREAD, BOMSELCT, BOMCOST
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2015-09-02 AJS  CR2355 REVISION COMPARISON ADDED
*> 2026-10-05 STERP SEQUENCE ONLY CHANGES MARKED SEPARATELY
*>================================================================
*> REQ: BOM-003, BOM-024, BOM-025, BOM-032
 IDENTIFICATION DIVISION.
 PROGRAM-ID. BOMCOMP.
 AUTHOR. A J SPENCER.
 DATE-WRITTEN. 02/09/2015.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 01  Ws-Id-Row.
     05  Pi-Id                   PIC X(12).
 COPY BOMBILL REPLACING ==BOM-RD-CTL== BY ==Ws-Rd-A==
      ==BOM-HDR-REC== BY ==Ws-Hdr-A==
      ==BOM-CMP-TAB== BY ==Ws-Cmp-A==
      LEADING ==BR-== BY ==AR-==
      LEADING ==BH-== BY ==AH-==
      LEADING ==BC-== BY ==AC-==
      ==BOM-CMP-LINE== BY ==Ws-Line-A==.
 COPY BOMBILL REPLACING ==BOM-RD-CTL== BY ==Ws-Rd-B==
      ==BOM-HDR-REC== BY ==Ws-Hdr-B==
      ==BOM-CMP-TAB== BY ==Ws-Cmp-B==
      LEADING ==BR-== BY ==ZR-==
      LEADING ==BH-== BY ==ZH-==
      LEADING ==BC-== BY ==ZC-==
      ==BOM-CMP-LINE== BY ==Ws-Line-B==.
 COPY BOMCOSTCA REPLACING ==DFHCOMMAREA== BY ==Ws-Cost-Ca==
      LEADING ==CA-== BY ==KA-==
      LEADING ==BC-== BY ==KK-==.
 01  Ws-Work.
     05  Ws-I                    PIC S9(4) VALUE 0.
     05  Ws-J                    PIC S9(4) VALUE 0.
     05  Ws-W                    PIC S9(4) VALUE 0.
     05  Ws-Hit                  PIC S9(4) VALUE 0.
     05  Ws-Res-Cnt              PIC S9(4) VALUE 0.
     05  Ws-Detail               PIC X(100) VALUE SPACES.
     05  Ws-Changed              PIC X VALUE "N".
     05  Ws-Seq-Only             PIC X VALUE "N".
     05  Ws-Edit                 PIC -(12)9.9(6).
     05  Ws-Want-Rev             PIC X(4) VALUE SPACES.
     05  Ws-Found-Id             PIC S9(9) COMP-5 VALUE 0.
 01  Ws-Match-Flags.
     05  Ws-B-Used OCCURS 500 TIMES PIC X.
 01  Ws-Results.
     05  Ws-Res OCCURS 300 TIMES.
         10  Wr-Action           PIC X(10).
         10  Wr-Item             PIC X(40).
         10  Wr-Rev              PIC X(4).
         10  Wr-Desc             PIC X(40).
         10  Wr-From-Line        PIC S9(4).
         10  Wr-To-Line          PIC S9(4).
         10  Wr-From-Qty         PIC S9(13)V9(6).
         10  Wr-To-Qty           PIC S9(13)V9(6).
         10  Wr-From-Uom         PIC X(10).
         10  Wr-To-Uom           PIC X(10).
         10  Wr-From-Basis       PIC X(12).
         10  Wr-To-Basis         PIC X(12).
         10  Wr-From-Scrap       PIC S9(5)V9(6).
         10  Wr-To-Scrap         PIC S9(5)V9(6).
         10  Wr-Detail           PIC X(100).
 LINKAGE SECTION.
 COPY BOMCOMPCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "BOMCOMP" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE "BOM-025" TO CA-REQ-REF
     IF BP-WIN-MAX < 1 OR BP-WIN-MAX > 25
         MOVE 25 TO BP-WIN-MAX
     END-IF
     MOVE 0 TO BP-WIN-CNT
     MOVE "N" TO BP-MORE-LINES
     IF BP-EFF-DATE = 0
         MOVE CA-BUSINESS-DATE TO BP-EFF-DATE
     END-IF
     EVALUATE CA-FUNCTION
         WHEN "COMPARE"  PERFORM Compare-Two-Bills
                         PERFORM Page-The-Differences
         WHEN "BYREF"    PERFORM Resolve-By-Reference
                         IF CA-RETURN-CODE <= 4
                             PERFORM Compare-Two-Bills
                             PERFORM Page-The-Differences
                         END-IF
         WHEN "IMPACT"   PERFORM Compare-Two-Bills
                         PERFORM Cost-Both-Revisions
                         PERFORM Page-The-Differences
         WHEN "GETLINE"  PERFORM Page-The-Differences
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "BOM1001" TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "BOMCOMP does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>----------------------------------------------------------------
*> BYREF names the two revisions of one bill rather than their
*> internal identities, which is how an engineer thinks of them.
 Resolve-By-Reference.
     MOVE BP-FROM-REV TO Ws-Want-Rev
     PERFORM Find-One-Revision
     MOVE Ws-Found-Id TO BP-FROM-BOM-ID
     IF Ws-Found-Id = 0
         MOVE 16 TO CA-RETURN-CODE
         MOVE "BOM1004" TO CA-REASON-CODE
         MOVE SPACES TO CA-MESSAGE
         STRING "Revision " FUNCTION TRIM(BP-FROM-REV)
                " of " FUNCTION TRIM(BP-BOM-REF) " does not exist"
             DELIMITED BY SIZE INTO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE BP-TO-REV TO Ws-Want-Rev
     PERFORM Find-One-Revision
     MOVE Ws-Found-Id TO BP-TO-BOM-ID
     IF Ws-Found-Id = 0
         MOVE 16 TO CA-RETURN-CODE
         MOVE "BOM1005" TO CA-REASON-CODE
         MOVE SPACES TO CA-MESSAGE
         STRING "Revision " FUNCTION TRIM(BP-TO-REV)
                " of " FUNCTION TRIM(BP-BOM-REF) " does not exist"
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF.

 Find-One-Revision.
     MOVE 0 TO Ws-Found-Id
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT h.BomHeaderId FROM bom.BomHeader h "
            "WHERE h.CompanyCode = ? AND h.BomRef = ? "
            "AND h.BomRevision = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE 3 TO SQLIO-PARM-CNT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
     MOVE "N" TO SQLIO-PARM-NULL(1)
     MOVE BP-BOM-REF TO SQLIO-PARM-VAL(2)
     MOVE "N" TO SQLIO-PARM-NULL(2)
     MOVE Ws-Want-Rev TO SQLIO-PARM-VAL(3)
     MOVE "N" TO SQLIO-PARM-NULL(3)
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 12 TO SQLIO-COL-WIDTH(1)
     MOVE 1 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA Ws-Id-Row
     IF SQLIO-OK
         COMPUTE Ws-Found-Id = FUNCTION NUMVAL(Pi-Id)
     END-IF.

 Compare-Two-Bills.
     PERFORM Read-Bill-A
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Read-Bill-B
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     MOVE 0 TO Ws-Res-Cnt
     MOVE 0 TO BP-ADDED-CNT BP-REMOVED-CNT BP-CHANGED-CNT
     MOVE 0 TO BP-SEQONLY-CNT BP-SAME-CNT
     MOVE "N" TO BP-MATERIAL-CHANGE
     MOVE "COMPLETE" TO BP-COMPLETENESS
     PERFORM VARYING Ws-J FROM 1 BY 1 UNTIL Ws-J > 500
         MOVE "N" TO Ws-B-Used(Ws-J)
     END-PERFORM
     PERFORM Compare-Headers
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > AR-LINE-COUNT
         PERFORM Match-One-From-Line
     END-PERFORM
     PERFORM VARYING Ws-J FROM 1 BY 1 UNTIL Ws-J > ZR-LINE-COUNT
         IF Ws-B-Used(Ws-J) = "N"
             PERFORM Record-An-Addition
         END-IF
     END-PERFORM
     PERFORM Publish-Verdict.

 Read-Bill-A.
     INITIALIZE Ws-Rd-A
     MOVE "BILL" TO AR-FUNC
     MOVE CA-COMPANY TO AR-COMPANY
     MOVE CA-SITE TO AR-SITE
     MOVE BP-FROM-BOM-ID TO AR-BOM-ID
     MOVE BP-EFF-DATE TO AR-EFF-DATE
     MOVE 500 TO AR-MAX-LINES
     CALL "BOMREAD" USING Ws-Rd-A Ws-Hdr-A Ws-Cmp-A
     IF AR-STATUS NOT = "OK"
         MOVE 16 TO CA-RETURN-CODE
         MOVE "BOM1002" TO CA-REASON-CODE
         MOVE AR-MESSAGE TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE AH-BOM-REF TO BP-BOM-REF
     MOVE AH-BOM-REV TO BP-FROM-REV
     MOVE AH-PARENT-ITEM TO BP-PARENT-ITEM.

 Read-Bill-B.
     INITIALIZE Ws-Rd-B
     MOVE "BILL" TO ZR-FUNC
     MOVE CA-COMPANY TO ZR-COMPANY
     MOVE CA-SITE TO ZR-SITE
     MOVE BP-TO-BOM-ID TO ZR-BOM-ID
     MOVE BP-EFF-DATE TO ZR-EFF-DATE
     MOVE 500 TO ZR-MAX-LINES
     CALL "BOMREAD" USING Ws-Rd-B Ws-Hdr-B Ws-Cmp-B
     IF ZR-STATUS NOT = "OK"
         MOVE 16 TO CA-RETURN-CODE
         MOVE "BOM1003" TO CA-REASON-CODE
         MOVE ZR-MESSAGE TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     MOVE ZH-BOM-REV TO BP-TO-REV.

*>----------------------------------------------------------------
 Compare-Headers.
     MOVE SPACES TO BP-HEADER-DIFF
     IF AH-NOMINAL-BASIS NOT = ZH-NOMINAL-BASIS
         MOVE ZH-NOMINAL-BASIS TO Ws-Edit
         STRING "Basis now " FUNCTION TRIM(Ws-Edit) "; "
             DELIMITED BY SIZE INTO BP-HEADER-DIFF
     END-IF
     IF AH-YIELD NOT = ZH-YIELD
         MOVE ZH-YIELD TO Ws-Edit
         STRING FUNCTION TRIM(BP-HEADER-DIFF) "yield now "
                FUNCTION TRIM(Ws-Edit) "; "
             DELIMITED BY SIZE INTO BP-HEADER-DIFF
     END-IF
     IF AH-QUANTUM NOT = ZH-QUANTUM
         MOVE ZH-QUANTUM TO Ws-Edit
         STRING FUNCTION TRIM(BP-HEADER-DIFF) "quantum now "
                FUNCTION TRIM(Ws-Edit) "; "
             DELIMITED BY SIZE INTO BP-HEADER-DIFF
     END-IF
     IF AH-BATCH-SIZE NOT = ZH-BATCH-SIZE
         MOVE ZH-BATCH-SIZE TO Ws-Edit
         STRING FUNCTION TRIM(BP-HEADER-DIFF) "batch size now "
                FUNCTION TRIM(Ws-Edit)
             DELIMITED BY SIZE INTO BP-HEADER-DIFF
     END-IF
     IF BP-HEADER-DIFF NOT = SPACES
         MOVE "Y" TO BP-MATERIAL-CHANGE
     END-IF.

*>----------------------------------------------------------------
*> Match on component identity and alternate group, taking the
*> first unused occurrence.  That keeps repeated lines of the same
*> item distinguishable instead of collapsing them (BOM-002).
 Match-One-From-Line.
     MOVE 0 TO Ws-Hit
     PERFORM VARYING Ws-J FROM 1 BY 1
         UNTIL Ws-J > ZR-LINE-COUNT OR Ws-Hit NOT = 0
         IF Ws-B-Used(Ws-J) = "N"
            AND ZC-ITEM(Ws-J) = AC-ITEM(Ws-I)
            AND ZC-ALT-GROUP(Ws-J) = AC-ALT-GROUP(Ws-I)
             MOVE Ws-J TO Ws-Hit
         END-IF
     END-PERFORM
     IF Ws-Hit = 0
         PERFORM Record-A-Removal
         EXIT PARAGRAPH
     END-IF
     MOVE "Y" TO Ws-B-Used(Ws-Hit)
     PERFORM Compare-Matched-Lines.

 Compare-Matched-Lines.
     MOVE "N" TO Ws-Changed
     MOVE "N" TO Ws-Seq-Only
     MOVE SPACES TO Ws-Detail
     IF AC-USAGE-QTY(Ws-I) NOT = ZC-USAGE-QTY(Ws-Hit)
         MOVE "Y" TO Ws-Changed
         MOVE ZC-USAGE-QTY(Ws-Hit) TO Ws-Edit
         STRING "Quantity now " FUNCTION TRIM(Ws-Edit) "; "
             DELIMITED BY SIZE INTO Ws-Detail
     END-IF
     IF AC-UOM(Ws-I) NOT = ZC-UOM(Ws-Hit)
         MOVE "Y" TO Ws-Changed
         STRING FUNCTION TRIM(Ws-Detail) "unit now "
                FUNCTION TRIM(ZC-UOM(Ws-Hit)) "; "
             DELIMITED BY SIZE INTO Ws-Detail
     END-IF
     IF AC-USAGE-BASIS(Ws-I) NOT = ZC-USAGE-BASIS(Ws-Hit)
         MOVE "Y" TO Ws-Changed
         STRING FUNCTION TRIM(Ws-Detail) "basis now "
                FUNCTION TRIM(ZC-USAGE-BASIS(Ws-Hit)) "; "
             DELIMITED BY SIZE INTO Ws-Detail
     END-IF
     IF AC-SCRAP-PCT(Ws-I) NOT = ZC-SCRAP-PCT(Ws-Hit)
         MOVE "Y" TO Ws-Changed
         MOVE ZC-SCRAP-PCT(Ws-Hit) TO Ws-Edit
         STRING FUNCTION TRIM(Ws-Detail) "allowance now "
                FUNCTION TRIM(Ws-Edit) "; "
             DELIMITED BY SIZE INTO Ws-Detail
     END-IF
     IF AC-EFF-FROM(Ws-I) NOT = ZC-EFF-FROM(Ws-Hit)
        OR AC-EFF-TO(Ws-I) NOT = ZC-EFF-TO(Ws-Hit)
         MOVE "Y" TO Ws-Changed
         STRING FUNCTION TRIM(Ws-Detail) "effectivity changed; "
             DELIMITED BY SIZE INTO Ws-Detail
     END-IF
     IF AC-SUPPLY(Ws-I) NOT = ZC-SUPPLY(Ws-Hit)
         MOVE "Y" TO Ws-Changed
         STRING FUNCTION TRIM(Ws-Detail) "supply now "
                FUNCTION TRIM(ZC-SUPPLY(Ws-Hit))
             DELIMITED BY SIZE INTO Ws-Detail
     END-IF
     IF Ws-Changed = "N"
        AND AC-LINE-NUM(Ws-I) NOT = ZC-LINE-NUM(Ws-Hit)
         MOVE "Y" TO Ws-Seq-Only
         MOVE "Sequence only - no material change" TO Ws-Detail
     END-IF
     EVALUATE TRUE
         WHEN Ws-Changed = "Y"
             ADD 1 TO BP-CHANGED-CNT
             MOVE "Y" TO BP-MATERIAL-CHANGE
             PERFORM Record-A-Difference
         WHEN Ws-Seq-Only = "Y"
             ADD 1 TO BP-SEQONLY-CNT
             PERFORM Record-A-Difference
         WHEN OTHER
             ADD 1 TO BP-SAME-CNT
     END-EVALUATE.

 Record-A-Difference.
     IF Ws-Res-Cnt >= 300
         MOVE "INCOMPLETE" TO BP-COMPLETENESS
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Res-Cnt
     IF Ws-Changed = "Y"
         MOVE "CHANGED" TO Wr-Action(Ws-Res-Cnt)
     ELSE
         MOVE "SEQUENCE" TO Wr-Action(Ws-Res-Cnt)
     END-IF
     MOVE AC-ITEM(Ws-I) TO Wr-Item(Ws-Res-Cnt)
     MOVE AC-REV(Ws-I) TO Wr-Rev(Ws-Res-Cnt)
     MOVE AC-DESC(Ws-I) TO Wr-Desc(Ws-Res-Cnt)
     MOVE AC-LINE-NUM(Ws-I) TO Wr-From-Line(Ws-Res-Cnt)
     MOVE ZC-LINE-NUM(Ws-Hit) TO Wr-To-Line(Ws-Res-Cnt)
     MOVE AC-USAGE-QTY(Ws-I) TO Wr-From-Qty(Ws-Res-Cnt)
     MOVE ZC-USAGE-QTY(Ws-Hit) TO Wr-To-Qty(Ws-Res-Cnt)
     MOVE AC-UOM(Ws-I) TO Wr-From-Uom(Ws-Res-Cnt)
     MOVE ZC-UOM(Ws-Hit) TO Wr-To-Uom(Ws-Res-Cnt)
     MOVE AC-USAGE-BASIS(Ws-I) TO Wr-From-Basis(Ws-Res-Cnt)
     MOVE ZC-USAGE-BASIS(Ws-Hit) TO Wr-To-Basis(Ws-Res-Cnt)
     MOVE AC-SCRAP-PCT(Ws-I) TO Wr-From-Scrap(Ws-Res-Cnt)
     MOVE ZC-SCRAP-PCT(Ws-Hit) TO Wr-To-Scrap(Ws-Res-Cnt)
     MOVE Ws-Detail TO Wr-Detail(Ws-Res-Cnt).

 Record-A-Removal.
     ADD 1 TO BP-REMOVED-CNT
     MOVE "Y" TO BP-MATERIAL-CHANGE
     IF Ws-Res-Cnt >= 300
         MOVE "INCOMPLETE" TO BP-COMPLETENESS
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Res-Cnt
     MOVE "REMOVED" TO Wr-Action(Ws-Res-Cnt)
     MOVE AC-ITEM(Ws-I) TO Wr-Item(Ws-Res-Cnt)
     MOVE AC-REV(Ws-I) TO Wr-Rev(Ws-Res-Cnt)
     MOVE AC-DESC(Ws-I) TO Wr-Desc(Ws-Res-Cnt)
     MOVE AC-LINE-NUM(Ws-I) TO Wr-From-Line(Ws-Res-Cnt)
     MOVE 0 TO Wr-To-Line(Ws-Res-Cnt)
     MOVE AC-USAGE-QTY(Ws-I) TO Wr-From-Qty(Ws-Res-Cnt)
     MOVE 0 TO Wr-To-Qty(Ws-Res-Cnt)
     MOVE AC-UOM(Ws-I) TO Wr-From-Uom(Ws-Res-Cnt)
     MOVE SPACES TO Wr-To-Uom(Ws-Res-Cnt)
     MOVE AC-USAGE-BASIS(Ws-I) TO Wr-From-Basis(Ws-Res-Cnt)
     MOVE SPACES TO Wr-To-Basis(Ws-Res-Cnt)
     MOVE AC-SCRAP-PCT(Ws-I) TO Wr-From-Scrap(Ws-Res-Cnt)
     MOVE 0 TO Wr-To-Scrap(Ws-Res-Cnt)
     MOVE "Component no longer used on this revision"
         TO Wr-Detail(Ws-Res-Cnt).

 Record-An-Addition.
     ADD 1 TO BP-ADDED-CNT
     MOVE "Y" TO BP-MATERIAL-CHANGE
     IF Ws-Res-Cnt >= 300
         MOVE "INCOMPLETE" TO BP-COMPLETENESS
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Res-Cnt
     MOVE "ADDED" TO Wr-Action(Ws-Res-Cnt)
     MOVE ZC-ITEM(Ws-J) TO Wr-Item(Ws-Res-Cnt)
     MOVE ZC-REV(Ws-J) TO Wr-Rev(Ws-Res-Cnt)
     MOVE ZC-DESC(Ws-J) TO Wr-Desc(Ws-Res-Cnt)
     MOVE 0 TO Wr-From-Line(Ws-Res-Cnt)
     MOVE ZC-LINE-NUM(Ws-J) TO Wr-To-Line(Ws-Res-Cnt)
     MOVE 0 TO Wr-From-Qty(Ws-Res-Cnt)
     MOVE ZC-USAGE-QTY(Ws-J) TO Wr-To-Qty(Ws-Res-Cnt)
     MOVE SPACES TO Wr-From-Uom(Ws-Res-Cnt)
     MOVE ZC-UOM(Ws-J) TO Wr-To-Uom(Ws-Res-Cnt)
     MOVE SPACES TO Wr-From-Basis(Ws-Res-Cnt)
     MOVE ZC-USAGE-BASIS(Ws-J) TO Wr-To-Basis(Ws-Res-Cnt)
     MOVE 0 TO Wr-From-Scrap(Ws-Res-Cnt)
     MOVE ZC-SCRAP-PCT(Ws-J) TO Wr-To-Scrap(Ws-Res-Cnt)
     MOVE "New component occurrence on this revision"
         TO Wr-Detail(Ws-Res-Cnt).

 Publish-Verdict.
     MOVE 0 TO CA-RETURN-CODE
     MOVE SPACES TO CA-MESSAGE
     IF BP-MATERIAL-CHANGE = "Y"
         STRING "Material change: "
             DELIMITED BY SIZE INTO CA-MESSAGE
     ELSE
         STRING "No material change; "
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF
     MOVE BP-ADDED-CNT TO Ws-Edit
     STRING FUNCTION TRIM(CA-MESSAGE) FUNCTION TRIM(Ws-Edit)
            " added, "
         DELIMITED BY SIZE INTO CA-MESSAGE
     MOVE BP-REMOVED-CNT TO Ws-Edit
     STRING FUNCTION TRIM(CA-MESSAGE) FUNCTION TRIM(Ws-Edit)
            " removed, "
         DELIMITED BY SIZE INTO CA-MESSAGE
     MOVE BP-CHANGED-CNT TO Ws-Edit
     STRING FUNCTION TRIM(CA-MESSAGE) FUNCTION TRIM(Ws-Edit)
            " changed, "
         DELIMITED BY SIZE INTO CA-MESSAGE
     MOVE BP-SEQONLY-CNT TO Ws-Edit
     STRING FUNCTION TRIM(CA-MESSAGE) FUNCTION TRIM(Ws-Edit)
            " resequenced"
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>----------------------------------------------------------------
*> The calculated impact: both revisions costed for the same
*> demand, so a reviewer sees the money as well as the structure.
 Cost-Both-Revisions.
     MOVE 0 TO BP-FROM-COST BP-TO-COST BP-COST-DELTA
     IF BP-REQ-QTY <= 0
         EXIT PARAGRAPH
     END-IF
     MOVE AH-PARENT-ITEM TO Ws-Detail
     PERFORM Cost-One-Revision
     MOVE KK-TOTAL-COST TO BP-FROM-COST
     MOVE ZH-PARENT-ITEM TO Ws-Detail
     PERFORM Cost-One-Revision
     MOVE KK-TOTAL-COST TO BP-TO-COST
     COMPUTE BP-COST-DELTA = BP-TO-COST - BP-FROM-COST.

 Cost-One-Revision.
     INITIALIZE Ws-Cost-Ca
     MOVE "COST" TO KA-FUNCTION
     MOVE CA-COMPANY TO KA-COMPANY
     MOVE CA-SITE TO KA-SITE
     MOVE CA-USER TO KA-USER
     MOVE BP-EFF-DATE TO KA-BUSINESS-DATE
     MOVE Ws-Detail TO KK-PARENT-ITEM
     MOVE "STANDARD" TO KK-PURPOSE
     MOVE BP-REQ-QTY TO KK-REQ-QTY
     MOVE BP-EFF-DATE TO KK-EFF-DATE
     MOVE 25 TO KK-MAX-DEPTH
     MOVE "BOMDEMO" TO KK-COST-SET
     MOVE "GBP" TO KK-CURRENCY
     CALL "BOMCOST" USING Ws-Cost-Ca.

*>----------------------------------------------------------------
 Page-The-Differences.
     IF BP-FROM-LINE < 1
         MOVE 1 TO BP-FROM-LINE
     END-IF
     MOVE BP-FROM-LINE TO Ws-I
     PERFORM UNTIL Ws-I > Ws-Res-Cnt OR BP-WIN-CNT >= BP-WIN-MAX
         ADD 1 TO BP-WIN-CNT
         MOVE BP-WIN-CNT TO Ws-W
         MOVE Ws-I TO BP-L-SEQ(Ws-W)
         MOVE Wr-Action(Ws-I) TO BP-L-ACTION(Ws-W)
         MOVE Wr-Item(Ws-I) TO BP-L-ITEM(Ws-W)
         MOVE Wr-Rev(Ws-I) TO BP-L-REV(Ws-W)
         MOVE Wr-Desc(Ws-I) TO BP-L-DESC(Ws-W)
         MOVE Wr-From-Line(Ws-I) TO BP-L-FROM-LINE(Ws-W)
         MOVE Wr-To-Line(Ws-I) TO BP-L-TO-LINE(Ws-W)
         MOVE Wr-From-Qty(Ws-I) TO BP-L-FROM-QTY(Ws-W)
         MOVE Wr-To-Qty(Ws-I) TO BP-L-TO-QTY(Ws-W)
         MOVE Wr-From-Uom(Ws-I) TO BP-L-FROM-UOM(Ws-W)
         MOVE Wr-To-Uom(Ws-I) TO BP-L-TO-UOM(Ws-W)
         MOVE Wr-From-Basis(Ws-I) TO BP-L-FROM-BASIS(Ws-W)
         MOVE Wr-To-Basis(Ws-I) TO BP-L-TO-BASIS(Ws-W)
         MOVE Wr-From-Scrap(Ws-I) TO BP-L-FROM-SCRAP(Ws-W)
         MOVE Wr-To-Scrap(Ws-I) TO BP-L-TO-SCRAP(Ws-W)
         COMPUTE BP-L-QTY-DELTA(Ws-W) =
             Wr-To-Qty(Ws-I) - Wr-From-Qty(Ws-I)
         MOVE Wr-Detail(Ws-I) TO BP-L-DETAIL(Ws-W)
         ADD 1 TO Ws-I
     END-PERFORM
     MOVE Ws-I TO BP-FROM-LINE
     IF Ws-I <= Ws-Res-Cnt
         MOVE "Y" TO BP-MORE-LINES
     END-IF.
