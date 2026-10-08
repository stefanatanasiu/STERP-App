>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : TBOMCOST
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL - UNIT TESTS
*> PURPOSE    : Specification B1's cost roll-up, reproduced with
*>              the real engine.  Leaf unit costs are plate 4.00,
*>              pin 0.25, resin 6.00, cleaner 8.00 and label 0.10.
*>                Plate   11 x 4.00  = 44.00
*>                Pins    70 x 0.25  = 17.50
*>                Resin  0.6 x 6.00  =  3.60
*>                Cleaner 0.5 x 8.00 =  4.00
*>                Labels  10 x 0.10  =  1.00
*>                Material total     = 70.10
*>                Brace conversion 25 x 0.60 = 15.00
*>                Frame setup, one run       = 10.00
*>                Frame conversion 10 x 0.50 =  5.00
*>                Total for ten good frames  = 100.10
*>                Per good frame             =  10.01
*>              The brace branch is worth 75.10 and is included
*>              once - the alternative presentation must not add
*>              it again on top of the expanded costs.
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2026-10-05 STERP B1 COST ROLL-UP AND AC-093
*>================================================================
*> REQ: BOM-021, BOM-026, BOM-027, BOM-032
*> AC: AC-092, AC-093
 IDENTIFICATION DIVISION.
 PROGRAM-ID. TBOMCOST.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY TESTKITW.
 COPY BOMCOSTCA REPLACING ==DFHCOMMAREA== BY ==Ws-Cost==
      LEADING ==CA-== BY ==KA-==
      LEADING ==BC-== BY ==KK-==.
 01  Ws-Flags.
     05  Ws-Db-Ok                PIC X VALUE "Y".
     05  Ws-I                    PIC S9(4) VALUE 0.
     05  Ws-Brace-Seq            PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Other                PIC S9(13)V9(2) VALUE 0.
 PROCEDURE DIVISION.
 Main-Entry.
     MOVE "TBOMCOST" TO TK-SUITE
     PERFORM TK-BEGIN
     PERFORM Cost-B1-Ten-Frames
     IF Ws-Db-Ok = "N"
         MOVE "BOM demonstration data or database not available"
             TO TK-DETAIL
         MOVE "B1 cost roll-up" TO TK-NAME
         PERFORM TK-SKIP
         PERFORM TK-END
         STOP RUN
     END-IF
     PERFORM Check-B1-Totals
     PERFORM Check-Brace-Branch
     PERFORM Check-Item-Standard-Fallback
     PERFORM Check-Missing-Resource-Rate
     PERFORM TK-END
     STOP RUN.

 Set-Up-Cost-Request.
     INITIALIZE Ws-Cost
     MOVE "COST" TO KA-FUNCTION
     MOVE "STC" TO KA-COMPANY
     MOVE "RDG" TO KA-SITE
     MOVE "COSTACC" TO KA-USER
     MOVE 20261001 TO KA-BUSINESS-DATE
     MOVE "DEMO-FRAME" TO KK-PARENT-ITEM
     MOVE "STANDARD" TO KK-PURPOSE
     MOVE 10 TO KK-REQ-QTY
     MOVE 20261001 TO KK-EFF-DATE
     MOVE 25 TO KK-MAX-DEPTH
     MOVE "BOMDEMO" TO KK-COST-SET
     MOVE "GBP" TO KK-CURRENCY
     MOVE "N" TO KK-NETTED.

 Cost-B1-Ten-Frames.
     PERFORM Set-Up-Cost-Request
     CALL "BOMCOST" USING Ws-Cost
     IF KA-RETURN-CODE = 12 OR KK-OCC-COUNT = 0
         MOVE "N" TO Ws-Db-Ok
     END-IF.

*>----------------------------------------------------------------
 Check-B1-Totals.
     MOVE "B1 material total is 70.10" TO TK-NAME
     MOVE 70.10 TO TK-EXPECTED-N
     MOVE KK-MATERIAL-COST TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "B1 conversion plus setup is 30.00" TO TK-NAME
     MOVE 30.00 TO TK-EXPECTED-N
     MOVE KK-CONVERSION-COST TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "B1 frame setup for one run is 10.00" TO TK-NAME
     MOVE 10.00 TO TK-EXPECTED-N
     MOVE KK-SETUP-COST TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "B1 total for ten good frames is 100.10" TO TK-NAME
     MOVE 100.10 TO TK-EXPECTED-N
     MOVE KK-TOTAL-COST TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "B1 cost per good frame is 10.01" TO TK-NAME
     MOVE 10.01 TO TK-EXPECTED-N
     MOVE KK-UNIT-COST TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "B1 unit denominator is the ten good frames" TO TK-NAME
     MOVE 10 TO TK-EXPECTED-N
     MOVE KK-UNIT-DENOM TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "B1 cost roll-up is complete" TO TK-NAME
     MOVE "COMPLETE" TO TK-EXPECTED-X
     MOVE KK-COMPLETENESS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "B1 no rate is missing" TO TK-NAME
     MOVE 0 TO TK-EXPECTED-N
     MOVE KK-MISSING-RATES TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

*>----------------------------------------------------------------
*> The brace branch rolls up to 75.10.  The alternative
*> presentation is 20 x 3.755 for the braces plus 25.00 of other
*> frame cost, which still totals 100.10 - the branch value is
*> included once, never added twice.
 Check-Brace-Branch.
     PERFORM Find-Brace-Occurrence
     MOVE "BRANCH" TO KA-FUNCTION
     MOVE Ws-Brace-Seq TO KK-BRANCH-SEQ
     CALL "BOMCOST" USING Ws-Cost
     MOVE "B1 brace branch rolls up to 75.10" TO TK-NAME
     MOVE 75.10 TO TK-EXPECTED-N
     MOVE KK-BRANCH-COST TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     MOVE "B1 brace branch unit value is 3.755" TO TK-NAME
     MOVE 3.755 TO TK-EXPECTED-N
     MOVE KK-BRANCH-UNIT TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     COMPUTE Ws-Other = KK-TOTAL-COST - KK-BRANCH-COST
     MOVE "B1 the other frame costs are 25.00" TO TK-NAME
     MOVE 25.00 TO TK-EXPECTED-N
     MOVE Ws-Other TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     COMPUTE Ws-Other = KK-BRANCH-COST + Ws-Other
     MOVE "B1 both presentations total 100.10" TO TK-NAME
     MOVE 100.10 TO TK-EXPECTED-N
     MOVE Ws-Other TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM.

 Find-Brace-Occurrence.
     MOVE 0 TO Ws-Brace-Seq
     MOVE "GETLINE" TO KA-FUNCTION
     MOVE 1 TO KK-FROM-LINE
     MOVE 25 TO KK-WIN-MAX
     CALL "BOMCOST" USING Ws-Cost
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > KK-WIN-CNT
         IF KK-L-ITEM(Ws-I) = "DEMO-BRACE"
             MOVE KK-L-SEQ(Ws-I) TO Ws-Brace-Seq
         END-IF
     END-PERFORM.

*>----------------------------------------------------------------
*> With no cost set named the engine falls back to the item
*> standard cost column and reaches the same answer.
 Check-Item-Standard-Fallback.
     PERFORM Set-Up-Cost-Request
     MOVE SPACES TO KK-COST-SET
     CALL "BOMCOST" USING Ws-Cost
     MOVE "Item standard cost gives the same 100.10" TO TK-NAME
     MOVE 100.10 TO TK-EXPECTED-N
     MOVE KK-TOTAL-COST TO TK-ACTUAL-N
     PERFORM TK-ASSERT-NUM
     PERFORM Find-Pin-Source
     MOVE "The fallback route is named on the line" TO TK-NAME
     MOVE "ITEMSTD" TO TK-EXPECTED-X
     PERFORM TK-ASSERT-STR.

 Find-Pin-Source.
     MOVE SPACES TO TK-ACTUAL-X
     MOVE "GETLINE" TO KA-FUNCTION
     MOVE 1 TO KK-FROM-LINE
     MOVE 25 TO KK-WIN-MAX
     CALL "BOMCOST" USING Ws-Cost
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > KK-WIN-CNT
         IF KK-L-ITEM(Ws-I) = "DEMO-PIN"
             MOVE KK-L-SOURCE(Ws-I) TO TK-ACTUAL-X
         END-IF
     END-PERFORM.

*>----------------------------------------------------------------
*> A cost-only resource with no rate cannot produce a falsely
*> complete cost (BOM-027, AC-092).
 Check-Missing-Resource-Rate.
     PERFORM Set-Up-Cost-Request
     MOVE "DEMO-OPT-TOP" TO KK-PARENT-ITEM
     MOVE 1 TO KK-REQ-QTY
     CALL "BOMCOST" USING Ws-Cost
     MOVE "A missing resource rate blocks a complete cost"
         TO TK-NAME
     MOVE "INCOMPLETE" TO TK-EXPECTED-X
     MOVE KK-COMPLETENESS TO TK-ACTUAL-X
     PERFORM TK-ASSERT-STR
     MOVE "The missing rate is counted" TO TK-NAME
     IF KK-MISSING-RATES > 0
         PERFORM TK-PASS
     ELSE
         PERFORM TK-FAIL
     END-IF
     MOVE "A by-product recovery is a positive credit" TO TK-NAME
     IF KK-RECOVERY-CREDIT > 0
         PERFORM TK-PASS
     ELSE
         PERFORM TK-FAIL
     END-IF.

 COPY TESTKITP.
