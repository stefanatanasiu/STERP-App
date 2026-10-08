>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : KITTRIAL
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : KIT MAINTENANCE AND EXECUTION
*> PURPOSE    : Trial kitting - material feasibility, shortages
*>              and the limiting requirement for a stated output
*>              quantity and date, WITHOUT ANY COMMITMENT
*>              (KIT-006).  Nothing here reserves, moves, orders
*>              or posts: the only database write is the optional
*>              SAVE of the answer to kit.TrialKit as evidence.
*>
*>              The maximum complete output is found by probing
*>              the whole calculation again at candidate output
*>              quantities, so yield, fixed-per-run material,
*>              batch steps, rounding and supply netting all take
*>              effect.  It is deliberately NOT the minimum of
*>              component ratios, which would over-promise on a
*>              bill with fixed or batched usage.
*> CALLED BY  : frmTrialKitting, MULTITRL, KITCREAT (what-if)
*> INTERFACE  : CALL "KITTRIAL" USING WS-KITTRIAL-CA
*>              COPYBOOK KITTRIALCA
*> CALLS      : BOMEXPL, AVAILQ, SQLIO (SAVE only)
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2015-01-19 PMK  CR2281 TRIAL KITTING
*> 2015-08-04 PMK  CR2374 TRIAL MUST NOT RESERVE
*> 2016-07-11 PMK  CR2492 MAXBUILD RE-EXPLODES INSTEAD OF TAKING
*>                 THE MINIMUM COMPONENT RATIO
*> 2026-10-06 STERP KIT-006
*>================================================================
*> REQ: KIT-006
 IDENTIFICATION DIVISION.
 PROGRAM-ID. KITTRIAL.
 AUTHOR. P M KEANE.
 DATE-WRITTEN. 19/01/2015.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY SQLIOCA.
 COPY BOMEXPLCA REPLACING ==DFHCOMMAREA== BY ==Ws-Expl-Ca==
      LEADING ==CA-== BY ==EA-==.
 COPY AVAILQCA REPLACING ==DFHCOMMAREA== BY ==Ws-Avail-Ca==
      LEADING ==CA-== BY ==VA-==.
*>   The retained answer.  It survives until the next TRIAL so a
*>   client may page it (GETLINE) or file it (SAVE) without the
*>   world being read a second time - a repeated read of a trial
*>   must never have a live effect.
 01  Ws-Result.
     05  Ws-Res-Cnt              PIC S9(4) VALUE 0.
     05  Ws-Res-Max              PIC S9(4) VALUE 150.
     05  Ws-Res OCCURS 150 TIMES.
         10  Ws-R-Line           PIC S9(9) COMP-5.
         10  Ws-R-Path           PIC X(20).
         10  Ws-R-Depth          PIC S9(4).
         10  Ws-R-Item           PIC X(40).
         10  Ws-R-Rev            PIC X(4).
         10  Ws-R-Uom            PIC X(10).
         10  Ws-R-Gross          PIC S9(13)V9(6).
         10  Ws-R-Net-Req        PIC S9(13)V9(6).
         10  Ws-R-Free           PIC S9(13)V9(6).
         10  Ws-R-Own-Resv       PIC S9(13)V9(6).
         10  Ws-R-Other-Resv     PIC S9(13)V9(6).
         10  Ws-R-Inbound        PIC S9(13)V9(6).
         10  Ws-R-Inb-Date       PIC 9(8).
         10  Ws-R-Allocated      PIC S9(13)V9(6).
         10  Ws-R-Shortage       PIC S9(13)V9(6).
         10  Ws-R-Short-Date     PIC 9(8).
         10  Ws-R-Limiting       PIC X.
         10  Ws-R-Action         PIC X(40).
         10  Ws-R-Warning        PIC X(60).
*>   Availability is read once per item per trial.  A component
*>   that appears on two paths of the same bill must not be
*>   promised the same free stock twice, so Ws-A-Remain is drawn
*>   down as the lines are satisfied (KIT-007 in miniature).
 01  Ws-Avail-Cache.
     05  Ws-Av-Cnt               PIC S9(4) VALUE 0.
     05  Ws-Av-Max               PIC S9(4) VALUE 200.
     05  Ws-Av OCCURS 200 TIMES.
         10  Ws-A-Item           PIC X(40).
         10  Ws-A-Uom            PIC X(10).
         10  Ws-A-Free           PIC S9(13)V9(6).
         10  Ws-A-Own-Resv       PIC S9(13)V9(6).
         10  Ws-A-Other-Resv     PIC S9(13)V9(6).
         10  Ws-A-Inbound        PIC S9(13)V9(6).
         10  Ws-A-Inb-Date       PIC 9(8).
         10  Ws-A-Remain         PIC S9(13)V9(6).
         10  Ws-A-Inb-Remain     PIC S9(13)V9(6).
 01  Ws-Work.
     05  Ws-I                    PIC S9(4) VALUE 0.
     05  Ws-J                    PIC S9(4) VALUE 0.
     05  Ws-Hit                  PIC S9(4) VALUE 0.
     05  Ws-Probe-Qty            PIC S9(13)V9(6) VALUE 0.
     05  Ws-Probe-Feasible       PIC X VALUE "N".
     05  Ws-Probe-Count          PIC S9(4) VALUE 0.
     05  Ws-Lo                   PIC S9(13)V9(6) VALUE 0.
     05  Ws-Hi                   PIC S9(13)V9(6) VALUE 0.
     05  Ws-Mid                  PIC S9(13)V9(6) VALUE 0.
     05  Ws-Best                 PIC S9(13)V9(6) VALUE 0.
     05  Ws-Net-Req              PIC S9(13)V9(6) VALUE 0.
     05  Ws-From-Stock           PIC S9(13)V9(6) VALUE 0.
     05  Ws-From-Inbound         PIC S9(13)V9(6) VALUE 0.
     05  Ws-Residual             PIC S9(13)V9(6) VALUE 0.
     05  Ws-Worst-Short          PIC S9(13)V9(6) VALUE 0.
     05  Ws-Keep-Result          PIC X VALUE "Y".
     05  Ws-More-Pages           PIC X VALUE "Y".
     05  Ws-Trial-Id-Txt         PIC X(20) VALUE SPACES.
     05  Ws-Edit                 PIC -(13)9.9(6).
     05  Ws-Int-Edit             PIC -(9)9.
 LINKAGE SECTION.
 COPY KITTRIALCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "KITTRIAL" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE
     MOVE 0 TO CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     MOVE "KIT-006" TO CA-REQ-REF
*>   The one promise this program makes before it does anything.
     MOVE "N" TO KT-COMMITTED
     EVALUATE CA-FUNCTION
         WHEN "TRIAL"    PERFORM Run-The-Trial
         WHEN "MAXBUILD" PERFORM Run-Max-Buildable
         WHEN "GETLINE"  PERFORM Fill-Window
         WHEN "SAVE"     PERFORM Save-The-Evidence
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     MOVE "N" TO KT-COMMITTED
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "KIT0002" TO CA-REASON-CODE
     STRING "KITTRIAL does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>----------------------------------------------------------------
 Validate-Request.
     IF KT-PARENT-ITEM = SPACES
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "A parent item is required for a trial." TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KT-REQUESTED-QTY <= 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0003" TO CA-REASON-CODE
         MOVE "The requested quantity must be greater than zero."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KT-SITE = SPACES
         MOVE CA-SITE TO KT-SITE
     END-IF
     IF KT-REQ-DATE = 0
         MOVE CA-BUSINESS-DATE TO KT-REQ-DATE
     END-IF
*>   D-10: the selection date is the planned start, so a trial
*>   sees the revision the shop floor would really use.
     IF KT-SELECTION-DATE = 0
         MOVE CA-BUSINESS-DATE TO KT-SELECTION-DATE
     END-IF
     IF KT-SCENARIO-NO <= 0
         MOVE 1 TO KT-SCENARIO-NO
     END-IF
     IF KT-WIN-MAX <= 0 OR KT-WIN-MAX > 20
         MOVE 20 TO KT-WIN-MAX
     END-IF.

 Clear-Answer.
     MOVE 0 TO Ws-Res-Cnt
     MOVE 0 TO Ws-Av-Cnt
     MOVE 0 TO Ws-Probe-Count
     MOVE 0 TO KT-LINE-TOTAL
     MOVE 0 TO KT-SHORTAGE-CNT
     MOVE 0 TO KT-MAX-BUILDABLE
     MOVE 0 TO KT-LIMITING-SHORT
     MOVE SPACES TO KT-LIMITING-ITEM
     MOVE SPACES TO KT-WARNINGS
     MOVE SPACES TO KT-ACTION-HINT
     MOVE "N" TO KT-FEASIBLE
     MOVE "COMPLETE" TO KT-COMPLETENESS
     MOVE 0 TO KT-WIN-CNT
     MOVE "N" TO KT-MORE-LINES.

*>----------------------------------------------------------------
*> TRIAL - feasibility of exactly the requested quantity.
 Run-The-Trial.
     PERFORM Validate-Request
     IF CA-RETURN-CODE NOT = 0
         EXIT PARAGRAPH
     END-IF
     PERFORM Clear-Answer
     MOVE KT-REQUESTED-QTY TO Ws-Probe-Qty
     MOVE "Y" TO Ws-Keep-Result
     PERFORM Probe-One-Quantity
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     MOVE Ws-Probe-Feasible TO KT-FEASIBLE
     IF KT-IS-FEASIBLE
         MOVE KT-REQUESTED-QTY TO KT-MAX-BUILDABLE
     END-IF
     PERFORM Describe-The-Answer
     PERFORM Fill-Window.

*>----------------------------------------------------------------
*> MAXBUILD - the largest complete output that satisfies EVERY
*> constraint.  Each candidate is a full re-explosion, because a
*> fixed-per-run component or a batch step makes the answer
*> non-linear: nine frames can need exactly as many batches as
*> ten and still fit, while eleven would not.
 Run-Max-Buildable.
     PERFORM Run-The-Trial
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     MOVE "N" TO Ws-Keep-Result
     IF KT-IS-FEASIBLE
         MOVE KT-REQUESTED-QTY TO Ws-Best
         MOVE KT-REQUESTED-QTY TO Ws-Lo
         PERFORM Grow-Upper-Bound
     ELSE
         MOVE 0 TO Ws-Best
         MOVE 0 TO Ws-Lo
         MOVE KT-REQUESTED-QTY TO Ws-Hi
     END-IF
     PERFORM Narrow-The-Bound
     MOVE Ws-Best TO KT-MAX-BUILDABLE
     IF Ws-Best = 0
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0021" TO CA-REASON-CODE
         MOVE "No complete output can be built from the material that is free today."
              TO CA-MESSAGE
     END-IF
*>   The retained lines must describe the requested quantity, not
*>   the last probe, so the window is rebuilt at the request.
     MOVE KT-REQUESTED-QTY TO Ws-Probe-Qty
     MOVE "Y" TO Ws-Keep-Result
     MOVE 0 TO Ws-Res-Cnt
     PERFORM Probe-One-Quantity
     MOVE Ws-Probe-Feasible TO KT-FEASIBLE
     PERFORM Describe-The-Answer
     PERFORM Fill-Window.

 Grow-Upper-Bound.
     COMPUTE Ws-Hi = Ws-Lo * 2
     MOVE "Y" TO Ws-Probe-Feasible
     PERFORM UNTIL Ws-Probe-Feasible = "N"
                OR Ws-Probe-Count > 8
                OR CA-RETURN-CODE > 4
         MOVE Ws-Hi TO Ws-Probe-Qty
         PERFORM Probe-One-Quantity
         IF Ws-Probe-Feasible = "Y"
             MOVE Ws-Hi TO Ws-Lo
             MOVE Ws-Hi TO Ws-Best
             COMPUTE Ws-Hi = Ws-Hi * 2
         END-IF
     END-PERFORM.

 Narrow-The-Bound.
     PERFORM UNTIL (Ws-Hi - Ws-Lo) <= 1
                OR Ws-Probe-Count > 20
                OR CA-RETURN-CODE > 4
         COMPUTE Ws-Mid = FUNCTION INTEGER ((Ws-Lo + Ws-Hi) / 2)
         IF Ws-Mid <= Ws-Lo OR Ws-Mid >= Ws-Hi
             EXIT PERFORM
         END-IF
         MOVE Ws-Mid TO Ws-Probe-Qty
         PERFORM Probe-One-Quantity
         IF Ws-Probe-Feasible = "Y"
             MOVE Ws-Mid TO Ws-Lo
             MOVE Ws-Mid TO Ws-Best
         ELSE
             MOVE Ws-Mid TO Ws-Hi
         END-IF
     END-PERFORM.

*>----------------------------------------------------------------
*> One complete what-if at Ws-Probe-Qty.
 Probe-One-Quantity.
     ADD 1 TO Ws-Probe-Count
     MOVE "Y" TO Ws-Probe-Feasible
     MOVE 0 TO Ws-Worst-Short
     IF Ws-Keep-Result = "Y"
         MOVE 0 TO Ws-Res-Cnt
         MOVE 0 TO KT-SHORTAGE-CNT
     END-IF
     PERFORM Reset-Cache-Remainders
     PERFORM Explode-At-Probe-Qty.

 Reset-Cache-Remainders.
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > Ws-Av-Cnt
         MOVE Ws-A-Free (Ws-I) TO Ws-A-Remain (Ws-I)
         MOVE Ws-A-Inbound (Ws-I) TO Ws-A-Inb-Remain (Ws-I)
     END-PERFORM.

*>   BOMEXPL owns yield, fixed usage, batch steps, rounding and
*>   phantom expansion.  KITTRIAL never reimplements any of it -
*>   a trial that disagreed with the bill would be worthless.
 Explode-At-Probe-Qty.
     INITIALIZE Ws-Expl-Ca
     MOVE "EXPLODE" TO EA-FUNCTION
     MOVE CA-COMPANY TO EA-COMPANY
     MOVE KT-SITE TO EA-SITE
     MOVE CA-USER TO EA-USER
     MOVE CA-BUSINESS-DATE TO EA-BUSINESS-DATE
     MOVE "EXECUTION" TO BX-CALC-TYPE
     MOVE KT-PARENT-ITEM TO BX-PARENT-ITEM
     MOVE KT-PARENT-REV TO BX-PARENT-REV
*>   BOMEXPL only knows STANDARD / JOB / ORDER (see docs\modules\bom.md).
*>   A job or order bill is only selectable when the demand reference
*>   that owns it comes with the trial; anything else is the standard bill.
     EVALUATE TRUE
         WHEN KT-DEMAND-TYPE = "JOB" AND KT-DEMAND-REF NOT = SPACES
             MOVE "JOB" TO BX-PURPOSE
             MOVE KT-DEMAND-REF TO BX-JOB-REF
         WHEN KT-DEMAND-TYPE = "SALESORDER" AND KT-DEMAND-REF NOT = SPACES
             MOVE "ORDER" TO BX-PURPOSE
             MOVE KT-DEMAND-REF TO BX-JOB-REF
         WHEN OTHER
             MOVE "STANDARD" TO BX-PURPOSE
     END-EVALUATE
     MOVE Ws-Probe-Qty TO BX-REQ-QTY
     MOVE KT-SELECTION-DATE TO BX-EFF-DATE
     MOVE "Y" TO BX-EXPAND-PHANTOM
     MOVE "N" TO BX-INCLUDE-NOTES
     MOVE "N" TO BX-INCLUDE-BYPROD
*>   A made sub-assembly that may be taken from stock stops the
*>   explosion at that level; otherwise the trial asks for the
*>   leaves that would actually have to be found.
     IF KT-USE-SUBASSY = "Y"
         MOVE 25 TO BX-MAX-DEPTH
         MOVE "LEVEL1" TO BX-VIEW
     ELSE
         MOVE 25 TO BX-MAX-DEPTH
         MOVE "LEAF" TO BX-VIEW
     END-IF
     MOVE 25 TO BX-WIN-MAX
     MOVE 1 TO BX-FROM-LINE
     CALL "BOMEXPL" USING Ws-Expl-Ca
     IF EA-RETURN-CODE > 4
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0004" TO CA-REASON-CODE
         MOVE EA-MESSAGE TO CA-MESSAGE
         MOVE "N" TO Ws-Probe-Feasible
         EXIT PARAGRAPH
     END-IF
     IF BX-COMPLETENESS NOT = "COMPLETE"
         MOVE "INCOMPLETE" TO KT-COMPLETENESS
         MOVE 4 TO CA-RETURN-CODE
         MOVE "KIT0004" TO CA-REASON-CODE
         MOVE "The bill of material is not complete, so the trial cannot be relied on."
              TO CA-MESSAGE
         MOVE BX-ERROR-PATH TO KT-WARNINGS
         MOVE "N" TO Ws-Probe-Feasible
         EXIT PARAGRAPH
     END-IF
     MOVE BX-BOM-ID TO KT-BOM-ID
*>   EXPLODE only computes and retains the result; the lines are
*>   read back a window at a time with GETLINE, and BOMEXPL itself
*>   advances BX-FROM-LINE to the next unread occurrence.
     MOVE "GETLINE" TO EA-FUNCTION
     MOVE 1 TO BX-FROM-LINE
     MOVE "Y" TO Ws-More-Pages
     PERFORM UNTIL Ws-More-Pages = "N" OR CA-RETURN-CODE > 4
         CALL "BOMEXPL" USING Ws-Expl-Ca
         IF EA-RETURN-CODE > 4
             MOVE 8 TO CA-RETURN-CODE
             MOVE "KIT0004" TO CA-REASON-CODE
             MOVE EA-MESSAGE TO CA-MESSAGE
             MOVE "N" TO Ws-Probe-Feasible
             EXIT PERFORM
         END-IF
         PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > BX-WIN-CNT
             PERFORM Assess-One-Component
         END-PERFORM
         IF BX-MORE-LINES NOT = "Y"
             MOVE "N" TO Ws-More-Pages
         END-IF
     END-PERFORM
     IF Ws-Keep-Result = "Y"
         MOVE Ws-Res-Cnt TO KT-LINE-TOTAL
     END-IF.

*>   Netting order, and the reason for it: free stock is real
*>   today, expected inbound is only a promise, so the promise is
*>   never allowed to hide the immediate shortage.  It only moves
*>   the date at which the shortage is cured.
 Assess-One-Component.
     IF BX-L-NODE-TYPE (Ws-I) = "NOTE"
         OR BX-L-NODE-TYPE (Ws-I) = "RESOURCE"
         OR BX-L-NODE-TYPE (Ws-I) = "BYPROD"
         EXIT PARAGRAPH
     END-IF
*>   Services, cost-only resources and non-stock lines are leaves of
*>   the bill but are never found in stock - they are not shortages.
     IF BX-L-SUPPLY (Ws-I) = "SERVICE"
         OR BX-L-SUPPLY (Ws-I) = "COSTONLY"
         OR BX-L-SUPPLY (Ws-I) = "NONSTOCK"
         OR BX-L-SUPPLY (Ws-I) = "NOTE"
         EXIT PARAGRAPH
     END-IF
     IF BX-L-REQ-QTY (Ws-I) = 0
         EXIT PARAGRAPH
     END-IF
     PERFORM Find-Availability
     IF Ws-Hit = 0
         EXIT PARAGRAPH
     END-IF
*>   The requirement of the line is BX-L-REQ-QTY.  BX-L-GROSS is the
*>   gross STARTS of a manufactured node and is zero on a leaf, so
*>   reading it found every bill feasible whatever the stock.
     COMPUTE Ws-Net-Req = BX-L-REQ-QTY (Ws-I) - BX-L-STOCK-USED (Ws-I)
     IF Ws-Net-Req < 0
         MOVE 0 TO Ws-Net-Req
     END-IF
     IF Ws-Net-Req > Ws-A-Remain (Ws-Hit)
         MOVE Ws-A-Remain (Ws-Hit) TO Ws-From-Stock
     ELSE
         MOVE Ws-Net-Req TO Ws-From-Stock
     END-IF
     COMPUTE Ws-A-Remain (Ws-Hit) = Ws-A-Remain (Ws-Hit)
                                  - Ws-From-Stock
     COMPUTE Ws-Residual = Ws-Net-Req - Ws-From-Stock
     MOVE 0 TO Ws-From-Inbound
     IF Ws-Residual > 0 AND KT-INBOUND-OK
         IF Ws-Residual > Ws-A-Inb-Remain (Ws-Hit)
             MOVE Ws-A-Inb-Remain (Ws-Hit) TO Ws-From-Inbound
         ELSE
             MOVE Ws-Residual TO Ws-From-Inbound
         END-IF
         COMPUTE Ws-A-Inb-Remain (Ws-Hit) = Ws-A-Inb-Remain (Ws-Hit)
                                          - Ws-From-Inbound
     END-IF
     IF (Ws-Residual - Ws-From-Inbound) > 0
         MOVE "N" TO Ws-Probe-Feasible
     END-IF
     IF Ws-Residual > Ws-Worst-Short
         MOVE Ws-Residual TO Ws-Worst-Short
         IF Ws-Keep-Result = "Y"
             MOVE BX-L-ITEM (Ws-I) TO KT-LIMITING-ITEM
             MOVE Ws-Residual TO KT-LIMITING-SHORT
         END-IF
     END-IF
     IF Ws-Keep-Result = "Y"
         PERFORM Record-Result-Line
     END-IF.

 Record-Result-Line.
     IF Ws-Res-Cnt >= Ws-Res-Max
         MOVE "More components than the trial table holds; the answer is truncated."
              TO KT-WARNINGS
         EXIT PARAGRAPH
     END-IF
     ADD 1 TO Ws-Res-Cnt
     MOVE Ws-Res-Cnt TO Ws-R-Line (Ws-Res-Cnt)
     MOVE BX-L-PATH (Ws-I) (1:20) TO Ws-R-Path (Ws-Res-Cnt)
     MOVE BX-L-DEPTH (Ws-I) TO Ws-R-Depth (Ws-Res-Cnt)
     MOVE BX-L-ITEM (Ws-I) TO Ws-R-Item (Ws-Res-Cnt)
     MOVE BX-L-REV (Ws-I) TO Ws-R-Rev (Ws-Res-Cnt)
     MOVE BX-L-UOM (Ws-I) TO Ws-R-Uom (Ws-Res-Cnt)
     MOVE BX-L-REQ-QTY (Ws-I) TO Ws-R-Gross (Ws-Res-Cnt)
     MOVE Ws-Net-Req TO Ws-R-Net-Req (Ws-Res-Cnt)
     MOVE Ws-A-Free (Ws-Hit) TO Ws-R-Free (Ws-Res-Cnt)
     MOVE Ws-A-Own-Resv (Ws-Hit) TO Ws-R-Own-Resv (Ws-Res-Cnt)
     MOVE Ws-A-Other-Resv (Ws-Hit) TO Ws-R-Other-Resv (Ws-Res-Cnt)
     MOVE Ws-A-Inbound (Ws-Hit) TO Ws-R-Inbound (Ws-Res-Cnt)
     MOVE Ws-A-Inb-Date (Ws-Hit) TO Ws-R-Inb-Date (Ws-Res-Cnt)
     MOVE Ws-From-Stock TO Ws-R-Allocated (Ws-Res-Cnt)
     MOVE Ws-Residual TO Ws-R-Shortage (Ws-Res-Cnt)
     MOVE "N" TO Ws-R-Limiting (Ws-Res-Cnt)
     MOVE SPACES TO Ws-R-Warning (Ws-Res-Cnt)
     MOVE 0 TO Ws-R-Short-Date (Ws-Res-Cnt)
     IF Ws-Residual > 0
         ADD 1 TO KT-SHORTAGE-CNT
         IF Ws-From-Inbound > 0
             MOVE Ws-A-Inb-Date (Ws-Hit)
                  TO Ws-R-Short-Date (Ws-Res-Cnt)
             MOVE "AWAIT INBOUND" TO Ws-R-Action (Ws-Res-Cnt)
             MOVE "Covered only by expected supply, not by free stock."
                  TO Ws-R-Warning (Ws-Res-Cnt)
         ELSE
             MOVE "ORDER OR SUBSTITUTE" TO Ws-R-Action (Ws-Res-Cnt)
         END-IF
     ELSE
         MOVE "AVAILABLE" TO Ws-R-Action (Ws-Res-Cnt)
     END-IF.

*>   AVAILQ answers for the item once per trial.  KT-DEMAND-TYPE
*>   / KT-DEMAND-REF let the service separate this demand's own
*>   reservations (which are available to it) from everybody
*>   else's (which are not) - KIT-008.
 Find-Availability.
     MOVE 0 TO Ws-Hit
     PERFORM VARYING Ws-J FROM 1 BY 1 UNTIL Ws-J > Ws-Av-Cnt
         IF Ws-A-Item (Ws-J) = BX-L-ITEM (Ws-I)
             MOVE Ws-J TO Ws-Hit
             EXIT PERFORM
         END-IF
     END-PERFORM
     IF Ws-Hit > 0
         EXIT PARAGRAPH
     END-IF
     IF Ws-Av-Cnt >= Ws-Av-Max
*>       Borrowing the last slot answered with another item's
*>       stock; a full table now makes the trial infeasible and
*>       says so.
         MOVE 0 TO Ws-Hit
         MOVE "N" TO Ws-Probe-Feasible
         MOVE "More distinct components than the trial can assess; not feasible as asked."
              TO KT-WARNINGS
         EXIT PARAGRAPH
     END-IF
     INITIALIZE Ws-Avail-Ca
     MOVE "AVAIL" TO VA-FUNCTION
     MOVE CA-COMPANY TO VA-COMPANY
     MOVE KT-SITE TO VA-SITE
     MOVE CA-USER TO VA-USER
     MOVE CA-BUSINESS-DATE TO VA-BUSINESS-DATE
     MOVE BX-L-ITEM (Ws-I) TO AV-ITEM-CODE
     MOVE KT-SITE TO AV-SITE
     MOVE KT-REQ-DATE TO AV-AS-AT
     IF KT-DEMAND-TYPE = SPACES
         MOVE "KIT" TO AV-DEMAND-TYPE
     ELSE
         MOVE KT-DEMAND-TYPE TO AV-DEMAND-TYPE
     END-IF
     IF KT-DEMAND-REF = SPACES
         MOVE KT-KIT-REF TO AV-DEMAND-REF
     ELSE
         MOVE KT-DEMAND-REF TO AV-DEMAND-REF
     END-IF
     CALL "AVAILQ" USING Ws-Avail-Ca
     ADD 1 TO Ws-Av-Cnt
     MOVE Ws-Av-Cnt TO Ws-Hit
     MOVE BX-L-ITEM (Ws-I) TO Ws-A-Item (Ws-Hit)
     MOVE AV-STOCK-UOM TO Ws-A-Uom (Ws-Hit)
     IF VA-RETURN-CODE > 4
         MOVE 0 TO Ws-A-Free (Ws-Hit)
         MOVE 0 TO Ws-A-Own-Resv (Ws-Hit)
         MOVE 0 TO Ws-A-Other-Resv (Ws-Hit)
         MOVE 0 TO Ws-A-Inbound (Ws-Hit)
         MOVE 0 TO Ws-A-Inb-Date (Ws-Hit)
     ELSE
*>       AV-QTY-FOR-DEMAND already adds back this demand's own
*>       reservations, so a kit is not told it is short of the
*>       very stock it has already set aside for itself.
         MOVE AV-QTY-FOR-DEMAND TO Ws-A-Free (Ws-Hit)
         MOVE AV-QTY-OWN-DEMAND TO Ws-A-Own-Resv (Ws-Hit)
         COMPUTE Ws-A-Other-Resv (Ws-Hit) = AV-QTY-RESERVED
                                          - AV-QTY-OWN-DEMAND
         MOVE AV-QTY-EXPECTED TO Ws-A-Inbound (Ws-Hit)
         MOVE AV-EXPECTED-DATE TO Ws-A-Inb-Date (Ws-Hit)
     END-IF
     IF Ws-A-Free (Ws-Hit) < 0
         MOVE 0 TO Ws-A-Free (Ws-Hit)
     END-IF
     IF Ws-A-Other-Resv (Ws-Hit) < 0
         MOVE 0 TO Ws-A-Other-Resv (Ws-Hit)
     END-IF
     MOVE Ws-A-Free (Ws-Hit) TO Ws-A-Remain (Ws-Hit)
     MOVE Ws-A-Inbound (Ws-Hit) TO Ws-A-Inb-Remain (Ws-Hit).

*>----------------------------------------------------------------
 Describe-The-Answer.
     PERFORM Mark-Limiting-Line
     IF KT-SHORTAGE-CNT = 0
         MOVE "COMPLETE" TO KT-COMPLETENESS
     END-IF
     IF KT-IS-FEASIBLE
         MOVE 0 TO CA-RETURN-CODE
         MOVE "RELEASE OR RESERVE" TO KT-ACTION-HINT
         MOVE KT-MAX-BUILDABLE TO Ws-Edit
         STRING "Feasible. Maximum complete output "
                FUNCTION TRIM(Ws-Edit) "."
             DELIMITED BY SIZE INTO CA-MESSAGE
     ELSE
         IF CA-RETURN-CODE = 0
             MOVE 4 TO CA-RETURN-CODE
             MOVE "KIT0010" TO CA-REASON-CODE
         END-IF
         MOVE "REDUCE, SUBSTITUTE OR ORDER" TO KT-ACTION-HINT
         MOVE KT-LIMITING-SHORT TO Ws-Edit
         STRING "Short on " FUNCTION TRIM(KT-LIMITING-ITEM)
                " by " FUNCTION TRIM(Ws-Edit)
                ". Nothing has been reserved or ordered."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF.

 Mark-Limiting-Line.
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > Ws-Res-Cnt
         IF Ws-R-Item (Ws-I) = KT-LIMITING-ITEM
            AND Ws-R-Shortage (Ws-I) = KT-LIMITING-SHORT
            AND KT-LIMITING-SHORT > 0
             MOVE "Y" TO Ws-R-Limiting (Ws-I)
             EXIT PERFORM
         END-IF
     END-PERFORM.

*>----------------------------------------------------------------
*> GETLINE - page the retained answer.  No recalculation, so a
*> second page can never disagree with the first.
 Fill-Window.
     IF KT-WIN-MAX <= 0 OR KT-WIN-MAX > 20
         MOVE 20 TO KT-WIN-MAX
     END-IF
     IF KT-FROM-LINE < 0
         MOVE 0 TO KT-FROM-LINE
     END-IF
     MOVE Ws-Res-Cnt TO KT-LINE-TOTAL
     MOVE 0 TO KT-WIN-CNT
     MOVE "N" TO KT-MORE-LINES
     PERFORM VARYING Ws-I FROM 1 BY 1
         UNTIL Ws-I > 20 OR KT-WIN-CNT >= KT-WIN-MAX
         COMPUTE Ws-J = KT-FROM-LINE + Ws-I
         IF Ws-J > Ws-Res-Cnt
             EXIT PERFORM
         END-IF
         ADD 1 TO KT-WIN-CNT
         MOVE Ws-R-Line (Ws-J) TO KT-L-LINE-NUM (KT-WIN-CNT)
         MOVE Ws-R-Path (Ws-J) TO KT-L-PATH (KT-WIN-CNT)
         MOVE Ws-R-Depth (Ws-J) TO KT-L-DEPTH (KT-WIN-CNT)
         MOVE Ws-R-Item (Ws-J) TO KT-L-ITEM (KT-WIN-CNT)
         MOVE Ws-R-Rev (Ws-J) TO KT-L-REV (KT-WIN-CNT)
         MOVE Ws-R-Uom (Ws-J) TO KT-L-UOM (KT-WIN-CNT)
         MOVE Ws-R-Gross (Ws-J) TO KT-L-GROSS (KT-WIN-CNT)
         MOVE Ws-R-Net-Req (Ws-J) TO KT-L-NET-REQ (KT-WIN-CNT)
         MOVE Ws-R-Free (Ws-J) TO KT-L-FREE (KT-WIN-CNT)
         MOVE Ws-R-Own-Resv (Ws-J) TO KT-L-OWN-RESV (KT-WIN-CNT)
         MOVE Ws-R-Other-Resv (Ws-J) TO KT-L-OTHER-RESV (KT-WIN-CNT)
         MOVE Ws-R-Inbound (Ws-J) TO KT-L-INBOUND (KT-WIN-CNT)
         MOVE Ws-R-Inb-Date (Ws-J) TO KT-L-INB-DATE (KT-WIN-CNT)
         MOVE Ws-R-Allocated (Ws-J) TO KT-L-ALLOCATED (KT-WIN-CNT)
         MOVE Ws-R-Shortage (Ws-J) TO KT-L-SHORTAGE (KT-WIN-CNT)
         MOVE Ws-R-Short-Date (Ws-J) TO KT-L-SHORT-DATE (KT-WIN-CNT)
         MOVE Ws-R-Limiting (Ws-J) TO KT-L-LIMITING (KT-WIN-CNT)
         MOVE Ws-R-Action (Ws-J) TO KT-L-ACTION (KT-WIN-CNT)
         MOVE Ws-R-Warning (Ws-J) TO KT-L-WARNING (KT-WIN-CNT)
     END-PERFORM
     IF (KT-FROM-LINE + KT-WIN-CNT) < Ws-Res-Cnt
         MOVE "Y" TO KT-MORE-LINES
     END-IF.

*>----------------------------------------------------------------
*> SAVE - evidence only.  A new scenario number is allocated on
*> every save, because re-running with a different priority order
*> is a NEW answer, never an overwrite of the first one (KIT-006).
 Save-The-Evidence.
     IF Ws-Res-Cnt = 0
         MOVE 8 TO CA-RETURN-CODE
         MOVE "KIT0021" TO CA-REASON-CODE
         MOVE "There is no trial result to save. Run TRIAL first."
              TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF KT-TRIAL-REF = SPACES
         MOVE KT-PARENT-ITEM (1:30) TO KT-TRIAL-REF
     END-IF
     PERFORM Next-Scenario-No
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM Insert-Trial-Header
     IF CA-RETURN-CODE > 4
         EXIT PARAGRAPH
     END-IF
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > Ws-Res-Cnt
                                        OR CA-RETURN-CODE > 4
         PERFORM Insert-Trial-Line
     END-PERFORM
     IF CA-RETURN-CODE = 0
         MOVE Ws-Res-Cnt TO Ws-Int-Edit
         STRING "Trial " FUNCTION TRIM(KT-TRIAL-REF)
                " saved as evidence with "
                FUNCTION TRIM(Ws-Int-Edit)
                " lines. Still no commitment of any kind."
             DELIMITED BY SIZE INTO CA-MESSAGE
     END-IF.

 Next-Scenario-No.
     MOVE SPACES TO SQLIO-STMT
     STRING "SELECT CONVERT(varchar(8), ISNULL(MAX(ScenarioNo),0)+1) "
            "FROM kit.TrialKit WHERE CompanyCode = ? AND TrialRef = ?"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE KT-TRIAL-REF TO SQLIO-PARM-VAL (2)
     MOVE "N" TO SQLIO-PARM-NULL (1)
     MOVE "N" TO SQLIO-PARM-NULL (2)
     MOVE 2 TO SQLIO-PARM-CNT
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 8 TO SQLIO-COL-WIDTH (1)
     MOVE 7 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Trial-Id-Txt
     IF SQLIO-ERROR
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     IF SQLIO-NOTFOUND
         MOVE 1 TO KT-SCENARIO-NO
     ELSE
         COMPUTE KT-SCENARIO-NO =
             FUNCTION NUMVAL (Ws-Trial-Id-Txt (1:8))
     END-IF.

 Insert-Trial-Header.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.TrialKit (CompanyCode, SiteCode, TrialRef, "
            "ParentItemCode, Revision, RequestedQty, RequiredDate, "
            "SelectionDate, IncludeExpectedSupply, "
            "StockEligibilityPolicy, PrioritySequence, ScenarioNo, "
            "MaxBuildableQty, LimitingItemCode, ShortageCount, "
            "Completeness, Warnings, RequestedBy) "
            "OUTPUT INSERTED.TrialId VALUES (?, ?, ?, ?, "
            "NULLIF(?,''), ?, CONVERT(date, NULLIF(?,'0'), 112), "
            "CONVERT(date, ?, 112), ?, NULLIF(?,''), NULLIF(?,''), "
            "?, ?, NULLIF(?,''), ?, ?, NULLIF(?,''), ?)"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE CA-COMPANY TO SQLIO-PARM-VAL (1)
     MOVE KT-SITE TO SQLIO-PARM-VAL (2)
     MOVE KT-TRIAL-REF TO SQLIO-PARM-VAL (3)
     MOVE KT-PARENT-ITEM TO SQLIO-PARM-VAL (4)
     MOVE KT-PARENT-REV TO SQLIO-PARM-VAL (5)
     MOVE KT-REQUESTED-QTY TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (6)
     MOVE KT-REQ-DATE TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (7)
     MOVE KT-SELECTION-DATE TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (8)
     IF KT-INBOUND-OK
         MOVE "1" TO SQLIO-PARM-VAL (9)
     ELSE
         MOVE "0" TO SQLIO-PARM-VAL (9)
     END-IF
     MOVE KT-ELIG-POLICY TO SQLIO-PARM-VAL (10)
     MOVE KT-PRIORITY-SEQ TO SQLIO-PARM-VAL (11)
     MOVE KT-SCENARIO-NO TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (12)
     MOVE KT-MAX-BUILDABLE TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (13)
     MOVE KT-LIMITING-ITEM TO SQLIO-PARM-VAL (14)
     MOVE KT-SHORTAGE-CNT TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (15)
     MOVE KT-COMPLETENESS TO SQLIO-PARM-VAL (16)
     MOVE KT-WARNINGS (1:200) TO SQLIO-PARM-VAL (17)
     MOVE CA-USER TO SQLIO-PARM-VAL (18)
     MOVE 18 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-J FROM 1 BY 1 UNTIL Ws-J > 18
         MOVE "N" TO SQLIO-PARM-NULL (Ws-J)
     END-PERFORM
     MOVE 1 TO SQLIO-COL-CNT
     MOVE 20 TO SQLIO-COL-WIDTH (1)
     MOVE 7 TO SQLIO-CURSOR
     MOVE "SNGL" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Trial-Id-Txt
     IF SQLIO-ERROR
         PERFORM Set-Sql-Failure
         EXIT PARAGRAPH
     END-IF
     COMPUTE KT-TRIAL-ID = FUNCTION NUMVAL (Ws-Trial-Id-Txt).

 Insert-Trial-Line.
     MOVE SPACES TO SQLIO-STMT
     STRING "INSERT kit.TrialKitLine (TrialId, LineNum, PathRef, "
            "Depth, ComponentItemCode, Revision, UomCode, "
            "GrossRequirementQty, NetRequirementQty, FreeStockQty, "
            "OwnReservedQty, OtherReservedQty, ExpectedInboundQty, "
            "ExpectedInboundDate, AllocatedQty, ShortageQty, "
            "TimePhasedShortageDate, IsLimiting, SuggestedAction, "
            "WarningText) VALUES (?, ?, NULLIF(?,''), ?, ?, "
            "NULLIF(?,''), NULLIF(?,''), ?, ?, ?, ?, ?, ?, "
            "CONVERT(date, NULLIF(?,'0'), 112), ?, ?, "
            "CONVERT(date, NULLIF(?,'0'), 112), ?, NULLIF(?,''), "
            "NULLIF(?,''))"
         DELIMITED BY SIZE INTO SQLIO-STMT
     MOVE KT-TRIAL-ID TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (1)
     MOVE Ws-R-Line (Ws-I) TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (2)
     MOVE Ws-R-Path (Ws-I) TO SQLIO-PARM-VAL (3)
     MOVE Ws-R-Depth (Ws-I) TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (4)
     MOVE Ws-R-Item (Ws-I) TO SQLIO-PARM-VAL (5)
     MOVE Ws-R-Rev (Ws-I) TO SQLIO-PARM-VAL (6)
     MOVE Ws-R-Uom (Ws-I) TO SQLIO-PARM-VAL (7)
     MOVE Ws-R-Gross (Ws-I) TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (8)
     MOVE Ws-R-Net-Req (Ws-I) TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (9)
     MOVE Ws-R-Free (Ws-I) TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (10)
     MOVE Ws-R-Own-Resv (Ws-I) TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (11)
     MOVE Ws-R-Other-Resv (Ws-I) TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (12)
     MOVE Ws-R-Inbound (Ws-I) TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (13)
     MOVE Ws-R-Inb-Date (Ws-I) TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (14)
     MOVE Ws-R-Allocated (Ws-I) TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (15)
     MOVE Ws-R-Shortage (Ws-I) TO SQLIO-NUM-EDIT
     MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL (16)
     MOVE Ws-R-Short-Date (Ws-I) TO SQLIO-INT-EDIT
     MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL (17)
     IF Ws-R-Limiting (Ws-I) = "Y"
         MOVE "1" TO SQLIO-PARM-VAL (18)
     ELSE
         MOVE "0" TO SQLIO-PARM-VAL (18)
     END-IF
     MOVE Ws-R-Action (Ws-I) TO SQLIO-PARM-VAL (19)
     MOVE Ws-R-Warning (Ws-I) TO SQLIO-PARM-VAL (20)
     MOVE 20 TO SQLIO-PARM-CNT
     PERFORM VARYING Ws-J FROM 1 BY 1 UNTIL Ws-J > 20
         MOVE "N" TO SQLIO-PARM-NULL (Ws-J)
     END-PERFORM
     MOVE 0 TO SQLIO-COL-CNT
     MOVE 7 TO SQLIO-CURSOR
     MOVE "EXEC" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Trial-Id-Txt
     IF SQLIO-ERROR
         PERFORM Set-Sql-Failure
     END-IF
     MOVE 7 TO SQLIO-CURSOR
     MOVE "CLOS" TO SQLIO-FUNC
     CALL "SQLIO" USING SQLIO-CA, Ws-Trial-Id-Txt.

 Set-Sql-Failure.
     MOVE 12 TO CA-RETURN-CODE
     MOVE "SQL0001" TO CA-REASON-CODE
     MOVE SQLIO-ERRMSG (1:200) TO CA-MESSAGE.
