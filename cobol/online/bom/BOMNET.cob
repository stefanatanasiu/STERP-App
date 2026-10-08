>>SOURCE FORMAT IS FREE
*>================================================================
*> PROGRAM-ID : BOMNET
*> SYSTEM     : STERP - Stefan's ERP
*> MODULE     : BILLS OF MATERIAL
*> PURPOSE    : Netted supply planning.  A gross structural
*>              explosion assumes everything is made; a netted
*>              plan consumes eligible free supply ONCE and
*>              expands only the remaining manufacturing
*>              requirement.  The order of work follows
*>              specification section 8:
*>                1 establish the demand scope and eligibility,
*>                2 satisfy finished and subassembly demand from
*>                  eligible free stock once,
*>                3 expand only the quantity chosen to manufacture,
*>                4 combine compatible leaf requirements,
*>                5 allocate leaf stock across the whole scope,
*>                6 report shortage, then the purchase proposal
*>                  and the excess it creates.
*>              MAXBUILD answers the planner's real question - how
*>              many whole parents can be built from what is
*>              actually available - by retrying the plan with the
*>              same reading of stock until every constraint
*>              passes.  Specification B2 gives nine frames.
*> CALLED BY  : the explosion enquiry form, BOMEXPL batch report,
*>              the unit tests
*> INTERFACE  : CALL "BOMNET" USING WS-BOMNET-CA
*>              COPYBOOK BOMNETCA
*> CALLS      : BOMEXPL, BOMWORK, BOMSUPPL, BOMMATH
*>----------------------------------------------------------------
*> CHANGE LOG
*> 2015-02-06 AJS  CR2295 NETTED PLANNING ADDED
*> 2015-04-28 AJS  CR2312 MAXIMUM BUILDABLE OUTPUT.  THE FIRST
*>                 VERSION SCALED THE SHORTAGE LINEARLY AND GOT
*>                 TEN FRAMES; ROUNDING AND BATCHES MAKE THE
*>                 ANSWER NINE, SO IT IS SEARCHED, NOT DERIVED.
*> 2026-10-05 STERP TRIAL SUPPLY AND PRIORITY EVIDENCE
*>================================================================
*> REQ: BOM-016, BOM-020, BOM-029, BOM-032
 IDENTIFICATION DIVISION.
 PROGRAM-ID. BOMNET.
 AUTHOR. A J SPENCER.
 DATE-WRITTEN. 06/02/2015.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 COPY BOMWKCTL.
 COPY BOMOCCUR.
 COPY BOMSUPCT.
 COPY BOMEXPLCA REPLACING ==DFHCOMMAREA== BY ==Ws-Expl-Ca==
      LEADING ==CA-== BY ==EA-==
      LEADING ==BX-== BY ==EX-==.
 01  Ws-Work.
     05  Ws-I                    PIC S9(9) COMP-5 VALUE 0.
     05  Ws-W                    PIC S9(4) VALUE 0.
     05  Ws-Total                PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Leaf-Total           PIC S9(9) COMP-5 VALUE 0.
     05  Ws-Trial-Qty            PIC S9(15)V9(6) VALUE 0.
     05  Ws-Target               PIC S9(15)V9(6) VALUE 0.
     05  Ws-Root-Taken           PIC S9(15)V9(6) VALUE 0.
     05  Ws-Shortages            PIC S9(4) VALUE 0.
     05  Ws-Trials               PIC S9(4) VALUE 0.
     05  Ws-Max-Trials           PIC S9(4) VALUE 500.
     05  Ws-Fits                 PIC X VALUE "N".
     05  Ws-Searching            PIC X VALUE "Y".
     05  Ws-Edit                 PIC -(14)9.9(6).
 LINKAGE SECTION.
 COPY BOMNETCA.
 PROCEDURE DIVISION USING DFHCOMMAREA.
 Main-Entry.
     MOVE "BOMNET" TO CA-PROGRAM
     MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
     MOVE SPACES TO CA-REASON-CODE
     MOVE "BOM-020" TO CA-REQ-REF
     IF BN-WIN-MAX < 1 OR BN-WIN-MAX > 25
         MOVE 25 TO BN-WIN-MAX
     END-IF
     MOVE 0 TO BN-WIN-CNT
     MOVE "N" TO BN-MORE-LINES
     EVALUATE CA-FUNCTION
         WHEN "NET"      PERFORM Plan-One-Quantity
         WHEN "MAXBUILD" PERFORM Search-Maximum-Buildable
         WHEN "GETLINE"  PERFORM Page-The-Plan
         WHEN OTHER      PERFORM Reject-Function
     END-EVALUATE
     GOBACK.

 Reject-Function.
     MOVE 8 TO CA-RETURN-CODE
     MOVE "BOM0701" TO CA-REASON-CODE
     MOVE SPACES TO CA-MESSAGE
     STRING "BOMNET does not support the function "
            FUNCTION TRIM(CA-FUNCTION)
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>----------------------------------------------------------------
*> Step 1: one demand scope, one reading of eligible supply.  A
*> TRIAL plan uses exactly the quantities the caller declared, so
*> a what-if never touches or depends on live balances.
 Load-Eligible-Supply.
     INITIALIZE BOM-SUP-CTL
     MOVE "CLEAR" TO BP-FUNC
     CALL "BOMSUPPL" USING BOM-SUP-CTL
     IF BN-STOCK-SOURCE = SPACES
         MOVE "LIVE" TO BN-STOCK-SOURCE
     END-IF
     IF BN-STOCK-SOURCE NOT = "TRIAL"
         EXIT PARAGRAPH
     END-IF
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > BN-SUPPLY-CNT
         INITIALIZE BOM-SUP-CTL
         MOVE "SET" TO BP-FUNC
         MOVE CA-COMPANY TO BP-COMPANY
         MOVE CA-SITE TO BP-SITE
         MOVE "TRIAL" TO BP-SOURCE
         MOVE BN-S-ITEM(Ws-W) TO BP-ITEM
         MOVE BN-S-REV(Ws-W) TO BP-REV
         MOVE BN-S-FREE-QTY(Ws-W) TO BP-FREE
         MOVE BN-S-INBOUND(Ws-W) TO BP-INBOUND
         CALL "BOMSUPPL" USING BOM-SUP-CTL
         MOVE 0 TO BN-S-USED(Ws-W)
     END-PERFORM.

 Snapshot-Supply.
     INITIALIZE BOM-SUP-CTL
     MOVE "SNAP" TO BP-FUNC
     CALL "BOMSUPPL" USING BOM-SUP-CTL.

 Restore-Supply.
     INITIALIZE BOM-SUP-CTL
     MOVE "RESET" TO BP-FUNC
     CALL "BOMSUPPL" USING BOM-SUP-CTL.

*>----------------------------------------------------------------
*> Step 2: the finished demand itself may be covered from eligible
*> free stock of the parent.  Only the remainder is manufactured.
 Take-Finished-Stock.
     MOVE 0 TO Ws-Root-Taken
     INITIALIZE BOM-SUP-CTL
     MOVE "TAKE" TO BP-FUNC
     MOVE CA-COMPANY TO BP-COMPANY
     MOVE CA-SITE TO BP-SITE
     MOVE BN-STOCK-SOURCE TO BP-SOURCE
     MOVE BN-PARENT-ITEM TO BP-ITEM
     MOVE BN-PARENT-REV TO BP-REV
     MOVE Ws-Trial-Qty TO BP-WANT
     MOVE BN-USE-INBOUND TO BP-USE-INBOUND
     CALL "BOMSUPPL" USING BOM-SUP-CTL
     MOVE BP-TAKEN TO Ws-Root-Taken
     COMPUTE Ws-Trial-Qty = Ws-Trial-Qty - BP-TAKEN.

*> Steps 3 to 5 are the explosion itself: BOMEXPL consumes the
*> shared pool as it goes, so a stock unit is used once.
 Explode-Remaining.
     INITIALIZE Ws-Expl-Ca
     MOVE "EXPLODE" TO EA-FUNCTION
     MOVE CA-COMPANY TO EA-COMPANY
     MOVE CA-SITE TO EA-SITE
     MOVE CA-USER TO EA-USER
     MOVE CA-BUSINESS-DATE TO EA-BUSINESS-DATE
     MOVE "NET" TO EX-CALC-TYPE
     MOVE BN-CALC-REF TO EX-CALC-REF
     MOVE BN-PARENT-ITEM TO EX-PARENT-ITEM
     MOVE BN-PARENT-REV TO EX-PARENT-REV
     MOVE BN-PURPOSE TO EX-PURPOSE
     MOVE BN-VARIANT TO EX-VARIANT
     MOVE Ws-Trial-Qty TO EX-REQ-QTY
     MOVE BN-EFF-DATE TO EX-EFF-DATE
     MOVE BN-MAX-DEPTH TO EX-MAX-DEPTH
     MOVE "Y" TO EX-EXPAND-PHANTOM
     MOVE "Y" TO EX-INCLUDE-NOTES
     MOVE BN-STOCK-SOURCE TO EX-STOCK-SOURCE
     MOVE BN-USE-INBOUND TO EX-USE-INBOUND
     CALL "BOMEXPL" USING Ws-Expl-Ca
     MOVE EX-COMPLETENESS TO BN-COMPLETENESS
     MOVE EX-ERROR-PATH TO BN-ERROR-PATH
     MOVE EX-GROSS-STARTS TO BN-GROSS-STARTS
     MOVE EX-RUN-COUNT TO BN-RUN-COUNT
     MOVE EX-BATCH-COUNT TO BN-BATCH-COUNT
     MOVE EX-OCC-COUNT TO BN-OCC-COUNT
     MOVE EX-LEAF-COUNT TO BN-LEAF-COUNT.

*>----------------------------------------------------------------
*> Step 6: shortage first, then the proposed purchase excess.  A
*> plan that cannot be met now says so; it does not quietly assume
*> the material will appear.
 Measure-Shortages.
     MOVE 0 TO Ws-Shortages
     MOVE SPACES TO BN-SHORTAGE-ITEM
     MOVE 0 TO BN-SHORTAGE-QTY
     MOVE "LEAFCNT" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     MOVE BW-COUNT TO Ws-Leaf-Total
     PERFORM VARYING Ws-I FROM 1 BY 1 UNTIL Ws-I > Ws-Leaf-Total
         MOVE Ws-I TO BW-INDEX
         MOVE "GETLEAF" TO BW-FUNC
         CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
         IF BO-SHORTAGE > 0
             ADD 1 TO Ws-Shortages
             IF BN-SHORTAGE-ITEM = SPACES
                 MOVE BO-ITEM TO BN-SHORTAGE-ITEM
                 MOVE BO-SHORTAGE TO BN-SHORTAGE-QTY
             END-IF
         END-IF
     END-PERFORM
     MOVE Ws-Shortages TO BN-SHORTAGE-CNT
     IF Ws-Shortages = 0
         MOVE "Y" TO Ws-Fits
     ELSE
         MOVE "N" TO Ws-Fits
     END-IF.

 Record-Supply-Used.
     PERFORM VARYING Ws-W FROM 1 BY 1 UNTIL Ws-W > BN-SUPPLY-CNT
         INITIALIZE BOM-SUP-CTL
         MOVE "AVAIL" TO BP-FUNC
         MOVE BN-S-ITEM(Ws-W) TO BP-ITEM
         MOVE BN-S-REV(Ws-W) TO BP-REV
         CALL "BOMSUPPL" USING BOM-SUP-CTL
         IF BP-STATUS = "OK"
             COMPUTE BN-S-USED(Ws-W) =
                 BN-S-FREE-QTY(Ws-W) - BP-FREE
         END-IF
     END-PERFORM.

*>----------------------------------------------------------------
 Plan-One-Quantity.
     PERFORM Load-Eligible-Supply
     PERFORM Snapshot-Supply
     MOVE BN-REQ-QTY TO Ws-Target
     MOVE Ws-Target TO Ws-Trial-Qty
     PERFORM Take-Finished-Stock
     PERFORM Explode-Remaining
     PERFORM Measure-Shortages
     PERFORM Record-Supply-Used
     MOVE Ws-Fits TO BN-BUILDABLE-NOW
     MOVE 1 TO BN-TRIALS
     MOVE 0 TO BN-MAX-BUILDABLE
     PERFORM Publish-Plan-Message.

 Publish-Plan-Message.
     EVALUATE TRUE
         WHEN BN-COMPLETENESS = "ERROR"
             MOVE 8 TO CA-RETURN-CODE
             MOVE "BOM0710" TO CA-REASON-CODE
             MOVE SPACES TO CA-MESSAGE
             STRING "The netted plan could not be completed: "
                    FUNCTION TRIM(BN-ERROR-PATH)
                 DELIMITED BY SIZE INTO CA-MESSAGE
         WHEN BN-SHORTAGE-CNT > 0
             MOVE 4 TO CA-RETURN-CODE
             MOVE "BOM0711" TO CA-REASON-CODE
             MOVE BN-SHORTAGE-QTY TO Ws-Edit
             MOVE SPACES TO CA-MESSAGE
             STRING "Short of "
                    FUNCTION TRIM(BN-SHORTAGE-ITEM) " by "
                    FUNCTION TRIM(Ws-Edit)
                    " - the plan is not buildable now"
                 DELIMITED BY SIZE INTO CA-MESSAGE
             ADD 1 TO CA-MSG-COUNT
         WHEN OTHER
             MOVE 0 TO CA-RETURN-CODE
             MOVE SPACES TO CA-MESSAGE
             STRING "Netted plan complete with no shortage"
                 DELIMITED BY SIZE INTO CA-MESSAGE
     END-EVALUATE.

*>----------------------------------------------------------------
*> MAXBUILD - the exact maximum whole parent output.  Rounding and
*> batch counts are step functions, so the answer cannot be scaled
*> from a shortage; it is found by trying.  The pool is restored
*> between trials so every trial sees the same supply.
 Search-Maximum-Buildable.
     PERFORM Load-Eligible-Supply
     PERFORM Snapshot-Supply
     MOVE 0 TO Ws-Trials
     MOVE "Y" TO Ws-Searching
     MOVE BN-REQ-QTY TO Ws-Target
     MOVE 0 TO BN-MAX-BUILDABLE
     PERFORM UNTIL Ws-Searching = "N"
         ADD 1 TO Ws-Trials
         PERFORM Restore-Supply
         PERFORM One-Trial
         EVALUATE TRUE
             WHEN BN-COMPLETENESS = "ERROR"
                 MOVE "N" TO Ws-Searching
             WHEN Ws-Fits = "Y"
                 MOVE Ws-Target TO BN-MAX-BUILDABLE
                 MOVE "N" TO Ws-Searching
             WHEN Ws-Target <= 0
                 MOVE "N" TO Ws-Searching
             WHEN Ws-Trials >= Ws-Max-Trials
                 MOVE "N" TO Ws-Searching
                 MOVE 4 TO CA-RETURN-CODE
                 MOVE "BOM0713" TO CA-REASON-CODE
                 MOVE "The buildable search reached its trial limit"
                     TO CA-MESSAGE
             WHEN OTHER
                 SUBTRACT 1 FROM Ws-Target
         END-EVALUATE
     END-PERFORM
     MOVE Ws-Trials TO BN-TRIALS
     PERFORM Record-Supply-Used
     PERFORM Publish-Buildable-Message.

 One-Trial.
     MOVE Ws-Target TO Ws-Trial-Qty
     PERFORM Take-Finished-Stock
     PERFORM Explode-Remaining
     PERFORM Measure-Shortages.

 Publish-Buildable-Message.
     IF BN-COMPLETENESS = "ERROR"
         MOVE 8 TO CA-RETURN-CODE
         MOVE "BOM0710" TO CA-REASON-CODE
         MOVE BN-ERROR-PATH TO CA-MESSAGE
         EXIT PARAGRAPH
     END-IF
     IF BN-MAX-BUILDABLE >= BN-REQ-QTY
         MOVE "Y" TO BN-BUILDABLE-NOW
     ELSE
         MOVE "N" TO BN-BUILDABLE-NOW
         MOVE 4 TO CA-RETURN-CODE
         MOVE "BOM0712" TO CA-REASON-CODE
         ADD 1 TO CA-MSG-COUNT
     END-IF
     MOVE BN-MAX-BUILDABLE TO Ws-Edit
     MOVE SPACES TO CA-MESSAGE
     STRING "Maximum whole output from the eligible supply is "
            FUNCTION TRIM(Ws-Edit)
         DELIMITED BY SIZE INTO CA-MESSAGE.

*>----------------------------------------------------------------
 Page-The-Plan.
     MOVE "COUNT" TO BW-FUNC
     CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
     MOVE BW-COUNT TO Ws-Total
     MOVE BW-COUNT TO BN-OCC-COUNT
     IF BN-FROM-LINE < 1
         MOVE 1 TO BN-FROM-LINE
     END-IF
     MOVE BN-FROM-LINE TO Ws-I
     PERFORM UNTIL Ws-I > Ws-Total OR BN-WIN-CNT >= BN-WIN-MAX
         MOVE Ws-I TO BW-INDEX
         MOVE "GET" TO BW-FUNC
         CALL "BOMWORK" USING BOM-WK-CTL BOM-OCC-ROW
         ADD 1 TO BN-WIN-CNT
         MOVE BN-WIN-CNT TO Ws-W
         PERFORM Fill-Plan-Line
         ADD 1 TO Ws-I
     END-PERFORM
     MOVE Ws-I TO BN-FROM-LINE
     IF Ws-I <= Ws-Total
         MOVE "Y" TO BN-MORE-LINES
     END-IF.

 Fill-Plan-Line.
     MOVE BO-SEQ TO BN-L-SEQ(Ws-W)
     MOVE BO-DEPTH TO BN-L-DEPTH(Ws-W)
     MOVE BO-PATH TO BN-L-PATH(Ws-W)
     MOVE BO-ITEM TO BN-L-ITEM(Ws-W)
     MOVE BO-ITEM-REV TO BN-L-REV(Ws-W)
     MOVE BO-DESCRIPTION TO BN-L-DESC(Ws-W)
     MOVE BO-STOCK-UOM TO BN-L-UOM(Ws-W)
     MOVE BO-NODE-TYPE TO BN-L-NODE-TYPE(Ws-W)
     MOVE BO-SUPPLY TO BN-L-SUPPLY(Ws-W)
     MOVE BO-STOP-REASON TO BN-L-STOP(Ws-W)
     MOVE BO-REQ-QTY TO BN-L-REQ-QTY(Ws-W)
     MOVE BO-GOOD-QTY TO BN-L-GOOD-QTY(Ws-W)
     MOVE BO-GROSS-STARTS TO BN-L-GROSS(Ws-W)
     MOVE BO-BATCH-COUNT TO BN-L-BATCHES(Ws-W)
     MOVE BO-STOCK-USED TO BN-L-STOCK-USED(Ws-W)
     MOVE BO-INBOUND-USED TO BN-L-INBOUND(Ws-W)
     MOVE BO-GOOD-QTY TO BN-L-TO-MAKE(Ws-W)
     MOVE BO-SHORTAGE TO BN-L-SHORTAGE(Ws-W)
     MOVE BO-PACK-QTY TO BN-L-PACK-QTY(Ws-W)
     MOVE BO-EXCESS TO BN-L-EXCESS(Ws-W)
     MOVE BO-WARNING TO BN-L-WARNING(Ws-W).
