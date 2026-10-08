      *>================================================================
      *> PROGRAM-ID : STKMOVE
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *> PURPOSE    : THE ONLY WAY PHYSICAL STOCK CHANGES.  A DISPLAYED
      *>              BALANCE IS NEVER EDITED: EVERY CHANGE IS AN
      *>              AUTHORISED RECEIPT, ISSUE, TRANSFER, RETURN,
      *>              ADJUSTMENT, COMPLETION, SCRAP, OPENING LOAD OR
      *>              QUALITY ACTION THAT STATES ITS SOURCE, REASON,
      *>              OWNERSHIP, TRACEABILITY AND CONDITION.
      *>              STKMOVE VALIDATES IN COBOL (ITEM STATE AND CLASS
      *>              THROUGH ITEMVAL, UNITS AND QUANTUM THROUGH
      *>              UOMCONV, LOT AND SERIAL THROUGH LOTSER) AND THEN
      *>              POSTS THROUGH THE SHARED DATABASE CONTRACT
      *>              inv.usp_PostStockMovement, WHICH REFUSES TO
      *>              CREATE A NEGATIVE PHYSICAL BALANCE.
      *>              A TRANSFER THAT IS NOT INSTANT PARKS THE STOCK IN
      *>              THE SITE TRANSIT LOCATION, WHERE IT IS AVAILABLE
      *>              AT NEITHER ENDPOINT UNTIL CONFIRM.
      *> CALLED BY  : GOODS IN, DESPATCH, KIT ISSUE, COUNT ADJUSTMENT,
      *>              OPENING STOCK LOAD, THE .NET CLIENT
      *> INTERFACE  : CALL "STKMOVE" USING WS-STKMOVE-CA
      *>              COPYBOOK STKMOVECA
      *> CALLS      : ITEMVAL, UOMCONV, LOTSER, ITMDATE,
      *>              SQLIO -> inv.usp_PostStockMovement
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-03-08 RJH  CR0415 ORIGINAL - STOCK MOVEMENT POSTING
      *> 1997-11-04 DJM  CR0744 IN TRANSIT STAGE FOR SITE TRANSFERS
      *>                 (TEN UNITS WERE AVAILABLE AT BOTH ENDS)
      *> 2003-06-16 KMP  CR1098 QUALITY HOLD MOVES THE CONDITION AND
      *>                 KEEPS OWNERSHIP AND VALUATION HISTORY
      *> 2026-10-05 STERP ITM-015 TO ITM-017 ON usp_PostStockMovement
      *>================================================================
      *> REQ: ITM-015, ITM-016, ITM-017, ITM-024
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STKMOVE.
       AUTHOR. R J HOLLIS.
       DATE-WRITTEN. 08/03/1996.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ITMWORK.
       COPY ITMDATEW.
       COPY ITEMVALCA REPLACING ==DFHCOMMAREA== BY ==WS-ITEMVAL==.
       COPY UOMCONVCA REPLACING ==DFHCOMMAREA== BY ==WS-UOMCONV==.
       COPY LOTSERCA REPLACING ==DFHCOMMAREA== BY ==WS-LOTSER==.
      *>   the header values are copied out once so that the rest of
      *>   the program does not have to qualify every CA- field
       01  WS-HDR.
           05  WS-CO                   PIC X(10) VALUE SPACES.
           05  WS-SITE-IN              PIC X(10) VALUE SPACES.
           05  WS-USER                 PIC X(20) VALUE SPACES.
           05  WS-BDATE                PIC 9(8) VALUE 0.
           05  WS-CORR                 PIC X(36) VALUE SPACES.
           05  WS-FUNC                 PIC X(8) VALUE SPACES.
       01  WS-CONTROL.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-RC                   PIC 9(2) VALUE 0.
           05  WS-REASON               PIC X(8) VALUE SPACES.
           05  WS-MSG                  PIC X(200) VALUE SPACES.
           05  WS-ITEM-CLASS           PIC X(12) VALUE SPACES.
           05  WS-LIFECYCLE            PIC X(10) VALUE SPACES.
           05  WS-STOCK-UOM            PIC X(10) VALUE SPACES.
           05  WS-TRACE                PIC X(8) VALUE SPACES.
           05  WS-DIRECTION            PIC X VALUE SPACES.
           05  WS-MOVE-TYPE            PIC X(14) VALUE SPACES.
           05  WS-POST-SITE            PIC X(10) VALUE SPACES.
           05  WS-POST-WHS             PIC X(20) VALUE SPACES.
           05  WS-POST-LOC             PIC X(20) VALUE SPACES.
           05  WS-POST-COND            PIC X(12) VALUE SPACES.
           05  WS-POST-QTY             PIC S9(13)V9(6) VALUE 0.
           05  WS-POST-COUNTER         PIC X(20) VALUE SPACES.
           05  WS-POST-CORR            PIC X(40) VALUE SPACES.
           05  WS-LEG                  PIC 9 VALUE 0.
           05  WS-TRANSIT-QTY          PIC S9(13)V9(6) VALUE 0.
           05  WS-HOLD-QTY             PIC S9(13)V9(6) VALUE 0.
       LINKAGE SECTION.
       COPY STKMOVECA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               PERFORM 9500-REPORT
               GO TO 0000-EXIT
           END-IF
           PERFORM 0300-READ-ITEM
           IF WS-INPUT-BAD
               PERFORM 9500-REPORT
               GO TO 0000-EXIT
           END-IF
           EVALUATE WS-FUNC
               WHEN "RECEIPT"  PERFORM 1000-SIMPLE-MOVE
               WHEN "ISSUE"    PERFORM 1000-SIMPLE-MOVE
               WHEN "RETURN"   PERFORM 1000-SIMPLE-MOVE
               WHEN "ADJUST"   PERFORM 1000-SIMPLE-MOVE
               WHEN "COMPLETE" PERFORM 1000-SIMPLE-MOVE
               WHEN "OPENING"  PERFORM 1000-SIMPLE-MOVE
               WHEN "SCRAP"    PERFORM 1000-SIMPLE-MOVE
               WHEN "TRANSFER" PERFORM 2000-TRANSFER
               WHEN "CONFIRM"  PERFORM 3000-CONFIRM
               WHEN "HOLD"     PERFORM 4000-HOLD
               WHEN "RELEASE"  PERFORM 5000-RELEASE-HOLD
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE
           PERFORM 9500-REPORT.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-SQL-FAILED
           MOVE 0 TO WS-RC
           MOVE SPACES TO WS-REASON WS-MSG
           MOVE "STKMOVE" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "ITM-015" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE CA-COMPANY OF DFHCOMMAREA TO WS-CO
           MOVE CA-SITE OF DFHCOMMAREA TO WS-SITE-IN
           MOVE CA-USER OF DFHCOMMAREA TO WS-USER
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO WS-BDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO WS-CORR
           MOVE FUNCTION UPPER-CASE(CA-FUNCTION OF DFHCOMMAREA)
               TO WS-FUNC
           MOVE 0 TO SM-STOCK-QTY SM-FACTOR SM-MOVEMENT-ID
           MOVE 0 TO SM-MOVEMENT-ID-2 SM-QTY-BEFORE SM-QTY-AFTER
           MOVE 0 TO SM-VALUE-GBP
      *>   CR2605.  SM-HOLD-ID is an OUTPUT of HOLD but an INPUT of
      *>   RELEASE - clearing it on every call threw away the hold the
      *>   caller named and 5000-RELEASE-HOLD answered STK0014.
           IF WS-FUNC NOT = "RELEASE"
               MOVE 0 TO SM-HOLD-ID
           END-IF
           MOVE SPACES TO SM-MOVEMENT-TYPE SM-TRANSIT-LOC SM-NARRATIVE
           MOVE "N" TO SM-DUPLICATE
           MOVE FUNCTION UPPER-CASE(SM-ITEM-CODE) TO SM-ITEM-CODE
           MOVE FUNCTION UPPER-CASE(SM-UOM-CODE) TO SM-UOM-CODE
           MOVE FUNCTION UPPER-CASE(SM-LOT-CODE) TO SM-LOT-CODE
           MOVE FUNCTION UPPER-CASE(SM-SERIAL-NO) TO SM-SERIAL-NO
           IF SM-SITE = SPACES
               MOVE WS-SITE-IN TO SM-SITE
           END-IF
           IF SM-OWNER-TYPE = SPACES
               MOVE "OWNED" TO SM-OWNER-TYPE
           END-IF
           IF SM-CONDITION = SPACES
               MOVE "ACCEPTED" TO SM-CONDITION
           END-IF
           IF SM-SOURCE-MODULE = SPACES
               MOVE "ITM" TO SM-SOURCE-MODULE
           END-IF
           IF SM-MOVEMENT-DATE = 0
               MOVE WS-BDATE TO SM-MOVEMENT-DATE
           END-IF
           IF SM-MOVEMENT-DATE = 0
               MOVE "TODAY" TO ITD-FUNC
               CALL "ITMDATE" USING ITD-BLOCK
               MOVE ITD-DATE-R TO SM-MOVEMENT-DATE
           END-IF
           IF SM-POSTING-DATE = 0
               MOVE SM-MOVEMENT-DATE TO SM-POSTING-DATE
           END-IF
           IF SM-WAREHOUSE = SPACES
               MOVE "MAIN" TO SM-WAREHOUSE
           END-IF
           IF SM-TO-WAREHOUSE = SPACES
               MOVE SM-WAREHOUSE TO SM-TO-WAREHOUSE
           END-IF
           IF SM-TO-SITE = SPACES
               MOVE SM-SITE TO SM-TO-SITE
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0200-VALIDATE SECTION.
       0200-START.
           IF WS-CO = SPACES OR SM-ITEM-CODE = SPACES
               MOVE "STK0001" TO WS-REASON
               MOVE "A company and an item are required" TO WS-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF SM-SITE = SPACES OR SM-LOCATION = SPACES
               MOVE "STK0002" TO WS-REASON
               MOVE "A site and a stock location are required"
                   TO WS-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF SM-QUANTITY = 0
               MOVE "STK0003" TO WS-REASON
               MOVE "A movement must state a quantity" TO WS-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF WS-FUNC NOT = "ADJUST" AND SM-QUANTITY < 0
               MOVE "STK0003" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Only an adjustment may carry a negative "
                      "quantity; the movement type gives the "
                      "direction"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           PERFORM 0250-REASON-REQUIRED.
       0200-EXIT.
           EXIT.

       0250-REASON-REQUIRED SECTION.
       0250-START.
           EVALUATE WS-FUNC
               WHEN "ADJUST"   CONTINUE
               WHEN "SCRAP"    CONTINUE
               WHEN "HOLD"     CONTINUE
               WHEN "RELEASE"  CONTINUE
               WHEN "OPENING"  CONTINUE
               WHEN OTHER      GO TO 0250-EXIT
           END-EVALUATE
           IF SM-REASON = SPACES AND SM-REASON-CODE = SPACES
               MOVE "STK0007" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A " FUNCTION TRIM(WS-FUNC)
                      " must record why it was made"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
           END-IF.
       0250-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 0300  THE ITEM MUST EXIST, BE IN A STATE THAT PERMITS STOCK
      *>       AND BE OF A CLASS THAT HOLDS PHYSICAL STOCK.  THE CLASS
      *>       RULE LIVES IN ITEMVAL SO THERE IS ONE ANSWER.
      *>----------------------------------------------------------------
       0300-READ-ITEM SECTION.
       0300-START.
           INITIALIZE WS-ITEMVAL
           MOVE "CLASSCHK" TO CA-FUNCTION OF WS-ITEMVAL
           MOVE WS-CO TO CA-COMPANY OF WS-ITEMVAL
           MOVE WS-SITE-IN TO CA-SITE OF WS-ITEMVAL
           MOVE WS-USER TO CA-USER OF WS-ITEMVAL
           MOVE WS-BDATE TO CA-BUSINESS-DATE OF WS-ITEMVAL
           MOVE SM-ITEM-CODE TO IV-ITEM-CODE
           MOVE "STOCK" TO IV-ACTION
           CALL "ITEMVAL" USING WS-ITEMVAL
           MOVE IV-ITEM-CLASS TO WS-ITEM-CLASS
           MOVE IV-LIFECYCLE TO WS-LIFECYCLE
           MOVE IV-STOCK-UOM TO WS-STOCK-UOM
           MOVE IV-TRACEABILITY TO WS-TRACE
           IF CA-RETURN-CODE OF WS-ITEMVAL = 16
               MOVE "STK0001" TO WS-REASON
               MOVE CA-MESSAGE OF WS-ITEMVAL TO WS-MSG
               PERFORM 9000-REJECT
               GO TO 0300-EXIT
           END-IF
           IF CA-RETURN-CODE OF WS-ITEMVAL = 12
               MOVE "SQL0001" TO WS-REASON
               MOVE CA-MESSAGE OF WS-ITEMVAL TO WS-MSG
               MOVE 12 TO WS-RC
               MOVE "Y" TO WS-ERROR
               GO TO 0300-EXIT
           END-IF
           IF IV-ALLOWED NOT = "Y"
               MOVE "STK0012" TO WS-REASON
               MOVE CA-MESSAGE OF WS-ITEMVAL TO WS-MSG
               PERFORM 9000-REJECT
               GO TO 0300-EXIT
           END-IF
           IF WS-LIFECYCLE = "DRAFT" OR WS-LIFECYCLE = "REVIEW"
               MOVE "STK0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Item " FUNCTION TRIM(SM-ITEM-CODE)
                      " is still a draft and cannot take stock "
                      "until it has been reviewed and released"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 0300-EXIT
           END-IF
           IF WS-LIFECYCLE = "RETIRED"
               MOVE "STK0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Item " FUNCTION TRIM(SM-ITEM-CODE)
                      " is retired; historical enquiry only"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 0300-EXIT
           END-IF
           IF WS-LIFECYCLE = "BLOCKED" AND WS-FUNC = "RECEIPT"
               MOVE "STK0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Item " FUNCTION TRIM(SM-ITEM-CODE)
                      " is blocked for new supply; an authorised "
                      "resolution is needed"
                   DELIMITED BY SIZE INTO WS-MSG
               MOVE 6 TO WS-RC
               MOVE "Y" TO WS-ERROR
               GO TO 0300-EXIT
           END-IF
           IF SM-REVISION = SPACES
               MOVE IV-CURRENT-REV TO SM-REVISION
           END-IF.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 0400  UNITS.  THE ENTERED QUANTITY IS CONVERTED ONCE AND THE
      *>       FACTOR USED IS KEPT ON THE MOVEMENT, SO A LATER
      *>       PACKAGING CHANGE CANNOT REWRITE WHAT WAS RECEIVED.
      *>----------------------------------------------------------------
       0400-CONVERT-UNITS SECTION.
       0400-START.
           INITIALIZE WS-UOMCONV
           MOVE "TOSTOCK" TO CA-FUNCTION OF WS-UOMCONV
           MOVE WS-CO TO CA-COMPANY OF WS-UOMCONV
           MOVE WS-USER TO CA-USER OF WS-UOMCONV
           MOVE SM-MOVEMENT-DATE TO CA-BUSINESS-DATE OF WS-UOMCONV
           MOVE SM-ITEM-CODE TO UC-ITEM-CODE
           IF SM-UOM-CODE = SPACES
               MOVE WS-STOCK-UOM TO SM-UOM-CODE
           END-IF
           MOVE SM-UOM-CODE TO UC-FROM-UOM
           EVALUATE WS-FUNC
               WHEN "RECEIPT"  MOVE "PURCHASE" TO UC-USAGE-TYPE
               WHEN "RETURN"   MOVE "SALES" TO UC-USAGE-TYPE
               WHEN "ISSUE"    MOVE "ISSUE" TO UC-USAGE-TYPE
               WHEN OTHER      MOVE "STOCK" TO UC-USAGE-TYPE
           END-EVALUATE
           MOVE SM-MOVEMENT-DATE TO UC-EFF-DATE
           COMPUTE UC-ENTERED-QTY = FUNCTION ABS(SM-QUANTITY)
           CALL "UOMCONV" USING WS-UOMCONV
           MOVE UC-FACTOR TO SM-FACTOR
           EVALUATE CA-REASON-CODE OF WS-UOMCONV
               WHEN "UOM0006"  MOVE "STK0004" TO WS-REASON
               WHEN "UOM0003"  MOVE "STK0003" TO WS-REASON
               WHEN "UOM0004"  MOVE "STK0003" TO WS-REASON
               WHEN "UOM0005"  MOVE "STK0003" TO WS-REASON
               WHEN OTHER      MOVE SPACES TO WS-REASON
           END-EVALUATE
           IF CA-RETURN-CODE OF WS-UOMCONV > 4
               IF WS-REASON = SPACES
                   MOVE "STK0003" TO WS-REASON
               END-IF
               MOVE CA-MESSAGE OF WS-UOMCONV TO WS-MSG
               PERFORM 9000-REJECT
               GO TO 0400-EXIT
           END-IF
           MOVE UC-STOCK-QTY TO SM-STOCK-QTY
           IF SM-QUANTITY < 0
               COMPUTE SM-STOCK-QTY = 0 - SM-STOCK-QTY
           END-IF
           IF SM-UNIT-COST NOT = 0
               COMPUTE SM-VALUE-GBP ROUNDED =
                   SM-STOCK-QTY * SM-UNIT-COST
           END-IF.
       0400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 0500  TRACEABILITY.  LOTSER DECIDES WHETHER THE LOT OR SERIAL
      *>       SUPPLIED IS ACCEPTABLE FOR THE DIRECTION OF TRAVEL.
      *>----------------------------------------------------------------
       0500-TRACE-CHECK SECTION.
       0500-START.
           INITIALIZE WS-LOTSER
           MOVE "CHKMOVE" TO CA-FUNCTION OF WS-LOTSER
           MOVE WS-CO TO CA-COMPANY OF WS-LOTSER
           MOVE WS-USER TO CA-USER OF WS-LOTSER
           MOVE SM-MOVEMENT-DATE TO CA-BUSINESS-DATE OF WS-LOTSER
           MOVE SM-ITEM-CODE TO LS-ITEM-CODE
           MOVE SM-REVISION TO LS-REVISION
           MOVE SM-LOT-CODE TO LS-LOT-CODE
           MOVE SM-SERIAL-NO TO LS-SERIAL-NO
           MOVE SM-SITE TO LS-SITE
           MOVE SM-WAREHOUSE TO LS-WAREHOUSE
           MOVE SM-LOCATION TO LS-LOCATION
           MOVE WS-DIRECTION TO LS-DIRECTION
           COMPUTE LS-QUANTITY = FUNCTION ABS(SM-STOCK-QTY)
           MOVE SM-MOVEMENT-DATE TO LS-AS-AT
           CALL "LOTSER" USING WS-LOTSER
           IF CA-RETURN-CODE OF WS-LOTSER > 4
               EVALUATE CA-REASON-CODE OF WS-LOTSER
                   WHEN "LOT0001" MOVE "STK0005" TO WS-REASON
                   WHEN "LOT0002" MOVE "STK0005" TO WS-REASON
                   WHEN "LOT0003" MOVE "STK0004" TO WS-REASON
                   WHEN "LOT0004" MOVE "STK0010" TO WS-REASON
                   WHEN "LOT0005" MOVE "STK0006" TO WS-REASON
                   WHEN "LOT0007" MOVE "STK0011" TO WS-REASON
                   WHEN "LOT0008" MOVE "STK0011" TO WS-REASON
                   WHEN "LOT0009" MOVE "STK0011" TO WS-REASON
                   WHEN OTHER     MOVE "STK0005" TO WS-REASON
               END-EVALUATE
               MOVE CA-MESSAGE OF WS-LOTSER TO WS-MSG
               MOVE CA-RETURN-CODE OF WS-LOTSER TO WS-RC
               MOVE "Y" TO WS-ERROR
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       0500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 0600  REGISTER THE TRACEABILITY IDENTITY ON AN INCREASE SO
      *>       THAT THE LOT OR SERIAL EXISTS BEFORE THE BALANCE DOES.
      *>----------------------------------------------------------------
       0600-REGISTER-TRACE SECTION.
       0600-START.
           IF WS-DIRECTION NOT = "I"
               GO TO 0600-EXIT
           END-IF
           IF WS-TRACE = "LOT" AND SM-LOT-CODE NOT = SPACES
               INITIALIZE WS-LOTSER
               MOVE "NEWLOT" TO CA-FUNCTION OF WS-LOTSER
               MOVE WS-CO TO CA-COMPANY OF WS-LOTSER
               MOVE WS-USER TO CA-USER OF WS-LOTSER
               MOVE SM-MOVEMENT-DATE TO CA-BUSINESS-DATE OF WS-LOTSER
               MOVE SM-ITEM-CODE TO LS-ITEM-CODE
               MOVE SM-REVISION TO LS-REVISION
               MOVE SM-LOT-CODE TO LS-LOT-CODE
               MOVE SM-EXPIRY-DATE TO LS-EXPIRY-DATE
               MOVE SM-MOVEMENT-DATE TO LS-RECEIVED-DATE
               MOVE SM-MOVEMENT-DATE TO LS-AS-AT
               MOVE SM-CONDITION TO LS-CONDITION
               CALL "LOTSER" USING WS-LOTSER
               IF CA-RETURN-CODE OF WS-LOTSER > 4
                   MOVE "STK0005" TO WS-REASON
                   MOVE CA-MESSAGE OF WS-LOTSER TO WS-MSG
                   PERFORM 9000-REJECT
                   GO TO 0600-EXIT
               END-IF
               MOVE LS-EXPIRY-DATE TO SM-EXPIRY-DATE
           END-IF
           IF WS-TRACE = "SERIAL" AND SM-SERIAL-NO NOT = SPACES
               INITIALIZE WS-LOTSER
               MOVE "NEWSER" TO CA-FUNCTION OF WS-LOTSER
               MOVE WS-CO TO CA-COMPANY OF WS-LOTSER
               MOVE WS-USER TO CA-USER OF WS-LOTSER
               MOVE SM-MOVEMENT-DATE TO CA-BUSINESS-DATE OF WS-LOTSER
               MOVE SM-ITEM-CODE TO LS-ITEM-CODE
               MOVE SM-REVISION TO LS-REVISION
               MOVE SM-LOT-CODE TO LS-LOT-CODE
               MOVE SM-SERIAL-NO TO LS-SERIAL-NO
               MOVE WS-POST-SITE TO LS-SITE
               MOVE WS-POST-LOC TO LS-LOCATION
               MOVE "INSTOCK" TO LS-SERIAL-STATUS
               CALL "LOTSER" USING WS-LOTSER
               IF CA-RETURN-CODE OF WS-LOTSER > 4
                   MOVE "STK0011" TO WS-REASON
                   MOVE CA-MESSAGE OF WS-LOTSER TO WS-MSG
                   PERFORM 9000-REJECT
                   GO TO 0600-EXIT
               END-IF
           END-IF.
       0600-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 0700  THE SHARED POSTING CONTRACT.  EVERY LEG OF EVERY
      *>       MOVEMENT TYPE COMES THROUGH HERE.
      *>----------------------------------------------------------------
       0700-POST SECTION.
       0700-START.
           IF WS-ERROR = "Y"
               GO TO 0700-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC inv.usp_PostStockMovement ?, ?, ?, ?, ?, ?, "
                  "?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, "
                  "?, ?, ?, ?, ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 30 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 30
               MOVE SPACES TO SQLIO-PARM-VAL(WS-JX)
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-POST-SITE TO SQLIO-PARM-VAL(2)
           MOVE WS-POST-WHS TO SQLIO-PARM-VAL(3)
           MOVE WS-POST-LOC TO SQLIO-PARM-VAL(4)
           MOVE SM-ITEM-CODE TO SQLIO-PARM-VAL(5)
           MOVE WS-MOVE-TYPE TO SQLIO-PARM-VAL(6)
           COMPUTE WS-QTY = FUNCTION ABS(WS-POST-QTY)
           MOVE WS-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE SM-MOVEMENT-DATE TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(8)
           MOVE WS-USER TO SQLIO-PARM-VAL(9)
           MOVE SM-REVISION TO SQLIO-PARM-VAL(10)
           MOVE SM-LOT-CODE TO SQLIO-PARM-VAL(11)
           MOVE SM-SERIAL-NO TO SQLIO-PARM-VAL(12)
           MOVE SM-OWNER-TYPE TO SQLIO-PARM-VAL(13)
           MOVE SM-OWNER-CODE TO SQLIO-PARM-VAL(14)
           MOVE WS-POST-COND TO SQLIO-PARM-VAL(15)
           MOVE WS-DIRECTION TO SQLIO-PARM-VAL(16)
           MOVE SM-UOM-CODE TO SQLIO-PARM-VAL(17)
           COMPUTE WS-QTY = FUNCTION ABS(SM-QUANTITY)
           MOVE WS-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(18)
           MOVE SM-FACTOR TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(19)
           IF SM-UNIT-COST = 0
               MOVE "Y" TO SQLIO-PARM-NULL(20)
           ELSE
               MOVE SM-UNIT-COST TO SQLIO-NUM-EDIT
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT)
                   TO SQLIO-PARM-VAL(20)
           END-IF
           MOVE SM-POSTING-DATE TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(21)
           MOVE SM-SOURCE-MODULE TO SQLIO-PARM-VAL(22)
           MOVE SM-SOURCE-DOC-TYPE TO SQLIO-PARM-VAL(23)
           MOVE SM-SOURCE-DOC-REF TO SQLIO-PARM-VAL(24)
           IF SM-SOURCE-DOC-LINE = 0
               MOVE "Y" TO SQLIO-PARM-NULL(25)
           ELSE
               MOVE SM-SOURCE-DOC-LINE TO SQLIO-INT-EDIT
               MOVE FUNCTION TRIM(SQLIO-INT-EDIT)
                   TO SQLIO-PARM-VAL(25)
           END-IF
           MOVE WS-POST-COUNTER TO SQLIO-PARM-VAL(26)
           MOVE SM-REASON-CODE TO SQLIO-PARM-VAL(27)
           MOVE SM-REASON TO SQLIO-PARM-VAL(28)
           MOVE SM-APPROVAL-REF TO SQLIO-PARM-VAL(29)
           IF WS-POST-CORR = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(30)
           ELSE
               MOVE WS-POST-CORR TO SQLIO-PARM-VAL(30)
           END-IF
           PERFORM 0750-NULL-BLANKS
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0700-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 0700-EXIT
           END-IF
           IF WS-LEG < 2
               COMPUTE SM-MOVEMENT-ID = FUNCTION NUMVAL(ITM-COL(1))
               COMPUTE SM-QTY-BEFORE = FUNCTION NUMVAL(ITM-COL(5))
               COMPUTE SM-QTY-AFTER = FUNCTION NUMVAL(ITM-COL(6))
           ELSE
               COMPUTE SM-MOVEMENT-ID-2 = FUNCTION NUMVAL(ITM-COL(1))
               COMPUTE SM-QTY-AFTER = FUNCTION NUMVAL(ITM-COL(6))
           END-IF
           IF ITM-COL(10)(1:1) = "1"
               MOVE "Y" TO SM-DUPLICATE
           END-IF
           MOVE WS-MOVE-TYPE TO SM-MOVEMENT-TYPE.
       0700-EXIT.
           EXIT.

       0750-NULL-BLANKS SECTION.
       0750-START.
      *>   optional text parameters go as NULL rather than as spaces
           PERFORM VARYING WS-JX FROM 23 BY 1 UNTIL WS-JX > 29
               IF SQLIO-PARM-VAL(WS-JX) = SPACES
                   MOVE "Y" TO SQLIO-PARM-NULL(WS-JX)
               END-IF
           END-PERFORM
           IF SQLIO-PARM-VAL(26) = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(26)
           END-IF.
       0750-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  THE ONE LEG MOVEMENT TYPES.
      *>----------------------------------------------------------------
       1000-SIMPLE-MOVE SECTION.
       1000-START.
           MOVE 1 TO WS-LEG
           EVALUATE WS-FUNC
               WHEN "RECEIPT"
                   MOVE "RECEIPT" TO WS-MOVE-TYPE
                   MOVE "I" TO WS-DIRECTION
               WHEN "RETURN"
                   MOVE "RETURNIN" TO WS-MOVE-TYPE
                   MOVE "I" TO WS-DIRECTION
               WHEN "COMPLETE"
                   MOVE "COMPLETION" TO WS-MOVE-TYPE
                   MOVE "I" TO WS-DIRECTION
               WHEN "OPENING"
                   MOVE "OPENING" TO WS-MOVE-TYPE
                   MOVE "I" TO WS-DIRECTION
               WHEN "ISSUE"
                   MOVE "ISSUE" TO WS-MOVE-TYPE
                   MOVE "O" TO WS-DIRECTION
               WHEN "SCRAP"
                   MOVE "SCRAP" TO WS-MOVE-TYPE
                   MOVE "O" TO WS-DIRECTION
               WHEN "ADJUST"
                   IF SM-QUANTITY < 0
                       MOVE "ADJUSTDOWN" TO WS-MOVE-TYPE
                       MOVE "O" TO WS-DIRECTION
                   ELSE
                       MOVE "ADJUSTUP" TO WS-MOVE-TYPE
                       MOVE "I" TO WS-DIRECTION
                   END-IF
           END-EVALUATE
           PERFORM 0400-CONVERT-UNITS
           IF WS-ERROR = "Y"
               GO TO 1000-EXIT
           END-IF
           PERFORM 0500-TRACE-CHECK
           IF WS-ERROR = "Y"
               GO TO 1000-EXIT
           END-IF
           MOVE SM-SITE TO WS-POST-SITE
           MOVE SM-WAREHOUSE TO WS-POST-WHS
           MOVE SM-LOCATION TO WS-POST-LOC
           MOVE SM-CONDITION TO WS-POST-COND
           MOVE SM-STOCK-QTY TO WS-POST-QTY
           MOVE SPACES TO WS-POST-COUNTER
           MOVE WS-CORR TO WS-POST-CORR
           PERFORM 0600-REGISTER-TRACE
           PERFORM 0700-POST
           IF WS-ERROR = "Y"
               GO TO 1000-EXIT
           END-IF
           PERFORM 1900-SERIAL-AFTER
           MOVE SM-STOCK-QTY TO WS-QTY-ED
           MOVE SPACES TO WS-MSG
           STRING FUNCTION TRIM(WS-FUNC) " of "
                  FUNCTION TRIM(WS-QTY-ED) " "
                  FUNCTION TRIM(WS-STOCK-UOM) " "
                  FUNCTION TRIM(SM-ITEM-CODE) " at "
                  FUNCTION TRIM(SM-SITE) "/"
                  FUNCTION TRIM(SM-LOCATION)
               DELIMITED BY SIZE INTO WS-MSG.
       1000-EXIT.
           EXIT.

       1900-SERIAL-AFTER SECTION.
       1900-START.
           IF WS-TRACE NOT = "SERIAL" OR SM-SERIAL-NO = SPACES
               GO TO 1900-EXIT
           END-IF
           INITIALIZE WS-LOTSER
           MOVE "SETSTAT" TO CA-FUNCTION OF WS-LOTSER
           MOVE WS-CO TO CA-COMPANY OF WS-LOTSER
           MOVE WS-USER TO CA-USER OF WS-LOTSER
           MOVE SM-ITEM-CODE TO LS-ITEM-CODE
           MOVE SM-SERIAL-NO TO LS-SERIAL-NO
           IF WS-DIRECTION = "I"
               MOVE "INSTOCK" TO LS-SERIAL-STATUS
               MOVE WS-POST-SITE TO LS-SITE
               MOVE WS-POST-LOC TO LS-LOCATION
           ELSE
               EVALUATE WS-MOVE-TYPE
                   WHEN "SCRAP"  MOVE "SCRAPPED" TO LS-SERIAL-STATUS
                   WHEN OTHER    MOVE "ISSUED" TO LS-SERIAL-STATUS
               END-EVALUATE
               MOVE SPACES TO LS-SITE LS-LOCATION
           END-IF
           CALL "LOTSER" USING WS-LOTSER.
       1900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  TRANSFER.  TOTAL QUANTITY, OWNERSHIP AND TRACEABILITY
      *>       ARE PRESERVED.  CR0744: WHEN THE PHYSICAL MOVE IS NOT
      *>       INSTANT THE STOCK GOES TO THE ORIGIN SITE TRANSIT
      *>       LOCATION FIRST, SO IT IS AVAILABLE AT NEITHER ENDPOINT
      *>       UNTIL THE RECEIPT IS CONFIRMED.
      *>----------------------------------------------------------------
       2000-TRANSFER SECTION.
       2000-START.
           IF SM-SITE = SM-TO-SITE
              AND SM-WAREHOUSE = SM-TO-WAREHOUSE
              AND SM-LOCATION = SM-TO-LOCATION
               MOVE "STK0009" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "The origin and the destination of a transfer"
                      " must be different"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           IF SM-TO-LOCATION = SPACES
               MOVE "STK0002" TO WS-REASON
               MOVE "A transfer must name its destination location"
                   TO WS-MSG
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           MOVE "O" TO WS-DIRECTION
           PERFORM 0400-CONVERT-UNITS
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           PERFORM 0500-TRACE-CHECK
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           IF SM-VIA-TRANSIT = "Y"
               PERFORM 2100-FIND-TRANSIT
               IF WS-ERROR = "Y"
                   GO TO 2000-EXIT
               END-IF
           END-IF
      *>   leg one - out of the origin
           MOVE 1 TO WS-LEG
           MOVE "TRANSFEROUT" TO WS-MOVE-TYPE
           MOVE "O" TO WS-DIRECTION
           MOVE SM-SITE TO WS-POST-SITE
           MOVE SM-WAREHOUSE TO WS-POST-WHS
           MOVE SM-LOCATION TO WS-POST-LOC
           MOVE SM-CONDITION TO WS-POST-COND
           MOVE SM-STOCK-QTY TO WS-POST-QTY
           IF SM-VIA-TRANSIT = "Y"
               MOVE SM-TRANSIT-LOC TO WS-POST-COUNTER
           ELSE
               MOVE SM-TO-LOCATION TO WS-POST-COUNTER
           END-IF
           MOVE WS-CORR TO WS-POST-CORR
           PERFORM 0700-POST
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
      *>   leg two - into the transit area or straight into the
      *>   destination.  a different correlation id keeps the two legs
      *>   idempotent on their own.
           MOVE 2 TO WS-LEG
           MOVE "TRANSFERIN" TO WS-MOVE-TYPE
           MOVE "I" TO WS-DIRECTION
           MOVE SM-LOCATION TO WS-POST-COUNTER
           MOVE SPACES TO WS-POST-CORR
           IF WS-CORR NOT = SPACES
               STRING FUNCTION TRIM(WS-CORR) "-IN"
                   DELIMITED BY SIZE INTO WS-POST-CORR
           END-IF
           IF SM-VIA-TRANSIT = "Y"
               MOVE SM-SITE TO WS-POST-SITE
               MOVE SM-WAREHOUSE TO WS-POST-WHS
               MOVE SM-TRANSIT-LOC TO WS-POST-LOC
           ELSE
               MOVE SM-TO-SITE TO WS-POST-SITE
               MOVE SM-TO-WAREHOUSE TO WS-POST-WHS
               MOVE SM-TO-LOCATION TO WS-POST-LOC
           END-IF
           PERFORM 0600-REGISTER-TRACE
           PERFORM 0700-POST
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           PERFORM 1900-SERIAL-AFTER
           MOVE SM-STOCK-QTY TO WS-QTY-ED
           MOVE SPACES TO WS-MSG
           IF SM-VIA-TRANSIT = "Y"
               STRING "Transferred " FUNCTION TRIM(WS-QTY-ED) " "
                      FUNCTION TRIM(SM-ITEM-CODE) " into transit at "
                      FUNCTION TRIM(SM-TRANSIT-LOC)
                      "; confirm the receipt at "
                      FUNCTION TRIM(SM-TO-SITE)
                   DELIMITED BY SIZE INTO WS-MSG
           ELSE
               STRING "Transferred " FUNCTION TRIM(WS-QTY-ED) " "
                      FUNCTION TRIM(SM-ITEM-CODE) " from "
                      FUNCTION TRIM(SM-LOCATION) " to "
                      FUNCTION TRIM(SM-TO-LOCATION)
                   DELIMITED BY SIZE INTO WS-MSG
           END-IF.
       2000-EXIT.
           EXIT.

       2100-FIND-TRANSIT SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 LocationCode, WarehouseCode FROM "
                  "inv.Location WHERE CompanyCode = ? AND SiteCode ="
                  " ? AND LocationType = 'TRANSIT' AND IsActive = 1 "
                  "ORDER BY WarehouseCode, LocationCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE SM-SITE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "STK0013" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Site " FUNCTION TRIM(SM-SITE)
                      " has no transit location, so a staged "
                      "transfer cannot be recorded"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 2100-EXIT
           END-IF
           MOVE ITM-COL(1) TO SM-TRANSIT-LOC
           MOVE ITM-COL(2) TO SM-WAREHOUSE.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  CONFIRM THE PHYSICAL RECEIPT OF AN IN TRANSIT TRANSFER.
      *>----------------------------------------------------------------
       3000-CONFIRM SECTION.
       3000-START.
           PERFORM 2100-FIND-TRANSIT
           IF WS-ERROR = "Y"
               GO TO 3000-EXIT
           END-IF
           IF SM-TO-LOCATION = SPACES
               MOVE "STK0002" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Name the destination location that is "
                      "receiving the transfer"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           MOVE "O" TO WS-DIRECTION
           PERFORM 0400-CONVERT-UNITS
           IF WS-ERROR = "Y"
               GO TO 3000-EXIT
           END-IF
           PERFORM 3100-CHECK-TRANSIT-QTY
           IF WS-ERROR = "Y"
               GO TO 3000-EXIT
           END-IF
           MOVE 1 TO WS-LEG
           MOVE "TRANSFEROUT" TO WS-MOVE-TYPE
           MOVE "O" TO WS-DIRECTION
           MOVE SM-SITE TO WS-POST-SITE
           MOVE SM-WAREHOUSE TO WS-POST-WHS
           MOVE SM-TRANSIT-LOC TO WS-POST-LOC
           MOVE SM-CONDITION TO WS-POST-COND
           MOVE SM-STOCK-QTY TO WS-POST-QTY
           MOVE SM-TO-LOCATION TO WS-POST-COUNTER
           MOVE WS-CORR TO WS-POST-CORR
           PERFORM 0700-POST
           IF WS-ERROR = "Y"
               GO TO 3000-EXIT
           END-IF
           MOVE 2 TO WS-LEG
           MOVE "TRANSFERIN" TO WS-MOVE-TYPE
           MOVE "I" TO WS-DIRECTION
           MOVE SM-TO-SITE TO WS-POST-SITE
           MOVE SM-TO-WAREHOUSE TO WS-POST-WHS
           MOVE SM-TO-LOCATION TO WS-POST-LOC
           MOVE SM-TRANSIT-LOC TO WS-POST-COUNTER
           MOVE SPACES TO WS-POST-CORR
           IF WS-CORR NOT = SPACES
               STRING FUNCTION TRIM(WS-CORR) "-RX"
                   DELIMITED BY SIZE INTO WS-POST-CORR
           END-IF
           PERFORM 0700-POST
           IF WS-ERROR = "Y"
               GO TO 3000-EXIT
           END-IF
           PERFORM 1900-SERIAL-AFTER
           MOVE SM-STOCK-QTY TO WS-QTY-ED
           MOVE SPACES TO WS-MSG
           STRING "Confirmed receipt of " FUNCTION TRIM(WS-QTY-ED)
                  " " FUNCTION TRIM(SM-ITEM-CODE) " at "
                  FUNCTION TRIM(SM-TO-SITE) "/"
                  FUNCTION TRIM(SM-TO-LOCATION)
               DELIMITED BY SIZE INTO WS-MSG.
       3000-EXIT.
           EXIT.

       3100-CHECK-TRANSIT-QTY SECTION.
       3100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(SUM(QtyOnHand),0) FROM "
                  "inv.StockBalance WHERE CompanyCode = ? AND "
                  "ItemCode = ? AND SiteCode = ? AND LocationCode = "
                  "? AND (? = '' OR LotCode = ?) AND (? = '' OR "
                  "SerialNo = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 8 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 8
               MOVE SPACES TO SQLIO-PARM-VAL(WS-JX)
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SM-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE SM-SITE TO SQLIO-PARM-VAL(3)
           MOVE SM-TRANSIT-LOC TO SQLIO-PARM-VAL(4)
           MOVE SM-LOT-CODE TO SQLIO-PARM-VAL(5)
           MOVE SM-LOT-CODE TO SQLIO-PARM-VAL(6)
           MOVE SM-SERIAL-NO TO SQLIO-PARM-VAL(7)
           MOVE SM-SERIAL-NO TO SQLIO-PARM-VAL(8)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3100-EXIT
           END-IF
           COMPUTE WS-TRANSIT-QTY = FUNCTION NUMVAL(ITM-COL(1))
           IF WS-TRANSIT-QTY < SM-STOCK-QTY
               MOVE "STK0006" TO WS-REASON
               MOVE WS-TRANSIT-QTY TO WS-QTY-ED
               MOVE SPACES TO WS-MSG
               STRING "Only " FUNCTION TRIM(WS-QTY-ED)
                      " of this item is in transit at "
                      FUNCTION TRIM(SM-TRANSIT-LOC)
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
           END-IF.
       3100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  QUALITY HOLD (ITM-016).  THE QUANTITY MOVES FROM ONE
      *>       CONDITION TO ANOTHER IN THE SAME PLACE, WHICH CHANGES
      *>       AVAILABILITY WITHOUT DELETING OWNERSHIP OR VALUATION
      *>       HISTORY.  THE HOLD ITSELF RECORDS WHO RAISED IT AND WHY.
      *>----------------------------------------------------------------
       4000-HOLD SECTION.
       4000-START.
           IF SM-TO-CONDITION = SPACES
               MOVE "QUARANTINE" TO SM-TO-CONDITION
           END-IF
           IF SM-TO-CONDITION = SM-CONDITION
               MOVE "STK0014" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A hold must move the stock to a different "
                      "quality condition"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 4000-EXIT
           END-IF
           IF SM-HOLD-TYPE = SPACES
               MOVE "QUARANTINE" TO SM-HOLD-TYPE
           END-IF
           MOVE "O" TO WS-DIRECTION
           PERFORM 0400-CONVERT-UNITS
           IF WS-ERROR = "Y"
               GO TO 4000-EXIT
           END-IF
           MOVE 1 TO WS-LEG
           MOVE "QUALITY" TO WS-MOVE-TYPE
           MOVE "O" TO WS-DIRECTION
           MOVE SM-SITE TO WS-POST-SITE
           MOVE SM-WAREHOUSE TO WS-POST-WHS
           MOVE SM-LOCATION TO WS-POST-LOC
           MOVE SM-CONDITION TO WS-POST-COND
           MOVE SM-STOCK-QTY TO WS-POST-QTY
           MOVE SPACES TO WS-POST-COUNTER
           MOVE WS-CORR TO WS-POST-CORR
           PERFORM 0700-POST
           IF WS-ERROR = "Y"
               GO TO 4000-EXIT
           END-IF
           MOVE 2 TO WS-LEG
           MOVE "I" TO WS-DIRECTION
           MOVE SM-TO-CONDITION TO WS-POST-COND
           MOVE SPACES TO WS-POST-CORR
           IF WS-CORR NOT = SPACES
               STRING FUNCTION TRIM(WS-CORR) "-QH"
                   DELIMITED BY SIZE INTO WS-POST-CORR
           END-IF
           PERFORM 0700-POST
           IF WS-ERROR = "Y"
               GO TO 4000-EXIT
           END-IF
           PERFORM 4100-WRITE-HOLD
           MOVE SM-STOCK-QTY TO WS-QTY-ED
           MOVE SPACES TO WS-MSG
           STRING FUNCTION TRIM(WS-QTY-ED) " of "
                  FUNCTION TRIM(SM-ITEM-CODE) " moved from "
                  FUNCTION TRIM(SM-CONDITION) " to "
                  FUNCTION TRIM(SM-TO-CONDITION)
               DELIMITED BY SIZE INTO WS-MSG.
       4000-EXIT.
           EXIT.

       4100-WRITE-HOLD SECTION.
       4100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT inv.QualityHold (CompanyCode, SiteCode, "
                  "ItemCode, LotCode, SerialNo, LocationCode, "
                  "Quantity, HoldType, ReasonCode, Reason, RaisedBy,"
                  " SourceDocType, SourceDocRef, Status) OUTPUT "
                  "inserted.HoldId VALUES (?, ?, ?, NULLIF(?,''), "
                  "NULLIF(?,''), ?, ?, ?, NULLIF(?,''), ?, ?, "
                  "NULLIF(?,''), NULLIF(?,''), 'OPEN')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 13 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 13
               MOVE SPACES TO SQLIO-PARM-VAL(WS-JX)
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SM-SITE TO SQLIO-PARM-VAL(2)
           MOVE SM-ITEM-CODE TO SQLIO-PARM-VAL(3)
           MOVE SM-LOT-CODE TO SQLIO-PARM-VAL(4)
           MOVE SM-SERIAL-NO TO SQLIO-PARM-VAL(5)
           MOVE SM-LOCATION TO SQLIO-PARM-VAL(6)
           MOVE SM-STOCK-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE SM-HOLD-TYPE TO SQLIO-PARM-VAL(8)
           MOVE SM-REASON-CODE TO SQLIO-PARM-VAL(9)
           MOVE SM-REASON TO SQLIO-PARM-VAL(10)
           MOVE WS-USER TO SQLIO-PARM-VAL(11)
           MOVE SM-SOURCE-DOC-TYPE TO SQLIO-PARM-VAL(12)
           MOVE SM-SOURCE-DOC-REF TO SQLIO-PARM-VAL(13)
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 4100-EXIT
           END-IF
           COMPUTE SM-HOLD-ID = FUNCTION NUMVAL(ITM-COL(1)).
       4100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000  RELEASE A HOLD WITH ITS INSPECTION OUTCOME AND THE
      *>       AUTHORITY THAT RELEASED IT.
      *>----------------------------------------------------------------
       5000-RELEASE-HOLD SECTION.
       5000-START.
           IF SM-HOLD-ID = 0
               MOVE "STK0014" TO WS-REASON
               MOVE "Name the hold that is being released" TO WS-MSG
               PERFORM 9000-REJECT
               GO TO 5000-EXIT
           END-IF
           IF SM-HOLD-OUTCOME = SPACES
               MOVE "ACCEPT" TO SM-HOLD-OUTCOME
           END-IF
           PERFORM 5100-READ-HOLD
           IF WS-ERROR = "Y"
               GO TO 5000-EXIT
           END-IF
           IF SM-TO-CONDITION = SPACES
               EVALUATE SM-HOLD-OUTCOME
                   WHEN "ACCEPT" MOVE "ACCEPTED" TO SM-TO-CONDITION
                   WHEN "REJECT" MOVE "REJECTED" TO SM-TO-CONDITION
                   WHEN "SCRAP"  MOVE "REJECTED" TO SM-TO-CONDITION
                   WHEN OTHER    MOVE "ACCEPTED" TO SM-TO-CONDITION
               END-EVALUATE
           END-IF
           MOVE "O" TO WS-DIRECTION
           PERFORM 0400-CONVERT-UNITS
           IF WS-ERROR = "Y"
               GO TO 5000-EXIT
           END-IF
           MOVE 1 TO WS-LEG
           MOVE "QUALITY" TO WS-MOVE-TYPE
           MOVE "O" TO WS-DIRECTION
           MOVE SM-SITE TO WS-POST-SITE
           MOVE SM-WAREHOUSE TO WS-POST-WHS
           MOVE SM-LOCATION TO WS-POST-LOC
           MOVE SM-CONDITION TO WS-POST-COND
           MOVE SM-STOCK-QTY TO WS-POST-QTY
           MOVE SPACES TO WS-POST-COUNTER
           MOVE WS-CORR TO WS-POST-CORR
           PERFORM 0700-POST
           IF WS-ERROR = "Y"
               GO TO 5000-EXIT
           END-IF
           MOVE 2 TO WS-LEG
           MOVE "I" TO WS-DIRECTION
           MOVE SM-TO-CONDITION TO WS-POST-COND
           MOVE SPACES TO WS-POST-CORR
           IF WS-CORR NOT = SPACES
               STRING FUNCTION TRIM(WS-CORR) "-QR"
                   DELIMITED BY SIZE INTO WS-POST-CORR
           END-IF
           PERFORM 0700-POST
           IF WS-ERROR = "Y"
               GO TO 5000-EXIT
           END-IF
           PERFORM 5200-CLOSE-HOLD
           MOVE SM-STOCK-QTY TO WS-QTY-ED
           MOVE SPACES TO WS-MSG
           STRING "Hold released with outcome "
                  FUNCTION TRIM(SM-HOLD-OUTCOME) " for "
                  FUNCTION TRIM(WS-QTY-ED) " "
                  FUNCTION TRIM(SM-ITEM-CODE)
               DELIMITED BY SIZE INTO WS-MSG.
       5000-EXIT.
           EXIT.

       5100-READ-HOLD SECTION.
       5100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT SiteCode, ItemCode, ISNULL(LotCode,''), "
                  "ISNULL(SerialNo,''), ISNULL(LocationCode,''), "
                  "Quantity, Status, HoldType FROM inv.QualityHold "
                  "WHERE CompanyCode = ? AND HoldId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE SM-HOLD-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 8 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 5100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "STK0014" TO WS-REASON
               MOVE "That quality hold does not exist" TO WS-MSG
               PERFORM 9000-REJECT
               GO TO 5100-EXIT
           END-IF
           IF ITM-COL(7) = "CLOSED"
               MOVE "STK0014" TO WS-REASON
               MOVE "That quality hold has already been released"
                   TO WS-MSG
               PERFORM 9000-REJECT
               GO TO 5100-EXIT
           END-IF
           MOVE ITM-COL(1) TO SM-SITE
           MOVE ITM-COL(2) TO SM-ITEM-CODE
           MOVE ITM-COL(3) TO SM-LOT-CODE
           MOVE ITM-COL(4) TO SM-SERIAL-NO
           MOVE ITM-COL(5) TO SM-LOCATION
           COMPUTE WS-HOLD-QTY = FUNCTION NUMVAL(ITM-COL(6))
           MOVE ITM-COL(8) TO SM-HOLD-TYPE
           IF SM-QUANTITY = 0 OR SM-QUANTITY > WS-HOLD-QTY
               MOVE WS-HOLD-QTY TO SM-QUANTITY
           END-IF
           IF SM-CONDITION = "ACCEPTED"
               EVALUATE SM-HOLD-TYPE
                   WHEN "INSPECTION"
                       MOVE "INSPECTION" TO SM-CONDITION
                   WHEN OTHER
                       MOVE "QUARANTINE" TO SM-CONDITION
               END-EVALUATE
           END-IF.
       5100-EXIT.
           EXIT.

       5200-CLOSE-HOLD SECTION.
       5200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE inv.QualityHold SET Status = 'CLOSED', "
                  "Outcome = ?, OutcomeQty = ?, ReleasedBy = ?, "
                  "ReleasedAt = SYSDATETIME(), AuthorityRef = "
                  "NULLIF(?,'') WHERE CompanyCode = ? AND HoldId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 6
               MOVE SPACES TO SQLIO-PARM-VAL(WS-JX)
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE SM-HOLD-OUTCOME TO SQLIO-PARM-VAL(1)
           MOVE SM-STOCK-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE WS-USER TO SQLIO-PARM-VAL(3)
           MOVE SM-APPROVAL-REF TO SQLIO-PARM-VAL(4)
           MOVE WS-CO TO SQLIO-PARM-VAL(5)
           MOVE SM-HOLD-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 5200-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW.
       5200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE 8 TO WS-RC.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "STK0008" TO WS-REASON
           MOVE SPACES TO WS-MSG
           STRING "STKMOVE does not support function "
                  FUNCTION TRIM(WS-FUNC)
               DELIMITED BY SIZE INTO WS-MSG
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE "Y" TO WS-ERROR WS-SQL-FAILED
           MOVE SQLIO-ERRMSG(1:200) TO WS-MSG
           IF SQLIO-ERRMSG(1:512) = SPACES
               MOVE "SQL failure in STKMOVE" TO WS-MSG
           END-IF
           IF WS-MSG(1:8) = "STERP-E-"
               MOVE 8 TO WS-RC
               IF WS-MSG(9:5) = "0015:"
                   MOVE "STK0006" TO WS-REASON
               ELSE
                   IF WS-MSG(9:5) = "0027:"
                       MOVE "STK0005" TO WS-REASON
                   ELSE
                       IF WS-MSG(9:5) = "0028:"
                           MOVE "STK0004" TO WS-REASON
                       ELSE
                           MOVE "STK0001" TO WS-REASON
                       END-IF
                   END-IF
               END-IF
           ELSE
               MOVE "SQL0001" TO WS-REASON
               MOVE 12 TO WS-RC
           END-IF.
       9300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 9500  COPY THE OUTCOME BACK INTO THE COMMAREA HEADER.
      *>----------------------------------------------------------------
       9500-REPORT SECTION.
       9500-START.
           MOVE WS-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE WS-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE WS-MSG(1:100) TO SM-NARRATIVE
           IF WS-RC > 0
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       9500-EXIT.
           EXIT.
