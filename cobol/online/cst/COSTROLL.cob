      *>================================================================
      *> PROGRAM-ID : COSTROLL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *> PURPOSE    : ROLL UP THE COST OF A MADE ITEM FOR A STATED
      *>              COSTING LOT SIZE.  COSTING DOES NOT EXPLODE
      *>              BILLS OF MATERIAL - ENGINEERING OWNS THE
      *>              REQUIRED QUANTITIES, SO THIS PROGRAM CALLS
      *>              BOMCOST (WHICH USES BOMEXPL AND BOMNET) AND
      *>              CONSUMES THE RESULT.  A SECOND, SUBTLY
      *>              DIFFERENT EXPLOSION IS EXACTLY THE DEFECT THAT
      *>              CST-010 EXISTS TO PREVENT.
      *>
      *>              ONE VALUATION BOUNDARY PER BRANCH: EITHER THE
      *>              SUBASSEMBLY UNIT COST IS TAKEN AND EXPANSION
      *>              STOPS, OR THE SUBASSEMBLY IS EXPANDED AND ITS
      *>              OWN FINISHED UNIT COST IS NOT ADDED AS WELL.
      *> CALLED BY  : STDCOST (SET PREPARATION), WHATIF, THE COST
      *>              ROLL-UP ENQUIRY, THE .NET CLIENT
      *> INTERFACE  : CALL "COSTROLL" USING WS-COSTROLL-CA
      *>              COPYBOOK COSTROLLCA
      *> CALLS      : BOMCOST (BOM MODULE), LABOVH, SQLIO
      *>              (cst.CostSet, cst.StandardCost)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-09-19 DJM  CR0577 ORIGINAL ROLL-UP (SINGLE LEVEL)
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT EFFECTIVE DATES
      *> 2015-07-01 AJS  CR2330 USE THE ENGINEERING EXPLOSION, NOT OURS
      *> 2026-10-06 STERP CST-010 ONE VALUE BOUNDARY PER BRANCH
      *> 2026-10-07 STERP CST-010 SETUP WAS CHARGED TWICE - ONCE INSIDE
      *>                 THE BOMCOST CONVERSION AND ONCE AS THE FIXED
      *>                 ELEMENT.  THE LABOUR ELEMENT NOW EXCLUDES IT.
      *>================================================================
      *> REQ: CST-002, CST-010, CST-024
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COSTROLL.
       AUTHOR. D J MARSH.
       DATE-WRITTEN. 19/09/1997.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY CSTWORK.
       COPY BOMCOSTCA REPLACING ==DFHCOMMAREA== BY ==WS-BC-CA==.
       COPY LABOVHCA  REPLACING ==DFHCOMMAREA== BY ==WS-LO-CA==.
       01  WS-FLAGS.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-NEXT-LINE            PIC S9(9) VALUE 0.
           05  WS-TAKEN                PIC S9(9) VALUE 0.
       LINKAGE SECTION.
       COPY COSTROLLCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION OF DFHCOMMAREA
               WHEN "ROLLUP"  PERFORM 1000-ROLL-UP
               WHEN "GETLINE" PERFORM 2000-GET-LINES
               WHEN "SAVESET" PERFORM 3000-SAVE-SET
               WHEN "EXPLAIN" PERFORM 4000-EXPLAIN
               WHEN OTHER     PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-SQL-FAILED
           MOVE "COSTROLL" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "CST-010" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE 0 TO CR-GROSS-STARTS CR-GOOD-OUTPUT CR-MATERIAL-COST
           MOVE 0 TO CR-LABOUR-COST CR-MACHINE-COST CR-SETUP-COST
           MOVE 0 TO CR-SUBCON-COST CR-OVERHEAD-COST CR-OTHER-COST
           MOVE 0 TO CR-RECOVERY-CREDIT CR-TOTAL-COST CR-UNIT-COST
           MOVE 0 TO CR-BRANCH-VALUE CR-OCC-COUNT CR-MISSING-COUNT
           MOVE 0 TO CR-SET-WRITTEN CR-LINE-CNT WS-TAKEN
           MOVE "N" TO CR-MORE-LINES
           MOVE "COMPLETE" TO CR-COMPLETENESS
           MOVE SPACES TO CR-ERROR-PATH CR-WARNING
           MOVE FUNCTION UPPER-CASE(CR-ITEM-CODE) TO CR-ITEM-CODE
           MOVE FUNCTION UPPER-CASE(CR-BOUNDARY) TO CR-BOUNDARY
           IF CR-BOUNDARY = SPACES
               MOVE "EXPAND" TO CR-BOUNDARY
           END-IF
           IF CR-CURRENCY = SPACES
               MOVE "GBP" TO CR-CURRENCY
           END-IF
           IF CR-LOT-SIZE = 0
               MOVE 1 TO CR-LOT-SIZE
           END-IF
           IF CR-EFF-DATE = 0
               MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO CR-EFF-DATE
           END-IF
           IF CR-COST-DATE = 0
               MOVE CR-EFF-DATE TO CR-COST-DATE
           END-IF
           IF CR-WIN-MAX <= 0 OR CR-WIN-MAX > 20
               MOVE 20 TO CR-WIN-MAX
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY OF DFHCOMMAREA = SPACES
              OR CR-ITEM-CODE = SPACES
               MOVE "CST0601" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A company and an item code are required"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CR-LOT-SIZE <= 0
               MOVE "CST0602" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "The costing lot size must be positive - a "
                      "fixed setup charge needs a denominator"
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CR-BOUNDARY NOT = "EXPAND" AND CR-BOUNDARY NOT = "STOP"
               MOVE "CST0606" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "The valuation boundary must be EXPAND or STOP"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  THE ROLL-UP.  BOMCOST DOES THE EXPLOSION AND THE
      *>       MATERIAL VALUATION; THIS PROGRAM ADDS CONVERSION AND
      *>       OVERHEAD AND DIVIDES BY THE PLANNED GOOD OUTPUT.
      *>----------------------------------------------------------------
       1000-ROLL-UP SECTION.
       1000-START.
           PERFORM 1100-CALL-BOMCOST
           IF CA-RETURN-CODE OF DFHCOMMAREA > 4
               GO TO 1000-EXIT
           END-IF
           PERFORM 1200-CONVERSION-COST
           PERFORM 1300-TOTALS
           PERFORM 2000-GET-LINES.
       1000-EXIT.
           EXIT.

       1100-CALL-BOMCOST SECTION.
       1100-START.
           INITIALIZE WS-BC-CA
           MOVE "BOMCOST" TO CA-PROGRAM OF WS-BC-CA
           MOVE "COST" TO CA-FUNCTION OF WS-BC-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-BC-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-BC-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-BC-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-BC-CA
           MOVE CR-ITEM-CODE TO BC-PARENT-ITEM
           MOVE CR-REVISION TO BC-PARENT-REV
      *>   THE BILL PURPOSES ARE THE ENGINEERING ONES - STANDARD,
      *>   JOB AND ORDER.  COSTING IS A USE OF A BILL, NOT A PURPOSE
      *>   OF ONE, AND ASKING FOR IT MADE BOMSELCT REFUSE EVERY
      *>   SELECTION (BOM0208).  THE ROLL-UP CONSUMES THE STANDARD
      *>   ENGINEERING BILL (CST-010).
           MOVE "STANDARD" TO BC-PURPOSE
           MOVE CR-LOT-SIZE TO BC-REQ-QTY
           MOVE CR-EFF-DATE TO BC-EFF-DATE
           MOVE CR-COST-DATE TO BC-COST-DATE
           MOVE CR-COST-SET TO BC-COST-SET
           MOVE CR-CURRENCY TO BC-CURRENCY
           MOVE CR-NETTED TO BC-NETTED
           IF CR-STOP-AT-SUB
               MOVE 1 TO BC-MAX-DEPTH
           ELSE
               MOVE CR-MAX-DEPTH TO BC-MAX-DEPTH
           END-IF
           CALL "BOMCOST" USING WS-BC-CA
           MOVE BC-ERROR-PATH TO CR-ERROR-PATH
           IF CA-RETURN-CODE OF WS-BC-CA > 4
               MOVE "CST0603" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-BC-CA
                   TO CA-MESSAGE OF DFHCOMMAREA
               MOVE CA-RETURN-CODE OF WS-BC-CA
                   TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "INCOMPLETE" TO CR-COMPLETENESS
               GO TO 1100-EXIT
           END-IF
           MOVE BC-GROSS-STARTS TO CR-GROSS-STARTS
           MOVE BC-MATERIAL-COST TO CR-MATERIAL-COST
           MOVE BC-SERVICE-COST TO CR-SUBCON-COST
           MOVE BC-SETUP-COST TO CR-SETUP-COST
           MOVE BC-RECOVERY-CREDIT TO CR-RECOVERY-CREDIT
           MOVE BC-OCC-COUNT TO CR-OCC-COUNT
           MOVE BC-MISSING-RATES TO CR-MISSING-COUNT
           MOVE BC-UNIT-DENOM TO CR-GOOD-OUTPUT
           MOVE BC-TOTAL-COST TO CR-BRANCH-VALUE
           IF BC-COMPLETENESS NOT = "COMPLETE"
               MOVE BC-COMPLETENESS TO CR-COMPLETENESS
           END-IF
           IF CR-GOOD-OUTPUT <= 0
               MOVE CR-LOT-SIZE TO CR-GOOD-OUTPUT
           END-IF
           IF CR-MISSING-COUNT > 0
               MOVE "INCOMPLETE" TO CR-COMPLETENESS
               MOVE "CST0604" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "Some components have no cost in this set"
                   TO CR-WARNING
               IF CA-RETURN-CODE OF DFHCOMMAREA = 0
                   MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               END-IF
           END-IF.
       1100-EXIT.
           EXIT.

      *>  CONVERSION.  BOMCOST HAS ALREADY PRICED THE RESOURCE LINES
      *>  OF THE BILL; A WORK CENTRE NAMED ON THE REQUEST ADDS THE
      *>  SITE BURDEN ON TOP, ON A BASE THAT HOLDS NO OVERHEAD.
       1200-CONVERSION-COST SECTION.
       1200-START.
      *>  BC-CONVERSION-COST ALREADY CONTAINS THE SETUP PER RUN THAT
      *>  BC-SETUP-COST REPORTS SEPARATELY, AND 1300-TOTALS ADDS THE
      *>  SETUP AS A FIXED ELEMENT IN ITS OWN RIGHT.  THE LABOUR
      *>  ELEMENT THEREFORE CARRIES THE CONVERSION WITHOUT IT, OR THE
      *>  PARENT BEARS THE SAME FIXED CHARGE TWICE (CST-010).
           COMPUTE CR-LABOUR-COST =
               BC-CONVERSION-COST - BC-SETUP-COST
           IF CR-WORK-CENTRE = SPACES
               GO TO 1200-EXIT
           END-IF
           INITIALIZE WS-LO-CA
           MOVE "LABOVH" TO CA-PROGRAM OF WS-LO-CA
           MOVE "OVERHEAD" TO CA-FUNCTION OF WS-LO-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-LO-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-LO-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-LO-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-LO-CA
           MOVE CR-WORK-CENTRE TO LO-WORK-CENTRE
           MOVE CR-EFF-DATE TO LO-EFF-DATE
           MOVE CR-ITEM-CODE TO LO-ITEM-CODE
           MOVE CR-GROSS-STARTS TO LO-GROSS-UNITS
           MOVE 1 TO LO-RUN-COUNT
           MOVE CR-LABOUR-COST TO LO-BASE-COST
           MOVE "Y" TO LO-BASE-EXCL-OVHD
           CALL "LABOVH" USING WS-LO-CA
           IF CA-RETURN-CODE OF WS-LO-CA > 4
               MOVE "CST0605" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-LO-CA TO CR-WARNING
               MOVE "INCOMPLETE" TO CR-COMPLETENESS
               IF CA-RETURN-CODE OF DFHCOMMAREA = 0
                   MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               END-IF
               GO TO 1200-EXIT
           END-IF
           MOVE LO-OVHD-COST TO CR-OVERHEAD-COST
           ADD LO-MACHINE-COST TO CR-MACHINE-COST.
       1200-EXIT.
           EXIT.

      *>  THE EQUATION OF SECTION 5 OF THE SPECIFICATION.  THE
      *>  BY-PRODUCT RECOVERY IS SUBTRACTED AS A CREDIT, NEVER ADDED
      *>  AS A NEGATIVE COMPONENT CONSUMPTION.
       1300-TOTALS SECTION.
       1300-START.
           COMPUTE CR-TOTAL-COST = CR-MATERIAL-COST
                                   + CR-LABOUR-COST
                                   + CR-MACHINE-COST
                                   + CR-SETUP-COST
                                   + CR-SUBCON-COST
                                   + CR-OVERHEAD-COST
                                   + CR-OTHER-COST
                                   - CR-RECOVERY-CREDIT
           IF CR-TOTAL-COST < 0
               MOVE "CST0609" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "The recovery credit drives the cost below "
                      "zero - the controller must review it"
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               MOVE 6 TO CA-RETURN-CODE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF
           IF CR-GOOD-OUTPUT > 0
               COMPUTE CR-UNIT-COST ROUNDED =
                   CR-TOTAL-COST / CR-GOOD-OUTPUT
           END-IF
           MOVE CR-TOTAL-COST TO CR-BRANCH-VALUE.
       1300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  PAGE THE COSTED TREE.  CR-L-CONTRIB SAYS WHETHER THE
      *>       LINE CONTRIBUTED TO THE TOTAL OR IS SHOWN FOR
      *>       INFORMATION ONLY - THE TWO VIEWS MAY BOTH BE SEEN BUT
      *>       ONLY ONE IS COUNTED (CST-010).
      *>----------------------------------------------------------------
       2000-GET-LINES SECTION.
       2000-START.
           MOVE 0 TO CR-LINE-CNT
           MOVE "N" TO CR-MORE-LINES
           MOVE CR-FROM-LINE TO BC-FROM-LINE
           MOVE 25 TO BC-WIN-MAX
           MOVE "GETLINE" TO CA-FUNCTION OF WS-BC-CA
           CALL "BOMCOST" USING WS-BC-CA
           IF CA-RETURN-CODE OF WS-BC-CA > 4
               GO TO 2000-EXIT
           END-IF
           MOVE BC-MORE-LINES TO CR-MORE-LINES
           PERFORM VARYING WS-IX FROM 1 BY 1
               UNTIL WS-IX > BC-WIN-CNT OR CR-LINE-CNT >= CR-WIN-MAX
               ADD 1 TO CR-LINE-CNT
               MOVE CR-LINE-CNT TO WS-IX2
               MOVE BC-L-SEQ(WS-IX) TO CR-L-SEQ(WS-IX2)
               MOVE BC-L-DEPTH(WS-IX) TO CR-L-DEPTH(WS-IX2)
               MOVE BC-L-ITEM(WS-IX) TO CR-L-ITEM(WS-IX2)
               MOVE BC-L-REV(WS-IX) TO CR-L-REV(WS-IX2)
               MOVE BC-L-DESC(WS-IX) TO CR-L-DESC(WS-IX2)
               MOVE BC-L-ELEMENT(WS-IX) TO CR-L-ELEMENT(WS-IX2)
               MOVE BC-L-SOURCE(WS-IX) TO CR-L-SOURCE(WS-IX2)
               MOVE BC-L-REQ-QTY(WS-IX) TO CR-L-REQ-QTY(WS-IX2)
               MOVE BC-L-UNIT-COST(WS-IX) TO CR-L-UNIT-COST(WS-IX2)
               MOVE BC-L-EXT-COST(WS-IX) TO CR-L-EXT-COST(WS-IX2)
               MOVE BC-L-WARNING(WS-IX) TO CR-L-WARNING(WS-IX2)
               PERFORM 2100-BOUNDARY-FLAG
           END-PERFORM
           IF BC-WIN-CNT > CR-WIN-MAX
               MOVE "Y" TO CR-MORE-LINES
           END-IF.
       2000-EXIT.
           EXIT.

       2100-BOUNDARY-FLAG SECTION.
       2100-START.
           IF BC-L-BRANCH(WS-IX) NOT = 0
               MOVE "BRANCH" TO CR-L-BOUNDARY(WS-IX2)
           ELSE
               MOVE "LEAF" TO CR-L-BOUNDARY(WS-IX2)
           END-IF
           IF CR-STOP-AT-SUB AND BC-L-DEPTH(WS-IX) > 1
               MOVE "N" TO CR-L-CONTRIB(WS-IX2)
               MOVE "STOPPED" TO CR-L-BOUNDARY(WS-IX2)
           ELSE
               IF BC-L-BRANCH(WS-IX) NOT = 0
                  AND BC-L-EXT-COST(WS-IX) = 0
                   MOVE "N" TO CR-L-CONTRIB(WS-IX2)
               ELSE
                   MOVE "Y" TO CR-L-CONTRIB(WS-IX2)
               END-IF
           END-IF.
       2100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  WRITE THE ROLLED COST INTO A DRAFT COST SET.  ONLY A
      *>       DRAFT MAY BE WRITTEN TO - AN APPROVED SET KEEPS ITS
      *>       ASSUMPTIONS FOR GOOD (CST-012).
      *>----------------------------------------------------------------
       3000-SAVE-SET SECTION.
       3000-START.
           IF CR-TARGET-SET = SPACES
               MOVE "CST0607" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A target cost set is required"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status FROM cst.CostSet WHERE "
                  "CompanyCode = ? AND CostSetCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE CR-TARGET-SET TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-SQL-BAD
               GO TO 3000-EXIT
           END-IF
           IF NOT WS-GOT-ROW OR WS-COL(1)(1:5) NOT = "DRAFT"
               MOVE "CST0607" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "The target cost set must exist and be a draft"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           PERFORM 1000-ROLL-UP
           IF CA-RETURN-CODE OF DFHCOMMAREA > 6
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM cst.StandardCostElement WHERE "
                  "StandardCostId IN (SELECT StandardCostId FROM "
                  "cst.StandardCost WHERE CompanyCode = ? AND "
                  "CostSetCode = ? AND ItemCode = ? AND Revision = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 3100-BIND-KEY
           PERFORM 8100-EXECUTE
           IF WS-SQL-BAD
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM cst.StandardCost WHERE CompanyCode "
                  "= ? AND CostSetCode = ? AND ItemCode = ? AND "
                  "Revision = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 3100-BIND-KEY
           PERFORM 8100-EXECUTE
           IF WS-SQL-BAD
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO cst.StandardCost (CompanyCode, "
                  "CostSetCode, ItemCode, Revision, CostingLotSize, "
                  "TotalUnitCost, Completeness, Warnings) VALUES "
                  "(?, ?, ?, ?, ?, ?, ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 3100-BIND-KEY
           MOVE 8 TO SQLIO-PARM-CNT
           MOVE CR-LOT-SIZE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE CR-UNIT-COST TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE CR-COMPLETENESS TO SQLIO-PARM-VAL(7)
           MOVE CR-WARNING TO SQLIO-PARM-VAL(8)
           MOVE "N" TO SQLIO-PARM-NULL(5)
           MOVE "N" TO SQLIO-PARM-NULL(6)
           MOVE "N" TO SQLIO-PARM-NULL(7)
           MOVE "N" TO SQLIO-PARM-NULL(8)
           PERFORM 8100-EXECUTE
           IF NOT WS-SQL-BAD
               MOVE 1 TO CR-SET-WRITTEN
           END-IF.
       3000-EXIT.
           EXIT.

       3100-BIND-KEY SECTION.
       3100-START.
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE CR-TARGET-SET TO SQLIO-PARM-VAL(2)
           MOVE CR-ITEM-CODE TO SQLIO-PARM-VAL(3)
           MOVE CR-REVISION TO SQLIO-PARM-VAL(4)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE "N" TO SQLIO-PARM-NULL(4).
       3100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  CST-024.  WHAT THE ANSWER IS MADE OF AND WHAT IS
      *>       MISSING FROM IT.  A ZERO IS NEVER PASSED OFF AS A COST.
      *>----------------------------------------------------------------
       4000-EXPLAIN SECTION.
       4000-START.
           MOVE SPACES TO CR-WARNING
           IF CR-COMPLETENESS = "COMPLETE"
               STRING "Rolled on " FUNCTION TRIM(CR-COST-SET)
                      " with boundary " FUNCTION TRIM(CR-BOUNDARY)
                   DELIMITED BY SIZE INTO CR-WARNING
           ELSE
               MOVE CR-MISSING-COUNT TO WS-INT-ED
               STRING "Incomplete - " FUNCTION TRIM(WS-INT-ED)
                      " missing inputs were valued at zero and "
                      "flagged"
                   DELIMITED BY SIZE INTO CR-WARNING
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
           END-IF.
       4000-EXIT.
           EXIT.

       8000-SINGLE-ROW SECTION.
       8000-START.
           MOVE "N" TO WS-ROW-FOUND
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           EVALUATE TRUE
               WHEN SQLIO-SQLCODE = 0
                   MOVE "Y" TO WS-ROW-FOUND
               WHEN SQLIO-SQLCODE = 100
                   CONTINUE
               WHEN OTHER
                   PERFORM 8900-SQL-ERROR
           END-EVALUATE.
       8000-EXIT.
           EXIT.

       8100-EXECUTE SECTION.
       8100-START.
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           IF SQLIO-SQLCODE < 0
               PERFORM 8900-SQL-ERROR
           ELSE
               MOVE 1 TO SQLIO-CURSOR
               MOVE "CLOS" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           END-IF.
       8100-EXIT.
           EXIT.

       8900-SQL-ERROR SECTION.
       8900-START.
           MOVE "Y" TO WS-SQL-FAILED
           MOVE "CST9001" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SQLIO-ERRMSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE 12 TO CA-RETURN-CODE OF DFHCOMMAREA.
       8900-EXIT.
           EXIT.

       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "CST0608" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           STRING "Function is not supported by COSTROLL"
               DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
