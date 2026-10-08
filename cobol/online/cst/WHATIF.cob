      *>================================================================
      *> PROGRAM-ID : WHATIF
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *> PURPOSE    : SCENARIO COSTING.  A BASE CASE AND UP TO FIVE
      *>              VARIANTS ARE PRICED THROUGH COSTROLL SO THE
      *>              PLANNER CAN SEE WHAT A DIFFERENT LOT SIZE, COST
      *>              SET, WORK CENTRE OR SUPPLIER PRICE WOULD DO.
      *>              A SCENARIO IS A QUESTION, NOT AN INSTRUCTION -
      *>              NOTHING IS WRITTEN, NO STOCK IS RESERVED, NO KIT
      *>              IS RELEASED AND NO STANDARD IS ACTIVATED
      *>              (CST-023).  COSTROLL IS ASKED FOR "ROLLUP" ONLY;
      *>              "SAVESET" IS NEVER ISSUED FROM HERE.
      *> CALLED BY  : THE COSTING CLIENT (ROLL-UP AND WHAT-IF SCREEN)
      *> INTERFACE  : CALL "WHATIF" USING WS-WHATIF-CA
      *>              COPYBOOK WHATIFCA
      *> CALLS      : COSTROLL (ROLLUP), MARGIN (EXPECTED)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1999-02-15 DJM  CR0733 ORIGINAL WHAT-IF COSTING
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT EFFECTIVE DATES
      *> 2026-10-06 STERP CST-023 NO LIVE SIDE EFFECTS
      *>================================================================
      *> REQ: CST-023
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WHATIF.
       AUTHOR. D J MORRIS.
       DATE-WRITTEN. 15/02/1999.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY CSTWORK.
       COPY COSTROLLCA REPLACING ==DFHCOMMAREA== BY ==WS-ROLL-CA==.
       COPY MARGINCA REPLACING ==DFHCOMMAREA== BY ==WS-MARGIN-CA==.
       01  WS-FLAGS.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-V                    PIC S9(4) VALUE 0.
           05  WS-BEST-UNIT            PIC S9(11)V9(6) VALUE 0.
           05  WS-RUN-SET              PIC X(20) VALUE SPACES.
           05  WS-RUN-LOT              PIC S9(13)V9(6) VALUE 0.
           05  WS-RUN-CTR              PIC X(20) VALUE SPACES.
           05  WS-RUN-BOUND            PIC X(10) VALUE SPACES.
           05  WS-MAT-FACTOR           PIC S9(5)V9(4) VALUE 1.
           05  WS-RATE-FACTOR          PIC S9(5)V9(4) VALUE 1.
           05  WS-CONVERSION           PIC S9(13)V9(2) VALUE 0.
           05  WS-OVERHEAD             PIC S9(13)V9(2) VALUE 0.
           05  WS-MATERIAL             PIC S9(13)V9(2) VALUE 0.
           05  WS-TOTAL                PIC S9(13)V9(2) VALUE 0.
           05  WS-UNIT                 PIC S9(11)V9(6) VALUE 0.
       LINKAGE SECTION.
       COPY WHATIFCA.
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
               WHEN "SCENARIO" PERFORM 1000-SCENARIO
               WHEN "COMPARE"  PERFORM 2000-COMPARE
               WHEN "MARGINWI" PERFORM 3000-MARGINWI
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR
           MOVE "WHATIF" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "CST-023" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE SPACES TO WI-WARNING
           MOVE 0 TO WI-BASE-TOTAL WI-BASE-UNIT WI-BEST-VARIANT
           MOVE SPACES TO WI-BASE-COMPLETE
      *>   a scenario never has side effects.  the flag is an output
      *>   the caller can show on the screen, not a choice.
           MOVE "N" TO WI-SIDE-EFFECTS
           IF WI-BASE-LOT-SIZE = 0
               MOVE 1 TO WI-BASE-LOT-SIZE
           END-IF
           IF WI-VARIANT-CNT < 0
               MOVE 0 TO WI-VARIANT-CNT
           END-IF.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY OF DFHCOMMAREA = SPACES
              OR WI-ITEM-CODE = SPACES
               MOVE "CST1201" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A company and an item are required"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF WI-VARIANT-CNT > 6
               MOVE "CST1205" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A scenario carries at most six variants"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF WI-BASE-LOT-SIZE <= 0
               MOVE "CST1203" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "The lot size must be positive"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF (CA-FUNCTION OF DFHCOMMAREA = "COMPARE"
               OR CA-FUNCTION OF DFHCOMMAREA = "MARGINWI")
              AND WI-VARIANT-CNT < 1
               MOVE "CST1202" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "At least one variant is required to compare"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  THE BASE CASE, THEN EACH VARIANT.  EVERY RUN IS A FRESH
      *>       ROLL-UP - NO VARIANT INHERITS ANOTHER'S ANSWER.
      *>----------------------------------------------------------------
       1000-SCENARIO SECTION.
       1000-START.
           PERFORM 1100-RUN-BASE
           IF WI-VARIANT-CNT > 0
               PERFORM VARYING WS-V FROM 1 BY 1
                       UNTIL WS-V > WI-VARIANT-CNT
                   PERFORM 1200-RUN-VARIANT
               END-PERFORM
           END-IF.
       1000-EXIT.
           EXIT.

       1100-RUN-BASE SECTION.
       1100-START.
           MOVE WI-BASE-COST-SET TO WS-RUN-SET
           MOVE WI-BASE-LOT-SIZE TO WS-RUN-LOT
           MOVE WI-BASE-WORK-CTR TO WS-RUN-CTR
           MOVE "EXPAND" TO WS-RUN-BOUND
           MOVE 1 TO WS-MAT-FACTOR WS-RATE-FACTOR
           PERFORM 4000-ROLL
           MOVE WS-TOTAL TO WI-BASE-TOTAL
           MOVE WS-UNIT TO WI-BASE-UNIT
           MOVE CR-COMPLETENESS TO WI-BASE-COMPLETE
           IF CR-COMPLETENESS NOT = "COMPLETE"
               MOVE "CST1204" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WI-WARNING
               STRING "Base case is "
                      FUNCTION TRIM(CR-COMPLETENESS)
                      " - compare the variants with that in mind"
                   DELIMITED BY SIZE INTO WI-WARNING
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
           END-IF.
       1100-EXIT.
           EXIT.

      *>   a variant is the base case with one or more knobs turned.
      *>   material and rate factors are applied to the rolled result
      *>   rather than to the stored data, because the stored data is
      *>   not ours to change in a what-if (CST-023).
       1200-RUN-VARIANT SECTION.
       1200-START.
           MOVE WI-BASE-COST-SET TO WS-RUN-SET
           MOVE WI-BASE-LOT-SIZE TO WS-RUN-LOT
           MOVE WI-BASE-WORK-CTR TO WS-RUN-CTR
           MOVE "EXPAND" TO WS-RUN-BOUND
           MOVE 1 TO WS-MAT-FACTOR WS-RATE-FACTOR
           IF WI-V-COST-SET(WS-V) NOT = SPACES
               MOVE WI-V-COST-SET(WS-V) TO WS-RUN-SET
           END-IF
           IF WI-V-LOT-SIZE(WS-V) > 0
               MOVE WI-V-LOT-SIZE(WS-V) TO WS-RUN-LOT
           END-IF
           IF WI-V-WORK-CTR(WS-V) NOT = SPACES
               MOVE WI-V-WORK-CTR(WS-V) TO WS-RUN-CTR
           END-IF
           IF WI-V-BOUNDARY(WS-V) NOT = SPACES
               MOVE WI-V-BOUNDARY(WS-V) TO WS-RUN-BOUND
           END-IF
           IF WI-V-MAT-FACTOR(WS-V) NOT = 0
               MOVE WI-V-MAT-FACTOR(WS-V) TO WS-MAT-FACTOR
           END-IF
           IF WI-V-RATE-FACTR(WS-V) NOT = 0
               MOVE WI-V-RATE-FACTR(WS-V) TO WS-RATE-FACTOR
           END-IF
           IF WI-V-LABEL(WS-V) = SPACES
               MOVE WS-V TO WS-INT-ED
               MOVE SPACES TO WI-V-LABEL(WS-V)
               STRING "Variant " FUNCTION TRIM(WS-INT-ED)
                   DELIMITED BY SIZE INTO WI-V-LABEL(WS-V)
           END-IF
           PERFORM 4000-ROLL
           MOVE WS-MATERIAL TO WI-V-MATERIAL(WS-V)
           MOVE WS-CONVERSION TO WI-V-CONVERSION(WS-V)
           MOVE WS-OVERHEAD TO WI-V-OVERHEAD(WS-V)
           MOVE WS-TOTAL TO WI-V-TOTAL(WS-V)
           MOVE WS-UNIT TO WI-V-UNIT(WS-V)
           MOVE CR-COMPLETENESS TO WI-V-COMPLETE(WS-V)
           MOVE CR-WARNING TO WI-V-WARNING(WS-V)
           COMPUTE WI-V-DIFF(WS-V) = WS-TOTAL - WI-BASE-TOTAL
           IF WI-BASE-TOTAL NOT = 0
               COMPUTE WI-V-DIFF-PCT(WS-V) ROUNDED =
                   (WS-TOTAL - WI-BASE-TOTAL) * 100 / WI-BASE-TOTAL
           ELSE
               MOVE 0 TO WI-V-DIFF-PCT(WS-V)
           END-IF
           MOVE 0 TO WI-V-MARGIN-PCT(WS-V).
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  COMPARISON.  THE CHEAPEST COMPLETE VARIANT IS NAMED.  AN
      *>       INCOMPLETE ROLL-UP IS NEVER DECLARED THE WINNER - A
      *>       MISSING COMPONENT ALWAYS LOOKS CHEAP.
      *>----------------------------------------------------------------
       2000-COMPARE SECTION.
       2000-START.
           PERFORM 1000-SCENARIO
           MOVE 0 TO WI-BEST-VARIANT
           MOVE 0 TO WS-BEST-UNIT
           PERFORM VARYING WS-V FROM 1 BY 1
                   UNTIL WS-V > WI-VARIANT-CNT
               IF WI-V-COMPLETE(WS-V) = "COMPLETE"
                  AND WI-V-UNIT(WS-V) > 0
                   IF WI-BEST-VARIANT = 0
                      OR WI-V-UNIT(WS-V) < WS-BEST-UNIT
                       MOVE WS-V TO WI-BEST-VARIANT
                       MOVE WI-V-UNIT(WS-V) TO WS-BEST-UNIT
                   END-IF
               END-IF
           END-PERFORM
           IF WI-BEST-VARIANT = 0
               MOVE "No complete variant to recommend"
                   TO WI-WARNING
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
           END-IF.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  CST-013 AND CST-023 TOGETHER.  EACH SCENARIO UNIT COST
      *>       IS PUT AGAINST THE TARGET SELLING PRICE THROUGH MARGIN,
      *>       SO THE SCENARIO IS JUDGED ON THE SAME MARGIN RULES AS A
      *>       REAL ORDER - EX VAT, AND UNDEFINED WHEN THERE IS NO
      *>       REVENUE TO DIVIDE BY.
      *>----------------------------------------------------------------
       3000-MARGINWI SECTION.
       3000-START.
           PERFORM 2000-COMPARE
           IF WI-TARGET-PRICE = 0
               MOVE "No target price - margin is undefined"
                   TO WI-WARNING
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               GO TO 3000-EXIT
           END-IF
           PERFORM VARYING WS-V FROM 1 BY 1
                   UNTIL WS-V > WI-VARIANT-CNT
               PERFORM 3100-ONE-MARGIN
           END-PERFORM.
       3000-EXIT.
           EXIT.

       3100-ONE-MARGIN SECTION.
       3100-START.
           INITIALIZE WS-MARGIN-CA
           MOVE "EXPECTED" TO CA-FUNCTION OF WS-MARGIN-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-MARGIN-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-MARGIN-CA
           MOVE "SCENARIO" TO MG-DOC-TYPE
           MOVE WI-SCENARIO-REF TO MG-DOC-REF
           MOVE WS-V TO MG-LINE-NUM
           MOVE WI-ITEM-CODE TO MG-ITEM-CODE
           MOVE 1 TO MG-QUANTITY
           MOVE WI-TARGET-PRICE TO MG-UNIT-PRICE
           MOVE "N" TO MG-REVENUE-INCL-VAT
           MOVE "QUOTE" TO MG-COST-BASIS
           MOVE WI-V-UNIT(WS-V) TO MG-UNIT-COST
           MOVE WI-V-UNIT(WS-V) TO MG-COST-AMOUNT
           CALL "MARGIN" USING WS-MARGIN-CA
           IF CA-RETURN-CODE OF WS-MARGIN-CA > 4
               MOVE 0 TO WI-V-MARGIN-PCT(WS-V)
               MOVE "Margin could not be computed"
                   TO WI-V-WARNING(WS-V)
           ELSE
               IF MG-MARGIN-STATE = "OK"
                   MOVE MG-MARGIN-PCT TO WI-V-MARGIN-PCT(WS-V)
               ELSE
                   MOVE 0 TO WI-V-MARGIN-PCT(WS-V)
                   MOVE "Margin undefined at this price"
                       TO WI-V-WARNING(WS-V)
               END-IF
           END-IF.
       3100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  ONE ROLL-UP.  "ROLLUP" ONLY - CR-TARGET-SET IS CLEARED
      *>       SO EVEN A CARELESS CALLER CANNOT HAVE A SCENARIO LAND IN
      *>       A COST SET.  THAT IS THE WHOLE POINT OF CST-023.
      *>----------------------------------------------------------------
       4000-ROLL SECTION.
       4000-START.
           INITIALIZE WS-ROLL-CA
           MOVE "ROLLUP" TO CA-FUNCTION OF WS-ROLL-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-ROLL-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-ROLL-CA
           MOVE WI-ITEM-CODE TO CR-ITEM-CODE
           MOVE WI-REVISION TO CR-REVISION
           MOVE WS-RUN-SET TO CR-COST-SET
           MOVE SPACES TO CR-TARGET-SET
           MOVE WI-EFF-DATE TO CR-EFF-DATE
           MOVE WI-EFF-DATE TO CR-COST-DATE
           MOVE WS-RUN-LOT TO CR-LOT-SIZE
           MOVE WS-RUN-BOUND TO CR-BOUNDARY
           MOVE WS-RUN-CTR TO CR-WORK-CENTRE
           MOVE "GBP" TO CR-CURRENCY
           MOVE 1 TO CR-FROM-LINE
           MOVE 20 TO CR-WIN-MAX
           CALL "COSTROLL" USING WS-ROLL-CA
           MOVE CR-MATERIAL-COST TO WS-MATERIAL
           COMPUTE WS-CONVERSION = CR-LABOUR-COST + CR-MACHINE-COST
               + CR-SETUP-COST + CR-SUBCON-COST
           COMPUTE WS-OVERHEAD =
               CR-OVERHEAD-COST + CR-RECOVERY-CREDIT
           IF WS-MAT-FACTOR NOT = 1
               COMPUTE WS-MATERIAL ROUNDED =
                   WS-MATERIAL * WS-MAT-FACTOR
           END-IF
           IF WS-RATE-FACTOR NOT = 1
               COMPUTE WS-CONVERSION ROUNDED =
                   WS-CONVERSION * WS-RATE-FACTOR
               COMPUTE WS-OVERHEAD ROUNDED =
                   WS-OVERHEAD * WS-RATE-FACTOR
           END-IF
           COMPUTE WS-TOTAL = WS-MATERIAL + WS-CONVERSION
               + WS-OVERHEAD + CR-OTHER-COST
           IF CR-GOOD-OUTPUT > 0
               COMPUTE WS-UNIT ROUNDED = WS-TOTAL / CR-GOOD-OUTPUT
           ELSE
               IF WS-RUN-LOT > 0
                   COMPUTE WS-UNIT ROUNDED = WS-TOTAL / WS-RUN-LOT
               ELSE
                   MOVE 0 TO WS-UNIT
               END-IF
           END-IF
           IF CA-RETURN-CODE OF WS-ROLL-CA > 4
               MOVE "FAILED" TO CR-COMPLETENESS
               MOVE CA-MESSAGE OF WS-ROLL-CA TO CR-WARNING
           END-IF.
       4000-EXIT.
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
           MOVE "CST1206" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           STRING "Function " FUNCTION TRIM(CA-FUNCTION OF DFHCOMMAREA)
                  " is not supported by WHATIF"
               DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
