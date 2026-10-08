      *>================================================================
      *> PROGRAM-ID : LABOVH
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *> PURPOSE    : LABOUR, MACHINE AND OVERHEAD COST.  SETUP IS
      *>              CHARGED PER RUN, NOT PER UNIT.  OVERHEAD IS
      *>              ABSORBED ON A RATE PER DIRECT LABOUR OR MACHINE
      *>              HOUR DERIVED FROM BUDGETED PRODUCTION OVERHEAD
      *>              OVER HOURS AT NORMAL CAPACITY (D-05).  THE
      *>              PERCENTAGE BASE EXCLUDES OVERHEAD ITSELF SO THE
      *>              CALCULATION CANNOT BECOME CIRCULAR, AND FIXED
      *>              OVERHEAD NOT RECOVERED BECAUSE PRODUCTION WAS
      *>              BELOW NORMAL CAPACITY IS A PERIOD EXPENSE.
      *> CALLED BY  : COSTROLL, JOBCOST, WHATIF, THE .NET CLIENT
      *> INTERFACE  : CALL "LABOVH" USING WS-LABOVH-CA
      *>              COPYBOOK LABOVHCA
      *> CALLS      : SQLIO (cst.WorkCentre)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-08-04 DJM  CR0560 ORIGINAL LABOUR AND BURDEN
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT EFFECTIVE DATES
      *> 2010-03-22 RJH  CR1755 NORMAL CAPACITY BASIS FOR FIXED BURDEN
      *> 2026-10-06 STERP CST-011 CST-026 NON CIRCULAR BASE
      *>================================================================
      *> REQ: CST-011, CST-026
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LABOVH.
       AUTHOR. D J MARSH.
       DATE-WRITTEN. 04/08/1997.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY CSTWORK.
       01  WS-FLAGS.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-HAVE-CENTRE          PIC X VALUE "N".
               88  WS-CENTRE-FOUND               VALUE "Y".
           05  WS-BASE                 PIC S9(13)V9(2) VALUE 0.
           05  WS-RUNS                 PIC S9(9) VALUE 0.
       LINKAGE SECTION.
       COPY LABOVHCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "RATES"     PERFORM 1000-READ-CENTRE
               WHEN "LABOUR"    PERFORM 2000-LABOUR
               WHEN "OVERHEAD"  PERFORM 3000-OVERHEAD
               WHEN "ABSORB"    PERFORM 4000-ABSORPTION
               WHEN "OVHRATE"   PERFORM 5000-RECOVERY-RATE
               WHEN OTHER       PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-SQL-FAILED WS-HAVE-CENTRE
           MOVE "LABOVH" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "CST-011" TO CA-REQ-REF
           MOVE 0 TO LO-SETUP-COST LO-RUN-COST LO-LABOUR-COST
           MOVE 0 TO LO-MACHINE-COST LO-OVHD-COST LO-TOTAL-COST
           MOVE 0 TO LO-UNIT-COST LO-TOTAL-HOURS LO-DERIVED-RATE
           MOVE 0 TO LO-ABSORBED-GBP LO-RECOVERY-GBP LO-CAPACITY-PCT
           MOVE "N" TO LO-UNDER-RECOVERED
           MOVE SPACES TO LO-WARNING LO-BASIS-TEXT
           MOVE 0 TO WS-BASE
           MOVE FUNCTION UPPER-CASE(LO-WORK-CENTRE) TO LO-WORK-CENTRE
           IF LO-EFF-DATE = 0
               MOVE CA-BUSINESS-DATE TO LO-EFF-DATE
           END-IF
           IF LO-RUN-COUNT <= 0
               MOVE 1 TO LO-RUN-COUNT
           END-IF
           MOVE LO-RUN-COUNT TO WS-RUNS
           IF LO-RATE-BASIS = SPACES
               MOVE "UNIT" TO LO-RATE-BASIS
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES
               MOVE "CST0501" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF LO-SETUP-HOURS < 0 OR LO-RUN-HOURS-UNIT < 0
              OR LO-MACHINE-HRS-UNIT < 0
               MOVE "CST0503" TO CA-REASON-CODE
               MOVE "Hours may not be negative" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF LO-RUN-COUNT <= 0
               MOVE "CST0504" TO CA-REASON-CODE
               MOVE "The execution run count must be positive"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  THE EFFECTIVE WORK CENTRE RATES.  THE LATEST ROW ON OR
      *>       BEFORE THE EFFECTIVE DATE WINS.
      *>----------------------------------------------------------------
       1000-READ-CENTRE SECTION.
       1000-START.
           IF LO-WORK-CENTRE = SPACES
               GO TO 1000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 SetupRatePerHour, RunRatePerHour, "
                  "MachineRatePerHour, OverheadRatePerHour, "
                  "OverheadBasis, ISNULL(NormalCapacityHours,0) "
                  "FROM cst.WorkCentre WHERE CompanyCode = ? AND "
                  "WorkCentreCode = ? AND EffectiveFrom <= ? AND "
                  "IsActive = 1 ORDER BY EffectiveFrom DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE LO-WORK-CENTRE TO SQLIO-PARM-VAL(2)
           MOVE LO-EFF-DATE TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 6 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-SQL-BAD
               GO TO 1000-EXIT
           END-IF
           IF NOT WS-GOT-ROW
               MOVE "CST0502" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Work centre " FUNCTION TRIM(LO-WORK-CENTRE)
                      " has no rates effective at that date"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               GO TO 1000-EXIT
           END-IF
           MOVE "Y" TO WS-HAVE-CENTRE
           IF LO-SETUP-RATE = 0
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO LO-SETUP-RATE
           END-IF
           IF LO-RUN-RATE = 0
               MOVE FUNCTION NUMVAL(WS-COL(2)) TO LO-RUN-RATE
           END-IF
           IF LO-MACHINE-RATE = 0
               MOVE FUNCTION NUMVAL(WS-COL(3)) TO LO-MACHINE-RATE
           END-IF
           IF LO-OVHD-RATE = 0
               MOVE FUNCTION NUMVAL(WS-COL(4)) TO LO-OVHD-RATE
           END-IF
           IF LO-OVHD-BASIS = SPACES
               MOVE WS-COL(5) TO LO-OVHD-BASIS
           END-IF
           IF LO-NORMAL-CAP-HRS = 0
               MOVE FUNCTION NUMVAL(WS-COL(6)) TO LO-NORMAL-CAP-HRS
           END-IF.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  CST-011.  SETUP x RUNS PLUS RUN HOURS x GROSS UNITS.
      *>       A PER BATCH RATE IS CHARGED PER BATCH, WHICH IS WHY THE
      *>       BASIS IS CARRIED EXPLICITLY RATHER THAN GUESSED.
      *>----------------------------------------------------------------
       2000-LABOUR SECTION.
       2000-START.
           PERFORM 1000-READ-CENTRE
           IF CA-RETURN-CODE > 4
               GO TO 2000-EXIT
           END-IF
           COMPUTE WS-CALC-1 = LO-SETUP-HOURS * WS-RUNS
           COMPUTE LO-SETUP-COST ROUNDED = WS-CALC-1 * LO-SETUP-RATE
           EVALUATE LO-RATE-BASIS
               WHEN "BATCH"
                   IF LO-BATCH-COUNT <= 0
                       MOVE 1 TO LO-BATCH-COUNT
                   END-IF
                   COMPUTE WS-CALC-2 =
                       LO-RUN-HOURS-UNIT * LO-BATCH-COUNT
                   MOVE "Run rate applied per batch" TO LO-BASIS-TEXT
               WHEN "RUN"
                   COMPUTE WS-CALC-2 = LO-RUN-HOURS-UNIT * WS-RUNS
                   MOVE "Run rate applied per run" TO LO-BASIS-TEXT
               WHEN OTHER
                   COMPUTE WS-CALC-2 =
                       LO-RUN-HOURS-UNIT * LO-GROSS-UNITS
                   MOVE "Run rate applied per gross unit"
                       TO LO-BASIS-TEXT
           END-EVALUATE
           COMPUTE LO-RUN-COST ROUNDED = WS-CALC-2 * LO-RUN-RATE
           COMPUTE LO-LABOUR-COST = LO-SETUP-COST + LO-RUN-COST
           COMPUTE WS-CALC-3 =
               LO-MACHINE-HRS-UNIT * LO-GROSS-UNITS
           COMPUTE LO-MACHINE-COST ROUNDED =
               WS-CALC-3 * LO-MACHINE-RATE
           COMPUTE LO-TOTAL-HOURS = WS-CALC-1 + WS-CALC-2
           COMPUTE LO-TOTAL-COST =
               LO-LABOUR-COST + LO-MACHINE-COST
           IF LO-GROSS-UNITS > 0
               COMPUTE LO-UNIT-COST ROUNDED =
                   LO-TOTAL-COST / LO-GROSS-UNITS
           END-IF
           IF LO-SETUP-COST NOT = 0 AND LO-GROSS-UNITS <= 0
               MOVE "A setup charge needs a stated lot size"
                   TO LO-WARNING
               MOVE 4 TO CA-RETURN-CODE
           END-IF.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  OVERHEAD.  THE BASE NEVER CONTAINS OVERHEAD, SO THE
      *>       PERCENTAGE CANNOT FEED ON ITSELF (CST-011).
      *>----------------------------------------------------------------
       3000-OVERHEAD SECTION.
       3000-START.
           PERFORM 1000-READ-CENTRE
           IF CA-RETURN-CODE > 4
               GO TO 3000-EXIT
           END-IF
           EVALUATE LO-OVHD-BASIS
               WHEN "LABOURHR"
                   IF LO-TOTAL-HOURS = 0
                       COMPUTE LO-TOTAL-HOURS =
                           (LO-SETUP-HOURS * WS-RUNS)
                           + (LO-RUN-HOURS-UNIT * LO-GROSS-UNITS)
                   END-IF
                   COMPUTE LO-OVHD-COST ROUNDED =
                       LO-TOTAL-HOURS * LO-OVHD-RATE
                   MOVE "Rate per direct labour hour at normal "
                       TO LO-BASIS-TEXT
                   STRING FUNCTION TRIM(LO-BASIS-TEXT) "capacity"
                       DELIMITED BY SIZE INTO WS-WORK-TEXT
                   MOVE WS-WORK-TEXT TO LO-BASIS-TEXT
               WHEN "MACHINEHR"
                   COMPUTE WS-CALC-3 =
                       LO-MACHINE-HRS-UNIT * LO-GROSS-UNITS
                   COMPUTE LO-OVHD-COST ROUNDED =
                       WS-CALC-3 * LO-OVHD-RATE
                   MOVE WS-CALC-3 TO LO-TOTAL-HOURS
                   MOVE "Rate per machine hour at normal capacity"
                       TO LO-BASIS-TEXT
               WHEN "PCTCOST"
                   MOVE LO-BASE-COST TO WS-BASE
                   IF LO-EXCLUDE-OVHD
                       COMPUTE WS-BASE =
                           LO-BASE-COST - LO-EMBEDDED-OVHD
                   END-IF
                   IF WS-BASE < 0
                       MOVE "CST0507" TO CA-REASON-CODE
                       MOVE "The overhead base is negative once "
                           TO CA-MESSAGE
                       STRING FUNCTION TRIM(CA-MESSAGE)
                              "embedded overhead is removed"
                           DELIMITED BY SIZE INTO WS-WORK-TEXT
                       MOVE WS-WORK-TEXT TO CA-MESSAGE
                       PERFORM 9000-REJECT
                       GO TO 3000-EXIT
                   END-IF
                   COMPUTE LO-OVHD-COST ROUNDED =
                       WS-BASE * LO-OVHD-PCT / 100
                   MOVE "Percentage of a base that excludes overhead"
                       TO LO-BASIS-TEXT
               WHEN OTHER
                   MOVE "CST0506" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Overhead basis "
                          FUNCTION TRIM(LO-OVHD-BASIS)
                          " is not recognised"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   PERFORM 9000-REJECT
                   GO TO 3000-EXIT
           END-EVALUATE
           COMPUTE LO-TOTAL-COST = LO-LABOUR-COST + LO-MACHINE-COST
                                   + LO-OVHD-COST
           IF LO-GROSS-UNITS > 0
               COMPUTE LO-UNIT-COST ROUNDED =
                   LO-TOTAL-COST / LO-GROSS-UNITS
           END-IF
           PERFORM 4000-ABSORPTION.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  CST-026.  OVERHEAD IS ABSORBED ON NORMAL CAPACITY.
      *>       WHEN THE PLANT RUNS BELOW NORMAL CAPACITY THE SHORTFALL
      *>       IS A PERIOD EXPENSE - IT IS NOT ADDED TO STOCK VALUE.
      *>----------------------------------------------------------------
       4000-ABSORPTION SECTION.
       4000-START.
           IF LO-NORMAL-CAP-HRS <= 0
               IF CA-FUNCTION = "ABSORB"
                   MOVE "CST0505" TO CA-REASON-CODE
                   MOVE "Normal capacity hours must be positive"
                       TO CA-MESSAGE
                   PERFORM 9000-REJECT
               END-IF
               GO TO 4000-EXIT
           END-IF
           IF LO-ACTUAL-HOURS = 0
               MOVE LO-TOTAL-HOURS TO LO-ACTUAL-HOURS
           END-IF
           COMPUTE LO-CAPACITY-PCT ROUNDED =
               LO-ACTUAL-HOURS * 100 / LO-NORMAL-CAP-HRS
           COMPUTE LO-ABSORBED-GBP ROUNDED =
               LO-ACTUAL-HOURS * LO-OVHD-RATE
           IF LO-INCURRED-GBP = 0
               COMPUTE LO-INCURRED-GBP ROUNDED =
                   LO-NORMAL-CAP-HRS * LO-OVHD-RATE
           END-IF
           COMPUTE LO-RECOVERY-GBP =
               LO-ABSORBED-GBP - LO-INCURRED-GBP
           IF LO-RECOVERY-GBP < 0
               MOVE "Y" TO LO-UNDER-RECOVERED
               MOVE "Under recovered overhead is a period expense"
                   TO LO-WARNING
               IF CA-RETURN-CODE = 0
                   MOVE 4 TO CA-RETURN-CODE
                   MOVE "CST0026" TO CA-REASON-CODE
               END-IF
           END-IF.
       4000-EXIT.
           EXIT.

      *>  BUDGETED PRODUCTION OVERHEAD DIVIDED BY HOURS AT NORMAL
      *>  CAPACITY - THE RATE MUST STATE ITS DENOMINATOR (D-05).
       5000-RECOVERY-RATE SECTION.
       5000-START.
           IF LO-NORMAL-CAP-HRS <= 0
               MOVE "CST0505" TO CA-REASON-CODE
               MOVE "Normal capacity hours must be positive"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 5000-EXIT
           END-IF
           COMPUTE LO-DERIVED-RATE ROUNDED =
               LO-BUDGET-OVHD-GBP / LO-NORMAL-CAP-HRS
           MOVE LO-DERIVED-RATE TO LO-OVHD-RATE
           MOVE SPACES TO LO-BASIS-TEXT
           MOVE LO-NORMAL-CAP-HRS TO WS-QTY-ED
           STRING "Budgeted overhead over "
                  FUNCTION TRIM(WS-QTY-ED)
                  " normal capacity hours"
               DELIMITED BY SIZE INTO LO-BASIS-TEXT.
       5000-EXIT.
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
                   MOVE "Y" TO WS-SQL-FAILED
                   MOVE "CST9001" TO CA-REASON-CODE
                   MOVE SQLIO-ERRMSG TO CA-MESSAGE
                   MOVE 12 TO CA-RETURN-CODE
           END-EVALUATE.
       8000-EXIT.
           EXIT.

       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "CST0508" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                  " is not supported by LABOVH"
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
