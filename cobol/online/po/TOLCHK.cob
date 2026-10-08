      *>================================================================
      *> PROGRAM-ID : TOLCHK
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : EVALUATE OVER AND UNDER DELIVERY, PRICE, DATE AND
      *>              CHARGE EXCEPTIONS AGAINST THE APPROVED TOLERANCES
      *>              OF D-08.  AN OVER RECEIPT BEYOND THE APPROVED
      *>              TOLERANCE REQUIRES A CHANGED ORDER AND A NEW
      *>              APPROVAL BEFORE CONFIRMATION; A RECEIPT WITHIN
      *>              TOLERANCE STILL INCREASES THE ACTUAL RECEIVED
      *>              QUANTITY AND THE ACCRUAL - IT IS NEVER
      *>              FINANCIALLY INVISIBLE (PO-011).
      *> CALLED BY  : RECEIPT, INSPECT, APMATCH (price), LANDCOST
      *> INTERFACE  : CALL "TOLCHK" USING DFHCOMMAREA  (TOLCHKCA)
      *> CALLS      : SQLIO -> po.ToleranceRule, DATECALC
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-06-18 RJH  CR0815 GOODS INWARD TOLERANCES
      *> 2003-09-22 DJM  CR1101 PRICE TOLERANCE NEEDS BOTH TESTS
      *> 2026-10-06 STERP D-08 BASELINE, WORKING DAY EARLY TEST
      *>================================================================
      *> REQ: PO-011, PO-022, D-08
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TOLCHK.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 18/06/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY POACCNTS.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATECALC-CA==
                      LEADING ==CA-== BY ==DH-==.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-RULE-ROW.
           05  WS-R-SCOPE              PIC X(10).
           05  WS-R-SCOPE-CODE         PIC X(40).
           05  WS-R-OVER-PCT           PIC X(16).
           05  WS-R-OVER-QTY           PIC X(24).
           05  WS-R-UNDER-PCT          PIC X(16).
           05  WS-R-PRICE-PCT          PIC X(16).
           05  WS-R-PRICE-ABS          PIC X(24).
           05  WS-R-CHARGE-ABS         PIC X(24).
           05  WS-R-EARLY-DAYS         PIC X(8).
           05  WS-R-LATE-DAYS          PIC X(8).
           05  WS-R-ACTION             PIC X(6).
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(40).
       01  WS-WORK.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-APPROVED-QTY         PIC S9(13)V9(6) VALUE 0.
           05  WS-AFTER-QTY            PIC S9(13)V9(6) VALUE 0.
           05  WS-ALLOW-PCT-QTY        PIC S9(13)V9(6) VALUE 0.
           05  WS-ALLOW-QTY            PIC S9(13)V9(6) VALUE 0.
           05  WS-ABS-VAR              PIC S9(13)V99 VALUE 0.
           05  WS-ABS-PCT              PIC S9(7)V9(4) VALUE 0.
           05  WS-PCT-FAIL             PIC X VALUE "N".
           05  WS-ABS-FAIL             PIC X VALUE "N".
           05  WS-QTY-ED               PIC -(9)9.999999.
           05  WS-AMT-ED               PIC -(11)9.99.
           05  WS-PCT-ED               PIC -(5)9.99.
           05  WS-DAY-ED               PIC -(4)9.
           05  WS-STEP-DATE            PIC 9(8) VALUE 0.
           05  WS-STEP-CNT             PIC S9(4) VALUE 0.
       LINKAGE SECTION.
       COPY TOLCHKCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           IF TC-RULE-LOOKUP
               PERFORM 1500-LOAD-RULE THRU 1500-EXIT
           END-IF
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "QTY"
                   PERFORM 2000-QTY THRU 2000-EXIT
               WHEN "PRICE"
                   PERFORM 3000-PRICE THRU 3000-EXIT
               WHEN "DATE"
                   PERFORM 4000-DATE THRU 4000-EXIT
               WHEN "CHARGE"
                   PERFORM 5000-CHARGE THRU 5000-EXIT
               WHEN OTHER
                   MOVE "TOL0008" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                          " is not supported by TOLCHK"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE 8 TO CA-RETURN-CODE
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE "N" TO WS-FATAL-SW
           MOVE "TOLCHK" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE 0 TO TC-PERMITTED-QTY TC-OUTSTANDING-QTY
           MOVE 0 TO TC-OVER-QTY TC-EXCESS-QTY
           MOVE 0 TO TC-VARIANCE-AMT TC-VARIANCE-PCT
           MOVE 0 TO TC-DAYS-EARLY TC-DAYS-LATE TC-WORK-DAYS-EARLY
           MOVE "WITHIN" TO TC-OUTCOME
           MOVE "N" TO TC-NEEDS-AMEND
           IF TC-RULE-SOURCE = SPACES
               MOVE "PASSED" TO TC-RULE-SOURCE
           END-IF
           IF TC-RULE-PASSED
               MOVE "CALLER" TO TC-RULE-SCOPE
               MOVE SPACES TO TC-RULE-SCOPE-CODE
               IF TC-PRICE-PCT-LIM = 0
                   MOVE PO-POL-PRICE-PCT TO TC-PRICE-PCT-LIM
               END-IF
               IF TC-PRICE-ABS-LIM = 0
                   MOVE PO-POL-PRICE-ABS TO TC-PRICE-ABS-LIM
               END-IF
               IF TC-CHARGE-ABS-LIM = 0
                   MOVE PO-POL-CHARGE-ABS TO TC-CHARGE-ABS-LIM
               END-IF
               IF TC-EARLY-DAY-LIM = 0
                   MOVE PO-POL-EARLY-DAYS TO TC-EARLY-DAY-LIM
               END-IF
               IF TC-ACTION-OVER = SPACES
                   MOVE "BLOCK" TO TC-ACTION-OVER
               END-IF
           END-IF
      *>     An item or supplier override above 5% is never allowed.
           IF TC-OVER-PCT > PO-POL-OVER-MAX-PCT
               MOVE PO-POL-OVER-MAX-PCT TO TC-OVER-PCT
               MOVE "TOL0009" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The over delivery override has been capped at "
                      "the policy maximum of 5%"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 4 TO CA-RETURN-CODE
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE MOST SPECIFIC SCOPE WINS: ITEM, THEN SUPPLIER, THEN THE
      *> COMPANY DEFAULT.
      *>----------------------------------------------------------------
       1500-LOAD-RULE SECTION.
       1500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 t.ScopeType, t.ScopeCode, "
                  "CAST(t.OverDeliveryPct AS varchar(16)), "
                  "CAST(t.OverDeliveryQty AS varchar(24)), "
                  "CAST(t.UnderDeliveryPct AS varchar(16)), "
                  "CAST(t.PriceVariancePct AS varchar(16)), "
                  "CAST(t.PriceVarianceAbs AS varchar(24)), "
                  "CAST(t.ChargeVarianceAbs AS varchar(24)), "
                  "CAST(t.EarlyDeliveryDays AS varchar(8)), "
                  "CAST(t.LateDeliveryDays AS varchar(8)), "
                  "t.ActionOverDelivery FROM po.ToleranceRule t "
                  "WHERE t.CompanyCode = ? AND t.IsActive = 1 AND "
                  "((t.ScopeType = 'ITEM' AND t.ScopeCode = ?) OR "
                  "(t.ScopeType = 'SUPPLIER' AND t.ScopeCode = ?) OR "
                  "(t.ScopeType = 'COMPANY')) ORDER BY CASE "
                  "t.ScopeType WHEN 'ITEM' THEN 1 WHEN 'SUPPLIER' "
                  "THEN 2 ELSE 3 END"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE TC-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE TC-SUPPLIER TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE 11 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 16 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 16 TO SQLIO-COL-WIDTH(5)
           MOVE 16 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 24 TO SQLIO-COL-WIDTH(8)
           MOVE 8 TO SQLIO-COL-WIDTH(9)
           MOVE 8 TO SQLIO-COL-WIDTH(10)
           MOVE 6 TO SQLIO-COL-WIDTH(11)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-RULE-ROW
           IF SQLIO-ERROR
               MOVE 12 TO CA-RETURN-CODE
               MOVE "SQL0001" TO CA-REASON-CODE
               MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 1500-EXIT
           END-IF
           IF SQLIO-NOTFOUND
      *>         No rule row: fall back to the compiled D-08 baseline.
               MOVE "BASELINE" TO TC-RULE-SCOPE
               MOVE SPACES TO TC-RULE-SCOPE-CODE
               MOVE PO-POL-OVER-PCT TO TC-OVER-PCT
               MOVE 0 TO TC-OVER-QTY-ALLOW
               MOVE PO-POL-PRICE-PCT TO TC-PRICE-PCT-LIM
               MOVE PO-POL-PRICE-ABS TO TC-PRICE-ABS-LIM
               MOVE PO-POL-CHARGE-ABS TO TC-CHARGE-ABS-LIM
               MOVE PO-POL-EARLY-DAYS TO TC-EARLY-DAY-LIM
               MOVE 0 TO TC-LATE-DAY-LIM
               MOVE "BLOCK" TO TC-ACTION-OVER
               GO TO 1500-EXIT
           END-IF
           MOVE WS-R-SCOPE TO TC-RULE-SCOPE
           MOVE WS-R-SCOPE-CODE TO TC-RULE-SCOPE-CODE
           COMPUTE TC-OVER-PCT = FUNCTION NUMVAL(WS-R-OVER-PCT)
           COMPUTE TC-OVER-QTY-ALLOW = FUNCTION NUMVAL(WS-R-OVER-QTY)
           COMPUTE TC-UNDER-PCT = FUNCTION NUMVAL(WS-R-UNDER-PCT)
           COMPUTE TC-PRICE-PCT-LIM = FUNCTION NUMVAL(WS-R-PRICE-PCT)
           COMPUTE TC-PRICE-ABS-LIM = FUNCTION NUMVAL(WS-R-PRICE-ABS)
           COMPUTE TC-CHARGE-ABS-LIM =
                   FUNCTION NUMVAL(WS-R-CHARGE-ABS)
           COMPUTE TC-EARLY-DAY-LIM = FUNCTION NUMVAL(WS-R-EARLY-DAYS)
           COMPUTE TC-LATE-DAY-LIM = FUNCTION NUMVAL(WS-R-LATE-DAYS)
           MOVE WS-R-ACTION TO TC-ACTION-OVER
           IF TC-OVER-PCT > PO-POL-OVER-MAX-PCT
               MOVE PO-POL-OVER-MAX-PCT TO TC-OVER-PCT
           END-IF.
       1500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> QUANTITY TEST.  AC-053: ORDER 30, RECEIVE 20 - TEN REMAIN
      *> OUTSTANDING.  A RECEIPT EXACTLY AT THE APPROVED LIMIT IS
      *> ACCEPTED UNDER THE APPROVED AUTHORITY; ONE UNIT ABOVE IT
      *> BLOCKS PENDING AN AMENDMENT.
      *>----------------------------------------------------------------
       2000-QTY SECTION.
       2000-START.
           IF TC-ORDERED-QTY <= 0
               MOVE "TOL0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The ordered quantity must be greater than zero"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 8 TO CA-RETURN-CODE
               MOVE "BLOCK" TO TC-OUTCOME
               GO TO 2000-EXIT
           END-IF
           COMPUTE WS-APPROVED-QTY =
                   TC-ORDERED-QTY - TC-CANCELLED-QTY
           IF WS-APPROVED-QTY < 0
               MOVE 0 TO WS-APPROVED-QTY
           END-IF
           COMPUTE WS-ALLOW-PCT-QTY ROUNDED =
                   WS-APPROVED-QTY * TC-OVER-PCT / 100
           MOVE WS-ALLOW-PCT-QTY TO WS-ALLOW-QTY
           IF TC-OVER-QTY-ALLOW > WS-ALLOW-QTY
               MOVE TC-OVER-QTY-ALLOW TO WS-ALLOW-QTY
           END-IF
           COMPUTE TC-PERMITTED-QTY = WS-APPROVED-QTY + WS-ALLOW-QTY
           COMPUTE WS-AFTER-QTY = TC-COUNTED-QTY + TC-PROPOSED-QTY
           COMPUTE TC-OUTSTANDING-QTY = WS-APPROVED-QTY - WS-AFTER-QTY
           IF TC-OUTSTANDING-QTY < 0
               MOVE 0 TO TC-OUTSTANDING-QTY
           END-IF
           COMPUTE TC-OVER-QTY = WS-AFTER-QTY - WS-APPROVED-QTY
           IF TC-OVER-QTY < 0
               MOVE 0 TO TC-OVER-QTY
           END-IF
           COMPUTE TC-EXCESS-QTY = WS-AFTER-QTY - TC-PERMITTED-QTY
           IF TC-EXCESS-QTY < 0
               MOVE 0 TO TC-EXCESS-QTY
           END-IF
           EVALUATE TRUE
               WHEN TC-EXCESS-QTY > 0
                   MOVE "BLOCK" TO TC-OUTCOME
                   MOVE "Y" TO TC-NEEDS-AMEND
                   MOVE "TOL0002" TO CA-REASON-CODE
                   MOVE TC-EXCESS-QTY TO WS-QTY-ED
                   MOVE SPACES TO CA-MESSAGE
                   STRING "The receipt is " FUNCTION TRIM(WS-QTY-ED)
                          " above the approved over delivery limit; "
                          "an approved order amendment is required "
                          "before it can be confirmed"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   IF TC-ACTION-OVER = "WARN"
                       MOVE 4 TO CA-RETURN-CODE
                       MOVE "WARN" TO TC-OUTCOME
                   ELSE
                       MOVE 6 TO CA-RETURN-CODE
                   END-IF
               WHEN WS-AFTER-QTY = TC-PERMITTED-QTY
                    AND TC-OVER-QTY > 0
                   MOVE "ATLIMIT" TO TC-OUTCOME
                   MOVE "TOL0003" TO CA-REASON-CODE
                   MOVE TC-OVER-QTY TO WS-QTY-ED
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Receipt accepted at the approved over "
                          "delivery limit; " FUNCTION TRIM(WS-QTY-ED)
                          " excess units are accrued and separately "
                          "visible"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE 4 TO CA-RETURN-CODE
               WHEN TC-OVER-QTY > 0
                   MOVE "WARN" TO TC-OUTCOME
                   MOVE "TOL0003" TO CA-REASON-CODE
                   MOVE TC-OVER-QTY TO WS-QTY-ED
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Receipt accepted within the approved over "
                          "delivery tolerance; "
                          FUNCTION TRIM(WS-QTY-ED)
                          " excess units are accrued"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE 4 TO CA-RETURN-CODE
               WHEN OTHER
                   MOVE "WITHIN" TO TC-OUTCOME
                   MOVE TC-OUTSTANDING-QTY TO WS-QTY-ED
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Receipt within the approved quantity; "
                          FUNCTION TRIM(WS-QTY-ED)
                          " remain outstanding"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE 0 TO CA-RETURN-CODE
           END-EVALUATE.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PRICE TEST (D-08).  BOTH 2% AND 50.00 MUST PASS, SO A SMALL
      *> PERCENTAGE ON A LARGE LINE STILL BLOCKS.
      *>----------------------------------------------------------------
       3000-PRICE SECTION.
       3000-START.
           IF TC-PRICE-QTY = 0
               MOVE 1 TO TC-PRICE-QTY
           END-IF
           COMPUTE TC-VARIANCE-AMT ROUNDED =
                   (TC-INVOICE-PRICE - TC-ORDER-PRICE) * TC-PRICE-QTY
           IF TC-ORDER-PRICE = 0
               MOVE 0 TO TC-VARIANCE-PCT
           ELSE
               COMPUTE TC-VARIANCE-PCT ROUNDED =
                   (TC-INVOICE-PRICE - TC-ORDER-PRICE) * 100
                   / TC-ORDER-PRICE
           END-IF
           COMPUTE WS-ABS-VAR = FUNCTION ABS(TC-VARIANCE-AMT)
           COMPUTE WS-ABS-PCT = FUNCTION ABS(TC-VARIANCE-PCT)
           MOVE "N" TO WS-PCT-FAIL
           MOVE "N" TO WS-ABS-FAIL
           IF WS-ABS-PCT > TC-PRICE-PCT-LIM
               MOVE "Y" TO WS-PCT-FAIL
           END-IF
           IF WS-ABS-VAR > TC-PRICE-ABS-LIM
               MOVE "Y" TO WS-ABS-FAIL
           END-IF
           IF WS-PCT-FAIL = "Y" AND WS-ABS-FAIL = "Y"
               MOVE "BLOCK" TO TC-OUTCOME
               MOVE "Y" TO TC-NEEDS-AMEND
               MOVE "TOL0004" TO CA-REASON-CODE
               MOVE WS-ABS-VAR TO WS-AMT-ED
               MOVE WS-ABS-PCT TO WS-PCT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Price variance of " FUNCTION TRIM(WS-AMT-ED)
                      " (" FUNCTION TRIM(WS-PCT-ED) "%) exceeds both "
                      "the percentage and the cash tolerance"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 6 TO CA-RETURN-CODE
           ELSE
               IF WS-PCT-FAIL = "Y" OR WS-ABS-FAIL = "Y"
                   MOVE "WARN" TO TC-OUTCOME
                   MOVE "TOL0004" TO CA-REASON-CODE
                   MOVE WS-ABS-VAR TO WS-AMT-ED
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Price variance of "
                          FUNCTION TRIM(WS-AMT-ED)
                          " passes one of the two tolerance tests"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE 4 TO CA-RETURN-CODE
               ELSE
                   MOVE "WITHIN" TO TC-OUTCOME
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Price variance is within tolerance"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE 0 TO CA-RETURN-CODE
               END-IF
           END-IF.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> DATE TEST.  MORE THAN FIVE WORKING DAYS EARLY IS FLAGGED
      *> (D-08).  DATECALC COUNTS THE WORKING DAYS ON THE SITE
      *> CALENDAR SO THAT A BANK HOLIDAY WEEK IS NOT MISREAD.
      *>----------------------------------------------------------------
       4000-DATE SECTION.
       4000-START.
           IF TC-PROMISED-DATE = 0 OR TC-ACTUAL-DATE = 0
               MOVE "WITHIN" TO TC-OUTCOME
               MOVE SPACES TO CA-MESSAGE
               STRING "No promised date is held, so no date "
                      "exception can be evaluated"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 4 TO CA-RETURN-CODE
               GO TO 4000-EXIT
           END-IF
           INITIALIZE WS-DATECALC-CA
           MOVE "DAYSBTWN" TO DH-FUNCTION
           MOVE TC-ACTUAL-DATE TO DC-DATE-1
           MOVE TC-PROMISED-DATE TO DC-DATE-2
           CALL "DATECALC" USING WS-DATECALC-CA
           IF DH-RETURN-CODE > 4
               MOVE 0 TO TC-DAYS-EARLY TC-DAYS-LATE
           ELSE
               IF DC-RESULT-NUM > 0
                   MOVE DC-RESULT-NUM TO TC-DAYS-EARLY
               ELSE
                   COMPUTE TC-DAYS-LATE = 0 - DC-RESULT-NUM
               END-IF
           END-IF
           IF TC-DAYS-EARLY > 0
               PERFORM 4200-WORK-DAYS THRU 4200-EXIT
           END-IF
           EVALUATE TRUE
               WHEN TC-WORK-DAYS-EARLY > TC-EARLY-DAY-LIM
                   MOVE "WARN" TO TC-OUTCOME
                   MOVE "TOL0005" TO CA-REASON-CODE
                   MOVE TC-WORK-DAYS-EARLY TO WS-DAY-ED
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Delivery is " FUNCTION TRIM(WS-DAY-ED)
                          " working days early, above the approved "
                          "five day allowance"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE 4 TO CA-RETURN-CODE
               WHEN TC-DAYS-LATE > TC-LATE-DAY-LIM
                   MOVE "WARN" TO TC-OUTCOME
                   MOVE "TOL0006" TO CA-REASON-CODE
                   MOVE TC-DAYS-LATE TO WS-DAY-ED
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Delivery is " FUNCTION TRIM(WS-DAY-ED)
                          " days late against the current promised "
                          "date"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE 4 TO CA-RETURN-CODE
               WHEN OTHER
                   MOVE "WITHIN" TO TC-OUTCOME
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Delivery date is within the approved "
                          "allowance"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE 0 TO CA-RETURN-CODE
           END-EVALUATE.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> DATECALC HAS NO "BUSINESS DAYS BETWEEN" FUNCTION, SO THE 1998
      *> ROUTINE STEPS FORWARD ONE BUSINESS DAY AT A TIME FROM THE
      *> ARRIVAL DATE UNTIL IT REACHES THE PROMISE.  THE LOOP IS
      *> CAPPED AT SIXTY STEPS; ANYTHING EARLIER THAN THAT IS ALREADY
      *> FAR OUTSIDE ANY SENSIBLE ALLOWANCE.
      *>----------------------------------------------------------------
       4200-WORK-DAYS SECTION.
       4200-START.
           MOVE TC-ACTUAL-DATE TO WS-STEP-DATE
           MOVE 0 TO WS-STEP-CNT
           PERFORM UNTIL WS-STEP-DATE >= TC-PROMISED-DATE
                      OR WS-STEP-CNT >= 60
               INITIALIZE WS-DATECALC-CA
               MOVE "ADDBUS" TO DH-FUNCTION
               MOVE WS-STEP-DATE TO DC-DATE-1
               MOVE 1 TO DC-NUMBER
               MOVE TC-CALENDAR-CODE TO DC-CALENDAR
               CALL "DATECALC" USING WS-DATECALC-CA
               IF DH-RETURN-CODE > 4
                   MOVE 60 TO WS-STEP-CNT
               ELSE
                   MOVE DC-RESULT-DATE TO WS-STEP-DATE
                   ADD 1 TO WS-STEP-CNT
               END-IF
           END-PERFORM
           IF WS-STEP-CNT >= 60
               MOVE TC-DAYS-EARLY TO TC-WORK-DAYS-EARLY
           ELSE
               MOVE WS-STEP-CNT TO TC-WORK-DAYS-EARLY
           END-IF.
       4200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> CHARGE TEST (D-08): FREIGHT AND CHARGE VARIANCE UP TO 25.00.
      *>----------------------------------------------------------------
       5000-CHARGE SECTION.
       5000-START.
           COMPUTE TC-VARIANCE-AMT = TC-ACTUAL-GBP - TC-ESTIMATE-GBP
           IF TC-ESTIMATE-GBP = 0
               MOVE 0 TO TC-VARIANCE-PCT
           ELSE
               COMPUTE TC-VARIANCE-PCT ROUNDED =
                   TC-VARIANCE-AMT * 100 / TC-ESTIMATE-GBP
           END-IF
           COMPUTE WS-ABS-VAR = FUNCTION ABS(TC-VARIANCE-AMT)
           IF WS-ABS-VAR > TC-CHARGE-ABS-LIM
               MOVE "BLOCK" TO TC-OUTCOME
               MOVE "Y" TO TC-NEEDS-AMEND
               MOVE "TOL0007" TO CA-REASON-CODE
               MOVE WS-ABS-VAR TO WS-AMT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Charge variance of " FUNCTION TRIM(WS-AMT-ED)
                      " exceeds the approved allowance and needs "
                      "authority"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 6 TO CA-RETURN-CODE
           ELSE
               MOVE "WITHIN" TO TC-OUTCOME
               MOVE SPACES TO CA-MESSAGE
               STRING "Charge variance is within the approved "
                      "allowance"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 0 TO CA-RETURN-CODE
           END-IF.
       5000-EXIT.
           EXIT.
