      *>================================================================
      *> PROGRAM-ID : VARIANCE
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *> PURPOSE    : SPLIT A COST DIFFERENCE INTO ITS QUANTITY AND
      *>              RATE COMPONENTS AND PROVE THAT THE TWO ADD BACK
      *>              TO THE TOTAL.  THE BASELINE QUANTITY IS ALWAYS
      *>              DISCLOSED - A VARIANCE WITHOUT ITS ALLOWANCE IS
      *>              AN OPINION, NOT AN ANALYSIS.
      *> CALLED BY  : JOBCOST, STDCOST, WIPCALC, THE VARIANCE REPORT,
      *>              THE .NET CLIENT
      *> INTERFACE  : CALL "VARIANCE" USING WS-VARIANCE-CA
      *>              COPYBOOK VARIANCECA
      *> CALLS      : SQLIO (cst.Variance)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-06-22 RJH  CR0668 ORIGINAL VARIANCE ANALYSIS
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT PERIOD DATES
      *> 2026-10-06 STERP CST-019 COMPONENTS RECONCILE TO THE TOTAL
      *>================================================================
      *> REQ: CST-019
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VARIANCE.
       AUTHOR. R J HARTLEY.
       DATE-WRITTEN. 22/06/1998.
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
           05  WS-CHECK                PIC S9(13)V9(2) VALUE 0.
       LINKAGE SECTION.
       COPY VARIANCECA.
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
               WHEN "SPLIT"    PERFORM 1000-SPLIT
               WHEN "MATERIAL" PERFORM 1000-SPLIT
               WHEN "LABOUR"   PERFORM 1000-SPLIT
               WHEN "OVERHEAD" PERFORM 1000-SPLIT
               WHEN "RECORD"   PERFORM 2000-RECORD
               WHEN "SUMMARY"  PERFORM 3000-SUMMARY
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-SQL-FAILED
           MOVE "VARIANCE" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "CST-019" TO CA-REQ-REF
           MOVE 0 TO VA-ALLOWED-QTY VA-BASELINE-GBP VA-ACTUAL-GBP
           MOVE 0 TO VA-QTY-VAR-GBP VA-RATE-VAR-GBP VA-TOTAL-VAR-GBP
           MOVE 0 TO VA-VARIANCE-ID VA-LINE-CNT WS-CHECK
           MOVE "N" TO VA-ADVERSE VA-MORE-LINES
           MOVE "Y" TO VA-RECONCILED
           MOVE SPACES TO VA-WARNING
           MOVE FUNCTION UPPER-CASE(VA-VARIANCE-TYPE)
               TO VA-VARIANCE-TYPE
           EVALUATE CA-FUNCTION
               WHEN "MATERIAL"
                   IF VA-VARIANCE-TYPE = SPACES
                       MOVE "MATUSAGE" TO VA-VARIANCE-TYPE
                   END-IF
               WHEN "LABOUR"
                   IF VA-VARIANCE-TYPE = SPACES
                       MOVE "LABEFF" TO VA-VARIANCE-TYPE
                   END-IF
               WHEN "OVERHEAD"
                   IF VA-VARIANCE-TYPE = SPACES
                       MOVE "OVHD" TO VA-VARIANCE-TYPE
                   END-IF
               WHEN OTHER CONTINUE
           END-EVALUATE
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES
               MOVE "CST1001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION = "RECORD" AND VA-FISCAL-YEAR = 0
               MOVE "CST1005" TO CA-REASON-CODE
               MOVE "A recorded variance needs a fiscal period"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  CST-019.  ALLOWED QUANTITY FOR THE ACTUAL OUTPUT, THEN
      *>         usage = (actual - allowed) x baseline rate
      *>         price = (actual rate - baseline rate) x actual qty
      *>       WHICH ADD EXACTLY TO ACTUAL LESS ALLOWED.  SPEC C5.
      *>----------------------------------------------------------------
       1000-SPLIT SECTION.
       1000-START.
           IF VA-OUTPUT-QTY NOT = 0 AND VA-QTY-PER-OUTPUT NOT = 0
               COMPUTE VA-ALLOWED-QTY =
                   VA-OUTPUT-QTY * VA-QTY-PER-OUTPUT
           ELSE
               MOVE VA-BASELINE-QTY TO VA-ALLOWED-QTY
           END-IF
           IF VA-ALLOWED-QTY = 0 AND VA-ACTUAL-QTY NOT = 0
               MOVE "CST1003" TO CA-REASON-CODE
               MOVE "The baseline quantity must be disclosed"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1000-EXIT
           END-IF
           MOVE VA-ALLOWED-QTY TO VA-BASELINE-QTY
           COMPUTE VA-BASELINE-GBP ROUNDED =
               VA-ALLOWED-QTY * VA-BASELINE-RATE
           COMPUTE VA-ACTUAL-GBP ROUNDED =
               VA-ACTUAL-QTY * VA-ACTUAL-RATE
           COMPUTE VA-QTY-VAR-GBP ROUNDED =
               (VA-ACTUAL-QTY - VA-ALLOWED-QTY) * VA-BASELINE-RATE
           COMPUTE VA-RATE-VAR-GBP ROUNDED =
               (VA-ACTUAL-RATE - VA-BASELINE-RATE) * VA-ACTUAL-QTY
           COMPUTE VA-TOTAL-VAR-GBP =
               VA-QTY-VAR-GBP + VA-RATE-VAR-GBP
           COMPUTE WS-CHECK = VA-ACTUAL-GBP - VA-BASELINE-GBP
           IF WS-CHECK NOT = VA-TOTAL-VAR-GBP
               MOVE "N" TO VA-RECONCILED
               MOVE "CST1004" TO CA-REASON-CODE
               MOVE "Rounding leaves the components short of the "
                   TO CA-MESSAGE
               STRING FUNCTION TRIM(CA-MESSAGE) "total difference"
                   DELIMITED BY SIZE INTO WS-WORK-TEXT
               MOVE WS-WORK-TEXT TO CA-MESSAGE
               MOVE 4 TO CA-RETURN-CODE
               MOVE WS-CHECK TO VA-TOTAL-VAR-GBP
           END-IF
           IF VA-TOTAL-VAR-GBP > 0
               MOVE "Y" TO VA-ADVERSE
           END-IF
           IF CA-FUNCTION NOT = "SPLIT" AND VA-COMMIT = "Y"
               PERFORM 2000-RECORD
           END-IF.
       1000-EXIT.
           EXIT.

       2000-RECORD SECTION.
       2000-START.
           IF VA-TOTAL-VAR-GBP = 0 AND VA-QTY-VAR-GBP = 0
               PERFORM 1000-SPLIT
               IF WS-INPUT-BAD
                   GO TO 2000-EXIT
               END-IF
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO cst.Variance (CompanyCode, "
                  "VarianceType, ItemCode, JobRef, SourceDocType, "
                  "SourceDocRef, BaselineQty, ActualQty, "
                  "BaselineRate, ActualRate, QuantityVarianceGBP, "
                  "RateVarianceGBP, TotalVarianceGBP, IsAdverse, "
                  "FiscalYear, PeriodNo, ReasonCode, Reason, "
                  "OwnerUser) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, "
                  "?, ?, ?, ?, ?, ?, ?, ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 19 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE VA-VARIANCE-TYPE TO SQLIO-PARM-VAL(2)
           MOVE VA-ITEM-CODE TO SQLIO-PARM-VAL(3)
           MOVE VA-JOB-REF TO SQLIO-PARM-VAL(4)
           MOVE VA-DOC-TYPE TO SQLIO-PARM-VAL(5)
           MOVE VA-DOC-REF TO SQLIO-PARM-VAL(6)
           MOVE VA-BASELINE-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE VA-ACTUAL-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE VA-BASELINE-RATE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE VA-ACTUAL-RATE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE VA-QTY-VAR-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE VA-RATE-VAR-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE VA-TOTAL-VAR-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           IF VA-IS-ADVERSE
               MOVE "1" TO SQLIO-PARM-VAL(14)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(14)
           END-IF
           MOVE VA-FISCAL-YEAR TO SQLIO-PARM-VAL(15)
           MOVE VA-PERIOD-NO TO SQLIO-PARM-VAL(16)
           MOVE VA-REASON-CODE TO SQLIO-PARM-VAL(17)
           MOVE VA-REASON TO SQLIO-PARM-VAL(18)
           MOVE VA-OWNER-USER TO SQLIO-PARM-VAL(19)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 19
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           IF VA-ITEM-CODE = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(3)
           END-IF
           IF VA-JOB-REF = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(4)
           END-IF
           PERFORM 8100-EXECUTE.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  TOTALS BY VARIANCE TYPE FOR A PERIOD - THE HEADLINE OF
      *>       THE VARIANCE REPORT.
      *>----------------------------------------------------------------
       3000-SUMMARY SECTION.
       3000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT VarianceType, COUNT(*), "
                  "SUM(QuantityVarianceGBP), SUM(RateVarianceGBP), "
                  "SUM(TotalVarianceGBP) FROM cst.Variance WHERE "
                  "CompanyCode = ? AND (? = 0 OR FiscalYear = ?) "
                  "AND (? = 0 OR PeriodNo = ?) GROUP BY "
                  "VarianceType ORDER BY VarianceType"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE VA-FISCAL-YEAR TO SQLIO-PARM-VAL(2)
           MOVE VA-FISCAL-YEAR TO SQLIO-PARM-VAL(3)
           MOVE VA-PERIOD-NO TO SQLIO-PARM-VAL(4)
           MOVE VA-PERIOD-NO TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           IF SQLIO-SQLCODE < 0
               PERFORM 8900-SQL-ERROR
               GO TO 3000-EXIT
           END-IF
           PERFORM UNTIL WS-SQL-BAD
               MOVE 2 TO SQLIO-CURSOR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
               IF SQLIO-SQLCODE = 100
                   EXIT PERFORM
               END-IF
               IF SQLIO-SQLCODE < 0
                   PERFORM 8900-SQL-ERROR
                   EXIT PERFORM
               END-IF
               IF VA-LINE-CNT >= 15
                   MOVE "Y" TO VA-MORE-LINES
                   EXIT PERFORM
               END-IF
               ADD 1 TO VA-LINE-CNT
               MOVE VA-LINE-CNT TO WS-IX2
               MOVE WS-COL(1) TO VA-L-TYPE(WS-IX2)
               MOVE FUNCTION NUMVAL(WS-COL(2)) TO VA-L-COUNT(WS-IX2)
               MOVE FUNCTION NUMVAL(WS-COL(3))
                   TO VA-L-QTY-VAR(WS-IX2)
               MOVE FUNCTION NUMVAL(WS-COL(4))
                   TO VA-L-RATE-VAR(WS-IX2)
               MOVE FUNCTION NUMVAL(WS-COL(5)) TO VA-L-TOTAL(WS-IX2)
               IF VA-L-TOTAL(WS-IX2) > 0
                   MOVE "Y" TO VA-L-ADVERSE(WS-IX2)
               ELSE
                   MOVE "N" TO VA-L-ADVERSE(WS-IX2)
               END-IF
               ADD VA-L-TOTAL(WS-IX2) TO VA-TOTAL-VAR-GBP
               PERFORM 3100-DESCRIBE
           END-PERFORM
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW.
       3000-EXIT.
           EXIT.

       3100-DESCRIBE SECTION.
       3100-START.
           EVALUATE VA-L-TYPE(WS-IX2)
               WHEN "PPV"
                   MOVE "Receipt price variance"
                       TO VA-L-DESC(WS-IX2)
               WHEN "INVOICEPRICE"
                   MOVE "Incremental invoice price variance"
                       TO VA-L-DESC(WS-IX2)
               WHEN "MATUSAGE"
                   MOVE "Material usage variance"
                       TO VA-L-DESC(WS-IX2)
               WHEN "MATPRICE"
                   MOVE "Material price variance"
                       TO VA-L-DESC(WS-IX2)
               WHEN "LABEFF"
                   MOVE "Labour efficiency variance"
                       TO VA-L-DESC(WS-IX2)
               WHEN "LABRATE"
                   MOVE "Labour rate variance"
                       TO VA-L-DESC(WS-IX2)
               WHEN "OVHD"
                   MOVE "Overhead absorption variance"
                       TO VA-L-DESC(WS-IX2)
               WHEN "RETURNVAL"
                   MOVE "Return valuation variance"
                       TO VA-L-DESC(WS-IX2)
               WHEN "LANDED"
                   MOVE "Landed cost difference"
                       TO VA-L-DESC(WS-IX2)
               WHEN "REVALUE"
                   MOVE "Standard cost revaluation"
                       TO VA-L-DESC(WS-IX2)
               WHEN OTHER
                   MOVE VA-L-TYPE(WS-IX2) TO VA-L-DESC(WS-IX2)
           END-EVALUATE.
       3100-EXIT.
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
           MOVE "CST9001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG TO CA-MESSAGE
           MOVE 12 TO CA-RETURN-CODE.
       8900-EXIT.
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
           MOVE "CST1006" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                  " is not supported by VARIANCE"
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
