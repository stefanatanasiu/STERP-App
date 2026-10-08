      *>================================================================
      *> PROGRAM-ID : WIPCALC
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *> PURPOSE    : WORK IN PROGRESS ROLL-FORWARD FOR A JOB OR A KIT.
      *>                closing = opening + material issues
      *>                        + conversion and other additions
      *>                        + value adjustments - material returns
      *>                        - finished transfers - write-offs
      *>              A FINISHED GOODS RECEIPT MOVES VALUE OUT OF WIP;
      *>              IT DOES NOT LEAVE THE SAME MONEY IN TWO PLACES.
      *>              RESIDUAL WIP AT CLOSURE IS REPORTED, NEVER
      *>              QUIETLY DISCARDED.
      *> CALLED BY  : JOBCOST (CLOSE), THE PERIOD END BATCH, THE WIP
      *>              MOVEMENT ENQUIRY, THE .NET CLIENT
      *> INTERFACE  : CALL "WIPCALC" USING WS-WIPCALC-CA
      *>              COPYBOOK WIPCALCCA
      *> CALLS      : SQLIO (cst.JobCostTransaction, cst.WipBalance,
      *>              fin.Period)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-04-30 DJM  CR0651 ORIGINAL WIP ROLL-FORWARD
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT PERIOD DATES
      *> 2026-10-06 STERP CST-018 CST-020 RESIDUAL REVIEW
      *>================================================================
      *> REQ: CST-018, CST-020
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WIPCALC.
       AUTHOR. D J MARSH.
       DATE-WRITTEN. 30/04/1998.
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
           05  WS-WRITE                PIC X VALUE "N".
               88  WS-DO-WRITE                   VALUE "Y".
           05  WS-KEY-COL              PIC X(8) VALUE SPACES.
           05  WS-KEY-VAL              PIC X(30) VALUE SPACES.
           05  WS-PRIOR-YEAR           PIC 9(4) VALUE 0.
           05  WS-PRIOR-PERIOD         PIC 9(2) VALUE 0.
       LINKAGE SECTION.
       COPY WIPCALCCA.
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
               WHEN "CALC"     PERFORM 1000-CALCULATE
               WHEN "POST"     PERFORM 2000-POST
               WHEN "ENQUIRE"  PERFORM 3000-ENQUIRE
               WHEN "RESIDUAL" PERFORM 4000-RESIDUAL
               WHEN "WRITEOFF" PERFORM 5000-WRITE-OFF
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-SQL-FAILED WS-WRITE
           MOVE "WIPCALC" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "CST-018" TO CA-REQ-REF
           MOVE 0 TO WC-OPENING-GBP WC-MAT-ISSUES-GBP
           MOVE 0 TO WC-MAT-RETURNS-GBP WC-LABOUR-GBP WC-MACHINE-GBP
           MOVE 0 TO WC-OVERHEAD-GBP WC-SUBCON-GBP WC-ADJUSTMENT-GBP
           MOVE 0 TO WC-TRANSFERS-GBP WC-SCRAP-GBP
           MOVE 0 TO WC-CLOSING-GBP WC-ADDITIONS-GBP
           MOVE 0 TO WC-DEDUCTIONS-GBP WC-RESIDUAL-GBP WC-TXN-COUNT
           MOVE 0 TO WC-WIP-ID
           IF CA-FUNCTION NOT = "WRITEOFF"
               MOVE 0 TO WC-WRITEOFF-GBP
           END-IF
           MOVE "N" TO WC-HAS-RESIDUAL
           MOVE "Y" TO WC-BALANCED
           MOVE SPACES TO WC-WARNING WC-PERIOD-STATUS
           MOVE FUNCTION UPPER-CASE(WC-JOB-REF) TO WC-JOB-REF
           IF WC-COMMIT = "Y" OR WC-COMMIT = "T"
               MOVE "Y" TO WS-WRITE
           END-IF
           IF WC-JOB-REF NOT = SPACES
               MOVE "JobRef" TO WS-KEY-COL
               MOVE WC-JOB-REF TO WS-KEY-VAL
           ELSE
               MOVE "KitRef" TO WS-KEY-COL
               MOVE WC-KIT-REF TO WS-KEY-VAL
           END-IF
           MOVE WC-FISCAL-YEAR TO WS-PRIOR-YEAR
           IF WC-PERIOD-NO > 1
               COMPUTE WS-PRIOR-PERIOD = WC-PERIOD-NO - 1
           ELSE
               MOVE 12 TO WS-PRIOR-PERIOD
               IF WC-FISCAL-YEAR > 0
                   COMPUTE WS-PRIOR-YEAR = WC-FISCAL-YEAR - 1
               END-IF
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES
               MOVE "CST0901" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF WC-JOB-REF = SPACES AND WC-KIT-REF = SPACES
               MOVE "CST0902" TO CA-REASON-CODE
               MOVE "A job or a kit reference is required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF WC-FISCAL-YEAR = 0 OR WC-PERIOD-NO = 0
               MOVE "CST0903" TO CA-REASON-CODE
               MOVE "A fiscal year and period are required"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  CST-018.  THE ROLL-FORWARD ITSELF.  EVERY FIGURE COMES
      *>       FROM THE COST TRANSACTIONS, SO IT RECONCILES TO ITS
      *>       SOURCES BY CONSTRUCTION.  SPEC EXAMPLE C6.
      *>----------------------------------------------------------------
       1000-CALCULATE SECTION.
       1000-START.
           PERFORM 1100-OPENING-BALANCE
           IF WS-SQL-BAD
               GO TO 1000-EXIT
           END-IF
           PERFORM 1200-PERIOD-MOVEMENTS
           IF WS-SQL-BAD
               GO TO 1000-EXIT
           END-IF
           COMPUTE WC-ADDITIONS-GBP = WC-MAT-ISSUES-GBP
                                      + WC-LABOUR-GBP
                                      + WC-MACHINE-GBP
                                      + WC-OVERHEAD-GBP
                                      + WC-SUBCON-GBP
                                      + WC-ADJUSTMENT-GBP
           COMPUTE WC-DEDUCTIONS-GBP = WC-MAT-RETURNS-GBP
                                       + WC-TRANSFERS-GBP
                                       + WC-SCRAP-GBP
                                       + WC-WRITEOFF-GBP
           COMPUTE WC-CLOSING-GBP = WC-OPENING-GBP
                                    + WC-ADDITIONS-GBP
                                    - WC-DEDUCTIONS-GBP
           MOVE WC-CLOSING-GBP TO WC-RESIDUAL-GBP
           IF WC-RESIDUAL-GBP NOT = 0
               MOVE "Y" TO WC-HAS-RESIDUAL
           END-IF.
       1000-EXIT.
           EXIT.

       1100-OPENING-BALANCE SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 ClosingValueGBP FROM cst.WipBalance "
                  "WHERE CompanyCode = ? AND ISNULL(JobRef,'') = ? "
                  "AND ISNULL(KitRef,'') = ? AND (FiscalYear < ? OR "
                  "(FiscalYear = ? AND PeriodNo < ?)) ORDER BY "
                  "FiscalYear DESC, PeriodNo DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE WC-JOB-REF TO SQLIO-PARM-VAL(2)
           MOVE WC-KIT-REF TO SQLIO-PARM-VAL(3)
           MOVE WC-FISCAL-YEAR TO SQLIO-PARM-VAL(4)
           MOVE WC-FISCAL-YEAR TO SQLIO-PARM-VAL(5)
           MOVE WC-PERIOD-NO TO SQLIO-PARM-VAL(6)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-GOT-ROW
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO WC-OPENING-GBP
           END-IF.
       1100-EXIT.
           EXIT.

      *>  THE MOVEMENTS OF THE PERIOD, BUCKETED BY ELEMENT TYPE.  AN
      *>  ELEMENT THAT IS NOT INVENTORIABLE NEVER REACHES WIP.
       1200-PERIOD-MOVEMENTS SECTION.
       1200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT e.ElementType, t.ElementCode, "
                  "SUM(t.AmountGBP), COUNT(*) FROM "
                  "cst.JobCostTransaction t JOIN cst.CostElement e "
                  "ON e.ElementCode = t.ElementCode WHERE "
                  "t.CompanyCode = ? AND ISNULL(t.JobRef,'') = ? "
                  "AND ISNULL(t.KitRef,'') = ? AND t.FiscalYear = ? "
                  "AND t.PeriodNo = ? AND t.IsApproved = 1 AND "
                  "t.CostNature IN ('ACTUAL','ACCRUED') GROUP BY "
                  "e.ElementType, t.ElementCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE WC-JOB-REF TO SQLIO-PARM-VAL(2)
           MOVE WC-KIT-REF TO SQLIO-PARM-VAL(3)
           MOVE WC-FISCAL-YEAR TO SQLIO-PARM-VAL(4)
           MOVE WC-PERIOD-NO TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           IF SQLIO-SQLCODE < 0
               PERFORM 8900-SQL-ERROR
               GO TO 1200-EXIT
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
               MOVE FUNCTION NUMVAL(WS-COL(3)) TO WS-AMT1
               ADD FUNCTION NUMVAL(WS-COL(4)) TO WC-TXN-COUNT
               PERFORM 1300-BUCKET
           END-PERFORM
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW.
       1200-EXIT.
           EXIT.

       1300-BUCKET SECTION.
       1300-START.
           EVALUATE WS-COL(2)(1:10)
               WHEN "TRANSFER  "
                   ADD WS-AMT1 TO WC-TRANSFERS-GBP
                   GO TO 1300-EXIT
               WHEN "MATRETURN "
                   ADD WS-AMT1 TO WC-MAT-RETURNS-GBP
                   GO TO 1300-EXIT
               WHEN "SCRAP     "
                   ADD WS-AMT1 TO WC-SCRAP-GBP
                   GO TO 1300-EXIT
               WHEN "WRITEOFF  "
                   ADD WS-AMT1 TO WC-WRITEOFF-GBP
                   GO TO 1300-EXIT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           EVALUATE WS-COL(1)(1:8)
               WHEN "MATERIAL"
                   IF WS-AMT1 < 0
                       COMPUTE WC-MAT-RETURNS-GBP =
                           WC-MAT-RETURNS-GBP - WS-AMT1
                   ELSE
                       ADD WS-AMT1 TO WC-MAT-ISSUES-GBP
                   END-IF
               WHEN "LABOUR  " ADD WS-AMT1 TO WC-LABOUR-GBP
               WHEN "MACHINE " ADD WS-AMT1 TO WC-MACHINE-GBP
               WHEN "SUBCON  " ADD WS-AMT1 TO WC-SUBCON-GBP
               WHEN "OVHDVAR " ADD WS-AMT1 TO WC-OVERHEAD-GBP
               WHEN "OVHDFIX " ADD WS-AMT1 TO WC-OVERHEAD-GBP
               WHEN "LANDED  " ADD WS-AMT1 TO WC-ADJUSTMENT-GBP
               WHEN "VARIANCE" ADD WS-AMT1 TO WC-ADJUSTMENT-GBP
               WHEN "RECOVERY"
                   COMPUTE WC-ADJUSTMENT-GBP =
                       WC-ADJUSTMENT-GBP - WS-AMT1
               WHEN OTHER      ADD WS-AMT1 TO WC-ADJUSTMENT-GBP
           END-EVALUATE.
       1300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  STORE THE ROLL-FORWARD SO THAT NEXT PERIOD HAS AN
      *>       OPENING BALANCE IT CAN TRUST.
      *>----------------------------------------------------------------
       2000-POST SECTION.
       2000-START.
           PERFORM 1000-CALCULATE
           IF WS-SQL-BAD OR WS-INPUT-BAD
               GO TO 2000-EXIT
           END-IF
           IF NOT WS-DO-WRITE
               MOVE "Preview only - no WIP balance was written"
                   TO WC-WARNING
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE cst.WipBalance SET OpeningValueGBP = ?, "
                  "MaterialIssuesGBP = ?, MaterialReturnsGBP = ?, "
                  "LabourGBP = ?, OverheadGBP = ?, SubcontractGBP = "
                  "?, CompletionTransfersGBP = ?, ScrapGBP = ?, "
                  "WriteOffGBP = ?, ValueAdjustmentGBP = ?, "
                  "ClosingValueGBP = ?, LastUpdatedAt = "
                  "SYSDATETIME() WHERE CompanyCode = ? AND "
                  "ISNULL(JobRef,'') = ? AND ISNULL(KitRef,'') = ? "
                  "AND FiscalYear = ? AND PeriodNo = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 2100-BIND-BALANCE
           PERFORM 8100-EXECUTE
           IF WS-SQL-BAD
               GO TO 2000-EXIT
           END-IF
           IF SQLIO-ROWCOUNT > 0
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO cst.WipBalance (OpeningValueGBP, "
                  "MaterialIssuesGBP, MaterialReturnsGBP, LabourGBP, "
                  "OverheadGBP, SubcontractGBP, "
                  "CompletionTransfersGBP, ScrapGBP, WriteOffGBP, "
                  "ValueAdjustmentGBP, ClosingValueGBP, "
                  "LastUpdatedAt, CompanyCode, JobRef, KitRef, "
                  "FiscalYear, PeriodNo) VALUES (?, ?, ?, ?, ?, ?, "
                  "?, ?, ?, ?, ?, SYSDATETIME(), ?, ?, ?, ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 2100-BIND-BALANCE
           IF WC-JOB-REF = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(13)
           END-IF
           IF WC-KIT-REF = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(14)
           END-IF
           PERFORM 8100-EXECUTE.
       2000-EXIT.
           EXIT.

       2100-BIND-BALANCE SECTION.
       2100-START.
           MOVE 16 TO SQLIO-PARM-CNT
           MOVE WC-OPENING-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE WC-MAT-ISSUES-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE WC-MAT-RETURNS-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE WC-LABOUR-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE WC-OVERHEAD-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE WC-SUBCON-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE WC-TRANSFERS-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE WC-SCRAP-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE WC-WRITEOFF-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE WC-ADJUSTMENT-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE WC-CLOSING-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(12)
           MOVE WC-JOB-REF TO SQLIO-PARM-VAL(13)
           MOVE WC-KIT-REF TO SQLIO-PARM-VAL(14)
           MOVE WC-FISCAL-YEAR TO SQLIO-PARM-VAL(15)
           MOVE WC-PERIOD-NO TO SQLIO-PARM-VAL(16)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 16
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM.
       2100-EXIT.
           EXIT.

       3000-ENQUIRE SECTION.
       3000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT WipId, OpeningValueGBP, "
                  "MaterialIssuesGBP, MaterialReturnsGBP, LabourGBP, "
                  "OverheadGBP, SubcontractGBP, "
                  "CompletionTransfersGBP, ScrapGBP, WriteOffGBP, "
                  "ValueAdjustmentGBP, ClosingValueGBP FROM "
                  "cst.WipBalance WHERE CompanyCode = ? AND "
                  "ISNULL(JobRef,'') = ? AND ISNULL(KitRef,'') = ? "
                  "AND FiscalYear = ? AND PeriodNo = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE WC-JOB-REF TO SQLIO-PARM-VAL(2)
           MOVE WC-KIT-REF TO SQLIO-PARM-VAL(3)
           MOVE WC-FISCAL-YEAR TO SQLIO-PARM-VAL(4)
           MOVE WC-PERIOD-NO TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 12 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF NOT WS-GOT-ROW
               IF NOT WS-SQL-BAD
                   MOVE "CST0905" TO CA-REASON-CODE
                   MOVE "No WIP balance is held for that period"
                       TO CA-MESSAGE
                   MOVE 16 TO CA-RETURN-CODE
               END-IF
               GO TO 3000-EXIT
           END-IF
           MOVE FUNCTION NUMVAL(WS-COL(1)) TO WC-WIP-ID
           MOVE FUNCTION NUMVAL(WS-COL(2)) TO WC-OPENING-GBP
           MOVE FUNCTION NUMVAL(WS-COL(3)) TO WC-MAT-ISSUES-GBP
           MOVE FUNCTION NUMVAL(WS-COL(4)) TO WC-MAT-RETURNS-GBP
           MOVE FUNCTION NUMVAL(WS-COL(5)) TO WC-LABOUR-GBP
           MOVE FUNCTION NUMVAL(WS-COL(6)) TO WC-OVERHEAD-GBP
           MOVE FUNCTION NUMVAL(WS-COL(7)) TO WC-SUBCON-GBP
           MOVE FUNCTION NUMVAL(WS-COL(8)) TO WC-TRANSFERS-GBP
           MOVE FUNCTION NUMVAL(WS-COL(9)) TO WC-SCRAP-GBP
           MOVE FUNCTION NUMVAL(WS-COL(10)) TO WC-WRITEOFF-GBP
           MOVE FUNCTION NUMVAL(WS-COL(11)) TO WC-ADJUSTMENT-GBP
           MOVE FUNCTION NUMVAL(WS-COL(12)) TO WC-CLOSING-GBP
           COMPUTE WC-ADDITIONS-GBP = WC-MAT-ISSUES-GBP
                                      + WC-LABOUR-GBP
                                      + WC-OVERHEAD-GBP
                                      + WC-SUBCON-GBP
                                      + WC-ADJUSTMENT-GBP
           COMPUTE WC-DEDUCTIONS-GBP = WC-MAT-RETURNS-GBP
                                       + WC-TRANSFERS-GBP
                                       + WC-SCRAP-GBP
                                       + WC-WRITEOFF-GBP
           MOVE WC-CLOSING-GBP TO WC-RESIDUAL-GBP
           IF WC-RESIDUAL-GBP NOT = 0
               MOVE "Y" TO WC-HAS-RESIDUAL
           END-IF.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  CST-020.  WOULD A CLOSE STRAND VALUE?  THE ANSWER IS A
      *>       WARNING, NOT A SILENT DELETION.
      *>----------------------------------------------------------------
       4000-RESIDUAL SECTION.
       4000-START.
           PERFORM 1000-CALCULATE
           IF WS-SQL-BAD OR WS-INPUT-BAD
               GO TO 4000-EXIT
           END-IF
           IF WC-RESIDUAL-GBP NOT = 0
               MOVE "CST0906" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               MOVE WC-RESIDUAL-GBP TO WS-MONEY-ED
               STRING "Residual work in progress of GBP "
                      FUNCTION TRIM(WS-MONEY-ED)
                      " needs a transfer, variance or write-off"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 6 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       4000-EXIT.
           EXIT.

      *>  AN APPROVED WRITE-OFF IS A COST TRANSACTION LIKE ANY OTHER,
      *>  WHICH IS WHY IT NEEDS A REASON AND AN APPROVER.
       5000-WRITE-OFF SECTION.
       5000-START.
           IF WC-REASON = SPACES OR CA-USER = SPACES
               MOVE "CST0907" TO CA-REASON-CODE
               MOVE "A write-off needs an approver and a reason"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 5000-EXIT
           END-IF
           IF WC-WRITEOFF-GBP = 0
               PERFORM 1000-CALCULATE
               MOVE WC-RESIDUAL-GBP TO WC-WRITEOFF-GBP
           END-IF
           IF WC-WRITEOFF-GBP = 0
               MOVE "There is no residual work in progress"
                   TO WC-WARNING
               MOVE 4 TO CA-RETURN-CODE
               GO TO 5000-EXIT
           END-IF
           IF NOT WS-DO-WRITE
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO cst.JobCostTransaction (CompanyCode, "
                  "JobRef, KitRef, ElementCode, SourceModule, "
                  "SourceDocType, SourceDocRef, Amount, AmountGBP, "
                  "CostNature, RecognitionDate, FiscalYear, "
                  "PeriodNo, IsApproved, ApprovedBy, Narrative, "
                  "CreatedBy) VALUES (?, ?, ?, 'WRITEOFF', 'CST', "
                  "'WIPWRITEOFF', ?, ?, ?, 'ACTUAL', "
                  "CAST(GETDATE() AS date), ?, ?, 1, ?, ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 10 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE WC-JOB-REF TO SQLIO-PARM-VAL(2)
           MOVE WC-KIT-REF TO SQLIO-PARM-VAL(3)
           MOVE WS-KEY-VAL TO SQLIO-PARM-VAL(4)
           MOVE WC-WRITEOFF-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE WC-FISCAL-YEAR TO SQLIO-PARM-VAL(7)
           MOVE WC-PERIOD-NO TO SQLIO-PARM-VAL(8)
           MOVE CA-USER TO SQLIO-PARM-VAL(9)
           MOVE WC-REASON TO SQLIO-PARM-VAL(10)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 10
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           IF WC-JOB-REF = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(2)
           END-IF
           IF WC-KIT-REF = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(3)
           END-IF
           PERFORM 8100-EXECUTE
           IF NOT WS-SQL-BAD
               PERFORM 2000-POST
           END-IF.
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
           MOVE "CST0908" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                  " is not supported by WIPCALC"
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
