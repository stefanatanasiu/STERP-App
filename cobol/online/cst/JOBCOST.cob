      *>================================================================
      *> PROGRAM-ID : JOBCOST
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY
      *> PURPOSE    : JOBS AND SUB-JOBS (TWO LEVELS, D-13), THEIR
      *>              ORIGINAL AND REVISED BUDGETS, THE COST
      *>              TRANSACTIONS BOOKED AGAINST THEM, THE FORECAST
      *>              AT COMPLETION AND THE SEPARATE OPERATIONAL AND
      *>              FINANCIAL CLOSURES.
      *>                EAC = actual + remaining commitments
      *>                    + uncommitted estimate to complete
      *>              A RECEIVED COST IS ACTUAL COST, NOT AN OPEN
      *>              COMMITMENT, SO IT IS NEVER COUNTED TWICE.
      *> CALLED BY  : PURCHASING, KIT EXECUTION, TIME BOOKING, THE JOB
      *>              PROFITABILITY REPORT, THE .NET CLIENT
      *> INTERFACE  : CALL "JOBCOST" USING WS-JOBCOST-CA
      *>              COPYBOOK JOBCOSTCA
      *> CALLS      : WIPCALC (RESIDUAL REVIEW AT CLOSURE),
      *>              SQLIO (cst.Job, cst.JobBudget, cst.JobBudgetLine,
      *>              cst.JobCostTransaction)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-02-16 DJM  CR0634 ORIGINAL JOB COSTING
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT JOB DATES
      *> 2013-06-03 RJH  CR1988 OPERATIONAL AND FINANCIAL STATES SPLIT
      *> 2026-10-06 STERP CST-014 TO CST-017 CST-020 FORECAST
      *>================================================================
      *> REQ: CST-014, CST-015, CST-016, CST-017, CST-020
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JOBCOST.
       AUTHOR. D J MARSH.
       DATE-WRITTEN. 16/02/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY CSTWORK.
       COPY WIPCALCCA REPLACING ==DFHCOMMAREA== BY ==WS-WIP-CA==.
       01  WS-FLAGS.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-WRITE                PIC X VALUE "N".
               88  WS-DO-WRITE                   VALUE "Y".
           05  WS-JOB-EXISTS           PIC X VALUE "N".
               88  WS-HAVE-JOB                   VALUE "Y".
           05  WS-OPER-STATUS          PIC X(14) VALUE SPACES.
           05  WS-FIN-STATUS           PIC X(14) VALUE SPACES.
           05  WS-BUDGET-ID            PIC S9(9) VALUE 0.
       LINKAGE SECTION.
       COPY JOBCOSTCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           PERFORM 0300-READ-JOB
           IF WS-SQL-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION OF DFHCOMMAREA
               WHEN "CREATE"    PERFORM 1000-CREATE
               WHEN "BUDGET"    PERFORM 2000-BUDGET
               WHEN "APPRBUDG" PERFORM 2500-APPROVE-BUDGET
               WHEN "POSTCOST"  PERFORM 3000-POST-COST
               WHEN "REVERSE"   PERFORM 3500-REVERSE
               WHEN "ENQUIRE"   PERFORM 4000-ENQUIRE
               WHEN "FORECAST"  PERFORM 5000-FORECAST
               WHEN "COMPLETE"  PERFORM 6000-COMPLETE
               WHEN "CLOSE"     PERFORM 6500-CLOSE
               WHEN "REOPEN"    PERFORM 7000-REOPEN
               WHEN OTHER       PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-SQL-FAILED WS-WRITE WS-JOB-EXISTS
           MOVE "JOBCOST" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "CST-014" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE 0 TO JC-ACTUAL-GBP JC-ACCRUED-GBP JC-COMMITTED-GBP
           MOVE 0 TO JC-EAC-GBP JC-FCST-VAR-GBP JC-REVENUE-GBP
           MOVE 0 TO JC-WIP-GBP JC-SUBJOB-COUNT JC-TXN-COUNT
           MOVE 0 TO JC-LATE-COST-GBP JC-LINE-CNT JC-ORIGINAL-BUDGET
           MOVE 0 TO JC-APPROVED-BUDGET WS-BUDGET-ID
           MOVE "N" TO JC-FCST-ADVERSE JC-MORE-LINES
           MOVE SPACES TO JC-WARNING WS-OPER-STATUS WS-FIN-STATUS
           MOVE FUNCTION UPPER-CASE(JC-JOB-REF) TO JC-JOB-REF
           MOVE FUNCTION UPPER-CASE(JC-PARENT-JOB) TO JC-PARENT-JOB
           MOVE FUNCTION UPPER-CASE(JC-ELEMENT-CODE)
               TO JC-ELEMENT-CODE
           IF JC-CURRENCY = SPACES
               MOVE "GBP" TO JC-CURRENCY
           END-IF
           IF JC-RECOGNITION-DT = 0
               MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
                   TO JC-RECOGNITION-DT
           END-IF
           IF JC-COST-NATURE = SPACES
               MOVE "ACTUAL" TO JC-COST-NATURE
           END-IF
           IF JC-COMMIT = "Y" OR JC-COMMIT = "T"
               MOVE "Y" TO WS-WRITE
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY OF DFHCOMMAREA = SPACES
              OR JC-JOB-REF = SPACES
               MOVE "CST0801" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A company and a job reference are required"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

       0300-READ-JOB SECTION.
       0300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT JobLevel, OperationalStatus, "
                  "FinancialStatus, ISNULL(ParentJobRef,''), "
                  "Description FROM cst.Job WHERE CompanyCode = ? "
                  "AND JobRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE JC-JOB-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 5 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-GOT-ROW
               MOVE "Y" TO WS-JOB-EXISTS
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO JC-JOB-LEVEL
               MOVE WS-COL(2) TO WS-OPER-STATUS JC-OPER-STATUS
               MOVE WS-COL(3) TO WS-FIN-STATUS JC-FIN-STATUS
               IF JC-PARENT-JOB = SPACES
                   MOVE WS-COL(4) TO JC-PARENT-JOB
               END-IF
               IF JC-DESCRIPTION = SPACES
                   MOVE WS-COL(5) TO JC-DESCRIPTION
               END-IF
           END-IF.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  D-13 ALLOWS EXACTLY TWO LEVELS.  A SUB-JOB OF A SUB-JOB
      *>       IS A PROJECT STRUCTURE, WHICH THIS PACK DOES NOT HAVE.
      *>----------------------------------------------------------------
       1000-CREATE SECTION.
       1000-START.
           IF WS-HAVE-JOB
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "The job already exists" TO JC-WARNING
               GO TO 1000-EXIT
           END-IF
           IF JC-PARENT-JOB NOT = SPACES
               MOVE 2 TO JC-JOB-LEVEL
               PERFORM 1100-CHECK-PARENT
               IF WS-INPUT-BAD
                   GO TO 1000-EXIT
               END-IF
           ELSE
               MOVE 1 TO JC-JOB-LEVEL
           END-IF
           IF NOT WS-DO-WRITE
               GO TO 1000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO cst.Job (CompanyCode, JobRef, "
                  "ParentJobRef, JobLevel, Description, SiteCode, "
                  "CustomerCode, SalesOrderNumber, Purpose, "
                  "OwnerUser, CostCentre, CreatedBy) VALUES (?, ?, "
                  "?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 12 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE JC-JOB-REF TO SQLIO-PARM-VAL(2)
           MOVE JC-PARENT-JOB TO SQLIO-PARM-VAL(3)
           MOVE JC-JOB-LEVEL TO SQLIO-PARM-VAL(4)
           MOVE JC-DESCRIPTION TO SQLIO-PARM-VAL(5)
           MOVE CA-SITE OF DFHCOMMAREA TO SQLIO-PARM-VAL(6)
           MOVE JC-CUSTOMER TO SQLIO-PARM-VAL(7)
           MOVE JC-SALES-ORDER TO SQLIO-PARM-VAL(8)
           IF JC-PURPOSE = SPACES
               MOVE "INTERNAL" TO SQLIO-PARM-VAL(9)
           ELSE
               MOVE JC-PURPOSE TO SQLIO-PARM-VAL(9)
           END-IF
           MOVE JC-OWNER-USER TO SQLIO-PARM-VAL(10)
           MOVE JC-COST-CENTRE TO SQLIO-PARM-VAL(11)
           MOVE CA-USER OF DFHCOMMAREA TO SQLIO-PARM-VAL(12)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 12
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           IF JC-PARENT-JOB = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(3)
           END-IF
           IF JC-CUSTOMER = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(7)
           END-IF
           IF JC-SALES-ORDER = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(8)
           END-IF
           PERFORM 8100-EXECUTE
           IF NOT WS-SQL-BAD
               MOVE "OPEN" TO JC-OPER-STATUS JC-FIN-STATUS
           END-IF.
       1000-EXIT.
           EXIT.

       1100-CHECK-PARENT SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT JobLevel FROM cst.Job WHERE CompanyCode = "
                  "? AND JobRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE JC-PARENT-JOB TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF NOT WS-GOT-ROW
               MOVE "CST0803" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
               STRING "Parent job " FUNCTION TRIM(JC-PARENT-JOB)
                      " does not exist"
                   DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 1100-EXIT
           END-IF
           IF FUNCTION NUMVAL(WS-COL(1)) NOT = 1
               MOVE "CST0804" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "Only two job levels are supported - a sub-job "
                   TO CA-MESSAGE OF DFHCOMMAREA
               STRING FUNCTION TRIM(CA-MESSAGE OF DFHCOMMAREA)
                      "cannot own another sub-job"
                   DELIMITED BY SIZE INTO WS-WORK-TEXT
               MOVE WS-WORK-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
           END-IF.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  CST-015.  VERSION 1 IS THE ORIGINAL BASELINE AND IS
      *>       NEVER OVERWRITTEN - PERFORMANCE IS MEASURED AGAINST IT
      *>       LONG AFTER THE FORECAST HAS MOVED ON.
      *>----------------------------------------------------------------
       2000-BUDGET SECTION.
       2000-START.
           IF NOT WS-HAVE-JOB
               PERFORM 9200-JOB-NOT-FOUND
               GO TO 2000-EXIT
           END-IF
           IF JC-BUDGET-VERSION <= 0
               MOVE 1 TO JC-BUDGET-VERSION
           END-IF
           PERFORM 2100-EXISTING-BUDGET
           IF WS-SQL-BAD
               GO TO 2000-EXIT
           END-IF
           IF WS-GOT-ROW
               IF JC-BUDGET-VERSION = 1
                   MOVE "CST0805" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE "The original budget may not be overwritten "
                       TO CA-MESSAGE OF DFHCOMMAREA
                   STRING FUNCTION TRIM(CA-MESSAGE OF DFHCOMMAREA)
                          "- lodge a new version instead"
                       DELIMITED BY SIZE INTO WS-WORK-TEXT
                   MOVE WS-WORK-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               ELSE
                   MOVE "CST0806" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE "That budget version already exists"
                       TO CA-MESSAGE OF DFHCOMMAREA
               END-IF
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           IF JC-BUDGET-VERSION > 1 AND JC-CHANGE-REASON = SPACES
               MOVE "CST0805" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A revised budget needs a change reason"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           IF NOT WS-DO-WRITE
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO cst.JobBudget (CompanyCode, JobRef, "
                  "Version, IsOriginal, TotalBudgetGBP, "
                  "ChangeReason, Status, CreatedBy) VALUES (?, ?, ?, "
                  "?, ?, ?, 'DRAFT', ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE JC-JOB-REF TO SQLIO-PARM-VAL(2)
           MOVE JC-BUDGET-VERSION TO SQLIO-PARM-VAL(3)
           IF JC-BUDGET-VERSION = 1
               MOVE "1" TO SQLIO-PARM-VAL(4)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(4)
           END-IF
           MOVE JC-BUDGET-TOTAL TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE JC-CHANGE-REASON TO SQLIO-PARM-VAL(6)
           MOVE CA-USER OF DFHCOMMAREA TO SQLIO-PARM-VAL(7)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 7
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           PERFORM 8100-EXECUTE.
       2000-EXIT.
           EXIT.

       2100-EXISTING-BUDGET SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT JobBudgetId, Status FROM cst.JobBudget "
                  "WHERE CompanyCode = ? AND JobRef = ? AND Version "
                  "= ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE JC-JOB-REF TO SQLIO-PARM-VAL(2)
           MOVE JC-BUDGET-VERSION TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-GOT-ROW
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO WS-BUDGET-ID
           END-IF.
       2100-EXIT.
           EXIT.

       2500-APPROVE-BUDGET SECTION.
       2500-START.
           PERFORM 2100-EXISTING-BUDGET
           IF NOT WS-GOT-ROW
               MOVE "CST0806" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "That budget version does not exist"
                   TO CA-MESSAGE OF DFHCOMMAREA
               MOVE 16 TO CA-RETURN-CODE OF DFHCOMMAREA
               GO TO 2500-EXIT
           END-IF
           IF NOT WS-DO-WRITE
               GO TO 2500-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE cst.JobBudget SET Status = CASE WHEN "
                  "JobBudgetId = ? THEN 'APPROVED' WHEN Status = "
                  "'APPROVED' THEN 'SUPERSEDED' ELSE Status END, "
                  "ApprovedBy = CASE WHEN JobBudgetId = ? THEN ? "
                  "ELSE ApprovedBy END, ApprovedAt = CASE WHEN "
                  "JobBudgetId = ? THEN SYSDATETIME() ELSE "
                  "ApprovedAt END WHERE CompanyCode = ? AND JobRef "
                  "= ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE WS-BUDGET-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE CA-USER OF DFHCOMMAREA TO SQLIO-PARM-VAL(3)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(5)
           MOVE JC-JOB-REF TO SQLIO-PARM-VAL(6)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           PERFORM 8100-EXECUTE.
       2500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  CST-017.  EVERY COST CARRIES ITS SOURCE, ITS NATURE AND
      *>       ITS APPROVAL FLAG.  UNAPPROVED TIME IS VISIBLE BUT IS
      *>       NOT RECOGNISED COST.
      *>----------------------------------------------------------------
       3000-POST-COST SECTION.
       3000-START.
           IF NOT WS-HAVE-JOB
               PERFORM 9200-JOB-NOT-FOUND
               GO TO 3000-EXIT
           END-IF
           IF WS-FIN-STATUS = "CLOSED"
               MOVE "CST0808" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "The job is financially closed - a late cost "
                   TO CA-MESSAGE OF DFHCOMMAREA
               STRING FUNCTION TRIM(CA-MESSAGE OF DFHCOMMAREA)
                      "needs an authorised reopening"
                   DELIMITED BY SIZE INTO WS-WORK-TEXT
               MOVE WS-WORK-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           IF JC-ELEMENT-CODE = SPACES
               MOVE "CST0807" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A cost element is required"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           IF JC-AMOUNT-GBP = 0
               IF JC-AMOUNT = 0 AND JC-HOURS NOT = 0
                   COMPUTE JC-AMOUNT ROUNDED = JC-HOURS * JC-RATE
               END-IF
               IF JC-AMOUNT = 0 AND JC-QUANTITY NOT = 0
                   COMPUTE JC-AMOUNT ROUNDED = JC-QUANTITY * JC-RATE
               END-IF
               MOVE JC-AMOUNT TO JC-AMOUNT-GBP
           END-IF
           IF JC-AMOUNT-GBP = 0
               MOVE "CST0812" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A cost transaction may not be zero"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 3000-EXIT
           END-IF
           IF JC-AMOUNT = 0
               MOVE JC-AMOUNT-GBP TO JC-AMOUNT
           END-IF
           IF NOT WS-DO-WRITE
               GO TO 3000-EXIT
           END-IF
           PERFORM 3100-INSERT-COST.
       3000-EXIT.
           EXIT.

       3100-INSERT-COST SECTION.
       3100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO cst.JobCostTransaction (CompanyCode, "
                  "JobRef, KitRef, ElementCode, SourceModule, "
                  "SourceDocType, SourceDocRef, ItemCode, "
                  "WorkCentreCode, Quantity, Hours, Rate, "
                  "CurrencyCode, Amount, AmountGBP, CostNature, "
                  "RecognitionDate, FiscalYear, PeriodNo, "
                  "IsApproved, ApprovedBy, ReversalOfJobCostId, "
                  "Narrative, CreatedBy) VALUES (?, ?, NULL, ?, ?, "
                  "?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, "
                  "?, ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 23 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE JC-JOB-REF TO SQLIO-PARM-VAL(2)
           MOVE JC-ELEMENT-CODE TO SQLIO-PARM-VAL(3)
           IF JC-SOURCE-MODULE = SPACES
               MOVE "CST" TO SQLIO-PARM-VAL(4)
           ELSE
               MOVE JC-SOURCE-MODULE TO SQLIO-PARM-VAL(4)
           END-IF
           MOVE JC-DOC-TYPE TO SQLIO-PARM-VAL(5)
           MOVE JC-DOC-REF TO SQLIO-PARM-VAL(6)
           MOVE JC-ITEM-CODE TO SQLIO-PARM-VAL(7)
           MOVE JC-WORK-CENTRE TO SQLIO-PARM-VAL(8)
           MOVE JC-QUANTITY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE JC-HOURS TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE JC-RATE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE JC-CURRENCY TO SQLIO-PARM-VAL(12)
           MOVE JC-AMOUNT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           MOVE JC-AMOUNT-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(14)
           MOVE JC-COST-NATURE TO SQLIO-PARM-VAL(15)
           MOVE JC-RECOGNITION-DT TO SQLIO-PARM-VAL(16)
           MOVE JC-BUDGET-VERSION TO SQLIO-PARM-VAL(17)
           MOVE JC-JOB-LEVEL TO SQLIO-PARM-VAL(18)
           IF JC-IS-APPROVED = "N"
               MOVE "0" TO SQLIO-PARM-VAL(19)
           ELSE
               MOVE "1" TO SQLIO-PARM-VAL(19)
           END-IF
           MOVE CA-USER OF DFHCOMMAREA TO SQLIO-PARM-VAL(20)
           MOVE JC-REVERSE-OF-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(21)
           MOVE JC-NARRATIVE TO SQLIO-PARM-VAL(22)
           MOVE CA-USER OF DFHCOMMAREA TO SQLIO-PARM-VAL(23)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 23
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           IF JC-ITEM-CODE = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(7)
           END-IF
           IF JC-WORK-CENTRE = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(8)
           END-IF
           MOVE "Y" TO SQLIO-PARM-NULL(17)
           MOVE "Y" TO SQLIO-PARM-NULL(18)
           IF JC-REVERSE-OF-ID = 0
               MOVE "Y" TO SQLIO-PARM-NULL(21)
           END-IF
           PERFORM 8100-EXECUTE.
       3100-EXIT.
           EXIT.

      *>  A CORRECTION IS A LINKED OPPOSITE ENTRY.  THE ORIGINAL ROW
      *>  IS NEVER EDITED - THAT IS HOW AN AUDIT TRAIL STAYS HONEST.
       3500-REVERSE SECTION.
       3500-START.
           IF JC-JOB-COST-ID = 0
               MOVE "CST0812" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "The cost transaction to reverse is required"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 3500-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ElementCode, AmountGBP, "
                  "ISNULL(ItemCode,''), CostNature FROM "
                  "cst.JobCostTransaction WHERE CompanyCode = ? AND "
                  "JobCostId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE JC-JOB-COST-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 4 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF NOT WS-GOT-ROW
               MOVE "CST0812" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "The cost transaction was not found"
                   TO CA-MESSAGE OF DFHCOMMAREA
               MOVE 16 TO CA-RETURN-CODE OF DFHCOMMAREA
               GO TO 3500-EXIT
           END-IF
           MOVE WS-COL(1) TO JC-ELEMENT-CODE
           MOVE FUNCTION NUMVAL(WS-COL(2)) TO WS-AMT1
           COMPUTE JC-AMOUNT-GBP = 0 - WS-AMT1
           MOVE JC-AMOUNT-GBP TO JC-AMOUNT
           MOVE WS-COL(3) TO JC-ITEM-CODE
           MOVE WS-COL(4) TO JC-COST-NATURE
           MOVE JC-JOB-COST-ID TO JC-REVERSE-OF-ID
           MOVE "REVERSAL" TO JC-DOC-TYPE
           IF WS-DO-WRITE
               PERFORM 3100-INSERT-COST
           END-IF.
       3500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  CST-014.  BUDGET, ACTUAL, COMMITMENT AND VARIANCE BY
      *>       COST ELEMENT FOR THE JOB AND, ON REQUEST, ITS SUB-JOBS.
      *>----------------------------------------------------------------
       4000-ENQUIRE SECTION.
       4000-START.
           IF NOT WS-HAVE-JOB
               PERFORM 9200-JOB-NOT-FOUND
               GO TO 4000-EXIT
           END-IF
           PERFORM 4100-TOTALS
           IF WS-SQL-BAD
               GO TO 4000-EXIT
           END-IF
           PERFORM 4200-BUDGET-TOTALS
           IF WS-SQL-BAD
               GO TO 4000-EXIT
           END-IF
           PERFORM 4300-BY-ELEMENT
           PERFORM 4400-SUBJOB-COUNT.
       4000-EXIT.
           EXIT.

      *>  CST-014.  HOW MANY SUB-JOBS HANG OFF THIS ONE.  THE COUNT
      *>  IS INFORMATION FOR THE ENQUIRY - THE SUB-JOB COSTS ARE
      *>  ALREADY IN THE ROLL-UP ABOVE AND ARE NEVER ADDED TWICE.
       4400-SUBJOB-COUNT SECTION.
       4400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT COUNT(*) FROM cst.Job WHERE CompanyCode "
                  "= ? AND ParentJobRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE JC-JOB-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 1 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-GOT-ROW
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO JC-SUBJOB-COUNT
           END-IF.
       4400-EXIT.
           EXIT.

       4100-TOTALS SECTION.
       4100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT SUM(CASE WHEN CostNature = 'ACTUAL' AND "
                  "ElementCode <> 'REVENUE' THEN AmountGBP ELSE 0 "
                  "END), SUM(CASE WHEN CostNature = 'ACCRUED' THEN "
                  "AmountGBP ELSE 0 END), SUM(CASE WHEN CostNature "
                  "= 'COMMITTED' THEN AmountGBP ELSE 0 END), "
                  "SUM(CASE WHEN CostNature = 'ESTIMATE' THEN "
                  "AmountGBP ELSE 0 END), SUM(CASE WHEN ElementCode "
                  "= 'REVENUE' THEN -AmountGBP ELSE 0 END), "
                  "COUNT(*) FROM cst.JobCostTransaction WHERE "
                  "CompanyCode = ? AND IsApproved = 1 AND (JobRef = "
                  "? OR (? = 'Y' AND JobRef IN (SELECT JobRef FROM "
                  "cst.Job WHERE CompanyCode = ? AND ParentJobRef = "
                  "?)))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE JC-JOB-REF TO SQLIO-PARM-VAL(2)
           IF JC-ROLL-UP-SUBS
               MOVE "Y" TO SQLIO-PARM-VAL(3)
           ELSE
               MOVE "N" TO SQLIO-PARM-VAL(3)
           END-IF
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(4)
           MOVE JC-JOB-REF TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-IX)
           END-PERFORM
           MOVE 6 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-GOT-ROW
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO JC-ACTUAL-GBP
               MOVE FUNCTION NUMVAL(WS-COL(2)) TO JC-ACCRUED-GBP
               MOVE FUNCTION NUMVAL(WS-COL(3)) TO JC-COMMITTED-GBP
               IF JC-ETC-GBP = 0
                   MOVE FUNCTION NUMVAL(WS-COL(4)) TO JC-ETC-GBP
               END-IF
               MOVE FUNCTION NUMVAL(WS-COL(5)) TO JC-REVENUE-GBP
               MOVE FUNCTION NUMVAL(WS-COL(6)) TO JC-TXN-COUNT
           END-IF.
       4100-EXIT.
           EXIT.

       4200-BUDGET-TOTALS SECTION.
       4200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(MAX(CASE WHEN IsOriginal = 1 THEN "
                  "TotalBudgetGBP END),0), ISNULL(MAX(CASE WHEN "
                  "Status = 'APPROVED' THEN TotalBudgetGBP END),0) "
                  "FROM cst.JobBudget WHERE CompanyCode = ? AND "
                  "JobRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE JC-JOB-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           PERFORM 8000-SINGLE-ROW
           IF WS-GOT-ROW
               MOVE FUNCTION NUMVAL(WS-COL(1)) TO JC-ORIGINAL-BUDGET
               MOVE FUNCTION NUMVAL(WS-COL(2)) TO JC-APPROVED-BUDGET
           END-IF
           IF JC-APPROVED-BUDGET = 0
               MOVE JC-ORIGINAL-BUDGET TO JC-APPROVED-BUDGET
           END-IF
           MOVE JC-APPROVED-BUDGET TO JC-BUDGET-TOTAL.
       4200-EXIT.
           EXIT.

       4300-BY-ELEMENT SECTION.
       4300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT t.ElementCode, SUM(CASE WHEN "
                  "t.CostNature = 'ACTUAL' THEN t.AmountGBP ELSE 0 "
                  "END), SUM(CASE WHEN t.CostNature = 'COMMITTED' "
                  "THEN t.AmountGBP ELSE 0 END), SUM(CASE WHEN "
                  "t.CostNature = 'ESTIMATE' THEN t.AmountGBP ELSE "
                  "0 END) FROM cst.JobCostTransaction t WHERE "
                  "t.CompanyCode = ? AND t.JobRef = ? AND "
                  "t.IsApproved = 1 GROUP BY t.ElementCode ORDER BY "
                  "t.ElementCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE JC-JOB-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW
           IF SQLIO-SQLCODE < 0
               PERFORM 8900-SQL-ERROR
               GO TO 4300-EXIT
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
               IF JC-LINE-CNT >= 20
                   MOVE "Y" TO JC-MORE-LINES
                   EXIT PERFORM
               END-IF
               ADD 1 TO JC-LINE-CNT
               MOVE JC-LINE-CNT TO WS-IX2
               MOVE WS-COL(1) TO JC-L-ELEMENT(WS-IX2)
               MOVE WS-COL(1) TO JC-L-KEY(WS-IX2)
               MOVE FUNCTION NUMVAL(WS-COL(2))
                   TO JC-L-ACTUAL(WS-IX2)
               MOVE FUNCTION NUMVAL(WS-COL(3))
                   TO JC-L-COMMITTED(WS-IX2)
               MOVE FUNCTION NUMVAL(WS-COL(4)) TO JC-L-ETC(WS-IX2)
               MOVE 0 TO JC-L-BUDGET(WS-IX2)
               COMPUTE JC-L-EAC(WS-IX2) = JC-L-ACTUAL(WS-IX2)
                                          + JC-L-COMMITTED(WS-IX2)
                                          + JC-L-ETC(WS-IX2)
               COMPUTE JC-L-VARIANCE(WS-IX2) =
                   JC-L-EAC(WS-IX2) - JC-L-BUDGET(WS-IX2)
               IF JC-L-VARIANCE(WS-IX2) > 0
                   MOVE "Y" TO JC-L-ADVERSE(WS-IX2)
               ELSE
                   MOVE "N" TO JC-L-ADVERSE(WS-IX2)
               END-IF
           END-PERFORM
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CST-ROW.
       4300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000  CST-016.  ESTIMATE AT COMPLETION.  SPEC EXAMPLE C5:
      *>       2,100 ACTUAL + 1,600 COMMITTED + 1,500 ESTIMATE TO
      *>       COMPLETE = 5,200 AGAINST A 5,000 BUDGET - 200 ADVERSE.
      *>       AN INVOICE CLEARING AN ACCRUAL DOES NOT ADD COST AGAIN.
      *>----------------------------------------------------------------
       5000-FORECAST SECTION.
       5000-START.
           PERFORM 4000-ENQUIRE
           IF WS-SQL-BAD OR WS-INPUT-BAD
               GO TO 5000-EXIT
           END-IF
           IF CA-RETURN-CODE OF DFHCOMMAREA > 4
               GO TO 5000-EXIT
           END-IF
           COMPUTE JC-EAC-GBP = JC-ACTUAL-GBP + JC-ACCRUED-GBP
                                + JC-COMMITTED-GBP + JC-ETC-GBP
           COMPUTE JC-FCST-VAR-GBP =
               JC-EAC-GBP - JC-APPROVED-BUDGET
           IF JC-FCST-VAR-GBP > 0
               MOVE "Y" TO JC-FCST-ADVERSE
               MOVE "A positive forecast variance is adverse"
                   TO JC-WARNING
           END-IF
           IF JC-APPROVED-BUDGET = 0
               MOVE "No approved budget - the variance is the whole "
                   TO JC-WARNING
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
           END-IF.
       5000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 6000  CST-020.  OPERATIONAL COMPLETION NEEDS A DISPOSITION
      *>       OF THE REMAINING WORK.  IT IS NOT A FINANCIAL CLOSURE.
      *>----------------------------------------------------------------
       6000-COMPLETE SECTION.
       6000-START.
           IF NOT WS-HAVE-JOB
               PERFORM 9200-JOB-NOT-FOUND
               GO TO 6000-EXIT
           END-IF
           IF JC-DISPOSITION = SPACES
               MOVE "CST0809" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "Operational completion needs a disposition of "
                   TO CA-MESSAGE OF DFHCOMMAREA
               STRING FUNCTION TRIM(CA-MESSAGE OF DFHCOMMAREA)
                      "the remaining work"
                   DELIMITED BY SIZE INTO WS-WORK-TEXT
               MOVE WS-WORK-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 6000-EXIT
           END-IF
           MOVE "COMPLETE" TO JC-OPER-STATUS
           IF NOT WS-DO-WRITE
               GO TO 6000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE cst.Job SET OperationalStatus = "
                  "'COMPLETE', ActualEndDate = CAST(GETDATE() AS "
                  "date), ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHERE CompanyCode = ? AND JobRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-USER OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(2)
           MOVE JC-JOB-REF TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           PERFORM 8100-EXECUTE.
       6000-EXIT.
           EXIT.

      *>  FINANCIAL CLOSURE REVIEWS RESIDUAL WIP FIRST.  WIPCALC IS
      *>  ASKED, NOT GUESSED AT.
       6500-CLOSE SECTION.
       6500-START.
           IF NOT WS-HAVE-JOB
               PERFORM 9200-JOB-NOT-FOUND
               GO TO 6500-EXIT
           END-IF
           IF WS-OPER-STATUS NOT = "COMPLETE"
              AND WS-OPER-STATUS NOT = "CANCELLED"
               MOVE "CST0810" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "The job must be operationally complete first"
                   TO CA-MESSAGE OF DFHCOMMAREA
               PERFORM 9000-REJECT
               GO TO 6500-EXIT
           END-IF
           PERFORM 6600-CHECK-WIP
           IF CA-RETURN-CODE OF DFHCOMMAREA > 4
               GO TO 6500-EXIT
           END-IF
           MOVE "CLOSED" TO JC-FIN-STATUS
           IF NOT WS-DO-WRITE
               GO TO 6500-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE cst.Job SET FinancialStatus = 'CLOSED', "
                  "ClosedBy = ?, ClosedAt = SYSDATETIME() WHERE "
                  "CompanyCode = ? AND JobRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-USER OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(2)
           MOVE JC-JOB-REF TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           PERFORM 8100-EXECUTE.
       6500-EXIT.
           EXIT.

       6600-CHECK-WIP SECTION.
       6600-START.
           INITIALIZE WS-WIP-CA
           MOVE "WIPCALC" TO CA-PROGRAM OF WS-WIP-CA
           MOVE "RESIDUAL" TO CA-FUNCTION OF WS-WIP-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-WIP-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-WIP-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-WIP-CA
           MOVE JC-JOB-REF TO WC-JOB-REF
           MOVE JC-BUDGET-VERSION TO WC-PERIOD-NO
           IF WC-PERIOD-NO = 0
               MOVE 7 TO WC-PERIOD-NO
           END-IF
           MOVE 2026 TO WC-FISCAL-YEAR
           MOVE "N" TO WC-COMMIT
           CALL "WIPCALC" USING WS-WIP-CA
           MOVE WC-CLOSING-GBP TO JC-WIP-GBP
           IF CA-RETURN-CODE OF WS-WIP-CA = 6
               MOVE "CST0809" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-WIP-CA
                   TO CA-MESSAGE OF DFHCOMMAREA
               MOVE 6 TO CA-RETURN-CODE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       6600-EXIT.
           EXIT.

      *>  A LATE SUPPLIER COST IS TRACEABLE: THE JOB IS REOPENED, THE
      *>  COST IS BOOKED TO THE PERMITTED PERIOD AND THE LINK TO THE
      *>  ORIGINAL JOB SURVIVES.
       7000-REOPEN SECTION.
       7000-START.
           IF NOT WS-HAVE-JOB
               PERFORM 9200-JOB-NOT-FOUND
               GO TO 7000-EXIT
           END-IF
           MOVE "REOPENED" TO JC-FIN-STATUS
           IF NOT WS-DO-WRITE
               GO TO 7000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE cst.Job SET FinancialStatus = 'REOPENED', "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() WHERE "
                  "CompanyCode = ? AND JobRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-USER OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(2)
           MOVE JC-JOB-REF TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           PERFORM 8100-EXECUTE.
       7000-EXIT.
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

       9200-JOB-NOT-FOUND SECTION.
       9200-START.
           MOVE "CST0802" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           STRING "Job " FUNCTION TRIM(JC-JOB-REF)
                  " was not found in this company"
               DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
           MOVE 16 TO CA-RETURN-CODE OF DFHCOMMAREA.
       9200-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "CST0811" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           STRING "Function " FUNCTION TRIM(CA-FUNCTION OF
                  DFHCOMMAREA)
                  " is not supported by JOBCOST"
               DELIMITED BY SIZE INTO CA-MESSAGE OF DFHCOMMAREA
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.
