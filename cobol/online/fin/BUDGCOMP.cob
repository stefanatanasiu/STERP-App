      *>================================================================
      *> PROGRAM-ID : BUDGCOMP
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : GENERAL LEDGER
      *> PURPOSE    : BUDGET VERSIONS AND THE BUDGET TO ACTUAL
      *>              COMPARISON (FIN-009, FIN-023).
      *>              A REVISED FORECAST IS A NEW VERSION.  IT NEVER
      *>              OVERWRITES THE APPROVED BASELINE, AND A
      *>              COMMITMENT IS NEVER RELABELLED AS ACTUAL EXPENSE.
      *>              AC-012: AN APPROVED BUDGET OF 1,000.00 STAYS AT
      *>              1,000.00 AFTER A 1,100.00 FORECAST IS CREATED;
      *>              ACTUAL EXPENSE OF 1,050.00 IS 50.00 ADVERSE TO
      *>              BUDGET AND 50.00 BELOW FORECAST, AND EVERY
      *>              STATEMENT DRILLS TO THE SAME POSTED 1,050.00.
      *> CALLED BY  : BUDGRPT, frmGenericMaint, .NET CLIENT
      *> INTERFACE  : CALL "BUDGCOMP" USING WS-BUDGCOMP-CA
      *>              COPYBOOK BUDGCOMPCA
      *> CALLS      : PERMCHK, AUDITLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1995-04-11 DRO CR0255 ORIGINAL - MICRO FOCUS COBOL/2
      *> 2003-02-17 APW CR1655 FORECAST VERSIONS SEPARATED FROM THE
      *>                APPROVED BUDGET AFTER THE 2002 AUDIT POINT
      *> 2026-10-05 STERP VARIANCE SENSE BY ACCOUNT CLASS
      *>================================================================
      *> REQ: FIN-009, FIN-023
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUDGCOMP.
       AUTHOR. D R OKONJO.
       DATE-WRITTEN. 11/04/1995.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY PERMCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-PERMCHK-CA==
                      LEADING ==CA-== BY ==KH-==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUDITLOG-CA==
                      LEADING ==CA-== BY ==AH-==.
       01  WS-SWITCHES.
           05  WS-REJECTED             PIC X VALUE "N".
               88  WS-IS-REJECTED                VALUE "Y".
       01  WS-WORK.
           05  WS-IDX                  PIC S9(4) COMP-5 VALUE 0.
           05  WS-PERM                 PIC X(40).
           05  WS-MONEY-ED             PIC -(11)9.99.
       01  WS-CMP-ROW.
           05  WS-C-ACCOUNT            PIC X(12).
           05  WS-C-NAME               PIC X(60).
           05  WS-C-STATEMENT          PIC X(4).
           05  WS-C-CC                 PIC X(20).
           05  WS-C-CODE               PIC X(20).
           05  WS-C-VERSION            PIC X(8).
           05  WS-C-BUDGET             PIC X(22).
           05  WS-C-ACTUAL             PIC X(22).
           05  WS-C-VARIANCE           PIC X(22).
           05  WS-C-SENSE              PIC X(12).
           05  WS-C-CMPCODE            PIC X(20).
           05  WS-C-CMPVER             PIC X(8).
           05  WS-C-COMPARE            PIC X(22).
           05  WS-C-CMPVAR             PIC X(22).
       01  WS-VER-ROW.
           05  WS-V-VERSION            PIC X(8).
           05  WS-V-STATUS             PIC X(10).
           05  WS-V-BASELINE           PIC X(4).
       LINKAGE SECTION.
       COPY BUDGCOMPCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           PERFORM 0200-EDIT-REQUEST THRU 0200-EXIT
           IF WS-IS-REJECTED
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "COMPARE"  PERFORM 1000-COMPARE THRU 1000-EXIT
               WHEN "VERSION"  PERFORM 2000-NEW-VERSION THRU 2000-EXIT
               WHEN "APPROVE"  PERFORM 3000-APPROVE THRU 3000-EXIT
               WHEN "LIST"     PERFORM 4000-LIST THRU 4000-EXIT
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       0000-RETURN.
           GOBACK.

       0100-INITIALISE.
           MOVE "N" TO WS-REJECTED
           MOVE "BUDGCOMP" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           MOVE "FIN-009" TO CA-REQ-REF
           MOVE 0 TO BC-BUDGET-GBP
           MOVE 0 TO BC-ACTUAL-GBP
           MOVE 0 TO BC-VARIANCE-GBP
           MOVE 0 TO BC-COMPARE-GBP
           MOVE 0 TO BC-COMPARE-VAR
           MOVE 0 TO BC-ROW-CNT
           MOVE 0 TO BC-NEW-VERSION
           MOVE SPACES TO BC-VARIANCE-SENSE
           MOVE SPACES TO BC-STATUS
           MOVE "N" TO BC-IS-BASELINE
           IF BC-PERIOD-FROM = 0
               MOVE 1 TO BC-PERIOD-FROM
           END-IF
           IF BC-PERIOD-TO = 0
               MOVE 12 TO BC-PERIOD-TO
           END-IF
           MOVE FUNCTION UPPER-CASE(BC-BUDGET-CODE) TO BC-BUDGET-CODE
           MOVE FUNCTION UPPER-CASE(BC-COMPARE-CODE)
               TO BC-COMPARE-CODE.
       0100-EXIT.
           EXIT.

       0200-EDIT-REQUEST.
           IF CA-COMPANY = SPACES
               MOVE "BUD0001" TO CA-REASON-CODE
               MOVE "A company is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF BC-FISCAL-YEAR = 0
               MOVE "BUD0002" TO CA-REASON-CODE
               MOVE "A fiscal year is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 0200-EXIT
           END-IF
           IF BC-BUDGET-CODE = SPACES
               MOVE "BUD0003" TO CA-REASON-CODE
               MOVE "A budget code is required" TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000 - COMPARE ACTUALS WITH A NAMED VERSION
      *>----------------------------------------------------------------
       1000-COMPARE.
           MOVE 0 TO BC-ROW-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC fin.usp_BudgetCompare ?, ?, ?, ?, ?, ?, ?, ?, "
                  "?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 9 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE BC-FISCAL-YEAR TO SQLIO-PARM-VAL(2)
           MOVE BC-BUDGET-CODE TO SQLIO-PARM-VAL(3)
           IF BC-VERSION = 0
               MOVE SPACES TO SQLIO-PARM-VAL(4)
           ELSE
               MOVE BC-VERSION TO SQLIO-PARM-VAL(4)
           END-IF
           MOVE BC-PERIOD-FROM TO SQLIO-PARM-VAL(5)
           MOVE BC-PERIOD-TO TO SQLIO-PARM-VAL(6)
           MOVE BC-DIM-CC TO SQLIO-PARM-VAL(7)
           MOVE BC-COMPARE-CODE TO SQLIO-PARM-VAL(8)
           IF BC-COMPARE-VERSION = 0
               MOVE SPACES TO SQLIO-PARM-VAL(9)
           ELSE
               MOVE BC-COMPARE-VERSION TO SQLIO-PARM-VAL(9)
           END-IF
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           IF BC-VERSION = 0
               MOVE "Y" TO SQLIO-PARM-NULL(4)
           END-IF
           IF BC-DIM-CC = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(7)
           END-IF
           IF BC-COMPARE-CODE = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(8)
           END-IF
           IF BC-COMPARE-VERSION = 0
               MOVE "Y" TO SQLIO-PARM-NULL(9)
           END-IF
           MOVE 14 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 60 TO SQLIO-COL-WIDTH(2)
           MOVE 4 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 20 TO SQLIO-COL-WIDTH(5)
           MOVE 8 TO SQLIO-COL-WIDTH(6)
           MOVE 22 TO SQLIO-COL-WIDTH(7)
           MOVE 22 TO SQLIO-COL-WIDTH(8)
           MOVE 22 TO SQLIO-COL-WIDTH(9)
           MOVE 12 TO SQLIO-COL-WIDTH(10)
           MOVE 20 TO SQLIO-COL-WIDTH(11)
           MOVE 8 TO SQLIO-COL-WIDTH(12)
           MOVE 22 TO SQLIO-COL-WIDTH(13)
           MOVE 22 TO SQLIO-COL-WIDTH(14)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CMP-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 1000-EXIT
           END-IF
           PERFORM 1100-FETCH-ROW THRU 1100-EXIT
               UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CMP-ROW
           COMPUTE BC-VARIANCE-GBP = BC-ACTUAL-GBP - BC-BUDGET-GBP
           COMPUTE BC-COMPARE-VAR = BC-ACTUAL-GBP - BC-COMPARE-GBP
           PERFORM 1200-OVERALL-SENSE THRU 1200-EXIT
           MOVE SPACES TO CA-MESSAGE
           MOVE BC-VARIANCE-GBP TO WS-MONEY-ED
           STRING "Budget " FUNCTION TRIM(BC-BUDGET-CODE)
                  " version " BC-VERSION ": variance "
                  FUNCTION TRIM(WS-MONEY-ED) " GBP "
                  FUNCTION TRIM(FUNCTION LOWER-CASE(BC-VARIANCE-SENSE))
               DELIMITED BY SIZE INTO CA-MESSAGE.
       1000-EXIT.
           EXIT.

       1100-FETCH-ROW.
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-CMP-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               GO TO 1100-EXIT
           END-IF
           IF BC-VERSION = 0
               COMPUTE BC-VERSION = FUNCTION NUMVAL(WS-C-VERSION)
           END-IF
           IF BC-COMPARE-VERSION = 0 AND WS-C-CMPVER NOT = SPACES
               COMPUTE BC-COMPARE-VERSION =
                   FUNCTION NUMVAL(WS-C-CMPVER)
           END-IF
           COMPUTE BC-BUDGET-GBP =
               BC-BUDGET-GBP + FUNCTION NUMVAL(WS-C-BUDGET)
           COMPUTE BC-ACTUAL-GBP =
               BC-ACTUAL-GBP + FUNCTION NUMVAL(WS-C-ACTUAL)
           COMPUTE BC-COMPARE-GBP =
               BC-COMPARE-GBP + FUNCTION NUMVAL(WS-C-COMPARE)
           IF BC-ROW-CNT < 40
               ADD 1 TO BC-ROW-CNT
               MOVE BC-ROW-CNT TO WS-IDX
               MOVE WS-C-ACCOUNT TO BC-R-ACCOUNT(WS-IDX)
               MOVE WS-C-NAME TO BC-R-NAME(WS-IDX)
               MOVE WS-C-CC TO BC-R-DIM-CC(WS-IDX)
               COMPUTE BC-R-BUDGET(WS-IDX) =
                   FUNCTION NUMVAL(WS-C-BUDGET)
               COMPUTE BC-R-ACTUAL(WS-IDX) =
                   FUNCTION NUMVAL(WS-C-ACTUAL)
               COMPUTE BC-R-VARIANCE(WS-IDX) =
                   FUNCTION NUMVAL(WS-C-VARIANCE)
               MOVE WS-C-SENSE TO BC-R-SENSE(WS-IDX)
               COMPUTE BC-R-COMPARE(WS-IDX) =
                   FUNCTION NUMVAL(WS-C-COMPARE)
           END-IF.
       1100-EXIT.
           EXIT.

      *> Spending more than budget is adverse; earning less than budget
      *> is also adverse.  The sense depends on the account, not on the
      *> sign of the arithmetic.
       1200-OVERALL-SENSE.
           IF BC-VARIANCE-GBP = 0
               MOVE "ON BUDGET" TO BC-VARIANCE-SENSE
               GO TO 1200-EXIT
           END-IF
           IF BC-BUDGET-GBP >= 0
               IF BC-VARIANCE-GBP > 0
                   MOVE "ADVERSE" TO BC-VARIANCE-SENSE
               ELSE
                   MOVE "FAVOURABLE" TO BC-VARIANCE-SENSE
               END-IF
           ELSE
               IF BC-VARIANCE-GBP > 0
                   MOVE "FAVOURABLE" TO BC-VARIANCE-SENSE
               ELSE
                   MOVE "ADVERSE" TO BC-VARIANCE-SENSE
               END-IF
           END-IF.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000 - CREATE A REVISED VERSION FROM AN EXISTING ONE
      *>        THE SOURCE VERSION IS COPIED, NOT CHANGED (FIN-009).
      *>----------------------------------------------------------------
       2000-NEW-VERSION.
           MOVE "FIN.BUDGET.MAINT" TO WS-PERM
           PERFORM 8100-AUTHORITY THRU 8100-EXIT
           IF WS-IS-REJECTED
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-READ-VERSION THRU 2100-EXIT
           IF CA-FAILED
               GO TO 2000-EXIT
           END-IF
           COMPUTE BC-NEW-VERSION = BC-VERSION + 1
           IF BC-BUDGET-TYPE = SPACES
               MOVE "FORECAST" TO BC-BUDGET-TYPE
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-VER-ROW
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.Budget (CompanyCode, BudgetCode, "
                  "FiscalYear, Name, BudgetType, Version, "
                  "CurrencyCode, Status, IsBaseline, OwnerUser, "
                  "CreatedBy) SELECT b.CompanyCode, b.BudgetCode, "
                  "b.FiscalYear, ?, ?, ?, b.CurrencyCode, 'DRAFT', 0, "
                  "?, ? FROM fin.Budget b WHERE b.CompanyCode = ? "
                  "AND b.BudgetCode = ? AND b.FiscalYear = ? "
                  "AND b.Version = ? AND NOT EXISTS (SELECT 1 FROM "
                  "fin.Budget x WHERE x.CompanyCode = b.CompanyCode "
                  "AND x.BudgetCode = b.BudgetCode AND x.FiscalYear = "
                  "b.FiscalYear AND x.Version = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 9 TO SQLIO-PARM-CNT
           IF BC-NAME = SPACES
               MOVE SPACES TO BC-NAME
               STRING "Revision " BC-NEW-VERSION
                   DELIMITED BY SIZE INTO BC-NAME
           END-IF
           MOVE BC-NAME TO SQLIO-PARM-VAL(1)
           MOVE BC-BUDGET-TYPE TO SQLIO-PARM-VAL(2)
           MOVE BC-NEW-VERSION TO SQLIO-PARM-VAL(3)
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           MOVE CA-USER TO SQLIO-PARM-VAL(5)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(6)
           MOVE BC-BUDGET-CODE TO SQLIO-PARM-VAL(7)
           MOVE BC-FISCAL-YEAR TO SQLIO-PARM-VAL(8)
           MOVE BC-VERSION TO SQLIO-PARM-VAL(9)
           MOVE BC-NEW-VERSION TO SQLIO-PARM-VAL(10)
           MOVE 10 TO SQLIO-PARM-CNT
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-VER-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-VER-ROW
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-COPY-LINES THRU 2200-EXIT
           IF CA-FAILED
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-VER-ROW
               GO TO 2000-EXIT
           END-IF
           MOVE "COMT" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-VER-ROW
           MOVE "DRAFT" TO BC-STATUS
           PERFORM 8200-AUDIT THRU 8200-EXIT
           MOVE SPACES TO CA-MESSAGE
           STRING "Version " BC-NEW-VERSION " created as a "
                  FUNCTION TRIM(FUNCTION LOWER-CASE(BC-BUDGET-TYPE))
                  ". The approved baseline is unchanged"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.

       2100-READ-VERSION.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CAST(b.Version AS varchar(8)), "
                  "b.Status, CAST(b.IsBaseline AS char(1)) "
                  "FROM fin.Budget b WHERE b.CompanyCode = ? "
                  "AND b.BudgetCode = ? AND b.FiscalYear = ? "
                  "AND (? = 0 OR b.Version = ?) "
                  "ORDER BY b.Version DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE BC-BUDGET-CODE TO SQLIO-PARM-VAL(2)
           MOVE BC-FISCAL-YEAR TO SQLIO-PARM-VAL(3)
           MOVE BC-VERSION TO SQLIO-PARM-VAL(4)
           MOVE BC-VERSION TO SQLIO-PARM-VAL(5)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 4 TO SQLIO-COL-WIDTH(3)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-VER-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "BUD0010" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Budget " FUNCTION TRIM(BC-BUDGET-CODE)
                      " is not held for that year"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 2100-EXIT
           END-IF
           COMPUTE BC-VERSION = FUNCTION NUMVAL(WS-V-VERSION)
           MOVE WS-V-STATUS TO BC-STATUS
           IF WS-V-BASELINE(1:1) = "1"
               MOVE "Y" TO BC-IS-BASELINE
           ELSE
               MOVE "N" TO BC-IS-BASELINE
           END-IF.
       2100-EXIT.
           EXIT.

       2200-COPY-LINES.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.BudgetLine (CompanyCode, BudgetCode, "
                  "FiscalYear, Version, PeriodNo, AccountCode, "
                  "DimCostCentre, DimProject, Amount, Narrative) "
                  "SELECT l.CompanyCode, l.BudgetCode, l.FiscalYear, "
                  "?, l.PeriodNo, l.AccountCode, l.DimCostCentre, "
                  "l.DimProject, ROUND(l.Amount * (1 + ? / 100.0), 2),"
                  " l.Narrative FROM fin.BudgetLine l "
                  "WHERE l.CompanyCode = ? AND l.BudgetCode = ? "
                  "AND l.FiscalYear = ? AND l.Version = ? "
                  "AND NOT EXISTS (SELECT 1 FROM fin.BudgetLine x "
                  "WHERE x.CompanyCode = l.CompanyCode AND "
                  "x.BudgetCode = l.BudgetCode AND x.FiscalYear = "
                  "l.FiscalYear AND x.Version = ? AND x.PeriodNo = "
                  "l.PeriodNo AND x.AccountCode = l.AccountCode)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE BC-NEW-VERSION TO SQLIO-PARM-VAL(1)
           MOVE BC-UPLIFT-PCT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE BC-BUDGET-CODE TO SQLIO-PARM-VAL(4)
           MOVE BC-FISCAL-YEAR TO SQLIO-PARM-VAL(5)
           MOVE BC-VERSION TO SQLIO-PARM-VAL(6)
           MOVE BC-NEW-VERSION TO SQLIO-PARM-VAL(7)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-VER-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       2200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000 - APPROVE A VERSION AS THE BASELINE
      *>----------------------------------------------------------------
       3000-APPROVE.
           MOVE "FIN.BUDGET.APPROVE" TO WS-PERM
           PERFORM 8100-AUTHORITY THRU 8100-EXIT
           IF WS-IS-REJECTED
               GO TO 3000-EXIT
           END-IF
           IF BC-VERSION = 0
               MOVE "BUD0020" TO CA-REASON-CODE
               MOVE "The budget version to approve must be identified"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE fin.Budget SET Status = 'APPROVED', "
                  "IsBaseline = CASE WHEN BudgetType = 'BUDGET' THEN 1"
                  " ELSE IsBaseline END, ApprovedBy = ?, ApprovedAt = "
                  "SYSDATETIME(), ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHERE CompanyCode = ? "
                  "AND BudgetCode = ? AND FiscalYear = ? "
                  "AND Version = ? AND Status = 'DRAFT'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE BC-BUDGET-CODE TO SQLIO-PARM-VAL(4)
           MOVE BC-FISCAL-YEAR TO SQLIO-PARM-VAL(5)
           MOVE BC-VERSION TO SQLIO-PARM-VAL(6)
           PERFORM 8000-NOT-NULL THRU 8000-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-VER-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3000-EXIT
           END-IF
           IF SQLIO-ROWCOUNT = 0
               MOVE "BUD0021" TO CA-REASON-CODE
               MOVE "No draft version was found to approve"
                   TO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 3000-EXIT
           END-IF
           MOVE "APPROVED" TO BC-STATUS
           PERFORM 8200-AUDIT THRU 8200-EXIT
           MOVE SPACES TO CA-MESSAGE
           STRING "Budget " FUNCTION TRIM(BC-BUDGET-CODE)
                  " version " BC-VERSION " approved"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       3000-EXIT.
           EXIT.

       4000-LIST.
           PERFORM 2100-READ-VERSION THRU 2100-EXIT
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO CA-MESSAGE
           STRING "Latest version of " FUNCTION TRIM(BC-BUDGET-CODE)
                  " is " BC-VERSION ", status "
                  FUNCTION TRIM(BC-STATUS)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-NOT-NULL.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-IDX)
           END-PERFORM.
       8000-EXIT.
           EXIT.

       8100-AUTHORITY.
           INITIALIZE WS-PERMCHK-CA
           MOVE "CHECK" TO KH-FUNCTION
           MOVE CA-COMPANY TO KH-COMPANY
           MOVE CA-SITE TO KH-SITE
           MOVE CA-USER TO KH-USER
           MOVE CA-BUSINESS-DATE TO KH-BUSINESS-DATE
           MOVE WS-PERM TO PK-PERMISSION
           CALL "PERMCHK" USING WS-PERMCHK-CA
           IF KH-RETURN-CODE > 4
               MOVE KH-REASON-CODE TO CA-REASON-CODE
               MOVE KH-MESSAGE TO CA-MESSAGE
               MOVE KH-RETURN-CODE TO CA-RETURN-CODE
               MOVE "Y" TO WS-REJECTED
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       8100-EXIT.
           EXIT.

       8200-AUDIT.
           INITIALIZE WS-AUDITLOG-CA
           MOVE "WRITE" TO AH-FUNCTION
           MOVE CA-COMPANY TO AH-COMPANY
           MOVE CA-USER TO AH-USER
           MOVE CA-BUSINESS-DATE TO AH-BUSINESS-DATE
           MOVE "fin.Budget" TO AU-ENTITY-TYPE
           MOVE SPACES TO AU-ENTITY-KEY
           STRING FUNCTION TRIM(BC-BUDGET-CODE) "/" BC-FISCAL-YEAR
                  "/" BC-VERSION
               DELIMITED BY SIZE INTO AU-ENTITY-KEY
           MOVE CA-FUNCTION TO AU-ACTION
           MOVE BC-STATUS TO AU-NEW-VALUE
           MOVE "COBOL" TO AU-SOURCE
           CALL "AUDITLOG" USING WS-AUDITLOG-CA.
       8200-EXIT.
           EXIT.

       9000-REJECT.
           MOVE "Y" TO WS-REJECTED
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE "BUD0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "BUDGCOMP does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF CA-MESSAGE = SPACES
               MOVE "SQL failure in BUDGCOMP" TO CA-MESSAGE
           END-IF
           MOVE 12 TO CA-RETURN-CODE
           MOVE "Y" TO WS-REJECTED
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
