      *>================================================================
      *> PROGRAM-ID : BUDGCHK
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : COMMITMENT ACCOUNTING AND BUDGET AUTHORITY.
      *>              COMPARES POSTED ACTUAL PLUS OPEN COMMITMENT PLUS
      *>              THE PROPOSED VALUE AGAINST THE APPROVED ANNUAL
      *>              BUDGET.  WARNS AT 90% AND BLOCKS AT 100% UNLESS
      *>              THE FINANCIAL CONTROLLER APPROVES (PO-005, D-08).
      *> CALLED BY  : POREQ, POORDER, POAPPRV, POCLOSE
      *> INTERFACE  : CALL "BUDGCHK" USING DFHCOMMAREA (BUDGCHKCA)
      *> CALLS      : SQLIO -> fin.Budget, fin.BudgetLine,
      *>              fin.Journal(Line), po.Commitment, po.Requisition
      *>----------------------------------------------------------------
      *> WHY THE COMMITMENT SUM ONLY COUNTS COMMITTED AND RELEASED:
      *> po.Commitment HOLDS ONE ROW PER STAGE MOVEMENT.  WHEN GOODS
      *> ARRIVE THE SAME MONEY MOVES FROM COMMITMENT TO ACCRUAL AND
      *> THEN TO PAYABLE, AND THE ACCRUAL IS ALREADY IN THE NOMINAL
      *> LEDGER.  ADDING ACCRUED AND INVOICED ROWS HERE WOULD COUNT
      *> THE SAME PURCHASE TWICE AND EXHAUST THE BUDGET ON PAPER.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-05-06 RJH  CR0802 COMMITMENT ACCOUNTING
      *> 2007-04-02 DJM  CR1422 SPLIT PURCHASE DETECTION
      *> 2026-10-06 STERP D-08 WARN 90 BLOCK 100
      *>================================================================
      *> REQ: PO-005, PO-017, D-08, D-07
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUDGCHK.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 06/05/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
           05  WS-NOBUDGET-SW          PIC X VALUE "N".
               88  WS-NOBUDGET                   VALUE "Y".
       01  WS-POLICY.
           05  WS-DEF-WARN-PCT         PIC S9(5)V9(4) VALUE 90.
           05  WS-DEF-BLOCK-PCT        PIC S9(5)V9(4) VALUE 100.
           05  WS-SPLIT-WINDOW         PIC S9(4) VALUE 30.
       01  WS-TWO-COL.
           05  WS-C1                   PIC X(24).
           05  WS-C2                   PIC X(12).
       01  WS-ONE-COL.
           05  WS-V1                   PIC X(24).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-YEAR                 PIC 9(4) VALUE 0.
           05  WS-YEAR-X               PIC X(4).
           05  WS-DATE-X               PIC X(8).
           05  WS-TOTAL-USED           PIC S9(13)V99 VALUE 0.
           05  WS-AFTER-USED           PIC S9(13)V99 VALUE 0.
           05  WS-WITH-RELATED         PIC S9(13)V99 VALUE 0.
           05  WS-RELATED-PCT          PIC S9(7)V9(4) VALUE 0.
           05  WS-AMT-ED               PIC -(11)9.99.
           05  WS-PCT-ED               PIC -(5)9.99.
           05  WS-CNT-ED               PIC Z(3)9.
           05  WS-NARRATIVE            PIC X(200).
           05  WS-STAGE                PIC X(12).
       LINKAGE SECTION.
       COPY BUDGCHKCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "CHECK"
                   PERFORM 2000-CHECK THRU 2000-EXIT
               WHEN "COMMIT"
                   IF NOT WS-NOBUDGET
                       PERFORM 2000-CHECK THRU 2000-EXIT
                   END-IF
                   IF NOT WS-FATAL AND
                      (CA-RETURN-CODE < 6 OR WS-NOBUDGET)
                       MOVE "COMMITTED" TO WS-STAGE
                       PERFORM 5000-MOVEMENT THRU 5000-EXIT
                       PERFORM 5100-UNBUDGETED THRU 5100-EXIT
                   END-IF
               WHEN "RELEASE"
                   MOVE "RELEASED" TO WS-STAGE
                   PERFORM 5000-MOVEMENT THRU 5000-EXIT
                   PERFORM 5100-UNBUDGETED THRU 5100-EXIT
               WHEN "OVERRIDE"
                   PERFORM 4000-OVERRIDE THRU 4000-EXIT
               WHEN OTHER
                   MOVE "BUD0006" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                          " is not supported by BUDGCHK"
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
           MOVE "N" TO WS-NOBUDGET-SW
           MOVE "BUDGCHK" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE 0 TO BC-BUDGET-GBP BC-ACTUAL-GBP BC-COMMITTED-GBP
           MOVE 0 TO BC-AVAILABLE-GBP BC-USED-PCT BC-USED-AFTER-PCT
           MOVE 0 TO BC-RELATED-CNT BC-RELATED-GBP BC-COMMITMENT-ID
           MOVE "WITHIN" TO BC-OUTCOME
           IF BC-WARN-PCT <= 0
               MOVE WS-DEF-WARN-PCT TO BC-WARN-PCT
           END-IF
           IF BC-BLOCK-PCT <= 0
               MOVE WS-DEF-BLOCK-PCT TO BC-BLOCK-PCT
           END-IF
           IF BC-MOVEMENT-DATE = 0
               MOVE CA-BUSINESS-DATE TO BC-MOVEMENT-DATE
           END-IF
           MOVE BC-MOVEMENT-DATE TO WS-DATE-X
           IF BC-FISCAL-YEAR = 0
               MOVE WS-DATE-X(1:4) TO WS-YEAR-X
               MOVE WS-YEAR-X TO BC-FISCAL-YEAR
           END-IF
           COMPUTE BC-PROPOSED-GBP =
                   BC-NET-GBP + BC-NONREC-VAT-GBP + BC-CHARGE-GBP
           IF CA-COMPANY = SPACES OR
              (BC-BUDGET-CODE = SPACES AND BC-COST-CENTRE = SPACES)
               IF CA-COMPANY NOT = SPACES AND
                  (CA-FUNCTION = "COMMIT" OR CA-FUNCTION = "RELEASE")
                   MOVE "Y" TO WS-NOBUDGET-SW
               ELSE
                   MOVE 8 TO CA-RETURN-CODE
                   MOVE "BUD0001" TO CA-REASON-CODE
                   MOVE "BLOCK" TO BC-OUTCOME
                   MOVE SPACES TO CA-MESSAGE
                   STRING "A company code and either a budget code "
                          "or a cost centre are needed for a "
                          "budget check"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE "Y" TO WS-FATAL-SW
               END-IF
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2000-CHECK SECTION.
       2000-START.
           PERFORM 2100-READ-BUDGET THRU 2100-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           IF BC-BUDGET-GBP = 0
               MOVE "Y" TO WS-NOBUDGET-SW
               MOVE 16 TO CA-RETURN-CODE
               MOVE "BUD0002" TO CA-REASON-CODE
               MOVE "BLOCK" TO BC-OUTCOME
               MOVE BC-FISCAL-YEAR TO WS-YEAR-X
               MOVE SPACES TO CA-MESSAGE
               STRING "No approved budget for "
                      FUNCTION TRIM(BC-BUDGET-CODE) " in "
                      WS-YEAR-X
                      " - the purchase cannot be committed against it"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-READ-ACTUAL THRU 2200-EXIT
           PERFORM 2300-READ-COMMITTED THRU 2300-EXIT
           PERFORM 2400-READ-RELATED THRU 2400-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           PERFORM 3000-ASSESS THRU 3000-EXIT.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE APPROVED ANNUAL BUDGET.  ONLY THE HIGHEST APPROVED
      *> VERSION COUNTS - A DRAFT RE-FORECAST GIVES NOBODY AUTHORITY.
      *>----------------------------------------------------------------
       2100-READ-BUDGET SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(l.Amount), 0) AS "
                  "varchar(24)), CAST(ISNULL(MAX(l.Version), 0) AS "
                  "varchar(12)) FROM fin.BudgetLine l WHERE "
                  "l.CompanyCode = ? AND l.FiscalYear = ? AND "
                  "(? = '' OR l.BudgetCode = ?) AND "
                  "(? = '' OR l.AccountCode = ?) AND "
                  "(? = '' OR ISNULL(l.DimCostCentre, '') = ?) AND "
                  "l.Version = (SELECT MAX(b.Version) FROM "
                  "fin.Budget b WHERE b.CompanyCode = l.CompanyCode "
                  "AND b.BudgetCode = l.BudgetCode AND b.FiscalYear "
                  "= l.FiscalYear AND b.Status = 'APPROVED')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE BC-FISCAL-YEAR TO SQLIO-PARM-VAL(2)
           MOVE BC-BUDGET-CODE TO SQLIO-PARM-VAL(3)
           MOVE BC-BUDGET-CODE TO SQLIO-PARM-VAL(4)
           MOVE BC-ACCOUNT-CODE TO SQLIO-PARM-VAL(5)
           MOVE BC-ACCOUNT-CODE TO SQLIO-PARM-VAL(6)
           MOVE BC-COST-CENTRE TO SQLIO-PARM-VAL(7)
           MOVE BC-COST-CENTRE TO SQLIO-PARM-VAL(8)
           MOVE 8 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 2100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 0 TO BC-BUDGET-GBP
               GO TO 2100-EXIT
           END-IF
           COMPUTE BC-BUDGET-GBP = FUNCTION NUMVAL(WS-C1).
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> POSTED ACTUAL FOR THE YEAR.  DEBITS LESS CREDITS, SO A
      *> CREDIT NOTE GIVES THE BUDGET ITS MONEY BACK.
      *>----------------------------------------------------------------
       2200-READ-ACTUAL SECTION.
       2200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(jl.DebitAmountGBP - "
                  "jl.CreditAmountGBP), 0) AS varchar(24)) FROM "
                  "fin.JournalLine jl JOIN fin.Journal j ON "
                  "j.JournalId = jl.JournalId WHERE jl.CompanyCode "
                  "= ? AND j.FiscalYear = ? AND j.Status = 'POSTED' "
                  "AND (? = '' OR jl.AccountCode = ?) AND "
                  "(? = '' OR ISNULL(jl.DimCostCentre, '') = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE BC-FISCAL-YEAR TO SQLIO-PARM-VAL(2)
           MOVE BC-ACCOUNT-CODE TO SQLIO-PARM-VAL(3)
           MOVE BC-ACCOUNT-CODE TO SQLIO-PARM-VAL(4)
           MOVE BC-COST-CENTRE TO SQLIO-PARM-VAL(5)
           MOVE BC-COST-CENTRE TO SQLIO-PARM-VAL(6)
           MOVE 6 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 2200-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               COMPUTE BC-ACTUAL-GBP = FUNCTION NUMVAL(WS-V1)
           END-IF.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2300-READ-COMMITTED SECTION.
       2300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(CASE c.Stage WHEN "
                  "'COMMITTED' THEN (c.AmountGBP + "
                  "c.NonRecoverableVatGBP + c.ChargeGBP) WHEN "
                  "'RELEASED' THEN -(c.AmountGBP + "
                  "c.NonRecoverableVatGBP + c.ChargeGBP) WHEN "
                  "'CANCELLED' THEN -(c.AmountGBP + "
                  "c.NonRecoverableVatGBP + c.ChargeGBP) ELSE 0 "
                  "END), 0) AS varchar(24)) FROM po.Commitment c "
                  "WHERE c.CompanyCode = ? AND ISNULL(c.FiscalYear, "
                  "0) IN (0, ?) AND (? = '' OR ISNULL(c.BudgetCode, "
                  "'') = ?) AND (? = '' OR ISNULL(c.DimCostCentre, "
                  "'') = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE BC-FISCAL-YEAR TO SQLIO-PARM-VAL(2)
           MOVE BC-BUDGET-CODE TO SQLIO-PARM-VAL(3)
           MOVE BC-BUDGET-CODE TO SQLIO-PARM-VAL(4)
           MOVE BC-COST-CENTRE TO SQLIO-PARM-VAL(5)
           MOVE BC-COST-CENTRE TO SQLIO-PARM-VAL(6)
           MOVE 6 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 2300-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               COMPUTE BC-COMMITTED-GBP = FUNCTION NUMVAL(WS-V1)
           END-IF.
       2300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> SPLIT PURCHASE DETECTION.  RELATED REQUESTS BY THE SAME
      *> REQUESTER ON THE SAME COST CENTRE INSIDE THE SPLITTING
      *> WINDOW ARE ADDED UP, SO THREE REQUISITIONS OF 4,000 CANNOT
      *> WALK UNDER A 5,000 THRESHOLD THAT ONE OF 12,000 WOULD HIT.
      *>----------------------------------------------------------------
       2400-READ-RELATED SECTION.
       2400-START.
           IF BC-REQUESTER = SPACES
               GO TO 2400-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(r.TotalValueGBP), 0) AS "
                  "varchar(24)), CAST(COUNT(*) AS varchar(12)) FROM "
                  "po.Requisition r WHERE r.CompanyCode = ? AND "
                  "r.RequesterUser = ? AND (? = '' OR "
                  "ISNULL(r.DimCostCentre, '') = ?) AND r.Status IN "
                  "('SUBMITTED', 'APPROVED', 'PARTCONV') AND "
                  "r.RequisitionNumber <> ? AND r.RequisitionDate >= "
                  "DATEADD(day, -1 * TRY_CONVERT(int, ?), "
                  "TRY_CONVERT(date, ?)) AND r.RequisitionDate <= "
                  "TRY_CONVERT(date, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE BC-REQUESTER TO SQLIO-PARM-VAL(2)
           MOVE BC-COST-CENTRE TO SQLIO-PARM-VAL(3)
           MOVE BC-COST-CENTRE TO SQLIO-PARM-VAL(4)
           MOVE BC-DOC-REF TO SQLIO-PARM-VAL(5)
           MOVE WS-SPLIT-WINDOW TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE BC-MOVEMENT-DATE TO SQLIO-PARM-VAL(7)
           MOVE BC-MOVEMENT-DATE TO SQLIO-PARM-VAL(8)
           MOVE 8 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 8
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 2400-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               COMPUTE BC-RELATED-GBP = FUNCTION NUMVAL(WS-C1)
               COMPUTE BC-RELATED-CNT = FUNCTION NUMVAL(WS-C2)
           END-IF.
       2400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE VERDICT.  THE PERCENTAGES ARE CALCULATED ON THE VALUE
      *> AFTER THIS PURCHASE, NOT BEFORE IT - OTHERWISE THE ORDER
      *> THAT BREAKS THE BUDGET IS ALWAYS THE ONE THAT PASSES.
      *>----------------------------------------------------------------
       3000-ASSESS SECTION.
       3000-START.
           COMPUTE WS-TOTAL-USED = BC-ACTUAL-GBP + BC-COMMITTED-GBP
           COMPUTE WS-AFTER-USED = WS-TOTAL-USED + BC-PROPOSED-GBP
           COMPUTE WS-WITH-RELATED = WS-AFTER-USED + BC-RELATED-GBP
           COMPUTE BC-AVAILABLE-GBP = BC-BUDGET-GBP - WS-TOTAL-USED
           COMPUTE BC-USED-PCT ROUNDED =
                   (WS-TOTAL-USED / BC-BUDGET-GBP) * 100
           COMPUTE BC-USED-AFTER-PCT ROUNDED =
                   (WS-AFTER-USED / BC-BUDGET-GBP) * 100
           COMPUTE WS-RELATED-PCT ROUNDED =
                   (WS-WITH-RELATED / BC-BUDGET-GBP) * 100
           MOVE BC-USED-AFTER-PCT TO WS-PCT-ED
           EVALUATE TRUE
               WHEN BC-USED-AFTER-PCT >= BC-BLOCK-PCT
                   MOVE "BLOCK" TO BC-OUTCOME
                   MOVE 6 TO CA-RETURN-CODE
                   MOVE "BUD0004" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "This purchase would take budget "
                          FUNCTION TRIM(BC-BUDGET-CODE) " to "
                          FUNCTION TRIM(WS-PCT-ED)
                          "% - the financial controller must "
                          "approve it before it can proceed"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               WHEN WS-RELATED-PCT >= BC-BLOCK-PCT AND
                    BC-RELATED-CNT > 0
                   MOVE "BLOCK" TO BC-OUTCOME
                   MOVE 6 TO CA-RETURN-CODE
                   MOVE "BUD0007" TO CA-REASON-CODE
                   MOVE BC-RELATED-CNT TO WS-CNT-ED
                   MOVE WS-RELATED-PCT TO WS-PCT-ED
                   MOVE SPACES TO CA-MESSAGE
                   STRING "With " FUNCTION TRIM(WS-CNT-ED)
                          " related open request(s) from the same "
                          "requester this takes the budget to "
                          FUNCTION TRIM(WS-PCT-ED)
                          "% and needs approval"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               WHEN BC-USED-AFTER-PCT >= BC-WARN-PCT
                   MOVE "WARN" TO BC-OUTCOME
                   MOVE 4 TO CA-RETURN-CODE
                   MOVE "BUD0003" TO CA-REASON-CODE
                   MOVE BC-AVAILABLE-GBP TO WS-AMT-ED
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Budget " FUNCTION TRIM(BC-BUDGET-CODE)
                          " reaches " FUNCTION TRIM(WS-PCT-ED)
                          "% after this purchase; "
                          FUNCTION TRIM(WS-AMT-ED)
                          " remains uncommitted"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               WHEN OTHER
                   MOVE "WITHIN" TO BC-OUTCOME
                   MOVE 0 TO CA-RETURN-CODE
           END-EVALUATE.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AN OVERRIDE IS A NAMED PERSON TAKING RESPONSIBILITY.  THE
      *> REASON IS KEPT ON THE COMMITMENT MOVEMENT ITSELF SO THAT
      *> THE COMMITMENT REPORT CAN SHOW WHY THE BUDGET WAS BROKEN.
      *>----------------------------------------------------------------
       4000-OVERRIDE SECTION.
       4000-START.
           IF BC-OVERRIDE-BY = SPACES OR BC-OVERRIDE-REASON = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "BUD0005" TO CA-REASON-CODE
               MOVE "BLOCK" TO BC-OUTCOME
               MOVE SPACES TO CA-MESSAGE
               STRING "A budget override needs both an approver "
                      "and a reason"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
           PERFORM 2000-CHECK THRU 2000-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           MOVE "COMMITTED" TO WS-STAGE
           MOVE SPACES TO WS-NARRATIVE
           STRING "Budget override by "
                  FUNCTION TRIM(BC-OVERRIDE-BY) ": "
                  FUNCTION TRIM(BC-OVERRIDE-REASON)
               DELIMITED BY SIZE INTO WS-NARRATIVE
           PERFORM 5000-MOVEMENT THRU 5000-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           MOVE "OVERRIDE" TO BC-OUTCOME
           MOVE 4 TO CA-RETURN-CODE
           MOVE "BUD0004" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "Budget override authorised by "
                  FUNCTION TRIM(BC-OVERRIDE-BY)
                  " - the commitment has been recorded"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> ONE ROW PER STAGE MOVEMENT.  NOTHING IS EVER UPDATED HERE.
      *>----------------------------------------------------------------
       5000-MOVEMENT SECTION.
       5000-START.
           IF BC-PROPOSED-GBP = 0
               COMPUTE BC-PROPOSED-GBP =
                   BC-NET-GBP + BC-NONREC-VAT-GBP + BC-CHARGE-GBP
           END-IF
           IF WS-NARRATIVE = SPACES
               MOVE SPACES TO WS-NARRATIVE
               STRING FUNCTION TRIM(WS-STAGE) " movement for "
                      FUNCTION TRIM(BC-DOC-TYPE) " "
                      FUNCTION TRIM(BC-DOC-REF)
                   DELIMITED BY SIZE INTO WS-NARRATIVE
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.Commitment (CompanyCode, OrderNumber, "
                  "LineNum, ScheduleNo, BudgetCode, AccountCode, "
                  "DimCostCentre, DimProject, FiscalYear, Stage, "
                  "MovementDate, Quantity, AmountGBP, "
                  "NonRecoverableVatGBP, ChargeGBP, SourceDocType, "
                  "SourceDocRef, Narrative, CreatedBy) OUTPUT "
                  "CAST(INSERTED.CommitmentId AS varchar(24)) "
                  "VALUES (?, ?, ?, NULLIF(?, 0), NULLIF(?, ''), "
                  "NULLIF(?, ''), NULLIF(?, ''), NULLIF(?, ''), ?, "
                  "?, TRY_CONVERT(date, ?), ?, ?, ?, ?, "
                  "NULLIF(?, ''), NULLIF(?, ''), ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE BC-DOC-REF TO SQLIO-PARM-VAL(2)
           MOVE BC-DOC-LINE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE BC-SCHEDULE-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE BC-BUDGET-CODE TO SQLIO-PARM-VAL(5)
           MOVE BC-ACCOUNT-CODE TO SQLIO-PARM-VAL(6)
           MOVE BC-COST-CENTRE TO SQLIO-PARM-VAL(7)
           MOVE BC-PROJECT TO SQLIO-PARM-VAL(8)
           MOVE BC-FISCAL-YEAR TO SQLIO-PARM-VAL(9)
           MOVE WS-STAGE TO SQLIO-PARM-VAL(10)
           MOVE BC-MOVEMENT-DATE TO SQLIO-PARM-VAL(11)
           MOVE BC-QUANTITY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE BC-NET-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           MOVE BC-NONREC-VAT-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(14)
           MOVE BC-CHARGE-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(15)
           MOVE BC-DOC-TYPE TO SQLIO-PARM-VAL(16)
           MOVE BC-DOC-REF TO SQLIO-PARM-VAL(17)
           MOVE WS-NARRATIVE TO SQLIO-PARM-VAL(18)
           MOVE CA-USER TO SQLIO-PARM-VAL(19)
           MOVE 19 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 19
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           COMPUTE BC-COMMITMENT-ID = FUNCTION NUMVAL(WS-V1).
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AN ENCUMBRANCE IS A FACT, NOT A PERMISSION (PO-005, PO-017).
      *> AN ORDER LINE THAT CARRIES NO BUDGET CODING, OR WHOSE CODING
      *> HAS NO APPROVED BUDGET FOR THE YEAR, STILL COMMITS THE
      *> COMPANY'S MONEY, SO THE MOVEMENT IS RECORDED AND REPORTED AS
      *> UNBUDGETED SPEND INSTEAD OF BEING DROPPED ON THE FLOOR.  THE
      *> VERDICT IS A WARNING, NOT A BLOCK - THERE IS NO BUDGET TO
      *> BREAK, SO THERE IS NOTHING FOR A CONTROLLER TO AUTHORISE.
      *>----------------------------------------------------------------
       5100-UNBUDGETED SECTION.
       5100-START.
           IF WS-FATAL OR NOT WS-NOBUDGET
               GO TO 5100-EXIT
           END-IF
           MOVE 4 TO CA-RETURN-CODE
           MOVE "BUD0008" TO CA-REASON-CODE
           MOVE "NOBUDGET" TO BC-OUTCOME
           MOVE BC-FISCAL-YEAR TO WS-YEAR-X
           MOVE BC-PROPOSED-GBP TO WS-AMT-ED
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(WS-STAGE) " commitment of "
                  FUNCTION TRIM(WS-AMT-ED) " recorded for "
                  FUNCTION TRIM(BC-DOC-TYPE) " "
                  FUNCTION TRIM(BC-DOC-REF)
                  "; no approved budget covers it in " WS-YEAR-X
               DELIMITED BY SIZE INTO CA-MESSAGE.
       5100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-SQL-FAIL SECTION.
       9000-START.
           MOVE 12 TO CA-RETURN-CODE
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           MOVE "BLOCK" TO BC-OUTCOME
           MOVE "Y" TO WS-FATAL-SW.
       9000-EXIT.
           EXIT.
       END PROGRAM BUDGCHK.
