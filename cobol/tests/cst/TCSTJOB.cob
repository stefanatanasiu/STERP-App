      *>================================================================
      *> PROGRAM-ID : TCSTJOB
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COSTING AND PROFITABILITY (UNIT TESTS)
      *> PURPOSE    : JOB AND CONTRACT COSTING AGAINST REAL ROWS -
      *>              JOBCOST (JOBS, BUDGETS, POSTINGS AND THE
      *>              FORECAST AT COMPLETION) AND WIPCALC (THE WORK
      *>              IN PROGRESS ROLL-FORWARD).
      *>
      *>                C5  APPROVED BUDGET 5,000.00, ACTUAL COST TO
      *>                    DATE 2,100.00 AND COMMITMENTS OF 900.00
      *>                    FORECAST AT COMPLETION 3,000.00, SO THE
      *>                    VARIANCE IS 2,000.00 FAVOURABLE.
      *>                C6  OPENING 1,000.00 + ADDITIONS 1,300.00
      *>                    - DEDUCTIONS 1,100.00 = CLOSING 1,200.00
      *>
      *>              THE DRAFT BUDGET IS NOT THE BASELINE: ONLY THE
      *>              APPROVED VERSION IS (CST-015).  A SUB-JOB IS
      *>              COUNTED ONCE, IN THE PARENT ROLL-UP (D-13).
      *>
      *>              EVERY DATABASE CASE IS BUILT INSIDE ONE
      *>              TRANSACTION THAT IS ALWAYS ROLLED BACK, SO THE
      *>              SUITE LEAVES NOTHING BEHIND AND MAY BE RUN AS
      *>              OFTEN AS ANYONE LIKES.  THE ENGINES ARE CALLED
      *>              WITH COMMIT FLAG "T" - WRITE, BUT DO NOT OWN
      *>              THE TRANSACTION.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: CST-014, CST-015, CST-016, CST-017, CST-018, CST-020
      *> AC : AC-C5, AC-C6
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TCSTJOB.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY SQLIOCA.
       COPY JOBCOSTCA REPLACING ==DFHCOMMAREA== BY ==WS-JC-CA==.
       COPY WIPCALCCA REPLACING ==DFHCOMMAREA== BY ==WS-WC-CA==.
       01  WS-PROBE                    PIC X(60).
       01  WS-WORK.
           05  WS-DB-UP                PIC X VALUE "Y".
           05  WS-TABLES               PIC X VALUE "Y".
           05  WS-SQL-BAD              PIC X VALUE "N".
           05  WS-SQL-MSG              PIC X(200) VALUE SPACES.
           05  WS-IDENTITY             PIC S9(13)V9(2) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TCSTJOB jobs, budgets and WIP" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-ENGINES-REFUSE-NONSENSE
           PERFORM 8000-PROBE-DATABASE
           IF WS-DB-UP = "N" OR WS-TABLES = "N"
               PERFORM 8100-SKIP-DATABASE-TESTS
           ELSE
               PERFORM 8200-BEGIN-TRANSACTION
               PERFORM 2000-BUILD-THE-CONTRACT
               PERFORM 3000-BUDGET-IS-THE-BASELINE
               PERFORM 4000-FORECAST-AT-COMPLETION
               PERFORM 5000-WIP-ROLL-FORWARD
               PERFORM 8300-ROLLBACK-TRANSACTION
           END-IF
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
      *> 1000  BOTH ENGINES NAME THEIR OWN FAULTS BEFORE THEY READ A
      *>       SINGLE ROW, SO NO DATABASE IS NEEDED HERE.
      *>----------------------------------------------------------------
       1000-ENGINES-REFUSE-NONSENSE.
           PERFORM 1900-RESET-JOB
           MOVE SPACES TO JC-JOB-REF
           CALL "JOBCOST" USING WS-JC-CA
           MOVE "JOBCOST refuses an enquiry with no job reference"
               TO TK-NAME
           MOVE "CST0801" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-JC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "A refused enquiry forecasts nothing" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE JC-EAC-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 1950-RESET-WIP
           MOVE SPACES TO WC-JOB-REF
           CALL "WIPCALC" USING WS-WC-CA
           MOVE "WIPCALC refuses a roll-forward with nothing to roll"
               TO TK-NAME
           MOVE "CST0902" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-WC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 1950-RESET-WIP
           MOVE 0 TO WC-FISCAL-YEAR
           MOVE 0 TO WC-PERIOD-NO
           CALL "WIPCALC" USING WS-WC-CA
           MOVE "WIPCALC refuses a roll-forward with no period"
               TO TK-NAME
           MOVE "CST0903" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-WC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "A refused roll-forward closes at nothing" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE WC-CLOSING-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       1900-RESET-JOB.
           INITIALIZE WS-JC-CA
           MOVE "ENQUIRE" TO CA-FUNCTION OF WS-JC-CA
           MOVE "STC" TO CA-COMPANY OF WS-JC-CA
           MOVE "RDG" TO CA-SITE OF WS-JC-CA
           MOVE "COSTACC" TO CA-USER OF WS-JC-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-JC-CA
           MOVE "ZZCJOB1" TO JC-JOB-REF
           MOVE "Y" TO JC-INCLUDE-SUBJOBS
           MOVE "GBP" TO JC-CURRENCY
           MOVE "N" TO JC-COMMIT.

       1950-RESET-WIP.
           INITIALIZE WS-WC-CA
           MOVE "CALC" TO CA-FUNCTION OF WS-WC-CA
           MOVE "STC" TO CA-COMPANY OF WS-WC-CA
           MOVE "RDG" TO CA-SITE OF WS-WC-CA
           MOVE "COSTACC" TO CA-USER OF WS-WC-CA
           MOVE 20261006 TO CA-BUSINESS-DATE OF WS-WC-CA
           MOVE "ZZCJOB2" TO WC-JOB-REF
           MOVE 2027 TO WC-FISCAL-YEAR
           MOVE 04 TO WC-PERIOD-NO
           MOVE "N" TO WC-COMMIT.

      *>----------------------------------------------------------------
      *> 2000  A LEVEL ONE CONTRACT WITH ONE SUB-JOB, AN APPROVED
      *>       BUDGET OF 5,000.00 AND A DRAFT ONE OF 9,000.00 THAT
      *>       MUST BE IGNORED.  REQ: CST-014, CST-015
      *>----------------------------------------------------------------
       2000-BUILD-THE-CONTRACT.
           PERFORM 2900-CLEAR-PREVIOUS-ROWS
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT cst.Job (CompanyCode, JobRef, "
                  "ParentJobRef, JobLevel, Description, SiteCode, "
                  "Purpose, OwnerUser, StartDate, TargetEndDate, "
                  "OperationalStatus, FinancialStatus, CreatedBy) "
                  "VALUES ('STC','ZZCJOB1',NULL,1,"
                  "'Costing test contract','RDG','CUSTOMER','PM',"
                  "'2026-09-01','2026-12-01','OPEN','OPEN',"
                  "'TCSTJOB'),('STC','ZZCJOB1S','ZZCJOB1',2,"
                  "'Costing test sub-job','RDG','CUSTOMER','PM',"
                  "'2026-09-01','2026-11-01','OPEN','OPEN',"
                  "'TCSTJOB'),('STC','ZZCJOB2',NULL,1,"
                  "'Work in progress test','RDG','INTERNAL','PM',"
                  "'2026-09-01','2026-12-01','OPEN','OPEN',"
                  "'TCSTJOB')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT cst.JobBudget (CompanyCode, JobRef, "
                  "Version, IsOriginal, TotalBudgetGBP, "
                  "ChangeReason, Status, ApprovedBy, ApprovedAt, "
                  "CreatedBy) VALUES ('STC','ZZCJOB1',1,1,5000.00,"
                  "'Original estimate','APPROVED','FINMGR',"
                  "SYSUTCDATETIME(),'TCSTJOB'),('STC','ZZCJOB1',2,"
                  "0,9000.00,'Scope change under discussion',"
                  "'DRAFT',NULL,NULL,'TCSTJOB')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT cst.JobCostTransaction (CompanyCode, "
                  "JobRef, ElementCode, SourceModule, SourceDocType, "
                  "SourceDocRef, Amount, AmountGBP, CostNature, "
                  "RecognitionDate, FiscalYear, PeriodNo, "
                  "IsApproved, CreatedBy) VALUES "
                  "('STC','ZZCJOB1','MATERIAL','INV','ISSUE',"
                  "'ZZMV1',1500.00,1500.00,'ACTUAL','2026-10-02',"
                  "2027,4,1,'TCSTJOB'),"
                  "('STC','ZZCJOB1S','LABOUR','CST','TIME',"
                  "'ZZTM1',600.00,600.00,'ACTUAL','2026-10-03',"
                  "2027,4,1,'TCSTJOB'),"
                  "('STC','ZZCJOB1','SUBCON','PO','ORDER',"
                  "'ZZPO1',900.00,900.00,'COMMITTED','2026-10-04',"
                  "2027,4,1,'TCSTJOB')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT
           MOVE "The contract, its sub-job and its costs were built"
               TO TK-NAME
           IF WS-SQL-BAD = "Y"
               MOVE WS-SQL-MSG TO TK-DETAIL
               PERFORM TK-FAIL
           ELSE
               PERFORM TK-PASS
           END-IF.

       2900-CLEAR-PREVIOUS-ROWS.
           MOVE "N" TO WS-SQL-BAD
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM cst.JobCostTransaction WHERE "
                  "CompanyCode = 'STC' AND JobRef LIKE 'ZZCJOB%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM cst.WipBalance WHERE CompanyCode = "
                  "'STC' AND JobRef LIKE 'ZZCJOB%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM cst.JobBudgetLine WHERE JobBudgetId "
                  "IN (SELECT JobBudgetId FROM cst.JobBudget WHERE "
                  "CompanyCode = 'STC' AND JobRef LIKE 'ZZCJOB%')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM cst.JobBudget WHERE CompanyCode = "
                  "'STC' AND JobRef LIKE 'ZZCJOB%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM cst.Job WHERE CompanyCode = 'STC' "
                  "AND JobRef LIKE 'ZZCJOB%' ORDER BY JobLevel DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM cst.Job WHERE CompanyCode = 'STC' "
                  "AND JobRef LIKE 'ZZCJOB%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT.

      *>----------------------------------------------------------------
      *> 3000  CST-015.  THE BASELINE IS THE APPROVED VERSION.  A
      *>       DRAFT OF 9,000.00 SITTING BESIDE IT CHANGES NOTHING,
      *>       AND THE ORIGINAL MAY NOT BE QUIETLY OVERWRITTEN.
      *>----------------------------------------------------------------
       3000-BUDGET-IS-THE-BASELINE.
           PERFORM 1900-RESET-JOB
           CALL "JOBCOST" USING WS-JC-CA
           MOVE "The enquiry answers the contract" TO TK-NAME
           IF CA-RETURN-CODE OF WS-JC-CA > 4
               MOVE CA-MESSAGE OF WS-JC-CA TO TK-DETAIL
               PERFORM TK-FAIL
           ELSE
               PERFORM TK-PASS
           END-IF
           MOVE "The approved budget of 5,000.00 is the baseline"
               TO TK-NAME
           MOVE 5000.00 TO TK-EXPECTED-N
           MOVE JC-APPROVED-BUDGET TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The draft version of 9,000.00 is not the baseline"
               TO TK-NAME
           IF JC-APPROVED-BUDGET = 9000.00
               MOVE "a draft budget was taken as approved"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           ELSE
               PERFORM TK-PASS
           END-IF
           MOVE "The contract has one sub-job" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE JC-SUBJOB-COUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Actual cost is 2,100.00 - the sub-job counted once"
               TO TK-NAME
           MOVE 2100.00 TO TK-EXPECTED-N
           MOVE JC-ACTUAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "Commitments of 900.00 are not actual cost"
               TO TK-NAME
           MOVE 900.00 TO TK-EXPECTED-N
           MOVE JC-COMMITTED-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 3500-SUB-JOB-ALONE
           PERFORM 3700-REFUSE-A-THIRD-LEVEL.

      *>  THE SUB-JOB ON ITS OWN CARRIES ONLY ITS OWN 600.00, WHICH
      *>  IS WHY ADDING IT TO THE PARENT ROLL-UP WOULD DOUBLE COUNT.
       3500-SUB-JOB-ALONE.
           PERFORM 1900-RESET-JOB
           MOVE "ZZCJOB1S" TO JC-JOB-REF
           MOVE "N" TO JC-INCLUDE-SUBJOBS
           CALL "JOBCOST" USING WS-JC-CA
           MOVE "The sub-job carries its own 600.00 only" TO TK-NAME
           MOVE 600.00 TO TK-EXPECTED-N
           MOVE JC-ACTUAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The sub-job has no budget of its own" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE JC-APPROVED-BUDGET TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 1900-RESET-JOB
           MOVE "N" TO JC-INCLUDE-SUBJOBS
           CALL "JOBCOST" USING WS-JC-CA
           MOVE "The parent alone carries 1,500.00" TO TK-NAME
           MOVE 1500.00 TO TK-EXPECTED-N
           MOVE JC-ACTUAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>  D-13.  TWO LEVELS OF JOB, NO MORE.  A THIRD LEVEL IS NOT A
      *>  CONVENIENCE, IT IS A COST THAT NO ROLL-UP WOULD FIND.
       3700-REFUSE-A-THIRD-LEVEL.
           PERFORM 1900-RESET-JOB
           MOVE "CREATE" TO CA-FUNCTION OF WS-JC-CA
           MOVE "ZZCJOB3" TO JC-JOB-REF
           MOVE "ZZCJOB1S" TO JC-PARENT-JOB
           MOVE 3 TO JC-JOB-LEVEL
           MOVE "Sub-sub-job" TO JC-DESCRIPTION
           MOVE "CUSTOMER" TO JC-PURPOSE
           MOVE "T" TO JC-COMMIT
           CALL "JOBCOST" USING WS-JC-CA
           MOVE "A third job level is refused" TO TK-NAME
           IF CA-REASON-CODE OF WS-JC-CA = "CST0803" OR
              CA-REASON-CODE OF WS-JC-CA = "CST0804"
               PERFORM TK-PASS
           ELSE
               MOVE CA-REASON-CODE OF WS-JC-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           PERFORM 1900-RESET-JOB
           MOVE "POSTCOST" TO CA-FUNCTION OF WS-JC-CA
           MOVE "MATERIAL" TO JC-ELEMENT-CODE
           MOVE 0 TO JC-AMOUNT JC-AMOUNT-GBP
           MOVE "ACTUAL" TO JC-COST-NATURE
           MOVE "T" TO JC-COMMIT
           CALL "JOBCOST" USING WS-JC-CA
           MOVE "A cost posting of nothing is refused" TO TK-NAME
           MOVE "CST0812" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-JC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 1900-RESET-JOB
           MOVE "WIBBLE" TO CA-FUNCTION OF WS-JC-CA
           CALL "JOBCOST" USING WS-JC-CA
           MOVE "JOBCOST refuses an unknown function" TO TK-NAME
           MOVE "CST0811" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-JC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 1900-RESET-JOB
           MOVE "ZZNOSUCHJOB" TO JC-JOB-REF
           CALL "JOBCOST" USING WS-JC-CA
           MOVE "An enquiry on a job that does not exist is refused"
               TO TK-NAME
           MOVE "CST0802" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-JC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 4000  WORKED EXAMPLE C5.  CST-016.  THE FORECAST AT
      *>       COMPLETION IS ACTUAL + ACCRUED + COMMITTED + THE
      *>       ESTIMATE TO COMPLETE.  2,100 + 900 = 3,000, SO THE
      *>       5,000 BUDGET IS 2,000 TO THE GOOD - AND A COMMITMENT
      *>       THAT HAS NOT YET BECOME A COST IS STILL MONEY SPENT.
      *>----------------------------------------------------------------
       4000-FORECAST-AT-COMPLETION.
           PERFORM 1900-RESET-JOB
           MOVE "FORECAST" TO CA-FUNCTION OF WS-JC-CA
           CALL "JOBCOST" USING WS-JC-CA
           MOVE "C5 the forecast at completion is 3,000.00"
               TO TK-NAME
           MOVE 3000.00 TO TK-EXPECTED-N
           MOVE JC-EAC-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C5 the forecast variance is 2,000.00 favourable"
               TO TK-NAME
           MOVE -2000.00 TO TK-EXPECTED-N
           MOVE JC-FCST-VAR-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C5 a favourable forecast is not flagged adverse"
               TO TK-NAME
           IF JC-FCST-ADVERSE = "Y"
               PERFORM TK-FAIL
           ELSE
               PERFORM TK-PASS
           END-IF
           COMPUTE WS-IDENTITY = JC-ACTUAL-GBP + JC-ACCRUED-GBP
                               + JC-COMMITTED-GBP + JC-ETC-GBP
                               - JC-EAC-GBP
           MOVE "C5 the four parts of the forecast add up" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-IDENTITY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 4500-A-NEW-COST-MOVES-THE-FORECAST.

      *>  CST-017.  POSTING 2,400.00 OF LABOUR TAKES THE FORECAST TO
      *>  5,400.00, WHICH IS 400.00 OVER THE APPROVED BUDGET AND SO
      *>  BECOMES ADVERSE.  THE OVERRUN IS REPORTED, NOT BLOCKED.
       4500-A-NEW-COST-MOVES-THE-FORECAST.
           PERFORM 1900-RESET-JOB
           MOVE "POSTCOST" TO CA-FUNCTION OF WS-JC-CA
           MOVE "LABOUR" TO JC-ELEMENT-CODE
           MOVE "CST" TO JC-SOURCE-MODULE
           MOVE "TIME" TO JC-DOC-TYPE
           MOVE "ZZTM2" TO JC-DOC-REF
           MOVE 2400.00 TO JC-AMOUNT
           MOVE 2400.00 TO JC-AMOUNT-GBP
           MOVE "ACTUAL" TO JC-COST-NATURE
           MOVE 20261005 TO JC-RECOGNITION-DT
           MOVE "Y" TO JC-IS-APPROVED
           MOVE "T" TO JC-COMMIT
           CALL "JOBCOST" USING WS-JC-CA
           MOVE "A labour cost of 2,400.00 is posted to the job"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-JC-CA > 4
               MOVE CA-MESSAGE OF WS-JC-CA TO TK-DETAIL
               PERFORM TK-FAIL
           ELSE
               PERFORM TK-PASS
           END-IF
           PERFORM 1900-RESET-JOB
           MOVE "FORECAST" TO CA-FUNCTION OF WS-JC-CA
           CALL "JOBCOST" USING WS-JC-CA
           MOVE "Actual cost has risen to 4,500.00" TO TK-NAME
           MOVE 4500.00 TO TK-EXPECTED-N
           MOVE JC-ACTUAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The forecast at completion is now 5,400.00"
               TO TK-NAME
           MOVE 5400.00 TO TK-EXPECTED-N
           MOVE JC-EAC-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "The overrun of 400.00 is reported as adverse"
               TO TK-NAME
           MOVE 400.00 TO TK-EXPECTED-N
           MOVE JC-FCST-VAR-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "An overrun is flagged, never silently blocked"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE JC-FCST-ADVERSE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> 5000  WORKED EXAMPLE C6.  CST-018.  A FINISHED GOODS
      *>       TRANSFER TAKES VALUE OUT OF WORK IN PROGRESS; IT DOES
      *>       NOT LEAVE THE SAME MONEY IN BOTH PLACES.
      *>
      *>         opening   1,000.00
      *>         additions 1,300.00  (800 + 300 + 150 + 50)
      *>         deductions 1,100.00 (100 + 900 + 60 + 40)
      *>         closing   1,200.00
      *>----------------------------------------------------------------
       5000-WIP-ROLL-FORWARD.
           PERFORM 5900-BUILD-WIP-PERIOD
           IF WS-SQL-BAD = "Y"
               MOVE "C6 the WIP period could not be built" TO TK-NAME
               MOVE WS-SQL-MSG TO TK-DETAIL
               PERFORM TK-FAIL
               EXIT PARAGRAPH
           END-IF
           PERFORM 1950-RESET-WIP
           CALL "WIPCALC" USING WS-WC-CA
           MOVE "C6 the roll-forward is calculated" TO TK-NAME
           IF CA-RETURN-CODE OF WS-WC-CA > 4
               MOVE CA-MESSAGE OF WS-WC-CA TO TK-DETAIL
               PERFORM TK-FAIL
           ELSE
               PERFORM TK-PASS
           END-IF
           MOVE "C6 the opening balance is last period's closing"
               TO TK-NAME
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE WC-OPENING-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C6 material issues of 800.00 went in" TO TK-NAME
           MOVE 800.00 TO TK-EXPECTED-N
           MOVE WC-MAT-ISSUES-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C6 material returns of 100.00 came back out"
               TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE WC-MAT-RETURNS-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C6 labour of 300.00 was absorbed" TO TK-NAME
           MOVE 300.00 TO TK-EXPECTED-N
           MOVE WC-LABOUR-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C6 overhead of 150.00 was absorbed" TO TK-NAME
           MOVE 150.00 TO TK-EXPECTED-N
           MOVE WC-OVERHEAD-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C6 subcontract of 50.00 was absorbed" TO TK-NAME
           MOVE 50.00 TO TK-EXPECTED-N
           MOVE WC-SUBCON-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C6 the completion transfer of 900.00 left WIP"
               TO TK-NAME
           MOVE 900.00 TO TK-EXPECTED-N
           MOVE WC-TRANSFERS-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C6 scrap of 60.00 left WIP" TO TK-NAME
           MOVE 60.00 TO TK-EXPECTED-N
           MOVE WC-SCRAP-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C6 the write-off of 40.00 left WIP" TO TK-NAME
           MOVE 40.00 TO TK-EXPECTED-N
           MOVE WC-WRITEOFF-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C6 additions are 1,300.00" TO TK-NAME
           MOVE 1300.00 TO TK-EXPECTED-N
           MOVE WC-ADDITIONS-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C6 deductions are 1,100.00" TO TK-NAME
           MOVE 1100.00 TO TK-EXPECTED-N
           MOVE WC-DEDUCTIONS-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C6 the closing balance is 1,200.00" TO TK-NAME
           MOVE 1200.00 TO TK-EXPECTED-N
           MOVE WC-CLOSING-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           COMPUTE WS-IDENTITY = WC-OPENING-GBP + WC-ADDITIONS-GBP
                               - WC-DEDUCTIONS-GBP - WC-CLOSING-GBP
           MOVE "C6 the roll-forward identity holds" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-IDENTITY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 5500-POST-AND-READ-BACK.

      *>  CST-020.  THE RESIDUAL IS WHAT A CLOSURE WOULD STRAND.  IT
      *>  IS REPORTED SO THAT SOMEONE DECIDES, NOT DISCARDED.
       5500-POST-AND-READ-BACK.
           PERFORM 1950-RESET-WIP
           MOVE "POST" TO CA-FUNCTION OF WS-WC-CA
           MOVE "T" TO WC-COMMIT
           CALL "WIPCALC" USING WS-WC-CA
           MOVE "C6 the balance is posted" TO TK-NAME
           IF CA-RETURN-CODE OF WS-WC-CA > 4
               MOVE CA-MESSAGE OF WS-WC-CA TO TK-DETAIL
               PERFORM TK-FAIL
           ELSE
               PERFORM TK-PASS
           END-IF
           PERFORM 1950-RESET-WIP
           MOVE "ENQUIRE" TO CA-FUNCTION OF WS-WC-CA
           CALL "WIPCALC" USING WS-WC-CA
           MOVE "C6 the stored balance reads back at 1,200.00"
               TO TK-NAME
           MOVE 1200.00 TO TK-EXPECTED-N
           MOVE WC-CLOSING-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 1950-RESET-WIP
           MOVE "RESIDUAL" TO CA-FUNCTION OF WS-WC-CA
           CALL "WIPCALC" USING WS-WC-CA
           MOVE "C6 a closure now would strand 1,200.00" TO TK-NAME
           MOVE 1200.00 TO TK-EXPECTED-N
           MOVE WC-RESIDUAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "C6 the residual is held, not quietly discarded"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE WC-HAS-RESIDUAL TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       5900-BUILD-WIP-PERIOD.
           MOVE "N" TO WS-SQL-BAD
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT cst.WipBalance (CompanyCode, JobRef, "
                  "FiscalYear, PeriodNo, OpeningValueGBP, "
                  "MaterialIssuesGBP, ClosingValueGBP) VALUES "
                  "('STC','ZZCJOB2',2027,3,0,1000.00,1000.00)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT cst.JobCostTransaction (CompanyCode, "
                  "JobRef, ElementCode, SourceModule, SourceDocType, "
                  "SourceDocRef, Amount, AmountGBP, CostNature, "
                  "RecognitionDate, FiscalYear, PeriodNo, "
                  "IsApproved, CreatedBy) SELECT 'STC','ZZCJOB2',"
                  "v.El,'CST','WIP',v.Ref,v.Amt,v.Amt,'ACTUAL',"
                  "CONVERT(date,'20261015',112),2027,4,1,'TCSTJOB' "
                  "FROM (VALUES ('MATERIAL','ZZW1',800.00),"
                  "('LABOUR','ZZW2',300.00),"
                  "('OVHDVAR','ZZW3',150.00),"
                  "('SUBCON','ZZW4',50.00),"
                  "('MATRETURN','ZZW5',100.00),"
                  "('TRANSFER','ZZW6',900.00),"
                  "('SCRAP','ZZW7',60.00),"
                  "('WRITEOFF','ZZW8',40.00)) v(El, Ref, Amt)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8600-RUN-STATEMENT.

      *>================================================================
      *> 8000  DATABASE PLUMBING.  PROBE, BEGIN, BUILD, ROLL BACK.
      *>================================================================
       8000-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(30), COUNT(*)) FROM "
                  "sys.objects o JOIN sys.schemas s ON s.schema_id "
                  "= o.schema_id WHERE s.name = 'cst' AND o.name IN "
                  "('Job','JobBudget','JobCostTransaction',"
                  "'WipBalance')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 8500-READ-SCALAR
           IF WS-SQL-BAD = "Y"
               MOVE "N" TO WS-DB-UP
               MOVE "N" TO WS-SQL-BAD
               EXIT PARAGRAPH
           END-IF
           IF FUNCTION NUMVAL (WS-PROBE) < 4
               MOVE "N" TO WS-TABLES
           END-IF.

       8100-SKIP-DATABASE-TESTS.
           MOVE "Job costing against real jobs, budgets and WIP"
               TO TK-NAME
           IF WS-DB-UP = "N"
               MOVE "no database connection" TO TK-DETAIL
           ELSE
               MOVE "the cst tables are not deployed" TO TK-DETAIL
           END-IF
           PERFORM TK-SKIP.

       8200-BEGIN-TRANSACTION.
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 7 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA, WS-PROBE.

       8300-ROLLBACK-TRANSACTION.
           MOVE "ROLB" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 7 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA, WS-PROBE.

       8500-READ-SCALAR.
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 60 TO SQLIO-COL-WIDTH (1)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           MOVE SPACES TO WS-PROBE
           CALL "SQLIO" USING SQLIO-CA, WS-PROBE
           IF SQLIO-SQLCODE < 0
               MOVE "Y" TO WS-SQL-BAD
               MOVE SQLIO-ERRMSG (1:200) TO WS-SQL-MSG
           END-IF.

       8600-RUN-STATEMENT.
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA, WS-PROBE
           IF SQLIO-SQLCODE < 0
               MOVE "Y" TO WS-SQL-BAD
               MOVE SQLIO-ERRMSG (1:200) TO WS-SQL-MSG
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA, WS-PROBE.

       COPY TESTKITP.
