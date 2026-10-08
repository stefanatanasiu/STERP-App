      *>================================================================
      *> PROGRAM-ID : TBUDGCHK
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : UNIT TESTS FOR BUDGCHK, THE PURCHASING BUDGET
      *>              AND AUTHORITY CHECK (PO-005, D-08).
      *>              THE SUITE BUILDS ITS OWN APPROVED BUDGET OF
      *>              10,000.00 THROUGH SQLIO AND DELETES IT AGAIN,
      *>              SO IT MAY BE RUN AS OFTEN AS YOU LIKE AGAINST A
      *>              LIVE DATABASE.  WITHOUT A CONNECTION IT SKIPS.
      *>
      *>              THE TWO RULES THAT MATTER ARE TESTED
      *>              DELIBERATELY:
      *>              1. THE PERCENTAGE IS MEASURED **AFTER** THE
      *>                 PURCHASE.  A CHECK TAKEN BEFORE IT WOULD LET
      *>                 THE VERY ORDER THAT EXHAUSTS THE BUDGET
      *>                 THROUGH, BECAUSE THE BUDGET IS ALWAYS INTACT
      *>                 UNTIL THE MOMENT IT IS NOT.
      *>              2. RELATED REQUESTS FROM THE SAME REQUESTER ON
      *>                 THE SAME COST CENTRE INSIDE THE 30 DAY
      *>                 WINDOW ARE ADDED UP, SO THREE REQUESTS OF
      *>                 4,000 CANNOT WALK UNDER A THRESHOLD THAT ONE
      *>                 OF 12,000 WOULD HIT.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: PO-005, PO-017, D-08
      *> AC: warn at 90 per cent, block at 100 per cent, split purchase
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TBUDGCHK.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY BUDGCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-BC==.
       COPY SQLIOCA.
       01  WS-ROW                      PIC X(256).
       01  WS-DB-OK                    PIC X VALUE "N".
       01  WS-BUD                      PIC X(20)
                                       VALUE "ZZ-TEST-BUD".
       01  WS-CC                       PIC X(20) VALUE "ZZTESTCC".
       01  WS-REQ-USER                 PIC X(20) VALUE "ZZTESTREQ".
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           MOVE "po BUDGCHK" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-CONNECT THRU 0100-EXIT
           IF WS-DB-OK = "Y"
               PERFORM 0200-CLEAN THRU 0200-EXIT
               PERFORM 0300-FIXTURES THRU 0300-EXIT
               PERFORM 1000-WITHIN THRU 1000-EXIT
               PERFORM 1100-JUST-UNDER THRU 1100-EXIT
               PERFORM 1200-WARN THRU 1200-EXIT
               PERFORM 1300-BLOCK THRU 1300-EXIT
               PERFORM 1400-COMMITTED THRU 1400-EXIT
               PERFORM 1500-SPLIT THRU 1500-EXIT
               PERFORM 1600-SPLIT-SCOPE THRU 1600-EXIT
               PERFORM 1700-OVERRIDE-NAME THRU 1700-EXIT
               PERFORM 1800-OVERRIDE-OK THRU 1800-EXIT
               PERFORM 1900-NO-BUDGET THRU 1900-EXIT
               PERFORM 2000-COMMIT-CODED THRU 2000-EXIT
               PERFORM 2100-COMMIT-UNCODED THRU 2100-EXIT
               PERFORM 2200-COMMIT-BLOCKED THRU 2200-EXIT
               PERFORM 0200-CLEAN THRU 0200-EXIT
               PERFORM 9000-DISCONNECT THRU 9000-EXIT
           ELSE
               MOVE "no database connection" TO TK-DETAIL
               MOVE "po BUDGCHK needs the STERP database" TO TK-NAME
               PERFORM TK-SKIP
           END-IF
           PERFORM TK-END
           STOP RUN.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0100-CONNECT SECTION.
       0100-START.
           INITIALIZE SQLIO-CA
           MOVE "CONN" TO SQLIO-FUNC
           MOVE SPACES TO SQLIO-STMT
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-SQLCODE = 0
               MOVE "Y" TO WS-DB-OK
           END-IF.
       0100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> EVERY FIXTURE CARRIES A ZZ PREFIX OF ITS OWN, SO A FAILED RUN
      *> CANNOT LEAVE A BUDGET OR A COMMITMENT BEHIND FOR THE NEXT
      *> ONE - AND CANNOT TOUCH A REAL ONE EITHER.
      *>----------------------------------------------------------------
       0200-CLEAN SECTION.
       0200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.Commitment WHERE "
                  "ISNULL(BudgetCode,'') = 'ZZ-TEST-BUD' OR "
                  "OrderNumber LIKE 'ZZ-TEST-ORD%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.Requisition WHERE "
                  "RequisitionNumber LIKE 'ZZ-TEST-REQ%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM fin.BudgetLine WHERE BudgetCode = "
                  "'ZZ-TEST-BUD'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM fin.Budget WHERE BudgetCode = "
                  "'ZZ-TEST-BUD'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT.
       0200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AN APPROVED ANNUAL BUDGET OF 10,000.00 ON COST CENTRE
      *> ZZTESTCC FOR 2027, PLUS A DRAFT VERSION 2 OF 99,000.00 THAT
      *> MUST BE IGNORED - A RE-FORECAST NOBODY HAS APPROVED GIVES
      *> NOBODY AUTHORITY TO SPEND.
      *>----------------------------------------------------------------
       0300-FIXTURES SECTION.
       0300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.Budget (CompanyCode, BudgetCode, "
                  "FiscalYear, Name, BudgetType, Version, "
                  "CurrencyCode, Status, CreatedBy) VALUES "
                  "('STC','ZZ-TEST-BUD',2027,'Budget check test',"
                  "'BUDGET',1,'GBP','APPROVED','TEST'), "
                  "('STC','ZZ-TEST-BUD',2027,'Draft reforecast',"
                  "'FORECAST',2,'GBP','DRAFT','TEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.BudgetLine (CompanyCode, BudgetCode, "
                  "FiscalYear, Version, PeriodNo, AccountCode, "
                  "DimCostCentre, Amount, Narrative) VALUES "
                  "('STC','ZZ-TEST-BUD',2027,1,1,'5300',"
                  "'ZZTESTCC',6000.00,'Test budget part one'), "
                  "('STC','ZZ-TEST-BUD',2027,1,2,'5300',"
                  "'ZZTESTCC',4000.00,'Test budget part two'), "
                  "('STC','ZZ-TEST-BUD',2027,2,1,'5300',"
                  "'ZZTESTCC',99000.00,'Unapproved reforecast')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT.
       0300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> ONE PLACE SETS UP A CHECK, SO THAT EVERY TEST DIFFERS ONLY IN
      *> THE FIGURE IT PROPOSES.
      *>----------------------------------------------------------------
       0950-ASK SECTION.
       0950-START.
           MOVE "BUDGCHK" TO CA-PROGRAM
           MOVE "STC" TO CA-COMPANY
           MOVE "RDG" TO CA-SITE
           MOVE "BUYER1" TO CA-USER
           MOVE 20270601 TO CA-BUSINESS-DATE
           MOVE 2027 TO BC-FISCAL-YEAR
           MOVE WS-CC TO BC-COST-CENTRE
           MOVE 20270601 TO BC-MOVEMENT-DATE
           CALL "BUDGCHK" USING WS-BC.
       0950-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 5,000 OF A 10,000 BUDGET IS HALF OF IT AND PASSES WITHOUT
      *> COMMENT.
      *>----------------------------------------------------------------
       1000-WITHIN SECTION.
       1000-START.
           INITIALIZE WS-BC
           MOVE "CHECK" TO CA-FUNCTION
           MOVE WS-BUD TO BC-BUDGET-CODE
           MOVE 5000.00 TO BC-NET-GBP
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "D-08 only the APPROVED version is the budget"
             TO TK-NAME
           MOVE 10000.00 TO TK-EXPECTED-N
           MOVE BC-BUDGET-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "D-08 5,000 of 10,000 is within budget" TO TK-NAME
           MOVE "WITHIN" TO TK-EXPECTED-X
           MOVE BC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "D-08 a within-budget check returns 00" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "D-08 nothing is used before the purchase" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE BC-USED-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "D-08 the purchase takes it to 50 per cent"
             TO TK-NAME
           MOVE 50 TO TK-EXPECTED-N
           MOVE BC-USED-AFTER-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "D-08 10,000 is still uncommitted" TO TK-NAME
           MOVE 10000.00 TO TK-EXPECTED-N
           MOVE BC-AVAILABLE-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 8,999.99 IS 89.9999 PER CENT.  THE WARNING IS AT 90, AND A
      *> BOUNDARY THAT IS NOT TESTED IS A BOUNDARY THAT IS WRONG.
      *>----------------------------------------------------------------
       1100-JUST-UNDER SECTION.
       1100-START.
           INITIALIZE WS-BC
           MOVE "CHECK" TO CA-FUNCTION
           MOVE WS-BUD TO BC-BUDGET-CODE
           MOVE 8999.99 TO BC-NET-GBP
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "D-08 89.9999 per cent is below the warning point"
             TO TK-NAME
           MOVE "WITHIN" TO TK-EXPECTED-X
           MOVE BC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "D-08 the warning point itself is reported"
             TO TK-NAME
           MOVE 90 TO TK-EXPECTED-N
           MOVE BC-WARN-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "D-08 the blocking point itself is reported"
             TO TK-NAME
           MOVE 100 TO TK-EXPECTED-N
           MOVE BC-BLOCK-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 9,000 IS EXACTLY 90 PER CENT AND WARNS.  THE PROOF THAT THE
      *> MEASURE IS TAKEN AFTER THE PURCHASE IS THAT NOTHING AT ALL IS
      *> USED BEFORE IT AND THE CHECK STILL WARNS.
      *>----------------------------------------------------------------
       1200-WARN SECTION.
       1200-START.
           INITIALIZE WS-BC
           MOVE "CHECK" TO CA-FUNCTION
           MOVE WS-BUD TO BC-BUDGET-CODE
           MOVE 8000.00 TO BC-NET-GBP
           MOVE 600.00 TO BC-NONREC-VAT-GBP
           MOVE 400.00 TO BC-CHARGE-GBP
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE SPACES TO TK-NAME
           STRING "PO-005 non recoverable VAT and charges are "
                  "committed too" DELIMITED BY SIZE INTO TK-NAME
           MOVE 9000.00 TO TK-EXPECTED-N
           MOVE BC-PROPOSED-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "D-08 90 per cent after the purchase warns"
             TO TK-NAME
           MOVE "WARN" TO TK-EXPECTED-X
           MOVE BC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "D-08 a warning returns 04 and proceeds" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "D-08 the warning carries reason BUD0003" TO TK-NAME
           MOVE "BUD0003" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "D-08 nothing was used BEFORE this purchase"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE BC-USED-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "D-08 the measure is taken AFTER the purchase"
             TO TK-NAME
           MOVE 90 TO TK-EXPECTED-N
           MOVE BC-USED-AFTER-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 10,000 EXHAUSTS THE BUDGET.  A BLOCK IS A HOLD (06), NOT A
      *> REJECTION - THE FINANCIAL CONTROLLER MAY STILL AUTHORISE IT.
      *>----------------------------------------------------------------
       1300-BLOCK SECTION.
       1300-START.
           INITIALIZE WS-BC
           MOVE "CHECK" TO CA-FUNCTION
           MOVE WS-BUD TO BC-BUDGET-CODE
           MOVE 10000.00 TO BC-NET-GBP
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "D-08 100 per cent after the purchase blocks"
             TO TK-NAME
           MOVE "BLOCK" TO TK-EXPECTED-X
           MOVE BC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "D-08 a block is a hold (06), not a rejection"
             TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "D-08 the block carries reason BUD0004" TO TK-NAME
           MOVE "BUD0004" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A COMMITMENT ALREADY MADE IS SPENT AUTHORITY EVEN THOUGH NO
      *> INVOICE HAS ARRIVED.  4,000 COMMITTED PLUS 5,000 PROPOSED IS
      *> 90 PER CENT OF 10,000, AND THE RELEASED 1,000 GIVES ITS
      *> AUTHORITY BACK.
      *>----------------------------------------------------------------
       1400-COMMITTED SECTION.
       1400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.Commitment (CompanyCode, OrderNumber, "
                  "LineNum, BudgetCode, DimCostCentre, FiscalYear, "
                  "Stage, MovementDate, Quantity, AmountGBP, "
                  "SourceDocType, CreatedBy) VALUES "
                  "('STC','ZZ-TEST-ORD',1,'ZZ-TEST-BUD',"
                  "'ZZTESTCC',2027,'COMMITTED','2027-05-01',1,"
                  "5000.00,'TEST','TEST'), "
                  "('STC','ZZ-TEST-ORD',1,'ZZ-TEST-BUD',"
                  "'ZZTESTCC',2027,'RELEASED','2027-05-15',1,"
                  "1000.00,'TEST','TEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           INITIALIZE WS-BC
           MOVE "CHECK" TO CA-FUNCTION
           MOVE WS-BUD TO BC-BUDGET-CODE
           MOVE 5000.00 TO BC-NET-GBP
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-017 a release gives its commitment back"
             TO TK-NAME
           MOVE 4000.00 TO TK-EXPECTED-N
           MOVE BC-COMMITTED-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-005 commitment counts before any invoice"
             TO TK-NAME
           MOVE 40 TO TK-EXPECTED-N
           MOVE BC-USED-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "D-08 4,000 committed plus 5,000 is 90 per cent"
             TO TK-NAME
           MOVE 90 TO TK-EXPECTED-N
           MOVE BC-USED-AFTER-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "D-08 6,000 of the budget is still uncommitted"
             TO TK-NAME
           MOVE 6000.00 TO TK-EXPECTED-N
           MOVE BC-AVAILABLE-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "D-08 a committed budget at 90 per cent warns"
             TO TK-NAME
           MOVE "WARN" TO TK-EXPECTED-X
           MOVE BC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.Commitment WHERE "
                  "ISNULL(BudgetCode,'') = 'ZZ-TEST-BUD'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT.
       1400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> CR1422.  TWO OPEN REQUESTS OF 4,000 FROM THE SAME REQUESTER
      *> ON THE SAME COST CENTRE, PLUS A THIRD OF 4,000, IS 12,000 OF
      *> A 10,000 BUDGET.  EACH ONE ON ITS OWN IS 40 PER CENT AND
      *> LOOKS HARMLESS.
      *>----------------------------------------------------------------
       1500-SPLIT SECTION.
       1500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.Requisition (CompanyCode, SiteCode, "
                  "RequisitionNumber, RequesterUser, Purpose, "
                  "RequisitionDate, DimCostCentre, TotalValueGBP, "
                  "BudgetCode, Status, CreatedBy) VALUES "
                  "('STC','RDG','ZZ-TEST-REQ-1','ZZTESTREQ',"
                  "'Split purchase part one','2027-05-20',"
                  "'ZZTESTCC',4000.00,'ZZ-TEST-BUD','SUBMITTED',"
                  "'TEST'), ('STC','RDG','ZZ-TEST-REQ-2',"
                  "'ZZTESTREQ','Split purchase part two',"
                  "'2027-05-25','ZZTESTCC',4000.00,'ZZ-TEST-BUD',"
                  "'APPROVED','TEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           INITIALIZE WS-BC
           MOVE "CHECK" TO CA-FUNCTION
           MOVE WS-BUD TO BC-BUDGET-CODE
           MOVE 4000.00 TO BC-NET-GBP
           MOVE WS-REQ-USER TO BC-REQUESTER
           MOVE "ZZ-TEST-REQ-3" TO BC-DOC-REF
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-005 this request on its own is only 40 per cent"
             TO TK-NAME
           MOVE 40 TO TK-EXPECTED-N
           MOVE BC-USED-AFTER-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "CR1422 two related open requests are found"
             TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE BC-RELATED-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "CR1422 the related requests total 8,000" TO TK-NAME
           MOVE 8000.00 TO TK-EXPECTED-N
           MOVE BC-RELATED-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "CR1422 split purchases are aggregated and blocked"
             TO TK-NAME
           MOVE "BLOCK" TO TK-EXPECTED-X
           MOVE BC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "CR1422 the aggregation carries reason BUD0007"
             TO TK-NAME
           MOVE "BUD0007" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE AGGREGATION IS SCOPED TO ONE REQUESTER.  A DIFFERENT
      *> BUYER'S REQUESTS ARE NOT THAT BUYER'S SPLIT PURCHASE, AND
      *> COUNTING THEM WOULD BLOCK HONEST ORDERS.
      *>----------------------------------------------------------------
       1600-SPLIT-SCOPE SECTION.
       1600-START.
           INITIALIZE WS-BC
           MOVE "CHECK" TO CA-FUNCTION
           MOVE WS-BUD TO BC-BUDGET-CODE
           MOVE 4000.00 TO BC-NET-GBP
           MOVE "ZZOTHERBUYER" TO BC-REQUESTER
           MOVE "ZZ-TEST-REQ-9" TO BC-DOC-REF
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "CR1422 another requester's requests are not counted"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE BC-RELATED-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "CR1422 so the same 4,000 passes for someone else"
             TO TK-NAME
           MOVE "WITHIN" TO TK-EXPECTED-X
           MOVE BC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AN OVERRIDE IS A NAMED PERSON TAKING RESPONSIBILITY.  WITHOUT
      *> A NAME AND A REASON IT IS NOT AN OVERRIDE, IT IS A HOLE.
      *>----------------------------------------------------------------
       1700-OVERRIDE-NAME SECTION.
       1700-START.
           INITIALIZE WS-BC
           MOVE "OVERRIDE" TO CA-FUNCTION
           MOVE WS-BUD TO BC-BUDGET-CODE
           MOVE 12000.00 TO BC-NET-GBP
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-005 an override without a name is refused"
             TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-005 the refusal carries reason BUD0005"
             TO TK-NAME
           MOVE "BUD0005" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-BC
           MOVE "OVERRIDE" TO CA-FUNCTION
           MOVE WS-BUD TO BC-BUDGET-CODE
           MOVE 12000.00 TO BC-NET-GBP
           MOVE "FINCTRL1" TO BC-OVERRIDE-BY
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-005 a name without a reason is refused too"
             TO TK-NAME
           MOVE "BUD0005" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1700-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> WITH BOTH, THE OVERRIDE IS RECORDED AS A COMMITMENT MOVEMENT
      *> CARRYING ITS REASON, SO THE COMMITMENT REPORT CAN SHOW WHY
      *> THE BUDGET WAS BROKEN AND BY WHOM.
      *>----------------------------------------------------------------
       1800-OVERRIDE-OK SECTION.
       1800-START.
           INITIALIZE WS-BC
           MOVE "OVERRIDE" TO CA-FUNCTION
           MOVE WS-BUD TO BC-BUDGET-CODE
           MOVE 12000.00 TO BC-NET-GBP
           MOVE "FINCTRL1" TO BC-OVERRIDE-BY
           MOVE SPACES TO BC-OVERRIDE-REASON
           STRING "Plant down, this is the only supplier who can "
                  "deliver this week" DELIMITED BY SIZE
               INTO BC-OVERRIDE-REASON
           MOVE "ZZ-TEST-ORD-OV" TO BC-DOC-REF
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-005 a named, reasoned override is accepted"
             TO TK-NAME
           MOVE "OVERRIDE" TO TK-EXPECTED-X
           MOVE BC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-017 the override is written as a commitment"
             TO TK-NAME
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(12)) FROM "
                  "po.Commitment WHERE ISNULL(BudgetCode,'') = "
                  "'ZZ-TEST-BUD' AND ISNULL(Narrative,'') LIKE "
                  "'%Plant down%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-COUNT THRU 0920-EXIT
           MOVE 1 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-ROW(1:12))
           PERFORM TK-ASSERT-NUM.
       1800-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> NO APPROVED BUDGET IS NOT AN UNLIMITED BUDGET.
      *>----------------------------------------------------------------
       1900-NO-BUDGET SECTION.
       1900-START.
           INITIALIZE WS-BC
           MOVE "CHECK" TO CA-FUNCTION
           MOVE "ZZ-TEST-NONE" TO BC-BUDGET-CODE
           MOVE 100.00 TO BC-NET-GBP
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-005 a missing budget is not an open cheque"
             TO TK-NAME
           MOVE "BLOCK" TO TK-EXPECTED-X
           MOVE BC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-005 a missing budget carries reason BUD0002"
             TO TK-NAME
           MOVE "BUD0002" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-BC
           MOVE "CHECK" TO CA-FUNCTION
           MOVE 100.00 TO BC-NET-GBP
           MOVE "BUDGCHK" TO CA-PROGRAM
           MOVE SPACES TO CA-COMPANY
           MOVE "BUYER1" TO CA-USER
           CALL "BUDGCHK" USING WS-BC
           MOVE "PO-005 a check with no company is refused"
             TO TK-NAME
           MOVE "BUD0001" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1900-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-005/PO-017.  ISSUING AN ORDER MUST LEAVE A COMMITTED
      *> MOVEMENT BEHIND IT - A COMMITMENT THAT IS NEVER WRITTEN IS A
      *> BUDGET THAT CANNOT BE BELIEVED.  A CODED LINE INSIDE ITS
      *> BUDGET COMMITS SILENTLY AND RETURNS THE ROW IT WROTE.
      *>----------------------------------------------------------------
       2000-COMMIT-CODED SECTION.
       2000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.Commitment WHERE "
                  "ISNULL(BudgetCode,'') = 'ZZ-TEST-BUD' OR "
                  "OrderNumber LIKE 'ZZ-TEST-ORD%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.Requisition WHERE "
                  "RequisitionNumber LIKE 'ZZ-TEST-REQ%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           INITIALIZE WS-BC
           MOVE "COMMIT" TO CA-FUNCTION
           MOVE WS-BUD TO BC-BUDGET-CODE
           MOVE "ZZ-TEST-ORD1" TO BC-DOC-REF
           MOVE "PURCHASEORDER" TO BC-DOC-TYPE
           MOVE 1 TO BC-DOC-LINE
           MOVE WS-REQ-USER TO BC-REQUESTER
           MOVE 2000.00 TO BC-NET-GBP
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-005 a coded commit inside budget is accepted"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-017 the commitment movement is returned"
             TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           IF BC-COMMITMENT-ID > 0
               MOVE "Y" TO TK-ACTUAL-X
           ELSE
               MOVE "N" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(12)) FROM "
                  "po.Commitment WHERE OrderNumber = 'ZZ-TEST-ORD1' "
                  "AND Stage = 'COMMITTED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-COUNT THRU 0920-EXIT
           MOVE "PO-017 issuing a coded line writes one COMMITTED row"
             TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-ROW(1:12))
           PERFORM TK-ASSERT-NUM.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AN ORDER LINE WITH NO BUDGET CODING STILL SPENDS THE
      *> COMPANY'S MONEY.  IT IS RECORDED AND REPORTED AS UNBUDGETED,
      *> NOT DROPPED: THERE IS NO BUDGET TO BREAK, SO IT WARNS
      *> RATHER THAN BLOCKS.
      *>----------------------------------------------------------------
       2100-COMMIT-UNCODED SECTION.
       2100-START.
           INITIALIZE WS-BC
           MOVE "COMMIT" TO CA-FUNCTION
           MOVE "ZZ-TEST-ORD2" TO BC-DOC-REF
           MOVE "PURCHASEORDER" TO BC-DOC-TYPE
           MOVE 1 TO BC-DOC-LINE
           MOVE 750.00 TO BC-NET-GBP
           MOVE "BUDGCHK" TO CA-PROGRAM
           MOVE "STC" TO CA-COMPANY
           MOVE "RDG" TO CA-SITE
           MOVE "BUYER1" TO CA-USER
           MOVE 20270601 TO CA-BUSINESS-DATE
           MOVE 2027 TO BC-FISCAL-YEAR
           MOVE 20270601 TO BC-MOVEMENT-DATE
           CALL "BUDGCHK" USING WS-BC
           MOVE "PO-017 an uncoded commit warns, it is not refused"
             TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-005 an uncoded commit is reported as NOBUDGET"
             TO TK-NAME
           MOVE "NOBUDGET" TO TK-EXPECTED-X
           MOVE BC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-005 unbudgeted spend carries reason BUD0008"
             TO TK-NAME
           MOVE "BUD0008" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(12)) FROM "
                  "po.Commitment WHERE OrderNumber = 'ZZ-TEST-ORD2' "
                  "AND Stage = 'COMMITTED' AND AmountGBP = 750.00"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-COUNT THRU 0920-EXIT
           MOVE "PO-017 an uncoded order still records its commitment"
             TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-ROW(1:12))
           PERFORM TK-ASSERT-NUM
           INITIALIZE WS-BC
           MOVE "RELEASE" TO CA-FUNCTION
           MOVE "ZZ-TEST-ORD2" TO BC-DOC-REF
           MOVE "PURCHASEORDER" TO BC-DOC-TYPE
           MOVE 1 TO BC-DOC-LINE
           MOVE 750.00 TO BC-NET-GBP
           MOVE "BUDGCHK" TO CA-PROGRAM
           MOVE "STC" TO CA-COMPANY
           MOVE "RDG" TO CA-SITE
           MOVE "BUYER1" TO CA-USER
           MOVE 20270601 TO CA-BUSINESS-DATE
           MOVE 2027 TO BC-FISCAL-YEAR
           MOVE 20270601 TO BC-MOVEMENT-DATE
           CALL "BUDGCHK" USING WS-BC
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(12)) FROM "
                  "po.Commitment WHERE OrderNumber = 'ZZ-TEST-ORD2' "
                  "AND Stage = 'RELEASED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-COUNT THRU 0920-EXIT
           MOVE "PO-017 an uncoded commitment can be released again"
             TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-ROW(1:12))
           PERFORM TK-ASSERT-NUM.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A COMMIT THAT WOULD BREAK A REAL BUDGET IS STILL REFUSED AND
      *> WRITES NOTHING - THE MONEY IS NOT ENCUMBERED UNTIL SOMEBODY
      *> WITH THE AUTHORITY SAYS SO.
      *>----------------------------------------------------------------
       2200-COMMIT-BLOCKED SECTION.
       2200-START.
           INITIALIZE WS-BC
           MOVE "COMMIT" TO CA-FUNCTION
           MOVE WS-BUD TO BC-BUDGET-CODE
           MOVE "ZZ-TEST-ORD3" TO BC-DOC-REF
           MOVE "PURCHASEORDER" TO BC-DOC-TYPE
           MOVE 1 TO BC-DOC-LINE
           MOVE 50000.00 TO BC-NET-GBP
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "D-08 a commit that breaks the budget is blocked"
             TO TK-NAME
           MOVE "BLOCK" TO TK-EXPECTED-X
           MOVE BC-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(12)) FROM "
                  "po.Commitment WHERE OrderNumber = 'ZZ-TEST-ORD3'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-COUNT THRU 0920-EXIT
           MOVE "D-08 a blocked purchase encumbers nothing"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-ROW(1:12))
           PERFORM TK-ASSERT-NUM.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0900-EXEC SECTION.
       0900-START.
           MOVE "EXEC" TO SQLIO-FUNC
           MOVE 8 TO SQLIO-CURSOR
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       0900-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0920-COUNT SECTION.
       0920-START.
           MOVE "SNGL" TO SQLIO-FUNC
           MOVE 8 TO SQLIO-CURSOR
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE SPACES TO WS-ROW
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       0920-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-DISCONNECT SECTION.
       9000-START.
           MOVE "DISC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       9000-EXIT.
           EXIT.
       COPY TESTKITP.
       END PROGRAM TBUDGCHK.
