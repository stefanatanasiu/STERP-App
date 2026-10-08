      *>================================================================
      *> PROGRAM-ID : TARCRED
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES LEDGER (AR) UNIT TESTS
      *> PURPOSE    : CREDCHK - CREDIT EXPOSURE AND THE RELEASE
      *>              DECISION.  USES THE WORKED NUMBERS OF THE
      *>              DEBTORS SPECIFICATION SECTION 6 AND ACCEPTANCE
      *>              SCENARIO AC-021:
      *>                open debt 800.00, eligible credit 100.00,
      *>                firm unshipped 350.00, shipped unbilled
      *>                150.00, deposit 50.00  ->  exposure 1,150.00
      *>                against a limit of 1,100.00
      *>              CREDCHK OWNS NO TRANSACTION, SO THE WHOLE SUITE
      *>              RUNS INSIDE ONE BEGN ... ROLB AND LEAVES NOTHING
      *>              BEHIND.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: AR-007, AR-008, D-06   AC: AC-021
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TARCRED.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY CREDCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-CK-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
       01  WS-PROBE.
           05  WS-PROBE-COL            PIC X(24).
           05  WS-PROBE-2              PIC X(24).
       01  WS-I                        PIC S9(4) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TARCRED credit exposure" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 0150-BEGIN-WORK
               PERFORM 0160-BUILD-FIXTURE
               PERFORM 1000-EXPOSURE-AC021
               PERFORM 1100-RELEASE-IS-BLOCKED
               PERFORM 1200-INPUT-GUARDS
               PERFORM 1300-ACCOUNT-STATE
               PERFORM 1400-OVERDUE-HOLDS-RELEASE
               PERFORM 1500-OVERRIDE-LIFECYCLE
               PERFORM 1600-STAGE-MOVEMENT
               PERFORM 0190-ROLL-BACK
           END-IF
           PERFORM TK-END
           STOP RUN.

       0100-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CustomerCode, '' FROM ar.Customer "
                  "WHERE CompanyCode = 'STC'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-DB-UP
           END-IF.

       0150-BEGIN-WORK.
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       0190-ROLL-BACK.
           MOVE "ROLB" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       0170-RUN-SQL.
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR
               DISPLAY "# fixture SQL: "
                       FUNCTION TRIM(SQLIO-ERRMSG) UPON SYSOUT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       0180-READ-ONE.
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 7 TO SQLIO-CURSOR
           MOVE SPACES TO WS-PROBE
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

      *>   AC-021 - the five legs of exposure, each counted once
       0160-BUILD-FIXTURE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT ar.Customer (CompanyCode, "
                  "CustomerCode, LegalName, LegalForm, VatLocation, "
                  "CountryCode, CurrencyCode, AllowedCurrencies, "
                  "Status, SanctionsStatus, CreatedBy) VALUES "
                  "('STC','ZZCRED1','Zed Credit Test Limited',"
                  "'LTD','GB','GB','GBP','GBP','ACTIVE','CLEAR',"
                  "'ZZARTEST'); "
                  "INSERT ar.CustomerCredit (CompanyCode, "
                  "CustomerCode, CreditLimitGBP, OverdueThresholdGBP,"
                  " OverdueDaysTrigger, CreatedBy) VALUES "
                  "('STC','ZZCRED1',1100.00,0,30,'ZZARTEST'); "
                  "INSERT ar.OpenItem (CompanyCode, CustomerCode, "
                  "ItemType, DocumentNumber, DocumentDate, DueDate, "
                  "PostingDate, CurrencyCode, FxRate, OriginalAmount,"
                  " OriginalAmountGBP, ResidualAmount, "
                  "ResidualAmountGBP, SettlementStatus, CreatedBy) "
                  "VALUES ('STC','ZZCRED1','INVOICE','ZZCR-INV-1',"
                  "'2026-10-01','2026-11-15','2026-10-01','GBP',1,"
                  "800.00,800.00,800.00,800.00,'OPEN','ZZARTEST'), "
                  "('STC','ZZCRED1','CREDIT','ZZCR-CRD-1',"
                  "'2026-10-01','2026-10-01','2026-10-01','GBP',1,"
                  "-100.00,-100.00,-100.00,-100.00,'OPEN',"
                  "'ZZARTEST'), "
                  "('STC','ZZCRED1','DEPOSIT','ZZCR-DEP-1',"
                  "'2026-10-01','2026-10-01','2026-10-01','GBP',1,"
                  "-50.00,-50.00,-50.00,-50.00,'OPEN','ZZARTEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0170-RUN-SQL
           PERFORM 0165-BUILD-ORDER-BOOK.

       0165-BUILD-ORDER-BOOK.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; DECLARE @so bigint, @l1 bigint, "
                  "@l2 bigint, @d bigint; "
                  "INSERT so.SalesOrder (CompanyCode, SiteCode, "
                  "OrderNumber, CustomerCode, OrderDate, "
                  "CurrencyCode, FxRate, CommercialStatus, CreatedBy)"
                  " VALUES ('STC','RDG','ZZCR-SO-1','ZZCRED1',"
                  "'2026-10-01','GBP',1,'ACCEPTED','ZZARTEST'); "
                  "SET @so = SCOPE_IDENTITY(); "
                  "INSERT so.SalesOrderLine (SalesOrderId, "
                  "CompanyCode, OrderNumber, LineNum, LineRef, "
                  "Description, LineType, AcceptedQty, CancelledQty, "
                  "DespatchedQty, BilledQty, UnitPrice, NetAmount, "
                  "VatAmount, Status) VALUES (@so,'STC','ZZCR-SO-1',"
                  "1,'001','Unshipped frame kit','STOCK',10,0,0,0,"
                  "30,300.00,50.00,'OPEN'); "
                  "SET @l1 = SCOPE_IDENTITY(); "
                  "INSERT so.SalesOrderLine (SalesOrderId, "
                  "CompanyCode, OrderNumber, LineNum, LineRef, "
                  "Description, LineType, AcceptedQty, CancelledQty, "
                  "DespatchedQty, BilledQty, UnitPrice, NetAmount, "
                  "VatAmount, Status) VALUES (@so,'STC','ZZCR-SO-1',"
                  "2,'002','Despatched frame kit','STOCK',10,0,10,0,"
                  "12.5,125.00,25.00,'OPEN'); "
                  "SET @l2 = SCOPE_IDENTITY(); "
                  "INSERT so.Despatch (CompanyCode, SiteCode, "
                  "DespatchRef, CustomerCode, OrderNumber, "
                  "DespatchDate, PostingDate, Status, CreatedBy) "
                  "VALUES ('STC','RDG','ZZCR-DSP-1','ZZCRED1',"
                  "'ZZCR-SO-1','2026-10-05','2026-10-05','CONFIRMED',"
                  "'ZZARTEST'); SET @d = SCOPE_IDENTITY(); "
                  "INSERT so.DespatchLine (DespatchId, LineNum, "
                  "SalesOrderLineId, OrderNumber, OrderLineNum, "
                  "ItemCode, Description, PickedQty, DespatchedQty, "
                  "StockQty, BilledQty) VALUES (@d,1,@l2,"
                  "'ZZCR-SO-1',2,'ZZCR-ITEM','Despatched frame kit',"
                  "10,10,10,0)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0170-RUN-SQL.

      *>   put the account back to its baseline between tests
       0200-RESET-ACCOUNT.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; UPDATE ar.Customer SET Status = "
                  "'ACTIVE', SanctionsStatus = 'CLEAR' WHERE "
                  "CompanyCode = 'STC' AND CustomerCode = 'ZZCRED1'; "
                  "UPDATE ar.CustomerCredit SET CreditLimitGBP = "
                  "1100.00, OverdueThresholdGBP = 0, "
                  "OverdueDaysTrigger = 30, HoldFlag = 0, "
                  "HoldReason = NULL, ProFormaOnly = 0, "
                  "RequiresPrepayment = 0 WHERE CompanyCode = 'STC' "
                  "AND CustomerCode = 'ZZCRED1'; "
                  "UPDATE ar.OpenItem SET DueDate = '2026-11-15' "
                  "WHERE CompanyCode = 'STC' AND DocumentNumber = "
                  "'ZZCR-INV-1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0170-RUN-SQL.

       0250-BUILD-CALL.
           INITIALIZE WS-CK-CA
           MOVE "STC" TO CA-COMPANY OF WS-CK-CA
           MOVE "RDG" TO CA-SITE OF WS-CK-CA
           MOVE "ARCLERK" TO CA-USER OF WS-CK-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-CK-CA
           MOVE "ZZCRED1" TO CK-CUSTOMER
           MOVE "ORDER" TO CK-CHECK-POINT
           MOVE 20261015 TO CK-EVAL-DATE
           MOVE "GBP" TO CK-CURRENCY
           MOVE "EXPOSURE" TO CA-FUNCTION OF WS-CK-CA.

      *>   AC-021 - 800 - 100 + 350 + 150 - 50 = 1,150.00
       1000-EXPOSURE-AC021.
           PERFORM 0250-BUILD-CALL
           CALL "CREDCHK" USING WS-CK-CA
           IF CA-RETURN-CODE OF WS-CK-CA = 12
               DISPLAY "# CREDCHK: "
                       FUNCTION TRIM(CA-MESSAGE OF WS-CK-CA)
                   UPON SYSOUT
           END-IF
           MOVE "exposure is computed" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "open invoice residuals are 800.00" TO TK-NAME
           MOVE 800.00 TO TK-EXPECTED-N
           MOVE CK-OPEN-DEBT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "eligible open credits are 100.00" TO TK-NAME
           MOVE 100.00 TO TK-EXPECTED-N
           MOVE CK-OPEN-CREDITS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "firm unshipped orders are 350.00" TO TK-NAME
           MOVE 350.00 TO TK-EXPECTED-N
           MOVE CK-FIRM-ORDERS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "despatched unbilled goods are 150.00" TO TK-NAME
           MOVE 150.00 TO TK-EXPECTED-N
           MOVE CK-SHIPPED-UNBILL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "eligible deposits are 50.00" TO TK-NAME
           MOVE 50.00 TO TK-EXPECTED-N
           MOVE CK-DEPOSITS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "exposure is 1,150.00" TO TK-NAME
           MOVE 1150.00 TO TK-EXPECTED-N
           MOVE CK-EXPOSURE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the approved limit is 1,100.00" TO TK-NAME
           MOVE 1100.00 TO TK-EXPECTED-N
           MOVE CK-LIMIT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "headroom is minus 50.00" TO TK-NAME
           MOVE -50.00 TO TK-EXPECTED-N
           MOVE CK-HEADROOM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the excess is 50.00" TO TK-NAME
           MOVE 50.00 TO TK-EXPECTED-N
           MOVE CK-EXCESS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the limit is 104.55% utilised" TO TK-NAME
           MOVE 104.55 TO TK-EXPECTED-N
           MOVE CK-UTILISED-PCT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AR-008 / AC-021 - release is blocked without an override
       1100-RELEASE-IS-BLOCKED.
           PERFORM 0250-BUILD-CALL
           MOVE "CHECK" TO CA-FUNCTION OF WS-CK-CA
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "a credit check over the limit holds" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the decision is a hold, not a block" TO TK-NAME
           MOVE "HOLD" TO TK-EXPECTED-X
           MOVE CK-DECISION TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the limit rule is quoted" TO TK-NAME
           MOVE "CRD0034" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the decision text states the excess" TO TK-NAME
           MOVE "Credit limit exceeded by " TO TK-EXPECTED-X
           MOVE CK-DECISION-TEXT(1:25) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0250-BUILD-CALL
           MOVE 0 TO CK-ORDER-VALUE
           MOVE "CHECK" TO CA-FUNCTION OF WS-CK-CA
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.CustomerCredit SET CreditLimitGBP = 0 "
                  "WHERE CompanyCode = 'STC' AND CustomerCode = "
                  "'ZZCRED1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0170-RUN-SQL
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "a zero limit is not an unlimited limit" TO TK-NAME
           MOVE 1150.00 TO TK-EXPECTED-N
           MOVE CK-EXCESS TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "a zero limit still holds release" TO TK-NAME
           MOVE "HOLD" TO TK-EXPECTED-X
           MOVE CK-DECISION TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0200-RESET-ACCOUNT.

      *>   AR-007 - the candidate commitment is evaluated once and in
      *>   the approved credit evaluation currency
       1200-INPUT-GUARDS.
           PERFORM 0250-BUILD-CALL
           MOVE SPACES TO CK-CUSTOMER
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "a credit check needs a customer" TO TK-NAME
           MOVE "CRD0002" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0250-BUILD-CALL
           MOVE "ZZNOSUCHCUST" TO CK-CUSTOMER
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "an unknown account is not found" TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           PERFORM 0250-BUILD-CALL
           MOVE -1 TO CK-ORDER-VALUE
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "a negative commitment is refused" TO TK-NAME
           MOVE "CRD0003" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0250-BUILD-CALL
           MOVE "USD" TO CK-CURRENCY
           MOVE 1270.00 TO CK-ORDER-VALUE
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "a foreign commitment needs a stated rate" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the missing rate rule is quoted" TO TK-NAME
           MOVE "CRD0011" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0250-BUILD-CALL
           MOVE "USD" TO CK-CURRENCY
           MOVE 1270.00 TO CK-ORDER-VALUE
           MOVE 1.27 TO CK-FX-RATE
           MOVE 20261015 TO CK-RATE-DATE
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "1,270 USD at 1.27 is 1,000.00 sterling" TO TK-NAME
           MOVE 1000.00 TO TK-EXPECTED-N
           MOVE CK-ORDER-VALUE-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AR-008 / D-06 - hard refusals are not overridable
       1300-ACCOUNT-STATE.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.Customer SET SanctionsStatus = "
                  "'POTENTIAL' WHERE CompanyCode = 'STC' AND "
                  "CustomerCode = 'ZZCRED1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0170-RUN-SQL
           PERFORM 0250-BUILD-CALL
           MOVE "CHECK" TO CA-FUNCTION OF WS-CK-CA
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "a sanctions match blocks release" TO TK-NAME
           MOVE "BLOCK" TO TK-EXPECTED-X
           MOVE CK-DECISION TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           MOVE "a sanctions block is an error, not a hold"
               TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CK-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the sanctions rule is quoted" TO TK-NAME
           MOVE "CRD0020" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0200-RESET-ACCOUNT
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.Customer SET Status = 'SUSPENDED' "
                  "WHERE CompanyCode = 'STC' AND CustomerCode = "
                  "'ZZCRED1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0170-RUN-SQL
           PERFORM 0250-BUILD-CALL
           MOVE "CHECK" TO CA-FUNCTION OF WS-CK-CA
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "a suspended account cannot take new demand"
               TO TK-NAME
           MOVE "CRD0022" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CK-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 0200-RESET-ACCOUNT
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.CustomerCredit SET HoldFlag = 1, "
                  "HoldReason = 'Stale credit approval', "
                  "CreditLimitGBP = 50000 WHERE CompanyCode = 'STC' "
                  "AND CustomerCode = 'ZZCRED1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0170-RUN-SQL
           PERFORM 0250-BUILD-CALL
           MOVE "CHECK" TO CA-FUNCTION OF WS-CK-CA
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "an explicit credit hold holds release" TO TK-NAME
           MOVE "CRD0030" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CK-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           MOVE "a credit hold needs authority, code 06" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CK-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0200-RESET-ACCOUNT
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.CustomerCredit SET ProFormaOnly = 1, "
                  "CreditLimitGBP = 50000 WHERE CompanyCode = 'STC' "
                  "AND CustomerCode = 'ZZCRED1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0170-RUN-SQL
           PERFORM 0250-BUILD-CALL
           MOVE 100.00 TO CK-ORDER-VALUE
           MOVE "CHECK" TO CA-FUNCTION OF WS-CK-CA
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "a new account trades on pro forma until approved"
               TO TK-NAME
           MOVE "CRD0031" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CK-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 0200-RESET-ACCOUNT.

      *>   D-06 - any invoice more than 30 days past due holds new
      *>   release, whatever the headroom
       1400-OVERDUE-HOLDS-RELEASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; UPDATE ar.CustomerCredit SET "
                  "CreditLimitGBP = 50000 WHERE CompanyCode = 'STC' "
                  "AND CustomerCode = 'ZZCRED1'; "
                  "UPDATE ar.OpenItem SET DueDate = '2026-09-01' "
                  "WHERE CompanyCode = 'STC' AND DocumentNumber = "
                  "'ZZCR-INV-1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0170-RUN-SQL
           PERFORM 0250-BUILD-CALL
           MOVE "DESPATCH" TO CK-CHECK-POINT
           MOVE "CHECK" TO CA-FUNCTION OF WS-CK-CA
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "overdue debt holds release at despatch" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the overdue hold rule is quoted" TO TK-NAME
           MOVE "CRD0032" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the overdue debt is 800.00" TO TK-NAME
           MOVE 800.00 TO TK-EXPECTED-N
           MOVE CK-OVERDUE-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the oldest item is 44 days past due" TO TK-NAME
           MOVE 44 TO TK-EXPECTED-N
           MOVE CK-OVERDUE-DAYS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the oldest due date is 01/09/2026" TO TK-NAME
           MOVE 20260901 TO TK-EXPECTED-N
           MOVE CK-OLDEST-DUE-DATE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the trigger is the D-06 default of 30 days"
               TO TK-NAME
           MOVE 30 TO TK-EXPECTED-N
           MOVE CK-OVERDUE-TRIGGER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0200-RESET-ACCOUNT.

      *>   AR-008 / D-06 - an override records scope, amount, expiry
      *>   and independent authority
       1500-OVERRIDE-LIFECYCLE.
           PERFORM 0250-BUILD-CALL
           MOVE "OVERRIDE" TO CA-FUNCTION OF WS-CK-CA
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "an override without a reason is refused" TO TK-NAME
           MOVE "CRD0050" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CK-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 0250-BUILD-CALL
           MOVE "OVERRIDE" TO CA-FUNCTION OF WS-CK-CA
           MOVE "ZZCR-SO-1" TO CK-DOCUMENT-REF
           MOVE "Agreed with the customer finance director"
               TO CK-OVERRIDE-REASON
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "a reasoned override is requested" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the override covers the 50.00 excess" TO TK-NAME
           MOVE 50.00 TO TK-EXPECTED-N
           MOVE CK-OVERRIDE-AMT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "it is scoped to the order" TO TK-NAME
           MOVE "ORDER" TO TK-EXPECTED-X
           MOVE CK-OVERRIDE-SCOPE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the credit manager may grant it" TO TK-NAME
           MOVE "AR.CREDIT.OVERRIDE" TO TK-EXPECTED-X
           MOVE CK-APPROVER-PERM TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "it expires after five working days" TO TK-NAME
           MOVE 20261022 TO TK-EXPECTED-N
           MOVE CK-OVERRIDE-EXPIRY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "it is not usable until approved" TO TK-NAME
           MOVE "PENDING" TO TK-EXPECTED-X
           MOVE CK-OVERRIDE-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 1550-BIG-OVERRIDE
           PERFORM 1570-APPROVE-OVERRIDE.

      *>   D-06 - above GBP 50,000 the financial controller approves
       1550-BIG-OVERRIDE.
           PERFORM 0250-BUILD-CALL
           MOVE "OVERRIDE" TO CA-FUNCTION OF WS-CK-CA
           MOVE "ZZCR-SO-9" TO CK-DOCUMENT-REF
           MOVE 60000.00 TO CK-OVERRIDE-AMT
           MOVE "Large one off contract" TO CK-OVERRIDE-REASON
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "an excess over 50,000 needs the controller"
               TO TK-NAME
           MOVE "FIN.CONTROL.OVERRIDE" TO TK-EXPECTED-X
           MOVE CK-APPROVER-PERM TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   AR-008 - independent authority: never the requester
       1570-APPROVE-OVERRIDE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), MIN(OverrideId)), '' "
                  "FROM ar.CreditOverride WHERE CompanyCode = 'STC' "
                  "AND CustomerCode = 'ZZCRED1' AND DocumentRef = "
                  "'ZZCR-SO-1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0180-READ-ONE
           PERFORM 0250-BUILD-CALL
           COMPUTE CK-OVERRIDE-ID = FUNCTION NUMVAL(WS-PROBE-COL)
           MOVE "APPROVE" TO CA-FUNCTION OF WS-CK-CA
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "the requester cannot approve his own override"
               TO TK-NAME
           MOVE "CRD0054" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CK-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 0250-BUILD-CALL
           COMPUTE CK-OVERRIDE-ID = FUNCTION NUMVAL(WS-PROBE-COL)
           MOVE "CREDMGR" TO CA-USER OF WS-CK-CA
           MOVE "APPROVE" TO CA-FUNCTION OF WS-CK-CA
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "an independent authority approves it" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-CK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the override becomes active" TO TK-NAME
           MOVE "ACTIVE" TO TK-EXPECTED-X
           MOVE CK-OVERRIDE-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0250-BUILD-CALL
           MOVE "ZZCR-SO-1" TO CK-DOCUMENT-REF
           MOVE "RELEASE" TO CA-FUNCTION OF WS-CK-CA
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "the order releases under the override" TO TK-NAME
           MOVE "RELEASE" TO TK-EXPECTED-X
           MOVE CK-DECISION TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           MOVE "the release is a warning, with the override quoted"
               TO TK-NAME
           MOVE "CRD0040" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-CK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE ar.CreditOverride SET Status = 'EXPIRED' "
                  "WHERE CompanyCode = 'STC' AND CustomerCode = "
                  "'ZZCRED1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0170-RUN-SQL
           PERFORM 0250-BUILD-CALL
           MOVE "ZZCR-SO-1" TO CK-DOCUMENT-REF
           MOVE "RELEASE" TO CA-FUNCTION OF WS-CK-CA
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "an expired override does not release new demand"
               TO TK-NAME
           MOVE "HOLD" TO TK-EXPECTED-X
           MOVE CK-DECISION TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   AR-007 / AC-021 - moving the same value from order to
      *>   despatch to invoice neither duplicates nor loses exposure
       1600-STAGE-MOVEMENT.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; DECLARE @l1 bigint, @d bigint; "
                  "SELECT @l1 = SalesOrderLineId FROM "
                  "so.SalesOrderLine WHERE CompanyCode = 'STC' AND "
                  "OrderNumber = 'ZZCR-SO-1' AND LineNum = 1; "
                  "SELECT @d = DespatchId FROM so.Despatch WHERE "
                  "CompanyCode = 'STC' AND DespatchRef = "
                  "'ZZCR-DSP-1'; "
                  "UPDATE so.SalesOrderLine SET DespatchedQty = 10 "
                  "WHERE SalesOrderLineId = @l1; "
                  "INSERT so.DespatchLine (DespatchId, LineNum, "
                  "SalesOrderLineId, OrderNumber, OrderLineNum, "
                  "ItemCode, Description, PickedQty, DespatchedQty, "
                  "StockQty, BilledQty) VALUES (@d,2,@l1,"
                  "'ZZCR-SO-1',1,'ZZCR-ITEM','Unshipped frame kit',"
                  "10,10,10,0)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0170-RUN-SQL
           PERFORM 0250-BUILD-CALL
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "despatching empties the unshipped order leg"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CK-FIRM-ORDERS TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the value arrives in the unbilled leg" TO TK-NAME
           MOVE 500.00 TO TK-EXPECTED-N
           MOVE CK-SHIPPED-UNBILL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "exposure after despatch is still 1,150.00"
               TO TK-NAME
           MOVE 1150.00 TO TK-EXPECTED-N
           MOVE CK-EXPOSURE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; UPDATE so.DespatchLine SET "
                  "BilledQty = 10 WHERE OrderNumber = 'ZZCR-SO-1' "
                  "AND OrderLineNum = 1; "
                  "INSERT ar.OpenItem (CompanyCode, CustomerCode, "
                  "ItemType, DocumentNumber, DocumentDate, DueDate, "
                  "PostingDate, CurrencyCode, FxRate, OriginalAmount,"
                  " OriginalAmountGBP, ResidualAmount, "
                  "ResidualAmountGBP, SettlementStatus, CreatedBy) "
                  "VALUES ('STC','ZZCRED1','INVOICE','ZZCR-INV-2',"
                  "'2026-10-10','2026-11-20','2026-10-10','GBP',1,"
                  "350.00,350.00,350.00,350.00,'OPEN','ZZARTEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0170-RUN-SQL
           PERFORM 0250-BUILD-CALL
           CALL "CREDCHK" USING WS-CK-CA
           MOVE "billing moves the value into open debt" TO TK-NAME
           MOVE 1150.00 TO TK-EXPECTED-N
           MOVE CK-OPEN-DEBT TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-CK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "the unbilled leg falls back to 150.00" TO TK-NAME
           MOVE 150.00 TO TK-EXPECTED-N
           MOVE CK-SHIPPED-UNBILL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "exposure after invoicing is still 1,150.00"
               TO TK-NAME
           MOVE 1150.00 TO TK-EXPECTED-N
           MOVE CK-EXPOSURE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       0900-SKIP-EVERYTHING.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 44
               MOVE "database not reachable - STERP_ODBC / SQL Server"
                   TO TK-DETAIL
               MOVE "TARCRED credit exposure" TO TK-NAME
               PERFORM TK-SKIP
           END-PERFORM.

       COPY TESTKITP.
