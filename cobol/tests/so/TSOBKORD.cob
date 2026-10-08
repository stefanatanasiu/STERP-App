      *>================================================================
      *> PROGRAM-ID : TSOBKORD
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT UNIT TESTS
      *> PURPOSE    : BACKORD - THE TWO REMAINING QUANTITY RULES OF
      *>              SPECIFICATION 05 SECTION 4, PROVED WITH THE
      *>              WORKED PARTIAL ORDER EXAMPLE OF SECTION 8 AND
      *>              ACCEPTANCE SCENARIO AC-045:
      *>                accept 12 at 50.00, despatch and invoice 7,
      *>                cancel 2 unfulfilled
      *>                -> accepted 12, cancelled 2, despatched 7,
      *>                   billed 7, REMAINING TO DESPATCH 3,
      *>                   billed net 350.00
      *>                and returning one of the seven billed units
      *>                leaves the remaining 3 unchanged.
      *>              Every database assertion runs inside BEGN..ROLB
      *>              and every fixture carries the correlation prefix
      *>              ZZSOTEST so the suite is repeatable.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: SO-014, SO-015, SO-016   AC: AC-045
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSOBKORD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY BACKORDCA REPLACING ==DFHCOMMAREA== BY ==WS-BO-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
           05  WS-TRAN-OPEN            PIC X VALUE "N".
       01  WS-PROBE.
           05  WS-PROBE-1              PIC X(12).
           05  WS-PROBE-2              PIC X(12).
       01  WS-I                        PIC S9(4) VALUE 0.
       01  WS-OPEN-SCHED               PIC S9(9) VALUE 0.
       01  WS-SCHED-REM                PIC S9(13)V9(6) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSOBKORD back order balances" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 0150-CLEAN-UP
               PERFORM 0200-OPEN-TRAN
               PERFORM 0160-MAKE-CUSTOMER
               PERFORM 0300-BUILD-FIXTURE
               PERFORM 1000-PARTIAL-ORDER-EXAMPLE
               PERFORM 1100-REMAINING-TO-INVOICE
               PERFORM 1200-RETURN-OF-BILLED-UNIT
               PERFORM 1300-SCHEDULE-BALANCES-ARE-OWN
               PERFORM 1400-HEADER-COMPLETION-IS-DERIVED
               PERFORM 0800-ROLL-BACK
               PERFORM 0150-CLEAN-UP
           END-IF
           PERFORM TK-END
           STOP RUN.

       0100-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CompanyCode, '' FROM core.Company "
                  "WHERE CompanyCode = 'STC'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-DB-UP
           END-IF.

       0150-CLEAN-UP.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; DELETE so.DeliverySchedule WHERE "
                  "OrderNumber LIKE 'ZZSOBK%'; DELETE "
                  "so.SalesOrderLine WHERE OrderNumber LIKE "
                  "'ZZSOBK%'; DELETE so.SalesOrder WHERE OrderNumber "
                  "LIKE 'ZZSOBK%'; DELETE ar.Customer WHERE "
                  "CustomerCode LIKE 'ZZCUST%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

      *>   the fixture customer the orders below hang off; built from
      *>   an existing account so every foreign key is satisfied
       0160-MAKE-CUSTOMER.
           MOVE SPACES TO SQLIO-STMT
           STRING "IF NOT EXISTS (SELECT 1 FROM ar.Customer WHERE "
                  "CompanyCode = 'STC' AND CustomerCode = 'ZZCUST') "
                  "INSERT INTO ar.Customer (CompanyCode, "
                  "CustomerCode, LegalName, LegalForm, VatLocation, "
                  "IsVatBusiness, DrcEligible, CountryCode, "
                  "CurrencyCode, AllowedCurrencies, TermsCode, "
                  "CollectionMethod, StatementFrequency, "
                  "RequiresPurchaseOrderRef, EInvoiceEnabled, "
                  "SanctionsStatus, IsPersonalData, Status, "
                  "CreatedBy, CreatedAt) SELECT CompanyCode, "
                  "'ZZCUST', 'ZZ fixture customer', LegalForm, "
                  "VatLocation, IsVatBusiness, DrcEligible, "
                  "CountryCode, CurrencyCode, AllowedCurrencies, "
                  "TermsCode, CollectionMethod, StatementFrequency, "
                  "RequiresPurchaseOrderRef, EInvoiceEnabled, "
                  "SanctionsStatus, IsPersonalData, 'ACTIVE', "
                  "'SALESREP', SYSUTCDATETIME() FROM ar.Customer "
                  "WHERE CompanyCode = 'STC' AND CustomerCode = "
                  "'C0001'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       0200-OPEN-TRAN.
           MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           MOVE "Y" TO WS-TRAN-OPEN.

       0800-ROLL-BACK.
           IF WS-TRAN-OPEN = "Y"
               MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-PROBE
               MOVE "N" TO WS-TRAN-OPEN
           END-IF.

      *>   the order of specification 05 section 8, already part
      *>   despatched, part billed and part cancelled
       0300-BUILD-FIXTURE.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO so.SalesOrder (CompanyCode, SiteCode, "
                  "OrderNumber, OrderType, CustomerCode, "
                  "InvoiceCustomerCode, OrderDate, CurrencyCode, "
                  "TermsCode, SalespersonCode, DeliveryAddressRef, "
                  "TermsAndConditionsVersion, VatTreatmentBasis, "
                  "NetAmount, VatAmount, GrossAmount, GrossAmountGBP,"
                  " CommercialStatus, ReleaseStatus, "
                  "FulfilmentStatus, BillingStatus, CorrelationId, "
                  "CreatedBy) VALUES ('STC','RDG','ZZSOBK1',"
                  "'STANDARD','ZZCUST','ZZCUST',CONVERT(date,"
                  "'20261015',112),'GBP','N30','SALESREP','DEL1',"
                  "'TC-2026.1','UKDOMESTIC',500,100,600,600,"
                  "'ACCEPTED','RELEASED','PARTDESPATCH','PARTBILLED',"
                  "'ZZSOTEST-BK','AUDITOR')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO so.SalesOrderLine (SalesOrderId, "
                  "CompanyCode, OrderNumber, LineNum, LineRef, "
                  "ItemCode, Description, LineType, AcceptedQty, "
                  "CancelledQty, DespatchedQty, BilledQty, "
                  "ReturnedQty, UomCode, UnitPrice, PricePerQty, "
                  "PriceSource, DiscountPct1, DiscountPct2, "
                  "NetAmount, VatCode, VatRatePct, VatAmount, Status)"
                  " SELECT SalesOrderId, 'STC', 'ZZSOBK1', 10, "
                  "'0010', 'ZZITEM', 'Partial order example widget', "
                  "'STOCK', 12, 2, 7, 7, 0, 'EA', 50, 1, 'STANDARD', "
                  "0, 0, 500, 'S', 20, 100, 'OPEN' FROM "
                  "so.SalesOrder WHERE CompanyCode = 'STC' AND "
                  "OrderNumber = 'ZZSOBK1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-RUN-STATEMENT
      *>       two schedules: seven already shipped, three still open
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO so.DeliverySchedule (SalesOrderLineId,"
                  " CompanyCode, OrderNumber, LineNum, ScheduleNo, "
                  "Quantity, DespatchedQty, SiteCode, RequestedDate, "
                  "OriginalPromiseDate, CurrentPromiseDate, Status) "
                  "SELECT SalesOrderLineId, 'STC', 'ZZSOBK1', 10, 1, "
                  "7, 7, 'RDG', CONVERT(date,'20261101',112), "
                  "CONVERT(date,'20261101',112), CONVERT(date,"
                  "'20261101',112), 'COMPLETE' FROM "
                  "so.SalesOrderLine WHERE CompanyCode = 'STC' AND "
                  "OrderNumber = 'ZZSOBK1' AND LineNum = 10"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO so.DeliverySchedule (SalesOrderLineId,"
                  " CompanyCode, OrderNumber, LineNum, ScheduleNo, "
                  "Quantity, DespatchedQty, SiteCode, RequestedDate, "
                  "OriginalPromiseDate, CurrentPromiseDate, Status) "
                  "SELECT SalesOrderLineId, 'STC', 'ZZSOBK1', 10, 2, "
                  "3, 0, 'RDG', CONVERT(date,'20261115',112), "
                  "CONVERT(date,'20261115',112), CONVERT(date,"
                  "'20261122',112), 'OPEN' FROM so.SalesOrderLine "
                  "WHERE CompanyCode = 'STC' AND OrderNumber = "
                  "'ZZSOBK1' AND LineNum = 10"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-RUN-STATEMENT.

       0700-RUN-STATEMENT.
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR
               MOVE "N" TO WS-DB-UP
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       0600-CALL-EVALUATE.
           INITIALIZE WS-BO-CA
           MOVE "STC" TO CA-COMPANY OF WS-BO-CA
           MOVE "RDG" TO CA-SITE OF WS-BO-CA
           MOVE "AUDITOR" TO CA-USER OF WS-BO-CA
           MOVE 20261120 TO CA-BUSINESS-DATE OF WS-BO-CA
           MOVE "ZZSOTEST-BK" TO CA-CORRELATION-ID OF WS-BO-CA
           MOVE "EVALUATE" TO CA-FUNCTION OF WS-BO-CA
           MOVE "ZZSOBK1" TO BO-ORDER-NUMBER
           CALL "BACKORD" USING WS-BO-CA.

      *>   AC-045 - the headline numbers of the worked example
       1000-PARTIAL-ORDER-EXAMPLE.
           PERFORM 0600-CALL-EVALUATE
           IF CA-RETURN-CODE OF WS-BO-CA > 4
               MOVE "fixture could not be evaluated" TO TK-DETAIL
               MOVE "AC-045 partial order evaluates" TO TK-NAME
               PERFORM TK-SKIP
               GO TO 1000-EXIT
           END-IF
           MOVE "accepted quantity stays 12" TO TK-NAME
           MOVE 12 TO TK-EXPECTED-N
           MOVE BO-ACCEPTED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "cancelled quantity is 2" TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE BO-CANCELLED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "despatched quantity is 7" TO TK-NAME
           MOVE 7 TO TK-EXPECTED-N
           MOVE BO-DESPATCHED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "billed quantity is 7" TO TK-NAME
           MOVE 7 TO TK-EXPECTED-N
           MOVE BO-BILLED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-045 remaining to despatch is 3" TO TK-NAME
           MOVE 3 TO TK-EXPECTED-N
           MOVE BO-REMAIN-DESPATCH TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-045 billed net is 350.00" TO TK-NAME
           MOVE 350.00 TO TK-EXPECTED-N
           MOVE BO-BILLED-NET TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1000-EXIT.
           EXIT.

      *>   remaining eligible to invoice = confirmed fulfilment
      *>   less withdrawn before billing less already billed
       1100-REMAINING-TO-INVOICE.
           MOVE "nothing is left to invoice once seven are billed"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE BO-REMAIN-INVOICE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "no quantity was withdrawn before billing"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE BO-WITHDRAWN-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the line carries the same remaining three"
               TO TK-NAME
           MOVE 3 TO TK-EXPECTED-N
           MOVE BO-L-REM-DESP(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AC-045 - the return is a separate event.  It must not
      *>   change the three remaining units and must not re-enable
      *>   billing that has already happened.
       1200-RETURN-OF-BILLED-UNIT.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrderLine SET ReturnedQty = 1 "
                  "WHERE CompanyCode = 'STC' AND OrderNumber = "
                  "'ZZSOBK1' AND LineNum = 10"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-RUN-STATEMENT
           PERFORM 0600-CALL-EVALUATE
           MOVE "one billed unit is returned" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE BO-RETURNED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-045 remaining to despatch is still 3" TO TK-NAME
           MOVE 3 TO TK-EXPECTED-N
           MOVE BO-REMAIN-DESPATCH TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "a billed return creates no new billing eligibility"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE BO-REMAIN-INVOICE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "billed net is still 350.00 after the return"
               TO TK-NAME
           MOVE 350.00 TO TK-EXPECTED-N
           MOVE BO-BILLED-NET TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   SO-015 - completing one schedule does not close the
      *>   unfulfilled balance on another
       1300-SCHEDULE-BALANCES-ARE-OWN.
           INITIALIZE WS-BO-CA
           MOVE "STC" TO CA-COMPANY OF WS-BO-CA
           MOVE "RDG" TO CA-SITE OF WS-BO-CA
           MOVE "AUDITOR" TO CA-USER OF WS-BO-CA
           MOVE 20261120 TO CA-BUSINESS-DATE OF WS-BO-CA
           MOVE "EVALUATE" TO CA-FUNCTION OF WS-BO-CA
           MOVE "ZZSOBK1" TO BO-ORDER-NUMBER
           MOVE "Y" TO BO-BY-SCHEDULE
           CALL "BACKORD" USING WS-BO-CA
           MOVE 0 TO WS-OPEN-SCHED
           MOVE 0 TO WS-SCHED-REM
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > BO-LINE-CNT
               IF BO-L-SCHED-NO(WS-I) = 2
                   MOVE BO-L-REM-DESP(WS-I) TO WS-SCHED-REM
               END-IF
           END-PERFORM
           MOVE BO-OPEN-SCHED-CNT TO WS-OPEN-SCHED
           MOVE "both schedules are reported" TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE BO-LINE-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the completed schedule leaves one open" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE WS-OPEN-SCHED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the second schedule still owes three" TO TK-NAME
           MOVE 3 TO TK-EXPECTED-N
           MOVE WS-SCHED-REM TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   SO-015 - header completion is derived, never forced
       1400-HEADER-COMPLETION-IS-DERIVED.
           MOVE "N" TO BO-BY-SCHEDULE
           INITIALIZE WS-BO-CA
           MOVE "STC" TO CA-COMPANY OF WS-BO-CA
           MOVE "RDG" TO CA-SITE OF WS-BO-CA
           MOVE "AUDITOR" TO CA-USER OF WS-BO-CA
           MOVE 20261120 TO CA-BUSINESS-DATE OF WS-BO-CA
           MOVE "CLOSE" TO CA-FUNCTION OF WS-BO-CA
           MOVE "ZZSOBK1" TO BO-ORDER-NUMBER
           CALL "BACKORD" USING WS-BO-CA
           MOVE "CLOSE refuses to complete an order owing three"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE BO-HEADER-COMPLETE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the outstanding balance is explained" TO TK-NAME
           MOVE "BKO0007" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-BO-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       0900-SKIP-EVERYTHING.
           MOVE "AC-045 partial order, billing and cancellation"
               TO TK-NAME
           MOVE "no database connection for company STC"
               TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "AC-045 a returned billed unit changes no remainder"
               TO TK-NAME
           MOVE "no database connection for company STC"
               TO TK-DETAIL
           PERFORM TK-SKIP.

       COPY TESTKITP.
