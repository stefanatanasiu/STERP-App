      *>================================================================
      *> PROGRAM-ID : TSOSCHED
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT UNIT TESTS
      *> PURPOSE    : SOSCHED - DELIVERY SCHEDULES.  ACCEPTANCE
      *>              SCENARIO AC-043 SPLITS TEN ORDERED UNITS INTO
      *>              SIX AND FOUR AND REQUIRES THAT
      *>                * the schedule total remains ten, and
      *>                * the requested and ORIGINAL promise dates
      *>                  survive every re-promise, so that late
      *>                  delivery cannot be improved by editing the
      *>                  current promise.
      *>              The refusals (a split that does not add up and a
      *>              destination that is not a site of the company)
      *>              are tested too, because both happen before any
      *>              database write.
      *>              Fixtures carry the prefix ZZSOSC and are removed
      *>              before and after the run.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: SO-008   AC: AC-043
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSOSCHED.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY SOSCHEDCA REPLACING ==DFHCOMMAREA== BY ==WS-SC-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
           05  WS-TRAN-OPEN            PIC X VALUE "N".
       01  WS-PROBE.
           05  WS-PROBE-1              PIC X(12).
           05  WS-PROBE-2              PIC X(12).
       01  WS-I                        PIC S9(4) VALUE 0.
       01  WS-SAVED.
           05  WS-SAVE-REQ             PIC 9(8) VALUE 0.
           05  WS-SAVE-ORIG            PIC 9(8) VALUE 0.
           05  WS-SAVE-CURR            PIC 9(8) VALUE 0.
           05  WS-SAVE-CNT             PIC S9(9) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSOSCHED delivery schedules" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 0150-CLEAN-UP
               PERFORM 0200-OPEN-TRAN
               PERFORM 0160-MAKE-CUSTOMER
               PERFORM 0300-BUILD-FIXTURE
               PERFORM 1000-SPLIT-TEN-INTO-SIX-AND-FOUR
               PERFORM 1100-TOTAL-STAYS-TEN
               PERFORM 1200-REPROMISE-KEEPS-EVIDENCE
               PERFORM 1300-SPLIT-MUST-ADD-UP
               PERFORM 1400-DESTINATION-MUST-BE-A-SITE
               PERFORM 0800-ROLL-BACK
               PERFORM 0150-CLEAN-UP
           END-IF
           PERFORM TK-END
           STOP RUN.

      *>   AC-043 needs a real site, because SOSCHED refuses a
      *>   destination that is not one of the company's
       0100-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) SiteCode, '' FROM core.Site WHERE "
                  "CompanyCode = 'STC' AND SiteCode = 'RDG'"
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
                  "OrderNumber LIKE 'ZZSOSC%'; DELETE "
                  "so.SalesOrderLine WHERE OrderNumber LIKE "
                  "'ZZSOSC%'; DELETE so.SalesOrder WHERE OrderNumber "
                  "LIKE 'ZZSOSC%'; DELETE ar.Customer WHERE "
                  "CustomerCode LIKE 'ZZCUST%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-RUN-STATEMENT.

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
           PERFORM 0700-RUN-STATEMENT.

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

      *>   ten units on one line, one schedule, promised 1 Nov 2026
       0300-BUILD-FIXTURE.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO so.SalesOrder (CompanyCode, SiteCode, "
                  "OrderNumber, OrderType, CustomerCode, OrderDate, "
                  "CurrencyCode, TermsCode, SalespersonCode, "
                  "NetAmount, VatAmount, GrossAmount, GrossAmountGBP,"
                  " CommercialStatus, ReleaseStatus, "
                  "FulfilmentStatus, BillingStatus, CorrelationId, "
                  "CreatedBy) VALUES ('STC','RDG','ZZSOSC1',"
                  "'STANDARD','ZZCUST',CONVERT(date,'20261015',112),"
                  "'GBP','N30','SALESREP',250,50,300,300,'ACCEPTED',"
                  "'RELEASED','UNALLOCATED','NOTELIGIBLE',"
                  "'ZZSOTEST-SC','AUDITOR')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO so.SalesOrderLine (SalesOrderId, "
                  "CompanyCode, OrderNumber, LineNum, LineRef, "
                  "ItemCode, Description, LineType, AcceptedQty, "
                  "CancelledQty, DespatchedQty, BilledQty, UomCode, "
                  "UnitPrice, PricePerQty, PriceSource, NetAmount, "
                  "VatCode, VatRatePct, VatAmount, Status) SELECT "
                  "SalesOrderId, 'STC', 'ZZSOSC1', 10, '0010', "
                  "'ZZITEM', 'Scheduled widget', 'STOCK', 10, 0, 0, "
                  "0, 'EA', 25, 1, 'STANDARD', 250, 'S', 20, 50, "
                  "'OPEN' FROM so.SalesOrder WHERE CompanyCode = "
                  "'STC' AND OrderNumber = 'ZZSOSC1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO so.DeliverySchedule (SalesOrderLineId,"
                  " CompanyCode, OrderNumber, LineNum, ScheduleNo, "
                  "Quantity, DespatchedQty, SiteCode, RequestedDate, "
                  "OriginalPromiseDate, CurrentPromiseDate, "
                  "ChangeCount, Status) SELECT SalesOrderLineId, "
                  "'STC', 'ZZSOSC1', 10, 1, 10, 0, 'RDG', "
                  "CONVERT(date,'20261101',112), CONVERT(date,"
                  "'20261101',112), CONVERT(date,'20261101',112), 0, "
                  "'OPEN' FROM so.SalesOrderLine WHERE CompanyCode = "
                  "'STC' AND OrderNumber = 'ZZSOSC1' AND LineNum = 10"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-RUN-STATEMENT.

       0600-PREPARE-CA.
           INITIALIZE WS-SC-CA
           MOVE "STC" TO CA-COMPANY OF WS-SC-CA
           MOVE "RDG" TO CA-SITE OF WS-SC-CA
           MOVE "SALESREP" TO CA-USER OF WS-SC-CA
           MOVE 20261020 TO CA-BUSINESS-DATE OF WS-SC-CA
           MOVE "ZZSOTEST-SC" TO CA-CORRELATION-ID OF WS-SC-CA
           MOVE "ZZSOSC1" TO SS-ORDER-NUMBER
           MOVE 10 TO SS-LINE-NUM.

      *>   AC-043 - ten becomes six on 1 Nov and four on 15 Nov
       1000-SPLIT-TEN-INTO-SIX-AND-FOUR.
           PERFORM 0600-PREPARE-CA
           MOVE "SPLIT" TO CA-FUNCTION OF WS-SC-CA
           MOVE 2 TO SS-SPLIT-CNT
           MOVE 0 TO SS-S-SCHED-NO(1)
           MOVE 6 TO SS-S-QTY(1)
           MOVE "RDG" TO SS-S-SITE(1)
           MOVE 20261101 TO SS-S-REQUESTED(1)
           MOVE 20261101 TO SS-S-PROMISE(1)
           MOVE 0 TO SS-S-SCHED-NO(2)
           MOVE 4 TO SS-S-QTY(2)
           MOVE "RDG" TO SS-S-SITE(2)
           MOVE 20261115 TO SS-S-REQUESTED(2)
           MOVE 20261115 TO SS-S-PROMISE(2)
           CALL "SOSCHED" USING WS-SC-CA
           MOVE "AC-043 ten split into six and four is accepted"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-SC-CA < 8
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SC-CA TO TK-DETAIL
               PERFORM TK-FAIL
               MOVE "N" TO WS-DB-UP
           END-IF.

      *>   the invariant that makes a schedule trustworthy
       1100-TOTAL-STAYS-TEN.
           PERFORM 0600-PREPARE-CA
           MOVE "LIST" TO CA-FUNCTION OF WS-SC-CA
           CALL "SOSCHED" USING WS-SC-CA
           MOVE "AC-043 the schedule total is still ten" TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE SS-SCHEDULE-TOTAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the active order quantity is ten" TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE SS-ACTIVE-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "there is no variance between the two" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SS-VARIANCE-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE 0 TO WS-SAVE-REQ WS-SAVE-ORIG WS-SAVE-CURR
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > SS-LIST-CNT
               IF SS-X-QTY(WS-I) = 6 AND
                  SS-X-STATUS(WS-I) NOT = "CANCELLED"
                   MOVE SS-X-SCHED-NO(WS-I) TO SS-SCHEDULE-NO
                   MOVE SS-X-REQUESTED(WS-I) TO WS-SAVE-REQ
                   MOVE SS-X-ORIG-PROM(WS-I) TO WS-SAVE-ORIG
                   MOVE SS-X-CURR-PROM(WS-I) TO WS-SAVE-CURR
               END-IF
           END-PERFORM
           MOVE "the six unit schedule was created" TO TK-NAME
           MOVE 20261101 TO TK-EXPECTED-N
           MOVE WS-SAVE-ORIG TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AC-043 / SO-008 - re-promising moves only the CURRENT
      *>   promise.  The requested date and the original promise are
      *>   the service evidence and must survive untouched.
       1200-REPROMISE-KEEPS-EVIDENCE.
           PERFORM 0600-PREPARE-CA
           MOVE "REPROMISE" TO CA-FUNCTION OF WS-SC-CA
           MOVE 20261129 TO SS-NEW-PROMISE
           MOVE "Supplier shortage on the moulding" TO
                SS-CHANGE-REASON
           PERFORM 1250-SET-TARGET-SCHEDULE
           CALL "SOSCHED" USING WS-SC-CA
           MOVE "a re-promise with a reason is accepted" TO TK-NAME
           IF CA-RETURN-CODE OF WS-SC-CA < 8
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-SC-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           PERFORM 0600-PREPARE-CA
           MOVE "LIST" TO CA-FUNCTION OF WS-SC-CA
           CALL "SOSCHED" USING WS-SC-CA
           MOVE 0 TO WS-SAVE-REQ WS-SAVE-ORIG WS-SAVE-CURR
           MOVE 0 TO WS-SAVE-CNT
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > SS-LIST-CNT
               IF SS-X-CURR-PROM(WS-I) = 20261129
                   MOVE SS-X-REQUESTED(WS-I) TO WS-SAVE-REQ
                   MOVE SS-X-ORIG-PROM(WS-I) TO WS-SAVE-ORIG
                   MOVE SS-X-CURR-PROM(WS-I) TO WS-SAVE-CURR
                   MOVE SS-X-CHANGE-CNT(WS-I) TO WS-SAVE-CNT
               END-IF
           END-PERFORM
           MOVE "AC-043 the requested date is unchanged" TO TK-NAME
           MOVE 20261101 TO TK-EXPECTED-N
           MOVE WS-SAVE-REQ TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-043 the original promise is unchanged" TO TK-NAME
           MOVE 20261101 TO TK-EXPECTED-N
           MOVE WS-SAVE-ORIG TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the current promise has moved" TO TK-NAME
           MOVE 20261129 TO TK-EXPECTED-N
           MOVE WS-SAVE-CURR TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the change is counted" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE WS-SAVE-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the schedule total is still ten afterwards"
               TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE SS-SCHEDULE-TOTAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       1250-SET-TARGET-SCHEDULE.
           MOVE "LIST" TO CA-FUNCTION OF WS-SC-CA
           CALL "SOSCHED" USING WS-SC-CA
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > SS-LIST-CNT
               IF SS-X-QTY(WS-I) = 6 AND
                  SS-X-STATUS(WS-I) NOT = "CANCELLED"
                   MOVE SS-X-SCHED-NO(WS-I) TO SS-SCHEDULE-NO
               END-IF
           END-PERFORM
           MOVE "REPROMISE" TO CA-FUNCTION OF WS-SC-CA
           MOVE 20261129 TO SS-NEW-PROMISE
           MOVE "Supplier shortage on the moulding" TO
                SS-CHANGE-REASON.

      *>   a split that does not add up never reaches the database
       1300-SPLIT-MUST-ADD-UP.
           PERFORM 0600-PREPARE-CA
           MOVE "SPLIT" TO CA-FUNCTION OF WS-SC-CA
           MOVE 2 TO SS-SPLIT-CNT
           MOVE 0 TO SS-S-SCHED-NO(1)
           MOVE 6 TO SS-S-QTY(1)
           MOVE "RDG" TO SS-S-SITE(1)
           MOVE 20261101 TO SS-S-REQUESTED(1)
           MOVE 20261101 TO SS-S-PROMISE(1)
           MOVE 0 TO SS-S-SCHED-NO(2)
           MOVE 3 TO SS-S-QTY(2)
           MOVE "RDG" TO SS-S-SITE(2)
           MOVE 20261115 TO SS-S-REQUESTED(2)
           MOVE 20261115 TO SS-S-PROMISE(2)
           CALL "SOSCHED" USING WS-SC-CA
           MOVE "nine against ten is refused with SCH0002"
               TO TK-NAME
           MOVE "SCH0002" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the refusal is an error, not a warning" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-SC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0600-PREPARE-CA
           MOVE "LIST" TO CA-FUNCTION OF WS-SC-CA
           CALL "SOSCHED" USING WS-SC-CA
           MOVE "the refused split left the schedules alone"
               TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE SS-SCHEDULE-TOTAL TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   SO-008 - a destination must be a site of this company
       1400-DESTINATION-MUST-BE-A-SITE.
           PERFORM 0600-PREPARE-CA
           MOVE "SPLIT" TO CA-FUNCTION OF WS-SC-CA
           MOVE 1 TO SS-SPLIT-CNT
           MOVE 0 TO SS-S-SCHED-NO(1)
           MOVE 10 TO SS-S-QTY(1)
           MOVE "NOWHERE" TO SS-S-SITE(1)
           MOVE 20261101 TO SS-S-REQUESTED(1)
           MOVE 20261101 TO SS-S-PROMISE(1)
           CALL "SOSCHED" USING WS-SC-CA
           MOVE "an unknown destination is refused with SCH0006"
               TO TK-NAME
           MOVE "SCH0006" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-SC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       0900-SKIP-EVERYTHING.
           MOVE "AC-043 ten split into six and four stays ten"
               TO TK-NAME
           MOVE "no database connection or no site STC/RDG"
               TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "AC-043 re-promising keeps the original evidence"
               TO TK-NAME
           MOVE "no database connection or no site STC/RDG"
               TO TK-DETAIL
           PERFORM TK-SKIP.

       COPY TESTKITP.
