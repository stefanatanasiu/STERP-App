      *>================================================================
      *> PROGRAM-ID : TSOACCPT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT UNIT TESTS
      *> PURPOSE    : SOACCEPT - COMPLETENESS, RELEASE AND DEPOSITS.
      *>              AC-039 needs a controlled order that cannot be
      *>              treated as accepted demand until it is complete.
      *>              AC-042 needs the rule that printing or
      *>              reprinting an acknowledgement does NOT release a
      *>              held order and is not an override; only RELEASE
      *>              releases, and only after the CURRENT checks.
      *>              SO-019 needs a deposit that is recorded but not
      *>              counted until the funds have cleared.
      *>              Every database assertion runs inside BEGN..ROLB
      *>              with the fixture prefix ZZSOAC, removed before
      *>              and after the run.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: SO-002, SO-003, SO-011, SO-019   AC: AC-042, AC-039
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSOACCPT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY SOACCEPTCA REPLACING ==DFHCOMMAREA== BY ==WS-AC-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
           05  WS-TRAN-OPEN            PIC X VALUE "N".
           05  WS-HIT                  PIC X VALUE "N".
       01  WS-PROBE.
           05  WS-PROBE-1              PIC X(14).
           05  WS-PROBE-2              PIC X(14).
       01  WS-I                        PIC S9(4) VALUE 0.
       01  WS-WANTED                   PIC X(8) VALUE SPACES.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSOACCPT acceptance and release" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-EVERYTHING
           ELSE
               PERFORM 0150-CLEAN-UP
               PERFORM 0200-OPEN-TRAN
               PERFORM 0160-MAKE-CUSTOMER
               PERFORM 0300-BUILD-INCOMPLETE-ORDER
               PERFORM 1000-INCOMPLETE-IS-NOT-DEMAND
               PERFORM 1100-COMPLETE-HEADER-PASSES
               PERFORM 1200-INELIGIBLE-ITEM-IS-A-BLOCKER
               PERFORM 1300-RELEASE-NEEDS-ACCEPTANCE
               PERFORM 1400-PRINTING-DOES-NOT-RELEASE
               PERFORM 1500-ONLY-CLEARED-FUNDS-COUNT
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
           MOVE 14 TO SQLIO-COL-WIDTH(1)
           MOVE 14 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-DB-UP
           END-IF.

       0150-CLEAN-UP.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; DELETE so.DeliverySchedule WHERE "
                  "OrderNumber LIKE 'ZZSOAC%'; DELETE "
                  "so.SalesOrderLine WHERE OrderNumber LIKE "
                  "'ZZSOAC%'; DELETE so.SalesOrder WHERE OrderNumber "
                  "LIKE 'ZZSOAC%'; DELETE ar.Customer WHERE "
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

      *>   a draft with no destination, no terms, no salesperson, no
      *>   terms and conditions version, no VAT basis and no lines
       0300-BUILD-INCOMPLETE-ORDER.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO so.SalesOrder (CompanyCode, SiteCode, "
                  "OrderNumber, OrderType, CustomerCode, OrderDate, "
                  "CurrencyCode, NetAmount, VatAmount, GrossAmount, "
                  "GrossAmountGBP, CommercialStatus, ReleaseStatus, "
                  "FulfilmentStatus, BillingStatus, CorrelationId, "
                  "CreatedBy) VALUES ('STC','RDG','ZZSOAC1',"
                  "'STANDARD','ZZCUST',CONVERT(date,'20261015',112),"
                  "'GBP',0,0,0,0,'DRAFT','AWAITAPPROVAL',"
                  "'UNALLOCATED','NOTELIGIBLE','ZZSOTEST-AC',"
                  "'SALESREP')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-RUN-STATEMENT.

       0600-CALL-SOACCEPT.
           INITIALIZE WS-AC-CA
           MOVE "STC" TO CA-COMPANY OF WS-AC-CA
           MOVE "RDG" TO CA-SITE OF WS-AC-CA
           MOVE "SALESREP" TO CA-USER OF WS-AC-CA
           MOVE 20261020 TO CA-BUSINESS-DATE OF WS-AC-CA
           MOVE "ZZSOTEST-AC" TO CA-CORRELATION-ID OF WS-AC-CA
           MOVE "VALIDATE" TO CA-FUNCTION OF WS-AC-CA
           MOVE "ZZSOAC1" TO SA-ORDER-NUMBER
           CALL "SOACCEPT" USING WS-AC-CA.

      *>   scan the returned blocker list for one reason code
       0650-LOOK-FOR-REASON.
           MOVE "N" TO WS-HIT
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > SA-BLOCKER-CNT
               IF SA-B-REASON(WS-I) = WS-WANTED AND
                  SA-B-SEVERITY(WS-I) = "BLOCKER"
                   MOVE "Y" TO WS-HIT
               END-IF
           END-PERFORM.

      *>   SO-002 - an incomplete draft is never accepted demand
       1000-INCOMPLETE-IS-NOT-DEMAND.
           PERFORM 0600-CALL-SOACCEPT
           MOVE "an incomplete draft is refused" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-AC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the whole blocker list comes back, not one fault"
               TO TK-NAME
           IF SA-HARD-CNT > 4
               PERFORM TK-PASS
           ELSE
               MOVE SA-HARD-CNT TO TK-ACTUAL-N
               MOVE 5 TO TK-EXPECTED-N
               PERFORM TK-ASSERT-NUM
           END-IF
           MOVE "SOA0002" TO WS-WANTED
           PERFORM 0650-LOOK-FOR-REASON
           MOVE "missing commercial fields are reported as SOA0002"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE WS-HIT TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "SOA0007" TO WS-WANTED
           PERFORM 0650-LOOK-FOR-REASON
           MOVE "an order with no lines has no demand to accept"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE WS-HIT TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   the same order, completed, with one TEXT line
       1100-COMPLETE-HEADER-PASSES.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrder SET InvoiceCustomerCode = "
                  "'ZZCUST', DeliveryAddressRef = 'DEL1', TermsCode "
                  "= 'N30', SalespersonCode = 'SALESREP', "
                  "TermsAndConditionsVersion = 'TC-2026.1', "
                  "RetentionOfTitle = 1, VatTreatmentBasis = "
                  "'UKDOMESTIC', VatLocation = 'GB' WHERE "
                  "CompanyCode = 'STC' AND OrderNumber = 'ZZSOAC1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-RUN-STATEMENT
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO so.SalesOrderLine (SalesOrderId, "
                  "CompanyCode, OrderNumber, LineNum, LineRef, "
                  "Description, LineType, AcceptedQty, UnitPrice, "
                  "PricePerQty, NetAmount, VatAmount, Status) SELECT "
                  "SalesOrderId, 'STC', 'ZZSOAC1', 10, '0010', "
                  "'Delivery note wording', 'TEXT', 0, 0, 1, 0, 0, "
                  "'OPEN' FROM so.SalesOrder WHERE CompanyCode = "
                  "'STC' AND OrderNumber = 'ZZSOAC1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-RUN-STATEMENT
           PERFORM 0600-CALL-SOACCEPT
           MOVE "a complete order has no blockers left" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SA-HARD-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the retention of title position is read back"
               TO TK-NAME
           MOVE "1" TO TK-EXPECTED-X
           MOVE SA-RETENTION-TITLE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   SO-003 - an item that is not sellable is an explicit
      *>   blocker, and so is a line with no price source or VAT code
       1200-INELIGIBLE-ITEM-IS-A-BLOCKER.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT INTO so.SalesOrderLine (SalesOrderId, "
                  "CompanyCode, OrderNumber, LineNum, LineRef, "
                  "ItemCode, Revision, Description, LineType, "
                  "AcceptedQty, UomCode, UnitPrice, PricePerQty, "
                  "NetAmount, VatAmount, Status) SELECT "
                  "SalesOrderId, 'STC', 'ZZSOAC1', 20, '0020', "
                  "'ZZ-NO-SUCH-ITEM', 'A', 'Unknown part', 'STOCK', "
                  "5, 'EA', 10, 1, 50, 0, 'OPEN' FROM so.SalesOrder "
                  "WHERE CompanyCode = 'STC' AND OrderNumber = "
                  "'ZZSOAC1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-RUN-STATEMENT
           PERFORM 0600-CALL-SOACCEPT
           MOVE "an ineligible item blocks acceptance" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-AC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "SOA0003" TO WS-WANTED
           PERFORM 0650-LOOK-FOR-REASON
           MOVE "the item blocker carries reason SOA0003" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE WS-HIT TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "SOA0004" TO WS-WANTED
           PERFORM 0650-LOOK-FOR-REASON
           MOVE "a missing price source or VAT code is SOA0004"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE WS-HIT TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE so.SalesOrderLine WHERE CompanyCode = "
                  "'STC' AND OrderNumber = 'ZZSOAC1' AND LineNum = 20"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-RUN-STATEMENT.

      *>   SO-011 - a draft cannot be released
       1300-RELEASE-NEEDS-ACCEPTANCE.
           PERFORM 0600-CALL-SOACCEPT
           MOVE "RELEASE" TO CA-FUNCTION OF WS-AC-CA
           MOVE "N" TO SA-PRINT-ONLY
           MOVE "ZZSOAC1" TO SA-ORDER-NUMBER
           CALL "SOACCEPT" USING WS-AC-CA
           MOVE "a draft order cannot be released" TO TK-NAME
           MOVE "SOA0014" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-AC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AC-042 - printing or reprinting an acknowledgement does not
      *>   release a held order, reserve stock or constitute an
      *>   override.  The release status must be untouched afterwards.
       1400-PRINTING-DOES-NOT-RELEASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrder SET CommercialStatus = "
                  "'ACCEPTED', ReleaseStatus = 'CREDITHOLD', "
                  "HoldReason = 'Account stopped pending payment' "
                  "WHERE CompanyCode = 'STC' AND OrderNumber = "
                  "'ZZSOAC1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0700-RUN-STATEMENT
           PERFORM 0600-CALL-SOACCEPT
           MOVE "RELEASE" TO CA-FUNCTION OF WS-AC-CA
           MOVE "ZZSOAC1" TO SA-ORDER-NUMBER
           MOVE "Y" TO SA-PRINT-ONLY
           CALL "SOACCEPT" USING WS-AC-CA
           MOVE "AC-042 a print request is a warning, not a release"
               TO TK-NAME
           MOVE "SOA0013" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-AC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the print is reported with 04, never 00" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-AC-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ReleaseStatus, '' FROM so.SalesOrder WHERE "
                  "CompanyCode = 'STC' AND OrderNumber = 'ZZSOAC1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 14 TO SQLIO-COL-WIDTH(1)
           MOVE 14 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           MOVE "AC-042 the order is still on credit hold"
               TO TK-NAME
           MOVE "CREDITHOLD" TO TK-EXPECTED-X
           MOVE WS-PROBE-1 TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   SO-019 - a deposit is a liability, not sales revenue, and
      *>   only cleared funds count towards the requirement
       1500-ONLY-CLEARED-FUNDS-COUNT.
           PERFORM 0600-CALL-SOACCEPT
           MOVE "DEPOSIT" TO CA-FUNCTION OF WS-AC-CA
           MOVE "ZZSOAC1" TO SA-ORDER-NUMBER
           MOVE 500.00 TO SA-DEPOSIT-REQ-GBP
           MOVE 0 TO SA-DEPOSIT-AMOUNT
           MOVE "N" TO SA-DEPOSIT-CLEARED
           CALL "SOACCEPT" USING WS-AC-CA
           MOVE "an uncleared deposit is reported for review"
               TO TK-NAME
           MOVE "SOA0012" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-AC-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "nothing is counted as received until it clears"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SA-DEPOSIT-RCVD-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the whole requested deposit is outstanding"
               TO TK-NAME
           MOVE 500.00 TO TK-EXPECTED-N
           MOVE SA-DEPOSIT-OUTSTAND TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       0900-SKIP-EVERYTHING.
           MOVE "AC-039 an incomplete draft is not accepted demand"
               TO TK-NAME
           MOVE "no database connection for company STC"
               TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "AC-042 printing does not release a held order"
               TO TK-NAME
           MOVE "no database connection for company STC"
               TO TK-DETAIL
           PERFORM TK-SKIP.

       COPY TESTKITP.
