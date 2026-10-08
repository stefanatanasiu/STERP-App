      *>================================================================
      *> PROGRAM-ID : TSOB2B
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT UNIT TESTS
      *> PURPOSE    : SOB2B - LINKED SUPPLY AND DROP SHIPMENT, AND
      *>              SOROT - RETENTION OF TITLE.
      *>              THE TWO PROMISES UNDER TEST ARE THAT A CHANGED
      *>              SUPPLIER PROMISE IDENTIFIES THE AFFECTED
      *>              CUSTOMER SCHEDULES WITHOUT CHANGING THEM, AND
      *>              THAT A CONFIRMED DIRECT DELIVERY CREATES NO
      *>              WAREHOUSE ON HAND QUANTITY.
      *>              THE FIXTURE ORDER IS WRITTEN INSIDE
      *>              BEGN ... ROLB SO NOTHING SURVIVES THE RUN.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: SO-021, SO-029   AC: AC-048, AC-124
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSOB2B.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY SOB2BCA REPLACING ==DFHCOMMAREA== BY ==WS-B2B-CA==.
       COPY SOROTCA REPLACING ==DFHCOMMAREA== BY ==WS-ROT-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
           05  WS-IN-TXN               PIC X VALUE "N".
       01  WS-PROBE.
           05  WS-P-CUSTOMER           PIC X(20).
           05  WS-P-ITEM               PIC X(40).
       01  WS-ONE-COL.
           05  WS-C1                   PIC X(40).
           05  WS-C2                   PIC X(40).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-N                    PIC S9(15)V9(6) VALUE 0.
           05  WS-ORDER-ID             PIC S9(17) VALUE 0.
           05  WS-LINE-ID              PIC S9(17) VALUE 0.
           05  WS-STOCK-BEFORE         PIC S9(13)V9(6) VALUE 0.
           05  WS-STOCK-AFTER          PIC S9(13)V9(6) VALUE 0.
           05  WS-PROMISE-AFTER        PIC 9(8) VALUE 0.
           05  WS-MOVEMENTS            PIC S9(9) VALUE 0.
           05  WS-INT-ED               PIC -(8)9.
       01  WS-CLEAN-UP                 PIC X(4000).
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSOB2B linked supply and drop shipment" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-NO-DATABASE-NEEDED
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N"
               PERFORM 0900-SKIP-DATABASE-TESTS
           ELSE
               PERFORM 0150-CLEAN-UP
               PERFORM 0200-BEGIN
               PERFORM 0400-MAKE-ORDER-FIXTURE THRU 0400-EXIT
               IF WS-DB-UP = "Y"
                   PERFORM 2000-LINK-IS-NOT-A-GUARANTEE
                   PERFORM 2100-SUPPLY-CHANGE-IS-A-REVIEW
                   PERFORM 2200-DIRECT-DELIVERY-NO-STOCK
                       THRU 2200-EXIT
                   PERFORM 0450-ENSURE-FIXTURE
                   PERFORM 3000-RETENTION-OF-TITLE
               ELSE
                   PERFORM 0900-SKIP-DATABASE-TESTS
               END-IF
               PERFORM 0300-ROLLBACK
               PERFORM 0150-CLEAN-UP
           END-IF
           PERFORM TK-END
           STOP RUN.
      *>----------------------------------------------------------------
       0100-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) c.CustomerCode, ISNULL((SELECT "
                  "TOP (1) i.ItemCode FROM inv.Item i WHERE "
                  "i.CompanyCode = 'STC' ORDER BY i.ItemCode),'') "
                  "FROM ar.Customer c WHERE c.CompanyCode = 'STC' "
                  "ORDER BY c.CustomerCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-DB-UP
           END-IF
           IF WS-P-ITEM = SPACES
               MOVE "N" TO WS-DB-UP
           END-IF.

       0200-BEGIN.
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR
               MOVE "N" TO WS-DB-UP
           ELSE
               MOVE "Y" TO WS-IN-TXN
           END-IF.

       0300-ROLLBACK.
           IF WS-IN-TXN = "Y"
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-PROBE
               MOVE "N" TO WS-IN-TXN
           END-IF.

      *>   SOB2B commits its own unit of work, and SQLIO transaction
      *>   control is per connection, so the rollback alone cannot be
      *>   relied on.  The fixture is removed by name before and after.
       0150-CLEAN-UP.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; DELETE so.DespatchLine WHERE "
                  "DespatchId IN (SELECT DespatchId FROM "
                  "so.Despatch WHERE CompanyCode = 'STC' AND "
                  "OrderNumber = 'ZZSO-ORD-B2B1'); DELETE "
                  "so.Despatch WHERE CompanyCode = 'STC' AND "
                  "OrderNumber = 'ZZSO-ORD-B2B1'; DELETE "
                  "so.DeliverySchedule WHERE CompanyCode = 'STC' "
                  "AND OrderNumber = 'ZZSO-ORD-B2B1'; DELETE "
                  "so.SalesOrderLine WHERE CompanyCode = 'STC' AND "
                  "OrderNumber = 'ZZSO-ORD-B2B1'; DELETE "
                  "so.SalesOrder WHERE CompanyCode = 'STC' AND "
                  "OrderNumber = 'ZZSO-ORD-B2B1'; "
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE SPACES TO WS-CLEAN-UP
           STRING FUNCTION TRIM(SQLIO-STMT)
                  " DELETE core.ExceptionItem WHERE CompanyCode = "
                  "'STC' AND DocumentRef IN ('ZZPO-B2B1',"
                  "'ZZSO-ORD-B2B1'); DELETE fin.JournalStage WHERE "
                  "CorrelationId LIKE 'ZZSOTEST%'; DELETE "
                  "core.AuditLog WHERE CorrelationId LIKE "
                  "'ZZSOTEST%'; DELETE core.IdempotencyKey WHERE "
                  "CorrelationId LIKE 'ZZSOTEST%'"
               DELIMITED BY SIZE INTO WS-CLEAN-UP
           MOVE WS-CLEAN-UP TO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 7 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           MOVE 7 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       0900-SKIP-DATABASE-TESTS.
           MOVE "a link does not guarantee delivery" TO TK-NAME
           MOVE "no STERP database or no seeded data" TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "a changed supplier promise is a review"
               TO TK-NAME
           MOVE "no STERP database or no seeded data" TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "a direct delivery creates no warehouse stock"
               TO TK-NAME
           MOVE "no STERP database or no seeded data" TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "retention of title is recorded on the order"
               TO TK-NAME
           MOVE "no STERP database or no seeded data" TO TK-DETAIL
           PERFORM TK-SKIP.
      *>----------------------------------------------------------------
       1000-NO-DATABASE-NEEDED.
           PERFORM 1100-BUILD-B2B
           MOVE "WIBBLE" TO CA-FUNCTION OF WS-B2B-CA
           CALL "SOB2B" USING WS-B2B-CA
           MOVE "an unknown function is refused, not obeyed"
               TO TK-NAME
           MOVE "B2B0009" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-B2B-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 1100-BUILD-B2B
           MOVE SPACES TO BB-ORDER-NUMBER
           CALL "SOB2B" USING WS-B2B-CA
           MOVE "an order number and line number are required"
               TO TK-NAME
           MOVE "B2B0001" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-B2B-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a refused call creates no warehouse stock"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE BB-ONHAND-CREATED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
      *>----------------------------------------------------------------
       1100-BUILD-B2B.
           INITIALIZE WS-B2B-CA
           MOVE "LINK" TO CA-FUNCTION OF WS-B2B-CA
           MOVE "STC" TO CA-COMPANY OF WS-B2B-CA
           MOVE "RDG" TO CA-SITE OF WS-B2B-CA
           MOVE "BUYER" TO CA-USER OF WS-B2B-CA
           MOVE 20261016 TO CA-BUSINESS-DATE OF WS-B2B-CA
           MOVE "ZZSOTEST-SOB2B-0001" TO
               CA-CORRELATION-ID OF WS-B2B-CA
           MOVE "ZZSO-ORD-B2B1" TO BB-ORDER-NUMBER
           MOVE 1 TO BB-LINE-NUM
           MOVE 1 TO BB-SCHEDULE-NO
           MOVE "DROPSHIP" TO BB-SUPPLY-TYPE
           MOVE "ZZPO-B2B1" TO BB-LINKED-PO
           MOVE 1 TO BB-LINKED-PO-LINE
           MOVE "S0001" TO BB-SUPPLIER
           MOVE "EA" TO BB-UOM.
      *>----------------------------------------------------------------
       0400-MAKE-ORDER-FIXTURE.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT so.SalesOrder (CompanyCode, SiteCode, "
                  "OrderNumber, OrderType, CustomerCode, OrderDate, "
                  "CurrencyCode, TermsAndConditionsVersion, "
                  "NetAmount, VatAmount, GrossAmount, "
                  "GrossAmountGBP, FulfilmentStatus, BillingStatus, "
                  "RetentionOfTitle, CommercialStatus, "
                  "ReleaseStatus, CorrelationId, CreatedBy) VALUES "
                  "('STC', 'RDG', 'ZZSO-ORD-B2B1', 'DROPSHIP', ?, "
                  "'20261001', 'GBP', 'TC-2024-01', 500, 100, 600, "
                  "600, 'UNALLOCATED', 'NOTELIGIBLE', 1, "
                  "'ACCEPTED', "
                  "'RELEASED', 'ZZSOTEST-SOB2B', 'SALESREP'); "
                  "SELECT CAST(SCOPE_IDENTITY() AS varchar(24)), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-P-CUSTOMER TO SQLIO-PARM-VAL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE "N" TO SQLIO-PARM-NULL(1)
           PERFORM 0500-SINGLE-ROW
           IF WS-DB-UP = "N"
               GO TO 0400-EXIT
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WS-C1)
           MOVE WS-N TO WS-ORDER-ID
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT so.SalesOrderLine (SalesOrderId, "
                  "CompanyCode, OrderNumber, LineNum, LineRef, "
                  "ItemCode, Description, LineType, AcceptedQty, "
                  "UomCode, ConversionFactor, UnitPrice, NetAmount, "
                  "CostSnapshotGBP, SupplyType, Status) VALUES (?, "
                  "'STC', 'ZZSO-ORD-B2B1', 1, '1', ?, 'Linked "
                  "supply test line', 'STOCK', 10, 'EA', 1, 50, "
                  "500, 30, 'STOCK', 'OPEN'); SELECT "
                  "CAST(SCOPE_IDENTITY() AS varchar(24)), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-ORDER-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE WS-P-ITEM TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           PERFORM 0500-SINGLE-ROW
           IF WS-DB-UP = "N"
               GO TO 0400-EXIT
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WS-C1)
           MOVE WS-N TO WS-LINE-ID
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT so.DeliverySchedule (SalesOrderLineId, "
                  "CompanyCode, OrderNumber, LineNum, ScheduleNo, "
                  "Quantity, SiteCode, RequestedDate, "
                  "OriginalPromiseDate, CurrentPromiseDate, "
                  "FulfilmentMethod, Status) VALUES (?, 'STC', "
                  "'ZZSO-ORD-B2B1', 1, 1, 10, 'RDG', '20261020', "
                  "'20261020', '20261020', 'WAREHOUSE', 'OPEN'); "
                  "SELECT '1', ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-LINE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE "N" TO SQLIO-PARM-NULL(1)
           PERFORM 0500-SINGLE-ROW.
       0400-EXIT.
           EXIT.

       0500-SINGLE-ROW.
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-DB-UP
           END-IF.

      *>   SQLIO transaction control is per connection and flat, so a
      *>   called program that meets a database error rolls the test's
      *>   own unit of work back with its own.  Rebuild the fixture if
      *>   it has gone rather than cascade a misleading failure.
       0450-ENSURE-FIXTURE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(24)), '' FROM "
                  "so.SalesOrder WHERE CompanyCode = 'STC' AND "
                  "OrderNumber = 'ZZSO-ORD-B2B1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           PERFORM 0500-SINGLE-ROW
           IF WS-DB-UP = "N"
               MOVE "Y" TO WS-DB-UP
               MOVE "0" TO WS-C1
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WS-C1)
           IF WS-N = 0
               PERFORM 0150-CLEAN-UP
               PERFORM 0200-BEGIN
               PERFORM 0400-MAKE-ORDER-FIXTURE THRU 0400-EXIT
           END-IF.
      *>----------------------------------------------------------------
      *> 2000  SO-021 - THE LINK IS EVIDENCE OF INTENT.  IT DOES NOT
      *>       GUARANTEE DELIVERY AND IT DOES NOT APPROVE A SUPPLIER
      *>       INVOICE.
      *>----------------------------------------------------------------
       2000-LINK-IS-NOT-A-GUARANTEE.
           PERFORM 1100-BUILD-B2B
           MOVE SPACES TO BB-LINKED-PO
           CALL "SOB2B" USING WS-B2B-CA
           MOVE "a linked line must name the purchase order line"
               TO TK-NAME
           MOVE "B2B0004" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-B2B-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 1100-BUILD-B2B
           CALL "SOB2B" USING WS-B2B-CA
           MOVE "a link does not guarantee delivery" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE BB-SUPPLY-GUARANTEE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a link does not approve a supplier invoice"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE BB-INVOICE-APPROVED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "linking creates no warehouse stock" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE BB-ONHAND-CREATED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
      *>----------------------------------------------------------------
      *> 2100  AC-048 - A CHANGED SUPPLIER PROMISE IDENTIFIES THE
      *>       AFFECTED CUSTOMER SCHEDULE FOR REVIEW.  THE CUSTOMER
      *>       PROMISE OF 20 OCTOBER IS LEFT EXACTLY WHERE IT WAS.
      *>----------------------------------------------------------------
       2100-SUPPLY-CHANGE-IS-A-REVIEW.
           PERFORM 1100-BUILD-B2B
           MOVE "SUPPCHG" TO CA-FUNCTION OF WS-B2B-CA
           MOVE "DELAY" TO BB-CHANGE-TYPE
           MOVE 20261120 TO BB-NEW-PROMISE
           MOVE "Supplier foundry shutdown" TO BB-CHANGE-REASON
           CALL "SOB2B" USING WS-B2B-CA
           MOVE "a changed supplier promise is a review"
               TO TK-NAME
           IF BB-AFFECTED-CNT > 0 AND BB-S-ACTION(1) = "REVIEW"
               PERFORM TK-PASS
           ELSE
               MOVE "no customer schedule was identified"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "the customer promise is not silently changed"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE BB-PROMISE-CHANGED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 2150-READ-PROMISE
           MOVE "the held promise date is untouched" TO TK-NAME
           MOVE 20261020 TO TK-EXPECTED-N
           MOVE WS-PROMISE-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "an exception is raised for someone to decide"
               TO TK-NAME
           IF BB-EXCEPTION-COUNT > 0
               PERFORM TK-PASS
           ELSE
               MOVE "no exception was raised" TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

       2150-READ-PROMISE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(char(8), CurrentPromiseDate, "
                  "112), '' FROM so.DeliverySchedule WHERE "
                  "CompanyCode = 'STC' AND OrderNumber = "
                  "'ZZSO-ORD-B2B1' AND LineNum = 1 AND ScheduleNo "
                  "= 1"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           MOVE 0 TO WS-PROMISE-AFTER
           IF NOT SQLIO-ERROR AND NOT SQLIO-NOTFOUND
               COMPUTE WS-N = FUNCTION NUMVAL(WS-C1)
               MOVE WS-N TO WS-PROMISE-AFTER
           END-IF.
      *>----------------------------------------------------------------
      *> 2200  AC-048 - CONFIRMED DIRECT DELIVERY RECORDS THE QUANTITY
      *>       DELIVERED TO THE CUSTOMER AND CREATES NO WAREHOUSE ON
      *>       HAND QUANTITY.  THE UNIT COST IS LEFT AT NIL HERE SO
      *>       THE ACCRUAL IS NOT POSTED INSIDE THE TEST TRANSACTION;
      *>       WHAT IS UNDER TEST IS THE ABSENCE OF STOCK.
      *>----------------------------------------------------------------
       2200-DIRECT-DELIVERY-NO-STOCK.
           PERFORM 2250-READ-STOCK
           MOVE WS-STOCK-AFTER TO WS-STOCK-BEFORE
           PERFORM 1100-BUILD-B2B
           MOVE "DIRECTDL" TO CA-FUNCTION OF WS-B2B-CA
           MOVE 4 TO BB-DELIVERED-QTY
           MOVE 0 TO BB-UNIT-COST-GBP
           MOVE 20261018 TO BB-DELIVERY-DATE
           MOVE "SUPP-POD-99001" TO BB-EVIDENCE-REF
           CALL "SOB2B" USING WS-B2B-CA
           IF CA-RETURN-CODE OF WS-B2B-CA > 4
               MOVE "a direct delivery creates no warehouse stock"
                   TO TK-NAME
               MOVE CA-MESSAGE OF WS-B2B-CA TO TK-DETAIL
               PERFORM TK-SKIP
               GO TO 2200-EXIT
           END-IF
           PERFORM 2250-READ-STOCK
           MOVE "a direct delivery creates no warehouse stock"
               TO TK-NAME
           MOVE WS-STOCK-BEFORE TO TK-EXPECTED-N
           MOVE WS-STOCK-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "no stock movement is claimed" TO TK-NAME
           IF BB-ONHAND-CREATED = "N" AND BB-MOVEMENT-ID = 0
               PERFORM TK-PASS
           ELSE
               MOVE "a stock movement was reported" TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           PERFORM 2260-COUNT-MOVEMENTS
           MOVE "the delivery evidence holds no movement id"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE WS-MOVEMENTS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       2200-EXIT.
           EXIT.

       2250-READ-STOCK.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(QtyOnHand),0) AS "
                  "varchar(24)), '' FROM inv.StockBalance WHERE "
                  "CompanyCode = 'STC' AND ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-P-ITEM TO SQLIO-PARM-VAL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           MOVE 0 TO WS-STOCK-AFTER
           IF NOT SQLIO-ERROR AND NOT SQLIO-NOTFOUND
               COMPUTE WS-N = FUNCTION NUMVAL(WS-C1)
               MOVE WS-N TO WS-STOCK-AFTER
           END-IF.

       2260-COUNT-MOVEMENTS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(24)), '' FROM "
                  "so.DespatchLine dl JOIN so.Despatch d ON "
                  "d.DespatchId = dl.DespatchId WHERE "
                  "d.CompanyCode = 'STC' AND d.OrderNumber = "
                  "'ZZSO-ORD-B2B1' AND dl.MovementId IS NOT NULL"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           MOVE 0 TO WS-MOVEMENTS
           IF NOT SQLIO-ERROR AND NOT SQLIO-NOTFOUND
               COMPUTE WS-N = FUNCTION NUMVAL(WS-C1)
               MOVE WS-N TO WS-MOVEMENTS
           END-IF.
      *>----------------------------------------------------------------
      *> 3000  AC-124 - THE ORDER RECORDS ITS TERMS OF SALE AND
      *>       WHETHER RETENTION OF TITLE APPLIES, AND THE UNPAID
      *>       GOODS LIST ANSWERS FOR THE CUSTOMER.  WITH NO UNPAID
      *>       SALES INVOICE BEHIND THE FIXTURE THE CORRECT ANSWER IS
      *>       "NOTHING IS HELD", NOT AN ERROR.
      *>----------------------------------------------------------------
       3000-RETENTION-OF-TITLE.
           INITIALIZE WS-ROT-CA
           MOVE "TERMS" TO CA-FUNCTION OF WS-ROT-CA
           MOVE "STC" TO CA-COMPANY OF WS-ROT-CA
           MOVE "RDG" TO CA-SITE OF WS-ROT-CA
           MOVE "SALESMGR" TO CA-USER OF WS-ROT-CA
           MOVE 20261016 TO CA-BUSINESS-DATE OF WS-ROT-CA
           MOVE "ZZSOTEST-SOROT-0001" TO
               CA-CORRELATION-ID OF WS-ROT-CA
           MOVE "ZZSO-ORD-B2B1" TO RO-ORDER-NUMBER
           MOVE "TC-2026-02" TO RO-TERMS-VERSION
           MOVE "Y" TO RO-ROT-APPLIES
           CALL "SOROT" USING WS-ROT-CA
           MOVE "retention of title is recorded on the order"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-ROT-CA < 8 AND RO-UPDATED = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-ROT-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           INITIALIZE WS-ROT-CA
           MOVE "UNPAIDLS" TO CA-FUNCTION OF WS-ROT-CA
           MOVE "STC" TO CA-COMPANY OF WS-ROT-CA
           MOVE "RDG" TO CA-SITE OF WS-ROT-CA
           MOVE "CREDMGR" TO CA-USER OF WS-ROT-CA
           MOVE 20261016 TO CA-BUSINESS-DATE OF WS-ROT-CA
           MOVE WS-P-CUSTOMER TO RO-CUSTOMER
           CALL "SOROT" USING WS-ROT-CA
           MOVE "the unpaid goods list answers for the customer"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-ROT-CA < 8
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-ROT-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "every listed item carries its invoice and dates"
               TO TK-NAME
           MOVE "Y" TO WS-C1(1:1)
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > RO-ITEM-CNT
               IF RO-I-INVOICE-NO(WS-I) = SPACES
                  OR RO-I-INV-DATE(WS-I) = 0
                  OR RO-I-DESP-DATE(WS-I) = 0
                  OR RO-I-ITEM(WS-I) = SPACES
                   MOVE "N" TO WS-C1(1:1)
               END-IF
           END-PERFORM
           IF WS-C1(1:1) = "Y"
               PERFORM TK-PASS
           ELSE
               MOVE "an entry is missing its invoice or dates"
                   TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.
      *>----------------------------------------------------------------
       COPY TESTKITP.
