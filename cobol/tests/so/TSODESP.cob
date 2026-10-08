      *>================================================================
      *> PROGRAM-ID : TSODESP
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT UNIT TESTS
      *> PURPOSE    : PICKGEN AND DESPATCH - ACCEPTANCE SCENARIO
      *>              AC-044, "PICK VERSUS DESPATCH".
      *>
      *>              Two things have to be true and both are proved
      *>              here against the database:
      *>
      *>              1  PICKING ALONE HAS NO DESPATCH OR COGS
      *>                 EFFECT.  Generating and confirming a pick
      *>                 list - including a controlled short pick -
      *>                 moves no stock, posts no cost of sale, sets
      *>                 no despatched quantity and leaves the
      *>                 outstanding demand intact.  Printing it
      *>                 again changes nothing but the print count.
      *>
      *>              2  A REPEAT CONFIRMATION DOES NOT CONSUME STOCK
      *>                 TWICE.  Re-sending a confirmed instruction
      *>                 with the same correlation id returns the
      *>                 existing despatch with return code 04 and
      *>                 creates no second despatch and no second
      *>                 stock movement.
      *>
      *>              Fixtures are prefixed ZZSOTEST so the suite is
      *>              repeatable; the idempotency test runs inside
      *>              BEGN..ROLB and everything skips when the
      *>              database is not available.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: SO-012, SO-013, SO-007, SO-022   AC: AC-044
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSODESP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY PICKGENCA REPLACING ==DFHCOMMAREA== BY ==WS-PCK-CA==.
       COPY DESPATCHCA REPLACING ==DFHCOMMAREA== BY ==WS-DSP-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
           05  WS-FIXTURE              PIC X VALUE "Y".
       01  WS-PROBE.
           05  WS-P-ITEM               PIC X(40).
           05  WS-P-SITE               PIC X(10).
           05  WS-P-WHSE               PIC X(20).
           05  WS-P-LOC                PIC X(20).
           05  WS-P-CUST               PIC X(20).
       01  WS-ROW.
           05  WS-R1                   PIC X(40).
           05  WS-R2                   PIC X(40).
       01  WS-SAVE.
           05  WS-ITEM                 PIC X(40) VALUE SPACES.
           05  WS-SITE                 PIC X(10) VALUE SPACES.
           05  WS-WHSE                 PIC X(20) VALUE SPACES.
           05  WS-LOC                  PIC X(20) VALUE SPACES.
           05  WS-CUST                 PIC X(20) VALUE SPACES.
           05  WS-LINE-ID              PIC S9(17) VALUE 0.
           05  WS-MOVES-BEFORE         PIC S9(9) VALUE 0.
           05  WS-MOVES-AFTER          PIC S9(9) VALUE 0.
           05  WS-DESP-BEFORE          PIC S9(9) VALUE 0.
           05  WS-DESP-AFTER           PIC S9(9) VALUE 0.
           05  WS-PICKREF              PIC X(30) VALUE SPACES.
           05  WS-I                    PIC S9(4) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSODESP pick versus despatch" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-PICKING-MOVES-NOTHING
           PERFORM 1100-DESPATCH-GUARDS
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N" OR WS-FIXTURE = "N"
               PERFORM 9000-SKIP-DB-TESTS
           ELSE
               PERFORM 0200-CLEAN-UP
               PERFORM 0300-BUILD-ORDER
               IF WS-FIXTURE = "Y"
                   PERFORM 2000-GENERATE-PICK-LIST
                   PERFORM 2100-SHORT-PICK-KEEPS-DEMAND
                   PERFORM 2200-NO-STOCK-NO-COGS
                   PERFORM 2300-PRINT-IS-INERT
                   PERFORM 2400-DESPATCH-DEFAULTS-BIN
               ELSE
                   PERFORM 9000-SKIP-DB-TESTS
               END-IF
               PERFORM 3000-REPEAT-CONFIRMATION
               PERFORM 0200-CLEAN-UP
           END-IF
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
      *> AC-044 part one, without any database: PICKGEN states on
      *> every single call that it has moved no stock, created no cost
      *> of sale and despatched nothing.
      *>----------------------------------------------------------------
       1000-PICKING-MOVES-NOTHING.
           INITIALIZE WS-PCK-CA
           MOVE "STC" TO CA-COMPANY OF WS-PCK-CA
           MOVE "RDG" TO CA-SITE OF WS-PCK-CA
           MOVE "WHSE" TO CA-USER OF WS-PCK-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-PCK-CA
           MOVE "GENERATE" TO CA-FUNCTION OF WS-PCK-CA
           MOVE SPACES TO PK-ORDER-NUMBER
           CALL "PICKGEN" USING WS-PCK-CA
           MOVE "a pick list needs an order number" TO TK-NAME
           MOVE "PCK0001" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-PCK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-044 picking never moves stock" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE PK-STOCK-MOVED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-044 picking never creates a cost of sale"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE PK-COGS-CREATED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-044 picking never despatches a quantity"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE PK-DESPATCHED-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "RELEASE" TO CA-FUNCTION OF WS-PCK-CA
           MOVE "ZZSOTEST-X" TO PK-PICKLIST-REF
           CALL "PICKGEN" USING WS-PCK-CA
           MOVE "an unknown pick function reports PCK0008" TO TK-NAME
           MOVE "PCK0008" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-PCK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

       1100-DESPATCH-GUARDS.
           PERFORM 0150-BUILD-DESPATCH
           MOVE SPACES TO CA-CORRELATION-ID OF WS-DSP-CA
           CALL "DESPATCH" USING WS-DSP-CA
           MOVE "a confirmation needs a correlation id" TO TK-NAME
           MOVE "DSP0001" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-DSP-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0150-BUILD-DESPATCH
           MOVE 0 TO DS-LINE-CNT
           CALL "DESPATCH" USING WS-DSP-CA
           MOVE "a confirmation needs at least one line" TO TK-NAME
           MOVE "DSP0013" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-DSP-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0150-BUILD-DESPATCH
           MOVE "SHIP" TO CA-FUNCTION OF WS-DSP-CA
           CALL "DESPATCH" USING WS-DSP-CA
           MOVE "an unknown despatch function reports DSP0008"
               TO TK-NAME
           MOVE "DSP0008" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-DSP-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0150-BUILD-DESPATCH
           MOVE "REVERSE" TO CA-FUNCTION OF WS-DSP-CA
           MOVE "ZZSOTEST-D1" TO DS-REVERSAL-OF
           MOVE SPACES TO DS-REVERSE-APPR
           MOVE SPACES TO DS-REVERSE-REASON
           CALL "DESPATCH" USING WS-DSP-CA
           MOVE "a reversal needs authority and a reason" TO TK-NAME
           MOVE "DSP0010" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-DSP-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "a refused despatch posts no cost of sale" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE DS-COGS-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       0150-BUILD-DESPATCH.
           INITIALIZE WS-DSP-CA
           MOVE "STC" TO CA-COMPANY OF WS-DSP-CA
           MOVE "RDG" TO CA-SITE OF WS-DSP-CA
           MOVE "WHSE" TO CA-USER OF WS-DSP-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-DSP-CA
           MOVE "ZZSOTEST-C1" TO CA-CORRELATION-ID OF WS-DSP-CA
           MOVE "CONFIRM" TO CA-FUNCTION OF WS-DSP-CA
           MOVE "ZZSOTEST-O1" TO DS-ORDER-NUMBER
           MOVE "RDG" TO DS-SITE
           MOVE 20261015 TO DS-DESPATCH-DATE
           MOVE 20261015 TO DS-POSTING-DATE
           MOVE 1 TO DS-LINE-CNT
           MOVE 1 TO DS-L-ORDER-LINE(1)
           MOVE 1 TO DS-L-SCHEDULE(1)
           MOVE 1 TO DS-L-QTY(1).

      *>----------------------------------------------------------------
      *> Database fixtures
      *>----------------------------------------------------------------
       0100-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) b.ItemCode, b.SiteCode, "
                  "b.WarehouseCode, b.LocationCode, "
                  "(SELECT TOP (1) c.CustomerCode FROM ar.Customer c "
                  "WHERE c.CompanyCode = 'STC' ORDER BY "
                  "c.CustomerCode) FROM inv.StockBalance b "
                  "JOIN inv.Location l ON "
                  "l.CompanyCode = b.CompanyCode AND "
                  "l.SiteCode = b.SiteCode AND "
                  "l.WarehouseCode = b.WarehouseCode AND "
                  "l.LocationCode = b.LocationCode "
                  "WHERE b.CompanyCode = 'STC' AND "
                  "b.QualityStatus = 'ACCEPTED' AND "
                  "b.OwnerType = 'OWNED' AND b.SerialNo = '' AND "
                  "b.LotCode = '' AND l.IsAvailable = 1 AND "
                  "l.IsOwnedStock = 1 AND b.QtyOnHand > 20 "
                  "ORDER BY b.ItemCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 20 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 20 TO SQLIO-COL-WIDTH(5)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR
               MOVE "N" TO WS-DB-UP
               GO TO 0100-EXIT
           END-IF
           IF SQLIO-NOTFOUND OR WS-P-CUST = SPACES
               MOVE "N" TO WS-FIXTURE
               GO TO 0100-EXIT
           END-IF
           MOVE WS-P-ITEM TO WS-ITEM
           MOVE WS-P-SITE TO WS-SITE
           MOVE WS-P-WHSE TO WS-WHSE
           MOVE WS-P-LOC TO WS-LOC
           MOVE WS-P-CUST TO WS-CUST.
       0100-EXIT.
           EXIT.

      *>   remove anything an earlier run of this suite left behind
       0200-CLEAN-UP.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; "
                  "DELETE so.PickListLine WHERE PickListId IN "
                  "(SELECT PickListId FROM so.PickList WHERE "
                  "OrderNumber LIKE 'ZZSOTEST%'); "
                  "DELETE so.PickList WHERE OrderNumber LIKE "
                  "'ZZSOTEST%'; "
                  "DELETE so.DespatchLine WHERE DespatchId IN "
                  "(SELECT DespatchId FROM so.Despatch WHERE "
                  "DespatchRef LIKE 'ZZSOTEST%'); "
                  "DELETE so.Despatch WHERE DespatchRef LIKE "
                  "'ZZSOTEST%'; "
                  "DELETE so.DeliverySchedule WHERE OrderNumber "
                  "LIKE 'ZZSOTEST%'; "
                  "DELETE so.SalesOrderLine WHERE OrderNumber LIKE "
                  "'ZZSOTEST%'; "
                  "DELETE so.SalesOrder WHERE OrderNumber LIKE "
                  "'ZZSOTEST%'; "
                  "DELETE core.IdempotencyKey WHERE CorrelationId "
                  "LIKE 'ZZSOTEST%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

      *>   a released order for ten units with one schedule
       0300-BUILD-ORDER.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT so.SalesOrder "
                  "(CompanyCode, SiteCode, OrderNumber, "
                  "CustomerCode, OrderDate, CurrencyCode, FxRate, "
                  "CommercialStatus, ReleaseStatus, CorrelationId, "
                  "CreatedBy) VALUES ('STC', ?, 'ZZSOTEST-O1', ?, "
                  "'20261015', 'GBP', 1, 'ACCEPTED', 'RELEASED', "
                  "'ZZSOTEST-O1', 'TEST'); "
                  "DECLARE @o bigint = SCOPE_IDENTITY(); "
                  "INSERT so.SalesOrderLine (SalesOrderId, "
                  "CompanyCode, OrderNumber, LineNum, LineRef, "
                  "ItemCode, Description, LineType, AcceptedQty, "
                  "ConversionFactor, UnitPrice, PricePerQty, "
                  "NetAmount, Status) VALUES (@o, 'STC', "
                  "'ZZSOTEST-O1', 1, '1', ?, 'Pick versus despatch "
                  "fixture', 'STOCK', 10, 1, 10, 1, 100, 'OPEN'); "
                  "DECLARE @l bigint = SCOPE_IDENTITY(); "
                  "INSERT so.DeliverySchedule (SalesOrderLineId, "
                  "CompanyCode, OrderNumber, LineNum, ScheduleNo, "
                  "Quantity, SiteCode, RequestedDate, "
                  "OriginalPromiseDate, CurrentPromiseDate) VALUES "
                  "(@l, 'STC', 'ZZSOTEST-O1', 1, 1, 10, ?, "
                  "'20261020', '20261020', '20261020'); "
                  "SELECT CONVERT(varchar(24), @l), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE WS-SITE TO SQLIO-PARM-VAL(1)
           MOVE WS-CUST TO SQLIO-PARM-VAL(2)
           MOVE WS-ITEM TO SQLIO-PARM-VAL(3)
           MOVE WS-SITE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-FIXTURE
               MOVE 0 TO SQLIO-SQLCODE
               GO TO 0300-EXIT
           END-IF
           COMPUTE WS-LINE-ID = FUNCTION NUMVAL(WS-R1)
           PERFORM 0400-COUNT-MOVEMENTS
           MOVE WS-MOVES-AFTER TO WS-MOVES-BEFORE.
       0300-EXIT.
           EXIT.

       0400-COUNT-MOVEMENTS.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(20), COUNT(*)), "
                  "CONVERT(varchar(20), (SELECT COUNT(*) FROM "
                  "so.Despatch d WHERE d.CompanyCode = 'STC')) "
                  "FROM inv.StockMovement m WHERE "
                  "m.CompanyCode = 'STC'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE 0 TO SQLIO-SQLCODE
               GO TO 0400-EXIT
           END-IF
           COMPUTE WS-MOVES-AFTER = FUNCTION NUMVAL(WS-R1)
           COMPUTE WS-DESP-AFTER = FUNCTION NUMVAL(WS-R2).
       0400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       2000-GENERATE-PICK-LIST.
           INITIALIZE WS-PCK-CA
           MOVE "STC" TO CA-COMPANY OF WS-PCK-CA
           MOVE WS-SITE TO CA-SITE OF WS-PCK-CA
           MOVE "WHSE" TO CA-USER OF WS-PCK-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-PCK-CA
           MOVE "ZZSOTEST-G1" TO CA-CORRELATION-ID OF WS-PCK-CA
           MOVE "GENERATE" TO CA-FUNCTION OF WS-PCK-CA
           MOVE "ZZSOTEST-O1" TO PK-ORDER-NUMBER
           MOVE WS-SITE TO PK-SITE
           MOVE WS-WHSE TO PK-WAREHOUSE
           CALL "PICKGEN" USING WS-PCK-CA
           MOVE PK-PICKLIST-REF TO WS-PICKREF
           MOVE "a released order generates a pick list" TO TK-NAME
           IF CA-RETURN-CODE OF WS-PCK-CA = 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-PCK-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "the pick list carries the one open schedule"
               TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE PK-GENERATED-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "the required quantity is the schedule quantity"
               TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE PK-L-REQUIRED(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-044 generating a pick list moves no stock"
               TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE PK-STOCK-MOVED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AC-044 - a controlled short pick preserves the back order
       2100-SHORT-PICK-KEEPS-DEMAND.
           INITIALIZE WS-PCK-CA
           MOVE "STC" TO CA-COMPANY OF WS-PCK-CA
           MOVE WS-SITE TO CA-SITE OF WS-PCK-CA
           MOVE "WHSE" TO CA-USER OF WS-PCK-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-PCK-CA
           MOVE "ZZSOTEST-G2" TO CA-CORRELATION-ID OF WS-PCK-CA
           MOVE "CONFIRM" TO CA-FUNCTION OF WS-PCK-CA
           MOVE WS-PICKREF TO PK-PICKLIST-REF
           MOVE 1 TO PK-LINE-NUM
           MOVE 4 TO PK-PICKED-QTY
           MOVE 0 TO PK-SHORT-QTY
           MOVE SPACES TO PK-SHORT-REASON
           CALL "PICKGEN" USING WS-PCK-CA
           MOVE "a short pick without a reason is refused" TO TK-NAME
           MOVE "PCK0005" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-PCK-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "CONFIRM" TO CA-FUNCTION OF WS-PCK-CA
           MOVE WS-PICKREF TO PK-PICKLIST-REF
           MOVE 1 TO PK-LINE-NUM
           MOVE 4 TO PK-PICKED-QTY
           MOVE 0 TO PK-SHORT-QTY
           MOVE "Only four were on the shelf" TO PK-SHORT-REASON
           CALL "PICKGEN" USING WS-PCK-CA
           MOVE "a reasoned short pick is accepted with a warning"
               TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-PCK-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-PCK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "AC-044 the short quantity is recorded" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE PK-SHORT-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-044 the outstanding demand survives" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE PK-OUTSTANDING-QTY TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>   AC-044 - the whole point: after picking, the stock ledger
      *>   and the despatched quantity are exactly as they were
       2200-NO-STOCK-NO-COGS.
           PERFORM 0400-COUNT-MOVEMENTS
           MOVE "AC-044 picking creates no stock movement" TO TK-NAME
           MOVE WS-MOVES-BEFORE TO TK-EXPECTED-N
           MOVE WS-MOVES-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(30), l.DespatchedQty), "
                  "CONVERT(varchar(30), s.DespatchedQty) FROM "
                  "so.SalesOrderLine l JOIN so.DeliverySchedule s "
                  "ON s.SalesOrderLineId = l.SalesOrderLineId "
                  "WHERE l.SalesOrderLineId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-LINE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "AC-044 the order line is still undespatched"
               TO TK-NAME
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "order line could not be read" TO TK-DETAIL
               MOVE 0 TO SQLIO-SQLCODE
               PERFORM TK-FAIL
           ELSE
               MOVE 0 TO TK-EXPECTED-N
               COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-R1)
               PERFORM TK-ASSERT-NUM
               MOVE "AC-044 the schedule is still undespatched"
                   TO TK-NAME
               MOVE 0 TO TK-EXPECTED-N
               COMPUTE TK-ACTUAL-N = FUNCTION NUMVAL(WS-R2)
               PERFORM TK-ASSERT-NUM
           END-IF.

      *>   SO-011 - printing is not a release and not an override
       2300-PRINT-IS-INERT.
           INITIALIZE WS-PCK-CA
           MOVE "STC" TO CA-COMPANY OF WS-PCK-CA
           MOVE WS-SITE TO CA-SITE OF WS-PCK-CA
           MOVE "WHSE" TO CA-USER OF WS-PCK-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-PCK-CA
           MOVE "PRINT" TO CA-FUNCTION OF WS-PCK-CA
           MOVE WS-PICKREF TO PK-PICKLIST-REF
           CALL "PICKGEN" USING WS-PCK-CA
           MOVE "printing increments the print count" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE PK-PRINT-COUNT TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-PCK-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           CALL "PICKGEN" USING WS-PCK-CA
           MOVE "reprinting increments it again and nothing else"
               TO TK-NAME
           MOVE 2 TO TK-EXPECTED-N
           MOVE PK-PRINT-COUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "reprinting moves no stock" TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE PK-STOCK-MOVED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0400-COUNT-MOVEMENTS
           MOVE "reprinting creates no stock movement" TO TK-NAME
           MOVE WS-MOVES-BEFORE TO TK-EXPECTED-N
           MOVE WS-MOVES-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

      *>----------------------------------------------------------------
      *> AC-044 part two - a repeat confirmation does not consume
      *> stock twice.  A despatch and its completed idempotency key
      *> are planted, the instruction is sent again and the service
      *> must answer with the original despatch and change nothing.
      *>----------------------------------------------------------------
       3000-REPEAT-CONFIRMATION.
           MOVE "AC-044 a repeat confirmation is not a second effect"
               TO TK-NAME
           IF WS-DB-UP = "N"
               MOVE "database not available" TO TK-DETAIL
               PERFORM TK-SKIP
               GO TO 3000-EXIT
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           PERFORM 3100-PLANT-DESPATCH
           IF WS-FIXTURE = "N"
               MOVE "despatch fixture could not be planted"
                   TO TK-DETAIL
               PERFORM TK-SKIP
               PERFORM 3900-ROLLBACK
               GO TO 3000-EXIT
           END-IF
           PERFORM 0400-COUNT-MOVEMENTS
           MOVE WS-MOVES-AFTER TO WS-MOVES-BEFORE
           MOVE WS-DESP-AFTER TO WS-DESP-BEFORE
           PERFORM 0150-BUILD-DESPATCH
           MOVE "ZZSOTEST-IDEM1"
               TO CA-CORRELATION-ID OF WS-DSP-CA
           CALL "DESPATCH" USING WS-DSP-CA
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-DSP-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-DSP-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "AC-044 the repeat is reported as a duplicate"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE DS-WAS-DUPLICATE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-044 the original despatch is returned" TO TK-NAME
           MOVE "ZZSOTEST-D1" TO TK-EXPECTED-X
           MOVE DS-DESPATCH-REF TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-044 the original cost of sale is unchanged"
               TO TK-NAME
           MOVE 123.45 TO TK-EXPECTED-N
           MOVE DS-COGS-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 0400-COUNT-MOVEMENTS
           MOVE "AC-044 no second stock movement was created"
               TO TK-NAME
           MOVE WS-MOVES-BEFORE TO TK-EXPECTED-N
           MOVE WS-MOVES-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-044 no second despatch was created" TO TK-NAME
           MOVE WS-DESP-BEFORE TO TK-EXPECTED-N
           MOVE WS-DESP-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           PERFORM 3900-ROLLBACK.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> CR2607.  A despatch follows the pick and does not restate the
      *> bin, so a confirmation that names neither a warehouse nor a
      *> location must name one for itself before it judges the line.
      *> The quantity asked for here is far beyond the demand, so the
      *> despatch is refused (DSP0004) and nothing is written - but
      *> the bin has already been defaulted and can be read back from
      *> the commarea and checked against the stock file.
      *>----------------------------------------------------------------
       2400-DESPATCH-DEFAULTS-BIN.
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           PERFORM 2410-CLEAR-THE-CREDIT
           PERFORM 0150-BUILD-DESPATCH
           MOVE "ZZSOTEST-BIN1"
               TO CA-CORRELATION-ID OF WS-DSP-CA
           MOVE WS-PICKREF TO DS-PICKLIST-REF
           MOVE 999 TO DS-L-QTY(1)
           MOVE SPACES TO DS-L-WHSE(1)
           MOVE SPACES TO DS-L-LOCATION(1)
           CALL "DESPATCH" USING WS-DSP-CA
           MOVE "SO-013 the line is judged, not the missing bin"
               TO TK-NAME
           MOVE "DSP0004" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-DSP-CA TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-DSP-CA TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           MOVE "SO-013 the despatch names the bin it would take from"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE "N" TO TK-ACTUAL-X
           IF DS-L-WHSE(1) NOT = SPACES
               AND DS-L-LOCATION(1) NOT = SPACES
               MOVE "Y" TO TK-ACTUAL-X
           END-IF
           MOVE SPACES TO TK-DETAIL
           STRING "warehouse '" FUNCTION TRIM(DS-L-WHSE(1))
                  "' location '" FUNCTION TRIM(DS-L-LOCATION(1)) "'"
               DELIMITED BY SIZE INTO TK-DETAIL
           PERFORM TK-ASSERT-STR
           PERFORM 2450-CHECK-DEFAULTED-BIN
           MOVE "SO-013 the defaulted bin holds owned accepted stock"
               TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE WS-R1 TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 3900-ROLLBACK.

      *>   The seeded customer carries an invoice four months past due,
      *>   so CREDCHK rightly holds every despatch for it.  What is
      *>   under test here is the bin, not the credit position, so the
      *>   debt is dated forward inside the transaction that is rolled
      *>   back - the customer file is left exactly as it was found.
       2410-CLEAR-THE-CREDIT.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; UPDATE ar.OpenItem SET DueDate = "
                  "DATEADD(year, 2, DueDate) WHERE CompanyCode = "
                  "'STC' AND CustomerCode = ? AND ResidualAmount "
                  "<> 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-CUST TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

       2450-CHECK-DEFAULTED-BIN.
           MOVE SPACES TO WS-R1
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CASE WHEN EXISTS (SELECT 1 FROM "
                  "inv.StockBalance b WHERE b.CompanyCode = 'STC' "
                  "AND b.ItemCode = ? AND b.SiteCode = ? AND "
                  "b.WarehouseCode = ? AND b.LocationCode = ? AND "
                  "b.QualityStatus = 'ACCEPTED' AND b.OwnerType = "
                  "'OWNED' AND b.QtyOnHand > 0) THEN 'Y' ELSE 'N' "
                  "END, ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE WS-ITEM TO SQLIO-PARM-VAL(1)
           MOVE WS-SITE TO SQLIO-PARM-VAL(2)
           MOVE DS-L-WHSE(1) TO SQLIO-PARM-VAL(3)
           MOVE DS-L-LOCATION(1) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

       3100-PLANT-DESPATCH.
           MOVE SPACES TO SQLIO-STMT
           STRING "SET NOCOUNT ON; INSERT so.Despatch (CompanyCode, "
                  "SiteCode, DespatchRef, CustomerCode, "
                  "OrderNumber, DespatchDate, PostingDate, Status, "
                  "CogsPostedGBP, CorrelationId, ConfirmedBy, "
                  "CreatedBy) VALUES ('STC', ?, 'ZZSOTEST-D1', ?, "
                  "'ZZSOTEST-O1', '20261015', '20261015', "
                  "'CONFIRMED', 123.45, 'ZZSOTEST-IDEM1', 'WHSE', "
                  "'TEST'); INSERT core.IdempotencyKey "
                  "(CorrelationId, Operation, CompanyCode, "
                  "UserCode, Status, ResultRef, EndedAt) VALUES "
                  "('ZZSOTEST-IDEM1', 'SO.DESPATCH.CONFIRM', 'STC', "
                  "'WHSE', 'COMPLETED', 'ZZSOTEST-D1', "
                  "SYSDATETIME()); SELECT '1', ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           IF WS-SITE = SPACES
               MOVE "RDG" TO SQLIO-PARM-VAL(1)
           ELSE
               MOVE WS-SITE TO SQLIO-PARM-VAL(1)
           END-IF
           IF WS-CUST = SPACES
               MOVE "C0001" TO SQLIO-PARM-VAL(2)
           ELSE
               MOVE WS-CUST TO SQLIO-PARM-VAL(2)
           END-IF
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE "N" TO WS-FIXTURE
               MOVE 0 TO SQLIO-SQLCODE
           END-IF.

       3900-ROLLBACK.
           MOVE "ROLB" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

       9000-SKIP-DB-TESTS.
           MOVE "AC-044 picking creates no stock movement" TO TK-NAME
           IF WS-DB-UP = "N"
               MOVE "database not available" TO TK-DETAIL
           ELSE
               MOVE "no stock or customer fixture is available"
                   TO TK-DETAIL
           END-IF
           PERFORM TK-SKIP
           MOVE "AC-044 the outstanding demand survives a short pick"
               TO TK-NAME
           MOVE "order fixture not available" TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "printing does not release or reserve" TO TK-NAME
           MOVE "order fixture not available" TO TK-DETAIL
           PERFORM TK-SKIP.

       COPY TESTKITP.
