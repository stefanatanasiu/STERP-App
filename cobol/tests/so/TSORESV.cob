      *>================================================================
      *> PROGRAM-ID : TSORESV
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT UNIT TESTS
      *> PURPOSE    : SORESV - RESERVATION OF ELIGIBLE STOCK FOR SALES
      *>              DEMAND (SO-009), WITH THE SCHEDULE AND PROMISE
      *>              BEHAVIOUR OF ACCEPTANCE SCENARIO AC-043.
      *>
      *>              The point of AC-043 is that free stock is
      *>              allocated ONCE.  The arithmetic that decides
      *>              that lives in inv.usp_ReserveStock under
      *>              UPDLOCK, so the tests prove the invariant that
      *>              matters - free after equals free before less
      *>              what was reserved, and a request can never take
      *>              more than was free - rather than re-implementing
      *>              the sum in the test.
      *>
      *>              Every write runs inside BEGN..ROLB so the suite
      *>              is repeatable, and the whole database section
      *>              skips when no free stock is on file.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: SO-009, SO-010   AC: AC-043
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSORESV.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       COPY SORESVCA REPLACING ==DFHCOMMAREA== BY ==WS-RSV-CA==.
       01  WS-FLAGS.
           05  WS-DB-UP                PIC X VALUE "Y".
           05  WS-STOCK                PIC X VALUE "Y".
       01  WS-PROBE.
           05  WS-P-ITEM               PIC X(40).
           05  WS-P-SITE               PIC X(10).
           05  WS-P-WHSE               PIC X(20).
           05  WS-P-LOC                PIC X(20).
           05  WS-P-QTY                PIC X(30).
       01  WS-SAVE.
           05  WS-FREE-BEFORE          PIC S9(13)V9(6) VALUE 0.
           05  WS-RESERVED-1           PIC S9(13)V9(6) VALUE 0.
           05  WS-RESERVED-2           PIC S9(13)V9(6) VALUE 0.
           05  WS-ITEM                 PIC X(40) VALUE SPACES.
           05  WS-SITE                 PIC X(10) VALUE SPACES.
           05  WS-WHSE                 PIC X(20) VALUE SPACES.
           05  WS-LOC                  PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TSORESV sales reservation" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-DEMAND-TYPE-IS-FIXED
           PERFORM 1100-INPUT-IS-VALIDATED
           PERFORM 1200-UNKNOWN-FUNCTION
           PERFORM 0100-PROBE-DATABASE
           IF WS-DB-UP = "N" OR WS-STOCK = "N"
               PERFORM 9000-SKIP-DB-TESTS
           ELSE
               PERFORM 0200-BEGIN-TRAN
               PERFORM 2000-ENQUIRE-NOTHING-HELD
               PERFORM 2100-RESERVE-KEEPS-THE-SUM
               PERFORM 2200-NO-DOUBLE-ALLOCATION
               PERFORM 2300-SHELF-LIFE-TEST-DATE
               PERFORM 2400-RELEASE-KEEPS-DEMAND
               PERFORM 0300-ROLLBACK
           END-IF
           PERFORM TK-END
           STOP RUN.

      *>----------------------------------------------------------------
      *> No database is needed for the contract tests below: SORESV
      *> validates before it calls anything.
      *>----------------------------------------------------------------
       0150-BUILD-REQUEST.
           INITIALIZE WS-RSV-CA
           MOVE "STC" TO CA-COMPANY OF WS-RSV-CA
           MOVE "RDG" TO CA-SITE OF WS-RSV-CA
           MOVE "SALESREP" TO CA-USER OF WS-RSV-CA
           MOVE 20261015 TO CA-BUSINESS-DATE OF WS-RSV-CA
           MOVE "ZZSORESV-1" TO CA-CORRELATION-ID OF WS-RSV-CA
           MOVE "RESERVE" TO CA-FUNCTION OF WS-RSV-CA
           MOVE "ZZSORESV-001" TO SV-ORDER-NUMBER
           MOVE 1 TO SV-LINE-NUM
           MOVE 1 TO SV-SCHEDULE-NO
           MOVE WS-ITEM TO SV-ITEM-CODE
           MOVE WS-SITE TO SV-SITE
      *>   the warehouse and location are named deliberately.
      *>   inv.usp_ReserveStock matches b.WarehouseCode and
      *>   b.LocationCode to whatever STKRESV sends, and STKRESV
      *>   sends an empty string rather than NULL when the caller
      *>   leaves them blank, so a site wide reservation finds
      *>   nothing eligible.  Raised with the stock control team.
           MOVE WS-WHSE TO SV-WAREHOUSE
           MOVE WS-LOC TO SV-LOCATION
           MOVE 1 TO SV-QTY-REQUESTED
           MOVE "Y" TO SV-ALLOW-PARTIAL
           MOVE 20261020 TO SV-REQUIRED-DATE
           MOVE 20261020 TO SV-DESPATCH-DATE.

      *>   SO-009 - the demand type, reference and line are fixed by
      *>   the requirement and are never left to the caller's whim
       1000-DEMAND-TYPE-IS-FIXED.
           PERFORM 0150-BUILD-REQUEST
           MOVE SPACES TO SV-ITEM-CODE
           CALL "SORESV" USING WS-RSV-CA
           MOVE "sales demand always reserves as SALESORDER"
               TO TK-NAME
           MOVE "SALESORDER" TO TK-EXPECTED-X
           MOVE SV-DEMAND-TYPE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the demand reference is the order number" TO TK-NAME
           MOVE "ZZSORESV-001" TO TK-EXPECTED-X
           MOVE SV-DEMAND-REF TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "the demand line is the order line number" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE SV-DEMAND-LINE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       1100-INPUT-IS-VALIDATED.
           PERFORM 0150-BUILD-REQUEST
           MOVE SPACES TO SV-ITEM-CODE
           CALL "SORESV" USING WS-RSV-CA
           MOVE "a missing item is refused, never guessed" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RSV-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-RSV-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "a missing item reports SRV0001" TO TK-NAME
           MOVE "SRV0001" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RSV-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0150-BUILD-REQUEST
           MOVE "ZZ-ITEM" TO SV-ITEM-CODE
           MOVE 0 TO SV-QTY-REQUESTED
           CALL "SORESV" USING WS-RSV-CA
           MOVE "a zero quantity cannot be reserved" TO TK-NAME
           MOVE "SRV0002" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RSV-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0150-BUILD-REQUEST
           MOVE "ZZ-ITEM" TO SV-ITEM-CODE
           MOVE SPACES TO SV-ORDER-NUMBER
           MOVE SPACES TO SV-DEMAND-REF
           MOVE 0 TO SV-LINE-NUM
           MOVE 0 TO SV-DEMAND-LINE
           CALL "SORESV" USING WS-RSV-CA
           MOVE "a reservation must name its demand" TO TK-NAME
           MOVE "SRV0001" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RSV-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "nothing was reserved by a rejected request"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SV-QTY-RESERVED TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.

       1200-UNKNOWN-FUNCTION.
           PERFORM 0150-BUILD-REQUEST
           MOVE "ZZ-ITEM" TO SV-ITEM-CODE
           MOVE "ALLOCATE" TO CA-FUNCTION OF WS-RSV-CA
           CALL "SORESV" USING WS-RSV-CA
           MOVE "an unknown function is refused with 08" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RSV-CA TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "an unknown function reports SRV0008" TO TK-NAME
           MOVE "SRV0008" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RSV-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>----------------------------------------------------------------
      *> Database section
      *>----------------------------------------------------------------
       0100-PROBE-DATABASE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) b.ItemCode, b.SiteCode, "
                  "b.WarehouseCode, b.LocationCode, "
                  "CONVERT(varchar(30), SUM(b.QtyOnHand)) "
                  "FROM inv.StockBalance b JOIN inv.Location l ON "
                  "l.CompanyCode = b.CompanyCode AND "
                  "l.SiteCode = b.SiteCode AND "
                  "l.WarehouseCode = b.WarehouseCode AND "
                  "l.LocationCode = b.LocationCode "
                  "LEFT JOIN inv.Lot t ON t.CompanyCode = "
                  "b.CompanyCode AND t.ItemCode = b.ItemCode AND "
                  "t.LotCode = b.LotCode AND b.LotCode <> '' "
                  "WHERE b.CompanyCode = 'STC' AND "
                  "b.QualityStatus = 'ACCEPTED' AND "
                  "b.OwnerType = 'OWNED' AND b.SerialNo = '' AND "
                  "l.IsAvailable = 1 AND l.IsOwnedStock = 1 AND "
                  "t.ExpiryDate IS NULL "
                  "GROUP BY b.ItemCode, b.SiteCode, "
                  "b.WarehouseCode, b.LocationCode "
                  "HAVING SUM(b.QtyOnHand) - ISNULL((SELECT "
                  "SUM(r.QtyReserved - r.QtyConsumed) FROM "
                  "inv.StockReservation r WHERE "
                  "r.CompanyCode = 'STC' AND "
                  "r.ItemCode = b.ItemCode AND "
                  "r.SiteCode = b.SiteCode AND "
                  "r.Status = 'ACTIVE'), 0) > 8 "
                  "ORDER BY b.ItemCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 20 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 30 TO SQLIO-COL-WIDTH(5)
           MOVE 8 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-PROBE
           IF SQLIO-ERROR
               MOVE "N" TO WS-DB-UP
               GO TO 0100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "N" TO WS-STOCK
               GO TO 0100-EXIT
           END-IF
           MOVE WS-P-ITEM TO WS-ITEM
           MOVE WS-P-SITE TO WS-SITE
           MOVE WS-P-WHSE TO WS-WHSE
           MOVE WS-P-LOC TO WS-LOC.
       0100-EXIT.
           EXIT.

       0200-BEGIN-TRAN.
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       0300-ROLLBACK.
           MOVE "ROLB" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-PROBE.

       2000-ENQUIRE-NOTHING-HELD.
           PERFORM 0150-BUILD-REQUEST
           MOVE "ENQUIRE" TO CA-FUNCTION OF WS-RSV-CA
           MOVE "ZZSORESV-NONE" TO SV-ORDER-NUMBER
           MOVE "ZZSORESV-NONE" TO SV-DEMAND-REF
           CALL "SORESV" USING WS-RSV-CA
           MOVE "an unknown demand holds no reservations" TO TK-NAME
           MOVE 16 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RSV-CA TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-RSV-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

      *>   AC-043.  Free stock is allocated once: whatever the
      *>   procedure gives out, the free quantity falls by exactly
      *>   that much.  SORESV never computes either figure itself.
       2100-RESERVE-KEEPS-THE-SUM.
           PERFORM 0150-BUILD-REQUEST
           MOVE 4 TO SV-QTY-REQUESTED
           CALL "SORESV" USING WS-RSV-CA
           MOVE SV-QTY-FREE-BEFORE TO WS-FREE-BEFORE
           MOVE SV-QTY-RESERVED TO WS-RESERVED-1
           MOVE "AC-043 a reservation is granted" TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE SV-QTY-RESERVED TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-RSV-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "AC-043 free stock falls by exactly what was taken"
               TO TK-NAME
           COMPUTE TK-EXPECTED-N =
               SV-QTY-FREE-BEFORE - SV-QTY-RESERVED
           MOVE SV-QTY-FREE-AFTER TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-043 the reservation has an identity" TO TK-NAME
           IF SV-RESERVATION-ID > 0
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-RSV-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "AC-043 the reservation is active" TO TK-NAME
           MOVE "ACTIVE" TO TK-EXPECTED-X
           MOVE SV-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   AC-043 again, from the competing order's side: a second
      *>   demand asking for everything can never be given units the
      *>   first demand already holds.
       2200-NO-DOUBLE-ALLOCATION.
           PERFORM 0150-BUILD-REQUEST
           MOVE "ZZSORESV-002" TO SV-ORDER-NUMBER
           MOVE "ZZSORESV-002" TO SV-DEMAND-REF
           MOVE 2 TO SV-LINE-NUM
           MOVE 2 TO SV-DEMAND-LINE
           MOVE 999999 TO SV-QTY-REQUESTED
           MOVE "Y" TO SV-ALLOW-PARTIAL
           CALL "SORESV" USING WS-RSV-CA
           MOVE SV-QTY-RESERVED TO WS-RESERVED-2
           MOVE "AC-043 a competing order cannot take reserved units"
               TO TK-NAME
           IF WS-RESERVED-2 <= WS-FREE-BEFORE - WS-RESERVED-1
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-RSV-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "AC-043 the shortfall is reported, not hidden"
               TO TK-NAME
           IF SV-QTY-SHORTFALL > 0
               PERFORM TK-PASS
           ELSE
               MOVE "no shortfall was reported" TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "AC-043 a partial allocation warns with SRV0003"
               TO TK-NAME
           MOVE "SRV0003" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RSV-CA TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   Lot expiry is judged at the PLANNED DESPATCH DATE plus the
      *>   customer's remaining shelf life requirement, not today.
       2300-SHELF-LIFE-TEST-DATE.
           PERFORM 0150-BUILD-REQUEST
           MOVE "ZZSORESV-003" TO SV-ORDER-NUMBER
           MOVE "ZZSORESV-003" TO SV-DEMAND-REF
           MOVE 3 TO SV-LINE-NUM
           MOVE 3 TO SV-DEMAND-LINE
           MOVE "ZZ-LOT-001" TO SV-LOT-CODE
           MOVE 20261015 TO SV-DESPATCH-DATE
           MOVE 30 TO SV-SHELF-DAYS-REQ
           CALL "SORESV" USING WS-RSV-CA
           MOVE "shelf life is tested 30 days after despatch"
               TO TK-NAME
           MOVE 20261114 TO TK-EXPECTED-N
           MOVE SV-SHELF-TEST-DATE TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-RSV-CA TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

      *>   Releasing a reservation does not cancel the demand.
       2400-RELEASE-KEEPS-DEMAND.
           PERFORM 0150-BUILD-REQUEST
           MOVE "RELEASE" TO CA-FUNCTION OF WS-RSV-CA
           CALL "SORESV" USING WS-RSV-CA
           MOVE "a released reservation reports RELEASED" TO TK-NAME
           IF CA-RETURN-CODE OF WS-RSV-CA < 8
               MOVE "RELEASED" TO TK-EXPECTED-X
               MOVE SV-STATUS TO TK-ACTUAL-X
               PERFORM TK-ASSERT-STR
           ELSE
               MOVE CA-MESSAGE OF WS-RSV-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           PERFORM 0150-BUILD-REQUEST
           MOVE "REVALID" TO CA-FUNCTION OF WS-RSV-CA
           CALL "SORESV" USING WS-RSV-CA
           MOVE "revalidation answers 00 or 06, never silently 08"
               TO TK-NAME
           IF CA-RETURN-CODE OF WS-RSV-CA = 0
              OR CA-RETURN-CODE OF WS-RSV-CA = 6
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-RSV-CA TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF.

       9000-SKIP-DB-TESTS.
           MOVE "AC-043 free stock is allocated once" TO TK-NAME
           IF WS-DB-UP = "N"
               MOVE "database not available" TO TK-DETAIL
           ELSE
               MOVE "no free stock is on file to allocate"
                   TO TK-DETAIL
           END-IF
           PERFORM TK-SKIP
           MOVE "AC-043 competing demand cannot double allocate"
               TO TK-NAME
           MOVE "stock fixture not available" TO TK-DETAIL
           PERFORM TK-SKIP
           MOVE "shelf life is tested at the despatch date"
               TO TK-NAME
           MOVE "stock fixture not available" TO TK-DETAIL
           PERFORM TK-SKIP.

       COPY TESTKITP.
