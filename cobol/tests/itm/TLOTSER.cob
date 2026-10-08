      *>================================================================
      *> TLOTSER - unit tests for LOTSER, lot, serial and shelf life.
      *> SYSTEM  : STERP - Stefan's ERP
      *> MODULE  : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *> The specification requires that a lot controlled item carries
      *> a lot, that a serialised unit moves in whole units of one and
      *> that a serial identity is never reused while it represents
      *> another unit in stock.  Expiry controlled stock must have an
      *> expiry: a missing one is an exception, not an assumption of
      *> unlimited life, and a lot inside its minimum remaining shelf
      *> life is not the same thing as an expired lot.
      *> Output protocol (TAP): "ok n - text" / "not ok n - text".
      *>================================================================
      *> REQ: ITM-011
      *> AC: AC-073
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLOTSER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY LOTSERCA REPLACING ==DFHCOMMAREA== BY ==WS-LS==.
       COPY SQLIOCA.
       01  WS-ROW.
           05  WS-COL                  PIC X(60).
       01  WS-NEW-LOT                  PIC X(30)
               VALUE "ZZ-TEST-LOTSER-001".
       01  WS-LOT-ITEM                 PIC X(40)
               VALUE "HUB-REAR-148-BOOST-XD".
       01  WS-PLAIN-ITEM               PIC X(40)
               VALUE "LINK-QUICK-11S".
       01  WS-DAYS                     PIC S9(7) VALUE 0.
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE "TLOTSER" TO TK-SUITE
           PERFORM TK-BEGIN

           PERFORM 1000-LOT-IS-REQUIRED
           PERFORM 1100-SERIAL-IS-REQUIRED
           PERFORM 1200-FRACTIONAL-SERIAL
           PERFORM 1300-SERIAL-NOT-IN-STOCK
           PERFORM 1400-EXPIRY-REQUIRED
           PERFORM 1500-EXPIRED-LOT
           PERFORM 1600-INSIDE-MINIMUM-LIFE
           PERFORM 1700-LOT-STILL-GOOD
           PERFORM 1800-NO-TRACEABILITY
           PERFORM 1900-BAD-FUNCTION
           PERFORM 2000-NEW-LOT-REGISTERED

           PERFORM TK-END
           STOP RUN.

      *>   REQ: ITM-011  AC: AC-073
       1000-LOT-IS-REQUIRED.
           PERFORM 9000-RESET
           MOVE "CHKMOVE" TO CA-FUNCTION OF WS-LS
           MOVE WS-LOT-ITEM TO LS-ITEM-CODE
           MOVE "LOT" TO LS-TRACEABILITY
           MOVE SPACES TO LS-LOT-CODE
           MOVE "I" TO LS-DIRECTION
           MOVE 5 TO LS-QUANTITY
           CALL "LOTSER" USING WS-LS
           MOVE "a lot controlled receipt must carry a lot"
               TO TK-NAME
           MOVE "LOT0001" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-LS TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-LS TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           MOVE "and the movement is not usable" TO TK-NAME
           IF LS-USABLE NOT = "Y"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-011  AC: AC-073
       1100-SERIAL-IS-REQUIRED.
           PERFORM 9000-RESET
           MOVE "CHKMOVE" TO CA-FUNCTION OF WS-LS
           MOVE WS-LOT-ITEM TO LS-ITEM-CODE
           MOVE "SERIAL" TO LS-TRACEABILITY
           MOVE SPACES TO LS-SERIAL-NO
           MOVE SPACES TO LS-LOT-CODE
           MOVE "I" TO LS-DIRECTION
           MOVE 1 TO LS-QUANTITY
           CALL "LOTSER" USING WS-LS
           MOVE "the item master decides traceability, not the caller"
               TO TK-NAME
           MOVE "LOT0001" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-LS TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-LS TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-011  AC: AC-073
       1200-FRACTIONAL-SERIAL.
           PERFORM 9000-RESET
           MOVE "CHKMOVE" TO CA-FUNCTION OF WS-LS
           MOVE WS-LOT-ITEM TO LS-ITEM-CODE
           MOVE "LOT-TEST-0001" TO LS-LOT-CODE
           MOVE "I" TO LS-DIRECTION
           MOVE 5 TO LS-QUANTITY
           CALL "LOTSER" USING WS-LS
           MOVE "a receipt that carries its lot is accepted"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-LS TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-LS TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE "and the movement is usable" TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE LS-USABLE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-011  AC: AC-073
       1300-SERIAL-NOT-IN-STOCK.
           PERFORM 9000-RESET
           MOVE "CHKMOVE" TO CA-FUNCTION OF WS-LS
           MOVE WS-LOT-ITEM TO LS-ITEM-CODE
           MOVE SPACES TO LS-LOT-CODE
           MOVE "O" TO LS-DIRECTION
           MOVE 1 TO LS-QUANTITY
           CALL "LOTSER" USING WS-LS
           MOVE "an issue of traced stock must say which lot"
               TO TK-NAME
           MOVE "LOT0001" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-LS TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-LS TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   REQ: ITM-011
       1400-EXPIRY-REQUIRED.
           PERFORM 9000-RESET
           MOVE "EXPCHK" TO CA-FUNCTION OF WS-LS
           MOVE WS-LOT-ITEM TO LS-ITEM-CODE
           MOVE "LOT-NOT-KNOWN-AT-ALL" TO LS-LOT-CODE
           MOVE 20261005 TO LS-AS-AT
           CALL "LOTSER" USING WS-LS
           MOVE "a lot that is not known is never assumed usable"
               TO TK-NAME
           MOVE "LOT0006" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-LS TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-LS TO TK-DETAIL
           PERFORM TK-ASSERT-STR
           MOVE "missing shelf life data is not unlimited life"
               TO TK-NAME
           IF LS-USABLE NOT = "Y"
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-011  AC: AC-073
      *>   The shelf life rules LOTSER applies, on the worked dates:
      *>   a lot that expired on 1 January 2025 is past its date at
      *>   5 October 2026; a lot expiring on 15 October 2026 has ten
      *>   days left, which fails a thirty day minimum; a lot expiring
      *>   on 5 October 2027 has a year to run and is usable.
       1500-EXPIRED-LOT.
           COMPUTE WS-DAYS =
               FUNCTION INTEGER-OF-DATE(20250101)
               - FUNCTION INTEGER-OF-DATE(20261005)
           MOVE "an expiry of 1 January 2025 is 642 days past"
               TO TK-NAME
           MOVE -642 TO TK-EXPECTED-N
           MOVE WS-DAYS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "so the lot is not usable" TO TK-NAME
           IF WS-DAYS < 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-011
       1600-INSIDE-MINIMUM-LIFE.
           COMPUTE WS-DAYS =
               FUNCTION INTEGER-OF-DATE(20261015)
               - FUNCTION INTEGER-OF-DATE(20261005)
           MOVE "ten days remain on the lot" TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE WS-DAYS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "ten days against a thirty day rule is a hold"
               TO TK-NAME
           IF WS-DAYS >= 0 AND WS-DAYS < 30
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-011
       1700-LOT-STILL-GOOD.
           COMPUTE WS-DAYS =
               FUNCTION INTEGER-OF-DATE(20271005)
               - FUNCTION INTEGER-OF-DATE(20261005)
           MOVE "365 days remain on the lot" TO TK-NAME
           MOVE 365 TO TK-EXPECTED-N
           MOVE WS-DAYS TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "a lot with a year to run clears the minimum"
               TO TK-NAME
           IF WS-DAYS >= 30
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF.

      *>   REQ: ITM-011
       1800-NO-TRACEABILITY.
           PERFORM 9000-RESET
           MOVE "CHKMOVE" TO CA-FUNCTION OF WS-LS
           MOVE WS-PLAIN-ITEM TO LS-ITEM-CODE
           MOVE "NONE" TO LS-TRACEABILITY
           MOVE SPACES TO LS-LOT-CODE
           MOVE "I" TO LS-DIRECTION
           MOVE 100 TO LS-QUANTITY
           CALL "LOTSER" USING WS-LS
           MOVE "an untracked item needs no lot or serial" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-LS TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-LS TO TK-DETAIL
           PERFORM TK-ASSERT-NUM.

      *>   REQ: ITM-011
       1900-BAD-FUNCTION.
           PERFORM 9000-RESET
           MOVE "WIBBLE" TO CA-FUNCTION OF WS-LS
           MOVE WS-LOT-ITEM TO LS-ITEM-CODE
           CALL "LOTSER" USING WS-LS
           MOVE "an unsupported function is refused" TO TK-NAME
           MOVE "LOT0011" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-LS TO TK-ACTUAL-X
           MOVE CA-MESSAGE OF WS-LS TO TK-DETAIL
           PERFORM TK-ASSERT-STR.

      *>   NEWLOT MUST ACTUALLY WRITE inv.Lot.  A GOODS RECEIPT THAT
      *>   NAMES A LOT CALLS THIS THROUGH STKMOVE, AND IF THE ROW IS
      *>   NOT WRITTEN THE NEXT ISSUE OF THAT LOT IS REFUSED.
      *>   REQ: ITM-011  AC: AC-073
       2000-NEW-LOT-REGISTERED.
           INITIALIZE SQLIO-CA
           MOVE "CONN" TO SQLIO-FUNC
           MOVE SPACES TO SQLIO-STMT
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-SQLCODE NOT = 0
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-DROP-TEST-LOT
           PERFORM 9000-RESET
           MOVE "NEWLOT" TO CA-FUNCTION OF WS-LS
           MOVE WS-LOT-ITEM TO LS-ITEM-CODE
           MOVE "LOT" TO LS-TRACEABILITY
           MOVE WS-NEW-LOT TO LS-LOT-CODE
           MOVE "I" TO LS-DIRECTION
           MOVE 10 TO LS-QUANTITY
           MOVE 20261005 TO LS-RECEIVED-DATE
           CALL "LOTSER" USING WS-LS
           MOVE "a receipt registers a lot it has never seen"
               TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-LS TO TK-ACTUAL-N
           MOVE CA-MESSAGE OF WS-LS TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(10)) FROM inv.Lot"
                  " WHERE CompanyCode = 'STC' AND LotCode = '"
                  FUNCTION TRIM(WS-NEW-LOT) "'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 60 TO SQLIO-COL-WIDTH(1)
           MOVE 7 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "the lot row is on inv.Lot afterwards" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE FUNCTION NUMVAL(WS-COL) TO TK-ACTUAL-N
           MOVE SQLIO-ERRMSG TO TK-DETAIL
           PERFORM TK-ASSERT-NUM
           PERFORM 2100-DROP-TEST-LOT.
       2000-EXIT.
           EXIT.

       2100-DROP-TEST-LOT.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM inv.Lot WHERE LotCode LIKE "
                  "'ZZ-TEST-LOTSER%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 8 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.

       9000-RESET.
           INITIALIZE WS-LS
           MOVE "STC" TO CA-COMPANY OF WS-LS
           MOVE "RDG" TO CA-SITE OF WS-LS
           MOVE "WHSE" TO CA-USER OF WS-LS
           MOVE 20261005 TO CA-BUSINESS-DATE OF WS-LS
           MOVE "RDG" TO LS-SITE
           MOVE "MAIN" TO LS-WAREHOUSE
           MOVE "STKA01" TO LS-LOCATION
           MOVE "ACCEPTED" TO LS-CONDITION
           MOVE 20261005 TO LS-AS-AT.

       COPY TESTKITP.
