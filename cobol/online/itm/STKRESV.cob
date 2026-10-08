      *>================================================================
      *> PROGRAM-ID : STKRESV
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *> PURPOSE    : STOCK RESERVATION PRIMITIVES.  A RESERVATION IS
      *>              DEMAND SPECIFIC, QUANTITY BOUNDED AND NOT
      *>              CONSUMPTION.  THE ARITHMETIC THAT DECIDES WHETHER
      *>              THERE IS ENOUGH FREE STOCK RUNS INSIDE
      *>              inv.usp_ReserveStock UNDER UPDLOCK, SO TWO USERS
      *>              CANNOT BOTH TAKE THE LAST UNIT.  THIS PROGRAM
      *>              OWNS THE VALIDATION AND THE MESSAGES.
      *> CALLED BY  : SALES ORDER ENTRY, KIT ISSUE, TRANSFER REQUESTS,
      *>              STKMOVE (CONSUME ON ISSUE), THE .NET CLIENT
      *> INTERFACE  : CALL "STKRESV" USING WS-STKRESV-CA
      *>              COPYBOOK STKRESVCA
      *> CALLS      : SQLIO -> inv.usp_ReserveStock, AVAILQ
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-02-25 RJH  CR0688 ORIGINAL - ALLOCATION OF FREE STOCK
      *> 2008-10-14 KMP  CR2105 OVERRESERVATION UNDER CONCURRENT USERS
      *>                 MOVED INTO THE DATABASE UNDER UPDLOCK
      *> 2026-10-05 STERP ITM-014 REVALIDATION ON MATERIAL CHANGE
      *>================================================================
      *> REQ: ITM-014
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STKRESV.
       AUTHOR. R J HOLLIS.
       DATE-WRITTEN. 25/02/1997.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ITMWORK.
       01  WS-CONTROL.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-ACTION               PIC X(10) VALUE SPACES.
       LINKAGE SECTION.
       COPY STKRESVCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "RESERVE"  PERFORM 1000-RESERVE
               WHEN "RELEASE"  PERFORM 2000-RELEASE
               WHEN "CONSUME"  PERFORM 3000-CONSUME
               WHEN "REVALID"  PERFORM 4000-REVALIDATE
               WHEN "ENQUIRE"  PERFORM 5000-ENQUIRE
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-SQL-FAILED
           MOVE "STKRESV" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "ITM-014" TO CA-REQ-REF
           MOVE 0 TO SR-QTY-RESERVED SR-QTY-SHORTFALL
           MOVE 0 TO SR-QTY-FREE-BEFORE SR-QTY-FREE-AFTER
           MOVE 0 TO SR-RESERVATION-ID SR-INVALID-CNT SR-LINE-CNT
           MOVE SPACES TO SR-STATUS SR-INVALID-REASON
           MOVE FUNCTION UPPER-CASE(SR-ITEM-CODE) TO SR-ITEM-CODE
           IF SR-DEMAND-TYPE = SPACES
               MOVE "OTHER" TO SR-DEMAND-TYPE
           END-IF
           IF SR-FIRMNESS = SPACES
               MOVE "FIRM" TO SR-FIRMNESS
           END-IF
           IF SR-PRIORITY = 0
               MOVE 5 TO SR-PRIORITY
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES OR SR-ITEM-CODE = SPACES
               MOVE "RSV0001" TO CA-REASON-CODE
               MOVE "A company and an item are required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION = "ENQUIRE"
               GO TO 0200-EXIT
           END-IF
           IF SR-DEMAND-REF = SPACES
               MOVE "RSV0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A reservation must name the demand it is "
                      "held for"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF CA-FUNCTION = "RESERVE" OR CA-FUNCTION = "CONSUME"
               IF SR-QTY-REQUESTED <= 0
                   MOVE "RSV0002" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "A quantity greater than zero is required"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   PERFORM 9000-REJECT
               END-IF
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 0500  THE SHARED CALL ON inv.usp_ReserveStock.  WS-ACTION
      *>       SELECTS RESERVE / RELEASE / CONSUME / REVALID.
      *>----------------------------------------------------------------
       0500-CALL-PROC SECTION.
       0500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC inv.usp_ReserveStock ?, ?, ?, ?, ?, ?, ?, ?,"
                  " ?, ?, ?, ?, ?, ?, ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 18 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 18
               MOVE SPACES TO SQLIO-PARM-VAL(WS-JX)
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SR-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE WS-ACTION TO SQLIO-PARM-VAL(3)
           MOVE SR-DEMAND-TYPE TO SQLIO-PARM-VAL(4)
           MOVE SR-DEMAND-REF TO SQLIO-PARM-VAL(5)
           MOVE CA-USER TO SQLIO-PARM-VAL(6)
           IF SR-SITE = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(7)
           ELSE
               MOVE SR-SITE TO SQLIO-PARM-VAL(7)
           END-IF
           IF SR-DEMAND-LINE = 0
               MOVE "Y" TO SQLIO-PARM-NULL(8)
           ELSE
               MOVE SR-DEMAND-LINE TO SQLIO-INT-EDIT
               MOVE FUNCTION TRIM(SQLIO-INT-EDIT)
                   TO SQLIO-PARM-VAL(8)
           END-IF
           MOVE SR-REVISION TO SQLIO-PARM-VAL(9)
           MOVE SR-WAREHOUSE TO SQLIO-PARM-VAL(10)
           MOVE SR-LOCATION TO SQLIO-PARM-VAL(11)
      *>   2026-10-07 STERP CR2603.  A warehouse or location the
      *>   caller did not state is NOT a scope of its own: passed as
      *>   an empty string it made inv.usp_ReserveStock look for
      *>   balances in a location literally called '', so nothing was
      *>   eligible and the reservation was written for zero.  Blank
      *>   now means "anywhere the demand may be met", as the
      *>   procedure's NULL defaults intend.
           IF SR-WAREHOUSE = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(10)
           END-IF
           IF SR-LOCATION = SPACES
               MOVE "Y" TO SQLIO-PARM-NULL(11)
           END-IF
           MOVE SR-LOT-CODE TO SQLIO-PARM-VAL(12)
           MOVE SR-SERIAL-NO TO SQLIO-PARM-VAL(13)
           MOVE SR-QTY-REQUESTED TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(14)
           IF SR-ALLOW-PARTIAL = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(15)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(15)
           END-IF
           MOVE SR-FIRMNESS TO SQLIO-PARM-VAL(16)
           IF SR-REQUIRED-DATE = 0
               MOVE "Y" TO SQLIO-PARM-NULL(17)
           ELSE
               MOVE SR-REQUIRED-DATE TO WS-DATE-N
               MOVE WS-DATE-N TO SQLIO-PARM-VAL(17)
           END-IF
           MOVE SR-PRIORITY TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(18)
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0500-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 0500-EXIT
           END-IF
           COMPUTE SR-RESERVATION-ID = FUNCTION NUMVAL(ITM-COL(2))
           COMPUTE SR-QTY-RESERVED = FUNCTION NUMVAL(ITM-COL(3))
           COMPUTE SR-QTY-SHORTFALL = FUNCTION NUMVAL(ITM-COL(4))
           COMPUTE SR-QTY-FREE-BEFORE = FUNCTION NUMVAL(ITM-COL(5))
           COMPUTE SR-QTY-FREE-AFTER = FUNCTION NUMVAL(ITM-COL(6))
           COMPUTE SR-INVALID-CNT = FUNCTION NUMVAL(ITM-COL(10)).
       0500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-RESERVE SECTION.
       1000-START.
           MOVE "RESERVE" TO WS-ACTION
           PERFORM 0500-CALL-PROC
           IF CA-FAILED
               GO TO 1000-EXIT
           END-IF
           MOVE "ACTIVE" TO SR-STATUS
           MOVE SR-QTY-RESERVED TO WS-QTY-ED
           MOVE SPACES TO WS-TEXT
           STRING "Reserved " FUNCTION TRIM(WS-QTY-ED) " of "
                  FUNCTION TRIM(SR-ITEM-CODE) " for "
                  FUNCTION TRIM(SR-DEMAND-REF)
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE
           IF SR-QTY-SHORTFALL > 0
               MOVE SR-QTY-SHORTFALL TO WS-QTY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(WS-TEXT) "; short by "
                      FUNCTION TRIM(WS-QTY-ED)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 4 TO CA-RETURN-CODE
               MOVE "RSV0003" TO CA-REASON-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       1000-EXIT.
           EXIT.

       2000-RELEASE SECTION.
       2000-START.
           MOVE "RELEASE" TO WS-ACTION
           PERFORM 0500-CALL-PROC
           IF CA-FAILED
               GO TO 2000-EXIT
           END-IF
           MOVE "RELEASED" TO SR-STATUS
           MOVE SPACES TO CA-MESSAGE
           STRING "Reservation of " FUNCTION TRIM(SR-ITEM-CODE)
                  " for " FUNCTION TRIM(SR-DEMAND-REF) " released"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  FINAL USE.  THE RESERVATION IS REVALIDATED FIRST: A
      *>       DEMAND MAY NOT GO ON USING A RESERVATION THAT POINTS AT
      *>       NEWLY BLOCKED, QUARANTINED OR EXPIRED STOCK.
      *>----------------------------------------------------------------
       3000-CONSUME SECTION.
       3000-START.
           MOVE "REVALID" TO WS-ACTION
           PERFORM 0500-CALL-PROC
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           IF SR-INVALID-CNT > 0
               MOVE "RSV0006" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The stock reserved for "
                      FUNCTION TRIM(SR-DEMAND-REF)
                      " is no longer eligible; reallocate before "
                      "issuing"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "INVALID" TO SR-STATUS
               MOVE 6 TO CA-RETURN-CODE
               MOVE "Y" TO WS-ERROR
               ADD 1 TO CA-MSG-COUNT
               GO TO 3000-EXIT
           END-IF
           MOVE "CONSUME" TO WS-ACTION
           PERFORM 0500-CALL-PROC
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           MOVE SR-QTY-REQUESTED TO SR-QTY-CONSUMED
           MOVE "CONSUMED" TO SR-STATUS
           MOVE SR-QTY-REQUESTED TO WS-QTY-ED
           MOVE SPACES TO CA-MESSAGE
           STRING "Consumed " FUNCTION TRIM(WS-QTY-ED) " of "
                  FUNCTION TRIM(SR-ITEM-CODE) " against "
                  FUNCTION TRIM(SR-DEMAND-REF)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       3000-EXIT.
           EXIT.

       4000-REVALIDATE SECTION.
       4000-START.
           MOVE "REVALID" TO WS-ACTION
           PERFORM 0500-CALL-PROC
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           MOVE SR-INVALID-CNT TO WS-INT-ED
           MOVE SPACES TO CA-MESSAGE
           IF SR-INVALID-CNT = 0
               STRING "All reservations for "
                      FUNCTION TRIM(SR-ITEM-CODE)
                      " remain valid"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               STRING FUNCTION TRIM(WS-INT-ED)
                      " reservation(s) were invalidated and an "
                      "exception was raised for the demand owner"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 4 TO CA-RETURN-CODE
               MOVE "RSV0006" TO CA-REASON-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       5000-ENQUIRE SECTION.
       5000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ReservationId, ItemCode, SiteCode, "
                  "ISNULL(LocationCode,''), ISNULL(LotCode,''), "
                  "DemandType, DemandRef, ISNULL(DemandLine,0), "
                  "QtyReserved, QtyPicked, QtyConsumed, Status, "
                  "Firmness, ISNULL(CONVERT(char(8), RequiredDate, "
                  "112),'') FROM inv.StockReservation WHERE "
                  "CompanyCode = ? AND (? = '' OR ItemCode = ?) AND "
                  "(? = '' OR DemandRef = ?) AND (? = '' OR "
                  "SiteCode = ?) ORDER BY Priority, ReservationId"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 7
               MOVE SPACES TO SQLIO-PARM-VAL(WS-JX)
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SR-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE SR-ITEM-CODE TO SQLIO-PARM-VAL(3)
           MOVE SR-DEMAND-REF TO SQLIO-PARM-VAL(4)
           MOVE SR-DEMAND-REF TO SQLIO-PARM-VAL(5)
           MOVE SR-SITE TO SQLIO-PARM-VAL(6)
           MOVE SR-SITE TO SQLIO-PARM-VAL(7)
           MOVE 14 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 5000-EXIT
           END-IF
           MOVE 0 TO SR-LINE-CNT
           MOVE 0 TO WS-QTY
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA ITM-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   IF SR-LINE-CNT < 25
                       ADD 1 TO SR-LINE-CNT
                       COMPUTE SR-L-RES-ID(SR-LINE-CNT) =
                           FUNCTION NUMVAL(ITM-COL(1))
                       MOVE ITM-COL(2) TO SR-L-ITEM(SR-LINE-CNT)
                       MOVE ITM-COL(3) TO SR-L-SITE(SR-LINE-CNT)
                       MOVE ITM-COL(4) TO SR-L-LOCATION(SR-LINE-CNT)
                       MOVE ITM-COL(5) TO SR-L-LOT(SR-LINE-CNT)
                       MOVE ITM-COL(6) TO SR-L-DEM-TYPE(SR-LINE-CNT)
                       MOVE ITM-COL(7) TO SR-L-DEM-REF(SR-LINE-CNT)
                       COMPUTE SR-L-DEM-LINE(SR-LINE-CNT) =
                           FUNCTION NUMVAL(ITM-COL(8))
                       COMPUTE SR-L-QTY(SR-LINE-CNT) =
                           FUNCTION NUMVAL(ITM-COL(9))
                       COMPUTE SR-L-PICKED(SR-LINE-CNT) =
                           FUNCTION NUMVAL(ITM-COL(10))
                       COMPUTE SR-L-CONSUMED(SR-LINE-CNT) =
                           FUNCTION NUMVAL(ITM-COL(11))
                       MOVE ITM-COL(12) TO SR-L-STATUS(SR-LINE-CNT)
                       MOVE ITM-COL(13) TO SR-L-FIRMNESS(SR-LINE-CNT)
                       IF ITM-COL(14) = SPACES
                           MOVE 0 TO SR-L-REQUIRED(SR-LINE-CNT)
                       ELSE
                           COMPUTE SR-L-REQUIRED(SR-LINE-CNT) =
                               FUNCTION NUMVAL(ITM-COL(14))
                       END-IF
                       IF ITM-COL(12) = "ACTIVE"
                           COMPUTE WS-QTY = WS-QTY +
                               FUNCTION NUMVAL(ITM-COL(9)) -
                               FUNCTION NUMVAL(ITM-COL(11))
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           MOVE WS-QTY TO SR-QTY-RESERVED
           IF SR-LINE-CNT = 0
               MOVE 16 TO CA-RETURN-CODE
               MOVE "RSV0005" TO CA-REASON-CODE
               MOVE "No reservations match that enquiry"
                   TO CA-MESSAGE
           ELSE
               MOVE SR-LINE-CNT TO WS-INT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(WS-INT-ED) " reservation(s)"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       5000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "RSV0008" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "STKRESV does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE "Y" TO WS-ERROR WS-SQL-FAILED
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF SQLIO-ERRMSG(1:512) = SPACES
               MOVE "SQL failure in STKRESV" TO CA-MESSAGE
           END-IF
           IF CA-MESSAGE(1:8) = "STERP-E-"
               MOVE "RSV0003" TO CA-REASON-CODE
               MOVE 8 TO CA-RETURN-CODE
           ELSE
               MOVE "SQL0001" TO CA-REASON-CODE
               MOVE 12 TO CA-RETURN-CODE
           END-IF
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
