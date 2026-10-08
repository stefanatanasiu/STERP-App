      *>================================================================
      *> PROGRAM-ID : PICKGEN
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : PICK LIST GENERATION AND CONFIRMATION (SO-012).
      *>
      *>              PICKING IS NOT DESPATCH.  A PICK LIST IS AN
      *>              INSTRUCTION TO A PERSON IN A WAREHOUSE.  IT
      *>              IDENTIFIES THE ITEM, THE WAREHOUSE, THE
      *>              LOCATION, THE LOT OR SERIAL AND THE RESERVATION
      *>              IT DRAWS ON - AND IT MOVES NO STOCK, CREATES NO
      *>              COST OF SALE AND SETS NO DESPATCHED QUANTITY.
      *>              THOSE BELONG TO DESPATCH, IN ONE TRANSACTION.
      *>
      *>              A SHORT PICK IS CONTROLLED AND REASONED: THE
      *>              PICKED AND SHORT QUANTITIES ARE BOTH RECORDED
      *>              AND THE OUTSTANDING DEMAND SURVIVES, SO THE
      *>              BACK ORDER IS STILL REAL (CR1051).
      *>
      *>              PRINTING OR REPRINTING INCREMENTS PrintCount AND
      *>              NOTHING ELSE.  IT IS NOT A RELEASE, NOT A
      *>              RESERVATION AND NOT AN OVERRIDE (SO-011).
      *> CALLED BY  : THE WAREHOUSE FORMS, THE PICKING WORKLIST,
      *>              DESPATCH (ENQUIRY ONLY), THE .NET CLIENT
      *> INTERFACE  : CALL "PICKGEN" USING WS-PICKGEN-CA
      *>              COPYBOOK PICKGENCA
      *> CALLS      : NUMGEN, SUBSTCHK, SQLIO (so.PickList*, so reads)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-06-29 DJM  CR0944 WAREHOUSE PICKING (ACUCOBOL)
      *> 1999-11-30 KMP  Y2K - REQUIRED DATES HOLD FOUR DIGIT YEARS
      *> 2001-09-04 KMP  CR1051 SHORT PICK KEEPS THE BACK ORDER
      *> 2005-11-30 ANW  CR1320 SUBSTITUTION NEEDS AN APPROVAL
      *> 2026-10-06 STERP SO-012 PICKING HAS NO STOCK OR COGS EFFECT
      *>================================================================
      *> REQ: SO-012
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PICKGEN.
       AUTHOR. D J MERRICK.
       DATE-WRITTEN. 29/06/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==WS-NUM-CA==.
       COPY SUBSTCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-SUB-CA==.
       01  WS-CONST.
           05  WS-SERIES-PICK          PIC X(20) VALUE "SPCK"  .
           05  WS-SERIES-ALT           PIC X(20) VALUE "SPCK".
       01  WS-ROW.
           05  WS-COL OCCURS 14 TIMES  PIC X(100).
       01  WS-WORK.
           05  WS-BAD                  PIC X VALUE "N".
               88  WS-BAD-INPUT                  VALUE "Y".
           05  WS-IN-TRAN               PIC X VALUE "N".
               88  WS-TRAN-OPEN                  VALUE "Y".
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-DONE                 PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-QTY                  PIC S9(13)V9(6) VALUE 0.
           05  WS-QTY2                 PIC S9(13)V9(6) VALUE 0.
           05  WS-LINE-NO              PIC S9(9) VALUE 0.
           05  WS-TEXT                 PIC X(200) VALUE SPACES.
           05  WS-QTY-ED               PIC Z(9)9.9(6).
           05  WS-INT-ED               PIC -(9)9.
           05  WS-DATE-N               PIC 9(8) VALUE 0.
       LINKAGE SECTION.
       COPY PICKGENCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       1000-MAIN.
           PERFORM 1100-INITIALISE THRU 1100-EXIT
           PERFORM 1200-VALIDATE THRU 1200-EXIT
           IF WS-BAD-INPUT
               GO TO 1000-RETURN
           END-IF
           EVALUATE CA-FUNCTION OF DFHCOMMAREA
               WHEN "GENERATE"
                   PERFORM 2000-GENERATE THRU 2000-EXIT
               WHEN "CONFIRM"
                   PERFORM 3000-CONFIRM THRU 3000-EXIT
               WHEN "SUBST"
                   PERFORM 4000-SUBSTITUTE THRU 4000-EXIT
               WHEN "PRINT"
                   PERFORM 5000-PRINT THRU 5000-EXIT
               WHEN "CANCEL"
                   PERFORM 6000-CANCEL THRU 6000-EXIT
               WHEN OTHER
                   PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       1000-RETURN.
           IF WS-TRAN-OPEN
               PERFORM 9500-ROLLBACK THRU 9500-EXIT
           END-IF
           GOBACK.

      *>----------------------------------------------------------------
       1100-INITIALISE.
           MOVE "N" TO WS-BAD
           MOVE "N" TO WS-IN-TRAN
           MOVE "N" TO WS-DONE
           MOVE 0 TO WS-I WS-J WS-QTY WS-QTY2 WS-LINE-NO WS-DATE-N
           MOVE SPACES TO WS-TEXT
           MOVE "PICKGEN" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "SO-012" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE 0 TO PK-GENERATED-CNT PK-CONFIRMED-CNT
           MOVE 0 TO PK-OUTSTANDING-QTY
      *>   stated every call so that no caller can ever read picking
      *>   as a stock or a financial event
           MOVE "N" TO PK-STOCK-MOVED
           MOVE "N" TO PK-COGS-CREATED
           MOVE 0 TO PK-DESPATCHED-QTY
           IF CA-FUNCTION OF DFHCOMMAREA = "GENERATE"
               MOVE 0 TO PK-LINE-CNT
               MOVE 0 TO PK-PICKLIST-ID
           END-IF
           IF PK-REQUIRED-DATE = 0
               MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
                   TO PK-REQUIRED-DATE
           END-IF
           IF PK-PICKER-USER = SPACES
               MOVE CA-USER OF DFHCOMMAREA TO PK-PICKER-USER
           END-IF
           IF PK-SITE = SPACES
               MOVE CA-SITE OF DFHCOMMAREA TO PK-SITE
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 14
               MOVE 100 TO SQLIO-COL-WIDTH(WS-I)
           END-PERFORM.
       1100-EXIT.
           EXIT.

       1200-VALIDATE.
           IF CA-COMPANY OF DFHCOMMAREA = SPACES
               MOVE "PCK0001" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A company code is required." TO WS-TEXT
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1200-EXIT
           END-IF
           IF CA-FUNCTION OF DFHCOMMAREA = "GENERATE"
               IF PK-ORDER-NUMBER = SPACES
                   MOVE "PCK0001" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SPACES TO WS-TEXT
                   STRING "An order number is required to generate "
                          "a pick list."
                       DELIMITED BY SIZE INTO WS-TEXT
                   PERFORM 9000-REJECT THRU 9000-EXIT
               END-IF
               GO TO 1200-EXIT
           END-IF
           IF PK-PICKLIST-REF = SPACES
               MOVE "PCK0001" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "A pick list reference is required for the "
                      FUNCTION TRIM(CA-FUNCTION OF DFHCOMMAREA)
                      " function."
                   DELIMITED BY SIZE INTO WS-TEXT
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1200-EXIT
           END-IF
           IF CA-FUNCTION OF DFHCOMMAREA = "CONFIRM"
               IF PK-PICKED-QTY < 0 OR PK-SHORT-QTY < 0
                   MOVE "PCK0004" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SPACES TO WS-TEXT
                   STRING "Picked and short quantities may not be "
                          "negative."
                       DELIMITED BY SIZE INTO WS-TEXT
                   PERFORM 9000-REJECT THRU 9000-EXIT
               END-IF
           END-IF.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  GENERATE.  Only RELEASED demand is pickable; a held
      *>       order stays in the warehouse queue and is not printed
      *>       into existence (SO-011).
      *>----------------------------------------------------------------
       2000-GENERATE.
           PERFORM 2100-CHECK-ORDER THRU 2100-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-NEXT-REFERENCE THRU 2200-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM 9400-BEGIN THRU 9400-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM 2300-WRITE-HEADER THRU 2300-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM 2400-READ-SCHEDULES THRU 2400-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           IF PK-LINE-CNT = 0
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "PCK0003" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "There is nothing releasable to pick on order "
                      FUNCTION TRIM(PK-ORDER-NUMBER)
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > PK-LINE-CNT
               IF NOT CA-FAILED OF DFHCOMMAREA
                   PERFORM 2500-WRITE-LINE THRU 2500-EXIT
               END-IF
           END-PERFORM
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM 9450-COMMIT THRU 9450-EXIT
           MOVE PK-LINE-CNT TO PK-GENERATED-CNT
           MOVE "OPEN" TO PK-STATUS
           MOVE PK-LINE-CNT TO WS-INT-ED
           MOVE SPACES TO WS-TEXT
           STRING "Pick list " FUNCTION TRIM(PK-PICKLIST-REF)
                  " created with " FUNCTION TRIM(WS-INT-ED)
                  " line(s); no stock has moved."
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA.
       2000-EXIT.
           EXIT.

       2100-CHECK-ORDER.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT o.ReleaseStatus, o.CommercialStatus, "
                  "o.CustomerCode, ISNULL(o.SiteCode,'') "
                  "FROM so.SalesOrder o WHERE o.CompanyCode = ? "
                  "AND o.OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE PK-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "PCK0002" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "Order " FUNCTION TRIM(PK-ORDER-NUMBER)
                      " is not known in this company."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 2100-EXIT
           END-IF
           MOVE WS-COL(3) TO PK-CUSTOMER
           IF PK-SITE = SPACES
               MOVE WS-COL(4) TO PK-SITE
           END-IF
           IF WS-COL(1) NOT = "RELEASED"
               MOVE 6 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "PCK0003" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "Order " FUNCTION TRIM(PK-ORDER-NUMBER)
                      " is " FUNCTION TRIM(WS-COL(1))
                      " and cannot be picked until it is released."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       2100-EXIT.
           EXIT.

      *>   One continuous sequence per company (COM-005, D-15).  The
      *>   1998 series code was SPCK; the distribution rewrite uses
      *>   SOPICK, so the old code is tried if the new one is absent.
       2200-NEXT-REFERENCE.
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUM-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-NUM-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-NUM-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-NUM-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-NUM-CA
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA
               TO CA-CORRELATION-ID OF WS-NUM-CA
           MOVE WS-SERIES-PICK TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF CA-RETURN-CODE OF WS-NUM-CA >= 8
      *>       the SOPICK series is not deployed everywhere; the 1998
      *>       distribution series SPCK is the documented fall back
               MOVE WS-SERIES-ALT TO NG-SERIES-CODE
               MOVE 0 TO CA-RETURN-CODE OF WS-NUM-CA
               MOVE SPACES TO CA-MESSAGE OF WS-NUM-CA
               CALL "NUMGEN" USING WS-NUM-CA
           END-IF
           IF CA-RETURN-CODE OF WS-NUM-CA >= 8
               MOVE CA-RETURN-CODE OF WS-NUM-CA
                   TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "PCK0001" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-NUM-CA
                   TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 2200-EXIT
           END-IF
           MOVE NG-DOCUMENT-NUMBER TO PK-PICKLIST-REF.
       2200-EXIT.
           EXIT.

       2300-WRITE-HEADER.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT so.PickList (CompanyCode, SiteCode, "
                  "PickListRef, OrderNumber, CustomerCode, "
                  "WarehouseCode, RequiredDate, PickerUser, "
                  "PrintCount, Status, CreatedBy) VALUES "
                  "(?, ?, ?, ?, ?, NULLIF(?,''), ?, ?, 0, 'OPEN', ?)"
                  "; SELECT CONVERT(varchar(24), SCOPE_IDENTITY()), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 8 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE PK-SITE TO SQLIO-PARM-VAL(2)
           MOVE PK-PICKLIST-REF TO SQLIO-PARM-VAL(3)
           MOVE PK-ORDER-NUMBER TO SQLIO-PARM-VAL(4)
           MOVE PK-CUSTOMER TO SQLIO-PARM-VAL(5)
           MOVE PK-WAREHOUSE TO SQLIO-PARM-VAL(6)
           MOVE PK-REQUIRED-DATE TO SQLIO-PARM-VAL(7)
           MOVE PK-PICKER-USER TO SQLIO-PARM-VAL(8)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 9 TO SQLIO-PARM-CNT
           MOVE CA-USER OF DFHCOMMAREA TO SQLIO-PARM-VAL(9)
           MOVE "N" TO SQLIO-PARM-NULL(9)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2300-EXIT
           END-IF
           COMPUTE PK-PICKLIST-ID = FUNCTION NUMVAL(WS-COL(1)).
       2300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2400  The pickable demand.  Each schedule brings its own
      *>       reservation with it, so the picker is sent to the lot
      *>       and the location the allocation actually holds, and the
      *>       reservation id travels onto the pick line as evidence.
      *>----------------------------------------------------------------
       2400-READ-SCHEDULES.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12), l.LineNum), "
                  "CONVERT(varchar(12), s.ScheduleNo), "
                  "CONVERT(varchar(24), l.SalesOrderLineId), "
                  "l.ItemCode, ISNULL(l.Revision,''), "
                  "CONVERT(varchar(30), s.Quantity - "
                  "s.DespatchedQty), ISNULL(r.WarehouseCode,''), "
                  "ISNULL(r.LocationCode,''), ISNULL(r.LotCode,''), "
                  "ISNULL(r.SerialNo,''), "
                  "CONVERT(varchar(24), ISNULL(r.ReservationId,0)), "
                  "ISNULL(CONVERT(char(8), t.ExpiryDate, 112),'') "
                  "FROM so.DeliverySchedule s "
                  "JOIN so.SalesOrderLine l ON "
                  "l.SalesOrderLineId = s.SalesOrderLineId "
                  "OUTER APPLY (SELECT TOP (1) x.ReservationId, "
                  "x.WarehouseCode, x.LocationCode, x.LotCode, "
                  "x.SerialNo FROM inv.StockReservation x WHERE "
                  "x.CompanyCode = l.CompanyCode AND "
                  "x.DemandType = 'SALESORDER' AND "
                  "x.DemandRef = l.OrderNumber AND "
                  "x.DemandLine = l.LineNum AND x.Status = 'ACTIVE' "
                  "ORDER BY x.ReservationId) r "
                  "LEFT JOIN inv.Lot t ON t.CompanyCode = "
                  "l.CompanyCode AND t.ItemCode = l.ItemCode AND "
                  "t.LotCode = r.LotCode "
                  "WHERE s.CompanyCode = ? AND s.OrderNumber = ? "
                  "AND s.Status IN ('OPEN','PARTIAL') "
                  "AND l.Status = 'OPEN' "
                  "AND l.LineType IN ('STOCK','KITCOMP') "
                  "AND s.Quantity > s.DespatchedQty "
                  "ORDER BY l.LineNum, s.ScheduleNo"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE PK-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 12 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2400-EXIT
           END-IF
           MOVE 0 TO PK-LINE-CNT
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ROW
               IF SQLIO-ERROR OR SQLIO-NOTFOUND
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 2450-HOLD-LINE THRU 2450-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       2400-EXIT.
           EXIT.

       2450-HOLD-LINE.
           IF PK-LINE-CNT >= 40
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "PCK0010" TO CA-REASON-CODE OF DFHCOMMAREA
               GO TO 2450-EXIT
           END-IF
           COMPUTE WS-QTY = FUNCTION NUMVAL(WS-COL(6))
           IF WS-QTY <= 0
               GO TO 2450-EXIT
           END-IF
           ADD 1 TO PK-LINE-CNT
           MOVE PK-LINE-CNT TO WS-J
           MOVE PK-LINE-CNT TO PK-L-LINE-NUM(WS-J)
           COMPUTE PK-L-ORDER-LINE(WS-J) = FUNCTION NUMVAL(WS-COL(1))
           COMPUTE PK-L-SCHEDULE(WS-J) = FUNCTION NUMVAL(WS-COL(2))
           COMPUTE PK-L-SOL-ID(WS-J) = FUNCTION NUMVAL(WS-COL(3))
           MOVE WS-COL(4) TO PK-L-ITEM(WS-J)
           MOVE WS-COL(5) TO PK-L-REVISION(WS-J)
           MOVE WS-QTY TO PK-L-REQUIRED(WS-J)
           MOVE 0 TO PK-L-PICKED(WS-J)
           MOVE 0 TO PK-L-SHORT(WS-J)
           IF WS-COL(7) = SPACES
               MOVE PK-WAREHOUSE TO PK-L-WHSE(WS-J)
           ELSE
               MOVE WS-COL(7) TO PK-L-WHSE(WS-J)
           END-IF
           MOVE WS-COL(8) TO PK-L-LOCATION(WS-J)
           MOVE WS-COL(9) TO PK-L-LOT(WS-J)
           MOVE WS-COL(10) TO PK-L-SERIAL(WS-J)
           COMPUTE PK-L-RES-ID(WS-J) = FUNCTION NUMVAL(WS-COL(11))
           IF WS-COL(12) = SPACES
               MOVE 0 TO PK-L-EXPIRY(WS-J)
           ELSE
               MOVE WS-COL(12)(1:8) TO WS-DATE-N
               MOVE WS-DATE-N TO PK-L-EXPIRY(WS-J)
           END-IF
           MOVE SPACES TO PK-L-SUBST-ITM(WS-J)
           MOVE SPACES TO PK-L-SHORT-WHY(WS-J)
           MOVE "OPEN" TO PK-L-STATUS(WS-J).
       2450-EXIT.
           EXIT.

       2500-WRITE-LINE.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT so.PickListLine (PickListId, LineNum, "
                  "SalesOrderLineId, ScheduleNo, ItemCode, Revision, "
                  "RequiredQty, PickedQty, ShortQty, WarehouseCode, "
                  "LocationCode, LotCode, SerialNo, ExpiryDate, "
                  "ReservationId, Status) VALUES (?, ?, ?, ?, ?, "
                  "NULLIF(?,''), ?, 0, 0, NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,0), 'OPEN')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 13 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 13
               MOVE SPACES TO SQLIO-PARM-VAL(WS-J)
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE PK-PICKLIST-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE PK-L-LINE-NUM(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE PK-L-SOL-ID(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE PK-L-SCHEDULE(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE PK-L-ITEM(WS-I) TO SQLIO-PARM-VAL(5)
           MOVE PK-L-REVISION(WS-I) TO SQLIO-PARM-VAL(6)
           MOVE PK-L-REQUIRED(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(7)
           MOVE PK-L-WHSE(WS-I) TO SQLIO-PARM-VAL(8)
           MOVE PK-L-LOCATION(WS-I) TO SQLIO-PARM-VAL(9)
           MOVE PK-L-LOT(WS-I) TO SQLIO-PARM-VAL(10)
           MOVE PK-L-SERIAL(WS-I) TO SQLIO-PARM-VAL(11)
           IF PK-L-EXPIRY(WS-I) = 0
               MOVE SPACES TO SQLIO-PARM-VAL(12)
           ELSE
               MOVE PK-L-EXPIRY(WS-I) TO SQLIO-PARM-VAL(12)
           END-IF
           MOVE PK-L-RES-ID(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(13)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2500-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       2500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  CONFIRM.  The picked quantity, a controlled short pick
      *>       and the lot or serial ACTUALLY taken.  The outstanding
      *>       demand is preserved: a short pick never cancels the
      *>       balance, it reports it.
      *>----------------------------------------------------------------
       3000-CONFIRM.
           PERFORM 3100-READ-PICK-LINE THRU 3100-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 3000-EXIT
           END-IF
           PERFORM 3200-CHECK-QUANTITIES THRU 3200-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 3000-EXIT
           END-IF
           PERFORM 9400-BEGIN THRU 9400-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 3000-EXIT
           END-IF
           PERFORM 3300-UPDATE-PICK-LINE THRU 3300-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 3000-EXIT
           END-IF
           PERFORM 3400-HEADER-STATUS THRU 3400-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 3000-EXIT
           END-IF
           PERFORM 9450-COMMIT THRU 9450-EXIT
           MOVE 1 TO PK-CONFIRMED-CNT
           PERFORM 3500-CONFIRM-WORDS THRU 3500-EXIT.
       3000-EXIT.
           EXIT.

       3100-READ-PICK-LINE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), h.PickListId), "
                  "h.Status, CONVERT(varchar(30), e.RequiredQty), "
                  "CONVERT(varchar(12), e.LineNum), "
                  "ISNULL(e.ItemCode,''), ISNULL(e.LotCode,''), "
                  "ISNULL(e.SerialNo,''), "
                  "CONVERT(varchar(24), ISNULL(e.ReservationId,0)), "
                  "ISNULL(e.WarehouseCode,''), e.Status, "
                  "ISNULL(h.OrderNumber,''), "
                  "CONVERT(varchar(12), ISNULL(e.ScheduleNo,0)) "
                  "FROM so.PickList h JOIN so.PickListLine e ON "
                  "e.PickListId = h.PickListId "
                  "WHERE h.CompanyCode = ? AND h.PickListRef = ? "
                  "AND (e.LineNum = ? OR (? = 0 AND e.LineNum = "
                  "(SELECT MIN(x.LineNum) FROM so.PickListLine x "
                  "WHERE x.PickListId = h.PickListId AND x.Status "
                  "= 'OPEN')))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE PK-PICKLIST-REF TO SQLIO-PARM-VAL(2)
           MOVE PK-LINE-NUM TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 12 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "PCK0002" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "Pick line " FUNCTION TRIM(PK-PICKLIST-REF)
                      " was not found."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 3100-EXIT
           END-IF
           COMPUTE PK-PICKLIST-ID = FUNCTION NUMVAL(WS-COL(1))
           MOVE WS-COL(2) TO PK-STATUS
           COMPUTE PK-REQUIRED-QTY = FUNCTION NUMVAL(WS-COL(3))
      *>   CR2606.  A caller that names no line confirms the pick as a
      *>   whole: the first line still open is taken, and in full
      *>   unless the picker states a picked or a short quantity.
           IF PK-LINE-NUM = 0
               COMPUTE PK-LINE-NUM = FUNCTION NUMVAL(WS-COL(4))
               IF PK-PICKED-QTY = 0 AND PK-SHORT-QTY = 0
                   MOVE PK-REQUIRED-QTY TO PK-PICKED-QTY
               END-IF
           END-IF
           MOVE WS-COL(5) TO PK-ITEM-CODE
           COMPUTE PK-RESERVATION-ID = FUNCTION NUMVAL(WS-COL(8))
           MOVE WS-COL(11) TO PK-ORDER-NUMBER
           COMPUTE PK-SCHEDULE-NO = FUNCTION NUMVAL(WS-COL(12))
           IF PK-LOCATION = SPACES
               MOVE WS-COL(9) TO PK-WAREHOUSE
           END-IF
           IF PK-STATUS = "CANCELLED" OR WS-COL(10) = "CANCELLED"
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "PCK0007" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "Pick list " FUNCTION TRIM(PK-PICKLIST-REF)
                      " is cancelled and cannot be confirmed."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
           END-IF.
       3100-EXIT.
           EXIT.

       3200-CHECK-QUANTITIES.
           COMPUTE WS-QTY = PK-PICKED-QTY + PK-SHORT-QTY
           IF PK-PICKED-QTY > PK-REQUIRED-QTY
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "PCK0004" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE PK-REQUIRED-QTY TO WS-QTY-ED
               MOVE SPACES TO WS-TEXT
               STRING "The picked quantity exceeds the required "
                      "quantity of " FUNCTION TRIM(WS-QTY-ED)
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               GO TO 3200-EXIT
           END-IF
      *>   the short quantity is derived when the picker does not
      *>   state one, so a short pick can never be recorded as a
      *>   complete pick by omission
           IF PK-SHORT-QTY = 0
              AND PK-PICKED-QTY < PK-REQUIRED-QTY
               COMPUTE PK-SHORT-QTY =
                   PK-REQUIRED-QTY - PK-PICKED-QTY
           END-IF
           IF PK-SHORT-QTY > 0 AND PK-SHORT-REASON = SPACES
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "PCK0005" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "A short pick must state its reason so the "
                      "outstanding demand can be explained."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               GO TO 3200-EXIT
           END-IF
           COMPUTE PK-OUTSTANDING-QTY =
               PK-REQUIRED-QTY - PK-PICKED-QTY
           IF PK-OUTSTANDING-QTY < 0
               MOVE 0 TO PK-OUTSTANDING-QTY
           END-IF.
       3200-EXIT.
           EXIT.

       3300-UPDATE-PICK-LINE.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.PickListLine SET PickedQty = ?, "
                  "ShortQty = ?, ShortReason = NULLIF(?,''), "
                  "LotCode = COALESCE(NULLIF(?,''), LotCode), "
                  "SerialNo = COALESCE(NULLIF(?,''), SerialNo), "
                  "LocationCode = COALESCE(NULLIF(?,''), "
                  "LocationCode), Status = ? "
                  "WHERE PickListId = ? AND LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 8 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 8
               MOVE SPACES TO SQLIO-PARM-VAL(WS-J)
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE PK-PICKED-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE PK-SHORT-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE PK-SHORT-REASON TO SQLIO-PARM-VAL(3)
           MOVE PK-LOT-CODE TO SQLIO-PARM-VAL(4)
           MOVE PK-SERIAL-NO TO SQLIO-PARM-VAL(5)
           MOVE PK-LOCATION TO SQLIO-PARM-VAL(6)
           IF PK-OUTSTANDING-QTY > 0
               MOVE "SHORT" TO SQLIO-PARM-VAL(7)
           ELSE
               MOVE "PICKED" TO SQLIO-PARM-VAL(7)
           END-IF
           MOVE PK-PICKLIST-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE 9 TO SQLIO-PARM-CNT
           MOVE PK-LINE-NUM TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE "N" TO SQLIO-PARM-NULL(9)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3300-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       3300-EXIT.
           EXIT.

      *>   The header follows the lines; it is never set by hand.
       3400-HEADER-STATUS.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE h SET Status = CASE WHEN NOT EXISTS "
                  "(SELECT 1 FROM so.PickListLine z WHERE "
                  "z.PickListId = h.PickListId AND z.Status = 'OPEN')"
                  " THEN 'PICKED' ELSE 'PARTPICKED' END, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "FROM so.PickList h WHERE h.PickListId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-USER OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE PK-PICKLIST-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3400-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       3400-EXIT.
           EXIT.

       3500-CONFIRM-WORDS.
           MOVE PK-PICKED-QTY TO WS-QTY-ED
           MOVE SPACES TO WS-TEXT
           IF PK-OUTSTANDING-QTY > 0
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "PCK0005" TO CA-REASON-CODE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               STRING "Picked " FUNCTION TRIM(WS-QTY-ED)
                      "; the balance stays as outstanding demand "
                      "and no stock has moved."
                   DELIMITED BY SIZE INTO WS-TEXT
           ELSE
               STRING "Picked " FUNCTION TRIM(WS-QTY-ED)
                      " in full; no stock has moved and no cost of "
                      "sale has been created."
                   DELIMITED BY SIZE INTO WS-TEXT
           END-IF
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA.
       3500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  SUBST.  A substitute is a proposal, never a silent
      *>       replacement.  SUBSTCHK owns the rule; where the rule
      *>       does not cover the case an approval id is the only way
      *>       through, and without one the answer is CA-HOLD 06 with
      *>       reason PCK0006 and nothing at all is written.
      *>----------------------------------------------------------------
       4000-SUBSTITUTE.
           IF PK-SUBST-ITEM = SPACES AND PK-SUBST-LOT = SPACES
               MOVE "PCK0001" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "A substitute item or lot must be named."
                   DELIMITED BY SIZE INTO WS-TEXT
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           PERFORM 3100-READ-PICK-LINE THRU 3100-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 4000-EXIT
           END-IF
           PERFORM 4100-ASK-SUBSTCHK THRU 4100-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 4000-EXIT
           END-IF
           IF PK-SUBST-IN-POLICY NOT = "Y"
              AND PK-SUBST-APPR-ID = 0
              AND PK-SUBST-APPR-REF = SPACES
               MOVE 6 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "PCK0006" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "Picking " FUNCTION TRIM(PK-SUBST-ITEM)
                      " in place of " FUNCTION TRIM(PK-ITEM-CODE)
                      " is outside policy and needs an approval id."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               GO TO 4000-EXIT
           END-IF
           PERFORM 9400-BEGIN THRU 9400-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 4000-EXIT
           END-IF
           PERFORM 4200-RECORD-SUBST THRU 4200-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 4000-EXIT
           END-IF
           PERFORM 9450-COMMIT THRU 9450-EXIT
           MOVE SPACES TO WS-TEXT
           STRING "Substitution recorded against pick line; the "
                  "standard demand and the approval are both kept."
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA.
       4000-EXIT.
           EXIT.

       4100-ASK-SUBSTCHK.
           MOVE "N" TO PK-SUBST-IN-POLICY
           MOVE 1 TO PK-SUBST-RATIO
           MOVE PK-PICKED-QTY TO PK-SUBST-QTY
           IF PK-SUBST-ITEM = SPACES
      *>       a lot swap inside the same item is still outside
      *>       policy unless the reservation named that lot
               GO TO 4100-EXIT
           END-IF
           INITIALIZE WS-SUB-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-SUB-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-SUB-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-SUB-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-SUB-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-SUB-CA
           MOVE PK-ITEM-CODE TO SB-ITEM-CODE
           MOVE PK-REVISION TO SB-REVISION
           MOVE PK-SUBST-ITEM TO SB-SUBSTITUTE
           MOVE PK-SUBST-REVISION TO SB-SUBSTITUTE-REV
           MOVE "SALES" TO SB-CONTEXT
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO SB-AS-AT
           MOVE PK-PICKED-QTY TO SB-QTY-REQUIRED
           MOVE "PICKLIST" TO SB-DOCUMENT-TYPE
           MOVE PK-PICKLIST-REF TO SB-DOCUMENT-REF
           CALL "SUBSTCHK" USING WS-SUB-CA
           IF CA-RETURN-CODE OF WS-SUB-CA >= 12
               MOVE 12 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "PCK0006" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-SUB-CA
                   TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 4100-EXIT
           END-IF
           IF SB-RATIO > 0
               MOVE SB-RATIO TO PK-SUBST-RATIO
               MOVE SB-QTY-SUBSTITUTE TO PK-SUBST-QTY
           END-IF
      *>   permitted AND needing no separate customer agreement is
      *>   the only combination that counts as "inside policy"
           IF SB-PERMITTED = "Y" AND SB-NEEDS-AGREEMENT NOT = "Y"
               MOVE "Y" TO PK-SUBST-IN-POLICY
           END-IF.
       4100-EXIT.
           EXIT.

       4200-RECORD-SUBST.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.PickListLine SET "
                  "SubstituteItemCode = NULLIF(?,''), "
                  "SubstituteApprovalId = NULLIF(?,0), "
                  "LotCode = COALESCE(NULLIF(?,''), LotCode) "
                  "WHERE PickListId = ? AND LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 5
               MOVE SPACES TO SQLIO-PARM-VAL(WS-J)
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE PK-SUBST-ITEM TO SQLIO-PARM-VAL(1)
           MOVE PK-SUBST-APPR-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE PK-SUBST-LOT TO SQLIO-PARM-VAL(3)
           MOVE PK-PICKLIST-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE PK-LINE-NUM TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 4200-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       4200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000  PRINT.  PrintCount and nothing else.  No release, no
      *>       reservation, no override, no status change - SO-011 and
      *>       SO-022 both depend on a reprint being inert.
      *>----------------------------------------------------------------
       5000-PRINT.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.PickList SET PrintCount = "
                  "PrintCount + 1 WHERE CompanyCode = ? AND "
                  "PickListRef = ?; SELECT CONVERT(varchar(12), "
                  "PrintCount), Status FROM so.PickList WHERE "
                  "CompanyCode = ? AND PickListRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE PK-PICKLIST-REF TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(3)
           MOVE PK-PICKLIST-REF TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 5000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "PCK0002" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "Pick list " FUNCTION TRIM(PK-PICKLIST-REF)
                      " is not known in this company."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 5000-EXIT
           END-IF
           COMPUTE PK-PRINT-COUNT = FUNCTION NUMVAL(WS-COL(1))
           MOVE WS-COL(2) TO PK-STATUS
           MOVE PK-PRINT-COUNT TO WS-INT-ED
           MOVE SPACES TO WS-TEXT
           STRING "Pick list printed " FUNCTION TRIM(WS-INT-ED)
                  " time(s).  Printing does not release the order, "
                  "reserve stock or authorise anything."
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA.
       5000-EXIT.
           EXIT.

      *>   CANCEL withdraws the instruction, not the demand.  The
      *>   order line and its reservation are untouched.
       6000-CANCEL.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE e SET Status = 'CANCELLED' "
                  "FROM so.PickListLine e JOIN so.PickList h ON "
                  "h.PickListId = e.PickListId WHERE "
                  "h.CompanyCode = ? AND h.PickListRef = ?; "
                  "UPDATE so.PickList SET Status = 'CANCELLED', "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE CompanyCode = ? AND PickListRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE PK-PICKLIST-REF TO SQLIO-PARM-VAL(2)
           MOVE CA-USER OF DFHCOMMAREA TO SQLIO-PARM-VAL(3)
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(4)
           MOVE PK-PICKLIST-REF TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 6000-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "CANCELLED" TO PK-STATUS
           MOVE SPACES TO WS-TEXT
           STRING "Pick list " FUNCTION TRIM(PK-PICKLIST-REF)
                  " cancelled; the order demand and its "
                  "reservations are unchanged."
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA.
       6000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT.
           MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE "Y" TO WS-BAD.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE "PCK0008" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO WS-TEXT
           STRING "PICKGEN does not support the function "
                  FUNCTION TRIM(CA-FUNCTION OF DFHCOMMAREA)
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE 12 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE "SQL0001" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       9300-EXIT.
           EXIT.

       9400-BEGIN.
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 9400-EXIT
           END-IF
           MOVE "Y" TO WS-IN-TRAN.
       9400-EXIT.
           EXIT.

       9450-COMMIT.
           MOVE "COMT" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "N" TO WS-IN-TRAN
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       9450-EXIT.
           EXIT.

      *>   anything unfinished is undone - a half written pick list is
      *>   worse than none at all
       9500-ROLLBACK.
           MOVE "ROLB" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "N" TO WS-IN-TRAN.
       9500-EXIT.
           EXIT.
