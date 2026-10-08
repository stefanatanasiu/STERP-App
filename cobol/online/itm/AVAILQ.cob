      *>================================================================
      *> PROGRAM-ID : AVAILQ
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *> PURPOSE    : STOCK ENQUIRY AND AVAILABILITY.  SEPARATES THE
      *>              PHYSICAL, ELIGIBLE, RESERVED, FREE, EXPECTED AND
      *>              PLANNED QUANTITIES EXACTLY AS SECTION 6 OF THE
      *>              SPECIFICATION DEFINES THEM.  NO STATE IS COUNTED
      *>              TWICE: A BUCKET IS OWNED OR THIRD PARTY, AND IT
      *>              IS ACCEPTED, QUARANTINED, UNDER INSPECTION,
      *>              REJECTED, EXPIRED, IN TRANSIT OR IN A LOCATION
      *>              THAT IS NOT AVAILABLE - NEVER TWO OF THEM.
      *>
      *>              WORKED EXAMPLE (SPECIFICATION SECTION 6)
      *>              OWNED ON HAND 120, QUARANTINE 15, EXPIRED 5,
      *>              OTHER RESERVATIONS 35 -> ELIGIBLE 100, FREE 65.
      *>              A PURCHASE RECEIPT OF 40 NEXT WEEK IS REPORTED
      *>              SEPARATELY AND TODAY'S FREE STOCK IS STILL 65.
      *> CALLED BY  : SALES ORDER ENTRY, KIT ISSUE, STKRESV, STKMOVE,
      *>              THE STOCK ENQUIRY SCREEN, THE BATCH SUITE
      *> INTERFACE  : CALL "AVAILQ" USING WS-AVAILQ-CA
      *>              COPYBOOK AVAILQCA
      *> CALLS      : SQLIO (inv.vw_StockAvailability,
      *>              inv.StockReservation, inv.ItemSite,
      *>              po.PurchaseOrderLine, kit.ExecutionKit)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-06-03 RJH  CR0431 ORIGINAL - FREE STOCK ENQUIRY
      *> 1998-01-26 RJH  CR0801 SAFETY STOCK SHOWN AS A POLICY VIEW,
      *>                 NOT DEDUCTED FROM PHYSICAL FREE STOCK
      *> 2001-03-19 KMP  CR1022 THIRD PARTY OWNED MATERIAL SEPARATED
      *> 2026-10-05 STERP ITM-012, ITM-013 DEFINITIONS AND BUCKETS
      *>================================================================
      *> REQ: ITM-012, ITM-013
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AVAILQ.
       AUTHOR. R J HOLLIS.
       DATE-WRITTEN. 03/06/1996.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ITMWORK.
       COPY ITMDATEW.
       01  WS-CONTROL.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-BUCKET-QTY           PIC S9(13)V9(6) VALUE 0.
           05  WS-TRANSIT-QTY          PIC S9(13)V9(6) VALUE 0.
           05  WS-EXPIRY               PIC 9(8) VALUE 0.
           05  WS-MIN-SHELF            PIC S9(5) VALUE 0.
           05  WS-LIMIT-DATE           PIC 9(8) VALUE 0.
           05  WS-OWNER-TYPE           PIC X(10) VALUE SPACES.
           05  WS-CONDITION            PIC X(12) VALUE SPACES.
           05  WS-LOC-TYPE             PIC X(12) VALUE SPACES.
           05  WS-LOC-AVAIL            PIC X VALUE "N".
           05  WS-LOC-OWNED            PIC X VALUE "N".
           05  WS-LIFECYCLE            PIC X(10) VALUE SPACES.
           05  WS-ELIGIBLE             PIC X VALUE "N".
           05  WS-WHY                  PIC X(30) VALUE SPACES.
           05  WS-OVERFLOW             PIC X VALUE "N".
       LINKAGE SECTION.
       COPY AVAILQCA.
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
               WHEN "AVAIL"    PERFORM 1000-AVAILABILITY
               WHEN "ENQUIRE"  PERFORM 1000-AVAILABILITY
               WHEN "BYLOC"    PERFORM 1000-AVAILABILITY
               WHEN "LOTS"     PERFORM 1000-AVAILABILITY
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-OVERFLOW WS-SQL-FAILED
           MOVE "AVAILQ" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "ITM-013" TO CA-REQ-REF
           MOVE 0 TO AV-QTY-PHYSICAL AV-QTY-ELIGIBLE AV-QTY-RESERVED
           MOVE 0 TO AV-QTY-FREE AV-QTY-EXPECTED AV-QTY-PLANNED
           MOVE 0 TO AV-QTY-QUARANTINE AV-QTY-INSPECTION
           MOVE 0 TO AV-QTY-REJECTED AV-QTY-EXPIRED AV-QTY-INTRANSIT
           MOVE 0 TO AV-QTY-THIRDPARTY AV-QTY-UNAVAIL-LOC
           MOVE 0 TO AV-QTY-OWN-DEMAND AV-QTY-FOR-DEMAND
           MOVE 0 TO AV-QTY-SAFETY AV-QTY-POLICY AV-EXPECTED-DATE
           MOVE 0 TO AV-LINE-CNT
           MOVE "Y" TO AV-DATA-COMPLETE
           MOVE "N" TO AV-RESTRICTED
           MOVE SPACES TO AV-STOCK-UOM
           MOVE FUNCTION UPPER-CASE(AV-ITEM-CODE) TO AV-ITEM-CODE
           IF AV-OWNER-TYPE = SPACES
               MOVE "OWNED" TO AV-OWNER-TYPE
           END-IF
           IF AV-AS-AT = 0
               MOVE CA-BUSINESS-DATE TO AV-AS-AT
           END-IF
           IF AV-AS-AT = 0
               MOVE "TODAY" TO ITD-FUNC
               CALL "ITMDATE" USING ITD-BLOCK
               MOVE ITD-DATE-R TO AV-AS-AT
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES OR AV-ITEM-CODE = SPACES
               MOVE "AVL0001" TO CA-REASON-CODE
               MOVE "A company and an item are required for an "
                   TO WS-TEXT
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(WS-TEXT) " availability enquiry"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           MOVE "VALIDATE" TO ITD-FUNC
           MOVE AV-AS-AT TO ITD-DATE-1
           CALL "ITMDATE" USING ITD-BLOCK
           IF ITD-RC > 4
               MOVE "AVL0005" TO CA-REASON-CODE
               MOVE "The as-at date is not a valid date"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-AVAILABILITY SECTION.
       1000-START.
           PERFORM 1100-READ-BUCKETS
           IF CA-FAILED
               GO TO 1000-EXIT
           END-IF
           PERFORM 1500-READ-RESERVATIONS
           IF CA-FAILED
               GO TO 1000-EXIT
           END-IF
           PERFORM 1600-READ-PLANNING
           PERFORM 1700-READ-EXPECTED
           PERFORM 1800-READ-PLANNED
           PERFORM 1900-COMPUTE
           IF WS-OVERFLOW = "Y"
               MOVE 4 TO CA-RETURN-CODE
               MOVE "AVL0003" TO CA-REASON-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1100  ONE PASS OVER THE PHYSICAL BUCKETS.  EACH BUCKET FALLS
      *>       INTO EXACTLY ONE CLASSIFICATION (1200).
      *>----------------------------------------------------------------
       1100-READ-BUCKETS SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT SiteCode, WarehouseCode, LocationCode, "
                  "LocationType, Revision, LotCode, SerialNo, "
                  "OwnerType, OwnerCode, QualityStatus, QtyOnHand, "
                  "QtyInTransit, ISNULL(CONVERT(char(8), ExpiryDate,"
                  " 112),''), IsAvailable, IsOwnedStock, "
                  "StockUomCode, LifecycleStatus, "
                  "ISNULL(MinRemainingShelfLifeDays,0) FROM "
                  "inv.vw_StockAvailability WHERE CompanyCode = ? "
                  "AND ItemCode = ? AND (? = '' OR SiteCode = ?) AND"
                  " (? = '' OR WarehouseCode = ?) AND (? = '' OR "
                  "LocationCode = ?) AND (? = '' OR Revision = ?) "
                  "AND (? = '' OR LotCode = ?) AND (? = '' OR "
                  "SerialNo = ?) AND (? = '' OR QualityStatus = ?) "
                  "ORDER BY SiteCode, WarehouseCode, LocationCode, "
                  "LotCode, SerialNo"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 16 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 16
               MOVE SPACES TO SQLIO-PARM-VAL(WS-JX)
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE AV-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE AV-SITE TO SQLIO-PARM-VAL(3)
           MOVE AV-SITE TO SQLIO-PARM-VAL(4)
           MOVE AV-WAREHOUSE TO SQLIO-PARM-VAL(5)
           MOVE AV-WAREHOUSE TO SQLIO-PARM-VAL(6)
           MOVE AV-LOCATION TO SQLIO-PARM-VAL(7)
           MOVE AV-LOCATION TO SQLIO-PARM-VAL(8)
           MOVE AV-REVISION TO SQLIO-PARM-VAL(9)
           MOVE AV-REVISION TO SQLIO-PARM-VAL(10)
           MOVE AV-LOT-CODE TO SQLIO-PARM-VAL(11)
           MOVE AV-LOT-CODE TO SQLIO-PARM-VAL(12)
           MOVE AV-SERIAL-NO TO SQLIO-PARM-VAL(13)
           MOVE AV-SERIAL-NO TO SQLIO-PARM-VAL(14)
           MOVE AV-CONDITION TO SQLIO-PARM-VAL(15)
           MOVE AV-CONDITION TO SQLIO-PARM-VAL(16)
           MOVE 18 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1100-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA ITM-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 1200-CLASSIFY-BUCKET
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW.
       1100-EXIT.
           EXIT.

       1200-CLASSIFY-BUCKET SECTION.
       1200-START.
           COMPUTE WS-BUCKET-QTY = FUNCTION NUMVAL(ITM-COL(11))
           COMPUTE WS-TRANSIT-QTY = FUNCTION NUMVAL(ITM-COL(12))
           MOVE ITM-COL(8) TO WS-OWNER-TYPE
           MOVE ITM-COL(10) TO WS-CONDITION
           MOVE ITM-COL(4) TO WS-LOC-TYPE
           MOVE ITM-COL(14)(1:1) TO WS-LOC-AVAIL
           MOVE ITM-COL(15)(1:1) TO WS-LOC-OWNED
           MOVE ITM-COL(17) TO WS-LIFECYCLE
           MOVE ITM-COL(16) TO AV-STOCK-UOM
           COMPUTE WS-MIN-SHELF = FUNCTION NUMVAL(ITM-COL(18))
           MOVE 0 TO WS-EXPIRY
           IF ITM-COL(13) NOT = SPACES
               COMPUTE WS-EXPIRY = FUNCTION NUMVAL(ITM-COL(13))
           END-IF
           MOVE "N" TO WS-ELIGIBLE
           MOVE SPACES TO WS-WHY
           PERFORM 1300-ELIGIBILITY
           PERFORM 1400-ACCUMULATE
           PERFORM 1450-ADD-LINE.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1300  THE ELIGIBILITY LADDER.  THE FIRST REASON THAT APPLIES
      *>       IS THE REASON REPORTED, SO A QUANTITY NEVER APPEARS IN
      *>       TWO GROUPS.
      *>----------------------------------------------------------------
       1300-ELIGIBILITY SECTION.
       1300-START.
           IF WS-OWNER-TYPE NOT = "OWNED"
               MOVE "Third party owned" TO WS-WHY
               GO TO 1300-EXIT
           END-IF
           IF WS-LOC-OWNED NOT = "1"
               MOVE "Location holds external stock" TO WS-WHY
               GO TO 1300-EXIT
           END-IF
           IF WS-LOC-TYPE = "TRANSIT"
               MOVE "In transit" TO WS-WHY
               GO TO 1300-EXIT
           END-IF
           IF WS-CONDITION = "QUARANTINE"
               MOVE "Quarantined" TO WS-WHY
               GO TO 1300-EXIT
           END-IF
           IF WS-CONDITION = "INSPECTION"
               MOVE "Awaiting inspection" TO WS-WHY
               GO TO 1300-EXIT
           END-IF
           IF WS-CONDITION = "REJECTED"
               MOVE "Rejected" TO WS-WHY
               GO TO 1300-EXIT
           END-IF
           IF WS-CONDITION = "EXPIRED"
               MOVE "Expired" TO WS-WHY
               GO TO 1300-EXIT
           END-IF
           IF WS-EXPIRY > 0 AND WS-EXPIRY < AV-AS-AT
               MOVE "Expired" TO WS-WHY
               GO TO 1300-EXIT
           END-IF
           IF WS-EXPIRY > 0 AND WS-MIN-SHELF > 0
               MOVE "ADDDAYS" TO ITD-FUNC
               MOVE AV-AS-AT TO ITD-DATE-1
               MOVE WS-MIN-SHELF TO ITD-NUMBER
               CALL "ITMDATE" USING ITD-BLOCK
               MOVE ITD-DATE-R TO WS-LIMIT-DATE
               IF WS-EXPIRY < WS-LIMIT-DATE
                   MOVE "Inside minimum shelf life" TO WS-WHY
                   GO TO 1300-EXIT
               END-IF
           END-IF
           IF WS-LOC-AVAIL NOT = "1"
               MOVE "Location not available" TO WS-WHY
               GO TO 1300-EXIT
           END-IF
           IF WS-LIFECYCLE = "BLOCKED" OR WS-LIFECYCLE = "RETIRED"
               MOVE "Item is blocked or retired" TO WS-WHY
               GO TO 1300-EXIT
           END-IF
           MOVE "Y" TO WS-ELIGIBLE.
       1300-EXIT.
           EXIT.

       1400-ACCUMULATE SECTION.
       1400-START.
           IF WS-OWNER-TYPE NOT = "OWNED"
               ADD WS-BUCKET-QTY TO AV-QTY-THIRDPARTY
               GO TO 1400-EXIT
           END-IF
           ADD WS-BUCKET-QTY TO AV-QTY-PHYSICAL
           IF WS-ELIGIBLE = "Y"
               ADD WS-BUCKET-QTY TO AV-QTY-ELIGIBLE
               GO TO 1400-EXIT
           END-IF
           EVALUATE WS-WHY
               WHEN "In transit"
                   ADD WS-BUCKET-QTY TO AV-QTY-INTRANSIT
               WHEN "Quarantined"
                   ADD WS-BUCKET-QTY TO AV-QTY-QUARANTINE
               WHEN "Awaiting inspection"
                   ADD WS-BUCKET-QTY TO AV-QTY-INSPECTION
               WHEN "Rejected"
                   ADD WS-BUCKET-QTY TO AV-QTY-REJECTED
               WHEN "Expired"
                   ADD WS-BUCKET-QTY TO AV-QTY-EXPIRED
               WHEN "Inside minimum shelf life"
                   ADD WS-BUCKET-QTY TO AV-QTY-EXPIRED
               WHEN OTHER
                   ADD WS-BUCKET-QTY TO AV-QTY-UNAVAIL-LOC
           END-EVALUATE.
       1400-EXIT.
           EXIT.

       1450-ADD-LINE SECTION.
       1450-START.
           IF CA-FUNCTION = "AVAIL"
               GO TO 1450-EXIT
           END-IF
           IF AV-LINE-CNT >= 40
               MOVE "Y" TO WS-OVERFLOW
               GO TO 1450-EXIT
           END-IF
           ADD 1 TO AV-LINE-CNT
           MOVE ITM-COL(1) TO AV-L-SITE(AV-LINE-CNT)
           MOVE ITM-COL(2) TO AV-L-WAREHOUSE(AV-LINE-CNT)
           MOVE ITM-COL(3) TO AV-L-LOCATION(AV-LINE-CNT)
           MOVE ITM-COL(4) TO AV-L-LOC-TYPE(AV-LINE-CNT)
           MOVE ITM-COL(5) TO AV-L-REVISION(AV-LINE-CNT)
           MOVE ITM-COL(6) TO AV-L-LOT(AV-LINE-CNT)
           MOVE ITM-COL(7) TO AV-L-SERIAL(AV-LINE-CNT)
           MOVE ITM-COL(8) TO AV-L-OWNER-TYPE(AV-LINE-CNT)
           MOVE ITM-COL(9) TO AV-L-OWNER(AV-LINE-CNT)
           MOVE ITM-COL(10) TO AV-L-CONDITION(AV-LINE-CNT)
           MOVE WS-BUCKET-QTY TO AV-L-QTY(AV-LINE-CNT)
           MOVE 0 TO AV-L-RESERVED(AV-LINE-CNT)
           MOVE WS-BUCKET-QTY TO AV-L-FREE(AV-LINE-CNT)
           MOVE WS-EXPIRY TO AV-L-EXPIRY(AV-LINE-CNT)
           MOVE WS-ELIGIBLE TO AV-L-ELIGIBLE(AV-LINE-CNT)
           MOVE WS-WHY TO AV-L-WHY(AV-LINE-CNT)
           IF WS-ELIGIBLE NOT = "Y"
               MOVE 0 TO AV-L-FREE(AV-LINE-CNT)
           END-IF.
       1450-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1500  ACTIVE RESERVATIONS.  STOCK ALREADY RESERVED TO THE
      *>       DEMAND BEING EVALUATED IS REPORTED SEPARATELY SO THAT
      *>       IT CAN BE INCLUDED IN THAT DEMAND'S AVAILABILITY BUT
      *>       NOT IN THE FREE STOCK OFFERED TO ANOTHER DEMAND.
      *>----------------------------------------------------------------
       1500-READ-RESERVATIONS SECTION.
       1500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(SUM(QtyReserved - QtyConsumed),0), "
                  "ISNULL(SUM(CASE WHEN DemandType = ? AND DemandRef"
                  " = ? THEN QtyReserved - QtyConsumed ELSE 0 END),0)"
                  " FROM inv.StockReservation WHERE CompanyCode = ? "
                  "AND ItemCode = ? AND Status = 'ACTIVE' AND (? = "
                  "'' OR SiteCode = ?) AND (? = '' OR LocationCode "
                  "IS NULL OR LocationCode = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 8 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 8
               MOVE SPACES TO SQLIO-PARM-VAL(WS-JX)
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE AV-DEMAND-TYPE TO SQLIO-PARM-VAL(1)
           MOVE AV-DEMAND-REF TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE AV-ITEM-CODE TO SQLIO-PARM-VAL(4)
           MOVE AV-SITE TO SQLIO-PARM-VAL(5)
           MOVE AV-SITE TO SQLIO-PARM-VAL(6)
           MOVE AV-LOCATION TO SQLIO-PARM-VAL(7)
           MOVE AV-LOCATION TO SQLIO-PARM-VAL(8)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1500-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 1500-EXIT
           END-IF
           COMPUTE AV-QTY-RESERVED = FUNCTION NUMVAL(ITM-COL(1))
           COMPUTE AV-QTY-OWN-DEMAND = FUNCTION NUMVAL(ITM-COL(2)).
       1500-EXIT.
           EXIT.

       1600-READ-PLANNING SECTION.
       1600-START.
           IF AV-SITE = SPACES
               GO TO 1600-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(SafetyStockQty,0) FROM inv.ItemSite"
                  " WHERE CompanyCode = ? AND SiteCode = ? AND "
                  "ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE AV-SITE TO SQLIO-PARM-VAL(2)
           MOVE AV-ITEM-CODE TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-OK
               COMPUTE AV-QTY-SAFETY = FUNCTION NUMVAL(ITM-COL(1))
           END-IF.
       1600-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1700  EXPECTED RECEIPTS ARE REPORTED SEPARATELY WITH A DATE.
      *>       THEY NEVER MASQUERADE AS PHYSICALLY AVAILABLE UNITS.
      *>----------------------------------------------------------------
       1700-READ-EXPECTED SECTION.
       1700-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(SUM((l.OrderedQty - l.ReceivedQty - "
                  "l.CancelledQty) * ISNULL(NULLIF(l.ConversionFactor"
                  ",0),1)),0), ISNULL(CONVERT(char(8), MIN(x.Eta), "
                  "112),'') FROM po.PurchaseOrderLine l JOIN "
                  "po.PurchaseOrder h ON h.PurchaseOrderId = "
                  "l.PurchaseOrderId OUTER APPLY (SELECT "
                  "MIN(s.ExpectedArrivalDate) AS Eta FROM "
                  "po.PurchaseSchedule s WHERE s.PurchaseOrderLineId"
                  " = l.PurchaseOrderLineId) x WHERE "
                  "l.CompanyCode = ? AND l.ItemCode = ? AND h.Status "
                  "IN ('ISSUED','ACKNOWLEDGED','AMENDED') AND "
                  "l.OrderedQty > l.ReceivedQty + l.CancelledQty AND "
                  "(? = '' OR ISNULL(l.DestinationSiteCode, "
                  "h.SiteCode) = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE AV-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE AV-SITE TO SQLIO-PARM-VAL(3)
           MOVE AV-SITE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
      *>       the purchasing module may not be deployed yet; an
      *>       unavailable supply view is labelled, never invented
               MOVE "N" TO AV-DATA-COMPLETE
               MOVE 0 TO AV-QTY-EXPECTED
               GO TO 1700-EXIT
           END-IF
           IF SQLIO-OK
               COMPUTE AV-QTY-EXPECTED = FUNCTION NUMVAL(ITM-COL(1))
               IF ITM-COL(2) NOT = SPACES
                   COMPUTE AV-EXPECTED-DATE =
                       FUNCTION NUMVAL(ITM-COL(2))
               END-IF
           END-IF.
       1700-EXIT.
           EXIT.

       1800-READ-PLANNED SECTION.
       1800-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(SUM(PlannedGoodQty - "
                  "CompletedGoodQty),0) FROM kit.ExecutionKit WHERE "
                  "CompanyCode = ? AND ParentItemCode = ? AND Status"
                  " IN ('RELEASED','ISSUED','PARTISSUED',"
                  "'PARTCOMPLETE') AND (? = '' OR SiteCode = ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE AV-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE AV-SITE TO SQLIO-PARM-VAL(3)
           MOVE AV-SITE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               MOVE "N" TO AV-DATA-COMPLETE
               MOVE 0 TO AV-QTY-PLANNED
               GO TO 1800-EXIT
           END-IF
           IF SQLIO-OK
               COMPUTE AV-QTY-PLANNED = FUNCTION NUMVAL(ITM-COL(1))
           END-IF.
       1800-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1900  free = eligible - active reservations.  CR0801: SAFETY
      *>       STOCK IS A PLANNING TARGET, SO IT IS SHOWN AS A SECOND,
      *>       POLICY VIEW AND NOT DEDUCTED FROM PHYSICAL FREE STOCK.
      *>----------------------------------------------------------------
       1900-COMPUTE SECTION.
       1900-START.
           COMPUTE AV-QTY-FREE = AV-QTY-ELIGIBLE - AV-QTY-RESERVED
           IF AV-QTY-FREE < 0
               MOVE 0 TO AV-QTY-FREE
           END-IF
           COMPUTE AV-QTY-FOR-DEMAND =
               AV-QTY-FREE + AV-QTY-OWN-DEMAND
           COMPUTE AV-QTY-POLICY = AV-QTY-FREE - AV-QTY-SAFETY
           IF AV-QTY-POLICY < 0
               MOVE 0 TO AV-QTY-POLICY
           END-IF
           MOVE AV-QTY-FREE TO WS-QTY-ED
           MOVE SPACES TO WS-TEXT
           STRING "Free " FUNCTION TRIM(WS-QTY-ED) " "
                  FUNCTION TRIM(AV-STOCK-UOM) " of eligible "
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE AV-QTY-ELIGIBLE TO WS-QTY-ED
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(WS-TEXT) FUNCTION TRIM(WS-QTY-ED)
                  " (physical " DELIMITED BY SIZE INTO CA-MESSAGE
           MOVE AV-QTY-PHYSICAL TO WS-QTY-ED
           MOVE SPACES TO WS-TEXT
           STRING FUNCTION TRIM(CA-MESSAGE) FUNCTION TRIM(WS-QTY-ED)
                  ")" DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE
           IF AV-QTY-PHYSICAL = 0 AND AV-QTY-THIRDPARTY = 0
               MOVE 16 TO CA-RETURN-CODE
               MOVE "AVL0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No stock of " FUNCTION TRIM(AV-ITEM-CODE)
                      " is recorded in that scope"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           IF AV-DATA-COMPLETE NOT = "Y"
               MOVE 4 TO CA-RETURN-CODE
               MOVE "AVL0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Expected and planned supply are not available"
                      " and are shown as not reported"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       1900-EXIT.
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
           MOVE "AVL0002" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "AVAILQ does not support function "
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
               MOVE "SQL failure in AVAILQ" TO CA-MESSAGE
           END-IF
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE 12 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
