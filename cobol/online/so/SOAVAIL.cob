      *>================================================================
      *> PROGRAM-ID : SOAVAIL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : AVAILABILITY AND PROMISE ENQUIRY FOR THE SALES
      *>              DESK (SO-010).  THE ANSWER IS NEVER ONE NUMBER.
      *>              PHYSICAL FREE STOCK, EXISTING RESERVATIONS,
      *>              EXPECTED RECEIPTS AND FEASIBLE KIT OR ASSEMBLY
      *>              SUPPLY ARE EACH REPORTED ON THEIR OWN LABELLED
      *>              LINE WITH THE SOURCE DOCUMENT AND THE DATE.
      *>
      *>              ONLY PHYSICAL FREE STOCK IS GUARANTEED.  AN
      *>              EXPECTED RECEIPT IS A SUPPLIER'S PROMISE AND A
      *>              TRIAL KIT IS AN ARITHMETIC POSSIBILITY; BOTH ARE
      *>              RETURNED WITH SA-L-GUARANTEED = "N" AND SAY SO
      *>              IN WORDS.  NOTHING HERE RESERVES ANYTHING - THAT
      *>              IS SORESV'S JOB AND THE ONLY THING THAT STOPS
      *>              ANOTHER ORDER TAKING THE SAME UNITS.
      *>
      *>              THE STOCK PICTURE ITSELF IS NOT RECOMPUTED HERE.
      *>              IT COMES FROM AVAILQ, WHICH OWNS THE DEFINITIONS
      *>              OF PHYSICAL, ELIGIBLE, RESERVED AND FREE.
      *> CALLED BY  : SALES ORDER ENTRY, THE PROMISE ENQUIRY, SORESV,
      *>              THE .NET CLIENT
      *> INTERFACE  : CALL "SOAVAIL" USING WS-SOAVAIL-CA
      *>              COPYBOOK SOAVAILCA
      *> CALLS      : AVAILQ, DATECALC, SQLIO (po, kit, so reads only)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-05-11 RJH  CR0931 ORDER PROMISING (ACUCOBOL DISTRIBUTION)
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS THROUGHOUT
      *> 2003-02-18 DJM  CR1101 EXPECTED RECEIPTS SHOWN SEPARATELY
      *>                 AFTER THE RDG DOUBLE PROMISE INCIDENT
      *> 2026-10-06 STERP SO-010 LABELLED SOURCES AND GUARANTEE FLAG
      *>================================================================
      *> REQ: SO-010
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOAVAIL.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 11/05/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY AVAILQCA REPLACING ==DFHCOMMAREA== BY ==WS-AVQ-CA==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DTC-CA==.
       01  WS-ROW.
           05  WS-COL OCCURS 12 TIMES  PIC X(100).
       01  WS-WORK.
           05  WS-BAD                  PIC X VALUE "N".
               88  WS-BAD-INPUT                  VALUE "Y".
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-LX                   PIC S9(4) VALUE 0.
           05  WS-DONE                 PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-QTY                  PIC S9(13)V9(6) VALUE 0.
           05  WS-NEEDED               PIC S9(13)V9(6) VALUE 0.
           05  WS-TAKEN                PIC S9(13)V9(6) VALUE 0.
           05  WS-DATE-N               PIC 9(8) VALUE 0.
           05  WS-BEST-DATE            PIC 9(8) VALUE 0.
           05  WS-TEXT                 PIC X(200) VALUE SPACES.
           05  WS-QTY-ED               PIC Z(9)9.9(6).
           05  WS-INT-ED               PIC -(9)9.
           05  WS-ROWS                 PIC S9(4) VALUE 0.
       LINKAGE SECTION.
       COPY SOAVAILCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       1000-MAIN.
           PERFORM 1100-INITIALISE THRU 1100-EXIT
           PERFORM 1200-VALIDATE THRU 1200-EXIT
           IF WS-BAD-INPUT
               GO TO 1000-RETURN
           END-IF
           EVALUATE CA-FUNCTION OF DFHCOMMAREA
               WHEN "ENQUIRE"
                   PERFORM 2000-ENQUIRE THRU 2000-EXIT
               WHEN "PROMISE"
                   PERFORM 3000-PROMISE THRU 3000-EXIT
               WHEN "ORDER"
                   PERFORM 4000-ORDER THRU 4000-EXIT
               WHEN OTHER
                   PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       1000-RETURN.
           GOBACK.

      *>----------------------------------------------------------------
      *> 1100  WORKING-STORAGE SURVIVES BETWEEN CALLS INSIDE THE ENGINE,
      *>       SO EVERY FIELD IS RESET BEFORE IT IS USED.
      *>----------------------------------------------------------------
       1100-INITIALISE.
           MOVE "N" TO WS-BAD
           MOVE "N" TO WS-DONE
           MOVE 0 TO WS-I WS-J WS-LX WS-ROWS
           MOVE 0 TO WS-QTY WS-NEEDED WS-TAKEN
           MOVE 0 TO WS-DATE-N WS-BEST-DATE
           MOVE SPACES TO WS-TEXT
           MOVE "SOAVAIL" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "SO-010" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE FUNCTION UPPER-CASE(SA-ITEM-CODE) TO SA-ITEM-CODE
           MOVE 0 TO SA-QTY-PHYSICAL SA-QTY-ELIGIBLE
           MOVE 0 TO SA-QTY-RESERVED SA-QTY-FREE
           MOVE 0 TO SA-QTY-OWN-DEMAND SA-QTY-EXPECTED
           MOVE 0 TO SA-QTY-PLANNED SA-QTY-KITABLE
           MOVE 0 TO SA-QTY-QUARANTINE SA-QTY-EXPIRED
           MOVE 0 TO SA-QTY-REJECTED SA-QTY-THIRDPARTY
           MOVE 0 TO SA-QTY-GUARANTEED SA-QTY-PROMISED
           MOVE 0 TO SA-QTY-SHORTFALL
           MOVE 0 TO SA-EXPECTED-DATE SA-PROMISE-DATE
           MOVE 0 TO SA-LINE-CNT SA-ORDER-LINE-CNT
           MOVE SPACES TO SA-PROMISE-TEXT SA-STOCK-UOM
           MOVE "NONE" TO SA-PROMISE-BASIS
           MOVE "N" TO SA-PROMISE-FULL
           MOVE "N" TO SA-GUARANTEED
           MOVE "Y" TO SA-DATA-COMPLETE
           IF SA-AS-AT = 0
               MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO SA-AS-AT
           END-IF
           IF SA-REQUIRED-DATE = 0
               MOVE SA-AS-AT TO SA-REQUIRED-DATE
           END-IF
           IF SA-HORIZON-DAYS = 0
               MOVE 90 TO SA-HORIZON-DAYS
           END-IF
           IF SA-WITH-RECEIPTS NOT = "N"
               MOVE "Y" TO SA-WITH-RECEIPTS
           END-IF
           IF SA-WITH-KIT NOT = "N"
               MOVE "Y" TO SA-WITH-KIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 12
               MOVE 100 TO SQLIO-COL-WIDTH(WS-I)
           END-PERFORM.
       1100-EXIT.
           EXIT.

       1200-VALIDATE.
           IF CA-COMPANY OF DFHCOMMAREA = SPACES
               MOVE "SAV0001" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A company code is required." TO WS-TEXT
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1200-EXIT
           END-IF
           IF CA-FUNCTION OF DFHCOMMAREA = "ORDER"
               IF SA-ORDER-NUMBER = SPACES
                   MOVE "SAV0001" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SPACES TO WS-TEXT
                   STRING "An order number is required to report "
                          "availability for every line."
                       DELIMITED BY SIZE INTO WS-TEXT
                   PERFORM 9000-REJECT THRU 9000-EXIT
               END-IF
               GO TO 1200-EXIT
           END-IF
           IF SA-ITEM-CODE = SPACES
               MOVE "SAV0001" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "An item code is required." TO WS-TEXT
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1200-EXIT
           END-IF
           IF CA-FUNCTION OF DFHCOMMAREA = "PROMISE"
              AND SA-QTY-REQUIRED <= 0
               MOVE "SAV0006" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "A quantity greater than zero is needed "
                      "before a delivery can be promised."
                   DELIMITED BY SIZE INTO WS-TEXT
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  ENQUIRE - the full labelled picture for one item at one
      *>       site.  Each supply kind is a separate line; none of them
      *>       is added into another.
      *>----------------------------------------------------------------
       2000-ENQUIRE.
           PERFORM 2100-CALL-AVAILQ THRU 2100-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-STOCK-LINES THRU 2200-EXIT
           IF SA-WITH-RECEIPTS = "Y"
               PERFORM 2300-RECEIPT-LINES THRU 2300-EXIT
               PERFORM 2400-PLANNED-LINES THRU 2400-EXIT
           END-IF
           IF SA-WITH-KIT = "Y"
               PERFORM 2500-KIT-LINE THRU 2500-EXIT
           END-IF
           PERFORM 2900-SUMMARISE THRU 2900-EXIT.
       2000-EXIT.
           EXIT.

      *>   AVAILQ owns the quantity definitions - never re-implement
      *>   them here.  The demand reference is passed on so that stock
      *>   already held for THIS order is reported, not offered twice.
       2100-CALL-AVAILQ.
           INITIALIZE WS-AVQ-CA
           MOVE "AVAIL" TO CA-FUNCTION OF WS-AVQ-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-AVQ-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-AVQ-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-AVQ-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-AVQ-CA
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA
               TO CA-CORRELATION-ID OF WS-AVQ-CA
           MOVE SA-ITEM-CODE TO AV-ITEM-CODE
           MOVE SA-REVISION TO AV-REVISION
           MOVE SA-SITE TO AV-SITE
           MOVE SA-WAREHOUSE TO AV-WAREHOUSE
           MOVE "OWNED" TO AV-OWNER-TYPE
           MOVE SA-AS-AT TO AV-AS-AT
           IF SA-ORDER-NUMBER NOT = SPACES
               MOVE "SALESORDER" TO AV-DEMAND-TYPE
               MOVE SA-ORDER-NUMBER TO AV-DEMAND-REF
               MOVE SA-LINE-NUM TO AV-DEMAND-LINE
           END-IF
           CALL "AVAILQ" USING WS-AVQ-CA
           IF CA-RETURN-CODE OF WS-AVQ-CA > 4
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "SAV0003" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-AVQ-CA
                   TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 2100-EXIT
           END-IF
           MOVE AV-STOCK-UOM TO SA-STOCK-UOM
           MOVE AV-QTY-PHYSICAL TO SA-QTY-PHYSICAL
           MOVE AV-QTY-ELIGIBLE TO SA-QTY-ELIGIBLE
           MOVE AV-QTY-RESERVED TO SA-QTY-RESERVED
           MOVE AV-QTY-FREE TO SA-QTY-FREE
           MOVE AV-QTY-OWN-DEMAND TO SA-QTY-OWN-DEMAND
           MOVE AV-QTY-QUARANTINE TO SA-QTY-QUARANTINE
           MOVE AV-QTY-EXPIRED TO SA-QTY-EXPIRED
           MOVE AV-QTY-REJECTED TO SA-QTY-REJECTED
           MOVE AV-QTY-THIRDPARTY TO SA-QTY-THIRDPARTY
           IF AV-DATA-COMPLETE NOT = "Y"
               MOVE "N" TO SA-DATA-COMPLETE
           END-IF.
       2100-EXIT.
           EXIT.

      *>   Line 1 is the only guaranteed one.  Line 2 says plainly that
      *>   the reserved quantity belongs to somebody else already.
       2200-STOCK-LINES.
           PERFORM 8000-NEXT-LINE THRU 8000-EXIT
           MOVE "FREESTOCK" TO SA-L-KIND(WS-LX)
           MOVE "Physical free stock" TO SA-L-LABEL(WS-LX)
           MOVE SA-ITEM-CODE TO SA-L-ITEM(WS-LX)
           MOVE SA-SITE TO SA-L-SITE(WS-LX)
           MOVE SA-WAREHOUSE TO SA-L-WHSE(WS-LX)
           MOVE SA-QTY-FREE TO SA-L-QTY(WS-LX)
           MOVE SA-AS-AT TO SA-L-DATE(WS-LX)
           MOVE "STOCKBALANCE" TO SA-L-DOC-TYPE(WS-LX)
           MOVE SA-SITE TO SA-L-DOC-REF(WS-LX)
           MOVE "Y" TO SA-L-GUARANTEED(WS-LX)
           MOVE SPACES TO WS-TEXT
           STRING "Owned, eligible and unreserved - this quantity "
                  "may be allocated now."
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO SA-L-TEXT(WS-LX)
           IF SA-QTY-RESERVED > 0
               PERFORM 8000-NEXT-LINE THRU 8000-EXIT
               MOVE "RESERVED" TO SA-L-KIND(WS-LX)
               MOVE "Already reserved" TO SA-L-LABEL(WS-LX)
               MOVE SA-ITEM-CODE TO SA-L-ITEM(WS-LX)
               MOVE SA-SITE TO SA-L-SITE(WS-LX)
               MOVE SA-QTY-RESERVED TO SA-L-QTY(WS-LX)
               MOVE SA-AS-AT TO SA-L-DATE(WS-LX)
               MOVE "RESERVATION" TO SA-L-DOC-TYPE(WS-LX)
               MOVE "N" TO SA-L-GUARANTEED(WS-LX)
               MOVE SPACES TO WS-TEXT
               STRING "Held for other demand; it is not available "
                      "to this order."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO SA-L-TEXT(WS-LX)
           END-IF.
       2200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2300  EXPECTED RECEIPTS.  Confirmed purchase commitments that
      *>       have not arrived.  They are a supplier's promise, so the
      *>       guarantee flag is "N" and the purchase order number and
      *>       its confirmed date are shown with every quantity.
      *>----------------------------------------------------------------
       2300-RECEIPT-LINES.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT p.OrderNumber, "
                  "CONVERT(char(8), COALESCE(s.ExpectedArrivalDate, "
                  "s.CurrentConfirmedDate, s.RequestedDate), 112), "
                  "CONVERT(varchar(30), s.Quantity - s.ReceivedQty), "
                  "CONVERT(varchar(12), s.LineNum), "
                  "ISNULL(s.DestinationSiteCode, p.SiteCode), "
                  "p.SupplierCode "
                  "FROM po.PurchaseSchedule s "
                  "JOIN po.PurchaseOrderLine l ON "
                  "l.PurchaseOrderLineId = s.PurchaseOrderLineId "
                  "JOIN po.PurchaseOrder p ON "
                  "p.PurchaseOrderId = l.PurchaseOrderId "
                  "WHERE s.CompanyCode = ? AND l.ItemCode = ? "
                  "AND s.Status IN ('OPEN','PARTIAL') "
                  "AND l.Status = 'OPEN' "
                  "AND p.Status IN ('APPROVED','ISSUED', "
                  "'ACKNOWLEDGED','AMENDED') "
                  "AND s.Quantity > s.ReceivedQty "
                  "AND (? = '' OR ISNULL(s.DestinationSiteCode, "
                  "p.SiteCode) = ?) "
                  "ORDER BY 2, p.OrderNumber"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE SA-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE SA-SITE TO SQLIO-PARM-VAL(3)
           MOVE SA-SITE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9350-SOFT-SQL THRU 9350-EXIT
               GO TO 2300-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ROW
               IF SQLIO-ERROR OR SQLIO-NOTFOUND
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 2350-ONE-RECEIPT THRU 2350-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       2300-EXIT.
           EXIT.

       2350-ONE-RECEIPT.
           IF SA-LINE-CNT >= 40
               MOVE "N" TO SA-DATA-COMPLETE
               GO TO 2350-EXIT
           END-IF
           COMPUTE WS-QTY = FUNCTION NUMVAL(WS-COL(3))
           IF WS-QTY <= 0
               GO TO 2350-EXIT
           END-IF
           PERFORM 8000-NEXT-LINE THRU 8000-EXIT
           MOVE "EXPECTED" TO SA-L-KIND(WS-LX)
           MOVE "Expected purchase receipt" TO SA-L-LABEL(WS-LX)
           MOVE SA-ITEM-CODE TO SA-L-ITEM(WS-LX)
           MOVE WS-COL(5) TO SA-L-SITE(WS-LX)
           MOVE WS-QTY TO SA-L-QTY(WS-LX)
           MOVE WS-COL(2)(1:8) TO WS-DATE-N
           MOVE WS-DATE-N TO SA-L-DATE(WS-LX)
           MOVE "PURCHASEORDER" TO SA-L-DOC-TYPE(WS-LX)
           MOVE WS-COL(1) TO SA-L-DOC-REF(WS-LX)
           COMPUTE SA-L-DOC-LINE(WS-LX) = FUNCTION NUMVAL(WS-COL(4))
           MOVE "N" TO SA-L-GUARANTEED(WS-LX)
           MOVE SPACES TO WS-TEXT
           STRING "Not a physical allocation - supplier "
                  FUNCTION TRIM(WS-COL(6))
                  " has yet to deliver this quantity."
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO SA-L-TEXT(WS-LX)
           ADD WS-QTY TO SA-QTY-EXPECTED
           IF SA-EXPECTED-DATE = 0 OR WS-DATE-N < SA-EXPECTED-DATE
               MOVE WS-DATE-N TO SA-EXPECTED-DATE
           END-IF.
       2350-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2400  PLANNED ASSEMBLY OUTPUT.  A released execution kit that
      *>       will finish this item.  Still not a physical allocation.
      *>----------------------------------------------------------------
       2400-PLANNED-LINES.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT k.KitRef, CONVERT(char(8), "
                  "COALESCE(k.PlannedCompletionDate, k.NeedDate), "
                  "112), CONVERT(varchar(30), k.PlannedGoodQty - "
                  "k.CompletedGoodQty), k.SiteCode "
                  "FROM kit.ExecutionKit k "
                  "WHERE k.CompanyCode = ? AND k.ParentItemCode = ? "
                  "AND k.Status IN ('RELEASED','PARTISSUED', "
                  "'ISSUED','PARTCOMPLETE','OPCOMPLETE') "
                  "AND k.PlannedGoodQty > k.CompletedGoodQty "
                  "AND (? = '' OR k.SiteCode = ?) "
                  "ORDER BY 2, k.KitRef"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE SA-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE SA-SITE TO SQLIO-PARM-VAL(3)
           MOVE SA-SITE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9350-SOFT-SQL THRU 9350-EXIT
               GO TO 2400-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ROW
               IF SQLIO-ERROR OR SQLIO-NOTFOUND
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 2450-ONE-PLANNED THRU 2450-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       2400-EXIT.
           EXIT.

       2450-ONE-PLANNED.
           IF SA-LINE-CNT >= 40
               MOVE "N" TO SA-DATA-COMPLETE
               GO TO 2450-EXIT
           END-IF
           COMPUTE WS-QTY = FUNCTION NUMVAL(WS-COL(3))
           IF WS-QTY <= 0
               GO TO 2450-EXIT
           END-IF
           PERFORM 8000-NEXT-LINE THRU 8000-EXIT
           MOVE "PLANNED" TO SA-L-KIND(WS-LX)
           MOVE "Planned assembly output" TO SA-L-LABEL(WS-LX)
           MOVE SA-ITEM-CODE TO SA-L-ITEM(WS-LX)
           MOVE WS-COL(4) TO SA-L-SITE(WS-LX)
           MOVE WS-QTY TO SA-L-QTY(WS-LX)
           MOVE WS-COL(2)(1:8) TO WS-DATE-N
           MOVE WS-DATE-N TO SA-L-DATE(WS-LX)
           MOVE "EXECUTIONKIT" TO SA-L-DOC-TYPE(WS-LX)
           MOVE WS-COL(1) TO SA-L-DOC-REF(WS-LX)
           MOVE "N" TO SA-L-GUARANTEED(WS-LX)
           MOVE SPACES TO WS-TEXT
           STRING "Not a physical allocation - the assembly has "
                  "still to be completed and booked in."
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO SA-L-TEXT(WS-LX)
           ADD WS-QTY TO SA-QTY-PLANNED.
       2450-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2500  TRIAL KIT FEASIBILITY.  How many whole kits the free
      *>       component stock could make.  It is arithmetic on other
      *>       items' stock, so it is emphatically not an allocation
      *>       of this item and the guarantee flag stays "N".
      *>----------------------------------------------------------------
       2500-KIT-LINE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(30), ISNULL(MIN("
                  "FLOOR(ISNULL(f.Freeq,0) / l.Quantity)), 0)), "
                  "CONVERT(varchar(12), COUNT(*)) "
                  "FROM kit.KitDefinition d "
                  "JOIN kit.KitDefinitionLine l ON "
                  "l.KitDefId = d.KitDefId "
                  "OUTER APPLY (SELECT SUM(b.QtyOnHand - "
                  "b.QtyReserved) AS Freeq FROM inv.StockBalance b "
                  "WHERE b.CompanyCode = d.CompanyCode "
                  "AND b.ItemCode = l.ComponentItemCode "
                  "AND b.QualityStatus = 'ACCEPTED' "
                  "AND b.OwnerType = 'OWNED' "
                  "AND (? = '' OR b.SiteCode = ?)) f "
                  "WHERE d.CompanyCode = ? AND d.KitItemCode = ? "
                  "AND d.Status = 'RELEASED' AND l.IsOptional = 0 "
                  "AND l.Quantity > 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE SA-SITE TO SQLIO-PARM-VAL(1)
           MOVE SA-SITE TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(3)
           MOVE SA-ITEM-CODE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9350-SOFT-SQL THRU 9350-EXIT
               GO TO 2500-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 2500-EXIT
           END-IF
           COMPUTE WS-ROWS = FUNCTION NUMVAL(WS-COL(2))
           IF WS-ROWS = 0
               GO TO 2500-EXIT
           END-IF
           COMPUTE WS-QTY = FUNCTION NUMVAL(WS-COL(1))
           MOVE WS-QTY TO SA-QTY-KITABLE
           IF WS-QTY <= 0
               GO TO 2500-EXIT
           END-IF
           PERFORM 8000-NEXT-LINE THRU 8000-EXIT
           MOVE "KITBUILD" TO SA-L-KIND(WS-LX)
           MOVE "Feasible kit or assembly" TO SA-L-LABEL(WS-LX)
           MOVE SA-ITEM-CODE TO SA-L-ITEM(WS-LX)
           MOVE SA-SITE TO SA-L-SITE(WS-LX)
           MOVE WS-QTY TO SA-L-QTY(WS-LX)
           MOVE SA-AS-AT TO SA-L-DATE(WS-LX)
           MOVE "KITDEFINITION" TO SA-L-DOC-TYPE(WS-LX)
           MOVE SA-ITEM-CODE TO SA-L-DOC-REF(WS-LX)
           MOVE "N" TO SA-L-GUARANTEED(WS-LX)
           MOVE SPACES TO WS-TEXT
           STRING "Trial result only - the components are free but "
                  "no kit has been built or reserved."
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO SA-L-TEXT(WS-LX).
       2500-EXIT.
           EXIT.

      *>   The headline numbers.  Only free stock is guaranteed; the
      *>   message says in plain English what the rest depends on.
       2900-SUMMARISE.
           MOVE SA-QTY-FREE TO SA-QTY-GUARANTEED
           IF SA-QTY-FREE > 0
               MOVE "Y" TO SA-GUARANTEED
           ELSE
               MOVE "N" TO SA-GUARANTEED
           END-IF
           MOVE SA-QTY-FREE TO WS-QTY-ED
           MOVE SPACES TO WS-TEXT
           STRING "Free stock " FUNCTION TRIM(WS-QTY-ED)
                  " is available now; expected receipts and kit "
                  "builds are shown separately and are not "
                  "guaranteed allocations."
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO SA-PROMISE-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
           IF SA-DATA-COMPLETE NOT = "Y"
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "SAV0008" TO CA-REASON-CODE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       2900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  PROMISE - the earliest feasible date for SA-QTY-REQUIRED.
      *>       Free stock is taken first, then the labelled expected
      *>       receipts in date order.  The promise date is the date of
      *>       the LAST source needed, because the customer cannot have
      *>       a complete delivery before then.  A promise that leans on
      *>       a receipt is reported as not guaranteed.
      *>----------------------------------------------------------------
       3000-PROMISE.
           PERFORM 2000-ENQUIRE THRU 2000-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 3000-EXIT
           END-IF
           MOVE SA-QTY-REQUIRED TO WS-NEEDED
           MOVE 0 TO WS-TAKEN
           MOVE 0 TO WS-BEST-DATE
           MOVE "NONE" TO SA-PROMISE-BASIS
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > SA-LINE-CNT OR WS-NEEDED <= 0
               IF SA-L-KIND(WS-I) = "FREESTOCK"
                  OR SA-L-KIND(WS-I) = "EXPECTED"
                   PERFORM 3100-TAKE-FROM-LINE THRU 3100-EXIT
               END-IF
           END-PERFORM
           MOVE WS-TAKEN TO SA-QTY-PROMISED
           COMPUTE SA-QTY-SHORTFALL = SA-QTY-REQUIRED - WS-TAKEN
           IF SA-QTY-SHORTFALL < 0
               MOVE 0 TO SA-QTY-SHORTFALL
           END-IF
           MOVE WS-BEST-DATE TO SA-PROMISE-DATE
           PERFORM 3200-PROMISE-WORDS THRU 3200-EXIT.
       3000-EXIT.
           EXIT.

       3100-TAKE-FROM-LINE.
           IF SA-L-QTY(WS-I) <= 0
               GO TO 3100-EXIT
           END-IF
           IF SA-L-QTY(WS-I) >= WS-NEEDED
               ADD WS-NEEDED TO WS-TAKEN
               MOVE 0 TO WS-NEEDED
           ELSE
               ADD SA-L-QTY(WS-I) TO WS-TAKEN
               SUBTRACT SA-L-QTY(WS-I) FROM WS-NEEDED
           END-IF
           IF SA-L-DATE(WS-I) > WS-BEST-DATE
               MOVE SA-L-DATE(WS-I) TO WS-BEST-DATE
           END-IF
           IF SA-L-KIND(WS-I) = "FREESTOCK"
               ADD 0 TO SA-QTY-GUARANTEED
               IF SA-PROMISE-BASIS = "NONE"
                   MOVE "FREESTOCK" TO SA-PROMISE-BASIS
               END-IF
           ELSE
               MOVE "EXPECTED" TO SA-PROMISE-BASIS
           END-IF.
       3100-EXIT.
           EXIT.

      *>   Say it in words.  "Promised" never implies "reserved".
       3200-PROMISE-WORDS.
           IF SA-QTY-SHORTFALL > 0
               MOVE "N" TO SA-PROMISE-FULL
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "SAV0007" TO CA-REASON-CODE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           ELSE
               MOVE "Y" TO SA-PROMISE-FULL
           END-IF
           IF SA-PROMISE-BASIS = "FREESTOCK"
               MOVE "Y" TO SA-GUARANTEED
           ELSE
               MOVE "N" TO SA-GUARANTEED
           END-IF
           MOVE WS-TAKEN TO WS-QTY-ED
           MOVE SPACES TO WS-TEXT
           EVALUATE SA-PROMISE-BASIS
               WHEN "FREESTOCK"
                   STRING "Can promise " FUNCTION TRIM(WS-QTY-ED)
                          " from physical free stock; this is a "
                          "guaranteed quantity once reserved."
                       DELIMITED BY SIZE INTO WS-TEXT
               WHEN "EXPECTED"
                   STRING "Can promise " FUNCTION TRIM(WS-QTY-ED)
                          " only by relying on expected receipts, "
                          "which are not guaranteed physical stock."
                       DELIMITED BY SIZE INTO WS-TEXT
               WHEN OTHER
                   STRING "Nothing can be promised from free stock "
                          "or expected receipts inside the horizon."
                       DELIMITED BY SIZE INTO WS-TEXT
           END-EVALUATE
           MOVE WS-TEXT TO SA-PROMISE-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA.
       3200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  ORDER - availability for every line of an order.  One
      *>       summary row per line, each carrying its own guarantee
      *>       flag so a part-promised order cannot look complete.
      *>----------------------------------------------------------------
       4000-ORDER.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12), l.LineNum), "
                  "ISNULL(l.ItemCode,''), ISNULL(l.Revision,''), "
                  "CONVERT(varchar(30), l.AcceptedQty - "
                  "l.CancelledQty - l.DespatchedQty), "
                  "ISNULL(o.SiteCode,''), l.LineType "
                  "FROM so.SalesOrderLine l "
                  "JOIN so.SalesOrder o ON "
                  "o.SalesOrderId = l.SalesOrderId "
                  "WHERE l.CompanyCode = ? AND l.OrderNumber = ? "
                  "AND l.Status = 'OPEN' ORDER BY l.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE SA-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 6 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 4000-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           MOVE 0 TO WS-ROWS
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ROW
               IF SQLIO-ERROR OR SQLIO-NOTFOUND
                   MOVE "Y" TO WS-DONE
               ELSE
                   ADD 1 TO WS-ROWS
                   PERFORM 4100-HOLD-ORDER-LINE THRU 4100-EXIT
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF WS-ROWS = 0
               MOVE 16 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "SAV0004" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "No open lines were found for order "
                      FUNCTION TRIM(SA-ORDER-NUMBER)
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 4000-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > SA-ORDER-LINE-CNT
               PERFORM 4200-ONE-ORDER-LINE THRU 4200-EXIT
           END-PERFORM
           MOVE SPACES TO WS-TEXT
           MOVE SA-ORDER-LINE-CNT TO WS-INT-ED
           STRING "Availability reported for "
                  FUNCTION TRIM(WS-INT-ED)
                  " order line(s); only free stock is guaranteed."
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA.
       4000-EXIT.
           EXIT.

      *>   the cursor is needed again for AVAILQ, so the rows are
      *>   captured first and priced for availability afterwards
       4100-HOLD-ORDER-LINE.
           IF SA-ORDER-LINE-CNT >= 20
               MOVE "N" TO SA-DATA-COMPLETE
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "SAV0005" TO CA-REASON-CODE OF DFHCOMMAREA
               GO TO 4100-EXIT
           END-IF
           ADD 1 TO SA-ORDER-LINE-CNT
           MOVE SA-ORDER-LINE-CNT TO WS-J
           COMPUTE SA-O-LINE-NUM(WS-J) = FUNCTION NUMVAL(WS-COL(1))
           MOVE WS-COL(2) TO SA-O-ITEM(WS-J)
           MOVE WS-COL(3) TO SA-O-REVISION(WS-J)
           COMPUTE SA-O-REQUIRED(WS-J) = FUNCTION NUMVAL(WS-COL(4))
           MOVE WS-COL(5) TO SA-O-SITE(WS-J)
           MOVE WS-COL(6) TO SA-O-STATUS(WS-J)
           MOVE 0 TO SA-O-FREE(WS-J) SA-O-RESERVED(WS-J)
           MOVE 0 TO SA-O-EXPECTED(WS-J) SA-O-KITABLE(WS-J)
           MOVE 0 TO SA-O-SHORT(WS-J) SA-O-PROMISE(WS-J)
           MOVE "N" TO SA-O-GUARANTEED(WS-J).
       4100-EXIT.
           EXIT.

       4200-ONE-ORDER-LINE.
           IF SA-O-ITEM(WS-I) = SPACES
              OR SA-O-STATUS(WS-I) = "TEXT"
              OR SA-O-STATUS(WS-I) = "SERVICE"
              OR SA-O-STATUS(WS-I) = "CHARGE"
               MOVE "NOSTOCK" TO SA-O-STATUS(WS-I)
               GO TO 4200-EXIT
           END-IF
           MOVE SA-O-ITEM(WS-I) TO SA-ITEM-CODE
           MOVE SA-O-REVISION(WS-I) TO SA-REVISION
           MOVE SA-O-SITE(WS-I) TO SA-SITE
           MOVE SA-O-LINE-NUM(WS-I) TO SA-LINE-NUM
           PERFORM 2100-CALL-AVAILQ THRU 2100-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "UNKNOWN" TO SA-O-STATUS(WS-I)
               GO TO 4200-EXIT
           END-IF
           MOVE SA-QTY-FREE TO SA-O-FREE(WS-I)
           MOVE SA-QTY-RESERVED TO SA-O-RESERVED(WS-I)
           MOVE 0 TO SA-QTY-EXPECTED
           PERFORM 2300-RECEIPT-LINES THRU 2300-EXIT
           MOVE SA-QTY-EXPECTED TO SA-O-EXPECTED(WS-I)
           IF SA-QTY-FREE >= SA-O-REQUIRED(WS-I)
               MOVE "Y" TO SA-O-GUARANTEED(WS-I)
               MOVE "FREESTOCK" TO SA-O-STATUS(WS-I)
               MOVE 0 TO SA-O-SHORT(WS-I)
               MOVE SA-AS-AT TO SA-O-PROMISE(WS-I)
           ELSE
               MOVE "N" TO SA-O-GUARANTEED(WS-I)
               COMPUTE SA-O-SHORT(WS-I) =
                   SA-O-REQUIRED(WS-I) - SA-QTY-FREE
               MOVE SA-EXPECTED-DATE TO SA-O-PROMISE(WS-I)
               IF SA-QTY-FREE + SA-QTY-EXPECTED
                  >= SA-O-REQUIRED(WS-I)
                   MOVE "EXPECTED" TO SA-O-STATUS(WS-I)
               ELSE
                   MOVE "SHORT" TO SA-O-STATUS(WS-I)
               END-IF
           END-IF.
       4200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 8000  COMMON
      *>----------------------------------------------------------------
       8000-NEXT-LINE.
           IF SA-LINE-CNT >= 40
               MOVE "N" TO SA-DATA-COMPLETE
               MOVE 40 TO WS-LX
               GO TO 8000-EXIT
           END-IF
           ADD 1 TO SA-LINE-CNT
           MOVE SA-LINE-CNT TO WS-LX
           MOVE SA-LINE-CNT TO SA-L-SEQ(WS-LX)
           MOVE SPACES TO SA-L-KIND(WS-LX)
           MOVE SPACES TO SA-L-LABEL(WS-LX)
           MOVE SPACES TO SA-L-ITEM(WS-LX)
           MOVE SPACES TO SA-L-SITE(WS-LX)
           MOVE SPACES TO SA-L-WHSE(WS-LX)
           MOVE 0 TO SA-L-QTY(WS-LX)
           MOVE 0 TO SA-L-DATE(WS-LX)
           MOVE SPACES TO SA-L-DOC-TYPE(WS-LX)
           MOVE SPACES TO SA-L-DOC-REF(WS-LX)
           MOVE 0 TO SA-L-DOC-LINE(WS-LX)
           MOVE "N" TO SA-L-GUARANTEED(WS-LX)
           MOVE SPACES TO SA-L-TEXT(WS-LX).
       8000-EXIT.
           EXIT.

       9000-REJECT.
           MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE "Y" TO WS-BAD.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE "SAV0002" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO WS-TEXT
           STRING "SOAVAIL does not support the function "
                  FUNCTION TRIM(CA-FUNCTION OF DFHCOMMAREA)
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE 12 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE "SQL0001" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SQLIO-ERRMSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       9300-EXIT.
           EXIT.

      *>   An optional supplementary read (purchases, kits) that is not
      *>   deployed must degrade the answer, not destroy it: the stock
      *>   picture is still true, it is simply incomplete, and the
      *>   caller is told so rather than shown a confident half answer.
       9350-SOFT-SQL.
           MOVE "N" TO SA-DATA-COMPLETE
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE 0 TO SQLIO-SQLCODE.
       9350-EXIT.
           EXIT.
