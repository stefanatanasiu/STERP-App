      *>================================================================
      *> PROGRAM-ID : POORDER
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : PURCHASE ORDER CAPTURE, PRICING, SCHEDULING AND
      *>              ISSUE.  COMPLETE SUPPLIER, CURRENCY, ITEM,
      *>              QUANTITY, UNIT, DESTINATION, TERMS AND SCHEDULE
      *>              INFORMATION (PO-003, PO-008).  ISSUE IS THE
      *>              ONLY POINT AT WHICH A COMMITMENT EXISTS.
      *> CALLED BY  : the .NET purchase order form, REQGEN
      *> INTERFACE  : CALL "POORDER" USING DFHCOMMAREA (POORDERCA)
      *> CALLS      : NUMGEN, POPRICE, PAYTERMC, POSUPCHK, POAPPRV,
      *>              BUDGCHK, POAMEND, SQLIO -> po.PurchaseOrder,
      *>              po.PurchaseOrderLine, po.PurchaseSchedule
      *>----------------------------------------------------------------
      *> A DRAFT IS NOT A PURCHASE ORDER.  THE SUPPLIER HAS NOT BEEN
      *> TOLD, NO MONEY IS COMMITTED AND NOTHING IS OWED.  ISSUE IS
      *> REFUSED UNLESS THE EXACT CONTENT IS APPROVED (POAPPRV), THE
      *> SUPPLIER PASSES DUE DILIGENCE (POSUPCHK) AND THE PAYMENT
      *> TERMS ARE INSIDE THE STATUTORY MAXIMUM (PAYTERMC).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-03-09 RJH  CR0780 PURCHASE ORDER ENTRY
      *> 2001-06-25 DJM  CR0998 MULTIPLE SCHEDULES PER LINE
      *> 2026-10-06 STERP ISSUE GATES AND COMMITMENT MOVEMENTS
      *> 2026-10-07 STERP CR9014 A STOCK LINE MUST NAME A GOVERNED
      *>                 ITEM - FREE TEXT CANNOT CREATE ONE
      *>================================================================
      *> REQ: PO-003, PO-004, PO-005, PO-006, PO-008, PO-026, PO-027
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POORDER.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 09/03/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==WS-NUM-CA==
                      LEADING ==CA-== BY ==NM-==.
       COPY POPRICECA REPLACING ==DFHCOMMAREA== BY ==WS-PRICE-CA==
                      LEADING ==CA-== BY ==PC-==.
       COPY PAYTERMCCA REPLACING ==DFHCOMMAREA== BY ==WS-TERM-CA==
                      LEADING ==CA-== BY ==TR-==.
       COPY POSUPCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-DIL-CA==
                      LEADING ==CA-== BY ==DL-==.
       COPY POAPPRVCA REPLACING ==DFHCOMMAREA== BY ==WS-APR-CA==
                      LEADING ==CA-== BY ==AP-==.
       COPY BUDGCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-BUD-CA==
                      LEADING ==CA-== BY ==BG-==.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-HDR-ROW.
           05  WS-H-ID                 PIC X(20).
           05  WS-H-STATUS             PIC X(14).
           05  WS-H-SUPPLIER           PIC X(20).
           05  WS-H-CURRENCY           PIC X(3).
           05  WS-H-TERMS              PIC X(12).
           05  WS-H-NET                PIC X(24).
           05  WS-H-VAT                PIC X(24).
           05  WS-H-GROSS              PIC X(24).
           05  WS-H-LINES              PIC X(12).
           05  WS-H-BUDGET             PIC X(20).
           05  WS-H-TYPE               PIC X(12).
       01  WS-LIN-ROW.
           05  WS-L-ID                 PIC X(20).
           05  WS-L-NET                PIC X(24).
           05  WS-L-NONREC             PIC X(24).
           05  WS-L-ACCOUNT            PIC X(12).
           05  WS-L-CC                 PIC X(20).
           05  WS-L-PROJECT            PIC X(20).
           05  WS-L-QTY                PIC X(24).
           05  WS-L-NUM                PIC X(12).
       01  WS-ONE-COL.
           05  WS-V1                   PIC X(24).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-EFFECTIVE            PIC S9(11)V9(6) VALUE 0.
           05  WS-SCHED-TOTAL          PIC S9(13)V9(6) VALUE 0.
           05  WS-AMT-ED               PIC -(11)9.99.
           05  WS-QTY-ED               PIC -(9)9.999999.
       LINKAGE SECTION.
       COPY POORDERCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "CREATE"
                   PERFORM 2000-CREATE THRU 2000-EXIT
               WHEN "ADDLINE"
                   PERFORM 3000-ADDLINE THRU 3000-EXIT
               WHEN "SCHEDULE"
                   PERFORM 4000-SCHEDULE THRU 4000-EXIT
               WHEN "ISSUE"
                   PERFORM 5000-ISSUE THRU 5000-EXIT
               WHEN "ENQUIRE"
                   PERFORM 8000-READ-HDR THRU 8000-EXIT
               WHEN "ACKNOWL"
                   CALL "POAMEND" USING DFHCOMMAREA
               WHEN "AMEND"
                   CALL "POAMEND" USING DFHCOMMAREA
               WHEN "CALLOFF"
                   CALL "POAMEND" USING DFHCOMMAREA
               WHEN "COPY"
                   CALL "POAMEND" USING DFHCOMMAREA
               WHEN OTHER
                   MOVE "ORD0008" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                          " is not supported by POORDER"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE 8 TO CA-RETURN-CODE
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE "N" TO WS-FATAL-SW
           MOVE "POORDER" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE 0 TO OR-ORDER-ID OR-LINE-ID OR-SCHEDULE-ID
           MOVE 0 TO OR-LINE-NET-GBP OR-LINE-VAT-GBP
           MOVE 0 TO OR-LINE-NONREC-VAT OR-NET-AMOUNT
           MOVE 0 TO OR-VAT-AMOUNT OR-GROSS-AMOUNT
           MOVE 0 TO OR-COMMITMENT-GBP OR-OUTSTANDING-QTY
           MOVE 0 TO OR-SCHEDULED-QTY OR-LINE-CNT OR-SCHEDULE-CNT
           MOVE 0 TO OR-BLANKET-QTY-LEFT OR-BLANKET-VAL-LEFT
           MOVE SPACES TO OR-PRICE-SOURCE OR-STATUS OR-OUTCOME
           MOVE "N" TO OR-NEEDS-REAPPR OR-MATERIAL-CHANGE
           IF OR-ORDER-DATE = 0
               MOVE CA-BUSINESS-DATE TO OR-ORDER-DATE
           END-IF
           IF OR-ORDER-TYPE = SPACES
               MOVE "STANDARD" TO OR-ORDER-TYPE
           END-IF
           IF OR-CURRENCY = SPACES
               MOVE "GBP" TO OR-CURRENCY
           END-IF
           IF OR-FX-RATE <= 0
               MOVE 1 TO OR-FX-RATE
           END-IF
           IF OR-LINE-TYPE = SPACES
               MOVE "STOCK" TO OR-LINE-TYPE
           END-IF
           IF OR-PRICE-PER <= 0
               MOVE 1 TO OR-PRICE-PER
           END-IF
           IF OR-CONV-FACTOR <= 0
               MOVE 1 TO OR-CONV-FACTOR
           END-IF
           IF OR-RECOVERABLE-VAT = SPACES
               MOVE "Y" TO OR-RECOVERABLE-VAT
           END-IF
           IF CA-COMPANY = SPACES OR CA-SITE = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A company code and a site code are needed "
                      "for purchase order maintenance"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> CREATE THE HEADER.  THE PAYMENT TERMS ARE CHECKED AND THE
      *> AGREED WORDING IS SNAPSHOT NOW, SO THAT A LATER CHANGE TO
      *> THE TERMS CODE CANNOT REWRITE WHAT WE AGREED (PO-027).
      *>----------------------------------------------------------------
       2000-CREATE SECTION.
       2000-START.
           IF OR-SUPPLIER = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A purchase order must name a supplier"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF
           IF NOT OR-TYPE-KNOWN
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0008" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Order type " FUNCTION TRIM(OR-ORDER-TYPE)
                      " is not recognised"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF
           IF OR-CALLOFF AND OR-BLANKET-NUMBER = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0017" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A call-off order must name the blanket "
                      "order it is called off against"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-TERMS THRU 2100-EXIT
           IF CA-RETURN-CODE >= 8
               GO TO 2000-EXIT
           END-IF
           IF OR-ORDER-NUMBER = SPACES
               PERFORM 9100-NUMBER THRU 9100-EXIT
               IF WS-FATAL
                   GO TO 2000-EXIT
               END-IF
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.PurchaseOrder (CompanyCode, SiteCode, "
                  "OrderNumber, OrderType, BlanketOrderNumber, "
                  "SupplierCode, BuyerUser, OrderDate, CurrencyCode,"
                  " FxRate, TermsCode, TermsSnapshot, IncotermsRule,"
                  " IncotermsNamedPlace, OwnershipPassesAt, "
                  "DeliveryAddressSnapshot, "
                  "TermsAndConditionsVersion, SupplierReference, "
                  "BudgetCode, LinkedSalesOrderNumber, Status, "
                  "CorrelationId, CreatedBy) OUTPUT "
                  "CAST(INSERTED.PurchaseOrderId AS varchar(20)) "
                  "VALUES (?, ?, ?, ?, NULLIF(?, ''), ?, "
                  "NULLIF(?, ''), TRY_CONVERT(date, ?), ?, ?, "
                  "NULLIF(?, ''), NULLIF(?, ''), NULLIF(?, ''), "
                  "NULLIF(?, ''), NULLIF(?, ''), NULLIF(?, ''), "
                  "NULLIF(?, ''), NULLIF(?, ''), NULLIF(?, ''), "
                  "NULLIF(?, ''), 'DRAFT', NULLIF(?, ''), ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE CA-SITE TO SQLIO-PARM-VAL(2)
           MOVE OR-ORDER-NUMBER TO SQLIO-PARM-VAL(3)
           MOVE OR-ORDER-TYPE TO SQLIO-PARM-VAL(4)
           MOVE OR-BLANKET-NUMBER TO SQLIO-PARM-VAL(5)
           MOVE OR-SUPPLIER TO SQLIO-PARM-VAL(6)
           IF OR-BUYER = SPACES
               MOVE CA-USER TO OR-BUYER
           END-IF
           MOVE OR-BUYER TO SQLIO-PARM-VAL(7)
           MOVE OR-ORDER-DATE TO SQLIO-PARM-VAL(8)
           MOVE OR-CURRENCY TO SQLIO-PARM-VAL(9)
           MOVE OR-FX-RATE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE OR-TERMS-CODE TO SQLIO-PARM-VAL(11)
           MOVE OR-TERMS-SNAPSHOT TO SQLIO-PARM-VAL(12)
           MOVE OR-INCOTERMS TO SQLIO-PARM-VAL(13)
           MOVE OR-NAMED-PLACE TO SQLIO-PARM-VAL(14)
           MOVE OR-OWNERSHIP-AT TO SQLIO-PARM-VAL(15)
           MOVE OR-DELIVERY-ADDR TO SQLIO-PARM-VAL(16)
           MOVE OR-TC-VERSION TO SQLIO-PARM-VAL(17)
           MOVE OR-SUPPLIER-REF TO SQLIO-PARM-VAL(18)
           MOVE OR-BUDGET-CODE TO SQLIO-PARM-VAL(19)
           MOVE OR-LINKED-SO TO SQLIO-PARM-VAL(20)
           MOVE CA-CORRELATION-ID TO SQLIO-PARM-VAL(21)
           MOVE CA-USER TO SQLIO-PARM-VAL(22)
           MOVE 22 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           COMPUTE OR-ORDER-ID = FUNCTION NUMVAL(WS-V1)
           MOVE "DRAFT" TO OR-STATUS
           MOVE "CAPTURED" TO OR-OUTCOME
           IF CA-RETURN-CODE = 0
               MOVE SPACES TO CA-MESSAGE
               STRING "Draft order " FUNCTION TRIM(OR-ORDER-NUMBER)
                      " opened for supplier "
                      FUNCTION TRIM(OR-SUPPLIER) " on "
                      FUNCTION TRIM(OR-TERMS-SNAPSHOT)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2100-TERMS SECTION.
       2100-START.
           IF OR-TERMS-CODE = SPACES
               GO TO 2100-EXIT
           END-IF
           INITIALIZE WS-TERM-CA
           MOVE "APPLY" TO TR-FUNCTION
           MOVE CA-COMPANY TO TR-COMPANY
           MOVE CA-SITE TO TR-SITE
           MOVE CA-USER TO TR-USER
           MOVE OR-ORDER-DATE TO TR-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO TR-CORRELATION-ID
           MOVE OR-SUPPLIER TO PT-SUPPLIER
           MOVE OR-ORDER-NUMBER TO PT-ORDER-NUMBER
           MOVE OR-TERMS-CODE TO PT-TERMS-CODE
           MOVE OR-ORDER-DATE TO PT-ORDER-DATE
           CALL "PAYTERMC" USING WS-TERM-CA
           IF PT-TERMS-SNAPSHOT NOT = SPACES
               MOVE PT-TERMS-SNAPSHOT TO OR-TERMS-SNAPSHOT
           END-IF
           IF TR-RETURN-CODE >= 4
               MOVE TR-RETURN-CODE TO CA-RETURN-CODE
               MOVE TR-REASON-CODE TO CA-REASON-CODE
               MOVE TR-MESSAGE TO CA-MESSAGE
           END-IF
           IF TR-RETURN-CODE = 6
               MOVE "ORD0014" TO CA-REASON-CODE
           END-IF.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> ADD OR REPLACE A LINE.  A STOCK LINE MUST NAME A GOVERNED
      *> ITEM; A FREE TEXT LINE MUST CARRY A NOMINAL ACCOUNT.  THAT
      *> IS HOW PO-003 STOPS UNGOVERNED STOCKED ITEMS APPEARING.
      *>----------------------------------------------------------------
       3000-ADDLINE SECTION.
       3000-START.
           PERFORM 8000-READ-HDR THRU 8000-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           IF OR-STATUS = "ISSUED" OR OR-STATUS = "CLOSED" OR
              OR-STATUS = "CANCELLED" OR OR-STATUS = "ACKNOWLEDGED"
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Order " FUNCTION TRIM(OR-ORDER-NUMBER)
                      " is " FUNCTION TRIM(OR-STATUS)
                      " - use AMEND rather than ADDLINE"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 3000-EXIT
           END-IF
           IF OR-DESCRIPTION = SPACES OR OR-ORDERED-QTY <= 0
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "An order line needs a description and a "
                      "quantity greater than zero"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 3000-EXIT
           END-IF
           IF OR-L-STOCK AND OR-ITEM-CODE = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0018" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A stock line must name an existing item; "
                      "free text cannot create a stocked item"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 3000-EXIT
           END-IF
           IF OR-L-STOCK
               PERFORM 3050-ITEM-GOVERNED THRU 3050-EXIT
               IF CA-RETURN-CODE >= 8
                   GO TO 3000-EXIT
               END-IF
           END-IF
           IF OR-ITEM-CODE = SPACES AND OR-ACCOUNT-CODE = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0005" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A non stock or service line needs a nominal "
                      "account code"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 3000-EXIT
           END-IF
           IF OR-L-STOCK AND OR-DEST-SITE = SPACES
               MOVE CA-SITE TO OR-DEST-SITE
           END-IF
           IF OR-PRICE-FROM-FILE
               PERFORM 3100-PRICE THRU 3100-EXIT
           END-IF
           PERFORM 3200-VALUE THRU 3200-EXIT
           IF OR-LINE-NUM = 0
               PERFORM 3300-NEXT-LINE THRU 3300-EXIT
           END-IF
           PERFORM 3400-WRITE-LINE THRU 3400-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           PERFORM 3500-RETOTAL THRU 3500-EXIT
           MOVE OR-LINE-NET-GBP TO WS-AMT-ED
           IF CA-RETURN-CODE = 0
               MOVE SPACES TO CA-MESSAGE
               STRING "Line " FUNCTION TRIM(OR-DESCRIPTION)
                      " valued at " FUNCTION TRIM(WS-AMT-ED)
                      " added to order "
                      FUNCTION TRIM(OR-ORDER-NUMBER)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-003.  A STOCK LINE MAY ONLY NAME AN ITEM THAT THE ITEM
      *> MASTER ALREADY GOVERNS AND ALLOWS TO BE BOUGHT.  WITHOUT
      *> THIS A TYPED PART NUMBER WOULD BECOME AN UNGOVERNED STOCKED
      *> ITEM ON AN ORDER AND, LATER, IN THE WAREHOUSE.
      *> 2026-10-07 STERP CR9014 UNGOVERNED STOCK ITEM ON FREE TEXT
      *>----------------------------------------------------------------
       3050-ITEM-GOVERNED SECTION.
       3050-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CASE WHEN i.IsPurchasable = 1 THEN 'Y' "
                  "ELSE 'N' END FROM inv.Item i WHERE "
                  "i.CompanyCode = ? AND i.ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE OR-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-COL-WIDTH(1)
           MOVE SPACES TO WS-ONE-COL
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 3050-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0018" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(OR-ITEM-CODE)
                      " is not on the item master; a stocked item "
                      "cannot be created from free text"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 3050-EXIT
           END-IF
           IF WS-V1(1:1) NOT = "Y"
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0018" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(OR-ITEM-CODE)
                      " is not flagged as purchasable on the item "
                      "master"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       3050-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE PRICING DATE OF AN ORDER IS ITS ORDER DATE, SO A LATER
      *> SUPPLIER PRICE CHANGE CANNOT SILENTLY REPRICE IT (PO-004).
      *>----------------------------------------------------------------
       3100-PRICE SECTION.
       3100-START.
           INITIALIZE WS-PRICE-CA
           MOVE "PRICE" TO PC-FUNCTION
           MOVE CA-COMPANY TO PC-COMPANY
           MOVE CA-SITE TO PC-SITE
           MOVE CA-USER TO PC-USER
           MOVE OR-ORDER-DATE TO PC-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO PC-CORRELATION-ID
           MOVE OR-SUPPLIER TO PR-SUPPLIER
           MOVE OR-ITEM-CODE TO PR-ITEM-CODE
           MOVE OR-UOM-CODE TO PR-UOM-CODE
           MOVE OR-ORDERED-QTY TO PR-QUANTITY
           MOVE OR-ORDER-DATE TO PR-PRICING-DATE
           MOVE OR-CURRENCY TO PR-CURRENCY
           CALL "POPRICE" USING WS-PRICE-CA
           IF PC-RETURN-CODE >= 8
               MOVE 4 TO CA-RETURN-CODE
               MOVE PC-REASON-CODE TO CA-REASON-CODE
               MOVE PC-MESSAGE TO CA-MESSAGE
               GO TO 3100-EXIT
           END-IF
           MOVE PR-UNIT-PRICE TO OR-UNIT-PRICE
           MOVE PR-PRICE-PER TO OR-PRICE-PER
           MOVE PR-DISCOUNT-PCT TO OR-DISCOUNT-PCT
           MOVE PR-PRICE-BASIS TO OR-PRICE-SOURCE
           IF OR-PRICE-PER <= 0
               MOVE 1 TO OR-PRICE-PER
           END-IF.
       3100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> VALUE THE LINE AT SIX DECIMALS AND ROUND ONCE.
      *>----------------------------------------------------------------
       3200-VALUE SECTION.
       3200-START.
           COMPUTE WS-EFFECTIVE ROUNDED =
                   (OR-UNIT-PRICE / OR-PRICE-PER) *
                   (1 - (OR-DISCOUNT-PCT / 100))
           COMPUTE OR-LINE-NET-GBP ROUNDED =
                   (WS-EFFECTIVE * OR-ORDERED-QTY * OR-FX-RATE)
                   + OR-CHARGE-AMT
           COMPUTE OR-LINE-VAT-GBP ROUNDED =
                   OR-LINE-NET-GBP * (OR-VAT-RATE-PCT / 100)
           IF OR-VAT-RECOVERABLE
               MOVE 0 TO OR-LINE-NONREC-VAT
           ELSE
               MOVE OR-LINE-VAT-GBP TO OR-LINE-NONREC-VAT
           END-IF.
       3200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3300-NEXT-LINE SECTION.
       3300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(MAX(LineNum), 0) + 1 AS "
                  "varchar(24)) FROM po.PurchaseOrderLine WHERE "
                  "PurchaseOrderId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE OR-ORDER-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 3300-EXIT
           END-IF
           COMPUTE OR-LINE-NUM = FUNCTION NUMVAL(WS-V1).
       3300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3400-WRITE-LINE SECTION.
       3400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE po.PurchaseSchedule WHERE "
                  "PurchaseOrderLineId IN (SELECT "
                  "PurchaseOrderLineId FROM po.PurchaseOrderLine "
                  "WHERE PurchaseOrderId = ? AND LineNum = ?); "
                  "DELETE po.PurchaseOrderLine WHERE "
                  "PurchaseOrderId = ? AND LineNum = ?; INSERT "
                  "po.PurchaseOrderLine (PurchaseOrderId, "
                  "CompanyCode, OrderNumber, LineNum, ItemCode, "
                  "Revision, Description, LineType, OrderedQty, "
                  "UomCode, ConversionFactor, UnitPrice, "
                  "PricePerQty, PriceSource, DiscountPct, "
                  "ChargeAmount, NetAmount, VatCode, VatRatePct, "
                  "ExpectedVatAmount, IsRecoverableVat, "
                  "NonRecoverableVatGBP, AccountCode, DimCostCentre,"
                  " DimProject, JobRef, DestinationSiteCode, "
                  "DestinationLocationCode, RequisitionNumber, "
                  "LinkedSalesOrderNumber, LinkedSalesOrderLine, "
                  "Status) OUTPUT CAST(INSERTED.PurchaseOrderLineId "
                  "AS varchar(20)) VALUES (?, ?, ?, ?, NULLIF(?, "
                  "''), NULLIF(?, ''), ?, ?, ?, NULLIF(?, ''), ?, ?,"
                  " ?, NULLIF(?, ''), ?, ?, ?, NULLIF(?, ''), ?, ?, "
                  "?, ?, NULLIF(?, ''), NULLIF(?, ''), NULLIF(?, "
                  "''), NULLIF(?, ''), NULLIF(?, ''), NULLIF(?, ''),"
                  " NULLIF(?, ''), NULLIF(?, ''), NULLIF(?, 0), "
                  "'OPEN')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE OR-ORDER-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE OR-LINE-NUM TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE SQLIO-PARM-VAL(1) TO SQLIO-PARM-VAL(3)
           MOVE SQLIO-PARM-VAL(2) TO SQLIO-PARM-VAL(4)
           MOVE SQLIO-PARM-VAL(1) TO SQLIO-PARM-VAL(5)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(6)
           MOVE OR-ORDER-NUMBER TO SQLIO-PARM-VAL(7)
           MOVE SQLIO-PARM-VAL(2) TO SQLIO-PARM-VAL(8)
           MOVE OR-ITEM-CODE TO SQLIO-PARM-VAL(9)
           MOVE OR-REVISION TO SQLIO-PARM-VAL(10)
           MOVE OR-DESCRIPTION TO SQLIO-PARM-VAL(11)
           MOVE OR-LINE-TYPE TO SQLIO-PARM-VAL(12)
           MOVE OR-ORDERED-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           MOVE OR-UOM-CODE TO SQLIO-PARM-VAL(14)
           MOVE OR-CONV-FACTOR TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(15)
           MOVE OR-UNIT-PRICE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(16)
           MOVE OR-PRICE-PER TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(17)
           MOVE OR-PRICE-SOURCE TO SQLIO-PARM-VAL(18)
           MOVE OR-DISCOUNT-PCT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(19)
           MOVE OR-CHARGE-AMT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(20)
           MOVE OR-LINE-NET-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(21)
           MOVE OR-VAT-CODE TO SQLIO-PARM-VAL(22)
           MOVE OR-VAT-RATE-PCT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(23)
           MOVE OR-LINE-VAT-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(24)
           IF OR-VAT-RECOVERABLE
               MOVE "1" TO SQLIO-PARM-VAL(25)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(25)
           END-IF
           MOVE OR-LINE-NONREC-VAT TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(26)
           MOVE OR-ACCOUNT-CODE TO SQLIO-PARM-VAL(27)
           MOVE OR-COST-CENTRE TO SQLIO-PARM-VAL(28)
           MOVE OR-PROJECT TO SQLIO-PARM-VAL(29)
           MOVE OR-JOB-REF TO SQLIO-PARM-VAL(30)
           MOVE OR-DEST-SITE TO SQLIO-PARM-VAL(31)
           MOVE OR-DEST-LOCATION TO SQLIO-PARM-VAL(32)
           MOVE OR-REQ-NUMBER TO SQLIO-PARM-VAL(33)
           MOVE OR-LINKED-SO TO SQLIO-PARM-VAL(34)
           MOVE OR-LINKED-SO-LINE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(35)
           MOVE 35 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 3400-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               COMPUTE OR-LINE-ID = FUNCTION NUMVAL(WS-V1)
           END-IF.
       3400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3500-RETOTAL SECTION.
       3500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE o SET NetAmount = t.Net, VatAmount = "
                  "t.Vat, GrossAmount = t.Net + t.Vat, "
                  "CommitmentGBP = t.Net + t.NonRec, ModifiedBy = ?,"
                  " ModifiedAt = SYSDATETIME() FROM "
                  "po.PurchaseOrder o CROSS APPLY (SELECT "
                  "ISNULL(SUM(l.NetAmount), 0) AS Net, "
                  "ISNULL(SUM(l.ExpectedVatAmount), 0) AS Vat, "
                  "ISNULL(SUM(l.NonRecoverableVatGBP), 0) AS NonRec "
                  "FROM po.PurchaseOrderLine l WHERE "
                  "l.PurchaseOrderId = o.PurchaseOrderId AND "
                  "l.Status <> 'CANCELLED') t WHERE "
                  "o.PurchaseOrderId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE OR-ORDER-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 3500-EXIT
           END-IF
           PERFORM 8000-READ-HDR THRU 8000-EXIT.
       3500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> SCHEDULES.  THE SCHEDULE TOTAL MUST RECONCILE TO THE
      *> AUTHORISED LINE QUANTITY - OTHERWISE THE COMMITMENT WOULD
      *> BE DUPLICATED ACROSS SCHEDULES (PO-008).
      *>----------------------------------------------------------------
       4000-SCHEDULE SECTION.
       4000-START.
           PERFORM 8000-READ-HDR THRU 8000-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           IF OR-SCH-QTY <= 0 OR OR-SCH-REQ-DATE = 0
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A delivery schedule needs a quantity and a "
                      "requested date"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
           PERFORM 4100-READ-LINE THRU 4100-EXIT
           IF WS-FATAL
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(CASE WHEN s.ScheduleNo = ?"
                  " THEN 0 ELSE s.Quantity END), 0) + ? AS "
                  "varchar(24)) FROM po.PurchaseSchedule s WHERE "
                  "s.PurchaseOrderLineId = ? AND s.Status <> "
                  "'CANCELLED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE OR-SCHEDULE-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE OR-SCH-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE OR-LINE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           COMPUTE WS-SCHED-TOTAL = FUNCTION NUMVAL(WS-V1)
           MOVE WS-SCHED-TOTAL TO OR-SCHEDULED-QTY
           IF WS-SCHED-TOTAL > OR-ORDERED-QTY
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0007" TO CA-REASON-CODE
               MOVE WS-SCHED-TOTAL TO WS-QTY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "The schedules would total "
                      FUNCTION TRIM(WS-QTY-ED)
                      " which is more than the authorised line "
                      "quantity"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
           IF OR-SCHEDULE-NO = 0
               MOVE 1 TO OR-SCHEDULE-NO
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "MERGE po.PurchaseSchedule AS t USING (SELECT ? AS"
                  " L, ? AS S) AS s ON t.PurchaseOrderLineId = s.L "
                  "AND t.ScheduleNo = s.S WHEN MATCHED THEN UPDATE "
                  "SET Quantity = ?, RequestedDate = "
                  "TRY_CONVERT(date, ?), CurrentConfirmedDate = "
                  "TRY_CONVERT(date, NULLIF(?, '00000000')), "
                  "DestinationSiteCode = NULLIF(?, ''), "
                  "DestinationLocationCode = NULLIF(?, '') WHEN NOT "
                  "MATCHED THEN INSERT (PurchaseOrderLineId, "
                  "CompanyCode, OrderNumber, LineNum, ScheduleNo, "
                  "Quantity, RequestedDate, OriginalConfirmedDate, "
                  "CurrentConfirmedDate, DestinationSiteCode, "
                  "DestinationLocationCode, Status) VALUES (s.L, ?, "
                  "?, ?, s.S, ?, TRY_CONVERT(date, ?), "
                  "TRY_CONVERT(date, NULLIF(?, '00000000')), "
                  "TRY_CONVERT(date, NULLIF(?, '00000000')), "
                  "NULLIF(?, ''), NULLIF(?, ''), 'OPEN') OUTPUT "
                  "CAST(INSERTED.ScheduleId AS varchar(24));"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE OR-LINE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE OR-SCHEDULE-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE OR-SCH-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE OR-SCH-REQ-DATE TO SQLIO-PARM-VAL(4)
           MOVE OR-SCH-CONF-DATE TO SQLIO-PARM-VAL(5)
           MOVE OR-SCH-DEST-SITE TO SQLIO-PARM-VAL(6)
           MOVE OR-SCH-DEST-LOCN TO SQLIO-PARM-VAL(7)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(8)
           MOVE OR-ORDER-NUMBER TO SQLIO-PARM-VAL(9)
           MOVE OR-LINE-NUM TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE SQLIO-PARM-VAL(3) TO SQLIO-PARM-VAL(11)
           MOVE OR-SCH-REQ-DATE TO SQLIO-PARM-VAL(12)
           MOVE OR-SCH-CONF-DATE TO SQLIO-PARM-VAL(13)
           MOVE OR-SCH-CONF-DATE TO SQLIO-PARM-VAL(14)
           MOVE OR-SCH-DEST-SITE TO SQLIO-PARM-VAL(15)
           MOVE OR-SCH-DEST-LOCN TO SQLIO-PARM-VAL(16)
           MOVE 16 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF NOT SQLIO-NOTFOUND
               COMPUTE OR-SCHEDULE-ID = FUNCTION NUMVAL(WS-V1)
           END-IF
           COMPUTE OR-OUTSTANDING-QTY =
                   OR-ORDERED-QTY - WS-SCHED-TOTAL
           MOVE OR-SCH-QTY TO WS-QTY-ED
           MOVE SPACES TO CA-MESSAGE
           STRING "Schedule " FUNCTION TRIM(WS-QTY-ED)
                  " recorded on line "
                  FUNCTION TRIM(OR-DESCRIPTION)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4100-READ-LINE SECTION.
       4100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(l.PurchaseOrderLineId AS "
                  "varchar(20)), CAST(l.NetAmount AS varchar(24)), "
                  "CAST(l.NonRecoverableVatGBP AS varchar(24)), "
                  "ISNULL(l.AccountCode, ''), "
                  "ISNULL(l.DimCostCentre, ''), "
                  "ISNULL(l.DimProject, ''), CAST(l.OrderedQty AS "
                  "varchar(24)), CAST(l.LineNum AS varchar(12)) FROM"
                  " po.PurchaseOrderLine l WHERE l.PurchaseOrderId ="
                  " ? AND l.LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE OR-ORDER-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE OR-LINE-NUM TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 8 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 20 TO SQLIO-COL-WIDTH(5)
           MOVE 20 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 12 TO SQLIO-COL-WIDTH(8)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LIN-ROW
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 4100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "ORD0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "That line is not on order "
                      FUNCTION TRIM(OR-ORDER-NUMBER)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 4100-EXIT
           END-IF
           COMPUTE OR-LINE-ID = FUNCTION NUMVAL(WS-L-ID)
           COMPUTE OR-LINE-NET-GBP = FUNCTION NUMVAL(WS-L-NET)
           COMPUTE OR-LINE-NONREC-VAT = FUNCTION NUMVAL(WS-L-NONREC)
           COMPUTE OR-ORDERED-QTY = FUNCTION NUMVAL(WS-L-QTY)
           IF OR-ACCOUNT-CODE = SPACES
               MOVE WS-L-ACCOUNT TO OR-ACCOUNT-CODE
           END-IF
           IF OR-COST-CENTRE = SPACES
               MOVE WS-L-CC TO OR-COST-CENTRE
           END-IF
           IF OR-PROJECT = SPACES
               MOVE WS-L-PROJECT TO OR-PROJECT
           END-IF.
       4100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> ISSUE.  THREE GATES, THEN THE COMMITMENT.
      *>----------------------------------------------------------------
       5000-ISSUE SECTION.
       5000-START.
           PERFORM 8000-READ-HDR THRU 8000-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           IF OR-LINE-CNT = 0
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0009" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Order " FUNCTION TRIM(OR-ORDER-NUMBER)
                      " has no lines and cannot be issued"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 5000-EXIT
           END-IF
           IF OR-STATUS = "ISSUED" OR OR-STATUS = "ACKNOWLEDGED"
               MOVE 4 TO CA-RETURN-CODE
               MOVE "ORD0003" TO CA-REASON-CODE
               MOVE "ISSUED" TO OR-OUTCOME
               MOVE SPACES TO CA-MESSAGE
               STRING "Order " FUNCTION TRIM(OR-ORDER-NUMBER)
                      " has already been issued to the supplier"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 5000-EXIT
           END-IF
           PERFORM 5100-DILIGENCE THRU 5100-EXIT
           IF CA-RETURN-CODE >= 6
               MOVE "HELD" TO OR-OUTCOME
               GO TO 5000-EXIT
           END-IF
           PERFORM 2100-TERMS THRU 2100-EXIT
           IF CA-RETURN-CODE >= 6
               MOVE "HELD" TO OR-OUTCOME
               GO TO 5000-EXIT
           END-IF
           PERFORM 5200-APPROVAL THRU 5200-EXIT
           IF CA-RETURN-CODE >= 6
               MOVE "HELD" TO OR-OUTCOME
               GO TO 5000-EXIT
           END-IF
           PERFORM 5300-COMMIT-LINES THRU 5300-EXIT
           IF WS-FATAL
               GO TO 5000-EXIT
           END-IF
           PERFORM 5400-MARK-ISSUED THRU 5400-EXIT
           IF OR-ORDER-TYPE = "BLANKET"
               PERFORM 5500-BLANKET-BAL THRU 5500-EXIT
           END-IF.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A BLANKET AGREEMENT IS ONLY AN AGREEMENT ONCE IT HAS BEEN
      *> ISSUED, SO THE CALL-OFF BALANCE IS OPENED HERE AND NOWHERE
      *> ELSE (PO-024).  THE MERGE MAKES A REISSUE HARMLESS.
      *>----------------------------------------------------------------
       5500-BLANKET-BAL SECTION.
       5500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "MERGE po.BlanketBalance AS t USING (SELECT "
                  "l.CompanyCode, l.OrderNumber, l.LineNum, "
                  "l.ItemCode, l.Description, l.OrderedQty, "
                  "l.NetAmount, o.CurrencyCode FROM "
                  "po.PurchaseOrderLine l JOIN po.PurchaseOrder o ON"
                  " o.PurchaseOrderId = l.PurchaseOrderId WHERE "
                  "o.PurchaseOrderId = ? AND l.Status = 'OPEN') AS s"
                  " ON t.CompanyCode = s.CompanyCode AND "
                  "t.BlanketOrderNumber = s.OrderNumber AND "
                  "t.LineNum = s.LineNum WHEN NOT MATCHED THEN "
                  "INSERT (CompanyCode, BlanketOrderNumber, LineNum,"
                  " ItemCode, Description, AgreedQty, AgreedValue, "
                  "CurrencyCode, Status, ModifiedBy, ModifiedAt) "
                  "VALUES (s.CompanyCode, s.OrderNumber, s.LineNum, "
                  "s.ItemCode, s.Description, s.OrderedQty, "
                  "s.NetAmount, s.CurrencyCode, 'OPEN', ?, "
                  "SYSDATETIME());"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE OR-ORDER-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
           END-IF.
       5500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       5100-DILIGENCE SECTION.
       5100-START.
           INITIALIZE WS-DIL-CA
           MOVE "CHECK" TO DL-FUNCTION
           MOVE CA-COMPANY TO DL-COMPANY
           MOVE CA-SITE TO DL-SITE
           MOVE CA-USER TO DL-USER
           MOVE OR-ORDER-DATE TO DL-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO DL-CORRELATION-ID
           MOVE OR-SUPPLIER TO DD-SUPPLIER
           MOVE OR-ORDER-NUMBER TO DD-ORDER-NUMBER
           MOVE OR-GROSS-AMOUNT TO DD-ORDER-VALUE-GBP
           MOVE OR-ORDER-DATE TO DD-CHECK-DATE
           CALL "POSUPCHK" USING WS-DIL-CA
           IF DL-RETURN-CODE >= 6 OR DD-CAN-ORDER NOT = "Y"
               MOVE 6 TO CA-RETURN-CODE
               IF DL-RETURN-CODE = 8
                   MOVE 8 TO CA-RETURN-CODE
               END-IF
               MOVE "ORD0013" TO CA-REASON-CODE
               MOVE DL-MESSAGE TO CA-MESSAGE
               GO TO 5100-EXIT
           END-IF
           IF DL-RETURN-CODE = 4
               MOVE 4 TO CA-RETURN-CODE
               MOVE "ORD0013" TO CA-REASON-CODE
               MOVE DL-MESSAGE TO CA-MESSAGE
           END-IF.
       5100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       5200-APPROVAL SECTION.
       5200-START.
           INITIALIZE WS-APR-CA
           MOVE "CHECK" TO AP-FUNCTION
           MOVE CA-COMPANY TO AP-COMPANY
           MOVE CA-SITE TO AP-SITE
           MOVE CA-USER TO AP-USER
           MOVE OR-ORDER-DATE TO AP-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO AP-CORRELATION-ID
           MOVE OR-ORDER-NUMBER TO AV-ORDER-NUMBER
           CALL "POAPPRV" USING WS-APR-CA
           MOVE AV-CONTENT-HASH TO OR-CONTENT-HASH
           IF AV-WAS-INVALIDATED = "Y"
               MOVE "Y" TO OR-NEEDS-REAPPR
           END-IF
           IF AP-RETURN-CODE NOT = 0
               MOVE AP-RETURN-CODE TO CA-RETURN-CODE
               MOVE "ORD0012" TO CA-REASON-CODE
               MOVE AP-MESSAGE TO CA-MESSAGE
           END-IF.
       5200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> ONE COMMITTED MOVEMENT PER LIVE LINE, WITH THE LINE'S OWN
      *> CODING, SO THAT THE COMMITMENT REPORT CAN BE ANALYSED THE
      *> SAME WAY AS THE NOMINAL LEDGER.
      *>----------------------------------------------------------------
       5300-COMMIT-LINES SECTION.
       5300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(l.PurchaseOrderLineId AS "
                  "varchar(20)), CAST(l.NetAmount AS varchar(24)), "
                  "CAST(l.NonRecoverableVatGBP AS varchar(24)), "
                  "ISNULL(l.AccountCode, ''), "
                  "ISNULL(l.DimCostCentre, ''), "
                  "ISNULL(l.DimProject, ''), CAST(l.OrderedQty AS "
                  "varchar(24)), CAST(l.LineNum AS varchar(12)) FROM"
                  " po.PurchaseOrderLine l WHERE l.PurchaseOrderId ="
                  " ? AND l.Status = 'OPEN' ORDER BY l.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE OR-ORDER-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 8 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 20 TO SQLIO-COL-WIDTH(5)
           MOVE 20 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 12 TO SQLIO-COL-WIDTH(8)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LIN-ROW
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 5300-EXIT
           END-IF
           MOVE 0 TO OR-COMMITMENT-GBP
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LIN-ROW
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               PERFORM 5350-ONE-LINE THRU 5350-EXIT
               MOVE 3 TO SQLIO-CURSOR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-LIN-ROW
           END-PERFORM
           MOVE 3 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LIN-ROW.
       5300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       5350-ONE-LINE SECTION.
       5350-START.
           INITIALIZE WS-BUD-CA
           MOVE "COMMIT" TO BG-FUNCTION
           MOVE CA-COMPANY TO BG-COMPANY
           MOVE CA-SITE TO BG-SITE
           MOVE CA-USER TO BG-USER
           MOVE OR-ORDER-DATE TO BG-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO BG-CORRELATION-ID
           MOVE OR-BUDGET-CODE TO BC-BUDGET-CODE
           MOVE WS-L-ACCOUNT TO BC-ACCOUNT-CODE
           MOVE WS-L-CC TO BC-COST-CENTRE
           MOVE WS-L-PROJECT TO BC-PROJECT
           MOVE "PURCHASEORDER" TO BC-DOC-TYPE
           MOVE OR-ORDER-NUMBER TO BC-DOC-REF
           COMPUTE BC-DOC-LINE = FUNCTION NUMVAL(WS-L-NUM)
           MOVE OR-BUYER TO BC-REQUESTER
           MOVE OR-ORDER-DATE TO BC-MOVEMENT-DATE
           COMPUTE BC-QUANTITY = FUNCTION NUMVAL(WS-L-QTY)
           COMPUTE BC-NET-GBP = FUNCTION NUMVAL(WS-L-NET)
           COMPUTE BC-NONREC-VAT-GBP = FUNCTION NUMVAL(WS-L-NONREC)
           CALL "BUDGCHK" USING WS-BUD-CA
           ADD BC-PROPOSED-GBP TO OR-COMMITMENT-GBP
           IF BG-RETURN-CODE >= 6
               MOVE BG-RETURN-CODE TO CA-RETURN-CODE
               MOVE BG-REASON-CODE TO CA-REASON-CODE
               MOVE BG-MESSAGE TO CA-MESSAGE
               MOVE "HELD" TO OR-OUTCOME
               MOVE "Y" TO WS-FATAL-SW
           END-IF.
       5350-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       5400-MARK-ISSUED SECTION.
       5400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.PurchaseOrder SET Status = 'ISSUED', "
                  "IssuedAt = SYSDATETIME(), CommitmentGBP = ?, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() WHERE "
                  "PurchaseOrderId = ? AND Status IN ('APPROVED', "
                  "'AMENDED')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE OR-COMMITMENT-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE OR-ORDER-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 5400-EXIT
           END-IF
           MOVE "ISSUED" TO OR-STATUS
           MOVE "ISSUED" TO OR-OUTCOME
           MOVE OR-COMMITMENT-GBP TO WS-AMT-ED
           MOVE SPACES TO CA-MESSAGE
           STRING "Order " FUNCTION TRIM(OR-ORDER-NUMBER)
                  " issued to " FUNCTION TRIM(OR-SUPPLIER)
                  " committing " FUNCTION TRIM(WS-AMT-ED)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       5400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8000-READ-HDR SECTION.
       8000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(o.PurchaseOrderId AS varchar(20)), "
                  "o.Status, o.SupplierCode, o.CurrencyCode, "
                  "ISNULL(o.TermsCode, ''), CAST(o.NetAmount AS "
                  "varchar(24)), CAST(o.VatAmount AS varchar(24)), "
                  "CAST(o.GrossAmount AS varchar(24)), "
                  "CAST((SELECT COUNT(*) FROM po.PurchaseOrderLine l"
                  " WHERE l.PurchaseOrderId = o.PurchaseOrderId AND "
                  "l.Status <> 'CANCELLED') AS varchar(12)), "
                  "ISNULL(o.BudgetCode, ''), o.OrderType FROM "
                  "po.PurchaseOrder o WHERE o.CompanyCode = ? AND "
                  "o.OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE OR-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 11 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 14 TO SQLIO-COL-WIDTH(2)
           MOVE 20 TO SQLIO-COL-WIDTH(3)
           MOVE 3 TO SQLIO-COL-WIDTH(4)
           MOVE 12 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 24 TO SQLIO-COL-WIDTH(8)
           MOVE 12 TO SQLIO-COL-WIDTH(9)
           MOVE 20 TO SQLIO-COL-WIDTH(10)
           MOVE 12 TO SQLIO-COL-WIDTH(11)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-HDR-ROW
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 8000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "ORD0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Purchase order "
                      FUNCTION TRIM(OR-ORDER-NUMBER)
                      " is not on file for company "
                      FUNCTION TRIM(CA-COMPANY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 8000-EXIT
           END-IF
           COMPUTE OR-ORDER-ID = FUNCTION NUMVAL(WS-H-ID)
           MOVE WS-H-STATUS TO OR-STATUS
           MOVE WS-H-SUPPLIER TO OR-SUPPLIER
           MOVE WS-H-CURRENCY TO OR-CURRENCY
           IF OR-TERMS-CODE = SPACES
               MOVE WS-H-TERMS TO OR-TERMS-CODE
           END-IF
           IF OR-BUDGET-CODE = SPACES
               MOVE WS-H-BUDGET TO OR-BUDGET-CODE
           END-IF
           MOVE WS-H-TYPE TO OR-ORDER-TYPE
           COMPUTE OR-NET-AMOUNT = FUNCTION NUMVAL(WS-H-NET)
           COMPUTE OR-VAT-AMOUNT = FUNCTION NUMVAL(WS-H-VAT)
           COMPUTE OR-GROSS-AMOUNT = FUNCTION NUMVAL(WS-H-GROSS)
           COMPUTE OR-LINE-CNT = FUNCTION NUMVAL(WS-H-LINES).
       8000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-SQL-FAIL SECTION.
       9000-START.
           MOVE 12 TO CA-RETURN-CODE
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           MOVE "Y" TO WS-FATAL-SW.
       9000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9100-NUMBER SECTION.
       9100-START.
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO NM-FUNCTION
           MOVE CA-COMPANY TO NM-COMPANY
           MOVE CA-SITE TO NM-SITE
           MOVE CA-USER TO NM-USER
           MOVE OR-ORDER-DATE TO NM-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO NM-CORRELATION-ID
           MOVE "POORD" TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF NM-RETURN-CODE > 4
               MOVE 12 TO CA-RETURN-CODE
               MOVE "ORD0010" TO CA-REASON-CODE
               MOVE NM-MESSAGE TO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 9100-EXIT
           END-IF
           MOVE NG-DOCUMENT-NUMBER TO OR-ORDER-NUMBER.
       9100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9300-NO-NULLS SECTION.
       9300-START.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM.
       9300-EXIT.
           EXIT.
       END PROGRAM POORDER.
