      *>================================================================
      *> PROGRAM-ID : SOAMEND
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT
      *> PURPOSE    : AMENDMENT AND CANCELLATION OF ACCEPTED SALES
      *>              DEMAND.  WHAT HAS ALREADY HAPPENED IS NEVER
      *>              REWRITTEN: A LINE CANNOT FALL BELOW THE QUANTITY
      *>              DESPATCHED OR BILLED, AND A CANCELLATION TOUCHES
      *>              ONLY THE UNFULFILLED BALANCE.  IT DOES NOT
      *>              REVERSE A DESPATCH, AN INVOICE OR A SUPPLIER
      *>              COMMITMENT - EACH OF THOSE HAS ITS OWN
      *>              AUTHORISED PROCESS - AND IT RELEASES ONLY THE
      *>              UNUSED RESERVATIONS.  EVERY AMENDMENT RE-RUNS
      *>              THE CREDIT CHECK, INVALIDATES THE APPROVALS THAT
      *>              DEPENDED ON THE OLD CONTENT AND BUMPS THE
      *>              DOCUMENT VERSION.
      *> CALLED BY  : ORDER AMENDMENT, CUSTOMER SERVICE, .NET CLIENT
      *> INTERFACE  : CALL "SOAMEND" USING WS-SOAMEND-CA
      *>              COPYBOOK SOAMENDCA
      *> CALLS      : SQLIO, CREDCHK, STKRESV, AUDITLOG
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-10-05 RJH  CR0255 ORDER AMENDMENT CONTROL
      *> 2002-04-18 KMP  CR0981 RELEASE ONLY THE UNUSED RESERVATION
      *> 2016-02-22 DLS  CR2901 APPROVAL INVALIDATION ON CONTENT CHANGE
      *> 2026-10-06 STERP SO-016, SO-007, SO-022
      *>================================================================
      *> REQ: SO-016, SO-007, SO-022
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOAMEND.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 05/10/1993.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY CREDCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-CRED-CA==.
       COPY STKRESVCA REPLACING ==DFHCOMMAREA== BY ==WS-RESV-CA==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUD-CA==.
       01  WS-CTL.
           05  WS-FUNC                 PIC X(8).
           05  WS-CO                   PIC X(10).
           05  WS-SITE                 PIC X(10).
           05  WS-USER                 PIC X(20).
           05  WS-BUSDATE              PIC 9(8).
           05  WS-CORREL               PIC X(36).
           05  WS-RC                   PIC 9(2).
           05  WS-REASON               PIC X(8).
           05  WS-MSG                  PIC X(200).
           05  WS-MSGCNT               PIC 9(2).
           05  WS-TRAN-OPEN            PIC X.
           05  WS-I                    PIC S9(4).
           05  WS-P                    PIC S9(4).
           05  WS-LINE-STATUS          PIC X(10).
           05  WS-CANCEL-QTY           PIC S9(13)V9(6).
           05  WS-NEW-CANCELLED        PIC S9(13)V9(6).
           05  WS-NEW-NET              PIC S9(13)V99.
           05  WS-NEW-VAT              PIC S9(13)V99.
           05  WS-OLD-VERSION          PIC 9(6).
           05  WS-HASH-NUM             PIC 9(6).
           05  WS-AMT1                 PIC S9(13)V99.
           05  WS-AMT2                 PIC S9(13)V99.
           05  WS-PPQ                  PIC S9(9)V9(6).
       01  WS-EDITS.
           05  WS-MONEY-ED             PIC -(13)9.99.
           05  WS-QTY-ED               PIC -(13)9.999999.
           05  WS-PRICE-ED             PIC -(11)9.999999.
           05  WS-INT-ED               PIC -(9)9.
           05  WS-UK-MONEY             PIC Z,ZZZ,ZZZ,ZZ9.99-.
           05  WS-UK-QTY               PIC Z,ZZZ,ZZ9.999999-.
           05  WS-UK-QTY2              PIC Z,ZZZ,ZZ9.999999-.
           05  WS-VER-ED               PIC 9(6).
      *>   so.SalesOrder header
       01  WS-OHDR.
           05  WS-H-ID                 PIC X(24).
           05  WS-H-CUST               PIC X(20).
           05  WS-H-CURR               PIC X(3).
           05  WS-H-FX                 PIC X(24).
           05  WS-H-SITE               PIC X(10).
           05  WS-H-COMM               PIC X(12).
           05  WS-H-REL                PIC X(14).
           05  WS-H-NET                PIC X(24).
           05  WS-H-VAT                PIC X(24).
           05  WS-H-GROSS              PIC X(24).
           05  WS-H-GROSSGBP           PIC X(24).
           05  WS-H-HASH               PIC X(64).
      *>   so.SalesOrderLine
       01  WS-LROW.
           05  WS-L-ID                 PIC X(24).
           05  WS-L-ITEM               PIC X(40).
           05  WS-L-REV                PIC X(4).
           05  WS-L-UOM                PIC X(10).
           05  WS-L-TYPE               PIC X(10).
           05  WS-L-ACCEPTED           PIC X(24).
           05  WS-L-CANCELLED          PIC X(24).
           05  WS-L-DESPATCHED         PIC X(24).
           05  WS-L-BILLED             PIC X(24).
           05  WS-L-RETURNED           PIC X(24).
           05  WS-L-PRICE              PIC X(24).
           05  WS-L-PPQ                PIC X(24).
           05  WS-L-D1                 PIC X(12).
           05  WS-L-D2                 PIC X(12).
           05  WS-L-NET                PIC X(24).
           05  WS-L-VATCODE            PIC X(4).
           05  WS-L-VATRATE            PIC X(12).
           05  WS-L-STATUS             PIC X(10).
      *>   open line numbers for CANCELORD.  OCCURS 200 since CR0255.
       01  WS-OPEN-TAB.
           05  WS-OT-CNT               PIC S9(4).
           05  WS-OT OCCURS 200 TIMES.
               10  WS-OT-NUM           PIC S9(9).
       01  WS-NROW.
           05  WS-N1                   PIC X(30).
           05  WS-N2                   PIC X(30).
       01  WS-TWO.
           05  WS-T1                   PIC X(30).
           05  WS-T2                   PIC X(30).
       LINKAGE SECTION.
       COPY SOAMENDCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE-INPUT
           IF WS-RC > 4
               GO TO 0000-EXIT
           END-IF
           PERFORM 0300-READ-ORDER
           IF WS-RC > 4
               GO TO 0000-EXIT
           END-IF
           EVALUATE WS-FUNC
               WHEN "AMENDQTY"   PERFORM 1000-AMEND-QUANTITY
               WHEN "AMENDPRI"   PERFORM 1100-AMEND-PRICE
               WHEN "AMENDDAT"   PERFORM 1200-AMEND-DATE
               WHEN "CANCELLN"   PERFORM 2000-CANCEL-LINE
               WHEN "CANCELOR"   PERFORM 2100-CANCEL-ORDER
               WHEN OTHER        PERFORM 9100-BAD-FUNCTION
           END-EVALUATE
           IF WS-RC > 6
               PERFORM 9500-ROLLBACK
           END-IF.
       0000-EXIT.
           PERFORM 9900-SET-RESULT
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE WS-CTL
           INITIALIZE WS-EDITS
           INITIALIZE WS-OHDR
           INITIALIZE WS-LROW
           INITIALIZE WS-TWO
           INITIALIZE WS-NROW
           MOVE 0 TO WS-OT-CNT
           MOVE "N" TO WS-TRAN-OPEN
           MOVE 0 TO WS-RC WS-MSGCNT
           MOVE SPACES TO WS-REASON WS-MSG
           MOVE CA-FUNCTION OF DFHCOMMAREA TO WS-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO WS-CO
           MOVE CA-SITE OF DFHCOMMAREA TO WS-SITE
           MOVE CA-USER OF DFHCOMMAREA TO WS-USER
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO WS-BUSDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO WS-CORREL
           MOVE "SOAMEND" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE "SO-016" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE 0 TO AM-APPLIED-QTY AM-RELEASED-QTY
           MOVE 0 TO AM-LINES-CANCELLED AM-APPROVALS-VOID
           MOVE 0 TO AM-NET-DELTA AM-AUDIT-ID
           MOVE SPACES TO AM-HOLD-REASON
           MOVE 0 TO WS-CANCEL-QTY WS-NEW-CANCELLED.
       0100-EXIT.
           EXIT.

       0200-VALIDATE-INPUT SECTION.
       0200-START.
           IF WS-CO = SPACES OR AM-ORDER-NUMBER = SPACES
               MOVE "AMD0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A company code and an order number are "
                      "required" DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF AM-REASON-TEXT = SPACES
               MOVE "AMD0009" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "An amendment reason is required so the change "
                      "can be explained later"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF WS-FUNC NOT = "CANCELOR" AND AM-LINE-NUM = 0
               MOVE "AMD0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A line number is required for this amendment"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       0300-READ-ORDER SECTION.
       0300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), o.SalesOrderId), "
                  "o.CustomerCode, o.CurrencyCode, "
                  "CONVERT(varchar(24), o.FxRate), o.SiteCode, "
                  "o.CommercialStatus, o.ReleaseStatus, "
                  "CONVERT(varchar(24), o.NetAmount), "
                  "CONVERT(varchar(24), o.VatAmount), "
                  "CONVERT(varchar(24), o.GrossAmount), "
                  "CONVERT(varchar(24), o.GrossAmountGBP), "
                  "ISNULL(o.ContentHash,'') FROM so.SalesOrder o "
                  "WHERE o.CompanyCode = ? AND o.OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE AM-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 12 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 10 TO SQLIO-COL-WIDTH(5)
           MOVE 12 TO SQLIO-COL-WIDTH(6)
           MOVE 14 TO SQLIO-COL-WIDTH(7)
           MOVE 24 TO SQLIO-COL-WIDTH(8)
           MOVE 24 TO SQLIO-COL-WIDTH(9)
           MOVE 24 TO SQLIO-COL-WIDTH(10)
           MOVE 24 TO SQLIO-COL-WIDTH(11)
           MOVE 64 TO SQLIO-COL-WIDTH(12)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-OHDR
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0300-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO WS-RC
               MOVE "AMD0002" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Order " FUNCTION TRIM(AM-ORDER-NUMBER)
                      " is not held for company "
                      FUNCTION TRIM(WS-CO)
                   DELIMITED BY SIZE INTO WS-MSG
               ADD 1 TO WS-MSGCNT
               GO TO 0300-EXIT
           END-IF
           COMPUTE AM-SALES-ORDER-ID = FUNCTION NUMVAL(WS-H-ID)
           MOVE WS-H-CUST TO AM-CUSTOMER
           MOVE WS-H-CURR TO AM-CURRENCY
           COMPUTE AM-FX-RATE = FUNCTION NUMVAL(WS-H-FX)
           MOVE WS-H-SITE TO AM-SITE-CODE
           MOVE WS-H-COMM TO AM-ORDER-STATUS
           MOVE WS-H-REL TO AM-RELEASE-STATUS
           COMPUTE AM-ORDER-NET = FUNCTION NUMVAL(WS-H-NET)
           MOVE WS-H-HASH TO AM-CONTENT-HASH
           IF AM-ORDER-STATUS = "CANCELLED" OR
              AM-ORDER-STATUS = "COMPLETED"
               MOVE "AMD0006" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Order " FUNCTION TRIM(AM-ORDER-NUMBER)
                      " is " FUNCTION TRIM(AM-ORDER-STATUS)
                      " and can no longer be amended"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
           END-IF.
       0300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   the line, and the floor below which it may not be reduced
       0400-READ-LINE SECTION.
       0400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), l.SalesOrderLineId), "
                  "ISNULL(l.ItemCode,''), ISNULL(l.Revision,''), "
                  "ISNULL(l.UomCode,''), l.LineType, "
                  "CONVERT(varchar(24), l.AcceptedQty), "
                  "CONVERT(varchar(24), l.CancelledQty), "
                  "CONVERT(varchar(24), l.DespatchedQty), "
                  "CONVERT(varchar(24), l.BilledQty), "
                  "CONVERT(varchar(24), l.ReturnedQty), "
                  "CONVERT(varchar(24), l.UnitPrice), "
                  "CONVERT(varchar(24), l.PricePerQty), "
                  "CONVERT(varchar(12), l.DiscountPct1), "
                  "CONVERT(varchar(12), l.DiscountPct2), "
                  "CONVERT(varchar(24), l.NetAmount), "
                  "ISNULL(l.VatCode,''), CONVERT(varchar(12), "
                  "ISNULL(l.VatRatePct,0)), l.Status FROM "
                  "so.SalesOrderLine l WHERE l.CompanyCode = ? AND "
                  "l.OrderNumber = ? AND l.LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE AM-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE AM-LINE-NUM TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           MOVE 18 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 4 TO SQLIO-COL-WIDTH(3)
           MOVE 10 TO SQLIO-COL-WIDTH(4)
           MOVE 10 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 24 TO SQLIO-COL-WIDTH(8)
           MOVE 24 TO SQLIO-COL-WIDTH(9)
           MOVE 24 TO SQLIO-COL-WIDTH(10)
           MOVE 24 TO SQLIO-COL-WIDTH(11)
           MOVE 24 TO SQLIO-COL-WIDTH(12)
           MOVE 12 TO SQLIO-COL-WIDTH(13)
           MOVE 12 TO SQLIO-COL-WIDTH(14)
           MOVE 24 TO SQLIO-COL-WIDTH(15)
           MOVE 4 TO SQLIO-COL-WIDTH(16)
           MOVE 12 TO SQLIO-COL-WIDTH(17)
           MOVE 10 TO SQLIO-COL-WIDTH(18)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 0400-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO WS-RC
               MOVE "AMD0002" TO WS-REASON
               MOVE AM-LINE-NUM TO WS-INT-ED
               MOVE SPACES TO WS-MSG
               STRING "Line " FUNCTION TRIM(WS-INT-ED) " of order "
                      FUNCTION TRIM(AM-ORDER-NUMBER)
                      " is not held" DELIMITED BY SIZE INTO WS-MSG
               ADD 1 TO WS-MSGCNT
               GO TO 0400-EXIT
           END-IF
           COMPUTE AM-ORDER-LINE-ID = FUNCTION NUMVAL(WS-L-ID)
           MOVE WS-L-ITEM TO AM-ITEM-CODE
           MOVE WS-L-REV TO AM-REVISION
           MOVE WS-L-UOM TO AM-UOM-CODE
           MOVE WS-L-TYPE TO AM-LINE-TYPE
           COMPUTE AM-ACCEPTED-QTY = FUNCTION NUMVAL(WS-L-ACCEPTED)
           COMPUTE AM-CANCELLED-QTY = FUNCTION NUMVAL(WS-L-CANCELLED)
           COMPUTE AM-DESPATCHED-QTY =
                   FUNCTION NUMVAL(WS-L-DESPATCHED)
           COMPUTE AM-BILLED-QTY = FUNCTION NUMVAL(WS-L-BILLED)
           COMPUTE AM-RETURNED-QTY = FUNCTION NUMVAL(WS-L-RETURNED)
           COMPUTE AM-OLD-PRICE = FUNCTION NUMVAL(WS-L-PRICE)
           COMPUTE AM-LINE-NET = FUNCTION NUMVAL(WS-L-NET)
           MOVE AM-ACCEPTED-QTY TO AM-OLD-QTY
           MOVE WS-L-STATUS TO AM-LINE-STATUS
           COMPUTE AM-UNFULFILLED-QTY = AM-ACCEPTED-QTY
                   - AM-CANCELLED-QTY - AM-DESPATCHED-QTY
           MOVE AM-UNFULFILLED-QTY TO AM-REMAIN-DESPATCH
      *>       SO-016 - the floor is whatever has actually happened
           MOVE AM-DESPATCHED-QTY TO AM-FLOOR-QTY
           IF AM-BILLED-QTY > AM-FLOOR-QTY
               MOVE AM-BILLED-QTY TO AM-FLOOR-QTY
           END-IF.
       0400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   AMENDQTY - the accepted quantity moves, within the floor
       1000-AMEND-QUANTITY SECTION.
       1000-START.
           PERFORM 0400-READ-LINE
           IF WS-RC > 4
               GO TO 1000-EXIT
           END-IF
           IF AM-NEW-QTY <= 0
               MOVE "AMD0010" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "An amended quantity must be positive - use "
                      "CANCELLN to withdraw a line"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 1000-EXIT
           END-IF
           IF AM-NEW-QTY < AM-FLOOR-QTY
               PERFORM 9200-BELOW-THE-FLOOR
               GO TO 1000-EXIT
           END-IF
           PERFORM 5000-EXTEND-LINE
           PERFORM 8000-BEGIN-TRAN
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrderLine SET AcceptedQty = "
                  "CONVERT(decimal(19,6), ?), NetAmount = "
                  "CONVERT(decimal(18,2), ?), VatAmount = "
                  "CONVERT(decimal(18,2), ?) WHERE "
                  "SalesOrderLineId = ? AND DespatchedQty <= "
                  "CONVERT(decimal(19,6), ?) AND BilledQty <= "
                  "CONVERT(decimal(19,6), ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE AM-NEW-QTY TO WS-QTY-ED
           MOVE FUNCTION TRIM(WS-QTY-ED) TO SQLIO-PARM-VAL(1)
           MOVE WS-NEW-NET TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(2)
           MOVE WS-NEW-VAT TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(3)
           MOVE AM-ORDER-LINE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE AM-NEW-QTY TO WS-QTY-ED
           MOVE FUNCTION TRIM(WS-QTY-ED) TO SQLIO-PARM-VAL(5)
           MOVE FUNCTION TRIM(WS-QTY-ED) TO SQLIO-PARM-VAL(6)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS
           IF WS-RC > 4
               GO TO 1000-EXIT
           END-IF
           PERFORM 5100-REFRESH-HEADER
           PERFORM 4200-BUMP-VERSION
           IF WS-RC > 4
               GO TO 1000-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           MOVE AM-NEW-QTY TO AM-APPLIED-QTY
           COMPUTE AM-NET-DELTA = WS-NEW-NET - AM-LINE-NET
           IF AM-NEW-QTY < AM-OLD-QTY
               COMPUTE WS-CANCEL-QTY = AM-OLD-QTY - AM-NEW-QTY
               PERFORM 3000-RELEASE-RESERVATIONS
           END-IF
           PERFORM 4000-RECHECK-CREDIT
           PERFORM 4100-INVALIDATE-APPROVALS
           PERFORM 7500-AUDIT-THE-CHANGE
           MOVE AM-OLD-QTY TO WS-UK-QTY
           MOVE AM-NEW-QTY TO WS-UK-QTY2
           MOVE SPACES TO WS-MSG
           STRING "Line quantity amended from "
                  FUNCTION TRIM(WS-UK-QTY) " to "
                  FUNCTION TRIM(WS-UK-QTY2)
                  "; despatched and billed quantities are unchanged"
               DELIMITED BY SIZE INTO WS-MSG.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   AMENDPRICE - after acceptance the customer has a price, so
      *>   changing it needs a named approval (SO-006, SO-016)
       1100-AMEND-PRICE SECTION.
       1100-START.
           PERFORM 0400-READ-LINE
           IF WS-RC > 4
               GO TO 1100-EXIT
           END-IF
           IF AM-NEW-PRICE < 0
               MOVE "AMD0010" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "An amended price cannot be negative"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 1100-EXIT
           END-IF
           IF AM-ORDER-STATUS NOT = "DRAFT" AND AM-APPROVAL-ID = 0
               MOVE "AMD0004" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A price amendment after acceptance needs an "
                      "approval id - the customer already has this "
                      "price" DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 1100-EXIT
           END-IF
           MOVE AM-ACCEPTED-QTY TO AM-NEW-QTY
           PERFORM 5000-EXTEND-LINE
           PERFORM 8000-BEGIN-TRAN
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrderLine SET UnitPrice = "
                  "CONVERT(decimal(19,6), ?), NetAmount = "
                  "CONVERT(decimal(18,2), ?), VatAmount = "
                  "CONVERT(decimal(18,2), ?), PriceSource = "
                  "'APPROVED', MarginApprovalId = NULLIF(CONVERT("
                  "bigint, ?),0) WHERE SalesOrderLineId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE AM-NEW-PRICE TO WS-PRICE-ED
           MOVE FUNCTION TRIM(WS-PRICE-ED) TO SQLIO-PARM-VAL(1)
           MOVE WS-NEW-NET TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(2)
           MOVE WS-NEW-VAT TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(3)
           MOVE AM-APPROVAL-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE AM-ORDER-LINE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS
           IF WS-RC > 4
               GO TO 1100-EXIT
           END-IF
           PERFORM 5100-REFRESH-HEADER
           PERFORM 4200-BUMP-VERSION
           IF WS-RC > 4
               GO TO 1100-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           COMPUTE AM-NET-DELTA = WS-NEW-NET - AM-LINE-NET
           PERFORM 4000-RECHECK-CREDIT
           PERFORM 4100-INVALIDATE-APPROVALS
           PERFORM 7500-AUDIT-THE-CHANGE
           MOVE AM-OLD-PRICE TO WS-UK-MONEY
           MOVE SPACES TO WS-MSG
           STRING "Unit price amended from "
                  FUNCTION TRIM(WS-UK-MONEY)
                  " under approval " DELIMITED BY SIZE INTO WS-MSG
           MOVE AM-APPROVAL-ID TO WS-INT-ED
           STRING FUNCTION TRIM(WS-MSG) " "
                  FUNCTION TRIM(WS-INT-ED)
               DELIMITED BY SIZE INTO WS-MSG.
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   AMENDDATE - only CurrentPromiseDate moves.  RequestedDate
      *>   and OriginalPromiseDate stay out of the SET list.
       1200-AMEND-DATE SECTION.
       1200-START.
           PERFORM 0400-READ-LINE
           IF WS-RC > 4
               GO TO 1200-EXIT
           END-IF
           IF AM-NEW-DATE = 0
               MOVE "AMD0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A new promise date is required"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 1200-EXIT
           END-IF
           PERFORM 8000-BEGIN-TRAN
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.DeliverySchedule SET "
                  "CurrentPromiseDate = CONVERT(date, ?, 112), "
                  "ChangeReason = ?, ChangeCount = ChangeCount + 1 "
                  "WHERE SalesOrderLineId = ? AND Status <> "
                  "'CANCELLED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE AM-NEW-DATE TO SQLIO-PARM-VAL(1)
           MOVE AM-REASON-TEXT TO SQLIO-PARM-VAL(2)
           MOVE AM-ORDER-LINE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS
           IF WS-RC > 4
               GO TO 1200-EXIT
           END-IF
           PERFORM 4200-BUMP-VERSION
           IF WS-RC > 4
               GO TO 1200-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           PERFORM 4000-RECHECK-CREDIT
           PERFORM 4100-INVALIDATE-APPROVALS
           PERFORM 7500-AUDIT-THE-CHANGE
           MOVE SPACES TO WS-MSG
           STRING "Promise date amended to " AM-NEW-DATE
                  "; the requested and original promise dates are "
                  "retained" DELIMITED BY SIZE INTO WS-MSG.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   CANCELLN - SO-016.  Only the unfulfilled balance moves.
      *>   DespatchedQty and BilledQty are not in the SET list at all,
      *>   so a cancellation can never undo a despatch or an invoice.
       2000-CANCEL-LINE SECTION.
       2000-START.
           PERFORM 0400-READ-LINE
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-DECIDE-CANCEL-QTY
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           PERFORM 8000-BEGIN-TRAN
           PERFORM 2300-POST-CANCELLATION
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           PERFORM 5100-REFRESH-HEADER
           PERFORM 4200-BUMP-VERSION
           IF WS-RC > 4
               GO TO 2000-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           PERFORM 3000-RELEASE-RESERVATIONS
           PERFORM 4000-RECHECK-CREDIT
           PERFORM 4100-INVALIDATE-APPROVALS
           PERFORM 7500-AUDIT-THE-CHANGE
           MOVE WS-CANCEL-QTY TO WS-UK-QTY
           MOVE AM-DESPATCHED-QTY TO WS-UK-QTY2
           MOVE SPACES TO WS-MSG
           STRING FUNCTION TRIM(WS-UK-QTY)
                  " unfulfilled units cancelled; "
                  FUNCTION TRIM(WS-UK-QTY2)
                  " already despatched are untouched"
               DELIMITED BY SIZE INTO WS-MSG.
       2000-EXIT.
           EXIT.

       2200-DECIDE-CANCEL-QTY SECTION.
       2200-START.
           IF AM-CANCEL-QTY = 0
               MOVE AM-UNFULFILLED-QTY TO WS-CANCEL-QTY
           ELSE
               MOVE AM-CANCEL-QTY TO WS-CANCEL-QTY
           END-IF
           IF WS-CANCEL-QTY <= 0
               MOVE 4 TO WS-RC
               MOVE "AMD0005" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "There is no unfulfilled balance left to "
                      "cancel on this line"
                   DELIMITED BY SIZE INTO WS-MSG
               ADD 1 TO WS-MSGCNT
               GO TO 2200-EXIT
           END-IF
           IF WS-CANCEL-QTY > AM-UNFULFILLED-QTY
               MOVE "AMD0005" TO WS-REASON
               MOVE WS-CANCEL-QTY TO WS-UK-QTY
               MOVE AM-UNFULFILLED-QTY TO WS-UK-QTY2
               MOVE SPACES TO WS-MSG
               STRING "A cancellation of " FUNCTION TRIM(WS-UK-QTY)
                      " exceeds the unfulfilled balance of "
                      FUNCTION TRIM(WS-UK-QTY2)
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT
               GO TO 2200-EXIT
           END-IF
           COMPUTE WS-NEW-CANCELLED =
                   AM-CANCELLED-QTY + WS-CANCEL-QTY
           COMPUTE AM-REMAIN-DESPATCH = AM-ACCEPTED-QTY
                   - WS-NEW-CANCELLED - AM-DESPATCHED-QTY
           IF AM-REMAIN-DESPATCH > 0
               MOVE "OPEN" TO WS-LINE-STATUS
           ELSE
               IF AM-DESPATCHED-QTY > 0
                   MOVE "CLOSED" TO WS-LINE-STATUS
               ELSE
                   MOVE "CANCELLED" TO WS-LINE-STATUS
               END-IF
           END-IF.
       2200-EXIT.
           EXIT.

       2300-POST-CANCELLATION SECTION.
       2300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrderLine SET CancelledQty = "
                  "CONVERT(decimal(19,6), ?), Status = ? WHERE "
                  "SalesOrderLineId = ? AND AcceptedQty - "
                  "CONVERT(decimal(19,6), ?) - DespatchedQty >= 0"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE WS-NEW-CANCELLED TO WS-QTY-ED
           MOVE FUNCTION TRIM(WS-QTY-ED) TO SQLIO-PARM-VAL(1)
           MOVE WS-LINE-STATUS TO SQLIO-PARM-VAL(2)
           MOVE AM-ORDER-LINE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE WS-NEW-CANCELLED TO WS-QTY-ED
           MOVE FUNCTION TRIM(WS-QTY-ED) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS
           IF WS-RC > 4
               GO TO 2300-EXIT
           END-IF
           MOVE WS-NEW-CANCELLED TO AM-CANCELLED-QTY
           MOVE WS-LINE-STATUS TO AM-LINE-STATUS
           ADD 1 TO AM-LINES-CANCELLED
      *>       the cancelled quantity is withdrawn from the open
      *>       schedules, newest first, so a part despatched schedule
      *>       keeps what it has already shipped
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.DeliverySchedule SET Quantity = "
                  "DespatchedQty, Status = CASE WHEN DespatchedQty > "
                  "0 THEN 'COMPLETE' ELSE 'CANCELLED' END, "
                  "ChangeReason = ? WHERE SalesOrderLineId = ? AND "
                  "Status NOT IN ('CANCELLED','COMPLETE') AND "
                  "Quantity > DespatchedQty AND ? = 'ALL'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE AM-REASON-TEXT TO SQLIO-PARM-VAL(1)
           MOVE AM-ORDER-LINE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(2)
           IF AM-REMAIN-DESPATCH = 0
               MOVE "ALL" TO SQLIO-PARM-VAL(3)
           ELSE
               MOVE "PART" TO SQLIO-PARM-VAL(3)
           END-IF
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS.
       2300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   CANCELORD - every open line's unfulfilled balance, then the
      *>   header.  Despatched and billed history is left alone.
       2100-CANCEL-ORDER SECTION.
       2100-START.
           PERFORM 2400-LIST-OPEN-LINES
           IF WS-RC > 4
               GO TO 2100-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-OT-CNT OR WS-RC > 4
               MOVE WS-OT-NUM(WS-I) TO AM-LINE-NUM
               MOVE 0 TO AM-CANCEL-QTY
               PERFORM 0400-READ-LINE
               IF WS-RC < 8
                   PERFORM 2200-DECIDE-CANCEL-QTY
                   IF WS-RC < 8 AND WS-CANCEL-QTY > 0
                       PERFORM 8000-BEGIN-TRAN
                       PERFORM 2300-POST-CANCELLATION
                       IF WS-RC < 8
                           PERFORM 8100-COMMIT-TRAN
                           PERFORM 3000-RELEASE-RESERVATIONS
                       END-IF
                   END-IF
                   IF WS-RC = 4
                       MOVE 0 TO WS-RC
                       MOVE SPACES TO WS-REASON
                   END-IF
               END-IF
           END-PERFORM
           IF WS-RC > 4
               GO TO 2100-EXIT
           END-IF
           PERFORM 8000-BEGIN-TRAN
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrder SET CommercialStatus = "
                  "'CANCELLED', HoldReason = ?, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE CompanyCode = ? "
                  "AND OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE AM-REASON-TEXT TO SQLIO-PARM-VAL(1)
           MOVE WS-USER TO SQLIO-PARM-VAL(2)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           MOVE AM-ORDER-NUMBER TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS
           IF WS-RC > 4
               GO TO 2100-EXIT
           END-IF
           PERFORM 5100-REFRESH-HEADER
           PERFORM 4200-BUMP-VERSION
           IF WS-RC > 4
               GO TO 2100-EXIT
           END-IF
           PERFORM 8100-COMMIT-TRAN
           MOVE "CANCELLED" TO AM-ORDER-STATUS
           PERFORM 4000-RECHECK-CREDIT
           PERFORM 4100-INVALIDATE-APPROVALS
           PERFORM 7500-AUDIT-THE-CHANGE
           MOVE AM-LINES-CANCELLED TO WS-INT-ED
           MOVE SPACES TO WS-MSG
           STRING "Order cancelled on " FUNCTION TRIM(WS-INT-ED)
                  " line(s); despatches, invoices and supplier "
                  "commitments are not reversed"
               DELIMITED BY SIZE INTO WS-MSG.
       2100-EXIT.
           EXIT.

       2400-LIST-OPEN-LINES SECTION.
       2400-START.
           MOVE 0 TO WS-OT-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(10), l.LineNum), '' FROM "
                  "so.SalesOrderLine l WHERE l.CompanyCode = ? AND "
                  "l.OrderNumber = ? AND l.Status = 'OPEN' ORDER BY "
                  "l.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE AM-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-NROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2400-EXIT
           END-IF
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-NROW
           PERFORM UNTIL SQLIO-SQLCODE NOT = 0 OR WS-OT-CNT >= 200
               ADD 1 TO WS-OT-CNT
               COMPUTE WS-OT-NUM(WS-OT-CNT) =
                       FUNCTION NUMVAL(WS-N1)
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-NROW
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-NROW.
       2400-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   CR0981 - release ONLY what the cancelled quantity was
      *>   holding.  STKRESV owns the reservation arithmetic; a
      *>   consumed or picked reservation is not ours to undo.
       3000-RELEASE-RESERVATIONS SECTION.
       3000-START.
           MOVE 0 TO AM-RELEASED-QTY
           IF WS-CANCEL-QTY <= 0 OR AM-ITEM-CODE = SPACES
               GO TO 3000-EXIT
           END-IF
           INITIALIZE WS-RESV-CA
           MOVE "RELEASE" TO CA-FUNCTION OF WS-RESV-CA
           MOVE WS-CO TO CA-COMPANY OF WS-RESV-CA
           MOVE AM-SITE-CODE TO CA-SITE OF WS-RESV-CA
           MOVE WS-USER TO CA-USER OF WS-RESV-CA
           MOVE WS-BUSDATE TO CA-BUSINESS-DATE OF WS-RESV-CA
           MOVE WS-CORREL TO CA-CORRELATION-ID OF WS-RESV-CA
           MOVE AM-ITEM-CODE TO SR-ITEM-CODE
           MOVE AM-REVISION TO SR-REVISION
           MOVE AM-SITE-CODE TO SR-SITE
           MOVE "SALESORDER" TO SR-DEMAND-TYPE
           MOVE AM-ORDER-NUMBER TO SR-DEMAND-REF
           MOVE AM-LINE-NUM TO SR-DEMAND-LINE
           MOVE WS-CANCEL-QTY TO SR-QTY-REQUESTED
           MOVE "Y" TO SR-ALLOW-PARTIAL
           CALL "STKRESV" USING WS-RESV-CA
           MOVE SR-QTY-RESERVED TO AM-RESERVED-BEFORE
           IF CA-RETURN-CODE OF WS-RESV-CA < 8
               MOVE WS-CANCEL-QTY TO AM-RELEASED-QTY
           ELSE
               IF WS-RC < 4
                   MOVE 4 TO WS-RC
                   MOVE "AMD0005" TO WS-REASON
                   MOVE CA-MESSAGE OF WS-RESV-CA TO WS-MSG
                   ADD 1 TO WS-MSGCNT
               END-IF
           END-IF.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   SO-007 - every amendment re-asks the credit question.  The
      *>   answer is reported, and a worsened position holds the order
      *>   rather than silently letting the change through.
       4000-RECHECK-CREDIT SECTION.
       4000-START.
           INITIALIZE WS-CRED-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-CRED-CA
           MOVE WS-CO TO CA-COMPANY OF WS-CRED-CA
           MOVE AM-SITE-CODE TO CA-SITE OF WS-CRED-CA
           MOVE WS-USER TO CA-USER OF WS-CRED-CA
           MOVE WS-BUSDATE TO CA-BUSINESS-DATE OF WS-CRED-CA
           MOVE WS-CORREL TO CA-CORRELATION-ID OF WS-CRED-CA
           MOVE AM-CUSTOMER TO CK-CUSTOMER
           MOVE "AMEND" TO CK-CHECK-POINT
           MOVE AM-ORDER-NUMBER TO CK-DOCUMENT-REF
           MOVE AM-ORDER-NUMBER TO CK-EXCLUDE-REF
           MOVE WS-BUSDATE TO CK-EVAL-DATE
           MOVE AM-CURRENCY TO CK-CURRENCY
           MOVE AM-FX-RATE TO CK-FX-RATE
           MOVE AM-ORDER-NET TO CK-ORDER-VALUE
           MOVE AM-ORDER-NET TO CK-ORDER-VALUE-GBP
           CALL "CREDCHK" USING WS-CRED-CA
           MOVE CK-DECISION TO AM-CREDIT-DECISION
           MOVE CK-EXPOSURE TO AM-CREDIT-EXPOSURE
           MOVE CK-LIMIT TO AM-CREDIT-LIMIT
           MOVE CK-HEADROOM TO AM-CREDIT-HEADROOM
           IF CA-RETURN-CODE OF WS-CRED-CA > 11
               GO TO 4000-EXIT
           END-IF
           IF CK-HELD OR CK-BLOCKED
               MOVE "CREDITHOLD" TO AM-RELEASE-STATUS
               MOVE CK-DECISION-TEXT TO AM-HOLD-REASON
               IF AM-HOLD-REASON = SPACES
                   MOVE CK-HOLD-REASON TO AM-HOLD-REASON
               END-IF
               PERFORM 4050-HOLD-THE-ORDER
           END-IF.
       4000-EXIT.
           EXIT.

       4050-HOLD-THE-ORDER SECTION.
       4050-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrder SET ReleaseStatus = "
                  "'CREDITHOLD', HoldReason = ?, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE CompanyCode = ? "
                  "AND OrderNumber = ? AND CommercialStatus <> "
                  "'CANCELLED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE AM-HOLD-REASON TO SQLIO-PARM-VAL(1)
           MOVE WS-USER TO SQLIO-PARM-VAL(2)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           MOVE AM-ORDER-NUMBER TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS
           IF WS-RC < 6
               MOVE 6 TO WS-RC
               MOVE "AMD0007" TO WS-REASON
               MOVE AM-HOLD-REASON TO WS-MSG
               ADD 1 TO WS-MSGCNT
           END-IF.
       4050-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   CR2901 - an approval was given for particular content.  A
      *>   changed document invalidates it, so the decision has to be
      *>   taken again against what the order now says.
      *>   The procedure can THROW, so it runs outside our own
      *>   transaction and never dooms it.
       4100-INVALIDATE-APPROVALS SECTION.
       4100-START.
           IF WS-RC > 6
               GO TO 4100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC core.usp_InvalidateApprovals ?, ?, ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE "SALESORDER" TO SQLIO-PARM-VAL(2)
           MOVE AM-ORDER-NUMBER TO SQLIO-PARM-VAL(3)
           MOVE AM-CONTENT-HASH TO SQLIO-PARM-VAL(4)
           MOVE WS-USER TO SQLIO-PARM-VAL(5)
           MOVE AM-REASON-TEXT TO SQLIO-PARM-VAL(6)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE 0 TO AM-APPROVALS-VOID
           ELSE
               COMPUTE AM-APPROVALS-VOID = FUNCTION NUMVAL(WS-T1)
           END-IF.
       4100-EXIT.
           EXIT.

      *>   SO-022 - the document version is carried in ContentHash as
      *>   Vnnnnnn so every amendment is reproducible from history.
      *>   (Kept as text since CR2901 rather than adding a column.)
       4200-BUMP-VERSION SECTION.
       4200-START.
           IF WS-RC > 4
               GO TO 4200-EXIT
           END-IF
           MOVE 1 TO WS-OLD-VERSION
           IF AM-CONTENT-HASH(1:1) = "V"
               MOVE AM-CONTENT-HASH(2:6) TO WS-HASH-NUM
               MOVE WS-HASH-NUM TO WS-OLD-VERSION
           END-IF
           COMPUTE AM-DOC-VERSION = WS-OLD-VERSION + 1
           MOVE AM-DOC-VERSION TO WS-VER-ED
           MOVE SPACES TO AM-CONTENT-HASH
           MOVE AM-ORDER-NET TO WS-MONEY-ED
           STRING "V" WS-VER-ED "-" FUNCTION TRIM(WS-MONEY-ED)
                  "-" FUNCTION TRIM(WS-FUNC)
               DELIMITED BY SIZE INTO AM-CONTENT-HASH
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrder SET ContentHash = ?, "
                  "CommercialStatus = CASE WHEN CommercialStatus = "
                  "'ACCEPTED' THEN 'AMENDED' ELSE CommercialStatus "
                  "END, ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE CompanyCode = ? AND OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE AM-CONTENT-HASH TO SQLIO-PARM-VAL(1)
           MOVE WS-USER TO SQLIO-PARM-VAL(2)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           MOVE AM-ORDER-NUMBER TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-P FROM 1 BY 1 UNTIL WS-P > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-P)
           END-PERFORM
           PERFORM 8200-EXEC-NO-ROWS.
       4200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *>   line extension: quantity to the price unit, extend by
      *>   price-per, then the percentage discounts in order
       5000-EXTEND-LINE SECTION.
       5000-START.
           COMPUTE WS-PPQ = FUNCTION NUMVAL(WS-L-PPQ)
           IF WS-PPQ = 0
               MOVE 1 TO WS-PPQ
           END-IF
           IF WS-FUNC = "AMENDPRI"
               CONTINUE
           ELSE
               MOVE AM-OLD-PRICE TO AM-NEW-PRICE
           END-IF
           COMPUTE WS-AMT1 ROUNDED =
                   AM-NEW-QTY / WS-PPQ * AM-NEW-PRICE
           COMPUTE WS-AMT2 ROUNDED = WS-AMT1 *
                   (1 - FUNCTION NUMVAL(WS-L-D1) / 100)
           COMPUTE WS-NEW-NET ROUNDED = WS-AMT2 *
                   (1 - FUNCTION NUMVAL(WS-L-D2) / 100)
           COMPUTE WS-NEW-VAT ROUNDED =
                   WS-NEW-NET * FUNCTION NUMVAL(WS-L-VATRATE) / 100.
       5000-EXIT.
           EXIT.

      *>   the header totals are always the sum of the live lines
       5100-REFRESH-HEADER SECTION.
       5100-START.
           IF WS-RC > 4
               GO TO 5100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE o SET NetAmount = t.N, VatAmount = t.V, "
                  "GrossAmount = t.N + t.V, GrossAmountGBP = "
                  "CONVERT(decimal(18,2), (t.N + t.V) / o.FxRate) "
                  "FROM so.SalesOrder o CROSS APPLY (SELECT "
                  "ISNULL(SUM(l.NetAmount),0) AS N, "
                  "ISNULL(SUM(l.VatAmount),0) AS V FROM "
                  "so.SalesOrderLine l WHERE l.SalesOrderId = "
                  "o.SalesOrderId AND l.Status <> 'CANCELLED') t "
                  "WHERE o.CompanyCode = ? AND o.OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE AM-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           PERFORM 8200-EXEC-NO-ROWS.
       5100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       7500-AUDIT-THE-CHANGE SECTION.
       7500-START.
           IF WS-RC > 6
               GO TO 7500-EXIT
           END-IF
           INITIALIZE WS-AUD-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUD-CA
           MOVE WS-CO TO CA-COMPANY OF WS-AUD-CA
           MOVE AM-SITE-CODE TO CA-SITE OF WS-AUD-CA
           MOVE WS-USER TO CA-USER OF WS-AUD-CA
           MOVE WS-BUSDATE TO CA-BUSINESS-DATE OF WS-AUD-CA
           MOVE WS-CORREL TO CA-CORRELATION-ID OF WS-AUD-CA
           MOVE "so.SalesOrderLine" TO AU-ENTITY-TYPE
           MOVE AM-LINE-NUM TO WS-INT-ED
           MOVE SPACES TO AU-ENTITY-KEY
           STRING FUNCTION TRIM(WS-CO) "/"
                  FUNCTION TRIM(AM-ORDER-NUMBER) "/"
                  FUNCTION TRIM(WS-INT-ED)
               DELIMITED BY SIZE INTO AU-ENTITY-KEY
           MOVE WS-FUNC TO AU-ACTION
           EVALUATE WS-FUNC
               WHEN "AMENDPRI"
                   MOVE "UnitPrice" TO AU-FIELD-NAME
                   MOVE AM-OLD-PRICE TO WS-PRICE-ED
                   MOVE WS-PRICE-ED TO AU-OLD-VALUE
                   MOVE AM-NEW-PRICE TO WS-PRICE-ED
                   MOVE WS-PRICE-ED TO AU-NEW-VALUE
               WHEN "CANCELLN"
                   MOVE "CancelledQty" TO AU-FIELD-NAME
                   MOVE AM-CANCELLED-QTY TO WS-QTY-ED
                   MOVE WS-QTY-ED TO AU-NEW-VALUE
               WHEN "CANCELOR"
                   MOVE "CommercialStatus" TO AU-FIELD-NAME
                   MOVE "CANCELLED" TO AU-NEW-VALUE
               WHEN OTHER
                   MOVE "AcceptedQty" TO AU-FIELD-NAME
                   MOVE AM-OLD-QTY TO WS-QTY-ED
                   MOVE WS-QTY-ED TO AU-OLD-VALUE
                   MOVE AM-NEW-QTY TO WS-QTY-ED
                   MOVE WS-QTY-ED TO AU-NEW-VALUE
           END-EVALUATE
           MOVE AM-REASON-TEXT TO AU-REASON
           MOVE SPACES TO AU-AUTHORITY-REF
           IF AM-APPROVAL-ID NOT = 0
               MOVE AM-APPROVAL-ID TO WS-INT-ED
               STRING "APPROVAL " FUNCTION TRIM(WS-INT-ED)
                   DELIMITED BY SIZE INTO AU-AUTHORITY-REF
           END-IF
           MOVE "SOAMEND" TO AU-SOURCE
           MOVE "N" TO AU-SENSITIVE
           CALL "AUDITLOG" USING WS-AUD-CA
           MOVE AU-AUDIT-ID TO AM-AUDIT-ID.
       7500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-BEGIN-TRAN SECTION.
       8000-START.
           IF WS-RC > 4 OR WS-TRAN-OPEN = "Y"
               GO TO 8000-EXIT
           END-IF
           MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
           ELSE
               MOVE "Y" TO WS-TRAN-OPEN
           END-IF.
       8000-EXIT.
           EXIT.

       8100-COMMIT-TRAN SECTION.
       8100-START.
           IF WS-TRAN-OPEN NOT = "Y"
               GO TO 8100-EXIT
           END-IF
           MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
           MOVE "COMT" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
           END-IF
           MOVE "N" TO WS-TRAN-OPEN.
       8100-EXIT.
           EXIT.

       8200-EXEC-NO-ROWS SECTION.
       8200-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 8200-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO.
       8200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE 8 TO WS-RC
           ADD 1 TO WS-MSGCNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE 8 TO WS-RC
           MOVE "AMD0008" TO WS-REASON
           MOVE SPACES TO WS-MSG
           STRING "SOAMEND does not support function ["
                  FUNCTION TRIM(WS-FUNC) "]"
               DELIMITED BY SIZE INTO WS-MSG
           ADD 1 TO WS-MSGCNT.
       9100-EXIT.
           EXIT.

      *>   AMD0003 - the whole of SO-016 in one message
       9200-BELOW-THE-FLOOR SECTION.
       9200-START.
           MOVE "AMD0003" TO WS-REASON
           MOVE AM-NEW-QTY TO WS-UK-QTY
           MOVE AM-FLOOR-QTY TO WS-UK-QTY2
           MOVE SPACES TO WS-MSG
           STRING "A quantity of " FUNCTION TRIM(WS-UK-QTY)
                  " is below the " FUNCTION TRIM(WS-UK-QTY2)
                  " already despatched or billed on this line"
               DELIMITED BY SIZE INTO WS-MSG
           PERFORM 9000-REJECT.
       9200-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE 12 TO WS-RC
           MOVE "SQL0001" TO WS-REASON
           MOVE SQLIO-ERRMSG TO WS-MSG
           ADD 1 TO WS-MSGCNT.
       9300-EXIT.
           EXIT.

       9500-ROLLBACK SECTION.
       9500-START.
           IF WS-TRAN-OPEN = "Y"
               MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-TWO
               MOVE "N" TO WS-TRAN-OPEN
           END-IF.
       9500-EXIT.
           EXIT.

       9900-SET-RESULT SECTION.
       9900-START.
           MOVE WS-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE WS-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE WS-MSGCNT TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE AM-LINE-STATUS TO AM-FULFIL-STATUS.
       9900-EXIT.
           EXIT.
