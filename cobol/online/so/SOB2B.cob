      *>================================================================
      *> PROGRAM-ID : SOB2B
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : BACK TO BACK SUPPLY AND DROP SHIPMENT.
      *>              LINK TIES A SALES ORDER LINE TO A SPECIFIC
      *>              PURCHASE ORDER LINE OR ASSEMBLY COMMITMENT.  THE
      *>              LINK IS EVIDENCE OF INTENT: IT DOES NOT BY
      *>              ITSELF GUARANTEE DELIVERY AND IT DOES NOT
      *>              AUTHORISE AN UNAPPROVED SUPPLIER INVOICE.
      *>              SUPPCHG IDENTIFIES THE AFFECTED CUSTOMER
      *>              SCHEDULES FOR REVIEW AND RAISES AN EXCEPTION; IT
      *>              NEVER SILENTLY CHANGES THE CUSTOMER PROMISE.
      *>              DIRECTDL RECORDS CONFIRMED DIRECT DELIVERY
      *>              EVIDENCE.  NO WAREHOUSE ON HAND QUANTITY IS
      *>              CREATED - NO STOCK MOVEMENT IS MADE AT ALL.  THE
      *>              DIRECT FULFILMENT COST IS ACCRUED TO COGS
      *>              AGAINST GRNI THROUGH fin.JournalStage AND
      *>              fin.usp_PostJournal, RECOGNISED ON THE SUPPLIER
      *>              CONFIRMED DELIVERY DATE (D-03), AND LATER
      *>              CLEARED BY THE SUPPLIER INVOICE.  CUSTOMER
      *>              INVOICING AND SUPPLIER SETTLEMENT REMAIN
      *>              SEPARATE OBLIGATIONS.
      *> CALLED BY  : STERP DESKTOP CLIENT (LINKED SUPPLY), PO RECEIPT
      *> INTERFACE  : CALL "SOB2B" USING DFHCOMMAREA (COPY SOB2BCA)
      *> CALLS      : NUMGEN, PARMGET, EXCPRAIS, SQLIO ->
      *>              so.SalesOrderLine, so.DeliverySchedule,
      *>              so.Despatch, fin.JournalStage/usp_PostJournal
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-07-23 RJH  CR0498 BACK TO BACK PURCHASE LINKS
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS ON PROMISE DATES
      *> 2008-02-11 DJM  CR1750 DIRECT DELIVERY ACCRUAL TO GRNI
      *> 2026-10-06 STERP SO-021 D-03 DROP SHIP OWNERSHIP POLICY
      *>================================================================
      *> REQ: SO-021
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOB2B.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 23/07/1996.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARM-CA==.
       COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==WS-NUM-CA==.
       COPY EXCPRAISCA REPLACING ==DFHCOMMAREA== BY ==WS-EXC-CA==.
       01  WS-CONTROL.
           05  WS-FUNC                 PIC X(8).
           05  WS-CO                   PIC X(10).
           05  WS-SITE                 PIC X(10).
           05  WS-USER                 PIC X(20).
           05  WS-BDATE                PIC 9(8).
           05  WS-CORR                 PIC X(36).
           05  WS-POST-CORR            PIC X(64).
           05  WS-ERROR                PIC X VALUE "N".
           05  WS-IN-TXN               PIC X VALUE "N".
           05  WS-RC                   PIC 9(2) VALUE 0.
           05  WS-REASON               PIC X(8).
           05  WS-MSG                  PIC X(200).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-N                    PIC S9(15)V9(6) VALUE 0.
           05  WS-ROWS                 PIC S9(4) VALUE 0.
           05  WS-AMT1                 PIC S9(13)V99 VALUE 0.
           05  WS-DESPATCH-ID          PIC S9(17) VALUE 0.
           05  WS-QTY-ED               PIC -(9)9.9(6).
           05  WS-AMT-ED               PIC -(11)9.99.
           05  WS-INT-ED               PIC -(8)9.
           05  WS-DATE-ED              PIC X(10).
           05  WS-COGS-CC              PIC X(20) VALUE SPACES.
           05  WS-GRNI-CC              PIC X(20) VALUE SPACES.
           05  WS-LEG-CC               PIC X(20) VALUE SPACES.
           05  WS-LEG-CODE             PIC X(8) VALUE SPACES.
       01  WS-LINE-ROW.
           05  WB-LINE-ID              PIC X(24).
           05  WB-CUSTOMER             PIC X(20).
           05  WB-ITEM                 PIC X(40).
           05  WB-DESC                 PIC X(60).
           05  WB-UOM                  PIC X(10).
           05  WB-ACCEPTED             PIC X(24).
           05  WB-CANCELLED            PIC X(24).
           05  WB-DESPATCHED           PIC X(24).
           05  WB-SUPPLY-TYPE          PIC X(12).
           05  WB-PO-NUMBER            PIC X(30).
           05  WB-PO-LINE              PIC X(12).
           05  WB-KIT-REF              PIC X(30).
           05  WB-STATUS               PIC X(10).
           05  WB-SITE                 PIC X(10).
           05  WB-COST                 PIC X(24).
       01  WS-SCHED-ROW.
           05  WS-S-ORDER              PIC X(30).
           05  WS-S-LINE               PIC X(12).
           05  WS-S-SCHEDULE           PIC X(12).
           05  WS-S-CUSTOMER           PIC X(20).
           05  WS-S-ITEM               PIC X(40).
           05  WS-S-QTY                PIC X(24).
           05  WS-S-DESP               PIC X(24).
           05  WS-S-ORIG               PIC X(8).
           05  WS-S-CURR               PIC X(8).
           05  WS-S-STATUS             PIC X(10).
       01  WS-POST-ROW.
           05  WJ-ID                   PIC X(24).
           05  WJ-NUMBER               PIC X(30).
           05  WJ-COMPANY              PIC X(10).
           05  WJ-YEAR                 PIC X(8).
           05  WJ-PERIOD               PIC X(8).
           05  WJ-DEBIT                PIC X(24).
           05  WJ-CREDIT               PIC X(24).
           05  WJ-STATUS               PIC X(12).
           05  WJ-DUP                  PIC X(8).
       01  WS-ONE-COL.
           05  WS-C1                   PIC X(40).
           05  WS-C2                   PIC X(40).
       LINKAGE SECTION.
       COPY SOB2BCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 1000-INITIALISE THRU 1000-EXIT
           IF WS-ERROR = "N"
               EVALUATE WS-FUNC
                   WHEN "LINK"
                       PERFORM 2000-LINK THRU 2000-EXIT
                   WHEN "SUPPCHG"
                       PERFORM 3000-SUPPLY-CHANGE THRU 3000-EXIT
                   WHEN "DIRECTDL"
                       PERFORM 4000-DIRECT-DELIVERY THRU 4000-EXIT
                   WHEN "ENQUIRE"
                       PERFORM 5000-ENQUIRE THRU 5000-EXIT
                   WHEN OTHER
                       PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
               END-EVALUATE
           END-IF
           PERFORM 9400-REPORT THRU 9400-EXIT.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INITIALISE SECTION.
       1000-START.
           MOVE "N" TO WS-ERROR
           MOVE "N" TO WS-IN-TXN
           MOVE 0 TO WS-RC WS-I WS-J WS-ROWS
           MOVE SPACES TO WS-REASON WS-MSG
           MOVE "SOB2B" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "SO-021" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE FUNCTION UPPER-CASE(CA-FUNCTION OF DFHCOMMAREA)
               TO WS-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO WS-CO
           MOVE CA-SITE OF DFHCOMMAREA TO WS-SITE
           MOVE CA-USER OF DFHCOMMAREA TO WS-USER
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO WS-BDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO WS-CORR
      *>   a linked line never creates warehouse stock and a link is
      *>   never a guarantee or an invoice approval
           MOVE "N" TO BB-ONHAND-CREATED
           MOVE "N" TO BB-SUPPLY-GUARANTEE
           MOVE "N" TO BB-INVOICE-APPROVED
           MOVE "N" TO BB-PROMISE-CHANGED
           MOVE "N" TO BB-WAS-DUPLICATE
           MOVE "N" TO BB-AFFECT-TRUNC
           MOVE 0 TO BB-MOVEMENT-ID
           MOVE 0 TO BB-EXCEPTION-COUNT
           MOVE 0 TO BB-AFFECTED-CNT
           IF WS-BDATE = 0
               MOVE 20261006 TO WS-BDATE
               MOVE WS-BDATE TO CA-BUSINESS-DATE OF DFHCOMMAREA
           END-IF
           IF BB-POSTING-DATE = 0
               MOVE WS-BDATE TO BB-POSTING-DATE
           END-IF
           IF WS-CO = SPACES
               MOVE "B2B0001" TO WS-REASON
               MOVE "A company code is required" TO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 2000  LINK.  THE SALES LINE IS TOLD WHERE ITS SUPPLY IS
      *>       COMING FROM.  A BACK TO BACK LINE IS STILL DELIVERED
      *>       THROUGH THE WAREHOUSE; A DROP SHIP LINE NEVER IS.
      *>----------------------------------------------------------------
       2000-LINK SECTION.
       2000-START.
           PERFORM 8000-LOAD-LINE THRU 8000-EXIT
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           MOVE FUNCTION UPPER-CASE(BB-SUPPLY-TYPE) TO BB-SUPPLY-TYPE
           IF NOT BB-BACKTOBACK AND NOT BB-DROPSHIP AND NOT BB-MAKE
               MOVE "B2B0003" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Supply type "
                      FUNCTION TRIM(BB-SUPPLY-TYPE)
                      " is not BACKTOBACK, DROPSHIP or MAKE"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF BB-MAKE
               IF BB-LINKED-KIT-REF = SPACES
                   MOVE "B2B0004" TO WS-REASON
                   MOVE SPACES TO WS-MSG
                   STRING "A made line must name the assembly "
                          "commitment it is linked to"
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM 9000-REJECT THRU 9000-EXIT
                   GO TO 2000-EXIT
               END-IF
           ELSE
               IF BB-LINKED-PO = SPACES OR BB-LINKED-PO-LINE = 0
                   MOVE "B2B0004" TO WS-REASON
                   MOVE SPACES TO WS-MSG
                   STRING "A linked line must name the purchase "
                          "order number and the purchase order line"
                       DELIMITED BY SIZE INTO WS-MSG
                   PERFORM 9000-REJECT THRU 9000-EXIT
                   GO TO 2000-EXIT
               END-IF
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrderLine SET SupplyType = ?, "
                  "LinkedPoNumber = NULLIF(?,''), LinkedPoLine = "
                  "NULLIF(?,'0'), LinkedKitRef = NULLIF(?,'') "
                  "WHERE SalesOrderLineId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE BB-SUPPLY-TYPE TO SQLIO-PARM-VAL(1)
           MOVE BB-LINKED-PO TO SQLIO-PARM-VAL(2)
           MOVE BB-LINKED-PO-LINE TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE BB-LINKED-KIT-REF TO SQLIO-PARM-VAL(4)
           MOVE BB-ORDER-LINE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(5)
           MOVE 5 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           IF BB-DROPSHIP
               PERFORM 2100-MARK-DROPSHIP THRU 2100-EXIT
           END-IF
           MOVE SPACES TO WS-MSG
           IF BB-MAKE
               STRING "Line " FUNCTION TRIM(BB-ITEM)
                      " is linked to assembly commitment "
                      FUNCTION TRIM(BB-LINKED-KIT-REF)
                      "; the link does not guarantee delivery"
                   DELIMITED BY SIZE INTO WS-MSG
           ELSE
               MOVE BB-LINKED-PO-LINE TO WS-INT-ED
               STRING "Line " FUNCTION TRIM(BB-ITEM)
                      " is linked to purchase order "
                      FUNCTION TRIM(BB-LINKED-PO) " line "
                      FUNCTION TRIM(WS-INT-ED)
                      "; the link does not guarantee delivery nor "
                      "approve a supplier invoice"
                   DELIMITED BY SIZE INTO WS-MSG
           END-IF
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE SPACES TO WS-MSG.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2100-MARK-DROPSHIP SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.DeliverySchedule SET FulfilmentMethod "
                  "= 'DROPSHIP' WHERE SalesOrderLineId = ? AND "
                  "Status IN ('OPEN','PARTIAL')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE BB-ORDER-LINE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 3000  SUPPCHG.  THE SUPPLIER HAS MOVED OR CANCELLED ITS
      *>       PROMISE.  THE CUSTOMER PROMISE IS SERVICE EVIDENCE AND
      *>       IS LEFT EXACTLY AS IT IS; WHAT HAPPENS IS THAT THE
      *>       AFFECTED SCHEDULES ARE LISTED FOR REVIEW AND AN
      *>       EXCEPTION IS RAISED FOR SOMEONE TO DECIDE.
      *>----------------------------------------------------------------
       3000-SUPPLY-CHANGE SECTION.
       3000-START.
           IF BB-LINKED-PO = SPACES
               MOVE "B2B0004" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Name the purchase order whose promise has "
                      "changed"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           MOVE FUNCTION UPPER-CASE(BB-CHANGE-TYPE) TO BB-CHANGE-TYPE
           IF BB-CHANGE-TYPE = SPACES
               MOVE "DELAY" TO BB-CHANGE-TYPE
           END-IF
           IF NOT BB-CHG-DELAY AND NOT BB-CHG-CANCEL AND
              NOT BB-CHG-QTY AND NOT BB-CHG-EARLY
               MOVE "B2B0011" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Change type "
                      FUNCTION TRIM(BB-CHANGE-TYPE)
                      " is not DELAY, CANCEL, QTYCHG or EARLY"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           PERFORM 3100-FIND-AFFECTED THRU 3100-EXIT
           IF WS-ERROR = "Y"
               GO TO 3000-EXIT
           END-IF
           IF BB-AFFECTED-CNT = 0
               MOVE 4 TO WS-RC
               MOVE SPACES TO WS-MSG
               STRING "No open customer schedule is linked to "
                      "purchase order " FUNCTION TRIM(BB-LINKED-PO)
                   DELIMITED BY SIZE INTO WS-MSG
               GO TO 3000-EXIT
           END-IF
           PERFORM 3200-RAISE-REVIEW THRU 3200-EXIT
           MOVE "N" TO BB-PROMISE-CHANGED
           MOVE BB-AFFECTED-CNT TO WS-INT-ED
           MOVE SPACES TO WS-MSG
           STRING FUNCTION TRIM(WS-INT-ED)
                  " customer schedule(s) are identified for review; "
                  "no customer promise has been changed"
               DELIMITED BY SIZE INTO WS-MSG
           IF WS-RC < 4
               MOVE 4 TO WS-RC
           END-IF
           MOVE "B2B0005" TO WS-REASON.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3100-FIND-AFFECTED SECTION.
       3100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT s.OrderNumber, CAST(s.LineNum AS "
                  "varchar(12)), CAST(s.ScheduleNo AS varchar(12)), "
                  "h.CustomerCode, ISNULL(l.ItemCode,''), "
                  "CAST(s.Quantity AS varchar(24)), "
                  "CAST(s.DespatchedQty AS varchar(24)), "
                  "CONVERT(char(8), s.OriginalPromiseDate, 112), "
                  "CONVERT(char(8), s.CurrentPromiseDate, 112), "
                  "s.Status FROM so.DeliverySchedule s JOIN "
                  "so.SalesOrderLine l ON l.SalesOrderLineId = "
                  "s.SalesOrderLineId JOIN so.SalesOrder h ON "
                  "h.SalesOrderId = l.SalesOrderId WHERE "
                  "s.CompanyCode = ? AND l.LinkedPoNumber = ? AND "
                  "(? = '0' OR l.LinkedPoLine = ?) AND s.Status IN "
                  "('OPEN','PARTIAL') ORDER BY s.OrderNumber, "
                  "s.LineNum, s.ScheduleNo"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE BB-LINKED-PO TO SQLIO-PARM-VAL(2)
           MOVE BB-LINKED-PO-LINE TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE 4 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 12 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 20 TO SQLIO-COL-WIDTH(4)
           MOVE 40 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 8 TO SQLIO-COL-WIDTH(8)
           MOVE 8 TO SQLIO-COL-WIDTH(9)
           MOVE 10 TO SQLIO-COL-WIDTH(10)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SCHED-ROW
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
               GO TO 3100-EXIT
           END-IF
           MOVE 0 TO WS-ROWS
           PERFORM 3150-ONE-SCHEDULE THRU 3150-EXIT
               UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR OR WS-ROWS >= 20
           IF NOT SQLIO-NOTFOUND AND NOT SQLIO-ERROR
               MOVE "Y" TO BB-AFFECT-TRUNC
           END-IF
           MOVE 3 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SCHED-ROW
           MOVE WS-ROWS TO BB-AFFECTED-CNT.
       3100-EXIT.
           EXIT.

       3150-ONE-SCHEDULE SECTION.
       3150-START.
           MOVE 3 TO SQLIO-CURSOR
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-SCHED-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               GO TO 3150-EXIT
           END-IF
           ADD 1 TO WS-ROWS
           MOVE WS-ROWS TO WS-I
           MOVE WS-S-ORDER TO BB-S-ORDER(WS-I)
           COMPUTE WS-N = FUNCTION NUMVAL(WS-S-LINE)
           MOVE WS-N TO BB-S-LINE(WS-I)
           COMPUTE WS-N = FUNCTION NUMVAL(WS-S-SCHEDULE)
           MOVE WS-N TO BB-S-SCHEDULE(WS-I)
           MOVE WS-S-CUSTOMER TO BB-S-CUSTOMER(WS-I)
           MOVE WS-S-ITEM TO BB-S-ITEM(WS-I)
           COMPUTE WS-N = FUNCTION NUMVAL(WS-S-QTY)
           MOVE WS-N TO BB-S-QTY(WS-I)
           COMPUTE WS-N = FUNCTION NUMVAL(WS-S-DESP)
           MOVE WS-N TO BB-S-DESP-QTY(WS-I)
           COMPUTE WS-N = FUNCTION NUMVAL(WS-S-ORIG)
           MOVE WS-N TO BB-S-ORIG-PROM(WS-I)
           COMPUTE WS-N = FUNCTION NUMVAL(WS-S-CURR)
           MOVE WS-N TO BB-S-CURR-PROM(WS-I)
           MOVE WS-S-STATUS TO BB-S-STATUS(WS-I)
           MOVE "REVIEW" TO BB-S-ACTION(WS-I)
           MOVE SPACES TO BB-S-MESSAGE(WS-I)
           EVALUATE TRUE
               WHEN BB-CHG-CANCEL
                   STRING "The supplier has cancelled; this "
                          "schedule needs a new supply decision"
                       DELIMITED BY SIZE INTO BB-S-MESSAGE(WS-I)
               WHEN BB-CHG-QTY
                   STRING "The supplier quantity has changed; "
                          "review the cover for this schedule"
                       DELIMITED BY SIZE INTO BB-S-MESSAGE(WS-I)
               WHEN OTHER
                   MOVE BB-NEW-PROMISE TO WS-DATE-ED
                   STRING "The supplier now promises "
                          FUNCTION TRIM(WS-DATE-ED)
                          "; review the customer promise"
                       DELIMITED BY SIZE INTO BB-S-MESSAGE(WS-I)
           END-EVALUATE.
       3150-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3200-RAISE-REVIEW SECTION.
       3200-START.
           INITIALIZE WS-EXC-CA
           MOVE "RAISE" TO CA-FUNCTION OF WS-EXC-CA
           MOVE WS-CO TO CA-COMPANY OF WS-EXC-CA
           MOVE WS-SITE TO CA-SITE OF WS-EXC-CA
           MOVE WS-USER TO CA-USER OF WS-EXC-CA
           MOVE WS-BDATE TO CA-BUSINESS-DATE OF WS-EXC-CA
           MOVE WS-CORR TO CA-CORRELATION-ID OF WS-EXC-CA
           MOVE "SO" TO EX-MODULE
           MOVE "HOLD" TO EX-SEVERITY
           MOVE "LINKED.SUPPLY.CHANGED" TO EX-CATEGORY
           MOVE BB-AFFECTED-CNT TO WS-INT-ED
           MOVE SPACES TO EX-MESSAGE-TEXT
           STRING "Purchase order " FUNCTION TRIM(BB-LINKED-PO)
                  " has a " FUNCTION TRIM(BB-CHANGE-TYPE)
                  " supplier promise affecting "
                  FUNCTION TRIM(WS-INT-ED)
                  " customer schedule(s); review the customer "
                  "promises, they have not been changed"
               DELIMITED BY SIZE INTO EX-MESSAGE-TEXT
           MOVE "PURCHASEORDER" TO EX-DOCUMENT-TYPE
           MOVE BB-LINKED-PO TO EX-DOCUMENT-REF
           MOVE "SALESMGR" TO EX-OWNER-ROLE
           MOVE 2 TO EX-PRIORITY
           IF BB-NEW-PROMISE > 0
               MOVE BB-NEW-PROMISE TO EX-DUE-AT
           ELSE
               MOVE WS-BDATE TO EX-DUE-AT
           END-IF
           CALL "EXCPRAIS" USING WS-EXC-CA
           IF CA-RETURN-CODE OF WS-EXC-CA < 8
               MOVE EX-EXCEPTION-ID TO BB-EXCEPTION-ID
               ADD 1 TO BB-EXCEPTION-COUNT
           END-IF.
       3200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 4000  DIRECTDL.  THE SUPPLIER HAS CONFIRMED DELIVERY TO THE
      *>       CUSTOMER'S DOOR.  THE QUANTITY DELIVERED IS RECORDED AS
      *>       FULFILMENT EVIDENCE AND NO STOCK MOVEMENT IS MADE, SO
      *>       NO WAREHOUSE ON HAND QUANTITY IS CREATED ANYWHERE.
      *>----------------------------------------------------------------
       4000-DIRECT-DELIVERY SECTION.
       4000-START.
           PERFORM 8000-LOAD-LINE THRU 8000-EXIT
           IF WS-ERROR = "Y"
               GO TO 4000-EXIT
           END-IF
           IF BB-DELIVERED-QTY <= 0
               MOVE "B2B0006" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "State the quantity the supplier delivered "
                      "to the customer"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF BB-DELIVERED-QTY > BB-REMAINING-QTY
               MOVE "B2B0007" TO WS-REASON
               MOVE BB-REMAINING-QTY TO WS-QTY-ED
               MOVE SPACES TO WS-MSG
               STRING "Only " FUNCTION TRIM(WS-QTY-ED)
                      " remains to be delivered on this line"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF BB-EVIDENCE-REF = SPACES
               MOVE "B2B0008" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Confirmed direct delivery needs the "
                      "supplier's delivery evidence reference"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF NOT BB-DROPSHIP
               MOVE "B2B0005" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Line is supplied as "
                      FUNCTION TRIM(BB-SUPPLY-TYPE)
                      "; only a DROPSHIP line is fulfilled by "
                      "direct delivery"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
      *>   D-03: the cost is recognised on the supplier confirmed
      *>   delivery date, not on the date the clerk keys it in
           IF BB-DELIVERY-DATE = 0
               MOVE WS-BDATE TO BB-DELIVERY-DATE
           END-IF
           MOVE BB-DELIVERY-DATE TO BB-POSTING-DATE
           COMPUTE WS-AMT1 ROUNDED =
               BB-DELIVERED-QTY * BB-UNIT-COST-GBP
           MOVE WS-AMT1 TO BB-COST-GBP
           PERFORM 4050-ACCOUNTS THRU 4050-EXIT
           PERFORM 4060-EVENT-ANALYSIS THRU 4060-EXIT
           PERFORM 4100-DRAW-DESPATCH-REF THRU 4100-EXIT
           IF WS-ERROR = "Y"
               GO TO 4000-EXIT
           END-IF
           PERFORM 8600-BEGIN THRU 8600-EXIT
           IF WS-ERROR = "Y"
               GO TO 4000-EXIT
           END-IF
           PERFORM 4200-WRITE-EVIDENCE THRU 4200-EXIT
           IF WS-ERROR = "Y"
               PERFORM 8620-ROLLBACK THRU 8620-EXIT
               GO TO 4000-EXIT
           END-IF
           PERFORM 4300-UPDATE-DEMAND THRU 4300-EXIT
           IF WS-ERROR = "Y"
               PERFORM 8620-ROLLBACK THRU 8620-EXIT
               GO TO 4000-EXIT
           END-IF
           PERFORM 8610-COMMIT THRU 8610-EXIT
      *>   the accrual is posted outside the SQLIO transaction: a
      *>   procedure that throws would doom an open one
           IF WS-AMT1 NOT = 0
               PERFORM 4400-ACCRUE-COGS THRU 4400-EXIT
           END-IF
           MOVE "N" TO BB-ONHAND-CREATED
           MOVE 0 TO BB-MOVEMENT-ID
           IF WS-ERROR = "Y"
               GO TO 4000-EXIT
           END-IF
           MOVE BB-DELIVERED-QTY TO WS-QTY-ED
           MOVE BB-COST-GBP TO WS-AMT-ED
           MOVE SPACES TO WS-MSG
           STRING FUNCTION TRIM(WS-QTY-ED)
                  " delivered direct to the customer; £"
                  FUNCTION TRIM(WS-AMT-ED)
                  " accrued to cost of sales against goods received "
                  "not invoiced, and no warehouse stock was created"
               DELIMITED BY SIZE INTO WS-MSG
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE SPACES TO WS-MSG.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4050-ACCOUNTS SECTION.
       4050-START.
           IF BB-COGS-ACCOUNT = SPACES
               INITIALIZE WS-PARM-CA
               MOVE "GET" TO CA-FUNCTION OF WS-PARM-CA
               MOVE WS-CO TO CA-COMPANY OF WS-PARM-CA
               MOVE WS-USER TO CA-USER OF WS-PARM-CA
               MOVE BB-POSTING-DATE TO CA-BUSINESS-DATE OF WS-PARM-CA
               MOVE "SO.DROPSHIP.COGS.ACCOUNT" TO PM-PARAM-CODE
               MOVE "Y" TO PM-USE-DEFAULT
               MOVE "5010" TO PM-DEFAULT-TEXT
               CALL "PARMGET" USING WS-PARM-CA
               MOVE PM-VALUE-TEXT(1:12) TO BB-COGS-ACCOUNT
               IF BB-COGS-ACCOUNT = SPACES
                   MOVE "5010" TO BB-COGS-ACCOUNT
               END-IF
           END-IF
           IF BB-GRNI-ACCOUNT = SPACES
               INITIALIZE WS-PARM-CA
               MOVE "GET" TO CA-FUNCTION OF WS-PARM-CA
               MOVE WS-CO TO CA-COMPANY OF WS-PARM-CA
               MOVE WS-USER TO CA-USER OF WS-PARM-CA
               MOVE BB-POSTING-DATE TO CA-BUSINESS-DATE OF WS-PARM-CA
               MOVE "SO.DROPSHIP.GRNI.ACCOUNT" TO PM-PARAM-CODE
               MOVE "Y" TO PM-USE-DEFAULT
               MOVE "2010" TO PM-DEFAULT-TEXT
               CALL "PARMGET" USING WS-PARM-CA
               MOVE PM-VALUE-TEXT(1:12) TO BB-GRNI-ACCOUNT
               IF BB-GRNI-ACCOUNT = SPACES
                   MOVE "2010" TO BB-GRNI-ACCOUNT
               END-IF
           END-IF.
       4050-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 4060  THE ANALYSIS CODES COME FROM THE BUSINESS EVENT MAP, NOT
      *>       FROM THIS PROGRAM.  fin.usp_PostJournal REFUSES A COST OF
      *>       SALES LINE THAT CARRIES NO COST CENTRE AND REFUSES A
      *>       GOODS RECEIVED NOT INVOICED LINE THAT CARRIES ONE.
      *>----------------------------------------------------------------
       4060-EVENT-ANALYSIS SECTION.
       4060-START.
           MOVE "COGS" TO WS-LEG-CODE
           PERFORM 4070-ONE-LEG THRU 4070-EXIT
           MOVE WS-C2(1:20) TO WS-COGS-CC
           IF WS-C1 NOT = SPACES
               MOVE WS-C1(1:12) TO BB-COGS-ACCOUNT
           END-IF
           MOVE "GRNI" TO WS-LEG-CODE
           PERFORM 4070-ONE-LEG THRU 4070-EXIT
           MOVE WS-C2(1:20) TO WS-GRNI-CC
           IF WS-C1 NOT = SPACES
               MOVE WS-C1(1:12) TO BB-GRNI-ACCOUNT
           END-IF
           MOVE SPACES TO WS-LEG-CODE.
       4060-EXIT.
           EXIT.

       4070-ONE-LEG SECTION.
       4070-START.
           MOVE SPACES TO WS-C1
           MOVE SPACES TO WS-C2
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) m.AccountCode, "
                  "ISNULL(m.DimCostCentre,'') FROM "
                  "fin.EventAccountMap m WHERE m.CompanyCode = ? "
                  "AND m.EventCode = 'SO.DROPSHIP.COGS' AND "
                  "m.LegCode = ? AND m.IsActive = 1 ORDER BY "
                  "m.SortOrder"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-LEG-CODE TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE SPACES TO WS-C1
               MOVE SPACES TO WS-C2
           END-IF.
       4070-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4100-DRAW-DESPATCH-REF SECTION.
       4100-START.
           IF BB-CARRIER-REF NOT = SPACES
               GO TO 4100-EXIT
           END-IF
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUM-CA
           MOVE WS-CO TO CA-COMPANY OF WS-NUM-CA
           MOVE WS-USER TO CA-USER OF WS-NUM-CA
           MOVE WS-CORR TO CA-CORRELATION-ID OF WS-NUM-CA
           MOVE BB-DELIVERY-DATE TO CA-BUSINESS-DATE OF WS-NUM-CA
           MOVE "SDES" TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF CA-RETURN-CODE OF WS-NUM-CA > 4
               MOVE CA-REASON-CODE OF WS-NUM-CA TO WS-REASON
               MOVE CA-MESSAGE OF WS-NUM-CA TO WS-MSG
               MOVE CA-RETURN-CODE OF WS-NUM-CA TO WS-RC
               MOVE "Y" TO WS-ERROR
               GO TO 4100-EXIT
           END-IF
           MOVE NG-DOCUMENT-NUMBER(1:30) TO BB-CARRIER-REF.
       4100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 4200  THE EVIDENCE ROW.  IT LOOKS LIKE A DESPATCH BECAUSE IT
      *>       IS ONE - THE GOODS WENT TO THE CUSTOMER - BUT NO STOCK
      *>       MOVEMENT ID IS HELD AGAINST THE LINE BECAUSE NO STOCK
      *>       EVER ENTERED OR LEFT A WAREHOUSE.
      *>----------------------------------------------------------------
       4200-WRITE-EVIDENCE SECTION.
       4200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT so.Despatch (CompanyCode, SiteCode, "
                  "DespatchRef, CustomerCode, OrderNumber, "
                  "DespatchDate, PostingDate, CarrierReference, "
                  "Status, IsExport, CorrelationId, ConfirmedBy, "
                  "ConfirmedAt, CreatedBy) VALUES (?, ?, ?, ?, ?, "
                  "CONVERT(date, ?, 112), CONVERT(date, ?, 112), "
                  "NULLIF(?,''), 'CONFIRMED', 0, NULLIF(?,''), ?, "
                  "SYSDATETIME(), ?); SELECT CAST(SCOPE_IDENTITY() "
                  "AS varchar(24)), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-SITE TO SQLIO-PARM-VAL(2)
           MOVE BB-CARRIER-REF(1:30) TO SQLIO-PARM-VAL(3)
           MOVE BB-CUSTOMER TO SQLIO-PARM-VAL(4)
           MOVE BB-ORDER-NUMBER TO SQLIO-PARM-VAL(5)
           MOVE BB-DELIVERY-DATE TO SQLIO-PARM-VAL(6)
           MOVE BB-POSTING-DATE TO SQLIO-PARM-VAL(7)
           MOVE BB-EVIDENCE-REF(1:60) TO SQLIO-PARM-VAL(8)
           MOVE WS-CORR TO SQLIO-PARM-VAL(9)
           MOVE WS-USER TO SQLIO-PARM-VAL(10)
           MOVE WS-USER TO SQLIO-PARM-VAL(11)
           MOVE 11 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
               GO TO 4200-EXIT
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WS-C1)
           MOVE WS-N TO WS-DESPATCH-ID
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT so.DespatchLine (DespatchId, LineNum, "
                  "SalesOrderLineId, OrderNumber, OrderLineNum, "
                  "ScheduleNo, ItemCode, Description, PickedQty, "
                  "DespatchedQty, UomCode, ConversionFactor, "
                  "StockQty, UnitCostGBP, CogsGBP, CostMethod, "
                  "MovementId) VALUES (?, 1, ?, ?, ?, NULLIF(?,'0'),"
                  " ?, ?, 0, ?, NULLIF(?,''), 1, 0, ?, ?, "
                  "'DROP', NULL)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-DESPATCH-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE BB-ORDER-LINE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE BB-ORDER-NUMBER TO SQLIO-PARM-VAL(3)
           MOVE BB-LINE-NUM TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE BB-SCHEDULE-NO TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(5)
           MOVE BB-ITEM TO SQLIO-PARM-VAL(6)
           MOVE BB-DESCRIPTION TO SQLIO-PARM-VAL(7)
           MOVE BB-DELIVERED-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE BB-UOM TO SQLIO-PARM-VAL(9)
           MOVE BB-UNIT-COST-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE BB-COST-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE 11 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       4200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4300-UPDATE-DEMAND SECTION.
       4300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrderLine SET DespatchedQty = "
                  "DespatchedQty + ? WHERE SalesOrderLineId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE BB-DELIVERED-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE BB-ORDER-LINE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-ERROR = "Y" OR BB-SCHEDULE-NO = 0
               GO TO 4300-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.DeliverySchedule SET DespatchedQty = "
                  "DespatchedQty + ?, Status = CASE WHEN "
                  "DespatchedQty + ? >= Quantity THEN 'COMPLETE' "
                  "ELSE 'PARTIAL' END WHERE SalesOrderLineId = ? "
                  "AND ScheduleNo = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE BB-DELIVERED-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE BB-ORDER-LINE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE BB-SCHEDULE-NO TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE 4 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       4300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 4400  THE ACCRUAL.  DR COST OF SALES, CR GOODS RECEIVED NOT
      *>       INVOICED.  THE SUPPLIER INVOICE CLEARS THE GRNI LATER,
      *>       WHICH IS WHY THE CREDIT IS NOT THE SUPPLIER ACCOUNT.
      *>----------------------------------------------------------------
       4400-ACCRUE-COGS SECTION.
       4400-START.
           MOVE SPACES TO WS-POST-CORR
           STRING FUNCTION TRIM(WS-CORR) "-DS"
               DELIMITED BY SIZE INTO WS-POST-CORR
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE fin.JournalStage WHERE CorrelationId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-POST-CORR TO SQLIO-PARM-VAL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-ERROR = "Y"
               GO TO 4400-EXIT
           END-IF
           MOVE 1 TO WS-I
           MOVE BB-COGS-ACCOUNT TO WS-C1
           MOVE "DR" TO WS-C2
           MOVE WS-COGS-CC TO WS-LEG-CC
           PERFORM 4450-STAGE-ONE THRU 4450-EXIT
           IF WS-ERROR = "Y"
               GO TO 4400-EXIT
           END-IF
           MOVE 2 TO WS-I
           MOVE BB-GRNI-ACCOUNT TO WS-C1
           MOVE "CR" TO WS-C2
           MOVE WS-GRNI-CC TO WS-LEG-CC
           PERFORM 4450-STAGE-ONE THRU 4450-EXIT
           IF WS-ERROR = "Y"
               GO TO 4400-EXIT
           END-IF
           PERFORM 4500-POST THRU 4500-EXIT.
       4400-EXIT.
           EXIT.

       4450-STAGE-ONE SECTION.
       4450-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.JournalStage (CorrelationId, LineNum, "
                  "CompanyCode, AccountCode, DebitAmount, "
                  "CreditAmount, CurrencyCode, Narrative, "
                  "SubledgerType, SubledgerRef, SourceRef, "
                  "CreatedBy, DimCostCentre) VALUES (?, ?, ?, ?, "
                  "?, ?, 'GBP', ?, 'SO', NULLIF(?,''), "
                  "NULLIF(?,''), ?, NULLIF(?,''))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-POST-CORR TO SQLIO-PARM-VAL(1)
           MOVE WS-I TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE WS-CO TO SQLIO-PARM-VAL(3)
           MOVE WS-C1(1:12) TO SQLIO-PARM-VAL(4)
           MOVE BB-COST-GBP TO SQLIO-NUM-EDIT
           IF WS-C2(1:2) = "DR"
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT)
                   TO SQLIO-PARM-VAL(5)
               MOVE "0" TO SQLIO-PARM-VAL(6)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(5)
               MOVE FUNCTION TRIM(SQLIO-NUM-EDIT)
                   TO SQLIO-PARM-VAL(6)
           END-IF
           MOVE SPACES TO SQLIO-PARM-VAL(7)
           STRING "Direct delivery " FUNCTION TRIM(BB-CARRIER-REF)
                  " order " FUNCTION TRIM(BB-ORDER-NUMBER)
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(7)
           MOVE BB-CUSTOMER TO SQLIO-PARM-VAL(8)
           MOVE BB-CARRIER-REF(1:30) TO SQLIO-PARM-VAL(9)
           MOVE WS-USER TO SQLIO-PARM-VAL(10)
           MOVE WS-LEG-CC TO SQLIO-PARM-VAL(11)
           MOVE 11 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       4450-EXIT.
           EXIT.

       4500-POST SECTION.
       4500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC fin.usp_PostJournal ?, ?, ?, ?, ?, 'SO', ?, "
                  "'GBP', 'SO', ?, 'SO.DROPSHIP.COGS', 'JNL', 1, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE WS-POST-CORR TO SQLIO-PARM-VAL(2)
           MOVE WS-USER TO SQLIO-PARM-VAL(3)
           MOVE SPACES TO SQLIO-PARM-VAL(4)
           STRING "Drop shipment direct fulfilment cost, order "
                  FUNCTION TRIM(BB-ORDER-NUMBER)
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(4)
           MOVE BB-POSTING-DATE TO SQLIO-PARM-VAL(5)
           MOVE BB-DELIVERY-DATE TO SQLIO-PARM-VAL(6)
           MOVE BB-CARRIER-REF(1:30) TO SQLIO-PARM-VAL(7)
           MOVE BB-EVIDENCE-REF TO SQLIO-PARM-VAL(8)
           MOVE 8 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 9 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 10 TO SQLIO-COL-WIDTH(3)
           MOVE 8 TO SQLIO-COL-WIDTH(4)
           MOVE 8 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 12 TO SQLIO-COL-WIDTH(8)
           MOVE 8 TO SQLIO-COL-WIDTH(9)
           MOVE 5 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-POST-ROW
           IF SQLIO-ERROR
               MOVE "Y" TO WS-ERROR
               MOVE 12 TO WS-RC
               MOVE "B2B0010" TO WS-REASON
               MOVE SQLIO-ERRMSG(1:200) TO WS-MSG
               IF WS-MSG = SPACES
                   MOVE SPACES TO WS-MSG
                   STRING "The nominal ledger refused the direct "
                          "fulfilment accrual"
                       DELIMITED BY SIZE INTO WS-MSG
               END-IF
               MOVE 5 TO SQLIO-CURSOR
               MOVE "CLOS" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-POST-ROW
               GO TO 4500-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 4500-EXIT
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WJ-ID)
           MOVE WS-N TO BB-JOURNAL-ID
           MOVE WJ-NUMBER TO BB-JOURNAL-NUMBER
           IF WJ-DUP(1:1) = "1"
               MOVE "Y" TO BB-WAS-DUPLICATE
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.Despatch SET CogsPostedGBP = ?, "
                  "JournalId = NULLIF(?,'0') WHERE DespatchId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE BB-COST-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE BB-JOURNAL-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(2)
           MOVE WS-DESPATCH-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       4500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       5000-ENQUIRE SECTION.
       5000-START.
           PERFORM 8000-LOAD-LINE THRU 8000-EXIT
           IF WS-ERROR = "Y"
               GO TO 5000-EXIT
           END-IF
           IF BB-LINKED-PO NOT = SPACES
               PERFORM 3100-FIND-AFFECTED THRU 3100-EXIT
               PERFORM VARYING WS-I FROM 1 BY 1
                       UNTIL WS-I > BB-AFFECTED-CNT
                   MOVE "LINKED" TO BB-S-ACTION(WS-I)
               END-PERFORM
           END-IF
           MOVE BB-REMAINING-QTY TO WS-QTY-ED
           MOVE SPACES TO WS-MSG
           STRING "Line is supplied as "
                  FUNCTION TRIM(BB-SUPPLY-TYPE) "; "
                  FUNCTION TRIM(WS-QTY-ED)
                  " remains to be delivered"
               DELIMITED BY SIZE INTO WS-MSG
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE SPACES TO WS-MSG.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8000-LOAD-LINE SECTION.
       8000-START.
           IF BB-ORDER-NUMBER = SPACES OR BB-LINE-NUM = 0
               MOVE "B2B0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "An order number and a line number are "
                      "required"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 8000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(l.SalesOrderLineId AS varchar(24)), "
                  "h.CustomerCode, ISNULL(l.ItemCode,''), "
                  "l.Description, ISNULL(l.UomCode,''), "
                  "CAST(l.AcceptedQty AS varchar(24)), "
                  "CAST(l.CancelledQty AS varchar(24)), "
                  "CAST(l.DespatchedQty AS varchar(24)), "
                  "l.SupplyType, ISNULL(l.LinkedPoNumber,''), "
                  "ISNULL(CAST(l.LinkedPoLine AS varchar(12)),'0'), "
                  "ISNULL(l.LinkedKitRef,''), l.Status, "
                  "h.SiteCode, ISNULL(CAST(l.CostSnapshotGBP AS "
                  "varchar(24)),'0') FROM so.SalesOrderLine l JOIN "
                  "so.SalesOrder h ON h.SalesOrderId = "
                  "l.SalesOrderId WHERE l.CompanyCode = ? AND "
                  "l.OrderNumber = ? AND l.LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE BB-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE BB-LINE-NUM TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 15 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 40 TO SQLIO-COL-WIDTH(3)
           MOVE 60 TO SQLIO-COL-WIDTH(4)
           MOVE 10 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 24 TO SQLIO-COL-WIDTH(8)
           MOVE 12 TO SQLIO-COL-WIDTH(9)
           MOVE 30 TO SQLIO-COL-WIDTH(10)
           MOVE 12 TO SQLIO-COL-WIDTH(11)
           MOVE 30 TO SQLIO-COL-WIDTH(12)
           MOVE 10 TO SQLIO-COL-WIDTH(13)
           MOVE 10 TO SQLIO-COL-WIDTH(14)
           MOVE 24 TO SQLIO-COL-WIDTH(15)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
               GO TO 8000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO WS-RC
               MOVE "Y" TO WS-ERROR
               MOVE "B2B0002" TO WS-REASON
               MOVE BB-LINE-NUM TO WS-INT-ED
               MOVE SPACES TO WS-MSG
               STRING "Order " FUNCTION TRIM(BB-ORDER-NUMBER)
                      " line " FUNCTION TRIM(WS-INT-ED)
                      " is not held for this company"
                   DELIMITED BY SIZE INTO WS-MSG
               GO TO 8000-EXIT
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WB-LINE-ID)
           MOVE WS-N TO BB-ORDER-LINE-ID
           MOVE WB-CUSTOMER TO BB-CUSTOMER
           MOVE WB-ITEM TO BB-ITEM
           MOVE WB-DESC TO BB-DESCRIPTION
           IF BB-UOM = SPACES
               MOVE WB-UOM TO BB-UOM
           END-IF
           IF WS-SITE = SPACES
               MOVE WB-SITE TO WS-SITE
           END-IF
           IF WS-FUNC NOT = "LINK"
               MOVE WB-SUPPLY-TYPE TO BB-SUPPLY-TYPE
               MOVE WB-PO-NUMBER TO BB-LINKED-PO
               COMPUTE WS-N = FUNCTION NUMVAL(WB-PO-LINE)
               MOVE WS-N TO BB-LINKED-PO-LINE
               MOVE WB-KIT-REF TO BB-LINKED-KIT-REF
           END-IF
           IF BB-UNIT-COST-GBP = 0
               COMPUTE WS-N = FUNCTION NUMVAL(WB-COST)
               MOVE WS-N TO BB-UNIT-COST-GBP
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WB-ACCEPTED)
                        - FUNCTION NUMVAL(WB-CANCELLED)
                        - FUNCTION NUMVAL(WB-DESPATCHED)
           IF WS-N < 0
               MOVE 0 TO WS-N
           END-IF
           MOVE WS-N TO BB-REMAINING-QTY.
       8000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8500-EXEC-UPDATE SECTION.
       8500-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
           END-IF
           MOVE 1 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       8500-EXIT.
           EXIT.

       8600-BEGIN SECTION.
       8600-START.
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
           ELSE
               MOVE "Y" TO WS-IN-TXN
           END-IF.
       8600-EXIT.
           EXIT.

       8610-COMMIT SECTION.
       8610-START.
           IF WS-IN-TXN = "Y"
               MOVE "COMT" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
               MOVE "N" TO WS-IN-TXN
           END-IF.
       8610-EXIT.
           EXIT.

       8620-ROLLBACK SECTION.
       8620-START.
           IF WS-IN-TXN = "Y"
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
               MOVE "N" TO WS-IN-TXN
           END-IF.
       8620-EXIT.
           EXIT.

       8650-NO-NULLS SECTION.
       8650-START.
           PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM.
       8650-EXIT.
           EXIT.

       8900-SQL-FAILED SECTION.
       8900-START.
           MOVE "Y" TO WS-ERROR
           MOVE 12 TO WS-RC
           MOVE "SQL0001" TO WS-REASON
           MOVE SQLIO-ERRMSG(1:200) TO WS-MSG
           IF WS-MSG = SPACES
               MOVE "The database refused the request" TO WS-MSG
           END-IF.
       8900-EXIT.
           EXIT.

       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           IF WS-RC < 8
               MOVE 8 TO WS-RC
           END-IF.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "B2B0009" TO WS-REASON
           MOVE SPACES TO WS-MSG
           STRING "Function " FUNCTION TRIM(WS-FUNC)
                  " is not supported by SOB2B; use LINK, "
                  "SUPPCHG, DIRECTDL or ENQUIRE"
               DELIMITED BY SIZE INTO WS-MSG
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9400-REPORT SECTION.
       9400-START.
           PERFORM 8620-ROLLBACK THRU 8620-EXIT
           MOVE WS-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           IF WS-REASON NOT = SPACES
               MOVE WS-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           END-IF
           IF WS-MSG NOT = SPACES
               MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           END-IF
           IF CA-MESSAGE OF DFHCOMMAREA NOT = SPACES
               MOVE 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF
      *>   these three answers are constant whatever happened
           MOVE "N" TO BB-ONHAND-CREATED
           MOVE "N" TO BB-SUPPLY-GUARANTEE
           MOVE "N" TO BB-INVOICE-APPROVED.
       9400-EXIT.
           EXIT.
