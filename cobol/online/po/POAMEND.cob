      *>================================================================
      *> PROGRAM-ID : POAMEND
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : SUPPLIER ACKNOWLEDGEMENTS, BUYER AMENDMENTS,
      *>              BLANKET CALL-OFF DRAW DOWN AND ORDER COPYING.
      *>              SHARES THE POORDERCA COMMAREA WITH POORDER,
      *>              WHICH DELEGATES THESE FOUR FUNCTIONS HERE.
      *> CALLED BY  : POORDER
      *> INTERFACE  : CALL "POAMEND" USING DFHCOMMAREA (POORDERCA)
      *> CALLS      : POAPPRV, SQLIO -> po.OrderAmendment,
      *>              po.PurchaseOrderLine, po.PurchaseSchedule,
      *>              po.BlanketBalance
      *>----------------------------------------------------------------
      *> AN ACKNOWLEDGEMENT DIFFERENCE IS A BUYER ACTION, NOT AN
      *> UNEXPLAINED OVERWRITE (PO-007).  THE ORDERED VALUE AND THE
      *> ACKNOWLEDGED VALUE ARE BOTH KEPT; THE PROMISED DATE KEEPS
      *> ITS ORIGINAL AS WELL AS ITS CURRENT VALUE, SO SUPPLIER
      *> PERFORMANCE CAN BE MEASURED ON EITHER BASIS (PO-023).
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1999-11-08 DJM  CR0902 ACKNOWLEDGEMENT DIFFERENCES
      *> 2004-10-11 DJM  CR1190 BLANKET CALL-OFFS
      *> 2026-10-06 STERP ORIGINAL VERSUS REVISED HISTORY
      *> 2026-10-07 STERP CR9015 BLANKET VALUE DRAW DOWN, COPY KEEPS
      *>                 ITS SOURCE ORDER REFERENCE
      *>================================================================
      *> REQ: PO-007, PO-008, PO-021, PO-023
       IDENTIFICATION DIVISION.
       PROGRAM-ID. POAMEND.
       AUTHOR. D J MILLER.
       DATE-WRITTEN. 08/11/1999.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY POAPPRVCA REPLACING ==DFHCOMMAREA== BY ==WS-APR-CA==
                      LEADING ==CA-== BY ==AP-==.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-LIN-ROW.
           05  WS-L-ID                 PIC X(20).
           05  WS-L-QTY                PIC X(24).
           05  WS-L-PRICE              PIC X(24).
           05  WS-L-DESC               PIC X(60).
           05  WS-L-STATUS             PIC X(12).
       01  WS-BLK-ROW.
           05  WS-B-ID                 PIC X(12).
           05  WS-B-AGREED-QTY         PIC X(24).
           05  WS-B-CALLED-QTY         PIC X(24).
           05  WS-B-AGREED-VAL         PIC X(24).
           05  WS-B-CALLED-VAL         PIC X(24).
           05  WS-B-STATUS             PIC X(10).
           05  WS-B-VALID              PIC X(1).
       01  WS-ONE-COL.
           05  WS-V1                   PIC X(24).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-ORD-QTY              PIC S9(13)V9(6) VALUE 0.
           05  WS-ORD-PRICE            PIC S9(11)V9(6) VALUE 0.
           05  WS-OLD-TEXT             PIC X(100).
           05  WS-NEW-TEXT             PIC X(100).
           05  WS-FIELD                PIC X(40).
           05  WS-MATERIAL             PIC X VALUE "N".
           05  WS-QTY-ED               PIC -(9)9.999999.
           05  WS-PRC-ED               PIC -(9)9.999999.
           05  WS-AMT-ED               PIC -(11)9.99.
           05  WS-DATE-ED              PIC X(8).
       LINKAGE SECTION.
       COPY POORDERCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           MOVE "POAMEND" TO CA-PROGRAM
           MOVE "N" TO WS-FATAL-SW
           EVALUATE CA-FUNCTION
               WHEN "ACKNOWL"
                   PERFORM 2000-ACKNOWLEDGE THRU 2000-EXIT
               WHEN "AMEND"
                   PERFORM 3000-AMEND THRU 3000-EXIT
               WHEN "CALLOFF"
                   PERFORM 4000-CALLOFF THRU 4000-EXIT
               WHEN "COPY"
                   PERFORM 5000-COPY THRU 5000-EXIT
               WHEN OTHER
                   MOVE "ORD0008" TO CA-REASON-CODE
                   MOVE 8 TO CA-RETURN-CODE
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> ACKNOWLEDGEMENT.  EVERY DIFFERENCE BECOMES AN AMENDMENT ROW
      *> WITH ITS ORIGINAL AND REVISED VALUE.  A PRICE OR QUANTITY
      *> DIFFERENCE IS MATERIAL AND COSTS THE ORDER ITS APPROVAL; A
      *> DATE DIFFERENCE IS A BUYER ACTION BUT NOT A REAPPROVAL.
      *>----------------------------------------------------------------
       2000-ACKNOWLEDGE SECTION.
       2000-START.
           PERFORM 8000-READ-LINE THRU 8000-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           MOVE "N" TO WS-MATERIAL
           MOVE 0 TO OR-AMEND-CNT
           PERFORM 8100-NEXT-AMEND THRU 8100-EXIT
           IF OR-ACK-QTY NOT = 0 AND OR-ACK-QTY NOT = WS-ORD-QTY
               MOVE "OrderedQty" TO WS-FIELD
               MOVE WS-ORD-QTY TO WS-QTY-ED
               MOVE WS-QTY-ED TO WS-OLD-TEXT
               MOVE OR-ACK-QTY TO WS-QTY-ED
               MOVE WS-QTY-ED TO WS-NEW-TEXT
               MOVE "Y" TO WS-MATERIAL
               PERFORM 8200-WRITE-AMEND THRU 8200-EXIT
           END-IF
           IF OR-ACK-PRICE NOT = 0 AND
              OR-ACK-PRICE NOT = WS-ORD-PRICE
               MOVE "UnitPrice" TO WS-FIELD
               MOVE WS-ORD-PRICE TO WS-PRC-ED
               MOVE WS-PRC-ED TO WS-OLD-TEXT
               MOVE OR-ACK-PRICE TO WS-PRC-ED
               MOVE WS-PRC-ED TO WS-NEW-TEXT
               MOVE "Y" TO WS-MATERIAL
               PERFORM 8200-WRITE-AMEND THRU 8200-EXIT
           END-IF
           IF OR-ACK-DATE NOT = 0 AND
              OR-ACK-DATE NOT = OR-SCH-REQ-DATE AND
              OR-SCH-REQ-DATE NOT = 0
               MOVE "ConfirmedDate" TO WS-FIELD
               MOVE OR-SCH-REQ-DATE TO WS-DATE-ED
               MOVE WS-DATE-ED TO WS-OLD-TEXT
               MOVE OR-ACK-DATE TO WS-DATE-ED
               MOVE WS-DATE-ED TO WS-NEW-TEXT
               PERFORM 8200-WRITE-AMEND THRU 8200-EXIT
               PERFORM 2100-SCHEDULE-DATE THRU 2100-EXIT
           END-IF
           PERFORM 2200-STAMP-LINE THRU 2200-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           PERFORM 2300-STAMP-HEADER THRU 2300-EXIT
           MOVE "ACKNOWLEDGE" TO OR-OUTCOME
           IF WS-MATERIAL = "Y"
               MOVE "Y" TO OR-MATERIAL-CHANGE
               MOVE "Y" TO OR-NEEDS-REAPPR
               PERFORM 3500-INVALIDATE THRU 3500-EXIT
               MOVE 4 TO CA-RETURN-CODE
               MOVE "ORD0015" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The acknowledgement from "
                      FUNCTION TRIM(OR-SUPPLIER)
                      " differs materially from order "
                      FUNCTION TRIM(OR-ORDER-NUMBER)
                      " - a buyer action and a new approval are "
                      "needed"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               IF OR-AMEND-CNT > 0
                   MOVE 4 TO CA-RETURN-CODE
                   MOVE "ORD0015" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Order " FUNCTION TRIM(OR-ORDER-NUMBER)
                          " acknowledged with a revised promised "
                          "date - raised as a buyer action"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               ELSE
                   MOVE 0 TO CA-RETURN-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Order " FUNCTION TRIM(OR-ORDER-NUMBER)
                          " acknowledged by "
                          FUNCTION TRIM(OR-SUPPLIER)
                          " exactly as ordered"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               END-IF
           END-IF.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE ORIGINAL CONFIRMED DATE IS WRITTEN ONLY ONCE.  IT IS THE
      *> PROMISE WE MEASURE THE SUPPLIER AGAINST FOR EVER AFTER.
      *>----------------------------------------------------------------
       2100-SCHEDULE-DATE SECTION.
       2100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.PurchaseSchedule SET "
                  "OriginalConfirmedDate = "
                  "ISNULL(OriginalConfirmedDate, TRY_CONVERT(date, "
                  "?)), CurrentConfirmedDate = TRY_CONVERT(date, ?),"
                  " ChangeReason = NULLIF(?, ''), ChangeCount = "
                  "ChangeCount + 1 WHERE PurchaseOrderLineId = ? AND"
                  " ScheduleNo = CASE WHEN ? = 0 THEN ScheduleNo "
                  "ELSE ? END"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE OR-ACK-DATE TO SQLIO-PARM-VAL(1)
           MOVE OR-ACK-DATE TO SQLIO-PARM-VAL(2)
           MOVE OR-ACK-NOTE TO SQLIO-PARM-VAL(3)
           MOVE OR-LINE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE OR-SCHEDULE-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE SQLIO-PARM-VAL(5) TO SQLIO-PARM-VAL(6)
           MOVE 6 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
           END-IF.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2200-STAMP-LINE SECTION.
       2200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.PurchaseOrderLine SET AckQty = "
                  "NULLIF(TRY_CONVERT(decimal(19,6), ?), 0), "
                  "AckUnitPrice = "
                  "NULLIF(TRY_CONVERT(decimal(19,6), ?), 0), "
                  "AckNote = NULLIF(?, '') WHERE "
                  "PurchaseOrderLineId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE OR-ACK-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE OR-ACK-PRICE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE OR-ACK-NOTE TO SQLIO-PARM-VAL(3)
           MOVE OR-LINE-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE 4 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
           END-IF.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2300-STAMP-HEADER SECTION.
       2300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.PurchaseOrder SET Status = 'ACKNOWLEDG"
                  "ED', AcknowledgedAt = SYSDATETIME(), "
                  "AcknowledgementNote = NULLIF(?, ''), "
                  "SupplierReference = COALESCE(NULLIF(?, ''), "
                  "SupplierReference), ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHERE CompanyCode = ? AND "
                  "OrderNumber = ? AND Status = 'ISSUED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE OR-ACK-NOTE TO SQLIO-PARM-VAL(1)
           MOVE OR-SUPPLIER-REF TO SQLIO-PARM-VAL(2)
           MOVE CA-USER TO SQLIO-PARM-VAL(3)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(4)
           MOVE OR-ORDER-NUMBER TO SQLIO-PARM-VAL(5)
           MOVE 5 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 2300-EXIT
           END-IF
           MOVE "ACKNOWLEDGED" TO OR-STATUS.
       2300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A BUYER AMENDMENT.  A REASON IS NOT OPTIONAL: THE WHOLE
      *> POINT OF THE AMENDMENT HISTORY IS TO ANSWER "WHY".
      *>----------------------------------------------------------------
       3000-AMEND SECTION.
       3000-START.
           IF OR-AMEND-REASON = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0016" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "An amendment to order "
                      FUNCTION TRIM(OR-ORDER-NUMBER)
                      " needs a reason"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 3000-EXIT
           END-IF
           IF OR-AMEND-FIELD = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0016" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "An amendment must name the field that "
                      "changed"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 3000-EXIT
           END-IF
           PERFORM 8100-NEXT-AMEND THRU 8100-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           MOVE OR-AMEND-FIELD TO WS-FIELD
           MOVE OR-AMEND-OLD TO WS-OLD-TEXT
           MOVE OR-AMEND-NEW TO WS-NEW-TEXT
           MOVE "N" TO WS-MATERIAL
           EVALUATE OR-AMEND-FIELD
               WHEN "SupplierCode"
                   MOVE "Y" TO WS-MATERIAL
               WHEN "OrderedQty"
                   MOVE "Y" TO WS-MATERIAL
               WHEN "UnitPrice"
                   MOVE "Y" TO WS-MATERIAL
               WHEN "TermsCode"
                   MOVE "Y" TO WS-MATERIAL
               WHEN "CurrencyCode"
                   MOVE "Y" TO WS-MATERIAL
               WHEN OTHER
                   MOVE "N" TO WS-MATERIAL
           END-EVALUATE
           PERFORM 8200-WRITE-AMEND THRU 8200-EXIT
           IF WS-FATAL
               GO TO 3000-EXIT
           END-IF
           MOVE "AMENDED" TO OR-OUTCOME
           IF WS-MATERIAL = "Y"
               MOVE "Y" TO OR-MATERIAL-CHANGE
               MOVE "Y" TO OR-NEEDS-REAPPR
               PERFORM 3500-INVALIDATE THRU 3500-EXIT
               PERFORM 3600-MARK-AMENDED THRU 3600-EXIT
               MOVE 6 TO CA-RETURN-CODE
               MOVE "ORD0012" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Order " FUNCTION TRIM(OR-ORDER-NUMBER)
                      " amended materially ("
                      FUNCTION TRIM(OR-AMEND-FIELD)
                      ") and must be approved again before reissue"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               MOVE 0 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Order " FUNCTION TRIM(OR-ORDER-NUMBER)
                      " amendment recorded: "
                      FUNCTION TRIM(OR-AMEND-REASON)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3500-INVALIDATE SECTION.
       3500-START.
           INITIALIZE WS-APR-CA
           MOVE "INVALID" TO AP-FUNCTION
           MOVE CA-COMPANY TO AP-COMPANY
           MOVE CA-SITE TO AP-SITE
           MOVE CA-USER TO AP-USER
           MOVE CA-BUSINESS-DATE TO AP-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO AP-CORRELATION-ID
           MOVE OR-ORDER-NUMBER TO AV-ORDER-NUMBER
           IF OR-AMEND-REASON NOT = SPACES
               MOVE OR-AMEND-REASON TO AV-CHANGE-REASON
           ELSE
               MOVE SPACES TO AV-CHANGE-REASON
               STRING "Supplier acknowledgement differs from the "
                      "approved order"
                   DELIMITED BY SIZE INTO AV-CHANGE-REASON
           END-IF
           CALL "POAPPRV" USING WS-APR-CA
           MOVE AV-CONTENT-HASH TO OR-CONTENT-HASH.
       3500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3600-MARK-AMENDED SECTION.
       3600-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.PurchaseOrder SET Status = 'AMENDED', "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() WHERE "
                  "CompanyCode = ? AND OrderNumber = ? AND Status "
                  "NOT IN ('CLOSED', 'CANCELLED')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-USER TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           MOVE OR-ORDER-NUMBER TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 3600-EXIT
           END-IF
           MOVE "AMENDED" TO OR-STATUS.
       3600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> BLANKET CALL-OFF.  THE AGREED QUANTITY AND VALUE ARE A
      *> CEILING.  REPEATING AN ACCEPTED CALL-OFF CANNOT ISSUE A
      *> DUPLICATE OBLIGATION BECAUSE THE DRAW DOWN IS CONDITIONAL
      *> ON THE REMAINING BALANCE (PO-021).
      *>----------------------------------------------------------------
       4000-CALLOFF SECTION.
       4000-START.
           IF OR-BLANKET-NUMBER = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0017" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A call-off must name the blanket order it "
                      "draws on"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
      *>     POORDER CLEARS THE LINE VALUE ON ENTRY BECAUSE IT IS AN
      *>     ANSWER FIELD EVERYWHERE ELSE, SO A CALL-OFF ARRIVING
      *>     THROUGH POORDER USED TO DRAW DOWN THE AGREED QUANTITY
      *>     BUT NOT THE AGREED VALUE.  PRICE THE CALL-OFF HERE WHEN
      *>     THE CALLER HAS NOT PRICED IT (PO-021).
      *>     2026-10-07 STERP CR9015 BLANKET VALUE NOT DRAWN DOWN
           IF OR-PRICE-PER <= 0
               MOVE 1 TO OR-PRICE-PER
           END-IF
           IF OR-LINE-NET-GBP = 0 AND OR-UNIT-PRICE > 0
               COMPUTE OR-LINE-NET-GBP ROUNDED =
                       (OR-UNIT-PRICE / OR-PRICE-PER) *
                       OR-ORDERED-QTY
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(b.BlanketBalanceId AS varchar(12)), "
                  "CAST(b.AgreedQty AS varchar(24)), "
                  "CAST(b.CalledOffQty AS varchar(24)), "
                  "CAST(b.AgreedValue AS varchar(24)), "
                  "CAST(b.CalledOffValue AS varchar(24)), b.Status, "
                  "CASE WHEN (b.ValidFrom IS NULL OR b.ValidFrom <= "
                  "TRY_CONVERT(date, ?)) AND (b.ValidTo IS NULL OR "
                  "b.ValidTo >= TRY_CONVERT(date, ?)) THEN 'Y' ELSE "
                  "'N' END FROM po.BlanketBalance b WHERE "
                  "b.CompanyCode = ? AND b.BlanketOrderNumber = ? "
                  "AND b.LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE OR-ORDER-DATE TO SQLIO-PARM-VAL(1)
           MOVE OR-ORDER-DATE TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE OR-BLANKET-NUMBER TO SQLIO-PARM-VAL(4)
           MOVE OR-LINE-NUM TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE 5 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 7 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 24 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 10 TO SQLIO-COL-WIDTH(6)
           MOVE 1 TO SQLIO-COL-WIDTH(7)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-BLK-ROW
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "ORD0011" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Blanket order "
                      FUNCTION TRIM(OR-BLANKET-NUMBER)
                      " has no balance line to call off against"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
           COMPUTE OR-BLANKET-QTY-LEFT =
                   FUNCTION NUMVAL(WS-B-AGREED-QTY) -
                   FUNCTION NUMVAL(WS-B-CALLED-QTY)
           COMPUTE OR-BLANKET-VAL-LEFT =
                   FUNCTION NUMVAL(WS-B-AGREED-VAL) -
                   FUNCTION NUMVAL(WS-B-CALLED-VAL)
           IF WS-B-STATUS NOT = "OPEN" OR WS-B-VALID NOT = "Y"
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0011" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Blanket order "
                      FUNCTION TRIM(OR-BLANKET-NUMBER)
                      " is " FUNCTION TRIM(WS-B-STATUS)
                      " or outside its validity dates"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
           IF OR-ORDERED-QTY > OR-BLANKET-QTY-LEFT AND
              OR-BLANKET-QTY-LEFT >= 0 AND
              FUNCTION NUMVAL(WS-B-AGREED-QTY) > 0
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0011" TO CA-REASON-CODE
               MOVE OR-BLANKET-QTY-LEFT TO WS-QTY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Only " FUNCTION TRIM(WS-QTY-ED)
                      " remains on blanket order "
                      FUNCTION TRIM(OR-BLANKET-NUMBER)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.BlanketBalance SET CalledOffQty = "
                  "CalledOffQty + ?, CalledOffValue = CalledOffValue"
                  " + ?, Status = CASE WHEN AgreedQty > 0 AND "
                  "CalledOffQty + ? >= AgreedQty THEN 'EXHAUSTED' "
                  "ELSE Status END, ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() OUTPUT CAST(INSERTED.AgreedQty - "
                  "INSERTED.CalledOffQty AS varchar(24)) WHERE "
                  "BlanketBalanceId = ? AND (AgreedQty = 0 OR "
                  "CalledOffQty + ? <= AgreedQty)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE OR-ORDERED-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE OR-LINE-NET-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE SQLIO-PARM-VAL(1) TO SQLIO-PARM-VAL(3)
           MOVE CA-USER TO SQLIO-PARM-VAL(4)
           MOVE WS-B-ID TO SQLIO-PARM-VAL(5)
           MOVE SQLIO-PARM-VAL(1) TO SQLIO-PARM-VAL(6)
           MOVE 6 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0011" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The call-off would exceed blanket order "
                      FUNCTION TRIM(OR-BLANKET-NUMBER)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
           COMPUTE OR-BLANKET-QTY-LEFT = FUNCTION NUMVAL(WS-V1)
           MOVE "CAPTURED" TO OR-OUTCOME
           MOVE OR-BLANKET-QTY-LEFT TO WS-QTY-ED
           MOVE SPACES TO CA-MESSAGE
           STRING "Call-off recorded against blanket order "
                  FUNCTION TRIM(OR-BLANKET-NUMBER) "; "
                  FUNCTION TRIM(WS-QTY-ED) " remains"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> COPY.  THE SOURCE REFERENCE IS KEPT, THE RECEIPT, INVOICE
      *> AND APPROVAL STATE IS NOT.  A COPY IS A NEW DRAFT, NEVER AN
      *> INHERITED COMMITMENT (PO-021).
      *>----------------------------------------------------------------
       5000-COPY SECTION.
       5000-START.
           IF OR-COPY-FROM = SPACES OR OR-ORDER-NUMBER = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "ORD0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A copy needs both the source order and the "
                      "new order number"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.PurchaseOrderLine (PurchaseOrderId, "
                  "CompanyCode, OrderNumber, LineNum, ItemCode, "
                  "Revision, Description, LineType, OrderedQty, "
                  "UomCode, ConversionFactor, UnitPrice, "
                  "PricePerQty, PriceSource, DiscountPct, "
                  "ChargeAmount, NetAmount, VatCode, VatRatePct, "
                  "ExpectedVatAmount, IsRecoverableVat, "
                  "NonRecoverableVatGBP, AccountCode, DimCostCentre,"
                  " DimProject, JobRef, DestinationSiteCode, "
                  "DestinationLocationCode, Status) SELECT n."
                  "PurchaseOrderId, l.CompanyCode, n.OrderNumber, "
                  "l.LineNum, l.ItemCode, l.Revision, l.Description,"
                  " l.LineType, l.OrderedQty, l.UomCode, "
                  "l.ConversionFactor, l.UnitPrice, l.PricePerQty, "
                  "l.PriceSource, l.DiscountPct, l.ChargeAmount, "
                  "l.NetAmount, l.VatCode, l.VatRatePct, "
                  "l.ExpectedVatAmount, l.IsRecoverableVat, "
                  "l.NonRecoverableVatGBP, l.AccountCode, "
                  "l.DimCostCentre, l.DimProject, l.JobRef, "
                  "l.DestinationSiteCode, l.DestinationLocationCode,"
                  " 'OPEN' FROM po.PurchaseOrderLine l CROSS JOIN "
                  "po.PurchaseOrder n WHERE l.CompanyCode = ? AND "
                  "l.OrderNumber = ? AND l.Status <> 'CANCELLED' AND"
                  " n.CompanyCode = ? AND n.OrderNumber = ? AND NOT "
                  "EXISTS (SELECT 1 FROM po.PurchaseOrderLine x "
                  "WHERE x.PurchaseOrderId = n.PurchaseOrderId)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE OR-COPY-FROM TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE OR-ORDER-NUMBER TO SQLIO-PARM-VAL(4)
           MOVE 4 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           MOVE SQLIO-ROWCOUNT TO OR-LINE-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE o SET NetAmount = t.Net, VatAmount = "
                  "t.Vat, GrossAmount = t.Net + t.Vat, "
                  "CommitmentGBP = 0, Status = 'DRAFT', ApprovalId ="
                  " NULL, ContentHash = NULL, ApprovedBy = NULL, "
                  "ApprovedAt = NULL, IssuedAt = NULL, "
                  "AcknowledgedAt = NULL, DeliveryStatus = "
                  "'NOTRECEIVED', MatchStatus = 'UNINVOICED', "
                  "SupplierReference = NULL, "
                  "CopiedFromOrderNumber = NULLIF(?, ''), "
                  "ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() FROM po.PurchaseOrder "
                  "o CROSS APPLY (SELECT ISNULL(SUM(l.NetAmount), 0)"
                  " AS Net, ISNULL(SUM(l.ExpectedVatAmount), 0) AS "
                  "Vat FROM po.PurchaseOrderLine l WHERE "
                  "l.PurchaseOrderId = o.PurchaseOrderId) t WHERE "
                  "o.CompanyCode = ? AND o.OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE OR-COPY-FROM TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE OR-ORDER-NUMBER TO SQLIO-PARM-VAL(4)
           MOVE 4 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           MOVE "DRAFT" TO OR-STATUS
           MOVE "CAPTURED" TO OR-OUTCOME
           MOVE SPACES TO CA-MESSAGE
           STRING "Order " FUNCTION TRIM(OR-ORDER-NUMBER)
                  " copied from " FUNCTION TRIM(OR-COPY-FROM)
                  " as a new draft with no receipt or payment state"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8000-READ-LINE SECTION.
       8000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(l.PurchaseOrderLineId AS "
                  "varchar(20)), CAST(l.OrderedQty AS varchar(24)), "
                  "CAST(l.UnitPrice AS varchar(24)), "
                  "LEFT(l.Description, 60), l.Status FROM "
                  "po.PurchaseOrderLine l WHERE l.CompanyCode = ? "
                  "AND l.OrderNumber = ? AND l.LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE OR-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE OR-LINE-NUM TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE 24 TO SQLIO-COL-WIDTH(2)
           MOVE 24 TO SQLIO-COL-WIDTH(3)
           MOVE 60 TO SQLIO-COL-WIDTH(4)
           MOVE 12 TO SQLIO-COL-WIDTH(5)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LIN-ROW
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 8000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE
               MOVE "ORD0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "That line is not on order "
                      FUNCTION TRIM(OR-ORDER-NUMBER)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 8000-EXIT
           END-IF
           COMPUTE OR-LINE-ID = FUNCTION NUMVAL(WS-L-ID)
           COMPUTE WS-ORD-QTY = FUNCTION NUMVAL(WS-L-QTY)
           COMPUTE WS-ORD-PRICE = FUNCTION NUMVAL(WS-L-PRICE)
           MOVE WS-L-DESC TO OR-DESCRIPTION.
       8000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8100-NEXT-AMEND SECTION.
       8100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(MAX(AmendmentNo), 0) + 1 AS "
                  "varchar(24)) FROM po.OrderAmendment WHERE "
                  "CompanyCode = ? AND OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE OR-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 8100-EXIT
           END-IF
           COMPUTE OR-AMEND-NO = FUNCTION NUMVAL(WS-V1).
       8100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8200-WRITE-AMEND SECTION.
       8200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.OrderAmendment (CompanyCode, "
                  "OrderNumber, LineNum, ScheduleNo, AmendmentNo, "
                  "Source, FieldName, OriginalValue, RevisedValue, "
                  "IsMaterial, RequiresReapproval, "
                  "AffectedSalesOrder, OwnerUser, Reason, AmendedBy,"
                  " Status) VALUES (?, ?, NULLIF(?, 0), NULLIF(?, "
                  "0), ?, ?, ?, NULLIF(?, ''), NULLIF(?, ''), ?, ?, "
                  "NULLIF(?, ''), NULLIF(?, ''), NULLIF(?, ''), ?, "
                  "'OPEN')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE OR-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE OR-LINE-NUM TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE OR-SCHEDULE-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE OR-AMEND-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           IF CA-FUNCTION = "ACKNOWL"
               MOVE "ACK" TO SQLIO-PARM-VAL(6)
           ELSE
               MOVE "BUYER" TO SQLIO-PARM-VAL(6)
           END-IF
           MOVE WS-FIELD TO SQLIO-PARM-VAL(7)
           MOVE WS-OLD-TEXT TO SQLIO-PARM-VAL(8)
           MOVE WS-NEW-TEXT TO SQLIO-PARM-VAL(9)
           IF WS-MATERIAL = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(10)
               MOVE "1" TO SQLIO-PARM-VAL(11)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(10)
               MOVE "0" TO SQLIO-PARM-VAL(11)
           END-IF
           MOVE OR-LINKED-SO TO SQLIO-PARM-VAL(12)
           MOVE OR-BUYER TO SQLIO-PARM-VAL(13)
           IF OR-AMEND-REASON NOT = SPACES
               MOVE OR-AMEND-REASON TO SQLIO-PARM-VAL(14)
           ELSE
               MOVE OR-ACK-NOTE TO SQLIO-PARM-VAL(14)
           END-IF
           MOVE CA-USER TO SQLIO-PARM-VAL(15)
           MOVE 15 TO SQLIO-PARM-CNT
           PERFORM 9300-NO-NULLS THRU 9300-EXIT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 9000-SQL-FAIL THRU 9000-EXIT
               GO TO 8200-EXIT
           END-IF
           ADD 1 TO OR-AMEND-CNT
           ADD 1 TO OR-AMEND-NO.
       8200-EXIT.
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
       9300-NO-NULLS SECTION.
       9300-START.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-I)
           END-PERFORM.
       9300-EXIT.
           EXIT.
       END PROGRAM POAMEND.
