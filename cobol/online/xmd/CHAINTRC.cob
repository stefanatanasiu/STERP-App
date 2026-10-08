      *>================================================================
      *> PROGRAM-ID : CHAINTRC
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : CROSS MODULE CONTROLS AND REPORTING (XMD)
      *> PURPOSE    : BUSINESS CHAIN TRACE ENQUIRY.  GIVEN ONE
      *>              DOCUMENT REFERENCE THE PROGRAM ASSEMBLES THE
      *>              ORDERED STEPS OF ITS CHAIN - ORDER TO CASH FOR
      *>              A SALES ORDER, PROCURE TO PAY FOR A PURCHASE
      *>              ORDER AND THE ASSEMBLY CHAIN FOR AN EXECUTION
      *>              KIT - TOGETHER WITH THE FINANCIAL EFFECTS
      *>              POSTED FOR IT.  PARTIAL COMPLETION IS ALWAYS
      *>              VISIBLE: ONE STEP BEING COMPLETE NEVER IMPLIES
      *>              THAT ITS SUCCESSOR HAS HAPPENED.  RETURNS,
      *>              CREDITS AND REVERSALS ARE MARKED AS SEPARATE
      *>              CONSEQUENCES SO THAT A PHYSICAL RETURN IS NOT
      *>              MISTAKEN FOR A FINANCIAL CREDIT.
      *> CALLED BY  : CHAINRPT, XMDCTL (.NET)
      *> INTERFACE  : CALL "CHAINTRC" USING WS-CHAINTRC-CA
      *>              COPYBOOK CHAINTRCCA, FUNCTION TRACE
      *> CALLS      : SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-06-30 RJH  CR0655 ORIGINAL DOCUMENT TRAIL ENQUIRY
      *> 1999-11-30 KMP  Y2K - 4 DIGIT YEARS THROUGHOUT
      *> 2005-02-14 DJM  CR1811 REVERSALS SHOWN AS THEIR OWN STEP
      *> 2026-10-06 STERP XMD-001..004 ONE TRACEABLE CHAIN
      *>================================================================
      *> REQ: XMD-001, XMD-002, XMD-003, XMD-004, XMD-012
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHAINTRC.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 30/06/1997.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       01  WS-ROW.
           05  WS-COL OCCURS 12 TIMES  PIC X(120).
       01  WS-WORK.
           05  WS-RC                   PIC 9(2) VALUE 0.
           05  WS-REASON               PIC X(8) VALUE SPACES.
           05  WS-MSG                  PIC X(200) VALUE SPACES.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-N                    PIC S9(4) VALUE 0.
           05  WS-TEXT                 PIC X(200) VALUE SPACES.
           05  WS-DATE                 PIC 9(8) VALUE 0.
           05  WS-DATE-ED              PIC 9(8) VALUE 0.
           05  WS-STAGE                PIC X(24) VALUE SPACES.
           05  WS-FOUND                PIC X VALUE "N".
           05  WS-OPER                 PIC S9(4) VALUE 0.
           05  WS-FIN                  PIC S9(4) VALUE 0.
       LINKAGE SECTION.
       COPY CHAINTRCCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE THRU 0100-EXIT
           IF WS-RC > 4
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION OF DFHCOMMAREA
               WHEN "TRACE"  PERFORM 1000-TRACE THRU 1000-EXIT
               WHEN OTHER    PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           PERFORM 9900-SET-RESULT
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           MOVE 0 TO WS-RC
           MOVE SPACES TO WS-REASON
           MOVE SPACES TO WS-MSG
           MOVE 0 TO CT-STEP-CNT
           MOVE 24 TO CT-MAX-STEPS
           MOVE 0 TO CT-REVERSALS
           MOVE 0 TO CT-TOTAL-GBP
           MOVE "N" TO CT-TRUNCATED
           MOVE "N" TO CT-COMPLETE
           MOVE "N" TO CT-BROKEN
           MOVE SPACES TO CT-CHAIN-TYPE
           MOVE SPACES TO CT-CHAIN-NAME
           MOVE SPACES TO CT-PARTY
           MOVE 0 TO WS-OPER
           MOVE 0 TO WS-FIN
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 24
               MOVE 0 TO CT-SEQ(WS-I)
               MOVE SPACES TO CT-STAGE(WS-I)
               MOVE SPACES TO CT-DOC-T(WS-I)
               MOVE SPACES TO CT-REF(WS-I)
               MOVE 0 TO CT-DATE(WS-I)
               MOVE SPACES TO CT-STATUS(WS-I)
               MOVE 0 TO CT-QTY(WS-I)
               MOVE 0 TO CT-VALUE(WS-I)
               MOVE SPACES TO CT-KIND(WS-I)
               MOVE SPACES TO CT-NOTE(WS-I)
           END-PERFORM
           IF CA-COMPANY OF DFHCOMMAREA = SPACES
               MOVE 8 TO WS-RC
               MOVE "XMD0001" TO WS-REASON
               MOVE "A company is required for a chain trace."
                   TO WS-MSG
               GO TO 0100-EXIT
           END-IF
           IF CT-DOC-REF = SPACES
               MOVE 8 TO WS-RC
               MOVE "XMD0002" TO WS-REASON
               MOVE "A document reference is required." TO WS-MSG
               GO TO 0100-EXIT
           END-IF
           MOVE FUNCTION UPPER-CASE(CT-DOC-TYPE) TO CT-DOC-TYPE
           EVALUATE CT-DOC-TYPE
               WHEN "ORDER"
               WHEN "SALESORDER"
                   MOVE "O2C" TO CT-CHAIN-TYPE
                   MOVE "Order to cash" TO CT-CHAIN-NAME
               WHEN "PO"
               WHEN "PURCHASE"
                   MOVE "P2P" TO CT-CHAIN-TYPE
                   MOVE "Procure to pay" TO CT-CHAIN-NAME
               WHEN "KIT"
                   MOVE "ASSEMBLY" TO CT-CHAIN-TYPE
                   MOVE "Make an assembly" TO CT-CHAIN-NAME
               WHEN OTHER
                   MOVE 8 TO WS-RC
                   MOVE "XMD0003" TO WS-REASON
                   MOVE "Document type must be ORDER, PO or KIT."
                       TO WS-MSG
           END-EVALUATE.
       0100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-001, XMD-002, XMD-003  the operational steps first,
      *> then the financial effects of the same chain
      *>----------------------------------------------------------------
       1000-TRACE SECTION.
       1000-START.
           EVALUATE CT-CHAIN-TYPE
               WHEN "O2C"      PERFORM 2000-O2C THRU 2000-EXIT
               WHEN "P2P"      PERFORM 3000-P2P THRU 3000-EXIT
               WHEN "ASSEMBLY" PERFORM 4000-KIT THRU 4000-EXIT
           END-EVALUATE
           PERFORM 5000-FINANCIAL THRU 5000-EXIT
           PERFORM 6000-JUDGE THRU 6000-EXIT.
       1000-EXIT.
           EXIT.

       2000-O2C SECTION.
       2000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT 'Accepted schedules', CONVERT(varchar(24)"
                  ",SUM(s.Quantity)), CONVERT(varchar(24),SUM("
                  "ISNULL(s.DespatchedQty,0))), CONVERT(varchar(12)"
                  ",COUNT(*)), CONVERT(varchar(12),SUM(CASE WHEN "
                  "s.Status = 'CANCELLED' THEN 1 ELSE 0 END)), "
                  "ISNULL(MIN(CONVERT(char(8),"
                  "s.OriginalPromiseDate,112)),'') FROM "
                  "so.DeliverySchedule s WHERE s.CompanyCode = ? "
                  "AND s.OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-COL-CNT
           PERFORM 8000-RUN-SINGLE THRU 8000-EXIT
           IF WS-FOUND = "Y" AND WS-COL(4) NOT = "0"
               MOVE "Sales order schedules" TO WS-STAGE
               PERFORM 8200-ADD-OPERATIONAL THRU 8200-EXIT
               MOVE "ORDER" TO CT-DOC-T(CT-STEP-CNT)
               MOVE CT-DOC-REF TO CT-REF(CT-STEP-CNT)
               COMPUTE CT-QTY(CT-STEP-CNT) =
                   FUNCTION NUMVAL(WS-COL(2))
               MOVE "ACCEPTED" TO CT-STATUS(CT-STEP-CNT)
               MOVE SPACES TO WS-TEXT
               STRING "Scheduled " WS-COL(2)(1:12) " despatched "
                      WS-COL(3)(1:12) " cancelled lines "
                      WS-COL(5)(1:4)
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT(1:60) TO CT-NOTE(CT-STEP-CNT)
               MOVE WS-COL(6) TO WS-TEXT
               PERFORM 8100-ISO-DATE THRU 8100-EXIT
               MOVE WS-DATE TO CT-DATE(CT-STEP-CNT)
           ELSE
               MOVE "Y" TO CT-BROKEN
               MOVE 16 TO WS-RC
               MOVE "XMD0016" TO WS-REASON
               MOVE "No sales order schedule carries that number."
                   TO WS-MSG
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT h.DespatchRef, CONVERT(char(8),"
                  "h.DespatchDate,112), h.Status, ISNULL("
                  "h.CustomerCode,''), CONVERT(varchar(24),ISNULL("
                  "h.CogsPostedGBP,0)) FROM so.Despatch h WHERE "
                  "h.CompanyCode = ? AND h.OrderNumber = ? ORDER BY"
                  " h.DespatchDate, h.DespatchRef"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-COL-CNT
           MOVE "Despatch" TO WS-STAGE
           MOVE "DESPATCH" TO CT-DOC-TYPE
           PERFORM 8300-RUN-LIST THRU 8300-EXIT.
       2000-EXIT.
           EXIT.

       3000-P2P SECTION.
       3000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT 'Approved schedules', CONVERT(varchar(24)"
                  ",SUM(s.Quantity)), CONVERT(varchar(24),SUM("
                  "ISNULL(s.ReceivedQty,0))), CONVERT(varchar(12),"
                  "COUNT(*)), '0', ISNULL(MIN(CONVERT(char(8),"
                  "s.OriginalConfirmedDate,112)),'') FROM "
                  "po.PurchaseSchedule s WHERE s.CompanyCode = ? "
                  "AND s.OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-COL-CNT
           PERFORM 8000-RUN-SINGLE THRU 8000-EXIT
           IF WS-FOUND = "Y" AND WS-COL(4) NOT = "0"
               MOVE "Purchase order schedules" TO WS-STAGE
               PERFORM 8200-ADD-OPERATIONAL THRU 8200-EXIT
               MOVE "PO" TO CT-DOC-T(CT-STEP-CNT)
               MOVE CT-DOC-REF TO CT-REF(CT-STEP-CNT)
               COMPUTE CT-QTY(CT-STEP-CNT) =
                   FUNCTION NUMVAL(WS-COL(2))
               MOVE "APPROVED" TO CT-STATUS(CT-STEP-CNT)
               MOVE SPACES TO WS-TEXT
               STRING "Ordered " WS-COL(2)(1:12) " received "
                      WS-COL(3)(1:12) " unreceived commitment "
                      "remains open"
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT(1:60) TO CT-NOTE(CT-STEP-CNT)
               MOVE WS-COL(6) TO WS-TEXT
               PERFORM 8100-ISO-DATE THRU 8100-EXIT
               MOVE WS-DATE TO CT-DATE(CT-STEP-CNT)
           ELSE
               MOVE "Y" TO CT-BROKEN
               MOVE 16 TO WS-RC
               MOVE "XMD0016" TO WS-REASON
               MOVE "No purchase schedule carries that number."
                   TO WS-MSG
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT h.ReceiptRef, CONVERT(char(8),"
                  "h.PhysicalDate,112), h.Status, ISNULL("
                  "h.SupplierCode,''), CONVERT(varchar(24),ISNULL("
                  "h.GrniValueGBP,0)) FROM po.Receipt h WHERE "
                  "h.CompanyCode = ? AND h.PurchaseOrderNumber = ? "
                  "ORDER BY h.PhysicalDate, h.ReceiptRef"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-COL-CNT
           MOVE "Receipt and GRNI" TO WS-STAGE
           MOVE "RECEIPT" TO CT-DOC-TYPE
           PERFORM 8300-RUN-LIST THRU 8300-EXIT.
       3000-EXIT.
           EXIT.

       4000-KIT SECTION.
       4000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT k.ParentItemCode, CONVERT(varchar(24),"
                  "k.PlannedGoodQty), CONVERT(varchar(24),ISNULL("
                  "k.CompletedGoodQty,0)), k.Status, ISNULL("
                  "k.SourceDemandRef,''), CONVERT(varchar(24),"
                  "ISNULL(k.WipValueGBP,0)) FROM kit.ExecutionKit k"
                  " WHERE k.CompanyCode = ? AND k.KitRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-COL-CNT
           PERFORM 8000-RUN-SINGLE THRU 8000-EXIT
           IF WS-FOUND = "N"
               MOVE "Y" TO CT-BROKEN
               MOVE 16 TO WS-RC
               MOVE "XMD0016" TO WS-REASON
               MOVE "No execution kit carries that reference."
                   TO WS-MSG
               GO TO 4000-EXIT
           END-IF
           MOVE "Execution kit" TO WS-STAGE
           PERFORM 8200-ADD-OPERATIONAL THRU 8200-EXIT
           MOVE "KIT" TO CT-DOC-T(CT-STEP-CNT)
           MOVE CT-DOC-REF TO CT-REF(CT-STEP-CNT)
           COMPUTE CT-QTY(CT-STEP-CNT) = FUNCTION NUMVAL(WS-COL(2))
           COMPUTE CT-VALUE(CT-STEP-CNT) =
               FUNCTION NUMVAL(WS-COL(6))
           MOVE WS-COL(4) TO CT-STATUS(CT-STEP-CNT)
           MOVE WS-COL(1) TO CT-PARTY
           MOVE SPACES TO WS-TEXT
           STRING "Parent " WS-COL(1)(1:20) " planned "
                  WS-COL(2)(1:10) " completed " WS-COL(3)(1:10)
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT(1:60) TO CT-NOTE(CT-STEP-CNT)
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT 'Component issues', ISNULL(CONVERT("
                  "char(8),MAX(k.ModifiedAt),112),''), 'ISSUED', '"
                  "', CONVERT(varchar(24),SUM(ISNULL("
                  "l.IssuedValueGBP,0))) FROM kit.ExecutionKitLine "
                  "l JOIN kit.ExecutionKit k ON k.ExecutionKitId = "
                  "l.ExecutionKitId WHERE k.CompanyCode = ? AND "
                  "k.KitRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-COL-CNT
           MOVE "Material issue" TO WS-STAGE
           MOVE "KITLINE" TO CT-DOC-TYPE
           PERFORM 8300-RUN-LIST THRU 8300-EXIT.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-004  reversals, credits and returns are listed as
      *> their own consequences, never folded into the original step
      *>----------------------------------------------------------------
       5000-FINANCIAL SECTION.
       5000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT j.JournalNumber, CONVERT(char(8),"
                  "j.PostingDate,112), j.Status, ISNULL("
                  "j.EventCode,j.SourceModule), CONVERT(varchar(24)"
                  ",ISNULL(j.TotalDebitGBP,0)), CONVERT(char(1),"
                  "ISNULL(j.IsReversal,0)), ISNULL(j.SourceModule,'"
                  "') FROM fin.Journal j WHERE j.CompanyCode = ? "
                  "AND ISNULL(j.SourceRef,'') = ? ORDER BY "
                  "j.PostingDate, j.JournalNumber"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-COL-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE CT-DOC-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 8400-SET-WIDTHS THRU 8400-EXIT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               GO TO 5000-EXIT
           END-IF
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ROW
               IF SQLIO-OK AND CT-STEP-CNT < CT-MAX-STEPS
                   ADD 1 TO CT-STEP-CNT
                   ADD 1 TO WS-FIN
                   MOVE CT-STEP-CNT TO CT-SEQ(CT-STEP-CNT)
                   MOVE "JOURNAL" TO CT-DOC-T(CT-STEP-CNT)
                   MOVE WS-COL(1) TO CT-REF(CT-STEP-CNT)
                   MOVE WS-COL(2) TO WS-TEXT
                   PERFORM 8100-ISO-DATE THRU 8100-EXIT
                   MOVE WS-DATE TO CT-DATE(CT-STEP-CNT)
                   MOVE WS-COL(3) TO CT-STATUS(CT-STEP-CNT)
                   COMPUTE CT-VALUE(CT-STEP-CNT) =
                       FUNCTION NUMVAL(WS-COL(5))
                   ADD CT-VALUE(CT-STEP-CNT) TO CT-TOTAL-GBP
                   IF WS-COL(6)(1:1) = "1"
                       MOVE "REVERSAL" TO CT-KIND(CT-STEP-CNT)
                       MOVE "Reversal or credit"
                           TO CT-STAGE(CT-STEP-CNT)
                       ADD 1 TO CT-REVERSALS
                   ELSE
                       MOVE "FINANCIAL" TO CT-KIND(CT-STEP-CNT)
                       MOVE "Financial effect"
                           TO CT-STAGE(CT-STEP-CNT)
                   END-IF
                   MOVE SPACES TO WS-TEXT
                   STRING "Event " WS-COL(4)(1:24) " raised by "
                          WS-COL(7)(1:10)
                       DELIMITED BY SIZE INTO WS-TEXT
                   MOVE WS-TEXT(1:60) TO CT-NOTE(CT-STEP-CNT)
               ELSE
                   IF SQLIO-OK
                       MOVE "Y" TO CT-TRUNCATED
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       5000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> REQ: XMD-012  partial completion is stated, not implied
      *>----------------------------------------------------------------
       6000-JUDGE SECTION.
       6000-START.
           IF CT-STEP-CNT = 0
               MOVE "Y" TO CT-BROKEN
               IF WS-RC < 16
                   MOVE 16 TO WS-RC
                   MOVE "XMD0016" TO WS-REASON
                   MOVE "Nothing is recorded for that reference."
                       TO WS-MSG
               END-IF
               GO TO 6000-EXIT
           END-IF
           IF WS-OPER > 1 AND WS-FIN > 0
               MOVE "Y" TO CT-COMPLETE
           END-IF
           IF WS-FIN = 0
               MOVE "Y" TO CT-BROKEN
               MOVE 4 TO WS-RC
               MOVE "XMD0004" TO WS-REASON
               MOVE "The chain has no posted financial effect yet."
                   TO WS-MSG
           END-IF
           IF CT-TRUNCATED = "Y"
               MOVE 4 TO WS-RC
               MOVE "XMD0005" TO WS-REASON
               MOVE "More steps exist than the enquiry can return."
                   TO WS-MSG
           END-IF.
       6000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       8000-RUN-SINGLE SECTION.
       8000-START.
           MOVE "N" TO WS-FOUND
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE CT-DOC-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 8400-SET-WIDTHS THRU 8400-EXIT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-OK
               MOVE "Y" TO WS-FOUND
           END-IF
           IF SQLIO-ERROR
               MOVE 12 TO WS-RC
               MOVE "SQL0001" TO WS-REASON
               MOVE SQLIO-ERRMSG(1:200) TO WS-MSG
           END-IF.
       8000-EXIT.
           EXIT.

       8100-ISO-DATE SECTION.
       8100-START.
           MOVE 0 TO WS-DATE
           IF WS-TEXT(1:8) NUMERIC
               COMPUTE WS-DATE = FUNCTION NUMVAL(WS-TEXT(1:8))
           ELSE
               IF WS-TEXT(5:1) = "-"
                   MOVE WS-TEXT(1:4) TO WS-DATE-ED(1:4)
                   MOVE WS-TEXT(6:2) TO WS-DATE-ED(5:2)
                   MOVE WS-TEXT(9:2) TO WS-DATE-ED(7:2)
                   MOVE WS-DATE-ED TO WS-DATE
               END-IF
           END-IF.
       8100-EXIT.
           EXIT.

       8200-ADD-OPERATIONAL SECTION.
       8200-START.
           IF CT-STEP-CNT >= CT-MAX-STEPS
               MOVE "Y" TO CT-TRUNCATED
               GO TO 8200-EXIT
           END-IF
           ADD 1 TO CT-STEP-CNT
           ADD 1 TO WS-OPER
           MOVE CT-STEP-CNT TO CT-SEQ(CT-STEP-CNT)
           MOVE WS-STAGE TO CT-STAGE(CT-STEP-CNT)
           MOVE "OPERATION" TO CT-KIND(CT-STEP-CNT).
       8200-EXIT.
           EXIT.

       8300-RUN-LIST SECTION.
       8300-START.
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE CT-DOC-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(1) SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 8400-SET-WIDTHS THRU 8400-EXIT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               GO TO 8300-EXIT
           END-IF
           PERFORM UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ROW
               IF SQLIO-OK
                   IF WS-COL(1) = SPACES
                       CONTINUE
                   ELSE
                       PERFORM 8200-ADD-OPERATIONAL THRU 8200-EXIT
                       IF CT-TRUNCATED = "N"
                           MOVE CT-DOC-TYPE
                               TO CT-DOC-T(CT-STEP-CNT)
                           MOVE WS-COL(1) TO CT-REF(CT-STEP-CNT)
                           MOVE WS-COL(2) TO WS-TEXT
                           PERFORM 8100-ISO-DATE THRU 8100-EXIT
                           MOVE WS-DATE TO CT-DATE(CT-STEP-CNT)
                           MOVE WS-COL(3)
                               TO CT-STATUS(CT-STEP-CNT)
                           COMPUTE CT-VALUE(CT-STEP-CNT) =
                               FUNCTION NUMVAL(WS-COL(5))
                           IF CT-PARTY = SPACES
                               MOVE WS-COL(4) TO CT-PARTY
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       8300-EXIT.
           EXIT.

       8400-SET-WIDTHS SECTION.
       8400-START.
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 12
               MOVE 120 TO SQLIO-COL-WIDTH(WS-J)
           END-PERFORM.
       8400-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE 8 TO WS-RC
           MOVE "XMD0008" TO WS-REASON
           MOVE "Function must be TRACE." TO WS-MSG.
       9100-EXIT.
           EXIT.

       9900-SET-RESULT SECTION.
       9900-START.
           MOVE WS-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE WS-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "CHAINTRC" TO CA-PROGRAM OF DFHCOMMAREA.
       9900-EXIT.
           EXIT.
