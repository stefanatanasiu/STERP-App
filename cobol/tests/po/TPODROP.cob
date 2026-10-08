      *>================================================================
      *> PROGRAM-ID : TPODROP
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : UNIT TESTS FOR THE TWO RECEIPT PATHS WHERE WHAT
      *>              ARRIVES DOES NOT BECOME ORDINARY OWN STOCK:
      *>              THE DROP SHIP / BACK TO BACK RECEIPT (PO-018)
      *>              AND THE SUBCONTRACT RECEIPT AGAINST A JOB
      *>              (PO-019).
      *>
      *>              THE VALIDATE TESTS HAVE NO DATABASE EFFECT AND
      *>              ALWAYS RUN.  THE CONFIRM TESTS NEED THE STERP
      *>              DATABASE AND SKIP WITHOUT IT; THEY BUILD AND
      *>              REMOVE THEIR OWN ZZ-TEST RECEIPTS.  THEY ARE
      *>              DELIBERATELY CONFIRMED AT NIL VALUE SO THAT
      *>              RECEIPT'S GRNI ACCRUAL IS NOT RAISED AND A UNIT
      *>              TEST NEVER LEAVES A JOURNAL BEHIND IT; WHAT IS
      *>              BEING MEASURED HERE IS OWNERSHIP AND STOCK, NOT
      *>              THE ACCRUAL, WHICH TPORCV ALREADY COVERS.
      *>
      *>              THE THINGS THAT MATTER ARE TESTED DELIBERATELY:
      *>              1. A DROP SHIP LINE NEVER BECOMES OUR STOCK -
      *>                 THE GOODS GO STRAIGHT TO THE CUSTOMER, SO
      *>                 NO STOCK MOVEMENT IS MADE, YET THE ARRIVAL
      *>                 IS STILL RECORDED AND THE OWNERSHIP BASIS
      *>                 IS KEPT ON THE RECEIPT.
      *>              2. A DROP SHIP LINE HAS NO PUT AWAY LOCATION,
      *>                 AND DEMANDING ONE WOULD STOP A PERFECTLY
      *>                 GOOD RECEIPT; EVERY OTHER RECEIPT STILL
      *>                 NEEDS ONE.
      *>              3. A SUBCONTRACT RECEIPT CARRIES THE JOB IT
      *>                 BELONGS TO ALL THE WAY TO THE FILE, OR THE
      *>                 WORK CANNOT BE COSTED BACK TO THE JOB.
      *>              4. THE TWO ARE KEPT APART: A DROP SHIP RECEIPT
      *>                 IS NOT A SUBCONTRACT RECEIPT.
      *>----------------------------------------------------------------
      *> 2026-10-07 STERP ORIGINAL
      *>================================================================
      *> REQ: PO-018, PO-019, PO-009
      *> AC: a drop-ship line is received but never taken into stock
      *> AC: a drop-ship line needs no put-away location
      *> AC: a subcontract receipt keeps its job reference
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TPODROP.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY RECEIPTCA REPLACING ==DFHCOMMAREA== BY ==WS-RC==.
       COPY SQLIOCA.
       01  WS-ROW                      PIC X(256).
       01  WS-DB-OK                    PIC X VALUE "N".
       01  WS-JOB-REF                  PIC X(20)
                                       VALUE "ZZ-TEST-JOB1".
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           MOVE "po RECEIPT dropship and subcontract" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 1000-DROP-NO-LOCATION THRU 1000-EXIT
           PERFORM 1100-STOCK-NEEDS-LOCATION THRU 1100-EXIT
           PERFORM 1200-DROP-STILL-VALUED THRU 1200-EXIT
           PERFORM 1300-SUBCON-VALIDATE THRU 1300-EXIT
           PERFORM 0100-CONNECT THRU 0100-EXIT
           IF WS-DB-OK = "Y"
               PERFORM 0200-CLEAN THRU 0200-EXIT
               PERFORM 2000-DROP-CONFIRM THRU 2000-EXIT
               PERFORM 2100-SUBCON-CONFIRM THRU 2100-EXIT
               PERFORM 0200-CLEAN THRU 0200-EXIT
               PERFORM 9000-DISCONNECT THRU 9000-EXIT
           ELSE
               MOVE "no database connection" TO TK-DETAIL
               MOVE "PO-018 and PO-019 confirm need the database"
                 TO TK-NAME
               PERFORM TK-SKIP
           END-IF
           PERFORM TK-END
           STOP RUN.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0100-CONNECT SECTION.
       0100-START.
           INITIALIZE SQLIO-CA
           MOVE "CONN" TO SQLIO-FUNC
           MOVE SPACES TO SQLIO-STMT
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-SQLCODE = 0
               MOVE "Y" TO WS-DB-OK
           END-IF.
       0100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0200-CLEAN SECTION.
       0200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE l FROM po.ReceiptLine l JOIN po.Receipt r "
                  "ON r.ReceiptId = l.ReceiptId WHERE r.ReceiptRef "
                  "LIKE 'ZZ-TEST-RCV%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.Receipt WHERE ReceiptRef LIKE "
                  "'ZZ-TEST-RCV%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT.
       0200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THIRTY ORDERED, TWENTY ARRIVED.  THE LINE IS A STOCKED ITEM
      *> IN EVERY CASE - WHAT CHANGES BETWEEN THE TESTS IS WHO OWNS
      *> IT WHEN IT ARRIVES, AND THAT IS THE WHOLE POINT.
      *>----------------------------------------------------------------
       0500-SETUP SECTION.
       0500-START.
           INITIALIZE WS-RC
           MOVE "VALIDATE" TO CA-FUNCTION
           MOVE "STC" TO CA-COMPANY
           MOVE "RDG" TO CA-SITE
           MOVE "WHSE" TO CA-USER
           MOVE 20270601 TO CA-BUSINESS-DATE
           MOVE "ZZ-TEST-RCV-1" TO RC-RECEIPT-REF
           MOVE "ZZ-TEST-RCV-PO1" TO RC-ORDER-NUMBER
           MOVE "S0001" TO RC-SUPPLIER
           MOVE "ZZ-TEST-RCV-DN1" TO RC-DELIVERY-NOTE
           MOVE 20270601 TO RC-PHYSICAL-DATE
           MOVE 20270601 TO RC-POSTING-DATE
           MOVE "MAIN" TO RC-WAREHOUSE
           MOVE "RECEIPT" TO RC-OWNERSHIP
           MOVE 1 TO RC-LINE-CNT
           MOVE 1 TO RC-L-NUM(1)
           MOVE 1 TO RC-L-PO-LINE(1)
           MOVE "SCREW-M4X10-SS" TO RC-L-ITEM(1)
           MOVE "Drop ship test line" TO RC-L-DESC(1)
           MOVE 30 TO RC-L-ORDERED(1)
           MOVE 20 TO RC-L-RECEIVED(1)
           MOVE "EA" TO RC-L-UOM(1)
           MOVE 1 TO RC-L-FACTOR(1)
           MOVE "NONE" TO RC-L-TRACE(1)
           MOVE "Y" TO RC-L-STOCKED(1)
           MOVE "STKA01" TO RC-L-LOCATION(1)
           MOVE 5 TO RC-L-PRICE(1).
       0500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-018.  THE GOODS WENT STRAIGHT TO THE CUSTOMER, SO THERE
      *> IS NO SHELF TO PUT THEM ON.  ASKING FOR ONE WOULD BLOCK A
      *> RECEIPT THAT IS PERFECTLY CORRECT.
      *>----------------------------------------------------------------
       1000-DROP-NO-LOCATION SECTION.
       1000-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE "DROPSHIP" TO RC-OWNERSHIP
           MOVE SPACES TO RC-L-LOCATION(1)
           CALL "RECEIPT" USING WS-RC
           MOVE "PO-018 a drop-ship line needs no put-away location"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-018 and the line is accepted, not held"
             TO TK-NAME
           MOVE "OK" TO TK-EXPECTED-X
           MOVE RC-L-OUTCOME(1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-018 the ownership basis is kept as given"
             TO TK-NAME
           MOVE "DROPSHIP" TO TK-EXPECTED-X
           MOVE RC-OWNERSHIP TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-018 a drop-ship receipt is not stock in transit"
             TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE RC-IS-TRANSIT TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE CONTRAST THAT MAKES THE ABOVE MEAN SOMETHING: AN
      *> ORDINARY RECEIPT OF THE SAME LINE STILL HAS TO SAY WHERE THE
      *> GOODS WENT, OR NOBODY WILL EVER FIND THEM AGAIN.
      *>----------------------------------------------------------------
       1100-STOCK-NEEDS-LOCATION SECTION.
       1100-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE SPACES TO RC-L-LOCATION(1)
           CALL "RECEIPT" USING WS-RC
           MOVE "PO-018 an owned receipt still needs its location"
             TO TK-NAME
           MOVE "RCP0005" TO TK-EXPECTED-X
           MOVE RC-L-REASON(1) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-018 and it is refused" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-018.  OWNERSHIP CHANGES WHERE THE GOODS GO, NOT WHAT WAS
      *> ORDERED.  THE OUTSTANDING QUANTITY AND THE VALUE OF THE
      *> DELIVERY ARE THE SAME FACTS AS ON ANY OTHER RECEIPT - THE
      *> BUYER STILL HAS TEN TO COME AND THE SUPPLIER IS STILL OWED
      *> FOR TWENTY.
      *>----------------------------------------------------------------
       1200-DROP-STILL-VALUED SECTION.
       1200-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE "DROPSHIP" TO RC-OWNERSHIP
           MOVE SPACES TO RC-L-LOCATION(1)
           CALL "RECEIPT" USING WS-RC
           MOVE "PO-018 ten of the thirty are still outstanding"
             TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE RC-L-OUTSTAND(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-018 the delivery is valued like any other"
             TO TK-NAME
           MOVE 100 TO TK-EXPECTED-N
           MOVE RC-TOTAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-018 all twenty are accepted" TO TK-NAME
           MOVE 20 TO TK-EXPECTED-N
           MOVE RC-L-ACCEPTED(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-018 nothing is quarantined on the way through"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE RC-L-QUARANT(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-019.  SUBCONTRACT WORK COMES BACK AGAINST THE JOB IT WAS
      *> SENT OUT FOR.  THE JOB REFERENCE TRAVELS WITH THE RECEIPT
      *> AND THE PROCESSING CHARGE IS NOT OWN STOCK, SO IT NEEDS NO
      *> LOCATION EITHER.
      *>----------------------------------------------------------------
       1300-SUBCON-VALIDATE SECTION.
       1300-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE WS-JOB-REF TO RC-SUBCONTRACT-JOB
           MOVE "N" TO RC-L-STOCKED(1)
           MOVE SPACES TO RC-L-LOCATION(1)
           MOVE "Heat treatment, job ZZ-TEST-JOB1" TO RC-L-DESC(1)
           CALL "RECEIPT" USING WS-RC
           MOVE "PO-019 a subcontract receipt validates" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-019 the job reference survives the call"
             TO TK-NAME
           MOVE WS-JOB-REF TO TK-EXPECTED-X
           MOVE RC-SUBCONTRACT-JOB TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-019 the subcontract charge is valued" TO TK-NAME
           MOVE 100 TO TK-EXPECTED-N
           MOVE RC-TOTAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-018.  THE REAL TEST.  CONFIRM A DROP SHIP RECEIPT AND
      *> LOOK AT WHAT IT DID: THE ARRIVAL IS ON THE FILE, THE
      *> OWNERSHIP BASIS SAYS DROPSHIP, AND NO STOCK MOVEMENT WAS
      *> MADE, BECAUSE THESE GOODS WERE NEVER OURS.
      *>----------------------------------------------------------------
       2000-DROP-CONFIRM SECTION.
       2000-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE "CONFIRM" TO CA-FUNCTION
           MOVE "ZZ-TEST-RCV-D1" TO RC-RECEIPT-REF
           MOVE "ZZ-TEST-RCV-DND1" TO RC-DELIVERY-NOTE
           MOVE "ZZTESTRCV-DROP-1" TO CA-CORRELATION-ID
           MOVE "DROPSHIP" TO RC-OWNERSHIP
           MOVE SPACES TO RC-L-LOCATION(1)
           MOVE 0 TO RC-L-PRICE(1)
           CALL "RECEIPT" USING WS-RC
           MOVE "PO-018 a drop-ship receipt confirms" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-018 and the receipt is CONFIRMED" TO TK-NAME
           MOVE "CONFIRMED" TO TK-EXPECTED-X
           MOVE RC-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-018 no stock movement is made for a drop-ship"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE RC-L-MOVEMENT(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-018 the ownership basis is on the file"
             TO TK-NAME
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT OwnershipBasis FROM po.Receipt WHERE "
                  "ReceiptRef = 'ZZ-TEST-RCV-D1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-ONE THRU 0920-EXIT
           MOVE "DROPSHIP" TO TK-EXPECTED-X
           MOVE FUNCTION TRIM(WS-ROW(1:24)) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-018 the goods are not ours in transit either"
             TO TK-NAME
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(IsStockInTransit AS varchar(2)) FROM "
                  "po.Receipt WHERE ReceiptRef = 'ZZ-TEST-RCV-D1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-ONE THRU 0920-EXIT
           MOVE "0" TO TK-EXPECTED-X
           MOVE FUNCTION TRIM(WS-ROW(1:24)) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-018 the arrival is still recorded as a line"
             TO TK-NAME
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(l.ReceivedQty AS varchar(20)) FROM "
                  "po.ReceiptLine l JOIN po.Receipt r ON r.ReceiptId "
                  "= l.ReceiptId WHERE r.ReceiptRef = "
                  "'ZZ-TEST-RCV-D1' AND l.LineNum = 1"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-ONE THRU 0920-EXIT
           MOVE "20.000000" TO TK-EXPECTED-X
           MOVE FUNCTION TRIM(WS-ROW(1:24)) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-018 a drop-ship receipt is not a subcontract one"
             TO TK-NAME
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(SubcontractJobRef,'(none)') FROM "
                  "po.Receipt WHERE ReceiptRef = 'ZZ-TEST-RCV-D1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-ONE THRU 0920-EXIT
           MOVE "(none)" TO TK-EXPECTED-X
           MOVE FUNCTION TRIM(WS-ROW(1:24)) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-019.  CONFIRM A SUBCONTRACT RECEIPT AND CHECK THAT THE
      *> JOB REFERENCE REACHED THE FILE.  WITHOUT IT THE PROCESSING
      *> CHARGE CANNOT BE COSTED BACK TO THE WORK IT PAID FOR.
      *> A SECOND CONFIRMATION IS GIVEN ITS OWN CORRELATION ID,
      *> BECAUSE TWO DIFFERENT RECEIPTS SHARING ONE WOULD HAVE THE
      *> SECOND READ AS A REPLAY OF THE FIRST.
      *>----------------------------------------------------------------
       2100-SUBCON-CONFIRM SECTION.
       2100-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE "CONFIRM" TO CA-FUNCTION
           MOVE "ZZ-TEST-RCV-S1" TO RC-RECEIPT-REF
           MOVE "ZZ-TEST-RCV-DNS1" TO RC-DELIVERY-NOTE
           MOVE "ZZTESTRCV-SUBC-1" TO CA-CORRELATION-ID
           MOVE WS-JOB-REF TO RC-SUBCONTRACT-JOB
           MOVE "N" TO RC-L-STOCKED(1)
           MOVE SPACES TO RC-L-LOCATION(1)
           MOVE "Heat treatment, job ZZ-TEST-JOB1" TO RC-L-DESC(1)
           MOVE 0 TO RC-L-PRICE(1)
           CALL "RECEIPT" USING WS-RC
           MOVE "PO-019 a subcontract receipt confirms" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-019 and the receipt is CONFIRMED" TO TK-NAME
           MOVE "CONFIRMED" TO TK-EXPECTED-X
           MOVE RC-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-019 the job reference is on the file" TO TK-NAME
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(SubcontractJobRef,'(none)') FROM "
                  "po.Receipt WHERE ReceiptRef = 'ZZ-TEST-RCV-S1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-ONE THRU 0920-EXIT
           MOVE WS-JOB-REF TO TK-EXPECTED-X
           MOVE FUNCTION TRIM(WS-ROW(1:24)) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-019 a subcontract charge is not taken into stock"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE RC-L-MOVEMENT(1) TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-019 the receipt is owned on a receipt basis"
             TO TK-NAME
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT OwnershipBasis FROM po.Receipt WHERE "
                  "ReceiptRef = 'ZZ-TEST-RCV-S1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-ONE THRU 0920-EXIT
           MOVE "RECEIPT" TO TK-EXPECTED-X
           MOVE FUNCTION TRIM(WS-ROW(1:24)) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0900-EXEC SECTION.
       0900-START.
           MOVE "EXEC" TO SQLIO-FUNC
           MOVE 8 TO SQLIO-CURSOR
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               DISPLAY "TPODROP fixture SQL failed: "
                       FUNCTION TRIM(SQLIO-ERRMSG) UPON SYSOUT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       0900-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0920-ONE SECTION.
       0920-START.
           MOVE "SNGL" TO SQLIO-FUNC
           MOVE 8 TO SQLIO-CURSOR
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE SPACES TO WS-ROW
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       0920-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-DISCONNECT SECTION.
       9000-START.
           MOVE "DISC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       9000-EXIT.
           EXIT.
       COPY TESTKITP.
       END PROGRAM TPODROP.
