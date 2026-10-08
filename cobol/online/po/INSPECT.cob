      *>================================================================
      *> PROGRAM-ID : INSPECT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : RECORD THE RESULT OF GOODS INWARD INSPECTION AND
      *>              DISPOSITION THE REJECTED QUANTITY.  GOODS AWAIT
      *>              INSPECTION IN QUARANTINE, WHICH IS A QUALITY
      *>              CONDITION OF REAL STOCK AND NOT A SEPARATE
      *>              LEDGER - THE GOODS WERE ON THE BALANCE SHEET
      *>              FROM THE MOMENT THEY WERE RECEIVED.
      *> CALLED BY  : INSPECTION SCREEN, QUALITY WORKBENCH
      *> INTERFACE  : CALL "INSPECT" USING DFHCOMMAREA  (INSPECTCA)
      *> CALLS      : STKMOVE, POPOST, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-07-30 RJH  CR0824 GOODS INWARD INSPECTION
      *> 2004-02-11 DJM  CR1142 CONCESSION NEEDS A NAMED AUTHORITY
      *> 2026-10-06 STERP PO-010 THROUGH STKMOVE AND POPOST
      *>================================================================
      *> REQ: PO-010, PO-023
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSPECT.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 30/07/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY POACCNTS.
       COPY POPOSTWK.
       COPY STKMOVECA REPLACING ==DFHCOMMAREA== BY ==WS-STKMOVE-CA==
                      LEADING ==CA-== BY ==SK-==.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(60).
       01  WS-WORK.
           05  WS-SUM-QTY              PIC S9(13)V9(6) VALUE 0.
           05  WS-QTY-ED               PIC -(9)9.999999.
           05  WS-AMT-ED               PIC -(11)9.99.
           05  WS-CORR                 PIC X(64).
           05  WS-LINE-ED              PIC -(8)9.
       LINKAGE SECTION.
       COPY INSPECTCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "RECORD"
                   PERFORM 2000-VALIDATE THRU 2000-EXIT
                   IF NOT CA-FAILED
                       PERFORM 3000-DISPOSE THRU 3000-EXIT
                   END-IF
               WHEN "ENQUIRE"
                   PERFORM 6000-ENQUIRE THRU 6000-EXIT
               WHEN OTHER
                   MOVE "INS0006" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                          " is not supported by INSPECT"
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
           MOVE "INSPECT" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE 0 TO IN-REMAINING-QTY IN-TO-GOOD-QTY
           MOVE 0 TO IN-TO-SCRAP-QTY IN-HELD-FOR-RET-QTY
           MOVE 0 TO IN-SCRAP-VALUE-GBP
           MOVE 0 TO IN-MOVEMENT-ID IN-MOVEMENT-ID-2
           MOVE SPACES TO IN-JOURNAL-NUMBER
           MOVE "ACCEPTED" TO IN-QUALITY-STATUS
           MOVE "OK" TO IN-OUTCOME
           IF IN-INSPECT-DATE = 0
               MOVE CA-BUSINESS-DATE TO IN-INSPECT-DATE
           END-IF
           IF IN-INSPECTOR = SPACES
               MOVE CA-USER TO IN-INSPECTOR
           END-IF
           IF IN-DISPOSITION = SPACES
               MOVE "HOLD" TO IN-DISPOSITION
           END-IF
           MOVE SPACES TO WS-CORR
           MOVE IN-RECEIPT-LINE TO WS-LINE-ED
           STRING "INS-" FUNCTION TRIM(CA-COMPANY) "-"
                  FUNCTION TRIM(IN-RECEIPT-REF) "-"
                  FUNCTION TRIM(WS-LINE-ED)
               DELIMITED BY SIZE INTO WS-CORR
           IF CA-CORRELATION-ID NOT = SPACES
               MOVE CA-CORRELATION-ID TO WS-CORR
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE INSPECTED QUANTITY MUST ADD BACK.  ACCEPTED PLUS REJECTED
      *> PLUS REWORK EQUALS INSPECTED, AND INSPECTED NEVER EXCEEDS
      *> WHAT IS ACTUALLY IN QUARANTINE - OTHERWISE THE RESULT WOULD
      *> SIGN OFF STOCK THAT WAS NEVER THERE.
      *>----------------------------------------------------------------
       2000-VALIDATE SECTION.
       2000-START.
           IF IN-QUARANTINE-QTY <= 0
               PERFORM 1500-READ-LINE THRU 1500-EXIT
           END-IF
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           IF IN-INSPECTED-QTY <= 0
               COMPUTE IN-INSPECTED-QTY = IN-ACCEPTED-QTY
                       + IN-REJECTED-QTY + IN-REWORK-QTY
           END-IF
           COMPUTE WS-SUM-QTY = IN-ACCEPTED-QTY + IN-REJECTED-QTY
                   + IN-REWORK-QTY
           IF WS-SUM-QTY NOT = IN-INSPECTED-QTY
               MOVE "INS0002" TO CA-REASON-CODE
               MOVE IN-INSPECTED-QTY TO WS-QTY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Accepted, rejected and reworked quantities "
                      "must add back to the inspected quantity of "
                      FUNCTION TRIM(WS-QTY-ED)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF IN-INSPECTED-QTY > IN-QUARANTINE-QTY
               MOVE "INS0003" TO CA-REASON-CODE
               MOVE IN-QUARANTINE-QTY TO WS-QTY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Only " FUNCTION TRIM(WS-QTY-ED)
                      " is held in quarantine on this receipt line; "
                      "an inspection cannot sign off stock that is "
                      "not there"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF IN-REJECTED-QTY > 0 AND IN-REJECT-REASON = SPACES
               MOVE "INS0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A rejection must state its reason; the "
                      "supplier performance measure and any claim "
                      "both depend on it"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF IN-DISP-CONCESSION AND IN-CONCESSION-BY = SPACES
               MOVE "INS0005" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Accepting goods outside specification is a "
                      "named decision; record who authorised the "
                      "concession and its reference"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF IN-CERT-REQUIRED = "Y" AND IN-CERT-REF = SPACES
               MOVE "INS0009" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "This item needs a certificate of conformity; "
                      "record its reference before the goods leave "
                      "quarantine"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1500-READ-LINE SECTION.
       1500-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(l.QuarantineQty AS varchar(30)) "
                  "FROM po.ReceiptLine l JOIN po.Receipt r ON "
                  "r.ReceiptId = l.ReceiptId WHERE "
                  "r.CompanyCode = ? AND r.ReceiptRef = ? AND "
                  "l.LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE IN-RECEIPT-REF TO SQLIO-PARM-VAL(2)
           MOVE IN-RECEIPT-LINE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 60 TO SQLIO-COL-WIDTH(1)
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-SQLCODE NOT = 0 OR WS-C-VALUE = SPACES
               MOVE 16 TO CA-RETURN-CODE
               MOVE "INS0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Receipt line not found, or it is not "
                      "awaiting inspection"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 1500-EXIT
           END-IF
           MOVE FUNCTION NUMVAL(WS-C-VALUE) TO IN-QUARANTINE-QTY.
       1500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> DISPOSITION.  ACCEPTED AND REWORKED QUANTITIES ARE RELEASED
      *> FROM QUARANTINE TO GOOD; SCRAPPED GOODS LEAVE STOCK AGAINST
      *> THE MATERIAL USAGE VARIANCE; GOODS TO GO BACK TO THE SUPPLIER
      *> STAY IN QUARANTINE SO THAT SUPPRET MOVES AND VALUES THEM
      *> ONCE, NEVER TWICE.
      *>----------------------------------------------------------------
       3000-DISPOSE SECTION.
       3000-START.
           COMPUTE IN-TO-GOOD-QTY = IN-ACCEPTED-QTY + IN-REWORK-QTY
           IF IN-DISP-CONCESSION
               ADD IN-REJECTED-QTY TO IN-TO-GOOD-QTY
           END-IF
           IF IN-DISP-SCRAP
               MOVE IN-REJECTED-QTY TO IN-TO-SCRAP-QTY
           END-IF
           IF IN-DISP-RETURN OR IN-DISP-HOLD
               MOVE IN-REJECTED-QTY TO IN-HELD-FOR-RET-QTY
           END-IF
           COMPUTE IN-REMAINING-QTY = IN-QUARANTINE-QTY
                   - IN-TO-GOOD-QTY - IN-TO-SCRAP-QTY
           IF IN-TO-GOOD-QTY > 0
               PERFORM 3200-RELEASE THRU 3200-EXIT
           END-IF
           IF IN-TO-SCRAP-QTY > 0 AND NOT CA-FAILED
               PERFORM 3400-SCRAP THRU 3400-EXIT
           END-IF
           IF NOT CA-FAILED
               PERFORM 3600-UPDATE-LINE THRU 3600-EXIT
           END-IF
           IF IN-REJECTED-QTY > 0
               MOVE 4 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               MOVE IN-REJECTED-QTY TO WS-QTY-ED
               STRING FUNCTION TRIM(WS-QTY-ED)
                      " rejected with disposition "
                      FUNCTION TRIM(IN-DISPOSITION)
                      "; the supplier quality measure has been "
                      "updated"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           IF IN-REMAINING-QTY > 0
               MOVE "QUARANTINE" TO IN-QUALITY-STATUS
           ELSE
               IF IN-TO-SCRAP-QTY > 0 AND IN-TO-GOOD-QTY = 0
                   MOVE "REJECTED" TO IN-QUALITY-STATUS
               END-IF
           END-IF.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3200-RELEASE SECTION.
       3200-START.
           INITIALIZE WS-STKMOVE-CA
           MOVE "RELEASE" TO SK-FUNCTION
           MOVE CA-COMPANY TO SK-COMPANY
           MOVE CA-SITE TO SK-SITE
           MOVE CA-USER TO SK-USER
           MOVE CA-BUSINESS-DATE TO SK-BUSINESS-DATE
           MOVE WS-CORR TO SK-CORRELATION-ID
           MOVE IN-ITEM-CODE TO SM-ITEM-CODE
           MOVE CA-SITE TO SM-SITE
           MOVE IN-WAREHOUSE TO SM-WAREHOUSE
           MOVE IN-LOCATION TO SM-LOCATION
           IF IN-GOOD-LOCATION NOT = SPACES
               MOVE IN-GOOD-LOCATION TO SM-TO-LOCATION
           END-IF
           MOVE IN-LOT-CODE TO SM-LOT-CODE
           MOVE IN-SERIAL-NO TO SM-SERIAL-NO
           MOVE "QUARANTINE" TO SM-CONDITION
           MOVE "GOOD" TO SM-TO-CONDITION
           MOVE IN-TO-GOOD-QTY TO SM-QUANTITY
           MOVE IN-UOM-CODE TO SM-UOM-CODE
           MOVE IN-UNIT-COST TO SM-UNIT-COST
           MOVE IN-INSPECT-DATE TO SM-MOVEMENT-DATE
           MOVE "QUALITY" TO SM-HOLD-TYPE
           MOVE "ACCEPTED" TO SM-HOLD-OUTCOME
           MOVE "INSPACCEPT" TO SM-REASON-CODE
           MOVE SPACES TO SM-REASON
           IF IN-DISP-CONCESSION
               STRING "Accepted on concession "
                      FUNCTION TRIM(IN-CONCESSION-REF)
                      " authorised by "
                      FUNCTION TRIM(IN-CONCESSION-BY)
                   DELIMITED BY SIZE INTO SM-REASON
               MOVE IN-CONCESSION-REF TO SM-APPROVAL-REF
           ELSE
               STRING "Inspection passed on receipt "
                      FUNCTION TRIM(IN-RECEIPT-REF)
                   DELIMITED BY SIZE INTO SM-REASON
           END-IF
           MOVE "PO" TO SM-SOURCE-MODULE
           MOVE "INSPECTION" TO SM-SOURCE-DOC-TYPE
           MOVE IN-RECEIPT-REF TO SM-SOURCE-DOC-REF
           MOVE IN-RECEIPT-LINE TO SM-SOURCE-DOC-LINE
           CALL "STKMOVE" USING WS-STKMOVE-CA
           MOVE SM-MOVEMENT-ID TO IN-MOVEMENT-ID
           IF SK-RETURN-CODE > 4
               MOVE SK-RETURN-CODE TO CA-RETURN-CODE
               MOVE "INS0007" TO CA-REASON-CODE
               MOVE SK-MESSAGE TO CA-MESSAGE
           END-IF.
       3200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> SCRAP ON SITE.  THE STOCK LEAVES AND THE COST IS RECOGNISED
      *> NOW.  WHETHER THE SUPPLIER PAYS FOR IT IS A SEPARATE
      *> COMMERCIAL MATTER AND IS NEVER ASSUMED HERE - AN EXPECTED
      *> CREDIT THAT NEVER ARRIVES WOULD OTHERWISE SIT IN STOCK.
      *>----------------------------------------------------------------
       3400-SCRAP SECTION.
       3400-START.
           INITIALIZE WS-STKMOVE-CA
           MOVE "SCRAP" TO SK-FUNCTION
           MOVE CA-COMPANY TO SK-COMPANY
           MOVE CA-SITE TO SK-SITE
           MOVE CA-USER TO SK-USER
           MOVE CA-BUSINESS-DATE TO SK-BUSINESS-DATE
           MOVE SPACES TO SK-CORRELATION-ID
           STRING FUNCTION TRIM(WS-CORR) "-S"
               DELIMITED BY SIZE INTO SK-CORRELATION-ID
           MOVE IN-ITEM-CODE TO SM-ITEM-CODE
           MOVE CA-SITE TO SM-SITE
           MOVE IN-WAREHOUSE TO SM-WAREHOUSE
           MOVE IN-LOCATION TO SM-LOCATION
           MOVE IN-LOT-CODE TO SM-LOT-CODE
           MOVE IN-SERIAL-NO TO SM-SERIAL-NO
           MOVE "QUARANTINE" TO SM-CONDITION
           MOVE IN-TO-SCRAP-QTY TO SM-QUANTITY
           MOVE IN-UOM-CODE TO SM-UOM-CODE
           MOVE IN-UNIT-COST TO SM-UNIT-COST
           MOVE IN-INSPECT-DATE TO SM-MOVEMENT-DATE
           MOVE IN-REJECT-REASON TO SM-REASON-CODE
           MOVE IN-REJECT-NARR TO SM-REASON
           MOVE "PO" TO SM-SOURCE-MODULE
           MOVE "INSPECTION" TO SM-SOURCE-DOC-TYPE
           MOVE IN-RECEIPT-REF TO SM-SOURCE-DOC-REF
           MOVE IN-RECEIPT-LINE TO SM-SOURCE-DOC-LINE
           CALL "STKMOVE" USING WS-STKMOVE-CA
           MOVE SM-MOVEMENT-ID TO IN-MOVEMENT-ID-2
           IF SK-RETURN-CODE > 4
               MOVE SK-RETURN-CODE TO CA-RETURN-CODE
               MOVE "INS0007" TO CA-REASON-CODE
               MOVE SK-MESSAGE TO CA-MESSAGE
               GO TO 3400-EXIT
           END-IF
           COMPUTE IN-SCRAP-VALUE-GBP ROUNDED =
                   IN-TO-SCRAP-QTY * IN-UNIT-COST
           IF IN-SCRAP-VALUE-GBP = 0
               GO TO 3400-EXIT
           END-IF
           INITIALIZE WS-POPOST
           MOVE "POST" TO PP-FUNC
           MOVE CA-COMPANY TO PP-COMPANY
           MOVE CA-SITE TO PP-SITE
           MOVE CA-USER TO PP-USER
           MOVE CA-BUSINESS-DATE TO PP-BUSINESS-DATE
           MOVE SPACES TO PP-CORRELATION
           STRING FUNCTION TRIM(WS-CORR) "-SCRAP"
               DELIMITED BY SIZE INTO PP-CORRELATION
           MOVE IN-INSPECT-DATE TO PP-POSTING-DATE
           MOVE IN-INSPECT-DATE TO PP-DOCUMENT-DATE
           MOVE "GBP" TO PP-CURRENCY
           MOVE 1 TO PP-FX-RATE
           MOVE "PO.INSPSCRAP" TO PP-EVENT-CODE
           MOVE IN-RECEIPT-REF TO PP-SOURCE-REF
           MOVE "PO" TO PP-SUBLEDGER-TYPE
           MOVE IN-SUPPLIER TO PP-SUBLEDGER-REF
           MOVE IN-CERT-REF TO PP-EVIDENCE-REF
           MOVE SPACES TO PP-DESCRIPTION
           STRING "Inspection scrap on receipt "
                  FUNCTION TRIM(IN-RECEIPT-REF) " - "
                  FUNCTION TRIM(IN-REJECT-REASON)
               DELIMITED BY SIZE INTO PP-DESCRIPTION
           MOVE PO-ACC-SCRAP TO PP-L-ACCOUNT(1)
           MOVE IN-SCRAP-VALUE-GBP TO PP-L-DR(1)
           MOVE 0 TO PP-L-CR(1)
           MOVE "PURCH" TO PP-L-COST-CTR(1)
           MOVE "Goods scrapped at inspection"
             TO PP-L-NARRATIVE(1)
           MOVE PO-ACC-STOCK TO PP-L-ACCOUNT(2)
           MOVE 0 TO PP-L-DR(2)
           MOVE IN-SCRAP-VALUE-GBP TO PP-L-CR(2)
           MOVE "Stock removed at inspection"
             TO PP-L-NARRATIVE(2)
           MOVE 2 TO PP-LINE-CNT
           CALL "POPOST" USING WS-POPOST
           MOVE PP-JOURNAL-NUMBER TO IN-JOURNAL-NUMBER
           IF PP-RC > 4
               MOVE PP-RC TO CA-RETURN-CODE
               MOVE "INS0008" TO CA-REASON-CODE
               MOVE PP-MESSAGE TO CA-MESSAGE
           END-IF.
       3400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3600-UPDATE-LINE SECTION.
       3600-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE l SET l.AcceptedQty = l.AcceptedQty + ?, "
                  "l.RejectedQty = l.RejectedQty + ?, "
                  "l.QuarantineQty = ?, l.QualityStatus = ?, "
                  "l.InspectionRequired = CASE WHEN ? > 0 THEN 1 "
                  "ELSE 0 END FROM po.ReceiptLine l JOIN "
                  "po.Receipt r ON r.ReceiptId = l.ReceiptId WHERE "
                  "r.CompanyCode = ? AND r.ReceiptRef = ? AND "
                  "l.LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE IN-TO-GOOD-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE IN-REJECTED-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE IN-REMAINING-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE IN-QUALITY-STATUS TO SQLIO-PARM-VAL(4)
           MOVE IN-REMAINING-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(6)
           MOVE IN-RECEIPT-REF TO SQLIO-PARM-VAL(7)
           MOVE IN-RECEIPT-LINE TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE 8 TO SQLIO-PARM-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       3600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       6000-ENQUIRE SECTION.
       6000-START.
           PERFORM 1500-READ-LINE THRU 1500-EXIT
           IF WS-FATAL
               GO TO 6000-EXIT
           END-IF
           MOVE IN-QUARANTINE-QTY TO IN-REMAINING-QTY
           IF IN-REMAINING-QTY > 0
               MOVE "QUARANTINE" TO IN-QUALITY-STATUS
               MOVE 4 TO CA-RETURN-CODE
               MOVE IN-REMAINING-QTY TO WS-QTY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(WS-QTY-ED)
                      " is still held in quarantine and is not "
                      "available to pick, issue or sell"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       6000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE 8 TO CA-RETURN-CODE
           MOVE "Y" TO WS-FATAL-SW
           MOVE "REJECTED" TO IN-OUTCOME.
       9000-EXIT.
           EXIT.
       END PROGRAM INSPECT.
