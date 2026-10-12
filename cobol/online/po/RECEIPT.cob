      *>================================================================
      *> PROGRAM-ID : RECEIPT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : CONFIRM A GOODS RECEIPT.  RECORDS WHAT ARRIVED,
      *>              WHEN IT ARRIVED AND WHO OWNS IT, MOVES THE STOCK
      *>              THROUGH THE ITEM SUITE AND ACCRUES GOODS RECEIVED
      *>              NOT INVOICED.  OWNERSHIP AND NOT THE GATE DECIDES
      *>              THE ACCOUNTING DATE: AN EX WORKS SHIPMENT IS OURS
      *>              WHILE IT IS STILL ON THE WATER AND IS HELD AS
      *>              STOCK IN TRANSIT UNTIL IT LANDS.
      *> CALLED BY  : GOODS INWARD SCREEN, REQGEN DEMO LOADER
      *> INTERFACE  : CALL "RECEIPT" USING DFHCOMMAREA  (RECEIPTCA)
      *> CALLS      : TOLCHK, STKMOVE, POPOST, NUMGEN, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-05-12 RJH  CR0801 GOODS INWARD
      *> 2000-02-19 KMP  CR0902 LOT AND SERIAL AT GOODS INWARD
      *> 2021-01-01 TMW  BREXIT - STOCK IN TRANSIT FOR IMPORTS
      *> 2026-10-06 STERP PO-009/014/017 VIA STKMOVE AND POPOST
      *>================================================================
      *> REQ: PO-009, PO-011, PO-014, PO-017, PO-018, PO-019
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECEIPT.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 12/05/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY POACCNTS.
       COPY POPOSTWK.
       COPY TOLCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-TOLCHK-CA==
                      LEADING ==CA-== BY ==TK-==.
       COPY STKMOVECA REPLACING ==DFHCOMMAREA== BY ==WS-STKMOVE-CA==
                      LEADING ==CA-== BY ==SK-==.
       COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==WS-NUM-CA==
                     LEADING ==CA-== BY ==NM-==.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
           05  WS-DB-SW                PIC X VALUE "N".
               88  WS-DB-READY                   VALUE "Y".
           05  WS-HELD-SW              PIC X VALUE "N".
               88  WS-HELD                       VALUE "Y".
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(60).
       01  WS-TWO-COL.
           05  WS-C2-ONE               PIC X(30).
           05  WS-C2-TWO               PIC X(30).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-N                    PIC S9(3) VALUE 0.
           05  WS-OK-LINES             PIC S9(4) VALUE 0.
           05  WS-STOCK-GBP            PIC S9(13)V99 VALUE 0.
           05  WS-LINE-GBP             PIC S9(13)V99 VALUE 0.
           05  WS-PPV-GBP              PIC S9(13)V99 VALUE 0.
           05  WS-STD-GBP              PIC S9(13)V99 VALUE 0.
           05  WS-QTY-ED               PIC -(9)9.999999.
           05  WS-AMT-ED               PIC -(11)9.99.
           05  WS-PRC-ED               PIC -(9)9.999999.
           05  WS-NUM-ED               PIC -(8)9.
           05  WS-DATE-ED              PIC 9(8).
           05  WS-CORR                 PIC X(64).
           05  WS-CORR-SHORT           PIC X(30).
           05  WS-POST-CORR            PIC X(64).
           05  WS-OWN-CORR-SW          PIC X VALUE "N".
               88  WS-OWN-CORR                   VALUE "Y".
           05  WS-MOVE-DATE            PIC 9(8) VALUE 0.
           05  WS-OWN-DATE             PIC 9(8) VALUE 0.
           05  WS-LINE-NO              PIC S9(4) VALUE 0.
           05  WS-LINE-SQLCODE         PIC S9(9) COMP-5 VALUE 0.
           05  WS-LINE-ERRMSG          PIC X(256) VALUE SPACES.
           05  WS-UOW-ACTIVE           PIC X VALUE "N".
           05  WS-UOW-OWN              PIC X VALUE "N".
       LINKAGE SECTION.
       COPY RECEIPTCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           IF CA-FUNCTION = "CONFIRM" OR "TRANSIT" OR "REVERSE"
               PERFORM 8300-BEGIN-UOW THRU 8300-EXIT
               IF CA-FAILED
                   GO TO 0000-RETURN
               END-IF
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "VALIDATE"
                   PERFORM 2000-VALIDATE THRU 2000-EXIT
               WHEN "CONFIRM"
                   PERFORM 2000-VALIDATE THRU 2000-EXIT
                   IF NOT CA-FAILED
                       PERFORM 3000-CONFIRM THRU 3000-EXIT
                   END-IF
               WHEN "TRANSIT"
                   PERFORM 4000-TRANSIT THRU 4000-EXIT
               WHEN "REVERSE"
                   PERFORM 5000-REVERSE THRU 5000-EXIT
               WHEN OTHER
                   MOVE "RCP0008" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                          " is not supported by RECEIPT"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE 8 TO CA-RETURN-CODE
           END-EVALUATE
           IF WS-UOW-ACTIVE = "Y"
               PERFORM 8400-END-UOW THRU 8400-EXIT
           END-IF.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> INITIALISE AND VALIDATE THE HEADER.  THE POSTING DATE DEFAULTS
      *> TO THE PHYSICAL DATE; FOR AN INCOTERMS RECEIPT THE OWNERSHIP
      *> DATE IS THE SHIPMENT DATE EVIDENCED BY THE BILL OF LADING, SO
      *> THE CALLER SUPPLIES IT AS THE POSTING DATE AND THE SHIPMENT
      *> EVIDENCE IS MANDATORY (PO-014, D-03).
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE "N" TO WS-FATAL-SW
           MOVE "N" TO WS-DB-SW
           MOVE "N" TO WS-HELD-SW
           MOVE "N" TO WS-UOW-ACTIVE WS-UOW-OWN
           MOVE "RECEIPT" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE 0 TO RC-RECEIPT-ID RC-TOTAL-GBP RC-GRNI-GBP
           MOVE 0 TO RC-PPV-GBP RC-LINES-ACCEPTED RC-LINES-HELD
           MOVE 0 TO RC-LINES-QUARANT
           MOVE SPACES TO RC-JOURNAL-NUMBER
           MOVE "N" TO RC-WAS-DUPLICATE
           MOVE "N" TO RC-IS-TRANSIT
           MOVE "DRAFT" TO RC-STATUS
           IF RC-OWNERSHIP = SPACES
               MOVE "RECEIPT" TO RC-OWNERSHIP
           END-IF
           IF RC-DUP-POLICY = SPACES
               MOVE "BLOCK" TO RC-DUP-POLICY
           END-IF
           IF RC-CURRENCY = SPACES
               MOVE "GBP" TO RC-CURRENCY
           END-IF
           IF RC-FX-RATE = 0
               MOVE 1 TO RC-FX-RATE
           END-IF
           IF RC-PHYSICAL-DATE = 0
               MOVE CA-BUSINESS-DATE TO RC-PHYSICAL-DATE
           END-IF
           IF RC-POSTING-DATE = 0
               MOVE RC-PHYSICAL-DATE TO RC-POSTING-DATE
           END-IF
           MOVE RC-PHYSICAL-DATE TO WS-MOVE-DATE
           MOVE RC-POSTING-DATE TO WS-OWN-DATE
           IF RC-RECEIVED-BY = SPACES
               MOVE CA-USER TO RC-RECEIVED-BY
           END-IF
           IF CA-CORRELATION-ID = SPACES
               MOVE "Y" TO WS-OWN-CORR-SW
           ELSE
               MOVE "N" TO WS-OWN-CORR-SW
           END-IF
           PERFORM 1050-CORRELATION THRU 1050-EXIT
           IF RC-PHYSICAL-DATE > CA-BUSINESS-DATE
               MOVE "RCP0013" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The goods cannot be received before they "
                      "arrive; the physical date is in the future"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1000-EXIT
           END-IF
           IF RC-OWN-INCOTERMS AND RC-SHIP-EVIDENCE = SPACES
               MOVE "RCP0009" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Ownership passed at the supplier's door, so "
                      "the shipment evidence (bill of lading or air "
                      "waybill) must be recorded before the goods "
                      "can be taken into stock in transit"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1000-EXIT
           END-IF
           IF RC-OWN-INCOTERMS
               MOVE "Y" TO RC-IS-TRANSIT
           END-IF
           IF CA-FUNCTION = "CONFIRM" OR CA-FUNCTION = "REVERSE"
              OR CA-FUNCTION = "TRANSIT"
               MOVE "Y" TO WS-DB-SW
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A SUPPLIER DELIVERY NOTE IS THE ONE REFERENCE THAT TELLS US
      *> TWO LORRY LOADS APART.  THE SAME NOTE RECEIVED TWICE IS
      *> ALMOST ALWAYS A SECOND KEYING OF THE SAME DELIVERY, AND AN
      *> UNDETECTED REPEAT ACCRUES GRNI TWICE AND OVERSTATES STOCK.
      *>----------------------------------------------------------------
       1200-DUPLICATE SECTION.
       1200-START.
           IF RC-DELIVERY-NOTE = SPACES
               GO TO 1200-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 ReceiptRef FROM po.Receipt "
                  "WHERE CompanyCode = ? AND SupplierCode = ? "
                  "AND DeliveryNoteRef = ? AND Status <> 'REVERSED' "
                  "AND ReceiptRef <> ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE RC-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE RC-DELIVERY-NOTE TO SQLIO-PARM-VAL(3)
           MOVE RC-RECEIPT-REF TO SQLIO-PARM-VAL(4)
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 60 TO SQLIO-COL-WIDTH(1)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-SQLCODE = 0 AND WS-C-VALUE NOT = SPACES
               MOVE "Y" TO RC-WAS-DUPLICATE
               MOVE "RCP0007" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Delivery note "
                      FUNCTION TRIM(RC-DELIVERY-NOTE)
                      " was already received from this supplier on "
                      FUNCTION TRIM(WS-C-VALUE)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               IF RC-DUP-BLOCK
                   PERFORM 9000-REJECT THRU 9000-EXIT
               ELSE
                   MOVE 4 TO CA-RETURN-CODE
               END-IF
           END-IF.
       1200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE CORRELATION IDS.  THE RECEIPT'S OWN CORRELATION IS THE ONE
      *> THE CALLER SEES; THE STOCK MOVEMENTS AND THE NOMINAL POSTING
      *> EACH GET A DERIVED ID OF THEIR OWN.  THEY MUST NEVER COLLIDE:
      *> inv.usp_PostStockMovement AND fin.usp_PostJournal SHARE
      *> core.IdempotencyKey, SO A MOVEMENT AND A JOURNAL FILED UNDER
      *> THE SAME ID MAKE THE SECOND ONE LOOK LIKE A REPLAY OF THE
      *> FIRST AND NO JOURNAL IS EVER WRITTEN.  THE COMMAREA FIELD IS
      *> ONLY 36 CHARACTERS, SO THE LINE SUFFIX IS APPENDED TO A
      *> SHORTENED STEM - A 36 CHARACTER CALLER ID WOULD OTHERWISE
      *> TRUNCATE THE SUFFIX AWAY AND PUT EVERY LINE ON ONE KEY.
      *>----------------------------------------------------------------
       1050-CORRELATION SECTION.
       1050-START.
           MOVE SPACES TO WS-CORR
           IF WS-OWN-CORR
               STRING "RCP-" FUNCTION TRIM(CA-COMPANY) "-"
                      FUNCTION TRIM(RC-RECEIPT-REF)
                   DELIMITED BY SIZE INTO WS-CORR
               MOVE WS-CORR TO CA-CORRELATION-ID
           ELSE
               MOVE CA-CORRELATION-ID TO WS-CORR
           END-IF
           MOVE WS-CORR(1:30) TO WS-CORR-SHORT
           MOVE SPACES TO WS-POST-CORR
           STRING FUNCTION TRIM(WS-CORR-SHORT) "-GRNI"
               DELIMITED BY SIZE INTO WS-POST-CORR.
       1050-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE GOODS RECEIVED NOTE NUMBER.  A CALLER MAY PRESENT ITS OWN
      *> REFERENCE (THE WAREHOUSE SOMETIMES PRE-PRINTS THEM); IF IT
      *> DOES NOT, ONE IS DRAWN HERE SO THAT NOTHING IS EVER WRITTEN
      *> AGAINST A BLANK GRN.  REQ: PO-010
      *>----------------------------------------------------------------
       1100-NUMBER SECTION.
       1100-START.
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO NM-FUNCTION
           MOVE CA-COMPANY TO NM-COMPANY
           MOVE CA-SITE TO NM-SITE
           MOVE CA-USER TO NM-USER
           MOVE RC-PHYSICAL-DATE TO NM-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO NM-CORRELATION-ID
           MOVE "PORCP" TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF NM-RETURN-CODE > 4
               MOVE "RCP0012" TO CA-REASON-CODE
               MOVE NM-MESSAGE TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1100-EXIT
           END-IF
           MOVE NG-DOCUMENT-NUMBER TO RC-RECEIPT-REF
           IF WS-OWN-CORR
               PERFORM 1050-CORRELATION THRU 1050-EXIT
           END-IF.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> LINE VALIDATION.  EVERY LINE STATES ITS QUANTITY, ITS
      *> TRACEABILITY AND ITS DESTINATION BEFORE ANYTHING MOVES.
      *> OUTSTANDING AND OVER DELIVERY COME FROM TOLCHK SO THAT THE
      *> SCREEN, THE RECEIPT AND THE COMMITMENT ALL AGREE.
      *>----------------------------------------------------------------
       2000-VALIDATE SECTION.
       2000-START.
           IF RC-LINE-CNT <= 0
               MOVE "RCP0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A receipt must have at least one line"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF RC-LINE-CNT > 20
               MOVE 20 TO RC-LINE-CNT
           END-IF
           IF WS-DB-READY AND RC-RECEIPT-REF = SPACES
               PERFORM 1100-NUMBER THRU 1100-EXIT
               IF CA-FAILED
                   GO TO 2000-EXIT
               END-IF
           END-IF
           IF WS-DB-READY
               PERFORM 1200-DUPLICATE THRU 1200-EXIT
               IF CA-FAILED
                   GO TO 2000-EXIT
               END-IF
           END-IF
           MOVE 0 TO WS-OK-LINES
           MOVE 0 TO RC-TOTAL-GBP RC-PPV-GBP
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > RC-LINE-CNT
               PERFORM 2100-ONE-LINE THRU 2100-EXIT
           END-PERFORM
           MOVE WS-OK-LINES TO RC-LINES-ACCEPTED
           IF CA-RETURN-CODE >= 8
               GO TO 2000-EXIT
           END-IF
           IF WS-HELD
               MOVE 6 TO CA-RETURN-CODE
               MOVE "RCP0006" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "One or more lines are above the approved "
                      "over delivery tolerance; the order must be "
                      "amended and approved before the delivery can "
                      "be confirmed"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           IF WS-OK-LINES = 0 AND NOT WS-HELD
               MOVE "RCP0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No line carries a receivable quantity"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2100-ONE-LINE SECTION.
       2100-START.
           MOVE 0 TO RC-L-ACCEPTED(WS-I) RC-L-QUARANT(WS-I)
           MOVE 0 TO RC-L-OVER-QTY(WS-I) RC-L-OUTSTAND(WS-I)
           MOVE 0 TO RC-L-VALUE-GBP(WS-I) RC-L-PPV-GBP(WS-I)
           MOVE 0 TO RC-L-MOVEMENT(WS-I)
           MOVE "OK" TO RC-L-OUTCOME(WS-I)
           MOVE SPACES TO RC-L-REASON(WS-I)
           IF RC-L-FACTOR(WS-I) = 0
               MOVE 1 TO RC-L-FACTOR(WS-I)
           END-IF
           IF RC-L-TRACE(WS-I) = SPACES
               MOVE "NONE" TO RC-L-TRACE(WS-I)
           END-IF
           IF RC-L-STOCKED(WS-I) = SPACES
               MOVE "Y" TO RC-L-STOCKED(WS-I)
           END-IF
           IF RC-L-RECEIVED(WS-I) <= 0
               MOVE "REJECT" TO RC-L-OUTCOME(WS-I)
               MOVE "RCP0003" TO RC-L-REASON(WS-I)
               MOVE "RCP0003" TO CA-REASON-CODE
               MOVE 8 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(RC-L-ITEM(WS-I))
                      " must carry a positive receivable quantity"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 2100-EXIT
           END-IF
           COMPUTE RC-L-STOCK-QTY(WS-I) ROUNDED =
                   RC-L-RECEIVED(WS-I) * RC-L-FACTOR(WS-I)
           IF RC-L-TRACE-LOT(WS-I) AND RC-L-LOT(WS-I) = SPACES
               MOVE "REJECT" TO RC-L-OUTCOME(WS-I)
               MOVE "RCP0004" TO RC-L-REASON(WS-I)
               MOVE "RCP0004" TO CA-REASON-CODE
               MOVE 8 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(RC-L-ITEM(WS-I))
                      " is lot traceable; a lot number must be "
                      "recorded at goods inward because it cannot "
                      "be reconstructed afterwards"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 2100-EXIT
           END-IF
           IF RC-L-TRACE-SERIAL(WS-I) AND RC-L-SERIAL(WS-I) = SPACES
               MOVE "REJECT" TO RC-L-OUTCOME(WS-I)
               MOVE "RCP0004" TO RC-L-REASON(WS-I)
               MOVE "RCP0004" TO CA-REASON-CODE
               MOVE 8 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(RC-L-ITEM(WS-I))
                      " is serial traceable; a serial number must "
                      "be recorded at goods inward"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 2100-EXIT
           END-IF
           IF RC-L-TRACE-SERIAL(WS-I) AND RC-L-STOCK-QTY(WS-I) NOT = 1
               MOVE "REJECT" TO RC-L-OUTCOME(WS-I)
               MOVE "RCP0004" TO RC-L-REASON(WS-I)
               MOVE "RCP0004" TO CA-REASON-CODE
               MOVE 8 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A serial traceable receipt line carries "
                      "exactly one unit; split the delivery into "
                      "one line for each serial number"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 2100-EXIT
           END-IF
           IF RC-L-STOCKED(WS-I) = "Y" AND RC-L-LOCATION(WS-I) = SPACES
              AND NOT RC-OWN-DROPSHIP AND RC-OWNERSHIP NOT = "INCOTERMS"
               MOVE "REJECT" TO RC-L-OUTCOME(WS-I)
               MOVE "RCP0005" TO RC-L-REASON(WS-I)
               MOVE "RCP0005" TO CA-REASON-CODE
               MOVE 8 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A stocked receipt must say where the goods "
                      "were put away; the location is not optional"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 2100-EXIT
           END-IF
           PERFORM 2200-TOLERANCE THRU 2200-EXIT
           IF RC-L-OUTCOME(WS-I) = "HOLD"
               ADD 1 TO RC-LINES-HELD
               MOVE "Y" TO WS-HELD-SW
               GO TO 2100-EXIT
           END-IF
           IF RC-L-NEEDS-INSP(WS-I)
               MOVE RC-L-RECEIVED(WS-I) TO RC-L-QUARANT(WS-I)
               MOVE 0 TO RC-L-ACCEPTED(WS-I)
               ADD 1 TO RC-LINES-QUARANT
           ELSE
               MOVE RC-L-RECEIVED(WS-I) TO RC-L-ACCEPTED(WS-I)
           END-IF
           COMPUTE WS-LINE-GBP ROUNDED =
                   RC-L-RECEIVED(WS-I) * RC-L-PRICE(WS-I)
                   / RC-FX-RATE
           MOVE WS-LINE-GBP TO RC-L-VALUE-GBP(WS-I)
           ADD WS-LINE-GBP TO RC-TOTAL-GBP
      *>     A standard costed item is taken into stock at standard
      *>     and the difference is recognised at once, so the stock
      *>     value never silently carries a buying error (D-25).
           IF RC-L-STD-COST(WS-I) > 0
               COMPUTE WS-STD-GBP ROUNDED =
                       RC-L-STOCK-QTY(WS-I) * RC-L-STD-COST(WS-I)
               COMPUTE RC-L-PPV-GBP(WS-I) = WS-LINE-GBP - WS-STD-GBP
               ADD RC-L-PPV-GBP(WS-I) TO RC-PPV-GBP
           END-IF
           ADD 1 TO WS-OK-LINES.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> TOLCHK OWNS THE D-08 ARITHMETIC.  A RECEIPT WITHIN TOLERANCE
      *> IS CONFIRMED AND THE EXCESS STAYS SEPARATELY VISIBLE; ABOVE
      *> TOLERANCE THE LINE IS HELD FOR AN AMENDED ORDER (PO-011).
      *>----------------------------------------------------------------
       2200-TOLERANCE SECTION.
       2200-START.
           IF RC-L-ORDERED(WS-I) <= 0
               MOVE 0 TO RC-L-OUTSTAND(WS-I)
               GO TO 2200-EXIT
           END-IF
           INITIALIZE WS-TOLCHK-CA
           MOVE "QTY" TO TK-FUNCTION
           MOVE CA-COMPANY TO TK-COMPANY
           MOVE CA-SITE TO TK-SITE
           MOVE CA-USER TO TK-USER
           MOVE CA-BUSINESS-DATE TO TK-BUSINESS-DATE
           MOVE RC-ORDER-NUMBER TO TC-ORDER-NUMBER
           MOVE RC-L-PO-LINE(WS-I) TO TC-LINE-NUM
           MOVE RC-SUPPLIER TO TC-SUPPLIER
           MOVE RC-L-ITEM(WS-I) TO TC-ITEM-CODE
           MOVE "PASSED" TO TC-RULE-SOURCE
           MOVE RC-L-ORDERED(WS-I) TO TC-ORDERED-QTY
           MOVE RC-L-CANCELLED(WS-I) TO TC-CANCELLED-QTY
           MOVE RC-L-COUNTED(WS-I) TO TC-COUNTED-QTY
           MOVE RC-L-RECEIVED(WS-I) TO TC-PROPOSED-QTY
           MOVE RC-L-OVER-PCT(WS-I) TO TC-OVER-PCT
           MOVE "BLOCK" TO TC-ACTION-OVER
           CALL "TOLCHK" USING WS-TOLCHK-CA
           MOVE TC-OUTSTANDING-QTY TO RC-L-OUTSTAND(WS-I)
           MOVE TC-OVER-QTY TO RC-L-OVER-QTY(WS-I)
           IF TK-RETURN-CODE >= 6
               MOVE "HOLD" TO RC-L-OUTCOME(WS-I)
               MOVE TK-REASON-CODE TO RC-L-REASON(WS-I)
           ELSE
               IF TK-RETURN-CODE = 4
                   MOVE "WARN" TO RC-L-OUTCOME(WS-I)
                   MOVE TK-REASON-CODE TO RC-L-REASON(WS-I)
               END-IF
           END-IF.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> CONFIRMATION.  THE RECEIPT HEADER AND LINES ARE WRITTEN FIRST
      *> AND COMMITTED, THEN THE STOCK IS MOVED, THEN THE ACCRUAL IS
      *> POSTED.  POPOST IS IDEMPOTENT ON THE CORRELATION ID, SO A
      *> RETRY AFTER A FAILURE REPEATS THE WORK WITHOUT DOUBLING IT.
      *>----------------------------------------------------------------
       3000-CONFIRM SECTION.
       3000-START.
           PERFORM 3100-WRITE-HEAD THRU 3100-EXIT
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > RC-LINE-CNT OR CA-FAILED
               IF RC-L-OUTCOME(WS-I) NOT = "REJECT"
                  AND RC-L-OUTCOME(WS-I) NOT = "HOLD"
                   PERFORM 3200-MOVE-STOCK THRU 3200-EXIT
                   IF NOT CA-FAILED
                       PERFORM 3300-WRITE-LINE THRU 3300-EXIT
                   END-IF
               END-IF
           END-PERFORM
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           MOVE RC-TOTAL-GBP TO RC-GRNI-GBP
           PERFORM 3500-POST-GRNI THRU 3500-EXIT
           IF NOT CA-FAILED
               PERFORM 3600-STAMP THRU 3600-EXIT
               IF NOT CA-FAILED
                   MOVE "CONFIRMED" TO RC-STATUS
               END-IF
           END-IF.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3100-WRITE-HEAD SECTION.
       3100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.Receipt (CompanyCode, SiteCode, "
                  "ReceiptRef, SupplierCode, PurchaseOrderNumber, "
                  "DeliveryNoteRef, PhysicalDate, PostingDate, "
                  "WarehouseCode, OwnershipBasis, IsStockInTransit, "
                  "ShipmentEvidenceRef, ImportEntryRef, "
                  "SubcontractJobRef, Status, CorrelationId, "
                  "ReceivedBy, CreatedBy) OUTPUT INSERTED.ReceiptId "
                  "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,'DRAFT',?,?,?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE CA-SITE TO SQLIO-PARM-VAL(2)
           MOVE RC-RECEIPT-REF TO SQLIO-PARM-VAL(3)
           MOVE RC-SUPPLIER TO SQLIO-PARM-VAL(4)
           MOVE RC-ORDER-NUMBER TO SQLIO-PARM-VAL(5)
           MOVE RC-DELIVERY-NOTE TO SQLIO-PARM-VAL(6)
           MOVE RC-PHYSICAL-DATE TO SQLIO-PARM-VAL(7)
           MOVE RC-POSTING-DATE TO SQLIO-PARM-VAL(8)
           MOVE RC-WAREHOUSE TO SQLIO-PARM-VAL(9)
           MOVE RC-OWNERSHIP TO SQLIO-PARM-VAL(10)
           IF RC-IS-TRANSIT = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(11)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(11)
           END-IF
           MOVE RC-SHIP-EVIDENCE TO SQLIO-PARM-VAL(12)
           MOVE RC-IMPORT-ENTRY TO SQLIO-PARM-VAL(13)
           MOVE RC-SUBCONTRACT-JOB TO SQLIO-PARM-VAL(14)
           MOVE WS-CORR TO SQLIO-PARM-VAL(15)
           MOVE RC-RECEIVED-BY TO SQLIO-PARM-VAL(16)
           MOVE CA-USER TO SQLIO-PARM-VAL(17)
           PERFORM 8100-NULL-BLANKS THRU 8100-EXIT
           MOVE 17 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-SQLCODE NOT = 0
               MOVE "RCP0010" TO CA-REASON-CODE
               MOVE SQLIO-ERRMSG TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3100-EXIT
           END-IF
           MOVE FUNCTION NUMVAL(WS-C-VALUE) TO RC-RECEIPT-ID.
       3100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> STOCK.  AN INCOTERMS RECEIPT GOES TO THE SITE TRANSIT
      *> LOCATION AND IS NOT AVAILABLE TO PICK UNTIL IT LANDS; A DROP
      *> SHIPPED LINE NEVER BECOMES OUR STOCK AT ALL, SO NO MOVEMENT
      *> IS RAISED FOR IT (PO-018).  GOODS AWAITING INSPECTION ARE
      *> RECEIVED INTO THE QUARANTINE CONDITION (PO-010).
      *>----------------------------------------------------------------
       3200-MOVE-STOCK SECTION.
       3200-START.
           IF RC-OWN-DROPSHIP OR RC-L-STOCKED(WS-I) NOT = "Y"
               GO TO 3200-EXIT
           END-IF
           INITIALIZE WS-STKMOVE-CA
           MOVE "RECEIPT" TO SK-FUNCTION
           MOVE CA-COMPANY TO SK-COMPANY
           MOVE CA-SITE TO SK-SITE
           MOVE CA-USER TO SK-USER
           MOVE CA-BUSINESS-DATE TO SK-BUSINESS-DATE
           MOVE SPACES TO SK-CORRELATION-ID
           MOVE RC-L-NUM(WS-I) TO WS-NUM-ED
           STRING FUNCTION TRIM(WS-CORR-SHORT) "-L"
                  FUNCTION TRIM(WS-NUM-ED)
               DELIMITED BY SIZE INTO SK-CORRELATION-ID
           MOVE RC-L-ITEM(WS-I) TO SM-ITEM-CODE
           MOVE RC-L-REVISION(WS-I) TO SM-REVISION
           MOVE CA-SITE TO SM-SITE
           MOVE RC-WAREHOUSE TO SM-WAREHOUSE
           MOVE RC-L-LOCATION(WS-I) TO SM-LOCATION
           MOVE RC-L-LOT(WS-I) TO SM-LOT-CODE
           MOVE RC-L-SERIAL(WS-I) TO SM-SERIAL-NO
           MOVE "OWNED" TO SM-OWNER-TYPE
           IF RC-RETENTION-TITLE = "Y"
               MOVE "ROT" TO SM-OWNER-TYPE
               MOVE RC-SUPPLIER TO SM-OWNER-CODE
           END-IF
           IF RC-L-NEEDS-INSP(WS-I)
               MOVE "QUARANTINE" TO SM-CONDITION
           ELSE
               MOVE "ACCEPTED" TO SM-CONDITION
           END-IF
           MOVE RC-L-RECEIVED(WS-I) TO SM-QUANTITY
           MOVE RC-L-UOM(WS-I) TO SM-UOM-CODE
           COMPUTE SM-VALUE-GBP =
               RC-L-VALUE-GBP(WS-I) - RC-L-PPV-GBP(WS-I)
           IF RC-L-STOCK-QTY(WS-I) > 0
               COMPUTE SM-UNIT-COST ROUNDED =
                   SM-VALUE-GBP / RC-L-STOCK-QTY(WS-I)
           END-IF
           MOVE WS-MOVE-DATE TO SM-MOVEMENT-DATE
           MOVE RC-POSTING-DATE TO SM-POSTING-DATE
           MOVE RC-L-EXPIRY(WS-I) TO SM-EXPIRY-DATE
           MOVE "PORECEIPT" TO SM-REASON-CODE
           MOVE SPACES TO SM-REASON
           STRING "Goods received on " FUNCTION TRIM(RC-RECEIPT-REF)
                  " against order " FUNCTION TRIM(RC-ORDER-NUMBER)
               DELIMITED BY SIZE INTO SM-REASON
           MOVE "PO" TO SM-SOURCE-MODULE
           MOVE "GOODSRECEIPT" TO SM-SOURCE-DOC-TYPE
           MOVE RC-RECEIPT-REF TO SM-SOURCE-DOC-REF
           MOVE RC-L-NUM(WS-I) TO SM-SOURCE-DOC-LINE
           IF RC-IS-TRANSIT = "Y"
               MOVE "Y" TO SM-VIA-TRANSIT
           END-IF
           CALL "STKMOVE" USING WS-STKMOVE-CA
           MOVE SM-MOVEMENT-ID TO RC-L-MOVEMENT(WS-I)
           IF SM-STOCK-QTY NOT = 0
               MOVE SM-STOCK-QTY TO RC-L-STOCK-QTY(WS-I)
               MOVE SM-FACTOR TO RC-L-FACTOR(WS-I)
           END-IF
           IF SK-RETURN-CODE > 4
               MOVE "REJECT" TO RC-L-OUTCOME(WS-I)
               MOVE "RCP0010" TO RC-L-REASON(WS-I)
               MOVE SK-RETURN-CODE TO CA-RETURN-CODE
               MOVE "RCP0010" TO CA-REASON-CODE
               MOVE SK-MESSAGE TO CA-MESSAGE
               SUBTRACT RC-L-VALUE-GBP(WS-I) FROM RC-TOTAL-GBP
               MOVE 0 TO RC-L-VALUE-GBP(WS-I)
           END-IF.
       3200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3300-WRITE-LINE SECTION.
       3300-START.
      *>   THE HEADER INSERT LEFT NULL FLAGS SET ON ITS OWN BLANK
      *>   PARAMETERS.  CLEAR THEM ALL FIRST OR A BLANK DELIVERY NOTE
      *>   WOULD NULL THE QUARANTINE QUANTITY AND LOSE THE LINE.
           PERFORM VARYING WS-N FROM 1 BY 1 UNTIL WS-N > 40
               MOVE "N" TO SQLIO-PARM-NULL(WS-N)
           END-PERFORM
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.ReceiptLine (ReceiptId, LineNum, "
                  "PurchaseOrderNumber, PurchaseOrderLine, "
                  "ScheduleNo, ItemCode, Revision, Description, "
                  "ReceivedQty, UomCode, ConversionFactor, "
                  "StockQty, AcceptedQty, QuarantineQty, "
                  "OverDeliveryQty, LocationCode, SupplierLotRef, "
                  "LotCode, SerialNo, UnitPrice, "
                  "ProvisionalValueGBP, StandardCostGBP, "
                  "PriceVarianceGBP, QualityStatus, "
                  "InspectionRequired, MovementId) VALUES "
                  "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,"
                  "?,?,?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE RC-RECEIPT-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE RC-L-NUM(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE RC-ORDER-NUMBER TO SQLIO-PARM-VAL(3)
           MOVE RC-L-PO-LINE(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE RC-L-SCHEDULE(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE RC-L-ITEM(WS-I) TO SQLIO-PARM-VAL(6)
           MOVE RC-L-REVISION(WS-I) TO SQLIO-PARM-VAL(7)
           MOVE RC-L-DESC(WS-I) TO SQLIO-PARM-VAL(8)
           MOVE RC-L-RECEIVED(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE RC-L-UOM(WS-I) TO SQLIO-PARM-VAL(10)
           MOVE RC-L-FACTOR(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE RC-L-STOCK-QTY(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE RC-L-ACCEPTED(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           MOVE RC-L-QUARANT(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(14)
           MOVE RC-L-OVER-QTY(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(15)
           MOVE RC-L-LOCATION(WS-I) TO SQLIO-PARM-VAL(16)
           MOVE RC-L-SUPP-LOT(WS-I) TO SQLIO-PARM-VAL(17)
           MOVE RC-L-LOT(WS-I) TO SQLIO-PARM-VAL(18)
           MOVE RC-L-SERIAL(WS-I) TO SQLIO-PARM-VAL(19)
           MOVE RC-L-PRICE(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(20)
           MOVE RC-L-VALUE-GBP(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(21)
           MOVE RC-L-STD-COST(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(22)
           MOVE RC-L-PPV-GBP(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(23)
           IF RC-L-NEEDS-INSP(WS-I)
               MOVE "QUARANTINE" TO SQLIO-PARM-VAL(24)
               MOVE "1" TO SQLIO-PARM-VAL(25)
           ELSE
               MOVE "ACCEPTED" TO SQLIO-PARM-VAL(24)
               MOVE "0" TO SQLIO-PARM-VAL(25)
           END-IF
           MOVE RC-L-MOVEMENT(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(26)
           IF RC-L-MOVEMENT(WS-I) = 0
               MOVE "Y" TO SQLIO-PARM-NULL(26)
           END-IF
           MOVE 26 TO SQLIO-PARM-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           MOVE SQLIO-SQLCODE TO WS-LINE-SQLCODE
           MOVE SQLIO-ERRMSG TO WS-LINE-ERRMSG
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF WS-LINE-SQLCODE NOT = 0
               MOVE "RCP0011" TO CA-REASON-CODE
               MOVE WS-LINE-ERRMSG TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       3300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> GRNI (PO-017).  THE LIABILITY FOR GOODS HELD BUT NOT INVOICED
      *> IS RECOGNISED AT THE ORDER PRICE FOR THE QUANTITY RECEIVED,
      *> SO THE PERIOD IN WHICH THE GOODS ARRIVED CARRIES THEIR COST
      *> WHETHER OR NOT THE SUPPLIER HAS INVOICED.  A STANDARD COSTED
      *> LINE TAKES STOCK IN AT STANDARD AND RECOGNISES THE VARIANCE.
      *>----------------------------------------------------------------
       3500-POST-GRNI SECTION.
       3500-START.
           IF RC-TOTAL-GBP = 0
               GO TO 3500-EXIT
           END-IF
           INITIALIZE WS-POPOST
           MOVE "POST" TO PP-FUNC
           MOVE CA-COMPANY TO PP-COMPANY
           MOVE CA-SITE TO PP-SITE
           MOVE CA-USER TO PP-USER
           MOVE CA-BUSINESS-DATE TO PP-BUSINESS-DATE
           MOVE WS-POST-CORR TO PP-CORRELATION
           MOVE RC-POSTING-DATE TO PP-POSTING-DATE
           MOVE RC-PHYSICAL-DATE TO PP-DOCUMENT-DATE
           MOVE "GBP" TO PP-CURRENCY
           MOVE 1 TO PP-FX-RATE
           MOVE PO-EV-RECEIPT TO PP-EVENT-CODE
           MOVE RC-RECEIPT-REF TO PP-SOURCE-REF
           MOVE "PO" TO PP-SUBLEDGER-TYPE
           MOVE RC-SUPPLIER TO PP-SUBLEDGER-REF
           MOVE RC-DELIVERY-NOTE TO PP-EVIDENCE-REF
           MOVE SPACES TO PP-DESCRIPTION
           STRING "Goods received " FUNCTION TRIM(RC-RECEIPT-REF)
                  " from " FUNCTION TRIM(RC-SUPPLIER)
                  " on order " FUNCTION TRIM(RC-ORDER-NUMBER)
               DELIMITED BY SIZE INTO PP-DESCRIPTION
           COMPUTE WS-STOCK-GBP = RC-TOTAL-GBP - RC-PPV-GBP
           MOVE 1 TO WS-N
           MOVE PO-ACC-STOCK TO PP-L-ACCOUNT(WS-N)
           MOVE WS-STOCK-GBP TO PP-L-DR(WS-N)
           MOVE 0 TO PP-L-CR(WS-N)
           MOVE "Goods received into stock"
             TO PP-L-NARRATIVE(WS-N)
           IF RC-PPV-GBP NOT = 0
               ADD 1 TO WS-N
               MOVE PO-ACC-PPV TO PP-L-ACCOUNT(WS-N)
               MOVE RC-PPV-GBP TO PP-L-DR(WS-N)
               MOVE 0 TO PP-L-CR(WS-N)
               MOVE "PURCH" TO PP-L-COST-CTR(WS-N)
               MOVE "Purchase price variance on receipt"
                 TO PP-L-NARRATIVE(WS-N)
           END-IF
           ADD 1 TO WS-N
           MOVE PO-ACC-GRNI TO PP-L-ACCOUNT(WS-N)
           MOVE 0 TO PP-L-DR(WS-N)
           MOVE RC-TOTAL-GBP TO PP-L-CR(WS-N)
           MOVE "Goods received not invoiced"
             TO PP-L-NARRATIVE(WS-N)
           MOVE WS-N TO PP-LINE-CNT
           CALL "POPOST" USING WS-POPOST
           MOVE PP-JOURNAL-NUMBER TO RC-JOURNAL-NUMBER
           IF PP-WAS-DUPLICATE = "Y"
               MOVE "Y" TO RC-WAS-DUPLICATE
           END-IF
           IF PP-RC > 4
               MOVE PP-RC TO CA-RETURN-CODE
               MOVE "RCP0011" TO CA-REASON-CODE
               MOVE PP-MESSAGE TO CA-MESSAGE
           END-IF.
       3500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3600-STAMP SECTION.
       3600-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC po.usp_ReceiptDeliveryState ?, ?, ?, "
                  "'CONFIRMED', ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE CA-SITE TO SQLIO-PARM-VAL(2)
           MOVE RC-RECEIPT-REF TO SQLIO-PARM-VAL(3)
           MOVE RC-GRNI-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE WS-POST-CORR TO SQLIO-PARM-VAL(5)
           MOVE CA-USER TO SQLIO-PARM-VAL(6)
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           PERFORM 8200-CHECK-WRITE THRU 8200-EXIT.
       3600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> TRANSIT CONFIRMATION (PO-014).  THE GOODS WERE ALREADY OURS
      *> AND ALREADY ACCRUED WHEN THEY LEFT THE SUPPLIER; LANDING THEM
      *> IS A STOCK MOVE ONLY AND MUST NOT ACCRUE A SECOND TIME.
      *>----------------------------------------------------------------
       4000-TRANSIT SECTION.
       4000-START.
           IF RC-LINE-CNT <= 0
               MOVE "RCP0001" TO CA-REASON-CODE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > RC-LINE-CNT
               INITIALIZE WS-STKMOVE-CA
               MOVE "CONFIRM" TO SK-FUNCTION
               MOVE CA-COMPANY TO SK-COMPANY
               MOVE CA-SITE TO SK-SITE
               MOVE CA-USER TO SK-USER
               MOVE CA-BUSINESS-DATE TO SK-BUSINESS-DATE
               MOVE RC-L-ITEM(WS-I) TO SM-ITEM-CODE
               MOVE CA-SITE TO SM-SITE
               MOVE RC-WAREHOUSE TO SM-TO-WAREHOUSE
               MOVE RC-L-LOCATION(WS-I) TO SM-TO-LOCATION
               MOVE RC-L-LOT(WS-I) TO SM-LOT-CODE
               MOVE RC-L-SERIAL(WS-I) TO SM-SERIAL-NO
               MOVE RC-L-RECEIVED(WS-I) TO SM-QUANTITY
               MOVE RC-L-UOM(WS-I) TO SM-UOM-CODE
               MOVE CA-BUSINESS-DATE TO SM-MOVEMENT-DATE
               MOVE "PO" TO SM-SOURCE-MODULE
               MOVE "GOODSRECEIPT" TO SM-SOURCE-DOC-TYPE
               MOVE RC-RECEIPT-REF TO SM-SOURCE-DOC-REF
               MOVE RC-L-NUM(WS-I) TO SM-SOURCE-DOC-LINE
               CALL "STKMOVE" USING WS-STKMOVE-CA
               MOVE SM-MOVEMENT-ID TO RC-L-MOVEMENT(WS-I)
               IF SK-RETURN-CODE > 4
                   MOVE SK-RETURN-CODE TO CA-RETURN-CODE
                   MOVE "RCP0010" TO CA-REASON-CODE
                   MOVE SK-MESSAGE TO CA-MESSAGE
               END-IF
           END-PERFORM
           IF NOT CA-FAILED
               MOVE "N" TO RC-IS-TRANSIT
               MOVE "CONFIRMED" TO RC-STATUS
               MOVE SPACES TO SQLIO-STMT
               STRING "UPDATE po.Receipt SET IsStockInTransit = 0, "
                      "WarehouseCode = ?, ModifiedBy = ?, "
                      "ModifiedAt = SYSDATETIME() WHERE "
                      "CompanyCode = ? AND ReceiptRef = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE RC-WAREHOUSE TO SQLIO-PARM-VAL(1)
               MOVE CA-USER TO SQLIO-PARM-VAL(2)
               MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
               MOVE RC-RECEIPT-REF TO SQLIO-PARM-VAL(4)
               MOVE 4 TO SQLIO-PARM-CNT
               MOVE 3 TO SQLIO-CURSOR
               MOVE "EXEC" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
               MOVE "CLOS" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           END-IF.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> REVERSAL.  A TRUE REVERSAL, NOT A NEGATIVE RECEIPT: THE
      *> ORIGINAL ROW IS MARKED REVERSED SO THAT THE COUNTED QUANTITY
      *> FALLS AND THE OUTSTANDING COMMITMENT COMES BACK.  THE STOCK
      *> AND THE ACCRUAL ARE BOTH PUT BACK, NEVER ONE WITHOUT THE
      *> OTHER.
      *>----------------------------------------------------------------
       5000-REVERSE SECTION.
       5000-START.
           IF RC-REVERSE-REASON = SPACES
               MOVE "RCP0012" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A reversal must say why; an unexplained "
                      "reversal of a goods receipt is not auditable"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 5000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ReceiptId AS varchar(30)), "
                  "CAST(GrniValueGBP AS varchar(30)) FROM "
                  "po.Receipt WHERE CompanyCode = ? AND "
                  "ReceiptRef = ? AND SiteCode = ? "
                  "AND Status = 'CONFIRMED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE RC-REVERSE-REF TO SQLIO-PARM-VAL(2)
           MOVE CA-SITE TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-TWO-COL
           IF SQLIO-SQLCODE NOT = 0 OR WS-C2-ONE = SPACES
               MOVE 16 TO CA-RETURN-CODE
               MOVE "RCP0012" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Receipt " FUNCTION TRIM(RC-REVERSE-REF)
                      " is not a confirmed receipt of this company"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 5000-EXIT
           END-IF
           MOVE FUNCTION NUMVAL(WS-C2-TWO) TO RC-GRNI-GBP
           PERFORM 5200-REV-STOCK THRU 5200-EXIT
           PERFORM 5400-REV-POST THRU 5400-EXIT
           IF NOT CA-FAILED
               MOVE SPACES TO SQLIO-STMT
               STRING "EXEC po.usp_ReceiptDeliveryState ?, ?, ?, "
                      "'REVERSED', 0, NULL, ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
               MOVE CA-SITE TO SQLIO-PARM-VAL(2)
               MOVE RC-REVERSE-REF TO SQLIO-PARM-VAL(3)
               MOVE CA-USER TO SQLIO-PARM-VAL(4)
               MOVE 4 TO SQLIO-PARM-CNT
               MOVE 3 TO SQLIO-CURSOR
               MOVE "EXEC" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
               PERFORM 8200-CHECK-WRITE THRU 8200-EXIT
               IF NOT CA-FAILED
                   MOVE "REVERSED" TO RC-STATUS
               END-IF
           END-IF.
       5000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       5200-REV-STOCK SECTION.
       5200-START.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > RC-LINE-CNT
               IF RC-L-STOCK-QTY(WS-I) > 0
                  AND RC-L-STOCKED(WS-I) = "Y"
                  AND NOT RC-OWN-DROPSHIP
                   INITIALIZE WS-STKMOVE-CA
                   MOVE "ISSUE" TO SK-FUNCTION
                   MOVE CA-COMPANY TO SK-COMPANY
                   MOVE CA-SITE TO SK-SITE
                   MOVE CA-USER TO SK-USER
                   MOVE CA-BUSINESS-DATE TO SK-BUSINESS-DATE
                   MOVE RC-L-ITEM(WS-I) TO SM-ITEM-CODE
                   MOVE CA-SITE TO SM-SITE
                   MOVE RC-WAREHOUSE TO SM-WAREHOUSE
                   MOVE RC-L-LOCATION(WS-I) TO SM-LOCATION
                   MOVE RC-L-LOT(WS-I) TO SM-LOT-CODE
                   MOVE RC-L-SERIAL(WS-I) TO SM-SERIAL-NO
                   MOVE RC-L-RECEIVED(WS-I) TO SM-QUANTITY
                   MOVE RC-L-REVISION(WS-I) TO SM-REVISION
                   IF RC-L-QUARANT(WS-I) > 0
                       MOVE "QUARANTINE" TO SM-CONDITION
                   END-IF
                   MOVE RC-L-UOM(WS-I) TO SM-UOM-CODE
                   COMPUTE SM-VALUE-GBP =
                       RC-L-VALUE-GBP(WS-I) - RC-L-PPV-GBP(WS-I)
                   COMPUTE SM-UNIT-COST ROUNDED =
                       SM-VALUE-GBP / RC-L-STOCK-QTY(WS-I)
                   MOVE CA-BUSINESS-DATE TO SM-MOVEMENT-DATE
                   MOVE "PORCPTREV" TO SM-REASON-CODE
                   MOVE RC-REVERSE-REASON TO SM-REASON
                   MOVE "PO" TO SM-SOURCE-MODULE
                   MOVE "RECEIPTREVERSAL" TO SM-SOURCE-DOC-TYPE
                   MOVE RC-REVERSE-REF TO SM-SOURCE-DOC-REF
                   MOVE RC-L-NUM(WS-I) TO SM-SOURCE-DOC-LINE
                   CALL "STKMOVE" USING WS-STKMOVE-CA
                   MOVE SM-MOVEMENT-ID TO RC-L-MOVEMENT(WS-I)
                   IF SK-RETURN-CODE > 4
                       MOVE SK-RETURN-CODE TO CA-RETURN-CODE
                       MOVE "RCP0010" TO CA-REASON-CODE
                       MOVE SK-MESSAGE TO CA-MESSAGE
                   END-IF
               END-IF
           END-PERFORM.
       5200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       5400-REV-POST SECTION.
       5400-START.
           IF RC-GRNI-GBP = 0 OR CA-FAILED
               GO TO 5400-EXIT
           END-IF
           INITIALIZE WS-POPOST
           MOVE "POST" TO PP-FUNC
           MOVE CA-COMPANY TO PP-COMPANY
           MOVE CA-SITE TO PP-SITE
           MOVE CA-USER TO PP-USER
           MOVE CA-BUSINESS-DATE TO PP-BUSINESS-DATE
           MOVE SPACES TO PP-CORRELATION
           STRING "RCPREV-" FUNCTION TRIM(CA-COMPANY) "-"
                  FUNCTION TRIM(RC-REVERSE-REF)
               DELIMITED BY SIZE INTO PP-CORRELATION
           MOVE CA-BUSINESS-DATE TO PP-POSTING-DATE
           MOVE CA-BUSINESS-DATE TO PP-DOCUMENT-DATE
           MOVE "GBP" TO PP-CURRENCY
           MOVE 1 TO PP-FX-RATE
           MOVE PO-EV-RECEIPT TO PP-EVENT-CODE
           MOVE RC-REVERSE-REF TO PP-SOURCE-REF
           MOVE "PO" TO PP-SUBLEDGER-TYPE
           MOVE RC-SUPPLIER TO PP-SUBLEDGER-REF
           MOVE SPACES TO PP-DESCRIPTION
           STRING "Reversal of goods receipt "
                  FUNCTION TRIM(RC-REVERSE-REF) " - "
                  FUNCTION TRIM(RC-REVERSE-REASON)
               DELIMITED BY SIZE INTO PP-DESCRIPTION
           MOVE PO-ACC-GRNI TO PP-L-ACCOUNT(1)
           MOVE RC-GRNI-GBP TO PP-L-DR(1)
           MOVE 0 TO PP-L-CR(1)
           MOVE "GRNI reversed with the receipt"
             TO PP-L-NARRATIVE(1)
           MOVE PO-ACC-STOCK TO PP-L-ACCOUNT(2)
           MOVE 0 TO PP-L-DR(2)
           MOVE RC-GRNI-GBP TO PP-L-CR(2)
           MOVE "Stock removed with the receipt"
             TO PP-L-NARRATIVE(2)
           MOVE 2 TO PP-LINE-CNT
           CALL "POPOST" USING WS-POPOST
           MOVE PP-JOURNAL-NUMBER TO RC-JOURNAL-NUMBER
           IF PP-RC > 4
               MOVE PP-RC TO CA-RETURN-CODE
               MOVE "RCP0011" TO CA-REASON-CODE
               MOVE PP-MESSAGE TO CA-MESSAGE
           END-IF.
       5400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AN EMPTY OPTIONAL REFERENCE IS A NULL, NOT AN EMPTY STRING,
      *> SO THAT THE UNIQUE INDEXES AND THE REPORTS BEHAVE.
      *>----------------------------------------------------------------
       8100-NULL-BLANKS SECTION.
       8100-START.
           PERFORM VARYING WS-N FROM 5 BY 1 UNTIL WS-N > 14
               IF SQLIO-PARM-VAL(WS-N) = SPACES
                   MOVE "Y" TO SQLIO-PARM-NULL(WS-N)
               END-IF
           END-PERFORM.
       8100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8200-CHECK-WRITE SECTION.
       8200-START.
           MOVE SQLIO-SQLCODE TO WS-LINE-SQLCODE
           MOVE SQLIO-ERRMSG TO WS-LINE-ERRMSG
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF WS-LINE-SQLCODE NOT = 0
               MOVE "RCP0010" TO CA-REASON-CODE
               MOVE WS-LINE-ERRMSG TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       8200-EXIT.
           EXIT.
      *>   Driver UOW ownership, not a batch's @@TRANCOUNT, controls
      *>   commit. Header, stock, line identity, GRNI and delivery
      *>   state must persist together or not at all.
       8300-BEGIN-UOW SECTION.
       8300-START.
           MOVE "TSTA" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               MOVE SQLIO-ERRMSG TO CA-MESSAGE
               MOVE "RCP0010" TO CA-REASON-CODE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 8300-EXIT
           END-IF
           IF SQLIO-ROWCOUNT = 1
               MOVE "BEGN" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
               IF SQLIO-ERROR
                   MOVE SQLIO-ERRMSG TO CA-MESSAGE
                   MOVE "RCP0010" TO CA-REASON-CODE
                   PERFORM 9000-REJECT THRU 9000-EXIT
                   GO TO 8300-EXIT
               END-IF
               MOVE "Y" TO WS-UOW-OWN
           END-IF
           MOVE "Y" TO WS-UOW-ACTIVE
           MOVE SPACES TO SQLIO-STMT
           STRING "IF @@TRANCOUNT = 0 BEGIN TRANSACTION; "
                  "SAVE TRANSACTION RECEIPT"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               MOVE SQLIO-ERRMSG TO CA-MESSAGE
               MOVE "RCP0010" TO CA-REASON-CODE
               PERFORM 9000-REJECT THRU 9000-EXIT
               PERFORM 8400-END-UOW THRU 8400-EXIT
           ELSE
               MOVE "CLOS" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           END-IF.
       8300-EXIT.
           EXIT.

       8400-END-UOW SECTION.
       8400-START.
           MOVE 0 TO SQLIO-PARM-CNT SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           IF WS-UOW-OWN = "Y"
               IF CA-FAILED
                   MOVE "ROLB" TO SQLIO-FUNC
               ELSE
                   MOVE "COMT" TO SQLIO-FUNC
               END-IF
               CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
               IF SQLIO-ERROR AND NOT CA-FAILED
                   MOVE SQLIO-ERRMSG TO CA-MESSAGE
                   MOVE "RCP0010" TO CA-REASON-CODE
                   PERFORM 9000-REJECT THRU 9000-EXIT
               END-IF
           ELSE
               IF CA-FAILED
                   MOVE SPACES TO SQLIO-STMT
                   STRING "IF XACT_STATE() = 1 "
                          "ROLLBACK TRANSACTION RECEIPT"
                       DELIMITED BY SIZE INTO SQLIO-STMT
                   MOVE "EXEC" TO SQLIO-FUNC
                   CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
                   MOVE "CLOS" TO SQLIO-FUNC
                   CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
               END-IF
           END-IF
           MOVE "N" TO WS-UOW-ACTIVE.
       8400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE 8 TO CA-RETURN-CODE
           MOVE "Y" TO WS-FATAL-SW
           MOVE "REJECTED" TO RC-STATUS.
       9000-EXIT.
           EXIT.
       END PROGRAM RECEIPT.
