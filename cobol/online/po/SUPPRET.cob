      *>================================================================
      *> PROGRAM-ID : SUPPRET
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : RETURN GOODS TO THE SUPPLIER.  THE DESPATCH AND
      *>              THE EXPECTED CREDIT ARE TWO SEPARATE FACTS: THE
      *>              GOODS LEAVE STOCK AT THE COST THEY CAME IN AT,
      *>              AND THE CREDIT IS EXPECTED AND CHASED UNTIL THE
      *>              SUPPLIER'S CREDIT NOTE ARRIVES.  A RETURN NEVER
      *>              AUTOMATICALLY REOPENS THE PURCHASE ORDER.
      *> CALLED BY  : RETURNS SCREEN, INSPECTION DISPOSITION
      *> INTERFACE  : CALL "SUPPRET" USING DFHCOMMAREA  (SUPPRETCA)
      *> CALLS      : STKMOVE, POPOST, DATECALC, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-09-14 RJH  CR0833 RETURNS TO SUPPLIER
      *> 2002-04-08 DJM  CR1056 EXPECTED CREDIT CHASING
      *> 2026-10-06 STERP PO-013, AC-055 WORKED CASE
      *>================================================================
      *> REQ: PO-013, PO-017, PO-020
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUPPRET.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 14/09/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY POACCNTS.
       COPY POPOSTWK.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DATECALC-CA==
                      LEADING ==CA-== BY ==DH-==.
       COPY STKMOVECA REPLACING ==DFHCOMMAREA== BY ==WS-STKMOVE-CA==
                      LEADING ==CA-== BY ==SK-==.
       COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==WS-NUM-CA==
                     LEADING ==CA-== BY ==NM-==.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-ONE-COL.
           05  WS-C-VALUE              PIC X(60).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-N                    PIC S9(3) VALUE 0.
           05  WS-QTY-ED               PIC -(9)9.999999.
           05  WS-AMT-ED               PIC -(11)9.99.
           05  WS-LINE-ED              PIC -(8)9.
           05  WS-CORR                 PIC X(64).
           05  WS-C-AMT                PIC X(40).
           05  WS-C-STATUS             PIC X(12).
       LINKAGE SECTION.
       COPY SUPPRETCA.
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
                   PERFORM 2000-VALIDATE THRU 2000-EXIT
                   IF NOT CA-FAILED
                       PERFORM 2800-WRITE-HEAD THRU 2800-EXIT
                   END-IF
               WHEN "DESPATCH"
                   PERFORM 2000-VALIDATE THRU 2000-EXIT
                   IF NOT CA-FAILED
                       PERFORM 2800-WRITE-HEAD THRU 2800-EXIT
                       PERFORM 3000-DESPATCH THRU 3000-EXIT
                   END-IF
               WHEN "CREDIT"
                   PERFORM 4000-CREDIT THRU 4000-EXIT
               WHEN "ENQUIRE"
                   PERFORM 6000-ENQUIRE THRU 6000-EXIT
               WHEN OTHER
                   MOVE "SRT0007" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                          " is not supported by SUPPRET"
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
           MOVE "SUPPRET" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE 0 TO SR-RETURN-ID SR-TOTAL-GBP SR-CLEAR-TOTAL-GBP
           MOVE 0 TO SR-VARIANCE-GBP SR-EXPECTED-CR-GBP
           MOVE 0 TO SR-CHASE-DUE-DATE
           MOVE SPACES TO SR-JOURNAL-NUMBER
           MOVE "N" TO SR-REOPENS-ORDER
           MOVE "OPEN" TO SR-STATUS
           MOVE "OK" TO SR-OUTCOME
           IF SR-RETURN-DATE = 0
               MOVE CA-BUSINESS-DATE TO SR-RETURN-DATE
           END-IF
           IF SR-POSTING-DATE = 0
               MOVE SR-RETURN-DATE TO SR-POSTING-DATE
           END-IF
           IF SR-CREDIT-TERMS-DAY = 0
               MOVE 30 TO SR-CREDIT-TERMS-DAY
           END-IF
           IF SR-AFTER-INVOICE
               MOVE "RETCLEAR" TO SR-CLEARING-TARGET
           ELSE
               MOVE "GRNI" TO SR-CLEARING-TARGET
           END-IF
           MOVE SPACES TO WS-CORR
           STRING "SRT-" FUNCTION TRIM(CA-COMPANY) "-"
                  FUNCTION TRIM(SR-RETURN-REF)
               DELIMITED BY SIZE INTO WS-CORR
           IF CA-CORRELATION-ID NOT = SPACES
               MOVE CA-CORRELATION-ID TO WS-CORR
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> VALIDATION.  A RETURN IS VALUED AT THE COST THE GOODS CAME IN
      *> AT, BECAUSE THAT IS THE LIABILITY BEING CLEARED.  WHERE THE
      *> CARRYING VALUE DIFFERS - A STANDARD COST CHANGE, A REVALUED
      *> LOT - THE DIFFERENCE IS A PURCHASE PRICE VARIANCE AND NOT A
      *> SILENT STOCK GAIN.
      *>----------------------------------------------------------------
      *>----------------------------------------------------------------
      *> THE RETURN NOTE NUMBER.  A CALLER MAY PRESENT ITS OWN (THE
      *> SUPPLIER'S RMA PAPERWORK SOMETIMES DICTATES IT); IF IT DOES
      *> NOT, ONE IS DRAWN HERE SO THAT NOTHING IS EVER WRITTEN
      *> AGAINST A BLANK RETURN REFERENCE.  REQ: PO-014
      *>----------------------------------------------------------------
       1100-NUMBER SECTION.
       1100-START.
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO NM-FUNCTION
           MOVE CA-COMPANY TO NM-COMPANY
           MOVE CA-SITE TO NM-SITE
           MOVE CA-USER TO NM-USER
           MOVE SR-RETURN-DATE TO NM-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO NM-CORRELATION-ID
           MOVE "PORTN" TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF NM-RETURN-CODE > 4
               MOVE "SRT0012" TO CA-REASON-CODE
               MOVE NM-MESSAGE TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1100-EXIT
           END-IF
           MOVE NG-DOCUMENT-NUMBER TO SR-RETURN-REF
      *>   THE CORRELATION WAS BUILT IN 1000-INIT FROM A RETURN
      *>   REFERENCE THAT DID NOT EXIST YET, SO EVERY RETURN THAT
      *>   DREW ITS OWN NUMBER SHARED THE ONE CORRELATION AND POPOST
      *>   READ THE SECOND POSTING AS A REPLAY OF THE FIRST.  REBUILD
      *>   IT NOW THE REFERENCE IS KNOWN, AS RECEIPT DOES.  A CALLER
      *>   SUPPLIED CORRELATION STILL WINS.
           IF CA-CORRELATION-ID = SPACES
               MOVE SPACES TO WS-CORR
               STRING "SRT-" FUNCTION TRIM(CA-COMPANY) "-"
                      FUNCTION TRIM(SR-RETURN-REF)
                   DELIMITED BY SIZE INTO WS-CORR
           END-IF.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2000-VALIDATE SECTION.
       2000-START.
           IF SR-LINE-CNT <= 0
               MOVE "SRT0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A return must say what is going back"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF SR-LINE-CNT > 12
               MOVE 12 TO SR-LINE-CNT
           END-IF
           IF SR-RETURN-REF = SPACES
               PERFORM 1100-NUMBER THRU 1100-EXIT
               IF CA-FAILED
                   GO TO 2000-EXIT
               END-IF
           END-IF
           IF SR-REASON-CODE = SPACES
               MOVE "SRT0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A return must state its reason; the supplier "
                      "quality measure and any claim both depend "
                      "on it"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           IF CA-FUNCTION = "DESPATCH" AND SR-DESPATCH-EVID = SPACES
               MOVE "SRT0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Record the carrier consignment note before "
                      "the goods are taken out of stock; without it "
                      "a disputed return cannot be evidenced"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           MOVE 0 TO SR-TOTAL-GBP SR-CLEAR-TOTAL-GBP SR-VARIANCE-GBP
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > SR-LINE-CNT
               MOVE "OK" TO SR-L-OUTCOME(WS-I)
               MOVE 0 TO SR-L-RETURN-GBP(WS-I)
               MOVE 0 TO SR-L-CLEAR-GBP(WS-I)
               MOVE 0 TO SR-L-VAR-GBP(WS-I)
               MOVE 0 TO SR-L-MOVEMENT(WS-I)
               IF SR-L-RETURNED(WS-I) <= 0
                   MOVE "REJECT" TO SR-L-OUTCOME(WS-I)
                   MOVE 8 TO CA-RETURN-CODE
                   MOVE "SRT0001" TO CA-REASON-CODE
               ELSE
                   IF SR-L-RECEIVED(WS-I) > 0
                      AND SR-L-RETURNED(WS-I) > SR-L-RECEIVED(WS-I)
                       MOVE "REJECT" TO SR-L-OUTCOME(WS-I)
                       MOVE 8 TO CA-RETURN-CODE
                       MOVE "SRT0002" TO CA-REASON-CODE
                       MOVE SR-L-RECEIVED(WS-I) TO WS-QTY-ED
                       MOVE SPACES TO CA-MESSAGE
                       STRING "Only " FUNCTION TRIM(WS-QTY-ED)
                              " was received on this line; more "
                              "cannot be sent back than ever "
                              "arrived"
                           DELIMITED BY SIZE INTO CA-MESSAGE
                   ELSE
                       IF SR-L-CARRY-COST(WS-I) = 0
                           MOVE SR-L-RCPT-COST(WS-I)
                             TO SR-L-CARRY-COST(WS-I)
                       END-IF
                       COMPUTE SR-L-CLEAR-GBP(WS-I) ROUNDED =
                               SR-L-RETURNED(WS-I)
                               * SR-L-RCPT-COST(WS-I)
                       COMPUTE SR-L-RETURN-GBP(WS-I) ROUNDED =
                               SR-L-RETURNED(WS-I)
                               * SR-L-CARRY-COST(WS-I)
                       COMPUTE SR-L-VAR-GBP(WS-I) =
                               SR-L-CLEAR-GBP(WS-I)
                               - SR-L-RETURN-GBP(WS-I)
                       ADD SR-L-RETURN-GBP(WS-I) TO SR-TOTAL-GBP
                       ADD SR-L-CLEAR-GBP(WS-I)
                        TO SR-CLEAR-TOTAL-GBP
                       ADD SR-L-VAR-GBP(WS-I) TO SR-VARIANCE-GBP
                   END-IF
               END-IF
           END-PERFORM
           IF CA-FAILED
               GO TO 2000-EXIT
           END-IF
           MOVE SR-CLEAR-TOTAL-GBP TO SR-EXPECTED-CR-GBP
           PERFORM 2400-CHASE-DATE THRU 2400-EXIT
           IF SR-REPLACEMENT = "Y" AND SR-REPLACE-ORDER = SPACES
               MOVE 4 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A replacement was requested but no "
                      "replacement order has been raised; the "
                      "original order is NOT reopened automatically"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE EXPECTED CREDIT IS CHASED ON A WORKING DAY, NOT ON A
      *> SATURDAY THAT NOBODY IS IN TO CHASE IT.
      *>----------------------------------------------------------------
       2400-CHASE-DATE SECTION.
       2400-START.
           INITIALIZE WS-DATECALC-CA
           MOVE "ADDBUS" TO DH-FUNCTION
           MOVE CA-COMPANY TO DH-COMPANY
           MOVE CA-SITE TO DH-SITE
           MOVE CA-USER TO DH-USER
           MOVE CA-BUSINESS-DATE TO DH-BUSINESS-DATE
           MOVE SR-RETURN-DATE TO DC-DATE-1
           MOVE SR-CREDIT-TERMS-DAY TO DC-NUMBER
           CALL "DATECALC" USING WS-DATECALC-CA
           IF DH-RETURN-CODE > 4 OR DC-RESULT-DATE = 0
               COMPUTE SR-CHASE-DUE-DATE = SR-RETURN-DATE
           ELSE
               MOVE DC-RESULT-DATE TO SR-CHASE-DUE-DATE
           END-IF.
       2400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2800-WRITE-HEAD SECTION.
       2800-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.SupplierReturn (CompanyCode, SiteCode, "
                  "ReturnRef, SupplierCode, SupplierRmaRef, "
                  "OriginalReceiptRef, PurchaseOrderNumber, "
                  "ReturnDate, PostingDate, ReasonCode, Reason, "
                  "DespatchEvidence, ExpectedCreditGBP, "
                  "CreditChaseDueDate, ReplacementExpected, "
                  "ReplacementOrderNumber, IsPostInvoice, Status, "
                  "CreatedBy) OUTPUT INSERTED.SupplierReturnId "
                  "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE CA-SITE TO SQLIO-PARM-VAL(2)
           MOVE SR-RETURN-REF TO SQLIO-PARM-VAL(3)
           MOVE SR-SUPPLIER TO SQLIO-PARM-VAL(4)
           MOVE SR-RMA-REF TO SQLIO-PARM-VAL(5)
           MOVE SR-RECEIPT-REF TO SQLIO-PARM-VAL(6)
           MOVE SR-ORDER-NUMBER TO SQLIO-PARM-VAL(7)
           MOVE SR-RETURN-DATE TO SQLIO-PARM-VAL(8)
           MOVE SR-POSTING-DATE TO SQLIO-PARM-VAL(9)
           MOVE SR-REASON-CODE TO SQLIO-PARM-VAL(10)
           MOVE SR-REASON TO SQLIO-PARM-VAL(11)
           MOVE SR-DESPATCH-EVID TO SQLIO-PARM-VAL(12)
           MOVE SR-EXPECTED-CR-GBP TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           MOVE SR-CHASE-DUE-DATE TO SQLIO-PARM-VAL(14)
           IF SR-REPLACEMENT = "Y"
               MOVE "1" TO SQLIO-PARM-VAL(15)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(15)
           END-IF
           MOVE SR-REPLACE-ORDER TO SQLIO-PARM-VAL(16)
           IF SR-AFTER-INVOICE
               MOVE "1" TO SQLIO-PARM-VAL(17)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(17)
           END-IF
           IF CA-FUNCTION = "DESPATCH"
               MOVE "DESPATCHED" TO SQLIO-PARM-VAL(18)
               MOVE "DESPATCHED" TO SR-STATUS
           ELSE
               MOVE "OPEN" TO SQLIO-PARM-VAL(18)
           END-IF
           MOVE CA-USER TO SQLIO-PARM-VAL(19)
           PERFORM VARYING WS-N FROM 1 BY 1 UNTIL WS-N > 19
               IF WS-N > 4 AND WS-N < 17 AND
                  SQLIO-PARM-VAL(WS-N) = SPACES
                   MOVE "Y" TO SQLIO-PARM-NULL(WS-N)
               ELSE
                   MOVE "N" TO SQLIO-PARM-NULL(WS-N)
               END-IF
           END-PERFORM
           MOVE 19 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-SQLCODE NOT = 0
               MOVE "SRT0005" TO CA-REASON-CODE
               MOVE SQLIO-ERRMSG TO CA-MESSAGE
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2800-EXIT
           END-IF
           MOVE FUNCTION NUMVAL(WS-C-VALUE) TO SR-RETURN-ID
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > SR-LINE-CNT
               IF SR-L-OUTCOME(WS-I) = "OK"
                   PERFORM 2900-WRITE-LINE THRU 2900-EXIT
               END-IF
           END-PERFORM.
       2800-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2900-WRITE-LINE SECTION.
       2900-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.SupplierReturnLine (SupplierReturnId, "
                  "LineNum, ReceiptRef, ReceiptLine, ItemCode, "
                  "Revision, LotCode, SerialNo, ReturnedQty, "
                  "UomCode, StockQty, WasAccepted, "
                  "AttributableUnitCostGBP, ReturnValueGBP, "
                  "ClearingValueGBP, VarianceGBP) VALUES "
                  "(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE SR-RETURN-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE SR-L-NUM(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE SR-RECEIPT-REF TO SQLIO-PARM-VAL(3)
           MOVE SR-L-RCPT-LINE(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE SR-L-ITEM(WS-I) TO SQLIO-PARM-VAL(5)
           MOVE SR-L-REVISION(WS-I) TO SQLIO-PARM-VAL(6)
           MOVE SR-L-LOT(WS-I) TO SQLIO-PARM-VAL(7)
           MOVE SR-L-SERIAL(WS-I) TO SQLIO-PARM-VAL(8)
           MOVE SR-L-RETURNED(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE SR-L-UOM(WS-I) TO SQLIO-PARM-VAL(10)
           MOVE SR-L-RETURNED(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           IF SR-L-WAS-ACCEPT(WS-I) = "N"
               MOVE "0" TO SQLIO-PARM-VAL(12)
           ELSE
               MOVE "1" TO SQLIO-PARM-VAL(12)
           END-IF
           MOVE SR-L-RCPT-COST(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           MOVE SR-L-RETURN-GBP(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(14)
           MOVE SR-L-CLEAR-GBP(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(15)
           MOVE SR-L-VAR-GBP(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(16)
           PERFORM VARYING WS-N FROM 6 BY 1 UNTIL WS-N > 8
               IF SQLIO-PARM-VAL(WS-N) = SPACES
                   MOVE "Y" TO SQLIO-PARM-NULL(WS-N)
               END-IF
           END-PERFORM
           MOVE 16 TO SQLIO-PARM-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       2900-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> DESPATCH.  THE GOODS LEAVE STOCK FROM THE CONDITION THEY WERE
      *> HELD IN - QUARANTINED GOODS LEAVE QUARANTINE, SO AN
      *> INSPECTION REJECT FOLLOWED BY A RETURN MOVES THE SAME UNITS
      *> ONCE, NOT TWICE (PO-010, PO-013).
      *>----------------------------------------------------------------
       3000-DESPATCH SECTION.
       3000-START.
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > SR-LINE-CNT
               IF SR-L-OUTCOME(WS-I) = "OK"
                   PERFORM 3200-MOVE-OUT THRU 3200-EXIT
               END-IF
           END-PERFORM
           IF NOT CA-FAILED
               PERFORM 3500-POST THRU 3500-EXIT
           END-IF.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3200-MOVE-OUT SECTION.
       3200-START.
           INITIALIZE WS-STKMOVE-CA
           MOVE "ISSUE" TO SK-FUNCTION
           MOVE CA-COMPANY TO SK-COMPANY
           MOVE CA-SITE TO SK-SITE
           MOVE CA-USER TO SK-USER
           MOVE CA-BUSINESS-DATE TO SK-BUSINESS-DATE
           MOVE SR-L-NUM(WS-I) TO WS-LINE-ED
           MOVE SPACES TO SK-CORRELATION-ID
           STRING FUNCTION TRIM(WS-CORR) "-L"
                  FUNCTION TRIM(WS-LINE-ED)
               DELIMITED BY SIZE INTO SK-CORRELATION-ID
           MOVE SR-L-ITEM(WS-I) TO SM-ITEM-CODE
           MOVE SR-L-REVISION(WS-I) TO SM-REVISION
           MOVE CA-SITE TO SM-SITE
           MOVE SR-WAREHOUSE TO SM-WAREHOUSE
           MOVE SR-L-LOCATION(WS-I) TO SM-LOCATION
           MOVE SR-L-LOT(WS-I) TO SM-LOT-CODE
           MOVE SR-L-SERIAL(WS-I) TO SM-SERIAL-NO
           IF SR-L-CONDITION(WS-I) = SPACES
               MOVE "GOOD" TO SM-CONDITION
           ELSE
               MOVE SR-L-CONDITION(WS-I) TO SM-CONDITION
           END-IF
           MOVE SR-L-RETURNED(WS-I) TO SM-QUANTITY
           MOVE SR-L-UOM(WS-I) TO SM-UOM-CODE
           MOVE SR-L-CARRY-COST(WS-I) TO SM-UNIT-COST
           MOVE SR-L-RETURN-GBP(WS-I) TO SM-VALUE-GBP
           MOVE SR-RETURN-DATE TO SM-MOVEMENT-DATE
           MOVE SR-POSTING-DATE TO SM-POSTING-DATE
           MOVE SR-REASON-CODE TO SM-REASON-CODE
           MOVE SPACES TO SM-REASON
           STRING "Returned to supplier on "
                  FUNCTION TRIM(SR-RETURN-REF) " - "
                  FUNCTION TRIM(SR-REASON)
               DELIMITED BY SIZE INTO SM-REASON
           MOVE SR-DESPATCH-EVID TO SM-APPROVAL-REF
           MOVE "PO" TO SM-SOURCE-MODULE
           MOVE "SUPPLIERRETURN" TO SM-SOURCE-DOC-TYPE
           MOVE SR-RETURN-REF TO SM-SOURCE-DOC-REF
           MOVE SR-L-NUM(WS-I) TO SM-SOURCE-DOC-LINE
           CALL "STKMOVE" USING WS-STKMOVE-CA
           MOVE SM-MOVEMENT-ID TO SR-L-MOVEMENT(WS-I)
           IF SK-RETURN-CODE > 4
               MOVE "REJECT" TO SR-L-OUTCOME(WS-I)
               MOVE SK-RETURN-CODE TO CA-RETURN-CODE
               MOVE "SRT0005" TO CA-REASON-CODE
               MOVE SK-MESSAGE TO CA-MESSAGE
           END-IF.
       3200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> POSTING.  BEFORE THE INVOICE THE RETURN CLEARS GOODS RECEIVED
      *> NOT INVOICED, SO THE LEDGER STOPS CARRYING A LIABILITY FOR
      *> GOODS WE NO LONGER HOLD.  AFTER THE INVOICE THE LIABILITY IS
      *> REAL AND THE EXPECTED CREDIT IS A RETURN CLEARING BALANCE
      *> THAT THE SUPPLIER'S CREDIT NOTE SETTLES - NEVER AN ASSUMED
      *> REDUCTION OF THE CREDITOR.
      *>----------------------------------------------------------------
       3500-POST SECTION.
       3500-START.
           IF SR-CLEAR-TOTAL-GBP = 0
               GO TO 3500-EXIT
           END-IF
           INITIALIZE WS-POPOST
           MOVE "POST" TO PP-FUNC
           MOVE CA-COMPANY TO PP-COMPANY
           MOVE CA-SITE TO PP-SITE
           MOVE CA-USER TO PP-USER
           MOVE CA-BUSINESS-DATE TO PP-BUSINESS-DATE
           MOVE WS-CORR TO PP-CORRELATION
           MOVE SR-POSTING-DATE TO PP-POSTING-DATE
           MOVE SR-RETURN-DATE TO PP-DOCUMENT-DATE
           MOVE "GBP" TO PP-CURRENCY
           MOVE 1 TO PP-FX-RATE
           MOVE PO-EV-RETURN TO PP-EVENT-CODE
           MOVE SR-RETURN-REF TO PP-SOURCE-REF
           MOVE "PO" TO PP-SUBLEDGER-TYPE
           MOVE SR-SUPPLIER TO PP-SUBLEDGER-REF
           MOVE SR-DESPATCH-EVID TO PP-EVIDENCE-REF
           MOVE SPACES TO PP-DESCRIPTION
           STRING "Return to supplier "
                  FUNCTION TRIM(SR-RETURN-REF) " - "
                  FUNCTION TRIM(SR-REASON-CODE)
               DELIMITED BY SIZE INTO PP-DESCRIPTION
           MOVE 1 TO WS-N
           IF SR-AFTER-INVOICE
               MOVE PO-ACC-CREDITOR TO PP-L-ACCOUNT(WS-N)
               MOVE "Expected credit from supplier, chased"
                 TO PP-L-NARRATIVE(WS-N)
           ELSE
               MOVE PO-ACC-GRNI TO PP-L-ACCOUNT(WS-N)
               MOVE "GRNI cleared, goods no longer held"
                 TO PP-L-NARRATIVE(WS-N)
           END-IF
           MOVE SR-CLEAR-TOTAL-GBP TO PP-L-DR(WS-N)
           MOVE 0 TO PP-L-CR(WS-N)
           ADD 1 TO WS-N
           MOVE PO-ACC-STOCK TO PP-L-ACCOUNT(WS-N)
           MOVE 0 TO PP-L-DR(WS-N)
           MOVE SR-TOTAL-GBP TO PP-L-CR(WS-N)
           MOVE "Stock returned to supplier"
             TO PP-L-NARRATIVE(WS-N)
           IF SR-VARIANCE-GBP NOT = 0
               ADD 1 TO WS-N
               MOVE PO-ACC-PPV TO PP-L-ACCOUNT(WS-N)
               MOVE 0 TO PP-L-DR(WS-N)
               MOVE SR-VARIANCE-GBP TO PP-L-CR(WS-N)
               MOVE "PURCH" TO PP-L-COST-CTR(WS-N)
               MOVE "Carrying value differs from receipt cost"
                 TO PP-L-NARRATIVE(WS-N)
           END-IF
           MOVE WS-N TO PP-LINE-CNT
           CALL "POPOST" USING WS-POPOST
           MOVE PP-JOURNAL-NUMBER TO SR-JOURNAL-NUMBER
           IF PP-RC > 4
               MOVE PP-RC TO CA-RETURN-CODE
               MOVE "SRT0006" TO CA-REASON-CODE
               MOVE PP-MESSAGE TO CA-MESSAGE
           ELSE
               MOVE "DESPATCHED" TO SR-STATUS
               MOVE SR-EXPECTED-CR-GBP TO WS-AMT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "Goods despatched; "
                      FUNCTION TRIM(WS-AMT-ED)
                      " is EXPECTED from the supplier and is "
                      "chased, not taken"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 4 TO CA-RETURN-CODE
           END-IF.
       3500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> CREDIT NOTE MATCHING.  THE SUPPLIER'S CREDIT NOTE SETTLES THE
      *> EXPECTED CREDIT.  A SHORT CREDIT IS AN EXCEPTION TO CHASE,
      *> NOT A DIFFERENCE TO WRITE OFF QUIETLY.
      *>----------------------------------------------------------------
       4000-CREDIT SECTION.
       4000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ExpectedCreditGBP AS varchar(20)) "
                  "+ '|' + Status "
                  "FROM po.SupplierReturn WHERE CompanyCode = ? "
                  "AND ReturnRef = ? AND Status IN "
                  "('OPEN','DESPATCHED')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SR-RETURN-REF TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-SQLCODE NOT = 0 OR WS-C-VALUE = SPACES
               MOVE 16 TO CA-RETURN-CODE
               MOVE "SRT0008" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Return " FUNCTION TRIM(SR-RETURN-REF)
                      " is not an open return of this company"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO WS-C-AMT WS-C-STATUS
           UNSTRING WS-C-VALUE DELIMITED BY "|"
               INTO WS-C-AMT WS-C-STATUS
           END-UNSTRING
           MOVE FUNCTION NUMVAL(WS-C-AMT) TO SR-EXPECTED-CR-GBP
           COMPUTE SR-VARIANCE-GBP =
                   SR-EXPECTED-CR-GBP - SR-CREDIT-VALUE-GBP
           IF SR-VARIANCE-GBP = 0
               MOVE SPACES TO SQLIO-STMT
               STRING "UPDATE po.SupplierReturn SET Status = "
                      "'CREDITED', CreditNoteRef = ?, "
                      "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                      "WHERE CompanyCode = ? AND ReturnRef = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE "CREDITED" TO SR-STATUS
           ELSE
      *>       A CREDIT NOTE FOR THE WRONG MONEY SETTLES NOTHING AND
      *>       MOVES NO GOODS, SO THE RETURN KEEPS THE STATUS IT
      *>       ALREADY HAD.  WRITING "DESPATCHED" HERE CLAIMED A
      *>       PHYSICAL DESPATCH THAT MAY NEVER HAVE HAPPENED AND PUT
      *>       AN OPEN RETURN INTO THE DESPATCHED COHORT.
               MOVE SPACES TO SQLIO-STMT
               STRING "UPDATE po.SupplierReturn SET CreditNoteRef "
                      "= ?, ModifiedBy = ?, ModifiedAt = "
                      "SYSDATETIME() WHERE CompanyCode = ? AND "
                      "ReturnRef = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE WS-C-STATUS TO SR-STATUS
               MOVE 4 TO CA-RETURN-CODE
               MOVE "SRT0009" TO CA-REASON-CODE
               MOVE SR-VARIANCE-GBP TO WS-AMT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING "The credit note differs from the expected "
                      "credit by " FUNCTION TRIM(WS-AMT-ED)
                      "; the difference stays visible as an "
                      "exception to chase"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           MOVE SR-CREDIT-NOTE-REF TO SQLIO-PARM-VAL(1)
           MOVE CA-USER TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(3)
           MOVE SR-RETURN-REF TO SQLIO-PARM-VAL(4)
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE 6 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       6000-ENQUIRE SECTION.
       6000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ISNULL(SUM(ExpectedCreditGBP),0) AS "
                  "varchar(40)) FROM po.SupplierReturn WHERE "
                  "CompanyCode = ? AND SupplierCode = ? AND "
                  "Status IN ('OPEN','DESPATCHED')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SR-SUPPLIER TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-SQLCODE = 0 AND WS-C-VALUE NOT = SPACES
               MOVE FUNCTION NUMVAL(WS-C-VALUE)
                 TO SR-EXPECTED-CR-GBP
           END-IF.
       6000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE 8 TO CA-RETURN-CODE
           MOVE "Y" TO WS-FATAL-SW
           MOVE "REJECTED" TO SR-OUTCOME.
       9000-EXIT.
           EXIT.
       END PROGRAM SUPPRET.
