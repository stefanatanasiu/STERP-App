      *>================================================================
      *> PROGRAM-ID : TPOSRET
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : UNIT TESTS FOR SUPPRET, RETURN OF GOODS TO THE
      *>              SUPPLIER (PO-013).
      *>              THE SUITE RAISES ITS OWN RETURNS THROUGH SUPPRET
      *>              AND DELETES THEM AGAIN, SO IT MAY BE RUN AS
      *>              OFTEN AS YOU LIKE AGAINST A LIVE DATABASE.
      *>              WITHOUT A CONNECTION IT SKIPS.
      *>
      *>              ONLY CREATE, CREDIT AND ENQUIRE ARE EXERCISED.
      *>              DESPATCH TAKES THE GOODS OUT OF STOCK AND POSTS
      *>              A JOURNAL, AND A UNIT TEST HAS NO BUSINESS
      *>              LEAVING LEDGER ENTRIES BEHIND IT.
      *>
      *>              THE THINGS THAT MATTER ARE TESTED DELIBERATELY:
      *>              1. THE CREDIT IS EXPECTED, NOT TAKEN.  IT IS
      *>                 CHASED FROM A DUE DATE AND IT CLEARS GRNI
      *>                 BEFORE THE INVOICE AND A RETURN CLEARING
      *>                 BALANCE AFTER IT.
      *>              2. A RETURN NEVER SILENTLY REOPENS THE ORDER
      *>                 (AC-055); A REPLACEMENT IS A DECISION AND
      *>                 HAS TO NAME ITS ORDER.
      *>              3. A CREDIT NOTE FOR THE WRONG MONEY DOES NOT
      *>                 CLOSE THE RETURN, IT RAISES A VARIANCE.
      *>              4. EVERY ALLOCATED RETURN GETS ITS OWN
      *>                 REFERENCE, BECAUSE THE POSTING CORRELATION
      *>                 IS BUILT FROM IT - TWO RETURNS SHARING ONE
      *>                 CORRELATION MEANS THE SECOND POSTING IS READ
      *>                 AS A REPLAY OF THE FIRST AND IS LOST.
      *>----------------------------------------------------------------
      *> 2026-10-07 STERP ORIGINAL
      *>================================================================
      *> REQ: PO-013, PO-017, PO-020
      *> AC: AC-055
      *> AC: the expected credit is chased, never assumed
      *> AC: a return does not reopen the order by itself
      *> AC: a credit note that disagrees raises a variance
      *> AC: each allocated return carries its own correlation
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TPOSRET.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY SUPPRETCA REPLACING ==DFHCOMMAREA== BY ==WS-SR==.
       COPY SQLIOCA.
       01  WS-ROW                      PIC X(256).
       01  WS-DB-OK                    PIC X VALUE "N".
       01  WS-SUPPLIER                 PIC X(20) VALUE "S0001".
       01  WS-USER                     PIC X(20) VALUE "PURMGR".
       01  WS-ITEM                     PIC X(40)
                                       VALUE "SCREW-M4X10-SS".
       01  WS-RMA                      PIC X(40)
                                       VALUE "ZZ-TEST-SRT-RMA".
       01  WS-BEFORE-GBP               PIC S9(13)V99 VALUE 0.
       01  WS-AFTER-GBP                PIC S9(13)V99 VALUE 0.
       01  WS-REF-1                    PIC X(30) VALUE SPACES.
       01  WS-REF-2                    PIC X(30) VALUE SPACES.
       01  WS-CORR-SEQ                 PIC 9(4) VALUE 0.
       01  WS-CORR-ED                  PIC 9(4).
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           MOVE "po SUPPRET" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-CONNECT THRU 0100-EXIT
           IF WS-DB-OK = "Y"
               PERFORM 0200-CLEAN THRU 0200-EXIT
               PERFORM 1000-NO-LINES THRU 1000-EXIT
               PERFORM 1100-NO-REASON THRU 1100-EXIT
               PERFORM 1200-TOO-MANY THRU 1200-EXIT
               PERFORM 1300-CREATE THRU 1300-EXIT
               PERFORM 1400-AFTER-INVOICE THRU 1400-EXIT
               PERFORM 1500-REPLACEMENT THRU 1500-EXIT
               PERFORM 1600-ENQUIRE THRU 1600-EXIT
               PERFORM 1700-CREDIT-WRONG THRU 1700-EXIT
               PERFORM 1800-CREDIT-RIGHT THRU 1800-EXIT
               PERFORM 1900-CREDIT-UNKNOWN THRU 1900-EXIT
               PERFORM 2000-OWN-REFERENCE THRU 2000-EXIT
               PERFORM 2100-BAD-FUNCTION THRU 2100-EXIT
               PERFORM 0200-CLEAN THRU 0200-EXIT
               PERFORM 9000-DISCONNECT THRU 9000-EXIT
           ELSE
               MOVE "no database connection" TO TK-DETAIL
               MOVE "po SUPPRET needs the STERP database" TO TK-NAME
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
      *> EVERY RETURN THIS SUITE RAISES CARRIES THE ZZ TEST RMA
      *> REFERENCE, SO A RETURN WHOSE NUMBER WAS DRAWN FROM THE REAL
      *> SERIES CAN STILL BE FOUND AND REMOVED, AND NOTHING THAT IS
      *> NOT OURS CAN EVER BE DELETED.
      *>----------------------------------------------------------------
       0200-CLEAN SECTION.
       0200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE l FROM po.SupplierReturnLine l JOIN "
                  "po.SupplierReturn r ON r.SupplierReturnId = "
                  "l.SupplierReturnId WHERE r.SupplierRmaRef LIKE "
                  "'ZZ-TEST-SRT%' OR r.ReturnRef LIKE 'ZZ-TEST-SRT%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.SupplierReturn WHERE "
                  "SupplierRmaRef LIKE 'ZZ-TEST-SRT%' OR ReturnRef "
                  "LIKE 'ZZ-TEST-SRT%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT.
       0200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> ONE GOOD RETURN: TWENTY RECEIVED, FIVE GOING BACK AT THE
      *> COST THEY CAME IN AT.
      *>----------------------------------------------------------------
       0500-SETUP SECTION.
       0500-START.
           INITIALIZE WS-SR
           ADD 1 TO WS-CORR-SEQ
           MOVE WS-CORR-SEQ TO WS-CORR-ED
           MOVE SPACES TO CA-CORRELATION-ID
           STRING "ZZ-TEST-SRT-C" WS-CORR-ED
               DELIMITED BY SIZE INTO CA-CORRELATION-ID
           MOVE "CREATE" TO CA-FUNCTION
           MOVE "STC" TO CA-COMPANY
           MOVE "RDG" TO CA-SITE
           MOVE WS-USER TO CA-USER
           MOVE 20270601 TO CA-BUSINESS-DATE
           MOVE "ZZ-TEST-SRT-1" TO SR-RETURN-REF
           MOVE WS-SUPPLIER TO SR-SUPPLIER
           MOVE WS-RMA TO SR-RMA-REF
           MOVE "DAMAGED" TO SR-REASON-CODE
           MOVE "Crushed in transit, photographed on arrival"
             TO SR-REASON
           MOVE "MAIN" TO SR-WAREHOUSE
           MOVE 1 TO SR-LINE-CNT
           MOVE 1 TO SR-L-NUM(1)
           MOVE 1 TO SR-L-RCPT-LINE(1)
           MOVE WS-ITEM TO SR-L-ITEM(1)
           MOVE "EA" TO SR-L-UOM(1)
           MOVE "STKA01" TO SR-L-LOCATION(1)
           MOVE "GOOD" TO SR-L-CONDITION(1)
           MOVE "Y" TO SR-L-WAS-ACCEPT(1)
           MOVE 20 TO SR-L-RECEIVED(1)
           MOVE 5 TO SR-L-RETURNED(1)
           MOVE 2 TO SR-L-RCPT-COST(1).
       0500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-013.  A RETURN HAS TO SAY WHAT IS GOING BACK.
      *>----------------------------------------------------------------
       1000-NO-LINES SECTION.
       1000-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE 0 TO SR-LINE-CNT
           CALL "SUPPRET" USING WS-SR
           MOVE "PO-013 a return with no lines is refused" TO TK-NAME
           MOVE "SRT0001" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-013 and it is refused, not warned" TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-013.  THE SUPPLIER QUALITY MEASURE AND ANY CLAIM BOTH
      *> DEPEND ON THE REASON, SO IT IS NOT OPTIONAL.
      *>----------------------------------------------------------------
       1100-NO-REASON SECTION.
       1100-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE SPACES TO SR-REASON-CODE
           CALL "SUPPRET" USING WS-SR
           MOVE "PO-013 a return must state its reason" TO TK-NAME
           MOVE "SRT0003" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-013.  YOU CANNOT SEND BACK MORE THAN ARRIVED.
      *>----------------------------------------------------------------
       1200-TOO-MANY SECTION.
       1200-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE 2 TO SR-L-RECEIVED(1)
           MOVE 5 TO SR-L-RETURNED(1)
           CALL "SUPPRET" USING WS-SR
           MOVE "PO-013 returning more than was received is refused"
             TO TK-NAME
           MOVE "SRT0002" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-013.  THE GOOD CASE.  FIVE AT 2.00 IS 10.00 EXPECTED FROM
      *> THE SUPPLIER - EXPECTED, AND CHASED FROM A DUE DATE.  BEFORE
      *> THE INVOICE THE RETURN CLEARS GRNI.
      *>----------------------------------------------------------------
       1300-CREATE SECTION.
       1300-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           CALL "SUPPRET" USING WS-SR
           MOVE "PO-013 a complete return is accepted" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-013 valued at the cost the goods came in at"
             TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE SR-TOTAL-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-013 the credit is expected, not taken" TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE SR-EXPECTED-CR-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-013 before the invoice the return clears GRNI"
             TO TK-NAME
           MOVE "GRNI" TO TK-EXPECTED-X
           MOVE SR-CLEARING-TARGET TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-013 the carrying cost agrees, so no variance"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SR-VARIANCE-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-013 the expected credit has a chase date"
             TO TK-NAME
           IF SR-CHASE-DUE-DATE > 20270601
               MOVE "dated" TO TK-ACTUAL-X
           ELSE
               MOVE "undated" TO TK-ACTUAL-X
           END-IF
           MOVE "dated" TO TK-EXPECTED-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-013 the return opens, it does not post" TO TK-NAME
           MOVE "OPEN" TO TK-EXPECTED-X
           MOVE SR-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "AC-055 a return does not reopen the order"
             TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE SR-REOPENS-ORDER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-013 the return is on the file as OPEN" TO TK-NAME
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status FROM po.SupplierReturn WHERE "
                  "ReturnRef = 'ZZ-TEST-SRT-1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-ONE THRU 0920-EXIT
           MOVE "OPEN" TO TK-EXPECTED-X
           MOVE FUNCTION TRIM(WS-ROW(1:12)) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-013 and the expected credit is on the file"
             TO TK-NAME
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ExpectedCreditGBP AS varchar(20)) "
                  "FROM po.SupplierReturn WHERE ReturnRef = "
                  "'ZZ-TEST-SRT-1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-ONE THRU 0920-EXIT
           MOVE "10.00" TO TK-EXPECTED-X
           MOVE FUNCTION TRIM(WS-ROW(1:20)) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-017.  AFTER THE INVOICE THE LIABILITY IS REAL, SO THE
      *> RETURN CANNOT CLEAR GRNI - IT RAISES A RETURN CLEARING
      *> BALANCE THAT THE CREDIT NOTE SETTLES.
      *>----------------------------------------------------------------
       1400-AFTER-INVOICE SECTION.
       1400-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE "ZZ-TEST-SRT-2" TO SR-RETURN-REF
           MOVE "Y" TO SR-POST-INVOICE
           CALL "SUPPRET" USING WS-SR
           MOVE "PO-017 after the invoice the target is RETCLEAR"
             TO TK-NAME
           MOVE "RETCLEAR" TO TK-EXPECTED-X
           MOVE SR-CLEARING-TARGET TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-017 and the expected credit is unchanged by it"
             TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           MOVE SR-EXPECTED-CR-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-020, AC-055.  WHETHER THE GOODS ARE REPLACED IS A
      *> COMMERCIAL DECISION AND IS RECORDED AS ONE.  SAYING
      *> "REPLACEMENT" WITHOUT NAMING THE ORDER IS A LOOSE END, AND A
      *> LOOSE END IS WORTH A WARNING.
      *>----------------------------------------------------------------
       1500-REPLACEMENT SECTION.
       1500-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE "ZZ-TEST-SRT-3" TO SR-RETURN-REF
           MOVE "Y" TO SR-REPLACEMENT
           MOVE SPACES TO SR-REPLACE-ORDER
           CALL "SUPPRET" USING WS-SR
           MOVE "AC-055 a replacement with no order is warned about"
             TO TK-NAME
           MOVE 4 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "AC-055 and the order is still not reopened"
             TO TK-NAME
           MOVE "N" TO TK-EXPECTED-X
           MOVE SR-REOPENS-ORDER TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE "ZZ-TEST-SRT-4" TO SR-RETURN-REF
           MOVE "Y" TO SR-REPLACEMENT
           MOVE "ZZ-TEST-SRT-REPL" TO SR-REPLACE-ORDER
           CALL "SUPPRET" USING WS-SR
           MOVE "AC-055 a named replacement order is accepted"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM.
       1500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-013.  THE OUTSTANDING EXPECTED CREDIT FOR A SUPPLIER IS
      *> WHAT IS BEING CHASED.  THE DATABASE HOLDS REAL RETURNS AS
      *> WELL AS OURS, SO THE TEST MEASURES THE MOVEMENT AND NOT THE
      *> ABSOLUTE FIGURE.
      *>----------------------------------------------------------------
       1600-ENQUIRE SECTION.
       1600-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE "ENQUIRE" TO CA-FUNCTION
           CALL "SUPPRET" USING WS-SR
           MOVE SR-EXPECTED-CR-GBP TO WS-BEFORE-GBP
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE "ZZ-TEST-SRT-5" TO SR-RETURN-REF
           CALL "SUPPRET" USING WS-SR
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE "ENQUIRE" TO CA-FUNCTION
           CALL "SUPPRET" USING WS-SR
           MOVE SR-EXPECTED-CR-GBP TO WS-AFTER-GBP
           MOVE "PO-013 the enquiry is answered" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-013 a new return adds to what is chased"
             TO TK-NAME
           MOVE 10 TO TK-EXPECTED-N
           COMPUTE TK-ACTUAL-N = WS-AFTER-GBP - WS-BEFORE-GBP
           PERFORM TK-ASSERT-NUM.
       1600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-013.  A CREDIT NOTE FOR THE WRONG MONEY IS NOT A SETTLED
      *> RETURN.  IT IS A VARIANCE SOMEBODY HAS TO ARGUE ABOUT, AND
      *> THE RETURN STAYS OPEN WHILE THEY DO.
      *>----------------------------------------------------------------
       1700-CREDIT-WRONG SECTION.
       1700-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE "CREDIT" TO CA-FUNCTION
           MOVE "ZZ-TEST-SRT-1" TO SR-RETURN-REF
           MOVE "ZZ-TEST-SRT-CN1" TO SR-CREDIT-NOTE-REF
           MOVE 7 TO SR-CREDIT-VALUE-GBP
           CALL "SUPPRET" USING WS-SR
           MOVE "PO-013 a credit note that disagrees is not accepted"
             TO TK-NAME
           MOVE "SRT0009" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-013 the difference is shown as a variance"
             TO TK-NAME
           MOVE 3 TO TK-EXPECTED-N
           MOVE SR-VARIANCE-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-013 and the return is not closed by it"
             TO TK-NAME
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status FROM po.SupplierReturn WHERE "
                  "ReturnRef = 'ZZ-TEST-SRT-1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-ONE THRU 0920-EXIT
           MOVE "OPEN" TO TK-EXPECTED-X
           MOVE FUNCTION TRIM(WS-ROW(1:12)) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-013 nor does it claim the goods were despatched"
             TO TK-NAME
           MOVE "OPEN" TO TK-EXPECTED-X
           MOVE SR-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-013 but the credit note reference is recorded"
             TO TK-NAME
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT ISNULL(CreditNoteRef,'(none)') FROM "
                  "po.SupplierReturn WHERE ReturnRef = "
                  "'ZZ-TEST-SRT-1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-ONE THRU 0920-EXIT
           MOVE "ZZ-TEST-SRT-CN1" TO TK-EXPECTED-X
           MOVE FUNCTION TRIM(WS-ROW(1:24)) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1700-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-013.  THE CREDIT NOTE FOR THE RIGHT MONEY SETTLES IT.
      *>----------------------------------------------------------------
       1800-CREDIT-RIGHT SECTION.
       1800-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE "CREDIT" TO CA-FUNCTION
           MOVE "ZZ-TEST-SRT-1" TO SR-RETURN-REF
           MOVE "ZZ-TEST-SRT-CN2" TO SR-CREDIT-NOTE-REF
           MOVE 10 TO SR-CREDIT-VALUE-GBP
           CALL "SUPPRET" USING WS-SR
           MOVE "PO-013 the matching credit note settles the return"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-013 nothing is left in variance" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE SR-VARIANCE-GBP TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-013 the return is credited on the file"
             TO TK-NAME
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status FROM po.SupplierReturn WHERE "
                  "ReturnRef = 'ZZ-TEST-SRT-1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-ONE THRU 0920-EXIT
           MOVE "CREDITED" TO TK-EXPECTED-X
           MOVE FUNCTION TRIM(WS-ROW(1:12)) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-013 and it cannot be credited a second time"
             TO TK-NAME
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE "CREDIT" TO CA-FUNCTION
           MOVE "ZZ-TEST-SRT-1" TO SR-RETURN-REF
           MOVE "ZZ-TEST-SRT-CN3" TO SR-CREDIT-NOTE-REF
           MOVE 10 TO SR-CREDIT-VALUE-GBP
           CALL "SUPPRET" USING WS-SR
           MOVE "SRT0008" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1800-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1900-CREDIT-UNKNOWN SECTION.
       1900-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE "CREDIT" TO CA-FUNCTION
           MOVE "ZZ-TEST-SRT-NONE" TO SR-RETURN-REF
           MOVE "ZZ-TEST-SRT-CN9" TO SR-CREDIT-NOTE-REF
           MOVE 10 TO SR-CREDIT-VALUE-GBP
           CALL "SUPPRET" USING WS-SR
           MOVE "PO-013 a credit against no return is refused"
             TO TK-NAME
           MOVE "SRT0008" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1900-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-013.  WHEN THE CALLER DOES NOT BRING A REFERENCE ONE IS
      *> DRAWN FROM THE SERIES, AND THE POSTING CORRELATION IS BUILT
      *> FROM IT.  TWO RETURNS RAISED IN A ROW MUST THEREFORE GET TWO
      *> REFERENCES: IF THEY SHARED ONE, THEY WOULD SHARE A
      *> CORRELATION, AND THE SECOND RETURN'S POSTING WOULD BE READ
      *> AS A REPLAY OF THE FIRST AND SILENTLY DROPPED.
      *>----------------------------------------------------------------
       2000-OWN-REFERENCE SECTION.
       2000-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE SPACES TO SR-RETURN-REF
           MOVE SPACES TO CA-CORRELATION-ID
           CALL "SUPPRET" USING WS-SR
           MOVE SR-RETURN-REF TO WS-REF-1
           MOVE "PO-013 a return with no reference is given one"
             TO TK-NAME
           IF WS-REF-1 = SPACES
               MOVE "blank" TO TK-ACTUAL-X
           ELSE
               MOVE "allocated" TO TK-ACTUAL-X
           END-IF
           MOVE "allocated" TO TK-EXPECTED-X
           PERFORM TK-ASSERT-STR
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE SPACES TO SR-RETURN-REF
           MOVE SPACES TO CA-CORRELATION-ID
           CALL "SUPPRET" USING WS-SR
           MOVE SR-RETURN-REF TO WS-REF-2
           MOVE "PO-013 two allocated returns are two references"
             TO TK-NAME
           IF WS-REF-1 = WS-REF-2
               MOVE "same" TO TK-ACTUAL-X
           ELSE
               MOVE "different" TO TK-ACTUAL-X
           END-IF
           MOVE "different" TO TK-EXPECTED-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-013 both allocated returns are on the file"
             TO TK-NAME
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(DISTINCT ReturnRef) AS "
                  "varchar(12)) FROM po.SupplierReturn WHERE "
                  "SupplierRmaRef = 'ZZ-TEST-SRT-RMA' AND ReturnRef "
                  "NOT LIKE 'ZZ-TEST-SRT%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-ONE THRU 0920-EXIT
           MOVE "2" TO TK-EXPECTED-X
           MOVE FUNCTION TRIM(WS-ROW(1:12)) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2100-BAD-FUNCTION SECTION.
       2100-START.
           PERFORM 0500-SETUP THRU 0500-EXIT
           MOVE "SILLY" TO CA-FUNCTION
           CALL "SUPPRET" USING WS-SR
           MOVE "PO-013 an unsupported function is refused"
             TO TK-NAME
           MOVE "SRT0007" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
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
               DISPLAY "TPOSRET fixture SQL failed: "
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
       END PROGRAM TPOSRET.
