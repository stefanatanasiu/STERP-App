      *>================================================================
      *> PROGRAM-ID : TPOSOD
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : UNIT TESTS FOR THE PURCHASING SEPARATION OF
      *>              DUTIES CONTROLS AND THE APPROVAL ROUTE.  POREQ
      *>              MUST REFUSE A REQUESTER WHO APPROVES THEIR OWN
      *>              REQUISITION (COM-003), AND POAPPRV MUST FOLD A
      *>              PUNCTUATED APPROVAL TYPE ONTO THE ONE THE
      *>              APPROVAL MATRIX IS KEYED ON, OTHERWISE A BRAND
      *>              NEW ORDER CAN NEVER BE APPROVED AND NEVER BE
      *>              ISSUED.  THE SUITE MAKES ITS OWN REQUISITION
      *>              THROUGH POREQ AND CANCELS IT AGAIN, SO IT MAY BE
      *>              RUN AS OFTEN AS YOU LIKE AGAINST A LIVE
      *>              DATABASE.  WITHOUT A CONNECTION THE SUITE SKIPS.
      *>----------------------------------------------------------------
      *> 2026-10-07 STERP ORIGINAL
      *>================================================================
      *> REQ: PO-001, PO-005, PO-006, COM-003
      *> AC: AC-098 self approval refused, approval route for a new order
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TPOSOD.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY POREQCA REPLACING ==DFHCOMMAREA== BY ==WS-RQ==.
       COPY POAPPRVCA REPLACING ==DFHCOMMAREA== BY ==WS-AV==
                      LEADING ==CA-== BY ==AP-==.
       COPY SQLIOCA.
       01  WS-ROW                      PIC X(256).
       01  WS-DB-OK                    PIC X VALUE "N".
       01  WS-REQ-NO                   PIC X(30) VALUE SPACES.
       01  WS-RAISER                   PIC X(20) VALUE "ZZSODREQ".
       01  WS-APPROVER                 PIC X(20) VALUE "ZZSODAPR".
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           MOVE "po separation of duties" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-CONNECT THRU 0100-EXIT
           IF WS-DB-OK = "Y"
               PERFORM 1500-FOLD-TYPE THRU 1500-EXIT
               PERFORM 0300-RAISE THRU 0300-EXIT
               IF WS-REQ-NO NOT = SPACES
                   PERFORM 1000-SELF-REFUSED THRU 1000-EXIT
                   PERFORM 1100-STILL-SUBMITTED THRU 1100-EXIT
                   PERFORM 1200-SELF-AS-CA-USER THRU 1200-EXIT
                   PERFORM 1300-INDEPENDENT-OK THRU 1300-EXIT
                   PERFORM 0800-CLEAN THRU 0800-EXIT
               ELSE
                   MOVE SPACES TO TK-DETAIL
                   STRING "the requisition fixture could not be "
                          "raised: "
                          FUNCTION TRIM(CA-REASON-CODE OF WS-RQ) " "
                          CA-MESSAGE OF WS-RQ (1:80)
                       DELIMITED BY SIZE INTO TK-DETAIL
                   MOVE "po POREQ separation of duties" TO TK-NAME
                   PERFORM TK-SKIP
               END-IF
               PERFORM 9000-DISCONNECT THRU 9000-EXIT
           ELSE
               MOVE "no database connection" TO TK-DETAIL
               MOVE "po separation of duties needs the STERP database"
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
      *> THE FIXTURE IS RAISED THROUGH POREQ ITSELF, SO THE TEST IS
      *> MEASURING THE SERVICE AND NOT A HAND BUILT ROW.
      *>----------------------------------------------------------------
       0300-RAISE SECTION.
       0300-START.
           INITIALIZE WS-RQ
           PERFORM 0950-HEADER THRU 0950-EXIT
           MOVE "CREATE" TO CA-FUNCTION OF WS-RQ
           MOVE WS-RAISER TO RQ-REQUESTER
           MOVE "Separation of duties unit test" TO RQ-PURPOSE
           MOVE "MANUAL" TO RQ-DEMAND-TYPE
           MOVE 20261106 TO RQ-NEEDED-BY
           CALL "POREQ" USING WS-RQ
           IF CA-RETURN-CODE OF WS-RQ > 4
               GO TO 0300-EXIT
           END-IF
           MOVE RQ-REQ-NUMBER TO WS-REQ-NO
           INITIALIZE WS-RQ
           PERFORM 0950-HEADER THRU 0950-EXIT
           MOVE "ADDLINE" TO CA-FUNCTION OF WS-RQ
           MOVE WS-REQ-NO TO RQ-REQ-NUMBER
           MOVE "Separation of duties unit test line"
             TO RQ-DESCRIPTION
           MOVE "ADHESIVE-EP420-250ML" TO RQ-ITEM-CODE
           MOVE 1 TO RQ-QUANTITY
           MOVE "EA" TO RQ-UOM-CODE
           MOVE 10 TO RQ-EST-UNIT-PRICE
           MOVE 20261106 TO RQ-LINE-NEEDED-BY
           CALL "POREQ" USING WS-RQ
           IF CA-RETURN-CODE OF WS-RQ > 4
               MOVE SPACES TO WS-REQ-NO
               GO TO 0300-EXIT
           END-IF
           INITIALIZE WS-RQ
           PERFORM 0950-HEADER THRU 0950-EXIT
           MOVE "SUBMIT" TO CA-FUNCTION OF WS-RQ
           MOVE WS-REQ-NO TO RQ-REQ-NUMBER
           CALL "POREQ" USING WS-RQ
           IF CA-RETURN-CODE OF WS-RQ > 4
               MOVE SPACES TO WS-REQ-NO
           END-IF.
       0300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0800-CLEAN SECTION.
       0800-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE l FROM po.RequisitionLine l JOIN "
                  "po.Requisition r ON r.RequisitionId = "
                  "l.RequisitionId WHERE r.RequesterUser = 'ZZSODREQ'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.Requisition WHERE RequesterUser = "
                  "'ZZSODREQ'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT.
       0800-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0900-EXEC SECTION.
       0900-START.
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 9 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       0900-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0950-HEADER SECTION.
       0950-START.
           MOVE "STC" TO CA-COMPANY OF WS-RQ
           MOVE "BHM" TO CA-SITE OF WS-RQ
           MOVE WS-RAISER TO CA-USER OF WS-RQ
           MOVE 20261007 TO CA-BUSINESS-DATE OF WS-RQ.
       0950-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> COM-003.  THE RAISER NAMED AS THE APPROVER IS REFUSED, AND
      *> THE REFUSAL IS CODED SO THE SCREEN CAN EXPLAIN IT.
      *>----------------------------------------------------------------
       1000-SELF-REFUSED SECTION.
       1000-START.
           INITIALIZE WS-RQ
           PERFORM 0950-HEADER THRU 0950-EXIT
           MOVE "APPROVE" TO CA-FUNCTION OF WS-RQ
           MOVE WS-REQ-NO TO RQ-REQ-NUMBER
           MOVE "APPROVE" TO RQ-DECISION
           MOVE WS-RAISER TO RQ-APPROVER
           CALL "POREQ" USING WS-RQ
           MOVE "COM-003 the requester may not approve their own"
             TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RQ TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "COM-003 the refusal carries reason code REQ0013"
             TO TK-NAME
           MOVE "REQ0013" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RQ TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "COM-003 the refused decision reports REFUSED"
             TO TK-NAME
           MOVE "REFUSED" TO TK-EXPECTED-X
           MOVE RQ-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1100-STILL-SUBMITTED SECTION.
       1100-START.
           INITIALIZE WS-RQ
           PERFORM 0950-HEADER THRU 0950-EXIT
           MOVE "ENQUIRE" TO CA-FUNCTION OF WS-RQ
           MOVE WS-REQ-NO TO RQ-REQ-NUMBER
           CALL "POREQ" USING WS-RQ
           MOVE "COM-003 a refused self approval changes nothing"
             TO TK-NAME
           MOVE "SUBMITTED" TO TK-EXPECTED-X
           MOVE RQ-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE SAME CONTROL HOLDS WHEN THE APPROVER FIELD IS LEFT BLANK
      *> AND ONLY THE SIGNED ON USER GIVES THE GAME AWAY.
      *>----------------------------------------------------------------
       1200-SELF-AS-CA-USER SECTION.
       1200-START.
           INITIALIZE WS-RQ
           PERFORM 0950-HEADER THRU 0950-EXIT
           MOVE "APPROVE" TO CA-FUNCTION OF WS-RQ
           MOVE WS-REQ-NO TO RQ-REQ-NUMBER
           MOVE "APPROVE" TO RQ-DECISION
           CALL "POREQ" USING WS-RQ
           MOVE "COM-003 the signed on raiser is refused as well"
             TO TK-NAME
           MOVE "REQ0013" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE OF WS-RQ TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1300-INDEPENDENT-OK SECTION.
       1300-START.
           INITIALIZE WS-RQ
           PERFORM 0950-HEADER THRU 0950-EXIT
           MOVE WS-APPROVER TO CA-USER OF WS-RQ
           MOVE "APPROVE" TO CA-FUNCTION OF WS-RQ
           MOVE WS-REQ-NO TO RQ-REQ-NUMBER
           MOVE "APPROVE" TO RQ-DECISION
           MOVE WS-APPROVER TO RQ-APPROVER
           CALL "POREQ" USING WS-RQ
           MOVE "PO-005 an independent approver is accepted"
             TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE OF WS-RQ TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-005 the requisition is approved" TO TK-NAME
           MOVE "APPROVED" TO TK-EXPECTED-X
           MOVE RQ-STATUS TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-006.  core.ApprovalRule IS KEYED ON PURCHASEORDER.  A
      *> CALLER THAT SPELLS THE TYPE WITH PUNCTUATION USED TO MISS
      *> EVERY RULE, SO core.usp_RequestApproval THREW AND THE ORDER
      *> STAYED A DRAFT FOREVER.  POAPPRV NOW FOLDS THE TYPE FIRST.
      *>----------------------------------------------------------------
       1500-FOLD-TYPE SECTION.
       1500-START.
           INITIALIZE WS-AV
           MOVE "CHECK" TO AP-FUNCTION
           MOVE "STC" TO AP-COMPANY
           MOVE "BHM" TO AP-SITE
           MOVE WS-APPROVER TO AP-USER
           MOVE 20261007 TO AP-BUSINESS-DATE
           MOVE "ZZ-SOD-NO-SUCH-ORDER" TO AV-ORDER-NUMBER
           MOVE "PURCHASE-ORDER" TO AV-APPROVAL-TYPE
           CALL "POAPPRV" USING WS-AV
           MOVE "PO-006 a punctuated approval type is folded"
             TO TK-NAME
           MOVE "PURCHASEORDER" TO TK-EXPECTED-X
           MOVE AV-APPROVAL-TYPE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-AV
           MOVE "CHECK" TO AP-FUNCTION
           MOVE "STC" TO AP-COMPANY
           MOVE "BHM" TO AP-SITE
           MOVE WS-APPROVER TO AP-USER
           MOVE 20261007 TO AP-BUSINESS-DATE
           MOVE "ZZ-SOD-NO-SUCH-ORDER" TO AV-ORDER-NUMBER
           CALL "POAPPRV" USING WS-AV
           MOVE "PO-006 an unstated approval type defaults"
             TO TK-NAME
           MOVE "PURCHASEORDER" TO TK-EXPECTED-X
           MOVE AV-APPROVAL-TYPE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       9000-DISCONNECT SECTION.
       9000-START.
           MOVE "DISC" TO SQLIO-FUNC
           MOVE SPACES TO SQLIO-STMT
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       9000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       COPY TESTKITP.
       END PROGRAM TPOSOD.
