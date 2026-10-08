      *>================================================================
      *> PROGRAM-ID : TPOAPPRV
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : UNIT TESTS FOR POAPPRV, PURCHASE ORDER APPROVAL
      *>              (PO-006, PO-007, D-07).
      *>              THE SUITE BUILDS ITS OWN DRAFT ORDER OF 4,800.00
      *>              THROUGH SQLIO AND DELETES IT AGAIN, SO IT MAY BE
      *>              RUN AS OFTEN AS YOU LIKE AGAINST A LIVE
      *>              DATABASE.  WITHOUT A CONNECTION IT SKIPS.
      *>
      *>              THE THREE THINGS THAT MATTER ARE TESTED
      *>              DELIBERATELY:
      *>              1. AN UNAPPROVED ORDER IS A DRAFT, NOT A
      *>                 SUPPLIER COMMITMENT.
      *>              2. THE PREPARER MAY NOT APPROVE THEIR OWN
      *>                 ORDER, HOWEVER SENIOR THEY ARE.
      *>              3. APPROVAL IS FOR THE EXACT CONTENT.  A CHANGE
      *>                 TO THE VALUE OR THE SUPPLIER INVALIDATES IT;
      *>                 A CHANGE TO A NOTE DOES NOT, BECAUSE AN
      *>                 APPROVAL THAT HAS TO BE REPEATED FOR EVERY
      *>                 TYPO IS AN APPROVAL NOBODY READS.
      *>----------------------------------------------------------------
      *> 2026-10-06 STERP ORIGINAL
      *>================================================================
      *> REQ: PO-006, PO-007, D-07
      *> AC: self approval refused, material change needs reapproval
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TPOAPPRV.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTKITW.
       COPY POAPPRVCA REPLACING ==DFHCOMMAREA== BY ==WS-AV==.
       COPY SQLIOCA.
       01  WS-ROW                      PIC X(256).
       01  WS-DB-OK                    PIC X VALUE "N".
       01  WS-HAVE-APPROVER            PIC X VALUE "N".
       01  WS-ORDER                    PIC X(30)
                                       VALUE "ZZ-TEST-POA-1".
       01  WS-BUYER                    PIC X(20) VALUE "ZZTESTBUY".
       01  WS-APPROVER                 PIC X(20) VALUE SPACES.
       01  WS-ORDER-ID                 PIC X(20) VALUE SPACES.
       01  WS-HASH-1                   PIC X(64) VALUE SPACES.
       PROCEDURE DIVISION.
       0000-MAIN SECTION.
       0000-START.
           MOVE "po POAPPRV" TO TK-SUITE
           PERFORM TK-BEGIN
           PERFORM 0100-CONNECT THRU 0100-EXIT
           IF WS-DB-OK = "Y"
               PERFORM 0200-CLEAN THRU 0200-EXIT
               PERFORM 0400-FIND-USERS THRU 0400-EXIT
               PERFORM 0300-FIXTURES THRU 0300-EXIT
               PERFORM 1000-HASH THRU 1000-EXIT
               PERFORM 1100-NOT-APPROVED THRU 1100-EXIT
               PERFORM 1200-REQUEST THRU 1200-EXIT
               PERFORM 1300-DECIDE-NEEDS-BOTH THRU 1300-EXIT
               PERFORM 1400-SELF-APPROVAL THRU 1400-EXIT
               PERFORM 1500-APPROVE THRU 1500-EXIT
               PERFORM 1600-IMMATERIAL THRU 1600-EXIT
               PERFORM 1700-MATERIAL THRU 1700-EXIT
               PERFORM 1800-NOT-FOUND THRU 1800-EXIT
               PERFORM 0200-CLEAN THRU 0200-EXIT
               PERFORM 9000-DISCONNECT THRU 9000-EXIT
           ELSE
               MOVE "no database connection" TO TK-DETAIL
               MOVE "po POAPPRV needs the STERP database" TO TK-NAME
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
      *> THE ORDER AND EVERY APPROVAL RAISED AGAINST IT CARRY A ZZ
      *> REFERENCE, SO A FAILED RUN CANNOT LEAVE AN APPROVED ORDER
      *> BEHIND AND CANNOT TOUCH A REAL ONE.
      *>----------------------------------------------------------------
       0200-CLEAN SECTION.
       0200-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM core.Approval WHERE DocumentType = "
                  "'PURCHASEORDER' AND DocumentRef LIKE "
                  "'ZZ-TEST-POA%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE l FROM po.PurchaseOrderLine l WHERE "
                  "l.OrderNumber LIKE 'ZZ-TEST-POA%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE FROM po.PurchaseOrder WHERE OrderNumber "
                  "LIKE 'ZZ-TEST-POA%'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT.
       0200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A DRAFT ORDER FOR 4,000.00 NET, 800.00 VAT, 4,800.00 GROSS -
      *> INSIDE THE FIRST APPROVAL BAND OF THE SEEDED MATRIX.
      *>----------------------------------------------------------------
       0300-FIXTURES SECTION.
       0300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.PurchaseOrder (CompanyCode, SiteCode, "
                  "OrderNumber, OrderType, SupplierCode, BuyerUser, "
                  "OrderDate, CurrencyCode, FxRate, TermsCode, "
                  "IncotermsRule, NetAmount, VatAmount, "
                  "GrossAmount, Status, CreatedBy) VALUES "
                  "('STC','RDG','ZZ-TEST-POA-1','STANDARD',"
                  "'S0001','" FUNCTION TRIM(WS-BUYER)
                  "','2027-06-01','GBP',1,"
                  "'N30','DAP',4000.00,800.00,4800.00,'DRAFT',"
                  "'TEST')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(PurchaseOrderId AS varchar(20)) FROM "
                  "po.PurchaseOrder WHERE OrderNumber = "
                  "'ZZ-TEST-POA-1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-ONE THRU 0920-EXIT
           MOVE WS-ROW(1:20) TO WS-ORDER-ID
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT po.PurchaseOrderLine (PurchaseOrderId, "
                  "CompanyCode, OrderNumber, LineNum, ItemCode, "
                  "Description, LineType, OrderedQty, UnitPrice, "
                  "PricePerQty, NetAmount, VatCode, VatRatePct, "
                  "ExpectedVatAmount, AccountCode, Status) VALUES ("
                  FUNCTION TRIM(WS-ORDER-ID) ",'STC',"
                  "'ZZ-TEST-POA-1',1,'ZZ-TEST-APV-ITEM',"
                  "'Approval test line','STOCK',100,40.000000,1,"
                  "4000.00,'S',20.0000,800.00,'5300','OPEN')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT.
       0300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE APPROVER IS DISCOVERED, NOT ASSUMED.  THE DATABASE
      *> PROCEDURE CHECKS THE AUTHORITY FOR REAL, SO THE TEST NEEDS A
      *> USER WHO GENUINELY HOLDS IT IN THIS DATABASE RATHER THAN A
      *> NAME MADE
      *> up here - and it must not be the buyer who raised the order.
      *>----------------------------------------------------------------
       0400-FIND-USERS SECTION.
       0400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 u.UserCode FROM core.AppUser u "
                  "WHERE u.IsActive = 1 AND "
                  "core.fn_HasPermission(u.UserCode,"
                  "'PO.ORDER.ENTER','STC',NULL) = 1 AND "
                  "core.fn_HasPermission(u.UserCode,"
                  "'PO.ORDER.APPROVE','STC',NULL) = 0 "
                  "ORDER BY u.UserCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-ONE THRU 0920-EXIT
           IF SQLIO-SQLCODE = 0 AND WS-ROW(1:1) NOT = SPACE
               MOVE WS-ROW(1:20) TO WS-BUYER
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 u.UserCode FROM core.AppUser u "
                  "WHERE u.IsActive = 1 AND u.UserCode <> '"
                  FUNCTION TRIM(WS-BUYER) "' AND "
                  "core.fn_HasPermission(u.UserCode,"
                  "'PO.ORDER.APPROVE','STC',NULL) = 1 AND NOT "
                  "EXISTS (SELECT 1 FROM core.SoDRule s WHERE "
                  "s.IsActive = 1 AND s.Severity = 'BLOCK' AND "
                  "(s.ApprovalType IS NULL OR s.ApprovalType = "
                  "'PURCHASEORDER') AND core.fn_HasPermission("
                  "u.UserCode,s.PermissionA,'STC',NULL) = 1 AND "
                  "core.fn_HasPermission(u.UserCode,s.PermissionB,"
                  "'STC',NULL) = 1) ORDER BY u.UserCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-ONE THRU 0920-EXIT
           IF SQLIO-SQLCODE = 0 AND WS-ROW(1:1) NOT = SPACE
               MOVE WS-ROW(1:20) TO WS-APPROVER
               MOVE "Y" TO WS-HAVE-APPROVER
           END-IF.
       0400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0950-ASK SECTION.
       0950-START.
           MOVE "POAPPRV" TO CA-PROGRAM
           MOVE "STC" TO CA-COMPANY
           MOVE "RDG" TO CA-SITE
           MOVE 20270601 TO CA-BUSINESS-DATE
           CALL "POAPPRV" USING WS-AV.
       0950-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE HASH IS THE APPROVED VERSION.  IT MUST BE THE SAME EVERY
      *> TIME THE SAME ORDER IS READ, OR NOTHING BUILT ON IT MEANS
      *> ANYTHING.
      *>----------------------------------------------------------------
       1000-HASH SECTION.
       1000-START.
           INITIALIZE WS-AV
           MOVE "HASH" TO CA-FUNCTION
           MOVE WS-BUYER TO CA-USER
           MOVE WS-ORDER TO AV-ORDER-NUMBER
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE AV-CONTENT-HASH TO WS-HASH-1
           MOVE "PO-006 the order content is reduced to a hash"
             TO TK-NAME
           IF AV-CONTENT-HASH = SPACES
               MOVE "a hash" TO TK-EXPECTED-X
               MOVE "no hash" TO TK-ACTUAL-X
           ELSE
               MOVE "a hash" TO TK-EXPECTED-X
               MOVE "a hash" TO TK-ACTUAL-X
           END-IF
           PERFORM TK-ASSERT-STR
           MOVE "PO-006 the order value is read from the order"
             TO TK-NAME
           MOVE 4800.00 TO TK-EXPECTED-N
           MOVE AV-GROSS-AMOUNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-006 the live line is counted" TO TK-NAME
           MOVE 1 TO TK-EXPECTED-N
           MOVE AV-LINE-CNT TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           INITIALIZE WS-AV
           MOVE "HASH" TO CA-FUNCTION
           MOVE WS-BUYER TO CA-USER
           MOVE WS-ORDER TO AV-ORDER-NUMBER
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-006 the same content hashes the same way"
             TO TK-NAME
           MOVE WS-HASH-1 TO TK-EXPECTED-X
           MOVE AV-CONTENT-HASH TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-006.  BEFORE ANYBODY HAS APPROVED IT, AN ORDER IS A DRAFT.
      *>----------------------------------------------------------------
       1100-NOT-APPROVED SECTION.
       1100-START.
           INITIALIZE WS-AV
           MOVE "CHECK" TO CA-FUNCTION
           MOVE WS-BUYER TO CA-USER
           MOVE WS-ORDER TO AV-ORDER-NUMBER
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-006 an unapproved order is not a commitment"
             TO TK-NAME
           MOVE "NOTAPPROVED" TO TK-EXPECTED-X
           MOVE AV-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-006 the check holds the order (06)" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-006 and carries reason APV0011" TO TK-NAME
           MOVE "APV0011" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE REQUEST FINDS THE BAND IN THE APPROVAL MATRIX AND NAMES
      *> THE AUTHORITY THAT HAS TO DECIDE.
      *>----------------------------------------------------------------
       1200-REQUEST SECTION.
       1200-START.
           INITIALIZE WS-AV
           MOVE "REQUEST" TO CA-FUNCTION
           MOVE WS-BUYER TO CA-USER
           MOVE WS-ORDER TO AV-ORDER-NUMBER
           MOVE "PURCHASEORDER" TO AV-APPROVAL-TYPE
           MOVE WS-BUYER TO AV-REQUESTED-BY
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-006 the request awaits a decision" TO TK-NAME
           MOVE "PENDING" TO TK-EXPECTED-X
           MOVE AV-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-006 an approval request is recorded" TO TK-NAME
           IF AV-APPROVAL-ID > 0
               MOVE "recorded" TO TK-ACTUAL-X
           ELSE
               MOVE "none" TO TK-ACTUAL-X
           END-IF
           MOVE "recorded" TO TK-EXPECTED-X
           PERFORM TK-ASSERT-STR
           MOVE "D-07 the matrix names the required permission"
             TO TK-NAME
           MOVE "PO.ORDER.APPROVE" TO TK-EXPECTED-X
           MOVE AV-REQUIRED-PERM TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-006 the order is now awaiting approval"
             TO TK-NAME
           MOVE "AWAITAPPROVAL" TO TK-EXPECTED-X
           PERFORM 0930-ORDER-STATUS THRU 0930-EXIT
           MOVE WS-ROW(1:14) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> A DECISION WITH NO NAME ON IT IS NOT A DECISION.
      *>----------------------------------------------------------------
       1300-DECIDE-NEEDS-BOTH SECTION.
       1300-START.
           INITIALIZE WS-AV
           MOVE "DECIDE" TO CA-FUNCTION
           MOVE WS-BUYER TO CA-USER
           MOVE WS-ORDER TO AV-ORDER-NUMBER
           MOVE "APPROVE" TO AV-DECISION
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "D-07 a decision needs a named approver" TO TK-NAME
           MOVE "APV0010" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-AV
           MOVE "DECIDE" TO CA-FUNCTION
           MOVE WS-BUYER TO CA-USER
           MOVE WS-ORDER TO AV-ORDER-NUMBER
           MOVE "PURMGR1" TO AV-APPROVER
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "D-07 and an explicit approve or reject" TO TK-NAME
           MOVE "APV0010" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> D-07.  THE PREPARER MAY NOT APPROVE THEIR OWN ORDER.  THIS IS
      *> THE ONE RULE THAT CANNOT BE SOFTENED BY SENIORITY, SO IT IS
      *> CHECKED IN THE DATABASE AND NOT ONLY IN THE CLIENT.
      *>----------------------------------------------------------------
       1400-SELF-APPROVAL SECTION.
       1400-START.
           INITIALIZE WS-AV
           MOVE "DECIDE" TO CA-FUNCTION
           MOVE WS-BUYER TO CA-USER
           MOVE WS-ORDER TO AV-ORDER-NUMBER
           MOVE WS-BUYER TO AV-APPROVER
           MOVE "APPROVE" TO AV-DECISION
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "D-07 the preparer cannot approve their own order"
             TO TK-NAME
           MOVE 8 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "D-07 self approval carries reason APV0007"
             TO TK-NAME
           MOVE "APV0007" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "D-07 the order is still awaiting approval"
             TO TK-NAME
           MOVE "AWAITAPPROVAL" TO TK-EXPECTED-X
           PERFORM 0930-ORDER-STATUS THRU 0930-EXIT
           MOVE WS-ROW(1:14) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> AN INDEPENDENT APPROVER WITH THE AUTHORITY CAN APPROVE IT.
      *>----------------------------------------------------------------
       1500-APPROVE SECTION.
       1500-START.
           IF WS-HAVE-APPROVER NOT = "Y"
               MOVE "no independent PO.ORDER.APPROVE holder seeded"
                 TO TK-DETAIL
               MOVE "D-07 an independent approver can approve it"
                 TO TK-NAME
               PERFORM TK-SKIP
               GO TO 1500-EXIT
           END-IF
           INITIALIZE WS-AV
           MOVE "DECIDE" TO CA-FUNCTION
           MOVE WS-APPROVER TO CA-USER
           MOVE WS-ORDER TO AV-ORDER-NUMBER
           MOVE WS-APPROVER TO AV-APPROVER
           MOVE "APPROVE" TO AV-DECISION
           MOVE "Checked against the quotation" TO AV-COMMENT
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "D-07 an independent approver can approve it"
             TO TK-NAME
           MOVE "APPROVED" TO TK-EXPECTED-X
           MOVE AV-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-006 an approved order returns 00" TO TK-NAME
           MOVE 0 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-006 the order status becomes APPROVED"
             TO TK-NAME
           MOVE "APPROVED" TO TK-EXPECTED-X
           PERFORM 0930-ORDER-STATUS THRU 0930-EXIT
           MOVE WS-ROW(1:14) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-AV
           MOVE "CHECK" TO CA-FUNCTION
           MOVE WS-BUYER TO CA-USER
           MOVE WS-ORDER TO AV-ORDER-NUMBER
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-006 the check now answers APPROVED" TO TK-NAME
           MOVE "APPROVED" TO TK-EXPECTED-X
           MOVE AV-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-007.  A NOTE IS NOT A MATERIAL CHANGE.  AN APPROVAL THAT
      *> HAS TO BE REPEATED FOR EVERY TYPO IS AN APPROVAL NOBODY
      *> READS, SO THE HASH DELIBERATELY IGNORES FREE TEXT.
      *>----------------------------------------------------------------
       1600-IMMATERIAL SECTION.
       1600-START.
           IF WS-HAVE-APPROVER NOT = "Y"
               GO TO 1600-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.PurchaseOrder SET "
                  "AcknowledgementNote = 'Supplier rang to confirm'"
                  ", SupplierReference = 'THEIR-REF-99' WHERE "
                  "OrderNumber = 'ZZ-TEST-POA-1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           INITIALIZE WS-AV
           MOVE "CHECK" TO CA-FUNCTION
           MOVE WS-BUYER TO CA-USER
           MOVE WS-ORDER TO AV-ORDER-NUMBER
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-007 a note does not invalidate the approval"
             TO TK-NAME
           MOVE "APPROVED" TO TK-EXPECTED-X
           MOVE AV-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-007 and the content hash is unchanged"
             TO TK-NAME
           MOVE WS-HASH-1 TO TK-EXPECTED-X
           MOVE AV-CONTENT-HASH TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> PO-007.  CHANGING THE VALUE IS MATERIAL.  THE OLD AUTHORITY
      *> CANNOT BE INHERITED BY A BIGGER ORDER - THAT IS EXACTLY HOW
      *> AN APPROVED 4,800 BECOMES AN UNAPPROVED 6,000.
      *>----------------------------------------------------------------
       1700-MATERIAL SECTION.
       1700-START.
           IF WS-HAVE-APPROVER NOT = "Y"
               GO TO 1700-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.PurchaseOrder SET NetAmount = 5000.00,"
                  " VatAmount = 1000.00, GrossAmount = 6000.00 "
                  "WHERE OrderNumber = 'ZZ-TEST-POA-1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE po.PurchaseOrderLine SET UnitPrice = "
                  "50.000000, NetAmount = 5000.00 WHERE "
                  "OrderNumber = 'ZZ-TEST-POA-1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0900-EXEC THRU 0900-EXIT
           INITIALIZE WS-AV
           MOVE "CHECK" TO CA-FUNCTION
           MOVE WS-BUYER TO CA-USER
           MOVE WS-ORDER TO AV-ORDER-NUMBER
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-007 a changed value needs approving again"
             TO TK-NAME
           MOVE "REAPPROVE" TO TK-EXPECTED-X
           MOVE AV-OUTCOME TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-007 the check holds the order (06)" TO TK-NAME
           MOVE 6 TO TK-EXPECTED-N
           MOVE CA-RETURN-CODE TO TK-ACTUAL-N
           PERFORM TK-ASSERT-NUM
           MOVE "PO-007 and carries reason APV0006" TO TK-NAME
           MOVE "APV0006" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-007 the old approval is marked invalidated"
             TO TK-NAME
           MOVE "Y" TO TK-EXPECTED-X
           MOVE AV-WAS-INVALIDATED TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           MOVE "PO-007 the hash of the changed order differs"
             TO TK-NAME
           IF AV-CONTENT-HASH = WS-HASH-1
               MOVE "same" TO TK-ACTUAL-X
           ELSE
               MOVE "different" TO TK-ACTUAL-X
           END-IF
           MOVE "different" TO TK-EXPECTED-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-AV
           MOVE "REQUEST" TO CA-FUNCTION
           MOVE WS-BUYER TO CA-USER
           MOVE WS-ORDER TO AV-ORDER-NUMBER
           MOVE "PURCHASEORDER" TO AV-APPROVAL-TYPE
           MOVE WS-BUYER TO AV-REQUESTED-BY
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-007 re-requesting invalidates the old approval"
             TO TK-NAME
           MOVE "1" TO TK-EXPECTED-X
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(COUNT(*) AS varchar(12)) FROM "
                  "core.Approval WHERE DocumentRef = "
                  "'ZZ-TEST-POA-1' AND Status = 'INVALIDATED'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-ONE THRU 0920-EXIT
           MOVE FUNCTION TRIM(WS-ROW(1:12)) TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1700-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1800-NOT-FOUND SECTION.
       1800-START.
           INITIALIZE WS-AV
           MOVE "CHECK" TO CA-FUNCTION
           MOVE WS-BUYER TO CA-USER
           MOVE "ZZ-TEST-POA-NONE" TO AV-ORDER-NUMBER
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-006 an order that does not exist is not found"
             TO TK-NAME
           MOVE "APV0002" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR
           INITIALIZE WS-AV
           MOVE "SILLY" TO CA-FUNCTION
           MOVE WS-BUYER TO CA-USER
           MOVE WS-ORDER TO AV-ORDER-NUMBER
           PERFORM 0950-ASK THRU 0950-EXIT
           MOVE "PO-006 an unsupported function is refused"
             TO TK-NAME
           MOVE "APV0008" TO TK-EXPECTED-X
           MOVE CA-REASON-CODE TO TK-ACTUAL-X
           PERFORM TK-ASSERT-STR.
       1800-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0930-ORDER-STATUS SECTION.
       0930-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status FROM po.PurchaseOrder WHERE "
                  "OrderNumber = 'ZZ-TEST-POA-1'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           PERFORM 0920-ONE THRU 0920-EXIT.
       0930-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       0900-EXEC SECTION.
       0900-START.
           MOVE "EXEC" TO SQLIO-FUNC
           MOVE 8 TO SQLIO-CURSOR
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 0 TO SQLIO-COL-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ROW
      *>     A fixture that fails quietly turns twenty honest
      *>     assertions into twenty lies, so say so.
           IF SQLIO-ERROR
               DISPLAY "TPOAPPRV fixture SQL failed: "
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
       END PROGRAM TPOAPPRV.
